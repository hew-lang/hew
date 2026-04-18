# ============================================================================
# Hew Developer Makefile
#
# Builds all project artifacts into build/ with a predictable layout:
#
#   build/
#     bin/hew              — compiler driver (Rust, embeds MLIR/LLVM backend)
#     bin/adze             — package manager (Rust)
#     lib/libhew.a         — combined library: runtime + all stdlib packages
#     lib/wasm32-wasip1/*.a — WASM runtime + focused wire stdlib archives
#     std/*.hew            — standard library stubs
#
# Each entry under build/ is a symlink into the real Cargo/CMake output dirs,
# so there are no redundant copies and incremental builds just work.
#
# Usage:
#   make              — build everything (debug)
#   make release      — build everything (release, optimized)
#   make pre-release  — release + validate on all platforms before tagging
#   make hew          — just the compiler driver
#   make adze         — just the package manager
#   make astgen       — regenerate the C++ msgpack reader from Rust AST defs
#   make codegen      — C++ MLIR test infrastructure (unit tests + E2E harness)
#   make runtime      — just libhew_runtime.a
#   make stdlib       — all stdlib packages + combine into libhew.a
#   make wasm-runtime — WASM runtime + wire JSON/YAML archives
#   make wasm         — build hew-wasm (browser WASM via wasm-pack)
#   make playground-manifest       — regenerate examples/playground/manifest.json
#   make playground-manifest-check — verify examples/playground/manifest.json freshness
#   make playground-check          — manifest freshness + curated analysis smoke + build hew-wasm
#   make playground-wasi-check     — focused curated manifest WASI runtime preflight
#   make ci-preflight              — dispatch a conservative local preflight from the current diff
#   make wasm-dist    — build + copy WASM to hew.sh and hew.run
#   make test         — run all tests (Rust + codegen + Hew)
#   make test-rust    — just Rust workspace tests
#   make test-parser  — parser + lexer crate tests (narrow)
#   make test-types   — type-checker + parser + lexer crate tests (narrow)
#   make test-cli     — CLI crate tests (narrow)
#   make test-codegen — just hew-codegen ctest (native E2E + unit)
#   make test-hew     — run Hew test files (std/ *_test.hew)
#   make test-wasm    — just WASM E2E tests (requires wasmtime)
#   make asan         — run the nightly rust-runtime ASan test command locally
#   make lsan         — run the nightly codegen sanitizer tests with CI leak env
#   make tsan         — run the nightly rust-runtime TSan test command locally
#   make lint         — cargo clippy (workspace + tests, warnings are errors)
#   make clean        — remove build/, target/, hew-codegen/build{,-cov,-lsan}/
# ============================================================================

.PHONY: all hew adze astgen codegen runtime stdlib wasm-runtime wasm playground-manifest playground-manifest-check playground-check playground-wasi-check ci-preflight wasm-dist release
.PHONY: test test-all test-rust test-parser test-types test-cli test-codegen test-stdlib test-hew test-wasm test-cpp asan lsan tsan lint runtime-poison-safe-lint grammar
.PHONY: clean install install-check uninstall verify-ffi
.PHONY: assemble assemble-release pre-release
.PHONY: coverage coverage-summary coverage-lcov coverage-e2e coverage-combined coverage-cpp

# ── Configuration ───────────────────────────────────────────────────────────

# Prefer clang/clang++ when available (consistent with the LLVM/MLIR toolchain).
# Respects CC/CXX from the command line or environment; only overrides Make's
# built-in defaults (cc / g++).
ifeq ($(origin CC),default)
  CC := $(shell command -v clang  2>/dev/null || echo cc)
endif
ifeq ($(origin CXX),default)
  CXX := $(shell command -v clang++ 2>/dev/null || echo c++)
endif
export CC CXX

ASTGEN_ARGS = --ast hew-parser/src/ast.rs --module hew-parser/src/module.rs --output hew-codegen/src/msgpack_reader.cpp

# Static linking for the C++ test infrastructure — on by default so dev and
# release test builds match what the embedded codegen uses.
#
# To build with shared LLVM instead (faster cold-link, requires LLVM at runtime):
#   make HEW_STATIC=0
# Note: switching from a previous shared/static build requires: make clean
HEW_STATIC ?= 1

# Rust binaries (hew, adze, hew-lsp) link only against libgcc_s and libc,
# which are always present on any Linux system.  Full static Rust binaries
# require the musl target (x86_64-unknown-linux-musl) — this is handled by
# the CI release pipeline (Alpine/musl Docker build), not by the local Makefile.
# Using +crt-static on the glibc target breaks proc-macro builds.

# Installation prefix (used by `make install`)
PREFIX     ?= /usr/local/hew
DESTDIR    ?=

# Output directory — all usable artifacts land here as symlinks
BUILD_DIR  := build

# Cargo profile directory names
DEBUG_DIR  := target/debug
RELEASE_DIR := target/release
WASM_DEBUG_DIR  := target/wasm32-wasip1/debug
WASM_RELEASE_DIR := target/wasm32-wasip1/release

# Host triple used to populate lib/<triple>/ for target-aware lib lookup.
HOST_TRIPLE := $(shell rustc -vV 2>/dev/null | awk '/^host:/ { print $$2 }')
ifeq ($(shell uname -s),Darwin)
DARWIN_NATIVE_LIB_TRIPLES := aarch64-apple-darwin x86_64-apple-darwin
else
DARWIN_NATIVE_LIB_TRIPLES :=
endif
NATIVE_LIB_TRIPLES := $(HOST_TRIPLE) $(DARWIN_NATIVE_LIB_TRIPLES)

# Sanitizer targets mirror .github/workflows/nightly-sanitizers.yml as closely
# as possible while remaining usable as local entrypoints.
SANITIZER_LLVM_VERSION ?= 22
SANITIZER_RUST_TARGET ?= x86_64-unknown-linux-gnu
SANITIZER_JOBS ?= $(shell getconf _NPROCESSORS_ONLN 2>/dev/null || getconf NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
SANITIZER_CC ?= $(or $(shell command -v clang-$(SANITIZER_LLVM_VERSION) 2>/dev/null),$(CC))
SANITIZER_CXX ?= $(or $(shell command -v clang++-$(SANITIZER_LLVM_VERSION) 2>/dev/null),$(CXX))
CODEGEN_SANITIZER_FLAGS := -fsanitize=address,undefined -fno-omit-frame-pointer
CODEGEN_SANITIZER_LINK_FLAGS := -fsanitize=address,undefined
CODEGEN_SANITIZER_UNIT_REGEX := ^(mlir_dialect|translate|coro_generator|coro_fib_generator)$$
CODEGEN_SANITIZER_E2E_REGEX := ^(mlirgen|e2e_actor_basic|e2e_vec_basic|e2e_hashmap_basic|e2e_concurrency_concurrent_counter|e2e_concurrency_message_ordering|e2e_memory_.*|e2e_concurrency_.*|coro_generator|coro_fib_generator)$$
CODEGEN_SANITIZER_ASAN_OPTIONS := detect_leaks=1:strict_string_checks=1
CODEGEN_SANITIZER_LSAN_OPTIONS := suppressions=$(CURDIR)/hew-codegen/lsan.supp
CODEGEN_SANITIZER_UBSAN_OPTIONS := print_stacktrace=1:halt_on_error=1
CODEGEN_SANITIZER_CMAKE_ARGS := -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_C_FLAGS="$(CODEGEN_SANITIZER_FLAGS)" -DCMAKE_CXX_FLAGS="$(CODEGEN_SANITIZER_FLAGS)"
ifneq ($(shell uname -s),Darwin)
  CODEGEN_SANITIZER_CMAKE_ARGS += -DCMAKE_EXE_LINKER_FLAGS="$(CODEGEN_SANITIZER_LINK_FLAGS)"
  CODEGEN_SANITIZER_CMAKE_ARGS += -DCMAKE_SHARED_LINKER_FLAGS="$(CODEGEN_SANITIZER_LINK_FLAGS)"
endif
CODEGEN_SANITIZER_TEST_ENV := ASAN_OPTIONS="$(CODEGEN_SANITIZER_ASAN_OPTIONS)" LSAN_OPTIONS="$(CODEGEN_SANITIZER_LSAN_OPTIONS)" UBSAN_OPTIONS="$(CODEGEN_SANITIZER_UBSAN_OPTIONS)"
RUNTIME_ASAN_TARGET_DIR := target/sanitizer-runtime-asan
RUNTIME_TSAN_TARGET_DIR := target/sanitizer-runtime-tsan

# ── Default target ──────────────────────────────────────────────────────────

all: hew adze runtime stdlib assemble

# ── Rust targets ────────────────────────────────────────────────────────────

# Build the hew compiler driver (debug)
hew:
	cargo build -p hew-cli

# Build the adze package manager (debug)
adze:
	cargo build -p adze-cli

# Build the runtime static library (debug)
runtime:
	cargo build -p hew-runtime

# Build libhew.a — the combined runtime + stdlib static library.
# The hew-lib umbrella crate depends on hew-runtime + all stdlib crates;
# Cargo produces a single deduplicated staticlib.
stdlib:
	cargo build -p hew-lib

# Build the WASM runtime + focused wire JSON/YAML archives
wasm-runtime:
	cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features
	cargo build -p hew-std-encoding-json --target wasm32-wasip1
	cargo build -p hew-std-encoding-yaml --target wasm32-wasip1

# Build the hew-wasm browser analysis-only module (requires: cargo install wasm-pack)
wasm:
	wasm-pack build hew-wasm --target web --release

# Regenerate the curated playground manifest consumed by downstream browser tooling.
playground-manifest:
	python3 scripts/gen-playground-manifest.py

# Verify the checked-in playground manifest is current.
playground-manifest-check:
	python3 scripts/gen-playground-manifest.py --check

# Repo-local browser/tooling smoke:
# manifest freshness + curated hew-wasm analysis smoke + analysis-only WASM build.
playground-check: playground-manifest-check
	cargo test -p hew-wasm --lib curated_playground_manifest_smoke -- --exact
	$(MAKE) wasm

# Focused curated playground WASI runtime preflight.
# Requires the hew binary to be built with embedded codegen plus wasmtime.
playground-wasi-check:
	cargo test -p hew-cli --test wasi_run_e2e curated_playground_examples_run_under_wasi -- --exact
	cargo test -p hew-cli --test wasi_run_e2e supervisor_stays_on_the_unsupported_diagnostic_path_under_wasi -- --exact

# Conservative diff-based local preflight dispatcher.
# Usage: make ci-preflight ARGS="--dry-run" or ARGS="--base origin/main"
ci-preflight:
	scripts/ci-preflight-dispatcher.sh $(ARGS)

# Downstream repo roots (sibling directories of hew/)
HEW_SH  ?= $(CURDIR)/../hew.sh
HEW_RUN ?= $(CURDIR)/../hew.run

# Build hew-wasm and distribute to downstream repos
wasm-dist: wasm
	@echo "==> Distributing hew-wasm to hew.sh"
	cp hew-wasm/pkg/hew_wasm.js      $(HEW_SH)/src/lib/wasm/hew_wasm.js
	cp hew-wasm/pkg/hew_wasm_bg.wasm $(HEW_SH)/public/wasm/hew_wasm_bg.wasm
	@echo "==> Distributing hew-wasm to hew.run"
	cp hew-wasm/pkg/hew_wasm.js      $(HEW_RUN)/src/lib/wasm/hew_wasm.js
	cp hew-wasm/pkg/hew_wasm_bg.wasm $(HEW_RUN)/static/wasm/hew_wasm_bg.wasm
	@echo "==> Done. Commit in hew.sh and hew.run."

# ── C++ test infrastructure ──────────────────────────────────────────────────

# Build hew-codegen libraries and test executables.  The codegen is embedded
# in the `hew` binary via build.rs; this CMake build exists only for C++ unit
# tests (test_mlirgen, test_mlir_dialect, test_translate) and the ctest E2E
# harness.  No standalone binary is produced.
#
# Auto-detects LLVM/MLIR paths:
#   Linux (apt.llvm.org):  /usr/lib/llvm-<ver>/lib/cmake/{llvm,mlir}
#   macOS (Homebrew):      $(brew --prefix llvm@<ver>)/lib/cmake/{llvm,mlir}
#   FreeBSD (pkg):         /usr/local/llvm<ver>/lib/cmake/{llvm,mlir}
#
# Override with: make codegen LLVM_DIR=/path/to/llvm MLIR_DIR=/path/to/mlir
#            or: make codegen LLVM_PREFIX=/usr/lib/llvm-22

# Auto-detect LLVM prefix if not explicitly provided
ifndef LLVM_PREFIX
  # Try versioned apt.llvm.org paths (22, 21, 20, 19...)
  LLVM_PREFIX := $(firstword $(wildcard /usr/lib/llvm-22 /usr/lib/llvm-21 /usr/lib/llvm-20 /usr/lib/llvm-19))
  # Try FreeBSD pkg paths (/usr/local/llvm<ver>)
  ifeq ($(LLVM_PREFIX),)
    LLVM_PREFIX := $(firstword $(wildcard /usr/local/llvm22 /usr/local/llvm21 /usr/local/llvm20 /usr/local/llvm19))
  endif
  # Try Homebrew on macOS
  ifeq ($(LLVM_PREFIX),)
    LLVM_PREFIX := $(shell brew --prefix llvm 2>/dev/null)
  endif
endif

CMAKE_EXTRA_ARGS :=
ifdef LLVM_DIR
  CMAKE_EXTRA_ARGS += -DLLVM_DIR=$(LLVM_DIR)
else ifneq ($(LLVM_PREFIX),)
  CMAKE_EXTRA_ARGS += -DLLVM_DIR=$(LLVM_PREFIX)/lib/cmake/llvm
endif
ifdef MLIR_DIR
  CMAKE_EXTRA_ARGS += -DMLIR_DIR=$(MLIR_DIR)
else ifneq ($(LLVM_PREFIX),)
  CMAKE_EXTRA_ARGS += -DMLIR_DIR=$(LLVM_PREFIX)/lib/cmake/mlir
endif
ifeq ($(HEW_STATIC),1)
  CMAKE_EXTRA_ARGS += -DHEW_STATIC_LINK=ON
else
  CMAKE_EXTRA_ARGS += -DHEW_STATIC_LINK=OFF
endif

# macOS requires brew's clang (not Apple Clang) to handle LLVM 21 bitcode
# in the statically linked MLIR objects, plus the Apple SDK sysroot to fix
# header conflicts, and brew's libc++ path for ABI compatibility.
# See docs/cross-platform-build-guide.md for details.
ifeq ($(shell uname -s),Darwin)
  ifneq ($(LLVM_PREFIX),)
    CMAKE_EXTRA_ARGS += -DCMAKE_C_COMPILER=$(LLVM_PREFIX)/bin/clang
    CMAKE_EXTRA_ARGS += -DCMAKE_CXX_COMPILER=$(LLVM_PREFIX)/bin/clang++
    CMAKE_EXTRA_ARGS += -DCMAKE_OSX_SYSROOT=$(shell xcrun --show-sdk-path)
    CMAKE_EXTRA_ARGS += -DCMAKE_EXE_LINKER_FLAGS="-L$(LLVM_PREFIX)/lib/c++ -Wl,-rpath,$(LLVM_PREFIX)/lib/c++"
  endif
endif

astgen:
	cargo run -q -p hew-astgen -- $(ASTGEN_ARGS)

# Clean the cmake build directory (forces full reconfigure).
codegen-clean:
	rm -rf hew-codegen/build

# Clean, reconfigure, and rebuild the codegen test infrastructure.
codegen-rebuild: codegen-clean codegen

# Run a subset of codegen E2E tests by regex pattern.
# Usage: make codegen-test PATTERN=supervisor
#
# Build local compiler/runtime artifacts first so dedicated worktrees do not
# fall back to an unrelated `hew` binary from PATH or skip WASM runtime linking.
codegen-test: hew stdlib wasm-runtime
	$(MAKE) codegen
	cd hew-codegen/build && ctest --output-on-failure $(if $(PATTERN),-R "$(PATTERN)")

codegen:
ifeq ($(shell uname -s),Darwin)
	cmake -B hew-codegen/build -G Ninja \
		$(CMAKE_EXTRA_ARGS) \
		-S hew-codegen
else
	cmake -B hew-codegen/build -G Ninja \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		$(CMAKE_EXTRA_ARGS) \
		-S hew-codegen
endif
	cmake --build hew-codegen/build

# Create symlinks from build/ into the real output locations.
# This gives you one stable directory to point PATH at during development.
assemble: | hew adze runtime stdlib
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib
	@# assemble-release makes build/std a symlink to ../std; reset it so the
	@# flat std stub loop below cannot rewrite tracked std/*.hew files in root.
	@rm -rf $(BUILD_DIR)/std
	@mkdir -p $(BUILD_DIR)/std
	@# Compiler driver
	@ln -sfn ../../$(DEBUG_DIR)/hew                $(BUILD_DIR)/bin/hew
	@# Package manager
	@ln -sfn ../../$(DEBUG_DIR)/adze               $(BUILD_DIR)/bin/adze
	@# Combined Hew library (runtime + all stdlib packages)
	@ln -sfn ../../$(DEBUG_DIR)/libhew.a           $(BUILD_DIR)/lib/libhew.a
	@# WASM runtime + focused wire stdlib archives (symlink if built)
	@for lib in libhew_runtime.a libhew_std_encoding_json.a libhew_std_encoding_yaml.a; do \
		if [ -f $(WASM_DEBUG_DIR)/$$lib ]; then \
			mkdir -p $(BUILD_DIR)/lib/wasm32-wasip1; \
			ln -sfn ../../../$(WASM_DEBUG_DIR)/$$lib \
				$(BUILD_DIR)/lib/wasm32-wasip1/$$lib; \
		fi; \
	done
	@# Native per-triple lib symlinks — mirrors the wasm32-wasip1 pattern,
	@# keeps the host lib under lib/<triple>/ on Linux and Darwin, and lets
	@# Darwin same-OS cross-arch linking pick up prebuilt libhew.a slices.
	@for triple in $(NATIVE_LIB_TRIPLES); do \
		[ -n "$$triple" ] || continue; \
		lib_path=""; \
		if [ -f target/$$triple/debug/libhew.a ]; then \
			lib_path="target/$$triple/debug/libhew.a"; \
		elif [ "$$triple" = "$(HOST_TRIPLE)" ] && [ -f $(DEBUG_DIR)/libhew.a ]; then \
			lib_path="$(DEBUG_DIR)/libhew.a"; \
		else \
			continue; \
		fi; \
		mkdir -p $(BUILD_DIR)/lib/$$triple; \
		ln -sfn ../../../$$lib_path $(BUILD_DIR)/lib/$$triple/libhew.a; \
	done
	@# Standard library stubs (one symlink per file so the dir stays flat)
	@for f in std/*.hew; do \
		ln -sfn "../../$$f" "$(BUILD_DIR)/std/$$(basename $$f)"; \
	done
	@echo "build/ assembled (debug). Add to PATH:"
	@echo "  export PATH=\"$(CURDIR)/$(BUILD_DIR)/bin:\$$PATH\""

# ── Release build ───────────────────────────────────────────────────────────

# Build everything in release mode and repoint the build/ symlinks.
# On macOS, force a clean release-artifact rebuild so the pinned deployment
# target does not reuse older release outputs built with the host-default
# target while preserving debug/incremental work.
RELEASE_PREP = @:
RELEASE_ENV =
ifeq ($(shell uname -s),Darwin)
  RELEASE_PREP = cargo clean --profile release
  RELEASE_ENV = MACOSX_DEPLOYMENT_TARGET=13.0
endif

release:
	$(RELEASE_PREP)
	$(RELEASE_ENV) HEW_EMBED_STATIC=1 cargo build -p hew-cli --release
	$(RELEASE_ENV) cargo build -p adze-cli --release
	$(RELEASE_ENV) cargo build -p hew-lib --release
	$(RELEASE_ENV) cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features --release
	$(RELEASE_ENV) cargo build -p hew-std-encoding-json --target wasm32-wasip1 --release
	$(RELEASE_ENV) cargo build -p hew-std-encoding-yaml --target wasm32-wasip1 --release
	$(MAKE) assemble-release

# Validate release builds on all supported platforms before tagging.
# Runs linux locally first (fail-fast), then remote platforms in parallel.
#   make pre-release                    — all platforms
#   make pre-release PLATFORMS="linux"  — linux only
pre-release: release
	scripts/pre-release-validate.sh $(PLATFORMS)

# Assemble build/ with release symlinks.
assemble-release:
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib $(BUILD_DIR)/std
	@ln -sfn ../../$(RELEASE_DIR)/hew              $(BUILD_DIR)/bin/hew
	@ln -sfn ../../$(RELEASE_DIR)/adze             $(BUILD_DIR)/bin/adze
	@# Combined Hew library (runtime + all stdlib packages)
	@ln -sfn ../../$(RELEASE_DIR)/libhew.a         $(BUILD_DIR)/lib/libhew.a
	@for lib in libhew_runtime.a libhew_std_encoding_json.a libhew_std_encoding_yaml.a; do \
		if [ -f $(WASM_RELEASE_DIR)/$$lib ]; then \
			mkdir -p $(BUILD_DIR)/lib/wasm32-wasip1; \
			ln -sfn ../../../$(WASM_RELEASE_DIR)/$$lib \
				$(BUILD_DIR)/lib/wasm32-wasip1/$$lib; \
		fi; \
	done
	@# Native per-triple lib symlinks — mirrors the wasm32-wasip1 pattern.
	@for triple in $(NATIVE_LIB_TRIPLES); do \
		[ -n "$$triple" ] || continue; \
		lib_path=""; \
		if [ -f target/$$triple/release/libhew.a ]; then \
			lib_path="target/$$triple/release/libhew.a"; \
		elif [ "$$triple" = "$(HOST_TRIPLE)" ] && [ -f $(RELEASE_DIR)/libhew.a ]; then \
			lib_path="$(RELEASE_DIR)/libhew.a"; \
		else \
			continue; \
		fi; \
		mkdir -p $(BUILD_DIR)/lib/$$triple; \
		ln -sfn ../../../$$lib_path $(BUILD_DIR)/lib/$$triple/libhew.a; \
	done
	@rm -rf $(BUILD_DIR)/std
	@ln -sfn ../std $(BUILD_DIR)/std
	@echo "build/ assembled (release)."

# ── Tests ───────────────────────────────────────────────────────────────────

test: test-rust test-codegen test-hew test-cpp

# TODO: Add test-stdlib to `test` target once stdlib files are type-check clean
test-all: test-rust test-codegen test-stdlib test-hew test-wasm

test-rust:
	cargo test

test-parser:
	cargo test -p hew-parser -p hew-lexer

test-types:
	cargo test -p hew-types -p hew-parser -p hew-lexer

test-cli:
	cargo test -p hew-cli -p adze-cli

test-codegen: hew codegen runtime stdlib
	cd hew-codegen/build && ctest --output-on-failure -LE wasm

test-wasm: hew codegen wasm-runtime
	cd hew-codegen/build && ctest --output-on-failure -L wasm

test-stdlib: hew
	@echo "==> Type-checking stdlib .hew files"
	@fail=0; total=0; \
	for f in $$(find std/ -name '*.hew' -not -path '*/target/*' | sort); do \
	  total=$$((total + 1)); \
	  if ! $(DEBUG_DIR)/hew check "$$f" >/dev/null 2>&1; then \
	    echo "  FAIL: $$f"; \
	    $(DEBUG_DIR)/hew check "$$f" 2>&1 | head -3; \
	    fail=$$((fail + 1)); \
	  fi; \
	done; \
	echo "  $$((total - fail))/$$total stdlib files pass type-check"; \
	if [ $$fail -gt 0 ]; then \
	  echo "ERROR: $$fail stdlib file(s) failed type-check"; \
	  exit 1; \
	fi

test-hew: hew codegen runtime stdlib
	@echo "==> Running Hew test files"
	$(DEBUG_DIR)/hew test tests/hew/

# C++ unit tests only (not E2E)
test-cpp: codegen
	@echo "==> Running C++ unit tests"
	cd hew-codegen/build && ctest --output-on-failure -R "^(mlir_dialect|mlirgen|translate|codegen_capi|msgpack_reader)$$"

# Nightly rust-runtime ASan command (Linux/nightly toolchain required).
asan:
	CARGO_TARGET_DIR=$(RUNTIME_ASAN_TARGET_DIR) \
	RUSTFLAGS="-Zsanitizer=address -Cforce-frame-pointers=yes" \
	ASAN_OPTIONS="detect_leaks=1" \
	cargo +nightly test --target $(SANITIZER_RUST_TARGET) -p hew-runtime --lib

# Nightly codegen sanitizer lane: ASan+UBSan build plus leak-checking test env.
# Darwin/Homebrew LLVM caveat: on macOS arm64 with Homebrew LLVM 22, the
# mlir_dialect and translate unit tests may crash with
# "AddressSanitizer: use-after-poison" inside mlir::BuiltinDialect::initialize()
# before any Hew codegen path executes.  This is a known MLIR/Homebrew LLVM
# interaction, not a Hew bug.  The authoritative sanitizer gate is the Linux CI
# workflow (.github/workflows/nightly-sanitizers.yml, ubuntu-24.04 + apt LLVM 22).
lsan:
	cargo build -p hew-cli -p hew-runtime -p hew-serialize
	cargo build -p hew-lib
ifeq ($(shell uname -s),Darwin)
	cmake -B hew-codegen/build-lsan -G Ninja \
		$(CODEGEN_SANITIZER_CMAKE_ARGS) \
		$(CMAKE_EXTRA_ARGS) \
		-S hew-codegen
else
	cmake -B hew-codegen/build-lsan -G Ninja \
		-DCMAKE_C_COMPILER=$(SANITIZER_CC) \
		-DCMAKE_CXX_COMPILER=$(SANITIZER_CXX) \
		$(CODEGEN_SANITIZER_CMAKE_ARGS) \
		$(CMAKE_EXTRA_ARGS) \
		-S hew-codegen
endif
	cmake --build hew-codegen/build-lsan --parallel $(SANITIZER_JOBS)
	cd hew-codegen/build-lsan && $(CODEGEN_SANITIZER_TEST_ENV) \
	ctest --output-on-failure -R "$(CODEGEN_SANITIZER_UNIT_REGEX)"
	cd hew-codegen/build-lsan && $(CODEGEN_SANITIZER_TEST_ENV) \
	ctest --output-on-failure -j"$(SANITIZER_JOBS)" -R "$(CODEGEN_SANITIZER_E2E_REGEX)"

# Nightly rust-runtime TSan command (Linux/nightly toolchain required).
tsan:
	CARGO_TARGET_DIR=$(RUNTIME_TSAN_TARGET_DIR) \
	RUSTFLAGS="-Zsanitizer=thread -Cforce-frame-pointers=yes" \
	TSAN_OPTIONS="halt_on_error=1" \
	cargo +nightly test \
		--target $(SANITIZER_RUST_TARGET) \
		-p hew-runtime \
		--no-default-features \
		--lib \
		-- --test-threads=1

# ── Lint ────────────────────────────────────────────────────────────────────

lint: runtime-poison-safe-lint
	cargo clippy --workspace --tests -- -D warnings

# Grep-gate: fail on raw .lock()/.read()/.write() against any runtime global
# that has been migrated to the PoisonSafe/PoisonSafeRw wrapper, and on the
# `if let Ok(_) = X.lock()` anti-pattern anywhere in hew-runtime/src/. Extend
# the allowlist in scripts/lint-runtime-poison-safe.sh as future sweeps land.
runtime-poison-safe-lint: runtime-poison-safe-lint-self-test
	bash scripts/lint-runtime-poison-safe.sh

# Validate that the lint script's own pattern-matching regex is coherent.
# Runs synthetic violations through the linter to confirm every guard fires.
runtime-poison-safe-lint-self-test:
	bash scripts/lint-runtime-poison-safe.sh --self-test

# ── Coverage ───────────────────────────────────────────────────────────────
#
#   make coverage         — Rust unit/integration tests only (cargo llvm-cov)
#   make coverage-e2e     — E2E tests exercising the full compile pipeline
#   make coverage-cpp     — C++ codegen coverage (llvm-cov profiling)
#   make coverage-combined — both merged into a single report
#
# E2E coverage instruments the hew CLI binary and runtime, then runs all 424+
# ctest E2E tests. Each `hew compile` invocation and each compiled binary
# execution generates profraw data.
#
# Requires: cargo-llvm-cov, llvm-profdata-22, llvm-cov-22

COV_DIR          := coverage-out
COV_PROFRAW_DIR  := $(COV_DIR)/profraw
COV_E2E_DIR      := $(COV_DIR)/e2e-profraw
COV_PROFDATA     := $(COV_DIR)/combined.profdata
LLVM_PROFDATA    ?= llvm-profdata-22
LLVM_COV         ?= llvm-cov-22

# Rust-only coverage (cargo test)
coverage:
	cargo llvm-cov --workspace --exclude hew-wasm --html --output-dir $(COV_DIR)/html
	@echo "==> Open $(COV_DIR)/html/index.html"

coverage-summary:
	cargo llvm-cov --workspace --exclude hew-wasm --no-report
	cargo llvm-cov report --summary-only

coverage-lcov:
	cargo llvm-cov --workspace --exclude hew-wasm --lcov --output-path $(COV_DIR)/lcov.info
	@echo "==> Wrote $(COV_DIR)/lcov.info"

# E2E coverage: instruments hew CLI + runtime, runs ctest, generates report
coverage-e2e: codegen stdlib
	@echo "==> Building hew CLI + runtime with coverage instrumentation"
	RUSTFLAGS="-C instrument-coverage" cargo build -p hew-cli -p hew-runtime
	@rm -rf $(COV_E2E_DIR) && mkdir -p $(COV_E2E_DIR)
	@echo "==> Running $(words $(wildcard hew-codegen/tests/examples/**/*.hew)) E2E tests with instrumented binary"
	LLVM_PROFILE_FILE="$(CURDIR)/$(COV_E2E_DIR)/e2e_%p_%m.profraw" \
	  sh -c 'cd hew-codegen/build && ctest --output-on-failure -LE wasm -j8'
	@echo "==> Merging profraw data"
	$(LLVM_PROFDATA) merge -sparse $(COV_E2E_DIR)/*.profraw -o $(COV_DIR)/e2e.profdata
	@echo "==> E2E coverage summary (Rust frontend):"
	$(LLVM_COV) report target/debug/hew \
	  -instr-profile=$(COV_DIR)/e2e.profdata \
	  --ignore-filename-regex='(\.cargo|rustc|/usr/)' \
	  -summary-only
	@echo "==> For full file report: $(LLVM_COV) report target/debug/hew -instr-profile=$(COV_DIR)/e2e.profdata --ignore-filename-regex='(\\.cargo|rustc|/usr/)'"

# Combined coverage: cargo tests + E2E tests merged into one report
coverage-combined: codegen stdlib
	@echo "==> Phase 1: Running cargo tests with coverage"
	cargo llvm-cov --workspace --exclude hew-wasm --no-report
	@echo "==> Phase 2: Building hew CLI with coverage instrumentation"
	RUSTFLAGS="-C instrument-coverage" cargo build -p hew-cli -p hew-runtime
	@rm -rf $(COV_E2E_DIR) && mkdir -p $(COV_E2E_DIR)
	@echo "==> Phase 3: Running E2E tests with instrumented binary"
	LLVM_PROFILE_FILE="$(CURDIR)/$(COV_E2E_DIR)/e2e_%p_%m.profraw" \
	  sh -c 'cd hew-codegen/build && ctest --output-on-failure -LE wasm -j8'
	@echo "==> Phase 4: Merging all profraw data (cargo tests + E2E)"
	@mkdir -p $(COV_DIR)/merged
	@cp target/llvm-cov-target/*.profraw $(COV_DIR)/merged/ 2>/dev/null || true
	@cp $(COV_E2E_DIR)/*.profraw $(COV_DIR)/merged/ 2>/dev/null || true
	$(LLVM_PROFDATA) merge -sparse $(COV_DIR)/merged/*.profraw -o $(COV_PROFDATA)
	@echo "==> Combined coverage summary:"
	$(LLVM_COV) report target/debug/hew \
	  -instr-profile=$(COV_PROFDATA) \
	  --ignore-filename-regex='(\.cargo|rustc|/usr/)' \
	  -summary-only
	@echo "==> Generating HTML report"
	$(LLVM_COV) show target/debug/hew \
	  -instr-profile=$(COV_PROFDATA) \
	  --ignore-filename-regex='(\.cargo|rustc|/usr/)' \
	  -format=html -output-dir=$(COV_DIR)/combined-html
	@echo "==> Open $(COV_DIR)/combined-html/index.html"
	@rm -rf $(COV_DIR)/merged

# C++ codegen coverage: instruments hew-codegen, runs unit + E2E tests, reports
coverage-cpp: stdlib
	@echo "==> Building hew-codegen with coverage instrumentation"
ifeq ($(shell uname -s),Darwin)
	cmake -B hew-codegen/build-cov -G Ninja \
		$(CMAKE_EXTRA_ARGS) \
		-DHEW_COVERAGE=ON \
		-S hew-codegen
else
	cmake -B hew-codegen/build-cov -G Ninja \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		$(CMAKE_EXTRA_ARGS) \
		-DHEW_COVERAGE=ON \
		-S hew-codegen
endif
	cmake --build hew-codegen/build-cov
	@echo "==> Running C++ tests with coverage"
	@rm -rf $(COV_DIR)/cpp-profraw && mkdir -p $(COV_DIR)/cpp-profraw
	LLVM_PROFILE_FILE="$(CURDIR)/$(COV_DIR)/cpp-profraw/unit_%p_%m.profraw" \
	  sh -c 'cd hew-codegen/build-cov && ctest --output-on-failure -LE wasm -j8'
	@echo "==> Merging profdata"
	$(LLVM_PROFDATA) merge -sparse $(COV_DIR)/cpp-profraw/*.profraw -o $(COV_DIR)/cpp.profdata
	@echo "==> C++ codegen coverage summary:"
	$(LLVM_COV) report hew-codegen/build-cov/src/hew-codegen \
	  -instr-profile=$(COV_DIR)/cpp.profdata \
	  --ignore-filename-regex='(llvm/|mlir/include|mlir/lib|/usr/|msgpack\.h|nlohmann|json\.hpp|_deps/)' \
	  -summary-only

# ── FFI symbol verification ───────────────────────────────────────────────
# Checks that every hew_* function name referenced in C++ codegen has a
# matching #[no_mangle] export in hew-runtime (or is in a known exception
# list for stdlib packages and codegen-internal rewrites).

verify-ffi:
	python3 scripts/verify-ffi-symbols.py --strict

# ── ANTLR4 grammar validation ──────────────────────────────────────────────
# Requires Java and the ANTLR4 jar. This is rarely needed — only when
# modifying docs/specs/Hew.g4.

ANTLR4_JAR  ?= /tmp/antlr-4.13.2-complete.jar
JAVA_HOME   ?= /usr/lib/jvm/java-21-openjdk-amd64
JAVA        := $(JAVA_HOME)/bin/java
JAVAC       := $(JAVA_HOME)/bin/javac
GRAMMAR     := docs/specs/Hew.g4
GRAMMAR_OUT := .tmp/hew-grammar-test
HEW_FILES   := $(sort $(shell find examples/ -name '*.hew' 2>/dev/null))

grammar: $(GRAMMAR) $(HEW_FILES)
	@echo "==> Generating ANTLR4 parser"
	@rm -rf $(GRAMMAR_OUT)
	@cp $(GRAMMAR) .tmp/Hew.g4
	$(JAVA) -jar $(ANTLR4_JAR) -Dlanguage=Java -o $(GRAMMAR_OUT) .tmp/Hew.g4
	@echo "==> Compiling grammar test parser"
	$(JAVAC) -cp $(ANTLR4_JAR) $(GRAMMAR_OUT)/*.java
	@echo "==> Parsing example files"
	@pass=0; fail=0; \
	for f in $(HEW_FILES); do \
		if $(JAVA) -cp $(ANTLR4_JAR):$(GRAMMAR_OUT) \
			org.antlr.v4.gui.TestRig Hew program < "$$f" > /dev/null 2>&1; then \
			echo "  OK   $$f"; \
			pass=$$((pass + 1)); \
		else \
			echo "  FAIL $$f"; \
			fail=$$((fail + 1)); \
		fi; \
	done; \
	echo "==> $$pass passed, $$fail failed"; \
	if [ $$fail -gt 0 ]; then exit 1; fi

# ── Install / Uninstall ────────────────────────────────────────────────────
# Installs release-built artifacts to $(DESTDIR)$(PREFIX).
# Run `make release` first, or this target will build release for you.

install: install-check
	@echo "==> Installing to $(DESTDIR)$(PREFIX)"
	install -d $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/lib
	install -d $(DESTDIR)$(PREFIX)/std
	install -d $(DESTDIR)$(PREFIX)/completions
	install -m 755 $(RELEASE_DIR)/hew                $(DESTDIR)$(PREFIX)/bin/hew
	install -m 755 $(RELEASE_DIR)/adze               $(DESTDIR)$(PREFIX)/bin/adze
	install -m 644 $(RELEASE_DIR)/libhew.a           $(DESTDIR)$(PREFIX)/lib/libhew.a
	@for lib in libhew_runtime.a libhew_std_encoding_json.a libhew_std_encoding_yaml.a; do \
		if [ -f $(WASM_RELEASE_DIR)/$$lib ]; then \
			install -d $(DESTDIR)$(PREFIX)/lib/wasm32-wasip1; \
			install -m 644 $(WASM_RELEASE_DIR)/$$lib \
				$(DESTDIR)$(PREFIX)/lib/wasm32-wasip1/$$lib; \
		fi; \
	done
	@# Native per-triple lib subtree — mirrors assemble-release and gives
	@# find_hew_lib() its preferred lib/<triple>/libhew.a probe path.
	@for triple in $(NATIVE_LIB_TRIPLES); do \
		[ -n "$$triple" ] || continue; \
		lib_path=""; \
		if [ -f target/$$triple/release/libhew.a ]; then \
			lib_path="target/$$triple/release/libhew.a"; \
		elif [ "$$triple" = "$(HOST_TRIPLE)" ] && [ -f $(RELEASE_DIR)/libhew.a ]; then \
			lib_path="$(RELEASE_DIR)/libhew.a"; \
		else \
			continue; \
		fi; \
		install -d $(DESTDIR)$(PREFIX)/lib/$$triple; \
		install -m 644 $$lib_path $(DESTDIR)$(PREFIX)/lib/$$triple/libhew.a; \
	done
	cp -r std/. $(DESTDIR)$(PREFIX)/std/
	install -m 644 completions/hew.bash              $(DESTDIR)$(PREFIX)/completions/
	install -m 644 completions/hew.zsh               $(DESTDIR)$(PREFIX)/completions/
	install -m 644 completions/hew.fish              $(DESTDIR)$(PREFIX)/completions/
	install -m 644 completions/adze.bash             $(DESTDIR)$(PREFIX)/completions/
	install -m 644 completions/adze.zsh              $(DESTDIR)$(PREFIX)/completions/
	install -m 644 completions/adze.fish             $(DESTDIR)$(PREFIX)/completions/
	@echo "==> Installed to $(DESTDIR)$(PREFIX)"
	@echo "    Add $(PREFIX)/bin to your PATH:"
	@echo "      export PATH=\"$(PREFIX)/bin:\$$PATH\""

install-check:
	@test -f $(RELEASE_DIR)/hew \
		|| { echo "Error: release hew not built. Run 'make release' first."; exit 1; }
	@test -f $(RELEASE_DIR)/libhew.a \
		|| { echo "Error: libhew.a not built. Run 'make release' first."; exit 1; }

uninstall:
	rm -rf $(DESTDIR)$(PREFIX)
	@echo "==> Removed $(DESTDIR)$(PREFIX)"

# ── Cleanup ─────────────────────────────────────────────────────────────────

clean:
	rm -rf $(BUILD_DIR)
	rm -rf hew-codegen/build hew-codegen/build-cov hew-codegen/build-lsan
	cargo clean
	rm -rf $(GRAMMAR_OUT) .tmp/Hew.g4
	rm -rf $(COV_DIR)
