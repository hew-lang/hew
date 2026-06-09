# ============================================================================
# Hew Developer Makefile
#
# Builds all project artifacts into build/ with a predictable layout:
#
#   build/
#     bin/hew              — compiler driver (Rust)
#     bin/adze             — package manager (Rust)
#     lib/libhew.a         — combined library: runtime + all stdlib packages
#     lib/wasm32-wasip1/*.a — WASM runtime + focused wire stdlib archives
#     std/*.hew            — standard library stubs
#
# Each entry under build/ is a symlink into the real Cargo output dirs,
# so there are no redundant copies and incremental builds just work.
#
# Usage:
#   make              — build everything (debug)
#   make release      — build everything (release, optimized)
#   make pre-release  — release + validate on all platforms before tagging
#   make publish-docs — build stdlib docs + print wrangler deploy command (operator runs wrangler)
#   make hew          — just the compiler driver
#   make adze         — just the package manager
#   make runtime      — just libhew_runtime.a
#   make stdlib       — all stdlib packages + combine into libhew.a
#   make wasm-runtime — WASM runtime + wire JSON/YAML archives
#   make wasm         — build hew-wasm (browser WASM via wasm-pack)
#   make playground-manifest       — regenerate examples/playground/manifest.json
#   make playground-manifest-check — verify examples/playground/manifest.json freshness
#   make playground-check          — manifest freshness + curated analysis smoke + build hew-wasm
#   make playground-wasi-check     — focused curated manifest WASI runtime preflight
#   make ci-preflight              — dispatch a conservative local preflight from the current diff
#   make ci-preflight-strict       — run the local preflight superset that mirrors merge-queue gates
#   make wasm-dist    — build + copy WASM to hew.sh and hew.run
#   make test         — Rust workspace tests (fast path; excludes test-hew)
#   make test-all     — everything in test + stdlib + Hew tests (slow)
#   make test-rust         — just Rust workspace tests
#   make test-parser       — parser + lexer crate tests (narrow)
#   make test-types        — type-checker + parser + lexer crate tests (narrow)
#   make test-cli          — CLI crate tests (narrow)
#   make test-runtime-net  — runtime / analysis / lsp / std-net crate tests (narrow)
#   make test-runtime-unit — hew-runtime tests without heavy QUIC/TLS/profiler stack (~3× faster)
#   make test-hew          — run Hew test files (std/ *_test.hew)
#   make asan         — run the nightly rust-runtime ASan test command locally
#   make tsan         — run the nightly rust-runtime TSan test command locally
#   make lint         — cargo clippy (workspace + tests, warnings are errors) + hew fmt gate
#   make hew-fmt-check — check that std/ and examples/ .hew files are formatted (part of lint)
#   make fuzz-corpus    — regenerate ignored cargo-fuzz corpora from current fixtures/examples
#   make fuzz-smoke     — build and smoke-run cargo-fuzz targets locally
#   make clean        — remove build/, target/
# ============================================================================

.PHONY: all bootstrap install-hooks hew adze runtime stdlib wasm-runtime wasm playground-manifest playground-manifest-check playground-check playground-wasi-check ci-preflight ci-preflight-strict wasm-dist release
.PHONY: test test-all test-rust test-parser test-types test-cli test-runtime-net test-runtime-unit test-stdlib test-hew test-release-binary asan tsan lint runtime-poison-safe-lint stdlib-lint stdlib-errno-gate lint-wasm-todo hew-fmt-check grammar
.PHONY: clean install install-check uninstall verify-ffi
.PHONY: assemble assemble-release pre-release publish-docs
.PHONY: coverage coverage-summary coverage-lcov coverage-e2e coverage-combined
.PHONY: fuzz-corpus fuzz-smoke

# ── Configuration ───────────────────────────────────────────────────────────

# Installation prefix (used by `make install`)
PREFIX     ?= /usr/local/hew
DESTDIR    ?=

# Output directory — all usable artifacts land here as symlinks
BUILD_DIR  := build
COMMON_GIT_DIR := $(shell git rev-parse --git-common-dir 2>/dev/null)

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

# Sanitizer targets for the Rust runtime. The dedicated codegen sanitizer
# lane was retired together with the C++/MLIR subtree; the runtime ASan
# and TSan lanes here remain as local entry points for nightly coverage.
SANITIZER_RUST_TARGET ?= x86_64-unknown-linux-gnu
RUNTIME_ASAN_TARGET_DIR := target/sanitizer-runtime-asan
RUNTIME_TSAN_TARGET_DIR := target/sanitizer-runtime-tsan
FUZZ_TARGETS := fuzz_parse fuzz_lex fuzz_structured fuzz_machine fuzz_check fuzz_mir fuzz_msgpack
FUZZ_SMOKE_SECONDS ?= 45

# ── Default target ──────────────────────────────────────────────────────────

all: hew adze runtime stdlib assemble

# ── Rust targets ────────────────────────────────────────────────────────────

# Build the hew compiler driver (debug).
# hew-emit-v05 is the out-of-process LLVM object emitter required by
# `hew compile`; it must sit alongside the hew binary at runtime.
hew:
	cargo build -p hew-cli
	cargo build -p hew-codegen-rs --bin hew-emit-v05

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
playground-wasi-check:
	cargo test -p hew-cli --test wasi_run_e2e curated_playground_examples_run_under_wasi -- --exact
	cargo test -p hew-cli --test wasi_run_e2e supervisor_stays_on_the_unsupported_diagnostic_path_under_wasi -- --exact

# Conservative diff-based local preflight dispatcher.
# Usage: make ci-preflight ARGS="--dry-run" or ARGS="--base origin/main"
ci-preflight:
	scripts/ci-preflight-dispatcher.sh $(ARGS)

# Opt-in merge-queue parity preflight.
ci-preflight-strict:
	cargo fmt --all -- --check
	cargo clippy --workspace --tests -- -D warnings
	$(MAKE) playground-check
	$(MAKE) test
	$(MAKE) stdlib-lint

fuzz-corpus:
	scripts/fuzz/hydrate-corpus.sh

fuzz-smoke: fuzz-corpus
	@command -v cargo-fuzz >/dev/null 2>&1 || { echo "error: cargo-fuzz is required (cargo install cargo-fuzz)"; exit 127; }
	@rustup toolchain list | grep -q '^nightly' || { echo "error: Rust nightly toolchain is required (rustup install nightly)"; exit 127; }
	@cd hew-parser && for target in $(FUZZ_TARGETS); do \
		echo "==> cargo fuzz build $$target"; \
		cargo +nightly fuzz build "$$target"; \
	done
	@cd hew-parser && for target in $(FUZZ_TARGETS); do \
		echo "==> cargo fuzz smoke $$target ($(FUZZ_SMOKE_SECONDS)s)"; \
		cargo +nightly fuzz run "$$target" -- -max_total_time=$(FUZZ_SMOKE_SECONDS); \
	done

bootstrap: install-hooks

install-hooks:
	@common_git_dir="$(COMMON_GIT_DIR)"; \
	pre_commit_dir="$$common_git_dir/hooks/pre-commit.d"; \
	pre_push_dir="$$common_git_dir/hooks/pre-push.d"; \
	mkdir -p "$$pre_commit_dir" "$$pre_push_dir"; \
	format_link_target="../../../scripts/pre-commit-fmt.sh"; \
	preflight_link_target="../../../scripts/pre-push-ci-preflight.sh"; \
	wrote_links=""; \
	skipped_links=""; \
	dispatcher_summary=""; \
	if [ -L "$$pre_commit_dir/format" ] && [ "$$(readlink "$$pre_commit_dir/format")" = "$$format_link_target" ]; then \
		skipped_links="$$skipped_links\n  - $$pre_commit_dir/format -> $$format_link_target"; \
	else \
		ln -sfn "$$format_link_target" "$$pre_commit_dir/format"; \
		wrote_links="$$wrote_links\n  - $$pre_commit_dir/format -> $$format_link_target"; \
	fi; \
	if [ -L "$$pre_push_dir/ci-preflight" ] && [ "$$(readlink "$$pre_push_dir/ci-preflight")" = "$$preflight_link_target" ]; then \
		skipped_links="$$skipped_links\n  - $$pre_push_dir/ci-preflight -> $$preflight_link_target"; \
	else \
		ln -sfn "$$preflight_link_target" "$$pre_push_dir/ci-preflight"; \
		wrote_links="$$wrote_links\n  - $$pre_push_dir/ci-preflight -> $$preflight_link_target"; \
	fi; \
	hooks_path="$$(git config --global --get core.hooksPath 2>/dev/null; status=$$?; if [ $$status -eq 0 ]; then :; elif [ $$status -eq 1 ]; then printf ''; else exit $$status; fi)"; \
	if [ -z "$$hooks_path" ]; then \
		for hook_name in pre-commit pre-push; do \
			hook_path="$$common_git_dir/hooks/$$hook_name"; \
			hook_dir="$$common_git_dir/hooks/$$hook_name.d"; \
			if [ -e "$$hook_path" ] || [ -L "$$hook_path" ]; then \
				dispatcher_summary="$$dispatcher_summary\n  - $$hook_path (skipped: already exists)"; \
				continue; \
			fi; \
			{ \
				printf '%s\n' '#!/usr/bin/env bash'; \
				printf '%s\n' 'set -Eeuo pipefail'; \
				printf '%s\n' 'hook_name="$$(basename "$$0")"'; \
				printf '%s\n' 'hook_dir="$$(dirname "$$0")/$${hook_name}.d"'; \
				printf '%s\n' 'if [ ! -d "$$hook_dir" ]; then'; \
				printf '%s\n' '    exit 0'; \
				printf '%s\n' 'fi'; \
				printf '%s\n' 'for hook in "$$hook_dir"/*; do'; \
				printf '%s\n' '    [ -e "$$hook" ] || continue'; \
				printf '%s\n' '    [ -x "$$hook" ] || continue'; \
				printf '%s\n' '    "$$hook" "$$@"'; \
				printf '%s\n' 'done'; \
			} >"$$hook_path"; \
			chmod +x "$$hook_path"; \
			dispatcher_summary="$$dispatcher_summary\n  - $$hook_path (created fallback dispatcher)"; \
		done; \
	else \
		dispatcher_summary="$$dispatcher_summary\n  - skipped fallback dispatcher install (core.hooksPath=$$hooks_path)"; \
	fi; \
	echo "==> install-hooks summary"; \
	echo "Common git dir: $$common_git_dir"; \
	if [ -n "$$wrote_links" ]; then \
		echo "Symlinks written:"; \
		printf '%b\n' "$$wrote_links"; \
	else \
		echo "Symlinks written: none"; \
	fi; \
	if [ -n "$$skipped_links" ]; then \
		echo "Symlinks already correct:"; \
		printf '%b\n' "$$skipped_links"; \
	else \
		echo "Symlinks already correct: none"; \
	fi; \
	echo "Dispatcher status:"; \
	printf '%b\n' "$$dispatcher_summary"

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
	$(RELEASE_ENV) cargo build -p hew-cli --release
	$(RELEASE_ENV) cargo build -p hew-codegen-rs --bin hew-emit-v05 --release
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

# Build stdlib docs and print the wrangler deploy command.
# Requires a release binary; run `make release` first if target/release/hew
# is absent or stale.  The operator supplies the Cloudflare token via
# `wrangler login` or CLOUDFLARE_API_TOKEN in the shell — it is never in
# this file.
publish-docs: target/release/hew ## Build stdlib docs; print wrangler deploy command for hew-docs
	./target/release/hew doc std/ --output-dir target/doc/
	@echo ""
	@echo "Docs generated at target/doc/."
	@echo "Deploy with: wrangler pages deploy target/doc/ --project-name hew-docs"

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

test: test-rust

# test-all: the full sweep including the Hew JIT test suite.
# Hew tests (~354 functions through the Rust JIT path) are omitted from the
# default `test` target because they take ~5 min locally and are not called
# by CI (which uses cargo nextest directly). Use `make test-all` when you
# want the previous behaviour.
# TODO: Add test-stdlib to `test-all` unconditionally once stdlib files are type-check clean
test-all: test test-stdlib test-hew

test-rust:
	@if command -v cargo-nextest >/dev/null 2>&1 || cargo nextest --version >/dev/null 2>&1; then \
		cargo nextest run --workspace --profile ci; \
	else \
		echo "WARNING: cargo-nextest not installed — per-test timeouts are not enforced." >&2; \
		echo "         Install with: cargo install cargo-nextest" >&2; \
		cargo test; \
	fi

test-parser:
	cargo nextest run --profile ci -p hew-parser -p hew-lexer

test-types:
	cargo nextest run --profile ci -p hew-types -p hew-parser -p hew-lexer

test-cli:
	cargo nextest run --profile ci -p hew-cli -p adze-cli

test-runtime-net:
	cargo nextest run --profile ci --no-fail-fast \
		-p hew-runtime \
		-p hew-analysis \
		-p hew-lsp \
		-p hew-std-net-dns \
		-p hew-std-net-http \
		-p hew-std-net-ipnet \
		-p hew-std-net-mime \
		-p hew-std-net-quic \
		-p hew-std-net-smtp \
		-p hew-std-net-tls \
		-p hew-std-net-url \
		-p hew-std-net-websocket

# Fast hew-runtime target: runs lib unit tests and all integration tests without the heavy
# QUIC/TLS/profiler feature stack (quinn, rustls, rcgen, ring, hyper, snow).
# Compile time is ~3× lower than the default-features build (measured: ~32s vs ~85s per binary).
# Profiler allocator tests in transport.rs are skipped (they require feature = "profiler").
# Run `cargo test -p hew-runtime` for the full suite including QUIC, TLS, and profiler paths.
test-runtime-unit:
	cargo nextest run --profile ci -p hew-runtime --no-default-features

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

test-hew: hew runtime stdlib
	@echo "==> Running Hew test files"
	$(DEBUG_DIR)/hew test tests/hew/

# Nightly rust-runtime ASan command (Linux/nightly toolchain required).
asan:
	CARGO_TARGET_DIR=$(RUNTIME_ASAN_TARGET_DIR) \
	RUSTFLAGS="-Zsanitizer=address -Cforce-frame-pointers=yes" \
	ASAN_OPTIONS="detect_leaks=1" \
	cargo +nightly test --target $(SANITIZER_RUST_TARGET) -p hew-runtime --lib

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

lint: runtime-poison-safe-lint lint-wasm-todo verify-ffi hew-fmt-check
	cargo clippy --workspace --tests -- -D warnings

# Check that std/ and examples/ .hew sources are formatted.
# Run `find std examples -name "*.hew" -print0 | xargs -0 hew fmt` to fix.
hew-fmt-check: hew
	@echo "==> hew-fmt-check: checking std/ and examples/ .hew sources"
	@find std examples -name "*.hew" -print0 \
	    | xargs -0 $(DEBUG_DIR)/hew fmt --check \
	    && echo "hew-fmt-check passed: all .hew sources are formatted." \
	    || { echo "error: unformatted .hew sources found — run 'find std examples -name \"*.hew\" -print0 | xargs -0 hew fmt' to fix." >&2; exit 1; }

# Smoke-test the release binary with `hew run` to catch process-exit aborts
# (e.g. libc++ ABI mismatch at locale destructor — issue #1606).
# Builds release binary then runs a trivial program and checks exit 0 + output.
test-release-binary:
	scripts/test-release-binary.sh

stdlib-errno-gate:
	@bash -euo pipefail -c '\
		echo "==> stdlib-errno-gate: checking for banned string-match error patterns in std/"; \
		if rg -n --glob "*.hew" "os error" std/; then \
			echo "error: \047os error\047 string patterns found in std/ — use errno-based error classification instead." >&2; \
			exit 1; \
		fi; \
		if rg -n --glob "*.hew" "contains\\(\\\"Connection refused" std/; then \
			echo "error: OS message string \047Connection refused\047 used in .contains() in std/ — use errno-based error classification instead." >&2; \
			exit 1; \
		fi; \
		if rg -n --glob "*.hew" "contains\\(\\\"Permission denied" std/; then \
			echo "error: OS message string \047Permission denied\047 used in .contains() in std/ — use errno-based error classification instead." >&2; \
			exit 1; \
		fi; \
		if rg -n --glob "*.hew" "contains\\(\\\"timed out" std/; then \
			echo "error: OS message string \047timed out\047 used in .contains() in std/ — use errno-based error classification instead." >&2; \
			exit 1; \
		fi; \
		echo "stdlib-errno-gate passed: no banned string-match error patterns in std/."'

stdlib-lint: stdlib-errno-gate
	bash scripts/lint-stdlib-int-surface.sh

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

# Reject WASM-TODO comments that do not carry an issue reference.
# Every actionable WASM-TODO must use: WASM-TODO(#NNN): <description>
# Umbrella issue: https://github.com/hew-lang/hew/issues/1451
lint-wasm-todo:
	bash scripts/lint-wasm-todo-issue-ref.sh

# ── Coverage ───────────────────────────────────────────────────────────────
#
#   make coverage         — Rust unit/integration tests only (cargo llvm-cov)
#   make coverage-e2e     — E2E tests exercising the full compile pipeline
#   make coverage-combined — both merged into a single report
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

# E2E coverage: instruments hew CLI + runtime, generates report
coverage-e2e: stdlib
	@echo "==> Building hew CLI + runtime with coverage instrumentation"
	RUSTFLAGS="-C instrument-coverage" cargo build -p hew-cli -p hew-runtime
	@rm -rf $(COV_E2E_DIR) && mkdir -p $(COV_E2E_DIR)
	@echo "==> Merging profraw data"
	$(LLVM_PROFDATA) merge -sparse $(COV_E2E_DIR)/*.profraw -o $(COV_DIR)/e2e.profdata 2>/dev/null || true
	@echo "==> E2E coverage summary (Rust frontend):"
	@if [ -f $(COV_DIR)/e2e.profdata ]; then \
		$(LLVM_COV) report target/debug/hew \
		  -instr-profile=$(COV_DIR)/e2e.profdata \
		  --ignore-filename-regex='(\.cargo|rustc|/usr/)' \
		  -summary-only; \
	else \
		echo "No profraw data captured."; \
	fi

# Combined coverage: cargo tests + Rust frontend coverage merged
coverage-combined: stdlib
	@echo "==> Phase 1: Running cargo tests with coverage"
	cargo llvm-cov --workspace --exclude hew-wasm --no-report
	@echo "==> Phase 2: Building hew CLI with coverage instrumentation"
	RUSTFLAGS="-C instrument-coverage" cargo build -p hew-cli -p hew-runtime
	@mkdir -p $(COV_DIR)/merged
	@cp target/llvm-cov-target/*.profraw $(COV_DIR)/merged/ 2>/dev/null || true
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

# ── FFI symbol verification ───────────────────────────────────────────────
# Validates that every hew-runtime #[no_mangle] export is classified in
# scripts/jit-symbol-classification.toml (stable vs internal).

verify-ffi:
	python3 scripts/verify-ffi-symbols.py --classify stable --validate > /dev/null

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
	cargo clean
	rm -rf $(GRAMMAR_OUT) .tmp/Hew.g4
	rm -rf $(COV_DIR)
