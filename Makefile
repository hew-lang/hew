# ============================================================================
# Hew Developer Makefile
#
# Builds all project artifacts into build/ with a predictable layout:
#
#   build/
#     bin/hew              — compiler driver (Rust)
#     bin/hew-codegen      — MLIR code generator (C++)
#     bin/adze             — package manager (Rust)
#     lib/libhew_runtime.a — actor runtime (Rust staticlib)
#     lib/wasm32-wasip1/libhew_runtime.a — WASM runtime (if built)
#     std/*.hew            — standard library stubs
#
# Each entry under build/ is a symlink into the real Cargo/CMake output dirs,
# so there are no redundant copies and incremental builds just work.
#
# Usage:
#   make              — build everything (debug)
#   make release      — build everything (release, optimized)
#   make hew          — just the compiler driver
#   make adze         — just the package manager
#   make codegen      — just hew-codegen (C++ MLIR)
#   make runtime      — just libhew_runtime.a
#   make wasm-runtime — WASM runtime (requires: rustup target add wasm32-wasip1)
#   make test         — run all tests (Rust + codegen)
#   make test-rust    — just Rust workspace tests
#   make test-codegen — just hew-codegen ctest (native E2E + unit)
#   make test-wasm    — just WASM E2E tests (requires wasmtime)
#   make lint         — cargo clippy
#   make clean        — remove build/, target/, hew-codegen/build/
# ============================================================================

.PHONY: all hew adze codegen runtime stdlib wasm-runtime release
.PHONY: test test-all test-rust test-codegen test-wasm test-cpp lint grammar
.PHONY: clean install install-check uninstall
.PHONY: assemble assemble-release

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

# ── Default target ──────────────────────────────────────────────────────────

all: hew adze codegen runtime stdlib assemble

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

# Build all stdlib per-package staticlibs (needed for linking programs that import stdlib modules)
STDLIB_PACKAGES := \
    hew-std-encoding-base64 hew-std-encoding-compress hew-std-encoding-csv \
    hew-std-encoding-json hew-std-encoding-markdown \
    hew-std-encoding-msgpack hew-std-encoding-protobuf hew-std-encoding-toml \
    hew-std-encoding-yaml \
    hew-std-crypto-crypto hew-std-crypto-jwt hew-std-crypto-password \
    hew-std-net-http hew-std-net-ipnet hew-std-net-smtp \
    hew-std-net-url hew-std-net-websocket \
    hew-std-time-cron hew-std-time-datetime \
    hew-std-text-regex hew-std-text-semver \
    hew-std-misc-uuid hew-std-misc-log

stdlib:
	cargo build $(addprefix -p ,$(STDLIB_PACKAGES))

# Build the WASM runtime (requires wasm32-wasip1 target: rustup target add wasm32-wasip1)
wasm-runtime:
	cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features

# ── C++ codegen target ──────────────────────────────────────────────────────

# Build hew-codegen: configure with CMake if needed, then build with Ninja.
# Requires LLVM 21 and MLIR to be installed.
#
# Auto-detects LLVM/MLIR paths:
#   Linux (apt.llvm.org):  /usr/lib/llvm-<ver>/lib/cmake/{llvm,mlir}
#   macOS (Homebrew):      $(brew --prefix llvm@<ver>)/lib/cmake/{llvm,mlir}
#
# Override with: make codegen LLVM_DIR=/path/to/llvm MLIR_DIR=/path/to/mlir
#            or: make codegen LLVM_PREFIX=/usr/lib/llvm-21

# Auto-detect LLVM prefix if not explicitly provided
ifndef LLVM_PREFIX
  # Try versioned apt.llvm.org paths (21, 20, 19...)
  LLVM_PREFIX := $(firstword $(wildcard /usr/lib/llvm-21 /usr/lib/llvm-20 /usr/lib/llvm-19))
  # Try Homebrew on macOS
  ifeq ($(LLVM_PREFIX),)
    LLVM_PREFIX := $(shell brew --prefix llvm@21 2>/dev/null || brew --prefix llvm 2>/dev/null)
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

codegen:
	cmake -B hew-codegen/build -G Ninja \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		$(CMAKE_EXTRA_ARGS) \
		-S hew-codegen
	cmake --build hew-codegen/build

# Create symlinks from build/ into the real output locations.
# This gives you one stable directory to point PATH at during development.
assemble: | hew adze codegen runtime
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib $(BUILD_DIR)/std
	@# Compiler driver
	@ln -sfn ../../$(DEBUG_DIR)/hew                $(BUILD_DIR)/bin/hew
	@# MLIR code generator
	@ln -sfn ../../hew-codegen/build/src/hew-codegen    $(BUILD_DIR)/bin/hew-codegen
	@# Package manager
	@ln -sfn ../../$(DEBUG_DIR)/adze               $(BUILD_DIR)/bin/adze
	@# Runtime library
	@ln -sfn ../../$(DEBUG_DIR)/libhew_runtime.a   $(BUILD_DIR)/lib/libhew_runtime.a
	@# WASM runtime (symlink if built)
	@if [ -f $(WASM_DEBUG_DIR)/libhew_runtime.a ]; then \
		mkdir -p $(BUILD_DIR)/lib/wasm32-wasip1; \
		ln -sfn ../../../$(WASM_DEBUG_DIR)/libhew_runtime.a \
			$(BUILD_DIR)/lib/wasm32-wasip1/libhew_runtime.a; \
	fi
	@# Standard library stubs (one symlink per file so the dir stays flat)
	@for f in std/*.hew; do \
		ln -sfn "../../$$f" "$(BUILD_DIR)/std/$$(basename $$f)"; \
	done
	@echo "build/ assembled (debug). Add to PATH:"
	@echo "  export PATH=\"$(CURDIR)/$(BUILD_DIR)/bin:\$$PATH\""

# ── Release build ───────────────────────────────────────────────────────────

# Build everything in release mode and repoint the build/ symlinks.
release:
	cargo build -p hew-cli --release
	cargo build -p adze-cli --release
	cargo build -p hew-runtime --release
	cargo build $(addprefix -p ,$(STDLIB_PACKAGES)) --release
	cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features --release
	cmake -B hew-codegen/build -G Ninja \
		-DCMAKE_BUILD_TYPE=Release \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		$(CMAKE_EXTRA_ARGS) \
		-S hew-codegen
	cmake --build hew-codegen/build
	$(MAKE) assemble-release

# Assemble build/ with release symlinks.
assemble-release:
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib $(BUILD_DIR)/std
	@ln -sfn ../../$(RELEASE_DIR)/hew              $(BUILD_DIR)/bin/hew
	@ln -sfn ../../hew-codegen/build/src/hew-codegen    $(BUILD_DIR)/bin/hew-codegen
	@ln -sfn ../../$(RELEASE_DIR)/adze             $(BUILD_DIR)/bin/adze
	@ln -sfn ../../$(RELEASE_DIR)/libhew_runtime.a $(BUILD_DIR)/lib/libhew_runtime.a
	@if [ -f $(WASM_RELEASE_DIR)/libhew_runtime.a ]; then \
		mkdir -p $(BUILD_DIR)/lib/wasm32-wasip1; \
		ln -sfn ../../../$(WASM_RELEASE_DIR)/libhew_runtime.a \
			$(BUILD_DIR)/lib/wasm32-wasip1/libhew_runtime.a; \
	fi
	@for f in std/*.hew; do \
		ln -sfn "../../$$f" "$(BUILD_DIR)/std/$$(basename $$f)"; \
	done
	@echo "build/ assembled (release)."

# ── Tests ───────────────────────────────────────────────────────────────────

test: test-rust test-codegen

test-all: test-rust test-codegen test-wasm

test-rust:
	cargo test

test-codegen: hew codegen runtime stdlib
	cd hew-codegen/build && ctest --output-on-failure -LE wasm

test-wasm: hew codegen wasm-runtime
	cd hew-codegen/build && ctest --output-on-failure -L wasm

# Legacy alias
test-cpp: test-codegen

# ── Lint ────────────────────────────────────────────────────────────────────

lint:
	cargo clippy --workspace

# ── ANTLR4 grammar validation ──────────────────────────────────────────────
# Requires Java and the ANTLR4 jar. This is rarely needed — only when
# modifying docs/specs/Hew.g4.

ANTLR4_JAR  ?= /tmp/antlr-4.13.2-complete.jar
JAVA_HOME   ?= /usr/lib/jvm/java-21-openjdk-amd64
JAVA        := $(JAVA_HOME)/bin/java
JAVAC       := $(JAVA_HOME)/bin/javac
GRAMMAR     := docs/specs/Hew.g4
GRAMMAR_OUT := .tmp/hew-grammar-test
HEW_FILES   := $(wildcard examples/*.hew)

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
	install -m 755 hew-codegen/build/src/hew-codegen      $(DESTDIR)$(PREFIX)/bin/hew-codegen
	install -m 755 $(RELEASE_DIR)/adze               $(DESTDIR)$(PREFIX)/bin/adze
	install -m 644 $(RELEASE_DIR)/libhew_runtime.a   $(DESTDIR)$(PREFIX)/lib/libhew_runtime.a
	@if [ -f $(WASM_RELEASE_DIR)/libhew_runtime.a ]; then \
		install -d $(DESTDIR)$(PREFIX)/lib/wasm32-wasip1; \
		install -m 644 $(WASM_RELEASE_DIR)/libhew_runtime.a \
			$(DESTDIR)$(PREFIX)/lib/wasm32-wasip1/libhew_runtime.a; \
	fi
	install -m 644 std/*.hew                         $(DESTDIR)$(PREFIX)/std/
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
	@test -f hew-codegen/build/src/hew-codegen \
		|| { echo "Error: hew-codegen not built. Run 'make release' first."; exit 1; }
	@test -f $(RELEASE_DIR)/libhew_runtime.a \
		|| { echo "Error: release runtime not built. Run 'make release' first."; exit 1; }

uninstall:
	rm -rf $(DESTDIR)$(PREFIX)
	@echo "==> Removed $(DESTDIR)$(PREFIX)"

# ── Cleanup ─────────────────────────────────────────────────────────────────

clean:
	rm -rf $(BUILD_DIR)
	rm -rf hew-codegen/build
	cargo clean
	rm -rf $(GRAMMAR_OUT) .tmp/Hew.g4
