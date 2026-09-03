# ============================================================================
# Hew Developer Makefile
#
# Authority boundary: Cargo owns Rust dependency resolution, fingerprints, and
# crate compilation. This Makefile is the single repository authority for which
# crate artifacts form a usable Hew toolchain, how they are assembled, and
# which verification gates run. xtask is reserved for tasks that need Hew Rust
# APIs directly; it does not carry a second build or gate graph.
#
# Builds all project artifacts into build/ with a predictable layout:
#
#   build/
#     bin/hew              — compiler driver + package manager (Rust)
#     bin/hew-observe      — TUI actor observer (Rust)
#     bin/hew-lsp          — language server (Rust)
#     lib/libhew.a         — combined library: runtime + all stdlib packages
#     lib/<native-triple>/libhew.a — host/cross consumer-linkable archives
#     lib/wasm32-wasip1/*.a — WASM runtime + focused wire stdlib archives
#     std/*.hew            — standard library stubs
#
# Each entry under build/ is a symlink into the real Cargo output dirs,
# so there are no redundant copies and incremental builds just work.
#
# Usage:
#   make              — build a usable compiler toolchain, including supported
#                       native cross archives and WASI runtime archives
#   make release      — build everything (release, optimized)
#   make pre-release  — release + validate on all platforms before tagging
#   make publish-docs — build stdlib docs + print wrangler deploy command (operator runs wrangler)
#   make hew          — release-lib compiler + archive, staged at build/bin/hew
#   make hew-debug    — debug compiler + archive, staged at build/bin/hew-debug
#   make hew-native   — compiler driver + native libhew archive for `hew build`
#   make observe      — just the TUI observer (hew-observe)
#   make observe-functional-test — HTTP-backed functional observe harness
#   make mqtt-broker-e2e       — real MQTT broker publish/delivery oracle
#   make libhew-link-race-test   — real multi-process libhew.a bootstrap-race proof
#   make runtime      — just libhew_runtime.a
#   make stdlib       — all stdlib packages + combine into libhew.a
#   make wasm-runtime — WASM runtime + wire JSON/YAML/TOML archives
#   make stage-release-package — assemble a validated distributable tree
#   make dev-dist    — package the current development build as a portable .tgz
#   make wasm         — build hew-wasm (browser WASM via wasm-pack)
#   make baselines                 — regenerate deterministic generated metadata
#   make baselines-check           — verify deterministic generated metadata
#   make wasm-capability-check     — verify manifest-owned generated outputs
#   make playground-manifest-check — verify examples/playground/manifest.json freshness
#   make sandbox-fixtures-check    — verify sandbox VM bytecode fixtures are fresh
#   make sandbox-vm-deps           — install hew-sandbox-vm npm deps (hash-stamped, idempotent)
#   make sandbox-parity            — native hew run ↔ sandbox VM parity harness
#   make playground-check          — manifest freshness + full hew-wasm test suite + build hew-wasm
#   make playground-wasi-check     — focused curated manifest WASI runtime preflight
#   make playground-verify         — native run of every runnable playground example vs. its .expected
#   make licenses-check            — verify THIRD-PARTY-LICENSES is current (used in CI)
#   make preflight                 — run every unconditional Linux gate, fail-fast
#   make ci-preflight              — compatibility alias for make preflight
#   make ci-preflight-smoke        — fast smoke tier: fmt + in-process tests (<5 min)
#   make wasm-dist    — build + copy WASM to hew.sh and hew.run
#   make test         — Rust workspace tests with the exact known-failure ratchet
#   make test-strict  — Rust workspace tests; require every test to pass
#   make macos-leak-oracle — ratcheted local leaks(1) + poisoned-allocator corpus
#   make test-leak-oracle-selftest — fail-closed leak runner/harness counterfactuals
#   make test-cabi         — C-ABI crate tests (narrow; excluded from the workspace run)
#   make test-compiler-pipeline — compiler ladder + CLI pipeline tests (narrow)
#   make test-vertical-slice — end-to-end Hew compiler oracle
#   make test-package-install — hew install -> Hew import consumer proof
#   make test-runtime-unit — hew-runtime tests without heavy QUIC/TLS/profiler stack (~3× faster)
#   make test-ux-examples  — run examples/ux + examples/progressive tutorials against .expected files
#   make asan         — run the nightly rust-runtime ASan test command locally
#   make tsan         — run the nightly rust-runtime TSan test command locally
#   make miri         — run the curated rust-runtime Miri allowlist locally
#   make lint         — cargo clippy (workspace + tests, warnings are errors) + hew fmt gate
#   make structural-lint — pinned ast-grep scan + compiler authority ratchets
#   make hew-fmt-check — check that std/ and examples/ .hew files are formatted (part of lint)
#   make fuzz-corpus    — regenerate ignored cargo-fuzz corpora from current fixtures/examples
#   make clean        — remove generated build and test artifacts
# ============================================================================

.PHONY: all build bootstrap install-hooks help shell-script-lint test-install-version-resolution actionlint hew hew-debug hew-profile-check hew-native shared-host-debug hew-lsp observe observe-functional-test mqtt-broker-e2e libhew-link-race-test runtime stdlib wasm-runtime wasm wasm-capability wasm-capability-check playground-manifest playground-manifest-check sandbox-fixtures sandbox-fixtures-check sandbox-vm-deps sandbox-parity playground-check playground-wasi-check playground-verify preflight ci-preflight ci-preflight-smoke ci-local-linux wasm-dist release licenses licenses-check baselines baselines-check
.PHONY: test test-strict ratchet-accounting ratchet-accounting-nextest test-ratchet-accounting-runner macos-leak-oracle test-leak-oracle-selftest test-cabi test-compiler-pipeline test-compiler-lifecycle test-opaque-resource-lifecycle-matrix test-opaque-resource-lifecycle-matrix-external test-vertical-slice test-pkg-import test-package-install test-runtime-unit test-hew-ratchet test-core-matrix core-matrix-record funcupdate-mir-baselines-golden test-o2-differential o2-differential-selftest test-stdlib-ratchet test-ux-examples ux-examples-expect test-surface-examples surface-examples-expect test-example-expectations-selftest test-release-binary test-release-lib-link asan asan-fixtures test-asan-fixture-selftest tsan miri lint structural-lint structural-lint-bootstrap structural-lint-bootstrap-install test-ast-grep-contract stdlib-lint stdlib-errno-gate legacy-path-syntax-lint hew-fmt-check test-migrate-corpus doc-ratchet-selftest verify-sys-lane-closure test-sys-lane-closure hew-fmt-property test-build-harness forced-cancel-composite-check
.PHONY: test-ownership-balance-corpus test-ownership-balance-runner-selftest
.PHONY: stdlib-user-build-clean
.PHONY: clean install uninstall verify-ffi ffi-ownership-ratchet-record test-verify-ffi test-cabi-surface cabi-surface cabi-surface-check
.PHONY: assemble assemble-release stage-release-package dev-dist pre-release windows-release-candidate publish-docs
.PHONY: coverage coverage-summary coverage-lcov coverage-runtime coverage-combined coverage-branch
.PHONY: fuzz-corpus fuzz-oracle fuzz-oracle-selftest fuzz-smoke fuzz-smoke-bootstrap-install
.PHONY: dogfood-compile-measure bench-mir
.PHONY: compile-determinism-verify compile-determinism-verify-build compile-determinism-selftest compile-determinism-selftest-build
.PHONY: checked-mir-verify checked-mir-golden checked-mir-run checked-mir-expect
.PHONY: hew-check-all
.PHONY: sir-coverage sir-parity
.PHONY: test-journeys check-time-ratchet check-time-ratchet-record
.PHONY: size-ratchet size-ratchet-record

help:
	@$(PYTHON) scripts/make-help.py

LINT_GATES += shell-script-lint
shell-script-lint:
	@$(PYTHON) scripts/shell-script-lint.py

# The installer's newest-tag semver picker (installers/install.sh's
# pick_newest_tag()) has no coverage anywhere else in the build: it never
# runs from a compiled binary, only from a shell function that curl | sh
# executes directly. Wired into LINT_GATES so it runs on every PR the same
# way shell-script-lint does, without a release download or network access.
LINT_GATES += test-install-version-resolution
test-install-version-resolution:
	@sh installers/test-pick-newest-tag.sh


# Local GitHub Actions syntax, expression, local-action input, and embedded-shell
# validation. Run this before pushing workflow edits: a malformed CI workflow
# may be unable to start the job that would otherwise try to validate it.
actionlint:
	actionlint -color

# Static workflow validation produces no build artifacts.

# ── Configuration ───────────────────────────────────────────────────────────

# Repository scripts require Python 3.12+ (PEP 701 and stdlib tomllib).
# Override with `make PYTHON=/path/to/python3.12 <target>` when needed.
ifeq ($(OS),Windows_NT)
PYTHON ?= python
else
PYTHON ?= python3
endif
PYTHON_VERSION_CHECK := $(shell $(PYTHON) -c 'import sys; version = ".".join(map(str, sys.version_info[:3])); print(("ok" if sys.version_info >= (3, 12) else "unsupported") + " " + version)' 2>/dev/null)
PYTHON_VERSION := $(word 2,$(PYTHON_VERSION_CHECK))

ifeq ($(PYTHON_VERSION_CHECK),)
$(error Python 3.12 or newer is required; could not run `$(PYTHON)`. Install Python 3.12+ and rerun with `make PYTHON=/path/to/python3.12`)
endif
ifneq ($(word 1,$(PYTHON_VERSION_CHECK)),ok)
$(error Python 3.12 or newer is required; found Python $(PYTHON_VERSION) via `$(PYTHON)`. Install Python 3.12+ and rerun with `make PYTHON=/path/to/python3.12`)
endif

# Installation prefix (used by `make install`)
PREFIX     ?= /usr/local/hew
DESTDIR    ?=

# Output directory — all usable artifacts land here as symlinks
BUILD_DIR  := build
COMMON_GIT_DIR := $(shell git rev-parse --git-common-dir 2>/dev/null)
# Resolve helper scripts relative to the selected Makefile while package inputs
# remain relative to the caller's source checkout.
MAKEFILE_ROOT := $(patsubst %/,%,$(dir $(abspath $(firstword $(MAKEFILE_LIST)))))

# Cargo profile directory names.
#
# Cargo does not always write into `target/`: CARGO_TARGET_DIR, build.target-dir
# in any .cargo/config.toml, CARGO_BUILD_TARGET, build.target and an explicit
# --target each move the output directory. A rule that builds through Cargo and
# then touches, inspects or installs a hard-coded `target/debug` is looking at a
# different file than the one Cargo just wrote — which is precisely how a
# an old libhew.a in a shared scratch target directory was mistaken for the
# output Cargo had just produced.
# scripts/cargo-output-dir.py resolves the real directory the way Cargo does,
# and everything below is derived from it.
#
# TARGET_TRIPLE passes --target through to the native cargo invocations here;
# leave it empty to build for the host.
TARGET_TRIPLE ?=
CARGO_TARGET_FLAG := $(if $(TARGET_TRIPLE),--target $(TARGET_TRIPLE),)
CARGO_TARGET_ROOT := $(shell $(PYTHON) scripts/cargo-output-dir.py --root)
CARGO_NATIVE_OUT := $(shell $(PYTHON) scripts/cargo-output-dir.py --native $(CARGO_TARGET_FLAG))
ifeq ($(CARGO_NATIVE_OUT),)
$(error scripts/cargo-output-dir.py could not resolve Cargo's output directory)
endif

DEBUG_DIR  := $(CARGO_NATIVE_OUT)/debug
RELEASE_DIR := $(CARGO_NATIVE_OUT)/release
# The SHIPPED libhew.a builds under the non-LTO `release-lib` cargo profile:
# a fat-LTO archive cannot dedupe its folded libstd against external Rust
# staticlibs (`--link-lib` packages), so packaging must never ship the
# `release` archive. See `[profile.release-lib]` in Cargo.toml.
RELEASE_LIB_DIR := $(CARGO_NATIVE_OUT)/release-lib
ifeq ($(OS),Windows_NT)
DEBUG_HEW := $(DEBUG_DIR)/hew.exe
RELEASE_HEW := $(RELEASE_DIR)/hew.exe
RELEASE_LIB_HEW := $(RELEASE_LIB_DIR)/hew.exe
RELEASE_LIBHEW := $(RELEASE_LIB_DIR)/hew.lib
else
DEBUG_HEW := $(DEBUG_DIR)/hew
RELEASE_HEW := $(RELEASE_DIR)/hew
RELEASE_LIB_HEW := $(RELEASE_LIB_DIR)/hew
RELEASE_LIBHEW := $(RELEASE_LIB_DIR)/libhew.a
endif
# The wasm archives are always built with an explicit `--target wasm32-wasip1`,
# which outranks the environment, so they hang off the target root rather than
# the native output directory.
WASM_DEBUG_DIR  := $(CARGO_TARGET_ROOT)/wasm32-wasip1/debug
WASM_RELEASE_DIR := $(CARGO_TARGET_ROOT)/wasm32-wasip1/release

# Symlinks under build/ point into the Cargo output directory. While that
# directory is inside the repository the links stay relative, so a moved
# checkout keeps working; an out-of-tree CARGO_TARGET_DIR resolves to an
# absolute path, which must not have `../` hops prepended to it.
ifeq ($(filter /%,$(CARGO_TARGET_ROOT)),)
LINK_UP2 := ../../
LINK_UP3 := ../../../
else
LINK_UP2 :=
LINK_UP3 :=
endif

# ── The combined runtime + stdlib archive ───────────────────────────────────
#
# `cargo build -p hew-cli` produces the compiler DRIVER only; linking a compiled
# Hew program additionally needs hew-lib's staticlib sitting beside it.
#
# Cargo emits `hew.lib` on MSVC and `libhew.a` everywhere else; keep the whole
# build graph cross-platform so a fresh Windows host uses the same edges as
# Linux/macOS rather than a bespoke manual `cargo build -p hew-lib` follow-up.
#
# Do not put `$(LIBHEW)` itself in Make's target graph. Cargo permits target
# directories containing spaces, while Make splits expanded target and
# prerequisite names on whitespace. The phony build edge below lets Cargo
# perform its own incremental freshness check without corrupting the graph.
ifeq ($(OS),Windows_NT)
LIBHEW_NAME := hew.lib
else
LIBHEW_NAME := libhew.a
endif
LIBHEW := $(DEBUG_DIR)/$(LIBHEW_NAME)

TEST_RUN_ENV := HEW_TEST_NO_BUILD=1

# Ordinary development and pull-request runs keep executing known failing tests
# while rejecting every unrecorded failure, changed outcome, missing test,
# process signal, setup error, or malformed report. Release gates use the same
# nextest invocation through `test-strict`, but require an all-pass exit.
NEXTEST_WORKSPACE_FILTER ?=
NEXTEST_WORKSPACE_SELECTION_ARGS := --workspace --exclude hew-cabi --profile ci
NEXTEST_WORKSPACE_ARGS := $(NEXTEST_WORKSPACE_SELECTION_ARGS) --no-fail-fast
NEXTEST_FULL_INVENTORY := $(CARGO_TARGET_ROOT)/nextest-full-inventory.json
NEXTEST_SELECTED_INVENTORY := $(CARGO_TARGET_ROOT)/nextest-selected-inventory.json
NEXTEST_RATCHET_INVENTORY_ARGS :=
NEXTEST_PREPARE_FULL_INVENTORY := :
NEXTEST_PREPARE_SELECTED_INVENTORY := :
ifneq ($(strip $(NEXTEST_WORKSPACE_FILTER)),)
# Ask nextest for both sides of the selection boundary. The ratchet may ignore
# an absent expected test only when the unfiltered inventory still contains it
# and the exact active filter excludes it; a selected test missing from JUnit
# remains a hard error.
NEXTEST_WORKSPACE_ARGS += --filterset '$(NEXTEST_WORKSPACE_FILTER)'
NEXTEST_RATCHET_INVENTORY_ARGS := --full-inventory "$(NEXTEST_FULL_INVENTORY)" --selected-inventory "$(NEXTEST_SELECTED_INVENTORY)"
NEXTEST_PREPARE_FULL_INVENTORY := $(TEST_RUN_ENV) cargo nextest list $(NEXTEST_WORKSPACE_SELECTION_ARGS) --message-format json > "$(NEXTEST_FULL_INVENTORY)"
NEXTEST_PREPARE_SELECTED_INVENTORY := $(TEST_RUN_ENV) cargo nextest list $(NEXTEST_WORKSPACE_SELECTION_ARGS) --filterset '$(NEXTEST_WORKSPACE_FILTER)' --message-format json > "$(NEXTEST_SELECTED_INVENTORY)"
endif
NEXTEST_JUNIT := $(CARGO_TARGET_ROOT)/nextest/ci/junit.xml
NEXTEST_RATCHET_JUNIT := $(CARGO_TARGET_ROOT)/nextest/ci/ratchet.xml
NEXTEST_FAILURE_LEDGER := scripts/nextest-expected-failures.tsv
RATCHET_STRICT_RECOVERIES ?= 0
RATCHET_STRICT_RECOVERIES_ARG := $(if $(filter 1 true yes,$(RATCHET_STRICT_RECOVERIES)),--strict-recoveries,)

ifndef NEXTEST_PLATFORM
ifeq ($(OS),Windows_NT)
NEXTEST_PLATFORM := windows
else ifeq ($(shell uname -s),Darwin)
NEXTEST_PLATFORM := macos
else ifeq ($(shell uname -s),Linux)
NEXTEST_PLATFORM := linux
else ifeq ($(shell uname -s),FreeBSD)
NEXTEST_PLATFORM := freebsd
else
NEXTEST_PLATFORM := unsupported
endif
endif

# wasm32-wasip1 has no profiler runtime. A Make-owned artifact build can run
# under cargo-llvm-cov's exported environment, so scrub only its
# instrumentation controls while retaining CARGO_TARGET_DIR and a developer's
# ordinary compiler wrapper (for example sccache).
WASM_UNINSTRUMENTED_ENV := env \
	-u LLVM_PROFILE_FILE \
	-u __CARGO_LLVM_COV_RUSTC_WRAPPER \
	-u __CARGO_LLVM_COV_RUSTC_WRAPPER_RUSTFLAGS \
	-u __CARGO_LLVM_COV_RUSTC_WRAPPER_CRATE_NAMES \
	-u __CARGO_LLVM_COV_RUSTC_WRAPPER_PRE_EXISTING \
	-u CARGO_LLVM_COV

# cargo-llvm-cov replaces RUSTC_WRAPPER and records the caller's wrapper here.
# Restore that value only when the coverage shim was installed; ordinary
# builds leave the user's environment or Cargo config untouched.
ifneq ($(strip $(__CARGO_LLVM_COV_RUSTC_WRAPPER)$(CARGO_LLVM_COV)),)
WASM_UNINSTRUMENTED_ENV += -u RUSTC_WRAPPER
ifneq ($(origin __CARGO_LLVM_COV_RUSTC_WRAPPER_PRE_EXISTING),undefined)
WASM_UNINSTRUMENTED_ENV += RUSTC_WRAPPER="$(__CARGO_LLVM_COV_RUSTC_WRAPPER_PRE_EXISTING)"
endif
endif

# Host triple used to populate lib/<triple>/ for target-aware lib lookup.
HOST_TRIPLE := $(shell rustc -vV 2>/dev/null | awk '/^host:/ { print $$2 }')
EFFECTIVE_CARGO_TARGET := $(shell $(PYTHON) scripts/cargo-output-dir.py --triple $(CARGO_TARGET_FLAG))
# Make assembles a runnable host toolchain. Cross-target native archives have
# dedicated targets; accepting a foreign Cargo default here would mislabel that
# archive as the host library and eventually try to execute a foreign hew.
.PHONY: require-host-cargo-target
require-host-cargo-target:
	@if [ -n "$(EFFECTIVE_CARGO_TARGET)" ] && [ "$(EFFECTIVE_CARGO_TARGET)" != "$(HOST_TRIPLE)" ]; then \
		echo "Error: Cargo target $(EFFECTIVE_CARGO_TARGET) is not host $(HOST_TRIPLE); use a dedicated cross-library target" >&2; \
		exit 2; \
	fi
ifeq ($(shell uname -s),Darwin)
DARWIN_NATIVE_LIB_TRIPLES := aarch64-apple-darwin x86_64-apple-darwin
else
DARWIN_NATIVE_LIB_TRIPLES :=
endif
ifeq ($(shell uname -s),Linux)
ifneq ($(filter aarch64-%-linux-musl,$(HOST_TRIPLE)),)
LINUX_CROSS_LIB_TRIPLE := x86_64-unknown-linux-musl
LINUX_CROSS_SYSROOT := /usr/x86_64-linux-musl
else ifneq ($(filter aarch64-%-linux-gnu,$(HOST_TRIPLE)),)
LINUX_CROSS_LIB_TRIPLE := x86_64-unknown-linux-gnu
LINUX_CROSS_SYSROOT := /usr/x86_64-linux-gnu
else ifneq ($(filter x86_64-%-linux-musl,$(HOST_TRIPLE)),)
LINUX_CROSS_LIB_TRIPLE := aarch64-unknown-linux-musl
LINUX_CROSS_SYSROOT := /usr/aarch64-linux-musl
else ifneq ($(filter x86_64-%-linux-gnu,$(HOST_TRIPLE)),)
LINUX_CROSS_LIB_TRIPLE := aarch64-unknown-linux-gnu
LINUX_CROSS_SYSROOT := /usr/aarch64-linux-gnu
endif
endif
LINUX_CROSS_AVAILABLE := $(if $(wildcard $(LINUX_CROSS_SYSROOT)),$(LINUX_CROSS_LIB_TRIPLE))
CROSS_NATIVE_LIB_TRIPLES := $(filter-out $(HOST_TRIPLE),$(DARWIN_NATIVE_LIB_TRIPLES) $(LINUX_CROSS_AVAILABLE))
NATIVE_LIB_TRIPLES := $(HOST_TRIPLE) $(CROSS_NATIVE_LIB_TRIPLES)

# Sanitizer targets for the Rust runtime. The dedicated codegen sanitizer
# lane was retired together with the C++/MLIR subtree; the runtime ASan
# and TSan lanes here remain as local entry points for nightly coverage.
#
# Default to the host triple so `make asan` works on any sanitizer-capable
# host (darwin-arm64, linux-x86_64, ...). CI selects its explicit runner target
# through `SANITIZER_RUST_TARGET` while retaining this single command authority.
SANITIZER_RUST_TARGET ?= $(HOST_TRIPLE)
RUNTIME_ASAN_TARGET_DIR := target/sanitizer-runtime-asan
RUNTIME_TSAN_TARGET_DIR := target/sanitizer-runtime-tsan
RUNTIME_MIRI_TARGET_DIR := target/miri-runtime

# ── Default target ──────────────────────────────────────────────────────────

all: assemble ## Build: build the release-lib compiler and debug support artifacts
	$(ASSERT_RELEASE_LIB_HEW_PROFILE)

# Convenience alias — builds the release-lib compiler and debug support tools.
# Equivalent to `make all`; exists so that `make build` behaves as expected.
build: all

# ── Rust targets ────────────────────────────────────────────────────────────

# The supported developer launcher uses the non-LTO release-lib profile for
# both the driver and its linkable archive. Keep a separate debug launcher for
# compiler debugging without changing the stable build/bin/hew selection.
hew: ## Build: build the release-lib compiler and native archive
	cargo build -p hew-lib -p hew-cli --profile release-lib $(CARGO_TARGET_FLAG)
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib
	@ln -sfn "$(LINK_UP2)$(RELEASE_LIB_HEW)" "$(BUILD_DIR)/bin/hew"
	@ln -sfn "$(LINK_UP2)$(RELEASE_LIBHEW)" "$(BUILD_DIR)/lib/$(notdir $(RELEASE_LIBHEW))"

hew-debug: hew-native
	@mkdir -p $(BUILD_DIR)/bin
	@ln -sfn "$(LINK_UP2)$(DEBUG_HEW)" "$(BUILD_DIR)/bin/hew-debug"
	@echo "compiler profile: debug"
	@echo "compiler path: $(DEBUG_HEW)"
	@test -f "$(DEBUG_HEW)"

# Shared assertion recipe. `hew-profile-check` builds the supported launcher
# before checking it; `all` runs the same assertion only after `assemble`, so a
# later assembly step cannot silently replace the launcher with another profile.
define ASSERT_RELEASE_LIB_HEW_PROFILE
	@actual="$$(readlink "$(BUILD_DIR)/bin/hew")"; \
	expected="$(LINK_UP2)$(RELEASE_LIB_HEW)"; \
	echo "compiler profile: release-lib"; \
	echo "compiler path: $(RELEASE_LIB_HEW)"; \
	test -f "$(RELEASE_LIB_HEW)"; \
	test "$$actual" = "$$expected" || { \
		echo "Error: build/bin/hew resolves through $$actual, expected $$expected" >&2; \
		exit 1; \
	}
endef

hew-profile-check: hew
	$(ASSERT_RELEASE_LIB_HEW_PROFILE)

# Build the native artifacts required for `hew build` from a source checkout:
# the driver plus hew-lib's staticlib (`target/debug/libhew.a` on Unix,
# `target/debug/hew.lib` on Windows). Keep this target cross-platform so fresh
# Windows hosts use the same build graph as Linux/macOS.
shared-host-debug:
	cargo build -p hew-runtime -p hew-lib -p hew-cli $(CARGO_TARGET_FLAG)

# Human-facing compiler alias. The shared host transaction is also the
# inventory builder for the CLI, libhew and runtime archives, preventing Cargo
# feature-set churn between separate package invocations.
hew-native: shared-host-debug
	@:

# Build the language server (debug).
hew-lsp:
	cargo build -p hew-lsp $(CARGO_TARGET_FLAG)

# Build the TUI actor observer (debug).
# hew-observe is a sibling binary: `hew observe` delegates to it when it is
# present next to the running hew binary or on PATH (see exec_sibling_binary).
observe:
	cargo build -p hew-observe $(CARGO_TARGET_FLAG)

observe-functional-test: hew-native observe
	$(TEST_RUN_ENV) cargo test -p hew-observe --test functional -- --ignored --nocapture

# Opt-in real-client proof for the advertised pure-Hew MQTT broker. Mosquitto
# clients are an explicit external prerequisite, so this is not folded into the
# hermetic workspace test lane.
mqtt-broker-e2e: hew-native
	HEW_BIN="$(DEBUG_DIR)/hew" scripts/mqtt-broker-e2e.sh

# Real multi-process proof that test-run calls to
# `hew_testutil::ensure_hew_lib_built` are verify-only: concurrent real
# `hew compile` links share one NEXTEST_RUN_ID and a fail-closed Cargo spy
# proves that none of them attempts to rebuild `libhew.a`.
# Excluded from routine `cargo nextest run` (see the #[ignore] reasons in
# hew-testutil/tests/libhew_link_race.rs) because it repeatedly shells real
# cargo/hew subprocesses; run explicitly here instead, same convention as
# observe-functional-test above.
libhew-link-race-test: hew-native
	$(TEST_RUN_ENV) cargo test -p hew-testutil --test libhew_link_race -- --ignored --nocapture --test-threads=1

# Build the runtime static library (debug)
runtime:
	cargo build -p hew-runtime $(CARGO_TARGET_FLAG)

# Build libhew.a — the combined runtime + stdlib static library.
# The hew-lib umbrella crate depends on hew-runtime + all stdlib crates;
# Cargo produces a single deduplicated staticlib.
#
# Human-facing native standard-library archive.
stdlib: libhew-debug ## Build: build all standard-library packages

# Internal integration-test bootstrap. Broad artifacts are explicit here and
# are not imposed on every host-only gate.
.PHONY: test-artifacts
test-artifacts: shared-host-debug libhew-cross-release-lib wasm-runtime

# Cargo owns freshness for its configurable output tree. This target remains
# phony deliberately: a fixed Make stamp cannot distinguish two different
# CARGO_TARGET_DIR/build.target/build.target-dir selections without putting the
# possibly space-bearing output path back into Make's target graph.
.PHONY: libhew-debug libhew-cross-release-lib
libhew-debug: shared-host-debug
	@:

# The compiler is useful for cross compilation only when its consumer-linkable
# native archive is available for the requested architecture. Build the
# non-LTO release-lib archive once and use it for both assembled/installed
# toolchains and cross-target tests. This avoids maintaining a test-only debug
# copy of the same large staticlib in every worktree.
libhew-cross-release-lib:
ifeq ($(shell uname -s),Darwin)
	@for triple in $(CROSS_NATIVE_LIB_TRIPLES); do \
		cargo build -p hew-lib --profile release-lib --target "$$triple" || exit $$?; \
	done
else ifeq ($(shell uname -s),Linux)
	@if [ -z "$(LINUX_CROSS_AVAILABLE)" ]; then \
		echo "Skipping $(LINUX_CROSS_LIB_TRIPLE) libhew.a: install its cross sysroot at $(LINUX_CROSS_SYSROOT) to include it"; \
	else \
		cargo build -p hew-lib --profile release-lib --target "$(LINUX_CROSS_AVAILABLE)"; \
	fi
else
	@:
endif

# Build the WASM runtime + the consolidated stdlib archive (libhew_std.a).
#
# Keep the Cargo output filenames out of Make's target graph for the same
# spacious-target-directory reason as libhew-debug. Cargo's own incremental
# graph makes repeated invocations cheap and authoritative.
.PHONY: wasm-runtime-debug wasm-runtime-release stage-portable-release-libs
wasm-runtime-debug:
	$(WASM_UNINSTRUMENTED_ENV) cargo build -p hew-runtime -p hew-std --target wasm32-wasip1 --no-default-features

wasm-runtime-release:
	$(WASM_UNINSTRUMENTED_ENV) cargo build -p hew-runtime -p hew-std --target wasm32-wasip1 --no-default-features --release

# Stage the platform-independent release libraries for packaging/CI transfer.
# Make owns product membership; callers choose only the destination.
PORTABLE_RELEASE_LIB_DIR := $(BUILD_DIR)/portable-release-libs
stage-portable-release-libs: wasm-runtime-release
	@rm -rf "$(PORTABLE_RELEASE_LIB_DIR)"
	@mkdir -p "$(PORTABLE_RELEASE_LIB_DIR)/wasm32-wasip1"
	@cp "$(WASM_RELEASE_DIR)/libhew_runtime.a" "$(PORTABLE_RELEASE_LIB_DIR)/wasm32-wasip1/"
	@cp "$(WASM_RELEASE_DIR)/libhew_std.a" "$(PORTABLE_RELEASE_LIB_DIR)/wasm32-wasip1/"

# Assemble the platform-neutral release tree before platform-specific
# stripping, signing, archiving, and post-extraction smoke tests. The inputs are
# intentionally caller-selectable: release CI can package target-native and
# cross-built libraries without maintaining another copy of product membership.
RELEASE_PACKAGE_SOURCE_DIR ?= $(CURDIR)
RELEASE_PACKAGE_BIN_DIR ?= $(RELEASE_DIR)
RELEASE_PACKAGE_BIN_SUFFIX ?= $(if $(filter Windows_NT,$(OS)),.exe,)
RELEASE_PACKAGE_NATIVE_LIB ?= $(RELEASE_LIBHEW)
RELEASE_PACKAGE_NATIVE_TRIPLE ?= $(HOST_TRIPLE)
RELEASE_PACKAGE_NATIVE_LIB_NAME ?= $(LIBHEW_NAME)
RELEASE_PACKAGE_WASI_LIB_DIR ?= $(WASM_RELEASE_DIR)
RELEASE_PACKAGE_DEST ?= $(BUILD_DIR)/release-package
RELEASE_PACKAGE_COMPLETIONS ?= $(if $(filter Windows_NT,$(OS)),bash zsh fish powershell,bash zsh fish)

# `make dev-dist` produces a self-contained, host-specific development
# toolchain. The archive name is derived from the staged binary so it always
# matches `hew --version`, including the git development identity.
DEV_DIST_DIR ?= dist
DEV_DIST_STAGE_DIR ?= $(BUILD_DIR)/dev-dist-stage
DEV_DIST_PREFIX ?= hew

stage-release-package: ## Release: stage a validated distributable toolchain tree
	@sh "$(MAKEFILE_ROOT)/scripts/stage-release-package.sh" \
		--source-dir "$(RELEASE_PACKAGE_SOURCE_DIR)" \
		--bin-dir "$(RELEASE_PACKAGE_BIN_DIR)" \
		--bin-suffix "$(RELEASE_PACKAGE_BIN_SUFFIX)" \
		--native-lib "$(RELEASE_PACKAGE_NATIVE_LIB)" \
		--native-triple "$(RELEASE_PACKAGE_NATIVE_TRIPLE)" \
		--native-lib-name "$(RELEASE_PACKAGE_NATIVE_LIB_NAME)" \
		--wasi-lib-dir "$(RELEASE_PACKAGE_WASI_LIB_DIR)" \
		--destination "$(RELEASE_PACKAGE_DEST)" \
		--completion-shells "$(RELEASE_PACKAGE_COMPLETIONS)"

dev-dist: assemble-release ## Release: package the current development build as a portable .tgz
	@set -eu; \
	version="$$("$(RELEASE_HEW)" --version | sed -n 's/^hew //p')"; \
	[ -n "$$version" ] || { echo "Error: could not determine the Hew version" >&2; exit 1; }; \
	case "$$version" in *[!A-Za-z0-9.+-]*) echo "Error: unsafe Hew version: $$version" >&2; exit 1;; esac; \
	package="$(DEV_DIST_PREFIX)-v$$version-$(HOST_TRIPLE)"; \
	stage_root="$(DEV_DIST_STAGE_DIR)"; \
	stage="$$stage_root/$$package"; \
	archive="$(DEV_DIST_DIR)/$$package.tgz"; \
	rm -rf "$$stage_root"; \
	mkdir -p "$$stage_root" "$(DEV_DIST_DIR)"; \
	sh "$(MAKEFILE_ROOT)/scripts/stage-release-package.sh" \
		--source-dir "$(RELEASE_PACKAGE_SOURCE_DIR)" \
		--bin-dir "$(RELEASE_PACKAGE_BIN_DIR)" \
		--bin-suffix "$(RELEASE_PACKAGE_BIN_SUFFIX)" \
		--native-lib "$(RELEASE_PACKAGE_NATIVE_LIB)" \
		--native-triple "$(RELEASE_PACKAGE_NATIVE_TRIPLE)" \
		--native-lib-name "$(RELEASE_PACKAGE_NATIVE_LIB_NAME)" \
		--wasi-lib-dir "$(RELEASE_PACKAGE_WASI_LIB_DIR)" \
		--destination "$$stage" \
		--completion-shells "$(RELEASE_PACKAGE_COMPLETIONS)"; \
	tar -C "$$stage_root" -czf "$$archive" "$$package"; \
	printf 'Created %s\nInstall with: sudo mkdir -p /opt/hew && sudo tar -xzf %s -C /opt/hew --strip-components=1\n' "$$archive" "$$archive"

wasm-runtime: wasm-runtime-debug

# Build the hew-wasm browser analysis-only module (requires: cargo install wasm-pack)
wasm: ## Build: build the browser WebAssembly package
	wasm-pack build hew-wasm --target web --release

# Regenerate the typed WASM capability consumers.
wasm-capability:
	cargo run -p hew-capability-gen

# Verify the generated checker, playground, and matrix consumers are current.
wasm-capability-check:
	cargo run -p hew-capability-gen -- --check

# Regenerate the curated playground manifest consumed by downstream browser tooling.
playground-manifest: wasm-capability
	$(PYTHON) scripts/gen-playground-manifest.py

# Verify the checked-in playground manifest is current.
playground-manifest-check: wasm-capability-check
	$(PYTHON) scripts/gen-playground-manifest.py --check

sandbox-fixtures:
	cargo run -p xtask -- sandbox-fixtures

sandbox-fixtures-check:
	cargo run -p xtask -- sandbox-fixtures --check

# Regenerate THIRD-PARTY-LICENSES from the current dependency tree.
# Requires cargo-about: cargo install cargo-about --locked
licenses:
	cargo about generate about.hbs --workspace > THIRD-PARTY-LICENSES

# Verify THIRD-PARTY-LICENSES is current relative to Cargo.lock and about.hbs.
# Exits non-zero if the file is stale; run `make baselines` to regenerate.
licenses-check:
	scripts/check-licenses-fresh.sh

# ── Derived baselines ─────────────────────────────────────────────────────
#
# Deterministic generated repository artifacts. Behavioral ratchets remain
# owned by their named test targets; they are intentionally not re-recorded by
# a generic command from observed compiler output.
baselines: wasm-capability playground-manifest sandbox-fixtures licenses cabi-surface

baselines-check: wasm-capability-check playground-manifest-check sandbox-fixtures-check licenses-check cabi-surface-check ## Check: verify deterministic generated artifacts are current


# Install hew-sandbox-vm's npm dependencies, skipping the install when
# node_modules already matches package-lock.json (hash-stamped). Split out
# from sandbox-parity as its own reusable prerequisite.
sandbox-vm-deps:
	@set -e; \
	lock_hash=$$($(PYTHON) -c 'import hashlib, pathlib; print(hashlib.sha256(pathlib.Path("hew-sandbox-vm/package-lock.json").read_bytes()).hexdigest())'); \
	stamp=hew-sandbox-vm/node_modules/.package-lock.sha256; \
	if [ ! -d hew-sandbox-vm/node_modules ] || [ ! -f "$$stamp" ] || [ "$$lock_hash" != "$$(cat "$$stamp")" ]; then \
		echo "npm --prefix hew-sandbox-vm ci"; \
		npm --prefix hew-sandbox-vm ci; \
		printf '%s\n' "$$lock_hash" > "$$stamp"; \
	else \
		echo "hew-sandbox-vm dependencies are fresh; skipping install"; \
	fi

# Native Hew <-> sandbox VM parity harness. The complete sandbox-wasm package
# is excluded from generic nextest runs and owned here with Node provisioned.
sandbox-parity: wasm-runtime hew-native sandbox-vm-deps
	npm --prefix hew-sandbox-vm test
	$(TEST_RUN_ENV) cargo test -p hew-sandbox-wasm

# Repo-local browser/tooling smoke:
# manifest freshness + full hew-wasm test suite (lib + integration) + analysis-only WASM build.
# Running full `cargo test -p hew-wasm` subsumes the --lib curated-manifest smoke and compiles
# and runs tests/v05_wasm_coverage.rs (the fixture-coverage integration suite).
playground-check: playground-manifest-check ## Build: test and build the playground package
	$(TEST_RUN_ENV) cargo test -p hew-wasm
	$(MAKE) wasm

# Focused curated playground WASI runtime preflight.
playground-wasi-check: wasm-runtime hew-native
	$(TEST_RUN_ENV) cargo test -p hew-cli --test wasi_run_e2e curated_playground_examples_run_under_wasi -- --exact
	$(TEST_RUN_ENV) cargo test -p hew-cli --test wasi_run_e2e supervisor_stays_on_the_unsupported_diagnostic_path_under_wasi -- --exact

# Native run of every runnable playground example against its checked-in
# `.expected` file (`hew tool playground-verify`), catching drift the
# analysis-only WASM/manifest checks above don't exercise.
playground-verify: hew-native
	$(DEBUG_HEW) tool playground-verify

# Standard per-branch gate: validate workflow syntax locally, then run the lint
# graph and the same three Make-owned test groups used by hosted Linux CI. One
# Make graph lets shared prerequisites build once instead of being replanned by
# four recursive invocations. Hosted CI invokes lint and the shards directly:
# actionlint cannot rescue a workflow that is too malformed to start.
.NOTPARALLEL: preflight
preflight: actionlint lint ci-shard-1 ci-shard-2 ci-shard-3 ## Develop: run unconditional local branch gates
	@:

# Compatibility alias for automation that used the older name.
ci-preflight: preflight
	@:

.PHONY: ci-shard-1 ci-shard-2 ci-shard-3
ci-shard-1: observe-functional-test test-cabi test-compiler-lifecycle \
	test-vertical-slice test-pkg-import test-runtime-unit test-ux-examples \
	test-doc-examples doc-ratchet-selftest test-migrate-corpus \
	o2-differential-selftest playground-verify

ci-shard-2: hew-profile-check libhew-link-race-test test \
	test-leak-oracle-selftest test-opaque-resource-lifecycle-matrix-external \
	test-ownership-balance-corpus compile-determinism-verify compile-determinism-selftest \
	test-ownership-balance-runner-selftest stdlib-user-build-clean \
	test-asan-fixture-selftest hew-fmt-property stdlib-lint \
	sir-coverage sir-parity

ci-shard-3: mqtt-broker-e2e sandbox-parity \
	fuzz-oracle fuzz-oracle-selftest test-package-install \
	checked-mir-verify checked-mir-run \
	test-core-matrix test-stdlib-ratchet \
	test-surface-examples forced-cancel-composite-check hew-check-all

# Fast smoke preflight: Rust fmt + the workspace's deterministic in-process
# tests (nextest smoke profile). Designed to complete in <5 min and surface
# format and fast oracle failures during local iteration. Clippy remains in
# the lint target and is not duplicated here.
#
# Run this target directly for a quick sanity pass on any diff without waiting
# for E2E compilation. The unconditional assignment reserves it for local
# opt-in because its full workspace run already includes the smoke test.
#
# The smoke nextest profile excludes subprocess-intensive tests (eval_e2e,
# test_runner_e2e, parity) and hew-wasm; see .config/nextest.toml [profile.smoke].
#
ci-preflight-smoke:
	cargo fmt --all -- --check
	$(TEST_RUN_ENV) cargo nextest run --workspace --profile smoke

# ── Local Linux CI-parity harness ────────────────────────────────────────────
# Runs the GitHub Actions `Build & test (Linux)` job on a NATIVE x86_64 Linux
# host over ssh — the faithful, fast local parity. Docker on Apple Silicon is a
# dead end here (qemu segfaults rustc; arm64 containers diverge from CI on the
# ppv-lite86 SIMD path and the ARM64 LLVM tarball). See scripts/ci-local-linux.sh
# and LESSONS.md `ci-local-parity-needs-native-x86_64`.
#
#   make ci-local-linux CI_LINUX_HOST=user@host                   # full Linux job
#   make ci-local-linux CI_LINUX_HOST=user@host STEP=vertical-slice
#   STEP ∈ { wasm workspace vertical-slice pkg-import hew-ratchet stdlib-ratchet sandbox all }
#
# The host must provide CI's toolchain (LLVM via LLVM_SYS_221_PREFIX, the pinned
# Rust toolchain, cargo-nextest, wasmtime). Override the remote LLVM prefix with
# HEW_CI_LLVM_PREFIX for byte-faithful parity against CI's upstream LLVM tarball.
STEP ?= all

ci-local-linux:
	@test -n "$(CI_LINUX_HOST)" || { echo "error: set CI_LINUX_HOST=<user@host> (a native x86_64 Linux box)"; exit 2; }
	STEP="$(STEP)" CI_LINUX_HOST="$(CI_LINUX_HOST)" scripts/ci-local-linux.sh

fuzz-corpus:
	scripts/fuzz/hydrate-corpus.sh

# Fuzz-to-run completeness oracle.
#
# Default (CI) mode: regressions only — vertical-slice/accept + tests/fuzz-oracle/regressions.
# Deterministic, bounded, suitable for the merge queue.
#
# Full mode (manual): also scans the raw cargo-fuzz corpus (nondeterministic; not in CI).
#   make fuzz-oracle FUZZ_ORACLE_FULL=1
#
# Prereqs mirror test-vertical-slice: libhew.a must be fresh so native links
# do not test against stale runtime/stdlib archives.
FUZZ_ORACLE_FULL ?=
fuzz-oracle: hew-native
	@if [ -n "$(FUZZ_ORACLE_FULL)" ]; then \
		$(PYTHON) scripts/fuzz/run-oracle.py --hew "$(DEBUG_DIR)/hew" --full --timeout 30 $(RATCHET_STRICT_RECOVERIES_ARG); \
	else \
		$(PYTHON) scripts/fuzz/run-oracle.py --hew "$(DEBUG_DIR)/hew" --timeout 30 $(RATCHET_STRICT_RECOVERIES_ARG); \
	fi

# Oracle self-tests: five independently-failable checks that prove the
# harness has teeth (flags real crashes), honours the ratchet contract
# (unexpected-pass and unexpected-fail both fail closed), and refuses to
# report PASS over a candidate set below its floor.
fuzz-oracle-selftest: hew-native
	HEW_BIN="$(DEBUG_DIR)/hew" bash scripts/fuzz/oracle-selftest.sh

# Bounded libFuzzer smoke: nightly-only (see .github/workflows/nightly-sanitizers.yml).
# A per-PR fuzz run is nondeterministic (a corpus mutation can trip one run
# and not the next), which the deterministic per-PR fuzz-oracle above does
# not tolerate — so this stays off ci.yml. Self-provisioning mirrors
# structural-lint: the toolchain install is a prerequisite of the gate
# target, not a separate manual step, and it is idempotent.
FUZZ_SMOKE_MAX_TOTAL_TIME ?= 120

fuzz-smoke-bootstrap-install:
	bash scripts/fuzz/smoke-bootstrap.sh

fuzz-smoke: fuzz-smoke-bootstrap-install
	FUZZ_SMOKE_MAX_TOTAL_TIME="$(FUZZ_SMOKE_MAX_TOTAL_TIME)" bash scripts/fuzz/run-smoke.sh

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

# Downstream repo roots (sibling directories of hew/).
# Derive from the common git directory (already computed above) rather than
# $(CURDIR), which points to the worktree's own filesystem location and yields
# the wrong parent when `make -C <worktree>` is invoked from an out-of-tree path.
HEW_SH  ?= $(shell dirname "$(COMMON_GIT_DIR)")/../hew.sh
HEW_RUN ?= $(shell dirname "$(COMMON_GIT_DIR)")/../hew.run

# Build hew-wasm and distribute to downstream repos
wasm-dist: wasm
	@echo "==> Distributing hew-wasm to hew.sh"
	cp $(CURDIR)/hew-wasm/pkg/hew_wasm.js      $(HEW_SH)/src/lib/wasm/hew_wasm.js
	cp $(CURDIR)/hew-wasm/pkg/hew_wasm_bg.wasm $(HEW_SH)/public/wasm/hew_wasm_bg.wasm
	@echo "==> Distributing hew-wasm to hew.run"
	cp $(CURDIR)/hew-wasm/pkg/hew_wasm.js      $(HEW_RUN)/src/lib/wasm/hew_wasm.js
	cp $(CURDIR)/hew-wasm/pkg/hew_wasm_bg.wasm $(HEW_RUN)/static/wasm/hew_wasm_bg.wasm
	@echo "==> Done. Commit in hew.sh and hew.run."

# Build the native debug artifacts assembled by `make all` in one Cargo graph.
# Individual targets remain separate for focused development builds, but running
# their Cargo commands concurrently through `assemble` just serializes on the
# shared target directory lock and re-plans overlapping dependency closures.
.PHONY: assemble-host-debug
assemble-host-debug:
	cargo build -p hew-lib -p hew-cli -p hew-lsp -p hew-observe $(CARGO_TARGET_FLAG)

# Create symlinks from build/ into the real output locations.
# This gives you one stable directory to point PATH at during development.
assemble: require-host-cargo-target | hew assemble-host-debug libhew-cross-release-lib wasm-runtime
	@rm -rf $(BUILD_DIR)/bin $(BUILD_DIR)/lib $(BUILD_DIR)/std
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib
	@# Compiler drivers: keep the supported release-lib launcher stable while
	@# exposing the debug compiler under its explicit debug name.
	@ln -sfn "$(LINK_UP2)$(RELEASE_LIB_HEW)"              "$(BUILD_DIR)/bin/hew"
	@ln -sfn "$(LINK_UP2)$(DEBUG_HEW)"                    "$(BUILD_DIR)/bin/hew-debug"
	@# Language server
	@ln -sfn "$(LINK_UP2)$(DEBUG_DIR)/hew-lsp"            "$(BUILD_DIR)/bin/hew-lsp"
	@# TUI actor observer (sibling binary — `hew observe` delegates here)
	@ln -sfn "$(LINK_UP2)$(DEBUG_DIR)/hew-observe"        "$(BUILD_DIR)/bin/hew-observe"
	@# Combined Hew library (runtime + all stdlib packages)
	@ln -sfn "$(LINK_UP2)$(RELEASE_LIB_DIR)/libhew.a"     "$(BUILD_DIR)/lib/libhew.a"
	@# WASM runtime + focused wire stdlib archives (symlink if built)
	@for lib in libhew_runtime.a libhew_std.a; do \
		if [ -f "$(WASM_DEBUG_DIR)/$$lib" ]; then \
			mkdir -p $(BUILD_DIR)/lib/wasm32-wasip1; \
			ln -sfn "$(LINK_UP3)$(WASM_DEBUG_DIR)/$$lib" \
				"$(BUILD_DIR)/lib/wasm32-wasip1/$$lib"; \
		fi; \
	done
	@# Native per-triple lib symlinks — mirrors the wasm32-wasip1 pattern,
	@# keeps the host lib under lib/<triple>/ on Linux and Darwin, and lets
	@# Darwin same-OS cross-arch linking pick up prebuilt libhew.a slices.
	@for triple in $(NATIVE_LIB_TRIPLES); do \
		[ -n "$$triple" ] || continue; \
		lib_path=""; \
		if [ -f "$(CARGO_TARGET_ROOT)/$$triple/release-lib/libhew.a" ]; then \
			lib_path="$(CARGO_TARGET_ROOT)/$$triple/release-lib/libhew.a"; \
		elif [ "$$triple" = "$(HOST_TRIPLE)" ] && [ -f "$(RELEASE_LIB_DIR)/libhew.a" ]; then \
			lib_path="$(RELEASE_LIB_DIR)/libhew.a"; \
		else \
			continue; \
		fi; \
		mkdir -p $(BUILD_DIR)/lib/$$triple; \
		ln -sfn "$(LINK_UP3)$$lib_path" "$(BUILD_DIR)/lib/$$triple/libhew.a"; \
	done
	@# Keep the complete nested standard library available from the staged tree.
	@ln -sfn ../std $(BUILD_DIR)/std
	@echo "build/ assembled (release-lib compiler, debug support tools). Add to PATH:"
	@echo "  export PATH=\"$(CURDIR)/$(BUILD_DIR)/bin:\$$PATH\""

# ── Release build ───────────────────────────────────────────────────────────

# Host release product. Other release gates and scripts consume this target
# instead of repeating its Cargo package/profile selection.
.PHONY: release-host
release-host: require-host-cargo-target
	cargo build -p hew-cli -p hew-lsp -p hew-observe --release $(CARGO_TARGET_FLAG)
	cargo build -p hew-lib --profile release-lib $(CARGO_TARGET_FLAG)

# Build everything in release mode and repoint the build/ symlinks. Cargo's
# workspace config owns platform defaults while allowing caller overrides.
release: assemble-release test-release-lib-link ## Release: build optimized release artifacts
	@:

# Validate release builds on all supported platforms before tagging.
# Runs linux locally first (fail-fast), then remote platforms in parallel.
#   make pre-release                    — all platforms
#   make pre-release PLATFORMS="linux"  — linux only
pre-release: ## Release: build and validate a release candidate on requested platforms
	scripts/pre-release-validate.sh $(PLATFORMS)

# Build the staged source tree the Windows validator builds from
# (scripts/windows-release-build.ps1 runs `cargo build` itself; it needs
# Cargo.toml/Cargo.lock and every crate, not compiled artifacts). Archives
# the committed HEAD tree, so uncommitted changes are not included.
windows-release-candidate:
	@mkdir -p target
	git archive --format=tar.gz -o target/hew-windows-candidate.tar.gz HEAD
	@echo "Wrote target/hew-windows-candidate.tar.gz from $$(git rev-parse HEAD)"

# Build stdlib docs and print the wrangler deploy command.
# Requires a release binary; run `make release` first if the release hew
# is absent or stale.  The operator supplies the Cloudflare token via
# `wrangler login` or CLOUDFLARE_API_TOKEN in the shell — it is never in
# this file.
publish-docs: ## Release: build docs and print the operator deploy command
	@test -f "$(RELEASE_DIR)/hew" \
		|| { echo "Error: release hew not built. Run 'make release' first."; exit 1; }
	"$(RELEASE_DIR)/hew" doc std/ --output-dir "$(CARGO_TARGET_ROOT)/doc/"
	@echo ""
	@echo "Docs generated at $(CARGO_TARGET_ROOT)/doc/."
	@echo "Deploy with: wrangler pages deploy $(CARGO_TARGET_ROOT)/doc/ --project-name hew-docs"

# Prove the shipped archive can link a real Rust staticlib through the public
# `hew build --link-lib` interface. Rust controls archive member names, so the
# behavioural consumer proof is more stable than inspecting `ar t` output.
test-release-lib-link: release-host
ifeq ($(OS),Windows_NT)
	@powershell -NoProfile -ExecutionPolicy Bypass -File "$(CURDIR)/scripts/test-release-lib-link.ps1" -Hew "$(RELEASE_HEW)" -Archive "$(RELEASE_LIBHEW)"
else
	@"$(CURDIR)/scripts/test-release-lib-link.sh" --hew "$(RELEASE_HEW)" --archive "$(RELEASE_LIBHEW)"
endif

# Assemble build/ with release symlinks.
assemble-release: require-host-cargo-target release-host libhew-cross-release-lib wasm-runtime-release
	@rm -rf $(BUILD_DIR)/bin $(BUILD_DIR)/lib $(BUILD_DIR)/std
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib
	@ln -sfn "$(LINK_UP2)$(RELEASE_DIR)/hew"              "$(BUILD_DIR)/bin/hew"
	@ln -sfn "$(LINK_UP2)$(RELEASE_DIR)/hew-lsp"          "$(BUILD_DIR)/bin/hew-lsp"
	@ln -sfn "$(LINK_UP2)$(RELEASE_DIR)/hew-observe"      "$(BUILD_DIR)/bin/hew-observe"
	@# Combined Hew library (runtime + all stdlib packages), from the non-LTO
	@# release-lib profile — never the fat-LTO target/release archive.
	@ln -sfn "$(LINK_UP2)$(RELEASE_LIB_DIR)/libhew.a"     "$(BUILD_DIR)/lib/libhew.a"
	@for lib in libhew_runtime.a libhew_std.a; do \
		if [ -f "$(WASM_RELEASE_DIR)/$$lib" ]; then \
			mkdir -p $(BUILD_DIR)/lib/wasm32-wasip1; \
			ln -sfn "$(LINK_UP3)$(WASM_RELEASE_DIR)/$$lib" \
				"$(BUILD_DIR)/lib/wasm32-wasip1/$$lib"; \
		fi; \
	done
	@# Native per-triple lib symlinks — mirrors the wasm32-wasip1 pattern.
	@for triple in $(NATIVE_LIB_TRIPLES); do \
		[ -n "$$triple" ] || continue; \
		lib_path=""; \
		if [ -f "$(CARGO_TARGET_ROOT)/$$triple/release-lib/libhew.a" ]; then \
			lib_path="$(CARGO_TARGET_ROOT)/$$triple/release-lib/libhew.a"; \
		elif [ "$$triple" = "$(HOST_TRIPLE)" ] && [ -f "$(RELEASE_LIB_DIR)/libhew.a" ]; then \
			lib_path="$(RELEASE_LIB_DIR)/libhew.a"; \
		else \
			continue; \
		fi; \
		mkdir -p $(BUILD_DIR)/lib/$$triple; \
		ln -sfn "$(LINK_UP3)$$lib_path" "$(BUILD_DIR)/lib/$$triple/libhew.a"; \
	done
	@rm -rf $(BUILD_DIR)/std
	@ln -sfn ../std $(BUILD_DIR)/std
	@echo "build/ assembled (release)."

# ── Tests ───────────────────────────────────────────────────────────────────

# Build the combined runtime+stdlib static lib, the native runtime staticlib,
# and the WASM runtime before running the full workspace test suite.  Several
# hew-cli integration tests (eval_e2e, eval_wasm_*) call `hew eval` which needs
# both libs at link time.
# The WASM runtime (libhew_runtime.a for wasm32-wasip1) is required by the
# wasm32-wasi eval tests even when they are expected to fail before codegen:
# the linker library search runs before the fast-typecheck diagnostic path,
# so a missing staticlib causes an unrelated error that aborts those tests.
# `runtime` builds the *native* libhew_runtime.a that the hew-codegen-rs coro
# substrate execution tests link directly.  `cargo test`/`nextest` build only
# hew-runtime's rlib, never its staticlib, so without this prereq a stale
# cached archive (e.g. one predating the hew_cont_* continuation substrate)
# would be linked against freshly-emitted coro objects and fail with
# undefined-symbol errors on a target dir carried across commits.
test: test-artifacts ## Test: run the ratcheted Rust workspace test suite
	@rm -f "$(NEXTEST_JUNIT)" "$(NEXTEST_RATCHET_JUNIT)" "$(NEXTEST_FULL_INVENTORY)" "$(NEXTEST_SELECTED_INVENTORY)"
	@$(NEXTEST_PREPARE_FULL_INVENTORY)
	@$(NEXTEST_PREPARE_SELECTED_INVENTORY)
	@status=0; \
		$(TEST_RUN_ENV) cargo nextest run $(NEXTEST_WORKSPACE_ARGS) || status=$$?; \
		cargo xtask nextest-ratchet \
			--junit "$(NEXTEST_JUNIT)" \
			--ledger "$(NEXTEST_FAILURE_LEDGER)" \
			--output "$(NEXTEST_RATCHET_JUNIT)" \
			--platform "$(NEXTEST_PLATFORM)" \
			--runner-exit "$$status" $(NEXTEST_RATCHET_INVENTORY_ARGS) $(RATCHET_STRICT_RECOVERIES_ARG)

test-strict: test-artifacts ## Test: run the Rust workspace test suite with no known failures
	@rm -f "$(NEXTEST_JUNIT)" "$(NEXTEST_RATCHET_JUNIT)" "$(NEXTEST_FULL_INVENTORY)" "$(NEXTEST_SELECTED_INVENTORY)"
	$(TEST_RUN_ENV) cargo nextest run $(NEXTEST_WORKSPACE_ARGS)

# Scheduled ledger authority. Each family runs independently so a red first
# family cannot suppress reports from the later ledgers.
ratchet-accounting: ## Check: strict expected-failure ledger accounting
	RATCHET_STRICT_RECOVERIES=1 RATCHET_ACCOUNTING_MAKE="$(MAKE)" scripts/ratchet-accounting.sh

# Platform-scoped nextest ledger entries receive their own scheduled jobs.
# This target owns strict mode rather than relying on workflow environment.
ratchet-accounting-nextest: ## Check: strict nextest expected-failure accounting
	RATCHET_STRICT_RECOVERIES=1 $(MAKE) test

test-ratchet-accounting-runner: ## Test: accounting runner executes all families after failures
	TMPDIR="$${TMPDIR:-/tmp}" scripts/tests/test_ratchet_accounting_runner.sh

# Canonical local macOS memory authority. This is deliberately named as a local
# authority, not a CI `test-*` gate: hosted macOS processes cannot grant
# leaks(1) the task port it needs. The runner rejects a non-Darwin host,
# a missing leaks(1), an empty/shrunken inventory, any unexpected selected
# binary, and the absence of ffi_link_e2e's real allocator slope probe. It runs
# ignored tests too, so a newly ignored memory verdict cannot disappear behind
# a green nextest summary.
macos-leak-oracle: test-leak-oracle-selftest hew-native
	$(TEST_RUN_ENV) scripts/macos-leak-oracle.sh

# Platform-independent teeth for the leak harness and the runner's inventory
# contract. The Rust counterfactuals inject missing/declined/malformed/timed-out
# inspector commands and incomplete work witnesses; the shell counterfactuals
# prove empty/shrunken inventories and a missing ffi authority are red.
test-leak-oracle-selftest:
	$(TEST_RUN_ENV) cargo nextest run --profile ci -p hew-cli --test leak_harness_fail_closed
	scripts/tests/test_macos_leak_oracle_runner.sh

# The C-ABI crate, run on its own.
#
# Every workspace-wide nextest invocation excludes hew-cabi: its `#[cfg(test)]`
# runtime stubs re-`#[no_mangle]` symbols hew-runtime also defines, so a
# workspace test binary that links both fails at link time. That exclusion is a
# link workaround, not a coverage decision -- but for a long time nothing ran
# the crate afterwards, so the FFI ownership contract's own tests executed on
# developer machines only. This target is that missing half; every job carrying
# an `--exclude hew-cabi` runs it.
test-cabi:
	$(TEST_RUN_ENV) cargo nextest run --profile ci-cabi -p hew-cabi

# Build the combined runtime+stdlib static lib and the WASM runtime before
# running the compiler-pipeline tests.  Several hew-cli integration tests
# (eval_e2e, eval_wasm_*) call `hew eval` which needs both libs at link time.
# Without this prerequisite the lazy per-test build of libhew.a (~18 s on a
# cold worktree) consumes most of the default 30 s `hew eval --timeout` budget,
# causing spurious timeouts under the concurrent nextest run.  The WASM runtime
# (libhew_runtime.a for wasm32-wasip1) is needed by wasm32-wasi eval tests
# even when they are expected to fail before codegen (the linker search runs
# before the fast typecheck path reports its diagnostic).
test-compiler-pipeline: test-artifacts test-compiler-lifecycle
	$(TEST_RUN_ENV) cargo nextest run --profile ci \
		-p hew-lexer \
		-p hew-parser \
		-p hew-types \
		-p hew-hir \
		-p hew-sir \
		-p hew-mir \
		-p hew-codegen-rs \
		-p hew-cli \
		-p hew-pkg

# The compiled-Hew lifecycle evidence is separate so CI jobs that already ran
# workspace nextest can retain this evidence without replaying its Rust tests.
test-compiler-lifecycle: test-opaque-resource-lifecycle-matrix

# Both lifecycle targets read the pinned ast-grep at
# .ast-grep/tool/bin/ast-grep and abort when it is absent. The toolchain is
# provisioned only in the CI jobs and shards that run these targets
# (.github/actions/setup-ast-grep, the same cache-then-verify shape as
# setup-llvm and the wasmtime install), not as a make prerequisite:
# `structural-lint-bootstrap-install` cargo-installs
# tree-sitter-cli and ast-grep and then runs a full authority scan, which is
# minutes of work that has no place inside a test target invoked from several
# other targets. Locally, any `make lint` provisions the same tree.
test-opaque-resource-lifecycle-matrix: wasm-runtime hew-native
	HEW_BIN="$(DEBUG_DIR)/hew" $(PYTHON) scripts/tests/test_opaque_resource_lifecycle_facts.py
	HEW_BIN="$(DEBUG_DIR)/hew" $(PYTHON) scripts/tests/test_opaque_resource_lifecycle_matrix.py

test-opaque-resource-lifecycle-matrix-external: wasm-runtime hew-native
	HEW_BIN="$(DEBUG_DIR)/hew" $(PYTHON) scripts/tests/test_opaque_resource_lifecycle_facts.py
	HEW_BIN="$(DEBUG_DIR)/hew" $(PYTHON) scripts/tests/test_opaque_resource_lifecycle_matrix.py --runtime-profile external-network

# End-to-end Hew compiler oracle: real .hew fixtures through check/compile/run.
# Build libhew first so native fixture links use the current product.
test-vertical-slice: hew-native ## Test: run the end-to-end compiler oracle
	bash tests/vertical-slice/test-compile-accept.sh
	HEW_BIN="$(DEBUG_DIR)/hew" bash tests/vertical-slice/run.sh

# Cross-module package-import oracle: fixtures importing the in-tree
# `hew::testffi` package through `hew run --pkg-path` — imported-actor value
# asks, imported-type trait methods, and the [native] auto-link path.
test-pkg-import: hew-native
	HEW_BIN="$(DEBUG_DIR)/hew" bash tests/pkg-import/run.sh

# Package-manager consumer oracle: publish-like local setup, `hew install`,
# lock/materialization assertions, `hew check`, and exact `hew run`
# stdout under an isolated HOME.
test-package-install: hew-native ## Test: prove installed packages import and execute
	HEW_BIN="$(DEBUG_DIR)/hew" bash tests/package-install/run.sh

# Golden MIR corpus (examples/v05/checked-mir): byte-identical --dump-mir
# oracle for internal retyping work. `checked-mir-verify` re-dumps every
# fixture and diffs against the committed goldens; `checked-mir-golden`
# recaptures them (only in a commit that justifies the dump change).
checked-mir-verify: hew-native
	HEW_BIN="$(DEBUG_HEW)" bash scripts/checked-mir-corpus.sh verify

# Regenerate explicitly with `make checked-mir-golden`.

checked-mir-golden: hew-native
	HEW_BIN="$(DEBUG_HEW)" bash scripts/checked-mir-corpus.sh golden

# Execution gate for the same corpus: build and run every fixture and diff
# a transcript (exit status + verbatim stdout) against its committed
# `<name>.expected` sibling.  Dumping is not running — a fixture can
# segfault on every execution while every golden stays byte-identical, so
# checked-mir-verify alone is not evidence that a drop-elaboration or
# codegen change is correct.  Runnability is read back from the compiler
# (a fixture is runnable exactly when its raw MIR declares `main`), and
# the expectation set is closed both ways: a fixture with `main` and no
# expectation fails, an expectation for a fixture without `main` fails.
checked-mir-run: hew-native
	HEW_BIN="$(DEBUG_HEW)" bash scripts/checked-mir-corpus.sh run

# Regenerate explicitly with `make checked-mir-expect`.

# Artifacts only.

checked-mir-expect: hew-native
	HEW_BIN="$(DEBUG_HEW)" bash scripts/checked-mir-corpus.sh expect

# Repeated-compile determinism over the LL-oracle corpus: the same input
# compiled several times must produce the same exit status, the same
# `ownership EdgeCarry` ordering in raw MIR, and byte-identical stderr.
# checked-mir-verify compares a single run against a committed golden, so it
# cannot see a compiler that reorders hashed ownership facts or
# accumulated diagnostics from run to run.  This gate is the one that can.
# inputs: tests/ll-oracle/corpus/*.hew scripts/compile-determinism-corpus.sh
# inputs: hew-hir/src/*.rs hew-mir/src/*.rs hew-cli/src/*.rs
compile-determinism-verify: hew-native
	HEW_BIN="$(DEBUG_HEW)" bash scripts/compile-determinism-corpus.sh

# Build-only form for targeted validation.
compile-determinism-verify-build: hew-native
	@:

# inputs: scripts/tests/test_compile_determinism_corpus.py scripts/compile-determinism-corpus.sh
compile-determinism-selftest:
	$(PYTHON) scripts/tests/test_compile_determinism_corpus.py

compile-determinism-selftest-build:
	@:

# ── SIR admission gates (dev-only, until the legacy lowerer is deleted) ──────
# Until the final ladder's cutover, each function body is either taken by SIR
# or still owned by the legacy HIR->MIR body lowerer. `sir-coverage`
# inventories every function body over the corpora (free fns, impl methods,
# actor/machine handler bodies) and compares the admitted COUNT with the
# committed ratchet: a drop fails, a rise prints the new value to record. The
# ratchet is a raw count, not a percentage, so it moves only when a body's
# own admission outcome changes — never merely because a corpus entry was
# added or removed.
# inputs: scripts/sir-coverage-ratchet.txt hew-cli/src/sir_coverage.rs hew-sir/src/*.rs
SIR_COVERAGE_CORPORA := tests/vertical-slice/accept tests/hew examples std
sir-coverage: hew-native ## Test: fail when the SIR admission count drops below its ratchet
	$(DEBUG_HEW) tool sir-coverage --ratchet scripts/sir-coverage-ratchet.txt $(SIR_COVERAGE_CORPORA)

# Every program the strict SIR lane admits is also compiled through the
# legacy route; both binaries run and their exit status and stdout must be
# byte-identical. The fixture directory guarantees at least one admitted
# program so the harness never passes by comparing nothing. The ratchet is a
# second, independent floor on how many programs got compared at all: the
# MIR bridge the strict route compiles through is narrower than SIR
# admission, so a bridge-only regression can de-admit most of the corpus
# while `sir-coverage` sees no change.
# inputs: scripts/sir-parity.sh scripts/sir-parity-ratchet.txt hew-cli/tests/fixtures/sir-parity/*.hew
sir-parity: hew-native ## Test: run SIR-route and legacy-route binaries and compare their output
	HEW_BIN="$(DEBUG_HEW)" bash scripts/sir-parity.sh --ratchet scripts/sir-parity-ratchet.txt hew-cli/tests/fixtures/sir-parity $(SIR_COVERAGE_CORPORA)

# Dogfood-shaped compile measurement. The raw IR byte ceiling is a real
# regression gate; timings remain observational. Lint already builds the same
# release-lib compiler for hew-fmt-check, so this adds only the focused compile.
#
#         tests/compile-measure/** scripts/dogfood-compile-measure.sh
# The gate measures define blocks, excluding host-specific module headers.
# It uses Cargo's resolved release-lib binary by default, and honours HEW_BIN
# when a caller supplies a staged compiler explicitly.
HEW_BIN ?= $(RELEASE_LIB_HEW)
LINT_GATES += dogfood-compile-measure
dogfood-compile-measure: hew
	HEW_BIN="$(HEW_BIN)" bash scripts/dogfood-compile-measure.sh

# MIR lowering time budget. The IR-size gate above measures what lowering
# produces; this one measures what lowering costs.
LINT_GATES += bench-mir
bench-mir: hew ## Test: fail when MIR lowering time exceeds its budget
	HEW_BIN="$(HEW_BIN)" bash scripts/bench-mir.sh

# Fast hew-runtime target: runs lib unit tests and all integration tests without the heavy
# QUIC/TLS/profiler feature stack (quinn, rustls, rcgen, ring, hyper, snow).
# Compile time is ~3× lower than the default-features build (measured: ~32s vs ~85s per binary).
# Profiler allocator tests in transport.rs are skipped (they require feature = "profiler").
# Run `cargo test -p hew-runtime` for the full suite including QUIC, TLS, and profiler paths.
test-runtime-unit:
	$(TEST_RUN_ENV) cargo nextest run --profile ci -p hew-runtime --no-default-features

# Ratcheted wrappers for the Hew-language test suites.
#
# These targets run the suites through scripts/corpus-ratchet.sh, which
# compares the set of failing tests against an exhaustive tracked-failures
# list. Unexpected failures fail every gate; recovered tracked failures are
# reported in PRs and fail only when RATCHET_STRICT_RECOVERIES=1. When the
# converging lanes land and tracked failures drop to zero, delete the list
# entries; the ratchets then pass with no tracking overhead.
#
# HEW_O0_OUTCOMES_FILE, when set, wires the ratchet's O0 outcome capture into
# test-o2-differential's O0 baseline so the differential gate does not re-run
# the identical O0 pass a second time (CI sets this across both targets in the
# same job; plain `make test-hew-ratchet` / `make test-o2-differential` with no
# env var keep their original standalone behaviour).
test-hew-ratchet:

ifneq ($(strip $(HEW_SHARD_REPORT_DIR)),)
test-hew-ratchet:
	$(PYTHON) scripts/compiled-hew-shards.py aggregate --mode ratchet \
		--reports-dir "$(HEW_SHARD_REPORT_DIR)" \
		--full-inventory "$(HEW_FULL_INVENTORY)" \
		--shard-count "$(HEW_SHARD_COUNT)" $(RATCHET_STRICT_RECOVERIES_ARG)

# The shard-aggregate form reads reports; it builds nothing.
else
test-hew-ratchet: hew-native ## Test: run compiled Hew suites against their ratchet
	@echo "==> Running Hew test suite (ratcheted)"
	PYTHON="$(PYTHON)" HEW_BIN="$(DEBUG_DIR)/hew" scripts/corpus-ratchet.sh hew-suite $(if $(HEW_O0_OUTCOMES_FILE),--emit-o0-outcomes "$(HEW_O0_OUTCOMES_FILE)")

endif

# The core matrix: every core primitive crossed with every common operation,
# one runnable program per cell, each asserting the exact value and -- where
# the row carries a `#[resource]` whose close prints -- exactly-once release.
#
# The corpus is an ENUMERATION of the language, not an accretion of fixtures:
# it is generated by scripts/core-matrix-gen.py, so a new primitive is a new
# row and a new operation is a new column. tests/core-matrix/matrix.tsv records
# the outcome class of every cell today, and this gate fails on drift in either
# direction -- a passing cell that regresses, and a recorded failure that
# starts passing (which means the table is stale and must be re-recorded).
#
# The generator self-check runs first: a cell cannot be hand-edited into
# agreement with a broken compiler without the corpus diverging from the
# enumeration that produced it.
test-core-matrix: hew-native
	@echo "==> Checking the core-matrix corpus matches its generator"
	@rm -rf "$(CURDIR)/.tmp/core-matrix-regen"
	$(PYTHON) scripts/core-matrix-gen.py --out "$(CURDIR)/.tmp/core-matrix-regen"
	diff -r tests/core-matrix/cells "$(CURDIR)/.tmp/core-matrix-regen"
	@echo "==> Running the core matrix (primitive x operation)"
	HEW_BIN="$(DEBUG_DIR)/hew" $(PYTHON) scripts/core-matrix.py $(RATCHET_STRICT_RECOVERIES_ARG)

# Regen seam: driven only by an explicit
# `make core-matrix-record`.
core-matrix-record: hew-native
	HEW_BIN="$(DEBUG_DIR)/hew" $(PYTHON) scripts/core-matrix.py --record

# Regen seam: re-dumps every row of the funcupdate/reassign manifest. The dump's
# function order is nondeterministic, so this is a reviewed act, never a sweep.
funcupdate-mir-baselines-golden: hew
	@set -e; \
	baseline_dir=tests/mir-baselines/funcupdate-reassign; \
	grep -v '^#' "$$baseline_dir/manifest.tsv" | while IFS="$$(printf '\t')" read -r fixture baseline; do \
	  [ -n "$$fixture" ] || continue; \
	  echo "re-dumping $$fixture -> $$baseline"; \
	  "$(BUILD_DIR)/bin/hew" compile --dump-mir elab "$$fixture" > "$$baseline_dir/$$baseline"; \
	done

# Direct-call match carriers have a separate exact-count corpus because the
# ordinary Hew suites do not pin ownership-verifier finding counts. Every fixture is checked
# under inherited and HEW_*-scrubbed environments, and any count drift in
# either direction fails.
test-ownership-balance-corpus: hew-native hew
	HEW_BIN="$(DEBUG_DIR)/hew" HEW_RELEASE_BIN="$(RELEASE_LIB_DIR)/hew" \
		$(PYTHON) tests/ownership-balance/run.py

test-ownership-balance-runner-selftest:
	$(PYTHON) scripts/tests/test_ownership_balance_run.py
	$(PYTHON) scripts/tests/test_obligation_site_diff.py


# The -O0-vs-O2 differential-exec parity gate: every compiled `.hew` program
# must behave identically at -O0 and -O2. The no-miscompile oracle for the LLVM
# middle-end pipeline (RC9). A divergence is a miscompile and a full stop.
test-o2-differential:

ifneq ($(strip $(HEW_SHARD_REPORT_DIR)),)
test-o2-differential:
	$(PYTHON) scripts/compiled-hew-shards.py aggregate --mode differential \
		--reports-dir "$(HEW_SHARD_REPORT_DIR)" \
		--full-inventory "$(HEW_FULL_INVENTORY)" \
		--shard-count "$(HEW_SHARD_COUNT)"

# The shard-aggregate form reads reports; it builds nothing.
else
test-o2-differential: hew-native
	@echo "==> Running -O0-vs-O2 differential-exec parity gate"
	PYTHON="$(PYTHON)" HEW_BIN="$(DEBUG_DIR)/hew" scripts/o2-differential.sh $(if $(HEW_O0_OUTCOMES_FILE),--o0-outcomes "$(HEW_O0_OUTCOMES_FILE)")

endif

o2-differential-selftest:
	PYTHON="$(PYTHON)" bash scripts/o2-differential-selftest.sh

# Shell only; no artifacts.

test-stdlib-ratchet: hew-native ## Test: type-check the standard library against its ratchet
	@bash scripts/tests/test_stdlib_ratchet_bare_variants.sh
	@echo "==> Type-checking stdlib (ratcheted)"
	HEW_BIN="$(DEBUG_HEW)" scripts/corpus-ratchet.sh stdlib

# Every stdlib source must stay clean in isolation, and every module must stay
# silent when checked and built through a temporary user package.
stdlib-user-build-clean: hew-native
	HEW_BIN="$(DEBUG_DIR)/hew" scripts/stdlib-user-build-clean.py

# Run every examples/ux and examples/progressive tutorial against its paired
# .expected file. The shared runner fails closed on missing/orphan expectations,
# nonzero exit status, timeout, output drift, empty inventory, and duplicate
# admission. New examples therefore cannot disappear from the authority by
# omitting their expectation.
#
# One inventory definition, shared by the gate and its regen seam: a corpus that
# drifts between the two would gate one set of examples and re-record another.
UX_EXAMPLE_INVENTORY = --label "ux + progressive tutorial" \
	  --source-root examples/ux \
	  --source-root examples/progressive

test-ux-examples: hew-native test-example-expectations-selftest
	@echo "==> Running ux + progressive tutorials against .expected"
	@$(PYTHON) scripts/example-expectations.py \
	  --hew-bin "$(DEBUG_DIR)/hew" $(UX_EXAMPLE_INVENTORY)

# Regen seam: driven only by an explicit
# `make ux-examples-expect`, never by a
# blanket regen. An example's output is its user-facing contract.
ux-examples-expect: hew-native
	@$(PYTHON) scripts/example-expectations.py \
	  --hew-bin "$(DEBUG_DIR)/hew" $(UX_EXAMPLE_INVENTORY) --write-expected

# Artifacts only: the expectations self-test belongs to the gate.

# Run every offline v0.5-surface example against its paired .expected file.
# Two lanes:
#   1. examples/v05/surfaces/*.hew — idiomatic single-file demos for the landed
#      v0.5 surfaces (typed streams, regex captures, template, unicode). Pure,
#      deterministic, no I/O.
#   2. examples/net/http_await_service.hew — the async HTTP/1.1 flagship. It is
#      LOOPBACK-only (127.0.0.1) so it needs no external network and is offline;
#      its output is deterministic and was verified stable across repeated runs,
#      so it is gated here too.
# The TLS client (examples/net/tls_client.hew) is intentionally NOT gated: it
# dials a real public host (example.com:443) — a genuine outbound network
# dependency that cannot run offline — and additionally exercises a known TLS
# data-plane ABI gap (it fails closed on a short write). It ships a paired
# .expected for local diffing only. See examples/README.md for the rationale.
#
# The comparison merges stderr into stdout DELIBERATELY. An `.expected` file is
# the example's whole observable contract: a shipped example that prints an
# unannounced diagnostic is a defect whether the text lands on fd 1 or fd 2.
# Splitting the streams would let a new compiler warning ride along unnoticed —
# exactly the failure this lane exists to catch. A diagnostic an example is
# supposed to print is recorded verbatim in its `.expected`, so the strictness
# costs nothing legitimate.
#
# The shared runner treats the surface inventory as closed: missing or orphan
# expectations, process failures, timeouts, and output drift all fail the gate.
# `scanner_tokens.hew` is fully admitted with its repaired five-line output.
#
SURFACE_EXAMPLE_INVENTORY = --label "surface" \
	  --source-root examples/v05/surfaces \
	  --source examples/net/http_await_service.hew

test-surface-examples: hew-native test-example-expectations-selftest
	@echo "==> Running v0.5 surface examples against .expected"
	@$(PYTHON) scripts/example-expectations.py \
	  --hew-bin "$(DEBUG_DIR)/hew" $(SURFACE_EXAMPLE_INVENTORY)

# Regen seam: see ux-examples-expect.
surface-examples-expect: hew-native
	@$(PYTHON) scripts/example-expectations.py \
	  --hew-bin "$(DEBUG_DIR)/hew" $(SURFACE_EXAMPLE_INVENTORY) --write-expected

# Artifacts only: the expectations self-test belongs to the gate.

test-example-expectations-selftest:
	@$(PYTHON) scripts/tests/test_example_expectations.py

# Python only; no artifacts.

# Check ```hew fenced blocks in docs/ and std/ against hew check.
# Extracts each fence from the Markdown guides, docs/language/*.hew module
# doc blocks, and every std/**/*.hew doc comment into .tmp/doc-fences/, runs
# `hew check` on each, and applies the ratchet from
# scripts/doc-test-expected-failures.txt so known-failing fences do not block
# the gate while new failures always do.
#
# Skip-annotated fences (<!-- doctest: skip --> or preceding NYI callout) are
# never compiled — they describe aspirational or not-yet-implemented surfaces.
# Fail-closed default: a fence is compiled unless explicitly skipped.
#
# Run `make test-doc-examples` after any docs/ or std/ change to confirm no
# fence regressions were introduced.
test-doc-examples: hew-native
	@HEW_BIN="$(DEBUG_HEW)" scripts/corpus-ratchet.sh doc-fences

# Drive matching and mutated doc-failure sets through the production harness.
doc-ratchet-selftest:
	@scripts/tests/test_doc_ratchet_membership.sh
	@scripts/tests/test_std_doc_fence_extraction.sh

# Shell/python only; no artifacts.

# Nightly rust-runtime ASan command (Linux/nightly toolchain required).
#
# ASAN_SYMBOLIZER_PATH: ASan/LSan use llvm-symbolizer to resolve addresses
# into function names for suppression matching.  On Debian/Ubuntu the binary
# lives under /usr/lib/llvm-N/bin/ but is not always on PATH.  Detect the
# highest-versioned copy available and export it so LSAN suppression patterns
# that match by function name (e.g. leak:hew_sched_init) fire correctly.
# Falls back gracefully to the empty string if none is found (suppressions
# may not apply without a symbolizer, but the build will still run).
# NOTE: GNU make $(sort) is lexicographic, so llvm-9 would rank after llvm-17.
# Use a shell pipeline with sort -V (version-aware) to find the newest copy.
ASAN_SYMBOLIZER ?= $(shell ls /usr/lib/llvm-*/bin/llvm-symbolizer 2>/dev/null | sort -V | tail -1)
# NOTE: the suppressions path is passed RELATIVE, not absolute. The sanitizer
# runtime parses [LA]SAN_OPTIONS as a space-tolerant key=value:key=value list and
# treats an embedded space as a separator, so an absolute worktree path
# containing a space (e.g. /Volumes/Extreme SSD/...) aborts before any test runs
# with "expected '=' in LSAN_OPTIONS" (hew-lang/hew#1889). cargo runs the
# hew-runtime test binary with cwd = the package dir (hew-runtime/), so the bare
# filename `lsan.supp` resolves correctly and never contains a space regardless
# of the absolute worktree location.
# The runtime crate is instrumented but the prebuilt sysroot is not. Nightly
# Rust rejects that sanitizer ABI mismatch unless it is explicitly allowed;
# this lane intentionally measures Hew's runtime rather than rebuilding std.
# Hew installs its own per-worker alternate signal stack. Asking compiler-rt to
# manage a competing stack makes it unmap Hew's heap-backed stack at thread exit.
asan:
	CARGO_TARGET_DIR=$(RUNTIME_ASAN_TARGET_DIR) \
	RUSTFLAGS="-Zsanitizer=address -Cforce-frame-pointers=yes -Cunsafe-allow-abi-mismatch=sanitizer" \
	ASAN_OPTIONS="detect_leaks=1:use_sigaltstack=0" \
	ASAN_SYMBOLIZER_PATH=$(ASAN_SYMBOLIZER) \
	LSAN_OPTIONS="suppressions=lsan.supp" \
	cargo +nightly test --target $(SANITIZER_RUST_TARGET) -p hew-runtime --lib -- --test-threads=1

# ASan gate for compiled .hew fixture binaries (Linux/nightly toolchain required).
#
# Unlike `make asan` (which instruments the Rust runtime crate under test),
# this target builds an ASan-instrumented copy of the full hew toolchain
# (hew CLI + libhew.a) using nightly Rust, then compiles .hew leak-test
# fixtures against that instrumented library and runs them under
# ASAN_OPTIONS=detect_leaks=1.  This catches leaks in the GENERATED CODE
# emitted by hew (the Vec<string> compare-temp leak and the owned array-repeat
# clone leak were only caught by the macOS `leaks` oracle before this gate).
#
# Passes LLVM_VERSION through to the script if set (e.g. LLVM_VERSION=22).
asan-fixtures: test-asan-fixture-selftest
ifeq ($(shell uname -s),Darwin)
	@echo "asan-fixtures: skipped on macOS — use the leaks oracle in hew-cli/tests/*_leak_oracle.rs"
else
	LLVM_VERSION=$(LLVM_VERSION) \
	SANITIZER_RUST_TARGET=$(SANITIZER_RUST_TARGET) \
	scripts/asan-fixture-check.sh
endif

# Dynamic proof that a TaskEntry adapter's cancel-exit never publishes a
# substitute composite return value as a task result.
#
# This gate is NOT superseded by the Rust suites that surround it.
# hew-codegen-rs/tests/emission/task_entry_cancel_composite_emission.rs pins the
# emitted IR shape, and hew-cli/tests/task_entry_composite_cancel_e2e.rs covers
# the sibling non-cancelled paths -- neither can force the cancel edge, because
# the trigger needs a task's own entry-block cooperate check to observe
# cancellation before the body stores anything. Only a program linked against
# libhew built with hew-runtime/forced-cancel-test can do that, which is why
# this lives in a script with its own isolated target directory rather than in
# the workspace test run.
forced-cancel-composite-check:
	bash scripts/forced-cancel-composite-check.sh


# Platform-independent counterfactuals for the ASan/LSan sentinel: a genuine
# sanitizer diagnostic must be accepted, while a bare non-zero probe exit must
# stay red instead of certifying instrumentation that never reported a leak.
test-asan-fixture-selftest:
	scripts/asan-fixture-check.sh --selftest

# Shell only; no artifacts.

# Nightly rust-runtime TSan command (Linux/nightly toolchain required).
#
# TSan is not currently supported on darwin-arm64 by the upstream Rust
# nightly toolchain (build-std + TSan link failures, mirrored by the
# nightly-sanitizers.yml advisory lane).  Skip with a clear message so
# the make target is a usable signal rather than a confusing failure.
tsan:
ifeq ($(shell uname -sm),Darwin arm64)
	@echo "tsan: skipped on darwin-arm64 (upstream Rust nightly TSan not supported on this target — see the rust-runtime-tsan advisory job in nightly-sanitizers.yml)"
else
	CARGO_TARGET_DIR=$(RUNTIME_TSAN_TARGET_DIR) \
	RUSTFLAGS="-Zsanitizer=thread -Cforce-frame-pointers=yes -Cunsafe-allow-abi-mismatch=sanitizer" \
	TSAN_OPTIONS="$${TSAN_OPTIONS:-halt_on_error=0 suppressions=tsan.supp}" \
	cargo +nightly test \
		--target $(SANITIZER_RUST_TARGET) \
		-p hew-runtime \
		--no-default-features \
		--lib \
		-- --test-threads=1
endif

# Curated rust-runtime Miri command (aliasing / uninit-read / provenance axis).
#
# Miri interprets MIR, so it catches the Stacked/Tree-Borrows aliasing,
# uninitialised-read, and pointer-provenance bugs that ASan/TSan cannot — and it
# runs on any host (no sanitizer-capable target required).  The lane is scoped to
# hew-runtime's pure-Rust unsafe data structures; the FFI / syscall / socket
# surface stays on ASan because Miri cannot execute foreign code.  Optional
# features (tokio/quinn/profiler) are excluded for the same reason TSan excludes
# them — they pull in async + network FFI that Miri cannot interpret.
#
# One-time setup:  rustup component add --toolchain $(MIRI_TOOLCHAIN) miri rust-src
# CI pins nightly-2026-06-14 (see .github/workflows/nightly-sanitizers.yml); the
# default floats to the host `nightly` so a dev box with Miri installed just works.
#
# MIRIFLAGS:
#   -Zmiri-disable-isolation       — timer/clock/random reads need host time/entropy.
#   -Zmiri-permissive-provenance   — silences the benign int->ptr cast warning from
#     cross-thread pointer hand-off in tests; does not weaken UB detection.
#
# The trailing filters are the GREEN allowlist (substring-matched, `::`-anchored
# to module boundaries).  arena runs against the `cfg(miri)` std::alloc shim that
# replaces mmap/VirtualAlloc.  send_ptr/tagged_union carry no dedicated unit tests
# yet (exercised transitively) but stay listed as curated-surface members.
MIRI_TOOLCHAIN ?= nightly
MIRI_ALLOWLIST := send_ptr:: rc:: arc:: tagged_union:: arena:: bytes:: vecdeque:: vec::
miri:
	CARGO_TARGET_DIR=$(RUNTIME_MIRI_TARGET_DIR) \
	MIRIFLAGS="-Zmiri-disable-isolation -Zmiri-permissive-provenance" \
	cargo +$(MIRI_TOOLCHAIN) miri test \
		-p hew-runtime \
		--no-default-features \
		--lib \
		-- $(MIRI_ALLOWLIST)

# ── Lint ────────────────────────────────────────────────────────────────────

.SECONDEXPANSION:
lint: $$(LINT_GATES) ## Check: run the complete local lint graph and Clippy
	cargo fmt --all -- --check
	cargo clippy --workspace --tests -- -D warnings

LINT_GATES += legacy-path-syntax-lint
legacy-path-syntax-lint:
	$(PYTHON) scripts/lint-legacy-path-syntax.py

# Python only; no artifacts.

# Self-provisioning: the pinned toolchain install is a prerequisite of every
# structural-lint entry point, not a separate manual step. The install path
# (scripts/ast-grep-lint.sh --bootstrap --install-only, via
# build-ast-grep-lang.sh) is idempotent and checks the pinned lock/version
# before touching the network or recompiling, so a warm cache makes this a
# fast no-op — local `make lint` and CI both provision through the same
# target instead of drifting. --install-only stops after the verified
# install: the audit and the scan belong to the structural-lint recipe
# below, so provisioning a consumer never re-runs the lint gate.
LINT_GATES += structural-lint
.NOTPARALLEL: structural-lint structural-lint-bootstrap
structural-lint: structural-lint-bootstrap-install test-ast-grep-contract ## Check: run structural and compiler-authority ratchets
	scripts/ast-grep-lint.sh

structural-lint-bootstrap: structural-lint-bootstrap-install test-ast-grep-contract

structural-lint-bootstrap-install:
	scripts/ast-grep-lint.sh --bootstrap --install-only

test-ast-grep-contract:
	bash scripts/tests/test_ast_grep_contract.sh

LINT_GATES += test-build-harness
# Focused behavior tests for the Hew JUnit transaction, generated help, shell discovery,
# and compiled-Hew report aggregation. None needs a built compiler.
test-build-harness:
	$(PYTHON) scripts/tests/test_hew_suite_runner.py
	$(PYTHON) scripts/tests/test_makefile_interfaces.py
	$(PYTHON) scripts/tests/test_cargo_output_dir.py
	$(PYTHON) scripts/tests/test_compiled_hew_shards.py

# Python and shell only; no artifacts.

LINT_GATES += hew-fmt-check
# Check that std/ and examples/ .hew sources are formatted.
# Run `find std examples -name "*.hew" -print0 | xargs -0 hew fmt` to fix.
hew-fmt-check: hew
	@echo "==> hew-fmt-check: checking std/ and examples/ .hew sources"
	@total=$$(find std examples -name "*.hew" | wc -l | tr -d ' '); \
	bash scripts/lib/corpus-nonempty.sh hew-fmt-check-files "$$total" || exit 1; \
	find std examples -name "*.hew" -print0 \
	    | xargs -0 "$(BUILD_DIR)/bin/hew" fmt --check \
	    && echo "hew-fmt-check passed: all $$total .hew sources are formatted." \
	    || { echo "error: unformatted .hew sources found — run 'find std examples -name \"*.hew\" -print0 | xargs -0 hew fmt' to fix." >&2; exit 1; }

# Exercise representative migration inputs in an isolated copy so the proof
# never edits the checkout. The second pass must leave the first-pass snapshot
# byte-identical.
test-migrate-corpus: hew
	@set -e; migration_root=$$(mktemp -d); migration_fixed=$$(mktemp -d); \
	trap 'rm -rf "$$migration_root" "$$migration_fixed"' 0; \
	cp -R tests/corpus/migrate/. "$$migration_root/"; \
	echo "1/6 migrate accepted representative sources"; \
	"$(BUILD_DIR)/bin/hew" fmt --migrate --root "$$migration_root/accept"; \
	echo "2/6 compare exact migrated sources"; \
	for migration_source in "$$migration_root"/accept/*.hew; do \
		migration_expected="$${migration_source%.hew}.expected"; \
		diff -u "$$migration_expected" "$$migration_source"; \
	done; \
	echo "3/6 require the unresolvable source to fail loudly"; \
	migration_refusal="$$migration_root/refusal.log"; \
	if "$(BUILD_DIR)/bin/hew" fmt --migrate --root "$$migration_root/reject" >"$$migration_refusal" 2>&1; then \
		cat "$$migration_refusal"; \
		echo "error: migration accepted the unresolvable representative site" >&2; \
		exit 1; \
	fi; \
	grep -F 'unresolvable.hew:24-35: type checking failed: undefined function `Missing`' "$$migration_refusal"; \
	diff -u tests/corpus/migrate/reject/unresolvable.hew "$$migration_root/reject/unresolvable.hew"; \
	echo "4/6 prove the migrated snapshot reaches a successful typecheck"; \
	for migration_source in "$$migration_root"/accept/*.hew; do \
		"$(BUILD_DIR)/bin/hew" check "$$migration_source"; \
	done; \
	echo "5/6 require a byte-identical second migration pass"; \
	cp -R "$$migration_root/accept/." "$$migration_fixed/"; \
	"$(BUILD_DIR)/bin/hew" fmt --migrate --root "$$migration_root/accept"; \
	diff -ru "$$migration_fixed" "$$migration_root/accept"; \
	echo "6/6 require check mode to recognize the fixed point"; \
	"$(BUILD_DIR)/bin/hew" fmt --migrate --check --root "$$migration_root/accept"

# Derive the compilable corpus from the tracked source roots, format a private
# path-preserving mirror, then require the result to check and reach a fixed point.
hew-fmt-property: hew
	HEW_BIN="$(BUILD_DIR)/bin/hew" bash scripts/hew-fmt-property.sh

# Repo-wide hew check sweep over all tracked .hew files (excluding intentional
# reject fixtures).  Ratchets against scripts/hew-corpus-expected-failures.txt.
# Catches the class of bug where a symbol rename or type change lands in the
# compiler but fixture files across crates/tests/examples are silently missed.
# See scripts/corpus-ratchet.sh for the allowlist format and classification guide.
hew-check-all: hew-native
	@echo "==> hew-check-all: compiling full .hew corpus"
	HEW_BIN="$(DEBUG_HEW)" scripts/corpus-ratchet.sh hew-corpus

# Runs one journey script under repros/journeys/ (day-one, day-two, or
# week-one-local) against HEW_BIN and ratchets its `step <id>: pass|fail`
# lines against scripts/journeys-expected.tsv: the target fails when a
# step outside that file fails, or a step listed in it now passes (V060-FD-1).
# inputs: repros/journeys/*.sh scripts/run-journeys.sh scripts/journeys-expected.tsv
test-journeys: hew ## Test: run a repros/journeys script and ratchet its steps (JOURNEY=day-one|day-two|week-one-local)
	@if [ -z "$(JOURNEY)" ]; then echo "usage: make test-journeys JOURNEY=day-one|day-two|week-one-local" >&2; exit 64; fi
	HEW_BIN="$(HEW_BIN)" bash scripts/run-journeys.sh $(JOURNEY)

# Five runs of `hew check` on the largest std module a newcomer's program
# pulls in; the median wall-clock cannot exceed 2x the recorded baseline
# for this host class (uname -m plus the CI runner label when CI is set,
# else "local"). A class with no baseline records one and passes: a first
# run on a new runner class cannot be compared against itself (V060-FD-1).
# inputs: scripts/check-time-ratchet.sh scripts/check-time-baseline.tsv std/net/http/http.hew
check-time-ratchet: hew ## Test: fail when hew check's median time on the fixture exceeds 2x baseline
	HEW_BIN="$(HEW_BIN)" bash scripts/check-time-ratchet.sh check

check-time-ratchet-record: hew ## Build: record scripts/check-time-baseline.tsv's median for this host class
	HEW_BIN="$(HEW_BIN)" bash scripts/check-time-ratchet.sh record

# Per-crate `wc -l` over <crate>/src/**/*.rs against scripts/size-ratchet.tsv's
# ceilings; a crate over its ceiling fails the gate. wc -l only, no hew build
# needed.
# inputs: scripts/size-ratchet.sh scripts/size-ratchet.tsv Cargo.toml
size-ratchet: ## Test: fail when a workspace crate's line count exceeds its ceiling
	bash scripts/size-ratchet.sh check

size-ratchet-record: ## Build: record scripts/size-ratchet.tsv's per-crate counts as ceilings
	bash scripts/size-ratchet.sh record

.PHONY: codegen-trap-inventory-check
LINT_GATES += codegen-trap-inventory-check
codegen-trap-inventory-check:
	$(PYTHON) scripts/check-codegen-trap-inventory.py

# Python only; no artifacts.

# Smoke-test the release binary with `hew run` to catch process-exit aborts
# (e.g. libc++ ABI mismatch at locale destructor — issue #1606).
# Builds release binary then runs a trivial program and checks exit 0 + output.
test-release-binary: release-host
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

# rg only; no artifacts.

stdlib-lint: stdlib-errno-gate
	bash scripts/lint-stdlib-int-surface.sh

# rg over std/ only; no artifacts.

# ── Coverage ───────────────────────────────────────────────────────────────
#
#   make coverage          — Rust unit/integration tests only (cargo llvm-cov)
#   make coverage-summary  — Rust-only, terminal summary
#   make coverage-lcov     — Rust-only, lcov.info for external tooling
#   make coverage-runtime  — runtime (libhew) FFI coverage exercised by
#                            compiled-and-run Hew programs (print/assert/vec/
#                            string/bytes/hashmap/actor/...) — the surface the
#                            Rust-only report cannot see. See
#                            scripts/coverage-runtime-e2e.sh.
#   make coverage-combined — both of the above, printed as TWO reports.
#   make coverage-branch   — Rust-only WITH branch coverage (needs nightly).
#
# Why coverage-combined is two reports, not one merged number: the runtime FFI
# counters come from compiled Hew program binaries, whose covmap is keyed by
# function structural hashes that do NOT match the cargo-test binaries. llvm-cov
# cannot fold e2e profraw into the cargo-llvm-cov report — verified empirically
# (cross-object reporting yields all-zero + "mismatched data"). The honest
# product is therefore two coherent reports, not a fabricated union.
#
# Requires: cargo-llvm-cov + the rustc llvm-tools-preview component (the harness
# auto-discovers version-matched llvm-profdata/llvm-cov from the rust sysroot).

COV_DIR          := coverage-out

# Rust-only coverage (cargo test) — unchanged stable default.
coverage:
	cargo llvm-cov --workspace --exclude hew-wasm --html --output-dir $(COV_DIR)/html
	@echo "==> Open $(COV_DIR)/html/index.html"

coverage-summary:
	cargo llvm-cov --workspace --exclude hew-wasm --no-report
	cargo llvm-cov report --summary-only

coverage-lcov:
	cargo llvm-cov --workspace --exclude hew-wasm --lcov --output-path $(COV_DIR)/lcov.info
	@echo "==> Wrote $(COV_DIR)/lcov.info"

# Runtime FFI coverage via compiled-and-run Hew programs. Builds an
# instrument-coverage libhew.a, links example programs with the profiler runtime
# (HEW_COVERAGE=1, handled in hew-cli/src/link.rs), runs them, and reports the
# runtime/stdlib surface. Pass HTML=1 for an HTML report.
coverage-runtime:
	bash scripts/coverage-runtime-e2e.sh $(if $(HTML),--html,)

# Combined: the Rust-test report AND the runtime-FFI report. Two reports by
# construction (see header note above) — neither subsumes the other.
coverage-combined:
	@echo "==> Report 1/2: Rust unit/integration test coverage (cargo-llvm-cov)"
	cargo llvm-cov --workspace --exclude hew-wasm --no-report
	cargo llvm-cov report --summary-only
	@echo ""
	@echo "==> Report 2/2: runtime (libhew) FFI coverage via compiled Hew programs"
	bash scripts/coverage-runtime-e2e.sh $(if $(HTML),--html,)
	@echo ""
	@echo "==> Two reports above: Rust-test crates, then the runtime FFI surface."
	@echo "    They are separate by construction; see the Makefile coverage header."

# Branch coverage of the Rust-test suite. Branch instrumentation is nightly-only
# (cargo-llvm-cov --branch refuses on stable), so this target opts into nightly
# explicitly rather than changing the stable default of `make coverage`.
coverage-branch:
	cargo +nightly llvm-cov --branch --workspace --exclude hew-wasm \
	  --html --output-dir $(COV_DIR)/branch-html
	@echo "==> Open $(COV_DIR)/branch-html/index.html"

# ── FFI symbol verification ───────────────────────────────────────────────
# Validates that every hew-runtime #[no_mangle] export is classified in
# scripts/jit-symbol-classification.toml (stable vs internal).

LINT_GATES += verify-ffi
verify-ffi: cabi-surface-check
	$(PYTHON) scripts/verify-ffi-symbols.py --classify stable --validate > /dev/null

# Regen seam: re-records the exact unclassified-ownership count. Records a fall
# (ABI surface gaining contracts); refuses a rise, which is new unclassified
# surface and needs a deliberate decision.
ffi-ownership-ratchet-record:
	$(PYTHON) scripts/verify-ffi-symbols.py --classify stable --validate \
	  --write-ownership-ratchet > /dev/null

# Python only; no artifacts.

LINT_GATES += test-verify-ffi
test-verify-ffi:
	$(PYTHON) scripts/tests/test_verify_ffi_symbols.py

# Python only; no artifacts.

cabi-surface:
	$(PYTHON) scripts/generate-cabi-surface.py --write


cabi-surface-check:
	$(PYTHON) scripts/generate-cabi-surface.py --check

# Python only; no artifacts.

LINT_GATES += test-cabi-surface
test-cabi-surface:
	$(PYTHON) scripts/tests/test_cabi_surface.py

# Python only; no artifacts.

LINT_GATES += verify-sys-lane-closure
# ── System-lane closure ────────────────────────────────────────────────────
# docs/internal/jit-host-abi.md forbids any `stable` symbol from producing,
# installing, mutating, observing or destroying system-lane state. That is a
# property of the transitive CALL GRAPH, not of a symbol's own body: four
# hand-audits of the stable tier produced four different answers because each
# read the symbols one at a time and none of them followed the calls. This
# recomputes the closure from the lane operations outward and fails if a stable
# symbol can reach one. Run it with --list-roots or --explain SYM to see why.
verify-sys-lane-closure: test-sys-lane-closure
	$(PYTHON) scripts/sys-lane-closure.py

# Python only; no artifacts.

# Self-test for the checker above: proves it still fails on a transitive reach,
# that an authenticated edge clears only the caller it names, and that a stale
# or unreasoned waiver fails rather than silently widening the stable tier.
test-sys-lane-closure:
	$(PYTHON) scripts/tests/test_sys_lane_closure.py

# Python only; no artifacts.


# ── Install / Uninstall ────────────────────────────────────────────────────
# Installs release-built artifacts to $(DESTDIR)$(PREFIX).
# Run `make release` first; install fails closed if any required artifact is
# absent or a release binary cannot generate its completion scripts.

# Release-artefact preconditions for `install`. A macro rather than a target:
# it is only ever meaningful as the first step of `install`'s recipe.
define require_absolute_install_root
	@prefix="$(PREFIX)"; destdir="$(DESTDIR)"; \
	case "$$prefix" in \
		/*) ;; \
		*) echo "Error: PREFIX must be an absolute path (got '$$prefix')" >&2; exit 1 ;; \
	esac; \
	case "$$destdir" in \
		""|/*) ;; \
		*) echo "Error: DESTDIR must be empty or an absolute path (got '$$destdir')" >&2; exit 1 ;; \
	esac
endef

define require_release_artifacts
	@test -x "$(RELEASE_DIR)/hew" \
		|| { echo "Error: release hew not built. Run 'make release' first."; exit 1; }
	@test -x "$(RELEASE_DIR)/hew-lsp" \
		|| { echo "Error: release hew-lsp not built. Run 'make release' first."; exit 1; }
	@test -x "$(RELEASE_DIR)/hew-observe" \
		|| { echo "Error: release hew-observe not built. Run 'make release' first."; exit 1; }
	@test -f "$(RELEASE_LIB_DIR)/libhew.a" \
		|| { echo "Error: libhew.a not built. Run 'make release' first."; exit 1; }
	@test -f "$(WASM_RELEASE_DIR)/libhew_runtime.a" \
		|| { echo "Error: wasm runtime not built. Run 'make release' first."; exit 1; }
	@test -f "$(WASM_RELEASE_DIR)/libhew_std.a" \
		|| { echo "Error: wasm standard library not built. Run 'make release' first."; exit 1; }
endef

install: require-host-cargo-target
	$(call require_absolute_install_root)
	$(call require_release_artifacts)
	@echo "==> Installing to $(DESTDIR)$(PREFIX)"
	install -d "$(DESTDIR)$(PREFIX)/bin"
	install -d "$(DESTDIR)$(PREFIX)/lib"
	install -d "$(DESTDIR)$(PREFIX)/std"
	install -d "$(DESTDIR)$(PREFIX)/completions"
	install -m 755 "$(RELEASE_DIR)/hew"                "$(DESTDIR)$(PREFIX)/bin/hew"
	install -m 755 "$(RELEASE_DIR)/hew-lsp"            "$(DESTDIR)$(PREFIX)/bin/hew-lsp"
	install -m 755 "$(RELEASE_DIR)/hew-observe"        "$(DESTDIR)$(PREFIX)/bin/hew-observe"
	install -m 644 "$(RELEASE_LIB_DIR)/libhew.a"       "$(DESTDIR)$(PREFIX)/lib/libhew.a"
	@for lib in libhew_runtime.a libhew_std.a; do \
		if [ -f "$(WASM_RELEASE_DIR)/$$lib" ]; then \
			install -d "$(DESTDIR)$(PREFIX)/lib/wasm32-wasip1"; \
			install -m 644 "$(WASM_RELEASE_DIR)/$$lib" \
				"$(DESTDIR)$(PREFIX)/lib/wasm32-wasip1/$$lib"; \
		fi; \
	done
	@# Native per-triple lib subtree — mirrors assemble-release and gives
	@# find_hew_lib() its preferred lib/<triple>/libhew.a probe path.
	@for triple in $(NATIVE_LIB_TRIPLES); do \
		[ -n "$$triple" ] || continue; \
		lib_path=""; \
		if [ -f "$(CARGO_TARGET_ROOT)/$$triple/release-lib/libhew.a" ]; then \
			lib_path="$(CARGO_TARGET_ROOT)/$$triple/release-lib/libhew.a"; \
		elif [ "$$triple" = "$(HOST_TRIPLE)" ] && [ -f "$(RELEASE_LIB_DIR)/libhew.a" ]; then \
			lib_path="$(RELEASE_LIB_DIR)/libhew.a"; \
		else \
			continue; \
		fi; \
		install -d "$(DESTDIR)$(PREFIX)/lib/$$triple"; \
		install -m 644 "$$lib_path" "$(DESTDIR)$(PREFIX)/lib/$$triple/libhew.a"; \
	done
	cp -r std/. "$(DESTDIR)$(PREFIX)/std/"
	@set -e; for shell in bash zsh fish; do \
		"$(RELEASE_DIR)/hew" completions "$$shell" \
			> "$(DESTDIR)$(PREFIX)/completions/hew.$$shell"; \
		chmod 644 "$(DESTDIR)$(PREFIX)/completions/hew.$$shell"; \
	done
	@echo "==> Installed to $(DESTDIR)$(PREFIX)"
	@echo "    Add $(PREFIX)/bin to your PATH:"
	@echo "      export PATH=\"$(PREFIX)/bin:\$$PATH\""


uninstall:
	$(call require_absolute_install_root)
	@install_root="$(DESTDIR)$(PREFIX)"; \
	if [ -z "$$install_root" ]; then \
		echo "Error: refusing to uninstall unsafe path '$$install_root'" >&2; exit 1; \
	fi; \
	case "$$install_root" in \
		*[!/]*) ;; \
		*) echo "Error: refusing to uninstall unsafe path '$$install_root'" >&2; exit 1 ;; \
	esac; \
	if [ -d "$$install_root" ]; then \
		case "$$install_root" in /*) cd_target="$$install_root" ;; *) cd_target="./$$install_root" ;; esac; \
		canonical_root=$$(CDPATH= cd -P "$$cd_target" 2>/dev/null && pwd -P) \
			|| { echo "Error: cannot resolve uninstall path '$$install_root'" >&2; exit 1; }; \
		if [ "$$canonical_root" = "/" ]; then \
			echo "Error: refusing to uninstall unsafe path '$$install_root'" >&2; exit 1; \
		fi; \
	fi; \
	rm -rf -- "$$install_root"
	@echo "==> Removed $(DESTDIR)$(PREFIX)"

# ── Cleanup ─────────────────────────────────────────────────────────────────

clean: ## Develop: remove generated build and test artifacts
	rm -rf -- $(BUILD_DIR)
	cargo clean
	rm -rf -- $(COV_DIR) \
		"$(CURDIR)/.tmp/compile-out" \
		"$(CURDIR)/.tmp/doc-fences" \
		"$(CURDIR)/.tmp/core-matrix-regen" \
		"$(CURDIR)/.tmp/forced-cancel-gate-out" \
		"$(CURDIR)/.tmp/asan-fixture-out" \
		"$(CURDIR)/.tmp/tool-tmp"
	rm -f -- \
		"$(CURDIR)/.tmp/vertical-slice-accept-output.txt" \
		"$(CURDIR)/.tmp/vertical-slice-reject-output.txt" \
		"$(CURDIR)/.tmp/vertical-slice.stdout" \
		"$(CURDIR)/.tmp/vertical-slice.stderr" \
		"$(CURDIR)/.tmp/vertical-slice-remote-pid-old-verb.hew" \
		"$(CURDIR)/.tmp/pkg-import-actual.txt" \
		"$(CURDIR)/.tmp/scanner-test-input.txt" \
		"$(CURDIR)/.tmp/stdlib-io-scanner-oracle-input.txt"
