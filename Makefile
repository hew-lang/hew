# ============================================================================
# Hew Developer Makefile
#
# Builds all project artifacts into build/ with a predictable layout:
#
#   build/
#     bin/hew              — compiler driver + package manager (Rust)
#     bin/hew-observe      — TUI actor observer (Rust)
#     bin/hew-lsp          — language server (Rust)
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
#   make hew          — alias for hew-native (a driver-only build cannot link)
#   make hew-native   — compiler driver + native libhew archive for `hew build`
#   make observe      — just the TUI observer (hew-observe)
#   make observe-functional-test — HTTP-backed functional observe harness
#   make mqtt-broker-e2e       — real MQTT broker publish/delivery oracle
#   make libhew-link-race-test   — real multi-process libhew.a bootstrap-race proof
#   make runtime      — just libhew_runtime.a
#   make stdlib       — all stdlib packages + combine into libhew.a
#   make wasm-runtime — WASM runtime + wire JSON/YAML/TOML archives
#   make wasm         — build hew-wasm (browser WASM via wasm-pack)
#   make wasm-capability           — regenerate manifest-owned Rust/JSON/docs outputs
#   make wasm-capability-check     — verify manifest-owned generated outputs
#   make playground-manifest       — regenerate examples/playground/manifest.json
#   make playground-manifest-check — verify examples/playground/manifest.json freshness
#   make sandbox-fixtures          — regenerate sandbox VM bytecode fixtures from main.hew
#   make sandbox-fixtures-check    — verify sandbox VM bytecode fixtures are fresh
#   make sandbox-vm-deps           — install hew-sandbox-vm npm deps (hash-stamped, idempotent)
#   make sandbox-parity            — native hew run ↔ sandbox VM parity harness
#   make playground-check          — manifest freshness + full hew-wasm test suite + build hew-wasm
#   make playground-wasi-check     — focused curated manifest WASI runtime preflight
#   make licenses                  — regenerate THIRD-PARTY-LICENSES from current Cargo.lock
#   make licenses-check            — verify THIRD-PARTY-LICENSES is current (used in CI)
#   make check-gate-reachability   — verify every gate target/crate/exclusion is reached by CI,
#                                    and every documented make target exists
#   make preflight                 — STANDARD per-branch gate: dispatcher-routed checks for the
#                                    current diff, fail-fast; hosted CI remains the backstop
#   make ci-preflight              — run-all dispatcher preflight; for integration/release
#                                    moments (merge trains, RC cuts), not routine branch gating
#   make ci-preflight-smoke        — fast smoke tier: fmt + in-process tests (<5 min)
#   make ci-preflight-strict       — run the local preflight superset that mirrors merge-queue gates
#   make wasm-dist    — build + copy WASM to hew.sh and hew.run
#   make test         — Rust workspace tests
#   make macos-leak-oracle — ratcheted local leaks(1) + poisoned-allocator corpus
#   make test-leak-oracle-selftest — fail-closed leak runner/harness counterfactuals
#   make test-cabi         — C-ABI crate tests (narrow; excluded from the workspace run)
#   make test-compiler-pipeline — compiler ladder + CLI pipeline tests (narrow)
#   make test-vertical-slice — end-to-end Hew compiler oracle
#   make test-package-install — hew install -> Hew import consumer proof
#   make test-runtime-unit — hew-runtime tests without heavy QUIC/TLS/profiler stack (~3× faster)
#   make test-stdlib-execution-proofs — verify every README-indexed public stdlib module has an executed API proof
#   make test-ux-examples  — run examples/ux + examples/progressive tutorials against .expected files
#   make asan         — run the nightly rust-runtime ASan test command locally
#   make tsan         — run the nightly rust-runtime TSan test command locally
#   make miri         — run the curated rust-runtime Miri allowlist locally
#   make lint         — cargo clippy (workspace + tests, warnings are errors) + hew fmt gate
#   make structural-lint — pinned ast-grep scan + compiler authority ratchets
#   make hew-fmt-check — check that std/ and examples/ .hew files are formatted (part of lint)
#   make leak-scan    — scan tracked source for orchestration-token leaks (lane IDs, Q-tags, .tmp/ paths)
#   make fuzz-corpus    — regenerate ignored cargo-fuzz corpora from current fixtures/examples
#   make clean        — remove build/, target/
# ============================================================================

.PHONY: all build bootstrap install-hooks hew hew-native hew-lsp observe observe-functional-test mqtt-broker-e2e libhew-link-race-test runtime stdlib wasm-runtime wasm wasm-capability wasm-capability-check playground-manifest playground-manifest-check sandbox-fixtures sandbox-fixtures-check sandbox-vm-deps sandbox-parity playground-check playground-wasi-check preflight ci-preflight ci-preflight-smoke ci-preflight-strict ci-local-linux wasm-dist release check-libhew-fresh licenses licenses-check
.PHONY: test macos-leak-oracle test-leak-oracle-selftest test-cabi test-compiler-pipeline test-compiler-lifecycle test-opaque-resource-lifecycle-matrix test-opaque-resource-lifecycle-matrix-external test-vertical-slice test-pkg-import test-package-install test-runtime-unit test-hew-ratchet test-core-matrix test-o2-differential o2-differential-selftest test-stdlib-ratchet test-stdlib-execution-proofs test-ux-examples test-surface-examples test-example-expectations-selftest test-release-binary test-release-lib-link test-release-workflow-contract check-sanitizer-gate asan asan-fixtures test-asan-fixture-selftest tsan miri lint lint-ci-coverage-check structural-lint structural-lint-bootstrap structural-lint-bootstrap-install test-structural-authority-audit test-ast-grep-contract test-structural-lint-bootstrap runtime-poison-safe-lint stdlib-lint stdlib-errno-gate lint-wasm-todo lint-wasm-todo-self-test leak-scan hew-fmt-check test-migrate-corpus check-gate-reachability test-check-gate-reachability sandbox-parity-coverage-check test-sandbox-parity-coverage-check doc-ratchet-selftest freebsd-workflow-contract-check tool-pin-contract-check verify-sys-lane-closure test-sys-lane-closure hew-fmt-property
.PHONY: clean install uninstall verify-ffi test-verify-ffi test-python310-toml-compat
.PHONY: assemble assemble-release pre-release windows-release-candidate publish-docs
.PHONY: coverage coverage-summary coverage-lcov coverage-runtime coverage-combined coverage-branch
.PHONY: fuzz-corpus fuzz-oracle fuzz-oracle-selftest fuzz-smoke fuzz-smoke-bootstrap-install
.PHONY: ll-diff ll-golden ll-identity-selftest
.PHONY: checked-mir-verify checked-mir-golden checked-mir-run checked-mir-expect
.PHONY: hew-check-all

# ── Configuration ───────────────────────────────────────────────────────────

# Installation prefix (used by `make install`)
PREFIX     ?= /usr/local/hew
DESTDIR    ?=

# Output directory — all usable artifacts land here as symlinks
BUILD_DIR  := build
COMMON_GIT_DIR := $(shell git rev-parse --git-common-dir 2>/dev/null)

# Cargo profile directory names.
#
# Cargo does not always write into `target/`: CARGO_TARGET_DIR, build.target-dir
# in any .cargo/config.toml, CARGO_BUILD_TARGET, build.target and an explicit
# --target each move the output directory. A rule that builds through Cargo and
# then touches, inspects or installs a hard-coded `target/debug` is looking at a
# different file than the one Cargo just wrote — which is precisely how a
# month-old libhew.a in a shared scratch target directory got certified fresh.
# scripts/cargo-output-dir.py resolves the real directory the way Cargo does,
# and everything below is derived from it.
#
# TARGET_TRIPLE passes --target through to the native cargo invocations here;
# leave it empty to build for the host.
TARGET_TRIPLE ?=
CARGO_TARGET_FLAG := $(if $(TARGET_TRIPLE),--target $(TARGET_TRIPLE),)
CARGO_TARGET_ROOT := $(shell scripts/cargo-output-dir.py --root)
CARGO_NATIVE_OUT := $(shell scripts/cargo-output-dir.py --native $(CARGO_TARGET_FLAG))
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
RELEASE_HEW := $(RELEASE_DIR)/hew.exe
RELEASE_LIBHEW := $(RELEASE_LIB_DIR)/hew.lib
else
RELEASE_HEW := $(RELEASE_DIR)/hew
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

# Sources that feed the archive. The list is derived, never hand-listed:
# hew-lib's non-dev path-dependency closure, its Rust sources and manifests,
# the embedded assets its code names with include_str!/include_bytes!, and the
# workspace manifest and lockfile.  The build graph conservatively reruns
# Cargo for any lockfile edit; the final content certificate narrows that to
# Cargo's relevant lock closure.  Deriving it is the point: a hand-written list
# is how an input that changes the archive ends up not counting toward freshness.
LIBHEW_INPUTS_SCRIPT := scripts/libhew-inputs.py
LIBHEW_FRESHNESS_SCRIPT := scripts/libhew-freshness.py
LIBHEW_SRC_DIRS := $(shell $(LIBHEW_INPUTS_SCRIPT) crates)
LIBHEW_SRCS := $(shell $(LIBHEW_INPUTS_SCRIPT) files)
ifeq ($(strip $(LIBHEW_SRCS)),)
$(error $(LIBHEW_INPUTS_SCRIPT) produced no inputs for $(LIBHEW_NAME); refusing to \
treat the archive as having no sources. Run '$(LIBHEW_INPUTS_SCRIPT) files' to see why)
endif

# Prerequisite bundle for every target that LINKS a native Hew program.
# Cargo performs the incremental rebuild through `libhew-debug`; the order-only
# freshness oracle then re-asserts the archive at the point of use.
LIBHEW_READY := libhew-debug | check-libhew-fresh

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
#
# Default to the host triple so `make asan` works on any sanitizer-capable
# host (darwin-arm64, linux-x86_64, ...).  Nightly CI invokes `cargo +nightly
# test --target x86_64-unknown-linux-gnu` directly rather than via `make
# asan`, so changing this default does not affect the CI lane.
SANITIZER_RUST_TARGET ?= $(HOST_TRIPLE)
RUNTIME_ASAN_TARGET_DIR := target/sanitizer-runtime-asan
RUNTIME_TSAN_TARGET_DIR := target/sanitizer-runtime-tsan
RUNTIME_MIRI_TARGET_DIR := target/miri-runtime

# ── Default target ──────────────────────────────────────────────────────────

all: hew-native hew-lsp observe runtime stdlib wasm-runtime assemble

# Convenience alias — rebuilds all debug artifacts including libhew.a.
# Equivalent to `make all`; exists so that `make build` behaves as expected.
build: all

# ── Rust targets ────────────────────────────────────────────────────────────

# `hew` used to run `cargo build -p hew-cli`, which produces the compiler
# DRIVER only. The link step for a compiled program also needs hew-lib's
# staticlib beside that driver, so the target handed out a compiler that could
# not link a single program — and the resulting undefined-symbol wall reads
# like a compiler bug. There is no legitimate use for that pairing, so `hew`
# is now exactly `hew-native`. If you truly want the driver alone (to check
# that it compiles, say), run `cargo build -p hew-cli` and own the consequence.
hew: hew-native

# The gate is itself an artifact build; warming it is building it.
hew-build: hew
	@:

# Build the native artifacts required for `hew build` from a source checkout:
# the driver plus hew-lib's staticlib (`target/debug/libhew.a` on Unix,
# `target/debug/hew.lib` on Windows). Keep this target cross-platform so fresh
# Windows hosts use the same build graph as Linux/macOS.
hew-native: libhew-debug
	cargo build -p hew-cli $(CARGO_TARGET_FLAG)

# The gate is itself an artifact build; warming it is building it.
hew-native-build: hew-native
	@:

# Build the language server (debug).
hew-lsp:
	cargo build -p hew-lsp $(CARGO_TARGET_FLAG)

# Build the TUI actor observer (debug).
# hew-observe is a sibling binary: `hew observe` delegates to it when it is
# present next to the running hew binary or on PATH (see exec_sibling_binary).
observe:
	cargo build -p hew-observe $(CARGO_TARGET_FLAG)

observe-functional-test: hew-native observe $(LIBHEW_READY)
	cargo test -p hew-observe --test functional -- --ignored --nocapture

# Warm-up form for the preflight dispatcher, which derives it by name.
observe-functional-test-build: hew-native observe $(LIBHEW_READY)
	cargo test -p hew-observe --test functional --no-run

# Opt-in real-client proof for the advertised pure-Hew MQTT broker. Mosquitto
# clients are an explicit external prerequisite, so this is not folded into the
# hermetic workspace test lane.
mqtt-broker-e2e: hew-native $(LIBHEW_READY)
	HEW_BIN="$(DEBUG_DIR)/hew" scripts/mqtt-broker-e2e.sh

# Warm-up form for the preflight dispatcher, which derives it by name.
mqtt-broker-e2e-build: hew-native $(LIBHEW_READY)
	@:

# Real multi-process proof that `hew_testutil::ensure_hew_lib_built` closes
# the `libhew.a` uplift race: real `cargo build -p hew-lib` writers, a real
# `hew compile` link, and a real shared NEXTEST_RUN_ID across OS processes.
# Excluded from routine `cargo nextest run` (see the #[ignore] reasons in
# hew-testutil/tests/libhew_link_race.rs) because it repeatedly shells real
# cargo/hew subprocesses; run explicitly here instead, same convention as
# observe-functional-test above.
libhew-link-race-test: hew-native $(LIBHEW_READY)
	cargo test -p hew-testutil --test libhew_link_race -- --ignored --nocapture --test-threads=1

# Warm-up form for the preflight dispatcher, which derives it by name.
libhew-link-race-test-build: hew-native $(LIBHEW_READY)
	cargo test -p hew-testutil --test libhew_link_race --no-run

# Build the runtime static library (debug)
runtime:
	cargo build -p hew-runtime $(CARGO_TARGET_FLAG)

# The gate is itself an artifact build; warming it is building it.
runtime-build: runtime
	@:

# Build libhew.a — the combined runtime + stdlib static library.
# The hew-lib umbrella crate depends on hew-runtime + all stdlib crates;
# Cargo produces a single deduplicated staticlib.
#
# `stdlib` is the human-facing alias.
stdlib: libhew-debug

# The gate is itself an artifact build; warming it is building it.
stdlib-build: stdlib
	@:

# Cargo owns freshness for its configurable output tree. This target remains
# phony deliberately: a fixed Make stamp cannot distinguish two different
# CARGO_TARGET_DIR/build.target/build.target-dir selections without putting the
# possibly space-bearing output path back into Make's target graph.
.PHONY: libhew-debug
libhew-debug: $(LIBHEW_SRCS)
	$(LIBHEW_FRESHNESS_SCRIPT) build --debug-dir "$(DEBUG_DIR)" -- cargo build -p hew-lib $(CARGO_TARGET_FLAG)

# Build the WASM runtime + the consolidated stdlib archive (libhew_std.a).
#
# Keep the Cargo output filenames out of Make's target graph for the same
# spacious-target-directory reason as libhew-debug. Cargo's own incremental
# graph makes repeated invocations cheap and authoritative.
.PHONY: wasm-runtime-debug wasm-std-debug
wasm-runtime-debug: $(LIBHEW_SRCS)
	cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features

wasm-std-debug: $(LIBHEW_SRCS)
	cargo build -p hew-std --target wasm32-wasip1

wasm-runtime: wasm-runtime-debug wasm-std-debug

# The gate is itself an artifact build; warming it is building it.
wasm-runtime-build: wasm-runtime
	@:

# Build the hew-wasm browser analysis-only module (requires: cargo install wasm-pack)
wasm:
	wasm-pack build hew-wasm --target web --release

# Regenerate the typed WASM capability consumers.
wasm-capability:
	cargo run -p hew-capability-gen

# Verify the generated checker, playground, and matrix consumers are current.
wasm-capability-check:
	cargo run -p hew-capability-gen -- --check

# Build the generator the check runs.
wasm-capability-check-build:
	cargo build -p hew-capability-gen

# Regenerate the curated playground manifest consumed by downstream browser tooling.
playground-manifest: wasm-capability
	python3 scripts/gen-playground-manifest.py

# Verify the checked-in playground manifest is current.
playground-manifest-check: wasm-capability-check
	python3 scripts/gen-playground-manifest.py --check

# Build the generator the manifest check runs.
playground-manifest-check-build:
	cargo build -p hew-capability-gen

sandbox-fixtures:
	cargo run -p xtask -- sandbox-fixtures

sandbox-fixtures-check:
	cargo run -p xtask -- sandbox-fixtures --check

# The check runs xtask; warming it builds that binary.
sandbox-fixtures-check-build:
	cargo build -p xtask

# Regenerate THIRD-PARTY-LICENSES from the current dependency tree.
# Requires cargo-about: cargo install cargo-about --locked
licenses:
	cargo about generate about.hbs --workspace > THIRD-PARTY-LICENSES

# Verify THIRD-PARTY-LICENSES is current relative to Cargo.lock and about.hbs.
# Exits non-zero if the file is stale; run 'make licenses' to regenerate.
licenses-check:
	scripts/check-licenses-fresh.sh

# Install hew-sandbox-vm's npm dependencies, skipping the install when
# node_modules already matches package-lock.json (hash-stamped). Split out
# from sandbox-parity as its own reusable prerequisite.
sandbox-vm-deps:
	@set -e; \
	lock_hash=$$(python3 -c 'import hashlib, pathlib; print(hashlib.sha256(pathlib.Path("hew-sandbox-vm/package-lock.json").read_bytes()).hexdigest())'); \
	stamp=hew-sandbox-vm/node_modules/.package-lock.sha256; \
	if [ ! -d hew-sandbox-vm/node_modules ] || [ ! -f "$$stamp" ] || [ "$$lock_hash" != "$$(cat "$$stamp")" ]; then \
		echo "npm --prefix hew-sandbox-vm ci"; \
		npm --prefix hew-sandbox-vm ci; \
		printf '%s\n' "$$lock_hash" > "$$stamp"; \
	else \
		echo "hew-sandbox-vm dependencies are fresh; skipping install"; \
	fi

# Native hew run <-> sandbox VM parity harness. All four VM-dependent test
# binaries (parity, parity_ratchet, playground, ios_subset) are excluded
# WHOLE from the generic nextest default-filter (.config/nextest.toml) on
# every platform, because each contains at least one function that spawns
# the hew-sandbox-vm Node runner -- this is a binary-level exclusion, not a
# per-test one, so parity_ratchet's non-VM structural ratchet tests also
# run only here now (see scripts/check-sandbox-parity-coverage.py for why
# per-test attribution inside a VM-touching binary is not trusted). This
# target is the one place that provisions the npm toolchain and then runs
# every test in all four binaries via plain `cargo test`, so nothing is
# silently skipped anywhere.
sandbox-parity: hew-native sandbox-vm-deps $(LIBHEW_READY)
	npm --prefix hew-sandbox-vm run conformance
	cargo test -p hew-sandbox-wasm --test parity --test parity_ratchet --test playground --test ios_subset

# Warm-up form for the preflight dispatcher, which derives it by name.
sandbox-parity-build: hew-native sandbox-vm-deps $(LIBHEW_READY)
	cargo test -p hew-sandbox-wasm --test parity --test parity_ratchet --test playground --test ios_subset --no-run

# Repo-local browser/tooling smoke:
# manifest freshness + full hew-wasm test suite (lib + integration) + analysis-only WASM build.
# Running full `cargo test -p hew-wasm` subsumes the --lib curated-manifest smoke and compiles
# and runs tests/v05_wasm_coverage.rs (the fixture-coverage integration suite).
playground-check: playground-manifest-check
	cargo test -p hew-wasm
	$(MAKE) wasm

# Build this target's test binaries; `make wasm` is left to the gate.
playground-check-build: playground-manifest-check
	cargo test -p hew-wasm --no-run

# Focused curated playground WASI runtime preflight.
playground-wasi-check:
	cargo test -p hew-cli --test wasi_run_e2e curated_playground_examples_run_under_wasi -- --exact
	cargo test -p hew-cli --test wasi_run_e2e supervisor_stays_on_the_unsupported_diagnostic_path_under_wasi -- --exact

# Standard per-branch gate: the dispatcher classifies the current diff and
# runs the narrowest sufficient checks for its file classes, stopping at the
# first failure for quick iteration.  This is THE routine gate before pushing a
# branch — hosted CI is the backstop for cross-cutting fallout the routing cannot
# foresee.  Escape classes observed while gating by hand-picked per-crate tests
# (structural ratchets outside the cargo dependency graph, cross-crate exec
# fallout from resolver changes, ll-oracle golden drift from emission reorders)
# are routed by the dispatcher itself, so use this target rather than an ad-hoc
# test selection.
# Usage: make preflight            (classify + run, fail-fast)
#        make preflight ARGS="--dry-run"   (print the routing only)
preflight:
	scripts/ci-preflight-dispatcher.sh --fail-fast $(ARGS)

# Conservative diff-based local preflight dispatcher, run-all failure policy.
# Reserve for integration/release moments (merge trains, RC cuts, post-squash
# re-verification) where the complete failure report is worth the wall clock;
# routine branch gating uses make preflight above.
# Usage: make ci-preflight ARGS="--dry-run" or ARGS="--base origin/main"
ci-preflight:
	scripts/ci-preflight-dispatcher.sh $(ARGS)

# Fast smoke preflight: Rust fmt + the workspace's deterministic in-process
# tests (nextest smoke profile). Designed to complete in <5 min and surface
# format and fast oracle failures during local iteration. Clippy remains in
# the lint target and is not duplicated here.
#
# Run this target directly for a quick sanity pass on any diff without waiting
# for E2E compilation. The comprehensive dispatcher reserves it for that local
# opt-in because its full workspace run already includes the smoke test.
#
# The smoke nextest profile excludes subprocess-intensive tests (eval_e2e,
# test_runner_e2e, parity) and hew-wasm; see .config/nextest.toml [profile.smoke].
#
# Build-graph note: cargo clippy and cargo nextest both compile the hew-cli
# library, so `make hew-native` after them only pays for the final link step
# (~1–2 s on a warm tree).  Some nextest smoke tests execute `hew run`, which
# links against the combined archive; bring it up to date AND assert its
# freshness before nextest so a fresh checkout does not fail smoke with
# "cannot find libhew.a" and a carried-over target dir cannot feed a stale one
# to the tests. Running `make hew-native` here also eliminates the redundant
# compile triggered by make lint → hew-fmt-check later in that same run
# (hew-fmt-check requires target/debug/hew but nextest does not produce it).
ci-preflight-smoke:
	cargo fmt --all -- --check
	$(MAKE) check-libhew-fresh
	cargo nextest run --workspace --profile smoke
	$(MAKE) hew-native

# Assert that libhew.a matches the content-addressed certificate written only
# after Cargo successfully built (or fingerprint-verified) it.  The certificate
# binds the archive bytes to hew-lib's semantic input closure, so lockfile mtime
# noise cannot contradict Cargo while a real source or relevant lock change
# remains fail-closed.  It is wired as an ORDER-ONLY prerequisite of every
# target that links a native Hew program (see $(LIBHEW_READY)).
check-libhew-fresh: libhew-debug
	scripts/check-libhew-fresh.sh --debug-dir "$(DEBUG_DIR)"

# The freshness check reads libhew; warming it builds that archive.
check-libhew-fresh-build: libhew-debug
	@:

# Opt-in merge-queue parity preflight.
ci-preflight-strict:
	cargo fmt --all -- --check
	cargo clippy --workspace --tests -- -D warnings
	$(MAKE) playground-check
	$(MAKE) test
	$(MAKE) stdlib-lint

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
fuzz-oracle: hew-native runtime $(LIBHEW_READY)
	@if [ -n "$(FUZZ_ORACLE_FULL)" ]; then \
		python3 scripts/fuzz/run-oracle.py --hew "$(DEBUG_DIR)/hew" --full --timeout 30; \
	else \
		python3 scripts/fuzz/run-oracle.py --hew "$(DEBUG_DIR)/hew" --timeout 30; \
	fi

# Warm-up form for the preflight dispatcher, which derives it by name.
fuzz-oracle-build: hew-native runtime $(LIBHEW_READY)
	@:

# Oracle self-tests: four independently-failable checks that prove the
# harness has teeth (flags real crashes), honours the ratchet contract
# (unexpected-pass and unexpected-fail both fail closed), and refuses to
# report PASS over a candidate set below its floor.
fuzz-oracle-selftest: hew-native runtime $(LIBHEW_READY)
	HEW_BIN="$(DEBUG_DIR)/hew" bash scripts/fuzz/oracle-selftest.sh

# Warm-up form for the preflight dispatcher, which derives it by name.
fuzz-oracle-selftest-build: hew-native runtime $(LIBHEW_READY)
	@:

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
	leak_scan_link_target="../../../scripts/pre-push-leak-scan.sh"; \
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
	if [ -L "$$pre_push_dir/leak-scan" ] && [ "$$(readlink "$$pre_push_dir/leak-scan")" = "$$leak_scan_link_target" ]; then \
		skipped_links="$$skipped_links\n  - $$pre_push_dir/leak-scan -> $$leak_scan_link_target"; \
	else \
		ln -sfn "$$leak_scan_link_target" "$$pre_push_dir/leak-scan"; \
		wrote_links="$$wrote_links\n  - $$pre_push_dir/leak-scan -> $$leak_scan_link_target"; \
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

# Create symlinks from build/ into the real output locations.
# This gives you one stable directory to point PATH at during development.
assemble: | hew-native hew-lsp observe runtime stdlib wasm-runtime
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib
	@# assemble-release makes build/std a symlink to ../std; reset it so the
	@# flat std stub loop below cannot rewrite tracked std/*.hew files in root.
	@rm -rf $(BUILD_DIR)/std
	@mkdir -p $(BUILD_DIR)/std
	@# Compiler driver
	@ln -sfn "$(LINK_UP2)$(DEBUG_DIR)/hew"                "$(BUILD_DIR)/bin/hew"
	@# Language server
	@ln -sfn "$(LINK_UP2)$(DEBUG_DIR)/hew-lsp"            "$(BUILD_DIR)/bin/hew-lsp"
	@# TUI actor observer (sibling binary — `hew observe` delegates here)
	@ln -sfn "$(LINK_UP2)$(DEBUG_DIR)/hew-observe"        "$(BUILD_DIR)/bin/hew-observe"
	@# Combined Hew library (runtime + all stdlib packages)
	@ln -sfn "$(LINK_UP2)$(DEBUG_DIR)/libhew.a"           "$(BUILD_DIR)/lib/libhew.a"
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
		if [ -f "$(CARGO_TARGET_ROOT)/$$triple/debug/libhew.a" ]; then \
			lib_path="$(CARGO_TARGET_ROOT)/$$triple/debug/libhew.a"; \
		elif [ "$$triple" = "$(HOST_TRIPLE)" ] && [ -f "$(DEBUG_DIR)/libhew.a" ]; then \
			lib_path="$(DEBUG_DIR)/libhew.a"; \
		else \
			continue; \
		fi; \
		mkdir -p $(BUILD_DIR)/lib/$$triple; \
		ln -sfn "$(LINK_UP3)$$lib_path" "$(BUILD_DIR)/lib/$$triple/libhew.a"; \
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
  RELEASE_PREP = cargo clean --profile release && cargo clean --profile release-lib
  RELEASE_ENV = MACOSX_DEPLOYMENT_TARGET=13.0
endif

release:
	$(RELEASE_PREP)
	$(RELEASE_ENV) cargo build -p hew-cli --release $(CARGO_TARGET_FLAG)
	$(RELEASE_ENV) cargo build -p hew-lsp --release $(CARGO_TARGET_FLAG)
	$(RELEASE_ENV) cargo build -p hew-observe --release $(CARGO_TARGET_FLAG)
	$(RELEASE_ENV) cargo build -p hew-lib --profile release-lib $(CARGO_TARGET_FLAG)
	$(RELEASE_ENV) cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features --release
	$(RELEASE_ENV) cargo build -p hew-std --target wasm32-wasip1 --release
	$(MAKE) assemble-release

# Validate release builds on all supported platforms before tagging.
# Runs linux locally first (fail-fast), then remote platforms in parallel.
#   make pre-release                    — all platforms
#   make pre-release PLATFORMS="linux"  — linux only
pre-release: release
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
publish-docs: ## Build stdlib docs; print wrangler deploy command for hew-docs
	@test -f "$(RELEASE_DIR)/hew" \
		|| { echo "Error: release hew not built. Run 'make release' first."; exit 1; }
	"$(RELEASE_DIR)/hew" doc std/ --output-dir "$(CARGO_TARGET_ROOT)/doc/"
	@echo ""
	@echo "Docs generated at $(CARGO_TARGET_ROOT)/doc/."
	@echo "Deploy with: wrangler pages deploy $(CARGO_TARGET_ROOT)/doc/ --project-name hew-docs"

# Prove the shipped archive can link a real Rust staticlib through the public
# `hew build --link-lib` interface. Rust controls archive member names, so the
# behavioural consumer proof is more stable than inspecting `ar t` output.
test-release-lib-link:
ifeq ($(OS),Windows_NT)
	@powershell -NoProfile -ExecutionPolicy Bypass -File "$(CURDIR)/scripts/test-release-lib-link.ps1" -Hew "$(RELEASE_HEW)" -Archive "$(RELEASE_LIBHEW)"
else
	@"$(CURDIR)/scripts/test-release-lib-link.sh" --hew "$(RELEASE_HEW)" --archive "$(RELEASE_LIBHEW)"
endif

# Assemble build/ with release symlinks.
assemble-release:
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib $(BUILD_DIR)/std
	@ln -sfn "$(LINK_UP2)$(RELEASE_DIR)/hew"              "$(BUILD_DIR)/bin/hew"
	@ln -sfn "$(LINK_UP2)$(RELEASE_DIR)/hew-lsp"          "$(BUILD_DIR)/bin/hew-lsp"
	@ln -sfn "$(LINK_UP2)$(RELEASE_DIR)/hew-observe"      "$(BUILD_DIR)/bin/hew-observe"
	@# Combined Hew library (runtime + all stdlib packages), from the non-LTO
	@# release-lib profile — never the fat-LTO target/release archive.
	@$(MAKE) test-release-lib-link
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
test: wasm-runtime runtime $(LIBHEW_READY)
	@if command -v cargo-nextest >/dev/null 2>&1 || cargo nextest --version >/dev/null 2>&1; then \
		set -e; \
		cargo nextest run --workspace --exclude hew-cabi --profile ci --no-run; \
		test -f "$(LIBHEW)"; \
		cargo nextest run --workspace --exclude hew-cabi --profile ci --no-fail-fast; \
	else \
		echo "WARNING: cargo-nextest not installed — per-test timeouts are not enforced." >&2; \
		echo "         Install with: cargo install cargo-nextest" >&2; \
		cargo test --workspace --exclude hew-cabi --no-fail-fast; \
	fi

# Build this target's binaries the way it builds them, without running them.
test-build: wasm-runtime runtime $(LIBHEW_READY)
	cargo nextest run --workspace --exclude hew-cabi --profile ci --no-run

# Canonical local macOS memory authority. This is deliberately named as a local
# authority, not a CI `test-*` gate: hosted macOS processes cannot grant
# leaks(1) the task port it needs. The runner rejects a non-Darwin host,
# a missing leaks(1), an empty/shrunken inventory, any unexpected selected
# binary, and the absence of ffi_link_e2e's real allocator slope probe. It runs
# ignored tests too, so a newly ignored memory verdict cannot disappear behind
# a green nextest summary.
macos-leak-oracle: test-leak-oracle-selftest hew-native $(LIBHEW_READY)
	scripts/macos-leak-oracle.sh

# Platform-independent teeth for the leak harness and the runner's inventory
# contract. The Rust counterfactuals inject missing/declined/malformed/timed-out
# inspector commands and incomplete work witnesses; the shell counterfactuals
# prove empty/shrunken inventories and a missing ffi authority are red.
test-leak-oracle-selftest:
	cargo nextest run --profile ci -p hew-cli --test leak_harness_fail_closed
	scripts/tests/test_macos_leak_oracle_runner.sh

# The shell counterfactual needs no build; the Rust one does.
test-leak-oracle-selftest-build:
	cargo nextest run --profile ci -p hew-cli --test leak_harness_fail_closed --no-run

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
	cargo nextest run --profile ci-cabi -p hew-cabi

# Build test-cabi's binaries without running them. The preflight warm-up
# calls this instead of spelling the nextest invocation out, so the profile
# name lives in exactly one place: scripts/check-gate-reachability.py A3a
# scans the dispatcher's own dry-run output for `--profile <fast-tier>` and
# a second literal there would read as CI running a non-`ci` profile.
test-cabi-build:
	cargo nextest run --profile ci-cabi -p hew-cabi --no-run

# Build the combined runtime+stdlib static lib and the WASM runtime before
# running the compiler-pipeline tests.  Several hew-cli integration tests
# (eval_e2e, eval_wasm_*) call `hew eval` which needs both libs at link time.
# Without this prerequisite the lazy per-test build of libhew.a (~18 s on a
# cold worktree) consumes most of the default 30 s `hew eval --timeout` budget,
# causing spurious timeouts under the concurrent nextest run.  The WASM runtime
# (libhew_runtime.a for wasm32-wasip1) is needed by wasm32-wasi eval tests
# even when they are expected to fail before codegen (the linker search runs
# before the fast typecheck path reports its diagnostic).
test-compiler-pipeline: wasm-runtime hew-native $(LIBHEW_READY)
	cargo nextest run --profile ci \
		-p hew-lexer \
		-p hew-parser \
		-p hew-types \
		-p hew-hir \
		-p hew-mir \
		-p hew-codegen-rs \
		-p hew-cli \
		-p hew-pkg
	$(MAKE) test-compiler-lifecycle

# Build this target's binaries the way it builds them, without running them.
test-compiler-pipeline-build: wasm-runtime hew-native $(LIBHEW_READY)
	cargo nextest run --profile ci --no-run \
		-p hew-lexer \
		-p hew-parser \
		-p hew-types \
		-p hew-hir \
		-p hew-mir \
		-p hew-codegen-rs \
		-p hew-cli \
		-p hew-pkg
	$(MAKE) test-opaque-resource-lifecycle-matrix-build

# The compiled-Hew lifecycle evidence is separate so CI jobs that already ran
# workspace nextest can retain this evidence without replaying its Rust tests.
test-compiler-lifecycle: test-opaque-resource-lifecycle-matrix

# Both lifecycle targets read the pinned ast-grep at
# .ast-grep/tool/bin/ast-grep and abort when it is absent. The toolchain is
# provisioned at the job level (.github/actions/setup-ast-grep, the same
# cache-then-verify shape as setup-llvm and the wasmtime install), not as a
# make prerequisite: `structural-lint-bootstrap-install` cargo-installs
# tree-sitter-cli and ast-grep and then runs a full authority scan, which is
# minutes of work that has no place inside a test target invoked from three
# other targets. Locally, any `make lint` provisions the same tree.
test-opaque-resource-lifecycle-matrix: wasm-runtime hew-native
	HEW_BIN="$(DEBUG_DIR)/hew" python3 scripts/tests/test_opaque_resource_lifecycle_facts.py
	HEW_BIN="$(DEBUG_DIR)/hew" python3 scripts/tests/test_opaque_resource_lifecycle_matrix.py

# Warm-up form for the preflight dispatcher, which derives it by name.
test-opaque-resource-lifecycle-matrix-build: wasm-runtime hew-native
	@:

test-opaque-resource-lifecycle-matrix-external: wasm-runtime hew-native
	HEW_BIN="$(DEBUG_DIR)/hew" python3 scripts/tests/test_opaque_resource_lifecycle_facts.py
	HEW_BIN="$(DEBUG_DIR)/hew" python3 scripts/tests/test_opaque_resource_lifecycle_matrix.py --runtime-profile external-network

# Warm-up form for the preflight dispatcher, which derives it by name.
test-opaque-resource-lifecycle-matrix-external-build: wasm-runtime hew-native
	@:

# End-to-end Hew compiler oracle: real .hew fixtures through check/compile/run.
# Build libhew first and verify freshness so native fixture links do not test
# against stale runtime/stdlib archives on a fresh checkout or CI runner.
test-vertical-slice: hew-native runtime $(LIBHEW_READY)
	HEW_BIN="$(DEBUG_DIR)/hew" bash tests/vertical-slice/run.sh

# Warm-up form for the preflight dispatcher, which derives it by name.
test-vertical-slice-build: hew-native runtime $(LIBHEW_READY)
	@:

# Cross-module package-import oracle: fixtures importing the in-tree
# `hew::testffi` package through `hew run --pkg-path` — imported-actor value
# asks, imported-type trait methods, and the [native] auto-link path.
test-pkg-import: hew-native runtime $(LIBHEW_READY)
	HEW_BIN="$(DEBUG_DIR)/hew" bash tests/pkg-import/run.sh

# Warm-up form for the preflight dispatcher, which derives it by name.
test-pkg-import-build: hew-native runtime $(LIBHEW_READY)
	@:

# Package-manager consumer oracle: publish-like local setup, `hew install`,
# lock/materialization assertions, `hew check`, and exact `hew run`
# stdout under an isolated HOME.
test-package-install: hew-native runtime $(LIBHEW_READY)
	HEW_BIN="$(DEBUG_DIR)/hew" bash tests/package-install/run.sh

# Warm-up form for the preflight dispatcher, which derives it by name.
test-package-install-build: hew-native runtime $(LIBHEW_READY)
	@:

# Golden MIR corpus (examples/v05/checked-mir): byte-identical --dump-mir
# oracle for internal retyping work. `checked-mir-verify` re-dumps every
# fixture and diffs against the committed goldens; `checked-mir-golden`
# recaptures them (only in a commit that justifies the dump change).
checked-mir-verify: hew
	HEW_BIN="$(DEBUG_DIR)/hew" bash scripts/checked-mir-corpus.sh verify

# Warm-up form for the preflight dispatcher, which derives it by name.
checked-mir-verify-build: hew
	@:

checked-mir-golden: hew
	HEW_BIN="$(DEBUG_DIR)/hew" bash scripts/checked-mir-corpus.sh golden

# Execution gate for the same corpus: build and run every fixture and diff
# a transcript (exit status + verbatim stdout) against its committed
# `<name>.expected` sibling.  Dumping is not running — a fixture can
# segfault on every execution while every golden stays byte-identical, so
# checked-mir-verify alone is not evidence that a drop-elaboration or
# codegen change is correct.  Runnability is read back from the compiler
# (a fixture is runnable exactly when its raw MIR declares `main`), and
# the expectation set is closed both ways: a fixture with `main` and no
# expectation fails, an expectation for a fixture without `main` fails.
checked-mir-run: hew runtime stdlib check-libhew-fresh
	HEW_BIN="$(DEBUG_DIR)/hew" bash scripts/checked-mir-corpus.sh run

# Artifacts only: the freshness check belongs to the gate.
checked-mir-run-build: hew runtime stdlib
	@:

checked-mir-expect: hew runtime stdlib check-libhew-fresh
	HEW_BIN="$(DEBUG_DIR)/hew" bash scripts/checked-mir-corpus.sh expect

# Per-function .ll byte-identity oracle (tests/ll-oracle/corpus/): proves a
# pure codegen refactor (dedup, extract-helper, file-split) emits zero changed
# IR.  `ll-diff` recompiles every fixture and diffs per-function bodies against
# the committed goldens; `ll-golden` recaptures them (only in a commit that
# justifies the IR change, with the diff in the commit body).  Both native and
# wasm32 targets are covered.
ll-diff: hew
	HEW_BIN="$(DEBUG_DIR)/hew" bash scripts/ll-corpus.sh verify

# Warm-up form for the preflight dispatcher, which derives it by name.
ll-diff-build: hew
	@:

ll-golden: hew
	HEW_BIN="$(DEBUG_DIR)/hew" bash scripts/ll-corpus.sh golden

# Self-test for the ll-byte-identity normaliser: six independently-failable
# cases that prove string-content changes and numeric-const NAME changes are
# caught, and pool-id reorderings (both string-pool and numeric-const) are
# transparent.  No compiler build required — exercises the oracle script
# against synthetic .ll snippets only.
ll-identity-selftest:
	bash scripts/ll-identity-selftest.sh

# Synthetic .ll snippets only; no compiler build.
ll-identity-selftest-build:
	@:

# Fast hew-runtime target: runs lib unit tests and all integration tests without the heavy
# QUIC/TLS/profiler feature stack (quinn, rustls, rcgen, ring, hyper, snow).
# Compile time is ~3× lower than the default-features build (measured: ~32s vs ~85s per binary).
# Profiler allocator tests in transport.rs are skipped (they require feature = "profiler").
# Run `cargo test -p hew-runtime` for the full suite including QUIC, TLS, and profiler paths.
test-runtime-unit:
	cargo nextest run --profile ci -p hew-runtime --no-default-features

# Build this target's binaries the way it builds them, without running them.
test-runtime-unit-build:
	cargo nextest run --profile ci -p hew-runtime --no-default-features --no-run

# Ratcheted wrappers for the Hew-language test suites.
#
# These targets run the suites through scripts/hew-suite-ratchet.sh and
# scripts/stdlib-ratchet.sh, which compare the set of failing tests against
# an exhaustive tracked-failures list.  Any unexpected failure or unexpected
# pass causes the gate to exit 1.  When the converging lanes land and the
# tracked failures drop to zero, delete the list entries; the ratchets then
# pass with no tracking overhead.
#
# HEW_O0_OUTCOMES_FILE, when set, wires the ratchet's O0 outcome capture into
# test-o2-differential's O0 baseline so the differential gate does not re-run
# the identical O0 pass a second time (CI sets this across both targets in the
# same job; plain `make test-hew-ratchet` / `make test-o2-differential` with no
# env var keep their original standalone behaviour).
ifneq ($(strip $(HEW_SHARD_REPORT_DIR)),)
test-hew-ratchet:
	python3 scripts/compiled-hew-shards.py aggregate --mode ratchet \
		--reports-dir "$(HEW_SHARD_REPORT_DIR)" \
		--full-inventory "$(HEW_FULL_INVENTORY)" \
		--shard-count "$(HEW_SHARD_COUNT)"

# The shard-aggregate form reads reports; it builds nothing.
test-hew-ratchet-build:
	@:
else
test-hew-ratchet: hew-native runtime $(LIBHEW_READY)
	@echo "==> Running Hew test suite (ratcheted)"
	HEW_BIN="$(DEBUG_DIR)/hew" scripts/hew-suite-ratchet.sh $(if $(HEW_O0_OUTCOMES_FILE),--emit-o0-outcomes "$(HEW_O0_OUTCOMES_FILE)")

# Warm-up form for the preflight dispatcher, which derives it by name.
test-hew-ratchet-build: hew-native runtime $(LIBHEW_READY)
	@:
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
test-core-matrix: hew-native runtime $(LIBHEW_READY)
	@echo "==> Checking the core-matrix corpus matches its generator"
	@rm -rf "$(CURDIR)/.tmp/core-matrix-regen"
	python3 scripts/core-matrix-gen.py --out "$(CURDIR)/.tmp/core-matrix-regen"
	diff -r tests/core-matrix/cells "$(CURDIR)/.tmp/core-matrix-regen"
	@echo "==> Running the core matrix (primitive x operation)"
	HEW_BIN="$(DEBUG_DIR)/hew" python3 scripts/core-matrix.py

# Warm-up form for the preflight dispatcher, which derives it by name.
test-core-matrix-build: hew-native runtime $(LIBHEW_READY)
	@:

# The -O0-vs-O2 differential-exec parity gate: every compiled `.hew` program
# must behave identically at -O0 and -O2. The no-miscompile oracle for the LLVM
# middle-end pipeline (RC9). A divergence is a miscompile and a full stop.
ifneq ($(strip $(HEW_SHARD_REPORT_DIR)),)
test-o2-differential:
	python3 scripts/compiled-hew-shards.py aggregate --mode differential \
		--reports-dir "$(HEW_SHARD_REPORT_DIR)" \
		--full-inventory "$(HEW_FULL_INVENTORY)" \
		--shard-count "$(HEW_SHARD_COUNT)"

# The shard-aggregate form reads reports; it builds nothing.
test-o2-differential-build:
	@:
else
test-o2-differential: hew-native runtime $(LIBHEW_READY)
	@echo "==> Running -O0-vs-O2 differential-exec parity gate"
	HEW_BIN="$(DEBUG_DIR)/hew" scripts/o2-differential.sh $(if $(HEW_O0_OUTCOMES_FILE),--o0-outcomes "$(HEW_O0_OUTCOMES_FILE)")

# Warm-up form for the preflight dispatcher, which derives it by name.
test-o2-differential-build: hew-native runtime $(LIBHEW_READY)
	@:
endif

o2-differential-selftest:
	bash scripts/o2-differential-selftest.sh

# Shell only; no artifacts.
o2-differential-selftest-build:
	@:

# Reachability gate: every gate target in this Makefile, every workspace crate,
# every nextest exclusion and every #[ignore]d test must be reached by a named
# CI step or preflight command. There is no waiver list — an unreached check is
# either wired in or deleted. CI runs the local dispatcher directly; this check
# proves the resulting command graph actually covers the tree.
#
# The self-test runs first. This checker parses workflows structurally, and an
# earlier version did not: it read them as raw text, so a TODO comment saying a
# gate was NOT wired counted as the wiring. The self-test pins every non-edge
# (comment, echoed string, `if: false`, untriggerable workflow) and both filter
# parsers against their counterfactuals.
check-gate-reachability: test-check-gate-reachability
	python3 scripts/check-gate-reachability.py

# Python only; no artifacts.
check-gate-reachability-build:
	@:

test-check-gate-reachability:
	python3 scripts/tests/test_check_gate_reachability.py

# Python only; no artifacts.
test-check-gate-reachability-build:
	@:

test-stdlib-ratchet: hew
	@echo "==> Type-checking stdlib (ratcheted)"
	HEW_BIN="$(DEBUG_DIR)/hew" scripts/stdlib-ratchet.sh

# Warm-up form for the preflight dispatcher, which derives it by name.
test-stdlib-ratchet-build: hew
	@:

# Verify the public stdlib index has exactly one executable fixture proof per
# module, and that each manifest fixture is run by its declared test command.
test-stdlib-execution-proofs:
	@echo "==> Verifying public stdlib execution proofs"
	HEW_BIN="$(DEBUG_DIR)/hew" scripts/stdlib-execution-proof.sh --check

# The proof runner shells out to a hew-parser example for the production
# import validation; build it here rather than inside the timed gate.
test-stdlib-execution-proofs-build:
	cargo build --locked -p hew-parser --example stdlib_import_authority

# Run every examples/ux and examples/progressive tutorial against its paired
# .expected file. The shared runner fails closed on missing/orphan expectations,
# nonzero exit status, timeout, output drift, empty inventory, and duplicate
# admission. New examples therefore cannot disappear from the authority by
# omitting their expectation.
#
test-ux-examples: hew-native runtime $(LIBHEW_READY) test-example-expectations-selftest
	@echo "==> Running ux + progressive tutorials against .expected"
	@python3 scripts/example-expectations.py \
	  --hew-bin "$(DEBUG_DIR)/hew" \
	  --label "ux + progressive tutorial" \
	  --source-root examples/ux \
	  --source-root examples/progressive

# Artifacts only: the expectations self-test belongs to the gate.
test-ux-examples-build: hew-native runtime $(LIBHEW_READY)
	@:

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
test-surface-examples: hew-native runtime $(LIBHEW_READY) test-example-expectations-selftest
	@echo "==> Running v0.5 surface examples against .expected"
	@python3 scripts/example-expectations.py \
	  --hew-bin "$(DEBUG_DIR)/hew" \
	  --label "surface" \
	  --source-root examples/v05/surfaces \
	  --source examples/net/http_await_service.hew

# Artifacts only: the expectations self-test belongs to the gate.
test-surface-examples-build: hew-native runtime $(LIBHEW_READY)
	@:

test-example-expectations-selftest:
	@python3 scripts/tests/test_example_expectations.py

# Python only; no artifacts.
test-example-expectations-selftest-build:
	@:

# Check ```hew fenced blocks in docs/ against hew check.
# Extracts each fence from docs/hew-language-guide.md and docs/specs/HEW-SPEC-2026.md
# into .tmp/doc-fences/, runs `hew check` on each, and applies the ratchet
# from scripts/doc-test-expected-failures.txt so known-failing fences do not
# block the gate while new failures always do.
#
# Skip-annotated fences (<!-- doctest: skip --> or preceding NYI callout) are
# never compiled — they describe aspirational or not-yet-implemented surfaces.
# Fail-closed default: a fence is compiled unless explicitly skipped.
#
# Run `make test-doc-examples` after any docs/ change to confirm no fence
# regressions were introduced.
test-doc-examples: hew
	@HEW_BIN="$(DEBUG_DIR)/hew" scripts/extract-doc-fences.sh

# Warm-up form for the preflight dispatcher, which derives it by name.
test-doc-examples-build: hew
	@:

# Exercise pipe-safe membership wiring across every shell ratchet, then drive
# matching and mutated doc-failure sets through the production harness.
doc-ratchet-selftest:
	@scripts/tests/test_ratchet_membership_wiring.sh
	@scripts/tests/test_doc_ratchet_membership.sh

# Shell/python only; no artifacts.
doc-ratchet-selftest-build:
	@:

# Release sanitizer gate validator self-test.
check-sanitizer-gate:
	@set -e; \
	version=0.6.0-rc1; \
	fixture=scripts/fixtures/sanitizer-gate; \
	pass=0; \
	fail=0; \
	expect_reject() { \
	  name="$$1"; asan_file="$$2"; waiver_file="$$3"; \
	  if scripts/check-sanitizer-gate.sh "$$version" "$$asan_file" "$$waiver_file"; then \
	    echo "FAIL $$name: expected reject"; fail=$$((fail + 1)); \
	  else \
	    echo "ok $$name: rejected"; pass=$$((pass + 1)); \
	  fi; \
	}; \
	expect_accept() { \
	  name="$$1"; asan_file="$$2"; waiver_file="$$3"; \
	  if scripts/check-sanitizer-gate.sh "$$version" "$$asan_file" "$$waiver_file"; then \
	    echo "ok $$name: accepted"; pass=$$((pass + 1)); \
	  else \
	    echo "FAIL $$name: expected accept"; fail=$$((fail + 1)); \
	  fi; \
	}; \
	expect_reject "1 no ASan result" "$$fixture/missing.result" "$$fixture/waivers/valid.toml"; \
	expect_reject "2 ASan red" "$$fixture/asan-fail.result" "$$fixture/waivers/valid.toml"; \
	expect_reject "3 ASan ambiguous/skipped" "$$fixture/asan-ambiguous.result" "$$fixture/waivers/valid.toml"; \
	expect_reject "4 missing TSan/Miri evidence" "$$fixture/asan-pass.result" "$$fixture/waivers/none.toml"; \
	expect_reject "5 evidence for different release" "$$fixture/asan-pass.result" "$$fixture/waivers/different-release.toml"; \
	expect_reject "6 expired evidence" "$$fixture/asan-pass.result" "$$fixture/waivers/expired.toml"; \
	expect_reject "7 blanket evidence" "$$fixture/asan-pass.result" "$$fixture/waivers/blanket.toml"; \
	expect_reject "8 missing behavior" "$$fixture/asan-pass.result" "$$fixture/waivers/missing-field.toml"; \
	expect_reject "9 duplicate axis evidence" "$$fixture/asan-pass.result" "$$fixture/waivers/duplicate.toml"; \
	expect_reject "10 untracked evidence" "$$fixture/asan-pass.result" "$$fixture/waivers/bad-tracking.toml"; \
	expect_reject "11 duplicate ledger key" "$$fixture/asan-pass.result" "$$fixture/waivers/duplicate-key.toml"; \
	expect_reject "12 vague behavioral evidence" "$$fixture/asan-pass.result" "$$fixture/waivers/vague.toml"; \
	expect_accept "13 ASan green with bounded release evidence" "$$fixture/asan-pass.result" "$$fixture/waivers/valid.toml"; \
	echo "$$pass sanitizer gate cases passed, $$fail failed"; \
	if [ "$$fail" -ne 0 ]; then exit 1; fi

# Shell fixtures only; no artifacts.
check-sanitizer-gate-build:
	@:

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
asan:
	CARGO_TARGET_DIR=$(RUNTIME_ASAN_TARGET_DIR) \
	RUSTFLAGS="-Zsanitizer=address -Cforce-frame-pointers=yes" \
	ASAN_OPTIONS="detect_leaks=1" \
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

# Platform-independent counterfactuals for the ASan/LSan sentinel: a genuine
# sanitizer diagnostic must be accepted, while a bare non-zero probe exit must
# stay red instead of certifying instrumentation that never reported a leak.
test-asan-fixture-selftest:
	scripts/asan-fixture-check.sh --selftest

# Shell only; no artifacts.
test-asan-fixture-selftest-build:
	@:

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

lint: structural-lint runtime-poison-safe-lint lint-wasm-todo leak-scan codegen-carried-identity-gate codegen-trap-inventory-check verify-ffi test-verify-ffi test-python310-toml-compat verify-sys-lane-closure hew-fmt-check sandbox-parity-coverage-check tool-pin-contract-check lint-ci-coverage-check
	cargo clippy --workspace --tests -- -D warnings

# Clippy's check artifacts are a separate fingerprint from rustc's, so the
# warm-up runs clippy.  The trailing `-- -D warnings` is dropped: a lint
# failure must surface as the timed gate, not as an aborted warm-up.
lint-build: structural-lint-bootstrap-install
	cargo clippy --workspace --tests

lint-ci-coverage-check:
	python3 scripts/check-lint-ci-coverage.py
	python3 scripts/tests/test_check_lint_ci_coverage.py

# Python only; no artifacts.
lint-ci-coverage-check-build:
	@:

# Self-provisioning: the pinned toolchain install is a prerequisite of every
# structural-lint entry point, not a separate manual step. The install path
# (scripts/ast-grep-lint.sh --bootstrap --install-only, via
# build-ast-grep-lang.sh) is idempotent and checks the pinned lock/version
# before touching the network or recompiling, so a warm cache makes this a
# fast no-op — local `make lint` and CI both provision through the same
# target instead of drifting. --install-only stops after the verified
# install: the audit and the scan belong to the structural-lint recipe
# below, so provisioning a consumer never re-runs the lint gate.
.NOTPARALLEL: structural-lint structural-lint-bootstrap
structural-lint: structural-lint-bootstrap-install test-structural-authority-audit
	scripts/ast-grep-lint.sh

# Provision the pinned ast-grep toolchain; the scan belongs to the gate.
structural-lint-build: structural-lint-bootstrap-install
	@:

structural-lint-bootstrap: structural-lint-bootstrap-install test-structural-authority-audit test-ast-grep-contract test-structural-lint-bootstrap

structural-lint-bootstrap-install:
	scripts/ast-grep-lint.sh --bootstrap --install-only

test-structural-authority-audit:
	python3 scripts/tests/test_structural_authority_audit.py

# Provision the pinned ast-grep toolchain; the audit belongs to the gate.
test-structural-authority-audit-build: structural-lint-bootstrap-install
	@:

test-ast-grep-contract:
	bash scripts/tests/test_ast_grep_contract.sh

# Provision the pinned ast-grep toolchain; the contract belongs to the gate.
test-ast-grep-contract-build: structural-lint-bootstrap-install
	@:

test-structural-lint-bootstrap:
	python3 scripts/tests/test_structural_lint_bootstrap.py

# Provision the pinned ast-grep toolchain; the assertions belong to the gate.
test-structural-lint-bootstrap-build: structural-lint-bootstrap-install
	@:

# Keep nightly FreeBSD coverage and both release-gate legs on one exact
# nextest/provisioning contract. The required Clippy & format job runs this
# unconditionally; the docs/scripts job and scripts-config preflight also run it
# for fast feedback on the files most likely to change the contract.
freebsd-workflow-contract-check:
	python3 scripts/tests/test_freebsd_workflow_contract.py

# Python only; no artifacts.
freebsd-workflow-contract-check-build:
	@:

# The preflight dispatcher resolves every warm-up build form in ONE
# `make --always-make --dry-run` pass and splits the plan on this marker, so
# make -- not a scan of this file's text -- decides which rules exist and what
# they would run. Under --dry-run the marker prints its own echo line; it is
# never part of a build.
preflight-plan-mark-%:
	@echo "==preflight-plan==$*"

# Keep build-system tool verification and every CI installer on one exact pin.
tool-pin-contract-check:
	python3 scripts/tests/test_tool_pin_contract.py

# Python only; no artifacts.
tool-pin-contract-check-build:
	@:

# Assert every VM-dependent hew-sandbox-wasm test binary (one containing a
# function that spawns the hew-sandbox-vm Node runner) is excluded WHOLE
# from the generic nextest default-filter (.config/nextest.toml,
# profile.default and profile.ci) and has every one of its tests run by the
# provisioned `make sandbox-parity` gate. Catches a new VM-dependent binary
# landing in either state alone -- unprovisioned generic runs failing, or
# provisioned coverage silently never running it. Classification is
# binary-level, not per-test: see the script's module docstring for why a
# same-file call graph cannot safely attribute VM-dependence to individual
# tests.
sandbox-parity-coverage-check: test-sandbox-parity-coverage-check
	python3 scripts/check-sandbox-parity-coverage.py

# Python only; no artifacts.
sandbox-parity-coverage-check-build:
	@:

# Self-test for the checker above: proves a VM spawn marker anywhere in a
# test file condemns the whole binary regardless of which test can be
# statically shown to reach it, and that a test reaching the marker only
# through untraceable indirection (e.g. a runtime dispatch table) cannot
# evade classification. See scripts/tests/test_check_sandbox_parity_coverage.py.
test-sandbox-parity-coverage-check:
	python3 scripts/tests/test_check_sandbox_parity_coverage.py

# Python only; no artifacts.
test-sandbox-parity-coverage-check-build:
	@:

# Keep the required release handoff fail-closed and correlated to its exact
# downstream workflow run. This target is called by CI for release workflow
# and static-oracle changes and by the scripts/config preflight profile.
test-release-workflow-contract:
	python3 scripts/tests/test_release_workflow_contract.py
	python3 scripts/tests/test_pre_release_validate_contract.py
	python3 scripts/tests/test_cargo_output_dir.py
	python3 scripts/tests/test_target_dir_gate_wiring.py

# Python only; no artifacts.
test-release-workflow-contract-build:
	@:

# Scan tracked source for orchestration-token leaks (lane IDs, Q-tags, .tmp/ paths)
# and scan commit-message bodies of commits not yet on origin/main for the same tokens.
# Runs fast (<2 s each, git-grep and git-log only).
# See scripts/lint-orchestration-leak.sh and tests/leak-scan/ for the token catalogue.
leak-scan:
	bash scripts/lint-orchestration-leak.sh
	bash scripts/lint-orchestration-leak.sh --scan-commits

# git-grep only; no artifacts.
leak-scan-build:
	@:

# Check that std/ and examples/ .hew sources are formatted.
# Run `find std examples -name "*.hew" -print0 | xargs -0 hew fmt` to fix.
hew-fmt-check: hew
	@echo "==> hew-fmt-check: checking std/ and examples/ .hew sources"
	@total=$$(find std examples -name "*.hew" | wc -l | tr -d ' '); \
	bash scripts/lib/corpus-nonempty.sh hew-fmt-check-files "$$total" || exit 1; \
	find std examples -name "*.hew" -print0 \
	    | xargs -0 "$(DEBUG_DIR)/hew" fmt --check \
	    && echo "hew-fmt-check passed: all $$total .hew sources are formatted." \
	    || { echo "error: unformatted .hew sources found — run 'find std examples -name \"*.hew\" -print0 | xargs -0 hew fmt' to fix." >&2; exit 1; }

# Warm-up form for the preflight dispatcher, which derives it by name.
hew-fmt-check-build: hew
	@:

# Exercise representative migration inputs in an isolated copy so the proof
# never edits the checkout. The second pass must leave the first-pass snapshot
# byte-identical.
test-migrate-corpus: hew
	@set -e; migration_root=$$(mktemp -d); migration_fixed=$$(mktemp -d); \
	trap 'rm -rf "$$migration_root" "$$migration_fixed"' 0; \
	cp -R tests/corpus/migrate/. "$$migration_root/"; \
	echo "1/6 migrate accepted representative sources"; \
	"$(DEBUG_DIR)/hew" fmt --migrate --root "$$migration_root/accept"; \
	echo "2/6 compare exact migrated sources"; \
	for migration_source in "$$migration_root"/accept/*.hew; do \
		migration_expected="$${migration_source%.hew}.expected"; \
		diff -u "$$migration_expected" "$$migration_source"; \
	done; \
	echo "3/6 require the unresolvable source to fail loudly"; \
	migration_refusal="$$migration_root/refusal.log"; \
	if "$(DEBUG_DIR)/hew" fmt --migrate --root "$$migration_root/reject" >"$$migration_refusal" 2>&1; then \
		cat "$$migration_refusal"; \
		echo "error: migration accepted the unresolvable representative site" >&2; \
		exit 1; \
	fi; \
	grep -F 'unresolvable.hew:24-35: type checking failed: undefined function `Missing`' "$$migration_refusal"; \
	diff -u tests/corpus/migrate/reject/unresolvable.hew "$$migration_root/reject/unresolvable.hew"; \
	echo "4/6 prove the migrated snapshot reaches a successful typecheck"; \
	for migration_source in "$$migration_root"/accept/*.hew; do \
		"$(DEBUG_DIR)/hew" check "$$migration_source"; \
	done; \
	echo "5/6 require a byte-identical second migration pass"; \
	cp -R "$$migration_root/accept/." "$$migration_fixed/"; \
	"$(DEBUG_DIR)/hew" fmt --migrate --root "$$migration_root/accept"; \
	diff -ru "$$migration_fixed" "$$migration_root/accept"; \
	echo "6/6 require check mode to recognize the fixed point"; \
	"$(DEBUG_DIR)/hew" fmt --migrate --check --root "$$migration_root/accept"

# Warm-up form for the preflight dispatcher, which derives it by name.
test-migrate-corpus-build: hew
	@:

# Derive the compilable corpus from the tracked source roots, format a private
# path-preserving mirror, then require the result to check and reach a fixed point.
hew-fmt-property: hew
	HEW_BIN="$(DEBUG_DIR)/hew" bash scripts/hew-fmt-property.sh

# Warm-up form for the preflight dispatcher, which derives it by name.
hew-fmt-property-build: hew
	@:

# Repo-wide hew check sweep over all tracked .hew files (excluding intentional
# reject fixtures).  Ratchets against scripts/hew-corpus-expected-failures.txt.
# Catches the class of bug where a symbol rename or type change lands in the
# compiler but fixture files across crates/tests/examples are silently missed.
# See scripts/hew-corpus-check.sh for the allowlist format and classification guide.
hew-check-all: hew
	@echo "==> hew-check-all: compiling full .hew corpus"
	HEW_BIN="$(DEBUG_DIR)/hew" scripts/hew-corpus-check.sh

# Warm-up form for the preflight dispatcher, which derives it by name.
hew-check-all-build: hew
	@:

.PHONY: codegen-carried-identity-gate
codegen-carried-identity-gate:
	@if rg -n 'contains\("__recv__"\)|split_once\("__recv__"\)|strip_suffix\("__step"\)|starts_with\("hew_metric_"\)|hew_tcp_connect|hew_dns_|actor_name_from_handler_symbol|actor_layout_key_from_handler_symbol|is_machine_step_symbol|module_uses_blocking_offload' hew-codegen-rs/src; then \
		echo "error: codegen reintroduced a string consumer for MIR-carried identity" >&2; \
		exit 1; \
	fi
	@echo "codegen carried-identity gate: OK"

# rg only; no artifacts.
codegen-carried-identity-gate-build:
	@:

.PHONY: codegen-trap-inventory-check
codegen-trap-inventory-check:
	python3 scripts/check-codegen-trap-inventory.py

# Python only; no artifacts.
codegen-trap-inventory-check-build:
	@:

# Smoke-test the release binary with `hew run` to catch process-exit aborts
# (e.g. libc++ ABI mismatch at locale destructor — issue #1606).
# Builds release binary then runs a trivial program and checks exit 0 + output.
test-release-binary:
	scripts/test-release-binary.sh

# The gate's script builds both release halves before it smoke-tests them
# (scripts/test-release-binary.sh); the release profile is a cold build and
# does not belong inside a timed gate.
test-release-binary-build:
	cargo build --release -p hew-cli
	cargo build -p hew-lib --profile release-lib

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
stdlib-errno-gate-build:
	@:

stdlib-lint: stdlib-errno-gate
	bash scripts/lint-stdlib-int-surface.sh

# rg over std/ only; no artifacts.
stdlib-lint-build:
	@:

# Grep-gate: fail on raw .lock()/.read()/.write() against any runtime global
# that has been migrated to the PoisonSafe/PoisonSafeRw wrapper, and on the
# `if let Ok(_) = X.lock()` anti-pattern anywhere in hew-runtime/src/. Extend
# the allowlist in scripts/lint-runtime-poison-safe.sh as future sweeps land.
runtime-poison-safe-lint: runtime-poison-safe-lint-self-test
	bash scripts/lint-runtime-poison-safe.sh

# grep only; no artifacts.
runtime-poison-safe-lint-build:
	@:

# Validate that the lint script's own pattern-matching regex is coherent.
# Runs synthetic violations through the linter to confirm every guard fires.
runtime-poison-safe-lint-self-test:
	bash scripts/lint-runtime-poison-safe.sh --self-test

# grep only; no artifacts.
runtime-poison-safe-lint-self-test-build:
	@:

# Validate the repository-owned WASM backlog authority and every actionable
# WASM-TODO(<stable-backlog-id>): marker. The self-test pins fail-closed
# behaviour independently of the live corpus.
lint-wasm-todo: lint-wasm-todo-self-test wasm-capability-check
	python3 scripts/lint-wasm-todo.py

# Reaches cargo through wasm-capability-check; build that generator.
lint-wasm-todo-build:
	cargo build -p hew-capability-gen

lint-wasm-todo-self-test:
	python3 scripts/lint-wasm-todo.py --self-test

# Python only; no artifacts.
lint-wasm-todo-self-test-build:
	@:

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

verify-ffi:
	python3 scripts/verify-ffi-symbols.py --classify stable --validate > /dev/null

# Python only; no artifacts.
verify-ffi-build:
	@:

test-verify-ffi:
	python3 scripts/tests/test_verify_ffi_symbols.py

# Python only; no artifacts.
test-verify-ffi-build:
	@:

# The release macOS validator uses Python 3.10, which has no stdlib tomllib.
# Force the dependency-free parser even on newer CI interpreters and run every
# production consumer of repository TOML policy/configuration.
test-python310-toml-compat:
	HEW_FORCE_TOML_FALLBACK=1 python3 scripts/tests/test_toml_compat.py

# Python only; no artifacts.
test-python310-toml-compat-build:
	@:

# ── System-lane closure ────────────────────────────────────────────────────
# docs/internal/jit-host-abi.md forbids any `stable` symbol from producing,
# installing, mutating, observing or destroying system-lane state. That is a
# property of the transitive CALL GRAPH, not of a symbol's own body: four
# hand-audits of the stable tier produced four different answers because each
# read the symbols one at a time and none of them followed the calls. This
# recomputes the closure from the lane operations outward and fails if a stable
# symbol can reach one. Run it with --list-roots or --explain SYM to see why.
verify-sys-lane-closure: test-sys-lane-closure
	python3 scripts/sys-lane-closure.py

# Python only; no artifacts.
verify-sys-lane-closure-build:
	@:

# Self-test for the checker above: proves it still fails on a transitive reach,
# that an authenticated edge clears only the caller it names, and that a stale
# or unreasoned waiver fails rather than silently widening the stable tier.
test-sys-lane-closure:
	python3 scripts/tests/test_sys_lane_closure.py

# Python only; no artifacts.
test-sys-lane-closure-build:
	@:


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
endef

install:
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

clean:
	rm -rf $(BUILD_DIR)
	cargo clean
	rm -rf $(COV_DIR)
