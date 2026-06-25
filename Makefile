# ============================================================================
# Hew Developer Makefile
#
# Builds all project artifacts into build/ with a predictable layout:
#
#   build/
#     bin/hew              — compiler driver (Rust)
#     bin/adze             — package manager (Rust)
#     bin/hew-observe      — TUI actor observer (Rust)
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
#   make hew-native   — compiler driver + native libhew archive for `hew build`
#   make adze         — just the package manager
#   make observe      — just the TUI observer (hew-observe)
#   make runtime      — just libhew_runtime.a
#   make stdlib       — all stdlib packages + combine into libhew.a
#   make wasm-runtime — WASM runtime + wire JSON/YAML/TOML archives
#   make wasm         — build hew-wasm (browser WASM via wasm-pack)
#   make playground-manifest       — regenerate examples/playground/manifest.json
#   make playground-manifest-check — verify examples/playground/manifest.json freshness
#   make sandbox-fixtures          — regenerate sandbox VM bytecode fixtures from main.hew
#   make sandbox-fixtures-check    — verify sandbox VM bytecode fixtures are fresh
#   make sandbox-parity            — native hew run ↔ sandbox VM parity harness
#   make playground-check          — manifest freshness + full hew-wasm test suite + build hew-wasm
#   make playground-wasi-check     — focused curated manifest WASI runtime preflight
#   make ci-preflight              — dispatch a conservative local preflight from the current diff
#   make ci-preflight-smoke        — fast smoke tier: fmt + in-process tests (<5 min)
#   make ci-preflight-strict       — run the local preflight superset that mirrors merge-queue gates
#   make wasm-dist    — build + copy WASM to hew.sh and hew.run
#   make test         — Rust workspace tests (fast path; excludes test-hew)
#   make test-all     — everything in test + stdlib + Hew tests (slow)
#   make test-rust         — just Rust workspace tests
#   make test-parser       — parser + lexer crate tests (narrow)
#   make test-types        — type-checker + parser + lexer crate tests (narrow)
#   make test-cli          — CLI crate tests (narrow)
#   make test-compiler-pipeline — compiler ladder + CLI pipeline tests (narrow)
#   make test-vertical-slice — end-to-end Hew compiler oracle
#   make test-runtime-net  — runtime / analysis / lsp / std-net crate tests (narrow)
#   make test-runtime-unit — hew-runtime tests without heavy QUIC/TLS/profiler stack (~3× faster)
#   make test-real-timing  — serialized real wall-clock / OS-timing quarantine tests (narrow)
#   make test-lane CRATE=<crate> — fast in-process tier for one crate (lane iteration)
#   make test-lane-all          — fast in-process tier for the whole workspace
#   make test-fast              — fast tier scoped to git-diff-affected crates (agents/devs)
#   make test-fast CRATE=<c>   — pin fast tier to one crate
#   make test-hew          — run Hew test files (std/ *_test.hew)
#   make test-ux-examples  — run examples/ux + examples/progressive tutorials against .expected files
#   make asan         — run the nightly rust-runtime ASan test command locally
#   make tsan         — run the nightly rust-runtime TSan test command locally
#   make miri         — run the curated rust-runtime Miri allowlist locally
#   make lint         — cargo clippy (workspace + tests, warnings are errors) + hew fmt gate
#   make hew-fmt-check — check that std/ and examples/ .hew files are formatted (part of lint)
#   make leak-scan    — scan tracked source for orchestration-token leaks (lane IDs, Q-tags, .tmp/ paths)
#   make fuzz-corpus    — regenerate ignored cargo-fuzz corpora from current fixtures/examples
#   make fuzz-smoke     — build and smoke-run cargo-fuzz targets locally
#   make clean        — remove build/, target/
# ============================================================================

.PHONY: all build bootstrap install-hooks hew hew-native adze observe runtime stdlib wasm-runtime wasm playground-manifest playground-manifest-check sandbox-fixtures sandbox-fixtures-check sandbox-parity playground-check playground-wasi-check ci-preflight ci-preflight-smoke ci-preflight-strict ci-local-linux wasm-dist release check-libhew-fresh
.PHONY: test test-all test-rust test-parser test-types test-cli test-compiler-pipeline test-vertical-slice test-pkg-import test-runtime-net test-runtime-unit test-real-timing test-lane test-lane-all test-fast test-stdlib test-hew test-hew-ratchet test-o2-differential test-stdlib-ratchet test-ux-examples test-surface-examples test-release-binary check-sanitizer-gate asan asan-fixtures tsan miri lint runtime-poison-safe-lint stdlib-lint stdlib-errno-gate lint-wasm-todo leak-scan hew-fmt-check grammar
.PHONY: clean install install-check uninstall verify-ffi
.PHONY: assemble assemble-release pre-release publish-docs
.PHONY: coverage coverage-summary coverage-lcov coverage-runtime coverage-combined coverage-branch
.PHONY: fuzz-corpus fuzz-smoke fuzz-oracle fuzz-oracle-selftest
.PHONY: ll-diff ll-golden ll-identity-selftest

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
#
# Default to the host triple so `make asan` works on any sanitizer-capable
# host (darwin-arm64, linux-x86_64, ...).  Nightly CI invokes `cargo +nightly
# test --target x86_64-unknown-linux-gnu` directly rather than via `make
# asan`, so changing this default does not affect the CI lane.
SANITIZER_RUST_TARGET ?= $(HOST_TRIPLE)
RUNTIME_ASAN_TARGET_DIR := target/sanitizer-runtime-asan
RUNTIME_TSAN_TARGET_DIR := target/sanitizer-runtime-tsan
RUNTIME_MIRI_TARGET_DIR := target/miri-runtime
FUZZ_TARGETS := fuzz_parse fuzz_lex fuzz_structured fuzz_machine fuzz_check fuzz_mir
FUZZ_SMOKE_SECONDS ?= 45

# ── Default target ──────────────────────────────────────────────────────────

all: hew adze observe runtime stdlib wasm-runtime assemble

# Convenience alias — rebuilds all debug artifacts including libhew.a.
# Equivalent to `make all`; exists so that `make build` behaves as expected.
build: all

# ── Rust targets ────────────────────────────────────────────────────────────

# Build the hew compiler driver (debug).
hew:
	cargo build -p hew-cli

# Build the native artifacts required for `hew build` from a source checkout.
# `cargo build -p hew-cli` only produces the driver; the link step also needs
# hew-lib's staticlib next to the driver (`target/debug/libhew.a` on Unix,
# `target/debug/hew.lib` on Windows).  Keep this target cross-platform so fresh
# Windows hosts use the same build graph as Linux/macOS rather than a bespoke
# manual `cargo build -p hew-lib` follow-up.
hew-native:
	cargo build -p hew-cli -p hew-lib

# Build the adze package manager (debug)
adze:
	cargo build -p adze-cli

# Build the TUI actor observer (debug).
# hew-observe is a sibling binary: `hew observe` delegates to it when it is
# present next to the running hew binary or on PATH (see exec_sibling_binary).
observe:
	cargo build -p hew-observe

# Build the runtime static library (debug)
runtime:
	cargo build -p hew-runtime

# Build libhew.a — the combined runtime + stdlib static library.
# The hew-lib umbrella crate depends on hew-runtime + all stdlib crates;
# Cargo produces a single deduplicated staticlib.
stdlib:
	cargo build -p hew-lib

# Build the WASM runtime + the consolidated stdlib archive (libhew_std.a)
wasm-runtime:
	cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features
	cargo build -p hew-std --target wasm32-wasip1

# Build the hew-wasm browser analysis-only module (requires: cargo install wasm-pack)
wasm:
	wasm-pack build hew-wasm --target web --release

# Regenerate the curated playground manifest consumed by downstream browser tooling.
playground-manifest:
	python3 scripts/gen-playground-manifest.py

# Verify the checked-in playground manifest is current.
playground-manifest-check:
	python3 scripts/gen-playground-manifest.py --check

sandbox-fixtures:
	cargo run -p xtask -- sandbox-fixtures

sandbox-fixtures-check:
	cargo run -p xtask -- sandbox-fixtures --check

sandbox-parity: hew stdlib
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
	npm --prefix hew-sandbox-vm run build
	cargo test -p hew-sandbox-wasm --test parity --test parity_ratchet

# Repo-local browser/tooling smoke:
# manifest freshness + full hew-wasm test suite (lib + integration) + analysis-only WASM build.
# Running full `cargo test -p hew-wasm` subsumes the --lib curated-manifest smoke and compiles
# and runs tests/v05_wasm_coverage.rs (the fixture-coverage integration suite).
playground-check: playground-manifest-check
	cargo test -p hew-wasm
	$(MAKE) wasm

# Focused curated playground WASI runtime preflight.
playground-wasi-check:
	cargo test -p hew-cli --test wasi_run_e2e curated_playground_examples_run_under_wasi -- --exact
	cargo test -p hew-cli --test wasi_run_e2e supervisor_stays_on_the_unsupported_diagnostic_path_under_wasi -- --exact

# Conservative diff-based local preflight dispatcher.
# Usage: make ci-preflight ARGS="--dry-run" or ARGS="--base origin/main"
ci-preflight:
	scripts/ci-preflight-dispatcher.sh $(ARGS)

# Fast smoke preflight: Rust fmt + the workspace's deterministic in-process
# tests (nextest smoke profile).  Designed to complete in <5 min and surface
# format and fast oracle failures before the full heavy tier is invoked.
# Clippy runs in the lint target; the fallback lane runs both sequentially.
#
# This target is invoked by the dispatcher as the first step of the fallback/heavy
# lane; the full suite (make test) still runs on smoke pass.  Run it directly for
# a quick sanity pass on any diff without waiting for E2E compilation.
#
# The smoke nextest profile excludes subprocess-intensive tests (eval_e2e,
# test_runner_e2e, parity) and hew-wasm; see .config/nextest.toml [profile.smoke].
#
# Build-graph note: cargo clippy and cargo nextest both compile the hew-cli
# library, so `make hew` (cargo build -p hew-cli) after them only pays for the
# final link step (~1–2 s on a warm tree).  Some nextest smoke tests execute
# `hew run`, which links against target/debug/libhew.a; build the stdlib archive
# before nextest so a fresh checkout does not fail smoke with "cannot find
# libhew.a". Running `make hew` here also eliminates the redundant compile
# triggered by make lint → hew-fmt-check later in the fallback lane
# (hew-fmt-check requires target/debug/hew but nextest does not produce it).
ci-preflight-smoke:
	cargo fmt --all -- --check
	$(MAKE) stdlib
	cargo nextest run --workspace --profile smoke
	$(MAKE) hew

# Assert that target/debug/libhew.a is not stale relative to hew-lib and hew-runtime sources.
# Run after `make stdlib` as a fast gate; exits non-zero if the .a predates any source input.
check-libhew-fresh:
	scripts/check-libhew-fresh.sh

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

fuzz-smoke: fuzz-corpus
	@command -v cargo-fuzz >/dev/null 2>&1 || { echo "error: cargo-fuzz is required (cargo install cargo-fuzz)"; exit 127; }
	@rustup toolchain list | grep -q '^nightly' || { echo "error: Rust nightly toolchain is required (rustup install nightly)"; exit 127; }
	@cd hew-parser && rc=0 && for target in $(FUZZ_TARGETS); do \
		echo "==> cargo fuzz build $$target"; \
		cargo +nightly fuzz build "$$target" || rc=$$?; \
	done; \
	exit $$rc
	@cd hew-parser && rc=0 && for target in $(FUZZ_TARGETS); do \
		echo "==> cargo fuzz smoke $$target ($(FUZZ_SMOKE_SECONDS)s)"; \
		cargo +nightly fuzz run "$$target" -- -max_total_time=$(FUZZ_SMOKE_SECONDS) || rc=$$?; \
	done; \
	exit $$rc

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
fuzz-oracle: hew runtime stdlib check-libhew-fresh
	@if [ -n "$(FUZZ_ORACLE_FULL)" ]; then \
		python3 scripts/fuzz/run-oracle.py --full --timeout 30; \
	else \
		python3 scripts/fuzz/run-oracle.py --timeout 30; \
	fi

# Oracle self-tests: three independently-failable checks that prove the
# harness has teeth (flags real crashes) and honours the ratchet contract
# (unexpected-pass and unexpected-fail both fail closed).
fuzz-oracle-selftest: hew runtime stdlib check-libhew-fresh
	bash scripts/fuzz/oracle-selftest.sh

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
assemble: | hew adze runtime stdlib wasm-runtime
	@mkdir -p $(BUILD_DIR)/bin $(BUILD_DIR)/lib
	@# assemble-release makes build/std a symlink to ../std; reset it so the
	@# flat std stub loop below cannot rewrite tracked std/*.hew files in root.
	@rm -rf $(BUILD_DIR)/std
	@mkdir -p $(BUILD_DIR)/std
	@# Compiler driver
	@ln -sfn ../../$(DEBUG_DIR)/hew                $(BUILD_DIR)/bin/hew
	@# Package manager
	@ln -sfn ../../$(DEBUG_DIR)/adze               $(BUILD_DIR)/bin/adze
	@# TUI actor observer (sibling binary — `hew observe` delegates here)
	@ln -sfn ../../$(DEBUG_DIR)/hew-observe        $(BUILD_DIR)/bin/hew-observe
	@# Combined Hew library (runtime + all stdlib packages)
	@ln -sfn ../../$(DEBUG_DIR)/libhew.a           $(BUILD_DIR)/lib/libhew.a
	@# WASM runtime + focused wire stdlib archives (symlink if built)
	@for lib in libhew_runtime.a libhew_std.a; do \
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
	$(RELEASE_ENV) cargo build -p adze-cli --release
	$(RELEASE_ENV) cargo build -p hew-observe --release
	$(RELEASE_ENV) cargo build -p hew-lib --release
	$(RELEASE_ENV) cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features --release
	$(RELEASE_ENV) cargo build -p hew-std --target wasm32-wasip1 --release
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
	@ln -sfn ../../$(RELEASE_DIR)/hew-observe      $(BUILD_DIR)/bin/hew-observe
	@# Combined Hew library (runtime + all stdlib packages)
	@ln -sfn ../../$(RELEASE_DIR)/libhew.a         $(BUILD_DIR)/lib/libhew.a
	@for lib in libhew_runtime.a libhew_std.a; do \
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
test-all: test test-stdlib test-hew test-ux-examples test-surface-examples

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
test-rust: stdlib wasm-runtime runtime
	@if command -v cargo-nextest >/dev/null 2>&1 || cargo nextest --version >/dev/null 2>&1; then \
		set -e; \
		cargo nextest run --workspace --profile ci --no-run; \
		test -f target/debug/libhew.a; \
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

# Build the combined runtime+stdlib static lib and the WASM runtime before
# running the compiler-pipeline tests.  Several hew-cli integration tests
# (eval_e2e, eval_wasm_*) call `hew eval` which needs both libs at link time.
# Without this prerequisite the lazy per-test build of libhew.a (~18 s on a
# cold worktree) consumes most of the default 30 s `hew eval --timeout` budget,
# causing spurious timeouts under the concurrent nextest run.  The WASM runtime
# (libhew_runtime.a for wasm32-wasip1) is needed by wasm32-wasi eval tests
# even when they are expected to fail before codegen (the linker search runs
# before the fast typecheck path reports its diagnostic).
test-compiler-pipeline: stdlib wasm-runtime
	cargo nextest run --profile ci \
		-p hew-lexer \
		-p hew-parser \
		-p hew-types \
		-p hew-hir \
		-p hew-mir \
		-p hew-codegen-rs \
		-p hew-cli \
		-p adze-cli

# End-to-end Hew compiler oracle: real .hew fixtures through check/compile/run.
# Build libhew first and verify freshness so native fixture links do not test
# against stale runtime/stdlib archives on a fresh checkout or CI runner.
test-vertical-slice: hew runtime stdlib check-libhew-fresh
	bash tests/vertical-slice/run.sh

# Cross-module package-import oracle: fixtures importing the in-tree
# `hew::testffi` package through `hew run --pkg-path` — imported-actor value
# asks, imported-type trait methods, and the [native] auto-link path.
test-pkg-import: hew runtime stdlib check-libhew-fresh
	bash tests/pkg-import/run.sh

# Golden MIR corpus (examples/v05/checked-mir): byte-identical --dump-mir
# oracle for internal retyping work. `checked-mir-verify` re-dumps every
# fixture and diffs against the committed goldens; `checked-mir-golden`
# recaptures them (only in a commit that justifies the dump change).
checked-mir-verify: hew
	bash scripts/checked-mir-corpus.sh verify

checked-mir-golden: hew
	bash scripts/checked-mir-corpus.sh golden

# Per-function .ll byte-identity oracle (tests/ll-oracle/corpus/): proves a
# pure codegen refactor (dedup, extract-helper, file-split) emits zero changed
# IR.  `ll-diff` recompiles every fixture and diffs per-function bodies against
# the committed goldens; `ll-golden` recaptures them (only in a commit that
# justifies the IR change, with the diff in the commit body).  Both native and
# wasm32 targets are covered.
ll-diff: hew
	bash scripts/ll-corpus.sh verify

ll-golden: hew
	bash scripts/ll-corpus.sh golden

# Self-test for the ll-byte-identity normaliser: four independently-failable
# cases that prove string-content changes are caught and pool-id reorderings
# are transparent.  No compiler build required — exercises the oracle script
# against synthetic .ll snippets only.
ll-identity-selftest:
	bash scripts/ll-identity-selftest.sh

test-runtime-net:
	cargo nextest run --profile ci --no-fail-fast \
		-p hew-runtime \
		-p hew-analysis \
		-p hew-lsp \
		-p hew-std

# Fast hew-runtime target: runs lib unit tests and all integration tests without the heavy
# QUIC/TLS/profiler feature stack (quinn, rustls, rcgen, ring, hyper, snow).
# Compile time is ~3× lower than the default-features build (measured: ~32s vs ~85s per binary).
# Profiler allocator tests in transport.rs are skipped (they require feature = "profiler").
# Run `cargo test -p hew-runtime` for the full suite including QUIC, TLS, and profiler paths.
test-runtime-unit:
	cargo nextest run --profile ci -p hew-runtime --no-default-features

# Real-timing quarantine gate: the genuinely-real wall-clock / OS-timing tests
# (SIGKILL delivery to a process group + the two-process loopback ask) that run
# serialized in the `real-timing` nextest group (max-threads = 1; see
# .config/nextest.toml) and are kept off the lane/smoke fast tiers.  Scoped to the
# owning crates so the gate stays prompt.  Keep this selector in sync with the
# `real-timing` group membership in .config/nextest.toml.
test-real-timing:
	cargo nextest run --profile ci \
		-p hew-cli \
		-p hew-runtime \
		-E 'test(bounded_child_timeout_kills_grandchild_process_group) or test(two_process_remote_ask_echo_double_returns_42) or test(remote_ask_two_process_echo_client_helper)'

# ── Lane-iteration tier ──────────────────────────────────────────────────────
# Fast in-process tests only — exec/e2e corpus excluded (see profile.lane in
# .config/nextest.toml for the exclusion list and coverage contract).
#
# Fast tier — exec corpus runs at the integrated gate.
#
# Usage:
#   make test-lane CRATE=hew-types        # single crate
#   make test-lane CRATE=hew-mir          # single crate
#   make test-lane-all                    # full workspace
#
# Acceptance: use the plan's named proving gates (make test-types,
#   make test-compiler-pipeline) — not this tier — before declaring ready.
test-lane:
ifndef CRATE
	$(error CRATE is required: make test-lane CRATE=<crate-name>)
endif
	@echo "==> fast tier — exec corpus runs at the integrated gate"
	cargo nextest run --profile lane -p $(CRATE)

test-lane-all:
	@echo "==> fast tier — exec corpus runs at the integrated gate"
	cargo nextest run --workspace --profile lane

# ── Affected-crate fast tier ─────────────────────────────────────────────────
# Derives scope from git diff: runs nextest --profile lane restricted to the
# rdeps() closure of crates that have changed since the merge base.  Falls back
# to a full workspace lane run when no crates are detected (e.g. first commit,
# workspace-wide file change, or no diff vs origin).
#
# Usage:
#   make test-fast             # auto-derive scope from git diff
#   make test-fast CRATE=hew-types  # pin to one crate (skips diff derivation)
#
# This target replaces hand-curated -p lists for interactive iteration.
# The exec/e2e corpus is still excluded (profile.lane).  Acceptance gates
# require the plan's named proving commands, not this tier.
test-fast:
	@crates=$$(CRATE="$(CRATE)" scripts/changed-crates.sh); \
	if [ -n "$$crates" ]; then \
	  echo "==> fast tier — affected: $$crates"; \
	  cargo nextest run --profile lane -E "rdeps($$crates)"; \
	else \
	  echo "==> fast tier — full workspace (no crate-specific diff detected)"; \
	  cargo nextest run --workspace --profile lane; \
	fi

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

# Ratcheted wrappers for the Hew-language test suites.
#
# These targets run the suites through scripts/hew-suite-ratchet.sh and
# scripts/stdlib-ratchet.sh, which compare the set of failing tests against
# an exhaustive tracked-failures list.  Any unexpected failure or unexpected
# pass causes the gate to exit 1.  When the converging lanes land and the
# tracked failures drop to zero, delete the list entries; the ratchets then
# pass with no tracking overhead.
test-hew-ratchet: hew runtime stdlib
	@echo "==> Running Hew test suite (ratcheted)"
	scripts/hew-suite-ratchet.sh

# The -O0-vs-O2 differential-exec parity gate: every compiled `.hew` program
# must behave identically at -O0 and -O2. The no-miscompile oracle for the LLVM
# middle-end pipeline (RC9). A divergence is a miscompile and a full stop.
test-o2-differential: hew runtime stdlib
	@echo "==> Running -O0-vs-O2 differential-exec parity gate"
	scripts/o2-differential.sh

test-stdlib-ratchet: hew
	@echo "==> Type-checking stdlib (ratcheted)"
	scripts/stdlib-ratchet.sh

# Run every examples/ux and examples/progressive tutorial against its paired
# .expected file.  Any tutorial whose .expected output diverges from `hew run`
# output fails the gate, catching dialect regressions before they reach users.
#
# Tutorials that depend on unimplemented substrate are explicitly listed in
# UX_SKIP and PROG_SKIP with a one-line reason; they are reported as SKIP, not
# failures.  Remove a file from the skip list once the substrate gap is closed.
#
#   hew_duplex_close NYI — lambda-actor close() not yet lowered to MIR
#     tracked: deferred-v05-followups.md (hew_duplex_close)
UX_SKIP   := examples/ux/10_lambda_actor.hew
PROG_SKIP  := examples/progressive/10_lambda_actor.hew

test-ux-examples: hew runtime stdlib
	@echo "==> Running ux + progressive tutorials against .expected"
	@fail=0; pass=0; skip=0; \
	for corpus in examples/ux examples/progressive; do \
	  for src in $$(find $$corpus -maxdepth 1 -name '*.hew' | sort); do \
	    exp="$${src%.hew}.expected"; \
	    test -f "$$exp" || continue; \
	    for s in $(UX_SKIP) $(PROG_SKIP); do \
	      if [ "$$src" = "$$s" ]; then \
	        echo "  SKIP: $$src  (substrate-gated: hew_duplex_close NYI)"; \
	        skip=$$((skip + 1)); \
	        continue 2; \
	      fi; \
	    done; \
	    actual=$$($(DEBUG_DIR)/hew run "$$src" 2>&1); \
	    expected=$$(cat "$$exp"); \
	    if [ "$$actual" = "$$expected" ]; then \
	      pass=$$((pass + 1)); \
	    else \
	      echo "  FAIL: $$src"; \
	      echo "    expected: $$(echo "$$expected" | head -3 | tr '\n' '|')"; \
	      echo "    actual:   $$(echo "$$actual"   | head -3 | tr '\n' '|')"; \
	      fail=$$((fail + 1)); \
	    fi; \
	  done; \
	done; \
	echo "  $$pass passed, $$skip skipped (substrate-gated), $$fail failed"; \
	if [ $$fail -gt 0 ]; then \
	  echo "ERROR: $$fail tutorial(s) failed — run \`hew run <file>\` to reproduce"; \
	  exit 1; \
	fi

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
test-surface-examples: hew runtime stdlib
	@echo "==> Running v0.5 surface examples against .expected"
	@fail=0; pass=0; \
	srcs="$$(find examples/v05/surfaces -maxdepth 1 -name '*.hew' | sort) examples/net/http_await_service.hew"; \
	for src in $$srcs; do \
	  exp="$${src%.hew}.expected"; \
	  test -f "$$exp" || continue; \
	  actual=$$($(DEBUG_DIR)/hew run "$$src" 2>&1); \
	  expected=$$(cat "$$exp"); \
	  if [ "$$actual" = "$$expected" ]; then \
	    pass=$$((pass + 1)); \
	  else \
	    echo "  FAIL: $$src"; \
	    echo "    expected: $$(echo "$$expected" | head -3 | tr '\n' '|')"; \
	    echo "    actual:   $$(echo "$$actual"   | head -3 | tr '\n' '|')"; \
	    fail=$$((fail + 1)); \
	  fi; \
	done; \
	echo "  $$pass passed, $$fail failed"; \
	if [ $$fail -gt 0 ]; then \
	  echo "ERROR: $$fail surface example(s) failed — run \`hew run <file>\` to reproduce"; \
	  exit 1; \
	fi

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
	@scripts/extract-doc-fences.sh

# Release sanitizer gate validator self-test.
check-sanitizer-gate:
	@set -e; \
	commit=0123456789abcdef0123456789abcdef01234567; \
	fixture=scripts/fixtures/sanitizer-gate; \
	pass=0; \
	fail=0; \
	expect_reject() { \
	  name="$$1"; asan_file="$$2"; waiver_file="$$3"; \
	  if scripts/check-sanitizer-gate.sh "$$commit" "$$asan_file" "$$waiver_file"; then \
	    echo "FAIL $$name: expected reject"; fail=$$((fail + 1)); \
	  else \
	    echo "ok $$name: rejected"; pass=$$((pass + 1)); \
	  fi; \
	}; \
	expect_accept() { \
	  name="$$1"; asan_file="$$2"; waiver_file="$$3"; \
	  if scripts/check-sanitizer-gate.sh "$$commit" "$$asan_file" "$$waiver_file"; then \
	    echo "ok $$name: accepted"; pass=$$((pass + 1)); \
	  else \
	    echo "FAIL $$name: expected accept"; fail=$$((fail + 1)); \
	  fi; \
	}; \
	expect_reject "1 no ASan result" "$$fixture/missing.result" "$$fixture/waivers/valid.toml"; \
	expect_reject "2 ASan red" "$$fixture/asan-fail.result" "$$fixture/waivers/valid.toml"; \
	expect_reject "3 ASan ambiguous/skipped" "$$fixture/asan-ambiguous.result" "$$fixture/waivers/valid.toml"; \
	expect_reject "4 missing TSan/Miri waivers" "$$fixture/asan-pass.result" "$$fixture/waivers/none.toml"; \
	expect_reject "5 waiver for different commit" "$$fixture/asan-pass.result" "$$fixture/waivers/different-commit.toml"; \
	expect_reject "6 expired waiver" "$$fixture/asan-pass.result" "$$fixture/waivers/expired.toml"; \
	expect_reject "7 blanket waiver" "$$fixture/asan-pass.result" "$$fixture/waivers/blanket.toml"; \
	expect_reject "8 missing required field" "$$fixture/asan-pass.result" "$$fixture/waivers/missing-field.toml"; \
	expect_accept "9 ASan green with valid commit-scoped waivers" "$$fixture/asan-pass.result" "$$fixture/waivers/valid.toml"; \
	echo "$$pass sanitizer gate cases passed, $$fail failed"; \
	if [ "$$fail" -ne 0 ]; then exit 1; fi

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
asan:
	CARGO_TARGET_DIR=$(RUNTIME_ASAN_TARGET_DIR) \
	RUSTFLAGS="-Zsanitizer=address -Cforce-frame-pointers=yes" \
	ASAN_OPTIONS="detect_leaks=1" \
	ASAN_SYMBOLIZER_PATH=$(ASAN_SYMBOLIZER) \
	LSAN_OPTIONS="suppressions=$(CURDIR)/hew-runtime/lsan.supp" \
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
asan-fixtures:
ifeq ($(shell uname -s),Darwin)
	@echo "asan-fixtures: skipped on macOS — use the leaks oracle in hew-cli/tests/*_leak_oracle.rs"
else
	LLVM_VERSION=$(LLVM_VERSION) \
	SANITIZER_RUST_TARGET=$(SANITIZER_RUST_TARGET) \
	scripts/asan-fixture-check.sh
endif

# Nightly rust-runtime TSan command (Linux/nightly toolchain required).
#
# TSan is not currently supported on darwin-arm64 by the upstream Rust
# nightly toolchain (build-std + TSan link failures, mirrored by the
# nightly-sanitizers.yml advisory lane).  Skip with a clear message so
# the make target is a usable signal rather than a confusing failure.
tsan:
ifeq ($(shell uname -sm),Darwin arm64)
	@echo "tsan: skipped on darwin-arm64 (upstream Rust nightly TSan not supported on this target — see nightly-sanitizers.yml rust-runtime-tsan advisory lane)"
else
	CARGO_TARGET_DIR=$(RUNTIME_TSAN_TARGET_DIR) \
	RUSTFLAGS="-Zsanitizer=thread -Cforce-frame-pointers=yes -Cunsafe-allow-abi-mismatch=sanitizer" \
	TSAN_OPTIONS="halt_on_error=0 suppressions=$(CURDIR)/hew-runtime/tsan.supp" \
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

lint: runtime-poison-safe-lint lint-wasm-todo leak-scan verify-ffi hew-fmt-check
	cargo clippy --workspace --tests -- -D warnings

# Scan tracked source for orchestration-token leaks (lane IDs, Q-tags, .tmp/ paths)
# and scan commit-message bodies of commits not yet on origin/main for the same tokens.
# Runs fast (<2 s each, git-grep and git-log only).
# See scripts/lint-orchestration-leak.sh and tests/leak-scan/ for the token catalogue.
leak-scan:
	bash scripts/lint-orchestration-leak.sh
	bash scripts/lint-orchestration-leak.sh --scan-commits

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
	install -m 755 $(RELEASE_DIR)/hew-observe        $(DESTDIR)$(PREFIX)/bin/hew-observe
	install -m 644 $(RELEASE_DIR)/libhew.a           $(DESTDIR)$(PREFIX)/lib/libhew.a
	@for lib in libhew_runtime.a libhew_std.a; do \
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
	@test -f $(RELEASE_DIR)/adze \
		|| { echo "Error: release adze not built. Run 'make release' first."; exit 1; }
	@test -f $(RELEASE_DIR)/hew-observe \
		|| { echo "Error: release hew-observe not built. Run 'make release' first."; exit 1; }
	@test -f $(RELEASE_DIR)/libhew.a \
		|| { echo "Error: libhew.a not built. Run 'make release' first."; exit 1; }
	@test -f $(WASM_RELEASE_DIR)/libhew_runtime.a \
		|| { echo "Error: wasm runtime not built. Run 'make release' first."; exit 1; }

uninstall:
	rm -rf $(DESTDIR)$(PREFIX)
	@echo "==> Removed $(DESTDIR)$(PREFIX)"

# ── Cleanup ─────────────────────────────────────────────────────────────────

clean:
	rm -rf $(BUILD_DIR)
	cargo clean
	rm -rf $(GRAMMAR_OUT) .tmp/Hew.g4
	rm -rf $(COV_DIR)
