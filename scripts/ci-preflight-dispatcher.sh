#!/usr/bin/env bash

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# shellcheck source=scripts/lib/timeout.sh
# shellcheck disable=SC1091
source "${REPO_ROOT}/scripts/lib/timeout.sh"

# Per-lane wall-clock budgets (seconds).  These values bound hung commands and
# surface the dominant-cost step in the summary table.  Override via env vars
# *for measurement only* — the timeout still kills the command on expiry; these
# variables only adjust the budget ceiling, they are not a bypass.
PREFLIGHT_TIMEOUT_DOCS="${PREFLIGHT_TIMEOUT_DOCS:-30}"
PREFLIGHT_TIMEOUT_NARROW="${PREFLIGHT_TIMEOUT_NARROW:-180}"
PREFLIGHT_TIMEOUT_FALLBACK="${PREFLIGHT_TIMEOUT_FALLBACK:-600}"

DRY_RUN=0
FAIL_FAST=0
BASE_REF=""
EXPLICIT_PATHS=0
LANE=""
LANE_REASON=""
CHANGED_FILES=()
COMMANDS=()
PROFILE_JSON_PATH=""
LIST_CI_REQUIRED=0

# ── CI-required command patterns (single source of truth) ─────────────────────
# These are the command substrings that the fallback lane MUST include so that
# a local `make ci-preflight` predicts the merge-queue outcome.  The
# check-preflight-ci-parity.sh script reads these via --ci-required rather than
# maintaining its own independent list.  Update here when the CI workflows or
# the fallback lane change.
#
# Each entry is a tab-separated pair: LABEL<TAB>PATTERN
# PATTERN is a substring matched against the dispatcher's fallback-lane output.
CI_REQUIRED_CHECKS=(
    "Rust fmt check (ci.yml: cargo fmt --all -- --check)	make ci-preflight-smoke"
    "Cargo clippy (ci.yml: cargo clippy --workspace --tests -- -D warnings)	make ci-preflight-smoke"
    "verify-ffi (ci.yml: make verify-ffi)	make lint"
    "runtime-poison-safe-lint (ci.yml: make runtime-poison-safe-lint)	make lint"
    "nextest workspace ci (release-gate.yml: nextest run --workspace --profile ci)	make test"
    "playground-check (release-gate.yml: make playground-check)	make playground-check"
    "Hew test suite ratchet (ci.yml: make test-hew-ratchet)	make test-hew-ratchet"
    "Stdlib type-check ratchet (ci.yml: make test-stdlib-ratchet)	make test-stdlib-ratchet"
    "Vertical slice oracle (ci.yml: make test-vertical-slice)	make test-vertical-slice"
    "Package-import oracle (ci.yml: make test-pkg-import)	make test-pkg-import"
    "Fuzz-oracle ratchet (ci.yml: make fuzz-oracle)	make fuzz-oracle"
    "Sandbox parity (ci.yml: make sandbox-parity)	make sandbox-parity"
)

usage() {
    cat <<'EOF'
Usage: scripts/ci-preflight-dispatcher.sh [--dry-run] [--fail-fast] [--base <ref>] [--profile-json <path>] [--ci-required] [--] [path...]

Dispatch a conservative local CI preflight based on changed files.

- Pass explicit paths to classify those files directly.
- With no paths, the script inspects committed, staged, unstaged, and untracked changes.
- By default, all selected commands run and failures are reported together at the end.
- --fail-fast           Stop after the first failed command.
- If the first-slice routing is unclear, the script runs the broader local check profile.
- --profile-json <path> Write per-command timing as a JSON array to <path> (one object per
                        command, with "cmd", "elapsed_s", "status" fields).
- --ci-required         Print the CI-required check patterns (LABEL<TAB>PATTERN) and exit 0.
                        Used by check-preflight-ci-parity.sh to avoid a second source of truth.
EOF
}

die() {
    echo "error: $*" >&2
    exit 1
}

append_unique_path() {
    local path="$1"
    local existing
    if [[ ${CHANGED_FILES[0]+set} == set ]]; then
        for existing in "${CHANGED_FILES[@]}"; do
            if [[ "$existing" == "$path" ]]; then
                return 0
            fi
        done
    fi
    CHANGED_FILES+=("$path")
}

has_changed_files() {
    [[ ${CHANGED_FILES[0]+set} == set ]]
}

normalize_path() {
    local path="$1"
    if [[ "$path" == "$REPO_ROOT/"* ]]; then
        path="${path#"$REPO_ROOT"/}"
    fi
    while [[ "$path" == ./* ]]; do
        path="${path#./}"
    done
    printf '%s\n' "$path"
}

collect_paths_from_command() {
    local path=""
    while IFS= read -r path; do
        [[ -n "$path" ]] || continue
        append_unique_path "$(normalize_path "$path")"
    done < <("$@")
}

add_command() {
    COMMANDS+=("$1")
}

has_commands() {
    [[ ${COMMANDS[0]+set} == set ]]
}

is_docs_path() {
    case "$1" in
        docs/*|*.md|AUTHORS|LICENSE|LICENSE-*|NOTICE)
            return 0
            ;;
    esac
    return 1
}

is_grammar_path() {
    case "$1" in
        docs/specs/Hew.g4|docs/specs/grammar.ebnf)
            return 0
            ;;
    esac
    return 1
}

is_parser_path() {
    case "$1" in
        hew-parser/*|hew-lexer/*)
            return 0
            ;;
    esac
    return 1
}

is_types_path() {
    case "$1" in
        hew-types/*)
            return 0
            ;;
    esac
    return 1
}

is_compiler_pipeline_path() {
    case "$1" in
        hew-hir/*|hew-mir/*|hew-codegen-rs/*)
            return 0
            ;;
    esac
    return 1
}

is_cli_path() {
    case "$1" in
        # Direct CLI crates.
        hew-cli/*|adze-cli/*)
            return 0
            ;;
        # CLI pipeline support crates: compile pipeline, C ABI helpers,
        # and code generators.  Changes here are covered by
        # cargo nextest run -p hew-cli -p adze-cli because hew-cli links
        # the full pipeline including hew-runtime (which links hew-cabi)
        # and hew-compile.
        hew-compile/*|hew-cabi/*|hew-capability-gen/*)
            return 0
            ;;
    esac
    return 1
}

is_observe_path() {
    case "$1" in
        hew-observe/Cargo.toml|hew-observe/src/*)
            return 0
            ;;
    esac
    return 1
}

is_runtime_path() {
    case "$1" in
        hew-runtime/*)
            return 0
            ;;
    esac
    return 1
}

is_runtime_testkit_path() {
    case "$1" in
        hew-runtime-testkit/*)
            return 0
            ;;
    esac
    return 1
}

is_hew_lib_path() {
    case "$1" in
        hew-lib/*)
            return 0
            ;;
    esac
    return 1
}

is_stdlib_net_path() {
    case "$1" in
        std/net/*)
            return 0
            ;;
    esac
    return 1
}

is_analysis_path() {
    case "$1" in
        hew-analysis/*)
            return 0
            ;;
    esac
    return 1
}

is_lsp_path() {
    case "$1" in
        hew-lsp/*)
            return 0
            ;;
    esac
    return 1
}

is_wasm_path() {
    case "$1" in
        hew-wasm/*)
            return 0
            ;;
    esac
    return 1
}

is_sandbox_fixture_path() {
    case "$1" in
        hew-sandbox-vm/fixtures/*|hew-sandbox-vm/test/build-fixtures.test.mjs|xtask/*)
            return 0
            ;;
    esac
    return 1
}

is_sandbox_parity_path() {
    case "$1" in
        hew-sandbox-wasm/tests/parity.rs|hew-sandbox-wasm/Cargo.toml|\
        hew-sandbox-vm/src/interpreter/parity-runner.ts|hew-sandbox-vm/package.json|\
        examples/playground/basics/hello_world.hew|examples/playground/basics/fibonacci.hew|\
        examples/playground/concurrency/counter_actor.hew|examples/playground/concurrency/actor_pipeline.hew|\
        examples/playground/concurrency/supervisor.hew|examples/playground/machines/traffic_light.hew)
            return 0
            ;;
    esac
    return 1
}

is_vertical_slice_path() {
    # tests/pkg-import is the cross-module package-import sibling of the
    # vertical-slice oracle: same end-to-end compiler ladder, same lane.
    # tests/fuzz-oracle is the trap/signal ratchet — same compiler-ladder tier.
    case "$1" in
        tests/vertical-slice/*|tests/pkg-import/*|tests/fuzz-oracle/*)
            return 0
            ;;
    esac
    return 1
}

is_hew_tests_path() {
    case "$1" in
        tests/hew/*)
            return 0
            ;;
    esac
    return 1
}

is_trap_fixtures_path() {
    # MIR bounds/trap lowering and the fuzz-oracle fixture corpus.
    # Changes here must run make fuzz-oracle — the ratchet that checks trap
    # signal codes (SIGILL/SIGTRAP) and expected-failures.txt alignment.
    case "$1" in
        hew-mir/src/lower.rs|\
        hew-mir/src/model.rs|\
        hew-codegen-rs/src/llvm.rs|\
        tests/fuzz-oracle/*)
            return 0
            ;;
    esac
    return 1
}

is_scripts_config_path() {
    case "$1" in
        Makefile|.gitignore|scripts/*|.config/nextest.toml|.github/workflows/*|Cargo.toml|Cargo.lock|.cargo/*|rust-toolchain*)
            return 0
            ;;
    esac
    return 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        --fail-fast)
            FAIL_FAST=1
            shift
            ;;
        --base)
            shift
            [[ $# -gt 0 ]] || die "--base requires a ref"
            BASE_REF="$1"
            shift
            ;;
        --profile-json)
            shift
            [[ $# -gt 0 ]] || die "--profile-json requires a path"
            PROFILE_JSON_PATH="$1"
            shift
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        --ci-required)
            LIST_CI_REQUIRED=1
            shift
            ;;
        --)
            shift
            EXPLICIT_PATHS=1
            while [[ $# -gt 0 ]]; do
                append_unique_path "$(normalize_path "$1")"
                shift
            done
            ;;
        -*)
            die "unknown option: $1"
            ;;
        *)
            EXPLICIT_PATHS=1
            append_unique_path "$(normalize_path "$1")"
            shift
            ;;
    esac
done

if (( LIST_CI_REQUIRED == 1 )); then
    for entry in "${CI_REQUIRED_CHECKS[@]}"; do
        printf '%s\n' "$entry"
    done
    exit 0
fi

if [[ -n "$BASE_REF" ]] && ! git rev-parse --verify "$BASE_REF" >/dev/null 2>&1; then
    die "unknown base ref: $BASE_REF"
fi

ci_preflight_base_unresolved=0

if (( EXPLICIT_PATHS == 0 )); then
    if [[ -z "$BASE_REF" ]]; then
        if [[ -n "${CI_PREFLIGHT_BASE:-}" ]]; then
            if git rev-parse --verify "$CI_PREFLIGHT_BASE" >/dev/null 2>&1; then
                BASE_REF="$CI_PREFLIGHT_BASE"
            else
                ci_preflight_base_unresolved=1
                echo "warning: CI_PREFLIGHT_BASE=$CI_PREFLIGHT_BASE did not resolve; falling back" >&2
            fi
        fi
    fi

    if [[ -z "$BASE_REF" ]]; then
        if (( ci_preflight_base_unresolved == 0 )) && [[ -z "${CI:-}" && -z "${GITHUB_ACTIONS:-}" ]] && git rev-parse --verify v05-integration >/dev/null 2>&1; then
            BASE_REF="v05-integration"
        elif git rev-parse --verify origin/main >/dev/null 2>&1; then
            BASE_REF="origin/main"
        elif git rev-parse --verify main >/dev/null 2>&1; then
            BASE_REF="main"
        fi
    fi

    if [[ -n "$BASE_REF" ]]; then
        collect_paths_from_command git diff --name-only --diff-filter=ACMRD "$BASE_REF...HEAD"
    fi
    collect_paths_from_command git diff --cached --name-only --diff-filter=ACMRD
    collect_paths_from_command git diff --name-only --diff-filter=ACMRD
    collect_paths_from_command git ls-files --others --exclude-standard
fi

if ! has_changed_files; then
    echo "==> Hew CI preflight dispatcher"
    echo "No changed files detected."
    exit 0
fi

fallback_lane=0
has_grammar=0
has_parser=0
has_types=0
has_cli=0
has_compiler_pipeline=0
has_runtime_net=0
has_observe=0
has_runtime_testkit=0
has_vertical_slice=0
has_hew_tests=0
has_scripts_config=0
has_wasm=0
needs_codegen_release_smoke=0
needs_stdlib_lint=0
needs_hew_suite=0
needs_sandbox_fixture_check=0
needs_sandbox_parity=0
needs_trap_fixtures=0

for path in "${CHANGED_FILES[@]}"; do
    case "$path" in
        std/*)
            # .hew sources under std/net/* still need stdlib-lint (int-surface / errno-gate);
            # only Rust files there are fully covered by the runtime-net lane.
            case "$path" in
                *.hew)
                    needs_stdlib_lint=1
                    # Any .hew stdlib change can affect test-hew or test-stdlib
                    # outcomes; run both suites via the ratchet so regressions
                    # surface before push.
                    needs_hew_suite=1
                    ;;
                *)
                    if ! is_stdlib_net_path "$path"; then
                        needs_stdlib_lint=1
                    fi
                    ;;
            esac
            ;;
    esac

    # Parallel side-channel: trap-fixture paths set needs_trap_fixtures regardless
    # of which primary bucket claims the path.  This mirrors the needs_hew_suite /
    # needs_stdlib_lint pattern — the flag appends make fuzz-oracle after the
    # lane body without changing the lane selector itself.
    if is_trap_fixtures_path "$path"; then
        needs_trap_fixtures=1
    fi

    if is_sandbox_parity_path "$path"; then
        has_scripts_config=1
        needs_sandbox_parity=1
    elif is_sandbox_fixture_path "$path"; then
        has_scripts_config=1
        needs_sandbox_fixture_check=1
    elif is_grammar_path "$path"; then
        has_grammar=1
    elif is_docs_path "$path"; then
        continue
    elif is_scripts_config_path "$path"; then
        has_scripts_config=1
    elif is_parser_path "$path"; then
        has_parser=1
    elif is_types_path "$path"; then
        has_types=1
    elif is_compiler_pipeline_path "$path"; then
        has_compiler_pipeline=1
    elif is_cli_path "$path"; then
        has_cli=1
    elif is_observe_path "$path"; then
        has_observe=1
    elif is_runtime_path "$path" || is_hew_lib_path "$path" || is_stdlib_net_path "$path" || is_analysis_path "$path" || is_lsp_path "$path"; then
        has_runtime_net=1
    elif is_runtime_testkit_path "$path"; then
        has_runtime_testkit=1
    elif is_vertical_slice_path "$path"; then
        has_vertical_slice=1
    elif is_hew_tests_path "$path"; then
        has_hew_tests=1
    elif is_wasm_path "$path"; then
        has_wasm=1
        needs_sandbox_fixture_check=1
    else
        # Fail closed for repo areas without a proven narrow target, such as
        # tests/corpus, tools, installers, editors, and hew-observe/test-harness.
        fallback_lane=1
    fi
done

compiler_related=0
if (( has_compiler_pipeline == 1 || has_vertical_slice == 1 )); then
    compiler_related=1
fi

if (( compiler_related == 1 )); then
    # HIR/MIR/codegen and vertical-slice fixture changes are part of the same
    # end-to-end compiler ladder.  Keep mixed parser/types/CLI edits narrow by
    # running the compiler-pipeline lane instead of falling back solely because
    # adjacent compiler stages changed together.
    has_parser=0
    has_types=0
    has_cli=0
fi

runtime_related=0
if (( has_runtime_net == 1 || has_observe == 1 || has_runtime_testkit == 1 )); then
    runtime_related=1
fi

bucket_count=$((has_grammar + has_parser + has_types + has_cli + compiler_related + runtime_related + has_hew_tests + has_scripts_config + has_wasm))

if (( fallback_lane == 1 )); then
    LANE="fallback"
    LANE_REASON="changed files extend beyond the first-slice targeted buckets"
elif (( bucket_count == 0 )); then
    LANE="docs"
    LANE_REASON="docs-only change"
elif (( bucket_count > 1 )); then
    # Before falling back unconditionally, check for narrow multi-bucket
    # combinations where the union of narrow targets provably covers all
    # changed crates' reverse-dep closure.  Only promote when every bucket
    # in the set is covered by a known-complete narrow target.
    #
    # Conservative invariant: any unrecognised combination falls back.
    # Every promoted combination must have a dispatcher test case.
    #
    #   parser + types:  test-types runs hew-types + hew-parser + hew-lexer,
    #     covering both buckets.  Dependency closure: types ← parser ← lexer
    #     (no runtime/codegen downstream in this narrow set).
    if (( has_parser == 1 && has_types == 1 && bucket_count == 2 )); then
        LANE="types"
        LANE_REASON="parser + type-checker changed; test-types covers both (parser + types dep closure)"
    else
        LANE="fallback"
        LANE_REASON="multiple targeted buckets changed; keeping the first slice conservative"
    fi
elif (( has_scripts_config == 1 )); then
    LANE="scripts-config"
    LANE_REASON="build / scripts / workflow configuration changed"
elif (( has_grammar == 1 )); then
    LANE="grammar"
    LANE_REASON="grammar/spec inputs changed"
elif (( has_parser == 1 )); then
    LANE="parser"
    LANE_REASON="parser/frontend surface changed"
elif (( has_types == 1 )); then
    LANE="types"
    LANE_REASON="type-checker surface changed"
elif (( compiler_related == 1 )); then
    if (( has_compiler_pipeline == 1 )); then
        LANE="compiler-pipeline"
        LANE_REASON="HIR / MIR / codegen compiler pipeline changed"
    else
        LANE="vertical-slice"
        LANE_REASON="vertical-slice fixtures changed"
    fi
elif (( runtime_related == 1 )); then
    if (( has_observe == 1 && has_runtime_net == 0 && has_runtime_testkit == 0 )); then
        LANE="observe"
        LANE_REASON="hew-observe runtime observability surface changed"
    elif (( has_runtime_testkit == 1 && has_runtime_net == 0 && has_observe == 0 )); then
        LANE="runtime-testkit"
        LANE_REASON="runtime testkit surface changed"
    else
        LANE="runtime-net"
        LANE_REASON="runtime / std/net / analysis / lsp / observability surface changed"
    fi
elif (( has_hew_tests == 1 )); then
    LANE="hew-tests"
    LANE_REASON="Hew test files changed"
elif (( has_wasm == 1 )); then
    LANE="wasm"
    LANE_REASON="hew-wasm browser WASM surface changed"
else
    LANE="cli"
    LANE_REASON="CLI surface changed"
fi

case "$LANE" in
    docs)
        ;;
    scripts-config)
        add_command "cargo fmt --all -- --check"
        add_command "make test-rust"
        ;;
    grammar)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "make grammar"
        ;;
    parser)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "make test-parser"
        ;;
    types)
        # Run the full frontend pipeline (hew-types + hew-hir + hew-mir) rather
        # than the narrow make test-types (hew-types + hew-parser + hew-lexer only).
        # A type-checker change can break hew-hir / hew-mir tests that test-types
        # never runs — this was the root cause of #2026 (reject_unbounded_generic_ordering).
        # make test-types remains the iteration target; this lane is the gate.
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "make test-compiler-pipeline"
        add_command "make fuzz-oracle"
        ;;
    cli)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "make test-cli"
        ;;
    compiler-pipeline)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "make test-compiler-pipeline"
        add_command "make test-vertical-slice"
        add_command "make test-pkg-import"
        # fuzz-oracle catches trap signal-code regressions (SIGILL/SIGTRAP) and
        # ratchet mismatches invisible to the nextest workspace run (#2025).
        add_command "make fuzz-oracle"
        # await_e2e is in hew-cli, not covered by test-compiler-pipeline; a codegen
        # change can break the suspend/resume async path visible only via CLI e2e (#2023).
        add_command "cargo nextest run --profile ci -p hew-cli --test await_e2e"
        ;;
    vertical-slice)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "make test-vertical-slice"
        add_command "make test-pkg-import"
        ;;
    observe)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "cargo nextest run --profile ci -p hew-observe"
        ;;
    runtime-testkit)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "cargo nextest run --profile ci -p hew-runtime-testkit -p hew-runtime"
        ;;
    hew-tests)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "make test-hew-ratchet"
        add_command "make test-stdlib-ratchet"
        ;;
    runtime-net)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "make stdlib"
        add_command "scripts/check-libhew-fresh.sh"
        add_command "make test-runtime-net"
        # Runtime ABI changes (e.g. HewCont layout / continuation resume protocol)
        # are invisible to rlib unit tests alone — a Rust unit test can pass over a
        # garbage codegen path.  Run the compiled .hew suites so a local preflight
        # catches the same class of breakage that CI's fallback lane would catch.
        add_command "make test-hew-ratchet"
        add_command "make test-vertical-slice"
        # await_e2e covers the suspend/resume crash-recovery path; this lane owns
        # the runtime surface where that breakage originates (#2023).
        add_command "cargo nextest run --profile ci -p hew-cli --test await_e2e"
        # fuzz-oracle catches trap signal-code regressions visible via the runtime.
        add_command "make fuzz-oracle"
        if (( has_observe == 1 )); then
            add_command "cargo nextest run --profile ci -p hew-observe"
        fi
        if (( has_runtime_testkit == 1 )); then
            add_command "cargo nextest run --profile ci -p hew-runtime-testkit"
        fi
        ;;
    wasm)
        # hew-wasm/* changes: run the WASM lib tests and the playground build
        # (which includes wasm-pack --release and the curated-manifest smoke test).
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy --workspace --tests -- -D warnings"
        add_command "cargo test -p hew-wasm --lib"
        add_command "make playground-check"
        ;;
    fallback)
        # Smoke tier first: fast fail-early on format, clippy, and deterministic
        # in-process oracles (including fmt_roundtrip_corpus) before committing
        # to the heavy build + E2E suite.  The smoke tier completes in <5 min and
        # catches the most common regression classes (bad formatting, clippy
        # violations, formatter-AST bugs) without building and running test-codegen.
        #
        # On smoke pass, the heavy tier runs in full — no coverage is dropped.
        # make lint follows smoke because it adds hew-fmt-check (Hew file
        # formatting), runtime-poison-safe-lint, lint-wasm-todo, and verify-ffi
        # which the smoke tier does not include.
        #
        # Hew-language suites run after the Rust workspace to keep the ratchet
        # verdict separate from the Rust test verdict.
        #
        # The sequence below mirrors CI's build-and-test job exactly so a green
        # fallback preflight predicts a green merge-queue outcome.
        add_command "make ci-preflight-smoke"
        add_command "make lint"
        add_command "make playground-check"
        add_command "make test"
        add_command "make test-vertical-slice"
        add_command "make test-pkg-import"
        add_command "make fuzz-oracle"
        add_command "make test-hew-ratchet"
        add_command "make test-stdlib-ratchet"
        add_command "make sandbox-parity"
        ;;
    *)
        die "unhandled lane: $LANE"
        ;;
esac

if (( needs_codegen_release_smoke == 1 )); then
    # Build the release binary and run a hew run smoke test.  This specifically
    # guards against process-exit aborts (e.g. static std::regex locale init
    # crossing libc++ ABI boundaries — issue #1606) that only surface in
    # release builds and are invisible to unit tests and debug builds.
    add_command "make test-release-binary"
fi

if (( needs_stdlib_lint == 1 )); then
    add_command "make stdlib-lint"
fi

if (( needs_hew_suite == 1 )) && [[ "$LANE" != "fallback" && "$LANE" != "hew-tests" ]]; then
    # A .hew file under std/ changed: run both suites through their ratchets.
    # This catches breakage in test-hew (which imports stdlib) and in test-stdlib
    # (type-check of the modified file itself) before the diff reaches CI.
    # Skip when LANE is fallback or hew-tests: both already include the ratchets.
    add_command "make test-hew-ratchet"
    add_command "make test-stdlib-ratchet"
fi

if (( needs_sandbox_fixture_check == 1 )); then
    add_command "make sandbox-fixtures-check"
fi

if (( needs_sandbox_parity == 1 )); then
    add_command "make sandbox-parity"
fi

if (( needs_trap_fixtures == 1 )) && [[ "$LANE" != "fallback" && "$LANE" != "compiler-pipeline" && "$LANE" != "types" && "$LANE" != "runtime-net" ]]; then
    # MIR bounds/trap lowering or fuzz-oracle corpus changed in a lane that does
    # not already include fuzz-oracle.  Append it so the trap signal-code ratchet
    # runs before push regardless of the primary lane selected.
    add_command "make fuzz-oracle"
fi

# Test-only override for dispatcher command execution. This keeps failure-policy
# tests deterministic without widening the public command-substitution surface.
if [[ -n "${PREFLIGHT_TEST_COMMANDS:-}" ]]; then
    if [[ "${PREFLIGHT_TEST_ALLOW_OVERRIDE:-}" != "1" ]]; then
        die "PREFLIGHT_TEST_COMMANDS requires PREFLIGHT_TEST_ALLOW_OVERRIDE=1 for test-only use; unset PREFLIGHT_TEST_COMMANDS to run the normal preflight."
    fi
    echo "warning: PREFLIGHT_TEST_COMMANDS override active (test-only); replacing dispatcher command list." >&2
    COMMANDS=()
    while IFS= read -r test_cmd; do
        [[ -n "$test_cmd" ]] || continue
        add_command "$test_cmd"
    done <<< "$PREFLIGHT_TEST_COMMANDS"
fi

echo "==> Hew CI preflight dispatcher"
if (( EXPLICIT_PATHS == 1 )); then
    echo "Source: explicit paths"
else
    if [[ -n "$BASE_REF" ]]; then
        echo "Source: branch diff + working tree"
        echo "Base ref: $BASE_REF"
    else
        echo "Source: working tree"
    fi
fi
case "$LANE" in
    docs)
        PROFILE_LABEL="docs-only"
        ;;
    scripts-config)
        PROFILE_LABEL="scripts-config"
        ;;
    grammar)
        PROFILE_LABEL="grammar"
        ;;
    parser)
        PROFILE_LABEL="parser"
        ;;
    types)
        PROFILE_LABEL="types"
        ;;
    cli)
        PROFILE_LABEL="cli"
        ;;
    compiler-pipeline)
        PROFILE_LABEL="compiler-pipeline"
        ;;
    vertical-slice)
        PROFILE_LABEL="vertical-slice"
        ;;
    observe)
        PROFILE_LABEL="observe"
        ;;
    runtime-testkit)
        PROFILE_LABEL="runtime-testkit"
        ;;
    hew-tests)
        PROFILE_LABEL="hew-tests"
        ;;
    runtime-net)
        PROFILE_LABEL="runtime-net"
        ;;
    fallback)
        PROFILE_LABEL="comprehensive"
        ;;
    *)
        PROFILE_LABEL="$LANE"
        ;;
esac

# Resolve the per-command timeout budget for this lane.
case "$LANE" in
    docs)
        CMD_TIMEOUT="$PREFLIGHT_TIMEOUT_DOCS"
        ;;
    fallback)
        CMD_TIMEOUT="$PREFLIGHT_TIMEOUT_FALLBACK"
        ;;
    compiler-pipeline|vertical-slice|hew-tests|scripts-config)
        CMD_TIMEOUT="$PREFLIGHT_TIMEOUT_FALLBACK"
        ;;
    *)
        CMD_TIMEOUT="$PREFLIGHT_TIMEOUT_NARROW"
        ;;
esac

echo "Selected profile: $PROFILE_LABEL"
if (( FAIL_FAST == 1 )); then
    echo "Failure policy: fail-fast"
else
    echo "Failure policy: run-all (default)"
fi
echo "Reason: $LANE_REASON"
echo "Changed files:"
for path in "${CHANGED_FILES[@]}"; do
    echo "  - $path"
done

if ! has_commands; then
    echo "Commands: none (docs-only)"
    if (( DRY_RUN == 1 )); then
        echo "Dry run: no commands executed."
    fi
    exit 0
fi

echo "Commands:"
for cmd in "${COMMANDS[@]}"; do
    if (( DRY_RUN == 1 )); then
        echo "  - $cmd  (budget: ${CMD_TIMEOUT}s)"
    else
        echo "  - $cmd"
    fi
done

if (( DRY_RUN == 1 )); then
    echo "Dry run: no commands executed."
    exit 0
fi

# Execution — per-command timing with process-group-safe outer timeout.
#
# run_timed_command delegates to run_in_pgroup_with_timeout (scripts/lib/timeout.sh)
# which forks each command into its own process group via perl setpgid so that
# a timeout kills the entire tree — bash -lc, cargo, make, and all grandchildren
# — not just the direct child.  This prevents artifact-directory lock contention
# when cargo is orphaned by a timeout that only kills the bash wrapper.
#
# Exit-code contract: SIGTERM → 143 (128+15), SIGKILL → 137 (128+9).  These
# raw signal codes deliberately differ from run_with_timeout's translated 124/137;
# the ==> TIMEOUT: message and the dispatcher python tests both key on 143||137.

PREFLIGHT_OVERALL_START=$SECONDS

# Accumulator for --profile-json output.
_json_entries=()

_elapsed_s=0

run_timed_command() {
    local cmd="$1"
    local start=$SECONDS
    local status=0

    echo ""
    echo "==> $cmd"

    run_in_pgroup_with_timeout "$CMD_TIMEOUT" "$cmd" || status=$?

    _elapsed_s=$(( SECONDS - start ))

    # Timeout exit codes from the watchdog:
    #   143 = SIGTERM (128+15): watchdog's initial soft kill reached the child
    #   137 = SIGKILL (128+9): watchdog's hard-kill fallback fired
    if [[ "$status" -eq 137 || "$status" -eq 143 ]]; then
        echo "==> TIMEOUT: '$cmd' exceeded ${CMD_TIMEOUT}s budget and was killed."
    fi

    if [[ "$status" -ne 0 ]]; then
        echo "<-- $cmd  elapsed ${_elapsed_s}s  FAILED (exit $status)"
    else
        echo "<-- $cmd  elapsed ${_elapsed_s}s  ok"
    fi

    # Accumulate JSON entry.
    _json_entries+=("{\"cmd\":$(printf '%s' "$cmd" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))'),\"elapsed_s\":${_elapsed_s},\"status\":${status}}")

    return "$status"
}

PREFLIGHT_FAILURES=()
PREFLIGHT_EXECUTED_COMMANDS=()
PREFLIGHT_CMD_ELAPSED=()
PREFLIGHT_CMD_STATUS=()
STOPPED_EARLY=0
for cmd in "${COMMANDS[@]}"; do
    status=0
    if run_timed_command "$cmd"; then
        status=0
    else
        status=$?
        PREFLIGHT_FAILURES+=("$cmd")
    fi

    PREFLIGHT_EXECUTED_COMMANDS+=("$cmd")
    PREFLIGHT_CMD_ELAPSED+=("$_elapsed_s")
    PREFLIGHT_CMD_STATUS+=("$status")

    if (( status != 0 && FAIL_FAST == 1 )); then
        STOPPED_EARLY=1
        echo "==> Stopping after first failed command (--fail-fast)."
        break
    fi
done

PREFLIGHT_OVERALL_ELAPSED=$(( SECONDS - PREFLIGHT_OVERALL_START ))

# Summary table.
echo ""
echo "==> Preflight summary (${PREFLIGHT_OVERALL_ELAPSED}s total)"
i=0
for cmd in "${PREFLIGHT_EXECUTED_COMMANDS[@]+"${PREFLIGHT_EXECUTED_COMMANDS[@]}"}"; do
    elapsed="${PREFLIGHT_CMD_ELAPSED[$i]:-?}"
    status="${PREFLIGHT_CMD_STATUS[$i]:-0}"
    status_label="ok"
    if [[ "$status" -ne 0 ]]; then
        status_label="FAILED"
    fi
    printf "    %s  %ss  [%s]\n" "$cmd" "$elapsed" "$status_label"
    (( i++ )) || true
done

if (( STOPPED_EARLY == 1 )); then
    echo "    ... remaining commands not run"
fi

# Write --profile-json if requested.
if [[ -n "$PROFILE_JSON_PATH" ]]; then
    {
        printf '['
        first=1
        for entry in "${_json_entries[@]+"${_json_entries[@]}"}"; do
            if (( first == 0 )); then printf ','; fi
            printf '%s' "$entry"
            first=0
        done
        printf ']\n'
    } > "$PROFILE_JSON_PATH"
    echo "Timing profile written to: $PROFILE_JSON_PATH"
fi

if [[ ${#PREFLIGHT_FAILURES[@]} -gt 0 ]]; then
    echo ""
    echo "==> Preflight FAILED — ${#PREFLIGHT_FAILURES[@]} command(s) did not pass:"
    for failed in "${PREFLIGHT_FAILURES[@]}"; do
        echo "    - $failed"
    done
    exit 1
fi

echo "==> Preflight passed."
