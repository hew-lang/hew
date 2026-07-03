#!/usr/bin/env bash
# scripts/lane-gates.sh — mechanical per-lane gate battery.
#
# A fast, developer-invoked self-check an implementer runs BEFORE
# `make ci-preflight`, mechanising three defect classes that were previously
# caught only by review:
#
#   1. Workspace-wide clippy misses  — a per-crate-only local check hides
#      cross-crate lint fallout; this script always runs
#      `cargo clippy --workspace --tests -- -D warnings`, never scoped to
#      `-p <crate>`.
#   2. Literal `\n` / `\n\n` commit bodies — a commit made with `-m 'Subject\n\nBody'`
#      inside single quotes puts a raw two-character backslash-n sequence into
#      the commit object instead of a real newline. Reproduced 3x across Copilot
#      lanes; this script automates the manual `git log -1 --format=%B | cat -v`
#      eyeball check that caught it.
#   3. Sibling-gate misses (the trap-fixture two-gate) — a fixture added to
#      tests/vertical-slice/accept/ without a matching entry in
#      tests/fuzz-oracle/expected-failures.txt passes the vertical-slice gate
#      but fails `make fuzz-oracle` in CI. See LESSONS.md `trap-fixture-two-gate`.
#      This script runs the real `make fuzz-oracle` ratchet, not a
#      reimplementation of it.
#
# Not a preflight replacement. `make ci-preflight` (scripts/ci-preflight-dispatcher.sh)
# remains the required pre-push gate; this is a narrower, faster, standalone
# tool a developer runs mid-lane, before that gate.
#
# Usage:
#   scripts/lane-gates.sh -p <crate> [-p <crate> ...] [--base <ref>]
#                          [--skip-oracle] [--fail-fast] [--dry-run] [--help|-h]
#
# At least one -p <crate> is required.
#
# Battery (fixed order, cheapest/most-likely-to-fail first):
#   1. cargo fmt --all -- --check
#   2. cargo clippy --workspace --tests -- -D warnings
#   3. cargo nextest run --profile lane -p <crate1> [-p <crate2> ...]
#   4. commit-body lint (orchestration-framing delegation + literal-\n +
#      tombstone-word checks over ${BASE_REF}..HEAD)
#   5. make fuzz-oracle (skippable via --skip-oracle)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# shellcheck source=scripts/lib/timeout.sh
# shellcheck disable=SC1091
source "${REPO_ROOT}/scripts/lib/timeout.sh"

LANE_GATES_TIMEOUT_FMT="${LANE_GATES_TIMEOUT_FMT:-60}"
LANE_GATES_TIMEOUT_CLIPPY="${LANE_GATES_TIMEOUT_CLIPPY:-300}"
LANE_GATES_TIMEOUT_TESTS="${LANE_GATES_TIMEOUT_TESTS:-180}"
LANE_GATES_TIMEOUT_ORACLE="${LANE_GATES_TIMEOUT_ORACLE:-600}"

CRATES=()
BASE_REF_ARG=""
SKIP_ORACLE=0
FAIL_FAST=0
DRY_RUN=0

usage() {
    cat <<'EOF'
Usage: scripts/lane-gates.sh -p <crate> [-p <crate> ...] [--base <ref>] [--skip-oracle] [--fail-fast] [--dry-run] [--help|-h]

Mechanical per-lane gate battery: fmt-check, workspace clippy, targeted crate
tests, commit-body lint, and the fuzz-oracle two-gate ratchet check. Run this
before `make ci-preflight`, not instead of it.

  -p <crate>       Crate to run `cargo nextest run --profile lane -p <crate>`
                    against. Repeatable; at least one is required.
  --base <ref>     Base ref for the commit-body lint range (<ref>..HEAD).
                    Overrides the CI_PREFLIGHT_BASE / origin/main / main
                    resolution chain.
  --skip-oracle    Skip the make fuzz-oracle step (prints SKIPPED, not run).
  --fail-fast      Stop after the first failing step. Default: run all steps.
  --dry-run        Print the resolved crate list, base ref, commit range, and
                    ordered step list with budgets; exit 0 without running
                    anything.
  --help, -h       Show this help and exit 0.
EOF
}

die() {
    echo "error: $*" >&2
    exit 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -p)
            shift
            [[ $# -gt 0 ]] || die "-p requires a crate name"
            CRATES+=("$1")
            shift
            ;;
        --base)
            shift
            [[ $# -gt 0 ]] || die "--base requires a ref"
            BASE_REF_ARG="$1"
            shift
            ;;
        --skip-oracle)
            SKIP_ORACLE=1
            shift
            ;;
        --fail-fast)
            FAIL_FAST=1
            shift
            ;;
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        *)
            die "unknown option: $1"
            ;;
    esac
done

# ── BASE_REF resolution (decision 6) ──────────────────────────────────────────
# Used only to compute the commit-body lint's range. Resolved once, at startup,
# before any step (including --dry-run) runs — an unresolvable base makes the
# whole invocation unusable, not just the commit-lint row.
resolve_base_ref() {
    if [[ -n "$BASE_REF_ARG" ]]; then
        if ! git rev-parse --verify "$BASE_REF_ARG" >/dev/null 2>&1; then
            die "unknown base ref: $BASE_REF_ARG"
        fi
        printf '%s\n' "$BASE_REF_ARG"
        return 0
    fi
    if [[ -n "${CI_PREFLIGHT_BASE:-}" ]] && git rev-parse --verify "$CI_PREFLIGHT_BASE" >/dev/null 2>&1; then
        printf '%s\n' "$CI_PREFLIGHT_BASE"
        return 0
    fi
    if git rev-parse --verify origin/main >/dev/null 2>&1; then
        printf 'origin/main\n'
        return 0
    fi
    if git rev-parse --verify main >/dev/null 2>&1; then
        printf 'main\n'
        return 0
    fi
    return 1
}

if ! BASE_REF="$(resolve_base_ref)"; then
    echo "error: lane-gates: no resolvable base ref (tried --base, CI_PREFLIGHT_BASE, origin/main, main)." >&2
    echo "  Pass an explicit ref: scripts/lane-gates.sh -p <crate> --base <ref>" >&2
    exit 2
fi
RANGE="${BASE_REF}..HEAD"

if [[ ${#CRATES[@]} -eq 0 ]]; then
    die "at least one -p <crate> is required"
fi

# ── Commit-body lint (Slice 2 replaces this stub) ─────────────────────────────
# TODO(lane-gates Slice 2): replace with the real 3-sub-check implementation
# (orchestration-framing delegation, literal-\n check, tombstone-word check).
run_commit_lint() {
    return 0
}

# ── Step list (decision 4) ────────────────────────────────────────────────────
NEXTEST_ARGS=()
for c in "${CRATES[@]}"; do
    NEXTEST_ARGS+=("-p" "$c")
done
NEXTEST_CMD="cargo nextest run --profile lane ${NEXTEST_ARGS[*]}"

if (( DRY_RUN == 1 )); then
    echo "==> lane-gates dry run"
    echo "Crates: ${CRATES[*]}"
    echo "Base ref: $BASE_REF"
    echo "Commit range: $RANGE"
    echo "Steps:"
    echo "  1. cargo fmt --all -- --check  (budget: ${LANE_GATES_TIMEOUT_FMT}s)"
    echo "  2. cargo clippy --workspace --tests -- -D warnings  (budget: ${LANE_GATES_TIMEOUT_CLIPPY}s)"
    echo "  3. ${NEXTEST_CMD}  (budget: ${LANE_GATES_TIMEOUT_TESTS}s)"
    echo "  4. commit-body lint (${RANGE})  (not yet implemented)"
    if (( SKIP_ORACLE == 1 )); then
        echo "  5. make fuzz-oracle  SKIPPED (--skip-oracle)"
    else
        echo "  5. make fuzz-oracle  (budget: ${LANE_GATES_TIMEOUT_ORACLE}s)"
    fi
    echo "Dry run: no commands executed."
    exit 0
fi

LANE_GATES_LABELS=()
LANE_GATES_ELAPSED=()
LANE_GATES_STATUS=()
LANE_GATES_FAILURES=()
OVERALL_START=$SECONDS

run_step() {
    local label="$1"
    local cmd="$2"
    local budget="$3"
    local start=$SECONDS
    local status=0

    echo ""
    echo "==> $label"
    run_in_pgroup_with_timeout "$budget" "$cmd" || status=$?
    local elapsed=$(( SECONDS - start ))

    if [[ "$status" -eq 137 || "$status" -eq 143 ]]; then
        echo "==> TIMEOUT: '$label' exceeded ${budget}s budget and was killed."
    fi
    if [[ "$status" -ne 0 ]]; then
        echo "<-- $label  elapsed ${elapsed}s  FAILED (exit $status)"
        LANE_GATES_FAILURES+=("$label")
    else
        echo "<-- $label  elapsed ${elapsed}s  ok"
    fi

    LANE_GATES_LABELS+=("$label")
    LANE_GATES_ELAPSED+=("$elapsed")
    LANE_GATES_STATUS+=("$status")
    return "$status"
}

should_run_next() {
    [[ "$FAIL_FAST" -eq 0 || ${#LANE_GATES_FAILURES[@]} -eq 0 ]]
}

if should_run_next; then
    run_step "cargo fmt --all -- --check" "cargo fmt --all -- --check" "$LANE_GATES_TIMEOUT_FMT" || true
fi
if should_run_next; then
    run_step "cargo clippy --workspace --tests -- -D warnings" "cargo clippy --workspace --tests -- -D warnings" "$LANE_GATES_TIMEOUT_CLIPPY" || true
fi
if should_run_next; then
    run_step "$NEXTEST_CMD" "$NEXTEST_CMD" "$LANE_GATES_TIMEOUT_TESTS" || true
fi

# Commit-body lint: manual SECONDS timing, no process-group wrapper (decision 9)
# — it only calls git log / perl / grep against a bounded commit range, which
# cannot hang absent a corrupt repo, unlike cargo/make.
if should_run_next; then
    _lint_label="commit-body lint (${RANGE})"
    echo ""
    echo "==> $_lint_label"
    _lint_start=$SECONDS
    _lint_status=0
    run_commit_lint "$RANGE" || _lint_status=$?
    _lint_elapsed=$(( SECONDS - _lint_start ))
    if [[ "$_lint_status" -ne 0 ]]; then
        echo "<-- $_lint_label  elapsed ${_lint_elapsed}s  FAILED (exit $_lint_status)"
        LANE_GATES_FAILURES+=("$_lint_label")
    else
        echo "<-- $_lint_label  elapsed ${_lint_elapsed}s  ok"
    fi
    LANE_GATES_LABELS+=("$_lint_label")
    LANE_GATES_ELAPSED+=("$_lint_elapsed")
    LANE_GATES_STATUS+=("$_lint_status")
fi

# make fuzz-oracle — skippable, still recorded in the summary as SKIPPED.
if (( SKIP_ORACLE == 1 )); then
    LANE_GATES_LABELS+=("make fuzz-oracle")
    LANE_GATES_ELAPSED+=(0)
    LANE_GATES_STATUS+=(0)
    echo ""
    echo "==> make fuzz-oracle"
    echo "<-- make fuzz-oracle  SKIPPED (--skip-oracle)"
elif should_run_next; then
    run_step "make fuzz-oracle" "make fuzz-oracle" "$LANE_GATES_TIMEOUT_ORACLE" || true
fi

OVERALL_ELAPSED=$(( SECONDS - OVERALL_START ))

echo ""
echo "==> lane-gates summary (${OVERALL_ELAPSED}s total)"
i=0
for label in "${LANE_GATES_LABELS[@]}"; do
    elapsed="${LANE_GATES_ELAPSED[$i]}"
    status="${LANE_GATES_STATUS[$i]}"
    status_label="ok"
    if [[ "$label" == "make fuzz-oracle" && "$SKIP_ORACLE" -eq 1 ]]; then
        status_label="SKIPPED"
    elif [[ "$status" -ne 0 ]]; then
        status_label="FAILED"
    fi
    printf "    %s  %ss  [%s]\n" "$label" "$elapsed" "$status_label"
    (( i++ )) || true
done

if [[ ${#LANE_GATES_FAILURES[@]} -gt 0 ]]; then
    echo ""
    echo "==> lane-gates verdict: FAIL — ${#LANE_GATES_FAILURES[@]} check(s) did not pass:"
    for failed in "${LANE_GATES_FAILURES[@]}"; do
        echo "    - $failed"
    done
    exit 1
fi

echo ""
echo "==> lane-gates verdict: PASS"
