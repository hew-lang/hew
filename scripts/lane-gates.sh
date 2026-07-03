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
SELF_TEST=0

usage() {
    cat <<'EOF'
Usage: scripts/lane-gates.sh -p <crate> [-p <crate> ...] [--base <ref>] [--skip-oracle] [--fail-fast] [--dry-run] [--self-test] [--help|-h]

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
  --self-test      Run the commit-body lint's isolated self-test suite and exit.
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
        --self-test)
            SELF_TEST=1
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

if (( SELF_TEST == 0 )); then
    if ! BASE_REF="$(resolve_base_ref)"; then
        echo "error: lane-gates: no resolvable base ref (tried --base, CI_PREFLIGHT_BASE, origin/main, main)." >&2
        echo "  Pass an explicit ref: scripts/lane-gates.sh -p <crate> --base <ref>" >&2
        exit 2
    fi
    RANGE="${BASE_REF}..HEAD"

    if [[ ${#CRATES[@]} -eq 0 ]]; then
        die "at least one -p <crate> is required"
    fi
fi

# ── Commit-body lint ───────────────────────────────────────────────────────────
# Three sub-checks over every commit in $1 (a "<ref>..HEAD"-shaped range):
#   (a) orchestration framing — delegates to lint-orchestration-leak.sh, does
#       not reimplement its T1-T10 patterns.
#   (b) literal `\n` — fixed-string match for the two-byte backslash+n
#       sequence, via `cat -v` (matches the manual proof method that found
#       this defect 3x: ledger `\n\n` commit-body evidence).
#   (c) tombstone words — case-insensitive `used to be` / `legacy` only.
#       Deliberately narrower than U134's full sweep pattern (measured ~43%
#       false-positive rate on "before the" alone) and narrower still to
#       avoid conflicting with the mandatory bug-fix commit template, which
#       requires describing prior-wrong behaviour ("previously this trapped
#       incorrectly", "no longer accepts negative values" are legitimate).
# Aggregate verdict: fail if ANY sub-check fails for ANY commit in range.
run_commit_lint() {
    local range="$1"
    local fail=0
    local leak_scan_out
    leak_scan_out="$(mktemp)"

    if ! bash "${REPO_ROOT}/scripts/lint-orchestration-leak.sh" --scan-commits "$range" >"$leak_scan_out" 2>&1; then
        fail=1
        echo "  ${range}: (a) orchestration framing: FAILED"
        sed 's/^/    /' "$leak_scan_out" >&2
    fi
    rm -f "$leak_scan_out"

    local sha cat_v_msg raw_msg
    while IFS= read -r sha; do
        [[ -n "$sha" ]] || continue
        cat_v_msg="$(git log -1 --format='%B' "$sha" | cat -v)"
        if printf '%s' "$cat_v_msg" | grep -qF '\n'; then
            fail=1
            echo "  ${sha}: (b) literal backslash-n escape found in commit body"
        fi
        # Tombstone check runs against the raw %B text (not the cat -v output):
        # tombstone detection has nothing to do with non-printing bytes.
        raw_msg="$(git log -1 --format='%B' "$sha")"
        if printf '%s' "$raw_msg" | grep -qiE '\bused to be\b'; then
            fail=1
            echo "  ${sha}: (c) tombstone word 'used to be' found"
        fi
        if printf '%s' "$raw_msg" | grep -qiE '\blegacy\b'; then
            fail=1
            echo "  ${sha}: (c) tombstone word 'legacy' found"
        fi
    done < <(git log --format='%H' "$range" 2>/dev/null)

    return "$fail"
}

# ── Self-test ──────────────────────────────────────────────────────────────────
# Same isolated-tmp-repo scaffolding pattern as lint-orchestration-leak.sh's own
# --self-test (init_repo, assert_commit_detects/assert_commit_clean helpers).
if (( SELF_TEST == 1 )); then
    _tmpdir=$(mktemp -d)
    trap 'rm -rf "$_tmpdir"' EXIT
    _pass=0
    _fail=0

    # Initial branch is deliberately NOT "main": lane-gates.sh's fallback chain
    # resolves a same-repo "main" branch, so a repo whose default branch is
    # literally named "main" (git's own init.defaultBranch default) would make
    # the fail-closed assertion below trivially pass for the wrong reason.
    init_repo() {
        git -C "$_tmpdir" init -q --initial-branch=scratch
        git -C "$_tmpdir" config user.email "test@test"
        git -C "$_tmpdir" config user.name "test"
        git -C "$_tmpdir" commit --allow-empty -m "root"
    }

    assert_commit_detects() {
        local desc="$1"
        local msg="$2"
        git -C "$_tmpdir" commit --allow-empty -m "$msg" 2>/dev/null
        if (cd "$_tmpdir" && run_commit_lint "HEAD~1..HEAD" >/dev/null 2>&1); then
            echo "FAIL: $desc — expected non-zero exit but got zero"
            _fail=$((_fail + 1))
        else
            echo "PASS: $desc"
            _pass=$((_pass + 1))
        fi
        git -C "$_tmpdir" reset --hard HEAD~1 >/dev/null 2>&1
    }

    assert_commit_clean() {
        local desc="$1"
        local msg="$2"
        git -C "$_tmpdir" commit --allow-empty -m "$msg" 2>/dev/null
        if (cd "$_tmpdir" && run_commit_lint "HEAD~1..HEAD" >/dev/null 2>&1); then
            echo "PASS: $desc"
            _pass=$((_pass + 1))
        else
            echo "FAIL: $desc — expected zero exit (clean) but got non-zero"
            _fail=$((_fail + 1))
        fi
        git -C "$_tmpdir" reset --hard HEAD~1 >/dev/null 2>&1
    }

    init_repo

    # literal \n detected: a single-quoted -m string puts a raw backslash-n
    # into the commit object (bash does not expand escapes in single quotes).
    assert_commit_detects "literal backslash-n detected" 'Subject\n\nBody with literal escape'
    # real newlines NOT flagged.
    assert_commit_clean "real newlines not flagged" "$(printf 'Subject\n\nReal body.')"
    # tombstone words detected.
    assert_commit_detects "'used to be' detected" "fix: correct the thing that used to be broken"
    assert_commit_detects "'legacy' detected" "chore: remove legacy fallback path"
    # scoped-narrower vocabulary NOT flagged (decision 8c) — protects the
    # scoping choice from later accidental broadening.
    assert_commit_clean "'previously' not flagged" "fix: previously this trapped incorrectly"
    assert_commit_clean "'no longer' not flagged" "fix: no longer accepts negative values"
    assert_commit_clean "'in v0.5' not flagged" "docs: behaviour changed in v0.5"
    # delegation wiring: a known lint-orchestration-leak.sh token fails via (a),
    # proving the delegation without re-testing the token patterns themselves.
    # Built at runtime (not a literal in this file) so this fixture does not
    # itself trip the leak scan's tracked-source check.
    _leak_token="DIST-$(printf '%d' 4)"
    assert_commit_detects "known leak-scan token fails via delegation" "feat: ${_leak_token} slice — add cddl body"
    # clean commit — no false positive.
    assert_commit_clean "clean commit message" "feat: add robust error propagation"

    # fail-closed: no resolvable base ref and no --base (mirrors
    # lint-orchestration-leak.sh's own --scan-commits fail-closed behaviour).
    # lane-gates.sh resolves REPO_ROOT from its own script path (decision 2,
    # not the caller's cwd), so `cd "$_tmpdir"` alone would not redirect its
    # git calls into the scratch repo — GIT_DIR/GIT_WORK_TREE overrides do.
    _fail_closed_status=0
    GIT_DIR="$_tmpdir/.git" GIT_WORK_TREE="$_tmpdir" \
        bash "${REPO_ROOT}/scripts/lane-gates.sh" -p hew-lexer --dry-run >/dev/null 2>&1 || _fail_closed_status=$?
    if [[ "$_fail_closed_status" -eq 2 ]]; then
        echo "PASS: no-resolvable-base fail-closed"
        _pass=$((_pass + 1))
    else
        echo "FAIL: no-resolvable-base fail-closed — expected exit 2, got ${_fail_closed_status}"
        _fail=$((_fail + 1))
    fi

    echo ""
    echo "lane-gates self-test: ${_pass} passed, ${_fail} failed"
    [[ "$_fail" -eq 0 ]]
    exit $?
fi

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
    echo "  4. commit-body lint (${RANGE})"
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
