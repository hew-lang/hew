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
# Cold artifact construction is intentionally outside the command-tier budgets.
# Keep its watchdog independent and generous, while applying the same
# host-parallelism scaling as every timed preflight step.
PREFLIGHT_TIMEOUT_WARMUP="${PREFLIGHT_TIMEOUT_WARMUP:-900}"

TIMEOUT_CALIBRATION_PARALLELISM=16
HOST_PARALLELISM=1
HOST_PARALLELISM_SOURCE="conservative fallback"
TIMEOUT_SCALE_NUMERATOR="$TIMEOUT_CALIBRATION_PARALLELISM"
TIMEOUT_SCALE_DENOMINATOR=1

detect_host_parallelism() {
    local detected=""

    if command -v nproc >/dev/null 2>&1 && detected="$(nproc 2>/dev/null)" && [[ "$detected" =~ ^[1-9][0-9]*$ ]]; then
        HOST_PARALLELISM="$detected"
        HOST_PARALLELISM_SOURCE="nproc"
    elif command -v sysctl >/dev/null 2>&1 && detected="$(sysctl -n hw.ncpu 2>/dev/null)" && [[ "$detected" =~ ^[1-9][0-9]*$ ]]; then
        HOST_PARALLELISM="$detected"
        HOST_PARALLELISM_SOURCE="sysctl -n hw.ncpu"
    elif command -v getconf >/dev/null 2>&1 && detected="$(getconf _NPROCESSORS_ONLN 2>/dev/null)" && [[ "$detected" =~ ^[1-9][0-9]*$ ]]; then
        HOST_PARALLELISM="$detected"
        HOST_PARALLELISM_SOURCE="getconf _NPROCESSORS_ONLN"
    fi

    if (( HOST_PARALLELISM >= TIMEOUT_CALIBRATION_PARALLELISM )); then
        TIMEOUT_SCALE_NUMERATOR=1
        TIMEOUT_SCALE_DENOMINATOR=1
    else
        TIMEOUT_SCALE_NUMERATOR="$TIMEOUT_CALIBRATION_PARALLELISM"
        TIMEOUT_SCALE_DENOMINATOR="$HOST_PARALLELISM"
    fi
}

scale_timeout_budget() {
    local baseline="$1"
    echo $(( (baseline * TIMEOUT_SCALE_NUMERATOR + TIMEOUT_SCALE_DENOMINATOR - 1) / TIMEOUT_SCALE_DENOMINATOR ))
}

print_timeout_scaling() {
    local factor_hundredths
    factor_hundredths=$(( (TIMEOUT_SCALE_NUMERATOR * 100 + TIMEOUT_SCALE_DENOMINATOR / 2) / TIMEOUT_SCALE_DENOMINATOR ))
    printf 'Host parallelism: %s (%s); timeout scale: max(1, %s / %s) = %d.%02dx; budgets use ceil(baseline * %s / %s), with a finite 16.00x maximum.\n' \
        "$HOST_PARALLELISM" \
        "$HOST_PARALLELISM_SOURCE" \
        "$TIMEOUT_CALIBRATION_PARALLELISM" \
        "$HOST_PARALLELISM" \
        "$(( factor_hundredths / 100 ))" \
        "$(( factor_hundredths % 100 ))" \
        "$TIMEOUT_SCALE_NUMERATOR" \
        "$TIMEOUT_SCALE_DENOMINATOR"
}

# Per-command stuck ceilings from the local preflight timing audit.  These are
# measurement-only hang budgets, not coverage skips or bypasses.  The effective
# timeout is max(command floor, lane tier), so narrow lanes get enough time for
# known-long suites while retaining finite stuck ceilings for every command.
command_timeout_floor() {
    local cmd="$1"
    case "$cmd" in
        "make ci-preflight-smoke") echo 420 ;;
        # A warm lint run measured 698 s on the 16-core developer machine.
        # Add roughly 35% cache and host-load headroom, rounded to 945 s, while
        # retaining a finite stuck ceiling.
        "make lint") echo 945 ;;
        "make playground-check") echo 150 ;;
        # A warm workspace make test run measured 1129 s for 14,050 nextest
        # tests on the 16-core developer machine.  Add roughly 35% cache and
        # host-load headroom, rounded to 1530 s, while retaining a finite stuck
        # ceiling.
        "make test") echo 1530 ;;
        # test-compiler-pipeline carries the hew-cli consumer corpus (compiled
        # leak/drop oracles + e2e suites).  The 600 s figure came from ~234 s
        # on a warm 16-core developer machine; hosted runners have far fewer
        # cores and the corpus has grown, so 600 s killed this gate on CI while
        # it was still healthy.  1800 s is the scale of its sibling compiled
        # corpora (ratchet 1500 s, O2 differential 2700 s) and still bounds a
        # hang.
        "make test-compiler-pipeline") echo 1800 ;;
        "make test-opaque-resource-lifecycle-matrix-external") echo 600 ;;
        "make test-vertical-slice") echo 240 ;;
        "make test-pkg-import") echo 60 ;;
        "make fuzz-oracle") echo 210 ;;
        # Isolated warm timings for 1326 tests were 1105 s for the ratchet and
        # 1988 s for the two-pass differential check.  Keep roughly 35% cold
        # cache and host-load headroom while retaining a finite stuck ceiling.
        "make test-hew-ratchet") echo 1500 ;;
        "make test-core-matrix") echo 600 ;;
        "make test-o2-differential") echo 2700 ;;
        "make o2-differential-selftest") echo 30 ;;
        "make doc-ratchet-selftest") echo 45 ;;
        "make test-release-workflow-contract") echo 30 ;;
        "make test-stdlib-ratchet") echo 45 ;;
        "make test-stdlib-execution-proofs") echo 45 ;;
        "make test-doc-examples") echo 45 ;;
        "make sandbox-parity") echo 150 ;;
        "make checked-mir-verify") echo 45 ;;
        "make checked-mir-run") echo 420 ;;
        "make ll-diff") echo 45 ;;
        # Cold-tree headroom for the capability authority ratchet: the crate
        # itself builds in seconds (serde/toml only) but a cold cargo tree
        # still pays the shared-dependency compile before the test runs.
        "cargo nextest run --profile ci -p hew-capability-gen") echo 300 ;;
        "make hew-check-all") echo 300 ;;
        "make hew-fmt-property") echo 600 ;;
        "make test-leak-oracle-selftest") echo 60 ;;
        *) echo 0 ;;
    esac
}

# Balance weight (seconds) used to partition a profile into shards.
#
# The weight is a MEASURED duration, not a hang ceiling: command_timeout_floor
# carries deliberate 35%-and-more headroom (test-compiler-pipeline's 1800 s
# ceiling bounds a ~1243 s gate), so packing against floors would mis-rank the
# long tail.  The values below are the per-command elapsed times from a hosted
# ubuntu-24.04 Linux-gates run of the comprehensive profile: 7823 s total, of
# which 754 s was warm-up and the nine commands named here were 5283 s.  The
# remaining 29 commands each measured at or under 200 s and average about 60 s,
# which is the default; anything with no entry and no floor takes that default.
#
# SHIM SEAM: these are point measurements, refreshed by hand from a
# --profile-json run.  They become obsolete when the dispatcher reads a
# committed timing corpus; until then a drifted weight costs balance, never
# coverage — the partition is exhaustive and disjoint regardless of the weights.
PREFLIGHT_DEFAULT_COMMAND_WEIGHT=60

command_weight() {
    local cmd="$1"
    local floor
    case "$cmd" in
        # Measured on the hosted Linux gates job.
        "make test-compiler-pipeline") echo 1243 ;;
        "make test") echo 1104 ;;
        "make hew-check-all") echo 633 ;;
        "make fuzz-oracle") echo 580 ;;
        "make hew-fmt-property") echo 567 ;;
        "make lint") echo 435 ;;
        "make test-vertical-slice") echo 429 ;;
        "make sandbox-parity") echo 292 ;;
        # Not present in the hosted profile (the compiled-Hew aggregate owns
        # both), so no hosted measurement exists.  Local isolated warm timings
        # were 1105 s for the ratchet and 1988 s for the two-pass differential.
        "make test-hew-ratchet") echo 1105 ;;
        "make test-o2-differential") echo 1988 ;;
        *)
            floor="$(command_timeout_floor "$cmd")"
            if (( floor > 0 && floor < PREFLIGHT_DEFAULT_COMMAND_WEIGHT )); then
                echo "$floor"
            else
                echo "$PREFLIGHT_DEFAULT_COMMAND_WEIGHT"
            fi
            ;;
    esac
}

# Ordering-dependency groups.  Commands that share a group key are ATOMIC for
# sharding: they always land in the same shard, in their original relative
# order, because splitting them would split a producer from its consumer or a
# gate from the self-test that proves the gate has teeth.
#
# Every membership here is a real dependency, not a stylistic pairing:
#   hew-suites   test-hew-ratchet emits HEW_O0_OUTCOMES_FILE, which
#                test-o2-differential consumes as its O0 baseline; the
#                selftest proves that differential gate is falsifiable.
#   fuzz-oracle  the selftest proves run-oracle.py flags real crashes.
#   ll-oracle    ll-identity-selftest proves the ll-diff normaliser is not a
#                no-op that would pass any IR.
#   checked-mir  verify diffs the golden corpus, run executes it; the run
#                verdict is only meaningful next to the byte diff.
#   doc-ratchet  doc-ratchet-selftest proves the doc-example ratchet is
#                falsifiable.
#   libhew       check-libhew-fresh.sh certifies the archive `make stdlib`
#                just produced.
#   lane-nextest `make hew-native wasm-runtime` builds the compiler and WASM
#                runtime that the lane's nextest invocations link against.
command_shard_group() {
    case "$1" in
        "make test-hew-ratchet"|"make test-o2-differential"|"make o2-differential-selftest")
            echo "group:hew-suites" ;;
        "make fuzz-oracle"|"make fuzz-oracle-selftest")
            echo "group:fuzz-oracle" ;;
        "make ll-diff"|"make ll-identity-selftest")
            echo "group:ll-oracle" ;;
        "make checked-mir-verify"|"make checked-mir-run")
            echo "group:checked-mir" ;;
        "make test-doc-examples"|"make doc-ratchet-selftest")
            echo "group:doc-ratchet" ;;
        "make stdlib"|"scripts/check-libhew-fresh.sh")
            echo "group:libhew" ;;
        "make hew-native wasm-runtime"|"cargo nextest run "*)
            echo "group:lane-nextest" ;;
        *)
            echo "cmd:$1" ;;
    esac
}

command_timeout() {
    local cmd="$1"
    local baseline
    local floor
    floor="$(command_timeout_floor "$cmd")"
    if (( floor > CMD_TIMEOUT )); then
        baseline="$floor"
    else
        baseline="$CMD_TIMEOUT"
    fi
    scale_timeout_budget "$baseline"
}

warmup_timeout() {
    scale_timeout_budget "$PREFLIGHT_TIMEOUT_WARMUP"
}

# First-failure extraction.
#
# A failed CI step used to hand back only "exit 1" plus tens of thousands of
# log lines (--log-failed alone is 6-7 MB).  These patterns cover the concrete
# failure shapes this repo's gates actually emit:
#   nextest         "        FAIL [   1.234s] hew-cli::suite test_name"
#   make            "make[1]: *** [Makefile:759: test] Error 2"
#   rustc/clippy    "error[E0382]: ..." / "error: ..."
#   python          "AssertionError: ..." / "ValueError: ..."
#   hew ratchets    "RATCHET FAIL: ..." / "==> Ratchet: FAILED"
#   fuzz oracle     "ORACLE gate: FAIL"
#   O2 differential "==> Differential gate: FAILED — ..."
#   corpus drift    "NOW-FAILS: tests/hew/x.hew" / "NOW-PASSES: ..."
#   golden corpora  "TRANSCRIPT MISMATCH: lambda_actor_lifecycle",
#                   "DUMP DRIFT: x (elab stage)", "MANIFEST DRIFT: ...",
#                   "MISSING/STALE GOLDEN: ...", "MISSING/STALE/ORPHAN
#                   EXPECTATION: ...", "CANNOT CLASSIFY: ...",
#                   "LEAK ORACLE SELFTEST FAILED: ...", "CORPUS FAIL: ...",
#                   and the "<gate>: FAILED" summary line every corpus driver
#                   ends on
#   watchdog        "==> TIMEOUT: 'make test' exceeded 1530s budget"
#   rust panics     "thread 'main' panicked at ..."
#   signal deaths   "Illegal instruction (core dumped)" / "Segmentation fault" /
#                   "Aborted" / "Bus error" / "Trace/breakpoint trap" /
#                   "zsh: trace trap" / "Abort trap: 6", the SIG* spellings
#                   cargo and nextest use ("signal: 4, SIGILL"), and nextest's
#                   "killed by signal 11" form
# The golden-corpus drivers (scripts/checked-mir-corpus.sh, scripts/ll-corpus.sh,
# scripts/hew-corpus-check.sh) name the offending FIXTURE on their diagnostic
# line and then let make report a bare "Error 1"; without these patterns the
# annotation said "make: *** [Makefile:888: checked-mir-run] Error 1", which
# names the target and not the defect. Their PASSING-path lines ("PASS x",
# "NO-MAIN x", "SELFTEST x") are deliberately NOT matched: they precede a later
# fixture's real failure in the same log, so matching them would make the
# first-match grep name an innocent fixture.
#
# A compiled-Hew gate that dies on a trap is the failure mode this repo cares
# most about, and the shell reports it as a bare "Illegal instruction" line with
# no "error" or "FAIL" token anywhere — without these patterns that whole class
# fell through to the generic exit-status fallback.
# A log with none of these still reports its exit status rather than nothing.
PREFLIGHT_FAILURE_LINE_RE='(^|[[:space:]])FAIL \[|^[[:space:]]*FAILED?([[:space:]]|:)|^make(\[[0-9]+\])?: \*\*\* |^[[:space:]]*error(\[[A-Za-z0-9_]+\])?:|^[[:space:]]*[A-Za-z_][A-Za-z0-9_.]*Error:|RATCHET FAIL|Ratchet: FAILED|ORACLE gate: FAIL|Differential gate: FAILED|NOW-FAILS|NOW-PASSES|^(TRANSCRIPT MISMATCH|DUMP DRIFT|MANIFEST DRIFT|CANNOT CLASSIFY|CORPUS FAIL|LEAK ORACLE SELFTEST FAILED|(MISSING|STALE|ORPHAN) (GOLDEN|MANIFEST|EXPECTATION)):|^(checked-mir-(verify|run)|ll-corpus verify|==> Corpus sweep): FAILED([[:space:]]|$)|^==> TIMEOUT:|^thread .* panicked|^[[:space:]]*assert(ion)?.*failed|(^|[[:space:]])SIG(ILL|TRAP|SEGV|ABRT|BUS|FPE|KILL)([[:space:]:,)]|$)|Illegal instruction|Segmentation fault|Bus error|Floating point exception|Trace/breakpoint trap|Trace/BPT trap|trace trap|Abort trap|^[[:space:]]*Aborted([[:space:]]|$)|killed by signal[[:space:]]+[0-9]+|core dumped'

# Counterfactual marker.  A self-test proves its gate has teeth by RUNNING that
# gate against deliberately broken input, so a PASSING self-test log is full of
# real-looking failure text — "ORACLE gate: FAIL", "error: ...", a bare "FAIL".
# Grepping such a log for the first failure line reports the counterfactual, not
# the defect.  Every deliberately-provoked failure is therefore replayed with
# this marker at the START of the line, and extraction skips lines that carry it
# there.  The position is load-bearing: matching the marker anywhere in the line
# would let a real failure that merely mentions it — a diff of this protocol, a
# path like tests/CF-cases — hide itself from the extractor.  Marked replay is a
# gate's own choice about its own output; nothing a gate under test emits can
# claim the prefix by accident.
# --check-counterfactual-output below proves no rostered gate emits an unmarked
# match while exiting 0, and that each one still emits marked lines at all.
PREFLIGHT_COUNTERFACTUAL_MARKER='CF-'

extract_first_failure() {
    local log="$1"
    local status="$2"
    local line=""

    if [[ -s "$log" ]]; then
        line="$(
            sed $'s/\033\\[[0-9;?]*[a-zA-Z]//g' "$log" \
                | grep -v -E "^${PREFLIGHT_COUNTERFACTUAL_MARKER}" \
                | grep -m1 -E "$PREFLIGHT_FAILURE_LINE_RE" || true
        )"
    fi
    line="${line//$'\r'/}"
    line="${line//$'\t'/ }"
    # Trim surrounding whitespace without spawning another process.
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"

    if [[ -z "$line" ]]; then
        line="exited $status with no recognised failure line; see the step log"
    fi
    if (( ${#line} > 300 )); then
        line="${line:0:297}..."
    fi
    printf '%s\n' "$line"
}

# GitHub Actions workflow-command escaping.  A raw '%' or newline in a message
# truncates or corrupts the annotation, and ':' / ',' terminate a property.
escape_annotation_data() {
    local text="$1"
    text="${text//'%'/%25}"
    text="${text//$'\r'/%0D}"
    text="${text//$'\n'/%0A}"
    printf '%s' "$text"
}

escape_annotation_property() {
    local text
    text="$(escape_annotation_data "$1")"
    text="${text//:/%3A}"
    text="${text//,/%2C}"
    printf '%s' "$text"
}

emit_failure_annotation() {
    local cmd="$1"
    local failure="$2"
    [[ "${GITHUB_ACTIONS:-}" == "true" ]] || return 0
    printf '::error file=%s,title=%s::%s\n' \
        "scripts/ci-preflight-dispatcher.sh" \
        "$(escape_annotation_property "$cmd")" \
        "$(escape_annotation_data "$failure")"
}

# Counterfactual-output gate.
#
# First-failure extraction is only sound if a PASSING gate emits no
# failure-shaped line.  Gate self-tests break exactly that property by design:
# they prove a gate has teeth by RUNNING it against deliberately broken input,
# so a green self-test log carries "ORACLE gate: FAIL" or "error: ..." long
# before any real defect would appear.
#
# This mode runs the rostered counterfactual-carrying gates and fails if a zero
# exit carries an unmarked match of PREFLIGHT_FAILURE_LINE_RE.  It lives here
# rather than in a checker of its own so the pattern and the marker are the same
# two variables the extractor uses — there is nothing to keep in sync.
#
# Membership is earned, not declared: a rostered gate must emit at least one
# CF-marked line.  Without that the check passes vacuously — a self-test whose
# counterfactual was deleted, disabled, or silently redirected away prints
# nothing, offends nothing, and reports ok while proving nothing.  The marked
# replay IS the evidence that the bait path ran, so a gate that produces none is
# either gutted or was never counterfactual-carrying and does not belong here.
#
# The roster covers the gates that need no compiler build, keeping this a fast
# check rather than a second preflight.  A gate that needs `make hew` is proved
# by the preflight run itself: its first-failure line is only ever consulted
# once it has already failed.
# test-stdlib-execution-proofs and freebsd-workflow-contract-check are
# deliberately absent: both are plain assertion gates that never drive a tool
# against rigged input, so they carry no counterfactual to mark and nothing here
# could prove about them.  Listing them would have been the vacuous pass this
# check exists to reject.
COUNTERFACTUAL_ROSTER=(
    "make ll-identity-selftest"
    "make o2-differential-selftest"
    "make doc-ratchet-selftest"
    "make check-sanitizer-gate"
    "make test-release-workflow-contract"
    "make test-leak-oracle-selftest"
)

run_counterfactual_output_check() {
    local cmd output offenders marked
    local status=0
    local failures=0
    local roster=("${COUNTERFACTUAL_ROSTER[@]}")

    # Test-only roster replacement, behind the same explicit sentinel every
    # other command override uses.  The three verdicts below (OFFENDS, SILENT,
    # UNPROVABLE) are only credible if they can be provoked, and the real roster
    # is green by construction.
    if [[ -n "${PREFLIGHT_TEST_COUNTERFACTUAL_ROSTER:-}" ]]; then
        if [[ "${PREFLIGHT_TEST_ALLOW_OVERRIDE:-}" != "1" ]]; then
            die "PREFLIGHT_TEST_COUNTERFACTUAL_ROSTER requires PREFLIGHT_TEST_ALLOW_OVERRIDE=1 for test-only use."
        fi
        echo "warning: PREFLIGHT_TEST_COUNTERFACTUAL_ROSTER override active (test-only)." >&2
        roster=()
        while IFS= read -r cmd; do
            [[ -n "$cmd" ]] || continue
            roster+=("$cmd")
        done <<< "$PREFLIGHT_TEST_COUNTERFACTUAL_ROSTER"
    fi

    echo "==> counterfactual-output check (${#roster[@]} gate(s))"
    for cmd in "${roster[@]}"; do
        status=0
        output="$(bash -c "$cmd" 2>&1)" || status=$?
        if (( status != 0 )); then
            echo "    UNPROVABLE $cmd — exited $status; a rostered gate must be green before its output can be judged" >&2
            failures=$(( failures + 1 ))
            continue
        fi
        offenders="$(
            printf '%s\n' "$output" \
                | sed $'s/\033\\[[0-9;?]*[a-zA-Z]//g' \
                | grep -v -E "^${PREFLIGHT_COUNTERFACTUAL_MARKER}" \
                | grep -E "$PREFLIGHT_FAILURE_LINE_RE" || true
        )"
        marked="$(printf '%s\n' "$output" | grep -c -E "^${PREFLIGHT_COUNTERFACTUAL_MARKER}" || true)"
        if [[ -n "$offenders" ]]; then
            echo "    OFFENDS $cmd — passed while printing failure-shaped line(s):" >&2
            printf '        %s\n' "$offenders" >&2
            echo "        Route the provoked output through a CF- marked replay so extraction skips it." >&2
            failures=$(( failures + 1 ))
        elif (( marked == 0 )); then
            echo "    SILENT $cmd — passed without emitting a single ${PREFLIGHT_COUNTERFACTUAL_MARKER}marked line." >&2
            echo "        A rostered gate proves its counterfactual ran by replaying it marked." >&2
            echo "        Replay the provoked output instead of discarding it, or drop this gate" >&2
            echo "        from COUNTERFACTUAL_ROSTER because it carries no counterfactual." >&2
            failures=$(( failures + 1 ))
        else
            echo "    ok $cmd ($marked marked line(s))"
        fi
    done

    if (( failures > 0 )); then
        echo "counterfactual-output check: FAIL ($failures gate(s))" >&2
        return 1
    fi
    echo "counterfactual-output check: PASS"
}

DRY_RUN=0
FAIL_FAST=0
CHECK_COUNTERFACTUAL_OUTPUT=0
BASE_REF=""
EXPLICIT_PATHS=0
LANE=""
LANE_REASON=""
CHANGED_FILES=()
CHANGED_CRATE_DIRS=()
COMMANDS=()
WARMUP_COMMANDS=()
WARMUP_NOTES=()
PROFILE_JSON_PATH=""
GITHUB_OUTPUT_PATH=""
SHARD_INDEX=0
SHARD_COUNT=0
SHARD_PLAN_ONLY=0
SHARD_PLAN_LINES=""
PREFLIGHT_SUMMARY_FILE="${PREFLIGHT_SUMMARY_FILE:-.tmp/preflight-summary.md}"

usage() {
    cat <<'EOF'
Usage: scripts/ci-preflight-dispatcher.sh [--dry-run] [--fail-fast] [--base <ref>] [--shard K/N]
                                         [--shard-plan N] [--profile-json <path>]
                                         [--github-output <path>] [--] [path...]

Dispatch a conservative local CI preflight based on changed files.

- Pass explicit paths to classify those files directly.
- With no paths, the script inspects committed, staged, unstaged, and untracked changes.
- By default, all selected commands run and failures are reported together at the end.
- --fail-fast           Stop after the first failed command.
- If the first-slice routing is unclear, the script runs the broader local check profile.
- --shard K/N           Run only shard K of N.  The selected profile's command list is
                        partitioned into N duration-balanced groups; every command appears
                        in exactly one shard and ordering-dependent commands stay together.
                        K and N must be positive integers with 1 <= K <= N.
- --shard-plan N        Print the full shard assignment for N shards and exit without
                        running anything.
- --check-counterfactual-output
                        Run the rostered self-tests and fail if a PASSING one prints a
                        line the first-failure extractor would report; exit without
                        dispatching a preflight.
- --profile-json <path> Write command and warm-up timing as a JSON array to <path> (one
                        object per step, with "cmd", "elapsed_s", "status", "phase" fields).
- --github-output <path> Append the selected profile and compile requirement as
                         GitHub Actions outputs.

Diagnostics: every run writes a per-command result table (command, elapsed, result, first
failure line) to $PREFLIGHT_SUMMARY_FILE (default .tmp/preflight-summary.md), and, under
GitHub Actions, to $GITHUB_STEP_SUMMARY plus one ::error annotation per failed command.
EOF
}

die() {
    echo "error: $*" >&2
    exit 1
}

# Reject any shard spec that could silently drop coverage.  A malformed K/N, a
# zero denominator, or an out-of-range index means the caller's shard set does
# not partition the command list — fail before a single command runs rather
# than report a green verdict over an unrun remainder.
parse_shard_spec() {
    local spec="$1"
    [[ "$spec" =~ ^([0-9]+)/([0-9]+)$ ]] \
        || die "--shard expects K/N with positive integers, got '$spec'"
    SHARD_INDEX="$((10#${BASH_REMATCH[1]}))"
    SHARD_COUNT="$((10#${BASH_REMATCH[2]}))"
    (( SHARD_COUNT >= 1 )) || die "--shard denominator must be >= 1, got '$spec'"
    (( SHARD_INDEX >= 1 && SHARD_INDEX <= SHARD_COUNT )) \
        || die "--shard index must satisfy 1 <= K <= N, got '$spec'"
}

parse_shard_count() {
    local count="$1"
    [[ "$count" =~ ^[0-9]+$ ]] \
        || die "--shard-plan expects a positive integer shard count, got '$count'"
    SHARD_COUNT="$((10#$count))"
    (( SHARD_COUNT >= 1 )) || die "--shard-plan count must be >= 1, got '$count'"
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

append_unique_crate() {
    local crate="$1"
    local existing
    if [[ ${CHANGED_CRATE_DIRS[0]+set} == set ]]; then
        for existing in "${CHANGED_CRATE_DIRS[@]}"; do
            [[ "$existing" == "$crate" ]] && return 0
        done
    fi
    CHANGED_CRATE_DIRS+=("$crate")
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
    local command="$1"
    local existing
    if [[ "${COMPILED_HEW_GATE_OWNER:-dispatcher}" == "aggregate" ]]; then
        case "$command" in
            "make test-hew-ratchet"|"make test-o2-differential")
                return 0
                ;;
        esac
    fi
    if [[ ${COMMANDS[0]+set} == set ]]; then
        for existing in "${COMMANDS[@]}"; do
            [[ "$existing" == "$command" ]] && return 0
        done
    fi
    COMMANDS+=("$command")
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
        hew-cli/*|hew-pkg/*)
            return 0
            ;;
        # CLI pipeline support crates: compile pipeline, C ABI helpers,
        # and code generators.  Changes here are covered by
        # cargo nextest run -p hew-cli -p hew-pkg because hew-cli links
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
        tests/hew/*|tests/core-matrix/*|scripts/core-matrix.py|scripts/core-matrix-gen.py)
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

is_capability_authority_path() {
    # hew-capability-gen's authority ratchet (tests/authority.rs) pins checker
    # coverage over the capability surface STRUCTURALLY — the crate has no
    # cargo dependency on hew-mir or hew-types, so the reverse-dependency
    # closure that feeds AFFECTED_PACKAGE_ARGS can never select it.  A
    # hew-mir / hew-types change can therefore break the ratchet while every
    # closure-routed test stays green (escape class: structural ratchets that
    # live outside the cargo dependency graph).
    case "$1" in
        hew-mir/*|hew-types/*)
            return 0
            ;;
    esac
    return 1
}

is_ll_oracle_path() {
    # The ll-oracle golden corpus (tests/ll-oracle/corpus/golden) pins the
    # emitted per-function LLVM IR.  Any MIR-lowering or codegen emission
    # change can drift it (e.g. an epilogue reorder), and nothing else in the
    # narrow lanes diffs those goldens — only make ll-diff does (~45s).
    case "$1" in
        hew-hir/*|hew-mir/*|hew-codegen-rs/*|tests/ll-oracle/*)
            return 0
            ;;
    esac
    return 1
}

is_scripts_config_path() {
    case "$1" in
        .gitignore|scripts/*|.github/*)
            return 0
            ;;
        # License and attribution files are policy inputs, not compiler inputs.
        THIRD-PARTY-LICENSES|NOTICE|about.toml|about.hbs|deny.toml|LICENSE-*|LICENSE)
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
        --check-counterfactual-output)
            CHECK_COUNTERFACTUAL_OUTPUT=1
            shift
            ;;
        --shard)
            shift
            [[ $# -gt 0 ]] || die "--shard requires a K/N spec"
            parse_shard_spec "$1"
            shift
            ;;
        --shard-plan)
            shift
            [[ $# -gt 0 ]] || die "--shard-plan requires a shard count"
            parse_shard_count "$1"
            SHARD_PLAN_ONLY=1
            shift
            ;;
        --profile-json)
            shift
            [[ $# -gt 0 ]] || die "--profile-json requires a path"
            PROFILE_JSON_PATH="$1"
            shift
            ;;
        --github-output)
            shift
            [[ $# -gt 0 ]] || die "--github-output requires a path"
            GITHUB_OUTPUT_PATH="$1"
            shift
            ;;
        --help|-h)
            usage
            exit 0
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

if [[ -n "$BASE_REF" ]] && ! git rev-parse --verify "$BASE_REF" >/dev/null 2>&1; then
    die "unknown base ref: $BASE_REF"
fi

if (( SHARD_PLAN_ONLY == 1 && SHARD_INDEX != 0 )); then
    die "--shard and --shard-plan are mutually exclusive"
fi

if (( CHECK_COUNTERFACTUAL_OUTPUT == 1 )); then
    run_counterfactual_output_check
    exit $?
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
needs_hew_corpus=0
needs_hew_fmt_property=0
needs_sandbox_fixture_check=0
needs_sandbox_parity=0
needs_trap_fixtures=0
needs_runtime_compiled_suite=0
needs_capability_authority=0
needs_ll_diff=0

for path in "${CHANGED_FILES[@]}"; do
    crate_dir="${path%%/*}"
    if [[ "$path" == */* && -f "$REPO_ROOT/$crate_dir/Cargo.toml" ]]; then
        append_unique_crate "$crate_dir"
    fi

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

    # Parallel side-channel: any tracked .hew file change triggers needs_hew_corpus
    # so that the repo-wide corpus sweep runs on every lane where a .hew file
    # changed.  This mirrors the needs_hew_suite / needs_stdlib_lint pattern.
    # The flag is a no-op when the lane already includes make hew-check-all
    # (the fallback lane) — checked in the append block below.
    case "$path" in
        *.hew)
            needs_hew_corpus=1
            needs_hew_fmt_property=1
            ;;
    esac

    case "$path" in
        hew-parser/*|hew-cli/src/main.rs|hew-cli/src/args.rs)
            needs_hew_fmt_property=1
            ;;
    esac

    # Parallel side-channel: trap-fixture paths set needs_trap_fixtures regardless
    # of which primary bucket claims the path.  This mirrors the needs_hew_suite /
    # needs_stdlib_lint pattern — the flag appends make fuzz-oracle after the
    # lane body without changing the lane selector itself.
    if is_trap_fixtures_path "$path"; then
        needs_trap_fixtures=1
    fi

    # Parallel side-channels in the same pattern: these flags append catching
    # gates after the lane body without changing the lane selector.
    if is_capability_authority_path "$path"; then
        needs_capability_authority=1
    fi
    if is_ll_oracle_path "$path"; then
        needs_ll_diff=1
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
        if is_runtime_path "$path" || is_hew_lib_path "$path" || is_stdlib_net_path "$path"; then
            needs_runtime_compiled_suite=1
        fi
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

AFFECTED_PACKAGE_ARGS=""
if [[ ${CHANGED_CRATE_DIRS[0]+set} == set ]]; then
    while IFS= read -r package; do
        [[ -n "$package" ]] || continue
        AFFECTED_PACKAGE_ARGS="$AFFECTED_PACKAGE_ARGS -p $package"
    done < <(
        cargo metadata --no-deps --format-version 1 | python3 -c '
import json, pathlib, sys
changed = set(sys.argv[1:])
packages = json.load(sys.stdin)["packages"]
selected = {
    package["name"]
    for package in packages
    if pathlib.Path(package["manifest_path"]).parent.name in changed
}
dependencies = {
    package["name"]: {dependency["name"] for dependency in package["dependencies"]}
    for package in packages
}
while True:
    expanded = selected | {
        package for package, deps in dependencies.items() if deps & selected
    }
    if expanded == selected:
        break
    selected = expanded
print("\n".join(sorted(selected)))
' "${CHANGED_CRATE_DIRS[@]}"
    )
fi

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
    #   parser + types: the compiler-pipeline suite covers both buckets and
    #     their HIR/MIR/codegen consumers.
    if (( has_parser == 1 && has_types == 1 && bucket_count == 2 )); then
        LANE="types"
        LANE_REASON="parser + type-checker changed; compiler pipeline covers both"
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
        add_command "make doc-ratchet-selftest"
        ;;
    scripts-config)
        add_command "make structural-lint"
        add_command "make leak-scan"
        add_command "make test-release-workflow-contract"
        add_command "make test-stdlib-execution-proofs"
        add_command "cargo fmt --all -- --check"
        add_command "make freebsd-workflow-contract-check"
        add_command "make o2-differential-selftest"
        add_command "make doc-ratchet-selftest"
        add_command "make check-counterfactual-output"
        ;;
    grammar)
        add_command "cargo fmt --all -- --check"
        add_command "make grammar"
        ;;
    parser)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy$AFFECTED_PACKAGE_ARGS --tests -- -D warnings"
        add_command "make hew-native wasm-runtime"
        add_command "cargo nextest run --profile ci$AFFECTED_PACKAGE_ARGS"
        add_command "make hew-fmt-property"
        ;;
    types)
        # A type-checker change can break hew-hir / hew-mir tests, so use the
        # full frontend pipeline rather than a package subset.
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy$AFFECTED_PACKAGE_ARGS --tests -- -D warnings"
        add_command "make hew-native wasm-runtime"
        add_command "cargo nextest run --profile ci$AFFECTED_PACKAGE_ARGS"
        add_command "make fuzz-oracle"
        # A type-checker change reaches MIR lowering and can drift the checked-MIR
        # golden corpus (examples/v05/checked-mir) just as a lowering edit can.
        # Run the same golden diff here so the drift is caught locally rather than
        # at hosted CI.  Fast (~45s), well within this lane's budget.
        add_command "make checked-mir-verify"
        # The golden diff never loads the programs.  Execute the corpus too, so
        # a retype that changes runtime behaviour fails here instead of leaving
        # a byte-identical dump over a crashing binary.
        add_command "make checked-mir-run"
        ;;
    cli)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy$AFFECTED_PACKAGE_ARGS --tests -- -D warnings"
        add_command "make hew-native wasm-runtime"
        add_command "cargo nextest run --profile ci$AFFECTED_PACKAGE_ARGS"
        add_command "make hew-fmt-property"
        ;;
    compiler-pipeline)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy$AFFECTED_PACKAGE_ARGS --tests -- -D warnings"
        add_command "make hew-native wasm-runtime"
        add_command "cargo nextest run --profile ci$AFFECTED_PACKAGE_ARGS"
        add_command "make test-opaque-resource-lifecycle-matrix"
        add_command "make test-vertical-slice"
        add_command "make test-pkg-import"
        # fuzz-oracle catches trap signal-code regressions (SIGILL/SIGTRAP) and
        # ratchet mismatches invisible to the nextest workspace run (#2025).
        add_command "make fuzz-oracle"
        # checked-mir-verify diffs every fixture's --dump-mir against the
        # committed golden corpus (examples/v05/checked-mir).  A drop-plan or
        # lowering edit that shifts the emitted MIR drifts these goldens; without
        # this step the drift only surfaces at hosted CI's Build & test (Linux)
        # job, costing a full hosted cycle.  Fast (compile-and-compare, ~45s).
        add_command "make checked-mir-verify"
        # A drop-plan edit can leave every golden byte-identical and still make
        # the compiled fixture crash — the goldens are text, nothing runs them.
        # checked-mir-run builds and executes the corpus and diffs exit status
        # and stdout, so that failure mode surfaces here.
        add_command "make checked-mir-run"
        ;;
    vertical-slice)
        add_command "cargo fmt --all -- --check"
        add_command "make test-vertical-slice"
        add_command "make test-pkg-import"
        # fuzz-oracle reads the vertical-slice/accept fixtures: a fixture change
        # that skips fuzz-oracle misses the trap signal-code ratchet (#2025).
        add_command "make fuzz-oracle"
        ;;
    observe)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy$AFFECTED_PACKAGE_ARGS --tests -- -D warnings"
        add_command "cargo nextest run --profile ci$AFFECTED_PACKAGE_ARGS"
        add_command "make observe-functional-test"
        ;;
    runtime-testkit)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy$AFFECTED_PACKAGE_ARGS --tests -- -D warnings"
        add_command "cargo nextest run --profile ci$AFFECTED_PACKAGE_ARGS"
        ;;
    hew-tests)
        add_command "cargo fmt --all -- --check"
        add_command "make test-hew-ratchet"
        add_command "make test-core-matrix"
        add_command "make test-stdlib-ratchet"
        ;;
    runtime-net)
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy$AFFECTED_PACKAGE_ARGS --tests -- -D warnings"
        add_command "cargo nextest run --profile ci$AFFECTED_PACKAGE_ARGS"
        if (( needs_runtime_compiled_suite == 1 )); then
            add_command "make stdlib"
            add_command "scripts/check-libhew-fresh.sh"
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
            add_command "make mqtt-broker-e2e"
            add_command "make observe-functional-test"
        fi
        ;;
    wasm)
        # hew-wasm/* changes: run the WASM lib tests and the playground build
        # (which includes wasm-pack --release and the curated-manifest smoke test).
        add_command "cargo fmt --all -- --check"
        add_command "cargo clippy$AFFECTED_PACKAGE_ARGS --tests -- -D warnings"
        add_command "cargo nextest run --profile ci$AFFECTED_PACKAGE_ARGS"
        add_command "make playground-check"
        ;;
    fallback)
        # Keep the fast smoke target as a direct local opt-in. Its nextest
        # selection is a subset of the full workspace run below, so running it
        # here would make it a serial prefix of its own superset. Format checking
        # remains first for quick feedback, and make lint supplies clippy,
        # structural-lint, Hew formatting, and the remaining static checks.
        #
        # Hew-language suites run after the Rust workspace to keep the ratchet
        # verdict separate from the Rust test verdict.
        #
        # The sequence below mirrors CI's build-and-test job exactly so a green
        # fallback preflight predicts a green merge-queue outcome.
        add_command "cargo fmt --all -- --check"
        add_command "make lint"
        add_command "make freebsd-workflow-contract-check"
        add_command "make playground-check"
        add_command "make test"
        add_command "make test-compiler-pipeline"
        add_command "make test-opaque-resource-lifecycle-matrix-external"
        add_command "make test-vertical-slice"
        add_command "make test-pkg-import"
        add_command "make fuzz-oracle"
        add_command "make test-hew-ratchet"
        add_command "make test-core-matrix"
        add_command "make test-o2-differential"
        add_command "make o2-differential-selftest"
        add_command "make test-release-workflow-contract"
        add_command "make test-stdlib-ratchet"
        add_command "make test-stdlib-execution-proofs"
        add_command "make test-doc-examples"
        add_command "make doc-ratchet-selftest"
        add_command "make sandbox-parity"
        add_command "make checked-mir-verify"
        add_command "make checked-mir-run"
        add_command "make ll-diff"
        add_command "make ll-identity-selftest"
        add_command "make hew-check-all"
        add_command "make hew-fmt-property"
        add_command "make test-cabi"
        add_command "make check-sanitizer-gate"
        add_command "make check-gate-reachability"
        add_command "make check-counterfactual-output"
        add_command "make test-leak-oracle-selftest"
        add_command "make test-ux-examples"
        add_command "make test-surface-examples"
        add_command "make test-package-install"
        add_command "make test-runtime-unit"
        add_command "make fuzz-oracle-selftest"
        add_command "make libhew-link-race-test"
        add_command "make mqtt-broker-e2e"
        add_command "make observe-functional-test"
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

if (( needs_hew_corpus == 1 )) && [[ "$LANE" != "fallback" ]]; then
    # A tracked .hew file changed.  Run the repo-wide corpus sweep so any new
    # "undefined symbol" or type mismatch introduced by the diff surfaces
    # before push.  Skip when LANE is fallback: it already covers the whole
    # test suite (including hew-check-all's constituency) via make test +
    # make test-hew-ratchet.
    add_command "make hew-check-all"
fi

if (( needs_hew_fmt_property == 1 )) && [[ "$LANE" != "fallback" && "$LANE" != "parser" && "$LANE" != "cli" ]]; then
    # Any Hew source change exercises the formatter property over the complete
    # derived corpus, independently of the primary lane for that source file.
    add_command "make hew-fmt-property"
fi

if (( needs_sandbox_fixture_check == 1 )); then
    add_command "make sandbox-fixtures-check"
fi

if (( needs_sandbox_parity == 1 )); then
    add_command "make sandbox-parity"
fi

if (( needs_trap_fixtures == 1 )) && [[ "$LANE" != "fallback" && "$LANE" != "compiler-pipeline" && "$LANE" != "types" && "$LANE" != "runtime-net" && "$LANE" != "vertical-slice" ]]; then
    # MIR bounds/trap lowering or fuzz-oracle corpus changed in a lane that does
    # not already include fuzz-oracle.  Append it so the trap signal-code ratchet
    # runs before push regardless of the primary lane selected.
    add_command "make fuzz-oracle"
fi

if (( needs_capability_authority == 1 )) && [[ "$LANE" != "fallback" ]]; then
    # hew-mir / hew-types changed: run the capability authority ratchet
    # (hew-capability-gen/tests/authority.rs), which pins checker coverage
    # structurally and sits OUTSIDE the reverse-dep closure — no closure-routed
    # nextest run ever selects it (see is_capability_authority_path).  Cheap:
    # the crate depends only on serde/serde_json/toml.  The hew-cli
    # ownership/affine suites (e.g. affine_resource_carrier_boundaries) do NOT
    # need an explicit entry here: hew-cli is a reverse dependency of both
    # crates, so the lane's closure nextest run already carries them — pinned
    # by test_mir_types_diff_routes_cross_crate_catching_gates.
    # Skip when LANE is fallback: make test covers the whole workspace.
    add_command "cargo nextest run --profile ci -p hew-capability-gen"
fi

if (( needs_ll_diff == 1 )) && [[ "$LANE" != "fallback" ]]; then
    # MIR lowering / codegen emission changed: diff the ll-oracle golden corpus
    # so an emission drift (an epilogue reorder, a changed intrinsic sequence)
    # surfaces locally with the regen instruction instead of costing a hosted
    # CI cycle.  Intentional drifts regenerate via make ll-golden in the same
    # commit.  Skip when LANE is fallback: it already includes make ll-diff.
    add_command "make ll-diff"
fi

# Orchestration-token leak scan — run on every push for every lane.
# Catches lane IDs, Q-tags, and .tmp/ path references in committed source before review.
# Fast (<2 s, git grep only).  Fallback lane gets this via `make lint`; scripts-config
# lane gets it explicitly above; all other lanes get it here so no lane silently skips.
if [[ "$LANE" != "fallback" && "$LANE" != "scripts-config" ]]; then
    add_command "make leak-scan"
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

# ── Shard partitioning ────────────────────────────────────────────────────────
#
# The selected profile's command list is partitioned into SHARD_COUNT groups by
# longest-processing-time-first bin packing over duration-weighted dependency
# groups.  The partition is a pure function of (command list, SHARD_COUNT), so
# every shard job computes the same plan from the same diff without exchanging
# state, and it is exhaustive and disjoint by construction: each dependency
# group is placed in exactly one bin, and every command belongs to exactly one
# group.  Within a shard, commands keep their original relative order, so every
# ordering constraint that held in the unsharded list still holds.
compute_shard_plan() {
    local cmd
    {
        for cmd in "${COMMANDS[@]}"; do
            printf '%s\t%s\t%s\n' "$(command_shard_group "$cmd")" "$(command_weight "$cmd")" "$cmd"
        done
    } | python3 -c '
import sys

shard_count = int(sys.argv[1])
commands = []
for line in sys.stdin.read().splitlines():
    if not line:
        continue
    group, weight, cmd = line.split("\t", 2)
    commands.append((group, int(weight), cmd))

order = []
weights = {}
for index, (group, weight, _cmd) in enumerate(commands):
    if group not in weights:
        weights[group] = 0
        order.append(group)
    weights[group] += weight

first_index = {group: order.index(group) for group in order}
# Longest-processing-time-first: heaviest group to the emptiest bin, ties
# broken by first appearance then by lowest bin index, so the plan is stable.
loads = [0] * shard_count
assignment = {}
for group in sorted(order, key=lambda g: (-weights[g], first_index[g])):
    target = min(range(shard_count), key=lambda b: (loads[b], b))
    assignment[group] = target + 1
    loads[target] += weights[group]

counts = [0] * shard_count
for group, weight, cmd in commands:
    shard = assignment[group]
    counts[shard - 1] += 1
    print("ASSIGN\t%d\t%d\t%s" % (shard, weight, cmd))
for index in range(shard_count):
    print("TOTAL\t%d\t%d\t%d" % (index + 1, loads[index], counts[index]))
' "$SHARD_COUNT"
}

print_shard_plan() {
    local shard total count line assign_shard weight cmd marker
    echo "Shard plan: $SHARD_COUNT shard(s), LPT bin packing over duration-weighted dependency groups"
    for shard in $(seq 1 "$SHARD_COUNT"); do
        total="$(printf '%s\n' "$SHARD_PLAN_LINES" | awk -F'\t' -v s="$shard" '$1=="TOTAL" && $2==s {print $3}')"
        count="$(printf '%s\n' "$SHARD_PLAN_LINES" | awk -F'\t' -v s="$shard" '$1=="TOTAL" && $2==s {print $4}')"
        marker=""
        if (( SHARD_INDEX == shard )); then
            marker="  <- selected"
        fi
        printf '  shard %s/%s  ~%s min estimated  %s command(s)%s\n' \
            "$shard" "$SHARD_COUNT" "$(( (total + 30) / 60 ))" "$count" "$marker"
        while IFS=$'\t' read -r line assign_shard weight cmd; do
            [[ "$line" == "ASSIGN" && "$assign_shard" == "$shard" ]] || continue
            printf '      - %s  (weight: %ss)\n' "$cmd" "$weight"
        done <<< "$SHARD_PLAN_LINES"
    done
}

select_shard_commands() {
    local line assign_shard weight cmd
    local selected=()
    while IFS=$'\t' read -r line assign_shard weight cmd; do
        [[ "$line" == "ASSIGN" && "$assign_shard" == "$SHARD_INDEX" ]] || continue
        selected+=("$cmd")
    done <<< "$SHARD_PLAN_LINES"
    COMMANDS=("${selected[@]+"${selected[@]}"}")
}

if (( SHARD_COUNT > 0 )) && has_commands; then
    SHARD_PLAN_LINES="$(compute_shard_plan)"
fi

if (( SHARD_INDEX > 0 )); then
    select_shard_commands
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

REQUIRES_COMPILE=true
case "$LANE" in
    docs|scripts-config|grammar)
        REQUIRES_COMPILE=false
        ;;
esac

# Warm artifact construction before applying per-command watchdog budgets.  The
# timed checks retain their finite hang ceilings; this separates one-time
# compilation from the gate measurements those ceilings are calibrated for.
#
# Keep this list derived from the chosen commands rather than from a lane-wide
# superset: a parser diff needs the native compiler and WASM runtime, while a
# documentation diff needs neither.  An unrecognised command falls back to the
# complete artifact set and leaves a visible note so its mapping can be added.
#
# A warm-up must build the artifacts its command needs THE SAME WAY the command
# builds them.  `cargo build --all-targets` is not that way: the root
# `[profile.dev] panic = "abort"` cannot apply to a libtest harness, so mixing
# lib and test targets in one invocation fails outright
# ("requires panic strategy abort which is incompatible with this crate's
# strategy of unwind" on hew-sandbox-wasm, plus a duplicate-unit serde mismatch
# in xtask).  Test-target warm-ups therefore mirror the real gate: the same
# nextest/cargo-test invocation with `--no-run`, exactly as `make test` builds
# its binaries.  Clippy warms through clippy — its check artifacts are a
# separate fingerprint from rustc's — with the trailing `-- -D warnings` dropped
# so a lint failure surfaces as the timed gate's failure, not as an aborted
# warm-up.  Dropping the deny flag does not cost the warm: it only re-checks the
# top-level packages, dependencies stay cached.
WORKSPACE_TEST_WARMUP="cargo nextest run --workspace --exclude hew-cabi --profile ci --no-run"

add_warmup_command() {
    local candidate="$1"
    local existing
    for existing in "${WARMUP_COMMANDS[@]+"${WARMUP_COMMANDS[@]}"}"; do
        [[ "$existing" == "$candidate" ]] && return 0
    done
    WARMUP_COMMANDS+=("$candidate")
}

add_full_warmup() {
    local cmd="$1"
    add_warmup_command "$WORKSPACE_TEST_WARMUP"
    add_warmup_command "make hew"
    add_warmup_command "make runtime"
    add_warmup_command "make stdlib"
    add_warmup_command "make wasm-runtime"
    WARMUP_NOTES+=("$cmd has no artifact mapping; using the full warm-up set")
}

map_warmup_artifacts() {
    local cmd="$1"
    case "$cmd" in
        "cargo fmt "*)
            ;;
        "cargo clippy "*)
            add_warmup_command "${cmd%% -- *}"
            ;;
        "cargo nextest run "*)
            add_warmup_command "$cmd --no-run"
            ;;
        "make test-cabi")
            add_warmup_command "make test-cabi-build"
            ;;
        "make test-runtime-unit")
            add_warmup_command "cargo nextest run --profile ci -p hew-runtime --no-default-features --no-run"
            ;;
        "make playground-check")
            add_warmup_command "cargo test -p hew-wasm --no-run"
            ;;
        "make grammar")
            add_warmup_command "$WORKSPACE_TEST_WARMUP"
            ;;
        "make structural-lint")
            add_warmup_command "scripts/ast-grep-lint.sh --bootstrap --install-only"
            ;;
        "make lint")
            # `make lint` bootstraps ast-grep and then runs
            # `cargo clippy --workspace --tests -- -D warnings`; warm both.
            add_warmup_command "scripts/ast-grep-lint.sh --bootstrap --install-only"
            add_warmup_command "cargo clippy --workspace --tests"
            ;;
        "make hew-native wasm-runtime")
            add_warmup_command "make hew"
            add_warmup_command "make wasm-runtime"
            ;;
        "make hew"|"make test-doc-examples"|"make test-stdlib-ratchet"|"make checked-mir-verify"|"make ll-diff"|"make hew-check-all"|"make hew-fmt-property")
            add_warmup_command "make hew"
            ;;
        "make runtime")
            add_warmup_command "make runtime"
            ;;
        "make stdlib"|"scripts/check-libhew-fresh.sh")
            add_warmup_command "make stdlib"
            ;;
        "make wasm-runtime")
            add_warmup_command "make wasm-runtime"
            ;;
        "make test-compiler-pipeline"|"make test-opaque-resource-lifecycle-matrix"|"make test-opaque-resource-lifecycle-matrix-external")
            add_warmup_command "make hew"
            add_warmup_command "make stdlib"
            add_warmup_command "make wasm-runtime"
            ;;
        "make test-vertical-slice"|"make test-pkg-import"|"make test-package-install"|"make fuzz-oracle"|"make fuzz-oracle-selftest"|"make test-hew-ratchet"|"make test-core-matrix"|"make test-o2-differential"|"make test-ux-examples"|"make test-surface-examples"|"make observe-functional-test"|"make mqtt-broker-e2e"|"make libhew-link-race-test"|"make sandbox-parity")
            add_warmup_command "make hew"
            add_warmup_command "make runtime"
            add_warmup_command "make stdlib"
            ;;
        "make checked-mir-run")
            add_warmup_command "make hew"
            add_warmup_command "make runtime"
            add_warmup_command "make stdlib"
            ;;
        "make leak-scan"|"make check-counterfactual-output"|"make freebsd-workflow-contract-check"|"make test-release-workflow-contract"|"make test-stdlib-execution-proofs"|"make o2-differential-selftest"|"make doc-ratchet-selftest"|"make check-sanitizer-gate"|"make check-gate-reachability"|"make test-leak-oracle-selftest"|"make ll-identity-selftest")
            ;;
        "make test")
            # Mirror the target's own order: wasm-runtime/runtime/libhew first,
            # then the nextest binary build.
            add_warmup_command "make hew"
            add_warmup_command "make runtime"
            add_warmup_command "make stdlib"
            add_warmup_command "make wasm-runtime"
            add_warmup_command "$WORKSPACE_TEST_WARMUP"
            ;;
        *)
            add_full_warmup "$cmd"
            ;;
    esac
}

for cmd in "${COMMANDS[@]+"${COMMANDS[@]}"}"; do
    map_warmup_artifacts "$cmd"
done

# Test-only warm-up replacement.  Keep this behind the same explicit sentinel
# as command replacement so normal preflight invocations always warm real
# artifacts.
if [[ -n "${PREFLIGHT_TEST_WARMUP_COMMANDS:-}" ]]; then
    if [[ "${PREFLIGHT_TEST_ALLOW_OVERRIDE:-}" != "1" ]]; then
        die "PREFLIGHT_TEST_WARMUP_COMMANDS requires PREFLIGHT_TEST_ALLOW_OVERRIDE=1 for test-only use; unset PREFLIGHT_TEST_WARMUP_COMMANDS to run the normal preflight."
    fi
    echo "warning: PREFLIGHT_TEST_WARMUP_COMMANDS override active (test-only); replacing warm-up commands." >&2
    WARMUP_COMMANDS=()
    WARMUP_NOTES=()
    while IFS= read -r test_cmd; do
        [[ -n "$test_cmd" ]] || continue
        WARMUP_COMMANDS+=("$test_cmd")
    done <<< "$PREFLIGHT_TEST_WARMUP_COMMANDS"
fi

if [[ -n "$GITHUB_OUTPUT_PATH" ]]; then
    {
        printf 'profile=%s\n' "$PROFILE_LABEL"
        printf 'requires_compile=%s\n' "$REQUIRES_COMPILE"
    } >> "$GITHUB_OUTPUT_PATH"
fi

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

detect_host_parallelism

echo "Selected profile: $PROFILE_LABEL"
if (( FAIL_FAST == 1 )); then
    echo "Failure policy: fail-fast"
else
    echo "Failure policy: run-all (default)"
fi
print_timeout_scaling
echo "Reason: $LANE_REASON"
echo "Changed files:"
for path in "${CHANGED_FILES[@]}"; do
    echo "  - $path"
done

if [[ -n "$SHARD_PLAN_LINES" ]]; then
    print_shard_plan
elif (( SHARD_COUNT > 0 )); then
    echo "Shard plan: no commands to partition across $SHARD_COUNT shard(s)"
fi

if (( SHARD_PLAN_ONLY == 1 )); then
    echo "Shard plan only: no commands executed."
    exit 0
fi

if (( SHARD_INDEX > 0 )); then
    echo "Shard: $SHARD_INDEX/$SHARD_COUNT"
fi

if ! has_commands; then
    if (( SHARD_INDEX > 0 )); then
        echo "Commands: none (this shard's partition is empty)"
    else
        echo "Commands: none (docs-only)"
    fi
    if (( DRY_RUN == 1 )); then
        echo "Dry run: no commands executed."
    fi
    exit 0
fi

if [[ ${#WARMUP_COMMANDS[@]} -gt 0 ]]; then
    echo "Warm-up:"
    for cmd in "${WARMUP_COMMANDS[@]}"; do
        echo "  - $cmd"
    done
    for note in "${WARMUP_NOTES[@]+"${WARMUP_NOTES[@]}"}"; do
        echo "  ! $note"
    done
fi

echo "Commands:"
for cmd in "${COMMANDS[@]}"; do
    if (( DRY_RUN == 1 )); then
        echo "  - $cmd  (budget: $(command_timeout "$cmd")s)"
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
_failure_line=""

append_profile_entry() {
    local cmd="$1"
    local elapsed="$2"
    local status="$3"
    local phase="$4"
    _json_entries+=("{\"cmd\":$(printf '%s' "$cmd" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))'),\"elapsed_s\":${elapsed},\"status\":${status},\"phase\":$(printf '%s' "$phase" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))')}")
}

PREFLIGHT_WARMUP_ELAPSED=0
PREFLIGHT_WARMUP_STATUS=0
PREFLIGHT_WARMUP_FAILED_COMMAND=""
PREFLIGHT_WARMUP_FAILURE_LINE=""

run_warmup() {
    local cmd
    local start=$SECONDS
    local command_start
    local command_elapsed
    local cmd_timeout
    local status=0
    local log
    local pipe_status

    echo ""
    echo "==> warm-up"
    for cmd in "${WARMUP_COMMANDS[@]}"; do
        echo "    $cmd"
        command_start=$SECONDS
        cmd_timeout="$(warmup_timeout)"
        status=0
        log="$(mktemp "${TMPDIR:-/tmp}/hew-preflight-warmup.XXXXXX")"
        set +e
        run_in_pgroup_with_timeout "$cmd_timeout" "$cmd" 2>&1 | tee "$log"
        pipe_status="${PIPESTATUS[0]}"
        set -e
        status="$pipe_status"
        command_elapsed=$(( SECONDS - command_start ))
        append_profile_entry "$cmd" "$command_elapsed" "$status" "warm-up"
        if [[ "$status" -ne 0 ]]; then
            PREFLIGHT_WARMUP_FAILED_COMMAND="$cmd"
            PREFLIGHT_WARMUP_FAILURE_LINE="$(extract_first_failure "$log" "$status")"
        fi
        rm -f "$log"
        if [[ "$status" -eq 137 || "$status" -eq 143 ]]; then
            echo "==> TIMEOUT: '$cmd' exceeded ${cmd_timeout}s warm-up budget and was killed."
            PREFLIGHT_WARMUP_FAILURE_LINE="exceeded the ${cmd_timeout}s warm-up budget and was killed (exit $status)"
        fi
        if [[ "$status" -ne 0 ]]; then
            echo "    first failure: $PREFLIGHT_WARMUP_FAILURE_LINE"
            emit_failure_annotation "warm-up: $cmd" "$PREFLIGHT_WARMUP_FAILURE_LINE"
            break
        fi
    done

    PREFLIGHT_WARMUP_ELAPSED=$(( SECONDS - start ))
    PREFLIGHT_WARMUP_STATUS="$status"

    if [[ "$status" -ne 0 ]]; then
        echo "<-- warm-up  elapsed ${PREFLIGHT_WARMUP_ELAPSED}s  FAILED (exit $status)"
    else
        echo "<-- warm-up  elapsed ${PREFLIGHT_WARMUP_ELAPSED}s  ok"
    fi

    return "$status"
}

run_timed_command() {
    local cmd="$1"
    local cmd_timeout
    local start=$SECONDS
    local status=0
    local log
    local pipe_status
    cmd_timeout="$(command_timeout "$cmd")"

    echo ""
    echo "==> $cmd"

    log="$(mktemp "${TMPDIR:-/tmp}/hew-preflight-cmd.XXXXXX")"
    # tee, not a redirect: the log is only a diagnostic side-channel; the
    # command's output must still stream to the job log in real time so a
    # hung gate is visible before its watchdog fires.  PIPESTATUS[0] carries
    # the command's real exit code — tee's status must never stand in for it.
    set +e
    run_in_pgroup_with_timeout "$cmd_timeout" "$cmd" 2>&1 | tee "$log"
    pipe_status="${PIPESTATUS[0]}"
    set -e
    status="$pipe_status"

    _elapsed_s=$(( SECONDS - start ))
    _failure_line=""
    if [[ "$status" -ne 0 ]]; then
        _failure_line="$(extract_first_failure "$log" "$status")"
    fi
    rm -f "$log"

    # Timeout exit codes from the watchdog:
    #   143 = SIGTERM (128+15): watchdog's initial soft kill reached the child
    #   137 = SIGKILL (128+9): watchdog's hard-kill fallback fired
    if [[ "$status" -eq 137 || "$status" -eq 143 ]]; then
        echo "==> TIMEOUT: '$cmd' exceeded ${cmd_timeout}s budget and was killed."
        # The watchdog verdict outranks whatever the truncated log happened to
        # contain: a killed gate did not fail on that line, it ran out of time.
        _failure_line="exceeded the ${cmd_timeout}s budget and was killed (exit $status)"
    fi

    if [[ "$status" -ne 0 ]]; then
        echo "<-- $cmd  elapsed ${_elapsed_s}s  FAILED (exit $status)"
        echo "    first failure: $_failure_line"
        emit_failure_annotation "$cmd" "$_failure_line"
    else
        echo "<-- $cmd  elapsed ${_elapsed_s}s  ok"
    fi

    append_profile_entry "$cmd" "$_elapsed_s" "$status" "command"

    return "$status"
}

PREFLIGHT_FAILURES=()
PREFLIGHT_EXECUTED_COMMANDS=()
PREFLIGHT_CMD_ELAPSED=()
PREFLIGHT_CMD_STATUS=()
PREFLIGHT_CMD_FAILURE=()
STOPPED_EARLY=0
if [[ ${#WARMUP_COMMANDS[@]} -gt 0 ]] && ! run_warmup; then
    PREFLIGHT_FAILURES+=("warm-up")
    STOPPED_EARLY=1
else
    for cmd in "${COMMANDS[@]+"${COMMANDS[@]}"}"; do
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
        PREFLIGHT_CMD_FAILURE+=("$_failure_line")

        if (( status != 0 && FAIL_FAST == 1 )); then
            STOPPED_EARLY=1
            echo "==> Stopping after first failed command (--fail-fast)."
            break
        fi
    done
fi

PREFLIGHT_OVERALL_ELAPSED=$(( SECONDS - PREFLIGHT_OVERALL_START ))

# Summary table.
echo ""
echo "==> Preflight summary (${PREFLIGHT_OVERALL_ELAPSED}s total)"
if [[ ${#WARMUP_COMMANDS[@]} -gt 0 ]]; then
    warmup_status_label="ok"
    if [[ "$PREFLIGHT_WARMUP_STATUS" -ne 0 ]]; then
        warmup_status_label="FAILED"
    fi
    printf "    warm-up  %ss  [%s]\n" "$PREFLIGHT_WARMUP_ELAPSED" "$warmup_status_label"
fi
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

# ── Result table ──────────────────────────────────────────────────────────────
#
# The same table goes to three destinations so a local run and a hosted shard
# hand back identical information: the job's step summary (GitHub renders it
# above the log), a file under .tmp/ for local reads and log capture, and — for
# failures — one ::error annotation per command, emitted at failure time above.
# A literal backtick in a printf format string reads as a command
# substitution to shellcheck; keep it as data.
MD_CODE_TICK='`'

escape_markdown_cell() {
    local text="$1"
    text="${text//|/\\|}"
    text="${text//$'\n'/ }"
    printf '%s' "$text"
}

render_result_table() {
    local heading="Hew CI preflight — $PROFILE_LABEL profile"
    if (( SHARD_INDEX > 0 )); then
        heading="$heading (shard $SHARD_INDEX/$SHARD_COUNT)"
    fi
    printf '### %s\n\n' "$heading"
    printf '%ss total, %s failure(s).\n\n' \
        "$PREFLIGHT_OVERALL_ELAPSED" "${#PREFLIGHT_FAILURES[@]}"
    printf '| Command | Elapsed | Result | First failure |\n'
    printf '| --- | ---: | --- | --- |\n'

    if [[ ${#WARMUP_COMMANDS[@]} -gt 0 ]]; then
        local warmup_label="ok"
        local warmup_detail=""
        local warmup_cmd="warm-up"
        if [[ "$PREFLIGHT_WARMUP_STATUS" -ne 0 ]]; then
            warmup_label="FAILED"
            warmup_cmd="warm-up: $PREFLIGHT_WARMUP_FAILED_COMMAND"
            warmup_detail="$PREFLIGHT_WARMUP_FAILURE_LINE"
        fi
        printf '| %s%s%s | %ss | %s | %s |\n' \
            "$MD_CODE_TICK" \
            "$(escape_markdown_cell "$warmup_cmd")" \
            "$MD_CODE_TICK" \
            "$PREFLIGHT_WARMUP_ELAPSED" \
            "$warmup_label" \
            "$(escape_markdown_cell "$warmup_detail")"
    fi

    local index=0
    local entry entry_elapsed entry_status entry_label entry_failure
    for entry in "${PREFLIGHT_EXECUTED_COMMANDS[@]+"${PREFLIGHT_EXECUTED_COMMANDS[@]}"}"; do
        entry_elapsed="${PREFLIGHT_CMD_ELAPSED[$index]:-?}"
        entry_status="${PREFLIGHT_CMD_STATUS[$index]:-0}"
        entry_failure="${PREFLIGHT_CMD_FAILURE[$index]:-}"
        entry_label="ok"
        if [[ "$entry_status" -ne 0 ]]; then
            entry_label="FAILED (exit $entry_status)"
        fi
        printf '| %s%s%s | %ss | %s | %s |\n' \
            "$MD_CODE_TICK" \
            "$(escape_markdown_cell "$entry")" \
            "$MD_CODE_TICK" \
            "$entry_elapsed" \
            "$entry_label" \
            "$(escape_markdown_cell "$entry_failure")"
        index=$(( index + 1 ))
    done

    if (( STOPPED_EARLY == 1 )); then
        printf '\nRemaining commands were not run.\n'
    fi
}

PREFLIGHT_RESULT_TABLE="$(render_result_table)"

mkdir -p "$(dirname "$PREFLIGHT_SUMMARY_FILE")"
printf '%s\n' "$PREFLIGHT_RESULT_TABLE" > "$PREFLIGHT_SUMMARY_FILE"
echo "Result table written to: $PREFLIGHT_SUMMARY_FILE"

if [[ -n "${GITHUB_STEP_SUMMARY:-}" ]]; then
    printf '%s\n\n' "$PREFLIGHT_RESULT_TABLE" >> "$GITHUB_STEP_SUMMARY"
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
