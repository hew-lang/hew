#!/usr/bin/env bash
# ci-preflight-dispatcher.sh — classify the current diff and run the narrowest
# sufficient set of checks for it.
#
# Every `make preflight` goes through here, so this file decides what a branch
# is gated on before it is pushed. It reads the changed paths, picks one LANE
# (the primary classification: docs, scripts-config, parser, types,
# compiler-pipeline, runtime-net, cli, wasm, hew-tests, or the fail-closed
# fallback), and appends side-channel gates for changes whose blast radius the
# lane does not cover -- a structural ratchet outside the cargo dependency
# graph, an ll-oracle golden the lane never diffs.
#
# The default when a path matches nothing is the fallback lane: the widest set,
# not the narrowest. A path nobody classified is a path nobody reasoned about.
#
# Every selected command runs under a wall-clock budget scaled to the host's
# parallelism, so a hang fails the preflight instead of holding it open.
#
# Usage:
#   scripts/ci-preflight-dispatcher.sh [--dry-run] [--fail-fast] [--base <ref>]
#   scripts/ci-preflight-dispatcher.sh --help
#
# Its own routing and timeout counterfactuals are `make test-build-harness`.

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
        "make test-obligation-advisory-corpus") echo 120 ;;
        "make test-obligation-advisory-runner-selftest") echo 30 ;;
        "make test-o2-differential") echo 2700 ;;
        "make o2-differential-selftest") echo 30 ;;
        "make doc-ratchet-selftest") echo 45 ;;
        "make test-release-workflow-contract") echo 30 ;;
        "make test-stdlib-ratchet") echo 45 ;;
        # The full 75-module user-build gate measured about 85 s. Add roughly
        # 35% cache and host-load headroom while retaining a finite ceiling.
        "make stdlib-user-build-clean") echo 115 ;;
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
        "make test-obligation-advisory-corpus"|"make test-obligation-advisory-runner-selftest")
            echo "group:obligation-corpus" ;;
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
    local tier="$CMD_TIMEOUT"
    # A `make` gate builds its own prerequisites, so its stuck ceiling is the
    # wide tier wherever it is selected; the cargo commands keep the narrow one.
    if [[ "$cmd" == make\ * ]] && (( PREFLIGHT_TIMEOUT_FALLBACK > tier )); then
        tier="$PREFLIGHT_TIMEOUT_FALLBACK"
    fi
    floor="$(command_timeout_floor "$cmd")"
    if (( floor > tier )); then
        baseline="$floor"
    else
        baseline="$tier"
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

extract_failure_lines() {
    local log="$1"
    local failure_line_re="${2:-$PREFLIGHT_FAILURE_LINE_RE}"
    local counterfactual_marker="${3:-$PREFLIGHT_COUNTERFACTUAL_MARKER}"

    [[ -s "$log" ]] || return 0
    sed $'s/\033\\[[0-9;?]*[a-zA-Z]//g' "$log" \
        | grep -v -E "^${counterfactual_marker}" \
        | grep -E "$failure_line_re" || true
}

extract_first_failure() {
    local log="$1"
    local status="$2"
    local line=""

    line="$(extract_failure_lines "$log" | sed -n '1p')"
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
# The shared-artifact inventory declares which roster members need compiled
# inputs. The default selection runs only artifact-free checks, while the
# companion Make target runs the declared shared-artifact checks after building
# their prerequisites.
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

COUNTERFACTUAL_GATE_REQUIREMENTS_FILE="${REPO_ROOT}/hew-testutil/shared-test-artifacts.tsv"

counterfactual_gate_requirement() {
    local target="$1"
    local requirement

    requirement="$(
        awk -F '\t' -v target="$target" \
            '$1 == "gate" && $2 == target { print $3 }' \
            "$COUNTERFACTUAL_GATE_REQUIREMENTS_FILE"
    )"
    case "$requirement" in
        "")
            printf '%s\n' "artifact-free"
            ;;
        shared-artifacts)
            printf '%s\n' "$requirement"
            ;;
        *)
            die "invalid shared-artifact requirement for $target: $requirement"
            ;;
    esac
}

run_counterfactual_output_check() {
    local cmd target requirement output offenders marked log
    local status=0
    local failures=0
    local roster=()

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
    else
        for cmd in "${COUNTERFACTUAL_ROSTER[@]}"; do
            target="${cmd#make }"
            requirement="$(counterfactual_gate_requirement "$target")"
            if [[ "$requirement" == "shared-artifacts" ]] && (( SHARED_ARTIFACT_GATES == 0 )); then
                continue
            fi
            if [[ "$requirement" == "artifact-free" ]] && (( SHARED_ARTIFACT_GATES == 1 )); then
                continue
            fi
            roster+=("$cmd")
        done
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
        log="$(mktemp "${TMPDIR:-/tmp}/hew-counterfactual-output.XXXXXX")"
        printf '%s\n' "$output" > "$log"
        offenders="$(
            extract_failure_lines \
                "$log" \
                "$PREFLIGHT_FAILURE_LINE_RE" \
                "${PREFLIGHT_COUNTERFACTUAL_MARKER}"
        )"
        rm -f "$log"
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
CLASSIFY_ONLY=0
CHECK_COUNTERFACTUAL_OUTPUT=0
SHARED_ARTIFACT_GATES=0
BASE_REF=""
EXPLICIT_PATHS=0
LANE_REASON=""
CHANGED_FILES=()
CHANGED_CRATE_DIRS=()
COMMANDS=()
WARMUP_COMMANDS=()
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
- --classify            Print '<path>\t<gate,gate,...>' for each path and exit. With no
                        paths, reads newline-separated paths from stdin.
- Routing rule: a changed path selects every gate whose declared `# inputs:` it matches,
  and the preflight runs the union of those gates over the reverse-dependency closure of
  the changed crates. A path matching no declaration and no positive no-gate pattern is
  undeclared: it fails closed to comprehensive and is named in the Reason line.
- If the first-slice routing is unclear, the script runs the broader local check profile.
- --explain-warmup <cmd> Print the warm-up derived from <cmd> and exit; fails when <cmd>
                        has no derivable warm-up.
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
- --shared-artifact-gates
                        With --check-counterfactual-output, select only rostered checks
                        declared to require shared artifacts.
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

# Collect from `git diff --name-status`, taking BOTH sides of a rename or copy.
#
# WHY: `--name-only` reports only the destination of an R/C entry, so moving a
# file out of one declared set and into another dropped the gate that owned the
# OLD path — precisely the change most likely to break it.  A rename is a
# change to both locations, and both are routed.
collect_paths_from_status() {
    local status=""
    local first=""
    local second=""
    while IFS=$'\t' read -r status first second; do
        [[ -n "$status" ]] || continue
        case "$status" in
            R*|C*)
                [[ -n "$first" ]] && append_unique_path "$(normalize_path "$first")"
                [[ -n "$second" ]] && append_unique_path "$(normalize_path "$second")"
                ;;
            *)
                [[ -n "$first" ]] && append_unique_path "$(normalize_path "$first")"
                ;;
        esac
    done < <("$@")
    return 0
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

# ═══════════════════════════════════════════════════════════════════════════
# GATE SELECTION.
#
#   Every gate DECLARES the paths it reads, on an `# inputs:` line above its
#   recipe in the Makefile.  This dispatcher selects every gate whose declared
#   inputs intersect the diff.  The comprehensive profile is every
#   participating gate, so it is a SUPERSET of any narrower selection by
#   construction — not by a curated list somebody has to keep in step.
#
#   There is no separate path table. A gate that grows a new input root is
#   edited next to the recipe that reads it.
#
#   Three declarations, all in the Makefile, all machine-read here and asserted
#   by scripts/check-gate-reachability.py (A6/A7):
#
#     # inputs: <glob>...          the paths a gate reads
#     # preflight: <marker>        never / comprehensive-only, with the reason
#     # global-input: <glob> — ..  parameterises every gate; forces comprehensive
#     # no-gate: <glob> — ..       positive inert-path allowlist
#
#   A gate whose only declared input is the bare `*` is a TREE-WIDE SCANNER
#   (leak-scan, lint-wasm-todo): it is always selected, and it deliberately does
#   not count as "something reads this path" — otherwise no path could ever be
#   unclassified and the fail-closed answer would be unreachable.
#
#   Glob syntax: `*` matches any characters including `/`.
#
#   scripts/lib/gate_inputs.py also scans likely consumers as an advisory
#   lint. Static analysis is incomplete, so scan results never select a gate,
#   widen the no-gate set, or otherwise narrow this plan.

# Runs the selector over the changed paths.  Sets:
#   SELECTED_GATES   make targets to run, in Makefile order
#   UNDECLARED_PATHS   paths no gate declares and no `no-gate` entry covers
#   GLOBAL_PATHS     paths declared as global inputs
#   COMPREHENSIVE    1 when the whole gate set must run
#   NEEDS_RUST_CLOSURE 1 when a Rust input changed
select_gates() {
    local mode="$1"
    shift
    local key value
    SELECTED_GATES=()
    UNDECLARED_PATHS=()
    GLOBAL_PATHS=()
    COMPREHENSIVE=0
    NEEDS_RUST_CLOSURE=0
    # The selector's exit status is CHECKED, and its stream must end with the
    # END sentinel.  Reading it through process substitution discarded both: a
    # selector that raised produced no records, which read as "no gate matches"
    # and narrowed the plan to `cargo fmt` with exit 0.  A router that cannot
    # decide must run everything or stop; it must never quietly run less.
    local records=""
    local status=0
    local selector=(python3 "$REPO_ROOT/scripts/lib/gate_inputs.py" select "$REPO_ROOT" --mode "$mode")
    # Test-only selector replacement, behind the same sentinel as the command
    # and warm-up overrides, so the fail-closed path can be exercised without a
    # second copy of the repository.
    if [[ -n "${PREFLIGHT_TEST_SELECTOR:-}" ]]; then
        if [[ "${PREFLIGHT_TEST_ALLOW_OVERRIDE:-}" != "1" ]]; then
            die "PREFLIGHT_TEST_SELECTOR requires PREFLIGHT_TEST_ALLOW_OVERRIDE=1 for test-only use"
        fi
        selector=(bash -c "$PREFLIGHT_TEST_SELECTOR")
    fi
    records="$(printf '%s\n' "$@" | "${selector[@]}")" || status=$?
    if (( status != 0 )); then
        die "gate selector failed (exit $status); refusing to narrow the preflight on an undecided routing"
    fi
    local saw_end=0
    while IFS=' ' read -r key value; do
        case "$key" in
            MODE)
                case "$value" in
                    selected) ;;
                    comprehensive) COMPREHENSIVE=1 ;;
                    *) die "selector emitted an invalid MODE value: $value" ;;
                esac
                ;;
            GATE) SELECTED_GATES+=("$value") ;;
            UNDECLARED) UNDECLARED_PATHS+=("$value") ;;
            GLOBAL) GLOBAL_PATHS+=("$value") ;;
            RUSTCLOSURE)
                case "$value" in
                    0) ;;
                    1) NEEDS_RUST_CLOSURE=1 ;;
                    *) die "selector emitted an invalid RUSTCLOSURE value: $value" ;;
                esac
                ;;
            END) saw_end=1 ;;
            "") ;;
            *) die "selector emitted an unknown record: $key $value" ;;
        esac
    done <<< "$records"
    if (( saw_end != 1 )); then
        die "gate selector produced a truncated record stream (no END); refusing to narrow the preflight on a partial routing"
    fi
}

# Warm artifact construction, ahead of the per-command watchdog budgets, so
# one-time compilation is not measured against a timed gate's hang ceiling.
#
# Every warm-up is DERIVED from the gate it warms; a second, hand-written list
# of build commands is what turned main red for four hours, when a `cargo build
# --workspace --all-targets` warm-up sat beside a `cargo nextest run --workspace
# --exclude hew-cabi` gate and the two builds put incompatible units in one
# target dir (the root `panic = "abort"` cannot apply to a libtest harness).
#
#   cargo fmt ...            nothing (rustfmt parses source)
#   cargo clippy ... -- ...  the same invocation without the trailing
#                            `-- -D warnings`, so a lint failure is the timed
#                            gate's verdict and not an aborted warm-up
#   cargo nextest run ...    the same invocation plus --no-run
#   cargo test ...           the same invocation plus --no-run
#   make <target>...         make <target>-build..., declared NEXT TO <target>
#                            in the Makefile (the test-cabi-build precedent)
#
# A gate with no derivable warm-up is fatal: the fallback that warmed the whole
# artifact set let a new gate reopen the divergence without anyone deciding to.
add_warmup_command() {
    local candidate="$1"
    local existing
    for existing in "${WARMUP_COMMANDS[@]+"${WARMUP_COMMANDS[@]}"}"; do
        [[ "$existing" == "$candidate" ]] && return 0
    done
    WARMUP_COMMANDS+=("$candidate")
}

# Resolve every warm-up build form through make itself, in one pass.
# `make --always-make --dry-run` prints the commands make WOULD run, with the
# Makefile's own conditionals applied and freshness taken out of the answer.
# Reading the Makefile text instead cannot answer this: `test-hew-ratchet-build`
# and `test-o2-differential-build` are each declared twice, once in a
# shard-aggregate branch that builds nothing, and a first-match text scan reads
# that branch's empty recipe as the answer for the branch that builds the
# compiler.  A missing rule makes the pass fail, so an undeclared build form
# cannot pass as an empty one.
#
# The pass interleaves a marker goal before each build form and splits its
# output on the marker, so one make invocation answers for every gate.  make
# builds each target once per invocation, so a form whose only work is a
# prerequisite an earlier form already covers reports no work of its own — and
# is left out of the warm-up, which the earlier form has already warmed.
PLAN_TARGETS_WITH_WORK=""

collect_make_build_targets() {
    local cmd target
    local -a targets=()
    for cmd in "${COMMANDS[@]}"; do
        [[ "$cmd" == "make "* ]] || continue
        read -r -a targets <<< "${cmd#make }"
        for target in "${targets[@]}"; do
            if [[ "$target" == -* || "$target" == *=* ]]; then
                die "warm-up derivation for '$cmd' is undefined: only bare make targets are derivable, got '$target'"
            fi
            printf '%s\n' "${target}-build"
        done
    done
}

# make --question exits 2 for a target it has no rule for, 0 or 1 otherwise,
# and runs nothing either way.  Only reached when the plan pass has already
# failed, so the cost of one invocation per target does not matter.
make_has_no_rule_for() {
    local status=0
    make --question "$1" >/dev/null 2>&1 || status=$?
    [[ "$status" -eq 2 ]]
}

diagnose_missing_build_form() {
    local cmd target
    local -a targets=()
    for cmd in "${COMMANDS[@]}"; do
        [[ "$cmd" == "make "* ]] || continue
        read -r -a targets <<< "${cmd#make }"
        for target in "${targets[@]}"; do
            if make_has_no_rule_for "$target"; then
                die "gate '$cmd' names an undeclared make target '$target'"
            fi
            if make_has_no_rule_for "${target}-build"; then
                die "gate '$cmd' has no derivable warm-up: declare '${target}-build' next to '${target}' in the Makefile (see test-cabi-build), building what '${target}' needs and running nothing"
            fi
        done
    done
    die "make could not plan the warm-up for the selected gates"
}

plan_make_build_forms() {
    local -a goals=()
    local target plan line section=""
    while IFS= read -r target; do
        [[ -n "$target" ]] || continue
        case " ${goals[*]-} " in
            *" $target "*) continue ;;
        esac
        goals+=("preflight-plan-mark-${target}" "$target")
    done < <(collect_make_build_targets)
    [[ ${#goals[@]} -gt 0 ]] || return 0

    plan="$(make --always-make --dry-run "${goals[@]}" 2>&1)" || diagnose_missing_build_form

    while IFS= read -r line; do
        case "$line" in
            *"==preflight-plan=="*)
                section="${line##*==preflight-plan==}"
                section="${section%\"}"
                continue
                ;;
        esac
        [[ -n "$section" ]] || continue
        [[ -z "${line//[[:space:]]/}" || "$line" == ":" || "$line" == make* ]] && continue
        PLAN_TARGETS_WITH_WORK="$PLAN_TARGETS_WITH_WORK $section"
        section=""
    done <<< "$plan"
}

derive_make_warmup() {
    local cmd="$1"
    local -a targets=()
    local -a build_targets=()
    local target
    read -r -a targets <<< "${cmd#make }"
    for target in "${targets[@]}"; do
        case " $PLAN_TARGETS_WITH_WORK " in
            *" ${target}-build "*) build_targets+=("${target}-build") ;;
        esac
    done
    [[ ${#build_targets[@]} -gt 0 ]] || return 0
    add_warmup_command "make ${build_targets[*]}"
}

# Clippy warms through clippy: its check artifacts carry a different
# fingerprint from rustc's.  Only the lint arguments after `--` are dropped, so
# a lint failure is the timed gate's verdict rather than an aborted warm-up.
# Everything else after `--` is a rustc argument that changes what is compiled,
# and dropping it would make the warm-up build something other than the gate --
# the divergence this derivation exists to prevent.
derive_clippy_warmup() {
    local cmd="$1"
    local -a kept=()
    local head="${cmd%% -- *}"
    local rest="" token skip=0
    [[ "$cmd" == *" -- "* ]] && rest="${cmd#* -- }"
    for token in $rest; do
        if (( skip == 1 )); then
            skip=0
            continue
        fi
        case "$token" in
            -D|-W|-A|-F) skip=1 ;;
            -D*|-W*|-A*|-F*) ;;
            *) kept+=("$token") ;;
        esac
    done
    if [[ ${#kept[@]} -gt 0 ]]; then
        add_warmup_command "$head -- ${kept[*]}"
    else
        add_warmup_command "$head"
    fi
}

derive_warmup() {
    local cmd="$1"
    case "$cmd" in
        "cargo fmt "*)
            ;;
        "cargo clippy "*)
            derive_clippy_warmup "$cmd"
            ;;
        "cargo nextest run "*|"cargo test "*)
            # Test processes may only read the shared libhew archive. Build and
            # certify it once before any nextest process exists; the executed
            # command receives HEW_TEST_NO_BUILD below and fails closed if a
            # nested path attempts to rebuild it.
            add_warmup_command "make stdlib"
            if [[ "$cmd" == *" --no-run"* ]]; then
                add_warmup_command "$cmd"
            else
                add_warmup_command "$cmd --no-run"
            fi
            ;;
        "make "*)
            derive_make_warmup "$cmd"
            ;;
        *)
            die "gate '$cmd' has no derivable warm-up: express it as a make target with a '<target>-build' form, or as a cargo invocation with a no-execute flag"
            ;;
    esac
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
        --classify)
            CLASSIFY_ONLY=1
            shift
            ;;
        --explain-warmup)
            shift
            [[ $# -gt 0 ]] || die "--explain-warmup requires a command"
            COMMANDS=("$1")
            plan_make_build_forms
            derive_warmup "$1"
            [[ ${#WARMUP_COMMANDS[@]} -eq 0 ]] || printf '%s\n' "${WARMUP_COMMANDS[@]}"
            exit 0
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
        --shared-artifact-gates)
            SHARED_ARTIFACT_GATES=1
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

if (( SHARED_ARTIFACT_GATES == 1 && CHECK_COUNTERFACTUAL_OUTPUT == 0 )); then
    die "--shared-artifact-gates requires --check-counterfactual-output"
fi

if (( CHECK_COUNTERFACTUAL_OUTPUT == 1 )); then
    run_counterfactual_output_check
    exit $?
fi

ci_preflight_base_unresolved=0

if (( EXPLICIT_PATHS == 0 && CLASSIFY_ONLY == 0 )); then
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
        collect_paths_from_status git diff --name-status --diff-filter=ACMRD "$BASE_REF...HEAD"
    fi
    collect_paths_from_status git diff --cached --name-status --diff-filter=ACMRD
    collect_paths_from_status git diff --name-status --diff-filter=ACMRD
    collect_paths_from_command git ls-files --others --exclude-standard
fi

if (( CLASSIFY_ONLY == 1 )); then
    # Streamed through the same selector the routing uses, one path at a time,
    # so the answer this prints is the answer the routing would give.
    classify_paths=()
    if has_changed_files; then
        classify_paths=("${CHANGED_FILES[@]}")
    else
        while IFS= read -r classify_input; do
            [[ -n "$classify_input" ]] || continue
            classify_paths+=("$(normalize_path "$classify_input")")
        done
    fi
    if (( ${#classify_paths[@]} > 0 )); then
        printf '%s\n' "${classify_paths[@]}" \
            | python3 "$REPO_ROOT/scripts/lib/gate_inputs.py" classify "$REPO_ROOT"
    fi
    exit 0
fi

if ! has_changed_files; then
    echo "==> Hew CI preflight dispatcher"
    echo "No changed files detected."
    exit 0
fi

for path in "${CHANGED_FILES[@]}"; do
    # Nearest ancestor carrying a Cargo.toml, so nested workspace members
    # (std/encoding/binary, std/text/template) reach the closure below.
    crate_dir="${path%/*}"
    while [[ "$crate_dir" != "$path" && -n "$crate_dir" && "$crate_dir" != "." ]]; do
        if [[ -f "$REPO_ROOT/$crate_dir/Cargo.toml" ]]; then
            append_unique_crate "$crate_dir"
            break
        fi
        [[ "$crate_dir" == */* ]] || break
        crate_dir="${crate_dir%/*}"
    done
done

select_gates selected "${CHANGED_FILES[@]}"

AFFECTED_PACKAGE_ARGS=""
if [[ ${CHANGED_CRATE_DIRS[0]+set} == set ]]; then
    while IFS= read -r package; do
        [[ -n "$package" ]] || continue
        AFFECTED_PACKAGE_ARGS="$AFFECTED_PACKAGE_ARGS -p $package"
    done < <(
        cargo metadata --no-deps --format-version 1 | python3 -c '
import json, pathlib, sys
changed = set(sys.argv[1:])
metadata = json.load(sys.stdin)
workspace_root = pathlib.Path(metadata["workspace_root"])
packages = metadata["packages"]


def relative_dir(package):
    directory = pathlib.Path(package["manifest_path"]).parent
    try:
        return str(directory.relative_to(workspace_root))
    except ValueError:
        return str(directory)


selected = {
    package["name"]
    for package in packages
    if relative_dir(package) in changed
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

# Formatting is first everywhere: it is seconds, and a format failure should not
# wait behind a compile.
add_command "cargo fmt --all -- --check"

if (( COMPREHENSIVE == 1 )); then
    # The comprehensive profile runs the workspace forms of the two closure
    # commands.  `cargo clippy --workspace --tests` subsumes any closure clippy
    # and `make test` subsumes any closure nextest, which is what keeps
    # comprehensive a superset of every narrower selection.
    add_command "cargo clippy --workspace --tests -- -D warnings"
elif (( NEEDS_RUST_CLOSURE == 1 )); then
    add_command "cargo clippy$AFFECTED_PACKAGE_ARGS --tests -- -D warnings"
    add_command "cargo nextest run --profile ci$AFFECTED_PACKAGE_ARGS"
fi

for selected_gate in "${SELECTED_GATES[@]+"${SELECTED_GATES[@]}"}"; do
    if [[ "$selected_gate" == "check-counterfactual-output" ]] && (( COMPREHENSIVE == 1 )); then
        continue
    fi
    add_command "make $selected_gate"
done

if (( COMPREHENSIVE == 1 )); then
    add_command "make check-counterfactual-output-artifacts"
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

BASELINE_COMMANDS=("${COMMANDS[@]+"${COMMANDS[@]}"}")

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
# The profile label is the gate count because gates are selected directly from
# their own declared inputs.
if (( COMPREHENSIVE == 1 )); then
    PROFILE_LABEL="comprehensive"
elif (( ${#SELECTED_GATES[@]} == 0 )); then
    PROFILE_LABEL="no-gate"
else
    PROFILE_LABEL="selected(${#SELECTED_GATES[@]})"
fi

if (( ${#UNDECLARED_PATHS[@]} > 0 )); then
    LANE_REASON="comprehensive: $(printf 'undeclared: %s; ' "${UNDECLARED_PATHS[@]}")"
    LANE_REASON="${LANE_REASON%; }"
elif (( ${#GLOBAL_PATHS[@]} > 0 )); then
    LANE_REASON="comprehensive: a global input changed, which parameterises every gate — ${GLOBAL_PATHS[*]}"
elif (( COMPREHENSIVE == 1 )); then
    LANE_REASON="comprehensive: every participating gate"
else
    LANE_REASON="${#SELECTED_GATES[@]} gate(s) whose declared inputs intersect the diff (see the # inputs: line above each recipe in the Makefile)"
fi

# The command override replaces the gate list with synthetic commands for the
# failure-policy tests; those are not gates and have nothing to warm.
if [[ -z "${PREFLIGHT_TEST_COMMANDS:-}" ]]; then
    plan_make_build_forms
    for cmd in "${COMMANDS[@]}"; do
        derive_warmup "$cmd"
    done
fi

# Test-only warm-up replacement.  Keep this behind the same explicit sentinel
# as command replacement so normal preflight invocations always warm real
# artifacts.
if [[ -n "${PREFLIGHT_TEST_WARMUP_COMMANDS:-}" ]]; then
    if [[ "${PREFLIGHT_TEST_ALLOW_OVERRIDE:-}" != "1" ]]; then
        die "PREFLIGHT_TEST_WARMUP_COMMANDS requires PREFLIGHT_TEST_ALLOW_OVERRIDE=1 for test-only use; unset PREFLIGHT_TEST_WARMUP_COMMANDS to run the normal preflight."
    fi
    echo "warning: PREFLIGHT_TEST_WARMUP_COMMANDS override active (test-only); replacing warm-up commands." >&2
    WARMUP_COMMANDS=()
    while IFS= read -r test_cmd; do
        [[ -n "$test_cmd" ]] || continue
        WARMUP_COMMANDS+=("$test_cmd")
    done <<< "$PREFLIGHT_TEST_WARMUP_COMMANDS"
fi

# Whether a toolchain is needed is read off the routing decision and the
# warm-up pass, never derived a second time here: a Rust input in the diff means
# the closure clippy and nextest runs compile, and a non-empty warm-up set means
# make itself planned work for a selected gate's build form.
#
# Both are properties of the SELECTION, so the answer survives the test-only
# command override, which replaces the gate list with a no-op.
REQUIRES_COMPILE=false
if (( COMPREHENSIVE == 1 || NEEDS_RUST_CLOSURE == 1 )) \
    || [[ ${WARMUP_COMMANDS[0]+set} == set ]]; then
    REQUIRES_COMPILE=true
fi

# Assert the supported developer compiler profile on every route that can
# exercise code or build configuration.
if [[ "$REQUIRES_COMPILE" == "true" ]]; then
    add_command "make hew-profile-check"
fi

if [[ -n "$GITHUB_OUTPUT_PATH" ]]; then
    {
        printf 'profile=%s\n' "$PROFILE_LABEL"
        printf 'requires_compile=%s\n' "$REQUIRES_COMPILE"
    } >> "$GITHUB_OUTPUT_PATH"
fi

# Job annotations for the routing decision: the selected profile, and every
# path no gate declares.  Annotations only — the step summary carries the
# result table, and a second writer there would make the two disagree.  A run that is silently comprehensive is otherwise
# invisible without opening a 37,000-line log, and an undeclared path is a cost
# somebody has to see in order to fix.
if [[ -n "${GITHUB_ACTIONS:-}" ]]; then
    printf '::notice title=Preflight profile::%s — %s\n' "$PROFILE_LABEL" "$LANE_REASON"
    for undeclared in "${UNDECLARED_PATHS[@]+"${UNDECLARED_PATHS[@]}"}"; do
        printf '::warning file=%s::%s is not a declared input of any gate. Add it to the inputs line of the gate that reads it, or record it as no-gate in the Makefile header.\n' \
            "$undeclared" "$undeclared"
    done
fi

# Resolve the per-command timeout budget.  Per-command floors below still
# override this tier; it is only the baseline stuck ceiling.
if (( COMPREHENSIVE == 1 )); then
    CMD_TIMEOUT="$PREFLIGHT_TIMEOUT_FALLBACK"
elif [[ "$REQUIRES_COMPILE" == "true" ]]; then
    CMD_TIMEOUT="$PREFLIGHT_TIMEOUT_NARROW"
else
    CMD_TIMEOUT="$PREFLIGHT_TIMEOUT_DOCS"
fi

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

# ── Baseline precheck ─────────────────────────────────────────────────────────
# Committed derived baselines — goldens, generated consumers, ratcheted
# expected-failure lists — drift whenever main moves under a branch, and every
# gate that compares against one sits at the far end of this lane.  That is why
# a stale baseline used to be an hour-deep CI red on an unrelated pull request.
# The fast-tier members need no compiler build, so they are proved FIRST, before
# warm-up, and only for the gates this lane actually runs; scripts/baselines.py
# owns both the membership and the regen command it prints for a stale artefact.
BASELINE_LANE_FILE=".tmp/preflight-lane.txt"
BASELINE_PRECHECK="make baselines-check BASELINE_TIER=fast BASELINE_GATES=$BASELINE_LANE_FILE"
RUN_BASELINE_PRECHECK=1
if (( SHARD_INDEX > 1 )); then
    RUN_BASELINE_PRECHECK=0
fi
if (( RUN_BASELINE_PRECHECK == 1 )); then
    echo "Baseline precheck:"
    echo "  - $BASELINE_PRECHECK"
else
    echo "Baseline precheck: runs in shard 1/$SHARD_COUNT before warm-up."
fi

if [[ ${#WARMUP_COMMANDS[@]} -gt 0 ]]; then
    echo "Warm-up:"
    for cmd in "${WARMUP_COMMANDS[@]}"; do
        echo "  - $cmd"
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
    local phase="${2:-command}"
    local executed_cmd="$cmd"
    local cmd_timeout
    local start=$SECONDS
    local status=0
    local log
    local pipe_status
    cmd_timeout="$(command_timeout "$cmd")"

    echo ""
    echo "==> $cmd"

    case "$cmd" in
        "cargo nextest run "*|"cargo test "*)
            executed_cmd="HEW_TEST_NO_BUILD=1 $cmd"
            ;;
    esac

    log="$(mktemp "${TMPDIR:-/tmp}/hew-preflight-cmd.XXXXXX")"
    # tee, not a redirect: the log is only a diagnostic side-channel; the
    # command's output must still stream to the job log in real time so a
    # hung gate is visible before its watchdog fires.  PIPESTATUS[0] carries
    # the command's real exit code — tee's status must never stand in for it.
    set +e
    run_in_pgroup_with_timeout "$cmd_timeout" "$executed_cmd" 2>&1 | tee "$log"
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

    append_profile_entry "$cmd" "$_elapsed_s" "$status" "$phase"

    return "$status"
}

# Toolchain pin. The workflows once installed whatever taiki-e/install-action
# resolved that hour, so `--no-pager` (a recent nextest release) was
# accepted or rejected by the same commit on different days: three reds, each after a full
# job. scripts/tests/test_tool_pin_contract.py now holds the pin for the whole
# build system and proves every installer matches it; this reads that same
# value rather than declaring a second one, and refuses to run gates on an
# older build.
assert_nextest_pin() {
    local contract="scripts/tests/test_tool_pin_contract.py"
    local pin installed oldest reported
    if [[ -n "${PREFLIGHT_TEST_NEXTEST_PIN:-}" || -n "${PREFLIGHT_TEST_NEXTEST_VERSION:-}" ]]; then
        if [[ "${PREFLIGHT_TEST_ALLOW_OVERRIDE:-}" != "1" ]]; then
            die "PREFLIGHT_TEST_NEXTEST_PIN and PREFLIGHT_TEST_NEXTEST_VERSION require PREFLIGHT_TEST_ALLOW_OVERRIDE=1 for test-only use."
        fi
        [[ -n "${PREFLIGHT_TEST_NEXTEST_PIN:-}" ]] ||
            die "PREFLIGHT_TEST_NEXTEST_PIN must accompany PREFLIGHT_TEST_NEXTEST_VERSION."
        [[ -n "${PREFLIGHT_TEST_NEXTEST_VERSION:-}" ]] ||
            die "PREFLIGHT_TEST_NEXTEST_VERSION must accompany PREFLIGHT_TEST_NEXTEST_PIN."
        pin="$PREFLIGHT_TEST_NEXTEST_PIN"
        reported="cargo-nextest $PREFLIGHT_TEST_NEXTEST_VERSION (test fixture)"
    else
        pin="$(sed -n 's/^[[:space:]]*"NEXTEST": ("cargo-nextest", "\([0-9][0-9.]*\)").*/\1/p' "$contract")"
        reported="$(cargo nextest --version 2>/dev/null)"
    fi
    [[ -n "$pin" ]] || die "no cargo-nextest pin in $contract (PINS[\"NEXTEST\"])"
    installed="$(printf '%s\n' "$reported" | sed -n 's/^cargo-nextest \([0-9][0-9.]*\).*/\1/p' | head -1)"
    [[ -n "$installed" ]] || die "cargo-nextest is not installed; every test gate runs through it (cargo install cargo-nextest --locked)"
    oldest="$(printf '%s\n%s\n' "$pin" "$installed" | sort -t. -k1,1n -k2,2n -k3,3n | head -1)"
    [[ "$installed" == "$pin" || "$oldest" == "$pin" ]] ||
        die "cargo-nextest $installed is older than the pinned $pin; it rejects flags the gates pass (cargo install cargo-nextest --locked)"
    echo "cargo-nextest $installed satisfies the pinned $pin"
}

# A test-only command replacement executes no selected gate, so it must not
# acquire a tool that only those replaced commands consume.  The routing result
# still reports requires_compile above; only execution-time provisioning is
# synthetic here.
if [[ "$REQUIRES_COMPILE" == "true" ]] &&
    {
        [[ -z "${PREFLIGHT_TEST_COMMANDS:-}" ]] ||
            [[ -n "${PREFLIGHT_TEST_NEXTEST_VERSION:-}" ]] ||
            [[ -n "${PREFLIGHT_TEST_NEXTEST_PIN:-}" ]]
    }; then
    assert_nextest_pin
fi

PREFLIGHT_FAILURES=()
PREFLIGHT_EXECUTED_COMMANDS=()
PREFLIGHT_CMD_ELAPSED=()
PREFLIGHT_CMD_STATUS=()
PREFLIGHT_CMD_FAILURE=()
STOPPED_EARLY=0
PREFLIGHT_PRECHECK_STATUS=0
PREFLIGHT_PRECHECK_ELAPSED=0
if (( RUN_BASELINE_PRECHECK == 1 )); then
    # Shard 1 owns this once-per-lane check and uses the pre-sharded command
    # list, so baseline members covered by another shard are still checked first.
    mkdir -p "$(dirname "$REPO_ROOT/$BASELINE_LANE_FILE")"
    printf '%s\n' "${BASELINE_COMMANDS[@]}" > "$REPO_ROOT/$BASELINE_LANE_FILE"
    if ! run_timed_command "$BASELINE_PRECHECK" precheck; then
        PREFLIGHT_PRECHECK_STATUS=1
        PREFLIGHT_FAILURES+=("baseline precheck")
        STOPPED_EARLY=1
    fi
    PREFLIGHT_PRECHECK_ELAPSED="$_elapsed_s"
fi

if (( PREFLIGHT_PRECHECK_STATUS != 0 )); then
    :
elif [[ ${#WARMUP_COMMANDS[@]} -gt 0 ]] && ! run_warmup; then
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
if (( RUN_BASELINE_PRECHECK == 1 )); then
    precheck_status_label="ok"
    if (( PREFLIGHT_PRECHECK_STATUS != 0 )); then
        precheck_status_label="FAILED"
    fi
    printf "    %s  %ss  [%s]\n" "$BASELINE_PRECHECK" "$PREFLIGHT_PRECHECK_ELAPSED" "$precheck_status_label"
fi
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
