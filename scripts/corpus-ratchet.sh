#!/usr/bin/env bash
# corpus-ratchet.sh — every corpus-versus-expected-failures gate, one driver.
#
# A corpus ratchet answers one question: does the set of things that FAIL right
# now exactly equal the set recorded in an expected-failures file? A new failure
# is a regression; a listed failure that now passes is a recovery that needs
# its ledger row removed. Regressions are hard errors in every invocation.
# Recoveries are reported but non-blocking in PR gates, and become hard errors
# only when RATCHET_STRICT_RECOVERIES=1 (the scheduled ledger-accounting run).
#
# Four gates asked that question in four separate scripts:
#
#   hew-suite    make test-hew-ratchet    `hew test tests/hew/`
#   stdlib       make test-stdlib-ratchet `hew check` over std/**.hew
#   hew-corpus   make hew-check-all       `hew check` over the tracked corpus
#   doc-fences   make test-doc-examples   `hew check` over ```hew doc fences
#
# They differed in how the corpus is enumerated and run, and in the prose they
# print. The comparison itself — parse the list, diff the two sets, refuse an
# empty corpus, report both directions — was copied four times, so each copy
# was one edit away from drifting from the others. It now exists once, and a
# fifth corpus is a table entry rather than a fifth script.
#
# Each corpus supplies:
#   * a run hook that enumerates its corpus, executes it, and fills ACTUAL_STR
#     with the newline-delimited set of failing entry names;
#   * a summary hook that prints its own counts block (the four differ in
#     labels and column alignment);
#   * the strings the shared verdict prints, plus optional hooks for a
#     per-failure diagnostic, an extra failure class, and a trailing report.
#
# The set comparison uses scripts/lib/line-set.sh rather than a
# `producer | grep -q` pipeline: under `set -o pipefail` grep can exit on an
# early match, deliver SIGPIPE to the producer, and turn a present entry into a
# false absence. Every corpus floors its own selection through
# scripts/lib/corpus-nonempty.sh before anything is compared — an enumeration
# that matched nothing reports no failures, which agrees with any expected list.
#
# WHEN OBSOLETE: per corpus, when its expected-failures list is empty and stays
# empty. Then that corpus's gate runs the underlying command directly and its
# entry here is deleted; the driver goes when the last entry does.
#
# Usage:
#   scripts/corpus-ratchet.sh <corpus> [options]
#   scripts/corpus-ratchet.sh --help
#
# Corpora: hew-suite | stdlib | hew-corpus | doc-fences
#
# Options (accepted only for the corpora that define them):
#   --expected-failures <path>  Override the corpus's expected-failures file.
#   --hew-bin <path>            Override the hew binary.
#                               [hew-corpus, doc-fences]
#   --emit-o0-outcomes <path>   Write the sorted per-test outcome lines for
#                               scripts/o2-differential.sh's --o0-outcomes to
#                               reuse instead of re-running the identical O0
#                               pass. [hew-suite]
#   --junit-output <path>       Where to write the parsed JUnit report.
#                               [hew-suite]
#   --outdir <dir>              Scratch directory for extracted fences.
#                               [doc-fences]
#
# The hew binary otherwise comes from $HEW_BIN, then from Cargo's resolved
# debug output directory.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/line-set.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/line-set.sh"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/corpus-nonempty.sh"
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/cargo-output-dir.sh"
# shellcheck source=scripts/lib/diagnostic-code-set.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/diagnostic-code-set.sh"

usage() {
    cat <<'EOF'
Usage: scripts/corpus-ratchet.sh <corpus> [options]

Run a corpus and assert it has no failures outside the tracked
expected-failures list. Exits 1 for a regression, infrastructure drift, or
when a scheduled strict run finds a recovered entry still in the ledger.
Ordinary PR runs report recovered entries and exit 0.

Corpora:
  hew-suite    `hew test tests/hew/`               (make test-hew-ratchet)
  stdlib       `hew check` over std/**.hew         (make test-stdlib-ratchet)
  hew-corpus   `hew check` over the tracked corpus (make hew-check-all)
  doc-fences   `hew check` over ```hew doc fences  (make test-doc-examples)

Options:
  --expected-failures <path>  Override the corpus's expected-failures file.
  --hew-bin <path>            Override the hew binary.       [hew-corpus, doc-fences]
  --emit-o0-outcomes <path>   Write the O0 outcome set out.  [hew-suite]
  --junit-output <path>       Where to write the report.     [hew-suite]
  --outdir <dir>              Fence scratch directory.       [doc-fences]
  --help                      Show this message.
EOF
}

# ── Corpus selection ──────────────────────────────────────────────────────────

CORPUS=""
EXPECTED_FAILURES_FILE=""
HEW_BIN_ARG=""
EMIT_O0_OUTCOMES_FILE=""
JUNIT_OUTPUT_ARG=""
OUTDIR_ARG=""

if [[ $# -eq 0 ]]; then
    echo "error: a corpus is required" >&2
    usage >&2
    exit 1
fi

case "$1" in
--help | -h)
    usage
    exit 0
    ;;
hew-suite | stdlib | hew-corpus | doc-fences)
    CORPUS="$1"
    shift
    ;;
*)
    echo "error: unknown corpus: $1" >&2
    usage >&2
    exit 1
    ;;
esac

# An option that belongs to a different corpus is rejected rather than ignored:
# a silently dropped --expected-failures would ratchet against the production
# list while its caller believed it was using a fixture.
reject_unless_corpus() {
    local option="$1"
    shift
    local corpus
    for corpus in "$@"; do
        [[ "$CORPUS" == "$corpus" ]] && return 0
    done
    echo "error: $option is not a $CORPUS option" >&2
    exit 1
}

require_value() {
    [[ $# -gt 1 ]] || {
        echo "error: $1 requires a path" >&2
        exit 1
    }
}

while [[ $# -gt 0 ]]; do
    case "$1" in
    --expected-failures)
        require_value "$@"
        EXPECTED_FAILURES_FILE="$2"
        shift 2
        ;;
    --hew-bin)
        reject_unless_corpus --hew-bin hew-corpus doc-fences
        require_value "$@"
        HEW_BIN_ARG="$2"
        shift 2
        ;;
    --emit-o0-outcomes)
        reject_unless_corpus --emit-o0-outcomes hew-suite
        require_value "$@"
        EMIT_O0_OUTCOMES_FILE="$2"
        shift 2
        ;;
    --junit-output)
        reject_unless_corpus --junit-output hew-suite
        require_value "$@"
        JUNIT_OUTPUT_ARG="$2"
        shift 2
        ;;
    --outdir)
        reject_unless_corpus --outdir doc-fences
        require_value "$@"
        OUTDIR_ARG="$2"
        shift 2
        ;;
    --help | -h)
        usage
        exit 0
        ;;
    *)
        echo "error: unknown argument: $1" >&2
        usage >&2
        exit 1
        ;;
    esac
done

# hew-corpus historically defaulted to a literal target/debug/hew rather than
# Cargo's resolved output directory. Both entry points are driven by the
# Makefile with an explicit HEW_BIN, so the resolved default is the correct one
# for every corpus and the literal is not preserved.
HEW_BIN="${HEW_BIN_ARG:-${HEW_BIN:-$(cargo_debug_dir "$REPO_ROOT")/hew}}"
RATCHET_STRICT_RECOVERIES="${RATCHET_STRICT_RECOVERIES:-0}"
case "$RATCHET_STRICT_RECOVERIES" in
0 | 1) ;;
*)
    echo "error: RATCHET_STRICT_RECOVERIES must be 0 or 1" >&2
    exit 1
    ;;
esac

require_hew_bin() {
    if [[ ! -f "$HEW_BIN" ]]; then
        echo "error: hew binary not found at $HEW_BIN" >&2
        echo "       Run: cargo build -p hew-cli" >&2
        exit 1
    fi
}

# ── Shared ratchet core ───────────────────────────────────────────────────────

# EXPECTED_STR / ACTUAL_STR are newline-delimited sets of entry names. The
# corpus hooks fill ACTUAL_STR; read_expected_failures fills EXPECTED_STR.
# Hew-suite expected-failure rows pin a semantic compiled-test failure kind.
# Other corpora may additionally pin one stable `E_*` diagnostic code. The path
# remains the ratchet identity; the second field is corpus-specific metadata.
EXPECTED_STR=""
ACTUAL_STR=""
# The complete selected corpus, populated by each runner before outcome
# comparison. An expected row absent here is an inventory/configuration error,
# not evidence that a test recovered.
RATCHET_INVENTORY_STR=""
# Only an observed ordinary PASS may discharge an expected-failure row. An
# ignored test or skipped doc fence remains a hard ledger/inventory defect.
RATCHET_PASSED_STR=""
EXPECTED_DIAGNOSTICS_STR=""
EXPECTED_DIAGNOSTIC_CODE=""
EXPECTED_FAILURE_KINDS_STR=""
EXPECTED_FAILURE_KIND=""

# Parse corpus-specific expected-failure metadata. Hew-suite requires
# `<name> <compile|runtime|timeout|launch>`; the check-only corpora retain
# `<name> [E_DIAGNOSTIC_CODE]`. Reject every unsupported trailing field.
read_expected_failures() {
    local line entry name metadata trailing
    EXPECTED_STR=""
    EXPECTED_DIAGNOSTICS_STR=""
    EXPECTED_FAILURE_KINDS_STR=""
    while IFS= read -r line; do
        entry="${line%%#*}"
        entry="${entry#"${entry%%[![:space:]]*}"}" # ltrim
        entry="${entry%"${entry##*[![:space:]]}"}" # rtrim
        [[ -z "$entry" ]] && continue
        name=""
        metadata=""
        trailing=""
        read -r name metadata trailing <<<"$entry"
        [[ -z "$name" ]] && continue

        if line_set_contains "$EXPECTED_STR" "$name"; then
            echo "error: expected-failures file contains duplicate identity: $name" >&2
            exit 1
        fi

        if [[ "$CORPUS" == "hew-suite" ]]; then
            if [[ -z "$metadata" || -n "$trailing" ]]; then
                echo "error: Hew-suite expected-failure entry must be '<identity> <failure-kind>': $entry" >&2
                exit 1
            fi
            case "$metadata" in
            compile | runtime | timeout | launch) ;;
            *)
                echo "error: unsupported Hew-suite failure kind '$metadata' for $name" >&2
                exit 1
                ;;
            esac
            EXPECTED_STR="${EXPECTED_STR}${name}"$'\n'
            EXPECTED_FAILURE_KINDS_STR="${EXPECTED_FAILURE_KINDS_STR}${name}"$'\t'"${metadata}"$'\n'
            continue
        fi

        if [[ -n "$trailing" ]]; then
            echo "error: expected-failure entry has unsupported trailing fields: $entry" >&2
            echo "       Format: <name> [E_DIAGNOSTIC_CODE] # comment" >&2
            exit 1
        fi
        if [[ -n "$metadata" && ! "$metadata" =~ ^E_[A-Z0-9_]+$ ]]; then
            echo "error: invalid expected diagnostic code '$metadata' for $name" >&2
            echo "       Stable diagnostic codes must match E_[A-Z0-9_]+" >&2
            exit 1
        fi
        EXPECTED_STR="${EXPECTED_STR}${name}"$'\n'
        if [[ -n "$metadata" ]]; then
            EXPECTED_DIAGNOSTICS_STR="${EXPECTED_DIAGNOSTICS_STR}${name}"$'\t'"${metadata}"$'\n'
        fi
    done <"$EXPECTED_FAILURES_FILE"
}

# Set EXPECTED_FAILURE_KIND for one Hew-suite identity.
find_expected_failure_kind() {
    local wanted="$1" name kind
    EXPECTED_FAILURE_KIND=""
    while IFS=$'\t' read -r name kind; do
        [[ -z "$name" ]] && continue
        if [[ "$name" == "$wanted" ]]; then
            EXPECTED_FAILURE_KIND="$kind"
            return 0
        fi
    done <<<"$EXPECTED_FAILURE_KINDS_STR"
    return 1
}

# Set EXPECTED_DIAGNOSTIC_CODE and return success when an expected-failure row
# pins a code for the requested identity. A line-based lookup keeps this script
# compatible with the Bash 3 shipped by macOS.
find_expected_diagnostic() {
    local wanted="$1" name code
    EXPECTED_DIAGNOSTIC_CODE=""
    while IFS=$'\t' read -r name code; do
        [[ -z "$name" ]] && continue
        if [[ "$name" == "$wanted" ]]; then
            EXPECTED_DIAGNOSTIC_CODE="$code"
            return 0
        fi
    done <<<"$EXPECTED_DIAGNOSTICS_STR"
    return 1
}

require_expected_failures_file() {
    if [[ ! -f "$EXPECTED_FAILURES_FILE" ]]; then
        echo "error: expected-failures file not found: $EXPECTED_FAILURES_FILE" >&2
        exit 1
    fi
}

# Entries present in the first set and absent from the second.
set_difference() {
    local present="$1"
    local against="$2"
    local entry
    local out=""
    while IFS= read -r entry; do
        [[ -z "$entry" ]] && continue
        if ! line_set_contains "$against" "$entry"; then
            out="${out}${entry}"$'\n'
        fi
    done <<<"$present"
    printf '%s' "$out"
}

set_intersection() {
    local present="$1"
    local against="$2"
    local entry
    local out=""
    while IFS= read -r entry; do
        [[ -z "$entry" ]] && continue
        if line_set_contains "$against" "$entry"; then
            out="${out}${entry}"$'\n'
        fi
    done <<<"$present"
    printf '%s' "$out"
}

count_set() {
    local set="$1"
    [[ -z "$set" ]] && {
        printf '0\n'
        return
    }
    line_set_count "$set"
}

# Verdict strings, set by the corpus before ratchet_verdict runs.
RATCHET_INDENT=""
RATCHET_ALL_PASS_TEXT=""
RATCHET_ALL_PASS_LEADING_BLANK=0
RATCHET_LIST_TRACKED=0
RATCHET_FAIL_LEADING_BLANK=0
RATCHET_FAIL_PREFIX="RATCHET FAIL"
RATCHET_VERDICT_LABEL="Ratchet"
RATCHET_UNEXPECTED_HELP=""
RATCHET_NOWPASS_HELP=""
RATCHET_DIAGNOSTIC_FN=""
RATCHET_EXTRA_FAIL_FN=""
RATCHET_TAIL_FN=""
# A listed `hew check` refusal is valid only through the compiler's ordinary
# structured-diagnostic exit. Set membership must never bless a panic, signal,
# timeout, or harness failure under the same fixture identity.
RATCHET_REFUSAL_DRIFT_STR=""
# Filled by RATCHET_EXTRA_FAIL_FN with the count of its own failure class, so a
# corpus with a third mutation to detect (doc-fences' stale checksums) reports
# it without the shared core knowing what it is.
RATCHET_EXTRA_FAIL_COUNT=0

record_expected_refusal_status() {
    local identity="$1" status="$2"
    if ((status != 1)) && line_set_contains "$EXPECTED_STR" "$identity"; then
        RATCHET_REFUSAL_DRIFT_STR="${RATCHET_REFUSAL_DRIFT_STR}${identity}"$'\t'"${status}"$'\n'
    fi
}

# Compare EXPECTED_STR against ACTUAL_STR and exit with the gate's verdict.
ratchet_verdict() {
    local unexpected_failures recovered missing_expected nonpassing_expected
    local count_actual count_unexpected_fail count_recovered count_missing_expected count_nonpassing_expected count_refusal_drift
    local entry status sorted_actual

    unexpected_failures="$(set_difference "$ACTUAL_STR" "$EXPECTED_STR")"
    recovered="$(set_intersection "$EXPECTED_STR" "$RATCHET_PASSED_STR")"
    missing_expected="$(set_difference "$EXPECTED_STR" "$RATCHET_INVENTORY_STR")"
    nonpassing_expected="$(set_difference "$(set_difference "$EXPECTED_STR" "$ACTUAL_STR")" "$RATCHET_PASSED_STR")"
    nonpassing_expected="$(set_difference "$nonpassing_expected" "$missing_expected")"
    count_actual="$(count_set "$ACTUAL_STR")"
    count_unexpected_fail="$(count_set "$unexpected_failures")"
    count_recovered="$(count_set "$recovered")"
    count_missing_expected="$(count_set "$missing_expected")"
    count_nonpassing_expected="$(count_set "$nonpassing_expected")"
    count_refusal_drift="$(count_set "$RATCHET_REFUSAL_DRIFT_STR")"

    RATCHET_EXTRA_FAIL_COUNT=0
    if [[ -n "$RATCHET_EXTRA_FAIL_FN" ]]; then
        "$RATCHET_EXTRA_FAIL_FN" detect
    fi

    if ((count_unexpected_fail == 0 && count_missing_expected == 0 && \
        count_nonpassing_expected == 0 && \
        count_refusal_drift == 0 && RATCHET_EXTRA_FAIL_COUNT == 0)); then
        if ((count_recovered > 0)); then
            echo "${RATCHET_VERDICT_LABEL}: $count_recovered tracked failure(s) now PASS — ledger cleanup required:"
            while IFS= read -r entry; do
                [[ -z "$entry" ]] && continue
                echo "  NOW-PASSES: $entry"
            done <<<"$recovered"
            echo ""
            printf '%s\n' "$RATCHET_NOWPASS_HELP"
            echo ""
            if ((RATCHET_STRICT_RECOVERIES == 1)); then
                echo "==> ${RATCHET_VERDICT_LABEL}: FAILED (strict recovery accounting)"
                exit 1
            fi
            echo "==> ${RATCHET_VERDICT_LABEL}: PASSED (recoveries reported)"
            exit 0
        fi
        if ((count_actual == 0)); then
            ((RATCHET_ALL_PASS_LEADING_BLANK == 1)) && echo ""
            echo "${RATCHET_INDENT}${RATCHET_ALL_PASS_TEXT}"
        else
            echo "${RATCHET_INDENT}Expected failure set matches. Tracked failures: $count_actual"
            if ((RATCHET_LIST_TRACKED == 1)); then
                sorted_actual="$(printf '%s' "$ACTUAL_STR" | sort)"
                while IFS= read -r entry; do
                    [[ -z "$entry" ]] && continue
                    echo "  - $entry"
                done <<<"$sorted_actual"
            fi
        fi
        echo ""
        echo "==> ${RATCHET_VERDICT_LABEL}: PASSED"
        exit 0
    fi

    ((RATCHET_FAIL_LEADING_BLANK == 1)) && echo ""

    if ((count_unexpected_fail > 0)); then
        echo "$RATCHET_FAIL_PREFIX: $count_unexpected_fail UNEXPECTED failure(s) — not in expected list:"
        while IFS= read -r entry; do
            [[ -z "$entry" ]] && continue
            if [[ -n "$RATCHET_DIAGNOSTIC_FN" ]]; then
                "$RATCHET_DIAGNOSTIC_FN" "$entry"
            else
                echo "  UNEXPECTED: $entry"
            fi
        done <<<"$unexpected_failures"
        echo ""
        printf '%s\n' "$RATCHET_UNEXPECTED_HELP"
        echo ""
    fi

    if ((count_missing_expected > 0)); then
        echo "$RATCHET_FAIL_PREFIX: $count_missing_expected expected entry/entries absent from the selected corpus:"
        while IFS= read -r entry; do
            [[ -z "$entry" ]] && continue
            echo "  MISSING-EXPECTED: $entry"
        done <<<"$missing_expected"
        echo ""
        echo "  This is an inventory or fixture-removal error, not a recovered test."
        echo ""
    fi

    if ((count_recovered > 0)); then
        echo "${RATCHET_VERDICT_LABEL}: $count_recovered tracked failure(s) now PASS — ledger cleanup required:"
        while IFS= read -r entry; do
            [[ -z "$entry" ]] && continue
            echo "  NOW-PASSES: $entry"
        done <<<"$recovered"
        echo ""
        printf '%s\n' "$RATCHET_NOWPASS_HELP"
        echo ""
    fi

    if ((count_nonpassing_expected > 0)); then
        echo "$RATCHET_FAIL_PREFIX: $count_nonpassing_expected expected entry/entries did not produce PASS or the pinned failure:"
        while IFS= read -r entry; do
            [[ -z "$entry" ]] && continue
            echo "  NONPASS-EXPECTED: $entry"
        done <<<"$nonpassing_expected"
        echo ""
        echo "  Skipped or otherwise nonterminal entries cannot be treated as recoveries."
        echo ""
    fi

    if ((count_refusal_drift > 0)); then
        echo "$RATCHET_FAIL_PREFIX: $count_refusal_drift expected refusal(s) changed process outcome:"
        while IFS=$'\t' read -r entry status; do
            [[ -z "$entry" ]] && continue
            echo "  OUTCOME DRIFT: $entry (exit $status, expected structured diagnostic exit 1)"
        done <<<"$RATCHET_REFUSAL_DRIFT_STR"
        echo ""
        echo "  A listed compiler refusal cannot cover a panic, signal, timeout, or harness failure."
        echo ""
    fi

    if [[ -n "$RATCHET_EXTRA_FAIL_FN" ]] && ((RATCHET_EXTRA_FAIL_COUNT > 0)); then
        "$RATCHET_EXTRA_FAIL_FN" report
    fi

    if [[ -n "$RATCHET_TAIL_FN" ]]; then
        "$RATCHET_TAIL_FN"
    fi

    echo "==> ${RATCHET_VERDICT_LABEL}: FAILED"
    exit 1
}

# ── Corpus: hew-suite ─────────────────────────────────────────────────────────
#
# The full directory runs as one `hew test` transaction so the runner can use
# its cross-file worker pool. Verdicts are not cached: a fixture can import
# arbitrary Hew sources and links the current runtime archive, so a per-file
# key that does not reproduce compiler dependency resolution can reuse stale
# outcomes. The structured report is the artifact and handoff boundary CI needs.

STDERR_FILE=""
HEW_SUITE_FAILURE_KIND_DRIFT=""

run_hew_suite() {
    local tests_dir junit_output hew_junit_py fresh_report invalid_report rc
    local parsed summary_line report_total report_failures report_skipped
    local status identity failure_kind parsed_failed

    tests_dir="${HEW_TESTS_DIR:-$REPO_ROOT/tests/hew}"
    junit_output="${JUNIT_OUTPUT_ARG:-$REPO_ROOT/target/hew-test-reports/hew-suite-ratchet.xml}"
    hew_junit_py="$REPO_ROOT/scripts/lib/hew_junit.py"
    HEW_SUITE_JUNIT_OUTPUT="$junit_output"

    require_hew_bin
    if [[ ! -d "$tests_dir" ]]; then
        echo "error: tests/hew/ directory not found" >&2
        exit 1
    fi
    require_expected_failures_file
    read_expected_failures

    mkdir -p "$(dirname "$junit_output")"
    STDERR_FILE="$(mktemp "${TMPDIR:-/tmp}/hew-suite-ratchet-stderr.XXXXXX")"
    fresh_report="${junit_output}.new.$$"
    trap 'rm -f "$STDERR_FILE" "${fresh_report:-}"' EXIT
    rc=0
    env HEW_OPT_LEVEL=0 "$HEW_BIN" test "$tests_dir" --format junit --allow-empty \
        >"$fresh_report" 2>"$STDERR_FILE" || rc=$?

    # Parsing owns both XML validity and the runner-status contract. In
    # particular, status 1 with failed testcase elements is a complete run,
    # not an invalid report; the ratchet below decides whether those failures
    # are expected. Publish only a coherent report, atomically.
    parsed=""
    if ! parsed="$("${PYTHON:-python3}" "$hew_junit_py" --runner-exit "$rc" "$fresh_report")"; then
        echo "error: hew test did not produce a coherent JUnit result for $tests_dir" >&2
        if [[ -s "$fresh_report" ]]; then
            invalid_report="${junit_output}.invalid.$$"
            mv "$fresh_report" "$invalid_report"
            echo "error: preserved runner output at $invalid_report" >&2
        fi
        echo "==> stderr from the run:" >&2
        cat "$STDERR_FILE" >&2
        exit 1
    fi
    mv "$fresh_report" "$junit_output"

    summary_line="$(printf '%s\n' "$parsed" | grep '^__SUMMARY__' || true)"
    if [[ -z "$summary_line" ]]; then
        echo "error: hew_junit.py produced no __SUMMARY__ line; refusing to ratchet" >&2
        exit 1
    fi
    IFS=$'\t' read -r _ report_total report_failures report_skipped <<<"$summary_line"
    : "$report_skipped"

    # Emit the full per-test outcome set for the O2-differential gate to reuse
    # as its O0 baseline, reconstructed in the
    # "test <path>::<name> ... ok|ignored|FAILED[<tab><failure-kind>]" form
    # consumed by the differential gate. Written before the ratchet verdict is
    # known — the outcome set is valid either way.
    if [[ -n "$EMIT_O0_OUTCOMES_FILE" ]]; then
        printf '%s\n' "$parsed" |
            awk -F'\t' '$1 != "__SUMMARY__" {
                line = "test " $2 " ... " $1
                if ($3 != "") line = line "\t" $3
                print line
            }' |
            sort >"$EMIT_O0_OUTCOMES_FILE"
    fi

    # Floor the size of the run itself. A run that executed no tests reports an
    # empty failing set, which agrees with an empty expected list.
    if ! corpus_nonempty_assert "hew-suite-tests" "$report_total"; then
        cat "$STDERR_FILE" >&2
        exit 1
    fi

    while IFS=$'\t' read -r status identity failure_kind; do
        [[ -n "$identity" ]] || continue
        RATCHET_INVENTORY_STR="${RATCHET_INVENTORY_STR}${identity}"$'\n'
        [[ "$status" == "ok" ]] && RATCHET_PASSED_STR="${RATCHET_PASSED_STR}${identity}"$'\n'
        [[ "$status" == "FAILED" ]] || continue
        ACTUAL_STR="${ACTUAL_STR}${identity}"$'\n'
        if line_set_contains "$EXPECTED_STR" "$identity"; then
            if ! find_expected_failure_kind "$identity"; then
                echo "error: expected Hew-suite failure has no semantic kind: $identity" >&2
                exit 1
            fi
            if [[ "$failure_kind" != "$EXPECTED_FAILURE_KIND" ]]; then
                HEW_SUITE_FAILURE_KIND_DRIFT="${HEW_SUITE_FAILURE_KIND_DRIFT}${identity}"$'\t'"${EXPECTED_FAILURE_KIND}"$'\t'"${failure_kind}"$'\n'
            fi
        fi
    done <<<"$(printf '%s\n' "$parsed" | grep -v '^__SUMMARY__')"

    parsed_failed="$(count_set "$ACTUAL_STR")"
    if [[ "$parsed_failed" -ne "$report_failures" ]]; then
        echo "error: parsed $parsed_failed FAILED test(s) but report summary reports $report_failures failed; refusing to ratchet" >&2
        exit 1
    fi

    echo "==> Hew suite ratchet"
    echo "Expected failures: $(count_set "$EXPECTED_STR")"
    echo "Actual failures:   $(count_set "$ACTUAL_STR")"
    echo ""
}

# Reached through RATCHET_EXTRA_FAIL_FN; shellcheck cannot see an indirect call.
# shellcheck disable=SC2317,SC2329
hew_suite_extra_failures() {
    local identity expected_kind actual_kind

    case "$1" in
    detect)
        RATCHET_EXTRA_FAIL_COUNT="$(count_set "$HEW_SUITE_FAILURE_KIND_DRIFT")"
        ;;
    report)
        echo "$RATCHET_FAIL_PREFIX: $RATCHET_EXTRA_FAIL_COUNT expected failure(s) changed semantic kind:"
        while IFS=$'\t' read -r identity expected_kind actual_kind; do
            [[ -z "$identity" ]] && continue
            echo "  FAILURE KIND DRIFT: $identity (expected=$expected_kind actual=$actual_kind)"
        done <<<"$HEW_SUITE_FAILURE_KIND_DRIFT"
        echo ""
        echo "  A ledgered failure cannot cover a different compile, runtime, timeout, or launch failure."
        echo ""
        ;;
    esac
}

# Reached through RATCHET_TAIL_FN; shellcheck cannot see an indirect call.
# shellcheck disable=SC2317,SC2329
hew_suite_tail() {
    # Print the run's stderr (build/FFI errors, warnings) and point at the full
    # JUnit report — release and FreeBSD CI upload this file into the checks
    # UI, so failure detail is one click away there too.
    echo "==> stderr from the run:"
    cat "$STDERR_FILE"
    echo ""
    echo "==> Full JUnit report: $HEW_SUITE_JUNIT_OUTPUT"
    echo ""
}

# ── Corpus: stdlib ────────────────────────────────────────────────────────────

run_stdlib() {
    local stdlib_dir total relpath f check_status

    stdlib_dir="$REPO_ROOT/std"
    require_hew_bin
    if [[ ! -d "$stdlib_dir" ]]; then
        echo "error: std/ directory not found" >&2
        exit 1
    fi
    require_expected_failures_file
    read_expected_failures

    total=0
    while IFS= read -r -d $'\0' f; do
        total=$((total + 1))
        relpath="${f#"$REPO_ROOT"/}"
        RATCHET_INVENTORY_STR="${RATCHET_INVENTORY_STR}${relpath}"$'\n'
        check_status=0
        "$HEW_BIN" check "$f" >/dev/null 2>&1 || check_status=$?
        if ((check_status != 0)); then
            ACTUAL_STR="${ACTUAL_STR}${relpath}"$'\n'
            record_expected_refusal_status "$relpath" "$check_status"
        else
            RATCHET_PASSED_STR="${RATCHET_PASSED_STR}${relpath}"$'\n'
        fi
    done < <(find "$stdlib_dir" -name '*.hew' -not -path '*/target/*' -print0 | sort -z)

    # A find that matched nothing type-checks nothing and reports no failures,
    # which agrees with any expected-failures list.
    corpus_nonempty_assert "stdlib-ratchet-files" "$total" || exit 1

    echo "==> Stdlib type-check ratchet"
    echo "Files checked:     $total"
    echo "Expected failures: $(count_set "$EXPECTED_STR")"
    echo "Actual failures:   $(count_set "$ACTUAL_STR")"
    echo ""
}

# Reached through RATCHET_DIAGNOSTIC_FN; shellcheck cannot see an indirect call.
# shellcheck disable=SC2317,SC2329
stdlib_diagnostic() {
    echo "  UNEXPECTED: $1"
    # The excerpt is informational. Under `set -e -o pipefail` a non-zero status
    # anywhere in this pipeline would abort the whole script and truncate the
    # report after the FIRST entry — which is how a two-file regression reached
    # CI showing only one file. `head` closing the pipe early is a normal
    # outcome here, so the status is deliberately dropped.
    "$HEW_BIN" check "$REPO_ROOT/$1" 2>&1 | head -3 | sed 's/^/    /' || true
}

# ── Corpus: hew-corpus ────────────────────────────────────────────────────────
#
# The repo-wide sweep is the migrate-forward safety net: it catches the class
# of bug where a breaking API change lands in the compiler while fixture files
# under crates, tests/ or examples/ keep the old call site, each a latent
# undefined-symbol or type error invisible to every narrower gate.

# is_reject_fixture — true when the path is an intentional-reject fixture.
#
# Two conventions cover all known reject fixtures in the repo:
#   1. Path contains /reject/ — files inside reject/ subdirectories, used by
#      test-vertical-slice, test-pkg-import and fuzz-oracle. Multi-file reject
#      cases include helper .hew files in the same directory that pass
#      `hew check` individually; they are still part of the reject fixture.
#   2. The basename contains "reject" — single-file reject tests use several
#      naming conventions (*_reject.hew, *_rejected.hew, reject_*.hew,
#      lsp_reject_*.hew, *_reject_reversed.hew). Matching on the basename
#      substring captures all variants without false positives.
is_separately_gated_or_reject_fixture() {
    local path="$1"
    local base
    base="$(basename "$path")"
    case "$path" in
    *"/reject/"*)
        return 0
        ;;
    tests/core-matrix/cells/*)
        # The core matrix is a deliberate enumeration of primitive x
        # operation, so a large minority of its cells are combinations the
        # compiler does not yet support. Their outcome is recorded per cell
        # in tests/core-matrix/matrix.tsv and gated by make test-core-matrix,
        # which fails on drift in either direction. Sweeping them here would
        # duplicate that verdict as a second, weaker expected-failures list.
        return 0
        ;;
    tests/hew/*)
        # Files that contribute actual test cases are compiled and run through
        # `make test-hew-ratchet`, with exact per-test expected failures. Keep
        # support modules and standalone programs in this sweep: discovery may
        # parse them, but it does not independently type-check a file with no
        # `#[test]` case.
        if grep -qF '#[test]' "$REPO_ROOT/$path"; then
            return 0
        fi
        ;;
    esac
    case "$base" in
    *"reject"*)
        return 0
        ;;
    esac
    return 1
}

HEW_CORPUS_DIAGNOSTIC_DRIFT=""
HEW_CORPUS_DIAGNOSTIC_DRIFT_COUNT=0
HEW_CORPUS_TMPDIR=""

run_hew_corpus() {
    local swept=() excluded=0 total f
    local check_log status expected_code actual_codes check_index=0

    require_hew_bin
    require_expected_failures_file
    read_expected_failures
    HEW_CORPUS_DIAGNOSTIC_DRIFT=""
    HEW_CORPUS_TMPDIR="$(mktemp -d)"
    trap '[[ -z "${HEW_CORPUS_TMPDIR:-}" ]] || rm -rf "$HEW_CORPUS_TMPDIR"' EXIT

    # Enumerate the corpus first so the floor can reject an empty or shrunken
    # sweep before spending minutes type-checking it.
    while IFS= read -r f; do
        if is_separately_gated_or_reject_fixture "$f"; then
            excluded=$((excluded + 1))
            continue
        fi
        swept+=("$f")
    done < <(cd "$REPO_ROOT" && git ls-files '*.hew')
    total=${#swept[@]}

    # `git ls-files` run from the wrong tree, or a pattern that stops matching,
    # yields an empty sweep: no files checked, no failures found, expected set
    # trivially satisfied.
    corpus_nonempty_assert "hew-corpus-check-files" "$total" || exit 1

    for f in "${swept[@]}"; do
        RATCHET_INVENTORY_STR="${RATCHET_INVENTORY_STR}${f}"$'\n'
        check_index=$((check_index + 1))
        check_log="$HEW_CORPUS_TMPDIR/check-$check_index.log"
        status=0
        "$HEW_BIN" check "$REPO_ROOT/$f" >"$check_log" 2>&1 || status=$?
        if ((status != 0)); then
            ACTUAL_STR="${ACTUAL_STR}${f}"$'\n'
            record_expected_refusal_status "$f" "$status"
            # Set membership alone is not enough for a known compiler refusal:
            # every tracked failure must remain the normal diagnostic exit (1),
            # never a panic, signal, or harness failure. Rows that pin a stable
            # code additionally require that exact unique code set, so a second
            # diagnostic class cannot hitchhike on the expected one.
            if line_set_contains "$EXPECTED_STR" "$f"; then
                expected_code="<none pinned>"
                if find_expected_diagnostic "$f"; then
                    expected_code="$EXPECTED_DIAGNOSTIC_CODE"
                fi

                if ((status == 1)) &&
                    { [[ "$expected_code" != "<none pinned>" ]] &&
                        ! diagnostic_log_has_exact_code "$check_log" "$expected_code"; }; then
                    actual_codes="$(diagnostic_code_set "$check_log" | paste -sd, -)"
                    [[ -n "$actual_codes" ]] || actual_codes="<none>"
                    HEW_CORPUS_DIAGNOSTIC_DRIFT="${HEW_CORPUS_DIAGNOSTIC_DRIFT}${f}"$'\t'"${status}"$'\t'"${expected_code}"$'\t'"${actual_codes}"$'\n'
                fi
            fi
        else
            RATCHET_PASSED_STR="${RATCHET_PASSED_STR}${f}"$'\n'
        fi
    done

    echo "==> Hew corpus compile sweep"
    echo "Files checked:          $total"
    echo "Separately gated/reject skipped: $excluded"
    echo "Expected failures:      $(count_set "$EXPECTED_STR")"
    echo "Actual failures:        $(count_set "$ACTUAL_STR")"
    echo ""
}

# Reached through RATCHET_EXTRA_FAIL_FN; shellcheck cannot see an indirect call.
# shellcheck disable=SC2317,SC2329
hew_corpus_extra_failures() {
    local path status expected actual
    case "$1" in
    detect)
        HEW_CORPUS_DIAGNOSTIC_DRIFT_COUNT="$(count_set "$HEW_CORPUS_DIAGNOSTIC_DRIFT")"
        RATCHET_EXTRA_FAIL_COUNT="$HEW_CORPUS_DIAGNOSTIC_DRIFT_COUNT"
        ;;
    report)
        ((HEW_CORPUS_DIAGNOSTIC_DRIFT_COUNT > 0)) || return 0
        echo "CORPUS FAIL: $HEW_CORPUS_DIAGNOSTIC_DRIFT_COUNT expected failure(s) changed diagnostic class:"
        while IFS=$'\t' read -r path status expected actual; do
            [[ -z "$path" ]] && continue
            echo "  DIAGNOSTIC DRIFT: $path (exit $status, expected $expected, observed $actual)"
        done <<<"$HEW_CORPUS_DIAGNOSTIC_DRIFT"
        echo ""
        echo "  Fix the regression, or update the shared expected-failures row only when the tracked issue's failure class intentionally changes."
        echo ""
        ;;
    esac
}

# Reached through RATCHET_DIAGNOSTIC_FN; shellcheck cannot see an indirect call.
# shellcheck disable=SC2317,SC2329
hew_corpus_diagnostic() {
    echo "  UNEXPECTED: $1"
    # This command is known to fail — it is the failure being reported. `head`
    # under `pipefail` would instead turn a long diagnostic into SIGPIPE (141),
    # preventing the deliberate gate failure and hiding the rest of the compact
    # report. `sed -n` reads the full stream while printing only three lines, so
    # the compiler exits normally.
    "$HEW_BIN" check "$REPO_ROOT/$1" 2>&1 | sed -n '1,3{s/^/    /;p;}' || true
}

# ── Corpus: doc-fences ────────────────────────────────────────────────────────
#
# Extracts every ```hew fence from the language guide, the spec, the
# docs/language/*.hew modules, and every std/**/*.hew doc comment into
# individual files, then type-checks each one. A fence whose preceding five
# lines carry a "Not yet implemented" callout or a `<!-- doctest: skip -->`
# comment is SKIPPED — spec-ahead-of-implementation is not drift when a plan
# exists. The default is fail-closed: a fence is checked unless marked.
#
# This corpus carries a third mutation class the others do not. Its
# expected-failures entries are `<fence-id> <cksum>` pairs, so a fence whose
# CONTENT changed under a listed id fails even when its verdict did not: the
# root-cause label attached to that id was written about different text and has
# to be re-verified rather than inherited by position.

DOC_FENCE_OUTDIR=""
DOC_FENCE_EXPECTED_CKSUM_STR=""
DOC_FENCE_STALE=""
DOC_FENCE_STALE_COUNT=0

DOC_FENCE_SOURCES=(
    "docs/hew-language-guide.md:guide"
    "docs/specs/HEW-SPEC-2026.md:spec"
)
DOC_FENCE_LANGUAGE_DIR="$REPO_ROOT/docs/language"
# Overridable so a test can point extraction at a small fixture tree instead
# of the real std/ (hundreds of fences) to exercise the tri-state extractor.
DOC_FENCE_STD_DIR="${DOC_FENCE_STD_DIR:-$REPO_ROOT/std}"
STD_FENCE_SOURCES=()

# Substrings that, in the five lines before a ```hew fence, mark it skippable.
DOC_FENCE_NYI_PATTERNS=("Not yet implemented" "doctest: skip" "doctest:skip")

DOC_FENCE_IDS=()
DOC_FENCE_SKIPPED=()

doc_fence_add_language_sources() {
    local language_doc language_name relative_path
    local found=0

    if [[ ! -d "$DOC_FENCE_LANGUAGE_DIR" ]]; then
        echo "error: language documentation directory not found: $DOC_FENCE_LANGUAGE_DIR" >&2
        exit 1
    fi

    while IFS= read -r language_doc; do
        found=1
        relative_path="${language_doc#"$REPO_ROOT"/}"
        language_name="${language_doc##*/}"
        language_name="${language_name%.hew}"
        DOC_FENCE_SOURCES+=("$relative_path:lang-$language_name")
    done < <(find "$DOC_FENCE_LANGUAGE_DIR" -maxdepth 1 -type f -name '*.hew' -print | LC_ALL=C sort)

    if ((found == 0)); then
        echo "error: no language documentation modules found in $DOC_FENCE_LANGUAGE_DIR" >&2
        exit 1
    fi
}

# std/**/*.hew doc comments use a different fencing convention than the guide
# and spec: item-level `///` comments (not only module-level `//!`), and a
# BARE ``` open (implicit hew — the only language std fences are written in)
# rather than an explicit ```hew tag. A handful of fences carry an explicit
# non-hew tag (e.g. ```text) that must stay excluded. doc_fence_extract's
# open condition (`stripped == '```hew'` only) and prefix strip (`//!` only)
# don't cover either case, so std gets its own enumerator and extractor
# instead of overloading the guide/spec one and risking a change in behaviour
# there.
# STD_FENCE_SOURCES pairs carry the ABSOLUTE file path (unlike
# DOC_FENCE_SOURCES's REPO_ROOT-relative paths) because DOC_FENCE_STD_DIR is
# overridable to a fixture tree outside REPO_ROOT for testing; run_doc_fences
# uses the std entries as-is instead of rejoining them under REPO_ROOT.
doc_fence_add_std_sources() {
    local std_file relative_path slug
    local found=0

    if [[ ! -d "$DOC_FENCE_STD_DIR" ]]; then
        echo "error: standard library directory not found: $DOC_FENCE_STD_DIR" >&2
        exit 1
    fi

    while IFS= read -r std_file; do
        found=1
        relative_path="${std_file#"$DOC_FENCE_STD_DIR"/}"
        slug="${relative_path%.hew}"
        slug="${slug//\//-}"
        STD_FENCE_SOURCES+=("$std_file:std-$slug")
    done < <(find "$DOC_FENCE_STD_DIR" -type f -name '*.hew' -not -path '*/target/*' -print | LC_ALL=C sort)

    if ((found == 0)); then
        echo "error: no standard library modules found in $DOC_FENCE_STD_DIR" >&2
        exit 1
    fi
}

doc_fence_extract() {
    local filepath="$1"
    local prefix="$2"
    local lines=()
    local line total fence_num i stripped fence_id skip j ctx_line marker
    local content fline fstripped outfile strip_doc_prefix=0

    [[ "$filepath" == *.hew ]] && strip_doc_prefix=1

    # mapfile requires bash 4; use a while loop for bash 3 (macOS ships 3.x).
    while IFS= read -r line; do
        if ((strip_doc_prefix)); then
            case "$line" in
            '//!'*)
                line="${line#//!}"
                line="${line# }"
                ;;
            *) line="" ;;
            esac
        fi
        lines+=("$line")
    done <"$filepath"

    total="${#lines[@]}"
    fence_num=0
    i=0

    while ((i < total)); do
        line="${lines[$i]}"
        stripped="${line%%$'\r'}"
        if [[ "$stripped" != '```hew' ]]; then
            ((i += 1))
            continue
        fi

        fence_num=$((fence_num + 1))
        printf -v fence_id "%s-%04d" "$prefix" "$fence_num"

        skip=0
        for ((j = i > 5 ? i - 5 : 0; j < i; j++)); do
            ctx_line="${lines[$j]}"
            for marker in "${DOC_FENCE_NYI_PATTERNS[@]}"; do
                if [[ "$ctx_line" == *"$marker"* ]]; then
                    skip=1
                    break 2
                fi
            done
        done

        ((i += 1))
        content=""
        while ((i < total)); do
            fline="${lines[$i]}"
            fstripped="${fline%%$'\r'}"
            if [[ "$fstripped" == '```' ]]; then
                ((i += 1))
                break
            fi
            content="${content}${fline}"$'\n'
            ((i += 1))
        done

        outfile="$DOC_FENCE_OUTDIR/${fence_id}.hew"
        printf '%s' "$content" >"$outfile"

        DOC_FENCE_IDS+=("$fence_id")
        DOC_FENCE_SKIPPED+=("$skip")
    done
}

# std's counterpart to doc_fence_extract: strips both `//!` and `///` doc
# prefixes, and tracks a third "inside a non-hew fence" state so a bare ```
# that closes e.g. a ```text block is never mistaken for the open of the next
# implicit-hew fence (which is itself opened by a bare ```, since std never
# tags its own fences ```hew). Content lines outside any comment prefix have
# already been blanked by the strip pass, so a fence body line is never
# confused with a fence delimiter.
doc_fence_extract_std() {
    local filepath="$1"
    local prefix="$2"
    local lines=()
    local line total fence_num i stripped fence_id skip j ctx_line marker
    local content fline fstripped outfile in_other=0

    while IFS= read -r line; do
        # doc comments on trait/impl members are indented (unlike the
        # module-level `//!` sources doc_fence_extract handles), so the
        # comment marker is matched after trimming leading whitespace rather
        # than only at column 0.
        local trimmed="${line#"${line%%[![:space:]]*}"}"
        case "$trimmed" in
        '//!'*)
            line="${trimmed#//!}"
            line="${line# }"
            ;;
        '///'*)
            line="${trimmed#///}"
            line="${line# }"
            ;;
        *) line="" ;;
        esac
        lines+=("$line")
    done <"$filepath"

    total="${#lines[@]}"
    fence_num=0
    i=0

    while ((i < total)); do
        line="${lines[$i]}"
        stripped="${line%%$'\r'}"

        if ((in_other)); then
            [[ "$stripped" == '```' ]] && in_other=0
            ((i += 1))
            continue
        fi

        if [[ "$stripped" != '```' && "$stripped" != '```hew' ]]; then
            [[ "$stripped" == '```'* ]] && in_other=1
            ((i += 1))
            continue
        fi

        fence_num=$((fence_num + 1))
        printf -v fence_id "%s-%04d" "$prefix" "$fence_num"

        skip=0
        for ((j = i > 5 ? i - 5 : 0; j < i; j++)); do
            ctx_line="${lines[$j]}"
            for marker in "${DOC_FENCE_NYI_PATTERNS[@]}"; do
                if [[ "$ctx_line" == *"$marker"* ]]; then
                    skip=1
                    break 2
                fi
            done
        done

        ((i += 1))
        content=""
        while ((i < total)); do
            fline="${lines[$i]}"
            fstripped="${fline%%$'\r'}"
            if [[ "$fstripped" == '```' ]]; then
                ((i += 1))
                break
            fi
            content="${content}${fline}"$'\n'
            ((i += 1))
        done

        outfile="$DOC_FENCE_OUTDIR/${fence_id}.hew"
        printf '%s' "$content" >"$outfile"

        DOC_FENCE_IDS+=("$fence_id")
        DOC_FENCE_SKIPPED+=("$skip")
    done
}

# doc-fences' expected-failures entries carry a checksum, so the shared
# name-only parser is not enough: the pair is validated here and the names are
# handed to EXPECTED_STR in the same form every other corpus uses.
doc_fence_read_expected() {
    local line fields name recorded_cksum extra_field
    EXPECTED_STR=""
    DOC_FENCE_EXPECTED_CKSUM_STR=""
    while IFS= read -r line; do
        fields="${line%%#*}"
        fields="${fields#"${fields%%[! ]*}"}"
        fields="${fields%"${fields##*[! ]}"}"
        [[ -z "$fields" ]] && continue
        name=""
        recorded_cksum=""
        extra_field=""
        read -r name recorded_cksum extra_field <<<"$fields"
        if [[ -z "$name" || -z "$recorded_cksum" || -n "$extra_field" ]]; then
            echo "error: expected-failures entry must be: <fence-id> <cksum>" >&2
            echo "       bad entry: $line" >&2
            exit 1
        fi
        if [[ ! "$recorded_cksum" =~ ^[0-9]+$ ]]; then
            echo "error: expected-failures checksum must be decimal cksum output" >&2
            echo "       bad entry: $line" >&2
            exit 1
        fi
        EXPECTED_STR="${EXPECTED_STR}${name}"$'\n'
        DOC_FENCE_EXPECTED_CKSUM_STR="${DOC_FENCE_EXPECTED_CKSUM_STR}${name} ${recorded_cksum}"$'\n'
    done <"$EXPECTED_FAILURES_FILE"
}

run_doc_fences() {
    local entry doc_path prefix full_path total_fences
    local pass=0 fail=0 skip=0 idx fence_id is_skip outfile check_rc

    DOC_FENCE_OUTDIR="${OUTDIR_ARG:-$REPO_ROOT/.tmp/doc-fences}"

    require_hew_bin
    require_expected_failures_file
    mkdir -p "$DOC_FENCE_OUTDIR"
    doc_fence_add_language_sources

    echo "==> Doc-test harness: extracting hew fences from docs/ and std/"
    for entry in "${DOC_FENCE_SOURCES[@]}"; do
        doc_path="${entry%%:*}"
        prefix="${entry##*:}"
        full_path="$REPO_ROOT/$doc_path"
        if [[ ! -f "$full_path" ]]; then
            echo "error: required doc-fence source not found: $full_path" >&2
            exit 1
        fi
        echo "  Scanning: $doc_path"
        doc_fence_extract "$full_path" "$prefix"
    done

    echo "  Scanning: std/ (standard library doc fences)"
    doc_fence_add_std_sources
    for entry in "${STD_FENCE_SOURCES[@]}"; do
        # Unlike DOC_FENCE_SOURCES, entries here already carry an absolute
        # path (see doc_fence_add_std_sources) — do not rejoin with REPO_ROOT.
        full_path="${entry%%:*}"
        doc_fence_extract_std "$full_path" "${entry##*:}"
    done

    total_fences="${#DOC_FENCE_IDS[@]}"
    echo "  Extracted: $total_fences fences total"

    # An extraction that produced nothing (a renamed doc, a changed fence
    # marker) would make both sets trivially agree once the expected list is
    # empty, so the extracted count is floored before anything is compared.
    corpus_nonempty_assert "doc-hew-fences" "$total_fences" || exit 1

    doc_fence_read_expected

    for ((idx = 0; idx < total_fences; idx++)); do
        fence_id="${DOC_FENCE_IDS[$idx]}"
        RATCHET_INVENTORY_STR="${RATCHET_INVENTORY_STR}${fence_id}"$'\n'
        is_skip="${DOC_FENCE_SKIPPED[$idx]}"
        outfile="$DOC_FENCE_OUTDIR/${fence_id}.hew"

        if [[ "$is_skip" == "1" ]]; then
            skip=$((skip + 1))
            continue
        fi

        check_rc=0
        "$HEW_BIN" check "$outfile" >/dev/null 2>&1 || check_rc=$?

        if [[ "$check_rc" == "0" ]]; then
            pass=$((pass + 1))
            RATCHET_PASSED_STR="${RATCHET_PASSED_STR}${fence_id}"$'\n'
        else
            fail=$((fail + 1))
            ACTUAL_STR="${ACTUAL_STR}${fence_id}"$'\n'
            record_expected_refusal_status "$fence_id" "$check_rc"
        fi
    done

    echo ""
    echo "==> Results: $pass passed, $fail failed, $skip skipped (NYI/aspirational)"
    echo "    Total fences: $total_fences"
    echo ""
    echo "==> Doc-test ratchet"
    echo "    Expected failures: $(count_set "$EXPECTED_STR")"
    echo "    Actual failures:   $(count_set "$ACTUAL_STR")"
}

# Reached through RATCHET_DIAGNOSTIC_FN; shellcheck cannot see an indirect call.
# shellcheck disable=SC2317,SC2329
doc_fence_diagnostic() {
    local outfile first_err
    outfile="$DOC_FENCE_OUTDIR/${1}.hew"
    # hew check is expected to fail here; capture without letting the non-zero
    # exit abort the script under set -e / pipefail.
    first_err="$("$HEW_BIN" check "$outfile" 2>&1 || true)"
    first_err="${first_err%%$'\n'*}"
    echo "  UNEXPECTED: $1  ($first_err)"
}

# Third mutation class: content changed under a listed id, so the root-cause
# label must be re-verified instead of trusted by position alone.
# Reached through RATCHET_EXTRA_FAIL_FN; shellcheck cannot see an indirect call.
# shellcheck disable=SC2317,SC2329
doc_fence_extra_failures() {
    local entry name recorded_cksum actual_cksum outfile plural

    case "$1" in
    detect)
        DOC_FENCE_STALE=""
        while IFS= read -r entry; do
            [[ -z "$entry" ]] && continue
            read -r name recorded_cksum <<<"$entry"
            outfile="$DOC_FENCE_OUTDIR/${name}.hew"
            [[ -f "$outfile" ]] || continue
            actual_cksum="$(cksum "$outfile" | awk '{print $1}')"
            if [[ "$recorded_cksum" != "$actual_cksum" ]]; then
                DOC_FENCE_STALE="${DOC_FENCE_STALE}${name} ${recorded_cksum} ${actual_cksum}"$'\n'
            fi
        done <<<"$DOC_FENCE_EXPECTED_CKSUM_STR"
        DOC_FENCE_STALE_COUNT="$(count_set "$DOC_FENCE_STALE")"
        RATCHET_EXTRA_FAIL_COUNT="$DOC_FENCE_STALE_COUNT"
        ;;
    report)
        ((DOC_FENCE_STALE_COUNT > 0)) || return 0
        plural="ies"
        ((DOC_FENCE_STALE_COUNT == 1)) && plural="y"
        echo "RATCHET FAIL: $DOC_FENCE_STALE_COUNT stale expected-failure metadata entr${plural}:"
        while IFS= read -r entry; do
            [[ -z "$entry" ]] && continue
            read -r name recorded_cksum actual_cksum <<<"$entry"
            echo "  STALE METADATA: $name content changed since label was written (recorded=$recorded_cksum actual=$actual_cksum) — re-verify and update the label"
        done <<<"$DOC_FENCE_STALE"
        echo ""
        ;;
    esac
}

# ── Corpus table ──────────────────────────────────────────────────────────────

case "$CORPUS" in
hew-suite)
    EXPECTED_FAILURES_FILE="${EXPECTED_FAILURES_FILE:-$REPO_ROOT/scripts/hew-suite-expected-failures.txt}"
    RATCHET_ALL_PASS_TEXT="All tests passed. Remove the expected-failures file entries when the list is empty."
    RATCHET_LIST_TRACKED=1
    RATCHET_UNEXPECTED_HELP="  To accept these as known failures, add them to:
  $EXPECTED_FAILURES_FILE"
    RATCHET_NOWPASS_HELP="  Delete these lines from:
  $EXPECTED_FAILURES_FILE
  (Do not restore a failing entry to make this green — fix the test.)"
    RATCHET_EXTRA_FAIL_FN=hew_suite_extra_failures
    RATCHET_TAIL_FN=hew_suite_tail
    run_hew_suite
    ;;
stdlib)
    EXPECTED_FAILURES_FILE="${EXPECTED_FAILURES_FILE:-$REPO_ROOT/scripts/stdlib-expected-failures.txt}"
    RATCHET_ALL_PASS_TEXT="All stdlib files pass type-check. Remove entries from expected-failures file."
    RATCHET_LIST_TRACKED=1
    RATCHET_DIAGNOSTIC_FN=stdlib_diagnostic
    RATCHET_UNEXPECTED_HELP="  To accept these as known failures, add them to:
  $EXPECTED_FAILURES_FILE"
    RATCHET_NOWPASS_HELP="  Delete these lines from:
  $EXPECTED_FAILURES_FILE
  (Do not restore a failing entry to make this green — fix the stdlib file.)"
    run_stdlib
    ;;
hew-corpus)
    EXPECTED_FAILURES_FILE="${EXPECTED_FAILURES_FILE:-$REPO_ROOT/scripts/hew-corpus-expected-failures.txt}"
    RATCHET_ALL_PASS_TEXT="All corpus files pass hew check. The expected-failures list is empty."
    RATCHET_LIST_TRACKED=1
    RATCHET_FAIL_PREFIX="CORPUS FAIL"
    RATCHET_VERDICT_LABEL="Corpus sweep"
    RATCHET_DIAGNOSTIC_FN=hew_corpus_diagnostic
    RATCHET_EXTRA_FAIL_FN=hew_corpus_extra_failures
    RATCHET_UNEXPECTED_HELP="  If these are deferred (NYI feature), add them to:
  $EXPECTED_FAILURES_FILE
  with a comment classifying the failure reason."
    RATCHET_NOWPASS_HELP="  Delete these lines from:
  $EXPECTED_FAILURES_FILE
  (Do not restore a failing entry to make this green — fix the file.)"
    run_hew_corpus
    ;;
doc-fences)
    EXPECTED_FAILURES_FILE="${EXPECTED_FAILURES_FILE:-$REPO_ROOT/scripts/doc-test-expected-failures.txt}"
    RATCHET_INDENT="    "
    RATCHET_ALL_PASS_TEXT="All doc fences pass. Consider removing the expected-failures file."
    RATCHET_ALL_PASS_LEADING_BLANK=1
    RATCHET_FAIL_LEADING_BLANK=1
    RATCHET_VERDICT_LABEL="Doc-test ratchet"
    RATCHET_DIAGNOSTIC_FN=doc_fence_diagnostic
    RATCHET_EXTRA_FAIL_FN=doc_fence_extra_failures
    RATCHET_UNEXPECTED_HELP="  A doc fence that previously passed now fails — this is a documentation
  regression.  Fix the fence in the doc file, OR if the failure is
  intentional (e.g. the surface is now NYI), add a '<!-- doctest: skip -->'
  comment before the fence and remove it from the expected-failures list.
  To accept as a known failure (discouraged): add to $EXPECTED_FAILURES_FILE"
    RATCHET_NOWPASS_HELP="  Delete these lines from: $EXPECTED_FAILURES_FILE
  (Do not restore a failing entry to keep this green — fix the docs.)"
    run_doc_fences
    ;;
esac

ratchet_verdict
