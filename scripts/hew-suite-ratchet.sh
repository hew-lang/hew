#!/usr/bin/env bash
# Run every compiled-Hew test, with content-addressed per-fixture verdicts.
#
# The expected test identities are recorded separately from verdicts. A live
# `hew test --list` must match that set before any cached report is trusted, so
# deleting or renaming tests cannot turn into a smaller green suite.
#
# Parses `hew test --format junit` output (via scripts/lib/hew_junit.py)
# rather than regex-matching the human-readable text — a cosmetic text-format
# change can no longer silently break pass/fail extraction, and the same
# JUnit report this script parses is the one uploaded to the CI checks UI.
#
# Usage:
#   scripts/hew-suite-ratchet.sh [--help]
#   scripts/hew-suite-ratchet.sh [--emit-o0-outcomes <path>]
#   scripts/hew-suite-ratchet.sh [--junit-output <path>]
#
# Options:
#   --emit-o0-outcomes <path>    Write the sorted per-test outcome lines (the
#                                same "test <name> ... ok|PASSED|FAILED|ignored"
#                                format scripts/o2-differential.sh compares) to
#                                <path>. This script's `hew test` run is O0 (its
#                                default opt level), so a CI job can hand the
#                                captured file to o2-differential.sh's
#                                --o0-outcomes flag instead of re-running the
#                                identical O0 pass a second time.
#   --junit-output <path>        Where to write the `hew test --format junit`
#                                report this script parses. Default:
#                                target/hew-test-reports/hew-suite-ratchet.xml
#                                — a stable path a CI job's upload step reads,
#                                the same way it reads target/nextest/*/junit.xml.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/cargo-output-dir.sh"
# HEW_BIN is overridable for parser tests (point it at a stub that replays
# captured runner output); production callers use the default.
HEW_BIN="${HEW_BIN:-$(cargo_debug_dir "$REPO_ROOT")/hew}"
TESTS_DIR="${HEW_TESTS_DIR:-$REPO_ROOT/tests/hew}"
EMIT_O0_OUTCOMES_FILE=""
JUNIT_OUTPUT="$REPO_ROOT/target/hew-test-reports/hew-suite-ratchet.xml"
HEW_JUNIT_PY="$REPO_ROOT/scripts/lib/hew_junit.py"
HEW_INVENTORY_PY="$REPO_ROOT/scripts/lib/hew_test_inventory.py"
EXPECTED_TESTS_FILE="${HEW_EXPECTED_TESTS_FILE:-$REPO_ROOT/scripts/hew-suite-expected-tests.txt}"
CACHE_DIR="${HEW_TEST_CACHE_DIR:-$REPO_ROOT/target/hew-test-cache}"

usage() {
    cat <<'EOF'
Usage: scripts/hew-suite-ratchet.sh [--emit-o0-outcomes <path>]

Run `hew test tests/hew/` and require every selected test to pass.

Options:
  --emit-o0-outcomes <path>    Write the full sorted per-test outcome set to <path>
                               for a downstream gate to reuse instead of re-running.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --emit-o0-outcomes)
            shift
            [[ $# -gt 0 ]] || { echo "error: --emit-o0-outcomes requires a path" >&2; exit 1; }
            EMIT_O0_OUTCOMES_FILE="$1"
            shift
            ;;
        --junit-output)
            shift
            [[ $# -gt 0 ]] || { echo "error: --junit-output requires a path" >&2; exit 1; }
            JUNIT_OUTPUT="$1"
            shift
            ;;
        --help|-h)
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

if [[ ! -f "$HEW_BIN" ]]; then
    echo "error: hew binary not found at $HEW_BIN" >&2
    echo "       Run: cargo build -p hew-cli" >&2
    exit 1
fi

if [[ ! -d "$TESTS_DIR" ]]; then
    echo "error: tests/hew/ directory not found" >&2
    exit 1
fi

# Establish the suite's name-level integrity bar before consulting the cache.
# Both this harness and the sharded harness use hew_test_inventory.py for the
# listing and JUnit identity rules.
mkdir -p "$(dirname "$JUNIT_OUTPUT")"
mkdir -p "$CACHE_DIR"
STDERR_FILE="$(mktemp /tmp/hew-suite-ratchet-stderr.XXXXXX)"
INVENTORY_FILE="$(mktemp /tmp/hew-suite-ratchet-inventory.XXXXXX)"
trap 'rm -f "$STDERR_FILE" "$INVENTORY_FILE"' EXIT

python3 "$HEW_INVENTORY_PY" list \
    --compiler "$HEW_BIN" \
    --tests "$TESTS_DIR" \
    --expected "$EXPECTED_TESTS_FILE" \
    --output "$INVENTORY_FILE"

libhew_archive=""
for candidate in "$(dirname "$HEW_BIN")/libhew.a" "$(dirname "$HEW_BIN")/hew.lib"; do
    if [[ -f "$candidate" ]]; then
        libhew_archive="$candidate"
        break
    fi
done
if [[ -z "$libhew_archive" ]]; then
    echo "error: no libhew archive beside $HEW_BIN; refusing an incomplete cache key" >&2
    exit 1
fi

suite_hash="$({
    printf '%s\n' 'hew-suite-cache-v3' 'hew test <fixture> --format junit'
    git hash-object "$HEW_BIN" "$libhew_archive" "$0" "$HEW_JUNIT_PY" \
        "$HEW_INVENTORY_PY" "$EXPECTED_TESTS_FILE"
    find "$REPO_ROOT" \
        \( -path "$REPO_ROOT/target" -o -path "$REPO_ROOT/.git" -o -path "$REPO_ROOT/.tmp" \) -prune -o \
        -type f \( -name '*.hew' -o -name '*.toml' -o -name '*.lock' \) -print \
        | LC_ALL=C sort \
        | git hash-object --stdin-paths
    env | LC_ALL=C sort \
        | sed -n '/^HEW_/p' \
        | sed '/^HEW_BIN=/d;/^HEW_TEST_CACHE_DIR=/d'
    for name in AR CC CFLAGS CI CPATH CXX DYLD_LIBRARY_PATH LANG LC_ALL LD \
        LDFLAGS LD_LIBRARY_PATH LIBRARY_PATH MACOSX_DEPLOYMENT_TARGET PATH \
        SDKROOT TZ; do
        printf '%s=%s\n' "$name" "${!name:-}"
    done
    uname -a
    for tool in clang cc ld.lld; do
        command -v "$tool" || true
        "$tool" --version 2>/dev/null | sed -n '1p' || true
    done
    printf '%s\n' "${ImageOS:-}" "${ImageVersion:-}"
} | git hash-object --stdin)"
reports=()
while IFS= read -r listed_fixture; do
    [[ -n "$listed_fixture" ]] || continue
    if [[ "$listed_fixture" = /* ]]; then
        fixture="$listed_fixture"
    else
        fixture="$REPO_ROOT/$listed_fixture"
    fi
    if [[ ! -f "$fixture" ]]; then
        echo "error: listed Hew fixture is missing: $fixture" >&2
        exit 1
    fi
    fixture_hash="$(git hash-object "$fixture")"
    cache_key="$(printf 'hew-suite-cache-v3\n%s\n%s\n%s\n' "$suite_hash" "$listed_fixture" "$fixture_hash" | git hash-object --stdin)"
    cached_report="$CACHE_DIR/$cache_key.xml"
    if [[ -s "$cached_report" ]] \
        && python3 "$HEW_INVENTORY_PY" check-report \
            --inventory "$INVENTORY_FILE" --fixture "$fixture" \
            --report "$cached_report" >/dev/null 2>&1; then
        reports+=("$cached_report")
        continue
    fi

    fresh_report="$CACHE_DIR/$cache_key.xml.new.$$"
    fresh_stderr="$CACHE_DIR/$cache_key.stderr.new.$$"
    rc=0
    "$HEW_BIN" test "$fixture" --format junit \
        > "$fresh_report" 2> "$fresh_stderr" || rc=$?
    {
        printf '%s\n' "==> $listed_fixture"
        cat "$fresh_stderr"
    } >> "$STDERR_FILE"

    if [[ "$rc" -ne 0 || ! -s "$fresh_report" ]] \
        || ! python3 "$HEW_INVENTORY_PY" check-report \
            --inventory "$INVENTORY_FILE" --fixture "$fixture" \
            --report "$fresh_report" >/dev/null 2>&1; then
        echo "error: Hew test produced an invalid or failing JUnit report for $fixture" >&2
        rm -f "$fresh_report" "$fresh_stderr"
        cat "$STDERR_FILE" >&2
        exit 1
    fi
    rm -f "$fresh_stderr"
    mv "$fresh_report" "$cached_report"
    reports+=("$cached_report")
done < <(awk -F '::' '{ print $1 }' "$INVENTORY_FILE" | LC_ALL=C sort -u)

if [[ ${#reports[@]} -eq 0 ]]; then
    echo "error: Hew suite selected no test-bearing fixtures under $TESTS_DIR" >&2
    exit 1
fi
python3 "$HEW_JUNIT_PY" --merge "$JUNIT_OUTPUT" "${reports[@]}"

# Fail closed if the runner produced no report at all: a runner crash before
# any output must never read as an empty (thus vacuously matching) run.
if [[ ! -s "$JUNIT_OUTPUT" ]]; then
    echo "error: hew test produced no JUnit report at $JUNIT_OUTPUT; refusing to ratchet" >&2
    echo "==> stderr from the run:" >&2
    cat "$STDERR_FILE" >&2
    exit 1
fi

# scripts/lib/hew_junit.py parses the report and prints one "<status>\t<name>"
# line per test plus a trailing "__SUMMARY__\t<total>\t<failures>\t<skipped>"
# line. It exits non-zero (with a diagnostic on stderr) on malformed XML or a
# <testsuites> whose failures attribute disagrees with its own <failure>
# element count — fail closed on either, the same "refuse to ratchet against
# an inconsistent run" posture the old text-parser's checks had.
PARSED=""
if ! PARSED="$(python3 "$HEW_JUNIT_PY" "$JUNIT_OUTPUT")"; then
    echo "error: could not parse JUnit report at $JUNIT_OUTPUT; refusing to ratchet" >&2
    echo "==> stderr from the run:" >&2
    cat "$STDERR_FILE" >&2
    exit 1
fi

SUMMARY_LINE="$(printf '%s\n' "$PARSED" | grep '^__SUMMARY__' || true)"
if [[ -z "$SUMMARY_LINE" ]]; then
    echo "error: hew_junit.py produced no __SUMMARY__ line; refusing to ratchet" >&2
    exit 1
fi
IFS=$'\t' read -r _ report_total report_failures _report_skipped <<< "$SUMMARY_LINE"

# Emit the full per-test outcome set for the O2-differential gate to reuse as
# its O0 baseline (C1: dedup the identical O0 re-run), reconstructed in the
# "test <name> ... ok|PASSED|FAILED|ignored" text form
# scripts/o2-differential.sh's run_outcomes() already extracts from a plain
# `hew test` run, so that consumer needs no change. Written before the
# ratchet verdict is known — the outcome set is valid regardless of whether
# the ratchet itself passes or fails.
if [[ -n "$EMIT_O0_OUTCOMES_FILE" ]]; then
    printf '%s\n' "$PARSED" \
        | awk -F'\t' '$1 != "__SUMMARY__" { print "test " $2 " ... " $1 }' \
        | sort > "$EMIT_O0_OUTCOMES_FILE"
fi

# Extract names of failing tests.
ACTUAL_STR=""
while IFS=$'\t' read -r status name; do
    [[ "$status" == "FAILED" ]] || continue
    [[ -n "$name" ]] && ACTUAL_STR="${ACTUAL_STR}${name}"$'\n'
done <<< "$(printf '%s\n' "$PARSED" | grep -v '^__SUMMARY__')"

parsed_failed=0
if [[ -n "$ACTUAL_STR" ]]; then
    parsed_failed="$(printf '%s' "$ACTUAL_STR" | awk 'NF { count++ } END { print count + 0 }')"
fi
if [[ "$parsed_failed" -ne "$report_failures" ]]; then
    echo "error: parsed $parsed_failed FAILED test(s) but report summary reports $report_failures failed; refusing to ratchet" >&2
    exit 1
fi

count_actual=0
if [[ -n "$ACTUAL_STR" ]]; then
    count_actual="$(printf '%s' "$ACTUAL_STR" | awk 'NF { count++ } END { print count + 0 }')"
fi

echo "==> Hew suite"
echo "Actual failures:   $count_actual"
echo ""

if [[ $count_actual -eq 0 ]]; then
    echo "All $report_total tests passed."
    echo "==> Hew suite: PASSED"
    exit 0
fi

echo "HEW SUITE FAIL: $count_actual test(s) failed:"
printf '%s' "$ACTUAL_STR" | sort | sed 's/^/  FAILED: /'

# Print the run's stderr (build/FFI errors, warnings) and point at the full
# JUnit report — the same report a CI job's upload step reads into the
# checks UI, so failure detail is one click away there too.
echo "==> stderr from the run:"
cat "$STDERR_FILE"
echo ""
echo "==> Full JUnit report: $JUNIT_OUTPUT"

echo ""
echo "==> Hew suite: FAILED"
exit 1
