#!/usr/bin/env bash
# Fixture tests for scripts/corpus-ratchet.sh's newly-passing verdict.
#
# The policy this proves:
#
#   * an unexpected FAILURE is red on every tier, with or without
#     --strict-passes. That is a regression and it always blocks.
#   * a listed failure that now PASSES is debt accounting. It annotates by
#     default -- failing somebody's pull request because they FIXED something
#     punishes exactly the behaviour the ratchet exists to encourage -- and it
#     BLOCKS under --strict-passes, which the default branch and the release
#     boundary pass, because the list must be current where the tree is
#     integrated and where the artefact is cut.
#
# Driven through the real verdict function against synthetic expected/actual
# sets, so the assertions are about the exit status and the emitted message,
# not about the shape of the source.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RATCHET="${ROOT}/scripts/corpus-ratchet.sh"

failures=0

# Run ratchet_verdict with a chosen expected/actual pair, by sourcing the
# script's function definitions without letting it dispatch a corpus.
run_verdict() {
    local expected="$1" actual="$2" strict="$3"
    bash -c '
        set -euo pipefail
        ratchet="$1"; expected="$2"; actual="$3"; strict="$4"; root="$5"
        # shellcheck source=/dev/null
        source "${root}/scripts/lib/line-set.sh"
        # Take the definitions up to the dispatch section; the file exits early
        # on a missing corpus, so it cannot be sourced whole.
        eval "$(sed -n "/^set_difference()/,/^# .. Corpus: hew-suite/p" "$ratchet")"
        EXPECTED_STR="$expected"
        ACTUAL_STR="$actual"
        STRICT_PASSES="$strict"
        RATCHET_EXTRA_FAIL_FN=""
        RATCHET_DIAGNOSTIC_FN=""
        RATCHET_TAIL_FN=""
        RATCHET_INDENT=""
        RATCHET_ALL_PASS_TEXT="all pass"
        RATCHET_ALL_PASS_LEADING_BLANK=0
        RATCHET_FAIL_LEADING_BLANK=0
        RATCHET_LIST_TRACKED=0
        RATCHET_FAIL_PREFIX="RATCHET"
        RATCHET_VERDICT_LABEL="fixture"
        RATCHET_UNEXPECTED_HELP="unexpected help"
        RATCHET_NOWPASS_HELP="nowpass help"
        EXPECTED_FAILURES_FILE="fixture-list.txt"
        ratchet_verdict
    ' _ "$RATCHET" "$expected" "$actual" "$strict" "$ROOT" 2>&1 || return $?
}

check() {
    local name="$1" expected="$2" actual="$3" strict="$4"
    local want_status="$5" want_text="$6"
    local output status=0
    output="$(run_verdict "$expected" "$actual" "$strict")" || status=$?
    if [[ "$status" -ne "$want_status" ]]; then
        echo "FAIL ${name}: exit ${status}, expected ${want_status}"
        printf '    %s\n' "${output}"
        failures=$((failures + 1))
        return
    fi
    if [[ -n "$want_text" ]] && ! grep -qF "$want_text" <<< "$output"; then
        echo "FAIL ${name}: output did not contain '${want_text}'"
        printf '    %s\n' "${output}"
        failures=$((failures + 1))
        return
    fi
    echo "PASS ${name}"
}

# An exact match is green either way.
check "exact match, default" $'a\nb' $'a\nb' 0 0 "PASSED"
check "exact match, strict" $'a\nb' $'a\nb' 1 0 "PASSED"

# A regression blocks on every tier. This is the verdict that must NOT move.
check "unexpected failure, default" $'a' $'a\nb' 0 1 "UNEXPECTED failure"
check "unexpected failure, strict" $'a' $'a\nb' 1 1 "UNEXPECTED failure"

# A newly-passing entry annotates by default and blocks under --strict-passes.
check "newly passing, default" $'a\nb' $'a' 0 0 "::warning::"
check "newly passing, default is green" $'a\nb' $'a' 0 0 "PASSED"
check "newly passing, strict" $'a\nb' $'a' 1 1 "now PASS"

# Both at once: the regression still decides the verdict on the default tier,
# and the newly-passing entry is still reported rather than swallowed.
check "both, default" $'a\nb' $'a\nc' 0 1 "UNEXPECTED failure"
check "both, strict" $'a\nb' $'a\nc' 1 1 "now PASS"

# The flag is accepted by the real command line, not only by the function.
if ! bash "$RATCHET" --help | grep -qF -- "--strict-passes"; then
    echo "FAIL --strict-passes is undocumented in --help"
    failures=$((failures + 1))
else
    echo "PASS --strict-passes is documented"
fi

if [[ "$failures" -ne 0 ]]; then
    echo "corpus ratchet pass-policy: ${failures} failure(s)" >&2
    exit 1
fi
echo "corpus ratchet pass-policy: PASS"
