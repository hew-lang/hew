#!/usr/bin/env bash
# scripts/run-journeys.sh <day-one|day-two|week-one-local>
#
# Runs one journey script under repros/journeys/, captures its `step
# <id>: pass|fail` lines, and compares the set of failing step ids
# against scripts/journeys-expected.tsv (columns: step_id, issue_or_lane,
# reason). Exits 0 exactly when the two sets are equal: every step listed
# for this journey in the expected file failed, and no step outside that
# list failed. The ratchet only tightens (V060-FD-1): a step that starts
# passing must have its row deleted by the lane that fixed it; a step
# that starts failing without a row is a new regression, not a pass.
#
# day-two runs under scripts/registry-harness.sh so its registry-dependent
# steps get a real local registry when one is available; when the harness
# reports no registry (exit 75), day-two.sh still runs, without
# HEW_REGISTRY, so its own no_registry() guards record those steps failed
# for the right reason instead of the runner treating a hung command as a
# harness bug.
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
JOURNEYS_DIR="$REPO_ROOT/repros/journeys"
EXPECTED_TSV="$REPO_ROOT/scripts/journeys-expected.tsv"
EX_UNAVAILABLE=75

JOURNEY="${1:-}"
case "$JOURNEY" in
day-one | day-two | week-one-local) ;;
*)
    echo "usage: run-journeys.sh <day-one|day-two|week-one-local>" >&2
    exit 64
    ;;
esac

SCRIPT="$JOURNEYS_DIR/$JOURNEY.sh"
[ -x "$SCRIPT" ] || {
    echo "run-journeys: missing or non-executable: $SCRIPT" >&2
    exit 1
}

# The journey scripts require TMPDIR outside the checkout; CI runners
# (ubuntu-latest has no TMPDIR by default) get RUNNER_TEMP, then /tmp.
export TMPDIR="${TMPDIR:-${RUNNER_TEMP:-/tmp}}"
export HEW_BIN="${HEW_BIN:-hew}"
# A relative HEW_BIN (the Makefile's default is target/release-lib/hew,
# relative to the repo root) stops resolving the moment a journey script
# `cd`s into its scratch dir. Canonicalize it here, before that happens;
# a bare command name (no `/`) is left to resolve via PATH as today.
case "$HEW_BIN" in
*/*)
    if [ "${HEW_BIN#/}" = "$HEW_BIN" ]; then
        HEW_BIN="$REPO_ROOT/$HEW_BIN"
    fi
    ;;
esac

OUT="$(mktemp "${TMPDIR}/run-journeys-out-XXXXXX")"
trap 'rm -f "$OUT"' EXIT

# week-one-local binds JOURNEY_PORT_BASE (default 8080, per its own header
# comment); a caller who leaves it unset can collide with an unrelated
# service already on 8080 on a shared/dev machine, producing a false pass
# (the stray service answers /health with 200) or a false fail. Pick a
# free port the same way scripts/registry-harness.sh does, unless the
# caller set one explicitly.
if [ "$JOURNEY" = "week-one-local" ] && [ -z "${JOURNEY_PORT_BASE:-}" ] && command -v python3 >/dev/null 2>&1; then
    FREE_PORT=$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()' 2>/dev/null)
    [ -n "$FREE_PORT" ] && export JOURNEY_PORT_BASE="$FREE_PORT"
fi

if [ "$JOURNEY" = "day-two" ]; then
    if bash "$REPO_ROOT/scripts/registry-harness.sh" bash "$SCRIPT" >"$OUT" 2>&1; then
        :
    else
        RC=$?
        if [ "$RC" -eq "$EX_UNAVAILABLE" ]; then
            bash "$SCRIPT" >"$OUT" 2>&1
        fi
        # Any other nonzero exit from day-two.sh itself is a harness/runner bug,
        # not a step failure: day-two.sh always exits 0 by design (never `set -e`).
    fi
else
    bash "$SCRIPT" >"$OUT" 2>&1
fi

cat "$OUT"

# Collect every step id this run reported, split by pass/fail.
PASSED=$(grep -oE "^step ${JOURNEY}\.[0-9]+: pass$" "$OUT" | sed -E 's/^step (.*): pass$/\1/' | sort -u)
FAILED=$(grep -oE "^step ${JOURNEY}\.[0-9]+: fail$" "$OUT" | sed -E 's/^step (.*): fail$/\1/' | sort -u)
SEEN_COUNT=$(printf '%s\n%s\n' "$PASSED" "$FAILED" | grep -c . || true)

if [ "$SEEN_COUNT" -eq 0 ]; then
    echo "run-journeys: $JOURNEY reported zero steps — the script died before its first assertion; treating as failure, not an empty-set pass" >&2
    exit 1
fi

# Expected-failing step ids for this journey (column 1, ignoring comments/blank lines).
EXPECTED=$(awk -F'\t' -v j="$JOURNEY." '!/^#/ && NF>=2 && index($1, j)==1 {print $1}' "$EXPECTED_TSV" | sort -u)

UNEXPECTED_FAILURES=$(comm -23 <(printf '%s\n' "$FAILED" | sed '/^$/d') <(printf '%s\n' "$EXPECTED" | sed '/^$/d'))
STALE_EXPECTATIONS=$(comm -23 <(printf '%s\n' "$EXPECTED" | sed '/^$/d') <(printf '%s\n' "$FAILED" | sed '/^$/d'))

STATUS=0
if [ -n "$UNEXPECTED_FAILURES" ]; then
    echo "run-journeys: failing steps with no row in journeys-expected.tsv (new regression, or the row's id is wrong):" >&2
    echo "  ${UNEXPECTED_FAILURES//$'\n'/$'\n  '}" >&2
    STATUS=1
fi
if [ -n "$STALE_EXPECTATIONS" ]; then
    echo "run-journeys: journeys-expected.tsv rows for steps that now pass (delete these rows in the fixing lane's PR):" >&2
    echo "  ${STALE_EXPECTATIONS//$'\n'/$'\n  '}" >&2
    STATUS=1
fi

if [ "$STATUS" -eq 0 ]; then
    echo "run-journeys: $JOURNEY — $(printf '%s\n' "$PASSED" | grep -c .) passed, $(printf '%s\n' "$FAILED" | grep -c .) failed (all expected), 0 unexpected"
fi

exit "$STATUS"
