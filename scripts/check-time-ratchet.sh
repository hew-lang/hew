#!/usr/bin/env bash
# scripts/check-time-ratchet.sh check|record
#
# Fixture: `hew check std/net/http/http.hew`, the largest std module a
# newcomer's program pulls in. Runs it five times and takes the median
# wall-clock time in milliseconds. `host_class` is `uname -m` plus the CI
# runner label when CI is set, else "local" — the same binary run on
# different hardware has a different budget, so the ratchet is per class.
#
# record: writes/replaces scripts/check-time-baseline.tsv's row for this
# host_class with the measured median.
# check: fails when the measured median exceeds 2x that row's median_ms.
# A host_class with no row yet cannot be compared against itself, so
# `check` records one and passes, printing that it did; the recorded row
# is only durable once someone commits it, so a new runner class is
# compared from the first run after that (V060-FD-1).
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BASELINE_TSV="$REPO_ROOT/scripts/check-time-baseline.tsv"
FIXTURE="std/net/http/http.hew"
HEW_BIN="${HEW_BIN:-hew}"

MODE="${1:-}"
case "$MODE" in
check | record) ;;
*)
    echo "usage: check-time-ratchet.sh check|record" >&2
    exit 64
    ;;
esac

command -v "$HEW_BIN" >/dev/null 2>&1 || {
    echo "check-time-ratchet: HEW_BIN '$HEW_BIN' not found" >&2
    exit 1
}
[ -f "$REPO_ROOT/$FIXTURE" ] || {
    echo "check-time-ratchet: fixture missing: $FIXTURE" >&2
    exit 1
}

runner_label="local"
if [ -n "${CI:-}" ]; then
    runner_label="${RUNNER_OS:-ci}"
fi
HOST_CLASS="$(uname -m)-${runner_label}"

# Millisecond wall-clock: `date +%s%3N` is GNU-only, but this repo's CI and
# dev hosts are all GNU coreutils or a Bash-only fallback would double the
# script's surface for no real gain (macOS/BSD add `gdate`, not stdlib date
# flags) — WHEN this needs a native macOS/BSD host without gdate, WHAT: use
# bash's $EPOCHREALTIME (bash 5+) instead of shelling out to `date`.
now_ms() {
    if [ -n "${EPOCHREALTIME:-}" ]; then
        awk -v t="$EPOCHREALTIME" 'BEGIN { printf "%d", t * 1000 }'
    else
        date +%s%3N
    fi
}

TIMES=()
for _ in 1 2 3 4 5; do
    START=$(now_ms)
    "$HEW_BIN" check "$REPO_ROOT/$FIXTURE" >/dev/null 2>&1
    RC=$?
    END=$(now_ms)
    [ "$RC" -eq 0 ] || {
        echo "check-time-ratchet: hew check exited $RC on $FIXTURE" >&2
        exit 1
    }
    TIMES+=("$((END - START))")
done

# Median of 5: sort, take the middle.
MEDIAN=$(printf '%s\n' "${TIMES[@]}" | sort -n | sed -n '3p')

record_baseline() {
    local tmp
    tmp=$(mktemp "${TMPDIR:-/tmp}/check-time-baseline-XXXXXX")
    # Keep the header comment block and every other host_class's row
    # unchanged; drop this host_class's old row (if any) and append its
    # freshly measured one.
    awk -F'\t' -v h="$HOST_CLASS" '/^#/ || $1 != h' "$BASELINE_TSV" >"$tmp"
    printf '%s\t%s\t%s\n' "$HOST_CLASS" "$MEDIAN" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >>"$tmp"
    mv "$tmp" "$BASELINE_TSV"
}

if [ "$MODE" = "record" ]; then
    record_baseline
    echo "check-time-ratchet: recorded ${HOST_CLASS} median=${MEDIAN}ms over 5 runs of hew check ${FIXTURE}"
    exit 0
fi

# check
BASELINE_MS=$(awk -F'\t' -v h="$HOST_CLASS" '!/^#/ && $1==h {print $2}' "$BASELINE_TSV" | tail -1)
if [ -z "$BASELINE_MS" ]; then
    record_baseline
    echo "check-time-ratchet: no baseline for host_class=${HOST_CLASS}; recorded median=${MEDIAN}ms and passing (nothing to compare against yet)"
    exit 0
fi

CEILING=$((BASELINE_MS * 2))
echo "check-time-ratchet: host_class=${HOST_CLASS} median=${MEDIAN}ms baseline=${BASELINE_MS}ms ceiling=${CEILING}ms"
if [ "$MEDIAN" -gt "$CEILING" ]; then
    echo "check-time-ratchet: FAIL — median ${MEDIAN}ms exceeds 2x baseline ${BASELINE_MS}ms" >&2
    exit 1
fi
echo "check-time-ratchet: ok"
