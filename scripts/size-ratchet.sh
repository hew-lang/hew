#!/usr/bin/env bash
# scripts/size-ratchet.sh check|record
#
# Instrument: `wc -l` over each workspace crate's `<crate>/src/**/*.rs`
# (the instrument this repo can run today; see #3296 for why the finer
# non-test-only counter named by the platform plan was not ported —
# neither `scripts/crate-size.py` nor `platform/` exists here, and
# rewriting that counting rule from prose is not safe). Crates are read
# from the `[workspace]` `members` list in the repo-root Cargo.toml.
#
# check: fails when any crate's line count exceeds its
# scripts/size-ratchet.tsv ceiling, printing `crate count ceiling
# ok|OVER` for every crate. A ceiling set below the current count makes
# this exit non-zero — including a crate seeded over its ceiling on
# purpose (see the tsv header); re-budgeting a ceiling to make the gate
# pass is forbidden (#3296).
# record: writes scripts/size-ratchet.tsv from the current per-crate
# counts. `check` calls this itself when the tsv is missing, then
# passes — a missing tsv has nothing to compare against yet, the same
# shape as scripts/check-time-ratchet.sh (V060-FD-1).
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TSV="$REPO_ROOT/scripts/size-ratchet.tsv"
CARGO_TOML="$REPO_ROOT/Cargo.toml"

MODE="${1:-}"
case "$MODE" in
check | record) ;;
*)
    echo "usage: size-ratchet.sh check|record" >&2
    exit 64
    ;;
esac

[ -f "$CARGO_TOML" ] || {
    echo "size-ratchet: workspace manifest missing: $CARGO_TOML" >&2
    exit 1
}

# Workspace members: the quoted path on each line between `members = [`
# and the closing `]`. `hew-parser/fuzz` is a separate `exclude` entry,
# not a member, so it is never counted.
mapfile -t CRATES < <(awk '/^members = \[/{f=1; next} /^\]/{f=0} f' "$CARGO_TOML" |
    sed -nE 's/^[[:space:]]*"([^"]+)".*/\1/p')

[ "${#CRATES[@]}" -gt 0 ] || {
    echo "size-ratchet: no [workspace] members found in $CARGO_TOML" >&2
    exit 1
}

count_crate() {
    find "$REPO_ROOT/$1/src" -name '*.rs' -exec cat {} + 2>/dev/null | wc -l
}

record_tsv() {
    local tmp
    tmp=$(mktemp "${TMPDIR:-/tmp}/size-ratchet-XXXXXX")
    {
        echo "# crate	ceiling_lines"
        echo "#"
        echo "# One row per workspace crate (Cargo.toml [workspace] members)."
        echo "# ceiling_lines is wc -l over <crate>/src/**/*.rs (scripts/size-ratchet.sh)."
        echo "# make size-ratchet fails when a crate's current count exceeds its"
        echo "# ceiling here. Ceilings are only ever lowered by deleting code, never"
        echo "# raised to meet a count (#3296)."
        for crate in "${CRATES[@]}"; do
            printf '%s\t%s\n' "$crate" "$(count_crate "$crate")"
        done
    } >"$tmp"
    mv "$tmp" "$TSV"
}

if [ "$MODE" = "record" ]; then
    record_tsv
    echo "size-ratchet: recorded $(wc -l <"$TSV" | tr -d ' ') rows to $TSV"
    exit 0
fi

# check
if [ ! -f "$TSV" ]; then
    record_tsv
    echo "size-ratchet: no ceilings tsv; recorded $TSV and passing (nothing to compare against yet)"
    exit 0
fi

STATUS=0
for crate in "${CRATES[@]}"; do
    ceiling=$(awk -F'\t' -v c="$crate" '!/^#/ && $1==c {print $2}' "$TSV" | tail -1)
    if [ -z "$ceiling" ]; then
        echo "size-ratchet: no ceiling row for crate '$crate' in $TSV" >&2
        STATUS=1
        continue
    fi
    count=$(count_crate "$crate")
    if [ "$count" -gt "$ceiling" ]; then
        echo "size-ratchet: $crate $count $ceiling OVER"
        STATUS=1
    else
        echo "size-ratchet: $crate $count $ceiling ok"
    fi
done

if [ "$STATUS" -ne 0 ]; then
    echo "size-ratchet: FAIL — one or more crates exceed their ceiling" >&2
fi
exit "$STATUS"
