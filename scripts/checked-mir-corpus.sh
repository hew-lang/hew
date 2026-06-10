#!/usr/bin/env bash
# Golden MIR corpus driver for examples/v05/checked-mir/.
#
# The corpus pins the textual `--dump-mir` output (raw + elab stages) for
# one fixture per compiler-known runtime-call family cluster. It is the
# behavioural oracle for internal retyping work: a refactor that claims
# "zero behaviour change" must leave every golden dump byte-identical.
# An INTENTIONAL dump change (e.g. a MIR carrier gaining a typed field
# that the Debug rendering prints) regenerates the golden in the same
# commit, with the diff justified in the commit body.
#
# Usage:
#   scripts/checked-mir-corpus.sh golden   # (re)capture golden dumps
#   scripts/checked-mir-corpus.sh verify   # re-dump and diff against golden
#
# Env:
#   HEW_BIN — compiler binary (default: target/debug/hew at the repo root).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CORPUS="$ROOT/examples/v05/checked-mir"
GOLDEN="$CORPUS/golden"
HEW_BIN="${HEW_BIN:-$ROOT/target/debug/hew}"
MODE="${1:-verify}"
STAGES=(raw elab)

if [[ ! -x "$HEW_BIN" ]]; then
    echo "checked-mir-corpus: compiler binary not found at $HEW_BIN" >&2
    echo "build it first (make hew) or set HEW_BIN" >&2
    exit 2
fi

fixtures=()
while IFS= read -r f; do
    fixtures+=("$f")
done < <(find "$CORPUS" -maxdepth 1 -name '*.hew' | sort)

if [[ ${#fixtures[@]} -eq 0 ]]; then
    echo "checked-mir-corpus: no fixtures under $CORPUS" >&2
    exit 2
fi

case "$MODE" in
golden)
    mkdir -p "$GOLDEN"
    for f in "${fixtures[@]}"; do
        name="$(basename "$f" .hew)"
        for stage in "${STAGES[@]}"; do
            "$HEW_BIN" compile --dump-mir "$stage" "$f" >"$GOLDEN/$name.$stage.mir"
            echo "captured $name.$stage.mir"
        done
    done
    ;;
verify)
    fail=0
    tmpdir="$(mktemp -d)"
    trap 'rm -rf "$tmpdir"' EXIT
    for f in "${fixtures[@]}"; do
        name="$(basename "$f" .hew)"
        for stage in "${STAGES[@]}"; do
            golden_file="$GOLDEN/$name.$stage.mir"
            if [[ ! -f "$golden_file" ]]; then
                echo "MISSING GOLDEN: $name.$stage.mir (run: make checked-mir-golden)" >&2
                fail=1
                continue
            fi
            "$HEW_BIN" compile --dump-mir "$stage" "$f" >"$tmpdir/$name.$stage.mir"
            if ! diff -u "$golden_file" "$tmpdir/$name.$stage.mir" >"$tmpdir/$name.$stage.diff"; then
                echo "DUMP DRIFT: $name ($stage stage) — first 40 diff lines:" >&2
                head -40 "$tmpdir/$name.$stage.diff" >&2
                fail=1
            fi
        done
    done
    # Stale goldens (golden exists, fixture removed) are an error too:
    # they silently shrink the oracle's coverage.
    for g in "$GOLDEN"/*.mir; do
        [[ -e "$g" ]] || continue
        base="$(basename "$g")"
        name="${base%.*.mir}"
        if [[ ! -f "$CORPUS/$name.hew" ]]; then
            echo "STALE GOLDEN: $base has no fixture $name.hew" >&2
            fail=1
        fi
    done
    if [[ $fail -ne 0 ]]; then
        echo "checked-mir-verify: FAILED" >&2
        exit 1
    fi
    echo "checked-mir-verify: OK (${#fixtures[@]} fixtures x ${#STAGES[@]} stages byte-identical)"
    ;;
*)
    echo "usage: $0 {golden|verify}" >&2
    exit 2
    ;;
esac
