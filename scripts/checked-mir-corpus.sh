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
# Regeneration visibility: `golden` never overwrites silently. It diffs
# every dump against the committed golden first and prints a per-file
# CHANGED/NEW report with line counts, then rewrites
# `golden/MANIFEST.sha256`. `verify` re-checks that manifest against the
# goldens on disk, so a regeneration cannot land without touching one
# central, small, line-per-golden file that shows a reviewer exactly
# which goldens moved and how many — even when the .mir diffs themselves
# are collapsed.
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
MANIFEST="$GOLDEN/MANIFEST.sha256"
HEW_BIN="${HEW_BIN:-$ROOT/target/debug/hew}"
MODE="${1:-verify}"
STAGES=(raw elab)

# sha256 over stdin-named files; `sha256sum` on Linux, `shasum -a 256` on
# macOS. Both print `<hash>  <name>`, so the manifest format is identical.
sha256_of() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$@"
    else
        shasum -a 256 "$@"
    fi
}

# Manifest lines for every committed golden, sorted byte-wise so the file
# is stable across platforms and locales.
render_manifest() {
    (
        cd "$GOLDEN"
        shopt -s nullglob
        local names=(*.mir)
        [[ ${#names[@]} -eq 0 ]] && return 0
        sha256_of "${names[@]}" | LC_ALL=C sort -k2
    )
}

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
    tmpdir="$(mktemp -d)"
    trap 'rm -rf "$tmpdir"' EXIT
    changed=()
    added=()
    unchanged=0
    for f in "${fixtures[@]}"; do
        name="$(basename "$f" .hew)"
        for stage in "${STAGES[@]}"; do
            golden_file="$GOLDEN/$name.$stage.mir"
            fresh="$tmpdir/$name.$stage.mir"
            "$HEW_BIN" compile --dump-mir "$stage" "$f" >"$fresh"
            if [[ ! -f "$golden_file" ]]; then
                added+=("$name.$stage.mir")
            elif cmp -s "$golden_file" "$fresh"; then
                unchanged=$((unchanged + 1))
                continue
            else
                plus="$(diff "$golden_file" "$fresh" | grep -c '^>' || true)"
                minus="$(diff "$golden_file" "$fresh" | grep -c '^<' || true)"
                changed+=("$name.$stage.mir (+$plus -$minus)")
            fi
            cp "$fresh" "$golden_file"
        done
    done
    render_manifest >"$MANIFEST"
    # The regeneration report: a golden corpus that moves silently is a
    # corpus that cannot fail for the change regenerating it, so every
    # recapture states what moved before the commit is written.
    echo "checked-mir-golden: ${#changed[@]} changed, ${#added[@]} new, $unchanged unchanged"
    for entry in ${added[@]+"${added[@]}"}; do
        echo "  NEW     $entry"
    done
    for entry in ${changed[@]+"${changed[@]}"}; do
        echo "  CHANGED $entry"
    done
    if [[ ${#changed[@]} -gt 0 || ${#added[@]} -gt 0 ]]; then
        echo "checked-mir-golden: quote this report in the commit body." >&2
    fi
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
    # The manifest is the reviewer-visible record of which goldens a commit
    # regenerated. Checking it here keeps it honest in both directions: a
    # golden edited without recapturing, and a manifest edited without
    # moving the golden, both fail.
    if [[ ! -f "$MANIFEST" ]]; then
        echo "MISSING MANIFEST: $MANIFEST (run: make checked-mir-golden)" >&2
        fail=1
    else
        render_manifest >"$tmpdir/MANIFEST.sha256"
        if ! diff -u "$MANIFEST" "$tmpdir/MANIFEST.sha256" >"$tmpdir/manifest.diff"; then
            echo "MANIFEST DRIFT: golden/MANIFEST.sha256 does not match the goldens on disk" >&2
            echo "(run: make checked-mir-golden)" >&2
            head -40 "$tmpdir/manifest.diff" >&2
            fail=1
        fi
    fi
    if [[ $fail -ne 0 ]]; then
        echo "checked-mir-verify: FAILED" >&2
        exit 1
    fi
    echo "checked-mir-verify: OK (${#fixtures[@]} fixtures x ${#STAGES[@]} stages byte-identical, manifest in sync)"
    ;;
*)
    echo "usage: $0 {golden|verify}" >&2
    exit 2
    ;;
esac
