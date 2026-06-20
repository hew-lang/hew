#!/usr/bin/env bash
# Per-function byte-identity oracle for codegen refactors.
#
# The raw `.ll` dump produced by `hew compile --emit-dir` is NON-DETERMINISTIC
# at the MODULE level: global type-decl order, whole-function emission order,
# and the `@str_lit.N` / `@__hew_const__NAME__N` pool-id suffixes are all
# HashMap-iteration-driven, so a positional `diff` of the whole file
# false-positives massively. The INSTRUCTION sequence WITHIN a named `define`
# IS deterministic for the same binary. This script:
#
#   1. Extracts every `define … { … }` body from each .ll file, keyed by
#      mangled name.
#   2. Normalises the module-level non-deterministic pool-id suffixes so that
#      the same binary compiled twice produces IDENTICAL normalised bodies.
#   3. Sorts the per-function records by name and diffs baseline-vs-head.
#   4. Exits non-zero (and prints a unified diff) if ANY function body differs.
#
# A pure codegen relocation (extract-helper, file-split, dedup) that changes
# NO emitted IR must produce zero differing function bodies.  A diff here
# means the change is NOT a pure relocation — stop and revert.
#
# Usage:
#   scripts/ll-byte-identity.sh <baseline-dir> <head-dir>
#
#   <baseline-dir>  directory containing the golden .ll files (one per fixture)
#   <head-dir>      directory containing the head .ll files to compare against
#
# Both directories must contain .ll files with matching base names.  Extra .ll
# files in either directory are reported as missing-counterpart warnings (not
# failures), so adding a new fixture does not break the oracle.
#
# Exit codes:
#   0  all matched function bodies are byte-identical
#   1  one or more function bodies differ (diff printed to stdout)
#   2  usage error or no .ll files found in baseline-dir
#
# Env:
#   LL_IDENTITY_VERBOSE=1  print per-fixture IDENTICAL/DIFFERS status lines
set -uo pipefail

BASELINE_DIR="${1:-}"
HEAD_DIR="${2:-}"

if [[ -z "$BASELINE_DIR" || -z "$HEAD_DIR" ]]; then
  echo "usage: $(basename "$0") <baseline-dir> <head-dir>" >&2
  exit 2
fi
if [[ ! -d "$BASELINE_DIR" ]]; then
  echo "ll-byte-identity: baseline dir not found: $BASELINE_DIR" >&2
  exit 2
fi
if [[ ! -d "$HEAD_DIR" ]]; then
  echo "ll-byte-identity: head dir not found: $HEAD_DIR" >&2
  exit 2
fi

VERBOSE="${LL_IDENTITY_VERBOSE:-0}"

# ---------------------------------------------------------------------------
# norm(): canonicalize module-level non-deterministic numeric suffixes.
#
# The codegen assigns pool IDs from HashMap iteration order; they shift
# run-to-run (and build-to-build) for the SAME semantic program.  These
# suffixes carry no instruction-logic — only the symbol's pool slot.
# Canonicalize them so the per-function body comparison is purely about
# emitted instructions.
#
# Patterns normalised:
#   @__hew_const__<NAME>__<N>  →  @__hew_const__<NAME>__N
#   @str_lit.<N>               →  @str_lit.N
#   @str_lit (bare, no suffix) →  left as-is (it IS the first pool slot,
#                                  already stable — the .1/.2/… siblings shift)
# ---------------------------------------------------------------------------
norm() {
  sed -E \
    -e 's/@__hew_const__([A-Za-z0-9_]+)__[0-9]+/@__hew_const__\1__N/g' \
    -e 's/@str_lit\.[0-9]+/@str_lit.N/g'
}

# ---------------------------------------------------------------------------
# all_fns(): emit every function body from one .ll file, each prefixed by
# its mangled name as "===FN <key>===", then sort the records by key.
#
# The awk pass extracts each contiguous `define … { … }` block (the closing
# `}` is always the first character on its line in LLVM textual IR).  There
# is NO ramp/name filter — every function body is included so that the oracle
# covers all emitted code, not just a hand-selected subset.
# ---------------------------------------------------------------------------
all_fns() {
  norm < "$1" | awk '
    /^define / {
      name = $0
      buf  = name "\n"
      collecting = 1
      next
    }
    collecting {
      buf = buf $0 "\n"
      if ($0 ~ /^}/) {
        # key = the @mangled_name for stable lexicographic sort
        if (match(name, /@[A-Za-z0-9_$."]+\(/))
          key = substr(name, RSTART, RLENGTH)
        else
          key = name
        printf "===FN %s===\n%s", key, buf
        collecting = 0
      }
    }
  ' | awk '
    /^===FN / {
      if (rec != "") recs[n++] = rec
      rec = $0 "\n"
      next
    }
    { rec = rec $0 "\n" }
    END {
      if (rec != "") recs[n++] = rec
      # stable insertion sort by first line (the ===FN key)
      for (i = 0; i < n; i++)
        for (j = i + 1; j < n; j++)
          if (recs[j] < recs[i]) { t = recs[i]; recs[i] = recs[j]; recs[j] = t }
      for (i = 0; i < n; i++) printf "%s", recs[i]
    }
  '
}

# ---------------------------------------------------------------------------
# Main comparison loop
# ---------------------------------------------------------------------------
fail=0
matched=0
missing=0

for bfile in "$BASELINE_DIR"/*.ll; do
  [[ -e "$bfile" ]] || { echo "ll-byte-identity: no .ll files in $BASELINE_DIR" >&2; exit 2; }
  label=$(basename "$bfile" .ll)
  hfile="$HEAD_DIR/$label.ll"

  if [[ ! -f "$hfile" ]]; then
    echo "  $label: SKIP (no head .ll — new fixture or renamed)"
    missing=$((missing + 1))
    continue
  fi

  b=$(all_fns "$bfile")
  h=$(all_fns "$hfile")

  if [[ -z "$b" ]]; then
    [[ "$VERBOSE" == "1" ]] && echo "  $label: (no function bodies — skipped)"
    continue
  fi

  matched=$((matched + 1))
  if [[ "$b" == "$h" ]]; then
    [[ "$VERBOSE" == "1" ]] && echo "  $label: IDENTICAL"
  else
    echo "  $label: *** DIFFERS ***"
    diff <(printf '%s' "$b") <(printf '%s' "$h")
    fail=1
  fi
done

# Report head-only files (not a failure — just informational)
for hfile in "$HEAD_DIR"/*.ll; do
  [[ -e "$hfile" ]] || continue
  label=$(basename "$hfile" .ll)
  if [[ ! -f "$BASELINE_DIR/$label.ll" ]]; then
    echo "  $label: (head-only — new fixture, no baseline to compare)"
  fi
done

if [[ $fail -ne 0 ]]; then
  echo "ll-byte-identity: FAILED ($matched fixture(s) compared; $missing missing)" >&2
  exit 1
fi

echo "ll-byte-identity: OK ($matched fixture(s) byte-identical)"
exit 0
