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
# norm(): canonicalize module-level non-deterministic pool-id suffixes.
#
# The codegen assigns pool IDs from HashMap iteration order; they shift
# run-to-run (and build-to-build) for the SAME semantic program.
# Canonicalize them so the per-function body comparison is purely about
# emitted instructions, not symbol-slot numbering.
#
# String-pool symbols (str_lit, __hew_const_str__NAME__N) carry CONTENT that
# discriminates semantic identity.  Two different string literals produce two
# different @str_lit.N globals with different c"..." initializers — a content
# change (e.g. "foo" → "bar") MUST be caught.  A pool-id reordering between
# runs for symbols with the SAME content must NOT be flagged.
#
# The solution: substitute each pool symbol reference with a canonical token
# derived from the INITIALIZER CONTENT, not the pool-id number.  This is a
# two-pass awk over the same file:
#   Pass 1  — collect every @str_lit[.N] / @__hew_const_str__NAME__N global
#             definition and record its c"…" content.
#   Pass 2  — replace each pool-symbol reference with @_pool_[<content>].
#
# @__hew_const__NAME__N (numeric constants) keep the existing treatment:
# the NAME already discriminates the constant; only the pool-id suffix is
# non-deterministic.  Collapse it to @__hew_const__NAME__N (literal "N").
#
# Patterns normalised:
#   @str_lit               }  → @_pool_[c"<initializer-bytes>"]
#   @str_lit.<N>           }    (content-derived canonical token)
#   @__hew_const_str__<NAME>__<N>  →  @_pool_[c"<initializer-bytes>"]
#   @__hew_const__<NAME>__<N>      →  @__hew_const__<NAME>__N
# ---------------------------------------------------------------------------
norm() {
  local file="$1"
  awk '
    # ------------------------------------------------------------------
    # Pass 1 (NR == FNR): scan global definitions and build the
    # pool_content[sym] → c"..." map.
    #
    # Matched definition forms:
    #   @str_lit = private unnamed_addr constant [N x i8] c"…", align 1
    #   @str_lit.4 = private unnamed_addr constant [N x i8] c"…", align 1
    #   @__hew_const_str__NAME__4 = ... c"…", ...
    # ------------------------------------------------------------------
    NR == FNR {
      if (match($0, /^@(str_lit(\.[0-9]+)?|__hew_const_str__[A-Za-z0-9_]+__[0-9]+) = /)) {
        sym  = substr($0, RSTART + 1, RLENGTH - 4)      # strip leading @ and trailing " = "
        rest = substr($0, RSTART + RLENGTH - 1)
        if (match(rest, /c"[^"]*"/))
          pool_content[sym] = substr(rest, RSTART, RLENGTH)
      }
      next
    }
    # ------------------------------------------------------------------
    # Between passes (FNR == 1 on the second file visit): sort the
    # collected symbols by length descending so that longer names
    # (e.g. @str_lit.4) are substituted before their shorter prefix
    # (@str_lit) — prevents partial-match corruption.
    # ------------------------------------------------------------------
    FNR == 1 {
      n_syms = 0
      for (s in pool_content)
        syms[n_syms++] = s
      for (i = 0; i < n_syms; i++)
        for (j = i + 1; j < n_syms; j++)
          if (length(syms[j]) > length(syms[i])) {
            t = syms[i]; syms[i] = syms[j]; syms[j] = t
          }
    }
    # ------------------------------------------------------------------
    # Pass 2: substitute pool-symbol references with content-derived
    # canonical tokens, then apply the numeric-const fallback.
    # ------------------------------------------------------------------
    {
      line = $0
      for (i = 0; i < n_syms; i++) {
        s       = syms[i]
        content = pool_content[s]
        canonical = "@_pool_[" content "]"
        # Build a regex pattern for this symbol; escape literal dots so
        # @str_lit.4 matches the four-char suffix, not any character.
        pat = "@" s
        gsub(/\./, "[.]", pat)
        gsub(pat, canonical, line)
      }
      # Numeric const pool: NAME discriminates identity; collapse pool-id.
      #
      # NOTE: the gsub replacement string is a LITERAL in POSIX/gawk/mawk;
      # it does NOT expand \1 backreferences the way sed/perl do, so a naive
      # `gsub(/.../, "@__hew_const__\\1__N", line)` here substitutes the
      # literal text "@__hew_const__\1__N" for every match, erasing the NAME
      # and making @__hew_const__FOO__1 and @__hew_const__BAR__99 collapse to
      # the SAME canonical token — a false-negative that hides a numeric
      # constant identity change. gensub() would expand the backreference,
      # but it is a gawk-only extension, absent from the default
      # one-true-awk-derived /usr/bin/awk shipped on macOS, so reconstruct
      # the substitution manually with the same portable match()/substr()
      # idiom already used above in this file, preserving the captured
      # NAME segment verbatim.
      out = ""
      rest = line
      while (match(rest, /@__hew_const__[A-Za-z0-9_]+__[0-9]+/)) {
        pre  = substr(rest, 1, RSTART - 1)
        m    = substr(rest, RSTART, RLENGTH)
        name = m
        sub(/^@__hew_const__/, "", name)
        sub(/__[0-9]+$/, "", name)
        out  = out pre "@__hew_const__" name "__N"
        rest = substr(rest, RSTART + RLENGTH)
      }
      line = out rest
      print line
    }
  ' "$file" "$file"
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
  norm "$1" | awk '
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
