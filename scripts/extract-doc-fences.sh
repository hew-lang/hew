#!/usr/bin/env bash
# extract-doc-fences.sh — Extract and typecheck ```hew fenced blocks from docs/.
#
# Walks docs/hew-language-guide.md and docs/specs/HEW-SPEC-2026.md, emits each
# ```hew fence to .tmp/doc-fences/<source>-<n>.hew, then runs `hew check` on
# each one and applies the ratchet against scripts/doc-test-expected-failures.txt.
#
# Skip rules (fail-closed default — a fence is checked unless explicitly skipped):
#   1. A ```hew fence immediately preceded (within 5 lines) by a "Not yet
#      implemented" callout or a "<!-- doctest: skip -->" HTML comment is
#      SKIP — the fence describes aspirational/NYI behaviour.
#      Per feedback_docs_may_be_aspirational: spec-ahead-of-impl is not
#      drift when a real plan exists; skip-classified fences count toward
#      the skip total and are never treated as failures.
#
# Ratchet rules (mirrors hew-suite-ratchet.sh):
#   - Exits 0 if the failing fence set exactly matches the expected-failures list.
#   - Exits 1 on any NEW failure (unexpected regression).
#   - Exits 1 if a LISTED failure now passes (ratchet forward — remove from list).
#   - Exits 1 if a LISTED failure's content checksum changed (re-verify label).
#
# WHY: Docs can rot silently with no gate. 133 fences currently pass; 111 fail
# (the docs-rot backlog, tracked per entry with root-cause notes). This ratchet
# is the standing gate: new failures are always visible; fixed fences require a
# list update confirming the regression is gone.
#
# WHEN OBSOLETE: When the expected-failures list is empty. Then remove the
# ratchet call and wire make test-doc-examples directly to `hew check --all-docs`.
#
# REAL SOLUTION: Fix the underlying docs (tracked per entry with root-cause notes).
#
# Usage:
#   scripts/extract-doc-fences.sh [--help]
#   scripts/extract-doc-fences.sh [--expected-failures <path>]
#   scripts/extract-doc-fences.sh [--outdir <dir>]
#
# Options:
#   --expected-failures <path>  Override the default expected-failures file.
#                               Default: scripts/doc-test-expected-failures.txt
#   --outdir <dir>              Override .tmp/doc-fences/ as the scratch dir.
#                               The dir is created if absent; contents are
#                               replaced each run.
#   --hew-bin <path>            Override the hew binary.
#                               Default: target/debug/hew

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXPECTED_FAILURES_FILE="$REPO_ROOT/scripts/doc-test-expected-failures.txt"
OUTDIR="$REPO_ROOT/.tmp/doc-fences"
HEW_BIN="${HEW_BIN:-$REPO_ROOT/target/debug/hew}"

DOCS=(
    "docs/hew-language-guide.md:guide"
    "docs/specs/HEW-SPEC-2026.md:spec"
)

usage() {
    cat <<'EOF'
Usage: scripts/extract-doc-fences.sh [options]

Extract ```hew fences from docs/ and typecheck each via `hew check`.
Applies a ratchet against the expected-failures list so known-failing fences
do not block the gate, but new failures always do.

Options:
  --expected-failures <path>  Override default (scripts/doc-test-expected-failures.txt).
  --outdir <dir>              Override scratch directory (default: .tmp/doc-fences/).
  --hew-bin <path>            Override hew binary (default: target/debug/hew).
  --help                      Show this message.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --expected-failures)
            shift; [[ $# -gt 0 ]] || { echo "error: --expected-failures requires a path" >&2; exit 1; }
            EXPECTED_FAILURES_FILE="$1"; shift ;;
        --outdir)
            shift; [[ $# -gt 0 ]] || { echo "error: --outdir requires a path" >&2; exit 1; }
            OUTDIR="$1"; shift ;;
        --hew-bin)
            shift; [[ $# -gt 0 ]] || { echo "error: --hew-bin requires a path" >&2; exit 1; }
            HEW_BIN="$1"; shift ;;
        --help|-h) usage; exit 0 ;;
        *) echo "error: unknown argument: $1" >&2; usage >&2; exit 1 ;;
    esac
done

# ── Prerequisites ──────────────────────────────────────────────────────────────

if [[ ! -f "$HEW_BIN" ]]; then
    echo "error: hew binary not found at $HEW_BIN" >&2
    echo "       Run: cargo build -p hew-cli" >&2
    exit 1
fi

if [[ ! -f "$EXPECTED_FAILURES_FILE" ]]; then
    echo "error: expected-failures file not found: $EXPECTED_FAILURES_FILE" >&2
    exit 1
fi

mkdir -p "$OUTDIR"

# ── Skip-marker detection (bash implementation) ────────────────────────────────
# Read a doc file and extract fence metadata: for each ```hew block, record
# whether it is skip-marked (NYI callout or doctest: skip comment in preceding 5
# lines) and emit the content to $OUTDIR.

# NYI_MARKERS: substrings that, when found in the 5 lines before a ```hew fence,
# mark the fence as aspirational/NYI and cause it to be skipped.
NYI_PATTERNS=("Not yet implemented" "doctest: skip" "doctest:skip")

# ── Extraction pass ────────────────────────────────────────────────────────────

declare -a FENCE_IDS     # fence IDs in extraction order (e.g. guide-0001, spec-0042)
declare -a FENCE_SKIPPED # 1 if NYI/skip-annotated, 0 otherwise (parallel to FENCE_IDS)

extract_doc() {
    local filepath="$1"
    local prefix="$2"

    # Read the file into an array (one element per line, newline preserved).
    # mapfile requires bash 4; use a while loop for bash 3 compat (macOS ships 3.x).
    local lines=()
    while IFS= read -r line; do
        lines+=("$line")
    done < "$filepath"

    local total="${#lines[@]}"
    local fence_num=0
    local i=0

    while (( i < total )); do
        local line="${lines[$i]}"

        # Detect opening fence: line is exactly "```hew" (possibly with trailing CR).
        local stripped="${line%%$'\r'}"
        if [[ "$stripped" != '```hew' ]]; then
            (( i += 1 ))
            continue
        fi

        fence_num=$(( fence_num + 1 ))
        local fence_id
        printf -v fence_id "%s-%04d" "$prefix" "$fence_num"

        # Check preceding 5 lines for a skip/NYI marker.
        local skip=0
        local j
        for (( j = i > 5 ? i - 5 : 0; j < i; j++ )); do
            local ctx_line="${lines[$j]}"
            for marker in "${NYI_PATTERNS[@]}"; do
                if [[ "$ctx_line" == *"$marker"* ]]; then
                    skip=1
                    break 2
                fi
            done
        done

        # Collect fence content lines (up to the closing ```).
        (( i += 1 ))
        local content=""
        while (( i < total )); do
            local fline="${lines[$i]}"
            local fstripped="${fline%%$'\r'}"
            if [[ "$fstripped" == '```' ]]; then
                (( i += 1 ))
                break
            fi
            content="${content}${fline}"$'\n'
            (( i += 1 ))
        done

        # Emit to outdir.
        local outfile="$OUTDIR/${fence_id}.hew"
        printf '%s' "$content" > "$outfile"

        FENCE_IDS+=("$fence_id")
        FENCE_SKIPPED+=("$skip")
    done
}

echo "==> Doc-test harness: extracting hew fences from docs/"
for entry in "${DOCS[@]}"; do
    doc_path="${entry%%:*}"
    prefix="${entry##*:}"
    full_path="$REPO_ROOT/$doc_path"
    if [[ ! -f "$full_path" ]]; then
        echo "warning: doc file not found: $full_path (skipping)" >&2
        continue
    fi
    echo "  Scanning: $doc_path"
    extract_doc "$full_path" "$prefix"
done

total_fences="${#FENCE_IDS[@]}"
echo "  Extracted: $total_fences fences total"

# ── Read expected-failures list ────────────────────────────────────────────────
EXPECTED_STR=""
EXPECTED_CKSUM_STR=""
while IFS= read -r line; do
    fields="${line%%#*}"                 # strip comment
    fields="${fields#"${fields%%[! ]*}"}" # ltrim
    fields="${fields%"${fields##*[! ]}"}" # rtrim
    [[ -z "$fields" ]] && continue
    set -- $fields
    if [[ $# -ne 2 ]]; then
        echo "error: expected-failures entry must be: <fence-id> <cksum>" >&2
        echo "       bad entry: $line" >&2
        exit 1
    fi
    name="$1"
    recorded_cksum="$2"
    [[ -z "$name" ]] && continue
    if [[ ! "$recorded_cksum" =~ ^[0-9]+$ ]]; then
        echo "error: expected-failures checksum must be decimal cksum output" >&2
        echo "       bad entry: $line" >&2
        exit 1
    fi
    EXPECTED_STR="${EXPECTED_STR}${name}"$'\n'
    EXPECTED_CKSUM_STR="${EXPECTED_CKSUM_STR}${name} ${recorded_cksum}"$'\n'
done < "$EXPECTED_FAILURES_FILE"

# ── Typecheck pass ─────────────────────────────────────────────────────────────

pass=0
fail=0
skip=0
ACTUAL_STR=""  # newline-separated fence IDs of failing fences

for (( idx=0; idx < ${#FENCE_IDS[@]}; idx++ )); do
    fence_id="${FENCE_IDS[$idx]}"
    is_skip="${FENCE_SKIPPED[$idx]}"
    outfile="$OUTDIR/${fence_id}.hew"

    if [[ "$is_skip" == "1" ]]; then
        skip=$(( skip + 1 ))
        continue
    fi

    check_rc=0
    "$HEW_BIN" check "$outfile" >/dev/null 2>&1 || check_rc=$?

    if [[ "$check_rc" == "0" ]]; then
        pass=$(( pass + 1 ))
    else
        fail=$(( fail + 1 ))
        ACTUAL_STR="${ACTUAL_STR}${fence_id}"$'\n'
    fi
done

echo ""
echo "==> Results: $pass passed, $fail failed, $skip skipped (NYI/aspirational)"
echo "    Total fences: $total_fences"

# ── Ratchet ────────────────────────────────────────────────────────────────────

count_expected=0
[[ -n "$EXPECTED_STR" ]] && count_expected="$(printf '%s' "$EXPECTED_STR" | grep -c .)"

count_actual=0
[[ -n "$ACTUAL_STR" ]] && count_actual="$(printf '%s' "$ACTUAL_STR" | grep -c .)"

echo ""
echo "==> Doc-test ratchet"
echo "    Expected failures: $count_expected"
echo "    Actual failures:   $count_actual"

# Unexpected failures: in actual but not in expected.
unexpected_failures=""
while IFS= read -r name; do
    [[ -z "$name" ]] && continue
    if ! printf '%s\n' "$EXPECTED_STR" | grep -qxF "$name"; then
        unexpected_failures="${unexpected_failures}${name}"$'\n'
    fi
done <<< "$ACTUAL_STR"

# Unexpected passes: in expected but not in actual.
unexpected_passes=""
while IFS= read -r name; do
    [[ -z "$name" ]] && continue
    if ! printf '%s\n' "$ACTUAL_STR" | grep -qxF "$name"; then
        unexpected_passes="${unexpected_passes}${name}"$'\n'
    fi
done <<< "$EXPECTED_STR"

# Stale metadata: content changed under a listed ID, so the root-cause label
# must be re-verified instead of trusted by position alone.
stale_metadata=""
while IFS= read -r entry; do
    [[ -z "$entry" ]] && continue
    set -- $entry
    name="$1"
    recorded_cksum="$2"
    outfile="$OUTDIR/${name}.hew"
    [[ -f "$outfile" ]] || continue
    actual_cksum="$(cksum "$outfile" | awk '{print $1}')"
    if [[ "$recorded_cksum" != "$actual_cksum" ]]; then
        stale_metadata="${stale_metadata}${name} ${recorded_cksum} ${actual_cksum}"$'\n'
    fi
done <<< "$EXPECTED_CKSUM_STR"

count_unexpected_fail=0
[[ -n "$unexpected_failures" ]] && count_unexpected_fail="$(printf '%s' "$unexpected_failures" | grep -c .)"

count_unexpected_pass=0
[[ -n "$unexpected_passes" ]] && count_unexpected_pass="$(printf '%s' "$unexpected_passes" | grep -c .)"

count_stale_metadata=0
[[ -n "$stale_metadata" ]] && count_stale_metadata="$(printf '%s' "$stale_metadata" | grep -c .)"

if [[ $count_unexpected_fail -eq 0 && $count_unexpected_pass -eq 0 && $count_stale_metadata -eq 0 ]]; then
    if [[ $count_actual -eq 0 ]]; then
        echo ""
        echo "    All doc fences pass. Consider removing the expected-failures file."
    else
        echo "    Expected failure set matches. Tracked failures: $count_actual"
    fi
    echo ""
    echo "==> Doc-test ratchet: PASSED"
    exit 0
fi

echo ""
if [[ $count_unexpected_fail -gt 0 ]]; then
    echo "RATCHET FAIL: $count_unexpected_fail UNEXPECTED failure(s) — not in expected list:"
    while IFS= read -r name; do
        [[ -z "$name" ]] && continue
        # Print the first error line for diagnosis.
        # hew check is expected to fail here; capture without letting the
        # non-zero exit abort the script under set -e / pipefail.
        outfile="$OUTDIR/${name}.hew"
        first_err="$("$HEW_BIN" check "$outfile" 2>&1 || true)"
        first_err="${first_err%%$'\n'*}"
        echo "  UNEXPECTED: $name  ($first_err)"
    done <<< "$unexpected_failures"
    echo ""
    echo "  A doc fence that previously passed now fails — this is a documentation"
    echo "  regression.  Fix the fence in the doc file, OR if the failure is"
    echo "  intentional (e.g. the surface is now NYI), add a '<!-- doctest: skip -->'"
    echo "  comment before the fence and remove it from the expected-failures list."
    echo "  To accept as a known failure (discouraged): add to $EXPECTED_FAILURES_FILE"
    echo ""
fi

if [[ $count_unexpected_pass -gt 0 ]]; then
    echo "RATCHET FAIL: $count_unexpected_pass listed failure(s) now PASS — remove from list:"
    while IFS= read -r name; do
        [[ -z "$name" ]] && continue
        echo "  NOW-PASSES: $name"
    done <<< "$unexpected_passes"
    echo ""
    echo "  Delete these lines from: $EXPECTED_FAILURES_FILE"
    echo "  (Do not restore a failing entry to keep this green — fix the docs.)"
    echo ""
fi

if [[ $count_stale_metadata -gt 0 ]]; then
    echo "RATCHET FAIL: $count_stale_metadata stale expected-failure metadata entr$( [[ $count_stale_metadata -eq 1 ]] && echo "y" || echo "ies" ):"
    while IFS= read -r entry; do
        [[ -z "$entry" ]] && continue
        set -- $entry
        name="$1"
        recorded_cksum="$2"
        actual_cksum="$3"
        echo "  STALE METADATA: $name content changed since label was written (recorded=$recorded_cksum actual=$actual_cksum) — re-verify and update the label"
    done <<< "$stale_metadata"
    echo ""
fi

echo "==> Doc-test ratchet: FAILED"
exit 1
