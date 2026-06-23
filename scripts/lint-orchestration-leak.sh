#!/usr/bin/env bash
# lint-orchestration-leak.sh — scan tracked source for orchestration-token leaks.
#
# Catches tokens that belong only in orchestration context (lane IDs, Q-tags,
# .tmp/ path references, fleet terms) when they appear in committed source code.
# Exits 1 with a diagnostic on any hit; exits 0 when the tree is clean.
#
# Background: four consecutive lanes leaked orchestration identifiers into
# committed source — PCA-8 (a PCA tracking ID), L7 (a lane ID), G8 (a lane ID),
# q185 and Qa (orchestration Q-tags in user-facing diagnostic strings). Each was
# caught only at review, causing revision cycles. This gate runs pre-push so
# those leaks are caught locally before they reach a reviewer.
#
# Token set — calibrated to 0 false positives on origin/main at the time this
# gate was added:
#
#   T1  PCA-<digits>        — PCA tracking IDs (e.g. PCA-8, PCA-12)
#   T2  wire-fleet          — fleet-orchestration term
#   T3  wire-l<digit>       — lane naming scheme (wire-l1, wire-l2, ...)
#   T4  L7 / L8 / L9        — high-numbered L-lane IDs (L1–L6 appear
#                             legitimately as layer labels in docs/diagrams.md
#                             and as hardware-cache tier references in .rs)
#   T5  q<NNN> (lowercase,  — 3+-digit Q-tags in .rs and .md files only
#              3+ digits)     (.hew files use q### to name feature programs
#                             intentionally, e.g. q175 in vertical-slice)
#   T6  Q<a-z> (short)      — short Q-tags like Qa (uppercase Q + lowercase
#                             letter, distinct from Q320-style design refs)
#   T7  .tmp/(orchestration — orchestration path references inside string
#         |plans|worktrees)   literals (comments that cite plan docs are
#         in string literals  accepted; string literals are not)
#
# Accepted convention (not flagged):
#   W### / R### work-item tags   — long-standing per-commit convention
#   G<1-9>, G<12> etc.           — issue/group-class references committed
#                                  intentionally (G1 float class, G7 ratchet
#                                  teeth, G12 value-class authority, …)
#   L1–L6 in architecture docs   — layer labels (L3 Actors, L4 Async …)
#   q### in .hew test fixtures   — feature-naming convention (q175 program)
#   .tmp/<path> in // comments   — plan citations (doc links, not leaks)
#
# Usage:
#   scripts/lint-orchestration-leak.sh          # normal scan
#   scripts/lint-orchestration-leak.sh --self-test   # verify every pattern fires

set -Eeuo pipefail

_SELF=$(cd "$(dirname "$0")" && pwd)/$(basename "$0")

# Determine the repo root.  When the script is called from within a git working
# tree (normal use and self-test), prefer the CWD-relative root so that a
# self-test running from a temp repo finds that repo instead of the real one.
# Fall back to the script's own directory for the edge case where the caller
# CWD is outside any git tree.
if REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)"; then
    : # resolved via CWD — correct for both normal use and self-test
else
    REPO_ROOT="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
fi
cd "$REPO_ROOT"

# ── Self-test ────────────────────────────────────────────────────────────────
if [[ "${1-}" == "--self-test" ]]; then
    _tmpdir=$(mktemp -d)
    trap 'rm -rf "$_tmpdir"' EXIT

    _pass=0
    _fail=0

    # Helpers: scaffold a minimal fake git repo in _tmpdir so git grep works.
    init_repo() {
        git -C "$_tmpdir" init -q
        git -C "$_tmpdir" config user.email "test@test"
        git -C "$_tmpdir" config user.name "test"
        git -C "$_tmpdir" commit --allow-empty -m "root"
    }

    # Write a file, `git add` it, then run the lint from _tmpdir.
    # Expects the lint to exit NON-ZERO (detects the leak).
    assert_detects() {
        local desc="$1"
        local rel_path="$2"
        local content="$3"
        mkdir -p "$_tmpdir/$(dirname "$rel_path")"
        printf '%s\n' "$content" > "$_tmpdir/$rel_path"
        git -C "$_tmpdir" add "$rel_path" 2>/dev/null
        if (cd "$_tmpdir" && bash "$_SELF" 2>/dev/null); then
            echo "FAIL: $desc — expected non-zero exit but got zero"
            _fail=$((_fail + 1))
        else
            echo "PASS: $desc"
            _pass=$((_pass + 1))
        fi
        git -C "$_tmpdir" rm -f "$rel_path" 2>/dev/null
    }

    # Expects the lint to exit ZERO (no false positive).
    assert_clean() {
        local desc="$1"
        local rel_path="$2"
        local content="$3"
        mkdir -p "$_tmpdir/$(dirname "$rel_path")"
        printf '%s\n' "$content" > "$_tmpdir/$rel_path"
        git -C "$_tmpdir" add "$rel_path" 2>/dev/null
        if (cd "$_tmpdir" && bash "$_SELF" 2>/dev/null); then
            echo "PASS: $desc"
            _pass=$((_pass + 1))
        else
            echo "FAIL: $desc — expected zero exit (clean) but got non-zero"
            _fail=$((_fail + 1))
        fi
        git -C "$_tmpdir" rm -f "$rel_path" 2>/dev/null
    }

    init_repo

    # T1 — PCA tracking IDs
    assert_detects "T1 PCA-8 in Rust comment"  "src/lib.rs"  '// PCA-8 fix'
    assert_detects "T1 PCA-12 in string"        "src/lib.rs"  'let r = "PCA-12";'
    assert_detects "T1 PCA-1 in .hew comment"  "std/x.hew"   '// see PCA-1'
    assert_detects "T1 PCA-99 in .md"          "docs/x.md"   '## PCA-99 notes'

    # T2 — wire-fleet
    assert_detects "T2 wire-fleet in Rust"     "src/lib.rs"  '// wire-fleet dispatch'
    assert_detects "T2 wire-fleet in string"   "src/lib.rs"  'let s = "wire-fleet";'

    # T3 — wire-l<digit>
    assert_detects "T3 wire-l1 in Rust"       "src/lib.rs"  '// from wire-l1'
    assert_detects "T3 wire-l2 in string"     "src/lib.rs"  'let l = "wire-l2";'
    assert_detects "T3 wire-l9 in .md"        "docs/x.md"   'See wire-l9 plan.'

    # T4 — L7/L8/L9
    assert_detects "T4 L7 in Rust comment"    "src/lib.rs"  '// lane L7 scope'
    assert_detects "T4 L8 in string literal"  "src/lib.rs"  'let s = "lane L8";'
    assert_detects "T4 L9 in .hew comment"   "std/x.hew"   '// L9 phase'

    # T5 — lowercase q-tags in .rs
    assert_detects "T5 q185 in Rust comment"  "src/lib.rs"  '// q185 errors-as-values'
    assert_detects "T5 q185 in string"        "src/lib.rs"  'let s = "q185";'
    assert_detects "T5 q023 in Rust"         "src/lib.rs"  '// per q023'

    # T6 — short Q-tags (Qa) in .rs
    assert_detects "T6 Qa in Rust string"     "src/lib.rs"  'let s = "Qa token";'
    assert_detects "T6 Qb in Rust comment"    "src/lib.rs"  '// Qb was the root cause'

    # T7 — .tmp/ orchestration paths in string literals (.rs)
    assert_detects "T7 .tmp/orchestration in string" "src/lib.rs" \
        'let p = ".tmp/orchestration/state.db";'
    assert_detects "T7 .tmp/plans in string"         "src/lib.rs" \
        'let p = ".tmp/plans/lane.md";'
    assert_detects "T7 .tmp/worktrees in string"     "src/lib.rs" \
        'let p = ".tmp/worktrees/foo";'

    # ── Negative controls (must NOT fire) ────────────────────────────────────

    # Accepted: W### / R### work-item tags
    assert_clean "NEG W123 tag"      "src/lib.rs"  '// W123 backfill'
    assert_clean "NEG R58 tag"       "src/lib.rs"  '// R58 Q136 Option B'

    # Accepted: G-class labels (G1 float class, G7 ratchet, G12 value class)
    assert_clean "NEG G1 in comment" "src/lib.rs"  '// G1 float class'
    assert_clean "NEG G7 in comment" "src/lib.rs"  '// G7 teeth #1'
    assert_clean "NEG G12 in comment" "src/lib.rs" '// G12 owned-aggregate'

    # Accepted: L1–L6 layer labels / hardware tiers
    assert_clean "NEG L1 timer"     "src/lib.rs"   '"cancelled L1 timer must not fire"'
    assert_clean "NEG L4 phase"     "src/mir.rs"   '// L4 phase 2 deadline'
    assert_clean "NEG L5 layer"     "docs/diag.md" 'block:L5["L5: Networking"]'

    # Accepted: q### in .hew files (feature naming convention)
    assert_clean "NEG q175 in .hew" "tests/vs/x.hew" '//! The q175 program'

    # Accepted: .tmp/compile-out (non-orchestration path)
    assert_clean "NEG .tmp/compile-out" "src/main.rs" \
        'let p = ".tmp/compile-out/a.out";'

    # Accepted: .tmp/ in comment (plan citation)
    assert_clean "NEG .tmp/plans in comment" "src/lib.rs" \
        '// see .tmp/plans/g1-generics-monomorphization.md'
    # The backticks below are literal Markdown code spans, not command substitution.
    # shellcheck disable=SC2016
    assert_clean "NEG .tmp/orchestration in doc" "src/lib.rs" \
        '/// `.tmp/orchestration/dispatch-invariants.md`'

    echo ""
    echo "lint-orchestration-leak self-test: ${_pass} passed, ${_fail} failed"
    [[ "$_fail" -eq 0 ]]
    exit $?
fi
# ── End self-test ─────────────────────────────────────────────────────────────

# ── Main scan ─────────────────────────────────────────────────────────────────
hits=0
fail_lines=""

# Record a hit: accumulate for a single exit-1 report.
record_hit() {
    local label="$1"
    local match="$2"
    if [[ -n "$match" ]]; then
        hits=$((hits + 1))
        fail_lines="${fail_lines}${label}:
${match}
"
    fi
}

# T1 — PCA-<digits> anywhere in .rs / .hew / .md tracked source
record_hit "T1 PCA-<digits>" "$(
    git grep -nP 'PCA-[0-9]+' -- \
        '*.rs' '*.hew' '*.md' \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T2 — wire-fleet anywhere in .rs / .hew / .md
record_hit "T2 wire-fleet" "$(
    git grep -nP '\bwire-fleet\b' -- \
        '*.rs' '*.hew' '*.md' \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T3 — wire-l<digit> lane names in .rs / .hew / .md
record_hit "T3 wire-l<N>" "$(
    git grep -nP '\bwire-l[0-9]+\b' -- \
        '*.rs' '*.hew' '*.md' \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T4 — L7 / L8 / L9 in .rs / .hew / .md  (L1–L6 excluded: layer labels + cache refs)
record_hit "T4 L[7-9] lane ID" "$(
    git grep -nP '\bL[7-9]\b' -- \
        '*.rs' '*.hew' '*.md' \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T5 — lowercase q-tags (3+ digits) in .rs and .md only
# (.hew excluded: feature-naming convention q175, q004 etc.)
record_hit "T5 q<NNN> Q-tag" "$(
    git grep -nP '\bq[0-9]{3,}\b' -- \
        '*.rs' '*.md' \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T6 — short Q-tags (uppercase Q + single lowercase letter: Qa, Qb, …)
# in .rs files only (short identifiers are uncommon; verified 0 in clean tree)
record_hit "T6 Q<a-z> short Q-tag" "$(
    git grep -nP '\bQ[a-z]\b' -- \
        '*.rs' \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T7 — .tmp/(orchestration|plans|worktrees) INSIDE STRING LITERALS in .rs / .hew
# (comment-based plan citations — /// `.tmp/plans/…` — are accepted; string literals
# are not, because they compile into binary or user-facing output)
# Excludes examples/hew-orch/ which is an orchestration-helper tool.
record_hit "T7 .tmp/orch path in string literal" "$(
    git grep -nP '"[^"]*\.tmp/(orchestration|plans|worktrees)' -- \
        '*.rs' '*.hew' \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!examples/hew-orch/' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# ── Report ────────────────────────────────────────────────────────────────────
if (( hits > 0 )); then
    echo "lint-orchestration-leak: ${hits} pattern group(s) flagged" >&2
    echo "" >&2
    echo "$fail_lines" >&2
    echo "Orchestration tokens (lane IDs, Q-tags, .tmp/ paths) must not appear in" >&2
    echo "committed source. Remove them or replace with a plain description." >&2
    echo "" >&2
    echo "Accepted: W###/R### work-item tags, G-class labels (G1/G7/G12 …)," >&2
    echo "          L1–L6 layer labels, q### in .hew feature programs," >&2
    echo "          .tmp/ references in // comments (plan citations)." >&2
    exit 1
fi

echo "lint-orchestration-leak: ok" >&2
