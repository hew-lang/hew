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
#   T8  orchestration vocab — framing phrases in *.rs/*.hew source only:
#         cross-eco(system)   — "cross-eco review/finding", "cross-ecosystem
#                               reviewers" — GPT/independent-review references
#         this lane           — "this lane fixes/closes/removes/adds" framing
#         lane plan           — "see lane plan §X" plan-document citations
#         companion lane      — "companion lane adds" framing
#         GPT                 — GPT reviewer mentions
#         later-wave/waves    — "later-wave containers", "in later waves"
#         lane's              — possessive "the lane's deliverable/invariant"
#       Not flagged: "express lane", "SIMD lanes", "per-actor-pair lanes",
#       "compilation phase", "wave of lints", Lane-A/Lane-B failure-class
#       labels, QUIC "lane-kind" wire encoding.  Excludes examples/hew-orch/
#       (orchestration-helper tool uses these terms intentionally).
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
#   scripts/lint-orchestration-leak.sh               # scan tracked source files
#   scripts/lint-orchestration-leak.sh --scan-commits # scan commit-message bodies
#   scripts/lint-orchestration-leak.sh --self-test    # verify every pattern fires
#
# Exclusion how-to: to permit a legitimate future L7/L8/L9 symbol (T4 sits one
# digit above the live L1–L6 layer-label convention), add a ':!path/to/file'
# exclusion to the T4 git grep call below — same ':!pattern' syntax used for
# ':!LESSONS.md' and ':!tests/leak-scan/'.
#
# Commit-message opt-out: to allow a specific pattern in one commit, add
# 'Leak-scan-allow: T<N> <reason>' (one line per pattern suppressed).
# Example: 'Leak-scan-allow: T4 L7 is OSI layer-7, not a lane ID'
# Only the named T-number is suppressed; all other patterns still fire.
# A line without a valid T<N> prefix or without a non-empty reason is
# ignored — the default is fail-closed.  Opt-outs are greppable:
#   git log --all --grep='Leak-scan-allow:'

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
    # T7 path variants (backslash + escaped-slash forms)
    assert_detects "T7 .tmp backslash in string"     "src/lib.rs" \
        'let p = ".tmp\orchestration\state.db";'
    assert_detects "T7 .tmp escaped-slash in string" "src/lib.rs" \
        'let p = ".tmp\/plans\/lane.md";'

    # File-type coverage — tokens in non-.rs files must be caught
    assert_detects "T1 PCA-8 in YAML file"    ".github/workflows/x.yml" '# PCA-8 internal note'
    assert_detects "T2 wire-fleet in shell"    "scripts/x.sh"            'echo wire-fleet dispatch'
    assert_detects "T5 q185 in YAML step"      ".github/workflows/x.yml" 'run: echo q185 path'
    assert_detects "T6 Qa in TOML value"       "Cargo.toml.extra"        'shortcut = "Qa token"'

    # Spelling variants — T1 case/separator tolerance, T3 camelCase
    assert_detects "T1 PCA_12 underscore sep"  "src/lib.rs"  '// PCA_12 note'
    assert_detects "T1 pca-12 lowercase"       "src/lib.rs"  '// pca-12 fix'
    assert_detects "T3 wireL2 no-hyphen camel" "src/lib.rs"  '// from wireL2'

    # T8 — orchestration vocabulary in *.rs/*.hew source
    assert_detects "T8 cross-eco in Rust comment"      "src/lib.rs" \
        '// cross-eco review found the issue'
    assert_detects "T8 cross-ecosystem in .hew"        "std/x.hew" \
        '// cross-ecosystem review finding: module-qualified enum'
    assert_detects "T8 this lane in Rust comment"      "src/lib.rs" \
        '// before this lane the test always passed'
    assert_detects "T8 lane plan in doc string"        "src/lib.rs" \
        '/// see lane plan §3.1 for the full rationale'
    assert_detects "T8 companion lane in comment"      "src/lib.rs" \
        '// companion lane adds the explicit floor check'
    assert_detects "T8 GPT in Rust comment"            "src/lib.rs" \
        '// Both GPT reviewers flagged this ABI shape'
    assert_detects "T8 later-wave in comment"          "src/lib.rs" \
        '// the (later-wave) generic containers are built on this'
    assert_detects "T8 later waves in .hew"            "std/x.hew" \
        '//! in later waves this will be supported'
    assert_detects "T8 lane-s possessive in doc"       "src/lib.rs" \
        "/// the lane's deliverable — the shape that previously failed"

    # T8 — negative controls (must NOT fire)
    assert_clean "T8 NEG express lane"     "src/lib.rs" \
        '// express lane in the skip-list, O(log n)'
    assert_clean "T8 NEG compilation phase" "src/lib.rs" \
        '// emitted during the compilation phase'
    assert_clean "T8 NEG wave of lints"    "src/lib.rs" \
        '// would surface a wave of unrelated pub clippy lints'
    assert_clean "T8 NEG lanes plural"     "src/lib.rs" \
        '// per-actor-pair lanes used for QUIC stream dispatch'
    assert_clean "T8 NEG hew-orch excl"    "examples/hew-orch/x.rs" \
        '// this lane fixes the orchestration gap'

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

    # ── Commit-message scan (--scan-commits) ──────────────────────────────────
    # Helpers: make an empty commit with a given message, run --scan-commits with
    # an explicit HEAD~1..HEAD range (no origin/main in temp repo), then roll back.
    assert_commit_detects() {
        local desc="$1"
        local msg="$2"
        git -C "$_tmpdir" commit --allow-empty -m "$msg" 2>/dev/null
        if (cd "$_tmpdir" && bash "$_SELF" --scan-commits "HEAD~1..HEAD" 2>/dev/null); then
            echo "FAIL: $desc — expected non-zero exit but got zero"
            _fail=$((_fail + 1))
        else
            echo "PASS: $desc"
            _pass=$((_pass + 1))
        fi
        git -C "$_tmpdir" reset --hard HEAD~1 >/dev/null 2>&1
    }

    assert_commit_clean() {
        local desc="$1"
        local msg="$2"
        git -C "$_tmpdir" commit --allow-empty -m "$msg" 2>/dev/null
        if (cd "$_tmpdir" && bash "$_SELF" --scan-commits "HEAD~1..HEAD" 2>/dev/null); then
            echo "PASS: $desc"
            _pass=$((_pass + 1))
        else
            echo "FAIL: $desc — expected zero exit (clean) but got non-zero"
            _fail=$((_fail + 1))
        fi
        git -C "$_tmpdir" reset --hard HEAD~1 >/dev/null 2>&1
    }

    # T5 token in commit subject line
    assert_commit_detects "CMSG T5 q185 in commit subject"  "feat: add q185 errors-as-values path"
    # T4 token in commit subject line
    assert_commit_detects "CMSG T4 L8 lane ID in subject"   "chore: fix L8 regression in dispatcher"
    # Clean commit message — must not fire
    assert_commit_clean   "CMSG clean commit (no tokens)"    "feat: add robust error propagation"
    # Scoped opt-out: T4 with reason suppresses only T4 — clean
    assert_commit_clean   "CMSG scoped opt-out T4 clean"    "feat: add networking routing
Leak-scan-allow: T4 L7 is OSI layer-7 networking, not a lane ID"
    # Scoped opt-out: T4 allowed but unrelated T5 in same commit still fires
    assert_commit_detects "CMSG scoped opt-out T4 doesnt suppress T5" \
        "feat: add L7 routing with q185 path
Leak-scan-allow: T4 L7 is OSI layer-7 networking"
    # Scoped opt-out without reason is invalid — T4 still fires
    assert_commit_detects "CMSG scoped opt-out no-reason invalid" \
        "chore: fix L8 regression
Leak-scan-allow: T4"
    # Fail-closed: no origin/main + no explicit range → non-zero exit
    if (cd "$_tmpdir" && bash "$_SELF" --scan-commits 2>/dev/null); then
        echo "FAIL: CMSG no-range fail-closed — expected non-zero but got zero"
        _fail=$((_fail + 1))
    else
        echo "PASS: CMSG no-range fail-closed"
        _pass=$((_pass + 1))
    fi

    echo ""
    echo "lint-orchestration-leak self-test: ${_pass} passed, ${_fail} failed"
    [[ "$_fail" -eq 0 ]]
    exit $?
fi
# ── End self-test ─────────────────────────────────────────────────────────────

# ── Fail-closed PCRE probe ────────────────────────────────────────────────────
# git grep -P exits 1 on no match (normal) but exits 128 when git was built
# without PCRE support.  Under '|| true' those two outcomes are indistinguishable
# — the scan silently passes on a broken environment.  Probe once here (after
# self-test, before any pattern run) so both --scan-commits and the main scan
# fail closed rather than silently passing when PCRE is absent.
# Searches all indexed files (or nothing in an empty repo); the nonce appears
# in this script (tracked), so exit 0 = match found (PCRE ok); exit 1 = no
# match (PCRE ok); exit ≥ 2 = PCRE unavailable.
_probe_rc=0
git grep -qP '__PCRE_PROBE_NONCE__' 2>/dev/null || _probe_rc=$?
if [[ $_probe_rc -ne 0 && $_probe_rc -ne 1 ]]; then
    echo "lint-orchestration-leak: fatal — git grep -P (PCRE) is unavailable (exit ${_probe_rc})." >&2
    echo "  Install git with PCRE support: brew install git (macOS) or apt install git (Linux)." >&2
    exit 2
fi

# ── Commit-message scan ───────────────────────────────────────────────────────
# Scans the message bodies of commits in the push range for the same 7 token
# patterns.  Pre-push is the right point: commit messages are finalised there.
# Exits 1 with "sha: pattern: line" diagnostics on any hit; exits 0 when clean.
#
# Range: explicit arg if supplied, else origin/main..HEAD when origin/main is
# reachable.  If neither is available, exits 2 (fail-closed) — guessing a
# partial range is worse than surfacing the configuration gap.
# The pre-push hook passes an explicit range derived from git's stdin push info.
if [[ "${1-}" == "--scan-commits" ]]; then
    _explicit_range="${2-}"
    _commit_range=""
    if [[ -n "$_explicit_range" ]]; then
        _commit_range="$_explicit_range"
    elif git rev-parse --verify origin/main >/dev/null 2>&1; then
        _commit_range="origin/main..HEAD"
    else
        echo "lint-orchestration-leak: fatal — commit range unknown." >&2
        echo "  Run with an explicit range: --scan-commits <range>" >&2
        echo "  Or ensure origin/main is reachable (git fetch origin main)." >&2
        exit 2
    fi

    _commit_hits=0
    _commit_fail=""

    _check_commit_msg() {
        local _sha="$1" _label="$2" _pat="$3" _msg="$4" _allows="$5"
        # Per-pattern opt-out: 'Leak-scan-allow: T<N> <reason>' suppresses only T<N>.
        # Extract T-number from label (e.g. "T4" from "T4 L[7-9] lane ID").
        # grep -E is portable; no PCRE needed for the T-number check.
        local _tnum
        _tnum=$(printf '%s' "$_label" | sed 's/[[:space:]].*//')
        if [[ -n "$_allows" ]] && \
           printf '%s\n' "$_allows" | grep -qE "^${_tnum}[[:space:]]+[^[:space:]]"; then
            return 0  # this pattern is explicitly allowed in this commit
        fi
        local _m
        # Use perl -ne for PCRE on commit-message text: macOS system grep lacks -P,
        # but perl (always available) supports the same PCRE patterns as git grep -P.
        # Use m!...! delimiter to avoid conflicts with '/' in T7 path patterns and
        # '{' in T5 quantifiers.
        _m=$(printf '%s\n' "$_msg" | perl -ne "print \$. . ':' . \$_ if m!${_pat}!")
        if [[ -n "$_m" ]]; then
            _commit_hits=$((_commit_hits + 1))
            _commit_fail="${_commit_fail}${_sha}: ${_label}
${_m}
"
        fi
    }

    while IFS= read -r _sha; do
        [[ -z "$_sha" ]] && continue
        _full_msg=$(git log -1 --format='%B' "$_sha")
        # Parse 'Leak-scan-allow: T<N> <reason>' lines; strip the trailer prefix.
        # Only lines with a valid T<N> + non-empty reason suppress a specific pattern.
        _allows=$(printf '%s\n' "$_full_msg" | \
            perl -ne 'if (/^Leak-scan-allow:\s*(.*)/i) { print "$1\n" }')
        _check_commit_msg "$_sha" "T1 PCA-<digits>"       '(?i)PCA[-_]?[0-9]+'                       "$_full_msg" "$_allows"
        _check_commit_msg "$_sha" "T2 wire-fleet"          '\bwire-fleet\b'                            "$_full_msg" "$_allows"
        _check_commit_msg "$_sha" "T3 wire-l<N>"           '(?i)\bwire-?l[0-9]+\b'                    "$_full_msg" "$_allows"
        _check_commit_msg "$_sha" "T4 L[7-9] lane ID"      '\bL[7-9]\b'                               "$_full_msg" "$_allows"
        _check_commit_msg "$_sha" "T5 q<NNN> Q-tag"        '\bq[0-9]{3,}\b'                           "$_full_msg" "$_allows"
        _check_commit_msg "$_sha" "T6 Q<a-z> short Q-tag"  '\bQ[a-z]\b'                               "$_full_msg" "$_allows"
        _check_commit_msg "$_sha" "T7 .tmp/orch path"      '\.tmp[/\\]+(orchestration|plans|worktrees)' "$_full_msg" "$_allows"
    done < <(git log --format='%H' "$_commit_range" 2>/dev/null)

    if (( _commit_hits > 0 )); then
        echo "lint-orchestration-leak (commit messages): ${_commit_hits} pattern group(s) flagged" >&2
        echo "" >&2
        echo "$_commit_fail" >&2
        echo "Orchestration tokens must not appear in commit-message bodies." >&2
        echo "Amend with: git commit --amend  (or interactive rebase for older commits)." >&2
        exit 1
    fi
    echo "lint-orchestration-leak (commit messages): ok" >&2
    exit 0
fi
# ── End commit-message scan ───────────────────────────────────────────────────

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

# T1 — PCA-<digits> in all tracked text files (case/separator tolerant)
record_hit "T1 PCA-<digits>" "$(
    git grep -InP '(?i)PCA[-_]?[0-9]+' -- \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T2 — wire-fleet in all tracked text files
record_hit "T2 wire-fleet" "$(
    git grep -InP '\bwire-fleet\b' -- \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T3 — wire-l<digit> lane names in all tracked text files (case/hyphen tolerant)
record_hit "T3 wire-l<N>" "$(
    git grep -InP '(?i)\bwire-?l[0-9]+\b' -- \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T4 — L7 / L8 / L9 in all tracked text files (L1–L6 excluded: layer labels + cache refs)
record_hit "T4 L[7-9] lane ID" "$(
    git grep -InP '\bL[7-9]\b' -- \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T5 — lowercase q-tags (3+ digits) in all tracked text files except .hew
# (.hew excluded: feature-naming convention q175, q004 etc.)
record_hit "T5 q<NNN> Q-tag" "$(
    git grep -InP '\bq[0-9]{3,}\b' -- \
        ':!*.hew' \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T6 — short Q-tags (uppercase Q + single lowercase letter: Qa, Qb, …)
# in all tracked text files (broadened from .rs-only; FP risk low, opt-out available)
record_hit "T6 Q<a-z> short Q-tag" "$(
    git grep -InP '\bQ[a-z]\b' -- \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T7 — .tmp/(orchestration|plans|worktrees) INSIDE STRING LITERALS in all tracked
# text files.  Accepts forward-slash, backslash, and escaped-slash forms.
# Comment-based plan citations (// `.tmp/plans/…`) are not caught by this pattern.
# Split/constructed literals (concat!(".tmp/", "plans/…")) are out of scope.
# examples/hew-orch/ is excluded for T7 only: an orchestration-helper tool may
# legitimately hardcode a stable .tmp/orchestration/ path, but must never hardcode
# an ephemeral lane ID or Q-tag.  This asymmetry is intentional.
record_hit "T7 .tmp/orch path in string literal" "$(
    git grep -InP '"[^"]*\.tmp[/\\]+(orchestration|plans|worktrees)' -- \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!examples/hew-orch/' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# T8 — orchestration vocabulary phrases in *.rs and *.hew source files only.
# Targets framing that belongs only in orchestration context — GPT/independent
# review references, "this lane" plan-framing, "lane plan" citations, and
# possessive "lane's" — when they appear in source comments or test names.
# Scoped to *.rs/*.hew (not .md/.toml/.yml) to avoid flagging prose documents
# where these words have legitimate general meanings.
# Excludes examples/hew-orch/ — the orchestration-helper tool uses these
# phrases intentionally as its subject matter.
# NOT flagged: "express lane", "SIMD lanes", "per-actor-pair lanes" (QUIC),
# "compilation phase", "wave of lints", "Lane-A/Lane-B" failure-class labels.
record_hit "T8 orchestration vocab in source" "$(
    git grep -InP '(?i)\bcross[-\s]?eco|\bthis lane\b|\blane plan\b|\bcompanion lane\b|\bGPT\b|later[\s-]waves?\b|\blane\x27s\b' -- \
        '*.rs' '*.hew' \
        ':!examples/hew-orch/' \
        ':!scripts/lint-orchestration-leak.sh' \
        ':!LESSONS.md' \
        ':!tests/leak-scan/' \
        2>/dev/null || true
)"

# ── Report ────────────────────────────────────────────────────────────────────
if (( hits > 0 )); then
    echo "lint-orchestration-leak: ${hits} pattern group(s) flagged" >&2
    echo "" >&2
    echo "$fail_lines" >&2
    echo "Orchestration tokens (lane IDs, Q-tags, .tmp/ paths, vocab phrases) must" >&2
    echo "not appear in committed source. Remove them or replace with a plain description." >&2
    echo "" >&2
    echo "Accepted: W###/R### work-item tags, G-class labels (G1/G7/G12 …)," >&2
    echo "          L1–L6 layer labels, q### in .hew feature programs," >&2
    echo "          .tmp/ references in // comments (plan citations)," >&2
    echo "          'express lane', 'compilation phase', 'wave of lints'," >&2
    echo "          Lane-A/Lane-B failure-class labels, QUIC lane-kind terms." >&2
    exit 1
fi

echo "lint-orchestration-leak: ok" >&2
