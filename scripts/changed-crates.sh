#!/usr/bin/env bash
# scripts/changed-crates.sh — map git diff → changed crate names
#
# Prints a space-separated list of Cargo package names whose source files
# appear in the current diff (committed, staged, unstaged, or untracked vs
# the merge base on the default branch).
#
# Used by `make test-fast` to scope `cargo nextest run -E 'rdeps(<crates>)'`
# to only the crates touched by the current branch.  When no crates are
# detected the script prints nothing, and `make test-fast` falls back to a
# full workspace run.
#
# WHY: replaces the hand-maintained `-p` crate list in the dispatcher with
# a git-derived scope; catches missed downstream crates automatically.
# WHEN obsolete: if Cargo gains a built-in affected-crate oracle.
# WHAT the real solution looks like: `cargo-difftests` with per-hunk
# coverage instrumentation — deferred until build-cost is justified.
#
# Usage:
#   scripts/changed-crates.sh               # diff vs origin/HEAD merge base
#   scripts/changed-crates.sh --base <ref>  # explicit base ref
#   CRATE=hew-types scripts/changed-crates.sh  # pin output to one crate (passthrough)
#
# Output format: space-separated package names, one line, or empty string.
# Exit 0 always.
#
# Requires: bash 3.2+ (macOS system bash compatible), git.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# ── CRATE= passthrough ────────────────────────────────────────────────────────
# If the caller has already narrowed scope via CRATE=, honour it directly.
if [[ -n "${CRATE:-}" ]]; then
    printf '%s' "$CRATE"
    exit 0
fi

# ── Base ref resolution ───────────────────────────────────────────────────────
BASE_REF=""
if [[ "${1:-}" == "--base" && -n "${2:-}" ]]; then
    BASE_REF="$2"
    shift 2
fi

if [[ -z "$BASE_REF" ]]; then
    # Try to find the common ancestor with the default remote branch.
    for candidate in origin/main origin/master; do
        if git -C "$REPO_ROOT" rev-parse --verify "$candidate" >/dev/null 2>&1; then
            BASE_REF="$(git -C "$REPO_ROOT" merge-base HEAD "$candidate" 2>/dev/null || true)"
            break
        fi
    done
fi

# ── Collect changed paths ─────────────────────────────────────────────────────
# Accumulate paths as a newline-delimited string; sort -u to deduplicate.
ALL_PATHS=""

append_paths() {
    local out
    out="$(eval "$1" 2>/dev/null || true)"
    if [[ -n "$out" ]]; then
        ALL_PATHS="${ALL_PATHS}${out}"$'\n'
    fi
}

if [[ -n "$BASE_REF" ]]; then
    # Paths changed since the merge base (the branch's own delta).
    append_paths "git -C '$REPO_ROOT' diff --name-only '$BASE_REF' HEAD"
fi

# Staged and unstaged changes on top of HEAD.
append_paths "git -C '$REPO_ROOT' diff --name-only HEAD"
# Untracked files (new source files not yet committed).
append_paths "git -C '$REPO_ROOT' ls-files --others --exclude-standard"

if [[ -z "$ALL_PATHS" ]]; then
    # No changed files detected — caller falls back to full workspace.
    exit 0
fi

# Deduplicated sorted paths.
UNIQUE_FILES="$(printf '%s' "$ALL_PATHS" | sort -u | grep -v '^$' || true)"

if [[ -z "$UNIQUE_FILES" ]]; then
    exit 0
fi

# ── Path → crate-name mapping ─────────────────────────────────────────────────
# Mirror the dispatcher's is_*_path() case patterns but output the Cargo
# package name for use in nextest's -E 'rdeps(...)' expressions.
# Each crate directory prefix maps to its Cargo package name (they match
# the directory name for this workspace).
#
# Returns: space-separated crate names, or the sentinel "__workspace__"
# when a workspace-wide file is detected (signals fallback to full run).

WORKSPACE_WIDE=0
# Use a plain string accumulator (bash 3.2 has no assoc arrays).
CRATES_FOUND=""

add_crate() {
    local c="$1"
    # Append only if not already present (substring check).
    case " $CRATES_FOUND " in
        *" $c "*) ;;  # already present
        *) CRATES_FOUND="${CRATES_FOUND:+$CRATES_FOUND }$c" ;;
    esac
}

while IFS= read -r f; do
    [[ -z "$f" ]] && continue
    case "$f" in
        hew-parser/*|hew-lexer/*)
            add_crate "hew-parser"; add_crate "hew-lexer" ;;
        hew-types/*)
            add_crate "hew-types" ;;
        hew-hir/*)
            add_crate "hew-hir" ;;
        hew-mir/*)
            add_crate "hew-mir" ;;
        hew-codegen-rs/*)
            add_crate "hew-codegen-rs" ;;
        hew-compile/*)
            add_crate "hew-compile" ;;
        hew-cabi/*)
            add_crate "hew-cabi" ;;
        hew-capability-gen/*)
            add_crate "hew-capability-gen" ;;
        hew-cli/*)
            add_crate "hew-cli" ;;
        adze-cli/*)
            add_crate "adze-cli" ;;
        hew-runtime/*)
            add_crate "hew-runtime" ;;
        hew-runtime-testkit/*)
            add_crate "hew-runtime-testkit" ;;
        hew-observe/*)
            add_crate "hew-observe" ;;
        hew-analysis/*)
            add_crate "hew-analysis" ;;
        hew-lib/*)
            add_crate "hew-lib" ;;
        hew-lsp/*)
            add_crate "hew-lsp" ;;
        hew-testutil/*)
            add_crate "hew-testutil" ;;
        hew-std/*)
            add_crate "hew-std" ;;
        hew-wasm/*)
            add_crate "hew-wasm" ;;
        hew-sandbox-vm/*)
            add_crate "hew-sandbox-vm" ;;
        hew-sandbox-wasm/*)
            add_crate "hew-sandbox-wasm" ;;
        # Shared source or config that could affect all crates — treat as workspace-wide.
        Cargo.toml|Cargo.lock|.config/*|.cargo/*|Makefile)
            WORKSPACE_WIDE=1 ;;
        scripts/*)
            # Script changes don't affect Rust tests — skip silently.
            ;;
        # Hew stdlib sources — exercised by hew-cli and hew-codegen-rs.
        std/*)
            add_crate "hew-cli"; add_crate "hew-codegen-rs" ;;
        tests/hew/*)
            add_crate "hew-cli"; add_crate "hew-codegen-rs" ;;
        # Other test fixtures — exercised by hew-cli.
        tests/*)
            add_crate "hew-cli" ;;
        # Anything else is treated as workspace-wide.
        *)
            WORKSPACE_WIDE=1 ;;
    esac
done <<< "$UNIQUE_FILES"

# If a workspace-wide file changed, signal fallback by printing nothing.
if [[ "$WORKSPACE_WIDE" -eq 1 ]]; then
    exit 0
fi

if [[ -z "$CRATES_FOUND" ]]; then
    exit 0
fi

printf '%s' "$CRATES_FOUND"
