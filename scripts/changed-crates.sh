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
declare -a CHANGED_FILES=()

collect_from_cmd() {
    local line
    while IFS= read -r line; do
        [[ -n "$line" ]] && CHANGED_FILES+=("$line")
    done < <(eval "$1" 2>/dev/null || true)
}

if [[ -n "$BASE_REF" ]]; then
    # Paths changed since the merge base (the branch's own delta).
    collect_from_cmd "git -C '$REPO_ROOT' diff --name-only '$BASE_REF' HEAD"
fi

# Staged and unstaged changes on top of HEAD.
collect_from_cmd "git -C '$REPO_ROOT' diff --name-only HEAD"
# Untracked files (new source files not yet committed).
collect_from_cmd "git -C '$REPO_ROOT' ls-files --others --exclude-standard '$REPO_ROOT'"

# Deduplicate.
declare -A SEEN_FILES=()
declare -a UNIQUE_FILES=()
for f in "${CHANGED_FILES[@]:-}"; do
    if [[ -z "${SEEN_FILES[$f]+set}" ]]; then
        SEEN_FILES[$f]=1
        UNIQUE_FILES+=("$f")
    fi
done

if [[ "${#UNIQUE_FILES[@]}" -eq 0 ]]; then
    # No changed files detected — caller falls back to full workspace.
    exit 0
fi

# ── Path → crate-name mapping ─────────────────────────────────────────────────
# Mirror the dispatcher's is_*_path() case patterns but output the Cargo
# package name for use in nextest's -E 'rdeps(...)' expressions.
# Each crate directory prefix maps to its Cargo package name (they match
# directory name except hew-wasm = "hew-wasm" and hew-std = "hew-std").

declare -A CRATES_FOUND=()

path_to_crate() {
    local p="$1"
    case "$p" in
        hew-parser/*|hew-lexer/*)               echo "hew-parser hew-lexer" ;;
        hew-types/*)                             echo "hew-types" ;;
        hew-hir/*)                               echo "hew-hir" ;;
        hew-mir/*)                               echo "hew-mir" ;;
        hew-codegen-rs/*)                        echo "hew-codegen-rs" ;;
        hew-compile/*)                           echo "hew-compile" ;;
        hew-cabi/*)                              echo "hew-cabi" ;;
        hew-capability-gen/*)                    echo "hew-capability-gen" ;;
        hew-cli/*)                               echo "hew-cli" ;;
        adze-cli/*)                              echo "adze-cli" ;;
        hew-runtime/*)                           echo "hew-runtime" ;;
        hew-runtime-testkit/*)                   echo "hew-runtime-testkit" ;;
        hew-observe/*)                           echo "hew-observe" ;;
        hew-analysis/*)                          echo "hew-analysis" ;;
        hew-lib/*)                               echo "hew-lib" ;;
        hew-lsp/*)                               echo "hew-lsp" ;;
        hew-testutil/*)                          echo "hew-testutil" ;;
        hew-std/*)                               echo "hew-std" ;;
        hew-wasm/*)                              echo "hew-wasm" ;;
        hew-sandbox-vm/*)                        echo "hew-sandbox-vm" ;;
        hew-sandbox-wasm/*)                      echo "hew-sandbox-wasm" ;;
        # Shared source or config touched multiple crates — treat as workspace-wide.
        Cargo.toml|Cargo.lock|.config/*|.cargo/*|Makefile|scripts/*)
            echo "__workspace__" ;;
        # Hew stdlib sources — checked by hew-cli and hew-codegen-rs.
        std/*|tests/hew/*)
            echo "hew-cli hew-codegen-rs" ;;
        # Test fixtures — touched by hew-cli.
        tests/*)
            echo "hew-cli" ;;
    esac
}

for f in "${UNIQUE_FILES[@]}"; do
    crate_names="$(path_to_crate "$f")"
    for c in $crate_names; do
        CRATES_FOUND[$c]=1
    done
done

# If a workspace-wide file changed, signal fallback by printing nothing.
if [[ -n "${CRATES_FOUND[__workspace__]+set}" ]]; then
    exit 0
fi

if [[ "${#CRATES_FOUND[@]}" -eq 0 ]]; then
    exit 0
fi

# Emit space-separated crate names.
printf '%s' "${!CRATES_FOUND[*]}"
