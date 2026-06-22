#!/usr/bin/env bash
#
# build-ast-grep-lang.sh — compile the tree-sitter-hew grammar into a dynamic
# library that ast-grep loads as the custom `hew` language (see sgconfig.yml).
#
# ast-grep cannot parse .hew natively; it dlopen()s this compiled grammar.
# The .so is a platform-specific build artifact (git-ignored) — rebuild it
# locally after pulling, or after the grammar changes.
#
# Usage:
#   scripts/build-ast-grep-lang.sh
#
# Env:
#   TREE_SITTER_HEW_DIR   Path to the tree-sitter-hew grammar repo
#                         (default: ../tree-sitter-hew, sibling of this repo).
#                         Grammar repo: https://github.com/hew-lang/tree-sitter-hew
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GRAMMAR_DIR="${TREE_SITTER_HEW_DIR:-$REPO_ROOT/../tree-sitter-hew}"
OUT="$REPO_ROOT/.ast-grep/hew-lang.so"

if ! command -v tree-sitter >/dev/null 2>&1; then
  echo "error: tree-sitter CLI not found on PATH (install via 'cargo install tree-sitter-cli' or 'npm i -g tree-sitter-cli')" >&2
  exit 1
fi
if [[ ! -f "$GRAMMAR_DIR/grammar.js" ]]; then
  echo "error: tree-sitter-hew grammar not found at: $GRAMMAR_DIR" >&2
  echo "       clone https://github.com/hew-lang/tree-sitter-hew next to this repo," >&2
  echo "       or set TREE_SITTER_HEW_DIR to its path." >&2
  exit 1
fi

mkdir -p "$REPO_ROOT/.ast-grep"
# A fixed .so name (not .dylib/.dll) keeps sgconfig.yml's libraryPath single-valued
# across platforms; ast-grep's dlopen does not care about the extension.
( cd "$GRAMMAR_DIR" && tree-sitter generate >/dev/null && tree-sitter build --output "$OUT" )
echo "built: $OUT"
