#!/usr/bin/env bash
# Build Hew's ast-grep grammar from the checked-in lock contract.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOCK="$REPO_ROOT/tools/ast-grep.lock"
ARTIFACT_DIR="$REPO_ROOT/.ast-grep"
CACHE_DIR="$ARTIFACT_DIR/cache"
GRAMMAR_ARCHIVE="$CACHE_DIR/tree-sitter-hew.tar.gz"
GRAMMAR_DIR="$CACHE_DIR/tree-sitter-hew"
OUT="$ARTIFACT_DIR/hew-lang.so"
BOOTSTRAP=0

usage() { echo "usage: $0 [--bootstrap]" >&2; }
if [[ "${1:-}" == "--bootstrap" ]]; then BOOTSTRAP=1; shift; fi
[[ $# -eq 0 ]] || { usage; exit 2; }
# shellcheck disable=SC1090
source "$LOCK"

need() {
    [[ -f "$GRAMMAR_ARCHIVE" ]] || return 1
    [[ "$(shasum -a 256 "$GRAMMAR_ARCHIVE" | awk '{print $1}')" == "$TREE_SITTER_HEW_ARCHIVE_SHA256" ]] || {
        echo "error: cached grammar archive checksum does not match tools/ast-grep.lock" >&2; return 1;
    }
}

if ! need; then
    if [[ "$BOOTSTRAP" != 1 ]]; then
        echo "error: pinned grammar source is absent; run '$0 --bootstrap' once (network required)." >&2
        exit 1
    fi
    mkdir -p "$CACHE_DIR"
    tmp="$GRAMMAR_ARCHIVE.tmp"
    curl --fail --location --silent --show-error \
        "$TREE_SITTER_HEW_REPOSITORY/archive/$TREE_SITTER_HEW_REV.tar.gz" -o "$tmp"
    actual="$(shasum -a 256 "$tmp" | awk '{print $1}')"
    [[ "$actual" == "$TREE_SITTER_HEW_ARCHIVE_SHA256" ]] || {
        rm -f "$tmp"
        echo "error: grammar archive checksum mismatch: expected $TREE_SITTER_HEW_ARCHIVE_SHA256, got $actual" >&2
        exit 1
    }
    mv "$tmp" "$GRAMMAR_ARCHIVE"
fi

rm -rf "$GRAMMAR_DIR"
mkdir -p "$GRAMMAR_DIR"
tar -xzf "$GRAMMAR_ARCHIVE" --strip-components=1 -C "$GRAMMAR_DIR"
grep -q "#define LANGUAGE_VERSION $TREE_SITTER_HEW_LANGUAGE_ABI" "$GRAMMAR_DIR/src/parser.c" || {
    echo "error: pinned grammar ABI does not match tools/ast-grep.lock" >&2; exit 1;
}
mkdir -p "$ARTIFACT_DIR"
case "$(uname -s)" in
    Darwin) cc -dynamiclib -fPIC "$GRAMMAR_DIR/src/parser.c" -o "$OUT" ;;
    Linux) cc -shared -fPIC "$GRAMMAR_DIR/src/parser.c" -o "$OUT" ;;
    *) echo "error: unsupported platform for custom ast-grep grammar: $(uname -s)" >&2; exit 1 ;;
esac
printf '%s\n%s\n' "$TREE_SITTER_HEW_ARCHIVE_SHA256" "$TREE_SITTER_HEW_LANGUAGE_ABI" > "$ARTIFACT_DIR/hew-lang.stamp"
echo "built pinned Hew grammar: $OUT"
