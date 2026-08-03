#!/usr/bin/env bash
# Build Hew's ast-grep grammar from the checked-in lock contract.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOCK="$REPO_ROOT/tools/ast-grep.lock"
ARTIFACT_DIR="$REPO_ROOT/.ast-grep"
CACHE_DIR="$ARTIFACT_DIR/cache"
GRAMMAR_ARCHIVE="$CACHE_DIR/tree-sitter-hew.tar.gz"
GRAMMAR_DIR="$CACHE_DIR/tree-sitter-hew"
GRAMMAR_DIALECT_PATCH="$REPO_ROOT/tools/tree-sitter-hew-consume-parameter.patch"
TREE_SITTER_ROOT="$ARTIFACT_DIR/tree-sitter-tool"
TREE_SITTER="$TREE_SITTER_ROOT/bin/tree-sitter"
OUT="$ARTIFACT_DIR/hew-lang.so"
BOOTSTRAP=0

usage() { echo "usage: $0 [--bootstrap]" >&2; }
if [[ "${1:-}" == "--bootstrap" ]]; then BOOTSTRAP=1; shift; fi
[[ $# -eq 0 ]] || { usage; exit 2; }
# shellcheck disable=SC1090
source "$LOCK"

if [[ "$BOOTSTRAP" == 1 ]] && {
    [[ ! -x "$TREE_SITTER" ]] ||
    [[ "$("$TREE_SITTER" --version)" != "tree-sitter $TREE_SITTER_CLI_VERSION" ]];
}; then
    command -v cargo >/dev/null || {
        echo "error: cargo is required to install pinned tree-sitter-cli" >&2
        exit 1
    }
    cargo install "$TREE_SITTER_CLI_CARGO_PACKAGE" \
        --version "$TREE_SITTER_CLI_VERSION" --locked --root "$TREE_SITTER_ROOT"
fi
[[ -x "$TREE_SITTER" ]] || {
    echo "error: pinned tree-sitter $TREE_SITTER_CLI_VERSION is absent; run '$0 --bootstrap' once (network required)." >&2
    exit 1
}
[[ "$("$TREE_SITTER" --version)" == "tree-sitter $TREE_SITTER_CLI_VERSION" ]] || {
    echo "error: pinned tree-sitter version mismatch; remove .ast-grep/tree-sitter-tool and bootstrap again" >&2
    exit 1
}

sha256_file() {
    local backend="${HEW_AST_GREP_SHA256_BACKEND:-auto}"
    if [[ "$backend" == "auto" ]]; then
        if command -v sha256sum >/dev/null 2>&1; then
            backend=sha256sum
        elif command -v shasum >/dev/null 2>&1; then
            backend=shasum
        else
            echo "error: sha256sum or shasum is required to verify the grammar archive" >&2
            return 1
        fi
    fi
    case "$backend" in
        sha256sum)
            command -v sha256sum >/dev/null 2>&1 || {
                echo "error: requested sha256sum backend is unavailable" >&2
                return 1
            }
            sha256sum "$1" | awk '{print $1}'
            ;;
        shasum)
            command -v shasum >/dev/null 2>&1 || {
                echo "error: requested shasum backend is unavailable" >&2
                return 1
            }
            shasum -a 256 "$1" | awk '{print $1}'
            ;;
        *)
            echo "error: unsupported SHA-256 backend: $backend" >&2
            return 1
            ;;
    esac
}

need() {
    [[ -f "$GRAMMAR_ARCHIVE" ]] || return 1
    [[ "$(sha256_file "$GRAMMAR_ARCHIVE")" == "$TREE_SITTER_HEW_ARCHIVE_SHA256" ]] || {
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
    actual="$(sha256_file "$tmp")"
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
[[ -f "$GRAMMAR_DIALECT_PATCH" ]] || {
    echo "error: pinned Hew grammar dialect patch is absent" >&2; exit 1;
}
# The upstream locked grammar spells the parameter ownership modifier as
# `var` XOR `consume`, but hew-parser reads `consume` and then `var`
# independently, so `consume var items: Vec<i64>` is accepted source that the
# upstream rule rejects. A mis-parse here is not a loud failure: the region
# becomes an ERROR node and silently disappears from every structural authority
# rule. Apply the small reviewed source dialect and regenerate with the
# separately pinned tree-sitter CLI; compiling an opaque pre-generated parser
# would make the accepted dialect unauditable.
patch --batch --forward -d "$GRAMMAR_DIR" -p1 < "$GRAMMAR_DIALECT_PATCH" >/dev/null
(cd "$GRAMMAR_DIR" && "$TREE_SITTER" generate)
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
