#!/usr/bin/env bash
# Counterfactuals for the pinned grammar contract: no stale ABI or bytes pass.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
archive="$ROOT/.ast-grep/cache/tree-sitter-hew.tar.gz"
[[ -f "$archive" ]] || { echo "error: bootstrap the structural toolchain before this test" >&2; exit 1; }
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/scripts" "$tmp/tools" "$tmp/.ast-grep/cache"
cp "$ROOT/scripts/build-ast-grep-lang.sh" "$tmp/scripts/"
cp "$ROOT/tools/ast-grep.lock" "$tmp/tools/"
cp "$archive" "$tmp/.ast-grep/cache/tree-sitter-hew.tar.gz"

# Exercise both supported checksum command shapes, independent of which one is
# installed on the host running this contract test.
mkdir -p "$tmp/hash-bin"
for backend in sha256sum shasum; do
    cat > "$tmp/hash-bin/$backend" <<'PY'
#!/usr/bin/env python3
import hashlib
import os
import sys

path = sys.argv[-1]
with open(path, "rb") as handle:
    digest = hashlib.sha256(handle.read()).hexdigest()
with open(os.environ["HASH_BACKEND_LOG"], "a") as handle:
    handle.write(os.path.basename(sys.argv[0]) + "\n")
print(f"{digest}  {path}")
PY
    chmod +x "$tmp/hash-bin/$backend"
done
export PATH="$tmp/hash-bin:$PATH"
export HASH_BACKEND_LOG="$tmp/hash-backends.log"

# A generated library is deliberately absent: the supported builder must make it.
HEW_AST_GREP_SHA256_BACKEND=sha256sum "$tmp/scripts/build-ast-grep-lang.sh"
[[ -f "$tmp/.ast-grep/hew-lang.so" ]] || { echo "missing rebuilt grammar library" >&2; exit 1; }
HEW_AST_GREP_SHA256_BACKEND=shasum "$tmp/scripts/build-ast-grep-lang.sh"
grep -qx sha256sum "$HASH_BACKEND_LOG" || { echo "sha256sum backend was not verified" >&2; exit 1; }
grep -qx shasum "$HASH_BACKEND_LOG" || { echo "shasum backend was not verified" >&2; exit 1; }
if HEW_AST_GREP_SHA256_BACKEND=unverified "$tmp/scripts/build-ast-grep-lang.sh" >/dev/null 2>&1; then
    echo "an unverified checksum backend unexpectedly passed" >&2
    exit 1
fi

# A stale grammar dialect/ABI lock must refuse otherwise-good bytes.
sed -i.bak 's/TREE_SITTER_HEW_LANGUAGE_ABI=15/TREE_SITTER_HEW_LANGUAGE_ABI=999/' "$tmp/tools/ast-grep.lock"
if "$tmp/scripts/build-ast-grep-lang.sh" >/dev/null 2>&1; then
    echo "stale grammar ABI lock unexpectedly passed" >&2
    exit 1
fi
mv "$tmp/tools/ast-grep.lock.bak" "$tmp/tools/ast-grep.lock"

# A corrupted cached corpus must never be silently rebuilt from arbitrary bytes.
printf 'not a grammar archive' > "$tmp/.ast-grep/cache/tree-sitter-hew.tar.gz"
if "$tmp/scripts/build-ast-grep-lang.sh" >/dev/null 2>&1; then
    echo "corrupt grammar cache unexpectedly passed" >&2
    exit 1
fi
echo "ast-grep grammar contract counterfactuals: PASS"
