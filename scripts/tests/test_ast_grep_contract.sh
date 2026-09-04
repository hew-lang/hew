#!/usr/bin/env bash
# Counterfactuals for the pinned grammar contract: no stale ABI or bytes pass.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
archive="$ROOT/.ast-grep/cache/tree-sitter-hew.tar.gz"
[[ -f "$archive" ]] || {
    echo "error: bootstrap the structural toolchain before this test" >&2
    exit 1
}
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/scripts" "$tmp/tools" "$tmp/.ast-grep/cache"
cp "$ROOT/scripts/build-ast-grep-lang.sh" "$tmp/scripts/"
cp "$ROOT/tools/ast-grep.lock" "$tmp/tools/"
cp "$archive" "$tmp/.ast-grep/cache/tree-sitter-hew.tar.gz"
mkdir -p "$tmp/.ast-grep/tree-sitter-tool/bin"
cp "$ROOT/.ast-grep/tree-sitter-tool/bin/tree-sitter" "$tmp/.ast-grep/tree-sitter-tool/bin/"

# Exercise both supported checksum command shapes, independent of which one is
# installed on the host running this contract test.
mkdir -p "$tmp/hash-bin"
for backend in sha256sum shasum; do
    cat >"$tmp/hash-bin/$backend" <<'PY'
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
[[ -f "$tmp/.ast-grep/hew-lang.so" ]] || {
    echo "missing rebuilt grammar library" >&2
    exit 1
}
HEW_AST_GREP_SHA256_BACKEND=shasum "$tmp/scripts/build-ast-grep-lang.sh"
grep -qx sha256sum "$HASH_BACKEND_LOG" || {
    echo "sha256sum backend was not verified" >&2
    exit 1
}
grep -qx shasum "$HASH_BACKEND_LOG" || {
    echo "shasum backend was not verified" >&2
    exit 1
}
if HEW_AST_GREP_SHA256_BACKEND=unverified "$tmp/scripts/build-ast-grep-lang.sh" >/dev/null 2>&1; then
    echo "an unverified checksum backend unexpectedly passed" >&2
    exit 1
fi

# The pinned custom dialect must parse the Hew ownership/arithmetic syntax
# that motivated the current upstream revision.  Check the library rebuilt in
# this isolated fixture, not the repository's already-built copy.
cat >"$tmp/sgconfig.yml" <<EOF
customLanguages:
  hew:
    libraryPath: $tmp/.ast-grep/hew-lang.so
    extensions: [hew]
    expandoChar: _
EOF
cat >"$tmp/dialect.hew" <<'EOF'
fn dialect(a: int, b: int) {
    let owned = clone a;
    let add = a &+ b;
    let sub = a &- b;
    let mul = a &* b;
}

type Handle {}

fn consume_var(consume var value: Handle) {
    value;
}

trait HandleOps {
    fn push(consuming self, consume child: Handle) -> Self;
}

extern "C" {
    #[runtime_capability(blocking_offload)]
    fn release(consume value: Handle);
    fn combine(left: Handle, consume right: Handle);
}
EOF
ast_grep="$ROOT/.ast-grep/tool/bin/ast-grep"
[[ "$("$ast_grep" run --config "$tmp/sgconfig.yml" --lang hew --kind clone_expression --json=stream "$tmp/dialect.hew" | wc -l | tr -d ' ')" == 1 ]] || {
    echo "pinned Hew grammar did not parse exactly one clone-prefix expression" >&2
    exit 1
}
for operator in '&+' '&-' '&*'; do
    [[ "$("$ast_grep" run --config "$tmp/sgconfig.yml" --lang hew --pattern "\$A $operator \$B" --json=stream "$tmp/dialect.hew" | wc -l | tr -d ' ')" == 1 ]] || {
        echo "pinned Hew grammar did not parse wrapping operator $operator" >&2
        exit 1
    }
done
# The dollar-prefixed names are ast-grep metavariables, not shell expansions.
# shellcheck disable=SC2016
[[ "$("$ast_grep" run --config "$tmp/sgconfig.yml" --lang hew --pattern 'fn $F(consume $P: $T);' --json=stream "$tmp/dialect.hew" | wc -l | tr -d ' ')" == 1 ]] || {
    echo "pinned Hew grammar did not parse a leading named consume parameter" >&2
    exit 1
}
# shellcheck disable=SC2016
[[ "$("$ast_grep" run --config "$tmp/sgconfig.yml" --lang hew --pattern 'fn $F($A: $AT, consume $B: $BT);' --json=stream "$tmp/dialect.hew" | wc -l | tr -d ' ')" == 1 ]] || {
    echo "pinned Hew grammar did not parse a secondary named consume parameter" >&2
    exit 1
}
# shellcheck disable=SC2016
[[ "$("$ast_grep" run --config "$tmp/sgconfig.yml" --lang hew --pattern 'fn $F(consume var $P: $T) { $$$BODY }' --json=stream "$tmp/dialect.hew" | wc -l | tr -d ' ')" == 1 ]] || {
    echo "pinned Hew grammar did not parse a mutable consuming parameter" >&2
    exit 1
}
cat >"$tmp/invalid-consume-order.hew" <<'EOF'
fn invalid(var consume handle: Handle) { handle; }
EOF
if ! "$ast_grep" run --config "$tmp/sgconfig.yml" --lang hew --kind ERROR "$tmp/invalid-consume-order.hew" >/dev/null 2>&1; then
    echo "pinned Hew grammar accepted invalid var-before-consume order" >&2
    exit 1
fi
cat >"$tmp/externs.hew" <<'EOF'
extern "C" {
    #[runtime_capability(blocking_offload)]
    fn make() -> Handle;
    fn close(consume handle: Handle);
}
#[resource]
#[opaque]
type Handle {}
EOF
[[ "$("$ast_grep" run --config "$tmp/sgconfig.yml" --lang hew --kind extern_function --json=stream "$tmp/externs.hew" | wc -l | tr -d ' ')" == 2 ]] || {
    echo "pinned Hew grammar did not parse attributed consuming extern declarations" >&2
    exit 1
}
if "$ast_grep" run --config "$tmp/sgconfig.yml" --lang hew --kind ERROR "$tmp/externs.hew" >/dev/null 2>&1; then
    echo "pinned Hew grammar accepted ERROR nodes for consuming extern declarations" >&2
    exit 1
fi
if "$ast_grep" run --config "$tmp/sgconfig.yml" --lang hew --kind ERROR "$tmp/dialect.hew" >/dev/null 2>&1; then
    echo "pinned Hew dialect corpus contains parser ERROR nodes" >&2
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
printf 'not a grammar archive' >"$tmp/.ast-grep/cache/tree-sitter-hew.tar.gz"
if "$tmp/scripts/build-ast-grep-lang.sh" >/dev/null 2>&1; then
    echo "corrupt grammar cache unexpectedly passed" >&2
    exit 1
fi
echo "ast-grep grammar contract counterfactuals: PASS"
