#!/usr/bin/env bash
# Type-check every stdlib source. The public target keeps its historical name,
# but the empty expected-failure ratchet is gone: every file must pass.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/corpus-nonempty.sh"
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/cargo-output-dir.sh"
HEW_BIN="${HEW_BIN:-$(cargo_debug_dir "$REPO_ROOT")/hew}"
STDLIB_DIR="$REPO_ROOT/std"

if [[ ! -x "$HEW_BIN" ]]; then
    echo "error: hew binary not found at $HEW_BIN" >&2
    exit 1
fi

files=()
while IFS= read -r -d $'\0' file; do
    files+=("$file")
done < <(find "$STDLIB_DIR" -name '*.hew' -not -path '*/target/*' -print0 | sort -z)
corpus_nonempty_assert "stdlib-check-files" "${#files[@]}" || exit 1

failed=0
for file in "${files[@]}"; do
    if ! "$HEW_BIN" check "$file"; then
        failed=$((failed + 1))
    fi
done

if [[ $failed -ne 0 ]]; then
    echo "stdlib check: $failed/${#files[@]} files failed" >&2
    exit 1
fi
echo "stdlib check: all ${#files[@]} files passed"
