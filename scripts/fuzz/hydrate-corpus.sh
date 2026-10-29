#!/usr/bin/env bash
# hydrate-corpus.sh — rebuild the parser fuzz corpus from tracked sources.
#
# Wipes each target's corpus directory and repopulates it from the
# vertical-slice accept fixtures and the curated examples. Because the wipe
# happens first, a source directory that has moved or emptied leaves a corpus
# of three hand-written seeds and still prints success — the fuzzers then run
# forever against nothing. Reject an empty seed set before accepting the copy.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$repo_root/scripts/lib/corpus-nonempty.sh"
corpus_root="$repo_root/hew-parser/fuzz/corpus"

source_targets=(fuzz_parse fuzz_lex fuzz_check fuzz_mir)
all_targets=("${source_targets[@]}" fuzz_machine fuzz_structured)

for target in "${all_targets[@]}"; do
    rm -rf "${corpus_root:?}/$target"
    mkdir -p "$corpus_root/$target"
done

seeds=0

copy_source_seed() {
    local src="$1"
    local label="$2"
    for target in "${source_targets[@]}"; do
        cp "$src" "$corpus_root/$target/$label"
    done
    seeds=$((seeds + 1))
}

while IFS= read -r -d '' src; do
    copy_source_seed "$src" "accept-$(basename "$src")"
done < <(find "$repo_root/tests/vertical-slice/accept" -maxdepth 1 -name '*.hew' -print0 | sort -z)

# Curated examples kept current by existing playground/machine gates.
while IFS= read -r -d '' src; do
    rel="${src#"$repo_root/"}"
    copy_source_seed "$src" "example-${rel//\//__}"
done < <(
    {
        find "$repo_root/examples/playground" -name '*.hew' -print0
        find "$repo_root/examples/machine" -name '*.hew' -print0
    } | sort -z
)

corpus_nonempty_assert "fuzz-seed-corpus" "$seeds" || exit 1

# Shapes that once ran the parser or the diagnostic path out of memory or time:
# nesting past the recursion limit, and long runs of one token on a single
# line. Each is under a few kilobytes, so every target starts from them.
repeat_unit() {
    local unit="$1" count="$2" out="" i
    for ((i = 0; i < count; i++)); do out+="$unit"; done
    printf '%s' "$out"
}

pathological_seed() {
    local label="$1" prefix="$2" unit="$3" count="$4" suffix="$5" target
    for target in "${source_targets[@]}"; do
        {
            printf '%s' "$prefix"
            repeat_unit "$unit" "$count"
            printf '%s\n' "$suffix"
        } \
            >"$corpus_root/$target/shape-$label.hew"
    done
}

pathological_seed nested-blocks 'fn main() ' '{' 300 ''
pathological_seed nested-block-exprs 'fn main() { let x = ' '{' 300 ' }'
pathological_seed nested-loops 'fn main() { ' 'loop { ' 300 ' }'
pathological_seed nested-parens 'fn main() { let x = ' '(' 300 ' }'
pathological_seed nested-generics 'fn f(x: ' 'Vec<' 300 ') {}'
pathological_seed nested-patterns 'fn main() { match x { ' 'Some(' 300 ' } }'
pathological_seed legacy-separators 'fn main() { ' '::' 400 ' }'
pathological_seed repeated-semicolons 'fn main() { ' ';' 600 ' }'
pathological_seed repeated-closers 'fn main() { ' '}' 600 ' }'
pathological_seed repeated-commas 'fn main() { f(' ',' 600 ') }'

cp "$repo_root/examples/machine/traffic_light.hew" \
    "$corpus_root/fuzz_machine/example-machine-traffic_light.hew"
cp "$repo_root/examples/machine/tcp_handshake.hew" \
    "$corpus_root/fuzz_machine/example-machine-tcp_handshake.hew"
printf 'state A;\nevent Tick;\non Tick: A -> A { A }\n' \
    >"$corpus_root/fuzz_machine/minimal-machine-body.hew"

printf '\0\1\2hew-v05-structured-seed' >"$corpus_root/fuzz_structured/structured-seed"

# The wipe above also removes the few regression seeds tracked in the ignored
# corpus directories; put those back. An export without a checkout has none.
if git -C "$repo_root" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    for target in "${all_targets[@]}"; do
        git -C "$repo_root" ls-files -z -- "hew-parser/fuzz/corpus/$target" |
            xargs -0 -r git -C "$repo_root" checkout --
    done
fi

printf 'hydrated fuzz corpus in %s (%d source seeds)\n' "$corpus_root" "$seeds"
