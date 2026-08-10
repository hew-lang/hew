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
    seeds=$(( seeds + 1 ))
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

cp "$repo_root/examples/machine/traffic_light.hew" \
    "$corpus_root/fuzz_machine/example-machine-traffic_light.hew"
cp "$repo_root/examples/machine/tcp_handshake.hew" \
    "$corpus_root/fuzz_machine/example-machine-tcp_handshake.hew"
printf 'state A;\nevent Tick;\non Tick: A -> A { A }\n' \
    >"$corpus_root/fuzz_machine/minimal-machine-body.hew"

printf '\0\1\2hew-v05-structured-seed' >"$corpus_root/fuzz_structured/structured-seed"

printf 'hydrated fuzz corpus in %s (%d source seeds)\n' "$corpus_root" "$seeds"
