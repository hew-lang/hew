#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
corpus_root="$repo_root/hew-parser/fuzz/corpus"

source_targets=(fuzz_parse fuzz_lex fuzz_check fuzz_mir fuzz_msgpack)
all_targets=("${source_targets[@]}" fuzz_machine fuzz_structured)

for target in "${all_targets[@]}"; do
    rm -rf "${corpus_root:?}/$target"
    mkdir -p "$corpus_root/$target"
done

copy_source_seed() {
    local src="$1"
    local label="$2"
    for target in "${source_targets[@]}"; do
        cp "$src" "$corpus_root/$target/$label"
    done
}

while IFS= read -r -d '' src; do
    copy_source_seed "$src" "accept-$(basename "$src")"
done < <(find "$repo_root/tests/v05-vertical-slice/accept" -maxdepth 1 -name '*.hew' -print0 | sort -z)

# Curated examples kept current by existing playground/machine lanes.
while IFS= read -r -d '' src; do
    rel="${src#"$repo_root/"}"
    copy_source_seed "$src" "example-${rel//\//__}"
done < <(
    {
        find "$repo_root/examples/playground" -name '*.hew' -print0
        find "$repo_root/examples/machine" -name '*.hew' -print0
    } | sort -z
)

cp "$repo_root/examples/machine/traffic_light.hew" \
    "$corpus_root/fuzz_machine/example-machine-traffic_light.hew"
cp "$repo_root/examples/machine/tcp_handshake.hew" \
    "$corpus_root/fuzz_machine/example-machine-tcp_handshake.hew"
printf 'state A;\nevent Tick;\non Tick: A -> A { A }\n' \
    >"$corpus_root/fuzz_machine/minimal-machine-body.hew"

printf '\0\1\2hew-v05-structured-seed' >"$corpus_root/fuzz_structured/structured-seed"
printf 'wire enum SeedCommand {\n    Start;\n    Stop;\n}\n' \
    >"$corpus_root/fuzz_msgpack/wire-enum-seed.hew"

printf 'hydrated fuzz corpus in %s\n' "$corpus_root"
