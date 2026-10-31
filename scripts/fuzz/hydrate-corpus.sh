#!/usr/bin/env bash
# hydrate-corpus.sh — rebuild the compiler fuzz corpora from tracked sources.
#
# Wipes each target's corpus directory and repopulates it from the
# vertical-slice fixtures, the curated examples and the example projects. Because the wipe
# happens first, a source directory that has moved or emptied leaves a corpus
# of three hand-written seeds and still prints success — the fuzzers then run
# forever against nothing. Reject an empty seed set before accepting the copy.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$repo_root/scripts/lib/corpus-nonempty.sh"
corpus_root="$repo_root/hew-parser/fuzz/corpus"
cli_corpus_root="$repo_root/hew-cli/fuzz/corpus"

source_targets=(fuzz_parse fuzz_lex fuzz_check fuzz_mir fuzz_fmt fuzz_check_determinism)
all_targets=("${source_targets[@]}" fuzz_machine fuzz_structured fuzz_project)

for target in "${all_targets[@]}"; do
    rm -rf "${corpus_root:?}/$target"
    mkdir -p "$corpus_root/$target"
done
rm -rf "${cli_corpus_root:?}/fuzz_diagnostics"
mkdir -p "$cli_corpus_root/fuzz_diagnostics"

seeds=0

# fuzz_diagnostics runs three full `hew check`s per input, about one input a
# second, so it takes every 20th source seed and every 4th rejected program:
# a seed set it replays in a few minutes.
diagnostic_stride_sources=20
diagnostic_stride_rejects=4

copy_source_seed() {
    local src="$1"
    local label="$2"
    for target in "${source_targets[@]}"; do
        cp "$src" "$corpus_root/$target/$label"
    done
    if ((seeds % diagnostic_stride_sources == 0)); then
        cp "$src" "$cli_corpus_root/fuzz_diagnostics/$label"
    fi
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

# Rejected programs exercise the diagnostic renderers.
rejects=0
while IFS= read -r -d '' src; do
    if ((rejects % diagnostic_stride_rejects == 0)); then
        cp "$src" "$cli_corpus_root/fuzz_diagnostics/reject-$(basename "$src")"
    fi
    rejects=$((rejects + 1))
done < <(find "$repo_root/tests/vertical-slice/reject" -maxdepth 1 -name '*.hew' -print0 | sort -z)

corpus_nonempty_assert "fuzz-seed-corpus" "$seeds" || exit 1

# fuzz_project reads a text archive: `=== <relative path>` starts a file and
# `=== @link <relative path>` makes a link to a directory outside the project.
# Each tracked project becomes one archive of its manifest and sources.
project_seeds=0
while IFS= read -r -d '' manifest; do
    dir="$(dirname "$manifest")"
    rel="${dir#"$repo_root/"}"
    out="$corpus_root/fuzz_project/project-${rel//\//__}"
    while IFS= read -r -d '' file; do
        printf '=== %s\n%s\n' "${file#"$dir/"}" "$(cat "$file")"
    done < <(find "$dir" \( -name hew.toml -o -name '*.hew' \) -type f -print0 | sort -z | head -z -n 12) >"$out"
    project_seeds=$((project_seeds + 1))
done < <(
    {
        find "$repo_root/examples/multifile" "$repo_root/examples/directory_module_demo" \
            "$repo_root/examples/module_generic_boundaries" -name hew.toml -print0
        find "$repo_root/tests/vertical-slice" -mindepth 3 -maxdepth 3 -name hew.toml -print0
    } | sort -z
)
corpus_nonempty_assert "fuzz-project-corpus" "$project_seeds" || exit 1

# Shapes that reach outside the project: an entry, a quoted import and a link
# leaving the root, and a dotted import that only the working directory holds.
project_seed() {
    printf '%b' "$2" >"$corpus_root/fuzz_project/escape-$1"
}
package='=== hew.toml\n[package]\nname = "app"\nversion = "0.1.0"\n'
project_seed main-parent "${package}main = \"../outside/canary.hew\"\n"
project_seed main-absolute "${package}main = \"/etc/hostname\"\n"
project_seed quoted-normalised "${package}=== main.hew\nimport \"./src/../../outside/canary.hew\";\nfn main() { println(canary()); }\n=== src/keep.hew\npub fn keep() {}\n"
project_seed quoted-absolute "${package}=== main.hew\nimport \"/etc/hostname\";\nfn main() {}\n"
project_seed quoted-link "${package}=== @link lnk\n=== main.hew\nimport \"lnk/canary.hew\";\nfn main() { println(canary()); }\n"
project_seed quoted-parent "${package}=== main.hew\nimport \"../outside/canary.hew\";\nfn main() { println(canary()); }\n"
project_seed cwd-module "${package}=== main.hew\nimport canary;\nfn main() { println(canary.canary()); }\n"
project_seed link-write-through "${package}=== @link lnk\n=== lnk/canary.hew\npub fn canary() -> i64 { 3 }\n=== main.hew\nfn main() {}\n"
project_seed link-module "${package}=== @link lnk\n=== main.hew\nimport lnk.canary;\nfn main() { println(canary.canary()); }\n"
project_seed path-dependency "${package}\n[dependencies]\ncanary = { path = \"../outside\" }\n=== main.hew\nimport canary;\nfn main() { println(canary.canary()); }\n"

# Shapes that once ran the parser or the diagnostic path out of memory or time:
# nesting past the recursion limit, and long runs of one token on a single
# line. Each is under a few kilobytes, so every target starts from them.
repeat_unit() {
    local unit="$1" count="$2" out="" i
    for ((i = 0; i < count; i++)); do out+="$unit"; done
    printf '%s' "$out"
}

pathological_seed() {
    local label="$1" prefix="$2" unit="$3" count="$4" suffix="$5" out
    for out in "${source_targets[@]/#/$corpus_root/}" "$cli_corpus_root/fuzz_diagnostics"; do
        {
            printf '%s' "$prefix"
            repeat_unit "$unit" "$count"
            printf '%s\n' "$suffix"
        } \
            >"$out/shape-$label.hew"
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

# Constants refer to each other across declaration order (spec §3.2).
printf 'const A: i64 = B + 1;\nconst B: i64 = C * 2;\nconst C: i64 = 3;\n\nfn main() {\n    println(A);\n}\n' \
    >"$corpus_root/fuzz_check_determinism/const-order.hew"
# Line endings and multi-byte text before a diagnostic site.
printf 'fn main() {\r\n    let s = "\xc3\xa9l\xc3\xa8ve \xf0\x9f\xa6\xab";\r\n    let x: i64 = s;\r\n}\r\n' \
    >"$cli_corpus_root/fuzz_diagnostics/crlf-unicode-mismatch.hew"

# The wipe above also removes the few regression seeds tracked in the ignored
# corpus directories; put those back. An export without a checkout has none.
if git -C "$repo_root" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    for target in "${all_targets[@]}"; do
        git -C "$repo_root" ls-files -z -- "hew-parser/fuzz/corpus/$target" |
            xargs -0 -r git -C "$repo_root" checkout --
    done
fi

printf 'hydrated fuzz corpora in %s and %s (%d source seeds, %d projects)\n' \
    "$corpus_root" "$cli_corpus_root" "$seeds" "$project_seeds"
