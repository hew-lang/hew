#!/usr/bin/env bash
#
# Project the certified shared test artefacts into Cargo's output directory.
#
# The Linux shards have two target directories. A test binary that ARRIVED in
# the nextest archive resolves shared artefacts under the archive's root; one
# the shard COMPILES resolves them under Cargo's, because hew-testutil derives
# the authority by walking `<target>/<profile>/deps/` upwards from
# `current_exe()` -- and nothing ever put them there.
#
# So project, never copy: one symlink per artefact, pointing at the read-only
# certified original, DERIVED from the same `[profile.ci] archive.include` the
# producer packs. `verify` accepts only the exact link `link` made, and runs
# before and after each affected gate, so a Cargo build that replaced one is
# red there rather than silently resolving to something nobody certified.
#
# Full mechanism: docs/dev/ci-tiers-and-routing.md, "The projection".
set -euo pipefail

die() {
    printf 'shared-artefact projection: %s\n' "$*" >&2
    exit 1
}

[ "$#" -eq 3 ] || die "usage: $0 link|verify <artefact-root> <cargo-root>"
action="$1"
artefact_root="$2"
cargo_root="$3"
case "$action" in
    link | verify) ;;
    *) die "unknown action '$action'" ;;
esac
[ "$artefact_root" != "$cargo_root" ] ||
    die "both roots are $cargo_root; there is nothing to project"

manifest="$(cd "$(dirname "$0")/.." && pwd)/.config/nextest.toml"
relatives="$(
    sed -n '/^archive\.include = \[/,/^]/p' "$manifest" |
        sed -n 's/.*path = "\([^"]*\)".*/\1/p'
)"
[ -n "$relatives" ] || die "no archive.include paths in $manifest"

while IFS= read -r relative; do
    source_path="$artefact_root/$relative"
    destination="$cargo_root/$relative"
    # `on-missing = "ignore"` rows -- the opposite-architecture Linux archive
    # -- are absent by design on hosts without the sysroot.
    [ -e "$source_path" ] || continue

    if [ -L "$destination" ]; then
        actual="$(readlink -- "$destination")"
        [ "$actual" = "$source_path" ] ||
            die "$destination points at $actual, not the certified $source_path"
    elif [ -e "$destination" ]; then
        die "$destination is a real file where the certified $source_path belongs;
Cargo wrote over the projection"
    elif [ "$action" = link ]; then
        mkdir -p "$(dirname "$destination")"
        ln -s "$source_path" "$destination"
    else
        die "$destination is gone; the projection of $source_path was removed"
    fi

    [ -f "$destination" ] || die "$destination does not resolve to a file"
done <<<"$relatives"
