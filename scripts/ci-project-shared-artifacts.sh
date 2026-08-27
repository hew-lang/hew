#!/usr/bin/env bash
#
# Project the certified shared test artefacts into Cargo's output directory.
#
# A gate that COMPILES its own test binary resolves shared artefacts under
# Cargo's root, because hew-testutil walks `<target>/<profile>/deps/` upwards
# from `current_exe()`, while the archive populates the other root. Symlink,
# never copy, derived from the same `archive.include` the producer packs.
# Rationale: docs/dev/ci-tiers-and-routing.md, "The projection".
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
