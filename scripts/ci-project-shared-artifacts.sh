#!/usr/bin/env bash
#
# Project the certified shared test artefacts into Cargo's output directory.
#
# A gate that COMPILES its own test binary resolves shared artefacts under
# Cargo's root -- hew-testutil walks `<target>/<profile>/deps/` upwards from
# `current_exe()` -- while the archive populates the other. Symlink, never
# copy, derived from the `archive.include` the producer packs. Rationale:
# docs/dev/ci-tiers-and-routing.md, "The projection".
set -euo pipefail

die() {
    printf 'shared-artefact projection: %s\n' "$*" >&2
    exit 1
}

[ "$#" -ge 3 ] || die "usage: $0 link|verify|gate <artefact-root> <cargo-root>"
action="$1"
artefact_root="$2"
cargo_root="$3"
case "$action" in
    link | verify) [ "$#" -eq 3 ] || die "$action takes no command" ;;
    gate) [ "$#" -eq 4 ] || die "gate takes one command string" ;;
    *) die "unknown action '$action'" ;;
esac
[ "$artefact_root" != "$cargo_root" ] ||
    die "both roots are $cargo_root; there is nothing to project"

# One process, so a failing gate cannot skip the verification the way a second
# recipe line can. The functional result wins the exit status; a clean run that
# corrupted the projection reports the corruption.
if [ "$action" = gate ]; then
    "$0" link "$artefact_root" "$cargo_root"
    functional=0
    sh -c "$4" || functional=$?
    verify=0
    "$0" verify "$artefact_root" "$cargo_root" || verify=$?
    [ "$functional" -eq 0 ] || exit "$functional"
    exit "$verify"
fi

manifest="$(cd "$(dirname "$0")/.." && pwd)/.config/nextest.toml"
# Absence is a policy read from the entry, not assumed: only an explicit
# `on-missing = "ignore"` may be absent. Anything else missing means the
# archive did not carry what it promised.
inventory="$(
    sed -n '/^archive\.include = \[/,/^]/p' "$manifest" |
        sed -n '/path = "/{
            s/.*path = "\([^"]*\)".*on-missing = "\([^"]*\)".*/\1|\2/
            t emit
            s/.*path = "\([^"]*\)".*/\1|error/
            :emit
            p
        }'
)"
[ -n "$inventory" ] || die "no archive.include paths in $manifest"

while IFS='|' read -r relative policy; do
    source_path="$artefact_root/$relative"
    destination="$cargo_root/$relative"
    if [ ! -e "$source_path" ]; then
        [ "$policy" = ignore ] ||
            die "$source_path is missing and its on-missing policy is '$policy'"
        continue
    fi

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
done <<<"$inventory"
