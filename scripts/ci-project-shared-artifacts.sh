#!/usr/bin/env bash
#
# Project the certified shared test artefacts into Cargo's output directory: a
# gate that COMPILES its own test binary resolves them under Cargo's root --
# hew-testutil walks `<target>/<profile>/deps/` up from `current_exe()` --
# while the archive populates the other. Symlink, never copy, derived from the
# `archive.include` the producer packs.
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

# One process, so a failing gate cannot skip the verification a second recipe
# line would. The functional result wins the exit status.
if [ "$action" = gate ]; then
    "$0" link "$artefact_root" "$cargo_root"
    functional=0
    sh -c "$4" || functional=$?
    verify=0
    "$0" verify "$artefact_root" "$cargo_root" || verify=$?
    [ "$functional" -eq 0 ] || exit "$functional"
    exit "$verify"
fi

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
manifest="$repo_root/.config/nextest.toml"
# Real TOML -- inline tables, any key order, escapes -- so the repository's own
# reader parses it. Records are NUL-delimited through a file: a path may hold
# anything but NUL, and a pipe would hide the reader's exit status.
records="$(mktemp "${TMPDIR:-/tmp}/shared-artefacts.XXXXXX")"
cleanup() { rm -f -- "$records"; }
trap cleanup EXIT
python3 - "$repo_root" "$manifest" >"$records" <<'PARSE' ||
import pathlib, sys

sys.path.insert(0, sys.argv[1] + "/scripts/lib")
import toml_compat

with open(sys.argv[2], "rb") as handle:
    include = toml_compat.load(handle)["profile"]["ci"]["archive"]["include"]
if not isinstance(include, list) or not include:
    sys.exit("profile.ci.archive.include is not a non-empty array")
for entry in include:
    if not isinstance(entry, dict):
        sys.exit(f"archive.include entry is not a table: {entry!r}")
    path, policy = entry.get("path"), entry.get("on-missing", "error")
    posix = pathlib.PurePosixPath(path if isinstance(path, str) else "")
    if not posix.parts or posix.is_absolute() or ".." in posix.parts:
        sys.exit(f"archive.include entry has no usable relative path: {entry!r}")
    if entry.get("relative-to") != "target":
        sys.exit(f"relative-to must be \"target\": {entry!r}")
    if policy not in ("error", "ignore"):
        sys.exit(f"{path}: unsupported on-missing policy {policy!r}")
    sys.stdout.buffer.write(path.encode() + b"\0" + policy.encode() + b"\0")
PARSE
    die "could not read the archive inventory from $manifest"
[ -s "$records" ] || die "no archive.include entries in $manifest"

while IFS= read -r -d '' relative && IFS= read -r -d '' policy; do
    source_path="$artefact_root/$relative"
    destination="$cargo_root/$relative"
    if [ ! -e "$source_path" ]; then
        [ "$policy" = ignore ] ||
            die "$source_path is missing and its on-missing policy is '$policy'"
        # Absent by policy means absent on BOTH sides: a leftover file or a
        # dangling symlink is a path a test resolves to something uncertified.
        { [ ! -e "$destination" ] && [ ! -L "$destination" ]; } ||
            die "$source_path is absent by policy but $destination still exists"
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
done <"$records"
