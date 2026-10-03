#!/usr/bin/env bash
# Set the workspace release version everywhere it is written by hand.
# Usage: scripts/bump-version.sh 0.6.0-rc7
set -euo pipefail

new="${1:?usage: scripts/bump-version.sh <version, e.g. 0.6.0-rc7>}"
cd "$(dirname "$0")/.."

old=$(sed -n '/^\[workspace.package\]/,/^\[/{s/^version = "\(.*\)"/\1/p}' Cargo.toml | head -1)
[[ -n "$old" ]] || {
    echo "workspace version not found in Cargo.toml" >&2
    exit 1
}
[[ "$old" != "$new" ]] || {
    echo "already at $new"
    exit 0
}

sed -i "/^\[workspace.package\]/,/^\[/s/^version = \"$old\"/version = \"$new\"/" Cargo.toml
sed -i "s/\"version\": \"$old\"/\"version\": \"$new\"/" docs/syntax-data.json
sed -i "s/$old/$new/g" hew-sandbox-vm/package.json hew-sandbox-vm/package-lock.json \
    hew-sandbox-vm/bytecode/package-v1.md hew-sandbox-vm/test/*.mjs

cargo metadata --format-version 1 >/dev/null # refreshes the workspace entries in Cargo.lock
make baselines                               # fixtures and manifests that embed the version

echo "bumped $old -> $new; add docs/releases/v$new.md before tagging"
