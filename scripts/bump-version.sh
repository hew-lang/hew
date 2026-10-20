#!/usr/bin/env bash
# Set the workspace base version for a release or the next development cycle.
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
cargo metadata --manifest-path hew-parser/fuzz/Cargo.toml --format-version 1 >/dev/null
make baselines # fixtures and manifests that embed the version

echo "bumped $old -> $new; development builds derive -dev metadata from Git"
echo "before tagging a release, add docs/releases/v$new.md and its changelog entry"
