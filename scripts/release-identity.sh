#!/usr/bin/env bash
# Check that every committed release identity names the same version, then
# print the release tag.
#
# Usage: scripts/release-identity.sh [TAG]
#   TAG  expected release tag (for example v0.6.0-rc6). Without it, the tag is
#        derived from the workspace version.
#   RELEASE_NOTES_OPTIONAL=1  downgrade missing release notes to a warning, for
#        dry runs of the release machinery from an ordinary branch.
#
# The checked identities are the workspace version in Cargo.toml, the
# hew-sandbox-vm package version, docs/syntax-data.json and the curated notes
# at docs/releases/<tag>.md. Release validation runs this before building and
# the release gate runs it on the release branch, so a mismatch fails before
# a tag exists.
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "${root}"

json_version() {
    python3 -c 'import json, sys; print(json.load(open(sys.argv[1]))["version"])' "$1"
}

version="$(awk -F'"' '/^\[workspace\.package\]/ { section = 1; next } /^\[/ { section = 0 } section && /^version[[:space:]]*=/ { print $2; exit }' Cargo.toml)"
if [[ -z "${version}" ]]; then
    echo "::error::Cargo.toml has no [workspace.package] version" >&2
    exit 1
fi
tag="${1:-v${version}}"

status=0
fail() {
    echo "::error::$*" >&2
    status=1
}

if ! [[ "${tag}" =~ ^v[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z.-]+)?$ ]]; then
    fail "release tag ${tag} is not v<SemVer>"
fi
[[ "${tag}" == "v${version}" ]] || fail "release tag ${tag} does not match Cargo.toml version ${version}"

sandbox_version="$(json_version hew-sandbox-vm/package.json)"
[[ "${sandbox_version}" == "${version}" ]] || fail "hew-sandbox-vm/package.json version ${sandbox_version} does not match ${version}"

syntax_version="$(json_version docs/syntax-data.json)"
[[ "${syntax_version}" == "${version}" ]] || fail "docs/syntax-data.json version ${syntax_version} does not match ${version}"

if [[ ! -s "docs/releases/${tag}.md" ]]; then
    if [[ "${RELEASE_NOTES_OPTIONAL:-}" == 1 ]]; then
        echo "::warning::release notes docs/releases/${tag}.md are missing or empty" >&2
    else
        fail "release notes docs/releases/${tag}.md are missing or empty"
    fi
fi

[[ "${status}" -eq 0 ]] || exit "${status}"
echo "${tag}"
