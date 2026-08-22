#!/usr/bin/env bash
# Assert that the playground release image for a Hew release tag exists in the
# container registry and that its image config binds the exact release commit.
#
# This is the release's acquisition contract: whoever publishes the image —
# the playground repository's Actions workflow, or a maintainer running
# `make release-publish` from a playground checkout — satisfies the identical  # external: hew-lang/playground
# assertion. The script observes only; it never publishes, never mutates, and
# never succeeds on a missing or mismatched image.
#
# The image config label `org.opencontainers.image.revision` carries the
# 40-character hew commit the image's toolchain and examples were built from.
# Every image manifest under the tag (one per platform) must carry it and every
# one must agree with the release commit.
#
# Inputs (environment):
#   IMAGE_REPOSITORY       registry repository, e.g. `hew-lang/playground`
#   IMAGE_TAG              image tag to assert, e.g. `v0.6.0-rc2`
#   EXPECTED_REVISION      exact lowercase 40-character hew commit SHA
#   REGISTRY_TOKEN         token with pull access to IMAGE_REPOSITORY
#   DEADLINE_MINUTES       whole minutes to wait for the manifest to appear
#   IMAGE_REGISTRY         registry host (default: ghcr.io)
#   POLL_INTERVAL_SECONDS  seconds between manifest polls (default: 20)
set -euo pipefail

readonly REVISION_LABEL="org.opencontainers.image.revision"
readonly MANIFEST_ACCEPT="application/vnd.oci.image.index.v1+json,application/vnd.docker.distribution.manifest.list.v2+json,application/vnd.oci.image.manifest.v1+json,application/vnd.docker.distribution.manifest.v2+json"
readonly CONFIG_ACCEPT="application/vnd.oci.image.config.v1+json,application/vnd.docker.container.image.v1+json"

fail() {
    echo "assert-playground-release-image: $*" >&2
    exit 1
}

IMAGE_REGISTRY="${IMAGE_REGISTRY:-ghcr.io}"
POLL_INTERVAL_SECONDS="${POLL_INTERVAL_SECONDS:-20}"

for var in IMAGE_REPOSITORY IMAGE_TAG EXPECTED_REVISION REGISTRY_TOKEN DEADLINE_MINUTES; do
    if [ -z "${!var:-}" ]; then
        fail "${var} must be set"
    fi
done

if ! [[ "${EXPECTED_REVISION}" =~ ^[0-9a-f]{40}$ ]]; then
    fail "EXPECTED_REVISION must be an exact lowercase 40-character commit SHA, got '${EXPECTED_REVISION}'"
fi
if ! [[ "${DEADLINE_MINUTES}" =~ ^[0-9]+$ ]]; then
    fail "DEADLINE_MINUTES must be a whole number of minutes, got '${DEADLINE_MINUTES}'"
fi
if ! [[ "${POLL_INTERVAL_SECONDS}" =~ ^[0-9]+$ ]] || [ "${POLL_INTERVAL_SECONDS}" -lt 1 ]; then
    fail "POLL_INTERVAL_SECONDS must be a positive whole number, got '${POLL_INTERVAL_SECONDS}'"
fi

readonly IMAGE_REF="${IMAGE_REGISTRY}/${IMAGE_REPOSITORY}:${IMAGE_TAG}"

# Mint a short-lived registry pull token. Re-minted on every poll so a long
# wait cannot outlive the token.
pull_token() {
    curl -fsS \
        -u "x-access-token:${REGISTRY_TOKEN}" \
        "https://${IMAGE_REGISTRY}/token?service=${IMAGE_REGISTRY}&scope=repository:${IMAGE_REPOSITORY}:pull" |
        jq -er '.token'
}

registry_get() {
    local token="$1" path="$2" accept="$3"
    curl -fsSL \
        -H "Authorization: Bearer ${token}" \
        -H "Accept: ${accept}" \
        "https://${IMAGE_REGISTRY}/v2/${IMAGE_REPOSITORY}/${path}"
}

body_file=$(mktemp)
trap 'rm -f "${body_file}"' EXIT

# Fetch the tag's manifest, printing the HTTP status. Kept separate from
# `registry_get` so the poll can tell "not published yet" (404) from "cannot
# read this repository" (401/403), which no amount of waiting fixes.
manifest_status() {
    curl -sSL -o "${body_file}" -w '%{http_code}' \
        -H "Authorization: Bearer $1" \
        -H "Accept: ${MANIFEST_ACCEPT}" \
        "https://${IMAGE_REGISTRY}/v2/${IMAGE_REPOSITORY}/manifests/${IMAGE_TAG}"
}

deadline=$(($(date +%s) + DEADLINE_MINUTES * 60))
token=""
manifest=""
echo "waiting up to ${DEADLINE_MINUTES}m for ${IMAGE_REF}"
while :; do
    if ! token=$(pull_token); then
        fail "could not obtain a pull token for ${IMAGE_REPOSITORY}; REGISTRY_TOKEN lacks package read access"
    fi
    status=$(manifest_status "${token}")
    case "${status}" in
    200)
        manifest=$(cat "${body_file}")
        break
        ;;
    404) ;;
    401 | 403)
        fail "REGISTRY_TOKEN cannot read ${IMAGE_REPOSITORY} (HTTP ${status}); grant it package read access"
        ;;
    *)
        fail "unexpected HTTP ${status} reading ${IMAGE_REF}"
        ;;
    esac
    if [ "$(date +%s)" -ge "${deadline}" ]; then
        fail "${IMAGE_REF} did not appear within ${DEADLINE_MINUTES}m"
    fi
    sleep "${POLL_INTERVAL_SECONDS}"
done
echo "manifest present: ${IMAGE_REF}"

# Resolve the image manifests under the tag. A tag may address a single image
# manifest or an index; attestation manifests carry no runnable config and are
# excluded.
config_digests=()
if jq -e 'has("manifests")' >/dev/null <<<"${manifest}"; then
    while IFS= read -r child_digest; do
        [ -n "${child_digest}" ] || continue
        child=$(registry_get "${token}" "manifests/${child_digest}" "${MANIFEST_ACCEPT}") ||
            fail "could not read image manifest ${child_digest}"
        config_digest=$(jq -er '.config.digest' <<<"${child}") ||
            fail "image manifest ${child_digest} has no config descriptor"
        config_digests+=("${config_digest}")
    done < <(jq -r '
        .manifests[]
        | select((.platform.os // "unknown") != "unknown")
        | select((.annotations["vnd.docker.reference.type"] // "") != "attestation-manifest")
        | .digest
    ' <<<"${manifest}")
else
    config_digest=$(jq -er '.config.digest' <<<"${manifest}") ||
        fail "${IMAGE_REF} has no config descriptor"
    config_digests+=("${config_digest}")
fi

if [ "${#config_digests[@]}" -eq 0 ]; then
    fail "${IMAGE_REF} resolves to no runnable image manifest"
fi

for config_digest in "${config_digests[@]}"; do
    config=$(registry_get "${token}" "blobs/${config_digest}" "${CONFIG_ACCEPT}") ||
        fail "could not read image config ${config_digest}"
    revision=$(jq -r --arg label "${REVISION_LABEL}" '.config.Labels[$label] // ""' <<<"${config}")
    if [ -z "${revision}" ]; then
        fail "${IMAGE_REF} (config ${config_digest}) carries no ${REVISION_LABEL} label; the publisher must stamp the hew release commit onto the image"
    fi
    if [ "${revision}" != "${EXPECTED_REVISION}" ]; then
        fail "${IMAGE_REF} (config ${config_digest}) binds ${revision}, expected ${EXPECTED_REVISION}"
    fi
    echo "config ${config_digest} binds ${EXPECTED_REVISION}"
done

echo "${IMAGE_REF} exists and binds release commit ${EXPECTED_REVISION}"
