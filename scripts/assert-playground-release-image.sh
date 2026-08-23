#!/usr/bin/env bash
# Assert that the playground release image for a Hew release tag exists in the
# container registry and that its image config binds the exact release commit.
#
# This is the release's acquisition contract: whoever publishes the image -
# the playground repository's Actions workflow, or a maintainer running the
# direct, sanitized `scripts/publish-release-image.sh candidate` entrypoint from
# its pinned clean checkout - satisfies the identical assertion. Post-tag
# publication uses `scripts/publish-release-image.sh publish` and publish authority.
# The script observes only; it never publishes or mutates.
#
# Inputs (environment):
#   IMAGE_REPOSITORY       registry repository, e.g. `hew-lang/playground`
#   IMAGE_TAG              image tag to assert, e.g. `v0.6.0-rc2`
#   EXPECTED_REVISION      exact lowercase 40-character hew commit SHA
#   GHCR_TOKEN             token with pull access to IMAGE_REPOSITORY. Local
#                          GHCR runs require a classic PAT with read:packages.
#   DEADLINE_MINUTES       whole minutes to wait (local use)
#   DEADLINE_EPOCH         absolute Unix deadline (workflow use)
#   IMAGE_REGISTRY         registry host (default: ghcr.io)
#   IMAGE_REGISTRY_SCHEME  registry URL scheme (default: https)
#   GHCR_USERNAME          registry username (default: GITHUB_ACTOR or USER)
#   POLL_INTERVAL_SECONDS  seconds between manifest polls (default: 20)
set -euo pipefail

readonly REVISION_LABEL="org.opencontainers.image.revision"
readonly MANIFEST_ACCEPT="application/vnd.oci.image.index.v1+json,application/vnd.docker.distribution.manifest.list.v2+json,application/vnd.oci.image.manifest.v1+json,application/vnd.docker.distribution.manifest.v2+json"
readonly CONFIG_ACCEPT="application/vnd.oci.image.config.v1+json,application/vnd.docker.container.image.v1+json"
readonly CURL_CONNECT_TIMEOUT_SECONDS=5
readonly CURL_MAX_TIME_SECONDS=20

fail() {
    echo "assert-playground-release-image: $*" >&2
    exit 1
}

IMAGE_REGISTRY="${IMAGE_REGISTRY:-ghcr.io}"
IMAGE_REGISTRY_SCHEME="${IMAGE_REGISTRY_SCHEME:-https}"
GHCR_USERNAME="${GHCR_USERNAME:-${GITHUB_ACTOR:-${USER:-}}}"
POLL_INTERVAL_SECONDS="${POLL_INTERVAL_SECONDS:-20}"

for var in IMAGE_REPOSITORY IMAGE_TAG EXPECTED_REVISION GHCR_TOKEN GHCR_USERNAME; do
    if [ -z "${!var:-}" ]; then
        fail "${var} must be set"
    fi
done

if ! [[ "${EXPECTED_REVISION}" =~ ^[0-9a-f]{40}$ ]]; then
    fail "EXPECTED_REVISION must be an exact lowercase 40-character commit SHA, got '${EXPECTED_REVISION}'"
fi
if [ -n "${DEADLINE_EPOCH:-}" ] && [ -n "${DEADLINE_MINUTES:-}" ]; then
    fail "set only one of DEADLINE_EPOCH or DEADLINE_MINUTES"
fi
if [ -z "${DEADLINE_EPOCH:-}" ] && [ -z "${DEADLINE_MINUTES:-}" ]; then
    fail "DEADLINE_EPOCH or DEADLINE_MINUTES must be set"
fi
if [ -n "${DEADLINE_EPOCH:-}" ] && ! [[ "${DEADLINE_EPOCH}" =~ ^[0-9]+$ ]]; then
    fail "DEADLINE_EPOCH must be a Unix timestamp, got '${DEADLINE_EPOCH}'"
fi
if [ -n "${DEADLINE_MINUTES:-}" ] && ! [[ "${DEADLINE_MINUTES}" =~ ^[0-9]+$ ]]; then
    fail "DEADLINE_MINUTES must be a whole number of minutes, got '${DEADLINE_MINUTES}'"
fi
if ! [[ "${POLL_INTERVAL_SECONDS}" =~ ^[0-9]+$ ]] || [ "${POLL_INTERVAL_SECONDS}" -lt 1 ]; then
    fail "POLL_INTERVAL_SECONDS must be a positive whole number, got '${POLL_INTERVAL_SECONDS}'"
fi
if [ "${IMAGE_REGISTRY_SCHEME}" != "https" ] &&
    { [ "${IMAGE_REGISTRY_SCHEME}" != "http" ] || [[ "${IMAGE_REGISTRY}" != 127.0.0.1:* ]]; }; then
    fail "IMAGE_REGISTRY_SCHEME must be https (http is allowed only for a loopback test registry)"
fi

readonly IMAGE_REF="${IMAGE_REGISTRY}/${IMAGE_REPOSITORY}:${IMAGE_TAG}"
if [ -n "${DEADLINE_EPOCH:-}" ]; then
    deadline="${DEADLINE_EPOCH}"
    deadline_description="the job deadline"
else
    deadline=$(($(date +%s) + DEADLINE_MINUTES * 60))
    deadline_description="${DEADLINE_MINUTES}m"
fi

check_deadline() {
    if [ "$(date +%s)" -ge "${deadline}" ]; then
        fail "${IMAGE_REF} did not bind ${EXPECTED_REVISION} within ${deadline_description}"
    fi
}

curl_before_deadline() {
    local now remaining max_time connect_timeout
    now=$(date +%s)
    remaining=$((deadline - now))
    if [ "${remaining}" -le 0 ]; then
        fail "${IMAGE_REF} did not bind ${EXPECTED_REVISION} within ${deadline_description}"
    fi
    max_time="${CURL_MAX_TIME_SECONDS}"
    if [ "${remaining}" -lt "${max_time}" ]; then
        max_time="${remaining}"
    fi
    connect_timeout="${CURL_CONNECT_TIMEOUT_SECONDS}"
    if [ "${max_time}" -lt "${connect_timeout}" ]; then
        connect_timeout="${max_time}"
    fi
    curl --connect-timeout "${connect_timeout}" --max-time "${max_time}" "$@"
}

pull_token() {
    curl_before_deadline -fsS \
        -u "${GHCR_USERNAME}:${GHCR_TOKEN}" \
        "${IMAGE_REGISTRY_SCHEME}://${IMAGE_REGISTRY}/token?service=${IMAGE_REGISTRY}&scope=repository:${IMAGE_REPOSITORY}:pull" |
        jq -er '.token'
}

registry_get() {
    local token="$1" path="$2" accept="$3"
    curl_before_deadline -fsSL \
        --oauth2-bearer "${token}" \
        -H "Accept: ${accept}" \
        "${IMAGE_REGISTRY_SCHEME}://${IMAGE_REGISTRY}/v2/${IMAGE_REPOSITORY}/${path}"
}

body_file=$(mktemp)
trap 'rm -f "${body_file}"' EXIT

manifest_status() {
    local token="$1"
    curl_before_deadline -sSL -o "${body_file}" -w '%{http_code}' \
        --oauth2-bearer "${token}" \
        -H "Accept: ${MANIFEST_ACCEPT}" \
        "${IMAGE_REGISTRY_SCHEME}://${IMAGE_REGISTRY}/v2/${IMAGE_REPOSITORY}/manifests/${IMAGE_TAG}"
}

manifest_binds_expected_revision() {
    local token="$1" manifest="$2" descriptor child_digest child config_digest
    local annotation platform_os config revision
    local stale=0
    local config_digests=()

    jq -e . >/dev/null <<<"${manifest}" ||
        fail "${IMAGE_REF} returned an invalid manifest document"

    if jq -e 'has("manifests")' >/dev/null <<<"${manifest}"; then
        while IFS= read -r descriptor; do
            [ -n "${descriptor}" ] || continue
            annotation=$(jq -r '.annotations["vnd.docker.reference.type"] // ""' <<<"${descriptor}")
            if [ "${annotation}" = "attestation-manifest" ]; then
                continue
            fi
            platform_os=$(jq -r '.platform.os // ""' <<<"${descriptor}")
            child_digest=$(jq -r '.digest // ""' <<<"${descriptor}")
            if [ -z "${platform_os}" ] || [ "${platform_os}" = "unknown" ]; then
                fail "${IMAGE_REF} index child ${child_digest:-<missing digest>} has no platform.os and is not marked as an attestation manifest"
            fi
            if [ -z "${child_digest}" ]; then
                fail "${IMAGE_REF} index contains a runnable child with no digest"
            fi
            child=$(registry_get "${token}" "manifests/${child_digest}" "${MANIFEST_ACCEPT}") ||
                fail "could not read image manifest ${child_digest}"
            config_digest=$(jq -er '.config.digest' <<<"${child}") ||
                fail "image manifest ${child_digest} has no config descriptor"
            config_digests+=("${config_digest}")
        done < <(jq -c '.manifests[]' <<<"${manifest}")
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
            echo "${IMAGE_REF} (config ${config_digest}) still binds ${revision}; waiting for ${EXPECTED_REVISION}" >&2
            stale=1
        fi
    done

    [ "${stale}" -eq 0 ]
}

echo "waiting until ${deadline} for ${IMAGE_REF} to bind ${EXPECTED_REVISION}"
while :; do
    check_deadline
    if ! token=$(pull_token); then
        fail "could not obtain a pull token for ${IMAGE_REPOSITORY}; GHCR_TOKEN must be a classic PAT with read:packages for local runs"
    fi
    if ! status=$(manifest_status "${token}"); then
        fail "registry request failed while reading ${IMAGE_REF}"
    fi
    case "${status}" in
    200)
        manifest=$(cat "${body_file}")
        if manifest_binds_expected_revision "${token}" "${manifest}"; then
            echo "${IMAGE_REF} exists and binds release commit ${EXPECTED_REVISION}"
            exit 0
        fi
        ;;
    404) ;;
    401 | 403)
        fail "GHCR_TOKEN cannot read ${IMAGE_REPOSITORY} (HTTP ${status}); local runs require a classic PAT with read:packages"
        ;;
    *)
        fail "unexpected HTTP ${status} reading ${IMAGE_REF}"
        ;;
    esac

    check_deadline
    remaining_seconds=$((deadline - $(date +%s)))
    sleep_seconds="${POLL_INTERVAL_SECONDS}"
    if [ "${remaining_seconds}" -lt "${sleep_seconds}" ]; then
        sleep_seconds="${remaining_seconds}"
    fi
    if [ "${sleep_seconds}" -gt 0 ]; then
        sleep "${sleep_seconds}"
    fi
done
