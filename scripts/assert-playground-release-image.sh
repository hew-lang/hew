#!/usr/bin/env bash
# Assert that the playground release image for a Hew release tag exists in the
# container registry and that its immutable digest, target platform, and image
# config bind the exact release commit.
#
# This is the release's handoff contract. A maintainer runs the direct,
# sanitized `scripts/publish-release-image.sh candidate` entrypoint from a pinned
# clean checkout, records the pushed digest, and only then creates the tag.
# The script observes only; it never publishes or mutates.
#
# Inputs (environment):
#   IMAGE_REPOSITORY       registry repository, e.g. `hew-lang/playground`
#   IMAGE_TAG              image tag to assert, e.g. `v0.6.0-rc2`
#   EXPECTED_DIGEST        exact immutable manifest/index sha256 digest
#   EXPECTED_PLATFORM      exact normalized platform (required: linux/amd64)
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
readonly OCI_IMAGE_MANIFEST="application/vnd.oci.image.manifest.v1+json"
readonly DOCKER_IMAGE_MANIFEST="application/vnd.docker.distribution.manifest.v2+json"
readonly ATTESTATION_TYPE="attestation-manifest"
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

for var in IMAGE_REPOSITORY IMAGE_TAG EXPECTED_DIGEST EXPECTED_PLATFORM EXPECTED_REVISION GHCR_TOKEN GHCR_USERNAME; do
    if [ -z "${!var:-}" ]; then
        fail "${var} must be set"
    fi
done

if ! [[ "${EXPECTED_REVISION}" =~ ^[0-9a-f]{40}$ ]]; then
    fail "EXPECTED_REVISION must be an exact lowercase 40-character commit SHA, got '${EXPECTED_REVISION}'"
fi
if ! [[ "${EXPECTED_DIGEST}" =~ ^sha256:[0-9a-f]{64}$ ]]; then
    fail "EXPECTED_DIGEST must be sha256 followed by exactly 64 lowercase hexadecimal digits"
fi
if [ "${EXPECTED_PLATFORM}" != "linux/amd64" ]; then
    fail "EXPECTED_PLATFORM must be exactly linux/amd64"
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
readonly IMMUTABLE_IMAGE_REF="${IMAGE_REGISTRY}/${IMAGE_REPOSITORY}@${EXPECTED_DIGEST}"
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

registry_get_to_file() {
    local token="$1" path="$2" accept="$3" output_file="$4"
    curl_before_deadline -fsSL \
        -o "${output_file}" \
        --oauth2-bearer "${token}" \
        -H "Accept: ${accept}" \
        "${IMAGE_REGISTRY_SCHEME}://${IMAGE_REGISTRY}/v2/${IMAGE_REPOSITORY}/${path}"
}

work_dir=$(mktemp -d)
body_file="${work_dir}/manifest"
headers_file="${work_dir}/headers"
trap 'rm -rf -- "${work_dir}"' EXIT

sha256_file() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | awk '{print $1}'
    else
        shasum -a 256 "$1" | awk '{print $1}'
    fi
}

manifest_status() {
    local token="$1"
    : >"${headers_file}"
    curl_before_deadline -sSL -D "${headers_file}" -o "${body_file}" -w '%{http_code}' \
        --oauth2-bearer "${token}" \
        -H "Accept: ${MANIFEST_ACCEPT}" \
        "${IMAGE_REGISTRY_SCHEME}://${IMAGE_REGISTRY}/v2/${IMAGE_REPOSITORY}/manifests/${IMAGE_TAG}"
}

manifest_binds_expected_identity() {
    local token="$1" manifest_file="$2" descriptor child_digest config_digest
    local annotation media_type platform_os platform_arch config_os config_arch revision
    local child_file config_file raw_digest runnable_count=0 descriptor_count=0
    local config_digests=()

    jq -e . "${manifest_file}" >/dev/null ||
        fail "${IMAGE_REF} returned an invalid manifest document"

    if jq -e 'has("manifests")' "${manifest_file}" >/dev/null; then
        while IFS= read -r descriptor; do
            [ -n "${descriptor}" ] || continue
            descriptor_count=$((descriptor_count + 1))
            annotation=$(jq -r '.annotations["vnd.docker.reference.type"] // ""' <<<"${descriptor}")
            media_type=$(jq -r '.mediaType // ""' <<<"${descriptor}")
            platform_os=$(jq -r '.platform.os // ""' <<<"${descriptor}")
            platform_arch=$(jq -r '.platform.architecture // ""' <<<"${descriptor}")
            child_digest=$(jq -r '.digest // ""' <<<"${descriptor}")
            if ! [[ "${child_digest}" =~ ^sha256:[0-9a-f]{64}$ ]]; then
                fail "${IMAGE_REF} index child has an invalid digest"
            fi
            if [ "${annotation}" = "${ATTESTATION_TYPE}" ]; then
                if [ "${platform_os}/${platform_arch}" != "unknown/unknown" ] ||
                    [ "${media_type}" != "${OCI_IMAGE_MANIFEST}" ]; then
                    fail "${IMAGE_REF} index child ${child_digest} is not a narrowly recognized non-runnable attestation descriptor"
                fi
                continue
            fi
            if [ -z "${platform_os}" ] || [ "${platform_os}" = "unknown" ]; then
                fail "${IMAGE_REF} index child ${child_digest} has no runnable platform and is not marked as an attestation manifest"
            fi
            if [ "${media_type}" != "${OCI_IMAGE_MANIFEST}" ] &&
                [ "${media_type}" != "${DOCKER_IMAGE_MANIFEST}" ]; then
                fail "${IMAGE_REF} runnable index child ${child_digest} has unsupported mediaType '${media_type:-<missing>}'"
            fi
            runnable_count=$((runnable_count + 1))
            if [ "${platform_os}/${platform_arch}" != "${EXPECTED_PLATFORM}" ]; then
                fail "${IMMUTABLE_IMAGE_REF} contains additional runnable platform ${platform_os:-<missing>}/${platform_arch:-<missing>}; expected exactly one ${EXPECTED_PLATFORM} image manifest"
            fi
            child_file="${work_dir}/child-${descriptor_count}"
            registry_get_to_file "${token}" "manifests/${child_digest}" "${MANIFEST_ACCEPT}" "${child_file}" ||
                fail "could not read image manifest ${child_digest}"
            raw_digest="sha256:$(sha256_file "${child_file}")"
            if [ "${raw_digest}" != "${child_digest}" ]; then
                fail "image manifest ${child_digest} raw response digest is ${raw_digest}, not its descriptor digest"
            fi
            jq -e . "${child_file}" >/dev/null ||
                fail "image manifest ${child_digest} returned an invalid document"
            config_digest=$(jq -er '.config.digest' "${child_file}") ||
                fail "image manifest ${child_digest} has no config descriptor"
            config_digests+=("${config_digest}")
        done < <(jq -c '.manifests[]' "${manifest_file}")
        if [ "${runnable_count}" -ne 1 ]; then
            fail "${IMMUTABLE_IMAGE_REF} must resolve to exactly one ${EXPECTED_PLATFORM} runnable image manifest"
        fi
    else
        config_digest=$(jq -er '.config.digest' "${manifest_file}") ||
            fail "${IMAGE_REF} has no config descriptor"
        config_digests+=("${config_digest}")
    fi

    if [ "${#config_digests[@]}" -ne 1 ]; then
        fail "${IMMUTABLE_IMAGE_REF} must resolve to exactly one ${EXPECTED_PLATFORM} image manifest"
    fi

    descriptor_count=0
    for config_digest in "${config_digests[@]}"; do
        descriptor_count=$((descriptor_count + 1))
        if ! [[ "${config_digest}" =~ ^sha256:[0-9a-f]{64}$ ]]; then
            fail "image manifest has an invalid config digest"
        fi
        config_file="${work_dir}/config-${descriptor_count}"
        registry_get_to_file "${token}" "blobs/${config_digest}" "${CONFIG_ACCEPT}" "${config_file}" ||
            fail "could not read image config ${config_digest}"
        raw_digest="sha256:$(sha256_file "${config_file}")"
        if [ "${raw_digest}" != "${config_digest}" ]; then
            fail "image config ${config_digest} raw response digest is ${raw_digest}, not its descriptor digest"
        fi
        jq -e . "${config_file}" >/dev/null ||
            fail "image config ${config_digest} returned an invalid document"
        config_os=$(jq -r '.os // ""' "${config_file}")
        config_arch=$(jq -r '.architecture // ""' "${config_file}")
        if [ "${config_os}/${config_arch}" != "${EXPECTED_PLATFORM}" ]; then
            fail "${IMMUTABLE_IMAGE_REF} config is ${config_os:-<missing>}/${config_arch:-<missing>}, not ${EXPECTED_PLATFORM}"
        fi
        revision=$(jq -r --arg label "${REVISION_LABEL}" '.config.Labels[$label] // ""' "${config_file}")
        if [ -z "${revision}" ]; then
            fail "${IMAGE_REF} (config ${config_digest}) carries no ${REVISION_LABEL} label; the publisher must stamp the hew release commit onto the image"
        fi
        if [ "${revision}" != "${EXPECTED_REVISION}" ]; then
            fail "${IMMUTABLE_IMAGE_REF} (config ${config_digest}) binds ${revision}, not ${EXPECTED_REVISION}"
        fi
    done
}

echo "waiting until ${deadline} for ${IMAGE_REF} to resolve to ${EXPECTED_DIGEST}"
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
        registry_digest=$(awk '
            tolower($1) == "docker-content-digest:" {
                value=$2
                sub(/\r$/, "", value)
                print value
            }
        ' "${headers_file}" | tail -n 1)
        if [ "${registry_digest}" != "${EXPECTED_DIGEST}" ]; then
            fail "${IMAGE_REF} registry digest is ${registry_digest:-<missing>}, not ${EXPECTED_DIGEST}"
        fi
        raw_digest="sha256:$(sha256_file "${body_file}")"
        if [ "${raw_digest}" != "${EXPECTED_DIGEST}" ]; then
            fail "${IMAGE_REF} raw manifest digest is ${raw_digest}, not ${EXPECTED_DIGEST}"
        fi
        manifest_binds_expected_identity "${token}" "${body_file}"
        echo "${IMMUTABLE_IMAGE_REF} is ${EXPECTED_PLATFORM} and binds release commit ${EXPECTED_REVISION}"
        exit 0
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
