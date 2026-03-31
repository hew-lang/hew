#!/usr/bin/env bash
# End-to-end package installation test for all Linux distribution formats.
#
# Builds all package formats from local release artifacts, then installs and
# tests each one in a matching Docker container.  Requires:
#   - Docker
#   - make release (or --skip-build with pre-built packages in dist/)
#
# Usage (from repo root, after `make release`):
#   ./installers/docker/test-all-packages.sh
#   ./installers/docker/test-all-packages.sh --skip-build
#   ./installers/docker/test-all-packages.sh --only debian,rpm
#
# Options:
#   --version VERSION   Override version (default: from Cargo.toml)
#   --skip-build        Skip `make release` and package build; use existing dist/
#   --only LIST         Comma-separated subset: debian,ubuntu,ubuntu22,rpm,fedora,arch,alpine
#   --arch ARCH         Target arch: x86_64 (default) or aarch64
#   --build-timeout N   Timeout in seconds for `make release` (default: 1800)
#   --package-timeout N Timeout in seconds for package assembly (default: 900)
#   --test-timeout N    Timeout in seconds per Docker distribution test (default: 300)
#   -h / --help         Show this help

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
DIST_DIR="${REPO_DIR}/dist"
TEST_HEW="${REPO_DIR}/installers/docker/test-stdlib.hew"

# ── Colours ───────────────────────────────────────────────────────────────────
if [ -t 1 ]; then
    BOLD='\033[1m' GREEN='\033[0;32m' CYAN='\033[0;36m'
    YELLOW='\033[0;33m' RED='\033[0;31m' DIM='\033[2m' RESET='\033[0m'
else
    BOLD='' GREEN='' CYAN='' YELLOW='' RED='' DIM='' RESET=''
fi

step() { printf "\n${BOLD}==> %s${RESET}\n" "$1"; }
info() { printf "    ${CYAN}%-14s${RESET} %s\n" "$1" "$2"; }
ok() { printf "  ${GREEN}✓${RESET}  %-12s %s\n" "$1" "$2"; }
fail() { printf "  ${RED}✗${RESET}  %-12s %s\n" "$1" "$2"; }
skip() { printf "  ${DIM}○  %-12s %s${RESET}\n" "$1" "$2"; }
warn() { printf "    ${YELLOW}warn:${RESET} %s\n" "$1" >&2; }

# ── Argument defaults ─────────────────────────────────────────────────────────
VERSION=""
TARGET_ARCH="$(uname -m)"
SKIP_BUILD=false
ONLY=""
BUILD_TIMEOUT=1800
PACKAGE_TIMEOUT=900
TEST_TIMEOUT=300

_usage() {
    awk '
        NR == 1 { next }
        /^#/ { sub(/^# ?/, ""); print; next }
        { exit }
    ' "$0"
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
    --version)
        VERSION="$2"
        shift 2
        ;;
    --arch)
        TARGET_ARCH="$2"
        shift 2
        ;;
    --build-timeout)
        BUILD_TIMEOUT="$2"
        shift 2
        ;;
    --package-timeout)
        PACKAGE_TIMEOUT="$2"
        shift 2
        ;;
    --test-timeout)
        TEST_TIMEOUT="$2"
        shift 2
        ;;
    --skip-build)
        SKIP_BUILD=true
        shift
        ;;
    --only)
        ONLY="$2"
        shift 2
        ;;
    -h | --help) _usage ;;
    *)
        echo "Unknown option: $1" >&2
        exit 1
        ;;
    esac
done

_should_test() { [[ -z "${ONLY}" ]] || [[ ",${ONLY}," == *",$1,"* ]]; }

if command -v timeout >/dev/null 2>&1; then
    TIMEOUT_BIN=timeout
elif command -v gtimeout >/dev/null 2>&1; then
    TIMEOUT_BIN=gtimeout
else
    echo "error: timeout or gtimeout is required for bounded execution" >&2
    exit 1
fi

run_with_timeout() {
    local seconds="$1"
    shift
    "$TIMEOUT_BIN" --kill-after=5 "$seconds" "$@"
}

# ── Version detection ─────────────────────────────────────────────────────────
if [[ -z "${VERSION}" ]]; then
    VERSION="$(grep '^version' "${REPO_DIR}/Cargo.toml" | head -1 | sed 's/.*= *"\(.*\)"/\1/')"
fi
info "version" "${VERSION}"
info "arch" "${TARGET_ARCH}"

# ── DEB arch mapping ──────────────────────────────────────────────────────────
case "${TARGET_ARCH}" in
x86_64) DEB_ARCH=amd64 ;;
aarch64) DEB_ARCH=arm64 ;;
*)
    echo "Unknown arch: ${TARGET_ARCH}" >&2
    exit 1
    ;;
esac

# ── Package paths ─────────────────────────────────────────────────────────────
DEB_PKG="${DIST_DIR}/hew_${VERSION}-1_${DEB_ARCH}.deb"
RPM_PKG="$(ls "${DIST_DIR}"/hew-${VERSION}-1.*.rpm 2>/dev/null | head -1)"
ARCH_PKG="${DIST_DIR}/hew-bin-${VERSION}-1-${TARGET_ARCH}.pkg.tar.zst"
APK_PKG="${DIST_DIR}/hew-${VERSION}.apk"

# ── Results tracking ──────────────────────────────────────────────────────────
PASSED=0
FAILED=0
SKIPPED=0

_pass() {
    PASSED=$((PASSED + 1))
    ok "$1" "$2"
}
_fail() {
    FAILED=$((FAILED + 1))
    fail "$1" "$2"
}
_skip() {
    SKIPPED=$((SKIPPED + 1))
    skip "$1" "$2"
}

# ── Step 1: Build packages ────────────────────────────────────────────────────
step "Building packages"

if $SKIP_BUILD; then
    info "mode" "skip-build — using existing packages in dist/"
else
    info "building" "release binaries (make release)..."
    info "timeout" "${BUILD_TIMEOUT}s make release / ${PACKAGE_TIMEOUT}s package build"
    run_with_timeout "${BUILD_TIMEOUT}" make -C "${REPO_DIR}" release

    info "building" "all package formats (installers/build-packages.sh)..."
    run_with_timeout "${PACKAGE_TIMEOUT}" "${REPO_DIR}/installers/build-packages.sh" \
        --version "${VERSION}" \
        --arch "${TARGET_ARCH}" \
        --only "tarball,debian,rpm,arch"
fi

# Locate RPM package (dist tag suffix varies: .fc41, .el9, etc.)
RPM_PKG="$(ls "${DIST_DIR}"/hew-${VERSION}-1.*.rpm 2>/dev/null | head -1 || true)"

# ── Smoke-test helper ─────────────────────────────────────────────────────────
# _run_test LABEL IMAGE INSTALL_CMDS
# Runs install + `hew run` inside a Docker container.
# INSTALL_CMDS is passed to bash -c as a single string.
_run_test() {
    local label="$1" image="$2" install_cmds="$3"

    step "Testing ${label} (${image})"
    local tmpdir
    tmpdir="$(mktemp -d)"

    # Stage the test file and packages
    cp "${TEST_HEW}" "${tmpdir}/test.hew"
    [[ -f "${DEB_PKG}" ]] && cp "${DEB_PKG}" "${tmpdir}/" || true
    [[ -f "${RPM_PKG}" ]] && cp "${RPM_PKG}" "${tmpdir}/" || true
    [[ -f "${ARCH_PKG}" ]] && cp "${ARCH_PKG}" "${tmpdir}/" || true
    [[ -f "${APK_PKG}" ]] && cp "${APK_PKG}" "${tmpdir}/" || true

    local output
    if output=$(run_with_timeout "${TEST_TIMEOUT}" docker run --rm \
        -v "${tmpdir}:/pkg" \
        "${image}" \
        bash -c "${install_cmds}" 2>&1); then
        echo "${output}" | sed 's/^/    /'
        _pass "${label}" "hew run succeeded"
    else
        status=$?
        echo "${output}" | sed 's/^/    /'
        if [[ "${status}" -eq 124 ]]; then
            _fail "${label}" "timed out after ${TEST_TIMEOUT}s"
        else
            _fail "${label}" "test failed (exit ${status})"
        fi
    fi
    rm -rf "${tmpdir}"
}

# ── Step 2: Test each distribution ───────────────────────────────────────────

# ── Debian 12 (bookworm) ──────────────────────────────────────────────────────
if _should_test debian; then
    if [[ -f "${DEB_PKG}" ]]; then
        _run_test "debian-12" "debian:bookworm" '
            set -e
            apt-get update -qq
            apt-get install -y --no-install-recommends gcc libc-dev 2>/dev/null | tail -3
            dpkg -i /pkg/hew_*.deb 2>/dev/null || apt-get install -f -y -qq
            echo "--- hew run ---"
            hew run /pkg/test.hew
        '
    else
        _skip "debian-12" "no .deb found in dist/"
    fi
fi

# ── Ubuntu 24.04 (noble) ──────────────────────────────────────────────────────
if _should_test ubuntu; then
    if [[ -f "${DEB_PKG}" ]]; then
        _run_test "ubuntu-24.04" "ubuntu:24.04" '
            set -e
            apt-get update -qq
            apt-get install -y --no-install-recommends gcc libc-dev 2>/dev/null | tail -3
            dpkg -i /pkg/hew_*.deb 2>/dev/null || apt-get install -f -y -qq
            echo "--- hew run ---"
            hew run /pkg/test.hew
        '
    else
        _skip "ubuntu-24.04" "no .deb found in dist/"
    fi
fi

# ── Ubuntu 22.04 (jammy) ─────────────────────────────────────────────────────
if _should_test ubuntu22; then
    if [[ -f "${DEB_PKG}" ]]; then
        # libz3-4 on 22.04 has a different package name (libz3-4 still works, but
        # check availability first).
        _run_test "ubuntu-22.04" "ubuntu:22.04" '
            set -e
            apt-get update -qq
            apt-get install -y --no-install-recommends gcc libc-dev 2>/dev/null | tail -3
            dpkg --force-depends -i /pkg/hew_*.deb 2>/dev/null || true
            apt-get install -f -y -qq
            echo "--- hew run ---"
            hew run /pkg/test.hew
        '
    else
        _skip "ubuntu-22.04" "no .deb found in dist/"
    fi
fi

# ── Fedora 41 ─────────────────────────────────────────────────────────────────
if _should_test rpm || _should_test fedora; then
    if [[ -n "${RPM_PKG}" ]] && [[ -f "${RPM_PKG}" ]]; then
        _run_test "fedora-41" "fedora:41" '
            set -e
            dnf install -y -q gcc 2>/dev/null | tail -3
            dnf install -y -q /pkg/hew-*.rpm
            echo "--- hew run ---"
            hew run /pkg/test.hew
        '
    else
        _skip "fedora-41" "no .rpm found in dist/"
    fi
fi

# ── Arch Linux ────────────────────────────────────────────────────────────────
if _should_test arch; then
    if [[ -f "${ARCH_PKG}" ]]; then
        _run_test "arch" "archlinux:latest" '
            set -e
            pacman -Sy --noconfirm gcc 2>/dev/null | tail -3
            pacman -U --noconfirm /pkg/hew-bin-*.pkg.tar.zst
            echo "--- hew run ---"
            hew run /pkg/test.hew
        '
    else
        _skip "arch" "no .pkg.tar.zst found in dist/"
    fi
fi

# ── Alpine Linux 3.21 ────────────────────────────────────────────────────────
# Alpine uses the musl tarball + apk package. The .apk may only be built in CI
# (requires Alpine-native hew-codegen build). Skip gracefully if absent.
if _should_test alpine; then
    if [[ -f "${APK_PKG}" ]]; then
        _run_test "alpine-3.21" "alpine:3.21" '
            set -e
            apk add --no-cache gcc musl-dev 2>/dev/null | tail -3
            apk add --no-cache --allow-untrusted /pkg/hew-*.apk
            echo "--- hew run ---"
            hew run /pkg/test.hew
        '
    else
        _skip "alpine-3.21" "no .apk found in dist/ (requires CI/Alpine native build)"
    fi
fi

# ── Summary ───────────────────────────────────────────────────────────────────
step "Results"
printf "  ${GREEN}passed:${RESET}  %d\n" "${PASSED}"
printf "  ${YELLOW}skipped:${RESET} %d\n" "${SKIPPED}"
printf "  ${RED}failed:${RESET}  %d\n" "${FAILED}"
echo ""

if [[ "${FAILED}" -gt 0 ]]; then
    printf "${RED}${BOLD}FAILED${RESET}: %d distribution(s) did not pass.\n\n" "${FAILED}"
    exit 1
fi
if [[ "${PASSED}" -eq 0 ]]; then
    printf "${YELLOW}No tests ran${RESET} — check --only filter or build packages first.\n\n"
    exit 1
fi
printf "${GREEN}${BOLD}All %d distribution(s) passed.${RESET}\n\n" "${PASSED}"
