#!/usr/bin/env bash
# installers/build-packages.sh — Build all Hew installer packages from source.
#
# Run from the repository root:
#   installers/build-packages.sh [OPTIONS]
#
# OPTIONS
#   --version VERSION    Release version (default: read from workspace Cargo.toml)
#   --skip-build         Skip cargo/cmake build; use existing dist/ tarball
#   --arch ARCH          Target arch: x86_64 (default) or aarch64
#   --only FORMAT[,...]  Build only these formats (comma-separated list)
#   -h / --help          Show this help
#
# FORMATS
#   tarball   Build binary tarball from source (always runs unless --skip-build)
#   debian    Debian/Ubuntu .deb  (requires Docker)
#   rpm       Fedora/RHEL .rpm    (requires Docker)
#   arch      Arch Linux .pkg.tar.zst (requires Docker)
#   alpine    Alpine .apk         (requires Docker)
#   docker    Container image     (requires Docker)
#   shell     Syntax-check install.sh
#   ps1       Syntax-check install.ps1 (requires pwsh)
#   homebrew  Syntax-check homebrew/hew.rb (requires ruby)
#   nix       Syntax-check nix/default.nix (requires nix-instantiate)
#
# OUTPUT
#   dist/hew-v{VERSION}-linux-{ARCH}.tar.gz
#   dist/hew_{VERSION}-1_{DEB_ARCH}.deb
#   dist/hew-{VERSION}-1.{RPM_ARCH}.rpm
#   dist/hew-bin-{VERSION}-1-{ARCH}.pkg.tar.zst
#   dist/hew-{VERSION}.apk
#   Docker image tagged r.hew.sh/hew:{VERSION} and r.hew.sh/hew:latest

set -euo pipefail

# ── Paths ────────────────────────────────────────────────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
DIST_DIR="${REPO_DIR}/dist"

# ── Option defaults ───────────────────────────────────────────────────────────
VERSION=""
TARGET_ARCH="$(uname -m)" # x86_64 or aarch64
SKIP_BUILD=false
ONLY=""

# ── Tracking ─────────────────────────────────────────────────────────────────
RESULTS=() # entries: "ok|skip|fail:FORMAT:MESSAGE"

# ── Colors ────────────────────────────────────────────────────────────────────
_setup_colors() {
    if [ -t 1 ]; then
        BOLD='\033[1m' GREEN='\033[0;32m' CYAN='\033[0;36m'
        YELLOW='\033[0;33m' RED='\033[0;31m' DIM='\033[2m' RESET='\033[0m'
    else
        BOLD='' GREEN='' CYAN='' YELLOW='' RED='' DIM='' RESET=''
    fi
}

# ── Logging helpers ───────────────────────────────────────────────────────────
step() { printf "\n${BOLD}==> %s${RESET}\n" "$1"; }
info() { printf "    ${CYAN}%-12s${RESET} %s\n" "$1" "$2"; }
warn() { printf "    ${YELLOW}warn:${RESET} %s\n" "$1" >&2; }
err() {
    printf "\n    ${RED}error:${RESET} %s\n\n" "$1" >&2
    exit 1
}

_record_ok() {
    RESULTS+=("ok:$1:$2")
    printf "  ${GREEN}✓${RESET}  %-10s %s\n" "$1" "$2"
}
_record_skip() {
    RESULTS+=("skip:$1:$2")
    printf "  ${DIM}○  %-10s %s${RESET}\n" "$1" "$2"
}
_record_fail() {
    RESULTS+=("fail:$1:$2")
    printf "  ${RED}✗  %-10s %s${RESET}\n" "$1" "$2"
}

# ── Helpers ───────────────────────────────────────────────────────────────────
_require_cmd() { command -v "$1" >/dev/null 2>&1 || err "required command not found: $1"; }
_has_cmd() { command -v "$1" >/dev/null 2>&1; }

# Clean up directories that may contain root-owned files from Docker
_docker_rm() {
    local dir="$1"
    rm -rf "${dir}" 2>/dev/null ||
        docker run --rm -v "${dir}:/cleanup" alpine rm -rf /cleanup 2>/dev/null ||
        true
}

_should_build() {
    local fmt="$1"
    [[ -z "${ONLY}" ]] || [[ ",${ONLY}," == *",${fmt},"* ]]
}

# ── Argument parsing ──────────────────────────────────────────────────────────
_parse_args() {
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
        --skip-build)
            SKIP_BUILD=true
            shift
            ;;
        --only)
            ONLY="$2"
            shift 2
            ;;
        -h | --help)
            _usage
            exit 0
            ;;
        *) err "unknown option: $1" ;;
        esac
    done
}

_usage() {
    # Print the block comment at the top of this script
    sed -n '2,/^[^#]/{ /^#/!d; s/^# \{0,1\}//; p }' "${BASH_SOURCE[0]}"
}

# Resolve VERSION from workspace Cargo.toml [workspace.package] version field
_resolve_version() {
    if [[ -z "${VERSION}" ]]; then
        VERSION="$(grep -A5 '^\[workspace\.package\]' "${REPO_DIR}/Cargo.toml" |
            grep '^version' |
            head -1 |
            sed -E 's/version *= *"([^"]+)".*/\1/')"
        VERSION="${VERSION:-0.1.0}"
    fi
}

# ── Step 1: Build binary tarball ──────────────────────────────────────────────
PLATFORM=""
TARBALL_NAME=""
TARBALL_PATH=""

_init_tarball_vars() {
    PLATFORM="linux-${TARGET_ARCH}"
    TARBALL_NAME="hew-v${VERSION}-${PLATFORM}.tar.gz"
    TARBALL_PATH="${DIST_DIR}/${TARBALL_NAME}"
}

build_tarball() {
    _init_tarball_vars

    if $SKIP_BUILD; then
        if [[ -f "${TARBALL_PATH}" ]]; then
            _record_ok "tarball" "${TARBALL_PATH} (existing)"
        else
            warn "tarball not found at ${TARBALL_PATH}; package builds that need it will fail"
            _record_skip "tarball" "skipped (--skip-build, tarball absent)"
        fi
        return 0
    fi

    step "Building from source"
    cd "${REPO_DIR}"

    info "cargo" "hew-cli adze-cli hew-serialize hew-runtime (release)..."
    cargo build -p hew-cli -p adze-cli -p hew-serialize -p hew-runtime --release

    if [[ -f "${REPO_DIR}/hew-codegen/build/build.ninja" ]]; then
        local jobs
        jobs="$(nproc 2>/dev/null || sysctl -n hw.logicalcpu 2>/dev/null || echo 4)"
        info "cmake" "hew-codegen (release, -j${jobs})..."
        cmake --build "${REPO_DIR}/hew-codegen/build" --config Release -j"${jobs}"
    else
        warn "hew-codegen/build not configured; hew-codegen will be absent. Run 'make codegen' first."
    fi

    step "Assembling tarball"
    local staging_root="${DIST_DIR}/.staging-$$"
    local staging="${staging_root}/hew-v${VERSION}-${PLATFORM}"
    rm -rf "${staging_root}"
    mkdir -p "${staging}/bin" "${staging}/lib" "${staging}/std" "${staging}/completions"

    for bin in hew adze; do
        if [[ -f "${REPO_DIR}/target/release/${bin}" ]]; then
            cp "${REPO_DIR}/target/release/${bin}" "${staging}/bin/"
            chmod +x "${staging}/bin/${bin}"
        else
            warn "binary not found: target/release/${bin}"
        fi
    done

    if [[ -f "${REPO_DIR}/hew-codegen/build/src/hew-codegen" ]]; then
        cp "${REPO_DIR}/hew-codegen/build/src/hew-codegen" "${staging}/bin/"
        chmod +x "${staging}/bin/hew-codegen"
    fi

    if [[ -f "${REPO_DIR}/target/release/libhew_runtime.a" ]]; then
        cp "${REPO_DIR}/target/release/libhew_runtime.a" "${staging}/lib/"
    else
        warn "libhew_runtime.a not found"
    fi

    cp "${REPO_DIR}/std/"*.hew "${staging}/std/" 2>/dev/null || true
    cp "${REPO_DIR}/completions/hew.bash" \
        "${REPO_DIR}/completions/hew.zsh" \
        "${REPO_DIR}/completions/hew.fish" \
        "${REPO_DIR}/completions/adze.bash" \
        "${REPO_DIR}/completions/adze.zsh" \
        "${REPO_DIR}/completions/adze.fish" "${staging}/completions/" 2>/dev/null || true
    cp "${REPO_DIR}/LICENSE-MIT" "${REPO_DIR}/LICENSE-APACHE" \
        "${REPO_DIR}/NOTICE" "${REPO_DIR}/README.md" "${staging}/"

    mkdir -p "${DIST_DIR}"
    tar -czf "${TARBALL_PATH}" -C "${staging_root}" "hew-v${VERSION}-${PLATFORM}"
    rm -rf "${staging_root}"

    _record_ok "tarball" "${TARBALL_PATH}"
}

# ── Step 2: Build .deb ────────────────────────────────────────────────────────
build_debian() {
    _should_build "debian" || {
        _record_skip "debian" "not selected"
        return 0
    }
    _init_tarball_vars
    [[ -f "${TARBALL_PATH}" ]] || {
        _record_fail "debian" "tarball not found: ${TARBALL_PATH}"
        return 0
    }
    _has_cmd docker || {
        _record_skip "debian" "docker not available"
        return 0
    }

    step "Building .deb package"

    local deb_arch
    case "${TARGET_ARCH}" in
    x86_64) deb_arch="amd64" ;;
    aarch64) deb_arch="arm64" ;;
    *)
        _record_fail "debian" "unsupported arch: ${TARGET_ARCH}"
        return 0
        ;;
    esac

    local ctx="${DIST_DIR}/.deb-build-$$"
    _docker_rm "${ctx}"
    mkdir -p "${ctx}/hew-${VERSION}/debian/source"

    # Copy debian packaging files
    cp "${SCRIPT_DIR}/debian/control" \
        "${SCRIPT_DIR}/debian/copyright" \
        "${SCRIPT_DIR}/debian/changelog" \
        "${SCRIPT_DIR}/debian/hew-version" \
        "${SCRIPT_DIR}/debian/rules" \
        "${ctx}/hew-${VERSION}/debian/"
    [[ -f "${SCRIPT_DIR}/debian/source/format" ]] &&
        cp "${SCRIPT_DIR}/debian/source/format" "${ctx}/hew-${VERSION}/debian/source/"
    chmod +x "${ctx}/hew-${VERSION}/debian/rules"

    # Pre-place the tarball so debian/rules skips the download
    cp "${TARBALL_PATH}" "${ctx}/hew-${VERSION}/"

    info "docker" "debian:bookworm -> hew_${VERSION}-1_${deb_arch}.deb"
    if docker run --rm \
        -v "${ctx}:/build" \
        -w "/build/hew-${VERSION}" \
        debian:bookworm bash -c \
        'apt-get update -qq >/dev/null 2>&1 &&
             apt-get install -y -qq dpkg-dev debhelper curl >/dev/null 2>&1 &&
             dpkg-buildpackage -b -nc -us -uc 2>&1'; then
        local deb_name="hew_${VERSION}-1_${deb_arch}.deb"
        if [[ -f "${ctx}/${deb_name}" ]]; then
            cp "${ctx}/${deb_name}" "${DIST_DIR}/"
            _docker_rm "${ctx}"
            _record_ok "debian" "${DIST_DIR}/${deb_name}"
        else
            _docker_rm "${ctx}"
            _record_fail "debian" ".deb not found after build"
        fi
    else
        _docker_rm "${ctx}"
        _record_fail "debian" "dpkg-buildpackage failed"
    fi
}

# ── Step 3: Build .rpm ────────────────────────────────────────────────────────
build_rpm() {
    _should_build "rpm" || {
        _record_skip "rpm" "not selected"
        return 0
    }
    _init_tarball_vars
    [[ -f "${TARBALL_PATH}" ]] || {
        _record_fail "rpm" "tarball not found: ${TARBALL_PATH}"
        return 0
    }
    _has_cmd docker || {
        _record_skip "rpm" "docker not available"
        return 0
    }

    step "Building .rpm package"

    local ctx="${DIST_DIR}/.rpm-build-$$"
    _docker_rm "${ctx}"
    mkdir -p "${ctx}"

    cp "${TARBALL_PATH}" "${ctx}/"
    cp "${SCRIPT_DIR}/rpm/hew.spec" "${ctx}/"

    info "docker" "fedora:41 -> hew-${VERSION}-1.*.${TARGET_ARCH}.rpm"
    if docker run --rm \
        -v "${ctx}:/build" \
        fedora:41 bash -c \
        'dnf install -y rpm-build >/dev/null 2>&1 &&
             mkdir -p /root/rpmbuild/{SOURCES,SPECS,RPMS} &&
             cp /build/'"${TARBALL_NAME}"' /root/rpmbuild/SOURCES/ &&
             cp /build/hew.spec /root/rpmbuild/SPECS/ &&
             rpmbuild -bb /root/rpmbuild/SPECS/hew.spec 2>&1 &&
             cp /root/rpmbuild/RPMS/'"${TARGET_ARCH}"'/*.rpm /build/ 2>/dev/null'; then
        # RPM filename includes dist tag (e.g. .fc41), so glob for it
        local rpm_file
        rpm_file="$(ls "${ctx}"/hew-"${VERSION}"-1.*.rpm 2>/dev/null | head -1)"
        if [[ -n "${rpm_file}" && -f "${rpm_file}" ]]; then
            cp "${rpm_file}" "${DIST_DIR}/"
            _docker_rm "${ctx}"
            _record_ok "rpm" "${DIST_DIR}/$(basename "${rpm_file}")"
        else
            _docker_rm "${ctx}"
            _record_fail "rpm" ".rpm not found after build"
        fi
    else
        _docker_rm "${ctx}"
        _record_fail "rpm" "rpmbuild failed"
    fi
}

# ── Step 4: Build .pkg.tar.zst (Arch Linux) ──────────────────────────────────
build_arch() {
    _should_build "arch" || {
        _record_skip "arch" "not selected"
        return 0
    }
    _init_tarball_vars
    [[ -f "${TARBALL_PATH}" ]] || {
        _record_fail "arch" "tarball not found: ${TARBALL_PATH}"
        return 0
    }
    _has_cmd docker || {
        _record_skip "arch" "docker not available"
        return 0
    }

    step "Building Arch Linux package"

    local ctx="${DIST_DIR}/.arch-build-$$"
    _docker_rm "${ctx}"
    mkdir -p "${ctx}"

    cp "${SCRIPT_DIR}/arch/PKGBUILD" "${ctx}/"
    # Place tarball where makepkg expects it (same dir as PKGBUILD)
    cp "${TARBALL_PATH}" "${ctx}/"

    info "docker" "archlinux:latest -> hew-bin-${VERSION}-1-${TARGET_ARCH}.pkg.tar.zst"
    if docker run --rm \
        -v "${ctx}:/build" \
        archlinux:latest bash -c \
        'pacman -Syu --noconfirm base-devel >/dev/null 2>&1 &&
             useradd -m builder &&
             cp -r /build /home/builder/pkg &&
             chown -R builder:builder /home/builder/pkg &&
             cd /home/builder/pkg &&
             su builder -c "makepkg -s --noconfirm --skipchecksums --skippgpcheck 2>&1" &&
             cp /home/builder/pkg/*.pkg.tar.zst /build/'; then
        local pkg_file
        pkg_file="$(ls "${ctx}"/hew-bin-*.pkg.tar.zst 2>/dev/null | head -1)"
        if [[ -n "${pkg_file}" && -f "${pkg_file}" ]]; then
            cp "${pkg_file}" "${DIST_DIR}/"
            _docker_rm "${ctx}"
            _record_ok "arch" "${DIST_DIR}/$(basename "${pkg_file}")"
        else
            _docker_rm "${ctx}"
            _record_fail "arch" ".pkg.tar.zst not found after build"
        fi
    else
        _docker_rm "${ctx}"
        _record_fail "arch" "makepkg failed"
    fi
}

# ── Step 5: Build .apk (Alpine Linux) ────────────────────────────────────────
build_alpine() {
    _should_build "alpine" || {
        _record_skip "alpine" "not selected"
        return 0
    }
    _init_tarball_vars
    [[ -f "${TARBALL_PATH}" ]] || {
        _record_fail "alpine" "tarball not found: ${TARBALL_PATH}"
        return 0
    }
    _has_cmd docker || {
        _record_skip "alpine" "docker not available"
        return 0
    }

    step "Building Alpine .apk package"

    local ctx="${DIST_DIR}/.alpine-build-$$"
    _docker_rm "${ctx}"
    mkdir -p "${ctx}"

    cp "${SCRIPT_DIR}/alpine/APKBUILD" "${ctx}/"
    # Place tarball where abuild expects it (same dir as APKBUILD)
    cp "${TARBALL_PATH}" "${ctx}/"

    # Compute real SHA-512 checksum so abuild doesn't reject the local file
    local sha512
    sha512="$(sha512sum "${TARBALL_PATH}" | awk '{print $1}')"
    sed -i "s|^sha512sums=.*|sha512sums=\"${sha512}  ${TARBALL_NAME}\"|" "${ctx}/APKBUILD"

    info "docker" "alpine:3.21 -> hew-${VERSION}.apk"
    if docker run --rm \
        -v "${ctx}:/build" \
        alpine:3.21 sh -c '
            apk update >/dev/null 2>&1 &&
            apk add --no-cache alpine-sdk sudo gcompat >/dev/null 2>&1 &&
            adduser -D builder &&
            addgroup builder abuild &&
            echo "builder ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers &&
            mkdir -p /home/builder/pkg &&
            cp /build/* /home/builder/pkg/ &&
            chown -R builder:builder /home/builder/pkg &&
            su builder -c "abuild-keygen -a -n >/dev/null 2>&1" &&
            cp /home/builder/.abuild/*.rsa.pub /etc/apk/keys/ &&
            su builder -c "
                cd /home/builder/pkg &&
                SRCDEST=/home/builder/pkg abuild -r 2>&1
            " &&
            cp /home/builder/packages/*/'"${TARGET_ARCH}"'/*.apk /build/ 2>/dev/null ||
            cp /home/builder/packages/*/*.apk /build/ 2>/dev/null'; then
        local apk_file
        apk_file="$(ls "${ctx}"/hew-*.apk 2>/dev/null | head -1)"
        if [[ -n "${apk_file}" && -f "${apk_file}" ]]; then
            cp "${apk_file}" "${DIST_DIR}/"
            _docker_rm "${ctx}"
            _record_ok "alpine" "${DIST_DIR}/$(basename "${apk_file}")"
        else
            _docker_rm "${ctx}"
            _record_fail "alpine" ".apk not found after build"
        fi
    else
        _docker_rm "${ctx}"
        _record_fail "alpine" "abuild failed"
    fi
}

# ── Step 6: Build Docker image ────────────────────────────────────────────────
build_docker() {
    _should_build "docker" || {
        _record_skip "docker" "not selected"
        return 0
    }
    _init_tarball_vars
    [[ -f "${TARBALL_PATH}" ]] || {
        _record_fail "docker" "tarball not found: ${TARBALL_PATH}"
        return 0
    }
    _has_cmd docker || {
        _record_skip "docker" "docker not available"
        return 0
    }

    step "Building Docker image"

    local ctx="${DIST_DIR}/.docker-build-$$"
    _docker_rm "${ctx}"
    mkdir -p "${ctx}"

    cp "${TARBALL_PATH}" "${ctx}/hew.tar.gz"

    # Generate a Dockerfile that uses the local tarball instead of downloading
    cat >"${ctx}/Dockerfile" <<'DOCKERFILE'
# syntax=docker/dockerfile:1
FROM alpine:3.21 AS fetch
COPY hew.tar.gz /tmp/hew.tar.gz
WORKDIR /tmp/hew-install
RUN tar -xzf /tmp/hew.tar.gz && mv hew-v*-linux-* hew

FROM alpine:3.21
RUN apk add --no-cache \
      gcompat \
      libgcc \
      libstdc++ \
      ca-certificates
COPY --from=fetch /tmp/hew-install/hew/bin/hew           /usr/local/bin/hew
COPY --from=fetch /tmp/hew-install/hew/bin/adze          /usr/local/bin/adze
COPY --from=fetch /tmp/hew-install/hew/bin/hew-codegen   /usr/local/bin/hew-codegen
COPY --from=fetch /tmp/hew-install/hew/lib               /usr/local/lib/hew/
COPY --from=fetch /tmp/hew-install/hew/std               /usr/local/share/hew/std/
ENV HEW_STD=/usr/local/share/hew/std
WORKDIR /work
ENTRYPOINT ["/usr/local/bin/hew"]
CMD ["--help"]
DOCKERFILE

    local tag_version="r.hew.sh/hew:${VERSION}"
    local tag_latest="r.hew.sh/hew:latest"

    info "docker" "${tag_version}"
    if docker build -t "${tag_version}" -t "${tag_latest}" "${ctx}" 2>&1; then
        _docker_rm "${ctx}"
        _record_ok "docker" "${tag_version}, ${tag_latest}"
    else
        _docker_rm "${ctx}"
        _record_fail "docker" "docker build failed"
    fi
}

# ── Step 7: Syntax checks ────────────────────────────────────────────────────
check_shell() {
    _should_build "shell" || {
        _record_skip "shell" "not selected"
        return 0
    }

    step "Syntax-checking install.sh"
    if bash -n "${SCRIPT_DIR}/install.sh" 2>&1; then
        _record_ok "shell" "install.sh syntax OK"
    else
        _record_fail "shell" "install.sh has syntax errors"
    fi
}

check_ps1() {
    _should_build "ps1" || {
        _record_skip "ps1" "not selected"
        return 0
    }
    _has_cmd pwsh || {
        _record_skip "ps1" "pwsh not available"
        return 0
    }

    step "Syntax-checking install.ps1"
    if pwsh -NoProfile -Command \
        "[System.Management.Automation.Language.Parser]::ParseFile('${SCRIPT_DIR}/install.ps1', [ref]\$null, [ref]\$errors); if (\$errors.Count -gt 0) { \$errors | ForEach-Object { Write-Error \$_ }; exit 1 }" 2>&1; then
        _record_ok "ps1" "install.ps1 syntax OK"
    else
        _record_fail "ps1" "install.ps1 has syntax errors"
    fi
}

check_homebrew() {
    _should_build "homebrew" || {
        _record_skip "homebrew" "not selected"
        return 0
    }
    _has_cmd ruby || {
        _record_skip "homebrew" "ruby not available"
        return 0
    }

    step "Syntax-checking homebrew/hew.rb"
    if ruby -c "${SCRIPT_DIR}/homebrew/hew.rb" 2>&1; then
        _record_ok "homebrew" "hew.rb syntax OK"
    else
        _record_fail "homebrew" "hew.rb has syntax errors"
    fi
}

check_nix() {
    _should_build "nix" || {
        _record_skip "nix" "not selected"
        return 0
    }
    _has_cmd nix-instantiate || {
        _record_skip "nix" "nix-instantiate not available"
        return 0
    }

    step "Syntax-checking nix/default.nix"
    if nix-instantiate --parse "${SCRIPT_DIR}/nix/default.nix" >/dev/null 2>&1; then
        _record_ok "nix" "default.nix syntax OK"
    else
        _record_fail "nix" "default.nix has syntax errors"
    fi
}

# ── Summary ───────────────────────────────────────────────────────────────────
print_summary() {
    local ok=0 skip=0 fail=0
    for r in "${RESULTS[@]}"; do
        case "${r%%:*}" in
        ok) ok=$((ok + 1)) ;;
        skip) skip=$((skip + 1)) ;;
        fail) fail=$((fail + 1)) ;;
        esac
    done

    printf "\n${BOLD}Summary${RESET}: ${GREEN}%d ok${RESET}, ${DIM}%d skipped${RESET}, ${RED}%d failed${RESET}\n\n" \
        "$ok" "$skip" "$fail"

    if ((fail > 0)); then
        return 1
    fi
}

# ── Main ──────────────────────────────────────────────────────────────────────
main() {
    _setup_colors
    _parse_args "$@"
    _resolve_version

    step "Hew v${VERSION} — ${TARGET_ARCH}"
    mkdir -p "${DIST_DIR}"

    build_tarball
    build_debian
    build_rpm
    build_arch
    build_alpine
    build_docker
    check_shell
    check_ps1
    check_homebrew
    check_nix

    print_summary
}

main "$@"
