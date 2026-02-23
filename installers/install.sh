#!/usr/bin/env bash
# Hew installer — https://hew.sh
# Usage: curl -fsSL https://install.hew.sh | bash
#        bash install.sh [--version <ver>] [--prefix <dir>] [--help]
set -euo pipefail

GITHUB_ORG="hew-lang"
GITHUB_REPO="hew"
GITHUB_API="https://api.github.com/repos/${GITHUB_ORG}/${GITHUB_REPO}"

# ---------------------------------------------------------------------------
# Color helpers (disabled when stdout is not a tty)
# ---------------------------------------------------------------------------
setup_colors() {
    if [ -t 1 ] && [ -t 2 ]; then
        BOLD='\033[1m'
        DIM='\033[2m'
        GREEN='\033[0;32m'
        CYAN='\033[0;36m'
        YELLOW='\033[0;33m'
        RED='\033[0;31m'
        RESET='\033[0m'
    else
        BOLD='' DIM='' GREEN='' CYAN='' YELLOW='' RED='' RESET=''
    fi
}

info() { printf "  ${GREEN}%s${RESET} %s\n" "$1" "$2"; }
warn() { printf "  ${YELLOW}warning:${RESET} %s\n" "$1" >&2; }
err() {
    printf "\n  ${RED}error:${RESET} %s\n\n" "$1" >&2
    exit 1
}

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
TMPDIR_INSTALL=""
cleanup() {
    if [ -n "$TMPDIR_INSTALL" ] && [ -d "$TMPDIR_INSTALL" ]; then
        rm -rf "$TMPDIR_INSTALL"
    fi
}
trap cleanup EXIT INT TERM

# ---------------------------------------------------------------------------
# Usage
# ---------------------------------------------------------------------------
usage() {
    cat <<EOF
Hew installer

USAGE:
    install.sh [OPTIONS]

OPTIONS:
    --version <ver>    Install a specific version (e.g. 0.1.0). Default: latest
    --prefix  <dir>    Installation directory. Default: \$HEW_HOME or \$HOME/.hew
    --help             Print this help message
EOF
    exit 0
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
REQUESTED_VERSION=""
INSTALL_PREFIX=""

parse_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
        --version)
            [ -z "${2:-}" ] && err "--version requires a value"
            REQUESTED_VERSION="$2"
            shift 2
            ;;
        --prefix)
            [ -z "${2:-}" ] && err "--prefix requires a value"
            INSTALL_PREFIX="$2"
            shift 2
            ;;
        --help | -h)
            usage
            ;;
        *)
            err "unknown option: $1"
            ;;
        esac
    done
}

# ---------------------------------------------------------------------------
# Platform detection
# ---------------------------------------------------------------------------
detect_platform() {
    local uname_s uname_m
    uname_s="$(uname -s)"
    uname_m="$(uname -m)"

    case "$uname_s" in
    Linux*) OS="linux" ;;
    Darwin*) OS="darwin" ;;
    *) err "unsupported operating system: $uname_s" ;;
    esac

    case "$uname_m" in
    x86_64 | amd64) ARCH="x86_64" ;;
    aarch64 | arm64) ARCH="aarch64" ;;
    *) err "unsupported architecture: $uname_m" ;;
    esac
}

# ---------------------------------------------------------------------------
# HTTP helper (curl preferred, wget fallback)
# ---------------------------------------------------------------------------
has_cmd() { command -v "$1" >/dev/null 2>&1; }

http_get() {
    local url="$1"
    if has_cmd curl; then
        curl -fsSL "$url"
    elif has_cmd wget; then
        wget -qO- "$url"
    else
        err "either curl or wget is required"
    fi
}

http_download() {
    local url="$1" dest="$2"
    if has_cmd curl; then
        curl -fsSL -o "$dest" "$url"
    elif has_cmd wget; then
        wget -q -O "$dest" "$url"
    else
        err "either curl or wget is required"
    fi
}

# ---------------------------------------------------------------------------
# Resolve version
# ---------------------------------------------------------------------------
resolve_version() {
    if [ -n "$REQUESTED_VERSION" ]; then
        # Strip leading 'v' if provided
        VERSION="${REQUESTED_VERSION#v}"
    else
        local response
        response="$(http_get "${GITHUB_API}/releases/latest")" ||
            err "failed to fetch latest release from GitHub API"
        VERSION="$(printf '%s' "$response" | grep '"tag_name"' | sed -E 's/.*"tag_name":\s*"v?([^"]+)".*/\1/')"
        [ -z "$VERSION" ] && err "could not determine latest version"
    fi
}

# ---------------------------------------------------------------------------
# SHA-256 helper
# ---------------------------------------------------------------------------
compute_sha256() {
    local file="$1"
    if has_cmd sha256sum; then
        sha256sum "$file" | awk '{print $1}'
    elif has_cmd shasum; then
        shasum -a 256 "$file" | awk '{print $1}'
    else
        err "sha256sum or shasum is required to verify downloads"
    fi
}

# ---------------------------------------------------------------------------
# Banner
# ---------------------------------------------------------------------------
print_banner() {
    printf '%b' "${CYAN}"
    cat <<'ART'

    _   _
   | | | | _____      __
   | |_| |/ _ \ \ /\ / /
   |  _  |  __/\ V  V /
   |_| |_|\___| \_/\_/

ART
    printf '%b' "${RESET}"
}

# ---------------------------------------------------------------------------
# Progress helper
# ---------------------------------------------------------------------------
step_start() { printf "  %-18s" "$1..."; }
step_done() { printf " ${GREEN}done${RESET}\n"; }
step_skip() { printf " ${DIM}skipped${RESET}\n"; }

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main() {
    setup_colors
    parse_args "$@"
    detect_platform

    # Resolve install prefix
    if [ -z "$INSTALL_PREFIX" ]; then
        INSTALL_PREFIX="${HEW_HOME:-$HOME/.hew}"
    fi

    resolve_version

    print_banner
    printf "  ${BOLD}Installing Hew v%s (%s-%s)${RESET}\n\n" "$VERSION" "$OS" "$ARCH"

    # Check if same version is already installed
    local version_file="$INSTALL_PREFIX/.hew-version"
    if [ -f "$version_file" ]; then
        local installed
        installed="$(cat "$version_file")"
        if [ "$installed" = "$VERSION" ]; then
            printf "  Hew v%s is already installed at %s\n\n" "$VERSION" "$INSTALL_PREFIX"
            printf "  To reinstall, remove %s and run again.\n\n" "$version_file"
            exit 0
        fi
    fi

    # Set up temp directory
    TMPDIR_INSTALL="$(mktemp -d)"

    local archive_name="hew-v${VERSION}-${OS}-${ARCH}.tar.gz"
    local checksums_name="hew-v${VERSION}-checksums.txt"
    local base_url="https://github.com/${GITHUB_ORG}/${GITHUB_REPO}/releases/download/v${VERSION}"

    # Download archive
    step_start "Downloading"
    http_download "${base_url}/${archive_name}" "${TMPDIR_INSTALL}/${archive_name}"
    step_done

    # Download and verify checksum
    step_start "Verifying"
    http_download "${base_url}/${checksums_name}" "${TMPDIR_INSTALL}/${checksums_name}"
    local expected actual
    expected="$(grep "${archive_name}" "${TMPDIR_INSTALL}/${checksums_name}" | awk '{print $1}')"
    [ -z "$expected" ] && err "checksum not found for ${archive_name} in ${checksums_name}"
    actual="$(compute_sha256 "${TMPDIR_INSTALL}/${archive_name}")"
    if [ "$expected" != "$actual" ]; then
        err "checksum mismatch\n  expected: ${expected}\n  got:      ${actual}"
    fi
    step_done

    # Extract
    step_start "Extracting"
    tar -xzf "${TMPDIR_INSTALL}/${archive_name}" -C "${TMPDIR_INSTALL}"
    local extracted_dir="${TMPDIR_INSTALL}/hew-v${VERSION}-${OS}-${ARCH}"
    [ -d "$extracted_dir" ] || err "expected directory ${extracted_dir} not found after extraction"

    # Install files
    mkdir -p "${INSTALL_PREFIX}/bin" "${INSTALL_PREFIX}/lib" \
        "${INSTALL_PREFIX}/std" "${INSTALL_PREFIX}/completions"

    for b in hew adze hew-codegen; do
        if [ -f "${extracted_dir}/bin/${b}" ]; then
            cp -f "${extracted_dir}/bin/${b}" "${INSTALL_PREFIX}/bin/${b}"
            chmod +x "${INSTALL_PREFIX}/bin/${b}"
        fi
    done

    cp -f "${extracted_dir}/lib/libhew_runtime.a" "${INSTALL_PREFIX}/lib/libhew_runtime.a"

    # Standard library and completions (best-effort — may not be in older releases)
    if [ -d "${extracted_dir}/std" ]; then
        cp -rf "${extracted_dir}/std/." "${INSTALL_PREFIX}/std/"
    fi
    if [ -d "${extracted_dir}/completions" ]; then
        cp -rf "${extracted_dir}/completions/." "${INSTALL_PREFIX}/completions/"
    fi

    for f in LICENSE-MIT LICENSE-APACHE NOTICE README.md; do
        [ -f "${extracted_dir}/${f}" ] && cp -f "${extracted_dir}/${f}" "${INSTALL_PREFIX}/${f}"
    done

    printf '%s' "$VERSION" >"${INSTALL_PREFIX}/.hew-version"
    step_done

    printf '\n'
    printf "  ${GREEN}Hew was installed to ${BOLD}%s${RESET}\n\n" "$INSTALL_PREFIX"

    # Check if already on PATH
    local bin_dir="${INSTALL_PREFIX}/bin"
    if echo "$PATH" | tr ':' '\n' | grep -qx "$bin_dir" 2>/dev/null; then
        printf "  Run ${CYAN}hew version${RESET} and ${CYAN}adze --version${RESET} to verify the installation.\n\n"
        return
    fi

    # Detect shell and suggest PATH configuration
    local user_shell
    user_shell="$(basename "${SHELL:-/bin/bash}")"

    printf "  To get started, add Hew to your PATH:\n\n"
    printf "    ${CYAN}export HEW_HOME=\"%s\"${RESET}\n" "$INSTALL_PREFIX"
    printf "    ${CYAN}export HEW_STD=\"\$HEW_HOME/std\"${RESET}\n"
    printf "    ${CYAN}export PATH=\"\$HEW_HOME/bin:\$PATH\"${RESET}\n\n"

    local rc_file=""
    case "$user_shell" in
    bash)
        if [ -f "$HOME/.bashrc" ]; then
            rc_file="$HOME/.bashrc"
        elif [ -f "$HOME/.bash_profile" ]; then
            rc_file="$HOME/.bash_profile"
        else
            rc_file="$HOME/.bashrc"
        fi
        printf "  Or add it permanently by running:\n\n"
        printf "    ${DIM}echo 'export HEW_HOME=\"%s\"' >> %s${RESET}\n" "$INSTALL_PREFIX" "$rc_file"
        printf "    ${DIM}echo 'export HEW_STD=\"\$HEW_HOME/std\"' >> %s${RESET}\n" "$rc_file"
        printf "    ${DIM}echo 'export PATH=\"\$HEW_HOME/bin:\$PATH\"' >> %s${RESET}\n\n" "$rc_file"
        ;;
    zsh)
        rc_file="${ZDOTDIR:-$HOME}/.zshrc"
        printf "  Or add it permanently by running:\n\n"
        printf "    ${DIM}echo 'export HEW_HOME=\"%s\"' >> %s${RESET}\n" "$INSTALL_PREFIX" "$rc_file"
        printf "    ${DIM}echo 'export HEW_STD=\"\$HEW_HOME/std\"' >> %s${RESET}\n" "$rc_file"
        printf "    ${DIM}echo 'export PATH=\"\$HEW_HOME/bin:\$PATH\"' >> %s${RESET}\n\n" "$rc_file"
        ;;
    fish)
        rc_file="$HOME/.config/fish/config.fish"
        printf "  Or add it permanently by running:\n\n"
        printf "    ${DIM}set -Ux HEW_HOME %s${RESET}\n" "$INSTALL_PREFIX"
        printf "    ${DIM}set -Ux HEW_STD \$HEW_HOME/std${RESET}\n"
        printf "    ${DIM}fish_add_path \$HEW_HOME/bin${RESET}\n\n"
        ;;
    *)
        printf "  Add the above lines to your shell's configuration file.\n\n"
        ;;
    esac
}

main "$@"
