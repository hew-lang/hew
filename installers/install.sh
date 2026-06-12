#!/usr/bin/env bash
# Hew installer — https://hew.sh
# Usage: curl -fsSL https://hew.sh/install | bash
#        fetch -o - https://hew.sh/install | bash   (FreeBSD — bash from ports/pkg)
#        irm https://hew.sh/install.ps1 | iex  (Windows PowerShell)
#        bash install.sh [--version <ver>] [--prefix <dir>] [--help]
set -euo pipefail

GITHUB_ORG="hew-lang"
GITHUB_REPO="hew"
GITHUB_API="https://api.github.com/repos/${GITHUB_ORG}/${GITHUB_REPO}"

# ---------------------------------------------------------------------------
# Colour helpers (disabled when stdout is not a tty)
# ---------------------------------------------------------------------------
setup_colours() {
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

info() { printf "  %b%s%b %s\n" "${GREEN}" "$1" "${RESET}" "$2"; }
warn() { printf "  %bwarning:%b %s\n" "${YELLOW}" "${RESET}" "$1" >&2; }
err() {
    printf "\n  %berror:%b %s\n\n" "${RED}" "${RESET}" "$1" >&2
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
    --version <ver>    Install a specific version (e.g. 0.1.3). Default: latest
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
    FreeBSD*) OS="freebsd" ;;
    *) err "unsupported operating system: $uname_s" ;;
    esac

    case "$uname_m" in
    x86_64 | amd64) ARCH="x86_64" ;;
    aarch64 | arm64)
        if [ "$OS" = "freebsd" ]; then
            err "FreeBSD prebuilt releases are x86_64 only today; build from source or use a x86_64 host"
        fi
        ARCH="aarch64"
        ;;
    *) err "unsupported architecture: $uname_m" ;;
    esac
}

# FreeBSD base ships fetch(1) but not curl/wget/bash. The script is bash; point
# users at pkg once, then the fetch pipe works without curl.
require_bash() {
    if [ "$(uname -s)" != "FreeBSD" ]; then
        return 0
    fi
    if has_cmd bash; then
        return 0
    fi
    err "FreeBSD base does not include bash.

  pkg install -y bash
  fetch -o - https://hew.sh/install | bash"
}

# ---------------------------------------------------------------------------
# HTTP helper (curl preferred, wget fallback)
# ---------------------------------------------------------------------------
has_cmd() { command -v "$1" >/dev/null 2>&1; }

http_get() {
    local url="$1"
    if has_cmd curl; then
        curl -fsSL "$url"
    elif has_cmd fetch; then
        fetch -o - "$url"
    elif has_cmd wget; then
        wget -qO- "$url"
    else
        err "curl, fetch, or wget is required"
    fi
}

http_download() {
    local url="$1" dest="$2"
    if has_cmd curl; then
        curl -fsSL -o "$dest" "$url"
    elif has_cmd fetch; then
        fetch -o "$dest" "$url"
    elif has_cmd wget; then
        wget -q -O "$dest" "$url"
    else
        err "curl, fetch, or wget is required"
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
        if [ -z "$VERSION" ]; then
            err "could not determine latest version"
        fi
    fi
}

# ---------------------------------------------------------------------------
# SHA-256 helper
# ---------------------------------------------------------------------------
compute_sha256() {
    local file="$1"
    if has_cmd sha256sum; then
        sha256sum "$file" | awk '{print $1}'
    elif has_cmd sha256; then
        sha256 -q "$file"
    elif has_cmd shasum; then
        shasum -a 256 "$file" | awk '{print $1}'
    else
        err "sha256sum, sha256, or shasum is required to verify downloads"
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
step_done() { printf " %bdone%b\n" "${GREEN}" "${RESET}"; }
step_skip() { printf " %bskipped%b\n" "${DIM}" "${RESET}"; }

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main() {
    setup_colours
    require_bash
    parse_args "$@"
    detect_platform

    # Resolve install prefix
    if [ -z "$INSTALL_PREFIX" ]; then
        INSTALL_PREFIX="${HEW_HOME:-$HOME/.hew}"
    fi

    resolve_version

    print_banner
    printf "  %bInstalling Hew v%s (%s-%s)%b\n\n" "${BOLD}" "$VERSION" "$OS" "$ARCH" "${RESET}"

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
    if [ -z "$expected" ]; then
        err "checksum not found for ${archive_name} in ${checksums_name}"
    fi
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

    for b in hew adze hew-lsp; do
        if [ -f "${extracted_dir}/bin/${b}" ]; then
            cp -f "${extracted_dir}/bin/${b}" "${INSTALL_PREFIX}/bin/${b}"
            chmod +x "${INSTALL_PREFIX}/bin/${b}"
        fi
    done

    # Copy all of lib/ — preserves libhew.a at both the flat path and any
    # triple-specific subdirectory (e.g. lib/aarch64-apple-darwin/libhew.a)
    # that hew uses when resolving the linker search path.
    cp -rf "${extracted_dir}/lib/." "${INSTALL_PREFIX}/lib/"

    # Standard library (best-effort — may not be in older releases)
    if [ -d "${extracted_dir}/std" ]; then
        cp -rf "${extracted_dir}/std/." "${INSTALL_PREFIX}/std/"
    fi

    # Generate shell completions from the installed binaries
    for shell in bash zsh fish; do
        "${INSTALL_PREFIX}/bin/hew" completions "${shell}" \
            > "${INSTALL_PREFIX}/completions/hew.${shell}" 2>/dev/null || true
        "${INSTALL_PREFIX}/bin/adze" completions "${shell}" \
            > "${INSTALL_PREFIX}/completions/adze.${shell}" 2>/dev/null || true
    done

    for f in LICENSE-MIT LICENSE-APACHE NOTICE README.md; do
        [ -f "${extracted_dir}/${f}" ] && cp -f "${extracted_dir}/${f}" "${INSTALL_PREFIX}/${f}"
    done

    printf '%s' "$VERSION" >"${INSTALL_PREFIX}/.hew-version"
    step_done

    printf '\n'
    printf "  %bHew was installed to %b%s%b\n\n" "${GREEN}" "${BOLD}" "$INSTALL_PREFIX" "${RESET}"

    # Check if already on PATH
    local bin_dir="${INSTALL_PREFIX}/bin"
    if echo "$PATH" | tr ':' '\n' | grep -qx "$bin_dir" 2>/dev/null; then
        printf "  Run %bhew version%b and %badze --version%b to verify the installation.\n\n" "${CYAN}" "${RESET}" "${CYAN}" "${RESET}"
        return
    fi

    # Detect shell and suggest PATH configuration
    local user_shell
    user_shell="$(basename "${SHELL:-/bin/bash}")"

    printf "  To get started, add Hew to your PATH:\n\n"
    printf "    %bexport HEW_HOME=\"%s\"%b\n" "${CYAN}" "$INSTALL_PREFIX" "${RESET}"
    printf "    %bexport HEW_STD=\"\$HEW_HOME/std\"%b\n" "${CYAN}" "${RESET}"
    printf "    %bexport PATH=\"\$HEW_HOME/bin:\$PATH\"%b\n\n" "${CYAN}" "${RESET}"

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
        printf "    %becho 'export HEW_HOME=\"%s\"' >> %s%b\n" "${DIM}" "$INSTALL_PREFIX" "$rc_file" "${RESET}"
        printf "    %becho 'export HEW_STD=\"\$HEW_HOME/std\"' >> %s%b\n" "${DIM}" "$rc_file" "${RESET}"
        printf "    %becho 'export PATH=\"\$HEW_HOME/bin:\$PATH\"' >> %s%b\n\n" "${DIM}" "$rc_file" "${RESET}"
        ;;
    zsh)
        rc_file="${ZDOTDIR:-$HOME}/.zshrc"
        printf "  Or add it permanently by running:\n\n"
        printf "    %becho 'export HEW_HOME=\"%s\"' >> %s%b\n" "${DIM}" "$INSTALL_PREFIX" "$rc_file" "${RESET}"
        printf "    %becho 'export HEW_STD=\"\$HEW_HOME/std\"' >> %s%b\n" "${DIM}" "$rc_file" "${RESET}"
        printf "    %becho 'export PATH=\"\$HEW_HOME/bin:\$PATH\"' >> %s%b\n\n" "${DIM}" "$rc_file" "${RESET}"
        ;;
    fish)
        rc_file="$HOME/.config/fish/config.fish"
        printf "  Or add it permanently by running:\n\n"
        printf "    %bset -Ux HEW_HOME %s%b\n" "${DIM}" "$INSTALL_PREFIX" "${RESET}"
        printf "    %bset -Ux HEW_STD \$HEW_HOME/std%b\n" "${DIM}" "${RESET}"
        printf "    %bfish_add_path \$HEW_HOME/bin%b\n\n" "${DIM}" "${RESET}"
        ;;
    *)
        printf "  Add the above lines to your shell's configuration file.\n\n"
        ;;
    esac
}

main "$@"
