#!/bin/sh
# Hew installer — https://hew.sh
# Usage: curl -fsSL https://hew.sh/install | sh       (Linux / macOS)
#        fetch -o - https://hew.sh/install | sh        (FreeBSD — no extra packages needed)
#        wget -qO- https://hew.sh/install | sh         (wget fallback)
#        irm https://hew.sh/install.ps1 | iex          (Windows PowerShell)
#        sh install.sh [--version <ver>] [--prefix <dir>] [--help]
set -eu

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
sh install.sh [OPTIONS]

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
    case "$(uname -s)" in
    Linux*)   OS="linux" ;;
    Darwin*)  OS="darwin" ;;
    FreeBSD*) OS="freebsd" ;;
    *)        err "unsupported operating system: $(uname -s)" ;;
    esac

    case "$(uname -m)" in
    x86_64 | amd64) ARCH="x86_64" ;;
    aarch64 | arm64)
        if [ "$OS" = "freebsd" ]; then
            err "FreeBSD prebuilt releases are x86_64 only today; build from source or use a x86_64 host"
        fi
        ARCH="aarch64"
        ;;
    *) err "unsupported architecture: $(uname -m)" ;;
    esac
}

# ---------------------------------------------------------------------------
# HTTP helper — prefers fetch (FreeBSD base), then curl, then wget
# ---------------------------------------------------------------------------
has_cmd() { command -v "$1" >/dev/null 2>&1; }

http_get() {
    if has_cmd fetch; then
        fetch -o - "$1"
    elif has_cmd curl; then
        curl -fsSL "$1"
    elif has_cmd wget; then
        wget -qO- "$1"
    else
        err "need one of: fetch, curl, wget"
    fi
}

http_download() {
    if has_cmd fetch; then
        fetch -o "$2" "$1"
    elif has_cmd curl; then
        curl -fsSL -o "$2" "$1"
    elif has_cmd wget; then
        wget -q -O "$2" "$1"
    else
        err "need one of: fetch, curl, wget"
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
        _rv_response="$(http_get "${GITHUB_API}/releases/latest")" ||
            err "failed to fetch latest release from GitHub API"
        _rv_raw="$(printf '%s' "$_rv_response" | grep '"tag_name"' | sed 's/.*"tag_name":[[:space:]]*"//;s/".*//')"
        VERSION="${_rv_raw#v}"
        if [ -z "$VERSION" ]; then
            err "could not determine latest version"
        fi
    fi
}

# ---------------------------------------------------------------------------
# SHA-256 helper
# ---------------------------------------------------------------------------
compute_sha256() {
    if has_cmd sha256sum; then
        sha256sum "$1" | awk '{print $1}'
    elif has_cmd sha256; then
        sha256 -q "$1"
    elif has_cmd shasum; then
        shasum -a 256 "$1" | awk '{print $1}'
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
    version_file="$INSTALL_PREFIX/.hew-version"
    if [ -f "$version_file" ]; then
        installed="$(cat "$version_file")"
        if [ "$installed" = "$VERSION" ]; then
            printf "  Hew v%s is already installed at %s\n\n" "$VERSION" "$INSTALL_PREFIX"
            printf "  To reinstall, remove %s and run again.\n\n" "$version_file"
            exit 0
        fi
    fi

    # Set up temp directory
    TMPDIR_INSTALL="$(mktemp -d)"

    archive_name="hew-v${VERSION}-${OS}-${ARCH}.tar.gz"
    checksums_name="hew-v${VERSION}-checksums.txt"
    base_url="https://github.com/${GITHUB_ORG}/${GITHUB_REPO}/releases/download/v${VERSION}"

    # Download archive
    step_start "Downloading"
    http_download "${base_url}/${archive_name}" "${TMPDIR_INSTALL}/${archive_name}"
    step_done

    # Download and verify checksum
    step_start "Verifying"
    http_download "${base_url}/${checksums_name}" "${TMPDIR_INSTALL}/${checksums_name}"
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
    extracted_dir="${TMPDIR_INSTALL}/hew-v${VERSION}-${OS}-${ARCH}"
    [ -d "$extracted_dir" ] || err "expected directory ${extracted_dir} not found after extraction"

    # Install files
    mkdir -p "${INSTALL_PREFIX}/bin" "${INSTALL_PREFIX}/lib" \
        "${INSTALL_PREFIX}/std" "${INSTALL_PREFIX}/completions"

    for b in hew adze hew-lsp hew-observe; do
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
    bin_dir="${INSTALL_PREFIX}/bin"
    if echo "$PATH" | tr ':' '\n' | grep -qx "$bin_dir" 2>/dev/null; then
        printf "  Run %bhew version%b and %badze --version%b to verify the installation.\n\n" "${CYAN}" "${RESET}" "${CYAN}" "${RESET}"
        return
    fi

    # Detect shell and suggest PATH configuration
    user_shell="$(basename "${SHELL:-/bin/sh}")"

    printf "  To get started, add Hew to your PATH:\n\n"
    printf "    %bexport HEW_HOME=\"%s\"%b\n" "${CYAN}" "$INSTALL_PREFIX" "${RESET}"
    printf "    %bexport HEW_STD=\"\$HEW_HOME/std\"%b\n" "${CYAN}" "${RESET}"
    printf "    %bexport PATH=\"\$HEW_HOME/bin:\$PATH\"%b\n\n" "${CYAN}" "${RESET}"

    rc_file=""
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
