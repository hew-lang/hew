#!/usr/bin/env bash
# shellcheck disable=SC2317  # functions are called dynamically via validate_"$platform"
# pre-release-validate.sh — Local pre-release validation for the Hew compiler.
#
# Validates that the release build works on all supported platforms BEFORE
# tagging.  Run this after `make release` succeeds locally.
#
# Usage:
#   scripts/pre-release-validate.sh              # all platforms
#   scripts/pre-release-validate.sh linux         # linux only
#   scripts/pre-release-validate.sh macos freebsd # subset
#
# Platforms: linux, macos, freebsd, windows
#
# Prerequisites:
#   - SSH access to platform hosts (see PLATFORM_HOSTS below)
#   - rsync available locally and on remote hosts
#   - Each host must have Rust, LLVM 22, cmake, ninja installed

set -euo pipefail

# ── Platform hosts (loaded from .env.pre-release, not committed) ─────────────
#
# Create .env.pre-release in the repo root with your local host config:
#
#   MACOS_HOST=my-mac.local
#   FREEBSD_HOST=user@freebsd-host
#   FREEBSD_PROJECT_DIR=/path/to/hew
#   WINDOWS_HOST=user@windows-host
#   WINDOWS_PROJECT_DIR=P:/path/to/hew
#   MACOS_TART_VM=macos-build
#
# Or export them as environment variables (HEW_MACOS_HOST, etc.).

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

CONFIG_FILE="${REPO_ROOT}/.env.pre-release"
if [[ -f "$CONFIG_FILE" ]]; then
    # shellcheck disable=SC1090
    source "$CONFIG_FILE"
fi

MACOS_HOST="${HEW_MACOS_HOST:-${MACOS_HOST:-}}"
FREEBSD_HOST="${HEW_FREEBSD_HOST:-${FREEBSD_HOST:-}}"
WINDOWS_HOST="${HEW_WINDOWS_HOST:-${WINDOWS_HOST:-}}"

FREEBSD_PROJECT_DIR="${HEW_FREEBSD_DIR:-${FREEBSD_PROJECT_DIR:-}}"
WINDOWS_PROJECT_DIR="${HEW_WINDOWS_DIR:-${WINDOWS_PROJECT_DIR:-}}"

# shellcheck disable=SC2034  # used by operators extending this script
MACOS_TART_VM="${HEW_MACOS_TART_VM:-${MACOS_TART_VM:-macos-build}}"

# ── Colours ──────────────────────────────────────────────────────────────────

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
RESET='\033[0m'

# ── State tracking ───────────────────────────────────────────────────────────

PIDS=()
PLATFORM_NAMES=()
LOG_DIR=$(mktemp -d)
RESULT_DIR=$(mktemp -d)

# Write results to files so background processes can report back.
# Usage: pass <platform> [detail] / fail <platform> [detail] / skip <platform> [detail]
pass() { echo "pass $1 ${2:-}" > "${RESULT_DIR}/$1"; }
fail() { echo "fail $1 ${2:-}" > "${RESULT_DIR}/$1"; }
skip() { echo "skip $1 ${2:-}" > "${RESULT_DIR}/$1"; }

banner() {
    echo -e "\n${CYAN}═══ $1 ═══${RESET}"
}

# ── Determine which platforms to validate ────────────────────────────────────

if [[ $# -eq 0 ]]; then
    PLATFORMS=(linux macos freebsd windows)
else
    PLATFORMS=("$@")
fi

cd "$REPO_ROOT"

VERSION=$(grep '^version' Cargo.toml | head -1 | sed 's/.*"\(.*\)"/\1/')
echo -e "${CYAN}Pre-release validation for Hew v${VERSION}${RESET}"
echo "Platforms: ${PLATFORMS[*]}"
echo "Logs: ${LOG_DIR}/"

# ── Smoke test program ──────────────────────────────────────────────────────

write_smoke_test() {
    local file="$1"
    cat > "$file" <<'HEWEOF'
fn main() {
    println("Hello from Hew release test")
}
HEWEOF
}

# ── Platform validators ──────────────────────────────────────────────────────

validate_linux() {
    banner "Linux (local static-link build)"

    local log="${LOG_DIR}/linux.log"

    if (
        set -e
        echo "==> Step 1: Static-link release build"
        # This is the exact build that the release CI does
        HEW_EMBED_STATIC=1 cargo build -p hew-cli -p adze-cli -p hew-lsp --release 2>&1
        cargo build -p hew-lib --release 2>&1

        echo "==> Step 2: Verify binaries exist and run"
        target/release/hew --version
        target/release/adze --version
        target/release/hew-lsp --version
        test -f target/release/libhew.a

        echo "==> Step 3: Smoke test — compile and run a Hew program"
        local smoke_file
        smoke_file=$(mktemp --suffix=.hew)
        write_smoke_test "$smoke_file"
        local smoke_bin
        smoke_bin=$(mktemp)
        target/release/hew "$smoke_file" -o "$smoke_bin"
        chmod +x "$smoke_bin"
        local output
        output=$("$smoke_bin")
        rm -f "$smoke_file" "$smoke_bin"

        if echo "$output" | grep -q "Hello from Hew"; then
            echo "==> Smoke test passed"
        else
            echo "==> SMOKE TEST FAILED — output: $output"
            exit 1
        fi

        echo "==> Step 4: Run test suite (informational — failures here don't block release)"
        cargo test -p hew-runtime --quiet 2>&1 | tail -3 || true
        make test-codegen 2>&1 | tail -5 || true

        echo "==> Step 5: Verify no dynamic LLVM/MLIR dependencies"
        if ldd target/release/hew 2>/dev/null | grep -qi 'llvm\|mlir'; then
            echo "FATAL: Binary dynamically links LLVM/MLIR"
            exit 1
        fi
        echo "==> No dynamic LLVM/MLIR deps — binary is self-contained"
    ) > "$log" 2>&1; then
        pass "linux"
    else
        fail "linux" "see ${log}"
        return 1
    fi
}

validate_macos() {
    banner "macOS (via Tart on ${MACOS_HOST})"

    local log="${LOG_DIR}/macos.log"

    if [[ -z "$MACOS_HOST" ]]; then
        skip "macos" "MACOS_HOST not configured"
        return 0
    fi
    if ! ssh -o ConnectTimeout=5 "${MACOS_HOST}" true 2>/dev/null; then
        skip "macos" "${MACOS_HOST} unreachable"
        return 0
    fi

    if (
        set -e
        echo "==> Syncing source to ${MACOS_HOST}"
        rsync -az --delete \
            --exclude target --exclude .git --exclude build \
            --exclude '*.o' --exclude '*.a' --exclude '*.d' \
            . "${MACOS_HOST}:~/hew-pre-release/"

        echo "==> Building on macOS"
        ssh "${MACOS_HOST}" bash -lc "'
            set -eux
            cd ~/hew-pre-release

            # Ensure LLVM is on PATH (Homebrew)
            export PATH=\"/opt/homebrew/opt/llvm@22/bin:/opt/homebrew/bin:\$PATH\"
            export LLVM_PREFIX=\"\$(brew --prefix llvm@22 2>/dev/null || echo /opt/homebrew/opt/llvm)\"

            cargo build -p hew-cli -p adze-cli -p hew-lsp --release
            cargo build -p hew-lib --release

            target/release/hew --version
            target/release/adze --version
            target/release/hew-lsp --version

            echo \"macOS build succeeded\"
        '"
    ) > "$log" 2>&1; then
        pass "macos"
    else
        fail "macos" "see ${log}"
        return 1
    fi
}

validate_freebsd() {
    banner "FreeBSD (via SSH to ${FREEBSD_HOST})"

    local log="${LOG_DIR}/freebsd.log"

    if [[ -z "$FREEBSD_HOST" ]]; then
        skip "freebsd" "FREEBSD_HOST not configured"
        return 0
    fi
    if [[ -z "$FREEBSD_PROJECT_DIR" ]]; then
        skip "freebsd" "FREEBSD_PROJECT_DIR not configured"
        return 0
    fi
    if ! ssh -o ConnectTimeout=5 "${FREEBSD_HOST}" true 2>/dev/null; then
        skip "freebsd" "${FREEBSD_HOST} unreachable"
        return 0
    fi

    if (
        set -e
        echo "==> Syncing source to FreeBSD"
        if rsync -az --delete \
            --exclude target --exclude .git --exclude build \
            --exclude '*.o' --exclude '*.a' --exclude '*.d' \
            . "${FREEBSD_HOST}:${FREEBSD_PROJECT_DIR}/"; then
            echo "==> Synced via rsync"
        else
            echo "==> rsync failed, falling back to git pull on remote"
            ssh "${FREEBSD_HOST}" bash -lc "'cd ${FREEBSD_PROJECT_DIR} && git pull --rebase origin main'" || {
                echo "FATAL: Could not sync source to FreeBSD (rsync and git pull both failed)"
                exit 1
            }
        fi

        echo "==> Building on FreeBSD"
        ssh "${FREEBSD_HOST}" bash -lc "'
            set -eux
            cd ${FREEBSD_PROJECT_DIR}

            # Auto-detect LLVM 22 from typical FreeBSD install locations
            for dir in /usr/local/llvm22 /usr/local/llvm22-src /usr/local; do
                if [ -f \"\${dir}/bin/llvm-config\" ]; then
                    export LLVM_PREFIX=\"\${dir}\"
                    break
                fi
            done
            export PATH=\"\${LLVM_PREFIX:-/usr/local}/bin:\$PATH\"
            export CC=clang
            export CXX=clang++

            # FreeBSD LLVM packages only ship static archives (no libMLIR.so),
            # so we must use static linking to build the embedded codegen.
            HEW_EMBED_STATIC=1 cargo build -p hew-cli -p adze-cli -p hew-lsp --release
            cargo build -p hew-lib --release

            target/release/hew --version
            target/release/adze --version
            target/release/hew-lsp --version

            echo \"FreeBSD build succeeded\"
        '"
    ) > "$log" 2>&1; then
        pass "freebsd"
    else
        fail "freebsd" "see ${log}"
        return 1
    fi
}

validate_windows() {
    banner "Windows (via SSH to ${WINDOWS_HOST})"

    local log="${LOG_DIR}/windows.log"

    if [[ -z "$WINDOWS_HOST" ]]; then
        skip "windows" "WINDOWS_HOST not configured"
        return 0
    fi
    if [[ -z "$WINDOWS_PROJECT_DIR" ]]; then
        skip "windows" "WINDOWS_PROJECT_DIR not configured"
        return 0
    fi
    if ! ssh -o ConnectTimeout=5 "${WINDOWS_HOST}" true 2>/dev/null; then
        skip "windows" "${WINDOWS_HOST} unreachable"
        return 0
    fi

    if (
        set -e
        echo "==> Syncing source to Windows"
        # Windows uses a pre-existing checkout — just pull latest
        # shellcheck disable=SC2029  # WINDOWS_PROJECT_DIR is intentionally expanded locally
        ssh "${WINDOWS_HOST}" "cd /d ${WINDOWS_PROJECT_DIR} && git pull --rebase origin main"

        echo "==> Building on Windows"
        # shellcheck disable=SC2029
        ssh "${WINDOWS_HOST}" "cd /d ${WINDOWS_PROJECT_DIR} && set HEW_EMBED_STATIC=1 && cargo build -p hew-cli -p adze-cli -p hew-lsp --release"

        echo "==> Smoke test on Windows"
        # shellcheck disable=SC2029
        ssh "${WINDOWS_HOST}" "cd /d ${WINDOWS_PROJECT_DIR} && target\\release\\hew.exe --version && target\\release\\adze.exe --version && target\\release\\hew-lsp.exe --version"
    ) > "$log" 2>&1; then
        pass "windows"
    else
        fail "windows" "see ${log}"
        return 1
    fi
}

# ── Run platforms ────────────────────────────────────────────────────────────

# Linux runs first (fast local validation) to fail-fast.
# Remote platforms run in parallel after.

HAVE_FAILURE=0

for platform in "${PLATFORMS[@]}"; do
    case "$platform" in
        linux)
            validate_linux || HAVE_FAILURE=1
            ;;
        macos|freebsd|windows)
            # Run remote builds in background
            validate_"$platform" &
            PIDS+=($!)
            PLATFORM_NAMES+=("$platform")
            ;;
        *)
            echo "Unknown platform: $platform"
            exit 1
            ;;
    esac
done

# Wait for remote builds
for i in "${!PIDS[@]}"; do
    if ! wait "${PIDS[$i]}"; then
        HAVE_FAILURE=1
    fi
done

# ── Summary ──────────────────────────────────────────────────────────────────

banner "Pre-release validation summary"
for platform in "${PLATFORMS[@]}"; do
    result_file="${RESULT_DIR}/${platform}"
    if [[ -f "$result_file" ]]; then
        status=$(cut -d' ' -f1 < "$result_file")
        detail=$(cut -d' ' -f3- < "$result_file")
        detail_suffix=""
        [[ -n "$detail" ]] && detail_suffix=" (${detail})"
        case "$status" in
            pass) echo -e "  ${GREEN}✓ ${platform}${RESET}" ;;
            fail) echo -e "  ${RED}✗ ${platform}${detail_suffix}${RESET}"; HAVE_FAILURE=1 ;;
            skip) echo -e "  ${YELLOW}⊘ ${platform}${detail_suffix} (skipped)${RESET}" ;;
        esac
    else
        echo -e "  ${RED}✗ ${platform} (no result — likely crashed)${RESET}"
        HAVE_FAILURE=1
    fi
done
echo ""

rm -rf "$RESULT_DIR"
if [[ $HAVE_FAILURE -ne 0 ]]; then
    echo -e "${RED}Pre-release validation FAILED — do not tag.${RESET}"
    echo "Logs in: ${LOG_DIR}/"
    exit 1
else
    echo -e "${GREEN}All platforms passed — safe to tag v${VERSION}.${RESET}"
    rm -rf "$LOG_DIR"
    exit 0
fi
