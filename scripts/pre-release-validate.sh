#!/usr/bin/env bash
# shellcheck disable=SC2317,SC2329  # functions are called dynamically via validate_"$platform"
# pre-release-validate.sh — Local pre-release validation for the Hew compiler.
#
# Validates that the release build works on all supported platforms BEFORE
# tagging.  Run this after `make release` succeeds locally.
#
# Usage:
#   scripts/pre-release-validate.sh                    # all platforms
#   scripts/pre-release-validate.sh linux               # linux only
#   scripts/pre-release-validate.sh linux linux-aarch64 # local + arm64 remote
#   scripts/pre-release-validate.sh macos freebsd       # subset
#
# Platforms: linux, linux-aarch64, macos, freebsd, windows
#
# Prerequisites:
#   - SSH access to platform hosts (see PLATFORM_HOSTS below)
#   - rsync available locally and on remote hosts
#   - Each host must have Rust and LLVM 22 installed
#   - Linux aarch64 remote validation requires Ubuntu 24.04 arm64 + sudo so the
#     script can provision LLVM 22 from apt.llvm.org/noble
#   - timeout (Linux) or gtimeout (macOS/coreutils) for bounded execution
#
# Timeout overrides (seconds):
#   HEW_TIMEOUT_SSH_CHECK, HEW_TIMEOUT_SYNC, HEW_TIMEOUT_LOCAL_BUILD,
#   HEW_TIMEOUT_REMOTE_BUILD, HEW_TIMEOUT_SMOKE, HEW_TIMEOUT_TEST

set -euo pipefail

# ── Platform hosts (loaded from .env.pre-release, not committed) ─────────────
#
# Create .env.pre-release in the repo root with your local host config:
#
#   MACOS_HOST=my-mac.local
#   LINUX_AARCH64_HOST=user@ubuntu-24-arm-host
#   FREEBSD_HOST=user@freebsd-host
#   WINDOWS_HOST=user@windows-host
#   MACOS_TART_VM=macos-build
#   HEW_MACOS_LLVM_PREFIX=/opt/homebrew/opt/llvm@22
#   HEW_WINDOWS_LLVM_PREFIX='C:\llvm-22'
#   HEW_WINDOWS_STAGE_ROOT='P:/hew-pre-release-stages'
#   HEW_WINDOWS_CANDIDATE_ARCHIVE='/path/to/target/hew-windows-candidate.tar.gz'
#   HEW_WINDOWS_CC=cl
#   HEW_WINDOWS_CXX=cl
#
# Or export them as environment variables (HEW_MACOS_HOST, etc.).

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

CONFIG_FILE="${REPO_ROOT}/.env.pre-release"
if [[ -f "$CONFIG_FILE" ]]; then
    # shellcheck disable=SC1090
    source "$CONFIG_FILE"
fi

MACOS_HOST="${HEW_MACOS_HOST:-${MACOS_HOST:-}}"
LINUX_AARCH64_HOST="${HEW_LINUX_AARCH64_HOST:-${LINUX_AARCH64_HOST:-}}"
FREEBSD_HOST="${HEW_FREEBSD_HOST:-${FREEBSD_HOST:-}}"
WINDOWS_HOST="${HEW_WINDOWS_HOST:-${WINDOWS_HOST:-}}"
MACOS_LLVM_PREFIX="${HEW_MACOS_LLVM_PREFIX:-${MACOS_LLVM_PREFIX:-}}"

WINDOWS_LLVM_PREFIX="${HEW_WINDOWS_LLVM_PREFIX:-C:\\llvm-22}"
WINDOWS_LLVM_CONFIG="${HEW_WINDOWS_LLVM_CONFIG:-${WINDOWS_LLVM_PREFIX}\\lib\\cmake\\llvm\\LLVMConfig.cmake}"
WINDOWS_STAGE_ROOT="${HEW_WINDOWS_STAGE_ROOT:-${WINDOWS_STAGE_ROOT:-}}"
WINDOWS_CANDIDATE_ARCHIVE="${HEW_WINDOWS_CANDIDATE_ARCHIVE:-${WINDOWS_CANDIDATE_ARCHIVE:-}}"
# Normalize the optional root before validating/interpolating it.
WINDOWS_STAGE_ROOT="${WINDOWS_STAGE_ROOT//\\//}"
WINDOWS_CC="${HEW_WINDOWS_CC:-cl}"
WINDOWS_CXX="${HEW_WINDOWS_CXX:-cl}"

# shellcheck disable=SC2034  # used by operators extending this script
MACOS_TART_VM="${HEW_MACOS_TART_VM:-${MACOS_TART_VM:-macos-build}}"

# ── Colours ──────────────────────────────────────────────────────────────────

RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
RESET='\033[0m'

# ── Timeout defaults ─────────────────────────────────────────────────────────

SSH_CHECK_TIMEOUT="${HEW_TIMEOUT_SSH_CHECK:-15}"
SYNC_TIMEOUT="${HEW_TIMEOUT_SYNC:-300}"
# Removing a staged Windows release tree includes Cargo's registry/cache and
# release/LTO outputs, so size cleanup like transport rather than a reachability
# probe. Keep this independently tunable for slower Windows filesystems.
REMOTE_CLEANUP_TIMEOUT="${HEW_TIMEOUT_REMOTE_CLEANUP:-${SYNC_TIMEOUT}}"
LOCAL_BUILD_TIMEOUT="${HEW_TIMEOUT_LOCAL_BUILD:-1800}"
REMOTE_BUILD_TIMEOUT="${HEW_TIMEOUT_REMOTE_BUILD:-1800}"
SMOKE_TIMEOUT="${HEW_TIMEOUT_SMOKE:-120}"
TEST_TIMEOUT="${HEW_TIMEOUT_TEST:-900}"

# shellcheck source=scripts/lib/timeout.sh
# shellcheck disable=SC1091
source "${REPO_ROOT}/scripts/lib/timeout.sh"
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck disable=SC1091
source "${REPO_ROOT}/scripts/lib/cargo-output-dir.sh"

# ── State tracking ───────────────────────────────────────────────────────────

PIDS=()
PLATFORM_NAMES=()
LOG_DIR=$(mktemp -d)
RESULT_DIR=$(mktemp -d)

# Write results to files so background processes can report back.
# Every requested platform is authoritative: absent configuration and
# unreachable hosts are failures. Narrow PLATFORMS explicitly when a host is
# intentionally outside the current validation run.
pass() { echo "pass $1 ${2:-}" >"${RESULT_DIR}/$1"; }
fail() { echo "fail $1 ${2:-}" >"${RESULT_DIR}/$1"; }

banner() {
    echo -e "\n${CYAN}═══ $1 ═══${RESET}"
}

# Prove the published archive's consumer contract rather than relying on ar
# member names, which rustc is free to change. The probe builds a Rust
# staticlib, links it through `hew build --link-lib`, and runs the binary from
# a staged release layout.
verify_libhew_external_link() {
    local hew="$1"
    local archive="$2"
    run_with_timeout "${SMOKE_TIMEOUT}" scripts/test-release-lib-link.sh \
        --hew "$hew" --archive "$archive"
}

# ── Determine which platforms to validate ────────────────────────────────────

if [[ $# -eq 0 ]]; then
    PLATFORMS=(linux linux-aarch64 macos freebsd windows)
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
    local message="${2:-Hello from Hew release test}"
    cat >"$file" <<'HEWEOF'
fn main() {
    println("__HEW_SMOKE_MESSAGE__")
}
HEWEOF
    python3 - "$file" "$message" <<'PY'
from pathlib import Path
import json
import sys

path = Path(sys.argv[1])
message = sys.argv[2]
path.write_text(path.read_text().replace('"__HEW_SMOKE_MESSAGE__"', json.dumps(message)))
PY
}

powershell_encode() {
    python3 -c 'import base64, sys; print(base64.b64encode(sys.argv[1].encode("utf-16le")).decode("ascii"))' "$1"
}

run_windows_powershell() {
    local timeout_seconds="$1"
    local script="$2"
    local encoded
    encoded=$(powershell_encode "$script")
    run_with_timeout "${timeout_seconds}" ssh "${WINDOWS_HOST}" \
        "powershell -NoProfile -ExecutionPolicy Bypass -EncodedCommand ${encoded}"
}

powershell_utf8_base64() {
    python3 -c 'import base64, sys; print(base64.b64encode(sys.argv[1].encode()).decode())' "$1"
}

run_windows_staged_build() {
    local remote_stage="$1"
    local llvm_config_b64 llvm_prefix_b64 cc_b64 cxx_b64
    llvm_config_b64=$(powershell_utf8_base64 "$WINDOWS_LLVM_CONFIG")
    llvm_prefix_b64=$(powershell_utf8_base64 "$WINDOWS_LLVM_PREFIX")
    cc_b64=$(powershell_utf8_base64 "$WINDOWS_CC")
    cxx_b64=$(powershell_utf8_base64 "$WINDOWS_CXX")

    # Keep the OpenSSH/cmd.exe command small. The complete build and consumer
    # proof live in the staged candidate at scripts/windows-release-build.ps1.
    run_windows_powershell "${REMOTE_BUILD_TIMEOUT}" "
\$ErrorActionPreference = 'Stop'
# Keep every high-volume write on the already space-checked candidate drive.
# These process-local values deliberately override a full host TEMP/TMP or an
# inherited Cargo target directory without requiring persistent host changes.
# Keep the provisioned Cargo cache in place, but never let validation fetch
# dependencies or update it.
\$env:TEMP = '${remote_stage}/.tmp'
\$env:TMP = \$env:TEMP
\$env:CARGO_TARGET_DIR = '${remote_stage}/target'
\$env:CARGO_NET_OFFLINE = 'true'
New-Item -ItemType Directory -Force -Path \$env:TEMP, \$env:CARGO_TARGET_DIR | Out-Null
\$Utf8 = [System.Text.Encoding]::UTF8
\$env:HEW_WINDOWS_LLVM_CONFIG = \$Utf8.GetString([Convert]::FromBase64String('${llvm_config_b64}'))
\$env:HEW_WINDOWS_LLVM_PREFIX = \$Utf8.GetString([Convert]::FromBase64String('${llvm_prefix_b64}'))
\$env:HEW_WINDOWS_CC = \$Utf8.GetString([Convert]::FromBase64String('${cc_b64}'))
\$env:HEW_WINDOWS_CXX = \$Utf8.GetString([Convert]::FromBase64String('${cxx_b64}'))
& '${remote_stage}/scripts/windows-release-build.ps1'
if (\$LASTEXITCODE -ne 0) { throw \"staged Windows build failed with exit code \$LASTEXITCODE\" }
"
}

create_unix_remote_stage() {
    local host="$1"
    local stage
    stage=$(run_with_timeout "${SSH_CHECK_TIMEOUT}" ssh "$host" \
        'mktemp -d /tmp/hew-pre-release.XXXXXX')
    if [[ "$stage" =~ ^/tmp/hew-pre-release\.[A-Za-z0-9._-]+$ ]]; then
        printf '%s' "$stage"
    else
        echo "invalid remote candidate directory: ${stage}" >&2
        return 1
    fi
}

remove_unix_remote_stage() {
    local host="$1"
    local stage="$2"
    if [[ "$stage" =~ ^/tmp/hew-pre-release\.[A-Za-z0-9._-]+$ ]]; then
        run_with_timeout "${SSH_CHECK_TIMEOUT}" ssh "$host" \
            "rm -rf -- '${stage}'" >/dev/null 2>&1 || true
    else
        echo "refusing to remove unexpected remote path: ${stage}" >&2
    fi
}

create_windows_remote_stage() {
    local root_init="\$Root = [System.IO.Path]::GetTempPath()"
    if [[ -n "$WINDOWS_STAGE_ROOT" ]]; then
        printf -v root_init "\$Root = '%s'" "$WINDOWS_STAGE_ROOT"
    fi
    local stage
    stage=$(run_windows_powershell "${SSH_CHECK_TIMEOUT}" "
\$ErrorActionPreference = 'Stop'
${root_init}
\$DriveName = [System.IO.Path]::GetPathRoot(\$Root).Substring(0, 1)
\$Drive = Get-PSDrive -Name \$DriveName -PSProvider FileSystem
if (\$Drive.Free -lt 8GB) {
    throw \"Windows candidate stage \$Root has only \$([math]::Round(\$Drive.Free / 1GB, 2)) GiB free; at least 8 GiB is required\"
}
New-Item -ItemType Directory -Path \$Root -Force | Out-Null
\$Stage = Join-Path \$Root ('hew-pre-release-' + [guid]::NewGuid())
New-Item -ItemType Directory -Path \$Stage | Out-Null
Write-Output (\$Stage.Replace('\\', '/'))
")
    # Windows OpenSSH preserves PowerShell's CRLF line ending. Command
    # substitution strips the LF but leaves a trailing CR, which must not
    # become part of the validated/scp'd path.
    printf '%s' "${stage//$'\r'/}"
}

remove_windows_remote_stage() {
    local stage="$1"
    if [[ "$stage" =~ ^[A-Za-z]:/[A-Za-z0-9._/\ -]*/hew-pre-release-[0-9A-Fa-f-]+$ ]]; then
        if ! run_windows_powershell "${REMOTE_CLEANUP_TIMEOUT}" "
\$ErrorActionPreference = 'Stop'
if (Test-Path -LiteralPath '${stage}') {
    Remove-Item -LiteralPath '${stage}' -Recurse -Force -ErrorAction Stop
}
" >/dev/null 2>&1; then
            # Cleanup is best-effort so an EXIT trap cannot replace the build
            # result, but a stranded multi-GB stage must remain visible.
            echo "WARNING: Windows remote candidate cleanup timed out or failed after ${REMOTE_CLEANUP_TIMEOUT}s: ${stage}" >&2
        fi
    else
        echo "refusing to remove unexpected Windows remote path: ${stage}" >&2
    fi
}

# ── Platform validators ──────────────────────────────────────────────────────

validate_linux() {
    banner "Linux (local static-link build)"

    local log="${LOG_DIR}/linux.log"
    if [[ "$(uname -s)" != "Linux" || "$(uname -m)" != "x86_64" ]]; then
        fail "linux" "requires a native Linux x86_64 host (got $(uname -s)/$(uname -m))"
        return 1
    fi

    local release_dir release_lib_dir
    release_dir=$(cargo_profile_dir "$REPO_ROOT" release)
    release_lib_dir=$(cargo_profile_dir "$REPO_ROOT" release-lib)

    set +e
    (
        set -e
        echo "==> Step 1: Static-link release build"
        run_with_timeout "${LOCAL_BUILD_TIMEOUT}" make release 2>&1

        echo "==> Step 2: Verify binaries exist and run"
        "${release_dir}/hew" --version
        "${release_dir}/hew-lsp" --version
        "${release_dir}/hew-observe" --version
        test -f "${release_lib_dir}/libhew.a"
        verify_libhew_external_link "${release_dir}/hew" "${release_lib_dir}/libhew.a"

        echo "==> Step 3: Smoke test — run a Hew program"
        local smoke_file_base
        smoke_file_base=$(mktemp)
        local smoke_file="${smoke_file_base}.hew"
        mv "$smoke_file_base" "$smoke_file"
        write_smoke_test "$smoke_file"
        local output
        output=$(run_with_timeout "${SMOKE_TIMEOUT}" "${release_dir}/hew" run "$smoke_file")
        rm -f "$smoke_file"

        if echo "$output" | grep -q "Hello from Hew"; then
            echo "==> Smoke test passed"
        else
            echo "==> SMOKE TEST FAILED — output: $output"
            exit 1
        fi

        echo "==> Step 4: Run gating test suite"
        run_with_timeout "${TEST_TIMEOUT}" bash -o pipefail -lc 'cargo test -p hew-runtime --quiet 2>&1 | tail -3'

        echo "==> Step 4b: Run foundational compiled-Hew gates"
        run_with_timeout "${TEST_TIMEOUT}" make test-compiler-pipeline
        run_with_timeout "${TEST_TIMEOUT}" make test-opaque-resource-lifecycle-matrix-external
        run_with_timeout "${TEST_TIMEOUT}" make test-vertical-slice
        run_with_timeout "${TEST_TIMEOUT}" make test-hew-ratchet
        run_with_timeout "${TEST_TIMEOUT}" make test-stdlib-ratchet

        echo "==> Step 5: Verify no dynamic LLVM/MLIR dependencies"
        if ldd "${release_dir}/hew" 2>/dev/null | grep -qi 'llvm\|mlir'; then
            echo "FATAL: Binary dynamically links LLVM/MLIR"
            exit 1
        fi
        echo "==> No dynamic LLVM/MLIR deps — binary is self-contained"

        echo "==> Step 6: Smoke test packaged archive layout"
        local archive_root
        archive_root=$(mktemp -d)
        trap 'rm -rf "${archive_root:-}"' EXIT

        local archive_name="hew-v${VERSION}-linux-x86_64"
        local package_root="${archive_root}/${archive_name}"
        local package_tarball="${archive_root}/${archive_name}.tar.gz"
        local package_stage="${archive_root}/staging"
        mkdir -p "${package_root}/bin" "${package_root}/lib/x86_64-unknown-linux-gnu" \
            "${package_root}/std" "${package_stage}"

        cp "${release_dir}/hew" "${release_dir}/hew-lsp" \
            "${release_dir}/hew-observe" "${package_root}/bin/"
        chmod +x "${package_root}/bin/"*
        verify_libhew_external_link "${release_dir}/hew" "${release_lib_dir}/libhew.a"
        cp "${release_lib_dir}/libhew.a" "${package_root}/lib/"
        cp "${release_lib_dir}/libhew.a" "${package_root}/lib/x86_64-unknown-linux-gnu/"
        cp -r std/. "${package_root}/std/"

        tar czf "${package_tarball}" -C "${archive_root}" "${archive_name}"
        tar -xf "${package_tarball}" -C "${package_stage}" --strip-components=1

        local package_smoke_file="${archive_root}/pkg-smoke.hew"
        write_smoke_test "${package_smoke_file}" "pkg-smoke-ok"
        local package_output
        package_output=$(run_with_timeout "${SMOKE_TIMEOUT}" env -i PATH=/usr/bin:/bin HOME="${HOME}" HEW_STD="${package_stage}/std" "${package_stage}/bin/hew" run "${package_smoke_file}")

        if echo "$package_output" | grep -q "pkg-smoke-ok"; then
            echo "==> Packaged archive smoke test passed"
        else
            echo "==> PACKAGED ARCHIVE SMOKE TEST FAILED — output: $package_output"
            exit 1
        fi
    ) >"$log" 2>&1
    local status=$?
    set -e
    if [[ "$status" -eq 0 ]]; then
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
        fail "macos" "MACOS_HOST not configured"
        return 1
    fi
    if ! run_with_timeout "${SSH_CHECK_TIMEOUT}" ssh -o ConnectTimeout=5 "${MACOS_HOST}" true 2>/dev/null; then
        fail "macos" "${MACOS_HOST} unreachable"
        return 1
    fi

    set +e
    (
        set -e
        # Pass an explicitly configured prefix to the remote shell without
        # relying on ssh AcceptEnv. %q keeps paths with spaces shell-safe.
        local macos_llvm_assignment="HEW_MACOS_LLVM_PREFIX="
        if [[ -n "$MACOS_LLVM_PREFIX" ]]; then
            printf -v macos_llvm_assignment 'HEW_MACOS_LLVM_PREFIX=%q' "$MACOS_LLVM_PREFIX"
        fi
        remote_stage=$(create_unix_remote_stage "${MACOS_HOST}")
        trap 'remove_unix_remote_stage "${MACOS_HOST}" "${remote_stage}"' EXIT
        echo "==> Staging local candidate on macOS: ${remote_stage}"
        run_with_timeout "${SYNC_TIMEOUT}" rsync -az \
            --exclude target --exclude .git --exclude build --exclude .tmp \
            --exclude node_modules \
            --exclude '*.o' --exclude '*.a' --exclude '*.d' \
            . "${MACOS_HOST}:${remote_stage}/"

        echo "==> Building on macOS"
        run_with_timeout "${REMOTE_BUILD_TIMEOUT}" ssh "${MACOS_HOST}" "${macos_llvm_assignment}" bash -lc "'
            set -eux
            cd ${remote_stage}

            [ \"\$(uname -s)\" = Darwin ] || {
                echo \"FATAL: macOS validator reached \$(uname -s)/\$(uname -m)\" >&2
                exit 1
            }
            case \"\$(uname -m)\" in
                arm64|x86_64) ;;
                *) echo \"FATAL: unsupported macOS architecture: \$(uname -m)\" >&2; exit 1 ;;
            esac

            # Prefer an operator-supplied LLVM root. Otherwise try Homebrew
            # when it exists, then probe the canonical versioned Homebrew
            # prefixes directly so a working llvm@22 install does not require
            # the brew executable to be on PATH.
            configured_prefix=\"\${HEW_MACOS_LLVM_PREFIX:-}\"
            llvm_prefix=\"\"
            if [ -n \"\$configured_prefix\" ]; then
                llvm_candidates=(\"\$configured_prefix\")
            else
                llvm_candidates=()
                if command -v brew >/dev/null 2>&1; then
                    brew_prefix=\"\$(brew --prefix llvm@22 2>/dev/null || true)\"
                    if [ -n \"\$brew_prefix\" ]; then
                        llvm_candidates+=(\"\$brew_prefix\")
                    fi
                fi
                llvm_candidates+=(
                    /opt/homebrew/opt/llvm@22
                    /usr/local/opt/llvm@22
                )
            fi

            for candidate in \"\${llvm_candidates[@]}\"; do
                llvm_config=\"\$candidate/bin/llvm-config\"
                [ -x \"\$llvm_config\" ] || continue
                llvm_version=\"\$(\"\$llvm_config\" --version 2>/dev/null || true)\"
                case \"\$llvm_version\" in
                    22.*)
                        llvm_prefix=\"\$candidate\"
                        break
                        ;;
                esac
            done

            if [ -z \"\$llvm_prefix\" ]; then
                echo \"FATAL: LLVM 22 was not found. Set HEW_MACOS_LLVM_PREFIX to an LLVM 22 root.\" >&2
                exit 1
            fi

            export LLVM_PREFIX=\"\$llvm_prefix\"
            export PATH=\"\$LLVM_PREFIX/bin:\$PATH\"
            \"\$LLVM_PREFIX/bin/llvm-config\" --version

            make release

            release_dir=\"\$(scripts/cargo-output-dir.py --profile release)\"
            release_lib_dir=\"\$(scripts/cargo-output-dir.py --profile release-lib)\"
            \"\$release_dir/hew\" --version
            \"\$release_dir/hew-lsp\" --version
            \"\$release_dir/hew-observe\" --version
            scripts/test-release-lib-link.sh \
                --hew \"\$release_dir/hew\" \
                --archive \"\$release_lib_dir/libhew.a\"

            echo \"==> Smoke test: hew run (guards against process-exit SIGABRT — issue #1606)\"
            scripts/test-release-binary.sh

            echo \"==> Darwin release-authority leak corpus\"
            make macos-leak-oracle

            echo \"macOS build succeeded\"
        '"
    ) >"$log" 2>&1
    local status=$?
    set -e
    if [[ "$status" -eq 0 ]]; then
        pass "macos"
    else
        fail "macos" "see ${log}"
        return 1
    fi
}

validate_linux_aarch64() {
    banner "Linux aarch64 (via SSH to ${LINUX_AARCH64_HOST})"

    local log="${LOG_DIR}/linux-aarch64.log"

    if [[ -z "$LINUX_AARCH64_HOST" ]]; then
        fail "linux-aarch64" "LINUX_AARCH64_HOST not configured"
        return 1
    fi
    if ! run_with_timeout "${SSH_CHECK_TIMEOUT}" ssh -o ConnectTimeout=5 "${LINUX_AARCH64_HOST}" true 2>/dev/null; then
        fail "linux-aarch64" "${LINUX_AARCH64_HOST} unreachable"
        return 1
    fi

    set +e
    (
        set -e
        remote_stage=$(create_unix_remote_stage "${LINUX_AARCH64_HOST}")
        trap 'remove_unix_remote_stage "${LINUX_AARCH64_HOST}" "${remote_stage}"' EXIT
        echo "==> Staging local candidate on Linux aarch64: ${remote_stage}"
        run_with_timeout "${SYNC_TIMEOUT}" rsync -az \
            --exclude target --exclude .git --exclude build --exclude .tmp \
            --exclude node_modules \
            --exclude '*.o' --exclude '*.a' --exclude '*.d' \
            . "${LINUX_AARCH64_HOST}:${remote_stage}/"

        echo "==> Provisioning LLVM 22 from apt.llvm.org/noble"
        run_with_timeout "${REMOTE_BUILD_TIMEOUT}" ssh "${LINUX_AARCH64_HOST}" bash -lc "'
            set -eux
            cd ${remote_stage}

            [ \"\$(uname -s)\" = Linux ] || {
                echo \"FATAL: Linux aarch64 validator reached \$(uname -s)/\$(uname -m)\" >&2
                exit 1
            }
            case \"\$(uname -m)\" in
                aarch64|arm64) ;;
                *) echo \"FATAL: Linux aarch64 validator reached \$(uname -m)\" >&2; exit 1 ;;
            esac
            . /etc/os-release
            [ \"\${ID:-}\" = ubuntu ] && [ \"\${VERSION_ID:-}\" = 24.04 ] || {
                echo \"FATAL: Linux aarch64 validator requires Ubuntu 24.04, got \${ID:-unknown}/\${VERSION_ID:-unknown}\" >&2
                exit 1
            }

            sudo mkdir -p /etc/apt/keyrings
            wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key \
                | sudo tee /etc/apt/keyrings/llvm.asc >/dev/null
            echo \"deb [signed-by=/etc/apt/keyrings/llvm.asc] http://apt.llvm.org/noble/ llvm-toolchain-noble-22 main\" \
                | sudo tee /etc/apt/sources.list.d/llvm.list >/dev/null
            sudo apt-get update -qq
            sudo apt-get install -y -qq \
                llvm-22-dev \
                clang-22 \
                lld-22 \
                libssl-dev pkg-config \
                zlib1g-dev libzstd-dev

            export LLVM_PREFIX=/usr/lib/llvm-22
            export CC=clang-22
            export CXX=clang++-22

            cargo build -p hew-cli -p hew-lsp -p hew-observe --release
            cargo build -p hew-lib --profile release-lib
            rustup target add wasm32-wasip1
            cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features --release

            release_dir=\"\$(scripts/cargo-output-dir.py --profile release)\"
            release_lib_dir=\"\$(scripts/cargo-output-dir.py --profile release-lib)\"
            \"\$release_dir/hew\" --version
            \"\$release_dir/hew-lsp\" --version
            \"\$release_dir/hew-observe\" --version
            test -f \"\$release_lib_dir/libhew.a\"
            scripts/test-release-lib-link.sh \
                --hew \"\$release_dir/hew\" \
                --archive \"\$release_lib_dir/libhew.a\"

            printf '%s\n' \"fn main() { println(\\\"Hello from Hew release test\\\") }\" > _smoke.hew
            \"\$release_dir/hew\" build _smoke.hew -o _smoke_bin
            chmod +x _smoke_bin
            ./_smoke_bin | grep -q \"Hello from Hew release test\"
            rm -f _smoke.hew _smoke_bin
        '"
    ) >"$log" 2>&1
    local status=$?
    set -e
    if [[ "$status" -eq 0 ]]; then
        pass "linux-aarch64"
    else
        fail "linux-aarch64" "see ${log}"
        return 1
    fi
}

validate_freebsd() {
    banner "FreeBSD (via SSH to ${FREEBSD_HOST})"

    local log="${LOG_DIR}/freebsd.log"

    if [[ -z "$FREEBSD_HOST" ]]; then
        fail "freebsd" "FREEBSD_HOST not configured"
        return 1
    fi
    if ! run_with_timeout "${SSH_CHECK_TIMEOUT}" ssh -o ConnectTimeout=5 "${FREEBSD_HOST}" true 2>/dev/null; then
        fail "freebsd" "${FREEBSD_HOST} unreachable"
        return 1
    fi

    set +e
    (
        set -e
        remote_stage=$(create_unix_remote_stage "${FREEBSD_HOST}")
        trap 'remove_unix_remote_stage "${FREEBSD_HOST}" "${remote_stage}"' EXIT
        echo "==> Staging local candidate on FreeBSD: ${remote_stage}"
        run_with_timeout "${SYNC_TIMEOUT}" rsync -az \
            --exclude target --exclude .git --exclude build --exclude .tmp \
            --exclude node_modules \
            --exclude '*.o' --exclude '*.a' --exclude '*.d' \
            . "${FREEBSD_HOST}:${remote_stage}/"

        echo "==> Building on FreeBSD"
        run_with_timeout "${REMOTE_BUILD_TIMEOUT}" ssh "${FREEBSD_HOST}" bash -lc "'
            set -eux
            cd ${remote_stage}

            [ \"\$(uname -s)\" = FreeBSD ] || {
                echo \"FATAL: FreeBSD validator reached \$(uname -s)/\$(uname -m)\" >&2
                exit 1
            }
            [ \"\$(uname -m)\" = amd64 ] || {
                echo \"FATAL: FreeBSD x86_64 validator reached \$(uname -m)\" >&2
                exit 1
            }

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

            cargo build -p hew-cli -p hew-lsp -p hew-observe --release
            cargo build -p hew-lib --profile release-lib

            release_dir=\"\$(scripts/cargo-output-dir.py --profile release)\"
            release_lib_dir=\"\$(scripts/cargo-output-dir.py --profile release-lib)\"
            \"\$release_dir/hew\" --version
            \"\$release_dir/hew-lsp\" --version
            \"\$release_dir/hew-observe\" --version
            scripts/test-release-lib-link.sh \
                --hew \"\$release_dir/hew\" \
                --archive \"\$release_lib_dir/libhew.a\"

            echo \"FreeBSD build succeeded\"
        '"
    ) >"$log" 2>&1
    local status=$?
    set -e
    if [[ "$status" -eq 0 ]]; then
        pass "freebsd"
    else
        fail "freebsd" "see ${log}"
        return 1
    fi
}

validate_windows() {
    banner "Windows (via SSH to ${WINDOWS_HOST})"

    local log="${LOG_DIR}/windows.log"
    local llvm_config_b64 llvm_prefix_b64
    llvm_config_b64=$(powershell_utf8_base64 "$WINDOWS_LLVM_CONFIG")
    llvm_prefix_b64=$(powershell_utf8_base64 "$WINDOWS_LLVM_PREFIX")

    if [[ -z "$WINDOWS_HOST" ]]; then
        fail "windows" "WINDOWS_HOST not configured"
        return 1
    fi
    if [[ -n "$WINDOWS_STAGE_ROOT" && ! "$WINDOWS_STAGE_ROOT" =~ ^[A-Za-z]:/[A-Za-z0-9._/\ -]+$ ]]; then
        fail "windows" "HEW_WINDOWS_STAGE_ROOT is not a safe absolute Windows path"
        return 1
    fi
    if [[ -z "$WINDOWS_CANDIDATE_ARCHIVE" ]]; then
        fail "windows" "HEW_WINDOWS_CANDIDATE_ARCHIVE not configured"
        return 1
    fi
    if [[ ! -f "$WINDOWS_CANDIDATE_ARCHIVE" || ! -r "$WINDOWS_CANDIDATE_ARCHIVE" ]]; then
        fail "windows" "HEW_WINDOWS_CANDIDATE_ARCHIVE is not a readable regular file"
        return 1
    fi
    if ! run_windows_powershell "${SSH_CHECK_TIMEOUT}" \
        "\$ErrorActionPreference = 'Stop'; Write-Output 'reachable'" >/dev/null 2>&1; then
        fail "windows" "${WINDOWS_HOST} unreachable"
        return 1
    fi

    set +e
    (
        set -e
        remote_stage=$(create_windows_remote_stage)
        if [[ ! "$remote_stage" =~ ^[A-Za-z]:/[A-Za-z0-9._/\ -]*/hew-pre-release-[0-9A-Fa-f-]+$ ]]; then
            echo "FATAL: invalid Windows candidate directory: ${remote_stage}"
            exit 1
        fi
        trap 'remove_windows_remote_stage "${remote_stage}"' EXIT
        echo "==> Staging Windows candidate archive: ${remote_stage}"
        run_with_timeout "${SYNC_TIMEOUT}" scp "${WINDOWS_CANDIDATE_ARCHIVE}" \
            "${WINDOWS_HOST}:${remote_stage}/candidate.tar.gz"
        run_windows_powershell "${SYNC_TIMEOUT}" "
\$ErrorActionPreference = 'Stop'
tar.exe -xf '${remote_stage}/candidate.tar.gz' -C '${remote_stage}'
if (\$LASTEXITCODE -ne 0) { throw 'failed to extract local release candidate' }
Remove-Item -LiteralPath '${remote_stage}/candidate.tar.gz' -Force
"

        echo "==> Verifying Windows LLVM install"
        run_windows_powershell "${SSH_CHECK_TIMEOUT}" "
\$ErrorActionPreference = 'Stop'
\$Utf8 = [System.Text.Encoding]::UTF8
\$LlvmConfig = \$Utf8.GetString([Convert]::FromBase64String('${llvm_config_b64}'))
if (-not (Test-Path \$LlvmConfig)) {
    throw \"Missing \$LlvmConfig. Bootstrap LLVM 22 at C:\\llvm-22 (see docs/cross-platform-build-guide.md) or set HEW_WINDOWS_LLVM_PREFIX / HEW_WINDOWS_LLVM_CONFIG before running pre-release validation.\"
}
Write-Host \"Found \$LlvmConfig\"
"

        echo "==> Building on Windows with the LLVM toolchain"
        run_windows_staged_build "${remote_stage}"
    ) >"$log" 2>&1
    local status=$?
    set -e
    if [[ "$status" -eq 0 ]]; then
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
    linux-aarch64)
        validate_linux_aarch64 &
        PIDS+=($!)
        PLATFORM_NAMES+=("$platform")
        ;;
    macos | freebsd | windows)
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
        status=$(cut -d' ' -f1 <"$result_file")
        detail=$(cut -d' ' -f3- <"$result_file")
        detail_suffix=""
        [[ -n "$detail" ]] && detail_suffix=" (${detail})"
        case "$status" in
        pass) echo -e "  ${GREEN}✓ ${platform}${RESET}" ;;
        fail)
            echo -e "  ${RED}✗ ${platform}${detail_suffix}${RESET}"
            HAVE_FAILURE=1
            ;;
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
