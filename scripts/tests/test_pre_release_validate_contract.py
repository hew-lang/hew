"""Executable contract for local-candidate host validation.

The validator must test the caller's working tree in an isolated temporary
directory. It must never update, reset, or delete an existing remote checkout,
and a requested but unavailable platform must fail the run.
"""

import os
import shutil
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
VALIDATOR = ROOT / "scripts" / "pre-release-validate.sh"
WINDOWS_BUILD_SCRIPT = ROOT / "scripts" / "windows-release-build.ps1"


def validator() -> str:
    return VALIDATOR.read_text()


def release_surface() -> str:
    """All source that makes up the staged Windows release-build contract."""
    return validator() + "\n" + WINDOWS_BUILD_SCRIPT.read_text()


def assert_isolated_staging(text: str) -> None:
    assert "mktemp -d /tmp/hew-pre-release.XXXXXX" in text
    assert "hew-pre-release-' + [guid]::NewGuid()" in text
    assert (
        'WINDOWS_STAGE_ROOT="${HEW_WINDOWS_STAGE_ROOT:-${WINDOWS_STAGE_ROOT:-}}"'
        in text
    )
    assert "\\$Drive.Free -lt 8GB" in text
    assert "^/tmp/hew-pre-release\\.[A-Za-z0-9._-]+$" in text
    assert "^[A-Za-z]:/[A-Za-z0-9._/\\ -]*/hew-pre-release-[0-9A-Fa-f-]+$" in text
    assert "--delete" not in text
    assert "git pull" not in text
    assert "git reset" not in text
    assert "git fetch" not in text
    assert (
        text.count("--exclude target --exclude .git --exclude build --exclude .tmp")
        == 3
    )
    assert text.count("--exclude node_modules") == 3
    assert (
        "--exclude='./target' --exclude='./.git' --exclude='./build' --exclude='./.tmp'"
        in text
    )
    assert "--exclude='node_modules'" in text
    assert '. "${MACOS_HOST}:${remote_stage}/"' in text
    assert '. "${LINUX_AARCH64_HOST}:${remote_stage}/"' in text
    assert '. "${FREEBSD_HOST}:${remote_stage}/"' in text
    assert "${WINDOWS_HOST}:${remote_stage}/candidate.tar.gz" in text


def assert_macos_llvm_discovery_contract(text: str) -> None:
    """The macOS probe must work without brew and reject non-LLVM-22 roots."""
    assert (
        'MACOS_LLVM_PREFIX="${HEW_MACOS_LLVM_PREFIX:-${MACOS_LLVM_PREFIX:-}}"' in text
    )
    assert "printf -v macos_llvm_assignment 'HEW_MACOS_LLVM_PREFIX=%q'" in text
    assert "command -v brew >/dev/null 2>&1" in text
    assert "brew --prefix llvm@22 2>/dev/null || true" in text
    assert (
        "llvm_candidates+=(\n                    /opt/homebrew/opt/llvm@22\n                    /usr/local/opt/llvm@22"
        in text
    )
    assert "$candidate/bin/llvm-config" in text
    assert "$llvm_config" in text
    assert "case" in text and "$llvm_version" in text
    assert "22.*)" in text
    assert "FATAL: LLVM 22 was not found." in text
    assert "brew --prefix llvm@22 2>/dev/null || echo" not in text


def assert_windows_toolchain_bootstrap_contract(text: str) -> None:
    assert "Microsoft Visual Studio\\Installer\\vswhere.exe" in text
    assert "Microsoft.VisualStudio.Component.VC.Tools.x86.x64" in text
    assert "Common7\\Tools\\VsDevCmd.bat" in text
    assert "-no_logo -arch=x64 -host_arch=x64 >nul && set" in text
    assert "[Environment]::SetEnvironmentVariable" in text
    assert "$env:WindowsSdkDir" in text
    assert "$env:LIB" in text
    assert "$env:AWS_LC_SYS_PREBUILT_NASM = '1'" in text
    assert "$env:CARGO_BUILD_JOBS = '2'" in text


def assert_windows_llvm_binding_contract(text: str) -> None:
    """The staged build must bind llvm-sys to its validated LLVM 22.1 root."""
    assert "$LlvmConfigExe = Join-Path $LlvmPrefix 'bin\\llvm-config.exe'" in text
    assert "Test-Path $LlvmConfigExe -PathType Leaf" in text
    assert "$LlvmVersion = & $LlvmConfigExe --version" in text
    assert "Assert-NativeSuccess 'llvm-config.exe --version'" in text
    assert "$LlvmVersion -notmatch '^22\\.1\\.0\\s*$'" in text
    assert "$env:LLVM_SYS_221_PREFIX = $LlvmPrefix" in text


def assert_platform_identity_contract(text: str) -> None:
    """No validator may certify artifacts under a guessed platform label."""
    assert "PLATFORMS=(linux linux-aarch64 macos freebsd windows)" in text
    assert '"$(uname -s)" != "Linux" || "$(uname -m)" != "x86_64"' in text
    assert '[ \\"\\$(uname -s)\\" = Darwin ]' in text
    assert 'case \\"\\$(uname -m)\\" in\n                arm64|x86_64)' in text
    assert '[ \\"\\$(uname -s)\\" = Linux ]' in text
    assert "aarch64|arm64)" in text
    assert ". /etc/os-release" in text
    assert 'VERSION_ID:-}\\" = 24.04' in text
    assert '[ \\"\\$(uname -s)\\" = FreeBSD ]' in text
    assert '[ \\"\\$(uname -m)\\" = amd64 ]' in text
    windows = WINDOWS_BUILD_SCRIPT.read_text()
    assert "[Environment]::Is64BitOperatingSystem" in windows
    assert "$env:PROCESSOR_ARCHITECTURE -ne 'AMD64'" in windows


def assert_windows_staged_build_transport_contract(text: str) -> None:
    assert 'run_windows_staged_build "${remote_stage}"' in text
    assert "scripts/windows-release-build.ps1" in text
    assert "Set-Location (Split-Path -Parent $PSScriptRoot)" in text
    # The single build/consumer/smoke process must override a potentially full
    # host C: temp directory with paths rooted in the already space-checked,
    # uniquely staged candidate.
    assert text.count("\\$env:TEMP = '${remote_stage}/.tmp'") == 1
    assert text.count("\\$env:TMP = \\$env:TEMP") == 1
    assert text.count("\\$env:CARGO_TARGET_DIR = '${remote_stage}/target'") == 1
    assert text.count("\\$env:CARGO_HOME = '${remote_stage}/.cargo-home'") == 1
    assert (
        text.count(
            "New-Item -ItemType Directory -Force -Path "
            "\\$env:TEMP, \\$env:CARGO_TARGET_DIR, \\$env:CARGO_HOME | Out-Null"
        )
        == 1
    )
    assert (
        "cargo build -p hew-cli -p adze-cli -p hew-lsp -p hew-observe --release" in text
    )
    assert "release library consumer proof" in text
    assert "& $Hew build $SmokeSource -o $SmokeOutput" in text
    assert "Smoke test passed" in text
    assert text.count("FromBase64String('${llvm_config_b64}')") == 2
    assert text.count("FromBase64String('${llvm_prefix_b64}')") == 1
    assert "Test-Path '${WINDOWS_LLVM_CONFIG}'" not in text
    assert "$env:Path = '${WINDOWS_LLVM_PREFIX}" not in text


def assert_windows_remote_cleanup_contract(text: str) -> None:
    # The stage now owns Cargo's target, registry/cache, and temp trees. Cleanup
    # must therefore have a transport-scale budget, report failure, and still
    # return success from the EXIT trap so it cannot mask the build result.
    assert (
        'REMOTE_CLEANUP_TIMEOUT="${HEW_TIMEOUT_REMOTE_CLEANUP:-${SYNC_TIMEOUT}}"'
        in text
    )
    assert 'if ! run_windows_powershell "${REMOTE_CLEANUP_TIMEOUT}" "' in text
    assert (
        "WARNING: Windows remote candidate cleanup timed out or failed after "
        "${REMOTE_CLEANUP_TIMEOUT}s: ${stage}" in text
    )
    assert (
        "Remove-Item -LiteralPath '${stage}' -Recurse -Force -ErrorAction Stop" in text
    )
    assert "if (Test-Path -LiteralPath '${stage}') {" in text
    assert (
        "Remove-Item -LiteralPath '${stage}' -Recurse -Force -ErrorAction Stop\n"
        '}\n" >/dev/null 2>&1; then' in text
    )
    assert (
        "Remove-Item -LiteralPath '${stage}' -Recurse -Force "
        "-ErrorAction SilentlyContinue" not in text
    )


def assert_cargo_output_dir_contract(text: str) -> None:
    """Every staged validator must inspect the artifacts Cargo just emitted."""
    windows = WINDOWS_BUILD_SCRIPT.read_text()
    assert 'source "${REPO_ROOT}/scripts/lib/cargo-output-dir.sh"' in text
    assert 'cargo_profile_dir "$REPO_ROOT" release' in text
    assert 'cargo_profile_dir "$REPO_ROOT" release-lib' in text
    # macOS, Linux aarch64, and FreeBSD each resolve both native profiles.
    assert text.count("scripts/cargo-output-dir.py --profile release)") == 3
    assert text.count("scripts/cargo-output-dir.py --profile release-lib)") == 3
    # Windows has no Python dependency: its own cached Cargo JSON build output
    # is the sole artifact-path authority. This covers custom target roots,
    # configured build targets, and path spaces without a filesystem search.
    assert ".hew-release-dir" in windows
    assert "--release --message-format=json" in windows
    assert "--profile release-lib --message-format=json" in windows
    assert "Get-CargoCompilerArtifacts" in windows
    assert "ConvertFrom-Json -ErrorAction Stop" in windows
    assert "if ($Message.reason -eq 'compiler-artifact')" in windows
    assert "Resolve-UniqueCargoArtifact" in windows
    assert "did not emit exactly one $LeafName artifact" in windows
    assert "'hew.exe' 'release binary build' -Executable" in windows
    assert "'adze.exe' 'release binary build' -Executable" in windows
    assert "'hew-lsp.exe' 'release binary build' -Executable" in windows
    assert "'hew-observe.exe' 'release binary build' -Executable" in windows
    assert "'hew.lib' 'release-lib build'" in windows
    assert (
        "$ReleaseArtifacts = Get-CargoCompilerArtifacts $ReleaseBuildMessages "
        "'release binary build'" in windows
    )
    assert (
        "$ReleaseLibArtifacts = Get-CargoCompilerArtifacts $ReleaseLibBuildMessages "
        "'release-lib build'" in windows
    )
    assert "$ReleaseDir = Split-Path -Parent $Hew" in windows
    assert "$ReleaseLibDir = Split-Path -Parent $ReleaseLib" in windows
    assert "& $Adze --version" in windows
    assert "& $HewLsp --version" in windows
    assert "& $HewObserve --version" in windows
    assert "Join-Path $ReleaseDir 'adze.exe'" not in windows
    assert "Join-Path $ReleaseDir 'hew-lsp.exe'" not in windows
    assert "Join-Path $ReleaseDir 'hew-observe.exe'" not in windows
    assert "cargo-output-dir.py" not in windows
    assert "Get-Command python" not in windows
    assert "Python 3 is required" not in windows

    for stale in (
        "target/release/hew",
        "target/release/adze",
        "target/release/hew-lsp",
        "target/release/hew-observe",
        "target/release-lib/libhew.a",
        r".\target\release",
        r".\target\release-lib",
    ):
        assert stale not in text + "\n" + windows


_FAKE_SSH = r"""#!/usr/bin/env bash
set -eu
printf 'ssh %s\n' "$*" >> "$FAKE_REMOTE_LOG"
case "$*" in
  *"fake-windows"*" true")
    # A real Windows OpenSSH server routes this through cmd.exe, where the
    # Unix utility `true` does not exist. Keep the fake honest so the
    # platform reachability probe must use PowerShell too.
    exit 97
    ;;
  *"mktemp -d /tmp/hew-pre-release.XXXXXX"*)
    printf '%s\n' "${FAKE_UNIX_STAGE:-/tmp/hew-pre-release.fake}"
    ;;
  *"-EncodedCommand"*)
    count=0
    if [ -f "$FAKE_WINDOWS_STATE" ]; then
      count=$(cat "$FAKE_WINDOWS_STATE")
    fi
    count=$((count + 1))
    printf '%s\n' "$count" > "$FAKE_WINDOWS_STATE"
    # Call 1 is the PowerShell reachability probe; call 2 creates the stage.
    if [ "$count" -eq 2 ]; then
      printf '%s\r\n' "${FAKE_WINDOWS_STAGE:-C:/Temp/hew-pre-release-00000000-0000-0000-0000-000000000001}"
    fi
    ;;
esac
"""

_FAKE_RSYNC = r"""#!/usr/bin/env bash
set -eu
printf 'rsync %s\n' "$*" >> "$FAKE_REMOTE_LOG"
if [ "${FAKE_RSYNC_EXIT:-0}" -ne 0 ]; then
  exit "$FAKE_RSYNC_EXIT"
fi
"""

_FAKE_SCP = r"""#!/usr/bin/env bash
set -eu
printf 'scp %s\n' "$*" >> "$FAKE_REMOTE_LOG"
"""


def run_with_fake_remote(
    platform: str, extra_env: dict[str, str] | None = None
) -> tuple[subprocess.CompletedProcess[str], str]:
    with tempfile.TemporaryDirectory() as directory:
        temp = Path(directory)
        bin_dir = temp / "bin"
        bin_dir.mkdir()
        for name, source in (
            ("ssh", _FAKE_SSH),
            ("rsync", _FAKE_RSYNC),
            ("scp", _FAKE_SCP),
        ):
            path = bin_dir / name
            path.write_text(source)
            path.chmod(0o755)

        log = temp / "remote.log"
        state = temp / "windows-state"
        env = os.environ.copy()
        env.update(
            {
                "PATH": f"{bin_dir}:{env['PATH']}",
                "FAKE_REMOTE_LOG": str(log),
                "FAKE_WINDOWS_STATE": str(state),
                "HEW_TIMEOUT_SSH_CHECK": "5",
                "HEW_TIMEOUT_SYNC": "5",
                "HEW_TIMEOUT_REMOTE_BUILD": "5",
                "HEW_MACOS_HOST": "fake-macos",
                "HEW_LINUX_AARCH64_HOST": "fake-linux-arm",
                "HEW_FREEBSD_HOST": "fake-freebsd",
                "HEW_WINDOWS_HOST": "fake-windows",
            }
        )
        if extra_env:
            env.update(extra_env)
        result = subprocess.run(
            ["bash", str(VALIDATOR), platform],
            cwd=ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
            timeout=30,
        )
        calls = log.read_text() if log.exists() else ""
        return result, calls


def test_static_staging_contract() -> None:
    assert_isolated_staging(validator())


def test_macos_llvm_discovery_contract() -> None:
    assert_macos_llvm_discovery_contract(validator())


def test_windows_toolchain_bootstrap_contract() -> None:
    assert_windows_toolchain_bootstrap_contract(release_surface())


def test_windows_llvm_binding_contract() -> None:
    assert_windows_llvm_binding_contract(release_surface())


def test_windows_staged_build_transport_contract() -> None:
    assert_windows_staged_build_transport_contract(release_surface())


def test_windows_remote_cleanup_contract() -> None:
    assert_windows_remote_cleanup_contract(validator())


def test_cargo_output_dir_contract() -> None:
    assert_cargo_output_dir_contract(validator())


def test_platform_identity_contract() -> None:
    assert_platform_identity_contract(validator())


def test_macos_llvm_discovery_mutations_are_rejected() -> None:
    original = validator()
    for mutation in (
        original.replace(
            "llvm_candidates+=(\n                    /opt/homebrew/opt/llvm@22",
            "llvm_candidates+=(\n                    /opt/homebrew/opt/llvm",
            1,
        ),
        original.replace("22.*)", "*)", 1),
    ):
        try:
            assert_macos_llvm_discovery_contract(mutation)
        except AssertionError:
            continue
        raise AssertionError("macOS LLVM discovery mutation escaped the contract")


def test_windows_toolchain_bootstrap_mutations_are_rejected() -> None:
    original = release_surface()
    for mutation in (
        original.replace(
            "Microsoft.VisualStudio.Component.VC.Tools.x86.x64",
            "Microsoft.VisualStudio.Component.CoreEditor",
            1,
        ),
        original.replace("Common7\\Tools\\VsDevCmd.bat", "missing.bat", 1),
        original.replace("[Environment]::SetEnvironmentVariable", "Write-Output", 1),
        original.replace(
            "$env:AWS_LC_SYS_PREBUILT_NASM = '1'",
            "$env:AWS_LC_SYS_PREBUILT_NASM = '0'",
            1,
        ),
        original.replace(
            "$env:CARGO_BUILD_JOBS = '2'", "$env:CARGO_BUILD_JOBS = '64'", 1
        ),
    ):
        try:
            assert_windows_toolchain_bootstrap_contract(mutation)
        except AssertionError:
            continue
        raise AssertionError(
            "Windows developer-environment mutation escaped the contract"
        )


def test_windows_llvm_binding_mutations_are_rejected() -> None:
    original = release_surface()
    for mutation in (
        original.replace(
            "$env:LLVM_SYS_221_PREFIX = $LlvmPrefix",
            "$env:LLVM_SYS_221_PREFIX = 'C:\\llvm-stale'",
            1,
        ),
        original.replace(
            "$LlvmVersion = & $LlvmConfigExe --version",
            "$LlvmVersion = '22.1.0'",
            1,
        ),
        original.replace("^22\\.1\\.0\\s*$", ".*", 1),
    ):
        try:
            assert_windows_llvm_binding_contract(mutation)
        except AssertionError:
            continue
        raise AssertionError("Windows LLVM binding mutation escaped the contract")


def test_platform_identity_mutations_are_rejected() -> None:
    original = validator()
    mutations = (
        original.replace("linux linux-aarch64 macos", "linux macos", 1),
        original.replace('"$(uname -s)" != "Linux"', '"$(uname -s)" != "Never"', 1),
        original.replace('[ \\"\\$(uname -s)\\" = Darwin ]', "true", 1),
        original.replace("aarch64|arm64)", "x86_64)", 1),
        original.replace(". /etc/os-release", "true", 1),
        original.replace('[ \\"\\$(uname -s)\\" = FreeBSD ]', "true", 1),
    )
    for mutation in mutations:
        try:
            assert_platform_identity_contract(mutation)
        except AssertionError:
            continue
        raise AssertionError("platform identity mutation escaped the contract")


def test_linux_validator_rejects_macos_before_building() -> None:
    with tempfile.TemporaryDirectory() as directory:
        temp = Path(directory)
        calls = temp / "calls"
        fake_uname = temp / "uname"
        fake_uname.write_text(
            "#!/usr/bin/env bash\n"
            "case \"${1:-}\" in -s) printf 'Darwin\\n' ;; -m) printf 'arm64\\n' ;; esac\n"
        )
        fake_uname.chmod(0o755)
        fake_cargo = temp / "cargo"
        fake_cargo.write_text(
            f"#!/usr/bin/env bash\nprintf '%s\\n' called >> '{calls}'\nexit 99\n"
        )
        fake_cargo.chmod(0o755)
        env = {**os.environ, "PATH": f"{temp}:{os.environ['PATH']}"}
        result = subprocess.run(
            ["bash", str(VALIDATOR), "linux"],
            cwd=ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
        )
    assert result.returncode != 0
    assert "requires a native Linux x86_64 host" in result.stdout
    assert not calls.exists(), "wrong-host rejection must happen before Cargo"


def test_windows_staged_build_transport_mutations_are_rejected() -> None:
    original = release_surface()
    for mutation in (
        original.replace(
            'run_windows_staged_build "${remote_stage}"', "Write-Output inline-build", 1
        ),
        original.replace(
            "scripts/windows-release-build.ps1", "scripts/inline-build.ps1"
        ),
        original.replace(
            "Set-Location (Split-Path -Parent $PSScriptRoot)", "Set-Location C:/hew", 1
        ),
        original.replace(
            "FromBase64String('${llvm_config_b64}')", "'${WINDOWS_LLVM_CONFIG}'", 1
        ),
        original.replace(
            "FromBase64String('${llvm_prefix_b64}')", "'${WINDOWS_LLVM_PREFIX}'", 1
        ),
        original.replace(
            "\\$env:TEMP = '${remote_stage}/.tmp'",
            "\\$env:TEMP = 'C:/Temp'",
            1,
        ),
        original.replace("\\$env:TMP = \\$env:TEMP", "\\$env:TMP = 'C:/Temp'", 1),
        original.replace(
            "\\$env:CARGO_TARGET_DIR = '${remote_stage}/target'",
            "\\$env:CARGO_TARGET_DIR = 'C:/hew-target'",
            1,
        ),
        original.replace(
            "\\$env:CARGO_HOME = '${remote_stage}/.cargo-home'",
            "\\$env:CARGO_HOME = 'C:/Users/hew/.cargo'",
            1,
        ),
        original.replace(
            "New-Item -ItemType Directory -Force -Path "
            "\\$env:TEMP, \\$env:CARGO_TARGET_DIR, \\$env:CARGO_HOME | Out-Null",
            "Write-Output skipped-candidate-directories",
            1,
        ),
    ):
        try:
            assert_windows_staged_build_transport_contract(mutation)
        except AssertionError:
            continue
        raise AssertionError(
            "Windows staged-build transport mutation escaped the contract"
        )


def test_windows_remote_cleanup_mutations_are_rejected() -> None:
    original = validator()
    for mutation in (
        original.replace(
            'REMOTE_CLEANUP_TIMEOUT="${HEW_TIMEOUT_REMOTE_CLEANUP:-${SYNC_TIMEOUT}}"',
            'REMOTE_CLEANUP_TIMEOUT="${SSH_CHECK_TIMEOUT}"',
            1,
        ),
        original.replace(
            'if ! run_windows_powershell "${REMOTE_CLEANUP_TIMEOUT}" "',
            'if ! run_windows_powershell "${SSH_CHECK_TIMEOUT}" "',
            1,
        ),
        original.replace(
            "WARNING: Windows remote candidate cleanup timed out or failed after ",
            "ignored cleanup failure after ",
            1,
        ),
        original.replace(
            "Remove-Item -LiteralPath '${stage}' -Recurse -Force -ErrorAction Stop",
            "Remove-Item -LiteralPath '${stage}' -Recurse -Force "
            "-ErrorAction SilentlyContinue",
            1,
        ),
        original.replace(
            "if (Test-Path -LiteralPath '${stage}') {",
            "if ($false) {",
            1,
        ),
        original.replace(
            "Remove-Item -LiteralPath '${stage}' -Recurse -Force -ErrorAction Stop\n"
            '}\n" >/dev/null 2>&1; then',
            "Remove-Item -LiteralPath '${stage}' -Recurse -Force -ErrorAction Stop\n"
            '}\n" >/dev/null 2>&1 || true',
            1,
        ),
    ):
        try:
            assert_windows_remote_cleanup_contract(mutation)
        except AssertionError:
            continue
        raise AssertionError("Windows remote cleanup mutation escaped the contract")


def test_cargo_output_dir_mutations_are_rejected() -> None:
    original = validator()
    for mutation in (
        original.replace(
            '"${release_dir}/hew" --version', "target/release/hew --version", 1
        ),
        original.replace(
            r"release_dir=\"\$(scripts/cargo-output-dir.py --profile release)\"",
            r"release_dir=\"target/release\"",
            1,
        ),
    ):
        try:
            assert_cargo_output_dir_contract(mutation)
        except AssertionError:
            continue
        raise AssertionError("Cargo output-directory mutation escaped the contract")


def test_windows_cargo_json_artifact_mutations_are_rejected() -> None:
    original = WINDOWS_BUILD_SCRIPT.read_text()
    for mutation in (
        original.replace("--release --message-format=json", "--release", 1),
        original.replace("ConvertFrom-Json -ErrorAction Stop", "ConvertFrom-String", 1),
        original.replace(
            "if ($Message.reason -eq 'compiler-artifact')",
            "if ($Message.reason -eq 'build-finished')",
            1,
        ),
        original.replace(
            "did not emit exactly one $LeafName artifact", "artifact missing", 1
        ),
        original.replace(
            "$Adze = Resolve-UniqueCargoArtifact $ReleaseArtifacts 'adze.exe' 'release binary build' -Executable",
            "$Adze = Join-Path $ReleaseDir 'adze.exe'",
            1,
        ),
        original.replace(
            "$HewLsp = Resolve-UniqueCargoArtifact $ReleaseArtifacts 'hew-lsp.exe' 'release binary build' -Executable",
            "$HewLsp = Join-Path $ReleaseDir 'hew-lsp.exe'",
            1,
        ),
        original.replace(
            "$HewObserve = Resolve-UniqueCargoArtifact $ReleaseArtifacts 'hew-observe.exe' 'release binary build' -Executable",
            "$HewObserve = Join-Path $ReleaseDir 'hew-observe.exe'",
            1,
        ),
        original.replace(
            "$ReleaseDir = Split-Path -Parent $Hew",
            "$ReleaseDir = '.\\target\\release'",
            1,
        ),
        original.replace(
            "$ReleaseLibDir = Split-Path -Parent $ReleaseLib",
            "$ReleaseLibDir = '.\\target\\release-lib'",
            1,
        ),
        original.replace(
            "Get-CargoCompilerArtifacts $ReleaseBuildMessages 'release binary build'",
            "Resolve-CargoProfileDir 'release'",
            1,
        ),
    ):
        try:
            windows = mutation
            assert "--release --message-format=json" in windows
            assert "--profile release-lib --message-format=json" in windows
            assert "Get-CargoCompilerArtifacts" in windows
            assert "ConvertFrom-Json -ErrorAction Stop" in windows
            assert "if ($Message.reason -eq 'compiler-artifact')" in windows
            assert "Resolve-UniqueCargoArtifact" in windows
            assert "did not emit exactly one $LeafName artifact" in windows
            assert "'adze.exe' 'release binary build' -Executable" in windows
            assert "'hew-lsp.exe' 'release binary build' -Executable" in windows
            assert "'hew-observe.exe' 'release binary build' -Executable" in windows
            assert (
                "$ReleaseArtifacts = Get-CargoCompilerArtifacts $ReleaseBuildMessages "
                "'release binary build'" in windows
            )
            assert (
                "$ReleaseLibArtifacts = Get-CargoCompilerArtifacts $ReleaseLibBuildMessages "
                "'release-lib build'" in windows
            )
            assert "$ReleaseDir = Split-Path -Parent $Hew" in windows
            assert "$ReleaseLibDir = Split-Path -Parent $ReleaseLib" in windows
            assert "& $Adze --version" in windows
            assert "& $HewLsp --version" in windows
            assert "& $HewObserve --version" in windows
            assert "Join-Path $ReleaseDir 'adze.exe'" not in windows
            assert "Join-Path $ReleaseDir 'hew-lsp.exe'" not in windows
            assert "Join-Path $ReleaseDir 'hew-observe.exe'" not in windows
            assert "cargo-output-dir.py" not in windows
            assert "Get-Command python" not in windows
        except AssertionError:
            continue
        raise AssertionError(
            "Windows Cargo JSON artifact mutation escaped the contract"
        )


def test_checkout_overwrite_mutations_are_rejected() -> None:
    original = validator()
    for mutation in (
        original + "\ngit pull --rebase origin main\n",
        original + "\ngit reset --hard origin/main\n",
        original.replace("rsync -az \\", "rsync -az --delete \\", 1),
    ):
        try:
            assert_isolated_staging(mutation)
        except AssertionError:
            continue
        raise AssertionError("remote-checkout overwrite mutation escaped the contract")


def test_ephemeral_output_exclusion_mutations_are_rejected() -> None:
    original = validator()
    for mutation in (
        original.replace(" --exclude .tmp", "", 1),
        original.replace("--exclude='./.tmp' ", "", 1),
        original.replace(" --exclude node_modules", "", 1),
        original.replace("--exclude='node_modules' ", "", 1),
    ):
        try:
            assert_isolated_staging(mutation)
        except AssertionError:
            continue
        raise AssertionError("ephemeral candidate output escaped the staging contract")


def test_linux_arm64_uses_only_the_staged_candidate() -> None:
    result, calls = run_with_fake_remote("linux-aarch64")
    assert result.returncode == 0, result.stdout + result.stderr
    assert "rsync " in calls
    assert "fake-linux-arm:/tmp/hew-pre-release.fake/" in calls
    assert "cd /tmp/hew-pre-release.fake" in calls
    assert "origin main" not in calls


def test_macos_uses_only_the_staged_candidate() -> None:
    result, calls = run_with_fake_remote("macos")
    assert result.returncode == 0, result.stdout + result.stderr
    assert "rsync " in calls
    assert "fake-macos:/tmp/hew-pre-release.fake/" in calls
    assert "cd /tmp/hew-pre-release.fake" in calls
    assert "origin main" not in calls


def test_macos_forwards_an_explicit_llvm_prefix_to_the_remote_shell() -> None:
    result, calls = run_with_fake_remote(
        "macos", {"HEW_MACOS_LLVM_PREFIX": "/custom/llvm 22"}
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "HEW_MACOS_LLVM_PREFIX=/custom/llvm\\ 22 bash -lc" in calls


def test_freebsd_uses_only_the_staged_candidate() -> None:
    result, calls = run_with_fake_remote("freebsd")
    assert result.returncode == 0, result.stdout + result.stderr
    assert "rsync " in calls
    assert "fake-freebsd:/tmp/hew-pre-release.fake/" in calls
    assert "cd /tmp/hew-pre-release.fake" in calls
    assert "origin main" not in calls


def test_windows_uses_only_the_staged_candidate() -> None:
    result, calls = run_with_fake_remote("windows")
    assert result.returncode == 0, result.stdout + result.stderr
    assert "fake-windows true" not in calls
    assert calls.count("-EncodedCommand") >= 2
    assert "scp " in calls
    assert (
        "fake-windows:C:/Temp/hew-pre-release-00000000-0000-0000-0000-000000000001/candidate.tar.gz"
        in calls
    )
    assert "windows-release-build.ps1" in validator()
    encoded_calls = [line for line in calls.splitlines() if "-EncodedCommand" in line]
    assert encoded_calls
    # Windows OpenSSH runs the remote invocation through cmd.exe. The staged
    # build launcher must stay well below cmd.exe's 8191-character limit;
    # the former inline build payload was far larger than this bound.
    assert max(map(len, encoded_calls)) < 4096
    assert "origin main" not in calls


def test_windows_accepts_a_safe_spacious_stage_root() -> None:
    stage = (
        "P:/hew pre-release stages/hew-pre-release-00000000-0000-0000-0000-000000000001"
    )
    result, calls = run_with_fake_remote(
        "windows",
        {
            "HEW_WINDOWS_STAGE_ROOT": "P:/hew pre-release stages",
            "FAKE_WINDOWS_STAGE": stage,
        },
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert f"fake-windows:{stage}/candidate.tar.gz" in calls


def test_windows_rejects_an_unsafe_stage_root_before_sync() -> None:
    result, calls = run_with_fake_remote(
        "windows", {"HEW_WINDOWS_STAGE_ROOT": "P:/hew-stages'; injected"}
    )
    assert result.returncode != 0
    assert "scp " not in calls


def test_malformed_remote_stage_is_rejected_before_sync() -> None:
    result, calls = run_with_fake_remote(
        "freebsd", {"FAKE_UNIX_STAGE": "/tmp/hew-pre-release.fake'; injected"}
    )
    assert result.returncode != 0
    assert "rsync " not in calls

    result, calls = run_with_fake_remote(
        "windows",
        {
            "FAKE_WINDOWS_STAGE": "C:/Temp/hew-pre-release-00000000-0000-0000-0000-000000000001'; injected"
        },
    )
    assert result.returncode != 0
    assert "scp " not in calls


def test_staging_failure_cannot_be_masked_by_a_later_remote_command() -> None:
    result, calls = run_with_fake_remote("freebsd", {"FAKE_RSYNC_EXIT": "1"})
    assert result.returncode != 0
    assert "rsync " in calls
    assert "bash -lc" not in calls


def test_requested_unreachable_host_fails_closed() -> None:
    with tempfile.TemporaryDirectory() as directory:
        temp = Path(directory)
        fake_ssh = temp / "ssh"
        fake_ssh.write_text("#!/usr/bin/env bash\nexit 1\n")
        fake_ssh.chmod(0o755)
        env = os.environ.copy()
        env.update(
            {
                "PATH": f"{temp}:{env['PATH']}",
                "HEW_FREEBSD_HOST": "unreachable-freebsd",
                "HEW_TIMEOUT_SSH_CHECK": "2",
            }
        )
        result = subprocess.run(
            ["bash", str(VALIDATOR), "freebsd"],
            cwd=ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
            timeout=10,
        )
    assert result.returncode != 0
    assert "Pre-release validation FAILED" in result.stdout
    assert "unreachable" in result.stdout


_FAKE_CARGO = r"""#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  metadata)
    exec "$REAL_CARGO" "$@"
    ;;
  build)
    exit 0
    ;;
  test)
    printf '%s\n' 'test result: ok. 1 passed; 0 failed' '' ''
    exit 0
    ;;
  *)
    printf 'unexpected fake cargo invocation: %s\n' "$*" >&2
    exit 93
    ;;
esac
"""

_FAKE_HEW = r"""#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  --version)
    printf '%s\n' 'hew 0.6.0-rc1'
    ;;
  run)
    printf '%s\n' 'Hello from Hew release test' 'pkg-smoke-ok'
    ;;
  build)
    output=''
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == '-o' ]]; then
        output="${2:-}"
        break
      fi
      shift
    done
    [[ -n "$output" ]] || exit 94
    printf '%s\n' '#!/usr/bin/env bash' \
      'printf "%s\\n" "release-native-link-ok"' > "$output"
    chmod +x "$output"
    ;;
  *)
    printf 'unexpected fake hew invocation: %s\n' "$*" >&2
    exit 95
    ;;
esac
"""

_FAKE_VERSION_BINARY = r"""#!/usr/bin/env bash
set -euo pipefail
[[ "${1:-}" == '--version' ]]
printf '%s\n' 'hew component 0.6.0-rc1'
"""

_FAKE_RUSTC = r"""#!/usr/bin/env bash
set -euo pipefail
output=''
while [[ $# -gt 0 ]]; do
  if [[ "$1" == '-o' ]]; then
    output="${2:-}"
    break
  fi
  shift
done
[[ -n "$output" ]] || exit 96
printf '%s\n' 'fake static archive' > "$output"
"""


def _write_executable(path: Path, source: str) -> None:
    path.write_text(source)
    path.chmod(0o755)


def _run_linux_target_dir_contract(
    target_dir: Path, *, cargo_target_dir: str | None, cargo_home: Path
) -> None:
    target_dir.mkdir(parents=True, exist_ok=True)
    release = target_dir / "release"
    release_lib = target_dir / "release-lib"
    release.mkdir()
    release_lib.mkdir()
    _write_executable(release / "hew", _FAKE_HEW)
    for name in ("adze", "hew-lsp", "hew-observe"):
        _write_executable(release / name, _FAKE_VERSION_BINARY)
    (release_lib / "libhew.a").write_text("fake archive")

    bin_dir = cargo_home / "test-bin"
    bin_dir.mkdir(parents=True)
    _write_executable(bin_dir / "cargo", _FAKE_CARGO)
    _write_executable(bin_dir / "rustc", _FAKE_RUSTC)
    _write_executable(bin_dir / "ldd", "#!/usr/bin/env bash\nexit 0\n")
    _write_executable(
        bin_dir / "uname",
        "#!/usr/bin/env bash\n"
        "case \"${1:-}\" in -s) printf 'Linux\\n' ;; -m) printf 'x86_64\\n' ;; esac\n",
    )
    _write_executable(bin_dir / "make", "#!/usr/bin/env bash\nexit 0\n")
    bash_env = cargo_home / "bash-env"
    bash_env.write_text(f'export PATH="{bin_dir}:$PATH"\n')

    env = os.environ.copy()
    env.update(
        {
            "PATH": f"{bin_dir}:{env['PATH']}",
            "BASH_ENV": str(bash_env),
            "CARGO_HOME": str(cargo_home),
            "REAL_CARGO": shutil.which("cargo", path=os.environ["PATH"]) or "cargo",
            "HEW_TIMEOUT_LOCAL_BUILD": "10",
            "HEW_TIMEOUT_SMOKE": "10",
            "HEW_TIMEOUT_TEST": "10",
        }
    )
    env.pop("CARGO_BUILD_TARGET", None)
    if cargo_target_dir is None:
        env.pop("CARGO_TARGET_DIR", None)
        (cargo_home / "config.toml").write_text(
            f'[build]\ntarget-dir = "{target_dir}"\n'
        )
    else:
        env["CARGO_TARGET_DIR"] = cargo_target_dir

    result = subprocess.run(
        ["bash", str(VALIDATOR), "linux"],
        cwd=ROOT,
        env=env,
        check=False,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "All platforms passed" in result.stdout


def test_linux_validator_honors_nondefault_cargo_target_dir() -> None:
    with tempfile.TemporaryDirectory(prefix="hew-prerelease-target-env-") as raw:
        temp = Path(raw)
        target = temp / "cargo artifacts with spaces"
        _run_linux_target_dir_contract(
            target, cargo_target_dir=str(target), cargo_home=temp / "cargo-home"
        )


def test_linux_validator_honors_cargo_build_target_dir_configuration() -> None:
    with tempfile.TemporaryDirectory(prefix="hew-prerelease-target-config-") as raw:
        temp = Path(raw)
        target = temp / "configured cargo artifacts"
        _run_linux_target_dir_contract(
            target, cargo_target_dir=None, cargo_home=temp / "cargo-home"
        )


_TESTS = [
    test_static_staging_contract,
    test_macos_llvm_discovery_contract,
    test_windows_toolchain_bootstrap_contract,
    test_windows_llvm_binding_contract,
    test_windows_staged_build_transport_contract,
    test_windows_remote_cleanup_contract,
    test_cargo_output_dir_contract,
    test_platform_identity_contract,
    test_macos_llvm_discovery_mutations_are_rejected,
    test_windows_toolchain_bootstrap_mutations_are_rejected,
    test_windows_llvm_binding_mutations_are_rejected,
    test_platform_identity_mutations_are_rejected,
    test_linux_validator_rejects_macos_before_building,
    test_windows_staged_build_transport_mutations_are_rejected,
    test_windows_remote_cleanup_mutations_are_rejected,
    test_cargo_output_dir_mutations_are_rejected,
    test_windows_cargo_json_artifact_mutations_are_rejected,
    test_checkout_overwrite_mutations_are_rejected,
    test_ephemeral_output_exclusion_mutations_are_rejected,
    test_linux_arm64_uses_only_the_staged_candidate,
    test_macos_uses_only_the_staged_candidate,
    test_macos_forwards_an_explicit_llvm_prefix_to_the_remote_shell,
    test_freebsd_uses_only_the_staged_candidate,
    test_windows_uses_only_the_staged_candidate,
    test_windows_accepts_a_safe_spacious_stage_root,
    test_windows_rejects_an_unsafe_stage_root_before_sync,
    test_malformed_remote_stage_is_rejected_before_sync,
    test_staging_failure_cannot_be_masked_by_a_later_remote_command,
    test_requested_unreachable_host_fails_closed,
    test_linux_validator_honors_nondefault_cargo_target_dir,
    test_linux_validator_honors_cargo_build_target_dir_configuration,
]


if __name__ == "__main__":
    failures = 0
    for test in _TESTS:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            failures += 1
            print(f"FAIL {test.__name__}: {exc}")
    if failures:
        raise SystemExit(f"{failures}/{len(_TESTS)} tests failed")
    print(f"All {len(_TESTS)} tests passed.")
