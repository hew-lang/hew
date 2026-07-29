"""Executable contract for local-candidate host validation.

The validator must test the caller's working tree in an isolated temporary
directory. It must never update, reset, or delete an existing remote checkout,
and a requested but unavailable platform must fail the run.
"""

import os
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
VALIDATOR = ROOT / "scripts" / "pre-release-validate.sh"


def validator() -> str:
    return VALIDATOR.read_text()


def assert_isolated_staging(text: str) -> None:
    assert "mktemp -d /tmp/hew-pre-release.XXXXXX" in text
    assert "hew-pre-release-' + [guid]::NewGuid()" in text
    assert "^/tmp/hew-pre-release\\.[A-Za-z0-9._-]+$" in text
    assert "^[A-Za-z]:/[A-Za-z0-9._/\\ -]*/hew-pre-release-[0-9A-Fa-f-]+$" in text
    assert "--delete" not in text
    assert "git pull" not in text
    assert "git reset" not in text
    assert "git fetch" not in text
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


_FAKE_SSH = r"""#!/usr/bin/env bash
set -eu
printf 'ssh %s\n' "$*" >> "$FAKE_REMOTE_LOG"
case "$*" in
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
    if [ "$count" -eq 1 ]; then
      printf '%s\n' "${FAKE_WINDOWS_STAGE:-C:/Temp/hew-pre-release-00000000-0000-0000-0000-000000000001}"
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
    assert "scp " in calls
    assert (
        "fake-windows:C:/Temp/hew-pre-release-00000000-0000-0000-0000-000000000001/candidate.tar.gz"
        in calls
    )
    assert "origin main" not in calls


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


_TESTS = [
    test_static_staging_contract,
    test_macos_llvm_discovery_contract,
    test_macos_llvm_discovery_mutations_are_rejected,
    test_checkout_overwrite_mutations_are_rejected,
    test_linux_arm64_uses_only_the_staged_candidate,
    test_macos_uses_only_the_staged_candidate,
    test_macos_forwards_an_explicit_llvm_prefix_to_the_remote_shell,
    test_freebsd_uses_only_the_staged_candidate,
    test_windows_uses_only_the_staged_candidate,
    test_malformed_remote_stage_is_rejected_before_sync,
    test_staging_failure_cannot_be_masked_by_a_later_remote_command,
    test_requested_unreachable_host_fails_closed,
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
