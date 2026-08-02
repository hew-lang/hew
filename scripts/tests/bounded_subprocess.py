"""Small fail-closed subprocess runner for repository evidence scripts."""

from __future__ import annotations

import os
import signal
import subprocess
from collections.abc import Mapping, Sequence
from pathlib import Path

try:
    import resource
except ImportError:  # pragma: no cover - Windows does not expose POSIX rlimits.
    resource = None  # type: ignore[assignment]

MAX_OUTPUT_FILE_BYTES = 512 * 1024 * 1024
MAX_OPEN_FILES = 1024


def _set_resource_limits(timeout_seconds: int) -> None:
    if resource is None:
        return

    def cap(kind: int, requested: int) -> None:
        _, hard = resource.getrlimit(kind)
        value = requested if hard == resource.RLIM_INFINITY else min(requested, hard)
        resource.setrlimit(kind, (value, value))

    cap(resource.RLIMIT_CPU, timeout_seconds + 5)
    cap(resource.RLIMIT_FSIZE, MAX_OUTPUT_FILE_BYTES)
    cap(resource.RLIMIT_NOFILE, MAX_OPEN_FILES)


def run(
    command: Sequence[str],
    *,
    cwd: Path,
    timeout_seconds: int,
    env: Mapping[str, str] | None = None,
) -> subprocess.CompletedProcess[str]:
    """Run with wall-clock, CPU, output-file, and descriptor bounds."""

    process = subprocess.Popen(
        command,
        cwd=cwd,
        env=env,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        start_new_session=os.name == "posix",
        preexec_fn=(
            (lambda: _set_resource_limits(timeout_seconds))
            if os.name == "posix"
            else None
        ),
    )
    try:
        stdout, stderr = process.communicate(timeout=timeout_seconds)
    except subprocess.TimeoutExpired as error:
        if os.name == "posix":
            os.killpg(process.pid, signal.SIGKILL)
        else:  # pragma: no cover - exercised only on Windows.
            process.kill()
        stdout, stderr = process.communicate()
        rendered = " ".join(command)
        raise AssertionError(
            f"subprocess exceeded {timeout_seconds}s: {rendered}\n"
            f"stdout:\n{stdout}\nstderr:\n{stderr}"
        ) from error
    return subprocess.CompletedProcess(command, process.returncode, stdout, stderr)
