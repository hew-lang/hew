#!/usr/bin/env python3
"""Run evidence subprocesses with bounded time, memory, and descendants."""

from __future__ import annotations

import ctypes
import os
import platform
import signal
import subprocess
import threading
from collections.abc import Mapping, Sequence
from pathlib import Path

try:
    import resource
except ImportError:  # pragma: no cover - Windows has no resource module.
    resource = None  # type: ignore[assignment]


DEFAULT_TIMEOUT_SECONDS = int(os.environ.get("HEW_EVIDENCE_TIMEOUT_SECONDS", "300"))
DEFAULT_MEMORY_MB = int(os.environ.get("HEW_EVIDENCE_MEMORY_MB", "16384"))


def run_bounded(
    command: Sequence[str],
    *,
    cwd: Path,
    text: bool = True,
    capture_output: bool = True,
    env: Mapping[str, str] | None = None,
    input: str | None = None,
    timeout_seconds: float = DEFAULT_TIMEOUT_SECONDS,
    memory_mb: int = DEFAULT_MEMORY_MB,
) -> subprocess.CompletedProcess[str]:
    """Run one command and kill its process group on an expired deadline."""

    if timeout_seconds <= 0:
        raise ValueError("timeout_seconds must be positive")
    if memory_mb <= 0:
        raise ValueError("memory_mb must be positive")
    if os.name != "posix" or resource is None:
        raise RuntimeError(
            "evidence subprocess memory/process-tree containment requires POSIX setrlimit"
        )

    def constrain_child() -> None:
        os.setsid()
        if resource is not None and platform.system() != "Darwin":
            limit = memory_mb * 1024 * 1024
            _, hard = resource.getrlimit(resource.RLIMIT_AS)
            soft = limit if hard == resource.RLIM_INFINITY else min(limit, hard)
            resource.setrlimit(resource.RLIMIT_AS, (soft, hard))

    process = subprocess.Popen(
        list(command),
        cwd=cwd,
        text=text,
        stdout=subprocess.PIPE if capture_output else None,
        stderr=subprocess.PIPE if capture_output else None,
        stdin=subprocess.PIPE if input is not None else None,
        env=dict(env) if env is not None else None,
        preexec_fn=constrain_child,
    )
    memory_exceeded = threading.Event()
    monitor_done = threading.Event()

    def monitor_darwin_process_group() -> None:
        class ProcTaskInfo(ctypes.Structure):
            _fields_ = [
                ("virtual_size", ctypes.c_uint64),
                ("resident_size", ctypes.c_uint64),
                ("total_user", ctypes.c_uint64),
                ("total_system", ctypes.c_uint64),
                ("threads_user", ctypes.c_uint64),
                ("threads_system", ctypes.c_uint64),
                *[
                    (name, ctypes.c_int32)
                    for name in (
                        "policy",
                        "faults",
                        "pageins",
                        "cow_faults",
                        "messages_sent",
                        "messages_received",
                        "syscalls_mach",
                        "syscalls_unix",
                        "csw",
                        "threadnum",
                        "numrunning",
                        "priority",
                    )
                ],
            ]

        libproc = ctypes.CDLL("/usr/lib/libproc.dylib", use_errno=True)
        pids = (ctypes.c_int32 * 8192)()
        ceiling_bytes = memory_mb * 1024 * 1024
        while not monitor_done.wait(0.02):
            try:
                ctypes.memset(ctypes.byref(pids), 0, ctypes.sizeof(pids))
                byte_count = libproc.proc_listpgrppids(
                    process.pid, ctypes.byref(pids), ctypes.sizeof(pids)
                )
                if byte_count < 0:
                    continue
                resident_bytes = 0
                # libproc variants have historically been described with both
                # count- and byte-count return conventions.  The buffer is
                # zero-initialized, so enumerate the actual populated PID
                # entries and remain correct under either convention.
                populated = next(
                    (index for index, pid in enumerate(pids) if pid == 0), len(pids)
                )
                for index in range(populated):
                    info = ProcTaskInfo()
                    read = libproc.proc_pidinfo(
                        pids[index], 4, 0, ctypes.byref(info), ctypes.sizeof(info)
                    )
                    if read == ctypes.sizeof(info):
                        resident_bytes += info.resident_size
                if resident_bytes > ceiling_bytes:
                    memory_exceeded.set()
                    os.killpg(process.pid, signal.SIGKILL)
                    return
            except (OSError, ValueError):
                continue

    monitor = None
    if platform.system() == "Darwin":
        monitor = threading.Thread(target=monitor_darwin_process_group, daemon=True)
        monitor.start()
    try:
        stdout, stderr = process.communicate(input=input, timeout=timeout_seconds)
    except subprocess.TimeoutExpired as error:
        os.killpg(process.pid, signal.SIGKILL)
        stdout, stderr = process.communicate()
        raise AssertionError(
            f"subprocess exceeded {timeout_seconds:g}s deadline: {' '.join(command)}\n"
            f"stdout:\n{stdout or ''}\nstderr:\n{stderr or ''}"
        ) from error
    finally:
        monitor_done.set()
        if monitor is not None:
            monitor.join(timeout=1)
    if memory_exceeded.is_set():
        stderr = (
            stderr or ""
        ) + f"\nsubprocess exceeded {memory_mb} MiB memory ceiling\n"
    return subprocess.CompletedProcess(command, process.returncode, stdout, stderr)


def assert_bounding_contract(root: Path) -> None:
    """Counterfactuals prove both the deadline and memory ceiling fail red."""

    try:
        run_bounded(
            ["python3", "-c", "import time; time.sleep(10)"],
            cwd=root,
            timeout_seconds=0.05,
            memory_mb=512,
        )
    except AssertionError as error:
        if "exceeded" not in str(error):
            raise
    else:
        raise AssertionError("subprocess deadline counterfactual unexpectedly passed")

    result = run_bounded(
        [
            "python3",
            "-c",
            "import time; value = bytearray(512 * 1024 * 1024); "
            "value[::4096] = b'x' * (len(value) // 4096); time.sleep(2)",
        ],
        cwd=root,
        timeout_seconds=10,
        memory_mb=128,
    )
    output = (result.stdout or "") + (result.stderr or "")
    expected_witness = (
        "memory ceiling" if platform.system() == "Darwin" else "MemoryError"
    )
    if result.returncode == 0 or expected_witness not in output:
        raise AssertionError(
            "subprocess memory counterfactual lacked the bound-specific failure witness"
        )
