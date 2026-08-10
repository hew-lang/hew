#!/usr/bin/env python3
"""Behavioral counterfactuals for scripts/example-expectations.py."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import subprocess
import sys
import tempfile


ROOT = Path(__file__).resolve().parents[2]
RUNNER = ROOT / "scripts/example-expectations.py"


def load_runner_module():
    spec = importlib.util.spec_from_file_location("example_expectations", RUNNER)
    if spec is None or spec.loader is None:
        raise AssertionError(f"could not load runner module from {RUNNER}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def invoke(
    compiler: Path,
    *inventory: str,
    timeout_seconds: str = "0.1",
) -> subprocess.CompletedProcess[str]:
    args = [
        sys.executable,
        str(RUNNER),
        "--hew-bin",
        str(compiler),
        "--label",
        "selftest",
        f"--timeout-seconds={timeout_seconds}",
    ]
    args.extend(inventory)
    return subprocess.run(
        args,
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )


def expect_status(
    expected: int,
    compiler: Path,
    *inventory: str,
    contains: str,
    timeout_seconds: str = "0.1",
    excludes: tuple[str, ...] = (),
) -> None:
    result = invoke(
        compiler,
        *inventory,
        timeout_seconds=timeout_seconds,
    )
    combined = result.stdout + result.stderr
    unexpected = [text for text in excludes if text in combined]
    if result.returncode != expected or contains not in combined or unexpected:
        raise AssertionError(
            f"expected status {expected} containing {contains!r}, "
            f"excluding {unexpected!r}, got status {result.returncode}:\n{combined}"
        )


def assert_timeout_exit_race_is_classified() -> None:
    module = load_runner_module()

    class ExitedProcess:
        pid = 424242
        returncode = 0

        def __init__(self) -> None:
            self.communicate_calls = 0

        def communicate(self, *, timeout=None):
            self.communicate_calls += 1
            if self.communicate_calls == 1:
                raise subprocess.TimeoutExpired(["fake-hew"], timeout)
            return b"finished during timeout cleanup\n", None

        def kill(self) -> None:
            raise ProcessLookupError("already exited")

        def poll(self) -> int:
            return 0

    process = ExitedProcess()
    original_popen = module.subprocess.Popen
    original_killpg = getattr(module.os, "killpg", None)
    try:
        module.subprocess.Popen = lambda *_args, **_kwargs: process

        def already_exited(*_args, **_kwargs) -> None:
            raise ProcessLookupError("already exited")

        if module.os.name != "nt":
            module.os.killpg = already_exited
        status, output = module.run_process(["fake-hew"], 0.001)
    finally:
        module.subprocess.Popen = original_popen
        if original_killpg is not None:
            module.os.killpg = original_killpg

    if status is not None or output != b"finished during timeout cleanup\n":
        raise AssertionError(
            "timeout/exit race must remain a clean timeout classification; "
            f"got status={status!r}, output={output!r}"
        )


def assert_still_live_kill_failure_is_fatal() -> None:
    module = load_runner_module()

    class LiveProcess:
        pid = 434343
        returncode = None

        def __init__(self) -> None:
            self.communicate_calls = 0

        def communicate(self, *, timeout=None):
            self.communicate_calls += 1
            if self.communicate_calls == 1:
                raise subprocess.TimeoutExpired(["fake-hew"], timeout)
            raise AssertionError(
                "run_process must not enter unbounded communicate() after a "
                "still-live process resists termination"
            )

        def kill(self) -> None:
            raise PermissionError("still-live process resisted termination")

        def poll(self) -> None:
            return None

    process = LiveProcess()
    original_popen = module.subprocess.Popen
    original_killpg = getattr(module.os, "killpg", None)
    try:
        module.subprocess.Popen = lambda *_args, **_kwargs: process

        def kill_failed(*_args, **_kwargs) -> None:
            raise PermissionError("still-live process resisted termination")

        if module.os.name != "nt":
            module.os.killpg = kill_failed
        try:
            module.run_process(["fake-hew"], 0.001)
        except PermissionError as error:
            if "resisted termination" not in str(error):
                raise AssertionError(
                    f"unexpected termination failure: {error}"
                ) from error
        else:
            raise AssertionError(
                "a still-live process termination failure must remain fatal"
            )
    finally:
        module.subprocess.Popen = original_popen
        if original_killpg is not None:
            module.os.killpg = original_killpg

    if process.communicate_calls != 1:
        raise AssertionError(
            "still-live termination failure must not enter a second, unbounded "
            f"communicate(); got {process.communicate_calls} calls"
        )


def main() -> None:
    counterfactuals = 0

    def check_status(
        expected: int,
        compiler: Path,
        *inventory: str,
        contains: str,
        timeout_seconds: str = "0.1",
        excludes: tuple[str, ...] = (),
    ) -> None:
        nonlocal counterfactuals
        expect_status(
            expected,
            compiler,
            *inventory,
            contains=contains,
            timeout_seconds=timeout_seconds,
            excludes=excludes,
        )
        counterfactuals += 1

    assert_timeout_exit_race_is_classified()
    counterfactuals += 1
    assert_still_live_kill_failure_is_fatal()
    counterfactuals += 1

    with tempfile.TemporaryDirectory(prefix="hew-example-expectations-") as temp:
        root = Path(temp)
        compiler = root / "fake-hew"
        write(
            compiler,
            """#!/usr/bin/env python3
import pathlib
import sys
import time

if len(sys.argv) != 5:
    print(f"unexpected argv length: {sys.argv!r}")
    raise SystemExit(91)
if sys.argv[1] != "run":
    print(f"unexpected subcommand: {sys.argv[1]!r}")
    raise SystemExit(92)
if sys.argv[2] != "--timeout":
    print(f"missing --timeout: {sys.argv!r}")
    raise SystemExit(93)

source = pathlib.Path(sys.argv[-1]).stem
expected_timeout = "124ms" if source == "fractional" else "100ms"
if sys.argv[3] != expected_timeout:
    print(f"unexpected timeout: {sys.argv[3]!r}, expected {expected_timeout!r}")
    raise SystemExit(94)
if not pathlib.Path(sys.argv[4]).is_file():
    print(f"source is not the final argv entry: {sys.argv!r}")
    raise SystemExit(95)

if source == "hang":
    time.sleep(10)
elif source == "crlf_output":
    sys.stdout.buffer.write(b"expected\\r\\n")
elif source == "nonzero":
    print("expected")
    raise SystemExit(7)
elif source == "drift":
    print("actual")
else:
    print("expected")
""",
        )
        compiler.chmod(0o755)

        good = root / "good"
        write(good / "ok.hew", "fn main() {}\n")
        write(good / "ok.expected", "expected\n")
        check_status(0, compiler, "--source-root", str(good), contains="1 passed")

        check_status(
            1,
            compiler,
            "--source-root",
            str(good),
            "--source-root",
            str(good),
            contains="duplicate source root",
        )

        not_a_directory = root / "not-a-directory"
        write(not_a_directory, "not a directory\n")
        check_status(
            1,
            compiler,
            "--source-root",
            str(not_a_directory),
            contains="source root is not a directory",
        )

        check_status(
            1,
            compiler,
            contains="inventory is empty",
        )

        crlf = root / "crlf"
        write(crlf / "crlf_output.hew", "fn main() {}\n")
        write(crlf / "crlf_output.expected", "expected\n")
        write(crlf / "crlf_expectation.hew", "fn main() {}\n")
        (crlf / "crlf_expectation.expected").write_bytes(b"expected\r\n")
        check_status(
            0,
            compiler,
            "--source-root",
            str(crlf),
            contains="2 passed",
        )

        fractional = root / "fractional.hew"
        write(fractional, "fn main() {}\n")
        write(fractional.with_suffix(".expected"), "expected\n")
        check_status(
            0,
            compiler,
            "--source",
            str(fractional),
            timeout_seconds="0.1234",
            contains="1 passed",
        )

        missing = root / "missing"
        write(missing / "missing.hew", "fn main() {}\n")
        check_status(
            1,
            compiler,
            "--source-root",
            str(missing),
            contains="source has no paired .expected",
        )

        orphan = root / "orphan"
        write(orphan / "ok.hew", "fn main() {}\n")
        write(orphan / "ok.expected", "expected\n")
        write(orphan / "gone.expected", "stale\n")
        check_status(
            1,
            compiler,
            "--source-root",
            str(orphan),
            contains="orphan expectation",
        )

        empty = root / "empty"
        empty.mkdir()
        check_status(
            1,
            compiler,
            "--source-root",
            str(empty),
            contains="contains no .hew files",
        )

        duplicate = root / "duplicate"
        write(duplicate / "ok.hew", "fn main() {}\n")
        write(duplicate / "ok.expected", "expected\n")
        check_status(
            1,
            compiler,
            "--source-root",
            str(duplicate),
            "--source",
            str(duplicate / "ok.hew"),
            contains="duplicate source admission",
        )

        malformed = root / "not-hew.txt"
        write(malformed, "not a source\n")
        check_status(
            1,
            compiler,
            "--source",
            str(malformed),
            contains="must end in .hew",
        )

        nonzero = root / "nonzero.hew"
        write(nonzero, "fn main() {}\n")
        write(nonzero.with_suffix(".expected"), "expected\n")
        check_status(
            1,
            compiler,
            "--source",
            str(nonzero),
            contains="exited with status 7",
        )

        drift = root / "drift.hew"
        write(drift, "fn main() {}\n")
        write(drift.with_suffix(".expected"), "expected\n")
        check_status(
            1,
            compiler,
            "--source",
            str(drift),
            contains="combined stdout/stderr differs",
        )

        hang = root / "hang.hew"
        write(hang, "fn main() {}\n")
        write(hang.with_suffix(".expected"), "")
        check_status(
            1,
            compiler,
            "--source",
            str(hang),
            contains="exceeded runner deadline 2.1s (Hew timeout 0.1s)",
            excludes=("exited with status",),
        )

        not_executable = root / "not-executable"
        write(not_executable, "#!/bin/sh\nexit 0\n")
        check_status(
            1,
            not_executable,
            "--source",
            str(nonzero),
            contains="compiler is not executable",
        )

        check_status(
            1,
            compiler,
            "--source",
            str(nonzero),
            timeout_seconds="0",
            contains="must be finite and greater than zero",
        )

        for invalid_timeout in ("nan", "inf", "-inf"):
            check_status(
                1,
                compiler,
                "--source",
                str(nonzero),
                timeout_seconds=invalid_timeout,
                contains="must be finite and greater than zero",
            )

    print(
        "example-expectations selftest: "
        f"{counterfactuals}/{counterfactuals} counterfactuals PASS"
    )


if __name__ == "__main__":
    main()
