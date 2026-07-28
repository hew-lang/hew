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
    return subprocess.run(
        [
            sys.executable,
            str(RUNNER),
            "--hew-bin",
            str(compiler),
            "--label",
            "selftest",
            f"--timeout-seconds={timeout_seconds}",
            *inventory,
        ],
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
    result = invoke(compiler, *inventory, timeout_seconds=timeout_seconds)
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


def main() -> None:
    assert_timeout_exit_race_is_classified()

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
        expect_status(0, compiler, "--source-root", str(good), contains="1 passed")

        crlf = root / "crlf"
        write(crlf / "crlf_output.hew", "fn main() {}\n")
        write(crlf / "crlf_output.expected", "expected\n")
        write(crlf / "crlf_expectation.hew", "fn main() {}\n")
        (crlf / "crlf_expectation.expected").write_bytes(b"expected\r\n")
        expect_status(
            0,
            compiler,
            "--source-root",
            str(crlf),
            contains="2 passed",
        )

        fractional = root / "fractional.hew"
        write(fractional, "fn main() {}\n")
        write(fractional.with_suffix(".expected"), "expected\n")
        expect_status(
            0,
            compiler,
            "--source",
            str(fractional),
            timeout_seconds="0.1234",
            contains="1 passed",
        )

        missing = root / "missing"
        write(missing / "missing.hew", "fn main() {}\n")
        expect_status(
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
        expect_status(
            1,
            compiler,
            "--source-root",
            str(orphan),
            contains="orphan expectation",
        )

        empty = root / "empty"
        empty.mkdir()
        expect_status(
            1,
            compiler,
            "--source-root",
            str(empty),
            contains="contains no .hew files",
        )

        duplicate = root / "duplicate"
        write(duplicate / "ok.hew", "fn main() {}\n")
        write(duplicate / "ok.expected", "expected\n")
        expect_status(
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
        expect_status(
            1,
            compiler,
            "--source",
            str(malformed),
            contains="must end in .hew",
        )

        nonzero = root / "nonzero.hew"
        write(nonzero, "fn main() {}\n")
        write(nonzero.with_suffix(".expected"), "expected\n")
        expect_status(
            1,
            compiler,
            "--source",
            str(nonzero),
            contains="exited with status 7",
        )

        drift = root / "drift.hew"
        write(drift, "fn main() {}\n")
        write(drift.with_suffix(".expected"), "expected\n")
        expect_status(
            1,
            compiler,
            "--source",
            str(drift),
            contains="combined stdout/stderr differs",
        )

        hang = root / "hang.hew"
        write(hang, "fn main() {}\n")
        write(hang.with_suffix(".expected"), "")
        expect_status(
            1,
            compiler,
            "--source",
            str(hang),
            contains="timed out after 0.1s",
            excludes=("exited with status",),
        )

        not_executable = root / "not-executable"
        write(not_executable, "#!/bin/sh\nexit 0\n")
        expect_status(
            1,
            not_executable,
            "--source",
            str(nonzero),
            contains="compiler is not executable",
        )

        expect_status(
            1,
            compiler,
            "--source",
            str(nonzero),
            timeout_seconds="0",
            contains="must be finite and greater than zero",
        )

        for invalid_timeout in ("nan", "inf", "-inf"):
            expect_status(
                1,
                compiler,
                "--source",
                str(nonzero),
                timeout_seconds=invalid_timeout,
                contains="must be finite and greater than zero",
            )

    print("example-expectations selftest: 18/18 counterfactuals PASS")


if __name__ == "__main__":
    main()
