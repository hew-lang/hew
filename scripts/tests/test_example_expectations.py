#!/usr/bin/env python3
"""Behavioral counterfactuals for scripts/example-expectations.py."""

from __future__ import annotations

from pathlib import Path
import subprocess
import sys
import tempfile


ROOT = Path(__file__).resolve().parents[2]
RUNNER = ROOT / "scripts/example-expectations.py"


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
) -> None:
    result = invoke(compiler, *inventory, timeout_seconds=timeout_seconds)
    combined = result.stdout + result.stderr
    if result.returncode != expected or contains not in combined:
        raise AssertionError(
            f"expected status {expected} containing {contains!r}, "
            f"got status {result.returncode}:\n{combined}"
        )


def main() -> None:
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
expected_timeout = "125ms" if source == "fractional" else "100ms"
if sys.argv[3] != expected_timeout:
    print(f"unexpected timeout: {sys.argv[3]!r}, expected {expected_timeout!r}")
    raise SystemExit(94)
if not pathlib.Path(sys.argv[4]).is_file():
    print(f"source is not the final argv entry: {sys.argv!r}")
    raise SystemExit(95)

if source == "hang":
    time.sleep(10)
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

        fractional = root / "fractional.hew"
        write(fractional, "fn main() {}\n")
        write(fractional.with_suffix(".expected"), "expected\n")
        expect_status(
            0,
            compiler,
            "--source",
            str(fractional),
            timeout_seconds="0.125",
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

    print("example-expectations selftest: 15/15 counterfactuals PASS")


if __name__ == "__main__":
    main()
