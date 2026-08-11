#!/usr/bin/env python3
"""Counterfactual tests for the adjacent-stderr Hew compiletest harness."""

from __future__ import annotations

import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HARNESS = ROOT / "scripts/hew-compiletest.py"


def run_case(
    compiler_body: str, expected: str | None
) -> subprocess.CompletedProcess[str]:
    with tempfile.TemporaryDirectory(prefix="hew-compiletest-") as raw:
        directory = Path(raw)
        fixture = directory / "case.hew"
        fixture.write_text(
            "// compiletest: check-fail\nfn main() {}\n", encoding="utf-8"
        )
        if expected is not None:
            fixture.with_suffix(".stderr").write_text(expected, encoding="utf-8")
        compiler = directory / "hew"
        compiler.write_text(f"#!/bin/sh\n{compiler_body}\n", encoding="utf-8")
        compiler.chmod(0o755)
        return subprocess.run(
            [
                "python3",
                str(HARNESS),
                "--hew-bin",
                str(compiler),
                "--directory",
                str(directory),
            ],
            cwd=ROOT,
            text=True,
            capture_output=True,
            check=False,
        )


def test_matching_rejection_passes() -> None:
    result = run_case("echo 'error: intended' >&2; exit 1", "error: intended\n")
    assert result.returncode == 0, result.stdout + result.stderr


def test_unexpected_pass_fails() -> None:
    result = run_case("exit 0", "")
    assert result.returncode == 1, result.stdout + result.stderr
    assert "expected check exit 1, got 0" in result.stderr


def test_diagnostic_drift_fails() -> None:
    result = run_case("echo 'error: changed' >&2; exit 1", "error: intended\n")
    assert result.returncode == 1, result.stdout + result.stderr
    assert "stderr differs" in result.stderr


if __name__ == "__main__":
    tests = (
        test_matching_rejection_passes,
        test_unexpected_pass_fails,
        test_diagnostic_drift_fails,
    )
    for test in tests:
        test()
        print(f"PASS {test.__name__}")
    print(f"All {len(tests)} tests passed.")
