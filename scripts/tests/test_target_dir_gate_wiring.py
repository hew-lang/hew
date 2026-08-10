#!/usr/bin/env python3
"""Prove RC1 language gates consume Cargo's configured debug output."""

from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
TARGETS = (
    "test-vertical-slice",
    "test-pkg-import",
    "test-package-install",
    "fuzz-oracle",
    "fuzz-oracle-selftest",
    "test-hew-ratchet",
    "test-o2-differential",
    "test-stdlib-ratchet",
    "test-stdlib-execution-proofs",
    "test-doc-examples",
    "test-ux-examples",
    "test-surface-examples",
    "hew-check-all",
    "checked-mir-verify",
    "checked-mir-golden",
    "checked-mir-run",
    "checked-mir-expect",
    "ll-diff",
    "ll-golden",
)
XTASK_TARGETS = {
    "test-vertical-slice": "vertical-slice",
    "test-stdlib-ratchet": "stdlib-ratchet",
}


def preview(target: str, target_dir: str) -> str:
    env = os.environ.copy()
    env["CARGO_TARGET_DIR"] = target_dir
    result = subprocess.run(
        ["make", "-n", target],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    return result.stdout


def assert_all_gates_use(target_dir: str, expected_debug_dir: str) -> None:
    expected_hew = f"{expected_debug_dir}/debug/hew"
    for target in TARGETS:
        recipe = preview(target, target_dir)
        if target in XTASK_TARGETS:
            expected = f"cargo xtask gate {XTASK_TARGETS[target]}"
            assert expected in recipe, f"{target} bypassed {expected}:\n{recipe}"
            continue
        assert expected_hew in recipe, f"{target} did not use {expected_hew}:\n{recipe}"
        assert "target/debug/hew" not in recipe, (
            f"{target} retained a repository-default compiler path:\n{recipe}"
        )


def test_absolute_target_dir() -> None:
    with tempfile.TemporaryDirectory(prefix="hew-rc1-gate-absolute-") as raw:
        assert_all_gates_use(raw, raw)


def test_relative_target_dir() -> None:
    directory = Path(tempfile.mkdtemp(prefix=".tmp-hew-rc1-gate-", dir=ROOT))
    try:
        relative = str(directory.relative_to(ROOT))
        assert_all_gates_use(relative, relative)
    finally:
        shutil.rmtree(directory)


_TESTS = (test_absolute_target_dir, test_relative_target_dir)


if __name__ == "__main__":
    failures = 0
    for test in _TESTS:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(_TESTS)} tests failed")
    print(f"All {len(_TESTS)} tests passed.")
