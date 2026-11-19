#!/usr/bin/env python3
"""Regression tests for the staging destination check (check-stage-dest.py).

Each case builds disposable Git checkouts with tracked sentinel files and runs
the validator from the caller's checkout, as the Makefile does.
"""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "check-stage-dest.py"

# Refused cases print "error: ..." on stderr; replay it behind a marker so a
# log reader cannot mistake this test's provoked diagnostics for a failure.
COUNTERFACTUAL_MARKER = "CF-"


def _git(cwd: Path, *args: str) -> None:
    subprocess.run(
        ["git", "-c", "user.name=t", "-c", "user.email=t@example.invalid", *args],
        cwd=cwd,
        check=True,
        capture_output=True,
    )


def _checkout(path: Path) -> Path:
    """A Git checkout with tracked std/, bin/ and lib/ sentinels."""
    for relative in ("std/string.hew", "bin/hew", "lib/libhew.a", "scripts/lib/x.sh"):
        file = path / relative
        file.parent.mkdir(parents=True, exist_ok=True)
        file.write_text("sentinel\n")
    _git(path, "init", "-q")
    _git(path, "add", ".")
    _git(path, "commit", "-qm", "sentinels")
    return path


def _check(caller: Path, build_dir: str, label: str) -> tuple[int, str]:
    result = subprocess.run(
        [sys.executable, str(SCRIPT), build_dir],
        cwd=caller,
        capture_output=True,
        text=True,
        check=False,
    )
    for line in result.stderr.splitlines():
        print(f"{COUNTERFACTUAL_MARKER}[{label}] {line}")
    return result.returncode, result.stderr


def _link(link: Path, target: Path) -> None:
    link.parent.mkdir(parents=True, exist_ok=True)
    os.symlink(target, link, target_is_directory=True)


def _assert_allowed(caller: Path, build_dir: str, label: str) -> None:
    code, stderr = _check(caller, build_dir, label)
    assert code == 0, f"{label}: expected allowed, got {code}: {stderr}"


def _assert_refused(caller: Path, build_dir: str, label: str, reason: str) -> None:
    code, stderr = _check(caller, build_dir, label)
    assert code == 1, f"{label}: expected refusal, got {code}: {stderr}"
    assert reason in stderr, f"{label}: expected {reason!r} in {stderr!r}"


def test_staging_destinations() -> None:
    with tempfile.TemporaryDirectory() as scratch_name:
        scratch = Path(os.path.realpath(scratch_name))
        caller = _checkout(scratch / "caller")
        neighbour = _checkout(scratch / "neighbour")

        # The default layout: absent, a fresh link, and a directory copy of
        # std (what Git Bash's ln leaves behind) are all safe.
        _assert_allowed(caller, "build", "fresh build/")
        _link(caller / "build" / "std", Path("../std"))
        (caller / "build" / "bin").mkdir()
        _assert_allowed(caller, "build", "staged build/")
        (caller / "build" / "std").unlink()
        (caller / "build" / "std").mkdir()
        (caller / "build" / "std" / "string.hew").write_text("copy\n")
        _assert_allowed(caller, "build", "std directory copy")

        # Out of tree, including a path with spaces and the word `build`.
        _assert_allowed(caller, str(scratch / "out of tree" / "separate build"), "spaced")
        untracked = scratch / "untracked bin"
        untracked.mkdir()
        _link(scratch / "linked-untracked" / "bin", untracked)
        _assert_allowed(caller, str(scratch / "linked-untracked"), "untracked link")

        # Empty and root destinations.
        _assert_refused(caller, "", "empty", "is empty")
        _assert_refused(caller, " ", "blank", "is empty")
        _assert_refused(caller, os.path.abspath(os.sep), "root", "filesystem root")

        # The caller's own sources: the checkout itself, an alias of it, and
        # a directory whose lib/ is tracked.
        _assert_refused(caller, ".", "checkout root", "tracked sources")
        _link(scratch / "alias", caller)
        _assert_refused(caller, str(scratch / "alias"), "alias", "tracked sources")
        _assert_refused(caller, "scripts", "tracked lib/", "tracked sources")

        # bin/ and lib/ reached through links into tracked sources.
        _link(caller / "linked" / "bin", caller / "bin")
        _assert_refused(caller, "linked", "linked bin/", "tracked sources")
        _link(scratch / "system-linked" / "bin", Path(os.path.abspath(os.sep)))
        _assert_refused(caller, str(scratch / "system-linked"), "bin at a root", "system directory")

        # A neighbouring checkout is another repository's sources.
        _assert_refused(caller, str(neighbour), "neighbour checkout", "tracked sources")
        _link(scratch / "to-neighbour" / "lib", neighbour / "lib")
        _assert_refused(caller, str(scratch / "to-neighbour"), "neighbour lib/", "tracked sources")

        # Destinations overlapping the caller's std/ form a source/output cycle.
        _assert_refused(caller, "std", "std itself", "standard library sources")
        _assert_refused(caller, "std/out", "inside std", "standard library sources")
        _link(scratch / "into-std" / "bin", caller / "std")
        _assert_refused(caller, str(scratch / "into-std"), "bin into std", "standard library sources")

        # Nothing the checks looked at was changed.
        for checkout in (caller, neighbour):
            status = subprocess.run(
                ["git", "status", "--porcelain", "--untracked-files=no"],
                cwd=checkout,
                capture_output=True,
                text=True,
                check=True,
            ).stdout
            assert status == "", f"{checkout.name} changed: {status}"


def main() -> int:
    test_staging_destinations()
    print("check-stage-dest: all staging destination cases pass")
    return 0


if __name__ == "__main__":
    sys.exit(main())
