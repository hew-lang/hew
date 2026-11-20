#!/usr/bin/env python3
"""Refuse a BUILD_DIR that staging would turn against source files.

`make hew`, `hew-debug`, `assemble` and `assemble-release` stage links under
BUILD_DIR: they remove and recreate `bin/`, `lib/` and `std/` there, and write
the compiler and runtime into `bin/` and `lib/`. Each runs this check first,
from the caller's checkout (the working directory), before it changes
anything:

  check-stage-dest.py BUILD_DIR

It exits 0 when staging is safe and 1, naming the reason, when it is not.
Every path is resolved physically, through symlinks and directory junctions,
because staging writes through a linked `bin/` or `lib/`. Staging is refused
when:

  - BUILD_DIR is empty, or resolves to a filesystem or volume root;
  - BUILD_DIR, or the directory a `bin/` or `lib/` resolves to, is the
    checkout's `std/` or lies inside it (a source/output cycle, and on
    Windows a destination inside the tree Git Bash's `ln` copies);
  - `bin/` or `lib/` resolves to a directory directly under a root (`/bin`);
  - `bin/` or `lib/` resolves to a directory holding files tracked by
    whichever Git working tree contains it, the caller's or any other;
  - `std/` is a real directory (not a link) that is the checkout's `std/` or
    holds tracked files. A `std/` link is only ever unlinked or kept.
"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

STAGED_DIRS = ("bin", "lib")


class Refusal(Exception):
    pass


def _physical(path: Path) -> Path:
    return Path(os.path.realpath(path))


def is_root(path: Path) -> bool:
    """A filesystem root, drive, UNC share, or an MSYS/Cygwin drive mount."""
    if path.parent == path:
        return True
    # MSYS and Cygwin mount each drive as /c, /d, ... (or /cygdrive/c).
    if sys.platform in ("msys", "cygwin"):
        parts = path.parts
        if len(parts) == 2 and len(parts[1]) == 1:
            return True
        if len(parts) == 3 and parts[1] == "cygdrive" and len(parts[2]) == 1:
            return True
    return False


def _within(path: Path, ancestor: Path) -> bool:
    return path == ancestor or ancestor in path.parents


def tracked_files(directory: Path) -> bool:
    """Whether the Git working tree containing `directory` tracks files in it."""
    try:
        result = subprocess.run(
            ["git", "-C", str(directory), "ls-files", "-z", "--", "."],
            capture_output=True,
            check=False,
        )
    except OSError:
        return False
    # Outside any working tree git fails, and an untracked directory lists
    # nothing; both mean there is nothing of a checkout's here.
    return result.returncode == 0 and bool(result.stdout)


def _is_link(path: Path) -> bool:
    if path.is_symlink():
        return True
    is_junction = getattr(os.path, "isjunction", None)
    return bool(is_junction and is_junction(path))


def check(build_dir: str, source_root: Path) -> None:
    if not build_dir.strip():
        raise Refusal("is empty")
    source_std = _physical(source_root / "std")
    destination = Path(build_dir)
    physical = _physical(destination)
    if is_root(physical):
        raise Refusal(f"is the filesystem root {physical}")
    if _within(physical, source_std):
        raise Refusal(f"lies inside the standard library sources {source_std}")

    for name in STAGED_DIRS:
        entry = destination / name
        if not entry.exists():
            continue
        target = _physical(entry)
        if is_root(target.parent):
            raise Refusal(f"would stage into the system directory {target}")
        if _within(target, source_std):
            raise Refusal(f"would stage into the standard library sources {target}")
        if target.is_dir() and tracked_files(target):
            raise Refusal(f"would stage over tracked sources in {target}")

    std = destination / "std"
    if std.exists() and not _is_link(std):
        target = _physical(std)
        if target == source_std or tracked_files(target):
            raise Refusal(f"would stage over tracked sources in {target}")


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("usage: check-stage-dest.py BUILD_DIR", file=sys.stderr)
        return 2
    try:
        check(argv[1], Path.cwd())
    except Refusal as refusal:
        print(f"error: BUILD_DIR='{argv[1]}' {refusal}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
