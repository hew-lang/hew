#!/usr/bin/env python3
"""Syntax-check and ShellCheck every tracked shell script in the repository."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def tracked_shell_scripts(root: Path) -> tuple[str, ...]:
    result = subprocess.run(
        ["git", "ls-files", "-z", "--", "*.sh"],
        cwd=root,
        check=True,
        capture_output=True,
    )
    return tuple(
        sorted(
            path.decode("utf-8")
            for path in result.stdout.split(b"\0")
            if path
            and path.endswith(b".sh")
            and (root / path.decode("utf-8")).is_file()
        )
    )


def check(root: Path) -> int:
    scripts = tracked_shell_scripts(root)
    if not scripts:
        print("shell script lint: no tracked *.sh files", file=sys.stderr)
        return 1
    for script in scripts:
        with (root / script).open(encoding="utf-8") as source:
            shebang = source.readline().strip()
        shell = {
            "#!/bin/sh": "sh",
            "#!/usr/bin/env sh": "sh",
            "#!/bin/bash": "bash",
            "#!/usr/bin/env bash": "bash",
        }.get(shebang)
        if shell is None:
            print(f"{script}: expected a sh or bash shebang", file=sys.stderr)
            return 1
        result = subprocess.run([shell, "-n", script], cwd=root, check=False)
        if result.returncode:
            return result.returncode
    result = subprocess.run(["shellcheck", *scripts], cwd=root, check=False)
    if result.returncode:
        return result.returncode
    print(f"shell script lint: PASS ({len(scripts)} tracked scripts)")
    return 0


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    if len(args) > 1:
        print("usage: scripts/shell-script-lint.py [repository-root]", file=sys.stderr)
        return 2
    return check(Path(args[0]).resolve() if args else ROOT)


if __name__ == "__main__":
    raise SystemExit(main())
