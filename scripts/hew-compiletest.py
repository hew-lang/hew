#!/usr/bin/env python3
"""Trybuild-style adjacent-stderr checks for intentional Hew rejections."""

from __future__ import annotations

import argparse
from pathlib import Path
import subprocess
import sys
from typing import NoReturn


REPO_ROOT = Path(__file__).resolve().parent.parent
DIRECTIVE = "// compiletest: check-fail"


def die(message: str) -> NoReturn:
    print(f"hew-compiletest: {message}", file=sys.stderr)
    raise SystemExit(1)


def normalize(stderr: str) -> str:
    normalized = stderr.replace("\r\n", "\n").replace("\r", "\n")
    normalized = normalized.replace(str(REPO_ROOT), "$DIR")
    return normalized if normalized.endswith("\n") else normalized + "\n"


def selected_fixtures(directory: Path) -> list[Path]:
    fixtures = [
        path
        for path in sorted(directory.glob("*.hew"))
        if DIRECTIVE in path.read_text(encoding="utf-8").splitlines()
    ]
    if not fixtures:
        die(f"no fixtures containing {DIRECTIVE!r} under {directory}")
    return fixtures


def run(compiler: Path, directory: Path, bless: bool) -> None:
    fixtures = selected_fixtures(directory)
    selected_stderr = {fixture.with_suffix(".stderr") for fixture in fixtures}
    orphaned = sorted(
        path for path in directory.glob("*.stderr") if path not in selected_stderr
    )
    if orphaned:
        die("orphan stderr snapshots: " + ", ".join(str(path) for path in orphaned))

    failed = False
    for fixture in fixtures:
        result = subprocess.run(
            # Compile-fail snapshots pin errors. Warnings were never part of
            # these fixtures' former substring contracts, and their emission
            # order is intentionally not stable, so suppress them before the
            # exact comparison rather than sorting compiler diagnostics.
            [str(compiler), "check", "--allow", "all", str(fixture)],
            cwd=REPO_ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if result.returncode != 1:
            print(
                f"FAIL {fixture.name}: expected check exit 1, got {result.returncode}",
                file=sys.stderr,
            )
            failed = True
            continue
        actual = normalize(result.stderr)
        expected_path = fixture.with_suffix(".stderr")
        if bless:
            expected_path.write_text(actual, encoding="utf-8")
            print(f"BLESSED {expected_path.relative_to(REPO_ROOT)}")
            continue
        try:
            expected = expected_path.read_text(encoding="utf-8")
        except FileNotFoundError:
            print(f"FAIL {fixture.name}: missing {expected_path.name}", file=sys.stderr)
            failed = True
            continue
        if actual != expected:
            print(
                f"FAIL {fixture.name}: stderr differs from {expected_path.name}\n"
                "run with --bless and review the adjacent diagnostic",
                file=sys.stderr,
            )
            failed = True
            continue
        print(f"PASS {fixture.name}")
    if failed:
        raise SystemExit(1)
    print(f"hew-compiletest: {len(fixtures)} compile-fail fixtures passed")


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--hew-bin", type=Path, required=True)
    parser.add_argument("--directory", type=Path, required=True)
    parser.add_argument("--bless", action="store_true")
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    run(args.hew_bin.resolve(), args.directory.resolve(), args.bless)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
