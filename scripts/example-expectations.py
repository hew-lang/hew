#!/usr/bin/env python3
"""Run a closed corpus of Hew examples against exact output expectations."""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import signal
import subprocess
import sys
from typing import NoReturn


REPO_ROOT = Path(__file__).resolve().parent.parent


def fail(message: str) -> NoReturn:
    print(f"example-expectations: {message}", file=sys.stderr)
    raise SystemExit(1)


def display(path: Path) -> str:
    try:
        return str(path.relative_to(Path.cwd()))
    except ValueError:
        return str(path)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Run every admitted Hew source, require a paired .expected file, "
            "and compare combined stdout/stderr exactly."
        )
    )
    parser.add_argument("--hew-bin", type=Path, required=True)
    parser.add_argument("--label", required=True)
    parser.add_argument("--floor-key")
    parser.add_argument("--source-root", type=Path, action="append", default=[])
    parser.add_argument("--source", type=Path, action="append", default=[])
    parser.add_argument("--timeout-seconds", type=float, default=30.0)
    return parser.parse_args()


def collect_sources(
    source_roots: list[Path], explicit_sources: list[Path]
) -> list[Path]:
    sources: list[Path] = []

    seen_roots: set[Path] = set()
    for root in source_roots:
        resolved_root = root.resolve()
        if resolved_root in seen_roots:
            fail(f"duplicate source root: {display(root)}")
        seen_roots.add(resolved_root)
        if not root.is_dir():
            fail(f"source root is not a directory: {display(root)}")

        root_sources = sorted(root.glob("*.hew"))
        if not root_sources:
            fail(f"source root contains no .hew files: {display(root)}")
        sources.extend(root_sources)

        for expectation in sorted(root.glob("*.expected")):
            paired_source = expectation.with_suffix(".hew")
            if not paired_source.is_file():
                fail(f"orphan expectation has no paired source: {display(expectation)}")

    for source in explicit_sources:
        if source.suffix != ".hew":
            fail(f"explicit source must end in .hew: {display(source)}")
        if not source.is_file():
            fail(f"explicit source does not exist: {display(source)}")
        sources.append(source)

    if not sources:
        fail("inventory is empty; pass --source-root or --source")

    unique: dict[Path, Path] = {}
    for source in sources:
        resolved_source = source.resolve()
        if resolved_source in unique:
            fail(
                "duplicate source admission: "
                f"{display(source)} (already admitted as {display(unique[resolved_source])})"
            )
        unique[resolved_source] = source

    ordered = sorted(unique.values(), key=lambda path: str(path))
    for source in ordered:
        expectation = source.with_suffix(".expected")
        if not expectation.is_file():
            fail(f"source has no paired .expected: {display(source)}")
    return ordered


def run_process(command: list[str], timeout_seconds: float) -> tuple[int | None, bytes]:
    kwargs: dict[str, object] = {
        "stdout": subprocess.PIPE,
        "stderr": subprocess.STDOUT,
    }
    if os.name == "nt":
        kwargs["creationflags"] = subprocess.CREATE_NEW_PROCESS_GROUP
    else:
        kwargs["start_new_session"] = True

    process = subprocess.Popen(command, **kwargs)
    try:
        output, _ = process.communicate(timeout=timeout_seconds)
        return process.returncode, output
    except subprocess.TimeoutExpired:
        if os.name == "nt":
            process.kill()
        else:
            os.killpg(process.pid, signal.SIGKILL)
        output, _ = process.communicate()
        return None, output


def normalized_text(data: bytes, *, source: str) -> str:
    try:
        return data.decode("utf-8").rstrip("\n")
    except UnicodeDecodeError as error:
        fail(f"{source} is not UTF-8: {error}")


def compact(text: str) -> str:
    return "|".join(text.splitlines()[:3])


def assert_floor(key: str, actual: int, label: str) -> None:
    helper = REPO_ROOT / "scripts/lib/corpus-floor.sh"
    result = subprocess.run(
        ["bash", str(helper), key, str(actual), label],
        cwd=REPO_ROOT,
        check=False,
    )
    if result.returncode != 0:
        raise SystemExit(result.returncode)


def main() -> None:
    args = parse_args()
    if args.timeout_seconds <= 0:
        fail("--timeout-seconds must be greater than zero")
    if not args.hew_bin.is_file() or not os.access(args.hew_bin, os.X_OK):
        fail(f"compiler is not executable: {display(args.hew_bin)}")

    sources = collect_sources(args.source_root, args.source)
    if args.floor_key:
        assert_floor(args.floor_key, len(sources), args.label)

    passed = 0
    failed = 0
    for source in sources:
        expectation = source.with_suffix(".expected")
        expected = normalized_text(
            expectation.read_bytes(), source=f"expectation {display(expectation)}"
        )
        status, output_bytes = run_process(
            [
                str(args.hew_bin.resolve()),
                "run",
                "--timeout",
                f"{args.timeout_seconds:g}s",
                str(source),
            ],
            args.timeout_seconds + 2.0,
        )
        actual = normalized_text(output_bytes, source=f"output from {display(source)}")

        reasons: list[str] = []
        if status is None:
            reasons.append(f"timed out after {args.timeout_seconds:g}s")
        elif status != 0:
            reasons.append(f"exited with status {status}")
        if actual != expected:
            reasons.append("combined stdout/stderr differs")

        if reasons:
            print(f"  FAIL: {display(source)} ({'; '.join(reasons)})")
            print(f"    expected: {compact(expected)}")
            print(f"    actual:   {compact(actual)}")
            failed += 1
        else:
            passed += 1

    print(f"  {passed} passed, {failed} failed")
    if failed:
        fail(
            f"{failed} {args.label} example(s) failed; "
            "run `hew run <file>` to reproduce"
        )


if __name__ == "__main__":
    main()
