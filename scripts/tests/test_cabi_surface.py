#!/usr/bin/env python3
"""Self-tests for the census-only C ABI manifest verifier."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any, Callable

ROOT = Path(__file__).resolve().parents[2]
GENERATOR = ROOT / "scripts" / "generate-cabi-surface.py"
MANIFEST = ROOT / "scripts" / "cabi-surface.json"


def run(*arguments: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(GENERATOR), *arguments],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )


def mutation(
    name: str, mutate: Callable[[dict[str, Any]], None], expected: str
) -> None:
    document = json.loads(MANIFEST.read_text(encoding="utf-8"))
    mutate(document)
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / "cabi-surface.json"
        path.write_text(json.dumps(document), encoding="utf-8")
        result = run("--validate", "--manifest", str(path))
    if result.returncode == 0:
        raise AssertionError(f"{name} unexpectedly passed")
    if expected not in result.stderr:
        raise AssertionError(f"{name} did not report {expected!r}:\n{result.stderr}")
    print(f"PASS {name}\n{result.stderr.strip()}")


def test_extra_export() -> None:
    def mutate(document: dict[str, Any]) -> None:
        row = dict(document["functions"][0])
        row["symbol"] = "hew_cabi_extra_export"
        document["functions"].append(row)

    mutation("extra export fails", mutate, "manifest-only: hew_cabi_extra_export")


def test_omitted_static() -> None:
    def mutate(document: dict[str, Any]) -> None:
        document["statics"].pop()

    mutation("omitted static fails", mutate, "source-only:")


def test_corrupted_signature() -> None:
    def mutate(document: dict[str, Any]) -> None:
        document["functions"][0]["signature"]["native"] = "fn corrupted()"

    mutation("corrupted signature fails", mutate, "native functions signature mismatch")


def test_release_on_unmeasured() -> None:
    def mutate(document: dict[str, Any]) -> None:
        document["functions"][0]["release_symbol"] = "hew_string_drop"

    mutation(
        "release symbol on unmeasured row fails",
        mutate,
        "is unmeasured and must not declare release_symbol",
    )


def main() -> int:
    test_extra_export()
    test_omitted_static()
    test_corrupted_signature()
    test_release_on_unmeasured()
    result = run("--check")
    if result.returncode:
        raise AssertionError(result.stderr)
    print("PASS generated manifest and C header compile")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except AssertionError as error:
        print(f"FAIL: {error}", file=sys.stderr)
        raise SystemExit(1) from error
