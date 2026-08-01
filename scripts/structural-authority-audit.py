#!/usr/bin/env python3
"""Fail closed on additions to reviewed compiler identity/ownership seams."""

from __future__ import annotations

import argparse
import csv
import re
import sys
from collections import defaultdict
from pathlib import Path

PATTERNS = {
    "short-name-fallback": r"(?:\w+\s*==\s*\w*short_name\s*\(|short_name\s*\([^\n]+?\)\s*==)",
    "string-method-identity": r"\b(?:method_key|qualified_name|qualified|callee_name|symbol|key)\s*=\s*format!\([^\n]*?\"[^\"\n]*::",
    "legacy-heap-reader": r"\bty_owns_heap\s*\(",
    "checker-hir-publication": r"\b(?:expr_types|resolved_calls|call_targets)\.insert\s*\(",
    "mir-ownership-sink": r"\b(?:ValueOwnership|OwnershipDecision)::classify\s*\(",
}


def code_only(text: str) -> str:
    """Blank comments while keeping line positions and format! string literals."""
    out, i, block = [], 0, 0
    while i < len(text):
        if block:
            if text.startswith("/*", i):
                block += 1
                out.extend("  ")
                i += 2
            elif text.startswith("*/", i):
                block -= 1
                out.extend("  ")
                i += 2
            else:
                out.append("\n" if text[i] == "\n" else " ")
                i += 1
        elif text.startswith("//", i):
            end = text.find("\n", i)
            if end < 0:
                end = len(text)
            out.extend(" " * (end - i))
            i = end
        elif text.startswith("/*", i):
            block = 1
            out.extend("  ")
            i += 2
        else:
            out.append(text[i])
            i += 1
    return "".join(out)


def production_files(root: Path):
    for path in root.glob("hew-*/src/**/*.rs"):
        rel = path.relative_to(root).as_posix()
        if "/tests/" not in rel and not rel.endswith(("_tests.rs", "_test.rs")):
            yield rel, path


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[1]
    )
    parser.add_argument("--inventory", type=Path)
    args = parser.parse_args()
    root = args.root.resolve()
    inventory = args.inventory or root / "scripts/structural-authority-inventory.tsv"
    expected: dict[tuple[str, str], int] = {}
    with inventory.open(newline="") as handle:
        rows = (line for line in handle if line.strip() and not line.startswith("#"))
        for row in csv.DictReader(rows, delimiter="\t"):
            group, path, count = row["group"], row["path"], row["count"]
            if group not in PATTERNS or not path or not count.isdigit():
                raise SystemExit(f"invalid authority inventory row: {row}")
            key = (group, path)
            if key in expected:
                raise SystemExit(f"duplicate authority inventory row: {group} {path}")
            expected[key] = int(count)

    actual = defaultdict(int)
    for rel, path in production_files(root):
        source = code_only(path.read_text())
        for group, pattern in PATTERNS.items():
            if group == "checker-hir-publication" and not rel.startswith(
                ("hew-types/src/check/", "hew-hir/src/")
            ):
                continue
            actual[(group, rel)] = len(re.findall(pattern, source, re.MULTILINE))

    failures = []
    for key in sorted(set(expected) | set(actual)):
        want, got = expected.get(key, 0), actual.get(key, 0)
        if want != got:
            failures.append(f"{key[0]} {key[1]}: expected {want}, found {got}")
    if failures:
        print(
            "structural authority inventory changed; review and update its explicit allowlist:",
            file=sys.stderr,
        )
        print("\n".join(f"  - {item}" for item in failures), file=sys.stderr)
        return 1
    floor_rows = (root / "scripts/corpus-floors.tsv").read_text().splitlines()
    floor = next(
        (
            line.split("\t")[2]
            for line in floor_rows
            if line.startswith("structural-authority-inventory\t")
        ),
        None,
    )
    if floor is None or not floor.isdigit() or int(floor) != len(expected):
        print(
            "structural-authority-inventory corpus floor is stale or missing",
            file=sys.stderr,
        )
        return 1
    print(
        f"structural authority inventory: {len(expected)} reviewed source-path entries"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
