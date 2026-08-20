#!/usr/bin/env python3
"""Reject legacy Hew path separators and glob imports in user-facing source."""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
HEW_FENCE = re.compile(r"^\s*```hew\s*$", re.IGNORECASE)
FENCE_END = re.compile(r"^\s*```\s*$")
BEFORE_LABEL = re.compile(r"^Before:\s*$", re.IGNORECASE)
LEGACY_PATTERNS = (
    re.compile(r"\bimport\s+[a-z_]+::"),
    re.compile(r"::<"),
    re.compile(r"\bimport\s+.*::\*\s*;"),
)


def legacy_matches(line: str) -> bool:
    return any(pattern.search(line) for pattern in LEGACY_PATTERNS)


def check_hew_files(errors: list[str]) -> None:
    for source_root in (REPO_ROOT / "examples", REPO_ROOT / "std"):
        for path in source_root.rglob("*.hew"):
            for line_number, line in enumerate(path.read_text().splitlines(), start=1):
                if legacy_matches(line):
                    errors.append(
                        f"{path.relative_to(REPO_ROOT)}:{line_number}: legacy path syntax"
                    )


def check_markdown_file(path: Path, errors: list[str]) -> None:
    in_hew_fence = False
    before_fence = False
    previous_nonblank = ""
    for line_number, line in enumerate(path.read_text().splitlines(), start=1):
        if not in_hew_fence:
            if HEW_FENCE.match(line):
                in_hew_fence = True
                before_fence = path.is_relative_to(
                    REPO_ROOT / "docs/migrations"
                ) and bool(BEFORE_LABEL.match(previous_nonblank))
                continue
            if line.strip():
                previous_nonblank = line.strip()
            continue
        if FENCE_END.match(line):
            in_hew_fence = False
            before_fence = False
            continue
        if legacy_matches(line) and not before_fence:
            errors.append(
                f"{path.relative_to(REPO_ROOT)}:{line_number}: legacy path syntax"
            )


def main() -> int:
    errors: list[str] = []
    check_hew_files(errors)
    for path in [REPO_ROOT / "README.md", *(REPO_ROOT / "docs").rglob("*.md")]:
        check_markdown_file(path, errors)
    if errors:
        print("legacy Hew path syntax is not allowed:", file=sys.stderr)
        print("\n".join(errors), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
