#!/usr/bin/env python3
"""Reject legacy Hew path separators and glob imports in user-facing prose."""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
FENCE = re.compile(r"^\s*(```+|~~~+)")
BEFORE_LABEL = re.compile(r"^Before:\s*$", re.IGNORECASE)
LEGACY_PATTERNS = (
    re.compile(r"\bimport\s+[a-z_]+::"),
    re.compile(r"::<"),
    re.compile(r"\bimport\s+.*::\*\s*;"),
    re.compile(r"\bimport\s+[A-Za-z_][A-Za-z0-9_.]*\.\*\s*;"),
)

# `hew test --list` prints `<file>::<test-name>` identities — this is the
# current, supported CLI output format (docs/hew-language-guide.md), not
# legacy `::`-path syntax.  The `::<` substring inside it would otherwise
# trip LEGACY_PATTERNS, so strip exact occurrences of this documented
# format (by pattern, not by file) before scanning prose lines.
TEST_IDENTITY_ALLOWANCE = re.compile(r"<file>::<test-name>")


def prose_legacy_matches(line: str) -> bool:
    prose = TEST_IDENTITY_ALLOWANCE.sub("", line)
    return any(pattern.search(prose) for pattern in LEGACY_PATTERNS)


def check_markdown_file(path: Path, errors: list[str]) -> None:
    fence_delimiter = ""
    previous_nonblank = ""
    is_migration_doc = path.is_relative_to(REPO_ROOT / "docs/migrations")
    for line_number, line in enumerate(path.read_text().splitlines(), start=1):
        if match := FENCE.match(line):
            marker = match.group(1)[0]
            if not fence_delimiter:
                fence_delimiter = marker
            elif fence_delimiter == marker:
                fence_delimiter = ""
            continue
        if fence_delimiter:
            continue
        before_prose = is_migration_doc and bool(BEFORE_LABEL.match(line))
        if (
            line.strip()
            and not before_prose
            and prose_legacy_matches(line)
            and not (is_migration_doc and BEFORE_LABEL.match(previous_nonblank))
        ):
            errors.append(
                f"{path.relative_to(REPO_ROOT)}:{line_number}: legacy path syntax"
            )
        if line.strip():
            previous_nonblank = line.strip()


def main() -> int:
    errors: list[str] = []
    markdown_roots = [
        REPO_ROOT / "README.md",
        REPO_ROOT / "CHANGELOG.md",
        *(REPO_ROOT / "docs").rglob("*.md"),
        *(REPO_ROOT / "examples").rglob("*.md"),
    ]
    for path in markdown_roots:
        check_markdown_file(path, errors)
    if errors:
        print("legacy Hew path syntax is not allowed:", file=sys.stderr)
        print("\n".join(errors), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
