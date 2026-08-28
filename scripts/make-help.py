#!/usr/bin/env python3
"""Render the maintained Make target index from inline target annotations."""

from __future__ import annotations

import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SECTION_ORDER = ("Build", "Check", "Test", "Release", "Develop")
ANNOTATION_RE = re.compile(
    r"^(?P<target>[A-Za-z0-9_.%/-]+)\s*:[^#]*"
    r"##\s*(?P<section>[^:]+):\s*(?P<purpose>.*\S)\s*$"
)


def entries(makefile: str) -> list[tuple[str, str, str]]:
    """Return validated ``(section, target, purpose)`` help entries."""
    found: list[tuple[str, str, str]] = []
    seen: set[str] = set()
    for number, line in enumerate(makefile.splitlines(), start=1):
        match = ANNOTATION_RE.match(line)
        if match is None:
            continue
        section = match.group("section").strip()
        target = match.group("target")
        purpose = match.group("purpose").strip()
        if section not in SECTION_ORDER:
            raise ValueError(
                f"Makefile:{number}: unknown help section {section!r}; "
                f"expected one of {', '.join(SECTION_ORDER)}"
            )
        if target in seen:
            raise ValueError(f"Makefile:{number}: duplicate help target {target!r}")
        seen.add(target)
        found.append((section, target, purpose))

    if not found:
        raise ValueError("Makefile help index is empty")
    return found


def render(makefile: str) -> str:
    found = entries(makefile)
    width = max(len(target) for _, target, _ in found)
    lines = [f"Hew developer targets ({len(found)})"]
    for section in SECTION_ORDER:
        section_entries = [entry for entry in found if entry[0] == section]
        if not section_entries:
            continue
        lines.extend(("", f"{section}:"))
        lines.extend(
            f"  make {target:<{width}}  {purpose}"
            for _, target, purpose in section_entries
        )
    return "\n".join(lines) + "\n"


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    if len(args) > 1:
        print("usage: scripts/make-help.py [Makefile]", file=sys.stderr)
        return 2
    path = Path(args[0]) if args else ROOT / "Makefile"
    try:
        output = render(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as error:
        print(f"make help: {error}", file=sys.stderr)
        return 1
    print(output, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
