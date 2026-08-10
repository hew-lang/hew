#!/usr/bin/env python3
"""Pin the CI and Make compatibility surfaces to the xtask entry point."""

from __future__ import annotations

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_ROOTS = (ROOT / ".github" / "workflows", ROOT / ".github" / "actions")
RAW_CARGO = re.compile(r"\bcargo\s+(?!xtask\b)")
RAW_MAKE = re.compile(r"\b(?:g?make)\s+")
BUILT_HEW = re.compile(
    r"^(?:\./)?(?:target|build)/\S*/hew(?:-lsp|-observe)?(?:\.exe)?\s"
)


def shell_lines(path: Path) -> list[str]:
    lines: list[str] = []
    in_run_block = False
    run_indent = 0
    for raw in path.read_text().splitlines():
        indent = len(raw) - len(raw.lstrip())
        stripped = raw.strip()
        if in_run_block and stripped and indent <= run_indent:
            in_run_block = False
        if re.match(r"^(?:-\s+)?run:\s*\|[-+]?\s*$", stripped):
            in_run_block = True
            run_indent = indent
            continue
        inline = re.match(r"^(?:-\s+)?run:\s+(.+)$", stripped)
        command = inline.group(1) if inline else (stripped if in_run_block else "")
        if command and not command.startswith("#"):
            lines.append(command.split(" #", 1)[0])
    return lines


def test_every_ci_shell_entry_uses_xtask() -> None:
    violations: list[str] = []
    for root in WORKFLOW_ROOTS:
        for path in sorted((*root.glob("*.yml"), *root.glob("*.yaml"))):
            for line in shell_lines(path):
                if (
                    RAW_CARGO.search(line)
                    or RAW_MAKE.search(line)
                    or BUILT_HEW.search(line)
                ):
                    violations.append(f"{path.relative_to(ROOT)}: {line}")
    assert not violations, "CI bypasses cargo xtask:\n" + "\n".join(violations)


def test_makefile_is_only_a_small_xtask_facade() -> None:
    makefile = (ROOT / "Makefile").read_text().splitlines()
    assert len(makefile) < 400
    recipes = [line.strip() for line in makefile if line.startswith("\t")]
    assert recipes
    assert all("cargo xtask" in recipe for recipe in recipes)


if __name__ == "__main__":
    test_every_ci_shell_entry_uses_xtask()
    test_makefile_is_only_a_small_xtask_facade()
    print("xtask cutover contract: ok")
