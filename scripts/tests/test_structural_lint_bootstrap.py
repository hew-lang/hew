#!/usr/bin/env python3
"""Prove parser-backed structural gates depend on the clean bootstrap."""

from __future__ import annotations

from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


build_system = (ROOT / "xtask/src/build_system.rs").read_text(encoding="utf-8")
makefile = (ROOT / "Makefile").read_text(encoding="utf-8")

for gate in ("structural-bootstrap-contract", "structural-lint"):
    declaration = build_system.split(f'"{gate}"', 1)[1].split("),", 1)[0]
    assert '"structural-bootstrap"' in declaration, declaration

target = makefile.split("structural-lint-bootstrap:\n", 1)[1].split("\n\n", 1)[0]
assert "cargo xtask gate structural-bootstrap-contract" in target, target

print("structural lint clean-bootstrap dependency: PASS")
