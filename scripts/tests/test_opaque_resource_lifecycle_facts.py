#!/usr/bin/env python3
"""Executable contracts for source-derived opaque lifecycle compiler cases."""

from __future__ import annotations

import json
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "scripts/structural-authority-audit.py"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"

if not AST_GREP.is_file():
    raise SystemExit("bootstrap pinned ast-grep before lifecycle fact test")

with tempfile.TemporaryDirectory() as temp:
    facts = Path(temp) / "facts.json"
    result = subprocess.run(
        [
            "python3",
            str(AUDIT),
            "--ast-grep",
            str(AST_GREP),
            "--opaque-resource-facts",
            str(facts),
        ],
        cwd=ROOT,
        text=True,
        capture_output=True,
    )
    assert result.returncode == 0, result.stderr
    payload = json.loads(facts.read_text())
    candidates = payload["candidates"]
    cases = payload["compiler_e2e_cases"]
    assert candidates and len(candidates) == len(cases)
    assert {row["carrier_key"] for row in candidates} == {
        row["carrier_key"] for row in cases
    }
    for row in cases:
        assert "import std::" in row["scope_exit_source"]
        assert "fn scope_exit_case(value:" in row["scope_exit_source"]
        assert "value.close();" in row["explicit_close_source"]
