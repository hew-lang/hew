#!/usr/bin/env python3
"""Counterfactuals for the no-format-symbol authority rule.

Identity-authority batch (plans/identity-authority-final.md section 4.2).
Replaces the retired no-format-map-key rule. The red fixture matches the
brief's own example: `Symbol::intern(&format!(...))` in an ordinary checker
function; the green fixture is the same line inside `machine_event_spelling`,
the one named exemption.
"""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RULE = ROOT / "rules/rust/authority/no-format-symbol.yml"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"


def findings(source: str) -> list[dict[str, object]]:
    with tempfile.TemporaryDirectory(prefix="hew-format-symbol-") as temp:
        root = Path(temp)
        target = root / "hew-types/src/check/scope.rs"
        target.parent.mkdir(parents=True)
        target.write_text(source, encoding="utf-8")
        result = subprocess.run(
            [
                str(AST_GREP),
                "scan",
                "--rule",
                str(RULE),
                "--json=stream",
                "hew-types/src",
            ],
            cwd=root,
            check=False,
            capture_output=True,
            text=True,
        )
        if result.returncode not in (0, 1):
            raise SystemExit(f"no-format-symbol rule failed closed:\n{result.stderr}")
        return [json.loads(line) for line in result.stdout.splitlines() if line]


def main() -> None:
    global AST_GREP
    if len(sys.argv) > 1:
        AST_GREP = Path(sys.argv[1]).resolve()

    red = findings(
        "fn mint_owner_key(a: &str, b: &str) -> Symbol {\n"
        '    Symbol::intern(&format!("{a}::{b}"))\n'
        "}\n"
    )
    if len(red) != 1:
        raise SystemExit(
            f"no-format-symbol counterfactual found {len(red)}, want 1: {red}"
        )

    green = findings(
        "fn machine_event_spelling(machine: Symbol) -> Symbol {\n"
        '    Symbol::intern(&format!("{machine}Event"))\n'
        "}\n"
    )
    if green:
        raise SystemExit(f"named exemption counterfactual was flagged: {green}")

    print("no-format-symbol counterfactuals: PASS")


if __name__ == "__main__":
    main()
