#!/usr/bin/env python3
"""Counterfactuals for the no-leaf-name-join authority rule.

Identity-authority batch (plans/identity-authority-final.md section 4.2).
"""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RULE = ROOT / "rules/rust/authority/no-leaf-name-join.yml"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"


def findings(source: str) -> list[dict[str, object]]:
    with tempfile.TemporaryDirectory(prefix="hew-leaf-name-join-") as temp:
        root = Path(temp)
        target = root / "hew-types/src/check/methods.rs"
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
            raise SystemExit(f"no-leaf-name-join rule failed closed:\n{result.stderr}")
        return [json.loads(line) for line in result.stdout.splitlines() if line]


def main() -> None:
    global AST_GREP
    if len(sys.argv) > 1:
        AST_GREP = Path(sys.argv[1]).resolve()

    red = findings(
        "fn leaf(owner_path: &str) -> &str {\n"
        "    owner_path.rsplit_once('.').map(|(_, leaf)| leaf).unwrap_or(owner_path)\n"
        "}\n"
        "fn is_qualified(name: &str) -> bool { name.contains('.') }\n"
    )
    if len(red) != 2:
        raise SystemExit(
            f"no-leaf-name-join counterfactual found {len(red)}, want 2: {red}"
        )

    green = findings(
        "fn leaf(id: DefId, defs: &DefTable) -> Symbol { defs.name(id) }\n"
    )
    if green:
        raise SystemExit(f"resolver-consuming counterfactual was flagged: {green}")

    print("no-leaf-name-join counterfactuals: PASS")


if __name__ == "__main__":
    main()
