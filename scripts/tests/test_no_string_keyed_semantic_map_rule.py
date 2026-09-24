#!/usr/bin/env python3
"""Counterfactuals for the no-string-keyed-semantic-map authority rule.

Identity-authority batch (plans/identity-authority-final.md section 4.2).
"""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RULE = ROOT / "rules/rust/authority/no-string-keyed-semantic-map.yml"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"


def findings(source: str) -> list[dict[str, object]]:
    with tempfile.TemporaryDirectory(prefix="hew-string-keyed-map-") as temp:
        root = Path(temp)
        target = root / "hew-types/src/check/types.rs"
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
            raise SystemExit(
                f"no-string-keyed-semantic-map rule failed closed:\n{result.stderr}"
            )
        return [json.loads(line) for line in result.stdout.splitlines() if line]


def main() -> None:
    global AST_GREP
    if len(sys.argv) > 1:
        AST_GREP = Path(sys.argv[1]).resolve()

    red = findings(
        "pub struct TypeCheckOutput {\n"
        "    pub type_defs: HashMap<String, TypeDef>,\n"
        "    pub trait_defs: HashMap<&str, TraitInfo>,\n"
        "}\n"
    )
    if len(red) != 2:
        raise SystemExit(
            f"no-string-keyed-semantic-map counterfactual found {len(red)}, want 2: {red}"
        )

    green = findings(
        "pub struct TypeCheckOutput {\n"
        "    pub type_defs: HashMap<NominalId, TypeDef>,\n"
        "    pub trait_defs: HashMap<DefId, TraitInfo>,\n"
        "}\n"
    )
    if green:
        raise SystemExit(f"id-keyed map counterfactual was flagged: {green}")

    print("no-string-keyed-semantic-map counterfactuals: PASS")


if __name__ == "__main__":
    main()
