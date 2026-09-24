#!/usr/bin/env python3
"""Counterfactuals for the no-builtin-name-literal-decision authority rule.

Identity-authority batch (plans/identity-authority-final.md section 4.2).
"""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RULE = ROOT / "rules/rust/authority/no-builtin-name-literal-decision.yml"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"


def findings(source: str) -> list[dict[str, object]]:
    with tempfile.TemporaryDirectory(prefix="hew-builtin-name-literal-") as temp:
        root = Path(temp)
        target = root / "hew-types/src/check/registration.rs"
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
                f"no-builtin-name-literal-decision rule failed closed:\n{result.stderr}"
            )
        return [json.loads(line) for line in result.stdout.splitlines() if line]


def main() -> None:
    global AST_GREP
    if len(sys.argv) > 1:
        AST_GREP = Path(sys.argv[1]).resolve()

    red = findings(
        'fn is_sleep(name: &str) -> bool { name == "sleep" }\n'
        'fn dispatch(name: &str) { if matches!(name, "sleep" | "sleep_until") {} }\n'
    )
    if len(red) != 2:
        raise SystemExit(
            f"no-builtin-name-literal-decision counterfactual found {len(red)}, "
            f"want 2: {red}"
        )

    green = findings(
        "fn is_sleep(family: RuntimeCallFamily) -> bool { family == RuntimeCallFamily::Sleep }\n"
    )
    if green:
        raise SystemExit(f"typed identity counterfactual was flagged: {green}")

    print("no-builtin-name-literal-decision counterfactuals: PASS")


if __name__ == "__main__":
    main()
