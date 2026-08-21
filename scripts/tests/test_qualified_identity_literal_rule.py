#!/usr/bin/env python3
"""Counterfactuals for qualified identity literals in Rust comparisons."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RULE = ROOT / "rules/rust/authority/no-qualified-identity-literal-comparison.yml"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"


def findings(source: str) -> list[dict[str, object]]:
    with tempfile.TemporaryDirectory(prefix="hew-qualified-identity-") as temp:
        root = Path(temp)
        target = root / "hew-types/src/lib.rs"
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
                f"qualified identity literal rule failed closed:\n{result.stderr}"
            )
        return [json.loads(line) for line in result.stdout.splitlines() if line]


def main() -> None:
    global AST_GREP
    if len(sys.argv) > 1:
        AST_GREP = Path(sys.argv[1]).resolve()

    red = findings(
        'fn is_rc_new(name: &str) -> bool { name == "Rc::new" }\n'
        'fn is_monitor_id(name: &str) -> bool { "MonitorRef::id" != name }\n'
    )
    if len(red) != 2:
        raise SystemExit(
            f"qualified identity literal counterfactual found {len(red)}, want 2: {red}"
        )

    green = findings(
        "fn is_rc_new(family: RuntimeCallFamily) -> bool { "
        "family == RuntimeCallFamily::RcNew }\n"
    )
    if green:
        raise SystemExit(f"typed identity counterfactual was flagged: {green}")

    print("qualified identity literal comparison counterfactuals: PASS")


if __name__ == "__main__":
    main()
