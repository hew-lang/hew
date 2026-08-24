#!/usr/bin/env python3
"""RED/GREEN counterfactuals for canonical string keyspace insertion."""

from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CHECKER = ROOT / "scripts/canonical-keyspace-lint.py"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"

SOURCE = """
use std::collections::{HashMap, HashSet};

struct Tables {
    item_defs: HashMap<String, u32>,
    machine_layout_names: HashSet<String>,
    numeric_defs: HashMap<u32, u32>,
}

fn direct_bare_key(tables: &mut Tables, name: String) {
    tables.item_defs.insert(name, 1);
}

fn accepted_constructors(tables: &mut Tables, module_full_path: &str, name: &str) {
    tables.item_defs.insert(machine_layout_key(name), 1);
    tables.item_defs.insert(mangle_instantiation(name), 2);
    tables.item_defs.insert(format!("{module_full_path}.{name}"), 3);
    tables.numeric_defs.insert(7, 4);
}
"""


def run(source: str, inventory_rows: str = "") -> subprocess.CompletedProcess[str]:
    with tempfile.TemporaryDirectory(prefix="hew-canonical-keyspace-") as temp:
        root = Path(temp)
        target = root / "hew-types/src/lib.rs"
        target.parent.mkdir(parents=True)
        target.write_text(source, encoding="utf-8")
        inventory = root / "inventory.tsv"
        inventory.write_text(
            "group\tform\tpath\tcount\tretirement_stage\treason\n" + inventory_rows,
            encoding="utf-8",
        )
        return subprocess.run(
            [
                sys.executable,
                str(CHECKER),
                "--root",
                str(root),
                "--inventory",
                str(inventory),
                "--ast-grep",
                str(AST_GREP),
            ],
            check=False,
            capture_output=True,
            text=True,
        )


def main() -> None:
    global AST_GREP
    if len(sys.argv) > 1:
        AST_GREP = Path(sys.argv[1]).resolve()

    red = run(SOURCE)
    if red.returncode != 1 or "bare identifier `name`" not in red.stderr:
        raise SystemExit(
            f"RED bare-key counterfactual did not fail with a fix hint:\n{red.stderr}"
        )

    allow = (
        "# canonical-keyspace-allow\thew-types/src/lib.rs\titem_defs\t1\t"
        "hew-types\ta316-keyspace-item-defs\treviewed pre-existing key\n"
    )
    green = run(SOURCE, allow)
    if green.returncode != 0:
        raise SystemExit(
            f"allowlisted pre-existing counterfactual failed:\n{green.stderr}"
        )

    stale = run(SOURCE, allow.replace("\t1\t", "\t2\t", 1))
    if stale.returncode != 1 or "allowlisted 2, found 1" not in stale.stderr:
        raise SystemExit(
            f"stale allowlist counterfactual did not fail closed:\n{stale.stderr}"
        )

    fixed_family = run(
        SOURCE.replace(
            "tables.item_defs.insert(name, 1);",
            "tables.machine_layout_names.insert(name);",
        ),
        "# canonical-keyspace-allow\thew-types/src/lib.rs\tmachine_layout_names\t1\t"
        "hew-types\ta316-keyspace-layouts\tinvalid canonical-only exception\n",
    )
    if (
        fixed_family.returncode == 0
        or "canonical-only keyspace cannot be allowlisted" not in fixed_family.stderr
    ):
        raise SystemExit(
            "canonical-only family allowlist unexpectedly passed:\n"
            f"{fixed_family.stderr}"
        )

    fixed = run(
        SOURCE.replace(
            "tables.item_defs.insert(name, 1);",
            "tables.item_defs.insert(machine_layout_key(&name), 1);",
        )
    )
    if fixed.returncode != 0:
        raise SystemExit(
            f"canonical constructor counterfactual failed:\n{fixed.stderr}"
        )

    print("canonical keyspace lint counterfactuals: PASS")


if __name__ == "__main__":
    main()
