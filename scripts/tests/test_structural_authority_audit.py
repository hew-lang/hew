#!/usr/bin/env python3
"""Counterfactual contracts for the source-path authority ratchet."""

from __future__ import annotations
import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "scripts/structural-authority-audit.py"


def run(root: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["python3", str(AUDIT), "--root", str(root)], text=True, capture_output=True
    )


with tempfile.TemporaryDirectory() as temp:
    work = Path(temp)
    (work / "scripts").mkdir()
    (work / "scripts/structural-authority-inventory.tsv").write_text(
        "group\tpath\tcount\tdisposition\n"
    )
    (work / "scripts/corpus-floors.tsv").write_text(
        "structural-authority-inventory\texact\t0\t-\ttemporary empty inventory\n"
    )
    source = work / "hew-mir/src/lower"
    source.mkdir(parents=True)
    target = source / "new_authority.rs"
    target.write_text("// short_name(name) == old_name is a comment, not a finding\n")
    assert run(work).returncode == 0, "comments must not create authority findings"
    target.write_text("if short_name(name) == old_name { }\n")
    assert run(work).returncode != 0, "a new short-name fallback must fail"
    target.unlink()
    target.write_text('let method_key = format!("{}::{}", owner, method);\n')
    assert run(work).returncode != 0, "a new method identity construction must fail"
    (work / "scripts/structural-authority-inventory.tsv").write_text(
        "group\tpath\tcount\tdisposition\nshort-name-fallback\thew-mir/src/lower/new_authority.rs\t1\tfixture\n"
    )
    target.write_text("if short_name(name) == old_name { }\n")
    assert run(work).returncode != 0, "a stale inventory floor/corpus shrink must fail"
print("structural authority audit counterfactuals: PASS")
