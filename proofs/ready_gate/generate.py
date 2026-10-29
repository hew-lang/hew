#!/usr/bin/env python3
"""Check proofs and refuse a stale generated scheduling oracle."""

import argparse
from pathlib import Path
import shutil
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--write", action="store_true")
args = parser.parse_args()
proof_dir = Path(__file__).resolve().parent
for tool in ("lean", "rustfmt"):
    if shutil.which(tool) is None:
        raise SystemExit(
            f"Missing {tool} on PATH; see proofs/ready_gate/README.md#running"
        )
if (
    subprocess.check_output(
        ["lean", "--short-version"], text=True, cwd=proof_dir
    ).strip()
    != "4.19.0"
):
    raise SystemExit("Lean 4.19.0 is required")
result = subprocess.run(
    [
        "lean",
        "-DwarningAsError=true",
        "--run",
        str(Path(__file__).with_name("ReadyGate.lean")),
    ],
    text=True,
    capture_output=True,
    cwd=proof_dir,
)
if result.returncode:
    print(result.stdout)
    print(result.stderr)
    raise SystemExit(result.returncode)
formatted = subprocess.run(
    ["rustfmt", "--edition", "2021", "--emit", "stdout"],
    input=result.stdout,
    text=True,
    capture_output=True,
    check=True,
).stdout
target = Path(__file__).with_name("expected.rs")
if args.write:
    target.write_text(formatted)
elif not target.exists() or target.read_text() != formatted:
    raise SystemExit(
        "Scheduling rules changed: review the Lean contract, then run generate.py --write"
    )
print("Scheduling proofs checked; verification oracle matches Lean")
