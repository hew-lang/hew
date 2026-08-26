#!/usr/bin/env python3
"""Reject unreviewed growth in hew-runtime's cargo-geiger unsafe count."""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
BASELINE = ROOT / ".github" / "hew-runtime-unsafe-count.txt"
COUNTER_KINDS = ("functions", "exprs", "item_impls", "item_traits", "methods")


def baseline_total(path: Path) -> int:
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        raise SystemExit(f"runtime unsafe count: cannot read {path}: {error}")
    values = [
        line.removeprefix("total=") for line in lines if line.startswith("total=")
    ]
    if len(values) != 1 or not values[0].isdigit():
        raise SystemExit(
            f"runtime unsafe count: {path} must contain one `total=<integer>` line"
        )
    return int(values[0])


def geiger_total(report: object) -> int:
    if not isinstance(report, dict) or not isinstance(report.get("packages"), list):
        raise SystemExit(
            "runtime unsafe count: cargo-geiger returned an unrecognised JSON report"
        )
    entries = [
        entry
        for entry in report["packages"]
        if isinstance(entry, dict)
        and entry.get("package", {}).get("id", {}).get("name") == "hew-runtime"
    ]
    if len(entries) != 1:
        raise SystemExit(
            "runtime unsafe count: cargo-geiger report must contain exactly one hew-runtime entry"
        )
    unsafety = entries[0].get("unsafety")
    if not isinstance(unsafety, dict):
        raise SystemExit(
            "runtime unsafe count: hew-runtime entry has no unsafe counters"
        )
    total = 0
    for scope in ("used", "unused"):
        counters = unsafety.get(scope)
        if not isinstance(counters, dict):
            raise SystemExit(f"runtime unsafe count: missing `{scope}` counters")
        for kind in COUNTER_KINDS:
            value = counters.get(kind, {}).get("unsafe_")
            if not isinstance(value, int) or value < 0:
                raise SystemExit(
                    f"runtime unsafe count: missing `{scope}.{kind}.unsafe_`"
                )
            total += value
    return total


def main() -> None:
    command = [
        "cargo",
        "geiger",
        "--manifest-path",
        str(ROOT / "hew-runtime" / "Cargo.toml"),
        "--output-format",
        "Json",
    ]
    result = subprocess.run(
        command, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False
    )
    if result.returncode:
        detail = result.stderr.decode("utf-8", errors="replace").strip()
        raise SystemExit(
            "runtime unsafe count: cargo geiger failed "
            "(install it with `cargo install cargo-geiger --locked`)\n"
            f"{detail}"
        )
    try:
        actual = geiger_total(json.loads(result.stdout))
    except json.JSONDecodeError as error:
        raise SystemExit(
            f"runtime unsafe count: cargo-geiger did not emit JSON: {error}"
        )
    expected = baseline_total(BASELINE)
    if actual > expected:
        raise SystemExit(
            f"runtime unsafe count: {actual} exceeds reviewed total {expected}; "
            f"update {BASELINE.relative_to(ROOT)} only with an explicit unsafe-code review waiver"
        )
    print(f"runtime unsafe count: {actual} <= reviewed total {expected}")


if __name__ == "__main__":
    main()
