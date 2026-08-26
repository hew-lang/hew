#!/usr/bin/env python3
"""Report hew-runtime's cargo-geiger unsafe counters as telemetry.

This once compared the summed counters against a committed `total=` ceiling on
the required lint job. The count is a snapshot magnitude: it moved on every
legal refactor that added a documented unsafe block, and the only available
response was to raise the committed number. It recorded churn, not a decision
anybody made.

What that number stood for is enforced by rules that remain required on every
pull request, and that reject the unsafe code which is actually wrong:

  * `runtime-unsafe-clippy` rejects an undocumented unsafe block
    (`-D clippy::undocumented_unsafe_blocks`), and
  * `unsafe-pattern-audit` rejects a newly added `transmute`, `from_raw_parts`
    or `as *mut c_void` whose nearby SAFETY rationale does not name provenance,
    bounds, and the type tag.

So this prints the trend and writes it to the run summary. It still FAILS on a
real defect -- cargo-geiger absent, erroring, or emitting a report this cannot
read -- because a telemetry step that silently reports nothing is
indistinguishable from one that has been broken for months.
"""

from __future__ import annotations

import json
import os
import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
COUNTER_KINDS = ("functions", "exprs", "item_impls", "item_traits", "methods")


def geiger_counts(report: object) -> dict[str, int]:
    """Sum the unsafe counters per scope, failing closed on an unreadable report."""
    if not isinstance(report, dict) or not isinstance(report.get("packages"), list):
        raise SystemExit(
            "runtime unsafe telemetry: cargo-geiger returned an unrecognised JSON report"
        )
    entries = [
        entry
        for entry in report["packages"]
        if isinstance(entry, dict)
        and entry.get("package", {}).get("id", {}).get("name") == "hew-runtime"
    ]
    if len(entries) != 1:
        raise SystemExit(
            "runtime unsafe telemetry: cargo-geiger report must contain "
            "exactly one hew-runtime entry"
        )
    unsafety = entries[0].get("unsafety")
    if not isinstance(unsafety, dict):
        raise SystemExit(
            "runtime unsafe telemetry: hew-runtime entry has no unsafe counters"
        )
    counts: dict[str, int] = {}
    for scope in ("used", "unused"):
        counters = unsafety.get(scope)
        if not isinstance(counters, dict):
            raise SystemExit(f"runtime unsafe telemetry: missing `{scope}` counters")
        total = 0
        for kind in COUNTER_KINDS:
            value = counters.get(kind, {}).get("unsafe_")
            if not isinstance(value, int) or value < 0:
                raise SystemExit(
                    f"runtime unsafe telemetry: missing `{scope}.{kind}.unsafe_`"
                )
            total += value
        counts[scope] = total
    return counts


def render(counts: dict[str, int]) -> str:
    used, unused = counts["used"], counts["unused"]
    return "\n".join(
        [
            "### Runtime unsafe telemetry",
            "",
            "| scope | unsafe counters |",
            "| --- | --- |",
            f"| used | {used} |",
            f"| unused | {unused} |",
            f"| total | {used + unused} |",
            "",
            "Measurement, not a gate. The rules this replaces are still required",
            "on every pull request: `runtime-unsafe-clippy` (documented unsafe",
            "blocks) and `unsafe-pattern-audit` (provenance, bounds, type tag).",
        ]
    )


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
            "runtime unsafe telemetry: cargo geiger failed "
            "(install it with `cargo install cargo-geiger --locked`)\n"
            f"{detail}"
        )
    try:
        counts = geiger_counts(json.loads(result.stdout))
    except json.JSONDecodeError as error:
        raise SystemExit(
            f"runtime unsafe telemetry: cargo-geiger did not emit JSON: {error}"
        )

    report = render(counts)
    print(report)

    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as handle:
            handle.write(report + "\n")


if __name__ == "__main__":
    main()
