#!/usr/bin/env python3
"""Plot calibration shapes from saved receipts (requires matplotlib)."""

import argparse
import json
from pathlib import Path
import statistics

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402


COLOURS = {
    "hew": "#ad3348",
    "rust": "#303b50",
    "rust-checked": "#aa7d25",
    "c": "#898989",
    "go": "#138796",
    "tokio": "#6154a4",
    "beam": "#43864b",
}
LABELS = {
    "hew": "Hew O2",
    "rust": "Rust O2",
    "rust-checked": "Rust O2 checked",
    "c": "C O2",
    "go": "Go",
    "tokio": "Tokio",
    "beam": "BEAM",
}


def series(axis, rows, workload, languages, xkey, denominator):
    for language in languages:
        grouped = {}
        for row in rows:
            if (
                row["language"] == language
                and row["workload"] == workload
                and row["workers"] == 1
            ):
                grouped.setdefault(row[xkey], []).append(
                    row["elapsed_ns"] / denominator(row)
                )
        xs = sorted(grouped)
        axis.plot(
            xs,
            [statistics.median(grouped[x]) for x in xs],
            "o-",
            color=COLOURS[language],
            label=LABELS[language],
            linewidth=1.8,
        )
    axis.set_xscale("log", base=2)
    axis.set_yscale("log")
    axis.grid(alpha=0.2)
    axis.legend(fontsize=8)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("receipts", type=Path, nargs="+")
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    receipts = [json.loads(path.read_text()) for path in args.receipts]
    rows = [row for receipt in receipts for row in receipt["runtime"]]
    matplotlib.rcParams.update(
        {"font.family": "DejaVu Sans", "svg.hashsalt": "hew-calibration"}
    )
    fig, axes = plt.subplots(2, 2, figsize=(12, 8), layout="constrained")
    axis = axes[0, 0]
    series(
        axis,
        rows,
        "vector_scan",
        ["hew", "rust", "rust-checked", "c", "go"],
        "size",
        lambda row: row["iterations"] * row["size"],
    )
    axis.set(
        title="Vector scan: safety checks change the shape",
        xlabel="Elements in buffer",
        ylabel="ns per visited element (log scale)",
    )
    axis = axes[0, 1]
    series(
        axis,
        rows,
        "task_ready",
        ["hew", "go", "tokio", "beam"],
        "size",
        lambda row: row["iterations"] * 1000,
    )
    axis.set(
        title="Ready tasks: batching changes lifecycle cost",
        xlabel="Tasks per batch, one worker",
        ylabel="µs per task (log scale)",
    )
    axis = axes[1, 0]
    for language in ("hew", "go", "tokio", "beam"):
        values = sorted(
            sample / 1e6
            for row in rows
            if row["language"] == language
            and row["workload"] == "timer"
            and row["workers"] == 1
            for sample in row["latencies_ns"]
        )
        axis.step(
            values,
            [(i + 1) / len(values) * 100 for i in range(len(values))],
            where="post",
            color=COLOURS[language],
            label=LABELS[language],
        )
    axis.set(
        title="1 ms sleeps: distribution of wakeup overshoot",
        xlabel="Overshoot beyond requested 1 ms (ms)",
        ylabel="Completed waits (%)",
        xlim=(0, 3),
        ylim=(0, 101),
    )
    axis.grid(alpha=0.2)
    axis.legend(fontsize=8)
    axis = axes[1, 1]
    for language in ("hew", "rust", "c", "go"):
        grouped = {}
        for receipt in receipts:
            for row in receipt["compilation_scaling"]:
                if row["language"] == language and row["sample"] > 0:
                    grouped.setdefault(row["functions"], []).append(
                        row["elapsed_ns"] / 1e6
                    )
        xs = sorted(grouped)
        label = LABELS[language] + (" (cache hits)" if language == "go" else "")
        axis.plot(
            xs,
            [statistics.median(grouped[x]) for x in xs],
            "o-",
            color=COLOURS[language],
            label=label,
        )
    axis.set(
        title="Unchanged application rebuilds, including linking",
        xlabel="Generated scalar functions",
        ylabel="Median build time (ms)",
    )
    axis.set_xscale("log", base=2)
    axis.grid(alpha=0.2)
    axis.legend(fontsize=8)
    fig.suptitle(
        "Hew behaviour calibration · Linux x86_64 · 16 October 2026\n"
        "Shared host, two-CPU affinity; broad shapes, not performance promises",
        fontsize=14,
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(
        args.output, metadata={"Date": None} if args.output.suffix == ".svg" else {}
    )
    if args.output.suffix == ".svg":
        args.output.write_text(
            "\n".join(line.rstrip() for line in args.output.read_text().splitlines())
            + "\n"
        )


if __name__ == "__main__":
    main()
