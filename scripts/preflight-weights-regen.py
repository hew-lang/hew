#!/usr/bin/env python3
"""Regenerate the preflight shard-balance timing corpus from a real run.

    scripts/ci-preflight-dispatcher.sh --comprehensive --profile-json run.json
    make preflight-weights-regen PROFILE_JSON=run.json

Reads the dispatcher's own `--profile-json` output and rewrites
`scripts/preflight-command-weights.tsv` with the measured elapsed seconds per
command. This replaces refreshing a case statement by hand, which is how a
number in the middle of a router decays without anybody noticing.

Design rules, each with a reason:

  * Only SUCCESSFUL command phases contribute. A timing taken from a run that
    died halfway is not a measurement of that command.
  * Warm-up phases are excluded. They are derived per shard from the selected
    commands, so folding them in would double-count the build the gate needs.
  * The regen writes into an isolated snapshot and diffs it against the tree,
    so `--check` can report drift without mutating anything
    (LESSONS.md regen-checks).
  * A profile with no usable rows is an ERROR, not an empty file. Silently
    truncating the corpus would hand every command the default weight and call
    it a regeneration.

Nothing runs this automatically. Drift is a number a human reads occasionally,
not a control loop; an auto-PR bot is a new moving part with its own failure
modes, and the plan this implements says so explicitly.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CORPUS = ROOT / "scripts" / "preflight-command-weights.tsv"

HEADER = """\
# Preflight command timings — the shard partitioner's balance input.
#
# Format: <seconds><TAB><command>
#
# These are MEASUREMENTS, not policy. The partitioner uses them only to balance
# the four Linux shards; a stale weight costs makespan, never coverage, because
# the partition is exhaustive and disjoint whatever the weights say. A command
# absent from this file falls back to its timeout floor or the default weight,
# so a new gate is never unrunnable for want of a row here.
#
# Regenerate from a real run rather than editing by hand:
#
#     scripts/ci-preflight-dispatcher.sh --comprehensive --profile-json run.json
#     make preflight-weights-regen PROFILE_JSON=run.json
#
# The regen reads the `elapsed_s` of each successful command phase and rewrites
# this file sorted by descending cost. Nothing regenerates it automatically: a
# bot that opens pull requests is a new moving part with its own failure modes,
# and this is a number a human reads occasionally, not a control loop.
"""


def measured(profile: list) -> dict[str, int]:
    """Elapsed seconds per successful command phase, rounded to a second."""
    timings: dict[str, int] = {}
    for step in profile:
        if not isinstance(step, dict):
            continue
        if step.get("phase") != "command":
            continue
        if step.get("status") not in (None, "ok", "pass", "success", 0, "0"):
            continue
        command = step.get("cmd")
        elapsed = step.get("elapsed_s")
        if not isinstance(command, str) or not command.strip():
            continue
        try:
            seconds = int(round(float(elapsed)))
        except (TypeError, ValueError):
            continue
        if seconds <= 0:
            continue
        timings[command.strip()] = max(seconds, timings.get(command.strip(), 0))
    return timings


def render(timings: dict[str, int]) -> str:
    rows = sorted(timings.items(), key=lambda item: (-item[1], item[0]))
    body = "".join(f"{seconds}\t{command}\n" for command, seconds in rows)
    return HEADER + body


def parse(text: str) -> dict[str, int]:
    """Read a corpus back, for the drift report."""
    found: dict[str, int] = {}
    for line in text.splitlines():
        if line.lstrip().startswith("#") or "\t" not in line:
            continue
        seconds, _, command = line.partition("\t")
        try:
            found[command.strip()] = int(seconds.strip())
        except ValueError:
            continue
    return found


def drift(old: dict[str, int], new: dict[str, int]) -> list[str]:
    lines: list[str] = []
    for command in sorted(set(old) | set(new)):
        before, after = old.get(command), new.get(command)
        if before is None:
            lines.append(f"+ {after:>6}s  {command}  (newly measured)")
        elif after is None:
            lines.append(f"? {before:>6}s  {command}  (not in this run)")
        elif before != after:
            delta = after - before
            lines.append(f"~ {after:>6}s  {command}  ({delta:+d}s from {before}s)")
    return lines


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("profile_json", type=Path)
    parser.add_argument(
        "--check",
        action="store_true",
        help="report drift and exit 0 without writing; never gates anything",
    )
    parser.add_argument("--output", type=Path, default=CORPUS)
    args = parser.parse_args(argv)

    try:
        profile = json.loads(args.profile_json.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        print(f"error: cannot read {args.profile_json}: {error}", file=sys.stderr)
        return 1
    if not isinstance(profile, list):
        print("error: --profile-json output must be a JSON array", file=sys.stderr)
        return 1

    timings = measured(profile)
    if not timings:
        print(
            "error: no successful command phase carried a usable elapsed_s; "
            "refusing to write an empty corpus, which would silently hand every "
            "command the default weight and call it a regeneration",
            file=sys.stderr,
        )
        return 1

    existing = (
        parse(args.output.read_text(encoding="utf-8")) if args.output.exists() else {}
    )
    changes = drift(existing, timings)

    if args.check:
        print(f"==> preflight weight drift ({len(changes)} change(s))")
        for line in changes:
            print(f"    {line}")
        if not changes:
            print("    corpus matches this run")
        return 0

    args.output.write_text(render(timings), encoding="utf-8")
    print(f"wrote {args.output} ({len(timings)} command timing(s))")
    for line in changes:
        print(f"    {line}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
