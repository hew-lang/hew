#!/usr/bin/env python3
"""Validate and execute the unconditional Linux CI shard assignment."""

from __future__ import annotations

import argparse
import shlex
import subprocess
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ASSIGNMENT = ROOT / "scripts" / "ci-gate-shards.tsv"
SHARD_COUNT = 4

sys.path.insert(0, str(ROOT / "scripts" / "lib"))
import gate_inputs  # noqa: E402


def assignment() -> list[tuple[int, str]]:
    rows: list[tuple[int, str]] = []
    for number, raw_line in enumerate(ASSIGNMENT.read_text().splitlines(), 1):
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        fields = line.split("\t")
        if len(fields) != 2:
            raise ValueError(f"{ASSIGNMENT}:{number}: expected shard<TAB>command")
        try:
            shard = int(fields[0])
        except ValueError as error:
            raise ValueError(
                f"{ASSIGNMENT}:{number}: shard must be an integer"
            ) from error
        if not 1 <= shard <= SHARD_COUNT:
            raise ValueError(
                f"{ASSIGNMENT}:{number}: shard must be between 1 and {SHARD_COUNT}"
            )
        command = fields[1].strip()
        if not command:
            raise ValueError(f"{ASSIGNMENT}:{number}: command is empty")
        rows.append((shard, command))
    return rows


def expected_commands() -> set[str]:
    gates, _global_inputs, _no_gate = gate_inputs.parse_makefile(
        (ROOT / "Makefile").read_text()
    )
    commands = {f"make {gate.target}" for gate in gates if gate.participates()}
    commands.update(
        {
            "cargo fmt --all -- --check",
            "cargo clippy --workspace --tests -- -D warnings",
            "make hew-profile-check",
            "make baselines-check BASELINE_TIER=fast",
        }
    )
    return commands


def validate(rows: list[tuple[int, str]]) -> None:
    commands = [command for _shard, command in rows]
    duplicates = sorted(
        command for command, count in Counter(commands).items() if count > 1
    )
    if duplicates:
        raise ValueError(f"commands assigned more than once: {', '.join(duplicates)}")

    actual = set(commands)
    expected = expected_commands()
    missing = sorted(expected - actual)
    extra = sorted(actual - expected)
    if missing or extra:
        details = []
        if missing:
            details.append(f"missing: {', '.join(missing)}")
        if extra:
            details.append(f"unknown: {', '.join(extra)}")
        raise ValueError(
            "static shard assignment is not exhaustive: " + "; ".join(details)
        )

    counts = Counter(shard for shard, _command in rows)
    empty = [str(shard) for shard in range(1, SHARD_COUNT + 1) if counts[shard] == 0]
    if empty:
        raise ValueError(f"empty shards: {', '.join(empty)}")


def run_commands(
    rows: list[tuple[int, str]], shards: set[int], keep_going: bool
) -> int:
    failures = 0
    for shard, command in rows:
        if shard not in shards:
            continue
        print(f"==> shard {shard}/{SHARD_COUNT}: {command}", flush=True)
        result = subprocess.run(shlex.split(command), cwd=ROOT, check=False)
        if result.returncode == 0:
            continue
        failures += 1
        print(
            f"error: shard {shard}/{SHARD_COUNT} command exited {result.returncode}: {command}",
            file=sys.stderr,
        )
        if not keep_going:
            return result.returncode
    return 1 if failures else 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", choices=("check", "run", "run-all"))
    parser.add_argument("shard", nargs="?", type=int)
    parser.add_argument("--keep-going", action="store_true")
    args = parser.parse_args()

    try:
        rows = assignment()
        validate(rows)
    except ValueError as error:
        print(f"error: {error}", file=sys.stderr)
        return 2

    counts = Counter(shard for shard, _command in rows)
    print(
        "Static Linux CI shards: "
        + ", ".join(
            f"{shard}/{SHARD_COUNT}={counts[shard]} commands"
            for shard in range(1, SHARD_COUNT + 1)
        )
    )
    if args.mode == "check":
        if args.shard is not None:
            parser.error("check does not accept a shard")
        print(f"All {len(rows)} commands are assigned exactly once.")
        return 0
    if args.mode == "run":
        if args.shard is None or not 1 <= args.shard <= SHARD_COUNT:
            parser.error(f"run requires a shard between 1 and {SHARD_COUNT}")
        return run_commands(rows, {args.shard}, args.keep_going)
    if args.shard is not None:
        parser.error("run-all does not accept a shard")
    return run_commands(rows, set(range(1, SHARD_COUNT + 1)), args.keep_going)


if __name__ == "__main__":
    raise SystemExit(main())
