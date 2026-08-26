#!/usr/bin/env python3
"""Assert the platform behaviour suite actually selects tests.

    cargo nextest list --profile platform --message-format json ... > listing.json
    python3 scripts/platform-suite-floor.py listing.json

A `default-filter` that matches nothing produces a green `nextest run`, a green
job, and a green required check having executed no test at all. That is the
vacuity this whole substrate exists to remove, so the floor is a hard red.

The floor is NON-EMPTY and nothing else (LESSONS.md enumeration-gate-floors).
An exact expected count would be a change-detector that fires on every added
test and tells a reviewer nothing about whether the platform surface is
covered.

Counting is done over nextest's MACHINE format, not its human output: test
records are read out of the JSON `rust-suites[*].testcases` map. Counting
non-blank lines of human output is a different measurement wearing the same
name -- it moves with progress lines, warnings, colour codes, and a
`--list-type` change, and any of those would silently turn "no tests selected"
into a number greater than zero.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path


class FloorError(RuntimeError):
    """A condition that must be red rather than reported as a count."""


def parse_listing(text: str) -> dict[str, list[str]]:
    """Test names per binary, from `cargo nextest list --message-format json`.

    A payload this cannot read is an ERROR, never zero: "I could not tell how
    many tests were selected" and "no tests were selected" are different
    findings, and collapsing them would report a broken command as a broken
    filter, or worse, the reverse.
    """
    try:
        document = json.loads(text)
    except json.JSONDecodeError as error:
        raise FloorError(f"listing is not valid JSON: {error}") from error
    if not isinstance(document, dict):
        raise FloorError("listing is not a JSON object")
    suites = document.get("rust-suites")
    if not isinstance(suites, dict):
        raise FloorError(
            "listing has no `rust-suites` map; this is not "
            "`cargo nextest list --message-format json` output"
        )
    selected: dict[str, list[str]] = {}
    for name, suite in suites.items():
        if not isinstance(suite, dict):
            raise FloorError(f"suite {name!r} is not an object")
        cases = suite.get("testcases")
        if cases is None:
            continue
        if not isinstance(cases, dict):
            raise FloorError(f"suite {name!r} has a non-object `testcases`")
        if cases:
            selected[name] = sorted(cases)
    return selected


def summarise(selected: dict[str, list[str]]) -> str:
    total = sum(len(cases) for cases in selected.values())
    lines = [
        f"Platform behaviour suite: {total} test(s) across {len(selected)} binaries"
    ]
    lines.append("")
    lines.append("| Binary | Tests |")
    lines.append("| --- | ---: |")
    for name in sorted(selected):
        lines.append(f"| `{name}` | {len(selected[name])} |")
    return "\n".join(lines)


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("listing", type=Path)
    parser.add_argument(
        "--summary",
        type=Path,
        default=None,
        help="append the per-binary table here (defaults to $GITHUB_STEP_SUMMARY)",
    )
    args = parser.parse_args(argv)

    try:
        selected = parse_listing(args.listing.read_text(encoding="utf-8"))
    except (OSError, FloorError) as error:
        print(f"::error::platform suite floor: {error}", file=sys.stderr)
        return 1

    total = sum(len(cases) for cases in selected.values())
    report = summarise(selected)
    print(report)

    destination = args.summary
    if destination is None:
        env_path = os.environ.get("GITHUB_STEP_SUMMARY")
        destination = Path(env_path) if env_path else None
    if destination is not None:
        with destination.open("a", encoding="utf-8") as handle:
            handle.write(report + "\n")

    if total == 0:
        print(
            "::error::the platform nextest profile selected no tests; a suite "
            "that runs nothing cannot report a platform regression",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
