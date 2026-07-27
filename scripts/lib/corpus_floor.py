#!/usr/bin/env python3
"""Tracked-minimum assertion for gates that enumerate a corpus (Python side).

The Python twin of scripts/lib/corpus-floor.sh; both read the single registry
at scripts/corpus-floors.tsv so a floor is declared once, in one place,
whatever language its gate is written in. See that shell file for the rationale
and the registry format.

    from corpus_floor import assert_floor, check_floor

    assert_floor("sandbox-vm-binaries", len(binaries))   # prints, exits 1
    err = check_floor("fuzz-oracle-candidates", total)   # returns message|None
"""

from __future__ import annotations

import sys
from pathlib import Path

REGISTRY = Path(__file__).resolve().parent.parent / "corpus-floors.tsv"
REGISTRY_DISPLAY = "scripts/corpus-floors.tsv"


def _load_rows() -> dict[str, tuple[str, str, str, str]]:
    rows: dict[str, tuple[str, str, str, str]] = {}
    for line in REGISTRY.read_text(encoding="utf-8").splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        fields = line.split("\t")
        if len(fields) != 5:
            raise SystemExit(
                f"corpus-floor: malformed row in {REGISTRY_DISPLAY}: {line!r} "
                "(want key<TAB>mode<TAB>floor<TAB>slack<TAB>description)"
            )
        key, mode, floor, slack, description = fields
        rows[key] = (mode, floor, slack, description)
    return rows


def check_floor(key: str, actual: int, context: str = "") -> str | None:
    """Return None when `actual` satisfies the registry row for `key`.

    Otherwise return the multi-line diagnostic to print. An unknown key is an
    error, never a pass: a gate cannot opt out by dropping its row.
    """
    rows = _load_rows()
    label = f"{key} ({context})" if context else key
    row = rows.get(key)
    if row is None:
        return (
            f"corpus-floor: no registry row for '{key}' in {REGISTRY_DISPLAY}\n"
            "              A gate may not assert against a floor it has not "
            "declared.\n"
            "              Add a row: <key>\t<exact|min>\t<floor>\t<slack|->\t"
            "<description>"
        )
    mode, floor_text, slack_text, description = row
    if not floor_text.isdigit() or int(floor_text) < 1:
        return (
            f"corpus-floor: {key}: registry floor must be a positive integer, "
            f"got {floor_text!r}"
        )
    floor = int(floor_text)

    if mode == "exact":
        if actual == floor:
            return None
        head = (
            f"\nCORPUS FLOOR: {label} enumerated {actual}, expected exactly "
            f"{floor}\n              {description}\n"
        )
        if actual < floor:
            return head + (
                "              The corpus SHRANK. Everything this gate "
                "compares, it now\n"
                "              compares over less than it did last time — an "
                "empty or\n"
                "              shrunken enumeration passes every comparison "
                "vacuously.\n"
                + (
                    "              An enumeration of zero is never a floor — "
                    "if this gate has\n"
                    "              no corpus left, delete the row and the "
                    "assertion together."
                    if actual == 0
                    else f"              If the removal is intended, set the "
                    f"count to {actual} in\n"
                    f"              {REGISTRY_DISPLAY} in the SAME commit and "
                    "justify it in the body."
                )
            )
        return head + (
            f"              The corpus GREW. Set the count to {actual} in "
            f"{REGISTRY_DISPLAY} in the\n"
            "              SAME commit that adds the fixtures — that is how "
            "the next\n"
            "              contributor inherits a floor that still means "
            "something."
        )

    if mode == "min":
        if not slack_text.isdigit():
            return (
                f"corpus-floor: {key}: mode=min requires an integer slack, got "
                f"{slack_text!r}"
            )
        slack = int(slack_text)
        if actual < floor:
            return (
                f"\nCORPUS FLOOR: {label} enumerated {actual}, floor is {floor}\n"
                f"              {description}\n"
                "              The corpus SHRANK below its tracked minimum. "
                "This gate's\n"
                "              comparison is only worth the corpus it ran "
                "over; below the\n"
                "              floor it proves less than it did last time.\n"
                "              Check first for a wrong directory, a renamed "
                "fixture, a\n"
                "              filter that matches nothing, or a build that "
                "produced no\n"
                "              tests. If the shrink is intended, lower the "
                f"floor to {actual}\n"
                f"              in {REGISTRY_DISPLAY} in the SAME commit and "
                "justify it in the body."
            )
        if actual > floor + slack:
            return (
                f"\nCORPUS FLOOR: {label} enumerated {actual}, floor is {floor} "
                f"(slack {slack})\n"
                f"              {description}\n"
                f"              The corpus has grown {actual - floor} past its "
                "floor, so the\n"
                "              floor no longer protects what is actually "
                "there. Raise it to\n"
                f"              {actual} in {REGISTRY_DISPLAY} in this commit."
            )
        return None

    return f"corpus-floor: {key}: unknown mode {mode!r} (want exact or min)"


def assert_floor(key: str, actual: int, context: str = "") -> None:
    """Print and exit 1 when the floor is violated; print the count otherwise."""
    error = check_floor(key, actual, context)
    if error is not None:
        print(error, file=sys.stderr)
        raise SystemExit(1)
    rows = _load_rows()
    mode, floor, _slack, _description = rows[key]
    label = f"{key} ({context})" if context else key
    print(f"corpus floor OK: {label} = {actual} ({mode} {floor})")


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        print(
            "usage: python3 scripts/lib/corpus_floor.py <key> <actual-count> [context]",
            file=sys.stderr,
        )
        return 2
    try:
        actual = int(argv[1])
    except ValueError:
        print(
            f"corpus-floor: {argv[0]}: actual count must be an integer, got "
            f"{argv[1]!r}",
            file=sys.stderr,
        )
        return 1
    error = check_floor(argv[0], actual, argv[2] if len(argv) > 2 else "")
    if error is not None:
        print(error, file=sys.stderr)
        return 1
    print(f"corpus floor OK: {argv[0]} = {actual}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
