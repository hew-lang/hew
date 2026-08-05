#!/usr/bin/env python3
"""Parse `hew test --format junit` output for a shell ratchet gate.

`hew test` supports `--format junit` (hew-cli/src/test_runner/output.rs), but
nothing consumed it before this: scripts/hew-suite-ratchet.sh regex-parsed the
human-readable text output instead. Regex-parsing human output means a
harmless text-format tweak (colour, spacing, a renamed status word) silently
breaks the ratchet's pass/fail extraction without a compiler change. Reading
the JUnit XML reads the same structured data the CI checks UI reads.

Usage (from bash):
    python3 scripts/lib/hew_junit.py <path/to/junit.xml>

Prints, on stdout, one line per test in the form:
    <outcome>\t<test-name>
where <outcome> is one of: ok, FAILED, ignored — the exact vocabulary
scripts/hew-suite-ratchet.sh and scripts/o2-differential.sh already compare
against, so existing consumers of the emitted outcome-line format need no
change.

Then a final summary line:
    __SUMMARY__\t<total>\t<failures>\t<skipped>

taken directly from the <testsuites> root's tests/failures/skipped
attributes — the same totals `mikepenz/action-junit-report` reads.

Exits 1 (with a diagnostic on stderr) on a missing file, malformed XML, or a
document with no <testsuites> root — a parse failure must never read as an
empty (thus vacuously matching) test run.
"""

from __future__ import annotations

import sys

# stdlib xml.etree.ElementTree, not defusedxml: this parses `hew test`'s own
# JUnit output, produced moments earlier by a compiler binary this same CI job
# just built and controls end to end — not attacker-supplied input, so the
# XXE/billion-laughs surface defusedxml guards against does not apply here.
# No project script (scripts/lib/, scripts/*.py) carries a third-party
# dependency; this repo's Python tooling is stdlib-only by convention.
import xml.etree.ElementTree as ET
from pathlib import Path


def parse(path: Path) -> tuple[list[tuple[str, str]], tuple[int, int, int]]:
    """Return (outcomes, (total, failures, skipped)).

    outcomes is a list of (status, test-name) pairs in document order, status
    in {"ok", "FAILED", "ignored"}.
    """
    try:
        root = ET.fromstring(path.read_text(encoding="utf-8"))
    except ET.ParseError as e:
        raise SystemExit(f"error: malformed JUnit XML in {path}: {e}") from e
    if root.tag != "testsuites":
        raise SystemExit(
            f"error: {path}: root element is <{root.tag}>, want <testsuites>"
        )

    try:
        total = int(root.get("tests", ""))
        failures = int(root.get("failures", ""))
        skipped = int(root.get("skipped", ""))
    except ValueError as e:
        raise SystemExit(
            f"error: {path}: <testsuites> tests/failures/skipped attributes are not integers"
        ) from e

    outcomes: list[tuple[str, str]] = []
    for testcase in root.iter("testcase"):
        name = testcase.get("name", "")
        if testcase.find("failure") is not None:
            outcomes.append(("FAILED", name))
        elif testcase.find("skipped") is not None:
            outcomes.append(("ignored", name))
        else:
            outcomes.append(("ok", name))

    return outcomes, (total, failures, skipped)


def main(argv: list[str]) -> int:
    if len(argv) != 1:
        print("usage: python3 scripts/lib/hew_junit.py <junit.xml>", file=sys.stderr)
        return 2
    path = Path(argv[0])
    if not path.is_file():
        print(f"error: JUnit report not found: {path}", file=sys.stderr)
        return 1

    outcomes, (total, failures, skipped) = parse(path)

    counted_failures = sum(1 for status, _ in outcomes if status == "FAILED")
    if counted_failures != failures:
        print(
            f'error: {path}: <testsuites failures="{failures}"> does not match '
            f"{counted_failures} counted <failure> testcase(s); refusing to ratchet "
            "against an inconsistent report",
            file=sys.stderr,
        )
        return 1

    for status, name in outcomes:
        print(f"{status}\t{name}")
    print(f"__SUMMARY__\t{total}\t{failures}\t{skipped}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
