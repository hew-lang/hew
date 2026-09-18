#!/usr/bin/env python3
"""ledger-issue-check.py — flag ledger rows citing a closed issue.

The four expected-failure ledgers pin a defect to an open issue. A row
citing an issue that has since closed is stale: either the defect is fixed
and the row should be deleted, or the fix landed under a different issue and
the row should be re-pointed. This script is read-only — it reports, it
does not edit the ledgers.

Requires `gh` (authenticated) and network access, so it runs as an
informational step in the nightly ratchet-accounting workflow, not in PR CI.
"""

from __future__ import annotations

import json
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

LEDGERS = [
    ROOT / "scripts" / "nextest-expected-failures.tsv",
    ROOT / "scripts" / "hew-suite-expected-failures.txt",
    ROOT / "scripts" / "hew-corpus-expected-failures.txt",
    ROOT / "tests" / "core-acceptance" / "expected-failures.txt",
]

ISSUE_RE = re.compile(r"#(\d+)")


def find_citations() -> list[tuple[Path, int, str, int]]:
    """Return (path, line number, line text, issue number) for every #NNNN row."""
    citations = []
    for path in LEDGERS:
        if not path.exists():
            continue
        for number, line in enumerate(path.read_text().splitlines(), start=1):
            stripped = line.strip()
            if not stripped:
                continue
            for match in ISSUE_RE.finditer(line):
                citations.append((path, number, line, int(match.group(1))))
    return citations


def query_issue_states(issue_numbers: set[int]) -> dict[int, str]:
    """One gh api graphql call for every cited issue's state."""
    if not issue_numbers:
        return {}
    fields = "\n".join(
        f"i{number}: issue(number: {number}) {{ number state }}"
        for number in sorted(issue_numbers)
    )
    query = f"""
    query {{
      repository(owner: "hew-lang", name: "hew") {{
        {fields}
      }}
    }}
    """
    result = subprocess.run(
        ["gh", "api", "graphql", "-f", f"query={query}"],
        capture_output=True,
        text=True,
        check=True,
    )
    data = json.loads(result.stdout)["data"]["repository"]
    states: dict[int, str] = {}
    for key, value in data.items():
        if value is None:
            continue
        states[value["number"]] = value["state"]
    return states


def main() -> int:
    citations = find_citations()
    if not citations:
        print("no #NNNN citations found across the ledgers")
        return 0

    issue_numbers = {citation[3] for citation in citations}
    states = query_issue_states(issue_numbers)

    closed_rows = [
        citation for citation in citations if states.get(citation[3]) == "CLOSED"
    ]

    print(
        f"checked {len(issue_numbers)} distinct issue(s) across {len(citations)} row(s)"
    )
    if not closed_rows:
        print("no ledger row cites a closed issue")
        return 0

    print("rows citing a closed issue:")
    for path, number, line, issue in closed_rows:
        print(
            f"  {path.relative_to(ROOT)}:{number}: #{issue} is CLOSED — {line.strip()}"
        )
    return 1


if __name__ == "__main__":
    sys.exit(main())
