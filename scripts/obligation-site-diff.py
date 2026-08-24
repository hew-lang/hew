#!/usr/bin/env python3
"""Compare obligation under-release sites between two compilers, by site.

Two compilers check the same tree; every `ObligationUnderReleased` diagnostic
is reduced to a NAME-INSENSITIVE key and the two site sets are differenced.

The key is `(file, function, unreleased exit block)` — ONE key per unbalanced
exit. It deliberately excludes the owner's rendered name and the diagnostic
prose, because a compiler change
that renames an owner (`__hew_call_scrutinee` becoming its mint expression,
`snapshot(...)`) or reformats the message moves EVERY site under a text diff
while discharging none of them. Differencing rendered strings therefore scores
renamed sites as fixed, which is how a diagnostic-rendering change can be
reported as a substrate win. Keying on the site itself cannot.

Per-exit granularity is what makes the two renderings comparable. The
pre-aggregation rendering emits one diagnostic per unbalanced exit (the block
named in `reaches <exit>[bbN]`); the aggregated rendering emits one diagnostic
per owner and names every block in an `unreleased exits:` note. Both explode to
the same set of per-exit keys, so a baseline compiler and a head compiler are
comparable. Keying on the whole exit SET instead would score every aggregated
finding as a removal plus an addition, which measures the reshape and not the
ownership.

Removed sites are the claim that needs proving: a site that no longer emits is
either genuinely discharged or silently suppressed, and only an executed leak
oracle over that shape can tell the two apart. This script reports which sites
moved; it never asserts why.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import re
import subprocess
import sys

FUNCTION = re.compile(r"obligation balance in `([^`]+)`")
# Aggregated rendering: `unreleased exits: bb3, bb7`.
EXITS_NOTE = re.compile(r"unreleased exits: ([^\n]+)")
# Per-exit rendering: `... reaches return[bb11] with at least ...`.
REACHES = re.compile(r"\b\w+\[bb(\d+)\]")
BLOCK = re.compile(r"bb(\d+)")

Site = tuple[str, str, int]


def diagnostic_sites(entry: dict, relpath: str) -> set[Site]:
    """The name-insensitive keys for one under-release diagnostic, one per exit."""
    if entry.get("code") != "ObligationUnderReleased":
        return set()
    message = entry.get("message", "")
    function_match = FUNCTION.search(message)
    if function_match is None:
        return set()
    notes = " ".join(note.get("message", "") for note in entry.get("notes", []))
    exits_match = EXITS_NOTE.search(notes)
    if exits_match is not None:
        blocks = [int(block) for block in BLOCK.findall(exits_match.group(1))]
    else:
        blocks = [int(block) for block in REACHES.findall(message)]
    function = function_match.group(1)
    return {(relpath, function, block) for block in blocks}


def sites_for_file(compiler: Path, source: Path, root: Path) -> set[Site]:
    result = subprocess.run(
        [str(compiler), "check", "--format", "json", str(source)],
        capture_output=True,
        text=True,
        timeout=180,
        check=False,
    )
    try:
        entries = json.loads(result.stdout or "[]")
    except json.JSONDecodeError:
        raise SystemExit(
            f"{compiler} produced no JSON diagnostic array for {source}:\n"
            f"{result.stdout}\n{result.stderr}"
        ) from None
    relpath = str(source.relative_to(root))
    found: set[Site] = set()
    for entry in entries:
        found |= diagnostic_sites(entry, relpath)
    return found


def sites_for_tree(compiler: Path, root: Path) -> set[Site]:
    found: set[Site] = set()
    for source in sorted(root.rglob("*.hew")):
        found |= sites_for_file(compiler, source, root)
    return found


def render(label: str, sites: set[Site]) -> str:
    lines = [f"{label} ({len(sites)}):"]
    for relpath, function, block in sorted(sites):
        lines.append(f"  {relpath}::{function} [bb{block}]")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline-bin", type=Path, required=True)
    parser.add_argument("--head-bin", type=Path, required=True)
    parser.add_argument("--tree", type=Path, required=True)
    parser.add_argument(
        "--list",
        action="store_true",
        help="print every moved site, not just the counts",
    )
    args = parser.parse_args()

    baseline = sites_for_tree(args.baseline_bin, args.tree)
    head = sites_for_tree(args.head_bin, args.tree)
    removed = baseline - head
    added = head - baseline
    retained = baseline & head

    print(
        f"obligation-site-diff: tree={args.tree} "
        f"baseline_sites={len(baseline)} head_sites={len(head)} "
        f"removed={len(removed)} added={len(added)} retained={len(retained)}"
    )
    if args.list:
        print(render("removed", removed))
        print(render("added", added))
    return 0


if __name__ == "__main__":
    sys.exit(main())
