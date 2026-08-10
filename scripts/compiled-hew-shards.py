#!/usr/bin/env python3
"""Run and aggregate stable partitions of the compiled-Hew test suites."""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import sys
from typing import NoReturn
import xml.etree.ElementTree as ET


REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO_ROOT / "scripts" / "lib"))
from corpus_floor import assert_floor  # noqa: E402


PARTITION_RE = re.compile(r"^hash:([1-9][0-9]*)/([1-9][0-9]*)$")


def die(message: str) -> NoReturn:
    print(f"compiled-hew-shards: {message}", file=sys.stderr)
    raise SystemExit(1)


def read_inventory(path: Path) -> list[str]:
    try:
        identities = [
            line for line in path.read_text(encoding="utf-8").splitlines() if line
        ]
    except FileNotFoundError:
        die(f"inventory is missing: {path}")
    if not identities:
        die(f"inventory is empty: {path}")
    seen: set[str] = set()
    duplicates: set[str] = set()
    for identity in identities:
        if identity in seen:
            duplicates.add(identity)
        seen.add(identity)
    if duplicates:
        die(
            f"inventory contains duplicate identities: {path}: {sorted(duplicates)[:5]}"
        )
    return identities


def normalized_identity(classname: str, name: str) -> str:
    if not classname or not name:
        die("JUnit testcase is missing classname or name")
    file = Path(classname)
    if file.is_absolute():
        try:
            file = file.resolve().relative_to(REPO_ROOT)
        except ValueError:
            die(f"JUnit classname is outside the repository: {classname}")
    return f"{file.as_posix()}::{name}"


def parse_junit(path: Path) -> dict[str, str]:
    try:
        root = ET.fromstring(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        die(f"JUnit report is missing: {path}")
    except ET.ParseError as error:
        die(f"malformed JUnit report {path}: {error}")
    if root.tag != "testsuites":
        die(f"JUnit report {path} has <{root.tag}> root, expected <testsuites>")
    outcomes: dict[str, str] = {}
    failures = 0
    skipped = 0
    for testcase in root.iter("testcase"):
        identity = normalized_identity(
            testcase.get("classname", ""), testcase.get("name", "")
        )
        if identity in outcomes:
            die(
                f"JUnit report contains duplicate testcase identity: {path}: {identity}"
            )
        if testcase.find("failure") is not None:
            outcome = "FAILED"
            failures += 1
        elif testcase.find("skipped") is not None:
            outcome = "ignored"
            skipped += 1
        else:
            outcome = "ok"
        outcomes[identity] = outcome
    try:
        declared = int(root.get("tests", ""))
        declared_failures = int(root.get("failures", ""))
        declared_skipped = int(root.get("skipped", ""))
    except ValueError:
        die(f"JUnit report has non-integer summary attributes: {path}")
    if (declared, declared_failures, declared_skipped) != (
        len(outcomes),
        failures,
        skipped,
    ):
        die(
            f"JUnit summary disagrees with testcase elements: {path}: "
            f"declared={(declared, declared_failures, declared_skipped)} "
            f"counted={(len(outcomes), failures, skipped)}"
        )
    return outcomes


def run_command(
    command: list[str], output: Path, stderr: Path, environment: dict[str, str]
) -> int:
    with (
        output.open("w", encoding="utf-8") as stdout_handle,
        stderr.open("w", encoding="utf-8") as stderr_handle,
    ):
        result = subprocess.run(
            command,
            cwd=REPO_ROOT,
            env=environment,
            stdout=stdout_handle,
            stderr=stderr_handle,
        )
    return result.returncode


def run_shard(compiler: Path, partition: str, output_dir: Path) -> None:
    match = PARTITION_RE.fullmatch(partition)
    if match is None or int(match.group(1)) > int(match.group(2)):
        die(f"invalid partition {partition!r}; expected hash:SHARD/TOTAL")
    shard = int(match.group(1))
    output_dir.mkdir(parents=True, exist_ok=True)
    inventory = output_dir / f"hew-inventory-shard-{shard}.txt"
    list_result = subprocess.run(
        [str(compiler), "test", "tests/hew", "--list", "--partition", partition],
        cwd=REPO_ROOT,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if list_result.returncode:
        die(f"test listing failed for {partition}: {list_result.stderr.strip()}")
    inventory.write_text(list_result.stdout, encoding="utf-8")
    expected = set(read_inventory(inventory))

    metadata: dict[str, object] = {"partition": partition, "tests": len(expected)}
    for optimization, label in (("0", "o0"), ("2", "o2")):
        report = output_dir / f"hew-{label}-shard-{shard}.xml"
        stderr = output_dir / f"hew-{label}-shard-{shard}.stderr.log"
        environment = dict(os.environ)
        environment["HEW_OPT_LEVEL"] = optimization
        returncode = run_command(
            [
                str(compiler),
                "test",
                "tests/hew",
                "--partition",
                partition,
                "--format",
                "junit",
            ],
            report,
            stderr,
            environment,
        )
        outcomes = parse_junit(report)
        if set(outcomes) != expected:
            die(
                f"{label.upper()} report inventory differs from the listed {partition} inventory"
            )
        expected_returncode = 1 if "FAILED" in outcomes.values() else 0
        if returncode != expected_returncode:
            die(
                f"{label.upper()} runner exited {returncode}, expected {expected_returncode} "
                "from its JUnit outcomes"
            )
        metadata[f"{label}_returncode"] = returncode

    (output_dir / f"hew-shard-{shard}.json").write_text(
        json.dumps(metadata, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )


def expected_failures(path: Path, full: set[str]) -> set[str]:
    names: list[str] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        value = line.split("#", 1)[0].strip()
        if value:
            names.append(value.split()[0])
    if len(names) != len(set(names)):
        die(f"expected-failures file contains duplicate names: {path}")
    by_name: dict[str, list[str]] = {}
    for identity in full:
        by_name.setdefault(identity.rsplit("::", 1)[-1], []).append(identity)
    identities: set[str] = set()
    for name in names:
        matches = by_name.get(name, [])
        if len(matches) != 1:
            die(
                f"expected failure {name!r} maps to {len(matches)} full-suite identities; "
                "expected exactly one"
            )
        identities.add(matches[0])
    return identities


def load_shards(
    reports_dir: Path, full: set[str], shard_count: int
) -> tuple[dict[str, str], dict[str, str]]:
    owner: dict[str, int] = {}
    combined_o0: dict[str, str] = {}
    combined_o2: dict[str, str] = {}
    for shard in range(1, shard_count + 1):
        inventory_path = reports_dir / f"hew-inventory-shard-{shard}.txt"
        inventory = set(read_inventory(inventory_path))
        for identity in inventory:
            if identity in owner:
                die(
                    f"shard inventories overlap: {identity} appears in shards "
                    f"{owner[identity]} and {shard}"
                )
            owner[identity] = shard
        o0 = parse_junit(reports_dir / f"hew-o0-shard-{shard}.xml")
        o2 = parse_junit(reports_dir / f"hew-o2-shard-{shard}.xml")
        if set(o0) != inventory:
            die(f"O0 report for shard {shard} differs from its inventory")
        if set(o2) != inventory:
            die(f"O2 report for shard {shard} differs from its inventory")
        combined_o0.update(o0)
        combined_o2.update(o2)
    union = set(owner)
    if union != full:
        missing = sorted(full - union)
        extra = sorted(union - full)
        die(
            "union of shard inventories does not equal the full inventory: "
            f"missing={missing[:5]} extra={extra[:5]}"
        )
    return combined_o0, combined_o2


def aggregate(
    mode: str,
    reports_dir: Path,
    full_inventory: Path,
    shard_count: int,
    expected_failures_path: Path,
) -> None:
    if shard_count < 2:
        die("shard count must be at least two")
    full = set(read_inventory(full_inventory))
    o0, o2 = load_shards(reports_dir, full, shard_count)
    floor_key = "hew-suite-tests" if mode == "ratchet" else "o2-differential-outcomes"
    assert_floor(floor_key, len(full), context="union of compiled-Hew shards")

    if mode == "ratchet":
        expected = expected_failures(expected_failures_path, full)
        actual = {identity for identity, outcome in o0.items() if outcome == "FAILED"}
        if actual != expected:
            die(
                "O0 shard failure set differs from the ratchet: "
                f"unexpected={sorted(actual - expected)[:5]} "
                f"now_passing={sorted(expected - actual)[:5]}"
            )
        print(
            f"compiled-Hew ratchet passed: {len(full)} tests across {shard_count} shards; "
            f"{len(actual)} tracked failures"
        )
        return

    differences = [
        identity for identity in sorted(full) if o0.get(identity) != o2.get(identity)
    ]
    if differences:
        examples = [
            f"{identity}: O0={o0.get(identity)} O2={o2.get(identity)}"
            for identity in differences[:5]
        ]
        die("O0/O2 shard outcomes differ: " + "; ".join(examples))
    print(
        f"compiled-Hew differential passed: {len(full)} identical outcomes across "
        f"{shard_count} shards"
    )


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    subcommands = parser.add_subparsers(dest="action", required=True)
    run_parser = subcommands.add_parser("run")
    run_parser.add_argument("--compiler", type=Path, required=True)
    run_parser.add_argument("--partition", required=True)
    run_parser.add_argument("--output-dir", type=Path, required=True)
    aggregate_parser = subcommands.add_parser("aggregate")
    aggregate_parser.add_argument(
        "--mode", choices=("ratchet", "differential"), required=True
    )
    aggregate_parser.add_argument("--reports-dir", type=Path, required=True)
    aggregate_parser.add_argument("--full-inventory", type=Path, required=True)
    aggregate_parser.add_argument("--shard-count", type=int, required=True)
    aggregate_parser.add_argument(
        "--expected-failures",
        type=Path,
        default=REPO_ROOT / "scripts" / "hew-suite-expected-failures.txt",
    )
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    if args.action == "run":
        run_shard(args.compiler.resolve(), args.partition, args.output_dir)
    else:
        aggregate(
            args.mode,
            args.reports_dir,
            args.full_inventory,
            args.shard_count,
            args.expected_failures,
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
