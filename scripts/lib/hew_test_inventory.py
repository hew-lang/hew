#!/usr/bin/env python3
"""List and validate the compiled-Hew suite's test identities."""

from __future__ import annotations

import argparse
from pathlib import Path
import subprocess
import sys
from typing import NoReturn
import xml.etree.ElementTree as ET


REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_EXPECTED = REPO_ROOT / "scripts/hew-suite-expected-tests.txt"


def die(message: str) -> NoReturn:
    print(f"hew-test-inventory: {message}", file=sys.stderr)
    raise SystemExit(1)


def normalize_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return resolved.relative_to(REPO_ROOT).as_posix()
    except ValueError:
        return resolved.as_posix()


def normalized_identity(classname: str, name: str) -> str:
    if not classname or not name:
        die("JUnit testcase is missing classname or name")
    return f"{normalize_path(Path(classname))}::{name}"


def parse_inventory(lines: list[str], source: str) -> list[str]:
    identities = [line.strip() for line in lines if line.strip()]
    if not identities:
        die(f"inventory is empty: {source}")
    duplicates = sorted(
        identity for identity in set(identities) if identities.count(identity) > 1
    )
    if duplicates:
        die(f"inventory contains duplicate identities: {source}: {duplicates[:5]}")
    malformed = [identity for identity in identities if "::" not in identity]
    if malformed:
        die(f"inventory contains malformed identities: {source}: {malformed[:5]}")
    return sorted(identities)


def read_inventory(path: Path) -> list[str]:
    try:
        return parse_inventory(path.read_text(encoding="utf-8").splitlines(), str(path))
    except FileNotFoundError:
        die(f"inventory is missing: {path}")


def assert_same_identities(expected: set[str], actual: set[str], context: str) -> None:
    if expected == actual:
        return
    missing = sorted(expected - actual)
    extra = sorted(actual - expected)
    die(
        f"{context} differs from the expected test identities: "
        f"missing={missing[:10]} extra={extra[:10]}"
    )


def list_inventory(compiler: Path, tests: Path) -> list[str]:
    fixtures = sorted(tests.glob("*.hew")) if tests.is_dir() else [tests]
    if not fixtures:
        die(f"test inventory selected no fixtures: {tests}")
    lines: list[str] = []
    for fixture in fixtures:
        result = subprocess.run(
            [str(compiler), "test", str(fixture), "--list", "--allow-empty"],
            cwd=REPO_ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if result.returncode:
            die(f"test listing failed for {fixture}: {result.stderr.strip()}")
        lines.extend(result.stdout.splitlines())
    return parse_inventory(lines, f"{compiler} test {tests} --list --allow-empty")


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
    counted = (len(outcomes), failures, skipped)
    declared_summary = (declared, declared_failures, declared_skipped)
    if declared_summary != counted:
        die(
            f"JUnit summary disagrees with testcase elements: {path}: "
            f"declared={declared_summary} counted={counted}"
        )
    return outcomes


def fixture_identities(inventory: list[str], fixture: Path) -> set[str]:
    prefix = f"{normalize_path(fixture)}::"
    return {identity for identity in inventory if identity.startswith(prefix)}


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    subcommands = parser.add_subparsers(dest="action", required=True)
    list_parser = subcommands.add_parser("list")
    list_parser.add_argument("--compiler", type=Path, required=True)
    list_parser.add_argument("--tests", type=Path, required=True)
    list_parser.add_argument("--expected", type=Path, default=DEFAULT_EXPECTED)
    list_parser.add_argument("--output", type=Path, required=True)
    record_parser = subcommands.add_parser("record")
    record_parser.add_argument("--compiler", type=Path, required=True)
    record_parser.add_argument("--tests", type=Path, required=True)
    record_parser.add_argument("--output", type=Path, default=DEFAULT_EXPECTED)
    report_parser = subcommands.add_parser("check-report")
    report_parser.add_argument("--inventory", type=Path, required=True)
    report_parser.add_argument("--fixture", type=Path, required=True)
    report_parser.add_argument("--report", type=Path, required=True)
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    if args.action == "list":
        actual = list_inventory(args.compiler.resolve(), args.tests.resolve())
        expected = read_inventory(args.expected.resolve())
        assert_same_identities(
            set(expected), set(actual), "live compiled-Hew inventory"
        )
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text("\n".join(actual) + "\n", encoding="utf-8")
    elif args.action == "record":
        actual = list_inventory(args.compiler.resolve(), args.tests.resolve())
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text("\n".join(actual) + "\n", encoding="utf-8")
    elif args.action == "check-report":
        inventory = read_inventory(args.inventory.resolve())
        expected = fixture_identities(inventory, args.fixture.resolve())
        if not expected:
            die(f"selected fixture has no expected tests: {args.fixture}")
        actual = parse_junit(args.report.resolve())
        assert_same_identities(
            expected, set(actual), f"JUnit report for {args.fixture}"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
