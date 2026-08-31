#!/usr/bin/env python3
"""Run and aggregate stable partitions of the compiled-Hew test suites."""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
from typing import NoReturn
import xml.etree.ElementTree as ET


REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO_ROOT / "scripts" / "lib"))
from corpus_nonempty import assert_nonempty  # noqa: E402
from hew_junit import (  # noqa: E402
    FAILURE_KINDS,
    JUnitError,
    JUnitReport,
    parse as parse_hew_junit,
)


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
        report = parse_hew_junit(path, REPO_ROOT)
    except JUnitError as error:
        die(str(error))
    outcomes: dict[str, str] = {}
    for testcase in report.testcases:
        identity = normalized_identity(testcase.classname, testcase.name)
        if identity in outcomes:
            die(
                f"JUnit report contains duplicate testcase identity: {path}: {identity}"
            )
        outcomes[identity] = testcase.failure_kind or testcase.outcome
    return outcomes


def report_failures(reports_dir: Path, shard_count: int) -> None:
    """Print every JUnit failure before an aggregate gate exits."""
    if shard_count < 1:
        die("shard count must be at least one")

    for shard in range(1, shard_count + 1):
        for label in ("o0", "o2"):
            path = reports_dir / f"hew-{label}-shard-{shard}.xml"
            if not path.is_file():
                print(
                    f"COMPILED_HEW_REPORT_MISSING shard={shard} suite={label.upper()} "
                    f"path={path}",
                    file=sys.stderr,
                )
                continue

            try:
                report = parse_hew_junit(path, REPO_ROOT)
            except JUnitError as error:
                # This is the diagnostic pass that runs before the aggregate
                # gate fails the job. Dying here would hide every other
                # shard's assertions behind one truncated report, so name the
                # unreadable report and keep going; the aggregate gate still
                # fails the job on the same malformed input.
                print(
                    f"COMPILED_HEW_REPORT_UNREADABLE shard={shard} "
                    f"suite={label.upper()} path={path}: {error}",
                    file=sys.stderr,
                )
                continue
            for testcase in report.testcases:
                if testcase.outcome != "FAILED":
                    continue
                identity = normalized_identity(testcase.classname, testcase.name)
                diagnostic_parts = [
                    part.strip()
                    for part in (testcase.failure_message, testcase.failure_text)
                    if part and part.strip()
                ]
                system_out = testcase.system_out.strip()
                if system_out:
                    diagnostic_parts.append(f"output:\n{system_out}")
                diagnostic = (
                    "\n".join(diagnostic_parts) or "JUnit failure without a diagnostic"
                )
                print(
                    f"COMPILED_HEW_FAILURE shard={shard} suite={label.upper()} "
                    f"test={identity} kind={testcase.failure_kind}\n"
                    f"assertion:\n{diagnostic}"
                )


def update_junit_totals(root: ET.Element) -> None:
    root.set("tests", str(sum(1 for _ in root.iter("testcase"))))
    root.set("failures", str(sum(1 for _ in root.iter("failure"))))
    root.set("skipped", str(sum(1 for _ in root.iter("skipped"))))
    for suite in root.iter("testsuite"):
        suite.set("tests", str(sum(1 for _ in suite.iter("testcase"))))
        suite.set("failures", str(sum(1 for _ in suite.iter("failure"))))
        suite.set("skipped", str(sum(1 for _ in suite.iter("skipped"))))


def write_finalization_failure(output_dir: Path, message: str) -> None:
    root = ET.Element("testsuites", tests="1", failures="1", skipped="0")
    suite = ET.SubElement(
        root,
        "testsuite",
        name="compiled-hew-finalization",
        tests="1",
        failures="1",
        skipped="0",
    )
    testcase = ET.SubElement(
        suite,
        "testcase",
        classname="compiled-hew-finalization",
        name="report-authority",
    )
    failure = ET.SubElement(testcase, "failure", type="launch", message=message)
    failure.text = message
    ET.ElementTree(root).write(
        output_dir / "compiled-hew-finalization.xml",
        encoding="unicode",
        xml_declaration=True,
    )


def finalize_reports(
    reports_dir: Path,
    output_dir: Path,
    full_inventory: Path,
    expected_failures_path: Path,
    prerequisites_succeeded: bool,
) -> int:
    """Prepare the JUnit input for GitHub after raw aggregate gates finish."""
    output_dir.mkdir(parents=True, exist_ok=True)
    reports = sorted(reports_dir.glob("hew-*-shard-*.xml"))
    for report in reports:
        shutil.copy2(report, output_dir / report.name)

    if not prerequisites_succeeded:
        write_finalization_failure(
            output_dir,
            "compiled-Hew raw ratchet or differential gate did not succeed",
        )
        return 1

    try:
        full = set(read_inventory(full_inventory))
        expected = expected_failures(expected_failures_path, full)
        actual: dict[str, str] = {}
        parsed_reports: list[tuple[Path, JUnitReport]] = []
        for report in reports:
            parsed = parse_hew_junit(report, REPO_ROOT)
            parsed_reports.append((report, parsed))
            if "hew-o0-" not in report.name:
                continue
            for testcase in parsed.testcases:
                identity = normalized_identity(testcase.classname, testcase.name)
                if identity in actual:
                    die(f"finalization report has duplicate identity: {identity}")
                if testcase.failure_kind is not None:
                    actual[identity] = testcase.failure_kind
        if set(actual) != set(expected):
            die("finalization failure set differs from the ratchet")
        for identity, expected_kind in expected.items():
            if actual[identity] != expected_kind:
                die(f"finalization failure kind differs from the ratchet: {identity}")

        for source, parsed in parsed_reports:
            root = ET.parse(source).getroot()
            for testcase in root.iter("testcase"):
                identity = normalized_identity(
                    testcase.get("classname", ""), testcase.get("name", "")
                )
                if identity not in expected:
                    continue
                failure = testcase.find("failure")
                if failure is None or failure.get("type") != expected[identity]:
                    die(f"finalization cannot normalize expected failure: {identity}")
                message = failure.get("message", "")
                text = failure.text or ""
                testcase.remove(failure)
                skipped = ET.SubElement(testcase, "skipped")
                skipped.set(
                    "message",
                    f"expected {expected[identity]} failure"
                    + (f": {message}" if message else ""),
                )
                skipped.text = text
            update_junit_totals(root)
            ET.ElementTree(root).write(
                output_dir / source.name,
                encoding="unicode",
                xml_declaration=True,
            )
    except (JUnitError, SystemExit, ET.ParseError) as error:
        message = (
            "compiled-Hew report finalization rejected its ratchet inputs; "
            "see workflow stderr"
            if isinstance(error, SystemExit)
            else str(error)
        )
        write_finalization_failure(output_dir, message)
        print(f"compiled-hew-shards: finalization failed: {message}", file=sys.stderr)
        return 1
    return 0


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
        expected_returncode = int(
            any(value in FAILURE_KINDS for value in outcomes.values())
        )
        if returncode != expected_returncode:
            die(
                f"{label.upper()} runner exited {returncode}, expected {expected_returncode} "
                "from its JUnit outcomes"
            )


def expected_failures(path: Path, full: set[str]) -> dict[str, str]:
    expected: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        value = line.split("#", 1)[0].strip()
        if not value:
            continue
        fields = value.split()
        if len(fields) != 2:
            die(
                f"expected-failure entry must be '<identity> <failure-kind>': "
                f"{path}: {value!r}"
            )
        identity, failure_kind = fields
        if failure_kind not in FAILURE_KINDS:
            die(
                f"expected-failure entry has unsupported failure kind "
                f"{failure_kind!r}: {path}: {identity}"
            )
        if identity in expected:
            die(f"expected-failures file contains duplicate identities: {path}")
        expected[identity] = failure_kind
    unknown = set(expected) - full
    if unknown:
        die(
            f"expected failures are absent from the full inventory: {sorted(unknown)[:5]}"
        )
    return expected


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
    label = "hew-suite-tests" if mode == "ratchet" else "o2-differential-outcomes"
    assert_nonempty(label, len(full), context="union of compiled-Hew shards")
    if mode == "ratchet":
        expected = expected_failures(expected_failures_path, full)
        actual = {
            identity: outcome
            for identity, outcome in o0.items()
            if outcome in FAILURE_KINDS
        }
        if set(actual) != set(expected):
            die(
                "O0 shard failure set differs from the ratchet: "
                f"unexpected={sorted(set(actual) - set(expected))[:5]} "
                f"now_passing={sorted(set(expected) - set(actual))[:5]}"
            )
        kind_differences = [
            f"{identity}: expected={expected[identity]} actual={actual[identity]}"
            for identity in sorted(expected)
            if actual[identity] != expected[identity]
        ]
        if kind_differences:
            die(
                "O0 shard failure kinds differ from the ratchet: "
                + "; ".join(kind_differences[:5])
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
    report_parser = subcommands.add_parser("report")
    report_parser.add_argument("--reports-dir", type=Path, required=True)
    report_parser.add_argument("--shard-count", type=int, required=True)
    finalize_parser = subcommands.add_parser("finalize")
    finalize_parser.add_argument("--reports-dir", type=Path, required=True)
    finalize_parser.add_argument("--output-dir", type=Path, required=True)
    finalize_parser.add_argument("--full-inventory", type=Path, required=True)
    finalize_parser.add_argument(
        "--expected-failures",
        type=Path,
        default=REPO_ROOT / "scripts" / "hew-suite-expected-failures.txt",
    )
    finalize_parser.add_argument(
        "--prerequisites-succeeded",
        choices=("true", "false"),
        required=True,
    )
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    if args.action == "run":
        run_shard(args.compiler.resolve(), args.partition, args.output_dir)
    elif args.action == "aggregate":
        aggregate(
            args.mode,
            args.reports_dir,
            args.full_inventory,
            args.shard_count,
            args.expected_failures,
        )
    elif args.action == "finalize":
        return finalize_reports(
            args.reports_dir,
            args.output_dir,
            args.full_inventory,
            args.expected_failures,
            args.prerequisites_succeeded == "true",
        )
    else:
        report_failures(args.reports_dir, args.shard_count)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
