#!/usr/bin/env python3
"""Parse and validate JUnit emitted by ``hew test --format junit``.

This module is the schema authority shared by the corpus ratchet and compiled
Hew shard orchestration. As a command it prints one structured identity per
test, followed by the report totals::

    <outcome>\t<source-path>::<test-name>
    __SUMMARY__\t<total>\t<failures>\t<skipped>

``--runner-exit`` additionally verifies the CLI status contract: a completed
run exits 1 exactly when its report contains at least one failed test.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import sys

# stdlib xml.etree.ElementTree, not defusedxml: this parses output produced by
# the compiler binary the same build controls, rather than attacker-supplied
# XML. Project Python tooling is intentionally stdlib-only.
import xml.etree.ElementTree as ET


class JUnitError(ValueError):
    """A Hew JUnit document violates the producer's schema contract."""


@dataclass(frozen=True)
class JUnitTestCase:
    classname: str
    name: str
    outcome: str
    failure_message: str
    failure_text: str
    system_out: str


@dataclass(frozen=True)
class JUnitReport:
    testcases: tuple[JUnitTestCase, ...]
    tests: int
    failures: int
    skipped: int


def parse(path: Path) -> JUnitReport:
    """Read one complete Hew JUnit report and validate its declared totals."""
    try:
        document = path.read_text(encoding="utf-8")
    except OSError as error:
        raise JUnitError(f"cannot read JUnit report {path}: {error}") from error
    try:
        root = ET.fromstring(document)
    except ET.ParseError as error:
        raise JUnitError(f"malformed JUnit report {path}: {error}") from error
    if root.tag != "testsuites":
        raise JUnitError(
            f"JUnit report {path} has <{root.tag}> root, expected <testsuites>"
        )

    try:
        declared = int(root.get("tests", ""))
        declared_failures = int(root.get("failures", ""))
        declared_skipped = int(root.get("skipped", ""))
    except ValueError as error:
        raise JUnitError(
            f"JUnit report has non-integer summary attributes: {path}"
        ) from error

    testcases: list[JUnitTestCase] = []
    identities: set[tuple[str, str]] = set()
    failures = 0
    skipped = 0
    for element in root.iter("testcase"):
        classname = element.get("classname", "")
        name = element.get("name", "")
        if not classname or not name:
            raise JUnitError(f"JUnit testcase in {path} is missing classname or name")
        identity = (classname, name)
        if identity in identities:
            raise JUnitError(
                f"JUnit report contains duplicate testcase identity: "
                f"{path}: {classname}::{name}"
            )
        identities.add(identity)

        failure = element.find("failure")
        is_skipped = element.find("skipped") is not None
        if failure is not None:
            outcome = "FAILED"
            failures += 1
        elif is_skipped:
            outcome = "ignored"
            skipped += 1
        else:
            outcome = "ok"
        testcases.append(
            JUnitTestCase(
                classname=classname,
                name=name,
                outcome=outcome,
                failure_message=(
                    failure.get("message", "") if failure is not None else ""
                ),
                failure_text=(failure.text or "" if failure is not None else ""),
                system_out=element.findtext("system-out", default="") or "",
            )
        )

    counted = (len(testcases), failures, skipped)
    declared_totals = (declared, declared_failures, declared_skipped)
    if declared_totals != counted:
        raise JUnitError(
            f"JUnit summary disagrees with testcase elements: {path}: "
            f"declared={declared_totals} counted={counted}"
        )
    return JUnitReport(
        testcases=tuple(testcases),
        tests=declared,
        failures=declared_failures,
        skipped=declared_skipped,
    )


def testcase_identity(testcase: JUnitTestCase, root: Path) -> str:
    """Return a portable path-qualified identity for one testcase."""
    file = Path(testcase.classname)
    if file.is_absolute():
        try:
            file = file.resolve().relative_to(root.resolve())
        except ValueError:
            pass
    return f"{file.as_posix()}::{testcase.name}"


def expected_exit_code(report: JUnitReport) -> int:
    """Return the status promised by ``hew test`` for a completed report."""
    return int(report.failures > 0)


def render_outcomes(report: JUnitReport, root: Path) -> str:
    lines = [
        f"{testcase.outcome}\t{testcase_identity(testcase, root)}"
        for testcase in report.testcases
    ]
    lines.append(f"__SUMMARY__\t{report.tests}\t{report.failures}\t{report.skipped}")
    return "\n".join(lines) + "\n"


def usage() -> None:
    print(
        "usage: python3 scripts/lib/hew_junit.py <junit.xml>\n"
        "       python3 scripts/lib/hew_junit.py --runner-exit <status> <junit.xml>",
        file=sys.stderr,
    )


def main(argv: list[str]) -> int:
    runner_exit: int | None = None
    if len(argv) == 3 and argv[0] == "--runner-exit":
        try:
            runner_exit = int(argv[1])
        except ValueError:
            usage()
            return 2
        path = Path(argv[2])
    elif len(argv) == 1:
        path = Path(argv[0])
    else:
        usage()
        return 2

    try:
        report = parse(path)
        expected = expected_exit_code(report)
        if runner_exit is not None and runner_exit != expected:
            raise JUnitError(
                f"hew test exited {runner_exit}, but {path} requires status {expected} "
                f"from its outcomes"
            )
    except JUnitError as error:
        print(f"error: {error}", file=sys.stderr)
        return 1

    sys.stdout.write(render_outcomes(report, Path.cwd()))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
