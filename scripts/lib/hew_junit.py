#!/usr/bin/env python3
"""Parse and validate JUnit emitted by ``hew test --format junit``.

This module is the schema authority shared by the corpus ratchet and compiled
Hew shard orchestration. As a command it prints one structured identity per
test, followed by the report totals::

    <outcome>\t<source-path>::<test-name>[\t<failure-kind>]
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


FAILURE_KINDS = frozenset({"compile", "runtime", "timeout", "launch"})


class JUnitError(ValueError):
    """A Hew JUnit document violates the producer's schema contract."""


@dataclass(frozen=True)
class JUnitTestCase:
    classname: str
    name: str
    outcome: str
    failure_kind: str | None
    failure_message: str
    failure_text: str
    system_out: str


@dataclass(frozen=True)
class JUnitReport:
    testcases: tuple[JUnitTestCase, ...]
    tests: int
    failures: int
    skipped: int


def canonical_classname(classname: str, root: Path) -> str:
    """Normalize a testcase source path against its invocation root."""
    canonical_root = root.resolve()
    source = Path(classname)
    if not source.is_absolute():
        source = canonical_root / source
    source = source.resolve()
    try:
        source = source.relative_to(canonical_root)
    except ValueError:
        pass
    return source.as_posix()


def parse(path: Path, identity_root: Path | None = None) -> JUnitReport:
    """Read one complete Hew JUnit report and validate its declared totals."""
    identity_root = identity_root or Path.cwd()
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
        raw_classname = element.get("classname", "")
        name = element.get("name", "")
        if not raw_classname or not name:
            raise JUnitError(f"JUnit testcase in {path} is missing classname or name")
        classname = canonical_classname(raw_classname, identity_root)
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
            failure_kind = failure.get("type", "")
            if failure_kind not in FAILURE_KINDS:
                expected = ", ".join(sorted(FAILURE_KINDS))
                raise JUnitError(
                    f"JUnit failure has missing or unsupported semantic type: "
                    f"{path}: {classname}::{name}: {failure_kind!r}; "
                    f"expected one of {expected}"
                )
            failures += 1
        elif is_skipped:
            outcome = "ignored"
            failure_kind = None
            skipped += 1
        else:
            outcome = "ok"
            failure_kind = None
        testcases.append(
            JUnitTestCase(
                classname=classname,
                name=name,
                outcome=outcome,
                failure_kind=failure_kind,
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
    classname = canonical_classname(testcase.classname, root)
    return f"{classname}::{testcase.name}"


def expected_exit_code(report: JUnitReport) -> int:
    """Return the status promised by ``hew test`` for a completed report."""
    return int(report.failures > 0)


def render_outcomes(report: JUnitReport, root: Path) -> str:
    lines = []
    for testcase in report.testcases:
        line = f"{testcase.outcome}\t{testcase_identity(testcase, root)}"
        if testcase.failure_kind is not None:
            line += f"\t{testcase.failure_kind}"
        lines.append(line)
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
        identity_root = Path.cwd()
        report = parse(path, identity_root)
        expected = expected_exit_code(report)
        if runner_exit is not None and runner_exit != expected:
            raise JUnitError(
                f"hew test exited {runner_exit}, but {path} requires status {expected} "
                f"from its outcomes"
            )
    except JUnitError as error:
        print(f"error: {error}", file=sys.stderr)
        return 1

    sys.stdout.write(render_outcomes(report, identity_root))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
