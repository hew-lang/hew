#!/usr/bin/env python3
"""Behaviour tests for the Hew-suite JUnit transaction."""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import tempfile
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[2]
RATCHET = ROOT / "scripts" / "corpus-ratchet.sh"


def run(
    compiler: Path,
    fixtures: Path,
    expected: Path,
    report: Path,
    mode: str,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            "bash",
            str(RATCHET),
            "hew-suite",
            "--expected-failures",
            str(expected),
            "--junit-output",
            str(report),
        ],
        cwd=ROOT,
        env={
            **os.environ,
            "HEW_BIN": str(compiler),
            "HEW_TESTS_DIR": str(fixtures),
            "HEW_JUNIT_STUB_MODE": mode,
        },
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def make_fixture(work: Path) -> tuple[Path, Path]:
    fixtures = work / "fixtures"
    fixtures.mkdir()
    (fixtures / "sample.hew").write_text("#[test]\nfn sample() {}\n", encoding="utf-8")
    compiler = work / "hew"
    compiler.write_text(
        "#!/bin/sh\n"
        'case "$HEW_JUNIT_STUB_MODE" in\n'
        "  pass|status-mismatch)\n"
        '    printf \'%s\\n\' \'<testsuites tests="1" failures="0" skipped="0"><testsuite name="sample.hew" tests="1" failures="0" skipped="0"><testcase classname="sample.hew" name="sample"/></testsuite></testsuites>\'\n'
        '    [ "$HEW_JUNIT_STUB_MODE" = pass ] && exit 0\n'
        "    exit 1\n"
        "    ;;\n"
        "  failure)\n"
        '    printf \'%s\\n\' \'<testsuites tests="1" failures="1" skipped="0"><testsuite name="sample.hew" tests="1" failures="1" skipped="0"><testcase classname="sample.hew" name="sample"><failure message="assertion failed">diagnostic</failure></testcase></testsuite></testsuites>\'\n'
        "    exit 1\n"
        "    ;;\n"
        "esac\n"
        "exit 2\n",
        encoding="utf-8",
    )
    compiler.chmod(0o755)
    return fixtures, compiler


def test_valid_failure_report_and_status_one_reach_the_ratchet() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        fixtures, compiler = make_fixture(work)
        expected = work / "expected.txt"
        expected.write_text("sample.hew::sample\n", encoding="utf-8")
        report = work / "report.xml"

        result = run(compiler, fixtures, expected, report, "failure")

        assert result.returncode == 0, result.stdout + result.stderr
        root = ET.parse(report).getroot()
        assert root.get("failures") == "1"
        assert root.find(".//failure") is not None


def test_runner_status_must_agree_before_report_is_published() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        fixtures, compiler = make_fixture(work)
        expected = work / "expected.txt"
        expected.write_text("", encoding="utf-8")
        report = work / "report.xml"
        previous = b"previous report\n"
        report.write_bytes(previous)

        result = run(compiler, fixtures, expected, report, "status-mismatch")

        assert result.returncode != 0
        assert report.read_bytes() == previous
        preserved = list(work.glob("report.xml.invalid.*"))
        assert len(preserved) == 1
        assert ET.parse(preserved[0]).getroot().get("failures") == "0"


if __name__ == "__main__":
    test_valid_failure_report_and_status_one_reach_the_ratchet()
    test_runner_status_must_agree_before_report_is_published()
    print("PASS: Hew-suite runner accepts complete failures and publishes atomically")
