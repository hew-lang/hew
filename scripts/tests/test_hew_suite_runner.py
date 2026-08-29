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
    *,
    emit_o0_outcomes: Path | None = None,
    ambient_opt_level: str | None = None,
) -> subprocess.CompletedProcess[str]:
    command = [
        "bash",
        str(RATCHET),
        "hew-suite",
        "--expected-failures",
        str(expected),
        "--junit-output",
        str(report),
    ]
    if emit_o0_outcomes is not None:
        command.extend(["--emit-o0-outcomes", str(emit_o0_outcomes)])
    environment = {
        **os.environ,
        "HEW_BIN": str(compiler),
        "HEW_TESTS_DIR": str(fixtures),
        "HEW_JUNIT_STUB_MODE": mode,
    }
    if ambient_opt_level is not None:
        environment["HEW_OPT_LEVEL"] = ambient_opt_level
    return subprocess.run(
        command,
        cwd=ROOT,
        env=environment,
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
        '    printf \'%s\\n\' \'<testsuites tests="1" failures="1" skipped="0"><testsuite name="sample.hew" tests="1" failures="1" skipped="0"><testcase classname="sample.hew" name="sample"><failure type="compile" message="assertion failed">diagnostic</failure></testcase></testsuite></testsuites>\'\n'
        "    exit 1\n"
        "    ;;\n"
        "  failure-kind-drift-runtime|failure-kind-drift-timeout|failure-kind-drift-launch)\n"
        '    kind="${HEW_JUNIT_STUB_MODE#failure-kind-drift-}"\n'
        "    cat <<XML\n"
        '<testsuites tests="1" failures="1" skipped="0"><testsuite name="sample.hew" tests="1" failures="1" skipped="0"><testcase classname="sample.hew" name="sample"><failure type="$kind" message="assertion failed">diagnostic</failure></testcase></testsuite></testsuites>\n'
        "XML\n"
        "    exit 1\n"
        "    ;;\n"
        "  identity-alias-dot|identity-alias-absolute)\n"
        '    alias="./sample.hew"\n'
        '    [ "$HEW_JUNIT_STUB_MODE" = identity-alias-absolute ] && alias="$PWD/sample.hew"\n'
        "    cat <<XML\n"
        '<testsuites tests="2" failures="1" skipped="0"><testsuite name="sample.hew" tests="2" failures="1" skipped="0"><testcase classname="$alias" name="sample"/><testcase classname="sample.hew" name="sample"><failure type="runtime" message="same identity">same identity</failure></testcase></testsuite></testsuites>\n'
        "XML\n"
        "    exit 1\n"
        "    ;;\n"
        "  opt-level)\n"
        '    name="opt-${HEW_OPT_LEVEL:-unset}"\n'
        "    cat <<XML\n"
        '<testsuites tests="1" failures="0" skipped="0"><testsuite name="sample.hew" tests="1" failures="0" skipped="0"><testcase classname="sample.hew" name="$name"/></testsuite></testsuites>\n'
        "XML\n"
        "    exit 0\n"
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
        expected.write_text("sample.hew::sample compile\n", encoding="utf-8")
        report = work / "report.xml"

        result = run(compiler, fixtures, expected, report, "failure")

        assert result.returncode == 0, result.stdout + result.stderr
        root = ET.parse(report).getroot()
        assert root.get("failures") == "1"
        failure = root.find(".//failure")
        assert failure is not None
        assert failure.get("type") == "compile"


def test_failure_kind_drift_fails_the_ratchet_without_matching_text() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        fixtures, compiler = make_fixture(work)
        expected = work / "expected.txt"
        expected.write_text("sample.hew::sample compile\n", encoding="utf-8")
        report = work / "report.xml"

        for actual_kind in ("runtime", "timeout", "launch"):
            result = run(
                compiler,
                fixtures,
                expected,
                report,
                f"failure-kind-drift-{actual_kind}",
            )

            assert result.returncode != 0
            assert f"expected=compile actual={actual_kind}" in result.stdout, (
                result.stdout + result.stderr
            )


def test_duplicate_failure_identities_are_rejected() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        fixtures, compiler = make_fixture(work)
        report = work / "report.xml"

        for duplicate_kind in ("compile", "runtime"):
            expected = work / f"expected-{duplicate_kind}.txt"
            expected.write_text(
                f"sample.hew::sample compile\nsample.hew::sample {duplicate_kind}\n",
                encoding="utf-8",
            )
            result = run(compiler, fixtures, expected, report, "failure")

            assert result.returncode != 0
            assert (
                "expected-failures file contains duplicate identity: "
                "sample.hew::sample" in result.stderr
            ), result.stdout + result.stderr


def test_pass_and_failure_path_aliases_are_rejected() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        fixtures, compiler = make_fixture(work)
        expected = work / "expected.txt"
        expected.write_text("", encoding="utf-8")
        report = work / "report.xml"

        for mode in ("identity-alias-dot", "identity-alias-absolute"):
            result = run(compiler, fixtures, expected, report, mode)

            assert result.returncode != 0
            assert (
                "JUnit report contains duplicate testcase identity" in result.stderr
            ), result.stdout + result.stderr


def test_o0_handoff_ignores_ambient_optimization_level() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        fixtures, compiler = make_fixture(work)
        expected = work / "expected.txt"
        expected.write_text("", encoding="utf-8")
        report = work / "report.xml"
        outcomes = work / "o0-outcomes.txt"

        result = run(
            compiler,
            fixtures,
            expected,
            report,
            "opt-level",
            emit_o0_outcomes=outcomes,
            ambient_opt_level="2",
        )

        assert result.returncode == 0, result.stdout + result.stderr
        rendered = outcomes.read_text(encoding="utf-8")
        assert "sample.hew::opt-0" in rendered
        assert "opt-2" not in rendered


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
    test_failure_kind_drift_fails_the_ratchet_without_matching_text()
    test_duplicate_failure_identities_are_rejected()
    test_pass_and_failure_path_aliases_are_rejected()
    test_o0_handoff_ignores_ambient_optimization_level()
    test_runner_status_must_agree_before_report_is_published()
    print("PASS: Hew-suite runner accepts complete failures and publishes atomically")
