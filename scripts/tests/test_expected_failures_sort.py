#!/usr/bin/env python3
"""Behaviour tests for the expected-failures ledger sort rule.

`corpus-ratchet.sh`'s shared `read_expected_failures` refuses an
expected-failures file that is not sorted by its id column, naming the first
out-of-order row. The rule resets at each "# ── ... ──" section header, so a
file organized into sections is sorted within each section rather than across
the whole file. This mirrors test_hew_suite_runner.py's stub-compiler
approach to exercise the shared ratchet core without a built `hew` binary.
"""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parents[2]
RATCHET = ROOT / "scripts" / "corpus-ratchet.sh"


def make_stub_compiler(work: Path) -> Path:
    fixtures = work / "fixtures"
    fixtures.mkdir()
    (fixtures / "sample.hew").write_text("#[test]\nfn sample() {}\n", encoding="utf-8")
    compiler = work / "hew"
    compiler.write_text(
        "#!/bin/sh\n"
        'printf \'%s\\n\' \'<testsuites tests="1" failures="0" skipped="0">'
        '<testsuite name="sample.hew" tests="1" failures="0" skipped="0">'
        '<testcase classname="sample.hew" name="sample"/></testsuite></testsuites>\'\n'
        "exit 0\n",
        encoding="utf-8",
    )
    compiler.chmod(0o755)
    return compiler


def run_hew_suite(work: Path, expected: Path) -> subprocess.CompletedProcess[str]:
    compiler = make_stub_compiler(work)
    report = work / "report.xml"
    command = [
        "bash",
        str(RATCHET),
        "hew-suite",
        "--expected-failures",
        str(expected),
        "--junit-output",
        str(report),
    ]
    environment = {
        **os.environ,
        "HEW_BIN": str(compiler),
        "HEW_TESTS_DIR": str(work / "fixtures"),
        "RATCHET_STRICT_RECOVERIES": "0",
    }
    return subprocess.run(
        command,
        cwd=ROOT,
        env=environment,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def test_out_of_order_rows_are_refused_with_the_first_bad_row_named() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        expected = work / "expected.txt"
        expected.write_text(
            "sample.hew::zzz compile\nsample.hew::aaa compile\n", encoding="utf-8"
        )
        result = run_hew_suite(work, expected)
        assert result.returncode != 0, result.stdout + result.stderr
        assert "not sorted by id" in result.stderr, result.stderr
        assert "sample.hew::aaa" in result.stderr, result.stderr


def test_sorted_rows_pass_the_ledger_parse() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        expected = work / "expected.txt"
        expected.write_text(
            "sample.hew::aaa compile\nsample.hew::zzz compile\n", encoding="utf-8"
        )
        result = run_hew_suite(work, expected)
        # Neither listed identity exists in this run's report, so the ratchet
        # itself still fails (inventory drift) — the point here is only that
        # sort validation does not reject a sorted file.
        assert "not sorted by id" not in result.stderr, result.stderr


def test_sort_order_resets_at_a_section_header() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        expected = work / "expected.txt"
        expected.write_text(
            "# ── zzz section ──\n"
            "sample.hew::zzz compile\n"
            "# ── aaa section ──\n"
            "sample.hew::aaa compile\n",
            encoding="utf-8",
        )
        result = run_hew_suite(work, expected)
        assert "not sorted by id" not in result.stderr, result.stderr


if __name__ == "__main__":
    test_out_of_order_rows_are_refused_with_the_first_bad_row_named()
    test_sorted_rows_pass_the_ledger_parse()
    test_sort_order_resets_at_a_section_header()
    print("PASS: expected-failures ledger sort rule refuses an unsorted file")
