#!/usr/bin/env python3
"""The Hew suite cache reuses only source/compiler-identical results."""

from __future__ import annotations

import os
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RATCHET = ROOT / "scripts/hew-suite-ratchet.sh"


def run(env: dict[str, str], expected: Path, report: Path) -> None:
    result = subprocess.run(
        [
            "bash",
            str(RATCHET),
            "--expected-failures",
            str(expected),
            "--junit-output",
            str(report),
        ],
        cwd=ROOT,
        env={**os.environ, **env},
        text=True,
        capture_output=True,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def test_cache_key_tracks_fixture_and_compiler_content() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        fixtures = work / "fixtures"
        fixtures.mkdir()
        fixture = fixtures / "sample.hew"
        fixture.write_text("test sample {}\n")
        expected = work / "expected.txt"
        expected.write_text("")
        counter = work / "counter"
        compiler = work / "hew"
        compiler.write_text(
            "#!/bin/sh\n"
            f"printf x >> '{counter}'\n"
            'printf \'%s\\n\' \'<testsuites tests="1" failures="0" skipped="0"><testsuite><testcase name="sample"/></testsuite></testsuites>\'\n'
        )
        compiler.chmod(0o755)
        env = {
            "HEW_BIN": str(compiler),
            "HEW_TESTS_DIR": str(fixtures),
            "HEW_TEST_CACHE_DIR": str(work / "cache"),
        }

        run(env, expected, work / "report.xml")
        run(env, expected, work / "report.xml")
        assert counter.read_text() == "x"

        fixture.write_text("test sample { assert true; }\n")
        run(env, expected, work / "report.xml")
        assert counter.read_text() == "xx"

        compiler.write_text(compiler.read_text() + "# rebuilt\n")
        run(env, expected, work / "report.xml")
        assert counter.read_text() == "xxx"


if __name__ == "__main__":
    test_cache_key_tracks_fixture_and_compiler_content()
    print("PASS: Hew suite cache reuses only identical source/compiler results")
