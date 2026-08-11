#!/usr/bin/env python3
"""The Hew suite cache reuses only source/compiler-identical results."""

from __future__ import annotations

import os
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RATCHET = ROOT / "scripts/hew-suite-ratchet.sh"


def run(env: dict[str, str], report: Path) -> None:
    result = subprocess.run(
        [
            "bash",
            str(RATCHET),
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
        empty_fixture = fixtures / "no_tests.hew"
        empty_fixture.write_text("fn main() {}\n")
        counter = work / "counter"
        empty_counter = work / "empty-counter"
        compiler = work / "hew"
        compiler.write_text(
            "#!/bin/sh\n"
            f"if grep -q 'fn main' \"$2\"; then printf e >> '{empty_counter}'; echo 'No test functions found.' >&2; exit 0; fi\n"
            f"printf x >> '{counter}'\n"
            'printf \'%s\\n\' \'<testsuites tests="1" failures="0" skipped="0"><testsuite><testcase name="sample"/></testsuite></testsuites>\'\n'
        )
        compiler.chmod(0o755)
        (work / "libhew.a").write_bytes(b"archive")
        env = {
            "HEW_BIN": str(compiler),
            "HEW_TESTS_DIR": str(fixtures),
            "HEW_TEST_CACHE_DIR": str(work / "cache"),
        }

        run(env, work / "report.xml")
        run(env, work / "report.xml")
        assert counter.read_text() == "x"
        assert empty_counter.read_text() == "e"

        empty_fixture.write_text("fn main() { let changed = true; }\n")
        run(env, work / "report.xml")
        assert counter.read_text() == "xx"
        assert empty_counter.read_text() == "ee"

        fixture.write_text("test sample { assert true; }\n")
        run(env, work / "report.xml")
        assert counter.read_text() == "xxx"
        assert empty_counter.read_text() == "eee"

        compiler.write_text(compiler.read_text() + "# rebuilt\n")
        run(env, work / "report.xml")
        assert counter.read_text() == "xxxx"
        assert empty_counter.read_text() == "eeee"

        (work / "libhew.a").write_bytes(b"archive-v2")
        run(env, work / "report.xml")
        assert counter.read_text() == "xxxxx"
        assert empty_counter.read_text() == "eeeee"

        run({**env, "HEW_TEST_SEED": "different"}, work / "report.xml")
        assert counter.read_text() == "xxxxxx"
        assert empty_counter.read_text() == "eeeeee"
