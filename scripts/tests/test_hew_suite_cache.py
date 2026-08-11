#!/usr/bin/env python3
"""Counterfactuals for compiled-Hew inventory, verdicts, and evidence."""

from __future__ import annotations

import os
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RATCHET = ROOT / "scripts/hew-suite-ratchet.sh"
FREEBSD_WORKFLOW = ROOT / ".github/workflows/freebsd.yml"
RELEASE_WORKFLOW = ROOT / ".github/workflows/release-gate.yml"


def run(
    env: dict[str, str], report: Path, expected_returncode: int = 0
) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(
        ["bash", str(RATCHET), "--junit-output", str(report)],
        cwd=ROOT,
        env={**os.environ, **env},
        text=True,
        capture_output=True,
    )
    assert result.returncode == expected_returncode, result.stdout + result.stderr
    return result


def fixture_env(work: Path, failing: bool = False) -> tuple[dict[str, str], Path, Path]:
    fixtures = work / "fixtures"
    fixtures.mkdir()
    first = fixtures / "first.hew"
    second = fixtures / "second.hew"
    first.write_text("test first {}\n", encoding="utf-8")
    second.write_text("test second {}\n", encoding="utf-8")
    inventory = work / "expected.txt"
    inventory.write_text(
        f"{first.resolve().as_posix()}::first\n{second.resolve().as_posix()}::second\n",
        encoding="utf-8",
    )
    counter = work / "counter"
    compiler = work / "hew"
    failure_element = (
        '<failure message="forced assertion">expected 1, got 2</failure>'
        if failing
        else ""
    )
    returncode = "exit 1" if failing else "exit 0"
    compiler.write_text(
        "#!/bin/sh\n"
        'if [ "$3" = "--list" ]; then\n'
        f"  printf '%s\\n' '{first.resolve().as_posix()}::first' '{second.resolve().as_posix()}::second'\n"
        "  exit 0\n"
        "fi\n"
        f"basename \"$2\" >> '{counter}'\n"
        'name=$(basename "$2" .hew)\n'
        f'printf \'<testsuites tests="1" failures="{1 if failing else 0}" skipped="0">\'\n'
        f'printf \'<testsuite><testcase classname="%s" name="%s">{failure_element}</testcase></testsuite></testsuites>\\n\' "$2" "$name"\n'
        f"{returncode}\n",
        encoding="utf-8",
    )
    compiler.chmod(0o755)
    (work / "libhew.a").write_bytes(b"archive")
    env = {
        "HEW_BIN": str(compiler),
        "HEW_TESTS_DIR": str(fixtures),
        "HEW_EXPECTED_TESTS_FILE": str(inventory),
        "HEW_TEST_CACHE_DIR": str(work / "cache"),
    }
    return env, counter, inventory


def test_one_fixture_edit_invalidates_only_that_fixture() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        env, counter, _ = fixture_env(work)
        report = work / "report.xml"

        first = run(env, report)
        assert "Cache: 0 hit fixture(s), 2 re-run fixture(s)." in first.stdout
        warm = run(env, report)
        assert "Cache: 2 hit fixture(s), 0 re-run fixture(s)." in warm.stdout
        assert counter.read_text(encoding="utf-8").splitlines() == [
            "first.hew",
            "second.hew",
        ]

        (work / "fixtures/first.hew").write_text(
            "test first {}\n// one-fixture mutation\n", encoding="utf-8"
        )
        changed = run(env, report)
        assert "Cache: 1 hit fixture(s), 1 re-run fixture(s)." in changed.stdout
        assert counter.read_text(encoding="utf-8").splitlines() == [
            "first.hew",
            "second.hew",
            "first.hew",
        ]


def test_cache_key_tracks_compiler_archive_and_semantic_environment() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        env, counter, _ = fixture_env(work)
        report = work / "report.xml"
        compiler = Path(env["HEW_BIN"])

        run(env, report)
        run(env, report)
        assert len(counter.read_text(encoding="utf-8").splitlines()) == 2

        compiler.write_text(compiler.read_text(encoding="utf-8") + "# rebuilt\n")
        run(env, report)
        assert len(counter.read_text(encoding="utf-8").splitlines()) == 4

        (work / "libhew.a").write_bytes(b"archive-v2")
        run(env, report)
        assert len(counter.read_text(encoding="utf-8").splitlines()) == 6

        run({**env, "HEW_TEST_SEED": "different"}, report)
        assert len(counter.read_text(encoding="utf-8").splitlines()) == 8


def test_missing_live_identity_fails_before_running_fixtures() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        env, counter, inventory = fixture_env(work)
        compiler = Path(env["HEW_BIN"])
        compiler.write_text(
            compiler.read_text(encoding="utf-8").replace(
                f" '{(work / 'fixtures/second.hew').resolve().as_posix()}::second'",
                "",
            ),
            encoding="utf-8",
        )
        result = run(env, work / "report.xml", expected_returncode=1)
        assert "differs from the expected test identities" in result.stderr
        assert "second.hew::second" in result.stderr
        assert inventory.is_file()
        assert not counter.exists()


def test_unsharded_ci_restores_the_fixture_cache() -> None:
    freebsd = FREEBSD_WORKFLOW.read_text(encoding="utf-8")
    release = RELEASE_WORKFLOW.read_text(encoding="utf-8")
    assert freebsd.count("path: target/hew-test-cache") == 1
    assert "hew-suite-v3-freebsd-x86_64-" in freebsd
    assert release.count("path: target/hew-test-cache") == 2
    assert "hew-suite-v3-linux-x86_64-" in release
    assert "hew-suite-v3-freebsd-x86_64-" in release


if __name__ == "__main__":
    test_one_fixture_edit_invalidates_only_that_fixture()
    test_cache_key_tracks_compiler_archive_and_semantic_environment()
    test_missing_live_identity_fails_before_running_fixtures()
    test_unsharded_ci_restores_the_fixture_cache()
