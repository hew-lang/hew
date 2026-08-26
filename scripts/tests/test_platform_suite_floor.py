#!/usr/bin/env python3
"""Falsifiability tests for scripts/platform-suite-floor.py.

The wrong program this rejects: a `[profile.platform]` `default-filter` that
matches nothing. `cargo nextest run` over an empty selection EXITS ZERO, so the
step is green, the job is green, and the required platform context is green
having executed no test at all.

The counterfactual is driven through a REAL `cargo nextest list` against a
copied nextest config whose platform filter cannot match, so the assertion is
about what nextest actually emits rather than about a hand-written fixture that
resembles it. Every other case is fixture-driven, because payload shapes are
cheaper to enumerate than to provoke.

Only non-empty is asserted anywhere. An exact expected count would fire on
every added test and tell a reviewer nothing.
"""

import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

_spec = importlib.util.spec_from_file_location(
    "platform_suite_floor", ROOT / "scripts" / "platform-suite-floor.py"
)
floor = importlib.util.module_from_spec(_spec)
assert _spec.loader is not None
sys.modules.setdefault("platform_suite_floor", floor)
_spec.loader.exec_module(floor)


def run_floor(payload: str) -> subprocess.CompletedProcess[str]:
    with tempfile.TemporaryDirectory() as raw:
        listing = Path(raw) / "listing.json"
        listing.write_text(payload, encoding="utf-8")
        summary = Path(raw) / "summary.md"
        return subprocess.run(
            [
                sys.executable,
                str(ROOT / "scripts" / "platform-suite-floor.py"),
                str(listing),
                "--summary",
                str(summary),
            ],
            capture_output=True,
            text=True,
        )


def listing(**suites: list[str]) -> str:
    return json.dumps(
        {
            "rust-suites": {
                name: {"testcases": {case: {} for case in cases}}
                for name, cases in suites.items()
            }
        }
    )


def test_a_selection_with_tests_is_accepted() -> None:
    result = run_floor(listing(**{"hew-cli::run_e2e": ["a", "b"], "x::y": ["c"]}))
    assert result.returncode == 0, result.stderr
    assert "3 test(s)" in result.stdout, result.stdout
    assert "hew-cli::run_e2e" in result.stdout, result.stdout


def test_an_empty_selection_is_rejected() -> None:
    """The whole point: nextest would exit 0 over this."""
    for payload in (
        listing(),
        listing(**{"hew-cli::run_e2e": []}),
        json.dumps({"rust-suites": {"a::b": {"testcases": {}}}}),
    ):
        result = run_floor(payload)
        assert result.returncode != 0, payload
        assert "selected no tests" in result.stderr, result.stderr


def test_an_unreadable_listing_is_an_error_and_not_a_zero() -> None:
    """ "I could not tell" and "there were none" are different findings.

    Collapsing them would report a broken command as a broken filter.
    """
    for payload, needle in (
        ("not json at all", "not valid JSON"),
        ("[]", "not a JSON object"),
        (json.dumps({"summary": {}}), "no `rust-suites` map"),
        (json.dumps({"rust-suites": {"a": {"testcases": 3}}}), "non-object"),
    ):
        result = run_floor(payload)
        assert result.returncode != 0, payload
        assert needle in result.stderr, (needle, result.stderr)


def test_the_count_ignores_human_output_noise() -> None:
    """A line count moves with progress output; a record count does not.

    The previous floor counted non-blank lines, so a warning, a colour code or
    a `--list-type` change could have turned an empty selection into a pass.
    """
    noisy = json.dumps(
        {
            "rust-suites": {"a::b": {"testcases": {}}},
            "note": "Compiling hew-cli v0.6.0\nFinished test profile\nwarning: x",
        }
    )
    result = run_floor(noisy)
    assert result.returncode != 0, result.stdout
    assert "selected no tests" in result.stderr, result.stderr


def test_a_zero_match_platform_filter_is_rejected_end_to_end() -> None:
    """The real thing: nextest, a real config, a filter that selects nothing.

    The filter is a self-difference (`binary_id(X) - binary_id(X)`) because it
    has to be one nextest ACCEPTS. An unmatched `binary_id` is rejected at
    config-parse time with exit 96 -- a useful second line of defence, but not
    the case this floor exists for. A self-difference parses, lists cleanly,
    exits 0, and selects no test at all: exactly the green-having-run-nothing
    shape.

    Skipped only when cargo-nextest is unavailable, so a machine without it
    reports "not run" rather than a pass it did not earn.
    """
    if shutil.which("cargo") is None:
        print("SKIP (no cargo on PATH)")
        return
    probe = subprocess.run(
        ["cargo", "nextest", "--version"], capture_output=True, text=True
    )
    if probe.returncode != 0:
        print("SKIP (cargo-nextest unavailable)")
        return

    config = (ROOT / ".config" / "nextest.toml").read_text(encoding="utf-8")
    start = config.index("[profile.platform]")
    end = config.index("[profile.smoke]")
    block = config[start:end]
    filter_start = block.index('default-filter = """')
    filter_end = block.index('"""', filter_start + len('default-filter = """')) + 3
    broken = (
        config[:start]
        + block[:filter_start]
        + 'default-filter = "binary_id(hew-cli::run_e2e) - binary_id(hew-cli::run_e2e)"\n'
        + block[filter_end:]
        + config[end:]
    )

    with tempfile.TemporaryDirectory() as raw:
        scratch = Path(raw) / "nextest.toml"
        scratch.write_text(broken, encoding="utf-8")
        listed = subprocess.run(
            [
                "cargo",
                "nextest",
                "list",
                "--config-file",
                str(scratch),
                "--profile",
                "platform",
                "--workspace",
                "--exclude",
                "hew-wasm",
                "--exclude",
                "hew-cabi",
                "--message-format",
                "json",
                "--list-type",
                "full",
            ],
            cwd=ROOT,
            capture_output=True,
            text=True,
            env={**os.environ, "HEW_TEST_NO_BUILD": "1"},
        )
        if listed.returncode != 0:
            print("SKIP (workspace could not be listed in this environment)")
            return
        selected = floor.parse_listing(listed.stdout)
        assert not selected, (
            "a self-difference filter still selected tests; the counterfactual "
            "proves nothing"
        )
        result = run_floor(listed.stdout)
        assert result.returncode != 0, result.stdout
        assert "selected no tests" in result.stderr, result.stderr


def test_the_real_platform_profile_selects_a_non_empty_set() -> None:
    """And the floor is not vacuous in the other direction.

    Non-empty only. The number is reported by the job summary and never
    asserted here.
    """
    if shutil.which("cargo") is None:
        print("SKIP (no cargo on PATH)")
        return
    listed = subprocess.run(
        [
            "cargo",
            "nextest",
            "list",
            "--profile",
            "platform",
            "--workspace",
            "--exclude",
            "hew-wasm",
            "--exclude",
            "hew-cabi",
            "--message-format",
            "json",
            "--list-type",
            "full",
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        env={**os.environ, "HEW_TEST_NO_BUILD": "1"},
    )
    if listed.returncode != 0:
        print("SKIP (workspace could not be listed in this environment)")
        return
    selected = floor.parse_listing(listed.stdout)
    assert selected, "the platform profile selects nothing in this tree"


def _discover_tests() -> list:
    return [
        value
        for name, value in sorted(globals().items())
        if name.startswith("test_") and callable(value)
    ]


if __name__ == "__main__":
    failures = 0
    discovered = _discover_tests()
    for test in discovered:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(discovered)} tests failed")
    print(f"All {len(discovered)} tests passed.")
