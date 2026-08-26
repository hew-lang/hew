#!/usr/bin/env python3
"""Falsifiability tests for scripts/platform-suite-floor.py.

The wrong program this rejects: a `[profile.platform]` `default-filter` that
matches nothing. `cargo nextest run` over an empty selection EXITS ZERO, so the
step is green, the job is green, and the required platform context is green
having executed no test at all.

These cases are fixture-driven on purpose. Listing the real workspace BUILDS
its test binaries, and this file is reached by the routed
`test-release-workflow-contract` gate whose build form declares that it
compiles nothing -- a `cargo nextest list` here would be the exact defect
`test_no_gate_compiles_behind_an_empty_build_form` exists to catch, and
guarding it at runtime does not help, because that walk reads the source.

The live proof is where it belongs: the Windows and macOS jobs run this floor
against a real listing on every smoke-tier pull request, so the non-empty
direction is proved by the thing it protects rather than by a local rehearsal.

The rejection direction was additionally verified end to end during
development, against cargo-nextest 0.9.132 and this repository real workspace:
a `[profile.platform]` `default-filter` of
`binary_id(hew-cli::run_e2e) - binary_id(hew-cli::run_e2e)` parses, lists
cleanly, exits 0 and selects zero tests -- and this floor rejects it. An
UNMATCHED `binary_id` is rejected by nextest itself at config-parse time with
exit 96, which is a useful second line of defence but not the case a floor is
for.

Only non-empty is asserted anywhere. An exact expected count would fire on
every added test and tell a reviewer nothing.
"""

import importlib.util
import json
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
