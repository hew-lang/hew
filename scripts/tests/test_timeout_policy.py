#!/usr/bin/env python3
"""Counterfactuals for centralized timeout policy."""

from __future__ import annotations

import importlib.util
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "timeout_policy", ROOT / "scripts/timeout-policy.py"
)
assert SPEC is not None and SPEC.loader is not None
POLICY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(POLICY)


def test_workflow_value_is_centralized() -> None:
    text = "jobs:\n  changes:\n    timeout-minutes: 999\n"
    rendered, seen = POLICY.render_workflow(ROOT / ".github/workflows/ci.yml", text)
    assert "timeout-minutes: 5" in rendered
    assert seen == {"ci.yml/changes"}


def test_workflow_job_without_timeout_is_rejected() -> None:
    text = "jobs:\n  changes:\n    runs-on: ubuntu-24.04\n"
    try:
        POLICY.render_workflow(ROOT / ".github/workflows/ci.yml", text)
    except ValueError as error:
        assert "jobs without timeout-minutes: ['changes']" in str(error)
    else:
        raise AssertionError("workflow job without timeout-minutes was accepted")


def test_nextest_values_are_centralized() -> None:
    line = 'slow-timeout = { period = "999s", terminate-after = 99 }\n'
    text = "".join(
        f"# timeout-class: {workload}\n{line}" for workload in POLICY.NEXTEST_CLASSES
    )
    rendered = POLICY.render_nextest(text)
    assert 'period = "100s", terminate-after = 3' in rendered
    assert 'period = "300s", terminate-after = 3' in rendered
    assert "999" not in rendered


if __name__ == "__main__":
    test_workflow_value_is_centralized()
    print("PASS test_workflow_value_is_centralized")
    test_workflow_job_without_timeout_is_rejected()
    print("PASS test_workflow_job_without_timeout_is_rejected")
    test_nextest_values_are_centralized()
    print("PASS test_nextest_values_are_centralized")
