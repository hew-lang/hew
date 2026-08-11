#!/usr/bin/env python3
"""Counterfactuals for generated timeout policy."""

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


def test_workflow_value_is_rederived() -> None:
    text = "jobs:\n  changes:\n    timeout-minutes: 999\n"
    rendered, seen = POLICY.render_workflow(ROOT / ".github/workflows/ci.yml", text)
    assert "timeout-minutes: 5" in rendered
    assert seen == {"ci.yml/changes"}


def test_nextest_values_are_rederived() -> None:
    line = 'slow-timeout = { period = "999s", terminate-after = 99 }\n'
    text = "".join(
        f"# timeout-class: {workload}\n{line}" for workload in POLICY.NEXTEST_CLASSES
    )
    rendered = POLICY.render_nextest(text)
    assert 'period = "100s", terminate-after = 3' in rendered
    assert 'period = "300s", terminate-after = 3' in rendered
    assert "999" not in rendered


if __name__ == "__main__":
    test_workflow_value_is_rederived()
    print("PASS test_workflow_value_is_rederived")
    test_nextest_values_are_rederived()
    print("PASS test_nextest_values_are_rederived")
