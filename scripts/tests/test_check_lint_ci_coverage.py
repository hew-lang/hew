#!/usr/bin/env python3
"""Counterfactual tests for the lint CI coverage assertion."""

from __future__ import annotations

import importlib.util
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CHECKER = ROOT / "scripts" / "check-lint-ci-coverage.py"
SPEC = importlib.util.spec_from_file_location("check_lint_ci_coverage", CHECKER)
assert SPEC is not None and SPEC.loader is not None
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def require_failure(makefile: str, workflow: str, message: str) -> None:
    errors = MODULE.check(makefile, workflow)
    assert any(message in error for error in errors), errors


def main() -> None:
    makefile = (ROOT / "Makefile").read_text(encoding="utf-8")
    workflow = (ROOT / ".github" / "workflows" / "ci.yml").read_text(encoding="utf-8")
    assert MODULE.check(makefile, workflow) == []

    extra = makefile.replace(
        "lint: structural-lint", "lint: future-lint structural-lint", 1
    )
    require_failure(extra, workflow, "no CI step: future-lint")

    missing = workflow.replace("run: make codegen-trap-inventory-check", "run: true", 1)
    require_failure(makefile, missing, "no CI step: codegen-trap-inventory-check")

    replay = workflow.replace(
        "run: make codegen-trap-inventory-check",
        "run: make lint codegen-trap-inventory-check",
        1,
    )
    require_failure(makefile, replay, "must not replay make lint")

    duplicate = workflow.replace(
        "run: make runtime-poison-safe-lint",
        "run: make runtime-poison-safe-lint structural-lint",
        1,
    )
    require_failure(makefile, duplicate, "runs more than once: structural-lint")

    no_clippy = workflow.replace(
        "cargo clippy --workspace --tests --message-format=json",
        "cargo check --workspace --tests --message-format=json",
        1,
    )
    require_failure(makefile, no_clippy, "exactly one CI Clippy step")

    narrow_clippy = workflow.replace(
        "cargo clippy --workspace --tests --message-format=json",
        "cargo clippy --workspace --lib --message-format=json",
        1,
    )
    require_failure(makefile, narrow_clippy, "does not cover the lint recipe")

    print("lint CI coverage counterfactuals: PASS")


if __name__ == "__main__":
    main()
