#!/usr/bin/env python3
"""Regression tests for Hew target selection and Cargo metadata failures."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import os
from collections.abc import Iterator
from pathlib import Path
from types import SimpleNamespace

# Mirrors run_counterfactual in scripts/fuzz/oracle-selftest.sh.  The
# counterfactuals below drive
# cargo-output-dir.py into its fail-closed paths, and those paths print
# "error: ..." to stderr before exiting.  On a PASSING run that text is bait:
# anything reading the log for a first failure line could report this test's
# own provoked diagnostic as the defect. Replaying it behind the
# marker keeps it readable and unmistakable.
# Keep the prefix on every provoked line.
COUNTERFACTUAL_MARKER = "CF-"


@contextlib.contextmanager
def counterfactual(label: str) -> Iterator[None]:
    buffer = io.StringIO()
    try:
        with contextlib.redirect_stderr(buffer):
            yield
    finally:
        for line in buffer.getvalue().splitlines():
            print(f"{COUNTERFACTUAL_MARKER}[{label}] {line}")


ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = ROOT / "scripts" / "cargo-output-dir.py"
SPEC = importlib.util.spec_from_file_location("cargo_output_dir", MODULE_PATH)
assert SPEC is not None and SPEC.loader is not None
cargo_output_dir = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(cargo_output_dir)


def test_configured_target_shapes() -> None:
    for config, expected in (
        ({}, None),
        ({"build": {}}, None),
        ({"build": {"target": "aarch64-apple-darwin"}}, "aarch64-apple-darwin"),
        ({"build": {"target": ["wasm32-wasip1"]}}, "wasm32-wasip1"),
    ):
        assert cargo_output_dir._configured_target(config, MODULE_PATH) == expected


def test_ambiguous_or_invalid_target_fails_closed() -> None:
    configs = [
        {"build": "not a table"},
        *({"build": {"target": value}} for value in ("", [], ["a", "b"], [1], 1)),
    ]
    for config in configs:
        try:
            with counterfactual("invalid-target"):
                cargo_output_dir._configured_target(config, MODULE_PATH)
        except SystemExit as exc:
            assert exc.code == 1
        else:
            raise AssertionError(f"invalid target was accepted: {config!r}")


def test_metadata_failure_fails_closed() -> None:
    original_run = cargo_output_dir.subprocess.run
    original_target_dir = os.environ.pop("CARGO_TARGET_DIR", None)
    cargo_output_dir.subprocess.run = lambda *args, **kwargs: SimpleNamespace(
        returncode=101,
        stdout=b"",
        stderr=b"workspace metadata failed",
    )
    try:
        try:
            with counterfactual("metadata-failure"):
                cargo_output_dir.target_root()
        except SystemExit as exc:
            assert exc.code == 1
        else:
            raise AssertionError(
                "metadata failure fell back to a stale target directory"
            )
    finally:
        cargo_output_dir.subprocess.run = original_run
        if original_target_dir is not None:
            os.environ["CARGO_TARGET_DIR"] = original_target_dir


def test_missing_cargo_fails_closed() -> None:
    original_run = cargo_output_dir.subprocess.run
    original_target_dir = os.environ.pop("CARGO_TARGET_DIR", None)

    def missing_cargo(*args: object, **kwargs: object) -> object:
        raise OSError("cargo is unavailable")

    cargo_output_dir.subprocess.run = missing_cargo
    try:
        try:
            with counterfactual("missing-cargo"):
                cargo_output_dir.target_root()
        except SystemExit as exc:
            assert exc.code == 1
        else:
            raise AssertionError("missing cargo fell back to a stale target directory")
    finally:
        cargo_output_dir.subprocess.run = original_run
        if original_target_dir is not None:
            os.environ["CARGO_TARGET_DIR"] = original_target_dir


def test_malformed_metadata_fails_closed() -> None:
    original_run = cargo_output_dir.subprocess.run
    original_target_dir = os.environ.pop("CARGO_TARGET_DIR", None)
    malformed = (
        b'{"workspace_root": "/tmp/no-target"}',
        b'{"target_directory": ""}',
        b'{"target_directory": ["/tmp/not-a-string"]}',
    )
    try:
        for output in malformed:
            cargo_output_dir.subprocess.run = lambda *args, _output=output, **kwargs: (
                SimpleNamespace(
                    returncode=0,
                    stdout=_output,
                    stderr=b"",
                )
            )
            try:
                with counterfactual("malformed-metadata"):
                    cargo_output_dir.target_root()
            except SystemExit as exc:
                assert exc.code == 1
            else:
                raise AssertionError(
                    f"malformed metadata fell back to a stale target directory: {output!r}"
                )
    finally:
        cargo_output_dir.subprocess.run = original_run
        if original_target_dir is not None:
            os.environ["CARGO_TARGET_DIR"] = original_target_dir


_TESTS = (
    test_configured_target_shapes,
    test_ambiguous_or_invalid_target_fails_closed,
    test_metadata_failure_fails_closed,
    test_missing_cargo_fails_closed,
    test_malformed_metadata_fails_closed,
)


if __name__ == "__main__":
    failures = 0
    for test in _TESTS:
        try:
            test()
            print(f"PASS {test.__name__}")
        except (AssertionError, SystemExit) as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(_TESTS)} tests failed")
    print(f"All {len(_TESTS)} tests passed.")
