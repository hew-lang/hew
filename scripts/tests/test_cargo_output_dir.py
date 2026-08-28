#!/usr/bin/env python3
"""Regression tests for Cargo config parsing."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import os
import tomllib
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


def parse_config(text: str) -> str:
    """Parse Cargo configuration through the production TOML path."""
    path = Path(".cargo/config.toml")
    config = cargo_output_dir._load_toml(text, path)
    return cargo_output_dir._configured_target(config, path) or ""


def test_build_table_string() -> None:
    assert (
        parse_config('[build]\ntarget = "aarch64-apple-darwin"\n')
        == "aarch64-apple-darwin"
    )


def test_root_dotted_key() -> None:
    assert (
        parse_config("build.target = 'x86_64-unknown-freebsd'\n")
        == "x86_64-unknown-freebsd"
    )


def test_single_multiline_array_and_comments() -> None:
    text = """
[build]
target = [
    "wasm32-wasip1", # the selected target
]
"""
    assert parse_config(text) == "wasm32-wasip1"


def test_hash_inside_target_is_not_a_comment() -> None:
    assert (
        parse_config('[build]\ntarget = "custom#target" # comment\n') == "custom#target"
    )


def test_literal_string_backslash_is_not_a_python_escape() -> None:
    assert parse_config("[build]\ntarget = 'custom\\target'\n") == "custom\\target"


def test_quoted_build_table() -> None:
    assert parse_config('["build"]\ntarget = "aarch64-apple-darwin"\n') == (
        "aarch64-apple-darwin"
    )


def test_malformed_config_fails_closed() -> None:
    malformed = (
        "[broken\n",
        '[build]\ntarget = "bad\\x41escape"\n',
        '[build]\ntarget = "first"\ntarget = "second"\n',
    )
    for source in malformed:
        try:
            parse_config(source)
        except tomllib.TOMLDecodeError:
            pass
        else:
            raise AssertionError(f"malformed Cargo config was accepted: {source!r}")


def test_missing_build_target() -> None:
    assert parse_config('[target.x86_64-pc-windows-msvc]\nlinker = "lld-link"\n') == ""


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
    test_build_table_string,
    test_root_dotted_key,
    test_single_multiline_array_and_comments,
    test_hash_inside_target_is_not_a_comment,
    test_literal_string_backslash_is_not_a_python_escape,
    test_quoted_build_table,
    test_malformed_config_fails_closed,
    test_missing_build_target,
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
