#!/usr/bin/env python3
"""Regression tests for the dependency-free Cargo config fallback."""

from __future__ import annotations

import importlib.util
import os
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = ROOT / "scripts" / "cargo-output-dir.py"
os.environ["HEW_FORCE_TOML_FALLBACK"] = "1"
SPEC = importlib.util.spec_from_file_location("cargo_output_dir", MODULE_PATH)
assert SPEC is not None and SPEC.loader is not None
cargo_output_dir = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(cargo_output_dir)


def parse(text: str) -> str:
    return cargo_output_dir._fallback_build_target(text, Path(".cargo/config.toml"))


def test_build_table_string() -> None:
    assert cargo_output_dir.toml_compat._stdlib_tomllib is None, (
        "test must exercise the dependency-free TOML reader"
    )
    assert parse('[build]\ntarget = "aarch64-apple-darwin"\n') == "aarch64-apple-darwin"


def test_root_dotted_key() -> None:
    assert (
        parse("build.target = 'x86_64-unknown-freebsd'\n") == "x86_64-unknown-freebsd"
    )


def test_single_multiline_array_and_comments() -> None:
    text = """
[build]
target = [
    "wasm32-wasip1", # the selected target
]
"""
    assert parse(text) == "wasm32-wasip1"


def test_hash_inside_target_is_not_a_comment() -> None:
    assert parse('[build]\ntarget = "custom#target" # comment\n') == "custom#target"


def test_literal_string_backslash_is_not_a_python_escape() -> None:
    assert parse("[build]\ntarget = 'custom\\target'\n") == "custom\\target"


def test_quoted_build_table() -> None:
    assert parse('["build"]\ntarget = "aarch64-apple-darwin"\n') == (
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
            parse(source)
        except SystemExit:
            pass
        else:
            raise AssertionError(f"malformed Cargo config was accepted: {source!r}")


def test_missing_build_target() -> None:
    assert parse('[target.x86_64-pc-windows-msvc]\nlinker = "lld-link"\n') == ""


_TESTS = (
    test_build_table_string,
    test_root_dotted_key,
    test_single_multiline_array_and_comments,
    test_hash_inside_target_is_not_a_comment,
    test_literal_string_backslash_is_not_a_python_escape,
    test_quoted_build_table,
    test_malformed_config_fails_closed,
    test_missing_build_target,
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
