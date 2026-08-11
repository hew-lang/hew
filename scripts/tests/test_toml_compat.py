#!/usr/bin/env python3
"""Regression coverage for the dependency-free Python 3.10 TOML path.

The Makefile runs this with HEW_FORCE_TOML_FALLBACK=1.  Keeping the force flag
in the command (rather than merely testing on whatever Python happens to be
installed) makes the fallback continuously testable on Linux's newer Python.
"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts" / "lib"))
import toml_compat  # noqa: E402


def run(*command: str) -> subprocess.CompletedProcess[str]:
    environment = os.environ.copy()
    environment["HEW_FORCE_TOML_FALLBACK"] = "1"
    return subprocess.run(
        [sys.executable, *command],
        cwd=ROOT,
        env=environment,
        check=False,
        text=True,
        capture_output=True,
    )


def assert_success(name: str, result: subprocess.CompletedProcess[str]) -> None:
    assert result.returncode == 0, (
        f"{name} failed:\nstdout:\n{result.stdout}\nstderr:\n{result.stderr}"
    )


def test_fallback_is_forced_and_parses_needed_constructs() -> None:
    assert toml_compat._stdlib_tomllib is None, "test must exercise the fallback"
    parsed = toml_compat.loads(
        """
title = "Hew\\nconfig"
path = { path = '../hew-runtime', default-features = false }
[target.'cfg(unix)'.dependencies]
libc = { version = "1" }
[[ownership.contracts]]
symbol = "hew_example"
params = ["borrow", "consume",]
note = '''literal # remains data'''
[[backlog]]
id = "one"
enabled = true
"""
    )
    assert parsed["path"]["path"] == "../hew-runtime"
    assert parsed["target"]["cfg(unix)"]["dependencies"]["libc"]["version"] == "1"
    assert parsed["ownership"]["contracts"][0]["params"] == ["borrow", "consume"]
    assert parsed["backlog"][0]["enabled"] is True
    multiline = toml_compat.loads('four = """\nline\n""""\nfive = """\nline\n"""""\n')
    assert multiline == {"four": 'line\n"', "five": 'line\n""'}
    try:
        toml_compat.loads("[broken\n")
    except toml_compat.TOMLDecodeError:
        pass
    else:
        raise AssertionError("malformed TOML must fail closed")


def test_real_policy_manifests_parse_with_fallback() -> None:
    classification = toml_compat.loads(
        (ROOT / "scripts" / "jit-symbol-classification.toml").read_text("utf-8")
    )
    assert "hew_actor_spawn" in classification["stable"]
    assert classification["ownership"]["contracts"]
    wasm = toml_compat.loads(
        (ROOT / "wasm-capability-manifest.toml").read_text("utf-8")
    )
    assert wasm["manifest_version"] == 1
    assert wasm["backlog"]


def test_malformed_toml_fails_closed() -> None:
    malformed = (
        "manifest_version = 1_\n",
        "[policy]\na = 1\n[policy]\nb = 2\n",
        "policy = { enabled = true }\npolicy.reason = 'late mutation'\n",
        'path = "invalid\\/toml"\n',
        'value = "\\uD800"\n',
        "value = {\nenabled = true\n}\n",
        "value = 'single-line\nliteral'\n",
        "[a.b.c]\nz = 9\n[a]\nb.c.t = 9\n",
        "[[tab.arr]]\n[tab]\narr.value = 1\n",
        "# invalid form feed\finside comment\n",
    )
    for source in malformed:
        try:
            toml_compat.loads(source)
        except toml_compat.TOMLDecodeError:
            pass
        else:
            raise AssertionError(f"malformed TOML was accepted: {source!r}")


def test_real_consumer_documents_match_stdlib_when_available() -> None:
    try:
        import tomllib
    except ModuleNotFoundError:
        return

    crates = run("scripts/libhew-inputs.py", "crates")
    assert_success("libhew crate discovery", crates)
    paths = [
        *(ROOT / name / "Cargo.toml" for name in crates.stdout.splitlines()),
        ROOT / "scripts" / "jit-symbol-classification.toml",
        ROOT / "scripts" / "ffi-ownership-ratchet.toml",
        ROOT / "wasm-capability-manifest.toml",
        ROOT / ".cargo" / "config.toml",
    ]
    for path in paths:
        text = path.read_text(encoding="utf-8")
        assert toml_compat.loads(text) == tomllib.loads(text), (
            f"fallback diverges from tomllib for {path}"
        )


def test_focused_consumers_run_with_fallback() -> None:
    cases = (
        ("libhew inputs", "scripts/libhew-inputs.py", "crates"),
        ("wasm todo self-test", "scripts/lint-wasm-todo.py", "--self-test"),
        (
            "ffi contracts",
            "scripts/verify-ffi-symbols.py",
            "--classify",
            "stable",
            "--validate",
        ),
        ("system-lane closure", "scripts/sys-lane-closure.py"),
    )
    for name, *command in cases:
        assert_success(name, run(*command))


_TESTS = (
    test_fallback_is_forced_and_parses_needed_constructs,
    test_real_policy_manifests_parse_with_fallback,
    test_malformed_toml_fails_closed,
    test_real_consumer_documents_match_stdlib_when_available,
    test_focused_consumers_run_with_fallback,
)


if __name__ == "__main__":
    failures = 0
    for test in _TESTS:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as error:
            failures += 1
            print(f"FAIL {test.__name__}: {error}")
    if failures:
        raise SystemExit(f"{failures}/{len(_TESTS)} tests failed")
    print(f"All {len(_TESTS)} tests passed.")
