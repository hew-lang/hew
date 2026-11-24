#!/usr/bin/env python3
"""Regression coverage for tracked-shell discovery and failure propagation."""

from __future__ import annotations

import importlib.util
import os
import re
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


shell_lint = load("shell_script_lint", ROOT / "scripts" / "shell-script-lint.py")


def test_make_help_renders_the_current_target_index() -> None:
    result = subprocess.run(
        ["make", "--no-print-directory", "help"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=True,
    )
    assert "make all " in result.stdout, result.stdout
    assert "make hew-debug " in result.stdout, result.stdout


def test_sandbox_parity_complement_covers_all_hew_wasm_tests() -> None:
    env = dict(os.environ)
    env.setdefault("CARGO_TARGET_DIR", "target")
    env.setdefault("HOST_TRIPLE", "x86_64-unknown-linux-gnu")
    playground = subprocess.run(
        [
            "make",
            "-n",
            "-o",
            "playground-manifest-check",
            "-o",
            "sandbox-vm-deps",
            "playground-check",
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        env=env,
        check=True,
    )
    parity = subprocess.run(
        [
            "make",
            "-n",
            "-o",
            "hew-native",
            "-o",
            "wasm-runtime",
            "-o",
            "sandbox-vm-deps",
            "SANDBOX_PARITY_SKIP_PLAYGROUND=1",
            "HEW_WASM_MODULE=file:///dev/null",
            "sandbox-parity",
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        env=env,
        check=True,
    )
    playground_cmds = [
        line
        for line in playground.stdout.splitlines()
        if "cargo test -p hew-wasm" in line
    ]
    parity_cmds = [
        line for line in parity.stdout.splitlines() if "cargo test -p hew-wasm" in line
    ]
    assert len(playground_cmds) == 1, playground.stdout
    assert len(parity_cmds) == 2, parity.stdout
    playground_tests = set(re.findall(r"--test\s+([A-Za-z0-9_]+)", playground_cmds[0]))
    parity_tests = set(re.findall(r"--test\s+([A-Za-z0-9_]+)", parity_cmds[0]))
    all_tests = {p.stem for p in (ROOT / "hew-wasm" / "tests").glob("*.rs")}
    assert "--lib" in playground_cmds[0], playground_cmds[0]
    assert "--doc" not in parity_cmds[0], parity_cmds[0]
    assert parity_cmds[1].strip().endswith("--doc"), parity_cmds[1]
    assert playground_tests.isdisjoint(parity_tests), playground_tests & parity_tests
    assert (playground_tests | parity_tests) == all_tests, (
        all_tests - (playground_tests | parity_tests),
        (playground_tests | parity_tests) - all_tests,
    )


def test_shell_lint_discovers_tracked_nested_scripts_only() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        subprocess.run(["git", "init", "-q"], cwd=root, check=True)
        tracked = (
            ".github/scripts/download.sh",
            "examples/demo/run.sh",
            "installers/install.sh",
            "scripts/lib/nested.sh",
            "scripts/top.sh",
            "tests/package-install/run.sh",
        )
        for name in tracked:
            path = root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("#!/bin/sh\ntrue\n")
        (root / "scripts" / "untracked.sh").write_text("if broken\n")
        subprocess.run(["git", "add", *tracked], cwd=root, check=True)
        assert shell_lint.tracked_shell_scripts(root) == tracked
        assert shell_lint.check(root) == 0


def test_tracked_nested_syntax_error_fails_shell_lint() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        subprocess.run(["git", "init", "-q"], cwd=root, check=True)
        (root / "scripts" / "lib").mkdir(parents=True)
        (root / "scripts" / "lib" / "broken.sh").write_text("#!/bin/sh\nif broken\n")
        subprocess.run(["git", "add", "scripts/lib/broken.sh"], cwd=root, check=True)
        assert shell_lint.check(root) != 0


def test_posix_installer_rejects_bash_syntax() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        subprocess.run(["git", "init", "-q"], cwd=root, check=True)
        installer = root / "install.sh"
        installer.write_text("#!/bin/sh\nvalues=(one two)\n")
        subprocess.run(["git", "add", "install.sh"], cwd=root, check=True)
        assert shell_lint.check(root) != 0
        installer.write_text(
            "#!/usr/bin/env bash\nvalues=(one two)\nprintf '%s\\n' \"${values[@]}\"\n"
        )
        assert shell_lint.check(root) == 0


def tests() -> list:
    return [
        value
        for name, value in sorted(globals().items())
        if name.startswith("test_") and callable(value)
    ]


if __name__ == "__main__":
    selected = tests()
    for test in selected:
        test()
        print(f"PASS {test.__name__}")
    print(f"All {len(selected)} tests passed.")
