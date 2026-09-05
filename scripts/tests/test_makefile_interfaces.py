#!/usr/bin/env python3
"""Regression coverage for tracked-shell discovery and failure propagation."""

from __future__ import annotations

import importlib.util
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


def test_shell_lint_discovers_tracked_nested_scripts_only() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        subprocess.run(["git", "init", "-q"], cwd=root, check=True)
        (root / "scripts" / "lib").mkdir(parents=True)
        (root / "scripts" / "top.sh").write_text("#!/usr/bin/env bash\ntrue\n")
        (root / "scripts" / "lib" / "nested.sh").write_text(
            "#!/usr/bin/env bash\ntrue\n"
        )
        (root / "scripts" / "untracked.sh").write_text("if broken\n")
        subprocess.run(
            ["git", "add", "scripts/top.sh", "scripts/lib/nested.sh"],
            cwd=root,
            check=True,
        )
        assert shell_lint.tracked_shell_scripts(root) == (
            "scripts/lib/nested.sh",
            "scripts/top.sh",
        )


def test_tracked_nested_syntax_error_fails_shell_lint() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        subprocess.run(["git", "init", "-q"], cwd=root, check=True)
        (root / "scripts" / "lib").mkdir(parents=True)
        (root / "scripts" / "lib" / "broken.sh").write_text("if broken\n")
        subprocess.run(["git", "add", "scripts/lib/broken.sh"], cwd=root, check=True)
        assert shell_lint.check(root) != 0


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
