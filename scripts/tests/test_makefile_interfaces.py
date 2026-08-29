#!/usr/bin/env python3
"""Counterfactuals for generated help and recursive tracked-shell linting."""

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


help_index = load("make_help", ROOT / "scripts" / "make-help.py")
shell_lint = load("shell_script_lint", ROOT / "scripts" / "shell-script-lint.py")


def require_value_error(makefile: str, message: str) -> None:
    try:
        help_index.entries(makefile)
    except ValueError as error:
        assert message in str(error), error
    else:
        raise AssertionError(f"expected help index failure containing {message!r}")


def test_real_help_index_is_generated_and_bounded() -> None:
    makefile = (ROOT / "Makefile").read_text(encoding="utf-8")
    entries = help_index.entries(makefile)
    assert entries
    output = help_index.render(makefile)
    for section, target, purpose in entries:
        assert f"{section}:" in output
        assert f"make {target}" in output
        assert purpose in output


def test_renamed_target_takes_its_help_entry_with_it() -> None:
    before = "old-name: dependency ## Build: compile it\n"
    after = before.replace("old-name:", "new-name:")
    assert help_index.entries(before)[0][1] == "old-name"
    assert help_index.entries(after)[0][1] == "new-name"


def test_unknown_help_section_fails_closed() -> None:
    require_value_error("thing: ## Deploy: ship it\n", "unknown help section 'Deploy'")


def test_empty_help_index_fails_closed() -> None:
    require_value_error("all:\n\ttrue\n", "help index is empty")


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
