#!/usr/bin/env python3
"""Counterfactuals for generated help and recursive tracked-shell linting."""

from __future__ import annotations

import importlib.util
import shlex
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


def dry_run_all(makefile: str) -> list[str]:
    """Return the executable plan for the default build under a staged Makefile."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".mk") as staged:
        staged.write(makefile)
        staged.flush()
        result = subprocess.run(
            ["make", "--always-make", "--dry-run", "--file", staged.name, "all"],
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
        )
    assert result.returncode == 0, result.stdout + result.stderr
    return result.stdout.splitlines()


def assert_default_compiler_profile(makefile: str) -> None:
    """The final default-build launcher is release-lib and is then asserted."""
    plan = dry_run_all(makefile)
    links: list[tuple[int, str, str]] = []
    for index, line in enumerate(plan):
        try:
            tokens = shlex.split(line)
        except ValueError:
            continue
        if len(tokens) == 4 and tokens[:2] == ["ln", "-sfn"]:
            links.append((index, tokens[2], tokens[3]))

    hew_links = [entry for entry in links if entry[2] == "build/bin/hew"]
    debug_links = [entry for entry in links if entry[2] == "build/bin/hew-debug"]
    assert hew_links, "default build never stages build/bin/hew"
    assert hew_links[-1][1] == "../../target/release-lib/hew", hew_links
    assert debug_links and debug_links[-1][1] == "../../target/debug/hew", debug_links

    assertion = next(
        (
            index
            for index, line in enumerate(plan)
            if 'actual="$(readlink "build/bin/hew")"' in line
        ),
        None,
    )
    assert assertion is not None, (
        "default build never asserts its final compiler profile"
    )
    assert assertion > hew_links[-1][0], "profile assertion runs before final assembly"


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
    assert len(entries) == 19, entries
    output = help_index.render(makefile)
    for section, target, purpose in entries:
        assert f"{section}:" in output
        assert f"make {target}" in output
        assert purpose in output


def test_default_build_preserves_and_asserts_release_lib_profile() -> None:
    makefile = (ROOT / "Makefile").read_text(encoding="utf-8")
    assert_default_compiler_profile(makefile)

    # Counterfactual: reproduce the old assembly recipe that overwrote the
    # supported launcher with DEBUG_HEW and exposed no explicit debug name.
    release_link = (
        '\t@ln -sfn "$(LINK_UP2)$(RELEASE_LIB_HEW)"              '
        '"$(BUILD_DIR)/bin/hew"\n'
    )
    debug_link = (
        '\t@ln -sfn "$(LINK_UP2)$(DEBUG_HEW)"                    '
        '"$(BUILD_DIR)/bin/hew-debug"\n'
    )
    old_overwrite = (
        '\t@ln -sfn "$(LINK_UP2)$(DEBUG_HEW)"                    '
        '"$(BUILD_DIR)/bin/hew"\n'
    )
    assert makefile.count(release_link) == 1
    assert makefile.count(debug_link) == 1
    mutated = makefile.replace(release_link + debug_link, old_overwrite, 1)
    try:
        assert_default_compiler_profile(mutated)
    except AssertionError:
        pass
    else:
        raise AssertionError("old debug-launcher overwrite escaped the counterfactual")


def test_renamed_target_takes_its_help_entry_with_it() -> None:
    before = "old-name: dependency ## Build: compile it\n"
    after = before.replace("old-name:", "new-name:")
    assert help_index.entries(before)[0][1] == "old-name"
    assert help_index.entries(after)[0][1] == "new-name"


def test_unknown_help_section_fails_closed() -> None:
    require_value_error("thing: ## Deploy: ship it\n", "unknown help section 'Deploy'")


def test_empty_help_index_fails_closed() -> None:
    require_value_error("all:\n\ttrue\n", "help index is empty")


def test_help_index_size_cap_has_teeth() -> None:
    makefile = "".join(
        f"target-{index}: ## Build: entry {index}\n"
        for index in range(help_index.MAX_ENTRIES + 1)
    )
    require_value_error(makefile, "maximum is 20")


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


def test_xtask_ratchet_gates_use_the_consolidated_driver() -> None:
    source = (ROOT / "xtask" / "src" / "build_system.rs").read_text(encoding="utf-8")
    assert '"scripts/hew-suite-ratchet.sh"' not in source
    assert '"scripts/stdlib-ratchet.sh"' not in source
    assert (
        'compiled_hew_script(root, "scripts/corpus-ratchet.sh", &["hew-suite"])'
        in source
    )
    assert (
        'compiled_hew_script(root, "scripts/corpus-ratchet.sh", &["stdlib"])' in source
    )


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
