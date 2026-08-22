#!/usr/bin/env python3
"""Assert that CI executes every member of the local lint graph exactly once."""

from __future__ import annotations

import re
import shlex
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MAKEFILE = ROOT / "Makefile"
WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
STRUCTURAL_LINT_WRAPPER = ROOT / "scripts" / "ast-grep-lint.sh"


def make_variable_words(makefile: str, name: str) -> tuple[str, ...]:
    words: list[str] = []
    assignment = re.compile(
        rf"^{re.escape(name)}\s*(\+=|:=|=)\s*([^\n]*)$", re.MULTILINE
    )
    for match in assignment.finditer(makefile):
        value = match.group(2).split()
        if match.group(1) == "+=":
            words.extend(value)
        else:
            words = value
    if not words:
        raise ValueError(f"Makefile variable {name} has no members")
    return tuple(words)


def lint_prerequisites(makefile: str) -> tuple[str, ...]:
    match = re.search(r"^lint:\s*([^\n]*)$", makefile, re.MULTILINE)
    if match is None:
        raise ValueError("Makefile has no plain lint target")
    prerequisites: list[str] = []
    for word in match.group(1).split():
        variable = re.fullmatch(r"\$\$\(([A-Za-z_][A-Za-z0-9_]*)\)", word)
        if variable is None:
            prerequisites.append(word)
        else:
            prerequisites.extend(make_variable_words(makefile, variable.group(1)))
    if not prerequisites or len(prerequisites) != len(set(prerequisites)):
        raise ValueError("lint prerequisites must be a nonempty unique list")
    return tuple(prerequisites)


def lint_recipe_tokens(makefile: str) -> tuple[str, ...]:
    match = re.search(r"^lint:[^\n]*\n(\t[^\n]+)", makefile, re.MULTILINE)
    if match is None:
        raise ValueError("lint target has no recipe")
    tokens = tuple(shlex.split(match.group(1).strip()))
    if tokens[:2] != ("cargo", "clippy"):
        raise ValueError("lint recipe must be a cargo clippy command")
    return tokens


def lint_job(workflow: str) -> str:
    start = workflow.find("  lint:\n")
    if start < 0:
        raise ValueError("ci.yml has no lint job")
    following = re.search(r"^  [a-zA-Z0-9_-]+:\n", workflow[start + 1 :], re.MULTILINE)
    if following is None:
        return workflow[start:]
    return workflow[start : start + 1 + following.start()]


def run_blocks(job: str) -> tuple[tuple[str, str], ...]:
    lines = job.splitlines()
    blocks: list[tuple[str, str]] = []
    index = 0
    while index < len(lines):
        name_match = re.match(r"^      - name:\s*(.+?)\s*$", lines[index])
        if name_match is None:
            index += 1
            continue
        name = name_match.group(1).strip("\"'")
        end = index + 1
        while end < len(lines) and not re.match(
            r"^      - (?:name:|uses:)", lines[end]
        ):
            end += 1
        step = lines[index:end]
        run = ""
        for offset, line in enumerate(step):
            scalar = re.match(r"^        run:\s*(.*?)\s*$", line)
            if scalar is None:
                continue
            if scalar.group(1) != "|":
                run = scalar.group(1)
                break
            body: list[str] = []
            for body_line in step[offset + 1 :]:
                if body_line.startswith("          "):
                    body.append(body_line[10:])
                elif body_line.strip() and not body_line.lstrip().startswith("#"):
                    break
            run = "\n".join(body)
            break
        if run:
            blocks.append((name, run))
        index = end
    return tuple(blocks)


def invoked_make_targets(command: str) -> set[str]:
    targets: set[str] = set()
    for line in command.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        try:
            words = shlex.split(stripped, comments=True)
        except ValueError:
            continue
        if words and words[0] == "make":
            targets.update(word for word in words[1:] if not word.startswith("-"))
    return targets


def command_tokens(command: str) -> tuple[str, ...]:
    command = command.replace("\\\n", " ").split("|", 1)[0].strip()
    try:
        return tuple(shlex.split(command, comments=True))
    except ValueError:
        return ()


def is_subsequence(expected: tuple[str, ...], actual: tuple[str, ...]) -> bool:
    remaining = iter(actual)
    return all(any(token == candidate for candidate in remaining) for token in expected)


def wrapper_command_count(wrapper: str, expected: tuple[str, ...]) -> int:
    count = 0
    for line in wrapper.splitlines():
        try:
            tokens = tuple(shlex.split(line.strip(), comments=True))
        except ValueError:
            continue
        if tokens == expected:
            count += 1
    return count


def check(
    makefile: str, workflow: str, structural_wrapper: str | None = None
) -> list[str]:
    errors: list[str] = []
    prerequisites = lint_prerequisites(makefile)
    blocks = run_blocks(lint_job(workflow))
    if structural_wrapper is None:
        structural_wrapper = STRUCTURAL_LINT_WRAPPER.read_text(encoding="utf-8")

    owners: dict[str, list[str]] = {target: [] for target in prerequisites}
    aggregate_owners: list[str] = []
    clippy_owners: list[tuple[str, tuple[str, ...]]] = []
    for name, command in blocks:
        targets = invoked_make_targets(command)
        if "lint" in targets:
            aggregate_owners.append(name)
        for target in prerequisites:
            if target in targets:
                owners[target].append(name)
        if re.search(r"(?:^|\s)cargo\s+clippy(?:\s|$)", command):
            clippy_owners.append((name, command_tokens(command)))

    if aggregate_owners:
        errors.append(
            "lint job must not replay make lint: " + ", ".join(aggregate_owners)
        )
    for target, names in owners.items():
        if not names:
            errors.append(f"lint prerequisite has no CI step: {target}")
        elif len(names) > 1:
            errors.append(
                f"lint prerequisite runs more than once: {target}: {', '.join(names)}"
            )
    if len(clippy_owners) != 1:
        errors.append(
            "lint recipe must have exactly one CI Clippy step: "
            + (
                ", ".join(name for name, _ in clippy_owners)
                if clippy_owners
                else "none"
            )
        )
    elif not is_subsequence(lint_recipe_tokens(makefile), clippy_owners[0][1]):
        errors.append(
            f"CI Clippy step does not cover the lint recipe: {clippy_owners[0][0]}"
        )

    keyspace_gate = wrapper_command_count(
        structural_wrapper,
        ("python3", "scripts/canonical-keyspace-lint.py", "--ast-grep", "$AST_GREP"),
    )
    if keyspace_gate != 1:
        errors.append(
            "structural lint must run canonical keyspace gate exactly once: "
            f"found {keyspace_gate}"
        )
    keyspace_test = wrapper_command_count(
        structural_wrapper,
        ("python3", "scripts/tests/test_canonical_keyspace_lint.py", "$AST_GREP"),
    )
    if keyspace_test != 1:
        errors.append(
            "structural lint must run canonical keyspace counterfactuals exactly once: "
            f"found {keyspace_test}"
        )
    return errors


def main() -> int:
    errors = check(
        MAKEFILE.read_text(encoding="utf-8"),
        WORKFLOW.read_text(encoding="utf-8"),
        STRUCTURAL_LINT_WRAPPER.read_text(encoding="utf-8"),
    )
    if errors:
        print("lint CI coverage: FAIL", file=sys.stderr)
        for error in errors:
            print(f"  - {error}", file=sys.stderr)
        return 1
    prerequisites = lint_prerequisites(MAKEFILE.read_text(encoding="utf-8"))
    print(f"lint CI coverage: PASS ({len(prerequisites)} prerequisites + Clippy)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
