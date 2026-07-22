"""Executable checks for the FreeBSD workflow's platform contract."""

import re
import shlex
from pathlib import Path
from typing import Callable


ROOT = Path(__file__).resolve().parents[2]
CI_WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
WORKFLOW = ROOT / ".github" / "workflows" / "freebsd.yml"
RELEASE_GATE = ROOT / ".github" / "workflows" / "release-gate.yml"

REQUIRED_CI_JOB = "lint"
REQUIRED_CI_JOB_NAME = "Clippy & format"
CONTRACT_STEP_NAME = "Verify FreeBSD workflow contract"
CONTRACT_COMMAND = "make freebsd-workflow-contract-check"

WASI_FILTER = (
    "not ((test(eval_wasm) & not test(reject)) | "
    "(binary(wasi_run_e2e) & not test(native_)))"
)
EXPECTED_NEXTEST_COMMAND = (
    "cargo",
    "nextest",
    "run",
    "--workspace",
    "--exclude",
    "hew-wasm",
    "--exclude",
    "hew-cabi",
    "-E",
    WASI_FILTER,
    "--profile",
    "ci",
    "--no-fail-fast",
)
EXPECTED_PKG_UPDATE = ("pkg", "update", "-f", "-r", "FreeBSD")
EXPECTED_PKG_BOOTSTRAP = (
    "/usr/sbin/pkg",
    "bootstrap",
    "-fy",
    "-r",
    "FreeBSD",
)
PKG_INSTALL_PREFIX = (
    "pkg",
    "install",
    "-y",
    "-U",
    "-r",
    "FreeBSD",
)
FREEBSD_TOOL_PACKAGES = (
    "llvm22",
    "rust",
    "cmake",
    "ninja",
    "git",
    "pkgconf",
    "libffi",
    "libxml2",
    "wasmtime",
)
EXPECTED_PKG_INSTALL = (*PKG_INSTALL_PREFIX, *FREEBSD_TOOL_PACKAGES)
EXPECTED_PKG_PHASES = (
    EXPECTED_PKG_BOOTSTRAP,
    EXPECTED_PKG_UPDATE,
    EXPECTED_PKG_INSTALL,
)
EXPECTED_WASM_LD_LINK = (
    "ln",
    "-sf",
    "/usr/local/llvm22/bin/wasm-ld",
    "/usr/local/bin/wasm-ld",
)
EXPECTED_WASMTIME_PROBE = (
    "command",
    "-v",
    "wasmtime",
    "&&",
    "wasmtime",
    "--version",
)
EXPECTED_WASM_LD_PROBE = (
    "command",
    "-v",
    "wasm-ld",
    "&&",
    "wasm-ld",
    "--version",
)
WASI_TOOL_COMMANDS = (
    EXPECTED_PKG_UPDATE,
    EXPECTED_PKG_BOOTSTRAP,
    EXPECTED_PKG_INSTALL,
    EXPECTED_WASM_LD_LINK,
    EXPECTED_WASMTIME_PROBE,
    EXPECTED_WASM_LD_PROBE,
)
LITERAL_SCALAR_HEADER = r"\|[+-]?"


def _job_block(workflow: str, job_name: str) -> str:
    match = re.search(
        rf"(?ms)^  {re.escape(job_name)}:\n(.*?)(?=^  [A-Za-z0-9_-]+:\n|\Z)",
        workflow,
    )
    assert match is not None, f"missing workflow job: {job_name}"
    return match.group(1)


def _step_block(job: str, step_name: str) -> str:
    match = re.search(
        rf"(?ms)^      - name: {re.escape(step_name)}\n.*?(?=^      - |\Z)",
        job,
    )
    assert match is not None, f"missing workflow step: {step_name}"
    return match.group(0)


def _with_mapping(step: str) -> tuple[list[str], int]:
    step_lines = step.splitlines()
    assert step_lines, "workflow step cannot be empty"
    step_header = re.fullmatch(r"(?P<indent>[ ]+)- name: .+", step_lines[0])
    assert step_header is not None, "workflow step must start with a named list item"
    item_indent = len(step_header.group("indent"))
    step_child_indents = [
        len(line) - len(line.lstrip(" "))
        for line in step_lines[1:]
        if line.strip() and not line.lstrip().startswith("#")
    ]
    assert step_child_indents, "named workflow step must contain direct children"
    step_child_indent = min(step_child_indents)
    assert step_child_indent > item_indent

    matches = list(re.finditer(rf"(?m)^[ ]{{{step_child_indent}}}with:[ ]*$", step))
    assert len(matches) == 1, f"expected one active 'with' mapping, got {len(matches)}"

    match = matches[0]
    with_indent = step_child_indent
    mapping_lines: list[str] = []
    for line in step[match.end() :].splitlines():
        if line.strip() and not line.lstrip().startswith("#"):
            indent = len(line) - len(line.lstrip(" "))
            if indent <= with_indent:
                break
        mapping_lines.append(line)

    child_indents = [
        len(line) - len(line.lstrip(" "))
        for line in mapping_lines
        if line.strip() and not line.lstrip().startswith("#")
    ]
    assert child_indents, "the action 'with' mapping must contain direct children"
    child_indent = min(child_indents)
    assert child_indent > with_indent
    return mapping_lines, child_indent


def _literal_block(step: str, key: str) -> str:
    mapping_lines, child_indent = _with_mapping(step)
    field = re.compile(
        rf"^[ ]{{{child_indent}}}{re.escape(key)}:[ ]*"
        rf"(?P<indicator>[^ #]+)?(?:[ ]+#.*)?$"
    )
    matches = [
        (index, match)
        for index, line in enumerate(mapping_lines)
        if (match := field.fullmatch(line)) is not None
    ]
    assert len(matches) == 1, (
        f"expected one direct 'with' child {key!r}, got {len(matches)}"
    )

    index, match = matches[0]
    indicator = match.group("indicator") or ""
    assert re.fullmatch(LITERAL_SCALAR_HEADER, indicator), (
        f"direct 'with' child {key!r} must use a literal scalar, got {indicator!r}"
    )

    scalar_lines = mapping_lines[index + 1 :]
    base_indent = None
    for line in scalar_lines:
        if not line.strip():
            continue
        content_indent = len(line) - len(line.lstrip(" "))
        if content_indent <= child_indent:
            if line.lstrip().startswith("#"):
                continue
            break
        base_indent = content_indent
        break
    assert base_indent == child_indent + 2, (
        f"direct 'with' child {key!r} must use the canonical two-space "
        f"scalar indentation, got {base_indent!r}"
    )

    block_lines: list[str] = []
    for line in scalar_lines:
        if not line.strip():
            block_lines.append("")
            continue
        content_indent = len(line) - len(line.lstrip(" "))
        if content_indent <= child_indent:
            if line.lstrip().startswith("#"):
                continue
            break
        assert content_indent >= base_indent, (
            f"direct 'with' child {key!r} contains a line shallower than its "
            "scalar base indentation"
        )
        block_lines.append(line[base_indent:])
    return "\n".join(block_lines)


def _active_shell_commands(block: str) -> list[tuple[str, ...]]:
    commands: list[tuple[str, ...]] = []
    lines = block.splitlines()
    index = 0
    while index < len(lines):
        line = lines[index].strip()
        if not line or line.startswith("#"):
            index += 1
            continue

        parts: list[str] = []
        while True:
            continued = line.endswith("\\")
            parts.append(line[:-1].rstrip() if continued else line)
            if not continued:
                break
            index += 1
            assert index < len(lines), "unterminated shell command continuation"
            line = lines[index].strip()
            assert line and not line.startswith("#"), (
                "shell command continuation cannot skip a blank or comment line"
            )
        commands.append(tuple(shlex.split(" ".join(parts), comments=True)))
        index += 1
    return commands


def _rewrite_pkg_commands(
    step: str, replacements: dict[int, tuple[str, ...] | None]
) -> str:
    lines = step.splitlines(keepends=True)
    pkg_lines: list[int] = []
    pkg_commands: list[tuple[str, ...]] = []
    for index, line in enumerate(lines):
        stripped = line.strip()
        if not stripped.startswith(("pkg ", "/usr/sbin/pkg ")):
            continue
        command = tuple(shlex.split(stripped, comments=True))
        if command[:1] in (("pkg",), ("/usr/sbin/pkg",)):
            pkg_lines.append(index)
            pkg_commands.append(command)
    assert pkg_commands == list(EXPECTED_PKG_PHASES)
    assert set(replacements) <= set(range(len(pkg_lines)))

    for command_index, replacement in replacements.items():
        line_index = pkg_lines[command_index]
        line = lines[line_index]
        indent = line[: len(line) - len(line.lstrip(" "))]
        ending = "\n" if line.endswith("\n") else ""
        content = (
            "# package phase removed" if replacement is None else " ".join(replacement)
        )
        lines[line_index] = f"{indent}{content}{ending}"
    return "".join(lines)


def _assert_wasi_tool_setup(workflow: str, job_name: str, step_name: str) -> None:
    step = _step_block(_job_block(workflow, job_name), step_name)
    prepare_commands = _active_shell_commands(_literal_block(step, "prepare"))
    run_commands = _active_shell_commands(_literal_block(step, "run"))

    pkg_commands = [
        command
        for command in prepare_commands
        if command[:1] in (("pkg",), ("/usr/sbin/pkg",))
    ]
    assert pkg_commands == list(EXPECTED_PKG_PHASES), (
        f"{job_name} must bootstrap pkg through the base utility from FreeBSD, "
        "refresh only that named repository, then install "
        f"the exact tool set without an automatic update; got {pkg_commands!r}"
    )
    for required in (
        EXPECTED_WASM_LD_LINK,
        EXPECTED_WASMTIME_PROBE,
        EXPECTED_WASM_LD_PROBE,
    ):
        assert run_commands.count(required) == 1, (
            f"{job_name} must run exactly one active command {required!r}"
        )


def _assert_required_ci_path(workflow: str) -> None:
    job = _job_block(workflow, REQUIRED_CI_JOB)
    assert job.count(f"    name: {REQUIRED_CI_JOB_NAME}") == 1

    step = _step_block(job, CONTRACT_STEP_NAME)
    assert step.count(f"run: {CONTRACT_COMMAND}") == 1
    assert "if:" not in step, "required FreeBSD contract step must be unconditional"

    start = job.find("# >>> CI-PARITY-STEPS")
    end = job.find("# <<< CI-PARITY-STEPS", start + 1)
    assert start >= 0 and end > start, (
        "required FreeBSD contract step must be inside a CI-PARITY-STEPS block"
    )
    assert f"run: {CONTRACT_COMMAND}" in job[start:end]


def _nextest_commands(job: str) -> list[tuple[str, ...]]:
    commands: list[tuple[str, ...]] = []
    lines = job.splitlines()
    index = 0
    while index < len(lines):
        line = lines[index].strip()
        if not line.startswith("cargo nextest run --workspace"):
            index += 1
            continue

        parts: list[str] = []
        while True:
            continued = line.endswith("\\")
            parts.append(line[:-1].rstrip() if continued else line)
            if not continued:
                break
            index += 1
            assert index < len(lines), "unterminated cargo nextest continuation"
            line = lines[index].strip()
        commands.append(tuple(shlex.split(" ".join(parts))))
        index += 1
    return commands


def _assert_command_list(commands: list[tuple[str, ...]], job_name: str) -> None:
    assert commands == [EXPECTED_NEXTEST_COMMAND], (
        f"{job_name} must contain exactly the canonical FreeBSD nextest command; "
        f"got {commands!r}"
    )


def _assert_exact_nextest(workflow: str, job_name: str) -> None:
    _assert_command_list(_nextest_commands(_job_block(workflow, job_name)), job_name)


def _assert_rejected(check: Callable[[], None]) -> None:
    try:
        check()
    except AssertionError:
        return
    raise AssertionError("mutated workflow contract was accepted")


def test_freebsd_nextest_command_is_exact() -> None:
    _assert_exact_nextest(WORKFLOW.read_text(), "build-and-test")


def test_both_release_gate_freebsd_commands_are_exact() -> None:
    release_gate = RELEASE_GATE.read_text()
    _assert_exact_nextest(release_gate, "gate-freebsd-x86_64")
    _assert_exact_nextest(release_gate, "gate-freebsd-aarch64")


def test_all_freebsd_jobs_provision_and_probe_wasi_tools() -> None:
    _assert_wasi_tool_setup(
        WORKFLOW.read_text(), "build-and-test", "Build and test on FreeBSD"
    )
    release_gate = RELEASE_GATE.read_text()
    _assert_wasi_tool_setup(
        release_gate, "gate-freebsd-x86_64", "Build and test on FreeBSD"
    )
    _assert_wasi_tool_setup(
        release_gate,
        "gate-freebsd-aarch64",
        "Build and test on FreeBSD aarch64",
    )


def test_required_clippy_job_runs_contract_unconditionally() -> None:
    _assert_required_ci_path(CI_WORKFLOW.read_text())


def test_docs_copy_cannot_mask_required_job_mutation() -> None:
    workflow = CI_WORKFLOW.read_text()
    job = _job_block(workflow, REQUIRED_CI_JOB)
    assert job.count(f"run: {CONTRACT_COMMAND}") == 1
    mutated_job = job.replace(
        f"run: {CONTRACT_COMMAND}",
        "run: echo contract-check-removed",
        1,
    )
    mutated = workflow.replace(job, mutated_job, 1)
    assert mutated.count(f"run: {CONTRACT_COMMAND}") == 1, (
        "the optional docs/scripts copy must remain in the mutation control"
    )
    _assert_rejected(lambda: _assert_required_ci_path(mutated))


def test_required_job_parity_marker_drift_is_rejected() -> None:
    workflow = CI_WORKFLOW.read_text()
    job = _job_block(workflow, REQUIRED_CI_JOB)
    assert job.count("# >>> CI-PARITY-STEPS") == 1
    mutated_job = job.replace(
        "# >>> CI-PARITY-STEPS",
        "# required parity marker removed",
        1,
    )
    mutated = workflow.replace(job, mutated_job, 1)
    _assert_rejected(lambda: _assert_required_ci_path(mutated))


def test_added_nightly_exclusion_is_rejected() -> None:
    workflow = WORKFLOW.read_text()
    marker = "--exclude hew-wasm --exclude hew-cabi \\\n"
    assert workflow.count(marker) == 1
    mutated = workflow.replace(
        marker,
        marker + "              --exclude hew-runtime \\\n",
        1,
    )
    _assert_rejected(lambda: _assert_exact_nextest(mutated, "build-and-test"))


def test_aarch64_release_gate_drift_is_rejected() -> None:
    release_gate = RELEASE_GATE.read_text()
    arm_job = _job_block(release_gate, "gate-freebsd-aarch64")
    assert arm_job.count("--profile ci --no-fail-fast") == 1
    mutated_arm_job = arm_job.replace("--profile ci --no-fail-fast", "--profile ci", 1)
    _assert_rejected(
        lambda: _assert_command_list(
            _nextest_commands(mutated_arm_job), "gate-freebsd-aarch64"
        )
    )


def test_commented_nightly_tool_commands_are_rejected() -> None:
    workflow = WORKFLOW.read_text()
    job_name = "build-and-test"
    step_name = "Build and test on FreeBSD"
    step = _step_block(_job_block(workflow, job_name), step_name)
    for command in WASI_TOOL_COMMANDS:
        command_text = " ".join(command)
        expected_count = EXPECTED_PKG_PHASES.count(command) or 1
        assert step.count(command_text) == expected_count
        mutated_step = step.replace(command_text, f"# {command_text}", 1)
        mutated = workflow.replace(step, mutated_step, 1)
        assert command_text in mutated, "comment mutation must preserve raw text"
        _assert_rejected(
            lambda mutated=mutated: _assert_wasi_tool_setup(
                mutated, job_name, step_name
            )
        )


def test_single_release_leg_missing_wasmtime_is_rejected() -> None:
    release_gate = RELEASE_GATE.read_text()
    for job_name, step_name in (
        ("gate-freebsd-x86_64", "Build and test on FreeBSD"),
        ("gate-freebsd-aarch64", "Build and test on FreeBSD aarch64"),
    ):
        job = _job_block(release_gate, job_name)
        step = _step_block(job, step_name)
        install_text = " ".join(EXPECTED_PKG_INSTALL)
        assert step.count(install_text) == 1
        mutated_step = step.replace(
            install_text, install_text.removesuffix(" wasmtime"), 1
        )
        mutated_job = job.replace(step, mutated_step, 1)
        mutated = release_gate.replace(job, mutated_job, 1)
        other_job = (
            "gate-freebsd-aarch64"
            if job_name == "gate-freebsd-x86_64"
            else "gate-freebsd-x86_64"
        )
        assert install_text in _job_block(mutated, other_job), (
            "the opposite release leg must remain intact in the mutation control"
        )
        _assert_rejected(
            lambda mutated=mutated, job_name=job_name, step_name=step_name: (
                _assert_wasi_tool_setup(mutated, job_name, step_name)
            )
        )


def test_named_repository_drift_is_rejected_in_every_freebsd_job() -> None:
    for workflow, job_name, step_name in (
        (WORKFLOW.read_text(), "build-and-test", "Build and test on FreeBSD"),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-x86_64",
            "Build and test on FreeBSD",
        ),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-aarch64",
            "Build and test on FreeBSD aarch64",
        ),
    ):
        job = _job_block(workflow, job_name)
        step = _step_block(job, step_name)
        for command_index, command in enumerate(EXPECTED_PKG_PHASES):
            command_text = " ".join(command)
            replacements = [
                command_text.replace(" -r FreeBSD", "", 1),
                command_text.replace(" -r FreeBSD", " -r FreeBSD-ports", 1),
            ]
            if command[:2] == ("pkg", "install"):
                replacements.append(command_text.replace(" -U", "", 1))
            if command == EXPECTED_PKG_BOOTSTRAP:
                replacements.extend(
                    (
                        command_text.replace(" -fy", " -y", 1),
                        command_text.replace(" -fy", " -f", 1),
                    )
                )
            for replacement in replacements:
                mutated_step = _rewrite_pkg_commands(
                    step,
                    {command_index: tuple(shlex.split(replacement))},
                )
                mutated_job = job.replace(step, mutated_step, 1)
                mutated = workflow.replace(job, mutated_job, 1)
                mutated_commands = [
                    active
                    for active in _active_shell_commands(
                        _literal_block(mutated_step, "prepare")
                    )
                    if active[:1] in (("pkg",), ("/usr/sbin/pkg",))
                ]
                assert len(mutated_commands) == len(EXPECTED_PKG_PHASES)
                for other_index, expected in enumerate(EXPECTED_PKG_PHASES):
                    if other_index != command_index:
                        assert mutated_commands[other_index] == expected, (
                            "every opposite package phase must remain intact in "
                            "the authority mutation control"
                        )
                _assert_rejected(
                    lambda mutated=mutated, job_name=job_name, step_name=step_name: (
                        _assert_wasi_tool_setup(mutated, job_name, step_name)
                    )
                )


def test_pkg_bootstrap_phases_cannot_be_removed_reordered_or_merged() -> None:
    dynamic_self_install = (*PKG_INSTALL_PREFIX, "pkg")
    combined_install = (*PKG_INSTALL_PREFIX, "pkg", *FREEBSD_TOOL_PACKAGES)
    mutations: tuple[dict[int, tuple[str, ...] | None], ...] = (
        {0: None},
        {1: None},
        {2: None},
        {0: EXPECTED_PKG_UPDATE, 1: EXPECTED_PKG_BOOTSTRAP},
        {1: EXPECTED_PKG_INSTALL, 2: EXPECTED_PKG_UPDATE},
        {0: EXPECTED_PKG_INSTALL, 2: EXPECTED_PKG_BOOTSTRAP},
        {0: dynamic_self_install},
        {0: combined_install, 2: None},
        {0: None, 2: combined_install},
    )
    for workflow, job_name, step_name in (
        (WORKFLOW.read_text(), "build-and-test", "Build and test on FreeBSD"),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-x86_64",
            "Build and test on FreeBSD",
        ),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-aarch64",
            "Build and test on FreeBSD aarch64",
        ),
    ):
        job = _job_block(workflow, job_name)
        step = _step_block(job, step_name)
        for replacements in mutations:
            mutated_step = _rewrite_pkg_commands(step, replacements)
            mutated_job = job.replace(step, mutated_step, 1)
            mutated = workflow.replace(job, mutated_job, 1)
            _assert_rejected(
                lambda mutated=mutated, job_name=job_name, step_name=step_name: (
                    _assert_wasi_tool_setup(mutated, job_name, step_name)
                )
            )


def test_literal_blocks_accept_only_optional_chomping() -> None:
    for indicator in ("|", "|-", "|+"):
        step = (
            "      - name: synthetic action\n"
            "        with:\n"
            "          release: '15.0'\n"
            f"          run: {indicator}\n"
            "\n"
            "            echo active \\\n"
            "              continued\n"
            "            # shell comment\n"
        )
        assert _literal_block(step, "run") == (
            "\necho active \\\n  continued\n# shell comment"
        )


def test_folded_tool_blocks_are_rejected_in_every_freebsd_job() -> None:
    for workflow, job_name, step_name in (
        (WORKFLOW.read_text(), "build-and-test", "Build and test on FreeBSD"),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-x86_64",
            "Build and test on FreeBSD",
        ),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-aarch64",
            "Build and test on FreeBSD aarch64",
        ),
    ):
        job = _job_block(workflow, job_name)
        step = _step_block(job, step_name)
        for key in ("prepare", "run"):
            header = re.compile(
                rf"(?m)^(?P<prefix>[ ]+{key}:[ ]*)\|"
                rf"(?P<modifiers>(?:[1-9][+-]?|[+-][1-9]?)?)"
                rf"(?P<trailing>[ ]*)$"
            )
            assert len(header.findall(step)) == 1
            mutated_step = header.sub(
                r"\g<prefix>>\g<modifiers>\g<trailing>", step, count=1
            )
            mutated_job = job.replace(step, mutated_step, 1)
            mutated = workflow.replace(job, mutated_job, 1)
            _assert_rejected(
                lambda mutated=mutated, job_name=job_name, step_name=step_name: (
                    _assert_wasi_tool_setup(mutated, job_name, step_name)
                )
            )


def test_nested_fake_literal_blocks_cannot_mask_folded_fields() -> None:
    for workflow, job_name, step_name in (
        (WORKFLOW.read_text(), "build-and-test", "Build and test on FreeBSD"),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-x86_64",
            "Build and test on FreeBSD",
        ),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-aarch64",
            "Build and test on FreeBSD aarch64",
        ),
    ):
        job = _job_block(workflow, job_name)
        step = _step_block(job, step_name)
        _, child_indent = _with_mapping(step)
        for key, commands in (
            ("prepare", (EXPECTED_PKG_INSTALL,)),
            (
                "run",
                (
                    EXPECTED_WASM_LD_LINK,
                    EXPECTED_WASMTIME_PROBE,
                    EXPECTED_WASM_LD_PROBE,
                ),
            ),
        ):
            header = re.compile(
                rf"(?m)^(?P<indent>[ ]{{{child_indent}}}){key}:[ ]*\|"
                rf"(?P<modifiers>(?:[1-9][+-]?|[+-][1-9]?)?)"
                rf"(?P<trailing>[ ]*(?:#.*)?)$"
            )
            assert len(header.findall(step)) == 1
            fake_indent = " " * (child_indent + 2)
            command_indent = " " * (child_indent + 4)
            fake = (
                rf"\g<indent>{key}: >\g<modifiers>\g<trailing>"
                f"\n{fake_indent}{key}: |"
                + "".join(
                    f"\n{command_indent}{' '.join(command)}" for command in commands
                )
            )
            mutated_step = header.sub(fake, step, count=1)
            mutated_job = job.replace(step, mutated_step, 1)
            mutated = workflow.replace(job, mutated_job, 1)
            for command in commands:
                assert " ".join(command) in mutated, (
                    "nested-header mutation must preserve all required raw command text"
                )
            _assert_rejected(
                lambda mutated=mutated, job_name=job_name, step_name=step_name: (
                    _assert_wasi_tool_setup(mutated, job_name, step_name)
                )
            )


def test_explicit_indent_leading_character_decoys_are_rejected() -> None:
    for workflow, job_name, step_name in (
        (WORKFLOW.read_text(), "build-and-test", "Build and test on FreeBSD"),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-x86_64",
            "Build and test on FreeBSD",
        ),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-aarch64",
            "Build and test on FreeBSD aarch64",
        ),
    ):
        job = _job_block(workflow, job_name)
        step = _step_block(job, step_name)
        _, child_indent = _with_mapping(step)
        for key, commands in (
            ("prepare", (EXPECTED_PKG_INSTALL,)),
            (
                "run",
                (
                    EXPECTED_WASM_LD_LINK,
                    EXPECTED_WASMTIME_PROBE,
                    EXPECTED_WASM_LD_PROBE,
                ),
            ),
        ):
            header = re.compile(
                rf"(?m)^(?P<indent>[ ]{{{child_indent}}}){key}:[ ]*\|"
                rf"(?P<chomping>[+-]?)(?P<trailing>[ ]*(?:#.*)?)$"
            )
            assert len(header.findall(step)) == 1
            mutated_step = header.sub(
                rf"\g<indent>{key}: |1\g<chomping>\g<trailing>", step, count=1
            )
            for command in commands:
                command_text = " ".join(command)
                active_line = " " * (child_indent + 2) + command_text
                decoy_line = " " * (child_indent + 1) + "x" + command_text
                assert mutated_step.count(active_line) == 1
                mutated_step = mutated_step.replace(active_line, decoy_line, 1)

            mutated_job = job.replace(step, mutated_step, 1)
            mutated = workflow.replace(job, mutated_job, 1)
            for command in commands:
                assert " ".join(command) in mutated, (
                    "leading-character mutation must preserve required raw substrings"
                )
            _assert_rejected(
                lambda mutated=mutated, job_name=job_name, step_name=step_name: (
                    _assert_wasi_tool_setup(mutated, job_name, step_name)
                )
            )


def test_implicit_indent_leading_character_decoys_are_rejected() -> None:
    for workflow, job_name, step_name in (
        (WORKFLOW.read_text(), "build-and-test", "Build and test on FreeBSD"),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-x86_64",
            "Build and test on FreeBSD",
        ),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-aarch64",
            "Build and test on FreeBSD aarch64",
        ),
    ):
        job = _job_block(workflow, job_name)
        step = _step_block(job, step_name)
        _, child_indent = _with_mapping(step)
        for key, commands in (
            ("prepare", (EXPECTED_PKG_INSTALL,)),
            (
                "run",
                (
                    EXPECTED_WASM_LD_LINK,
                    EXPECTED_WASMTIME_PROBE,
                    EXPECTED_WASM_LD_PROBE,
                ),
            ),
        ):
            lines = step.splitlines()
            header = re.compile(
                rf"^[ ]{{{child_indent}}}{key}:[ ]*\|[+-]?(?:[ ]+#.*)?$"
            )
            header_indexes = [
                index for index, line in enumerate(lines) if header.fullmatch(line)
            ]
            assert len(header_indexes) == 1
            header_index = header_indexes[0]
            end_index = len(lines)
            for index in range(header_index + 1, len(lines)):
                line = lines[index]
                if line.strip() and len(line) - len(line.lstrip(" ")) <= child_indent:
                    end_index = index
                    break

            for index in range(header_index + 1, end_index):
                if lines[index].strip():
                    assert lines[index].startswith(" " * (child_indent + 2))
                    lines[index] = lines[index][1:]

            for command in commands:
                command_text = " ".join(command)
                active_line = " " * (child_indent + 1) + command_text
                decoy_line = " " * (child_indent + 1) + "x" + command_text
                assert lines.count(active_line) == 1
                lines[lines.index(active_line)] = decoy_line

            mutated_step = "\n".join(lines) + "\n"
            mutated_job = job.replace(step, mutated_step, 1)
            mutated = workflow.replace(job, mutated_job, 1)
            for command in commands:
                assert " ".join(command) in mutated, (
                    "implicit-indent mutation must preserve required raw substrings"
                )
            _assert_rejected(
                lambda mutated=mutated, job_name=job_name, step_name=step_name: (
                    _assert_wasi_tool_setup(mutated, job_name, step_name)
                )
            )


_TESTS = (
    test_freebsd_nextest_command_is_exact,
    test_both_release_gate_freebsd_commands_are_exact,
    test_all_freebsd_jobs_provision_and_probe_wasi_tools,
    test_required_clippy_job_runs_contract_unconditionally,
    test_docs_copy_cannot_mask_required_job_mutation,
    test_required_job_parity_marker_drift_is_rejected,
    test_added_nightly_exclusion_is_rejected,
    test_aarch64_release_gate_drift_is_rejected,
    test_commented_nightly_tool_commands_are_rejected,
    test_single_release_leg_missing_wasmtime_is_rejected,
    test_named_repository_drift_is_rejected_in_every_freebsd_job,
    test_pkg_bootstrap_phases_cannot_be_removed_reordered_or_merged,
    test_literal_blocks_accept_only_optional_chomping,
    test_folded_tool_blocks_are_rejected_in_every_freebsd_job,
    test_nested_fake_literal_blocks_cannot_mask_folded_fields,
    test_explicit_indent_leading_character_decoys_are_rejected,
    test_implicit_indent_leading_character_decoys_are_rejected,
)


if __name__ == "__main__":
    failures = 0
    for test in _TESTS:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(_TESTS)} tests failed")
    print(f"All {len(_TESTS)} tests passed.")
