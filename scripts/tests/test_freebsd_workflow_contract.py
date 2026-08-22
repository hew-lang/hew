"""Executable checks for the FreeBSD workflow's platform contract."""

import re
import shlex
import subprocess
from pathlib import Path
from typing import Callable


ROOT = Path(__file__).resolve().parents[2]
CI_WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
WORKFLOW = ROOT / ".github" / "workflows" / "freebsd.yml"
RELEASE_GATE = ROOT / ".github" / "workflows" / "release-gate.yml"
RUST_TOOLCHAIN = ROOT / "rust-toolchain.toml"

REQUIRED_CI_JOB = "lint"
REQUIRED_CI_JOB_NAME = "Clippy & format"
CONTRACT_STEP_NAME = "Verify FreeBSD workflow contract"
CONTRACT_COMMAND = "make freebsd-workflow-contract-check"

EXPECTED_NEXTEST_COMMAND = (
    "cargo",
    "nextest",
    "run",
    "--workspace",
    "--exclude",
    "hew-wasm",
    "--exclude",
    "hew-cabi",
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
    "-r",
    "FreeBSD",
)
X86_64_FREEBSD_TOOL_PACKAGES = (
    "llvm22",
    "gdb",
    "rustup-init",
    "python3",
    "cmake",
    "ninja",
    "git",
    "gmake",
    "bash",
    "pkgconf",
    "libffi",
    "libxml2",
    "wasmtime",
)
EXPECTED_X86_64_PKG_INSTALL = (*PKG_INSTALL_PREFIX, *X86_64_FREEBSD_TOOL_PACKAGES)
EXPECTED_X86_64_PKG_PHASES = (
    EXPECTED_PKG_BOOTSTRAP,
    EXPECTED_PKG_UPDATE,
    EXPECTED_X86_64_PKG_INSTALL,
)
AARCH64_FREEBSD_TOOL_PACKAGES = (
    "llvm22",
    "gdb",
    "rust",
    "python3",
    "cmake",
    "ninja",
    "git",
    "gmake",
    "bash",
    "pkgconf",
    "libffi",
    "libxml2",
    "wasmtime",
)
EXPECTED_AARCH64_PKG_INSTALL = (*PKG_INSTALL_PREFIX, *AARCH64_FREEBSD_TOOL_PACKAGES)
EXPECTED_AARCH64_PKG_PHASES = (
    EXPECTED_PKG_BOOTSTRAP,
    EXPECTED_PKG_UPDATE,
    EXPECTED_AARCH64_PKG_INSTALL,
)
PINNED_RUST_TOOLCHAIN = "1.96.0"
EXPECTED_RUSTUP_INIT = (
    "/usr/local/bin/rustup-init",
    "-y",
    "--no-modify-path",
    "--profile",
    "minimal",
    "--default-toolchain",
    PINNED_RUST_TOOLCHAIN,
    "--target",
    "wasm32-wasip1",
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
EXPECTED_BASH_PROBE = (
    "command",
    "-v",
    "bash",
    "&&",
    "bash",
    "--version",
)
EXPECTED_GIT_SAFE_DIRECTORY = (
    "git",
    "config",
    "--global",
    "--add",
    "safe.directory",
    "$(pwd)",
)
EXPECTED_EXACT_REF_CHECK = (
    "test",
    "$(git rev-parse HEAD)",
    "=",
    "$GITHUB_SHA",
)
EXPECTED_LLVM_ENV = ("export", "LLVM_SYS_221_PREFIX=/usr/local/llvm22")
EXPECTED_RUSTUP_PATH_ENV = (
    "export",
    "PATH=$HOME/.cargo/bin:/usr/local/llvm22/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin",
)
EXPECTED_PKG_RUST_PATH_ENV = (
    "export",
    "PATH=/usr/local/llvm22/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin",
)
EXPECTED_RUSTUP_CARGO_ENV = ("export", "CARGO=$HOME/.cargo/bin/cargo")
EXPECTED_PKG_RUST_CARGO_ENV = ("export", "CARGO=/usr/local/bin/cargo")
EXPECTED_PYTHON_ENV = ("export", "PYTHON=/usr/local/bin/python3")
EXPECTED_CARGO_PROBE = ("test", "-x", "$CARGO")
EXPECTED_PYTHON_PROBE = ("test", "-x", "$PYTHON")
EXPECTED_RUSTC_PIN_PROBE = (
    "rustup",
    "run",
    PINNED_RUST_TOOLCHAIN,
    "rustc",
    "--version",
    "|",
    "grep",
    "-q",
    rf"^rustc 1\.96\.0 ",
)
EXPECTED_WASI_TARGET_PROBE = (
    "rustup",
    "target",
    "list",
    "--toolchain",
    PINNED_RUST_TOOLCHAIN,
    "--installed",
    "|",
    "grep",
    "-qx",
    "wasm32-wasip1",
)
EXPECTED_GNU_MAKE_ENV = ("export", "MAKE=gmake")
EXPECTED_VERTICAL_SLICE_GATE = ("gmake", "test-vertical-slice")
EXPECTED_HEW_RATCHET_GATE = ("gmake", "test-hew-ratchet")
FREEBSD_CI_USER = "hew-ci"
EXPECTED_CI_USER_CREATE = (
    "pw",
    "useradd",
    "-n",
    FREEBSD_CI_USER,
    "-m",
    "-s",
    "/bin/sh",
)
EXPECTED_ID_PROBE = ("id",)
EXPECTED_ROOT_UID_ASSERT = ("test", "$(id -u)", "=", "0")
EXPECTED_WORKSPACE_CHOWN = (
    "chown",
    "-R",
    f"{FREEBSD_CI_USER}:{FREEBSD_CI_USER}",
    "$GITHUB_WORKSPACE",
)
EXPECTED_CI_USER_SWITCH = (
    "su",
    "-m",
    FREEBSD_CI_USER,
    "-c",
    "sh -s",
    "<<HEW_FREEBSD_CI",
)
EXPECTED_CI_HOME = ("export", "HOME=/home/hew-ci")
EXPECTED_NON_ROOT_UID_ASSERT = ("test", "$(id -u)", "-ne", "0")
EXPECTED_CI_SCRIPT_END = ("HEW_FREEBSD_CI",)
EXPECTED_AARCH64_STDLIB_BUILD = ("gmake", "stdlib")
EXPECTED_AARCH64_LIBHEW_FRESHNESS_CHECK = ("gmake", "check-libhew-fresh")
EXPECTED_AARCH64_SMOKE_LINK = (
    "target/release/hew",
    "build",
    "_smoke.hew",
    "-o",
    "_smoke_bin",
)
WASI_TOOL_COMMANDS = (
    EXPECTED_PKG_UPDATE,
    EXPECTED_PKG_BOOTSTRAP,
    EXPECTED_X86_64_PKG_INSTALL,
    EXPECTED_WASI_TARGET_PROBE,
    EXPECTED_WASM_LD_LINK,
    EXPECTED_WASMTIME_PROBE,
    EXPECTED_WASM_LD_PROBE,
    EXPECTED_BASH_PROBE,
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
    assert base_indent is not None and base_indent == child_indent + 2, (
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
    step: str,
    replacements: dict[int, tuple[str, ...] | None],
    expected_phases: tuple[tuple[str, ...], ...] = EXPECTED_X86_64_PKG_PHASES,
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
    assert pkg_commands == list(expected_phases)
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


def _expected_pkg_phases(job_name: str) -> tuple[tuple[str, ...], ...]:
    if job_name in ("build-and-test", "gate-freebsd-x86_64"):
        return EXPECTED_X86_64_PKG_PHASES
    return EXPECTED_AARCH64_PKG_PHASES


def _assert_wasi_tool_setup(
    workflow: str,
    job_name: str,
    step_name: str,
) -> None:
    step = _step_block(_job_block(workflow, job_name), step_name)
    prepare_commands = _active_shell_commands(_literal_block(step, "prepare"))
    run_commands = _active_shell_commands(_literal_block(step, "run"))
    expected_phases = _expected_pkg_phases(job_name)

    pkg_commands = [
        command
        for command in prepare_commands
        if command[:1] in (("pkg",), ("/usr/sbin/pkg",))
    ]
    assert pkg_commands == list(expected_phases), (
        f"{job_name} must bootstrap pkg through the base utility from FreeBSD, "
        "refresh only that named repository, then install "
        f"the exact tool set without an automatic update; got {pkg_commands!r}"
    )
    required_commands = [
        EXPECTED_PYTHON_ENV,
        EXPECTED_CARGO_PROBE,
        EXPECTED_PYTHON_PROBE,
        EXPECTED_WASM_LD_LINK,
        EXPECTED_WASMTIME_PROBE,
        EXPECTED_WASM_LD_PROBE,
    ]
    if job_name in ("build-and-test", "gate-freebsd-x86_64"):
        required_commands.extend(
            (
                EXPECTED_RUSTUP_INIT,
                EXPECTED_RUSTUP_PATH_ENV,
                EXPECTED_RUSTUP_CARGO_ENV,
                EXPECTED_RUSTC_PIN_PROBE,
                EXPECTED_WASI_TARGET_PROBE,
            )
        )
    else:
        required_commands.extend(
            (EXPECTED_PKG_RUST_PATH_ENV, EXPECTED_PKG_RUST_CARGO_ENV)
        )
    required_commands.append(EXPECTED_BASH_PROBE)
    for required in required_commands:
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


def _assert_nightly_compiled_hew_authority(workflow: str) -> None:
    job_name = "build-and-test"
    step_name = "Build and test on FreeBSD"
    step = _step_block(_job_block(workflow, job_name), step_name)
    commands = _active_shell_commands(_literal_block(step, "run"))
    required = (
        EXPECTED_GIT_SAFE_DIRECTORY,
        EXPECTED_EXACT_REF_CHECK,
        EXPECTED_LLVM_ENV,
        EXPECTED_GNU_MAKE_ENV,
        EXPECTED_BASH_PROBE,
        EXPECTED_NEXTEST_COMMAND,
        EXPECTED_VERTICAL_SLICE_GATE,
        EXPECTED_HEW_RATCHET_GATE,
    )
    for command in required:
        assert commands.count(command) == 1, (
            "FreeBSD nightly must contain exactly one active command "
            f"{command!r}; got {commands.count(command)}"
        )

    indexes = [commands.index(command) for command in required]
    assert indexes == sorted(indexes), (
        "FreeBSD nightly must authenticate its checkout, establish the LLVM "
        "and GNU make environment, prove the Bash interpreter, run canonical "
        "nextest, then run vertical slice before the Hew ratchet; got indexes "
        f"{indexes!r}"
    )


def _assert_unprivileged_full_suite(
    workflow: str,
    job_name: str,
    step_name: str,
) -> None:
    step = _step_block(_job_block(workflow, job_name), step_name)
    prepare_commands = _active_shell_commands(_literal_block(step, "prepare"))
    run_commands = _active_shell_commands(_literal_block(step, "run"))

    assert prepare_commands.count(EXPECTED_CI_USER_CREATE) == 1, (
        f"{job_name} must create exactly one dedicated FreeBSD CI user"
    )
    for command in (
        EXPECTED_ROOT_UID_ASSERT,
        EXPECTED_WORKSPACE_CHOWN,
        EXPECTED_CI_USER_SWITCH,
        EXPECTED_CI_HOME,
        EXPECTED_NON_ROOT_UID_ASSERT,
        EXPECTED_CI_SCRIPT_END,
    ):
        assert run_commands.count(command) == 1, (
            f"{job_name} must run exactly one active command {command!r}"
        )
    assert run_commands.count(EXPECTED_ID_PROBE) == 2, (
        f"{job_name} must log both the VM login and test user identities"
    )

    root_id, ci_id = [
        index
        for index, command in enumerate(run_commands)
        if command == EXPECTED_ID_PROBE
    ]
    ordered = (
        root_id,
        run_commands.index(EXPECTED_ROOT_UID_ASSERT),
        run_commands.index(EXPECTED_WORKSPACE_CHOWN),
        run_commands.index(EXPECTED_CI_USER_SWITCH),
        run_commands.index(EXPECTED_CI_HOME),
        ci_id,
        run_commands.index(EXPECTED_NON_ROOT_UID_ASSERT),
        run_commands.index(EXPECTED_HEW_RATCHET_GATE),
        run_commands.index(EXPECTED_CI_SCRIPT_END),
    )
    assert list(ordered) == sorted(ordered), (
        f"{job_name} must prove root, transfer workspace ownership, then prove "
        "the full test workload finishes under a non-root user"
    )


def _assert_rejected(check: Callable[[], None]) -> None:
    try:
        check()
    except AssertionError:
        return
    raise AssertionError("mutated workflow contract was accepted")


def test_freebsd_nextest_command_is_exact() -> None:
    _assert_exact_nextest(WORKFLOW.read_text(), "build-and-test")


def test_freebsd_nightly_runs_authenticated_compiled_hew_gates() -> None:
    _assert_nightly_compiled_hew_authority(WORKFLOW.read_text())


def test_full_freebsd_suites_run_as_a_non_root_user() -> None:
    _assert_unprivileged_full_suite(
        WORKFLOW.read_text(),
        "build-and-test",
        "Build and test on FreeBSD",
    )
    _assert_unprivileged_full_suite(
        RELEASE_GATE.read_text(),
        "gate-freebsd-x86_64",
        "Build and test on FreeBSD",
    )


def test_freebsd_user_boundary_cannot_be_removed() -> None:
    for workflow, job_name, step_name in (
        (WORKFLOW.read_text(), "build-and-test", "Build and test on FreeBSD"),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-x86_64",
            "Build and test on FreeBSD",
        ),
    ):
        job = _job_block(workflow, job_name)
        step = _step_block(job, step_name)
        for command_text in (
            "pw useradd -n hew-ci -m -s /bin/sh",
            'test "$(id -u)" = 0',
            'chown -R hew-ci:hew-ci "$GITHUB_WORKSPACE"',
            "su -m hew-ci -c 'sh -s' <<'HEW_FREEBSD_CI'",
            'test "$(id -u)" -ne 0',
        ):
            assert step.count(command_text) == 1
            mutated_step = step.replace(command_text, f"# {command_text}", 1)
            mutated = workflow.replace(step, mutated_step, 1)
            _assert_rejected(
                lambda mutated=mutated, job_name=job_name, step_name=step_name: (
                    _assert_unprivileged_full_suite(mutated, job_name, step_name)
                )
            )


def test_nightly_compiled_hew_commands_cannot_be_commented_out() -> None:
    workflow = WORKFLOW.read_text()
    job_name = "build-and-test"
    step_name = "Build and test on FreeBSD"
    step = _step_block(_job_block(workflow, job_name), step_name)
    for command_text in (
        'test "$(git rev-parse HEAD)" = "$GITHUB_SHA"',
        "export LLVM_SYS_221_PREFIX=/usr/local/llvm22",
        "export MAKE=gmake",
        "command -v bash && bash --version",
        "gmake test-vertical-slice",
        "gmake test-hew-ratchet",
    ):
        assert step.count(command_text) == 1
        mutated_step = step.replace(command_text, f"# {command_text}", 1)
        mutated = workflow.replace(step, mutated_step, 1)
        assert command_text in mutated, (
            "comment mutation must preserve the required raw command text"
        )
        _assert_rejected(
            lambda mutated=mutated: _assert_nightly_compiled_hew_authority(mutated)
        )


def test_nightly_compiled_hew_gate_order_cannot_drift() -> None:
    workflow = WORKFLOW.read_text()
    job_name = "build-and-test"
    step_name = "Build and test on FreeBSD"
    step = _step_block(_job_block(workflow, job_name), step_name)
    vertical = " ".join(EXPECTED_VERTICAL_SLICE_GATE)
    ratchet = " ".join(EXPECTED_HEW_RATCHET_GATE)
    assert step.count(vertical) == 1
    assert step.count(ratchet) == 1
    mutated_step = step.replace(vertical, "__vertical_gate__", 1)
    mutated_step = mutated_step.replace(ratchet, vertical, 1)
    mutated_step = mutated_step.replace("__vertical_gate__", ratchet, 1)
    mutated = workflow.replace(step, mutated_step, 1)
    _assert_rejected(lambda: _assert_nightly_compiled_hew_authority(mutated))


def test_nightly_compiled_hew_gates_cannot_be_reduced_to_nextest() -> None:
    workflow = WORKFLOW.read_text()
    job_name = "build-and-test"
    step_name = "Build and test on FreeBSD"
    step = _step_block(_job_block(workflow, job_name), step_name)
    replacement = "cargo nextest run --workspace --profile ci --no-fail-fast"
    mutated_step = step.replace(
        " ".join(EXPECTED_VERTICAL_SLICE_GATE), replacement, 1
    ).replace(" ".join(EXPECTED_HEW_RATCHET_GATE), replacement, 1)
    mutated = workflow.replace(step, mutated_step, 1)
    _assert_rejected(lambda: _assert_nightly_compiled_hew_authority(mutated))


def test_x86_64_release_gate_command_is_exact() -> None:
    _assert_exact_nextest(RELEASE_GATE.read_text(), "gate-freebsd-x86_64")


def test_aarch64_release_gate_runs_no_nextest_suite() -> None:
    # gate-freebsd-aarch64 intentionally diverges from gate-freebsd-x86_64:
    # under full-system QEMU emulation the shared full-scope workload never
    # completed inside its 180-minute timeout (see the job's header comment
    # in release-gate.yml for the measured evidence). The leg is scoped down
    # to a release build of hew-cli plus a compile+run smoke test; it must
    # not carry any `cargo nextest run` invocation. A future PR reintroducing
    # one must also revert this test consciously.
    arm_job = _job_block(RELEASE_GATE.read_text(), "gate-freebsd-aarch64")
    assert _nextest_commands(arm_job) == []


def test_all_freebsd_jobs_provision_and_probe_wasi_tools() -> None:
    _assert_wasi_tool_setup(
        WORKFLOW.read_text(),
        "build-and-test",
        "Build and test on FreeBSD",
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


def test_x86_64_wasi_target_setup_matches_repository_toolchain() -> None:
    match = re.search(
        r'^channel = "(?P<channel>[^"]+)"$',
        RUST_TOOLCHAIN.read_text(),
        re.MULTILINE,
    )
    assert match is not None
    assert match.group("channel") == PINNED_RUST_TOOLCHAIN

    for workflow, job_name, step_name in (
        (WORKFLOW.read_text(), "build-and-test", "Build and test on FreeBSD"),
        (
            RELEASE_GATE.read_text(),
            "gate-freebsd-x86_64",
            "Build and test on FreeBSD",
        ),
    ):
        step = _step_block(_job_block(workflow, job_name), step_name)
        install = f"--default-toolchain {PINNED_RUST_TOOLCHAIN} --target wasm32-wasip1"
        assert step.count(install) == 1
        mutated_step = step.replace(
            install,
            f"--default-toolchain {PINNED_RUST_TOOLCHAIN} "
            "--target wasm32-unknown-unknown",
            1,
        )
        mutated = workflow.replace(step, mutated_step, 1)
        _assert_rejected(
            lambda mutated=mutated, job_name=job_name, step_name=step_name: (
                _assert_wasi_tool_setup(mutated, job_name, step_name)
            )
        )


def test_required_clippy_job_runs_contract_unconditionally() -> None:
    _assert_required_ci_path(CI_WORKFLOW.read_text())


def test_dispatcher_copy_cannot_mask_required_job_mutation() -> None:
    workflow = CI_WORKFLOW.read_text()
    job = _job_block(workflow, REQUIRED_CI_JOB)
    assert job.count(f"run: {CONTRACT_COMMAND}") == 1
    dispatched = subprocess.run(
        [
            "bash",
            str(ROOT / "scripts/ci-preflight-dispatcher.sh"),
            "--dry-run",
            "--",
            ".github/workflows/freebsd.yml",
        ],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    assert CONTRACT_COMMAND in dispatched, dispatched
    mutated_job = job.replace(
        f"run: {CONTRACT_COMMAND}",
        "run: echo contract-check-removed",
        1,
    )
    mutated = workflow.replace(job, mutated_job, 1)
    assert mutated.count(f"run: {CONTRACT_COMMAND}") == 0
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


def test_nightly_bash_package_removal_is_rejected() -> None:
    workflow = WORKFLOW.read_text()
    job_name = "build-and-test"
    step_name = "Build and test on FreeBSD"
    step = _step_block(_job_block(workflow, job_name), step_name)
    install_text = " ".join(EXPECTED_X86_64_PKG_INSTALL)
    assert step.count(install_text) == 1
    mutated_install = tuple(
        package for package in EXPECTED_X86_64_PKG_INSTALL if package != "bash"
    )
    assert len(mutated_install) + 1 == len(EXPECTED_X86_64_PKG_INSTALL)
    mutated_step = step.replace(install_text, " ".join(mutated_install), 1)
    mutated = workflow.replace(step, mutated_step, 1)
    assert "gmake" in mutated_step and "pkgconf" in mutated_step
    _assert_rejected(lambda: _assert_wasi_tool_setup(mutated, job_name, step_name))


def test_nightly_bash_probe_removal_is_rejected() -> None:
    workflow = WORKFLOW.read_text()
    job_name = "build-and-test"
    step_name = "Build and test on FreeBSD"
    step = _step_block(_job_block(workflow, job_name), step_name)
    probe = " ".join(EXPECTED_BASH_PROBE)
    assert step.count(probe) == 1
    mutated_step = step.replace(probe, "true", 1)
    mutated = workflow.replace(step, mutated_step, 1)
    _assert_rejected(lambda: _assert_nightly_compiled_hew_authority(mutated))
    _assert_rejected(lambda: _assert_wasi_tool_setup(mutated, job_name, step_name))


def test_aarch64_release_gate_stays_scoped_down() -> None:
    # Companion to test_aarch64_release_gate_runs_no_nextest_suite: guards
    # the other expensive commands the aarch64 leg dropped (the release-lib
    # profile rebuild, vertical-slice, the Hew ratchet) from silently
    # reappearing, and that the minimal build+smoke proof it keeps is still
    # present. A PR that wants any of these back must edit this test.
    release_gate = RELEASE_GATE.read_text()
    arm_job = _job_block(release_gate, "gate-freebsd-aarch64")
    for forbidden in (
        "cargo build -p hew-lib",
        "gmake test-vertical-slice",
        "gmake test-hew-ratchet",
        "cargo install cargo-nextest",
    ):
        assert forbidden not in arm_job, (
            f"gate-freebsd-aarch64 must not reintroduce {forbidden!r} without "
            "consciously updating this scope-down contract"
        )
    assert "cargo build -p hew-cli --release" in arm_job
    assert re.search(r"^\s+cpu:\s*4\s*$", arm_job, re.MULTILINE), (
        "gate-freebsd-aarch64 must keep cpu: 4 to use the runner's full core "
        "budget for the emulated build"
    )


def test_aarch64_release_gate_builds_and_checks_libhew_before_smoke() -> None:
    job = _job_block(RELEASE_GATE.read_text(), "gate-freebsd-aarch64")
    step = _step_block(job, "Build and test on FreeBSD aarch64")
    commands = _active_shell_commands(_literal_block(step, "run"))

    for command in (
        EXPECTED_AARCH64_STDLIB_BUILD,
        EXPECTED_AARCH64_LIBHEW_FRESHNESS_CHECK,
        EXPECTED_AARCH64_SMOKE_LINK,
    ):
        assert commands.count(command) == 1, (
            f"gate-freebsd-aarch64 must run exactly one active command {command!r}"
        )

    assert (
        commands.index(EXPECTED_AARCH64_STDLIB_BUILD)
        < commands.index(EXPECTED_AARCH64_LIBHEW_FRESHNESS_CHECK)
        < commands.index(EXPECTED_AARCH64_SMOKE_LINK)
    ), "gate-freebsd-aarch64 must build and verify libhew before its smoke link"


def test_commented_nightly_tool_commands_are_rejected() -> None:
    workflow = WORKFLOW.read_text()
    job_name = "build-and-test"
    step_name = "Build and test on FreeBSD"
    step = _step_block(_job_block(workflow, job_name), step_name)
    for command in WASI_TOOL_COMMANDS:
        command_text = " ".join(command)
        expected_count = EXPECTED_X86_64_PKG_PHASES.count(command) or 1
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
        install_text = " ".join(_expected_pkg_phases(job_name)[2])
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
        other_install = " ".join(_expected_pkg_phases(other_job)[2])
        assert other_install in _job_block(mutated, other_job), (
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
        expected_phases = _expected_pkg_phases(job_name)
        for command_index, command in enumerate(expected_phases):
            command_text = " ".join(command)
            replacements = [
                command_text.replace(" -r FreeBSD", "", 1),
                command_text.replace(" -r FreeBSD", " -r FreeBSD-ports", 1),
            ]
            if command[:2] == ("pkg", "install"):
                replacements.append(command_text.replace(" -y", " -y -U", 1))
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
                    expected_phases,
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
                assert len(mutated_commands) == len(expected_phases)
                for other_index, expected in enumerate(expected_phases):
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
        expected_phases = _expected_pkg_phases(job_name)
        expected_install = expected_phases[2]
        dynamic_self_install = (*PKG_INSTALL_PREFIX, "pkg")
        combined_install = (*PKG_INSTALL_PREFIX, "pkg", *expected_install[3:])
        mutations: tuple[dict[int, tuple[str, ...] | None], ...] = (
            {0: None},
            {1: None},
            {2: None},
            {0: EXPECTED_PKG_UPDATE, 1: EXPECTED_PKG_BOOTSTRAP},
            {1: expected_install, 2: EXPECTED_PKG_UPDATE},
            {0: expected_install, 2: EXPECTED_PKG_BOOTSTRAP},
            {0: dynamic_self_install},
            {0: combined_install, 2: None},
            {0: None, 2: combined_install},
        )
        job = _job_block(workflow, job_name)
        step = _step_block(job, step_name)
        for replacements in mutations:
            mutated_step = _rewrite_pkg_commands(step, replacements, expected_phases)
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
            ("prepare", (_expected_pkg_phases(job_name)[2],)),
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
            ("prepare", (_expected_pkg_phases(job_name)[2],)),
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
            ("prepare", (_expected_pkg_phases(job_name)[2],)),
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
    test_freebsd_nightly_runs_authenticated_compiled_hew_gates,
    test_full_freebsd_suites_run_as_a_non_root_user,
    test_freebsd_user_boundary_cannot_be_removed,
    test_nightly_compiled_hew_commands_cannot_be_commented_out,
    test_nightly_compiled_hew_gate_order_cannot_drift,
    test_nightly_compiled_hew_gates_cannot_be_reduced_to_nextest,
    test_x86_64_release_gate_command_is_exact,
    test_aarch64_release_gate_runs_no_nextest_suite,
    test_all_freebsd_jobs_provision_and_probe_wasi_tools,
    test_x86_64_wasi_target_setup_matches_repository_toolchain,
    test_required_clippy_job_runs_contract_unconditionally,
    test_dispatcher_copy_cannot_mask_required_job_mutation,
    test_required_job_parity_marker_drift_is_rejected,
    test_added_nightly_exclusion_is_rejected,
    test_nightly_bash_package_removal_is_rejected,
    test_nightly_bash_probe_removal_is_rejected,
    test_aarch64_release_gate_stays_scoped_down,
    test_aarch64_release_gate_builds_and_checks_libhew_before_smoke,
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
