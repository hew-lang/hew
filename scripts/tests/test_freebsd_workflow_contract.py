"""Executable checks for the FreeBSD workflow's platform contract."""

import re
import shlex
from pathlib import Path
from typing import Callable


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "freebsd.yml"
RELEASE_GATE = ROOT / ".github" / "workflows" / "release-gate.yml"

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
PKG_INSTALL = (
    "pkg install -y llvm22 rust cmake ninja git pkgconf libffi libxml2 wasmtime"
)


def _job_block(workflow: str, job_name: str) -> str:
    match = re.search(
        rf"(?ms)^  {re.escape(job_name)}:\n(.*?)(?=^  [A-Za-z0-9_-]+:\n|\Z)",
        workflow,
    )
    assert match is not None, f"missing workflow job: {job_name}"
    return match.group(1)


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


def test_freebsd_workflow_provisions_and_probes_wasi_tools() -> None:
    job = _job_block(WORKFLOW.read_text(), "build-and-test")

    assert job.count(PKG_INSTALL) == 1
    assert job.count("ln -sf /usr/local/llvm22/bin/wasm-ld /usr/local/bin/wasm-ld") == 1
    assert job.count("command -v wasmtime && wasmtime --version") == 1
    assert job.count("command -v wasm-ld && wasm-ld --version") == 1


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


TESTS = (
    test_freebsd_nextest_command_is_exact,
    test_both_release_gate_freebsd_commands_are_exact,
    test_freebsd_workflow_provisions_and_probes_wasi_tools,
    test_added_nightly_exclusion_is_rejected,
    test_aarch64_release_gate_drift_is_rejected,
)


if __name__ == "__main__":
    failures = 0
    for test in TESTS:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(TESTS)} tests failed")
    print(f"All {len(TESTS)} tests passed.")
