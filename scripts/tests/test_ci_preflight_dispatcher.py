import ast
import importlib.util
import json
import os
import re
import shlex
import subprocess
import sys
import tempfile
from collections.abc import Callable
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci-preflight-dispatcher.sh"

# The dispatcher proves the fast-tier derived baselines are current BEFORE it
# warms anything, so a baseline that drifted when main moved fails in seconds
# instead of at the far end of the lane. Its command line is fixed so the
# profile and summary rows stay comparable across runs.
PRECHECK = (
    "make baselines-check BASELINE_TIER=fast BASELINE_GATES=.tmp/preflight-lane.txt"
)


def run_dispatcher(
    *paths: str,
    extra_args: list[str] | None = None,
    env: dict[str, str] | None = None,
    dry_run: bool = True,
    timeout: float | None = None,
    parallelism: int = 16,
) -> subprocess.CompletedProcess[str]:
    extra_args = extra_args or []
    args = ["bash", str(SCRIPT)]
    if dry_run:
        args.append("--dry-run")
    args.extend(extra_args)
    args.extend(["--", *paths])
    run_env = os.environ.copy()
    if env is not None:
        run_env.update(env)
    with tempfile.TemporaryDirectory() as bin_dir:
        fake_nproc = Path(bin_dir) / "nproc"
        fake_nproc.write_text(
            f"#!/bin/sh\nprintf '{parallelism}\\n'\n", encoding="utf-8"
        )
        fake_nproc.chmod(0o755)
        run_env["PATH"] = f"{bin_dir}{os.pathsep}{run_env['PATH']}"
        return subprocess.run(
            args,
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
            env=run_env,
            timeout=timeout,
        )


def run_dispatcher_help() -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["bash", str(SCRIPT), "--help"],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )


def assert_scripts_config_profile(result: subprocess.CompletedProcess[str]) -> None:
    assert result.returncode == 0, result.stderr
    assert "Selected profile: scripts-config" in result.stdout
    assert "make lint" not in result.stdout
    assert "make playground-check" not in result.stdout
    assert "  - cargo fmt --all -- --check" in result.stdout
    assert "  - make freebsd-workflow-contract-check" in result.stdout
    assert "  - make test-release-workflow-contract" in result.stdout
    assert "\n  - make test  " not in result.stdout
    assert "  - make doc-ratchet-selftest" in result.stdout
    assert "make test-codegen" not in result.stdout


def assert_comprehensive_profile(result: subprocess.CompletedProcess[str]) -> None:
    assert result.returncode == 0, result.stderr
    assert "Selected profile: comprehensive" in result.stdout
    assert "  - make test  " in result.stdout


def test_makefile_routes_to_scripts_config_profile() -> None:
    assert_comprehensive_profile(run_dispatcher("Makefile"))


def test_scripts_path_routes_to_scripts_config_profile() -> None:
    assert_scripts_config_profile(run_dispatcher("scripts/foo.sh"))


def test_nextest_config_routes_to_scripts_config_profile() -> None:
    assert_comprehensive_profile(run_dispatcher(".config/nextest.toml"))


def test_workflow_routes_to_scripts_config_profile() -> None:
    assert_scripts_config_profile(run_dispatcher(".github/workflows/ci.yml"))


def test_cargo_toml_routes_to_scripts_config_profile() -> None:
    assert_comprehensive_profile(run_dispatcher("Cargo.toml"))


def test_cargo_lock_routes_to_scripts_config_profile() -> None:
    assert_comprehensive_profile(run_dispatcher("Cargo.lock"))


def test_dot_cargo_config_routes_to_scripts_config_profile() -> None:
    assert_comprehensive_profile(run_dispatcher(".cargo/config.toml"))


def test_rust_toolchain_routes_to_scripts_config_profile() -> None:
    assert_comprehensive_profile(run_dispatcher("rust-toolchain.toml"))


def test_structural_lint_label_matches_dispatched_command_and_ci_bootstraps() -> None:
    local = run_dispatcher("scripts/structural-authority-audit.py")
    assert local.returncode == 0, local.stderr
    assert "  - make structural-lint " in local.stdout, local.stdout
    assert "make structural-lint-bootstrap" not in local.stdout, local.stdout

    fallback = run_dispatcher("Cargo.toml")
    assert fallback.returncode == 0, fallback.stderr
    assert "  - make structural-lint " not in fallback.stdout, fallback.stdout
    assert "  - make lint " in fallback.stdout, fallback.stdout

    makefile = (ROOT / "Makefile").read_text()
    assert re.search(r"^lint:.*\bstructural-lint\b", makefile, re.MULTILINE), makefile

    workflow = (ROOT / ".github/workflows/ci.yml").read_text()
    assert re.search(
        r"name: Provision pinned ast-grep toolchain\s+uses: ./\.github/actions/setup-ast-grep",
        workflow,
    ), "hosted CI must explicitly provision the pinned toolchain"
    assert re.search(
        r"name: Verify structural lint bootstrap contract\s+run: make test-ast-grep-contract test-structural-lint-bootstrap",
        workflow,
    ), "hosted CI must run the bootstrap contract tests"
    assert re.search(
        r"name: Run structural authority lint\s+run: make structural-lint",
        workflow,
    ), "the required parity step must dispatch the labeled cache-only command"


# ---------------------------------------------------------------------------
# Slice 1: Instrumentation & hang-bound tests
# ---------------------------------------------------------------------------


def test_dry_run_shows_budget_annotation_narrow_lane() -> None:
    """--dry-run output must include (budget: Xs) for each command in a narrow lane."""
    result = run_dispatcher("hew-parser/src/lib.rs")
    assert result.returncode == 0, result.stderr
    # The command list section should annotate each command with the narrow budget.
    assert "(budget: 180s)" in result.stdout, (
        f"Expected '(budget: 180s)' in dry-run output for a narrow (parser) lane.\n"
        f"stdout:\n{result.stdout}"
    )


def test_dry_run_shows_budget_annotation_fallback_lane() -> None:
    """--dry-run output must include the fallback budget annotation."""
    # A path that escapes all narrow buckets routes to the fallback lane.
    # Use a path that has no bucket predicate (not any recognised hew-* crate or
    # docs/scripts path) so the else-fallback branch fires.
    result = run_dispatcher("some-unclassified-root-file.txt")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: comprehensive" in result.stdout, result.stdout
    assert "(budget: 600s)" in result.stdout, (
        f"Expected '(budget: 600s)' in dry-run output for fallback lane.\n"
        f"stdout:\n{result.stdout}"
    )


def test_dry_run_scales_every_budget_from_detected_parallelism() -> None:
    """An 8-core host doubles both profile and command-specific baselines."""
    result = run_dispatcher("some-unclassified-root-file.txt", parallelism=8)

    assert result.returncode == 0, result.stderr
    assert result.stdout.count("Host parallelism:") == 1, result.stdout
    assert "Host parallelism: 8 (nproc)" in result.stdout, result.stdout
    assert "max(1, 16 / 8) = 2.00x" in result.stdout, result.stdout
    assert "ceil(baseline * 16 / 8)" in result.stdout, result.stdout
    assert "make lint  (budget: 1890s)" in result.stdout, result.stdout
    assert "make test  (budget: 3060s)" in result.stdout, result.stdout
    assert "make test-hew-ratchet  (budget: 3000s)" in result.stdout, result.stdout
    assert "make test-o2-differential  (budget: 5400s)" in result.stdout, result.stdout

    fast_result = run_dispatcher("some-unclassified-root-file.txt", parallelism=32)
    assert fast_result.returncode == 0, fast_result.stderr
    assert "max(1, 16 / 32) = 1.00x" in fast_result.stdout, fast_result.stdout
    assert "make lint  (budget: 945s)" in fast_result.stdout, fast_result.stdout
    assert "make test  (budget: 1530s)" in fast_result.stdout, fast_result.stdout
    assert "make test-hew-ratchet  (budget: 1500s)" in fast_result.stdout, (
        fast_result.stdout
    )


def test_help_includes_profile_json() -> None:
    """--help must document the --profile-json flag."""
    result = run_dispatcher_help()
    assert result.returncode == 0, result.stderr
    assert "--profile-json" in result.stdout, (
        f"Expected '--profile-json' in --help output.\nstdout:\n{result.stdout}"
    )


def test_profile_json_flag_accepted_in_dry_run() -> None:
    """--profile-json is accepted alongside --dry-run without error."""
    result = run_dispatcher(
        "hew-parser/src/lib.rs",
        extra_args=["--profile-json", "/dev/null"],
    )
    assert result.returncode == 0, (
        f"Expected exit 0, got {result.returncode}.\nstderr:\n{result.stderr}"
    )


def test_help_includes_fail_fast_and_run_all_default() -> None:
    """--help documents the default run-all policy and the --fail-fast override."""
    result = run_dispatcher_help()
    assert result.returncode == 0, result.stderr
    assert "--fail-fast" in result.stdout, result.stdout
    assert "all selected commands run" in result.stdout, result.stdout


def test_dry_run_reports_run_all_default_policy() -> None:
    """Dry-run output makes the default run-all policy explicit."""
    result = run_dispatcher("hew-parser/src/lib.rs")
    assert result.returncode == 0, result.stderr
    assert "Failure policy: run-all (default)" in result.stdout, result.stdout


def test_run_all_continues_after_failure_and_profiles_all_commands() -> None:
    """Default policy runs all overridden commands and reports every result."""
    with tempfile.NamedTemporaryFile() as profile:
        result = run_dispatcher(
            "Makefile",
            extra_args=["--profile-json", profile.name],
            env={
                "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
                "PREFLIGHT_TEST_COMMANDS": (
                    "exit 7\n"
                    "printf '%s' RUN_TWO >/dev/null\n"
                    "printf '%s' RUN_THREE >/dev/null\n"
                ),
                "PREFLIGHT_TEST_WARMUP_COMMANDS": "true",
            },
            dry_run=False,
            timeout=30,
        )
        assert result.returncode == 1, result.stdout
        assert "Failure policy: run-all (default)" in result.stdout, result.stdout
        assert "Stopping after first failed command" not in result.stdout, result.stdout
        profile_entries = json.loads(Path(profile.name).read_text())

    assert [entry["cmd"] for entry in profile_entries] == [
        PRECHECK,
        "true",
        "exit 7",
        "printf '%s' RUN_TWO >/dev/null",
        "printf '%s' RUN_THREE >/dev/null",
    ]
    assert [entry["phase"] for entry in profile_entries] == [
        "precheck",
        "warm-up",
        "command",
        "command",
        "command",
    ]
    assert [entry["status"] for entry in profile_entries] == [0, 0, 7, 0, 0]
    assert "    exit 7" in result.stdout, result.stdout
    assert "    printf '%s' RUN_TWO >/dev/null" in result.stdout, result.stdout
    assert "    printf '%s' RUN_THREE >/dev/null" in result.stdout, result.stdout


def test_fail_fast_stops_after_first_failure_and_profiles_only_run_commands() -> None:
    """--fail-fast stops after the first failing command and keeps prior summary data."""
    with tempfile.NamedTemporaryFile() as profile:
        result = run_dispatcher(
            "Makefile",
            extra_args=["--fail-fast", "--profile-json", profile.name],
            env={
                "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
                "PREFLIGHT_TEST_COMMANDS": (
                    "exit 7\n"
                    "printf '%s' RUN_TWO >/dev/null\n"
                    "printf '%s' RUN_THREE >/dev/null\n"
                ),
                "PREFLIGHT_TEST_WARMUP_COMMANDS": "true",
            },
            dry_run=False,
            timeout=30,
        )
        assert result.returncode == 1, result.stdout
        assert "Failure policy: fail-fast" in result.stdout, result.stdout
        assert "Stopping after first failed command (--fail-fast)." in result.stdout, (
            result.stdout
        )
        profile_entries = json.loads(Path(profile.name).read_text())

    assert [entry["cmd"] for entry in profile_entries] == [PRECHECK, "true", "exit 7"]
    assert [entry["phase"] for entry in profile_entries] == [
        "precheck",
        "warm-up",
        "command",
    ]
    assert [entry["status"] for entry in profile_entries] == [0, 0, 7]
    assert "    exit 7" in result.stdout, result.stdout
    summary = result.stdout.split("==> Preflight summary", 1)[1]
    assert "RUN_TWO" not in summary, result.stdout
    assert "RUN_THREE" not in summary, result.stdout
    assert "remaining commands not run" in result.stdout, result.stdout


def test_override_without_sentinel_is_rejected() -> None:
    """PREFLIGHT_TEST_COMMANDS alone hard-fails before the dispatcher banner."""
    result = run_dispatcher(
        "Makefile",
        env={"PREFLIGHT_TEST_COMMANDS": "true"},
    )
    assert result.returncode != 0, result.stdout
    assert "PREFLIGHT_TEST_COMMANDS" in result.stderr, result.stderr
    assert "PREFLIGHT_TEST_ALLOW_OVERRIDE=1" in result.stderr, result.stderr
    assert "unset PREFLIGHT_TEST_COMMANDS" in result.stderr, result.stderr
    assert "==> Hew CI preflight dispatcher" not in result.stdout, result.stdout


def test_override_with_sentinel_emits_stderr_warning() -> None:
    """The test-only override requires a sentinel and emits a warning on stderr."""
    result = run_dispatcher(
        "Makefile",
        env={
            "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
            "PREFLIGHT_TEST_COMMANDS": "printf '%s' OVERRIDE_OK >/dev/null",
        },
    )
    assert result.returncode == 0, result.stderr
    assert "warning:" in result.stderr, result.stderr
    assert "PREFLIGHT_TEST_COMMANDS" in result.stderr, result.stderr
    assert "test-only" in result.stderr, result.stderr
    assert "==> Hew CI preflight dispatcher" in result.stdout, result.stdout
    assert "  - printf '%s' OVERRIDE_OK >/dev/null  (budget: 600s)" in result.stdout, (
        result.stdout
    )


def test_synthetic_timeout_via_run_loop() -> None:
    """A synthetic long-running command times out through the dispatcher loop."""
    with tempfile.NamedTemporaryFile() as profile:
        result = run_dispatcher(
            "hew-parser/src/lib.rs",
            extra_args=["--profile-json", profile.name],
            env={
                "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
                "PREFLIGHT_TEST_COMMANDS": "sleep 30",
                "PREFLIGHT_TEST_WARMUP_COMMANDS": "true",
                "PREFLIGHT_TIMEOUT_NARROW": "1",
            },
            dry_run=False,
            timeout=30,
        )
        profile_entries = json.loads(Path(profile.name).read_text())

    assert result.returncode != 0, result.stdout
    assert "TIMEOUT: 'sleep 30' exceeded 1s budget" in result.stdout, result.stdout
    assert [entry["phase"] for entry in profile_entries] == [
        "precheck",
        "warm-up",
        "command",
    ]
    assert profile_entries[2]["cmd"] == "sleep 30", profile_entries
    assert profile_entries[2]["status"] in {137, 143}, profile_entries
    assert 1 <= profile_entries[2]["elapsed_s"] <= 10, profile_entries
    summary = result.stdout.split("==> Preflight summary", 1)[1]
    assert "sleep 30" in summary, result.stdout
    assert "[FAILED]" in summary, result.stdout


def test_profile_json_records_elapsed_for_each_command() -> None:
    """Profile JSON records elapsed_s for each overridden command."""
    with tempfile.NamedTemporaryFile() as profile:
        result = run_dispatcher(
            "Makefile",
            extra_args=["--profile-json", profile.name],
            env={
                "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
                "PREFLIGHT_TEST_COMMANDS": "true\nfalse\ntrue",
                "PREFLIGHT_TEST_WARMUP_COMMANDS": "true",
            },
            dry_run=False,
            timeout=30,
        )
        profile_entries = json.loads(Path(profile.name).read_text())

    assert result.returncode == 1, result.stdout
    assert [entry["cmd"] for entry in profile_entries] == [
        PRECHECK,
        "true",
        "true",
        "false",
    ]
    assert [entry["phase"] for entry in profile_entries] == [
        "precheck",
        "warm-up",
        "command",
        "command",
    ]
    assert [entry["status"] for entry in profile_entries] == [0, 0, 0, 1]
    for entry in profile_entries:
        assert isinstance(entry["elapsed_s"], int), profile_entries
        assert entry["elapsed_s"] >= 0, profile_entries


def test_baseline_precheck_is_listed_before_warmup() -> None:
    """Derived-baseline staleness must be findable in minutes, not at lane end."""
    result = run_dispatcher("some-unclassified-root-file.txt")
    assert result.returncode == 0, result.stderr
    assert "Baseline precheck:" in result.stdout, result.stdout
    assert f"  - {PRECHECK}" in result.stdout, result.stdout
    assert result.stdout.index("Baseline precheck:") < result.stdout.index(
        "Warm-up:"
    ), result.stdout
    assert result.stdout.index("Baseline precheck:") < result.stdout.index(
        "Commands:"
    ), result.stdout


def test_baseline_precheck_is_scoped_to_the_lane() -> None:
    """A docs lane must not pay for baselines none of its gates compare against."""
    result = run_dispatcher("README.md")
    assert result.returncode == 0, result.stderr
    lane = [
        line.removeprefix("  - ").split("  (budget")[0]
        for line in result.stdout.splitlines()
        if line.startswith("  - ")
    ]
    scoped = subprocess.run(
        [
            "python3",
            "scripts/baselines.py",
            "check",
            "--tier",
            "fast",
            *[arg for command in lane for arg in ("--relevant-to", command)],
        ],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    assert scoped.returncode == 0, scoped.stdout + scoped.stderr
    assert "no members selected" in scoped.stdout, scoped.stdout


def test_compile_warmup_runs_first_and_has_a_summary_row() -> None:
    """Compile profiles warm artifacts before their watchdog-timed commands."""
    result = run_dispatcher(
        "hew-parser/src/lib.rs",
        env={
            "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
            "PREFLIGHT_TEST_WARMUP_COMMANDS": "printf '%s\\n' WARMUP_RAN",
            "PREFLIGHT_TEST_COMMANDS": "printf '%s\\n' GATE_RAN",
        },
        dry_run=False,
        timeout=30,
    )

    assert result.returncode == 0, result.stderr
    warmup_start = result.stdout.index("==> warm-up")
    gate_start = result.stdout.index("==> printf '%s\\n' GATE_RAN")
    summary = result.stdout.split("==> Preflight summary", 1)[1]
    assert "WARMUP_RAN" in result.stdout, result.stdout
    assert warmup_start < gate_start, result.stdout
    assert "warm-up  " in summary, result.stdout
    assert summary.index("warm-up  ") < summary.index("GATE_RAN"), result.stdout


def test_rust_diff_derives_its_warmup_artifacts_before_commands() -> None:
    """A parser diff warms only the artifacts its selected commands need."""
    result = run_dispatcher("hew-parser/src/lib.rs")

    assert result.returncode == 0, result.stderr
    packages = (
        "-p hew-analysis -p hew-cli -p hew-codegen-rs -p hew-compile -p hew-hir "
        "-p hew-lsp -p hew-mir -p hew-parser -p hew-sandbox-wasm -p hew-types "
        "-p hew-wasm -p xtask"
    )
    warmup = result.stdout.split("Warm-up:\n", 1)[1].split("Commands:\n", 1)[0]
    assert warmup == (
        f"  - cargo clippy {packages} --tests\n"
        "  - make hew-native-build wasm-runtime-build\n"
        f"  - cargo nextest run --profile ci {packages} --no-run\n"
    ), result.stdout
    assert result.stdout.index("Warm-up:\n") < result.stdout.index("Commands:\n")


def test_docs_diff_has_no_warmup_block() -> None:
    """The docs gates declare an empty build form, so nothing is warmed."""
    result = run_dispatcher("docs/hew-language-guide.md")

    assert result.returncode == 0, result.stderr
    assert "Warm-up:\n" not in result.stdout, result.stdout
    makefile = (ROOT / "Makefile").read_text()
    assert "\ndoc-ratchet-selftest-build:\n\t@:\n" in makefile, makefile


def test_no_lane_warms_test_targets_with_all_targets() -> None:
    """`cargo build --all-targets` cannot build a test harness under panic=abort.

    The root `[profile.dev] panic = "abort"` makes a combined lib+test build
    fail ("requires panic strategy abort which is incompatible with this
    crate's strategy of unwind"), which is how this warm-up turned main red.
    Test binaries are warmed the way `make test` builds them: `--no-run`.
    """
    for path in (
        "hew-parser/src/lib.rs",
        "hew-runtime/src/lib.rs",
        "hew-codegen-rs/src/lib.rs",
        "hew-observe/src/lib.rs",
        "Makefile",
    ):
        result = run_dispatcher(path)
        assert result.returncode == 0, result.stderr
        warmup = result.stdout.split("Warm-up:\n", 1)
        if len(warmup) == 1:
            continue
        warmup = warmup[1].split("Commands:\n", 1)[0]
        assert "--all-targets" not in warmup, (path, result.stdout)


def test_comprehensive_warms_every_gate_through_its_own_build_form() -> None:
    """The comprehensive lane warms `make lint` and `make test` by name.

    The warm-up is the gate's own `<target>-build`, so the clippy and nextest
    invocations live once each, next to the gate that runs them.
    """
    result = run_dispatcher("Makefile")

    assert result.returncode == 0, result.stderr
    warmup = result.stdout.split("Warm-up:\n", 1)[1].split("Commands:\n", 1)[0]
    assert "  - make lint-build\n" in warmup, result.stdout
    assert "  - make test-build\n" in warmup, result.stdout
    assert "  - make test-cabi-build\n" in warmup, result.stdout
    assert "  - make test-compiler-pipeline-build\n" in warmup, result.stdout
    assert "  - make sandbox-parity-build\n" in warmup, result.stdout

    makefile = (ROOT / "Makefile").read_text()
    assert (
        "\ntest-build: wasm-runtime runtime $(LIBHEW_READY)\n\tcargo nextest run --workspace --exclude hew-cabi --profile ci --no-run\n"
        in makefile
    ), "make test's build form must build its binaries the way make test does"
    assert (
        "\nlint-build: structural-lint-bootstrap-install\n\tcargo clippy --workspace --tests\n"
        in makefile
    ), "make lint's build form must warm through clippy, without -D warnings"


def _selected_commands(stdout: str) -> list[str]:
    body = stdout.split("Commands:\n", 1)[1]
    commands = []
    for line in body.splitlines():
        if not line.startswith("  - "):
            break
        commands.append(line[len("  - ") :].split("  (budget:")[0])
    return commands


def _warmup_commands(stdout: str) -> list[str]:
    if "Warm-up:\n" not in stdout:
        return []
    body = stdout.split("Warm-up:\n", 1)[1].split("Commands:\n", 1)[0]
    return [
        line[len("  - ") :] for line in body.splitlines() if line.startswith("  - ")
    ]


# One representative changed path per routing lane. Every lane must derive a
# warm-up for every command it selects; a lane that cannot is a dispatcher that
# dies before running anything, which is the point of the derivation being
# fail-closed.
LANE_PROBES = {
    "docs-only": "docs/hew-language-guide.md",
    "scripts-config": "scripts/foo.sh",
    "grammar": "docs/specs/Hew.g4",
    "parser": "hew-parser/src/lib.rs",
    "types": "hew-types/src/lib.rs",
    "cli": "hew-cli/src/main.rs",
    "compiler-pipeline": "hew-mir/src/lib.rs",
    "vertical-slice": "tests/vertical-slice/accept/foo.hew",
    "observe": "hew-observe/src/lib.rs",
    "runtime-testkit": "hew-runtime-testkit/src/lib.rs",
    "hew-tests": "tests/hew/foo.hew",
    "runtime-net": "hew-runtime/src/lib.rs",
    "wasm": "hew-wasm/src/lib.rs",
    "comprehensive": "Cargo.toml",
}


def test_every_lane_derives_a_warmup_for_every_command_it_selects() -> None:
    for profile, path in LANE_PROBES.items():
        result = run_dispatcher(path)
        assert result.returncode == 0, (profile, path, result.stderr)
        assert f"Selected profile: {profile}" in result.stdout, (path, result.stdout)


def explain_warmup(command: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["bash", str(SCRIPT), "--explain-warmup", command],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )


def test_a_gate_with_no_derivable_warmup_is_fatal() -> None:
    """The fallback used to warm everything and print a note; it now dies.

    Warming the full artifact set for an unmapped command is how a warm-up
    build diverged from its gate's build in the first place: the fallback was
    reached silently, so nothing forced the new gate to declare how it builds.
    """
    result = explain_warmup("scripts/some-new-gate.sh")

    assert result.returncode != 0, result.stdout
    assert "has no derivable warm-up" in result.stderr, result.stderr
    assert "full warm-up set" not in result.stdout, result.stdout


def test_a_make_gate_without_a_build_form_is_fatal() -> None:
    result = explain_warmup("make hew-lsp")

    assert result.returncode != 0, result.stdout
    assert "declare 'hew-lsp-build' next to 'hew-lsp'" in result.stderr, result.stderr


def test_a_make_gate_naming_an_undeclared_target_is_fatal() -> None:
    """The `grammar` target was dispatched for months after it was deleted."""
    result = explain_warmup("make grammar")

    assert result.returncode != 0, result.stdout
    assert "undeclared make target 'grammar'" in result.stderr, result.stderr


def test_a_nextest_gate_derives_its_own_invocation_with_no_run() -> None:
    result = explain_warmup("cargo nextest run --profile ci -p hew-mir")

    assert result.returncode == 0, result.stderr
    assert result.stdout == "cargo nextest run --profile ci -p hew-mir --no-run\n", (
        result.stdout
    )


def test_a_clippy_gate_derives_its_own_invocation_without_the_deny_flag() -> None:
    """A lint failure must be the timed gate's verdict, not an aborted warm-up."""
    result = explain_warmup("cargo clippy -p hew-mir --tests -- -D warnings")

    assert result.returncode == 0, result.stderr
    assert result.stdout == "cargo clippy -p hew-mir --tests\n", result.stdout


def test_a_fmt_gate_warms_nothing() -> None:
    result = explain_warmup("cargo fmt --all -- --check")

    assert result.returncode == 0, result.stderr
    assert result.stdout == "", result.stdout


def test_every_dispatched_make_target_exists_in_the_makefile() -> None:
    makefile = (ROOT / "Makefile").read_text()
    declared = set()
    for line in makefile.splitlines():
        match = re.match(r"^([A-Za-z0-9_][A-Za-z0-9_ .$()-]*):([^=]|$)", line)
        if match:
            declared.update(match.group(1).split())
    for path in LANE_PROBES.values():
        result = run_dispatcher(path)
        assert result.returncode == 0, (path, result.stderr)
        for command in _selected_commands(result.stdout):
            if not command.startswith("make "):
                continue
            for target in command[len("make ") :].split():
                assert target in declared, (path, command, target)


def test_no_warmup_carries_a_flag_its_gate_does_not() -> None:
    """A warm-up is its gate's command, minus execution — never a second build.

    The 2026-08-20 outage was a warm-up that carried `--all-targets` while its
    gate carried `--exclude hew-cabi`: two builds, one target dir, two
    incompatible `serde_core` rlibs. Only `--no-run` may be added.
    """
    for path in LANE_PROBES.values():
        result = run_dispatcher(path)
        assert result.returncode == 0, (path, result.stderr)
        gate_flags = set()
        for command in _selected_commands(result.stdout):
            gate_flags.update(
                token for token in command.split() if token.startswith("-")
            )
        for warmup in _warmup_commands(result.stdout):
            if warmup.startswith("make "):
                continue
            for token in warmup.split():
                if not token.startswith("-") or token == "--no-run":
                    continue
                assert token in gate_flags, (path, warmup, token)


def _make_plan(*targets: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["make", "--always-make", "--dry-run", *targets],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )


def _dispatchable_make_targets() -> set[str]:
    source = SCRIPT.read_text()
    return {
        target
        for command in re.findall(r'add_command "make ([^"]+)"', source)
        for target in command.split()
    }


def _gates_with_a_build_form() -> set[str]:
    """Every gate the Makefile declares a warm-up form for.

    Wider than the set the dispatcher selects today: a form is declared before
    its gate is dispatched by name, and an undeclared-but-wrong form is exactly
    what these checks exist to catch before it warms anything.
    """
    makefile = (ROOT / "Makefile").read_text()
    return set(re.findall(r"^([A-Za-z0-9][\w.-]*)-build:", makefile, re.MULTILINE))


def _checkable_gates() -> list[str]:
    return sorted(_dispatchable_make_targets() | _gates_with_a_build_form())


_COMPILING_COMMAND = re.compile(r"\bcargo\s+(?:build|run|test|nextest|clippy)\b")
_SCRIPT_REF = re.compile(r"(?:^|[\s\"'=])((?:scripts|tests)/[\w./-]+\.(?:sh|py))")
_MAKE_CALL = re.compile(r"(?:^|[\s;&|(\"'])(?:\$\(MAKE\)|make)\s+([A-Za-z][\w.-]*)")
_RUNS_A_SUBPROCESS = re.compile(
    r"subprocess\.|check_call|check_output|os\.system|Popen"
)
_PYTHON_CARGO = re.compile(r"\bcargo\b[\s\"',\]]*\b(?:build|run|test|nextest|clippy)\b")


def _dotted_name(node: ast.AST) -> str | None:
    """`subprocess.run` for the call `subprocess.run(...)`, else None."""
    parts: list[str] = []
    while isinstance(node, ast.Attribute):
        parts.append(node.attr)
        node = node.value
    if not isinstance(node, ast.Name):
        return None
    parts.append(node.id)
    return ".".join(reversed(parts))


def _literal_argv(call: ast.Call) -> list[str] | None:
    """The argv a subprocess call hands out, as far as it is written literally.

    A non-literal element (an f-string, a variable, a path built at runtime)
    becomes `...`: the command's shape is what the walk reads, and losing one
    argument must not lose the `cargo build` in front of it.
    """
    if not call.args:
        return None
    first = call.args[0]
    if isinstance(first, ast.Constant) and isinstance(first.value, str):
        try:
            return shlex.split(first.value)
        except ValueError:
            return first.value.split()
    if not isinstance(first, (ast.List, ast.Tuple)):
        return None
    argv: list[str] = []
    for element in first.elts:
        if isinstance(element, ast.Constant) and isinstance(element.value, str):
            argv.append(element.value)
        elif isinstance(element, ast.Constant):
            argv.append(str(element.value))
        else:
            argv.append("...")
    return argv


def _python_commands(text: str) -> list[str]:
    """Every subprocess argv a Python script writes out, as a command line.

    Reading a Python gate line by line only ever sees a compile that fits on
    one line. An argv past a few arguments is written over several — the
    `subprocess.run([` opener on one line and `"cargo",` on the next — so
    neither line carries both halves the line rule asks for and the compile is
    invisible. Parsing the source recovers the argv whatever its layout, and
    the recovered argv is a command line like any other: the shell rules for
    compiles, scripts, and `make` targets then apply to it unchanged.
    """
    try:
        tree = ast.parse(text)
    except SyntaxError:
        return []
    commands: list[str] = []
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        name = _dotted_name(node.func)
        if name is None or not _RUNS_A_SUBPROCESS.search(name):
            continue
        argv = _literal_argv(node)
        if argv:
            commands.append(shlex.join(argv))
    return commands


def _is_message(line: str) -> bool:
    """Text a script prints is not a command a script runs.

    Every one of these scripts ends its "binary missing" branch with
    `echo "Run: cargo build -p hew-cli"`; counting those would flag every gate
    in the tree and the check would mean nothing.
    """
    return line.startswith("#") or "echo" in line or "printf" in line


def compiling_evidence(
    plan: str,
    read_plan: "Callable[[str], str | None]",
    read_script: "Callable[[str], str | None]",
) -> str | None:
    """The first compile reachable from a gate's plan, at any depth.

    A gate's own `make --dry-run` plan shows its recipe and its prerequisites'
    recipes; it does not show what the scripts in that recipe do, and it does
    not show what a `make` those scripts run would do. Both hide compiles:
    `test-stdlib-execution-proofs` reaches a `cargo run` one script deep, and
    nothing stops the next one from reaching it through a script that calls
    another target. The walk follows plan -> script -> `make <target>` -> that target's plan
    until it runs out of new nodes.

    For a shell script a bare cargo line is an invocation. For a Python script
    it is usually data — a fixture, an error message, an assertion — so the
    source is parsed and the argv it hands to a subprocess is what counts,
    however many lines that argv is spread over.
    """
    seen_scripts: set[str] = set()
    seen_targets: set[str] = set()
    queue: list[tuple[str, str, bool]] = [("plan", plan, False)]

    def follow(line: str) -> None:
        for script in _SCRIPT_REF.findall(line):
            if script in seen_scripts:
                continue
            seen_scripts.add(script)
            body = read_script(script)
            if body is not None:
                queue.append((script, body, script.endswith(".py")))
        for target in _MAKE_CALL.findall(line):
            if target in seen_targets:
                continue
            seen_targets.add(target)
            body = read_plan(target)
            if body is not None:
                queue.append((f"make {target}", body, False))

    while queue:
        origin, text, python = queue.pop(0)
        if python:
            # An argv recovered from the source is a command line, whatever its
            # layout, so it is read with the shell rules. The line rule below
            # cannot see a multi-line argv at all: `subprocess.run([` and
            # `"cargo",` are different lines and neither one satisfies it.
            for command in _python_commands(text):
                if _COMPILING_COMMAND.search(command):
                    return f"{origin}: {command}"
                follow(command)
        for raw in text.splitlines():
            line = raw.strip()
            if _is_message(line):
                continue
            if python:
                # In a Python gate a cargo string is usually data — a fixture,
                # an assertion, an error message. Only a line that hands work
                # to a subprocess is running anything, and the same rule
                # applies to a `make` it names.
                if not _RUNS_A_SUBPROCESS.search(line):
                    continue
                if _PYTHON_CARGO.search(line):
                    return f"{origin}: {line}"
            elif _COMPILING_COMMAND.search(line):
                return f"{origin}: {line}"
            follow(line)
    return None


def _repo_plan_reader(target: str) -> str | None:
    result = _make_plan(target)
    return result.stdout if result.returncode == 0 else None


def _repo_script_reader(script: str) -> str | None:
    path = ROOT / script
    return path.read_text() if path.exists() else None


def _plans_work(target: str) -> bool:
    result = _make_plan(target)
    assert result.returncode == 0, (target, result.stderr)
    return any(
        line.strip() not in ("", ":") and not line.startswith("make")
        for line in result.stdout.splitlines()
    )


def test_no_gate_compiles_behind_an_empty_build_form() -> None:
    """A gate that compiles must warm that compile, however deep it hides.

    `make test-stdlib-execution-proofs` builds nothing in its own recipe and
    then shells out to a script that runs `cargo run -p hew-parser --example
    stdlib_import_authority`. Declaring the build form empty on the strength of
    the recipe alone puts that build back inside the timed gate — the same
    reading error as the warm-up that diverged from its gate.
    """
    for target in _checkable_gates():
        plan = _make_plan(target)
        assert plan.returncode == 0, (target, plan.stderr)
        evidence = compiling_evidence(
            plan.stdout, _repo_plan_reader, _repo_script_reader
        )
        if evidence is None:
            continue
        assert _plans_work(f"{target}-build"), (
            f"make {target} compiles ({evidence}) but {target}-build declares "
            f"that it builds nothing"
        )


_BUILD_SELECTOR = re.compile(r"--(?:test|bin|example)[= ]([A-Za-z0-9_-]+)")


def _named_binaries(plan: str) -> set[str]:
    return {
        name
        for line in plan.splitlines()
        if not _is_message(line.strip())
        for name in _BUILD_SELECTOR.findall(line)
    }


def test_a_build_form_names_every_binary_its_gate_runs() -> None:
    """Naming one of a gate's two test binaries leaves the other to the gate.

    A gate whose recipe runs two nextest binaries, with a build form that
    spells out only the first, warms half of itself: the second compiles inside
    the timed budget while the summary reports a warm run. A partial warm-up
    reads as a warm one, which is the failure mode this derivation removes.
    """
    for target in _checkable_gates():
        gate = _make_plan(target)
        assert gate.returncode == 0, (target, gate.stderr)
        build = _make_plan(f"{target}-build")
        assert build.returncode == 0, (target, build.stderr)
        missing = _named_binaries(gate.stdout) - _named_binaries(build.stdout)
        assert not missing, (
            f"make {target} runs {sorted(missing)}, which {target}-build does not build"
        )


def test_the_binary_check_sees_a_half_named_build_form() -> None:
    gate = "\tcargo nextest run -p hew-cli --test first --test second\n"
    form = "\tcargo nextest run -p hew-cli --test first --no-run\n"
    assert _named_binaries(gate) - _named_binaries(form) == {"second"}


def test_the_compile_walk_sees_a_compile_one_script_deep() -> None:
    scripts = {"scripts/proof.sh": "cargo run -p hew-parser --example authority\n"}
    evidence = compiling_evidence(
        "\tscripts/proof.sh --check\n",
        lambda target: None,
        scripts.get,
    )
    assert (
        evidence == "scripts/proof.sh: cargo run -p hew-parser --example authority"
    ), evidence


def test_the_compile_walk_sees_a_compile_two_levels_deep() -> None:
    """A script that calls make whose target runs another script that compiles."""
    scripts = {
        "scripts/outer.sh": "make inner-gate\n",
        "scripts/inner.sh": "cargo build -p hew-lib\n",
    }
    plans = {"inner-gate": "\tscripts/inner.sh\n"}
    evidence = compiling_evidence(
        "\tscripts/outer.sh\n",
        plans.get,
        scripts.get,
    )
    assert evidence == "scripts/inner.sh: cargo build -p hew-lib", evidence


def test_the_compile_walk_does_not_count_printed_text() -> None:
    scripts = {"scripts/hint.sh": '  echo "Run: cargo build -p hew-cli" >&2\n'}
    assert (
        compiling_evidence("\tscripts/hint.sh\n", lambda target: None, scripts.get)
        is None
    )


def test_the_compile_walk_reads_python_data_as_data() -> None:
    scripts = {
        "scripts/fixture.py": '    RECIPES = {"lint": "cargo clippy --workspace"}\n',
        "scripts/runner.py": '    subprocess.run(["cargo", "build", "-p", "hew-lib"])\n',
    }
    assert (
        compiling_evidence("\tscripts/fixture.py\n", lambda target: None, scripts.get)
        is None
    )
    assert (
        compiling_evidence("\tscripts/runner.py\n", lambda target: None, scripts.get)
        is not None
    )


def test_the_compile_walk_sees_a_multiline_python_subprocess() -> None:
    """An argv past a few arguments is written over several lines.

    `subprocess.run([` and `"cargo",` land on different lines, so a rule that
    asks one line for both the subprocess call and the cargo command finds
    neither and the gate reads as compiling nothing.
    """
    scripts = {
        "scripts/runner.py": (
            "subprocess.run(\n"
            '    [\n        "cargo",\n        "build",\n'
            '        "-p",\n        "hew-lib",\n    ],\n'
            "    check=True,\n)\n"
        )
    }
    evidence = compiling_evidence(
        "\tscripts/runner.py\n", lambda target: None, scripts.get
    )
    assert evidence == "scripts/runner.py: cargo build -p hew-lib", evidence


def test_the_compile_walk_sees_a_python_compile_behind_a_runtime_argument() -> None:
    """One argument built at runtime must not hide the compile in front of it."""
    scripts = {
        "scripts/runner.py": (
            "subprocess.check_call(\n"
            '    ["cargo", "run", "-p", "hew-cli", "--example", name]\n'
            ")\n"
        )
    }
    evidence = compiling_evidence(
        "\tscripts/runner.py\n", lambda target: None, scripts.get
    )
    assert evidence == "scripts/runner.py: cargo run -p hew-cli --example ...", evidence


def test_the_compile_walk_follows_a_multiline_python_make_call() -> None:
    scripts = {
        "scripts/runner.py": (
            'subprocess.run(\n    [\n        "make",\n        "inner-gate",\n    ]\n)\n'
        )
    }
    plans = {"inner-gate": "\tcargo build -p hew-lib\n"}
    evidence = compiling_evidence("\tscripts/runner.py\n", plans.get, scripts.get)
    assert evidence == "make inner-gate: cargo build -p hew-lib", evidence


def test_the_compile_walk_reads_a_multiline_python_list_as_data() -> None:
    """Parsing must not turn every cargo string in a Python file into a compile."""
    scripts = {
        "scripts/fixture.py": (
            "RECIPES = [\n"
            '    "cargo",\n    "clippy",\n    "--workspace",\n'
            "]\n"
            'MESSAGE = "run cargo build -p hew-cli first"\n'
        )
    }
    assert (
        compiling_evidence("\tscripts/fixture.py\n", lambda target: None, scripts.get)
        is None
    )


def test_the_compile_walk_still_reads_an_unparsable_python_script() -> None:
    """A file the parser rejects falls back to the line rule, not to silence."""
    scripts = {
        "scripts/broken.py": (
            'def gate(:\n    subprocess.run(["cargo", "build", "-p", "hew-lib"])\n'
        )
    }
    evidence = compiling_evidence(
        "\tscripts/broken.py\n", lambda target: None, scripts.get
    )
    assert evidence is not None, evidence


def test_no_warmup_names_a_non_ci_nextest_profile() -> None:
    """A3a of the reachability gate scans this dry-run text for fast-tier profiles.

    scripts/check-gate-reachability.py feeds the fallback dry-run output into the
    CI command corpus, so a warm-up that spells `--profile ci-cabi` reads as CI
    running a non-`ci` nextest profile. Warm through the Makefile target instead.
    """
    result = run_dispatcher("some-unclassified-root-file.txt")

    assert result.returncode == 0, result.stderr
    profiles = set(re.findall(r"--profile\s+([A-Za-z0-9_-]+)", result.stdout))
    assert profiles <= {"ci"}, result.stdout


def test_scripts_config_budget_annotation() -> None:
    """scripts-config lane uses the conservative fallback budget in dry-run."""
    result = run_dispatcher("Makefile")
    assert result.returncode == 0, result.stderr
    assert "(budget: 600s)" in result.stdout, (
        f"Expected '(budget: 600s)' for scripts-config lane.\nstdout:\n{result.stdout}"
    )


def run_with_fake_nextest(version: str) -> subprocess.CompletedProcess[str]:
    """Run a compiling lane with `cargo nextest --version` reporting `version`."""
    with tempfile.TemporaryDirectory() as bin_dir:
        fake_cargo = Path(bin_dir) / "cargo"
        fake_cargo.write_text(
            "#!/bin/sh\n"
            'if [ "$1" = "nextest" ] && [ "$2" = "--version" ]; then\n'
            f"  printf 'cargo-nextest {version} (deadbeef 2026-01-01)\\n'\n"
            "  exit 0\n"
            "fi\n"
            'if [ "$1" = "metadata" ]; then\n'
            '  printf \'{"packages":[],"target_directory":"target"}\\n\'\n'
            "  exit 0\n"
            "fi\n"
            "exit 0\n",
            encoding="utf-8",
        )
        fake_cargo.chmod(0o755)
        env = os.environ.copy()
        env["PATH"] = f"{bin_dir}{os.pathsep}{env['PATH']}"
        env["PREFLIGHT_TEST_ALLOW_OVERRIDE"] = "1"
        env["PREFLIGHT_TEST_COMMANDS"] = "true"
        return subprocess.run(
            ["bash", str(SCRIPT), "--", "hew-parser/src/lib.rs"],
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
            env=env,
            timeout=60,
        )


def pinned_nextest_version() -> str:
    """The build system's one cargo-nextest pin, from the tool-pin contract."""
    contract = (ROOT / "scripts/tests/test_tool_pin_contract.py").read_text()
    match = re.search(r'"NEXTEST": \("cargo-nextest", "([0-9.]+)"\)', contract)
    assert match, contract
    return match.group(1)


def test_a_nextest_older_than_the_pin_stops_the_preflight() -> None:
    """Three reds were an unpinned nextest rejecting a flag the gates pass."""
    result = run_with_fake_nextest("0.9.99")

    assert result.returncode != 0, result.stdout
    assert "older than the pinned" in result.stderr, result.stderr
    assert "==> true" not in result.stdout, result.stdout


def test_the_preflight_reads_its_pin_from_the_tool_pin_contract() -> None:
    """One declaration for the whole build system, not a second copy here."""
    pinned = pinned_nextest_version()
    result = run_with_fake_nextest(pinned)

    assert result.returncode == 0, result.stderr
    assert f"satisfies the pinned {pinned}" in result.stdout, result.stdout
    assert pinned not in SCRIPT.read_text(), (
        "the dispatcher must read the pin, not restate it"
    )


def test_runtime_net_lane_budget_annotation() -> None:
    """runtime-net lane (narrow) shows 180s budget in dry-run."""
    result = run_dispatcher("hew-runtime/src/actor.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: runtime-net" in result.stdout, result.stdout
    assert "(budget: 180s)" in result.stdout, (
        f"Expected '(budget: 180s)' for runtime-net lane.\nstdout:\n{result.stdout}"
    )


def test_runtime_net_lane_rebuilds_libhew() -> None:
    """runtime-net lane includes make stdlib + freshness check before tests.

    Both hew-runtime and hew-lib source changes must produce libhew.a before
    the workspace tests run, so linked programs never test against a stale .a.
    """
    for path in ("hew-runtime/src/lib.rs", "hew-lib/src/lib.rs"):
        result = run_dispatcher(path)
        assert result.returncode == 0, result.stderr
        assert "Selected profile: runtime-net" in result.stdout, (
            f"Expected runtime-net profile for {path}.\nstdout:\n{result.stdout}"
        )
        assert "make stdlib" in result.stdout, (
            f"Expected 'make stdlib' in runtime-net commands for {path}.\n"
            f"stdout:\n{result.stdout}"
        )
        assert "make check-libhew-fresh" in result.stdout, (
            f"Expected 'make check-libhew-fresh' in runtime-net commands for {path}.\n"
            f"stdout:\n{result.stdout}"
        )
        # Freshness gate must appear before the test command.
        commands = _selected_commands(result.stdout)
        stdlib_pos = commands.index("make stdlib")
        fresh_pos = commands.index("make check-libhew-fresh")
        test_pos = next(i for i, c in enumerate(commands) if c.startswith("make test"))
        assert stdlib_pos < fresh_pos < test_pos, (
            f"Expected order: make stdlib < check-libhew-fresh < test.\n"
            f"stdout:\n{result.stdout}"
        )


def test_zero_timeout_fails_closed() -> None:
    """PREFLIGHT_TIMEOUT_NARROW=0 must fail closed, not bypass the watchdog.

    alarm(0) in Perl cancels the watchdog; if the helper accepted 0 as a valid
    budget the command would run unguarded.  The validation in
    run_in_pgroup_with_timeout must reject it before launching the command.
    """
    result = run_dispatcher(
        "hew-types/src/lib.rs",
        env={
            "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
            "PREFLIGHT_TEST_COMMANDS": "true",
            "PREFLIGHT_TEST_WARMUP_COMMANDS": "true",
            "PREFLIGHT_TIMEOUT_NARROW": "0",
        },
        dry_run=False,
        timeout=10,
    )
    assert result.returncode != 0, (
        "Expected nonzero exit when PREFLIGHT_TIMEOUT_NARROW=0 "
        "(watchdog must not be bypassed)"
    )
    combined = result.stdout + result.stderr
    assert "positive integer" in combined, (
        f"Expected 'positive integer' diagnostic in output.\n"
        f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}"
    )


def test_compiler_pipeline_rs_change_includes_vertical_slice_oracle() -> None:
    """Pure compiler-pipeline Rust changes run the end-to-end vertical-slice oracle."""
    result = run_dispatcher("hew-mir/src/lower.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: compiler-pipeline" in result.stdout, result.stdout
    assert "cargo nextest run --profile ci" in result.stdout, result.stdout
    assert "make test-vertical-slice" in result.stdout, result.stdout
    # The hew-cli consumer corpus (compiled leak/drop oracles, await_e2e,
    # eval_e2e, …) runs inside make test-compiler-pipeline (-p hew-cli
    # -p hew-pkg, ci profile); a separate single-test hew-cli command here
    # would be a duplicate run of tests the lane already covers.
    assert "--test await_e2e" not in result.stdout, result.stdout


def test_compiler_pipeline_lane_includes_checked_mir_verify() -> None:
    """A MIR-changing (drop-plan / lowering) diff runs the checked-MIR golden diff.

    A hew-mir edit that shifts emitted MIR drifts the committed
    examples/v05/checked-mir goldens.  Without checked-mir-verify in this lane
    the drift is invisible to the local preflight and only surfaces at hosted
    CI's Build & test (Linux) job.  This ratchet locks the step into the lane so
    it cannot be silently dropped.
    """
    result = run_dispatcher("hew-mir/src/lower/drop_plan.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: compiler-pipeline" in result.stdout, result.stdout
    assert "make checked-mir-verify" in result.stdout, (
        f"Expected 'make checked-mir-verify' in compiler-pipeline lane.\n"
        f"stdout:\n{result.stdout}"
    )


def test_types_lane_includes_checked_mir_verify() -> None:
    """A type-checker change runs the checked-MIR golden diff.

    Type inference feeds MIR lowering, so a hew-types edit can drift the
    examples/v05/checked-mir goldens.  The types lane runs checked-mir-verify so
    that drift is caught locally rather than at hosted CI.
    """
    result = run_dispatcher("hew-types/src/lib.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: types" in result.stdout, result.stdout
    assert "make checked-mir-verify" in result.stdout, (
        f"Expected 'make checked-mir-verify' in types lane.\nstdout:\n{result.stdout}"
    )


def test_compiler_pipeline_lane_includes_checked_mir_run() -> None:
    """A MIR-changing diff also executes the checked-MIR corpus.

    checked-mir-verify only diffs dumped text; it never loads the compiled
    fixtures.  A drop-plan edit can leave every golden byte-identical and still
    make a fixture segfault on every run, which is how a segfaulting
    channel_auto_close_scope shipped with every gate green.  This ratchet locks
    the execution step in next to the golden diff.
    """
    result = run_dispatcher("hew-mir/src/lower/drop_plan.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: compiler-pipeline" in result.stdout, result.stdout
    assert "make checked-mir-run" in result.stdout, (
        f"Expected 'make checked-mir-run' in the compiler-pipeline profile.\n"
        f"stdout:\n{result.stdout}"
    )


def test_types_lane_includes_checked_mir_run() -> None:
    """A type-checker change also executes the checked-MIR corpus.

    Type inference feeds MIR lowering and therefore runtime behaviour, not just
    the dumped text, so the execution gate runs here alongside the golden diff.
    """
    result = run_dispatcher("hew-types/src/lib.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: types" in result.stdout, result.stdout
    assert "make checked-mir-run" in result.stdout, (
        f"Expected 'make checked-mir-run' in the types profile.\nstdout:\n{result.stdout}"
    )


def test_mir_types_diff_routes_capability_authority_ratchet() -> None:
    """hew-mir / hew-types diffs run the hew-capability-gen authority ratchet.

    hew-capability-gen/tests/authority.rs pins checker coverage over the
    capability surface STRUCTURALLY; the crate has no cargo dependency on
    hew-mir or hew-types, so the reverse-dep closure never selects it.  A
    hew-mir/hew-types branch broke structural_coverage_ratchet_is_pinned with
    every closure-routed test green — this ratchet locks the explicit gate in.
    """
    result = run_dispatcher("hew-mir/src/lower.rs", "hew-types/src/check/resolution.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: compiler-pipeline" in result.stdout, result.stdout
    assert (
        "  - cargo nextest run --profile ci -p hew-capability-gen" in result.stdout
    ), (
        f"Expected the capability authority ratchet in the routing.\n"
        f"stdout:\n{result.stdout}"
    )


def test_mir_types_diff_routes_cross_crate_catching_gates() -> None:
    """The closure nextest run for hew-mir / hew-types diffs carries hew-cli
    and hew-codegen-rs.

    Two observed escape classes depend on this closure: a hew-mir/hew-types
    change broke hew-cli/tests/affine_resource_carrier_boundaries.rs, and a
    hew-types resolver change split stdlib nominal identity, caught only by
    the hew-codegen-rs exec suites (sink_owned_element_exec).  Both catching
    suites ride AFFECTED_PACKAGE_ARGS' reverse-dep closure; this ratchet pins
    that the closure actually selects them, so a closure-computation change
    cannot silently drop the coverage.
    """
    result = run_dispatcher("hew-mir/src/lower.rs", "hew-types/src/check/resolution.rs")
    assert result.returncode == 0, result.stderr
    nextest_lines = [
        line
        for line in result.stdout.splitlines()
        if "cargo nextest run --profile ci" in line
        and "-p hew-capability-gen" not in line
    ]
    assert nextest_lines, (
        f"Expected a closure nextest command.\nstdout:\n{result.stdout}"
    )
    for package in ("-p hew-cli", "-p hew-codegen-rs"):
        assert any(package in line for line in nextest_lines), (
            f"Expected {package} in the closure nextest run.\nstdout:\n{result.stdout}"
        )


def test_types_resolver_diff_routes_codegen_exec_closure() -> None:
    """A lone hew-types resolver diff still routes the codegen exec suites.

    The nominal-identity escape came from a resolver change whose fallout was
    only visible when hew-codegen-rs exec tests compiled and ran stdlib-using
    programs.  The types lane's closure nextest must therefore carry
    -p hew-codegen-rs (and the exec binaries with it) for resolver-file diffs.
    """
    result = run_dispatcher("hew-types/src/check/resolution.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: types" in result.stdout, result.stdout
    assert "-p hew-codegen-rs" in result.stdout, (
        f"Expected -p hew-codegen-rs in the types lane closure.\n"
        f"stdout:\n{result.stdout}"
    )
    assert (
        "  - cargo nextest run --profile ci -p hew-capability-gen" in result.stdout
    ), (
        f"Expected the capability authority ratchet for a hew-types diff.\n"
        f"stdout:\n{result.stdout}"
    )


def test_codegen_emission_diff_routes_ll_oracle_golden_diff() -> None:
    """hew-hir / hew-codegen-rs / hew-mir diffs run the ll-oracle golden diff.

    A codegen epilogue reorder invalidated the ll-oracle goldens with the
    whole compiler-pipeline lane green: nothing in the lane diffed the
    committed per-function IR corpus.  make ll-diff (~45s) closes that class
    for emission-reaching diffs; intentional drifts regenerate via ll-golden
    in the same commit.
    """
    for path in (
        "hew-hir/src/lib.rs",
        "hew-codegen-rs/src/llvm.rs",
        "hew-mir/src/lower.rs",
    ):
        result = run_dispatcher(path)
        assert result.returncode == 0, result.stderr
        assert "Selected profile: compiler-pipeline" in result.stdout, result.stdout
        assert "  - make ll-diff" in result.stdout, (
            f"Expected 'make ll-diff' for {path}.\nstdout:\n{result.stdout}"
        )


def test_fallback_lane_does_not_duplicate_side_channel_gates() -> None:
    """The fallback lane already carries ll-diff and the full workspace run;
    the side-channel flags must not append duplicates on top of it."""
    result = run_dispatcher("hew-codegen-rs/src/llvm.rs", "tools/some-tool.py")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: comprehensive" in result.stdout, result.stdout
    assert result.stdout.count("  - make ll-diff") == 1, result.stdout
    assert (
        "cargo nextest run --profile ci -p hew-capability-gen" not in result.stdout
    ), (
        f"make test already covers hew-capability-gen in the fallback lane.\n"
        f"stdout:\n{result.stdout}"
    )


def test_make_test_compiler_pipeline_recipe_keeps_consumer_corpus_packages() -> None:
    """The compiler-pipeline and types lanes delegate hew-cli consumer-corpus
    coverage to make test-compiler-pipeline: its nextest invocation must keep
    -p hew-cli and -p hew-pkg under the ci profile.  If a Makefile edit drops
    either package, the compiled leak/drop oracles and the e2e suites silently
    stop running for HIR/MIR/codegen and type-checker diffs — exactly the
    consumer-corpus escape class this ratchet exists to block.
    """
    makefile = (ROOT / "Makefile").read_text(encoding="utf-8")
    match = re.search(
        r"^test-compiler-pipeline:[^\n]*\n(?:\t[^\n]*\n)+", makefile, re.MULTILINE
    )
    assert match is not None, "test-compiler-pipeline recipe not found in Makefile"
    recipe = match.group(0)
    assert "--profile ci" in recipe, recipe
    assert "-p hew-cli" in recipe, recipe
    assert "-p hew-pkg" in recipe, recipe


def test_docs_only_change_does_not_include_vertical_slice_oracle() -> None:
    """Docs-only changes run no compiler-backed suite."""
    result = run_dispatcher("docs/README.md")
    assert result.returncode == 0, result.stderr
    assert "docs-only" in result.stdout, result.stdout
    assert "make test-vertical-slice" not in result.stdout, result.stdout
    assert "make test-doc-examples" not in result.stdout, result.stdout
    assert "cargo nextest" not in result.stdout, result.stdout
    assert "cargo clippy" not in result.stdout, result.stdout


def test_comprehensive_profile_reserves_smoke_for_local_opt_in() -> None:
    result = run_dispatcher("some-unclassified-root-file.txt")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: comprehensive" in result.stdout, result.stdout
    assert "make ci-preflight-smoke" not in result.stdout, result.stdout

    # Order is a property of the gate list, not of the whole transcript: the
    # warm-up block above it also names `make test-*` targets.
    commands = result.stdout.split("Commands:\n", 1)[1]
    assert "cargo fmt --all -- --check" in commands, result.stdout
    assert "make lint" in commands, result.stdout
    assert "make test" in commands, result.stdout

    fmt_pos = commands.index("cargo fmt --all -- --check")
    lint_pos = commands.index("make lint")
    test_pos = commands.index("make test")
    assert fmt_pos < lint_pos < test_pos, result.stdout

    makefile = (ROOT / "Makefile").read_text()
    assert "cargo nextest run --workspace --profile smoke" in makefile, makefile
    assert (
        "cargo nextest run --workspace --exclude hew-cabi --profile ci --no-fail-fast"
        in makefile
    ), makefile

    nextest = (ROOT / ".config/nextest.toml").read_text()
    assert (
        'default-filter = "test(every_hew_file_roundtrips_through_formatter)"'
        in nextest
    )
    assert (
        "hew-parser/tests/fmt_roundtrip_corpus.rs"
        not in nextest.split("[profile.ci]", 1)[1].split("[profile.ci.junit]", 1)[0]
    )


def test_hew_tests_path_routes_to_hew_tests_lane() -> None:
    """Changes in tests/hew/ route to the hew-tests lane with both ratchets."""
    result = run_dispatcher("tests/hew/vec_test.hew")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: hew-tests" in result.stdout, result.stdout
    assert "make test-hew-ratchet" in result.stdout, (
        f"Expected 'make test-hew-ratchet' in hew-tests lane.\nstdout:\n{result.stdout}"
    )
    assert "make test-hew-ratchet  (budget: 1500s)" in result.stdout, result.stdout
    assert "make test-stdlib-ratchet" in result.stdout, (
        f"Expected 'make test-stdlib-ratchet' in hew-tests lane.\nstdout:\n{result.stdout}"
    )
    assert "make hew-fmt-property" in result.stdout, result.stdout


def test_parser_path_runs_formatter_property() -> None:
    result = run_dispatcher("hew-parser/src/fmt.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: parser" in result.stdout, result.stdout
    assert _selected_commands(result.stdout).count("make hew-fmt-property") == 1, (
        result.stdout
    )


def test_vertical_slice_source_runs_formatter_property() -> None:
    result = run_dispatcher("tests/vertical-slice/accept/example.hew")
    assert result.returncode == 0, result.stderr
    assert _selected_commands(result.stdout).count("make hew-fmt-property") == 1, (
        result.stdout
    )


def test_std_hew_file_adds_hew_suite_addon() -> None:
    """A .hew change under std/ appends both hew-suite ratchets as addons.

    The lane is determined by the non-hew parts of the diff; the ratchets are
    appended regardless of which lane was selected.  Use a pure std/.hew change,
    which routes to the runtime-net lane (std/net/*.hew) or may route to
    another lane — the key assertion is that the ratchets appear in the command
    list.
    """
    result = run_dispatcher("std/string.hew")
    assert result.returncode == 0, result.stderr
    assert "make test-hew-ratchet" in result.stdout, (
        f"Expected 'make test-hew-ratchet' appended for std/ .hew change.\n"
        f"stdout:\n{result.stdout}"
    )
    assert "make test-stdlib-ratchet" in result.stdout, (
        f"Expected 'make test-stdlib-ratchet' appended for std/ .hew change.\n"
        f"stdout:\n{result.stdout}"
    )


def test_fallback_lane_includes_hew_suite_ratchets() -> None:
    """Fallback (comprehensive) lane includes both Hew-language suite ratchets."""
    result = run_dispatcher("some-unclassified-root-file.txt")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: comprehensive" in result.stdout, result.stdout
    assert "make test-hew-ratchet" in result.stdout, (
        f"Expected 'make test-hew-ratchet' in fallback lane.\nstdout:\n{result.stdout}"
    )
    assert "make test-hew-ratchet  (budget: 1500s)" in result.stdout, result.stdout
    assert "make test-o2-differential  (budget: 2700s)" in result.stdout, result.stdout
    assert "make test-stdlib-ratchet" in result.stdout, (
        f"Expected 'make test-stdlib-ratchet' in fallback lane.\nstdout:\n{result.stdout}"
    )
    assert _selected_commands(result.stdout).count("make hew-fmt-property") == 1, (
        result.stdout
    )
    # Ratchets must appear after make test (Rust suite runs first).
    # The budget annotation "(budget: Xs)" may appear on the same line in dry-run.
    commands = _selected_commands(result.stdout)
    test_pos = commands.index("make test")
    hew_pos = commands.index("make test-hew-ratchet")
    stdlib_pos = commands.index("make test-stdlib-ratchet")
    assert test_pos < hew_pos, (
        f"Expected make test before make test-hew-ratchet.\nstdout:\n{result.stdout}"
    )
    assert test_pos < stdlib_pos, (
        f"Expected make test before make test-stdlib-ratchet.\nstdout:\n{result.stdout}"
    )


def test_stdlib_execution_proof_authorities_route_to_their_gate() -> None:
    """The manifest and its checker run the proof gate before push."""
    result = run_dispatcher(
        "scripts/stdlib-execution-proof.sh",
        "scripts/stdlib-execution-proofs.tsv",
    )
    assert result.returncode == 0, result.stderr
    assert "Selected profile: scripts-config" in result.stdout, result.stdout
    assert "make test-stdlib-execution-proofs" in result.stdout, (
        "Expected stdlib proof authorities to run their verifier.\n"
        f"stdout:\n{result.stdout}"
    )


def test_parser_plus_types_narrow_multi_bucket_uses_types_lane() -> None:
    """Parser + type-checker changes route to the types lane, not fallback.

    The types lane runs test-compiler-pipeline (the full HIR/MIR/codegen closure)
    plus fuzz-oracle, covering both buckets.  A type-checker change can break
    hew-hir / hew-mir tests that a package subset would never run (#2026).
    This avoids the 9156-test fallback suite while keeping the gate sound.
    """
    result = run_dispatcher("hew-parser/src/parser.rs", "hew-types/src/lib.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: types" in result.stdout, (
        f"Expected types profile for parser + types diff.\nstdout:\n{result.stdout}"
    )
    assert "cargo nextest run --profile ci" in result.stdout, result.stdout
    # The proving gate needs its per-command floor: 7887 tests measure ~234 s
    # warm, so the types lane's 180 s narrow tier would watchdog-kill it.
    assert "cargo nextest run --profile ci" in result.stdout, result.stdout
    # fuzz-oracle must run: a type-checker change can produce wrong trap signals.
    assert "make fuzz-oracle" in result.stdout, result.stdout
    # Must NOT have fallen back to the full suite.
    assert (
        "make test\n" not in result.stdout and "  - make test\n" not in result.stdout
    ), f"Expected narrow types lane, not full fallback.\nstdout:\n{result.stdout}"


# ---------------------------------------------------------------------------
# Slice 2: positive bucket-routing assertions (codegen/cli/wasm + mixed)
# ---------------------------------------------------------------------------


def test_hew_hir_routes_to_compiler_pipeline_lane() -> None:
    """hew-hir/* changes route to the compiler-pipeline lane.

    is_compiler_pipeline_path matches hew-hir/*, hew-mir/*, hew-codegen-rs/*.
    The lane runs test-compiler-pipeline + test-vertical-slice.
    """
    result = run_dispatcher("hew-hir/src/lib.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: compiler-pipeline" in result.stdout, (
        f"Expected compiler-pipeline for hew-hir change.\nstdout:\n{result.stdout}"
    )
    assert "cargo nextest run --profile ci" in result.stdout, result.stdout
    assert "make test-vertical-slice" in result.stdout, result.stdout


def test_hew_codegen_rs_routes_to_compiler_pipeline_lane() -> None:
    """hew-codegen-rs/* changes route to the compiler-pipeline lane."""
    result = run_dispatcher("hew-codegen-rs/src/emit.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: compiler-pipeline" in result.stdout, (
        f"Expected compiler-pipeline for hew-codegen-rs change.\nstdout:\n{result.stdout}"
    )
    assert "cargo nextest run --profile ci" in result.stdout, result.stdout


def test_hew_compile_routes_to_cli_lane() -> None:
    """hew-compile/* changes route to the cli lane.

    is_cli_path matches hew-cli/*, hew-pkg/*, hew-compile/*,
    hew-cabi/*, hew-capability-gen/*.
    """
    result = run_dispatcher("hew-compile/src/lib.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: cli" in result.stdout, (
        f"Expected cli lane for hew-compile change.\nstdout:\n{result.stdout}"
    )
    assert "cargo nextest run --profile ci" in result.stdout, result.stdout


def test_hew_cabi_routes_to_cli_lane() -> None:
    """hew-cabi/* changes route to the cli lane (C ABI helpers)."""
    result = run_dispatcher("hew-cabi/src/lib.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: cli" in result.stdout, (
        f"Expected cli lane for hew-cabi change.\nstdout:\n{result.stdout}"
    )
    assert "cargo nextest run --profile ci" in result.stdout, result.stdout


def test_hew_capability_gen_routes_to_cli_lane() -> None:
    """hew-capability-gen/* changes route to the cli lane."""
    result = run_dispatcher("hew-capability-gen/src/main.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: cli" in result.stdout, (
        f"Expected cli lane for hew-capability-gen change.\nstdout:\n{result.stdout}"
    )
    assert "cargo nextest run --profile ci" in result.stdout, result.stdout


def test_hew_wasm_routes_to_wasm_lane() -> None:
    """hew-wasm/* changes route to the wasm lane.

    The wasm selection runs the affected package plus make playground-check for the
    wasm-pack build smoke test.
    """
    result = run_dispatcher("hew-wasm/src/lib.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: wasm" in result.stdout, (
        f"Expected wasm lane for hew-wasm change.\nstdout:\n{result.stdout}"
    )
    assert "cargo nextest run --profile ci -p hew-wasm" in result.stdout, (
        f"Expected package-scoped nextest for wasm.\nstdout:\n{result.stdout}"
    )
    assert "make playground-check" in result.stdout, result.stdout
    # Must NOT have fallen back to the full test suite.
    assert (
        "  - make test\n" not in result.stdout and "make test\n" not in result.stdout
    ), f"Wasm lane must not run full make test.\nstdout:\n{result.stdout}"


def test_compiler_pipeline_absorbs_types_bucket_in_mixed_diff() -> None:
    """A diff spanning both hew-hir/* (codegen bucket) and hew-types/* stays
    on the compiler-pipeline lane rather than falling back.

    When compiler_related=1, the dispatcher zeroes out has_types (and
    has_parser/has_cli) before computing bucket_count, so the mix counts
    as a single compiler-pipeline bucket and selects the narrow lane.
    """
    result = run_dispatcher("hew-hir/src/lib.rs", "hew-types/src/lib.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: compiler-pipeline" in result.stdout, (
        f"Expected compiler-pipeline for hew-hir + hew-types diff.\n"
        f"stdout:\n{result.stdout}"
    )
    assert "cargo nextest run --profile ci" in result.stdout, result.stdout
    # Must NOT have fallen back to the full suite.
    assert (
        "  - make test\n" not in result.stdout and "make test\n" not in result.stdout
    ), (
        f"Expected narrow compiler-pipeline lane, not full fallback.\nstdout:\n{result.stdout}"
    )


def test_leaf_crate_runs_only_its_reverse_dependency_closure() -> None:
    result = run_dispatcher("hew-observe/src/lib.rs")
    assert result.returncode == 0, result.stderr
    sections = result.stdout.split("Warm-up:\n", 1)
    assert len(sections) == 2, result.stdout
    commands = sections[1].split("Commands:\n", 1)
    assert len(commands) == 2, result.stdout
    commands = commands[1]
    assert "cargo nextest run --profile ci -p hew-observe" in commands
    assert "--workspace" not in commands
    assert "-p hew-parser" not in commands


def test_analysis_change_runs_known_dependents_without_workspace() -> None:
    result = run_dispatcher("hew-analysis/src/lib.rs")
    assert result.returncode == 0, result.stderr
    nextest = next(
        line for line in result.stdout.splitlines() if "cargo nextest run" in line
    )
    for package in ("hew-analysis", "hew-cli", "hew-lsp", "hew-wasm"):
        assert f"-p {package}" in nextest
    assert "--workspace" not in nextest
    assert "make test-hew-ratchet" not in result.stdout


def test_hosted_linux_executes_the_dispatcher_directly() -> None:
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()
    assert 'scripts/ci-preflight-dispatcher.sh "${args[@]}"' in workflow
    assert "run: make test-hew-ratchet" not in workflow


def _trunk_health_step() -> str:
    """The trunk-health step's shell body, read out of the workflow it runs in."""
    spec = importlib.util.spec_from_file_location(
        "check_gate_reachability", ROOT / "scripts" / "check-gate-reachability.py"
    )
    assert spec and spec.loader
    reachability = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = reachability
    spec.loader.exec_module(reachability)
    workflow = reachability.parse_yaml(
        (ROOT / ".github/workflows/ci.yml").read_text(), "ci.yml"
    )
    steps = workflow["jobs"]["main-health"]["steps"]
    bodies = [step["run"] for step in steps if "run" in step]
    assert len(bodies) == 1, bodies
    return bodies[0]


def _run_trunk_health(
    gh_stdout: str, gh_status: int, labels: str = "[]"
) -> subprocess.CompletedProcess[str]:
    """Run the step with a stubbed `gh`, the way the runner would."""
    with tempfile.TemporaryDirectory() as work:
        bin_dir = Path(work) / "bin"
        bin_dir.mkdir()
        stub = bin_dir / "gh"
        stub.write_text(
            f"#!/bin/sh\ncat <<'STUB_JSON'\n{gh_stdout}\nSTUB_JSON\nexit {gh_status}\n",
            encoding="utf-8",
        )
        stub.chmod(0o755)
        script = Path(work) / "step.sh"
        script.write_text(_trunk_health_step(), encoding="utf-8")
        env = os.environ.copy()
        env["PATH"] = f"{bin_dir}{os.pathsep}{env['PATH']}"
        env["GITHUB_REPOSITORY"] = "hew-lang/hew"
        env["PR_LABELS"] = labels
        return subprocess.run(
            ["bash", str(script)],
            cwd=work,
            check=False,
            capture_output=True,
            text=True,
            env=env,
        )


_RED_MAIN = (
    '{"workflow_runs":[{"conclusion":"failure","head_sha":"deadbeef",'
    '"html_url":"https://example.invalid/run"}]}'
)
_GREEN_MAIN = (
    '{"workflow_runs":[{"conclusion":"success","head_sha":"cafebabe",'
    '"html_url":"https://example.invalid/run"}]}'
)


def test_trunk_health_blocks_on_a_confirmed_red_main() -> None:
    result = _run_trunk_health(_RED_MAIN, 0)

    assert result.returncode == 1, result.stdout
    assert "main is red at deadbeef" in result.stdout, result.stdout


def test_trunk_health_passes_on_a_green_main() -> None:
    result = _run_trunk_health(_GREEN_MAIN, 0)

    assert result.returncode == 0, result.stdout + result.stderr


def test_trunk_health_fails_open_when_the_api_call_fails() -> None:
    """This gate stands in front of every job; an API error is not evidence.

    A 404, an expired token or a transient 502 would otherwise skip all
    thirteen jobs through `needs` — the whole repository down on a read that
    says nothing about main.
    """
    result = _run_trunk_health("gh: not found", 1)

    assert result.returncode == 0, result.stdout + result.stderr
    assert "could not read main's CI status" in result.stdout, result.stdout


def test_trunk_health_fails_open_on_an_unreadable_response() -> None:
    result = _run_trunk_health("<html>502 Bad Gateway</html>", 0)

    assert result.returncode == 0, result.stdout + result.stderr


def test_a_labelled_fix_for_main_runs_against_a_red_main() -> None:
    result = _run_trunk_health(_RED_MAIN, 0, labels='["fix-main"]')

    assert result.returncode == 0, result.stdout + result.stderr
    assert "labelled fix-main" in result.stdout, result.stdout


def test_a_push_to_main_stops_at_the_first_failing_gate() -> None:
    """A red main broadcasts; finishing the other 37 commands proves nothing."""
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()
    assert workflow.count('if [[ "${GITHUB_EVENT_NAME}" == "push" ]]; then') == 2, (
        workflow
    )
    assert workflow.count("args+=(--fail-fast)") == 2, workflow


def test_every_job_waits_for_a_green_main() -> None:
    """A red main must not reach any job, including ones added later.

    On 2026-08-20 eleven branch runs reported one defect on main. The gate
    hangs off `changes`, which every job that runs anything already needs, so
    the fan-out is structural rather than a list somebody has to remember.
    """
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()
    assert "main is red at ${head_sha}; fix main first" in workflow, workflow
    # Blocking on anything but a failed run deadlocks the repository on the
    # cancellations the concurrency group produces as a matter of course.
    assert "failure|timed_out" in workflow, workflow

    # The reachability gate's stdlib-only workflow parser: no Python gate in
    # scripts/ may need a pip step to run on a fresh checkout.
    spec = importlib.util.spec_from_file_location(
        "check_gate_reachability", ROOT / "scripts" / "check-gate-reachability.py"
    )
    assert spec and spec.loader
    reachability = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = reachability
    spec.loader.exec_module(reachability)
    document = reachability.parse_yaml(workflow, "ci.yml")
    jobs = document["jobs"]
    assert "main-health" in jobs, sorted(jobs)

    def waits(name: str) -> bool:
        needs = jobs[name].get("needs") or []
        needs = [needs] if isinstance(needs, str) else needs
        return "main-health" in needs or any(waits(dep) for dep in needs)

    ungated = sorted(name for name in jobs if name != "main-health" and not waits(name))
    assert not ungated, ungated


def test_compiled_hew_aggregate_owns_hosted_full_suite_verdicts() -> None:
    local = run_dispatcher("some-unclassified-root-file.txt")
    assert local.returncode == 0, local.stderr
    assert "  - make test-hew-ratchet " in local.stdout, local.stdout
    assert "  - make test-o2-differential " in local.stdout, local.stdout

    hosted = run_dispatcher(
        "some-unclassified-root-file.txt",
        env={"COMPILED_HEW_GATE_OWNER": "aggregate"},
    )
    assert hosted.returncode == 0, hosted.stderr
    assert "make test-hew-ratchet" not in hosted.stdout, hosted.stdout
    assert "make test-o2-differential" not in hosted.stdout, hosted.stdout

    workflow = (ROOT / ".github/workflows/ci.yml").read_text()
    assert "COMPILED_HEW_GATE_OWNER: aggregate" in workflow, workflow


def test_selected_commands_are_unique() -> None:
    result = run_dispatcher(
        "Cargo.toml",
        "hew-sandbox-vm/src/interpreter/parity-runner.ts",
        "std/string.hew",
        "tests/fuzz-oracle/bounds.hew",
    )
    assert result.returncode == 0, result.stderr
    commands = [
        line.removeprefix("  - ").split("  (budget:", 1)[0]
        for line in result.stdout.splitlines()
        if line.startswith("  - ") and "  (budget:" in line
    ]
    assert len(commands) == len(set(commands)), commands
    assert commands.count("make sandbox-parity") == 1, commands


def test_selector_exports_fail_closed_compile_requirement() -> None:
    with tempfile.NamedTemporaryFile() as output:
        result = run_dispatcher(
            "some-unclassified-root-file.txt",
            extra_args=["--github-output", output.name],
        )
        values = output.read().decode()
    assert result.returncode == 0, result.stderr
    assert "profile=comprehensive" in values
    assert "requires_compile=true" in values

    with tempfile.NamedTemporaryFile() as output:
        result = run_dispatcher(
            ".github/workflows/ci.yml",
            extra_args=["--github-output", output.name],
        )
        values = output.read().decode()
    assert result.returncode == 0, result.stderr
    assert "profile=scripts-config" in values
    assert "requires_compile=false" in values


# ---------------------------------------------------------------------------
# Shard partitioning
# ---------------------------------------------------------------------------

# One representative path per routing profile.  A shard partition that loses a
# command on any of these loses it on a real diff of the same shape.
_PROFILE_PROBES: dict[str, tuple[str, ...]] = {
    "comprehensive": ("some-unclassified-root-file.txt",),
    "scripts-config": ("scripts/foo.sh",),
    "grammar": ("docs/specs/Hew.g4",),
    "parser": ("hew-parser/src/lib.rs",),
    "types": ("hew-types/src/lib.rs",),
    "cli": ("hew-cli/src/main.rs",),
    "compiler-pipeline": ("hew-mir/src/lower.rs",),
    "vertical-slice": ("tests/vertical-slice/accept/x.hew",),
    "observe": ("hew-observe/src/lib.rs",),
    "runtime-testkit": ("hew-runtime-testkit/src/lib.rs",),
    "hew-tests": ("tests/hew/x.hew",),
    "runtime-net": ("hew-runtime/src/lib.rs",),
    "wasm": ("hew-wasm/src/lib.rs",),
    "mixed": ("Cargo.toml", "std/string.hew", "tests/fuzz-oracle/bounds.hew"),
}

# Independent restatement of the dispatcher's ordering-dependency map.  If a
# member moves shard, a producer is separated from its consumer or a gate from
# the self-test that proves it has teeth.
_DEPENDENCY_GROUPS: tuple[tuple[str, ...], ...] = (
    (
        "make test-hew-ratchet",
        "make test-o2-differential",
        "make o2-differential-selftest",
    ),
    ("make fuzz-oracle", "make fuzz-oracle-selftest"),
    ("make ll-diff", "make ll-identity-selftest"),
    ("make checked-mir-verify", "make checked-mir-run"),
    ("make test-doc-examples", "make doc-ratchet-selftest"),
    ("make stdlib", "scripts/check-libhew-fresh.sh"),
)

_SHARD_HEADING_RE = re.compile(r"^  shard (\d+)/(\d+)  ")
_SHARD_ENTRY_RE = re.compile(r"^      - (.*)  \(weight: (\d+)s\)$")


def dry_run_commands(result: subprocess.CompletedProcess[str]) -> list[str]:
    sections = result.stdout.split("\nCommands:\n", 1)
    if len(sections) == 1:
        return []
    return [
        line.removeprefix("  - ").split("  (budget:", 1)[0]
        for line in sections[1].splitlines()
        if line.startswith("  - ")
    ]


def shard_plan(
    paths: tuple[str, ...], shards: int, env: dict[str, str] | None = None
) -> dict[int, list[str]]:
    result = run_dispatcher(
        *paths, extra_args=["--shard-plan", str(shards)], dry_run=False, env=env
    )
    assert result.returncode == 0, result.stderr
    plan: dict[int, list[str]] = {index: [] for index in range(1, shards + 1)}
    current: int | None = None
    for line in result.stdout.splitlines():
        heading = _SHARD_HEADING_RE.match(line)
        if heading is not None:
            current = int(heading.group(1))
            assert int(heading.group(2)) == shards, line
            continue
        entry = _SHARD_ENTRY_RE.match(line)
        if entry is not None:
            assert current is not None, result.stdout
            plan[current].append(entry.group(1))
    return plan


def test_shard_plan_is_exhaustive_and_disjoint_for_every_profile() -> None:
    """Every command of every profile lands in exactly one shard, for every N."""
    for profile, paths in _PROFILE_PROBES.items():
        unsharded = dry_run_commands(run_dispatcher(*paths))
        assert unsharded, f"{profile}: expected a nonempty command list"
        for shards in (1, 2, 3, 4, 5, 8):
            plan = shard_plan(paths, shards)
            assigned = [cmd for index in sorted(plan) for cmd in plan[index]]
            assert len(assigned) == len(unsharded), (
                f"{profile} N={shards}: {len(assigned)} assigned vs "
                f"{len(unsharded)} selected"
            )
            assert sorted(assigned) == sorted(unsharded), (
                f"{profile} N={shards}: partition is not the command list"
            )
            assert len(set(assigned)) == len(assigned), (
                f"{profile} N={shards}: a command appears in more than one shard"
            )


def test_shard_partition_preserves_relative_command_order() -> None:
    """Within a shard, commands keep the order the unsharded profile ran them."""
    for profile, paths in _PROFILE_PROBES.items():
        unsharded = dry_run_commands(run_dispatcher(*paths))
        position = {cmd: index for index, cmd in enumerate(unsharded)}
        for shards in (2, 3, 4):
            for index, commands in shard_plan(paths, shards).items():
                positions = [position[cmd] for cmd in commands]
                assert positions == sorted(positions), (
                    f"{profile} N={shards} shard {index}: reordered {commands}"
                )


def test_ordering_dependent_commands_share_a_shard() -> None:
    """A gate and its producer/self-test are never split across shards."""
    for profile, paths in _PROFILE_PROBES.items():
        for shards in (2, 3, 4, 5, 8):
            plan = shard_plan(paths, shards)
            location = {
                cmd: index for index, commands in plan.items() for cmd in commands
            }
            for group in _DEPENDENCY_GROUPS:
                present = [cmd for cmd in group if cmd in location]
                shards_used = {location[cmd] for cmd in present}
                assert len(shards_used) <= 1, (
                    f"{profile} N={shards}: {group} split across {shards_used}"
                )


def test_shard_selection_runs_exactly_its_planned_commands() -> None:
    """--shard K/N dispatches the plan's shard K, nothing more and nothing less."""
    paths = _PROFILE_PROBES["comprehensive"]
    plan = shard_plan(paths, 4)
    for index in range(1, 5):
        result = run_dispatcher(*paths, extra_args=["--shard", f"{index}/4"])
        assert result.returncode == 0, result.stderr
        assert f"Shard: {index}/4" in result.stdout, result.stdout
        assert dry_run_commands(result) == plan[index], result.stdout


def test_shard_one_owns_the_baseline_precheck() -> None:
    """The full lane's baseline check runs once before any shard warms artifacts."""
    paths = _PROFILE_PROBES["comprehensive"]
    first = run_dispatcher(*paths, extra_args=["--shard", "1/4"])
    assert first.returncode == 0, first.stderr
    assert first.stdout.count(f"  - {PRECHECK}") == 1, first.stdout
    assert first.stdout.index("Baseline precheck:") < first.stdout.index("Warm-up:"), (
        first.stdout
    )

    for index in range(2, 5):
        result = run_dispatcher(*paths, extra_args=["--shard", f"{index}/4"])
        assert result.returncode == 0, result.stderr
        assert f"  - {PRECHECK}" not in result.stdout, result.stdout
        assert "runs in shard 1/4 before warm-up." in result.stdout, result.stdout


def test_shard_warmup_is_scoped_to_that_shard_commands() -> None:
    """A shard warms only the artifacts its own commands need."""
    paths = _PROFILE_PROBES["comprehensive"]
    plan = shard_plan(paths, 4)
    workspace_warmup = "make test-build"
    for index in range(1, 5):
        result = run_dispatcher(*paths, extra_args=["--shard", f"{index}/4"])
        assert result.returncode == 0, result.stderr
        warmup = result.stdout.split("Warm-up:\n", 1)
        warmup = warmup[1].split("\nCommands:\n", 1)[0] if len(warmup) == 2 else ""
        if "make test" not in plan[index]:
            assert workspace_warmup not in warmup, (
                f"shard {index} warms the full workspace test build without make test"
            )
        else:
            assert workspace_warmup in warmup, warmup
        if "make lint" not in plan[index]:
            assert "cargo clippy --workspace --tests" not in warmup, (
                f"shard {index} warms workspace clippy without make lint"
            )


def test_invalid_shard_specs_fail_closed() -> None:
    """A spec that cannot partition the list is rejected before anything runs."""
    for spec in ("0/4", "5/4", "1/0", "abc", "2", "4/", "/4", "-1/4", "1/4/4"):
        result = run_dispatcher(
            "some-unclassified-root-file.txt", extra_args=["--shard", spec]
        )
        assert result.returncode != 0, f"--shard {spec} was accepted: {result.stdout}"
        assert "--shard" in result.stderr, result.stderr
        assert "==> Hew CI preflight dispatcher" not in result.stdout, result.stdout

    missing = subprocess.run(
        ["bash", str(SCRIPT), "--dry-run", "--shard"],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    assert missing.returncode != 0, missing.stdout
    assert "--shard requires a K/N spec" in missing.stderr, missing.stderr

    for count in ("0", "-2", "two", ""):
        result = run_dispatcher(
            "some-unclassified-root-file.txt", extra_args=["--shard-plan", count]
        )
        assert result.returncode != 0, (
            f"--shard-plan {count!r} was accepted: {result.stdout}"
        )

    both = run_dispatcher(
        "some-unclassified-root-file.txt",
        extra_args=["--shard", "1/4", "--shard-plan", "4"],
    )
    assert both.returncode != 0, both.stdout
    assert "mutually exclusive" in both.stderr, both.stderr


def test_shard_plan_prints_the_full_assignment_and_runs_nothing() -> None:
    result = run_dispatcher(
        "some-unclassified-root-file.txt",
        extra_args=["--shard-plan", "4"],
        dry_run=False,
    )
    assert result.returncode == 0, result.stderr
    assert "Shard plan: 4 shard(s), LPT bin packing" in result.stdout, result.stdout
    assert result.stdout.count("min estimated") == 4, result.stdout
    assert "Shard plan only: no commands executed." in result.stdout, result.stdout
    assert "==> warm-up" not in result.stdout, result.stdout


def test_help_documents_the_shard_flags() -> None:
    result = run_dispatcher_help()
    assert result.returncode == 0, result.stderr
    assert "--shard K/N" in result.stdout, result.stdout
    assert "--shard-plan N" in result.stdout, result.stdout
    assert "exactly one shard" in result.stdout, result.stdout


def test_hosted_linux_matrix_matches_the_dispatcher_shard_denominator() -> None:
    """Branch protection stays honest only if the matrix covers every shard.

    A matrix of [1,2,3] against `--shard N/4` would silently never run shard 4:
    its commands would vanish from CI while every required check stayed green.
    """
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()
    start = workflow.index("  build-and-test:\n")
    following = re.search(r"^  [a-z][a-z0-9-]*:\n", workflow[start + 1 :], re.MULTILINE)
    job = workflow[start : start + 1 + following.start()]

    matrix = re.search(r"matrix:\n\s+shard: \[([0-9, ]+)\]", job)
    assert matrix is not None, job
    shards = [int(value) for value in matrix.group(1).split(",")]
    assert shards == list(range(1, len(shards) + 1)), shards

    denominators = re.findall(
        r"ci-preflight-dispatcher\.sh --base origin/main --fail-fast "
        r"--shard \$\{\{ matrix\.shard \}\}/(\d+)",
        job,
    )
    if not denominators:
        denominators = re.findall(
            r"args=\(--base origin/main --shard "
            r"\$\{\{ matrix\.shard \}\}/(\d+)\)",
            job,
        )
    assert denominators, job
    assert {int(value) for value in denominators} == {len(shards)}, denominators

    required = workflow[workflow.index("  linux-required:\n") :]
    assert "name: Build & test (Linux)" in required, required
    assert "needs.build-and-test.result" in required, required


# ---------------------------------------------------------------------------
# Failure diagnostics
# ---------------------------------------------------------------------------


def first_failure_cells(summary: str) -> list[str]:
    """The First failure column of every FAILED row of the result table."""
    cells = []
    for line in summary.splitlines():
        if not line.startswith("| `") or "FAILED" not in line:
            continue
        cells.append(line.rsplit("|", 2)[1].strip())
    return cells


def run_failing_commands(commands: str) -> tuple[subprocess.CompletedProcess[str], str]:
    """Run synthetic failing commands under GitHub Actions; return the summary."""
    with tempfile.TemporaryDirectory() as scratch:
        step_summary = Path(scratch) / "step-summary.md"
        local_summary = Path(scratch) / "preflight-summary.md"
        result = run_dispatcher(
            "some-unclassified-root-file.txt",
            env={
                "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
                "PREFLIGHT_TEST_COMMANDS": commands,
                "PREFLIGHT_TEST_WARMUP_COMMANDS": "true",
                "GITHUB_ACTIONS": "true",
                "GITHUB_STEP_SUMMARY": str(step_summary),
                "PREFLIGHT_SUMMARY_FILE": str(local_summary),
            },
            dry_run=False,
            timeout=60,
        )
        local_text = local_summary.read_text()
        step_text = step_summary.read_text()
    assert local_text == step_text.rstrip("\n") + "\n", (
        "the local table and the GitHub step summary must be the same table"
    )
    return result, local_text


def test_failing_command_emits_an_annotation_and_a_result_table() -> None:
    result, summary = run_failing_commands(
        "printf 'ok\\n'\nprintf 'boom\\nerror: cannot find value x\\n' >&2; exit 4\n"
    )
    assert result.returncode == 1, result.stdout

    annotations = [
        line for line in result.stdout.splitlines() if line.startswith("::error ")
    ]
    assert len(annotations) == 1, result.stdout
    assert annotations[0].startswith(
        "::error file=scripts/ci-preflight-dispatcher.sh,title="
    ), annotations[0]
    assert annotations[0].endswith("::error: cannot find value x"), annotations[0]

    assert "| Command | Elapsed | Result | First failure |" in summary, summary
    assert "| `printf 'ok\\n'` | 0s | ok |  |" in summary, summary
    failing = next(line for line in summary.splitlines() if "FAILED (exit 4)" in line)
    assert "error: cannot find value x" in failing, failing
    assert "0 failure" not in summary, summary
    assert "1 failure(s)." in summary, summary


def test_first_failure_extraction_covers_every_gate_failure_shape() -> None:
    """nextest, make, python and ratchet failures each yield their own line."""
    cases = {
        'printf "        FAIL [   1.234s] hew-cli::suite maps_ok\\n"; exit 1': (
            "FAIL [   1.234s] hew-cli::suite maps_ok"
        ),
        'printf "warming\\nmake[1]: *** [Makefile:759: test] Error 2\\n"; exit 2': (
            "make[1]: *** [Makefile:759: test] Error 2"
        ),
        'printf "Traceback\\nAssertionError: union missing 3 tests\\n"; exit 1': (
            "AssertionError: union missing 3 tests"
        ),
        'printf "NOW-FAILS: tests/hew/actor.hew\\n"; exit 3': (
            "NOW-FAILS: tests/hew/actor.hew"
        ),
        'printf "NOW-PASSES: tests/hew/actor.hew\\n"; exit 3': (
            "NOW-PASSES: tests/hew/actor.hew"
        ),
        'printf "silent\\n"; exit 9': (
            "exited 9 with no recognised failure line; see the step log"
        ),
    }
    result, summary = run_failing_commands("\n".join(cases) + "\n")
    assert result.returncode == 1, result.stdout
    for command, expected in cases.items():
        assert f"::{expected}" in result.stdout, (
            f"no annotation carrying {expected!r} for {command!r}:\n{result.stdout}"
        )
        assert expected.replace("|", "\\|") in summary, summary


def test_timeout_reports_the_budget_not_a_stray_log_line() -> None:
    with tempfile.TemporaryDirectory() as scratch:
        local_summary = Path(scratch) / "preflight-summary.md"
        result = run_dispatcher(
            "some-unclassified-root-file.txt",
            env={
                "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
                "PREFLIGHT_TEST_COMMANDS": "printf 'error: red herring\\n'; sleep 30",
                "PREFLIGHT_TEST_WARMUP_COMMANDS": "true",
                "PREFLIGHT_TIMEOUT_FALLBACK": "1",
                "GITHUB_ACTIONS": "true",
                "PREFLIGHT_SUMMARY_FILE": str(local_summary),
            },
            dry_run=False,
            timeout=60,
        )
        summary = local_summary.read_text()
    assert result.returncode != 0, result.stdout
    assert first_failure_cells(summary) == [
        "exceeded the 1s budget and was killed (exit 143)"
    ], summary
    annotation = next(
        line for line in result.stdout.splitlines() if line.startswith("::error ")
    )
    assert "budget and was killed" in annotation, annotation


def test_counterfactual_output_never_becomes_the_reported_failure() -> None:
    """A self-test's provoked bait must not outrank the real failure line.

    A green self-test log carries "ORACLE gate: FAIL" and "error: ..." from
    running the real gate against broken input. Those lines come FIRST, so an
    unguarded first-match grep names the counterfactual instead of the defect.
    """
    bait = (
        "CF-[t1-known-crash] ORACLE gate: FAIL\\n"
        "CF-[t1-known-crash] error: provoked on purpose\\n"
        "PASS oracle-flags-a-known-crash\\n"
        "RATCHET FAIL: tests/hew/actor.hew regressed\\n"
    )
    result, summary = run_failing_commands(f"printf '{bait}'; exit 1\n")
    assert result.returncode == 1, result.stdout
    annotation = next(
        line for line in result.stdout.splitlines() if line.startswith("::error ")
    )
    assert annotation.endswith("::RATCHET FAIL: tests/hew/actor.hew regressed"), (
        annotation
    )
    assert first_failure_cells(summary) == [
        "RATCHET FAIL: tests/hew/actor.hew regressed"
    ], summary


def test_counterfactual_marker_is_shared_by_every_producer() -> None:
    """One marker string across the extractor and every counterfactual site."""
    dispatcher = (ROOT / "scripts/ci-preflight-dispatcher.sh").read_text()
    oracle = (ROOT / "scripts/fuzz/oracle-selftest.sh").read_text()
    consumer = (ROOT / "scripts/tests/test_cargo_output_dir.py").read_text()
    assert "PREFLIGHT_COUNTERFACTUAL_MARKER='CF-'" in dispatcher, dispatcher
    assert 'COUNTERFACTUAL_MARKER="CF-"' in oracle, oracle
    assert 'COUNTERFACTUAL_MARKER = "CF-"' in consumer, consumer
    assert oracle.count('run_counterfactual "') == 4, (
        "every unredirected oracle invocation must replay behind the marker"
    )

    makefile = (ROOT / "Makefile").read_text()
    assert re.search(
        r"^check-counterfactual-output:\n"
        r"\tscripts/ci-preflight-dispatcher\.sh --check-counterfactual-output$",
        makefile,
        re.MULTILINE,
    ), "the counterfactual-output gate must have a Makefile port"

    selection = run_dispatcher("some-unclassified-root-file.txt")
    assert "  - make check-counterfactual-output " in selection.stdout, (
        "the comprehensive profile must run the counterfactual-output gate"
    )


def test_counterfactual_output_check_reuses_the_extractor_authority() -> None:
    """The gate must judge output with the same pattern extraction uses.

    A second copy of the pattern would let a gate pass this check and still
    mislead the annotation, which is the whole failure it exists to prevent.
    """
    dispatcher = (ROOT / "scripts/ci-preflight-dispatcher.sh").read_text()
    body = dispatcher.split("run_counterfactual_output_check() {", 1)[1]
    body = body.split("\nDRY_RUN=0", 1)[0]
    assert "$PREFLIGHT_FAILURE_LINE_RE" in body, body
    assert "${PREFLIGHT_COUNTERFACTUAL_MARKER}" in body, body
    assert dispatcher.count("PREFLIGHT_FAILURE_LINE_RE='") == 1, (
        "the failure pattern must have exactly one definition"
    )

    result = subprocess.run(
        ["bash", str(SCRIPT), "--check-counterfactual-output"],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "counterfactual-output check: PASS" in result.stdout, result.stdout


def test_signal_deaths_are_named_not_swallowed_by_the_fallback() -> None:
    """A trap-killed gate must report the signal, not the generic fallback.

    The shell reports these as a bare line with no "error" or "FAIL" token
    anywhere, so without explicit patterns the whole compiled-Hew trap class
    fell through to "exited N with no recognised failure line".
    """
    cases = {
        'printf "Illegal instruction (core dumped)\\n"; exit 132': (
            "Illegal instruction (core dumped)"
        ),
        'printf "Segmentation fault\\n"; exit 139': "Segmentation fault",
        'printf "running\\nAborted\\n"; exit 134': "Aborted",
        'printf "Bus error\\n"; exit 138': "Bus error",
        'printf "zsh: trace trap  ./a.out\\n"; exit 133': "zsh: trace trap  ./a.out",
        'printf "note\\nAbort trap: 6\\n"; exit 134': "Abort trap: 6",
        'printf "(signal: 4, SIGILL: illegal instruction)\\n"; exit 101': (
            "(signal: 4, SIGILL: illegal instruction)"
        ),
        'printf "test t ... killed by signal 11\\n"; exit 100': (
            "test t ... killed by signal 11"
        ),
    }
    result, summary = run_failing_commands("\n".join(cases) + "\n")
    assert result.returncode == 1, result.stdout
    reported = first_failure_cells(summary)
    assert reported == list(cases.values()), reported
    assert not any("no recognised failure line" in cell for cell in reported), reported


def test_counterfactual_marker_must_be_a_line_prefix() -> None:
    """A real failure that merely mentions the marker must not hide itself.

    Matching CF- anywhere in the line would let any failure text containing it
    — a diff of this protocol, a path like tests/CF-cases — vanish from the
    annotation. The marker is a claim a gate makes about its own output by
    putting it first, not a word that appears somewhere.
    """
    lines = (
        "CF-[bait] ORACLE gate: FAIL\\n"
        "   CF-[indented bait] error: still bait\\n"
        "RATCHET FAIL: a diff of the CF- protocol regressed\\n"
    )
    result, summary = run_failing_commands(f"printf '{lines}'; exit 1\n")
    assert result.returncode == 1, result.stdout
    assert first_failure_cells(summary) == [
        "RATCHET FAIL: a diff of the CF- protocol regressed"
    ], summary


def run_counterfactual_roster(roster: str) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    env["PREFLIGHT_TEST_ALLOW_OVERRIDE"] = "1"
    env["PREFLIGHT_TEST_COUNTERFACTUAL_ROSTER"] = roster
    return subprocess.run(
        ["bash", str(SCRIPT), "--check-counterfactual-output"],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
        env=env,
    )


def test_counterfactual_check_rejects_a_gate_that_proves_nothing() -> None:
    """A rostered gate that emits no marked line is a vacuous pass."""
    silent = run_counterfactual_roster("true")
    assert silent.returncode != 0, silent.stdout
    assert "SILENT true" in silent.stderr, silent.stderr

    proving = run_counterfactual_roster('printf "CF-[x] ORACLE gate: FAIL\\n"')
    assert proving.returncode == 0, proving.stdout + proving.stderr
    assert "1 marked line(s)" in proving.stdout, proving.stdout

    offending = run_counterfactual_roster('printf "ORACLE gate: FAIL\\n"')
    assert offending.returncode != 0, offending.stdout
    assert "OFFENDS" in offending.stderr, offending.stderr

    red = run_counterfactual_roster('printf "CF-[x] bait\\n"; exit 3')
    assert red.returncode != 0, red.stdout
    assert "UNPROVABLE" in red.stderr, red.stderr

    mentions = run_counterfactual_roster(
        'printf "a note about the CF- protocol\\nORACLE gate: FAIL\\n"'
    )
    assert mentions.returncode != 0, mentions.stdout
    assert "OFFENDS" in mentions.stderr, mentions.stderr


def test_every_rostered_gate_replays_its_counterfactual() -> None:
    """The real roster is exactly the gates that drive a tool against bait."""
    dispatcher = (ROOT / "scripts/ci-preflight-dispatcher.sh").read_text()
    roster_block = dispatcher.split("COUNTERFACTUAL_ROSTER=(", 1)[1].split(")", 1)[0]
    roster = re.findall(r'"([^"]+)"', roster_block)
    assert roster, dispatcher
    for absent in (
        "make test-stdlib-execution-proofs",
        "make freebsd-workflow-contract-check",
    ):
        assert absent not in roster, (
            f"{absent} carries no counterfactual; rostering it is a vacuous pass"
        )

    marked_replay_sites = (
        "scripts/ll-identity-selftest.sh",
        "scripts/o2-differential-selftest.sh",
        "scripts/tests/test_doc_ratchet_membership.sh",
        "scripts/tests/test_macos_leak_oracle_runner.sh",
        "scripts/tests/test_cargo_output_dir.py",
        "scripts/fuzz/oracle-selftest.sh",
    )
    for path in marked_replay_sites:
        text = (ROOT / path).read_text()
        assert "COUNTERFACTUAL_MARKER" in text, f"{path} has no marked replay"
        assert '"CF-"' in text, f"{path} uses a different marker"
    assert "CF-[$$label]" in (ROOT / "Makefile").read_text(), (
        "check-sanitizer-gate must replay its rejections marked"
    )


def test_no_annotation_is_emitted_outside_github_actions() -> None:
    with tempfile.TemporaryDirectory() as scratch:
        result = run_dispatcher(
            "some-unclassified-root-file.txt",
            env={
                "PREFLIGHT_TEST_ALLOW_OVERRIDE": "1",
                "PREFLIGHT_TEST_COMMANDS": "exit 5",
                "PREFLIGHT_TEST_WARMUP_COMMANDS": "true",
                "GITHUB_ACTIONS": "",
                "PREFLIGHT_SUMMARY_FILE": str(Path(scratch) / "summary.md"),
            },
            dry_run=False,
            timeout=30,
        )
        summary = (Path(scratch) / "summary.md").read_text()
    assert result.returncode == 1, result.stdout
    assert "::error" not in result.stdout, result.stdout
    assert "FAILED (exit 5)" in summary, summary


_TESTS = [
    test_makefile_routes_to_scripts_config_profile,
    test_scripts_path_routes_to_scripts_config_profile,
    test_nextest_config_routes_to_scripts_config_profile,
    test_workflow_routes_to_scripts_config_profile,
    test_cargo_toml_routes_to_scripts_config_profile,
    test_cargo_lock_routes_to_scripts_config_profile,
    test_dot_cargo_config_routes_to_scripts_config_profile,
    test_rust_toolchain_routes_to_scripts_config_profile,
    test_structural_lint_label_matches_dispatched_command_and_ci_bootstraps,
    # Slice 1 instrumentation tests
    test_dry_run_shows_budget_annotation_narrow_lane,
    test_dry_run_shows_budget_annotation_fallback_lane,
    test_dry_run_scales_every_budget_from_detected_parallelism,
    test_help_includes_profile_json,
    test_profile_json_flag_accepted_in_dry_run,
    test_help_includes_fail_fast_and_run_all_default,
    test_dry_run_reports_run_all_default_policy,
    test_override_without_sentinel_is_rejected,
    test_override_with_sentinel_emits_stderr_warning,
    test_run_all_continues_after_failure_and_profiles_all_commands,
    test_fail_fast_stops_after_first_failure_and_profiles_only_run_commands,
    test_synthetic_timeout_via_run_loop,
    test_profile_json_records_elapsed_for_each_command,
    test_baseline_precheck_is_listed_before_warmup,
    test_baseline_precheck_is_scoped_to_the_lane,
    test_compile_warmup_runs_first_and_has_a_summary_row,
    test_rust_diff_derives_its_warmup_artifacts_before_commands,
    test_docs_diff_has_no_warmup_block,
    test_no_lane_warms_test_targets_with_all_targets,
    test_comprehensive_warms_every_gate_through_its_own_build_form,
    test_every_lane_derives_a_warmup_for_every_command_it_selects,
    test_a_gate_with_no_derivable_warmup_is_fatal,
    test_a_make_gate_without_a_build_form_is_fatal,
    test_a_make_gate_naming_an_undeclared_target_is_fatal,
    test_a_nextest_gate_derives_its_own_invocation_with_no_run,
    test_a_clippy_gate_derives_its_own_invocation_without_the_deny_flag,
    test_a_fmt_gate_warms_nothing,
    test_every_dispatched_make_target_exists_in_the_makefile,
    test_no_warmup_carries_a_flag_its_gate_does_not,
    test_no_gate_compiles_behind_an_empty_build_form,
    test_a_build_form_names_every_binary_its_gate_runs,
    test_the_binary_check_sees_a_half_named_build_form,
    test_the_compile_walk_sees_a_compile_one_script_deep,
    test_the_compile_walk_sees_a_compile_two_levels_deep,
    test_the_compile_walk_does_not_count_printed_text,
    test_the_compile_walk_reads_python_data_as_data,
    test_the_compile_walk_sees_a_multiline_python_subprocess,
    test_the_compile_walk_sees_a_python_compile_behind_a_runtime_argument,
    test_the_compile_walk_follows_a_multiline_python_make_call,
    test_the_compile_walk_reads_a_multiline_python_list_as_data,
    test_the_compile_walk_still_reads_an_unparsable_python_script,
    test_a_nextest_older_than_the_pin_stops_the_preflight,
    test_the_preflight_reads_its_pin_from_the_tool_pin_contract,
    test_no_warmup_names_a_non_ci_nextest_profile,
    test_scripts_config_budget_annotation,
    test_runtime_net_lane_budget_annotation,
    test_runtime_net_lane_rebuilds_libhew,
    test_zero_timeout_fails_closed,
    test_compiler_pipeline_rs_change_includes_vertical_slice_oracle,
    test_compiler_pipeline_lane_includes_checked_mir_verify,
    test_types_lane_includes_checked_mir_verify,
    test_make_test_compiler_pipeline_recipe_keeps_consumer_corpus_packages,
    test_docs_only_change_does_not_include_vertical_slice_oracle,
    test_comprehensive_profile_reserves_smoke_for_local_opt_in,
    test_parser_plus_types_narrow_multi_bucket_uses_types_lane,
    test_hew_tests_path_routes_to_hew_tests_lane,
    test_parser_path_runs_formatter_property,
    test_vertical_slice_source_runs_formatter_property,
    test_std_hew_file_adds_hew_suite_addon,
    test_fallback_lane_includes_hew_suite_ratchets,
    test_stdlib_execution_proof_authorities_route_to_their_gate,
    # Slice 2 positive bucket-routing tests
    test_hew_hir_routes_to_compiler_pipeline_lane,
    test_hew_codegen_rs_routes_to_compiler_pipeline_lane,
    test_hew_compile_routes_to_cli_lane,
    test_hew_cabi_routes_to_cli_lane,
    test_hew_capability_gen_routes_to_cli_lane,
    test_hew_wasm_routes_to_wasm_lane,
    test_compiler_pipeline_absorbs_types_bucket_in_mixed_diff,
    test_leaf_crate_runs_only_its_reverse_dependency_closure,
    test_analysis_change_runs_known_dependents_without_workspace,
    test_hosted_linux_executes_the_dispatcher_directly,
    test_compiled_hew_aggregate_owns_hosted_full_suite_verdicts,
    test_selected_commands_are_unique,
    test_selector_exports_fail_closed_compile_requirement,
    test_a_push_to_main_stops_at_the_first_failing_gate,
    test_every_job_waits_for_a_green_main,
    test_trunk_health_blocks_on_a_confirmed_red_main,
    test_trunk_health_passes_on_a_green_main,
    test_trunk_health_fails_open_when_the_api_call_fails,
    test_trunk_health_fails_open_on_an_unreadable_response,
    test_a_labelled_fix_for_main_runs_against_a_red_main,
    # Shard partitioning
    test_shard_plan_is_exhaustive_and_disjoint_for_every_profile,
    test_shard_partition_preserves_relative_command_order,
    test_ordering_dependent_commands_share_a_shard,
    test_shard_selection_runs_exactly_its_planned_commands,
    test_shard_one_owns_the_baseline_precheck,
    test_shard_warmup_is_scoped_to_that_shard_commands,
    test_invalid_shard_specs_fail_closed,
    test_shard_plan_prints_the_full_assignment_and_runs_nothing,
    test_help_documents_the_shard_flags,
    test_hosted_linux_matrix_matches_the_dispatcher_shard_denominator,
    # Failure diagnostics
    test_failing_command_emits_an_annotation_and_a_result_table,
    test_first_failure_extraction_covers_every_gate_failure_shape,
    test_timeout_reports_the_budget_not_a_stray_log_line,
    test_counterfactual_output_never_becomes_the_reported_failure,
    test_counterfactual_marker_is_shared_by_every_producer,
    test_counterfactual_output_check_reuses_the_extractor_authority,
    test_signal_deaths_are_named_not_swallowed_by_the_fallback,
    test_counterfactual_marker_must_be_a_line_prefix,
    test_counterfactual_check_rejects_a_gate_that_proves_nothing,
    test_every_rostered_gate_replays_its_counterfactual,
    test_no_annotation_is_emitted_outside_github_actions,
]

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
