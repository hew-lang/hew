import json
import os
import re
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci-preflight-dispatcher.sh"


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
    assert "make lint  (budget: 1200s)" in result.stdout, result.stdout
    assert "make test-hew-ratchet  (budget: 3000s)" in result.stdout, result.stdout
    assert "make test-o2-differential  (budget: 5400s)" in result.stdout, result.stdout

    fast_result = run_dispatcher("some-unclassified-root-file.txt", parallelism=32)
    assert fast_result.returncode == 0, fast_result.stderr
    assert "max(1, 16 / 32) = 1.00x" in fast_result.stdout, fast_result.stdout
    assert "make lint  (budget: 600s)" in fast_result.stdout, fast_result.stdout
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
            },
            dry_run=False,
            timeout=30,
        )
        assert result.returncode == 1, result.stdout
        assert "Failure policy: run-all (default)" in result.stdout, result.stdout
        assert "Stopping after first failed command" not in result.stdout, result.stdout
        profile_entries = json.loads(Path(profile.name).read_text())

    assert [entry["cmd"] for entry in profile_entries] == [
        "exit 7",
        "printf '%s' RUN_TWO >/dev/null",
        "printf '%s' RUN_THREE >/dev/null",
    ]
    assert [entry["status"] for entry in profile_entries] == [7, 0, 0]
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

    assert [entry["cmd"] for entry in profile_entries] == ["exit 7"]
    assert [entry["status"] for entry in profile_entries] == [7]
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
                "PREFLIGHT_TIMEOUT_NARROW": "1",
            },
            dry_run=False,
            timeout=30,
        )
        profile_entries = json.loads(Path(profile.name).read_text())

    assert result.returncode != 0, result.stdout
    assert "TIMEOUT: 'sleep 30' exceeded 1s budget" in result.stdout, result.stdout
    assert len(profile_entries) == 1, profile_entries
    assert profile_entries[0]["cmd"] == "sleep 30", profile_entries
    assert profile_entries[0]["status"] in {137, 143}, profile_entries
    assert 1 <= profile_entries[0]["elapsed_s"] <= 10, profile_entries
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
            },
            dry_run=False,
            timeout=30,
        )
        profile_entries = json.loads(Path(profile.name).read_text())

    assert result.returncode == 1, result.stdout
    assert [entry["cmd"] for entry in profile_entries] == ["true", "false", "true"]
    assert [entry["status"] for entry in profile_entries] == [0, 1, 0]
    for entry in profile_entries:
        assert isinstance(entry["elapsed_s"], int), profile_entries
        assert entry["elapsed_s"] >= 0, profile_entries


def test_scripts_config_budget_annotation() -> None:
    """scripts-config lane uses the conservative fallback budget in dry-run."""
    result = run_dispatcher("Makefile")
    assert result.returncode == 0, result.stderr
    assert "(budget: 600s)" in result.stdout, (
        f"Expected '(budget: 600s)' for scripts-config lane.\nstdout:\n{result.stdout}"
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
        assert "scripts/check-libhew-fresh.sh" in result.stdout, (
            f"Expected 'scripts/check-libhew-fresh.sh' in runtime-net commands for {path}.\n"
            f"stdout:\n{result.stdout}"
        )
        # Freshness gate must appear before the test command.
        stdlib_pos = result.stdout.index("make stdlib")
        fresh_pos = result.stdout.index("scripts/check-libhew-fresh.sh")
        test_pos = result.stdout.index("make test")
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


def test_fallback_lane_includes_smoke_tier_before_heavy() -> None:
    """Fallback lane runs the smoke tier (make ci-preflight-smoke) before make lint and make test.

    The smoke tier surfaces fmt/clippy/fast-oracle failures in <5 min before
    the expensive full suite (make lint + make playground-check + make test) is
    invoked.  Coverage is unchanged: the heavy tier runs on smoke pass.
    """
    # An unclassified path routes to the fallback (comprehensive) lane.
    result = run_dispatcher("some-unclassified-root-file.txt")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: comprehensive" in result.stdout, result.stdout
    assert "make ci-preflight-smoke" in result.stdout, (
        "Expected 'make ci-preflight-smoke' in fallback lane commands.\n"
        f"stdout:\n{result.stdout}"
    )
    assert "make lint" in result.stdout, (
        f"Expected 'make lint' in fallback lane commands.\nstdout:\n{result.stdout}"
    )
    assert "make test" in result.stdout, (
        f"Expected 'make test' in fallback lane commands.\nstdout:\n{result.stdout}"
    )
    # Smoke tier must appear before make lint in the command list.
    smoke_pos = result.stdout.index("make ci-preflight-smoke")
    lint_pos = result.stdout.index("make lint")
    test_pos = result.stdout.index("make test")
    assert smoke_pos < lint_pos < test_pos, (
        f"Expected order: make ci-preflight-smoke < make lint < make test.\n"
        f"smoke_pos={smoke_pos}, lint_pos={lint_pos}, test_pos={test_pos}\n"
        f"stdout:\n{result.stdout}"
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
    assert result.stdout.count("make hew-fmt-property") == 1, result.stdout


def test_vertical_slice_source_runs_formatter_property() -> None:
    result = run_dispatcher("tests/vertical-slice/accept/example.hew")
    assert result.returncode == 0, result.stderr
    assert result.stdout.count("make hew-fmt-property") == 1, result.stdout


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
    assert result.stdout.count("make hew-fmt-property") == 1, result.stdout
    # Ratchets must appear after make test (Rust suite runs first).
    # The budget annotation "(budget: Xs)" may appear on the same line in dry-run.
    test_pos = result.stdout.index("  - make test")
    hew_pos = result.stdout.index("make test-hew-ratchet")
    stdlib_pos = result.stdout.index("make test-stdlib-ratchet")
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
    assert "cargo nextest run --profile ci -p hew-observe" in result.stdout
    assert "--workspace" not in result.stdout
    assert "-p hew-parser" not in result.stdout


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
    assert "run: scripts/ci-preflight-dispatcher.sh --base origin/main" in workflow
    assert "run: make test-hew-ratchet" not in workflow


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
    test_scripts_config_budget_annotation,
    test_runtime_net_lane_budget_annotation,
    test_runtime_net_lane_rebuilds_libhew,
    test_zero_timeout_fails_closed,
    test_compiler_pipeline_rs_change_includes_vertical_slice_oracle,
    test_compiler_pipeline_lane_includes_checked_mir_verify,
    test_types_lane_includes_checked_mir_verify,
    test_make_test_compiler_pipeline_recipe_keeps_consumer_corpus_packages,
    test_docs_only_change_does_not_include_vertical_slice_oracle,
    test_fallback_lane_includes_smoke_tier_before_heavy,
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
    test_selector_exports_fail_closed_compile_requirement,
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
