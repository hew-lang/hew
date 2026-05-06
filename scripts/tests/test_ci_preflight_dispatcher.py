import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci-preflight-dispatcher.sh"


def run_dispatcher(
    *paths: str, extra_args: list[str] | None = None
) -> subprocess.CompletedProcess[str]:
    extra_args = extra_args or []
    return subprocess.run(
        ["bash", str(SCRIPT), "--dry-run", *extra_args, "--", *paths],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
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
    assert "  - make test-rust" in result.stdout
    assert "make test-codegen" not in result.stdout


def test_makefile_routes_to_scripts_config_profile() -> None:
    assert_scripts_config_profile(run_dispatcher("Makefile"))


def test_scripts_path_routes_to_scripts_config_profile() -> None:
    assert_scripts_config_profile(run_dispatcher("scripts/foo.sh"))


def test_nextest_config_routes_to_scripts_config_profile() -> None:
    assert_scripts_config_profile(run_dispatcher(".config/nextest.toml"))


def test_workflow_routes_to_scripts_config_profile() -> None:
    assert_scripts_config_profile(run_dispatcher(".github/workflows/ci.yml"))


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
    result = run_dispatcher("hew-codegen/src/some_new_file.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: comprehensive" in result.stdout, result.stdout
    assert "(budget: 600s)" in result.stdout, (
        f"Expected '(budget: 600s)' in dry-run output for fallback lane.\n"
        f"stdout:\n{result.stdout}"
    )


def test_dry_run_shows_budget_annotation_docs_lane() -> None:
    """docs-only changes produce 'Commands: none (docs-only)'; no budget line needed."""
    result = run_dispatcher("README.md")
    assert result.returncode == 0, result.stderr
    assert "docs-only" in result.stdout, result.stdout
    # docs lane has no commands, so no budget annotation — just confirm no crash.


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


def test_scripts_config_budget_annotation() -> None:
    """scripts-config lane (narrow) shows 180s budget in dry-run."""
    result = run_dispatcher("Makefile")
    assert result.returncode == 0, result.stderr
    assert "(budget: 180s)" in result.stdout, (
        f"Expected '(budget: 180s)' for scripts-config lane.\nstdout:\n{result.stdout}"
    )


def test_runtime_net_lane_budget_annotation() -> None:
    """runtime-net lane (narrow) shows 180s budget in dry-run."""
    result = run_dispatcher("hew-runtime/src/actor.rs")
    assert result.returncode == 0, result.stderr
    assert "Selected profile: runtime-net" in result.stdout, result.stdout
    assert "(budget: 180s)" in result.stdout, (
        f"Expected '(budget: 180s)' for runtime-net lane.\nstdout:\n{result.stdout}"
    )


_TESTS = [
    test_makefile_routes_to_scripts_config_profile,
    test_scripts_path_routes_to_scripts_config_profile,
    test_nextest_config_routes_to_scripts_config_profile,
    test_workflow_routes_to_scripts_config_profile,
    # Slice 1 instrumentation tests
    test_dry_run_shows_budget_annotation_narrow_lane,
    test_dry_run_shows_budget_annotation_fallback_lane,
    test_dry_run_shows_budget_annotation_docs_lane,
    test_help_includes_profile_json,
    test_profile_json_flag_accepted_in_dry_run,
    test_scripts_config_budget_annotation,
    test_runtime_net_lane_budget_annotation,
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
