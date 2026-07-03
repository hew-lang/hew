import os
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "lane-gates.sh"


def run_lane_gates(
    *args: str,
    env: dict[str, str] | None = None,
    timeout: float | None = None,
) -> subprocess.CompletedProcess[str]:
    run_env = None
    if env is not None:
        run_env = {**os.environ, **env}
    return subprocess.run(
        ["bash", str(SCRIPT), *args],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
        env=run_env,
        timeout=timeout,
    )


def test_single_crate_dry_run_shows_nextest_line() -> None:
    result = run_lane_gates("-p", "hew-lexer", "--dry-run")
    assert result.returncode == 0, result.stderr
    assert "cargo nextest run --profile lane -p hew-lexer" in result.stdout, (
        result.stdout
    )


def test_multi_crate_dry_run_preserves_given_order() -> None:
    result = run_lane_gates("-p", "hew-types", "-p", "hew-parser", "--dry-run")
    assert result.returncode == 0, result.stderr
    assert (
        "cargo nextest run --profile lane -p hew-types -p hew-parser" in result.stdout
    ), result.stdout


def test_missing_crate_flag_exits_nonzero_with_usage_message() -> None:
    result = run_lane_gates()
    assert result.returncode != 0
    assert "at least one -p <crate> is required" in result.stderr, result.stderr


def test_skip_oracle_dry_run_marks_fuzz_oracle_skipped() -> None:
    result = run_lane_gates("-p", "hew-lexer", "--skip-oracle", "--dry-run")
    assert result.returncode == 0, result.stderr
    assert "make fuzz-oracle  SKIPPED (--skip-oracle)" in result.stdout, result.stdout
    assert "make fuzz-oracle  (budget:" not in result.stdout, result.stdout


def test_base_flag_overrides_computed_commit_range() -> None:
    result = run_lane_gates("-p", "hew-lexer", "--base", "HEAD", "--dry-run")
    assert result.returncode == 0, result.stderr
    assert "Base ref: HEAD" in result.stdout, result.stdout
    assert "Commit range: HEAD..HEAD" in result.stdout, result.stdout


def test_unresolvable_base_exits_2_not_0_or_1() -> None:
    # lane-gates.sh resolves its repo root from its own script path (not the
    # caller's cwd), so a scratch repo must be wired in via GIT_DIR /
    # GIT_WORK_TREE rather than a plain `cwd=` override — same pattern the
    # script's own --self-test uses for this exact assertion.
    with tempfile.TemporaryDirectory() as tmpdir:
        subprocess.run(
            ["git", "init", "-q", "--initial-branch=scratch", tmpdir],
            check=True,
        )
        subprocess.run(
            ["git", "-C", tmpdir, "config", "user.email", "test@test"],
            check=True,
        )
        subprocess.run(["git", "-C", tmpdir, "config", "user.name", "test"], check=True)
        subprocess.run(
            ["git", "-C", tmpdir, "commit", "-q", "--allow-empty", "-m", "root"],
            check=True,
        )
        result = run_lane_gates(
            "-p",
            "hew-lexer",
            "--dry-run",
            env={
                "GIT_DIR": str(Path(tmpdir) / ".git"),
                "GIT_WORK_TREE": tmpdir,
            },
            timeout=10,
        )
        assert result.returncode == 2, (result.stdout, result.stderr)


def test_help_exits_0_and_prints_usage() -> None:
    result = run_lane_gates("--help")
    assert result.returncode == 0, result.stderr
    assert "Usage: scripts/lane-gates.sh" in result.stdout, result.stdout


_TESTS = [
    test_single_crate_dry_run_shows_nextest_line,
    test_multi_crate_dry_run_preserves_given_order,
    test_missing_crate_flag_exits_nonzero_with_usage_message,
    test_skip_oracle_dry_run_marks_fuzz_oracle_skipped,
    test_base_flag_overrides_computed_commit_range,
    test_unresolvable_base_exits_2_not_0_or_1,
    test_help_exits_0_and_prints_usage,
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
