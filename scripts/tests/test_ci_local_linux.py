#!/usr/bin/env python3
"""Exercise remote-CI worktree preservation with local Git and an SSH stand-in."""

from pathlib import Path
import os
import shutil
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "ci-local-linux.sh"


@unittest.skipIf(os.name == "nt", "the SSH helper runs on POSIX hosts")
class LocalLinuxTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.home = self.root / "remote home"
        self.repo = self.home / "clone"
        self.repo.mkdir(parents=True)
        self.git = shutil.which("git")
        self.env = os.environ | {
            "GIT_CONFIG_GLOBAL": os.devnull,
            "GIT_CONFIG_NOSYSTEM": "1",
            "CI_LINUX_HOST": "fixture",
            "HEW_CI_REMOTE_REL": "clone",
            "HEW_CI_REMOTE_WORKTREE_REL": "runs with spaces",
            "HEW_CI_LLVM_PREFIX": "/llvm with spaces",
            "REMOTE_HOME": str(self.home),
            "REMOTE_REPO": str(self.repo),
            "CALL_LOG": str(self.root / "calls"),
            "REAL_GIT": self.git,
        }
        self.run_git("init", "-q", str(self.repo))
        (self.repo / "tracked").write_text("keep me\n")
        self.run_git("-C", str(self.repo), "add", "tracked")
        self.run_git(
            "-C",
            str(self.repo),
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "-qm",
            "fixture",
        )
        self.sha = self.run_git(
            "-C", str(self.repo), "rev-parse", "HEAD"
        ).stdout.strip()
        self.env["FIXTURE_SHA"] = self.sha
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.wrapper(
            "git",
            """
if [ "$1" = -C ]; then
    shift 2
    case "$1" in
        rev-parse) printf '%s\\n' "$FIXTURE_SHA"; exit 0 ;;
        push) printf 'push\\n' >> "$CALL_LOG"; exit 0 ;;
    esac
fi
exec "$REAL_GIT" "$@"
""",
        )
        self.wrapper(
            "ssh",
            """
printf 'ssh\\n' >> "$CALL_LOG"
shift
HOME="$REMOTE_HOME" exec bash -c "$1"
""",
        )
        self.wrapper(
            "make",
            """
printf 'make %s %s %s\\n' "$PWD" "$1" "$LLVM_SYS_221_PREFIX" >> "$CALL_LOG"
test "$(git rev-parse HEAD)" = "$FIXTURE_SHA"
exit "${MAKE_STATUS:-0}"
""",
        )
        self.env["PATH"] = str(self.bin) + os.pathsep + self.env["PATH"]

    def run_git(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [self.git, *args],
            env=self.env,
            check=True,
            capture_output=True,
            text=True,
        )

    def wrapper(self, name: str, body: str) -> None:
        path = self.bin / name
        path.write_text("#!/bin/sh\nset -eu\n" + body)
        path.chmod(0o755)

    def run_helper(self, step: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["bash", str(SCRIPT), step],
            env=self.env,
            capture_output=True,
            text=True,
        )

    def test_invalid_step_or_path_has_no_remote_side_effects(self) -> None:
        result = self.run_helper("vertical-slice")
        self.assertEqual(result.returncode, 2, result.stderr)
        self.env["HEW_CI_REMOTE_WORKTREE_REL"] = "../other"
        result = self.run_helper("lint")
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertFalse((self.root / "calls").exists())

    def test_retries_preserve_dirty_worktrees_and_propagate_failure(self) -> None:
        result = self.run_helper("test-vertical-slice")
        self.assertEqual(result.returncode, 0, result.stderr)
        parent = self.home / "runs with spaces"
        (first,) = parent.iterdir()
        (first / "tracked").write_text("unfinished work\n")
        (first / "untracked").write_text("keep this too\n")
        self.env["MAKE_STATUS"] = "7"
        result = self.run_helper("all")
        self.assertEqual(result.returncode, 7, result.stderr)
        self.assertEqual(len(list(parent.iterdir())), 2)
        self.assertEqual((first / "tracked").read_text(), "unfinished work\n")
        self.assertEqual((first / "untracked").read_text(), "keep this too\n")
        calls = (self.root / "calls").read_text()
        self.assertIn("test-vertical-slice /llvm with spaces", calls)
        self.assertIn("preflight /llvm with spaces", calls)
        self.assertIn("Retained CI worktree:", result.stdout)


if __name__ == "__main__":
    unittest.main()
