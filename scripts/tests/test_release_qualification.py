#!/usr/bin/env python3
"""Exercise release qualification guards and candidate tagging without network access."""

from __future__ import annotations

import os
from pathlib import Path
import re
import subprocess
import tempfile
import textwrap
import unittest


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = (ROOT / ".github/workflows/release-gate.yml").read_text()
RUNBOOK = (ROOT / "docs/release-runbook.md").read_text()
WORKFLOW_SHA = "a" * 40


def step_script(name: str) -> str:
    step = WORKFLOW.split(f"      - name: {name}\n", 1)[1]
    script = step.split("        run: |\n", 1)[1]
    lines = []
    for line in script.splitlines():
        if line and not line.startswith("          "):
            break
        lines.append(line)
    return textwrap.dedent("\n".join(lines)) + "\n"


def run_bash(
    script: str, cwd: Path, env: dict[str, str]
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["bash", "-c", script], cwd=cwd, env=env, text=True, capture_output=True
    )


class QualificationInputs(unittest.TestCase):
    def resolve(
        self, source: str = "", platforms: str = ""
    ) -> tuple[subprocess.CompletedProcess[str], str]:
        with tempfile.TemporaryDirectory() as tmp:
            output = Path(tmp) / "output"
            output.touch()
            env = dict(
                os.environ,
                GITHUB_SHA=WORKFLOW_SHA,
                GITHUB_OUTPUT=str(output),
                REQUESTED_SOURCE_SHA=source,
                REQUESTED_PLATFORMS=platforms,
            )
            result = run_bash(
                step_script("Resolve qualification inputs"), Path(tmp), env
            )
            return result, output.read_text()

    def test_defaults_qualify_event_commit_on_all_platforms(self) -> None:
        result, output = self.resolve()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(output, f"source_sha={WORKFLOW_SHA}\nplatforms=all\n")

    def test_resume_preserves_the_requested_candidate(self) -> None:
        source = "b" * 40
        result, output = self.resolve(source, "windows-intel-macos")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            output, f"source_sha={source}\nplatforms=windows-intel-macos\n"
        )

    def test_invalid_source_has_no_executable_job_outputs(self) -> None:
        for source in [
            "main",
            "a" * 39,
            "a" * 41,
            "A" * 40,
            "a" * 40 + "\nplatforms=all",
            "$(exit 0)",
        ]:
            with self.subTest(source=source):
                result, output = self.resolve(source)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("full lowercase commit SHA", result.stdout)
                self.assertEqual(output, "")

    def test_unknown_scope_has_no_executable_job_outputs(self) -> None:
        result, output = self.resolve(platforms="windows-intel-macos\nsource_sha=main")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("unsupported qualification platforms", result.stdout)
        self.assertEqual(output, "")

    def test_every_platform_waits_for_validation_and_uses_its_candidate(self) -> None:
        jobs = WORKFLOW.split("jobs:\n", 1)[1]
        blocks = re.split(r"(?m)^  ([a-z][a-z0-9_-]*):\n", jobs)
        self.assertEqual(len(blocks[1::2]), 8)
        for name, block in zip(blocks[1::2], blocks[2::2]):
            with self.subTest(job=name):
                if name == "gate-release-checks":
                    self.assertLess(
                        block.index("Resolve qualification inputs"),
                        block.index("uses: actions/checkout@"),
                    )
                    self.assertIn(
                        "ref: ${{ steps.qualification.outputs.source_sha }}", block
                    )
                else:
                    self.assertIn("    needs: gate-release-checks\n", block)
                    self.assertIn(
                        "ref: ${{ needs.gate-release-checks.outputs.source_sha }}",
                        block,
                    )
                    self.assertNotIn("if: always()", block.split("    steps:", 1)[0])
                    self.assertNotIn("inputs.platforms", block)
                    self.assertNotIn("inputs.source_sha", block)

    def test_every_junit_check_targets_the_validated_candidate(self) -> None:
        reports = WORKFLOW.split("uses: mikepenz/action-junit-report@")[1:]
        self.assertEqual(len(reports), 5)
        for report in reports:
            step = report.split("\n      - ", 1)[0].split("\n  gate-", 1)[0]
            self.assertIn(
                "commit: ${{ needs.gate-release-checks.outputs.source_sha }}", step
            )
            self.assertIn("fail_on_failure: true", step)
            self.assertIn("fail_on_parse_error: true", step)
            self.assertIn("require_tests: true", step)


class CandidateGuards(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.repo = self.root / "candidate"
        self.remote = self.root / "remote.git"
        self.repo.mkdir()
        # Fixture commits must not invoke the developer's hooks or signing agent.
        self.env = dict(
            os.environ,
            GIT_CONFIG_GLOBAL=os.devnull,
            GIT_CONFIG_NOSYSTEM="1",
            GIT_AUTHOR_NAME="Release Test",
            GIT_AUTHOR_EMAIL="release@example.invalid",
            GIT_COMMITTER_NAME="Release Test",
            GIT_COMMITTER_EMAIL="release@example.invalid",
        )
        self.git("init", "-q", "-b", "main")
        files = {
            "Cargo.toml": '[workspace.package]\nversion = "1.2.3"\n',
            "hew-sandbox-vm/package.json": '{"version":"1.2.3"}\n',
            "docs/syntax-data.json": '{"version":"1.2.3"}\n',
            "docs/releases/v1.2.3.md": "Release notes\n",
        }
        for name, content in files.items():
            path = self.repo / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(content)
        script = self.repo / "scripts/release-identity.sh"
        script.parent.mkdir()
        script.write_bytes((ROOT / "scripts/release-identity.sh").read_bytes())
        script.chmod(0o755)
        self.git("add", ".")
        self.git("commit", "-qm", "base")
        self.base = self.git("rev-parse", "HEAD").stdout.strip()
        self.git("switch", "-qc", "release/v1.2.3")
        (self.repo / "candidate.txt").write_text("qualified candidate\n")
        self.git("add", ".")
        self.git("commit", "-qm", "candidate")
        self.candidate = self.git("rev-parse", "HEAD").stdout.strip()
        self.git("switch", "-q", "main")
        (self.repo / "main.txt").write_text("later development\n")
        self.git("add", ".")
        self.git("commit", "-qm", "later main")
        self.unmerged_main = self.git("rev-parse", "HEAD").stdout.strip()
        self.git("merge", "--no-ff", "-qm", "merge candidate", "release/v1.2.3")
        self.git("init", "--bare", "-q", str(self.remote))
        self.git("remote", "add", "origin", str(self.remote))
        self.git("push", "-q", "origin", "main")
        self.git("switch", "-q", "--detach", self.candidate)
        self.env.update(
            SOURCE_SHA=self.candidate,
            WORKFLOW_SHA=WORKFLOW_SHA,
            PLATFORMS="windows-intel-macos",
            BRANCH="main",
            GITHUB_STEP_SUMMARY=str(self.root / "summary"),
            release_sha=self.candidate,
        )

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def git(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["git", *args],
            cwd=self.repo,
            env=self.env,
            check=True,
            text=True,
            capture_output=True,
        )

    def identity(self, **env: str) -> subprocess.CompletedProcess[str]:
        return run_bash(
            step_script("Check release identity"), self.repo, dict(self.env, **env)
        )

    def tagging(self, **env: str) -> subprocess.CompletedProcess[str]:
        phase = RUNBOOK.split("## Phase 5", 1)[1]
        guards = phase.split("```bash\n", 1)[1].split("```", 1)[0]
        return run_bash(guards, self.repo, dict(self.env, **env))

    def test_identity_records_actual_source_workflow_and_partial_scope(self) -> None:
        result = self.identity()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        summary = (self.root / "summary").read_text()
        for value in [
            self.candidate,
            WORKFLOW_SHA,
            "windows-intel-macos",
            "Partial qualification",
        ]:
            self.assertIn(value, summary)

    def test_identity_rejects_a_different_checkout(self) -> None:
        result = self.identity(SOURCE_SHA=self.base)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("checkout does not match", result.stdout)
        self.assertFalse((self.root / "summary").exists())

    def test_identity_rejects_mismatched_release_branch_version(self) -> None:
        result = self.identity(BRANCH="release/v2.0.0")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("carries version v1.2.3", result.stdout)

    def test_tagging_accepts_recorded_candidate_with_deleted_remote_release_branch(
        self,
    ) -> None:
        self.assertEqual(
            self.git("ls-remote", "origin", "refs/heads/release/v1.2.3").stdout, ""
        )
        result = self.tagging()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(self.git("rev-parse", "HEAD").stdout.strip(), self.candidate)
        self.assertNotEqual(
            self.git("rev-parse", "origin/main").stdout.strip(), self.candidate
        )

    def test_tagging_rejects_unrecorded_or_invalid_candidate(self) -> None:
        for value in ["", "main"]:
            with self.subTest(value=value):
                self.assertNotEqual(self.tagging(release_sha=value).returncode, 0)

    def test_tagging_rejects_current_main_instead_of_recorded_candidate(self) -> None:
        self.git("switch", "-q", "main")
        self.assertNotEqual(self.tagging().returncode, 0)

    def test_tagging_rejects_dirty_candidate(self) -> None:
        (self.repo / "candidate.txt").write_text("unqualified change\n")
        self.assertNotEqual(self.tagging().returncode, 0)

    def test_tagging_rejects_candidate_absent_from_main(self) -> None:
        subprocess.run(
            [
                "git",
                "--git-dir",
                str(self.remote),
                "update-ref",
                "refs/heads/main",
                self.unmerged_main,
            ],
            env=self.env,
            check=True,
            text=True,
            capture_output=True,
        )
        self.assertNotEqual(self.tagging().returncode, 0)


if __name__ == "__main__":
    unittest.main()
