import sys
import unittest
from pathlib import Path
from uuid import uuid4

sys.path.insert(0, str(Path(__file__).resolve().parent))

from voice_guard import _load_commits_file, scan


class VoiceGuardTests(unittest.TestCase):
    def _write_commit_fixture(self, suffix: str, contents: str) -> Path:
        fixture_dir = Path(__file__).resolve().parent / ".voice_guard_testdata"
        fixture_dir.mkdir(exist_ok=True)
        path = fixture_dir / f"{uuid4().hex}{suffix}"
        path.write_text(contents, encoding="utf-8")
        self.addCleanup(path.unlink, missing_ok=True)
        self.addCleanup(self._cleanup_fixture_dir, fixture_dir)
        return path

    def _cleanup_fixture_dir(self, fixture_dir: Path) -> None:
        try:
            fixture_dir.rmdir()
        except OSError:
            pass

    def test_scan_flags_known_bad_strings(self) -> None:
        cases = [
            "- make ci-preflight ✅ (fallback lane)",
            "powered by gpt-5.4",
            "`review_worktree --models gpt-5.4` returned findings",
            "review: APPROVED",
            "Wave 3",
            "this lane does the full local CI pass",
            "Review PASS",
        ]

        for text in cases:
            with self.subTest(text=text):
                findings = scan([text])
                self.assertGreater(len(findings), 0)

    def test_scan_allows_known_good_edge_cases(self) -> None:
        cases = [
            "lane change in routing",
            "See CLAUDE.md for contributor guidance",
            "pass: the test passes",
            "landed safely after the release cut",
            "This vocabulary list stays neutral.",
            "center-lane markings were repainted before sunrise",
            "the change was approved by the owner",
        ]

        for text in cases:
            with self.subTest(text=text):
                self.assertEqual(scan([text]), [])

    def test_non_plaintext_content_is_ignored_for_all_patterns(self) -> None:
        findings = scan(
            [
                "```text\n.claude/CLAUDE.md\n```",
                "```text\nAPPROVED\nfallback lane\nWave 3\ngpt-5.4\n```",
                "Mention `fallback lane` as a forbidden literal.",
                "Mention `.claude/CLAUDE.md` as a forbidden literal.",
                "Please remove .claude/JOURNEY.md from the PR body.",
            ]
        )

        self.assertEqual(len(findings), 1)
        self.assertEqual(findings[0].match, ".claude/")

    def test_commit_file_scan_flags_bad_commit_message(self) -> None:
        path = self._write_commit_fixture(
            ".json",
            '[{"sha":"abc123","messageHeadline":"Keep wording neutral","messageBody":"APPROVED"}]',
        )

        entries = _load_commits_file(path)

        self.assertEqual(len(entries), 1)
        self.assertEqual(len(scan([entry.text for entry in entries])), 1)

    def test_commit_file_scan_allows_neutral_commit_message(self) -> None:
        path = self._write_commit_fixture(
            ".json",
            '[{"sha":"def456","messageHeadline":"Refine vocabulary list","messageBody":"Use plain reviewer guidance."}]',
        )

        entries = _load_commits_file(path)

        self.assertEqual(len(entries), 1)
        self.assertEqual(scan([entry.text for entry in entries]), [])


if __name__ == "__main__":
    unittest.main()
