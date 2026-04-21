import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from voice_guard import scan


class VoiceGuardTests(unittest.TestCase):
    def test_scan_flags_known_bad_strings(self) -> None:
        cases = [
            "- make ci-preflight ✅ (fallback lane)",
            "powered by gpt-5.4",
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
        ]

        for text in cases:
            with self.subTest(text=text):
                self.assertEqual(scan([text]), [])

    def test_claude_path_allowed_inside_fenced_code_block_only(self) -> None:
        findings = scan(
            [
                "```text\n.claude/CLAUDE.md\n```",
                "Please remove .claude/JOURNEY.md from the PR body.",
            ]
        )

        self.assertEqual(len(findings), 1)
        self.assertEqual(findings[0].match, ".claude/")


if __name__ == "__main__":
    unittest.main()
