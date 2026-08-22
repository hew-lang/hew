#!/usr/bin/env python3
"""Unit tests for bounded obligation runtime-oracle diagnostics."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import unittest


RUN_PATH = Path(__file__).with_name("run.py")
SPEC = importlib.util.spec_from_file_location("obligation_advisory_run", RUN_PATH)
assert SPEC is not None and SPEC.loader is not None
RUN = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUN)


class BoundedRuntimeReportTests(unittest.TestCase):
    def test_caps_preview_and_persists_complete_report(self) -> None:
        report = "\n".join(f"line {index}" for index in range(45)) + "\n"
        rendered = RUN.bounded_runtime_report(
            "nested/main.hew", "oracle failed", report
        )
        marker = "full report: "
        report_path = Path(rendered.split(marker, 1)[1].removesuffix("]"))
        try:
            self.assertIn("line 39\n", rendered)
            self.assertNotIn("line 40\n", rendered)
            self.assertIn("[5 additional line(s) omitted;", rendered)
            self.assertEqual(report_path.read_text(encoding="utf-8"), report)
        finally:
            report_path.unlink(missing_ok=True)

    def test_short_report_names_zero_omitted_lines(self) -> None:
        rendered = RUN.bounded_runtime_report(
            "short.hew", "oracle failed", "one\ntwo\n"
        )
        marker = "full report: "
        report_path = Path(rendered.split(marker, 1)[1].removesuffix("]"))
        try:
            self.assertIn("one\ntwo\n", rendered)
            self.assertIn("[0 additional line(s) omitted;", rendered)
        finally:
            report_path.unlink(missing_ok=True)


if __name__ == "__main__":
    unittest.main()
