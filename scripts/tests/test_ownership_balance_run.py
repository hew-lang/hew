#!/usr/bin/env python3
"""Unit tests for bounded obligation runtime-oracle diagnostics."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
RUN_PATH = ROOT / "tests" / "ownership-balance" / "run.py"
SPEC = importlib.util.spec_from_file_location("ownership_balance_run", RUN_PATH)
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


class BaselineRuntimeModeTests(unittest.TestCase):
    """The hard-cutover baseline admits only checked, clean, or refused rows."""

    def read(self, rows: str) -> dict:
        with tempfile.NamedTemporaryFile(
            mode="w", encoding="utf-8", suffix=".tsv", delete=False
        ) as stream:
            stream.write(rows)
            path = Path(stream.name)
        original = RUN.BASELINE
        RUN.BASELINE = path
        try:
            return RUN.read_baseline()
        finally:
            RUN.BASELINE = original
            path.unlink(missing_ok=True)

    def test_leaking_runtime_mode_is_refused(self) -> None:
        with self.assertRaises(ValueError) as raised:
            self.read("silent.hew\t0\t0\t0\tleaks\n")
        self.assertIn("unknown runtime mode", str(raised.exception))

    def test_clean_row_round_trips(self) -> None:
        rows = self.read("clean.hew\t0\t0\t0\tclean\n")
        self.assertEqual(rows["clean.hew"], (0, 0, 0, RUN.RUNTIME_CLEAN))

    def test_unknown_runtime_mode_is_refused(self) -> None:
        with self.assertRaises(ValueError):
            self.read("odd.hew\t1\t0\t0\tmaybe\n")


if __name__ == "__main__":
    unittest.main()
