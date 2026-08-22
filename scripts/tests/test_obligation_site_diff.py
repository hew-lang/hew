#!/usr/bin/env python3
"""Unit tests for the name-insensitive obligation site key."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[2]
DIFF_PATH = ROOT / "scripts" / "obligation-site-diff.py"
SPEC = importlib.util.spec_from_file_location("obligation_site_diff", DIFF_PATH)
assert SPEC is not None and SPEC.loader is not None
DIFF = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(DIFF)


def per_exit(function: str, owner: str, exit_label: str) -> dict:
    """A diagnostic in the pre-aggregation rendering: one per unbalanced exit."""
    return {
        "code": "ObligationUnderReleased",
        "message": (
            f"obligation balance in `{function}`: owned local `{owner}` is never "
            f"released on an exit path (leak): owned local `{owner}` reaches "
            f"{exit_label} with at least 1 owner mint(s), but at most 0 "
            "discharge(s) on every path modelling"
        ),
        "notes": [{"message": "MIR kind: ObligationUnderReleased"}],
    }


def aggregated(function: str, owner: str, exits: str) -> dict:
    """A diagnostic in the aggregated rendering: one per owner, exits in a note."""
    return {
        "code": "ObligationUnderReleased",
        "message": (
            f"obligation balance in `{function}`: owned value `{owner}` is never "
            "released on 1 exit path(s) (leak)"
        ),
        "notes": [
            {"message": "MIR kind: ObligationUnderReleased"},
            {"message": f"unreleased exits: {exits}"},
        ],
    }


class SiteKeyTests(unittest.TestCase):
    def test_renaming_the_owner_keeps_the_same_site(self) -> None:
        before = DIFF.diagnostic_sites(
            per_exit("parse_result", "__hew_call_scrutinee", "return[bb2]"),
            "src/stage.hew",
        )
        after = DIFF.diagnostic_sites(
            aggregated("parse_result", "try_parse(...)", "bb2"), "src/stage.hew"
        )
        self.assertEqual(before, after)
        self.assertEqual(before, ("src/stage.hew", "parse_result", (2,)))

    def test_aggregating_two_exits_is_not_the_same_site_as_one(self) -> None:
        one = DIFF.diagnostic_sites(
            aggregated("parse_result", "try_parse(...)", "bb2"), "src/stage.hew"
        )
        two = DIFF.diagnostic_sites(
            aggregated("parse_result", "try_parse(...)", "bb2, bb11"),
            "src/stage.hew",
        )
        self.assertNotEqual(one, two)

    def test_same_function_name_in_another_file_is_another_site(self) -> None:
        here = DIFF.diagnostic_sites(
            aggregated("parse_result", "try_parse(...)", "bb2"), "src/a.hew"
        )
        there = DIFF.diagnostic_sites(
            aggregated("parse_result", "try_parse(...)", "bb2"), "src/b.hew"
        )
        self.assertNotEqual(here, there)

    def test_other_diagnostic_kinds_are_not_sites(self) -> None:
        self.assertIsNone(
            DIFF.diagnostic_sites(
                {"code": "ObligationOverReleased", "message": "x"}, "src/a.hew"
            )
        )


if __name__ == "__main__":
    unittest.main()
