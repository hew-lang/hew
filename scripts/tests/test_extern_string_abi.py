#!/usr/bin/env python3
"""Behavioural checks for checked encoding extern verification."""

import importlib.util
import unittest
from pathlib import Path

PATH = Path(__file__).resolve().parents[1] / "verify-extern-string-abi.py"
SPEC = importlib.util.spec_from_file_location("extern_string_abi", PATH)
assert SPEC and SPEC.loader
VERIFIER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(VERIFIER)


class CheckedEncodingAbi(unittest.TestCase):
    def check(self, symbol, params, result, rust_params, rust_result):
        return VERIFIER.check_signatures(
            {symbol: ("std/encoding/example.hew", 1, params, result)},
            {symbol + "_checked": ("provider.rs", rust_params, rust_result)},
        )

    def test_managed_constructor_and_string_result(self):
        for format_name in ("json", "yaml", "toml"):
            with self.subTest(format=format_name):
                value = "Hew" + format_name.title() + "Value"
                self.assertFalse(
                    self.check(
                        f"hew_{format_name}_parse",
                        ["string"],
                        "Value",
                        ["*const HewString", "*const HewValueLayout", "*mut i32"],
                        f"*mut {value}",
                    )
                )
                self.assertFalse(
                    self.check(
                        f"hew_{format_name}_stringify",
                        ["Value"],
                        "string",
                        [f"*const {value}", "*const HewValueLayout", "*mut i32"],
                        "*mut HewString",
                    )
                )

    def test_in_place_mutation(self):
        self.assertFalse(
            self.check(
                "hew_json_object_set",
                ["Value", "string", "Value"],
                "",
                [
                    "*mut HewJsonValue",
                    "*const HewString",
                    "*mut HewJsonValue",
                    "*mut i32",
                ],
                "",
            )
        )

    def test_refuses_invalid_publication_slots_and_carriers(self):
        for params, result in [
            (["*const HewString"], "*mut HewJsonValue"),
            (
                ["*const HewString", "*mut HewValueLayout", "*mut i32"],
                "*mut HewJsonValue",
            ),
            (
                ["*const HewString", "*const HewValueLayout", "*const i32"],
                "*mut HewJsonValue",
            ),
            (
                ["*const c_char", "*const HewValueLayout", "*mut i32"],
                "*mut HewJsonValue",
            ),
            (["*const HewString", "*const HewValueLayout", "*mut i32"], "i64"),
        ]:
            with self.subTest(params=params, result=result):
                self.assertTrue(
                    self.check("hew_json_parse", ["string"], "Value", params, result)
                )

    def test_raw_export_cannot_substitute_for_missing_checked_export(self):
        failures = VERIFIER.check_signatures(
            {"hew_json_parse": ("std/encoding/json.hew", 1, ["string"], "Value")},
            {"hew_json_parse": ("raw.rs", ["*const HewString"], "*mut HewJsonValue")},
        )
        self.assertIn("hew_json_parse_checked", str(failures))

    def test_in_place_mutation_requires_void_result(self):
        self.assertTrue(
            self.check(
                "hew_json_object_set",
                ["Value", "string", "Value"],
                "",
                [
                    "*mut HewJsonValue",
                    "*const HewString",
                    "*mut HewJsonValue",
                    "*mut i32",
                ],
                "*mut HewJsonValue",
            )
        )


if __name__ == "__main__":
    unittest.main()
