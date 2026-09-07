#!/usr/bin/env python3
"""Counterfactual tests for compiled-Hew shard aggregation."""

from __future__ import annotations

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "compiled-hew-shards.py"
JUNIT = ROOT / "scripts" / "lib" / "hew_junit.py"
COUNT = 41
SHARDS = 4


def identity(index: int) -> str:
    # Repeated bare names make path qualification part of every aggregation
    # counterfactual, rather than relying on one specially named fixture.
    return f"tests/hew/generated_{index // 5}_test.hew::test_{index % 5}"


def write_junit(
    path: Path,
    identities: list[str],
    failed: set[str] | None = None,
    skipped: set[str] | None = None,
    failure_kind: str = "runtime",
) -> None:
    failed = failed or set()
    skipped = skipped or set()
    root = ET.Element(
        "testsuites",
        tests=str(len(identities)),
        failures=str(len(failed)),
        skipped=str(len(skipped)),
    )
    suite = ET.SubElement(
        root,
        "testsuite",
        name="generated",
        tests=str(len(identities)),
        failures=str(len(failed)),
        skipped=str(len(skipped)),
    )
    for value in identities:
        classname, name = value.rsplit("::", 1)
        testcase = ET.SubElement(suite, "testcase", classname=classname, name=name)
        if value in failed:
            failure = ET.SubElement(
                testcase,
                "failure",
                type=failure_kind,
                message="forced assertion",
            )
            failure.text = "forced diagnostic text"
            system_out = ET.SubElement(testcase, "system-out")
            system_out.text = "forced fixture output"
        elif value in skipped:
            ET.SubElement(testcase, "skipped")
    ET.ElementTree(root).write(path, encoding="unicode", xml_declaration=True)


class CompiledHewShardTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.reports = self.root / "reports"
        self.reports.mkdir()
        self.full = [identity(index) for index in range(COUNT)]
        self.full_path = self.root / "full.txt"
        self.full_path.write_text("\n".join(self.full) + "\n", encoding="utf-8")
        self.expected = self.root / "expected.txt"
        self.expected.write_text("# no expected failures\n", encoding="utf-8")
        for shard in range(1, SHARDS + 1):
            values = self.full[shard - 1 :: SHARDS]
            (self.reports / f"hew-inventory-shard-{shard}.txt").write_text(
                "\n".join(values) + "\n", encoding="utf-8"
            )
            write_junit(self.reports / f"hew-o0-shard-{shard}.xml", values)
            write_junit(self.reports / f"hew-o2-shard-{shard}.xml", values)

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def aggregate(
        self, mode: str, expect: int = 0, strict_recoveries: bool = False
    ) -> subprocess.CompletedProcess[str]:
        command = [
            sys.executable,
            str(SCRIPT),
            "aggregate",
            "--mode",
            mode,
            "--reports-dir",
            str(self.reports),
            "--full-inventory",
            str(self.full_path),
            "--shard-count",
            str(SHARDS),
            "--expected-failures",
            str(self.expected),
        ]
        if strict_recoveries:
            command.append("--strict-recoveries")
        result = subprocess.run(
            command,
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.assertEqual(result.returncode, expect, result.stdout + result.stderr)
        return result

    def report(self) -> subprocess.CompletedProcess[str]:
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                "report",
                "--reports-dir",
                str(self.reports),
                "--shard-count",
                str(SHARDS),
            ],
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result

    def finalize(
        self, prerequisites_succeeded: bool, output: Path | None = None
    ) -> tuple[subprocess.CompletedProcess[str], Path]:
        output = output if output is not None else self.root / "published"
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                "finalize",
                "--reports-dir",
                str(self.reports),
                "--output-dir",
                str(output),
                "--full-inventory",
                str(self.full_path),
                "--expected-failures",
                str(self.expected),
                "--prerequisites-succeeded",
                str(prerequisites_succeeded).lower(),
            ],
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        return result, output

    def test_finalization_skips_matching_failures_and_preserves_diagnostics(
        self,
    ) -> None:
        values = self.full[0::SHARDS]
        tracked = values[0]
        self.expected.write_text(f"{tracked} runtime\n", encoding="utf-8")
        for label in ("o0", "o2"):
            write_junit(
                self.reports / f"hew-{label}-shard-1.xml",
                values,
                failed={tracked},
            )

        result, published = self.finalize(True)

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        root = ET.parse(published / "hew-o0-shard-1.xml").getroot()
        self.assertEqual(root.get("failures"), "0")
        self.assertEqual(root.get("skipped"), "1")
        self.assertIsNone(root.find(".//failure"))
        skipped = root.find(".//skipped")
        self.assertEqual(
            skipped.get("message"), "expected runtime failure: forced assertion"
        )
        self.assertEqual(skipped.text, "forced diagnostic text")
        self.assertEqual(
            ET.parse(self.reports / "hew-o0-shard-1.xml").getroot().get("failures"),
            "1",
        )
        action_read = subprocess.run(
            [sys.executable, str(JUNIT), str(published / "hew-o0-shard-1.xml")],
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.assertEqual(action_read.returncode, 0, action_read.stderr)
        self.assertIn("__SUMMARY__\t11\t0\t1", action_read.stdout)

    def test_failed_prerequisite_keeps_raw_reports_and_adds_visible_failure(
        self,
    ) -> None:
        result, published = self.finalize(False)

        self.assertNotEqual(result.returncode, 0)
        self.assertTrue((published / "hew-o0-shard-1.xml").is_file())
        finalization = ET.parse(published / "compiled-hew-finalization.xml").getroot()
        self.assertEqual(finalization.get("failures"), "1")
        self.assertIsNotNone(finalization.find(".//failure"))

    def test_finalization_rejects_raw_directory_as_output(self) -> None:
        before = {path.name: path.read_bytes() for path in self.reports.iterdir()}
        result, _ = self.finalize(True, self.reports)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("output must be separate", result.stderr)
        self.assertEqual(
            {path.name: path.read_bytes() for path in self.reports.iterdir()}, before
        )

    def test_successful_retry_replaces_only_owned_reports(self) -> None:
        failed, published = self.finalize(False)
        self.assertNotEqual(failed.returncode, 0)
        stale = published / "hew-o0-shard-99.xml"
        write_junit(
            stale,
            ["tests/hew/old_test.hew::old"],
            failed={"tests/hew/old_test.hew::old"},
        )
        unrelated = published / "another-suite.xml"
        unrelated.write_text("preserve unrelated output\n")

        result, published = self.finalize(True)

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertFalse((published / "compiled-hew-finalization.xml").exists())
        self.assertFalse(stale.exists())
        self.assertEqual(unrelated.read_text(), "preserve unrelated output\n")
        for raw in self.reports.glob("hew-*-shard-*.xml"):
            self.assertIsNone(ET.parse(published / raw.name).find(".//failure"))
            self.assertTrue(raw.is_file())

    def test_finalization_refuses_mismatched_expected_failure_kind(self) -> None:
        values = self.full[0::SHARDS]
        tracked = values[0]
        self.expected.write_text(f"{tracked} compile\n", encoding="utf-8")
        for label in ("o0", "o2"):
            write_junit(
                self.reports / f"hew-{label}-shard-1.xml",
                values,
                failed={tracked},
                failure_kind="runtime",
            )

        result, published = self.finalize(True)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("failure kind differs", result.stderr)
        failure = ET.parse(published / "compiled-hew-finalization.xml").find(
            ".//failure"
        )
        self.assertIsNotNone(failure)
        self.assertIn("rejected its ratchet inputs", failure.get("message"))

    def test_finalization_publishes_a_failure_when_raw_junit_is_malformed(self) -> None:
        (self.reports / "hew-o0-shard-1.xml").write_text(
            "<testsuites><testcase", encoding="utf-8"
        )

        result, published = self.finalize(True)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("malformed JUnit report", result.stderr)
        self.assertIsNotNone(
            ET.parse(published / "compiled-hew-finalization.xml").find(".//failure")
        )

    def test_recovery_is_nonblocking_and_strict_accounting_rejects_it(
        self,
    ) -> None:
        tracked = self.full[0]
        self.expected.write_text(f"{tracked} runtime\n", encoding="utf-8")

        result = self.aggregate("ratchet")
        self.assertIn("recovery accounting", result.stderr)

        strict = self.aggregate("ratchet", expect=1, strict_recoveries=True)
        self.assertIn("recoveries under strict accounting", strict.stderr)

        result, published = self.finalize(True)

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertFalse((published / "compiled-hew-finalization.xml").exists())

    def test_skipped_expected_failure_is_not_a_recovery(self) -> None:
        values = self.full[0::SHARDS]
        tracked = values[0]
        self.expected.write_text(f"{tracked} runtime\n", encoding="utf-8")
        for label in ("o0", "o2"):
            write_junit(
                self.reports / f"hew-{label}-shard-1.xml",
                values,
                skipped={tracked},
            )

        result = self.aggregate("ratchet", expect=1)
        self.assertIn("did not PASS in both O0 and O2", result.stderr)

    def test_unexpected_pass_prerequisite_failure_keeps_raw_and_adds_red_verdict(
        self,
    ) -> None:
        tracked = self.full[0]
        self.expected.write_text(f"{tracked} runtime\n", encoding="utf-8")

        result, published = self.finalize(False)

        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(
            ET.parse(published / "hew-o0-shard-1.xml").getroot().get("failures"),
            "0",
        )
        self.assertIsNotNone(
            ET.parse(published / "compiled-hew-finalization.xml").find(".//failure")
        )

    def test_finalization_publishes_a_failure_when_inventory_is_empty(self) -> None:
        self.full_path.write_text("", encoding="utf-8")

        result, published = self.finalize(True)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("inventory is empty", result.stderr)
        self.assertIsNotNone(
            ET.parse(published / "compiled-hew-finalization.xml").find(".//failure")
        )

    def test_raw_junit_nonexclusive_outcomes_fail_aggregate(self) -> None:
        path = self.reports / "hew-o0-shard-1.xml"
        tree = ET.parse(path)
        testcase = tree.find(".//testcase")
        ET.SubElement(testcase, "failure", type="runtime")
        ET.SubElement(testcase, "skipped")
        tree.getroot().set("failures", "1")
        tree.getroot().set("skipped", "1")
        tree.find(".//testsuite").set("failures", "1")
        tree.find(".//testsuite").set("skipped", "1")
        tree.write(path, encoding="unicode", xml_declaration=True)

        result = self.aggregate("ratchet", expect=1)
        self.assertIn("non-exclusive outcomes", result.stderr)

    def test_raw_junit_testsuite_summary_mismatch_fails_aggregate(self) -> None:
        path = self.reports / "hew-o0-shard-1.xml"
        tree = ET.parse(path)
        tree.find(".//testsuite").set("failures", "1")
        tree.write(path, encoding="unicode", xml_declaration=True)

        result = self.aggregate("ratchet", expect=1)
        self.assertIn("testsuite summary disagrees", result.stderr)

    def test_complete_disjoint_union_passes_both_gates(self) -> None:
        self.assertIn("ratchet passed", self.aggregate("ratchet").stdout)
        self.assertIn("differential passed", self.aggregate("differential").stdout)

    def test_missing_identity_fails_union_assertion(self) -> None:
        path = self.reports / "hew-inventory-shard-1.txt"
        values = path.read_text(encoding="utf-8").splitlines()[1:]
        path.write_text("\n".join(values) + "\n", encoding="utf-8")
        write_junit(self.reports / "hew-o0-shard-1.xml", values)
        write_junit(self.reports / "hew-o2-shard-1.xml", values)
        result = self.aggregate("ratchet", expect=1)
        self.assertIn("does not equal the full inventory", result.stderr)

    def test_overlap_fails_even_when_the_union_is_complete(self) -> None:
        path = self.reports / "hew-inventory-shard-2.txt"
        values = path.read_text(encoding="utf-8").splitlines()
        values.append(self.full[0])
        path.write_text("\n".join(values) + "\n", encoding="utf-8")
        write_junit(self.reports / "hew-o0-shard-2.xml", values)
        write_junit(self.reports / "hew-o2-shard-2.xml", values)
        result = self.aggregate("ratchet", expect=1)
        self.assertIn("inventories overlap", result.stderr)

    def test_o2_outcome_drift_fails(self) -> None:
        values = self.full[0::SHARDS]
        write_junit(self.reports / "hew-o2-shard-1.xml", values, failed={values[0]})
        result = self.aggregate("differential", expect=1)
        self.assertIn("O0/O2 shard outcomes differ", result.stderr)

    def test_o2_failure_kind_drift_fails(self) -> None:
        values = self.full[0::SHARDS]
        failed = {values[0]}
        for actual_kind in ("runtime", "timeout", "launch"):
            with self.subTest(actual_kind=actual_kind):
                write_junit(
                    self.reports / "hew-o0-shard-1.xml",
                    values,
                    failed=failed,
                    failure_kind="compile",
                )
                write_junit(
                    self.reports / "hew-o2-shard-1.xml",
                    values,
                    failed=failed,
                    failure_kind=actual_kind,
                )

                result = self.aggregate("differential", expect=1)
                self.assertIn("O0/O2 shard outcomes differ", result.stderr)
                self.assertIn(
                    f"O0=compile O2={actual_kind}",
                    result.stderr,
                )

    def test_unexpected_o0_failure_fails_ratchet(self) -> None:
        values = self.full[0::SHARDS]
        write_junit(self.reports / "hew-o0-shard-1.xml", values, failed={values[0]})
        result = self.aggregate("ratchet", expect=1)
        self.assertIn("untracked failures", result.stderr)

    def test_expected_failure_is_an_exact_path_qualified_identity(self) -> None:
        values = self.full[0::SHARDS]
        tracked = values[0]
        same_name = [
            value
            for value in self.full
            if value != tracked
            and value.rsplit("::", 1)[1] == tracked.rsplit("::", 1)[1]
        ]
        self.assertTrue(same_name)
        self.expected.write_text(f"{tracked} runtime\n", encoding="utf-8")
        write_junit(self.reports / "hew-o0-shard-1.xml", values, failed={tracked})

        self.assertIn("ratchet passed", self.aggregate("ratchet").stdout)

    def test_expected_failure_must_belong_to_the_inventory(self) -> None:
        self.expected.write_text(
            "tests/hew/not_present_test.hew::test_missing compile\n", encoding="utf-8"
        )

        result = self.aggregate("ratchet", expect=1)
        self.assertIn("absent from the full inventory", result.stderr)

    def test_duplicate_expected_failure_is_rejected(self) -> None:
        tracked = self.full[0]
        self.expected.write_text(
            f"{tracked} compile\n{tracked} compile\n", encoding="utf-8"
        )

        result = self.aggregate("ratchet", expect=1)
        self.assertIn("duplicate identities", result.stderr)

    def test_expected_failure_kind_must_match(self) -> None:
        values = self.full[0::SHARDS]
        tracked = values[0]
        self.expected.write_text(f"{tracked} compile\n", encoding="utf-8")
        for actual_kind in ("runtime", "timeout", "launch"):
            with self.subTest(actual_kind=actual_kind):
                write_junit(
                    self.reports / "hew-o0-shard-1.xml",
                    values,
                    failed={tracked},
                    failure_kind=actual_kind,
                )

                result = self.aggregate("ratchet", expect=1)
                self.assertIn("failure kinds differ", result.stderr)
                self.assertIn(
                    f"expected=compile actual={actual_kind}",
                    result.stderr,
                )

    def test_expected_failure_requires_a_supported_kind(self) -> None:
        tracked = self.full[0]
        self.expected.write_text(f"{tracked} crash\n", encoding="utf-8")

        result = self.aggregate("ratchet", expect=1)
        self.assertIn("unsupported failure kind", result.stderr)

    def test_junit_failure_requires_a_semantic_type(self) -> None:
        values = self.full[0::SHARDS]
        path = self.reports / "hew-o0-shard-1.xml"
        write_junit(path, values, failed={values[0]})
        tree = ET.parse(path)
        tree.find(".//failure").attrib.pop("type")
        tree.write(path, encoding="unicode", xml_declaration=True)

        result = self.aggregate("ratchet", expect=1)
        self.assertIn("missing or unsupported semantic type", result.stderr)

    def test_report_names_the_failed_shard_test_and_diagnostic(self) -> None:
        values = self.full[0::SHARDS]
        write_junit(self.reports / "hew-o0-shard-1.xml", values, failed={values[0]})
        result = self.report()
        self.assertIn("COMPILED_HEW_FAILURE shard=1 suite=O0", result.stdout)
        self.assertIn(f"test={values[0]}", result.stdout)
        self.assertIn("kind=runtime", result.stdout)
        self.assertIn("forced assertion", result.stdout)
        self.assertIn("forced diagnostic text", result.stdout)
        self.assertIn("forced fixture output", result.stdout)

    def test_report_survives_a_malformed_shard_report(self) -> None:
        values = self.full[0::SHARDS]
        (self.reports / "hew-o0-shard-1.xml").write_text(
            "<testsuite><testcase", encoding="utf-8"
        )
        write_junit(self.reports / "hew-o2-shard-2.xml", values, failed={values[0]})
        result = self.report()
        self.assertIn("COMPILED_HEW_REPORT_UNREADABLE shard=1", result.stderr)
        self.assertIn("COMPILED_HEW_FAILURE shard=2 suite=O2", result.stdout)


if __name__ == "__main__":
    unittest.main()
