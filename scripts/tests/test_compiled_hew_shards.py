#!/usr/bin/env python3
"""Counterfactual tests for compiled-Hew shard aggregation."""

from __future__ import annotations

from pathlib import Path
import re
import subprocess
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "compiled-hew-shards.py"
WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
COUNT = 1169
SHARDS = 4


def identity(index: int) -> str:
    return f"tests/hew/generated_{index // 10}_test.hew::test_{index}"


def write_junit(
    path: Path, identities: list[str], failed: set[str] | None = None
) -> None:
    failed = failed or set()
    root = ET.Element(
        "testsuites",
        tests=str(len(identities)),
        failures=str(len(failed)),
        skipped="0",
    )
    suite = ET.SubElement(
        root,
        "testsuite",
        name="generated",
        tests=str(len(identities)),
        failures=str(len(failed)),
        skipped="0",
    )
    for value in identities:
        classname, name = value.rsplit("::", 1)
        testcase = ET.SubElement(suite, "testcase", classname=classname, name=name)
        if value in failed:
            failure = ET.SubElement(testcase, "failure", message="forced assertion")
            failure.text = "forced diagnostic text"
            system_out = ET.SubElement(testcase, "system-out")
            system_out.text = "forced fixture output"
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

    def aggregate(self, mode: str, expect: int = 0) -> subprocess.CompletedProcess[str]:
        result = subprocess.run(
            [
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
            ],
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

    def test_unexpected_o0_failure_fails_ratchet(self) -> None:
        values = self.full[0::SHARDS]
        write_junit(self.reports / "hew-o0-shard-1.xml", values, failed={values[0]})
        result = self.aggregate("ratchet", expect=1)
        self.assertIn("failure set differs", result.stderr)

    def test_report_names_the_failed_shard_test_and_diagnostic(self) -> None:
        values = self.full[0::SHARDS]
        write_junit(self.reports / "hew-o0-shard-1.xml", values, failed={values[0]})
        result = self.report()
        self.assertIn("COMPILED_HEW_FAILURE shard=1 suite=O0", result.stdout)
        self.assertIn(f"test={values[0]}", result.stdout)
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


class CompiledHewWorkflowContractTests(unittest.TestCase):
    def setUp(self) -> None:
        self.workflow = WORKFLOW.read_text(encoding="utf-8")

    def test_one_certified_build_feeds_all_four_shards(self) -> None:
        self.assertEqual(self.workflow.count("make hew-native libhew-debug"), 1)
        self.assertIn("shard: [1, 2, 3, 4]", self.workflow)
        self.assertIn('--partition "hash:${{ matrix.shard }}/4"', self.workflow)
        self.assertIn("name: compiled-hew-linux-${{ github.sha }}", self.workflow)
        for job in (
            "compiled-hew-linux",
            "compiled-hew-shards",
            "compiled-hew-aggregate",
        ):
            start = self.workflow.index(f"  {job}:\n")
            following = re.search(
                r"^  [a-zA-Z0-9_-]+:\n", self.workflow[start + 1 :], re.MULTILINE
            )
            end = (
                len(self.workflow)
                if following is None
                else start + 1 + following.start()
            )
            section = self.workflow[start:end]
            self.assertNotIn("RUN_CODE_PATH", section)
        self.assertGreaterEqual(
            self.workflow.count("scripts/compiled-hew-artifact.py unpack"), 2
        )

    def test_aggregate_uses_an_independent_full_inventory_and_both_gates(self) -> None:
        self.assertIn("for fixture in tests/hew/*.hew; do", self.workflow)
        self.assertIn('test "$fixture" --list --allow-empty', self.workflow)
        self.assertIn(
            'LC_ALL=C sort > "${{ runner.temp }}/compiled-hew-full.txt"',
            self.workflow,
        )
        self.assertIn("make test-hew-ratchet", self.workflow)
        self.assertIn("make test-o2-differential", self.workflow)
        self.assertEqual(self.workflow.count("HEW_SHARD_COUNT=4"), 2)

    def test_failure_reporter_runs_before_the_aggregate_gates(self) -> None:
        report = self.workflow.index("Report compiled Hew shard failures")
        gate = self.workflow.index("Require every shard to complete")
        ratchet = self.workflow.index("Assert shard union and Hew ratchet")
        self.assertLess(report, gate)
        self.assertLess(gate, ratchet)
        self.assertIn("scripts/compiled-hew-shards.py report", self.workflow)

    def test_established_required_check_requires_both_parallel_branches(self) -> None:
        required = self.workflow.split("  linux-required:\n", 1)[1].split(
            "\n  # Code coverage", 1
        )[0]
        self.assertIn("name: Build & test (Linux)", required)
        self.assertIn("needs: [build-and-test, compiled-hew-aggregate]", required)
        self.assertIn("RUST_GATES_RESULT: ${{ needs.build-and-test.result }}", required)
        self.assertIn(
            "COMPILED_HEW_RESULT: ${{ needs.compiled-hew-aggregate.result }}",
            required,
        )


if __name__ == "__main__":
    unittest.main()
