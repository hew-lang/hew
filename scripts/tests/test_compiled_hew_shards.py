#!/usr/bin/env python3
"""Counterfactual tests for compiled-Hew shard aggregation."""

from __future__ import annotations

from pathlib import Path
import os
import re
import subprocess
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "compiled-hew-shards.py"
WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
XTASK = ROOT / "xtask" / "src" / "build_system.rs"
INVENTORY_HELPER = ROOT / "scripts" / "lib" / "hew_test_inventory.py"
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
            ET.SubElement(testcase, "failure", message="forced")
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
            ],
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.assertEqual(result.returncode, expect, result.stdout + result.stderr)
        return result

    def test_complete_disjoint_union_passes_both_gates(self) -> None:
        self.assertIn("suite passed", self.aggregate("ratchet").stdout)
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
        self.assertIn("contain failing tests", result.stderr)


class CompiledHewVerdictCacheTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.compiler = self.root / "hew"
        self.counter = self.root / "counter"
        self.cache = self.root / "cache"
        self.output = self.root / "output"
        self.compiler.write_text(
            "#!/bin/sh\n"
            'if [ "$3" = "--list" ]; then\n'
            "  printf 'tests/hew/sample.hew::sample\\n'\n"
            "  exit 0\n"
            "fi\n"
            f"printf x >> '{self.counter}'\n"
            'printf \'%s\\n\' \'<testsuites tests="1" failures="0" skipped="0"><testsuite><testcase classname="tests/hew/sample.hew" name="sample"/></testsuite></testsuites>\'\n',
            encoding="utf-8",
        )
        self.compiler.chmod(0o755)
        (self.root / "libhew.a").write_bytes(b"archive-v1")

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def run_shard(self, extra_env: dict[str, str] | None = None) -> None:
        environment = os.environ.copy()
        environment["HEW_VERDICT_CACHE_DIR"] = str(self.cache)
        if extra_env:
            environment.update(extra_env)
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                "run",
                "--compiler",
                str(self.compiler),
                "--partition",
                "hash:1/1",
                "--output-dir",
                str(self.output),
            ],
            cwd=ROOT,
            env=environment,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_key_covers_compiler_archive_and_semantic_environment(self) -> None:
        self.run_shard()
        self.assertEqual(self.counter.read_text(encoding="utf-8"), "xx")
        self.run_shard()
        self.assertEqual(self.counter.read_text(encoding="utf-8"), "xx")

        self.compiler.write_text(
            self.compiler.read_text(encoding="utf-8") + "# compiler-v2\n",
            encoding="utf-8",
        )
        self.run_shard()
        self.assertEqual(self.counter.read_text(encoding="utf-8"), "xxxx")

        (self.root / "libhew.a").write_bytes(b"archive-v2")
        self.run_shard()
        self.assertEqual(self.counter.read_text(encoding="utf-8"), "xxxxxx")

        self.run_shard({"HEW_TEST_SEED": "different"})
        self.assertEqual(self.counter.read_text(encoding="utf-8"), "xxxxxxxx")


class CompiledHewWorkflowContractTests(unittest.TestCase):
    def setUp(self) -> None:
        self.workflow = WORKFLOW.read_text(encoding="utf-8")
        self.xtask = XTASK.read_text(encoding="utf-8")
        self.inventory_helper = INVENTORY_HELPER.read_text(encoding="utf-8")

    def test_one_certified_build_feeds_all_four_shards(self) -> None:
        self.assertEqual(
            self.workflow.count("cargo xtask gate compiled-artifact-pack"), 1
        )
        self.assertIn("shard: [1, 2, 3, 4]", self.workflow)
        self.assertIn("HEW_SHARD_PARTITION: hash:${{ matrix.shard }}/4", self.workflow)
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
            self.assertIn(
                "RUN_CODE_PATH: ${{ needs.changes.outputs.selected_compile }}",
                section,
            )
        self.assertGreaterEqual(
            self.workflow.count("cargo xtask gate compiled-artifact-unpack"), 2
        )

    def test_aggregate_uses_an_independent_full_inventory_and_both_gates(self) -> None:
        self.assertIn(
            "cargo xtask gate hew-inventory",
            self.workflow,
        )
        inventory_gate = self.xtask.split("fn hew_inventory_gate", 1)[1].split(
            "\nfn ", 1
        )[0]
        self.assertIn('"scripts/lib/hew_test_inventory.py"', inventory_gate)
        self.assertIn('tests.glob("*.hew")', self.inventory_helper)
        self.assertIn('"--list", "--allow-empty"', self.inventory_helper)
        self.assertIn("cargo xtask gate hew-ratchet", self.workflow)
        self.assertIn("cargo xtask gate o2-differential", self.workflow)
        self.assertEqual(self.workflow.count("HEW_SHARD_COUNT: 4"), 2)

    def test_established_required_check_requires_both_parallel_branches(self) -> None:
        required = self.workflow.split("  linux-required:\n", 1)[1].split(
            "\n  # Code coverage", 1
        )[0]
        self.assertIn("name: Build & test (Linux)", required)
        self.assertIn(
            "needs: [changes, build-and-test, compiled-hew-aggregate]", required
        )
        self.assertIn('test "$RUST_GATES_RESULT" = success', required)
        self.assertIn('test "$COMPILED_HEW_RESULT" = success', required)

    def test_verdict_cache_restores_only_the_matching_shard(self) -> None:
        prefix = (
            "hew-verdict-v1-${{ runner.os }}-${{ runner.arch }}-${{ matrix.shard }}-"
        )
        self.assertIn("actions/cache/restore@", self.workflow)
        self.assertIn("actions/cache/save@", self.workflow)
        self.assertIn(f"key: {prefix}${{{{ github.run_id }}}}", self.workflow)
        self.assertIn(f"restore-keys: |\n            {prefix}", self.workflow)


if __name__ == "__main__":
    unittest.main()
