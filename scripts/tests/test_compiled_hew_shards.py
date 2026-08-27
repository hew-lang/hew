#!/usr/bin/env python3
"""Counterfactual tests for compiled-Hew shard aggregation."""

from __future__ import annotations

import fnmatch
import importlib.util
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

# The fail-closed YAML subset already used by `scripts/check-gate-reachability.py`
# (and by `test_ci_workflow_contract.py`'s semantic contracts) — no third-party
# dependency, and one parser means one reading of the same file.
_reachability_spec = importlib.util.spec_from_file_location(
    "check_gate_reachability", ROOT / "scripts" / "check-gate-reachability.py"
)
_reachability = importlib.util.module_from_spec(_reachability_spec)
assert _reachability_spec.loader is not None
sys.modules.setdefault("check_gate_reachability", _reachability)
_reachability_spec.loader.exec_module(_reachability)
parse_yaml = _reachability.parse_yaml

# The compiled-Hew graph: `linux-nextest-archive` builds, `compiled-hew-linux`
# packages the certified bundle, and the shard matrix and the aggregate consume
# it. Every assertion below compares one side's published interface against the
# other side's requested one, so a rename or a miswire on either side fails.
ARCHIVE_JOB = "linux-nextest-archive"
PRODUCER_JOB = "compiled-hew-linux"
SHARD_JOB = "compiled-hew-shards"
AGGREGATE_JOB = "compiled-hew-aggregate"
UPLOAD = "actions/upload-artifact@"
DOWNLOAD = "actions/download-artifact@"
CODE_PATH_GATE = "${{ needs.changes.outputs.selected_compile }}"
MATRIX_SHARD = r"\$\{\{\s*matrix\.shard\s*\}\}"


def _jobs(text: str) -> dict[str, dict]:
    document = parse_yaml(text, "ci.yml")
    assert isinstance(document, dict), "workflow is not a mapping"
    found = document.get("jobs") or {}
    assert isinstance(found, dict), "jobs: is not a mapping"
    return {name: body for name, body in found.items() if isinstance(body, dict)}


def _needs(job: dict) -> list[str]:
    found = job.get("needs")
    found = [found] if isinstance(found, str) else (found or [])
    return [item for item in found if isinstance(item, str)]


def _steps(job: dict) -> list[dict]:
    return [step for step in (job.get("steps") or []) if isinstance(step, dict)]


def _runs(job: dict) -> list[str]:
    return [step["run"] for step in _steps(job) if isinstance(step.get("run"), str)]


def _inputs(job: dict, action: str) -> list[dict]:
    """The `with:` mapping of every step in `job` that uses `action`."""
    return [
        step["with"]
        for step in _steps(job)
        if str(step.get("uses", "")).startswith(action)
        and isinstance(step.get("with"), dict)
    ]


def _artifacts(jobs: dict[str, dict], name: str, action: str) -> list[str]:
    """Every artifact name a job uploads or downloads, matrix expanded."""
    job = jobs.get(name) or {}
    matrix = ((job.get("strategy") or {}).get("matrix") or {}).get("shard") or [""]
    return [
        re.sub(MATRIX_SHARD, str(shard), str(w.get("name") or w.get("pattern") or ""))
        for w in _inputs(job, action)
        for shard in matrix
    ]


def _strings(value: object) -> list[str]:
    if isinstance(value, dict):
        value = list(value.values())
    if isinstance(value, list):
        return [found for item in value for found in _strings(item)]
    return [value] if isinstance(value, str) else []


def _only(bodies: list[str], pattern: str) -> str:
    """The one run body matching `pattern`; any other count is a miswiring."""
    found = [body for body in bodies if re.search(pattern, body)]
    assert len(found) == 1, f"{len(found)} run steps match {pattern!r}, not one"
    return found[0]


def _flag(body: str, flag: str) -> str:
    """The value of `--flag X`, `--flag "X"` or `NAME="X"`; "" when absent."""
    found = re.search(rf"{re.escape(flag)}(?:=|\s+)(?:\"([^\"]*)\"|(\S+))", body)
    if found is None:
        return ""
    return found.group(1) if found.group(1) is not None else found.group(2)


def _clean(path: str) -> str:
    return path.strip().strip('"').removeprefix("./").rstrip("/")


def _covers(published: object, produced: str) -> bool:
    """Whether an upload `path:` publishes what a step wrote to `produced`."""
    produced = _clean(produced)
    return bool(produced) and any(
        fnmatch.fnmatchcase(produced, glob)
        or fnmatch.fnmatchcase(produced, f"{glob}/*")
        for glob in map(_clean, str(published or "").splitlines())
        if glob
    )


def assert_compiled_hew_wiring(text: str) -> None:
    """Every compiled-Hew interface matches the interface on its other side."""
    jobs = _jobs(text)
    for name in (ARCHIVE_JOB, PRODUCER_JOB, SHARD_JOB, AGGREGATE_JOB):
        assert name in jobs, f"no {name!r} job"
    producer = jobs[PRODUCER_JOB]
    shards = jobs[SHARD_JOB]
    aggregate = jobs[AGGREGATE_JOB]

    # Every artifact these jobs download -- the archive the producer packages,
    # the bundle both consumers unpack, the shard reports the aggregate
    # collects -- is uploaded under that same name by a job they declare in
    # `needs:`. A rename on either side, or a dropped edge, leaves it unmatched.
    for name in (PRODUCER_JOB, SHARD_JOB, AGGREGATE_JOB):
        supply = [
            artifact
            for upstream in _needs(jobs[name])
            for artifact in _artifacts(jobs, upstream, UPLOAD)
        ]
        wanted = _artifacts(jobs, name, DOWNLOAD)
        assert wanted, f"{name} downloads nothing"
        for want in wanted:
            assert [have for have in supply if fnmatch.fnmatchcase(have, want)], (
                f"{name} downloads {want!r}; its needs publish {supply}"
            )

    # The producer uploads the file it packed, under one name: the bundle.
    # Nothing else packs -- a consumer that can rebuild makes the certified
    # bundle decorative.
    packers = [
        name
        for name, job in jobs.items()
        if any("compiled-hew-artifact.py pack" in body for body in _runs(job))
    ]
    assert packers == [PRODUCER_JOB], f"jobs that pack a bundle: {packers}"
    pack = _only(_runs(producer), r"compiled-hew-artifact\.py pack")
    packed = _flag(pack, "--output")
    bundles = [w for w in _inputs(producer, UPLOAD) if _covers(w.get("path"), packed)]
    assert len(bundles) == 1, f"no single upload publishes the packed {packed!r}"
    bundle = str(bundles[0].get("name"))

    # This job's matrix -- not another job's -- covers the whole partition
    # space, and the denominator the shard runs with is that matrix's size.
    matrix = ((shards.get("strategy") or {}).get("matrix") or {}).get("shard")
    partitions = [str(value) for value in matrix or []]
    assert partitions == [str(n) for n in range(1, SHARDS + 1)], (
        f"{SHARD_JOB} runs partitions {partitions}, not 1..{SHARDS}"
    )
    shard_run = _only(_runs(shards), r"compiled-hew-shards\.py run")
    partition = _flag(shard_run, "--partition")
    assert "matrix.shard" in partition, f"{partition!r} ignores the shard matrix"
    assert partition.rsplit("/", 1)[-1] == str(SHARDS), (
        f"{partition!r} partitions against {SHARDS} matrix shards"
    )

    # The shard uploads the reports it wrote, and each consumer lands the
    # bundle where its own unpack reads it and runs out of what that wrote.
    reports = [w for w in _inputs(shards, UPLOAD) if w.get("name") != bundle]
    assert len(reports) == 1, f"{SHARD_JOB} uploads {len(reports)} report artifacts"
    assert _covers(reports[0].get("path"), _flag(shard_run, "--output-dir")), (
        f"{SHARD_JOB} writes its reports outside the path it uploads"
    )
    unpacked = {}
    for name in (SHARD_JOB, AGGREGATE_JOB):
        bodies = _runs(jobs[name])
        landings = [w for w in _inputs(jobs[name], DOWNLOAD) if w.get("name") == bundle]
        assert len(landings) == 1, f"{name} does not download {bundle!r}"
        landing = str(landings[0].get("path", "")).rstrip("/")
        unpack = _only(bodies, r"compiled-hew-artifact\.py unpack")
        assert _flag(unpack, "--input").startswith(f"{landing}/"), (
            f"{name} lands the bundle in {landing!r} and unpacks something else"
        )
        unpacked[name] = _flag(unpack, "--destination")
        assert any(unpacked[name] in body for body in bodies if body is not unpack), (
            f"{name} runs nothing out of {unpacked[name]!r}"
        )

    # The aggregate's `needs:` covers every job it reads a result or output of.
    read = {
        found
        for value in _strings(aggregate)
        for found in re.findall(r"needs\.([\w-]+)\.(?:result|outputs)", value)
    }
    assert SHARD_JOB in _needs(aggregate), f"{AGGREGATE_JOB} skips {SHARD_JOB}"
    assert read <= set(_needs(aggregate)), (
        f"{AGGREGATE_JOB} reads {sorted(read - set(_needs(aggregate)))} without "
        "depending on it"
    )

    # It reports, lists its own inventory with the certified compiler, and runs
    # both ratchet gates over the reports it actually downloaded.
    bodies = _runs(aggregate)
    patterns = [w for w in _inputs(aggregate, DOWNLOAD) if w.get("pattern")]
    assert len(patterns) == 1, f"{AGGREGATE_JOB} has {len(patterns)} pattern downloads"
    collected = str(patterns[0].get("path", "")).rstrip("/")
    report = _only(bodies, r"compiled-hew-shards\.py report")
    assert _flag(report, "--reports-dir").rstrip("/") == collected, (
        f"the failure reporter reads outside {collected!r}"
    )
    for target in ("test-hew-ratchet", "test-o2-differential"):
        gate = _only(bodies, rf"make\s+{re.escape(target)}(?:\s|$)")
        inventory = _flag(gate, "HEW_FULL_INVENTORY")
        assert _flag(gate, "HEW_SHARD_REPORT_DIR").rstrip("/") == collected and _flag(
            gate, "HEW_SHARD_COUNT"
        ) == str(SHARDS), (
            f"make {target} does not gate {collected!r} over {SHARDS} shards"
        )
        writer = _only(bodies, rf">\s*\"{re.escape(inventory)}\"")
        assert unpacked[AGGREGATE_JOB] in writer, (
            f"{inventory!r} is not listed by the certified compiler"
        )


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

    def test_the_compiled_hew_jobs_agree_on_every_interface(self) -> None:
        assert_compiled_hew_wiring(self.workflow)

    def test_all_three_compiled_hew_jobs_ride_the_same_code_path_gate(self) -> None:
        for name in (PRODUCER_JOB, SHARD_JOB, AGGREGATE_JOB):
            job = _jobs(self.workflow)[name]
            env = job.get("env") or {}
            self.assertEqual(env.get("RUN_CODE_PATH"), CODE_PATH_GATE)

    def test_the_wiring_contract_rejects_a_mismatched_interface(self) -> None:
        """One mutation per family of comparison the contract exists to make:
        an artifact name, a produced-against-published path, and this job's
        own shard matrix.
        """
        mutations = (
            (PRODUCER_JOB, "name: compiled-hew-linux-$", "name: renamed-$"),
            (PRODUCER_JOB, "--output compiled-hew-linux", "--output other-bundle"),
            (SHARD_JOB, "shard: [1, 2, 3, 4]", "shard: [1, 2, 3]"),
        )
        for job, old, new in mutations:
            with self.subTest(f"{job}: {old}"):
                head, marker, tail = self.workflow.partition(f"  {job}:\n")
                mutated = head + marker + tail.replace(old, new, 1)
                self.assertNotEqual(mutated, self.workflow, "mutation matched nothing")
                with self.assertRaises(AssertionError):
                    assert_compiled_hew_wiring(mutated)

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
        self.assertIn(
            "needs: [changes, build-and-test, compiled-hew-aggregate]", required
        )
        self.assertIn('test "$RUST_GATES_RESULT" = success', required)
        self.assertIn('test "$COMPILED_HEW_RESULT" = success', required)


if __name__ == "__main__":
    unittest.main()
