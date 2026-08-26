#!/usr/bin/env python3
"""Counterfactual tests for compiled-Hew shard aggregation."""

from __future__ import annotations

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

# The certified producer: packages the bundle the archive `linux-nextest-
# archive` already built. It compiles nothing itself.
PRODUCER_JOB = "compiled-hew-linux"
BUILD_AUTHORITY_JOB = "linux-nextest-archive"
CONSUMER_JOBS = ("compiled-hew-shards", "compiled-hew-aggregate")
ARTIFACT_NAME = "compiled-hew-linux-${{ github.sha }}"

# A consumer running any of these would mean the one-producer contract is
# decorative: the job could still build its own bundle instead of consuming
# the certified one.
_REBUILD_FRAGMENTS = (
    "compiled-hew-artifact.py pack",
    "cargo nextest archive",
    "make hew-native",
    "make hew-profile-check",
)


def _workflow_jobs(text: str) -> dict[str, dict]:
    document = parse_yaml(text, "ci.yml")
    assert isinstance(document, dict), "workflow is not a mapping"
    found = document.get("jobs") or {}
    assert isinstance(found, dict), "jobs: is not a mapping"
    return {name: body for name, body in found.items() if isinstance(body, dict)}


def _job_steps(job: dict) -> list[dict]:
    found = job.get("steps") or []
    return [step for step in found if isinstance(step, dict)]


def _job_needs(job: dict) -> list[str]:
    found = job.get("needs")
    if found is None:
        return []
    if isinstance(found, str):
        return [found]
    assert isinstance(found, list), f"needs: is neither a scalar nor a list: {found!r}"
    return [item for item in found if isinstance(item, str)]


def _run_bodies(job: dict) -> list[str]:
    return [step["run"] for step in _job_steps(job) if isinstance(step.get("run"), str)]


def _artifact_names(job: dict, action_prefix: str) -> list[str]:
    names = []
    for step in _job_steps(job):
        uses = step.get("uses")
        if isinstance(uses, str) and uses.startswith(action_prefix):
            with_ = step.get("with") or {}
            name = with_.get("name") if isinstance(with_, dict) else None
            if isinstance(name, str):
                names.append(name)
    return names


def assert_one_certified_producer_feeds_every_consumer(text: str) -> None:
    """Exactly one certified compiled-Hew artifact producer/build authority.

    `compiled-hew-linux` packages the bundle from the archive
    `linux-nextest-archive` already built; it compiles nothing itself. The
    four-way shard matrix and the aggregate are the consumers: both depend
    on and download that one certified bundle rather than building or
    repackaging their own.
    """
    jobs = _workflow_jobs(text)

    producer = jobs.get(PRODUCER_JOB)
    assert producer is not None, f"no {PRODUCER_JOB!r} job"
    assert BUILD_AUTHORITY_JOB in _job_needs(producer), (
        f"{PRODUCER_JOB} does not depend on {BUILD_AUTHORITY_JOB!r}; it would "
        "have no certified build to package and would have to compile its own"
    )

    # Exactly one step, in exactly one job, packages the certified bundle.
    packers = [
        name
        for name, job in jobs.items()
        for body in _run_bodies(job)
        if "compiled-hew-artifact.py pack" in body
    ]
    assert packers == [PRODUCER_JOB], (
        "expected exactly one packaging step, owned by "
        f"{PRODUCER_JOB!r}, found: {packers}"
    )

    # Exactly one job uploads the certified bundle under its one name.
    uploaders = [
        name
        for name, job in jobs.items()
        if ARTIFACT_NAME in _artifact_names(job, "actions/upload-artifact@")
    ]
    assert uploaders == [PRODUCER_JOB], (
        f"expected exactly one uploader of {ARTIFACT_NAME!r}, found: {uploaders}"
    )

    for name in CONSUMER_JOBS:
        job = jobs.get(name)
        assert job is not None, f"no {name!r} consumer job"
        assert PRODUCER_JOB in _job_needs(job), (
            f"{name} does not depend on {PRODUCER_JOB!r}; a missing bundle "
            "would look like an ordinary cold start rather than a failed gate"
        )
        assert ARTIFACT_NAME in _artifact_names(job, "actions/download-artifact@"), (
            f"{name} never downloads {ARTIFACT_NAME!r}"
        )
        assert any(
            "compiled-hew-artifact.py unpack" in body for body in _run_bodies(job)
        ), f"{name} downloads the bundle but never unpacks/verifies it"
        for body in _run_bodies(job):
            for fragment in _REBUILD_FRAGMENTS:
                assert fragment not in body, (
                    f"{name} runs {fragment!r}; a consumer that can still "
                    "build defeats the one-producer contract silently"
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

    def test_one_certified_producer_feeds_every_consumer(self) -> None:
        assert_one_certified_producer_feeds_every_consumer(self.workflow)

        # The four-way shard matrix is exhaustive and disjoint: one hash
        # partition per shard index, covering the whole `/4` space.
        self.assertIn("shard: [1, 2, 3, 4]", self.workflow)
        self.assertIn('--partition "hash:${{ matrix.shard }}/4"', self.workflow)

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

    def test_the_producer_consumer_contract_rejects_removal_or_miswiring(
        self,
    ) -> None:
        text = self.workflow

        def rejects(mutated: str) -> None:
            self.assertNotEqual(
                mutated, text, "the mutation matched nothing; the test is vacuous"
            )
            try:
                assert_one_certified_producer_feeds_every_consumer(mutated)
            except AssertionError:
                return
            raise AssertionError("a broken producer/consumer contract was accepted")

        # Deleting the pack step leaves no producer at all.
        rejects(
            text.replace(
                "          python3 scripts/compiled-hew-artifact.py pack \\\n"
                '            --source-debug-dir "$root/target/debug" \\\n'
                '            --source-revision "$GITHUB_SHA" \\\n'
                "            --output compiled-hew-linux.tar.gz\n",
                "",
            )
        )

        # Renaming the producer job strands both consumers' `needs:` and the
        # bundle they expect to download.
        rejects(
            text.replace(
                "  compiled-hew-linux:\n", "  compiled-hew-linux-renamed:\n", 1
            )
        )

        # Dropping the producer's own dependency on the build authority would
        # let it compile a second build that happened to agree.
        rejects(
            text.replace(
                "    needs: [changes, linux-nextest-archive]\n",
                "    needs: changes\n",
                1,
            )
        )

        # Un-gating a shard consumer from the producer's `needs:` turns a
        # missing bundle into an ordinary cold start instead of a failed gate.
        rejects(
            text.replace(
                "    needs: [changes, compiled-hew-linux]\n"
                "    runs-on: ubuntu-24.04\n"
                "    timeout-minutes: 60\n",
                "    needs: [changes]\n"
                "    runs-on: ubuntu-24.04\n"
                "    timeout-minutes: 60\n",
                1,
            )
        )

        # Miswiring the shard matrix's download to a different artifact name
        # hides a bundle that no longer matches what the producer certified.
        rejects(
            text.replace(
                "      - name: Download compiled Hew bundle\n"
                "        if: env.RUN_CODE_PATH == 'true'\n"
                "        uses: actions/download-artifact@3e5f45b2cfb9172054b4087a40e8e0b5a5461e7c  # v8.0.1\n"
                "        with:\n"
                "          name: compiled-hew-linux-${{ github.sha }}\n",
                "      - name: Download compiled Hew bundle\n"
                "        if: env.RUN_CODE_PATH == 'true'\n"
                "        uses: actions/download-artifact@3e5f45b2cfb9172054b4087a40e8e0b5a5461e7c  # v8.0.1\n"
                "        with:\n"
                "          name: compiled-hew-linux-old-${{ github.sha }}\n",
                1,
            )
        )

        # A consumer that still packages its own bundle defeats the contract
        # even while every wire and download stays intact.
        rejects(
            text.replace(
                "          python3 scripts/compiled-hew-shards.py run\n",
                "          python3 scripts/compiled-hew-artifact.py pack "
                '--source-debug-dir target/debug --source-revision "$GITHUB_SHA" '
                "--output compiled-hew-linux.tar.gz\n"
                "          python3 scripts/compiled-hew-shards.py run\n",
                1,
            )
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
        self.assertIn(
            "needs: [changes, build-and-test, compiled-hew-aggregate]", required
        )
        self.assertIn('test "$RUST_GATES_RESULT" = success', required)
        self.assertIn('test "$COMPILED_HEW_RESULT" = success', required)


if __name__ == "__main__":
    unittest.main()
