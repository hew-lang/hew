#!/usr/bin/env python3
"""Self-test for the derived-baselines registry and its regen harness.

The registry is only load-bearing if it stays true: a path that no longer
exists, a gate that is no longer a Makefile target, or a ratchet prune that
would launder a regression are each a silent hole in the one place the tree
trusts to answer "is this baseline current".
"""

from __future__ import annotations

import importlib.util
import re
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts"))

import baselines  # noqa: E402


FAILURES: list[str] = []


def check(name: str, condition: bool, detail: str = "") -> None:
    if condition:
        print(f"  ok   {name}")
        return
    FAILURES.append(f"{name}: {detail}")
    print(f"  FAIL {name}: {detail}")


def makefile_targets() -> set[str]:
    targets: set[str] = set()
    for line in (ROOT / "Makefile").read_text().splitlines():
        if line.startswith("\t") or line.startswith("#"):
            continue
        match = re.match(r"^([A-Za-z0-9_.\-/]+)\s*:(?!=)", line)
        if match and not match.group(1).startswith("."):
            targets.add(match.group(1))
    phony = set()
    for line in (ROOT / "Makefile").read_text().splitlines():
        if line.startswith(".PHONY:"):
            phony.update(line.removeprefix(".PHONY:").split())
    return targets | phony


def load_reachability_module():
    """Import the reachability checker by path; its filename is not an identifier."""
    name = "check_gate_reachability"
    spec = importlib.util.spec_from_file_location(
        name, ROOT / "scripts" / "check-gate-reachability.py"
    )
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def test_registry_is_well_formed() -> None:
    print("registry is well formed")
    ids = [member.id for member in baselines.REGISTRY]
    check("ids are unique", len(ids) == len(set(ids)), str(ids))
    targets = makefile_targets()
    for member in baselines.REGISTRY:
        check(
            f"{member.id}: tier is known",
            member.tier in baselines.TIERS,
            member.tier,
        )
        for path in member.paths:
            check(
                f"{member.id}: {path} exists",
                (ROOT / path).exists(),
                "registry names an artefact that is not in the tree",
            )
        for gate in member.gates:
            check(
                f"{member.id}: gate {gate} is a Makefile target",
                gate in targets,
                "registry names a gate the Makefile does not define",
            )
        check(
            f"{member.id}: has a regen path",
            member.regen is not None or member.prune_list is not None,
            "neither a regen command nor a ratchet prune list",
        )
        if member.prune_list is not None:
            check(
                f"{member.id}: prune list has a check command",
                member.check is not None,
                "a ratchet prune needs the gate whose report it reads",
            )


def test_every_gate_shaped_target_is_accounted_for() -> None:
    """The closure property: a new baseline cannot enter outside the registry."""
    print("every gate-shaped Makefile target is registered or exempt")
    reachability = load_reachability_module()
    phony = makefile_targets()
    check(
        "the current tree has no unregistered comparison target",
        reachability.baseline_membership_findings(phony) == [],
        str(reachability.baseline_membership_findings(phony)),
    )
    # The counterfactual: without it, an assertion that always returns [] would
    # pass the line above forever.
    invented = reachability.baseline_membership_findings(phony | {"frobnicate-check"})
    check(
        "a new comparison target is rejected until it is registered",
        [target for target, _ in invented] == ["frobnicate-check"],
        str(invented),
    )


def test_ratchet_report_parsing() -> None:
    print("ratchet verdict vocabulary parses")
    report = """
CORPUS FAIL: 1 listed failure(s) now PASS — remove from list:
  NOW-PASSES: examples/machine/bounded_resource.hew
CORPUS FAIL: 1 UNEXPECTED failure(s):
  UNEXPECTED: examples/net/broken.hew
    error: something
"""
    now_passes, unexpected = baselines.parse_ratchet_report(report)
    check(
        "NOW-PASSES entries are extracted",
        now_passes == ["examples/machine/bounded_resource.hew"],
        str(now_passes),
    )
    check(
        "UNEXPECTED entries are extracted",
        unexpected == ["examples/net/broken.hew"],
        str(unexpected),
    )
    indented = baselines.parse_ratchet_report("    error: not a verdict line\n")
    check("prose is not a verdict", indented == ([], []), str(indented))


def test_prune_removes_only_now_passing_entries() -> None:
    print("prune removes now-passing entries and keeps the total honest")
    with tempfile.TemporaryDirectory() as tmp:
        listing = Path(tmp) / "expected-failures.txt"
        listing.write_text(
            "# Total: 3 known-failing entries (3 trap fixtures)\n"
            "#\n"
            "alpha.hew  # reason a\n"
            "beta.hew   # reason b\n"
            "gamma.hew  # reason c\n"
        )
        removed = baselines.prune_entries(listing, ["beta.hew"])
        text = listing.read_text()
        check("the entry is removed", removed == ["beta.hew"], str(removed))
        check("siblings survive", "alpha.hew" in text and "gamma.hew" in text, text)
        check(
            "the Total comment is re-counted",
            "# Total: 2 known-failing entries (2 trap fixtures)" in text,
            text,
        )


def test_regen_refuses_to_record_a_new_failure() -> None:
    """A regen that could bless a regression would be worse than no regen."""
    print("a ratchet regen refuses to record a new failure")
    with tempfile.TemporaryDirectory() as tmp:
        listing = Path(tmp) / "list.txt"
        listing.write_text("alpha.hew\n")
        member = baselines.Baseline(
            id="probe",
            summary="probe",
            tier="fast",
            paths=(),
            gates=(),
            check="printf '  UNEXPECTED: delta.hew\\n'; exit 1",
            prune_list="list.txt",
        )
        # Registry paths resolve against the repo root, so the probe runs with
        # the temp directory standing in as the tree.
        original_root = baselines.ROOT
        baselines.ROOT = Path(tmp)
        try:
            status = baselines.regen_ratchet(member)
        finally:
            baselines.ROOT = original_root
        check("regen exits non-zero", status == 1, str(status))
        check(
            "the list is untouched",
            listing.read_text() == "alpha.hew\n",
            listing.read_text(),
        )


def test_lane_relevance_follows_prerequisites() -> None:
    print("lane relevance follows Makefile prerequisites")
    reached = baselines.lane_targets(["make lint"])
    check(
        "make lint reaches wasm-capability-check",
        "wasm-capability-check" in reached,
        "prerequisite closure did not reach a transitively-run gate",
    )
    selected = {m.id for m in baselines.select("fast", [], ["make playground-check"])}
    check(
        "playground-check selects the manifest baseline",
        "playground-manifest" in selected,
        str(selected),
    )
    unrelated = baselines.select("fast", [], ["cargo fmt --all -- --check"])
    check("an unrelated lane selects nothing", unrelated == [], str(unrelated))


def main() -> int:
    test_registry_is_well_formed()
    test_ratchet_report_parsing()
    test_prune_removes_only_now_passing_entries()
    test_regen_refuses_to_record_a_new_failure()
    test_lane_relevance_follows_prerequisites()
    test_every_gate_shaped_target_is_accounted_for()
    if FAILURES:
        print(f"\nbaselines registry self-test: {len(FAILURES)} failure(s)")
        for failure in FAILURES:
            print(f"  - {failure}")
        return 1
    print("\nbaselines registry self-test: PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
