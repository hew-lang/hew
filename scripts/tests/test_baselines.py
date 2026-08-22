#!/usr/bin/env python3
"""Self-test for the derived-baselines registry and its regen harness.

The registry is only load-bearing if it stays true: a path that no longer
exists, a gate that is no longer a Makefile target, or a ratchet prune that
would launder a regression are each a silent hole in the one place the tree
trusts to answer "is this baseline current".
"""

from __future__ import annotations

import argparse
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


def makefile_recipes() -> dict[str, str]:
    """Target -> recipe text, parsed the way the reachability checker parses it."""
    reachability = load_reachability_module()
    _phony, _prereqs, recipes = reachability.parse_makefile(
        (ROOT / "Makefile").read_text()
    )
    return recipes


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
            if any(ch in path for ch in "*?["):
                exists = any(
                    baselines._matches(tracked, path)
                    for tracked in baselines.tracked_files()
                )
            else:
                exists = (ROOT / path).exists()
            check(
                f"{member.id}: {path} exists",
                exists,
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
    recipes = makefile_recipes()
    unregistered, stale = reachability.baseline_membership_findings(recipes)
    check(
        "the current tree has no unregistered comparison target",
        unregistered == [],
        str(unregistered),
    )
    check("no exemption has expired", stale == [], str(stale))

    discovered = reachability.comparison_targets(recipes)
    # The .expected corpora are the case that broke the first, name-matching
    # version of this assertion: nothing about `test-ux-examples` looks like a
    # baseline gate, and it compares every tutorial against a committed
    # transcript. Discovery must find them from what the recipe RUNS.
    for target in ("test-ux-examples", "test-surface-examples"):
        check(
            f"{target} is discovered structurally",
            target in discovered,
            f"discovered: {sorted(discovered)}",
        )

    # The counterfactual: an assertion that always returns [] would pass the
    # lines above forever.
    invented = dict(recipes)
    invented["frobnicate"] = "\tpython3 scripts/example-expectations.py --label x\n"
    new_unregistered, _ = reachability.baseline_membership_findings(invented)
    check(
        "an unregistered comparison target is rejected",
        [target for target, _ in new_unregistered] == ["frobnicate"],
        str(new_unregistered),
    )
    # And the other direction: an exemption whose target stopped comparing is
    # dead text, so it is reported rather than left to accumulate.
    _, orphaned = reachability.baseline_membership_findings(
        {k: v for k, v in recipes.items() if k != "test-vertical-slice"}
    )
    check(
        "an exemption for a target that no longer compares is reported",
        "test-vertical-slice" in orphaned,
        str(orphaned),
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


def test_deferred_makefile_prerequisites_follow_variable_reference() -> None:
    print("deferred Makefile prerequisites follow their variable reference")
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        makefile = root / "Makefile"
        original_root = baselines.ROOT
        baselines.ROOT = root
        try:
            # The append follows the rule, as it does for most LINT_GATES
            # members in the root Makefile.
            makefile.write_text(
                "lint: $$(LINT_GATES)\nLINT_GATES += lint-member\nlint-member:\n"
            )
            reached = baselines.lane_targets(["make lint"])

            # Counterfactual: only the variable actually named by the
            # prerequisite can contribute its accumulated members.
            makefile.write_text(
                "lint: $$(OTHER_GATES)\nLINT_GATES += lint-member\nlint-member:\n"
            )
            counterfactual = baselines.lane_targets(["make lint"])
        finally:
            baselines.ROOT = original_root

    check(
        "a deferred variable reaches members appended after its rule",
        "lint-member" in reached,
        str(sorted(reached)),
    )
    check(
        "an unreferenced accumulated variable does not add prerequisites",
        "lint-member" not in counterfactual,
        str(sorted(counterfactual)),
    )


def test_snapshot_restores_kind_and_mode() -> None:
    """A check must leave the tree byte-, mode-, and type-identical."""
    print("snapshot/restore preserves mode, symlinks and empty directories")
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        corpus = root / "corpus"
        (corpus / "empty").mkdir(parents=True)
        script = corpus / "run.sh"
        script.write_text("#!/bin/sh\n")
        script.chmod(0o755)
        plain = corpus / "data.txt"
        plain.write_text("one\n")
        plain.chmod(0o644)
        link = corpus / "alias.txt"
        link.symlink_to("data.txt")

        original_root = baselines.ROOT
        baselines.ROOT = root
        try:
            before = baselines.snapshot(("corpus",))
            # Simulate a regen that rewrites content, drops the empty directory,
            # flattens the symlink and resets the executable bit.
            link.unlink()
            link.write_text("one\n")
            (corpus / "empty").rmdir()
            plain.write_text("two\n")
            script.chmod(0o644)
            after = baselines.snapshot(("corpus",))
            drift = baselines.describe_drift(before, after)
            baselines.restore(before, ("corpus",))
            restored = baselines.snapshot(("corpus",))
        finally:
            baselines.ROOT = original_root

        check("drift is reported", len(drift) >= 3, str(drift))
        check(
            "mode drift is named as mode drift",
            any("mode" in item for item in drift),
            str(drift),
        )
        check(
            "restore is exact",
            restored == before,
            str(baselines.describe_drift(before, restored)),
        )
        check(
            "the executable bit comes back",
            script.stat().st_mode & 0o111 != 0,
            oct(script.stat().st_mode),
        )
        check("the symlink comes back a symlink", link.is_symlink(), str(link))
        check(
            "the empty directory comes back", (corpus / "empty").is_dir(), str(corpus)
        )


def test_blanket_regen_skips_user_facing_contracts() -> None:
    """`make baselines` must never silently re-record an example's output."""
    print("a blanket regen defers explicit-only members")
    explicit = [m for m in baselines.REGISTRY if m.explicit_only]
    contracts = {
        "ux-example-expectations",
        "surface-example-expectations",
        "core-matrix-truth-table",
        "funcupdate-mir-baselines",
        "release-count-baseline",
    }
    check(
        "every artefact that records observed behaviour is explicit-only",
        contracts <= {m.id for m in explicit},
        str(sorted(contracts - {m.id for m in explicit})),
    )
    for member in explicit:
        check(
            f"{member.id}: regen command names the member",
            f"--only {member.id}" in member.regen_command(),
            member.regen_command(),
        )

    # Not just the advertised command -- the actual sweep. Drive cmd_regen with
    # no --only and record every command it would run: an explicit-only
    # member's regen must never appear, and an ordinary member's must.
    invoked: list[str] = []

    def record(command: str, *, capture: bool) -> tuple[int, str]:
        invoked.append(command)
        return 0, ""

    original_run = baselines.run
    baselines.run = record
    try:
        status = baselines.cmd_regen(
            argparse.Namespace(tier="compiler", only=[], relevant_to=[])
        )
    finally:
        baselines.run = original_run

    check("the sweep reports success", status == 0, str(status))
    for member in explicit:
        assert member.regen is not None
        check(
            f"{member.id}: the sweep did not run {member.regen}",
            member.regen not in invoked,
            str(invoked),
        )
    ordinary = [
        m
        for m in baselines.REGISTRY
        if m.tier == "compiler" and not m.explicit_only and m.regen
    ]
    check("the sweep has ordinary members to run", ordinary != [], "")
    for member in ordinary:
        check(
            f"{member.id}: the sweep ran {member.regen}",
            member.regen in invoked,
            str(invoked),
        )


def test_file_closure_is_the_authority() -> None:
    """The reviewer's case: a shaped tracked file nobody regenerates fails A6."""
    print("the file closure covers every baseline-shaped tracked file")
    tracked = baselines.tracked_files()
    findings, stale = baselines.coverage(tracked)
    check(
        "no baseline-shaped file is uncovered or double-owned",
        findings == [],
        str(findings[:5]),
    )
    check("no no-baseline entry matches nothing", stale == [], str(stale))

    shaped = [p for p in tracked if p and baselines.is_baseline_shaped(p)]
    check("the closure actually looks at files", len(shaped) > 500, str(len(shaped)))

    # An unregistered file matching a shape must fail, or the closure proves
    # nothing. `docs/` is outside every member path and every no-baseline entry.
    invented = "docs/synthetic-corpus/golden/case.mir"
    new_findings, _ = baselines.coverage(tracked + [invented])
    check(
        "an unregistered baseline-shaped file is reported",
        [path for path, _ in new_findings] == [invented],
        str(new_findings),
    )

    # And a no-baseline entry whose files all disappeared is dead text. Drop
    # every file the vertical-slice entry excuses and it must be reported.
    thinned = [
        path
        for path in tracked
        if not baselines._matches(path, "tests/vertical-slice/*")
    ]
    _, expired = baselines.coverage(thinned)
    check(
        "a no-baseline entry that excuses nothing is reported as expired",
        "tests/vertical-slice/*" in expired,
        str(expired),
    )
    check(
        "and it is reported as unowned, not as a conflict",
        len(new_findings) == 1 and new_findings[0][1] == [],
        str(new_findings),
    )

    # Two members claiming one file is the other failure: it means neither
    # regen is authoritative over those bytes.
    widened = baselines.REGISTRY + (
        baselines.Baseline(
            id="probe-overlap",
            summary="probe",
            tier="fast",
            paths=("tests/ll-oracle",),
            gates=(),
            regen="make ll-golden",
        ),
    )
    original = baselines.REGISTRY
    baselines.REGISTRY = widened
    try:
        overlap, _ = baselines.coverage(tracked)
    finally:
        baselines.REGISTRY = original
    check(
        "a file claimed by two members is reported",
        any(len(owners) > 1 for _, owners in overlap),
        str(overlap[:3]),
    )


def test_file_closure_survives_indirection() -> None:
    """The closure must not depend on how a script reaches its baseline."""
    print("the closure is over files, not over readers")
    # `tests/mir-baselines` and the release-count table are reached through
    # variables in Rust test code -- no Makefile recipe or script names them as
    # a literal. Both are registered, which is the property the reader analysis
    # could not deliver.
    ids = {m.id for m in baselines.REGISTRY}
    for member_id in (
        "funcupdate-mir-baselines",
        "release-count-baseline",
        "core-matrix-truth-table",
    ):
        check(f"{member_id} is registered", member_id in ids, str(sorted(ids)))


def main() -> int:
    test_registry_is_well_formed()
    test_ratchet_report_parsing()
    test_prune_removes_only_now_passing_entries()
    test_regen_refuses_to_record_a_new_failure()
    test_lane_relevance_follows_prerequisites()
    test_deferred_makefile_prerequisites_follow_variable_reference()
    test_file_closure_is_the_authority()
    test_file_closure_survives_indirection()
    test_snapshot_restores_kind_and_mode()
    test_blanket_regen_skips_user_facing_contracts()
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
