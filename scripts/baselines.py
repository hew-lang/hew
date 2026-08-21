#!/usr/bin/env python3
"""The single registry of committed derived baselines, and the one way to regenerate them.

A *baseline* is a tracked artefact whose content is DERIVED from other tracked
sources: a golden dump, a generated consumer, a manifest, or a ratcheted
expected-failure list.  Every baseline drifts the moment its source moves, and
because the gate that compares it usually sits at the far end of a 90-minute CI
job, the drift is discovered hours after the commit that caused it — on someone
else's pull request.

This module makes the regeneration path total and discoverable:

  make baselines          regenerate every member
  make baselines-check    prove every member is current, and print the exact
                          regen command for each one that is not

and it makes the set closed: ``scripts/check-gate-reachability.py`` asserts that
every ``*-check`` / ratchet gate in the Makefile is either a registered member's
gate or an explicitly reasoned exemption, so an eleventh baseline cannot be
added outside this file.

Two kinds of member
-------------------
*Generated artefacts* carry a real ``regen`` command and, usually, a native
freshness ``check``.  When a member has no native check, the harness snapshots
its paths, regenerates, compares, and restores — the artefact is never left
mutated by a check.

*Ratcheted expected-failure lists* have no generator: their content is the set
of things that currently fail.  Their regen is defined by the ratchet contract
every one of them already prints:

    NOW-PASSES: <entry>     the entry no longer fails -> remove it
    UNEXPECTED: <entry>     a new failure             -> a hard error

Regenerating such a list REMOVES now-passing entries and REFUSES to add a new
failure.  A regression is never blessed away by running a regen command; it is
fixed.

Tiers
-----
``fast``     needs no ``hew`` compiler build.  The preflight dispatcher runs the
             relevant fast members BEFORE its warm-up, so drift is a
             sub-minute failure instead of an hour-deep one.
``compiler`` needs ``target/debug/hew``; its gate already builds the compiler,
             so checking it early buys nothing and costs a full build.
"""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

TIERS = ("fast", "compiler")


@dataclass(frozen=True)
class Baseline:
    """One derived artefact, its regen path, and the gates that compare against it."""

    id: str
    summary: str
    tier: str
    paths: tuple[str, ...]
    gates: tuple[str, ...]
    regen: str | None = None
    check: str | None = None
    prune_list: str | None = None
    requires: tuple[str, ...] = field(default_factory=tuple)

    def regen_command(self) -> str:
        """The exact command a human runs to bring this baseline current."""
        if self.regen is not None:
            return self.regen
        return f"python3 scripts/baselines.py regen --only {self.id}"


# ── Registry ───────────────────────────────────────────────────────────────────
#
# Adding a baseline means adding a row here.  A `*-check` or ratchet gate whose
# artefact is absent from this registry fails check-gate-reachability (A6).
#
# Every command below is a Makefile target, not a driver script: the Makefile
# already owns the binary paths, the build prerequisites and the ordering, and
# routing through it keeps this registry stable while the driver scripts
# underneath are consolidated.  The Makefile's regen targets are the seam this
# registry drives; `make baselines` is the only entry point a contributor needs.

REGISTRY: tuple[Baseline, ...] = (
    Baseline(
        id="wasm-capability",
        summary="generated WASM capability consumers (checker, playground, matrix)",
        tier="fast",
        paths=(
            "hew-types/src/wasm_capabilities_generated.rs",
            "examples/playground/wasm-capabilities.json",
            "docs/wasm-capability-matrix.md",
        ),
        gates=("wasm-capability-check",),
        regen="make wasm-capability",
        check="make wasm-capability-check",
    ),
    Baseline(
        id="playground-manifest",
        summary="curated playground manifest consumed by browser tooling",
        tier="fast",
        paths=("examples/playground/manifest.json",),
        gates=("playground-manifest-check",),
        # The manifest is rendered from the capability outputs; `make
        # playground-manifest` carries that ordering as a prerequisite.
        regen="make playground-manifest",
        check="make playground-manifest-check",
    ),
    Baseline(
        id="sandbox-fixtures",
        summary="hew-sandbox-vm bytecode fixtures",
        tier="fast",
        paths=("hew-sandbox-vm/fixtures",),
        gates=("sandbox-fixtures-check",),
        regen="make sandbox-fixtures",
        check="make sandbox-fixtures-check",
    ),
    Baseline(
        id="licenses",
        summary="THIRD-PARTY-LICENSES derived from the dependency tree",
        tier="fast",
        paths=("THIRD-PARTY-LICENSES",),
        gates=("licenses-check",),
        # Requires cargo-about: cargo install cargo-about --locked
        regen="make licenses",
        check="make licenses-check",
    ),
    Baseline(
        id="core-matrix-cells",
        summary="core matrix corpus (primitive x operation enumeration)",
        tier="fast",
        # The generator owns the whole directory; a stale cell left behind by a
        # deleted row is exactly the drift this member exists to catch, so the
        # regen clears the tree first rather than writing over it.
        paths=("tests/core-matrix/cells",),
        gates=("test-core-matrix",),
        regen=(
            "rm -rf tests/core-matrix/cells "
            "&& python3 scripts/core-matrix-gen.py --out tests/core-matrix/cells"
        ),
        check=None,
    ),
    Baseline(
        id="structural-authority-inventory",
        summary="exact per-form/per-path structural authority counts",
        tier="fast",
        paths=("scripts/structural-authority-inventory.tsv",),
        gates=("structural-lint",),
        regen="python3 scripts/structural-authority-audit.py --write-inventory",
        check="python3 scripts/structural-authority-audit.py",
        requires=("make structural-lint-bootstrap-install",),
    ),
    Baseline(
        id="ll-goldens",
        summary="per-function .ll byte-identity goldens",
        tier="compiler",
        paths=("tests/ll-oracle/corpus",),
        gates=("ll-diff",),
        regen="make ll-golden",
        check="make ll-diff",
    ),
    Baseline(
        id="checked-mir-goldens",
        summary="checked-MIR corpus --dump-mir goldens",
        tier="compiler",
        paths=("examples/v05/checked-mir/golden",),
        gates=("checked-mir-verify",),
        regen="make checked-mir-golden",
        check="make checked-mir-verify",
    ),
    Baseline(
        id="checked-mir-expected",
        summary="checked-MIR corpus execution transcripts",
        tier="compiler",
        paths=("examples/v05/checked-mir",),
        gates=("checked-mir-run",),
        regen="make checked-mir-expect",
        check="make checked-mir-run",
    ),
    Baseline(
        id="hew-corpus-expected-failures",
        summary="repo-wide `hew check` sweep ratchet",
        tier="compiler",
        paths=("scripts/hew-corpus-expected-failures.txt",),
        gates=("hew-check-all",),
        check="make hew-check-all",
        prune_list="scripts/hew-corpus-expected-failures.txt",
    ),
    Baseline(
        id="hew-suite-expected-failures",
        summary="compiled Hew suite ratchet",
        tier="compiler",
        paths=("scripts/hew-suite-expected-failures.txt",),
        gates=("test-hew-ratchet",),
        check="make test-hew-ratchet",
        prune_list="scripts/hew-suite-expected-failures.txt",
    ),
    Baseline(
        id="stdlib-expected-failures",
        summary="stdlib type-check ratchet",
        tier="compiler",
        paths=("scripts/stdlib-expected-failures.txt",),
        gates=("test-stdlib-ratchet",),
        check="make test-stdlib-ratchet",
        prune_list="scripts/stdlib-expected-failures.txt",
    ),
    Baseline(
        id="doc-test-expected-failures",
        summary="docs ```hew fence ratchet",
        tier="compiler",
        paths=("scripts/doc-test-expected-failures.txt",),
        gates=("test-doc-examples",),
        check="make test-doc-examples",
        prune_list="scripts/doc-test-expected-failures.txt",
    ),
    Baseline(
        id="fuzz-oracle-expected-failures",
        summary="fuzz-to-run completeness oracle ratchet",
        tier="compiler",
        paths=("tests/fuzz-oracle/expected-failures.txt",),
        gates=("fuzz-oracle",),
        check="make fuzz-oracle",
        prune_list="tests/fuzz-oracle/expected-failures.txt",
    ),
)


# NOT a member: hew-parser/fuzz/corpus. `scripts/fuzz/hydrate-corpus.sh` rebuilds
# it from the vertical-slice and example sources, but the tracked corpus also
# carries hand-added regression seeds that hydration does not produce
# (`fuzz_mir/regression-link-monitor-spawn.hew`, `fuzz_structured/seed.hew`).
# It is a curated set with a hydrator, not a derived artefact — registering it
# would turn `make baselines` into a command that deletes curated seeds.


# ── Gates that compare against something other than a committed baseline ────────
#
# check-gate-reachability walks the Makefile for gate-shaped target names and
# requires each one to be reachable from this file: either it is a registered
# member's gate, or it appears here with the reason it owns no derived artefact.
# A new `*-check` target with a committed comparison file and no registry row is
# a lint failure — that is the whole point.

EXEMPT_GATES: dict[str, str] = {
    "baselines-check": "the aggregate check over this registry; it owns no artefact of its own",
    "check-gate-reachability": "computes the command graph from the tree; compares against no committed file",
    "test-check-gate-reachability": "self-test for the reachability checker",
    "check-libhew-fresh": "compares build timestamps, not committed content",
    "check-sanitizer-gate": "validates the release sanitizer contract in the workflow",
    "codegen-trap-inventory-check": "counts in-source TRAP-DISPOSITION markers; no committed inventory file",
    "freebsd-workflow-contract-check": "asserts workflow structure against itself",
    "hew-fmt-check": "formatter idempotency over the live corpus; nothing committed to regenerate",
    "lint-ci-coverage-check": "asserts every lint sub-target is reached by CI",
    "playground-check": "runs the hew-wasm suite; its manifest freshness is the playground-manifest member",
    "playground-wasi-check": "runs curated playground examples under WASI",
    "sandbox-parity-coverage-check": "asserts nextest exclusions cover the VM-touching binaries",
    "test-sandbox-parity-coverage-check": "self-test for the parity coverage checker",
    "doc-ratchet-selftest": "self-test for the ratchet membership wiring",
    "ll-golden": "regen half of the ll-goldens member",
    "checked-mir-golden": "regen half of the checked-mir-goldens member",
    "checked-mir-expect": "regen half of the checked-mir-expected member",
    "o2-differential-selftest": "self-test for the -O0/-O2 differential harness",
    "fuzz-oracle-selftest": "self-test for the fuzz oracle harness",
    # Not derivable, and deliberately so: the module-size ceiling is a fixed
    # constant in hew-mir/tests/lower_module_size.rs.  "Regenerating" it would
    # mean raising it, which is the one thing the ratchet forbids.  It is listed
    # here so the exclusion is a decision on the record rather than an omission.
    "lower_module_size": "a fixed line ceiling; carve the module, never re-record the bound",
}


# ── Member selection ───────────────────────────────────────────────────────────


def gate_index() -> dict[str, str]:
    """Map every gate target name to the baseline member it compares against."""
    index: dict[str, str] = {}
    for member in REGISTRY:
        for gate in member.gates:
            index[gate] = member.id
    return index


def select(
    tier: str | None,
    only: list[str],
    relevant_to: list[str],
) -> list[Baseline]:
    members = list(REGISTRY)
    if tier is not None:
        members = [m for m in members if m.tier == tier]
    if only:
        wanted = set(only)
        unknown = wanted - {m.id for m in REGISTRY}
        if unknown:
            raise SystemExit(
                f"error: no such baseline(s): {', '.join(sorted(unknown))}"
            )
        members = [m for m in members if m.id in wanted]
    if relevant_to:
        targets = lane_targets(relevant_to)
        members = [m for m in members if set(m.gates) & targets]
    return members


MAKE_RULE = re.compile(r"^([A-Za-z0-9_.\-/]+)\s*:(?!=)\s*(.*)$")
MAKE_INVOCATION = re.compile(r"\bmake\b((?:\s+[^\s|;&]+)*)")


def makefile_prerequisites() -> dict[str, list[str]]:
    """The target -> prerequisite edges of the root Makefile.

    Deliberately a shallow parse: enough to answer "does this lane reach that
    gate", which is a containment question over explicit prerequisite lists.
    """
    edges: dict[str, list[str]] = {}
    for line in (ROOT / "Makefile").read_text().splitlines():
        if line.startswith("\t") or line.startswith("#"):
            continue
        match = MAKE_RULE.match(line)
        if not match:
            continue
        target, prereqs = match.group(1), match.group(2)
        if target.startswith("."):
            continue
        edges.setdefault(target, []).extend(
            word for word in prereqs.split() if not word.startswith("$")
        )
    return edges


def lane_targets(commands: list[str]) -> set[str]:
    """Every make target a lane runs, including ones reached as prerequisites.

    A lane rarely names a baseline's gate directly: `make lint` reaches
    `wasm-capability-check` three prerequisite hops down. Matching only the
    literal command words would skip most members and quietly restore the
    late-discovery problem this precheck exists to remove.
    """
    edges = makefile_prerequisites()
    frontier: list[str] = []
    for command in commands:
        for invocation in MAKE_INVOCATION.finditer(command):
            for word in invocation.group(1).split():
                # Stop at the first VAR=value / option; make targets precede them.
                if "=" in word or word.startswith("-"):
                    break
                frontier.append(word)
    reached: set[str] = set()
    while frontier:
        target = frontier.pop()
        if target in reached:
            continue
        reached.add(target)
        frontier.extend(edges.get(target, ()))
    return reached


# ── Running ────────────────────────────────────────────────────────────────────


def run(command: str, *, capture: bool) -> tuple[int, str]:
    """Run one registry command from the repo root.

    When `capture` is set the output is both echoed and returned, because the
    ratchet prune path has to read the gate's own NOW-PASSES report.
    """
    if not capture:
        completed = subprocess.run(command, shell=True, cwd=ROOT, check=False)
        return completed.returncode, ""
    completed = subprocess.run(
        command,
        shell=True,
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    output = completed.stdout + completed.stderr
    sys.stdout.write(output)
    sys.stdout.flush()
    return completed.returncode, output


def run_requires(member: Baseline) -> int:
    for prerequisite in member.requires:
        status, _ = run(prerequisite, capture=False)
        if status != 0:
            print(
                f"error: prerequisite failed for {member.id}: {prerequisite}",
                file=sys.stderr,
            )
            return status
    return 0


# ── Snapshot / compare, for members with no native freshness check ─────────────


def snapshot(paths: tuple[str, ...]) -> dict[str, bytes]:
    contents: dict[str, bytes] = {}
    for rel in paths:
        target = ROOT / rel
        if target.is_dir():
            for child in sorted(target.rglob("*")):
                if child.is_file():
                    contents[str(child.relative_to(ROOT))] = child.read_bytes()
        elif target.is_file():
            contents[rel] = target.read_bytes()
    return contents


def restore(before: dict[str, bytes], paths: tuple[str, ...]) -> None:
    """Put the working tree back exactly as it was before a regen probe."""
    for rel in paths:
        target = ROOT / rel
        if target.is_dir():
            shutil.rmtree(target)
        elif target.is_file():
            target.unlink()
    for rel, data in before.items():
        target = ROOT / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(data)


def describe_drift(before: dict[str, bytes], after: dict[str, bytes]) -> list[str]:
    drift: list[str] = []
    for rel in sorted(set(before) | set(after)):
        if rel not in after:
            drift.append(f"removed: {rel}")
        elif rel not in before:
            drift.append(f"added:   {rel}")
        elif before[rel] != after[rel]:
            drift.append(f"changed: {rel}")
    return drift


# ── Ratcheted expected-failure lists ──────────────────────────────────────────

NOW_PASSES = "NOW-PASSES:"
UNEXPECTED = "UNEXPECTED:"


def parse_ratchet_report(output: str) -> tuple[list[str], list[str]]:
    """Pull the machine-readable verdicts out of a ratchet gate's own report.

    Every ratchet in the tree prints `NOW-PASSES: <entry>` for a listed failure
    that now passes and `UNEXPECTED: <entry>` for a failure that is not listed.
    That shared vocabulary is the contract this function depends on.
    """
    now_passes: list[str] = []
    unexpected: list[str] = []
    for line in output.splitlines():
        stripped = line.strip()
        if stripped.startswith(NOW_PASSES):
            entry = stripped[len(NOW_PASSES) :].strip().split()
            if entry:
                now_passes.append(entry[0])
        elif stripped.startswith(UNEXPECTED):
            entry = stripped[len(UNEXPECTED) :].strip().split()
            if entry:
                unexpected.append(entry[0])
    return now_passes, unexpected


TOTAL_COMMENT = re.compile(r"^#.*\bTotal:")


def _retotal(lines: list[str], before: int, after: int) -> list[str]:
    """Keep a `# Total: N ...` header honest after entries are removed.

    Some lists restate the count more than once on the same line ("28 entries
    (28 trap fixtures)"), so every standalone occurrence of the OLD count is
    rewritten and nothing else is touched — a number that was never the entry
    count stays as the author wrote it.
    """
    if before == after:
        return lines
    stale = re.compile(rf"\b{before}\b")
    return [
        stale.sub(str(after), line) if TOTAL_COMMENT.match(line) else line
        for line in lines
    ]


def entry_count(lines: list[str]) -> int:
    return sum(1 for line in lines if line.split("#", 1)[0].strip())


def prune_entries(list_path: Path, entries: list[str]) -> list[str]:
    """Remove the named entries from a ratchet list, leaving comments intact."""
    removed: list[str] = []
    targets = set(entries)
    kept: list[str] = []
    original = list_path.read_text().splitlines(keepends=True)
    for line in original:
        body = line.split("#", 1)[0].strip()
        first = body.split()[0] if body else ""
        if first and first in targets:
            removed.append(first)
            continue
        kept.append(line)
    if removed:
        kept = _retotal(kept, entry_count(original), entry_count(kept))
        list_path.write_text("".join(kept))
    return removed


def regen_ratchet(member: Baseline) -> int:
    """Recompute a ratchet list from the current compiler output.

    Removing a now-passing entry is the whole of the automatic edit.  A NEW
    failure is an error: the gate is telling you the tree regressed, and
    re-recording it would launder the regression into the baseline.
    """
    assert member.check is not None and member.prune_list is not None
    status, output = run(member.check, capture=True)
    if status == 0:
        print(f"{member.id}: already current")
        return 0

    now_passes, unexpected = parse_ratchet_report(output)
    if unexpected:
        print(
            f"\nerror: {member.id}: {len(unexpected)} NEW failure(s); a regen never "
            "records a regression:",
            file=sys.stderr,
        )
        for entry in unexpected:
            print(f"  - {entry}", file=sys.stderr)
        print(
            "Fix the failure, or add the entry by hand with its reason.",
            file=sys.stderr,
        )
        return 1
    if not now_passes:
        print(
            f"\nerror: {member.id}: the gate failed for a reason this regen cannot "
            f"resolve; run `{member.check}` and read the report.",
            file=sys.stderr,
        )
        return 1

    removed = prune_entries(ROOT / member.prune_list, now_passes)
    for entry in removed:
        print(f"{member.id}: removed now-passing entry {entry}")
    missing = sorted(set(now_passes) - set(removed))
    for entry in missing:
        print(
            f"warning: {member.id}: {entry} reported as now-passing but not found "
            f"in {member.prune_list}",
            file=sys.stderr,
        )
    return 0


# ── Commands ───────────────────────────────────────────────────────────────────


def cmd_list(args: argparse.Namespace) -> int:
    members = select(args.tier, args.only, [])
    if args.json:
        print(
            json.dumps(
                {
                    "members": [
                        {
                            "id": m.id,
                            "summary": m.summary,
                            "tier": m.tier,
                            "paths": list(m.paths),
                            "gates": list(m.gates),
                            "regen": m.regen_command(),
                        }
                        for m in members
                    ],
                    "exempt_gates": EXEMPT_GATES,
                },
                indent=2,
            )
        )
        return 0
    for member in members:
        print(f"{member.id}  [{member.tier}]")
        print(f"    {member.summary}")
        print(f"    paths:  {', '.join(member.paths)}")
        print(f"    gates:  {', '.join('make ' + g for g in member.gates)}")
        print(f"    regen:  {member.regen_command()}")
    return 0


def cmd_regen(args: argparse.Namespace) -> int:
    members = select(args.tier, args.only, [])
    failures: list[str] = []
    for member in members:
        print(
            f"\n==> baselines: regenerating {member.id} ({member.summary})", flush=True
        )
        if run_requires(member) != 0:
            failures.append(member.id)
            continue
        if member.prune_list is not None:
            status = regen_ratchet(member)
        else:
            assert member.regen is not None
            status, _ = run(member.regen, capture=False)
        if status != 0:
            failures.append(member.id)
    if failures:
        print(
            f"\n==> baselines: FAILED for {len(failures)} member(s): "
            f"{', '.join(failures)}",
            file=sys.stderr,
        )
        return 1
    print(f"\n==> baselines: {len(members)} member(s) regenerated.")
    return 0


def check_member(member: Baseline) -> tuple[bool, list[str]]:
    """Return (current, drift-detail). Never leaves the working tree mutated."""
    if run_requires(member) != 0:
        return False, ["prerequisite failed"]
    if member.check is not None:
        status, _ = run(member.check, capture=False)
        return status == 0, []

    assert member.regen is not None
    before = snapshot(member.paths)
    status, _ = run(member.regen, capture=False)
    if status != 0:
        restore(before, member.paths)
        return False, ["regen command failed"]
    after = snapshot(member.paths)
    drift = describe_drift(before, after)
    restore(before, member.paths)
    return not drift, drift


def cmd_check(args: argparse.Namespace) -> int:
    relevant_to = list(args.relevant_to)
    if args.relevant_to_file is not None:
        relevant_to.extend(
            line
            for line in args.relevant_to_file.read_text().splitlines()
            if line.strip()
        )
        # An empty lane file means "this lane runs no baseline gate", which is a
        # legitimate answer; an absent selection means "check everything". The
        # two must not collapse, or a docs lane would silently check the world.
        if not relevant_to:
            print("==> baselines-check: lane runs no baseline gate; nothing to check.")
            return 0
    members = select(args.tier, args.only, relevant_to)
    if not members:
        print("==> baselines-check: no members selected.")
        return 0

    stale: list[tuple[Baseline, list[str]]] = []
    for member in members:
        # Flushed because the member's own command writes straight to the
        # inherited fd; without this the headers arrive after everything they
        # label and the report reads as if it belonged to the wrong member.
        print(f"\n==> baselines-check: {member.id} ({member.summary})", flush=True)
        current, drift = check_member(member)
        if not current:
            stale.append((member, drift))

    if not stale:
        print(f"\n==> baselines-check: {len(members)} member(s) current.")
        return 0

    sys.stdout.flush()
    print(f"\n==> baselines-check: {len(stale)} STALE baseline(s):", file=sys.stderr)
    for member, drift in stale:
        print(f"\n  {member.id} — {member.summary}", file=sys.stderr)
        for path in member.paths:
            print(f"      artefact: {path}", file=sys.stderr)
        for detail in drift:
            print(f"      {detail}", file=sys.stderr)
        print(f"      regenerate with: {member.regen_command()}", file=sys.stderr)
    print(
        "\n  Or bring every baseline current at once: make baselines",
        file=sys.stderr,
    )
    return 1


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="regenerate and verify the tree's committed derived baselines"
    )
    sub = parser.add_subparsers(dest="command", required=True)

    def common(p: argparse.ArgumentParser) -> None:
        p.add_argument("--tier", choices=TIERS, default=None)
        p.add_argument("--only", action="append", default=[], metavar="ID")

    listing = sub.add_parser("list", help="show the registry")
    common(listing)
    listing.add_argument("--json", action="store_true")
    listing.set_defaults(func=cmd_list)

    regen = sub.add_parser("regen", help="regenerate baselines from source")
    common(regen)
    regen.set_defaults(func=cmd_regen)

    check = sub.add_parser("check", help="prove baselines are current")
    common(check)
    check.add_argument(
        "--relevant-to",
        action="append",
        default=[],
        metavar="COMMAND",
        help="restrict to members whose gate appears in this command line",
    )
    check.add_argument(
        "--relevant-to-file",
        type=Path,
        default=None,
        metavar="PATH",
        help="same, reading one command per line (how the dispatcher passes its lane)",
    )
    check.set_defaults(func=cmd_check)

    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    if args.command != "check":
        args.relevant_to = []
        args.relevant_to_file = None
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
