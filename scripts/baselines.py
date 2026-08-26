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
import fnmatch
import json
import os
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
    explicit_only: bool = False

    def regen_command(self) -> str:
        """The exact command a human runs to bring this baseline current."""
        if self.explicit_only or self.regen is None:
            return f"python3 scripts/baselines.py regen --only {self.id}"
        return self.regen


# ── Registry ───────────────────────────────────────────────────────────────────
#
# Adding a baseline means adding a row here.  check-gate-reachability's A6a
# closes the set over the TREE: every tracked file matching a shape in
# BASELINE_SHAPES below must be owned by exactly one member's `paths` or listed
# in NO_BASELINE_FILES with a reason.  A member's `paths` may be a file, a
# directory (covering its tree), or a glob.
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
        id="ffi-ownership-ratchet",
        summary="exact count of ABI-classified symbols without ownership contracts",
        tier="fast",
        paths=("scripts/ffi-ownership-ratchet.toml",),
        gates=("verify-ffi",),
        regen="make ffi-ownership-ratchet-record",
        check="make verify-ffi",
    ),
    Baseline(
        id="cabi-surface",
        summary="generated C ABI manifest and C header census",
        tier="fast",
        paths=(
            "scripts/cabi-surface.json",
            "hew-cabi/include/hew_cabi_surface.h",
        ),
        gates=("cabi-surface-check",),
        regen="make cabi-surface",
        check="make cabi-surface-check",
    ),
    Baseline(
        id="core-matrix-truth-table",
        summary="recorded outcome class of every core-matrix cell",
        tier="compiler",
        paths=("tests/core-matrix/matrix.tsv",),
        gates=("test-core-matrix",),
        regen="make core-matrix-record",
        check="make test-core-matrix",
        # The table records what each primitive x operation cell DOES today,
        # including the cells that fail. Re-recording it in a sweep would erase a
        # regression by writing the regression down, so the gate stays red until
        # someone names this member and says in the commit which cells moved.
        #
        # MEASURED 2026-08-21, and a trap for whoever re-records next: `--record`
        # is NOT idempotent. Six `*__map_key` cells report a different line:col
        # for the same source on consecutive runs (enum_payload 9:12 vs 11:11,
        # hashset 5:12 vs 6:5, vec 7:11 vs 5:12, ...) -- same message, same
        # outcome class, nondeterministic span. Separately, the committed detail
        # column is stale against the `::`-to-`.` method-syntax change in
        # diagnostics. Neither is visible to `make test-core-matrix`, which
        # compares the outcome CLASS column only. Fix the span nondeterminism
        # before re-recording, or the refresh commits noise.
        explicit_only=True,
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
        paths=("examples/v05/checked-mir/*.expected",),
        gates=("checked-mir-run",),
        regen="make checked-mir-expect",
        check="make checked-mir-run",
    ),
    Baseline(
        id="ux-example-expectations",
        summary="ux + progressive tutorial .expected transcripts",
        tier="compiler",
        paths=("examples/ux", "examples/progressive"),
        gates=("test-ux-examples",),
        regen="make ux-examples-expect",
        check="make test-ux-examples",
        # An example's combined stdout/stderr IS its user-facing contract. A
        # blanket `make baselines` must never rewrite it: that would turn a
        # behaviour regression into a committed expectation without anyone
        # deciding to. Changed output stays a gate failure until a human names
        # this member explicitly, exactly as a NOW-FAILS entry stays an error
        # until it is fixed. The regen itself refuses to record a nonzero exit
        # or a timeout at all (example-expectations.py --write-expected).
        explicit_only=True,
    ),
    Baseline(
        id="surface-example-expectations",
        summary="v0.5 surface + http flagship .expected transcripts",
        tier="compiler",
        paths=(
            "examples/v05/surfaces",
            "examples/net/http_await_service.expected",
        ),
        gates=("test-surface-examples",),
        regen="make surface-examples-expect",
        check="make test-surface-examples",
        explicit_only=True,
    ),
    Baseline(
        id="funcupdate-mir-baselines",
        summary="funcupdate/reassign elaborated-MIR interface pin",
        tier="compiler",
        paths=("tests/mir-baselines/funcupdate-reassign",),
        gates=(),
        regen="make funcupdate-mir-baselines-golden",
        check=(
            "cargo nextest run -p hew-cli --profile ci "
            "-E 'test(funcupdate_reassign_elab_mir_matches_committed_baselines)'"
        ),
        # The dump's function ORDER is nondeterministic (map iteration), so the
        # harness normalizes before comparing and a raw re-dump is not
        # byte-stable. Regeneration is therefore a reviewed act, exactly as the
        # manifest header has always said, and never a side effect of a sweep.
        explicit_only=True,
    ),
    Baseline(
        id="release-count-baseline",
        summary="per-(file, function) elaborated release counts over std/ and examples/",
        tier="compiler",
        paths=("hew-cli/tests/fixtures/release-count-baseline.tsv",),
        gates=(),
        regen=(
            "HEW_RELEASE_COUNT_CAPTURE=1 cargo nextest run -p hew-cli "
            "-E 'binary(stdlib_corpus_release_count_differential)' --no-capture"
        ),
        check=(
            "cargo nextest run -p hew-cli --profile ci "
            "-E 'binary(stdlib_corpus_release_count_differential)'"
        ),
        # A DROP in a release count is a leak. Re-recording it in a sweep would
        # erase the finding, so the capture stays a named, explained act.
        explicit_only=True,
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
        id="stdlib-user-build-calls",
        summary="public stdlib function calls exercised by the user-build gate",
        tier="compiler",
        paths=("scripts/stdlib-user-build-calls.tsv",),
        gates=("stdlib-user-build-clean",),
        regen="python3 scripts/stdlib-user-build-clean.py --write-calls",
        check=None,
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


# ── The file closure ───────────────────────────────────────────────────────────
#
# The authority for "is this set closed" is the TREE, not the gates. Two earlier
# attempts asked the question from the gate side — first by target name, then by
# analysing what each gate's scripts read — and both could be walked past: a
# reader that builds its path through a variable is invisible to the second, and
# a gate named nothing in particular is invisible to the first.
#
# So the question is asked of the files. Every tracked file whose NAME says it is
# a committed baseline must be covered by exactly one registry member, or by a
# NO_BASELINE_FILES entry that records why it is not derived. Nothing about how a
# script reaches it matters.

BASELINE_SHAPES = (
    "*.expected",
    "*/golden/*",
    "*expected-failures*.txt",
    "*ratchet*.txt",
    # A ratchet is a ratchet whatever it is serialized as. The .txt-only shape
    # missed scripts/ffi-ownership-ratchet.toml, which pins an exact count that
    # verify-ffi-symbols.py compares on every `make lint`.
    "*ratchet*.toml",
    "*ratchet*.tsv",
    "*ratchet*.json",
    # Every data table a checker keeps under scripts/ is a decision on the
    # record: either something regenerates it, or NO_BASELINE_FILES says why
    # nothing does. This is where checkers put the files they compare against.
    "scripts/*.toml",
    "scripts/*.tsv",
    "scripts/*.json",
    "*-inventory.tsv",
    "*census*",
    "*baseline*.tsv",
    "*baseline*.txt",
    "*baseline*.json",
    "tests/ll-oracle/*",
    "*.mir",
    "*_generated.*",
    "*.generated.*",
    "*matrix.tsv",
    "THIRD-PARTY-LICENSES",
)


# Tracked files that LOOK like baselines and are not. Every entry is a decision
# on the record: the reason says what the file is instead, and A6 reports an
# entry that stops matching anything so the list cannot rot.
NO_BASELINE_FILES: tuple[tuple[str, str], ...] = (
    (
        "scripts/preflight-command-weights.tsv",
        "measured elapsed seconds per preflight command, derived from a PROFILED "
        "RUN rather than from the tree: `make baselines` cannot regenerate it "
        "without executing a full comprehensive preflight, and no check can "
        "prove it current against a tree that carries no timing. It is also not "
        "a contract -- it only balances the shard partition, which stays "
        "exhaustive and disjoint whatever the weights say, and an unmeasured "
        "command falls back to its timeout floor. Refresh it deliberately with "
        "`make preflight-weights-regen PROFILE_JSON=<path>`",
    ),
    (
        "scripts/fixtures/*",
        "inputs to the checkers' own self-tests (sanitizer waivers, release-lib-link "
        "scaffolding); they are counterfactuals a gate is driven against, never "
        "output a gate compares the tree to",
    ),
    (
        "scripts/jit-symbol-classification.toml",
        "the JIT host-ABI classification and its two named escape hatches; a "
        "hand-authored declaration of which symbols are exempt and why, not derived",
    ),
    (
        "scripts/opaque-resource-lifecycle-evidence.json",
        "hand-authored runtime/wasm execution evidence per shipped opaque resource "
        "(schema 2). The AST-derived facts it is cross-checked against are a "
        "different artefact written by structural-authority-audit.py "
        "--opaque-resource-facts; this file is the evidence, not the facts",
    ),
    (
        "scripts/stdlib-execution-proofs.tsv",
        "the manifest of which fixture proves which stdlib module; a hand-authored "
        "index of intent, not derived output",
    ),
    (
        "tests/ownership-balance/baseline.tsv",
        "exact per-fixture ownership-diagnostic expectations plus the runtime mode "
        "each fixture is executed under (`clean` must report zero leaks, `leaks` "
        "must report one). Re-recording it from observed output would make the "
        "compiler agree with whatever it currently does and would let a silenced "
        "advisory or a new leak regenerate itself green (LESSONS "
        "oracle-encodes-intent-not-observed-output). Every row is a decision, "
        "changed by hand with the measurement that justifies it",
    ),
    (
        "tests/vertical-slice/*",
        "accept/reject fixture expectations state INTENDED behaviour. Re-recording "
        "them from observed output would make the compiler's own oracle agree with "
        "whatever the compiler currently does (LESSONS oracle-encodes-intent-not-"
        "observed-output), so this corpus has no regen by design",
    ),
    (
        "tests/pkg-import/*",
        "cross-module import oracle; its expectations state intended behaviour",
    ),
    (
        "tests/corpus/migrate/*",
        "each entry pairs a pre-migration source with the exact post-migration form "
        "the migration is specified to produce; re-recording from the migrator's "
        "output would make the oracle agree with the migrator",
    ),
    (
        "examples/playground/*",
        "compared by the sandbox-VM parity gate (hew-sandbox-wasm/tests/playground.rs) "
        "against VM execution. The expectation is the agreement point between the "
        "native compiler and the VM; re-recording it from either side would collapse "
        "the parity oracle into a tautology",
    ),
    (
        "examples/*",
        "reference output shipped beside an example for readers. No gate compares it "
        "-- the gated example corpora are the ux/progressive, v05-surfaces and "
        "checked-mir members above, and hew-check-all only type-checks the rest. An "
        "ungated .expected here is a documentation artefact, not a baseline",
    ),
)


# ── Gates that compare against something other than a committed baseline ────────
#
# check-gate-reachability walks the Makefile for gate-shaped target names and
# requires each one to be reachable from this file: either it is a registered
# member's gate, or it appears here with the reason it owns no derived artefact.
# A new `*-check` target with a committed comparison file and no registry row is
# a lint failure — that is the whole point.

EXEMPT_GATES: dict[str, str] = {
    "baselines-check": (
        "the aggregate check over this registry; it owns no artefact of its own"
    ),
    "preflight-weights-regen": (
        "scripts/preflight-command-weights.tsv is derived from a PROFILED RUN, "
        "not from the tree: it holds the elapsed seconds a real hosted "
        "comprehensive preflight measured. `make baselines` cannot regenerate "
        "it without executing that preflight, and `baselines-check` cannot "
        "prove it current against a tree that carries no timing. It is also "
        "not a contract -- a stale weight costs shard makespan and never "
        "coverage, because the partition is exhaustive and disjoint whatever "
        "the weights say, and an unmeasured command falls back to its timeout "
        "floor"
    ),
    "preflight-weights-drift": (
        "reports drift between that timing corpus and a supplied profile "
        "without writing anything and without gating; same artefact, same "
        "reason"
    ),
    "check-libhew-fresh": (
        "compares build-input hashes against a stamp under target/; nothing committed"
    ),
    "libhew-debug": (
        "scripts/libhew-freshness.py stamps build inputs; the freshness it reads is a "
        "build artefact, not a committed one"
    ),
    "structural-lint-bootstrap-install": (
        "provisions the pinned ast-grep toolchain (--install-only); the audit that reads "
        "the authority inventory is the structural-lint member's gate"
    ),
    "fuzz-smoke-bootstrap-install": (
        "provisions the libFuzzer toolchain; it compares nothing"
    ),
    "pre-release": (
        "validates built release artefacts against the release contract, not the tree "
        "against a committed file"
    ),
    "lint-wasm-todo": (
        "validates the repository's WASM backlog markers against a hand-authored "
        "authority; that authority is written by people, not derived from the tree"
    ),
    "test-o2-differential": (
        "compares two runs of the same program (-O0 against -O2). The oracle is "
        "self-referential; there is no committed artefact to regenerate"
    ),
    "forced-cancel-composite-check": (
        "checks emitted IR and live probe behaviour; it reads no committed derived "
        "artefact"
    ),
    # The four entries below share one reason, and it is the strongest reason a
    # comparison can have for staying outside the registry: their expectations
    # state INTENDED behaviour, so re-recording them from observed output would
    # make each oracle agree with whatever the compiler currently does. That is
    # the `oracle-encodes-intent-not-observed-output` failure in LESSONS.md, and
    # a regen command for these corpora would be a tool for committing it.
    "test-vertical-slice": (
        "accept/reject fixture expectations state intended behaviour; they are never "
        "re-recorded from observed output"
    ),
    "test-pkg-import": (
        "cross-module import oracle; its expectations state intended behaviour"
    ),
    "test-package-install": (
        "package-manager consumer oracle; its expectations state intended behaviour"
    ),
    "test-stdlib-execution-proofs": (
        "each stdlib module's executable proof states intended behaviour"
    ),
    "test-migrate-corpus": (
        "pairs a pre-migration source with the exact post-migration form the migration "
        "is specified to produce; re-recording it from the migrator's own output would "
        "make the oracle agree with the migrator"
    ),
    # NOT an entry, deliberately: the hew-mir module-size ceiling
    # (hew-mir/tests/lower_module_size.rs) is a fixed constant compared against
    # nothing on disk, so A6 never discovers it and an exemption for it would be
    # dead text. "Regenerating" it would mean raising the ceiling, which is the
    # one thing that ratchet forbids.
}


# ── Member selection ───────────────────────────────────────────────────────────


def _matches(path: str, pattern: str) -> bool:
    """Match a repo-relative path against a member path or a shape.

    A pattern without a wildcard is a prefix: naming a directory covers its
    tree, which is how members declare corpora. A pattern with one covers the
    whole path, so `*/golden/*` reaches any depth -- fnmatch does not treat `/`
    specially, and that is what is wanted here.
    """
    if any(ch in pattern for ch in "*?["):
        return fnmatch.fnmatch(path, pattern)
    return path == pattern or path.startswith(pattern.rstrip("/") + "/")


def is_baseline_shaped(path: str) -> bool:
    return any(_matches(path, shape) for shape in BASELINE_SHAPES)


def tracked_files() -> list[str]:
    return subprocess.run(
        ["git", "ls-files"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    ).stdout.split("\n")


def coverage(paths: list[str]) -> tuple[list[tuple[str, list[str]]], list[str]]:
    """Close the set over FILES.

    Returns (findings, stale-no-baseline-patterns). A finding is a
    baseline-shaped tracked file with the member ids covering it: zero means
    nobody regenerates what some gate compares, and two or more means two
    members would fight over the same bytes.
    """
    member_paths = [(m.id, p) for m in REGISTRY for p in m.paths]
    findings: list[tuple[str, list[str]]] = []
    used: set[str] = set()
    for path in paths:
        if not path or not is_baseline_shaped(path):
            continue
        owners = [mid for mid, pattern in member_paths if _matches(path, pattern)]
        if len(owners) == 1:
            continue
        if not owners:
            excuse = next(
                (
                    pattern
                    for pattern, _ in NO_BASELINE_FILES
                    if _matches(path, pattern)
                ),
                None,
            )
            if excuse is not None:
                used.add(excuse)
                continue
        findings.append((path, owners))
    stale = [pattern for pattern, _ in NO_BASELINE_FILES if pattern not in used]
    return findings, stale


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
MAKE_APPEND = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*)\s*\+=\s*(.*?)\s*$")
MAKE_VARIABLE_REF = re.compile(r"\$\$?\(([A-Za-z_][A-Za-z0-9_]*)\)")


def makefile_prerequisites() -> dict[str, list[str]]:
    """The target -> prerequisite edges of the root Makefile.

    Deliberately a shallow parse: enough to answer "does this lane reach that
    gate", which is a containment question over explicit prerequisite lists.
    """
    lines = (ROOT / "Makefile").read_text().splitlines()
    appended: dict[str, list[str]] = {}
    for line in lines:
        if line.startswith("\t"):
            continue
        bare = line.split("#", 1)[0].strip()
        match = MAKE_APPEND.match(bare)
        if match is not None:
            appended.setdefault(match.group(1), []).extend(match.group(2).split())

    edges: dict[str, list[str]] = {}
    for line in lines:
        if line.startswith("\t") or line.startswith("#"):
            continue
        match = MAKE_RULE.match(line)
        if not match:
            continue
        target, prereqs = match.group(1), match.group(2).split("#", 1)[0]
        if target.startswith("."):
            continue
        prereqs = MAKE_VARIABLE_REF.sub(
            lambda ref: " ".join(appended.get(ref.group(1), ())), prereqs
        )
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


@dataclass(frozen=True)
class Entry:
    """One filesystem entry as it stood before a regen probe.

    Content alone is not the artefact. A snapshot that recorded only file bytes
    silently dropped empty directories, flattened symlinks into their targets,
    and reset the executable bit — so a check over a corpus containing a
    generated script or a symlinked fixture would "restore" a tree that differs
    from the one it found. `kind` and `mode` are part of the artefact and are
    carried through restore.
    """

    kind: str  # "file" | "dir" | "symlink"
    mode: int
    payload: bytes  # file contents, or the symlink target, or empty for a dir


def _entry_for(path: Path) -> Entry | None:
    """Read one entry without following symlinks."""
    try:
        stat = path.lstat()
    except FileNotFoundError:
        return None
    mode = stat.st_mode & 0o7777
    if path.is_symlink():
        return Entry("symlink", mode, os.readlink(path).encode())
    if path.is_dir():
        return Entry("dir", mode, b"")
    return Entry("file", mode, path.read_bytes())


def snapshot(paths: tuple[str, ...]) -> dict[str, Entry]:
    entries: dict[str, Entry] = {}
    for rel in paths:
        target = ROOT / rel
        entry = _entry_for(target)
        if entry is None:
            continue
        entries[rel] = entry
        if entry.kind == "dir":
            for child in sorted(target.rglob("*")):
                child_entry = _entry_for(child)
                if child_entry is not None:
                    entries[str(child.relative_to(ROOT))] = child_entry
    return entries


def restore(before: dict[str, Entry], paths: tuple[str, ...]) -> None:
    """Put the working tree back exactly as it was before a regen probe."""
    for rel in paths:
        target = ROOT / rel
        if target.is_symlink() or target.is_file():
            target.unlink()
        elif target.is_dir():
            shutil.rmtree(target)
    # Shortest path first, so a directory exists before its children land.
    for rel in sorted(before, key=lambda r: r.count("/")):
        entry = before[rel]
        target = ROOT / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        if entry.kind == "dir":
            target.mkdir(exist_ok=True)
        elif entry.kind == "symlink":
            target.symlink_to(entry.payload.decode())
            continue  # a symlink's own mode is not portably settable
        else:
            target.write_bytes(entry.payload)
        os.chmod(target, entry.mode)


def describe_drift(before: dict[str, Entry], after: dict[str, Entry]) -> list[str]:
    drift: list[str] = []
    for rel in sorted(set(before) | set(after)):
        if rel not in after:
            drift.append(f"removed: {rel}")
        elif rel not in before:
            drift.append(f"added:   {rel}")
        elif before[rel] != after[rel]:
            was, now = before[rel], after[rel]
            if was.kind != now.kind:
                drift.append(f"changed: {rel} ({was.kind} -> {now.kind})")
            elif was.mode != now.mode:
                drift.append(f"changed: {rel} (mode {was.mode:o} -> {now.mode:o})")
            else:
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
    if not args.only:
        deferred = [m for m in members if m.explicit_only]
        members = [m for m in members if not m.explicit_only]
        for member in deferred:
            print(
                f"==> baselines: SKIPPING {member.id} — its artefact records "
                f"observed behaviour, so a sweep would write down whatever "
                f"changed. Re-record it deliberately: {member.regen_command()}"
            )
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
