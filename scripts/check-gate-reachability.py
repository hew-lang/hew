#!/usr/bin/env python3
"""check-gate-reachability.py — assert every gate in this repo is actually run.

The sibling gate `scripts/check-preflight-ci-parity.sh` asserts that local
preflight and CI agree about the checks they *share*. It says nothing about a
check that neither of them runs. That blind spot let eight gate targets, an
entire test crate, a whole test binary and a pile of `#[ignore]`d tests sit in
the tree looking like coverage while executing nowhere.

This gate closes it, in four directions:

  A0  self-anchor — this checker is invoked by a CI workflow step.
  A1  every gate-shaped Makefile target is reached by a CI workflow step, by the
      workflow, or transitively as a prerequisite of one that is.
  A2  every workspace crate is covered by a CI test invocation: included in a
      `--workspace` run that does not `--exclude` it, or named with `-p` by a
      CI step or by a CI-reached Makefile target.
  A3  every exclusion is compensated:
      a) CI never runs a nextest profile other than `ci` (so a fast local tier
         cannot quietly become the CI tier);
      b) every `binary(...)`/`package(...)`/`test(...)` term subtracted from
         `profile.ci`'s default-filter is named by a CI-reached invocation;
      c) `#[ignore]` is permitted only in a crate whose ignored tests are run by
         a CI-reached target (`--run-ignored` / `-- --ignored`); and
      d) every inline `-E` filter a CI step passes to nextest is compensated by
         some other CI step that runs the same crate scope unfiltered. An `-E`
         is an exclusion like any other: without a compensating unfiltered run,
         the tests it subtracts execute in no job at all.

There is deliberately no waiver list. An unreached gate is either wired in or
deleted; "tracked for later" is how the eight orphans got there in the first
place. A gate this checker cannot see is a gate that is not running.

Usage:
  scripts/check-gate-reachability.py            # check
  scripts/check-gate-reachability.py --verbose  # include the reached sets
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
MAKEFILE = REPO_ROOT / "Makefile"
WORKFLOW_DIR = REPO_ROOT / ".github" / "workflows"
DISPATCHER = REPO_ROOT / "scripts" / "ci-preflight-dispatcher.sh"
NEXTEST_TOML = REPO_ROOT / ".config" / "nextest.toml"
ROOT_CARGO = REPO_ROOT / "Cargo.toml"

SELF_TARGET = "check-gate-reachability"

# A target is a GATE — something that asserts rather than builds — when its name
# matches one of these. Build/publish/scaffold targets (`hew`, `runtime`,
# `wasm`, `release`, `install`) are out of scope: they have no verdict to lose.
GATE_NAME_RE = re.compile(
    r"""^(
          test | test-.* | check-.* | .*-check | .*-gate | .*-selftest
        | .*-ratchet | lint | lint-.* | .*-lint | leak-scan | verify-ffi
        | asan | asan-fixtures | tsan | miri | ll-diff | grammar
        | fuzz-oracle | sandbox-parity | licenses-check
        | libhew-link-race-test | observe-functional-test
        )$""",
    re.VERBOSE,
)

# Nextest profiles CI is allowed to run. Anything else would let a fast local
# iteration tier (which excludes most of the corpus) stand in for the CI tier.
CI_ALLOWED_NEXTEST_PROFILES = {"ci"}


class Findings:
    def __init__(self) -> None:
        self.failures: list[str] = []

    def fail(self, assertion: str, subject: str, detail: str) -> None:
        self.failures.append(f"  FAIL [{assertion}] {subject}\n       {detail}")


# ── Makefile ──────────────────────────────────────────────────────────────────

RULE_RE = re.compile(r"^([A-Za-z0-9_./%-]+(?:\s+[A-Za-z0-9_./%-]+)*)\s*:(?!=)\s*(.*)$")


def parse_makefile(text: str) -> tuple[set[str], dict[str, set[str]], dict[str, str]]:
    """Return (phony targets, target → prerequisites, target → recipe text)."""
    phony: set[str] = set()
    prereqs: dict[str, set[str]] = {}
    recipes: dict[str, str] = {}
    current: list[str] = []
    for raw in text.splitlines():
        if raw.startswith("\t"):
            for tgt in current:
                recipes[tgt] = recipes.get(tgt, "") + raw + "\n"
            continue
        if raw.startswith(".PHONY:"):
            phony.update(raw[len(".PHONY:") :].split())
            current = []
            continue
        stripped = raw.split("#", 1)[0].rstrip()
        if not stripped or stripped[0].isspace():
            if not stripped:
                current = []
            continue
        match = RULE_RE.match(stripped)
        if not match:
            current = []
            continue
        current = match.group(1).split()
        deps = set(match.group(2).split())
        for tgt in current:
            prereqs.setdefault(tgt, set()).update(deps)
            recipes.setdefault(tgt, "")
    return phony, prereqs, recipes


# ── Roots: what CI and the dispatcher invoke directly ─────────────────────────

MAKE_INVOKE_RE = re.compile(
    r"(?<![\w-])g?make\s+((?:[A-Za-z0-9_.-]+\s+)*[A-Za-z0-9_.-]+)"
)


def make_targets_in(text: str, known: set[str]) -> set[str]:
    """Every known target named by a `make`/`gmake` invocation in `text`.

    Multi-target invocations (`make verify-ffi test-verify-ffi`) count for each
    named target; trailing VAR=value arguments are not targets and are dropped
    by the `known` filter.
    """
    found: set[str] = set()
    for match in MAKE_INVOKE_RE.finditer(text):
        for word in match.group(1).split():
            if word in known:
                found.add(word)
            else:
                break
    return found


def workflow_files() -> list[Path]:
    return sorted(WORKFLOW_DIR.glob("*.yml")) + sorted(WORKFLOW_DIR.glob("*.yaml"))


def workflow_text() -> str:
    return "\n".join(path.read_text() for path in workflow_files())


# ── Reachability closure ──────────────────────────────────────────────────────


def close_over_makefile(
    roots: set[str],
    prereqs: dict[str, set[str]],
    recipes: dict[str, str],
    known: set[str],
) -> set[str]:
    """Expand `roots` with prerequisites and recipe-level `$(MAKE) x` recursion."""
    reached = set(roots)
    frontier = list(roots)
    while frontier:
        target = frontier.pop()
        nxt = set(prereqs.get(target, set()))
        recipe = recipes.get(target, "")
        for match in re.finditer(r"\$\(MAKE\)\s+([A-Za-z0-9_.-]+)", recipe):
            nxt.add(match.group(1))
        nxt |= make_targets_in(recipe, known)
        for dep in nxt:
            if dep in known and dep not in reached:
                reached.add(dep)
                frontier.append(dep)
    return reached


SELECTOR_RE = re.compile(
    r"^\s*cargo nextest run\s+--profile ci\s+((?:[^\n]|\\\n)*)$", re.M
)


def selected_crates(recipe: str) -> list[str] | None:
    """Crates a `cargo nextest run --profile ci -p ...` recipe selects.

    Returns None when the recipe is anything else (a script, a shell loop, a
    workspace run, or a run carrying an extra `-E` filter) — only a plain
    per-crate selection can have its coverage proved by containment.
    """
    body = recipe.strip()
    if body.count("\n") != body.replace("\\\n", "").count("\n"):
        pass
    flat = body.replace("\\\n", " ")
    lines = [ln.strip() for ln in flat.splitlines() if ln.strip()]
    if len(lines) != 1:
        return None
    line = lines[0]
    if not line.startswith("cargo nextest run --profile ci "):
        return None
    rest = line[len("cargo nextest run --profile ci ") :].split()
    crates: list[str] = []
    i = 0
    while i < len(rest):
        token = rest[i]
        if token == "-p" and i + 1 < len(rest):
            crates.append(rest[i + 1])
            i += 2
            continue
        if token in {"--no-fail-fast"}:
            i += 1
            continue
        # Anything else (-E, --workspace, --no-default-features, …) changes what
        # runs, so containment cannot be proved mechanically.
        return None
    return crates or None


# ── A2 / A3 helpers ───────────────────────────────────────────────────────────


def workspace_members() -> list[str]:
    text = ROOT_CARGO.read_text()
    block = re.search(r"^members\s*=\s*\[(.*?)\]", text, re.S | re.M)
    if not block:
        raise SystemExit("error: could not parse [workspace] members from Cargo.toml")
    return [m for m in re.findall(r'"([^"]+)"', block.group(1))]


def crate_name(member_path: str) -> str:
    manifest = REPO_ROOT / member_path / "Cargo.toml"
    match = re.search(r'^\s*name\s*=\s*"([^"]+)"', manifest.read_text(), re.M)
    if not match:
        raise SystemExit(f"error: no package name in {manifest}")
    return match.group(1)


def ci_test_commands(
    workflows: str, recipes: dict[str, str], reached: set[str]
) -> list[str]:
    """Text of every test invocation CI can reach: workflow bodies plus the
    recipes of the Makefile targets CI reaches."""
    return [workflows] + [recipes.get(t, "") for t in sorted(reached)]


WORKSPACE_RUN_RE = re.compile(
    r"(cargo\s+(?:nextest\s+run|test|llvm-cov)[^\n]*--workspace(?:[^\n]|\\\n)*)"
)


def crate_covered(crate: str, blobs: list[str]) -> bool:
    for blob in blobs:
        if re.search(rf"-p\s+{re.escape(crate)}(?![\w-])", blob):
            return True
        for run in WORKSPACE_RUN_RE.finditer(blob):
            if not re.search(rf"--exclude\s+{re.escape(crate)}(?![\w-])", run.group(1)):
                return True
    return False


def profile_ci_exclusions() -> list[tuple[str, str]]:
    text = NEXTEST_TOML.read_text()
    section = re.search(r"^\[profile\.ci\]$(.*?)^\[", text, re.S | re.M)
    if not section:
        raise SystemExit("error: could not locate [profile.ci] in .config/nextest.toml")
    filt = re.search(r'^default-filter\s*=\s*"([^"]*)"', section.group(1), re.M)
    if not filt:
        raise SystemExit("error: [profile.ci] has no default-filter to check")
    return [
        (kind, name)
        for kind, name in re.findall(
            r"-\s*(binary|package|test)\(([^)]+)\)", filt.group(1)
        )
    ]


IGNORE_RE = re.compile(r"^\s*#\[ignore\b")
# `.*?` under re.S rather than `(?:.|\n)*?`: the alternation form backtracks
# quadratically on the multi-thousand-line test files in this tree.
STRING_LIT_RE = re.compile(r'r#*".*?"#*|"(?:\\.|[^"\\])*"', re.S)
PRUNED_DIRS = {"target", "node_modules", ".git"}


def crates_with_ignored_tests(members: list[str]) -> dict[str, list[str]]:
    """crate name → source files carrying a real `#[ignore]` attribute.

    Rust string literals are stripped first: the `hew` test runner's own fixtures
    embed `#[ignore]` in Hew source strings, and that text is data, not an
    attribute on a Rust test.
    """
    out: dict[str, list[str]] = {}
    for member in members:
        name = crate_name(member)
        for dirpath, dirnames, filenames in os.walk(REPO_ROOT / member):
            dirnames[:] = sorted(d for d in dirnames if d not in PRUNED_DIRS)
            for filename in sorted(filenames):
                if not filename.endswith(".rs"):
                    continue
                path = Path(dirpath) / filename
                source = STRING_LIT_RE.sub("", path.read_text(errors="replace"))
                if any(IGNORE_RE.match(line) for line in source.splitlines()):
                    out.setdefault(name, []).append(str(path.relative_to(REPO_ROOT)))
    return out


IGNORED_RUN_RE = re.compile(r"--run-ignored|--\s+--ignored|--ignored")


def ignored_tests_run_for(crate: str, blobs: list[str]) -> bool:
    for blob in blobs:
        for line in blob.replace("\\\n", " ").splitlines():
            if re.search(
                rf"-p\s+{re.escape(crate)}(?![\w-])", line
            ) and IGNORED_RUN_RE.search(line):
                return True
    return False


# ── A3d helpers: inline `-E` filter exclusions ────────────────────────────────

RUN_KEY_RE = re.compile(r"^(\s*)run:\s*(.*)$")


def run_commands(text: str) -> list[str]:
    """Every `run:` command in a workflow, with folded/literal blocks joined.

    A step's command may be a one-liner or a `>-`/`|` block; both must read as a
    single command string or an `-E` on its own continuation line is invisible.
    """
    commands: list[str] = []
    lines = text.splitlines()
    index = 0
    while index < len(lines):
        match = RUN_KEY_RE.match(lines[index])
        if not match:
            index += 1
            continue
        indent, inline = match.group(1), match.group(2).strip()
        index += 1
        if inline and not inline.startswith((">", "|")):
            commands.append(inline)
            continue
        body: list[str] = []
        while index < len(lines):
            line = lines[index]
            if line.strip() and len(line) - len(line.lstrip()) <= len(indent):
                break
            body.append(line.strip())
            index += 1
        commands.append(" ".join(part for part in body if part))
    return commands


DASH_E_RE = re.compile(r"-E\s+'([^']*)'")
SELECTOR_RE = re.compile(r"(binary|package|test)\(([^)]*)\)")


def nextest_runs_in_ci() -> list[tuple[str, str]]:
    """(workflow-relative path, command) for every CI nextest invocation."""
    runs: list[tuple[str, str]] = []
    for path in workflow_files():
        rel = str(path.relative_to(REPO_ROOT))
        for command in run_commands(path.read_text()):
            if re.search(r"cargo\s+(?:nextest\s+run|llvm-cov\s+nextest)", command):
                runs.append((rel, command))
    return runs


def unfiltered_cover(runs: list[tuple[str, str]]) -> str | None:
    """A CI nextest run over the whole workspace that carries no `-E`.

    Deliberately coarse: a run whose own `-E` happens not to mention a given
    selector is not accepted as compensation, because a second filter can still
    remove the same tests by a different term. Only a genuinely unfiltered
    workspace run proves the subtracted tests execute somewhere. Which *crates*
    that run covers is A2's question, not this one's.
    """
    for rel, command in runs:
        if DASH_E_RE.search(command):
            continue
        if "--workspace" in command:
            return f"{rel}: {command.strip()[:90]}"
    return None


# ── Main ──────────────────────────────────────────────────────────────────────


def main() -> int:
    verbose = "--verbose" in sys.argv[1:]
    findings = Findings()

    makefile_text = MAKEFILE.read_text()
    phony, prereqs, recipes = parse_makefile(makefile_text)
    known = set(prereqs) | phony
    workflows = workflow_text()
    dispatcher = DISPATCHER.read_text()

    # ── A0: this checker is itself reached by CI ──────────────────────────────
    print("==> A0: reachability gate is invoked by CI")
    if SELF_TARGET in make_targets_in(workflows, known) or re.search(
        r"check-gate-reachability\.py", workflows
    ):
        print(f"     ok — a CI workflow step runs `{SELF_TARGET}`.")
    else:
        findings.fail(
            "A0",
            SELF_TARGET,
            "no CI workflow step invokes this gate. A reachability gate that "
            "nothing runs is the exact defect it exists to catch; add the step "
            "back to .github/workflows/ci.yml.",
        )

    # ── A1: every gate target is reached ──────────────────────────────────────
    # Roots are CI workflow invocations ONLY. The local preflight dispatcher is
    # deliberately NOT a root: it is a convenience that predicts CI, not an
    # authority that gates merges. Accepting a dispatcher-only edge would let a
    # gate be "reached" while never running on a pull request — the exact hole
    # this script exists to close. The reverse direction (a CI-required step
    # missing from the dispatcher) is check-preflight-ci-parity.sh's job, so
    # between the two every gate is pinned to both graphs.
    roots = make_targets_in(workflows, known)
    reached = close_over_makefile(roots, prereqs, recipes, known)

    gates = sorted(t for t in phony if GATE_NAME_RE.match(t))
    print(f"\n==> A1: Makefile gate-target reachability ({len(gates)} gate targets)")
    if verbose:
        print("    Roots (invoked directly by a CI workflow step):")
        for t in sorted(roots):
            print(f"      - {t}")
    # A per-crate selector is reached when everything it selects already runs in
    # CI. That is a containment PROOF, not an exemption: `make test-types` is a
    # narrower spelling of tests the CI workspace run executes, so it cannot
    # hide an unrun assertion. A selector naming a crate CI does not run stays
    # unreached and must be wired or cut like anything else.
    selector_blobs = ci_test_commands(workflows, recipes, reached)
    covered_selectors = set()
    for target in gates:
        if target in reached:
            continue
        picked = selected_crates(recipes.get(target, ""))
        if picked and all(crate_covered(c, selector_blobs) for c in picked):
            covered_selectors.add(target)
    reached |= covered_selectors

    unreached = [t for t in gates if t not in reached]
    for target in unreached:
        findings.fail(
            "A1",
            f"make {target}",
            "reached by no CI workflow step. Wire it into the job where it "
            "belongs, or delete the target and everything that exists only to "
            "serve it. A local-preflight-only edge does not count: it never "
            "runs on a pull request.",
        )
    print(f"    {len(gates) - len(unreached)}/{len(gates)} gate targets reached.")

    # ── A2: every workspace crate is tested by CI ─────────────────────────────
    members = workspace_members()
    crates = [crate_name(m) for m in members]
    blobs = ci_test_commands(workflows, recipes, reached)
    print(f"\n==> A2: workspace crate coverage ({len(crates)} crates)")
    uncovered = [c for c in crates if not crate_covered(c, blobs)]
    for crate in uncovered:
        findings.fail(
            "A2",
            crate,
            "no CI test invocation covers this crate: it is excluded from every "
            "--workspace run and named by no -p step. An --exclude that removes "
            "a crate from CI is invisible to the preflight parity checker, which "
            "only asserts CI is a subset of local.",
        )
    print(f"    {len(crates) - len(uncovered)}/{len(crates)} crates covered.")

    # ── A3a: CI runs only the ci nextest profile ──────────────────────────────
    print("\n==> A3a: CI uses no fast-tier nextest profile")
    bad_profiles = sorted(
        set(re.findall(r"--profile\s+([A-Za-z0-9_-]+)", workflows))
        - CI_ALLOWED_NEXTEST_PROFILES
    )
    # `--profile` also names cargo build profiles (release, dev); only flag the
    # ones that are nextest profiles defined in .config/nextest.toml.
    nextest_profiles = set(
        re.findall(r"^\[profile\.([A-Za-z0-9_-]+)\]", NEXTEST_TOML.read_text(), re.M)
    )
    bad_profiles = [p for p in bad_profiles if p in nextest_profiles]
    for profile in bad_profiles:
        findings.fail(
            "A3a",
            f"--profile {profile}",
            "a CI step runs a nextest profile other than `ci`. The fast tiers "
            "exclude most of the corpus; letting one gate CI silently shrinks "
            "coverage to whatever that tier happens to keep.",
        )
    print(f"     ok — {len(bad_profiles)} disallowed profile use(s) in workflows.")

    # ── A3b: profile.ci exclusions are compensated ────────────────────────────
    exclusions = profile_ci_exclusions()
    print(f"\n==> A3b: profile.ci default-filter exclusions ({len(exclusions)})")
    for kind, name in exclusions:
        token = name.lstrip("~")
        if kind == "package":
            covered = crate_covered(token, blobs)
        else:
            covered = any(
                re.search(rf"(?<![\w-]){re.escape(token)}(?![\w-])", b) for b in blobs
            )
        if not covered:
            findings.fail(
                "A3b",
                f"{kind}({name})",
                "subtracted from profile.ci's default-filter and named by no CI "
                "step or CI-reached Makefile target, so nothing runs it. Route it "
                "back into the CI run, give it a dedicated step, or delete it.",
            )
        elif verbose:
            print(f"  ok  {kind}({name}) run by a CI-reached invocation")
    print(
        f"    {len(exclusions) - sum(1 for f in findings.failures if '[A3b]' in f)}/{len(exclusions)} exclusions compensated."
    )

    # ── A3c: #[ignore] only where CI runs ignored tests ───────────────────────
    ignored = crates_with_ignored_tests(members)
    print(
        f"\n==> A3c: `#[ignore]` reachability ({len(ignored)} crate(s) with ignored tests)"
    )
    for crate in sorted(ignored):
        if not ignored_tests_run_for(crate, blobs):
            files = ", ".join(sorted(set(ignored[crate])))
            findings.fail(
                "A3c",
                f"{crate}: {files}",
                "carries `#[ignore]`d tests and no CI-reached target runs that "
                "crate's ignored tests. An `#[ignore]` with no target behind it "
                "is a test that never runs and a comment that says it does; give "
                "the crate a target CI reaches, or delete the tests.",
            )
        elif verbose:
            print(f"  ok  {crate}: ignored tests run by a CI-reached target")

    # ── A3d: inline `-E` exclusions are compensated ───────────────────────────
    ci_runs = nextest_runs_in_ci()
    filtered = [
        (rel, cmd, expr) for rel, cmd in ci_runs for expr in DASH_E_RE.findall(cmd)
    ]
    print(f"\n==> A3d: inline `-E` filter exclusions ({len(filtered)})")
    for rel, command, expr in filtered:
        selectors = sorted(
            {f"{kind}({name})" for kind, name in SELECTOR_RE.findall(expr)}
        )
        witness = unfiltered_cover(ci_runs)
        if witness is None:
            findings.fail(
                "A3d",
                f"{rel}: -E '{expr}'",
                "this step filters its nextest run and no CI step runs the "
                f"workspace unfiltered, so {', '.join(selectors)} is subtracted "
                "everywhere. An `-E` is only honest when some other job runs the "
                "same scope with no filter; wire that job, or delete the tests "
                "this expression hides.",
            )
        elif verbose:
            print(f"  ok  -E '{expr}' compensated by {witness}")
    if filtered and not any("[A3d]" in f for f in findings.failures):
        print(
            f"    {len(filtered)}/{len(filtered)} inline `-E` exclusions compensated."
        )

    # ── Verdict ───────────────────────────────────────────────────────────────
    print("")
    if findings.failures:
        print("\n".join(findings.failures))
        print("")
        print(f"FAIL: {len(findings.failures)} unreached gate(s).")
        print("      Every entry above is a WIRE-OR-CUT decision, not a waiver:")
        print("      attach it to the job where it belongs, or delete it.")
        print("      This gate has no exemption list by design.")
        return 1

    print("==> Gate reachability: every gate target, workspace crate, profile.ci")
    print("    exclusion, inline `-E` filter and `#[ignore]`d crate is reached by CI.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
