"""Semantic contracts over the CI workflow graph.

Every assertion here is a rule about BEHAVIOUR read off a parsed workflow, and
every one carries a mutation that must be rejected. None of them counts lines,
occurrences, or bytes: a count passes on a semantically broken workflow and
fails on a benign reformat, which is the shape this program exists to remove.

The parser is the fail-closed YAML subset already used by
`scripts/check-gate-reachability.py` — no third-party dependency, and one
parser means one reading of the same file.

Contracts, and the wrong program each rejects:

  * Workspace cleanliness — a step that extracts a downloaded archive into the
    checkout leaves untracked paths no gate declares as an input, so the change
    router fails closed to comprehensive on every run and the selection it
    exists for never happens. That was live for the whole life of the router
    (LESSONS.md preflight-perf-discipline).
"""

import importlib.util
import os
import shutil
import subprocess
import re
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / ".github" / "workflows"
ACTIONS = ROOT / ".github" / "actions"

_spec = importlib.util.spec_from_file_location(
    "check_gate_reachability", ROOT / "scripts" / "check-gate-reachability.py"
)
_reachability = importlib.util.module_from_spec(_spec)
assert _spec.loader is not None
sys.modules.setdefault("check_gate_reachability", _reachability)
_spec.loader.exec_module(_reachability)
parse_yaml = _reachability.parse_yaml


# ── workflow model ───────────────────────────────────────────────────────────


def workflow_files() -> list[Path]:
    """Every workflow this repository ships.

    Enumerated, never listed: a workflow added tomorrow is covered by
    construction. An empty enumeration is a failure, not a pass
    (LESSONS.md enumeration-gate-floors).
    """
    found = sorted(WORKFLOWS.glob("*.yml")) + sorted(WORKFLOWS.glob("*.yaml"))
    assert found, f"no workflows found under {WORKFLOWS}; the glob is wrong"
    return found


def load(path: Path) -> dict:
    document = parse_yaml(path.read_text(encoding="utf-8"), path.name)
    assert isinstance(document, dict), f"{path.name}: workflow is not a mapping"
    return document


def jobs(document: dict) -> dict[str, dict]:
    found = document.get("jobs") or {}
    assert isinstance(found, dict), "jobs: is not a mapping"
    return {name: body for name, body in found.items() if isinstance(body, dict)}


def steps(job: dict) -> list[dict]:
    found = job.get("steps") or []
    if not isinstance(found, list):
        return []
    return [step for step in found if isinstance(step, dict)]


def run_bodies(document: dict) -> list[tuple[str, str, str]]:
    """(job, step name, run body) for every step that runs a shell command."""
    bodies = []
    for job_name, job in jobs(document).items():
        for step in steps(job):
            body = step.get("run")
            if isinstance(body, str):
                bodies.append((job_name, str(step.get("name", "<unnamed>")), body))
    return bodies


def composite_run_bodies() -> list[tuple[str, str, str]]:
    bodies = []
    for action in sorted(ACTIONS.glob("*/action.yml")):
        document = parse_yaml(action.read_text(encoding="utf-8"), action.name)
        if not isinstance(document, dict):
            continue
        runs = document.get("runs") or {}
        if not isinstance(runs, dict):
            continue
        for step in runs.get("steps") or []:
            if isinstance(step, dict) and isinstance(step.get("run"), str):
                bodies.append(
                    (
                        action.parent.name,
                        str(step.get("name", "<unnamed>")),
                        step["run"],
                    )
                )
    return bodies


# ── contract: nothing unpacks a download into the tracked working tree ───────

# The rule is stated where the router reads: a destination is acceptable when
# `git ls-files --others --exclude-standard` cannot see it. That is satisfied
# three ways — outside the repository (an absolute path or the runner's own
# scratch), a temporary directory, or a path `.gitignore` already covers. Any
# other destination is a router input nobody declared.
#
# The gate is on the extraction CALL rather than on a directory listing,
# because both tools default to the working directory when no destination is
# given: an unstated destination is the checkout, invisibly.
_TAR_EXTRACT = re.compile(r"\btar\b[^|;&\n]*\s-[A-Za-z]*x")
_TAR_DESTINATION = re.compile(r"(?:\s-C\s|--directory[= ])(\S+)")
_EXPAND_ARCHIVE = re.compile(r"\bExpand-Archive\b[^\n]*", re.IGNORECASE)
_EXPAND_DESTINATION = re.compile(r"-DestinationPath\s+(\S+)", re.IGNORECASE)
# cargo-nextest is an archive extractor too. Its `--extract-to` sits on a
# continuation line of a multi-line `cargo nextest list` invocation, so the
# destination is read from the flag itself rather than from the command line
# that opened the call.
_NEXTEST_EXTRACT = re.compile(r"--extract-to\s+(\S+)")

_SCRATCH_ROOTS = (
    "RUNNER_TEMP",
    "RUNNER_WORKSPACE",
    "TMPDIR",
    "TEMP",
    "TMP",
)
_SCRATCH = re.compile(
    r"^\$(?:\{)?(?:env:)?(?:" + "|".join(_SCRATCH_ROOTS) + r")\b"
    r"|^\$\{\{\s*runner\.temp\s*\}\}"
    r"|^mktemp\b"
)
# `sh_var=value` and PowerShell `$Var = value`, taken from the same body. A
# destination spelled as a variable is readable only by following it; refusing
# to follow it would fail every correct workflow, and accepting it blind would
# accept the defect.
_SH_ASSIGN = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*)=(.+)$")
_PS_ASSIGN = re.compile(r"^\$([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+)$")


def _assignments(body: str) -> dict[str, str]:
    found: dict[str, str] = {}
    for raw in body.splitlines():
        line = raw.strip()
        for pattern in (_SH_ASSIGN, _PS_ASSIGN):
            match = pattern.match(line)
            if match:
                found.setdefault(match.group(1), match.group(2).strip())
                break
    return found


def _resolve(destination: str, assignments: dict[str, str], depth: int = 0) -> str:
    """Substitute a variable destination with the value assigned in the body."""
    if depth > 4:
        return destination
    text = destination.strip().strip('"').strip("'")
    match = re.match(r"^\$(?:\{)?(?:env:)?([A-Za-z_][A-Za-z0-9_]*)\}?", text)
    if match and match.group(1) in assignments:
        replacement = assignments[match.group(1)]
        # `Join-Path $env:RUNNER_TEMP "wasmtime"` reads as its first argument:
        # the root decides whether the result is inside the checkout.
        joined = re.match(r"^Join-Path\s+(\S+)", replacement, re.IGNORECASE)
        if joined:
            replacement = joined.group(1)
        return _resolve(replacement + text[match.end() :], assignments, depth + 1)
    return text


def _visible_to_git(destination: str) -> bool:
    """True when the router would see an artefact written here."""
    text = destination.strip().strip('"').strip("'")
    if not text:
        return True
    if _SCRATCH.match(text) or text.startswith("$(mktemp"):
        return False
    # Absolute, or a Windows drive root: outside the checkout by construction.
    if (
        text.startswith("/")
        or text.startswith("~")
        or re.match(r"^[A-Za-z]:[\\/]", text)
    ):
        return False
    if text.startswith("$"):
        # An unresolved variable is an unanswerable question, and an
        # unanswerable question about workspace cleanliness fails closed.
        return True
    # `lstrip` takes a character SET: it turns `.ast-grep` into `ast-grep`,
    # which Git has no rule for, so an ignored destination reads as tracked.
    relative = re.sub(r"^(?:\./)+", "", text) or "."
    if relative in (".", ""):
        return True
    return (
        not subprocess.run(
            ["git", "check-ignore", "-q", "--", relative],
            cwd=ROOT,
            capture_output=True,
        ).returncode
        == 0
    )


def extraction_destinations(body: str) -> list[tuple[str, str | None]]:
    """(command, resolved destination) for every archive extraction in a body.

    `None` means the command named no destination at all, which is the same
    defect as naming the checkout — worse, because it is invisible.
    """
    assignments = _assignments(body)
    found: list[tuple[str, str | None]] = []
    for raw in body.splitlines():
        line = raw.strip()
        if line.startswith("#"):
            continue
        if _TAR_EXTRACT.search(line):
            match = _TAR_DESTINATION.search(line)
            found.append(
                (line, _resolve(match.group(1), assignments) if match else None)
            )
        for command in _EXPAND_ARCHIVE.findall(line):
            match = _EXPAND_DESTINATION.search(command)
            found.append(
                (command, _resolve(match.group(1), assignments) if match else None)
            )
        nextest = _NEXTEST_EXTRACT.search(line)
        if nextest:
            found.append((line, _resolve(nextest.group(1), assignments)))
    return found


def offending_extractions(bodies: list[tuple[str, str, str]], origin: str) -> list[str]:
    offences = []
    for job_name, step_name, body in bodies:
        for command, destination in extraction_destinations(body):
            if destination is not None and not _visible_to_git(destination):
                continue
            offences.append(
                f"{origin} {job_name} / {step_name}: {command} "
                f"(destination {destination or 'unstated'})"
            )
    return offences


def test_every_archive_extraction_names_a_destination_the_router_cannot_see() -> None:
    """Four wasmtime unpacks in `$PWD` made every run report `undeclared: …`.

    Extraction calls only: the Linux archive reaches `target/` by `mv` from
    staging, and where it lands is the prebuilt path contract's business.
    """
    offences: list[str] = []
    for path in workflow_files():
        offences.extend(offending_extractions(run_bodies(load(path)), path.name))
    offences.extend(offending_extractions(composite_run_bodies(), "action"))
    assert not offences, "archive unpacked into the checkout:\n  " + "\n  ".join(
        offences
    )


def test_the_extraction_rule_rejects_an_unpack_into_the_checkout() -> None:
    """Falsifiability: the exact defect E1 describes must be named red."""
    into_pwd = [("build-and-test", "Install wasmtime", 'tar -xJf "$X.tar.xz"\n')]
    assert offending_extractions(into_pwd, "fixture"), (
        "an extraction with no destination was accepted"
    )

    explicit_dot = [("j", "s", "Expand-Archive -Path a.zip -DestinationPath . -Force")]
    assert offending_extractions(explicit_dot, "fixture"), (
        "an extraction into '.' was accepted"
    )

    tracked_relative = [("j", "s", 'tar -xJf a.tar.xz -C "./scripts"')]
    assert offending_extractions(tracked_relative, "fixture"), (
        "an extraction into a tracked directory was accepted"
    )

    via_pwd_variable = [
        (
            "j",
            "s",
            '$Root = Join-Path $PWD "pkg-smoke"\n'
            "Expand-Archive -Path a.zip -DestinationPath $Root -Force",
        )
    ]
    assert offending_extractions(via_pwd_variable, "fixture"), (
        "an extraction into a $PWD-rooted variable was accepted"
    )

    nextest_into_checkout = [
        ("j", "s", 'root="./ci-linux-nextest"\ncargo nextest list --extract-to "$root"')
    ]
    assert offending_extractions(nextest_into_checkout, "fixture"), (
        "a nextest archive extraction into the checkout was accepted"
    )


def test_the_extraction_rule_accepts_a_destination_the_router_cannot_see() -> None:
    """And is not vacuous in the other direction: every correct spelling passes."""
    for allowed in (
        'tar -xJf "$RUNNER_TEMP/a.tar.xz" -C "$RUNNER_TEMP"',
        "Expand-Archive -Path $archive -DestinationPath $env:RUNNER_TEMP -Force",
        "tar -xzf x.tar.gz --directory ${RUNNER_TEMP}",
        "tar -xzf $asset -C C:\\",
        'staging="$(mktemp -d)"\ntar -xf a.tar.gz -C "${staging}"',
        '$dest = Join-Path $env:RUNNER_TEMP "wasmtime"\n'
        "Expand-Archive -Path $zip -DestinationPath $dest -Force",
        # `dist/` is already ignored, so the router never sees it.
        "tar -xf a.tar.gz -C dist/test-tarball",
    ):
        assert not offending_extractions([("j", "s", allowed)], "fixture"), allowed


# ── contract: cache keys follow the rustc fingerprint ────────────────────────

# The mold rustflag changes the rustc invocation, so a job that links with mold
# and a job that does not are compiling different programs as far as a cache is
# concerned. Two facts follow, and they are the whole contract:
#
#   * a job may declare the mold rustflag only if it INSTALLS mold. The flag is
#     scoped per job rather than hoisted to workflow level or .cargo/config.toml
#     precisely because `lint`, `license-check`, `docs-and-scripts` and
#     `playground-wasm-build` do not install mold and would fail to link their
#     host build scripts.
#   * the `linux-mold` cache key belongs to exactly the jobs that declare the
#     flag. release-gate and coverage-nightly shared `build-test-linux` with
#     ci.yml's mold job while running WITHOUT mold: a key shared across
#     fingerprints, which is a guaranteed-miss restore in both directions.
#
# Keys name the fingerprint, never the job, so the set is closed and small.
MOLD_RUSTFLAG = "-fuse-ld=mold"
MOLD_KEY = "linux-mold"
FINGERPRINT_KEYS = {"linux", MOLD_KEY, "windows", "macos"}


def _job_strings(job: object) -> list[str]:
    """Every string a job declares: env values, run bodies, `with` values."""
    parts: list[str] = []

    def flatten(value: object) -> None:
        if isinstance(value, dict):
            for item in value.values():
                flatten(item)
        elif isinstance(value, list):
            for item in value:
                flatten(item)
        elif isinstance(value, str):
            parts.append(value)

    flatten(job)
    return parts


def _declared_cache_keys(job: object) -> list[str]:
    """Every cache-shared-key a job passes, read off the parsed `with:` block.

    Read structurally rather than by grepping the job's text: a key spelled in
    a comment is not a key, and a key nested in a `with:` mapping is one even
    though the flattened text loses its name.
    """
    keys: list[str] = []

    def walk(value: object) -> None:
        if isinstance(value, dict):
            for name, item in value.items():
                if name == "cache-shared-key" and isinstance(item, str):
                    keys.append(item.strip())
                else:
                    walk(item)
        elif isinstance(value, list):
            for item in value:
                walk(item)

    walk(job)
    return sorted(set(keys))


def cache_key_findings(document: dict) -> list[str]:
    findings: list[str] = []
    for name, job in jobs(document).items():
        declared = _job_strings(job)
        body = "\n".join(
            line
            for value in declared
            for line in value.splitlines()
            if not line.strip().startswith("#")
        )
        declares_flag = MOLD_RUSTFLAG in body
        installs_mold = re.search(r"apt-get install[^\n]*\bmold\b", body) is not None
        keys = _declared_cache_keys(job)

        if declares_flag and not installs_mold:
            findings.append(
                f"{name}: declares {MOLD_RUSTFLAG} without installing mold; "
                "every link in the job would fail"
            )
        if installs_mold and not declares_flag:
            findings.append(
                f"{name}: installs mold but never uses it; the install is dead cost"
            )
        for key in keys:
            if key not in FINGERPRINT_KEYS:
                findings.append(
                    f"{name}: cache key '{key}' is not one of "
                    f"{sorted(FINGERPRINT_KEYS)}; keys name the rustc "
                    "fingerprint, not the job"
                )
            elif key == MOLD_KEY and not declares_flag:
                findings.append(
                    f"{name}: uses the '{MOLD_KEY}' cache key without declaring "
                    f"{MOLD_RUSTFLAG}; that key restores from a different "
                    "fingerprint and can never hit"
                )
            elif key != MOLD_KEY and declares_flag:
                findings.append(
                    f"{name}: declares {MOLD_RUSTFLAG} but caches under '{key}'; "
                    f"its artefacts belong under '{MOLD_KEY}'"
                )
    return findings


def test_the_cache_key_enumeration_is_not_empty() -> None:
    """A rule that finds no keys to judge is a rule that judges nothing.

    LESSONS.md enumeration-gate-floors: a floor, not an asserted count.
    """
    seen: set[str] = set()
    for path in workflow_files():
        for job in jobs(load(path)).values():
            seen.update(_declared_cache_keys(job))
    assert seen, "no cache-shared-key declarations found; the rule is vacuous"
    assert seen <= FINGERPRINT_KEYS, sorted(seen - FINGERPRINT_KEYS)


def test_cache_keys_follow_the_rustc_fingerprint() -> None:
    findings: list[str] = []
    for path in workflow_files():
        for finding in cache_key_findings(load(path)):
            findings.append(f"{path.name} {finding}")
    assert not findings, "cache key / link-flag mismatch:\n  " + "\n  ".join(findings)


def _fixture(**job_bodies: str) -> dict:
    return parse_yaml(
        "on: push\njobs:\n"
        + "".join(
            f"  {name}:\n    runs-on: ubuntu-24.04\n{body}"
            for name, body in job_bodies.items()
        ),
        "fixture",
    )


def test_the_cache_key_rule_rejects_each_mismatch() -> None:
    """Falsifiability: mutate the invariant in every direction it can break."""
    flag_env = (
        "    env:\n"
        '      CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUSTFLAGS: "-C link-arg=-fuse-ld=mold"\n'
    )
    install = (
        "    steps:\n"
        "      - run: sudo apt-get install -y -qq mold\n"
        "      - uses: ./.github/actions/setup-rust-build\n"
    )

    # 1. flag without the install: every link in the job fails.
    findings = cache_key_findings(
        _fixture(
            j=flag_env
            + "    steps:\n"
            + "      - uses: ./.github/actions/setup-rust-build\n"
            + "        with:\n"
            + "          cache-shared-key: linux-mold\n"
        )
    )
    assert any("without installing mold" in f for f in findings), findings

    # 2. mold key without the flag: restores from another fingerprint, never hits.
    findings = cache_key_findings(
        _fixture(
            j="    steps:\n"
            "      - uses: ./.github/actions/setup-rust-build\n"
            "        with:\n"
            "          cache-shared-key: linux-mold\n"
        )
    )
    assert any("without declaring" in f for f in findings), findings

    # 3. flag with a non-mold key: writes mold artefacts into the shared layer.
    findings = cache_key_findings(
        _fixture(
            j=flag_env + install + "        with:\n          cache-shared-key: linux\n"
        )
    )
    assert any("belong under" in f for f in findings), findings

    # 4. a job-shaped key: the eight-way split this replaces.
    findings = cache_key_findings(
        _fixture(
            j="    steps:\n"
            "      - uses: ./.github/actions/setup-rust-build\n"
            "        with:\n"
            "          cache-shared-key: build-test-linux\n"
        )
    )
    assert any("not one of" in f for f in findings), findings

    # 5. an install nobody uses: dead provisioning cost.
    findings = cache_key_findings(
        _fixture(j=install + "        with:\n          cache-shared-key: linux\n")
    )
    assert any("never uses it" in f for f in findings), findings


def test_the_cache_key_rule_accepts_both_correct_shapes() -> None:
    """Not vacuous: a mold job and a plain job must both pass."""
    mold_job = (
        "    env:\n"
        '      CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUSTFLAGS: "-C link-arg=-fuse-ld=mold"\n'
        "    steps:\n"
        "      - run: sudo apt-get install -y -qq mold\n"
        "      - uses: ./.github/actions/setup-rust-build\n"
        "        with:\n"
        "          cache-shared-key: linux-mold\n"
    )
    plain_job = (
        "    steps:\n"
        "      - uses: ./.github/actions/setup-rust-build\n"
        "        with:\n"
        "          cache-shared-key: linux\n"
    )
    assert not cache_key_findings(_fixture(mold=mold_job, plain=plain_job))


# ── contract: pull requests read the cache and do not write it ───────────────


def test_pull_requests_restore_the_cache_but_never_save_it() -> None:
    """One open pull request held 10.47 GB of a 10 GB repo budget.

    Every PR job wrote entries scoped to its own ref -- readable by no other
    branch -- and evicted main's, which retained 0.01 GB. So every PR restored
    from a cold layer it had just displaced. Saving is now the default
    branch's job alone; restoring still works everywhere, because a
    main-saved entry is readable from any branch.
    """
    action = ACTIONS / "setup-rust-build" / "action.yml"
    document = parse_yaml(action.read_text(encoding="utf-8"), action.name)
    steps = document["runs"]["steps"]

    swatinem = [
        step
        for step in steps
        if isinstance(step, dict) and "Swatinem/rust-cache@" in str(step.get("uses"))
    ]
    assert len(swatinem) == 1, "one dependency-cache layer, one save policy"
    save_if = str(swatinem[0].get("with", {}).get("save-if", ""))
    assert save_if == "${{ github.ref == 'refs/heads/main' }}", save_if

    # sccache carries the same policy, but through its MODE rather than its
    # presence. "Restore but do not write" used to be inexpressible for the
    # GHA backend, so every sccache step was branch-gated and a pull request
    # got no compilation cache at all. sccache 0.16.0 added
    # SCCACHE_GHA_RW_MODE, so the steps now run on every ref and only the
    # default branch is READ_WRITE. Re-gating them on the branch would restore
    # the cold-PR behaviour this replaced, so it is rejected here.
    guard = "github.ref == 'refs/heads/main'"
    sccache_steps = [
        step
        for step in steps
        if isinstance(step, dict)
        and (
            "sccache-action@" in str(step.get("uses"))
            or "sccache" in str(step.get("name", "")).lower()
            or "SCCACHE_GHA_ENABLED" in str(step.get("run", ""))
        )
    ]
    assert sccache_steps, "no sccache steps found; the guard would be vacuous"
    gated = [
        str(step.get("name") or step.get("id") or step.get("uses"))
        for step in sccache_steps
        if guard in str(step.get("if", ""))
    ]
    assert not gated, (
        f"sccache steps are branch-gated again: {gated}; a pull request would "
        "install nothing and read nothing"
    )

    exports = [
        step
        for step in sccache_steps
        if "SCCACHE_GHA_RW_MODE" in str(step.get("run", ""))
    ]
    assert len(exports) == 1, "one step decides the sccache read/write mode"
    mode = str(exports[0].get("env", {}).get("SCCACHE_RW_MODE", ""))
    assert mode == (
        "${{ github.ref == 'refs/heads/main' && 'READ_WRITE' || 'READ_ONLY' }}"
    ), mode


# ── contract: one job builds ast-grep; the rest read what it built ───────────


def test_one_job_builds_the_ast_grep_toolchain_and_only_main_saves_its_cache() -> None:
    """Run 33028214259 paid five cold installs of one byte-identical tree."""
    artifact = "ast-grep-toolchain-${{ github.sha }}"

    def transfers(job, verb):
        return any(
            f"{verb}-artifact@" in str(s.get("uses"))
            and str(s.get("with", {}).get("name")) == artifact
            for s in steps(job)
        )

    ci = jobs(load(WORKFLOWS / "ci.yml"))
    assert [n for n, j in ci.items() if transfers(j, "upload")] == [
        "ast-grep-toolchain"
    ], "the ast-grep toolchain has more than one producer"
    for name, job in ci.items():
        needs = job.get("needs")
        assert not transfers(job, "download") or "ast-grep-toolchain" in (
            [needs] if isinstance(needs, str) else list(needs or [])
        ), f"{name} downloads the toolchain without depending on its producer"

    action = ACTIONS / "setup-ast-grep" / "action.yml"
    cache = [
        f"{str(s.get('uses', '')).split('@')[0]} {s.get('if', '')}"
        for s in parse_yaml(action.read_text(encoding="utf-8"), action.name)["runs"][
            "steps"
        ]
        if "actions/cache" in str(s.get("uses"))
    ]
    assert len(cache) == 2 and cache[0].startswith("actions/cache/restore"), (
        f"a combined cache step saves from every ref and races itself: {cache}"
    )
    assert "github.ref == 'refs/heads/main'" in cache[1], cache[1]


# ── contract: every scheduled workflow reports to an owner ───────────────────

REPORTER = ".github/workflows/scheduled-failure-report.yml"
OWNER_TABLE = ROOT / ".github" / "nightly-owners.yml"


def scheduled_workflows() -> list[Path]:
    """Every workflow with an `on: schedule:` trigger.

    Enumerated from the triggers, never listed: a scheduled workflow added
    tomorrow inherits the requirement. An empty enumeration is a failure --
    a reporter contract that finds nothing to check is a contract that checks
    nothing (LESSONS.md enumeration-gate-floors).
    """
    found = []
    for path in workflow_files():
        triggers = load(path).get("on") or load(path).get(True) or {}
        if isinstance(triggers, dict) and "schedule" in triggers:
            found.append(path)
    assert found, "no scheduled workflows found; the reporter contract is vacuous"
    return found


def reporter_findings(document: dict, name: str) -> list[str]:
    """Every way a reporter job can look wired and report nothing."""
    findings: list[str] = []
    all_jobs = jobs(document)
    callers = {
        job_name: job
        for job_name, job in all_jobs.items()
        if REPORTER in str(job.get("uses", ""))
    }
    if len(callers) != 1:
        return [
            f"{name}: expected exactly one job calling the reporter, found "
            f"{sorted(callers) or 'none'}"
        ]
    job_name, job = next(iter(callers.items()))

    needs = job.get("needs") or []
    needs = [needs] if isinstance(needs, str) else list(needs)
    expected = set(all_jobs) - {job_name}
    if set(needs) != expected:
        findings.append(
            f"{name}: reporter `needs:` is {sorted(needs)} but the workflow's "
            f"other jobs are {sorted(expected)}; a job outside `needs:` never "
            "reaches the report"
        )

    condition = str(job.get("if", ""))
    if "always()" not in condition:
        findings.append(
            f"{name}: reporter `if:` lacks always(); a red upstream would skip "
            "it at exactly the moment it is needed"
        )
    if "schedule" not in condition or "workflow_dispatch" not in condition:
        findings.append(
            f"{name}: reporter `if:` must be guarded to schedule/dispatch; a "
            "branch run of a scheduled workflow must not file issues"
        )

    permissions = job.get("permissions") or {}
    if not isinstance(permissions, dict) or permissions.get("issues") != "write":
        findings.append(
            f"{name}: the CALLER must grant `issues: write`. GitHub intersects "
            "a called workflow's permissions with the caller's token scope, so "
            "granting it only inside the reusable workflow 403s on every call"
        )

    outcome = str((job.get("with") or {}).get("outcome", ""))
    for state in ("failure", "cancelled", "timed_out"):
        if state not in outcome:
            findings.append(
                f"{name}: the aggregate outcome ignores `{state}`, so that "
                "state would be reported as a green run"
            )
    return findings


def callee_permission_findings(document: dict) -> list[str]:
    """The other half of the intersection, read off the reusable workflow.

    A caller that grants `issues: write` to a callee declaring only
    `contents: read` produces an effective token WITHOUT `issues` — the
    reporter runs, looks wired, and 403s on the first write. Neither side is
    sufficient alone, so neither side is the contract alone.
    """
    findings: list[str] = []
    permissions = document.get("permissions")
    if not isinstance(permissions, dict):
        return [
            "the reusable reporter declares no `permissions:` block; the "
            "intersection would be whatever a caller happened to grant"
        ]
    if permissions.get("issues") != "write":
        findings.append(
            "the reusable reporter does not declare `issues: write`; GitHub "
            "intersects it with the caller's grant, so the effective token "
            "cannot write an issue however the caller is configured"
        )
    if permissions.get("contents") != "read":
        findings.append(
            "the reusable reporter must keep `contents: read` to check out the "
            "owner table and the reporter script"
        )
    return findings


def test_the_reusable_reporter_keeps_issues_write_on_its_own_side() -> None:
    document = load(WORKFLOWS / "scheduled-failure-report.yml")
    findings = callee_permission_findings(document)
    assert not findings, "reusable reporter permissions:\n  " + "\n  ".join(findings)


def test_the_permission_intersection_is_rejected_from_either_side() -> None:
    """Falsifiability: mutate each half independently.

    Both mutations produce a reporter that runs to completion and writes
    nothing, which is why the pair has to be asserted rather than either half.
    """
    callee_without = parse_yaml(
        "on:\n  workflow_call:\njobs:\n"
        "  report:\n    runs-on: ubuntu-24.04\n    steps:\n      - run: 'true'\n",
        "fixture",
    )
    callee_without["permissions"] = {"contents": "read"}
    assert any(
        "does not declare `issues: write`" in finding
        for finding in callee_permission_findings(callee_without)
    ), callee_permission_findings(callee_without)

    callee_none = parse_yaml(
        "on:\n  workflow_call:\njobs:\n"
        "  report:\n    runs-on: ubuntu-24.04\n    steps:\n      - run: 'true'\n",
        "fixture",
    )
    assert any(
        "declares no `permissions:` block" in finding
        for finding in callee_permission_findings(callee_none)
    ), callee_permission_findings(callee_none)

    caller_without = _reporter_fixture(permissions="contents: read")
    assert any(
        "the CALLER must grant" in finding
        for finding in reporter_findings(caller_without, "fixture")
    ), reporter_findings(caller_without, "fixture")


def test_every_scheduled_workflow_reports_to_an_owner() -> None:
    findings: list[str] = []
    for path in scheduled_workflows():
        findings.extend(reporter_findings(load(path), path.name))
    assert not findings, "scheduled reporter wiring:\n  " + "\n  ".join(findings)


def _reporter_fixture(**overrides: str) -> dict:
    fields = {
        "needs": "[alpha, beta]",
        "if": "always() && (github.event_name == 'schedule' "
        "|| github.event_name == 'workflow_dispatch')",
        "permissions": "issues: write",
        "outcome": "failure-cancelled-timed_out",
    }
    fields.update(overrides)
    return parse_yaml(
        "on:\n  schedule:\n    - cron: '0 7 * * *'\njobs:\n"
        "  alpha:\n    runs-on: ubuntu-24.04\n    steps:\n      - run: 'true'\n"
        "  beta:\n    runs-on: ubuntu-24.04\n    steps:\n      - run: 'true'\n"
        "  report:\n"
        f"    needs: {fields['needs']}\n"
        f"    if: {fields['if']}\n"
        "    permissions:\n"
        f"      {fields['permissions']}\n"
        f"    uses: {REPORTER}\n"
        "    with:\n"
        f"      outcome: {fields['outcome']}\n",
        "fixture",
    )


def test_the_reporter_contract_rejects_each_broken_clause() -> None:
    """Falsifiability: mutate every clause the wiring depends on."""
    assert not reporter_findings(_reporter_fixture(), "fixture"), reporter_findings(
        _reporter_fixture(), "fixture"
    )

    cases = {
        "never reaches the report": _reporter_fixture(needs="[alpha]"),
        "lacks always()": _reporter_fixture(
            **{
                "if": "github.event_name == 'schedule' "
                "|| github.event_name == 'workflow_dispatch'"
            }
        ),
        "must not file issues": _reporter_fixture(**{"if": "always()"}),
        "must grant": _reporter_fixture(permissions="contents: read"),
        "ignores `cancelled`": _reporter_fixture(outcome="failure-timed_out"),
    }
    for expected, document in cases.items():
        findings = reporter_findings(document, "fixture")
        assert any(expected in finding for finding in findings), (expected, findings)

    # A scheduled workflow with no reporter at all is the state this replaces.
    none = parse_yaml(
        "on:\n  schedule:\n    - cron: '0 7 * * *'\njobs:\n"
        "  alpha:\n    runs-on: ubuntu-24.04\n    steps:\n      - run: 'true'\n",
        "fixture",
    )
    assert any(
        "exactly one job calling the reporter" in finding
        for finding in reporter_findings(none, "fixture")
    )


def test_the_owner_table_matches_the_scheduled_workflows_exactly() -> None:
    """Validated against the workflows, not maintained beside them.

    Both directions are errors: a scheduled workflow with no owner files
    unowned issues, and an owner entry for a workflow that no longer schedules
    is a freshness window nothing can ever satisfy.
    """
    spec = importlib.util.spec_from_file_location(
        "scheduled_failure_report", ROOT / "scripts" / "scheduled-failure-report.py"
    )
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    owners = module.parse_owners(OWNER_TABLE.read_text(encoding="utf-8"))

    scheduled = {path.name for path in scheduled_workflows()}
    assert set(owners) == scheduled, {
        "unowned": sorted(scheduled - set(owners)),
        "orphaned": sorted(set(owners) - scheduled),
    }
    for name, entry in owners.items():
        assert entry["owner"], name
        assert int(entry["freshness_hours"]) >= 24, (name, entry)


# ── contract: the freshness gate is durably required and correctly scoped ────


def test_the_freshness_job_is_standalone_scoped_and_advisory_for_now() -> None:
    """Every clause here is load-bearing, and each was wrong in a draft.

    `needs:` must be absent, or a failed or stranded upstream job would skip
    the very check that says the scheduled tier stopped producing verdicts.
    `actions: read` must be present, or the run-history read 404s and the
    script reports an auth defect as rot. Both token spellings must be
    exported, because the script accepts either and neither is set by default.

    The check is ADVISORY today, and while advisory the assertion STEP (never
    the job, never the script) carries `continue-on-error: true`: an advisory
    check that turns a PR's check suite red for a nightly no author can fix is
    the theatre this repository does not want, but the tolerance is scoped to
    that one step so a future step added to this job is never tolerated by
    accident, and the script's own exit code is untouched — it still fails
    closed for a stale nightly, an auth/scope defect, or a malformed response.
    Nothing about the SCRIPT is softened: no flag, no bypass, no grace window,
    no permissive fallback. Once activated (wired into `linux-required`), the
    tolerance must be gone: a required check that is allowed to fail silently
    is worse than no check. The ACTIVATION note in the workflow spells out
    exactly what removes it.
    """
    ci = load(WORKFLOWS / "ci.yml")
    all_jobs = jobs(ci)
    assert "nightly-freshness" in all_jobs, sorted(all_jobs)
    job = all_jobs["nightly-freshness"]

    assert not job.get("needs"), (
        "nightly-freshness must have no `needs:`; hanging it off another job "
        "means a stranded upstream skips it exactly when it matters"
    )
    # A job-level `if:` would make the check skippable on some diffs. Nightly
    # rot is not a property of anyone's change.
    assert not job.get("if"), job.get("if")
    permissions = job.get("permissions") or {}
    assert permissions.get("actions") == "read", permissions
    assert permissions.get("contents") == "read", permissions

    runner = next(
        step
        for step in steps(job)
        if "check-nightly-freshness.py" in str(step.get("run", ""))
    )
    env = runner.get("env") or {}
    assert env.get("GH_TOKEN") and env.get("GITHUB_TOKEN"), env
    # The tolerance, if any, must never widen to the whole job: a job-level
    # `continue-on-error` would tolerate a checkout failure or a future step
    # too, not just the one assertion this comment is about.
    assert not job.get("continue-on-error"), job

    required = all_jobs["linux-required"]
    needs = required.get("needs") or []
    needs = [needs] if isinstance(needs, str) else list(needs)
    assertion = "\n".join(str(step.get("run", "")) for step in steps(required))

    if "nightly-freshness" in needs:
        # Activated. Both halves must land together: a `needs:` entry without
        # the assertion makes the aggregate WAIT for the check and then ignore
        # its verdict, which is worse than not requiring it at all.
        assert "NIGHTLY_FRESHNESS_RESULT" in assertion, assertion
        assert 'test "$NIGHTLY_FRESHNESS_RESULT" = success' in assertion, assertion
        # A required check that is still allowed to fail silently is worse
        # than an advisory one: activation must remove the tolerance in the
        # same commit that wires the `needs:` edge.
        assert not runner.get("continue-on-error"), (
            "nightly-freshness is wired into linux-required but its "
            "assertion step still tolerates failure; activation must remove "
            "continue-on-error in the same commit"
        )
    else:
        assert "NIGHTLY_FRESHNESS_RESULT" not in assertion, (
            "linux-required asserts a freshness result it does not depend on; "
            "that expression evaluates to the empty string, so the aggregate "
            "would fail for a reason unrelated to nightly health"
        )
        workflow_text = (WORKFLOWS / "ci.yml").read_text(encoding="utf-8")
        assert "ACTIVATION, exactly" in workflow_text, (
            "a deferred required-check edge must carry the exact steps that "
            "undo it, or it becomes permanent by being forgotten"
        )
        # Advisory means "cannot turn the PR's check suite red", never
        # "cannot fail": the step still posts its own `::error::` annotation,
        # and only the JOB'S conclusion is tolerated, at the narrowest scope
        # GitHub Actions offers — the one step, not the job.
        assert runner.get("continue-on-error") is True, (
            "the advisory nightly-freshness assertion step must carry "
            "continue-on-error: true, or a nightly no author can fix turns "
            "every PR's check suite red"
        )

    # A job-level `if:` would report the required context as skipped rather
    # than satisfied (LESSONS.md ci-required-gate-sequencing clause 2).
    assert required.get("if") == "always()", required.get("if")


# ── contract: entry jobs run without any upstream gate ───────────────────────


def test_changes_and_license_check_have_no_prerequisite_job() -> None:
    """`changes` and `license-check` are entry points into the graph.

    Both start a pull request or push run directly, from `on:` triggers
    alone. Neither may declare a `needs:` naming another job in this
    workflow: a prerequisite here would make every job downstream of
    `changes` — which is effectively the whole workflow — wait on that
    prerequisite's own runner and verdict before a single line of the
    branch's own diff is examined.
    """
    ci = load(WORKFLOWS / "ci.yml")
    all_jobs = jobs(ci)
    for name in ("changes", "license-check"):
        assert name in all_jobs, sorted(all_jobs)
        assert not all_jobs[name].get("needs"), (
            f"{name} must have no needs: it is an entry point, not a follower of one"
        )


def _nightly_freshness_sparse_checkout_paths() -> list[str]:
    ci = load(WORKFLOWS / "ci.yml")
    job = jobs(ci)["nightly-freshness"]
    checkout = next(
        step for step in steps(job) if "checkout" in str(step.get("uses", ""))
    )
    raw = (checkout.get("with") or {}).get("sparse-checkout") or ""
    paths = [line.strip() for line in raw.splitlines() if line.strip()]
    assert paths, "nightly-freshness checkout step declares no sparse-checkout paths"
    return paths


def test_the_freshness_jobs_sparse_checkout_resolves_its_own_import_closure() -> None:
    """The declared checkout closure must actually satisfy the entrypoint's imports.

    `check-nightly-freshness.py` dynamically loads `check-gate-reachability.py`
    (`importlib.util`, not a static `import`), whose own module body inserts
    `scripts/lib` onto `sys.path` and does a bare `import gate_inputs` — a
    dependency invisible to any tool that only greps for `^import`/`^from`.
    Hosted run 33018584707 proved the failure mode directly: the sparse
    checkout omitted `scripts/lib/gate_inputs.py`, so the job died on
    `ModuleNotFoundError` before it ever reached the freshness verdict it
    exists to report — the worst kind of red, one that teaches nothing about
    nightly health.

    This reproduces the checkout exactly (only the declared paths, nothing
    more) in an isolated directory and actually RUNS the entrypoint, rather
    than statically walking its import graph — a walk needs its own special
    case for every dynamic-load pattern this codebase uses and silently stops
    catching the next one. Running it is the check that cannot go stale, and
    it fails again the moment any required file is dropped from the list
    above, whether that file is `gate_inputs.py` or something added later.
    """
    paths = _nightly_freshness_sparse_checkout_paths()

    with tempfile.TemporaryDirectory() as raw_tmp:
        tmp = Path(raw_tmp)
        for rel in paths:
            source = ROOT / rel
            if source.is_dir():
                shutil.copytree(source, tmp / rel, dirs_exist_ok=True)
                continue
            assert source.is_file(), (
                f"declared sparse-checkout path missing on disk: {rel}"
            )
            destination = tmp / rel
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, destination)

        entrypoint = tmp / "scripts" / "check-nightly-freshness.py"
        assert entrypoint.is_file(), (
            "sparse-checkout does not include its own entrypoint"
        )

        # No GITHUB_* in the environment: the entrypoint must reach its own
        # auth precondition deterministically, regardless of what is set in
        # whatever environment happens to run this test.
        result = subprocess.run(
            [sys.executable, str(entrypoint)],
            cwd=tmp,
            env={"PATH": os.environ.get("PATH", "")},
            capture_output=True,
            text=True,
            timeout=30,
        )

    stderr = result.stderr
    assert "ModuleNotFoundError" not in stderr and "ImportError" not in stderr, (
        "the declared sparse-checkout paths do not satisfy the entrypoint's "
        "own import closure (missing a scripts/lib dependency it dynamically "
        f"loads):\n{stderr}"
    )
    assert "Traceback" not in stderr, (
        f"the entrypoint crashed inside the checked-out closure:\n{stderr}"
    )
    # Past every import, the first thing main() can fail on with no GITHUB_*
    # environment is its own auth precondition — proof this ran the real
    # script to the real business logic, not a stub that never imports it.
    assert "GITHUB_REPOSITORY is unset" in stderr, (
        f"expected the entrypoint to reach its own auth precondition; got:\n{stderr}"
    )
    assert result.returncode == 1, result.returncode


# ── contract: the platform aggregator cannot be satisfied by a skip ──────────


def _aggregator_body(job_name: str, needle: str) -> str:
    ci = load(WORKFLOWS / "ci.yml")
    job = jobs(ci)[job_name]
    bodies = [
        str(step["run"])
        for step in steps(job)
        if isinstance(step.get("run"), str) and needle in str(step["run"])
    ]
    assert len(bodies) == 1, (job_name, len(bodies))
    return bodies[0]


def _run_aggregator(body: str, env: dict[str, str]) -> int:
    return subprocess.run(
        ["bash", "-c", body],
        env={"PATH": "/usr/bin:/bin", "GITHUB_STEP_SUMMARY": "/dev/null", **env},
        capture_output=True,
        text=True,
    ).returncode


def test_the_platform_aggregator_reports_rather_than_skips() -> None:
    """`Platform gates` must be a verdict, never a satisfied absence.

    `Build & test (Windows)` and `Build & test (macOS arm64)` are directly
    required today, which is what makes the platform wrong-green reachable: a
    job whose steps all skip still reports its context as satisfied. An
    aggregate that runs `if: always()` and asserts `needs.*.result` cannot be.

    Driven by executing the aggregator's own shell body against stubbed job
    results, not by reading its text.
    """
    ci = load(WORKFLOWS / "ci.yml")
    job = jobs(ci)["platform-required"]
    assert job.get("if") == "always()", job.get("if")
    needs = job.get("needs") or []
    assert set(needs) == {
        "changes",
        "build-and-test-windows",
        "build-and-test-macos",
    }, needs

    body = _aggregator_body("platform-required", "PLATFORM_TIER")
    cases = [
        (("success", "success", "success", "smoke"), 0, "both green at smoke"),
        (("success", "success", "success", "full"), 0, "both green at full"),
        (("success", "skipped", "skipped", "none"), 0, "docs-only: skips are right"),
        (
            ("success", "skipped", "success", "smoke"),
            1,
            "a skip at smoke is not a pass",
        ),
        (("success", "success", "skipped", "full"), 1, "a skip at full is not a pass"),
        (("success", "failure", "success", "smoke"), 1, "a red platform is red"),
        (("success", "cancelled", "success", "full"), 1, "a cancel is not a pass"),
        (("success", "success", "success", ""), 1, "an unusable tier is red"),
        (("failure", "skipped", "skipped", ""), 1, "stranded routing is red"),
    ]
    for (change, windows, macos, tier), expected, why in cases:
        code = _run_aggregator(
            body,
            {
                "CHANGE_RESULT": change,
                "WINDOWS_RESULT": windows,
                "MACOS_RESULT": macos,
                "PLATFORM_TIER": tier,
            },
        )
        assert (code == 0) == (expected == 0), (why, code, expected)


def test_no_workflow_still_reads_the_deleted_platform_outputs() -> None:
    """A dangling `needs.*.outputs.X` evaluates to the empty string.

    It does not fail. `linux-required` read `platform_smoke`, so deleting that
    output without sweeping its consumers would have left the required Linux
    aggregate permanently on its no-op branch -- green, forever, having
    asserted nothing.
    """
    dangling: list[str] = []
    for path in workflow_files():
        for number, line in enumerate(path.read_text().splitlines(), start=1):
            if line.lstrip().startswith("#"):
                continue
            for name in ("platform_smoke", "platform_full"):
                if f"outputs.{name}" in line:
                    dangling.append(f"{path.name}:{number}: {line.strip()}")
    assert not dangling, "reads a deleted output:\n  " + "\n  ".join(dangling)


def test_the_platform_profile_keeps_the_macos_oracles_off_hosted_ci() -> None:
    """The exclusion is structural now, not a filter flag nobody reads.

    The macOS leak oracles are a deliberate pre-release local-macOS authority.
    The old step spelled that as `-E 'not binary(~oracle)'`; the platform
    profile simply does not select any oracle binary, and this asserts it, so
    the policy cannot be undone by an edit to a filter expression.
    """
    config = (ROOT / ".config" / "nextest.toml").read_text(encoding="utf-8")
    start = config.index("[profile.platform]")
    end = config.index("[profile.smoke]")
    selectors = re.findall(r"binary_id\(([^)]+)\)", config[start:end])
    assert selectors, "the platform profile selects nothing"
    offenders = [name for name in selectors if "oracle" in name]
    assert not offenders, offenders


# ── contract: strict ratchet policy reaches the tiers that claim it ──────────

# `RATCHET_STRICT` is a BOOLEAN the Makefile maps to `--strict-passes`. It has
# to arrive by ENVIRONMENT rather than on the command line, because
# test_freebsd_workflow_contract.py asserts the exact `gmake` command tuples
# the FreeBSD release leg runs, and a policy knob spliced into one of those
# commands makes that assertion unmatchable.
#
# The FreeBSD leg is the case a text search gets wrong: its gates execute
# inside a `vmactions/freebsd-vm` guest, so the host job's `env:` does not
# reach them. It needs its own export, BEFORE the gate, inside the same script.
RATCHET_GATES = ("test-hew-ratchet", "test-stdlib-ratchet")


def strict_ratchet_findings(
    text: str, origin: str, *, from_step_env: bool = False
) -> list[str]:
    """Every ratchet invocation in a release script, and whether strict reached it.

    `from_step_env` says the step's own `env:` already carries the knob. That
    is true for an ordinary `run:` on the host and FALSE for a script handed to
    a VM action: `vmactions/freebsd-vm` executes its payload inside the guest,
    where the host step's environment does not exist. The FreeBSD leg
    therefore needs its own export INSIDE the script, before the gate -- which
    is the case a text search for "RATCHET_STRICT appears somewhere in this
    file" gets wrong.
    """
    findings: list[str] = []
    exported = from_step_env
    for number, raw in enumerate(text.splitlines(), start=1):
        line = raw.strip()
        if line.startswith("#"):
            continue
        if re.match(r"^export\s+RATCHET_STRICT=\S+", line):
            exported = True
            continue
        for gate in RATCHET_GATES:
            if re.search(rf"\b(?:g?make)\s+{re.escape(gate)}\b", line):
                if "RATCHET_STRICT" in line:
                    findings.append(
                        f"{origin}:{number}: {gate} carries RATCHET_STRICT on its "
                        "command line; the workflow contract asserts this exact "
                        "command tuple, so the knob must arrive by environment"
                    )
                elif not exported:
                    findings.append(
                        f"{origin}:{number}: {gate} runs without RATCHET_STRICT in "
                        "scope; the release tier would silently use the "
                        "pull-request policy"
                    )
    return findings


def test_the_freebsd_release_guest_exports_strict_ratchet_before_its_gates() -> None:
    """The claim and the code must agree.

    A previous revision of this branch described this export in its commit
    message and did not ship it, which is exactly the failure a contract test
    exists to make impossible: the FreeBSD release leg would have run the
    pull-request ratchet policy while the history said otherwise.
    """
    document = load(WORKFLOWS / "release-gate.yml")
    findings: list[str] = []
    for job_name, job in jobs(document).items():
        for step in steps(job):
            step_env = step.get("env") or {}
            in_env = isinstance(step_env, dict) and "RATCHET_STRICT" in step_env
            origin = f"release-gate.yml {job_name}"

            host = step.get("run")
            if isinstance(host, str):
                findings.extend(
                    strict_ratchet_findings(host, origin, from_step_env=in_env)
                )

            # A payload handed to an action runs wherever that action puts it.
            # For the FreeBSD VM that is a guest with its own environment, so
            # the host step's `env:` confers nothing on it.
            guest = step.get("with")
            if isinstance(guest, dict):
                for value in guest.values():
                    if isinstance(value, str):
                        findings.extend(
                            strict_ratchet_findings(
                                value, f"{origin} (guest)", from_step_env=False
                            )
                        )
    assert not findings, "strict ratchet policy:\n  " + "\n  ".join(findings)


def test_the_strict_ratchet_rule_rejects_a_gate_the_export_never_reached() -> None:
    """Falsifiability, in both the ways this can be got wrong."""
    missing = "cargo build\ngmake test-vertical-slice\ngmake test-hew-ratchet\n"
    assert any(
        "without RATCHET_STRICT" in finding
        for finding in strict_ratchet_findings(missing, "fixture")
    ), strict_ratchet_findings(missing, "fixture")

    after = "gmake test-hew-ratchet\nexport RATCHET_STRICT=1\n"
    assert any(
        "without RATCHET_STRICT" in finding
        for finding in strict_ratchet_findings(after, "fixture")
    ), "an export AFTER the gate was accepted"

    on_argv = "make test-hew-ratchet RATCHET_STRICT=1\n"
    assert any(
        "on its command line" in finding
        for finding in strict_ratchet_findings(on_argv, "fixture")
    ), strict_ratchet_findings(on_argv, "fixture")

    correct = (
        "export RATCHET_STRICT=1\ngmake test-vertical-slice\ngmake test-hew-ratchet\n"
    )
    assert not strict_ratchet_findings(correct, "fixture")

    # A host step may carry the knob in `env:` instead of exporting it inline.
    assert not strict_ratchet_findings(
        "make test-hew-ratchet\n", "fixture", from_step_env=True
    )
    # A step `env:` confers nothing on a VM guest, so the same script handed to
    # the FreeBSD action must still be rejected.
    assert any(
        "without RATCHET_STRICT in scope" in finding
        for finding in strict_ratchet_findings(
            "gmake test-hew-ratchet\n", "fixture", from_step_env=False
        )
    )


def test_the_strict_ratchet_knob_is_a_boolean_not_flag_text() -> None:
    """A variable spliced into a gate's argv is a bypass surface.

    As flag text, `RATCHET_STRICT` could pass any argument to the ratchet from
    the environment. As a boolean mapped by the Makefile, it can only turn one
    documented policy on.
    """
    makefile = (ROOT / "Makefile").read_text(encoding="utf-8")
    assert "RATCHET_STRICT_FLAG = $(if $(RATCHET_STRICT),--strict-passes,)" in makefile
    invocations = [
        line
        for line in makefile.splitlines()
        if "corpus-ratchet.sh" in line and not line.lstrip().startswith("#")
    ]
    assert invocations, makefile[:0]
    for line in invocations:
        assert "$(RATCHET_STRICT)" not in line, (
            f"a raw RATCHET_STRICT is spliced into a ratchet command line: {line}"
        )


# ── contract: one gate, one home, per pull request ───────────────────────────


def test_the_lint_owned_gate_list_matches_the_lint_job_exactly() -> None:
    """The de-duplication list is derived, not remembered.

    `lint` runs its authority gates with no `if:` guard, so the router must
    not select them into a Linux shard as well -- `structural-lint` cost 242s
    in a shard and 246s in `lint` for the same change. The dispatcher skips
    them when a caller declares `LINT_GATE_OWNER=lint`.

    Both directions of drift are defects, and only one of them is loud:
    a gate added to `lint` but missing from the list keeps running twice,
    while a gate REMOVED from `lint` but left in the list stops running on the
    pull-request path altogether. Equality is the only assertion that catches
    the second.
    """
    reachability_lint_targets = set()
    workflow_text = (WORKFLOWS / "ci.yml").read_text(encoding="utf-8")
    for _name, command in _reachability.lint_run_blocks(workflow_text):
        reachability_lint_targets.update(
            _reachability.lint_invoked_make_targets(command)
        )
    # `stdlib` is a build step the job needs, not an authority it owns.
    reachability_lint_targets.discard("stdlib")
    assert reachability_lint_targets, "the lint job invokes no make target"

    dispatcher = (ROOT / "scripts" / "ci-preflight-dispatcher.sh").read_text(
        encoding="utf-8"
    )
    block = dispatcher.split("LINT_OWNED_GATES=(", 1)[1].split(")", 1)[0]
    declared = {
        line.strip()
        for line in block.splitlines()
        if line.strip() and not line.strip().startswith("#")
    }

    assert declared == reachability_lint_targets, {
        "runs in lint but not declared owned (still duplicated)": sorted(
            reachability_lint_targets - declared
        ),
        "declared owned but no longer in lint (would stop running)": sorted(
            declared - reachability_lint_targets
        ),
    }


def test_declaring_lint_ownership_removes_the_duplicate_selection() -> None:
    """Behavioural, at the router: same diff, one fewer invocation.

    And the local path is unchanged, which is what keeps `make preflight` a
    rehearsal of CI rather than a subset of it.
    """
    dispatcher = ROOT / "scripts" / "ci-preflight-dispatcher.sh"
    probe = "scripts/structural-authority-audit.py"

    def selected(env: dict[str, str]) -> str:
        result = subprocess.run(
            ["bash", str(dispatcher), "--dry-run", "--", probe],
            cwd=ROOT,
            capture_output=True,
            text=True,
            env={**os.environ, **env},
        )
        assert result.returncode == 0, result.stderr
        return result.stdout

    local = selected({})
    hosted = selected({"LINT_GATE_OWNER": "lint"})
    assert "make structural-lint " in local, local[:0]
    assert "make structural-lint " not in hosted, (
        "declaring lint ownership did not remove the duplicate selection"
    )


def test_every_job_that_runs_the_router_declares_lint_ownership() -> None:
    """A job added later must not quietly reintroduce the duplication."""
    ci = load(WORKFLOWS / "ci.yml")
    missing: list[str] = []
    for job_name, job in jobs(ci).items():
        for step in steps(job):
            body = str(step.get("run", ""))
            if "ci-preflight-route.sh" not in body:
                continue
            # The `changes` job only classifies (`--dry-run`); it dispatches
            # nothing, so it cannot duplicate anything.
            if "--dry-run" in body:
                continue
            env = step.get("env") or {}
            if not isinstance(env, dict) or env.get("LINT_GATE_OWNER") != "lint":
                missing.append(f"{job_name} / {step.get('name', '<unnamed>')}")
    assert not missing, (
        "job(s) run the router without declaring who owns the lint gates, so "
        f"those gates run twice per pull request: {missing}"
    )


# ── contract: the compiled-Hew producer/consumer graph ───────────────────────

# Only the wiring BETWEEN the jobs is asserted. A renamed artifact, a dropped
# `needs:` edge or a matrix resized under a fixed denominator leaves each side
# internally valid, and GitHub reports the mismatch as a skip or an empty
# string, never a failure. Transport, paths and step order are proved by the
# run: `if-no-files-found: error`, and an unpack that verifies the revision.
ARCHIVE_JOB = "linux-nextest-archive"
PRODUCER_JOB = "compiled-hew-linux"
SHARD_JOB = "compiled-hew-shards"
AGGREGATE_JOB = "compiled-hew-aggregate"
REQUIRED_JOB = "linux-required"
_UPLOAD = "actions/upload-artifact@"
_DOWNLOAD = "actions/download-artifact@"


def _needs(job: dict) -> list[str]:
    found = job.get("needs")
    found = [found] if isinstance(found, str) else (found or [])
    return [name for name in found if isinstance(name, str)]


def _artifacts(job: dict, action: str) -> list[str]:
    """Names a job publishes or requests, `matrix.shard` globbed so the shard
    upload template compares against the pattern the aggregate collects with."""
    found: list[str] = []
    for step in steps(job):
        inputs = step.get("with") or {}
        if str(step.get("uses", "")).startswith(action) and isinstance(inputs, dict):
            name = inputs.get("name") or inputs.get("pattern")
            if isinstance(name, str):
                found.append(re.sub(r"\$\{\{\s*matrix\.shard\s*\}\}", "*", name))
    return sorted(found)


def test_the_compiled_hew_jobs_agree_on_their_graph_and_artifacts() -> None:
    """Every edge, artifact name and partition size, read off both sides."""
    all_jobs = jobs(load(WORKFLOWS / "ci.yml"))
    text = {name: "\n".join(_job_strings(job)) for name, job in all_jobs.items()}
    assert {ARCHIVE_JOB} <= set(_needs(all_jobs[PRODUCER_JOB])), PRODUCER_JOB
    assert {PRODUCER_JOB} <= set(_needs(all_jobs[SHARD_JOB])), SHARD_JOB
    consumed = set(_needs(all_jobs[AGGREGATE_JOB]))
    assert {PRODUCER_JOB, SHARD_JOB} <= consumed, consumed
    assert {AGGREGATE_JOB} <= set(_needs(all_jobs[REQUIRED_JOB])), REQUIRED_JOB

    archive = _artifacts(all_jobs[ARCHIVE_JOB], _UPLOAD)
    bundle = _artifacts(all_jobs[PRODUCER_JOB], _UPLOAD)
    reports = _artifacts(all_jobs[SHARD_JOB], _UPLOAD)
    collected = _artifacts(all_jobs[AGGREGATE_JOB], _DOWNLOAD)
    assert _artifacts(all_jobs[PRODUCER_JOB], _DOWNLOAD) == archive != [], archive
    assert _artifacts(all_jobs[SHARD_JOB], _DOWNLOAD) == bundle != [], bundle
    assert collected == sorted(bundle + reports) != bundle, (collected, reports)

    packs = "compiled-hew-artifact.py pack"
    packers = {name for name, body in text.items() if packs in body}
    assert packers == {PRODUCER_JOB}, packers

    matrix = (all_jobs[SHARD_JOB].get("strategy") or {}).get("matrix") or {}
    shards = [str(value) for value in matrix.get("shard") or []]
    total = len(shards)
    assert total >= 2 and shards == [str(n) for n in range(1, total + 1)], shards
    ran = re.search(r"--partition[^\n]*matrix\.shard[^\n]*?/(\d+)", text[SHARD_JOB])
    assert ran and ran.group(1) == str(total), (ran and ran.group(0), shards)
    counts = re.findall(
        r"(?:--shard-count\s+|HEW_SHARD_COUNT=)(\d+)", text[AGGREGATE_JOB]
    )
    assert counts and set(counts) == {str(total)}, (counts, total)


def test_every_needs_expression_names_a_declared_dependency() -> None:
    """An undeclared `needs.<job>.*` resolves to "", so its reader is guessing."""
    stranded: list[str] = []
    for name, job in jobs(load(WORKFLOWS / "ci.yml")).items():
        read: set[str] = set()
        for value in _job_strings(job):
            read |= set(re.findall(r"needs\.([\w-]+)\.(?:result|outputs)", value))
        stranded += [f"{name} reads needs.{miss}" for miss in read - set(_needs(job))]
    assert not stranded, sorted(stranded)


def test_the_required_linux_check_cannot_pass_without_a_compiled_hew_verdict() -> None:
    """Executed, not read: `set -e` is the shell GitHub runs a `run:` block under."""
    body = "set -e\n" + _aggregator_body(REQUIRED_JOB, "COMPILED_HEW_RESULT")
    green = {"CHANGE_RESULT": "success", "RUST_GATES_RESULT": "success"}
    for value in ("success", "failure", "cancelled", "skipped", ""):
        code = _run_aggregator(body, {**green, "COMPILED_HEW_RESULT": value})
        assert (code == 0) is (value == "success"), (value or "<empty>", code)


def _discover_tests() -> list:
    return [
        value
        for name, value in sorted(globals().items())
        if name.startswith("test_") and callable(value)
    ]


if __name__ == "__main__":
    failures = 0
    discovered = _discover_tests()
    for test in discovered:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(discovered)} tests failed")
    print(f"All {len(discovered)} tests passed.")
