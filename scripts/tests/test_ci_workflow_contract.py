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
import subprocess
import re
import sys
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
    relative = text.lstrip("./") or "."
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


def test_no_workflow_step_extracts_an_archive_into_the_checkout() -> None:
    """The router reads the working tree; CI must not write to it.

    Four downloaded wasmtime paths in `$PWD` were enough to make every hosted
    run report `comprehensive: undeclared: …`, on a push to main whose real
    diff was empty. The gate is on the extraction call because that is where
    the decision is made.
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
