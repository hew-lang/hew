#!/usr/bin/env python3
"""Migrate vertical-slice fixtures and playground examples into core-acceptance cases.

One acceptance runner needs one key space. The vertical-slice oracle is a
hand-written shell script whose failures are recorded as `${LINENO}` rows that
move whenever anything above them is edited, and the playground examples are a
third runner again. This script reads those oracles, observes each fixture
through the real compiler, and writes what it observed as a
`tests/core-acceptance/cases/<name>.toml` case, then deletes the lines it
migrated from run.sh.

It is deterministic and idempotent: a second run against its own output finds
nothing left to migrate and writes nothing. Cases point at the fixture where it
already lives (`../vertical-slice/accept/x.hew`), never a copy, so a re-run
after another lane edits a fixture picks the edit up. That is why this script
is what lands: the integration owner re-runs it at the integration head.

A fixture is migrated only when the observation reproduces what run.sh already
asserts, twice. Anything else stays in run.sh and is named in the report, so a
migration can never quietly weaken an oracle.

Usage:
    scripts/migrate-acceptance.py [--report] [--jobs N] [--hew-bin PATH]

    --report  parse, observe and classify; write nothing.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import os
import shlex
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RUN_SH = ROOT / "tests/vertical-slice/run.sh"
ACCEPT = ROOT / "tests/vertical-slice/accept"
CASES = ROOT / "tests/core-acceptance/cases"
CASE_BASE = ROOT / "tests/core-acceptance"
EXAMPLES_MANIFEST = ROOT / "examples/playground/manifest.json"

# run.sh pins the scheduler's worker count for every fixture binary, so an
# observation taken without it is not the observation run.sh recorded.
FIXTURE_ENV = {"HEW_WORKERS": "4"}
TIMEOUT_SECONDS = 30

# Fixtures that reach outside their own run. run.sh cleans their fixed paths
# through an EXIT trap; a case has no equivalent, so a leftover from a crashed
# run would fail the next one for a reason that is not the fixture's semantics.
EXTERNAL_STATE = {
    "node_config_atomic_lifecycle",
}

RUN_HELPERS = {
    "run_accept_expect_status",
    "run_accept_expect_stdout",
    "run_accept_expect_status_and_stdout",
    "run_accept_expect_stdout_contains",
    "run_accept_expect_trap",
    "run_accept_expect_panic",
    "run_actor_bounds_trap_fixture",
    "run_check_run_expect_stdout",
}
CHECK_HELPERS = {
    "expect_check_fail_contains",
    "expect_check_fail_contains_without",
    "expect_check_fail_error_count",
    "expect_check_fail_error_count_no_cascade",
}


@dataclass
class Call:
    helper: str
    args: list[str]
    first_line: int
    last_line: int


@dataclass
class Case:
    identifier: str
    kind: str
    source: str
    intent: str
    env: dict[str, str] = field(default_factory=dict)
    stdout: str | None = None
    stderr: str = ""
    exit_code: int | None = None
    diagnostics: list[dict] = field(default_factory=list)
    lines: list[int] = field(default_factory=list)


def toml_string(value: str) -> str:
    body = (
        value.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")
    )
    return (
        '"'
        + "".join(c if c >= " " or c in "" else f"\\u{ord(c):04x}" for c in body)
        + '"'
    )


def render(case: Case) -> str:
    out = [
        "[[case]]",
        f"id = {toml_string(case.identifier)}",
        f"intent = {toml_string(case.intent)}",
        f"source = {toml_string(case.source)}",
        'suites = ["acceptance"]',
        f"timeout_seconds = {TIMEOUT_SECONDS}",
    ]
    if case.kind != "run":
        out.append(f"kind = {toml_string(case.kind)}")
    if case.env:
        out.append("")
        out.append("[case.env]")
        for name in sorted(case.env):
            out.append(f"{name} = {toml_string(case.env[name])}")
    out.append("")
    if case.kind == "run":
        out.append("[case.expected]")
        out.append(f"stdout = {toml_string(case.stdout or '')}")
        if case.stderr:
            out.append(f"stderr = {toml_string(case.stderr)}")
        out.append(f"exit = {case.exit_code}")
    else:
        for diagnostic in case.diagnostics:
            out.append("[[case.expected.diagnostics]]")
            out.append(f"code = {toml_string(diagnostic['code'])}")
            out.append(f"line = {diagnostic['line']}")
            out.append(f"column = {diagnostic['column']}")
            if diagnostic.get("message"):
                out.append(f"message = {toml_string(diagnostic['message'])}")
            out.append("")
        out.pop()
    return "\n".join(out) + "\n"


# ---------------------------------------------------------------- run.sh ----


def logical_lines(lines: list[str]) -> list[tuple[int, int, str]]:
    joined = []
    index = 0
    while index < len(lines):
        first = index
        text = lines[index]
        while text.endswith("\\") and index + 1 < len(lines):
            index += 1
            text = text[:-1] + " " + lines[index].strip()
        joined.append((first, index, text))
        index += 1
    return joined


def unescape(text: str) -> str:
    """Undo the backslash escapes bash strips inside double quotes.

    Python's shlex leaves `\\`` alone because a backtick is not one of its
    escaped quote characters; bash does not, so the assertion text run.sh
    actually compares has no backslashes in it.
    """
    for escaped, literal in (("\\`", "`"), ('\\"', '"'), ("\\$", "$")):
        text = text.replace(escaped, literal)
    return text


def parse_calls(lines: list[str]) -> tuple[list[Call], list[str]]:
    calls: list[Call] = []
    declined: list[str] = []
    for first, last, text in logical_lines(lines):
        if not text or text[0].isspace():
            continue
        helper = text.split(" ", 1)[0].split("\t", 1)[0]
        if helper not in RUN_HELPERS and helper not in CHECK_HELPERS:
            continue
        try:
            args = shlex.split(text)[1:]
        except ValueError:
            declined.append(f"run.sh:{first + 1} {helper}: unparseable arguments")
            continue
        if any("$" in argument and "${ROOT}" not in argument for argument in args):
            declined.append(f"run.sh:{first + 1} {helper}: expands a shell variable")
            continue
        args = [unescape(argument.replace("${ROOT}", str(ROOT))) for argument in args]
        calls.append(Call(helper, args, *bracket(lines, first, last)))
    return calls, declined


def bracket(lines: list[str], first: int, last: int) -> tuple[int, int]:
    """Widen a call to the failure-count idiom that guards it.

    Several call sites are wrapped in `_fcb=${fail_count}` ... `[[ ... ]] &&
    mark_pass`, which exists only to report that one call. Left behind, it
    would print an unconditional PASS for a fixture run.sh no longer runs.
    """
    if first > 0 and lines[first - 1].strip() == "_fcb=${fail_count}":
        first -= 1
    if last + 1 < len(lines) and lines[last + 1].startswith(
        '[[ "${fail_count}" == "${_fcb}" ]]'
    ):
        last += 1
    return first, last


def case_id(fixture: str) -> str:
    """A case id for a fixture name.

    Some fixtures live in their own directory because they are multi-file
    (`dir_module_peer_span_identity/main.hew`); the directory is the fixture,
    so the case is named for it, not for every one of them being `main`.
    """
    name = fixture.removesuffix(".hew")
    if name.endswith("/main"):
        name = name[: -len("/main")]
    return name.replace("/", "-")


# ----------------------------------------------------------- observation ----


def observe_run(hew: str, source: Path, env: dict[str, str], work: Path):
    """Compile and run at every profile the acceptance runner uses.

    A migrated case is run at O0 and O2, and each is run twice: an expectation
    that only one profile reproduces, or that a rerun does not, is not an
    observation the runner can hold.
    """
    environment = dict(os.environ)
    environment.update(env)
    observations = []
    for level in ("0", "2"):
        emit = work / f"emit-O{level}"
        emit.mkdir(parents=True, exist_ok=True)
        compiled = subprocess.run(
            [
                hew,
                "compile",
                "--emit-dir",
                str(emit),
                "--opt-level",
                level,
                str(source),
            ],
            cwd=ROOT,
            capture_output=True,
            text=True,
            timeout=TIMEOUT_SECONDS * 4,
        )
        if compiled.returncode != 0:
            return None, f"compile at O{level} exited {compiled.returncode}"
        binary = emit / source.stem
        if not binary.is_file():
            return None, f"compiler produced no binary at O{level}"
        for _ in range(2):
            try:
                done = subprocess.run(
                    [str(binary)],
                    cwd=ROOT,
                    capture_output=True,
                    text=True,
                    timeout=TIMEOUT_SECONDS,
                    env=environment,
                )
            except subprocess.TimeoutExpired:
                return None, f"fixture timed out at O{level}"
            observations.append((done.returncode, done.stdout, done.stderr))
    if len(set(observations)) != 1:
        if observations[0] != observations[1] or observations[2] != observations[3]:
            return None, "output differs between two runs of the same binary"
        return None, "O0 and O2 disagree"
    return observations[0], None


def observe_check(hew: str, source: Path):
    done = subprocess.run(
        [hew, "check", str(source), "--format", "json"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=TIMEOUT_SECONDS * 4,
    )
    try:
        diagnostics = json.loads(done.stdout)
    except json.JSONDecodeError:
        return done.returncode, None
    return done.returncode, diagnostics


# ------------------------------------------------------------- migration ----


def run_case_from(call: Call, observed) -> tuple[Case | None, str | None]:
    """Turn one accept-fixture helper call into a case, or say why it cannot."""
    fixture = call.args[0]
    status, stdout, stderr = observed
    source = ACCEPT / f"{fixture}.hew"
    expected_file = ACCEPT / f"{fixture}.expected"
    env = dict(FIXTURE_ENV)
    helper = call.helper

    want_status = 0
    intent = ""
    if helper == "run_accept_expect_status":
        want_status = int(call.args[1])
        for assignment in call.args[2:]:
            name, _, value = assignment.partition("=")
            env[name] = value
        intent = f"The fixture runs to completion and exits {want_status}."
    elif helper in ("run_accept_expect_stdout", "run_check_run_expect_stdout"):
        intent = (
            "The fixture runs to completion and prints exactly its recorded output."
        )
    elif helper == "run_accept_expect_status_and_stdout":
        want_status = int(call.args[1])
        intent = (
            f"The fixture prints exactly its recorded output and exits {want_status}."
        )
    elif helper == "run_accept_expect_stdout_contains":
        for wanted in call.args[1:]:
            if wanted not in stdout:
                return None, f"stdout is missing {wanted!r}"
        intent = (
            "The fixture runs to completion and prints exactly what it printed here."
        )
    elif helper == "run_accept_expect_trap":
        want_status = 1
        marker = f"hew: failure: {call.args[1]}"
        if marker not in stderr:
            return None, f"stderr is missing the trap kind {call.args[1]!r}"
        intent = f"The guard traps: the fixture reports {call.args[1]} and exits 1 under the one exit rule."
    elif helper == "run_accept_expect_panic":
        want_status = 1
        if call.args[1] not in stderr:
            return None, f"stderr is missing the panic message {call.args[1]!r}"
        if "panicked at" in stderr:
            return None, "stderr carries Rust's default panic hook output"
        if len(call.args) > 2:
            recorded = Path(call.args[2])
            if not recorded.is_file():
                return None, f"expected-stdout file {recorded.name} is missing"
            if recorded.read_text() != stdout:
                return None, f"stdout does not match {recorded.name}"
        intent = (
            "The panic is reported as Hew's own typed failure and the process exits 1."
        )
    elif helper == "run_actor_bounds_trap_fixture":
        want_status = int(call.args[3]) if len(call.args) > 3 else 1
        for wanted in call.args[1:3]:
            if wanted and wanted not in stderr:
                return None, f"stderr is missing {wanted!r}"
        intent = "The actor's guard traps and the failure names the actor it fired in."
    else:
        return None, f"unhandled helper {helper}"

    if status != want_status:
        return None, f"exits {status} today, run.sh expects {want_status}"

    if helper in (
        "run_accept_expect_stdout",
        "run_check_run_expect_stdout",
        "run_accept_expect_status_and_stdout",
    ):
        if not expected_file.is_file():
            return None, "no .expected file"
        recorded = expected_file.read_text()
        if recorded != stdout:
            return None, "stdout does not match its .expected file"

    return (
        Case(
            identifier=case_id(fixture),
            kind="run",
            source=os.path.relpath(source, CASE_BASE),
            intent=intent,
            env=env,
            stdout=stdout,
            stderr=stderr,
            exit_code=status,
            lines=list(range(call.first_line, call.last_line + 1)),
        ),
        None,
    )


def check_case_from(
    calls: list[Call], status: int, diagnostics
) -> tuple[Case | None, str | None]:
    """Turn every assertion on one reject fixture into a single case."""
    path = Path(calls[0].args[0])
    identifier = case_id(
        str(path.relative_to(path.parent.parent))
        if path.parent.name not in ("accept", "reject")
        else path.stem
    )
    if status == 0:
        return None, "hew check accepts this fixture today"
    if not diagnostics:
        return None, "hew check reports no structured diagnostic"

    counted = [c for c in calls if c.helper.startswith("expect_check_fail_error_count")]
    if not counted and any(
        c.helper == "expect_check_fail_contains_without" for c in calls
    ):
        # The point of that helper is a substring that must be ABSENT, and a
        # reject case says nothing about diagnostics it did not name, so
        # migrating it would drop the control silently.
        return None, "asserts an absent substring, which reject cannot express"
    substrings = [
        c.args[1]
        for c in calls
        if c.helper
        in ("expect_check_fail_contains", "expect_check_fail_contains_without")
    ]

    def entry(diagnostic, message=None):
        return {
            "code": diagnostic["code"],
            "line": diagnostic["span"]["start_line"],
            "column": diagnostic["span"]["start_col"],
            "message": message,
        }

    lines = sorted({n for c in calls for n in range(c.first_line, c.last_line + 1)})

    if counted:
        wanted = int(counted[0].args[1])
        errors = [d for d in diagnostics if d.get("severity") == "error"]
        if status != 1 or len(errors) != wanted:
            return (
                None,
                f"reports {len(errors)} error(s) at exit {status}, run.sh expects {wanted} at exit 1",
            )
        entries = []
        claimed = set()
        # The count assertion is about errors, but a `check` case pins the
        # exact set `hew check` reports — a warning it drops would be an
        # unnamed extra diagnostic and fail the case.
        for diagnostic in diagnostics:
            message = None
            for substring in substrings:
                if substring not in claimed and substring in diagnostic["message"]:
                    message = substring
                    claimed.add(substring)
                    break
            entries.append(entry(diagnostic, message))
        if len(claimed) != len(set(substrings)):
            return None, "a named diagnostic substring is not in any structured message"
        return (
            Case(
                identifier=identifier,
                kind="check",
                source=os.path.relpath(path, CASE_BASE),
                intent=f"The fixture is refused with exactly {wanted} error(s), and no cascade beyond them.",
                diagnostics=entries,
                lines=lines,
            ),
            None,
        )

    entries = []
    claimed = set()
    for substring in substrings:
        match = next(
            (
                d
                for d in diagnostics
                if substring in d["message"] and id(d) not in claimed
            ),
            None,
        )
        if match is None:
            match = next((d for d in diagnostics if substring in d["message"]), None)
        if match is None:
            # Pinning code and position instead would turn a text assertion
            # that fails today into a case that passes, which is exactly the
            # silent weakening this migration must not do.
            return None, f"the named text {substring!r} is in no structured message"
        claimed.add(id(match))
        entries.append(entry(match, substring))
    if not entries:
        return None, "no assertion to express"
    return (
        Case(
            identifier=identifier,
            kind="reject",
            source=os.path.relpath(path, CASE_BASE),
            intent="The fixture is refused, and the refusal names this diagnostic.",
            diagnostics=entries,
            lines=lines,
        ),
        None,
    )


def example_cases(hew: str, jobs: int, work: Path) -> tuple[list[Case], list[str]]:
    """The playground examples, verified today by their own runner."""
    if not EXAMPLES_MANIFEST.is_file():
        return [], ["examples/playground/manifest.json is missing"]
    entries = json.loads(EXAMPLES_MANIFEST.read_text())
    manifest_dir = EXAMPLES_MANIFEST.parent
    runnable = [e for e in entries if e["capabilities"]["wasi"] == "runnable"]
    cases: list[Case] = []
    skipped: list[str] = []

    def observe(entry):
        source = manifest_dir / entry["source_path"]
        expected = manifest_dir / entry["expected_path"]
        if not source.is_file():
            return entry, None, "source is missing"
        if not expected.is_file():
            return entry, None, "no .expected file"
        observed, why = observe_run(
            hew, source, FIXTURE_ENV, work / entry["id"].replace("/", "-")
        )
        return entry, observed, why

    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
        for entry, observed, why in pool.map(observe, runnable):
            source = manifest_dir / entry["source_path"]
            name = f"example-{entry['id'].replace('/', '-')}"
            if observed is None:
                skipped.append(f"{entry['id']}: {why}")
                continue
            status, stdout, stderr = observed
            expected = (manifest_dir / entry["expected_path"]).read_text()
            if status != 0 or stdout != expected:
                skipped.append(
                    f"{entry['id']}: exits {status}, or its output does not match its .expected file"
                )
                continue
            cases.append(
                Case(
                    identifier=name,
                    kind="run",
                    source=os.path.relpath(source, CASE_BASE),
                    intent="The published example compiles, runs and prints exactly what it advertises.",
                    env=dict(FIXTURE_ENV),
                    stdout=stdout,
                    stderr=stderr,
                    exit_code=0,
                )
            )
    return cases, skipped


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--report", action="store_true")
    parser.add_argument("--jobs", type=int, default=min(16, os.cpu_count() or 4))
    parser.add_argument("--hew-bin", default=str(ROOT / "target/debug/hew"))
    arguments = parser.parse_args()
    hew = arguments.hew_bin
    if not Path(hew).is_file():
        print(
            f"error: no compiler at {hew}; run `make hew-debug` first", file=sys.stderr
        )
        return 2

    lines = RUN_SH.read_text().split("\n")
    calls, declined = parse_calls(lines)
    work = Path(
        tempfile.mkdtemp(
            prefix="migrate-acceptance-", dir=os.environ.get("TMPDIR", "/tmp")
        )
    )
    left_behind: list[str] = list(declined)
    cases: list[Case] = []

    try:
        run_calls = [c for c in calls if c.helper in RUN_HELPERS]
        by_fixture: dict[str, list[Call]] = {}
        for call in run_calls:
            by_fixture.setdefault(call.args[0], []).append(call)

        def observe_fixture(item):
            fixture, fixture_calls = item
            source = ACCEPT / f"{fixture}.hew"
            if not source.is_file():
                return fixture, fixture_calls, None, "fixture source is missing"
            if fixture in EXTERNAL_STATE:
                return (
                    fixture,
                    fixture_calls,
                    None,
                    "owns state at a fixed path that only run.sh's trap cleans",
                )
            env = dict(FIXTURE_ENV)
            if fixture_calls[0].helper == "run_accept_expect_status":
                for assignment in fixture_calls[0].args[2:]:
                    name, _, value = assignment.partition("=")
                    env[name] = value
            observed, why = observe_run(hew, source, env, work / fixture)
            return fixture, fixture_calls, observed, why

        with concurrent.futures.ThreadPoolExecutor(max_workers=arguments.jobs) as pool:
            for fixture, fixture_calls, observed, why in pool.map(
                observe_fixture, by_fixture.items()
            ):
                if observed is None:
                    left_behind.append(f"{fixture}: {why}")
                    continue
                if len(fixture_calls) > 1:
                    left_behind.append(
                        f"{fixture}: asserted by {len(fixture_calls)} separate rows"
                    )
                    continue
                case, why = run_case_from(fixture_calls[0], observed)
                if case is None:
                    left_behind.append(f"{fixture}: {why}")
                else:
                    cases.append(case)

        check_calls: dict[str, list[Call]] = {}
        for call in calls:
            if call.helper in CHECK_HELPERS:
                check_calls.setdefault(call.args[0], []).append(call)

        def observe_reject(item):
            path, path_calls = item
            status, diagnostics = observe_check(hew, Path(path))
            return path, path_calls, status, diagnostics

        with concurrent.futures.ThreadPoolExecutor(max_workers=arguments.jobs) as pool:
            for path, path_calls, status, diagnostics in pool.map(
                observe_reject, check_calls.items()
            ):
                case, why = check_case_from(path_calls, status, diagnostics)
                if case is None:
                    left_behind.append(f"{Path(path).stem}: {why}")
                else:
                    cases.append(case)

        examples, example_skips = example_cases(hew, arguments.jobs, work)
        cases.extend(examples)
        left_behind.extend(example_skips)
    finally:
        shutil.rmtree(work, ignore_errors=True)

    seen: dict[str, Case] = {}
    for case in sorted(cases, key=lambda c: c.identifier):
        if case.identifier in seen:
            left_behind.append(f"{case.identifier}: case id already taken")
            continue
        existing = CASES / f"{case.identifier}.toml"
        if existing.is_file():
            left_behind.append(
                f"{case.identifier}: a core-acceptance case already owns this id"
            )
            continue
        seen[case.identifier] = case

    kinds: dict[str, int] = {}
    for case in seen.values():
        kinds[case.kind] = kinds.get(case.kind, 0) + 1
    print("cases to write:")
    for kind, count in sorted(kinds.items()):
        print(f"  {count:4d}  {kind}")
    print(f"  total {len(seen)}")
    print(f"left in run.sh: {len(left_behind)}")
    for note in sorted(left_behind):
        print(f"  {note}")

    if arguments.report:
        return 0

    for case in seen.values():
        (CASES / f"{case.identifier}.toml").write_text(render(case))

    migrated = {n for case in seen.values() for n in case.lines}
    kept = [line for index, line in enumerate(lines) if index not in migrated]
    RUN_SH.write_text("\n".join(kept))
    print(f"run.sh: {len(lines)} lines -> {len(kept)} lines")
    return 0


if __name__ == "__main__":
    sys.exit(main())
