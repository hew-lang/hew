#!/usr/bin/env python3
"""check-gate-reachability.py — assert every gate in this repo is actually run.

The required Linux job executes the local dispatcher directly, so this checker
expands its fail-closed selection when it builds the hosted command graph.
It also detects a check absent from both graphs, the blind spot that previously
left test code in the tree while executing nowhere.

This gate closes it, in five directions:

  A0  self-anchor — this checker is invoked by a CI workflow step.
  A1  every CI-gate-shaped Makefile target is reached by a CI workflow step, by
      the workflow, or transitively as a prerequisite of one that is. A named
      host-release authority is checked separately: it is a real local port,
      not a hosted CI result pretending to be that port.
  A2  every workspace crate is covered by a CI test invocation: included in a
      `--workspace` run that does not `--exclude` it, or named with `-p` by a
      CI step or by a CI-reached Makefile target.
  A3  every exclusion is compensated:
      a) CI never runs a nextest profile other than `ci` (so a fast local tier
         cannot quietly become the CI tier);
      b) every selector subtracted by `profile.ci`'s default-filter is named by
         a CI-reached invocation;
      c) `#[ignore]` is permitted only in a crate whose ignored tests are run by
         a CI-reached target (`--run-ignored` / `-- --ignored`); and
      d) every inline `-E` filter a CI step passes to nextest is compensated,
         package by package, by an unfiltered CI run over the same packages. An
         `-E` is an exclusion like any other: without a compensating unfiltered
         run, the tests it subtracts execute in no job at all.
  A4  every `make <target>` written in tracked documentation, or in a script or
      workflow comment, names a target the Makefile still defines. A0..A3 all
      read the same edge CI -> Makefile; this one reads docs -> Makefile, which
      is why deleting a target could leave an invocation of `test-all` in the
      CONTRIBUTING table with nothing to notice it.

There is deliberately no waiver list. An unreached gate is either wired in or
deleted; "tracked for later" is how the eight orphans got there in the first
place. A gate this checker cannot see is a gate that is not running. A4 has no
skip list either: a doc that means to show a command shape rather than a real
target writes the target as a metavariable (`make <target>`), which is a
property of the example, not a licence for one file to be wrong.

# What counts as an edge

An edge from CI to a gate exists only when a step that CAN RUN invokes it. The
first version of this checker read every workflow as one raw string, so a
MENTION was an edge: `release-gate.yml` carried
`# TODO(playground-wasi-gate): add \\`make playground-wasi-check\\` here once …`
and the checker reported `playground-wasi-check` as reached — a comment saying
the gate is NOT wired was counted as wiring. A gate whose own defect class is
"looks connected, executes nowhere" cannot afford that.

So the workflows are parsed structurally (`parse_yaml` below, a fail-closed
subset parser — no third-party dependency, matching every other Python gate in
`scripts/`), and only these produce edges:

  * a workflow that can trigger. A workflow with no `on:` trigger, or one whose
    only trigger is `workflow_call` with no caller, never runs.
  * a job with no statically-false `if:`. `if: false` is a disabled job.
  * a step with no statically-false `if:`, carrying a `run:` body — plus the
    `run:` steps of a local composite action the step `uses:`.
  * that `run:` body with SHELL COMMENTS STRIPPED. Same rule one level down: a
    `# make foo` inside a script is a note, not an invocation.

A dynamic `if:` (`needs.changes.outputs.docs == 'true'`, `env.RUN_CODE_PATH`)
IS an edge: that step can run. Proving which pull requests it runs on is the
path-filter oracle's job, not this one's.

`continue-on-error: true` is also an edge. The advisory sanitizer jobs really
do execute their gate; whether a job BLOCKS a merge is a different axis from
whether it RUNS, and conflating the two here would demand deleting deliberately
advisory jobs. A0/A1/A2/A3 answer "does it run".

Inside the Makefile the two edges are a PREREQUISITE of an already-reached
target and a `$(MAKE) x` in its recipe — both read after variable expansion
(`makefile_variables` below), because a prerequisite list written as a bundle
variable is an edge make follows and a literal reader does not see. Both flow
strictly forward from the CI roots: being the prerequisite of an UNREACHED
target proves nothing, or every orphan could vouch for itself.

Usage:
  scripts/check-gate-reachability.py            # check
  scripts/check-gate-reachability.py --verbose  # include the reached sets
"""

from __future__ import annotations

import os
import re
import shlex
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
MAKEFILE = REPO_ROOT / "Makefile"
WORKFLOW_DIR = REPO_ROOT / ".github" / "workflows"
ACTION_DIR = REPO_ROOT / ".github" / "actions"
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
        | fuzz-oracle | sandbox-parity | licenses-check | .*-oracle | .*-e2e
        | libhew-link-race-test | observe-functional-test
        )$""",
    re.VERBOSE,
)


@dataclass(frozen=True)
class HostReleaseAuthority:
    """A release verdict whose measuring tool belongs to a specific host.

    This is intentionally a *named class*, not an ``unreached by design``
    escape hatch.  A member must have an executable Make port and must stay
    out of hosted CI: a hosted result cannot certify a measurement that needs
    the release operator's Darwin environment.  The runner itself must reject
    a wrong host rather than turn a skip into a green verdict (pinned by the
    self-tests below).
    """

    target: str
    host: str
    runner: str


# `leaks(1)` observes a local Darwin allocator/process configuration. GitHub's
# hosted macOS runners do not supply that release-authority environment, so
# adding this target to a hosted workflow would be a category error, not extra
# coverage. Keep the small, explicit set here rather than letting a comment or
# a target-name convention silently turn a non-CI gate green.
HOST_RELEASE_AUTHORITIES = (
    HostReleaseAuthority(
        target="macos-leak-oracle",
        host="Darwin",
        runner="scripts/macos-leak-oracle.sh",
    ),
)
HOST_RELEASE_AUTHORITY_BY_TARGET = {
    authority.target: authority for authority in HOST_RELEASE_AUTHORITIES
}


def ci_gate_targets(phony: set[str]) -> list[str]:
    """Gate-shaped targets that must execute in runnable hosted CI.

    Every generic ``*-oracle``/``*-e2e`` belongs here automatically. The only
    subtraction is the named host-release-authority class above; a new target
    cannot self-classify out of CI by adding a comment, a skip, or a suffix.
    """
    return sorted(
        target
        for target in phony
        if GATE_NAME_RE.match(target) and target not in HOST_RELEASE_AUTHORITY_BY_TARGET
    )


def unreached_ci_gates(phony: set[str], reached: set[str]) -> list[str]:
    """The CI-gate class that remains red for a particular CI graph."""
    return [target for target in ci_gate_targets(phony) if target not in reached]


def host_release_authority_is_ported(
    authority: HostReleaseAuthority,
    known: set[str],
    recipes: dict[str, str],
) -> bool:
    """Whether the authority has a real Make port, not a prose claim.

    Shell comments are stripped before comparing a complete command. An echo,
    a comment, or a conditional `skip` therefore cannot impersonate execution
    of the host runner.
    """
    if authority.target not in known:
        return False
    return any(
        segment.lstrip("@-").strip() == authority.runner
        for segment in _command_segments(
            strip_shell_comments(recipes.get(authority.target, ""))
        )
    )


# Nextest profiles CI is allowed to run. Anything else would let a fast local
# iteration tier (which excludes most of the corpus) stand in for the CI tier.
CI_ALLOWED_NEXTEST_PROFILES = {"ci"}

# GitHub workflow triggers this checker understands. An unknown key under `on:`
# is a parse failure, not a shrug: a trigger nobody modelled could be one that
# never fires, and every reachability answer downstream would be wrong.
KNOWN_TRIGGERS = {
    "push",
    "pull_request",
    "pull_request_target",
    "workflow_dispatch",
    "workflow_call",
    "workflow_run",
    "schedule",
    "release",
    "merge_group",
    "repository_dispatch",
    "issue_comment",
    "issues",
}

# Triggers that fire on their own. `workflow_call` only fires when another
# workflow calls it, which is resolved separately.
SELF_FIRING_TRIGGERS = KNOWN_TRIGGERS - {"workflow_call"}


class Findings:
    def __init__(self) -> None:
        self.failures: list[str] = []

    def fail(self, assertion: str, subject: str, detail: str) -> None:
        self.failures.append(f"  FAIL [{assertion}] {subject}\n       {detail}")

    def count(self, assertion: str) -> int:
        return sum(1 for f in self.failures if f"[{assertion}]" in f)


# ── A fail-closed YAML subset parser ──────────────────────────────────────────
#
# Deliberately NOT PyYAML: every other Python gate in scripts/ runs on a bare
# stdlib interpreter (there is no pip step in any workflow and no requirements
# file in the tree), and a reachability gate that cannot start on a fresh
# checkout is a gate that does not run. The subset covers what GitHub workflow
# files actually use — block mappings, block sequences, block scalars, flow
# sequences/mappings of scalars — and raises on everything else (anchors,
# aliases, tags, multi-document streams, multi-line plain scalars, tab
# indentation). Fail closed: an unparsed construct is an error, never an
# empty result that silently reports "nothing to check here".


class YamlError(Exception):
    """A workflow this checker refuses to guess about."""


@dataclass
class _Line:
    number: int
    indent: int
    text: str


_KEY_RE = re.compile(
    r"""^(?P<key>'[^']*'|"[^"]*"|[^\s:#][^:#]*?)\s*:(?=\s|$)(?P<rest>.*)$"""
)
_BLOCK_SCALAR_RE = re.compile(
    r"^(?P<style>[|>])(?P<chomp>[-+]?)(?P<explicit>\d*)\s*(?:#.*)?$"
)


def _scan_lines(text: str, source: str) -> list[_Line]:
    lines: list[_Line] = []
    for number, raw in enumerate(text.splitlines(), start=1):
        stripped = raw.lstrip(" ")
        indent = len(raw) - len(stripped)
        if raw[:indent].count("\t"):
            raise YamlError(f"{source}:{number}: tab in indentation")
        lines.append(_Line(number, indent, raw[indent:]))
    return lines


def _significant(lines: list[_Line], index: int) -> int:
    """Index of the next line that is neither blank nor comment-only."""
    while index < len(lines):
        text = lines[index].text
        if text.strip() and not text.lstrip().startswith("#"):
            return index
        index += 1
    return len(lines)


def _strip_comment(value: str) -> str:
    """Drop a trailing `#` comment from a plain scalar.

    YAML starts a comment at a `#` preceded by whitespace or line start, in a
    plain scalar regardless of any quote characters around it — a plain scalar
    has no quoting. Matching that exactly is the point: the runner sees the
    same truncation, so the checker must not read a command the job never gets.
    """
    for i, ch in enumerate(value):
        if ch == "#" and (i == 0 or value[i - 1] in " \t"):
            return value[:i]
    return value


def _unquote(token: str, source: str, number: int) -> str:
    token = token.strip()
    if len(token) >= 2 and token[0] == token[-1] == "'":
        return token[1:-1].replace("''", "'")
    if len(token) >= 2 and token[0] == token[-1] == '"':
        body = token[1:-1]
        out: list[str] = []
        i = 0
        while i < len(body):
            if body[i] == "\\" and i + 1 < len(body):
                nxt = body[i + 1]
                out.append({"n": "\n", "t": "\t", "\\": "\\", '"': '"'}.get(nxt, nxt))
                i += 2
                continue
            out.append(body[i])
            i += 1
        return "".join(out)
    if token[:1] in {"&", "*", "!"}:
        raise YamlError(
            f"{source}:{number}: anchors, aliases and tags are not supported "
            f"({token!r}); this checker will not guess what they expand to"
        )
    return token


def _scalar(token: str, source: str, number: int) -> object:
    token = token.strip()
    if token in {"", "~", "null"}:
        return None
    if token in {"true", "True", "yes", "on"}:
        return True
    if token in {"false", "False", "no", "off"}:
        return False
    return _unquote(token, source, number)


def _flow_items(body: str, source: str, number: int) -> list[str]:
    items: list[str] = []
    depth = 0
    quote = ""
    current = ""
    for ch in body:
        if quote:
            current += ch
            if ch == quote:
                quote = ""
            continue
        if ch in "'\"":
            quote = ch
            current += ch
            continue
        if ch in "[{":
            depth += 1
        elif ch in "]}":
            depth -= 1
        if ch == "," and depth == 0:
            items.append(current)
            current = ""
            continue
        current += ch
    if current.strip():
        items.append(current)
    if quote or depth:
        raise YamlError(f"{source}:{number}: unterminated flow collection")
    return items


def _parse_flow(token: str, source: str, number: int) -> object:
    token = token.strip()
    if token.startswith("["):
        if not token.endswith("]"):
            raise YamlError(f"{source}:{number}: multi-line flow sequence")
        return [
            _scalar(item, source, number)
            for item in _flow_items(token[1:-1], source, number)
        ]
    if not token.endswith("}"):
        raise YamlError(f"{source}:{number}: multi-line flow mapping")
    out: dict[str, object] = {}
    for item in _flow_items(token[1:-1], source, number):
        key, sep, value = item.partition(":")
        if not sep:
            raise YamlError(f"{source}:{number}: flow mapping entry without a value")
        out[_unquote(key, source, number)] = _scalar(value, source, number)
    return out


def _fold(body: list[str]) -> str:
    """Fold a `>` block scalar the way YAML does.

    Line breaks between equally-indented non-empty lines become spaces; a blank
    line becomes a newline; a more-indented line keeps its own breaks. This is
    load-bearing, not cosmetic: `run: >-` with `cargo nextest run --workspace`
    on one line and `--exclude hew-cabi` on the next is ONE command, and a
    checker that reads them as two would see an unfiltered workspace run that
    the job never issues.
    """
    out = ""
    for index, line in enumerate(body):
        more_indented = line[:1] == " "
        if index == 0:
            out = line
            continue
        previous = body[index - 1]
        if not line.strip():
            out += "\n"
        elif not previous.strip():
            out += line
        elif more_indented or previous[:1] == " ":
            out += "\n" + line
        else:
            out += " " + line
    return out


def _chomp(text: str, chomp: str) -> str:
    if chomp == "-":
        return text.rstrip("\n")
    if chomp == "+":
        return text
    return text.rstrip("\n") + "\n" if text.strip() else ""


def _parse_block_scalar(
    lines: list[_Line],
    index: int,
    parent_indent: int,
    header: re.Match[str],
    source: str,
) -> tuple[str, int]:
    if header.group("explicit"):
        raise YamlError(
            f"{source}:{lines[index - 1].number}: explicit block-scalar indentation "
            "indicators are not supported"
        )
    body_indent: int | None = None
    body: list[str] = []
    while index < len(lines):
        line = lines[index]
        if not line.text.strip():
            body.append("")
            index += 1
            continue
        if line.indent <= parent_indent:
            break
        if body_indent is None:
            body_indent = line.indent
        if line.indent < body_indent:
            break
        body.append(" " * (line.indent - body_indent) + line.text)
        index += 1
    while body and not body[-1].strip():
        body.pop()
    text = "\n".join(body) if header.group("style") == "|" else _fold(body)
    return _chomp(text + "\n" if body else "", header.group("chomp")), index


def _parse_value(
    lines: list[_Line], index: int, indent: int, rest: str, source: str
) -> tuple[object, int]:
    """Parse the value of `key:` whose remainder on the line is `rest`."""
    line = lines[index]
    body = rest.strip()
    block = _BLOCK_SCALAR_RE.match(body) if body else None
    if block:
        return _parse_block_scalar(lines, index + 1, indent, block, source)
    if not body or body.startswith("#"):
        nxt = _significant(lines, index + 1)
        if nxt < len(lines) and lines[nxt].indent > indent:
            return _parse_node(lines, nxt, lines[nxt].indent, source)
        if (
            nxt < len(lines)
            and lines[nxt].indent == indent
            and lines[nxt].text.startswith("-")
        ):
            return _parse_node(lines, nxt, indent, source)
        return None, index + 1
    stripped = _strip_comment(body).strip() if body[0] not in "'\"[{" else body
    if stripped[:1] in "[{":
        return _parse_flow(stripped, source, line.number), index + 1
    nxt = _significant(lines, index + 1)
    if nxt < len(lines) and lines[nxt].indent > indent:
        raise YamlError(
            f"{source}:{lines[nxt].number}: multi-line plain scalar continuing "
            f"{line.text.split(':')[0].strip()!r}; quote it or use a block scalar"
        )
    return _scalar(stripped, source, line.number), index + 1


def _parse_mapping(
    lines: list[_Line], index: int, indent: int, source: str
) -> tuple[dict[str, object], int]:
    out: dict[str, object] = {}
    while True:
        index = _significant(lines, index)
        if index >= len(lines):
            break
        line = lines[index]
        if line.indent < indent:
            break
        if line.indent > indent:
            raise YamlError(f"{source}:{line.number}: unexpected indentation")
        if line.text.startswith("- "):
            raise YamlError(f"{source}:{line.number}: sequence item inside a mapping")
        match = _KEY_RE.match(line.text)
        if not match:
            raise YamlError(
                f"{source}:{line.number}: not a mapping entry: {line.text!r}"
            )
        key = _unquote(match.group("key"), source, line.number)
        value, index = _parse_value(lines, index, indent, match.group("rest"), source)
        if key in out:
            raise YamlError(f"{source}:{line.number}: duplicate key {key!r}")
        out[key] = value
    return out, index


def _parse_sequence(
    lines: list[_Line], index: int, indent: int, source: str
) -> tuple[list[object], int]:
    out: list[object] = []
    while True:
        index = _significant(lines, index)
        if index >= len(lines):
            break
        line = lines[index]
        if line.indent < indent:
            break
        if line.indent > indent or not line.text.startswith("-"):
            break
        after = line.text[1:]
        if not after.strip() or after.lstrip().startswith("#"):
            nxt = _significant(lines, index + 1)
            if nxt < len(lines) and lines[nxt].indent > indent:
                value, index = _parse_node(lines, nxt, lines[nxt].indent, source)
            else:
                value, index = None, index + 1
            out.append(value)
            continue
        if not after.startswith(" "):
            raise YamlError(f"{source}:{line.number}: `-` must be followed by a space")
        content_indent = indent + 1 + (len(after) - len(after.lstrip(" ")))
        content = after.lstrip(" ")
        if not _KEY_RE.match(content) and not content.startswith("- "):
            out.append(_scalar(_strip_comment(content), source, line.number))
            index += 1
            continue
        lines[index] = _Line(line.number, content_indent, content)
        value, index = _parse_node(lines, index, content_indent, source)
        out.append(value)
    return out, index


def _parse_node(
    lines: list[_Line], index: int, indent: int, source: str
) -> tuple[object, int]:
    index = _significant(lines, index)
    if index >= len(lines):
        return None, index
    if lines[index].text.startswith("-") and (
        len(lines[index].text) == 1 or lines[index].text[1] in " \t"
    ):
        return _parse_sequence(lines, index, indent, source)
    return _parse_mapping(lines, index, indent, source)


def parse_yaml(text: str, source: str) -> object:
    """Parse the YAML subset GitHub workflow files use, or raise `YamlError`."""
    lines = _scan_lines(text, source)
    start = _significant(lines, 0)
    if start < len(lines) and lines[start].text.strip() == "---":
        start += 1
    if any(line.text.strip() == "---" for line in lines[start:]):
        raise YamlError(f"{source}: multi-document streams are not supported")
    value, index = _parse_node(lines, start, 0, source)
    index = _significant(lines, index)
    if index < len(lines):
        raise YamlError(f"{source}:{lines[index].number}: trailing content")
    return value


# ── Workflow model ────────────────────────────────────────────────────────────


@dataclass
class Step:
    workflow: str
    job: str
    name: str
    run: str | None
    uses: str | None
    disabled: bool

    @property
    def where(self) -> str:
        return f"{self.workflow}: {self.job} / {self.name}"


@dataclass
class Job:
    workflow: str
    ident: str
    disabled: bool
    calls: str | None
    steps: list[Step] = field(default_factory=list)


@dataclass
class Workflow:
    rel: str
    triggers: set[str]
    jobs: list[Job] = field(default_factory=list)


def _is_statically_false(condition: object) -> bool:
    """True only for a condition that can NEVER be true.

    `if: false` and `if: ${{ false }}` are disabled markers. Everything else —
    `needs.changes.outputs.docs == 'true'`, `env.RUN_CODE_PATH == 'true'` — CAN
    be true, so the step it guards is a real edge.
    """
    if condition is None:
        return False
    if condition is False:
        return True
    text = str(condition).strip()
    if text.startswith("${{") and text.endswith("}}"):
        text = text[3:-2].strip()
    return text.lower() in {"false", "0"}


def _require_mapping(value: object, source: str, what: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise YamlError(
            f"{source}: {what} must be a mapping, got {type(value).__name__}"
        )
    return value


def _triggers_of(document: dict[str, object], source: str) -> set[str]:
    if "on" not in document:
        raise YamlError(f"{source}: workflow has no `on:` trigger block")
    raw = document["on"]
    if raw is None:
        raise YamlError(f"{source}: empty `on:` trigger block")
    if isinstance(raw, str):
        names = {raw}
    elif isinstance(raw, list):
        names = {str(item) for item in raw}
    elif isinstance(raw, dict):
        names = set(raw)
    else:
        raise YamlError(f"{source}: unsupported `on:` block")
    unknown = sorted(names - KNOWN_TRIGGERS)
    if unknown:
        raise YamlError(
            f"{source}: unknown workflow trigger(s) {', '.join(unknown)}. Add them to "
            "KNOWN_TRIGGERS once their firing conditions are modelled; until then "
            "this checker will not claim to know whether the workflow runs."
        )
    return names


def _composite_run_steps(uses: str) -> list[str]:
    """`run:` bodies of a local composite action, so a gate moved into one is
    still an edge. A remote action (`actions/checkout@sha`) is opaque and
    contributes nothing."""
    if not uses.startswith("./.github/actions/"):
        return []
    action_dir = REPO_ROOT / uses[2:]
    for candidate in ("action.yml", "action.yaml"):
        path = action_dir / candidate
        if not path.is_file():
            continue
        rel = str(path.relative_to(REPO_ROOT))
        document = _require_mapping(parse_yaml(path.read_text(), rel), rel, "action")
        runs = document.get("runs")
        if not isinstance(runs, dict):
            return []
        steps = runs.get("steps")
        if not isinstance(steps, list):
            return []
        out: list[str] = []
        for step in steps:
            if isinstance(step, dict) and isinstance(step.get("run"), str):
                out.append(step["run"])
        return out
    raise YamlError(f"{uses}: local composite action has no action.yml")


def _load_workflow(path: Path) -> Workflow:
    rel = (
        str(path.relative_to(REPO_ROOT))
        if path.is_relative_to(REPO_ROOT)
        else str(path)
    )
    document = _require_mapping(parse_yaml(path.read_text(), rel), rel, "workflow")
    workflow = Workflow(rel=rel, triggers=_triggers_of(document, rel))
    jobs = _require_mapping(document.get("jobs"), rel, "`jobs:`")
    for ident, raw in jobs.items():
        body = _require_mapping(raw, rel, f"job `{ident}`")
        calls = body.get("uses")
        job = Job(
            workflow=rel,
            ident=ident,
            disabled=_is_statically_false(body.get("if")),
            calls=calls if isinstance(calls, str) else None,
        )
        steps = body.get("steps")
        if steps is None and job.calls is None:
            raise YamlError(f"{rel}: job `{ident}` has neither `steps:` nor `uses:`")
        if steps is not None and not isinstance(steps, list):
            raise YamlError(f"{rel}: job `{ident}` has a non-list `steps:`")
        for index, raw_step in enumerate(steps or []):
            body_step = _require_mapping(
                raw_step, rel, f"step {index} of job `{ident}`"
            )
            run = body_step.get("run")
            uses = body_step.get("uses")
            if run is not None and not isinstance(run, str):
                raise YamlError(
                    f"{rel}: job `{ident}` step {index} has a non-string `run:`"
                )
            if run is None and uses is None:
                raise YamlError(
                    f"{rel}: job `{ident}` step {index} has neither `run:` nor `uses:`"
                )
            job.steps.append(
                Step(
                    workflow=rel,
                    job=ident,
                    name=str(body_step.get("name") or uses or f"step {index}"),
                    run=run,
                    uses=uses if isinstance(uses, str) else None,
                    disabled=job.disabled or _is_statically_false(body_step.get("if")),
                )
            )
        workflow.jobs.append(job)
    return workflow


def workflow_files() -> list[Path]:
    return sorted(WORKFLOW_DIR.glob("*.yml")) + sorted(WORKFLOW_DIR.glob("*.yaml"))


def load_workflows() -> list[Workflow]:
    return [_load_workflow(path) for path in workflow_files()]


def triggerable(workflows: list[Workflow]) -> list[Workflow]:
    """Workflows that can actually start.

    A `workflow_call`-only workflow runs only when another workflow that itself
    runs calls it; the closure below resolves that. A workflow no trigger can
    fire is not CI, and a gate whose only invocation lives there is not run.
    """
    live = {w.rel for w in workflows if w.triggers & SELF_FIRING_TRIGGERS}
    changed = True
    while changed:
        changed = False
        for workflow in workflows:
            if workflow.rel in live:
                continue
            for caller in workflows:
                if caller.rel not in live:
                    continue
                for job in caller.jobs:
                    if job.disabled or not job.calls:
                        continue
                    target = job.calls.lstrip("./")
                    if workflow.rel.endswith(target) or target.endswith(workflow.rel):
                        live.add(workflow.rel)
                        changed = True
    return [w for w in workflows if w.rel in live]


def strip_shell_comments(script: str) -> str:
    """Drop `#` comments from a shell/PowerShell body.

    One level below the YAML fix and the same defect: `# make foo` inside a
    `run:` script is a note about a gate, not a step that runs it. A `#` counts
    as a comment when it starts a word outside quotes; `${FOO#bar}` and
    `"a#b"` are left alone.
    """
    out: list[str] = []
    for line in script.splitlines():
        result = ""
        quote = ""
        index = 0
        while index < len(line):
            char = line[index]
            if quote:
                if char == quote:
                    quote = ""
                result += char
                index += 1
                continue
            if char in "'\"":
                quote = char
                result += char
                index += 1
                continue
            if char == "#" and (index == 0 or line[index - 1] in " \t"):
                break
            result += char
            index += 1
        out.append(result)
    return "\n".join(out)


def ci_step_commands(workflows: list[Workflow]) -> list[tuple[str, str]]:
    """(where, command text) for every CI step that can run and executes shell.

    This is THE definition of "CI runs this" for every axis below: a runnable
    step of a triggerable workflow, comment-stripped, plus the `run:` bodies of
    any local composite action it uses.
    """
    out: list[tuple[str, str]] = []
    for workflow in triggerable(workflows):
        for job in workflow.jobs:
            for step in job.steps:
                if step.disabled:
                    continue
                if step.run is not None:
                    out.append(
                        (step.where, executing_text(strip_shell_comments(step.run)))
                    )
                if step.uses:
                    for body in _composite_run_steps(step.uses):
                        out.append(
                            (step.where, executing_text(strip_shell_comments(body)))
                        )
    return out


# ── Makefile ──────────────────────────────────────────────────────────────────

RULE_RE = re.compile(r"^([A-Za-z0-9_./%-]+(?:\s+[A-Za-z0-9_./%-]+)*)\s*:(?!=)\s*(.*)$")

# ── Variable expansion ────────────────────────────────────────────────────────
#
# Reading the Makefile as literal text hides edges behind names. When the
# archive freshness work made the link prerequisites a bundle,
#
#   LIBHEW_READY := $(LIBHEW) | check-libhew-fresh
#   observe-functional-test: hew-native observe $(LIBHEW_READY)
#
# the prerequisite list of a CI-reached target became the seven characters
# `$(LIBHEW_READY)`, and `check-libhew-fresh` — which make runs every time that
# target is built — vanished from the graph this checker walks. The gate then
# demanded a direct `make check-libhew-fresh` step, which would run the check
# a second time for no reason. The edge was always there; the reader could not
# see it.
#
# So expand. Not by implementing make — by inlining exactly the assignments
# that can be inlined without guessing, and leaving every other reference
# standing verbatim:
#
#   * one top-level `=` or `:=` assignment of the name, and no other;
#   * a value built only from literal text, `$$`, and further `$(NAME)`
#     references.
#
# A name assigned inside an `ifeq`, defaulted with `?=` (the environment
# outranks it), appended to with `+=`, or bound to a `$(shell …)`/`$(if …)`
# call is NOT inlined, and neither is anything that references it. That is the
# fail-closed direction: an unexpanded reference matches no target name and no
# command shape, so it can only ever cost reachability, never grant it.
#
# The result is also a normal form. `$(LIBHEW)` reduces to
# `$(CARGO_NATIVE_OUT)/debug/$(LIBHEW_NAME)` — still opaque at both ends, but
# reduced identically wherever it appears, so a prerequisite and a recipe that
# name the same artefact become the same token.

VARIABLE_REF_RE = re.compile(r"\$[({]([A-Za-z_][A-Za-z0-9_]*)[)}]")

ASSIGNMENT_RE = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*)\s*(::=|:=|\+=|\?=|!=|=)\s*(.*)$")

INLINABLE_OPERATORS = {"=", ":=", "::="}

CONDITIONAL_OPEN = ("ifeq", "ifneq", "ifdef", "ifndef")


def _has_unmodelled_expansion(value: str) -> bool:
    """True when `value` holds a `$` construct this expander refuses to model.

    Only `$$` and a bare `$(NAME)` / `${NAME}` reference are modelled. A
    function call, an automatic variable, a computed name — anything else —
    disqualifies the whole assignment rather than being silently dropped from
    the middle of a value, which would fabricate a path that make never uses.
    """
    rest = value
    while True:
        index = rest.find("$")
        if index < 0:
            return False
        tail = rest[index:]
        if tail.startswith("$$"):
            rest = tail[2:]
            continue
        match = VARIABLE_REF_RE.match(tail)
        if not match:
            return True
        rest = tail[match.end() :]


def _logical_lines(text: str) -> list[str]:
    """`text` with backslash continuations joined, so an assignment is one line."""
    out: list[str] = []
    pending = ""
    for line in text.splitlines():
        if line.endswith("\\"):
            pending += line[:-1]
            continue
        out.append(pending + line)
        pending = ""
    if pending:
        out.append(pending)
    return out


def makefile_variables(text: str) -> dict[str, str]:
    """The variable assignments this reader is willing to inline."""
    values: dict[str, str] = {}
    rejected: set[str] = set()
    depth = 0
    for line in _logical_lines(text):
        if line.startswith("\t"):
            continue
        bare = line.split("#", 1)[0].strip()
        if not bare:
            continue
        head = bare.split(None, 1)[0]
        if head in CONDITIONAL_OPEN:
            depth += 1
            continue
        if head == "endif":
            depth = max(0, depth - 1)
            continue
        if head in {"else", "define", "endef", "export", "unexport", "override"}:
            # `else` may carry an `ifeq` of its own; either way the branch body
            # that follows is conditional, and `define`/`export` are shapes this
            # reader does not model.
            continue
        match = ASSIGNMENT_RE.match(bare)
        if not match:
            continue
        name, operator, value = match.group(1), match.group(2), match.group(3).strip()
        if (
            name in values
            or depth > 0
            or operator not in INLINABLE_OPERATORS
            or _has_unmodelled_expansion(value)
        ):
            rejected.add(name)
            continue
        values[name] = value
    for name in rejected:
        values.pop(name, None)
    return values


def expand_makefile_text(
    text: str, variables: dict[str, str], seen: frozenset[str] = frozenset()
) -> str:
    """`text` with every inlinable variable reference replaced by its value.

    `seen` breaks reference cycles: a name already being expanded is left
    standing rather than recursed into.
    """
    out: list[str] = []
    rest = text
    while rest:
        index = rest.find("$")
        if index < 0:
            out.append(rest)
            break
        out.append(rest[:index])
        tail = rest[index:]
        if tail.startswith("$$"):
            out.append("$$")
            rest = tail[2:]
            continue
        match = VARIABLE_REF_RE.match(tail)
        name = match.group(1) if match else ""
        if match and name in variables and name not in seen:
            out.append(expand_makefile_text(variables[name], variables, seen | {name}))
            rest = tail[match.end() :]
            continue
        out.append(tail[:1])
        rest = tail[1:]
    return "".join(out)


MAKE_CONDITIONAL_RE = re.compile(r"^(ifeq|ifneq|ifdef|ifndef|else|endif)\b")


def parse_makefile(text: str) -> tuple[set[str], dict[str, set[str]], dict[str, str]]:
    """Return (phony targets, target → prerequisites, target → recipe text).

    Rule lines and recipes are read through `expand_makefile_text`, so a
    prerequisite list or a build-artefact path written as a variable is seen as
    the graph make sees. Recipe text is comment-stripped for the same reason
    workflow bodies are: a commented-out `$(MAKE) foo` in a recipe is not a
    call.

    An order-only `|` separator is left in the prerequisite list, where it
    matches no target name and is dropped by the callers' `in known` filter.
    Order-only says WHEN a prerequisite is brought up to date, not WHETHER, so
    it makes no difference to reachability.
    """
    variables = makefile_variables(text)
    phony: set[str] = set()
    prereqs: dict[str, set[str]] = {}
    recipes: dict[str, str] = {}
    current: list[str] = []
    for raw in text.splitlines():
        if raw.startswith("\t"):
            line = expand_makefile_text(raw, variables)
            for tgt in current:
                recipes[tgt] = recipes.get(tgt, "") + strip_shell_comments(line) + "\n"
            continue
        if raw.startswith(".PHONY:"):
            phony.update(expand_makefile_text(raw[len(".PHONY:") :], variables).split())
            current = []
            continue
        stripped = raw.split("#", 1)[0].rstrip()
        if not stripped or stripped[0].isspace():
            if not stripped:
                current = []
            continue
        # A conditional directive written at column 0 inside a rule does not end
        # the rule — make evaluates it and the recipe lines on either arm still
        # belong to the target above. Reading it as the end of the rule loses
        # every recipe guarded by a host check, which is how `asan-fixtures` and
        # `tsan` read as having no recipe at all.
        if MAKE_CONDITIONAL_RE.match(stripped):
            continue
        match = RULE_RE.match(expand_makefile_text(stripped, variables))
        if not match:
            current = []
            continue
        current = match.group(1).split()
        deps = set(match.group(2).split())
        for tgt in current:
            prereqs.setdefault(tgt, set()).update(deps)
            recipes.setdefault(tgt, "")
    return phony, prereqs, recipes


# ── Roots: what CI invokes directly ───────────────────────────────────────────

MAKE_INVOKE_RE = re.compile(
    r"(?<![\w-])g?make[^\S\n]+((?:[A-Za-z0-9_.-]+[^\S\n]+)*[A-Za-z0-9_.-]+)"
)


# Commands that print their arguments instead of running them. `echo "run make
# foo"` names a target without invoking it — the same class of false edge as a
# comment, one layer further in.
_NON_EXECUTING_RE = re.compile(r"^@?-?(echo|printf|:)\b")


def executing_text(script: str) -> str:
    """`script` with the pure-output commands removed."""
    return "\n".join(
        segment
        for segment in _command_segments(script)
        if not _NON_EXECUTING_RE.match(_strip_keywords(" ".join(segment.split())))
    )


def make_targets_in(text: str, known: set[str]) -> set[str]:
    """Every known target named by a `make`/`gmake` invocation in `text`.

    Multi-target invocations (`make verify-ffi test-verify-ffi`) count for each
    named target; trailing VAR=value arguments are not targets and are dropped
    by the `known` filter. The separator class excludes newlines: `make lint`
    followed by `make test` on the next line is two invocations, and a pattern
    that let `\\s` swallow the break would read the second `make` as an
    argument of the first and stop scanning there.
    """
    found: set[str] = set()
    for match in MAKE_INVOKE_RE.finditer(text):
        for word in match.group(1).split():
            if word in known:
                found.add(word)
            else:
                break
    return found


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
        recipe = executing_text(recipes.get(target, ""))
        for match in re.finditer(r"\$\(MAKE\)\s+([A-Za-z0-9_.-]+)", recipe):
            nxt.add(match.group(1))
        nxt |= make_targets_in(recipe, known)
        for dep in nxt:
            if dep in known and dep not in reached:
                reached.add(dep)
                frontier.append(dep)
    return reached


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
    step_commands: list[tuple[str, str]], recipes: dict[str, str], reached: set[str]
) -> list[str]:
    """Text of every test invocation CI can reach: the commands of runnable CI
    steps plus the recipes of the Makefile targets CI reaches."""
    return [command for _, command in step_commands] + [
        executing_text(recipes.get(t, "")) for t in sorted(reached)
    ]


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


# ── The nextest filterset grammar ─────────────────────────────────────────────
#
# `default-filter` is a boolean expression, not a list of `- term()` suffixes.
# The previous regex (`-\s*(binary|package|test)\(([^)]+)\)`) read only terms
# immediately after a `-`, so the leading `not package(hew-wasm)` of
#
#   not package(hew-wasm) - binary(parity) - binary(parity_ratchet) …
#
# was invisible and the gate reported "4/4 exclusions compensated" over a
# five-exclusion filter. A pattern cannot honestly cover this grammar: `not`,
# `!`, `-`, `and`/`&`, `or`/`|`/`+`, `^`, parentheses and nesting all change
# which tests are subtracted. So parse it, and refuse anything whose subtracted
# set this checker cannot name.


@dataclass
class FilterAtom:
    kind: str
    value: str

    def __str__(self) -> str:
        return f"{self.kind}({self.value})"


@dataclass
class FilterNot:
    operand: object


@dataclass
class FilterBinary:
    op: str
    left: object
    right: object


class FiltersetError(Exception):
    """A filterset expression this checker will not guess about."""


_FILTER_TOKEN_RE = re.compile(
    r"""
      (?P<space>\s+)
    | (?P<call>(?:all|none|test|binary|binary_id|package|deps|rdeps|kind|platform)\s*\()
    | (?P<word>[A-Za-z_][A-Za-z0-9_]*)
    | (?P<op>[()!&|+^-])
    """,
    re.VERBOSE,
)


def _tokenize_filterset(text: str) -> list[tuple[str, str]]:
    tokens: list[tuple[str, str]] = []
    index = 0
    while index < len(text):
        match = _FILTER_TOKEN_RE.match(text, index)
        if not match:
            raise FiltersetError(
                f"unexpected character at offset {index}: {text[index]!r}"
            )
        index = match.end()
        if match.lastgroup == "space":
            continue
        if match.lastgroup == "call":
            name = match.group().rstrip("( \t")
            depth = 1
            start = index
            while index < len(text) and depth:
                if text[index] == "(":
                    depth += 1
                elif text[index] == ")":
                    depth -= 1
                index += 1
            if depth:
                raise FiltersetError(f"unterminated `{name}(`")
            tokens.append(("call", f"{name}:{text[start : index - 1]}"))
            continue
        if match.lastgroup == "word":
            word = match.group().lower()
            if word not in {"and", "or", "not"}:
                raise FiltersetError(f"unknown bare word `{match.group()}`")
            tokens.append(("op", {"and": "&", "or": "|", "not": "!"}[word]))
            continue
        tokens.append(("op", match.group()))
    return tokens


def _parse_filterset(
    tokens: list[tuple[str, str]], pos: int, min_prec: int
) -> tuple[object, int]:
    precedence = {"|": 1, "+": 1, "^": 2, "&": 3, "-": 3}
    kind, value = tokens[pos]
    if kind == "op" and value == "!":
        operand, pos = _parse_filterset(tokens, pos + 1, 4)
        node: object = FilterNot(operand)
    elif kind == "op" and value == "(":
        node, pos = _parse_filterset(tokens, pos + 1, 0)
        if pos >= len(tokens) or tokens[pos] != ("op", ")"):
            raise FiltersetError("unbalanced `(`")
        pos += 1
    elif kind == "call":
        name, _, argument = value.partition(":")
        node = FilterAtom(name, argument.strip())
        pos += 1
    else:
        raise FiltersetError(f"expected an expression, found `{value}`")
    while pos < len(tokens):
        kind, value = tokens[pos]
        if kind != "op" or value not in precedence or precedence[value] < min_prec:
            break
        right, pos = _parse_filterset(tokens, pos + 1, precedence[value] + 1)
        node = FilterBinary(value, node, right)
    return node, pos


def parse_filterset(text: str) -> object:
    tokens = _tokenize_filterset(text)
    if not tokens:
        raise FiltersetError("empty filter expression")
    node, pos = _parse_filterset(tokens, 0, 0)
    if pos != len(tokens):
        raise FiltersetError(
            f"trailing tokens after a complete expression: {tokens[pos][1]!r}"
        )
    return node


def _union_atoms(node: object) -> list[FilterAtom]:
    """Atoms of a union-only subtree; anything else has no single subtracted set."""
    if isinstance(node, FilterAtom):
        if node.kind in {"all", "none"}:
            raise FiltersetError(f"`{node.kind}()` cannot be attributed to a test set")
        return [node]
    if isinstance(node, FilterBinary) and node.op in {"|", "+"}:
        return _union_atoms(node.left) + _union_atoms(node.right)
    raise FiltersetError(
        "a negated sub-expression this checker cannot decompose into the exact "
        "set it subtracts"
    )


def filterset_exclusions(text: str) -> list[FilterAtom]:
    """The selectors a default-filter SUBTRACTS, or raise.

    Accepts only the shape whose subtracted set is unambiguous: a conjunction
    (`&`, `and`, `-`) whose terms are `all()` or negations. `package(hew-cli)`
    as a whole filter also excludes almost everything, but naming what it
    subtracts means enumerating every other package — so it is refused, loudly,
    rather than reported as "no exclusions".
    """
    root = parse_filterset(text)
    terms: list[tuple[object, bool]] = []

    def flatten(node: object, negated: bool) -> None:
        if isinstance(node, FilterBinary) and node.op in {"&", "-"}:
            flatten(node.left, negated)
            flatten(node.right, negated or node.op == "-")
            return
        terms.append((node, negated))

    flatten(root, False)
    exclusions: list[FilterAtom] = []
    for node, negated in terms:
        if isinstance(node, FilterAtom) and node.kind == "all" and not negated:
            continue
        if negated and not isinstance(node, FilterNot):
            exclusions.extend(_union_atoms(node))
            continue
        if isinstance(node, FilterNot) and not negated:
            exclusions.extend(_union_atoms(node.operand))
            continue
        raise FiltersetError(
            f"term `{node}` selects rather than subtracts; this checker cannot "
            "name the tests such a filter removes. Express the exclusions as "
            "negated terms, or teach the checker this shape — do not leave it "
            "reporting a smaller exclusion set than the file has."
        )
    return exclusions


def profile_ci_exclusions() -> list[FilterAtom]:
    text = NEXTEST_TOML.read_text()
    section = re.search(r"^\[profile\.ci\]$(.*?)^\[", text, re.S | re.M)
    if not section:
        raise SystemExit("error: could not locate [profile.ci] in .config/nextest.toml")
    filt = re.search(r'^default-filter\s*=\s*"([^"]*)"', section.group(1), re.M)
    if not filt:
        raise SystemExit("error: [profile.ci] has no default-filter to check")
    try:
        return filterset_exclusions(filt.group(1))
    except FiltersetError as error:
        raise SystemExit(
            f"error: [profile.ci] default-filter {filt.group(1)!r}: {error}"
        ) from error


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


# ── A3d: inline `-E` filter exclusions ────────────────────────────────────────
#
# Two defects fixed here. The old `-E\s+'([^']*)'` saw only single-quoted
# filters, so `-E "not test(x)"` or a bare `-E not-a-quote` was silently no
# filter at all; and the old witness accepted ANY run containing `--workspace`,
# so a run that excluded the very crate being filtered "compensated" it. Both
# let A3d report compensated while the tests ran nowhere — the class this axis
# exists to detect. So: tokenise the command properly, and require an
# unfiltered run PER PACKAGE of the filtered run's scope.


@dataclass
class CargoInvocation:
    """One `cargo …` command, tokenised.

    Every `-E` spelling nextest accepts is recognised — `-E expr`, `-E 'expr'`,
    `-E "expr"`, `-E=expr`, `--filter-expr`, repeated — because the tokeniser is
    a shell tokeniser rather than a quote-shaped regex. The old
    `-E\\s+'([^']*)'` saw single quotes only, so `-E "not test(x)"` read as no
    filter at all.
    """

    where: str
    command: str
    subcommand: str
    workspace: bool
    packages: set[str]
    excludes: set[str]
    expr_filters: list[str]
    name_filters: list[str]
    tokens: list[str]
    probe: bool

    @property
    def filters(self) -> list[str]:
        return self.expr_filters + self.name_filters

    @property
    def filtered(self) -> bool:
        return bool(self.filters)

    def scope(self, all_crates: list[str]) -> set[str]:
        """Packages this command runs. A bare `cargo test` at a virtual
        workspace root runs the whole workspace, same as `--workspace`."""
        base = (
            set(all_crates)
            if self.workspace or not self.packages
            else set(self.packages)
        )
        return base - self.excludes


TEST_SUBCOMMANDS = {"nextest run", "test", "llvm-cov nextest", "llvm-cov test"}
NEXTEST_SUBCOMMANDS = {"nextest run", "llvm-cov nextest"}
# Flags that change only how cargo REPORTS, never what it checks or runs. A
# containment proof may ignore these and nothing else.
OUTPUT_ONLY_FLAGS = ("--message-format", "--color", "--quiet", "-q", "--verbose")

# Flags that consume the next token. Anything else that looks like a flag is
# treated as boolean — and if an UNKNOWN long flag is followed by a bare word,
# the checker stops rather than guess, because that word is either a value it
# must skip or a test-name filter it must count, and the two answers disagree
# about whether the run is filtered.
_VALUE_FLAGS = {
    "--profile",
    "--test",
    "--bin",
    "--example",
    "--features",
    "--target",
    "--target-dir",
    "--config",
    "--config-file",
    "--manifest-path",
    "--output-path",
    "--output-dir",
    "--status-level",
    "--final-status-level",
    "--failure-output",
    "--success-output",
    "--run-ignored",
    "--ignore-filename-regex",
    "--cargo-profile",
    "--test-threads",
    "--retries",
    "--partition",
    "--message-format",
    "--color",
    "-j",
    "--jobs",
    "-E",
    "--filter-expr",
    "--filter-expression",
    "-p",
    "--package",
    "--exclude",
}

# Long flags that take no value; every other unknown long flag is ambiguous.
_BOOLEAN_FLAGS = {
    "--workspace",
    "--all",
    "--all-targets",
    "--all-features",
    "--no-default-features",
    "--no-fail-fast",
    "--no-run",
    "--no-capture",
    "--nocapture",
    "--lib",
    "--tests",
    "--benches",
    "--examples",
    "--doc",
    "--release",
    "--locked",
    "--offline",
    "--frozen",
    "--verbose",
    "--quiet",
    "--lcov",
    "--html",
    "--json",
    "--summary-only",
    "--no-report",
    "--branch",
    "--version",
    "--help",
    "--hide-progress-bar",
    "--ignore-default-filter",
}

CARGO_CMD_RE = re.compile(r"(?<![\w./-])cargo(?![\w-])")
_SEGMENT_SPLIT_RE = re.compile(r"&&|\|\||[;\n]")


def _command_segments(script: str) -> list[str]:
    """Split a shell body into the individual commands it runs."""
    return [
        segment.strip()
        for segment in _SEGMENT_SPLIT_RE.split(script.replace("\\\n", " "))
        if segment.strip()
    ]


def parse_cargo_command(where: str, segment: str) -> CargoInvocation | None:
    """Model one `cargo` command, or fail closed.

    Returns None when the segment runs no cargo at all. Raises when it does but
    cannot be tokenised: a command this checker cannot read is a command whose
    filters and package scope it cannot check, and guessing is how a filtered
    run gets certified as unfiltered.
    """
    if not CARGO_CMD_RE.search(segment):
        return None
    try:
        tokens = shlex.split(segment, comments=False)
    except ValueError as error:
        raise SystemExit(
            f"error: {where}: cannot tokenise a cargo command ({error}): {segment!r}. "
            "A command this checker cannot read is a command whose filters it "
            "cannot check."
        ) from error
    try:
        start = next(i for i, t in enumerate(tokens) if t == "cargo")
    except StopIteration:
        return None
    index = start + 1
    if index < len(tokens) and tokens[index].startswith("+"):
        index += 1
    if index >= len(tokens):
        return None
    subcommand = tokens[index]
    index += 1
    if subcommand in {"nextest", "llvm-cov"} and index < len(tokens):
        if tokens[index] in {"run", "test", "nextest"}:
            subcommand = f"{subcommand} {tokens[index]}"
            index += 1
    invocation = CargoInvocation(
        where=where,
        command=segment,
        subcommand=subcommand,
        workspace=False,
        packages=set(),
        excludes=set(),
        expr_filters=[],
        name_filters=[],
        tokens=tokens,
        probe="--version" in tokens or "--help" in tokens,
    )
    saw_double_dash = False
    while index < len(tokens):
        token = tokens[index]
        index += 1
        if token == "--":
            saw_double_dash = True
            continue
        if saw_double_dash:
            continue
        if token in {"-E", "--filter-expr", "--filter-expression"}:
            if index >= len(tokens):
                raise SystemExit(f"error: {where}: `{token}` with no expression")
            invocation.expr_filters.append(tokens[index])
            index += 1
            continue
        if token.startswith(("-E=", "--filter-expr=", "--filter-expression=")):
            invocation.expr_filters.append(token.split("=", 1)[1])
            continue
        if token in {"-p", "--package"}:
            invocation.packages.add(tokens[index])
            index += 1
            continue
        if token.startswith(("-p=", "--package=")):
            invocation.packages.add(token.split("=", 1)[1])
            continue
        if token == "--exclude":
            invocation.excludes.add(tokens[index])
            index += 1
            continue
        if token.startswith("--exclude="):
            invocation.excludes.add(token.split("=", 1)[1])
            continue
        if token in {"--workspace", "--all"}:
            invocation.workspace = True
            continue
        if token.startswith("-"):
            if token in _VALUE_FLAGS:
                index += 1
                continue
            if (
                token.startswith("--")
                and "=" not in token
                and token not in _BOOLEAN_FLAGS
                and index < len(tokens)
                and not tokens[index].startswith("-")
            ):
                raise SystemExit(
                    f"error: {where}: unclassified flag `{token}` followed by "
                    f"`{tokens[index]}` in {segment!r}. Add it to _VALUE_FLAGS or "
                    "_BOOLEAN_FLAGS: whether that word is a value or a test-name "
                    "filter decides whether this run counts as filtered, and "
                    "guessing is how a filtered run gets certified as unfiltered."
                )
            continue
        # A bare positional is a test-name filter: it narrows the run exactly
        # like `-E` does, so it must be treated as one.
        invocation.name_filters.append(token)
    return invocation


def cargo_commands_in(where: str, script: str) -> list[CargoInvocation]:
    out: list[CargoInvocation] = []
    for segment in _command_segments(script):
        invocation = parse_cargo_command(where, segment)
        if invocation is not None:
            out.append(invocation)
    return out


def nextest_runs(
    step_commands: list[tuple[str, str]], recipes: dict[str, str], reached: set[str]
) -> list[CargoInvocation]:
    """Every nextest invocation CI can reach — workflow steps AND the recipes of
    CI-reached Makefile targets. `make test-cabi` is a real unfiltered run of
    hew-cabi; a compensation check that only looked at workflow bodies would
    miss it and demand a duplicate step."""
    sources = list(step_commands)
    sources += [
        (f"Makefile: {target}", recipes.get(target, "")) for target in sorted(reached)
    ]
    runs: list[CargoInvocation] = []
    for where, script in sources:
        for invocation in cargo_commands_in(where, script):
            if invocation.subcommand in NEXTEST_SUBCOMMANDS and not invocation.probe:
                runs.append(invocation)
    return runs


def uncompensated_packages(
    run: CargoInvocation, runs: list[CargoInvocation], all_crates: list[str]
) -> list[str]:
    """Packages the filtered `run` narrows that NO unfiltered CI run executes.

    Per package, not per command: `--workspace --exclude hew-cabi` genuinely
    compensates every package except hew-cabi, and `make test-cabi` compensates
    that one. What is never accepted is a "witness" that excludes the very
    package it is supposed to vouch for — the old check took any `--workspace`
    run, `--exclude`s and all, which let A3d certify a filter while the tests it
    subtracts ran nowhere.
    """
    covered: set[str] = set()
    for other in runs:
        if other is run or other.filtered:
            continue
        covered |= other.scope(all_crates)
    return sorted(run.scope(all_crates) - covered)


# ── Containment: proving a target CI never names is nevertheless run ──────────
#
# `make test` and `make lint` are local entry points that fan out to work CI
# does in pieces — CI cannot run `make test` verbatim,
# because its workspace run has no `--exclude hew-cabi` and that crate's
# cfg(test) symbols collide with hew-runtime's at link time. Deleting the
# developer entry point is not the answer, and neither is a waiver.
#
# So: a PROOF, uniform and mechanical. A target no CI step invokes is reached
# only when every prerequisite is reached AND every command in its recipe is
# one CI already runs. Anything the rules below cannot classify leaves the
# target unreached — `lint-wasm-todo`'s Python validator
# is not provable by any of them, which is exactly why it had to be wired into
# a workflow rather than argued about.

# Shell keywords that can prefix a real command (`if cargo …`, `then cargo …`).
# They are stripped so the command behind them is judged on its own merits: a
# blanket `^if .*` rule would class `if cargo nextest run -E weird; then` as
# harmless scaffolding, which is the shape of the very hole this file exists to
# close.
_LEADING_KEYWORDS = ("if", "elif", "while", "until", "then", "else", "do", "!")

# Commands that assert nothing about the code under test: shell bookkeeping,
# progress output, and control-flow scaffolding.
SCAFFOLDING_RE = re.compile(
    r"""^@?-?(
          set\s+[-+]\w+
        | echo\b.*
        | printf\b.*
        | true | false | :
        | fi | done | esac
        | for\s+[A-Za-z_][A-Za-z0-9_]*\s+in\s+[^`$]*
        | command\s+-v\b.*
        | cd\s+\S+
        )$""",
    re.VERBOSE | re.S,
)

# `test -f <path>` where <path> is one of the target's own PREREQUISITES: the
# recipe is checking that an artefact make was asked to bring up to date is on
# disk. That asserts the build ran, not that any behaviour holds — the
# surrounding cargo commands carry the verdict.
#
# The prerequisite is the whole of the licence. An earlier version accepted any
# path under `target/`, which was a guess at "build artefact" that stopped
# working the moment the output directory moved behind $(CARGO_NATIVE_OUT); the
# build graph knows the answer exactly, so ask it. A `test -f` on a path the
# target never declared stays unclassified, and the target stays a wire-or-cut
# decision.
ARTEFACT_PRECONDITION_RE = re.compile(r"^@?-?test\s+-[efxdsr]\s+(\S+)$")


def _strip_keywords(text: str) -> str:
    words = text.split()
    while words and words[0].strip("@-") in _LEADING_KEYWORDS:
        words.pop(0)
    return " ".join(words)


def _covers_flags(inner: CargoInvocation, outer: CargoInvocation) -> bool:
    """Every significant flag of `inner` also appears in `outer`.

    Output-only flags are ignored on the OUTER side only: CI may render clippy
    as JSON for SARIF, but it may not quietly drop `-D warnings` or add an
    `--exclude` the inner command does not have.
    """
    inner_flags = [t for t in inner.tokens if t.startswith("-")]
    outer_flags = {t for t in outer.tokens if t.startswith("-")}
    if any(
        flag not in outer_flags and not flag.startswith(OUTPUT_ONLY_FLAGS)
        for flag in inner_flags
    ):
        return False
    return not outer.excludes - inner.excludes


def _command_is_covered(
    segment: str,
    where: str,
    reached: set[str],
    known: set[str],
    ci_cargo: list[CargoInvocation],
    blobs: list[str],
    all_crates: list[str],
    excluded_binaries: set[str],
    artefacts: frozenset[str] | set[str] = frozenset(),
) -> bool:
    text = _strip_keywords(" ".join(segment.split()))
    if not text or SCAFFOLDING_RE.match(text):
        return True
    precondition = ARTEFACT_PRECONDITION_RE.match(text)
    if precondition and precondition.group(1).strip("\"'") in artefacts:
        return True
    targets = make_targets_in(text, known) | {
        m.group(1) for m in re.finditer(r"\$\(MAKE\)\s+([A-Za-z0-9_.-]+)", text)
    }
    if targets:
        return targets <= reached
    invocation = parse_cargo_command(where, text)
    if invocation is None:
        return False
    if invocation.probe:
        return True
    if not text.lstrip("@").startswith("cargo"):
        # A leading `VAR=value` (a sanitizer RUSTFLAGS, a redirected target dir)
        # changes what the command proves. Fail closed.
        return False
    if invocation.subcommand in TEST_SUBCOMMANDS:
        # A test selection is contained when CI runs every package it names.
        # Narrowing (`--test bin`, a test-name filter) only shrinks the set —
        # unless the binary it names is one profile.ci subtracts, in which case
        # the CI run does NOT include it and there is nothing to contain.
        for index, token in enumerate(invocation.tokens):
            if token == "--test" and index + 1 < len(invocation.tokens):
                if invocation.tokens[index + 1] in excluded_binaries:
                    return False
        return all(
            crate_covered(crate, blobs) for crate in invocation.scope(all_crates)
        )
    if invocation.subcommand in {"clippy", "fmt"}:
        return any(
            other.subcommand == invocation.subcommand
            and (
                other.workspace
                or not other.packages
                or invocation.packages <= other.packages
            )
            and _covers_flags(invocation, other)
            for other in ci_cargo
        )
    return False


def prove_contained(
    target: str,
    prereqs: dict[str, set[str]],
    recipes: dict[str, str],
    reached: set[str],
    known: set[str],
    ci_cargo: list[CargoInvocation],
    blobs: list[str],
    all_crates: list[str],
    excluded_binaries: set[str],
) -> bool:
    # Gate-shaped prerequisites must themselves be reached. Build prerequisites
    # (`stdlib`, `runtime`, `wasm-runtime`) are out of A1's scope for the same
    # reason they are not gates: they produce artefacts, not verdicts.
    gate_prereqs = {
        prereq
        for prereq in prereqs.get(target, set())
        if prereq in known and GATE_NAME_RE.match(prereq)
    }
    if not gate_prereqs <= reached:
        return False
    for segment in _command_segments(recipes.get(target, "")):
        if not _command_is_covered(
            segment,
            f"Makefile: {target}",
            reached,
            known,
            ci_cargo,
            blobs,
            all_crates,
            excluded_binaries,
            prereqs.get(target, set()),
        ):
            return False
    return True


# ── A4: every documented `make <target>` names a target that exists ───────────
#
# A0..A3 all prove the same direction: CI -> Makefile. Nothing proved the
# reverse, docs -> Makefile, so deleting a target left every documented
# invocation of it dangling in silence. An invocation of `test-all` sat in the
# CONTRIBUTING test-suite table after the target was gone, and the first person
# to find out would have been a new contributor typing it and getting `No rule to
# make target`. A gate that can only see one direction of an edge is exactly how
# that stayed invisible, so this checks the other one.
#
# Sources are the tracked text that routes a human to a command: Markdown, and
# the build/CI script layer. Where `make` is EXECUTABLE (shell, Makefile,
# workflow bodies) both code and comments are read; where it can only ever be
# prose (Python, TOML) comments and backticked spans are, because a bare
# invocation inside a Python string is test data or a generated fixture, not an
# instruction to anybody. Rust sources are out of scope: they document code, and
# "make a second copy" is English, not a command.
#
# WHAT COUNTS AS A REFERENCE. Prose is full of the verb "make", so reading every
# `make <word>` as an invocation would bury the signal. Two rules, both about
# the shape of the text rather than which file it lives in:
#
#   * at COMMAND POSITION — the start of a Markdown code span or fenced line, or
#     after a shell separator in script code — every following target token is a
#     reference. That is a command a reader can copy and run.
#   * MID-PROSE — anywhere else, only the single next token, and only when it is
#     target-shaped (carries a hyphen). An invocation of test-hew written in the
#     middle of a sentence is an invocation; make a second copy is a sentence. A
#     commit subject quoted in backticks (`fix(build): make Windows source builds
#     link-ready`) is prose under this rule, which is what it is.
#
# ILLUSTRATIVE EXAMPLES. A doc showing the SHAPE of a command rather than a real
# target writes the target as a metavariable — `make <target>`, `make $(GATE)`,
# `make foo-%`. Anything carrying `<>`, `$`, `{}`, `%`, `[]` or `...` is not a
# name make could resolve either, so the checker skips it and the reader sees a
# placeholder instead of something that looks runnable and is not. That is the
# entire exemption mechanism, and it is a property of how the example is written,
# not a list of files allowed to be wrong. There is no skip list here for the
# same reason there is none in A1: the first file on it would be the one that
# needed fixing.
#
# EXTERNAL TARGETS. A command for another repository may name a target this
# Makefile cannot define. Mark that single command's line explicitly with
# `# external: owner/repo` or `<!-- external-target: owner/repo -->`; A4 then
# leaves its target to the named repository. The annotation is deliberately
# same-line and applies only to that line, so an unannotated unknown target
# remains a finding.
#
# THE RESIDUAL. A hyphenated English compound directly after the verb — "make
# distinct-but-equal keys collide" — is read as a target by the mid-prose rule.
# That is the price of catching an invocation nobody wrapped in backticks, and it
# is why the scan stops at the files where invocations actually live. When it
# does bite, the answer is to reword or backtick the sentence, never to exempt
# the file: an exemption would take that file's real invocations out of the check
# along with the false one.

# Where `make <target>` can be an executed command as well as documentation.
EXECUTABLE_SUFFIXES = (".sh", ".bash", ".yml", ".yaml", ".mk")
EXECUTABLE_NAMES = {"Makefile", "GNUmakefile"}
# Where it can only ever be prose: comments and backticked spans are read, the
# rest is data — a bare `make <target>` in a Python string is a generated
# fixture or test input, not an instruction to anybody.
COMMENT_ONLY_SUFFIXES = (".py", ".toml")

_FENCE_RE = re.compile(r"^\s{0,3}(?P<fence>`{3,}|~{3,})")
_INLINE_CODE_RE = re.compile(r"(?P<ticks>`+)(?P<body>[^\n]+?)(?P=ticks)")
_DOC_MAKE_RE = re.compile(r"(?<![\w./-])g?make(?=[^\S\n]+\S)")
_COMMAND_POSITION_RE = re.compile(r"(?:^|[|&;(]|\$\(|\bsudo\b|\benv\b)\s*$")
_COMMAND_END_RE = re.compile(r"[|&;\n)`]")
_METAVARIABLE_RE = re.compile(r"[<>${}%*\[\]]|\.\.\.")
_TARGET_TOKEN_RE = re.compile(r"^[A-Za-z0-9_.-]+$")
_TARGET_SHAPED_RE = re.compile(r"^[A-Za-z0-9_.]+-[A-Za-z0-9_.-]*$")
_EXTERNAL_TARGET_ANNOTATION_RE = re.compile(
    r"(?:#\s*external|<!--\s*external-target)\s*:\s*"
    r"[A-Za-z0-9][A-Za-z0-9._/-]*(?:\s*-->)?\s*$"
)


@dataclass(frozen=True)
class MakeReference:
    """A `make <target>` a reader is told to run, and where it is written."""

    target: str
    where: str


def tracked_files(root: Path = REPO_ROOT) -> list[str]:
    """Every path git tracks under `root`.

    Fail closed: an untracked working copy is not this checker's subject, and a
    git invocation that fails is an error rather than an empty file list that
    would report "nothing to check" and pass.
    """
    result = subprocess.run(
        ["git", "ls-files", "-z"],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
    )
    return [path for path in result.stdout.split("\0") if path]


def split_shell_comment(line: str) -> tuple[str, str]:
    """(code, comment) for one line, splitting at a `#` that starts a word.

    Same `#` rule as strip_shell_comments: inside quotes, or glued to the
    previous character as in `${FOO#bar}`, it is not a comment.
    """
    quote = ""
    for index, char in enumerate(line):
        if quote:
            if char == quote:
                quote = ""
            continue
        if char in "'\"":
            quote = char
            continue
        if char == "#" and (index == 0 or line[index - 1] in " \t"):
            return line[:index], line[index + 1 :]
    return line, ""


def markdown_chunks(text: str) -> list[tuple[int, str, bool]]:
    """(line number, chunk, prose) for the code a Markdown file shows a reader.

    Fenced blocks and inline code spans only. Prose outside them is prose: it
    can say "make sure the runtime is built" without naming a target, and
    reading it as a command is how a checker earns the reputation of crying
    wolf. Every real invocation in this tree is already written as code,
    because that is what documentation conventions are for.
    """
    chunks: list[tuple[int, str, bool]] = []
    fence = ""
    for number, line in enumerate(text.splitlines(), start=1):
        match = _FENCE_RE.match(line)
        if not fence:
            if match:
                fence = match.group("fence")[0] * 3
                continue
            for span in _INLINE_CODE_RE.finditer(line):
                chunks.append((number, span.group("body"), False))
        else:
            if match and match.group("fence")[0] * 3 == fence:
                fence = ""
                continue
            chunks.append((number, line, False))
    return chunks


def script_chunks(text: str, executable: bool) -> list[tuple[int, str, bool]]:
    """(line number, chunk, prose) for a script, Makefile or workflow file.

    A comment is read as prose, plus its backticked spans as code — the repo
    writes invocations in comments the same way its Markdown does. Code is read
    as code where `make` is something the file can actually run; where it is
    not, a backticked span is still a command a reader is being shown, so those
    are read wherever they appear.
    """
    chunks: list[tuple[int, str, bool]] = []
    for number, line in enumerate(text.splitlines(), start=1):
        code, comment = split_shell_comment(line)
        if executable and code.strip():
            chunks.append((number, code, False))
        elif not executable:
            for span in _INLINE_CODE_RE.finditer(code):
                chunks.append((number, span.group("body"), False))
        if comment:
            chunks.append((number, comment, True))
            for span in _INLINE_CODE_RE.finditer(comment):
                chunks.append((number, span.group("body"), False))
    return chunks


def make_references_in(chunk: str, prose: bool) -> list[str]:
    """Target names invoked by `make`/`gmake` in one chunk of text."""
    found: list[str] = []
    for match in _DOC_MAKE_RE.finditer(chunk):
        tail = _COMMAND_END_RE.split(chunk[match.end() :])[0]
        at_command_position = not prose and bool(
            _COMMAND_POSITION_RE.search(chunk[: match.start()])
        )
        for word in tail.split():
            # Options and `VAR=value` overrides sit between `make` and its
            # targets; neither is a target.
            if word.startswith("-") or "=" in word:
                continue
            word = word.rstrip(".,;:")
            if _METAVARIABLE_RE.search(word) or not _TARGET_TOKEN_RE.match(word):
                break
            if at_command_position:
                found.append(word)
                continue
            if _TARGET_SHAPED_RE.match(word):
                found.append(word)
            break
    return found


def documented_make_references(root: Path = REPO_ROOT) -> list[MakeReference]:
    """Every `make <target>` a tracked doc, script or workflow tells you to run."""
    seen: set[tuple[str, str]] = set()
    references: list[MakeReference] = []
    for path in tracked_files(root):
        name = Path(path).name
        if path.endswith(".md"):
            reader = markdown_chunks
        elif path.endswith(EXECUTABLE_SUFFIXES) or name in EXECUTABLE_NAMES:
            reader = lambda text: script_chunks(text, executable=True)  # noqa: E731
        elif path.endswith(COMMENT_ONLY_SUFFIXES):
            reader = lambda text: script_chunks(text, executable=False)  # noqa: E731
        else:
            continue
        try:
            text = (root / path).read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        external_target_lines = {
            number
            for number, line in enumerate(text.splitlines(), start=1)
            if _EXTERNAL_TARGET_ANNOTATION_RE.search(line)
        }
        for number, chunk, prose in reader(text):
            if number in external_target_lines:
                continue
            for target in make_references_in(chunk, prose):
                key = (target, f"{path}:{number}")
                if key not in seen:
                    seen.add(key)
                    references.append(MakeReference(target, key[1]))
    return references


# ── Main ──────────────────────────────────────────────────────────────────────


def main() -> int:
    verbose = "--verbose" in sys.argv[1:]
    findings = Findings()

    makefile_text = MAKEFILE.read_text()
    phony, prereqs, recipes = parse_makefile(makefile_text)
    known = set(prereqs) | phony
    try:
        workflows = load_workflows()
    except YamlError as error:
        print(f"error: {error}", file=sys.stderr)
        return 2
    live = triggerable(workflows)
    step_commands = ci_step_commands(workflows)
    ci_text = "\n".join(command for _, command in step_commands)
    if "ci-preflight-dispatcher.sh" in ci_text:
        fallback = subprocess.run(
            [
                "bash",
                str(DISPATCHER),
                "--dry-run",
                "--",
                "some-unclassified-root-file.txt",
            ],
            cwd=REPO_ROOT,
            check=False,
            capture_output=True,
            text=True,
        )
        if fallback.returncode != 0:
            print(fallback.stderr, file=sys.stderr)
            return 2
        step_commands.append(("ci dispatcher fail-closed selection", fallback.stdout))
        ci_text += "\n" + fallback.stdout

    print(
        f"==> parsed {len(workflows)} workflow(s); {len(live)} can trigger; "
        f"{len(step_commands)} runnable step command(s)"
    )
    if verbose:
        for workflow in workflows:
            state = "live" if workflow in live else "NEVER TRIGGERS"
            print(
                f"      {workflow.rel}: {','.join(sorted(workflow.triggers))} [{state}]"
            )

    # ── A0: this checker is itself reached by CI ──────────────────────────────
    print("\n==> A0: reachability gate is invoked by CI")
    if SELF_TARGET in make_targets_in(ci_text, known) or re.search(
        r"check-gate-reachability\.py", ci_text
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
    # Roots are commands reached from CI. When CI invokes the dispatcher, its
    # fail-closed selection is expanded above from the same executable used by
    # local preflight instead of from a separately maintained command list.
    members = workspace_members()
    crates = [crate_name(m) for m in members]
    exclusions = profile_ci_exclusions()
    excluded_binaries = {
        atom.value.lstrip("~=")
        for atom in exclusions
        if atom.kind in {"binary", "binary_id"}
    }
    ci_cargo = [
        invocation
        for where, script in step_commands
        for invocation in cargo_commands_in(where, script)
    ]

    roots = make_targets_in(ci_text, known)
    reached = close_over_makefile(roots, prereqs, recipes, known)

    gates = ci_gate_targets(phony)
    print(f"\n==> A1: CI Makefile gate-target reachability ({len(gates)} gate targets)")
    if verbose:
        print("    Roots (invoked directly by a CI workflow step):")
        for t in sorted(roots):
            print(f"      - {t}")
    # Then the containment proof, to a fixpoint: a target CI never names is
    # reached when every prerequisite is reached and every command in its recipe
    # is one CI runs anyway. `make test` is the workspace suite CI runs in
    # pieces; `make lint` is CI's clippy invocation plus prerequisites that are
    # each their own CI step. A target with one unclassifiable command — a bash
    # script, a sanitizer-flagged run, a `cargo miri test` — is NOT proved, and
    # stays a wire-or-cut decision.
    proved: set[str] = set()
    while True:
        blobs = ci_test_commands(step_commands, recipes, reached)
        newly = {
            target
            for target in gates
            if target not in reached
            and prove_contained(
                target,
                prereqs,
                recipes,
                reached,
                known,
                ci_cargo,
                blobs,
                crates,
                excluded_binaries,
            )
        }
        if not newly:
            break
        proved |= newly
        reached = close_over_makefile(reached | newly, prereqs, recipes, known)
    if verbose and proved:
        print("    Proved contained (CI runs every command they run):")
        for t in sorted(proved):
            print(f"      - {t}")

    unreached = unreached_ci_gates(phony, reached)
    for target in unreached:
        findings.fail(
            "A1",
            f"make {target}",
            "reached by no CI workflow step. Wire it into the job where it "
            "belongs, or delete the target and everything that exists only to "
            "serve it. A local-preflight-only edge does not count: it never "
            "runs on a pull request, and neither does a mention in a comment.",
        )
    print(f"    {len(gates) - len(unreached)}/{len(gates)} CI gate targets reached.")

    # ── A1H: named host-release authorities are real, and not hosted CI ──────
    # A host authority is not an exception to A1. It is a different claim:
    # “this local host can take this measurement.” Treating a hosted macOS job
    # as equivalent would let a missing entitlement, allocator setting, or
    # inspector tool turn into a green skip. The port must execute its exact
    # runner, while CI must NOT reach it. Both conditions are structural and
    # comment-free.
    print(
        "\n==> A1H: named host-release authorities "
        f"({len(HOST_RELEASE_AUTHORITIES)} authority/authorities)"
    )
    host_failures = 0
    for authority in HOST_RELEASE_AUTHORITIES:
        if not host_release_authority_is_ported(authority, known, recipes):
            findings.fail(
                "A1H",
                f"make {authority.target}",
                f"the named {authority.host} release authority has no direct "
                f"executable `{authority.runner}` Make port. A comment, echo, "
                "or skip is not a release measurement; restore the runner or "
                "remove the authority class deliberately.",
            )
            host_failures += 1
        elif authority.target in reached:
            findings.fail(
                "A1H",
                f"make {authority.target}",
                f"the named {authority.host} release authority is CI-reached. "
                "Hosted CI cannot report a local host-release measurement as "
                "green authority; remove that workflow edge and retain the "
                "local port.",
            )
            host_failures += 1
        elif verbose:
            print(
                f"  ok  make {authority.target}: local {authority.host} "
                "authority is ported and not CI-reached"
            )
    print(
        f"    {len(HOST_RELEASE_AUTHORITIES) - host_failures}/"
        f"{len(HOST_RELEASE_AUTHORITIES)} host authorities are real local ports."
    )

    # ── A2: every workspace crate is tested by CI ─────────────────────────────
    blobs = ci_test_commands(step_commands, recipes, reached)
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
        set(re.findall(r"--profile\s+([A-Za-z0-9_-]+)", ci_text))
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
    print(f"\n==> A3b: profile.ci default-filter exclusions ({len(exclusions)})")
    for atom in exclusions:
        token = atom.value.lstrip("~=")
        if atom.kind == "package":
            covered = crate_covered(token, blobs)
        elif atom.kind in {"binary", "binary_id", "test"}:
            covered = any(
                re.search(rf"(?<![\w-]){re.escape(token)}(?![\w-])", b) for b in blobs
            )
        else:
            raise SystemExit(
                f"error: profile.ci subtracts `{atom}`, whose compensation this "
                "checker cannot express. Teach it that selector or state the "
                "exclusion in terms it can check."
            )
        if not covered:
            findings.fail(
                "A3b",
                str(atom),
                "subtracted from profile.ci's default-filter and named by no CI "
                "step or CI-reached Makefile target, so nothing runs it. Route it "
                "back into the CI run, give it a dedicated step, or delete it.",
            )
        elif verbose:
            print(f"  ok  {atom} run by a CI-reached invocation")
    print(
        f"    {len(exclusions) - findings.count('A3b')}/{len(exclusions)} "
        "exclusions compensated."
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
    runs = nextest_runs(step_commands, recipes, reached)
    filtered = [run for run in runs if run.filtered]
    print(f"\n==> A3d: filtered nextest runs ({len(filtered)} of {len(runs)})")
    for run in filtered:
        missing = uncompensated_packages(run, runs, crates)
        if missing:
            findings.fail(
                "A3d",
                f"{run.where}: {' '.join(f'-E {f!r}' for f in run.filters)}",
                "this step filters its nextest run and no unfiltered CI run "
                f"executes {', '.join(missing)}, so whatever the filter subtracts "
                "for those packages is subtracted everywhere. A `--workspace` run "
                "that --excludes the package does not vouch for it. Wire an "
                "unfiltered run, or delete the tests this expression hides.",
            )
        elif verbose:
            print(f"  ok  {run.where}: {run.filters} compensated per package")
    if filtered and not findings.count("A3d"):
        print(f"    {len(filtered)}/{len(filtered)} filtered runs compensated.")

    # ── A4: documented targets exist ──────────────────────────────────────────
    references = documented_make_references()
    print(f"\n==> A4: documented `make` targets exist ({len(references)} reference(s))")
    dangling = [ref for ref in references if ref.target not in known]
    for ref in dangling:
        findings.fail(
            "A4",
            f"{ref.where}: make {ref.target}",
            "this file tells a reader to run a target the Makefile does not "
            "define, so following it produces `No rule to make target`. Point it "
            "at the surviving target, restate the instruction against whatever "
            "enforces it now, or write it as a metavariable if it is only "
            "illustrating a command shape.",
        )
    print(
        f"    {len(references) - len(dangling)}/{len(references)} references resolve."
    )
    if verbose:
        for ref in sorted(references, key=lambda r: r.where):
            print(f"  ok  {ref.where}: make {ref.target}")

    # ── A5: a declared target actually does something ─────────────────────────
    #
    # A0..A4 all resolve a NAME. None of them asked whether the name does any
    # work. A `.PHONY` name with no recipe and no prerequisites is a legal,
    # always-out-of-date target: make runs it, has nothing to run, and exits 0.
    # So it satisfies every reachability check while enforcing nothing, and the
    # gate it used to invoke stops running without one red build.
    #
    # That is how the sys-lane closure went silent — a rebase across a Makefile
    # restructure carried the `.PHONY` line and the `lint` prerequisite but
    # dropped the two recipe blocks, and `make lint` kept passing. A4 reported
    # every reference resolving, correctly and uselessly: `verify-sys-lane-closure`
    # existed, it just did nothing.
    #
    # A phony target with prerequisites and no recipe is an aggregator (`lint:
    # a b c`) and is exactly right, so the failure is the conjunction: nothing to
    # run AND nothing to bring up to date.
    inert = sorted(
        t for t in phony if not recipes.get(t, "").strip() and not prereqs.get(t)
    )
    print(f"\n==> A5: declared targets do work ({len(phony)} phony target(s))")
    for target in inert:
        findings.fail(
            "A5",
            f"Makefile: .PHONY {target}",
            "this target is declared but has neither a recipe nor prerequisites, "
            "so make resolves it and succeeds having done nothing. Every check "
            "that only resolves the name passes while whatever it used to run "
            "no longer runs. Restore its recipe, or delete the name from .PHONY "
            "and from every target that lists it.",
        )
    print(f"    {len(phony) - len(inert)}/{len(phony)} phony targets do work.")

    # ── Verdict ───────────────────────────────────────────────────────────────
    print("")
    if findings.failures:
        print("\n".join(findings.failures))
        print("")
        print(f"FAIL: {len(findings.failures)} finding(s).")
        print("      Every entry above is a WIRE-OR-CUT decision, not a waiver:")
        print("      attach it to the job where it belongs, or delete it.")
        print("      This gate has no exemption list by design.")
        return 1

    print("==> Gate reachability: every CI gate target, workspace crate, profile.ci")
    print("    exclusion, inline `-E` filter and `#[ignore]`d crate is reached by")
    print("    CI; named host-release authorities are real local ports; and every")
    print("    documented `make` target exists.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
