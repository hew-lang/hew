#!/usr/bin/env python3
"""Assert the scheduled tier has actually run, recently, and successfully.

A tier without an owner is not a tier — and an owner who is never told is not
an owner. The reporter (`scripts/scheduled-failure-report.py`) tells somebody
when a scheduled run goes red; this tells everybody when a scheduled workflow
stops producing verdicts at all, which is the quieter failure. It runs as a
required job so nightly rot cannot accumulate behind green pull requests.

Four properties, each of which a naive version gets wrong:

  * **Enumerate, do not hardcode.** The scheduled workflows are read out of
    `.github/workflows/*.yml` by their `on: schedule:` trigger and cross-checked
    against `.github/nightly-owners.yml`. A scheduled workflow with no owner is
    red; an owner entry for a workflow that no longer schedules is red; an
    empty enumeration is red (LESSONS.md enumeration-gate-floors).

  * **Scheduled runs only.** The query filters `event=schedule`. A manual
    `workflow_dispatch` green must not reset the clock — that is precisely how
    a rotting nightly gets laundered into a healthy-looking one.

  * **Auth failure is never reported as staleness.** On a private repository
    an unauthenticated read 404s, which is indistinguishable from "this
    workflow has never run on schedule". 401/403/404 is therefore red naming
    the missing permission, and is NOT retried: a 403 will still be a 403 in
    thirty seconds. Reading workflow-run history needs `actions: read`, which
    `contents: read` does not cover.

  * **Fail closed on an unanswerable question.** Transport and 5xx failures get
    a bounded retry, then red. This gates one cheap assertion about this
    repository's own run history; "I could not find out whether the nightly
    is healthy" is not evidence that it is.

There is no bypass label and no skip environment variable
(LESSONS.md preflight-gate-no-bypass). A stale nightly is fixed, or the
nightly and its owner entry are deleted in one commit.
"""

from __future__ import annotations

import datetime as dt
import importlib.util
import json
import os
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOWS = ROOT / ".github" / "workflows"
OWNERS = ROOT / ".github" / "nightly-owners.yml"
API = "https://api.github.com"
RETRY_ATTEMPTS = 3
RETRY_BASE_DELAY = 2.0


class FreshnessError(RuntimeError):
    """A condition that must be red rather than worked around."""


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules.setdefault(name, module)
    spec.loader.exec_module(module)
    return module


_reporter = _load(
    "scheduled_failure_report", ROOT / "scripts" / "scheduled-failure-report.py"
)
_reachability = _load(
    "check_gate_reachability", ROOT / "scripts" / "check-gate-reachability.py"
)


# ── enumeration ──────────────────────────────────────────────────────────────


def scheduled_workflows(directory: Path = WORKFLOWS) -> list[str]:
    """Workflow file names carrying an `on: schedule:` trigger."""
    found = []
    for path in sorted(directory.glob("*.yml")) + sorted(directory.glob("*.yaml")):
        document = _reachability.parse_yaml(path.read_text(encoding="utf-8"), path.name)
        if not isinstance(document, dict):
            continue
        # PyYAML-style parsers fold a bare `on:` key to True; this one keeps
        # the string, but read both so a parser change cannot silently empty
        # the enumeration.
        triggers = document.get("on", document.get(True))
        if isinstance(triggers, dict) and "schedule" in triggers:
            found.append(path.name)
    return found


def reconcile(scheduled: list[str], owners: dict[str, dict[str, object]]) -> None:
    if not scheduled:
        raise FreshnessError(
            "no scheduled workflows found under .github/workflows; an empty "
            "enumeration is a defect in this check, not evidence that the "
            "scheduled tier is healthy"
        )
    unowned = sorted(set(scheduled) - set(owners))
    if unowned:
        raise FreshnessError(
            f"scheduled workflow(s) with no entry in .github/nightly-owners.yml: "
            f"{unowned}. A tier without an owner is not a tier."
        )
    orphaned = sorted(set(owners) - set(scheduled))
    if orphaned:
        raise FreshnessError(
            f"owner entr(ies) for workflow(s) that no longer schedule: {orphaned}. "
            "Delete the entry in the same commit as the workflow."
        )


# ── HTTP boundary ────────────────────────────────────────────────────────────


class GitHubRunHistory:
    """The one place this script reads workflow-run history."""

    def __init__(self, repository: str, token: str, sleep=time.sleep) -> None:
        if not repository:
            raise FreshnessError("GITHUB_REPOSITORY is unset")
        if not token:
            raise FreshnessError(
                "no GITHUB_TOKEN/GH_TOKEN exported; reading workflow-run history "
                "requires an authenticated call, and an unauthenticated one 404s "
                "on a private repository — which would be reported as rot"
            )
        self.repository = repository
        self.token = token
        self.sleep = sleep

    def latest_scheduled_success(self, workflow_file: str) -> str | None:
        """`run_started_at` of the most recent SUCCESSFUL SCHEDULED run.

        `None` means the query succeeded and there is no such run. It never
        means the query failed — that raises.
        """
        path = (
            f"/repos/{self.repository}/actions/workflows/{workflow_file}/runs"
            "?event=schedule&status=success&per_page=1"
        )
        last_transport_error: Exception | None = None
        for attempt in range(1, RETRY_ATTEMPTS + 1):
            try:
                status, payload = self._get(path)
            except Exception as error:  # transport: DNS, TLS, reset
                last_transport_error = error
                if attempt < RETRY_ATTEMPTS:
                    self.sleep(RETRY_BASE_DELAY * attempt)
                    continue
                raise FreshnessError(
                    f"{workflow_file}: workflow-run history unreachable after "
                    f"{RETRY_ATTEMPTS} attempts ({error}). An unanswerable "
                    "question about nightly freshness is not evidence of "
                    "freshness."
                ) from error

            if status in (401, 403, 404):
                # Not retried, and never reported as staleness: a 403 will
                # still be a 403 in thirty seconds, and on a private repository
                # a 404 is what an unauthorised read looks like.
                raise FreshnessError(
                    f"{workflow_file}: GitHub returned HTTP {status} for "
                    "workflow-run history. This is an auth/scope defect, not a "
                    "stale nightly: the job needs `actions: read` (contents: "
                    "read does not cover run history) and a GITHUB_TOKEN export."
                )
            if status >= 500 or status == 429:
                if attempt < RETRY_ATTEMPTS:
                    self.sleep(RETRY_BASE_DELAY * attempt)
                    continue
                raise FreshnessError(
                    f"{workflow_file}: GitHub returned HTTP {status} after "
                    f"{RETRY_ATTEMPTS} attempts; failing closed."
                )
            if status != 200:
                raise FreshnessError(
                    f"{workflow_file}: unexpected HTTP {status} reading run history"
                )
            if not isinstance(payload, dict) or "workflow_runs" not in payload:
                raise FreshnessError(
                    f"{workflow_file}: malformed run-history payload; refusing to "
                    "read a shape this check does not understand as health"
                )
            runs = payload["workflow_runs"]
            if not isinstance(runs, list):
                raise FreshnessError(f"{workflow_file}: workflow_runs is not a list")
            if not runs:
                return None
            started = runs[0].get("run_started_at") or runs[0].get("created_at")
            if not isinstance(started, str):
                raise FreshnessError(
                    f"{workflow_file}: run record carries no usable start time"
                )
            return started
        raise FreshnessError(str(last_transport_error))

    def _get(self, path: str) -> tuple[int, object]:
        request = urllib.request.Request(f"{API}{path}", method="GET")
        request.add_header("Authorization", f"Bearer {self.token}")
        request.add_header("Accept", "application/vnd.github+json")
        try:
            with urllib.request.urlopen(request) as response:  # noqa: S310
                return response.status, json.loads(response.read().decode() or "null")
        except urllib.error.HTTPError as error:
            body = error.read().decode() or "null"
            try:
                return error.code, json.loads(body)
            except json.JSONDecodeError:
                return error.code, body


# ── verdict ──────────────────────────────────────────────────────────────────


def check(client, owners, scheduled, *, now=None) -> list[str]:
    """Findings, one per stale or never-run workflow. Empty means fresh."""
    now = now or dt.datetime.now(dt.timezone.utc)
    reconcile(scheduled, owners)
    findings: list[str] = []
    for workflow_file in sorted(scheduled):
        entry = owners[workflow_file]
        window = int(entry["freshness_hours"])
        owner = entry["owner"]
        started = client.latest_scheduled_success(workflow_file)
        if started is None:
            findings.append(
                f"{workflow_file}: no successful SCHEDULED run on record "
                f"(owner @{owner}, window {window}h). A manual run does not "
                "count; the query filters event=schedule on purpose."
            )
            continue
        when = dt.datetime.fromisoformat(started.replace("Z", "+00:00"))
        age = (now - when).total_seconds() / 3600.0
        if age > window:
            findings.append(
                f"{workflow_file}: last successful SCHEDULED run was "
                f"{age:.1f}h ago, past its {window}h window "
                f"(owner @{owner}). Fix the workflow, or delete it and its "
                "owner entry in one commit — there is no bypass."
            )
    return findings


def main() -> int:
    try:
        owners = _reporter.parse_owners(OWNERS.read_text(encoding="utf-8"))
        scheduled = scheduled_workflows()
        client = GitHubRunHistory(
            os.environ.get("GITHUB_REPOSITORY", ""),
            os.environ.get("GITHUB_TOKEN") or os.environ.get("GH_TOKEN") or "",
        )
        findings = check(client, owners, scheduled)
    except (FreshnessError, _reporter.ReporterError) as error:
        print(f"::error::nightly freshness: {error}", file=sys.stderr)
        return 1

    print(f"==> nightly freshness ({len(scheduled)} scheduled workflow(s))")
    for workflow_file in sorted(scheduled):
        print(f"    {workflow_file} owner @{owners[workflow_file]['owner']}")
    if findings:
        for finding in findings:
            print(f"::error::nightly freshness: {finding}", file=sys.stderr)
        return 1
    print("nightly freshness: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
