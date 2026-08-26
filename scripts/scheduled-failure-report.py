#!/usr/bin/env python3
"""File, update, or close the ownership issue for a scheduled workflow run.

Invoked by `.github/workflows/scheduled-failure-report.yml`, which every
scheduled workflow calls as a job -- on green as well as on red, because a
red-only reporter is a one-way ratchet into a permanently-open issue.

Everything that can silently do nothing is asserted instead:

  * The `ci-nightly` label does not exist in this repository. Creating an
    issue with an unknown label is a 422, so the label is fetched, created on
    404, and a creation failure is a loud red -- never a quiet "proceed
    unlabelled".
  * `POST /issues/{n}/assignees` with a non-assignable login returns 201 and
    assigns NOBODY. The response's `assignees` array is therefore checked for
    the login. An unowned issue is exactly the failure this reporter exists to
    prevent, wearing a costume.
  * A workflow with no entry in `.github/nightly-owners.yml` is an error. A
    reporter that files unowned issues is not a reporter.

The HTTP boundary is one injectable object so the outcomes above can be proved
against a fake client rather than against live issues.
"""

from __future__ import annotations

import json
import os
import sys
import urllib.error
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OWNERS = ROOT / ".github" / "nightly-owners.yml"
LABEL = "ci-nightly"
LABEL_COLOR = "b60205"
LABEL_DESCRIPTION = "A scheduled (nightly) workflow is red and needs an owner."
API = "https://api.github.com"


class ReporterError(RuntimeError):
    """A condition the reporter must fail on rather than work around."""


# ── owner table ──────────────────────────────────────────────────────────────


def parse_owners(text: str) -> dict[str, dict[str, object]]:
    """Read `.github/nightly-owners.yml`.

    A hand parser rather than a dependency, matching every other Python gate
    in `scripts/`: the file is a list of three-key mappings and the reporter
    must run on a bare runner with no pip step.
    """
    owners: dict[str, dict[str, object]] = {}
    current: dict[str, object] | None = None
    in_list = False
    for raw in text.splitlines():
        line = raw.split("#", 1)[0].rstrip()
        if not line.strip():
            continue
        if line.strip() == "workflows:":
            in_list = True
            continue
        if not in_list:
            continue
        stripped = line.strip()
        if stripped.startswith("- "):
            current = {}
            stripped = stripped[2:]
        if current is None:
            continue
        if ":" not in stripped:
            raise ReporterError(f"nightly-owners.yml: cannot read line {raw!r}")
        key, _, value = stripped.partition(":")
        value = value.strip().strip('"').strip("'")
        key = key.strip()
        if key == "file":
            current["file"] = value
            owners[value] = current
        elif key == "freshness_hours":
            current[key] = int(value)
        else:
            current[key] = value
    for name, entry in owners.items():
        missing = {"file", "owner", "freshness_hours"} - set(entry)
        if missing:
            raise ReporterError(
                f"nightly-owners.yml: {name} is missing {sorted(missing)}"
            )
    if not owners:
        raise ReporterError("nightly-owners.yml declares no workflows")
    return owners


def owner_for(workflow_file: str, owners: dict[str, dict[str, object]]) -> str:
    entry = owners.get(workflow_file)
    if entry is None:
        raise ReporterError(
            f"{workflow_file} has no entry in .github/nightly-owners.yml; "
            "a scheduled workflow without a named owner is exactly the unowned "
            "red this reporter exists to prevent"
        )
    return str(entry["owner"])


# ── HTTP boundary ────────────────────────────────────────────────────────────


class GitHubClient:
    """The one place this script talks to GitHub, so tests can replace it."""

    def __init__(self, repository: str, token: str) -> None:
        if not token:
            raise ReporterError("no GITHUB_TOKEN; the reporter cannot authenticate")
        self.repository = repository
        self.token = token

    def request(
        self, method: str, path: str, body: dict | None = None
    ) -> tuple[int, object]:
        data = json.dumps(body).encode() if body is not None else None
        request = urllib.request.Request(
            f"{API}/repos/{self.repository}{path}", data=data, method=method
        )
        request.add_header("Authorization", f"Bearer {self.token}")
        request.add_header("Accept", "application/vnd.github+json")
        if data is not None:
            request.add_header("Content-Type", "application/json")
        try:
            with urllib.request.urlopen(request) as response:  # noqa: S310
                raw = response.read().decode() or "null"
                return response.status, json.loads(raw)
        except urllib.error.HTTPError as error:
            raw = error.read().decode() or "null"
            try:
                return error.code, json.loads(raw)
            except json.JSONDecodeError:
                return error.code, raw


# ── behaviour ────────────────────────────────────────────────────────────────


def ensure_label(client) -> None:
    status, _ = client.request("GET", f"/labels/{LABEL}")
    if status == 200:
        return
    if status != 404:
        raise ReporterError(f"could not read the {LABEL} label: HTTP {status}")
    status, payload = client.request(
        "POST",
        "/labels",
        {"name": LABEL, "color": LABEL_COLOR, "description": LABEL_DESCRIPTION},
    )
    if status not in (200, 201):
        raise ReporterError(
            f"could not create the {LABEL} label: HTTP {status} {payload!r}. "
            "Proceeding unlabelled would hide the issue from every query that "
            "looks for it."
        )


def find_issue(client, title: str) -> dict | None:
    status, payload = client.request(
        "GET", f"/issues?state=open&labels={LABEL}&per_page=100"
    )
    if status != 200:
        raise ReporterError(f"could not list {LABEL} issues: HTTP {status}")
    if not isinstance(payload, list):
        raise ReporterError(f"unexpected issue list payload: {payload!r}")
    for issue in payload:
        if isinstance(issue, dict) and issue.get("title") == title:
            return issue
    return None


def assign(client, number: int, owner: str) -> None:
    status, payload = client.request(
        "POST", f"/issues/{number}/assignees", {"assignees": [owner]}
    )
    if status not in (200, 201):
        raise ReporterError(f"could not assign {owner}: HTTP {status}")
    assignees = payload.get("assignees") if isinstance(payload, dict) else None
    logins = {
        entry.get("login") for entry in (assignees or []) if isinstance(entry, dict)
    }
    if owner not in logins:
        raise ReporterError(
            f"GitHub accepted the assignment but did not assign {owner} "
            f"(assignees are {sorted(logins)}). A non-assignable login is a "
            "silent no-op, and an unowned issue is the failure this reporter "
            "exists to prevent."
        )


def failing_jobs(needs_json: str) -> list[str]:
    try:
        needs = json.loads(needs_json or "{}")
    except json.JSONDecodeError:
        return []
    if not isinstance(needs, dict):
        return []
    return sorted(
        name
        for name, value in needs.items()
        if isinstance(value, dict)
        and value.get("result") in ("failure", "cancelled", "timed_out")
    )


def report(
    client,
    *,
    workflow_file: str,
    outcome: str,
    needs_json: str,
    run_url: str,
    owners: dict[str, dict[str, object]],
) -> str:
    if outcome not in ("success", "failure"):
        raise ReporterError(
            f"outcome must be 'success' or 'failure', got {outcome!r}; an "
            "unreadable outcome is not evidence of health"
        )
    owner = owner_for(workflow_file, owners)
    title = f"Scheduled workflow is red: {workflow_file}"
    ensure_label(client)
    existing = find_issue(client, title)

    if outcome == "success":
        if existing is None:
            return "green; no open issue to close"
        number = int(existing["number"])
        client.request(
            "POST",
            f"/issues/{number}/comments",
            {"body": f"Green again: {run_url}"},
        )
        status, _ = client.request("PATCH", f"/issues/{number}", {"state": "closed"})
        if status != 200:
            raise ReporterError(f"could not close issue #{number}: HTTP {status}")
        return f"green; closed #{number}"

    jobs = failing_jobs(needs_json)
    body = "\n".join(
        [
            f"`{workflow_file}` failed on a scheduled run.",
            "",
            f"- Run: {run_url}",
            f"- Owner: @{owner}",
            "- Failing jobs: "
            + (", ".join(f"`{job}`" for job in jobs) or "unreported"),
            "",
            "The scheduled tier gates the required Linux check through",
            "`scripts/check-nightly-freshness.py`: once this workflow's",
            "freshness window in `.github/nightly-owners.yml` elapses without a",
            "successful SCHEDULED run, `Build & test (Linux)` turns red on every",
            "pull request. Fix the workflow, or delete it and its owner entry in",
            "one commit. There is no bypass.",
        ]
    )

    if existing is not None:
        number = int(existing["number"])
        client.request("POST", f"/issues/{number}/comments", {"body": body})
        assign(client, number, owner)
        return f"red; updated #{number}"

    status, payload = client.request(
        "POST",
        "/issues",
        {"title": title, "body": body, "labels": [LABEL]},
    )
    if status not in (200, 201) or not isinstance(payload, dict):
        raise ReporterError(f"could not open an issue: HTTP {status} {payload!r}")
    number = int(payload["number"])
    assign(client, number, owner)
    return f"red; opened #{number}"


def main() -> int:
    try:
        owners = parse_owners(OWNERS.read_text(encoding="utf-8"))
        client = GitHubClient(
            os.environ.get("REPORT_REPOSITORY", ""),
            os.environ.get("GITHUB_TOKEN") or os.environ.get("GH_TOKEN") or "",
        )
        result = report(
            client,
            workflow_file=os.environ.get("REPORT_WORKFLOW_FILE", ""),
            outcome=os.environ.get("REPORT_OUTCOME", ""),
            needs_json=os.environ.get("REPORT_NEEDS", "{}"),
            run_url=os.environ.get("REPORT_RUN_URL", ""),
            owners=owners,
        )
    except ReporterError as error:
        print(f"::error::scheduled-failure-report: {error}", file=sys.stderr)
        return 1
    print(result)
    return 0


if __name__ == "__main__":
    sys.exit(main())
