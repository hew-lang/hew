#!/usr/bin/env python3
"""Behavioural tests for scripts/scheduled-failure-report.py.

Every outcome is driven through a fake HTTP client, so the assertions are
about what the reporter DOES at the GitHub boundary rather than about the
shape of its source. No live issue is created, read, or closed.

The three failure modes each test exists for are the ones a naive reporter
gets wrong while looking correct:

  * the `ci-nightly` label does not exist in this repository, so an issue
    filed with it 422s;
  * `POST /issues/{n}/assignees` with a non-assignable login returns 201 and
    assigns nobody, producing an unowned issue that looks wired;
  * a reporter that only runs on red can open an issue but never close one.
"""

import importlib.util
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

_spec = importlib.util.spec_from_file_location(
    "scheduled_failure_report", ROOT / "scripts" / "scheduled-failure-report.py"
)
reporter = importlib.util.module_from_spec(_spec)
assert _spec.loader is not None
sys.modules.setdefault("scheduled_failure_report", reporter)
_spec.loader.exec_module(reporter)

OWNERS = reporter.parse_owners(
    (ROOT / ".github" / "nightly-owners.yml").read_text(encoding="utf-8")
)
WORKFLOW = "coverage-nightly.yml"
OWNER = str(OWNERS[WORKFLOW]["owner"])
TITLE = f"Scheduled workflow is red: {WORKFLOW}"


class FakeClient:
    """A GitHub whose every answer is chosen by the test."""

    def __init__(
        self,
        *,
        label_status: int = 200,
        label_create_status: int = 201,
        open_issues: list[dict] | None = None,
        assign_logins: list[str] | None = "unset",  # type: ignore[assignment]
        assign_status: int = 201,
    ) -> None:
        self.label_status = label_status
        self.label_create_status = label_create_status
        self.open_issues = open_issues or []
        self.assign_logins = assign_logins
        self.assign_status = assign_status
        self.calls: list[tuple[str, str, dict | None]] = []
        self.next_number = 4242

    def request(self, method: str, path: str, body: dict | None = None):
        self.calls.append((method, path, body))
        if method == "GET" and path.startswith("/labels/"):
            return self.label_status, (
                {"name": "ci-nightly"} if self.label_status == 200 else {}
            )
        if method == "POST" and path == "/labels":
            return self.label_create_status, {"name": "ci-nightly"}
        if method == "GET" and path.startswith("/issues?"):
            return 200, self.open_issues
        if method == "POST" and path == "/issues":
            number = self.next_number
            self.next_number += 1
            return 201, {"number": number}
        if method == "POST" and path.endswith("/assignees"):
            logins = self.assign_logins
            if logins == "unset":
                logins = [OWNER]
            return self.assign_status, {
                "assignees": [{"login": login} for login in logins]
            }
        if method == "POST" and path.endswith("/comments"):
            return 201, {}
        if method == "PATCH" and path.startswith("/issues/"):
            return 200, {"state": "closed"}
        raise AssertionError(f"unexpected call {method} {path}")

    def did(self, method: str, needle: str) -> bool:
        return any(
            call_method == method and needle in path
            for call_method, path, _ in self.calls
        )

    def opened_an_issue(self) -> bool:
        """Exactly `POST /issues` -- not `POST /issues/7/comments`."""
        return any(
            method == "POST" and path == "/issues" for method, path, _ in self.calls
        )


def run(client, outcome: str, needs: str = "{}", workflow: str = WORKFLOW) -> str:
    return reporter.report(
        client,
        workflow_file=workflow,
        outcome=outcome,
        needs_json=needs,
        run_url="https://example.invalid/run/1",
        owners=OWNERS,
    )


def expect_error(fn, needle: str) -> None:
    try:
        fn()
    except reporter.ReporterError as error:
        assert needle in str(error), (needle, str(error))
        return
    raise AssertionError(f"expected a ReporterError naming {needle!r}")


def test_a_red_run_opens_a_labelled_assigned_issue() -> None:
    client = FakeClient()
    result = run(client, "failure", '{"coverage": {"result": "failure"}}')
    assert "opened" in result, result
    created = [body for method, path, body in client.calls if path == "/issues"]
    assert len(created) == 1, created
    assert created[0]["labels"] == ["ci-nightly"], created[0]
    assert "coverage" in created[0]["body"], created[0]["body"]
    assert client.did("POST", "/assignees"), client.calls


def test_a_missing_label_is_created_and_a_failed_creation_is_red() -> None:
    """Filing with an unknown label 422s; proceeding unlabelled hides the issue."""
    created = FakeClient(label_status=404)
    run(created, "failure")
    assert created.did("POST", "/labels"), created.calls

    refused = FakeClient(label_status=404, label_create_status=403)
    expect_error(lambda: run(refused, "failure"), "could not create")

    unreadable = FakeClient(label_status=500)
    expect_error(lambda: run(unreadable, "failure"), "could not read")


def test_an_unassignable_owner_is_red_rather_than_a_silent_no_op() -> None:
    """GitHub returns 201 and assigns nobody. A 2xx is not evidence."""
    client = FakeClient(assign_logins=[])
    expect_error(lambda: run(client, "failure"), "did not assign")

    wrong = FakeClient(assign_logins=["someone-else"])
    expect_error(lambda: run(wrong, "failure"), "did not assign")


def test_a_second_red_updates_the_existing_issue_rather_than_duplicating() -> None:
    client = FakeClient(open_issues=[{"number": 7, "title": TITLE}])
    result = run(client, "failure")
    assert "updated #7" in result, result
    assert not client.opened_an_issue(), client.calls
    assert client.did("POST", "/issues/7/comments"), client.calls
    assert client.did("POST", "/issues/7/assignees"), client.calls


def test_a_green_run_closes_the_open_issue() -> None:
    """A red-only reporter is a one-way ratchet into a permanent ticket."""
    client = FakeClient(open_issues=[{"number": 7, "title": TITLE}])
    result = run(client, "success")
    assert "closed #7" in result, result
    assert client.did("PATCH", "/issues/7"), client.calls
    assert client.did("POST", "/issues/7/comments"), client.calls


def test_a_green_run_with_no_open_issue_does_nothing() -> None:
    client = FakeClient()
    result = run(client, "success")
    assert "no open issue" in result, result
    assert not client.opened_an_issue(), client.calls


def test_a_workflow_with_no_owner_entry_is_red() -> None:
    """A reporter that files unowned issues is not a reporter."""
    expect_error(
        lambda: run(FakeClient(), "failure", workflow="not-scheduled.yml"),
        "no entry in .github/nightly-owners.yml",
    )


def test_an_unreadable_outcome_is_red() -> None:
    """An outcome nobody can parse is not evidence of health."""
    expect_error(lambda: run(FakeClient(), ""), "outcome must be")
    expect_error(lambda: run(FakeClient(), "skipped"), "outcome must be")


def test_the_failing_job_list_names_only_the_jobs_that_failed() -> None:
    needs = (
        '{"coverage": {"result": "failure"}, "full-windows": {"result": "success"},'
        ' "full-macos": {"result": "cancelled"}}'
    )
    assert reporter.failing_jobs(needs) == ["coverage", "full-macos"]
    assert reporter.failing_jobs("not json") == []
    assert reporter.failing_jobs("{}") == []


def test_a_reporter_without_a_token_cannot_start() -> None:
    expect_error(lambda: reporter.GitHubClient("owner/repo", ""), "cannot authenticate")


def test_the_owner_table_parser_rejects_an_incomplete_entry() -> None:
    expect_error(
        lambda: reporter.parse_owners(
            "workflows:\n  - file: a.yml\n    freshness_hours: 48\n"
        ),
        "missing ['owner']",
    )
    expect_error(lambda: reporter.parse_owners("workflows:\n"), "declares no workflows")


def test_the_owner_table_refuses_to_pick_between_two_answers() -> None:
    """An ownership table that disagrees with itself names no owner.

    Both spellings used to be accepted with the later value silently winning:
    a workflow listed twice, and a key set twice inside one entry. Either is
    somebody's edit landing on top of somebody else's, in the one file whose
    job is to say who gets woken up when a nightly goes red.
    """
    expect_error(
        lambda: reporter.parse_owners(
            "workflows:\n"
            "  - file: a.yml\n    owner: first\n    freshness_hours: 48\n"
            "  - file: a.yml\n    owner: second\n    freshness_hours: 72\n"
        ),
        "has more than one entry",
    )
    expect_error(
        lambda: reporter.parse_owners(
            "workflows:\n"
            "  - file: a.yml\n    owner: first\n    owner: second\n"
            "    freshness_hours: 48\n"
        ),
        "is set twice",
    )
    expect_error(
        lambda: reporter.parse_owners(
            "workflows:\n"
            "  - file: a.yml\n    owner: first\n"
            "    freshness_hours: 48\n    freshness_hours: 72\n"
        ),
        "is set twice",
    )
    expect_error(
        lambda: reporter.parse_owners(
            "workflows:\n  - file: a.yml\n    owner: o\n    freshness_hours: soon\n"
        ),
        "must be an integer",
    )

    # Falsifiability: the shape the table actually uses still parses, and two
    # DIFFERENT workflows are not a conflict.
    parsed = reporter.parse_owners(
        "workflows:\n"
        "  - file: a.yml\n    owner: one\n    freshness_hours: 48\n"
        "  - file: b.yml\n    owner: two\n    freshness_hours: 72\n"
    )
    assert sorted(parsed) == ["a.yml", "b.yml"], parsed
    assert parsed["b.yml"]["freshness_hours"] == 72, parsed


def test_the_real_owner_table_states_one_answer_per_workflow() -> None:
    parsed = reporter.parse_owners(
        (ROOT / ".github" / "nightly-owners.yml").read_text(encoding="utf-8")
    )
    assert parsed, "the committed owner table is empty"
    for name, entry in parsed.items():
        assert entry["owner"], name
        assert isinstance(entry["freshness_hours"], int), name


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
