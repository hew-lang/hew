#!/usr/bin/env python3
"""Behavioural tests for scripts/check-nightly-freshness.py.

The transport and the clock are both injected, so every verdict below is
proved against a named condition rather than against whatever GitHub happens
to answer today. No live API call is made.

The distinction this file exists to protect: an AUTH failure and a STALE
nightly must never be reported as each other. On a private repository an
unauthorised read returns 404, which looks exactly like "this workflow has
never run on schedule" — reporting a missing `actions: read` grant as nightly
rot would send somebody to fix a workflow that is perfectly healthy.
"""

import datetime as dt
import importlib.util
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

_spec = importlib.util.spec_from_file_location(
    "check_nightly_freshness", ROOT / "scripts" / "check-nightly-freshness.py"
)
freshness = importlib.util.module_from_spec(_spec)
assert _spec.loader is not None
sys.modules.setdefault("check_nightly_freshness", freshness)
_spec.loader.exec_module(freshness)

NOW = dt.datetime(2026, 8, 26, 12, 0, tzinfo=dt.timezone.utc)
OWNERS = {
    "coverage-nightly.yml": {
        "file": "coverage-nightly.yml",
        "owner": "someone",
        "freshness_hours": 48,
    }
}
SCHEDULED = ["coverage-nightly.yml"]


def hours_ago(hours: float) -> str:
    return (NOW - dt.timedelta(hours=hours)).strftime("%Y-%m-%dT%H:%M:%SZ")


class FakeHistory:
    """A run-history endpoint whose answer the test chooses."""

    def __init__(self, answer) -> None:
        self.answer = answer
        self.queries: list[str] = []

    def latest_scheduled_success(self, workflow_file: str):
        self.queries.append(workflow_file)
        if isinstance(self.answer, Exception):
            raise self.answer
        return self.answer


class FakeTransport:
    """Drives the real client's retry/status logic without a network."""

    def __init__(self, responses) -> None:
        self.responses = list(responses)
        self.slept: list[float] = []

    def client(self):
        history = freshness.GitHubRunHistory(
            "owner/repo", "token", sleep=self.slept.append
        )
        history._get = self._get  # type: ignore[method-assign]
        return history

    def _get(self, path: str):
        assert "event=schedule" in path, path
        assert "status=success" in path, path
        response = self.responses.pop(0)
        if isinstance(response, Exception):
            raise response
        return response


def expect_error(fn, needle: str) -> None:
    try:
        fn()
    except freshness.FreshnessError as error:
        assert needle in str(error), (needle, str(error))
        return
    raise AssertionError(f"expected a FreshnessError naming {needle!r}")


def test_a_recent_scheduled_success_is_fresh() -> None:
    client = FakeHistory(hours_ago(10))
    assert freshness.check(client, OWNERS, SCHEDULED, now=NOW) == []


def test_a_run_past_its_window_is_stale_and_names_the_owner() -> None:
    client = FakeHistory(hours_ago(60))
    findings = freshness.check(client, OWNERS, SCHEDULED, now=NOW)
    assert len(findings) == 1, findings
    assert "past its 48h window" in findings[0], findings
    assert "@someone" in findings[0], findings


def test_the_window_is_a_boundary_not_a_suggestion() -> None:
    assert (
        freshness.check(FakeHistory(hours_ago(47.9)), OWNERS, SCHEDULED, now=NOW) == []
    )
    assert freshness.check(FakeHistory(hours_ago(48.1)), OWNERS, SCHEDULED, now=NOW)


def test_no_scheduled_run_on_record_is_red() -> None:
    findings = freshness.check(FakeHistory(None), OWNERS, SCHEDULED, now=NOW)
    assert len(findings) == 1, findings
    assert "no successful SCHEDULED run" in findings[0], findings


def test_only_scheduled_runs_are_queried() -> None:
    """A manual green must not reset the clock.

    The FakeTransport asserts `event=schedule` and `status=success` are both in
    the query; dropping either would let a workflow_dispatch launder a rotting
    nightly into a healthy one.
    """
    transport = FakeTransport(
        [(200, {"workflow_runs": [{"run_started_at": hours_ago(1)}]})]
    )
    assert transport.client().latest_scheduled_success("coverage-nightly.yml")


def test_auth_and_scope_failures_are_red_immediately_and_never_as_staleness() -> None:
    for status in (401, 403, 404):
        transport = FakeTransport([(status, {})])
        expect_error(
            lambda t=transport: t.client().latest_scheduled_success("w.yml"),
            "actions: read",
        )
        assert transport.slept == [], (
            f"HTTP {status} was retried; a 403 will still be a 403 in thirty seconds"
        )


def test_a_server_error_is_retried_then_fails_closed() -> None:
    transport = FakeTransport([(503, {}), (503, {}), (503, {})])
    expect_error(
        lambda: transport.client().latest_scheduled_success("w.yml"),
        "after 3 attempts",
    )
    assert len(transport.slept) == 2, transport.slept

    recovers = FakeTransport(
        [(500, {}), (200, {"workflow_runs": [{"run_started_at": hours_ago(2)}]})]
    )
    assert recovers.client().latest_scheduled_success("w.yml")


def test_a_transport_failure_is_retried_then_fails_closed() -> None:
    """Unlike main-health, which fails OPEN: it gates thirteen jobs on a claim
    about another branch. This gates one cheap assertion about this
    repository's own history, and an unanswerable question about nightly
    freshness is not evidence of freshness."""
    transport = FakeTransport([OSError("dns"), OSError("dns"), OSError("dns")])
    expect_error(
        lambda: transport.client().latest_scheduled_success("w.yml"), "unreachable"
    )
    assert len(transport.slept) == 2, transport.slept


def test_a_malformed_payload_is_red() -> None:
    for payload in ({}, {"workflow_runs": "not-a-list"}, {"workflow_runs": [{}]}):
        transport = FakeTransport([(200, payload)])
        try:
            transport.client().latest_scheduled_success("w.yml")
        except freshness.FreshnessError:
            continue
        raise AssertionError(f"malformed payload accepted: {payload!r}")


def test_an_empty_enumeration_is_red() -> None:
    """LESSONS.md enumeration-gate-floors: zero found is a defect, not a pass."""
    expect_error(
        lambda: freshness.check(FakeHistory(None), OWNERS, [], now=NOW),
        "empty enumeration",
    )


def test_an_unowned_scheduled_workflow_is_red() -> None:
    expect_error(
        lambda: freshness.check(
            FakeHistory(None), OWNERS, SCHEDULED + ["new-nightly.yml"], now=NOW
        ),
        "no entry in .github/nightly-owners.yml",
    )


def test_an_owner_entry_for_a_retired_workflow_is_red() -> None:
    """A window nothing can ever satisfy would deadlock every pull request."""
    owners = dict(OWNERS)
    owners["retired.yml"] = {
        "file": "retired.yml",
        "owner": "someone",
        "freshness_hours": 48,
    }
    expect_error(lambda: freshness.reconcile(SCHEDULED, owners), "no longer schedule")


def test_the_real_repository_enumerates_its_scheduled_workflows() -> None:
    """Not vacuous against the tree it actually guards."""
    found = freshness.scheduled_workflows()
    assert found, "no scheduled workflows enumerated from .github/workflows"
    owners = freshness._reporter.parse_owners(
        (ROOT / ".github" / "nightly-owners.yml").read_text(encoding="utf-8")
    )
    freshness.reconcile(found, owners)


def test_a_client_without_a_token_cannot_start() -> None:
    expect_error(
        lambda: freshness.GitHubRunHistory("owner/repo", ""), "unauthenticated"
    )
    expect_error(lambda: freshness.GitHubRunHistory("", "token"), "GITHUB_REPOSITORY")


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
