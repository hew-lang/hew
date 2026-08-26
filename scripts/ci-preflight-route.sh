#!/usr/bin/env bash
# ci-preflight-route.sh — derive the preflight route from the CI event, then
# run it.
#
# There is one routing question per event, and exactly one answer:
#
#   pull_request       prove THIS DIFF.  Diff against the pull request's own
#                      base SHA from the event payload.  A missing payload is
#                      an error, not a licence to guess: the guess this
#                      replaces was `HEAD^` in one job and a hardcoded
#                      `origin/main` in another, and on a branch whose base
#                      has moved neither is the diff the author wrote.
#   push               prove THE INTEGRATED TREE.  Comprehensive by policy,
#                      spelled with --comprehensive so the run says why.  The
#                      old `--base origin/main` on a push TO main diffed main
#                      against itself: an empty change set, which the
#                      dispatcher used to accept as "nothing to do".
#   workflow_dispatch  same question as push, same answer.
#
# Any other event is an error.  A merge_group candidate is not a diff against
# a known base and would need a third derivation, a label escape, and a real
# merge-group run to validate; adding it here on speculation would ship an
# unexercised code path and a readiness claim nothing proves.
#
# Usage: scripts/ci-preflight-route.sh [dispatcher arguments...]
#
# Reads GITHUB_EVENT_NAME and, for a pull request, PREFLIGHT_BASE_SHA (set at
# workflow level from github.event.pull_request.base.sha, so the expression is
# written once).  Everything else is forwarded to the dispatcher unchanged.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

die() {
    echo "error: ci-preflight-route: $*" >&2
    exit 1
}

event="${GITHUB_EVENT_NAME:-}"
args=()

case "$event" in
    "")
        die "GITHUB_EVENT_NAME is unset; this helper routes a CI event and there is none"
        ;;
    pull_request | pull_request_target)
        base="${PREFLIGHT_BASE_SHA:-}"
        [[ -n "$base" ]] \
            || die "a ${event} event carries no base SHA; PREFLIGHT_BASE_SHA must come from github.event.pull_request.base.sha"
        args+=(--base "$base")
        ;;
    push | workflow_dispatch)
        # No diff is taken at all.  Both tiers prove the integrated tree, so
        # "what changed" is not the question they ask.
        args+=(--comprehensive)
        ;;
    *)
        die "unsupported event '${event}'; add its routing deliberately, with a test, rather than letting it fall through"
        ;;
esac

# A push to main broadcasts a failure to every open branch, so its shard stops
# at the first one.  Sharding keeps that from narrowing the report: the other
# shards run to completion.
if [[ "$event" == "push" ]]; then
    args+=(--fail-fast)
fi

if [[ -n "${PREFLIGHT_ROUTE_PRINT_ONLY:-}" ]]; then
    # Test-only: report the derived argv instead of dispatching, so the route
    # can be asserted against a synthetic event without running a preflight.
    printf '%s\n' "${args[@]}" "$@"
    exit 0
fi

exec "${SCRIPT_DIR}/ci-preflight-dispatcher.sh" "${args[@]}" "$@"
