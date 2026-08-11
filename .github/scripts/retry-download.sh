#!/usr/bin/env bash
# Retry an artifact download.
#
# Every toolchain download here names an immutable, version-pinned release
# asset, so fetching it again after a network failure returns the same bytes.
# That makes this resilience, not the flake-tolerance the test suites refuse:
# there is no outcome to re-roll, only a transfer to complete.
#
# A single blip currently fails a job minutes into a multi-hour run.
#
# Usage: retry-download.sh <command> [args...]
set -euo pipefail

attempts="${RETRY_ATTEMPTS:-4}"
delay="${RETRY_INITIAL_DELAY:-5}"

for attempt in $(seq 1 "$attempts"); do
  if "$@"; then
    [ "$attempt" -gt 1 ] && echo "retry-download: succeeded on attempt $attempt"
    exit 0
  fi
  if [ "$attempt" -eq "$attempts" ]; then
    echo "retry-download: failed after $attempts attempts: $*" >&2
    exit 1
  fi
  echo "retry-download: attempt $attempt failed, retrying in ${delay}s" >&2
  sleep "$delay"
  delay=$((delay * 2))
done
