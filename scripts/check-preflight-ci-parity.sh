#!/usr/bin/env bash
# check-preflight-ci-parity.sh — assert the dispatcher's fallback lane stays
# aligned with the commands that ci.yml and release-gate.yml run.
#
# Exits 0 if every CI-required check is present in the dispatcher's fallback
# command set; exits 1 and prints a diagnostic for each missing check.
#
# Run this script after any change to scripts/ci-preflight-dispatcher.sh,
# .github/workflows/ci.yml, or .github/workflows/release-gate.yml to confirm
# local preflight still predicts CI outcomes (LESSONS: preflight-perf-discipline).
#
# The required-check list is derived from the dispatcher itself via
# --ci-required so there is a single source of truth: update the
# CI_REQUIRED_CHECKS array in ci-preflight-dispatcher.sh, not here.
#
# Usage:
#   scripts/check-preflight-ci-parity.sh              # check only
#   scripts/check-preflight-ci-parity.sh --verbose    # include full command list

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DISPATCHER="$REPO_ROOT/scripts/ci-preflight-dispatcher.sh"
VERBOSE=0
[[ "${1:-}" == "--verbose" ]] && VERBOSE=1

# ── CI-required checks (derived from dispatcher --ci-required) ─────────────────
# The authoritative list lives in CI_REQUIRED_CHECKS inside ci-preflight-dispatcher.sh.
# Read it from there so this script never diverges.

declare -a CI_CHECKS_LABEL
declare -a CI_CHECKS_PATTERN
i=0
while IFS=$'\t' read -r label pattern; do
    [[ -n "$label" && -n "$pattern" ]] || continue
    CI_CHECKS_LABEL[i]="$label"
    CI_CHECKS_PATTERN[i]="$pattern"
    (( i++ ))
done < <("$DISPATCHER" --ci-required)
CI_CHECKS_COUNT=$i

if (( CI_CHECKS_COUNT == 0 )); then
    echo "error: $DISPATCHER --ci-required returned no entries; cannot verify parity." >&2
    exit 1
fi

# ── Capture fallback-lane command set via --dry-run ────────────────────────────
# Use a path that routes to the fallback (comprehensive) lane.
FALLBACK_OUT=$("$DISPATCHER" --dry-run -- some-unclassified-root-file.txt 2>&1)

# Extract commands from the "Commands:" block.
FALLBACK_CMDS=$(
    printf '%s\n' "$FALLBACK_OUT" \
        | awk '/^Commands:/{found=1; next} found && /^  - /{cmd=substr($0,5); sub(/  \(budget:[^)]*\)$/, "", cmd); print cmd} found && /^(Dry run:|Commands: none)/{found=0}'
)

if (( VERBOSE == 1 )); then
    echo "==> Dispatcher fallback-lane commands:"
    printf '%s\n' "$FALLBACK_CMDS" | sed 's/^/  /'
    echo ""
fi

# ── Parity checks ──────────────────────────────────────────────────────────────
pass=0
fail=0

for (( j=0; j<CI_CHECKS_COUNT; j++ )); do
    label="${CI_CHECKS_LABEL[$j]}"
    pattern="${CI_CHECKS_PATTERN[$j]}"
    if printf '%s\n' "$FALLBACK_CMDS" | grep -qF "$pattern"; then
        if (( VERBOSE == 1 )); then
            echo "  ok  [$label]: '$pattern' found"
        fi
        (( pass++ ))
    else
        echo "  FAIL [$label]: '$pattern' not found in fallback lane"
        echo "       CI requires this check; add it to the dispatcher's fallback lane."
        (( fail++ ))
    fi
done

echo ""
echo "==> Preflight↔CI parity: $pass/${CI_CHECKS_COUNT} checks present in fallback lane."

if (( fail > 0 )); then
    echo ""
    echo "FAIL: $fail CI-required check(s) missing from the dispatcher fallback lane."
    echo "      The local preflight will not predict CI outcomes for those checks."
    echo "      See LESSONS.md 'preflight-perf-discipline' for the apply checklist."
    exit 1
fi

echo "     Local preflight fallback lane covers all CI-required checks."
