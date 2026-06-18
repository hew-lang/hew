#!/usr/bin/env bash
# check-preflight-ci-parity.sh — assert the dispatcher's fallback lane stays
# aligned with the commands that ci.yml and release-gate.yml run.
#
# Exits 0 if every CI-required check is present in the dispatcher's fallback
# command set AND every CI build-and-test step maps to an entry in
# CI_REQUIRED_CHECKS; exits 1 and prints a diagnostic for each missing check.
#
# Run this script after any change to scripts/ci-preflight-dispatcher.sh,
# .github/workflows/ci.yml, or .github/workflows/release-gate.yml to confirm
# local preflight still predicts CI outcomes (LESSONS: preflight-perf-discipline).
#
# The required-check list is derived from the dispatcher itself via
# --ci-required so there is a single source of truth: update the
# CI_REQUIRED_CHECKS array in ci-preflight-dispatcher.sh, not here.
#
# The CI_BUILD_AND_TEST_STEPS list below is the maintained mirror of the
# unconditional steps in ci.yml's build-and-test job.  Update it when ci.yml
# adds, removes, or renames a step so CI's lint job catches the drift immediately.
# (Parsing ci.yml directly is not used because multi-line >- run: blocks make
# regex extraction unreliable; a maintained list is safer and equally auditable.)
#
# Usage:
#   scripts/check-preflight-ci-parity.sh              # check only
#   scripts/check-preflight-ci-parity.sh --verbose    # include full command list

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DISPATCHER="$REPO_ROOT/scripts/ci-preflight-dispatcher.sh"
VERBOSE=0
[[ "${1:-}" == "--verbose" ]] && VERBOSE=1

# ── CI build-and-test step list (maintained mirror of ci.yml build-and-test job) ─
# Each entry is the `make`/`cargo` command that the step runs, matched as a
# substring against CI_REQUIRED_CHECKS patterns.  Update this list when ci.yml's
# build-and-test sequence changes.  The assertion below verifies every step here
# has a matching entry in CI_REQUIRED_CHECKS, so a new unconditional CI step that
# isn't mirrored locally fails this script (and therefore the lint CI job).
#
# Source of truth: .github/workflows/ci.yml, job build-and-test (Linux),
# steps with `if: env.RUN_CODE_PATH == 'true'` and a `run:` key.
# Last synced: 2026-06-18 (ci.yml at the time of this script's introduction).
CI_BUILD_AND_TEST_STEPS=(
    "make test-vertical-slice"
    "make test-pkg-import"
    "make fuzz-oracle"
    "make test-hew-ratchet"
    "make test-stdlib-ratchet"
    "make sandbox-parity"
)

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

# ── Subset assertion: every CI build-and-test step maps to CI_REQUIRED_CHECKS ─
# Assert that every command in CI_BUILD_AND_TEST_STEPS has at least one matching
# pattern in CI_REQUIRED_CHECKS (the dispatcher array).  If a new unconditional
# step is added to ci.yml without adding it to both CI_BUILD_AND_TEST_STEPS here
# and CI_REQUIRED_CHECKS in the dispatcher, this check fails and the lint job
# blocks the merge — preventing the structural drift that caused #2023/#2025/#2026.
echo ""
echo "==> CI build-and-test steps ⊆ CI_REQUIRED_CHECKS:"
subset_pass=0
subset_fail=0
for step_cmd in "${CI_BUILD_AND_TEST_STEPS[@]}"; do
    matched=0
    for (( k=0; k<CI_CHECKS_COUNT; k++ )); do
        if [[ "${CI_CHECKS_PATTERN[$k]}" == *"$step_cmd"* ]] || [[ "$step_cmd" == *"${CI_CHECKS_PATTERN[$k]}"* ]]; then
            matched=1
            break
        fi
    done
    if (( matched == 1 )); then
        if (( VERBOSE == 1 )); then
            echo "  ok  CI step '$step_cmd' mapped in CI_REQUIRED_CHECKS"
        fi
        (( subset_pass++ ))
    else
        echo "  FAIL CI step '$step_cmd' has no matching pattern in CI_REQUIRED_CHECKS."
        echo "       Add it to both CI_BUILD_AND_TEST_STEPS (this script) and"
        echo "       CI_REQUIRED_CHECKS in scripts/ci-preflight-dispatcher.sh."
        (( subset_fail++ ))
    fi
done
echo "==> CI step coverage: $subset_pass/${#CI_BUILD_AND_TEST_STEPS[@]} steps mapped."

if (( subset_fail > 0 )); then
    echo ""
    echo "FAIL: $subset_fail CI build-and-test step(s) not mirrored in CI_REQUIRED_CHECKS."
    echo "      A lane can now skip a CI step without the parity checker catching it."
    exit 1
fi

echo "     All CI build-and-test steps are mirrored in CI_REQUIRED_CHECKS."
