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
# Usage:
#   scripts/check-preflight-ci-parity.sh              # check only
#   scripts/check-preflight-ci-parity.sh --verbose    # include full command list

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DISPATCHER="$REPO_ROOT/scripts/ci-preflight-dispatcher.sh"
VERBOSE=0
[[ "${1:-}" == "--verbose" ]] && VERBOSE=1

# ── CI-required checks (from ci.yml and release-gate.yml) ──────────────────────
# These are the checks that the merge queue's required signal depends on.
# For each entry: (label, command-substring-to-match-in-dispatcher-output).
# Matching is substring; order does not matter (we check presence, not ordering).

declare -a CI_CHECKS_LABEL
declare -a CI_CHECKS_PATTERN
i=0
# Patterns match either the direct command name OR a Makefile target that
# subsumes it (e.g. make ci-preflight-smoke runs cargo fmt + clippy;
# make lint runs verify-ffi + runtime-poison-safe-lint + clippy).
# Transitive inclusion via make targets is documented here so reviewers can
# verify the chain: command → make target → fallback-lane command.
CI_CHECKS_LABEL[i]="Rust fmt check (ci.yml: cargo fmt --all -- --check)";
CI_CHECKS_PATTERN[i]="make ci-preflight-smoke"; ((i++))
# ↑ make ci-preflight-smoke runs: cargo fmt --all -- --check + cargo clippy +
#   cargo nextest run --workspace --profile smoke + make hew

CI_CHECKS_LABEL[i]="Cargo clippy (ci.yml: cargo clippy --workspace --tests -- -D warnings)";
CI_CHECKS_PATTERN[i]="make ci-preflight-smoke"; ((i++))
# ↑ same: make ci-preflight-smoke includes cargo clippy --workspace --tests

CI_CHECKS_LABEL[i]="verify-ffi (ci.yml: make verify-ffi)";               CI_CHECKS_PATTERN[i]="make lint"; ((i++))
# ↑ make lint: runtime-poison-safe-lint lint-wasm-todo verify-ffi hew-fmt-check + clippy

CI_CHECKS_LABEL[i]="runtime-poison-safe-lint (ci.yml: make runtime-poison-safe-lint)"; CI_CHECKS_PATTERN[i]="make lint"; ((i++))
# ↑ same: make lint includes runtime-poison-safe-lint

CI_CHECKS_LABEL[i]="nextest workspace ci (release-gate.yml: nextest run --workspace --profile ci)";
CI_CHECKS_PATTERN[i]="make test"; ((i++))
# ↑ make test = make test-rust: cargo nextest run --workspace --profile ci

CI_CHECKS_LABEL[i]="playground-check (release-gate.yml: make playground-check)";
CI_CHECKS_PATTERN[i]="make playground-check"; ((i++))

CI_CHECKS_LABEL[i]="Hew test suite ratchet (ci.yml: make test-hew-ratchet)";
CI_CHECKS_PATTERN[i]="make test-hew-ratchet"; ((i++))
# ↑ ci.yml build-and-test: make test-hew-ratchet (runs scripts/hew-suite-ratchet.sh).
# Also in dispatcher fallback lane.  The ratchet gates on the tracked-failures list;
# unexpected failures or unexpected passes both cause exit 1.

CI_CHECKS_LABEL[i]="Stdlib type-check ratchet (ci.yml: make test-stdlib-ratchet)";
CI_CHECKS_PATTERN[i]="make test-stdlib-ratchet"; ((i++))
# ↑ ci.yml build-and-test: make test-stdlib-ratchet (runs scripts/stdlib-ratchet.sh).
# Also in dispatcher fallback lane.

CI_CHECKS_COUNT=$i

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
