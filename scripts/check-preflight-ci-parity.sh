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
# The CI build-and-test step list is parsed directly from .github/workflows/ci.yml
# using a comment-marked block (>>> CI-PARITY-STEPS … <<< CI-PARITY-STEPS).
# Within that block, steps are identified by:
#   - A single-line `run: <cmd>` value, OR
#   - A `# parity-cmd: <cmd>` annotation on a `run: >-` line (used when the
#     actual run: value is a multi-line YAML block whose canonical local command
#     differs from the CI command text, e.g. cargo nextest → make test).
#
# Adding a new step inside the marked block that is NOT in CI_REQUIRED_CHECKS
# fails this self-test (and therefore the lint CI job), blocking the merge.
#
# Usage:
#   scripts/check-preflight-ci-parity.sh              # check only
#   scripts/check-preflight-ci-parity.sh --verbose    # include full command list

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# DISPATCHER / CI_YML default to the in-repo paths but may be overridden via the
# environment so the self-test (scripts/preflight-parity-selftest.sh) can point
# this checker at controlled stub fixtures.
DISPATCHER="${PREFLIGHT_PARITY_DISPATCHER:-$REPO_ROOT/scripts/ci-preflight-dispatcher.sh}"
CI_YML="${PREFLIGHT_PARITY_CI_YML:-$REPO_ROOT/.github/workflows/ci.yml}"
VERBOSE=0
[[ "${1:-}" == "--verbose" ]] && VERBOSE=1

# ── Parse CI build-and-test steps from the marked block in ci.yml ─────────────
# Extract commands from the >>> CI-PARITY-STEPS … <<< CI-PARITY-STEPS block.
# Two extraction rules:
#   1. `run: >-  # parity-cmd: <cmd>` → extract <cmd> (multi-line run: override)
#   2. `run: <cmd>` (single-line, no >-) → extract <cmd>
declare -a CI_BUILD_AND_TEST_STEPS
in_block=0
while IFS= read -r line; do
    # The start marker is a comment line ending with ">>> CI-PARITY-STEPS".
    # Match conservatively: the trimmed line must be "# >>> CI-PARITY-STEPS".
    trimmed="${line#"${line%%[! ]*}"}"  # strip leading whitespace
    if [[ "$trimmed" == '# >>> CI-PARITY-STEPS' ]]; then
        in_block=1
        continue
    fi
    # The end marker is a comment line that IS "# <<< CI-PARITY-STEPS".
    # (The description comment on the next line mentions <<< as text; the
    # actual marker is a standalone comment, so require exact match.)
    if [[ "$trimmed" == '# <<< CI-PARITY-STEPS' ]]; then
        in_block=0
        continue
    fi
    if [[ "$in_block" == 0 ]]; then
        continue
    fi
    # Rule 1: parity-cmd annotation on a run: >- or run: > line.
    # Pattern: `run: >...  # parity-cmd: <cmd>`
    if [[ "$line" =~ run:[[:space:]]*..*"# parity-cmd:"[[:space:]]*(.+)$ ]]; then
        cmd="${BASH_REMATCH[1]}"
        cmd="${cmd#"${cmd%%[! ]*}"}"  # trim leading spaces
        cmd="${cmd%"${cmd##*[! ]}"}"  # trim trailing spaces
        CI_BUILD_AND_TEST_STEPS+=("$cmd")
        continue
    fi
    # Rule 2: single-line run: value.
    # Skip lines whose run: value starts with > or | (multi-line block markers).
    if [[ "$line" =~ ^[[:space:]]+run:[[:space:]]+(.+)$ ]]; then
        cmd="${BASH_REMATCH[1]}"
        cmd="${cmd#"${cmd%%[! ]*}"}"   # trim leading spaces
        cmd="${cmd%"${cmd##*[! ]}"}"   # trim trailing spaces
        # Skip YAML block scalars (>-, >|, |-, ||, etc.)
        case "$cmd" in
            '>'*|'|'*) continue ;;
        esac
        CI_BUILD_AND_TEST_STEPS+=("$cmd")
        continue
    fi
done < "$CI_YML"

if (( ${#CI_BUILD_AND_TEST_STEPS[@]} == 0 )); then
    echo "error: no steps found in the CI-PARITY-STEPS block in $CI_YML" >&2
    echo "       Ensure the block markers '>>> CI-PARITY-STEPS' and '<<< CI-PARITY-STEPS'" >&2
    echo "       exist in .github/workflows/ci.yml around the build-and-test gating steps." >&2
    exit 1
fi

if (( VERBOSE == 1 )); then
    echo "==> CI build-and-test steps (parsed from CI-PARITY-STEPS block in ci.yml):"
    for step in "${CI_BUILD_AND_TEST_STEPS[@]}"; do
        echo "  - $step"
    done
    echo ""
fi

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
    i=$(( i + 1 ))
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
    if printf "%s\n" "$FALLBACK_CMDS" | grep -qF "$pattern"; then
        if (( VERBOSE == 1 )); then
            echo "  ok  [$label]: '$pattern' found"
        fi
        (( ++pass ))
    else
        echo "  FAIL [$label]: '$pattern' not found in fallback lane"
        echo "       CI requires this check; add it to the dispatcher's fallback lane."
        (( ++fail ))
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
# Assert that every command parsed from the CI-PARITY-STEPS block has at least
# one matching pattern in CI_REQUIRED_CHECKS.  This is the structural drift
# detector: a new unconditional step added inside the block without updating
# CI_REQUIRED_CHECKS fails here, blocking the merge via the lint CI job.
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
        (( ++subset_pass ))
    else
        echo "  FAIL CI step '$step_cmd' has no matching pattern in CI_REQUIRED_CHECKS."
        echo "       Add the step's local command to CI_REQUIRED_CHECKS in"
        echo "       scripts/ci-preflight-dispatcher.sh, OR add a '# parity-cmd: <local-cmd>'"
        echo "       annotation on the step's run: line in .github/workflows/ci.yml."
        (( ++subset_fail ))
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

# ── GAP-2: lane→gate assertions ────────────────────────────────────────────────
# For path classes that must run specific gates, assert the dispatcher dry-run
# for a representative path in that class actually includes the required gate.
#
# Classes covered:
#   trap-fixture paths  → must include make fuzz-oracle
#   vertical-slice paths → must include make fuzz-oracle (fuzz-oracle reads accept fixtures)
#   checker-strictness (hew-hir/hew-mir) → must include make fuzz-oracle
#   runtime paths → must include make fuzz-oracle
echo ""
echo "==> GAP-2: lane→gate assertions:"
gap_pass=0
gap_fail=0

_assert_lane_includes() {
    local description="$1"
    local required_gate="$2"
    shift 2
    local path_args=("$@")

    local dry_out
    dry_out=$("$DISPATCHER" --dry-run -- "${path_args[@]}" 2>&1)
    local cmds
    cmds=$(printf '%s\n' "$dry_out" | awk '/^Commands:/{found=1; next} found && /^  - /{cmd=substr($0,5); sub(/  \(budget:[^)]*\)$/, "", cmd); print cmd} found && /^(Dry run:|Commands: none)/{found=0}')

    if printf '%s\n' "$cmds" | grep -qF "$required_gate"; then
        if (( VERBOSE == 1 )); then
            echo "  ok  [$description]: '$required_gate' present"
        fi
        (( ++gap_pass ))
    else
        local selected_lane
        selected_lane=$(printf '%s\n' "$dry_out" | awk '/^Selected profile:/{print $NF; exit}')
        echo "  FAIL [$description]: '$required_gate' missing from lane '$selected_lane'"
        echo "       Path(s): ${path_args[*]}"
        echo "       Dispatcher output:"
        printf '%s\n' "$dry_out" | sed 's/^/         /'
        (( ++gap_fail ))
    fi
}

# trap-fixture paths: MIR bounds/trap lowering changes must reach fuzz-oracle.
_assert_lane_includes \
    "trap-fixture (hew-mir/src/lower.rs)" \
    "make fuzz-oracle" \
    "hew-mir/src/lower.rs"

# fuzz-oracle corpus directly: must reach fuzz-oracle.
_assert_lane_includes \
    "fuzz-oracle corpus (tests/fuzz-oracle/some_test.hew)" \
    "make fuzz-oracle" \
    "tests/fuzz-oracle/some_test.hew"

# vertical-slice/accept fixtures: fuzz-oracle reads these, so the gate must run.
_assert_lane_includes \
    "vertical-slice accept fixture" \
    "make fuzz-oracle" \
    "tests/vertical-slice/accept/some_fixture.hew"

# checker-strictness (hew-hir): HIR changes must reach fuzz-oracle.
_assert_lane_includes \
    "checker-strictness (hew-hir/src/lib.rs)" \
    "make fuzz-oracle" \
    "hew-hir/src/lib.rs"

# runtime path: runtime changes must reach fuzz-oracle.
_assert_lane_includes \
    "runtime path (hew-runtime/src/lib.rs)" \
    "make fuzz-oracle" \
    "hew-runtime/src/lib.rs"

echo "==> GAP-2 lane→gate: $gap_pass assertions passed."

if (( gap_fail > 0 )); then
    echo ""
    echo "FAIL: $gap_fail lane→gate assertion(s) failed."
    echo "      A path class that requires a specific gate is routed to a lane that omits it."
    echo "      Update the dispatcher lane for the failing path class to include the gate."
    exit 1
fi

echo "     All lane→gate assertions pass."
