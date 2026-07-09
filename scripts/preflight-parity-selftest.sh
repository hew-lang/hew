#!/usr/bin/env bash
# preflight-parity-selftest.sh - Self-proof for scripts/check-preflight-ci-parity.sh.
#
# Four independently-failable cases prove the parity checker distinguishes a
# genuinely-mirrored dispatcher from one that has silently dropped a CI-required
# check, using EXACT command matching rather than substring containment.
#
# Regression guard for the bug fixed in #2028: the checker previously matched CI
# steps to CI_REQUIRED_CHECKS (and required checks to the fallback lane) with
# bidirectional substring containment, so the bare `make test` (nextest workspace
# suite) was falsely treated as covered by `make test-vertical-slice` /
# `make test-hew-ratchet` / etc. A lane could drop `make test` entirely and the
# gate would still report parity. Cases (b)/(c) fail if that absorption returns.
#
# Exit codes:
#   0  all cases pass
#   1  one or more cases fail (details on stderr)

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKER="$SCRIPT_DIR/check-preflight-ci-parity.sh"

TMPDIR_BASE="$(mktemp -d /tmp/hew-preflight-parity-selftest.XXXXXX)"
trap 'rm -rf "$TMPDIR_BASE"' EXIT

pass() { echo "PASS $1"; }
fail() { echo "FAIL $1: $2" >&2; exit 1; }

# A stub ci.yml carrying the CI-PARITY-STEPS block the checker parses.  Two of
# the steps (`make test` and `make test-vertical-slice`) are substring-related on
# purpose: exact matching must keep them distinct.
CI_YML="$TMPDIR_BASE/ci.yml"
cat > "$CI_YML" <<'EOF'
jobs:
  build-and-test:
    steps:
      # >>> CI-PARITY-STEPS
      - name: nextest workspace
        run: >-  # parity-cmd: make test
          cargo nextest run --workspace --profile ci
      - name: vertical slice
        run: make test-vertical-slice
      # <<< CI-PARITY-STEPS
EOF

# Build a stub dispatcher.  MODE selects which required check / fallback command
# is (or is not) emitted, so each case exercises a distinct code path.
#   full       : both checks present in required list AND fallback lane  (PASS)
#   drop-fb    : `make test` in required list, MISSING from fallback lane (FAIL presence)
#   drop-both  : `make test` MISSING from required list AND fallback lane (FAIL subset)
#   substring  : fallback lane has ONLY `make test-vertical-slice` (a superstring of
#                `make test`) — proves exact matching does NOT absorb it   (FAIL presence)
make_dispatcher() {
    local mode="$1"
    local path="$TMPDIR_BASE/dispatcher-$mode.sh"
    cat > "$path" <<EOF
#!/usr/bin/env bash
set -euo pipefail
MODE="$mode"
EOF
    cat >> "$path" <<'EOF'
if [[ "${1:-}" == "--ci-required" ]]; then
    if [[ "$MODE" != "drop-both" ]]; then
        printf 'nextest workspace ci\tmake test\n'
    fi
    printf 'vertical slice\tmake test-vertical-slice\n'
    exit 0
fi
# --dry-run -- <file>  → emit the fallback "Commands:" block.  `make fuzz-oracle`
# is always present so the checker's GAP-2 lane→gate assertions are satisfied;
# these cases exercise only the `make test` / `make test-vertical-slice` parity.
echo "Commands:"
echo "  - make fuzz-oracle  (budget: 120s)"
case "$MODE" in
    full)
        echo "  - make test  (budget: 360s)"
        echo "  - make test-vertical-slice  (budget: 240s)"
        ;;
    drop-fb|drop-both)
        echo "  - make test-vertical-slice  (budget: 240s)"
        ;;
    substring)
        # Only the superstring is present; exact matching must NOT count it as
        # covering the bare `make test` required check.
        echo "  - make test-vertical-slice  (budget: 240s)"
        ;;
esac
echo "Dry run: fallback lane"
exit 0
EOF
    chmod +x "$path"
    echo "$path"
}

run_case() {
    local name="$1" mode="$2" expected_rc="$3"
    local disp log rc=0
    disp="$(make_dispatcher "$mode")"
    log="$TMPDIR_BASE/$name.log"
    echo "--- Case: $name ---"
    PREFLIGHT_PARITY_DISPATCHER="$disp" PREFLIGHT_PARITY_CI_YML="$CI_YML" \
        bash "$CHECKER" > "$log" 2>&1 || rc=$?
    if [[ "$rc" -eq "$expected_rc" ]]; then
        pass "$name"
    else
        sed 's/^/  /' "$log" >&2
        fail "$name" "checker exited $rc (expected $expected_rc)"
    fi
}

run_case "mirrored-parity-passes"          full      0
run_case "dropped-fallback-command-caught" drop-fb   1
run_case "dropped-required-and-lane-caught" drop-both 1
run_case "substring-superstring-not-absorbed" substring 1

echo ""
echo "preflight-parity-selftest: all 4 cases PASS"
