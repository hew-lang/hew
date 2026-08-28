#!/usr/bin/env bash
# SIR shadow corpus driver.
#
# This is a temporary cutover proof, not a second supported compiler mode.  For
# each fixture it invokes the established pipeline and the `--sir-shadow` lane
# separately, then requires the observable compiler result to agree.  In
# particular, raw MIR stdout must be byte-identical (the shadow lane must
# return the established pipeline), while stderr is compared after removing
# only the SIR coverage report that `--sir-shadow` deliberately adds.  A
# changed diagnostic, exit status, or emitted raw MIR is a failure.  Remove this
# parity gate when SIR lowering replaces the established path by default.
#
# The default corpus is tests/ll-oracle/corpus, whose fixtures are standalone
# complete programs and library modules intended to reach the native backend.
# Additional files and directories may be supplied to exercise a focused
# surface while a new SIR lowering slice is being developed.
#
# Usage:
#   scripts/sir-shadow-corpus.sh
#   scripts/sir-shadow-corpus.sh path/to/fixture.hew path/to/corpus-dir
#
# Environment:
#   HEW_BIN                    compiler binary (default: target/debug/hew)
#   SIR_SHADOW_MIN_REALIZED    minimum total SIR→raw-MIR realizations (default: 2; may only raise)
#   SIR_SHADOW_MIN_SUCCESSES   minimum successful baseline compilations (default: 16; may only raise)

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CALLER_DIR="$PWD"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$ROOT/scripts/lib/corpus-nonempty.sh"

HEW_BIN="${HEW_BIN:-$ROOT/target/debug/hew}"
DEFAULT_CORPUS="$ROOT/tests/ll-oracle/corpus"
REALIZED_FLOOR=2
SUCCESS_FLOOR=16
MIN_REALIZED="${SIR_SHADOW_MIN_REALIZED:-$REALIZED_FLOOR}"
MIN_SUCCESSES="${SIR_SHADOW_MIN_SUCCESSES:-$SUCCESS_FLOOR}"
# Repeat the established compile enough times to make randomized ownership-fact
# emission fail closed without turning comparison into a set operation.  The
# exact EdgeCarry sequence is compiler output and must remain byte-identical.
DETERMINISM_RUNS=4

usage() {
    cat <<'EOF'
Usage: scripts/sir-shadow-corpus.sh [fixture.hew | directory ...]

Compare the established compiler pipeline with `--sir-shadow`.  With no
arguments, runs every top-level .hew fixture in tests/ll-oracle/corpus.

Environment:
  HEW_BIN                    compiler binary (default: target/debug/hew)
  SIR_SHADOW_MIN_REALIZED    minimum total SIR→raw-MIR realizations (default: 2; may only raise)
  SIR_SHADOW_MIN_SUCCESSES   minimum successful baseline compilations (default: 16; may only raise)
EOF
}

require_nonnegative_integer() {
    local name="$1"
    local value="$2"
    if [[ ! "$value" =~ ^[0-9]+$ ]]; then
        echo "sir-shadow-corpus: $name must be a non-negative integer, got '$value'" >&2
        exit 2
    fi
}

require_nonnegative_integer SIR_SHADOW_MIN_REALIZED "$MIN_REALIZED"
require_nonnegative_integer SIR_SHADOW_MIN_SUCCESSES "$MIN_SUCCESSES"
if (( MIN_REALIZED < REALIZED_FLOOR )); then
    echo "sir-shadow-corpus: SIR_SHADOW_MIN_REALIZED may not lower the committed floor $REALIZED_FLOOR" >&2
    exit 2
fi
if (( MIN_SUCCESSES < SUCCESS_FLOOR )); then
    echo "sir-shadow-corpus: SIR_SHADOW_MIN_SUCCESSES may not lower the committed floor $SUCCESS_FLOOR" >&2
    exit 2
fi

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    usage
    exit 0
fi

if [[ ! -x "$HEW_BIN" ]]; then
    echo "sir-shadow-corpus: compiler binary not found at $HEW_BIN" >&2
    echo "build it first (make hew-native) or set HEW_BIN=<path>" >&2
    exit 2
fi
if [[ "$HEW_BIN" != /* ]]; then
    HEW_BIN="$(cd "$(dirname "$HEW_BIN")" && pwd)/$(basename "$HEW_BIN")"
fi

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
fixture_list="$tmpdir/fixtures"
: >"$fixture_list"

resolve_input() {
    local input="$1"
    if [[ "$input" == /* ]]; then
        printf '%s\n' "$input"
    else
        printf '%s/%s\n' "$CALLER_DIR" "$input"
    fi
}

add_input() {
    local input="$1"
    if [[ -f "$input" ]]; then
        case "$input" in
            *.hew) printf '%s\n' "$input" >>"$fixture_list" ;;
            *)
                echo "sir-shadow-corpus: fixture is not a .hew file: $input" >&2
                exit 2
                ;;
        esac
    elif [[ -d "$input" ]]; then
        while IFS= read -r fixture; do
            printf '%s\n' "$fixture" >>"$fixture_list"
        done < <(find "$input" -type f -name '*.hew' -print | LC_ALL=C sort)
    else
        echo "sir-shadow-corpus: no such fixture or directory: $input" >&2
        exit 2
    fi
}

if [[ $# -eq 0 ]]; then
    # Keep the default deliberately shallow: LL-oracle fixtures are individual
    # compiler inputs.  A caller can pass a directory explicitly when a
    # recursive surface slice is appropriate.
    while IFS= read -r fixture; do
        printf '%s\n' "$fixture" >>"$fixture_list"
    done < <(find "$DEFAULT_CORPUS" -maxdepth 1 -type f -name '*.hew' -print | LC_ALL=C sort)
else
    for argument in "$@"; do
        add_input "$(resolve_input "$argument")"
    done
fi

LC_ALL=C sort -u "$fixture_list" >"$tmpdir/fixtures.sorted"
mv "$tmpdir/fixtures.sorted" "$fixture_list"
fixtures=()
while IFS= read -r fixture; do
    fixtures+=("$fixture")
done <"$fixture_list"
corpus_nonempty_assert "sir-shadow-fixtures" "${#fixtures[@]}" || exit 1

# The report is informational for humans, but it is also the proof that this
# gate actually reached the experimental lane.  It intentionally stays on
# stderr so `--dump-mir` retains its ordinary stdout contract.  Keep this
# parser exact: the five counts distinguish monomorphic HIR bodies, generic
# templates, HIR declarations, realized concrete SIR bodies, and the concrete
# SIR-body total.  Loosening it would let a reporting drift turn this coverage
# gate into a green no-op.
extract_report() {
    local stderr_path="$1"
    sed -nE \
        's/^SIR shadow: verified ([0-9]+) monomorphic HIR body\/bodies and registered ([0-9]+) generic template\(s\) across ([0-9]+) HIR function declaration\(s\); realized ([0-9]+)\/([0-9]+) concrete SIR body\/bodies through raw MIR$/\1 \2 \3 \4 \5/p' \
        "$stderr_path"
}

# Strip only the known coverage lines.  Do not broadly suppress stderr: a
# diagnostic emitted by the shadow candidate must match the baseline just as an
# ordinary compiler diagnostic would.
without_sir_report() {
    local stderr_path="$1"
    sed -E '/^SIR (shadow|HIR|raw-MIR)(:| fallback )/d' "$stderr_path"
}

run_compile() {
    local output_path="$1"
    local stderr_path="$2"
    local status=0
    shift 2
    "$@" >"$output_path" 2>"$stderr_path" || status=$?
    return "$status"
}

edge_carry_sequence() {
    local output_path="$1"
    sed -n '/ownership EdgeCarry/p' "$output_path"
}

failures=0
successes=0
verified=0
generic_templates=0
hir_declarations=0
realized=0
concrete_bodies=0

for index in "${!fixtures[@]}"; do
    fixture="${fixtures[$index]}"
    label="${fixture#"$ROOT"/}"
    baseline_out="$tmpdir/$index.baseline.out"
    baseline_err="$tmpdir/$index.baseline.err"
    baseline_edges="$tmpdir/$index.baseline.edges"
    shadow_out="$tmpdir/$index.shadow.out"
    shadow_err="$tmpdir/$index.shadow.err"
    normalized_shadow_err="$tmpdir/$index.shadow.normalized.err"

    baseline_status=0
    run_compile "$baseline_out" "$baseline_err" \
        "$HEW_BIN" compile --dump-mir raw "$fixture" || baseline_status=$?
    edge_carry_sequence "$baseline_out" >"$baseline_edges"
    shadow_status=0
    run_compile "$shadow_out" "$shadow_err" \
        "$HEW_BIN" compile --sir-shadow --dump-mir raw "$fixture" || shadow_status=$?

    fixture_failed=0
    for ((run = 2; run <= DETERMINISM_RUNS; run++)); do
        repeated_out="$tmpdir/$index.baseline.$run.out"
        repeated_err="$tmpdir/$index.baseline.$run.err"
        repeated_edges="$tmpdir/$index.baseline.$run.edges"
        repeated_status=0
        run_compile "$repeated_out" "$repeated_err" \
            "$HEW_BIN" compile --dump-mir raw "$fixture" || repeated_status=$?
        edge_carry_sequence "$repeated_out" >"$repeated_edges"
        if [[ "$baseline_status" -ne "$repeated_status" ]]; then
            echo "FAIL $label: repeated compile $run changed exit status from $baseline_status to $repeated_status" >&2
            fixture_failed=1
        fi
        if ! diff -u "$baseline_edges" "$repeated_edges"; then
            echo "FAIL $label: repeated compile $run reordered EdgeCarry facts" >&2
            fixture_failed=1
        fi
    done

    if [[ "$baseline_status" -ne "$shadow_status" ]]; then
        echo "FAIL $label: exit status baseline=$baseline_status shadow=$shadow_status" >&2
        fixture_failed=1
    fi

    if ! diff -u "$baseline_out" "$shadow_out"; then
        echo "FAIL $label: stdout differs between baseline and SIR shadow" >&2
        fixture_failed=1
    fi

    without_sir_report "$shadow_err" >"$normalized_shadow_err"
    if ! diff -u "$baseline_err" "$normalized_shadow_err"; then
        echo "FAIL $label: diagnostics differ after removing the SIR coverage report" >&2
        fixture_failed=1
    fi

    if [[ "$baseline_status" -eq 0 ]]; then
        successes=$((successes + 1))
        report="$(extract_report "$shadow_err")"
        if [[ -z "$report" ]]; then
            echo "FAIL $label: successful --sir-shadow run produced no SIR coverage report" >&2
            fixture_failed=1
        elif [[ "$(printf '%s\n' "$report" | wc -l | tr -d ' ')" -ne 1 ]]; then
            echo "FAIL $label: ambiguous SIR coverage report" >&2
            fixture_failed=1
        else
            read -r fixture_verified fixture_templates fixture_bodies fixture_realized fixture_concrete_bodies _ <<<"$report"
            verified=$((verified + fixture_verified))
            generic_templates=$((generic_templates + fixture_templates))
            hir_declarations=$((hir_declarations + fixture_bodies))
            realized=$((realized + fixture_realized))
            concrete_bodies=$((concrete_bodies + fixture_concrete_bodies))
            printf 'ok   %s  (SIR %s monomorphic, %s template(s), %s HIR declarations, %s/%s concrete realized)\n' \
                "$label" "$fixture_verified" "$fixture_templates" "$fixture_bodies" \
                "$fixture_realized" "$fixture_concrete_bodies"
        fi
    fi

    if [[ "$fixture_failed" -ne 0 ]]; then
        failures=$((failures + 1))
    fi
done

if [[ "$successes" -lt "$MIN_SUCCESSES" ]]; then
    echo "FAIL sir-shadow-corpus: only $successes successful baseline compilations; require $MIN_SUCCESSES" >&2
    failures=$((failures + 1))
fi
if [[ "$realized" -lt "$MIN_REALIZED" ]]; then
    echo "FAIL sir-shadow-corpus: only $realized SIR→raw-MIR realizations; require $MIN_REALIZED" >&2
    failures=$((failures + 1))
fi

if [[ "$failures" -ne 0 ]]; then
    echo "sir-shadow-corpus: FAILED ($failures fixture/parity requirement failure(s))" >&2
    exit 1
fi

echo "sir-shadow-corpus: OK (${#fixtures[@]} fixtures, $successes successful baseline compiles, SIR $verified monomorphic, $generic_templates template(s), $hir_declarations HIR declarations, $realized/$concrete_bodies concrete realized)"
