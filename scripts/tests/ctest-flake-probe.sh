#!/usr/bin/env bash
# ctest-flake-probe.sh — Loop a ctest invocation N times and report
# per-run pass/fail and a per-test failure histogram.
#
# Use to prove (or disprove) that a ctest selector is reliable under a
# given parallelism level before raising the default `-j` value, and to
# narrow down which tests are responsible for any flakes.
#
# Usage:
#   scripts/tests/ctest-flake-probe.sh [-n RUNS] [-j JOBS] [-d BUILD_DIR]
#                                      [-l LABEL_REGEX] [-o OUT_DIR]
#                                      [-- EXTRA_CTEST_ARGS ...]
#
# Defaults:
#   RUNS=10
#   JOBS=$(getconf _NPROCESSORS_ONLN || sysctl -n hw.ncpu || nproc || echo 1)
#   BUILD_DIR=hew-codegen/build
#   LABEL_REGEX="-LE wasm"   (passed verbatim; quote labels with spaces)
#   OUT_DIR=$(mktemp -d -t ctest-flake-XXXXXX)
#
# Examples:
#   # Default sweep used by `make test-codegen`
#   scripts/tests/ctest-flake-probe.sh -n 10 -j 16
#
#   # The wasm sweep used by `make test-wasm`
#   scripts/tests/ctest-flake-probe.sh -n 10 -j 8 -l "-L wasm"
#
#   # Narrow probe of a single suspicious test
#   scripts/tests/ctest-flake-probe.sh -n 20 -j 16 \
#       -- -R '^e2e_supervisor_supervisor_nested$'
#
# Exit status: 0 if every run passed, 1 if any run failed.

set -u

RUNS=10
JOBS=""
BUILD_DIR="hew-codegen/build"
LABEL_REGEX="-LE wasm"
OUT_DIR=""
EXTRA=()

usage() {
  sed -n '1,32p' "$0"
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    -n) RUNS="$2"; shift 2 ;;
    -j) JOBS="$2"; shift 2 ;;
    -d) BUILD_DIR="$2"; shift 2 ;;
    -l) LABEL_REGEX="$2"; shift 2 ;;
    -o) OUT_DIR="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    --) shift; EXTRA=("$@"); break ;;
    *) echo "unknown arg: $1" >&2; usage >&2; exit 2 ;;
  esac
done

if [ -z "$JOBS" ]; then
  JOBS=$(getconf _NPROCESSORS_ONLN 2>/dev/null \
         || getconf NPROCESSORS_ONLN 2>/dev/null \
         || nproc 2>/dev/null \
         || sysctl -n hw.ncpu 2>/dev/null \
         || echo 1)
fi

if [ ! -d "$BUILD_DIR" ]; then
  echo "build dir not found: $BUILD_DIR" >&2
  echo "run \`make codegen\` (or your project's equivalent) first" >&2
  exit 2
fi

if [ -z "$OUT_DIR" ]; then
  OUT_DIR=$(mktemp -d -t ctest-flake-XXXXXX)
fi
mkdir -p "$OUT_DIR"

echo "ctest-flake-probe: runs=$RUNS jobs=$JOBS build=$BUILD_DIR labels='$LABEL_REGEX' out=$OUT_DIR"
[ "${#EXTRA[@]:-0}" -gt 0 ] && echo "extra ctest args: ${EXTRA[*]}"

PASS=0
FAIL=0
FAILED_RUNS=()

# shellcheck disable=SC2086 # LABEL_REGEX is intentionally word-split
for i in $(seq -f "%02g" 1 "$RUNS"); do
  log="$OUT_DIR/run$i.txt"
  start=$(date +%s)
  ( cd "$BUILD_DIR" && ctest --output-on-failure $LABEL_REGEX -j"$JOBS" ${EXTRA[@]+"${EXTRA[@]}"} ) \
    > "$log" 2>&1
  rc=$?
  elapsed=$(( $(date +%s) - start ))
  summary=$(grep -E 'tests passed,.*tests failed out of' "$log" | tail -1)
  printf "run %s: rc=%d %ds | %s\n" "$i" "$rc" "$elapsed" "${summary:-<no summary>}"
  if [ "$rc" -eq 0 ]; then
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
    FAILED_RUNS+=("$i")
  fi
done

echo
echo "=== aggregate ==="
echo "passed: $PASS / $RUNS"
echo "failed: $FAIL / $RUNS"

if [ "$FAIL" -gt 0 ]; then
  echo
  echo "=== per-test failure histogram (failures across all runs) ==="
  # Lines look like: `123/797 Test #45: foo_bar ........ ***Failed   10.18 sec`
  # Extract the test name and tally.
  grep -hE '\*\*\*Failed|\*\*\*Timeout' "$OUT_DIR"/run*.txt 2>/dev/null \
    | sed -E 's/^[ 0-9]+\/[0-9]+ Test #?[0-9]+: ([^ .]+).*/\1/' \
    | sort | uniq -c | sort -rn
  echo
  echo "failed runs: ${FAILED_RUNS[*]}"
  echo "logs in: $OUT_DIR"
  exit 1
fi

echo "logs in: $OUT_DIR"
exit 0
