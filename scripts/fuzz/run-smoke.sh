#!/usr/bin/env bash
#
# run-smoke.sh — bounded libFuzzer smoke run over the parser fuzz targets
# (`make fuzz-smoke`).
#
# WHY bounded and nightly-only: an unbounded per-PR fuzz run is
# nondeterministic by construction (a corpus mutation that trips one run and
# not the next), which violates the repo's determinism doctrine for per-PR
# gates. This script runs each target for a fixed wall-clock budget
# (-max_total_time) and fails the build on any crash artifact libFuzzer
# writes — it is wired into .github/workflows/nightly-sanitizers.yml, never
# into the per-PR ci.yml. The differential fuzz-oracle (`make fuzz-oracle`)
# stays the deterministic per-PR gate.
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PARSER_DIR="${ROOT}/hew-parser"

TARGETS=(fuzz_structured fuzz_mir fuzz_check)
MAX_TOTAL_TIME="${FUZZ_SMOKE_MAX_TOTAL_TIME:-120}"

cd "${PARSER_DIR}" || exit 1

overall_status=0
for target in "${TARGETS[@]}"; do
  echo "── fuzz smoke: ${target} (max_total_time=${MAX_TOTAL_TIME}s) ──"
  artifact_dir="fuzz/artifacts/${target}"
  before="$(find "${artifact_dir}" -type f 2>/dev/null | sort)"

  cargo +nightly fuzz run "${target}" -- "-max_total_time=${MAX_TOTAL_TIME}"
  run_status=$?

  after="$(find "${artifact_dir}" -type f 2>/dev/null | sort)"
  new_artifacts="$(comm -13 <(printf '%s\n' "${before}") <(printf '%s\n' "${after}"))"

  if [[ ${run_status} -ne 0 ]]; then
    echo "fuzz smoke: ${target} exited non-zero (status=${run_status})" >&2
    overall_status=1
  fi
  if [[ -n "${new_artifacts}" ]]; then
    echo "fuzz smoke: ${target} wrote crash artifact(s):" >&2
    printf '%s\n' "${new_artifacts}" >&2
    overall_status=1
  fi
done

exit "${overall_status}"
