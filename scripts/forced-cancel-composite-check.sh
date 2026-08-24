#!/usr/bin/env bash
# forced-cancel-composite-check.sh — the #2437 dynamic proving gate.
#
# What this gate proves
# ──────────────────────
# A TaskEntry adapter must not turn its ABI-only cancellation return into a
# published task result. A Rust unit test over the codegen helper in isolation
# cannot prove this — the defect is only observable through the compiled ABI
# boundary a real `.hew` program's runtime reads. A plain `cargo nextest` run
# cannot exercise it either: the trigger requires a task's OWN entry-block
# cooperate check to observe cancellation before it stores anything, which a
# normal build cannot force deterministically.
#
# This script builds `hew` + `libhew.a` with `--features
# hew-runtime/forced-cancel-test`, compiles+links the probe fixture against
# that build, and asserts the fixture observes cancellation while a separate
# genuine all-zero return remains a successful value.
#
# Approach
# ────────
# 1. Build hew-cli + hew-lib with the `forced-cancel-test` feature into a
#    dedicated target dir (target/forced-cancel-gate/), isolated from the
#    default build so the test-only hook never reaches a production archive.
# 2. Compile the probe fixture to a relocatable object (`hew build
#    --emit-obj --emit-llvm`), then link with clang against the feature-enabled
#    `libhew.a` (mirrors `asan-fixture-check.sh`'s manual-link pattern).
# 3. Run the binary; assert the cancelled value task reports cancellation, the
#    genuine zero task reports `x=0 y=0`, and a post-publication cancellation
#    releases an owned string result exactly once.
#
# WHEN OBSOLETE: if a future construct needs a general deterministic
# actor-cancellation test harness, this narrow gate is superseded by that —
# see the plan's "Out of scope" note on not growing this beyond #2437's own
# proving gate.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
# shellcheck source=scripts/lib/timeout.sh
# shellcheck disable=SC1091
source "${ROOT}/scripts/lib/timeout.sh"

PROBE_TIMEOUT_SECONDS=60

if ! command -v clang >/dev/null 2>&1; then
  echo "forced-cancel-composite-check: clang not found — install llvm/clang" >&2
  exit 1
fi

GATE_TARGET_DIR="${ROOT}/target/forced-cancel-gate"
WORK_DIR="${ROOT}/.tmp/forced-cancel-gate-out"
mkdir -p "${WORK_DIR}"

echo "=== forced-cancel-composite-check: building hew + libhew.a with forced-cancel-test (may be slow on a cold cache) ==="
CARGO_TARGET_DIR="${GATE_TARGET_DIR}" \
  cargo build -p hew-cli -p hew-lib --features hew-runtime/forced-cancel-test --quiet

GATE_BIN_DIR="${GATE_TARGET_DIR}/debug"
GATE_HEW="${GATE_BIN_DIR}/hew"
GATE_LIBHEW="${GATE_BIN_DIR}/libhew.a"

for f in "${GATE_HEW}" "${GATE_LIBHEW}"; do
  if [[ ! -f "${f}" ]]; then
    echo "forced-cancel-composite-check: expected build artefact not found: ${f}" >&2
    exit 1
  fi
done

echo "  hew binary : ${GATE_HEW}"
echo "  libhew.a   : ${GATE_LIBHEW}"

PROBE_SRC="${ROOT}/scripts/fixtures/forced-cancel-gate/forced_cancel_composite_probe.hew"
PROBE_OBJ="${WORK_DIR}/forced_cancel_composite_probe.o"
PROBE_LL="${WORK_DIR}/forced_cancel_composite_probe.ll"
PROBE_BIN="${WORK_DIR}/forced_cancel_composite_probe"

echo ""
echo "=== forced-cancel-composite-check: compiling probe ==="
( cd "${WORK_DIR}" && "${GATE_HEW}" build --emit-obj --emit-llvm "${PROBE_SRC}" 2>&1 | sed 's/^/    /' )

if [[ ! -f "${PROBE_OBJ}" ]]; then
  echo "forced-cancel-composite-check: expected object ${PROBE_OBJ} not found after --emit-obj" >&2
  exit 1
fi

if [[ ! -f "${PROBE_LL}" ]]; then
  echo "forced-cancel-composite-check: expected LLVM IR ${PROBE_LL} not found after --emit-obj" >&2
  exit 1
fi

# The post-publication cancellation case leaves an owned string in the task
# result buffer. The wrapper must register its exact in-place destructor, and
# that destructor must release the embedded string. Runtime task teardown calls
# it only when the awaiter did not consume the buffer; the leak oracle below
# proves the dynamic leg.
if ! grep -Eq \
  'call void @hew_task_set_result_drop_fn\(ptr %[^,]+, ptr @__hew_reply_drop_string\)' \
  "${PROBE_LL}"; then
  echo "FAIL forced-cancel-composite-check: owned task result has no typed destructor" >&2
  exit 1
fi
if ! awk '
  /define internal void @__hew_reply_drop_string/ { in_drop = 1 }
  in_drop && /call void @hew_string_drop\(/ { released = 1 }
  in_drop && /^}/ { in_drop = 0 }
  END { exit released ? 0 : 1 }
' "${PROBE_LL}"; then
  echo "FAIL forced-cancel-composite-check: owned task-result destructor does not release its string" >&2
  exit 1
fi

echo "  LINK forced_cancel_composite_probe"
platform_link_args=()
case "$(uname -s)" in
  Darwin)
    platform_link_args=(-framework CoreFoundation -framework Security -framework SystemConfiguration)
    ;;
  Linux)
    platform_link_args=(-lm)
    ;;
esac
clang \
  "${PROBE_OBJ}" \
  "${GATE_LIBHEW}" \
  "${platform_link_args[@]}" \
  -o "${PROBE_BIN}"

if [[ ! -f "${PROBE_BIN}" ]]; then
  echo "forced-cancel-composite-check: linker produced no output at ${PROBE_BIN}" >&2
  exit 1
fi

echo ""
echo "=== forced-cancel-composite-check: running gate ==="
actual_exit=0
printf -v probe_command '%q' "${PROBE_BIN}"
actual_stdout="$(run_in_pgroup_with_timeout "${PROBE_TIMEOUT_SECONDS}" "${probe_command}")" || actual_exit=$?

if [[ "${actual_exit}" -eq 143 || "${actual_exit}" -eq 137 ]]; then
  echo "FAIL forced-cancel-composite-check: probe exceeded ${PROBE_TIMEOUT_SECONDS}s" >&2
  exit 1
fi

# Cancellation must surface without a payload; an independently completed zero
# result must still be delivered. The owned result releases silently.
expected_stdout=$'zero x=0 y=0\ncancelled\nowned cancelled'

if [[ "${actual_exit}" -ne 0 ]]; then
  echo "FAIL forced-cancel-composite-check: expected exit 0, got ${actual_exit}" >&2
  exit 1
fi

if [[ "${actual_stdout}" != "${expected_stdout}" ]]; then
  echo "FAIL forced-cancel-composite-check: expected stdout '${expected_stdout}', got '${actual_stdout}'" >&2
  echo "    (a pre-fix build reports the cancelled task as a successful zero value)" >&2
  exit 1
fi

if [[ "$(uname -s)" == "Darwin" ]] && command -v leaks >/dev/null 2>&1; then
  printf -v leaks_command 'leaks --atExit -- %q' "${PROBE_BIN}"
  leaks_output="$(run_in_pgroup_with_timeout "${PROBE_TIMEOUT_SECONDS}" "${leaks_command}" 2>&1)" || {
    echo "FAIL forced-cancel-composite-check: leaks --atExit failed" >&2
    echo "${leaks_output}" >&2
    exit 1
  }
  if ! grep -Eq '0 leaks for 0 total leaked bytes\.' <<<"${leaks_output}"; then
    echo "FAIL forced-cancel-composite-check: forced cancellation leaked the owned result" >&2
    echo "${leaks_output}" >&2
    exit 1
  fi
fi

echo "PASS forced-cancel-composite-check: cancellation surfaced; genuine zero preserved; owned result cleanup wired; exit ${actual_exit}"
