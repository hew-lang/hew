#!/usr/bin/env bash
# forced-cancel-composite-check.sh — the #2437 dynamic proving gate.
#
# What this gate proves
# ──────────────────────
# `emit_cancel_trap_or_return`'s composite-return arm (hew-codegen-rs/src/
# llvm.rs, TaskEntry adapter) must synthesize a well-defined zero value on
# the cancel-exit edge, never load the unstored `return_slot` alloca. A Rust
# unit test over the codegen helper in isolation cannot prove this — the
# defect is only observable through the compiled ABI boundary a real
# `.hew` program's runtime reads. `make test-lane` cannot exercise it either:
# the trigger requires a task's OWN entry-block cooperate check to observe
# cancellation before it stores anything, which a normal build cannot force
# deterministically (see `hew-runtime`'s `forced-cancel-test` feature).
#
# This script builds `hew` + `libhew.a` with `--features
# hew-runtime/forced-cancel-test`, compiles+links the probe fixture against
# that build, and asserts the fixture observes the FIXED (well-defined zero)
# value — not the pre-fix uninitialized-stack garbage.
#
# Approach
# ────────
# 1. Build hew-cli + hew-lib with the `forced-cancel-test` feature into a
#    dedicated target dir (target/forced-cancel-gate/), isolated from the
#    default build so the test-only hook never reaches a production archive.
# 2. Compile the probe fixture to a relocatable object (`hew build
#    --emit-obj`), then link with clang against the feature-enabled
#    `libhew.a` (mirrors `asan-fixture-check.sh`'s manual-link pattern).
# 3. Run the binary; assert stdout is exactly `x=0 y=0` (the fixed
#    zero-initialized composite) and exit 0.
#
# WHEN OBSOLETE: if a future construct needs a general deterministic
# actor-cancellation test harness, this narrow gate is superseded by that —
# see the plan's "Out of scope" note on not growing this beyond #2437's own
# proving gate.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

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
PROBE_BIN="${WORK_DIR}/forced_cancel_composite_probe"

echo ""
echo "=== forced-cancel-composite-check: compiling probe ==="
( cd "${WORK_DIR}" && "${GATE_HEW}" build --emit-obj "${PROBE_SRC}" 2>&1 | sed 's/^/    /' )

if [[ ! -f "${PROBE_OBJ}" ]]; then
  echo "forced-cancel-composite-check: expected object ${PROBE_OBJ} not found after --emit-obj" >&2
  exit 1
fi

echo "  LINK forced_cancel_composite_probe"
darwin_frameworks=()
if [[ "$(uname -s)" == "Darwin" ]]; then
  darwin_frameworks=(-framework CoreFoundation -framework Security -framework SystemConfiguration)
fi
clang \
  "${PROBE_OBJ}" \
  "${GATE_LIBHEW}" \
  "${darwin_frameworks[@]}" \
  -o "${PROBE_BIN}"

if [[ ! -f "${PROBE_BIN}" ]]; then
  echo "forced-cancel-composite-check: linker produced no output at ${PROBE_BIN}" >&2
  exit 1
fi

echo ""
echo "=== forced-cancel-composite-check: running gate ==="
actual_exit=0
actual_stdout="$("${PROBE_BIN}")" || actual_exit=$?

# The fixed adapter zero-initializes the WHOLE composite return, so both
# fields read as their type's zero value.
expected_stdout="x=0 y=0"

if [[ "${actual_exit}" -ne 0 ]]; then
  echo "FAIL forced-cancel-composite-check: expected exit 0, got ${actual_exit}" >&2
  exit 1
fi

if [[ "${actual_stdout}" != "${expected_stdout}" ]]; then
  echo "FAIL forced-cancel-composite-check: expected stdout '${expected_stdout}', got '${actual_stdout}'" >&2
  echo "    (a pre-fix build observes non-deterministic uninitialized-stack" >&2
  echo "    garbage here instead of the well-defined zero composite)" >&2
  exit 1
fi

echo "PASS forced-cancel-composite-check: '${actual_stdout}', exit ${actual_exit}"
