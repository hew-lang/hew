#!/usr/bin/env bash
# asan-fixture-check.sh — compile .hew fixture programs against an
# ASan/LSan-instrumented libhew.a and run them under leak detection.
#
# What this gate proves
# ─────────────────────
# Memory leaks in the GENERATED CODE emitted by hew (the compiled .hew binary
# linked against libhew.a) are detected on Linux.  The existing `make asan`
# target (cargo +nightly test -p hew-runtime --lib) only instruments the Rust
# crate; it cannot see leaks in the LLVM IR / clang-linked artefact.
#
# The Vec<string> index-into-compare leak and the owned array-repeat
# Vec<string> clone leak were both caught only by the macOS `leaks --atExit`
# oracle, not by `make asan`.  This gate closes that coverage gap on Linux.
#
# Approach
# ─────────
# 1. Build the hew CLI + libhew.a with nightly Rust + -Zsanitizer=address into
#    a dedicated target dir (target/sanitizer-fixture-asan/).
# 2. Use the resulting ASan hew binary to compile .hew fixtures to object files
#    (hew build --emit-obj), then link with clang -fsanitize=address against the
#    ASan libhew.a.
# 3. Run each binary with ASAN_OPTIONS=detect_leaks=1.
#
# Two fixture probes:
#   clean-probe  — a compiled Hew binary exercising the previously-leaky shapes
#                  (now fixed); must produce ZERO ASan/LSan findings.
#   leak-proof   — a pure C binary compiled with -fsanitize=address that leaks
#                  1 KiB via malloc; the gate must catch it, proving ASan/LSan
#                  fires correctly in binaries produced by this pipeline.
#
# SHIM: Linux-only gate.  On macOS the leak oracle is the `leaks --atExit`
# path in hew-cli/tests/*_leak_oracle.rs; ASan + LSan on Darwin does not
# support standalone leak detection in the same way.
# WHEN obsolete: when `hew build --sanitize=address` is a real CLI surface and
# the build system selects the sanitizer libhew.a automatically.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# ── Platform guard ────────────────────────────────────────────────────────
if [[ "$(uname -s)" != "Linux" ]]; then
  echo "asan-fixture-check: skipped on $(uname -s) (use the macOS leaks oracle)" >&2
  exit 0
fi

# ── Parse arguments and environment ──────────────────────────────────────
LLVM_VERSION="${LLVM_VERSION:-}"
SANITIZER_TARGET="${SANITIZER_RUST_TARGET:-x86_64-unknown-linux-gnu}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --llvm-version) LLVM_VERSION="$2"; shift 2 ;;
    *) echo "asan-fixture-check: unknown argument: $1" >&2; exit 1 ;;
  esac
done

# ── Tool availability ─────────────────────────────────────────────────────
if ! command -v clang >/dev/null 2>&1; then
  echo "asan-fixture-check: clang not found — install llvm/clang" >&2
  exit 1
fi

if ! rustup toolchain list 2>/dev/null | grep -q nightly; then
  echo "asan-fixture-check: nightly toolchain not installed — run: rustup toolchain install nightly" >&2
  exit 1
fi

# Locate the best available llvm-symbolizer for human-readable ASan reports.
if [[ -n "${LLVM_VERSION}" ]]; then
  ASAN_SYMBOLIZER_PATH="/usr/lib/llvm-${LLVM_VERSION}/bin/llvm-symbolizer"
else
  ASAN_SYMBOLIZER_PATH="$(find /usr/lib -name llvm-symbolizer -path '*/llvm-*/bin/*' 2>/dev/null \
    | sort -V | tail -1 || true)"
fi
export ASAN_SYMBOLIZER_PATH

# ── Directories ───────────────────────────────────────────────────────────
ASAN_FIXTURE_TARGET_DIR="${ROOT}/target/sanitizer-fixture-asan"
WORK_DIR="${ROOT}/.tmp/asan-fixture-out"
mkdir -p "${WORK_DIR}"

# ── Step 1: build ASan-instrumented hew + libhew.a ───────────────────────
# Build with -Zsanitizer=address so the runtime's malloc/free paths are
# instrumented.  The resulting hew binary and libhew.a live in the same
# directory; hew's find_hew_lib resolves libhew.a next to the binary
# (hew-cli/src/link.rs hew_lib_candidates: exe_dir.join(name)).
#
# -Cunsafe-allow-abi-mismatch=sanitizer: same flag as `make asan` — silences
# the ABI mismatch error when linking sanitizer-instrumented crates against the
# prebuilt (uninstrumented) sysroot std.
echo "=== asan-fixture-check: building ASan hew + libhew.a (nightly, may be slow on a cold cache) ==="

CARGO_TARGET_DIR="${ASAN_FIXTURE_TARGET_DIR}" \
RUSTFLAGS="-Zsanitizer=address -Cforce-frame-pointers=yes -Cunsafe-allow-abi-mismatch=sanitizer" \
  cargo +nightly build \
    --target "${SANITIZER_TARGET}" \
    -p hew-cli \
    -p hew-lib \
    --quiet

ASAN_BIN_DIR="${ASAN_FIXTURE_TARGET_DIR}/${SANITIZER_TARGET}/debug"
ASAN_HEW="${ASAN_BIN_DIR}/hew"
ASAN_LIBHEW="${ASAN_BIN_DIR}/libhew.a"

for f in "${ASAN_HEW}" "${ASAN_LIBHEW}"; do
  if [[ ! -f "${f}" ]]; then
    echo "asan-fixture-check: expected build artefact not found: ${f}" >&2
    exit 1
  fi
done

echo "  hew binary : ${ASAN_HEW}"
echo "  libhew.a   : ${ASAN_LIBHEW}"

# ── Helper: compile one .hew source to a native binary with ASan ─────────
# Uses hew build --emit-obj to get the LLVM-compiled object (bypassing hew's
# own link step), then links with clang -fsanitize=address against the ASan
# libhew.a.
#
# --emit-obj writes <stem>.o into the CWD, so we cd to WORK_DIR first.
# HEW_SANITIZE_ADDRESS=1 is set on the hew build call but does not affect
# --emit-obj (object emission only; link flags are for the link step which we
# override manually here).
compile_asan_fixture() {
  local label="$1"
  local src="$2"
  local out_bin="$3"

  local stem
  stem="$(basename "${src}" .hew)"
  local obj_file="${WORK_DIR}/${stem}.o"

  echo "  EMIT-OBJ ${label}"

  # The ASan hew binary finds the ASan libhew.a next to itself; use it here
  # for the emission step so codegen is consistent with the library ABI.
  ( cd "${WORK_DIR}" && \
    "${ASAN_HEW}" build --emit-obj "${src}" 2>&1 | sed 's/^/    /' )

  if [[ ! -f "${obj_file}" ]]; then
    echo "asan-fixture-check: expected object ${obj_file} not found after --emit-obj" >&2
    return 1
  fi

  echo "  LINK     ${label}"

  # Link with clang -fsanitize=address.  The -fsanitize=address flag tells
  # clang to pull in the ASan runtime (libclang_rt.asan-x86_64.a) and wire
  # __asan_init.  Without it the binary links cleanly but ASan is not
  # initialised — all leaks are silently missed.
  #
  # libhew.a is listed twice: once to resolve the object's direct runtime
  # references, and once after any extra objects for backward references
  # (mirrors the two-listing pattern in hew-cli/src/link.rs).
  clang \
    -fsanitize=address \
    -fno-omit-frame-pointer \
    -target "${SANITIZER_TARGET}" \
    "${obj_file}" \
    "${@:4}" \
    "${ASAN_LIBHEW}" \
    -lpthread -ldl -lm \
    "${ASAN_LIBHEW}" \
    -o "${out_bin}"

  if [[ ! -f "${out_bin}" ]]; then
    echo "asan-fixture-check: linker produced no output at ${out_bin}" >&2
    return 1
  fi
}

# ── Helper: run one binary under ASan/LSan ────────────────────────────────
# Returns 0 if the binary exits cleanly with no ASan/LSan findings.
# Returns 1 otherwise.
run_asan_fixture() {
  local label="$1"
  local bin="$2"
  local expected_exit="${3:-0}"

  echo "  RUN      ${label}"

  local actual_exit=0
  local log_prefix="${bin}.asan"

  ASAN_OPTIONS="detect_leaks=1:log_path=${log_prefix}" \
  ASAN_SYMBOLIZER_PATH="${ASAN_SYMBOLIZER_PATH}" \
  LSAN_OPTIONS="suppressions=${ROOT}/hew-runtime/lsan.supp" \
  HEW_WORKERS=1 \
    "${bin}" >/dev/null 2>/dev/null || actual_exit=$?

  # Collect all per-PID log files written by ASan.
  local asan_output=""
  for f in "${log_prefix}".*; do
    [[ -f "${f}" ]] || continue
    asan_output+="$(cat "${f}")"$'\n'
    rm -f "${f}"
  done

  # An ASan/LSan finding always contains one of these marker strings.
  if printf '%s' "${asan_output}" \
       | grep -qE "ERROR: (AddressSanitizer|LeakSanitizer)|SUMMARY: (AddressSanitizer|LeakSanitizer)"; then
    echo "    FAIL ${label}: ASan/LSan reported findings:" >&2
    printf '%s\n' "${asan_output}" | sed 's/^/    /' >&2
    return 1
  fi

  if [[ "${actual_exit}" -ne "${expected_exit}" ]]; then
    echo "    FAIL ${label}: expected exit ${expected_exit}, got ${actual_exit}" >&2
    return 1
  fi

  echo "    PASS ${label}: exit ${actual_exit}, no ASan/LSan findings"
}

# ── Proof probe: deliberately-leaky binary ───────────────────────────────
# To prove the gate actually fires on leaks, we compile a small C program
# that deliberately leaks 1 KiB via malloc, linked with -fsanitize=address
# using the same clang call as the Hew fixture pipeline.
#
# NOTE: We use a pure C proof rather than a Hew program calling extern "rt",
# because extern "rt" declarations are restricted to symbols in the JIT stable
# ABI list (scripts/jit-symbol-classification.toml).  A synthetic test symbol
# does not belong in that list.  The C proof demonstrates the same thing: that
# a binary produced by this gate's link pipeline is correctly intercepted by
# ASan/LSan.  The clean-probe (a real Hew fixture) validates the full Hew
# codegen → clang-link → ASan-run path.
LEAK_PROOF_C="${WORK_DIR}/asan_leak_proof.c"
cat > "${LEAK_PROOF_C}" <<'EOF'
#include <stdlib.h>

/* Deliberately leak 1024 bytes to prove the ASan/LSan gate catches leaks in
 * binaries produced by the asan-fixture pipeline.  The main function returns 0
 * so we test the LSan at-exit check, not the allocator error path. */
int main(void) {
    void *p = malloc(1024);
    (void)p; /* deliberately not freed — LSan must report this at exit */
    return 0;
}
EOF

PROOF_BIN="${WORK_DIR}/asan_leak_proof"
echo "  LINK     leak-proof (C)"
clang \
  -fsanitize=address \
  -fno-omit-frame-pointer \
  -target "${SANITIZER_TARGET}" \
  "${LEAK_PROOF_C}" \
  -o "${PROOF_BIN}"

# ── Step 2: compile the clean fixture ────────────────────────────────────
CLEAN_SRC="${ROOT}/tests/vertical-slice/accept/asan_fixture_clean_probe.hew"

# ── Step 3: compile the Hew clean fixture ────────────────────────────────
echo ""
echo "=== asan-fixture-check: compiling fixtures ==="

CLEAN_BIN="${WORK_DIR}/asan_fixture_clean_probe"
compile_asan_fixture "clean-probe" "${CLEAN_SRC}" "${CLEAN_BIN}"

# ── Step 4: run the gate ──────────────────────────────────────────────────
echo ""
echo "=== asan-fixture-check: running gate ==="

pass=0
fail=0

# ── Gate 1: clean fixture MUST produce zero ASan/LSan findings ──────────
if run_asan_fixture "clean-probe" "${CLEAN_BIN}" 0; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

# ── Gate 2: leaky fixture MUST produce an ASan/LSan finding ─────────────
echo "  RUN      leak-proof (expect LSan finding)"

proof_exit=0
ASAN_OPTIONS="detect_leaks=1:log_path=${PROOF_BIN}.asan" \
ASAN_SYMBOLIZER_PATH="${ASAN_SYMBOLIZER_PATH}" \
HEW_WORKERS=1 \
  "${PROOF_BIN}" >/dev/null 2>/dev/null || proof_exit=$?

proof_output=""
for f in "${PROOF_BIN}.asan".*; do
  [[ -f "${f}" ]] || continue
  proof_output+="$(cat "${f}")"$'\n'
  rm -f "${f}"
done

# Also capture stderr directly in case log_path was not respected.
proof_stderr=""
ASAN_OPTIONS="detect_leaks=1" \
ASAN_SYMBOLIZER_PATH="${ASAN_SYMBOLIZER_PATH}" \
  "${PROOF_BIN}" >/dev/null 2>"${PROOF_BIN}.stderr" || true
proof_stderr="$(cat "${PROOF_BIN}.stderr" 2>/dev/null || true)"

gate_fired=false
if printf '%s\n%s' "${proof_output}" "${proof_stderr}" \
     | grep -qE "ERROR: (AddressSanitizer|LeakSanitizer)|detected memory leaks|SUMMARY: (AddressSanitizer|LeakSanitizer)"; then
  gate_fired=true
elif [[ "${proof_exit}" -ne 0 ]]; then
  # LSan exits non-zero when leaks are detected even without matching text
  gate_fired=true
fi

if "${gate_fired}"; then
  echo "    PASS leak-proof: gate correctly caught deliberate leak (exit ${proof_exit})"
  pass=$((pass + 1))
else
  echo "    FAIL leak-proof: gate did NOT catch the deliberate 1 KiB leak" >&2
  echo "    This means ASan/LSan is not firing on the gate's binaries." >&2
  echo "    Check: clang -fsanitize=address at both compile and link steps." >&2
  echo "    Proof binary: ${PROOF_BIN}" >&2
  fail=$((fail + 1))
fi

# ── Summary ───────────────────────────────────────────────────────────────
echo ""
echo "=== asan-fixture-check: ${pass} passed, ${fail} failed ==="
if [[ "${fail}" -ne 0 ]]; then
  exit 1
fi
