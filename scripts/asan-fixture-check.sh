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
# Three fixture probes:
#   clean-probe (--emit-obj path)
#       A compiled Hew binary exercising the previously-leaky Vec<string>
#       index-into-compare and local drop shapes (now fixed); must produce ZERO
#       ASan/LSan findings.
#   leak-probe (Hew generated-code)
#       asan_fixture_leak_probe.hew calls malloc via extern "C" and never frees
#       it.  The malloc call is in the LLVM IR produced by hew codegen — a
#       genuine generated-code leak.  The gate must catch it, proving it would
#       have flagged the historical array-repeat Vec<string>/Vec<record> leaks
#       that were previously macOS-oracle-only.
#   clean-probe (HEW_SANITIZE_ADDRESS=1 hew build path)
#       The same clean fixture linked by `hew build` with HEW_SANITIZE_ADDRESS=1
#       (the full CLI link step, routing through hew-cli/src/link.rs).  Proves
#       the CLI flag path is exercised by this CI gate, not only the manual
#       clang step in the emit-obj pipeline.  Must produce ZERO findings.
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

RUSTUP_TOOLCHAINS=$(rustup toolchain list 2>/dev/null)
if ! grep -q nightly <<< "${RUSTUP_TOOLCHAINS}"; then
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

# ── Helper: run one binary expecting an ASan/LSan finding ─────────────────
# The inverse of run_asan_fixture.  Returns 0 (pass) if the binary's ASan/LSan
# report contains a finding marker or the binary exits non-zero due to LSan.
# Returns 1 (fail) if no finding is detected — indicating ASan/LSan is not
# firing and the gate would silently miss leaks.
run_asan_fixture_expect_leak() {
  local label="$1"
  local bin="$2"
  local log_prefix="${bin}.asan"

  echo "  RUN      ${label} (expect LSan finding)"

  local actual_exit=0
  ASAN_OPTIONS="detect_leaks=1:log_path=${log_prefix}" \
  ASAN_SYMBOLIZER_PATH="${ASAN_SYMBOLIZER_PATH}" \
  HEW_WORKERS=1 \
    "${bin}" >/dev/null 2>/dev/null || actual_exit=$?

  local asan_output=""
  for f in "${log_prefix}".*; do
    [[ -f "${f}" ]] || continue
    asan_output+="$(cat "${f}")"$'\n'
    rm -f "${f}"
  done

  # Also capture stderr directly in case log_path was not respected.
  local asan_stderr=""
  ASAN_OPTIONS="detect_leaks=1" \
  ASAN_SYMBOLIZER_PATH="${ASAN_SYMBOLIZER_PATH}" \
    "${bin}" >/dev/null 2>"${log_prefix}.stderr" || true
  asan_stderr="$(cat "${log_prefix}.stderr" 2>/dev/null || true)"
  rm -f "${log_prefix}.stderr"

  local gate_fired=false
  if printf '%s\n%s' "${asan_output}" "${asan_stderr}" \
       | grep -qE "ERROR: (AddressSanitizer|LeakSanitizer)|detected memory leaks|SUMMARY: (AddressSanitizer|LeakSanitizer)"; then
    gate_fired=true
  elif [[ "${actual_exit}" -ne 0 ]]; then
    # LSan exits non-zero when leaks are detected even without matching text
    gate_fired=true
  fi

  if "${gate_fired}"; then
    echo "    PASS ${label}: gate correctly caught deliberate generated-code leak (exit ${actual_exit})"
    return 0
  else
    echo "    FAIL ${label}: gate did NOT catch the deliberate 1 KiB malloc leak" >&2
    echo "    This means ASan/LSan is not firing on compiled Hew binaries." >&2
    echo "    Check: -fsanitize=address at both compile and link steps." >&2
    echo "    Binary: ${bin}" >&2
    return 1
  fi
}

# ── Step 2: compile the clean and leak-probe fixtures ────────────────────
CLEAN_SRC="${ROOT}/tests/vertical-slice/accept/asan_fixture_clean_probe.hew"
LEAK_SRC="${ROOT}/tests/vertical-slice/accept/asan_fixture_leak_probe.hew"

# ── Step 3: compile the Hew fixtures ─────────────────────────────────────
echo ""
echo "=== asan-fixture-check: compiling fixtures ==="

CLEAN_BIN="${WORK_DIR}/asan_fixture_clean_probe"
compile_asan_fixture "clean-probe" "${CLEAN_SRC}" "${CLEAN_BIN}"

# ── Step 3b: compile the Hew generated-code leak-proof fixture ───────────
# asan_fixture_leak_probe.hew calls malloc via extern "C" and intentionally
# never frees it.  The call to malloc is in the LLVM IR emitted by hew
# codegen — this is a genuine generated-code leak, not a standalone C program.
# The gate expects this binary to produce an ASan/LSan finding, proving that
# the pipeline catches leaks in compiled Hew code.
LEAK_BIN="${WORK_DIR}/asan_fixture_leak_probe"
compile_asan_fixture "leak-probe (Hew generated-code)" "${LEAK_SRC}" "${LEAK_BIN}"

# ── Step 3c: compile and link the clean probe via the CLI flag path ───────
# Uses HEW_SANITIZE_ADDRESS=1 hew build (full link, not --emit-obj) to exercise
# the CLI integration that routes -fsanitize=address through hew-cli/src/link.rs.
# This proves the HEW_SANITIZE_ADDRESS flag path is active in the CI gate, not
# only the manual clang step in compile_asan_fixture.
CLI_LINK_BIN="${WORK_DIR}/asan_fixture_clean_probe_cli_link"
echo "  CLI-LINK clean-probe (HEW_SANITIZE_ADDRESS=1 hew build)"
HEW_SANITIZE_ADDRESS=1 \
  "${ASAN_HEW}" build "${CLEAN_SRC}" -o "${CLI_LINK_BIN}" 2>&1 | sed 's/^/    /'
if [[ ! -f "${CLI_LINK_BIN}" ]]; then
  echo "asan-fixture-check: HEW_SANITIZE_ADDRESS=1 hew build produced no binary at ${CLI_LINK_BIN}" >&2
  exit 1
fi
echo "  VERIFY   CLI-linked binary contains ASan/LSan runtime symbols"
CLI_LINK_SYMS=$(nm -D "${CLI_LINK_BIN}" 2>/dev/null)
if ! grep -q "__asan_init\|__lsan_" <<< "${CLI_LINK_SYMS}"; then
  echo "asan-fixture-check: CLI-linked binary does not contain __asan_init / __lsan_ symbols" >&2
  echo "    Check: HEW_SANITIZE_ADDRESS=1 must add -fsanitize=address at the clang link step." >&2
  exit 1
fi
echo "    PASS CLI-link: binary contains ASan/LSan runtime symbols (__asan_init / __lsan_*)"

# ── Step 4: run the gate ──────────────────────────────────────────────────
echo ""
echo "=== asan-fixture-check: running gate ==="

pass=0
fail=0

# ── Gate 1: clean fixture (--emit-obj path) MUST produce zero findings ───
if run_asan_fixture "clean-probe" "${CLEAN_BIN}" 0; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

# ── Gate 2: Hew generated-code leak-probe MUST produce a LSan finding ────
# asan_fixture_leak_probe.hew calls malloc via extern "C" and never frees it.
# The malloc call is in the LLVM IR produced by hew codegen.  LSan's at-exit
# scan must report the 1 KiB unreachable block.  This is the positive proof
# that the gate would have caught the historical array-repeat Vec<string>/
# Vec<record> leaks.
if run_asan_fixture_expect_leak "leak-probe (Hew generated-code)" "${LEAK_BIN}"; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

# ── Gate 3: clean fixture (HEW_SANITIZE_ADDRESS=1 hew build path) ─────────
# Runs the CLI-linked binary (produced by the full `hew build` link step with
# HEW_SANITIZE_ADDRESS=1) under ASan/LSan.  Proves the CLI flag integration is
# exercised by this gate, not only the manual clang path.
if run_asan_fixture "clean-probe (CLI link path)" "${CLI_LINK_BIN}" 0; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

# ── Summary ───────────────────────────────────────────────────────────────
echo ""
echo "=== asan-fixture-check: ${pass} passed, ${fail} failed ==="
if [[ "${fail}" -ne 0 ]]; then
  exit 1
fi
