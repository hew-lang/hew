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
#   bytes COW retain-on-share (A240 S1)
#       Field-load-of-live-root, container element read, live-local co-own,
#       duplicating return, mutation-triggered COW fork, plus the two plain
#       move-share co-own shapes: `let alias = <by-value bytes param>` (the
#       source is a caller-owned borrow) and `let alias = source; ...; source`
#       (the co-owner escapes by return while alias drops locally). Each shape
#       mints multiple references to one bytes buffer; a missing retain-on-share
#       double-frees under ASan (the underflow macOS `leaks` cannot see). All
#       must remain ASan/LSan-clean.
#   string retain-on-share
#       Existing retained field/container reads plus aggregate ingress,
#       duplicating return, local co-own, borrowed-param co-own, and an escaping
#       partner. Missing retains surface as refcount UAF; excess retains leak.
#   composite-drop leak-oracle shapes (#2488)
#       Three already-clean vertical-slice fixtures covering the #2439 composite
#       yield-release and #2462 match-scrutinee enum-payload-release drop
#       mechanisms: an owned enum payload crossing the generator pump's yield
#       send path (tag-dispatched in-place drop thunk), the break-edge release
#       of an owned record on a cancelled `for await ... break` stream, and a
#       match-arm destructured call-result payload released exactly once per
#       loop back-edge. A refcount underflow in any would double-free a
#       String/Bytes header — invisible to macOS `leaks`, caught here by ASan.
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

# ── Suppressions staging (hew-lang/hew#1889) ──────────────────────────────
# The sanitizer runtime parses [LA]SAN_OPTIONS as a space-tolerant
# key=value:key=value list, so a space inside the suppressions path value
# (e.g. a worktree on /Volumes/Extreme SSD/...) makes it expect a new
# key=value and abort with "expected '=' in LSAN_OPTIONS" before any fixture
# runs. Stage lsan.supp into a mktemp dir (TMPDIR is conventionally space-free
# on both Linux and macOS) so the value we pass never contains a space,
# regardless of the absolute worktree location.
SUPP_STAGE_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hew-asan-supp.XXXXXX")"
cleanup_supp_stage() { rm -rf "${SUPP_STAGE_DIR}"; }
trap cleanup_supp_stage EXIT
cp "${ROOT}/hew-runtime/lsan.supp" "${SUPP_STAGE_DIR}/lsan.supp"
LSAN_SUPP="${SUPP_STAGE_DIR}/lsan.supp"
if printf '%s' "${LSAN_SUPP}" | grep -q ' '; then
  echo "asan-fixture-check: staged suppressions path still contains a space:" >&2
  echo "  ${LSAN_SUPP}" >&2
  echo "  set TMPDIR to a space-free directory and re-run." >&2
  exit 1
fi

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
  LSAN_OPTIONS="suppressions=${LSAN_SUPP}" \
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
  # Use a here-string to avoid a printf | grep -q pipeline: under set -o pipefail
  # grep -q closes stdin after the first match, sending SIGPIPE to printf, which
  # makes the pipeline exit 141 (false negative) on large reports.
  if grep -qE "ERROR: (AddressSanitizer|LeakSanitizer)|SUMMARY: (AddressSanitizer|LeakSanitizer)" \
       <<< "${asan_output}"; then
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

  # Combine both capture buffers into one string so we can use a here-string
  # predicate.  A printf | grep -q pipeline is unsafe under set -o pipefail:
  # grep -q closes stdin after the first match, SIGPIPE reaches printf, and the
  # pipeline exits 141 instead of 0 on any large report (false negative).
  local combined_asan="${asan_output}"$'\n'"${asan_stderr}"
  local gate_fired=false
  if grep -qE "ERROR: (AddressSanitizer|LeakSanitizer)|detected memory leaks|SUMMARY: (AddressSanitizer|LeakSanitizer)" \
       <<< "${combined_asan}"; then
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
# Crash+restart clean probe: an actor really traps, its #[on(crash)] hook clones
# and reads CrashInfo.message and returns CrashAction::Restart, the supervisor
# restarts it, and main exits 42. Exercises the emitted __on_crash on a REAL
# crash under ASan/LSan — the crash-message clone (hew_string_clone) and the
# CrashInfo drop must balance the supervisor's str_to_malloc/free_cstring with no
# double-free, no leak, no OOB. The pre-fix move-of-borrow + headerless drop
# would OOB-read and abort here; ASan would catch the heap-buffer-overflow even
# before the abort.
CRASH_RESTART_SRC="${ROOT}/tests/vertical-slice/accept/on_crash_action_restart_real_crash.hew"
BYTES_COW_SRC="${ROOT}/tests/vertical-slice/accept/bytes_cow_retain_s1.hew"
STRING_COW_SRC="${ROOT}/tests/vertical-slice/accept/string_cow_retain_share.hew"
# Composite-drop leak-oracle shapes (#2488): three already-clean vertical-slice
# fixtures that exercise the drop mechanisms #2439 (composite yield release via
# in-place drop thunks) and #2462 (match-scrutinee enum payload release). Prior
# to this the asan-fixtures corpus was blind to refcount-underflow double-frees
# on String/Bytes in these shapes — only ASan on an instrumented runtime catches
# that class as a heap-use-after-free (macOS `leaks` cannot see an underflow).
# All three exit 0 and must remain ASan/LSan-clean. (receive_gen_fn_owned_record
# _yield.hew is deliberately NOT added here: it carries a separate known 23-byte
# residual — retained string-field reads in for-await bodies leak a per-iteration
# temporary, called out in the #2439 merge message — so it would not be a clean
# gate member until that residual is fixed or suppressed.)
ENUM_YIELD_SRC="${ROOT}/tests/vertical-slice/accept/receive_gen_fn_owned_enum_yield.hew"
RECORD_STREAM_BREAK_SRC="${ROOT}/tests/vertical-slice/accept/receive_gen_fn_record_stream_break.hew"
ENUM_PAYLOAD_LOOP_SRC="${ROOT}/tests/vertical-slice/accept/enum_payload_call_loop_release.hew"
# Admitted fresh-producer call scrutinee (#2648): a heap-owning-enum result built
# FRESH through immutable bindings and a helper chain (the D108 shape the return-
# provenance preflight must keep admitting). The preflight mints exactly one
# synthetic owner over the call temp; a double-mint would double-free the payload
# header — invisible to macOS `leaks`, caught here by ASan. Exits 0, clean.
CALL_SCRUTINEE_FRESH_SRC="${ROOT}/tests/vertical-slice/accept/call_scrutinee_fresh_forwarder_release.hew"

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

# ── Step 3d: compile the crash+restart clean probe ───────────────────────
# The on(crash) hook runs on a real trap; the crash-message clone/drop path must
# be ASan/LSan-clean across the crash and restart.
CRASH_RESTART_BIN="${WORK_DIR}/on_crash_action_restart_real_crash"
compile_asan_fixture "crash-restart (on_crash real crash)" "${CRASH_RESTART_SRC}" "${CRASH_RESTART_BIN}"

BYTES_COW_BIN="${WORK_DIR}/bytes_cow_retain_s1"
compile_asan_fixture "bytes COW retain-on-share (A240 S1)" "${BYTES_COW_SRC}" "${BYTES_COW_BIN}"

STRING_COW_BIN="${WORK_DIR}/string_cow_retain_share"
compile_asan_fixture "string retain-on-share" "${STRING_COW_SRC}" "${STRING_COW_BIN}"

# ── Step 3e: compile the composite-drop leak-oracle fixtures (#2488) ──────
# Three already-clean vertical-slice fixtures covering the #2439 composite
# yield-release and #2462 match-scrutinee enum-payload-release drop shapes so
# the ASan gate — the only oracle that sees refcount-underflow double-frees on
# Linux — guards these mechanisms against future regressions.
ENUM_YIELD_BIN="${WORK_DIR}/receive_gen_fn_owned_enum_yield"
compile_asan_fixture "composite yield (owned enum)" "${ENUM_YIELD_SRC}" "${ENUM_YIELD_BIN}"

RECORD_STREAM_BREAK_BIN="${WORK_DIR}/receive_gen_fn_record_stream_break"
compile_asan_fixture "composite yield (record stream break)" "${RECORD_STREAM_BREAK_SRC}" "${RECORD_STREAM_BREAK_BIN}"

ENUM_PAYLOAD_LOOP_BIN="${WORK_DIR}/enum_payload_call_loop_release"
compile_asan_fixture "match-scrutinee enum payload (call loop)" "${ENUM_PAYLOAD_LOOP_SRC}" "${ENUM_PAYLOAD_LOOP_BIN}"

CALL_SCRUTINEE_FRESH_BIN="${WORK_DIR}/call_scrutinee_fresh_forwarder_release"
compile_asan_fixture "admitted fresh-producer call scrutinee (#2648)" "${CALL_SCRUTINEE_FRESH_SRC}" "${CALL_SCRUTINEE_FRESH_BIN}"

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

# ── Gate 4: crash+restart clean probe MUST produce zero findings, exit 42 ─
# A real crash fires the emitted __on_crash, which clones + reads
# CrashInfo.message and returns CrashAction::Restart. The crash-message
# clone/drop (hew_string_clone / CrashInfo record drop) must balance the
# supervisor's str_to_malloc/free_cstring with no double-free, no leak, no OOB.
# Exit 42 = the restarted child's init value; any ASan/LSan finding (or a non-42
# exit) fails the gate.
if run_asan_fixture "crash-restart (on_crash real crash)" "${CRASH_RESTART_BIN}" 42; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

# ── Gate 5: bytes retain-on-share mint points MUST be ASan/LSan-clean ─────
if run_asan_fixture "bytes COW retain-on-share (A240 S1)" "${BYTES_COW_BIN}" 0; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

# ── Gate 6: string retain-on-share mint points MUST be ASan/LSan-clean ────
if run_asan_fixture "string retain-on-share" "${STRING_COW_BIN}" 0; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

# ── Gate 7–9: composite-drop leak-oracle shapes MUST be ASan/LSan-clean (#2488) ─
# Each fixture exercises a merged drop mechanism that is otherwise unguarded on
# Linux: an owned enum payload crossing the generator pump's yield send path
# (#2439 tag-dispatched in-place drop thunk), the break-edge release of an owned
# record on a cancelled `for await ... break` stream (#2439), and a match-arm
# destructured call-result payload released exactly once per loop back-edge
# (#2462/#2429). A refcount underflow in any of these would double-free a
# String/Bytes header — invisible to macOS `leaks`, caught here by ASan. All
# exit 0.
if run_asan_fixture "composite yield (owned enum)" "${ENUM_YIELD_BIN}" 0; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

if run_asan_fixture "composite yield (record stream break)" "${RECORD_STREAM_BREAK_BIN}" 0; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

if run_asan_fixture "match-scrutinee enum payload (call loop)" "${ENUM_PAYLOAD_LOOP_BIN}" 0; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
fi

# ── Gate 9: admitted fresh-producer call scrutinee MUST be ASan/LSan-clean (#2648) ─
# The return-provenance preflight admits a fresh-through-bindings producer and
# mints exactly one synthetic owner over the call temp. This fixture proves that
# admit path is leak-clean under ASan; a double-mint (the #2648 double-free) would
# surface here as a heap-use-after-free on the payload header.
if run_asan_fixture "admitted fresh-producer call scrutinee (#2648)" "${CALL_SCRUTINEE_FRESH_BIN}" 0; then
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
