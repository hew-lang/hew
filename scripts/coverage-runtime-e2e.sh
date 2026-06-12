#!/usr/bin/env bash
# Measure runtime (libhew) coverage exercised by compiled-and-run Hew programs.
#
# WHY this exists: `make coverage` (cargo-llvm-cov) measures only the Rust
# crate unit/integration tests. It never sees the runtime `hew_*` C-ABI surface
# (print/assert/vec/string/bytes/hashmap/actor/...), because that code is
# exercised only by *compiled Hew programs* that link libhew.a and run as a
# subprocess — never called directly from a Rust test. As a result FFI files
# look ~0% covered when they are heavily e2e-covered.
#
# HOW it works (the mechanism, proven in PR build/coverage-measurement):
#   1. libhew.a is rebuilt with `-C instrument-coverage`, so its object code
#      carries LLVM counter (`__llvm_prf_*`) and coverage-map (`__llvm_cov*`)
#      sections. rustc bundles the profiler *runtime* only when it links the
#      final artifact — but here clang links the final program, so the runtime
#      is missing and the counters are never written.
#   2. The `hew` linker is told (via HEW_COVERAGE=1) to pass clang's
#      `-fprofile-instr-generate` and to skip dead-strip/strip, which pulls in
#      `libclang_rt.profile` (honours LLVM_PROFILE_FILE, writes profraw on exit)
#      and keeps the coverage sections alive. See hew-cli/src/link.rs.
#   3. We compile a set of self-contained example programs to KEPT binaries,
#      run each with LLVM_PROFILE_FILE set, merge the profraw, and ask llvm-cov
#      to report against the kept binaries (multi-`-object`). The report object
#      MUST be the compiled program itself — its embedded covmap is keyed by
#      function structural hashes that do NOT match the cargo-test binaries, so
#      e2e profraw cannot be folded into the cargo-llvm-cov report. They are
#      separate reports by construction; see .tmp/coverage-retriage.md.
#
# WHAT is and isn't captured: this measures the runtime FFI surface reachable
# from the curated example corpus (top-level self-contained examples/*.hew with
# a `main`, excluding network/server/service programs that need peers/ports).
# It does NOT re-run the Rust exec/e2e test corpus (those delete their temp
# binaries, leaving no report object). It is a faithful sample of runtime
# coverage, not the union of every e2e test.
#
# Usage: scripts/coverage-runtime-e2e.sh [--html]
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

COV_DIR="${COV_DIR:-coverage-out}"
RT_DIR="$COV_DIR/runtime-e2e"
BIN_DIR="$RT_DIR/bins"
PROFRAW_DIR="$RT_DIR/profraw"
PROFDATA="$RT_DIR/runtime.profdata"
PER_PROG_TIMEOUT="${PER_PROG_TIMEOUT:-15}"
WANT_HTML=0
[ "${1:-}" = "--html" ] && WANT_HTML=1

HEW_BIN="$REPO_ROOT/target/debug/hew"

# ── Locate version-matched LLVM tools ──────────────────────────────────
# Prefer the rustc-bundled llvm-tools (exact version match to the
# instrument-coverage producer); fall back to versioned/unversioned PATH tools.
RUST_BIN_DIR="$(rustc --print sysroot)/lib/rustlib/$(rustc -vV | sed -n 's/host: //p')/bin"
pick_tool() {
  local name="$1"
  if [ -x "$RUST_BIN_DIR/$name" ]; then echo "$RUST_BIN_DIR/$name"; return; fi
  for cand in "$name-22" "$name"; do
    if command -v "$cand" >/dev/null 2>&1; then echo "$cand"; return; fi
  done
  echo "error: cannot find $name (rust llvm-tools-preview or PATH)" >&2
  exit 1
}
LLVM_PROFDATA="$(pick_tool llvm-profdata)"
LLVM_COV="$(pick_tool llvm-cov)"

echo "==> Phase 1: build instrumented libhew.a"
RUSTFLAGS="-C instrument-coverage" cargo build -p hew-lib

echo "==> Phase 2: build the hew CLI (uninstrumented; just needs to drive the link)"
cargo build -p hew-cli --bin hew

echo "==> Phase 3: compile + run self-contained example programs (HEW_COVERAGE=1)"
rm -rf "$RT_DIR"
mkdir -p "$BIN_DIR" "$PROFRAW_DIR"

# Discover top-level example programs with a `main`, skipping two classes:
#  - peer/port programs (network/server/service/distributed) — they hang or
#    fail closed waiting for a peer;
#  - long-running demos/UIs (observe/showcase/live/loop/bench) — they run to the
#    per-program timeout, adding wall time without new coverage.
# Each kept program still has a hard timeout, so an unexpected long-runner only
# costs PER_PROG_TIMEOUT, never a hang. (Plain array append, not `mapfile` —
# macOS ships bash 3.2.)
PROGRAMS=()
for f in examples/*.hew; do
  b="$(basename "$f" .hew)"
  case "$b" in
    *server*|*service*|*client*|*chat*|*mqtt*|*http*|*quic*|*curl*|*net*|*distributed*|*tcp*|*socket*|*reader*|*broker*) continue ;;
    *observe*|*showcase*|*live*|*loop*|*bench*|*playground*|*orch*|*daemon*) continue ;;
  esac
  if grep -q 'fn main' "$f" 2>/dev/null; then
    PROGRAMS+=("$f")
  fi
done

built=0; build_fail=0; ran=0; run_fail=0
for f in "${PROGRAMS[@]}"; do
  stem="$(basename "$f" .hew)"
  bin="$BIN_DIR/$stem.bin"
  if HEW_COVERAGE=1 "$HEW_BIN" build "$f" -o "$bin" >/dev/null 2>&1; then
    built=$((built + 1))
  else
    build_fail=$((build_fail + 1))
    continue
  fi
  # %m = binary signature (distinct per program), %p = pid → no collisions.
  if LLVM_PROFILE_FILE="$PROFRAW_DIR/${stem}-%m-%p.profraw" \
      timeout "$PER_PROG_TIMEOUT" "$bin" >/dev/null 2>&1; then
    ran=$((ran + 1))
  else
    run_fail=$((run_fail + 1))
  fi
done
echo "    programs: built=$built (build_fail=$build_fail) ran=$ran (run_fail=$run_fail)"

shopt -s nullglob
PROFRAWS=("$PROFRAW_DIR"/*.profraw)
if [ "${#PROFRAWS[@]}" -eq 0 ]; then
  echo "error: no profraw produced — coverage capture failed" >&2
  exit 1
fi

echo "==> Phase 4: merge profraw + report runtime coverage"
"$LLVM_PROFDATA" merge -sparse "${PROFRAWS[@]}" -o "$PROFDATA"

BINS=("$BIN_DIR"/*.bin)
OBJ_ARGS=()
for b in "${BINS[@]:1}"; do OBJ_ARGS+=(-object "$b"); done

# Report only the runtime/stdlib source (drop the compiler crates, deps, std).
IGNORE='(/\.cargo/|/rustc/|/usr/|registry|/tests/|hew-cli/|hew-types/|hew-hir/|hew-mir/|hew-codegen-rs/|hew-parser/|hew-lexer/|hew-compile/|hew-analysis/|hew-lsp/|hew-observe/|hew-wasm|hew-sandbox|adze-cli/|xtask/|hew-testutil/|hew-runtime-testkit/|hew-capability-gen/)'

"$LLVM_COV" report "${BINS[0]}" "${OBJ_ARGS[@]}" \
  -instr-profile="$PROFDATA" \
  --ignore-filename-regex="$IGNORE" | tee "$RT_DIR/runtime-summary.txt"

if [ "$WANT_HTML" -eq 1 ]; then
  echo "==> Generating HTML runtime report"
  "$LLVM_COV" show "${BINS[0]}" "${OBJ_ARGS[@]}" \
    -instr-profile="$PROFDATA" \
    --ignore-filename-regex="$IGNORE" \
    -format=html -output-dir="$RT_DIR/html"
  echo "==> Open $RT_DIR/html/index.html"
fi

echo "==> Runtime e2e coverage summary: $RT_DIR/runtime-summary.txt"
