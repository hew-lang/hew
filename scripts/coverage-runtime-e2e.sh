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

# Exercise the program-invocation seam under the same shell running this
# harness. In particular, supported macOS hosts use Bash 3.2 and must retain
# both the zero-argument and deterministic command-argument paths.
"${BASH}" scripts/tests/test_coverage_runtime_program.sh
# shellcheck source=scripts/lib/coverage-runtime-program.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/coverage-runtime-program.sh"

COV_DIR="${COV_DIR:-coverage-out}"
RT_DIR="$COV_DIR/runtime-e2e"
BIN_DIR="$RT_DIR/bins"
PROFRAW_DIR="$RT_DIR/profraw"
LOG_DIR="$RT_DIR/logs"
PROFDATA="$RT_DIR/runtime.profdata"
PER_PROG_TIMEOUT="${PER_PROG_TIMEOUT:-15}"
WANT_HTML=0
[ "${1:-}" = "--html" ] && WANT_HTML=1

native_debug_dir="$(scripts/cargo-output-dir.py --native --profile debug)"
if [[ "${native_debug_dir}" != /* ]]; then
    native_debug_dir="${REPO_ROOT}/${native_debug_dir}"
fi
HEW_BIN="${native_debug_dir}/hew"

# ── Locate version-matched LLVM tools ──────────────────────────────────
# Prefer the rustc-bundled llvm-tools (exact version match to the
# instrument-coverage producer); fall back to versioned/unversioned PATH tools.
RUST_BIN_DIR="$(rustc --print sysroot)/lib/rustlib/$(rustc -vV | sed -n 's/host: //p')/bin"
pick_tool() {
    local name="$1"
    if [ -x "$RUST_BIN_DIR/$name" ]; then
        echo "$RUST_BIN_DIR/$name"
        return
    fi
    for cand in "$name-22" "$name"; do
        if command -v "$cand" >/dev/null 2>&1; then
            echo "$cand"
            return
        fi
    done
    echo "error: cannot find $name (rust llvm-tools-preview or PATH)" >&2
    exit 1
}
LLVM_PROFDATA="$(pick_tool llvm-profdata)"
LLVM_COV="$(pick_tool llvm-cov)"

# Discover top-level example programs with a `main`, skipping two classes:
#  - peer/port programs (network/server/service/distributed) — they hang or
#    fail closed waiting for a peer;
#  - long-running demos/UIs (observe/showcase/live/loop/bench) — they run to the
#    per-program timeout, adding wall time without new coverage.
# Each kept program still has a hard timeout, so an unexpected long-runner only
# costs PER_PROG_TIMEOUT, never a hang. (Plain array append, not `mapfile` —
# macOS ships bash 3.2.)
#
# Enumerated up front, before the two builds below: the report's meaning is
# exactly "what this program set exercised", so a filter that stops matching or
# a rename that drops half the examples turns a runtime-coverage number into a
# much narrower one with nothing to say so. Better to hear about it in a second
# than after an instrumented rebuild.
PROGRAMS=()
for f in examples/*.hew; do
    b="$(basename "$f" .hew)"
    case "$b" in
    *server* | *service* | *client* | *chat* | *mqtt* | *http* | *quic* | *curl* | *net* | *distributed* | *tcp* | *socket* | *reader* | *broker*) continue ;;
    *observe* | *showcase* | *live* | *loop* | *bench* | *playground* | *orch* | *daemon*) continue ;;
    esac
    if grep -q 'fn main' "$f" 2>/dev/null; then
        PROGRAMS+=("$f")
    fi
done
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/corpus-nonempty.sh"
corpus_nonempty_assert "coverage-e2e-programs" "${#PROGRAMS[@]}" || exit 1

echo "==> Phase 1: build instrumented libhew.a"
RUSTFLAGS="-C instrument-coverage" cargo build -p hew-lib

echo "==> Phase 2: build the hew CLI (uninstrumented; just needs to drive the link)"
cargo build -p hew-cli --bin hew

echo "==> Phase 3: compile + run self-contained example programs (HEW_COVERAGE=1)"
rm -rf "$RT_DIR"
mkdir -p "$BIN_DIR" "$PROFRAW_DIR" "$LOG_DIR"

# Inputs for command-style examples in the otherwise self-contained corpus.
# Keep these deterministic and local: coverage must not depend on the caller's
# argv, stdin, network, or filesystem state.
GREP_INPUT="$RT_DIR/hew-grep-input.txt"
printf 'alpha\nneedle one\nomega\nneedle two\n' >"$GREP_INPUT"

built=0
ran=0
BUILD_FAILURES=()
RUN_FAILURES=()
for f in "${PROGRAMS[@]}"; do
    stem="$(basename "$f" .hew)"
    bin="$BIN_DIR/$stem.bin"
    build_status=0
    HEW_COVERAGE=1 "$HEW_BIN" build "$f" -o "$bin" \
        >"$LOG_DIR/$stem.build.stdout" 2>"$LOG_DIR/$stem.build.stderr" || build_status=$?
    printf '%s\n' "$build_status" >"$LOG_DIR/$stem.build.exit"
    if [ "$build_status" -ne 0 ]; then
        BUILD_FAILURES+=("$f")
        continue
    fi
    built=$((built + 1))
    # %m = binary signature (distinct per program), %p = pid → no collisions.
    run_status=0
    coverage_runtime_run_program \
        "$stem" \
        "$PROFRAW_DIR/${stem}-%m-%p.profraw" \
        "$(command -v timeout)" \
        "$PER_PROG_TIMEOUT" \
        "$bin" \
        "$GREP_INPUT" \
        >"$LOG_DIR/$stem.run.stdout" 2>"$LOG_DIR/$stem.run.stderr" || run_status=$?
    printf '%s\n' "$run_status" >"$LOG_DIR/$stem.run.exit"
    expected_status=0
    completion_marker=""
    if [ "$stem" = supervisor_crash_budget ]; then
        # Exhausting a root supervisor's budget leaves an unrecovered fault.
        # Require the final observation too: an earlier assertion panic also
        # exits 1, but does not demonstrate the intended terminal role.
        expected_status=1
        completion_marker="Restart budget exhausted; failed child is unavailable."
    fi
    if [ "$run_status" -eq "$expected_status" ] && {
        [ -z "$completion_marker" ] || grep -Fxq "$completion_marker" "$LOG_DIR/$stem.run.stdout"
    }; then
        ran=$((ran + 1))
    else
        printf 'error: %s exited %s; expected %s\n' "$f" "$run_status" "$expected_status" >&2
        if [ -n "$completion_marker" ]; then
            printf '  required completion: %s\n' "$completion_marker" >&2
        fi
        RUN_FAILURES+=("$f")
    fi
done
echo "    programs: enumerated=${#PROGRAMS[@]} built=$built ran=$ran"

# Coverage is evidence only for programs that actually completed.  A profraw
# file from one successful binary must never mask build failures, crashes, or
# timeouts elsewhere in the corpus.
if [ "${#BUILD_FAILURES[@]}" -ne 0 ] || [ "${#RUN_FAILURES[@]}" -ne 0 ]; then
    if [ "${#BUILD_FAILURES[@]}" -ne 0 ]; then
        echo "error: ${#BUILD_FAILURES[@]} runtime-coverage program(s) failed to build:" >&2
        for f in "${BUILD_FAILURES[@]}"; do
            stem="$(basename "$f" .hew)"
            printf '  build: %s (exit %s)\n' "$f" "$(cat "$LOG_DIR/$stem.build.exit")" >&2
            printf '  stdout (%s):\n' "$LOG_DIR/$stem.build.stdout" >&2
            cat "$LOG_DIR/$stem.build.stdout" >&2
            printf '  stderr (%s):\n' "$LOG_DIR/$stem.build.stderr" >&2
            cat "$LOG_DIR/$stem.build.stderr" >&2
        done
    fi
    if [ "${#RUN_FAILURES[@]}" -ne 0 ]; then
        echo "error: ${#RUN_FAILURES[@]} runtime-coverage program(s) failed or timed out:" >&2
        for f in "${RUN_FAILURES[@]}"; do
            stem="$(basename "$f" .hew)"
            printf '  run: %s (exit %s)\n' "$f" "$(cat "$LOG_DIR/$stem.run.exit")" >&2
            printf '  stdout (%s):\n' "$LOG_DIR/$stem.run.stdout" >&2
            cat "$LOG_DIR/$stem.run.stdout" >&2
            printf '  stderr (%s):\n' "$LOG_DIR/$stem.run.stderr" >&2
            cat "$LOG_DIR/$stem.run.stderr" >&2
        done
    fi
    exit 1
fi
if [ "$built" -ne "${#PROGRAMS[@]}" ] || [ "$ran" -ne "$built" ]; then
    echo "error: runtime-coverage execution accounting drifted" >&2
    exit 1
fi

shopt -s nullglob
PROFRAWS=("$PROFRAW_DIR"/*.profraw)
if [ "${#PROFRAWS[@]}" -eq 0 ]; then
    echo "error: no profraw produced — coverage capture failed" >&2
    exit 1
fi
if [ "${#PROFRAWS[@]}" -lt "$ran" ]; then
    echo "error: only ${#PROFRAWS[@]} profraw file(s) for $ran successful program(s)" >&2
    exit 1
fi

echo "==> Phase 4: merge profraw + report runtime coverage"
"$LLVM_PROFDATA" merge -sparse "${PROFRAWS[@]}" -o "$PROFDATA"

BINS=("$BIN_DIR"/*.bin)
OBJ_ARGS=()
for b in "${BINS[@]:1}"; do OBJ_ARGS+=(-object "$b"); done

# Report only the runtime/stdlib source (drop the compiler crates, deps, std).
IGNORE='(/\.cargo/|/rustc/|/usr/|registry|/tests/|hew-cli/|hew-types/|hew-hir/|hew-mir/|hew-codegen-rs/|hew-parser/|hew-lexer/|hew-compile/|hew-analysis/|hew-lsp/|hew-observe/|hew-wasm|hew-sandbox|hew-pkg/|xtask/|hew-testutil/|hew-runtime-testkit/|hew-capability-gen/)'

"$LLVM_COV" report "${BINS[0]}" "${OBJ_ARGS[@]}" \
    -instr-profile="$PROFDATA" \
    --ignore-filename-regex="$IGNORE" | tee "$RT_DIR/runtime-summary.txt"

# Pin meaningful runtime reach, not just profiler-file existence. llvm-cov's
# TOTAL row is: regions, functions, lines, branches; each group reports total,
# missed, percent. A source or corpus change that silently stops exercising a
# runtime subsystem therefore turns this gate red even when all binaries exit.
COVERED_FUNCTIONS="$(awk '$1 == "TOTAL" { print $5 - $6 }' "$RT_DIR/runtime-summary.txt")"
if [ -z "$COVERED_FUNCTIONS" ]; then
    echo "error: llvm-cov report did not contain a TOTAL row" >&2
    exit 1
fi
corpus_nonempty_assert "runtime-e2e-covered-functions" "$COVERED_FUNCTIONS" || exit 1

if [ "$WANT_HTML" -eq 1 ]; then
    echo "==> Generating HTML runtime report"
    "$LLVM_COV" show "${BINS[0]}" "${OBJ_ARGS[@]}" \
        -instr-profile="$PROFDATA" \
        --ignore-filename-regex="$IGNORE" \
        -format=html -output-dir="$RT_DIR/html"
    echo "==> Open $RT_DIR/html/index.html"
fi

echo "==> Runtime e2e coverage summary: $RT_DIR/runtime-summary.txt"
