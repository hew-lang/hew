#!/usr/bin/env bash
# Concurrent burst harness for the distributed teardown-ordering fix.
#
# Builds the worktree's libhew.a + hew driver, compiles the dist_node fixture,
# and runs N bounded concurrent server/client pairs over loopback TCP. Each pair
# votes PASS or FAIL from the client's stdout. A summary line is printed at the
# end and the script exits nonzero if any pair failed.
#
# This is instrumentation-free: it works against any branch (fixed or unfixed)
# without requiring the HEW_DIAG_SEND build-time probe strings. Pass/fail
# detection relies solely on the fixture's own output contract:
#   "PASS remote_ask get-count=42"  — both tells and both asks succeeded
#   "FAIL remote_ask ..."            — any mismatch or send error
#
# Before the teardown-ordering fix, Linux CI saw 8/8 FAIL on a burst of 40
# pairs at concurrency 8. macOS scheduling masks the race window and typically
# reports all-PASS; that is expected and does NOT invalidate the harness —
# it is confirming the script runs to completion so obelisk can use it.
#
# Usage (from anywhere; resolves the repo root from its own location):
#   LLVM_SYS_221_PREFIX=/usr/lib/llvm-22 bash scripts/diag-send-join-repro.sh [ROUNDS] [CONC] [CTIMEOUT]
# Defaults: ROUNDS=40 CONC=8 CTIMEOUT=25  (=> up to 320 bounded pairs)
set -u

ROUNDS="${1:-40}"
CONC="${2:-8}"
CTIMEOUT="${3:-25}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${REPO}" || { echo "FATAL: cannot cd to repo root ${REPO}"; exit 2; }

FIXTURE="hew-cli/tests/fixtures/distributed/dist_node.hew"
[ -f "${FIXTURE}" ] || { echo "FATAL: fixture not found at ${REPO}/${FIXTURE}"; exit 2; }

command -v timeout >/dev/null 2>&1 || { echo "FATAL: 'timeout' not on PATH (need coreutils)"; exit 2; }
command -v python3 >/dev/null 2>&1 || { echo "FATAL: 'python3' not on PATH (port allocator)"; exit 2; }

echo "==> [1/3] Building libhew.a + hew driver (cargo build -p hew-lib -p hew-cli)"
cargo build -p hew-lib -p hew-cli 2>&1
HEW="${REPO}/target/debug/hew"
[ -x "${HEW}" ] || { echo "FATAL: ${HEW} not built"; exit 2; }

EMIT="$(mktemp -d)"
trap 'rm -rf "${EMIT}"' EXIT
echo "==> [2/3] Compiling ${FIXTURE} -> ${EMIT}/dist_node"
"${HEW}" compile --emit-dir "${EMIT}" "${FIXTURE}" 2>&1
BIN="${EMIT}/dist_node"
[ -x "${BIN}" ] || { echo "FATAL: fixture binary not produced at ${BIN}"; exit 2; }

OUT="$(mktemp -d)"
echo "==> [3/3] Burst: ROUNDS=${ROUNDS} CONC=${CONC} CTIMEOUT=${CTIMEOUT}s  artifacts=${OUT}"

alloc_port() {
  python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()'
}

# Run one server/client pair. Writes a one-line verdict to stdout.
run_one() {
  local idx="$1" port so se co ce srv ready crc
  port="$(alloc_port)"
  so="${OUT}/s.${idx}.out"; se="${OUT}/s.${idx}.err"
  co="${OUT}/c.${idx}.out"; ce="${OUT}/c.${idx}.err"

  HEW_TRANSPORT=tcp HEW_DIST_ROLE=server HEW_DIST_PORT="${port}" \
    HEW_DIST_SCENARIO=remote_ask "${BIN}" >"${so}" 2>"${se}" &
  srv=$!

  ready=0
  for _ in $(seq 1 300); do
    grep -q "^READY " "${so}" 2>/dev/null && { ready=1; break; }
    kill -0 "${srv}" 2>/dev/null || break
    sleep 0.02
  done
  if [ "${ready}" -ne 1 ]; then
    echo "${idx} NOTREADY port=${port}"
    kill -9 "${srv}" 2>/dev/null; wait "${srv}" 2>/dev/null
    return
  fi

  HEW_TRANSPORT=tcp HEW_DIST_ROLE=client HEW_DIST_PORT="${port}" \
    HEW_DIST_SCENARIO=remote_ask timeout "${CTIMEOUT}" "${BIN}" >"${co}" 2>"${ce}"
  crc=$?
  kill -9 "${srv}" 2>/dev/null; wait "${srv}" 2>/dev/null

  if grep -q "PASS remote_ask get-count=42" "${co}" 2>/dev/null && \
     ! grep -q "FAIL " "${co}" 2>/dev/null; then
    echo "${idx} PASS port=${port}"
    return
  fi

  # Extract the first FAIL line for a concise one-line report.
  local fail_detail
  fail_detail="$(grep "FAIL " "${co}" 2>/dev/null | head -1)"
  echo "${idx} FAIL port=${port} crc=${crc} detail=${fail_detail:-<no FAIL line — see artifacts>}"
  echo "    client stdout: $(tr '\n' '|' < "${co}" 2>/dev/null)"
  echo "    client stderr: $(tr '\n' '|' < "${ce}" 2>/dev/null | head -c 400)"
}

total=0; passes=0; fails=0; notready=0
for round in $(seq 1 "${ROUNDS}"); do
  pids=()
  for k in $(seq 1 "${CONC}"); do
    idx="r${round}k${k}"
    run_one "${idx}" > "${OUT}/res.${idx}" 2>&1 &
    pids+=($!)
  done
  for p in "${pids[@]}"; do wait "${p}"; done
  for k in $(seq 1 "${CONC}"); do
    idx="r${round}k${k}"
    res="$(cat "${OUT}/res.${idx}" 2>/dev/null)"
    total=$((total+1))
    if printf '%s' "${res}" | grep -q " PASS "; then
      passes=$((passes+1))
    elif printf '%s' "${res}" | grep -q " NOTREADY "; then
      notready=$((notready+1)); echo "${res}"
    else
      fails=$((fails+1)); echo "${res}"
    fi
  done
  echo "-- round ${round}: total=${total} pass=${passes} fail=${fails} notready=${notready} --"
done

echo "================================================================"
echo "RESULT: total=${total} pass=${passes} fail=${fails} notready=${notready}"
[ "${fails}" -eq 0 ] || { echo "VERDICT: FAIL (${fails} pair(s) failed)"; exit 1; }
echo "VERDICT: PASS"
