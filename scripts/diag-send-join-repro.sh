#!/usr/bin/env bash
# Throwaway diagnostic harness for the distributed send-vs-join failure.
#
# Reproduces, on a NATIVE Linux host, the intermittent first-send failure of the
# two-process remote-ask round-trip (the `dist_node` fixture). macOS/Windows
# scheduling masks the window; only Linux CI hits it. This script builds the
# env-gated-instrumented runtime, compiles the fixture, and runs many bounded
# concurrent server/client pairs with HEW_DIAG_SEND=1 so that a failing run names
# the exact `-1` path and prints the connection-table state at the instant of
# failure.
#
# The instrumentation it depends on is gated behind HEW_DIAG_SEND in
# hew-runtime/src/{hew_node,connection}.rs (env-gated; no effect when unset).
#
# This file lives on the throwaway `diag/send-join-instrumented` branch ONLY; it
# is not part of the product and is deleted with the branch.
#
# Usage (from anywhere; resolves the repo root from its own location):
#   LLVM_SYS_221_PREFIX=/usr/lib/llvm-22 bash scripts/diag-send-join-repro.sh [ROUNDS] [CONC] [CTIMEOUT]
# Defaults: ROUNDS=40 CONC=8 CTIMEOUT=25  (=> up to 320 bounded pairs)
#
# Stops early once it has captured >=3 failures (enough to read the path tag).
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

echo "==> [1/3] Building instrumented libhew.a + hew driver (cargo build -p hew-lib -p hew-cli)"
# The compiled fixture links target/<profile>/libhew.a produced by hew-lib (NOT
# libhew_runtime.a); a stale libhew.a silently drops the instrumentation.
cargo build -p hew-lib -p hew-cli 2>&1 | tail -6
HEW="${REPO}/target/debug/hew"
[ -x "${HEW}" ] || { echo "FATAL: ${HEW} not built"; exit 2; }

EMIT="$(mktemp -d)"
trap 'rm -rf "${EMIT}"' EXIT
echo "==> [2/3] Compiling ${FIXTURE} -> ${EMIT}/dist_node"
"${HEW}" compile --emit-dir "${EMIT}" "${FIXTURE}" 2>&1 | grep -E "native:|error:" | tail
BIN="${EMIT}/dist_node"
[ -x "${BIN}" ] || { echo "FATAL: fixture binary not produced"; exit 2; }
if ! strings "${BIN}" | grep -q "DIAG send"; then
  echo "FATAL: fixture binary lacks DIAG strings — libhew.a is stale (rebuild hew-lib) \
or the HEW_DIAG_SEND instrumentation is missing on this branch."; exit 2
fi

OUT="$(mktemp -d)"
echo "==> [3/3] Repro: ROUNDS=${ROUNDS} CONC=${CONC} CTIMEOUT=${CTIMEOUT}s  artifacts=${OUT}"
echo "    (HEW_DIAG_SEND=1; each client wrapped in 'timeout ${CTIMEOUT}')"

alloc_port() {
  python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()'
}

# Run one server/client pair; write a one-line verdict + (on fail) the diag dump.
run_one() {
  local idx="$1" port so se co ce srv ready crc
  port="$(alloc_port)"
  so="${OUT}/s.${idx}.out"; se="${OUT}/s.${idx}.err"
  co="${OUT}/c.${idx}.out"; ce="${OUT}/c.${idx}.err"

  HEW_DIAG_SEND=1 HEW_TRANSPORT=tcp HEW_DIST_ROLE=server HEW_DIST_PORT="${port}" \
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
    kill "${srv}" 2>/dev/null; wait "${srv}" 2>/dev/null
    return
  fi

  HEW_DIAG_SEND=1 HEW_TRANSPORT=tcp HEW_DIST_ROLE=client HEW_DIST_PORT="${port}" \
    HEW_DIST_SCENARIO=remote_ask timeout "${CTIMEOUT}" "${BIN}" >"${co}" 2>"${ce}"
  crc=$?
  kill "${srv}" 2>/dev/null; wait "${srv}" 2>/dev/null

  if grep -q "PASS remote_ask get-count=42" "${co}" 2>/dev/null && ! grep -q "FAIL " "${co}" 2>/dev/null; then
    echo "${idx} PASS port=${port}"
    return
  fi

  # --- Failure: classify the -1 path and the drop precedence. ---
  local path_tag client_drop server_drop conns
  if grep -q "path=route-missing" "${ce}" 2>/dev/null; then
    path_tag="route-missing"
  elif grep -q "path=no-active-conn-for-node" "${ce}" 2>/dev/null; then
    path_tag="no-active-conn-for-node"
  elif grep -q "path=node-not-running" "${ce}" 2>/dev/null; then
    path_tag="node-not-running"
  elif grep -q "path=connmgr-null" "${ce}" 2>/dev/null; then
    path_tag="connmgr-null"
  else
    path_tag="UNKNOWN (no DIAG -1 tag; check serialization/validator or stdout)"
  fi
  client_drop="no"
  grep -q "DIAG reader-drop" "${ce}" 2>/dev/null && client_drop="YES (client reader saw an UNEXPECTED drop)"
  server_drop="no"
  grep -q "DIAG reader-drop" "${se}" 2>/dev/null && server_drop="yes"
  conns="$(grep -Eo 'conns=\[[^]]*\]' "${ce}" 2>/dev/null | tail -1)"

  {
    echo "${idx} FAIL port=${port} crc=${crc}"
    echo "    -1 path tag      : ${path_tag}"
    echo "    client reader-drop preceded?: ${client_drop}"
    echo "    server reader-drop seen?    : ${server_drop}"
    echo "    conn-table @ fail : ${conns:-<none captured>}"
    echo "    client stdout    : $(tr '\n' '|' < "${co}")"
    echo "    client DIAG      : $(grep -E 'DIAG' "${ce}" 2>/dev/null | tr '\n' '|')"
    echo "    client cluster   : $(grep -E 'cluster' "${ce}" 2>/dev/null | tr '\n' '|')"
    echo "    server DIAG      : $(grep -E 'DIAG' "${se}" 2>/dev/null | tr '\n' '|')"
    echo "    server cluster   : $(grep -E 'cluster' "${se}" 2>/dev/null | tr '\n' '|')"
  }
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
  if [ "${fails}" -ge 3 ]; then
    echo "(captured >=3 failures — enough to read the path tag; stopping early)"
    break
  fi
done

echo "================================================================"
echo "REPRO RESULT: total=${total} pass=${passes} fail=${fails} notready=${notready}"
echo "artifacts=${OUT}  (NOTE: ${EMIT} is removed on exit)"
echo
echo "Reading a FAIL:"
echo "  '-1 path tag = route-missing'           -> routing table had no route for the"
echo "                                             server node at send time (the route was"
echo "                                             torn out, likely by a reconnect window)."
echo "  '-1 path tag = no-active-conn-for-node'  -> a route existed but no ACTIVE conn whose"
echo "                                             peer_node_id matched (conn not yet up / mismatch)."
echo "  'client reader-drop preceded? = YES'     -> an UNEXPECTED client-side connection drop"
echo "                                             removed the route before the first send"
echo "                                             (confirms the drop/reconnect mechanism)."
echo "  'conn-table @ fail'                      -> the connections snapshot at the failing send."
