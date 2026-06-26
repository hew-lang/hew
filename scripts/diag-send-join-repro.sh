#!/usr/bin/env bash
# Throwaway diagnostic harness for the distributed send-vs-join failure.
#
# Reproduces, on a NATIVE Linux host, the intermittent first-send failure of the
# two-process remote-ask round-trip (the `dist_node` fixture). macOS/Windows
# scheduling masks the window; only Linux CI hits it. This script builds the
# env-gated-instrumented runtime, compiles the fixture, and runs many bounded
# concurrent server/client pairs with HEW_DIAG_SEND=1 so that a failing run names
# the FULL causal chain: drop reason, join state, send path, and timestamp sequence.
#
# The instrumentation it depends on is gated behind HEW_DIAG_SEND in:
#   hew-runtime/src/hew_node.rs   — diag_send_log (send path tags + timestamps)
#   hew-runtime/src/connection.rs — reader_cleanup (drop_reason from last_error)
#                                  publish_connection_established (join-state)
#                                  hew_connmgr_send (no-active-conn, socket-write-error)
#   hew-runtime/src/cluster.rs    — unknown-node + known_members list
#
# This file lives on the throwaway `diag/send-join-deep` branch ONLY; it
# is not part of the product and is deleted with the branch.
#
# Usage (from anywhere; resolves the repo root from its own location):
#   LLVM_SYS_221_PREFIX=/usr/lib/llvm-22 bash scripts/diag-send-join-repro.sh [ROUNDS] [CONC] [CTIMEOUT]
# Defaults: ROUNDS=40 CONC=8 CTIMEOUT=25  (=> up to 320 bounded pairs)
#
# Stops early once it has captured >=3 failures (enough to read the full causal chain).
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
# NOTE: HEW_DIAG_SEND is a *runtime* env var — the build does not need it.
cargo build -p hew-lib -p hew-cli
HEW="${REPO}/target/debug/hew"
[ -x "${HEW}" ] || { echo "FATAL: ${HEW} not built"; exit 2; }

EMIT="$(mktemp -d)"
trap 'rm -rf "${EMIT}"' EXIT
echo "==> [2/3] Compiling ${FIXTURE} -> ${EMIT}/dist_node"
"${HEW}" compile --emit-dir "${EMIT}" "${FIXTURE}"
BIN="${EMIT}/dist_node"
[ -x "${BIN}" ] || { echo "FATAL: fixture binary not produced"; exit 2; }
# Verify the instrumentation is baked in (catches stale libhew.a or wrong branch).
if ! strings "${BIN}" | grep -q "DIAG reader-drop"; then
  echo "FATAL: fixture binary lacks DIAG strings — libhew.a is stale (rebuild hew-lib) \
or the HEW_DIAG_SEND instrumentation is missing on this branch."
  exit 2
fi
echo "    fixture binary contains DIAG strings: OK"

OUT="$(mktemp -d)"
echo "==> [3/3] Repro: ROUNDS=${ROUNDS} CONC=${CONC} CTIMEOUT=${CTIMEOUT}s  artifacts=${OUT}"
echo "    (HEW_DIAG_SEND=1 set for each server+client pair)"

alloc_port() {
  python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()'
}

# Run one server/client pair; write verdict + (on fail) the full DIAG chain dump.
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

  # ---- Failure: capture the FULL causal chain from the new DIAG lines. ----

  # 1. SEND -1 PATH TAG: which exact code path returned -1?
  local path_tag
  if grep -q "path=route-missing" "${ce}" 2>/dev/null; then
    path_tag="route-missing"
  elif grep -q "path=no-active-conn-for-node" "${ce}" 2>/dev/null; then
    path_tag="no-active-conn-for-node"
  elif grep -q "path=socket-write-error" "${ce}" 2>/dev/null; then
    path_tag="socket-write-error"
  elif grep -q "path=connmgr-send-error" "${ce}" 2>/dev/null; then
    path_tag="connmgr-send-error"
  elif grep -q "path=node-not-running" "${ce}" 2>/dev/null; then
    path_tag="node-not-running"
  elif grep -q "path=connmgr-null" "${ce}" 2>/dev/null; then
    path_tag="connmgr-null"
  else
    path_tag="UNKNOWN (no DIAG -1 tag — check serialization/validator or stdout)"
  fi

  # 2. DROP REASON: the exact framed_recv error string from last_error.
  #    New format: [DIAG reader-drop] t=<ms>ms ... drop_reason="..."
  local client_drop_reason server_drop_reason
  client_drop_reason="$(grep "DIAG reader-drop" "${ce}" 2>/dev/null | \
    sed -E 's/.*drop_reason="([^"]+)".*/\1/' | head -1)"
  [ -z "${client_drop_reason}" ] && client_drop_reason="(no client reader-drop)"
  server_drop_reason="$(grep "DIAG reader-drop" "${se}" 2>/dev/null | \
    sed -E 's/.*drop_reason="([^"]+)".*/\1/' | head -1)"
  [ -z "${server_drop_reason}" ] && server_drop_reason="(no server reader-drop)"

  # 3. JOIN STATE: did "unknown node / waiting for join" fire? On both pass + fail?
  #    New format: [DIAG cluster-unknown] t=<ms>ms peer_node=<N> known_members=[...]
  local client_cluster_unknown server_cluster_unknown
  client_cluster_unknown="$(grep "DIAG cluster-unknown" "${ce}" 2>/dev/null | head -1)"
  [ -z "${client_cluster_unknown}" ] && client_cluster_unknown="(not seen on client)"
  server_cluster_unknown="$(grep "DIAG cluster-unknown" "${se}" 2>/dev/null | head -1)"
  [ -z "${server_cluster_unknown}" ] && server_cluster_unknown="(not seen on server)"

  # 4. JOIN STATE at route-install: did publish fire with notify_result=1?
  local client_join_state server_join_state
  client_join_state="$(grep "DIAG join-state" "${ce}" 2>/dev/null | head -1)"
  [ -z "${client_join_state}" ] && client_join_state="(not seen on client)"
  server_join_state="$(grep "DIAG join-state" "${se}" 2>/dev/null | head -1)"
  [ -z "${server_join_state}" ] && server_join_state="(not seen on server)"

  # 5. CONN TABLE at moment of failure.
  local conns
  conns="$(grep -Eo 'conns=\[[^]]*\]' "${ce}" 2>/dev/null | tail -1)"

  # 6. TIMESTAMP SEQUENCE from all DIAG lines (ordered).
  # Full client DIAG sequence:
  local client_diag_seq server_diag_seq
  client_diag_seq="$(grep 'DIAG' "${ce}" 2>/dev/null | tr '\n' '|')"
  server_diag_seq="$(grep 'DIAG' "${se}" 2>/dev/null | tr '\n' '|')"

  {
    echo "${idx} FAIL port=${port} crc=${crc}"
    echo "  [SEND -1 PATH TAG]          : ${path_tag}"
    echo "  [DROP REASON - client]      : ${client_drop_reason}"
    echo "  [DROP REASON - server]      : ${server_drop_reason}"
    echo "  [JOIN-STATE notify - client]: ${client_join_state}"
    echo "  [JOIN-STATE notify - server]: ${server_join_state}"
    echo "  [CLUSTER-UNKNOWN - client]  : ${client_cluster_unknown}"
    echo "  [CLUSTER-UNKNOWN - server]  : ${server_cluster_unknown}"
    echo "  [CONN TABLE @ fail]         : ${conns:-<none captured>}"
    echo "  [CLIENT STDOUT]             : $(tr '\n' '|' < "${co}")"
    echo "  [CLIENT DIAG SEQUENCE]      :"
    printf '%s' "${client_diag_seq}" | tr '|' '\n' | sed 's/^/    /'
    echo "  [SERVER DIAG SEQUENCE]      :"
    printf '%s' "${server_diag_seq}" | tr '|' '\n' | sed 's/^/    /'
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
    echo "(captured >=3 failures — enough to read the full causal chain; stopping early)"
    break
  fi
done

echo "================================================================"
echo "REPRO RESULT: total=${total} pass=${passes} fail=${fails} notready=${notready}"
echo "artifacts=${OUT}  (NOTE: ${EMIT}/dist_node is removed on exit)"
echo
echo "Reading a FAIL — what each field means:"
echo "  [SEND -1 PATH TAG]"
echo "    route-missing           -> hew_routing_lookup returned no route; the route was"
echo "                              removed by reader_cleanup before this send."
echo "    no-active-conn-for-node -> a route was found but no ACTIVE conn with matching"
echo "                              peer_node_id in hew_connmgr_send (conn_id recycled,"
echo "                              state!=ACTIVE, or peer_node_id mismatch)."
echo "    socket-write-error      -> conn was ACTIVE, route existed, but the socket write"
echo "                              failed (peer closed / ECONNRESET / EPIPE)."
echo "    connmgr-send-error      -> connmgr_send returned -1 after route-ok; check the"
echo "                              connmgr_send DIAG line for the sub-path."
echo ""
echo "  [DROP REASON - client/server]"
echo "    The exact string from framed_recv's last_error at the moment of drop:"
echo "    'peer closed while reading header'  -> remote sent FIN before the first frame"
echo "    'header read failed: <errno>'       -> socket error (ECONNRESET, EPIPE, etc.)"
echo "    'peer closed while reading payload' -> FIN mid-frame"
echo "    (no last_error — ops.recv returned 0) -> ops.recv was None or returned 0"
echo ""
echo "  [CLUSTER-UNKNOWN] on both PASS and FAIL runs -> this message is a red herring;"
echo "    the route still installs (notify returns 1). On FAIL only -> join-order IS causal."
echo ""
echo "  [CONN TABLE @ fail]"
echo "    Shows the connection-table snapshot at the instant hew_node_send failed."
echo "    state=closed means the conn was removed before the send; state=active but"
echo "    peer_node_id != target_node means a conn_id reuse/mismatch."
