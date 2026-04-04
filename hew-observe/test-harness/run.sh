#!/usr/bin/env bash
# Test harness for hew-observe: compiles a workload, runs it with profiling,
# and launches hew-observe in a tmux session for visual inspection.
#
# On Unix (Linux / macOS) the harness uses HEW_PPROF=auto — the runtime
# binds a per-user unix socket and writes a JSON discovery file that
# hew-observe picks up automatically (no --addr needed).  This exercises
# the primary recommended workflow added in feat(cli): add --profile (#565).
#
# On non-Unix hosts the harness falls back to HEW_PPROF=:6060 (TCP) and
# passes --addr localhost:6060 explicitly, matching the non-Unix note in the
# hew-cli README and HEW-SPEC.md §10.2.
#
# Usage:
#   ./run.sh              # Interactive — opens tmux with observe TUI
#   ./run.sh --demo       # Demo mode (no workload needed)
#   ./run.sh --screenshot # Non-interactive — captures screenshots and exits
#   ./run.sh --tcp        # Force TCP mode (useful on Unix for debugging)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
HEW="$REPO_ROOT/target/debug/hew"
OBSERVE="$REPO_ROOT/target/debug/hew-observe"
WORKLOAD="$SCRIPT_DIR/observe_workload.hew"
SCREENSHOT_DIR="$SCRIPT_DIR/screenshots"
PPROF_PORT=6060
SESSION="hew-observe-test"

# Detect Unix so we can use HEW_PPROF=auto / unix socket discovery.
case "$(uname -s 2>/dev/null)" in
Linux | Darwin) IS_UNIX=1 ;;
*) IS_UNIX=0 ;;
esac

mode="interactive"
force_tcp=0
for arg in "$@"; do
    case "$arg" in
    --demo) mode="demo" ;;
    --screenshot) mode="screenshot" ;;
    --tcp) force_tcp=1 ;;
    --help | -h)
        echo "Usage: $0 [--demo | --screenshot | --tcp]"
        echo "  (default)     Interactive tmux session with workload + observe"
        echo "  --demo        Launch observe in demo mode (no compilation needed)"
        echo "  --screenshot  Capture screenshots non-interactively and exit"
        echo "  --tcp         Force TCP mode (default on non-Unix)"
        exit 0
        ;;
    esac
done

capture_screenshot() {
    local file_name="$1"
    local label="$2"
    local marker="$3"
    local capture_path="$SCREENSHOT_DIR/$file_name"

    tmux capture-pane -t "$SESSION" -p >"$capture_path"
    if ! grep -Fq -- "$marker" "$capture_path"; then
        echo "✗ Screenshot validation failed for $file_name ($label): missing marker '$marker'."
        exit 1
    fi
    echo "▸ Captured: $file_name ($label)"
}

cleanup() {
    # Kill any background workload
    if [[ -n "${WORKLOAD_PID:-}" ]]; then
        kill "$WORKLOAD_PID" 2>/dev/null || true
        wait "$WORKLOAD_PID" 2>/dev/null || true
    fi
    # Kill tmux session
    tmux kill-session -t "$SESSION" 2>/dev/null || true
    # Clean compiled binary
    rm -f "$SCRIPT_DIR/observe_workload" 2>/dev/null || true
}
trap cleanup EXIT

# ── Build ───────────────────────────────────────────────────────────
echo "▸ Building hew-observe..."
cargo build -p hew-observe --quiet 2>&1

if [[ "$mode" == "demo" ]]; then
    echo "▸ Launching hew-observe in demo mode..."
    exec "$OBSERVE" --demo
fi

if [[ ! -x "$HEW" ]]; then
    echo "▸ Building hew compiler..."
    (cd "$REPO_ROOT" && make 2>&1) || {
        echo "✗ Build failed. Falling back to --demo mode."
        exec "$OBSERVE" --demo
    }
fi

# ── Compile workload ────────────────────────────────────────────────
echo "▸ Compiling workload: $(basename "$WORKLOAD")..."
"$HEW" build "$WORKLOAD" -o "$SCRIPT_DIR/observe_workload" 2>&1 || {
    echo "✗ Compilation failed. Falling back to --demo mode."
    exec "$OBSERVE" --demo
}

# ── Run ─────────────────────────────────────────────────────────────
# Choose transport: unix-socket auto-discovery on Unix, TCP elsewhere.
if [[ "$IS_UNIX" -eq 1 && "$force_tcp" -eq 0 ]]; then
    echo "▸ Starting workload with HEW_PPROF=auto (unix socket, auto-discovery)..."
    HEW_PPROF=auto "$SCRIPT_DIR/observe_workload" &
    WORKLOAD_PID=$!

    # Wait for the discovery JSON file to appear (runtime writes it when the
    # unix socket is bound and ready).  hew-observe --list reads the same files.
    echo "▸ Waiting for profiler discovery..."
    _i=0
    while [[ $_i -lt 40 ]]; do
        if "$OBSERVE" --list 2>/dev/null | grep -qE "^[0-9]+"; then
            echo "▸ Profiler discovered (unix socket, auto-discovered by hew-observe)."
            break
        fi
        sleep 0.25
        (( _i++ )) || true
    done

    OBSERVE_CMD="$OBSERVE"          # no --addr: uses auto-discovery
    OBSERVE_ADDR="(auto-discovered)"
else
    echo "▸ Starting workload with HEW_PPROF=:$PPROF_PORT (TCP)..."
    HEW_PPROF=":$PPROF_PORT" "$SCRIPT_DIR/observe_workload" &
    WORKLOAD_PID=$!

    # Wait for the TCP profiler endpoint to accept connections.
    echo "▸ Waiting for profiler endpoint..."
    _i=0
    while [[ $_i -lt 20 ]]; do
        if curl -s "http://localhost:$PPROF_PORT/api/metrics" >/dev/null 2>&1; then
            echo "▸ Profiler endpoint ready (TCP localhost:$PPROF_PORT)."
            break
        fi
        sleep 0.25
        (( _i++ )) || true
    done

    OBSERVE_CMD="$OBSERVE --addr localhost:$PPROF_PORT"
    OBSERVE_ADDR="localhost:$PPROF_PORT"
fi

if [[ "$mode" == "screenshot" ]]; then
    # Non-interactive: use tmux to run observe, capture panes in UI tab order.
    # These markers are lightweight guards against the harness drifting from
    # hew-observe's actual tab order again.
    screenshot_files=(
        "01-overview.txt"
        "02-actors.txt"
        "03-supervisors.txt"
        "04-crashes.txt"
        "05-cluster.txt"
        "06-messages.txt"
        "07-timeline.txt"
    )
    screenshot_labels=(
        "Overview"
        "Actors"
        "Supervisors"
        "Crashes"
        "Cluster"
        "Messages"
        "Timeline"
    )
    screenshot_markers=(
        "System Overview"
        "Actor List"
        "Supervision Tree"
        "Crash Log"
        "Cluster Topology"
        "[f] filter actor"
        "[+/-] zoom"
    )
    screenshot_delays=(
        "3"
        "1.5"
        "1"
        "1"
        "1"
        "1.5"
        "1"
    )

    mkdir -p "$SCREENSHOT_DIR"
    rm -f "$SCREENSHOT_DIR"/*.txt

    tmux kill-session -t "$SESSION" 2>/dev/null || true
    tmux new-session -d -s "$SESSION" -x 120 -y 40

    # shellcheck disable=SC2086  # OBSERVE_CMD is intentionally word-split
    tmux send-keys -t "$SESSION" "$OBSERVE_CMD" Enter
    for idx in "${!screenshot_files[@]}"; do
        if [[ "$idx" -gt 0 ]]; then
            tmux send-keys -t "$SESSION" Tab
        fi
        sleep "${screenshot_delays[$idx]}" # let the active tab refresh before capture
        capture_screenshot \
            "${screenshot_files[$idx]}" \
            "${screenshot_labels[$idx]}" \
            "${screenshot_markers[$idx]}"
    done

    # Help overlay
    tmux send-keys -t "$SESSION" '?'
    sleep 0.5
    capture_screenshot "08-help.txt" "Help" "Key Bindings"

    tmux send-keys -t "$SESSION" q
    echo ""
    echo "Screenshots saved to: $SCREENSHOT_DIR/"
    echo "View with: cat $SCREENSHOT_DIR/01-overview.txt"
else
    # Interactive: attach to tmux with split panes
    tmux kill-session -t "$SESSION" 2>/dev/null || true
    tmux new-session -d -s "$SESSION" -x 160 -y 50

    # Top pane: hew-observe (auto-discover on Unix, --addr on TCP)
    # shellcheck disable=SC2086  # OBSERVE_CMD is intentionally word-split
    tmux send-keys -t "$SESSION" "$OBSERVE_CMD" Enter

    echo ""
    echo "▸ tmux session '$SESSION' ready.  Profiler: $OBSERVE_ADDR"
    echo "  Attach with: tmux attach -t $SESSION"
    echo "  Keys: Tab=switch tabs, ↑↓=scroll, s=sort, /=filter, ?=help, q=quit"
    echo ""

    tmux attach -t "$SESSION"
fi
