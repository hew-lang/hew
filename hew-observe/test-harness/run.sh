#!/usr/bin/env bash
# Test harness for hew-observe: compiles a workload, runs it with profiling,
# and launches hew-observe in a tmux session for visual inspection.
#
# Usage:
#   ./run.sh              # Interactive — opens tmux with observe TUI
#   ./run.sh --demo       # Demo mode (no workload needed)
#   ./run.sh --screenshot # Non-interactive — captures screenshots and exits
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
HEW="$REPO_ROOT/target/debug/hew"
OBSERVE="$REPO_ROOT/target/debug/hew-observe"
WORKLOAD="$SCRIPT_DIR/observe_workload.hew"
SCREENSHOT_DIR="$SCRIPT_DIR/screenshots"
PPROF_PORT=6060
SESSION="hew-observe-test"

mode="interactive"
for arg in "$@"; do
    case "$arg" in
    --demo) mode="demo" ;;
    --screenshot) mode="screenshot" ;;
    --help | -h)
        echo "Usage: $0 [--demo | --screenshot]"
        echo "  (default)     Interactive tmux session with workload + observe"
        echo "  --demo        Launch observe in demo mode (no compilation needed)"
        echo "  --screenshot  Capture screenshots non-interactively and exit"
        exit 0
        ;;
    esac
done

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
echo "▸ Starting workload with HEW_PPROF=:$PPROF_PORT..."
HEW_PPROF=":$PPROF_PORT" "$SCRIPT_DIR/observe_workload" &
WORKLOAD_PID=$!

# Wait for profiler to come up
for i in $(seq 1 20); do
    if curl -s "http://localhost:$PPROF_PORT/api/metrics" >/dev/null 2>&1; then
        echo "▸ Profiler endpoint ready."
        break
    fi
    sleep 0.25
done

if [[ "$mode" == "screenshot" ]]; then
    # Non-interactive: use tmux to run observe, capture panes
    mkdir -p "$SCREENSHOT_DIR"

    tmux kill-session -t "$SESSION" 2>/dev/null || true
    tmux new-session -d -s "$SESSION" -x 120 -y 40

    # Tab 1: Overview (default)
    tmux send-keys -t "$SESSION" "$OBSERVE --addr localhost:$PPROF_PORT" Enter
    sleep 3 # let it fetch a few cycles

    tmux capture-pane -t "$SESSION" -p >"$SCREENSHOT_DIR/01-overview.txt"
    echo "▸ Captured: 01-overview.txt"

    # Tab 2: Actors
    tmux send-keys -t "$SESSION" Tab
    sleep 1.5
    tmux capture-pane -t "$SESSION" -p >"$SCREENSHOT_DIR/02-actors.txt"
    echo "▸ Captured: 02-actors.txt"

    # Tab 3: Supervisors
    tmux send-keys -t "$SESSION" Tab
    sleep 1
    tmux capture-pane -t "$SESSION" -p >"$SCREENSHOT_DIR/03-supervisors.txt"
    echo "▸ Captured: 03-supervisors.txt"

    # Tab 4: Messages
    tmux send-keys -t "$SESSION" Tab
    sleep 1.5
    tmux capture-pane -t "$SESSION" -p >"$SCREENSHOT_DIR/04-messages.txt"
    echo "▸ Captured: 04-messages.txt"

    # Tab 5: Timeline
    tmux send-keys -t "$SESSION" Tab
    sleep 1
    tmux capture-pane -t "$SESSION" -p >"$SCREENSHOT_DIR/05-timeline.txt"
    echo "▸ Captured: 05-timeline.txt"

    # Tab 6: Cluster
    tmux send-keys -t "$SESSION" Tab
    sleep 1
    tmux capture-pane -t "$SESSION" -p >"$SCREENSHOT_DIR/06-cluster.txt"
    echo "▸ Captured: 06-cluster.txt"

    # Help overlay
    tmux send-keys -t "$SESSION" '?'
    sleep 0.5
    tmux capture-pane -t "$SESSION" -p >"$SCREENSHOT_DIR/07-help.txt"
    echo "▸ Captured: 07-help.txt"

    tmux send-keys -t "$SESSION" q
    echo ""
    echo "Screenshots saved to: $SCREENSHOT_DIR/"
    echo "View with: cat $SCREENSHOT_DIR/01-overview.txt"
else
    # Interactive: attach to tmux with split panes
    tmux kill-session -t "$SESSION" 2>/dev/null || true
    tmux new-session -d -s "$SESSION" -x 160 -y 50

    # Top pane: hew-observe
    tmux send-keys -t "$SESSION" "$OBSERVE --addr localhost:$PPROF_PORT" Enter

    echo ""
    echo "▸ tmux session '$SESSION' ready."
    echo "  Attach with: tmux attach -t $SESSION"
    echo "  Keys: Tab=switch tabs, ↑↓=scroll, s=sort, /=filter, ?=help, q=quit"
    echo ""

    tmux attach -t "$SESSION"
fi
