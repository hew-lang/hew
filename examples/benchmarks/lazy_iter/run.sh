#!/usr/bin/env bash
# std::iter lazy-vs-eager bench harness.
#
# Each binary times a single run and exits. We loop in shell because
# v0.4.0 still hits the `for + closures + now_nanos` MLIR bug when we
# try to time multiple runs inside one process.
#
# Outputs results.md alongside this script.

set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

# Prefer the in-repo build if it exists, otherwise fall back to a
# system-installed hew on PATH.
HEW="${HEW:-}"
if [ -z "$HEW" ]; then
  REPO_ROOT="$(cd "$ROOT/../../.." && pwd)"
  for candidate in \
      "$REPO_ROOT/target/release/hew" \
      "$REPO_ROOT/target/debug/hew" \
      "$(command -v hew || true)"; do
    if [ -n "$candidate" ] && [ -x "$candidate" ]; then HEW="$candidate"; break; fi
  done
fi
if [ -z "$HEW" ] || [ ! -x "$HEW" ]; then
  echo "error: no hew compiler found; set \$HEW or build with cargo build --release" >&2
  exit 1
fi

RUNS="${RUNS:-12}"      # total invocations (1 warm-up + RUNS-1 measured)
mkdir -p "$ROOT/build"

echo "== build =="
"$HEW" build "$ROOT/bench_eager.hew" -o "$ROOT/build/bench_eager" >/dev/null
"$HEW" build "$ROOT/bench_lazy.hew"  -o "$ROOT/build/bench_lazy"  >/dev/null

RAW="$ROOT/build/raw.txt"
: > "$RAW"

run_one() {
  local label="$1"
  local bin="$2"
  echo "== running $label =="
  for i in $(seq 0 $((RUNS - 1))); do
    line="$("$bin")"
    if [ "$i" -ge 1 ]; then
      r="$( echo "$line" | grep -oE 'result=[0-9]+'     | cut -d= -f2)"
      us="$(echo "$line" | grep -oE 'elapsed_us=[0-9]+' | cut -d= -f2)"
      echo "$label $((i-1)) $r $us" >> "$RAW"
    fi
  done
}

run_one eager "$ROOT/build/bench_eager"
run_one lazy  "$ROOT/build/bench_lazy"

python3 - "$RAW" > "$ROOT/results.md" <<'PY'
import sys, statistics
rows = {}
result_for = {}
for line in open(sys.argv[1]):
    parts = line.split()
    if len(parts) != 4: continue
    lab, _i, r, us = parts
    rows.setdefault(lab, []).append(int(us))
    result_for[lab] = r

def stats(xs):
    xs = sorted(xs)
    n = len(xs)
    median = statistics.median(xs)
    p99 = xs[int(0.99 * (n - 1))]
    stdev = statistics.pstdev(xs) if n > 1 else 0.0
    return median, p99, min(xs), max(xs), stdev, n

print("# std::iter — lazy vs eager filter+map+sum")
print()
print("Workload: `sum(map(add5, filter(even, 1..=100_000)))`. Expected result `2_500_300_000`.")
print()
print("| Variant | Result | Median (µs) | p99 (µs) | Min (µs) | Max (µs) | Stdev (µs) | Runs |")
print("|---|---|---:|---:|---:|---:|---:|---:|")
order = ["eager", "lazy"]
medians = {}
for lab in order:
    if lab not in rows: continue
    med, p99, mn, mx, sd, n = stats(rows[lab])
    medians[lab] = med
    print(f"| {lab} | {result_for[lab]} | {med:.0f} | {p99} | {mn} | {mx} | {sd:.1f} | {n} |")
print()
if "eager" in medians and "lazy" in medians:
    ratio = medians["eager"] / medians["lazy"]
    print(f"**Lazy speedup:** {ratio:.2f}× over eager (median over {len(rows['lazy'])} runs).")
PY

cat "$ROOT/results.md"
