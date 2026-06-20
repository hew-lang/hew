#!/usr/bin/env bash
# ll-oracle corpus driver — `ll-golden` and `ll-diff` make targets.
#
# Compiles every fixture under tests/ll-oracle/corpus/ with `hew compile
# --emit-dir` for both the native and wasm32 targets, then either:
#
#   golden  — writes the normalised per-fn corpus into tests/ll-oracle/corpus/golden/
#   verify  — recompiles and diffs against the committed golden; exits non-zero
#             if any function body differs from the golden (one or more bodies)
#
# The golden is the byte-identity proof substrate for pure codegen refactors
# (dedup, extract-helper, file-split): a refactor that claims "zero emitted-
# IR change" must leave the golden untouched.  An INTENTIONAL IR change
# (a new optimisation pass, a new ABI decision, an added intrinsic) regenerates
# the golden in the same commit, with the diff justified in the commit body.
#
# Usage:
#   scripts/ll-corpus.sh golden    # (re)capture golden from current build
#   scripts/ll-corpus.sh verify    # compare current build against golden
#
# Env:
#   HEW_BIN  compiler binary (default: target/debug/hew at repo root)
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CORPUS="$ROOT/tests/ll-oracle/corpus"
GOLDEN="$CORPUS/golden"
ORACLE="$ROOT/scripts/ll-byte-identity.sh"
HEW_BIN="${HEW_BIN:-$ROOT/target/debug/hew}"
MODE="${1:-verify}"

TARGETS=("native" "wasm32")

if [[ ! -x "$HEW_BIN" ]]; then
  echo "ll-corpus: compiler binary not found at $HEW_BIN" >&2
  echo "build it first (make hew) or set HEW_BIN=<path>" >&2
  exit 2
fi

# Collect corpus fixtures
fixtures=()
while IFS= read -r f; do
  fixtures+=("$f")
done < <(find "$CORPUS" -maxdepth 1 -name '*.hew' | sort)

if [[ ${#fixtures[@]} -eq 0 ]]; then
  echo "ll-corpus: no .hew fixtures under $CORPUS" >&2
  exit 2
fi

# Compile one fixture to a target-specific directory, return the .ll path.
# compile_fixture <fixture-path> <out-dir> [wasm32]
compile_fixture() {
  local fixture="$1"
  local out="$2"
  local target="${3:-}"
  mkdir -p "$out"
  if [[ "$target" == "wasm32" ]]; then
    "$HEW_BIN" compile "$fixture" --emit-dir "$out" \
      --target wasm32-unknown-unknown 2>&1 || true
  else
    "$HEW_BIN" compile "$fixture" --emit-dir "$out" 2>&1 || true
  fi
}

case "$MODE" in

# --------------------------------------------------------------------------
golden)
  mkdir -p "$GOLDEN/native" "$GOLDEN/wasm32"
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "$tmpdir"' EXIT

  for f in "${fixtures[@]}"; do
    name="$(basename "$f" .hew)"
    for tgt in "${TARGETS[@]}"; do
      outdir="$tmpdir/${name}_${tgt}"
      compile_fixture "$f" "$outdir" "$tgt"
      ll="$outdir/$name.ll"
      if [[ ! -f "$ll" ]]; then
        echo "ll-corpus golden: $name ($tgt) produced no .ll — check compile output above" >&2
        exit 1
      fi
      dest="$GOLDEN/$tgt/$name.ll"
      cp "$ll" "$dest"
      echo "captured $tgt/$name.ll  ($(grep -c "^define " "$dest") fn bodies)"
    done
  done
  echo "ll-corpus golden: captured ${#fixtures[@]} fixtures × ${#TARGETS[@]} targets"
  ;;

# --------------------------------------------------------------------------
verify)
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "$tmpdir"' EXIT
  fail=0

  for tgt in "${TARGETS[@]}"; do
    golden_tgt="$GOLDEN/$tgt"
    if [[ ! -d "$golden_tgt" ]]; then
      echo "ll-corpus verify: no golden for target $tgt (run: make ll-golden)" >&2
      fail=1
      continue
    fi

    head_tgt="$tmpdir/head_$tgt"
    mkdir -p "$head_tgt"

    for f in "${fixtures[@]}"; do
      name="$(basename "$f" .hew)"
      outdir="$tmpdir/${name}_${tgt}"
      compile_fixture "$f" "$outdir" "$tgt"
      ll="$outdir/$name.ll"
      if [[ ! -f "$ll" ]]; then
        echo "ll-corpus verify: $name ($tgt) produced no .ll — check compile output above" >&2
        fail=1
        continue
      fi
      cp "$ll" "$head_tgt/$name.ll"
    done

    echo "--- $tgt ---"
    if ! LL_IDENTITY_VERBOSE=1 bash "$ORACLE" "$golden_tgt" "$head_tgt"; then
      fail=1
    fi
  done

  # Stale goldens (fixture removed but golden still committed) shrink coverage silently.
  for tgt in "${TARGETS[@]}"; do
    for g in "$GOLDEN/$tgt"/*.ll; do
      [[ -e "$g" ]] || continue
      gname="$(basename "$g" .ll)"
      found=0
      for f in "${fixtures[@]}"; do
        [[ "$(basename "$f" .hew)" == "$gname" ]] && found=1 && break
      done
      if [[ $found -eq 0 ]]; then
        echo "STALE GOLDEN: $tgt/$gname.ll has no fixture $gname.hew (remove or regenerate)" >&2
        fail=1
      fi
    done
  done

  if [[ $fail -ne 0 ]]; then
    echo "ll-corpus verify: FAILED" >&2
    exit 1
  fi
  echo "ll-corpus verify: OK (${#fixtures[@]} fixtures × ${#TARGETS[@]} targets byte-identical)"
  ;;

# --------------------------------------------------------------------------
*)
  echo "usage: $0 {golden|verify}" >&2
  exit 2
  ;;
esac
