#!/usr/bin/env python3
"""Run the core-matrix cell corpus and gate it against the recorded truth table.

Every cell under tests/core-matrix/cells/ is a complete Hew program whose
stdout is an exact oracle: the value it computes, and -- where the row carries
a `#[resource]` whose `close` prints -- the exactly-once release trace.

tests/core-matrix/matrix.tsv is the truth table. Each row records the outcome
class the cell has TODAY:

    PASS            ran and produced exactly the expected output
    WRONG-ANSWER    ran and produced incorrect output
    SILENT-UNSOUND  released zero or two times with no diagnostic
    NYI-CLEAN       rejected with a user-facing message naming the limit
    NYI-INTERNAL    rejected with compiler internals in the message
    CRASH           aborted, trapped, hung, or failed to link

The gate fails on ANY drift in either direction: a PASS cell that stops
passing is a regression, and a non-PASS cell that starts passing means the
table is stale and must be updated (that is the ratchet).

Usage:
    python3 scripts/core-matrix.py            # gate against matrix.tsv
    python3 scripts/core-matrix.py --record   # rewrite matrix.tsv from a run
"""

from __future__ import annotations

import argparse
import concurrent.futures
import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, "scripts", "lib"))
from corpus_floor import assert_floor  # noqa: E402

CELLS = os.path.join(ROOT, "tests", "core-matrix", "cells")
MATRIX = os.path.join(ROOT, "tests", "core-matrix", "matrix.tsv")
NA = os.path.join(ROOT, "tests", "core-matrix", "na.tsv")
HEW = os.environ.get("HEW_BIN", os.path.join(ROOT, "target", "debug", "hew"))
TIMEOUT = int(os.environ.get("CORE_MATRIX_TIMEOUT", "120"))

# Compiler-internal vocabulary. A rejection quoting any of this is telling the
# user about the compiler's insides instead of about their program.
INTERNAL = re.compile(
    r"E_NOT_YET_IMPLEMENTED|E_CODEGEN_FRONT|E_INTERNAL|fail-closed|"
    r"no rewrite entry|llvm_type|StructType|LLVMType|MirType|HirType|"
    r"\bMIR\b|\bHIR\b|internal compiler|panicked at|unreachable!|"
    r"not yet implemented|todo!",
)


def classify(name):
    """Run one cell and return (class, detail)."""
    src = os.path.join(CELLS, name + ".hew")
    exp_path = os.path.join(CELLS, name + ".expected")
    with open(exp_path) as f:
        expected = f.read()
    # Diagnostics echo the path they were given. Passing a REPO-RELATIVE path
    # (the child already runs with cwd=ROOT) keeps the recorded `detail` column
    # identical across checkouts; an absolute path would rewrite every
    # rejected row's detail on a re-record from a different worktree, burying
    # the actual class change in whole-file churn.
    rel_src = os.path.relpath(src, ROOT)
    try:
        p = subprocess.run(
            [HEW, "run", rel_src],
            capture_output=True,
            timeout=TIMEOUT,
            cwd=ROOT,
        )
        # Cells have produced non-UTF-8 stdout (raw bytes escaping a value
        # channel). Decode lossily so the runner classifies it as a wrong
        # answer instead of dying.
        p_stdout = p.stdout.decode("utf-8", "replace")
        p_stderr = p.stderr.decode("utf-8", "replace")
        try:
            p.stdout.decode("utf-8")
        except UnicodeDecodeError:
            # A Hew `string` is UTF-8 by construction. Non-UTF-8 on stdout is
            # raw memory reaching a value channel -- the worst outcome class,
            # regardless of the exit status that follows.
            return (
                "WRONG-ANSWER",
                "non-UTF-8 stdout: raw memory reached a value channel",
            )
    except subprocess.TimeoutExpired:
        return "CRASH", "timeout"
    out, err, rc = p_stdout, p_stderr, p.returncode
    # A `.trap` cell aborts by design (out-of-bounds index). Its oracle is the
    # output produced BEFORE the trap, which is where the trap-path cleanup
    # semantic is pinned.
    trapped = (
        os.path.exists(os.path.join(CELLS, name + ".trap"))
        and rc != 0
        and ("trap in main context" in (err + out) or "Abort" in (err + out))
    )
    if rc == 0 or trapped:
        if out == expected:
            return "PASS", ""
        # Release order among independent owners is unspecified (a frame drops
        # LIFO, a container in insertion order). The oracle therefore compares
        # the VALUE lines in order and the `close` trace as a multiset: what
        # must hold is exactly-once per constructed resource, not a sequence.
        got_v, got_c = split_trace(out)
        want_v, want_c = split_trace(expected)
        if got_v == want_v and sorted(got_c) == sorted(want_c):
            return "PASS", ""
        if want_c and sorted(got_c) != sorted(want_c):
            return "SILENT-UNSOUND", f"released {got_c or '[]'}, expected {want_c}"
        return "WRONG-ANSWER", first_diff("\n".join(want_v), "\n".join(got_v))
    blob = err + out
    if rc < 0 or rc >= 128 or "Abort trap" in blob or "signal" in blob.lower():
        return "CRASH", firstline(blob)
    if INTERNAL.search(blob):
        return "NYI-INTERNAL", firstline(blob)
    if "error:" in blob:
        return "NYI-CLEAN", firstline(blob)
    return "CRASH", firstline(blob) or f"exit {rc}"


def split_trace(text):
    """Split output into (value lines in order, release trace)."""
    values, closes = [], []
    for line in text.splitlines():
        (closes if line.startswith("close ") else values).append(line)
    return values, closes


def firstline(blob):
    for line in blob.splitlines():
        line = line.strip()
        if "trap in main context" in line:
            return line[:220]
    for line in blob.splitlines():
        line = line.strip()
        if line.startswith("error:") or ": error:" in line:
            return line[:220]
    for line in blob.splitlines():
        line = line.strip()
        if "E_NOT_YET_IMPLEMENTED" in line or "E_" in line:
            return line[:220]
    for line in blob.splitlines():
        if line.strip():
            return line.strip()[:220]
    return ""


def first_diff(expected, got):
    e = expected.splitlines()
    g = got.splitlines()
    for i in range(max(len(e), len(g))):
        ev = e[i] if i < len(e) else "<missing>"
        gv = g[i] if i < len(g) else "<missing>"
        if ev != gv:
            return f"line {i + 1}: expected {ev!r}, got {gv!r}"
    return "output differs"


def cell_names():
    return sorted(f[:-4] for f in os.listdir(CELLS) if f.endswith(".hew"))


def run_all():
    names = cell_names()
    results = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=8) as ex:
        for name, res in zip(names, ex.map(classify, names)):
            results[name] = res
    return results


def read_matrix():
    table = {}
    if not os.path.exists(MATRIX):
        return table
    with open(MATRIX) as f:
        for line in f:
            line = line.rstrip("\n")
            if not line or line.startswith("#"):
                continue
            parts = line.split("\t")
            table[parts[0]] = parts[1]
    return table


def write_matrix(results):
    with open(MATRIX, "w") as f:
        f.write("# cell\toutcome\tdetail\n")
        f.write("# Generated by scripts/core-matrix.py --record.\n")
        for name in sorted(results):
            klass, detail = results[name]
            f.write(f"{name}\t{klass}\t{detail}\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--record", action="store_true")
    args = ap.parse_args()

    if not os.path.exists(HEW):
        print(f"error: hew binary not found at {HEW}", file=sys.stderr)
        return 1
    if not os.path.isdir(CELLS):
        print(f"error: cell corpus not found at {CELLS}", file=sys.stderr)
        return 1

    names = cell_names()
    if not names:
        print(
            "error: cell corpus is empty; refusing a vacuous verdict", file=sys.stderr
        )
        return 1
    # A matrix that silently shrinks is the same defect as a matrix that never
    # ran: assert the enumeration size before believing any verdict over it.
    assert_floor("core-matrix-cells", len(names), context="tests/core-matrix/cells")
    results = run_all()

    if args.record:
        write_matrix(results)
        counts = {}
        for klass, _ in results.values():
            counts[klass] = counts.get(klass, 0) + 1
        for k in sorted(counts):
            print(f"{k}\t{counts[k]}")
        print(f"TOTAL\t{len(results)}")
        return 0

    table = read_matrix()
    if not table:
        print(f"error: {MATRIX} is empty or missing", file=sys.stderr)
        return 1

    regressions, fixes, missing = [], [], []
    for name, (klass, detail) in sorted(results.items()):
        want = table.get(name)
        if want is None:
            missing.append(name)
        elif want != klass:
            (fixes if want != "PASS" and klass == "PASS" else regressions).append(
                f"{name}: recorded {want}, observed {klass} ({detail})"
            )
    stale = sorted(set(table) - set(results))

    ok = True
    if missing:
        ok = False
        print("cells with no truth-table row (run --record):", file=sys.stderr)
        for n in missing:
            print(f"  {n}", file=sys.stderr)
    if stale:
        ok = False
        print("truth-table rows with no cell:", file=sys.stderr)
        for n in stale:
            print(f"  {n}", file=sys.stderr)
    if regressions:
        ok = False
        print("REGRESSIONS:", file=sys.stderr)
        for r in regressions:
            print(f"  {r}", file=sys.stderr)
    if fixes:
        ok = False
        print(
            "cells that now PASS -- update the truth table with --record:",
            file=sys.stderr,
        )
        for r in fixes:
            print(f"  {r}", file=sys.stderr)

    counts = {}
    for klass, _ in results.values():
        counts[klass] = counts.get(klass, 0) + 1
    print("core-matrix: " + " ".join(f"{k}={counts[k]}" for k in sorted(counts)))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
