#!/usr/bin/env python3
"""Grammar-based fuzzing of the Hew frontend.

Uses Grammarinator to generate random programs from docs/specs/Hew.g4 and feeds
them to `hew check` to find panics, crashes, or hangs in the frontend.

Prerequisites:
    pip install grammarinator

Usage:
    ./scripts/fuzz/grammar.py              # generate 100 programs, depth 20
    ./scripts/fuzz/grammar.py -n 500       # 500 programs
    ./scripts/fuzz/grammar.py -d 15        # max depth 15
    ./scripts/fuzz/grammar.py -t 10        # 10s timeout per program
    ./scripts/fuzz/grammar.py --max-size 100000  # allow up to 100KB inputs
    ./scripts/fuzz/grammar.py -k           # keep generated files after run
    ./scripts/fuzz/grammar.py --hew ./target/release/hew   # custom binary
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
from pathlib import Path


def find_repo_root() -> Path:
    """Walk up from the script to find the repo root (contains Cargo.toml)."""
    d = Path(__file__).resolve().parent
    while d != d.parent:
        if (d / "Cargo.toml").exists():
            return d
        d = d.parent
    sys.exit(f"ERROR: Cannot find repo root from {__file__}")


def check_deps(hew: Path, grammar: Path) -> None:
    if shutil.which("grammarinator-process") is None:
        sys.exit("ERROR: grammarinator not found. Install with: pip install grammarinator")
    if not hew.exists():
        sys.exit(f"ERROR: hew binary not found at {hew}\n  Build it with: cargo build -p hew-cli")
    if not grammar.exists():
        sys.exit(f"ERROR: Grammar not found at {grammar}")


def process_grammar(grammar: Path, workdir: Path) -> None:
    """Patch grammar (rename `lambda` to avoid Python keyword) and process."""
    patched = workdir / "Hew.g4"
    text = grammar.read_text()
    # `lambda` is a Python reserved keyword; rename the grammar rule
    text = re.sub(r"\blambda\b", "lambdaExpr", text)
    patched.write_text(text)

    subprocess.run(
        ["grammarinator-process", str(patched), "-o", str(workdir) + "/", "--no-actions"],
        capture_output=True,
        check=True,
    )


def write_serializer(workdir: Path) -> None:
    """Create the custom serializer module."""
    content = r'''"""Custom Grammarinator serializer for Hew — inserts whitespace at word boundaries."""

from grammarinator.runtime import Rule


def hew_serializer(root: Rule) -> str:
    """Serialize with whitespace between word-like tokens.

    Inserts a space when the previous token ends with [a-zA-Z0-9_]
    and the next token starts with [a-zA-Z0-9_]. This prevents keyword
    merging (e.g., 'pubfn' -> 'pub fn') while keeping punctuation tight.
    """
    tokens = list(root.tokens())
    if not tokens:
        return ""

    def is_word_end(t: str) -> bool:
        return bool(t) and (t[-1].isalnum() or t[-1] == '_')

    def is_word_start(t: str) -> bool:
        return bool(t) and (t[0].isalnum() or t[0] == '_')

    parts = [tokens[0]]
    for i in range(1, len(tokens)):
        prev = tokens[i - 1]
        cur = tokens[i]
        if is_word_end(prev) and is_word_start(cur):
            parts.append(" ")
        elif prev.endswith(">") and cur.startswith(">"):
            # Prevent '>>' from fusing into a single GreaterGreater token
            parts.append(" ")
        elif prev.endswith(">") and cur.startswith("="):
            # Prevent '>=' from fusing into GreaterEqual after closing '>'
            parts.append(" ")
        parts.append(cur)

    return "".join(parts)
'''
    (workdir / "hew_serializer.py").write_text(content)


def generate_programs(workdir: Path, outdir: Path, num: int, depth: int) -> int:
    """Generate random programs and return the count."""
    outdir.mkdir(exist_ok=True)
    subprocess.run(
        [
            "grammarinator-generate",
            "HewGenerator.HewGenerator",
            "-r", "program",
            "-o", str(outdir / "%d.hew"),
            "-n", str(num),
            "-d", str(depth),
            "-s", "hew_serializer.hew_serializer",
            "--sys-path", str(workdir),
        ],
        capture_output=True,
        check=True,
    )
    return len(list(outdir.glob("*.hew")))


def test_program(hew: str, path: Path, timeout: int) -> tuple[str, int, str]:
    """Run `hew check` on a single file.

    Returns (category, exit_code, output) where category is one of:
    'pass', 'parse_error', 'type_error', 'crash', 'hang', 'ice'.
    """
    try:
        result = subprocess.run(
            [hew, "check", str(path)],
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        code = result.returncode
        output = (result.stdout + result.stderr).strip()

        # Check for signals (negative returncode in Python = killed by signal)
        if code < 0:
            sig = -code
            return "crash", code, f"killed by signal {sig} ({signal.Signals(sig).name})"

        # Non-standard exit codes (not 0 or 1) indicate crashes
        if code not in (0, 1):
            return "crash", code, output

        # ICE detection in output
        ice_patterns = ["internal compiler error", "panicked", "thread '", "RUST_BACKTRACE"]
        if any(p in output.lower() for p in ice_patterns):
            return "ice", code, output

        if "parsing failed" in output or "parse error" in output or "unexpected token" in output:
            return "parse_error", code, output

        if output.endswith(": OK"):
            return "pass", code, output

        # Type errors or other graceful diagnostics
        return "type_error", code, output

    except subprocess.TimeoutExpired:
        return "hang", 124, f"timed out after {timeout}s"


def main() -> None:
    repo = find_repo_root()

    parser = argparse.ArgumentParser(description="Grammar-based fuzzing of the Hew frontend")
    parser.add_argument("-n", type=int, default=100, help="Number of programs to generate (default: 100)")
    parser.add_argument("-d", type=int, default=20, help="Max derivation depth (default: 20)")
    parser.add_argument("-t", type=int, default=5, help="Timeout per program in seconds (default: 5)")
    parser.add_argument("--max-size", type=int, default=50000,
                        help="Skip files larger than this (bytes, default: 50000)")
    parser.add_argument("-k", action="store_true", help="Keep generated files after run")
    parser.add_argument("--hew", type=str, default=str(repo / "target/debug/hew"), help="Path to hew binary")
    args = parser.parse_args()

    grammar = repo / "docs" / "Hew.g4"
    hew = Path(args.hew)
    check_deps(hew, grammar)

    workdir = Path(tempfile.mkdtemp(prefix="hew-fuzz-grammar."))

    print("=== Hew Grammar Fuzzer ===")
    print(f"Grammar:  {grammar}")
    print(f"Binary:   {hew}")
    print(f"Programs: {args.n} (depth {args.d}, timeout {args.t}s)")
    print(f"Workdir:  {workdir}")
    print()

    # Step 1: Process grammar
    print("[1/3] Processing grammar...")
    process_grammar(grammar, workdir)
    write_serializer(workdir)
    print("      Generator ready.")

    # Step 2: Generate programs
    outdir = workdir / "out"
    print(f"[2/3] Generating {args.n} programs (depth {args.d})...")
    count = generate_programs(workdir, outdir, args.n, args.d)
    print(f"      Generated {count} files.")

    # Step 3: Test against frontend
    print("[3/3] Testing against hew check...")
    print()

    crash_dir = workdir / "crashes"
    hang_dir = workdir / "hangs"
    ice_dir = workdir / "ice"
    crash_dir.mkdir(exist_ok=True)
    hang_dir.mkdir(exist_ok=True)
    ice_dir.mkdir(exist_ok=True)

    stats: dict[str, int] = {"pass": 0, "parse_error": 0, "type_error": 0, "crash": 0, "hang": 0, "ice": 0}
    skipped = 0
    total = 0
    problems = False

    files = sorted(outdir.glob("*.hew"), key=lambda p: int(p.stem))
    for f in files:
        total += 1
        size = f.stat().st_size

        if size > args.max_size:
            skipped += 1
            continue

        category, code, output = test_program(str(hew), f, args.t)
        stats[category] += 1

        if category == "crash":
            shutil.copy2(f, crash_dir)
            print(f"  CRASH {f.name} ({size}B, exit {code})")
            for line in output.split("\n")[:3]:
                print(f"        {line}")
            problems = True
        elif category == "ice":
            shutil.copy2(f, ice_dir)
            print(f"  ICE   {f.name} ({size}B)")
            for line in output.split("\n")[:3]:
                print(f"        {line}")
            problems = True
        elif category == "hang":
            shutil.copy2(f, hang_dir)
            print(f"  HANG  {f.name} ({size}B, {output})")

        if total % 50 == 0:
            print(f"  ... {total}/{len(files)} processed")

    # Summary
    print()
    print("=== Results ===")
    print(f"  Total:       {total}")
    print(f"  Skipped:     {skipped} (over {args.max_size}B)")
    print(f"  Tested:      {total - skipped}")
    print(f"  Parsed OK:   {stats['pass']}")
    print(f"  Parse error: {stats['parse_error']} (expected — grammar is a superset)")
    print(f"  Type error:  {stats['type_error']} (graceful diagnostic)")
    print(f"  Hangs:       {stats['hang']}")
    print(f"  Crashes:     {stats['crash']}")
    print(f"  ICEs:        {stats['ice']}")

    if problems or stats["hang"] > 0:
        print()
        print("=== Problem files saved ===")
        if stats["crash"] > 0:
            print(f"  Crashes: {crash_dir}/")
        if stats["ice"] > 0:
            print(f"  ICEs:    {ice_dir}/")
        if stats["hang"] > 0:
            print(f"  Hangs:   {hang_dir}/")
        args.k = True  # Keep workdir if there are problems

    if not args.k:
        shutil.rmtree(workdir)

    if stats["crash"] > 0 or stats["ice"] > 0:
        print()
        sys.exit(1)

    print()
    print("No crashes or ICEs found.")


if __name__ == "__main__":
    main()
