#!/usr/bin/env python3
"""Analyze grammar fuzzer failures by category with sample programs."""

import subprocess
import os
import re
import sys
from pathlib import Path
from collections import Counter, defaultdict

def find_fuzz_dir():
    """Find the most recent fuzz output directory."""
    import glob
    dirs = sorted(glob.glob("/tmp/hew-fuzz-grammar.*"), key=os.path.getmtime, reverse=True)
    if not dirs:
        print("No fuzz directory found. Run scripts/fuzz/grammar.py -k first.")
        sys.exit(1)
    return Path(dirs[0]) / "out"

def strip_ansi(text):
    return re.sub(r'\x1b\[[0-9;]*m', '', text)

def check_file(hew_bin, filepath):
    """Run hew check on a file and return (category, first_error_msg, raw_output)."""
    env = os.environ.copy()
    env['NO_COLOR'] = '1'
    env['TERM'] = 'dumb'
    try:
        result = subprocess.run(
            [hew_bin, 'check', str(filepath)],
            capture_output=True, text=True, timeout=5, env=env
        )
    except subprocess.TimeoutExpired:
        return 'hang', 'timeout', ''

    output = strip_ansi((result.stdout + result.stderr).strip())

    if output.endswith(': OK'):
        return 'pass', '', output

    if 'parsing failed' in output or 'parse error' in output or 'unexpected token' in output:
        m = re.search(r'error: (.+)', output)
        if m:
            msg = m.group(1).strip()
            msg = re.sub(r"Some\(\w+\([^)]*\)\)", 'TOKEN', msg)
            msg = re.sub(r"Some\((\w+)\)", r'\1', msg)
            return 'parse_error', msg, output
        return 'parse_error', 'UNKNOWN', output

    return 'type_error', '', output

def main():
    fuzzdir = find_fuzz_dir()
    hew = './target/debug/hew'
    samples = int(sys.argv[1]) if len(sys.argv) > 1 else 3

    errors = Counter()
    examples = defaultdict(list)  # error_msg -> [(filename, source_snippet)]

    total = pass_count = type_err = 0

    for f in sorted(fuzzdir.glob('*.hew')):
        total += 1
        cat, msg, output = check_file(hew, f)

        if cat == 'pass':
            pass_count += 1
        elif cat == 'type_error':
            type_err += 1
        elif cat == 'parse_error':
            errors[msg] += 1
            if len(examples[msg]) < samples:
                try:
                    src = f.read_text()[:200]
                except:
                    src = '<unreadable>'
                examples[msg].append((f.name, src))

    print(f"Total: {total}  Pass: {pass_count} ({100*pass_count//total}%)  "
          f"Type err: {type_err}  Parse err: {sum(errors.values())}")
    print()

    for msg, cnt in errors.most_common(20):
        print(f"--- {cnt:4d}x  {msg} ---")
        for name, src in examples[msg]:
            print(f"  [{name}] {src[:120]}")
        print()

if __name__ == '__main__':
    main()
