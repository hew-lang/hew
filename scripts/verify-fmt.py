#!/usr/bin/env python3
"""Verify hew fmt preserves program semantics.

For every .hew file that passes `hew check`:
  1. Emit the enriched AST from the original
  2. Format a copy with `hew fmt`
  3. Emit the enriched AST from the formatted copy
  4. Compare the two ASTs, ignoring span byte offsets

Any structural difference means the formatter changed semantics.
"""

import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


def strip_spans(obj):
    """Recursively remove span/position fields (byte offsets change after formatting)."""
    if isinstance(obj, dict):
        return {
            k: strip_spans(v)
            for k, v in obj.items()
            if k not in ("span", "start", "end", "source_paths", "source_path", "path")
        }
    elif isinstance(obj, list):
        return [strip_spans(v) for v in obj]
    return obj


def emit_ast(filepath):
    """Run hew build --emit-ast and return parsed JSON, or None on failure."""
    result = subprocess.run(
        ["hew", "build", "--emit-ast", str(filepath), "-o", "/dev/null"],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return None
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError:
        return None


def diff_ast(before, after):
    """Return a list of (path, before_val, after_val) differences."""
    diffs = []

    def walk(a, b, path=""):
        if type(a) != type(b):
            diffs.append((path, type(a).__name__, type(b).__name__))
            return
        if isinstance(a, dict):
            all_keys = sorted(set(list(a.keys()) + list(b.keys())))
            for k in all_keys:
                if k not in a:
                    diffs.append((f"{path}.{k}", "<missing>", repr(b[k])[:80]))
                elif k not in b:
                    diffs.append((f"{path}.{k}", repr(a[k])[:80], "<missing>"))
                else:
                    walk(a[k], b[k], f"{path}.{k}")
        elif isinstance(a, list):
            if len(a) != len(b):
                diffs.append((f"{path}[]", f"len={len(a)}", f"len={len(b)}"))
                # Still compare up to min length
            for i in range(min(len(a), len(b))):
                walk(a[i], b[i], f"{path}[{i}]")
        elif a != b:
            diffs.append((path, repr(a)[:80], repr(b)[:80]))

    walk(before, after)
    return diffs


def main():
    repo_root = (
        subprocess.check_output(["git", "rev-parse", "--show-toplevel"])
        .decode()
        .strip()
    )

    hew_files = (
        subprocess.check_output(["git", "ls-files", "*.hew"], cwd=repo_root)
        .decode()
        .strip()
        .split("\n")
    )

    tmpdir = tempfile.mkdtemp(prefix="hew-fmt-verify-")
    total = 0
    skipped = 0
    passed = 0
    failed = 0
    failures = []

    try:
        for rel_path in hew_files:
            if not rel_path:
                continue
            filepath = Path(repo_root) / rel_path
            total += 1

            # Step 1: Emit AST from original
            before_ast = emit_ast(filepath)
            if before_ast is None:
                skipped += 1
                continue

            # Step 2: Copy and format
            tmp_file = Path(tmpdir) / rel_path
            tmp_file.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(filepath, tmp_file)

            fmt_result = subprocess.run(
                ["hew", "fmt", str(tmp_file)],
                capture_output=True,
                text=True,
            )
            if fmt_result.returncode != 0:
                failures.append((rel_path, f"hew fmt failed: {fmt_result.stderr.strip()}"))
                failed += 1
                continue

            # Step 3: Emit AST from formatted
            after_ast = emit_ast(tmp_file)
            if after_ast is None:
                failures.append((rel_path, "hew build --emit-ast failed after formatting"))
                failed += 1
                continue

            # Step 4: Compare (ignoring spans)
            before_clean = strip_spans(before_ast)
            after_clean = strip_spans(after_ast)

            diffs = diff_ast(before_clean, after_clean)
            if diffs:
                failed += 1
                failures.append((rel_path, diffs))
            else:
                passed += 1

        # Report
        print(f"\nhew fmt verification: {total} files, {skipped} skipped (pre-existing errors), {passed} passed, {failed} failed\n")

        if failures:
            print("=" * 70)
            print("FAILURES")
            print("=" * 70)
            for rel_path, detail in failures:
                print(f"\n--- {rel_path} ---")
                if isinstance(detail, str):
                    print(f"  {detail}")
                else:
                    for path, before, after in detail[:10]:
                        print(f"  {path}")
                        print(f"    before: {before}")
                        print(f"    after:  {after}")
                    if len(detail) > 10:
                        print(f"  ... and {len(detail) - 10} more differences")

        sys.exit(1 if failed > 0 else 0)

    finally:
        shutil.rmtree(tmpdir, ignore_errors=True)


if __name__ == "__main__":
    main()
