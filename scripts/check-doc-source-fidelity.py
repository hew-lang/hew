#!/usr/bin/env python3
"""Prove a documentation candidate changes only stdlib doc comments."""

from __future__ import annotations

import argparse
import subprocess
from pathlib import Path


def git(*args: str) -> str:
    return subprocess.check_output(["git", *args], text=True)


def without_doc_comments(source: str) -> str:
    return "".join(
        line
        for line in source.splitlines(keepends=True)
        if not line.lstrip().startswith(("///", "//!"))
    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stable-ref", required=True)
    parser.add_argument("--candidate-std", type=Path, required=True)
    args = parser.parse_args()

    stable_commit = git("rev-parse", "--verify", f"{args.stable_ref}^{{commit}}").strip()
    stable_files = {
        line
        for line in git("ls-tree", "-r", "--name-only", stable_commit, "--", "std")
        .splitlines()
        if line.endswith(".hew")
    }
    candidate_files = {
        f"std/{path.relative_to(args.candidate_std).as_posix()}"
        for path in args.candidate_std.rglob("*.hew")
    }
    if stable_files != candidate_files:
        added = sorted(candidate_files - stable_files)
        removed = sorted(stable_files - candidate_files)
        raise SystemExit(
            "standard-library module set differs from "
            f"{args.stable_ref}: added={added}, removed={removed}"
        )

    changed_code: list[str] = []
    for name in sorted(stable_files):
        stable_source = git("show", f"{stable_commit}:{name}")
        candidate_source = (args.candidate_std / name.removeprefix("std/")).read_text()
        if without_doc_comments(stable_source) != without_doc_comments(
            candidate_source
        ):
            changed_code.append(name)
    if changed_code:
        raise SystemExit(
            "candidate changes non-documentation stdlib source relative to "
            f"{args.stable_ref}: {', '.join(changed_code)}"
        )

    print(
        f"Verified {len(stable_files)} stdlib sources against "
        f"{args.stable_ref} ({stable_commit}): only //! and /// comments may differ"
    )


if __name__ == "__main__":
    main()
