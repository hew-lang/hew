#!/usr/bin/env python3
"""Verify hew fmt is stable under the current v0.5 CLI.

For every selected .hew file that passes `hew check` before formatting:
  1. Format the source through `hew fmt --stdin` twice from the same input.
  2. Format the formatted output again and require byte-for-byte idempotence.
  3. Write the formatted output to a scratch repository mirror and run
     `hew check` on that scratch copy.

This intentionally does not mutate tracked source files. The older verifier used
`hew build --emit-ast` to compare ASTs before/after formatting; that producer no
longer exists in the v0.5 CLI. Full AST-equivalence verification is deferred
until a current dump API is available again.
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


def run(
    cmd: list[str], *, cwd: Path, input_text: str | None = None
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        cmd,
        cwd=cwd,
        input=input_text,
        capture_output=True,
        text=True,
        check=False,
    )


def git_lines(repo_root: Path, args: list[str]) -> list[str]:
    result = run(["git", *args], cwd=repo_root)
    if result.returncode != 0:
        raise RuntimeError(result.stderr.strip() or result.stdout.strip())
    return [line for line in result.stdout.splitlines() if line]


def repo_root() -> Path:
    script_dir = Path(__file__).resolve().parent
    return Path(git_lines(script_dir, ["rev-parse", "--show-toplevel"])[0])


def resolve_hew(repo: Path, requested: str | None) -> str:
    candidates: list[Path | str] = []
    if requested:
        candidates.append(Path(requested))
    env_hew = os.environ.get("HEW")
    if env_hew:
        candidates.append(Path(env_hew))
    candidates.extend(
        [
            repo / "build" / "bin" / "hew",
            repo / "target" / "debug" / "hew",
            repo / "target" / "release" / "hew",
        ]
    )
    path_hew = shutil.which("hew")
    if path_hew:
        candidates.append(path_hew)

    for candidate in candidates:
        if isinstance(candidate, Path):
            if candidate.is_file() and candidate.exists():
                return str(candidate)
        elif candidate:
            return candidate

    raise RuntimeError("cannot find hew binary; pass --hew or set HEW")


def rel_path(repo: Path, path: str) -> Path:
    p = Path(path)
    if p.is_absolute():
        return p.resolve().relative_to(repo.resolve())
    return p


def selected_hew_files(repo: Path, explicit_files: list[str]) -> list[Path]:
    if explicit_files:
        files = [rel_path(repo, path) for path in explicit_files]
    else:
        files = [Path(line) for line in git_lines(repo, ["ls-files", "*.hew"])]
    return sorted(dict.fromkeys(path for path in files if path.suffix == ".hew"))


def copy_support_tree(
    repo: Path, scratch_repo: Path, tracked_hew_files: list[Path]
) -> None:
    for rel in tracked_hew_files:
        src = repo / rel
        dst = scratch_repo / rel
        dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src, dst)


def format_stdin(hew: str, repo: Path, source: str) -> subprocess.CompletedProcess[str]:
    return run([hew, "fmt", "--stdin"], cwd=repo, input_text=source)


def verify_file(
    hew: str, repo: Path, scratch_repo: Path, rel: Path
) -> tuple[str, str | None]:
    original = repo / rel
    scratch = scratch_repo / rel

    precheck = run([hew, "check", str(original)], cwd=repo)
    if precheck.returncode != 0:
        return "skipped", "pre-format hew check failed"

    source = original.read_text()
    first = format_stdin(hew, repo, source)
    if first.returncode != 0:
        return "failed", f"hew fmt --stdin failed: {first.stderr.strip()}"

    stability = format_stdin(hew, repo, source)
    if stability.returncode != 0:
        return "failed", f"second hew fmt --stdin failed: {stability.stderr.strip()}"
    if stability.stdout != first.stdout:
        return "failed", "hew fmt --stdin is not stable across identical runs"

    idempotence = format_stdin(hew, repo, first.stdout)
    if idempotence.returncode != 0:
        return (
            "failed",
            f"hew fmt --stdin failed on formatted output: {idempotence.stderr.strip()}",
        )
    if idempotence.stdout != first.stdout:
        return "failed", "hew fmt is not idempotent for this file"

    scratch.parent.mkdir(parents=True, exist_ok=True)
    scratch.write_text(first.stdout)
    postcheck = run([hew, "check", str(scratch)], cwd=scratch_repo)
    if postcheck.returncode != 0:
        detail = postcheck.stderr.strip() or postcheck.stdout.strip()
        return "failed", f"formatted scratch copy failed hew check: {detail}"

    return "passed", None


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "files",
        nargs="*",
        help="Specific .hew files to verify; defaults to git ls-files '*.hew'.",
    )
    parser.add_argument(
        "--hew",
        default=None,
        help="Path to the hew binary (defaults to HEW, build/bin/hew, target/debug/hew, target/release/hew, then PATH).",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo = repo_root()
    try:
        hew = resolve_hew(repo, args.hew)
    except RuntimeError as error:
        print(f"error: {error}", file=sys.stderr)
        return 2

    files = selected_hew_files(repo, args.files)
    tracked_hew_files = [Path(line) for line in git_lines(repo, ["ls-files", "*.hew"])]

    total = len(files)
    counts = {"passed": 0, "skipped": 0, "failed": 0}
    failures: list[tuple[Path, str]] = []

    with tempfile.TemporaryDirectory(prefix="hew-fmt-verify-") as tmp:
        scratch_repo = Path(tmp) / "repo"
        copy_support_tree(repo, scratch_repo, tracked_hew_files)

        for rel in files:
            status, detail = verify_file(hew, repo, scratch_repo, rel)
            counts[status] += 1
            if status == "failed" and detail is not None:
                failures.append((rel, detail))

    print(
        "hew fmt verification: "
        f"{total} files, {counts['skipped']} skipped (pre-existing check failures), "
        f"{counts['passed']} passed, {counts['failed']} failed"
    )
    print(
        "note: AST-equivalence checking is deferred until a current v0.5 dump API exists"
    )

    if failures:
        print("\nFAILURES")
        for rel, detail in failures:
            print(f"\n--- {rel} ---")
            print(detail)

    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
