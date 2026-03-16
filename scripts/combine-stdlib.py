#!/usr/bin/env python3
"""Combine per-crate stdlib archives into one shipped stdlib archive."""

from __future__ import annotations

import argparse
import hashlib
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


def find_ar_tool() -> str:
    candidates = [os.environ.get("AR"), "llvm-ar", "ar"]
    for candidate in candidates:
        if candidate and shutil.which(candidate):
            return candidate
    raise SystemExit("error: cannot find ar/llvm-ar; set AR or install LLVM binutils")


def find_nm_tool() -> str:
    candidates = [os.environ.get("NM"), "llvm-nm", "nm"]
    for candidate in candidates:
        if candidate and shutil.which(candidate):
            return candidate
    raise SystemExit("error: cannot find nm/llvm-nm; set NM or install LLVM binutils")


def find_objcopy_tool() -> str:
    candidates = [os.environ.get("OBJCOPY"), "llvm-objcopy", "objcopy"]
    for candidate in candidates:
        if candidate and shutil.which(candidate):
            return candidate
    raise SystemExit(
        "error: cannot find objcopy/llvm-objcopy; set OBJCOPY or install LLVM binutils"
    )


def list_members(ar_tool: str, archive: Path) -> list[str]:
    output = subprocess.check_output(
        [ar_tool, "t", str(archive)],
        text=True,
        stderr=subprocess.PIPE,
    )
    return [line.strip() for line in output.splitlines() if line.strip()]


def read_member(ar_tool: str, archive: Path, member: str) -> bytes:
    return subprocess.check_output(
        [ar_tool, "p", str(archive), member],
        stderr=subprocess.PIPE,
    )


def defined_global_symbols(nm_tool: str, path: Path) -> list[str]:
    output = subprocess.check_output(
        [nm_tool, "-g", "--defined-only", str(path)],
        text=True,
        stderr=subprocess.PIPE,
        errors="ignore",
    )
    symbols: list[str] = []
    for line in output.splitlines():
        line = line.strip()
        if not line or line.endswith(":"):
            continue
        parts = line.split()
        if len(parts) >= 2:
            symbols.append(parts[-1])
    return symbols


def localize_symbols(objcopy_tool: str, path: Path, symbols: list[str]) -> None:
    command = [objcopy_tool]
    for symbol in symbols:
        command.append(f"--localize-symbol={symbol}")
    command.append(str(path))
    subprocess.run(command, check=True)


def runtime_archive(release_dir: Path) -> Path:
    for name in ("libhew_runtime.a", "hew_runtime.lib"):
        candidate = release_dir / name
        if candidate.exists():
            return candidate
    raise SystemExit(f"error: cannot find libhew_runtime.a or hew_runtime.lib in {release_dir}")


def stdlib_archives(release_dir: Path) -> list[Path]:
    archives = sorted(release_dir.glob("libhew_std_*.a"))
    archives.extend(sorted(release_dir.glob("hew_std_*.lib")))
    if not archives:
        raise SystemExit(f"error: cannot find any stdlib archives in {release_dir}")
    return archives


def default_output_name(runtime: Path) -> str:
    return "hew_stdlib.lib" if runtime.suffix == ".lib" else "libhew_stdlib.a"


def safe_member_name(member: str) -> str:
    return member.replace("/", "_").replace("\\", "_")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--release-dir",
        default="target/release",
        help="Directory containing libhew_runtime.a and libhew_std_* archives",
    )
    parser.add_argument(
        "--output",
        help="Path for the combined stdlib archive (defaults to <release-dir>/libhew_stdlib.a or hew_stdlib.lib)",
    )
    args = parser.parse_args()

    release_dir = Path(args.release_dir).resolve()
    ar_tool = find_ar_tool()
    nm_tool = find_nm_tool()
    objcopy_tool = find_objcopy_tool()
    runtime = runtime_archive(release_dir)
    archives = stdlib_archives(release_dir)
    output = Path(args.output).resolve() if args.output else release_dir / default_output_name(runtime)

    runtime_members = set(list_members(ar_tool, runtime))
    runtime_symbols = set(defined_global_symbols(nm_tool, runtime))
    seen_members: dict[str, bytes] = {}
    kept_files: list[Path] = []
    skipped_runtime = 0
    skipped_duplicates = 0
    localized_objects = 0

    with tempfile.TemporaryDirectory(prefix="hew-stdlib-") as tmpdir:
        temp_dir = Path(tmpdir)

        for archive_index, archive in enumerate(archives):
            for member in list_members(ar_tool, archive):
                if member in runtime_members:
                    skipped_runtime += 1
                    continue

                data = read_member(ar_tool, archive, member)
                digest = hashlib.sha256(data).digest()
                existing_digest = seen_members.get(member)
                if existing_digest is not None:
                    if existing_digest != digest:
                        print(
                            f"error: archive member {member} differs across stdlib archives",
                            file=sys.stderr,
                        )
                        return 1
                    skipped_duplicates += 1
                    continue

                seen_members[member] = digest
                target = temp_dir / f"{archive_index:02d}-{safe_member_name(member)}"
                target.write_bytes(data)

                member_symbols = defined_global_symbols(nm_tool, target)
                duplicate_symbols = [
                    symbol for symbol in member_symbols if symbol in runtime_symbols
                ]
                if member_symbols and len(duplicate_symbols) == len(member_symbols):
                    target.unlink()
                    skipped_runtime += 1
                    continue
                if duplicate_symbols:
                    localize_symbols(objcopy_tool, target, duplicate_symbols)
                    localized_objects += 1

                kept_files.append(target)

        if not kept_files:
            print("error: no unique stdlib objects remain after combining", file=sys.stderr)
            return 1

        output.parent.mkdir(parents=True, exist_ok=True)
        output.unlink(missing_ok=True)
        subprocess.run(
            [ar_tool, "crs", str(output), *[str(path) for path in kept_files]],
            check=True,
        )

    print(f"combined {len(archives)} stdlib archives into {output}")
    print(f"  kept objects:        {len(kept_files)}")
    print(f"  skipped runtime:     {skipped_runtime}")
    print(f"  skipped duplicates:  {skipped_duplicates}")
    print(f"  localized objects:   {localized_objects}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
