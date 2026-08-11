#!/usr/bin/env python3
"""Pack and verify the compiled Hew bundle shared by CI test jobs."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import tempfile
from typing import NoReturn


REPO_ROOT = Path(__file__).resolve().parent.parent
FRESHNESS = REPO_ROOT / "scripts" / "libhew-freshness.py"
MANIFEST_NAME = "compiled-hew-manifest-v1.json"
STAMP_NAME = ".hew-libhew-freshness-v1"


def die(message: str) -> NoReturn:
    print(f"compiled-hew-artifact: {message}", file=sys.stderr)
    raise SystemExit(1)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def archive_name(debug_dir: Path) -> str:
    for name in ("libhew.a", "hew.lib"):
        if (debug_dir / name).is_file():
            return name
    die(f"no libhew archive found in {debug_dir}")


def compiler_name(debug_dir: Path) -> str:
    for name in ("hew", "hew.exe"):
        if (debug_dir / name).is_file():
            return name
    die(f"no Hew compiler found in {debug_dir}")


def verify_freshness(debug_dir: Path) -> None:
    result = subprocess.run(
        [sys.executable, str(FRESHNESS), "verify", "--debug-dir", str(debug_dir)],
        cwd=REPO_ROOT,
    )
    if result.returncode:
        die("libhew freshness verification failed")


def pack(source_debug_dir: Path, output: Path, source_revision: str) -> None:
    source_debug_dir = source_debug_dir.resolve()
    compiler = compiler_name(source_debug_dir)
    archive = archive_name(source_debug_dir)
    stamp = source_debug_dir / STAMP_NAME
    if not stamp.is_file():
        die(f"freshness certificate is missing from {source_debug_dir}")
    verify_freshness(source_debug_dir)

    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="compiled-hew-artifact-") as temporary:
        root = Path(temporary) / "compiled-hew"
        debug = root / "debug"
        debug.mkdir(parents=True)
        for name in (compiler, archive, STAMP_NAME):
            shutil.copy2(source_debug_dir / name, debug / name)
        manifest = {
            "format": "compiled-hew-artifact-v1",
            "source_revision": source_revision,
            "files": {
                f"debug/{name}": sha256(debug / name)
                for name in (compiler, archive, STAMP_NAME)
            },
        }
        (root / MANIFEST_NAME).write_text(
            json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )
        with tarfile.open(output, "w:gz") as bundle:
            bundle.add(root, arcname=root.name)


def safe_extract(bundle: tarfile.TarFile, destination: Path) -> None:
    destination = destination.resolve()
    for member in bundle.getmembers():
        resolved = (destination / member.name).resolve()
        if destination != resolved and destination not in resolved.parents:
            die(f"archive member escapes destination: {member.name}")
        if not (member.isfile() or member.isdir()):
            die(f"archive member has an unsupported type: {member.name}")
    bundle.extractall(destination)


def verify(extracted_root: Path, expected_revision: str | None) -> None:
    root = extracted_root.resolve()
    manifest_path = root / MANIFEST_NAME
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (FileNotFoundError, json.JSONDecodeError) as error:
        die(f"cannot read {manifest_path}: {error}")
    if manifest.get("format") != "compiled-hew-artifact-v1":
        die("artifact manifest has an unknown format")
    revision = manifest.get("source_revision")
    if expected_revision is not None and revision != expected_revision:
        die(f"artifact revision {revision!r} does not match {expected_revision!r}")
    files = manifest.get("files")
    if not isinstance(files, dict) or not files:
        die("artifact manifest has no file hashes")
    for relative, expected in files.items():
        path = root / relative
        if not path.is_file():
            die(f"artifact file is missing: {relative}")
        actual = sha256(path)
        if actual != expected:
            die(f"artifact file hash mismatch: {relative}")
    debug = root / "debug"
    compiler = debug / compiler_name(debug)
    if not compiler.stat().st_mode & 0o111:
        die(f"compiler is not executable: {compiler}")
    verify_freshness(debug)


def unpack(input_path: Path, destination: Path, expected_revision: str | None) -> None:
    destination.mkdir(parents=True, exist_ok=True)
    with tarfile.open(input_path, "r:gz") as bundle:
        safe_extract(bundle, destination)
    verify(destination / "compiled-hew", expected_revision)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    subcommands = parser.add_subparsers(dest="action", required=True)
    pack_parser = subcommands.add_parser("pack")
    pack_parser.add_argument("--source-debug-dir", type=Path, required=True)
    pack_parser.add_argument("--output", type=Path, required=True)
    pack_parser.add_argument("--source-revision", required=True)
    unpack_parser = subcommands.add_parser("unpack")
    unpack_parser.add_argument("--input", type=Path, required=True)
    unpack_parser.add_argument("--destination", type=Path, required=True)
    unpack_parser.add_argument("--expected-revision")
    verify_parser = subcommands.add_parser("verify")
    verify_parser.add_argument("--artifact-dir", type=Path, required=True)
    verify_parser.add_argument("--expected-revision")
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    if args.action == "pack":
        pack(args.source_debug_dir, args.output, args.source_revision)
    elif args.action == "unpack":
        unpack(args.input, args.destination, args.expected_revision)
    else:
        verify(args.artifact_dir, args.expected_revision)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
