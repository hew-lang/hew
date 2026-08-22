#!/usr/bin/env python3
"""Create content-addressed freshness certificates for Cargo's libhew archive.

Cargo remains the authority for whether it must relink.  After a successful
Cargo build this helper writes an atomic certificate binding the exact archive
bytes to a semantic digest of hew-lib's project-relative input closure.  The
reader in check-libhew-fresh.sh verifies a stable certificate/archive snapshot.
"""

from __future__ import annotations

import argparse
import hashlib
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
from typing import NoReturn


REPO_ROOT = Path(__file__).resolve().parent.parent
INPUTS = REPO_ROOT / "scripts" / "libhew-inputs.py"
STAMP_NAME = ".hew-libhew-freshness-v1"
MAX_STABILITY_ATTEMPTS = 3
MAX_VERIFY_ATTEMPTS = 40


def die(message: str) -> NoReturn:
    print(f"libhew-freshness: {message}", file=sys.stderr)
    raise SystemExit(1)


def archive_in(debug_dir: Path) -> Path:
    unix, windows = debug_dir / "libhew.a", debug_dir / "hew.lib"
    if unix.is_file():
        return unix
    if windows.is_file():
        return windows
    die(f"no libhew archive in {debug_dir} (looked for libhew.a and hew.lib)")


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def input_digest() -> str:
    result = subprocess.run(
        [sys.executable, str(INPUTS), "digest"],
        cwd=REPO_ROOT,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if result.returncode:
        die("could not calculate libhew input digest: " + result.stderr.strip())
    value = result.stdout.strip()
    if len(value) != 64 or any(char not in "0123456789abcdef" for char in value):
        die("libhew-inputs.py returned an invalid input digest")
    return value


def stable_archive_digest(archive: Path) -> str:
    for _ in range(MAX_STABILITY_ATTEMPTS):
        try:
            before = archive.stat()
            value = sha256(archive)
            after = archive.stat()
        except FileNotFoundError:
            continue
        if (before.st_dev, before.st_ino, before.st_size, before.st_mtime_ns) == (
            after.st_dev,
            after.st_ino,
            after.st_size,
            after.st_mtime_ns,
        ):
            return value
    die(f"{archive} changed while its certificate was being written")


def write_stamp(debug_dir: Path, expected_input: str | None) -> None:
    observed_input = input_digest()
    if expected_input is not None and observed_input != expected_input:
        die("libhew inputs changed during Cargo build; refusing to certify its archive")
    archive = archive_in(debug_dir)
    archive_digest = stable_archive_digest(archive)
    payload = (
        "hew-libhew-freshness-v1\n"
        f"input-sha256 {observed_input}\n"
        f"archive-sha256 {archive_digest}\n"
    ).encode("ascii")
    debug_dir.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(prefix=f".{STAMP_NAME}.", dir=debug_dir)
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(temporary, debug_dir / STAMP_NAME)
    finally:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass


def read_stamp(path: Path) -> tuple[str, str]:
    try:
        lines = path.read_text(encoding="ascii").splitlines()
    except FileNotFoundError:
        die(f"freshness certificate {path} is missing; run make libhew-debug")
    if (
        lines
        and lines[0].startswith("hew-libhew-freshness-")
        and lines[0] != "hew-libhew-freshness-v1"
    ):
        die(f"freshness certificate {path} has an unknown format version")
    if len(lines) != 3 or lines[0] != "hew-libhew-freshness-v1":
        die(f"freshness certificate {path} is malformed")
    expected = []
    for line, field in zip(lines[1:], ("input-sha256 ", "archive-sha256 ")):
        if not line.startswith(field):
            die(f"freshness certificate {path} is malformed")
        value = line[len(field) :]
        if len(value) != 64 or any(char not in "0123456789abcdef" for char in value):
            die(f"freshness certificate {path} has an invalid {field[:-1]}")
        expected.append(value)
    return expected[0], expected[1]


def verify(debug_dir: Path) -> None:
    stamp = debug_dir / STAMP_NAME
    last_problem = "archive or certificate changed while being checked"
    for _ in range(MAX_VERIFY_ATTEMPTS):
        # Read the certificate twice around the archive.  os.replace makes a
        # writer visible all-at-once; matching reads therefore prove this was
        # one certificate generation rather than a mixed writer/reader view.
        try:
            stamped_input, stamped_archive = read_stamp(stamp)
            archive = archive_in(debug_dir)
            current_input = input_digest()
            actual_archive = stable_archive_digest(archive)
            if stamp.read_text(encoding="ascii").splitlines() != [
                "hew-libhew-freshness-v1",
                f"input-sha256 {stamped_input}",
                f"archive-sha256 {stamped_archive}",
            ]:
                last_problem = "certificate changed while being checked"
            elif current_input != stamped_input:
                last_problem = (
                    f"{archive} is stale: its certificate does not match current "
                    "hew-lib inputs; run make libhew-debug"
                )
            elif actual_archive != stamped_archive:
                last_problem = (
                    f"{archive} is corrupt or stale: its bytes do not match the "
                    "Cargo-build certificate; run make libhew-debug"
                )
            else:
                print(f"ok: {archive} matches its Cargo/content freshness certificate")
                return
        except SystemExit:
            # Missing archive/stamp during a concurrent Cargo uplift can be a
            # transient state.  Give an active writer a short bounded window;
            # a genuinely missing/corrupt artifact still deterministically red.
            if not archive_or_stamp_maybe_transient(debug_dir):
                raise
            last_problem = "archive or certificate is temporarily absent"
        time.sleep(0.05)
    die(
        f"could not obtain a stable libhew archive/certificate snapshot: {last_problem}"
    )


def archive_or_stamp_maybe_transient(debug_dir: Path) -> bool:
    return not (debug_dir / STAMP_NAME).exists() or not (
        (debug_dir / "libhew.a").exists() or (debug_dir / "hew.lib").exists()
    )


def build(debug_dir: Path, command: list[str]) -> None:
    if os.environ.get("HEW_TEST_NO_BUILD") == "1" or os.environ.get("NEXTEST_RUN_ID"):
        die("build is disabled during a test run; run make stdlib before the test gate")
    if not command:
        die("build requires a Cargo command after --")
    before = input_digest()
    result = subprocess.run(command, cwd=REPO_ROOT)
    if result.returncode:
        raise SystemExit(result.returncode)
    write_stamp(debug_dir, before)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    subcommands = parser.add_subparsers(dest="action", required=True)
    for name in ("build", "stamp", "verify"):
        command = subcommands.add_parser(name)
        command.add_argument("--debug-dir", type=Path, required=True)
        command.add_argument("--expected-input")
    build_parser = subcommands.choices["build"]
    build_parser.add_argument("command", nargs=argparse.REMAINDER)
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    if args.action == "build":
        command = args.command[1:] if args.command[:1] == ["--"] else args.command
        build(args.debug_dir, command)
    elif args.action == "stamp":
        write_stamp(args.debug_dir, args.expected_input)
    else:
        verify(args.debug_dir)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
