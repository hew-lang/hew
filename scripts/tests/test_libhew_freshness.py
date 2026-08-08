#!/usr/bin/env python3
"""Counterfactual checks for the content-addressed libhew archive gate."""

from __future__ import annotations

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import threading


REPO = Path(__file__).resolve().parents[2]


def run(root: Path, *args: str, expect: int = 0) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(args, cwd=root, text=True, capture_output=True)
    assert result.returncode == expect, (
        f"expected {expect}, got {result.returncode}: {' '.join(args)}\n"
        f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}"
    )
    return result


def fixture() -> tuple[tempfile.TemporaryDirectory[str], Path, Path]:
    temp = tempfile.TemporaryDirectory()
    root = Path(temp.name)
    shutil.copytree(REPO / "scripts", root / "scripts")
    (root / "Cargo.toml").write_text(
        '[workspace]\nmembers = ["hew-lib"]\nresolver = "2"\n', encoding="utf-8"
    )
    crate = root / "hew-lib"
    crate.mkdir()
    (crate / "Cargo.toml").write_text(
        '[package]\nname = "hew-lib"\nversion = "0.1.0"\nedition = "2021"\n'
        '[lib]\ncrate-type = ["staticlib"]\n'
        '[dependencies]\nrelevant = "1"\n',
        encoding="utf-8",
    )
    (crate / "src").mkdir()
    (crate / "src" / "lib.rs").write_text("pub fn input() {}\n", encoding="utf-8")
    (root / "Cargo.lock").write_text(
        'version = 4\n\n[[package]]\nname = "hew-lib"\nversion = "0.1.0"\n'
        'dependencies = ["relevant"]\n\n[[package]]\nname = "relevant"\n'
        'version = "1.0.0"\nsource = "registry+https://example.invalid/index"\n'
        'checksum = "relevant-v1"\n',
        encoding="utf-8",
    )
    debug = root / "outside target with spaces" / "debug"
    debug.mkdir(parents=True)
    (debug / "libhew.a").write_bytes(b"good archive")
    return temp, root, debug


def stamp(root: Path, debug: Path) -> None:
    run(
        root,
        str(root / "scripts" / "libhew-freshness.py"),
        "stamp",
        "--debug-dir",
        str(debug),
    )


def no_op_build(root: Path, debug: Path) -> None:
    """Model Cargo's fingerprinted success path without recompiling the fixture."""
    run(
        root,
        str(root / "scripts" / "libhew-freshness.py"),
        "build",
        "--debug-dir",
        str(debug),
        "--",
        "/usr/bin/true",
    )


def verify(
    root: Path, debug: Path, expected: int = 0
) -> subprocess.CompletedProcess[str]:
    return run(
        root,
        str(root / "scripts" / "check-libhew-fresh.sh"),
        "--debug-dir",
        str(debug),
        expect=expected,
    )


def test_green_and_lockfile_noise() -> None:
    temp, root, debug = fixture()
    try:
        stamp(root, debug)
        verify(root, debug)
        # Timestamp-only lockfile noise must not invalidate a Cargo no-op.
        os.utime(root / "Cargo.lock", None)
        no_op_build(root, debug)
        verify(root, debug)
        # An unrelated workspace lock package must also leave the semantic
        # closure unchanged (Cargo does not relink hew-lib for this delta).
        with (root / "Cargo.lock").open("a", encoding="utf-8") as handle:
            handle.write(
                '\n[[package]]\nname = "unrelated"\nversion = "9.9.9"\n'
                'source = "registry+https://example.invalid/index"\nchecksum = "unused"\n'
            )
        no_op_build(root, debug)
        verify(root, debug)
    finally:
        temp.cleanup()


def test_counterfactuals_red() -> None:
    temp, root, debug = fixture()
    try:
        stamp(root, debug)
        # A source edit without Cargo is stale even when its mtime is restored.
        source = root / "hew-lib" / "src" / "lib.rs"
        original_stat = source.stat()
        source.write_text("pub fn input() { panic!() }\n", encoding="utf-8")
        os.utime(source, ns=(original_stat.st_atime_ns, original_stat.st_mtime_ns))
        assert "stale" in verify(root, debug, 1).stderr
        stamp(root, debug)

        # A relevant lock package change has the same fail-closed behavior.
        lock = root / "Cargo.lock"
        lock.write_text(
            lock.read_text(encoding="utf-8").replace("relevant-v1", "relevant-v2"),
            encoding="utf-8",
        )
        assert "stale" in verify(root, debug, 1).stderr
        lock.write_text(
            lock.read_text(encoding="utf-8").replace("relevant-v2", "relevant-v1"),
            encoding="utf-8",
        )
        stamp(root, debug)

        # Replacement/corruption after certification cannot be blessed by an
        # old stamp, and neither can a malformed/unknown-format stamp.
        (debug / "libhew.a").write_bytes(b"replacement archive")
        assert "corrupt or stale" in verify(root, debug, 1).stderr
        stamp(root, debug)
        (debug / ".hew-libhew-freshness-v1").write_text(
            "not a certificate\n", encoding="ascii"
        )
        assert "malformed" in verify(root, debug, 1).stderr
        (debug / ".hew-libhew-freshness-v1").write_text(
            "hew-libhew-freshness-v0\n", encoding="ascii"
        )
        assert "unknown format version" in verify(root, debug, 1).stderr
        (debug / "libhew.a").unlink()
        assert "stable libhew" in verify(root, debug, 1).stderr
    finally:
        temp.cleanup()


def test_windows_name_and_concurrent_readers_writers() -> None:
    temp, root, debug = fixture()
    try:
        archive = debug / "libhew.a"
        archive.rename(debug / "hew.lib")
        stamp(root, debug)
        verify(root, debug)

        helper = str(root / "scripts" / "libhew-freshness.py")
        failures: list[str] = []

        def writer() -> None:
            for _ in range(12):
                result = subprocess.run(
                    [helper, "stamp", "--debug-dir", str(debug)],
                    cwd=root,
                    text=True,
                    capture_output=True,
                )
                if result.returncode:
                    failures.append(result.stderr)

        def reader() -> None:
            for _ in range(12):
                result = subprocess.run(
                    [helper, "verify", "--debug-dir", str(debug)],
                    cwd=root,
                    text=True,
                    capture_output=True,
                )
                if result.returncode:
                    failures.append(result.stderr)

        threads = [threading.Thread(target=writer) for _ in range(2)] + [
            threading.Thread(target=reader) for _ in range(2)
        ]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
        assert not failures, "\n".join(failures)

        # A certificate may be atomically replaced while a reader is taking
        # its two reads.  The reader may observe the old complete generation,
        # but a completed bad generation is always red (never mixed green).
        good_stamp = (debug / ".hew-libhew-freshness-v1").read_text(encoding="ascii")
        # Keep the shape valid while changing the bound archive digest.
        bad_stamp = good_stamp.splitlines()
        bad_stamp[2] = "archive-sha256 " + "0" * 64
        replacement = debug / ".replacement-certificate"
        replacement.write_text("\n".join(bad_stamp) + "\n", encoding="ascii")
        for _ in range(4):
            os.replace(replacement, debug / ".hew-libhew-freshness-v1")
            assert verify(root, debug, 1).returncode == 1
            (debug / ".hew-libhew-freshness-v1").write_text(
                good_stamp, encoding="ascii"
            )
            replacement.write_text("\n".join(bad_stamp) + "\n", encoding="ascii")
    finally:
        temp.cleanup()


def main() -> None:
    test_green_and_lockfile_noise()
    test_counterfactuals_red()
    test_windows_name_and_concurrent_readers_writers()
    print("PASS: libhew content freshness counterfactuals")


if __name__ == "__main__":
    main()
