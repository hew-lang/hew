#!/usr/bin/env python3
"""Tests for the shared compiled-Hew artifact contract."""

from __future__ import annotations

import hashlib
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "compiled-hew-artifact.py"
INPUTS = ROOT / "scripts" / "libhew-inputs.py"
STAMP = ".hew-libhew-freshness-v1"


class CompiledHewArtifactTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.debug = self.root / "source" / "debug"
        self.debug.mkdir(parents=True)
        compiler = self.debug / "hew"
        compiler.write_bytes(b"#!/bin/sh\nexit 0\n")
        compiler.chmod(0o755)
        archive = self.debug / "libhew.a"
        archive.write_bytes(b"fresh archive bytes")
        input_digest = subprocess.check_output(
            [sys.executable, str(INPUTS), "digest"], cwd=ROOT, text=True
        ).strip()
        archive_digest = hashlib.sha256(archive.read_bytes()).hexdigest()
        (self.debug / STAMP).write_text(
            "hew-libhew-freshness-v1\n"
            f"input-sha256 {input_digest}\n"
            f"archive-sha256 {archive_digest}\n",
            encoding="ascii",
        )

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def run_script(
        self, *arguments: str, expect: int = 0
    ) -> subprocess.CompletedProcess[str]:
        result = subprocess.run(
            [sys.executable, str(SCRIPT), *arguments],
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.assertEqual(result.returncode, expect, result.stdout + result.stderr)
        return result

    def test_round_trip_preserves_revision_permissions_and_freshness(self) -> None:
        bundle = self.root / "bundle.tar.gz"
        destination = self.root / "unpacked"
        self.run_script(
            "pack",
            "--source-debug-dir",
            str(self.debug),
            "--output",
            str(bundle),
            "--source-revision",
            "abc123",
        )
        self.run_script(
            "unpack",
            "--input",
            str(bundle),
            "--destination",
            str(destination),
            "--expected-revision",
            "abc123",
        )
        compiler = destination / "compiled-hew" / "debug" / "hew"
        self.assertTrue(compiler.stat().st_mode & 0o111)

    def test_wrong_revision_fails_closed(self) -> None:
        bundle = self.root / "bundle.tar.gz"
        self.run_script(
            "pack",
            "--source-debug-dir",
            str(self.debug),
            "--output",
            str(bundle),
            "--source-revision",
            "abc123",
        )
        result = self.run_script(
            "unpack",
            "--input",
            str(bundle),
            "--destination",
            str(self.root / "wrong"),
            "--expected-revision",
            "def456",
            expect=1,
        )
        self.assertIn("does not match", result.stderr)

    def test_modified_archive_fails_before_freshness_check(self) -> None:
        bundle = self.root / "bundle.tar.gz"
        destination = self.root / "tampered"
        self.run_script(
            "pack",
            "--source-debug-dir",
            str(self.debug),
            "--output",
            str(bundle),
            "--source-revision",
            "abc123",
        )
        with tarfile.open(bundle, "r:gz") as archive:
            archive.extractall(destination)
        artifact = destination / "compiled-hew"
        (artifact / "debug" / "libhew.a").write_bytes(b"tampered")
        result = self.run_script(
            "verify",
            "--artifact-dir",
            str(artifact),
            "--expected-revision",
            "abc123",
            expect=1,
        )
        self.assertIn("hash mismatch", result.stderr)

    def test_archive_path_traversal_fails_closed(self) -> None:
        bundle = self.root / "escape.tar.gz"
        payload = self.root / "payload"
        payload.write_text("escape", encoding="utf-8")
        with tarfile.open(bundle, "w:gz") as archive:
            archive.add(payload, arcname="../escape")
        result = self.run_script(
            "unpack",
            "--input",
            str(bundle),
            "--destination",
            str(self.root / "escape-destination"),
            expect=1,
        )
        self.assertIn("escapes destination", result.stderr)


if __name__ == "__main__":
    unittest.main()
