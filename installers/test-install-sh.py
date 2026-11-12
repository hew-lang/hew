#!/usr/bin/env python3
"""Exercise the Unix installer's handling of real and incomplete archives."""

import hashlib
import io
import os
from pathlib import Path
import platform
import subprocess
import tarfile
import tempfile
import unittest


INSTALLER = Path(__file__).with_name("install.sh")


@unittest.skipUnless(platform.system() == "Linux" and platform.machine() == "x86_64", "Linux x86-64 fixture")
class InstallerTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="hew-installer-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.prefix = self.root / "install"
        self.fixture = self.root / "release"
        self.fixture.mkdir()
        bin_dir = self.root / "mock-bin"
        bin_dir.mkdir()
        curl = bin_dir / "curl"
        curl.write_text('''#!/bin/sh
while [ "$#" -gt 0 ]; do
    case "$1" in
        -o) destination=$2; shift 2 ;;
        -*) shift ;;
        *) url=$1; shift ;;
    esac
done
cp "$HEW_FIXTURE/${url##*/}" "$destination"
''')
        curl.chmod(0o755)
        self.env = dict(os.environ, PATH=f"{bin_dir}:{os.environ['PATH']}",
                        HEW_FIXTURE=str(self.fixture), TMPDIR=str(self.root))

    def archive(self, compiler=True, optional=False):
        name = "hew-v0.10.0-linux-x86_64.tar.gz"
        archive = self.fixture / name
        files = {"lib/libhew.a": b"test library"}
        if compiler:
            files["bin/hew"] = b"#!/bin/sh\nexit 0\n"
        if optional:
            files["bin/hew-lsp"] = b"#!/bin/sh\nexit 0\n"
        with tarfile.open(archive, "w:gz") as tar:
            for path, content in files.items():
                entry = tarfile.TarInfo(f"hew-v0.10.0-linux-x86_64/{path}")
                entry.size = len(content)
                entry.mode = 0o755 if path.startswith("bin/") else 0o644
                tar.addfile(entry, io.BytesIO(content))
        digest = hashlib.sha256(archive.read_bytes()).hexdigest()
        (self.fixture / "hew-v0.10.0-checksums.txt").write_text(f"{digest}  {name}\n")

    def run_installer(self, *args):
        return subprocess.run(["sh", str(INSTALLER), "--version", "0.10.0", "--prefix",
                               str(self.prefix), *args], env=self.env, text=True,
                              capture_output=True, check=False)

    def test_missing_compiler_is_repaired_and_optional_tools_are_installed(self):
        self.archive(optional=True)
        self.prefix.mkdir()
        (self.prefix / ".hew-version").write_text("0.10.0")
        result = self.run_installer()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("restoring the release toolchain", result.stdout)
        self.assertTrue(os.access(self.prefix / "bin/hew", os.X_OK))
        self.assertTrue(os.access(self.prefix / "bin/hew-lsp", os.X_OK))

    def test_healthy_same_version_does_not_download(self):
        (self.prefix / "bin").mkdir(parents=True)
        binary = self.prefix / "bin/hew"
        binary.write_text("existing")
        binary.chmod(0o755)
        (self.prefix / ".hew-version").write_text("0.10.0")
        result = self.run_installer()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("already installed", result.stdout)
        self.assertEqual(binary.read_text(), "existing")

    def test_archive_without_compiler_fails_without_recording_success(self):
        self.archive(compiler=False)
        result = self.run_installer()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("missing required binary: hew", result.stderr)
        self.assertFalse((self.prefix / ".hew-version").exists())

    def test_dry_run_does_not_create_installation(self):
        result = self.run_installer("--dry-run")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("v0.10.0", result.stdout)
        self.assertFalse(self.prefix.exists())


if __name__ == "__main__":
    unittest.main()
