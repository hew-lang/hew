#!/usr/bin/env python3
"""Check that Windows packaged debug qualification fails closed."""

import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import tarfile
import tempfile
import unittest
import zipfile


ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location(
    "packaged_sdk", ROOT / "scripts/test-packaged-sdk.py"
)
packaged_sdk = importlib.util.module_from_spec(spec)
spec.loader.exec_module(packaged_sdk)


class WindowsDebugAcceptance(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.output = Path(self.temp.name)
        self.sdk = self.output / "sdk"
        (self.sdk / "bin").mkdir(parents=True)
        (self.sdk / "bin/hew.exe").write_bytes(b"compiler")
        self.archive = self.output / "debug.zip"
        with zipfile.ZipFile(self.archive, "w") as package:
            package.writestr("hew-debug/hew.pdb", b"matching compiler symbols")
        self.labels = []
        self.reject_wrong_pdb = True
        self.resolve_pdb = True

    def run_command(self, label, command, **kwargs):
        self.labels.append(label)
        if label == "debug-build":
            (self.output / "p.pdb").write_bytes(b"different program symbols")
        elif label == "debug-wrong-pdb":
            if self.reject_wrong_pdb:
                return 1, b"", b"different build"
            return 0, b"", b""
        elif label == "debug-failure":
            record = {
                "message": "too big",
                "frames": [{"hew_fn": "level2", "pc": "0x1001"}],
                "build": {"image_base": "0x1000"},
            }
            (self.output / "failure.ndjson").write_text(json.dumps(record) + "\n")
            return 1, b"", b""
        elif label.startswith("debug-symbolize-"):
            return 0, b"level2 at p.hew:3" if self.resolve_pdb else b"??", b""
        return 0, b"", b""

    def qualify(self):
        return packaged_sdk.check_windows_debug_sdk(
            self.sdk,
            self.archive,
            self.output,
            ROOT / "tests/packaged-sdk/debug-failure.hew",
            self.run_command,
        )

    def test_qualifies_both_image_and_generated_pdb(self):
        self.qualify()
        self.assertEqual(
            (self.sdk / "bin/hew.pdb").read_bytes(), b"matching compiler symbols"
        )
        self.assertIn("debug-symbolize-image", self.labels)
        self.assertIn("debug-symbolize-pdb", self.labels)

    def test_rejects_missing_compiler_pdb(self):
        with zipfile.ZipFile(self.archive, "w") as package:
            package.writestr("hew-debug/other.pdb", b"symbols")
        with self.assertRaisesRegex(RuntimeError, "one compiler PDB"):
            self.qualify()

    def test_rejects_a_symbolizer_that_accepts_the_wrong_pdb(self):
        self.reject_wrong_pdb = False
        with self.assertRaisesRegex(RuntimeError, "mismatched compiler PDB"):
            self.qualify()

    def test_rejects_unresolved_failure(self):
        self.resolve_pdb = False
        with self.assertRaisesRegex(RuntimeError, "did not resolve"):
            self.qualify()


class MacosSplitSymbols(unittest.TestCase):
    @unittest.skipIf(os.name == "nt", "requires POSIX symlink and shell semantics")
    def test_copies_cargo_symlink_target_into_the_archive(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            built = root / "built"
            dwarf = built / "deps/hew-hash.dSYM/Contents/Resources/DWARF/hew-hash"
            dwarf.parent.mkdir(parents=True)
            dwarf.write_bytes(b"compiler dwarf")
            (built / "hew.dSYM").symlink_to("deps/hew-hash.dSYM")
            binary = root / "hew"
            binary.write_bytes(b"compiler")
            tools = root / "tools"
            tools.mkdir()
            strip = tools / "strip"
            strip.write_text("#!/bin/sh\nexit 0\n")
            strip.chmod(0o755)
            destination = root / "debug"
            subprocess.run(
                [
                    "sh",
                    ROOT / "scripts/split-debug-symbols.sh",
                    "--triple",
                    "aarch64-apple-darwin",
                    "--built-dir",
                    built,
                    "--binary",
                    binary,
                    "--destination",
                    destination,
                ],
                env={
                    **os.environ,
                    "PATH": str(tools) + os.pathsep + os.environ["PATH"],
                },
                check=True,
            )
            bundle = destination / "hew.dSYM"
            self.assertFalse(bundle.is_symlink())
            self.assertEqual(
                (bundle / "Contents/Resources/DWARF/hew-hash").read_bytes(),
                b"compiler dwarf",
            )


class MacosDebugAcceptance(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.output = Path(self.temp.name)
        self.sdk = self.output / "sdk"
        (self.sdk / "bin").mkdir(parents=True)
        (self.sdk / "bin/hew").write_bytes(b"compiler")
        self.archive = self.output / "debug.tar.gz"
        self.match_uuid = True
        self.line_table = True
        self.make_archive()

    def make_archive(self, dangling=False):
        with tarfile.open(self.archive, "w:gz") as package:
            if dangling:
                item = tarfile.TarInfo("hew-debug/hew.dSYM")
                item.type = tarfile.SYMTYPE
                item.linkname = "deps/hew.dSYM"
                package.addfile(item)
            else:
                data = b"compiler dwarf"
                item = tarfile.TarInfo(
                    "hew-debug/hew.dSYM/Contents/Resources/DWARF/hew-hash"
                )
                item.size = len(data)
                package.addfile(item, io.BytesIO(data))
                plist = b"plist"
                item = tarfile.TarInfo("hew-debug/hew.dSYM/Contents/Info.plist")
                item.size = len(plist)
                package.addfile(item, io.BytesIO(plist))

    def run_command(self, label, command, **kwargs):
        if label == "debug-dsym-lines":
            return 0, b"file_names[  1]:" if self.line_table else b"", b""
        uuid = "AAAAAAAA-AAAA-AAAA-AAAA-AAAAAAAAAAAA"
        if label == "debug-dsym-uuid" and not self.match_uuid:
            uuid = "BBBBBBBB-BBBB-BBBB-BBBB-BBBBBBBBBBBB"
        return 0, f"UUID: {uuid} (arm64) image\n".encode(), b""

    def qualify(self):
        return packaged_sdk.check_macos_debug_sdk(
            self.sdk, self.archive, self.output, self.run_command
        )

    def test_qualifies_matching_dwarf_and_line_table(self):
        self.qualify()
        self.assertEqual(
            (self.output / "packaged-hew.dwarf").read_bytes(), b"compiler dwarf"
        )

    def test_rejects_dangling_cargo_bundle(self):
        self.make_archive(dangling=True)
        with self.assertRaisesRegex(RuntimeError, "one compiler DWARF"):
            self.qualify()

    def test_rejects_wrong_uuid_and_missing_line_table(self):
        self.match_uuid = False
        with self.assertRaisesRegex(RuntimeError, "UUIDs differ"):
            self.qualify()
        self.match_uuid = True
        self.line_table = False
        with self.assertRaisesRegex(RuntimeError, "no source line table"):
            self.qualify()


if __name__ == "__main__":
    unittest.main()
