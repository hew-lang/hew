#!/usr/bin/env python3
"""Check that Windows packaged debug qualification fails closed."""

import importlib.util
import json
from pathlib import Path
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


if __name__ == "__main__":
    unittest.main()
