#!/usr/bin/env python3
"""Exercise required products, explicit deferrals and native-only package staging."""

import hashlib
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location(
    "release_assets", ROOT / "scripts/release-assets.py"
)
assets = importlib.util.module_from_spec(spec)
spec.loader.exec_module(assets)
SHA = "a" * 40
VERSION = "0.6.3"
DETAIL = {
    "owner": "release maintainer",
    "reason": "packaging tool unavailable; native qualification passed",
    "evidence": "https://example.invalid/qualification",
}


class ReleaseInventory(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        self.jobs = {
            name: {"result": "success"}
            for name in [
                "validate",
                "build-linux",
                "build",
                "build-freebsd",
                "build-npm-packages",
                "build-portable-release-libs",
                *assets.OPTIONAL_JOBS,
            ]
        }
        for target, suffix in assets.NATIVE.items():
            (self.directory / f"hew-v{VERSION}-{target}{suffix}").write_bytes(
                target.encode()
            )
        for name in [
            f"hew-lang-{name}-{VERSION}.tgz" for name in ("wasm", "sandbox-vm")
        ]:
            (self.directory / name).write_bytes(name.encode())
        for name in [
            "hew_0.6.3_amd64.deb",
            "hew-0.6.3.x86_64.rpm",
            "hew-bin-0.6.3.pkg.tar.zst",
        ]:
            (self.directory / name).write_bytes(b"package")

    def assemble(self, deferrals=None, version=VERSION):
        return assets.assemble(
            self.directory,
            version,
            SHA,
            "https://example.invalid/run",
            self.jobs,
            deferrals or {},
        )

    def test_complete_set_and_receipt_have_exact_checksums_without_self_reference(self):
        receipt = self.assemble()
        self.assertEqual(receipt["source"], SHA)
        self.assertTrue(
            all(
                receipt["platforms"][name]["state"] == "available"
                for name in assets.NATIVE
            )
        )
        checksums = self.directory / f"hew-v{VERSION}-checksums.txt"
        for line in checksums.read_text().splitlines():
            digest, name = line.split("  ")
            self.assertNotEqual(name, checksums.name)
            self.assertEqual(
                digest, hashlib.sha256((self.directory / name).read_bytes()).hexdigest()
            )
        self.assertIn(f"hew-v{VERSION}-platforms.json", checksums.read_text())

    def test_failed_secondary_packaging_keeps_linux_and_browser_delivery_with_explicit_evidence(
        self,
    ):
        for target in ["windows-x86_64", "freebsd-x86_64"]:
            (
                self.directory / f"hew-v{VERSION}-{target}{assets.NATIVE[target]}"
            ).unlink()
        self.jobs["build"]["result"] = "failure"
        self.jobs["build-freebsd"]["result"] = "cancelled"
        receipt = self.assemble(
            {name: DETAIL for name in ["windows-x86_64", "freebsd-x86_64"]}
        )
        self.assertEqual(receipt["jobs"]["build"], "failure")
        self.assertEqual(receipt["platforms"]["windows-x86_64"]["state"], "deferred")
        self.assertEqual(
            receipt["platforms"]["freebsd-x86_64"]["evidence"], DETAIL["evidence"]
        )
        self.assertEqual(receipt["platforms"]["darwin-aarch64"]["state"], "available")

    def test_failed_optional_wasi_is_reported_without_changing_native_requirements(
        self,
    ):
        self.jobs["build-portable-release-libs"]["result"] = "failure"
        receipt = self.assemble()
        self.assertEqual(receipt["platforms"]["wasm32-wasip1"]["state"], "not-produced")
        self.assertEqual(receipt["platforms"]["wasm32-wasip1"]["result"], "failure")

    def test_missing_linux_browser_or_successful_package_output_is_refused(self):
        for name in [
            f"hew-v{VERSION}-linux-aarch64.tar.gz",
            f"hew-lang-sandbox-vm-{VERSION}.tgz",
            "hew_0.6.3_amd64.deb",
        ]:
            path = self.directory / name
            content = path.read_bytes()
            path.unlink()
            with self.assertRaises(ValueError):
                self.assemble()
            path.write_bytes(content)

    def test_failed_primary_job_is_not_admitted_by_present_artifacts(self):
        for name in ["validate", "build-linux", "build-npm-packages"]:
            self.jobs[name]["result"] = "failure"
            with self.assertRaisesRegex(ValueError, "required producer"):
                self.assemble()
            self.jobs[name]["result"] = "success"

    def test_missing_secondary_needs_documented_patch_deferral(self):
        (self.directory / f"hew-v{VERSION}-darwin-aarch64.tar.gz").unlink()
        for deferrals in [
            {},
            {"darwin-aarch64": {"reason": "missing"}},
            {"linux-x86_64": DETAIL},
            {"darwin-aarch64": {**DETAIL, "evidence": ""}},
        ]:
            with self.assertRaises(ValueError):
                self.assemble(deferrals)
        with self.assertRaisesRegex(ValueError, "patch deferrals"):
            self.assemble({"darwin-aarch64": DETAIL}, version="0.7.0")

    def test_prerelease_records_unproduced_distro_packages_even_when_note_step_succeeds(
        self,
    ):
        version = "0.6.3-rc.1"
        for path in list(self.directory.iterdir()):
            if path.suffix == ".deb" or path.name.endswith((".rpm", ".pkg.tar.zst")):
                path.unlink()
            else:
                path.rename(path.with_name(path.name.replace(VERSION, version)))
        receipt = self.assemble(version=version)
        self.assertEqual(
            receipt["platforms"]["linux-packages"]["state"], "not-produced"
        )
        self.assertEqual(receipt["jobs"]["linux-packages"], "success")

    def test_channel_deferral_keeps_its_actual_failure_and_evidence(self):
        self.jobs["docker-image"]["result"] = "failure"
        with self.assertRaisesRegex(ValueError, "packaging needs"):
            self.assemble()
        receipt = self.assemble({"docker-image": DETAIL})
        self.assertEqual(
            receipt["platforms"]["docker-image"],
            {"state": "deferred", "result": "failure", **DETAIL},
        )


class NativePackage(unittest.TestCase):
    def test_optional_wasi_absence_preserves_native_package_but_partial_wasi_is_refused(
        self,
    ):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for name in [
                "std/example.hew",
                "hew-cabi/include/hew.h",
                "hew-cabi/include/hew_host.h",
                "scripts/debug/hew-gdb.py",
                "scripts/debug/hew_lldb.py",
                "LICENSE-MIT",
                "LICENSE-APACHE",
                "NOTICE",
                "THIRD-PARTY-LICENSES",
                "README.md",
                "native.a",
            ]:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("fixture")
            (root / "bin").mkdir()
            compiler = root / "bin/hew"
            compiler.write_text('#!/bin/sh\nprintf "complete -c hew\\n"\n')
            compiler.chmod(0o755)
            args = [
                "sh",
                str(ROOT / "scripts/stage-release-package.sh"),
                "--source-dir",
                str(root),
                "--bin-dir",
                str(root / "bin"),
                "--native-lib",
                str(root / "native.a"),
                "--native-triple",
                "x86_64-unknown-linux-gnu",
                "--native-lib-name",
                "libhew.a",
                "--wasi-lib-dir",
                str(root / "wasi"),
                "--destination",
                str(root / "output"),
                "--completion-shells",
                "bash",
            ]
            result = subprocess.run(args, capture_output=True, text=True)
            self.assertNotEqual(result.returncode, 0)
            result = subprocess.run(
                [*args, "--optional-wasi"], capture_output=True, text=True
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual((root / "output/lib/libhew.a").read_text(), "fixture")
            self.assertFalse((root / "output/lib/wasm32-wasip1").exists())
            self.assertIn("native toolchain", result.stdout)
            (root / "wasi").mkdir()
            (root / "wasi/libhew_runtime.a").write_bytes(b"partial")
            result = subprocess.run(
                [*args, "--optional-wasi"], capture_output=True, text=True
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("missing portable WASI library", result.stderr)
            (root / "wasi/libhew_std.a").write_bytes(b"standard")
            args[args.index("--destination") + 1] = str(root / "with-wasi")
            result = subprocess.run(args, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(
                (root / "with-wasi/lib/wasm32-wasip1/libhew_runtime.a").read_bytes(),
                b"partial",
            )
            self.assertEqual(
                (root / "with-wasi/lib/wasm32-wasip1/libhew_std.a").read_bytes(),
                b"standard",
            )


if __name__ == "__main__":
    unittest.main()
