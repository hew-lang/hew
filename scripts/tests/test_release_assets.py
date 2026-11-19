#!/usr/bin/env python3
"""Exercise required products, optional notes and native-only package staging."""

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
                *assets.CHANNEL_JOBS,
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
        (self.directory / f"hew-v{VERSION}-reference-docs.tar.gz").write_bytes(
            b"reference docs"
        )
        for name in [
            f"hew_{VERSION}-1_amd64.deb",
            f"hew_{VERSION}-1_arm64.deb",
            f"hew-dbgsym_{VERSION}-1_amd64.deb",
            f"hew-dbgsym_{VERSION}-1_arm64.deb",
            f"hew-{VERSION}-1.fc41.x86_64.rpm",
            f"hew-{VERSION}-1.fc41.aarch64.rpm",
            f"hew-debuginfo-{VERSION}-1.fc41.x86_64.rpm",
            f"hew-debuginfo-{VERSION}-1.fc41.aarch64.rpm",
            f"hew-bin-{VERSION}-1-x86_64.pkg.tar.zst",
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
        self.assertEqual(receipt["platforms"]["linux-packages"]["state"], "available")
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

    def test_patch_delivers_linux_and_browser_without_optional_outputs_or_notes(self):
        for target in set(assets.NATIVE) - assets.PRIMARY:
            (
                self.directory / f"hew-v{VERSION}-{target}{assets.NATIVE[target]}"
            ).unlink()
        self.jobs["build"]["result"] = "failure"
        self.jobs["build-freebsd"]["result"] = "cancelled"
        for job in assets.CHANNEL_JOBS - {"build-docs"}:
            self.jobs[job]["result"] = "failure"
        receipt = self.assemble()
        self.assertEqual(receipt["jobs"]["build"], "failure")
        self.assertEqual(receipt["jobs"]["build-freebsd"], "cancelled")
        for target in (set(assets.NATIVE) - assets.PRIMARY) | (
            assets.CHANNEL_JOBS - {"build-docs"}
        ):
            self.assertEqual(receipt["platforms"][target]["state"], "unavailable")
        self.assertEqual(receipt["platforms"]["build-docs"]["state"], "available")
        self.assertEqual(receipt["notes"], {})

    def test_documentation_reference_is_required_for_patch_releases(self):
        docs = self.directory / f"hew-v{VERSION}-reference-docs.tar.gz"
        docs.unlink()
        with self.assertRaisesRegex(ValueError, "build-docs"):
            self.assemble()
        docs.write_bytes(b"reference docs")
        self.jobs["build-docs"]["result"] = "failure"
        with self.assertRaisesRegex(ValueError, "build-docs"):
            self.assemble()

    def test_failed_optional_wasi_is_reported_without_changing_native_requirements(
        self,
    ):
        self.jobs["build-portable-release-libs"]["result"] = "failure"
        receipt = self.assemble()
        self.assertEqual(receipt["platforms"]["wasm32-wasip1"]["state"], "not-produced")
        self.assertEqual(receipt["platforms"]["wasm32-wasip1"]["result"], "failure")

    def test_missing_linux_or_browser_output_is_refused(self):
        for name in [
            f"hew-v{VERSION}-linux-x86_64.tar.gz",
            f"hew-v{VERSION}-linux-aarch64.tar.gz",
            f"hew-lang-wasm-{VERSION}.tgz",
            f"hew-lang-sandbox-vm-{VERSION}.tgz",
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

    def test_optional_notes_are_informational_and_minor_still_requires_platforms(self):
        (self.directory / f"hew-v{VERSION}-darwin-aarch64.tar.gz").unlink()
        notes = {"darwin-aarch64": {"state": "available"}, "other": "useful context"}
        receipt = self.assemble(notes)
        self.assertEqual(receipt["notes"], notes)
        self.assertEqual(receipt["platforms"]["darwin-aarch64"]["state"], "unavailable")
        for path in self.directory.iterdir():
            path.rename(path.with_name(path.name.replace(VERSION, "0.7.0")))
        with self.assertRaisesRegex(ValueError, "required qualified archive"):
            self.assemble(notes, version="0.7.0")

    def test_a_missing_architecture_or_debug_package_makes_the_channel_unavailable(
        self,
    ):
        for name in (
            f"hew_{VERSION}-1_arm64.deb",
            f"hew-{VERSION}-1.fc41.aarch64.rpm",
            f"hew-dbgsym_{VERSION}-1_amd64.deb",
            f"hew-debuginfo-{VERSION}-1.fc41.x86_64.rpm",
            f"hew-bin-{VERSION}-1-x86_64.pkg.tar.zst",
        ):
            with self.subTest(name=name):
                path = self.directory / name
                path.unlink()
                receipt = self.assemble()
                self.assertEqual(
                    receipt["platforms"]["linux-packages"],
                    {"state": "unavailable", "result": "success", "missing": [name]},
                )
                path.write_bytes(b"package")

    def test_an_empty_expected_package_is_missing(self):
        name = f"hew_{VERSION}-1_amd64.deb"
        (self.directory / name).write_bytes(b"")
        channel = self.assemble()["platforms"]["linux-packages"]
        self.assertEqual(channel["state"], "unavailable")
        self.assertEqual(channel["missing"], [name])

    def test_a_stale_package_makes_the_channel_unavailable(self):
        name = f"hew_{VERSION}-1_arm64.deb"
        current = self.directory / name
        stale = "hew_0.6.2-1_arm64.deb"
        current.rename(self.directory / stale)
        channel = self.assemble()["platforms"]["linux-packages"]
        self.assertEqual(channel["state"], "unavailable")
        self.assertEqual(channel["missing"], [name])
        self.assertEqual(channel["unexpected"], [stale])

        current.write_bytes(b"package")
        channel = self.assemble()["platforms"]["linux-packages"]
        self.assertEqual(channel["state"], "unavailable")
        self.assertNotIn("missing", channel)
        self.assertEqual(channel["unexpected"], [stale])

    def test_a_final_minor_requires_the_complete_distro_set(self):
        version = "0.7.0"
        for path in self.directory.iterdir():
            path.rename(path.with_name(path.name.replace(VERSION, version)))
        receipt = self.assemble(version=version)
        self.assertEqual(receipt["platforms"]["linux-packages"]["state"], "available")
        (self.directory / f"hew-dbgsym_{version}-1_arm64.deb").unlink()
        with self.assertRaisesRegex(ValueError, "linux-packages"):
            self.assemble(version=version)

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
            (root / "wasi").mkdir()
            (root / "wasi/libhew_runtime.a").write_bytes(b"partial")
            result = subprocess.run(
                [*args, "--optional-wasi"], capture_output=True, text=True
            )
            self.assertNotEqual(result.returncode, 0)
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
