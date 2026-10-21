#!/usr/bin/env python3
"""Exercise publication identity and downstream completion without external writes."""

import base64
import contextlib
import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import importlib.util
import io
import json
import os
import secrets
from pathlib import Path
import subprocess
import tarfile
import tempfile
import threading
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location(
    "release_channel", ROOT / "scripts/release-channel.py"
)
channel = importlib.util.module_from_spec(spec)
spec.loader.exec_module(channel)
SHA = "a" * 40
VERSION = "1.2.3"


def package(name, source=SHA, payload=b"producer"):
    output = io.BytesIO()
    with tarfile.open(fileobj=output, mode="w:gz") as tar:
        for filename, data in {
            "package/package.json": json.dumps(
                {
                    "name": f"@hew-lang/{name}",
                    "version": VERSION,
                    "hewSource": {"commit": source, "dirty": False},
                }
            ).encode(),
            "package/main.js": payload,
        }.items():
            info = tarfile.TarInfo(filename)
            info.size = len(data)
            tar.addfile(info, io.BytesIO(data))
    return output.getvalue()


class Registry(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def do_GET(self):
        if self.server.writes and "wasm" in self.path:
            self.server.published_reads += 1
            if self.server.delayed_metadata:
                self.send_error(503)
                return
        if self.server.mode == "unreadable":
            self.send_error(503)
            return
        if self.server.mode == "absent" and "wasm" in self.path:
            self.send_error(404)
            return
        name = "sandbox-vm" if "sandbox-vm" in self.path else "wasm"
        data = self.server.packages[name]
        if "/tarballs/" in self.path:
            body = data
        else:
            manifest = {
                "name": f"@hew-lang/{name}",
                "version": VERSION,
                "dist": {
                    "tarball": f"{self.server.url}/tarballs/{name}.tgz",
                    "shasum": hashlib.sha1(data).hexdigest(),
                },
            }
            body = json.dumps(
                {
                    **manifest,
                    "versions": {VERSION: manifest},
                    "dist-tags": {"latest": VERSION},
                }
            ).encode()
        self.send_response(200)
        self.end_headers()
        self.wfile.write(body)

    def do_PUT(self):
        self.server.writes += 1
        payload = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        attachment = next(iter(payload["_attachments"].values()))
        self.server.packages["wasm"] = base64.b64decode(attachment["data"])
        self.server.mode = "exists"
        if self.server.uncertain_publish:
            self.close_connection = True
            return
        self.send_response(201)
        self.end_headers()
        self.wfile.write(b'{"ok":true}')


class NpmDelivery(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        (self.root / "packages").mkdir()
        (self.root / "user.npmrc").touch()
        (self.root / "global.npmrc").touch()
        self.packages = {name: package(name) for name in ("wasm", "sandbox-vm")}
        for name, data in self.packages.items():
            (self.root / f"packages/hew-lang-{name}-{VERSION}.tgz").write_bytes(data)
        self.server = ThreadingHTTPServer(("127.0.0.1", 0), Registry)
        self.server.mode = "exists"
        self.server.packages = self.packages.copy()
        self.server.writes = 0
        self.server.published_reads = 0
        self.server.delayed_metadata = False
        self.server.uncertain_publish = False
        self.server.url = f"http://127.0.0.1:{self.server.server_port}"
        (self.root / "user.npmrc").write_text(
            f"//127.0.0.1:{self.server.server_port}/:_authToken={secrets.token_urlsafe(24)}\n@hew-lang:registry=http://127.0.0.1:9\n"
        )
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.addCleanup(self.stop)

    def stop(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join()

    def run_adapter(self):
        return subprocess.run(
            ["python3", str(ROOT / "scripts/release-channel.py"), "npm"],
            cwd=self.root,
            env=dict(
                PATH=os.environ["PATH"],
                HOME=str(self.root),
                GITHUB_SHA=SHA,
                VERSION=VERSION,
                DIST_TAG="latest",
                REGISTRY=self.server.url,
                NPM_CONFIG_FETCH_RETRIES="0",
                NPM_CONFIG_FETCH_TIMEOUT="2000",
                NPM_CONFIG_CACHE=str(self.root / "cache"),
                NPM_CONFIG_USERCONFIG=str(self.root / "user.npmrc"),
                NPM_CONFIG_GLOBALCONFIG=str(self.root / "global.npmrc"),
                NODE_AUTH_TOKEN="",
            ),
            capture_output=True,
            text=True,
        )

    def test_identical_existing_original_tarballs_skip_without_write(self):
        result = self.run_adapter()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.count("Verified identical tarball"), 2)
        self.assertEqual(self.server.writes, 0)

    def test_successful_publish_is_terminal_while_metadata_propagates(self):
        self.server.mode = "absent"
        self.server.delayed_metadata = True
        result = self.run_adapter()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.server.writes, 1)
        self.assertEqual(self.server.packages["wasm"], self.packages["wasm"])
        self.assertEqual(result.stdout.count("Verified identical tarball"), 1)
        self.assertIn(f"Published: @hew-lang/wasm@{VERSION}", result.stdout)
        self.assertEqual(self.server.published_reads, 0)

    def test_different_tarball_fails_without_overwrite(self):
        self.server.packages["wasm"] = package("wasm", payload=b"other")
        result = self.run_adapter()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Published bytes differ", result.stderr)
        self.assertEqual(self.server.writes, 0)

    def test_lost_publish_response_resumes_by_exact_bytes_without_another_write(self):
        self.server.mode = "absent"
        self.server.uncertain_publish = True
        first = self.run_adapter()
        self.assertNotEqual(first.returncode, 0)
        self.assertEqual(self.server.writes, 1)
        resumed = self.run_adapter()
        self.assertEqual(resumed.returncode, 0, resumed.stderr)
        self.assertEqual(resumed.stdout.count("Verified identical tarball"), 2)
        self.assertEqual(self.server.writes, 1)

    def test_unreadable_registry_fails_without_blind_publish(self):
        self.server.mode = "unreadable"
        result = self.run_adapter()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Cannot determine npm publication state", result.stderr)
        self.assertEqual(self.server.writes, 0)

    def test_wrong_producer_source_refuses_registry_work(self):
        (self.root / f"packages/hew-lang-wasm-{VERSION}.tgz").write_bytes(
            package("wasm", source="b" * 40)
        )
        result = self.run_adapter()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Producer package source/version mismatch", result.stderr)
        self.assertEqual(self.server.writes, 0)


class OciDelivery(unittest.TestCase):
    def setUp(self):
        self.raw = {
            arch: json.dumps({"schemaVersion": 2, "config": {"digest": arch}})
            for arch in ("amd64", "arm64")
        }
        self.expected = {
            arch: channel.digest(raw.encode()) for arch, raw in self.raw.items()
        }
        self.index = json.dumps(
            {
                "manifests": [
                    {"digest": value, "platform": {"os": "linux", "architecture": arch}}
                    for arch, value in self.expected.items()
                ]
            }
        )
        self.calls = []
        self.remote = self.index

    def fake(self, *args, check=True):
        self.calls.append(args)
        if args[:3] == ("skopeo", "inspect", "--raw"):
            ref = args[3]
            if ref.startswith("oci-archive:"):
                data = self.raw["arm64" if "arm64" in ref else "amd64"]
            elif "@sha256:" in ref:
                data = next(
                    raw
                    for raw in self.raw.values()
                    if channel.digest(raw.encode()) in ref
                )
            else:
                if self.remote is None:
                    return subprocess.CompletedProcess(args, 1, "", "manifest unknown")
                if self.remote == "unreadable":
                    return subprocess.CompletedProcess(args, 1, "", "unauthorized")
                data = self.remote
            return subprocess.CompletedProcess(args, 0, data, "")
        if args[:3] == ("skopeo", "inspect", "--config"):
            arch = "arm64" if "arm64" in args[3] else "amd64"
            data = {
                "architecture": arch,
                "os": "linux",
                "config": {
                    "Labels": {
                        "org.opencontainers.image.revision": SHA,
                        "org.opencontainers.image.version": VERSION,
                    }
                },
            }
            return subprocess.CompletedProcess(args, 0, json.dumps(data), "")
        if args[:4] == ("docker", "buildx", "imagetools", "create"):
            self.remote = self.index
        return subprocess.CompletedProcess(args, 0, "", "")

    def execute(self):
        with (
            tempfile.TemporaryDirectory() as tmp,
            patch.dict(
                os.environ,
                IMAGE="ghcr.io/hew-lang/hew",
                VERSION=VERSION,
                GITHUB_SHA=SHA,
                PRERELEASE="true",
                GITHUB_OUTPUT=f"{tmp}/output",
            ),
            patch.object(channel, "command", self.fake),
        ):
            channel.publish_ghcr()
            return Path(tmp, "output").read_text()

    def test_existing_index_and_children_verified_without_writes(self):
        self.assertIn(channel.digest(self.index.encode()), self.execute())
        self.assertFalse(
            any(args[1:2] == ("copy",) or args[0] == "docker" for args in self.calls)
        )

    def test_wrong_index_or_platform_fails_without_overwrite(self):
        self.remote = self.index.replace('"arm64"', '"s390x"')
        with self.assertRaisesRegex(RuntimeError, "index differs"):
            self.execute()
        self.assertFalse(
            any(args[0] == "docker" or args[1:2] == ("copy",) for args in self.calls)
        )

    def test_unreadable_is_not_absent(self):
        self.remote = "unreadable"
        with self.assertRaisesRegex(RuntimeError, "Cannot inspect"):
            self.execute()
        self.assertFalse(any(args[0] == "docker" for args in self.calls))

    def test_absent_push_verifies_final_index(self):
        self.remote = None
        self.assertIn("digest=sha256:", self.execute())
        self.assertEqual(sum(args[:2] == ("skopeo", "copy") for args in self.calls), 2)


class HomebrewDelivery(unittest.TestCase):
    def setUp(self):
        self.checksums = {
            target: str(i) * 64
            for i, target in enumerate(
                ("darwin-aarch64", "linux-x86_64", "linux-aarch64"), 1
            )
        }
        self.formula = f'  version "{VERSION}"\n' + "\n".join(
            f'url "https://github.com/hew-lang/hew/releases/download/v#{{version}}/hew-v#{{version}}-{target}.tar.gz"\nsha256 "{value}"'
            for target, value in self.checksums.items()
        )
        self.list_count = 0
        self.existing = False
        self.active = False
        self.return_url = True
        self.unreadable = False
        self.ancient = False
        self.ambiguous = False
        self.calls = []
        self.terminal = {
            "status": "completed",
            "conclusion": "success",
            "displayTitle": f"Update Hew {VERSION}",
        }

    def test_unavailable_macos_keeps_homebrew_unpublished_without_credentials(self):
        with tempfile.TemporaryDirectory() as tmp:
            previous = Path.cwd()
            os.chdir(tmp)
            try:
                Path("dist").mkdir()
                receipt = {
                    "version": VERSION,
                    "source": SHA,
                    "platforms": {
                        "darwin-aarch64": {
                            "state": "unavailable",
                        }
                    },
                }
                Path(f"dist/hew-v{VERSION}-platforms.json").write_text(
                    json.dumps(receipt)
                )
                with (
                    patch.dict(
                        os.environ, {"VERSION": VERSION, "GITHUB_SHA": SHA}, clear=True
                    ),
                    patch.object(channel, "command") as command,
                ):
                    channel.publish_homebrew()
                    command.assert_not_called()
                    receipt["source"] = "b" * 40
                    Path(f"dist/hew-v{VERSION}-platforms.json").write_text(
                        json.dumps(receipt)
                    )
                    with self.assertRaisesRegex(RuntimeError, "source/version"):
                        channel.publish_homebrew()
            finally:
                os.chdir(previous)

    def fake(self, *args, check=True):
        self.calls.append(args)
        if args[1:3] == ("run", "list"):
            self.list_count += 1
            result = (
                []
                if self.list_count == 1 and not self.existing
                else [
                    {
                        "databaseId": 42,
                        "headSha": SHA,
                        "url": "https://github.com/hew-lang/homebrew-hew/actions/runs/42",
                        "status": "in_progress" if self.active else "completed",
                        "conclusion": None if self.active else "success",
                        "displayTitle": "Update Formula"
                        if self.ambiguous
                        else f"Update Hew {VERSION}",
                    }
                ]
            )
            if self.ancient and "--created" not in args:
                result.append(
                    {
                        "databaseId": 1,
                        "status": "completed",
                        "conclusion": "success",
                        "displayTitle": "Update Formula",
                        "url": "https://example.invalid/expired",
                    }
                )
            if "--created" in args:
                self.assertEqual(
                    args[args.index("--created") + 1], ">=2026-10-21T00:00:00Z"
                )
        elif args[1:3] == ("run", "view"):
            if "--log" in args:
                raise RuntimeError("Expired downstream log")
            if self.unreadable:
                raise RuntimeError("Cannot read downstream outcome")
            result = self.terminal
        elif args[1:3] == ("release", "view"):
            result = {"publishedAt": "2026-10-21T00:00:00Z"}
        elif args[1:3] == ("workflow", "run"):
            return subprocess.CompletedProcess(
                args,
                0,
                "https://github.com/hew-lang/homebrew-hew/actions/runs/42\n"
                if self.return_url
                else "",
                "",
            )
        elif args[1:3] == ("api", "repos/hew-lang/homebrew-hew"):
            result = {"default_branch": "main"}
        elif args[1] == "api" and "/commits/" in args[2]:
            result = {"sha": SHA}
        elif args[1] == "api" and "/contents/" in args[2]:
            return subprocess.CompletedProcess(args, 0, self.formula, "")
        else:
            result = {}
        return subprocess.CompletedProcess(args, 0, json.dumps(result), "")

    def execute(self):
        with (
            tempfile.TemporaryDirectory() as tmp,
            contextlib.chdir(tmp),
            patch.dict(os.environ, VERSION=VERSION, GH_TOKEN="test"),
            patch.object(channel, "command", self.fake),
        ):
            Path("dist").mkdir()
            Path(f"dist/hew-v{VERSION}-checksums.txt").write_text(
                "\n".join(
                    f"{value}  hew-v{VERSION}-{target}.tar.gz"
                    for target, value in self.checksums.items()
                )
            )
            channel.publish_homebrew()

    def test_terminal_success_and_exact_both_formulas_required(self):
        self.execute()
        self.assertTrue(any(args[1:3] == ("run", "watch") for args in self.calls))
        self.assertEqual(
            sum(
                "/contents/Formula/" in args[2]
                for args in self.calls
                if args[1] == "api"
            ),
            2,
        )

    def test_existing_completion_and_exact_formula_skip_dispatch(self):
        self.existing = True
        self.execute()
        self.assertFalse(any(args[1:3] == ("workflow", "run") for args in self.calls))
        self.assertFalse(any(args[1:3] == ("run", "watch") for args in self.calls))

    def test_existing_active_run_is_awaited_without_another_dispatch(self):
        self.existing = self.active = True
        self.execute()
        self.assertTrue(any(args[1:3] == ("run", "watch") for args in self.calls))
        self.assertFalse(any(args[1:3] == ("workflow", "run") for args in self.calls))

    def test_existing_unreadable_outcome_blocks_dispatch(self):
        self.existing = self.unreadable = True
        with self.assertRaisesRegex(RuntimeError, "Cannot read"):
            self.execute()
        self.assertFalse(any(args[1:3] == ("workflow", "run") for args in self.calls))

    def test_expired_ancient_logs_are_not_consulted_before_dispatch(self):
        self.ancient = True
        self.execute()
        self.assertTrue(any(args[1:3] == ("workflow", "run") for args in self.calls))
        self.assertFalse(any("--log" in args for args in self.calls))

    def test_recent_legacy_run_is_ambiguous_and_blocks_dispatch(self):
        self.existing = self.ambiguous = True
        with self.assertRaisesRegex(RuntimeError, "reconcile before redispatch"):
            self.execute()
        self.assertFalse(any(args[1:3] == ("workflow", "run") for args in self.calls))

    def test_terminal_run_for_another_version_is_not_completion(self):
        self.terminal["displayTitle"] = "Update Hew 1.2.2"
        with self.assertRaisesRegex(RuntimeError, "version differs"):
            self.execute()

    def test_direct_dispatch_url_preserves_exact_run_without_discovery(self):
        self.execute()
        self.assertEqual(self.list_count, 1)
        watch = next(args for args in self.calls if args[1:3] == ("run", "watch"))
        self.assertEqual(watch[3], "42")

    def test_missing_dispatch_url_uses_unique_new_run(self):
        self.return_url = False
        self.execute()
        self.assertEqual(self.list_count, 2)

    def test_dispatch_or_nonterminal_success_does_not_complete(self):
        self.terminal = {"status": "in_progress", "conclusion": None}
        with self.assertRaisesRegex(RuntimeError, "did not complete"):
            self.execute()
        self.assertFalse(
            any("/contents/" in args[2] for args in self.calls if args[1] == "api")
        )

    def test_successful_run_with_wrong_formula_fails(self):
        self.existing = True
        self.formula = self.formula.replace('version "1.2.3"', 'version "1.2.2"')
        with self.assertRaisesRegex(RuntimeError, "version differs"):
            self.execute()
        self.assertFalse(any(args[1:3] == ("workflow", "run") for args in self.calls))

    def test_correct_version_wrong_source_or_checksum_fails(self):
        for text in (
            self.formula.replace("github.com/hew-lang/hew/", "example.invalid/"),
            self.formula.replace("1" * 64, "f" * 64),
        ):
            with (
                self.subTest(text=text),
                self.assertRaisesRegex(RuntimeError, "source/checksums"),
            ):
                channel.verify_formula(text, VERSION, self.checksums)


if __name__ == "__main__":
    unittest.main()
