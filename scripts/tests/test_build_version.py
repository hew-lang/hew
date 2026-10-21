#!/usr/bin/env python3
"""Exercise the real version build script across linked source worktrees."""

from __future__ import annotations

import os
import shutil
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


class SharedTargetVersion(unittest.TestCase):
    def test_source_worktree_refs_and_dirty_inputs(self) -> None:
        with tempfile.TemporaryDirectory(prefix="hew-version-") as temporary:
            base = Path(temporary)
            first = base / "first source"
            second = base / "second source"
            first.mkdir()
            crate = first / "hew-compile"
            (crate / "src").mkdir(parents=True)
            (crate / "Cargo.toml").write_text(
                '[package]\nname = "hew-version-probe"\nversion = "0.6.1"\nedition = "2024"\n'
            )
            (crate / "build.rs").write_bytes(
                (ROOT / "hew-compile/build.rs").read_bytes()
            )
            (crate / "src/lib.rs").write_text(
                'pub const VERSION: &str = env!("HEW_VERSION");\n'
            )
            cli = first / "hew-cli"
            (cli / "src").mkdir(parents=True)
            (first / "Cargo.toml").write_text(
                '[workspace]\nmembers = ["hew-compile", "hew-cli"]\nresolver = "3"\n'
            )
            (cli / "Cargo.toml").write_text(
                '[package]\nname = "hew-version-cli"\nversion = "0.6.1"\nedition = "2024"\n'
                '[dependencies]\nhew-version-probe = { path = "../hew-compile" }\n'
            )
            (cli / "src/main.rs").write_text(
                'fn main() { println!("{}|{}", hew_version_probe::VERSION, include_str!("../../hew-compile/marker").trim()); }\n'
            )
            (crate / "marker").write_text("first\n")
            (first / "notes").write_text("unchanged\n")
            (first / "Makefile").write_text(
                'export HEW_BUILD_SOURCE_ROOT := $(CURDIR)\nprobe:\n\tcargo run --quiet --offline --manifest-path hew-cli/Cargo.toml --target-dir "$(PROBE_TARGET)"\n'
            )

            def run(directory: Path, *arguments: str) -> str:
                result = subprocess.run(
                    arguments,
                    cwd=directory,
                    text=True,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    timeout=60,
                )
                self.assertEqual(
                    result.returncode,
                    0,
                    f"{arguments!r}\n{result.stdout}\n{result.stderr}",
                )
                return result.stdout.strip()

            def git(directory: Path, *arguments: str) -> str:
                return run(directory, "git", *arguments)

            def commit(directory: Path, subject: str) -> None:
                git(
                    directory,
                    "-c",
                    "user.name=Test",
                    "-c",
                    "user.email=test@example.invalid",
                    "commit",
                    "--no-gpg-sign",
                    "--allow-empty",
                    "-qm",
                    subject,
                )

            git(first, "init", "--quiet", "--initial-branch=main")
            git(first, "add", ".")
            commit(first, "initial")
            git(first, "tag", "--no-sign", "v0.6.1")
            git(first, "worktree", "add", "--quiet", "-b", "second", str(second))
            (second / "hew-compile/marker").write_text("second\n")
            git(second, "add", "hew-compile/marker")
            commit(second, "second source")

            # Git checkouts have different timestamps. Keep build-script inputs
            # identical so a timestamp accident cannot conceal target reuse.
            for name in ["build.rs", "Cargo.toml"]:
                stat = (crate / name).stat()
                os.utime(
                    second / "hew-compile" / name,
                    ns=(stat.st_atime_ns, stat.st_mtime_ns),
                )

            def probe(directory: Path, expected: str) -> None:
                observed = run(
                    directory,
                    "make",
                    "--no-print-directory",
                    "-s",
                    "probe",
                    f"PROBE_TARGET={base / 'shared target'}",
                )
                self.assertEqual(observed, expected)
                print(f"PASS {directory.name}: {observed}", flush=True)

            second_sha = git(second, "rev-parse", "--short", "HEAD")
            probe(second, f"0.6.1-dev.1+{second_sha}|second")
            # Both source trees predate the first compilation. Reuse the same
            # target when moving back to the older checkout, then forward.
            # Force the CLI source body to rebuild independently of metadata.
            os.utime(first / "hew-cli/src/main.rs", None)
            probe(first, "0.6.1|first")
            probe(second, f"0.6.1-dev.1+{second_sha}|second")
            git(second, "pack-refs", "--all", "--prune")
            commit(second, "advance active ref")
            third_sha = git(second, "rev-parse", "--short", "HEAD")
            probe(second, f"0.6.1-dev.2+{third_sha}|second")
            git(second, "checkout", "--quiet", "--detach", "HEAD~1")
            probe(second, f"0.6.1-dev.1+{second_sha}|second")
            (second / "hew-compile/marker").write_text("edited\n")
            probe(second, f"0.6.1-dev.1+{second_sha}.dirty|edited")
            git(second, "add", "hew-compile/marker")
            probe(second, f"0.6.1-dev.1+{second_sha}.dirty|edited")
            (second / "hew-compile/marker").write_text("second\n")
            git(second, "add", "hew-compile/marker")
            probe(second, f"0.6.1-dev.1+{second_sha}|second")
            (second / "notes").write_text("tracked non-Rust edit\n")
            probe(second, f"0.6.1-dev.1+{second_sha}.dirty|second")
            probe(first, "0.6.1|first")

            (second / "notes").write_text("unchanged\n")
            git(second, "tag", "--no-sign", "toolchain-preview")
            git(second, "tag", "--no-sign", "v05-int-tip")

            def open_development_version(previous: str, following: str) -> str:
                for name in ["hew-compile/Cargo.toml", "hew-cli/Cargo.toml"]:
                    manifest = second / name
                    manifest.write_text(
                        manifest.read_text().replace(
                            f'version = "{previous}"', f'version = "{following}"'
                        )
                    )
                git(second, "add", "hew-compile/Cargo.toml", "hew-cli/Cargo.toml")
                commit(second, f"open development {following}")
                return git(second, "rev-parse", "--short", "HEAD")

            development_sha = open_development_version("0.6.1", "0.6.2")
            development = f"0.6.2-dev.2+{development_sha}|second"
            # Count from the last Hew release, never an unrelated toolchain tag.
            probe(second, development)

            # A tag changes identity without changing source, HEAD or the index.
            git(second, "tag", "--no-sign", "v0.6.2")
            probe(second, "0.6.2|second")
            git(second, "pack-refs", "--all", "--prune")
            probe(second, "0.6.2|second")
            git(second, "tag", "--delete", "v0.6.2")
            probe(second, development)
            git(second, "tag", "--no-sign", "v0.6.2")
            probe(second, "0.6.2|second")

            following_sha = open_development_version("0.6.2", "0.6.3")
            probe(second, f"0.6.3-dev.1+{following_sha}|second")
            probe(first, "0.6.1|first")

    def test_embedded_std_inventory_follows_source_root(self) -> None:
        with tempfile.TemporaryDirectory(prefix="hew-embedded-std-") as temporary:
            base = Path(temporary)
            first = base / "first source"
            second = base / "second source"
            crate = first / "hew-wasm"
            (crate / "src").mkdir(parents=True)
            (first / "std").mkdir()
            (crate / "Cargo.toml").write_text(
                '[package]\nname = "hew-embedded-std-probe"\nversion = "0.1.0"\nedition = "2024"\n'
            )
            (crate / "build.rs").write_bytes((ROOT / "hew-wasm/build.rs").read_bytes())
            (crate / "src/main.rs").write_text(
                'include!(concat!(env!("OUT_DIR"), "/std_sources.rs"));\n'
                'fn main() { for (path, text) in STD_SOURCES { println!("{path}|{}", text.trim()); } }\n'
            )
            (first / "Makefile").write_text(
                "export HEW_BUILD_SOURCE_ROOT := $(CURDIR)\nprobe:\n"
                '\tcargo run --quiet --offline --manifest-path hew-wasm/Cargo.toml --target-dir "$(PROBE_TARGET)"\n'
            )
            (first / "std/first.hew").write_text("first source\n")
            shutil.copytree(first, second, copy_function=shutil.copy2)
            (second / "std/first.hew").unlink()
            (second / "std/second.hew").write_text("second source\n")

            # Both inventories predate compilation and all Rust inputs retain
            # identical timestamps. Only the exported source root changes.
            for directory, expected in [
                (second, "./std/second.hew|second source"),
                (first, "./std/first.hew|first source"),
                (second, "./std/second.hew|second source"),
            ]:
                result = subprocess.run(
                    [
                        "make",
                        "--no-print-directory",
                        "-s",
                        "probe",
                        f"PROBE_TARGET={base / 'shared target'}",
                    ],
                    cwd=directory,
                    text=True,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    timeout=60,
                )
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual(result.stdout.strip(), expected)
                print(f"PASS embedded std {directory.name}: {expected}", flush=True)


if __name__ == "__main__":
    unittest.main()
