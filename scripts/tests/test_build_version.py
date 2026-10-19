#!/usr/bin/env python3
"""Exercise the real version build script across linked source worktrees."""

from __future__ import annotations

import os
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


if __name__ == "__main__":
    unittest.main()
