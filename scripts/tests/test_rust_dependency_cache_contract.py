#!/usr/bin/env python3
"""Fail-closed contract for the source-stable Rust dependency cache."""

from __future__ import annotations

from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[2]
ACTION = ROOT / ".github" / "actions" / "setup-rust-build" / "action.yml"


class RustDependencyCacheContractTests(unittest.TestCase):
    def setUp(self) -> None:
        self.action = ACTION.read_text(encoding="utf-8")

    def test_specialized_rust_cache_action_remains_pinned(self) -> None:
        self.assertIn(
            "uses: Swatinem/rust-cache@c19371144df3bb44fab255c43d04cbc2ab54d1c4",
            self.action,
        )

    def test_exact_key_depends_only_on_dependency_and_toolchain_authorities(
        self,
    ) -> None:
        expected = (
            "key: deps-${{ hashFiles('Cargo.lock', 'rust-toolchain.toml', "
            "'.cargo/config.toml') }}"
        )
        self.assertIn(expected, self.action)
        key_line = next(
            line.strip()
            for line in self.action.splitlines()
            if line.strip().startswith("key: deps-")
        )
        self.assertNotIn("**/*.rs", key_line)
        self.assertNotIn("Cargo.toml", key_line)

    def test_automatic_manifest_hash_is_disabled(self) -> None:
        self.assertIn('add-rust-environment-hash-key: "false"', self.action)

    def test_only_dependency_target_artifacts_are_saved(self) -> None:
        self.assertIn('cache-targets: "true"', self.action)
        self.assertIn('cache-all-crates: "false"', self.action)
        self.assertIn('cache-workspace-crates: "false"', self.action)
        self.assertIn('cache-bin: "false"', self.action)

    def test_only_the_default_branch_writes_the_dependency_cache(self) -> None:
        """Restoring is for everyone; saving is main's alone.

        Without this, every pull request job wrote entries scoped to its own
        ref, readable by no other branch, evicting the default-branch layer it
        was itself trying to read.
        """
        self.assertIn("save-if: ${{ github.ref == 'refs/heads/main' }}", self.action)


if __name__ == "__main__":
    unittest.main()
