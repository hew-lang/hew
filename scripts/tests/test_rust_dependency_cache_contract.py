#!/usr/bin/env python3
"""Fail-closed contract for the source-stable Rust dependency cache."""

from __future__ import annotations

from pathlib import Path
import re
import unittest


ROOT = Path(__file__).resolve().parents[2]
ACTION = ROOT / ".github" / "actions" / "setup-rust-build" / "action.yml"

# The first sccache release whose GitHub Actions backend honours
# SCCACHE_GHA_RW_MODE (mozilla/sccache docs/GHA.md, v0.16.0). Below this, the
# read-only half of the policy silently does nothing and every pull request
# resumes writing ref-scoped objects into a 10 GB repository budget.
SCCACHE_GHA_READ_ONLY_SINCE = (0, 16, 0)

RW_MODE_EXPRESSION = (
    "${{ github.ref == 'refs/heads/main' && 'READ_WRITE' || 'READ_ONLY' }}"
)


def _versions(action: str) -> list[tuple[int, int, int]]:
    return [
        tuple(int(part) for part in match.groups())
        for match in re.finditer(r'version:\s*"v(\d+)\.(\d+)\.(\d+)"', action)
    ]


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

    # ── sccache: read everywhere, write from main ────────────────────────────

    def test_sccache_is_installed_on_every_ref(self) -> None:
        """A pull request that never installs sccache cannot read the cache.

        The previous shape gated all four sccache steps on
        `github.ref == 'refs/heads/main'`, which fixed the eviction by giving
        pull requests nothing at all. The eviction fix now lives in the mode,
        not in the absence of the tool.
        """
        installs = [
            line
            for line in self.action.splitlines()
            if "mozilla-actions/sccache-action@" in line
        ]
        self.assertTrue(installs, "no sccache install step found")
        block = self.action.split("sccache: workspace-crate compilation cache", 1)[1]
        block = block.split("uses: Swatinem/rust-cache@", 1)[0]
        for line in block.splitlines():
            stripped = line.strip()
            if stripped.startswith("if:") or stripped.startswith("- if:"):
                self.assertNotIn(
                    "refs/heads/main",
                    stripped,
                    "an sccache step is gated on the default branch again; "
                    "pull requests would install nothing and read nothing",
                )

    def test_the_read_write_mode_is_the_only_branch_policy(self) -> None:
        """One expression decides, and it decides both directions.

        A second spelling of the same policy is how two authorities for one
        number start; the mode is asserted by value, not by the absence of a
        guard somewhere else.
        """
        self.assertIn(f"SCCACHE_RW_MODE: {RW_MODE_EXPRESSION}", self.action)
        self.assertIn('echo "SCCACHE_GHA_RW_MODE=${SCCACHE_RW_MODE}"', self.action)
        # No step may hardcode a writable mode: that would let a pull request
        # write ref-scoped objects again without any branch condition.
        for line in self.action.splitlines():
            stripped = line.strip()
            if stripped.startswith("#"):
                continue
            if "SCCACHE_GHA_RW_MODE" in stripped and "SCCACHE_RW_MODE" not in stripped:
                self.fail(f"a second SCCACHE_GHA_RW_MODE authority: {stripped}")
        self.assertNotIn("SCCACHE_GHA_RW_MODE=READ_WRITE", self.action)

    def test_the_mode_expression_maps_main_to_write_and_everything_else_to_read(
        self,
    ) -> None:
        """Semantics of the ternary, evaluated the way Actions evaluates it."""

        def mode(ref: str) -> str:
            return "READ_WRITE" if ref == "refs/heads/main" else "READ_ONLY"

        self.assertIn("'READ_WRITE' || 'READ_ONLY'", RW_MODE_EXPRESSION)
        self.assertEqual(mode("refs/heads/main"), "READ_WRITE")
        for ref in (
            "refs/pull/3029/merge",
            "refs/heads/refactor/ci-modernization",
            "refs/tags/v0.6.0-rc2",
            "refs/heads/main-ish",
        ):
            self.assertEqual(mode(ref), "READ_ONLY", ref)

    def test_the_pinned_sccache_understands_the_read_only_mode(self) -> None:
        """A downgrade below 0.16.0 makes the read-only half a no-op.

        Compared numerically rather than by string, so `v0.9.10` cannot pass
        for being lexically greater than `v0.16.0`.
        """
        versions = _versions(self.action)
        self.assertTrue(versions, "no pinned sccache version found")
        for version in versions:
            self.assertGreaterEqual(
                version,
                SCCACHE_GHA_READ_ONLY_SINCE,
                f"sccache v{'.'.join(str(part) for part in version)} predates "
                "SCCACHE_GHA_RW_MODE; pull requests would write the cache",
            )
        self.assertEqual(
            len(set(versions)),
            1,
            "the retry ladder installs more than one sccache version",
        )

    def test_an_unreachable_cache_degrades_to_a_cold_compile(self) -> None:
        """A cache is a cost input. It must never become a correctness input."""
        self.assertIn('echo "SCCACHE_IGNORE_SERVER_IO_ERROR=1"', self.action)
        for line in self.action.splitlines():
            if "sccache-action@" in line:
                continue
            if line.strip().startswith("continue-on-error:"):
                self.assertIn("true", line)

    def test_no_secret_or_broad_target_tree_enters_the_cache(self) -> None:
        """The cache holds compiler invocations and the registry layer only."""
        self.assertNotIn("secrets.", self.action)
        self.assertNotIn("${{ secrets", self.action)
        self.assertIn('cache-workspace-crates: "false"', self.action)
        self.assertNotIn("path: target", self.action)

    def test_the_cache_reports_its_own_behaviour(self) -> None:
        """Mode, hits, and writes are readable from the run that produced them."""
        self.assertIn("Report sccache cache behaviour", self.action)
        summary = self.action.split("Report sccache cache behaviour", 1)[1]
        for token in ("mode:", "hits:", "misses:", "writes:", "GITHUB_STEP_SUMMARY"):
            self.assertIn(token, summary, token)
        self.assertIn("if: always()", summary.split("run: |", 1)[0])


if __name__ == "__main__":
    unittest.main()
