"""
Sample-path oracle for the CI `playground` path filter.

Reads the `playground:` filter stanza from `.github/workflows/ci.yml` and
checks that:
  - Upstream crate files (hew-analysis, hew-parser, hew-types, hew-lexer) set
    playground=true.
  - LSP fixture files (hew-lsp/tests/fixtures/**) set playground=true.
  - hew-wasm source files set playground=true.
  - Docs-only or unrelated paths (docs/**, README.md, hew-runtime/**) do NOT
    set playground=true.

This is a machine-checkable guard against path-filter regressions:
if upstream crates that hew-wasm depends on are removed from the filter,
a breaking change in e.g. hew-analysis would silently skip the playground CI
job.

Run standalone:
    python3 scripts/tests/test_playground_path_filter_oracle.py

Or via pytest:
    pytest scripts/tests/test_playground_path_filter_oracle.py -v
"""

from __future__ import annotations

import fnmatch
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CI_YML = ROOT / ".github" / "workflows" / "ci.yml"


def _extract_playground_patterns(ci_yml_text: str) -> list[str]:
    """Extract the list of path-glob strings from the playground: filter stanza."""
    # Find the playground: block inside dorny/paths-filter `filters:` section.
    # The YAML structure is:
    #   filters: |
    #     playground:
    #       - 'glob'
    #       ...
    #     <next-filter>:
    # We extract the indented lines immediately following `playground:`.
    lines = ci_yml_text.splitlines()
    patterns: list[str] = []
    in_playground = False
    for line in lines:
        stripped = line.strip()
        if stripped == "playground:":
            in_playground = True
            continue
        if in_playground:
            # A line starting with `- '...'` or `- "..."` is a pattern entry.
            m = re.match(r"^\s*-\s+'([^']+)'", line) or re.match(
                r'^\s*-\s+"([^"]+)"', line
            )
            if m:
                patterns.append(m.group(1))
            elif stripped and not stripped.startswith("-"):
                # Hit the next stanza or an unindented line — done.
                break
    return patterns


def path_matches_any(path: str, patterns: list[str]) -> bool:
    """Return True if `path` matches any of the glob patterns (fnmatch semantics).

    dorny/paths-filter uses minimatch (JS); we use fnmatch which handles the
    common cases `**` (multi-segment) and `*` (single segment) that appear in
    these filters.  For patterns ending in `/**`, we also test the prefix itself.
    """
    for pat in patterns:
        # Exact match.
        if fnmatch.fnmatch(path, pat):
            return True
        # Pattern like `hew-analysis/**` — also matches the directory prefix
        # component to behave like minimatch's directory globbing.
        if pat.endswith("/**"):
            prefix = pat[:-3]  # strip trailing /**
            if path.startswith(prefix + "/") or path == prefix:
                return True
    return False


# ── Test cases ────────────────────────────────────────────────────────────


# Paths that MUST trigger the playground filter.
MUST_TRIGGER: list[tuple[str, str]] = [
    # hew-wasm (direct crate)
    ("hew-wasm/src/lib.rs", "hew-wasm source must trigger playground"),
    (
        "hew-wasm/tests/v05_wasm_coverage.rs",
        "hew-wasm integration test must trigger playground",
    ),
    # Upstream analysis crate (hew-wasm depends on hew-analysis)
    ("hew-analysis/src/lib.rs", "hew-analysis source must trigger playground"),
    (
        "hew-analysis/src/analysis.rs",
        "hew-analysis analysis.rs must trigger playground",
    ),
    # Parser (hew-analysis -> hew-parser)
    ("hew-parser/src/lib.rs", "hew-parser source must trigger playground"),
    # Types (hew-parser -> hew-types)
    ("hew-types/src/lib.rs", "hew-types source must trigger playground"),
    # Lexer (hew-parser -> hew-lexer)
    ("hew-lexer/src/lib.rs", "hew-lexer source must trigger playground"),
    # LSP fixtures shared via include_str! in hew-wasm integration tests
    (
        "hew-lsp/tests/fixtures/v05_async_await.hew",
        "LSP fixture must trigger playground (used by hew-wasm tests)",
    ),
    (
        "hew-lsp/tests/fixtures/v05_machine_states.hew",
        "LSP fixture must trigger playground",
    ),
    # Playground manifest / examples
    ("examples/playground/some_example.hew", "playground examples must trigger"),
    (
        "scripts/gen-playground-manifest.py",
        "manifest generator must trigger playground",
    ),
    # Build metadata
    ("Cargo.toml", "root Cargo.toml must trigger playground"),
    ("Cargo.lock", "Cargo.lock must trigger playground"),
    ("Makefile", "Makefile must trigger playground"),
    (".github/workflows/ci.yml", "ci.yml self-change must trigger playground"),
]

# Paths that MUST NOT trigger the playground filter alone (docs-only or unrelated).
MUST_NOT_TRIGGER: list[tuple[str, str]] = [
    ("docs/spec.md", "docs-only change must not trigger playground"),
    ("README.md", "README change must not trigger playground"),
    ("CHANGELOG.md", "CHANGELOG change must not trigger playground"),
    # hew-runtime is not a hew-wasm dep
    (
        "hew-runtime/src/lib.rs",
        "hew-runtime is not a hew-wasm dep; must not trigger playground",
    ),
    # hew-codegen is not a hew-wasm dep
    (
        "hew-codegen/src/lib.rs",
        "hew-codegen is not a hew-wasm dep; must not trigger playground",
    ),
    # LSP source (not fixtures) is not in hew-wasm's dep tree
    (
        "hew-lsp/src/server/mod.rs",
        "hew-lsp source (non-fixture) must not trigger playground alone",
    ),
]


def run_oracle() -> bool:
    """Run the oracle; return True if all assertions pass."""
    ci_text = CI_YML.read_text()
    patterns = _extract_playground_patterns(ci_text)

    if not patterns:
        print(
            f"ERROR: could not parse any playground: patterns from {CI_YML}",
            file=sys.stderr,
        )
        return False

    print(f"playground filter patterns ({len(patterns)}):")
    for p in patterns:
        print(f"  {p}")
    print()

    failures: list[str] = []

    for path, reason in MUST_TRIGGER:
        if not path_matches_any(path, patterns):
            failures.append(f"  FAIL (should trigger): {path!r}  — {reason}")
        else:
            print(f"  OK (triggers):     {path!r}")

    for path, reason in MUST_NOT_TRIGGER:
        if path_matches_any(path, patterns):
            failures.append(f"  FAIL (false-trigger): {path!r}  — {reason}")
        else:
            print(f"  OK (no-trigger):   {path!r}")

    if failures:
        print("\nFAILURES:")
        for f in failures:
            print(f)
        return False

    print(
        f"\nAll {len(MUST_TRIGGER) + len(MUST_NOT_TRIGGER)} oracle assertions passed."
    )
    return True


# ── pytest-compatible test functions ─────────────────────────────────────────


def _get_patterns() -> list[str]:
    return _extract_playground_patterns(CI_YML.read_text())


def test_oracle_parses_patterns() -> None:
    """Oracle must be able to parse at least 5 playground filter patterns."""
    patterns = _get_patterns()
    assert len(patterns) >= 5, (
        f"Expected ≥5 playground filter patterns, got {len(patterns)}: {patterns}"
    )


def test_upstream_analysis_triggers_playground() -> None:
    patterns = _get_patterns()
    assert path_matches_any("hew-analysis/src/lib.rs", patterns), (
        "hew-analysis/** must be in the playground path filter"
    )


def test_upstream_parser_triggers_playground() -> None:
    patterns = _get_patterns()
    assert path_matches_any("hew-parser/src/lib.rs", patterns), (
        "hew-parser/** must be in the playground path filter"
    )


def test_upstream_types_triggers_playground() -> None:
    patterns = _get_patterns()
    assert path_matches_any("hew-types/src/lib.rs", patterns), (
        "hew-types/** must be in the playground path filter"
    )


def test_upstream_lexer_triggers_playground() -> None:
    patterns = _get_patterns()
    assert path_matches_any("hew-lexer/src/lib.rs", patterns), (
        "hew-lexer/** must be in the playground path filter"
    )


def test_lsp_fixture_triggers_playground() -> None:
    patterns = _get_patterns()
    assert path_matches_any("hew-lsp/tests/fixtures/v05_async_await.hew", patterns), (
        "hew-lsp/tests/fixtures/** must be in the playground path filter "
        "(fixtures are shared via include_str! in hew-wasm integration tests)"
    )


def test_hew_wasm_triggers_playground() -> None:
    patterns = _get_patterns()
    assert path_matches_any("hew-wasm/src/lib.rs", patterns), (
        "hew-wasm/** must be in the playground path filter"
    )


def test_docs_only_does_not_trigger_playground() -> None:
    patterns = _get_patterns()
    assert not path_matches_any("docs/spec.md", patterns), (
        "docs-only changes must not trigger the playground filter"
    )


def test_readme_does_not_trigger_playground() -> None:
    patterns = _get_patterns()
    assert not path_matches_any("README.md", patterns), (
        "README changes must not trigger the playground filter"
    )


def test_hew_runtime_does_not_trigger_playground() -> None:
    patterns = _get_patterns()
    assert not path_matches_any("hew-runtime/src/lib.rs", patterns), (
        "hew-runtime is not a hew-wasm dep; its changes must not trigger the playground filter"
    )


if __name__ == "__main__":
    sys.exit(0 if run_oracle() else 1)
