#!/usr/bin/env python3
"""Generate and verify examples/playground/manifest.json."""

from __future__ import annotations

import argparse
import difflib
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PLAYGROUND_DIR = ROOT / "examples" / "playground"
OUTPUT_FILE = PLAYGROUND_DIR / "manifest.json"
HEADER_PATTERN = re.compile(r"^//!\s*@(?P<key>name|description)\s+(?P<value>.+?)\s*$")
CATEGORY_ORDER = ("basics", "concurrency", "types")
# Keep the checked-in manifest aligned with the current curated snippet order.
EXAMPLE_ORDER = {
    "basics": (
        "hello_world",
        "fibonacci",
        "higher_order_functions",
        "string_interpolation",
    ),
    "concurrency": (
        "actor_pipeline",
        "async_await",
        "counter_actor",
        "supervisor",
    ),
    "types": (
        "collections",
        "pattern_matching",
        "wire_types",
    ),
}


def curated_source_paths() -> list[Path]:
    curated_categories = set(CATEGORY_ORDER)
    configured_categories = set(EXAMPLE_ORDER)

    missing_categories = sorted(curated_categories - configured_categories)
    if missing_categories:
        raise SystemExit(
            "error: curated category list missing EXAMPLE_ORDER entries for: "
            + ", ".join(missing_categories)
        )

    unexpected_categories = sorted(configured_categories - curated_categories)
    if unexpected_categories:
        raise SystemExit(
            "error: EXAMPLE_ORDER contains unexpected categories: "
            + ", ".join(unexpected_categories)
        )

    curated_paths: list[Path] = []
    for category in CATEGORY_ORDER:
        for example in EXAMPLE_ORDER[category]:
            curated_paths.append(PLAYGROUND_DIR / category / f"{example}.hew")

    return curated_paths


def parse_header_metadata(source_path: Path) -> dict[str, str]:
    metadata: dict[str, str] = {}

    for line in source_path.read_text().splitlines():
        stripped = line.strip()
        if not stripped:
            if metadata:
                break
            continue
        if not stripped.startswith("//!"):
            break

        match = HEADER_PATTERN.match(stripped)
        if match:
            metadata[match.group("key")] = match.group("value")

    missing = [key for key in ("name", "description") if key not in metadata]
    if missing:
        missing_str = ", ".join(missing)
        rel_path = source_path.relative_to(ROOT)
        raise SystemExit(f"error: {rel_path} missing header metadata: {missing_str}")

    return metadata


def fail_on_curated_scope_mismatch(curated_paths: list[Path]) -> None:
    curated_rel_paths = {
        source_path.relative_to(ROOT).as_posix() for source_path in curated_paths
    }
    actual_rel_paths = {
        source_path.relative_to(ROOT).as_posix()
        for source_path in PLAYGROUND_DIR.rglob("*.hew")
    }

    missing_sources = sorted(curated_rel_paths - actual_rel_paths)
    unexpected_sources = sorted(actual_rel_paths - curated_rel_paths)
    if not missing_sources and not unexpected_sources:
        return

    details = []
    if missing_sources:
        details.append(
            "missing curated playground snippets:\n  - "
            + "\n  - ".join(missing_sources)
        )
    if unexpected_sources:
        details.append(
            "unexpected playground snippets outside the curated 11:\n  - "
            + "\n  - ".join(unexpected_sources)
        )

    raise SystemExit(
        "error: examples/playground no longer matches the curated manifest scope\n"
        + "\n".join(details)
    )


def build_manifest_entries() -> list[dict[str, str]]:
    curated_paths = curated_source_paths()
    fail_on_curated_scope_mismatch(curated_paths)

    entries: list[dict[str, str]] = []
    for source_path in curated_paths:
        if not source_path.is_file():
            rel_path = source_path.relative_to(ROOT)
            raise SystemExit(f"error: missing curated playground snippet: {rel_path}")
        expected_path = source_path.with_suffix(".expected")
        if not expected_path.is_file():
            rel_path = expected_path.relative_to(ROOT)
            raise SystemExit(f"error: missing expected output file for {rel_path}")

        metadata = parse_header_metadata(source_path)
        category = source_path.parent.name

        entries.append(
            {
                "id": f"{category}/{source_path.stem}",
                "category": category,
                "name": metadata["name"],
                "description": metadata["description"],
                "source_path": source_path.relative_to(PLAYGROUND_DIR).as_posix(),
                "expected_path": expected_path.relative_to(PLAYGROUND_DIR).as_posix(),
            }
        )

    if not entries:
        raise SystemExit(
            "error: no playground snippets found under examples/playground"
        )

    return entries


def render_manifest(entries: list[dict[str, str]]) -> str:
    return json.dumps(entries, indent=2) + "\n"


def check_manifest(rendered: str) -> int:
    rel_output = OUTPUT_FILE.relative_to(ROOT).as_posix()
    if not OUTPUT_FILE.is_file():
        print(
            f"error: {rel_output} is missing; run python3 scripts/gen-playground-manifest.py",
            file=sys.stderr,
        )
        return 1

    existing = OUTPUT_FILE.read_text()
    if existing == rendered:
        print(f"{rel_output} is up to date.")
        return 0

    diff = difflib.unified_diff(
        existing.splitlines(keepends=True),
        rendered.splitlines(keepends=True),
        fromfile=rel_output,
        tofile=f"{rel_output} (generated)",
    )
    sys.stdout.writelines(diff)
    print(
        "\nerror: playground manifest is stale; run python3 scripts/gen-playground-manifest.py",
        file=sys.stderr,
    )
    return 1


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--check",
        action="store_true",
        help="Verify that examples/playground/manifest.json is up to date",
    )
    args = parser.parse_args()

    entries = build_manifest_entries()
    rendered = render_manifest(entries)

    if args.check:
        return check_manifest(rendered)

    OUTPUT_FILE.write_text(rendered)
    print(
        f"Generated {OUTPUT_FILE.relative_to(ROOT)} with {len(entries)} playground snippets."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
