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


def example_sort_key(source_path: Path) -> tuple[int, int, str]:
    category = source_path.parent.name
    stem = source_path.stem

    try:
        category_index = CATEGORY_ORDER.index(category)
    except ValueError:
        category_index = len(CATEGORY_ORDER)

    category_examples = EXAMPLE_ORDER.get(category, ())
    try:
        example_index = category_examples.index(stem)
    except ValueError:
        example_index = len(category_examples)

    return (category_index, example_index, stem)


def build_manifest_entries() -> list[dict[str, str]]:
    entries: list[dict[str, str]] = []

    for source_path in sorted(PLAYGROUND_DIR.glob("*/*.hew"), key=example_sort_key):
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
                "source_path": source_path.relative_to(ROOT).as_posix(),
                "expected_path": expected_path.relative_to(ROOT).as_posix(),
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
