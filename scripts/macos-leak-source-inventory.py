#!/usr/bin/env python3
"""Discover hew-cli test binaries that execute allocator/leak probes."""

from __future__ import annotations

import argparse
import re
from pathlib import Path

CALL = re.compile(
    r"\b(?:run_under_malloc_scribble|assert_frame_slope_below_[A-Za-z0-9_]*|"
    r"measure_leaks[A-Za-z0-9_]*)\s*\("
)


def mask_comments_and_literals(source: str) -> str:
    """Preserve positions while hiding Rust comments and string/char literals."""

    result = list(source)
    index = 0
    length = len(source)

    def mask(start: int, end: int) -> None:
        for position in range(start, min(end, length)):
            if result[position] != "\n":
                result[position] = " "

    while index < length:
        if source.startswith("//", index):
            end = source.find("\n", index + 2)
            end = length if end < 0 else end
            mask(index, end)
            index = end
            continue
        if source.startswith("/*", index):
            depth = 1
            end = index + 2
            while end < length and depth:
                if source.startswith("/*", end):
                    depth += 1
                    end += 2
                elif source.startswith("*/", end):
                    depth -= 1
                    end += 2
                else:
                    end += 1
            mask(index, end)
            index = end
            continue

        raw = re.match(r"(?:br|r)(?P<hashes>#{0,255})\"", source[index:])
        if raw:
            terminator = '"' + raw.group("hashes")
            content_start = index + raw.end()
            close = source.find(terminator, content_start)
            end = length if close < 0 else close + len(terminator)
            mask(index, end)
            index = end
            continue

        quote_start = index + 1 if source.startswith('b"', index) else index
        if quote_start < length and source[quote_start] == '"':
            end = quote_start + 1
            escaped = False
            while end < length:
                char = source[end]
                end += 1
                if char == '"' and not escaped:
                    break
                escaped = char == "\\" and not escaped
                if char != "\\":
                    escaped = False
            mask(index, end)
            index = end
            continue

        if source[index] == "'":
            # A Rust lifetime (`'a`) has no closing quote. Only mask a char
            # literal when a closing quote is present before whitespace.
            end = index + 1
            escaped = False
            while end < length and source[end] not in "\n\r\t ":
                char = source[end]
                end += 1
                if char == "'" and not escaped:
                    mask(index, end)
                    index = end
                    break
                escaped = char == "\\" and not escaped
                if char != "\\":
                    escaped = False
            else:
                index += 1
            continue
        index += 1
    return "".join(result)


def contains_probe_call(path: Path) -> bool:
    masked = mask_comments_and_literals(path.read_text())
    for match in CALL.finditer(masked):
        prefix = masked[max(0, match.start() - 8) : match.start()]
        if not re.search(r"\bfn\s*$", prefix):
            return True
    return False


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--tests-dir", required=True, type=Path)
    args = parser.parse_args()
    if not args.tests_dir.is_dir():
        raise SystemExit(f"test source directory does not exist: {args.tests_dir}")
    discovered = sorted(
        path.stem for path in args.tests_dir.glob("*.rs") if contains_probe_call(path)
    )
    if not discovered:
        raise SystemExit(
            "no allocator/leak execution calls discovered; refusing empty authority"
        )
    print("\n".join(discovered))


if __name__ == "__main__":
    main()
