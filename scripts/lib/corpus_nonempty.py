#!/usr/bin/env python3
"""Reject empty selections before corpus gates compare or execute them."""

from __future__ import annotations

import sys


def check_nonempty(key: str, actual: int, context: str = "") -> str | None:
    label = f"{key} ({context})" if context else key
    if actual < 0:
        return f"corpus selection: {label} count must be non-negative, got {actual}"
    if actual == 0:
        return f"corpus selection: {label} selected nothing"
    return None


def assert_nonempty(key: str, actual: int, context: str = "") -> None:
    error = check_nonempty(key, actual, context)
    if error is not None:
        print(error, file=sys.stderr)
        raise SystemExit(1)


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        print(
            "usage: python3 scripts/lib/corpus_nonempty.py <label> <actual-count> [context]",
            file=sys.stderr,
        )
        return 2
    try:
        actual = int(argv[1])
    except ValueError:
        print(
            f"corpus selection: {argv[0]} count must be an integer, got {argv[1]!r}",
            file=sys.stderr,
        )
        return 1
    error = check_nonempty(argv[0], actual, argv[2] if len(argv) > 2 else "")
    if error is not None:
        print(error, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
