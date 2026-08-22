#!/usr/bin/env python3
"""Import-safe entry point for the structural authority audit self-test."""

from __future__ import annotations

import runpy
from pathlib import Path


SELFTEST = (
    Path(__file__).resolve().parents[1] / "structural-authority-audit-selftest.py"
)


def main() -> None:
    runpy.run_path(str(SELFTEST), run_name="__main__")


if __name__ == "__main__":
    main()
