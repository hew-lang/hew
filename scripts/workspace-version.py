#!/usr/bin/env python3
"""Print the repository workspace version from Cargo.toml."""

from __future__ import annotations

import re
import sys
import tomllib
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent


def main() -> int:
    manifest_path = SCRIPT_DIR.parent / "Cargo.toml"
    with manifest_path.open("rb") as manifest_file:
        manifest = tomllib.load(manifest_file)
    version = manifest.get("workspace", {}).get("package", {}).get("version")
    if (
        not isinstance(version, str)
        or re.fullmatch(
            r"[0-9]+\.[0-9]+\.[0-9]+(?:-[0-9A-Za-z][0-9A-Za-z.-]*)?", version
        )
        is None
    ):
        raise SystemExit(
            f"{manifest_path}: workspace.package.version is missing or invalid"
        )
    print(version)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
