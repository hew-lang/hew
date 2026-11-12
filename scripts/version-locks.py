#!/usr/bin/env python3
"""Refresh or verify tracked Cargo locks containing workspace packages."""

import glob
import subprocess
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def load(path: Path) -> dict:
    with path.open("rb") as file:
        return tomllib.load(file)


def main() -> None:
    if sys.argv[1:] not in (["--update"], ["--check"]):
        raise SystemExit("usage: version-locks.py --update|--check")
    workspace = load(ROOT / "Cargo.toml")["workspace"]
    version = workspace["package"]["version"]
    names = set()
    for pattern in workspace["members"]:
        for member in glob.glob(str(ROOT / pattern)):
            manifest = Path(member) / "Cargo.toml"
            if manifest.is_file():
                names.add(load(manifest)["package"]["name"])
    locks = subprocess.check_output(
        ["git", "ls-files", "*Cargo.lock"], cwd=ROOT, text=True
    ).splitlines()
    failures = []
    for lock in locks:
        path = ROOT / lock
        packages = [
            package
            for package in load(path)["package"]
            if package["name"] in names and "source" not in package
        ]
        if not packages:
            continue
        if sys.argv[1] == "--update":
            subprocess.run(
                ["cargo", "update", "--workspace", "--manifest-path", str(path.parent / "Cargo.toml")],
                cwd=ROOT,
                check=True,
            )
        else:
            failures.extend(
                f"{lock}: {package['name']} is {package['version']}, expected {version}"
                for package in packages
                if package["version"] != version
            )
    if failures:
        raise SystemExit("\n".join(failures))


if __name__ == "__main__":
    main()
