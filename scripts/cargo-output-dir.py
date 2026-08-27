#!/usr/bin/env python3
"""Resolve the directory Cargo actually writes this workspace's artifacts into.

The Makefile used to hard-code `target/debug`. Cargo does not: it honours
`CARGO_TARGET_DIR`, `build.target-dir` from every `.cargo/config.toml` in
scope, `CARGO_BUILD_TARGET`, `build.target`, and an explicit `--target`. With
any of those set, a rule that builds through Cargo but then touches, inspects
or installs `target/debug` is looking at a different file than the one Cargo
just wrote — which is exactly how a month-old `libhew.a` in a shared scratch
target directory got certified as fresh.

Usage:
  cargo-output-dir.py [--root | --triple | --native] [--profile <dir>]
                      [--target <triple>]

  --root      print the target directory root (no triple, no profile)
  --triple    print the effective native target triple, empty for the host
  --native    print root[/triple] (default)
  --profile   append a profile directory name (debug, release, release-lib, ...)
  --target    an explicit `--target` triple, which outranks the environment

Paths inside the repository are printed relative to the repository root so the
default output stays exactly `target`, keeping every relative path and symlink
in the Makefile working; anything outside is printed absolute.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
import tomllib
from pathlib import Path
from typing import Any

REPO_ROOT = Path(__file__).resolve().parent.parent


def die(message: str) -> "None":
    print(f"error: {message}", file=sys.stderr)
    raise SystemExit(1)


def _configured_target(config: dict[str, Any], path: Path) -> str | None:
    """Validate and return Cargo's optional scalar build target."""
    build = config.get("build")
    if build is None:
        return None
    if not isinstance(build, dict):
        die(f"{path} has a non-table build configuration")
    target = build.get("target")
    if target is None:
        return None
    if isinstance(target, str):
        if not target:
            die(f"{path} has an empty build.target")
        return target
    if isinstance(target, list) and all(isinstance(item, str) for item in target):
        if len(target) == 1 and target[0]:
            return target[0]
        die(
            f"{path} configures {len(target)} build targets; there is no single "
            "Cargo output directory to check. Build one target at a time."
        )
    die(f"{path} has a non-string build.target value")


def _load_toml(text: str, path: Path) -> dict[str, Any]:
    return tomllib.loads(text)


def target_root() -> Path:
    """The `target/` equivalent Cargo writes into, absolute.

    `CARGO_TARGET_DIR` outranks configuration, so it short-circuits; otherwise
    `cargo metadata` is authoritative because it applies the whole
    `.cargo/config.toml` hierarchy that this script would only approximate.
    """
    env = os.environ.get("CARGO_TARGET_DIR")
    if env:
        # Cargo resolves a relative CARGO_TARGET_DIR against its own working
        # directory, and every Cargo invocation in the build runs from the
        # repository root. Normalise without resolving symlinks so the path is
        # spelled the way the caller and Cargo spell it.
        return Path(os.path.abspath(REPO_ROOT / env))

    failures: list[str] = []
    for extra in (["--offline"], []):
        try:
            out = subprocess.run(
                ["cargo", "metadata", "--no-deps", "--format-version", "1", *extra],
                cwd=REPO_ROOT,
                capture_output=True,
                check=False,
            )
        except OSError as err:
            die(f"cannot execute cargo metadata: {err}")
        if out.returncode == 0:
            try:
                target_directory = json.loads(out.stdout)["target_directory"]
            except (ValueError, KeyError, TypeError) as err:
                die(f"cargo metadata returned no valid target_directory: {err}")
            if not isinstance(target_directory, str) or not target_directory:
                die("cargo metadata returned an empty or non-string target_directory")
            return Path(target_directory).resolve()
        mode = "offline" if extra else "online"
        stderr = out.stderr.decode("utf-8", errors="replace").strip()
        failures.append(f"{mode}: exit {out.returncode}: {stderr or '<no stderr>'}")

    die(
        "cargo metadata could not resolve the artifact output directory; "
        + " | ".join(failures)
    )


def config_build_target() -> str:
    """`build.target` from Cargo's configuration search path."""
    candidates: list[Path] = []
    directory = REPO_ROOT
    while True:
        candidates.append(directory / ".cargo" / "config.toml")
        candidates.append(directory / ".cargo" / "config")
        if directory.parent == directory:
            break
        directory = directory.parent
    cargo_home = os.environ.get("CARGO_HOME")
    home = Path(cargo_home) if cargo_home else Path.home() / ".cargo"
    candidates.append(home / "config.toml")
    candidates.append(home / "config")

    for path in candidates:
        try:
            text = path.read_text(encoding="utf-8")
        except FileNotFoundError:
            continue
        except (OSError, UnicodeError) as err:
            die(f"cannot read {path}: {err}")
        try:
            config = _load_toml(text, path)
        except ValueError as err:
            die(str(err))
        target = _configured_target(config, path)
        if target is not None:
            return target
    return ""


def triple(explicit: str) -> str:
    """The triple whose subdirectory Cargo writes into, empty for the host.

    Precedence follows Cargo: an explicit `--target` beats `CARGO_BUILD_TARGET`,
    which beats `build.target` in configuration.
    """
    if explicit:
        return explicit
    env = os.environ.get("CARGO_BUILD_TARGET")
    if env:
        return env
    return config_build_target()


def display(path: Path) -> str:
    """Relative to the repository root when inside it, absolute otherwise.

    Symlinks are followed only for the containment test: a worktree reached
    through a symlinked path must still print the plain relative `target`.
    """
    real_root = Path(os.path.realpath(REPO_ROOT))
    real_path = Path(os.path.realpath(path))
    try:
        inside = real_path.relative_to(real_root)
    except ValueError:
        return str(path)
    return str(inside) if str(inside) != "." else "."


def main(argv: list[str]) -> int:
    mode = "native"
    profile = ""
    explicit_target = ""

    args = list(argv)
    while args:
        arg = args.pop(0)
        if arg in ("--root", "--triple", "--native"):
            mode = arg[2:]
        elif arg == "--profile":
            if not args:
                die("--profile requires a directory name")
            profile = args.pop(0)
        elif arg == "--target":
            if not args:
                die("--target requires a triple")
            explicit_target = args.pop(0)
        else:
            die(f"unknown argument: {arg}")

    if mode == "triple":
        print(triple(explicit_target))
        return 0

    path = target_root()
    if mode == "native":
        found = triple(explicit_target)
        if found:
            path = path / found
    if profile:
        path = path / profile
    print(display(path))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
