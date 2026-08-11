#!/usr/bin/env python3
"""Run and aggregate stable partitions of the compiled-Hew test suites."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
from pathlib import Path
import re
import shutil
import subprocess
import sys
from typing import NoReturn


REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO_ROOT / "scripts" / "lib"))
from corpus_nonempty import assert_nonempty  # noqa: E402
from hew_test_inventory import (  # noqa: E402
    parse_junit,
    read_inventory,
)


PARTITION_RE = re.compile(r"^hash:([1-9][0-9]*)/([1-9][0-9]*)$")
CACHE_FORMAT = "compiled-hew-verdict-v1"
HOST_ENVIRONMENT = (
    "AR",
    "CC",
    "CFLAGS",
    "CI",
    "CPATH",
    "CXX",
    "DYLD_LIBRARY_PATH",
    "LANG",
    "LC_ALL",
    "LD",
    "LDFLAGS",
    "LD_LIBRARY_PATH",
    "LIBRARY_PATH",
    "MACOSX_DEPLOYMENT_TARGET",
    "PATH",
    "SDKROOT",
    "TZ",
)


def die(message: str) -> NoReturn:
    print(f"compiled-hew-shards: {message}", file=sys.stderr)
    raise SystemExit(1)


def run_command(
    command: list[str], output: Path, stderr: Path, environment: dict[str, str]
) -> int:
    with (
        output.open("w", encoding="utf-8") as stdout_handle,
        stderr.open("w", encoding="utf-8") as stderr_handle,
    ):
        result = subprocess.run(
            command,
            cwd=REPO_ROOT,
            env=environment,
            stdout=stdout_handle,
            stderr=stderr_handle,
        )
    return result.returncode


def hash_file(digest: "hashlib._Hash", path: Path) -> None:
    digest.update(str(path.relative_to(REPO_ROOT)).encode())
    digest.update(b"\0")
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    digest.update(b"\0")


def tool_identity(name: str) -> str:
    path = shutil.which(name)
    if path is None:
        return f"{name}=missing"
    result = subprocess.run(
        [path, "--version"],
        cwd=REPO_ROOT,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=False,
    )
    return f"{name}={path}\n{result.returncode}\n{result.stdout}"


def verdict_cache_key(compiler: Path, partition: str, optimization: str) -> str:
    digest = hashlib.sha256()
    digest.update(f"{CACHE_FORMAT}\0{partition}\0O{optimization}\0".encode())
    for artifact in (
        compiler,
        next(
            (
                compiler.parent / name
                for name in ("libhew.a", "hew.lib")
                if (compiler.parent / name).is_file()
            ),
            None,
        ),
        Path(__file__),
        REPO_ROOT / "scripts/lib/hew_junit.py",
    ):
        if artifact is None:
            die(f"no libhew archive beside compiler {compiler}")
        resolved = artifact.resolve()
        digest.update(str(resolved).encode())
        digest.update(b"\0")
        with resolved.open("rb") as handle:
            for block in iter(lambda: handle.read(1024 * 1024), b""):
                digest.update(block)
        digest.update(b"\0")

    tracked = subprocess.run(
        ["git", "ls-files", "*.hew", "*.toml", "*.lock"],
        cwd=REPO_ROOT,
        text=True,
        stdout=subprocess.PIPE,
        check=True,
    ).stdout.splitlines()
    for relative in sorted(tracked):
        path = REPO_ROOT / relative
        if path.is_file():
            hash_file(digest, path)

    excluded = {
        "HEW_BIN",
        "HEW_SHARD_OUTPUT_DIR",
        "HEW_VERDICT_CACHE_DIR",
    }
    semantic_environment = sorted(
        (name, value)
        for name, value in os.environ.items()
        if name.startswith("HEW_") and name not in excluded
    )
    semantic_environment.extend(
        (name, os.environ.get(name, "")) for name in HOST_ENVIRONMENT
    )
    semantic_environment.sort()
    identity = {
        "environment": semantic_environment,
        "image_os": os.environ.get("ImageOS", ""),
        "image_version": os.environ.get("ImageVersion", ""),
        "machine": platform.machine(),
        "platform": platform.platform(),
        "python": platform.python_version(),
        "tools": [tool_identity(name) for name in ("clang", "cc", "ld.lld")],
    }
    digest.update(json.dumps(identity, sort_keys=True).encode())
    return digest.hexdigest()


def restore_cached_run(
    cache: Path, report: Path, stderr: Path, expected: set[str]
) -> int | None:
    metadata_path = cache / "metadata.json"
    cached_report = cache / "report.xml"
    cached_stderr = cache / "stderr.log"
    try:
        metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
        returncode = int(metadata["returncode"])
        outcomes = parse_junit(cached_report)
    except (FileNotFoundError, KeyError, ValueError, json.JSONDecodeError):
        return None
    if metadata.get("format") != CACHE_FORMAT or set(outcomes) != expected:
        return None
    expected_returncode = 1 if "FAILED" in outcomes.values() else 0
    if returncode != expected_returncode:
        return None
    report.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(cached_report, report)
    if cached_stderr.is_file():
        shutil.copy2(cached_stderr, stderr)
    else:
        stderr.write_text("", encoding="utf-8")
    return returncode


def store_cached_run(cache: Path, report: Path, stderr: Path, returncode: int) -> None:
    cache.mkdir(parents=True, exist_ok=True)
    shutil.copy2(report, cache / "report.xml")
    shutil.copy2(stderr, cache / "stderr.log")
    (cache / "metadata.json").write_text(
        json.dumps(
            {"format": CACHE_FORMAT, "returncode": returncode},
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )


def run_shard(compiler: Path, partition: str, output_dir: Path) -> None:
    match = PARTITION_RE.fullmatch(partition)
    if match is None or int(match.group(1)) > int(match.group(2)):
        die(f"invalid partition {partition!r}; expected hash:SHARD/TOTAL")
    shard = int(match.group(1))
    output_dir.mkdir(parents=True, exist_ok=True)
    inventory = output_dir / f"hew-inventory-shard-{shard}.txt"
    list_result = subprocess.run(
        [str(compiler), "test", "tests/hew", "--list", "--partition", partition],
        cwd=REPO_ROOT,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if list_result.returncode:
        die(f"test listing failed for {partition}: {list_result.stderr.strip()}")
    inventory.write_text(list_result.stdout, encoding="utf-8")
    expected = set(read_inventory(inventory))

    metadata: dict[str, object] = {"partition": partition, "tests": len(expected)}
    cache_root = Path(
        os.environ.get("HEW_VERDICT_CACHE_DIR", REPO_ROOT / "target/hew-verdict-cache")
    )
    for optimization, label in (("0", "o0"), ("2", "o2")):
        report = output_dir / f"hew-{label}-shard-{shard}.xml"
        stderr = output_dir / f"hew-{label}-shard-{shard}.stderr.log"
        key = verdict_cache_key(compiler, partition, optimization)
        cache = cache_root / key
        returncode = restore_cached_run(cache, report, stderr, expected)
        if returncode is None:
            environment = dict(os.environ)
            environment["HEW_OPT_LEVEL"] = optimization
            returncode = run_command(
                [
                    str(compiler),
                    "test",
                    "tests/hew",
                    "--partition",
                    partition,
                    "--format",
                    "junit",
                ],
                report,
                stderr,
                environment,
            )
            store_cached_run(cache, report, stderr, returncode)
            metadata[f"{label}_cache"] = "miss"
        else:
            metadata[f"{label}_cache"] = "hit"
        outcomes = parse_junit(report)
        if set(outcomes) != expected:
            die(
                f"{label.upper()} report inventory differs from the listed {partition} inventory"
            )
        expected_returncode = 1 if "FAILED" in outcomes.values() else 0
        if returncode != expected_returncode:
            die(
                f"{label.upper()} runner exited {returncode}, expected {expected_returncode} "
                "from its JUnit outcomes"
            )
        metadata[f"{label}_returncode"] = returncode

    (output_dir / f"hew-shard-{shard}.json").write_text(
        json.dumps(metadata, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )


def load_shards(
    reports_dir: Path, full: set[str], shard_count: int
) -> tuple[dict[str, str], dict[str, str]]:
    owner: dict[str, int] = {}
    combined_o0: dict[str, str] = {}
    combined_o2: dict[str, str] = {}
    for shard in range(1, shard_count + 1):
        inventory_path = reports_dir / f"hew-inventory-shard-{shard}.txt"
        inventory = set(read_inventory(inventory_path))
        for identity in inventory:
            if identity in owner:
                die(
                    f"shard inventories overlap: {identity} appears in shards "
                    f"{owner[identity]} and {shard}"
                )
            owner[identity] = shard
        o0 = parse_junit(reports_dir / f"hew-o0-shard-{shard}.xml")
        o2 = parse_junit(reports_dir / f"hew-o2-shard-{shard}.xml")
        if set(o0) != inventory:
            die(f"O0 report for shard {shard} differs from its inventory")
        if set(o2) != inventory:
            die(f"O2 report for shard {shard} differs from its inventory")
        combined_o0.update(o0)
        combined_o2.update(o2)
    union = set(owner)
    if union != full:
        missing = sorted(full - union)
        extra = sorted(union - full)
        die(
            "union of shard inventories does not equal the full inventory: "
            f"missing={missing[:5]} extra={extra[:5]}"
        )
    return combined_o0, combined_o2


def aggregate(
    mode: str,
    reports_dir: Path,
    full_inventory: Path,
    shard_count: int,
) -> None:
    if shard_count < 2:
        die("shard count must be at least two")
    full = set(read_inventory(full_inventory))
    o0, o2 = load_shards(reports_dir, full, shard_count)
    label = "hew-suite-tests" if mode == "ratchet" else "o2-differential-outcomes"
    assert_nonempty(label, len(full), context="union of compiled-Hew shards")

    if mode == "ratchet":
        actual = {identity for identity, outcome in o0.items() if outcome == "FAILED"}
        if actual:
            die(f"O0 shards contain failing tests: failures={sorted(actual)[:5]}")
        print(
            f"compiled-Hew suite passed: {len(full)} tests across {shard_count} shards"
        )
        return

    differences = [
        identity for identity in sorted(full) if o0.get(identity) != o2.get(identity)
    ]
    if differences:
        examples = [
            f"{identity}: O0={o0.get(identity)} O2={o2.get(identity)}"
            for identity in differences[:5]
        ]
        die("O0/O2 shard outcomes differ: " + "; ".join(examples))
    print(
        f"compiled-Hew differential passed: {len(full)} identical outcomes across "
        f"{shard_count} shards"
    )


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    subcommands = parser.add_subparsers(dest="action", required=True)
    run_parser = subcommands.add_parser("run")
    run_parser.add_argument("--compiler", type=Path, required=True)
    run_parser.add_argument("--partition", required=True)
    run_parser.add_argument("--output-dir", type=Path, required=True)
    aggregate_parser = subcommands.add_parser("aggregate")
    aggregate_parser.add_argument(
        "--mode", choices=("ratchet", "differential"), required=True
    )
    aggregate_parser.add_argument("--reports-dir", type=Path, required=True)
    aggregate_parser.add_argument("--full-inventory", type=Path, required=True)
    aggregate_parser.add_argument("--shard-count", type=int, required=True)
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    if args.action == "run":
        run_shard(args.compiler.resolve(), args.partition, args.output_dir)
    else:
        aggregate(
            args.mode,
            args.reports_dir,
            args.full_inventory,
            args.shard_count,
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
