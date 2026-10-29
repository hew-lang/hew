#!/usr/bin/env python3
"""Verify production runtime contracts and bounded concurrent compositions."""

from pathlib import Path
import argparse
import subprocess
import time
from boundaries import check

COUNTS = {
    "waker": 1,
    "cancellation": 1,
    "await": 1,
    "timer": 3,
    "arena": 3,
    "heap": 2,
    "read_slot": 1,
    "supervisor": 2,
    "managed_budget": 1,
    "root_driver": 1,
    "vector": 25,
    "executor": 4,
    "fault_records": 4,
    "restart": 2,
    "deadline": 1,
    "reactor": 1,
    "pipeline": 2,
    "one_shot": 2,
}
COVER_COUNTS = {
    "root_driver": 7,
    "vector": 34,
    "executor": 8,
    "fault_records": 1,
    "restart": 2,
    "deadline": 2,
    "reactor": 4,
    "pipeline": 7,
    "one_shot": 7,
}
LIFECYCLE = {"semaphore", "read_slot", "supervisor", "managed_budget"}
FEATURES = list(COUNTS) + ["semaphore", "channel", "periodic"]
ROOT = Path(__file__).resolve().parent


def command(root, feature, playback=False):
    args = [
        "cargo",
        "kani",
        "--lib",
        "--manifest-path",
        str(root / "Cargo.toml"),
        "--features",
        feature,
        "--output-format",
        "terse",
        "-Z",
        "stubbing",
        "-Z",
        "unstable-options",
        "--harness-timeout",
        "180s",
    ]
    if feature == "read_slot":
        args += ["--harness", "read_slot_teardown"]
    if feature in ("vector", "executor", "one_shot"):
        args += ["--harness", feature + "_"]
    if playback:
        args += ["-Z", "concrete-playback", "--concrete-playback", "print"]
    if feature in ("vector", "executor"):
        args += ["--cbmc-args", "--memory-leak-check"]
    return args


def verify(root, feature):
    start = time.monotonic()
    result = subprocess.run(command(root, feature), text=True, capture_output=True)
    logs = root / "target/proof-logs"
    logs.mkdir(parents=True, exist_ok=True)
    (logs / f"{feature}.log").write_text(result.stdout + result.stderr)
    count = COUNTS[feature]
    expected = f"Complete - {count} successfully verified harnesses, 0 failures, {count} total."
    covers = COVER_COUNTS.get(feature)
    import re

    reached = [
        (int(a), int(b))
        for a, b in re.findall(
            r"(\d+) of (\d+) cover properties satisfied", result.stdout
        )
    ]
    coverage_ok = (
        covers is None
        or sum(a for a, b in reached) == sum(b for a, b in reached) == covers
    )
    if result.returncode != 0 or expected not in result.stdout or not coverage_ok:
        print(result.stdout, flush=True)
        print(result.stderr, flush=True)
        replay = subprocess.run(
            command(root, feature, playback=True), text=True, capture_output=True
        )
        (logs / f"{feature}-counterexample.log").write_text(
            replay.stdout + replay.stderr
        )
        print(replay.stdout, flush=True)
        print(replay.stderr, flush=True)
        raise SystemExit(f"Protocol proof failed or was empty: {feature}; see {logs}")
    print(f"Verified {feature} ({time.monotonic() - start:.1f}s)", flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("features", nargs="*", choices=FEATURES)
    args = parser.parse_args()
    check()
    selected = args.features or FEATURES
    if any(feature in COUNTS for feature in selected):
        version = subprocess.check_output(
            ["cargo", "kani", "--version"], text=True
        ).splitlines()[0]
        if "0.68.0" not in version:
            raise SystemExit(
                "Kani 0.68.0 is required; see proofs/ready_gate/README.md#running"
            )
    for feature in selected:
        if feature in COUNTS:
            verify(ROOT, feature)
        extra = {
            "periodic": (
                [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(ROOT / "Cargo.toml"),
                    "--locked",
                    "--features",
                    "periodic_loom",
                    "--bin",
                    "periodic",
                ],
                "Verified periodic ",
                4,
            ),
            "reactor": (
                [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(ROOT / "Cargo.toml"),
                    "--locked",
                    "--features",
                    "reactor_loom",
                    "--bin",
                    "reactor",
                ],
                "Verified reactor ",
                10,
            ),
            "channel": (
                [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(ROOT / "Cargo.toml"),
                    "--locked",
                    "--features",
                    "channel_loom",
                    "--bin",
                    "channels",
                ],
                "Verified channel ",
                8,
            ),
            "fault_records": (
                [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(ROOT / "Cargo.toml"),
                    "--locked",
                    "--features",
                    "fault_loom",
                    "--bin",
                    "faults",
                ],
                "Verified fault ",
                6,
            ),
            "restart": (
                [
                    "cargo",
                    "test",
                    "--manifest-path",
                    str(ROOT / "Cargo.toml"),
                    "--locked",
                    "--features",
                    "failure_properties",
                    "--lib",
                    "failure_properties",
                ],
                "test result: ok. 3 passed; 0 failed;",
                1,
            ),
        }.get(feature)
        if extra:
            extra_args, marker, expected = extra
            result = subprocess.run(extra_args, text=True, capture_output=True)
            logs = ROOT / "target/proof-logs"
            logs.mkdir(parents=True, exist_ok=True)
            (logs / f"{feature}-failure-models.log").write_text(
                result.stdout + result.stderr
            )
            print(result.stdout, flush=True)
            if result.returncode or result.stdout.count(marker) != expected:
                print(result.stderr, flush=True)
                raise SystemExit(
                    f"Failure protocol check failed or was empty: {feature}"
                )
        if feature == "executor":
            result = subprocess.run(
                [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(ROOT / "Cargo.toml"),
                    "--locked",
                    "--features",
                    "executor_loom",
                    "--bin",
                    "executor",
                ],
                text=True,
                capture_output=True,
            )
            logs = ROOT / "target/proof-logs"
            logs.mkdir(parents=True, exist_ok=True)
            (logs / "executor-concurrent.log").write_text(result.stdout + result.stderr)
            print(result.stdout, flush=True)
            if result.returncode or result.stdout.count("Verified executor ") != 3:
                print(result.stderr, flush=True)
                raise SystemExit("Executor interleaving check failed or was empty")
    compositions = (
        [None] if not args.features else [f for f in selected if f in LIFECYCLE]
    )
    for composition in compositions:
        command_args = [
            "cargo",
            "run",
            "--manifest-path",
            str(ROOT / "Cargo.toml"),
            "--locked",
            "--features",
            "loom",
            "--bin",
            "protocols",
        ]
        if composition:
            command_args += ["--", composition]
        result = subprocess.run(command_args, text=True, capture_output=True)
        logs = ROOT / "target/proof-logs"
        logs.mkdir(parents=True, exist_ok=True)
        (logs / f"{composition or 'concurrent'}.log").write_text(
            result.stdout + result.stderr
        )
        print(result.stdout, flush=True)
        expected = 1 if composition else 7
        if result.returncode != 0 or result.stdout.count("Verified ") != expected:
            print(result.stderr, flush=True)
            raise SystemExit("Concurrent contract failed or was empty")
