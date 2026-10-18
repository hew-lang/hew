#!/usr/bin/env python3
"""Run checked workloads and retain build, throughput and latency samples."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import random
import re
import shutil
import signal
import statistics
import subprocess
import time


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
PRIMITIVES = ("hew-o0", "hew", "rust", "rust-checked", "go", "c")
CONCURRENCY = ("hew-o0", "hew", "go", "tokio", "beam")


def execute(command, *, timeout, env=None, cwd=HERE):
    start = time.perf_counter_ns()
    process = subprocess.Popen(
        command,
        cwd=cwd,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        start_new_session=os.name == "posix",
    )
    try:
        stdout, stderr = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        if os.name == "posix":
            os.killpg(process.pid, signal.SIGKILL)
        else:
            process.kill()
        stdout, stderr = process.communicate()
        raise RuntimeError(
            f"timeout after {timeout}s: {command[0]}\n{stderr}"
        ) from None
    elapsed = time.perf_counter_ns() - start
    if process.returncode:
        raise RuntimeError(
            f"exit {process.returncode}: {' '.join(map(str, command))}\n{stderr}\n{stdout}"
        )
    return stdout, stderr, elapsed


def expected(workload, iterations, size):
    if workload == "scalar":
        return pow(48271, iterations, 2147483647)
    if workload in ("vector_scan", "vector_build"):
        return iterations * sum(i % 251 for i in range(size))
    if workload == "bytes_shared":
        return sum((i % size) % 251 for i in range(iterations))
    if workload == "bytes_cow":
        return sum(
            ((i % size) % 251 + 1) % 251 + (i % size) % 251 for i in range(iterations)
        )
    if workload == "task_ready":
        return sum(i % 251 for i in range(iterations))
    if workload in ("actor_roundtrip", "actor_roundtrip_alloc"):
        return iterations * (iterations + 1) // 2
    if workload == "actor_fanout":
        return size * iterations * (iterations + 1) // 2
    if workload == "timer":
        return iterations
    raise ValueError(f"unknown workload: {workload}")


def validate(stdout, workload, iterations, size):
    value = json.loads(stdout)
    for key, wanted in (
        ("workload", workload),
        ("iterations", iterations),
        ("size", size),
        ("checksum", expected(workload, iterations, size)),
    ):
        if value.get(key) != wanted:
            raise ValueError(f"{key}: expected {wanted}, received {value.get(key)}")
    elapsed = value.get("elapsed_ns")
    if not isinstance(elapsed, int) or elapsed <= 0:
        raise ValueError("elapsed_ns must be a positive integer")
    latencies = value.get("latencies_ns", [])
    if not isinstance(latencies, list) or any(
        not isinstance(sample, int) or sample < 0 for sample in latencies
    ):
        raise ValueError("invalid latency samples")
    if workload in ("actor_roundtrip", "timer") and len(latencies) != min(
        iterations, 10000
    ):
        raise ValueError("one latency sample is required per completed operation")
    return value


def percentile(values, fraction):
    ordered = sorted(values)
    return ordered[max(0, math.ceil(len(ordered) * fraction) - 1)]


def describe(values):
    return {
        "median": statistics.median(values),
        "p25": percentile(values, 0.25),
        "p75": percentile(values, 0.75),
        "p95": percentile(values, 0.95),
        "p99": percentile(values, 0.99),
        "min": min(values),
        "max": max(values),
    }


def command_for(output, group, language, workload, iterations, size, workers):
    arguments = [workload, str(iterations), str(size)]
    if language == "tokio":
        return [str(output / "tokio-target/release/calibration-tokio"), *arguments]
    if language == "beam":
        return [
            "erl",
            "+S",
            f"{workers}:{workers}",
            "+A",
            "1",
            "-noshell",
            "-pa",
            str(output),
            "-s",
            "concurrency",
            "main",
            "-extra",
            *arguments,
        ]
    return [str(output / f"{group}-{language}"), *arguments]


def cases(smoke=False):
    if smoke:
        return [
            ("primitives", name, 10, 8)
            for name in (
                "scalar",
                "vector_scan",
                "vector_build",
                "bytes_shared",
                "bytes_cow",
            )
        ] + [
            ("concurrency", name, 10, 4)
            for name in ("task_ready", "actor_roundtrip", "actor_fanout", "timer")
        ]
    result = [("primitives", "scalar", n, 1) for n in (100_000, 1_000_000)]
    for size in (64, 4096, 65536):
        result.extend(
            [
                ("primitives", "vector_scan", max(8, 1_000_000 // size), size),
                ("primitives", "vector_build", max(8, 262_144 // size), size),
                ("primitives", "bytes_shared", 100_000, size),
                ("primitives", "bytes_cow", max(16, 1_048_576 // size), size),
            ]
        )
    for size in (1, 16, 64):
        result.append(("concurrency", "task_ready", 4096, size))
        result.append(("concurrency", "actor_fanout", 256, size))
    result.extend(
        [("concurrency", "actor_roundtrip", 1000, 1), ("concurrency", "timer", 100, 1)]
    )
    return result


def available_languages(requested):
    tools = {
        "hew": None,
        "hew-o0": None,
        "rust": "rustc",
        "rust-checked": "rustc",
        "c": "clang",
        "go": "go",
        "tokio": "cargo",
        "beam": "erl",
    }
    present, missing = [], []
    for language in requested:
        tool = tools[language]
        if tool is None or shutil.which(tool):
            present.append(language)
        else:
            missing.append({"language": language, "reason": f"missing {tool}"})
    return present, missing


def metadata(hew):
    versions = {}
    for name, command in (
        ("hew", [hew, "--version"]),
        ("rustc", ["rustc", "--version"]),
        ("go", ["go", "version"]),
        ("clang", ["clang", "--version"]),
        (
            "beam",
            [
                "erl",
                "+S",
                "1:1",
                "+A",
                "1",
                "-noshell",
                "-eval",
                'io:format("OTP ~s", [erlang:system_info(otp_release)]), halt().',
            ],
        ),
    ):
        if shutil.which(command[0]):
            stdout, stderr, _ = execute(command, timeout=30)
            versions[name] = (stdout or stderr).splitlines()[0]
    cpu_model = platform.processor()
    cpuinfo = Path("/proc/cpuinfo")
    if cpuinfo.exists():
        cpu_model = next(
            (
                line.split(":", 1)[1].strip()
                for line in cpuinfo.read_text().splitlines()
                if line.startswith("model name")
            ),
            cpu_model,
        )
    stdout, _, _ = execute(["git", "rev-parse", "HEAD"], timeout=10)
    fixture_hashes = {}
    for directory, subdirectories, files in os.walk(HERE):
        subdirectories[:] = [
            name
            for name in subdirectories
            if name not in ("out", "target", "__pycache__")
        ]
        for name in sorted(files):
            path = Path(directory) / name
            if path.suffix in (".hew", ".rs", ".go", ".c", ".erl", ".toml", ".lock"):
                fixture_hashes[str(path.relative_to(HERE))] = hashlib.sha256(
                    path.read_bytes()
                ).hexdigest()
    return {
        "source_revision": stdout.strip(),
        "platform": {
            "system": platform.system(),
            "release": platform.release(),
            "machine": platform.machine(),
            "cpu_model": cpu_model,
        },
        "versions": versions,
        "compiler_sha256": hashlib.sha256(Path(hew).read_bytes()).hexdigest(),
        "affinity": sorted(os.sched_getaffinity(0))
        if hasattr(os, "sched_getaffinity")
        else None,
        "load_average_start": os.getloadavg() if hasattr(os, "getloadavg") else None,
        "fixture_sha256": fixture_hashes,
    }


def build(args, languages, receipt):
    for group, choices in (("primitives", PRIMITIVES), ("concurrency", CONCURRENCY)):
        for language in choices:
            if language not in languages:
                continue
            target = f"{group}-{language}"
            for sample in range(args.build_repeats):
                stdout, stderr, elapsed = execute(
                    [
                        "make",
                        "--no-print-directory",
                        target,
                        f"HEW={args.hew}",
                        f"OUT={args.output}",
                    ],
                    timeout=args.build_timeout,
                )
                (args.output / f"{target}-build-{sample}.log").write_text(
                    stdout + stderr
                )
                receipt["builds"].append(
                    {
                        "group": group,
                        "language": language,
                        "sample": sample,
                        "state": "first_invocation" if sample == 0 else "rebuild",
                        "elapsed_ns": elapsed,
                    }
                )
            print(f"built {target}", flush=True)


def scaling_sources(directory, count):
    directory.mkdir(parents=True, exist_ok=True)
    hew = [
        f"fn step_{i}(x: i64) -> i64 {{ (x + {i + 1}) % 1000003 }}"
        for i in range(count)
    ]
    rust = [
        f"fn step_{i}(x: i64) -> i64 {{ (x + {i + 1}) % 1000003 }}"
        for i in range(count)
    ]
    go = ["package main", 'import "fmt"'] + [
        f"func step_{i}(x int64) int64 {{ return (x + {i + 1}) % 1000003 }}"
        for i in range(count)
    ]
    c = ["#include <stdio.h>", "#include <stdint.h>"] + [
        f"int64_t step_{i}(int64_t x) {{ return (x + {i + 1}) % 1000003; }}"
        for i in range(count)
    ]
    for lines, opening, ending, suffix in (
        (hew, "fn main() { var x: i64 = 1;", "println(x); }", "hew"),
        (rust, "fn main() { let mut x: i64 = 1;", 'println!("{}", x); }', "rs"),
        (go, "func main() { var x int64 = 1", "fmt.Println(x) }", "go"),
        (
            c,
            "int main(void) { int64_t x = 1;",
            'printf("%lld\\n", (long long)x); }',
            "c",
        ),
    ):
        lines.extend([opening, *(f"x = step_{i}(x);" for i in range(count)), ending])
        (directory / f"scale.{suffix}").write_text("\n".join(lines) + "\n")


def compile_scaling(args, languages, receipt):
    for count in (32, 128, 512):
        directory = args.output / "compile-scaling" / str(count)
        scaling_sources(directory, count)
        for language in PRIMITIVES:
            if language not in languages:
                continue
            for sample in range(args.build_repeats):
                stdout, stderr, elapsed = execute(
                    [
                        "make",
                        "--no-print-directory",
                        f"primitives-{language}",
                        f"HEW={args.hew}",
                        f"OUT={directory}",
                        f"PRIMITIVE_SOURCE={directory / 'scale'}",
                    ],
                    timeout=args.build_timeout,
                )
                (directory / f"{language}-build-{sample}.log").write_text(
                    stdout + stderr
                )
                actual, _, _ = execute(
                    [str(directory / f"primitives-{language}")], timeout=30
                )
                if int(actual.strip()) != (1 + count * (count + 1) // 2) % 1000003:
                    raise ValueError(
                        f"compile-scaling checksum failed: {language}, {count}"
                    )
                receipt["compilation_scaling"].append(
                    {
                        "language": language,
                        "functions": count,
                        "sample": sample,
                        "state": "first_invocation" if sample == 0 else "rebuild",
                        "elapsed_ns": elapsed,
                    }
                )
            print(f"compiled {count} functions with {language}", flush=True)


def runtime(args, languages, receipt):
    order = random.Random(20261016)
    for group, workload, iterations, size in cases(args.smoke):
        choices = [
            language
            for language in (PRIMITIVES if group == "primitives" else CONCURRENCY)
            if language in languages
        ]
        if not choices:
            continue
        for workers in [1] if group == "primitives" else args.workers:
            for sample in range(-1, args.repeats):
                order.shuffle(choices)
                for language in choices:
                    env = dict(
                        os.environ,
                        HEW_WORKERS=str(workers),
                        GOMAXPROCS=str(workers),
                        BENCH_WORKERS=str(workers),
                    )
                    command = command_for(
                        args.output,
                        group,
                        language,
                        workload,
                        iterations,
                        size,
                        workers,
                    )
                    rss_path = args.output / "rss.txt"
                    has_rss = (
                        platform.system() == "Linux" and Path("/usr/bin/time").exists()
                    )
                    if has_rss:
                        command = [
                            "/usr/bin/time",
                            "-f",
                            "%M",
                            "-o",
                            str(rss_path),
                            *command,
                        ]
                    stdout, _, process_ns = execute(
                        command, timeout=args.timeout, env=env
                    )
                    value = validate(stdout, workload, iterations, size)
                    if sample < 0:
                        continue
                    value.update(
                        group=group,
                        language=language,
                        workers=workers,
                        sample=sample,
                        process_elapsed_ns=process_ns,
                    )
                    if has_rss:
                        value["process_peak_rss_kib"] = int(
                            rss_path.read_text().strip()
                        )
                    receipt["runtime"].append(value)
            print(f"checked {workload} size={size} workers={workers}", flush=True)


def allocations(args, languages, receipt):
    if not shutil.which("valgrind"):
        receipt["skipped"].append(
            {"measurement": "allocations", "reason": "missing valgrind"}
        )
        return
    # These counts observe the native allocator, not Go's or BEAM's internal heaps.
    for group, workload, size in (
        ("primitives", "vector_build", 4096),
        ("primitives", "bytes_shared", 4096),
        ("primitives", "bytes_cow", 4096),
        ("concurrency", "task_ready", 16),
        ("concurrency", "actor_roundtrip_alloc", 1),
    ):
        for language in (
            ["hew", "rust", "c"] if group == "primitives" else ["hew", "tokio"]
        ):
            if language not in languages:
                continue
            for iterations in (32, 256):
                log = (
                    args.output / f"allocations-{workload}-{language}-{iterations}.log"
                )
                command = [
                    "valgrind",
                    "--tool=memcheck",
                    "--leak-check=no",
                    "--error-exitcode=97",
                    f"--log-file={log}",
                    *command_for(
                        args.output, group, language, workload, iterations, size, 1
                    ),
                ]
                env = dict(os.environ, HEW_WORKERS="1", BENCH_WORKERS="1")
                stdout, _, _ = execute(command, timeout=args.build_timeout, env=env)
                validate(stdout, workload, iterations, size)
                match = re.search(
                    r"total heap usage: ([\d,]+) allocs, ([\d,]+) frees, ([\d,]+) bytes allocated",
                    log.read_text(),
                )
                if match is None:
                    raise ValueError(f"no allocator measurements in {log.name}")
                allocated, freed, total_bytes = (
                    int(value.replace(",", "")) for value in match.groups()
                )
                receipt["allocations"].append(
                    {
                        "group": group,
                        "workload": workload,
                        "language": language,
                        "iterations": iterations,
                        "size": size,
                        "allocations": allocated,
                        "frees": freed,
                        "allocated_bytes": total_bytes,
                    }
                )
            print(f"profiled {workload} with {language}", flush=True)


def summary(receipt):
    groups = {}
    for row in receipt["runtime"]:
        key = (
            row["workload"],
            row["iterations"],
            row["size"],
            row["workers"],
            row["language"],
        )
        groups.setdefault(key, []).append(row)
    lines = [
        "# Calibration run",
        "",
        "Execution is timed inside each workload. "
        "Process RSS includes runtime startup. Latency tails pool the recorded operations; "
        "throughput ranges describe repeated process runs.",
        "",
        "| Workload | Iterations | Size | Workers | Implementation | Median batch ms | Batch IQR ms | p50 / p95 / p99 µs | Peak RSS MiB |",
        "| --- | ---: | ---: | ---: | --- | ---: | ---: | --- | ---: |",
    ]
    for (workload, iterations, size, workers, language), rows in groups.items():
        batch = describe([row["elapsed_ns"] for row in rows])
        samples = [value for row in rows for value in row.get("latencies_ns", [])]
        latency = (
            "—"
            if not samples
            else " / ".join(
                f"{percentile(samples, fraction) / 1000:.1f}"
                for fraction in (0.5, 0.95, 0.99)
            )
        )
        rss = [
            row["process_peak_rss_kib"] for row in rows if "process_peak_rss_kib" in row
        ]
        memory = f"{max(rss) / 1024:.1f}" if rss else "—"
        lines.append(
            f"| {workload} | {iterations} | {size} | {workers} | {language} | {batch['median'] / 1e6:.2f} | "
            f"{batch['p25'] / 1e6:.2f}–{batch['p75'] / 1e6:.2f} | {latency} | {memory} |"
        )
    lines.extend(
        [
            "",
            "## Application builds",
            "",
            "First means the first invocation in this run, with existing output and machine caches. "
            "Rebuild invokes the ordinary build tool again on unchanged source. "
            "Go/Cargo cache hits and direct compiler invocations have different behaviour.",
            "",
            "| Fixture | Implementation | First ms | Rebuild median ms |",
            "| --- | --- | ---: | ---: |",
        ]
    )
    builds = {}
    for row in receipt["builds"]:
        builds.setdefault((row["group"], row["language"]), []).append(row)
    for (group, language), rows in builds.items():
        rebuilds = [row["elapsed_ns"] for row in rows if row["sample"] > 0]
        rebuild = f"{statistics.median(rebuilds) / 1e6:.0f}" if rebuilds else "—"
        lines.append(
            f"| {group} | {language} | {rows[0]['elapsed_ns'] / 1e6:.0f} | {rebuild} |"
        )
    lines.extend(
        [
            "",
            "## Compilation scaling",
            "",
            "Generated scalar functions, including native linking and a checked executable result.",
            "",
            "| Functions | Implementation | First ms | Rebuild median ms |",
            "| ---: | --- | ---: | ---: |",
        ]
    )
    scaling = {}
    for row in receipt["compilation_scaling"]:
        scaling.setdefault((row["functions"], row["language"]), []).append(row)
    for (count, language), rows in scaling.items():
        rebuilds = [row["elapsed_ns"] for row in rows if row["sample"] > 0]
        rebuild = f"{statistics.median(rebuilds) / 1e6:.0f}" if rebuilds else "—"
        lines.append(
            f"| {count} | {language} | {rows[0]['elapsed_ns'] / 1e6:.0f} | {rebuild} |"
        )
    lines.extend(
        [
            "",
            "## Native allocator growth",
            "",
            "Difference between 32 and 256 operations, divided by 224. Includes workload "
            "and runtime allocations; excludes amortized process setup. Go/BEAM internal heaps "
            "are deliberately outside this measurement.",
            "",
            "| Workload | Implementation | Allocations / operation | Allocated bytes / operation |",
            "| --- | --- | ---: | ---: |",
        ]
    )
    allocation_groups = {}
    for row in receipt.get("allocations", []):
        allocation_groups.setdefault((row["workload"], row["language"]), []).append(row)
    for (workload, language), rows in allocation_groups.items():
        if len(rows) == 2:
            low, high = sorted(rows, key=lambda row: row["iterations"])
            count = high["iterations"] - low["iterations"]
            lines.append(
                f"| {workload} | {language} | "
                f"{(high['allocations'] - low['allocations']) / count:.1f} | "
                f"{(high['allocated_bytes'] - low['allocated_bytes']) / count:.0f} |"
            )
    return "\n".join(lines) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hew", default=str(ROOT / "build/bin/hew"))
    parser.add_argument("--output", type=Path, default=HERE / "out")
    parser.add_argument("--repeats", type=int, default=5)
    parser.add_argument("--build-repeats", type=int, default=3)
    parser.add_argument("--workers", type=int, nargs="+", default=[1, 2])
    parser.add_argument(
        "--languages",
        nargs="+",
        choices=sorted(set(PRIMITIVES + CONCURRENCY)),
        default=sorted(set(PRIMITIVES + CONCURRENCY)),
    )
    parser.add_argument("--timeout", type=float, default=30)
    parser.add_argument("--build-timeout", type=float, default=180)
    parser.add_argument("--smoke", action="store_true")
    parser.add_argument("--skip-build", action="store_true")
    parser.add_argument("--compile-only", action="store_true")
    parser.add_argument(
        "--allocations",
        action="store_true",
        help="profile native allocator growth with Valgrind",
    )
    parser.add_argument(
        "--cpus", type=int, default=2, help="maximum inherited CPU affinity on Linux"
    )
    args = parser.parse_args()
    if os.name != "posix":
        parser.error("the calibration runner currently requires a POSIX host")
    if min(args.repeats, args.build_repeats, args.cpus, *args.workers) < 1:
        parser.error("repeat, CPU and worker counts must be positive")
    args.output = args.output.resolve()
    args.hew = str(Path(args.hew).resolve())
    args.output.mkdir(parents=True, exist_ok=True)
    if hasattr(os, "sched_getaffinity"):
        allowed = sorted(os.sched_getaffinity(0))
        os.sched_setaffinity(0, allowed[: args.cpus])
    languages, skipped = available_languages(args.languages)
    receipt = {
        "schema": 1,
        "metadata": metadata(args.hew),
        "settings": {
            "repeats": args.repeats,
            "languages": languages,
            "workers": args.workers,
            "warmup_runs": 1,
            "language_order_seed": 20261016,
            "smoke": args.smoke,
        },
        "skipped": skipped,
        "builds": [],
        "runtime": [],
        "compilation_scaling": [],
        "allocations": [],
    }
    try:
        if not args.compile_only:
            if not args.skip_build:
                build(args, languages, receipt)
            runtime(args, languages, receipt)
        if not args.smoke:
            compile_scaling(args, languages, receipt)
            if args.allocations and not args.compile_only:
                allocations(args, languages, receipt)
    except (RuntimeError, ValueError, OSError) as error:
        receipt["failure"] = str(error)
        raise
    finally:
        receipt["metadata"]["load_average_end"] = (
            os.getloadavg() if hasattr(os, "getloadavg") else None
        )
        (args.output / "results.json").write_text(json.dumps(receipt, indent=2) + "\n")
        (args.output / "summary.md").write_text(summary(receipt))


if __name__ == "__main__":
    main()
