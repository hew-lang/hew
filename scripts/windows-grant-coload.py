#!/usr/bin/env python3
"""One unloaded/loaded comparison of a retained Windows grant executable."""

import ctypes
from ctypes import wintypes
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parent.parent
REPLAY = Path(os.environ["RUNNER_TEMP"]) / "grant-replay"
FIXTURE = ROOT / "hew-cli/tests/fixtures/distributed/dist_node.hew"
HARNESS = (
    REPLAY
    / "grant-harness-target/debug/deps/distributed_two_process_e2e-eb4250a2cc6995dc.exe"
)
EXECUTABLE = REPLAY / "grant-fixtures/debug-1/.tmpBJ1mkE/dist_node.exe"
ARCHIVE = REPLAY / "grant-sdk-debug/lib/hew.lib"
RETAINED_HASHES = {
    FIXTURE: "22ba1c51d1f184b0d141d1f6b9d706cc34a9d4cb5f20d461c0ffa204cf9fdd13",
    ROOT
    / "hew-cli/tests/distributed_two_process_e2e.rs": "8c3619c74c458b4f6651d0573e8d5f5b3ef7f21c11d9fee2bab00d7bec6241aa",
    HARNESS: "4a9d90e9ad846d19473d290001ff3d352714c390e9f2e17c9a0c55ca07634221",
    EXECUTABLE: "3f8772fe4351285694de36d8d76b62ee7d454f59e7c335e5a2e9a8e740848b88",
    ARCHIVE: "af32a9565516eedec09e61c1be11da89bb294deb177777774495f1633bd565ac",
}
CASE = "authenticated_service_grants_supply_and_revoke_exact_interface"


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def verify_retained():
    for path, expected in RETAINED_HASHES.items():
        if digest(path) != expected:
            raise RuntimeError(f"Retained source or binary hash mismatch: {path}")


def replay(arguments):
    """The original harness asks to compile; explicitly replay its proven binary."""
    verify_retained()
    if len(arguments) != 4 or arguments[:2] != ["compile", "--emit-dir"]:
        raise RuntimeError(f"Unexpected replay arguments: {arguments!r}")
    if Path(arguments[3]).resolve() != FIXTURE.resolve():
        raise RuntimeError("Replay source is not the recorded fixture")
    emit = Path(arguments[2]).resolve()
    scratch = Path(os.environ["HEW_GRANT_REPLAY_SCRATCH"]).resolve()
    if not emit.is_dir() or not emit.is_relative_to(scratch) or any(emit.iterdir()):
        raise RuntimeError("Replay output must be an empty harness temporary directory")
    target = emit / "dist_node.exe"
    shutil.copyfile(EXECUTABLE, target)
    if digest(target) != RETAINED_HASHES[EXECUTABLE]:
        raise RuntimeError("Replayed executable hash mismatch")
    Path(os.environ["HEW_GRANT_REPLAY_RECEIPT"]).write_text(
        json.dumps(
            {
                "operation": "retained binary reuse; no compilation",
                "argv": arguments,
                "executable_sha256": RETAINED_HASHES[EXECUTABLE],
                "harness_sha256": RETAINED_HASHES[HARNESS],
                "runtime_archive_sha256": RETAINED_HASHES[ARCHIVE],
                "source_sha256": RETAINED_HASHES[FIXTURE],
            },
            indent=2,
        )
        + "\n"
    )


def cpu_seconds(process):
    times = [wintypes.FILETIME() for _ in range(4)]
    function = ctypes.windll.kernel32.GetProcessTimes
    function.argtypes = [wintypes.HANDLE] + [ctypes.POINTER(wintypes.FILETIME)] * 4
    function.restype = wintypes.BOOL
    if not function(int(process._handle), *(ctypes.byref(value) for value in times)):
        raise ctypes.WinError()
    return (
        sum((value.dwHighDateTime << 32) | value.dwLowDateTime for value in times[2:])
        / 10_000_000
    )


def start(command, directory, environment):
    stdout = (directory / "stdout").open("wb")
    stderr = (directory / "stderr").open("wb")
    try:
        process = subprocess.Popen(
            command,
            cwd=ROOT,
            env=environment,
            stdin=subprocess.DEVNULL,
            stdout=stdout,
            stderr=stderr,
        )
    except BaseException:
        stdout.close()
        stderr.close()
        raise
    return {
        "process": process,
        "stdout": stdout,
        "stderr": stderr,
        "started": time.monotonic(),
        "started_unix_us": time.time_ns() // 1000,
        "timed_out": False,
    }


def collect(record):
    process = record["process"]
    status = process.wait()
    record["stdout"].close()
    record["stderr"].close()
    return {
        "pid": process.pid,
        "exit": status,
        "timed_out": record["timed_out"],
        "cpu_seconds": cpu_seconds(process),
        "started_unix_us": record["started_unix_us"],
        "finished_unix_us": record.get("finished_unix_us", time.time_ns() // 1000),
    }


def run_phase(name, compiler, adapter, evidence, load_count):
    phase = evidence / name
    scratch = Path(os.environ["RUNNER_TEMP"]) / "grant-coload-scratch" / name
    events = phase / "events"
    for path in [phase, scratch, events]:
        path.mkdir(parents=True, exist_ok=True)
    environment = os.environ.copy()
    for key in [
        "HEW_DIST_E2E_OPT_LEVEL",
        "HEW_SANITIZE_ADDRESS",
        "HEW_DIST_TEST_PROBE",
    ]:
        environment.pop(key, None)
    environment.update(
        {
            "TMP": str(scratch),
            "TEMP": str(scratch),
            "HEW_TEST_NO_BUILD": "1",
            "HEW_DIST_E2E_HEW_BIN": str(adapter),
            "HEW_GRANT_REPLAY_SCRATCH": str(scratch),
            "HEW_GRANT_REPLAY_RECEIPT": str(phase / "replay.json"),
            "HEW_GRANT_DIAGNOSTIC_DIR": str(events),
        }
    )
    harness = start(
        [str(HARNESS), CASE, "--exact", "--nocapture", "--test-threads=1"],
        phase,
        environment,
    )
    loads, samples, markers = [], [], {}
    load_started = False
    next_sample = 0.0
    try:
        with (phase / "cpu-samples.jsonl").open("w") as sampling:
            while True:
                now = time.monotonic()
                for marker in (
                    "service.held",
                    "service.reconnected",
                    "service.disconnect",
                ):
                    if marker not in markers:
                        paths = list(scratch.rglob(marker))
                        if paths:
                            markers[marker] = {
                                "written_unix_us": paths[0].stat().st_mtime_ns // 1000,
                                "observed_unix_us": time.time_ns() // 1000,
                            }
                if load_count and "service.held" in markers and not load_started:
                    load_started = True
                    # Each real compiler runs once. No delays or barriers are added to the fixture.
                    for index in range(load_count):
                        directory = phase / f"load-{index}"
                        directory.mkdir()
                        emit = (
                            Path(os.environ["RUNNER_TEMP"])
                            / f"grant-coload-output-{index}"
                        )
                        emit.mkdir()
                        load_environment = os.environ.copy()
                        load_environment["HEW_STD"] = str(ROOT / "std")
                        loads.append(
                            start(
                                [
                                    str(compiler),
                                    "compile",
                                    "--emit-dir",
                                    str(emit),
                                    "--opt-level",
                                    "0",
                                    str(FIXTURE),
                                ],
                                directory,
                                load_environment,
                            )
                        )
                for record, ceiling in [
                    (harness, 180),
                    *((load, 120) for load in loads),
                ]:
                    process = record["process"]
                    if process.poll() is not None:
                        record.setdefault("finished_unix_us", time.time_ns() // 1000)
                    elif now - record["started"] > ceiling:
                        # Popen retains the exact Windows process handle; no PID-only termination.
                        process.kill()
                        process.wait()
                        record["timed_out"] = True
                if now >= next_sample:
                    sample = {
                        "unix_us": time.time_ns() // 1000,
                        "loads": [
                            {
                                "pid": record["process"].pid,
                                "cpu_seconds": cpu_seconds(record["process"]),
                                "running": record["process"].poll() is None,
                            }
                            for record in loads
                        ],
                    }
                    samples.append(sample)
                    sampling.write(json.dumps(sample) + "\n")
                    next_sample = now + 0.020
                if harness["process"].poll() is not None and all(
                    x["process"].poll() is not None for x in loads
                ):
                    break
                time.sleep(0.005)
        harness_result = collect(harness)
        load_results = [collect(load) for load in loads]
        event_text = "\n".join(
            path.read_text() for path in sorted(events.glob("*.log"))
        )
        partitions = [
            line for line in event_text.splitlines() if "partition-origin=" in line
        ]
        refusals = [
            line
            for line in event_text.splitlines()
            if "accept-admission-refused" in line
        ]
        start_us = markers.get("service.held", {}).get("written_unix_us")
        end_us = markers.get("service.disconnect", {}).get("written_unix_us")
        if end_us is None and partitions:
            end_us = min(
                int(re.search(r"unix_us=(\d+)", line)[1]) for line in partitions
            )
        overlap = []
        if start_us is not None and end_us is not None:
            for previous, current in zip(samples, samples[1:]):
                if start_us <= previous["unix_us"] < current["unix_us"] <= end_us:
                    prior_cpu = {x["pid"]: x["cpu_seconds"] for x in previous["loads"]}
                    delta = sum(
                        x["cpu_seconds"] - prior_cpu.get(x["pid"], x["cpu_seconds"])
                        for x in current["loads"]
                    )
                    if delta > 0:
                        overlap.append(
                            {
                                "start_unix_us": previous["unix_us"],
                                "end_unix_us": current["unix_us"],
                                "load_cpu_seconds": delta,
                            }
                        )
        passed = (
            harness_result["exit"] == 0
            and not harness_result["timed_out"]
            and b"1 passed; 0 failed" in (phase / "stdout").read_bytes()
        )
        load_succeeded = len(load_results) == load_count and all(
            x["exit"] == 0 and not x["timed_out"] for x in load_results
        )
        result = {
            "phase": name,
            "case_passed": passed,
            "harness": harness_result,
            "load_succeeded": load_succeeded,
            "load_requested": load_count,
            "loads": load_results,
            "markers": markers,
            "partition_events": partitions,
            "accept_refusals": refusals,
            "measured_cpu_overlap_intervals": overlap,
            "overlap_established": bool(overlap) if load_count else None,
            "interpretation": "Binary reuse, not a compile proof; a loaded pass without measured overlap is inconclusive",
        }
        (phase / "result.json").write_text(json.dumps(result, indent=2) + "\n")
        print(json.dumps(result), flush=True)
        return result
    finally:
        for record in [harness, *loads]:
            if record["process"].poll() is None:
                record["process"].kill()
                record["process"].wait()
            record["stdout"].close()
            record["stderr"].close()


def main():
    verify_retained()
    # The retained harness embeds this original workspace parent in repo_root().
    if ROOT != Path(r"D:\a\hew\hew"):
        raise RuntimeError("Retained harness workspace path differs from this checkout")
    sdk = Path(os.environ["HEW_GRANT_LOAD_SDK"])
    compiler = sdk / "bin/hew.exe"
    sdk_hashes = {
        compiler: "f5d3c0e459675d4cbbd7a88df24b95b05828bcf10a68398d66b20b4af5f3fd65",
        sdk
        / "lib/hew.lib": "f4c6fc625b1190921ff8f1cbb180b5574f08bfc466a36cc15164e81fa052b841",
        sdk
        / "lib/x86_64-pc-windows-msvc/hew.lib": "f4c6fc625b1190921ff8f1cbb180b5574f08bfc466a36cc15164e81fa052b841",
    }
    for path, expected in sdk_hashes.items():
        if digest(path) != expected:
            raise RuntimeError(f"Frozen SDK changed: {path}")
    evidence = Path(os.environ["HEW_GRANT_LOAD_EVIDENCE"]) / "co-load"
    evidence.mkdir()
    # Verify-only bootstrap archive, not a newly linked runtime.
    shutil.copyfile(ARCHIVE, HARNESS.parent.parent / "hew.lib")
    adapter = evidence / "replay-compiler.cmd"
    adapter.write_text(
        f'@echo off\n@"{sys.executable}" "{Path(__file__).resolve()}" replay %*\n'
    )
    count = min(3, max(0, (os.cpu_count() or 1) - 1))
    (evidence / "provenance.json").write_text(
        json.dumps(
            {
                "diagnostic_revision": subprocess.check_output(
                    ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True
                ).strip(),
                "retained_run": 39446557642,
                "retained_source": "51623202d03c8d2f5363c15b0732f685acc50fe2",
                "retained_sha256": {
                    str(path): value for path, value in RETAINED_HASHES.items()
                },
                "sdk_sha256": {str(path): value for path, value in sdk_hashes.items()},
                "cpu_count": os.cpu_count(),
                "load_count": count,
                "load_ceiling_seconds": 120,
                "sample_period_seconds": 0.020,
                "load_trigger": "observed service.held marker",
                "compiler_version": subprocess.check_output(
                    [str(compiler), "--version"], text=True
                ).strip(),
                "clang_version": subprocess.check_output(
                    ["clang", "--version"], text=True
                ).strip(),
                "scope": "Unchanged retained runtime/fixture; no Rust compiler/runtime/harness build; actual frozen SDK source compiles supply load",
            },
            indent=2,
        )
        + "\n"
    )
    control = run_phase("control", compiler, adapter, evidence, 0)
    results = [control]
    if control["case_passed"] and count:
        results.append(run_phase("loaded", compiler, adapter, evidence, count))
    unchanged = all(digest(path) == expected for path, expected in sdk_hashes.items())
    verify_retained()
    (evidence / "result.json").write_text(
        json.dumps(
            {
                "phases": results,
                "sdk_unchanged": unchanged,
                "comparison_informative": len(results) == 2
                and results[1]["load_succeeded"]
                and results[1]["overlap_established"],
                "release_qualification": False,
            },
            indent=2,
        )
        + "\n"
    )
    return (
        0
        if unchanged
        and len(results) == 2
        and all(r["case_passed"] and r["load_succeeded"] for r in results)
        else 1
    )


if __name__ == "__main__":
    if sys.argv[1:2] == ["replay"]:
        replay(sys.argv[2:])
    elif len(sys.argv) == 1:
        raise SystemExit(main())
    else:
        raise SystemExit("Unexpected diagnostic arguments")
