#!/usr/bin/env python3
"""Explicit bounded replay of raw fuzz inputs.

Stable regressions belong to core-acceptance. Invalid source is expected in a
raw fuzz corpus; compiler crashes, runtime signals, timeouts and output floods
remain failures requiring investigation. No historical failure ledger applies.
"""

import argparse
import os
import re
import signal
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Markers that indicate a Rust/compiler ICE in build output.
_BUILD_ICE_MARKERS = (
    "thread '",
    "panicked at",
    "internal compiler error",
    "has overflowed its stack",
)

# Markers that indicate a NYI / codegen hard-abort in build output.
_NYI_MARKERS = (
    "E_NOT_YET_IMPLEMENTED",
    "E_MIR",
    "E_CODEGEN",
)

# Markers that indicate a Hew runtime panic/abort in binary output.
_RUNTIME_ABORT_MARKERS = (
    "PANIC",
    "panicked at",
    "assertion failed",
    "thread '",
)

# Pattern that parses the leading // EXPECT: <stdout> annotation.
_EXPECT_RE = re.compile(r"^//\s*EXPECT:\s*(.*)", re.MULTILINE)
_EXIT_RE = re.compile(r"^//\s*EXIT:\s*(\d+)", re.MULTILINE)

# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------


@dataclass
class Verdict:
    """Outcome of processing one candidate."""

    path: Path
    classification: str  # see taxonomy above
    detail: str = ""  # human-readable context (truncated)


# ---------------------------------------------------------------------------
# Core classification helpers
# ---------------------------------------------------------------------------


def _contains_any(text: str, markers: tuple[str, ...]) -> bool:
    return any(m in text for m in markers)


def _clip(text: str, limit: int = 400) -> str:
    return text[:limit] + "…" if len(text) > limit else text


def _read_annotations(source_text: str) -> tuple[Optional[str], Optional[int]]:
    """Return (expected_stdout, expected_exit) from // EXPECT:/EXIT: annotations."""
    expect_match = _EXPECT_RE.search(source_text)
    exit_match = _EXIT_RE.search(source_text)
    expected_stdout = expect_match.group(1) if expect_match else None
    expected_exit = int(exit_match.group(1)) if exit_match else None
    return expected_stdout, expected_exit


# ---------------------------------------------------------------------------
# Frontend gate: hew check
# ---------------------------------------------------------------------------


def _frontend_check(
    hew: Path, src: Path, timeout_s: float, hew_std: Optional[str]
) -> tuple[str, str]:
    """Distinguish expected invalid fuzz input from compiler failure."""
    env = dict(os.environ)
    if hew_std:
        env["HEW_STD"] = hew_std
    try:
        result = subprocess.run(
            [str(hew), "check", str(src)],
            capture_output=True,
            text=True,
            errors="replace",
            timeout=timeout_s,
            env=env,
        )
    except subprocess.TimeoutExpired:
        return "timeout", "frontend deadline exceeded"
    except OSError as error:
        return "build-ice", str(error)
    if result.returncode == 0:
        return "clean", ""
    if result.returncode == 1:
        return "frontend-reject", ""
    return "build-ice", _clip(result.stdout + result.stderr)


# ---------------------------------------------------------------------------
# Build step: hew build -o <bin> <src>
# ---------------------------------------------------------------------------


def _build(
    hew: Path,
    src: Path,
    bin_path: Path,
    timeout_s: float,
    hew_std: Optional[str],
) -> tuple[int, str]:
    """Compile src to bin_path.

    Return the compiler exit code and combined diagnostic output.
    """
    env = dict(os.environ)
    if hew_std:
        env["HEW_STD"] = hew_std

    try:
        result = subprocess.run(
            [str(hew), "build", "-o", str(bin_path), str(src)],
            capture_output=True,
            text=True,
            timeout=timeout_s,
            env=env,
        )
        return result.returncode, result.stdout + result.stderr
    except subprocess.TimeoutExpired:
        return -signal.SIGKILL, "build-timeout"
    except Exception as exc:
        return 1, f"build-exception: {exc}"


def _classify_build_failure(combined_output: str) -> str:
    """Given a non-zero build, return the verdict classification."""
    if _contains_any(combined_output, _BUILD_ICE_MARKERS):
        return "build-ice"
    if _contains_any(combined_output, _NYI_MARKERS):
        return "nyi-codegen"
    # Linker "undefined symbol: main" → this is a library file (no entry point),
    # not a runnable program.  Classify as frontend-reject so library fixtures
    # (checked with `hew check` only, not `hew build`) are not spurious failures.
    if "undefined symbol: main" in combined_output:
        return "frontend-reject"
    # Non-zero without known markers: treat as build-ice (fail-closed).
    return "build-ice"


# ---------------------------------------------------------------------------
# Run step: execute binary directly under process-group + timeout + output cap
# ---------------------------------------------------------------------------


def _run_binary(
    bin_path: Path,
    timeout_s: float,
    output_cap: int,
    hew_std: Optional[str],
) -> tuple[int, str, str, bool, bool]:
    """Execute the binary directly (NOT `hew run`).

    Returns (returncode, stdout_text, stderr_text, timed_out, output_capped).

    Process-group kill: start_new_session=True + os.killpg on timeout, so
    subprocesses cannot outlive a timed-out candidate. HEW_WORKERS=2 bounds
    scheduler concurrency.
    """
    env = dict(os.environ)
    env["HEW_WORKERS"] = "2"
    if hew_std:
        env["HEW_STD"] = hew_std

    try:
        proc = subprocess.Popen(
            [str(bin_path)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            start_new_session=True,
            env=env,
        )
    except OSError as error:
        return 1, "", f"run-launch-error: {error}", False, False

    import select

    chunks = {proc.stdout: bytearray(), proc.stderr: bytearray()}
    pending = list(chunks)
    deadline = time.monotonic() + timeout_s
    timed_out = False
    output_capped = False
    try:
        while pending or proc.poll() is None:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                timed_out = True
                break
            readable, _, _ = select.select(pending, [], [], min(0.1, remaining))
            for stream in readable:
                chunk = stream.read1(4096)
                if not chunk:
                    pending.remove(stream)
                    continue
                available = output_cap - len(chunks[stream])
                chunks[stream].extend(chunk[:available])
                if len(chunk) > available:
                    output_capped = True
                    break
            if output_capped:
                break
    finally:
        if timed_out or output_capped or proc.poll() is None:
            # The group can outlive its leader. Its established ID is the
            # child's original PID even after the leader has exited.
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
        proc.wait()
        for stream in chunks:
            stream.close()

    return (
        proc.returncode,
        chunks[proc.stdout].decode("utf-8", errors="replace"),
        chunks[proc.stderr].decode("utf-8", errors="replace"),
        timed_out,
        output_capped,
    )


def _classify_run(
    rc: int,
    stdout: str,
    stderr: str,
    timed_out: bool,
    output_capped: bool,
    expected_stdout: Optional[str],
    expected_exit: Optional[int],
) -> tuple[str, str]:
    """Classify raw execution, honouring any explicit source expectation."""
    if timed_out:
        return "timeout", "wall-clock exceeded"

    if output_capped:
        return "output-cap", "stdout/stderr cap exceeded"

    # Signal: rc < 0 on Unix means the process was killed by a signal.
    if rc < 0:
        sig_num = -rc
        sig_name = (
            signal.Signals(sig_num).name
            if sig_num in signal.Signals._value2member_map_
            else str(sig_num)
        )  # type: ignore[attr-defined]
        return "runtime-crash", f"killed by signal {sig_num} ({sig_name})"

    combined = stdout + stderr

    # Non-zero with Hew/Rust abort markers → runtime-abort (always, both modes).
    if rc != 0 and _contains_any(combined, _RUNTIME_ABORT_MARKERS):
        return "runtime-abort", _clip(combined)

    if expected_exit is not None and rc != expected_exit:
        return (
            "runtime-abort",
            f"exit {rc}, expected {expected_exit}; {_clip(combined)}",
        )

    # Check expected stdout if annotated.
    if expected_stdout is not None:
        actual = stdout.rstrip("\n")
        expected = expected_stdout.rstrip("\n")
        if actual != expected:
            return "wrong-output", f"expected: {repr(expected)} actual: {repr(actual)}"

    return "clean", ""


# ---------------------------------------------------------------------------
# Per-candidate processing
# ---------------------------------------------------------------------------


def process_candidate(
    src: Path,
    hew: Path,
    timeout_s: float,
    output_cap: int,
    hew_std: Optional[str],
    workdir: Path,
) -> Verdict:
    """Replay one raw source candidate through checking, linking and execution."""
    # Read source for annotation extraction.
    try:
        source_text = src.read_text(encoding="utf-8", errors="replace")
    except OSError as exc:
        return Verdict(src, "frontend-reject", f"read-error: {exc}")

    expected_stdout, expected_exit = _read_annotations(source_text)

    # Frontend gate.
    classification, detail = _frontend_check(hew, src, timeout_s, hew_std)
    if classification != "clean":
        return Verdict(src, classification, detail)

    # Build step.
    bin_path = workdir / (src.stem + "_oracle_bin")
    build_rc, build_out = _build(hew, src, bin_path, timeout_s, hew_std)

    if build_rc != 0:
        cls = _classify_build_failure(build_out)
        return Verdict(src, cls, _clip(build_out))

    if not bin_path.exists():
        return Verdict(src, "build-ice", "build exited 0 but no binary produced")

    try:
        os.chmod(str(bin_path), 0o755)
        # Run step.
        rc, stdout, stderr, timed_out, output_capped = _run_binary(
            bin_path, timeout_s, output_cap, hew_std
        )
        cls, detail = _classify_run(
            rc,
            stdout,
            stderr,
            timed_out,
            output_capped,
            expected_stdout,
            expected_exit,
        )
        return Verdict(src, cls, detail)
    finally:
        try:
            bin_path.unlink(missing_ok=True)
        except OSError:
            pass


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hew", type=Path, required=True)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--timeout", type=float, default=30)
    parser.add_argument("--output-cap", type=int, default=1024 * 1024)
    parser.add_argument("--hew-std")
    args = parser.parse_args()
    if args.timeout <= 0 or args.output_cap <= 0:
        parser.error("timeout and output cap must be positive")
    candidates = sorted(path for path in args.corpus.rglob("*") if path.is_file())
    if not candidates:
        parser.error("the raw fuzz corpus contains no files")
    failed = 0
    with tempfile.TemporaryDirectory(prefix="hew-fuzz-replay-") as directory:
        for index, source in enumerate(candidates):
            # libFuzzer stores extensionless hash names. The compiler's source
            # entry remains a .hew file, isolated from neighbouring candidates.
            candidate = Path(directory) / f"candidate-{index}.hew"
            candidate.write_bytes(source.read_bytes())
            verdict = process_candidate(
                candidate,
                args.hew.resolve(),
                args.timeout,
                args.output_cap,
                args.hew_std,
                Path(directory),
            )
            if verdict.classification not in {"clean", "frontend-reject"}:
                failed += 1
                print(f"FAIL {source}: {verdict.classification}: {verdict.detail}")
    print(f"Raw fuzz replay: {len(candidates)} inputs, {failed} failures")
    return int(failed != 0)


if __name__ == "__main__":
    sys.exit(main())
