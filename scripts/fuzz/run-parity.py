#!/usr/bin/env python3
"""Differential execution of generated Hew programs on native and sandbox targets.

Each program is compiled and run natively (O0 and O2), as a WASI module under
wasmtime, and as a sandbox VM package. Stdout, exit status and the failure
category must agree across every engine and with the generator's model of the
program when it has one. A refusal on one engine is a mismatch, not a skip:
parity means every target compiles and runs the program with the same result.

Programs come from `parity_gen` (hew-parity-gen): a deterministic seed range,
or a libFuzzer corpus of `fuzz_parity_gen` inputs rendered through the same
generator. The expensive compile-and-run work stays here, outside libFuzzer.
"""

import argparse
import concurrent.futures
import importlib.util
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

_spec = importlib.util.spec_from_file_location(
    "run_oracle", Path(__file__).with_name("run-oracle.py")
)
oracle = importlib.util.module_from_spec(_spec)
sys.modules[_spec.name] = oracle
_spec.loader.exec_module(oracle)

ROOT = Path(__file__).resolve().parents[2]
VM_HOST = Path(__file__).with_name("parity-vm-host.mjs")
SEED = "42"

# ---------------------------------------------------------------------------
# Observations
# ---------------------------------------------------------------------------


@dataclass
class Outcome:
    """What one engine observed for one program."""

    engine: str
    # ok | trap | compile-reject | compile-ice | timeout | crash | output-cap | host-error
    kind: str
    stdout: str = ""
    exit_code: Optional[int] = None
    stderr: str = ""
    detail: str = ""

    def key(self) -> tuple:
        return (self.kind, self.stdout, self.exit_code, failure_kinds(self.stderr))


_FAILURE_RE = re.compile(r"failure: (\w+)")


def failure_kinds(stderr: str) -> tuple:
    """The ordered language failure kinds reported on stderr.

    Every engine names a failure as `failure: <Kind>`; locations, actor ids
    and other host presentation around it are not compared.
    """
    return tuple(_FAILURE_RE.findall(stderr))


def _clip(text: str, limit: int = 600) -> str:
    return text if len(text) <= limit else text[:limit] + "…"


def _env() -> dict:
    env = dict(os.environ)
    env["HEW_SEED"] = SEED
    env["NO_COLOR"] = "1"
    env.setdefault("HEW_STD", str(ROOT / "std"))
    return env


def _classify_exit(
    engine: str, rc: int, stdout: str, stderr: str, timed_out: bool, capped: bool
) -> Outcome:
    if timed_out:
        return Outcome(engine, "timeout", stdout, None, stderr, "deadline exceeded")
    if capped:
        return Outcome(engine, "output-cap", stdout, None, stderr)
    if rc < 0:
        return Outcome(engine, "crash", stdout, rc, stderr, f"signal {-rc}")
    return Outcome(engine, "ok" if rc == 0 else "trap", stdout, rc, stderr)


def _compile(hew: Path, args: list, timeout: float) -> tuple[Optional[str], str]:
    """Run a compiler command; return (failure kind or None, diagnostics)."""
    try:
        result = subprocess.run(
            [str(hew), *args],
            capture_output=True,
            text=True,
            errors="replace",
            timeout=timeout,
            env=_env(),
        )
    except subprocess.TimeoutExpired:
        return "timeout", "compile deadline exceeded"
    combined = result.stdout + result.stderr
    if result.returncode == 0:
        return None, combined
    if (
        oracle._contains_any(combined, oracle._BUILD_ICE_MARKERS)
        or result.returncode != 1
    ):
        return "compile-ice", combined
    return "compile-reject", combined


def run_native(
    hew: Path, src: Path, work: Path, opt: str, timeout: float, cap: int
) -> Outcome:
    engine = f"native-O{opt}"
    binary = work / f"{src.stem}-O{opt}"
    failure, diagnostics = _compile(
        hew, ["build", "--opt-level", opt, "-o", str(binary), str(src)], timeout
    )
    if failure:
        return Outcome(engine, failure, detail=_clip(diagnostics))
    try:
        rc, out, err, timed_out, capped = oracle._run_binary(binary, timeout, cap, None)
    finally:
        binary.unlink(missing_ok=True)
    return _classify_exit(engine, rc, out, err, timed_out, capped)


def run_wasi(
    hew: Path, wasmtime: str, src: Path, work: Path, timeout: float, cap: int
) -> Outcome:
    engine = "wasi"
    module = work / f"{src.stem}.wasm"
    failure, diagnostics = _compile(
        hew, ["build", "--target", "wasm32-wasi", "-o", str(module), str(src)], timeout
    )
    if failure:
        return Outcome(engine, failure, detail=_clip(diagnostics))
    try:
        rc, out, err, timed_out, capped = oracle._run_binary(
            Path(wasmtime),
            timeout,
            cap,
            None,
            ("run", "--env", f"HEW_SEED={SEED}", str(module)),
        )
    finally:
        module.unlink(missing_ok=True)
    return _classify_exit(engine, rc, out, err, timed_out, capped)


class VmHost:
    """One persistent Node process per worker thread, replaced after a deadline."""

    def __init__(self, timeout: float):
        self.timeout = timeout
        self.proc: Optional[subprocess.Popen] = None

    def _start(self) -> None:
        env = _env()
        env["HEWPATH"] = str(ROOT)
        self.proc = subprocess.Popen(
            ["node", str(VM_HOST)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            start_new_session=True,
            env=env,
        )
        ready = self._read_line(120)
        if ready is None or not json.loads(ready).get("ready"):
            self.close()
            raise RuntimeError("sandbox VM host did not start")

    def _read_line(self, timeout: float) -> Optional[str]:
        result: list = []
        reader = threading.Thread(
            target=lambda: result.append(self.proc.stdout.readline())
        )
        reader.daemon = True
        reader.start()
        reader.join(timeout)
        if reader.is_alive() or not result or not result[0]:
            return None
        return result[0]

    def close(self) -> None:
        if self.proc is not None:
            try:
                os.killpg(self.proc.pid, 9)
            except ProcessLookupError:
                pass
            self.proc.wait()
            self.proc = None

    def run(self, source: str) -> Outcome:
        if self.proc is None:
            self._start()
        self.proc.stdin.write(json.dumps({"source": source}) + "\n")
        self.proc.stdin.flush()
        line = self._read_line(self.timeout)
        if line is None:
            self.close()
            return Outcome("vm", "timeout", detail="deadline exceeded")
        reply = json.loads(line)
        if reply["phase"] == "host":
            return Outcome("vm", "host-error", detail=_clip(reply["error"]))
        if reply["phase"] == "compile":
            if "error" in reply:
                return Outcome("vm", "compile-ice", detail=_clip(reply["error"]))
            return Outcome(
                "vm", "compile-reject", detail=_clip(json.dumps(reply["diagnostics"]))
            )
        status = reply["status"]
        stdout, stderr = reply["stdout"], reply["stderr"]
        # The parity runner's contract: a run without an exit code fails with 1.
        code = 1 if reply["exit_code"] is None else reply["exit_code"]
        detail = _clip(
            json.dumps({"status": status, "failures": reply.get("failures")})
        )
        if status in ("sandbox_rejected", "compile_error") or reply.get("rejections"):
            return Outcome(
                "vm",
                "compile-reject",
                stdout,
                code,
                stderr,
                _clip(json.dumps(reply.get("rejections"))),
            )
        if status == "budget_exhausted":
            return Outcome(
                "vm", "timeout", stdout, None, stderr, "step budget exhausted"
            )
        if status in ("runtime_failure",) and not failure_kinds(stderr):
            return Outcome("vm", "host-error", stdout, code, stderr, detail)
        return Outcome(
            "vm",
            "ok" if code == 0 else "trap",
            stdout,
            code,
            stderr,
            detail if code else "",
        )


# ---------------------------------------------------------------------------
# Comparison
# ---------------------------------------------------------------------------


@dataclass
class Result:
    name: str
    outcomes: list = field(default_factory=list)
    expected: Optional[dict] = None
    mismatch: Optional[str] = None


def compare(result: Result) -> None:
    """Record the first disagreement, if any, as a one-line reason."""
    reference = result.outcomes[0]
    for outcome in result.outcomes:
        if outcome.kind not in ("ok", "trap"):
            result.mismatch = f"{outcome.engine}: {outcome.kind}"
            return
    for outcome in result.outcomes[1:]:
        if outcome.key() != reference.key():
            result.mismatch = f"{outcome.engine} differs from {reference.engine}"
            return
    expected = result.expected
    if expected is not None:
        kinds = failure_kinds(reference.stderr)
        if reference.stdout != expected["stdout"]:
            result.mismatch = "all engines differ from the generator's model (stdout)"
        elif (reference.exit_code == 0) != (expected["trap"] is None):
            result.mismatch = "all engines differ from the generator's model (exit)"
        elif expected["trap"] is not None and (
            not kinds or kinds[-1] != expected["trap"]
        ):
            result.mismatch = (
                "all engines differ from the generator's model (failure kind)"
            )


def render_programs(
    generator: Path, out: Path, seeds: Optional[str], corpus: Optional[Path]
) -> list[Path]:
    args = [str(generator), "--out", str(out)]
    if seeds:
        args += ["--seeds", seeds]
    if corpus:
        args += ["--corpus", str(corpus)]
    subprocess.run(args, check=True)
    return sorted(out.glob("*.hew"))


def evaluate(
    src: Path,
    hew: Path,
    wasmtime: Optional[str],
    engines: list,
    host: VmHost,
    work: Path,
    timeout: float,
    cap: int,
) -> Result:
    expected_path = src.with_suffix(".json")
    expected = json.loads(expected_path.read_text()) if expected_path.exists() else None
    result = Result(src.name, expected=expected)
    for engine in engines:
        if engine == "native-O0":
            result.outcomes.append(run_native(hew, src, work, "0", timeout, cap))
        elif engine == "native-O2":
            result.outcomes.append(run_native(hew, src, work, "2", timeout, cap))
        elif engine == "wasi":
            result.outcomes.append(run_wasi(hew, wasmtime, src, work, timeout, cap))
        elif engine == "vm":
            result.outcomes.append(host.run(src.read_text()))
    compare(result)
    return result


def describe(result: Result) -> str:
    lines = [f"MISMATCH {result.name}: {result.mismatch}"]
    for outcome in result.outcomes:
        lines.append(
            f"  {outcome.engine}: {outcome.kind} exit={outcome.exit_code} "
            f"stdout={_clip(outcome.stdout, 300)!r} stderr={_clip(outcome.stderr, 300)!r}"
            + (f" detail={outcome.detail!r}" if outcome.detail else "")
        )
    if result.expected is not None:
        lines.append(
            f"  model: stdout={_clip(result.expected['stdout'], 300)!r} trap={result.expected['trap']!r}"
        )
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hew", type=Path, required=True)
    parser.add_argument(
        "--generator", type=Path, required=True, help="the parity_gen binary"
    )
    parser.add_argument(
        "--seeds", help="seed range START..END rendered by the generator"
    )
    parser.add_argument(
        "--corpus", type=Path, help="libFuzzer corpus of fuzz_parity_gen inputs"
    )
    parser.add_argument(
        "--programs", type=Path, help="an existing directory of rendered programs"
    )
    parser.add_argument("--engines", default="native-O0,native-O2,wasi,vm")
    parser.add_argument("--jobs", type=int, default=max(1, (os.cpu_count() or 2) // 2))
    parser.add_argument("--timeout", type=float, default=30)
    parser.add_argument("--output-cap", type=int, default=1024 * 1024)
    parser.add_argument("--keep", type=Path, help="copy mismatching programs here")
    args = parser.parse_args()

    engines = [engine for engine in args.engines.split(",") if engine]
    wasmtime = None
    if "wasi" in engines:
        wasmtime = shutil.which("wasmtime") or str(
            Path.home() / ".wasmtime/bin/wasmtime"
        )
        if not Path(wasmtime).exists() and not shutil.which(wasmtime):
            parser.error("the wasi engine needs wasmtime on PATH or in ~/.wasmtime/bin")

    with tempfile.TemporaryDirectory(prefix="hew-parity-") as directory:
        work = Path(directory)
        if args.programs:
            programs = sorted(args.programs.glob("*.hew"))
        else:
            if not args.seeds and not args.corpus:
                parser.error("one of --seeds, --corpus or --programs is required")
            programs = render_programs(
                args.generator, work / "programs", args.seeds, args.corpus
            )
        if not programs:
            parser.error("no programs to run")

        local = threading.local()
        hosts: list = []
        hosts_lock = threading.Lock()

        def task(src: Path) -> Result:
            if not hasattr(local, "host"):
                local.host = VmHost(args.timeout)
                local.work = Path(tempfile.mkdtemp(dir=work))
                with hosts_lock:
                    hosts.append(local.host)
            return evaluate(
                src,
                args.hew.resolve(),
                wasmtime,
                engines,
                local.host,
                local.work,
                args.timeout,
                args.output_cap,
            )

        started = time.monotonic()
        mismatches = 0
        try:
            with concurrent.futures.ThreadPoolExecutor(args.jobs) as pool:
                for result in pool.map(task, programs):
                    if result.mismatch:
                        mismatches += 1
                        print(describe(result), flush=True)
                        if args.keep:
                            args.keep.mkdir(parents=True, exist_ok=True)
                            source = next(p for p in programs if p.name == result.name)
                            shutil.copy(source, args.keep / source.name)
                            model = source.with_suffix(".json")
                            if model.exists():
                                shutil.copy(model, args.keep / model.name)
        finally:
            for host in hosts:
                host.close()
        elapsed = time.monotonic() - started
    total = len(programs)
    print(
        f"Parity: {total} programs, {total - mismatches} agree, {mismatches} mismatches, "
        f"{elapsed:.1f}s ({total / elapsed if elapsed else 0:.2f} programs/s, engines {','.join(engines)})"
    )
    return int(mismatches != 0)


if __name__ == "__main__":
    sys.exit(main())
