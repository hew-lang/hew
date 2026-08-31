#!/usr/bin/env python3
"""Fuzz-to-run completeness oracle.

For each candidate .hew file: run the frontend gate (parse → check), and if
checker-valid, compile a native binary and execute it directly (NOT `hew run`,
which masks signals — see E3 in the plan).  Classify the outcome fail-closed:
anything not provably clean is a failure unless registered in expected-failures.

Exit 0: no unexpected failures.
Exit 1: at least one unexpected failure (or a ratchet violation).

Classification taxonomy (fail-closed):
  frontend-reject  check non-zero OR non-runnable build       → FAIL
  clean            build exit 0 AND binary exit 0 (or EXIT:)  → pass
  nyi-codegen      build non-zero with NYI/MIR/CODEGEN marker → FAIL
  build-ice        build output contains Rust panic marker     → FAIL
  runtime-crash    binary returncode < 0 (signal)              → FAIL
  runtime-abort    binary exit non-zero with PANIC/abort       → FAIL
  timeout          wall-clock exceeded; process-group killed   → FAIL
  output-cap       stdout/stderr exceeded cap                  → FAIL
  wrong-output     self-check EXPECT: mismatch                 → FAIL

Self-check convention: a .hew file may carry a leading comment
  // EXPECT: <exact stdout>
and optionally
  // EXIT: <n>
The harness asserts exact stdout match and (if present) exit code.
Files without these annotations: clean = build-0 + run-0 within bounds.

Ratchet (--regressions mode):
  tests/fuzz-oracle/expected-failures.txt pins bare filenames from the
  ratcheted candidate sources (vertical-slice or regressions) to one exact
  failure classification, each annotated with # <root-cause>; issue: #NNNN.
  - A listed file observed clean          → recovery report (strict fails).
  - A listed file with another fail class → class drift → gate failure.
  - An unlisted file that FAILS           → unexpected-fail → gate failure.

Positive-corpus rejection ratchet:
  scripts/fuzz/expected-reject.txt lists exact repo-relative paths from
  tests/vertical-slice/accept that are currently rejected by the frontend or
  cannot build as standalone programs.
  - A listed file that stops being rejected → recovery report (strict accounting fails).
  - An unlisted file that is rejected       → unexpected-reject → gate failure.
  In --full mode the ratchet is NOT applied to cargo-fuzz corpus files (they
  are raw bytes; classification is advisory only).
"""

from __future__ import annotations

import argparse
import json
import os
import re
import signal
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "lib"))
from corpus_nonempty import check_nonempty  # noqa: E402

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

    @property
    def is_fail(self) -> bool:
        return self.classification != "clean"


@dataclass
class OracleStats:
    total: int = 0
    frontend_reject: int = 0
    clean: int = 0
    fails: dict[str, int] = field(default_factory=dict)

    def record(self, v: Verdict) -> None:
        self.total += 1
        if v.classification == "frontend-reject":
            self.frontend_reject += 1
        elif v.classification == "clean":
            self.clean += 1
        else:
            self.fails[v.classification] = self.fails.get(v.classification, 0) + 1

    @property
    def total_fail(self) -> int:
        return self.frontend_reject + sum(self.fails.values())


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


def _frontend_check(hew: Path, src: Path, timeout_s: float) -> bool:
    """Return True if `hew check <src>` exits 0 (checker-valid).

    Any non-zero exit (parse error, type error) → frontend-reject.
    A timeout → treated as frontend-reject (we cannot classify further).
    """
    try:
        result = subprocess.run(
            [str(hew), "check", str(src)],
            capture_output=True,
            timeout=timeout_s,
        )
        return result.returncode == 0
    except subprocess.TimeoutExpired:
        return False
    except Exception:
        return False


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

    Returns (returncode, combined_output). Build is NOT memory-capped (see
    run.sh:146-156: ulimit -v around compile/link fails ld.lld's libhew.a
    mmap; only the *run* step is capped).
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
    actor scheduler worker threads spawned by the binary are also reaped.
    HEW_WORKERS=2 pins the scheduler to two threads (host-independent; see
    run.sh:161-168).
    """
    env = dict(os.environ)
    env["HEW_WORKERS"] = "2"
    if hew_std:
        env["HEW_STD"] = hew_std

    stdout_chunks: list[bytes] = []
    stderr_chunks: list[bytes] = []
    stdout_capped = False
    stderr_capped = False

    try:
        proc = subprocess.Popen(
            [str(bin_path)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            start_new_session=True,  # new process group for pgkill on timeout
            env=env,
        )
    except Exception as exc:
        return 1, "", f"run-launch-error: {exc}", False, False

    deadline = time.monotonic() + timeout_s
    timed_out = False

    try:
        # Poll with a short sleep so we can enforce the output cap incrementally.
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                timed_out = True
                break

            # Non-blocking read from both pipes.
            import select as _select

            readable, _, _ = _select.select(
                [proc.stdout, proc.stderr], [], [], min(0.1, remaining)
            )
            for fh in readable:
                chunk = fh.read1(4096)  # type: ignore[attr-defined]
                if not chunk:
                    continue
                if fh is proc.stdout:
                    if not stdout_capped:
                        stdout_chunks.append(chunk)
                        if sum(len(c) for c in stdout_chunks) > output_cap:
                            stdout_capped = True
                else:
                    if not stderr_capped:
                        stderr_chunks.append(chunk)
                        if sum(len(c) for c in stderr_chunks) > output_cap:
                            stderr_capped = True

            if proc.poll() is not None:
                # Drain remaining output.
                try:
                    out_tail, err_tail = proc.communicate(timeout=2.0)
                    if not stdout_capped:
                        stdout_chunks.append(out_tail)
                    if not stderr_capped:
                        stderr_chunks.append(err_tail)
                except subprocess.TimeoutExpired:
                    pass
                break

    finally:
        if timed_out or proc.poll() is None:
            # Kill the entire process group.
            try:
                pgid = os.getpgid(proc.pid)
                os.killpg(pgid, signal.SIGKILL)
            except OSError:
                pass
            try:
                proc.wait(timeout=5.0)
            except subprocess.TimeoutExpired:
                pass

    rc = proc.returncode if proc.returncode is not None else -signal.SIGKILL
    stdout_text = b"".join(stdout_chunks).decode("utf-8", errors="replace")
    stderr_text = b"".join(stderr_chunks).decode("utf-8", errors="replace")
    output_capped = stdout_capped or stderr_capped

    return rc, stdout_text, stderr_text, timed_out, output_capped


def _classify_run(
    rc: int,
    stdout: str,
    stderr: str,
    timed_out: bool,
    output_capped: bool,
    expected_stdout: Optional[str],
    expected_exit: Optional[int],
    strict_exit: bool = True,
) -> tuple[str, str]:
    """Return (classification, detail) for a completed binary run.

    strict_exit=True (default, regression corpus): require exit 0 OR the
    explicit // EXIT: annotation value.  A non-zero exit with no annotation is
    a runtime-abort failure.

    strict_exit=False (vertical-slice/accept): only classify as failure for
    definitive structural failures (signal, PANIC/abort marker, timeout,
    output-cap, wrong EXPECT output).  A non-zero exit without annotations and
    without any abort marker is accepted as clean — these programs intentionally
    exit with codes like 42 as their correctness signal.
    """
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

    if strict_exit:
        # Regression corpus mode: require exit 0 OR explicit // EXIT: annotation.
        target_exit = expected_exit if expected_exit is not None else 0
        if rc != target_exit:
            return (
                "runtime-abort",
                f"exit {rc}, expected {target_exit}; {_clip(combined)}",
            )
    else:
        # Vertical-slice mode: only check exit if explicitly annotated.
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
    strict_exit: bool = True,
) -> Verdict:
    """Run the full pipeline for one candidate .hew file.

    strict_exit=True: classify non-zero exit (without // EXIT:) as runtime-abort.
    strict_exit=False: only flag signal/panic/NYI/timeout — unannotated non-zero
    exit is clean (used for vertical-slice accept files that intentionally exit
    with codes like 42 as their correctness signal).
    """
    # Read source for annotation extraction.
    try:
        source_text = src.read_text(encoding="utf-8", errors="replace")
    except OSError as exc:
        return Verdict(src, "frontend-reject", f"read-error: {exc}")

    expected_stdout, expected_exit = _read_annotations(source_text)

    # Frontend gate.
    if not _frontend_check(hew, src, timeout_s):
        return Verdict(src, "frontend-reject", "")

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
            strict_exit=strict_exit,
        )
        return Verdict(src, cls, detail)
    finally:
        try:
            bin_path.unlink(missing_ok=True)
        except OSError:
            pass


# ---------------------------------------------------------------------------
# Corpus enumeration
# ---------------------------------------------------------------------------


def _iter_hew_files(root: Path) -> list[Path]:
    """Recursively collect .hew files under root."""
    return sorted(p for p in root.rglob("*.hew") if p.is_file())


def collect_candidates(
    repo_root: Path,
    regressions_dir: Path,
    full_mode: bool,
    vertical_slice_dir: Optional[Path] = None,
) -> tuple[list[Path], list[Path], list[Path]]:
    """Return (fuzz_corpus_files, vertical_slice_files, regression_files).

    In regressions mode (full_mode=False) fuzz_corpus_files is empty.
    Pass vertical_slice_dir=<empty-dir> to skip source 2 (useful in self-tests).
    """
    fuzz_corpus: list[Path] = []
    if full_mode:
        corpus_root = repo_root / "hew-parser" / "fuzz" / "corpus"
        if corpus_root.exists():
            for corpus_dir in sorted(corpus_root.iterdir()):
                if corpus_dir.is_dir():
                    fuzz_corpus.extend(
                        sorted(p for p in corpus_dir.iterdir() if p.is_file())
                    )

    if vertical_slice_dir is None:
        vertical_slice_dir = repo_root / "tests" / "vertical-slice" / "accept"
    vertical_slice = (
        _iter_hew_files(vertical_slice_dir) if vertical_slice_dir.exists() else []
    )

    regressions = _iter_hew_files(regressions_dir) if regressions_dir.exists() else []

    return fuzz_corpus, vertical_slice, regressions


# ---------------------------------------------------------------------------
# Ratchet: expected-failures.txt
# ---------------------------------------------------------------------------


KNOWN_FAILURE_CLASSES = frozenset(
    {
        "frontend-reject",
        "nyi-codegen",
        "build-ice",
        "runtime-crash",
        "runtime-abort",
        "timeout",
        "output-cap",
        "wrong-output",
    }
)


def _load_expected_failures(expected_failures_path: Path) -> dict[str, str]:
    """Parse exact `<filename> <failure-class>` expected-failure entries."""
    if not expected_failures_path.exists():
        return {}
    expected: dict[str, str] = {}
    for line_number, line in enumerate(
        expected_failures_path.read_text().splitlines(), 1
    ):
        stripped = line.split("#")[0].strip()
        if not stripped:
            continue
        fields = stripped.split()
        if len(fields) != 2:
            raise ValueError(
                f"{expected_failures_path}:{line_number}: expected "
                "'<filename> <failure-class>'"
            )
        name, classification = fields
        if Path(name).name != name or not name.endswith(".hew"):
            raise ValueError(
                f"{expected_failures_path}:{line_number}: expected a bare .hew filename: {name}"
            )
        if classification not in KNOWN_FAILURE_CLASSES:
            raise ValueError(
                f"{expected_failures_path}:{line_number}: unsupported failure class "
                f"{classification!r}"
            )
        if name in expected:
            raise ValueError(
                f"{expected_failures_path}:{line_number}: duplicate expected failure: {name}"
            )
        expected[name] = classification
    return expected


def _load_expected_rejects(expected_rejects_path: Path) -> set[str]:
    """Parse exact positive-corpus rejection identities."""
    if not expected_rejects_path.exists():
        return set()
    expected: set[str] = set()
    for line_number, line in enumerate(
        expected_rejects_path.read_text().splitlines(), 1
    ):
        identity = line.split("#")[0].strip()
        if not identity:
            continue
        if len(identity.split()) != 1:
            raise ValueError(
                f"{expected_rejects_path}:{line_number}: expected one fixture identity"
            )
        if identity in expected:
            raise ValueError(
                f"{expected_rejects_path}:{line_number}: duplicate expected rejection: {identity}"
            )
        expected.add(identity)
    return expected


def _fixture_identity(src: Path, corpus_root: Path, prefix: Path) -> str:
    """Return the stable manifest identity for a corpus fixture."""
    return (prefix / src.relative_to(corpus_root)).as_posix()


# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------


def _print_verdict(v: Verdict, *, verbose: bool) -> None:
    status = "FAIL" if v.is_fail else v.classification.upper()
    if verbose or v.is_fail:
        detail = f"  {v.detail}" if v.detail else ""
        print(f"  {status:20s} {v.path.name}{detail}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Fuzz-to-run completeness oracle — compile and run every checker-valid .hew file.",
    )
    p.add_argument(
        "--hew",
        type=Path,
        default=None,
        help="Path to the hew binary. Default: Cargo's resolved debug output.",
    )
    p.add_argument(
        "--repo-root",
        type=Path,
        default=None,
        help="Repo root. Default: two directories above this script.",
    )
    p.add_argument(
        "--regressions-dir",
        type=Path,
        default=None,
        help="Directory containing regression .hew files. Default: <repo-root>/tests/fuzz-oracle/regressions.",
    )
    p.add_argument(
        "--expected-failures",
        type=Path,
        default=None,
        help="Expected-failures list. Default: <regressions-dir>/../expected-failures.txt.",
    )
    p.add_argument(
        "--expected-rejects",
        type=Path,
        default=None,
        help=(
            "Expected positive-corpus rejections. Default: "
            "<repo-root>/scripts/fuzz/expected-reject.txt."
        ),
    )
    p.add_argument(
        "--vertical-slice-dir",
        type=Path,
        default=None,
        help="Override the vertical-slice/accept directory. Default: <repo-root>/tests/vertical-slice/accept. Pass an empty dir to skip source 2 (useful in self-tests).",
    )
    p.add_argument(
        "--full",
        action="store_true",
        help="Also scan cargo-fuzz corpus (source 1). Default: regressions mode only.",
    )
    p.add_argument(
        "--strict-recoveries",
        action="store_true",
        help="fail when an expected failure or rejection now passes",
    )
    p.add_argument(
        "--min-candidates",
        type=int,
        default=None,
        help=(
            "Minimum number of ratcheted candidates (vertical-slice + regressions) "
            "this run must have collected. Required whenever --regressions-dir or "
            "--vertical-slice-dir is overridden; use it to guard the caller-owned "
            "corpus against an empty selection."
        ),
    )
    p.add_argument(
        "--timeout",
        type=float,
        default=15.0,
        help="Per-candidate wall-clock timeout in seconds (default: 15).",
    )
    p.add_argument(
        "--output-cap",
        type=int,
        default=65536,
        help="Per-candidate stdout+stderr cap in bytes (default: 65536).",
    )
    p.add_argument(
        "--report",
        type=Path,
        default=None,
        help="Write JSON report to this path.",
    )
    p.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Print every candidate (not just failures).",
    )
    return p


def main() -> int:
    args = build_arg_parser().parse_args()

    custom_corpus = (
        args.regressions_dir is not None or args.vertical_slice_dir is not None
    )
    if custom_corpus and args.min_candidates is None:
        print(
            "error: --regressions-dir/--vertical-slice-dir point at a corpus this "
            "gate has no floor for; pass --min-candidates <n>",
            file=sys.stderr,
        )
        print(
            "       An oracle that collected no candidates finds no unexpected "
            "failures and no unexpected passes, and prints PASS. Every corpus this "
            "gate runs over must declare how many candidates it expects.",
            file=sys.stderr,
        )
        return 1
    if args.min_candidates is not None and args.min_candidates < 1:
        print("error: --min-candidates must be a positive integer", file=sys.stderr)
        return 1

    script_dir = Path(__file__).resolve().parent
    repo_root: Path = args.repo_root or (script_dir.parent.parent)

    if args.hew:
        hew = args.hew
    else:
        resolver = repo_root / "scripts" / "cargo-output-dir.py"
        resolved = subprocess.run(
            [sys.executable, str(resolver), "--profile", "debug"],
            cwd=repo_root,
            capture_output=True,
            text=True,
            check=False,
        )
        if resolved.returncode != 0 or not resolved.stdout.strip():
            print(
                "error: could not resolve Cargo's debug output directory",
                file=sys.stderr,
            )
            return 1
        output_dir = Path(resolved.stdout.strip())
        hew = (
            output_dir if output_dir.is_absolute() else repo_root / output_dir
        ) / "hew"
    if not hew.exists():
        print(f"error: hew binary not found at {hew}", file=sys.stderr)
        print("       build it with: cargo build -p hew-cli", file=sys.stderr)
        return 1

    regressions_dir: Path = args.regressions_dir or (
        repo_root / "tests" / "fuzz-oracle" / "regressions"
    )
    expected_failures_path: Path = args.expected_failures or (
        regressions_dir.parent / "expected-failures.txt"
    )
    if args.expected_rejects is not None:
        expected_rejects_path = args.expected_rejects
    elif args.vertical_slice_dir is not None:
        expected_rejects_path = args.vertical_slice_dir.parent / "expected-reject.txt"
    else:
        expected_rejects_path = script_dir / "expected-reject.txt"

    # Detect HEW_STD: honour env if set, else use repo std/.
    hew_std: Optional[str] = os.environ.get("HEW_STD") or str(repo_root / "std")

    fuzz_corpus, vertical_slice, regressions = collect_candidates(
        repo_root,
        regressions_dir,
        args.full,
        vertical_slice_dir=args.vertical_slice_dir,
    )

    try:
        expected_failures = _load_expected_failures(expected_failures_path)
        expected_reject_paths = _load_expected_rejects(expected_rejects_path)
    except ValueError as error:
        print(f"error: {error}", file=sys.stderr)
        return 1

    # Floor the ratcheted candidate set. Both gate conditions below are searches
    # over the collected candidates: an empty collection yields no unexpected
    # failures and no unexpected passes, and prints PASS. The cargo-fuzz corpus
    # (--full) is deliberately excluded — it is hydrated, unratcheted and its
    # size is not a property of this repo's tracked fixtures.
    ratcheted_candidates = len(vertical_slice) + len(regressions)
    floor_error: Optional[str] = None
    if args.min_candidates is not None:
        if ratcheted_candidates < args.min_candidates:
            floor_error = (
                f"\nCORPUS MINIMUM: collected {ratcheted_candidates} ratcheted "
                f"candidate(s), caller declared at least {args.min_candidates}\n"
                "              vertical-slice/accept + fuzz-oracle/regressions\n"
                "              An oracle run over nothing proves nothing: both gate\n"
                "              conditions are searches over the candidates it "
                "collected."
            )
    else:
        floor_error = check_nonempty("fuzz-oracle-candidates", ratcheted_candidates)

    if floor_error is not None:
        print(
            "\n-- candidate execution skipped: the collected corpus is below "
            "its floor, so running it would prove nothing --"
        )
        print(f"   ({ratcheted_candidates} ratcheted candidate(s) collected)")
    else:
        print(f"corpus selection OK: {ratcheted_candidates} ratcheted candidate(s)")

    stats = OracleStats()
    all_verdicts: list[Verdict] = []
    unexpected_fails: list[Verdict] = []
    unexpected_passes: list[Verdict] = []
    expected_failure_class_drifts: list[tuple[str, str, Verdict]] = []
    unexpected_rejects: list[tuple[str, Verdict]] = []
    unexpected_reject_passes: list[tuple[str, Verdict]] = []
    missing_expected_entries: list[str] = []
    seen_reject_paths: set[str] = set()

    with tempfile.TemporaryDirectory(prefix="hew-oracle-") as tmpdir:
        workdir = Path(tmpdir)

        def _process_group(
            files: list[Path],
            label: str,
            apply_ratchet: bool,
            strict_exit: bool = True,
            reject_ratchet_root: Optional[Path] = None,
        ) -> None:
            if not files or floor_error is not None:
                return
            print(f"\n-- {label} ({len(files)} files) --")
            for src in files:
                verdict = process_candidate(
                    src,
                    hew=hew,
                    timeout_s=args.timeout,
                    output_cap=args.output_cap,
                    hew_std=hew_std,
                    workdir=workdir,
                    strict_exit=strict_exit,
                )
                stats.record(verdict)
                all_verdicts.append(verdict)
                _print_verdict(verdict, verbose=args.verbose)

                if apply_ratchet:
                    name = src.name
                    expected_class = expected_failures.get(name)
                    if expected_class is not None:
                        if not verdict.is_fail:
                            unexpected_passes.append(verdict)
                        elif verdict.classification != expected_class:
                            expected_failure_class_drifts.append(
                                (name, expected_class, verdict)
                            )
                    elif (
                        reject_ratchet_root is not None
                        and verdict.classification == "frontend-reject"
                    ):
                        identity = _fixture_identity(
                            src,
                            reject_ratchet_root,
                            Path("tests/vertical-slice/accept"),
                        )
                        seen_reject_paths.add(identity)
                        if identity not in expected_reject_paths:
                            unexpected_rejects.append((identity, verdict))
                    elif verdict.is_fail:
                        unexpected_fails.append(verdict)

                    if reject_ratchet_root is not None:
                        identity = _fixture_identity(
                            src,
                            reject_ratchet_root,
                            Path("tests/vertical-slice/accept"),
                        )
                        if (
                            identity in expected_reject_paths
                            and verdict.classification != "frontend-reject"
                        ):
                            unexpected_reject_passes.append((identity, verdict))

        # Source 1: cargo-fuzz corpus (full mode only, no ratchet).
        _process_group(fuzz_corpus, "cargo-fuzz corpus", apply_ratchet=False)

        # Source 2: vertical-slice accept fixtures.
        # strict_exit=False: these programs intentionally exit with specific
        # non-zero codes (42, 7, 134, etc.) as their correctness signal; the
        # oracle only fails them on structural failures (signal/PANIC/NYI/ICE/
        # timeout/output-cap).  Exit-code correctness is already pinned by
        # tests/vertical-slice/run.sh — we only add the build+run dimension here.
        _process_group(
            vertical_slice,
            "vertical-slice/accept",
            apply_ratchet=True,
            strict_exit=False,
            reject_ratchet_root=(
                args.vertical_slice_dir
                or repo_root / "tests" / "vertical-slice" / "accept"
            ),
        )

        # Source 3: regression corpus (ratchet enforced, strict exit required).
        _process_group(regressions, "fuzz-oracle/regressions", apply_ratchet=True)

    # Validate expected-failures completeness: entries listed but not found in
    # ANY candidate source (regressions + vertical-slice) → unexpected-pass (the
    # file was removed without clearing the entry — a ratchet violation).
    all_candidate_names = {p.name for p in regressions} | {
        p.name for p in vertical_slice
    }
    for name in sorted(expected_failures):
        if name not in all_candidate_names:
            missing_expected_entries.append(
                f"expected failure absent from corpus: {name}"
            )

    for identity in sorted(expected_reject_paths - seen_reject_paths):
        if not any(path == identity for path, _ in unexpected_reject_passes):
            missing_expected_entries.append(
                f"expected rejection absent from corpus: {identity}"
            )

    # Summary.
    print()
    fail_by_verdict = ", ".join(f"{k}={v}" for k, v in sorted(stats.fails.items()))
    print(
        f"ORACLE done  total={stats.total}  clean={stats.clean}"
        f"  frontend-reject={stats.frontend_reject}"
        f"  FAIL={stats.total_fail}"
        + (f"  (by-verdict: {fail_by_verdict})" if stats.fails else "")
    )

    gate_ok = True

    if floor_error is not None:
        print(floor_error, file=sys.stderr)
        gate_ok = False

    if missing_expected_entries:
        print("\nEXPECTED ENTRIES MISSING FROM CORPUS:")
        for entry in missing_expected_entries:
            print(f"  {entry}")
        gate_ok = False

    if unexpected_fails:
        print("\nUNEXPECTED FAILURES (not in expected-failures.txt):")
        for v in unexpected_fails:
            # `UNEXPECTED:` / `NOW-PASSES:` is the ratchet verdict vocabulary
            # shared with the other expected-failure gates in the tree, so the
            # prefix is part of the human- and machine-readable verdict.
            print(f"  UNEXPECTED: {v.path.name}  {v.classification}  {v.detail}")
        gate_ok = False

    if expected_failure_class_drifts:
        print("\nEXPECTED FAILURE CLASS DRIFT:")
        for name, expected_class, verdict in expected_failure_class_drifts:
            detail = f"  {verdict.detail}" if verdict.detail else ""
            print(
                f"  CLASS-DRIFT: {name}  expected={expected_class} "
                f"observed={verdict.classification}{detail}"
            )
        gate_ok = False

    if unexpected_passes:
        print("\nUNEXPECTED PASSES (listed in expected-failures.txt but passed):")
        for v in unexpected_passes:
            print(f"  NOW-PASSES: {v.path.name}  {v.detail}")
        if args.strict_recoveries:
            gate_ok = False

    if unexpected_rejects:
        print("\nUNEXPECTED REJECTIONS (not in expected-reject.txt):")
        for identity, verdict in unexpected_rejects:
            detail = f"  {verdict.detail}" if verdict.detail else ""
            print(f"  UNEXPECTED-REJECT: {identity}{detail}")
        gate_ok = False

    if unexpected_reject_passes:
        print("\nUNEXPECTED PASSES (listed in expected-reject.txt but passed):")
        for identity, verdict in unexpected_reject_passes:
            detail = f"  {verdict.detail}" if verdict.detail else ""
            print(f"  NOW-PASSES: {identity}{detail}")
        if args.strict_recoveries:
            gate_ok = False

    if gate_ok:
        print("ORACLE gate: PASS")
    else:
        print("ORACLE gate: FAIL")

    # JSON report.
    if args.report:
        report = {
            "stats": {
                "total": stats.total,
                "clean": stats.clean,
                "frontend_reject": stats.frontend_reject,
                "fail": stats.total_fail,
                "by_verdict": stats.fails,
            },
            "verdicts": [
                {
                    "path": str(v.path),
                    "classification": v.classification,
                    "detail": v.detail,
                }
                for v in all_verdicts
            ],
            "unexpected_fails": [
                {
                    "path": str(v.path),
                    "classification": v.classification,
                    "detail": v.detail,
                }
                for v in unexpected_fails
            ],
            "unexpected_passes": [
                {
                    "path": str(v.path),
                    "expected_classification": expected_failures[v.path.name],
                    "detail": v.detail,
                }
                for v in unexpected_passes
            ],
            "expected_failure_class_drifts": [
                {
                    "path": name,
                    "expected_classification": expected_class,
                    "observed_classification": verdict.classification,
                    "detail": verdict.detail,
                }
                for name, expected_class, verdict in expected_failure_class_drifts
            ],
            "unexpected_rejects": [
                {
                    "path": identity,
                    "classification": verdict.classification,
                    "detail": verdict.detail,
                }
                for identity, verdict in unexpected_rejects
            ],
            "unexpected_reject_passes": [
                {"path": identity, "detail": verdict.detail}
                for identity, verdict in unexpected_reject_passes
            ],
            "missing_expected_entries": missing_expected_entries,
        }
        args.report.write_text(json.dumps(report, indent=2))
        print(f"Report written to {args.report}")

    return 0 if gate_ok else 1


if __name__ == "__main__":
    sys.exit(main())
