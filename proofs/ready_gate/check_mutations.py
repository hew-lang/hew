#!/usr/bin/env python3
"""Demand counterexamples for realistic scheduling regressions."""

from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "hew-runtime/src/ready_state.rs"
PROOF = Path(__file__).with_name("ReadyGate.lean")
HARNESS = Path(__file__).with_name("check.rs")


def replace_once(source, before, after):
    if source.count(before) != 1:
        raise SystemExit(f"mutation site changed; review witness: {before!r}")
    return source.replace(before, after, 1)


def expect_counterexample(folder, harness, name):
    result = subprocess.run(
        [
            "kani",
            str(folder / "check.rs"),
            "--harness",
            harness,
            "--output-format",
            "terse",
            "-Z",
            "concrete-playback",
            "--concrete-playback",
            "print",
        ],
        text=True,
        capture_output=True,
    )
    # Demand a named contract assertion and a concrete input, not just any
    # nonzero exit (which could be a compiler, solver, or unwinding failure).
    if (
        result.returncode != 1
        or 'Failed Checks: "ready gate:' not in result.stdout
        or "kani::concrete_playback_run" not in result.stdout
        or "VERIFICATION:- FAILED" not in result.stdout
    ):
        print(result.stdout)
        print(result.stderr)
        raise SystemExit(f"expected a scheduling-contract counterexample: {name}")
    print(f"Kani rejected {name} with a concrete counterexample")


runtime = SOURCE.read_text()
mutations = {
    "lost notification": replace_once(
        runtime, "RUNNING => NOTIFIED,", "RUNNING => RUNNING,"
    ),
    "duplicate poll admission": replace_once(
        runtime, "compare_exchange(QUEUED, RUNNING,", "compare_exchange(QUEUED, QUEUED,"
    ),
    "poll while frozen": replace_once(
        runtime,
        "compare_exchange(QUEUED, RUNNING,",
        "compare_exchange(if self.0.load(Ordering::Acquire) == 9 { 9 } else { QUEUED }, RUNNING,",
    ),
    "completed work restarts": replace_once(
        runtime,
        "QUEUED | NOTIFIED | COMPLETE => return false,",
        "COMPLETE => QUEUED, QUEUED | NOTIFIED => return false,",
    ),
}

with tempfile.TemporaryDirectory(prefix="hew-ready-gate-mutants-") as directory:
    folder = Path(directory)
    (folder / "expected.rs").write_text(PROOF.with_name("expected.rs").read_text())
    (folder / "check.rs").write_text(
        HARNESS.read_text().replace(
            "../../hew-runtime/src/ready_state.rs", "ready_state.rs"
        )
    )
    for name, source in mutations.items():
        (folder / "ready_state.rs").write_text(source)
        expect_counterexample(
            folder, "production_refines_lean_rules_for_all_valid_states", name
        )

    # A verifier that silently drops all states must fail, rather than prove
    # the runtime correct under an impossible input assumption.
    (folder / "ready_state.rs").write_text(runtime)
    (folder / "expected.rs").write_text(
        "pub(super) const TABLE: [[u8; 6]; 16] = [[0; 6]; 16];\n"
        "pub(super) const QUIESCENT: [bool; 16] = [false; 16];\n"
    )
    expect_counterexample(
        folder,
        "production_refines_lean_rules_for_all_valid_states",
        "an oracle that silently drops every state",
    )

    proof = PROOF.read_text()
    for name, before, after in [
        (
            "discarding readiness during a poll",
            ".running => .notified",
            ".running => .idle",
        ),
        (
            "admitting frozen work",
            "s.phase == .queued && !s.frozen",
            "s.phase == .queued",
        ),
    ]:
        candidate = folder / "Broken.lean"
        candidate.write_text(replace_once(proof, before, after))
        result = subprocess.run(
            ["lean", "-DwarningAsError=true", str(candidate)],
            text=True,
            capture_output=True,
            cwd=PROOF.resolve().parent,
        )
        if result.returncode != 1 or not any(
            text in result.stdout for text in ("tactic 'decide'", "unsolved goals")
        ):
            print(result.stdout)
            print(result.stderr)
            raise SystemExit(f"expected a failed proof: {name}")
        print(f"Lean rejected {name}")
