#!/usr/bin/env python3
"""Require concrete counterexamples for broken production deadline rules."""

from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "hew-runtime/src/request_deadline.rs"
HARNESS = Path(__file__).with_name("check.rs")

MUTATIONS = [
    (
        "extending a merged deadline",
        "self.0.min(other.0)",
        "self.0.max(other.0)",
        "merging_never_extends_deadlines",
    ),
    (
        "discarding an inherited deadline",
        "(false, _) => other,",
        "(false, _) => Self(-1),",
        "merging_never_extends_deadlines",
    ),
    (
        "overflowing into unlimited time",
        "saturating_add",
        "wrapping_add",
        "durations_cannot_overflow_into_unlimited_time",
    ),
    (
        "not clamping a negative duration",
        "duration_ns.max(0)",
        "duration_ns",
        "durations_cannot_overflow_into_unlimited_time",
    ),
    (
        "missing the exact expiry instant",
        "remaining <= 0",
        "remaining < 0",
        "expiry_is_exact_and_cannot_reverse_as_time_advances",
    ),
]

with tempfile.TemporaryDirectory(prefix="hew-deadline-mutants-") as directory:
    folder = Path(directory)
    (folder / "check.rs").write_text(
        HARNESS.read_text().replace(
            "../../hew-runtime/src/request_deadline.rs", "request_deadline.rs"
        )
    )
    source = SOURCE.read_text()
    for name, before, after, harness in MUTATIONS:
        if source.count(before) != 1:
            raise SystemExit(f"mutation site changed; review witness: {before!r}")
        (folder / "request_deadline.rs").write_text(source.replace(before, after, 1))
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
        if (
            result.returncode != 1
            or 'Failed Checks: "deadline:' not in result.stdout
            or "kani::concrete_playback_run" not in result.stdout
            or "VERIFICATION:- FAILED" not in result.stdout
        ):
            print(result.stdout)
            print(result.stderr)
            raise SystemExit(f"expected a deadline-contract counterexample: {name}")
        print(f"Kani rejected {name} with a concrete counterexample")
