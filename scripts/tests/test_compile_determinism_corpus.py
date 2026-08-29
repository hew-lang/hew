#!/usr/bin/env python3
"""Self-test for the repeated-compile determinism gate.

The gate's whole value is that it goes red on output that varies between two
identical compiles.  A harness that only ever sees a deterministic compiler
cannot demonstrate that, so each assertion here is paired with a stub compiler
that breaks exactly one of them: EdgeCarry ordering, stderr, exit status.  The
deterministic stub is the control — the same corpus, the same runs, green.
"""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "compile-determinism-corpus.sh"
FIXTURE_COUNT = 16

STUB = r"""#!/usr/bin/env bash
# Stub compiler: accepts `compile --dump-mir raw <fixture>` and varies its
# output across repeated invocations according to $STUB_MODE.
set -u
fixture="${!#}"
name="$(basename "$fixture")"
counter="$STUB_STATE/$name.count"
count=0
if [[ -f "$counter" ]]; then count="$(cat "$counter")"; fi
count=$((count + 1))
printf '%s' "$count" >"$counter"

case "$STUB_MODE" in
    deterministic)
        printf 'ownership EdgeCarry a\nownership EdgeCarry b\n'
        printf 'note: compiled %s\n' "$name" >&2
        ;;
    reorder)
        if (( count == 1 )); then
            printf 'ownership EdgeCarry a\nownership EdgeCarry b\n'
        else
            printf 'ownership EdgeCarry b\nownership EdgeCarry a\n'
        fi
        printf 'note: compiled %s\n' "$name" >&2
        ;;
    diagnostics)
        printf 'ownership EdgeCarry a\nownership EdgeCarry b\n'
        printf 'note: compiled %s (attempt %s)\n' "$name" "$count" >&2
        ;;
    status)
        printf 'ownership EdgeCarry a\n'
        printf 'note: compiled %s\n' "$name" >&2
        if (( count > 1 )); then exit 1; fi
        ;;
    crash)
        printf 'note: compiler aborted\n' >&2
        exit 101
        ;;
    *)
        echo "stub: unknown STUB_MODE '$STUB_MODE'" >&2
        exit 2
        ;;
esac
exit 0
"""


class DeterminismGateTests(unittest.TestCase):
    def run_gate(
        self, mode: str, *, fixtures: int = FIXTURE_COUNT, **env_overrides: str
    ) -> subprocess.CompletedProcess[str]:
        with tempfile.TemporaryDirectory() as raw:
            workspace = Path(raw)
            corpus = workspace / "corpus"
            corpus.mkdir()
            for index in range(fixtures):
                (corpus / f"fixture_{index:02d}.hew").write_text(
                    "fn main() -> i64 { 0 }\n", encoding="utf-8"
                )
            state = workspace / "state"
            state.mkdir()
            stub = workspace / "hew"
            stub.write_text(STUB, encoding="utf-8")
            stub.chmod(0o755)

            env = dict(os.environ)
            env.update(
                {
                    "HEW_BIN": str(stub),
                    "STUB_MODE": mode,
                    "STUB_STATE": str(state),
                }
            )
            env.update(env_overrides)
            return subprocess.run(
                ["bash", str(SCRIPT), str(corpus)],
                cwd=ROOT,
                env=env,
                capture_output=True,
                text=True,
                timeout=300,
                check=False,
            )

    def test_a_deterministic_compiler_passes_the_gate(self) -> None:
        result = self.run_gate("deterministic")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("compile-determinism: OK", result.stdout)

    def test_reordered_edge_carry_facts_fail_the_gate(self) -> None:
        result = self.run_gate("reorder")
        self.assertEqual(result.returncode, 1)
        self.assertIn("reordered EdgeCarry facts", result.stderr)

    def test_a_changed_diagnostic_stream_fails_the_gate(self) -> None:
        result = self.run_gate("diagnostics")
        self.assertEqual(result.returncode, 1)
        self.assertIn("changed diagnostic emission", result.stderr)

    def test_a_changed_exit_status_fails_the_gate(self) -> None:
        result = self.run_gate("status")
        self.assertEqual(result.returncode, 1)
        self.assertIn("changed exit status", result.stderr)

    def test_an_abnormal_compiler_exit_fails_the_gate(self) -> None:
        result = self.run_gate("crash")
        self.assertEqual(result.returncode, 1)
        self.assertIn("exited abnormally with status 101", result.stderr)

    def test_too_few_verified_outcomes_fail_the_gate(self) -> None:
        result = self.run_gate(
            "deterministic",
            COMPILE_DETERMINISM_MIN_VERIFIED=str(FIXTURE_COUNT + 1),
        )
        self.assertEqual(result.returncode, 1)
        self.assertIn("verified compiler outcomes", result.stderr)

    def test_the_committed_floor_may_not_be_lowered(self) -> None:
        result = self.run_gate("deterministic", COMPILE_DETERMINISM_MIN_VERIFIED="0")
        self.assertEqual(result.returncode, 2)
        self.assertIn("may not lower the committed floor", result.stderr)

    def test_an_empty_selection_is_refused_rather_than_reported_green(self) -> None:
        result = self.run_gate("deterministic", fixtures=0)
        self.assertEqual(result.returncode, 1)
        self.assertIn("selected nothing", result.stderr)


if __name__ == "__main__":
    unittest.main()
