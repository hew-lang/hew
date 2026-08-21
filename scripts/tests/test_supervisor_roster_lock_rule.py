#!/usr/bin/env python3
"""RED/GREEN counterfactuals for typed supervisor-roster authority."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RULE = (
    ROOT / "rules/rust/concurrency-drop/no-unsynchronized-supervisor-roster-access.yml"
)
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"


def findings(source: str) -> list[dict[str, object]]:
    with tempfile.TemporaryDirectory(prefix="hew-supervisor-roster-rule-") as temp:
        fixture = Path(temp) / "supervisor.rs"
        fixture.write_text(source, encoding="utf-8")
        result = subprocess.run(
            [
                str(AST_GREP),
                "scan",
                "--rule",
                str(RULE),
                "--json=stream",
                str(fixture),
            ],
            cwd=temp,
            check=False,
            capture_output=True,
            text=True,
        )
        if result.returncode not in (0, 1):
            raise SystemExit(result.stderr or result.stdout)
        return [json.loads(line) for line in result.stdout.splitlines() if line.strip()]


RED = {
    "access_before_lock": """
struct Roster { children: Vec<usize> }
fn bad(roster: &Roster, lock: &Lock) -> usize {
    let value = roster.children[0];
    let roster = lock.lock_or_recover();
    value
}
""",
    "access_after_drop": """
struct Roster { children: Vec<usize> }
fn bad(lock: &Lock) -> usize {
    let roster = lock.lock_or_recover();
    drop(roster);
    roster.children[0]
}
""",
    "mutex_get_mut_bypass": """
struct Supervisor { roster: Lock }
fn bad(s: &mut Supervisor) { let _ = s.roster.get_mut(); }
""",
    "mutex_into_inner_bypass": """
struct Supervisor { roster: Lock }
fn bad(s: Supervisor) { let _ = s.roster.into_inner(); }
""",
    "unmarked_raw_roster_helper": """
struct SupervisorRoster { children: Vec<usize> }
fn bad(roster: &SupervisorRoster) -> usize { roster.children[0] }
""",
    "callback_while_guarded": """
fn bad(lock: &Lock, init_fn: fn()) {
    let roster = lock.lock_or_recover();
    init_fn();
    drop(roster);
}
""",
    "wait_while_guarded": """
fn bad(lock: &Lock, cond: &Condvar) {
    let roster = lock.lock_or_recover();
    cond.wait();
    drop(roster);
}
""",
}

GREEN = """
struct SupervisorRoster { children: Vec<usize>, child_count: usize }
fn direct(lock: &Lock) -> usize {
    let roster = lock.lock_or_recover();
    roster.children[0] + roster.child_count
}
fn scoped(lock: &Lock) -> usize {
    let guard = lock.lock_or_recover();
    let roster = &*guard;
    roster.children[0] + roster.child_count
}
fn helper(roster: &SupervisorRoster) -> usize {
    // ROSTER-GUARDED-HELPER: caller's MutexGuard bounds this borrow.
    roster.children[0]
}
"""


def main() -> None:
    global AST_GREP
    if len(sys.argv) > 1:
        AST_GREP = Path(sys.argv[1]).resolve()

    for name, source in RED.items():
        if not findings(source):
            raise SystemExit(f"RED counterfactual did not fire: {name}")

    if unexpected := findings(GREEN):
        raise SystemExit(f"GREEN typed-guard fixtures unexpectedly fired: {unexpected}")

    print("no-unsynchronized-supervisor-roster-access counterfactuals: PASS")


if __name__ == "__main__":
    main()
