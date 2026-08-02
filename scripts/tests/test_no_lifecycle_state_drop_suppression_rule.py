#!/usr/bin/env python3
"""RED counterfactuals for the typed state-drop authority ast-grep rule."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RULE = ROOT / "rules/rust/concurrency-drop/no-lifecycle-state-drop-suppression.yml"
AST_GREP = (
    Path(sys.argv[1]).resolve()
    if len(sys.argv) > 1
    else ROOT / ".ast-grep/tool/bin/ast-grep"
)


def findings(source: str) -> list[dict[str, object]]:
    with tempfile.TemporaryDirectory(prefix="hew-state-drop-rule-") as temp:
        fixture = Path(temp) / "outside_previous_module_allowlist.rs"
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
            # Run outside the repository so the Rust-only counterfactual does
            # not require loading Hew's separately-built custom grammar.
            cwd=temp,
            check=False,
            capture_output=True,
            text=True,
        )
        if result.returncode not in (0, 1):
            raise SystemExit(result.stderr or result.stdout)
        return [json.loads(line) for line in result.stdout.splitlines() if line.strip()]


RED = {
    "field_identifier_and_expression": """
struct CleanupOptions { suppress_state_drop: bool }
fn inspect(opts: CleanupOptions) -> bool { opts.suppress_state_drop }
""",
    "alternate_finalize_helper": """
fn finalize_quiescent_actor_cleanup_with_options(actor: *mut u8) { let _ = actor; }
""",
    "renamed_extra_argument": """
fn free_actor_resources(actor: *mut u8, release_fields: bool) { let _ = (actor, release_fields); }
fn caller(actor: *mut u8) { free_actor_resources(actor, false); }
""",
    "wasm_renamed_extra_argument": """
fn caller(actor: *mut u8) { free_actor_resources_wasm(actor, true); }
""",
}

for name, source in RED.items():
    if not findings(source):
        raise SystemExit(f"RED counterfactual did not fire: {name}")

GREEN = """
fn free_actor_resources(actor: *mut u8) { let _ = actor; }
fn finalize_quiescent_actor_cleanup(actor: *mut u8, state: i32) { let _ = (actor, state); }
"""
if unexpected := findings(GREEN):
    raise SystemExit(f"GREEN common-authority fixture unexpectedly fired: {unexpected}")

print("no-lifecycle-state-drop-suppression counterfactuals: PASS")
