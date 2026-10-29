#!/usr/bin/env python3
"""Refuse drift in the small ABI constants used by the verification host."""

from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]


def check():
    actor = (ROOT / "hew-runtime/src/actor/mod.rs").read_text()
    for name, kind in [
        ("cont_tag", "AtomicI32"),
        ("suspended_cont", "AtomicPtr<c_void>"),
        ("pending_wake", "AtomicBool"),
        ("checked_invocation", "AtomicPtr<c_void>"),
    ]:
        if not re.search(rf"pub {name}: {re.escape(kind)},", actor):
            raise SystemExit(
                f"Executor actor field {name} changed; review the instrumented host"
            )
    vector = (ROOT / "hew-runtime/src/vec.rs").read_text()
    if (
        vector.count("libc::abort()") != 1
        or "unsafe fn abort_vector() -> !" not in vector
    ):
        raise SystemExit(
            "Vec C aborts must use abort_vector so unexpected termination is checked"
        )
    host = (ROOT / "proofs/runtime_protocols/src/environment.rs").read_text()
    source = (ROOT / "hew-runtime/src/coro_state.rs").read_text()
    traps = (ROOT / "hew-runtime/src/internal/types.rs").read_text()
    trap = r"const HEW_TRAP_INDEX_OUT_OF_BOUNDS: i32 = (-?\d+);"
    if re.search(trap, traps).group(1) != re.search(trap, host).group(1):
        raise SystemExit(
            "Vec bounds trap ABI changed; update its verification boundary"
        )

    def status(text):
        body = re.search(r"pub enum CoroStatus\s*\{([^}]+)\}", text).group(1)
        return dict(re.findall(r"(\w+)\s*=\s*(-?\d+)", body))

    if status(source) != status(host):
        raise SystemExit(
            "CoroStatus changed; update the runtime protocol host before verifying"
        )
    for filename, enum, expected in [
        (
            "cancel_state.rs",
            "HewCancellationState",
            {
                "Active": "0",
                "CancelRequested": "1",
                "Observed": "2",
                "Trapped": "3",
                "Completed": "4",
            },
        ),
        (
            "await_state.rs",
            "AwaitCancelStatus",
            {"Pending": "0", "Completed": "1", "Cancelled": "2", "TimedOut": "3"},
        ),
    ]:
        text = (ROOT / "hew-runtime/src" / filename).read_text()
        body = re.search(rf"enum {enum}\s*\{{([^}}]+)\}}", text).group(1)
        if dict(re.findall(r"(\w+)\s*=\s*(-?\d+)", body)) != expected:
            raise SystemExit(
                f"{enum} input domain changed; review its symbolic proof assumptions"
            )
    read_slot = (ROOT / "hew-runtime/src/read_slot.rs").read_text()
    body = re.search(r"enum ReadStatus\s*\{([^}]+)\}", read_slot).group(1)
    if dict(re.findall(r"(\w+)\s*=\s*(-?\d+)", body)) != {
        "Pending": "0",
        "Data": "1",
        "Eof": "2",
        "Error": "3",
        "Cancelled": "4",
        "TimedOut": "5",
    }:
        raise SystemExit(
            "ReadStatus ABI changed; review the read-slot outcome assertions"
        )
    faults = (ROOT / "hew-runtime/src/fault.rs").read_text()
    for name in ["HEW_FAULT_DEADLINE", "HEW_FAULT_RACE_LOST"]:
        pattern = rf"const {name}: i32 = (-?\d+);"
        if re.search(pattern, faults).group(1) != re.search(pattern, host).group(1):
            raise SystemExit(
                f"{name} changed; update the runtime protocol host before verifying"
            )


if __name__ == "__main__":
    check()
    print("Runtime protocol host ABI constants match production")
