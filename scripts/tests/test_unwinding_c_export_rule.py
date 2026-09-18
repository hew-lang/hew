#!/usr/bin/env python3
"""Counterfactuals for unwinding calls inside plain `extern "C"` exports."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RULE = ROOT / "rules/rust/hygiene/unwinding-call-in-plain-c-export.yml"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"


def findings(source: str) -> list[dict[str, object]]:
    with tempfile.TemporaryDirectory(prefix="hew-unwinding-c-export-") as temp:
        root = Path(temp)
        target = root / "hew-runtime/src/lib.rs"
        target.parent.mkdir(parents=True)
        target.write_text(source, encoding="utf-8")
        result = subprocess.run(
            [
                str(AST_GREP),
                "scan",
                "--rule",
                str(RULE),
                "--json=stream",
                "hew-runtime/src",
            ],
            cwd=root,
            check=False,
            capture_output=True,
            text=True,
        )
        if result.returncode not in (0, 1):
            raise SystemExit(f"unwinding C export rule failed closed:\n{result.stderr}")
        return [json.loads(line) for line in result.stdout.splitlines() if line]


def main() -> None:
    global AST_GREP
    if len(sys.argv) > 1:
        AST_GREP = Path(sys.argv[1]).resolve()

    red = findings(
        "#[no_mangle]\n"
        'pub extern "C" fn hew_send_stub() { hew_panic(); }\n'
        "#[no_mangle]\n"
        'pub unsafe extern "C" fn hew_close_stub(f: *mut HewFault) {\n'
        "    unsafe { crate::fault::hew_fault_trap(1, f) };\n"
        "}\n"
        "#[no_mangle]\n"
        'pub unsafe extern "C" fn hew_release_stub(v: *mut HewVec) {\n'
        "    unsafe { hew_vec_free_owned_walk(v) };\n"
        "}\n"
        "#[no_mangle]\n"
        'pub unsafe extern "C" fn hew_elem_stub(slot: *mut c_void, drop_thunk: HewValueDropThunk) {\n'
        "    unsafe { (drop_thunk)(slot) };\n"
        "}\n"
    )
    if len(red) != 4:
        raise SystemExit(
            f"unwinding C export counterfactual found {len(red)}, want 4: {red}"
        )

    green = findings(
        # The correct declaration for a frame an unwind may cross.
        "#[no_mangle]\n"
        'pub extern "C-unwind" fn hew_send_stub() { hew_panic(); }\n'
        # A plain C export that reaches nothing which can unwind.
        "#[no_mangle]\n"
        'pub unsafe extern "C" fn hew_vec_len(v: *const HewVec) -> i64 {\n'
        "    unsafe { (*v).len }\n"
        "}\n"
        # A non-exported helper is not the C-ABI surface generated code links.
        'extern "C" fn helper() { hew_panic(); }\n'
        # Test doubles are their own ABI island.
        "#[cfg(test)]\n"
        "mod tests {\n"
        "    #[no_mangle]\n"
        '    pub extern "C" fn probe() { hew_panic(); }\n'
        "}\n"
    )
    if green:
        raise SystemExit(f"non-unwinding C export counterfactual was flagged: {green}")

    print("unwinding call in plain C export counterfactuals: PASS")


if __name__ == "__main__":
    main()
