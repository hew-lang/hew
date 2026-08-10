#!/usr/bin/env python3
"""Reject codegen consumers that reconstruct MIR-carried identity from names."""

from __future__ import annotations

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
PATTERN = re.compile(
    r'contains\("__recv__"\)|split_once\("__recv__"\)|'
    r'strip_suffix\("__step"\)|starts_with\("hew_metric_"\)|'
    r"hew_tcp_connect|hew_dns_|actor_name_from_handler_symbol|"
    r"actor_layout_key_from_handler_symbol|is_machine_step_symbol|"
    r"module_uses_blocking_offload"
)


def main() -> None:
    findings: list[str] = []
    for path in sorted((ROOT / "hew-codegen-rs/src").rglob("*.rs")):
        for line_number, line in enumerate(path.read_text().splitlines(), 1):
            if PATTERN.search(line):
                findings.append(f"{path.relative_to(ROOT)}:{line_number}:{line}")
    if findings:
        raise SystemExit(
            "codegen reintroduced a string consumer for MIR-carried identity:\n"
            + "\n".join(findings)
        )
    print("codegen carried-identity gate: OK")


if __name__ == "__main__":
    main()
