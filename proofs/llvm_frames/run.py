#!/usr/bin/env python3
"""Kernel-check terminal frame cleanup in current O0/O2 compiler output."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
from check import (
    HERE,
    ROOT,
    TARGET,
    Rejected,
    invoke,
    lean_document,
    marks_for,
    setup,
    validate,
)


def mutation_witnesses(original, env):
    source = original.read_text()
    # Mutate the real compiler output, then parse and verify the modified LLVM.
    # Never optimize it again: doing so could remove the defect being checked.
    match = re.search(
        r"(?m)^([ \t]*)call void @hew_cont_frame_free\(ptr (%[^)]+)\).*$", source
    )
    if not match:
        raise RuntimeError("No canonical frame-free witness; review the corpus shape")
    call, operand = match.group(0), match.group(2)
    after = match.end()
    variants = {
        "duplicate-release": source[:after] + "\n" + call + source[after:],
        "write-after-release": source[:after]
        + f"\n  store i8 0, ptr {operand}, align 1"
        + source[after:],
        "call-after-release": source[:after]
        + f"\n  call void @proof_touch(ptr {operand})"
        + source[after:]
        + "\ndeclare void @proof_touch(ptr)\n",
    }
    # Force a second release in a successor basic block. This is a separate
    # check of CFG extraction; an instruction-local scan alone must fail it.
    function_end = source.index("\n}", after)
    tail = source[after:function_end]
    target = re.search(r"br label %([-a-zA-Z0-9_.$]+)", tail)
    if not target:
        raise RuntimeError("No cleanup successor for the CFG mutation")
    label = re.search(
        r"(?m)^" + re.escape(target.group(1)) + r":[^\n]*\n", source[after:function_end]
    )
    if not label:
        raise RuntimeError("Missing cleanup block in CFG witness")
    insert = after + label.end()
    variants["successor-release"] = source[:insert] + call + "\n" + source[insert:]
    for name, text in variants.items():
        path = TARGET / f"mutant-{name}.ll"
        path.write_text(text)
        graph = json.loads(invoke([TARGET / "extract", path]))
        if graph["unknown"]:
            raise RuntimeError(f"Mutation became unsupported LLVM: {name}")
        rejected = []
        from check import certificate

        for function in graph["functions"]:
            try:
                certificate(function)
            except Rejected as error:
                rejected.append((function, str(error)))
        if not rejected:
            raise RuntimeError(f"Mutation was accepted: {name}")
        function, diagnostic = rejected[0]
        proof = TARGET / f"reject-{name}.lean"
        # A successful Lean proof that check=false is the rejection witness.
        # Parser failures, compiler errors and timeouts do not count.
        proof.write_text(
            lean_document(function["blocks"], marks_for(function), expected=False)
        )
        invoke(["lean", "-DwarningAsError=true", proof], env=env)
        (TARGET / f"reject-{name}.log").write_text(
            diagnostic + "\nLean kernel checked rejection.\n"
        )
        print(f"Rejected LLVM {name}: {diagnostic}", flush=True)
    escaped = TARGET / "mutant-release-address-escape.ll"
    escaped.write_text(
        source + "\n@proof_release_address = global ptr @hew_cont_frame_free\n"
    )
    graph = json.loads(invoke([TARGET / "extract", escaped]))
    if not graph["unknown"]:
        raise RuntimeError("Escaped release symbol was silently accepted")
    print("Rejected escaped release symbol as unsupported", flush=True)
    # An untrusted certificate producer must not be able to omit post-release
    # marks and still convince Lean of success.
    graph = json.loads(invoke([TARGET / "extract", original]))
    function = next(f for f in graph["functions"] if any(marks_for(f)))
    proof = TARGET / "reject-forged-marks.lean"
    proof.write_text(
        lean_document(
            function["blocks"], [False] * len(function["blocks"]), expected=False
        )
    )
    invoke(["lean", "-DwarningAsError=true", proof], env=env)
    print("Lean rejected forged post-release marks", flush=True)
    empty = TARGET / "empty.ll"
    empty.write_text("define void @empty() { ret void }\n")
    try:
        validate(empty, env, label="empty-witness")
    except Rejected as error:
        if "empty artifact" not in str(error):
            raise
    else:
        raise RuntimeError("Empty corpus was accepted")
    print("Rejected empty LLVM coverage", flush=True)
    return len(variants) + 3


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case", action="append")
    args = parser.parse_args()
    env = setup()
    # Build the selected checkout: a stale externally supplied compiler must
    # never be recorded as evidence about the current compiler source.
    print("Building current compiler for LLVM certificates", flush=True)
    build_log = invoke(["make", "hew-native"], timeout=1200)
    (TARGET / "compiler-build.log").write_text(build_log)
    compiler = ROOT / "target/debug/hew"
    cases = json.loads((HERE / "corpus.json").read_text())
    if args.case:
        ids = {case["id"] for case in cases}
        if set(args.case) - ids:
            raise RuntimeError("Unknown corpus case")
        cases = [case for case in cases if case["id"] in args.case]
    records = []
    first = None
    for case in cases:
        for opt in (0, 2):
            label = f"{case['id']}-O{opt}"
            obj = TARGET / f"{label}.o"
            log = invoke(
                [
                    compiler,
                    "build",
                    ROOT / case["source"],
                    "--emit-obj",
                    "--emit-llvm",
                    "--opt-level",
                    str(opt),
                    "-o",
                    obj,
                ],
                timeout=240,
            )
            (TARGET / f"{label}-compile.log").write_text(log)
            path = obj.with_suffix(".ll")
            if not path.exists():
                raise RuntimeError(f"Compiler did not emit IR for {label}")
            record = validate(path, env, label=label)
            if record["allocation_sites"] == 0:
                raise RuntimeError(f"No frame allocation exercised: {label}")
            record.update(case=case["id"], opt_level=opt, source=case["source"])
            records.append(record)
            first = first or path
            print(
                f"Kernel checked {label}: {record['functions']} functions, {record['release_sites']} releases, "
                f"{len(record['unproved_handle_returns'])} unproved handle-return paths",
                flush=True,
            )
    witnesses = mutation_witnesses(first, env)
    with compiler.open("rb") as executable:
        compiler_hash = hashlib.file_digest(executable, "sha256").hexdigest()
    report = {
        "compiler_sha256": compiler_hash,
        "compiler_version": invoke([compiler, "--version"]).strip(),
        "rule": "llvm-terminal-frame-cleanup",
        "status": "passed",
        "artifacts": records,
        "negative_controls": witnesses,
        "functions": sum(r["functions"] for r in records),
        "release_sites": sum(r["release_sites"] for r in records),
        "unproved_handle_returns": sum(
            len(r["unproved_handle_returns"]) for r in records
        ),
    }
    (TARGET / ("report-selection.json" if args.case else "report.json")).write_text(
        json.dumps(report, indent=2) + "\n"
    )
    print(
        f"Verified LLVM cleanup: {len(records)} artifacts, {report['release_sites']} release sites, "
        f"{witnesses} negative controls. Handle lifetime remains separate.",
        flush=True,
    )


if __name__ == "__main__":
    main()
