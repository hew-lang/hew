#!/usr/bin/env python3
"""Extract an actual LLVM CFG and have Lean's kernel check its cleanup certificate."""

from collections import deque
from pathlib import Path
import hashlib
import json
import os
import shlex
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
TARGET = HERE / "target"


class Rejected(Exception):
    """The artifact has no certificate for the declared terminal cleanup rule."""


def invoke(args, *, cwd=ROOT, env=None, timeout=180):
    result = subprocess.run(
        list(map(str, args)),
        cwd=cwd,
        env=env,
        text=True,
        capture_output=True,
        timeout=timeout,
    )
    if result.returncode:
        raise RuntimeError(f"Command failed: {args}\n{result.stdout}\n{result.stderr}")
    return result.stdout


def setup():
    TARGET.mkdir(parents=True, exist_ok=True)
    if invoke(["lean", "--short-version"]).strip() != "4.19.0":
        raise RuntimeError("Lean 4.19.0 is required")
    configured = os.environ.get("LLVM_SYS_221_PREFIX")
    config = (
        str(Path(configured) / "bin/llvm-config")
        if configured
        else shutil.which("llvm-config")
    )
    if not config or not invoke([config, "--version"]).strip().startswith("22.1."):
        raise RuntimeError("LLVM 22.1 toolchain required; set LLVM_SYS_221_PREFIX")
    prefix = Path(invoke([config, "--prefix"]).strip())
    flags = shlex.split(
        invoke(
            [
                config,
                "--cxxflags",
                "--ldflags",
                "--libs",
                "core",
                "irreader",
                "--system-libs",
            ]
        )
    )
    source = HERE / "extract.cpp"
    stamp = hashlib.sha256(
        source.read_bytes() + str(prefix).encode() + repr(flags).encode()
    ).hexdigest()
    if (
        not (TARGET / "extract").exists()
        or not (TARGET / "extract.sha256").exists()
        or (TARGET / "extract.sha256").read_text() != stamp
    ):
        invoke([prefix / "bin/clang++", source, *flags, "-o", TARGET / "extract"])
        (TARGET / "extract.sha256").write_text(stamp)
    invoke(
        [
            "lean",
            "-DwarningAsError=true",
            "-o",
            TARGET / "FrameCleanup.olean",
            "FrameCleanup.lean",
        ],
        cwd=HERE,
    )
    return {**os.environ, "LEAN_PATH": str(TARGET)}


def marks_for(function):
    blocks = function["blocks"]
    marks = [False] * len(blocks)
    todo = deque()
    for block in blocks:
        if 2 in block["events"]:
            todo.extend(block["successors"])
    while todo:
        i = todo.popleft()
        if i < 0 or i >= len(blocks):
            raise Rejected("CFG successor does not exist")
        if marks[i]:
            continue
        marks[i] = True
        todo.extend(blocks[i]["successors"])
    return marks


def certificate(function):
    blocks = function["blocks"]
    marks = marks_for(function)
    # This scan supplies actionable source diagnostics. Acceptance still
    # requires an independently checked Lean certificate of the same graph.
    for i, block in enumerate(blocks):
        after = marks[i]
        for event, instruction in zip(
            block["events"], block["instructions"], strict=True
        ):
            if after and event != 0:
                raise Rejected(
                    f"terminal frame cleanup: {function['name']} / {block['name']}: {instruction.strip()}"
                )
            after = after or event == 2
    return marks


def lean_document(graph, marks, expected=True):
    events = {0: ".quiet", 1: ".effect", 2: ".release"}
    blocks = []
    for block in graph:
        es = ",".join(events[e] for e in block["events"])
        edges = ",".join(map(str, block["successors"]))
        blocks.append(f"⟨[{es}], [{edges}]⟩")
    ms = ",".join(str(m).lower() for m in marks)
    return (
        "import FrameCleanup\nopen FrameCleanup\n"
        "set_option maxRecDepth 100000\nset_option maxHeartbeats 0\n"
        f"example : check [{','.join(blocks)}] [{ms}] = {str(expected).lower()} := by decide\n"
    )


def validate(path, env, *, label=None):
    path = Path(path)
    graph = json.loads(invoke([TARGET / "extract", path]))
    stem = label or path.stem
    folder = TARGET / stem
    folder.mkdir(parents=True, exist_ok=True)
    (folder / "extraction.json").write_text(json.dumps(graph, indent=2))
    if graph["unknown"]:
        raise Rejected("unsupported frame release: " + "; ".join(graph["unknown"]))
    if not graph["functions"] or graph["release_sites"] == 0:
        raise Rejected("empty artifact: no explicit frame releases checked")
    handle_obligations = []
    for index, function in enumerate(graph["functions"]):
        marks = certificate(function)
        for i, block in enumerate(function["blocks"]):
            after = marks[i]
            for ordinal, event in enumerate(block["events"]):
                if after and ordinal in block["pointer_returns"]:
                    handle_obligations.append(
                        {
                            "function": function["name"],
                            "block": block["name"],
                            "instruction": block["instructions"][ordinal].strip(),
                        }
                    )
                after = after or event == 2
        proof = folder / f"function-{index}.lean"
        proof.write_text(lean_document(function["blocks"], marks))
        output = invoke(["lean", "-DwarningAsError=true", proof], env=env)
        (folder / f"function-{index}.log").write_text(output)
    return {
        "artifact_sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
        "functions": len(graph["functions"]),
        "release_sites": graph["release_sites"],
        "allocation_sites": graph["allocation_sites"],
        "status": "kernel_checked",
        "path_bound": None,
        "unproved_handle_returns": handle_obligations,
    }


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("llvm_ir", type=Path)
    args = parser.parse_args()
    print(json.dumps(validate(args.llvm_ir, setup()), indent=2))
