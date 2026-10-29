#!/usr/bin/env python3
"""Show rule-level confidence and record proof results for the exact source tree."""

import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import subprocess
import sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
TARGET = HERE / "target"


def fingerprint(suite):
    paths = {
        ROOT / "Cargo.toml",
        ROOT / "Cargo.lock",
        ROOT / "rust-toolchain.toml",
        ROOT / "Makefile",
        HERE / "coverage.py",
        HERE / "coverage.json",
        HERE / "check_evidence.py",
        ROOT / ".cargo/config.toml",
        ROOT / "hew-runtime/build.rs",
    }
    paths.update(ROOT.glob("hew-*/Cargo.toml"))
    folders = [ROOT / "hew-runtime/src", ROOT / "hew-cabi/src"]
    if suite == "runtime":
        folders += [
            HERE / "ready_gate",
            HERE / "request_deadline",
            HERE / "runtime_protocols",
        ]
    else:
        folders += (
            list(ROOT.glob("hew-*/src"))
            + list(ROOT.glob("hew-*/build_support"))
            + [ROOT / "std", ROOT / "scripts", HERE / "llvm_frames"]
        )
        paths.update(ROOT.glob("hew-*/build.rs"))
        paths.add(HERE / "llvm_frames/corpus.json")
        paths.update(
            ROOT / c["source"]
            for c in json.loads((HERE / "llvm_frames/corpus.json").read_text())
        )
        paths.update(ROOT.glob("hew-*/Cargo.toml"))
        paths.add(ROOT / ".github/actions/setup-llvm/action.yml")
    for folder in folders:
        for path in folder.rglob("*"):
            if not path.is_file() or any(
                p in ("target", "__pycache__") for p in path.relative_to(folder).parts
            ):
                continue
            if path.suffix in (
                ".rs",
                ".cpp",
                ".h",
                ".lean",
                ".py",
                ".hew",
                ".toml",
            ) or path.name in ("Cargo.toml", "Cargo.lock", "lean-toolchain"):
                paths.add(path)
    digest = hashlib.sha256()
    for path in sorted(paths):
        digest.update(
            str(path.relative_to(ROOT)).encode() + b"\0" + path.read_bytes() + b"\0"
        )
    return digest.hexdigest()


def evidence(suite):
    path = TARGET / f"{suite}.json"
    if not path.exists():
        return {"status": "not_run"}
    record = json.loads(path.read_text())
    if record["fingerprint"] != fingerprint(suite):
        return {**record, "status": "stale"}
    return record


def run(suite):
    TARGET.mkdir(parents=True, exist_ok=True)
    before = fingerprint(suite)
    record = {
        "suite": suite,
        "fingerprint": before,
        "status": "running",
        "started_utc": datetime.now(timezone.utc).isoformat(),
    }
    output = TARGET / f"{suite}.json"
    output.write_text(json.dumps(record, indent=2) + "\n")
    command = (
        ["make", "prove-runtime-checks"]
        if suite == "runtime"
        else [sys.executable, "proofs/llvm_frames/run.py"]
    )
    try:
        with (TARGET / f"{suite}.log").open("w") as log:
            process = subprocess.Popen(
                command,
                cwd=ROOT,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
            )
            for line in process.stdout:
                print(line, end="", flush=True)
                log.write(line)
                log.flush()
            code = process.wait()
        record["status"] = "passed" if code == 0 else "failed"
        if before != fingerprint(suite):
            record["status"] = "stale"
            code = 1
        if suite == "llvm" and code == 0:
            details = json.loads((HERE / "llvm_frames/target/report.json").read_text())
            count = 2 * len(json.loads((HERE / "llvm_frames/corpus.json").read_text()))
            if len(details["artifacts"]) != count or details["negative_controls"] != 7:
                raise RuntimeError("LLVM coverage report is incomplete")
            record["coverage"] = details
    except BaseException as error:
        record["status"] = "failed"
        record["error"] = str(error)
        raise
    finally:
        record["finished_utc"] = datetime.now(timezone.utc).isoformat()
        output.write_text(json.dumps(record, indent=2) + "\n")
    if code:
        raise SystemExit(code)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--run", choices=["runtime", "llvm"])
    parser.add_argument("--json", action="store_true")
    parser.add_argument(
        "--rules",
        action="store_true",
        help="expand individual rules beneath the subsystem overview",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="require fresh passing evidence for both suites",
    )
    args = parser.parse_args()
    if args.run:
        run(args.run)
        return
    suites = {name: evidence(name) for name in ("runtime", "llvm")}
    groups = json.loads((HERE / "coverage.json").read_text())
    for group in groups:
        group["evidence"] = (
            suites[group["suite"]]["status"] if group["suite"] else "uncovered"
        )
    report = {
        "subsystems": groups,
        "suites": suites,
        "assessment": "Strong local rules and bounded protocol evidence; no whole subsystem is established as formally correct.",
    }
    if args.json:
        print(json.dumps(report, indent=2))
    else:
        print(report["assessment"] + "\n")
        for group in groups:
            print(f"{group['subsystem']} [{group['evidence']}]: {group['summary']}")
            print(f"  Confidence: {group['confidence']}. Gap: {group['missing']}")
            if args.rules:
                for rule in group["rules"]:
                    print(f"  • {rule}")
        llvm = suites["llvm"].get("coverage")
        if llvm:
            print(
                f"\nLast LLVM corpus: {len(llvm['artifacts'])} artifacts, {llvm['release_sites']} release sites; "
                f"{llvm['unproved_handle_returns']} syntactic post-release pointer-return obligations remain unproved."
            )
    if args.check and any(v["status"] != "passed" for v in suites.values()):
        raise SystemExit(1)


if __name__ == "__main__":
    main()
