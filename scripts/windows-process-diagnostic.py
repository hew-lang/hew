#!/usr/bin/env python3
"""Run only the selected process source oracles with an unchanged Windows SDK."""

import hashlib
import json
import os
from pathlib import Path
import subprocess
import time
import tomllib

SOURCE_REVISION = "c64e9659a53eaf8b00d5ca276f9a839c05f3778e"
SOURCE_HASHES = {
    "tests/core-acceptance/cases/process-bounded-output.hew": "c68ede5d69a87b134eac3d85ebc50cb35adbb11196a00c761f2fb9fcf809c1b6",
    "tests/core-acceptance/cases/process-run-suspends.hew": "92927b35b7eab795e90cb44f965ca392ee00889be78178cb19a283b3fa0341d0",
    "tests/core-acceptance/cases/process-supervised-reap.hew": "a1c720b77a5548f3d6d0cbf2e85a174b2ecea9a28dd31a983f2a818f6ca64af6",
    "tests/core-acceptance/fixtures/process-probe.hew": "7d67d8fc8ae456ae8bbf7b456d36c952852f39ae76abeaef7d1166a17fa85b22",
    "tests/core-acceptance/cases/process-bounded-output.toml": "088d50ebe88a8a4b770466bc044e42eae8a80ad01e8363ac7536bb259d7893bd",
    "tests/core-acceptance/cases/process-run-suspends.toml": "989cb07e1d429b7bfef16008663bf424473378035da1c569b52331f085f8fd77",
    "tests/core-acceptance/cases/process-supervised-reap.toml": "5e35e69b1f5ecd34cfc7a5687fbaf11c99803486907742bfe913c00b8852c008",
}
CASES = ("process-bounded-output", "process-run-suspends", "process-supervised-reap")


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def execute(command, prefix, timeout, environment, root):
    # Match core-acceptance: files prevent inherited pipes from extending a timeout.
    started = time.monotonic()
    with (
        prefix.with_suffix(".stdout").open("wb") as stdout,
        prefix.with_suffix(".stderr").open("wb") as stderr,
    ):
        with subprocess.Popen(
            command,
            cwd=root,
            env=environment,
            stdin=subprocess.DEVNULL,
            stdout=stdout,
            stderr=stderr,
        ) as child:
            try:
                status = child.wait(timeout=timeout)
                outcome = "completed"
            except subprocess.TimeoutExpired:
                child.kill()
                child.wait()
                status, outcome = None, "timeout"
    return {"outcome": outcome, "exit": status, "seconds": time.monotonic() - started}


def main():
    root = Path(__file__).resolve().parent.parent
    evidence = Path(os.environ["HEW_PROCESS_DIAGNOSTIC_EVIDENCE"]) / "process"
    evidence.mkdir()
    compiler = Path(os.environ["HEW_PROCESS_DIAGNOSTIC_COMPILER"])
    sdk = compiler.parent.parent
    binaries = Path(os.environ["RUNNER_TEMP"]) / "process-fixtures"
    for name, expected in SOURCE_HASHES.items():
        if digest(root / name) != expected:
            raise RuntimeError(
                f"Source/oracle does not match {SOURCE_REVISION}: {name}"
            )
    runtime_files = [sdk / "lib/hew.lib", sdk / "lib/x86_64-pc-windows-msvc/hew.lib"]
    sdk_hashes = {str(path): digest(path) for path in [compiler, *runtime_files]}
    provenance = {
        "source_revision": SOURCE_REVISION,
        "diagnostic_revision": subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=root, text=True
        ).strip(),
        "source_sha256": SOURCE_HASHES,
        "sdk_source_revision": "24720d0d7d28b56fb53854309682f35796751606",
        "sdk_sha256": sdk_hashes,
        "compiler_version": subprocess.check_output(
            [str(compiler), "--version"], text=True
        ).strip(),
        "clang_version": subprocess.check_output(
            ["clang", "--version"], text=True
        ).strip(),
        "runtime_modified": False,
        "scope": "SDK source execution only; Windows retained-process-handle Rust unit not executed",
    }
    (evidence / "provenance.json").write_text(json.dumps(provenance, indent=2) + "\n")
    results = []
    for name in CASES:
        manifest = tomllib.loads(
            (root / f"tests/core-acceptance/cases/{name}.toml").read_text()
        )
        [case] = manifest["case"]
        assert case["id"] == name and not case.get("fixtures")
        source = root / "tests/core-acceptance" / case["source"]
        for level in (0, 2):
            case_dir = evidence / name / f"O{level}"
            case_dir.mkdir(parents=True)
            emit = binaries / name / f"O{level}"
            emit.mkdir(parents=True)
            environment = os.environ.copy()
            environment.pop("HEW_SANITIZE_ADDRESS", None)
            environment["HEW_STD"] = str(root / "std")
            result = {
                "case": name,
                "opt_level": level,
                "timeout_seconds": case["timeout_seconds"],
                "env": case.get("env", {}),
                "pass": False,
            }
            try:
                result["compile"] = execute(
                    [
                        str(compiler),
                        "compile",
                        "--emit-dir",
                        str(emit),
                        "--opt-level",
                        str(level),
                        str(source),
                    ],
                    case_dir / "compile",
                    case["timeout_seconds"],
                    environment,
                    root,
                )
                if result["compile"]["exit"] != 0:
                    result["failure"] = (
                        "compile-timeout"
                        if result["compile"]["outcome"] == "timeout"
                        else "compile-failed"
                    )
                else:
                    binary = emit / (source.stem + ".exe")
                    result["binary_sha256"] = digest(binary)
                    environment.update(case.get("env", {}))
                    result["run"] = execute(
                        [str(binary), *case.get("argv", [])],
                        case_dir / "run",
                        case["timeout_seconds"],
                        environment,
                        root,
                    )
                    expected = case["expected"]
                    result["stdout_matches"] = (
                        case_dir / "run.stdout"
                    ).read_bytes() == expected["stdout"].encode()
                    result["stderr_matches"] = (
                        case_dir / "run.stderr"
                    ).read_bytes() == expected["stderr"].encode()
                    result["pass"] = (
                        result["run"]["exit"] == expected["exit"]
                        and result["stdout_matches"]
                        and result["stderr_matches"]
                    )
                    if not result["pass"]:
                        result["failure"] = (
                            "run-timeout"
                            if result["run"]["outcome"] == "timeout"
                            else "source-oracle-mismatch"
                        )
            except OSError as error:
                result["failure"] = "environment-error"
                result["error"] = str(error)
            results.append(result)
            (case_dir / "result.json").write_text(json.dumps(result, indent=2) + "\n")
            print(f"{'PASS' if result['pass'] else 'FAIL'} {name} O{level}", flush=True)
    unchanged = all(digest(Path(path)) == value for path, value in sdk_hashes.items())
    (evidence / "result.json").write_text(
        json.dumps(
            {
                "cases": results,
                "sdk_unchanged": unchanged,
                "release_qualification": False,
            },
            indent=2,
        )
        + "\n"
    )
    return 0 if unchanged and all(result["pass"] for result in results) else 1


if __name__ == "__main__":
    raise SystemExit(main())
