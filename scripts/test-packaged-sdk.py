#!/usr/bin/env python3
"""Execute the packaged SDK's source, relocation and native ABI contracts."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import signal
import subprocess
import sys
import tarfile
import time
import tomllib
import zipfile


def digest(path):
    with path.open("rb") as source:
        return hashlib.file_digest(source, "sha256").hexdigest()


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--archive", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    archive = args.archive.resolve()
    if not archive.is_file():
        parser.error(f"SDK archive does not exist: {archive}")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    logs = output / "logs"
    logs.mkdir()
    receipt = {
        "archive": str(archive),
        "archive_sha256": digest(archive),
        "script_sha256": digest(Path(__file__)),
        "selector_sha256": digest(root / "tests/packaged-sdk/cases.json"),
        "source_revision": os.environ.get("GITHUB_SHA"),
        "platform": platform.platform(),
        "commands": [],
        "cases": [],
        "case_inputs": {},
        "outcome": "running",
    }
    windows = sys.platform == "win32"
    suffix = ".exe" if windows else ""
    allowed = {
        "HOME",
        "USER",
        "LOGNAME",
        "PATH",
        "TMPDIR",
        "TEMP",
        "TMP",
        "LANG",
        "LC_ALL",
        "LD_LIBRARY_PATH",
        "SYSTEMROOT",
        "WINDIR",
        "COMSPEC",
        "USERPROFILE",
        "APPDATA",
        "LOCALAPPDATA",
        "HOMEDRIVE",
        "HOMEPATH",
        "PROGRAMDATA",
        "PROGRAMFILES",
        "PROGRAMFILES(X86)",
        "LIB",
        "LIBPATH",
        "INCLUDE",
        "MACOSX_DEPLOYMENT_TARGET",
    }
    env = {
        (key.upper() if windows else key): value
        for key, value in os.environ.items()
        if key.upper() in allowed
    }
    env.update(HEW_WORKERS="2", RAYON_NUM_THREADS="2", TOKIO_WORKER_THREADS="2")
    home = output / "home"
    home.mkdir()
    env["HOME"] = str(home)
    if windows:
        env["USERPROFILE"] = str(home)

    def save():
        (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")

    def run(label, command, *, cwd=None, seconds=120, expected=0):
        started = time.monotonic()
        process = subprocess.Popen(
            list(map(str, command)),
            cwd=cwd or output,
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            start_new_session=not windows,
        )
        timed_out = False
        try:
            stdout, stderr = process.communicate(timeout=seconds)
        except subprocess.TimeoutExpired:
            timed_out = True
            if windows:
                subprocess.run(
                    ["taskkill", "/PID", str(process.pid), "/T", "/F"],
                    capture_output=True,
                    timeout=15,
                )
            else:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            stdout, stderr = process.communicate(timeout=15)
        (logs / f"{label}.stdout").write_bytes(stdout)
        (logs / f"{label}.stderr").write_bytes(stderr)
        receipt["commands"].append(
            {
                "label": label,
                "argv": list(map(str, command)),
                "exit": process.returncode,
                "timeout": timed_out,
                "seconds": round(time.monotonic() - started, 3),
            }
        )
        save()
        require(not timed_out, f"{label}: exceeded {seconds}s")
        require(
            expected is None or process.returncode == expected,
            f"{label}: exit {process.returncode}, expected {expected}; see {logs}",
        )
        return (
            process.returncode,
            stdout.replace(b"\r\n", b"\n"),
            stderr.replace(b"\r\n", b"\n"),
        )

    try:
        extracted = output / "extracted"
        extracted.mkdir()
        if archive.suffix == ".zip":
            with zipfile.ZipFile(archive) as package:
                for item in package.infolist():
                    require(
                        not Path(item.filename).is_absolute()
                        and ".." not in Path(item.filename).parts,
                        f"invalid archive path: {item.filename}",
                    )
                package.extractall(extracted)
        else:
            with tarfile.open(archive) as package:
                package.extractall(extracted, filter="data")
        roots = [
            path
            for path in extracted.iterdir()
            if (path / "bin" / f"hew{suffix}").is_file()
        ]
        require(len(roots) == 1, "archive must contain one SDK root")
        sdk = output / "relocated-sdk"
        roots[0].rename(sdk)
        hew = sdk / "bin" / f"hew{suffix}"
        runtime = sdk / "lib" / ("hew.lib" if windows else "libhew.a")
        for path in (
            runtime,
            sdk / "std",
            sdk / "include/hew.h",
            sdk / "include/hew_host.h",
        ):
            require(path.exists(), f"missing packaged SDK input: {path}")
        receipt.update(compiler_sha256=digest(hew), runtime_sha256=digest(runtime))
        receipt["headers"] = {
            path.name: digest(path) for path in (sdk / "include").glob("*.h")
        }
        env["PATH"] = str(sdk / "bin") + os.pathsep + env.get("PATH", "")
        for tool in ("hew", "hew-lsp", "hew-observe"):
            _, version, _ = run(
                f"{tool}-version",
                [sdk / "bin" / f"{tool}{suffix}", "--version"],
                seconds=30,
            )
            receipt[tool + "_version"] = version.decode().strip()

        smoke = output / "pkg-smoke.hew"
        smoke.write_text('fn main() { println("pkg-smoke-ok"); }\n')
        _, stdout, stderr = run("packaged-smoke", [hew, "run", smoke])
        require(
            stdout == b"pkg-smoke-ok\n" and not stderr, "packaged hello output changed"
        )
        projects = output / "projects"
        projects.mkdir()
        run("init", [hew, "init", "demo"], cwd=projects, seconds=30)
        project = projects / "demo"
        sources = [
            path for path in project.rglob("*.hew") if "fn main" in path.read_text()
        ]
        require(len(sources) == 1, "init must produce one executable entrypoint")
        source = sources[0]
        executable = output / f"lifecycle{suffix}"
        run("init-build", [hew, "build", project, "-o", executable])
        run("init-run", [executable], seconds=30)
        for label, value in (("edit", 41), ("recovery", 42)):
            if label == "recovery":
                source.write_text(
                    'fn main() { let value: i64 = "wrong"; println(value); }\n'
                )
                code, stdout, stderr = run(
                    "source-diagnostic",
                    [hew, "build", project, "-o", executable],
                    expected=None,
                )
                diagnostic = (stdout + stderr).lower()
                require(
                    0 < code < 128
                    and b"error" in diagnostic
                    and b"internal compiler error" not in diagnostic,
                    "invalid source must produce an ordinary compiler diagnostic",
                )
            source.write_text(f"fn main() {{ println({value}); }}\n")
            run(f"{label}-build", [hew, "build", project, "-o", executable])
            _, stdout, stderr = run(f"{label}-run", [executable], seconds=30)
            require(
                stdout == f"{value}\n".encode() and not stderr,
                f"{label}: unexpected output",
            )

        run(
            "native-host-clients",
            [
                sys.executable,
                root / "tests/host/run.py",
                "--hew-bin",
                hew,
                "--hew-lib",
                runtime,
                "--include-dir",
                sdk / "include",
                "--out-dir",
                output / "host-clients",
            ],
            seconds=600,
        )
        source = root / "tests/packaged-sdk/bytes-client.c"
        for language in ("c", "cpp"):
            executable = output / f"bytes-{language}{suffix}"
            if windows:
                flags = (
                    ["/std:c11", "/TC"]
                    if language == "c"
                    else ["/std:c++17", "/EHsc", "/TP"]
                )
                command = [
                    "cl",
                    "/nologo",
                    "/MD",
                    *flags,
                    "/I" + str(sdk / "include"),
                    source,
                    "/Fe:" + str(executable),
                    "/link",
                    runtime,
                    "legacy_stdio_definitions.lib",
                    "ws2_32.lib",
                    "userenv.lib",
                    "bcrypt.lib",
                    "ntdll.lib",
                    "advapi32.lib",
                    "crypt32.lib",
                ]
            else:
                flags = (
                    ["-framework", "CoreFoundation", "-framework", "Security"]
                    if sys.platform == "darwin"
                    else []
                )
                command = [
                    "clang++",
                    "-x",
                    "c" if language == "c" else "c++",
                    "-std=c11" if language == "c" else "-std=c++17",
                    "-I",
                    sdk / "include",
                    source,
                    "-x",
                    "none",
                    runtime,
                    "-lpthread",
                    "-lm",
                    *flags,
                    "-o",
                    executable,
                ]
            run(f"bytes-{language}-build", command)
            _, stdout, stderr = run(f"bytes-{language}-run", [executable], seconds=30)
            require(
                stdout == b"sdk-native-consumer-ok\n" and not stderr,
                "packaged bytes ABI output changed",
            )
        cases = json.loads((root / "tests/packaged-sdk/cases.json").read_text())
        for selection in cases:
            contract = None
            if "contract" in selection:
                contract = root / selection["contract"]
                definitions = tomllib.loads(contract.read_text())["case"]
                require(len(definitions) == 1, f"expected one core case: {contract}")
                case = definitions[0]
                source = contract.parent.parent / case["source"]
            else:
                case = selection
                source = root / case["source"]
            expected = case["expected"]
            receipt["case_inputs"][case["id"]] = {
                "source_sha256": digest(source),
                "contract_sha256": digest(contract) if contract else None,
            }
            save()
            previous = dict(env)
            env.update(case.get("env", {}))
            for level in (0, 2):
                label = f"{case['id']}-O{level}"
                executable = output / f"{label}{suffix}"
                run(
                    label + "-build",
                    [
                        hew,
                        "build",
                        "--opt-level",
                        level,
                        "--emit-llvm",
                        source,
                        "-o",
                        executable,
                    ],
                )
                _, stdout, stderr = run(
                    label + "-run",
                    [executable, *case.get("args", [])],
                    seconds=case["timeout_seconds"],
                    expected=expected["exit"],
                )
                require(
                    stdout == expected["stdout"].encode()
                    and stderr == expected.get("stderr", "").encode(),
                    f"{label}: unexpected output; see {logs}",
                )
                llvm = executable.with_suffix(".ll")
                ir = llvm.read_text()
                run(
                    label + "-verify",
                    ["opt", "-passes=verify", "-disable-output", llvm],
                    seconds=30,
                )
                require(
                    "llvm.coro." not in ir, f"{label}: obsolete LLVM coroutine lowering"
                )
                if selection.get("expects_resume_frame", True):
                    require(
                        "$body" in ir
                        and "hew_cont_frame_alloc" in ir
                        and "switch i32" in ir,
                        f"{label}: missing explicit suspension state machine",
                    )
                if case["id"] == "hot-loop-dynamic" and level == 2:
                    blocks = [
                        match.group()
                        for match in re.finditer(
                            r"(?ms)^[A-Za-z$._][-A-Za-z$._0-9]*:[^\n]*\n.*?(?=^[A-Za-z$._][-A-Za-z$._0-9]*:|^\})",
                            ir,
                        )
                        if "1664525" in match.group()
                        and "2147483647" in match.group()
                        and re.search(
                            r"\bbr[^\n]*label %"
                            + re.escape(match.group().split(":", 1)[0])
                            + r"(?:[,\s]|$)",
                            match.group(),
                        )
                    ]
                    require(
                        blocks
                        and all(
                            not re.search(
                                r"\b(?:load|store|alloca|call)\b",
                                "\n".join(
                                    line.split(";", 1)[0] for line in block.splitlines()
                                ),
                            )
                            for block in blocks
                        ),
                        "optimized recurrence must keep loop state in registers",
                    )
                receipt["cases"].append(
                    {
                        "id": case["id"],
                        "opt_level": level,
                        **receipt["case_inputs"][case["id"]],
                        "llvm_sha256": digest(llvm),
                    }
                )
                save()
                print(f"PASS {label}", flush=True)
            env.clear()
            env.update(previous)

        receipt["outcome"] = "passed"
        save()
        print(f"PASS packaged SDK: {output / 'receipt.json'}", flush=True)
    except Exception as error:
        receipt.update(outcome="failed", failure=str(error))
        save()
        raise


if __name__ == "__main__":
    main()
