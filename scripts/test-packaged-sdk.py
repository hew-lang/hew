#!/usr/bin/env python3
"""Execute the packaged SDK's source, relocation and native ABI contracts."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
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


def check_live_sdk(sdk, compiler_identity, run):
    """Exercise the relocated Linux experimental SDK's same-build boundary."""
    require(
        sys.platform == "linux" and platform.machine() == "x86_64",
        "experimental live SDK acceptance is qualified only for Linux x86_64",
    )
    runtime = sdk / "lib/hew/libhew.so"
    transport = sdk / "lib/hew/live-node/network.so"
    manifest = json.loads((transport.parent / "network.json").read_text())
    require(
        manifest["compiler"] == compiler_identity,
        "live transport compiler identity differs",
    )
    require(
        manifest["runtime_sha256"] == digest(runtime), "live transport runtime differs"
    )
    require(
        manifest["module_sha256"] == digest(transport), "live transport module differs"
    )
    for path in (
        sdk / "lib/wasm32-wasip1/libhew_runtime.a",
        sdk / "lib/wasm32-wasip1/libhew_std.a",
        sdk / "share/hew/live/README.md",
        sdk / "share/hew/live/router.hew",
    ):
        require(path.is_file(), f"missing experimental live SDK input: {path}")
    run("live-node-help", [sdk / "bin/hew-live-node", "--help"])
    _, stdout, stderr = run(
        "live-native-session",
        [sdk / "bin/hew", "tool", "live", "--runtime", runtime],
        input=(
            b"actor Counter { var value: i64 = 40; receive fn next() -> i64 { value += 1; value } }\n"
            b"let counter = spawn Counter;\n"
            b'counter.next().expect("counter")\n'
            b'counter.next().expect("counter")\n'
            b"stop(counter); stopped(counter);\n:quit\n"
        ),
    )
    require(
        stdout == b"41\n42\n" and not stderr, "relocated live session output changed"
    )
    return {"runtime_sha256": digest(runtime), "transport": manifest}


def check_windows_debug_sdk(sdk, debug_archive, output, source, run):
    """Exercise the relocated compiler, its matching symbols and generated PDBs."""
    with zipfile.ZipFile(debug_archive) as package:
        entries = package.infolist()
        for entry in entries:
            require(
                not Path(entry.filename).is_absolute()
                and ".." not in Path(entry.filename).parts,
                f"invalid debug archive path: {entry.filename}",
            )
        pdbs = [entry for entry in entries if entry.filename.endswith("/hew.pdb")]
        require(len(pdbs) == 1, "debug archive must contain one compiler PDB")
        packaged_pdb = output / "packaged-hew.pdb"
        with package.open(pdbs[0]) as source_file, packaged_pdb.open("wb") as target:
            shutil.copyfileobj(source_file, target)

    hew = sdk / "bin/hew.exe"
    fixture = output / "p.hew"
    shutil.copyfile(source, fixture)
    binary = output / "p.exe"
    generated_pdb = binary.with_suffix(".pdb")
    run("debug-build", [hew, "build", "-g", fixture, "-o", binary])
    require(generated_pdb.is_file(), "packaged compiler did not generate a PDB")

    # The symbolizer must locate the adjacent PDB and reject a different build.
    # This negative control ensures that the successful check actually loaded it.
    adjacent_pdb = sdk / "bin/hew.pdb"
    shutil.copyfile(generated_pdb, adjacent_pdb)
    code, _, stderr = run(
        "debug-wrong-pdb", [hew, "symbolize", hew, "0"], seconds=300, expected=None
    )
    require(
        code != 0 and b"different build" in stderr,
        "mismatched compiler PDB was not rejected",
    )
    shutil.copyfile(packaged_pdb, adjacent_pdb)
    run("debug-compiler-pdb", [hew, "symbolize", hew, "0"], seconds=300)

    failure_log = output / "failure.ndjson"
    code, _, _ = run(
        "debug-failure",
        [binary],
        expected=None,
        extra_env={"HEW_BACKTRACE": "1", "HEW_FAILURE_LOG": str(failure_log)},
    )
    require(code != 0, "debug fixture did not fail")
    record = json.loads(failure_log.read_text().splitlines()[0])
    require(record["message"] == "too big", "debug fixture recorded the wrong failure")
    frame = next(
        (frame for frame in record["frames"] if frame["hew_fn"] == "level2"), None
    )
    require(frame is not None, "debug fixture did not record level2")
    base = record["build"]["image_base"]
    require(int(base, 16) != 0, "debug fixture has no runtime image base")
    for label, symbol_file in (("image", binary), ("pdb", generated_pdb)):
        _, stdout, _ = run(
            f"debug-symbolize-{label}",
            [
                hew,
                "symbolize",
                symbol_file,
                frame["pc"],
                f"--base={base}",
                "--return-addresses",
            ],
        )
        require(
            b"level2" in stdout and b"p.hew:3" in stdout,
            f"{label} did not resolve the failure site",
        )
    return {
        "archive_sha256": digest(debug_archive),
        "compiler_pdb_sha256": digest(packaged_pdb),
    }


def check_macos_debug_sdk(sdk, debug_archive, output, run):
    """Verify the archived compiler DWARF matches the packaged Mach-O."""
    with tarfile.open(debug_archive) as package:
        entries = package.getmembers()
        for entry in entries:
            require(
                not Path(entry.name).is_absolute()
                and ".." not in Path(entry.name).parts,
                f"invalid debug archive path: {entry.name}",
            )
        dwarf = [
            entry
            for entry in entries
            if "/hew.dSYM/Contents/Resources/DWARF/" in entry.name and entry.isfile()
        ]
        require(len(dwarf) == 1, "debug archive must contain one compiler DWARF file")
        plist = [
            entry
            for entry in entries
            if entry.name.endswith("/hew.dSYM/Contents/Info.plist") and entry.isfile()
        ]
        require(len(plist) == 1, "debug archive must contain the dSYM Info.plist")
        packaged_dwarf = output / "packaged-hew.dwarf"
        with (
            package.extractfile(dwarf[0]) as source_file,
            packaged_dwarf.open("wb") as target,
        ):
            shutil.copyfileobj(source_file, target)

    _, image_uuid, _ = run(
        "debug-compiler-uuid", ["dwarfdump", "--uuid", sdk / "bin/hew"]
    )
    _, dwarf_uuid, _ = run("debug-dsym-uuid", ["dwarfdump", "--uuid", packaged_dwarf])
    uuid = rb"UUID: ([0-9A-Fa-f-]+) \(arm64\)"
    image_match = re.search(uuid, image_uuid)
    dwarf_match = re.search(uuid, dwarf_uuid)
    require(
        image_match and dwarf_match and image_match.group(1) == dwarf_match.group(1),
        "compiler and debug archive UUIDs differ",
    )
    _, lines, _ = run(
        "debug-dsym-lines", ["dwarfdump", "--debug-line=0", packaged_dwarf]
    )
    require(b"file_names[" in lines, "compiler debug archive has no source line table")
    return {
        "archive_sha256": digest(debug_archive),
        "compiler_dwarf_sha256": digest(packaged_dwarf),
    }


def require_register_recurrence(ir, loop_info):
    identifier = r'"(?:[^"\\]|\\.)*"|[-A-Za-z$._0-9]+'
    functions = {}
    for match in re.finditer(
        rf"(?ms)^define[^\n]*?@({identifier})\([^\n]*\n(.*?)^\}}", ir
    ):
        body = match.group(2)
        labels = list(re.finditer(rf"(?m)^({identifier}):[^\n]*\n", body))
        functions[match.group(1).strip('"')] = {
            label.group(1): body[
                label.end() : labels[index + 1].start()
                if index + 1 < len(labels)
                else len(body)
            ]
            for index, label in enumerate(labels)
        }

    function = None
    selected = []
    for line in loop_info.splitlines():
        header = re.fullmatch(r"Loop info for function '(.*)':", line)
        if header:
            function = header.group(1)
            continue
        loop = re.search(r"Loop at depth \d+ containing: (.*)$", line)
        if not loop:
            continue
        require(function in functions, f"missing LLVM function body: {function}")
        members = re.findall(rf"%({identifier})(?=[<,\s]|$)", loop.group(1))
        require(members, f"missing LLVM loop members: {line}")
        blocks = functions[function]
        require(
            all(member in blocks for member in members),
            f"missing LLVM loop member body: {line}",
        )
        code = "\n".join(
            instruction.split(";", 1)[0]
            for member in members
            for instruction in blocks[member].splitlines()
        )
        if not all(re.search(rf"\b{value}\b", code) for value in (1664525, 2147483647)):
            continue
        # Check complete natural loops, including enclosing loops when present.
        # A clean arithmetic block must not hide a spill or call in its header.
        forbidden = re.search(
            rf"(?m)^\s*(?:%({identifier})\s*=\s*)?"
            r"(?:(?:tail|musttail|notail)\s+)?"
            r"(?:load|store|alloca|call|invoke|callbr|atomicrmw|cmpxchg)\s",
            code,
        )
        require(
            not forbidden,
            "optimized recurrence must keep loop state in registers: "
            f"{function} loop {', '.join(members)}",
        )
        selected.append((function, members))
    require(selected, "optimized recurrence must contain a natural loop")
    return selected


def case_profile_failures(cases, run_profile):
    for selection in cases:
        for level in (0, 2):
            try:
                run_profile(selection, level)
            except Exception as error:
                yield selection, level, error


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--archive", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--debug-archive", type=Path)
    parser.add_argument(
        "--experimental-live",
        action="store_true",
        help="require the Linux x86_64 live companions and same-build runtime",
    )
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
        "case_failures": [],
        "case_inputs": {},
        "outcome": "running",
    }
    windows = sys.platform == "win32"
    macos = sys.platform == "darwin"
    if (windows or macos) and args.debug_archive is None:
        parser.error("Windows and macOS SDK acceptance require --debug-archive")
    if args.debug_archive is not None and not (windows or macos):
        parser.error("--debug-archive is currently supported only on Windows and macOS")
    if args.debug_archive is not None and not args.debug_archive.is_file():
        parser.error(f"debug archive does not exist: {args.debug_archive}")
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

    def run(
        label, command, *, cwd=None, seconds=120, expected=0, input=None, extra_env=None
    ):
        started = time.monotonic()
        process = subprocess.Popen(
            list(map(str, command)),
            cwd=cwd or output,
            env=env | (extra_env or {}),
            stdin=subprocess.PIPE if input is not None else None,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            start_new_session=not windows,
        )
        timed_out = False
        try:
            stdout, stderr = process.communicate(input=input, timeout=seconds)
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
        expected_bins = [f"hew{suffix}"]
        if args.experimental_live:
            expected_bins += ["hew-live-host", "hew-live-node"]
        require(
            sorted(path.name for path in (sdk / "bin").iterdir())
            == sorted(expected_bins),
            "SDK executable membership differs from the selected product",
        )
        for command in ([], ["lsp"], ["observe"]):
            label = "-".join(["hew", *command])
            _, version, _ = run(
                f"{label}-version",
                [hew, *command, "--version"],
                seconds=30,
            )
            receipt[label + "_version"] = version.decode().strip()

        if args.experimental_live:
            receipt["experimental_live"] = check_live_sdk(
                sdk, receipt["hew_version"], run
            )
            save()

        if windows:
            receipt["windows_debug"] = check_windows_debug_sdk(
                sdk,
                args.debug_archive,
                output,
                root / "tests/packaged-sdk/debug-failure.hew",
                run,
            )
            save()

        if macos:
            receipt["macos_debug"] = check_macos_debug_sdk(
                sdk, args.debug_archive, output, run
            )
            save()

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

        def run_profile(selection, level):
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
            try:
                env.update(case.get("env", {}))
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
                _, stdout, _ = run(
                    label + "-run",
                    [executable, *case.get("args", [])],
                    seconds=case["timeout_seconds"],
                    expected=expected["exit"],
                )
                require(
                    stdout == expected["stdout"].encode(),
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
                    "llvm.coro." not in ir,
                    f"{label}: unlowered LLVM coroutine intrinsic",
                )
                if case["id"] == "hot-loop-dynamic" and level == 2:
                    _, loop_stdout, loop_stderr = run(
                        label + "-loops",
                        ["opt", "-passes=print<loops>", "-disable-output", llvm],
                        seconds=30,
                    )
                    require_register_recurrence(
                        ir, (loop_stdout + loop_stderr).decode()
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
            finally:
                env.clear()
                env.update(previous)

        for selection, level, error in case_profile_failures(cases, run_profile):
            receipt["case_failures"].append(
                {"selection": selection, "opt_level": level, "failure": str(error)}
            )
            save()
            name = selection.get("id", selection.get("contract"))
            print(f"FAIL {name}-O{level}: {error}", flush=True)
        require(
            not receipt["case_failures"],
            f"{len(receipt['case_failures'])} packaged SDK case/profile failures; "
            f"see {output / 'receipt.json'}",
        )

        receipt["outcome"] = "passed"
        save()
        print(f"PASS packaged SDK: {output / 'receipt.json'}", flush=True)
    except Exception as error:
        receipt.update(outcome="failed", failure=str(error))
        save()
        raise


if __name__ == "__main__":
    main()
