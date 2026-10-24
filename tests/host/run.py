#!/usr/bin/env python3
"""Compile and execute the real C/C++ host client against this build of Hew."""

import argparse
import os
from pathlib import Path
import subprocess
import sys


def run(command, *, env=None):
    print("+", " ".join(map(str, command)), flush=True)
    result = subprocess.run(command, env=env, capture_output=True, timeout=180)
    if result.returncode:
        sys.stdout.buffer.write(result.stdout)
        sys.stderr.buffer.write(result.stderr)
        raise RuntimeError(f"command failed with status {result.returncode}")
    return result


def platform_flags():
    if sys.platform == "win32":
        return [
            "-Wl,/NODEFAULTLIB:libcmt",
            "-Wl,/DEFAULTLIB:msvcrt",
            "-llegacy_stdio_definitions",
            "-lws2_32",
            "-luserenv",
            "-lbcrypt",
            "-lntdll",
            "-ladvapi32",
            "-lcrypt32",
        ]
    if sys.platform == "darwin":
        return [
            "-lpthread",
            "-lm",
            "-framework",
            "CoreFoundation",
            "-framework",
            "Security",
        ]
    if sys.platform.startswith("freebsd"):
        return ["-lpthread", "-lm"]
    if sys.platform.startswith("linux"):
        return ["-lpthread", "-lm", "-ldl", "-lrt"]
    raise RuntimeError(f"no host-client link contract for {sys.platform}")


def shared_flags(directory, headers):
    if sys.platform == "win32":
        import re

        # The generated declarations, together with the public SDK header, own
        # the exports. COFF requires an explicit DLL export list.
        symbols = set()
        for header in headers:
            symbols.update(
                re.findall(
                    r"\b((?:hew_|HewOwned_|orders_|async_)\w+)\s*\(", header.read_text()
                )
            )
        definition = directory / "exports.def"
        definition.write_text("EXPORTS\n" + "\n".join(sorted(symbols)) + "\n")
        return ["-shared", f"-Wl,/DEF:{definition}"], ".dll"
    if sys.platform == "darwin":
        return ["-dynamiclib"], ".dylib"
    return ["-shared"], ".so"


def asynchronous_clients(args, root, directory, opt, env):
    include = args.include_dir or root / "hew-cabi/include"
    fixture = directory / "async_component.o"
    run(
        [
            args.hew_bin,
            "build",
            root / "tests/host/async_component.hew",
            "--emit-obj",
            "--emit-llvm",
            "--opt-level",
            opt,
            "--export-c",
            "open_marker=async_open_marker",
            *[
                argument
                for name, alias in [
                    ("add", "add"),
                    ("echo", "echo"),
                    ("wait_for_cancel", "wait"),
                    ("body_and_cleanup_fail", "fail"),
                    ("open_marker", "marker_new"),
                ]
                for argument in ("--export-c-async", f"{name}=async_{alias}")
            ],
            "-o",
            fixture,
        ],
        env=env,
    )
    for compiler, standard, language in [
        (args.cc, "c11", "c"),
        (args.cxx, "c++17", "c++"),
    ]:
        executable = directory / (
            "async-client-c" if language == "c" else "async-client-cpp"
        )
        if sys.platform == "win32":
            executable = executable.with_suffix(".exe")
        flags = [f"-std={standard}", f"-O{opt}", "-g", "-Wall", "-Wextra", "-Werror"]
        if args.sanitize:
            flags.extend(["-fsanitize=address", "-fno-omit-frame-pointer"])
        run(
            [
                compiler,
                *flags,
                "-I",
                include,
                "-I",
                directory,
                "-x",
                language,
                root / "tests/host/async_client.c",
                "-x",
                "none",
                fixture,
                args.hew_lib,
                *platform_flags(),
                "-o",
                executable,
            ]
        )
        result = run([executable, directory / f"async-marker-{language}"], env=env)
        if result.stdout or result.stderr:
            raise RuntimeError(
                f"unexpected async protocol output: {result.stdout!r}, {result.stderr!r}"
            )
        print(f"PASS async {standard} / Hew O{opt}")
    if args.sanitize:
        print(
            "Asyncio shared-library execution uses the ordinary host-client target; C async protocol is instrumented here."
        )
        return
    shared, suffix = shared_flags(
        directory, [include / "hew_host.h", fixture.with_suffix(".h")]
    )
    library = directory / ("async_component" + suffix)
    run([args.cc, *shared, fixture, args.hew_lib, *platform_flags(), "-o", library])
    result = run(
        [
            sys.executable,
            "-B",
            root / "tests/host/asyncio_client.py",
            library,
            directory / "asyncio-cleanup",
        ],
        env=env,
    )
    if result.stdout or result.stderr:
        raise RuntimeError(
            f"unexpected asyncio protocol output: {result.stdout!r}, {result.stderr!r}"
        )
    component = directory / "orders_component.o"
    run(
        [
            args.hew_bin,
            "build",
            root / "tests/host/copy_component.hew",
            "--emit-obj",
            "--opt-level",
            opt,
            "--pkg-path",
            root / "tests/core-acceptance/fixtures/construction-capabilities",
            *[
                argument
                for name in ("open_input", "open_output")
                for argument in ("--export-c", f"{name}=orders_{name}")
            ],
            *[
                argument
                for name in ("create", "run", "shutdown")
                for argument in ("--export-c-async", f"{name}=orders_{name}")
            ],
            "-o",
            component,
        ],
        env=env,
    )
    shared, suffix = shared_flags(
        directory, [include / "hew_host.h", component.with_suffix(".h")]
    )
    library = directory / ("orders_component" + suffix)
    run([args.cc, *shared, component, args.hew_lib, *platform_flags(), "-o", library])
    supplied = directory / "asyncio-orders"
    for child in ("input", "output"):
        (supplied / child).mkdir(parents=True, exist_ok=True)
    (supplied / "input/orders.csv").write_text("order-41")
    result = run(
        [
            sys.executable,
            "-B",
            root / "examples/embedding/asyncio_orders.py",
            library,
            supplied / "input",
            supplied / "output",
        ],
        env=env,
    )
    if (
        result.stdout.replace(b"\r\n", b"\n") != b"16\n"
        or result.stderr
        or (supplied / "output/receipt.csv").read_text() != "receipt:order-41"
    ):
        raise RuntimeError(
            f"asyncio order copier differs: {result.stdout!r}, {result.stderr!r}"
        )
    print(
        f"PASS asyncio cancellation, owned errors and supplied-directory copier / Hew O{opt}"
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hew-bin", type=Path, required=True)
    parser.add_argument("--hew-lib", type=Path, required=True)
    parser.add_argument("--out-dir", type=Path, required=True)
    parser.add_argument("--include-dir", type=Path)
    parser.add_argument("--cc", default="clang")
    parser.add_argument("--cxx", default="clang++")
    parser.add_argument("--sanitize", action="store_true")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    config_source = root / "tests/host/config_policy.hew"
    regex_source = root / "tests/host/regex_policy.hew"
    component_source = root / "tests/host/copy_component.hew"
    output = args.out_dir.resolve()
    output.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    if args.sanitize:
        if not sys.platform.startswith("linux"):
            raise RuntimeError("host ASan/LSan acceptance currently requires Linux")
        env["HEW_SANITIZE_ADDRESS"] = "1"
        env["ASAN_OPTIONS"] = "detect_leaks=1"
        env["LSAN_OPTIONS"] = ""
    for opt in ("0", "2"):
        directory = output / f"o{opt}"
        directory.mkdir(exist_ok=True)
        objects = []
        for stem, source, selections in (
            (
                "config_policy",
                config_source,
                ["normalize_label=config_normalize_label"],
            ),
            ("take_policy", config_source, ["take_label=config_take_label"]),
            ("regex_policy", regex_source, ["classify_text=config_classify_text"]),
            (
                "copy_component",
                component_source,
                [
                    f"{name}=orders_{name}"
                    for name in (
                        "open_input",
                        "open_output",
                        "create",
                        "run",
                        "shutdown",
                        "create_failed",
                        "faulting_owner",
                        "add",
                        "invert",
                        "abandon_faulting",
                        "unit_echo",
                    )
                ],
            ),
        ):
            obj = directory / f"{stem}.o"
            run(
                [
                    args.hew_bin,
                    "build",
                    source,
                    "--emit-obj",
                    "--emit-llvm",
                    *[
                        argument
                        for selection in selections
                        for argument in ("--export-c", selection)
                    ],
                    "--pkg-path",
                    root / "tests/core-acceptance/fixtures/construction-capabilities",
                    "--opt-level",
                    opt,
                    "-o",
                    obj,
                ],
                env=env,
            )
            objects.append(obj)
        for compiler, standard, language in (
            (args.cc, "c11", "c"),
            (args.cxx, "c++17", "c++"),
        ):
            executable = directory / ("client-c" if language == "c" else "client-cpp")
            if sys.platform == "win32":
                executable = executable.with_suffix(".exe")
            flags = [
                "-std=" + standard,
                "-O" + opt,
                "-g",
                "-Wall",
                "-Wextra",
                "-Werror",
            ]
            if args.sanitize:
                flags.extend(["-fsanitize=address", "-fno-omit-frame-pointer"])
            run(
                [
                    compiler,
                    *flags,
                    "-I",
                    args.include_dir or root / "hew-cabi/include",
                    "-I",
                    directory,
                    "-x",
                    language,
                    root / "tests/host/client.c",
                    "-x",
                    "none",
                    *objects,
                    args.hew_lib,
                    *platform_flags(),
                    "-o",
                    executable,
                ]
            )
            supplied = directory / ("supplied-c" if language == "c" else "supplied-cpp")
            for child in ("input", "output", "missing"):
                (supplied / child).mkdir(parents=True, exist_ok=True)
            (supplied / "input/orders.csv").write_text("order-41")
            marker = supplied / "close-marker"
            result = run(
                [
                    executable,
                    supplied / "input",
                    supplied / "output",
                    supplied / "missing",
                    marker,
                ],
                env=env,
            )
            if (
                supplied / "output/receipt.csv"
            ).read_text() != "receipt:order-41" or marker.read_text() != "closed once":
                raise RuntimeError("host component copy/release result differs")
            if (
                result.stdout.replace(b"\r\n", b"\n")
                != b"host client: compiled Hew, independent values and owned errors OK\n"
                or result.stderr
            ):
                raise RuntimeError(
                    f"unexpected client output: {result.stdout!r}, {result.stderr!r}"
                )
            print(
                f"PASS {standard} / Hew O{opt}"
                + (" / ASan+LSan" if args.sanitize else "")
            )
        asynchronous_clients(args, root, directory, opt, env)


if __name__ == "__main__":
    main()
