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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hew-bin", type=Path, required=True)
    parser.add_argument("--hew-lib", type=Path, required=True)
    parser.add_argument("--out-dir", type=Path, required=True)
    parser.add_argument("--cc", default="clang")
    parser.add_argument("--cxx", default="clang++")
    parser.add_argument("--sanitize", action="store_true")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    source = root / "tests/host/config_policy.hew"
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
        for stem, selection in (
            ("config_policy", "normalize_label=config_normalize_label"),
            ("take_policy", "take_label=config_take_label"),
        ):
            obj = directory / f"{stem}.o"
            run(
                [
                    args.hew_bin,
                    "build",
                    source,
                    "--emit-obj",
                    "--emit-llvm",
                    "--export-c",
                    selection,
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
                    root / "hew-cabi/include",
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
            result = run([executable], env=env)
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


if __name__ == "__main__":
    main()
