#!/usr/bin/env python3
"""Compare C aggregate return ABIs and execute field/byte-ownership oracles at O0/O2."""

import argparse
import os
from pathlib import Path
import re
import subprocess
import sys


def run(command, *, env=None, expected=0):
    print("+", " ".join(map(str, command)), flush=True)
    result = subprocess.run(command, env=env, capture_output=True, timeout=180)
    if result.returncode != expected:
        sys.stdout.buffer.write(result.stdout)
        sys.stderr.buffer.write(result.stderr)
        raise RuntimeError(f"command exited {result.returncode}, expected {expected}")
    return result


def expand_type(ir, spelling):
    definitions = dict(re.findall(r"^(%[^=]+?) = type (.+)$", ir, re.MULTILINE))
    return re.sub(
        r"%[\w.]+", lambda match: expand_type(ir, definitions[match[0]]), spelling
    )


def result_abi(ir, symbol):
    declaration = next(
        line
        for line in ir.splitlines()
        if line.startswith("declare ") and f"@{symbol}(" in line
    )
    prefix, parameters = declaration.split(f"@{symbol}(", 1)
    carrier = prefix.removeprefix("declare ").removeprefix("dso_local ").strip()
    indirect = re.search(r"sret\(([^)]+)\) align (\d+)", parameters)
    if indirect is None:
        return expand_type(ir, carrier), None
    # sret must be on the first parameter, before the source's i32 argument.
    if not parameters.startswith("ptr "):
        raise RuntimeError(f"sret is not the leading C parameter: {declaration}")
    pointee, align = indirect.groups()
    return carrier, (expand_type(ir, pointee), align)


def compare_target_abis(args, source, output, env):
    for triple in (
        "x86_64-unknown-linux-gnu",
        "x86_64-unknown-freebsd",
        "x86_64-apple-darwin",
        "x86_64-pc-windows-msvc",
        "x86_64-pc-windows-gnu",
        "aarch64-unknown-linux-gnu",
        "aarch64-unknown-freebsd",
        "aarch64-apple-darwin",
        "aarch64-pc-windows-msvc",
    ):
        directory = output / triple
        directory.mkdir(parents=True, exist_ok=True)
        c_ir = run(
            [
                args.cc,
                "-target",
                triple,
                "-S",
                "-emit-llvm",
                "-O0",
                source / "signature.c",
                "-o",
                "-",
            ],
            env=env,
        ).stdout.decode()
        for case, symbols in (
            ("signature", ("oracle_make",)),
            (
                "aggregate_results",
                (
                    "oracle_small",
                    "oracle_int_three",
                    "oracle_tiny",
                    "oracle_packed",
                    "oracle_big",
                    "oracle_mixed",
                    "oracle_reversed",
                    "oracle_float_pair",
                    "oracle_float_three",
                    "oracle_nested",
                    "oracle_hfa",
                ),
            ),
        ):
            run(
                [
                    args.hew_bin,
                    "build",
                    source / f"{case}.hew",
                    "--target",
                    triple,
                    "--emit-obj",
                    "--emit-llvm",
                    "-o",
                    directory / f"{case}.o",
                ],
                env=env,
            )
            hew_ir = (directory / f"{case}.ll").read_text()
            for symbol in symbols:
                if result_abi(hew_ir, symbol) != result_abi(c_ir, symbol):
                    raise RuntimeError(
                        f"{triple} {symbol}: Hew {result_abi(hew_ir, symbol)} != C {result_abi(c_ir, symbol)}"
                    )
            print(f"PASS Clang C return ABI: {triple} / {case}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hew-bin", type=Path, required=True)
    parser.add_argument("--out-dir", type=Path, required=True)
    parser.add_argument("--cc", default="clang")
    parser.add_argument("--sanitize", action="store_true")
    args = parser.parse_args()
    source = Path(__file__).resolve().parent
    output = args.out_dir.resolve()
    output.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    if args.sanitize:
        if not sys.platform.startswith("linux"):
            raise RuntimeError("extern C ASan/LSan acceptance currently requires Linux")
        env["HEW_SANITIZE_ADDRESS"] = "1"
        env["ASAN_OPTIONS"] = "detect_leaks=1"
        env["LSAN_OPTIONS"] = ""
    else:
        env.pop("HEW_SANITIZE_ADDRESS", None)
        compare_target_abis(args, source, output / "abi", env)
    for opt in ("0", "2"):
        directory = output / f"o{opt}"
        directory.mkdir(exist_ok=True)
        obj = directory / "oracle.o"
        flags = ["-std=c11", "-Wall", "-Wextra", "-Werror", "-g", "-O" + opt]
        if args.sanitize:
            flags += ["-fsanitize=address", "-fno-omit-frame-pointer"]
        run([args.cc, *flags, "-c", source / "oracle.c", "-o", obj])
        for case, exit_code, stdout, stderr in (
            ("round_trip", 0, b"owned extern bytes released: 34\n", b""),
            (
                "fault_cleanup",
                1,
                b"owned extern bytes released: 1\n",
                b"hew: failure: DivideByZero (202)\n",
            ),
            ("aggregate_results", 0, b"C aggregate results preserve all fields\n", b""),
        ):
            executable = directory / (case + (".exe" if os.name == "nt" else ""))
            run(
                [
                    args.hew_bin,
                    "build",
                    source / f"{case}.hew",
                    "--opt-level",
                    opt,
                    "--emit-llvm",
                    "--link-lib",
                    obj,
                    "-o",
                    executable,
                ],
                env=env,
            )
            if args.sanitize:
                ir = (directory / f"{case}.ll").read_text()
                if "sanitize_address" not in ir or "@__asan_init" not in ir:
                    raise RuntimeError("generated Hew code lacks ASan instrumentation")
            result = run([executable], env=env, expected=exit_code)
            if (
                result.stdout.replace(b"\r\n", b"\n") != stdout
                or result.stderr.replace(b"\r\n", b"\n") != stderr
            ):
                raise RuntimeError(
                    f"unexpected {case} output: {result.stdout!r}, {result.stderr!r}"
                )
            print(
                f"PASS {case} / C and Hew O{opt}"
                + (" / ASan+LSan" if args.sanitize else "")
            )


if __name__ == "__main__":
    main()
