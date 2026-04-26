#!/usr/bin/env python3

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
VERIFY_FFI = ROOT / "scripts" / "verify-ffi-symbols.py"
POSITIVE_CONTROL_SYMBOL = "hew_io_write"
NEGATIVE_CONTROL_SYMBOL = "hew_shutdown_initiate"
HOST_STUB_BASENAME = "hew_jit_host_stub"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--cc", required=True)
    parser.add_argument("--work-dir", required=True)
    return parser.parse_args()


def run(
    cmd: list[str],
    *,
    cwd: Path,
    expect_success: bool,
    failure_message: str,
) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(
        cmd,
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )
    if expect_success and result.returncode != 0:
        raise AssertionError(
            f"{failure_message}\ncommand: {' '.join(cmd)}\n"
            f"stdout:\n{result.stdout}\n"
            f"stderr:\n{result.stderr}"
        )
    return result


def classified_symbols(kind: str) -> set[str]:
    result = run(
        [sys.executable, str(VERIFY_FFI), "--classify", kind, "--validate"],
        cwd=ROOT,
        expect_success=True,
        failure_message=f"failed to load `{kind}` JIT symbol classification",
    )
    return {line.strip() for line in result.stdout.splitlines() if line.strip()}


def shared_library_name(stem: str) -> str:
    if sys.platform == "darwin":
        return f"lib{stem}.dylib"
    if sys.platform.startswith("linux"):
        return f"lib{stem}.so"
    raise AssertionError(
        f"unsupported platform for jit_negative_link harness: {sys.platform}"
    )


def shared_library_flags() -> list[str]:
    if sys.platform == "darwin":
        return ["-dynamiclib"]
    if sys.platform.startswith("linux"):
        return ["-shared", "-fPIC"]
    raise AssertionError(
        f"unsupported platform for jit_negative_link harness: {sys.platform}"
    )


def unresolved_symbol_flags() -> list[str]:
    if sys.platform == "darwin":
        return ["-Wl,-undefined,error"]
    if sys.platform.startswith("linux"):
        return ["-Wl,-z,defs"]
    raise AssertionError(
        f"unsupported platform for jit_negative_link harness: {sys.platform}"
    )


def diagnostic_mentions_unresolved(output: str) -> bool:
    lowered = output.lower()
    needles = (
        "unresolved",
        "symbol not found",
        "undefined symbol",
        "undefined reference",
        "not found",
    )
    return any(needle in lowered for needle in needles)


def write_host_stub(work_dir: Path, symbol: str) -> Path:
    source = work_dir / "host_stub.c"
    source.write_text(
        f"void {symbol}(void) {{}}\n",
        encoding="utf-8",
    )
    return source


def write_module_source(work_dir: Path, name: str, symbol: str) -> Path:
    source = work_dir / f"{name}.c"
    source.write_text(
        "\n".join(
            [
                f"extern void {symbol}(void);",
                f"void {name}_entry(void) {{",
                f"  {symbol}();",
                "}",
                "",
            ]
        ),
        encoding="utf-8",
    )
    return source


def build_host_stub(cc: str, work_dir: Path, stable_symbol: str) -> Path:
    output = work_dir / shared_library_name(HOST_STUB_BASENAME)
    run(
        [
            cc,
            *shared_library_flags(),
            str(write_host_stub(work_dir, stable_symbol)),
            "-o",
            str(output),
        ],
        cwd=work_dir,
        expect_success=True,
        failure_message="failed to build host-side stable-symbol stub library",
    )
    return output


def build_module(
    cc: str,
    *,
    work_dir: Path,
    module_name: str,
    referenced_symbol: str,
    expect_success: bool,
) -> subprocess.CompletedProcess[str]:
    output = work_dir / shared_library_name(module_name)
    return run(
        [
            cc,
            *shared_library_flags(),
            str(write_module_source(work_dir, module_name, referenced_symbol)),
            f"-L{work_dir}",
            f"-l{HOST_STUB_BASENAME}",
            *unresolved_symbol_flags(),
            "-o",
            str(output),
        ],
        cwd=work_dir,
        expect_success=expect_success,
        failure_message=f"failed to link test module for symbol `{referenced_symbol}`",
    )


def main() -> int:
    args = parse_args()
    work_dir = Path(args.work_dir).resolve()
    shutil.rmtree(work_dir, ignore_errors=True)
    work_dir.mkdir(parents=True, exist_ok=True)

    stable_symbols = classified_symbols("stable")
    internal_symbols = classified_symbols("internal")
    assert POSITIVE_CONTROL_SYMBOL in stable_symbols, (
        f"{POSITIVE_CONTROL_SYMBOL} must stay classified as stable "
        f"in {VERIFY_FFI.parent / 'jit-symbol-classification.toml'}"
    )
    assert NEGATIVE_CONTROL_SYMBOL in internal_symbols, (
        f"{NEGATIVE_CONTROL_SYMBOL} must stay classified as internal "
        f"in {VERIFY_FFI.parent / 'jit-symbol-classification.toml'}"
    )

    build_host_stub(args.cc, work_dir, POSITIVE_CONTROL_SYMBOL)

    build_module(
        args.cc,
        work_dir=work_dir,
        module_name="jit_positive_control",
        referenced_symbol=POSITIVE_CONTROL_SYMBOL,
        expect_success=True,
    )

    negative = build_module(
        args.cc,
        work_dir=work_dir,
        module_name="jit_negative_control",
        referenced_symbol=NEGATIVE_CONTROL_SYMBOL,
        expect_success=False,
    )
    combined_output = "\n".join(
        part for part in (negative.stdout, negative.stderr) if part
    )
    if negative.returncode == 0:
        raise AssertionError(
            f"expected link failure for internal symbol `{NEGATIVE_CONTROL_SYMBOL}`, but it linked"
        )
    if not diagnostic_mentions_unresolved(combined_output):
        raise AssertionError(
            "negative link failed without a clean unresolved-symbol diagnostic\n"
            f"stdout:\n{negative.stdout}\n"
            f"stderr:\n{negative.stderr}"
        )
    if (
        NEGATIVE_CONTROL_SYMBOL not in combined_output
        and f"_{NEGATIVE_CONTROL_SYMBOL}" not in combined_output
    ):
        raise AssertionError(
            "negative link diagnostic did not name the internal symbol\n"
            f"stdout:\n{negative.stdout}\n"
            f"stderr:\n{negative.stderr}"
        )

    print(f"positive control linked with stable symbol `{POSITIVE_CONTROL_SYMBOL}`")
    print(f"negative control rejected internal symbol `{NEGATIVE_CONTROL_SYMBOL}`")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
