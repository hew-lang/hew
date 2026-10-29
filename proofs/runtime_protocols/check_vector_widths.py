#!/usr/bin/env python3
"""Execute production vector bounds on native and WASI, plus emitted Hew code."""

import argparse
import json
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
LOGS = HERE / "target/proof-logs/vector-widths"
WASI = """
const fs = require('node:fs');
const {WASI} = require('node:wasi');
(async () => {
  const wasi = new WASI({version:'preview1', args:[], env:{}, preopens:{}, returnOnExit:true});
  const module = await WebAssembly.compile(fs.readFileSync(process.argv[1]));
  const instance = await WebAssembly.instantiate(module, {wasi_snapshot_preview1:wasi.wasiImport});
  process.exitCode = wasi.start(instance);
})().catch(error => { console.error(error); process.exitCode = 1; });
"""


def run(name, args):
    result = subprocess.run(
        [str(arg) for arg in args], cwd=ROOT, text=True, capture_output=True
    )
    (LOGS / f"{name}.log").write_text(result.stdout + result.stderr)
    if result.returncode:
        raise SystemExit(
            f"{name} failed ({result.returncode}); see {LOGS / (name + '.log')}"
        )
    return result.stdout


def wasm(path):
    return ["node", "--no-warnings", "-e", WASI, path]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runtime-only", action="store_true")
    args = parser.parse_args()
    LOGS.mkdir(parents=True, exist_ok=True)
    (LOGS / "report.json").unlink(missing_ok=True)
    report = []
    cargo = [
        "cargo",
        "build",
        "--manifest-path",
        HERE / "Cargo.toml",
        "--locked",
        "--features",
        "vector",
        "--example",
        "vector_width",
    ]
    for bits, target in [(64, None), (32, "wasm32-wasip1")]:
        run(f"runtime-build-{bits}", cargo + (["--target", target] if target else []))
        path = HERE / "target" / (target or "") / "debug/examples/vector_width"
        invocation = wasm(path.with_suffix(".wasm")) if target else [path]
        output = run(f"runtime-{bits}", invocation)
        result = json.loads(output)
        if result != {"pointer_bits": bits, "checks": 113, "violations": []}:
            raise SystemExit(f"Production Vec mismatch on {bits}-bit: {output}")
        report.append(result)
        print(
            f"Validated production Vec on {bits}-bit ({result['checks']} checks)",
            flush=True,
        )

    if not args.runtime_only:
        run("compiler-build", ["make", "hew-native"])
        run(
            "wasi-runtime-build",
            [
                "cargo",
                "build",
                "--locked",
                "-p",
                "hew-runtime",
                "--no-default-features",
                "--target",
                "wasm32-wasip1",
            ],
        )
        compiler = ROOT / "target/debug/hew"
        expected = "-1\n-1\n-1\n-1\n22\n22\n"
        for target in [None, "wasm32-wasip1"]:
            for level in [0, 2]:
                name = f"hew-{'wasm32' if target else 'native'}-O{level}"
                path = LOGS / (name + (".wasm" if target else ""))
                run(
                    name + "-build",
                    [
                        compiler,
                        "build",
                        HERE / "cases/vector_width.hew",
                        "--emit-llvm",
                        "--opt-level",
                        str(level),
                        "-o",
                        path,
                    ]
                    + (["--target", target] if target else []),
                )
                output = run(name, wasm(path) if target else [path])
                if output != expected:
                    raise SystemExit(f"Generated-code mismatch in {name}: {output!r}")
                report.append({"program": name, "output": output})
                print(f"Validated compiled Hew vector reads: {name}", flush=True)
    (LOGS / "report.json").write_text(json.dumps(report, indent=2) + "\n")


if __name__ == "__main__":
    main()
