#!/usr/bin/env python3
"""Write the deterministic seed corpus for compiled stdlib fuzzing."""

import pathlib
import sys
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE

SEEDS = {
    "base64": bytes([0]) + b"SGVsbG8=\n",
    "csv": bytes([2]) + b"name,age\nAlice,30\n",
    "json": bytes([3]) + b'{"name":"hew","safe":true}\n',
    "cbor": bytes.fromhex("08 a3 65 6c6162656c 61 78 65 636f756e74 01 67 656e61626c6564 f5"),
    "msgpack": bytes.fromhex("09 83 a5 6c6162656c a1 78 a5 636f756e74 01 a7 656e61626c6564 c3"),
}


def write() -> None:
    target = ROOT / "fuzz_stdlib_text"
    target.mkdir(parents=True, exist_ok=True)
    for name, data in SEEDS.items():
        (target / name).write_bytes(data)


def files(root: pathlib.Path) -> dict[pathlib.Path, bytes]:
    target = root / "fuzz_stdlib_text"
    return {
        path.relative_to(root): path.read_bytes()
        for path in target.glob("*")
        if path.is_file()
    }


def check() -> int:
    global ROOT
    with tempfile.TemporaryDirectory() as scratch:
        ROOT = pathlib.Path(scratch)
        write()
        expected = files(ROOT)
    actual = files(HERE)
    stale = sorted(expected.keys() | actual.keys())
    stale = [name for name in stale if expected.get(name) != actual.get(name)]
    for name in stale:
        print(f"stale seed: {name}", file=sys.stderr)
    return 1 if stale else 0


if __name__ == "__main__":
    if sys.argv[1:] == ["--check"]:
        raise SystemExit(check())
    write()
