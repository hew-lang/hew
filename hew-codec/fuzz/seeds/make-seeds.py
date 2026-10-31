#!/usr/bin/env python3
"""Write the deterministic seed corpus for the hew-codec fuzz targets.

Run from any directory; seeds land beside this script, one directory per
target. The output is byte-for-byte reproducible: `--check` regenerates into
a scratch directory and fails when the checked-in seeds differ.
"""

import pathlib
import sys
import tempfile
import struct

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE

JSON, YAML, TOML, CBOR, MSGPACK = range(5)

DOC_JSON = (
    b'{"name":"edge","port":8080,"tags":["a","b"],"limits":{"x":1,"y":-2},'
    b'"blob":"AAEC","timeout":30,"outcome":{"Ok":7},"ratio":0.5,"pair":[1,2.5],'
    b'"ids":[3,1,2],"grade":"A","tree":{"value":1,"children":[{"value":2,"children":[]}]},'
    b'"codes":[[1,"one"]],"small":1.5,"triple":[1,-2,3]}'
)
DOC_YAML = b"""name: edge
port: 8080
tags: [a, b]
limits: {x: 1, y: -2}
blob: AAEC
timeout: 30
outcome: {Ok: 7}
ratio: 0.5
pair: [1, 2.5]
ids: [3, 1, 2]
grade: A
tree: {value: 1, children: [{value: 2, children: []}]}
small: 1.5
triple: [1, -2, 3]
"""
DOC_TOML = b"""name = "edge"
port = 8080
tags = ["a", "b"]
blob = "AAEC"
timeout = 30
ratio = 0.5
pair = [1, 2.5]
ids = [3, 1, 2]
grade = "A"
small = 1.5
triple = [1, -2, 3]
[limits]
x = 1
y = -2
[outcome]
Ok = 7
[tree]
value = 1
children = []
"""


def cbor_head(major, n):
    if n < 24:
        return bytes([major << 5 | n])
    if n < 256:
        return bytes([major << 5 | 24, n])
    if n < 65536:
        return bytes([major << 5 | 25]) + struct.pack(">H", n)
    return bytes([major << 5 | 26]) + struct.pack(">I", n)


def cbor_text(s):
    b = s.encode()
    return cbor_head(3, len(b)) + b


def mp_str(s):
    b = s.encode()
    return bytes([0xA0 | len(b)]) + b


CBOR_DOC = (
    cbor_head(5, 3)
    + cbor_text("name")
    + cbor_text("edge")
    + cbor_text("port")
    + cbor_head(0, 8080)
    + cbor_text("tags")
    + cbor_head(4, 2)
    + cbor_text("a")
    + cbor_text("b")
)
MSGPACK_DOC = (
    bytes([0x83])
    + mp_str("name")
    + mp_str("edge")
    + mp_str("port")
    + b"\xcd"
    + struct.pack(">H", 8080)
    + mp_str("tags")
    + b"\x92"
    + mp_str("a")
    + mp_str("b")
)

# Declared lengths far beyond the input, deep nesting and alias fan-out.
HOSTILE = {
    "cbor-huge-array": bytes([CBOR]) + b"\x9a\xff\xff\xff\xff\x00",
    "cbor-huge-bytes": bytes([CBOR]) + b"\x5b\x00\x00\x00\xff\xff\xff\xff\xff\x00",
    "cbor-deep": bytes([CBOR]) + b"\x81" * 200 + b"\x00",
    "cbor-tag": bytes([CBOR]) + b"\xc1\x1a\x00\x00\x00\x00",
    "msgpack-huge-array": bytes([MSGPACK]) + b"\xdd\xff\xff\xff\xff\xc0",
    "msgpack-huge-map": bytes([MSGPACK]) + b"\xdf\xff\xff\xff\xff\xc0\xc0",
    "msgpack-huge-str": bytes([MSGPACK]) + b"\xdb\xff\xff\xff\xff",
    "msgpack-deep": bytes([MSGPACK]) + b"\x91" * 200 + b"\xc0",
    "msgpack-ext": bytes([MSGPACK]) + b"\xd4\x01\x02",
    "json-deep": bytes([JSON]) + b"[" * 200 + b"]" * 200,
    "json-bignum": bytes([JSON])
    + b"[1e400, -1e400, 123456789012345678901234567890, -0, 1E-400]",
    "json-surrogate": bytes([JSON]) + b'["\\ud83d\\ude00", "\\udc00"]',
    "yaml-deep": bytes([YAML]) + b"[" * 200 + b"]" * 200,
    "yaml-tags": bytes([YAML])
    + b"a: !!binary AAEC\nb: !custom {x: 1}\nc: .nan\nd: -.inf\n",
    "toml-datetime": bytes([TOML])
    + b"a = 1979-05-27T07:32:00Z\nb = 07:32:00\nc = nan\nd = -inf\n",
    "toml-deep": bytes([TOML]) + b"a = " + b"[" * 200 + b"]" * 200 + b"\n",
}


def laughs(levels, fan):
    """Billion laughs: each level aliases the previous one `fan` times."""
    lines = ['l0: &l0 "lol"']
    for i in range(1, levels + 1):
        refs = ", ".join(f"*l{i - 1}" for _ in range(fan))
        lines.append(f"l{i}: &l{i} [{refs}]")
    return ("\n".join(lines) + "\n").encode()


def wide_alias(items, refs):
    """One large anchor referenced from a second anchor, itself referenced."""
    big = ", ".join("x" for _ in range(items))
    mid = ", ".join("*a" for _ in range(refs))
    top = ", ".join("*b" for _ in range(refs))
    return f"a: &a [{big}]\nb: &b [{mid}]\nc: [{top}]\n".encode()


YAML_SEEDS = {
    "plain": DOC_YAML,
    "laughs": laughs(9, 10),
    "wide-alias": wide_alias(400, 400),
    # A small source whose admitted tree approaches the absolute expansion
    # budget; unlike the larger bomb above, this must fit the heap oracle.
    "wide-alias-admitted": wide_alias(300, 50),
    "quoted-alias": b"a: &a [x]\nb: 'it''s *a'\nc: [*a, *a]\n",
    "comment-alias": b"a: &a [x] # it's\nb: [*a, *a]\r# '\rc: *a\n",
    "block-scalar": b"a: &a [x]\nb: |\n  don't # here\n  \"quoted\nc: [*a, *a, *a]\n",
    "merge": b"base: &base {a: 1}\nderived:\n  <<: *base\n  b: 2\n",
}


def write(target, name, data):
    path = ROOT / target / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(data)


def main():
    docs = {
        "json": bytes([JSON]) + DOC_JSON,
        "yaml": bytes([YAML]) + DOC_YAML,
        "toml": bytes([TOML]) + DOC_TOML,
        "cbor": bytes([CBOR]) + CBOR_DOC,
        "msgpack": bytes([MSGPACK]) + MSGPACK_DOC,
    }
    for target in ("fuzz_codec_formats", "fuzz_codec_typed"):
        for name, data in {**docs, **HOSTILE}.items():
            write(target, name, data)
    write("fuzz_codec_json", "doc", DOC_JSON)
    for name, data in HOSTILE.items():
        if data[0] == JSON:
            write("fuzz_codec_json", name, data[1:])
    write("fuzz_codec_json", "dup", b'{"a":1,"a":2}')
    for name, data in YAML_SEEDS.items():
        write("fuzz_yaml_value", name, data)


def seed_files(root):
    return {
        path.relative_to(root): path.read_bytes()
        for path in root.glob("fuzz_*/*")
        if path.is_file()
    }


def check():
    global ROOT
    with tempfile.TemporaryDirectory() as scratch:
        ROOT = pathlib.Path(scratch)
        main()
        fresh = seed_files(ROOT)
    stale = sorted(
        str(name)
        for name in fresh.keys() | seed_files(HERE).keys()
        if fresh.get(name) != seed_files(HERE).get(name)
    )
    for name in stale:
        print(f"stale seed: {name}", file=sys.stderr)
    return 1 if stale else 0


if __name__ == "__main__":
    if sys.argv[1:] == ["--check"]:
        sys.exit(check())
    main()
