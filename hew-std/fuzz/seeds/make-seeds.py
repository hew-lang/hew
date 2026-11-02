#!/usr/bin/env python3
"""Write the deterministic seed corpus for the hew-std fuzz targets.

Run from any directory; seeds land beside this script, one directory per
target. The output is byte-for-byte reproducible: `--check` regenerates into
a scratch directory and fails when the checked-in seeds differ.
"""

import base64
import gzip
import hashlib
import hmac
import json
import pathlib
import sys
import tempfile
import struct
import zlib

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE


def write(target, name, data):
    path = ROOT / target / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(data if isinstance(data, bytes) else data.encode())


def value_formats():
    seeds = {
        "json": b"\x00" + b'{"a":[1,2.5,-3,true,null,"x\\u00e9"],"b":{"c":1e300}}',
        "json-deep": b"\x00" + b"[" * 140 + b"]" * 140,
        "json-bignum": b"\x00" + b"[18446744073709551616, -0.0, 1e-400]",
        "yaml": b"\x01"
        + b"a: [1, 2.5, -3, true, ~, 'x']\nb: {c: !!binary AAEC}\nd: !tag 1\n",
        "yaml-alias": b"\x01" + b"a: &a {x: 1}\nb: *a\nc: [*a, *a]\n",
        "yaml-floats": b"\x01" + b"[.nan, .inf, -.inf, 1e400, 0x1F, 0o17, +12]\n",
        "yaml-key": b"\x01" + b"? [complex, key]\n: value\n? {a: 1}\n: b\n",
        "toml": b"\x02"
        + b'a = 1\nb = "x"\n[t]\nd = 1979-05-27T07:32:00Z\n[[arr]]\nx = 1\n',
        "toml-special": b"\x02"
        + b"a = nan\nb = -inf\nc = 0x1F\nd = 07:32:00\ne = 1979-05-27\n",
        "toml-empty": b"\x02",
    }
    for name, data in seeds.items():
        write("fuzz_value_formats", name, data)


def msgpack():
    seeds = {
        "map": b"\x82\xa1a\x93\x01\xcb\x40\x04\x00\x00\x00\x00\x00\x00\xc0\xa1b\xc3",
        "huge-array": b"\xdd\xff\xff\xff\xff\xc0",
        "huge-map": b"\xdf\xff\xff\xff\xff\xc0\xc0",
        "huge-str": b"\xdb\xff\xff\xff\xff",
        "huge-ext": b"\xc9\xff\xff\xff\xff\x01",
        "deep": b"\x91" * 200 + b"\xc0",
        "ext": b"\xd4\x01\x02",
        "bin": b"\xc4\x03abc",
        "u64": b"\xcf\xff\xff\xff\xff\xff\xff\xff\xff",
        "f32nan": b"\xca\x7f\xc0\x00\x00",
        "int-key": b"\x81\x01\x02",
    }
    for name, data in seeds.items():
        write("fuzz_msgpack", name, data)


def varint(n):
    out = bytearray()
    while True:
        b = n & 0x7F
        n >>= 7
        if n:
            out.append(b | 0x80)
        else:
            out.append(b)
            return bytes(out)


def protobuf():
    seeds = {
        "fields": varint(1 << 3 | 0)
        + varint(150)
        + varint(2 << 3 | 2)
        + varint(5)
        + b"hello"
        + varint(3 << 3 | 1)
        + struct.pack("<Q", 7)
        + varint(4 << 3 | 5)
        + struct.pack("<I", 9),
        "huge-len": varint(1 << 3 | 2) + b"\xff\xff\xff\xff\x0f",
        "overlong-varint": varint(1 << 3) + b"\xff" * 10 + b"\x01",
        "max-field": varint(((1 << 29) - 1) << 3) + b"\x01",
        "group-wire": varint(1 << 3 | 3) + varint(1 << 3 | 4),
        "repeated": (varint(1 << 3) + b"\x01") * 8,
    }
    for name, data in seeds.items():
        write("fuzz_protobuf", name, data)


def xml():
    seeds = {
        "doc": '<?xml version="1.0"?><root a="1" b=\'x&amp;y\'><c>t&lt;</c><d/><![CDATA[<raw>]]></root>',
        "deep": "<a>" * 300 + "</a>" * 300,
        "entities": "<r>&#65;&#x42;&apos;&quot;&gt;</r>",
        "attr-ws": '<r a="x&#9;y&#10;z" b="&#13;"/>',
        "text-cr": "<r>a\r\nb\rc</r>",
        "pi-comment": "<!-- c --><?pi x?><r><!-- in --></r>",
        "doctype": '<!DOCTYPE r [<!ENTITY e "boom">]><r>&e;</r>',
        "ns": '<x:r xmlns:x="u"><x:c/></x:r>',
    }
    for name, data in seeds.items():
        write("fuzz_xml", name, data)


def compress():
    payload = b"hello hello hello hello " * 8
    limit = struct.pack(">H", 64)
    bomb = b"\x00" * (1 << 20)
    seeds = {
        "gzip": b"\x00" + limit + gzip.compress(payload, mtime=0),
        "deflate": b"\x01" + limit + zlib.compress(payload)[2:-4],
        "zlib": b"\x02" + limit + zlib.compress(payload),
        "gzip-bomb": b"\x00" + struct.pack(">H", 1) + gzip.compress(bomb, mtime=0),
        "zlib-bomb": b"\x02" + struct.pack(">H", 1) + zlib.compress(bomb, 9),
        "gzip-multi": b"\x00"
        + limit
        + gzip.compress(b"a", mtime=0)
        + gzip.compress(b"b", mtime=0),
        "gzip-fname": b"\x00"
        + limit
        + b"\x1f\x8b\x08\x08\x00\x00\x00\x00\x00\xff"
        + b"n" * 64,
    }
    for name, data in seeds.items():
        write("fuzz_compress", name, data)


def markdown():
    seeds = {
        "basic": '# Title {#id .class onclick=x}\n\n*em* **strong** `code` [link](https://x.test "t")\n',
        "html": "<script>alert(1)</script>\n\n<img src=x onerror=alert(1)>\n",
        "js-link": "[a](javascript:alert(1)) [b](JaVaScRiPt:x) [c](java%0ascript:x) ![i](data:text/html,x)\n",
        "entity-link": "[a](&#106;avascript:x) [b](javascript&colon;x) [c](&#x6A;avascript:alert)\n",
        "autolink": "<javascript:alert(1)> <mailto:a@b.test> <https://ok.test>\n",
        "refdef": "[x]\n\n[x]: vbscript:msgbox\n",
        "table": "| a | b |\n|:--|--:|\n| 1 | 2 |\n",
        "footnote": "x[^1]\n\n[^1]: note <b>x</b>\n",
        "tasks": "- [x] done\n- [ ] todo\n",
        "math": "$x<y$ and $$\\frac{a}{b}$$\n",
        "wikilink": "[[javascript:alert(1)]] [[Page|text]]\n",
        "deflist": "term\n: definition\n",
        "alert": "> [!NOTE]\n> text\n",
        "codeinfo": '```js" onclick="x\nbody\n```\n',
        "metadata": "---\ntitle: x\n---\nbody\n",
        "nesting": ">" * 200 + " deep\n" + "- " * 200 + "x\n",
    }
    for name, data in seeds.items():
        write("fuzz_markdown", name, data)


def ws_frame(opcode, payload, mask=None, fin=True):
    head = bytes([(0x80 if fin else 0) | opcode])
    n = len(payload)
    mbit = 0x80 if mask else 0
    if n < 126:
        head += bytes([mbit | n])
    elif n < 65536:
        head += bytes([mbit | 126]) + struct.pack(">H", n)
    else:
        head += bytes([mbit | 127]) + struct.pack(">Q", n)
    if mask:
        head += mask
        payload = bytes(b ^ mask[i % 4] for i, b in enumerate(payload))
    return head + payload


# RFC 6455's example nonce, the key the client side of the harness sends.
WS_KEY = base64.b64encode(b"the sample nonce")


def websocket():
    request = (
        b"GET /chat HTTP/1.1\r\nHost: example.test\r\nUpgrade: websocket\r\n"
        b"Connection: Upgrade\r\nSec-WebSocket-Key: " + WS_KEY + b"\r\n"
        b"Sec-WebSocket-Version: 13\r\nSec-WebSocket-Protocol: chat\r\n\r\n"
    )
    response = (
        b"HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n"
        b"Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nSec-WebSocket-Protocol: chat\r\n\r\n"
    )
    mask = b"\x01\x02\x03\x04"
    seeds = {
        "server-text": b"\x00" + request + ws_frame(1, b"hello", mask),
        "server-frag": b"\x00"
        + request
        + ws_frame(1, b"hel", mask, fin=False)
        + ws_frame(9, b"p", mask)
        + ws_frame(0, b"lo", mask),
        "server-huge-len": b"\x00"
        + request
        + b"\x82\xff"
        + struct.pack(">Q", 1 << 62)
        + mask,
        "server-cap-len": b"\x00"
        + request
        + b"\x82\xff"
        + struct.pack(">Q", (1 << 20) + 1)
        + mask,
        "server-close": b"\x00" + request + ws_frame(8, b"\x03\xe8bye", mask),
        "server-bad-utf8": b"\x00" + request + ws_frame(1, b"\xff\xfe", mask),
        "client-text": b"\x01" + response + ws_frame(1, b"hello"),
        "client-binary": b"\x01" + response + ws_frame(2, bytes(range(256))),
        "client-redirect": b"\x01"
        + b"HTTP/1.1 302 Found\r\nLocation: wss://other.test:8443/x?y\r\n\r\n",
        "client-redirect-rel": b"\x01"
        + b"HTTP/1.1 301 Moved\r\nLocation: /relative\r\n\r\n",
        "client-other-proto": b"\x01"
        + response.replace(b"Protocol: chat", b"Protocol: evil"),
        "client-many-headers": b"\x01"
        + b"HTTP/1.1 101 OK\r\n"
        + b"X: y\r\n" * 200
        + b"\r\n",
    }
    for name, data in seeds.items():
        write("fuzz_websocket", name, data)


def b64url(data):
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode()


def token(header, claims, key):
    """A compact JWS signed with HMAC-SHA256 under `key` (empty: unsigned)."""
    signing = (
        b64url(json.dumps(header).encode()) + "." + b64url(json.dumps(claims).encode())
    )
    if not key:
        return signing + "."
    mac = hmac.new(key.encode(), signing.encode(), hashlib.sha256).digest()
    return signing + "." + b64url(mac)


def jwt():
    key = "k" * 40
    hs256 = token({"alg": "HS256", "typ": "JWT"}, {"sub": "x", "exp": 4102444800}, key)
    seeds = {
        "hs256": "\x00" + hs256 + "\0" + key,
        "expired": "\x00"
        + token({"alg": "HS256"}, {"sub": "x", "exp": 1}, key)
        + "\0"
        + key,
        "payload": "\x00" + '{"sub":"x","exp":4102444800}' + "\0" + key,
        "none-alg": "\x00" + token({"alg": "none"}, {"sub": "x"}, "") + "\0" + key,
        "short-key": "\x01" + hs256 + "\0k",
        "garbage": "\x02a.b.c.d\0" + key,
        "bad-aud": "\x00" + '{"aud":[1,2],"sub":3}' + "\0" + key,
    }
    for name, data in seeds.items():
        write("fuzz_jwt", name, data)


def regex():
    seeds = {
        "basic": r"(\w+)@(\w+)\.com" + "\0" + "a@b.com c@d.com" + "\0" + "$2:$1",
        "named": r"(?P<y>\d{4})-(?P<m>\d\d)" + "\0" + "2026-10" + "\0" + "${m}/${y}",
        "empty": "\0\0",
        "alternation": "(a|aa)*b" + "\0" + "a" * 64 + "\0x",
        "repeat": "(x{1000}){1000}" + "\0x\0",
        "unicode": r"\p{Greek}+\b" + "\0" + "αβγ δ" + "\0" + "$0$0$0",
        "groups": "(a?)" * 200 + "\0" + "a" * 100 + "\0$1",
        "backref": r"(a)\1" + "\0aa\0",
        "flags": "(?i-u:K)(?s:.)" + "\0k\n\0",
    }
    for name, data in seeds.items():
        write("fuzz_regex", name, data)


def calendar():
    def stamp(seconds, nanos):
        return struct.pack("<qI", seconds, nanos)

    seeds = {
        "iso": stamp(0, 0) + b"2026-10-29T12:00:00Z\0%Y-%m-%dT%H:%M:%SZ",
        "offset": stamp(1_700_000_000, 5)
        + b"2026-10-29 12:00 +05:30\0%Y-%m-%d %H:%M %z",
        "lossless": stamp(-62_135_596_800, 999_999_999)
        + b"0001-01-01T00:00:00.999999999+00:00\0%Y-%m-%dT%H:%M:%S%.9f%:z",
        "leap": stamp(0, 0) + b"2016-12-31T23:59:60Z\0%Y-%m-%dT%H:%M:%SZ",
        "extreme": stamp(2**63 - 1, 2**32 - 1) + b"+262143-12-31\0%Y-%m-%d",
        "min": stamp(-(2**63), 0) + b"-262144-01-01\0%Y-%m-%d",
        "directives": stamp(86_400, 0)
        + b"Thu 01 Jan\0%a %d %b %G %V %j %U %W %s %e %k %P %Z %#z %:::z %%",
        "bad": stamp(0, 0) + b"x\0%Q%-%",
    }
    for name, data in seeds.items():
        write("fuzz_calendar", name, data)


def net_text():
    seeds = {
        "url": "https://user:pw@example.test:8443/a/b?x=1&y=%20#frag\0../c?d",
        "url-ipv6": "http://[::ffff:127.0.0.1]:80/\0//other.test/x",
        "url-idna": "http://ex\u00e4mple.test/\u2603\0\u00e9",
        "url-file": "file:///c:/x/../y\0\\\\server\\share",
        "url-data": "data:text/plain,hi\0#x",
        "ipv4": "192.168.1.1\0" + "10.0.0.0/8",
        "ipv6": "fe80::1%eth0\0::1",
        "cidr4": "10.0.0.0/8\0" + "10.1.2.3",
        "cidr4-0": "0.0.0.0/0\0" + "8.8.8.8",
        "cidr6-0": "::/0\0::1",
        "cidr6-65": "2001:db8::/65\0" + "2001:db8::1",
        "cidr4-31": "192.168.0.0/31\0" + "192.168.0.1",
        "uuid": "550e8400-e29b-41d4-a716-446655440000\0",
        "uuid-braced": "{550e8400-e29b-41d4-a716-446655440000}\0",
        "uuid-urn": "urn:uuid:550e8400-e29b-41d4-a716-446655440000\0",
    }
    for name, data in seeds.items():
        write("fuzz_net_text", name, data)


def main():
    value_formats()
    msgpack()
    protobuf()
    xml()
    compress()
    markdown()
    websocket()
    jwt()
    regex()
    calendar()
    net_text()


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
