#!/usr/bin/env python3
"""Build and finish the shipped Hew runtime archive (`libhew.a` / `hew.lib`).

The `release-lib` Cargo profile gives `hew-runtime` line tables and every other
crate none. Two things a profile cannot express are done here, in one place,
for the Makefile and the release workflow alike:

  build   run a Cargo command with the archive's compiler flags: frame
          pointers, C dependencies at `-g0` (ring forces `-g3`; a later
          `-g0` wins), and the checkout, Cargo and rustup homes remapped to
          `/hew`, `/cargo` and `/rustup` so no host path is embedded in the
          archive (a debugger maps `/hew` back to a checkout with
          `substitute-path`). The flags extend any RUSTFLAGS the caller
          set; otherwise they go in as a `target.'cfg(all())'` config entry,
          which Cargo joins with the per-target flags cross wrappers inject.
  strip   remove debug sections from every archive member that is not
          `hew-runtime`: the prebuilt libstd and compiler_builtins members
          carry their own line tables and the C dependencies are compiled
          with `-g3`. The archive is rewritten member by member, keeping
          every name, header and symbol table (with offsets repointed), so
          GNU, COFF and Darwin archives keep their own format; the result is
          renamed over the original, so readers never see a partial file,
          and a second run changes nothing.

Usage:
  release-lib.py build --archive PATH -- cargo build ... --profile release-lib
  release-lib.py strip --archive PATH
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import struct
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

# Archive members whose debug info is kept: the runtime's own code units.
RUNTIME_MEMBER_PREFIX = "hew_runtime-"

# Where the remapped prefixes appear in DWARF and panic locations.
REMAPS = (("CARGO_HOME", ".cargo", "/cargo"), ("RUSTUP_HOME", ".rustup", "/rustup"))

# Windows caps a command line near 32k characters; keep each batch well under.
BATCH = 100


def die(message: str) -> "None":
    print(f"release-lib: {message}", file=sys.stderr)
    sys.exit(1)


def toolchain_home(variable: str, default: str) -> Path:
    value = os.environ.get(variable)
    return Path(value) if value else Path.home() / default


def archive_rustflags() -> list[str]:
    flags = ["-Cforce-frame-pointers=yes"]
    roots = [(toolchain_home(v, d), display) for v, d, display in REMAPS]
    roots.append((REPO_ROOT, "/hew"))
    for root, display in roots:
        spellings = {str(root)}
        if root.exists():
            spellings.add(str(root.resolve()))
        flags.extend(
            f"--remap-path-prefix={path}={display}" for path in sorted(spellings)
        )
    return flags


def target_triple(cargo_args: list[str]) -> str:
    for index, arg in enumerate(cargo_args):
        if arg == "--target" and index + 1 < len(cargo_args):
            return cargo_args[index + 1]
        if arg.startswith("--target="):
            return arg.split("=", 1)[1]
    return os.environ.get("CARGO_BUILD_TARGET") or host_triple()


def host_triple() -> str:
    output = subprocess.run(
        ["rustc", "-vV"], capture_output=True, text=True, check=True
    ).stdout
    for line in output.splitlines():
        if line.startswith("host:"):
            return line.split(":", 1)[1].strip()
    die("rustc -vV did not report a host triple")
    return ""


def llvm_tool(name: str) -> str:
    """Find an LLVM binutils tool from the LLVM 22 toolchain Hew builds with."""
    candidates: list[str] = []
    for variable in ("LLVM_SYS_221_PREFIX", "LLVM_PREFIX"):
        prefix = os.environ.get(variable)
        if prefix:
            candidates.append(str(Path(prefix) / "bin" / name))
    for config in ("llvm-config", "llvm-config-22"):
        found = shutil.which(config)
        if found:
            bindir = subprocess.run(
                [found, "--bindir"], capture_output=True, text=True, check=False
            ).stdout.strip()
            if bindir:
                candidates.append(str(Path(bindir) / name))
    candidates.extend([name, f"{name}-22"])
    for candidate in candidates:
        found = shutil.which(candidate)
        if found:
            return found
    die(
        f"cannot find {name}; put LLVM 22's bin directory on PATH or set LLVM_SYS_221_PREFIX"
    )
    return ""


@dataclass
class Member:
    header: bytes  # the 60-byte ar header, size field rewritten on output
    name: str  # resolved member name; "" for symbol and name tables
    prefix: bytes  # BSD `#1/N` name bytes that precede the payload
    payload: bytes
    kind: str  # "object", "gnu-symtab", "gnu-symtab64", "coff-symtab", "bsd-symtab", "bsd-symtab64", "table"


def read_archive(data: bytes) -> list[Member]:
    """Parse a GNU, COFF (`lib.exe`) or BSD/Darwin archive, keeping every byte."""
    if not data.startswith(b"!<arch>\n"):
        die("not an ar archive")
    members: list[Member] = []
    long_names = b""
    pos = 8
    linker_members = 0
    while pos < len(data):
        header = data[pos : pos + 60]
        if len(header) < 60 or header[58:60] != b"`\n":
            die(f"malformed archive header at offset {pos}")
        raw = header[:16].decode("ascii", "replace").rstrip()
        size = int(header[48:58].decode("ascii").strip())
        body = data[pos + 60 : pos + 60 + size]
        pos += 60 + size + (size & 1)
        prefix, name, kind = b"", raw, "object"
        if raw.startswith("#1/"):
            length = int(raw[3:])
            prefix, body = body[:length], body[length:]
            name = prefix.rstrip(b"\0").decode("utf-8", "replace")
        if name == "/":
            linker_members += 1
            kind = "gnu-symtab" if linker_members == 1 else "coff-symtab"
        elif name == "/SYM64/":
            kind = "gnu-symtab64"
        elif name in ("__.SYMDEF", "__.SYMDEF SORTED"):
            kind = "bsd-symtab"
        elif name in ("__.SYMDEF_64", "__.SYMDEF_64 SORTED"):
            kind = "bsd-symtab64"
        elif name == "//":
            kind, long_names = "table", body
        elif name.startswith("/<"):
            kind = "table"
        elif name.startswith("/") and name[1:].isdigit():
            offset = int(name[1:])
            end = len(long_names)
            for terminator in (b"/\n", b"\0"):
                found = long_names.find(terminator, offset)
                if found != -1:
                    end = min(end, found)
            name = long_names[offset:end].decode("utf-8", "replace")
        name = name.rstrip("/") if kind == "object" else ""
        members.append(Member(header, name, prefix, body, kind))
    return members


# Object formats llvm-strip rewrites: ELF, Mach-O, and plain COFF (x86-64,
# AArch64, i386). Short import objects and anything else pass through as is.
MACH_O_MAGICS = (
    b"\xcf\xfa\xed\xfe",
    b"\xce\xfa\xed\xfe",
    b"\xfe\xed\xfa\xcf",
    b"\xfe\xed\xfa\xce",
)
COFF_MACHINES = (0x8664, 0xAA64, 0x014C)


def is_strippable(payload: bytes) -> bool:
    if payload[:4] == b"\x7fELF" or payload[:4] in MACH_O_MAGICS:
        return True
    return (
        len(payload) >= 20 and struct.unpack_from("<H", payload, 0)[0] in COFF_MACHINES
    )


def is_runtime_member(name: str) -> bool:
    # COFF archives name members by their full build path.
    return re.split(r"[\\/]", name)[-1].startswith(RUNTIME_MEMBER_PREFIX)


def write_archive(members: list[Member], original_offsets: list[int]) -> bytes:
    """Lay the members out again and repoint every symbol table entry."""
    offsets: dict[int, int] = {}
    pos = 8
    for member, old in zip(members, original_offsets):
        offsets[old] = pos
        size = len(member.prefix) + len(member.payload)
        pos += 60 + size + (size & 1)

    def relocate(old: int) -> int:
        if old not in offsets:
            die(f"symbol table names offset {old}, which starts no member")
        return offsets[old]

    out = bytearray(b"!<arch>\n")
    for member in members:
        body = bytearray(member.payload)
        if member.kind == "gnu-symtab":
            (count,) = struct.unpack_from(">I", body, 0)
            for i in range(count):
                struct.pack_into(
                    ">I",
                    body,
                    4 + 4 * i,
                    relocate(struct.unpack_from(">I", body, 4 + 4 * i)[0]),
                )
        elif member.kind == "gnu-symtab64":
            (count,) = struct.unpack_from(">Q", body, 0)
            for i in range(count):
                struct.pack_into(
                    ">Q",
                    body,
                    8 + 8 * i,
                    relocate(struct.unpack_from(">Q", body, 8 + 8 * i)[0]),
                )
        elif member.kind == "coff-symtab":
            (count,) = struct.unpack_from("<I", body, 0)
            for i in range(count):
                struct.pack_into(
                    "<I",
                    body,
                    4 + 4 * i,
                    relocate(struct.unpack_from("<I", body, 4 + 4 * i)[0]),
                )
        elif member.kind == "bsd-symtab":
            (length,) = struct.unpack_from("<I", body, 0)
            for entry in range(4, 4 + length, 8):
                struct.pack_into(
                    "<I",
                    body,
                    entry + 4,
                    relocate(struct.unpack_from("<I", body, entry + 4)[0]),
                )
        elif member.kind == "bsd-symtab64":
            (length,) = struct.unpack_from("<Q", body, 0)
            for entry in range(8, 8 + length, 16):
                struct.pack_into(
                    "<Q",
                    body,
                    entry + 8,
                    relocate(struct.unpack_from("<Q", body, entry + 8)[0]),
                )
        size = len(member.prefix) + len(body)
        header = member.header[:48] + f"{size:<10}".encode("ascii") + member.header[58:]
        out += header + member.prefix + body
        if size & 1:
            out += b"\n"
    return bytes(out)


def strip_archive(archive: Path) -> None:
    if not archive.is_file():
        die(f"missing archive: {archive}")
    llvm_strip = llvm_tool("llvm-strip")
    members = read_archive(archive.read_bytes())
    original_offsets = []
    pos = 8
    for member in members:
        original_offsets.append(pos)
        size = len(member.prefix) + len(member.payload)
        pos += 60 + size + (size & 1)
    objects = [m for m in members if m.kind == "object"]
    if not any(is_runtime_member(m.name) for m in objects):
        die(
            f"{archive}: no {RUNTIME_MEMBER_PREFIX}* member; is this the Hew runtime archive?"
        )
    bsd = any(m.prefix for m in members)

    targets = [
        (i, m)
        for i, m in enumerate(objects)
        if not is_runtime_member(m.name) and is_strippable(m.payload)
    ]
    with tempfile.TemporaryDirectory(prefix=".strip-", dir=archive.parent) as scratch:
        work = Path(scratch)
        for index, member in targets:
            (work / f"{index}.o").write_bytes(member.payload)
        names = [f"{index}.o" for index, _ in targets]
        for start in range(0, len(names), BATCH):
            subprocess.run(
                [llvm_strip, "--strip-debug", *names[start : start + BATCH]],
                cwd=work,
                check=True,
            )
        for index, member in targets:
            stripped = (work / f"{index}.o").read_bytes()
            if bsd:
                # Darwin linkers need each member's payload 8-byte aligned:
                # keep every member's length, and so every later offset,
                # congruent to the original modulo 8.
                stripped += b"\0" * ((len(member.payload) - len(stripped)) % 8)
            member.payload = stripped
        rebuilt = work / archive.name
        rebuilt.write_bytes(write_archive(members, original_offsets))
        os.replace(rebuilt, archive)


def build(archive: Path, cargo_args: list[str]) -> None:
    if not cargo_args:
        die("build needs a Cargo command after --")
    env = dict(os.environ)
    flags = archive_rustflags()
    encoded = env.get("CARGO_ENCODED_RUSTFLAGS")
    plain = env.get("RUSTFLAGS")
    if encoded is not None or plain is not None:
        # The caller's flags already outrank every config source; extend them.
        caller = encoded.split("\x1f") if encoded is not None else plain.split()
        env["CARGO_ENCODED_RUSTFLAGS"] = "\x1f".join([f for f in caller if f] + flags)
        env.pop("RUSTFLAGS", None)
    else:
        # A `target.<cfg>` entry is joined with the `target.<triple>`
        # rustflags a wrapper such as cargo-xwin or cargo-zigbuild injects,
        # where an environment value would replace them.
        value = "[" + ", ".join(json.dumps(flag) for flag in flags) + "]"
        option = ["--config", f"target.'cfg(all())'.rustflags={value}"]
        split = cargo_args.index("--") if "--" in cargo_args else len(cargo_args)
        cargo_args = cargo_args[:split] + option + cargo_args[split:]
    # cl.exe rejects `-g0`; MSVC members rely on the strip alone.
    if not target_triple(cargo_args).endswith("-msvc"):
        for variable in ("CFLAGS", "CXXFLAGS"):
            env[variable] = f"{env.get(variable, '')} -g0".strip()
    status = subprocess.run(cargo_args, env=env, check=False).returncode
    if status != 0:
        sys.exit(status)
    strip_archive(archive)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="command", required=True)
    for name in ("build", "strip"):
        command = sub.add_parser(name)
        command.add_argument("--archive", type=Path, required=True)
    args, rest = parser.parse_known_args()
    if rest and rest[0] == "--":
        rest = rest[1:]
    if args.command == "build":
        build(args.archive, rest)
    else:
        if rest:
            die(f"unexpected arguments: {' '.join(rest)}")
        strip_archive(args.archive)


if __name__ == "__main__":
    main()
