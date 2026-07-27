#!/usr/bin/env python3
"""Lists the files whose contents define `libhew.a`.

This decides what makes Make rebuild the archive. An input that is missed here
is an edit that changes the archive without rebuilding it, which is how a stale
`libhew.a` reaches a link and fails as a wall of undefined `hew_*` symbols.

The set is derived, never hand-listed:

  * crates come from `hew-lib`'s non-dev path-dependency closure, read out of
    the manifests;
  * Rust sources, `Cargo.toml` and `build.rs` come from walking those crates;
  * embedded assets come from the `include_str!` / `include_bytes!` sites in
    those sources;
  * the workspace manifest and lockfile are always included, because a
    lockfile-selected dependency bump changes the archive while every crate
    source stays byte-identical.

Usage:
  libhew-inputs.py files   # every input file, workspace-relative, one per line
  libhew-inputs.py crates  # the resolved crate directories, one per line
"""

from __future__ import annotations

import os
import sys
import tomllib

ROOT_INPUT_CRATE = "hew-lib"
INPUT_FILE_NAMES = ("Cargo.toml", "build.rs")
INPUT_FILE_EXTENSIONS = (".rs",)
WORKSPACE_INPUT_FILES = ("Cargo.toml", "Cargo.lock")
ASSET_MACROS = ("include_str", "include_bytes")

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def die(message: str) -> "NoReturn":  # type: ignore[valid-type]
    print(f"libhew-inputs: {message}", file=sys.stderr)
    raise SystemExit(1)


def sibling_crate_name(path: str, manifest: str) -> str:
    """Every workspace member sits directly under the root, so a path
    dependency reads exactly `../<crate>`. Anything else is refused rather
    than guessed at: a dependency that cannot be placed cannot be hashed."""
    name = path[3:] if path.startswith("../") else ""
    if not name or "/" in name or "\\" in name or name.startswith("."):
        die(f"{manifest} has an unsupported path dependency '{path}'")
    return name


def path_dependencies(manifest: str) -> list[str]:
    with open(manifest, "rb") as handle:
        table = tomllib.load(handle)

    tables = [table]
    tables.extend(
        cfg for cfg in table.get("target", {}).values() if isinstance(cfg, dict)
    )

    found = []
    for scope in tables:
        # `dev-dependencies` is deliberately absent: nothing reached only
        # through it is compiled into the archive.
        for section in ("dependencies", "build-dependencies"):
            for spec in scope.get(section, {}).values():
                if isinstance(spec, dict) and "path" in spec:
                    found.append(sibling_crate_name(spec["path"], manifest))
    return found


def input_crates() -> list[str]:
    pending = [ROOT_INPUT_CRATE]
    seen: set[str] = set()
    while pending:
        name = pending.pop()
        if name in seen:
            continue
        seen.add(name)
        manifest = os.path.join(REPO_ROOT, name, "Cargo.toml")
        if not os.path.isfile(manifest):
            die(f"{manifest} does not exist; the input crate set cannot be resolved")
        pending.extend(path_dependencies(manifest))
    return sorted(seen)


def is_input_file(name: str) -> bool:
    return name in INPUT_FILE_NAMES or name.endswith(INPUT_FILE_EXTENSIONS)


def collect(directory: str, files: dict[str, str]) -> None:
    for entry in sorted(os.scandir(directory), key=lambda e: e.name):
        if entry.is_dir(follow_symlinks=False):
            # Build outputs are derived, not inputs.
            if entry.name != "target":
                collect(entry.path, files)
        elif is_input_file(entry.name):
            files[os.path.relpath(entry.path, REPO_ROOT)] = entry.path


def macro_call_sites(source: bytes, path: str) -> list[tuple[str, str]]:
    """Finds `include_str!` / `include_bytes!` sites and their arguments.

    A lexical pass, not a text search: comments, string literals and character
    literals are skipped, so a macro named in a doc comment or quoted inside a
    test fixture is not mistaken for a call, and a real call is not missed
    because a fixture mentioned it earlier in the file.
    """
    found: list[tuple[str, str]] = []
    at = 0
    size = len(source)

    while at < size:
        byte = source[at]

        if byte == 0x2F and at + 1 < size:  # '/'
            if source[at + 1] == 0x2F:
                end = source.find(b"\n", at)
                at = size if end < 0 else end
                continue
            if source[at + 1] == 0x2A:  # '*'
                depth, at = 1, at + 2
                while at < size and depth:
                    if source.startswith(b"/*", at):
                        depth, at = depth + 1, at + 2
                    elif source.startswith(b"*/", at):
                        depth, at = depth - 1, at + 2
                    else:
                        at += 1
                continue

        raw = raw_string_end(source, at)
        if raw is not None:
            at = raw
            continue

        if byte == 0x22:  # '"'
            at = string_end(source, at + 1)
            continue

        if byte == 0x27:  # '\''
            at = char_literal_end(source, at)
            continue

        if not is_ident_start(byte):
            at += 1
            continue

        end = ident_end(source, at)
        name = source[at:end].decode("ascii")
        if name not in ASSET_MACROS:
            at = end
            continue

        cursor = end
        if cursor >= size or source[cursor] != 0x21:  # '!'
            at = end
            continue
        cursor = skip_whitespace(source, cursor + 1)
        if cursor >= size or source[cursor] not in (0x28, 0x5B, 0x7B):  # ( [ {
            at = end
            continue

        argument = skip_whitespace(source, cursor + 1)
        if argument >= size or source[argument] != 0x22:
            die(
                f"{path} calls {name}! with something other than a plain string "
                "literal, so what it embeds cannot be tracked as an input"
            )
        end = string_end(source, argument + 1)
        found.append((name, unescape(source[argument + 1 : end - 1], name, path)))
        at = end

    return found


def is_ident_start(byte: int) -> bool:
    return byte == 0x5F or chr(byte).isascii() and chr(byte).isalpha()


def ident_end(source: bytes, at: int) -> int:
    while at < len(source):
        char = chr(source[at])
        if not (char.isascii() and (char.isalnum() or char == "_")):
            break
        at += 1
    return at


def skip_whitespace(source: bytes, at: int) -> int:
    while at < len(source) and chr(source[at]).isspace():
        at += 1
    return at


def raw_string_end(source: bytes, at: int) -> int | None:
    if at >= len(source):
        return None
    if source[at] == 0x72:  # 'r'
        prefix = 0
    elif source[at] in (0x62, 0x63) and source[at + 1 : at + 2] == b"r":
        prefix = 1
    else:
        return None

    hashes = 0
    while source[at + prefix + 1 + hashes : at + prefix + 2 + hashes] == b"#":
        hashes += 1
    if source[at + prefix + 1 + hashes : at + prefix + 2 + hashes] != b'"':
        return None

    close = b'"' + b"#" * hashes
    end = source.find(close, at + prefix + hashes + 2)
    return len(source) if end < 0 else end + len(close)


def string_end(source: bytes, at: int) -> int:
    while at < len(source):
        if source[at] == 0x5C:  # backslash
            at += 2
        elif source[at] == 0x22:
            return at + 1
        else:
            at += 1
    return len(source)


def char_literal_end(source: bytes, at: int) -> int:
    if source[at + 1 : at + 2] == b"\\":
        end = source.find(b"'", at + 2)
        return len(source) if end < 0 else end + 1
    if source[at + 2 : at + 3] == b"'":
        return at + 3
    return at + 1


ESCAPES = {
    0x5C: b"\\",
    0x22: b'"',
    0x27: b"'",
    0x6E: b"\n",
    0x72: b"\r",
    0x74: b"\t",
    0x30: b"\0",
}


def unescape(literal: bytes, name: str, path: str) -> str:
    out = bytearray()
    at = 0
    while at < len(literal):
        if literal[at] != 0x5C:
            out.append(literal[at])
            at += 1
            continue
        replacement = ESCAPES.get(literal[at + 1] if at + 1 < len(literal) else -1)
        if replacement is None:
            die(f"{path} uses a string escape in {name}! that cannot be resolved")
        out += replacement
        at += 2
    return out.decode("utf-8")


def relative_to_any(path: str, roots: list[str]) -> str | None:
    for root in roots:
        if path == root or path.startswith(root + os.sep):
            return os.path.relpath(path, REPO_ROOT)
    return None


def input_files() -> list[str]:
    crates = input_crates()
    roots = [os.path.join(REPO_ROOT, name) for name in crates]

    files: dict[str, str] = {}
    for root in roots:
        collect(root, files)

    for name in WORKSPACE_INPUT_FILES:
        path = os.path.join(REPO_ROOT, name)
        if not os.path.isfile(path):
            die(f"{path} does not exist")
        files[name] = path

    for rel, path in sorted(files.items()):
        if not rel.endswith(".rs"):
            continue
        with open(path, "rb") as handle:
            source = handle.read()
        # The lexical pass is the expensive part and make evaluates this list on
        # every invocation. A file that does not contain a macro's name anywhere
        # — comments and literals included — cannot contain a call to it, so the
        # substring test is a conservative filter, not a heuristic.
        if not any(name.encode() in source for name in ASSET_MACROS):
            continue
        for _, literal in macro_call_sites(source, path):
            asset = os.path.normpath(os.path.join(os.path.dirname(path), literal))
            # Assets resolving outside the scanned crates are test fixtures
            # reading stdlib sources, which are not compiled into the archive.
            asset_rel = relative_to_any(asset, roots)
            if asset_rel is None:
                continue
            if not os.path.isfile(asset):
                die(f"{path} embeds {asset}, which does not exist")
            files[asset_rel] = asset

    return sorted(files)


def main(argv: list[str]) -> int:
    if len(argv) != 2 or argv[1] not in ("files", "crates"):
        print(__doc__, file=sys.stderr)
        return 2
    lines = input_files() if argv[1] == "files" else input_crates()
    print("\n".join(lines))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
