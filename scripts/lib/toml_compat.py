"""Small TOML reader for the repository's checked-in configuration files.

Python 3.11 ships :mod:`tomllib`; the macOS 3.10 builder does not.  Pulling a
package at validation time is not an option, so scripts use this module rather
than importing ``tomllib`` directly.  On 3.11+ it deliberately delegates to
the standard library.  The fallback implements the TOML constructs used by
Cargo manifests and the three repository policy manifests: tables, array
tables, dotted/quoted keys, basic and literal strings (including multiline
strings), arrays, inline tables, booleans, integers and floats.  Unsupported
or malformed input raises ``TOMLDecodeError`` instead of guessing.

Set ``HEW_FORCE_TOML_FALLBACK=1`` in a test to exercise the Python 3.10 path
on a newer interpreter.
"""

from __future__ import annotations

import os
import re
from typing import Any, BinaryIO


class TOMLDecodeError(ValueError):
    """Fallback equivalent of ``tomllib.TOMLDecodeError``."""


_FORCE_FALLBACK = os.environ.get("HEW_FORCE_TOML_FALLBACK") == "1"
try:
    if _FORCE_FALLBACK:
        raise ModuleNotFoundError
    import tomllib as _stdlib_tomllib
except ModuleNotFoundError:
    _stdlib_tomllib = None

if _stdlib_tomllib is not None:
    TOMLDecodeError = _stdlib_tomllib.TOMLDecodeError


def _error(message: str) -> "None":
    raise TOMLDecodeError(message)


def _statements(text: str) -> list[str]:
    """Split TOML into complete top-level statements, discarding comments."""
    statements: list[str] = []
    current: list[str] = []
    at = 0
    depth = 0
    quote = ""
    multiline = False
    escaped = False
    while at < len(text):
        char = text[at]
        if quote:
            if multiline:
                if not escaped and text.startswith(quote * 3, at):
                    run = 3
                    while at + run < len(text) and text[at + run] == quote:
                        run += 1
                    if run > 5:
                        _error("too many quotes at multiline-string delimiter")
                    current.extend(quote * run)
                    at += run
                    quote = ""
                    multiline = False
                    escaped = False
                    continue
                current.append(char)
                if quote == '"' and char == "\\" and not escaped:
                    escaped = True
                else:
                    escaped = False
                at += 1
                continue
            if char in "\r\n":
                _error("newline in single-line string")
            current.append(char)
            if quote == '"' and char == "\\" and not escaped:
                escaped = True
            elif char == quote and not escaped:
                quote = ""
            else:
                escaped = False
            at += 1
            continue
        if char in ("'", '"'):
            quote = char
            multiline = text.startswith(char * 3, at)
            current.append(char)
            at += 1
            if multiline:
                current.extend(char * 2)
                at += 2
            continue
        if char == "#":
            while at < len(text) and text[at] not in "\r\n":
                if ord(text[at]) < 0x20 and text[at] != "\t" or ord(text[at]) == 0x7F:
                    _error("control character in comment")
                at += 1
            continue
        if char in "[{":
            depth += 1
        elif char in "]}":
            depth -= 1
            if depth < 0:
                _error("unexpected closing delimiter")
        if char in "\r\n" and depth == 0:
            statement = "".join(current).strip()
            if statement:
                statements.append(statement)
            current = []
            if char == "\r" and at + 1 < len(text) and text[at + 1] == "\n":
                at += 1
        else:
            current.append(char)
        at += 1
    if quote:
        _error("unterminated string")
    if depth:
        _error("unterminated array or inline table")
    statement = "".join(current).strip()
    if statement:
        statements.append(statement)
    return statements


def _split_top_level(text: str, delimiter: str) -> list[str]:
    parts: list[str] = []
    start = 0
    at = 0
    depth = 0
    quote = ""
    multiline = False
    escaped = False
    while at < len(text):
        char = text[at]
        if quote:
            if multiline:
                if not escaped and text.startswith(quote * 3, at):
                    run = 3
                    while at + run < len(text) and text[at + run] == quote:
                        run += 1
                    if run > 5:
                        _error("too many quotes at multiline-string delimiter")
                    at += run
                    quote = ""
                    multiline = False
                    escaped = False
                    continue
                if quote == '"' and char == "\\" and not escaped:
                    escaped = True
                else:
                    escaped = False
                at += 1
                continue
            if char in "\r\n":
                _error("newline in single-line string")
            if quote == '"' and char == "\\" and not escaped:
                escaped = True
            elif char == quote and not escaped:
                quote = ""
            else:
                escaped = False
            at += 1
            continue
        if char in ("'", '"'):
            quote = char
            multiline = text.startswith(char * 3, at)
            at += 3 if multiline else 1
            continue
        if char in "[{":
            depth += 1
        elif char in "]}":
            depth -= 1
            if depth < 0:
                _error("unexpected closing delimiter")
        elif char == delimiter and depth == 0:
            parts.append(text[start:at].strip())
            start = at + 1
        at += 1
    if quote or depth:
        _error("unterminated value")
    parts.append(text[start:].strip())
    return parts


def _key_parts(text: str) -> list[str]:
    parts = _split_top_level(text, ".")
    if not parts or any(not part for part in parts):
        _error("empty key")
    result: list[str] = []
    for part in parts:
        if part.startswith('"') or part.startswith("'"):
            value = _value(part)
            if not isinstance(value, str):
                _error("key must be a string")
            result.append(value)
        elif re.fullmatch(r"[A-Za-z0-9_-]+", part):
            result.append(part)
        else:
            _error(f"invalid key {part!r}")
    return result


def _basic_string(token: str) -> str:
    multiline = token.startswith('"""')
    marker = '"""' if multiline else '"'
    if not token.endswith(marker) or len(token) < len(marker) * 2:
        _error("unterminated basic string")
    content = token[len(marker) : -len(marker)]
    if multiline:
        if content.startswith("\r\n"):
            content = content[2:]
        elif content.startswith("\n"):
            content = content[1:]
        # TOML's line-ending backslash trims the following whitespace/newline.
        content = re.sub(r"\\[ \t]*\r?\n[ \t\r\n]*", "", content)

    result: list[str] = []
    at = 0
    escapes = {
        "b": "\b",
        "t": "\t",
        "n": "\n",
        "f": "\f",
        "r": "\r",
        '"': '"',
        "\\": "\\",
    }
    while at < len(content):
        char = content[at]
        if char == "\\":
            if at + 1 >= len(content):
                _error("unterminated basic-string escape")
            selector = content[at + 1]
            if selector in escapes:
                result.append(escapes[selector])
                at += 2
                continue
            if selector in ("u", "U"):
                digits = 4 if selector == "u" else 8
                escape = content[at + 2 : at + 2 + digits]
                if (
                    len(escape) != digits
                    or re.fullmatch(rf"[0-9A-Fa-f]{{{digits}}}", escape) is None
                ):
                    _error("invalid Unicode escape")
                scalar = int(escape, 16)
                if scalar > 0x10FFFF or 0xD800 <= scalar <= 0xDFFF:
                    _error("Unicode escape is not a scalar value")
                result.append(chr(scalar))
                at += digits + 2
                continue
            _error(f"unsupported basic-string escape \\{selector}")
        if char in "\r\n":
            if not multiline:
                _error("newline in single-line basic string")
            if char == "\r" and at + 1 < len(content) and content[at + 1] == "\n":
                at += 1
            result.append("\n")
        elif char == '"' and not multiline:
            _error("unescaped quote in basic string")
        elif ord(char) < 0x20 and char != "\t" or ord(char) == 0x7F:
            _error("control character in basic string")
        elif 0xD800 <= ord(char) <= 0xDFFF:
            _error("basic string contains a non-scalar value")
        else:
            result.append(char)
        at += 1
    return "".join(result)


def _literal_string(token: str) -> str:
    multiline = token.startswith("'''")
    marker = "'''" if multiline else "'"
    if not token.endswith(marker) or len(token) < len(marker) * 2:
        _error("unterminated literal string")
    content = token[len(marker) : -len(marker)]
    if multiline:
        if content.startswith("\r\n"):
            content = content[2:]
        elif content.startswith("\n"):
            content = content[1:]
        content = content.replace("\r\n", "\n").replace("\r", "\n")
        if "'''" in content:
            _error("unescaped delimiter in multiline literal string")
    elif "'" in content:
        _error("unescaped quote in literal string")
    for char in content:
        if ord(char) < 0x20 and char not in ("\t", "\n") or ord(char) == 0x7F:
            _error("control character in literal string")
        if 0xD800 <= ord(char) <= 0xDFFF:
            _error("literal string contains a non-scalar value")
    return content


class _ParserState:
    def __init__(self) -> None:
        self.array_tables: set[int] = set()
        self.declared_tables: set[int] = set()
        self.dotted_tables: set[int] = set()
        self.inline_tables: set[int] = set()


def _seal_inline(value: Any, state: _ParserState) -> None:
    if isinstance(value, dict):
        state.inline_tables.add(id(value))
        for child in value.values():
            _seal_inline(child, state)
    elif isinstance(value, list):
        for child in value:
            _seal_inline(child, state)


def _value(token: str, state: _ParserState | None = None) -> Any:
    if state is None:
        state = _ParserState()
    token = token.strip()
    if not token:
        _error("missing value")
    if token.startswith('"'):
        return _basic_string(token)
    if token.startswith("'"):
        return _literal_string(token)
    if token.startswith("["):
        if not token.endswith("]"):
            _error("unterminated array")
        inside = token[1:-1].strip()
        if not inside:
            return []
        pieces = _split_top_level(inside, ",")
        if any(not piece for piece in pieces):
            if pieces[-1] == "" and all(pieces[:-1]):
                pieces.pop()
            else:
                _error("empty array item")
        return [_value(piece, state) for piece in pieces]
    if token.startswith("{"):
        if not token.endswith("}"):
            _error("unterminated inline table")
        result: dict[str, Any] = {}
        raw_inside = token[1:-1]
        if (
            len(_split_top_level(raw_inside, "\n")) > 1
            or len(_split_top_level(raw_inside, "\r")) > 1
        ):
            _error("top-level newline in inline table")
        inside = raw_inside.strip()
        if not inside:
            _seal_inline(result, state)
            return result
        for entry in _split_top_level(inside, ","):
            _assign(result, _assignment(entry, state), state)
        _seal_inline(result, state)
        return result
    if token == "true":
        return True
    if token == "false":
        return False
    if "_" in token and re.search(r"(?<![0-9])_|_(?![0-9])", token):
        _error(f"invalid numeric underscore placement in {token!r}")
    normalized = token.replace("_", "")
    try:
        if re.fullmatch(r"[+-]?(?:0|[1-9][0-9]*)", normalized):
            return int(normalized)
        if re.fullmatch(
            r"[+-]?(?:[0-9]+\.[0-9]+|[0-9]+[eE][+-]?[0-9]+|[0-9]+\.[0-9]*[eE][+-]?[0-9]+)",
            normalized,
        ):
            return float(normalized)
    except ValueError as err:
        _error(f"invalid numeric value: {err}")
    _error(f"unsupported TOML value {token!r}")


def _assignment(
    statement: str, state: _ParserState | None = None
) -> tuple[list[str], Any]:
    parts = _split_top_level(statement, "=")
    if len(parts) != 2 or not parts[0] or not parts[1]:
        _error(f"invalid assignment {statement!r}")
    return _key_parts(parts[0]), _value(parts[1], state)


def _table_at(
    table: dict[str, Any],
    parts: list[str],
    state: _ParserState,
    *,
    for_header: bool = False,
    mark_dotted: bool = False,
) -> dict[str, Any]:
    current = table
    for index, part in enumerate(parts):
        if id(current) in state.inline_tables:
            _error(f"cannot extend inline table {part!r}")
        existing = current.get(part)
        if existing is None:
            existing = {}
            current[part] = existing
        if isinstance(existing, list):
            if id(existing) not in state.array_tables:
                _error(f"{part!r} is already an array value")
            if mark_dotted:
                _error(f"cannot extend array of tables {part!r} with a dotted key")
            if for_header and index == len(parts) - 1:
                _error(f"{part!r} is already an array of tables")
            if not existing or not isinstance(existing[-1], dict):
                _error(f"table {part!r} has no table item")
            existing = existing[-1]
        if not isinstance(existing, dict):
            _error(f"{part!r} is already a value")
        if id(existing) in state.inline_tables:
            _error(f"cannot extend inline table {part!r}")
        if mark_dotted and id(existing) in state.declared_tables:
            _error(f"cannot extend declared table {part!r} with a dotted key")
        if mark_dotted:
            state.dotted_tables.add(id(existing))
        current = existing
    return current


def _assign(
    table: dict[str, Any],
    assignment: tuple[list[str], Any],
    state: _ParserState,
) -> None:
    parts, value = assignment
    if id(table) in state.inline_tables:
        _error("cannot extend inline table")
    parent = _table_at(table, parts[:-1], state, mark_dotted=True)
    key = parts[-1]
    if key in parent:
        _error(f"duplicate key {key!r}")
    parent[key] = value


def _fallback_loads(text: str) -> dict[str, Any]:
    root: dict[str, Any] = {}
    current = root
    state = _ParserState()
    for statement in _statements(text):
        if statement.startswith("[[") and statement.endswith("]]"):
            parts = _key_parts(statement[2:-2].strip())
            parent = _table_at(root, parts[:-1], state)
            key = parts[-1]
            existing = parent.get(key)
            if existing is None:
                existing = []
                parent[key] = existing
                state.array_tables.add(id(existing))
            if not isinstance(existing, list) or id(existing) not in state.array_tables:
                _error(f"{key!r} is already a table or value")
            item: dict[str, Any] = {}
            existing.append(item)
            current = item
        elif statement.startswith("[") and statement.endswith("]"):
            current = _table_at(
                root,
                _key_parts(statement[1:-1].strip()),
                state,
                for_header=True,
            )
            if (
                id(current) in state.declared_tables
                or id(current) in state.dotted_tables
            ):
                _error("table is declared more than once")
            state.declared_tables.add(id(current))
        else:
            _assign(current, _assignment(statement, state), state)
    return root


def loads(text: str) -> dict[str, Any]:
    if _stdlib_tomllib is not None:
        return _stdlib_tomllib.loads(text)
    if not isinstance(text, str):
        raise TypeError("TOML must be str")
    return _fallback_loads(text)


def load(handle: BinaryIO) -> dict[str, Any]:
    if _stdlib_tomllib is not None:
        return _stdlib_tomllib.load(handle)
    raw = handle.read()
    if not isinstance(raw, bytes):
        raise TypeError("TOML file must be opened in binary mode")
    try:
        return loads(raw.decode("utf-8"))
    except UnicodeDecodeError as err:
        _error(f"invalid UTF-8: {err}")
