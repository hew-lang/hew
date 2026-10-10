#!/usr/bin/env python3
"""Generate Hew Observe v0.5 bindings from the checked-in OpenAPI document.

Python 3.12 is the pinned generator runtime (the repository-wide minimum).
The generator intentionally supports only the small OpenAPI 3.1/JSON Schema
subset used by this contract; unsupported constructs fail closed.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SPEC_PATH = ROOT / "protocol/observe/v0.5/openapi.json"
OUTPUTS = {
    "rust": ROOT / "hew-observe-protocol/src/generated.rs",
    "swift": ROOT / "protocol/observe/v0.5/generated/ObserveProtocolV05.swift",
    "typescript": ROOT / "protocol/observe/v0.5/generated/observe-protocol-v0.5.ts",
}

HISTORY_NAMES = {
    "ts": "tasks_spawned",
    "tc": "tasks_completed",
    "st": "steals",
    "ms": "messages_sent",
    "mr": "messages_received",
    "aw": "active_workers",
    "ac": "alloc_count",
    "dc": "dealloc_count",
    "ba": "bytes_allocated",
    "bf": "bytes_freed",
    "bl": "bytes_live",
    "pb": "peak_bytes_live",
    "tbr": "tcp_bytes_read",
    "tbw": "tcp_bytes_written",
    "tac": "tcp_accept_count",
    "tcc": "tcp_connect_count",
    "tec": "tcp_error_count",
}


def load_spec() -> dict:
    def reject_duplicate_keys(pairs: list[tuple[str, object]]) -> dict:
        result: dict = {}
        for key, value in pairs:
            if key in result:
                raise ValueError(f"duplicate JSON object key in OpenAPI contract: {key}")
            result[key] = value
        return result

    with SPEC_PATH.open(encoding="utf-8") as handle:
        spec = json.load(handle, object_pairs_hook=reject_duplicate_keys)
    if spec.get("openapi") != "3.1.0":
        raise ValueError("observe contract must remain OpenAPI 3.1.0")
    if spec.get("x-hew-schema-version") != "v0.5":
        raise ValueError("generator only supports the v0.5 contract")
    validate_local_references(spec)
    expected_json_paths = {
        "/api/metrics", "/api/memory", "/api/actors", "/api/metrics/history",
        "/api/cluster/members", "/api/connections", "/api/routing/table",
        "/api/traces", "/api/supervisors", "/api/crashes",
    }
    expected_paths = expected_json_paths | {
        "/", "/dashboard.js", "/api/observe/scrape",
        "/debug/pprof/heap", "/debug/pprof/profile",
    }
    actual_paths = set(spec.get("paths", {}))
    if actual_paths != expected_paths:
        missing = sorted(expected_paths - actual_paths)
        unexpected = sorted(actual_paths - expected_paths)
        raise ValueError(
            f"observe endpoint inventory drifted; missing={missing}, unexpected={unexpected}"
        )
    for path in expected_json_paths:
        path_item_ref = spec["paths"][path].get("$ref", "")
        path_item_name = path_item_ref.removeprefix("#/components/pathItems/")
        path_item = spec["components"]["pathItems"].get(path_item_name, {})
        response_ref = path_item.get("get", {}).get("responses", {}).get("200", {}).get("$ref", "")
        response_name = response_ref.removeprefix("#/components/responses/")
        response = spec["components"]["responses"].get(response_name, {})
        if "X-Hew-Schema-Version" not in response.get("headers", {}):
            raise ValueError(f"{path}: JSON response is missing schema-version header")
        schema_ref = (
            response.get("content", {})
            .get("application/json", {})
            .get("schema", {})
            .get("$ref", "")
        )
        if not schema_ref.startswith("#/components/schemas/"):
            raise ValueError(f"{path}: JSON response is missing an envelope schema")
    return spec


def validate_local_references(document: dict) -> None:
    def resolve(pointer: str) -> object:
        if not pointer.startswith("#/"):
            raise ValueError(f"only local OpenAPI references are supported: {pointer}")
        value: object = document
        for raw_part in pointer[2:].split("/"):
            part = raw_part.replace("~1", "/").replace("~0", "~")
            if not isinstance(value, dict) or part not in value:
                raise ValueError(f"unresolved OpenAPI reference: {pointer}")
            value = value[part]
        return value

    def walk(value: object) -> None:
        if isinstance(value, dict):
            if "$ref" in value:
                resolve(value["$ref"])
            for child in value.values():
                walk(child)
        elif isinstance(value, list):
            for child in value:
                walk(child)

    walk(document)


def model_schemas(spec: dict) -> dict[str, dict]:
    return {
        name: schema
        for name, schema in spec["components"]["schemas"].items()
        if schema.get("x-hew-model") is True
    }


def actionable_trace_event_types(spec: dict) -> list[str]:
    event_type = spec["components"]["schemas"]["TraceEvent"]["properties"]["event_type"]
    values = event_type.get("x-hew-actionable-values")
    known_values = set(event_type.get("x-known-values", []))
    if not isinstance(values, list) or not values:
        raise ValueError("TraceEvent.event_type must define x-hew-actionable-values")
    if any(not isinstance(value, str) or value not in known_values for value in values):
        raise ValueError("actionable trace event types must be known string values")
    return values


def ref_name(schema: dict) -> str | None:
    ref = schema.get("$ref")
    if ref is None:
        return None
    prefix = "#/components/schemas/"
    if not ref.startswith(prefix):
        raise ValueError(f"unsupported reference: {ref}")
    return ref.removeprefix(prefix)


def is_nullable(schema: dict) -> bool:
    return isinstance(schema.get("type"), list) and "null" in schema["type"]


def rust_type(schema: dict) -> str:
    if (name := ref_name(schema)) is not None:
        primitives = {"UInt64": "u64", "Int64": "i64", "UInt32": "u32", "UInt16": "u16", "Int32": "i32"}
        return primitives.get(name, name)
    kind = schema.get("type")
    if isinstance(kind, list):
        non_null = [part for part in kind if part != "null"]
        if len(non_null) != 1:
            raise ValueError(f"unsupported union: {kind}")
        return f"Option<{rust_type({**schema, 'type': non_null[0]})}>"
    if kind == "string":
        return "String"
    if kind == "number":
        return "f64"
    if kind == "array":
        return f"Vec<{rust_type(schema['items'])}>"
    raise ValueError(f"unsupported Rust schema: {schema}")


def snake_name(model: str, wire_name: str) -> str:
    if model == "HistoryEntry":
        return HISTORY_NAMES.get(wire_name, wire_name)
    return wire_name


def camel(name: str) -> str:
    first, *rest = name.split("_")
    return first + "".join(part[:1].upper() + part[1:] for part in rest)


def swift_type(schema: dict) -> str:
    if (name := ref_name(schema)) is not None:
        primitives = {"UInt64": "UInt64", "Int64": "Int64", "UInt32": "UInt32", "UInt16": "UInt16", "Int32": "Int32"}
        return primitives.get(name, name)
    kind = schema.get("type")
    if isinstance(kind, list):
        non_null = [part for part in kind if part != "null"]
        return f"{swift_type({**schema, 'type': non_null[0]})}?"
    if kind == "string":
        return "String"
    if kind == "number":
        return "Double"
    if kind == "array":
        return f"[{swift_type(schema['items'])}]"
    raise ValueError(f"unsupported Swift schema: {schema}")


def swift_default(schema: dict) -> str:
    if is_nullable(schema):
        return "nil"
    if (name := ref_name(schema)) is not None:
        if name in {"UInt64", "Int64", "UInt32", "UInt16", "Int32"}:
            return "0"
        return f"{name}()"
    kind = schema.get("type")
    if kind == "string":
        return '""'
    if kind == "number":
        return "0"
    if kind == "array":
        return "[]"
    raise ValueError(f"unsupported Swift default schema: {schema}")


def ts_type(schema: dict) -> str:
    if (name := ref_name(schema)) is not None:
        primitives = {"UInt64": "bigint", "Int64": "bigint", "UInt32": "number", "UInt16": "number", "Int32": "number"}
        return primitives.get(name, name)
    kind = schema.get("type")
    if isinstance(kind, list):
        non_null = [part for part in kind if part != "null"]
        return f"{ts_type({**schema, 'type': non_null[0]})} | null"
    if kind == "string":
        return "string"
    if kind == "number":
        return "number"
    if kind == "array":
        return f"{ts_type(schema['items'])}[]"
    raise ValueError(f"unsupported TypeScript schema: {schema}")


def rust_output(spec: dict) -> str:
    actionable = actionable_trace_event_types(spec)
    lines = [
        "// @generated by scripts/generate-observe-protocol.py; DO NOT EDIT.",
        "use serde::{Deserialize, Serialize};",
        "",
        f'pub const OBSERVE_SCHEMA_VERSION: &str = "{spec["x-hew-schema-version"]}";',
        "pub const ACTIONABLE_TRACE_EVENT_TYPES: &[&str] = &[",
        *[f'    "{value}",' for value in actionable],
        "];",
        "",
        "#[derive(Debug, Clone, Serialize, Deserialize)]",
        "pub struct Envelope<T> {",
        "    pub schema_version: String,",
        "    pub data: T,",
        "}",
        "",
        "fn deserialize_required_nullable<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>",
        "where",
        "    D: serde::Deserializer<'de>,",
        "{",
        "    Option::<String>::deserialize(deserializer)",
        "}",
        "",
    ]
    for model, schema in model_schemas(spec).items():
        required = set(schema.get("required", []))
        properties = schema.get("properties", {})
        if set(properties) != required:
            raise ValueError(f"{model}: v0.5 model fields must all be required")
        lines.extend(["#[derive(Debug, Clone, Default, Serialize, Deserialize)]", f"pub struct {model} {{"])
        for wire_name, prop in properties.items():
            field = snake_name(model, wire_name)
            if field != wire_name:
                lines.append(f'    #[serde(rename = "{wire_name}")]')
            if is_nullable(prop):
                lines.append('    #[serde(deserialize_with = "deserialize_required_nullable")]')
            lines.append(f"    pub {field}: {rust_type(prop)},")
        lines.extend(["}", ""])
    return "\n".join(lines)


def swift_output(spec: dict) -> str:
    actionable = actionable_trace_event_types(spec)
    lines = [
        "// @generated by scripts/generate-observe-protocol.py; DO NOT EDIT.",
        "import Foundation",
        "",
        f'public let observeSchemaVersion = "{spec["x-hew-schema-version"]}"',
        "public let actionableTraceEventTypes: Set<String> = [",
        *[f'    "{value}",' for value in actionable],
        "]",
        "",
        "public struct ObserveEnvelope<Data: Codable & Sendable>: Codable, Sendable {",
        "    public let schemaVersion: String",
        "    public let data: Data",
        "    enum CodingKeys: String, CodingKey { case schemaVersion = \"schema_version\", data }",
        "    public init(from decoder: Decoder) throws {",
        "        let values = try decoder.container(keyedBy: CodingKeys.self)",
        "        schemaVersion = try values.decode(String.self, forKey: .schemaVersion)",
        "        guard schemaVersion == observeSchemaVersion else {",
        r'            throw DecodingError.dataCorruptedError(forKey: .schemaVersion, in: values, debugDescription: "unsupported Hew Observe schema version \(schemaVersion)")',
        "        }",
        "        data = try values.decode(Data.self, forKey: .data)",
        "    }",
        "}",
        "",
    ]
    for model, schema in model_schemas(spec).items():
        properties = schema["properties"]
        lines.append(f"public struct {model}: Codable, Sendable {{")
        for wire_name, prop in properties.items():
            field = camel(snake_name(model, wire_name))
            lines.append(f"    public let {field}: {swift_type(prop)}")
        lines.append("")
        lines.append("    enum CodingKeys: String, CodingKey {")
        for wire_name in properties:
            field = camel(snake_name(model, wire_name))
            suffix = f' = "{wire_name}"' if field != wire_name else ""
            lines.append(f"        case {field}{suffix}")
        lines.extend(["    }", "", "    public init("])
        entries = list(properties.items())
        for index, (wire_name, prop) in enumerate(entries):
            field = camel(snake_name(model, wire_name))
            comma = "," if index + 1 < len(entries) else ""
            lines.append(f"        {field}: {swift_type(prop)} = {swift_default(prop)}{comma}")
        lines.append("    ) {")
        for wire_name in properties:
            field = camel(snake_name(model, wire_name))
            lines.append(f"        self.{field} = {field}")
        lines.extend(["    }", "", "    public init(from decoder: Decoder) throws {", "        let values = try decoder.container(keyedBy: CodingKeys.self)"])
        for wire_name, prop in properties.items():
            field = camel(snake_name(model, wire_name))
            lines.append(f"        {field} = try values.decode({swift_type(prop)}.self, forKey: .{field})")
        lines.extend(["    }", "}", ""])
    return "\n".join(lines)


def ts_decoder(schema: dict, expr: str) -> str:
    if (name := ref_name(schema)) is not None:
        integer_bounds = {
            "UInt64": ("0n", "18446744073709551615n", "asBoundedBigInt"),
            "Int64": ("-9223372036854775808n", "9223372036854775807n", "asBoundedBigInt"),
            "UInt32": ("0", "4294967295", "asBoundedNumber"),
            "UInt16": ("0", "65535", "asBoundedNumber"),
            "Int32": ("-2147483648", "2147483647", "asBoundedNumber"),
        }
        if name in integer_bounds:
            minimum, maximum, decoder = integer_bounds[name]
            return f"{decoder}({expr}, {minimum}, {maximum})"
        return f"decode{name}({expr})"
    kind = schema.get("type")
    if isinstance(kind, list):
        return f"({expr} === null ? null : asString({expr}))"
    if kind == "string":
        return f"asString({expr})"
    if kind == "number":
        return f"asDouble({expr})"
    if kind == "array":
        return f"asArray({expr}).map((item) => {ts_decoder(schema['items'], 'item')})"
    raise ValueError(f"unsupported TypeScript decoder schema: {schema}")


def ts_output(spec: dict) -> str:
    models = model_schemas(spec)
    actionable = actionable_trace_event_types(spec)
    lines = [
        "// @generated by scripts/generate-observe-protocol.py; DO NOT EDIT.",
        f'export const OBSERVE_SCHEMA_VERSION = "{spec["x-hew-schema-version"]}" as const;',
        "export const ACTIONABLE_TRACE_EVENT_TYPES = new Set([",
        *[f'  "{value}",' for value in actionable],
        "]);",
        "",
        "export interface ObserveEnvelope<T> { schema_version: typeof OBSERVE_SCHEMA_VERSION; data: T; [key: string]: unknown }",
        "",
    ]
    for model, schema in models.items():
        lines.append(f"export interface {model} {{")
        for wire_name, prop in schema["properties"].items():
            lines.append(f"  {wire_name}: {ts_type(prop)};")
        lines.extend(["  [key: string]: unknown;", "}", ""])
    lines.extend(
        [
            "const INTEGER_MARKER = \"__hew_observe_integer__\";",
            "",
            "/** Parse JSON without first rounding 64-bit integer tokens through Number. */",
            "export function parseLosslessJson(text: string): unknown {",
            "  let output = \"\"; let index = 0; let inString = false; let escaped = false;",
            "  while (index < text.length) {",
            "    const char = text[index];",
            r'''    if (inString) { output += char; if (escaped) escaped = false; else if (char === "\\") escaped = true; else if (char === '"') inString = false; index += 1; continue; }''',
            "    if (char === '\"') { inString = true; output += char; index += 1; continue; }",
            "    if (char === '-' || (char >= '0' && char <= '9')) {",
            "      const start = index; index += 1; while (index < text.length && /[0-9eE+.-]/.test(text[index])) index += 1;",
            "      const token = text.slice(start, index);",
            "      if (!/[.eE]/.test(token) && !/^-?(0|[1-9][0-9]*)$/.test(token)) throw new SyntaxError(`invalid JSON integer ${token}`);",
            "      output += /[.eE]/.test(token) ? token : `{\"${INTEGER_MARKER}\":${JSON.stringify(token)}}`; continue;",
            "    }",
            "    output += char; index += 1;",
            "  }",
            "  return JSON.parse(output);",
            "}",
            "",
            "function asRecord(value: unknown): Record<string, unknown> { if (typeof value !== 'object' || value === null || Array.isArray(value)) throw new TypeError('expected object'); return value as Record<string, unknown>; }",
            "function asArray(value: unknown): unknown[] { if (!Array.isArray(value)) throw new TypeError('expected array'); return value; }",
            "function integerLexeme(value: unknown): string | number { if (typeof value === 'number') return value; if (typeof value === 'string' && /^-?(0|[1-9][0-9]*)$/.test(value)) return value; const record = asRecord(value); const raw = record[INTEGER_MARKER]; if (typeof raw !== 'string') throw new TypeError('expected integer'); return raw; }",
            "function asString(value: unknown): string { if (typeof value !== 'string') throw new TypeError('expected string'); return value; }",
            "function asBoundedNumber(value: unknown, minimum: number, maximum: number): number { const number = Number(integerLexeme(value)); if (!Number.isSafeInteger(number) || number < minimum || number > maximum) throw new TypeError(`expected integer in [${minimum}, ${maximum}]`); return number === 0 ? 0 : number; }",
            "function asBoundedBigInt(value: unknown, minimum: bigint, maximum: bigint): bigint { const number = BigInt(integerLexeme(value)); if (number < minimum || number > maximum) throw new TypeError(`expected integer in [${minimum}, ${maximum}]`); return number; }",
            "function asDouble(value: unknown): number { const number = typeof value === 'number' ? value : Number(integerLexeme(value)); if (!Number.isFinite(number)) throw new TypeError('expected finite number'); return number; }",
            "",
        ]
    )
    for model, schema in models.items():
        lines.extend([f"export function decode{model}(value: unknown): {model} {{", "  const input = asRecord(value);", "  return { ...input,"])
        for wire_name, prop in schema["properties"].items():
            lines.append(f"    {wire_name}: {ts_decoder(prop, f'input.{wire_name}')},")
        lines.extend([f"  }} as {model};", "}", ""])

    envelope_payloads = {
        "Metrics": ("Metrics", False), "Memory": ("Memory", False), "Actors": ("ActorInfo", True),
        "History": ("HistoryEntry", True), "ClusterMembers": ("ClusterMember", True),
        "Connections": ("ConnectionInfo", True), "Routing": ("RoutingSnapshot", False),
        "Traces": ("TraceEvent", True), "Supervisors": ("SupervisorRow", True), "Crashes": ("CrashEntry", True),
    }
    for endpoint, (model, is_array) in envelope_payloads.items():
        data_type = f"{model}[]" if is_array else model
        decoder = f"asArray(input.data).map((item) => decode{model}(item))" if is_array else f"decode{model}(input.data)"
        lines.extend([
            f"export function decode{endpoint}Envelope(text: string): ObserveEnvelope<{data_type}> {{",
            "  const input = asRecord(parseLosslessJson(text));",
            "  if (input.schema_version !== OBSERVE_SCHEMA_VERSION) throw new TypeError(`unsupported Hew Observe schema version ${String(input.schema_version)}`);",
            f"  return {{ ...input, schema_version: OBSERVE_SCHEMA_VERSION, data: {decoder} }} as ObserveEnvelope<{data_type}>;",
            "}", "",
        ])
    return "\n".join(lines)


def write_or_check(outputs: dict[Path, str], check: bool) -> int:
    stale: list[Path] = []
    for path, content in outputs.items():
        rendered = content.rstrip() + "\n"
        if check:
            if not path.exists() or path.read_text(encoding="utf-8") != rendered:
                stale.append(path)
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(rendered, encoding="utf-8")
    if stale:
        print("generated Observe bindings are stale:", file=sys.stderr)
        for path in stale:
            print(f"  {path.relative_to(ROOT)}", file=sys.stderr)
        return 1
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true", help="fail if checked-in outputs differ")
    args = parser.parse_args()
    spec = load_spec()
    outputs = {
        OUTPUTS["rust"]: rust_output(spec),
        OUTPUTS["swift"]: swift_output(spec),
        OUTPUTS["typescript"]: ts_output(spec),
    }
    return write_or_check(outputs, args.check)


if __name__ == "__main__":
    raise SystemExit(main())
