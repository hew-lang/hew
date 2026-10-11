#!/usr/bin/env python3
"""Generate Hew Observe v1 bindings from the checked-in OpenAPI document.

Python 3.12 is the pinned generator runtime (the repository-wide minimum).
The generator intentionally supports only the small OpenAPI 3.1/JSON Schema
subset used by this contract; unsupported constructs fail closed.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SPEC_PATH = ROOT / "protocol/observe/v1/openapi.json"
OUTPUTS = {
    "rust": ROOT / "hew-observe-protocol/src/generated.rs",
    "swift": ROOT / "protocol/observe/v1/generated/ObserveProtocolV1.swift",
    "typescript": ROOT / "protocol/observe/v1/generated/observe-protocol-v1.ts",
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
    if spec.get("x-hew-schema-version") != "v1":
        raise ValueError("generator only supports the v1 contract")
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


def non_nullable_schema(schema: dict) -> dict:
    kind = schema.get("type")
    if isinstance(kind, list) and "null" in kind:
        non_null = [part for part in kind if part != "null"]
        if len(non_null) != 1:
            raise ValueError(f"unsupported nullable union: {kind}")
        return {**schema, "type": non_null[0]}
    alternatives = schema.get("anyOf")
    if alternatives is not None:
        if not isinstance(alternatives, list):
            raise ValueError(f"anyOf must be an array: {schema}")
        non_null = [part for part in alternatives if part.get("type") != "null"]
        nulls = [part for part in alternatives if part.get("type") == "null"]
        if len(non_null) == 1 and len(nulls) == 1:
            return non_null[0]
    return schema


def is_nullable(schema: dict) -> bool:
    return non_nullable_schema(schema) is not schema


def rust_type(schema: dict) -> str:
    if is_nullable(schema):
        return f"Option<{rust_type(non_nullable_schema(schema))}>"
    if (name := ref_name(schema)) is not None:
        primitives = {"UInt64Decimal": "u64", "Int64Decimal": "i64", "UInt32": "u32", "UInt16": "u16", "Int32": "i32", "Hex128Lower": "String"}
        return primitives.get(name, name)
    kind = schema.get("type")
    if kind == "string":
        return "String"
    if kind == "boolean":
        return "bool"
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
    if is_nullable(schema):
        return f"{swift_type(non_nullable_schema(schema))}?"
    if (name := ref_name(schema)) is not None:
        primitives = {"UInt64Decimal": "UInt64", "Int64Decimal": "Int64", "UInt32": "UInt32", "UInt16": "UInt16", "Int32": "Int32", "Hex128Lower": "String"}
        return primitives.get(name, name)
    kind = schema.get("type")
    if kind == "string":
        return "String"
    if kind == "boolean":
        return "Bool"
    if kind == "number":
        return "Double"
    if kind == "array":
        return f"[{swift_type(schema['items'])}]"
    raise ValueError(f"unsupported Swift schema: {schema}")


def swift_default(schema: dict) -> str:
    if is_nullable(schema):
        return "nil"
    if (name := ref_name(schema)) is not None:
        if name in {"UInt64Decimal", "Int64Decimal", "UInt32", "UInt16", "Int32"}:
            return "0"
        if name == "Hex128Lower":
            return '"00000000000000000000000000000000"'
        return f"{name}()"
    kind = schema.get("type")
    if kind == "string":
        return '""'
    if kind == "boolean":
        return "false"
    if kind == "number":
        return "0"
    if kind == "array":
        return "[]"
    raise ValueError(f"unsupported Swift default schema: {schema}")


def swift_decode_expression(schema: dict, field: str) -> str:
    if schema.get("const") == "trace_stream_mismatch":
        return f"try decodeTraceStreamMismatchLiteral(from: values, forKey: .{field})"
    if is_nullable(schema):
        inner = non_nullable_schema(schema)
        name = ref_name(inner)
        if name == "UInt64Decimal":
            return f"try decodeOptionalUInt64Decimal(from: values, forKey: .{field})"
        if name == "Int64Decimal":
            return f"try decodeOptionalInt64Decimal(from: values, forKey: .{field})"
        return f"try values.decode({swift_type(schema)}.self, forKey: .{field})"
    name = ref_name(schema)
    if name == "UInt64Decimal":
        return f"try decodeUInt64Decimal(from: values, forKey: .{field})"
    if name == "Int64Decimal":
        return f"try decodeInt64Decimal(from: values, forKey: .{field})"
    if name == "Hex128Lower":
        return f"try decodeHex128Lower(from: values, forKey: .{field})"
    return f"try values.decode({swift_type(schema)}.self, forKey: .{field})"


def swift_encode_expression(schema: dict, field: str) -> str:
    if schema.get("const") == "trace_stream_mismatch":
        return f"try encodeTraceStreamMismatchLiteral({field}, to: &values, forKey: .{field})"
    if is_nullable(schema):
        name = ref_name(non_nullable_schema(schema))
        if name in {"UInt64Decimal", "Int64Decimal"}:
            return f"try values.encode({field}.map(String.init), forKey: .{field})"
        return f"try values.encode({field}, forKey: .{field})"
    if ref_name(schema) in {"UInt64Decimal", "Int64Decimal"}:
        return f"try values.encode(String({field}), forKey: .{field})"
    if ref_name(schema) == "Hex128Lower":
        return f"try encodeHex128Lower({field}, to: &values, forKey: .{field})"
    return f"try values.encode({field}, forKey: .{field})"


def ts_type(schema: dict) -> str:
    if is_nullable(schema):
        return f"{ts_type(non_nullable_schema(schema))} | null"
    if (name := ref_name(schema)) is not None:
        primitives = {"UInt64Decimal": "bigint", "Int64Decimal": "bigint", "UInt32": "number", "UInt16": "number", "Int32": "number", "Hex128Lower": "string"}
        return primitives.get(name, name)
    kind = schema.get("type")
    if kind == "string":
        return "string"
    if kind == "boolean":
        return "boolean"
    if kind == "number":
        return "number"
    if kind == "array":
        return f"{ts_type(schema['items'])}[]"
    raise ValueError(f"unsupported TypeScript schema: {schema}")


def rust_output(spec: dict) -> str:
    actionable = actionable_trace_event_types(spec)

    def append_fields(
        output: list[str],
        model: str,
        properties: dict[str, dict],
        *,
        visibility: str,
        serde_mode: str,
    ) -> None:
        for wire_name, prop in properties.items():
            field = snake_name(model, wire_name)
            if field != wire_name and serde_mode != "none":
                output.append(f'    #[serde(rename = "{wire_name}")]')
            underlying_name = ref_name(non_nullable_schema(prop))
            module = None
            if is_nullable(prop) and underlying_name == "UInt64Decimal":
                module = "optional_uint64_decimal"
            elif is_nullable(prop) and underlying_name == "Int64Decimal":
                module = "optional_int64_decimal"
            elif not is_nullable(prop) and underlying_name == "UInt64Decimal":
                module = "uint64_decimal"
            elif not is_nullable(prop) and underlying_name == "Int64Decimal":
                module = "int64_decimal"
            elif not is_nullable(prop) and underlying_name == "Hex128Lower":
                module = "hex128_lower"
            elif prop.get("const") == "trace_stream_mismatch":
                module = "trace_stream_mismatch_literal"
            if module is not None:
                if serde_mode == "both":
                    output.append(f'    #[serde(with = "{module}")]')
                elif serde_mode == "deserialize":
                    output.append(f'    #[serde(deserialize_with = "{module}::deserialize")]')
                elif serde_mode == "serialize":
                    output.append(f'    #[serde(serialize_with = "{module}::serialize")]')
            elif is_nullable(prop) and serde_mode in {"both", "deserialize"}:
                output.append('    #[serde(deserialize_with = "deserialize_required_nullable")]')
            prefix = f"{visibility} " if visibility else ""
            output.append(f"    {prefix}{field}: {rust_type(prop)},")

    def clone_field_expression(prop: dict, field: str) -> str:
        copy_types = {
            "u64", "i64", "u32", "u16", "i32", "bool",
            "Option<u64>", "Option<i64>", "Option<u32>", "Option<u16>", "Option<i32>",
        }
        if rust_type(prop) in copy_types:
            return f"value.{field}"
        return f"value.{field}.clone()"

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
        "fn deserialize_required_nullable<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>",
        "where",
        "    D: serde::Deserializer<'de>,",
        "    T: serde::Deserialize<'de>,",
        "{",
        "    Option::<T>::deserialize(deserializer)",
        "}",
        "",
        "macro_rules! decimal_string_serde {",
        "    ($module:ident, $native:ty) => {",
        "        mod $module {",
        "            use serde::{Deserialize, Deserializer, Serializer};",
        "            pub fn serialize<S>(value: &$native, serializer: S) -> Result<S::Ok, S::Error>",
        "            where",
        "                S: Serializer,",
        "            {",
        "                serializer.serialize_str(&value.to_string())",
        "            }",
        "            pub fn deserialize<'de, D>(deserializer: D) -> Result<$native, D::Error>",
        "            where",
        "                D: Deserializer<'de>,",
        "            {",
        "                let wire = String::deserialize(deserializer)?;",
        "                let value = wire.parse::<$native>().map_err(serde::de::Error::custom)?;",
        "                if value.to_string() != wire {",
        "                    return Err(serde::de::Error::custom(",
        "                        \"expected canonical decimal string\",",
        "                    ));",
        "                }",
        "                Ok(value)",
        "            }",
        "        }",
        "    };",
        "}",
        "decimal_string_serde!(uint64_decimal, u64);",
        "decimal_string_serde!(int64_decimal, i64);",
        "",
        "macro_rules! optional_decimal_string_serde {",
        "    ($module:ident, $native:ty) => {",
        "        mod $module {",
        "            use serde::{Deserialize, Deserializer, Serializer};",
        "            #[expect(",
        "                clippy::ref_option,",
        '                reason = "serde with callback receives a field reference"',
        "            )]",
        "            pub fn serialize<S>(value: &Option<$native>, serializer: S) -> Result<S::Ok, S::Error>",
        "            where",
        "                S: Serializer,",
        "            {",
        "                match value {",
        "                    Some(value) => serializer.serialize_some(&value.to_string()),",
        "                    None => serializer.serialize_none(),",
        "                }",
        "            }",
        "            pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<$native>, D::Error>",
        "            where",
        "                D: Deserializer<'de>,",
        "            {",
        "                let Some(wire) = Option::<String>::deserialize(deserializer)? else {",
        "                    return Ok(None);",
        "                };",
        "                let value = wire.parse::<$native>().map_err(serde::de::Error::custom)?;",
        "                if value.to_string() != wire {",
        "                    return Err(serde::de::Error::custom(",
        "                        \"expected canonical decimal string\",",
        "                    ));",
        "                }",
        "                Ok(Some(value))",
        "            }",
        "        }",
        "    };",
        "}",
        "optional_decimal_string_serde!(optional_uint64_decimal, u64);",
        "",
        "mod hex128_lower {",
        "    use serde::{Deserialize, Deserializer, Serializer};",
        "    fn valid(value: &str) -> bool {",
        "        value.len() == 32",
        "            && value",
        "                .bytes()",
        "                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))",
        "    }",
        "    pub fn serialize<S>(value: &str, serializer: S) -> Result<S::Ok, S::Error>",
        "    where",
        "        S: Serializer,",
        "    {",
        "        if !valid(value) {",
        "            return Err(serde::ser::Error::custom(",
        "                \"expected 32 lowercase hexadecimal digits\",",
        "            ));",
        "        }",
        "        serializer.serialize_str(value)",
        "    }",
        "    pub fn deserialize<'de, D>(deserializer: D) -> Result<String, D::Error>",
        "    where",
        "        D: Deserializer<'de>,",
        "    {",
        "        let value = String::deserialize(deserializer)?;",
        "        if !valid(&value) {",
        "            return Err(serde::de::Error::custom(",
        "                \"expected 32 lowercase hexadecimal digits\",",
        "            ));",
        "        }",
        "        Ok(value)",
        "    }",
        "}",
        "",
        "mod trace_stream_mismatch_literal {",
        "    use serde::{Deserialize, Deserializer, Serializer};",
        '    const EXPECTED: &str = "trace_stream_mismatch";',
        "    pub fn serialize<S>(value: &str, serializer: S) -> Result<S::Ok, S::Error>",
        "    where",
        "        S: Serializer,",
        "    {",
        "        if value != EXPECTED {",
        "            return Err(serde::ser::Error::custom(\"expected trace_stream_mismatch\"));",
        "        }",
        "        serializer.serialize_str(value)",
        "    }",
        "    pub fn deserialize<'de, D>(deserializer: D) -> Result<String, D::Error>",
        "    where",
        "        D: Deserializer<'de>,",
        "    {",
        "        let value = String::deserialize(deserializer)?;",
        "        if value != EXPECTED {",
        "            return Err(serde::de::Error::custom(\"expected trace_stream_mismatch\"));",
        "        }",
        "        Ok(value)",
        "    }",
        "}",
        "",
    ]
    for model, schema in model_schemas(spec).items():
        required = set(schema.get("required", []))
        properties = schema.get("properties", {})
        if set(properties) != required:
            raise ValueError(f"{model}: v1 model fields must all be required")
        if model == "TraceSubject":
            lines.extend(["#[derive(Debug, Clone, Serialize, Deserialize)]", "struct TraceSubjectWire {"])
            append_fields(lines, model, properties, visibility="", serde_mode="both")
            lines.extend(["}", "", "#[derive(Debug, Clone, Default)]", "pub struct TraceSubject {"])
            append_fields(lines, model, properties, visibility="pub", serde_mode="none")
            lines.extend([
                "}",
                "",
                "fn validate_trace_subject(kind: &str, actor_present: bool) -> Result<(), String> {",
                "    match kind {",
                '        "actor" if !actor_present => Err("actor subject requires actor details".to_owned()),',
                '        "channel" | "resource" if actor_present => {',
                '            Err(format!("{kind} subject requires actor to be null"))',
                "        }",
                "        _ => Ok(()),",
                "    }",
                "}",
                "",
                "impl TryFrom<TraceSubjectWire> for TraceSubject {",
                "    type Error = String;",
                "",
                "    fn try_from(value: TraceSubjectWire) -> Result<Self, Self::Error> {",
                "        validate_trace_subject(&value.kind, value.actor.is_some())?;",
                "        Ok(Self {",
            ])
            for wire_name in properties:
                field = snake_name(model, wire_name)
                lines.append(f"            {field}: value.{field},")
            lines.extend(["        })", "    }", "}", "", "impl From<&TraceSubject> for TraceSubjectWire {", "    fn from(value: &TraceSubject) -> Self {", "        Self {"])
            for wire_name in properties:
                field = snake_name(model, wire_name)
                expression = clone_field_expression(properties[wire_name], field)
                lines.append(f"            {field}: {expression},")
            lines.extend([
                "        }",
                "    }",
                "}",
                "",
                "impl<'de> Deserialize<'de> for TraceSubject {",
                "    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>",
                "    where",
                "        D: serde::Deserializer<'de>,",
                "    {",
                "        let value = TraceSubjectWire::deserialize(deserializer)?;",
                "        Self::try_from(value).map_err(serde::de::Error::custom)",
                "    }",
                "}",
                "",
                "impl Serialize for TraceSubject {",
                "    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>",
                "    where",
                "        S: serde::Serializer,",
                "    {",
                "        validate_trace_subject(&self.kind, self.actor.is_some())",
                "            .map_err(serde::ser::Error::custom)?;",
                "        TraceSubjectWire::from(self).serialize(serializer)",
                "    }",
                "}",
                "",
            ])
        elif model == "TraceEvent":
            lines.extend(["#[derive(Debug, Clone, Serialize, Deserialize)]", "struct TraceEventWire {"])
            append_fields(lines, model, properties, visibility="", serde_mode="both")
            lines.extend([
                "}",
                "",
                "#[derive(Debug, Clone, Default)]",
                "pub struct TraceEvent {",
            ])
            append_fields(lines, model, properties, visibility="pub", serde_mode="none")
            lines.extend([
                "}",
                "",
                "fn validate_actor_attribution(",
                "    endpoint: &str,",
                "    attribution: &str,",
                "    actor_present: bool,",
                ") -> Result<(), String> {",
                "    match attribution {",
                '        "local_actor" if !actor_present => Err(format!(',
                '            "{endpoint}_actor is required for local_actor attribution"',
                "        )),",
                '        "external" | "remote_unknown" | "not_applicable" if actor_present => Err(format!(',
                '            "{endpoint}_actor must be null for {attribution} attribution"',
                "        )),",
                "        _ => Ok(()),",
                "    }",
                "}",
                "",
                "impl TryFrom<TraceEventWire> for TraceEvent {",
                "    type Error = String;",
                "",
                "    fn try_from(value: TraceEventWire) -> Result<Self, Self::Error> {",
                "        validate_actor_attribution(",
                '            "source",',
                "            &value.source_attribution,",
                "            value.source_actor.is_some(),",
                "        )?;",
                "        validate_actor_attribution(",
                '            "destination",',
                "            &value.destination_attribution,",
                "            value.destination_actor.is_some(),",
                "        )?;",
                "        Ok(Self {",
            ])
            for wire_name in properties:
                field = snake_name(model, wire_name)
                lines.append(f"            {field}: value.{field},")
            lines.extend(["        })", "    }", "}", "", "impl From<&TraceEvent> for TraceEventWire {", "    fn from(value: &TraceEvent) -> Self {", "        Self {"])
            for wire_name in properties:
                field = snake_name(model, wire_name)
                expression = clone_field_expression(properties[wire_name], field)
                lines.append(f"            {field}: {expression},")
            lines.extend([
                "        }",
                "    }",
                "}",
                "",
                "impl<'de> Deserialize<'de> for TraceEvent {",
                "    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>",
                "    where",
                "        D: serde::Deserializer<'de>,",
                "    {",
                "        let value = TraceEventWire::deserialize(deserializer)?;",
                "        Self::try_from(value).map_err(serde::de::Error::custom)",
                "    }",
                "}",
                "",
                "impl Serialize for TraceEvent {",
                "    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>",
                "    where",
                "        S: serde::Serializer,",
                "    {",
                "        validate_actor_attribution(",
                '            "source",',
                "            &self.source_attribution,",
                "            self.source_actor.is_some(),",
                "        )",
                "        .map_err(serde::ser::Error::custom)?;",
                "        validate_actor_attribution(",
                '            "destination",',
                "            &self.destination_attribution,",
                "            self.destination_actor.is_some(),",
                "        )",
                "        .map_err(serde::ser::Error::custom)?;",
                "        TraceEventWire::from(self).serialize(serializer)",
                "    }",
                "}",
                "",
            ])
        else:
            lines.extend(["#[derive(Debug, Clone, Default, Serialize, Deserialize)]", f"pub struct {model} {{"])
            append_fields(lines, model, properties, visibility="pub", serde_mode="both")
            lines.extend(["}", ""])
    return "\n".join(lines)


def swift_output(spec: dict) -> str:
    actionable = actionable_trace_event_types(spec)
    lines = [
        "// @generated by scripts/generate-observe-protocol.py; DO NOT EDIT.",
        "import Foundation",
        "",
        "private func decodeUInt64Decimal<Key: CodingKey>(from values: KeyedDecodingContainer<Key>, forKey key: Key) throws -> UInt64 {",
        "    let wire = try values.decode(String.self, forKey: key)",
        "    guard let value = UInt64(wire), String(value) == wire else {",
        r'        throw DecodingError.dataCorruptedError(forKey: key, in: values, debugDescription: "expected canonical UInt64 decimal string")',
        "    }",
        "    return value",
        "}",
        "",
        "private func decodeInt64Decimal<Key: CodingKey>(from values: KeyedDecodingContainer<Key>, forKey key: Key) throws -> Int64 {",
        "    let wire = try values.decode(String.self, forKey: key)",
        "    guard let value = Int64(wire), String(value) == wire else {",
        r'        throw DecodingError.dataCorruptedError(forKey: key, in: values, debugDescription: "expected canonical Int64 decimal string")',
        "    }",
        "    return value",
        "}",
        "",
        "private func decodeOptionalUInt64Decimal<Key: CodingKey>(from values: KeyedDecodingContainer<Key>, forKey key: Key) throws -> UInt64? {",
        "    let wire = try values.decode(Optional<String>.self, forKey: key)",
        "    guard let wire else { return nil }",
        "    guard let value = UInt64(wire), String(value) == wire else {",
        r'        throw DecodingError.dataCorruptedError(forKey: key, in: values, debugDescription: "expected canonical UInt64 decimal string")',
        "    }",
        "    return value",
        "}",
        "",
        "private func decodeOptionalInt64Decimal<Key: CodingKey>(from values: KeyedDecodingContainer<Key>, forKey key: Key) throws -> Int64? {",
        "    let wire = try values.decode(Optional<String>.self, forKey: key)",
        "    guard let wire else { return nil }",
        "    guard let value = Int64(wire), String(value) == wire else {",
        r'        throw DecodingError.dataCorruptedError(forKey: key, in: values, debugDescription: "expected canonical Int64 decimal string")',
        "    }",
        "    return value",
        "}",
        "",
        "private func isHex128Lower(_ value: String) -> Bool {",
        "    value.utf8.count == 32 && value.utf8.allSatisfy { byte in",
        "        (48...57).contains(byte) || (97...102).contains(byte)",
        "    }",
        "}",
        "",
        "private func decodeHex128Lower<Key: CodingKey>(from values: KeyedDecodingContainer<Key>, forKey key: Key) throws -> String {",
        "    let value = try values.decode(String.self, forKey: key)",
        "    guard isHex128Lower(value) else {",
        r'        throw DecodingError.dataCorruptedError(forKey: key, in: values, debugDescription: "expected 32 lowercase hexadecimal digits")',
        "    }",
        "    return value",
        "}",
        "",
        "private func encodeHex128Lower<Key: CodingKey>(_ value: String, to values: inout KeyedEncodingContainer<Key>, forKey key: Key) throws {",
        "    guard isHex128Lower(value) else {",
        r'        throw EncodingError.invalidValue(value, EncodingError.Context(codingPath: values.codingPath + [key], debugDescription: "expected 32 lowercase hexadecimal digits"))',
        "    }",
        "    try values.encode(value, forKey: key)",
        "}",
        "",
        "private func actorAttributionError(endpoint: String, attribution: String, actorPresent: Bool) -> String? {",
        "    if attribution == \"local_actor\" && !actorPresent {",
        "        return \"\\(endpoint)_actor is required for local_actor attribution\"",
        "    }",
        "    let knownNull = attribution == \"external\" || attribution == \"remote_unknown\" || attribution == \"not_applicable\"",
        "    if knownNull && actorPresent {",
        "        return \"\\(endpoint)_actor must be null for \\(attribution) attribution\"",
        "    }",
        "    return nil",
        "}",
        "",
        "private func traceSubjectError(kind: String, actorPresent: Bool) -> String? {",
        "    if kind == \"actor\" && !actorPresent { return \"actor subject requires actor details\" }",
        "    if (kind == \"channel\" || kind == \"resource\") && actorPresent {",
        "        return \"\\(kind) subject requires actor to be null\"",
        "    }",
        "    return nil",
        "}",
        "",
        "private func decodeTraceStreamMismatchLiteral<Key: CodingKey>(from values: KeyedDecodingContainer<Key>, forKey key: Key) throws -> String {",
        "    let value = try values.decode(String.self, forKey: key)",
        "    guard value == \"trace_stream_mismatch\" else {",
        r'        throw DecodingError.dataCorruptedError(forKey: key, in: values, debugDescription: "expected trace_stream_mismatch")',
        "    }",
        "    return value",
        "}",
        "",
        "private func encodeTraceStreamMismatchLiteral<Key: CodingKey>(_ value: String, to values: inout KeyedEncodingContainer<Key>, forKey key: Key) throws {",
        "    guard value == \"trace_stream_mismatch\" else {",
        r'        throw EncodingError.invalidValue(value, EncodingError.Context(codingPath: values.codingPath + [key], debugDescription: "expected trace_stream_mismatch"))',
        "    }",
        "    try values.encode(value, forKey: key)",
        "}",
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
        "    public func encode(to encoder: Encoder) throws {",
        "        var values = encoder.container(keyedBy: CodingKeys.self)",
        "        try values.encode(schemaVersion, forKey: .schemaVersion)",
        "        try values.encode(data, forKey: .data)",
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
            lines.append(f"        {field} = {swift_decode_expression(prop, field)}")
        if model == "TraceSubject":
            lines.extend([
                "        if let message = traceSubjectError(kind: kind, actorPresent: actor != nil) {",
                "            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath, debugDescription: message))",
                "        }",
            ])
        elif model == "TraceEvent":
            lines.extend([
                "        if let message = actorAttributionError(endpoint: \"source\", attribution: sourceAttribution, actorPresent: sourceActor != nil) {",
                "            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath, debugDescription: message))",
                "        }",
                "        if let message = actorAttributionError(endpoint: \"destination\", attribution: destinationAttribution, actorPresent: destinationActor != nil) {",
                "            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath, debugDescription: message))",
                "        }",
            ])
        lines.extend(["    }", "", "    public func encode(to encoder: Encoder) throws {", "        var values = encoder.container(keyedBy: CodingKeys.self)"])
        if model == "TraceSubject":
            lines.extend([
                "        if let message = traceSubjectError(kind: kind, actorPresent: actor != nil) {",
                "            throw EncodingError.invalidValue(kind, EncodingError.Context(codingPath: encoder.codingPath, debugDescription: message))",
                "        }",
            ])
        elif model == "TraceEvent":
            lines.extend([
                "        if let message = actorAttributionError(endpoint: \"source\", attribution: sourceAttribution, actorPresent: sourceActor != nil) {",
                "            throw EncodingError.invalidValue(sourceAttribution, EncodingError.Context(codingPath: encoder.codingPath, debugDescription: message))",
                "        }",
                "        if let message = actorAttributionError(endpoint: \"destination\", attribution: destinationAttribution, actorPresent: destinationActor != nil) {",
                "            throw EncodingError.invalidValue(destinationAttribution, EncodingError.Context(codingPath: encoder.codingPath, debugDescription: message))",
                "        }",
            ])
        for wire_name, prop in properties.items():
            field = camel(snake_name(model, wire_name))
            lines.append(f"        {swift_encode_expression(prop, field)}")
        lines.extend(["    }", "}", ""])
    return "\n".join(lines)


def ts_decoder(schema: dict, expr: str) -> str:
    if (literal := schema.get("const")) is not None:
        return f"asLiteral({expr}, {json.dumps(literal)})"
    if is_nullable(schema):
        return f"({expr} === null ? null : {ts_decoder(non_nullable_schema(schema), expr)})"
    if (name := ref_name(schema)) is not None:
        integer_bounds = {
            "UInt64Decimal": ("0n", "18446744073709551615n", "asBoundedBigInt"),
            "Int64Decimal": ("-9223372036854775808n", "9223372036854775807n", "asBoundedBigInt"),
            "UInt32": ("0", "4294967295", "asBoundedNumber"),
            "UInt16": ("0", "65535", "asBoundedNumber"),
            "Int32": ("-2147483648", "2147483647", "asBoundedNumber"),
        }
        if name in integer_bounds:
            minimum, maximum, decoder = integer_bounds[name]
            return f"{decoder}({expr}, {minimum}, {maximum})"
        if name == "Hex128Lower":
            return f"asHex128Lower({expr})"
        return f"decode{name}({expr})"
    kind = schema.get("type")
    if kind == "string":
        return f"asString({expr})"
    if kind == "boolean":
        return f"asBoolean({expr})"
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
            "function asRecord(value: unknown): Record<string, unknown> { if (typeof value !== 'object' || value === null || Array.isArray(value)) throw new TypeError('expected object'); return value as Record<string, unknown>; }",
            "function asArray(value: unknown): unknown[] { if (!Array.isArray(value)) throw new TypeError('expected array'); return value; }",
            "function asString(value: unknown): string { if (typeof value !== 'string') throw new TypeError('expected string'); return value; }",
            "function asLiteral(value: unknown, expected: string): string { const text = asString(value); if (text !== expected) throw new TypeError(`expected ${expected}`); return text; }",
            "function asHex128Lower(value: unknown): string { const text = asString(value); if (!/^[0-9a-f]{32}$/.test(text)) throw new TypeError('expected 32 lowercase hexadecimal digits'); return text; }",
            "function asBoolean(value: unknown): boolean { if (typeof value !== 'boolean') throw new TypeError('expected boolean'); return value; }",
            "function asBoundedNumber(value: unknown, minimum: number, maximum: number): number { if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < minimum || value > maximum) throw new TypeError(`expected integer in [${minimum}, ${maximum}]`); return value === 0 ? 0 : value; }",
            "function asBoundedBigInt(value: unknown, minimum: bigint, maximum: bigint): bigint { if (typeof value !== 'string' || !/^-?(0|[1-9][0-9]*)$/.test(value)) throw new TypeError('expected canonical decimal string'); const number = BigInt(value); if (number < minimum || number > maximum || number.toString() !== value) throw new TypeError(`expected integer in [${minimum}, ${maximum}]`); return number; }",
            "function asDouble(value: unknown): number { if (typeof value !== 'number' || !Number.isFinite(value)) throw new TypeError('expected finite number'); return value; }",
            "function validateActorAttribution(endpoint: string, attribution: string, actor: ActorIdentity | null): void { const knownNull = attribution === 'external' || attribution === 'remote_unknown' || attribution === 'not_applicable'; if (attribution === 'local_actor' && actor === null) throw new TypeError(`${endpoint}_actor is required for local_actor attribution`); if (knownNull && actor !== null) throw new TypeError(`${endpoint}_actor must be null for ${attribution} attribution`); }",
            "function validateTraceSubject(kind: string, actor: ActorIdentity | null): void { if (kind === 'actor' && actor === null) throw new TypeError('actor subject requires actor details'); if ((kind === 'channel' || kind === 'resource') && actor !== null) throw new TypeError(`${kind} subject requires actor to be null`); }",
            "",
        ]
    )
    for model, schema in models.items():
        lines.extend([f"export function decode{model}(value: unknown): {model} {{", "  const input = asRecord(value);", "  const output = { ...input,"])
        for wire_name, prop in schema["properties"].items():
            lines.append(f"    {wire_name}: {ts_decoder(prop, f'input.{wire_name}')},")
        lines.append(f"  }} as {model};")
        if model == "TraceSubject":
            lines.append("  validateTraceSubject(output.kind, output.actor);")
        elif model == "TraceEvent":
            lines.extend([
                "  validateActorAttribution('source', output.source_attribution, output.source_actor);",
                "  validateActorAttribution('destination', output.destination_attribution, output.destination_actor);",
            ])
        lines.extend(["  return output;", "}", ""])

    envelope_payloads = {
        "Metrics": ("Metrics", False), "Memory": ("Memory", False), "Actors": ("ActorInfo", True),
        "History": ("HistoryEntry", True), "ClusterMembers": ("ClusterMember", True),
        "Connections": ("ConnectionInfo", True), "Routing": ("RoutingSnapshot", False),
        "Traces": ("TraceBatch", False), "TraceRequestError": ("TraceRequestError", False),
        "TraceStreamMismatch": ("TraceStreamMismatch", False),
        "Supervisors": ("SupervisorRow", True), "Crashes": ("CrashEntry", True),
    }
    for endpoint, (model, is_array) in envelope_payloads.items():
        data_type = f"{model}[]" if is_array else model
        decoder = f"asArray(input.data).map((item) => decode{model}(item))" if is_array else f"decode{model}(input.data)"
        lines.extend([
            f"export function decode{endpoint}Envelope(text: string): ObserveEnvelope<{data_type}> {{",
            "  const input = asRecord(JSON.parse(text));",
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
