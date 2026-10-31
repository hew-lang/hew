//! Lossless native Encoding Value text operations for the sandbox bridge.

use hew_codec::managed_tree;
use serde_json::{json, Value as Json};

fn retained_capacity(value: &Json, len: usize) -> Result<usize, String> {
    match value.as_str() {
        None if value.is_null() => Ok(len),
        Some(text) => text
            .parse::<usize>()
            .ok()
            .filter(|capacity| *capacity >= len)
            .ok_or_else(|| "invalid retained encoding capacity".into()),
        _ => Err("invalid retained encoding capacity".into()),
    }
}

fn retained_text(value: &Json, capacity: &Json) -> Result<String, String> {
    let text = value.as_str().ok_or("encoding string is missing")?;
    let mut result = String::new();
    result
        .try_reserve_exact(retained_capacity(capacity, text.len())?)
        .map_err(|_| "encoding string capacity overflow")?;
    result.push_str(text);
    Ok(result)
}

fn retained_array<T>(values: Vec<T>, capacity: &Json) -> Result<Vec<T>, String> {
    let mut result = Vec::new();
    result
        .try_reserve_exact(retained_capacity(capacity, values.len())?)
        .map_err(|_| "encoding array capacity overflow")?;
    result.extend(values);
    Ok(result)
}

fn json_capacities(value: &Json) -> Json {
    match value {
        Json::String(text) => json!(text.capacity().to_string()),
        Json::Array(values) => json!([
            values.capacity().to_string(),
            values.iter().map(json_capacities).collect::<Vec<_>>()
        ]),
        Json::Object(values) => json!(values
            .iter()
            .map(|(key, value)| json!([key.capacity().to_string(), json_capacities(value)]))
            .collect::<Vec<_>>()),
        _ => Json::Null,
    }
}

fn json_response(value: Json) -> Result<Json, String> {
    let bytes = managed_tree::json_payload_bytes(&value, false)
        .ok_or("encoding payload capacity overflow")?;
    let copy_bytes =
        managed_tree::json_payload_bytes(&value, true).ok_or("encoding copy capacity overflow")?;
    Ok(
        json!({"managed": {"bytes": bytes.to_string(), "copy_bytes": copy_bytes.to_string(), "shared_copy": managed_tree::JSON_LAYOUT.shared_copy, "capacities": json_capacities(&value)}, "value": json_value_to_bridge(value)}),
    )
}

fn yaml_capacities(value: &serde_yaml::Value) -> Json {
    use serde_yaml::Value;
    match value {
        Value::String(text) => json!(text.capacity().to_string()),
        Value::Sequence(values) => json!([
            values.capacity().to_string(),
            values.iter().map(yaml_capacities).collect::<Vec<_>>()
        ]),
        Value::Mapping(values) => json!([
            values.capacity().to_string(),
            values
                .iter()
                .map(|(key, value)| json!([yaml_capacities(key), yaml_capacities(value)]))
                .collect::<Vec<_>>()
        ]),
        Value::Tagged(value) => yaml_capacities(&value.value),
        _ => Json::Null,
    }
}

fn yaml_response(mut value: serde_yaml::Value) -> Result<Json, String> {
    managed_tree::normalize_yaml_tags(&mut value);
    let bytes =
        managed_tree::yaml_payload_bytes(&value).ok_or("encoding payload capacity overflow")?;
    Ok(
        json!({"managed": {"bytes": bytes.to_string(), "copy_bytes": bytes.to_string(), "shared_copy": managed_tree::YAML_LAYOUT.shared_copy, "capacities": yaml_capacities(&value)}, "value": yaml_value_to_bridge(value)}),
    )
}

fn toml_capacities(value: &toml::Value) -> Json {
    use toml::Value;
    match value {
        Value::String(text) => json!(text.capacity().to_string()),
        Value::Array(values) => json!([
            values.capacity().to_string(),
            values.iter().map(toml_capacities).collect::<Vec<_>>()
        ]),
        Value::Table(values) => json!(values
            .iter()
            .map(|(key, value)| json!([key.capacity().to_string(), toml_capacities(value)]))
            .collect::<Vec<_>>()),
        _ => Json::Null,
    }
}

fn toml_response(value: toml::Value) -> Result<Json, String> {
    let bytes = managed_tree::toml_payload_bytes(&value, false)
        .ok_or("encoding payload capacity overflow")?;
    let copy_bytes =
        managed_tree::toml_payload_bytes(&value, true).ok_or("encoding copy capacity overflow")?;
    Ok(
        json!({"managed": {"bytes": bytes.to_string(), "copy_bytes": copy_bytes.to_string(), "shared_copy": managed_tree::TOML_LAYOUT.shared_copy, "capacities": toml_capacities(&value)}, "value": toml_value_to_bridge(value)}),
    )
}

// JSON Value uses the same serde_json policy as the native JSON runtime, rather
// than the strict typed wire codec. Positional tags preserve full-width integers
// and distinguish floating-point numbers while crossing the JavaScript bridge.
fn json_value_to_bridge(value: Json) -> Json {
    match value {
        Json::Null => json!(["null"]),
        Json::Bool(value) => json!(["bool", value]),
        Json::Number(value) => json!([
            if value.is_i64() || value.is_u64() {
                "integer"
            } else {
                "float"
            },
            value.to_string()
        ]),
        Json::String(value) => json!(["string", value]),
        Json::Array(values) => json!([
            "array",
            values
                .into_iter()
                .map(json_value_to_bridge)
                .collect::<Vec<_>>()
        ]),
        Json::Object(values) => json!([
            "object",
            values
                .into_iter()
                .map(|(key, value)| json!([key, json_value_to_bridge(value)]))
                .collect::<Vec<_>>()
        ]),
    }
}

fn json_value_from_bridge(value: &Json, capacities: &Json) -> Result<Json, String> {
    let invalid = || "invalid JSON value bridge payload".to_owned();
    let tag = value[0].as_str().ok_or_else(invalid)?;
    let payload = &value[1];
    Ok(match tag {
        "null" => Json::Null,
        "bool" => Json::Bool(payload.as_bool().ok_or_else(invalid)?),
        "string" => Json::String(retained_text(payload, capacities)?),
        "integer" => Json::Number(
            serde_json::from_str(payload.as_str().ok_or_else(invalid)?)
                .map_err(|error| error.to_string())?,
        ),
        "float" => {
            let number: f64 = payload
                .as_str()
                .ok_or_else(invalid)?
                .parse()
                .map_err(|error: std::num::ParseFloatError| error.to_string())?;
            Json::Number(serde_json::Number::from_f64(number).ok_or_else(invalid)?)
        }
        "array" => Json::Array(retained_array(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .enumerate()
                .map(|(index, value)| json_value_from_bridge(value, &capacities[1][index]))
                .collect::<Result<_, _>>()?,
            &capacities[0],
        )?),
        "object" => Json::Object(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .enumerate()
                .map(|(index, pair)| {
                    Ok((
                        retained_text(&pair[0], &capacities[index][0])?,
                        json_value_from_bridge(&pair[1], &capacities[index][1])?,
                    ))
                })
                .collect::<Result<_, String>>()?,
        ),
        _ => return Err(invalid()),
    })
}

pub(crate) fn execute(request: &Json) -> Result<Json, String> {
    let format = request["format"]
        .as_str()
        .ok_or("encoding format is missing")?;
    match request["operation"].as_str() {
        Some("parse") => {
            let text = request["text"]
                .as_str()
                .ok_or("encoding parse input is missing")?;
            let parsed = match format {
                "Json" => serde_json::from_str(text)
                    .map_err(|error| error.to_string())
                    .and_then(json_response),
                "Yaml" => hew_codec::yaml_value::parse(text).and_then(yaml_response),
                "Toml" => text
                    .parse::<toml::Table>()
                    .map_err(|error| error.to_string())
                    .and_then(|table| toml_response(toml::Value::Table(table))),
                _ => return Err("unknown encoding format".into()),
            };
            Ok(match parsed {
                Ok(value) => value,
                Err(error) => json!({"error": error}),
            })
        }
        Some("stringify") => {
            if request["value"].is_null() {
                return Ok(json!({"error": match format {
                    "Json" => "json stringify failed: invalid value handle",
                    "Yaml" => "yaml stringify failed: invalid value handle",
                    "Toml" => "toml: cannot serialize an invalid value",
                    _ => return Err("unknown encoding format".into()),
                }}));
            }
            let value = &request["value"];
            let encoded = match format {
                "Json" => serde_json::to_string(&json_value_from_bridge(value, &Json::Null)?)
                    .map_err(|error| format!("json stringify failed: {error}")),
                "Yaml" => serde_yaml::to_string(&yaml_value_from_bridge(value, &Json::Null)?)
                    .map_err(|error| format!("yaml stringify failed: {error}")),
                "Toml" => toml::to_string(&toml_value_from_bridge(value, &Json::Null)?)
                    .map_err(|error| format!("toml: {error}")),
                _ => return Err("unknown encoding format".into()),
            };
            Ok(match encoded {
                Ok(text) => json!({"text": text}),
                Err(error) => json!({"error": error}),
            })
        }
        Some("retain" | "clone" | "array_push" | "object_set") => mutate(request, format),
        _ => Err("unknown encoding value operation".into()),
    }
}

/// Rehydrate the retained capacities authored by the previous native bridge
/// response, then use the provider's real clone and mutation policies. The
/// browser retains the returned tree identity; these temporary native values
/// do not establish a second managed-storage authority.
fn mutate(request: &Json, format: &str) -> Result<Json, String> {
    let operation = request["operation"]
        .as_str()
        .ok_or("encoding operation is missing")?;
    if request["value"].is_null() {
        return Ok(json!({"value": null}));
    }
    let capacities = &request["capacities"];
    let child_capacities = &request["child_capacities"];
    let key = || request["key"].as_str().ok_or("encoding map key is missing");
    match format {
        "Json" => {
            let mut value = json_value_from_bridge(&request["value"], capacities)?;
            if operation == "clone" {
                value = value.clone();
            }
            if !request["child"].is_null() {
                let child = json_value_from_bridge(&request["child"], child_capacities)?;
                match (&mut value, operation) {
                    (Json::Array(values), "array_push") => values.push(child),
                    (Json::Object(values), "object_set") => {
                        values.insert(key()?.to_owned(), child);
                    }
                    _ => {}
                }
            }
            json_response(value)
        }
        "Yaml" => {
            let mut value = yaml_value_from_bridge(&request["value"], capacities)?;
            if operation == "clone" || request["detach"].as_bool() == Some(true) {
                value = value.clone();
            }
            if !request["child"].is_null() {
                let mut child = yaml_value_from_bridge(&request["child"], child_capacities)?;
                if request["child_detach"].as_bool() == Some(true) {
                    child = child.clone();
                }
                match (&mut value, operation) {
                    (serde_yaml::Value::Sequence(values), "array_push") => values.push(child),
                    (serde_yaml::Value::Mapping(values), "object_set") => {
                        let key = serde_yaml::Value::String(key()?.to_owned());
                        let growth = hew_codec::managed_tree::yaml_mapping_growth(values, &key);
                        hew_codec::managed_tree::yaml_mapping_insert(values, growth, key, child);
                    }
                    _ => {}
                }
            }
            yaml_response(value)
        }
        "Toml" => {
            let mut value = toml_value_from_bridge(&request["value"], capacities)?;
            if operation == "clone" {
                value = value.clone();
            }
            if !request["child"].is_null() {
                let child = toml_value_from_bridge(&request["child"], child_capacities)?;
                match (&mut value, operation) {
                    (toml::Value::Array(values), "array_push") => values.push(child),
                    (toml::Value::Table(values), "object_set") => {
                        values.insert(key()?.to_owned(), child);
                    }
                    _ => {}
                }
            }
            toml_response(value)
        }
        _ => Err("unknown encoding format".into()),
    }
}

fn float_to_bridge(value: f64) -> Json {
    json!(["float", value.to_string()])
}

fn bridge_float(value: &Json) -> Result<f64, String> {
    value
        .as_str()
        .ok_or("encoding float is missing")?
        .parse::<f64>()
        .map_err(|error| error.to_string())
}

fn yaml_value_to_bridge(value: serde_yaml::Value) -> Json {
    use serde_yaml::Value;
    match value {
        Value::Null => json!(["null"]),
        Value::Bool(value) => json!(["bool", value]),
        Value::Number(value) => {
            if value.is_i64() || value.is_u64() {
                json!(["integer", value.to_string()])
            } else {
                float_to_bridge(
                    value
                        .as_f64()
                        .expect("YAML number has a float representation"),
                )
            }
        }
        Value::String(value) => json!(["string", value]),
        Value::Sequence(values) => json!([
            "array",
            values
                .into_iter()
                .map(yaml_value_to_bridge)
                .collect::<Vec<_>>()
        ]),
        Value::Mapping(values) => json!([
            "mapping",
            values
                .into_iter()
                .map(|(key, value)| json!([yaml_value_to_bridge(key), yaml_value_to_bridge(value)]))
                .collect::<Vec<_>>()
        ]),
        Value::Tagged(value) => json!([
            "tagged",
            value.tag.to_string(),
            yaml_value_to_bridge(value.value)
        ]),
    }
}

fn yaml_value_from_bridge(value: &Json, capacities: &Json) -> Result<serde_yaml::Value, String> {
    use serde_yaml::Value;
    let invalid = || "invalid YAML value bridge payload".to_owned();
    let payload = &value[1];
    Ok(match value[0].as_str().ok_or_else(invalid)? {
        "null" => Value::Null,
        "bool" => Value::Bool(payload.as_bool().ok_or_else(invalid)?),
        "string" => Value::String(retained_text(payload, capacities)?),
        "integer" => {
            let text = payload.as_str().ok_or_else(invalid)?;
            Value::Number(
                text.parse::<i64>()
                    .map(serde_yaml::Number::from)
                    .or_else(|_| text.parse::<u64>().map(serde_yaml::Number::from))
                    .map_err(|error| error.to_string())?,
            )
        }
        "float" => Value::Number(serde_yaml::Number::from(bridge_float(payload)?)),
        "array" => Value::Sequence(retained_array(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .enumerate()
                .map(|(index, value)| yaml_value_from_bridge(value, &capacities[1][index]))
                .collect::<Result<_, _>>()?,
            &capacities[0],
        )?),
        "mapping" => {
            let pairs = payload.as_array().ok_or_else(invalid)?;
            let mut mapping =
                serde_yaml::Mapping::with_capacity(retained_capacity(&capacities[0], pairs.len())?);
            for (index, pair) in pairs.iter().enumerate() {
                mapping.insert(
                    yaml_value_from_bridge(&pair[0], &capacities[1][index][0])?,
                    yaml_value_from_bridge(&pair[1], &capacities[1][index][1])?,
                );
            }
            Value::Mapping(mapping)
        }
        "tagged" => Value::Tagged(Box::new(serde_yaml::value::TaggedValue {
            tag: serde_yaml::value::Tag::new(payload.as_str().ok_or_else(invalid)?),
            value: yaml_value_from_bridge(&value[2], capacities)?,
        })),
        _ => return Err(invalid()),
    })
}

fn toml_value_to_bridge(value: toml::Value) -> Json {
    use toml::Value;
    match value {
        Value::Boolean(value) => json!(["bool", value]),
        Value::Integer(value) => json!(["integer", value.to_string()]),
        Value::Float(value) => float_to_bridge(value),
        Value::String(value) => json!(["string", value]),
        Value::Datetime(value) => json!(["datetime", value.to_string()]),
        Value::Array(values) => json!([
            "array",
            values
                .into_iter()
                .map(toml_value_to_bridge)
                .collect::<Vec<_>>()
        ]),
        Value::Table(values) => json!([
            "object",
            values
                .into_iter()
                .map(|(key, value)| json!([key, toml_value_to_bridge(value)]))
                .collect::<Vec<_>>()
        ]),
    }
}

fn toml_value_from_bridge(value: &Json, capacities: &Json) -> Result<toml::Value, String> {
    use toml::Value;
    let invalid = || "invalid TOML value bridge payload".to_owned();
    let payload = &value[1];
    Ok(match value[0].as_str().ok_or_else(invalid)? {
        "bool" => Value::Boolean(payload.as_bool().ok_or_else(invalid)?),
        "string" => Value::String(retained_text(payload, capacities)?),
        "integer" => Value::Integer(
            payload
                .as_str()
                .ok_or_else(invalid)?
                .parse::<i64>()
                .map_err(|error| error.to_string())?,
        ),
        "float" => Value::Float(bridge_float(payload)?),
        "datetime" => Value::Datetime(
            payload
                .as_str()
                .ok_or_else(invalid)?
                .parse::<toml::value::Datetime>()
                .map_err(|error| error.to_string())?,
        ),
        "array" => Value::Array(retained_array(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .enumerate()
                .map(|(index, value)| toml_value_from_bridge(value, &capacities[1][index]))
                .collect::<Result<_, _>>()?,
            &capacities[0],
        )?),
        "object" => Value::Table(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .enumerate()
                .map(|(index, pair)| {
                    Ok((
                        retained_text(&pair[0], &capacities[index][0])?,
                        toml_value_from_bridge(&pair[1], &capacities[index][1])?,
                    ))
                })
                .collect::<Result<_, String>>()?,
        ),
        _ => return Err(invalid()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn retained_tree_bridge_preserves_capacity_and_native_clone_policy() {
        for (format, text) in [
            ("Json", "[1,2,3]"),
            ("Yaml", "[1, 2, 3]"),
            ("Toml", "items = [1, 2, 3]"),
        ] {
            let parsed =
                execute(&json!({"format": format, "operation": "parse", "text": text})).unwrap();
            let retained = execute(&json!({"format": format, "operation": "retain", "value": parsed["value"], "capacities": parsed["managed"]["capacities"]})).unwrap();
            assert_eq!(
                retained["managed"]["bytes"], parsed["managed"]["bytes"],
                "{format}"
            );
            assert_eq!(
                retained["managed"]["capacities"], parsed["managed"]["capacities"],
                "{format}"
            );
            let cloned = execute(&json!({"format": format, "operation": "clone", "value": parsed["value"], "capacities": parsed["managed"]["capacities"]})).unwrap();
            if format != "Yaml" {
                assert_eq!(
                    cloned["managed"]["bytes"], parsed["managed"]["copy_bytes"],
                    "{format}"
                );
            }
        }
    }

    #[test]
    fn numbers_beyond_f64_are_refused_on_every_text_format() {
        let big_integer = format!("[{}]", "9".repeat(400));
        for (format, text) in [
            ("Json", "[1e400]"),
            ("Json", "[-1e400]"),
            ("Json", big_integer.as_str()),
            ("Toml", "a = [1e400]"),
        ] {
            let parsed = execute(&json!({"format": format, "operation": "parse", "text": text}));
            let refused = parsed.as_ref().is_err_and(|error| !error.is_empty())
                || parsed
                    .as_ref()
                    .is_ok_and(|reply| reply["error"].is_string());
            assert!(refused, "{format} accepted {text}: {parsed:?}");
        }
        let tiny =
            execute(&json!({"format": "Json", "operation": "parse", "text": "[1e-400]"})).unwrap();
        assert!(tiny["error"].is_null(), "{tiny}");
    }

    #[test]
    fn retained_array_mutation_matches_actual_native_growth() {
        let mut native: Json = serde_json::from_str("[1,2,3]").unwrap();
        let mut bridged =
            execute(&json!({"format": "Json", "operation": "parse", "text": "[1,2,3]"})).unwrap();
        for value in [4, 5] {
            native.as_array_mut().unwrap().push(json!(value));
            bridged = execute(&json!({"format": "Json", "operation": "array_push", "value": bridged["value"], "capacities": bridged["managed"]["capacities"], "child": ["integer", value.to_string()]})).unwrap();
            assert_eq!(
                bridged["managed"]["bytes"],
                managed_tree::json_payload_bytes(&native, false)
                    .unwrap()
                    .to_string()
            );
        }
    }

    #[test]
    fn retained_yaml_mapping_uses_the_native_growth_and_detach_policy() {
        let mut native = serde_yaml::Value::Mapping(serde_yaml::Mapping::new());
        let mut bridged =
            execute(&json!({"format": "Yaml", "operation": "parse", "text": "{}"})).unwrap();
        for key in ["a", "b", "c", "d", "e", "f", "g", "h", "a"] {
            let mapping = native.as_mapping_mut().unwrap();
            let native_key = serde_yaml::Value::String(key.to_owned());
            let growth = managed_tree::yaml_mapping_growth(mapping, &native_key);
            managed_tree::yaml_mapping_insert(
                mapping,
                growth,
                native_key,
                serde_yaml::Value::from(7),
            );
            bridged = execute(&json!({"format": "Yaml", "operation": "object_set", "value": bridged["value"], "capacities": bridged["managed"]["capacities"], "key": key, "child": ["integer", "7"]})).unwrap();
            assert_eq!(
                bridged["managed"]["bytes"],
                managed_tree::yaml_payload_bytes(&native)
                    .unwrap()
                    .to_string(),
                "insert {key}"
            );
        }
        let retained = native;
        let mut native = retained.clone();
        let mapping = native.as_mapping_mut().unwrap();
        let key = serde_yaml::Value::String("detached".to_owned());
        let growth = managed_tree::yaml_mapping_growth(mapping, &key);
        managed_tree::yaml_mapping_insert(mapping, growth, key, serde_yaml::Value::from(8));
        let detached = execute(&json!({"format": "Yaml", "operation": "object_set", "detach": true, "value": bridged["value"], "capacities": bridged["managed"]["capacities"], "key": "detached", "child": ["integer", "8"]})).unwrap();
        assert_eq!(
            detached["managed"]["bytes"],
            managed_tree::yaml_payload_bytes(&native)
                .unwrap()
                .to_string()
        );
        assert_eq!(
            bridged["managed"]["bytes"],
            managed_tree::yaml_payload_bytes(&retained)
                .unwrap()
                .to_string()
        );
    }

    #[test]
    fn json_value_bridge_retains_native_numbers_and_duplicate_policy() {
        let parsed = execute(&json!({"format": "Json", "operation": "parse", "text":
            r#"{"n":0,"u":18446744073709551615,"n":-9223372036854775808,"f":1.0,"z":-0.0}"#
        }))
        .unwrap();
        assert_eq!(
            parsed["value"],
            json!([
                "object",
                [
                    ["f", ["float", "1.0"]],
                    ["n", ["integer", "-9223372036854775808"]],
                    ["u", ["integer", "18446744073709551615"]],
                    ["z", ["float", "-0.0"]]
                ]
            ])
        );
        let encoded =
            execute(&json!({"format": "Json", "operation": "stringify", "value": parsed["value"]}))
                .unwrap();
        assert_eq!(
            encoded["text"],
            r#"{"f":1.0,"n":-9223372036854775808,"u":18446744073709551615,"z":-0.0}"#
        );
    }

    #[test]
    fn json_value_bridge_preserves_parser_errors_and_invalid_handle_failure() {
        let failed =
            execute(&json!({"format": "Json", "operation": "parse", "text": "["})).unwrap();
        assert_eq!(
            failed["error"],
            "EOF while parsing a list at line 1 column 1"
        );
        let failed =
            execute(&json!({"format": "Json", "operation": "stringify", "value": null})).unwrap();
        assert_eq!(
            failed["error"],
            "json stringify failed: invalid value handle"
        );
        let succeeded =
            execute(&json!({"format": "Json", "operation": "parse", "text": "null"})).unwrap();
        assert_eq!(succeeded["value"], json!(["null"]));
        assert!(succeeded.get("error").is_none());
    }
}
