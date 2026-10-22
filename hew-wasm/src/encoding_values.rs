//! Lossless native Encoding Value text operations for the sandbox bridge.

use serde_json::{json, Value as Json};

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

fn json_value_from_bridge(value: &Json) -> Result<Json, String> {
    let invalid = || "invalid JSON value bridge payload".to_owned();
    let tag = value[0].as_str().ok_or_else(invalid)?;
    let payload = &value[1];
    Ok(match tag {
        "null" => Json::Null,
        "bool" => Json::Bool(payload.as_bool().ok_or_else(invalid)?),
        "string" => Json::String(payload.as_str().ok_or_else(invalid)?.to_owned()),
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
        "array" => Json::Array(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .map(json_value_from_bridge)
                .collect::<Result<_, _>>()?,
        ),
        "object" => Json::Object(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .map(|pair| {
                    Ok((
                        pair[0].as_str().ok_or_else(invalid)?.to_owned(),
                        json_value_from_bridge(&pair[1])?,
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
                    .map(json_value_to_bridge)
                    .map_err(|error| error.to_string()),
                "Yaml" => hew_codec::yaml_value::parse(text).map(yaml_value_to_bridge),
                "Toml" => text
                    .parse::<toml::Table>()
                    .map(|table| toml_value_to_bridge(toml::Value::Table(table)))
                    .map_err(|error| error.to_string()),
                _ => return Err("unknown encoding format".into()),
            };
            Ok(match parsed {
                Ok(value) => json!({"value": value}),
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
                "Json" => serde_json::to_string(&json_value_from_bridge(value)?)
                    .map_err(|error| format!("json stringify failed: {error}")),
                "Yaml" => serde_yaml::to_string(&yaml_value_from_bridge(value)?)
                    .map_err(|error| format!("yaml stringify failed: {error}")),
                "Toml" => toml::to_string(&toml_value_from_bridge(value)?)
                    .map_err(|error| format!("toml: {error}")),
                _ => return Err("unknown encoding format".into()),
            };
            Ok(match encoded {
                Ok(text) => json!({"text": text}),
                Err(error) => json!({"error": error}),
            })
        }
        _ => Err("unknown encoding value operation".into()),
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

fn yaml_value_from_bridge(value: &Json) -> Result<serde_yaml::Value, String> {
    use serde_yaml::Value;
    let invalid = || "invalid YAML value bridge payload".to_owned();
    let payload = &value[1];
    Ok(match value[0].as_str().ok_or_else(invalid)? {
        "null" => Value::Null,
        "bool" => Value::Bool(payload.as_bool().ok_or_else(invalid)?),
        "string" => Value::String(payload.as_str().ok_or_else(invalid)?.to_owned()),
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
        "array" => Value::Sequence(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .map(yaml_value_from_bridge)
                .collect::<Result<_, _>>()?,
        ),
        "mapping" => Value::Mapping(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .map(|pair| {
                    Ok((
                        yaml_value_from_bridge(&pair[0])?,
                        yaml_value_from_bridge(&pair[1])?,
                    ))
                })
                .collect::<Result<_, String>>()?,
        ),
        "tagged" => Value::Tagged(Box::new(serde_yaml::value::TaggedValue {
            tag: serde_yaml::value::Tag::new(payload.as_str().ok_or_else(invalid)?),
            value: yaml_value_from_bridge(&value[2])?,
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

fn toml_value_from_bridge(value: &Json) -> Result<toml::Value, String> {
    use toml::Value;
    let invalid = || "invalid TOML value bridge payload".to_owned();
    let payload = &value[1];
    Ok(match value[0].as_str().ok_or_else(invalid)? {
        "bool" => Value::Boolean(payload.as_bool().ok_or_else(invalid)?),
        "string" => Value::String(payload.as_str().ok_or_else(invalid)?.to_owned()),
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
        "array" => Value::Array(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .map(toml_value_from_bridge)
                .collect::<Result<_, _>>()?,
        ),
        "object" => Value::Table(
            payload
                .as_array()
                .ok_or_else(invalid)?
                .iter()
                .map(|pair| {
                    Ok((
                        pair[0].as_str().ok_or_else(invalid)?.to_owned(),
                        toml_value_from_bridge(&pair[1])?,
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
