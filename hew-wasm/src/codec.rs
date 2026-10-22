//! The native codec engine exposed to sandbox hosts through Wasm.

use hew_codec::{property::Shape, typed, Format, Source, Value};
use serde::Deserialize;
use serde_json::{json, Value as Json};
use wasm_bindgen::prelude::*;

#[derive(Deserialize)]
struct Request {
    shape: String,
    format: u8,
    direction: String,
    #[serde(default)]
    value: Json,
    #[serde(default)]
    input: Vec<u8>,
    ordinal: Option<usize>,
    duplicate: Option<[String; 2]>,
    error_root: Option<usize>,
}

/// Execute typed codec plans and pure JSON Value text operations with the
/// corresponding native format engines.
#[wasm_bindgen(js_name = executeCodec)]
#[must_use]
pub fn execute_codec(request: &str) -> String {
    execute(request)
        .unwrap_or_else(|error| json!({"failure": error}))
        .to_string()
}

fn execute(request: &str) -> Result<Json, String> {
    let envelope: Json = serde_json::from_str(request).map_err(|error| error.to_string())?;
    if let Some(request) = envelope.get("json_value") {
        return execute_json_value(request);
    }
    let request: Request = serde_json::from_value(envelope).map_err(|error| error.to_string())?;
    let mut shape: Shape =
        serde_json::from_str(&request.shape).map_err(|error| error.to_string())?;
    let format = match request.format {
        0 => Format::Cbor,
        1 => Format::Json,
        2 => Format::Yaml,
        3 => Format::Toml,
        4 => Format::Msgpack,
        _ => return Err("unknown codec format".into()),
    };
    if request.direction == "encode" {
        return typed::encode(&shape, format, &request.value)
            .map(|bytes| json!({"ok": true, "value": bytes}))
            .map_err(|error| error.to_string());
    }
    let decoded = if let Some([path, key]) = request.duplicate {
        Err(hew_codec::DecodeError::Duplicate { path, key })
    } else if request.direction == "test_input" {
        let ordinal = request
            .ordinal
            .ok_or("property input has no argument ordinal")?;
        let values = Source::new(Format::Cbor, &request.input)
            .and_then(|mut source| source.read_any())
            .map_err(|error| error.to_string())?;
        let Value::Seq(mut values) = values else {
            return Err("property input is not an array".into());
        };
        if ordinal >= values.len() {
            return Err("property argument is absent".into());
        }
        typed::decode_value(&shape, Format::Cbor, values.swap_remove(ordinal))
    } else if request.direction == "decode" {
        typed::decode(&shape, format, &request.input)
    } else {
        return Err("unknown codec direction".into());
    };
    let typed_error = |error: hew_codec::DecodeError| -> Result<Json, String> {
        shape.root = request.error_root.ok_or_else(|| error.to_string())?;
        let decoded = typed::decode_value(&shape, Format::Json, error.into_value())
            .map_err(|error| error.to_string())?;
        if let Some(error) = decoded.error {
            return Err(error.to_string());
        }
        Ok(decoded.value)
    };
    let mut typed_error = typed_error;
    match decoded {
        Ok(decoded) => {
            let error = decoded.error.map(&mut typed_error).transpose()?;
            Ok(json!({"ok": true, "value": decoded.value, "error": error}))
        }
        Err(error) => Ok(json!({"ok": false, "value": typed_error(error)?})),
    }
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

fn execute_json_value(request: &Json) -> Result<Json, String> {
    match request["operation"].as_str() {
        Some("parse") => {
            let text = request["text"]
                .as_str()
                .ok_or("JSON parse input is missing")?;
            Ok(match serde_json::from_str(text) {
                Ok(value) => json!({"value": json_value_to_bridge(value)}),
                Err(error) => json!({"error": error.to_string()}),
            })
        }
        Some("stringify") => {
            if request["value"].is_null() {
                return Ok(json!({"error": "json stringify failed: invalid value handle"}));
            }
            let value = json_value_from_bridge(&request["value"])?;
            Ok(match serde_json::to_string(&value) {
                Ok(text) => json!({"text": text}),
                Err(error) => json!({"error": format!("json stringify failed: {error}")}),
            })
        }
        _ => Err("unknown JSON value operation".into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn json_value_bridge_retains_native_numbers_and_duplicate_policy() {
        let parsed = execute_json_value(&json!({"operation": "parse", "text":
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
            execute_json_value(&json!({"operation": "stringify", "value": parsed["value"]}))
                .unwrap();
        assert_eq!(
            encoded["text"],
            r#"{"f":1.0,"n":-9223372036854775808,"u":18446744073709551615,"z":-0.0}"#
        );
    }

    #[test]
    fn json_value_bridge_preserves_parser_errors_and_invalid_handle_failure() {
        let failed = execute_json_value(&json!({"operation": "parse", "text": "["})).unwrap();
        assert_eq!(
            failed["error"],
            "EOF while parsing a list at line 1 column 1"
        );
        let failed = execute_json_value(&json!({"operation": "stringify", "value": null})).unwrap();
        assert_eq!(
            failed["error"],
            "json stringify failed: invalid value handle"
        );
        let succeeded = execute_json_value(&json!({"operation": "parse", "text": "null"})).unwrap();
        assert_eq!(succeeded["value"], json!(["null"]));
        assert!(succeeded.get("error").is_none());
    }
}
