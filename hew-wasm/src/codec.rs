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

/// Execute the compiler's typed codec plan using the shared native engine.
#[wasm_bindgen(js_name = executeCodec)]
#[must_use]
pub fn execute_codec(request: &str) -> String {
    execute(request)
        .unwrap_or_else(|error| json!({"failure": error}))
        .to_string()
}

fn execute(request: &str) -> Result<Json, String> {
    let request: Request = serde_json::from_str(request).map_err(|error| error.to_string())?;
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
