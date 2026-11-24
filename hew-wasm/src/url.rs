//! Stateless WHATWG URL parser bridge backed by the `url` crate so
//! `std.net.url` executes in the sandbox VM with exact native parity.

use serde::Deserialize;
use serde_json::{json, Value};
use wasm_bindgen::prelude::*;

#[derive(Deserialize)]
struct Request {
    operation: String,
    #[serde(default)]
    url: String,
    #[serde(default)]
    input: String,
}

/// Parse or resolve a URL using the same `url` crate engine as `hew-std`.
#[wasm_bindgen(js_name = executeUrl)]
#[must_use]
pub fn execute_url(request: &str) -> String {
    execute(request)
        .unwrap_or_else(|error| json!({ "ok": false, "error": error }))
        .to_string()
}

fn serialize_parsed(parsed: Option<url::Url>) -> Value {
    match parsed {
        Some(u) => json!({
            "ok": true,
            "valid": true,
            "href": u.as_str(),
            "scheme": u.scheme(),
            "host": u.host_str().unwrap_or(""),
            "port": u.port().map_or(-1, i32::from),
            "path": u.path(),
            "query": u.query().unwrap_or(""),
            "fragment": u.fragment().unwrap_or(""),
        }),
        None => json!({ "ok": true, "valid": false }),
    }
}

fn execute(request: &str) -> Result<Value, String> {
    let req: Request = serde_json::from_str(request).map_err(|error| error.to_string())?;
    match req.operation.as_str() {
        "parse" => Ok(serialize_parsed(url::Url::parse(&req.input).ok())),
        "join" => Ok(serialize_parsed(
            url::Url::parse(&req.url)
                .ok()
                .and_then(|base| base.join(&req.input).ok()),
        )),
        _ => Err("unknown url operation".into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn call(payload: &Value) -> Value {
        serde_json::from_str(&execute_url(&payload.to_string())).expect("valid json response")
    }

    #[test]
    fn parses_components_matching_native_hew_std() {
        let parsed = call(&json!({
            "operation": "parse",
            "input": "https://example.com:8443/v1/items?page=2&q=a#top"
        }));
        assert_eq!(parsed["ok"], true);
        assert_eq!(parsed["valid"], true);
        assert_eq!(parsed["scheme"], "https");
        assert_eq!(parsed["host"], "example.com");
        assert_eq!(parsed["port"], 8443);
        assert_eq!(parsed["path"], "/v1/items");
        assert_eq!(parsed["query"], "page=2&q=a");
        assert_eq!(parsed["fragment"], "top");
        assert_eq!(
            parsed["href"],
            "https://example.com:8443/v1/items?page=2&q=a#top"
        );
    }

    #[test]
    fn absent_optional_components_and_default_port_match_native() {
        let parsed = call(&json!({
            "operation": "parse",
            "input": "https://example.com:443"
        }));
        assert_eq!(parsed["valid"], true);
        assert_eq!(parsed["port"], -1);
        assert_eq!(parsed["path"], "/");
        assert_eq!(parsed["query"], "");
        assert_eq!(parsed["fragment"], "");

        let mailto = call(&json!({
            "operation": "parse",
            "input": "mailto:user@example.com"
        }));
        assert_eq!(mailto["valid"], true);
        assert_eq!(mailto["scheme"], "mailto");
        assert_eq!(mailto["host"], "");
        assert_eq!(mailto["port"], -1);
    }

    #[test]
    fn invalid_parse_and_relative_join() {
        let invalid = call(&json!({ "operation": "parse", "input": "not a url" }));
        assert_eq!(invalid["ok"], true);
        assert_eq!(invalid["valid"], false);

        let joined = call(&json!({
            "operation": "join",
            "url": "https://example.com/a/b",
            "input": "../c?x=1"
        }));
        assert_eq!(joined["valid"], true);
        assert_eq!(joined["href"], "https://example.com/c?x=1");
    }
}
