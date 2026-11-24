//! Pure-Rust cryptographic digest, HMAC, and constant-time equality bridge for
//! `std.crypto.hash` in the sandbox VM.

use hmac::{Hmac, KeyInit, Mac};
use serde::Deserialize;
use serde_json::{json, Value};
use sha2::{Digest, Sha256, Sha384, Sha512};
use subtle::ConstantTimeEq;
use wasm_bindgen::prelude::*;

#[derive(Deserialize)]
struct Request {
    operation: String,
    args: Vec<Vec<u8>>,
}

/// Execute a deterministic `std.crypto.hash` primitive using the same pure-Rust
/// crates (`sha2`, `hmac`, `subtle`) as `hew-std`.
#[wasm_bindgen(js_name = executeCrypto)]
#[must_use]
pub fn execute_crypto(request: &str) -> String {
    execute(request)
        .unwrap_or_else(|error| json!({ "ok": false, "error": error }))
        .to_string()
}

fn arg(args: &[Vec<u8>], index: usize) -> Result<&[u8], String> {
    args.get(index)
        .map(Vec::as_slice)
        .ok_or_else(|| "crypto argument is missing".into())
}

fn digest_bytes<D: Digest>(data: &[u8]) -> Value {
    json!({ "ok": true, "bytes": D::digest(data).to_vec() })
}

fn execute(request: &str) -> Result<Value, String> {
    let req: Request = serde_json::from_str(request).map_err(|error| error.to_string())?;
    match req.operation.as_str() {
        "sha256" => Ok(digest_bytes::<Sha256>(arg(&req.args, 0)?)),
        "sha384" => Ok(digest_bytes::<Sha384>(arg(&req.args, 0)?)),
        "sha512" => Ok(digest_bytes::<Sha512>(arg(&req.args, 0)?)),
        "hmac_sha256" => {
            let key = arg(&req.args, 0)?;
            let data = arg(&req.args, 1)?;
            let mut mac =
                Hmac::<Sha256>::new_from_slice(key).expect("HMAC accepts keys of any length");
            mac.update(data);
            Ok(json!({ "ok": true, "bytes": mac.finalize().into_bytes().to_vec() }))
        }
        "constant_time_eq" => {
            let a = arg(&req.args, 0)?;
            let b = arg(&req.args, 1)?;
            let equal = a.len() == b.len() && bool::from(a.ct_eq(b));
            Ok(json!({ "ok": true, "equal": equal }))
        }
        _ => Err("unknown crypto operation".into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hex(bytes: &[u64]) -> String {
        use std::fmt::Write;
        let mut out = String::with_capacity(bytes.len() * 2);
        for &b in bytes {
            write!(&mut out, "{b:02x}").unwrap();
        }
        out
    }

    fn call(operation: &str, args: &[&[u8]]) -> Value {
        let req = json!({ "operation": operation, "args": args }).to_string();
        serde_json::from_str(&execute_crypto(&req)).expect("valid json response")
    }

    #[test]
    fn nist_sha2_vectors() {
        let sha256 = call("sha256", &[b"abc"]);
        assert_eq!(sha256["ok"], true);
        let bytes: Vec<u64> = sha256["bytes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_u64().unwrap())
            .collect();
        assert_eq!(
            hex(&bytes),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );

        let sha384 = call("sha384", &[b"abc"]);
        let bytes384: Vec<u64> = sha384["bytes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_u64().unwrap())
            .collect();
        assert_eq!(
            hex(&bytes384),
            "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"
        );

        let sha512 = call("sha512", &[b"abc"]);
        let bytes512: Vec<u64> = sha512["bytes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_u64().unwrap())
            .collect();
        assert_eq!(
            hex(&bytes512),
            "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
        );
    }

    #[test]
    fn rfc4231_hmac_sha256_vector() {
        let key = [0x0bu8; 20];
        let res = call("hmac_sha256", &[&key, b"Hi There"]);
        assert_eq!(res["ok"], true);
        let bytes: Vec<u64> = res["bytes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_u64().unwrap())
            .collect();
        assert_eq!(
            hex(&bytes),
            "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
        );
    }

    #[test]
    fn constant_time_eq_checks_contents_and_length() {
        assert_eq!(call("constant_time_eq", &[b"abc", b"abc"])["equal"], true);
        assert_eq!(call("constant_time_eq", &[b"abc", b"abd"])["equal"], false);
        assert_eq!(call("constant_time_eq", &[b"abc", b"abcd"])["equal"], false);
        assert_eq!(call("constant_time_eq", &[b"", b""])["equal"], true);
    }

    #[test]
    fn invalid_request_returns_error() {
        let missing = call("sha256", &[]);
        assert_eq!(missing["ok"], false);
        let unknown = call("md5", &[b"abc"]);
        assert_eq!(unknown["ok"], false);
    }
}
