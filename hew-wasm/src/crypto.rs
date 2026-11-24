//! Pure-Rust cryptographic digest, HMAC, and constant-time equality bridge for
//! `std.crypto.hash` in the sandbox VM.

use std::borrow::Cow;

use hmac::{Hmac, KeyInit, Mac};
use serde::Deserialize;
use serde_json::{json, Value};
use sha2::{Digest, Sha256, Sha384, Sha512};
use subtle::{Choice, ConditionallySelectable, ConstantTimeEq};
use wasm_bindgen::prelude::*;

#[derive(Deserialize)]
struct Request<'a> {
    #[serde(borrow)]
    operation: Cow<'a, str>,
    #[serde(default)]
    args: Option<Vec<Vec<u8>>>,
    #[serde(default, borrow)]
    args_hex: Option<Vec<Cow<'a, str>>>,
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

/// Returns `Choice::from(1)` iff `lo <= b <= hi`, without secret-dependent branches.
fn ct_in_range(b: u8, lo: u8, hi: u8) -> Choice {
    // `u16::from(x).wrapping_sub(u16::from(y))` produces high byte `0x00` when
    // `x >= y` and `0xff` when `x < y`.
    let ge_lo = !u16::from(b).wrapping_sub(u16::from(lo)).to_be_bytes()[0];
    let le_hi = !u16::from(hi).wrapping_sub(u16::from(b)).to_be_bytes()[0];
    Choice::from((ge_lo & le_hi) & 1)
}

/// Decodes a single ASCII hex digit into `(nibble, valid)` without branches or lookup tables.
fn ct_decode_hex_nibble(b: u8) -> (u8, Choice) {
    let is_digit = ct_in_range(b, b'0', b'9');
    let is_lower = ct_in_range(b, b'a', b'f');
    let is_upper = ct_in_range(b, b'A', b'F');

    let mut nibble = 0u8;
    nibble = u8::conditional_select(&nibble, &b.wrapping_sub(b'0'), is_digit);
    nibble = u8::conditional_select(&nibble, &b.wrapping_sub(b'a').wrapping_add(10), is_lower);
    nibble = u8::conditional_select(&nibble, &b.wrapping_sub(b'A').wrapping_add(10), is_upper);

    (nibble, is_digit | is_lower | is_upper)
}

/// Decodes a fixed-width hex string into raw bytes without value-dependent
/// branches, lookup tables, or early exits on invalid characters.
fn decode_hex_ct(hex: &str) -> Result<Vec<u8>, String> {
    let raw = hex.as_bytes();
    if !raw.len().is_multiple_of(2) {
        return Err("constant_time_eq hex argument must have even length".into());
    }
    let mut out = Vec::with_capacity(raw.len() / 2);
    let mut all_valid = Choice::from(1u8);
    for pair in raw.chunks_exact(2) {
        let (hi, hi_ok) = ct_decode_hex_nibble(pair[0]);
        let (lo, lo_ok) = ct_decode_hex_nibble(pair[1]);
        all_valid &= hi_ok & lo_ok;
        out.push((hi << 4) | lo);
    }
    if bool::from(all_valid) {
        Ok(out)
    } else {
        Err("constant_time_eq hex argument contains invalid characters".into())
    }
}

fn digest_bytes<D: Digest>(data: &[u8]) -> Value {
    json!({ "ok": true, "bytes": D::digest(data).to_vec() })
}

fn execute(request: &str) -> Result<Value, String> {
    let req: Request<'_> = serde_json::from_str(request).map_err(|error| error.to_string())?;
    let args = req.args.as_deref().unwrap_or(&[]);
    match req.operation.as_ref() {
        "sha256" => Ok(digest_bytes::<Sha256>(arg(args, 0)?)),
        "sha384" => Ok(digest_bytes::<Sha384>(arg(args, 0)?)),
        "sha512" => Ok(digest_bytes::<Sha512>(arg(args, 0)?)),
        "hmac_sha256" => {
            let key = arg(args, 0)?;
            let data = arg(args, 1)?;
            let mut mac =
                Hmac::<Sha256>::new_from_slice(key).expect("HMAC accepts keys of any length");
            mac.update(data);
            Ok(json!({ "ok": true, "bytes": mac.finalize().into_bytes().to_vec() }))
        }
        "constant_time_eq" => {
            if req.args.is_some() {
                return Err(
                    "constant_time_eq requires fixed-width args_hex instead of decimal args".into(),
                );
            }
            let args_hex = req
                .args_hex
                .as_deref()
                .ok_or_else(|| "constant_time_eq requires args_hex".to_string())?;
            if args_hex.len() != 2 {
                return Err("constant_time_eq requires exactly two hex arguments".into());
            }
            let a = decode_hex_ct(args_hex[0].as_ref())?;
            let b = decode_hex_ct(args_hex[1].as_ref())?;
            let equal = a.len() == b.len() && bool::from(a.ct_eq(&b));
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

    fn encode_hex_bytes(bytes: &[u8]) -> String {
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

    fn call_ct_eq_bytes(a: &[u8], b: &[u8]) -> Value {
        call_ct_eq_hex(&encode_hex_bytes(a), &encode_hex_bytes(b))
    }

    fn call_ct_eq_hex(a: &str, b: &str) -> Value {
        let req = json!({ "operation": "constant_time_eq", "args_hex": [a, b] }).to_string();
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
        assert_eq!(call_ct_eq_bytes(b"abc", b"abc")["equal"], true);
        assert_eq!(call_ct_eq_bytes(b"abc", b"abd")["equal"], false);
        assert_eq!(call_ct_eq_bytes(b"abc", b"xbc")["equal"], false);
        assert_eq!(call_ct_eq_bytes(b"abc", b"axc")["equal"], false);
        assert_eq!(call_ct_eq_bytes(b"abc", b"abcd")["equal"], false);
        assert_eq!(call_ct_eq_bytes(b"", b"")["equal"], true);
        assert_eq!(call_ct_eq_bytes(b"", b"a")["equal"], false);
        assert_eq!(call_ct_eq_hex("09ff", "09FF")["equal"], true);

        let all_bytes: Vec<u8> = (0u8..=255).collect();
        assert_eq!(call_ct_eq_bytes(&all_bytes, &all_bytes)["equal"], true);
        let mut flipped = all_bytes.clone();
        flipped[128] ^= 0x01;
        assert_eq!(call_ct_eq_bytes(&all_bytes, &flipped)["equal"], false);
    }

    #[test]
    fn decode_hex_ct_round_trips_every_byte_value() {
        let all_bytes: Vec<u8> = (0u8..=255).collect();
        let lower = encode_hex_bytes(&all_bytes);
        let upper = lower.to_uppercase();
        assert_eq!(decode_hex_ct(&lower).unwrap(), all_bytes);
        assert_eq!(decode_hex_ct(&upper).unwrap(), all_bytes);
        assert_eq!(decode_hex_ct("").unwrap(), Vec::<u8>::new());
    }

    #[test]
    fn constant_time_eq_rejects_decimal_args_and_malformed_hex() {
        // Variable-length decimal JSON arrays must be rejected for constant_time_eq.
        let legacy = call("constant_time_eq", &[b"abc", b"abc"]);
        assert_eq!(legacy["ok"], false);

        // Missing or wrong-arity args_hex.
        let missing: Value = serde_json::from_str(&execute_crypto(
            &json!({ "operation": "constant_time_eq" }).to_string(),
        ))
        .unwrap();
        assert_eq!(missing["ok"], false);
        let one_arg: Value = serde_json::from_str(&execute_crypto(
            &json!({ "operation": "constant_time_eq", "args_hex": ["00"] }).to_string(),
        ))
        .unwrap();
        assert_eq!(one_arg["ok"], false);
        let three_args: Value = serde_json::from_str(&execute_crypto(
            &json!({ "operation": "constant_time_eq", "args_hex": ["00", "00", "00"] }).to_string(),
        ))
        .unwrap();
        assert_eq!(three_args["ok"], false);

        // Odd length and non-hex boundary characters around '0'-'9', 'A'-'F', 'a'-'f'.
        assert_eq!(call_ct_eq_hex("abc", "abc")["ok"], false);
        for bad in [
            "/0", ":0", "@0", "G0", "`0", "g0", "0/", "0:", "0@", "0G", "0`", "0g", "zz",
        ] {
            assert_eq!(
                call_ct_eq_hex(bad, "00")["ok"],
                false,
                "expected error for {bad}"
            );
            assert_eq!(
                call_ct_eq_hex("00", bad)["ok"],
                false,
                "expected error for {bad}"
            );
        }
    }

    #[test]
    fn invalid_request_returns_error() {
        let missing = call("sha256", &[]);
        assert_eq!(missing["ok"], false);
        let unknown = call("md5", &[b"abc"]);
        assert_eq!(unknown["ok"], false);
    }
}
