//! `std.crypto.jwt` tokens from an untrusted bearer.
//!
//! Input: an algorithm byte, then `token NUL secret`. Properties: decoding,
//! validating and inspecting never panic; a token that decodes also
//! validates and inspects to the same claims; and a payload that encodes
//! inspects back to the same claims.
#![no_main]

use hew_std::jwt::{
    hew_jwt_decode_hew, hew_jwt_decode_insecure, hew_jwt_encode_hew, hew_jwt_validate,
};
use libfuzzer_sys::fuzz_target;

mod support;
use support::{take, Managed};

fn claims(json: &str) -> serde_json::Value {
    serde_json::from_str(json).expect("the codec returns JSON claims")
}

fuzz_target!(|data: &[u8]| {
    let Some((selector, text)) = support::selector_text(data) else {
        return;
    };
    let algo = i32::from(selector % 4);
    let (token, secret) = support::two_fields(text);
    let token = Managed::new(token);
    let secret = Managed::new(secret);
    // SAFETY: inputs are live for each call; results are owned managed strings.
    unsafe {
        let decoded = take(hew_jwt_decode_hew(token.ptr(), secret.ptr(), algo));
        let valid = hew_jwt_validate(token.ptr(), secret.ptr(), algo);
        let inspected = take(hew_jwt_decode_insecure(token.ptr()));
        if !decoded.is_empty() {
            assert_eq!(valid, 1, "a token that decodes does not validate");
            assert_eq!(
                claims(&decoded),
                claims(&inspected),
                "decode and inspect disagree"
            );
        }

        // The token field doubles as a payload for the encoder.
        let issued = take(hew_jwt_encode_hew(token.ptr(), secret.ptr(), algo));
        if !issued.is_empty() {
            let issued = Managed::new(&issued);
            let back = take(hew_jwt_decode_insecure(issued.ptr()));
            assert_eq!(
                claims(&back),
                claims(token.text()),
                "encode changed the claims"
            );
        }
    }
});
