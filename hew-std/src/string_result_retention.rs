//! R1/R2/R3 retention measurements for local hew-std string producers.
//!
//! Every promoted symbol is named at its own call site below. The shared
//! instrument proves two live results are distinct (R1), each arrives solely
//! owned (R2), and the producer/input remains usable after the caller releases
//! both (R3). JSON/YAML instead use managed ownership and verify results
//! after container destruction, including canonical empty and embedded NUL.
//! An unmeasured symbol has no call site here and must remain absent
//! from the classification's `result-retention` axis.

use std::ffi::{c_char, CStr, CString};

use hew_cabi::cabi::{cstring_ensure_unique, free_cstring};
use hew_runtime::bytes::{hew_bytes_drop, BytesTriple};

fn assert_transferred(
    symbol: &str,
    mut call: impl FnMut() -> *mut c_char,
    validate: impl Fn(&CStr),
) {
    let first = call();
    let second = call();
    assert!(
        !first.is_null() && !second.is_null(),
        "{symbol}: expected two live results"
    );
    assert_ne!(
        first, second,
        "{symbol}: R1 failed: two live results share an address"
    );

    for (label, ptr) in [("first", first), ("second", second)] {
        // SAFETY: `ptr` is a live header-aware result from the named producer.
        let unique = unsafe { cstring_ensure_unique(ptr) };
        assert_eq!(
            unique, ptr,
            "{symbol}: R2 failed: the {label} result was shared at handoff"
        );
        // SAFETY: the uniqueness probe returned the live pointer unchanged.
        validate(unsafe { CStr::from_ptr(ptr) });
    }

    // SAFETY: R1/R2 establish two distinct, solely-owned live results.
    unsafe {
        free_cstring(first);
        free_cstring(second);
    }

    let third = call();
    assert!(
        !third.is_null(),
        "{symbol}: R3 failed after caller releases"
    );
    // SAFETY: `third` is a fresh live result.
    validate(unsafe { CStr::from_ptr(third) });
    // SAFETY: the third result is solely owned by this caller.
    unsafe { free_cstring(third) };
}

#[test]
fn uuid_results_are_transferred() {
    let valid_uuid = |text: &CStr| {
        let parsed = uuid::Uuid::parse_str(text.to_str().unwrap()).expect("valid UUID");
        assert!(matches!(parsed.get_version_num(), 4 | 7));
    };
    assert_transferred("hew_uuid_v4", || crate::uuid::hew_uuid_v4(), valid_uuid);
    assert_transferred("hew_uuid_v7", || crate::uuid::hew_uuid_v7(), valid_uuid);
}

#[test]
fn url_and_cidr_results_are_transferred() {
    let input = CString::new("https://example.test/a/b?q=hew#frag").unwrap();
    // SAFETY: `input` is a valid NUL-terminated URL.
    let url = unsafe { crate::url::hew_url_parse(input.as_ptr()) };
    assert!(!url.is_null());

    for (symbol, expected, call) in [
        (
            "hew_url_scheme",
            "https",
            crate::url::hew_url_scheme
                as unsafe extern "C" fn(*const crate::url::HewUrl) -> *mut c_char,
        ),
        ("hew_url_host", "example.test", crate::url::hew_url_host),
        ("hew_url_path", "/a/b", crate::url::hew_url_path),
        ("hew_url_query", "q=hew", crate::url::hew_url_query),
        ("hew_url_fragment", "frag", crate::url::hew_url_fragment),
        (
            "hew_url_to_string",
            "https://example.test/a/b?q=hew#frag",
            crate::url::hew_url_to_string,
        ),
    ] {
        assert_transferred(
            symbol,
            // SAFETY: `url` remains live through the whole loop.
            || unsafe { call(url) },
            |text| assert_eq!(text.to_str().unwrap(), expected),
        );
    }
    // SAFETY: all accessors borrowed `url`; the handle is still owned here.
    unsafe { crate::url::hew_url_free(url) };

    let cidr = CString::new("192.0.2.129/25").unwrap();
    assert_transferred(
        "hew_cidr_network",
        // SAFETY: `cidr` is a live NUL-terminated string.
        || unsafe { crate::ipnet::hew_cidr_network(cidr.as_ptr()) },
        |text| assert_eq!(text.to_str().unwrap(), "192.0.2.128"),
    );
    assert_transferred(
        "hew_cidr_broadcast",
        // SAFETY: `cidr` is a live NUL-terminated string.
        || unsafe { crate::ipnet::hew_cidr_broadcast(cidr.as_ptr()) },
        |text| assert_eq!(text.to_str().unwrap(), "192.0.2.255"),
    );
}

/// Returned text must outlive sibling releases, later calls and its value container.
fn assert_managed_value_results<T>(
    from_string: unsafe extern "C" fn(*const hew_cabi::string::HewString) -> *mut T,
    get_string: unsafe extern "C" fn(*const T) -> *mut hew_cabi::string::HewString,
    stringify: unsafe extern "C" fn(*const T) -> *mut hew_cabi::string::HewString,
    free_value: unsafe extern "C" fn(*mut T),
    free_string: unsafe extern "C" fn(*mut hew_cabi::string::HewString),
    decode: impl Fn(&str) -> String,
) {
    use crate::test_string::ManagedString;
    use hew_cabi::string::{string_as_str, string_release, string_retain};

    for expected in ["", "clé\0雪\0fin"] {
        let input = ManagedString::new(expected);
        // SAFETY: the input is a live managed string, borrowed by the constructor.
        let value = unsafe { from_string(input.as_ptr()) };
        assert!(!value.is_null());
        drop(input);
        // SAFETY: value owns its copied contents; all text results are separate owners.
        unsafe {
            let first = get_string(value);
            let second = get_string(value);
            assert_eq!(first.is_null(), expected.is_empty());
            assert_eq!(second.is_null(), expected.is_empty());
            if !expected.is_empty() {
                assert_ne!(first, second);
            }
            assert_eq!(string_as_str(first), expected);
            let retained = string_retain(first);
            free_string(first);
            assert_eq!(string_as_str(second), expected);
            free_string(second);

            let serialized = stringify(value);
            assert_eq!(decode(string_as_str(serialized)), expected);
            let later_serialized = stringify(value);
            assert_ne!(serialized, later_serialized);
            free_string(later_serialized);
            let third = get_string(value);
            free_value(value);

            assert_eq!(string_as_str(retained), expected);
            assert_eq!(string_as_str(third), expected);
            assert_eq!(decode(string_as_str(serialized)), expected);
            string_release(retained);
            free_string(third);
            free_string(serialized);
        }
    }
}

#[test]
fn json_managed_results_survive_value_destruction() {
    use crate::json;
    assert_managed_value_results(
        json::hew_json_from_string,
        json::hew_json_get_string,
        json::hew_json_stringify,
        json::hew_json_free,
        json::hew_json_string_free,
        |text| serde_json::from_str::<String>(text).unwrap(),
    );
}

#[test]
fn yaml_managed_results_survive_value_destruction() {
    use crate::yaml;
    assert_managed_value_results(
        yaml::hew_yaml_from_string,
        yaml::hew_yaml_get_string,
        yaml::hew_yaml_stringify,
        yaml::hew_yaml_free,
        yaml::hew_yaml_string_free,
        |text| serde_yaml::from_str::<String>(text).unwrap(),
    );
}

#[test]
fn toml_results_are_transferred() {
    let scalar = CString::new("retention probe").unwrap();

    // SAFETY: `scalar` is a valid NUL-terminated string.
    let toml_scalar = unsafe { crate::toml::hew_toml_from_string(scalar.as_ptr()) };
    assert_transferred(
        "hew_toml_get_string",
        // SAFETY: `toml_scalar` stays live through the measurement.
        || unsafe { crate::toml::hew_toml_get_string(toml_scalar) },
        |text| assert_eq!(text.to_str().unwrap(), "retention probe"),
    );
    // SAFETY: the getter borrowed the live value.
    unsafe { crate::toml::hew_toml_free(toml_scalar) };

    let key = CString::new("answer").unwrap();
    let table = crate::toml::hew_toml_table_new();
    // SAFETY: table/key are live; the setter copies the scalar value.
    unsafe { crate::toml::hew_toml_table_set_int(table, key.as_ptr(), 42) };
    assert_transferred(
        "hew_toml_stringify",
        // SAFETY: `table` stays live through the measurement.
        || unsafe { crate::toml::hew_toml_stringify(table) },
        |text| assert_eq!(text.to_str().unwrap(), "answer = 42\n"),
    );
    // SAFETY: stringify borrowed the live value.
    unsafe { crate::toml::hew_toml_free(table) };

    assert_transferred(
        "hew_toml_last_serialize_error",
        || {
            // Induce the documented deterministic error before every read.
            // SAFETY: null is an explicitly accepted invalid-value sentinel.
            let empty = unsafe { crate::toml::hew_toml_stringify(std::ptr::null()) };
            // SAFETY: the failed stringify still returns an owned empty string.
            unsafe { free_cstring(empty) };
            crate::toml::hew_toml_last_serialize_error()
        },
        |text| {
            assert_eq!(
                text.to_str().unwrap(),
                "toml: cannot serialize an invalid value"
            );
        },
    );
}

#[test]
fn markdown_results_are_transferred() {
    let markdown = CString::new("# heading\n\n<script>bad()</script>").unwrap();
    assert_transferred(
        "hew_markdown_to_html",
        // SAFETY: `markdown` is a live NUL-terminated string.
        || unsafe { crate::markdown::hew_markdown_to_html(markdown.as_ptr()) },
        |text| assert!(text.to_str().unwrap().contains("<h1>heading</h1>")),
    );
    assert_transferred(
        "hew_markdown_to_html_safe",
        // SAFETY: `markdown` is a live NUL-terminated string.
        || unsafe { crate::markdown::hew_markdown_to_html_safe(markdown.as_ptr()) },
        |text| {
            let text = text.to_str().unwrap();
            assert!(text.contains("<h1>heading</h1>"));
            assert!(!text.contains("<script>"));
        },
    );
}

#[test]
fn regex_managed_results_survive_inputs_and_handle() {
    use crate::regex::*;
    use crate::test_string::ManagedString;
    use hew_cabi::string::{string_as_str, string_release};

    let pattern = ManagedString::new("(?P<part>a\0[0-9]+)");
    let text = ManagedString::new("left a\u{0}1 right a\u{0}22 tail");
    let replacement = ManagedString::new("R\0雪");
    // SAFETY: pattern is a live managed string borrowed during compilation.
    let re = unsafe { hew_regex_new(pattern.as_ptr()) };
    assert!(!re.is_null());
    drop(pattern);
    // SAFETY: re and input handles are live; each output transfers an independent owner.
    unsafe {
        let first = hew_regex_find(re, text.as_ptr());
        let second = hew_regex_find(re, text.as_ptr());
        assert_ne!(first, second);
        assert_eq!(string_as_str(first), "a\u{0}1");
        string_release(first);
        let third = hew_regex_find(re, text.as_ptr());
        let captured = hew_regex_capture(re, text.as_ptr(), 1);
        let replaced = hew_regex_replace(re, text.as_ptr(), replacement.as_ptr());
        let later = hew_regex_replace(re, text.as_ptr(), replacement.as_ptr());
        assert_ne!(replaced, later);
        string_release(later);
        hew_regex_free(re);
        drop(text);
        drop(replacement);

        assert_eq!(string_as_str(second), "a\u{0}1");
        assert_eq!(string_as_str(third), "a\u{0}1");
        assert_eq!(string_as_str(captured), "a\u{0}1");
        assert_eq!(string_as_str(replaced), "left R\0雪 right R\0雪 tail");
        string_release(second);
        string_release(third);
        hew_regex_free_capture(captured);
        string_release(replaced);
    }
}

fn borrowed_triple(data: &mut [u8]) -> BytesTriple {
    BytesTriple {
        ptr: data.as_mut_ptr(),
        offset: 0,
        len: u32::try_from(data.len()).unwrap(),
    }
}

#[test]
fn encrypt_results_are_transferred() {
    let mut key = [0x5au8; 32];
    let key_triple = borrowed_triple(&mut key);
    let plaintext = CString::new("local encryption retention probe").unwrap();

    assert_transferred(
        "hew_encrypt_try_seal_base64_hew",
        // SAFETY: key/plaintext stay live through the measurement.
        || unsafe {
            crate::encrypt::hew_encrypt_try_seal_base64_hew(
                &raw const key_triple,
                plaintext.as_ptr(),
            )
        },
        |text| {
            let text = text.to_bytes();
            assert!(text.len() > 32 && text.iter().all(u8::is_ascii));
        },
    );

    // Build one raw nonce+ciphertext+tag buffer through the same implementation
    // used by the Hew wrapper so both open producers receive valid local input.
    // SAFETY: key/plaintext are live; null output asks for the required size.
    let ciphertext_len = unsafe {
        crate::encrypt::hew_encrypt_seal(
            key.as_ptr(),
            key.len(),
            plaintext.as_ptr(),
            std::ptr::null_mut(),
            0,
        )
    };
    assert!(ciphertext_len > 0);
    let mut ciphertext = vec![0u8; ciphertext_len];
    // SAFETY: output has exactly the requested capacity.
    let written = unsafe {
        crate::encrypt::hew_encrypt_seal(
            key.as_ptr(),
            key.len(),
            plaintext.as_ptr(),
            ciphertext.as_mut_ptr(),
            ciphertext.len(),
        )
    };
    assert_eq!(written, ciphertext_len);
    let ciphertext_triple = borrowed_triple(&mut ciphertext);

    assert_transferred(
        "hew_encrypt_try_open_hew",
        // SAFETY: both borrowed triples stay live through the measurement.
        || unsafe {
            crate::encrypt::hew_encrypt_try_open_hew(
                &raw const key_triple,
                &raw const ciphertext_triple,
            )
        },
        |text| assert_eq!(text, plaintext.as_c_str()),
    );
    assert_transferred(
        "hew_encrypt_must_open_hew",
        // SAFETY: both borrowed triples stay live through the measurement.
        || unsafe {
            crate::encrypt::hew_encrypt_must_open_hew(
                &raw const key_triple,
                &raw const ciphertext_triple,
            )
        },
        |text| assert_eq!(text, plaintext.as_c_str()),
    );
    assert_transferred(
        "hew_encrypt_open_hew",
        // SAFETY: both borrowed triples stay live through the measurement.
        || unsafe {
            crate::encrypt::hew_encrypt_open_hew(
                &raw const key_triple,
                &raw const ciphertext_triple,
            )
        },
        |text| assert_eq!(text, plaintext.as_c_str()),
    );
}

#[test]
fn jwt_and_password_results_are_transferred() {
    let payload = CString::new(r#"{"sub":"hew"}"#).unwrap();
    let secret = CString::new("local-retention-secret").unwrap();
    assert_transferred(
        "hew_jwt_encode_hew",
        // SAFETY: payload/secret stay live through the measurement.
        || unsafe { crate::jwt::hew_jwt_encode_hew(payload.as_ptr(), secret.as_ptr(), 0) },
        |text| assert_eq!(text.to_str().unwrap().split('.').count(), 3),
    );

    // Produce a valid token once; the decode calls borrow this independent copy.
    // SAFETY: payload/secret are live NUL-terminated strings.
    let token_ptr = unsafe { crate::jwt::hew_jwt_encode_hew(payload.as_ptr(), secret.as_ptr(), 0) };
    assert!(!token_ptr.is_null());
    // SAFETY: token_ptr is a live NUL-terminated JWT result.
    let token = unsafe { CStr::from_ptr(token_ptr) }.to_owned();
    // SAFETY: token_ptr is the sole owner of the temporary token result.
    unsafe { free_cstring(token_ptr) };
    assert_transferred(
        "hew_jwt_decode_hew",
        // SAFETY: token/secret stay live through the measurement.
        || unsafe { crate::jwt::hew_jwt_decode_hew(token.as_ptr(), secret.as_ptr(), 0) },
        |text| assert_eq!(text.to_str().unwrap(), r#"{"sub":"hew"}"#),
    );
    assert_transferred(
        "hew_jwt_decode_insecure",
        // SAFETY: token stays live through the measurement.
        || unsafe { crate::jwt::hew_jwt_decode_insecure(token.as_ptr()) },
        |text| assert_eq!(text.to_str().unwrap(), r#"{"sub":"hew"}"#),
    );

    let password = CString::new("correct horse battery staple").unwrap();
    assert_transferred(
        "hew_password_hash",
        // SAFETY: password stays live through the measurement.
        || unsafe { crate::password::hew_password_hash(password.as_ptr()) },
        |text| assert!(text.to_bytes().starts_with(b"$argon2id$")),
    );
    assert_transferred(
        "hew_password_hash_custom",
        // SAFETY: password stays live through the measurement; cost 1 is valid.
        || unsafe { crate::password::hew_password_hash_custom(password.as_ptr(), 1) },
        |text| assert!(text.to_bytes().starts_with(b"$argon2id$")),
    );
}

#[test]
fn datetime_and_cron_results_are_transferred() {
    let format = CString::new("%Y-%m-%dT%H:%M:%SZ").unwrap();
    assert_transferred(
        "hew_datetime_format",
        // SAFETY: format is a live NUL-terminated strftime format.
        || unsafe { crate::time::datetime::hew_datetime_format(0, format.as_ptr()) },
        |text| assert_eq!(text.to_str().unwrap(), "1970-01-01T00:00:00Z"),
    );

    let source = CString::new("0 15 10 * * * *").unwrap();
    // SAFETY: source is a live NUL-terminated cron expression.
    let cron = unsafe { crate::time::cron::hew_cron_parse(source.as_ptr()) };
    assert!(!cron.is_null());
    assert_transferred(
        "hew_cron_to_string",
        // SAFETY: cron stays live through the measurement.
        || unsafe { crate::time::cron::hew_cron_to_string(cron) },
        |text| assert!(!text.to_bytes().is_empty()),
    );
    // SAFETY: to_string borrowed the live cron handle.
    unsafe { crate::time::cron::hew_cron_free(cron) };
}

#[test]
fn local_codec_string_results_are_transferred() {
    let message = crate::protobuf::hew_proto_msg_new();
    let value = CString::new("protobuf retention probe").unwrap();
    // SAFETY: message/value stay live and the setter copies the input.
    unsafe { crate::protobuf::hew_proto_msg_set_string(message, 7, value.as_ptr()) };
    assert_transferred(
        "hew_proto_msg_get_string",
        // SAFETY: message stays live through the measurement.
        || unsafe { crate::protobuf::hew_proto_msg_get_string(message, 7) },
        |text| assert_eq!(text, value.as_c_str()),
    );
    // SAFETY: the getter borrowed the live message.
    unsafe { crate::protobuf::hew_proto_msg_free(message) };

    let json = CString::new(r#"{"codec":"msgpack","n":7}"#).unwrap();
    // SAFETY: json is a live NUL-terminated document.
    let encoded = unsafe { crate::msgpack::hew_msgpack_from_json_hew(json.as_ptr()) };
    assert!(!encoded.ptr.is_null());
    assert_transferred(
        "hew_msgpack_to_json_hew",
        // SAFETY: encoded stays live through the measurement.
        || unsafe { crate::msgpack::hew_msgpack_to_json_hew(&raw const encoded) },
        |text| {
            let value: serde_json::Value = serde_json::from_slice(text.to_bytes()).unwrap();
            assert_eq!(value["codec"], "msgpack");
            assert_eq!(value["n"], 7);
        },
    );
    // SAFETY: the codec borrowed encoded; this releases its sole bytes owner.
    unsafe { hew_bytes_drop(encoded.ptr) };
}
