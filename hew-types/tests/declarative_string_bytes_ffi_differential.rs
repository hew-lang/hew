//! W3.002 Stage 2 -- string and bytes declarative FFI cutover.
//!
//! Pins checker-owned receiver rewrites to the `#[extern_symbol]`
//! annotations declared in `std/string.hew` and `std/io.hew`.

mod common;

use hew_types::check::MethodCallRewrite;
use hew_types::error::TypeErrorKind;

use common::typecheck;

fn has_rewrite(output: &hew_types::TypeCheckOutput, symbol: &str) -> bool {
    output.method_call_rewrites.values().any(
        |rewrite| matches!(rewrite, MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == symbol),
    )
}

#[test]
fn string_methods_resolve_through_std_string_extern_symbols() {
    let source = r#"
        fn main() {
            let s = "hello";
            let _: i64 = s.len();
            let _: bool = s.is_empty();
            let _: bool = s.contains("ell");
            let _: bool = s.starts_with("he");
            let _: bool = s.ends_with("lo");
            let _: string = s.to_uppercase();
            let _: string = s.to_upper();
            let _: string = s.to_lowercase();
            let _: string = s.to_lower();
            let _: string = s.trim();
            let _: bool = s.is_digit();
            let _: bool = s.is_alpha();
            let _: bool = s.is_alphanumeric();
            let _: string = s.clone();
            let _: string = s.replace("l", "x");
            let _: Vec<string> = s.split("l");
            let _: Vec<string> = s.lines();
            let _: i64 = s.find("e");
            let _: i64 = s.index_of("e");
            let _: string = s.slice(0, 1);
            let _: string = s.repeat(2);
            let _: i64 = s.char_at(0);
            let _: Vec<char> = s.chars();
            let _: bytes = s.to_bytes();
        }
    "#;
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "string extern-symbol methods should typecheck; got: {:#?}",
        output.errors
    );
    for symbol in [
        "hew_string_length",
        "hew_string_is_empty",
        "hew_string_contains",
        "hew_string_starts_with",
        "hew_string_ends_with",
        "hew_string_to_uppercase",
        "hew_string_to_lowercase",
        "hew_string_trim",
        "hew_string_is_digit",
        "hew_string_is_alpha",
        "hew_string_is_alphanumeric",
        "hew_string_clone",
        "hew_string_replace",
        "hew_string_split",
        "hew_string_lines",
        "hew_string_find",
        "hew_string_index_of_start",
        "hew_string_slice",
        "hew_string_repeat",
        "hew_string_char_at",
        "hew_string_chars",
        "hew_string_to_bytes",
    ] {
        assert!(
            has_rewrite(&output, symbol),
            "expected {symbol} in method_call_rewrites; got: {:#?}",
            output.method_call_rewrites
        );
    }
}

#[test]
fn bytes_methods_resolve_through_std_io_extern_symbols() {
    let source = r"
        fn exercise(buf: bytes, other: bytes) {
            buf.push(65);
            let _: u8 = buf.pop();
            let _: i64 = buf.len();
            let _: u8 = buf.get(0);
            buf.set(0, 66);
            let _: bool = buf.is_empty();
            buf.clear();
            let _: bool = buf.contains(66);
            let _: string = buf.to_string();
            buf.append(other);
            buf.extend(other);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "bytes extern-symbol methods should typecheck; got: {:#?}",
        output.errors
    );
    for symbol in [
        "hew_bytes_push",
        "hew_vec_pop_i32",
        "hew_vec_len",
        // `bytes.get` routes to the dedicated `hew_bytes_index` getter (the
        // BytesTriple element load), NOT the heap-Vec getter `hew_vec_get_i32`.
        "hew_bytes_index",
        "hew_vec_set_i32",
        "hew_vec_is_empty",
        "hew_vec_clear",
        "hew_vec_contains_i32",
        "hew_bytes_to_string",
        "hew_vec_append",
    ] {
        assert!(
            has_rewrite(&output, symbol),
            "expected {symbol} in method_call_rewrites; got: {:#?}",
            output.method_call_rewrites
        );
    }
}

#[test]
fn bytes_remove_fails_closed_without_a_checked_in_runtime_symbol() {
    let source = r"
        fn exercise(buf: bytes) {
            let _: i32 = buf.remove(0);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.iter().any(|err| {
            err.kind == TypeErrorKind::UndefinedMethod && err.message.contains("remove")
        }),
        "bytes.remove has no checked-in runtime symbol and should fail closed; got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.is_empty(),
        "unsupported bytes.remove must not record a rewrite; got: {:#?}",
        output.method_call_rewrites
    );
}
