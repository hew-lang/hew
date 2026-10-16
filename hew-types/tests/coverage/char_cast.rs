//! Checker-level coverage for the `char as <integer>` codepoint cast.
//!
//! A `char` is a Unicode scalar value; `as <integer>` extracts the codepoint
//! without truncation. The reverse
//! (`<integer> as char`) and `char -> float` stay rejected — they need a
//! checked conversion, not an `as` cast.

use crate::common;

use common::typecheck_isolated as typecheck;
use hew_types::error::TypeErrorKind;

#[test]
fn char_to_i64_cast_typechecks() {
    let out = typecheck(
        r"
        fn codepoint(c: char) -> i64 {
            c as i64
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "`char as i64` should typecheck as a codepoint cast, got: {:?}",
        out.errors
    );
}

#[test]
fn char_to_lossless_integer_width_typechecks() {
    for target in ["i32", "i64", "u32", "u64", "isize", "usize"] {
        let source = format!(
            r"
            fn f(c: char) -> {target} {{
                c as {target}
            }}
        "
        );
        let out = typecheck(&source);
        assert!(
            out.errors.is_empty(),
            "`char as {target}` should typecheck, got: {:?}",
            out.errors
        );
    }
}

#[test]
fn string_index_char_cast_typechecks() {
    // The dogfood pattern: `s[i]` yields a `char`; `as i64` turns it into the
    // codepoint a text/byte parser consumes.
    let out = typecheck(
        r"
        fn first_codepoint(s: string) -> i64 {
            s[0] as i64
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "`s[i] as i64` should typecheck, got: {:?}",
        out.errors
    );
}

#[test]
fn integer_to_char_cast_rejected() {
    // The reverse direction is NOT an `as` cast: not every integer is a valid
    // scalar value, so it must stay fail-closed.
    let out = typecheck(
        r"
        fn f(n: i64) -> char {
            n as char
        }
    ",
    );
    assert!(
        out.errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })
                && e.message.contains("try_to_char")),
        "`i64 as char` must be rejected, got: {:?}",
        out.errors
    );
}

#[test]
fn char_to_float_cast_rejected() {
    // The codepoint cast only targets integers; `char -> float` requires the
    // intermediate `c as u32 as f64`.
    let out = typecheck(
        r"
        fn f(c: char) -> f64 {
            c as f64
        }
    ",
    );
    assert!(
        out.errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })
                && e.message.contains("cannot cast")),
        "`char as f64` must be rejected, got: {:?}",
        out.errors
    );
}

#[test]
fn narrow_char_casts_require_checked_integer_conversion() {
    for target in ["i8", "i16", "u8", "u16"] {
        let out = typecheck(&format!(
            "fn narrow(c: char) -> {target} {{ c as {target} }}"
        ));
        assert!(
            out.errors
                .iter()
                .any(|error| error.message.contains("try_to_")),
            "{target}: {:?}",
            out.errors
        );
    }
}

#[test]
fn string_index_assignment_has_a_specific_diagnostic() {
    for assignment in ["text[0] = 'H'", "text[0..1] = \"H\""] {
        let output = typecheck(&format!(
            "fn main() {{ var text = \"hello\"; {assignment}; }}"
        ));
        assert!(
            output.errors.iter().any(|error| {
                error.kind == TypeErrorKind::StringIndexAssign
                    && error.kind.as_kind_str() == "E_STRING_INDEX_ASSIGN"
            }),
            "{:?}",
            output.errors
        );
    }
}

#[test]
fn string_satisfies_the_scalar_index_bound() {
    let output =
        typecheck("fn accepts<T: Index<i64>>(_value: T) {} fn main() { accepts(\"Aé\"); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}
