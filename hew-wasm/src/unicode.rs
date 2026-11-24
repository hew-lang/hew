//! Narrow scalar-codepoint (`u32`) Unicode classification and case-mapping
//! bridge matching `hew-runtime`'s pinned Rust `char` + `finl_unicode` tables.

use finl_unicode::categories::CharacterCategories;
use wasm_bindgen::prelude::*;

const BIT_DIGIT: u32 = 1 << 0;
const BIT_LETTER: u32 = 1 << 1;
const BIT_ALPHANUMERIC: u32 = 1 << 2;
const BIT_WHITESPACE: u32 = 1 << 3;
const BIT_UPPER: u32 = 1 << 4;
const BIT_LOWER: u32 = 1 << 5;
const BIT_PUNCTUATION: u32 = 1 << 6;
const BIT_CONTROL: u32 = 1 << 7;
const BIT_ASCII: u32 = 1 << 8;

/// Return the 9-bit property bitmask for Unicode scalar `cp` (matching
/// `hew-runtime`'s `hew_char_is_*` predicates), or `0` for non-scalars.
#[wasm_bindgen(js_name = charProperties)]
#[must_use]
pub fn char_properties(cp: u32) -> u32 {
    let Some(ch) = char::from_u32(cp) else {
        return 0;
    };
    let mut bits = 0u32;
    let is_digit = ch.is_ascii_digit();
    let is_letter = ch.is_alphabetic();
    if is_digit {
        bits |= BIT_DIGIT;
    }
    if is_letter {
        bits |= BIT_LETTER;
    }
    if is_letter || is_digit {
        bits |= BIT_ALPHANUMERIC;
    }
    if ch.is_whitespace() {
        bits |= BIT_WHITESPACE;
    }
    if ch.is_uppercase() {
        bits |= BIT_UPPER;
    }
    if ch.is_lowercase() {
        bits |= BIT_LOWER;
    }
    if CharacterCategories::is_punctuation(ch) {
        bits |= BIT_PUNCTUATION;
    }
    if ch.is_control() {
        bits |= BIT_CONTROL;
    }
    if cp <= 0x7f {
        bits |= BIT_ASCII;
    }
    bits
}

/// Convert `cp` using its one-scalar case mapping, or return `cp` unchanged
/// for expanding mappings, unmapped scalars, or non-scalars.
fn map_single_scalar<I: Iterator<Item = char>>(cp: u32, mut mapping: I) -> u32 {
    match (mapping.next(), mapping.next()) {
        (Some(mapped), None) => u32::from(mapped),
        _ => cp,
    }
}

/// Convert `cp` to its one-scalar uppercase equivalent, or return `cp`
/// unchanged when no one-scalar mapping exists.
#[wasm_bindgen(js_name = charToUpper)]
#[must_use]
pub fn char_to_upper(cp: u32) -> u32 {
    char::from_u32(cp).map_or(cp, |ch| map_single_scalar(cp, ch.to_uppercase()))
}

/// Convert `cp` to its one-scalar lowercase equivalent, or return `cp`
/// unchanged when no one-scalar mapping exists.
#[wasm_bindgen(js_name = charToLower)]
#[must_use]
pub fn char_to_lower(cp: u32) -> u32 {
    char::from_u32(cp).map_or(cp, |ch| map_single_scalar(cp, ch.to_lowercase()))
}

/// Convert `cp` to its one-scalar titlecase mapping, matching
/// `hew_char_to_title` in `hew-runtime`.
#[wasm_bindgen(js_name = charToTitle)]
#[must_use]
pub fn char_to_title(cp: u32) -> u32 {
    match cp {
        0x1c4..=0x1c6 => 0x1c5,
        0x1c7..=0x1c9 => 0x1c8,
        0x1ca..=0x1cc => 0x1cb,
        0x1f1..=0x1f3 => 0x1f2,
        0x10d0..=0x10fa | 0x10fd..=0x10ff => cp,
        0x1f80..=0x1f87 | 0x1f90..=0x1f97 | 0x1fa0..=0x1fa7 => cp + 8,
        0x1fb3 => 0x1fbc,
        0x1fc3 => 0x1fcc,
        0x1ff3 => 0x1ffc,
        _ => char_to_upper(cp),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_surrogates_and_out_of_range_codepoints() {
        for invalid in [0xd800, 0xdfff, 0x11_0000, u32::MAX] {
            assert_eq!(char_properties(invalid), 0, "U+{invalid:X}");
            assert_eq!(char_to_upper(invalid), invalid, "U+{invalid:X}");
            assert_eq!(char_to_lower(invalid), invalid, "U+{invalid:X}");
            assert_eq!(char_to_title(invalid), invalid, "U+{invalid:X}");
        }
    }

    #[test]
    fn exhaustive_scalar_parity_across_unicode_range() {
        for cp in 0u32..=0x10_ffff {
            let bits = char_properties(cp);
            let upper = char_to_upper(cp);
            let lower = char_to_lower(cp);
            let title = char_to_title(cp);
            let Some(ch) = char::from_u32(cp) else {
                assert_eq!(bits, 0);
                assert_eq!(upper, cp);
                assert_eq!(lower, cp);
                assert_eq!(title, cp);
                continue;
            };
            assert_eq!(bits & BIT_DIGIT != 0, ch.is_ascii_digit());
            assert_eq!(bits & BIT_LETTER != 0, ch.is_alphabetic());
            assert_eq!(
                bits & BIT_ALPHANUMERIC != 0,
                ch.is_alphabetic() || ch.is_ascii_digit()
            );
            assert_eq!(bits & BIT_WHITESPACE != 0, ch.is_whitespace());
            assert_eq!(bits & BIT_UPPER != 0, ch.is_uppercase());
            assert_eq!(bits & BIT_LOWER != 0, ch.is_lowercase());
            assert_eq!(
                bits & BIT_PUNCTUATION != 0,
                CharacterCategories::is_punctuation(ch)
            );
            assert_eq!(bits & BIT_CONTROL != 0, ch.is_control());
            assert_eq!(bits & BIT_ASCII != 0, cp <= 0x7f);
            assert!(char::from_u32(upper).is_some());
            assert!(char::from_u32(lower).is_some());
            assert!(char::from_u32(title).is_some());
        }
    }

    #[test]
    fn titlecase_digraphs_and_multi_scalar_expansions() {
        assert_eq!(char_to_title(0x01c6), 0x01c5); // ǆ -> ǅ
        assert_eq!(char_to_title(0x01f3), 0x01f2); // ǳ -> ǲ
        assert_eq!(char_to_title(0x1f80), 0x1f88); // ᾀ -> ᾈ
        assert_eq!(char_to_upper(0x00df), 0x00df); // ß -> SS expands (>1 scalar) -> unchanged
        assert_eq!(char_to_lower(0x0130), 0x0130); // İ -> i\u{307} expands (>1 scalar) -> unchanged
        assert!(char_properties(u32::from('5')) & BIT_DIGIT != 0);
        assert_eq!(char_properties(u32::from('٥')) & BIT_DIGIT, 0);
        assert!(char_properties(u32::from('—')) & BIT_PUNCTUATION != 0);
    }
}
