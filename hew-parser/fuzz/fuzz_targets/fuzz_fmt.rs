//! `hew fmt` on arbitrary source.
//!
//! The formatter runs only on source without parse errors, as `hew fmt` does.
//! For such source it must reprint the same program with every comment in
//! place (the fidelity oracle `hew fmt` itself applies), and formatting its
//! own output must change nothing.

#![no_main]

use hew_parser::fmt::{fidelity, format_source};
use hew_parser::Severity;
use libfuzzer_sys::fuzz_target;

fn parse_clean(source: &str) -> Option<hew_parser::ParseResult> {
    let parsed = hew_parser::parse(source);
    (!parsed.errors.iter().any(|e| e.severity == Severity::Error)).then_some(parsed)
}

fuzz_target!(|data: &[u8]| {
    let Ok(source) = std::str::from_utf8(data) else {
        return;
    };
    let Some(parsed) = parse_clean(source) else {
        return;
    };
    let formatted = format_source(source, &parsed.program);
    if let Err(error) = fidelity::check(source, &formatted) {
        panic!(
            "formatter is unfaithful: {error}\n--- source ---\n{source}\n--- formatted ---\n{formatted}"
        );
    }
    let reparsed = parse_clean(&formatted).expect("fidelity check admits only parse-clean output");
    let again = format_source(&formatted, &reparsed.program);
    assert_eq!(
        again, formatted,
        "formatting is not a fixed point\n--- source ---\n{source}"
    );
});
