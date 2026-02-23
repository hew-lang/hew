#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Only try valid UTF-8 strings
    if let Ok(s) = std::str::from_utf8(data) {
        // This should never panic — only return errors
        let _ = hew_parser::parser::parse(s);
    }
});
