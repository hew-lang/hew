#![no_main]

mod support;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        support::exercise_parse_check_lower(source);
    }
});
