#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let prefixed = format!("machine Fuzz {{ {} }} fn main() {{}}", s);
        let _ = hew_parser::parser::parse(&prefixed);
    }
});
