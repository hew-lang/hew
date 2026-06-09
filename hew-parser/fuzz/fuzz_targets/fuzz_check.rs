#![no_main]

mod support;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let parsed = hew_parser::parse(source);
        if parsed
            .errors
            .iter()
            .any(|e| e.severity == hew_parser::Severity::Error)
        {
            return;
        }

        let mut checker = support::checker();
        let _ = checker.check_program(&parsed.program);
    }
});
