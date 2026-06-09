#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let tokens = hew_lexer::lex(s);
        let mut last_end = 0;
        for (_, span) in tokens {
            assert!(span.start <= span.end);
            assert!(span.end <= s.len());
            assert!(s.is_char_boundary(span.start));
            assert!(s.is_char_boundary(span.end));
            assert!(span.start >= last_end);
            last_end = span.end;
        }
    }
});
