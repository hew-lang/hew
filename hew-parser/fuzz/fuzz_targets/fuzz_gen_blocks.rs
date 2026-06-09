#![no_main]

mod support;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Arbitrary)]
struct GenInput {
    first: i8,
    second: i8,
    form: u8,
}

impl GenInput {
    fn to_source(&self) -> String {
        let first = i64::from(self.first);
        let second = i64::from(self.second);
        let body = match self.form % 4 {
            0 => format!("gen {{ yield {first}; {second} }}"),
            1 => format!("gen {{ yield {first}; yield {second}; () }}"),
            2 => format!("gen {{ return {first}; }}"),
            _ => format!("gen {{ {second} }}"),
        };

        format!(
            r#"fn fuzz_gen_blocks() {{
    let _g = {body};
}}
"#
        )
    }
}

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        support::exercise_parse_check_lower(source);
    }
    if let Ok(input) = GenInput::arbitrary(&mut Unstructured::new(data)) {
        support::exercise_parse_check_lower(&input.to_source());
    }
});
