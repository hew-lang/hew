#![no_main]

mod support;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Arbitrary)]
struct RecordInput {
    x: i8,
    y: i8,
    use_tuple: bool,
    update_first: bool,
}

impl RecordInput {
    fn to_source(&self) -> String {
        let update = if self.update_first {
            format!("FuzzPoint {{ x: {}, ..base }}", self.x)
        } else {
            format!("FuzzPoint {{ y: {}, ..base }}", self.y)
        };
        let tuple_use = if self.use_tuple {
            "    let _tuple = FuzzPair(out.x, out.y);\n"
        } else {
            ""
        };

        format!(
            r#"type FuzzPoint {{
    x: i32;
    y: i32;
}}

type FuzzPair(i32, i32);

fn fuzz_record_types() -> i32 {{
    let base = FuzzPoint {{ x: {}, y: {} }};
    let out = {update};
{tuple_use}    out.x + out.y
}}
"#,
            self.x, self.y
        )
    }
}

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        support::exercise_parse_check_lower(source);
    }
    if let Ok(input) = RecordInput::arbitrary(&mut Unstructured::new(data)) {
        support::exercise_parse_check_lower(&input.to_source());
    }
});
