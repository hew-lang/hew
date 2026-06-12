#![no_main]

mod support;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Arbitrary)]
struct QualifiedCtorInput {
    code: u8,
    variant: u8,
    match_qualified: bool,
}

impl QualifiedCtorInput {
    fn to_source(&self) -> String {
        let ctor = match self.variant % 4 {
            0 => "fs.IoError::NotFound",
            1 => "fs.IoError::PermissionDenied",
            2 => "fs.IoError::TimedOut",
            _ => "fs.IoError::Other",
        };
        let pattern = if self.match_qualified {
            ctor.replace("fs.", "")
        } else {
            "IoError::Other".to_string()
        };

        format!(
            r#"import std::fs;

fn fuzz_qualified_ctor_resolution() -> i64 {{
    let err = {ctor}({});
    match err {{
        {pattern}(code) => code,
        _ => 0,
    }}
}}
"#,
            self.code
        )
    }
}

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        support::exercise_parse_check_lower(source);
    }
    if let Ok(input) = QualifiedCtorInput::arbitrary(&mut Unstructured::new(data)) {
        support::exercise_parse_check_lower(&input.to_source());
    }
});
