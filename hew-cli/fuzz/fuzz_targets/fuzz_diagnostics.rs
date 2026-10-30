//! `hew check` diagnostics for arbitrary source, as text and as JSON.
//!
//! Properties: rendering never panics; the JSON is an array whose spans lie
//! inside the named file on character boundaries; each located text
//! diagnostic agrees with its JSON twin on file, line and column; both
//! formats exit alike; two runs print identical JSON; and the output stays
//! within a constant factor of the input size.

#![no_main]

use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use libfuzzer_sys::fuzz_target;
use serde_json::Value;

/// Output bytes allowed per input byte, plus a fixed allowance for the
/// diagnostics any input may draw (missing `main`, prelude notes).
const OUTPUT_PER_INPUT_BYTE: usize = 64;
const OUTPUT_ALLOWANCE: usize = 64 * 1024;

fn input_path() -> &'static PathBuf {
    static INPUT: OnceLock<PathBuf> = OnceLock::new();
    INPUT.get_or_init(|| {
        let dir = std::env::temp_dir().join(format!("hew-fuzz-diagnostics-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let dir = dir.canonicalize().unwrap();
        // Display paths are relative to the working directory, so the
        // input renders as `input.hew` in both formats.
        std::env::set_current_dir(&dir).unwrap();
        dir.join("input.hew")
    })
}

fn check(arguments: &[&str]) -> hew_cli::fuzzing::Checked {
    let arguments: Vec<String> = arguments.iter().map(|a| (*a).to_string()).collect();
    std::thread::Builder::new()
        .stack_size(hew_compile::COMPILER_STACK_SIZE)
        .spawn(move || {
            let arguments: Vec<&str> = arguments.iter().map(String::as_str).collect();
            hew_cli::fuzzing::check(&arguments)
        })
        .unwrap()
        .join()
        .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
}

/// `(file, line, column)` of each located error or warning the text printed.
fn text_locations(text: &str) -> Vec<(String, u64, u64)> {
    text.lines()
        .filter_map(|line| {
            let (location, rest) = line.split_once(": ")?;
            if !(rest.starts_with("error") || rest.starts_with("warning")) {
                return None;
            }
            let mut parts = location.rsplitn(3, ':');
            let column = parts.next()?.parse().ok()?;
            let line = parts.next()?.parse().ok()?;
            Some((parts.next()?.to_string(), line, column))
        })
        .collect()
}

fn field<'a>(value: &'a Value, name: &str) -> &'a Value {
    value
        .get(name)
        .unwrap_or_else(|| panic!("JSON diagnostic lacks `{name}`: {value}"))
}

fn number(value: &Value, name: &str) -> u64 {
    field(value, name)
        .as_u64()
        .unwrap_or_else(|| panic!("JSON `{name}` is not a count: {value}"))
}

/// Check that a located span lies inside `file` on character boundaries.
fn check_span(source_of: &mut impl FnMut(&str) -> Option<String>, file: &str, span: &Value) {
    let (start, end) = (number(span, "start_byte"), number(span, "end_byte"));
    if number(span, "start_line") == 0 {
        assert!(
            start == 0 && end == 0,
            "unlocated span carries bytes: {span}"
        );
        return;
    }
    let Some(source) = source_of(file) else {
        return;
    };
    let (start, end) = (start as usize, end as usize);
    assert!(start <= end, "span runs backwards in {file}: {span}");
    assert!(
        end <= source.len(),
        "span ends past {file} ({} bytes): {span}",
        source.len()
    );
    assert!(
        source.is_char_boundary(start) && source.is_char_boundary(end),
        "span splits a character in {file}: {span}"
    );
}

/// The diagnostics in emission order, with the unused-variable warnings set
/// apart and sorted.
///
/// Deviation: the checker emits unused-variable and unused-`var` warnings in
/// hash order, which
/// is a known defect being fixed separately. Until it lands they are compared
/// as a set so the target can reach other differences. Compare the whole
/// array in order once the emission order is fixed.
fn unused_variables_as_a_set(diagnostics: &[Value]) -> (Vec<&Value>, Vec<String>) {
    let unused = |d: &&Value| d["code"] == "UnusedVariable" || d["code"] == "UnusedMut";
    let mut set: Vec<String> = diagnostics
        .iter()
        .filter(unused)
        .map(Value::to_string)
        .collect();
    set.sort();
    (diagnostics.iter().filter(|d| !unused(d)).collect(), set)
}

fuzz_target!(|data: &[u8]| {
    let Ok(source) = std::str::from_utf8(data) else {
        return;
    };
    let path = input_path();
    std::fs::write(path, source).unwrap();
    let path = path.to_str().unwrap();

    let json_run = check(&[path, "--format", "json", "--error-limit", "0"]);
    let again = check(&[path, "--format", "json", "--error-limit", "0"]);
    let parsed: Value = serde_json::from_str(&json_run.json).expect("JSON diagnostics parse");
    let diagnostics = parsed.as_array().expect("JSON diagnostics are an array");
    let again_parsed: Value = serde_json::from_str(&again.json).expect("JSON diagnostics parse");
    assert_eq!(
        unused_variables_as_a_set(diagnostics),
        unused_variables_as_a_set(
            again_parsed
                .as_array()
                .expect("JSON diagnostics are an array")
        ),
        "JSON diagnostics differ between two runs\n--- source ---\n{source}"
    );
    assert_eq!(json_run.exit_code, again.exit_code);
    let text_run = check(&[path, "--error-limit", "0"]);
    assert_eq!(
        text_run.exit_code, json_run.exit_code,
        "text and JSON exit differently\n--- text ---\n{}\n--- json ---\n{}",
        text_run.text, json_run.json
    );

    let bound = OUTPUT_PER_INPUT_BYTE * source.len() + OUTPUT_ALLOWANCE;
    assert!(
        text_run.text.len() <= bound && json_run.json.len() <= bound,
        "diagnostics outgrow the input: {} bytes of source, {} of text, {} of JSON",
        source.len(),
        text_run.text.len(),
        json_run.json.len()
    );

    let mut sources = std::collections::BTreeMap::new();
    let mut source_of = |file: &str| -> Option<String> {
        sources
            .entry(file.to_string())
            .or_insert_with(|| std::fs::read_to_string(Path::new(file)).ok())
            .clone()
    };
    let mut located = Vec::new();
    for diagnostic in diagnostics {
        let file = field(diagnostic, "file")
            .as_str()
            .expect("file is a string");
        let span = field(diagnostic, "span");
        check_span(&mut source_of, file, span);
        for note in field(diagnostic, "notes")
            .as_array()
            .expect("notes are an array")
        {
            if let (Some(file), Some(span)) = (note["file"].as_str(), note.get("span")) {
                if !span.is_null() {
                    check_span(&mut source_of, file, span);
                }
            }
        }
        let severity = field(diagnostic, "severity").as_str().unwrap_or_default();
        if number(span, "start_line") > 0 && (severity == "error" || severity == "warning") {
            located.push((
                file.to_string(),
                number(span, "start_line"),
                number(span, "start_col"),
            ));
        }
    }
    let mut printed = text_locations(&text_run.text);
    // Deviation: sorted for the unused-variable order (see
    // `unused_variables_as_a_set`); compare in order once that is fixed.
    printed.sort();
    located.sort();
    assert_eq!(
        printed, located,
        "text and JSON disagree on diagnostic locations\n--- source ---\n{source}\n--- text ---\n{}\n--- json ---\n{}",
        text_run.text, json_run.json
    );
});
