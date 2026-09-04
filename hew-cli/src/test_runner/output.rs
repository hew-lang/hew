//! Output formatting for test results.
//!
//! Supports coloured text (default) and `JUnit` XML for CI integration.

#[cfg(test)]
use super::runner::TestFailureKind;
use super::runner::{TestOutcome, TestSummary};

/// Output format for test results.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Human-readable coloured text (default).
    Text,
    /// `JUnit` XML for CI systems.
    Junit,
}

/// ANSI colour codes.
struct Colors {
    green: &'static str,
    red: &'static str,
    yellow: &'static str,
    bold: &'static str,
    reset: &'static str,
}

const COLORS: Colors = Colors {
    green: "\x1b[32m",
    red: "\x1b[31m",
    yellow: "\x1b[33m",
    bold: "\x1b[1m",
    reset: "\x1b[0m",
};

const NO_COLORS: Colors = Colors {
    green: "",
    red: "",
    yellow: "",
    bold: "",
    reset: "",
};

/// Format and output test results in the specified format.
pub fn output_results(
    summary: &TestSummary,
    use_color: bool,
    format: OutputFormat,
    invocation_root: &std::path::Path,
) {
    let rendered = match format {
        OutputFormat::Text => render_results(summary, use_color),
        OutputFormat::Junit => render_junit(summary, invocation_root),
    };
    print!("{rendered}");
}

use std::fmt::Write as _;

/// Render test results as coloured text.
#[must_use]
pub fn render_results(summary: &TestSummary, use_color: bool) -> String {
    let c = if use_color { &COLORS } else { &NO_COLORS };
    let total = summary.passed + summary.failed + summary.ignored;
    let mut out = String::new();

    let _ = writeln!(out, "\nrunning {total} tests");

    for result in &summary.results {
        let status = match &result.outcome {
            TestOutcome::Passed => format!("{}ok{}", c.green, c.reset),
            TestOutcome::Failed(_) => format!("{}FAILED{}", c.red, c.reset),
            TestOutcome::Ignored => format!("{}ignored{}", c.yellow, c.reset),
        };
        let _ = writeln!(out, "test {} ... {status}", result.test.name);
    }

    // Print failure details.
    let failures: Vec<_> = summary
        .results
        .iter()
        .filter(|r| matches!(r.outcome, TestOutcome::Failed(_)))
        .collect();

    if !failures.is_empty() {
        out.push_str("\nfailures:\n\n");
        for result in &failures {
            let _ = writeln!(out, "---- {} ----", result.test.name);
            if let TestOutcome::Failed(failure) = &result.outcome {
                out.push_str(&failure.message);
                out.push('\n');
            }
            if !result.output.is_empty() {
                out.push_str("output:\n");
                out.push_str(&result.output);
                if !result.output.ends_with('\n') {
                    out.push('\n');
                }
            }
            out.push('\n');
        }
    }

    // Summary line.
    let result_word = if summary.failed > 0 {
        format!("{}{}FAILED{}", c.bold, c.red, c.reset)
    } else {
        format!("{}{}ok{}", c.bold, c.green, c.reset)
    };

    let _ = write!(
        out,
        "test result: {result_word}. {} passed; {} failed; {} ignored\n\n",
        summary.passed, summary.failed, summary.ignored,
    );

    out
}

/// Print test results as `JUnit` XML to stdout.
///
/// Produces a `<testsuites>` document with one `<testsuite>` per source file.
/// Compatible with Jenkins, GitHub Actions (`mikepenz/action-junit-report`),
/// and other `JUnit` XML consumers.
fn render_junit(summary: &TestSummary, invocation_root: &std::path::Path) -> String {
    use std::collections::BTreeMap;
    use std::fmt::Write as _;

    // Group results by source file for testsuite elements.
    let mut suites: BTreeMap<&str, Vec<&super::runner::TestResult>> = BTreeMap::new();
    for result in &summary.results {
        suites
            .entry(result.test.file.as_str())
            .or_default()
            .push(result);
    }

    let total = summary.passed + summary.failed + summary.ignored;
    let total_time: f64 = summary
        .results
        .iter()
        .map(|r| r.duration.as_secs_f64())
        .sum();

    let mut out = String::new();
    writeln!(out, r#"<?xml version="1.0" encoding="UTF-8"?>"#).unwrap();
    writeln!(
        out,
        r#"<testsuites name="hew test" tests="{total}" failures="{}" skipped="{}" time="{total_time:.3}">"#,
        summary.failed, summary.ignored,
    )
    .unwrap();

    for (file, results) in &suites {
        let classname = junit_classname(file, invocation_root);
        let suite_tests = results.len();
        let suite_failures = results
            .iter()
            .filter(|r| matches!(r.outcome, TestOutcome::Failed(_)))
            .count();
        let suite_skipped = results
            .iter()
            .filter(|r| matches!(r.outcome, TestOutcome::Ignored))
            .count();
        let suite_time: f64 = results.iter().map(|r| r.duration.as_secs_f64()).sum();

        writeln!(
            out,
            r#"  <testsuite name="{}" tests="{suite_tests}" failures="{suite_failures}" skipped="{suite_skipped}" time="{suite_time:.3}">"#,
            xml_escape(&classname),
        )
        .unwrap();

        for result in results {
            let time = result.duration.as_secs_f64();
            writeln!(
                out,
                r#"    <testcase name="{}" classname="{}" time="{time:.3}">"#,
                xml_escape(&result.test.name),
                xml_escape(&classname),
            )
            .unwrap();

            match &result.outcome {
                TestOutcome::Passed => {}
                TestOutcome::Failed(failure) => {
                    writeln!(
                        out,
                        r#"      <failure type="{}" message="{}">{}</failure>"#,
                        failure.kind.as_str(),
                        xml_escape(&failure.message),
                        xml_escape(&failure.message),
                    )
                    .unwrap();
                    if !result.output.is_empty() {
                        writeln!(
                            out,
                            "      <system-out>{}</system-out>",
                            xml_escape(&result.output),
                        )
                        .unwrap();
                    }
                }
                TestOutcome::Ignored => {
                    writeln!(out, "      <skipped/>").unwrap();
                }
            }

            writeln!(out, "    </testcase>").unwrap();
        }

        writeln!(out, "  </testsuite>").unwrap();
    }

    writeln!(out, "</testsuites>").unwrap();
    out
}

/// Keep test identities stable across checkout locations and CI runners.
fn junit_classname(file: &str, invocation_root: &std::path::Path) -> String {
    let path = std::path::Path::new(file);
    path.strip_prefix(invocation_root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

/// Strip ANSI escape sequences from a string.
fn strip_ansi(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            // Skip until 'm' (SGR terminator) or end of string.
            for esc_c in chars.by_ref() {
                if esc_c == 'm' {
                    break;
                }
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Escape XML special characters and replace characters forbidden by XML 1.0.
///
/// Test programs can write arbitrary control bytes. Their lossy UTF-8 decoding
/// still preserves characters such as NUL and vertical tab, which are invalid
/// in XML even when they appear as text rather than markup.
fn xml_escape(s: &str) -> String {
    let stripped = strip_ansi(s);
    let mut escaped = String::with_capacity(stripped.len());
    for character in stripped.chars() {
        if !is_xml_1_0_character(character) {
            escaped.push('\u{fffd}');
            continue;
        }
        match character {
            '&' => escaped.push_str("&amp;"),
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '"' => escaped.push_str("&quot;"),
            '\'' => escaped.push_str("&apos;"),
            _ => escaped.push(character),
        }
    }
    escaped
}

fn is_xml_1_0_character(character: char) -> bool {
    matches!(
        character,
        '\u{9}' | '\u{a}' | '\u{d}'
            | '\u{20}'..='\u{d7ff}'
            | '\u{e000}'..='\u{fffd}'
            | '\u{10000}'..='\u{10ffff}'
    )
}

#[cfg(test)]
mod tests {
    use super::super::discovery::TestCase;
    use super::super::runner::TestResult;
    use super::*;

    #[test]
    fn render_all_passing() {
        let summary = TestSummary {
            results: vec![TestResult {
                test: TestCase {
                    name: "test_ok".into(),
                    file: "f.hew".into(),
                    occurrence: hew_types::DeclarationOccurrence::new(
                        None,
                        &(0..0),
                        hew_types::DeclarationKind::Function,
                        0,
                    ),
                    companion: None,
                    ignored: false,
                    should_panic: false,
                    serial: false,
                },
                outcome: TestOutcome::Passed,
                output: String::new(),
                duration: std::time::Duration::from_millis(42),
            }],
            passed: 1,
            failed: 0,
            ignored: 0,
        };
        let rendered = render_results(&summary, false);
        assert!(rendered.contains("running 1 tests"));
        assert!(rendered.contains("test test_ok ... ok"));
        assert!(rendered.contains("1 passed; 0 failed; 0 ignored"));
    }

    #[test]
    fn render_with_failure_details() {
        let summary = TestSummary {
            results: vec![TestResult {
                test: TestCase {
                    name: "test_bad".into(),
                    file: "f.hew".into(),
                    occurrence: hew_types::DeclarationOccurrence::new(
                        None,
                        &(0..0),
                        hew_types::DeclarationKind::Function,
                        0,
                    ),
                    companion: None,
                    ignored: false,
                    should_panic: false,
                    serial: false,
                },
                outcome: TestOutcome::failed(TestFailureKind::Runtime, "assertion failed"),
                output: "debug line".into(),
                duration: std::time::Duration::from_millis(13),
            }],
            passed: 0,
            failed: 1,
            ignored: 0,
        };
        let rendered = render_results(&summary, false);
        assert!(rendered.contains("test test_bad ... FAILED"));
        assert!(rendered.contains("---- test_bad ----"));
        assert!(rendered.contains("assertion failed"));
        assert!(rendered.contains("output:\ndebug line"));
    }

    #[test]
    fn junit_output_contains_xml_structure() {
        let summary = TestSummary {
            results: vec![
                TestResult {
                    test: TestCase {
                        name: "test_pass".into(),
                        file: "math_test.hew".into(),
                        occurrence: hew_types::DeclarationOccurrence::new(
                            None,
                            &(0..0),
                            hew_types::DeclarationKind::Function,
                            0,
                        ),
                        companion: None,
                        ignored: false,
                        should_panic: false,
                        serial: false,
                    },
                    outcome: TestOutcome::Passed,
                    output: String::new(),
                    duration: std::time::Duration::from_millis(100),
                },
                TestResult {
                    test: TestCase {
                        name: "test_fail".into(),
                        file: "math_test.hew".into(),
                        occurrence: hew_types::DeclarationOccurrence::new(
                            None,
                            &(0..0),
                            hew_types::DeclarationKind::Function,
                            0,
                        ),
                        companion: None,
                        ignored: false,
                        should_panic: false,
                        serial: false,
                    },
                    outcome: TestOutcome::failed(TestFailureKind::Runtime, "expected 4, got 5"),
                    output: "debug output".into(),
                    duration: std::time::Duration::from_millis(50),
                },
                TestResult {
                    test: TestCase {
                        name: "test_skip".into(),
                        file: "other_test.hew".into(),
                        occurrence: hew_types::DeclarationOccurrence::new(
                            None,
                            &(0..0),
                            hew_types::DeclarationKind::Function,
                            0,
                        ),
                        companion: None,
                        ignored: true,
                        should_panic: false,
                        serial: false,
                    },
                    outcome: TestOutcome::Ignored,
                    output: String::new(),
                    duration: std::time::Duration::ZERO,
                },
            ],
            passed: 1,
            failed: 1,
            ignored: 1,
        };
        let rendered = render_junit(&summary, std::path::Path::new("."));
        assert!(
            rendered.contains(r#"<testsuites name="hew test" tests="3" failures="1" skipped="1""#)
        );
        assert!(rendered
            .contains(r#"<testsuite name="math_test.hew" tests="2" failures="1" skipped="0""#));
        assert!(rendered.contains(
            r#"<failure type="runtime" message="expected 4, got 5">expected 4, got 5</failure>"#
        ));
        assert!(rendered.contains(r"<system-out>debug output</system-out>"));
        assert!(rendered.contains(r"<skipped/>"));
    }

    #[test]
    fn xml_escape_special_chars() {
        assert_eq!(
            xml_escape(r#"a<b>c&d"e'f"#),
            "a&lt;b&gt;c&amp;d&quot;e&apos;f"
        );
    }

    #[test]
    fn xml_escape_strips_ansi() {
        assert_eq!(xml_escape("\x1b[31mred\x1b[0m text"), "red text");
        assert_eq!(xml_escape("\x1b[1;33mwarn\x1b[0m"), "warn");
    }

    #[test]
    fn xml_escape_replaces_xml_1_0_forbidden_controls() {
        assert_eq!(
            xml_escape("before\0\u{b}after"),
            "before\u{fffd}\u{fffd}after"
        );
        assert_eq!(xml_escape("tab\tline\nreturn\r"), "tab\tline\nreturn\r");
    }

    #[test]
    fn junit_classnames_are_relative_and_portable() {
        assert_eq!(
            junit_classname(
                "/checkout/hew/tests/hew/example_test.hew",
                std::path::Path::new("/checkout/hew"),
            ),
            "tests/hew/example_test.hew",
        );
    }

    #[test]
    fn strip_ansi_codes() {
        assert_eq!(strip_ansi("no codes"), "no codes");
        assert_eq!(strip_ansi("\x1b[32mgreen\x1b[0m"), "green");
        assert_eq!(strip_ansi("\x1b[1m\x1b[31mBOLD RED\x1b[0m"), "BOLD RED");
    }
}
