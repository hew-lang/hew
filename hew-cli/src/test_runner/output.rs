//! Output formatting for test results.
//!
//! Supports coloured text (default) and `JUnit` XML for CI integration.

#[cfg(test)]
use super::runner::TestFailureKind;
use super::runner::{TestEvent, TestOutcome, TestSummary};
use std::io::Write as _;

/// Output format for test results.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Human-readable coloured text (default).
    Text,
    /// Newline-delimited machine-readable test events.
    Json,
    /// `JUnit` XML for CI systems.
    Junit,
}

/// ANSI colour codes.
struct Colors {
    green: &'static str,
    red: &'static str,
    yellow: &'static str,
    #[cfg(test)]
    bold: &'static str,
    reset: &'static str,
}

const COLORS: Colors = Colors {
    green: "\x1b[32m",
    red: "\x1b[31m",
    yellow: "\x1b[33m",
    #[cfg(test)]
    bold: "\x1b[1m",
    reset: "\x1b[0m",
};

const NO_COLORS: Colors = Colors {
    green: "",
    red: "",
    yellow: "",
    #[cfg(test)]
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
        OutputFormat::Text => render_stream_summary(summary, use_color, invocation_root),
        OutputFormat::Json => {
            serde_json::json!({
                "event": "run_finished",
                "passed": summary.passed,
                "failed": summary.failed,
                "ignored": summary.ignored,
            })
            .to_string()
                + "\n"
        }
        OutputFormat::Junit => render_junit(summary, invocation_root),
    };
    print!("{rendered}");
}

pub fn run_started(tests: usize, format: OutputFormat) {
    match format {
        OutputFormat::Json => println!(
            "{}",
            serde_json::json!({ "event": "run_started", "tests": tests })
        ),
        OutputFormat::Text => println!("hew test ({tests} tests)"),
        OutputFormat::Junit => {}
    }
    let _ = std::io::stdout().flush();
}

pub fn output_event(
    event: TestEvent,
    format: OutputFormat,
    root: &std::path::Path,
    use_color: bool,
    show_output: bool,
) {
    use serde_json::json;
    match (format, event) {
        (
            OutputFormat::Json,
            TestEvent::FileCompiled {
                file,
                tests,
                diagnostics,
            },
        ) => {
            println!(
                "{}",
                json!({ "event": "file_compiled", "file": file, "tests": tests, "ok": diagnostics.is_none(), "diagnostics": diagnostics })
            );
        }
        (OutputFormat::Json, TestEvent::TestStarted(test)) => {
            println!(
                "{}",
                json!({ "event": "test_started", "identity": super::test_identity(&test, root), "selector": super::test_selector(&test) })
            );
        }
        (OutputFormat::Json, TestEvent::TestFinished(result)) => {
            let (outcome, kind, message) = match &result.outcome {
                TestOutcome::Passed => ("passed", None, None),
                TestOutcome::Ignored(_) => ("ignored", None, None),
                TestOutcome::Failed(failure) => (
                    "failed",
                    Some(failure.kind.as_str()),
                    Some(failure.message.as_str()),
                ),
            };
            let reason = match &result.outcome {
                TestOutcome::Ignored(reason) => Some(reason.as_str()),
                _ => None,
            };
            println!(
                "{}",
                json!({ "event": "test_finished", "identity": super::test_identity(&result.test, root), "selector": super::test_selector(&result.test), "outcome": outcome, "kind": kind, "message": message, "reason": reason, "duration_ms": result.duration.as_millis(), "output": result.output, "report": result.report })
            );
        }
        (
            OutputFormat::Text,
            TestEvent::FileCompiled {
                file,
                diagnostics: Some(message),
                ..
            },
        ) => {
            println!("FAIL  {file} (compile)\n{message}");
        }
        (OutputFormat::Text, TestEvent::TestFinished(result)) => {
            let c = if use_color { &COLORS } else { &NO_COLORS };
            let (status, detail) = match &result.outcome {
                TestOutcome::Passed => (format!("{}ok{}", c.green, c.reset), None),
                TestOutcome::Ignored(reason) => (
                    format!("{}skip{}", c.yellow, c.reset),
                    Some(reason.as_str()),
                ),
                TestOutcome::Failed(failure) => (
                    format!("{}FAIL{}", c.red, c.reset),
                    Some(failure.message.as_str()),
                ),
            };
            let elapsed = (result.duration.as_millis() > 100)
                .then(|| format!("  {} ms", result.duration.as_millis()))
                .unwrap_or_default();
            println!(
                "{status}  {}{elapsed}",
                super::test_identity(&result.test, root)
            );
            if let Some(detail) =
                detail.filter(|_| matches!(&result.outcome, TestOutcome::Failed(_)))
            {
                println!("{detail}");
                if !result.output.is_empty() {
                    print!("output:\n{}", result.output);
                }
            } else if let TestOutcome::Ignored(reason) = &result.outcome {
                println!("  {reason}");
            } else if show_output && !result.output.is_empty() {
                print!("output:\n{}", result.output);
            }
        }
        _ => {}
    }
    let _ = std::io::stdout().flush();
}

fn render_stream_summary(summary: &TestSummary, use_color: bool, root: &std::path::Path) -> String {
    let c = if use_color { &COLORS } else { &NO_COLORS };
    let mut out = String::new();
    let result_word = if summary.failed > 0 {
        format!("{}FAILED{}", c.red, c.reset)
    } else {
        format!("{}ok{}", c.green, c.reset)
    };
    let _ = writeln!(
        out,
        "\n{result_word}  {} passed, {} failed, {} ignored",
        summary.passed, summary.failed, summary.ignored
    );
    if summary.failed > 0 {
        out.push_str("failures:\n");
        for file in &summary.compile_failures {
            let _ = writeln!(out, "  {} (compile): {}", file.file, file.message);
        }
        for result in &summary.results {
            if let TestOutcome::Failed(failure) = &result.outcome {
                let _ = writeln!(
                    out,
                    "  {}: {}",
                    super::test_identity(&result.test, root),
                    failure.message
                );
            }
        }
        out.push_str("rerun failures: hew test --rerun-failed\n");
    }
    let mut slowest = summary
        .results
        .iter()
        .filter(|result| result.duration.as_millis() > 100)
        .collect::<Vec<_>>();
    slowest.sort_unstable_by(|left, right| right.duration.cmp(&left.duration));
    if !slowest.is_empty() {
        out.push_str("slowest:\n");
        for result in slowest.into_iter().take(5) {
            let _ = writeln!(
                out,
                "  {}  {} ms",
                super::test_identity(&result.test, root),
                result.duration.as_millis()
            );
        }
    }
    out
}

use std::fmt::Write as _;

/// Render test results as coloured text.
#[must_use]
#[cfg(test)]
pub fn render_results(summary: &TestSummary, use_color: bool) -> String {
    let c = if use_color { &COLORS } else { &NO_COLORS };
    let total = summary.results.len()
        + summary
            .compile_failures
            .iter()
            .map(|failure| failure.tests.len())
            .sum::<usize>();
    let mut out = String::new();

    let _ = writeln!(out, "\nrunning {total} tests");

    for result in &summary.results {
        let status = match &result.outcome {
            TestOutcome::Passed => format!("{}ok{}", c.green, c.reset),
            TestOutcome::Failed(_) => format!("{}FAILED{}", c.red, c.reset),
            TestOutcome::Ignored(_) => format!("{}ignored{}", c.yellow, c.reset),
        };
        let _ = writeln!(out, "test {} ... {status}", result.test.name);
    }
    for failure in &summary.compile_failures {
        let _ = writeln!(out, "file {} ... {}FAILED{}", failure.file, c.red, c.reset);
    }

    // Print failure details.
    let failures: Vec<_> = summary
        .results
        .iter()
        .filter(|r| matches!(r.outcome, TestOutcome::Failed(_)))
        .collect();

    if !failures.is_empty() || !summary.compile_failures.is_empty() {
        out.push_str("\nfailures:\n\n");
        for failure in &summary.compile_failures {
            let _ = writeln!(out, "---- {} (compile) ----", failure.file);
            let _ = writeln!(out, "selected tests: {}", failure.tests.join(", "));
            let _ = writeln!(out, "{}\n", failure.message);
        }
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
        "test result: {result_word}. {} passed; {} failed; {} ignored",
        summary.passed, summary.failed, summary.ignored,
    );
    let not_run = summary
        .compile_failures
        .iter()
        .map(|failure| failure.tests.len())
        .sum::<usize>();
    if not_run > 0 {
        let _ = write!(out, "; {not_run} not run after file compilation failed");
    }
    out.push_str("\n\n");

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
    for failure in &summary.compile_failures {
        suites.entry(failure.file.as_str()).or_default();
    }

    let total = summary.passed + summary.failed + summary.ignored;
    let total_time: f64 = summary
        .results
        .iter()
        .map(|r| r.duration.as_secs_f64())
        .sum::<f64>()
        + summary
            .compile_failures
            .iter()
            .map(|failure| failure.duration.as_secs_f64())
            .sum::<f64>();

    let mut out = String::new();
    writeln!(out, r#"<?xml version="1.0" encoding="UTF-8"?>"#).unwrap();
    writeln!(
        out,
        r#"<testsuites name="hew test" tests="{total}" failures="{}" skipped="{}" time="{total_time:.3}">"#,
        summary.failed, summary.ignored,
    )
    .unwrap();

    for (file, results) in &suites {
        let compile_failure = summary
            .compile_failures
            .iter()
            .find(|failure| failure.file == *file);
        let classname = junit_classname(file, invocation_root);
        let suite_tests = results.len() + usize::from(compile_failure.is_some());
        let suite_failures = results
            .iter()
            .filter(|r| matches!(r.outcome, TestOutcome::Failed(_)))
            .count()
            + usize::from(compile_failure.is_some());
        let suite_skipped = results
            .iter()
            .filter(|r| matches!(r.outcome, TestOutcome::Ignored(_)))
            .count();
        let suite_time: f64 = results
            .iter()
            .map(|r| r.duration.as_secs_f64())
            .sum::<f64>()
            + compile_failure.map_or(0.0, |failure| failure.duration.as_secs_f64());

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

            if let Some(seed) = result
                .report
                .as_ref()
                .and_then(|report| report.seed.as_deref())
            {
                writeln!(
                    out,
                    "      <properties><property name=\"seed\" value=\"{}\"/></properties>",
                    xml_escape(seed)
                )
                .unwrap();
            }

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
                TestOutcome::Ignored(_) => {
                    writeln!(out, "      <skipped/>").unwrap();
                }
            }

            writeln!(out, "    </testcase>").unwrap();
        }
        if let Some(failure) = compile_failure {
            let detail = format!(
                "selected tests: {}\n{}",
                failure.tests.join(", "),
                failure.message
            );
            writeln!(
                out,
                r#"    <testcase name="&lt;compile&gt;" classname="{}" time="{:.3}">"#,
                xml_escape(&classname),
                failure.duration.as_secs_f64(),
            )
            .unwrap();
            writeln!(
                out,
                r#"      <failure type="compile" message="{}">{}</failure>"#,
                xml_escape(&detail),
                xml_escape(&detail),
            )
            .unwrap();
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
                    ignore_reason: None,
                    should_panic: false,
                    should_panic_message: None,
                    timeout_ns: None,
                    serial: false,
                    clock: crate::test_runner::discovery::TestClock::Deterministic,
                    doc: None,
                },
                outcome: TestOutcome::Passed,
                output: String::new(),
                duration: std::time::Duration::from_millis(42),
                report: None,
            }],
            passed: 1,
            failed: 0,
            ignored: 0,
            compile_failures: Vec::new(),
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
                    ignore_reason: None,
                    should_panic: false,
                    should_panic_message: None,
                    timeout_ns: None,
                    serial: false,
                    clock: crate::test_runner::discovery::TestClock::Deterministic,
                    doc: None,
                },
                outcome: TestOutcome::failed(TestFailureKind::Runtime, "assertion failed"),
                output: "debug line".into(),
                duration: std::time::Duration::from_millis(13),
                report: None,
            }],
            passed: 0,
            failed: 1,
            ignored: 0,
            compile_failures: Vec::new(),
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
                        ignore_reason: None,
                        should_panic: false,
                        should_panic_message: None,
                        timeout_ns: None,
                        serial: false,
                        clock: crate::test_runner::discovery::TestClock::Deterministic,
                        doc: None,
                    },
                    outcome: TestOutcome::Passed,
                    output: String::new(),
                    duration: std::time::Duration::from_millis(100),
                    report: None,
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
                        ignore_reason: None,
                        should_panic: false,
                        should_panic_message: None,
                        timeout_ns: None,
                        serial: false,
                        clock: crate::test_runner::discovery::TestClock::Deterministic,
                        doc: None,
                    },
                    outcome: TestOutcome::failed(TestFailureKind::Runtime, "expected 4, got 5"),
                    output: "debug output".into(),
                    duration: std::time::Duration::from_millis(50),
                    report: None,
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
                        ignore_reason: None,
                        should_panic: false,
                        should_panic_message: None,
                        timeout_ns: None,
                        serial: false,
                        clock: crate::test_runner::discovery::TestClock::Deterministic,
                        doc: None,
                    },
                    outcome: TestOutcome::Ignored("ignored".to_string()),
                    output: String::new(),
                    duration: std::time::Duration::ZERO,
                    report: None,
                },
            ],
            passed: 1,
            failed: 1,
            ignored: 1,
            compile_failures: Vec::new(),
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
