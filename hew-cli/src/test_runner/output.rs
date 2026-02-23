//! Colored output formatting for test results.

use super::runner::{TestOutcome, TestSummary};

/// ANSI color codes.
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

/// Format and print test results.
pub fn print_results(summary: &TestSummary, use_color: bool) {
    let c = if use_color { &COLORS } else { &NO_COLORS };
    let total = summary.passed + summary.failed + summary.ignored;

    println!("\nrunning {total} tests");

    for result in &summary.results {
        let status = match &result.outcome {
            TestOutcome::Passed => format!("{}ok{}", c.green, c.reset),
            TestOutcome::Failed(_) => format!("{}FAILED{}", c.red, c.reset),
            TestOutcome::Ignored => format!("{}ignored{}", c.yellow, c.reset),
        };
        println!("test {} ... {status}", result.test.name);
    }

    // Print failure details.
    let failures: Vec<_> = summary
        .results
        .iter()
        .filter(|r| matches!(r.outcome, TestOutcome::Failed(_)))
        .collect();

    if !failures.is_empty() {
        println!("\nfailures:\n");
        for result in &failures {
            println!("---- {} ----", result.test.name);
            if let TestOutcome::Failed(msg) = &result.outcome {
                println!("{msg}");
            }
            if !result.output.is_empty() {
                println!("output:\n{}", result.output);
            }
            println!();
        }
    }

    // Summary line.
    let result_word = if summary.failed > 0 {
        format!("{}{}FAILED{}", c.bold, c.red, c.reset)
    } else {
        format!("{}{}ok{}", c.bold, c.green, c.reset)
    };

    println!(
        "test result: {result_word}. {} passed; {} failed; {} ignored\n",
        summary.passed, summary.failed, summary.ignored,
    );
}

#[cfg(test)]
mod tests {
    use super::super::discovery::TestCase;
    use super::super::runner::TestResult;
    use super::*;

    #[test]
    fn print_all_passing() {
        let summary = TestSummary {
            results: vec![TestResult {
                test: TestCase {
                    name: "test_ok".into(),
                    file: "f.hew".into(),
                    ignored: false,
                    should_panic: false,
                },
                outcome: TestOutcome::Passed,
                output: String::new(),
            }],
            passed: 1,
            failed: 0,
            ignored: 0,
        };
        // Just ensure it doesn't panic.
        print_results(&summary, false);
    }

    #[test]
    fn print_with_failure() {
        let summary = TestSummary {
            results: vec![TestResult {
                test: TestCase {
                    name: "test_bad".into(),
                    file: "f.hew".into(),
                    ignored: false,
                    should_panic: false,
                },
                outcome: TestOutcome::Failed("assertion failed".into()),
                output: String::new(),
            }],
            passed: 0,
            failed: 1,
            ignored: 0,
        };
        print_results(&summary, false);
    }
}
