//! Test runner for the Hew programming language.
//!
//! Discovers `#[test]` functions in `.hew` source files, compiles each as an
//! isolated program via the native compilation pipeline, and reports results
//! with colored output.

pub mod discovery;
pub mod output;
pub mod runner;

pub fn cmd_test(args: &[String]) {
    let mut filter: Option<String> = None;
    let mut use_color = true;
    let mut include_ignored = false;
    let mut paths: Vec<String> = Vec::new();
    let mut i = 0;

    while i < args.len() {
        match args[i].as_str() {
            "--filter" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --filter requires an argument");
                    std::process::exit(1);
                }
                filter = Some(args[i].clone());
            }
            "--no-color" => use_color = false,
            "--include-ignored" => include_ignored = true,
            arg => paths.push(arg.to_string()),
        }
        i += 1;
    }

    // If no paths given, search current directory.
    if paths.is_empty() {
        paths.push(".".to_string());
    }

    // Discover test files and test cases.
    let mut all_tests = Vec::new();
    for path in &paths {
        let p = std::path::Path::new(path);
        if p.is_file() {
            match discovery::discover_tests_in_file(path) {
                Ok(tests) => all_tests.extend(tests),
                Err(e) => {
                    eprintln!("Error: {e}");
                    std::process::exit(1);
                }
            }
        } else {
            match discovery::discover_test_files(path) {
                Ok(files) => {
                    for file in files {
                        match discovery::discover_tests_in_file(&file) {
                            Ok(tests) => all_tests.extend(tests),
                            Err(e) => eprintln!("Warning: {e}"),
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Error: {e}");
                    std::process::exit(1);
                }
            }
        }
    }

    if all_tests.is_empty() {
        eprintln!("No test functions found.");
        std::process::exit(0);
    }

    let summary = runner::run_tests(&all_tests, filter.as_deref(), include_ignored);
    output::print_results(&summary, use_color);

    if summary.failed > 0 {
        std::process::exit(1);
    }
}
