//! Stage boundaries and source identities exposed by compile inspection.

mod support;

use std::collections::HashSet;
use std::path::Path;
use std::process::{Command, Output};
use support::{hew_binary, repo_root, run_bounded_command, strip_ansi, tempdir};

fn inspect(input: &Path, flags: &[&str]) -> Output {
    let mut command = Command::new(hew_binary());
    command
        .args(["tool", "compile"])
        .args(flags)
        .arg(input)
        .env("HEW_STD", repo_root().join("std"));
    run_bounded_command(command, "compile inspection")
}

fn stdout(output: &Output) -> String {
    String::from_utf8(output.stdout.clone()).expect("UTF-8 dump")
}

fn stderr(output: &Output) -> String {
    strip_ansi(&String::from_utf8_lossy(&output.stderr))
}

#[test]
fn expanded_ast_does_not_require_imported_bodies_to_type_check() {
    let dir = tempdir();
    let directory = dir.path().canonicalize().unwrap();
    let input = directory.join("main.hew");
    let helper = directory.join("helpers.hew");
    let emitted = dir.path().join("emitted");
    std::fs::write(&input, "import helpers; fn main() { helpers.answer(); }").unwrap();
    std::fs::write(&helper, "pub fn answer() -> i64 { true }").unwrap();
    let ast = inspect(
        &input,
        &["--dump-ast", "--emit-dir", emitted.to_str().unwrap()],
    );
    assert!(ast.status.success(), "{}", stderr(&ast));
    let dump = stdout(&ast);
    assert!(dump.contains(helper.to_str().unwrap()), "{dump}");
    assert!(dump.contains("answer"), "{dump}");
    assert!(
        !emitted.exists(),
        "AST inspection must not emit native artefacts"
    );

    for flag in ["--dump-defs", "--dump-hir"] {
        let checked = inspect(&input, &[flag, "--format", "json"]);
        assert!(!checked.status.success(), "{flag} must type-check");
        let diagnostics: serde_json::Value = serde_json::from_slice(&checked.stdout).unwrap();
        assert!(
            diagnostics.as_array().unwrap().iter().any(|diagnostic| {
                diagnostic["file"].as_str() == helper.to_str() && diagnostic["severity"] == "error"
            }),
            "{diagnostics}"
        );
    }
}

#[test]
fn defs_and_hir_stop_before_ownership_validation() {
    let input = repo_root().join("tests/vertical-slice/reject/linear_unconsumed.hew");
    for flag in ["--dump-defs", "--dump-hir"] {
        let output = inspect(&input, &[flag]);
        assert!(output.status.success(), "{flag}: {}", stderr(&output));
        assert!(stdout(&output).contains("Tx"));
    }
    let ownership = inspect(&input, &["--dump-sir", "--format", "json"]);
    assert!(!ownership.status.success());
    let diagnostics: serde_json::Value = serde_json::from_slice(&ownership.stdout).unwrap();
    assert!(
        diagnostics
            .as_array()
            .unwrap()
            .iter()
            .any(|diagnostic| { diagnostic["code"] == "E_MUST_CONSUME" }),
        "{diagnostics}"
    );
}

#[test]
fn hir_links_imported_helpers_actor_bodies_and_collection_sites() {
    let dir = tempdir();
    let directory = dir.path().canonicalize().unwrap();
    let input = directory.join("main.hew");
    let helper = directory.join("helpers.hew");
    let source = r"import helpers;
actor Recorder {
    var values: Vec<i64>;
    receive fn add(value: i64) -> i64 {
        values.push(value);
        values.len()
    }
}
fn main() {
    let recorder = spawn Recorder { values: Vec.new() };
    let _ = recorder.add(helpers.answer());
}
";
    let helper_source =
        "fn hidden(value: i64) -> i64 { value + 1 }\npub fn answer() -> i64 { hidden(40) }\n";
    std::fs::write(&input, source).unwrap();
    std::fs::write(&helper, helper_source).unwrap();
    let output = inspect(&input, &["--dump-hir", "--format", "json"]);
    assert!(output.status.success(), "{}", stderr(&output));
    assert_eq!(
        serde_json::from_slice::<serde_json::Value>(&output.stdout).unwrap(),
        serde_json::json!([])
    );
    let dump = stderr(&output);
    for identity in [
        "helpers.answer",
        "helpers.hidden",
        "Recorder",
        "declaration=DefId(",
    ] {
        assert!(dump.contains(identity), "missing {identity}: {dump}");
    }
    let helper_file = dump
        .lines()
        .find_map(|line| {
            let line = line.trim();
            (line.starts_with("file#") && line.contains(helper.to_str().unwrap()))
                .then(|| line.split_whitespace().next().unwrap().to_string())
        })
        .expect("imported source has a checker file identity");
    let sites: HashSet<_> = dump
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            (line.starts_with('s') && line.contains(" module=") && line.contains(" span="))
                .then(|| line.split_whitespace().next().unwrap())
        })
        .collect();
    let expression_sites: Vec<_> = dump
        .lines()
        .filter_map(|line| {
            let mut words = line.split_whitespace();
            (words.next() == Some("expr")).then(|| words.nth(1).unwrap())
        })
        .collect();
    assert!(!expression_sites.is_empty());
    assert!(
        expression_sites.iter().all(|site| sites.contains(site)),
        "every rendered HIR expression has recorded site facts"
    );
    let mut imported_sites = 0;
    for line in dump.lines() {
        let Some(checked) = line.split(" checked=").nth(1) else {
            continue;
        };
        let (file, span) = checked.split_once(':').unwrap();
        let length = if file == helper_file {
            imported_sites += 1;
            helper_source.len()
        } else if file == "file#0" {
            source.len()
        } else {
            continue;
        };
        let (start, end) = span.split_once("..").unwrap();
        let (start, end): (usize, usize) = (start.parse().unwrap(), end.parse().unwrap());
        assert!(
            start <= end && end <= length,
            "out-of-source checked identity: {line}"
        );
    }
    assert!(
        imported_sites > 0,
        "helper sites must retain their file identity"
    );
}

#[test]
fn inspection_rejects_conflicting_stages_and_reports_parse_failures() {
    let dir = tempdir();
    let input = dir.path().join("bad.hew");
    std::fs::write(&input, "fn main( {").unwrap();
    let conflicting = inspect(&input, &["--dump-ast", "--dump-hir"]);
    assert_eq!(conflicting.status.code(), Some(2));
    let parse = inspect(&input, &["--dump-ast", "--format", "json"]);
    assert!(!parse.status.success());
    let diagnostics: serde_json::Value = serde_json::from_slice(&parse.stdout).unwrap();
    assert!(
        diagnostics.as_array().unwrap().iter().any(|diagnostic| {
            diagnostic["file"].as_str() == input.to_str() && diagnostic["severity"] == "error"
        }),
        "{diagnostics}"
    );
}
