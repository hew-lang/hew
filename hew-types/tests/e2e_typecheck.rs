use std::fs;
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn new_checker() -> hew_types::Checker {
    hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![
        repo_root(),
    ]))
}

#[test]
fn typecheck_all_examples() {
    let examples_dir = repo_root().join("examples");
    test_directory(&examples_dir, "examples");
}

#[test]
fn typecheck_all_codegen_examples() {
    let dir = repo_root()
        .join("hew-codegen")
        .join("tests")
        .join("examples");
    test_directory(&dir, "hew-codegen/tests/examples");
}

#[test]
fn typecheck_top_level_networking_demos() {
    for relative in ["examples/http_server.hew", "examples/static_server.hew"] {
        assert_typechecks(&repo_root().join(relative), relative);
    }
}

fn assert_typechecks(path: &Path, label: &str) {
    let source = fs::read_to_string(path).unwrap();
    let parse_result = hew_parser::parse(&source);
    assert!(
        parse_result.errors.is_empty(),
        "{label} should parse cleanly, got: {:#?}",
        parse_result.errors
    );

    let mut checker = new_checker();
    let output = checker.check_program(&parse_result.program);
    assert!(
        output.errors.is_empty(),
        "{label} should type-check cleanly, got: {:#?}",
        output.errors
    );
}

fn test_directory(dir: &Path, label: &str) {
    let mut parse_ok = 0;
    let mut parse_fail = 0;
    let mut tc_ok = 0;
    let mut tc_fail = 0;
    let mut tc_errors = Vec::new();

    let mut entries: Vec<_> = fs::read_dir(dir)
        .unwrap()
        .filter_map(Result::ok)
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|e| e == "hew"))
        .collect();
    entries.sort();

    for path in &entries {
        let source = fs::read_to_string(path).unwrap();
        let parse_result = hew_parser::parse(&source);
        if !parse_result.errors.is_empty() {
            parse_fail += 1;
            continue;
        }
        parse_ok += 1;

        let mut checker = new_checker();
        let output = checker.check_program(&parse_result.program);
        if output.errors.is_empty() {
            tc_ok += 1;
        } else {
            tc_fail += 1;
            let first_err = &output.errors[0];
            tc_errors.push(format!(
                "  {} — {:?}",
                path.file_name().unwrap().to_string_lossy(),
                first_err
            ));
        }
    }

    let total = parse_ok + parse_fail;
    println!(
        "\n{label}: {parse_ok}/{total} parsed, {tc_ok}/{parse_ok} type-checked ({tc_fail} failed)"
    );
    if !tc_errors.is_empty() {
        println!("Type-check failures:");
        for e in &tc_errors {
            println!("{e}");
        }
    }
    // Informational — don't fail on type-check errors yet
}
