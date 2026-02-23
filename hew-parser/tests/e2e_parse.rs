use std::fs;
use std::path::Path;

#[test]
fn parse_all_examples() {
    let examples_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("examples");
    test_directory(&examples_dir, "examples");
}

#[test]
fn parse_all_codegen_examples() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("hew-codegen")
        .join("tests")
        .join("examples");
    test_directory(&dir, "hew-codegen/tests/examples");
}

fn test_directory(dir: &Path, label: &str) {
    let mut passed = 0;
    let mut failed = 0;
    let mut errors = Vec::new();

    let mut entries: Vec<_> = fs::read_dir(dir)
        .unwrap()
        .filter_map(Result::ok)
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|e| e == "hew"))
        .collect();
    entries.sort();

    for path in &entries {
        let source = fs::read_to_string(path).unwrap();
        let result = hew_parser::parse(&source);
        if result.errors.is_empty() {
            passed += 1;
        } else {
            failed += 1;
            let first_err = &result.errors[0];
            errors.push(format!(
                "  {} — {}",
                path.file_name().unwrap().to_string_lossy(),
                first_err.message
            ));
        }
    }

    let total = passed + failed;
    println!("\n{label}: {passed} passed, {failed} failed out of {total}");
    if !errors.is_empty() {
        println!("Failures:");
        for e in &errors {
            println!("{e}");
        }
    }
    assert!(
        passed >= failed,
        "{label}: too many parse failures ({passed} pass, {failed} fail)"
    );
}
