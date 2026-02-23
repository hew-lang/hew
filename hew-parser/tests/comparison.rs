use std::fs;
use std::path::Path;

fn lex_directory(dir: &Path) -> (usize, usize, Vec<(String, usize)>) {
    let mut passed = 0;
    let mut failed = 0;
    let mut errors = Vec::new();

    let mut entries: Vec<_> = fs::read_dir(dir).unwrap().filter_map(Result::ok).collect();
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        if path.extension().map_or(true, |e| e != "hew") {
            continue;
        }
        let source = fs::read_to_string(&path).unwrap();
        let tokens = hew_lexer::lex(&source);
        let error_count = tokens
            .iter()
            .filter(|(t, _)| matches!(t, hew_lexer::Token::Error))
            .count();
        if error_count == 0 {
            passed += 1;
        } else {
            failed += 1;
            errors.push((
                path.file_name().unwrap().to_string_lossy().to_string(),
                error_count,
            ));
        }
    }

    (passed, failed, errors)
}

#[test]
fn lex_all_examples() {
    let examples_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("examples");
    let (passed, failed, errors) = lex_directory(&examples_dir);

    println!(
        "Lexer comparison (examples/): {passed} passed, {failed} failed out of {}",
        passed + failed
    );
    for (file, count) in &errors {
        println!("  FAIL: {file} ({count} error tokens)");
    }
    assert!(
        passed > failed,
        "More files should lex successfully than fail"
    );
}

#[test]
fn lex_all_codegen_examples() {
    let codegen_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("hew-codegen")
        .join("tests")
        .join("examples");
    let (passed, failed, errors) = lex_directory(&codegen_dir);

    println!(
        "Lexer comparison (hew-codegen/tests/examples/): {passed} passed, {failed} failed out of {}",
        passed + failed
    );
    for (file, count) in &errors {
        println!("  FAIL: {file} ({count} error tokens)");
    }
    assert!(
        passed > failed,
        "More files should lex successfully than fail"
    );
}
