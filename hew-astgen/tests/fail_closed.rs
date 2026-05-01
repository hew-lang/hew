/// Integration tests verifying that hew-astgen exits with a clear diagnostic
/// rather than emitting silently-broken C++ for unsupported input forms.
///
/// Each test spawns the binary, feeds it a minimal Rust source containing
/// the unsupported construct, and asserts that the process exits non-zero.
/// This exercises the `process::exit(1)` paths that cannot be reached from
/// unit tests without killing the test runner.
use std::fs;
use std::path::PathBuf;
use std::process::Command;

/// Build-time path to the compiled hew-astgen binary.
fn bin_path() -> PathBuf {
    // CARGO_BIN_EXE_hew_astgen is set by cargo test when the crate declares
    // a binary target.
    PathBuf::from(env!("CARGO_BIN_EXE_hew-astgen"))
}

/// A minimal, valid Rust source that declares no serialisable types.
/// Used as the --module argument when the test only cares about --ast.
const EMPTY_MODULE: &str = "// empty";

/// Write `content` to a temp file with the given suffix and return its path.
fn write_tmp(dir: &std::path::Path, name: &str, content: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, content).expect("write temp file");
    path
}

#[test]
fn unsupported_tuple_arity_in_struct_field_exits_nonzero() {
    // A struct with a 3-tuple field cannot be mapped to C++.
    // hew-astgen should detect this at code-generation time and exit(1)
    // rather than emitting `/* unsupported tuple */` as a C++ type string.
    let source = r"
use serde::Serialize;

#[derive(Serialize)]
pub struct ThreeTuple {
    pub coords: (i64, i64, i64),
}
";

    let tmp = tempdir();
    let ast_file = write_tmp(tmp.path(), "ast.rs", source);
    let module_file = write_tmp(tmp.path(), "module.rs", EMPTY_MODULE);
    let output_file = tmp.path().join("out.cpp");

    let status = Command::new(bin_path())
        .args([
            "--ast",
            ast_file.to_str().unwrap(),
            "--module",
            module_file.to_str().unwrap(),
            "--output",
            output_file.to_str().unwrap(),
        ])
        .status()
        .expect("hew-astgen binary should be present");

    assert!(
        !status.success(),
        "hew-astgen should exit non-zero for a struct with a 3-tuple field"
    );
}

#[test]
fn vec_of_unsupported_tuple_arity_exits_nonzero() {
    // A Vec<(A, B, C)> field wraps a 3-tuple, which cannot be mapped to C++.
    // The cpp_type abort fires when the Vec element type is resolved, before
    // the parse-expression is generated.  The binary should exit non-zero
    // rather than emitting a comment string as the C++ type.
    let source = r"
use serde::Serialize;

#[derive(Serialize)]
pub struct ThreeVec {
    pub entries: Vec<(String, u64, bool)>,
}
";

    let tmp = tempdir();
    let ast_file = write_tmp(tmp.path(), "ast.rs", source);
    let module_file = write_tmp(tmp.path(), "module.rs", EMPTY_MODULE);
    let output_file = tmp.path().join("out.cpp");

    let status = Command::new(bin_path())
        .args([
            "--ast",
            ast_file.to_str().unwrap(),
            "--module",
            module_file.to_str().unwrap(),
            "--output",
            output_file.to_str().unwrap(),
        ])
        .status()
        .expect("hew-astgen binary should be present");

    assert!(
        !status.success(),
        "hew-astgen should exit non-zero for a Vec of an unsupported tuple arity"
    );
}

/// Returns a temporary directory that cleans itself up when dropped.
fn tempdir() -> TempDir {
    let base = std::env::temp_dir();
    let name = format!(
        "hew-astgen-test-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .subsec_nanos()
    );
    let path = base.join(name);
    fs::create_dir_all(&path).expect("create temp dir");
    TempDir(path)
}

struct TempDir(PathBuf);

impl TempDir {
    fn path(&self) -> &std::path::Path {
        &self.0
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
