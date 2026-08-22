//! Exact ownership contract for `std::path::{glob,try_glob}` failure paths.
//!
//! `hew_glob` always returns one heap-owned handle, including an expansion
//! failure.  Its failure detail is borrowed from that handle, so each wrapper
//! must read the detail before releasing the handle.  This is intentionally an
//! emitted-IR counterfactual rather than a permission-shaped glob failure:
//! before the repair each function contained zero `hew_glob_free` calls, which
//! makes the regression deterministic on every host (including privileged CI).

mod support;

use std::process::Command;

use tempfile::tempdir;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE: &str = r#"
import std.path;

fn main() {
    match path.try_glob("std/*.hew") {
        Ok(matches) => matches.close(),
        Err(_) => (),
    }
}
"#;

fn function_body<'a>(ir: &'a str, symbol: &str) -> &'a str {
    let start = ir
        .find(symbol)
        .unwrap_or_else(|| panic!("missing {symbol} in emitted IR"));
    let body = &ir[start..];
    let end = body
        .find("\n}")
        .map_or(body.len(), |closing_brace| closing_brace + 2);
    &body[..end]
}

fn assert_one_failure_cleanup(ir: &str, symbol: &str) {
    let body = function_body(ir, symbol);
    let error = body
        .find("@hew_glob_error")
        .unwrap_or_else(|| panic!("{symbol} must read the failure detail:\n{body}"));
    let free = body
        .find("call void @hew_glob_free")
        .unwrap_or_else(|| panic!("{symbol} must free an invalid glob handle:\n{body}"));

    assert!(
        error < free,
        "{symbol} must copy the failure detail before freeing its borrowed handle:\n{body}"
    );
    assert_eq!(
        body.matches("call void @hew_glob_free").count(),
        1,
        "{symbol} must consume the invalid handle exactly once:\n{body}"
    );
    assert_eq!(
        body.matches("@\"GlobResult::close\"").count(),
        0,
        "{symbol} must not close the success-arm GlobResult before transferring it to the caller:\n{body}"
    );
}

#[test]
fn path_glob_failure_handles_are_released_once_after_detail_is_copied() {
    require_codegen();
    let dir = tempdir().expect("temporary emit directory");
    let source = dir.path().join("glob_ownership.hew");
    std::fs::write(&source, SOURCE).expect("write Hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-llvm",
            "--emit-dir",
            dir.path().to_str().expect("emit directory is UTF-8"),
            source.to_str().expect("source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("run hew compile");
    assert!(
        output.status.success(),
        "ownership fixture must compile:\n{}",
        describe_output(&output)
    );

    let ir = std::fs::read_to_string(dir.path().join("glob_ownership.ll"))
        .expect("read emitted LLVM IR");
    assert_one_failure_cleanup(
        &ir,
        "define internal %std.path.GlobResult @\"std$path$glob\"",
    );
    assert_one_failure_cleanup(
        &ir,
        "define internal %\"Result$$std$mpath$mGlobResult$std$mpath$mPathError\" @\"std$path$try_glob\"",
    );
}
