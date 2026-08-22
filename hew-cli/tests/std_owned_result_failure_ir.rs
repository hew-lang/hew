//! Failure paths must discharge raw stdlib FFI owners before returning or panicking.
//!
//! These wrappers validate a fresh opaque handle before wrapping it in a Hew
//! resource. A failed validation must therefore read thread-local error detail
//! first, then consume the raw handle exactly once. The structural oracle is
//! deterministic even on hosts where the runtime represents failure as null:
//! removing any cleanup call reproduces the pre-fix owner-debt shape.

mod support;

use std::process::Command;

use tempfile::tempdir;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE: &str = r#"
import std.process;
import std.time.cron;

fn main() {
    match process.try_run("printf ok") {
        Ok(output) => println(output.stdout),
        Err(_) => (),
    }
    let run_args: Vec<string> = Vec.new();
    match process.try_run_argv("printf", run_args) {
        Ok(output) => println(output.stdout),
        Err(_) => (),
    }
    match process.try_start("true") {
        Ok(child) => println(child.wait()),
        Err(_) => (),
    }
    let spawn_args: Vec<string> = Vec.new();
    match process.try_start_argv("true", spawn_args) {
        Ok(child) => println(child.wait()),
        Err(_) => (),
    }
    let parsed = cron.parse("* * * * *");
    parsed.close();
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

fn assert_failure_cleanup(
    ir: &str,
    symbol: &str,
    detail_call: &str,
    release_call: &str,
    success_close: Option<&str>,
) {
    let body = function_body(ir, symbol);
    let detail = body
        .find(detail_call)
        .unwrap_or_else(|| panic!("{symbol} must preserve failure detail:\n{body}"));
    let release = body
        .find(release_call)
        .unwrap_or_else(|| panic!("{symbol} must consume its failed raw handle:\n{body}"));

    assert!(
        detail < release,
        "{symbol} must preserve failure detail before consuming the handle:\n{body}"
    );
    assert_eq!(
        body.matches(release_call).count(),
        1,
        "{symbol} must consume the failed raw handle exactly once:\n{body}"
    );
    if let Some(success_close) = success_close {
        assert_eq!(
            body.matches(success_close).count(),
            0,
            "{symbol} must not close the valid success-arm resource before returning it:\n{body}"
        );
    }
}

#[test]
fn stdlib_raw_owned_failure_handles_are_released_once_after_detail_is_copied() {
    require_codegen();
    let dir = tempdir().expect("temporary emit directory");
    let source = dir.path().join("owned_failure.hew");
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

    let ir =
        std::fs::read_to_string(dir.path().join("owned_failure.ll")).expect("read emitted LLVM IR");

    assert_failure_cleanup(
        &ir,
        "define internal %\"Result$$std$mprocess$mCommandOutput$std$mprocess$mProcessError\" @\"std$process$try_run\"",
        "@\"std$process$last_process_error\"",
        "call void @hew_process_result_free",
        None,
    );
    assert_failure_cleanup(
        &ir,
        "define internal %\"Result$$std$mprocess$mCommandOutput$std$mprocess$mProcessError\" @\"std$process$try_run_argv\"",
        "@\"std$process$last_process_error\"",
        "call void @hew_process_result_free",
        None,
    );
    assert_failure_cleanup(
        &ir,
        "define internal %std.process.Child @\"std$process$start\"",
        "@\"std$process$last_process_error\"",
        "call void @hew_process_drop",
        Some("@\"std.process.Child::close\""),
    );
    assert_failure_cleanup(
        &ir,
        "define internal %\"Result$$std$mprocess$mChild$std$mprocess$mProcessError\" @\"std$process$try_start\"",
        "@\"std$process$last_process_error\"",
        "call void @hew_process_drop",
        Some("@\"std.process.Child::close\""),
    );
    assert_failure_cleanup(
        &ir,
        "define internal %\"Result$$std$mprocess$mChild$std$mprocess$mProcessError\" @\"std$process$try_start_argv\"",
        "@\"std$process$last_process_error\"",
        "call void @hew_process_drop",
        Some("@\"std.process.Child::close\""),
    );
    assert_failure_cleanup(
        &ir,
        "define internal %std.time.cron.Expr @\"std$time$cron$parse\"",
        "@\"std$time$cron$cron_last_error_message\"",
        "call void @hew_cron_free",
        Some("@\"std.time.cron.Expr::close\""),
    );
}
