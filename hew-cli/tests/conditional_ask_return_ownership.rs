//! Native memory-safety oracle for conditional actor asks whose bytes argument
//! is also returned. The true path must snapshot the ask payload because the
//! original remains live after resumption; the false path must return the
//! untouched original. A transfer or premature release manifests as a poisoned
//! read, double free, or non-zero sentinel under Darwin's scribbled allocator.

#![cfg(unix)]

mod support;

use support::leak_slope::{compile_to_native, run_under_malloc_scribble};
use support::{describe_output, require_codegen};

const SOURCE: &str = r#"
actor Recipient {
    receive fn take(data: bytes) -> i64 {
        data.len() as i64
    }
}

actor Forwarder {
    let recipient: LocalPid<Recipient>,

    receive fn forward(data: bytes, flag: bool) -> bytes {
        if flag {
            let _ = await recipient.take(data);
        }
        data
    }
}

fn main() -> i64 {
    let recipient = spawn Recipient;
    let forwarder = spawn Forwarder(recipient: recipient);

    let false_len = match await forwarder.forward("false-path".to_bytes(), false) {
        .Ok(data) => data.len() as i64,
        .Err(_) => -100,
    };
    let true_len = match await forwarder.forward("true-path".to_bytes(), true) {
        .Ok(data) => data.len() as i64,
        .Err(_) => -100,
    };

    if false_len == 10 && true_len == 9 { 0 } else { 1 }
}
"#;

#[cfg_attr(not(target_os = "macos"), ignore = "poisoned allocator is macOS-only")]
#[test]
fn conditional_ask_return_bytes_survive_true_and_false_paths() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("conditional-ask-return-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(SOURCE, dir.path(), "conditional_ask_return");
    let output = run_under_malloc_scribble(&bin);

    assert_eq!(
        output.status.code(),
        Some(0),
        "both conditional ask paths must return live bytes under the poisoned allocator:\n{}",
        describe_output(&output)
    );
}
