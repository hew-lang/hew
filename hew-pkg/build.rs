//! Stamps the rustc identity used to build the Hew runtime.

use std::env;
use std::process::Command;

fn main() {
    // Cargo may use a rustc other than the one on PATH.
    let rustc = env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
    println!("cargo:rerun-if-env-changed=RUSTC");

    let output = Command::new(&rustc)
        .arg("-vV")
        .output()
        .unwrap_or_else(|e| panic!("failed to run `{rustc} -vV`: {e}"));
    assert!(
        output.status.success(),
        "`{rustc} -vV` failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let text = String::from_utf8(output.stdout)
        .unwrap_or_else(|e| panic!("`{rustc} -vV` produced non-UTF-8 output: {e}"));

    let release = field(&text, "release").unwrap_or_else(|| {
        panic!("`{rustc} -vV` output has no `release:` line:\n{text}");
    });
    let host = field(&text, "host").unwrap_or_else(|| {
        panic!("`{rustc} -vV` output has no `host:` line:\n{text}");
    });

    println!("cargo:rustc-env=HEW_RUNTIME_RUSTC={release} {host}");
}

/// Extract the value of a `<name>: <value>` line from `rustc -vV` output.
fn field(text: &str, name: &str) -> Option<String> {
    let prefix = format!("{name}: ");
    text.lines()
        .find_map(|line| line.strip_prefix(&prefix))
        .map(str::trim)
        .map(str::to_string)
}
