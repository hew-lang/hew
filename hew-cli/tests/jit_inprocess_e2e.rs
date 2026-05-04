mod support;

use std::process::Command;

use support::{hew_binary, require_codegen};

/// `hew eval --jit inprocess "1 + 1"` must print `2` and exit 0.
///
/// This test was the primary failure mode before this fix: the hew binary
/// had no dependency on hew-runtime, so all 653 kStableJitHostSymbols
/// C-ABI functions were absent from its dynamic export table, causing
/// `DynamicLibrarySearchGenerator::GetForCurrentProcess` to report
/// "Symbols not found: [ `_hew_print_value` ]".
#[test]
fn jit_inprocess_simple_expression_succeeds() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", "--jit", "inprocess", "1 + 1"])
        .output()
        .expect("failed to spawn hew binary");

    assert!(
        output.status.success(),
        "hew eval --jit inprocess exited with non-zero status\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "2\n",
        "expected stdout to be '2\\n'"
    );
}

/// `hew eval --jit auto "1 + 1"` must print `2` and exit 0.
///
/// `auto` selects `inprocess` on platforms where the embedded codegen
/// backend is present, so this exercises the same symbol-export path as
/// `inprocess`.
#[test]
fn jit_auto_simple_expression_succeeds() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", "--jit", "auto", "1 + 1"])
        .output()
        .expect("failed to spawn hew binary");

    assert!(
        output.status.success(),
        "hew eval --jit auto exited with non-zero status\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "2\n",
        "expected stdout to be '2\\n'"
    );
}

/// The release binary must not SIGABRT on `hew eval --jit inprocess`.
///
/// Before the symbol-export fix, the release binary aborted silently (exit
/// 134, SIGABRT) because `panic=abort` in the release profile prevents
/// `catch_unwind` from intercepting the symbol-not-found panic.  The
/// symbol-export fix itself is confirmed working — the debug binary prints
/// `2` and the release binary now carries the same 716 exported symbols.
///
/// This test was previously `#[ignore]`'d pending two follow-on fixes:
///
/// 1. UAF in `hew-codegen/src/jit_session.cpp:207-223`: `HewJitSymbolMap
///    symbolMap` was a block-local captured by reference into a
///    `DynamicLibrarySearchGenerator` that outlived its scope, causing
///    `EXC_BAD_ACCESS` in the release binary.  Fixed in 3520dfbe by hoisting
///    `symbolMap` to function scope.
///
/// 2. `strip = true` stripping `__SYMTAB` — this was a misdiagnosis.
///    Runtime-symbol resolution uses `#[used]` anchors combined with
///    `DynamicLibrarySearchGenerator::GetForCurrentProcess`, which resolves
///    symbols via the dynamic linker rather than `__SYMTAB`.  A freshly-built
///    release binary (strip active, zero exported `T _hew_` symbols) passes
///    this test without any profile changes.
#[test]
fn jit_inprocess_release_binary_does_not_abort() {
    require_codegen();

    // Locate the release binary relative to the debug binary path.
    // hew_binary() returns .../target/debug/hew; release is at
    // .../target/release/hew.
    let debug_bin = hew_binary();
    let release_bin = debug_bin
        .parent() // debug/
        .and_then(|p| p.parent()) // target/
        .map(|p| {
            p.join("release")
                .join(debug_bin.file_name().expect("binary has a file name"))
        });

    let release_bin = match release_bin {
        Some(path) if path.exists() => path,
        _ => {
            eprintln!(
                "skipping jit_inprocess_release_binary_does_not_abort: \
                 release binary not found (run `cargo build --release -p hew-cli` to enable)"
            );
            return;
        }
    };

    let output = Command::new(&release_bin)
        .args(["eval", "--jit", "inprocess", "1 + 1"])
        .output()
        .expect("failed to spawn release hew binary");

    assert!(
        output.status.success(),
        "hew eval --jit inprocess (release) exited with non-zero status\n\
         exit code: {:?}\nstdout: {}\nstderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "2\n",
        "expected stdout to be '2\\n'"
    );
}
