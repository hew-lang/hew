//! `hew symbolize` on real `hew build -g` binaries at O0 and O2: a Hew
//! function's address resolves to its source file and line, an address no
//! symbols cover prints `??`, and a binary from another build is refused.
//!
//! Windows images carry no COFF symbol table to find the probe address in;
//! the PDB path is covered by `hew-symbolize`'s own tests.
#![cfg(not(windows))]

mod support;

use std::path::Path;
use std::process::Command;

use object::{Object, ObjectSymbol};
use support::{hew_binary, require_codegen, tempdir};

const SOURCE: &str = "\
fn level2(x: i64) -> i64 {
    let y = x * 3;
    y + 1
}

fn main() -> i64 {
    println(level2(4));
    0
}
";

/// The file address of `level2`, as `nm` prints it.
fn level2_address(binary: &Path) -> u64 {
    let data = std::fs::read(binary).expect("read binary");
    let file = object::File::parse(&*data).expect("parse binary");
    file.symbols()
        .find(|symbol| {
            symbol
                .name()
                .is_ok_and(|name| name.ends_with("__hew_fn_level2"))
        })
        .expect("level2 has a symbol")
        .address()
}

fn build(dir: &Path, release: bool) -> std::path::PathBuf {
    let src = dir.join("p.hew");
    std::fs::write(&src, SOURCE).expect("write source");
    let binary = hew_testutil::compiled_binary_path(dir, "p");
    let mut command = Command::new(hew_binary());
    command.arg("build").arg("-g");
    if release {
        command.arg("--release");
    }
    let output = command
        .arg(&src)
        .arg("-o")
        .arg(&binary)
        .output()
        .expect("run hew build");
    assert!(
        output.status.success(),
        "hew build failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    binary
}

fn symbolize(binary: &Path, args: &[String]) -> std::process::Output {
    Command::new(hew_binary())
        .arg("symbolize")
        .arg(binary)
        .args(args)
        .output()
        .expect("run hew symbolize")
}

fn names_level2_in_source(release: bool) {
    require_codegen();
    let dir = tempdir();
    let binary = build(dir.path(), release);
    let start = format!("{:#x}", level2_address(&binary));
    let output = symbolize(&binary, std::slice::from_ref(&start));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(stdout.contains("level2 at "), "{stdout}");
    assert!(stdout.contains("p.hew:1"), "{stdout}");

    // Far outside the image: no symbols cover it.
    let output = symbolize(&binary, &["0x7fff00000000".into()]);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "0x7fff00000000 ??\n"
    );

    // A frame recorded by another build is refused, not misattributed. The
    // binary may have no build id of its own (a FreeBSD link without
    // --build-id), so only the refusal and the expected id are fixed.
    let output = symbolize(&binary, &[start, "--build-id=00112233".into()]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(1), "{stderr}");
    assert!(stderr.contains("00112233"), "{stderr}");
}

#[test]
fn debug_build_resolves_a_hew_function_to_its_source() {
    names_level2_in_source(false);
}

#[test]
fn optimized_debug_build_resolves_a_hew_function_to_its_source() {
    names_level2_in_source(true);
}
