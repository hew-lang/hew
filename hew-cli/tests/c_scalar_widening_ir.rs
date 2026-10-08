//! Narrow C scalars cross every extern boundary widened.
//!
//! A release-built runtime reads a `bool` or `u8` argument from its whole
//! register, so a declaration without `zeroext`/`signext` lets an O2 caller
//! pass stale upper bits. The emitted IR is checked directly because the
//! debug runtime masks the value itself and never shows the fault.

mod support;

use std::path::Path;
use std::process::Command;

use tempfile::tempdir;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE: &str = r#"
extern "C" {
    fn widening_probe(a: bool, b: u8, c: i8, d: u16, e: i16, f: i32) -> u8;
    fn widening_truth() -> bool;
}

fn main() {
    let flag = 3 > 2;
    println(flag);
    println(f"{flag}");
    var data = bytes::new();
    data.push(7);
    let narrow = unsafe { widening_probe(flag, 1, -1, 2, -2, 3) };
    let truth = unsafe { widening_truth() };
    println(f"{narrow} {truth} {data.len()}");
}
"#;

fn emit_ir(source: &str) -> String {
    require_codegen();
    let dir = tempdir().expect("temporary emit directory");
    let path = dir.path().join("widening.hew");
    std::fs::write(&path, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-llvm",
            "--emit-dir",
            dir.path().to_str().expect("emit directory is UTF-8"),
            path.to_str().expect("source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("run hew compile");
    assert!(
        output.status.success(),
        "widening fixture must compile:\n{}",
        describe_output(&output)
    );
    std::fs::read_to_string(dir.path().join("widening.ll")).expect("read emitted LLVM IR")
}

fn declaration<'a>(ir: &'a str, symbol: &str) -> &'a str {
    let needle = format!("@{symbol}(");
    ir.lines()
        .find(|line| line.starts_with("declare ") && line.contains(&needle))
        .unwrap_or_else(|| panic!("missing declaration of {symbol} in emitted IR"))
}

/// Split a declaration's parameter list, keeping each parameter's attributes.
fn parameters(declaration: &str) -> Vec<&str> {
    let open = declaration.find('(').expect("declaration has parameters");
    let close = declaration
        .rfind(')')
        .expect("declaration closes its parameters");
    let list = &declaration[open + 1..close];
    if list.trim().is_empty() {
        return Vec::new();
    }
    list.split(", ").map(str::trim).collect()
}

/// The register width each narrow LLVM integer needs, by C type.
fn expected_extension(c_type: &str) -> Option<&'static str> {
    match c_type {
        "bool" | "u8" | "u16" | "c_uchar" | "c_ushort" => Some("zeroext"),
        "i8" | "i16" | "c_char" | "c_schar" | "c_short" => Some("signext"),
        _ => None,
    }
}

#[test]
fn narrow_extern_arguments_carry_their_c_widening() {
    let ir = emit_ir(SOURCE);

    assert_eq!(
        declaration(&ir, "hew_bool_to_string"),
        "declare ptr @hew_bool_to_string(i8 zeroext)"
    );
    assert_eq!(
        declaration(&ir, "widening_probe"),
        "declare i8 @widening_probe(i8 zeroext, i8 zeroext, i8 signext, i16 zeroext, i16 signext, i32)",
        "a narrow extern return stays unmarked: the caller masks it"
    );
    assert_eq!(
        declaration(&ir, "widening_truth"),
        "declare i8 @widening_truth()"
    );
    assert!(
        ir.lines()
            .any(|line| line.contains("call ptr @hew_bool_to_string(i8 zeroext %")),
        "the call site carries the declaration's widening:\n{ir}"
    );
}

#[test]
fn every_runtime_declaration_widens_as_its_rust_signature_requires() {
    let ir = emit_ir(SOURCE);
    let surface: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(Path::new(repo_root()).join("scripts/cabi-surface.json"))
            .expect("read the C ABI surface"),
    )
    .expect("parse the C ABI surface");
    let mut checked = 0;
    for line in ir.lines().filter(|line| line.starts_with("declare ")) {
        let Some(symbol) = line
            .split('@')
            .nth(1)
            .and_then(|rest| rest.split('(').next())
        else {
            continue;
        };
        let Some(row) = surface["functions"]
            .as_array()
            .expect("surface functions")
            .iter()
            .find(|row| row["symbol"] == symbol)
        else {
            continue;
        };
        let signature = row["signature"]["native"]
            .as_str()
            .expect("native signature");
        let open = signature.find('(').expect("signature parameters");
        let close = signature.rfind(')').expect("signature closes");
        let rust: Vec<&str> = signature[open + 1..close]
            .split(',')
            .map(str::trim)
            .filter(|ty| !ty.is_empty())
            .collect();
        let llvm = parameters(line);
        assert_eq!(rust.len(), llvm.len(), "{symbol}: {signature} vs {line}");
        for (c_type, param) in rust.iter().zip(&llvm) {
            if let Some(extension) = expected_extension(c_type) {
                assert!(
                    param.contains(extension),
                    "{symbol}: `{c_type}` parameter must be {extension}: {line}"
                );
                checked += 1;
            }
        }
    }
    assert!(
        checked >= 3,
        "the fixture must exercise narrow runtime parameters, checked {checked}"
    );
}
