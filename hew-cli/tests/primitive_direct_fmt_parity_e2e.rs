//! A direct `x.fmt()` call on a primitive must render identically to the same
//! value interpolated in an f-string (`f"{x}"`).
//!
//! Both surfaces resolve to the lang-item `impl Display for <primitive>` in
//! `std/builtins.hew` (whose body is `to_string(val)`). Before this was wired,
//! a direct `.fmt()` failed closed at HIR lowering
//! (`CallableUnsupportedInMir` / `E_NOT_YET_IMPLEMENTED`) because the injected
//! builtin Display impl's declaration-to-symbol projection was only established
//! when its body was emitted — after user bodies were lowered — while f-string
//! interpolation never consults that projection. This pins that the two paths
//! agree, per primitive.

#![cfg(unix)]

mod support;

use support::{hew_binary, repo_root, require_codegen};

/// Each primitive prints its direct `.fmt()` result immediately followed by the
/// interpolation of the same value, so an inequality shows up as two differing
/// adjacent lines. A negative int, a signed cast type, floats, bool, char, and
/// string each cover a distinct `lower_display_dispatch` arm
/// (`scalar_display_builtin`, the f32 widening arm, and the string-identity
/// arm).
const PARITY_SOURCE: &str = r#"
fn main() -> i64 {
    let i: i64 = -42;
    let u: u32 = 7;
    let z: isize = -3;
    let f: f64 = 3.5;
    let g: f32 = 1.25;
    let b: bool = true;
    let c: char = 'z';
    let s: string = "hi";
    println(i.fmt());
    println(f"{i}");
    println(u.fmt());
    println(f"{u}");
    println(z.fmt());
    println(f"{z}");
    println(f.fmt());
    println(f"{f}");
    println(g.fmt());
    println(f"{g}");
    println(b.fmt());
    println(f"{b}");
    println(c.fmt());
    println(f"{c}");
    println(s.fmt());
    println(f"{s}");
    0
}
"#;

#[test]
fn direct_primitive_fmt_matches_fstring_interpolation() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("primitive-direct-fmt-parity-")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join("primitive_direct_fmt_parity.hew");
    std::fs::write(&path, PARITY_SOURCE).expect("write parity source");

    let output = std::process::Command::new(hew_binary())
        .arg("run")
        .arg(&path)
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|error| panic!("failed to run hew binary: {error}"));

    assert!(
        output.status.success(),
        "direct primitive `.fmt()` must compile and run; stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        [
            "-42", "-42", // i64
            "7", "7", // u32
            "-3", "-3", // isize
            "3.5", "3.5", // f64
            "1.25", "1.25", // f32 (widened to f64)
            "true", "true", // bool
            "z", "z", // char
            "hi", "hi", // string (identity)
        ],
        "each primitive's direct `.fmt()` must equal its f-string \
         interpolation; stdout:\n{stdout}"
    );
}
