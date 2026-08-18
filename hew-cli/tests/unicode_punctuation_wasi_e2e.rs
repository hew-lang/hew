//! Exhaustive Hew-surface punctuation oracle executed as real WASI WebAssembly.
//!
//! The runtime unit test proves the native Rust export. This test compiles a
//! Hew program that calls `std::text::unicode.is_punct` for every Unicode code
//! point, runs that program through wasmtime, and compares the complete set to
//! the pinned Unicode 17 category tables.

mod support;

use finl_unicode::categories::{CharacterCategories, MinorCategory};
use std::fs;
use support::{repo_root, require_wasi_runner, run_hew_in};

#[test]
fn hew_wasi_unicode_is_punct_matches_all_unicode_17_punctuation_scalars() {
    require_wasi_runner();

    let dir = tempfile::tempdir().expect("create punctuation oracle directory");
    let source = dir.path().join("punctuation_oracle.hew");
    fs::write(
        &source,
        r"import std.text.unicode;

fn main() {
    for cp in 0 .. 1114112 {
        if unicode.is_punct(cp) {
            println(cp);
        }
    }
}
",
    )
    .expect("write Hew punctuation oracle");

    let source = source.to_str().expect("oracle path is UTF-8");
    let output = run_hew_in(repo_root(), &["run", source, "--target", "wasm32-wasi"]);
    let stdout = String::from_utf8(output.stdout).expect("oracle stdout is UTF-8");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "Hew WASI punctuation oracle failed\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );

    let actual: Vec<u32> = stdout
        .lines()
        .map(|line| {
            line.parse::<u32>()
                .unwrap_or_else(|err| panic!("invalid codepoint {line:?}: {err}"))
        })
        .collect();
    let expected: Vec<u32> = (0..=0x10_ffff_u32)
        .filter_map(|cp| {
            let ch = char::from_u32(cp)?;
            matches!(
                ch.get_minor_category(),
                MinorCategory::Pc
                    | MinorCategory::Pd
                    | MinorCategory::Ps
                    | MinorCategory::Pe
                    | MinorCategory::Pi
                    | MinorCategory::Pf
                    | MinorCategory::Po
            )
            .then_some(cp)
        })
        .collect();

    assert_eq!(expected.len(), 856, "Unicode 17 P* population moved");
    assert_eq!(
        actual, expected,
        "Hew's actual wasm surface disagrees with the exhaustive Unicode 17 P* oracle"
    );
}
