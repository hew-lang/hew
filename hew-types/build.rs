//! Build script for hew-types.
//!
//! Emits `rerun-if-changed` directives so Cargo rebuilds when stdlib
//! `.hew` files change. Module discovery and parsing is now handled at
//! user compile time by `ModuleRegistry` / `stdlib_loader`.

use std::path::PathBuf;

fn main() {
    let manifest_dir =
        PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set"));
    let repo_root = manifest_dir
        .parent()
        .expect("hew-types should be in repo root");

    // Emit rerun-if-changed for .hew source directories so Cargo rebuilds
    // when stdlib files change (the ModuleRegistry loads them at compile time).
    println!("cargo:rerun-if-changed={}", repo_root.join("std").display());
    println!(
        "cargo:rerun-if-changed={}",
        repo_root.join("ecosystem").display()
    );
}
