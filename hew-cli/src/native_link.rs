//! Auto-linking of native (FFI) libraries declared by imported packages.
//!
//! When a Hew program imports a package whose `hew.toml` declares a `[native]`
//! library, `hew build` compiles that Rust staticlib and links it into the
//! final binary — without the user passing `--link-lib` by hand. This module
//! walks the resolved import graph of the type-checked program, finds each
//! imported package directory that declares `[native]`, and (build-on-demand)
//! compiles + locates its staticlib via [`adze_cli::native::build_native`].
//!
//! Linking only succeeds when the staticlib embeds a byte-identical `libstd` to
//! `libhew.a` (same pinned rustc) and is built `panic = "abort"`; otherwise the
//! final link fails with a duplicate `rust_eh_personality`. A package enforces
//! this with its own `rust-toolchain.toml` + `[profile]`, which `cargo` honours
//! automatically because `build_native` invokes cargo against the package's own
//! manifest directory.

use std::collections::BTreeSet;
use std::path::PathBuf;

use hew_parser::ast::{Item, Program, Spanned};

/// Unique package directories (each containing a `hew.toml`) reachable from
/// `program`'s resolved imports, including transitive module imports. Pure-Hew
/// packages are included here and filtered out later by [`build_native_link_libs`],
/// which skips directories with no `[native]` section.
pub fn collect_import_pkg_dirs(program: &Program) -> Vec<PathBuf> {
    let mut dirs = BTreeSet::new();
    collect_items(&program.items, &mut dirs);
    dirs.into_iter().collect()
}

fn collect_items(items: &[Spanned<Item>], dirs: &mut BTreeSet<PathBuf>) {
    for (item, _span) in items {
        let Item::Import(decl) = item else { continue };
        for src in &decl.resolved_source_paths {
            if let Some(parent) = src.parent() {
                if parent.join("hew.toml").is_file() {
                    dirs.insert(parent.to_path_buf());
                }
            }
        }
        if let Some(resolved) = &decl.resolved_items {
            collect_items(resolved, dirs);
        }
    }
}

/// Build (on demand) and return the staticlib paths for every package in `dirs`
/// that declares a `[native]` library. Directories without a `[native]` section
/// yield nothing. Returns an error string if a native crate fails to build.
pub fn build_native_link_libs(dirs: &[PathBuf]) -> Result<Vec<String>, String> {
    let mut libs = Vec::new();
    for dir in dirs {
        if let Some(artifact) = adze_cli::native::build_native(dir)? {
            let path = artifact.path.to_str().ok_or_else(|| {
                format!(
                    "native library path is not valid UTF-8: {}",
                    artifact.path.display()
                )
            })?;
            libs.push(path.to_string());
        }
    }
    Ok(libs)
}
