//! Auto-linking of native (FFI) libraries declared by imported packages.
//!
//! When a Hew program imports a package whose `hew.toml` declares a `[native]`
//! library, `hew build` compiles that Rust staticlib and links it into the
//! final binary — without the user passing `--link-lib` by hand. This module
//! walks the resolved import graph of the type-checked program, finds each
//! imported package directory that declares `[native]`, and (build-on-demand)
//! compiles + locates its staticlib via [`hew_pkg::native::build_native`].
//!
//! Linking only succeeds when the staticlib embeds a byte-identical `libstd` to
//! `libhew.a` (same pinned rustc) and is built `panic = "abort"`; otherwise the
//! final link fails with a duplicate `rust_eh_personality`. A package enforces
//! this with its own `rust-toolchain.toml` + `[profile]`, which `cargo` honours
//! automatically because `build_native` invokes cargo against the package's own
//! manifest directory.

use std::collections::{BTreeSet, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

use hew_parser::ast::{Item, Program, Spanned};

/// Unique package directories (each containing a `hew.toml`) reachable from
/// `program`'s resolved imports, including transitive module imports. Pure-Hew
/// packages are included here and filtered out later by [`build_native_link_libs`],
/// which skips directories with no `[native]` section.
pub fn collect_import_pkg_dirs(program: &Program) -> Vec<PathBuf> {
    let mut dirs = BTreeSet::new();
    // Shared imports retain one immutable body per module, so the walk is
    // bounded by that body's identity. Recursing per import edge instead
    // expands a module once per path that reaches it, and a diamond import
    // graph then costs one filesystem probe per path rather than per module.
    let mut seen: HashSet<*const Vec<Spanned<Item>>> = HashSet::new();
    let mut queue: Vec<&[Spanned<Item>]> = vec![program.items.as_slice()];
    while let Some(items) = queue.pop() {
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
                if seen.insert(Arc::as_ptr(resolved)) {
                    queue.push(resolved.as_slice());
                }
            }
        }
    }
    dirs.into_iter().collect()
}

/// Build (on demand) and return the staticlib paths for every package in `dirs`
/// that declares a `[native]` library. Directories without a `[native]` section
/// yield nothing. Returns an error string if a native crate fails to build.
pub fn build_native_link_libs(dirs: &[PathBuf]) -> Result<Vec<String>, String> {
    let expected = hew_pkg::native::embedded_rustc_identity();
    let mut libs = Vec::new();
    for dir in dirs {
        if let Some(artifact) = hew_pkg::native::build_native(dir, &expected)? {
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
