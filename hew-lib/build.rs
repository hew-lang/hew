//! Stamps the runtime + stdlib build identity into `libhew.a` / `hew.lib`.
//!
//! The driver bakes the same digest in at its own build time and refuses to
//! link an archive whose stamp disagrees, so a freshly built `hew` can never
//! silently pair with a stale archive. See `hew-build-identity` for the format.

use std::path::{Path, PathBuf};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let manifest_dir = PathBuf::from(
        std::env::var_os("CARGO_MANIFEST_DIR").expect("cargo sets CARGO_MANIFEST_DIR"),
    );
    let workspace_root = hew_build_identity::scan::workspace_root_from_manifest_dir(&manifest_dir)
        .unwrap_or_else(|error| panic!("hew-lib build identity: {error}"));

    // Fail closed: an archive that cannot be stamped is an archive nothing can
    // validate, so refuse to produce one rather than emitting a placeholder.
    let identity = hew_build_identity::scan::compute(&workspace_root)
        .unwrap_or_else(|error| panic!("hew-lib build identity: {error}"));
    identity.emit_cargo_rerun_directives();

    let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").expect("cargo sets OUT_DIR"));
    let generated = out_dir.join("build_identity.rs");
    write_if_changed(
        &generated,
        &identity.stamp_static_source(hew_build_identity::STAMP_SYMBOL),
    );

    println!("cargo:rustc-env=HEW_BUILD_IDENTITY={}", identity.digest());
}

/// Avoids rewriting the generated file when the digest is unchanged, so an
/// unrelated rerun does not force a recompile of `hew-lib`.
fn write_if_changed(path: &Path, contents: &str) {
    if std::fs::read_to_string(path).is_ok_and(|existing| existing == contents) {
        return;
    }
    std::fs::write(path, contents)
        .unwrap_or_else(|error| panic!("hew-lib build identity: cannot write {path:?}: {error}"));
}
