//! Collects all `#[hew_export]` metadata from the runtime.
//!
//! Enabled with the `export-meta` cargo feature.  The `hew-stdlib-gen` binary
//! depends on this module to drive `.hew` stub file and `stdlib.rs` generation.
//!
//! Most ecosystem modules have been extracted to standalone packages.
//! Their export metadata will be collected from those packages in the future.

/// Return metadata for every annotated runtime export enabled in this build.
///
/// After the package extraction, most ecosystem modules now live in their own
/// crates. This function only returns metadata from modules still in hew-runtime.
#[must_use]
pub fn all_exports() -> Vec<hew_export_types::ExportMeta> {
    // All annotated modules (uuid, markdown, logging) have been extracted
    // to standalone packages. This will be repopulated when those packages
    // gain export-meta support (Task 17).
    Vec::new()
}
