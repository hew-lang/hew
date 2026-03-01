//! Export metadata for the `std::misc::log` package.
//!
//! All logging functions are implemented in `hew-runtime/src/log_core.rs`,
//! so this package has no local exports.

/// Return metadata for all `#[hew_export]`-annotated functions in this package.
#[must_use]
pub fn all_exports() -> Vec<hew_export_types::ExportMeta> {
    vec![]
}
