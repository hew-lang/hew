//! Export metadata for the `std::misc::uuid` package.

/// Return metadata for all `#[hew_export]`-annotated functions in this package.
#[must_use]
pub fn all_exports() -> Vec<hew_export_types::ExportMeta> {
    vec![
        crate::__hew_export_meta_hew_uuid_v4(),
        crate::__hew_export_meta_hew_uuid_v7(),
        crate::__hew_export_meta_hew_uuid_parse(),
    ]
}
