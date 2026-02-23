//! Export metadata for the `std::encoding::markdown` package.

/// Return metadata for all `#[hew_export]`-annotated functions in this package.
#[must_use]
pub fn all_exports() -> Vec<hew_export_types::ExportMeta> {
    vec![
        crate::__hew_export_meta_hew_markdown_to_html(),
        crate::__hew_export_meta_hew_markdown_to_html_safe(),
    ]
}
