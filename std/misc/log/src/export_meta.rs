//! Export metadata for the `std::misc::log` package.

/// Return metadata for all `#[hew_export]`-annotated functions in this package.
#[must_use]
pub fn all_exports() -> Vec<hew_export_types::ExportMeta> {
    vec![
        crate::__hew_export_meta_hew_log_init(),
        crate::__hew_export_meta_hew_log_init_level(),
        crate::__hew_export_meta_hew_log_error(),
        crate::__hew_export_meta_hew_log_warn(),
        crate::__hew_export_meta_hew_log_info(),
        crate::__hew_export_meta_hew_log_debug(),
        crate::__hew_export_meta_hew_log_trace(),
    ]
}
