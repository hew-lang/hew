/// Returns `".exe"` on Windows, `""` on all other platforms.
pub fn exe_suffix() -> &'static str {
    if cfg!(target_os = "windows") {
        ".exe"
    } else {
        ""
    }
}
