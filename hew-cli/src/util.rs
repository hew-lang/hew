use std::path::PathBuf;

/// Locate the `hew` binary for subprocess-based CLI features.
///
/// When running as `hew <subcommand>`, `current_exe()` is the hew binary itself.
/// When running unit tests, the test binary is in `target/debug/deps/` and the
/// hew binary is usually at `target/debug/hew`.
pub(crate) fn find_hew_binary() -> Result<PathBuf, String> {
    let exe = std::env::current_exe().map_err(|e| format!("cannot locate self: {e}"))?;

    if exe
        .file_name()
        .is_some_and(|name| name == "hew" || name == "hew.exe")
    {
        return Ok(exe);
    }

    let exe_dir = exe.parent().expect("exe should have a parent directory");
    let hew_name = format!("hew{}", crate::platform::exe_suffix());
    let candidates = [
        exe_dir.join(format!("../{hew_name}")),
        exe_dir.join(&hew_name),
        exe_dir.join(format!("../../debug/{hew_name}")),
    ];

    for candidate in &candidates {
        if candidate.exists() {
            return candidate
                .canonicalize()
                .map_err(|e| format!("cannot resolve hew binary path: {e}"));
        }
    }

    Err(format!(
        "cannot find hew binary (searched relative to {})",
        exe_dir.display()
    ))
}
