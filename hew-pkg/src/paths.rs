use std::path::PathBuf;

/// Return the user's home directory, preferring `$HOME` (Unix) then
/// `%USERPROFILE%` (Windows). Falls back to `std::env::temp_dir()` so callers
/// always get a usable path regardless of platform.
#[must_use]
pub fn home_dir() -> PathBuf {
    std::env::var("HOME")
        .or_else(|_| std::env::var("USERPROFILE"))
        .map_or_else(|_| std::env::temp_dir(), PathBuf::from)
}

/// Return the Hew package-manager home directory.
///
/// `HEW_HOME` overrides the default `~/.hew` when set. This is the one
/// choke point for package-manager state: config, the local registry cache,
/// the index, signing keys, and credentials all resolve through this
/// function rather than reading `HEW_HOME` themselves.
#[must_use]
pub fn hew_home() -> PathBuf {
    std::env::var("HEW_HOME").map_or_else(|_| home_dir().join(".hew"), PathBuf::from)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::Mutex;

    use super::hew_home;

    // Serialize env-var–mutating tests: `std::env::set_var` / `remove_var`
    // are not thread-safe when multiple tests share the same process.
    static ENV_LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn hew_home_honours_env() {
        let _guard = ENV_LOCK.lock().unwrap();
        std::env::set_var("HEW_HOME", "/tmp/custom-hew-home");
        let resolved = hew_home();
        std::env::remove_var("HEW_HOME");
        assert_eq!(resolved, PathBuf::from("/tmp/custom-hew-home"));
    }

    #[test]
    fn hew_home_defaults_when_env_absent() {
        let _guard = ENV_LOCK.lock().unwrap();
        std::env::remove_var("HEW_HOME");
        let resolved = hew_home();
        assert!(resolved.ends_with(".hew"));
    }
}
