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

/// Return the Hew package-manager home directory (`~/.hew`).
#[must_use]
pub fn hew_home() -> PathBuf {
    home_dir().join(".hew")
}
