//! Building and staging a package's `[native]` Rust FFI library.
//!
//! A package may declare a Rust crate that backs its `extern` functions via a
//! `[native]` section in `hew.toml` (see [`crate::manifest::NativeLib`]). This
//! module compiles that crate and locates / stages the produced library so the
//! compiler can link it when the package is imported.

use std::path::{Path, PathBuf};
use std::process::Command;

use crate::manifest;

/// A built native FFI artifact for a package.
#[derive(Debug, Clone)]
pub struct NativeArtifact {
    /// The `[lib] name` (without the `lib` prefix or file extension).
    pub lib: String,
    /// Absolute path to the built artifact (e.g. `.../release/lib<lib>.a`).
    pub path: PathBuf,
}

/// Platform-specific file name for a built library of the given `kind`.
fn artifact_file_name(lib: &str, kind: &str) -> String {
    if kind == "cdylib" {
        if cfg!(target_os = "macos") {
            format!("lib{lib}.dylib")
        } else if cfg!(target_os = "windows") {
            format!("{lib}.dll")
        } else {
            format!("lib{lib}.so")
        }
    } else if cfg!(target_os = "windows") {
        // staticlib
        format!("{lib}.lib")
    } else {
        format!("lib{lib}.a")
    }
}

/// Build the `[native]` library declared in `<manifest_dir>/hew.toml`, if any.
///
/// Runs `cargo build --release` for the crate and locates the produced library.
/// Returns `Ok(None)` when the manifest has no `[native]` section.
///
/// # Errors
///
/// Returns an error when the manifest can't be read, the crate fails to build,
/// or the built artifact can't be located.
pub fn build_native(manifest_dir: &Path) -> Result<Option<NativeArtifact>, String> {
    let manifest_path = manifest_dir.join("hew.toml");
    let m = manifest::parse_manifest(&manifest_path).map_err(|e| e.to_string())?;
    let Some(native) = m.native else {
        return Ok(None);
    };

    let crate_dir = manifest_dir.join(&native.crate_dir);
    let cargo_toml = crate_dir.join("Cargo.toml");
    if !cargo_toml.exists() {
        return Err(format!(
            "[native] crate at {} has no Cargo.toml",
            crate_dir.display()
        ));
    }

    // Run cargo from the crate directory (not just `--manifest-path`) so that
    // rustup resolves the package's `rust-toolchain.toml` by walking up from
    // the crate dir. The native staticlib must be built with the *same* rustc
    // as `libhew.a` so its embedded `libstd` is byte-identical and the linker
    // dedups `rust_eh_personality`; a mismatched toolchain re-introduces a
    // duplicate-symbol link failure.
    let status = Command::new("cargo")
        .args(["build", "--release", "--manifest-path"])
        .arg(&cargo_toml)
        .current_dir(&crate_dir)
        .status()
        .map_err(|e| format!("failed to run cargo: {e}"))?;
    if !status.success() {
        return Err(format!(
            "cargo build failed for [native] crate {}",
            crate_dir.display()
        ));
    }

    let target_dir = cargo_target_dir(&cargo_toml)?;
    let file_name = artifact_file_name(&native.lib, &native.kind);
    let artifact = target_dir.join("release").join(&file_name);
    if !artifact.exists() {
        return Err(format!(
            "[native] crate built but artifact not found: {} (expected lib name `{}`, kind `{}`)",
            artifact.display(),
            native.lib,
            native.kind
        ));
    }
    Ok(Some(NativeArtifact {
        lib: native.lib,
        path: artifact,
    }))
}

/// Query the Cargo `target_directory` for a crate via `cargo metadata`.
fn cargo_target_dir(cargo_toml: &Path) -> Result<PathBuf, String> {
    let out = Command::new("cargo")
        .args([
            "metadata",
            "--no-deps",
            "--format-version",
            "1",
            "--manifest-path",
        ])
        .arg(cargo_toml)
        .output()
        .map_err(|e| format!("failed to run cargo metadata: {e}"))?;
    if !out.status.success() {
        return Err("cargo metadata failed".to_string());
    }
    let json: serde_json::Value =
        serde_json::from_slice(&out.stdout).map_err(|e| format!("invalid cargo metadata: {e}"))?;
    json.get("target_directory")
        .and_then(serde_json::Value::as_str)
        .map(PathBuf::from)
        .ok_or_else(|| "cargo metadata missing target_directory".to_string())
}

/// Copy a built artifact into `dest_dir`, returning the staged path.
///
/// # Errors
///
/// Returns an error when the destination can't be created or the copy fails.
pub fn stage_native(artifact: &NativeArtifact, dest_dir: &Path) -> Result<PathBuf, String> {
    std::fs::create_dir_all(dest_dir)
        .map_err(|e| format!("cannot create {}: {e}", dest_dir.display()))?;
    let file_name = artifact
        .path
        .file_name()
        .ok_or_else(|| "artifact has no file name".to_string())?;
    let dest = dest_dir.join(file_name);
    std::fs::copy(&artifact.path, &dest).map_err(|e| {
        format!(
            "cannot stage {} -> {}: {e}",
            artifact.path.display(),
            dest.display()
        )
    })?;
    Ok(dest)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn staticlib_artifact_name() {
        let name = artifact_file_name("hew_hew_db_sqlite", "staticlib");
        if cfg!(target_os = "windows") {
            assert_eq!(name, "hew_hew_db_sqlite.lib");
        } else {
            assert_eq!(name, "libhew_hew_db_sqlite.a");
        }
    }

    #[test]
    fn cdylib_artifact_name() {
        let name = artifact_file_name("foo", "cdylib");
        if cfg!(target_os = "macos") {
            assert_eq!(name, "libfoo.dylib");
        } else if cfg!(target_os = "windows") {
            assert_eq!(name, "foo.dll");
        } else {
            assert_eq!(name, "libfoo.so");
        }
    }

    #[test]
    fn no_native_section_returns_none() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("hew.toml"),
            "[package]\nname = \"p\"\nversion = \"0.1.0\"\nedition = \"2026\"\n",
        )
        .unwrap();
        assert!(build_native(dir.path()).unwrap().is_none());
    }
}
