//! Building a package's `[native]` Rust FFI library.
//!
//! A package may declare a Rust crate that backs its `extern` functions via a
//! `[native]` section in `hew.toml` (see [`crate::manifest::NativeLib`]). This
//! module compiles that crate and locates the produced library so the compiler
//! can link it when the package is built or imported.

use std::path::{Path, PathBuf};
use std::process::Command;

use crate::manifest;

/// A built native FFI artifact for a package.
#[derive(Debug, Clone)]
pub struct NativeArtifact {
    /// The `[lib] name` (without the `lib` prefix or file extension).
    pub lib: String,
    /// Absolute path to the built artifact (e.g. `.../release-lib/lib<lib>.a`).
    pub path: PathBuf,
}

/// The `<release> <host>` identity of the rustc that built something.
///
/// A `[native]` crate's staticlib must be built with the identical rustc that
/// built `libhew.a`, or its embedded `libstd` is not byte-identical and the
/// final link fails on a duplicate `rust_eh_personality` symbol (see
/// [`build_native`]'s toolchain check below).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustcIdentity {
    pub release: String,
    pub host: String,
}

impl std::fmt::Display for RustcIdentity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "rustc {} ({})", self.release, self.host)
    }
}

impl RustcIdentity {
    /// Parse the `<release> <host>` stamp `hew-pkg/build.rs` embeds via
    /// `cargo:rustc-env=HEW_RUNTIME_RUSTC`.
    fn parse_stamp(text: &str) -> Option<Self> {
        let (release, host) = text.trim().split_once(' ')?;
        Some(Self {
            release: release.to_string(),
            host: host.to_string(),
        })
    }

    /// Parse the `release:`/`host:` lines out of full `rustc -vV` output.
    fn parse_verbose(text: &str) -> Option<Self> {
        let field = |name: &str| -> Option<String> {
            let prefix = format!("{name}: ");
            text.lines()
                .find_map(|line| line.strip_prefix(prefix.as_str()))
                .map(str::trim)
                .map(str::to_string)
        };
        Some(Self {
            release: field("release")?,
            host: field("host")?,
        })
    }
}

/// The rustc identity that built this compiler — and, in the same workspace
/// build, `libhew.a` — embedded at compile time by `hew-pkg/build.rs`.
///
/// # Panics
///
/// Panics if `HEW_RUNTIME_RUSTC` is malformed, which cannot happen from a
/// normal build: `hew-pkg/build.rs` is the only writer of this env var and
/// always emits the `<release> <host>` shape this parses.
#[must_use]
pub fn embedded_rustc_identity() -> RustcIdentity {
    RustcIdentity::parse_stamp(env!("HEW_RUNTIME_RUSTC")).unwrap_or_else(|| {
        panic!(
            "HEW_RUNTIME_RUSTC={:?} embedded by hew-pkg/build.rs is malformed",
            env!("HEW_RUNTIME_RUSTC")
        )
    })
}

/// Query the rustc identity `cargo build` will use for the crate at
/// `crate_dir`. Spawned **from** `crate_dir`, exactly like the `cargo build`
/// call in [`build_native`], so rustup resolves the same
/// `rust-toolchain.toml` cargo is about to honour — probing from a different
/// directory could report a different (wrong) toolchain.
fn crate_rustc_identity(crate_dir: &Path) -> Result<RustcIdentity, String> {
    let output = Command::new("rustc")
        .arg("-vV")
        .current_dir(crate_dir)
        .output()
        .map_err(|e| format!("failed to run `rustc -vV`: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "`rustc -vV` failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    let text = String::from_utf8(output.stdout)
        .map_err(|e| format!("`rustc -vV` produced non-UTF-8 output: {e}"))?;
    RustcIdentity::parse_verbose(&text)
        .ok_or_else(|| format!("could not parse `rustc -vV` output:\n{text}"))
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
/// Runs Cargo with the non-LTO `release-lib` profile used for consumer-facing
/// static libraries and locates the produced artifact.
/// Returns `Ok(None)` when the manifest has no `[native]` section.
///
/// `expected` is the rustc identity `libhew.a` was built with — callers pass
/// [`embedded_rustc_identity`] in production; tests pass a deliberately
/// mismatching value directly, with no env var involved.
///
/// # Errors
///
/// Returns an error when the manifest can't be read, the crate's rustc
/// doesn't match `expected` (`E_NATIVE_TOOLCHAIN`), the crate fails to build,
/// or the built artifact can't be located.
pub fn build_native(
    manifest_dir: &Path,
    expected: &RustcIdentity,
) -> Result<Option<NativeArtifact>, String> {
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

    // Fail closed before invoking cargo: cargo build "succeeds" under a
    // mismatched rustc, but the resulting staticlib embeds a libstd that
    // isn't byte-identical to libhew.a's, and the *final* link fails much
    // later on a duplicate `rust_eh_personality` symbol — a confusing
    // failure far from its cause.
    let actual = crate_rustc_identity(&crate_dir)?;
    if actual != *expected {
        return Err(format!(
            "error[E_NATIVE_TOOLCHAIN]: [native] crate at {} would build with {actual}, \
             but the Hew runtime (libhew.a) was built with {expected}. A mismatched rustc \
             embeds an incompatible libstd, and the final link would fail on a duplicate \
             `rust_eh_personality` symbol. Fix: pin {}/rust-toolchain.toml to release {}.",
            crate_dir.display(),
            crate_dir.display(),
            expected.release,
        ));
    }

    // Run cargo from the crate directory (not just `--manifest-path`) so that
    // rustup resolves the package's `rust-toolchain.toml` by walking up from
    // the crate dir. The native staticlib must be built with the *same* rustc
    // as `libhew.a` so its embedded `libstd` is byte-identical and the linker
    // dedups `rust_eh_personality`; a mismatched toolchain re-introduces a
    // duplicate-symbol link failure. Define `release-lib` on the Cargo
    // command line so standalone packages inherit their own release settings
    // while always disabling LTO for a consumer-linkable archive.
    let status = Command::new("cargo")
        .args([
            "build",
            "--profile",
            "release-lib",
            "--config",
            r#"profile.release-lib.inherits="release""#,
            "--config",
            "profile.release-lib.lto=false",
            "--manifest-path",
        ])
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
    let artifact = target_dir.join("release-lib").join(&file_name);
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
        assert!(build_native(dir.path(), &embedded_rustc_identity())
            .unwrap()
            .is_none());
    }

    #[test]
    fn rustc_identity_display_names_release_and_host() {
        let id = RustcIdentity {
            release: "1.82.0".to_string(),
            host: "x86_64-unknown-linux-gnu".to_string(),
        };
        assert_eq!(id.to_string(), "rustc 1.82.0 (x86_64-unknown-linux-gnu)");
    }

    #[test]
    fn rustc_identity_parses_verbose_output() {
        let text = "rustc 1.82.0 (f6e511eec 2024-10-15)\n\
                     binary: rustc\n\
                     commit-hash: f6e511eec5f43ba5e5e2b60eb1a35d4f1a35e97a\n\
                     commit-date: 2024-10-15\n\
                     host: x86_64-unknown-linux-gnu\n\
                     release: 1.82.0\n\
                     LLVM version: 19.1.1\n";
        let id = RustcIdentity::parse_verbose(text).expect("parses");
        assert_eq!(id.release, "1.82.0");
        assert_eq!(id.host, "x86_64-unknown-linux-gnu");
    }

    #[test]
    fn rustc_identity_parse_verbose_rejects_output_missing_fields() {
        assert!(RustcIdentity::parse_verbose("binary: rustc\n").is_none());
    }

    /// `build_native` must refuse a `[native]` crate before ever invoking
    /// cargo when the caller's `expected` identity doesn't match the host's
    /// actual rustc — the negative control for the toolchain check: with a
    /// matching `expected` (any real build) this same fixture would proceed
    /// past this point.
    #[test]
    fn build_native_refuses_mismatched_rustc() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("hew.toml"),
            "[package]\nname = \"p\"\nversion = \"0.1.0\"\nedition = \"2026\"\n\
             [native]\nlib = \"p_native\"\ncrate = \"native\"\n",
        )
        .unwrap();
        let native_dir = dir.path().join("native");
        std::fs::create_dir_all(&native_dir).unwrap();
        std::fs::write(
            native_dir.join("Cargo.toml"),
            "[package]\nname = \"p_native\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\
             [lib]\ncrate-type = [\"staticlib\"]\n",
        )
        .unwrap();

        let mismatching = RustcIdentity {
            release: "0.0.0-does-not-exist".to_string(),
            host: "nowhere".to_string(),
        };
        let error = build_native(dir.path(), &mismatching).unwrap_err();

        assert!(
            error.contains("E_NATIVE_TOOLCHAIN"),
            "must carry the toolchain-mismatch code: {error}"
        );
        assert!(
            error.contains("0.0.0-does-not-exist"),
            "must name the expected (libhew.a) rustc: {error}"
        );
        assert!(
            !error.contains("cargo build failed"),
            "must fail before ever invoking cargo: {error}"
        );
    }
}
