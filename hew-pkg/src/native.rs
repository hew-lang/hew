//! Building a package's `[native]` code into link inputs.
//!
//! [`build`] is the one place a package's native code is built: the Rust crate
//! (through Cargo), the C and C++ sources (through the C driver Hew links
//! with) and the system libraries (`pkg-config`, `link-libs`, `lib-dirs`). It
//! returns everything the final link needs from that package, plus the files
//! the build read, so `hew build --emit-deps` can name them.
//!
//! The caller supplies the C toolchain ([`NativeToolchain`]) because the driver
//! and its target flags belong to the linker driver, not to the manifest.

use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;

use sha2::{Digest, Sha256};

use crate::manifest::{HewManifest, LinkLib, NativeInputs, NativeOs, RustCrate, SourceLanguage};

/// The symbol prefix the Hew runtime owns. Package C and C++ code may not
/// define symbols under it.
const RESERVED_PREFIX: &str = "hew_";

/// How C and C++ sources compile for the current target.
#[derive(Debug, Clone)]
pub struct NativeToolchain {
    /// Selects the `[native.<os>]` table.
    pub os: NativeOs,
    /// The C driver (`clang`, a gcc-compatible `cc`, or `HEW_CC`). It picks
    /// the language from the source extension, so it compiles C++ too.
    pub driver: String,
    /// Flags every compile carries: target, profile, PIC, CRT and sanitizer.
    pub flags: Vec<String>,
    /// Object file suffix for the target (`.o` or `.obj`).
    pub object_suffix: &'static str,
}

/// What one package's `[native]` section contributes to the final link.
#[derive(Debug, Clone, Default)]
pub struct NativeBuild {
    /// Archives and objects, linked before any system library.
    pub inputs: Vec<PathBuf>,
    /// Library search paths and system libraries, linked after every input.
    pub link_args: Vec<String>,
    /// Whether a C++ source was compiled; the link then needs the C++ runtime.
    pub compiles_cxx: bool,
    /// Files the build read: native sources, their headers and Rust sources.
    pub dependencies: Vec<PathBuf>,
}

/// Build the `[native]` section of the package rooted at `root`.
///
/// Returns `Ok(None)` when the manifest has no `[native]` section.
///
/// # Errors
///
/// Returns the diagnostic text when the Rust crate's toolchain does not match
/// the runtime's (`E_NATIVE_TOOLCHAIN`), a build step fails
/// (`E_NATIVE_COMPILE`, `E_NATIVE_PKG_CONFIG`), or a C or C++ source defines
/// a symbol under the runtime's reserved prefix (`E_RESERVED_NATIVE_SYMBOL`).
pub fn build(
    root: &Path,
    manifest: &HewManifest,
    toolchain: &NativeToolchain,
) -> Result<Option<NativeBuild>, String> {
    let Some(native) = &manifest.native else {
        return Ok(None);
    };
    let package = manifest.package.name.as_str();
    let mut out = NativeBuild::default();
    if let Some(rust) = native.rust_crate() {
        let artifact = build_rust_crate(root, &rust, &embedded_rustc_identity())?;
        out.dependencies
            .extend(cargo_dep_info(&artifact.with_extension("d")));
        out.inputs.push(artifact);
    }
    let inputs = native.inputs_for(toolchain.os);
    let compile_flags = pkg_config(package, &inputs.pkg_config, "--cflags")?;
    let link_flags = pkg_config(package, &inputs.pkg_config, "--libs")?;
    if !inputs.sources.is_empty() {
        let sources = SourceSet {
            root,
            package,
            inputs: &inputs,
            pkg_cflags: &compile_flags,
        };
        sources.compile(toolchain, &mut out)?;
    }
    for dir in &inputs.lib_dirs {
        out.link_args
            .push(format!("-L{}", root.join(dir).display()));
    }
    out.link_args.extend(link_flags);
    for lib in &inputs.link_libs {
        out.link_args.push(match LinkLib::of(lib) {
            LinkLib::Name(name) => format!("-l{name}"),
            LinkLib::Path(path) => root.join(path).display().to_string(),
        });
    }
    Ok(Some(out))
}

// ── C and C++ sources ──────────────────────────────────────────────────────

struct SourceSet<'a> {
    root: &'a Path,
    package: &'a str,
    inputs: &'a NativeInputs,
    pkg_cflags: &'a [String],
}

impl SourceSet<'_> {
    fn compile(&self, toolchain: &NativeToolchain, out: &mut NativeBuild) -> Result<(), String> {
        let c_args = self.args(toolchain, SourceLanguage::C);
        let cxx_args = self.args(toolchain, SourceLanguage::Cxx);
        // Objects live under a directory named for the full command line, so
        // a changed flag, define, target or profile never reuses an object.
        let object_dir = self.root.join("target").join("native").join(command_key(
            &toolchain.driver,
            &c_args,
            &cxx_args,
        ));
        for source in &self.inputs.sources {
            let language = SourceLanguage::of(source)
                .ok_or_else(|| format!("{source}: not a C or C++ source"))?;
            let path = self.root.join(source);
            if !path.is_file() {
                return Err(format!(
                    "error[E_NATIVE_COMPILE]: package `{}` lists source `{source}` in [native], \
                     but {} does not exist",
                    self.package,
                    path.display()
                ));
            }
            let object = object_dir.join(format!("{source}{}", toolchain.object_suffix));
            let depfile = object_dir.join(format!("{source}.d"));
            let args = match language {
                SourceLanguage::C => &c_args,
                SourceLanguage::Cxx => &cxx_args,
            };
            if fresh_dependencies(&object, &depfile).is_none() {
                compile_one(&toolchain.driver, args, &path, &object, &depfile)
                    .map_err(|stderr| self.compile_error(source, &stderr))?;
            }
            refuse_reserved_symbols(&object, source, self.package)?;
            out.dependencies.extend(read_depfile(&depfile));
            out.compiles_cxx |= language == SourceLanguage::Cxx;
            out.inputs.push(object);
        }
        Ok(())
    }

    fn args(&self, toolchain: &NativeToolchain, language: SourceLanguage) -> Vec<String> {
        let mut args = toolchain.flags.clone();
        args.extend_from_slice(self.pkg_cflags);
        for dir in &self.inputs.include_dirs {
            args.push(format!("-I{}", self.root.join(dir).display()));
        }
        args.extend(self.inputs.defines.iter().map(|d| format!("-D{d}")));
        args.extend_from_slice(match language {
            SourceLanguage::C => &self.inputs.cflags,
            SourceLanguage::Cxx => &self.inputs.cxxflags,
        });
        args
    }

    fn compile_error(&self, source: &str, stderr: &str) -> String {
        format!(
            "error[E_NATIVE_COMPILE]: cannot compile `{source}` for package `{}`\n{}",
            self.package,
            stderr.trim_end()
        )
    }
}

fn command_key(driver: &str, c_args: &[String], cxx_args: &[String]) -> String {
    let mut hasher = Sha256::new();
    for part in std::iter::once(driver)
        .chain(c_args.iter().map(String::as_str))
        .chain(std::iter::once("\0cxx"))
        .chain(cxx_args.iter().map(String::as_str))
    {
        hasher.update(part.as_bytes());
        hasher.update([0]);
    }
    hasher.finalize()[..8]
        .iter()
        .fold(String::new(), |mut key, byte| {
            let _ = write!(key, "{byte:02x}");
            key
        })
}

/// Compile `source` to `object`, writing its header dependencies to
/// `depfile`. Both land through a temporary name so a concurrent build of the
/// same package never links a half-written object.
fn compile_one(
    driver: &str,
    args: &[String],
    source: &Path,
    object: &Path,
    depfile: &Path,
) -> Result<(), String> {
    let parent = object
        .parent()
        .ok_or_else(|| format!("object path has no parent: {}", object.display()))?;
    std::fs::create_dir_all(parent)
        .map_err(|e| format!("cannot create {}: {e}", parent.display()))?;
    // In-process test compiles build on several threads, so the process id
    // alone does not make the name unique.
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let serial = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let unique = format!(".{}-{serial}.tmp", std::process::id());
    let object_tmp = append(object, &unique);
    let depfile_tmp = append(depfile, &unique);
    let output = Command::new(driver)
        .args(args)
        .arg("-c")
        .arg(source)
        .arg("-o")
        .arg(&object_tmp)
        .arg("-MMD")
        .arg("-MF")
        .arg(&depfile_tmp)
        .output()
        .map_err(|e| format!("cannot run the C compiler `{driver}`: {e}"))?;
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !output.status.success() {
        let _ = std::fs::remove_file(&object_tmp);
        let _ = std::fs::remove_file(&depfile_tmp);
        return Err(stderr.into_owned());
    }
    // Warnings are build output the package asked for (`-Wall` and friends).
    if !stderr.trim().is_empty() {
        eprint!("{stderr}");
    }
    std::fs::rename(&depfile_tmp, depfile)
        .and_then(|()| std::fs::rename(&object_tmp, object))
        .map_err(|e| format!("cannot write {}: {e}", object.display()))
}

fn append(path: &Path, suffix: &str) -> PathBuf {
    let mut name = path.as_os_str().to_owned();
    name.push(suffix);
    PathBuf::from(name)
}

/// The object's dependencies when it is newer than every one of them; `None`
/// when it must be rebuilt.
fn fresh_dependencies(object: &Path, depfile: &Path) -> Option<Vec<PathBuf>> {
    let built = std::fs::metadata(object).ok()?.modified().ok()?;
    let deps = read_depfile(depfile);
    if deps.is_empty() {
        return None;
    }
    deps.iter()
        .all(|dep| {
            std::fs::metadata(dep)
                .and_then(|meta| meta.modified())
                .is_ok_and(|modified| modified <= built)
        })
        .then_some(deps)
}

fn read_depfile(path: &Path) -> Vec<PathBuf> {
    std::fs::read_to_string(path)
        .map(|text| parse_depfile(&text))
        .unwrap_or_default()
}

/// The prerequisites of a Make-format dependency file, as written by
/// `cc -MMD` and Cargo: `target: dep dep \` with `\ ` escaping a space and
/// `$$` a dollar sign.
fn parse_depfile(text: &str) -> Vec<PathBuf> {
    let joined = text.replace("\\\r\n", " ").replace("\\\n", " ");
    let mut deps = Vec::new();
    for line in joined.lines() {
        // The target ends at the first `:` followed by whitespace or the end
        // of the line, so a Windows drive (`C:\`) stays inside a path.
        let bytes = line.as_bytes();
        let Some(colon) = (0..bytes.len())
            .find(|&i| bytes[i] == b':' && bytes.get(i + 1).is_none_or(u8::is_ascii_whitespace))
        else {
            continue;
        };
        let mut token = String::new();
        let mut chars = line[colon + 1..].chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '\\' if chars
                    .peek()
                    .is_some_and(|next| *next == ' ' || *next == '#') =>
                {
                    token.push(chars.next().unwrap_or(' '));
                }
                '$' if chars.peek() == Some(&'$') => {
                    chars.next();
                    token.push('$');
                }
                c if c.is_whitespace() => {
                    if !token.is_empty() {
                        deps.push(PathBuf::from(std::mem::take(&mut token)));
                    }
                }
                c => token.push(c),
            }
        }
        if !token.is_empty() {
            deps.push(PathBuf::from(token));
        }
    }
    deps
}

/// Cargo writes `<artifact>.d` beside each library it builds; it lists the
/// crate's Rust sources. A missing file only means no Rust sources reach the
/// dependency file.
fn cargo_dep_info(path: &Path) -> Vec<PathBuf> {
    read_depfile(path)
}

// ── Reserved runtime symbols ───────────────────────────────────────────────

fn refuse_reserved_symbols(object: &Path, source: &str, package: &str) -> Result<(), String> {
    let reserved = reserved_definitions(object)?;
    if reserved.is_empty() {
        return Ok(());
    }
    let prefix = symbol_prefix(package);
    let mut message = String::new();
    for symbol in &reserved {
        let renamed = format!("{prefix}{}", &symbol[RESERVED_PREFIX.len()..]);
        let _ = write!(
            message,
            "error[E_RESERVED_NATIVE_SYMBOL]: `{source}` in package `{package}` defines \
             `{symbol}`, but the `{RESERVED_PREFIX}` symbol prefix is reserved for the Hew \
             runtime\n  help: rename it to `{renamed}` here and in the `extern \"C\"` block \
             that declares it\n"
        );
    }
    Err(message.trim_end().to_string())
}

/// Global symbols `object` defines under the reserved prefix.
fn reserved_definitions(object: &Path) -> Result<Vec<String>, String> {
    use object::{Object as _, ObjectSymbol as _};

    let data =
        std::fs::read(object).map_err(|e| format!("cannot read {}: {e}", object.display()))?;
    let file = object::File::parse(&*data)
        .map_err(|e| format!("cannot read object {}: {e}", object.display()))?;
    // Mach-O spells every C symbol with a leading underscore.
    let mangling = match file.format() {
        object::BinaryFormat::MachO => "_",
        _ => "",
    };
    let mut reserved = Vec::new();
    for symbol in file.symbols() {
        if !symbol.is_definition() || !symbol.is_global() {
            continue;
        }
        let Ok(name) = symbol.name() else { continue };
        let Some(name) = name.strip_prefix(mangling) else {
            continue;
        };
        if name.starts_with(RESERVED_PREFIX) {
            reserved.push(name.to_string());
        }
    }
    Ok(reserved)
}

/// The symbol prefix to suggest for `package`: its name with every
/// non-identifier character as `_`, so `meshcore.roles` suggests
/// `meshcore_roles_`. A name inside the runtime's own namespace suggests its
/// last segment instead.
fn symbol_prefix(package: &str) -> String {
    let spelled: String = package
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '_'
            }
        })
        .collect();
    let mut prefix = spelled
        .split('_')
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>();
    if prefix.first() == Some(&"hew") && prefix.len() > 1 {
        prefix.remove(0);
    }
    if prefix.is_empty() || prefix == ["hew"] {
        return "pkg_".to_string();
    }
    format!("{}_", prefix.join("_"))
}

// ── pkg-config ─────────────────────────────────────────────────────────────

/// Run `pkg-config <flag> <names>` (or `$PKG_CONFIG`) and split its output.
fn pkg_config(package: &str, names: &[String], flag: &str) -> Result<Vec<String>, String> {
    if names.is_empty() {
        return Ok(Vec::new());
    }
    let program = std::env::var("PKG_CONFIG").unwrap_or_else(|_| "pkg-config".to_string());
    let listed = names.join(", ");
    let output = Command::new(&program)
        .arg(flag)
        .args(names)
        .output()
        .map_err(|error| {
            format!(
                "error[E_NATIVE_PKG_CONFIG]: package `{package}` finds {listed} with pkg-config, \
                 but `{program}` cannot run: {error}\n  help: install pkg-config (or pkgconf), \
                 or name the libraries with `link-libs` and `lib-dirs` in a \
                 [native.<os>] table"
            )
        })?;
    if !output.status.success() {
        return Err(format!(
            "error[E_NATIVE_PKG_CONFIG]: package `{package}` needs {listed}, which pkg-config \
             cannot find\n{}",
            String::from_utf8_lossy(&output.stderr).trim_end()
        ));
    }
    Ok(split_flags(&String::from_utf8_lossy(&output.stdout)))
}

/// Split pkg-config output into arguments: whitespace separates them and `\`
/// escapes the next character.
fn split_flags(text: &str) -> Vec<String> {
    let mut flags = Vec::new();
    let mut flag = String::new();
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => flag.extend(chars.next()),
            c if c.is_whitespace() => {
                if !flag.is_empty() {
                    flags.push(std::mem::take(&mut flag));
                }
            }
            c => flag.push(c),
        }
    }
    if !flag.is_empty() {
        flags.push(flag);
    }
    flags
}

// ── Rust crates ────────────────────────────────────────────────────────────

/// The release and host identity of a rustc toolchain.
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

/// The rustc identity embedded while building the Hew runtime.
///
/// # Panics
///
/// Panics if the build stamp is malformed.
#[must_use]
pub fn embedded_rustc_identity() -> RustcIdentity {
    RustcIdentity::parse_stamp(env!("HEW_RUNTIME_RUSTC")).unwrap_or_else(|| {
        panic!(
            "HEW_RUNTIME_RUSTC={:?} embedded by hew-pkg/build.rs is malformed",
            env!("HEW_RUNTIME_RUSTC")
        )
    })
}

/// Query the rustc selected for a native crate directory.
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

/// Build a `[native]` Rust crate with Cargo's non-LTO `release-lib` profile
/// and return the produced library.
///
/// `expected` is the rustc identity `libhew.a` was built with: production
/// passes [`embedded_rustc_identity`]; tests pass a deliberately mismatching
/// value.
fn build_rust_crate(
    root: &Path,
    rust: &RustCrate<'_>,
    expected: &RustcIdentity,
) -> Result<PathBuf, String> {
    let crate_dir = root.join(rust.dir);
    let cargo_toml = crate_dir.join("Cargo.toml");
    if !cargo_toml.exists() {
        return Err(format!(
            "[native] crate at {} has no Cargo.toml",
            crate_dir.display()
        ));
    }

    // Refuse before cargo builds a static library with an incompatible libstd.
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
    let file_name = artifact_file_name(rust.lib, rust.kind);
    let artifact = target_dir.join("release-lib").join(&file_name);
    if !artifact.exists() {
        return Err(format!(
            "[native] crate built but artifact not found: {} (expected lib name `{}`, kind `{}`)",
            artifact.display(),
            rust.lib,
            rust.kind
        ));
    }
    Ok(artifact)
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

    #[test]
    fn build_rust_crate_refuses_mismatched_rustc() {
        let dir = tempfile::tempdir().unwrap();
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
        let rust = RustCrate {
            dir: "native",
            lib: "p_native",
            kind: "staticlib",
        };
        let error = build_rust_crate(dir.path(), &rust, &mismatching).unwrap_err();

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

    #[test]
    fn depfile_prerequisites_keep_escaped_spaces_and_drive_letters() {
        let deps =
            parse_depfile("out/a.o: src/a.c include/my\\ header.h \\\n  C:\\sdk\\x.h cost$$.h\n");
        assert_eq!(
            deps,
            [
                PathBuf::from("src/a.c"),
                PathBuf::from("include/my header.h"),
                PathBuf::from("C:\\sdk\\x.h"),
                PathBuf::from("cost$.h"),
            ]
        );
    }

    #[test]
    fn suggested_prefix_comes_from_the_package_name() {
        assert_eq!(symbol_prefix("meshcore.roles"), "meshcore_roles_");
        assert_eq!(symbol_prefix("hew.db.sqlite"), "db_sqlite_");
        assert_eq!(symbol_prefix("hew::testffi"), "testffi_");
        assert_eq!(symbol_prefix("hew"), "pkg_");
    }

    #[test]
    fn pkg_config_output_splits_on_unescaped_whitespace() {
        assert_eq!(
            split_flags("-I/opt/my\\ sdk/include -DX=1\n"),
            ["-I/opt/my sdk/include", "-DX=1"]
        );
    }
}
