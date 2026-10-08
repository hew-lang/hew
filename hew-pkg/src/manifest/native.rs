//! The `[native]` section of `hew.toml`: the native code behind a package's
//! `extern "C"` functions.
//!
//! One table describes every native input a package brings to the final link:
//!
//! - a Rust crate (`lib`, with optional `crate` and `kind`), built by Cargo;
//! - C and C++ `sources`, compiled by the C driver Hew links with, with their
//!   `include-dirs`, `defines`, `cflags` and `cxxflags`;
//! - system libraries, discovered with `pkg-config` or named directly with
//!   `link-libs` and `lib-dirs`.
//!
//! `[native.linux]`, `[native.macos]`, `[native.freebsd]` and
//! `[native.windows]` add C inputs and system libraries for one target
//! operating system; they extend the base table and never replace it.

use std::path::{Component, Path};

use serde::{Deserialize, Serialize};

/// Library kinds accepted for a `[native]` Rust crate.
const RUST_KINDS: &[&str] = &["staticlib", "cdylib"];

/// File extensions compiled as C (`.c`) or C++ (`.cc`, `.cpp`, `.cxx`).
const C_EXTENSIONS: &[&str] = &["c"];
const CXX_EXTENSIONS: &[&str] = &["cc", "cpp", "cxx"];

/// The operating system a native build targets. Selects the per-OS table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeOs {
    Linux,
    Macos,
    FreeBsd,
    Windows,
}

/// The language a native source file is compiled as.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceLanguage {
    C,
    Cxx,
}

impl SourceLanguage {
    /// The language of `path`, from its extension; `None` when Hew does not
    /// compile files of that kind.
    #[must_use]
    pub fn of(path: &str) -> Option<Self> {
        let extension = Path::new(path).extension()?.to_str()?;
        if C_EXTENSIONS.contains(&extension) {
            Some(Self::C)
        } else if CXX_EXTENSIONS.contains(&extension) {
            Some(Self::Cxx)
        } else {
            None
        }
    }
}

/// C/C++ inputs and system libraries. The base `[native]` table carries one
/// set; each `[native.<os>]` table carries another, appended for that OS.
#[derive(Debug, Clone, Default, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct NativeInputs {
    /// C and C++ source files, relative to `hew.toml`.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub sources: Vec<String>,
    /// Header search directories for `sources`, relative to `hew.toml`.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub include_dirs: Vec<String>,
    /// Preprocessor definitions: `NAME` or `NAME=VALUE`.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub defines: Vec<String>,
    /// Extra driver flags for C sources (for example `-std=c11`).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub cflags: Vec<String>,
    /// Extra driver flags for C++ sources (for example `-std=c++17`).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub cxxflags: Vec<String>,
    /// `pkg-config` packages whose compile flags apply to `sources` and whose
    /// libraries join the final link.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub pkg_config: Vec<String>,
    /// System libraries by name (`crypto` links `-lcrypto` or `crypto.lib`),
    /// or library files by path.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub link_libs: Vec<String>,
    /// Library search directories: absolute, or relative to `hew.toml`.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub lib_dirs: Vec<String>,
}

impl NativeInputs {
    fn is_empty(&self) -> bool {
        *self == Self::default()
    }

    fn extend(&mut self, other: &Self) {
        self.sources.extend_from_slice(&other.sources);
        self.include_dirs.extend_from_slice(&other.include_dirs);
        self.defines.extend_from_slice(&other.defines);
        self.cflags.extend_from_slice(&other.cflags);
        self.cxxflags.extend_from_slice(&other.cxxflags);
        self.pkg_config.extend_from_slice(&other.pkg_config);
        self.link_libs.extend_from_slice(&other.link_libs);
        self.lib_dirs.extend_from_slice(&other.lib_dirs);
    }

    fn validate(&self, table: &str) -> Result<(), String> {
        for source in &self.sources {
            package_relative(table, "sources", source)?;
            if SourceLanguage::of(source).is_none() {
                return Err(format!(
                    "{table} sources entry \"{source}\" is not a C or C++ file \
                     (expected .c, .cc, .cpp or .cxx)"
                ));
            }
        }
        for dir in &self.include_dirs {
            package_relative(table, "include-dirs", dir)?;
        }
        for define in &self.defines {
            let name = define.split_once('=').map_or(define.as_str(), |(n, _)| n);
            if !is_c_identifier(name) {
                return Err(format!(
                    "{table} defines entry \"{define}\" must be NAME or NAME=VALUE, \
                     where NAME is a C identifier"
                ));
            }
        }
        for (field, flags) in [("cflags", &self.cflags), ("cxxflags", &self.cxxflags)] {
            if flags.iter().any(|flag| flag.trim().is_empty()) {
                return Err(format!("{table} {field} entries must not be empty"));
            }
        }
        for name in &self.pkg_config {
            if name.is_empty() || name.starts_with('-') || name.contains(char::is_whitespace) {
                return Err(format!(
                    "{table} pkg-config entry \"{name}\" must be one pkg-config package name"
                ));
            }
        }
        for lib in &self.link_libs {
            validate_link_lib(table, lib)?;
        }
        for dir in &self.lib_dirs {
            if !Path::new(dir).is_absolute() {
                package_relative(table, "lib-dirs", dir)?;
            }
        }
        Ok(())
    }
}

/// A `link-libs` entry: a library name or a path to a library file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkLib<'a> {
    /// Linked by name through the library search path.
    Name(&'a str),
    /// A library file, absolute or relative to `hew.toml`.
    Path(&'a str),
}

impl<'a> LinkLib<'a> {
    /// Classify an already-validated `link-libs` entry: anything containing a
    /// path separator is a file path.
    #[must_use]
    pub fn of(entry: &'a str) -> Self {
        if entry.contains('/') || entry.contains('\\') {
            Self::Path(entry)
        } else {
            Self::Name(entry)
        }
    }
}

fn validate_link_lib(table: &str, lib: &str) -> Result<(), String> {
    if lib.is_empty() || lib.contains(char::is_whitespace) {
        return Err(format!(
            "{table} link-libs entry \"{lib}\" must be a library name or a library file path"
        ));
    }
    if let Some(name) = lib.strip_prefix("-l") {
        return Err(format!(
            "{table} link-libs entry \"{lib}\" is a linker flag; write the library name \
             (\"{name}\") instead"
        ));
    }
    if lib.starts_with('-') {
        return Err(format!(
            "{table} link-libs entry \"{lib}\" is a linker flag; link-libs names libraries"
        ));
    }
    match LinkLib::of(lib) {
        LinkLib::Path(path) if !Path::new(path).is_absolute() => {
            package_relative(table, "link-libs", path)
        }
        LinkLib::Path(_) | LinkLib::Name(_) => Ok(()),
    }
}

/// Refuse a path that is absolute, climbs out of the package or uses `\`: a
/// published package carries only files below its own `hew.toml`, spelled
/// the same on every host.
fn package_relative(table: &str, field: &str, path: &str) -> Result<(), String> {
    if path.is_empty() {
        return Err(format!("{table} {field} entries must not be empty"));
    }
    if path.contains('\\') {
        return Err(format!(
            "{table} {field} entry \"{path}\" uses `\\`; write package paths with `/`"
        ));
    }
    let escapes = Path::new(path).components().any(|component| {
        matches!(
            component,
            Component::ParentDir | Component::RootDir | Component::Prefix(_)
        )
    });
    // `C:/x` is a drive path on Windows but a plain relative path elsewhere;
    // refuse it on every host so a manifest means the same thing everywhere.
    let drive = path.as_bytes().get(1) == Some(&b':');
    if escapes || drive {
        return Err(format!(
            "{table} {field} entry \"{path}\" must be a path inside the package, \
             relative to hew.toml"
        ));
    }
    Ok(())
}

fn is_c_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    chars
        .next()
        .is_some_and(|first| first.is_ascii_alphabetic() || first == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// A `[native]` Rust crate, resolved with its defaults.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustCrate<'a> {
    /// Crate directory, relative to `hew.toml`.
    pub dir: &'a str,
    /// The `[lib]` name the crate produces.
    pub lib: &'a str,
    /// `"staticlib"` or `"cdylib"`.
    pub kind: &'a str,
}

/// `[native]` — the native code that backs this package's `extern` functions.
/// `hew build` and `hew run` build it and link it into every program that
/// compiles the package, so neither the package nor its consumers pass
/// `--link-lib`.
///
/// The C fields repeat [`NativeInputs`] because serde cannot combine
/// `flatten` with `deny_unknown_fields`; [`NativeLib::inputs_for`] is the one
/// reader of both.
#[derive(Debug, Clone, Default, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct NativeLib {
    /// Rust crate directory, relative to `hew.toml` (default `"."`).
    #[serde(rename = "crate", default, skip_serializing_if = "Option::is_none")]
    pub crate_dir: Option<String>,
    /// The `[lib]` name the Rust crate produces as `lib<lib>.a`; its presence
    /// is what declares a Rust crate.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lib: Option<String>,
    /// Rust library kind: `"staticlib"` (default) or `"cdylib"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub kind: Option<String>,
    /// See [`NativeInputs::sources`].
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub sources: Vec<String>,
    /// See [`NativeInputs::include_dirs`].
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub include_dirs: Vec<String>,
    /// See [`NativeInputs::defines`].
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub defines: Vec<String>,
    /// See [`NativeInputs::cflags`].
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub cflags: Vec<String>,
    /// See [`NativeInputs::cxxflags`].
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub cxxflags: Vec<String>,
    /// See [`NativeInputs::pkg_config`].
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub pkg_config: Vec<String>,
    /// See [`NativeInputs::link_libs`].
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub link_libs: Vec<String>,
    /// See [`NativeInputs::lib_dirs`].
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub lib_dirs: Vec<String>,
    /// `[native.linux]` additions.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub linux: Option<NativeInputs>,
    /// `[native.macos]` additions.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub macos: Option<NativeInputs>,
    /// `[native.freebsd]` additions.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub freebsd: Option<NativeInputs>,
    /// `[native.windows]` additions.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub windows: Option<NativeInputs>,
}

impl NativeLib {
    /// The Rust crate this section declares, if any.
    #[must_use]
    pub fn rust_crate(&self) -> Option<RustCrate<'_>> {
        Some(RustCrate {
            lib: self.lib.as_deref()?,
            dir: self.crate_dir.as_deref().unwrap_or("."),
            kind: self.kind.as_deref().unwrap_or("staticlib"),
        })
    }

    /// The C inputs and system libraries for `os`: the base table followed by
    /// that OS's table.
    #[must_use]
    pub fn inputs_for(&self, os: NativeOs) -> NativeInputs {
        let mut inputs = self.base();
        if let Some(extra) = self.os_table(os) {
            inputs.extend(extra);
        }
        inputs
    }

    fn base(&self) -> NativeInputs {
        NativeInputs {
            sources: self.sources.clone(),
            include_dirs: self.include_dirs.clone(),
            defines: self.defines.clone(),
            cflags: self.cflags.clone(),
            cxxflags: self.cxxflags.clone(),
            pkg_config: self.pkg_config.clone(),
            link_libs: self.link_libs.clone(),
            lib_dirs: self.lib_dirs.clone(),
        }
    }

    fn os_table(&self, os: NativeOs) -> Option<&NativeInputs> {
        match os {
            NativeOs::Linux => self.linux.as_ref(),
            NativeOs::Macos => self.macos.as_ref(),
            NativeOs::FreeBsd => self.freebsd.as_ref(),
            NativeOs::Windows => self.windows.as_ref(),
        }
    }

    fn os_tables(&self) -> [(&'static str, Option<&NativeInputs>); 4] {
        [
            ("[native.linux]", self.linux.as_ref()),
            ("[native.macos]", self.macos.as_ref()),
            ("[native.freebsd]", self.freebsd.as_ref()),
            ("[native.windows]", self.windows.as_ref()),
        ]
    }

    /// Check the section's shape. Paths are checked for form only; whether a
    /// file exists is a build-time question.
    ///
    /// # Errors
    ///
    /// Returns the diagnostic text for the first malformed entry.
    pub fn validate(&self) -> Result<(), String> {
        match &self.lib {
            Some(lib) if lib.trim().is_empty() => {
                return Err("[native] lib must be a non-empty library name".to_string());
            }
            None if self.crate_dir.is_some() || self.kind.is_some() => {
                return Err("[native] `crate` and `kind` describe a Rust crate; add \
                     `lib = \"<the crate's [lib] name>\"` to build it"
                    .to_string());
            }
            Some(_) | None => {}
        }
        if let Some(kind) = &self.kind {
            if !RUST_KINDS.contains(&kind.as_str()) {
                return Err(format!(
                    "[native] kind = \"{kind}\" is invalid (expected one of {RUST_KINDS:?})"
                ));
            }
        }
        let base = self.base();
        let tables = self.os_tables();
        if self.lib.is_none()
            && base.is_empty()
            && tables
                .iter()
                .all(|(_, table)| table.is_none_or(NativeInputs::is_empty))
        {
            return Err(
                "[native] declares nothing: add a Rust crate (`lib`), C or C++ \
                 `sources`, or system libraries (`pkg-config`, `link-libs`)"
                    .to_string(),
            );
        }
        base.validate("[native]")?;
        for (name, table) in tables {
            if let Some(table) = table {
                table.validate(name)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(text: &str) -> Result<NativeLib, String> {
        let native: NativeLib = toml::from_str(text).map_err(|e| e.to_string())?;
        native.validate()?;
        Ok(native)
    }

    #[test]
    fn rust_crate_defaults_apply_only_when_lib_is_declared() {
        let native = parse("lib = \"p_native\"\n").unwrap();
        assert_eq!(
            native.rust_crate(),
            Some(RustCrate {
                dir: ".",
                lib: "p_native",
                kind: "staticlib"
            })
        );
        let c_only = parse("sources = [\"src/a.c\"]\n").unwrap();
        assert_eq!(c_only.rust_crate(), None);
    }

    #[test]
    fn os_table_extends_the_base_inputs() {
        let native = parse(
            "sources = [\"src/a.c\"]\nlink-libs = [\"m\"]\n\
             [windows]\nlink-libs = [\"ws2_32\"]\ndefines = [\"WIN=1\"]\n",
        )
        .unwrap();
        let windows = native.inputs_for(NativeOs::Windows);
        assert_eq!(windows.sources, ["src/a.c"]);
        assert_eq!(windows.link_libs, ["m", "ws2_32"]);
        assert_eq!(windows.defines, ["WIN=1"]);
        assert_eq!(native.inputs_for(NativeOs::Linux).link_libs, ["m"]);
    }

    #[test]
    fn malformed_entries_are_refused_with_the_field_named() {
        for (text, expected) in [
            ("", "declares nothing"),
            ("crate = \"native\"\n", "add `lib"),
            ("lib = \"x\"\nkind = \"bogus\"\n", "kind = \"bogus\""),
            ("sources = [\"src/a.rs\"]\n", "not a C or C++ file"),
            ("sources = [\"../shared/a.c\"]\n", "inside the package"),
            ("sources = [\"/abs/a.c\"]\n", "inside the package"),
            ("sources = [\"C:/abs/a.c\"]\n", "inside the package"),
            (
                "include-dirs = [\"src\\\\inc\"]\n",
                "write package paths with `/`",
            ),
            (
                "defines = [\"1BAD\"]\nsources = [\"a.c\"]\n",
                "C identifier",
            ),
            (
                "link-libs = [\"-lcrypto\"]\n",
                "write the library name (\"crypto\")",
            ),
            ("link-libs = [\"-Wl,--as-needed\"]\n", "linker flag"),
            (
                "pkg-config = [\"lib curl\"]\n",
                "one pkg-config package name",
            ),
            (
                "[linux]\nsources = [\"../a.c\"]\n",
                "[native.linux] sources",
            ),
        ] {
            let error = parse(text).expect_err(text);
            assert!(error.contains(expected), "{text:?}: {error}");
        }
    }

    #[test]
    fn unknown_fields_and_os_tables_are_refused() {
        assert!(parse("sourcse = [\"a.c\"]\n")
            .unwrap_err()
            .contains("sourcse"));
        assert!(parse("[win32]\nlink-libs = [\"x\"]\n")
            .unwrap_err()
            .contains("win32"));
    }

    #[test]
    fn link_libs_accept_names_and_library_paths() {
        let native =
            parse("link-libs = [\"crypto\", \"vendor/libfoo.a\", \"/usr/lib/libsodium.so.23\"]\n")
                .unwrap();
        let kinds: Vec<_> = native.link_libs.iter().map(|l| LinkLib::of(l)).collect();
        assert_eq!(
            kinds,
            [
                LinkLib::Name("crypto"),
                LinkLib::Path("vendor/libfoo.a"),
                LinkLib::Path("/usr/lib/libsodium.so.23"),
            ]
        );
    }
}
