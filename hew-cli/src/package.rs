//! Input resolution shared by `hew build`, `hew run`, and `hew check`.
//!
//! Each of those commands accepts three forms: an explicit `.hew` file, a
//! directory, or nothing at all. The last two are package forms — the manifest
//! names the entry point, so the CLI resolves them here, through the single
//! `hew-pkg` authority, before any path reaches the compiler. A directory is
//! therefore never read as source.

use std::path::{Path, PathBuf};

use hew_pkg::project::{self, ResolveError, ResolvedPackage};

/// What the command was pointed at.
#[derive(Debug)]
pub enum Input {
    /// An explicit `.hew` source file, used verbatim.
    File(PathBuf),
    /// A package — resolved from a directory argument or the current directory.
    Package(Box<ResolvedPackage>),
}

impl Input {
    /// The source file to compile.
    pub fn source(&self) -> PathBuf {
        match self {
            Self::File(path) => path.clone(),
            Self::Package(pkg) => pkg.entry_path(),
        }
    }

    /// The resolved package, when the input named one.
    pub fn package(&self) -> Option<&ResolvedPackage> {
        match self {
            Self::File(_) => None,
            Self::Package(pkg) => Some(pkg),
        }
    }
}

/// A resolution failure and the exit code the command should return with.
pub struct Failure {
    /// Rendered diagnostic, printed after an `Error: ` prefix.
    pub message: String,
    /// `2` for usage errors (wrong place, wrong argument), `1` for a package
    /// that exists but is broken.
    pub code: i32,
}

/// Resolve the command's input argument.
///
/// `None` means the package enclosing the current directory; a directory means
/// the package rooted at or above it; anything else is taken as a source file.
///
/// # Errors
///
/// Returns [`Failure`] when the current directory is unavailable or the package
/// cannot be resolved.
pub fn resolve(input: Option<&Path>) -> Result<Input, Failure> {
    match input {
        Some(path) if !path.is_dir() => Ok(Input::File(path.to_path_buf())),
        Some(dir) => resolve_package(dir),
        None => {
            let cwd = std::env::current_dir().map_err(|e| Failure {
                message: format!("cannot read the current directory: {e}"),
                code: 1,
            })?;
            resolve_package(&cwd)
        }
    }
}

fn resolve_package(dir: &Path) -> Result<Input, Failure> {
    project::resolve_package(dir)
        .map(|pkg| Input::Package(Box::new(pkg)))
        .map_err(|error| Failure {
            code: exit_code(&error),
            message: error.to_string(),
        })
}

/// A missing manifest or a non-directory path is a usage error (exit 2); a
/// package that resolved but is malformed or incomplete is a real failure
/// (exit 1), matching what a broken source file returns.
fn exit_code(error: &ResolveError) -> i32 {
    match error {
        ResolveError::NotADirectory(_) | ResolveError::NoManifest(_) => 2,
        ResolveError::CurrentDirectory(_)
        | ResolveError::Manifest { .. }
        | ResolveError::MissingEntry { .. } => 1,
    }
}

/// Build the package's own `[native]` FFI crate — the prerequisite step of a
/// package build — and return the archive to add to the link line.
///
/// Returns nothing for a file input or a package with no `[native]` section.
/// Native libraries of *imported* packages are handled separately, by
/// [`crate::native_link`], off the resolved import graph.
///
/// # Errors
///
/// Returns the cargo build failure message.
pub fn native_link_libs(input: &Input) -> Result<Vec<String>, String> {
    let Some(pkg) = input.package() else {
        return Ok(Vec::new());
    };
    if !pkg.has_native() {
        return Ok(Vec::new());
    }
    let Some(path) = project::build_native_lib(&pkg.root)? else {
        return Ok(Vec::new());
    };
    let lib = path
        .to_str()
        .ok_or_else(|| format!("native library path is not valid UTF-8: {}", path.display()))?;
    Ok(vec![lib.to_string()])
}
