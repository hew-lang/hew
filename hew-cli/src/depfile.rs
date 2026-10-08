//! `--emit-deps`: a Makefile-format dependency file naming every file a build
//! read, so `make` rebuilds a Hew binary exactly when one of them changes.
//!
//! The file holds one rule, `<target>: <prerequisites>`, followed by an empty
//! rule per prerequisite (as `cc -MP` writes) so deleting a header or module
//! does not break the next `make` run. Standard-library sources are left out,
//! as `cc -MMD` leaves out system headers.

use std::path::{Path, PathBuf};

use crate::native_link::ProgramInputs;

/// The prerequisites of a build: Hew sources, each owning package's manifest
/// and lockfile, then the native sources, headers and Rust sources its
/// `[native]` build read. Each appears once, in that order.
pub fn prerequisites(inputs: &ProgramInputs, native: &[PathBuf]) -> Vec<PathBuf> {
    let mut out: Vec<PathBuf> = Vec::new();
    for path in inputs
        .sources
        .iter()
        .cloned()
        .chain(inputs.manifests())
        .chain(native.iter().cloned())
    {
        if !out.contains(&path) {
            out.push(path);
        }
    }
    out
}

/// Write the dependency file for `target` at `path`.
///
/// # Errors
///
/// Returns the I/O failure.
pub fn write(path: &Path, target: &Path, prerequisites: &[PathBuf]) -> std::io::Result<()> {
    let cwd = std::env::current_dir().ok();
    let rendered: Vec<String> = prerequisites
        .iter()
        .map(|dep| escape(&relative(dep, cwd.as_deref())))
        .collect();
    let mut text = escape(&relative(target, cwd.as_deref()));
    text.push(':');
    for dep in &rendered {
        text.push_str(" \\\n  ");
        text.push_str(dep);
    }
    text.push('\n');
    for dep in &rendered {
        text.push('\n');
        text.push_str(dep);
        text.push_str(":\n");
    }
    if let Some(parent) = path.parent().filter(|p| !p.as_os_str().is_empty()) {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, text)
}

/// `path` relative to the working directory when it lies below it, so the
/// rule reads like the Makefile that includes it.
fn relative(path: &Path, cwd: Option<&Path>) -> PathBuf {
    let Some(cwd) = cwd else {
        return path.to_path_buf();
    };
    if let Ok(rest) = path.strip_prefix(cwd) {
        return rest.to_path_buf();
    }
    // Sources arrive canonical; the working directory may be spelled through
    // a symbolic link.
    cwd.canonicalize()
        .ok()
        .and_then(|cwd| path.strip_prefix(cwd).ok().map(Path::to_path_buf))
        .unwrap_or_else(|| path.to_path_buf())
}

/// Spell `path` for Make: `/` separators, and `\ `, `\#` and `$$` for the
/// characters Make would otherwise split on or expand.
fn escape(path: &Path) -> String {
    let text = path.to_string_lossy();
    let mut out = String::with_capacity(text.len());
    for c in text.chars() {
        match c {
            ' ' => out.push_str("\\ "),
            '#' => out.push_str("\\#"),
            '$' => out.push_str("$$"),
            '\\' if cfg!(windows) => out.push('/'),
            c => out.push(c),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_lists_each_prerequisite_with_an_empty_rule() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("out.d");
        write(
            &path,
            Path::new("build/app"),
            &[PathBuf::from("main.hew"), PathBuf::from("src/my bridge.c")],
        )
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(&path).unwrap(),
            "build/app: \\\n  main.hew \\\n  src/my\\ bridge.c\n\nmain.hew:\n\nsrc/my\\ bridge.c:\n"
        );
    }

    #[test]
    fn make_metacharacters_are_escaped() {
        assert_eq!(escape(Path::new("a$b#c d")), "a$$b\\#c\\ d");
    }
}
