//! Module/package visibility boundary predicates.
//!
//! Implements the three-level access model for Hew's `Visibility` enum:
//!
//! | `Visibility` | Allowed from                       |
//! |--------------|-------------------------------------|
//! | `Private`    | Same module only                    |
//! | `Package`    | Same package (shared parent dir)    |
//! | `Pub`        | Anywhere                            |
//!
//! "Same module" is vector equality on `ModuleId.path` segments.
//! "Same package" is the immediate parent-directory prefix: two modules
//! share a package iff their path vectors agree on every segment except
//! the last.  Root modules (`path = []`) are in a package by themselves.
//!
//! The predicates operate on dotted-string module paths (e.g. `"net.http"`)
//! which are the representation used throughout the checker (`current_module`,
//! `fn_def_spans` second element, `type_visibility` second element).

use hew_parser::ast::Visibility;

/// Convert a dotted module path string to a path segment vector.
///
/// `"net.http"` → `["net", "http"]`
/// `""` (empty / root) → `[]`
fn path_segments(dotted: &str) -> Vec<&str> {
    if dotted.is_empty() {
        vec![]
    } else {
        dotted.split('.').collect()
    }
}

/// Return true iff `decl_module` and `acc_module` are the same module.
///
/// Both arguments are dotted paths; `None` denotes the root compilation unit.
#[must_use]
pub(super) fn same_module(decl_module: Option<&str>, acc_module: Option<&str>) -> bool {
    // Root (None) is same-module only with itself.
    match (decl_module, acc_module) {
        (None, None) => true,
        (Some(d), Some(a)) => d == a,
        _ => false,
    }
}

/// Return true iff `decl_module` and `acc_module` are in the same package.
///
/// Package = the parent-directory path: all segments except the last.
/// Two modules share a package iff their paths agree up to the second-to-last
/// segment.  Root modules (`path = []`) are in a package with themselves only,
/// which is the same as `same_module` for flat-file programs.
///
/// Decision (from plan): package = immediate parent directory.  The spec says
/// "package is the directory the module file lives in", so `std::net::http`
/// → package `std::net`.  Two modules in the same package have paths that
/// share all segments except the last (their file stem).
#[must_use]
pub(super) fn same_package(decl_module: Option<&str>, acc_module: Option<&str>) -> bool {
    match (decl_module, acc_module) {
        // Root module is in a package with itself (same as same_module for root).
        (None, None) => true,
        (None, Some(_)) | (Some(_), None) => false,
        (Some(d), Some(a)) => {
            let d_segs = path_segments(d);
            let a_segs = path_segments(a);
            // Single-segment modules (e.g. "http") have parent package = [].
            // They share a package only if both are single-segment (both at
            // "package root level").  Two modules at "net.http" and "net.smtp"
            // share package "net" — their parent is the same.
            match (d_segs.len(), a_segs.len()) {
                (0, 0) => true,
                (1, 1) => {
                    // Both are top-level modules, no parent → same package only
                    // if they are the same module.
                    d == a
                }
                (d_len, a_len) if d_len >= 1 && a_len >= 1 => {
                    // Package = all segments except the last.
                    d_segs[..d_len - 1] == a_segs[..a_len - 1]
                }
                _ => false,
            }
        }
    }
}

/// Return true iff the accessor at `acc_module` is permitted to reference a
/// symbol declared with `vis` in `decl_module`.
///
/// - `Pub`     → always allowed.
/// - `Package` → allowed iff `same_package(decl_module, acc_module)`.
/// - `Private` → allowed iff `same_module(decl_module, acc_module)`.
///
/// `None` means the root compilation unit (single-file program or the
/// root module of a module graph).
#[must_use]
pub(super) fn access_allowed(
    decl_module: Option<&str>,
    acc_module: Option<&str>,
    vis: Visibility,
) -> bool {
    match vis {
        Visibility::Pub => true,
        Visibility::Package => same_package(decl_module, acc_module),
        Visibility::Private => same_module(decl_module, acc_module),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── same_module ──────────────────────────────────────────────────────────

    #[test]
    fn same_module_both_root() {
        assert!(same_module(None, None));
    }

    #[test]
    fn same_module_root_vs_non_root() {
        assert!(!same_module(None, Some("net")));
        assert!(!same_module(Some("net"), None));
    }

    #[test]
    fn same_module_identical_paths() {
        assert!(same_module(Some("net.http"), Some("net.http")));
        assert!(same_module(Some("std"), Some("std")));
    }

    #[test]
    fn same_module_different_paths() {
        assert!(!same_module(Some("net.http"), Some("net.smtp")));
        assert!(!same_module(Some("a"), Some("b")));
    }

    // ── same_package ─────────────────────────────────────────────────────────

    #[test]
    fn same_package_both_root() {
        assert!(same_package(None, None));
    }

    #[test]
    fn same_package_root_vs_module() {
        assert!(!same_package(None, Some("net")));
        assert!(!same_package(Some("net"), None));
    }

    #[test]
    fn same_package_sibling_modules() {
        // net.http and net.smtp share package "net" → same package
        assert!(same_package(Some("net.http"), Some("net.smtp")));
    }

    #[test]
    fn same_package_same_module_counts_as_same_package() {
        assert!(same_package(Some("net.http"), Some("net.http")));
    }

    #[test]
    fn same_package_cross_package() {
        // std.net.http vs std.io.fs — parent of net.http is std.net; parent of io.fs is std.io
        assert!(!same_package(Some("std.net.http"), Some("std.io.fs")));
    }

    #[test]
    fn same_package_top_level_modules_are_not_same_package() {
        // "http" and "smtp" are both top-level modules; they have no shared parent
        assert!(!same_package(Some("http"), Some("smtp")));
    }

    #[test]
    fn same_package_top_level_module_with_itself() {
        // A module is in the same package as itself
        assert!(same_package(Some("http"), Some("http")));
    }

    // ── access_allowed ───────────────────────────────────────────────────────

    #[test]
    fn access_allowed_pub_always() {
        assert!(access_allowed(
            Some("mod_a"),
            Some("mod_b"),
            Visibility::Pub
        ));
        assert!(access_allowed(None, Some("other"), Visibility::Pub));
        assert!(access_allowed(Some("a.b"), None, Visibility::Pub));
    }

    #[test]
    fn access_allowed_private_same_module_ok() {
        assert!(access_allowed(
            Some("net.http"),
            Some("net.http"),
            Visibility::Private
        ));
        assert!(access_allowed(None, None, Visibility::Private));
    }

    #[test]
    fn access_allowed_private_cross_module_rejected() {
        assert!(!access_allowed(
            Some("net.http"),
            Some("net.smtp"),
            Visibility::Private
        ));
        assert!(!access_allowed(None, Some("net"), Visibility::Private));
    }

    #[test]
    fn access_allowed_package_same_package_ok() {
        // Sibling modules share a package
        assert!(access_allowed(
            Some("net.http"),
            Some("net.smtp"),
            Visibility::Package
        ));
    }

    #[test]
    fn access_allowed_package_cross_package_rejected() {
        assert!(!access_allowed(
            Some("net.http"),
            Some("io.fs"),
            Visibility::Package
        ));
    }

    #[test]
    fn access_allowed_package_cross_module_within_same_package_ok() {
        // Same module is trivially same package
        assert!(access_allowed(
            Some("net.http"),
            Some("net.http"),
            Visibility::Package
        ));
    }
}
