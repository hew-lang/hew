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
/// Two modules share a package iff their parent segment vectors are equal.
///
/// Decision: package = immediate parent directory.  `std::net::http`
/// → package `["std", "net"]`.  Single-segment modules (e.g. `"mod_a"`)
/// have parent `[]` — the root package — which is also the parent of the
/// root compilation unit (`None`).  Therefore a root file and its sibling
/// top-level module files share the root package, as do any two top-level
/// sibling module files in the same directory.
#[must_use]
pub(super) fn same_package(decl_module: Option<&str>, acc_module: Option<&str>) -> bool {
    parent_package(decl_module) == parent_package(acc_module)
}

/// The package (parent-directory) of a module, as a segment vector.
///
/// - `None` (root compilation unit) → `[]`
/// - `Some("")` (unusual empty path) → `[]`
/// - `Some("mod_a")` (single segment) → `[]` (lives in the root package)
/// - `Some("net.http")` (multi-segment) → `["net"]`
fn parent_package(module: Option<&str>) -> Vec<&str> {
    match module {
        None => vec![],
        Some(m) => {
            let segs = path_segments(m);
            if segs.len() <= 1 {
                // Single-segment or empty → root package.
                vec![]
            } else {
                segs[..segs.len() - 1].to_vec()
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
    fn same_package_root_vs_top_level_module() {
        // Root file (None) and a top-level sibling module share the root package.
        assert!(same_package(None, Some("net")));
        assert!(same_package(Some("net"), None));
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
    fn same_package_top_level_modules_share_root_package() {
        // "http" and "smtp" are both single-segment modules → parent = [] → same root package
        assert!(same_package(Some("http"), Some("smtp")));
    }

    #[test]
    fn same_package_top_level_module_with_itself() {
        // A module is in the same package as itself
        assert!(same_package(Some("http"), Some("http")));
    }

    #[test]
    fn same_package_root_vs_root() {
        assert!(same_package(None, None));
    }

    #[test]
    fn same_package_top_level_cross_package_nested() {
        // A nested module and a top-level module are NOT in the same package.
        // "net.http" has parent ["net"]; root has parent [] → different packages.
        assert!(!same_package(Some("net.http"), None));
        assert!(!same_package(None, Some("net.http")));
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
