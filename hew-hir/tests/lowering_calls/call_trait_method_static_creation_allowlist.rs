//! Structural enforcement for the planned retirement of
//! [`hew_hir::HirExprKind::CallTraitMethodStatic`].
//!
//! `HirExprKind::ResolvedImplCall` is the substrate replacement; the
//! legacy `CallTraitMethodStatic` variant is `#[deprecated]` pending
//! full migration of builtin generic dispatch (`HashMap`, `HashSet`,
//! and later `Vec` / `Option` / `Result`) off the string-keyed
//! `method_call_rewrites` path.
//!
//! This test snapshots the **surviving construction sites** (lines that
//! build a fresh `CallTraitMethodStatic` value, as opposed to merely
//! destructuring one in a walker). If a new construction site appears
//! anywhere in `hew-hir/src/`, this test fails with the new file/line
//! attached, forcing a conscious decision to extend the allowlist or
//! route the new dispatch through `ResolvedImplCall` instead.
//!
//! Without this gate the variant cannot be retired safely: new
//! producers can slip in unobserved between revisions.

use std::path::{Path, PathBuf};

const EXPECTED_CONSTRUCTION_SITE_COUNT: usize = 1;

/// Allowlisted construction sites. The path is relative to the
/// `hew-hir` crate root.
///
/// The current allowlist mirrors the only surviving producer: the
/// `MethodCallRewrite::StaticTraitDispatch` arm inside
/// `lower_method_call`. Once Vec / Option / Result dispatch migrates
/// off `method_call_rewrites`, this allowlist drains to empty.
const ALLOWED_CONSTRUCTION_SITES: &[&str] = &["src/lower.rs"];

/// Match the first token of a `CallTraitMethodStatic` CONSTRUCTION (struct
/// expression `HirExprKind::CallTraitMethodStatic {` immediately
/// followed by a field assignment line whose first non-whitespace
/// content is one of the known field names). Distinguishing
/// construction from destructuring is done by looking at the line
/// AFTER the marker: a construction assigns a value (`field: <expr>,`)
/// while a destructure binds a name (`field,` or `field: <pat>,`). The
/// simpler robust heuristic used here is: a CONSTRUCTION line is
/// followed within the next 8 lines by `receiver: Box::new(`. Walkers
/// destructure with `receiver, args, ..` or `receiver` and never call
/// `Box::new`.
fn is_construction_marker(window: &[&str]) -> bool {
    // Look forward up to 8 lines for `receiver: Box::new(` — the
    // unambiguous tell-tale of a fresh value construction.
    for line in window.iter().take(8) {
        if line.contains("receiver: Box::new(") {
            return true;
        }
    }
    false
}

#[test]
fn call_trait_method_static_creation_sites_are_allowlisted() {
    let crate_root: PathBuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let src_root = crate_root.join("src");

    let mut construction_sites: Vec<(String, usize)> = Vec::new();
    walk_rs_files(&src_root, &mut |rel_path: &Path, contents: &str| {
        let lines: Vec<&str> = contents.lines().collect();
        for (idx, line) in lines.iter().enumerate() {
            // The struct-expression marker:
            //     HirExprKind::CallTraitMethodStatic {
            // Walker destructures have `HirExprKind::CallTraitMethodStatic { receiver, args, .. }`
            // on a single line and are filtered out below; the
            // construction sites in lower.rs span across multiple lines
            // with field assignments.
            if line.contains("HirExprKind::CallTraitMethodStatic {")
                && !line.contains("..")
                // The `..` rest-pattern is a destructure tell. The
                // dump.rs site uses `..` explicitly; the walker sites
                // use it implicitly. Construction sites never elide
                // fields with `..`.
                && is_construction_marker(&lines[(idx + 1).min(lines.len())..])
            {
                let rel = rel_path.to_string_lossy().to_string();
                construction_sites.push((rel, idx + 1));
            }
        }
    });

    // Normalise to forward slashes for stable comparison across platforms.
    let observed: Vec<(String, usize)> = construction_sites
        .iter()
        .map(|(p, l)| (p.replace('\\', "/"), *l))
        .collect();

    let allowed: Vec<&str> = ALLOWED_CONSTRUCTION_SITES.to_vec();

    // Every observed construction site MUST live in an allowlisted file.
    let unauthorised: Vec<&(String, usize)> = observed
        .iter()
        .filter(|(path, _)| !allowed.iter().any(|allow| path == *allow))
        .collect();

    assert!(
        unauthorised.is_empty(),
        "Allowlist violation: new `HirExprKind::CallTraitMethodStatic` \
         construction sites detected outside the allowlist.\n\
         Unauthorised sites:\n  {}\n\n\
         Allowlist (relative to `hew-hir/`):\n  {}\n\n\
         Either route the new dispatch through `HirExprKind::ResolvedImplCall` (the \
         substrate that consumes `TypeCheckOutput::resolved_calls`), or, if this is \
         a genuinely new authority, extend `ALLOWED_CONSTRUCTION_SITES` in this test \
         with a justification.",
        unauthorised
            .iter()
            .map(|(p, l)| format!("{p}:{l}"))
            .collect::<Vec<_>>()
            .join("\n  "),
        allowed.join("\n  "),
    );

    // The allowlist must not include files that no longer construct the
    // variant — otherwise the gate rusts open. Once the variant is fully
    // retired, `ALLOWED_CONSTRUCTION_SITES` should be empty; until then
    // every allowlisted file must still produce at least one construction.
    for allow in &allowed {
        let hits = observed.iter().filter(|(p, _)| p == allow).count();
        assert!(
            hits > 0,
            "Allowlist drift: `{allow}` is listed in \
             ALLOWED_CONSTRUCTION_SITES but no `HirExprKind::CallTraitMethodStatic` \
             construction site was found there. Remove the stale entry — the \
             variant may already be retired in that file."
        );
    }

    // Snapshot the exact count so an in-place duplication (adding a
    // second producer inside an allowlisted file) is also flagged.
    // Update `EXPECTED_CONSTRUCTION_SITE_COUNT` (at the top of this file)
    // when the producer set shrinks or expands with a deliberate review.
    assert_eq!(
        observed.len(),
        EXPECTED_CONSTRUCTION_SITE_COUNT,
        "Allowlist snapshot mismatch: expected exactly \
         {EXPECTED_CONSTRUCTION_SITE_COUNT} `HirExprKind::CallTraitMethodStatic` \
         construction site(s), observed {} at:\n  {}\n\n\
         If you intentionally added or removed a producer, update \
         EXPECTED_CONSTRUCTION_SITE_COUNT with a justification.",
        observed.len(),
        observed
            .iter()
            .map(|(p, l)| format!("{p}:{l}"))
            .collect::<Vec<_>>()
            .join("\n  "),
    );
}

fn walk_rs_files<F>(root: &Path, visit: &mut F)
where
    F: FnMut(&Path, &str),
{
    let entries = std::fs::read_dir(root)
        .unwrap_or_else(|err| panic!("read_dir({}) failed: {err}", root.display()));
    for entry in entries {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        let ty = entry.file_type().expect("file_type");
        if ty.is_dir() {
            walk_rs_files(&path, visit);
        } else if path.extension().and_then(|s| s.to_str()) == Some("rs") {
            let contents = std::fs::read_to_string(&path)
                .unwrap_or_else(|err| panic!("read {} failed: {err}", path.display()));
            // Express the path relative to the crate root (one level
            // above `src/`) so the allowlist entries match what humans
            // read in error messages.
            let crate_root: PathBuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            let rel = path
                .strip_prefix(&crate_root)
                .unwrap_or(&path)
                .to_path_buf();
            visit(&rel, &contents);
        }
    }
}
