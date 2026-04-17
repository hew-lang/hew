//! Rename analysis: validate rename sites and compute text edits.
//!
//! The headline entry point is [`plan_rename`], which returns either a
//! batch of [`RenameEdit`]s or a [`RenameError`] describing why the
//! rename was refused before any text edit was produced. Failure modes
//! are intentionally kept small — a keyword / builtin clash, an invalid
//! identifier, or a conflict with an existing binding in scope at one
//! of the rename sites.
//!
//! The legacy [`rename`] wrapper returns `Option<Vec<RenameEdit>>` and
//! is retained because existing callers (LSP handler, WASM tooling)
//! treat any failure as "no edits" and do not need to distinguish
//! causes. New callers should prefer [`plan_rename`].

use hew_parser::ParseResult;

use crate::definition::{find_definition, find_local_binding_definition, find_param_definition};
use crate::references::{find_all_references, is_top_level_name};
use crate::resolver::find_matching_import;
use crate::util::{simple_word_at_offset, word_at_offset};
use crate::{OffsetSpan, RenameConflict, RenameConflictKind, RenameEdit, RenameError};

/// Identifier names that are resolved by the compiler independent of
/// user code. Renaming to one of these would shadow a global reachable
/// anywhere; the heuristic rejects it pre-emptively.
///
/// SHIM: hard-coded list; a complete reflection of the builtin registry
/// requires Lane 1B type-checker introspection. Until then, this list
/// covers the most commonly-collided names; a user attempting to rename
/// into an unlisted builtin will parse-fail rather than be blocked at
/// rename time. Every name here is verified against `register_builtin_fn`
/// calls in `hew-types/src/check/registration.rs` (the canonical registry).
const BUILTIN_FUNCTION_NAMES: &[&str] = &[
    // Registered via register_builtin_fn in hew-types/src/check/registration.rs.
    // SHIM: hard-coded until Lane 1B's type-checker introspection lands.
    // Core I/O
    "print",
    "println",
    "panic",
    "assert",
    // Assertions
    "assert_eq",
    "assert_ne",
    // Math
    "abs",
    "sqrt",
    // String / collection
    "len",
    "to_string",
    // System / process
    "exit",
    "stop",
    "close",
    "sleep",
    "sleep_ms",
    // File I/O
    "read_file",
    "write_file",
    // Actor fault-propagation
    "link",
    "unlink",
    "monitor",
    "demonitor",
];

/// Return `true` if `name` is a syntactically valid Hew identifier.
///
/// Must start with `_` or an alphabetic character and continue with
/// identifier characters only. The empty string is rejected.
pub(crate) fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|c| c == '_' || c.is_ascii_alphanumeric())
}

/// Return `true` if `name` clashes with a language keyword or a
/// compiler-intrinsic name. Used by [`plan_rename`] to refuse renames
/// to names that would shadow builtins.
#[must_use]
pub fn is_builtin_name(name: &str) -> bool {
    if hew_lexer::ALL_KEYWORDS.contains(&name) {
        return true;
    }
    BUILTIN_FUNCTION_NAMES.contains(&name)
}

/// Check whether rename is valid at `offset`. Returns the word span if yes.
///
/// Returns `None` if the cursor is not on an identifier, the identifier contains
/// a dot or `::` qualifier, or neither a definition nor any references exist for the name.
#[must_use]
pub fn prepare_rename(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
) -> Option<OffsetSpan> {
    let word = word_at_offset(source, offset)?;
    if word.contains('.') || word.contains("::") {
        return None;
    }
    if find_all_references(source, parse_result, offset).is_none()
        && find_definition(source, parse_result, &word).is_none()
    {
        return None;
    }
    let (_word, span) = simple_word_at_offset(source, offset)?;
    Some(span)
}

/// Compute rename edits for the symbol at `offset`, replaced with `new_name`.
///
/// Legacy entry point that silently returns `None` for every failure
/// mode — invalid name, no symbol at offset, no references or
/// definition found, conflict with an existing binding. It does not
/// distinguish causes.
///
/// New callers should prefer [`plan_rename`], which surfaces the
/// reason for failure and checks that `new_name` does not clash with
/// an existing binding.
#[must_use]
pub fn rename(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
    new_name: &str,
) -> Option<Vec<RenameEdit>> {
    plan_rename(source, parse_result, offset, new_name)
        .ok()
        .filter(|edits| !edits.is_empty())
}

/// Plan a rename: compute edits, or return a structured reason for
/// refusing the rename.
///
/// On success, returns the sorted, de-duplicated list of edits for the
/// current file. If the symbol has no references and no definition in
/// the current file, returns an empty `Vec` (not an error).
///
/// # Errors
///
/// Returns a [`RenameError`] describing why the rename was refused:
/// - [`RenameError::InvalidIdentifier`] — `new_name` is not a valid
///   Hew identifier.
/// - [`RenameError::Builtin`] — `new_name` is a language keyword or a
///   compiler-intrinsic name (e.g. `println`, `if`). These would
///   shadow a global.
/// - [`RenameError::Conflicts`] — `new_name` already refers to a
///   binding in scope at one or more of the rename sites; applying
///   the rename would introduce a shadow.
pub fn plan_rename(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
    new_name: &str,
) -> Result<Vec<RenameEdit>, RenameError> {
    if !is_valid_identifier(new_name) {
        return Err(RenameError::InvalidIdentifier {
            name: new_name.to_string(),
            message: format!("'{new_name}' is not a valid identifier"),
        });
    }

    if is_builtin_name(new_name) {
        return Err(RenameError::Builtin {
            name: new_name.to_string(),
            message: format!("cannot rename to '{new_name}': reserved keyword or builtin name"),
        });
    }

    let Some((name, def_word_span)) = simple_word_at_offset(source, offset) else {
        return Ok(Vec::new());
    };

    // Renaming to the same name is a no-op.
    if name == new_name {
        return Ok(Vec::new());
    }

    let mut spans = find_all_references(source, parse_result, offset)
        .map(|(_, spans)| spans)
        .unwrap_or_default();

    if let Some(def_span) = find_definition(source, parse_result, &name) {
        if !spans
            .iter()
            .any(|s| s.start == def_span.start && s.end == def_span.end)
        {
            spans.push(def_span);
        }
    }

    if spans.is_empty() {
        return Ok(Vec::new());
    }

    // Classify the rename target to scope conflict detection correctly.
    let is_local = find_local_binding_definition(source, parse_result, &name, offset).is_some()
        || find_param_definition(parse_result, &name, offset).is_some();

    let mut conflicts = detect_conflicts(source, parse_result, &spans, new_name, is_local);
    // If the cursor is on a reference-only site with no corresponding
    // declaration in `spans`, re-check at the cursor offset itself so a
    // shadow in the cursor's scope is not missed.
    if conflicts.is_empty() && !spans.iter().any(|s| s.start == def_word_span.start) {
        conflicts = detect_conflicts(
            source,
            parse_result,
            std::slice::from_ref(&def_word_span),
            new_name,
            is_local,
        );
    }

    if !conflicts.is_empty() {
        return Err(RenameError::Conflicts { conflicts });
    }

    let mut edits: Vec<RenameEdit> = spans
        .into_iter()
        .map(|span| RenameEdit {
            span,
            new_text: new_name.to_string(),
        })
        .collect();

    edits.sort_by_key(|e| (e.span.start, e.span.end));
    edits.dedup_by(|a, b| a.span.start == b.span.start && a.span.end == b.span.end);

    Ok(edits)
}

/// Detect whether applying the rename at `sites` would collide with an
/// existing binding named `new_name`.
///
/// For local renames, a conflict is any in-scope local/param named
/// `new_name` at any rename site. For top-level renames, a conflict is
/// an existing top-level item or import named `new_name` in the same
/// file (the top-level check is position-independent, so we report it
/// once against the first site).
fn detect_conflicts(
    source: &str,
    parse_result: &ParseResult,
    sites: &[OffsetSpan],
    new_name: &str,
    is_local: bool,
) -> Vec<RenameConflict> {
    let mut conflicts = Vec::new();

    if is_local {
        for site in sites {
            if let Some(existing) =
                find_local_binding_definition(source, parse_result, new_name, site.start)
            {
                push_conflict(
                    &mut conflicts,
                    RenameConflictKind::ShadowsLocal,
                    existing,
                    *site,
                    format!("renaming would shadow existing local '{new_name}' in the same scope"),
                );
                continue;
            }
            if let Some(existing) = find_param_definition(parse_result, new_name, site.start) {
                push_conflict(
                    &mut conflicts,
                    RenameConflictKind::ShadowsLocal,
                    existing,
                    *site,
                    format!(
                        "renaming would shadow existing parameter '{new_name}' in the same scope"
                    ),
                );
            }
        }
    } else if is_top_level_name(parse_result, new_name) {
        if let Some(existing) = find_definition(source, parse_result, new_name) {
            let offending = sites.first().copied().unwrap_or(existing);
            push_conflict(
                &mut conflicts,
                RenameConflictKind::ShadowsTopLevel,
                existing,
                offending,
                format!("renaming would clash with existing top-level '{new_name}' in this file"),
            );
        }
    } else if let Some(existing) = find_matching_import(parse_result, new_name) {
        let offending = sites.first().copied().unwrap_or(existing);
        push_conflict(
            &mut conflicts,
            RenameConflictKind::ShadowsImport,
            existing,
            offending,
            format!("renaming would clash with imported '{new_name}' in this file"),
        );
    }

    // For top-level renames the file-level checks above handle structural
    // collisions. But each individual call site may sit inside a function
    // body where a local variable or parameter named `new_name` is in scope.
    // If the call is rewritten there the local would shadow the renamed
    // top-level symbol at that site — detect that per-site even when the
    // symbol itself is not a local binding.
    if !is_local {
        for site in sites {
            if let Some(existing) =
                find_local_binding_definition(source, parse_result, new_name, site.start)
            {
                push_conflict(
                    &mut conflicts,
                    RenameConflictKind::ShadowsLocal,
                    existing,
                    *site,
                    format!("renaming would shadow local '{new_name}' in scope at a call site"),
                );
                continue;
            }
            if let Some(existing) = find_param_definition(parse_result, new_name, site.start) {
                push_conflict(
                    &mut conflicts,
                    RenameConflictKind::ShadowsLocal,
                    existing,
                    *site,
                    format!("renaming would shadow parameter '{new_name}' in scope at a call site"),
                );
            }
        }
    }

    conflicts
}

fn push_conflict(
    conflicts: &mut Vec<RenameConflict>,
    kind: RenameConflictKind,
    existing: OffsetSpan,
    offending: OffsetSpan,
    message: String,
) {
    // Deduplicate on (existing, offending) — iterating sites in a local
    // rename will otherwise report the same pre-existing binding many
    // times when the cursor moves across its usages.
    if conflicts
        .iter()
        .any(|c| c.existing_span == existing && c.offending_span == offending)
    {
        return;
    }
    conflicts.push(RenameConflict {
        kind,
        existing_span: existing,
        offending_span: offending,
        message,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::Reverse;

    fn parse(source: &str) -> hew_parser::ParseResult {
        hew_parser::parse(source)
    }

    fn apply_edits(source: &str, edits: &[RenameEdit]) -> String {
        let mut updated = source.to_string();
        let mut ordered: Vec<_> = edits.iter().collect();
        ordered.sort_by_key(|edit| Reverse(edit.span.start));
        for edit in ordered {
            updated.replace_range(edit.span.start..edit.span.end, &edit.new_text);
        }
        updated
    }

    #[test]
    fn rename_local_variable() {
        let source = "fn main() {\n    let x = 1;\n    let y = x + 2;\n}";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let result = rename(source, &pr, offset, "z");
        assert!(result.is_some(), "should produce rename edits");
        let edits = result.unwrap();
        assert!(
            edits.len() >= 2,
            "should rename definition and usage, got {}",
            edits.len()
        );
        for edit in &edits {
            assert_eq!(edit.new_text, "z");
        }
    }

    #[test]
    fn prepare_rename_at_whitespace() {
        let source = "fn main() { }";
        let pr = parse(source);
        let offset = source.find("{ }").unwrap() + 1;
        let result = prepare_rename(source, &pr, offset);
        assert!(result.is_none(), "cannot rename at whitespace");
    }

    #[test]
    fn prepare_rename_on_definition_without_local_references() {
        let source = "fn main() { }";
        let pr = parse(source);
        let offset = source.find("main").unwrap();
        let result = prepare_rename(source, &pr, offset);
        assert!(
            result.is_some(),
            "definition-only symbols should still be renameable"
        );
    }

    #[test]
    fn prepare_rename_rejects_qualified_name() {
        let source = "fn main() {\n    foo.bar();\n}";
        let pr = parse(source);
        let offset = source.find("bar").unwrap();
        let result = prepare_rename(source, &pr, offset);
        assert!(result.is_none(), "cannot rename qualified name");
    }

    #[test]
    fn rename_function_name() {
        let source = "fn greet() {}\nfn main() {\n    greet()\n}";
        let pr = parse(source);
        let offset = source.find("greet").unwrap();
        let result = rename(source, &pr, offset, "hello");
        assert!(result.is_some(), "should produce rename edits for function");
        let edits = result.unwrap();
        for edit in &edits {
            assert_eq!(edit.new_text, "hello");
        }
        assert!(
            edits.len() >= 2,
            "should rename at definition and call site, got {}",
            edits.len()
        );
    }

    #[test]
    fn prepare_rename_returns_span() {
        let source = "fn main() {\n    let x = 1;\n    let y = x + 2;\n}";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let result = prepare_rename(source, &pr, offset);
        assert!(result.is_some(), "prepare_rename should return a span");
        let span = result.unwrap();
        assert_eq!(&source[span.start..span.end], "x");
    }

    #[test]
    fn rename_struct_field_updates_declaration_and_accesses() {
        let source = "type Point { x: i32; y: i32 }\nfn main() { let p = Point { x: 1, y: 2 }; let q = Point { x: 3, y: 4 }; p.x + q.x }";
        let pr = parse(source);
        let offset = source.find("p.x").unwrap() + 2;
        let edits = rename(source, &pr, offset, "z").expect("should rename struct field");

        assert_eq!(
            edits.len(),
            5,
            "should rename declaration, both struct init fields, and both accesses"
        );
        assert!(edits.iter().all(|edit| edit.new_text == "z"));

        let decl_start = source.find("x: i32").unwrap();
        assert!(edits.iter().any(|edit| edit.span.start == decl_start));

        let renamed = apply_edits(source, &edits);
        assert!(renamed.contains("type Point { z: i32; y: i32 }"));
        assert!(renamed.contains("Point { z: 1, y: 2 }"));
        assert!(renamed.contains("Point { z: 3, y: 4 }"));
        assert!(renamed.contains("p.z + q.z"));
    }

    // ── plan_rename: validation ────────────────────────────────────

    #[test]
    fn plan_rename_rejects_keyword() {
        let source = "fn main() { let x = 1; }";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let err = plan_rename(source, &pr, offset, "fn").unwrap_err();
        assert!(matches!(err, RenameError::Builtin { .. }));
    }

    #[test]
    fn plan_rename_rejects_builtin_function_name() {
        let source = "fn main() { let x = 1; }";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let err = plan_rename(source, &pr, offset, "println").unwrap_err();
        assert!(
            matches!(err, RenameError::Builtin { ref name, .. } if name == "println"),
            "expected Builtin for println, got {err:?}"
        );
    }

    #[test]
    fn plan_rename_rejects_invalid_identifier() {
        let source = "fn main() { let x = 1; }";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let err = plan_rename(source, &pr, offset, "1bad").unwrap_err();
        assert!(matches!(err, RenameError::InvalidIdentifier { .. }));
    }

    #[test]
    fn plan_rename_rejects_empty_new_name() {
        let source = "fn main() { let x = 1; }";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let err = plan_rename(source, &pr, offset, "").unwrap_err();
        assert!(matches!(err, RenameError::InvalidIdentifier { .. }));
    }

    #[test]
    fn rejects_unicode_letters_not_in_ascii() {
        // The Hew lexer is ASCII-only; names like `héllo` must be rejected
        // even though `é` is alphabetic in Unicode.
        assert!(
            !is_valid_identifier("héllo"),
            "non-ASCII alphabetic must be rejected"
        );
        assert!(
            !is_valid_identifier("naïve"),
            "non-ASCII letter in body must be rejected"
        );
        assert!(
            !is_valid_identifier("Ångström"),
            "non-ASCII first char must be rejected"
        );
        // ASCII identifiers must still be accepted.
        assert!(is_valid_identifier("hello"), "ASCII ident must be accepted");
        assert!(
            is_valid_identifier("_foo"),
            "underscore prefix must be accepted"
        );
        assert!(
            is_valid_identifier("x1"),
            "alphanumeric body must be accepted"
        );
    }

    #[test]
    fn plan_rename_same_name_is_noop() {
        let source = "fn main() { let x = 1; x + 2 }";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let edits = plan_rename(source, &pr, offset, "x").unwrap();
        assert!(
            edits.is_empty(),
            "rename to same name should produce no edits"
        );
    }

    // ── plan_rename: local conflict detection ──────────────────────

    #[test]
    fn plan_rename_local_shadow_returns_conflict() {
        // Rename `x` to `y` but `y` is already declared in the same scope.
        let source = "fn main() {\n    let x = 1;\n    let y = 2;\n    x + y\n}";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let err = plan_rename(source, &pr, offset, "y").unwrap_err();
        match err {
            RenameError::Conflicts { conflicts } => {
                assert!(!conflicts.is_empty(), "expected at least one conflict");
                assert_eq!(conflicts[0].kind, RenameConflictKind::ShadowsLocal);
                assert!(conflicts[0].message.contains("shadow"));
            }
            other => panic!("expected Conflicts, got {other:?}"),
        }
    }

    #[test]
    fn plan_rename_param_shadow_returns_conflict() {
        // Rename a local `x` to a parameter name `a` that's in scope.
        let source = "fn main(a: int) {\n    let x = 1;\n    x + a\n}";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let err = plan_rename(source, &pr, offset, "a").unwrap_err();
        assert!(matches!(err, RenameError::Conflicts { .. }));
    }

    #[test]
    fn plan_rename_local_in_one_fn_does_not_affect_another() {
        // Two separate `let x`s in distinct fns; renaming one must not
        // touch the other.
        let source = "fn a() { let x = 1; x }\nfn b() { let x = 2; x }";
        let pr = parse(source);
        let offset = source.find("fn a()").unwrap() + "fn a() { let ".len();
        let edits =
            plan_rename(source, &pr, offset, "y").expect("should succeed: different scopes");
        let applied = apply_edits(source, &edits);
        // `fn a`'s `let x` and sole usage should be renamed; `fn b`'s
        // binding must be entirely untouched.
        assert!(
            applied.starts_with("fn a() { let y"),
            "fn a's let should be renamed: {applied}"
        );
        assert!(
            applied.contains("fn b() { let x = 2; x }"),
            "fn b's binding must be preserved: {applied}"
        );
        // No rename edit should land in the second function's byte range.
        let b_start = source.find("fn b()").unwrap();
        assert!(
            edits.iter().all(|e| e.span.start < b_start),
            "all edits must be inside fn a's range; got {edits:?}"
        );
    }

    #[test]
    fn plan_rename_top_level_conflict_with_existing_item() {
        let source = "fn greet() {}\nfn other() {}";
        let pr = parse(source);
        let offset = source.find("fn greet").unwrap() + 3;
        let err = plan_rename(source, &pr, offset, "other").unwrap_err();
        match err {
            RenameError::Conflicts { conflicts } => {
                assert_eq!(conflicts[0].kind, RenameConflictKind::ShadowsTopLevel);
            }
            other => panic!("expected Conflicts, got {other:?}"),
        }
    }

    #[test]
    fn plan_rename_top_level_happy_path() {
        let source = "fn greet() {}\nfn main() { greet() }";
        let pr = parse(source);
        let offset = source.find("fn greet").unwrap() + 3;
        let edits = plan_rename(source, &pr, offset, "hello").expect("should succeed");
        assert!(edits.len() >= 2);
        let applied = apply_edits(source, &edits);
        assert!(applied.contains("fn hello"));
        assert!(applied.contains("hello()"));
    }

    // ── plan_rename: ShadowsImport detection ──────────────────────────

    #[test]
    fn plan_rename_same_file_shadows_import_returns_conflict() {
        // Rename the top-level `foo` to `bar`, but `bar` is already imported
        // in the same file.  Detect before producing any edit.
        let source = "import other::{ bar };\nfn foo() -> i32 { 1 }\nfn main() { foo() }";
        let pr = parse(source);
        let offset = source.find("fn foo").unwrap() + 3;
        let err = plan_rename(source, &pr, offset, "bar").unwrap_err();
        match err {
            RenameError::Conflicts { conflicts } => {
                assert!(
                    conflicts
                        .iter()
                        .any(|c| c.kind == RenameConflictKind::ShadowsImport),
                    "expected ShadowsImport conflict, got {conflicts:?}"
                );
            }
            other => panic!("expected Conflicts, got {other:?}"),
        }
    }
}
