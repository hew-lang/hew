//! Rename analysis: validate rename sites and compute text edits.
//!
//! The headline entry point is [`plan_rename`], which returns either a
//! batch of [`RenameEdit`]s or a [`RenameError`] describing why the
//! rename was refused before any text edit was produced. Failure modes
//! are intentionally kept small — a keyword, an invalid
//! identifier, or a conflict with an existing binding in scope at one
//! of the rename sites.
//!
//! The legacy [`rename`] wrapper returns `Option<Vec<RenameEdit>>` and
//! is retained because existing callers (LSP handler, WASM tooling)
//! treat any failure as "no edits" and do not need to distinguish
//! causes. New callers should prefer [`plan_rename`].

use hew_parser::ast::{Expr, FnDecl, Item, Param, Pattern, Span, Stmt, TypeBodyItem, TypeExpr};
use hew_parser::ParseResult;

use crate::ast_visit::{self, AstVisitor, BindingInfo, VisitContext};
use crate::definition::find_matching_import;
use crate::definition::{find_definition, find_local_binding_definition, find_param_definition};
use crate::references::{find_all_references, is_top_level_name};
use crate::util::{simple_word_at_offset, word_at_offset};
use crate::{OffsetSpan, RenameConflict, RenameConflictKind, RenameEdit, RenameError};

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

/// Return `true` if `name` is a language keyword. Prelude functions are
/// ordinary lexical bindings and may be shadowed by user code (D554).
#[must_use]
pub fn is_builtin_name(name: &str) -> bool {
    hew_lexer::ALL_KEYWORDS.contains(&name)
}

/// Validate a proposed Hew identifier for a rename operation.
///
/// Prelude function names remain legal targets because they are lexical
/// bindings; only syntax keywords are reserved.
///
/// # Errors
///
/// Returns an error when `name` is not a Hew identifier or is a reserved keyword.
pub fn validate_new_name(name: &str) -> Result<(), RenameError> {
    if !is_valid_identifier(name) {
        return Err(RenameError::InvalidIdentifier {
            name: name.to_string(),
            message: format!("'{name}' is not a valid identifier"),
        });
    }
    if is_builtin_name(name) {
        return Err(RenameError::Builtin {
            name: name.to_string(),
            message: format!("cannot rename to '{name}': reserved keyword"),
        });
    }
    Ok(())
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

/// Recognize local annotation and field roles without resolving imports.
///
/// This preserves eligibility for local edits when project imports are incomplete.
/// A same-spelled declaration alone cannot prove an imported function reference
/// is local: the selected token must occupy the parsed non-function role.
/// Unproved expression leaves and named imports still require complete project
/// function identities and keep the operation in the conservative refusal path.
#[must_use]
pub fn is_local_non_function_reference(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
) -> bool {
    let Some((name, token)) = simple_word_at_offset(source, offset) else {
        return false;
    };
    let mut roles = LocalNonFunctionRoles {
        parse_result,
        name: &name,
        token,
        typed_bindings: std::collections::HashMap::new(),
        pending_fields: std::collections::HashMap::new(),
        found: false,
        ambiguous: false,
    };
    ast_visit::walk_parse_result(Some(source), parse_result, &mut roles);
    roles.found && !roles.ambiguous && roles.pending_fields.is_empty()
}

struct LocalNonFunctionRoles<'a> {
    parse_result: &'a ParseResult,
    name: &'a str,
    token: OffsetSpan,
    typed_bindings: std::collections::HashMap<usize, String>,
    pending_fields: std::collections::HashMap<usize, OffsetSpan>,
    found: bool,
    ambiguous: bool,
}

impl LocalNonFunctionRoles<'_> {
    fn local_type(&self, name: &str) -> bool {
        self.parse_result
            .program
            .items
            .iter()
            .any(|(item, _)| match item {
                Item::TypeDecl(item) => item.name.name.as_str() == name,
                Item::TypeAlias(item) => item.name.name.as_str() == name,
                Item::Record(item) => item.name.name.as_str() == name,
                Item::Actor(item) => item.name.name.as_str() == name,
                Item::Trait(item) => item.name.name.as_str() == name,
                Item::Machine(item) => item.name.name.as_str() == name,
                _ => false,
            })
    }

    fn annotation(&mut self, ty: &TypeExpr) {
        match ty {
            TypeExpr::Named { path, type_args } => {
                if let Some(name) = path.as_single() {
                    let span = &path.segments[0].1;
                    self.found |= name.name.as_str() == self.name
                        && span.start == self.token.start
                        && span.end == self.token.end
                        && self.local_type(self.name);
                }
                for (argument, _) in type_args.iter().flatten() {
                    self.annotation(argument);
                }
            }
            TypeExpr::Fallible {
                success: left,
                error: right,
            }
            | TypeExpr::Result {
                ok: left,
                err: right,
            } => {
                self.annotation(&left.0);
                self.annotation(&right.0);
            }
            TypeExpr::Option(inner) | TypeExpr::Slice(inner) | TypeExpr::Borrow(inner) => {
                self.annotation(&inner.0);
            }
            TypeExpr::Array { element, .. } => self.annotation(&element.0),
            TypeExpr::Pointer { pointee, .. } => self.annotation(&pointee.0),
            TypeExpr::Tuple(elements) => {
                for (element, _) in elements {
                    self.annotation(element);
                }
            }
            TypeExpr::Function {
                params,
                return_type,
                ..
            }
            | TypeExpr::ActorFn {
                params,
                return_type,
            } => {
                for (param, _) in params {
                    self.annotation(param);
                }
                self.annotation(&return_type.0);
            }
            TypeExpr::QualifiedAssocPath(path) => self.annotation(&path.base.0),
            TypeExpr::TraitObject(_) | TypeExpr::Infer => {}
        }
    }

    fn binding(&mut self, start: usize, ty: &TypeExpr) {
        self.annotation(ty);
        if let TypeExpr::Named { path, .. } = ty {
            if let Some(name) = path.as_single() {
                if self.local_type(name.name.as_str()) {
                    self.typed_bindings
                        .insert(start, name.name.as_str().to_string());
                }
            }
        }
    }

    fn signature(&mut self, params: &[Param], result: Option<&(TypeExpr, Span)>) {
        for param in params {
            self.binding(param.name_span.start, &param.ty.0);
        }
        if let Some((result, _)) = result {
            self.annotation(result);
        }
    }

    fn function(&mut self, function: &FnDecl) {
        self.signature(&function.params, function.return_type.as_ref());
    }

    fn nominal_has_field(&self, nominal: &str) -> bool {
        self.parse_result.program.items.iter().any(|(item, _)| {
            let Item::TypeDecl(item) = item else { return false; };
            item.name.name.as_str() == nominal && item.body.iter().any(|member| {
                matches!(member, TypeBodyItem::Field { name, .. } if name.name.as_str() == self.name)
            })
        })
    }
}

impl<'ast> AstVisitor<'ast> for LocalNonFunctionRoles<'_> {
    fn visit_item(&mut self, item: &'ast Item, _: &'ast Span, _: VisitContext<'ast>) {
        if self.ambiguous {
            return;
        }
        match item {
            Item::Function(function) => {
                self.ambiguous |=
                    function.visibility.is_pub() && function.name.name.as_str() == self.name;
                self.function(function);
            }
            Item::Import(import) => {
                if let Some(hew_parser::ast::ImportSpec::Names(names)) = &import.spec {
                    self.ambiguous |= names.iter().any(|name| {
                        name.name.name.as_str() == self.name
                            || name
                                .alias
                                .is_some_and(|alias| alias.name.as_str() == self.name)
                    });
                }
            }
            Item::TypeDecl(item) => {
                for member in &item.body {
                    match member {
                        TypeBodyItem::Field { ty, .. } => self.annotation(&ty.0),
                        TypeBodyItem::Method(function) => self.function(function),
                        TypeBodyItem::Variant(_) => {}
                    }
                }
            }
            Item::Actor(actor) => {
                for field in &actor.fields {
                    self.annotation(&field.ty.0);
                }
                if let Some(init) = &actor.init {
                    self.signature(&init.params, None);
                }
                for receive in &actor.receive_fns {
                    self.signature(&receive.params, receive.return_type.as_ref());
                }
                for method in &actor.methods {
                    self.function(method);
                }
            }
            Item::Impl(item) => {
                for method in &item.methods {
                    self.function(method);
                }
            }
            Item::Trait(item) => {
                for member in &item.items {
                    if let hew_parser::ast::TraitItem::Method(method) = member {
                        self.signature(&method.params, method.return_type.as_ref());
                    }
                }
            }
            Item::TypeAlias(item) => self.annotation(&item.ty.0),
            Item::Const(item) => self.annotation(&item.ty.0),
            Item::Record(item) => match &item.kind {
                hew_parser::ast::RecordKind::Named(fields) => {
                    for field in fields {
                        self.annotation(&field.ty.0);
                    }
                }
                hew_parser::ast::RecordKind::Tuple(fields) => {
                    for (ty, _) in fields {
                        self.annotation(ty);
                    }
                }
            },
            _ => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt, _: &'ast Span, _: VisitContext<'ast>) {
        if self.ambiguous {
            return;
        }
        match stmt {
            Stmt::Let {
                pattern,
                ty: Some(ty),
                ..
            } => {
                if matches!(pattern.0, Pattern::Identifier(_)) {
                    self.binding(pattern.1.start, &ty.0);
                } else {
                    self.annotation(&ty.0);
                }
            }
            Stmt::Var {
                name_span,
                ty: Some(ty),
                ..
            } => self.binding(name_span.start, &ty.0),
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr, _: &'ast Span, _: VisitContext<'ast>) {
        if self.ambiguous {
            return;
        }
        if matches!(expr, Expr::Ident(name) if name.name.as_str() == self.name) {
            self.ambiguous = true;
        }
        let Expr::FieldAccess { object, field } = expr else {
            return;
        };
        if field.0.name.as_str() != self.name {
            return;
        }
        if matches!(object.0, Expr::Ident(_)) {
            // The walker visits the receiver next with its scoped binding fact.
            self.pending_fields
                .insert(object.1.start, field.1.clone().into());
        } else {
            self.ambiguous = true;
        }
    }

    fn visit_identifier(
        &mut self,
        _: &'ast str,
        span: &'ast Span,
        binding: Option<BindingInfo<'ast>>,
        _: VisitContext<'ast>,
    ) {
        if self.ambiguous {
            return;
        }
        let Some(field) = self.pending_fields.remove(&span.start) else {
            return;
        };
        let local_field = binding
            .and_then(|binding| self.typed_bindings.get(&binding.span.start))
            .is_some_and(|nominal| self.nominal_has_field(nominal));
        // Any unproved leaf could be an exported function, so do not let a
        // proved local field lend its role to those other occurrences.
        self.ambiguous |= !local_field;
        self.found |= local_field && field == self.token;
    }
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
/// - [`RenameError::Builtin`] — `new_name` is a language keyword (e.g. `if`).
/// - [`RenameError::Conflicts`] — `new_name` already refers to a
///   binding in scope at one or more of the rename sites; applying
///   the rename would introduce a shadow.
pub fn plan_rename(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
    new_name: &str,
) -> Result<Vec<RenameEdit>, RenameError> {
    validate_new_name(new_name)?;
    if simple_word_at_offset(source, offset).is_none_or(|(name, _)| name == new_name) {
        return Ok(Vec::new());
    }
    let output = crate::identity::source_identities(parse_result);
    plan_rename_with_output(source, parse_result, &output, offset, new_name)
}

/// Plan a rename using the checker's recoverable declaration identities.
///
/// Diagnostics elsewhere in the document do not invalidate published facts.
/// A token without a checked identity produces no edits.
/// The output must describe this parsed buffer, focused to source module 0.
///
/// # Errors
///
/// Returns the same identifier and capture errors as [`plan_rename`].
pub fn plan_rename_with_output(
    source: &str,
    parse_result: &ParseResult,
    output: &hew_types::TypeCheckOutput,
    offset: usize,
    new_name: &str,
) -> Result<Vec<RenameEdit>, RenameError> {
    use hew_types::check::scope::Resolution;

    validate_new_name(new_name)?;
    let Some((name, token)) = simple_word_at_offset(source, offset) else {
        return Ok(Vec::new());
    };
    if name == new_name {
        return Ok(Vec::new());
    }
    let Some((_, resolution)) = crate::identity::resolution_at(output, 0, offset)
        .filter(|(span, _)| *span == token)
        .or_else(|| crate::identity::declaration_at(output, source, parse_result, offset))
    else {
        return Ok(Vec::new());
    };
    let is_local = matches!(resolution, Resolution::Local(_));
    let target = crate::identity::declaration_target(output, resolution);
    if !is_local && target.is_none() {
        return Ok(Vec::new());
    }
    let mut spans = crate::identity::reference_spans(output, 0, resolution);
    if let Some(target) = target {
        if target.occurrence.module() == output.defs.root_module() {
            if let Some(span) =
                crate::identity::declaration_name_span(source, parse_result, &target)
            {
                spans.push(span);
            }
        }
    }
    let shorthand = crate::identity::all_shorthand_label_spans(output, resolution)
        .into_iter()
        .filter_map(|(module, span)| (module == 0).then_some(span))
        .collect::<Vec<_>>();
    spans.extend(&shorthand);
    spans.sort_by_key(|span| (span.start, span.end));
    spans.dedup();
    let conflicts = detect_conflicts(source, parse_result, &spans, new_name, is_local);
    if !conflicts.is_empty() {
        return Err(RenameError::Conflicts { conflicts });
    }
    Ok(spans
        .into_iter()
        .filter(|span| source.get(span.start..span.end) == Some(name.as_str()))
        .map(|span| RenameEdit {
            new_text: if shorthand.contains(&span) {
                format!("{new_name}: {name}")
            } else if crate::identity::is_shorthand_label(output, 0, span) {
                format!("{name}: {new_name}")
            } else {
                new_name.to_string()
            },
            span,
        })
        .collect())
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
    fn local_rename_preserves_binding_trivia() {
        let source = "fn main() { let answer /* keep answer */ : i32 = 7; println(answer); }\n";
        let parsed = parse(source);
        assert!(parsed.errors.is_empty());
        let edits = plan_rename(source, &parsed, source.find("answer").unwrap(), "result").unwrap();
        assert_eq!(
            apply_edits(source, &edits),
            "fn main() { let result /* keep answer */ : i32 = 7; println(result); }\n"
        );
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
    fn local_non_function_roles_require_the_selected_annotation_or_typed_field() {
        let source = "import util.{ greet };\nimport missing;\ntype Thing { value: i32; greet: i32; }\ntype Other { value: i32; }\nfn read(item: Thing) -> i32 { let local: Thing = item; local.value + item.value }\nfn main() { println(util.greet()); println(greet()); }\n";
        let parsed = parse(source);
        assert!(parsed.errors.is_empty());
        for needle in ["Thing)", "Thing =", "value +", "value }"] {
            assert!(
                is_local_non_function_reference(source, &parsed, source.find(needle).unwrap()),
                "{needle}"
            );
        }
        for needle in ["greet };", "greet());", "greet: i32"] {
            assert!(
                !is_local_non_function_reference(source, &parsed, source.find(needle).unwrap()),
                "{needle}"
            );
        }
        assert!(!is_local_non_function_reference(
            source,
            &parsed,
            source.rfind("greet()").unwrap()
        ));
    }

    #[test]
    fn local_field_role_respects_receiver_shadowing_and_nominal_owner() {
        let source = "type Thing { value: i32; }\ntype Other { count: i32; }\nfn read(item: Thing) { { let item: Other; println(item.value); } println(item.value); }\n";
        let parsed = parse(source);
        assert!(parsed.errors.is_empty());
        assert!(!is_local_non_function_reference(
            source,
            &parsed,
            source.find("value);").unwrap()
        ));
        assert!(
            !is_local_non_function_reference(source, &parsed, source.rfind("value);").unwrap()),
            "an unproved peer occurrence prevents legacy name collection"
        );
        let unambiguous = "type Thing { value: i32; }\nfn read(item: Thing) { { let item: Thing; println(item.value); } println(item.value); }\n";
        let parsed = parse(unambiguous);
        assert!(is_local_non_function_reference(
            unambiguous,
            &parsed,
            unambiguous.rfind("value);").unwrap()
        ));
    }

    #[test]
    fn local_field_role_cannot_lend_identity_to_imported_function_uses() {
        for imported in ["import util.{ greet };", "import util as utility;"] {
            let source = format!("{imported}\nimport missing;\ntype Holder {{ greet: i32; }}\nfn read(item: Holder) -> i32 {{ item.greet }}\nfn main() {{ println(utility.greet()); println(greet()); }}\n");
            let parsed = parse(&source);
            assert!(parsed.errors.is_empty());
            assert!(!is_local_non_function_reference(
                &source,
                &parsed,
                source.find("greet }").unwrap()
            ));
        }
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
        let source = "type Point {\n    x: i32;\n    y: i32;\n}\n\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    let q = Point { x: 3, y: 4 };\n    p.x + q.x\n}\n";
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
        assert!(renamed.contains("type Point {\n    z: i32;\n    y: i32;\n}\n"));
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
    fn plan_rename_allows_prelude_shadow_without_capturing_a_call() {
        let source = "fn main() { let value = 1; value; }";
        let pr = parse(source);
        let offset = source.find("let value").unwrap() + 4;
        let edits = plan_rename(source, &pr, offset, "println").unwrap();
        assert_eq!(edits.len(), 2);
        assert!(edits.iter().all(|edit| edit.new_text == "println"));
    }

    #[test]
    fn plan_rename_allows_an_internal_endpoint_spelling() {
        let source = "fn main() { let value = 1; value; }";
        let pr = parse(source);
        let offset = source.find("let value").unwrap() + 4;
        let edits = plan_rename(source, &pr, offset, "hew_stream_next_layout").unwrap();
        assert_eq!(edits.len(), 2);
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
        let source = "fn main(a: i64) {\n    let x = 1;\n    x + a\n}";
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
        let source = "import other.{ bar };\nfn foo() -> i32 { 1 }\nfn main() { foo() }";
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

    // ── plan_rename: Actor.fields ShadowsTopLevel detection ──────────

    #[test]
    fn plan_rename_conflicts_with_actor_field_name() {
        // Renaming a top-level function to a name that is already used as an
        // actor field must be rejected with ShadowsTopLevel.  Prior to the
        // fix, find_definition skipped Actor.fields so detect_conflicts
        // silently bypassed the conflict check.
        let source = "actor Counter {\n    let count: i64;\n    receive fn inc() {}\n}\n\nfn foo() -> i64 {\n    0\n}\n";
        let pr = parse(source);
        let offset = source.find("fn foo").unwrap() + 3;
        let err = plan_rename(source, &pr, offset, "count").unwrap_err();
        match err {
            RenameError::Conflicts { conflicts } => {
                assert!(
                    conflicts
                        .iter()
                        .any(|c| c.kind == RenameConflictKind::ShadowsTopLevel),
                    "expected ShadowsTopLevel conflict for actor field name, got {conflicts:?}"
                );
            }
            other => panic!("expected Conflicts, got {other:?}"),
        }
    }
}
