//! Find-all-references analysis: scope-aware identifier reference collection.

use std::collections::HashMap;

use hew_parser::ast::{
    Block, Expr, Item, Pattern, Span, Stmt, StringPart, TraitItem, TypeBodyItem,
};
use hew_parser::ParseResult;

use crate::ast_visit::{self, AstVisitor, BindingInfo, VisitContext};
use crate::util::simple_word_at_offset;
use crate::OffsetSpan;

// ── Public API ──────────────────────────────────────────────────────

/// Walk the entire AST collecting all spans where the identifier at `offset`
/// appears as a reference. Uses scope-aware resolution: for local variables,
/// only collects references that refer to the same binding as the one at the
/// cursor position.
///
/// Returns `None` if no identifier is found at `offset` or no references exist.
#[must_use]
pub fn find_all_references(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
) -> Option<(String, Vec<OffsetSpan>)> {
    let (name, spans) = find_all_references_raw(source, parse_result, offset)?;
    let offset_spans = spans
        .into_iter()
        .map(|s| OffsetSpan {
            start: s.start,
            end: s.end,
        })
        .collect();
    Some((name, offset_spans))
}

/// Walk item bodies collecting references that resolve to an imported module-scope
/// binding named `name`.
///
/// If the current file also defines a top-level item with the same name, the
/// imported binding is considered hidden and no reference spans are returned.
#[must_use]
pub fn find_import_binding_references(parse_result: &ParseResult, name: &str) -> Vec<OffsetSpan> {
    if is_top_level_name(parse_result, name) {
        return Vec::new();
    }

    let mut spans = Vec::new();
    collect_import_binding_refs(parse_result, name, &mut spans);

    spans
        .into_iter()
        .map(|span| OffsetSpan {
            start: span.start,
            end: span.end,
        })
        .collect()
}

/// Check if a name matches a module-scope item definition.
#[must_use]
pub fn is_top_level_name(parse_result: &ParseResult, name: &str) -> bool {
    parse_result.program.items.iter().any(|(item, _)| {
        // Check item-level names (functions, actors, types, etc.)
        let item_name = match item {
            Item::Function(f) => Some(f.name.as_str()),
            Item::Actor(a) => Some(a.name.as_str()),
            Item::Supervisor(s) => Some(s.name.as_str()),
            Item::Trait(t) => Some(t.name.as_str()),
            Item::Const(c) => Some(c.name.as_str()),
            Item::TypeDecl(td) => Some(td.name.as_str()),
            Item::Wire(w) => Some(w.name.as_str()),
            Item::TypeAlias(ta) => Some(ta.name.as_str()),
            Item::Machine(m) => Some(m.name.as_str()),
            Item::Import(_) | Item::ExternBlock(_) | Item::Impl(_) => None,
        };
        if item_name == Some(name) {
            return true;
        }
        // Also treat actor receive handler names and field names as global —
        // they can be referenced via message sends from any scope.
        if let Item::Actor(a) = item {
            if a.receive_fns.iter().any(|r| r.name == name) {
                return true;
            }
            if a.fields.iter().any(|f| f.name == name) {
                return true;
            }
        }
        if let Item::TypeDecl(td) = item {
            if td
                .body
                .iter()
                .any(|body_item| matches!(body_item, TypeBodyItem::Field { name: field_name, .. } if field_name == name))
            {
                return true;
            }
        }
        false
    })
}

// ── Internal helpers ────────────────────────────────────────────────

/// Internal version that returns raw `Span` (used by the public API and by
/// `count_all_references`).
fn find_all_references_raw(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
) -> Option<(String, Vec<Span>)> {
    let (name, _) = simple_word_at_offset(source, offset)?;
    let local_definition =
        crate::definition::find_local_binding_definition(source, parse_result, &name, offset)
            .or_else(|| crate::definition::find_param_definition(parse_result, &name, offset));

    let mut spans = Vec::new();
    collect_refs_in_parse_result(source, parse_result, &name, &mut spans);

    // For top-level names (functions, actors, types, receive handlers, fields),
    // return all references globally.
    if local_definition.is_none() && is_top_level_name(parse_result, &name) {
        if spans.is_empty() {
            return None;
        }
        return Some((name, spans));
    }

    // Cursor on an import binding token (e.g. `foo` in `import util::{ foo }`)
    // — treat exactly like a top-level name: return all body usages without
    // scope-filtering, since the binding is file-scoped.
    // HOWEVER: use the shadow-aware walker to avoid returning locals/parameters
    // that shadow the import (e.g., a `foo` parameter when the import is `foo`).
    if is_cursor_on_import_binding(source, parse_result, &name, offset) {
        // Use shadow-aware reference collection to skip shadowed bindings.
        spans.clear();
        collect_import_binding_refs(parse_result, &name, &mut spans);
        // Also include the import token itself as a reference site.
        if let Some(import_span) = find_import_binding_span(source, parse_result, &name, offset) {
            spans.insert(0, import_span);
        }
        if spans.is_empty() {
            return None;
        }
        return Some((name, spans));
    }

    // For local variables/parameters, restrict to the enclosing function/handler scope.
    let scope_opt = find_defining_scope(parse_result, offset);
    if let Some(scope) = &scope_opt {
        spans.retain(|s| s.start >= scope.start && s.end <= scope.end);
    }

    // Further refine: if the name is shadowed within the scope (e.g., a for-loop
    // re-declares `x`), only keep references that share the same binding.
    // Use AST-derived binding sites rather than source-text prefix heuristics.
    let mut binding_starts = collect_binding_starts_in_parse_result(parse_result, &name);
    // Restrict binding starts to the current scope.
    if let Some(scope) = &scope_opt {
        binding_starts.retain(|&s| s >= scope.start && s < scope.end);
    }
    binding_starts.sort_unstable();
    binding_starts.dedup();

    if binding_starts.len() > 1 {
        // Each binding "owns" references from its declaration to the next binding
        // (or end of scope).
        let mut region_start = 0;
        let mut region_end = usize::MAX;
        for (i, &bstart) in binding_starts.iter().enumerate() {
            if bstart <= offset {
                region_start = bstart;
                region_end = binding_starts.get(i + 1).copied().unwrap_or(usize::MAX);
            }
        }
        spans.retain(|s| s.start >= region_start && s.start < region_end);
    }

    if spans.is_empty() {
        None
    } else {
        Some((name, spans))
    }
}

/// Returns the span of the enclosing scope for the cursor offset. For actors,
/// narrows to a specific receive fn when possible. Falls back to the enclosing
/// item span.
fn find_defining_scope(parse_result: &ParseResult, offset: usize) -> Option<Span> {
    for (item, item_span) in &parse_result.program.items {
        if !item_span.contains(&offset) {
            continue;
        }
        if let Item::Actor(a) = item {
            for recv in &a.receive_fns {
                if !recv.span.is_empty() && recv.span.contains(&offset) {
                    return Some(recv.span.clone());
                }
            }
        }
        return Some(item_span.clone());
    }
    None
}

/// Return `true` if `offset` sits inside an `import` item AND the word at
/// that offset is one of the explicitly-listed imported binding names.
///
/// Used by `find_all_references_raw` to detect the import-token cursor path
/// and return file-scoped references instead of falling through to the
/// local-variable scope-filtering path.
///
/// The check requires that `offset` lies *after* the opening `{` of the import
/// spec in the source text so that a cursor on a path component that happens to
/// share a name with a binding (e.g. `import foo::{ foo }`) is not misclassified.
fn is_cursor_on_import_binding(
    source: &str,
    parse_result: &ParseResult,
    name: &str,
    offset: usize,
) -> bool {
    use hew_parser::ast::ImportSpec;
    for (item, item_span) in &parse_result.program.items {
        if !item_span.contains(&offset) {
            continue;
        }
        if let Item::Import(decl) = item {
            if let Some(ImportSpec::Names(names)) = &decl.spec {
                if !names
                    .iter()
                    .any(|n| n.name == name || n.alias.as_deref() == Some(name))
                {
                    break;
                }
                // Confirm the cursor is inside the `{...}` spec portion, not on a
                // path segment that shares the same text.  Find the first `{` within
                // the item span in the source text.
                let item_src = &source[item_span.clone()];
                let brace_offset = item_src.find('{').map(|rel| item_span.start + rel);
                if let Some(brace_pos) = brace_offset {
                    return offset > brace_pos;
                }
                // No `{` found (should not happen for Names spec); fall through.
            }
        }
        break;
    }
    false
}

/// Return the byte span of the import binding token at `offset` in `source`.
///
/// Since the cursor is already on the token, `simple_word_at_offset` gives us
/// the span directly.
fn find_import_binding_span(
    source: &str,
    _parse_result: &ParseResult,
    _name: &str,
    offset: usize,
) -> Option<Span> {
    let (_, os) = simple_word_at_offset(source, offset)?;
    Some(os.start..os.end)
}

/// Collect the start offsets of all binding (declaration) sites for `name`
/// across all items in the parse result.
///
/// Tracks `let`, `var`, and `for`/`if let`/`while let`/`match`-arm patterns —
/// the same set of constructs the bespoke walker covered. Function and lambda
/// parameters are excluded so the resulting offsets only mark in-body shadow
/// boundaries.
fn collect_binding_starts_in_parse_result(parse_result: &ParseResult, name: &str) -> Vec<usize> {
    let mut visitor = BindingStartsVisitor {
        name,
        starts: Vec::new(),
    };
    ast_visit::walk_parse_result(None, parse_result, &mut visitor);
    visitor.starts
}

struct BindingStartsVisitor<'a> {
    name: &'a str,
    starts: Vec<usize>,
}

impl<'ast> AstVisitor<'ast> for BindingStartsVisitor<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt, span: &'ast Span, _ctx: VisitContext<'ast>) {
        match stmt {
            Stmt::Let { pattern, .. } | Stmt::For { pattern, .. }
                if pattern_binds_name(&pattern.0, self.name) =>
            {
                self.starts.push(pattern.1.start);
            }
            Stmt::IfLet { pattern, .. } | Stmt::WhileLet { pattern, .. }
                if pattern_binds_name(&pattern.0, self.name) =>
            {
                self.starts.push(pattern.1.start);
            }
            Stmt::Var {
                name: binding_name, ..
            } if binding_name == self.name => {
                // Stmt::Var carries no dedicated name-span; use the statement span.
                self.starts.push(span.start);
            }
            Stmt::Match { arms, .. } => {
                for arm in arms {
                    if pattern_binds_name(&arm.pattern.0, self.name) {
                        self.starts.push(arm.pattern.1.start);
                    }
                }
            }
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr, _span: &'ast Span, _ctx: VisitContext<'ast>) {
        match expr {
            Expr::IfLet { pattern, .. } if pattern_binds_name(&pattern.0, self.name) => {
                self.starts.push(pattern.1.start);
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    if pattern_binds_name(&arm.pattern.0, self.name) {
                        self.starts.push(arm.pattern.1.start);
                    }
                }
            }
            _ => {}
        }
    }
}

/// Collect every body-side reference to an import-bound name, skipping any
/// usage that a local binding shadows. The `AstWalker`'s scope stack handles
/// parameter, lambda, scope, and pattern-binding shadowing automatically; the
/// only out-of-band case is `actor` field shadowing, which is detected up
/// front per actor item span and applied while walking inside that span.
fn collect_import_binding_refs(parse_result: &ParseResult, name: &str, spans: &mut Vec<Span>) {
    let actor_shadow_spans: Vec<Span> = parse_result
        .program
        .items
        .iter()
        .filter_map(|(item, item_span)| match item {
            Item::Actor(a) if a.fields.iter().any(|field| field.name == name) => {
                Some(item_span.clone())
            }
            _ => None,
        })
        .collect();

    let mut visitor = ImportBindingRefsVisitor {
        name,
        actor_shadow_spans: &actor_shadow_spans,
        spans,
    };
    ast_visit::walk_parse_result(None, parse_result, &mut visitor);
}

struct ImportBindingRefsVisitor<'a> {
    name: &'a str,
    actor_shadow_spans: &'a [Span],
    spans: &'a mut Vec<Span>,
}

impl<'ast> AstVisitor<'ast> for ImportBindingRefsVisitor<'_> {
    fn visit_identifier(
        &mut self,
        name: &'ast str,
        span: &'ast Span,
        binding: Option<BindingInfo<'ast>>,
        _ctx: VisitContext<'ast>,
    ) {
        if name != self.name || binding.is_some() {
            return;
        }
        if self
            .actor_shadow_spans
            .iter()
            .any(|actor_span| span.start >= actor_span.start && span.end <= actor_span.end)
        {
            return;
        }
        self.spans.push(span.clone());
    }
}

fn pattern_binds_name(pattern: &Pattern, name: &str) -> bool {
    match pattern {
        Pattern::Identifier(ident) => ident == name,
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => patterns
            .iter()
            .any(|(pattern, _)| pattern_binds_name(pattern, name)),
        Pattern::Struct { fields, .. } => fields.iter().any(|field| {
            field
                .pattern
                .as_ref()
                .is_some_and(|(pattern, _)| pattern_binds_name(pattern, name))
        }),
        Pattern::Or(left, right) => {
            pattern_binds_name(&left.0, name) || pattern_binds_name(&right.0, name)
        }
        Pattern::Wildcard | Pattern::Literal(_) => false,
    }
}

/// Collect every span where `name` appears as an identifier, field name, or
/// pattern binding across all item bodies in `parse_result`.
///
/// This is the unscoped collection step the public reference walkers feed
/// into; callers narrow the result to the cursor's binding region with
/// `find_defining_scope` and `collect_binding_starts_in_parse_result`.
fn collect_refs_in_parse_result(
    source: &str,
    parse_result: &ParseResult,
    name: &str,
    spans: &mut Vec<Span>,
) {
    let mut visitor = RefsVisitor {
        source,
        name,
        spans,
    };
    ast_visit::walk_parse_result(Some(source), parse_result, &mut visitor);
}

struct RefsVisitor<'a> {
    source: &'a str,
    name: &'a str,
    spans: &'a mut Vec<Span>,
}

impl RefsVisitor<'_> {
    fn push_pattern_matches(&mut self, pattern: &Pattern, span: &Span) {
        match pattern {
            Pattern::Identifier(ident) => {
                if ident == self.name {
                    self.spans.push(span.clone());
                }
            }
            Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
                for (p, s) in patterns {
                    self.push_pattern_matches(p, s);
                }
            }
            Pattern::Struct { fields, .. } => {
                for field in fields {
                    if let Some((p, s)) = &field.pattern {
                        self.push_pattern_matches(p, s);
                    }
                }
            }
            Pattern::Or(left, right) => {
                self.push_pattern_matches(&left.0, &left.1);
                self.push_pattern_matches(&right.0, &right.1);
            }
            Pattern::Wildcard | Pattern::Literal(_) => {}
        }
    }
}

impl<'ast> AstVisitor<'ast> for RefsVisitor<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt, _span: &'ast Span, _ctx: VisitContext<'ast>) {
        match stmt {
            Stmt::Let { pattern, .. } | Stmt::For { pattern, .. } => {
                self.push_pattern_matches(&pattern.0, &pattern.1);
            }
            Stmt::IfLet { pattern, .. } | Stmt::WhileLet { pattern, .. } => {
                self.push_pattern_matches(&pattern.0, &pattern.1);
            }
            Stmt::Match { arms, .. } => {
                for arm in arms {
                    self.push_pattern_matches(&arm.pattern.0, &arm.pattern.1);
                }
            }
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr, span: &'ast Span, _ctx: VisitContext<'ast>) {
        match expr {
            Expr::FieldAccess { field, .. } if field == self.name => {
                self.spans
                    .push(field_access_name_span(self.source, span, field));
            }
            Expr::StructInit { fields, .. } => {
                let mut search_from = span.start;
                for (field, val) in fields {
                    if field == self.name {
                        if let Some(field_span) = struct_init_field_name_span(
                            self.source,
                            span,
                            search_from,
                            field,
                            &val.1,
                        ) {
                            self.spans.push(field_span);
                        }
                    }
                    search_from = val.1.end;
                }
            }
            Expr::IfLet { pattern, .. } => {
                self.push_pattern_matches(&pattern.0, &pattern.1);
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    self.push_pattern_matches(&arm.pattern.0, &arm.pattern.1);
                }
            }
            Expr::Select { arms, .. } => {
                for arm in arms {
                    self.push_pattern_matches(&arm.binding.0, &arm.binding.1);
                }
            }
            _ => {}
        }
    }

    fn visit_identifier(
        &mut self,
        name: &'ast str,
        span: &'ast Span,
        _binding: Option<BindingInfo<'ast>>,
        _ctx: VisitContext<'ast>,
    ) {
        if name == self.name {
            self.spans.push(span.clone());
        }
    }
}

fn field_access_name_span(source: &str, span: &Span, field: &str) -> Span {
    let bytes = source.as_bytes();
    let mut end = span.end;
    while end > span.start && bytes[end - 1].is_ascii_whitespace() {
        end -= 1;
    }
    Span {
        start: end.saturating_sub(field.len()),
        end,
    }
}

fn struct_init_field_name_span(
    source: &str,
    span: &Span,
    search_from: usize,
    field: &str,
    value_span: &Span,
) -> Option<Span> {
    let bytes = source.as_bytes();
    let mut cursor = search_from.max(span.start);

    while cursor < value_span.start {
        let field_span = crate::util::find_name_span(source, cursor, field);
        if field_span.end > value_span.start || field_span.start >= span.end {
            return None;
        }

        let mut separator = field_span.end;
        while separator < value_span.start && bytes[separator].is_ascii_whitespace() {
            separator += 1;
        }
        if separator < value_span.start && bytes[separator] == b':' {
            separator += 1;
            while separator < value_span.start && bytes[separator].is_ascii_whitespace() {
                separator += 1;
            }
            if separator == value_span.start {
                return Some(Span {
                    start: field_span.start,
                    end: field_span.end,
                });
            }
        }

        cursor = field_span.end;
    }

    None
}

// ── Identifier-count analysis ────────────────────────────────────────

/// Count how many times each identifier appears across all item bodies
/// in the parse result. Used by the LSP for code-lens reference hints.
#[must_use]
pub fn count_all_references(parse_result: &ParseResult) -> HashMap<String, usize> {
    let mut counts = HashMap::new();
    for (item, _) in &parse_result.program.items {
        count_idents_in_item(item, &mut counts);
    }
    counts
}

fn count_idents_in_item(item: &Item, counts: &mut HashMap<String, usize>) {
    match item {
        Item::Function(f) => count_idents_in_block(&f.body, counts),
        Item::Actor(a) => {
            if let Some(init) = &a.init {
                count_idents_in_block(&init.body, counts);
            }
            if let Some(term) = &a.terminate {
                count_idents_in_block(&term.body, counts);
            }
            for recv in &a.receive_fns {
                count_idents_in_block(&recv.body, counts);
            }
            for method in &a.methods {
                count_idents_in_block(&method.body, counts);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(m) = body_item {
                    count_idents_in_block(&m.body, counts);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                count_idents_in_block(&method.body, counts);
            }
        }
        Item::Trait(t) => {
            for trait_item in &t.items {
                if let TraitItem::Method(m) = trait_item {
                    if let Some(body) = &m.body {
                        count_idents_in_block(body, counts);
                    }
                }
            }
        }
        Item::Const(c) => count_idents_in_expr(&c.value.0, counts),
        Item::Supervisor(s) => {
            for child in &s.children {
                for arg in &child.args {
                    count_idents_in_expr(&arg.0, counts);
                }
            }
        }
        Item::Machine(m) => {
            for transition in &m.transitions {
                if let Some(guard) = &transition.guard {
                    count_idents_in_expr(&guard.0, counts);
                }
                count_idents_in_expr(&transition.body.0, counts);
            }
        }
        Item::Import(_) | Item::ExternBlock(_) | Item::Wire(_) | Item::TypeAlias(_) => {}
    }
}

fn count_idents_in_block(block: &Block, counts: &mut HashMap<String, usize>) {
    for (stmt, _) in &block.stmts {
        count_idents_in_stmt(stmt, counts);
    }
    if let Some(trailing) = &block.trailing_expr {
        count_idents_in_expr(&trailing.0, counts);
    }
}

fn count_idents_in_stmt(stmt: &Stmt, counts: &mut HashMap<String, usize>) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some(val) = value {
                count_idents_in_expr(&val.0, counts);
            }
        }
        Stmt::Assign { target, value, .. } => {
            count_idents_in_expr(&target.0, counts);
            count_idents_in_expr(&value.0, counts);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            count_idents_in_expr(&condition.0, counts);
            count_idents_in_block(then_block, counts);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &eb.if_stmt {
                    count_idents_in_stmt(&if_stmt.0, counts);
                }
                if let Some(block) = &eb.block {
                    count_idents_in_block(block, counts);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            count_idents_in_expr(&expr.0, counts);
            count_idents_in_block(body, counts);
            if let Some(block) = else_body {
                count_idents_in_block(block, counts);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            count_idents_in_expr(&scrutinee.0, counts);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    count_idents_in_expr(&guard.0, counts);
                }
                count_idents_in_expr(&arm.body.0, counts);
            }
        }
        Stmt::Loop { body, .. }
        | Stmt::While { body, .. }
        | Stmt::WhileLet { body, .. }
        | Stmt::For { body, .. } => {
            count_idents_in_block(body, counts);
        }
        Stmt::Expression(expr) | Stmt::Return(Some(expr)) => {
            count_idents_in_expr(&expr.0, counts);
        }
        Stmt::Break {
            value: Some(val), ..
        } => count_idents_in_expr(&val.0, counts),
        _ => {}
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over all Expr variants for identifier counting"
)]
fn count_idents_in_expr(expr: &Expr, counts: &mut HashMap<String, usize>) {
    match expr {
        Expr::Identifier(ident) => {
            *counts.entry(ident.clone()).or_insert(0) += 1;
        }
        Expr::Binary { left, right, .. } => {
            count_idents_in_expr(&left.0, counts);
            count_idents_in_expr(&right.0, counts);
        }
        Expr::Unary { operand, .. } => count_idents_in_expr(&operand.0, counts),
        Expr::Call { function, args, .. } => {
            count_idents_in_expr(&function.0, counts);
            for arg in args {
                count_idents_in_expr(&arg.expr().0, counts);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            count_idents_in_expr(&receiver.0, counts);
            for arg in args {
                count_idents_in_expr(&arg.expr().0, counts);
            }
        }
        Expr::FieldAccess { object, .. } => count_idents_in_expr(&object.0, counts),
        Expr::Index { object, index } => {
            count_idents_in_expr(&object.0, counts);
            count_idents_in_expr(&index.0, counts);
        }
        Expr::StructInit { fields, .. } => {
            for (_, val) in fields {
                count_idents_in_expr(&val.0, counts);
            }
        }
        Expr::Spawn { target, args } => {
            count_idents_in_expr(&target.0, counts);
            for (_, val) in args {
                count_idents_in_expr(&val.0, counts);
            }
        }
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => count_idents_in_block(block, counts),
        Expr::Scope { body, .. } => count_idents_in_block(body, counts),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            count_idents_in_expr(&condition.0, counts);
            count_idents_in_expr(&then_block.0, counts);
            if let Some(else_expr) = else_block {
                count_idents_in_expr(&else_expr.0, counts);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            count_idents_in_expr(&expr.0, counts);
            count_idents_in_block(body, counts);
            if let Some(block) = else_body {
                count_idents_in_block(block, counts);
            }
        }
        Expr::Match { scrutinee, arms } => {
            count_idents_in_expr(&scrutinee.0, counts);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    count_idents_in_expr(&guard.0, counts);
                }
                count_idents_in_expr(&arm.body.0, counts);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            count_idents_in_expr(&body.0, counts);
        }
        Expr::ArrayRepeat { value, count } => {
            count_idents_in_expr(&value.0, counts);
            count_idents_in_expr(&count.0, counts);
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                count_idents_in_expr(&k.0, counts);
                count_idents_in_expr(&v.0, counts);
            }
        }
        Expr::Tuple(elems) | Expr::Array(elems) | Expr::Join(elems) => {
            for elem in elems {
                count_idents_in_expr(&elem.0, counts);
            }
        }
        Expr::Send { target, message } => {
            count_idents_in_expr(&target.0, counts);
            count_idents_in_expr(&message.0, counts);
        }
        Expr::Select {
            arms: sel_arms,
            timeout,
        } => {
            for arm in sel_arms {
                count_idents_in_expr(&arm.body.0, counts);
            }
            if let Some(t) = timeout {
                count_idents_in_expr(&t.body.0, counts);
            }
        }
        Expr::Timeout { expr: e, .. } => count_idents_in_expr(&e.0, counts),
        Expr::Await(inner) | Expr::PostfixTry(inner) | Expr::Yield(Some(inner)) => {
            count_idents_in_expr(&inner.0, counts);
        }
        Expr::Cast { expr: inner, .. } => count_idents_in_expr(&inner.0, counts),
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                count_idents_in_expr(&s.0, counts);
            }
            if let Some(e) = end {
                count_idents_in_expr(&e.0, counts);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(e) = part {
                    count_idents_in_expr(&e.0, counts);
                }
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> hew_parser::ParseResult {
        hew_parser::parse(source)
    }

    #[test]
    fn find_refs_local_variable() {
        // Variable declared and used in the same scope
        let source = "fn main() {\n    let x = 1;\n    let y = x + 2;\n}";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let result = find_all_references(source, &pr, offset);
        assert!(result.is_some(), "should find references to x");
        let (name, spans) = result.unwrap();
        assert_eq!(name, "x");
        assert!(
            spans.len() >= 2,
            "should find definition and at least one usage, got {}",
            spans.len()
        );
    }

    #[test]
    fn find_refs_function_name() {
        // Function defined and called — a top-level name
        // Note: references only captures usage sites (in bodies), not the definition name
        let source = "fn greet() {}\nfn main() {\n    greet()\n}";
        let pr = parse(source);
        let offset = source.find("greet").unwrap();
        let result = find_all_references(source, &pr, offset);
        assert!(result.is_some(), "should find references to greet");
        let (name, spans) = result.unwrap();
        assert_eq!(name, "greet");
        assert!(
            !spans.is_empty(),
            "should find at least one reference to greet"
        );
    }

    #[test]
    fn no_refs_at_whitespace() {
        let source = "fn main() { }";
        let pr = parse(source);
        // Offset 3 is the space between "fn" and "main"
        let result = find_all_references(source, &pr, 3);
        assert!(result.is_none(), "no identifier at whitespace");
    }

    #[test]
    fn is_top_level_detects_function() {
        let source = "fn greet() {}\nfn main() {}";
        let pr = parse(source);
        assert!(is_top_level_name(&pr, "greet"));
        assert!(is_top_level_name(&pr, "main"));
        assert!(!is_top_level_name(&pr, "unknown"));
    }

    #[test]
    fn refs_scoped_to_enclosing_function() {
        // Same variable name in two different functions — references should be scoped
        let source =
            "fn foo() {\n    let x = 1;\n    let a = x;\n}\nfn bar() {\n    let x = 2;\n    let b = x;\n}";
        let pr = parse(source);
        let x_in_foo = source.find("let x = 1").unwrap() + 4;
        let result = find_all_references(source, &pr, x_in_foo);
        assert!(result.is_some());
        let (_name, spans) = result.unwrap();
        let bar_start = source.find("fn bar").unwrap();
        for span in &spans {
            assert!(
                span.end <= bar_start,
                "reference at {}..{} should be within foo's scope (before bar at {bar_start})",
                span.start,
                span.end,
            );
        }
    }

    #[test]
    fn is_top_level_detects_actor() {
        let source = "actor Counter {\n    receive fn inc() {\n        let x = 1;\n    }\n}";
        let pr = parse(source);
        assert!(is_top_level_name(&pr, "Counter"));
    }

    #[test]
    fn var_binding_detected_by_ast_not_text() {
        // `var x` binding — previously is_binding_span missed this because
        // collect_refs_in_stmt for Var never pushed the name span into spans.
        let source = "fn f() {\n    var x = 1;\n    x = 2;\n    let y = x;\n}";
        let pr = parse(source);
        let var_offset = source.find("var x").unwrap() + 4; // cursor on `x` in `var x`
        let result = find_all_references(source, &pr, var_offset);
        assert!(result.is_some(), "var binding should be found");
        let (name, spans) = result.unwrap();
        assert_eq!(name, "x");
        // Must include the usage spans (x = 2 and y = x)
        assert!(
            !spans.is_empty(),
            "should include at least the reference spans"
        );
    }

    #[test]
    fn for_loop_binding_scoped_separately_from_outer_let() {
        // `let x` in outer scope, `for x in` inside loop — cursor on the `for x`
        // should NOT include the outer `let x` declaration.
        let source = "fn f() {\n    let x = 0;\n    for x in 0..3 {\n        let _y = x;\n    }\n}";
        let pr = parse(source);
        let for_x_offset = source.find("for x").unwrap() + 4; // cursor on `x` in `for x`
        let result = find_all_references(source, &pr, for_x_offset);
        assert!(result.is_some());
        let (_name, spans) = result.unwrap();
        let outer_let_offset = source.find("let x").unwrap();
        for span in &spans {
            assert!(
                span.start > outer_let_offset + 5,
                "for-loop `x` refs should not include the outer let x at {outer_let_offset}"
            );
        }
    }

    #[test]
    fn shadowed_let_disambiguated_by_ast() {
        // Two `let x` declarations in the same function — cursor on the first one
        // should not return references after the second declaration.
        let source = "fn f() {\n    let x = 1;\n    let a = x;\n    let x = 2;\n    let b = x;\n}";
        let pr = parse(source);
        let first_x_offset = source.find("let x").unwrap() + 4; // cursor on first `x`
        let result = find_all_references(source, &pr, first_x_offset);
        assert!(result.is_some());
        let (_name, spans) = result.unwrap();
        let second_let_x_offset = source.rfind("let x").unwrap();
        for span in &spans {
            assert!(
                span.start < second_let_x_offset,
                "first `x` refs should not include spans after the second `let x` at {second_let_x_offset}"
            );
        }
    }

    #[test]
    fn local_shadowing_global_stays_local() {
        let source = "fn foo() -> int { 1 }\nfn main() {\n    let foo = 2;\n    foo\n}";
        let pr = parse(source);
        let local_offset = source.find("let foo").unwrap() + 4;
        let (_name, spans) =
            find_all_references(source, &pr, local_offset).expect("should find local references");
        let main_start = source.find("fn main").unwrap();
        assert_eq!(spans.len(), 2, "expected local definition and use only");
        assert!(
            spans.iter().all(|span| span.start >= main_start),
            "local references must stay inside main(): {spans:?}"
        );
    }

    #[test]
    fn match_arm_body_rebinding_scoped_separately() {
        // A `let x` inside a match arm body re-binds `x`; references after it
        // inside that arm belong to the inner binding, not the outer `let x`.
        let source = "fn f() {\n    let x = 1;\n    match v { _ => { let x = 2; let y = x; } }\n}";
        let pr = parse(source);
        // Cursor on the outer `let x` (first occurrence).
        let outer_x_offset = source.find("let x").unwrap() + 4;
        let result = find_all_references(source, &pr, outer_x_offset);
        assert!(result.is_some());
        let (_name, spans) = result.unwrap();
        // `let y = x` inside the arm refers to the inner `x`; the outer binding
        // should not include spans inside the arm body after the inner `let x`.
        let inner_let_x_offset = source.rfind("let x").unwrap();
        for span in &spans {
            assert!(
                span.start < inner_let_x_offset,
                "outer `x` refs should not cross into the inner `let x` at {inner_let_x_offset}: found span at {}",
                span.start
            );
        }
    }

    #[test]
    fn find_refs_struct_field_from_declaration() {
        let source = "type Point { x: i32; y: i32 }\nfn main() { let p = Point { x: 1, y: 2 }; let q = Point { x: 3, y: 4 }; p.x + q.x }";
        let pr = parse(source);
        let offset = source
            .find("x: i32")
            .expect("field declaration should exist");
        let result =
            find_all_references(source, &pr, offset).expect("should find field references");
        let (name, spans) = result;
        assert_eq!(name, "x");

        assert_eq!(spans.len(), 4);
        let mut access_refs = 0;
        let mut init_refs = 0;
        for span in spans {
            assert_eq!(&source[span.start..span.end], "x");
            if source.as_bytes()[span.start - 1] == b'.' {
                access_refs += 1;
            } else if source[span.end..].trim_start().starts_with(':') {
                init_refs += 1;
            }
        }
        assert_eq!(access_refs, 2);
        assert_eq!(init_refs, 2);
    }

    #[test]
    fn find_refs_struct_field_from_access() {
        let source = "type Point { x: i32; y: i32 }\nfn main() { let p = Point { x: 1, y: 2 }; let q = Point { x: 3, y: 4 }; p.x + q.x }";
        let pr = parse(source);
        let offset = source.find("p.x").expect("field access should exist") + 2;
        let result =
            find_all_references(source, &pr, offset).expect("should find field references");
        let (name, spans) = result;
        assert_eq!(name, "x");

        assert_eq!(spans.len(), 4);
        let mut access_refs = 0;
        let mut init_refs = 0;
        for span in spans {
            assert_eq!(&source[span.start..span.end], "x");
            if source.as_bytes()[span.start - 1] == b'.' {
                access_refs += 1;
            } else if source[span.end..].trim_start().starts_with(':') {
                init_refs += 1;
            }
        }
        assert_eq!(access_refs, 2);
        assert_eq!(init_refs, 2);
    }

    // ── find_import_binding_references: multi-scope coverage (#1283) ──

    #[test]
    fn import_binding_refs_collected_across_all_functions() {
        // Regression test for issue #1283: find_import_binding_references must
        // return usages in every function body, not just the first one.
        // Previously this was suspected to miss scopes; this test confirms it
        // already works correctly and guards against regressions.
        let source =
            "import util::{ foo };\nfn a() -> i64 { foo() }\nfn b() -> i64 { foo() }\nfn c() -> i64 { foo() }";
        let pr = parse(source);

        let spans = find_import_binding_references(&pr, "foo");
        assert_eq!(
            spans.len(),
            3,
            "foo() usage must be found in all three function bodies, got {spans:?}"
        );
        // Each span must point to "foo".
        for span in &spans {
            assert_eq!(&source[span.start..span.end], "foo");
        }
    }

    #[test]
    fn import_binding_refs_finds_usages_from_importer_cursor() {
        // When the cursor sits on the import binding token (the `foo` in
        // `import util::{ foo }`), find_all_references must return every usage
        // of `foo` in the file — including usages across multiple functions.
        let source = "import util::{ foo };\nfn a() -> i64 { foo() }\nfn b() -> i64 { foo() }";
        let pr = parse(source);

        // Place cursor on the import token.
        let import_offset = source.find("{ foo }").unwrap() + 2;
        let result = find_all_references(source, &pr, import_offset);
        let (_name, spans) = result.expect("should find references from import token");

        // Must include both call sites.
        assert!(
            spans.len() >= 2,
            "expected at least 2 references (one per function body), got {spans:?}"
        );
    }

    #[test]
    fn import_binding_cursor_excludes_shadowed_parameters() {
        // Regression: import-binding cursors must not return locals/parameters
        // that shadow the import. When cursor is on `foo` in `import util::{ foo }`,
        // and there's a `foo` parameter in main(), find_all_references must return
        // ONLY the import span, not the parameter use.
        let source = "import util::{ foo }; fn main(foo: i64) -> i64 { foo }";
        let pr = parse(source);

        // Cursor on the import binding token `foo` (after `{`).
        let import_offset = source.find("{ foo }").unwrap() + 2;
        let result = find_all_references(source, &pr, import_offset);

        // Must find the reference, but it should be ONLY the import itself,
        // not the parameter or its use in main's body.
        let (_name, spans) = result.expect("should find import reference");
        assert_eq!(
            spans.len(),
            1,
            "import-binding cursor should find ONLY the import token, not the shadowing parameter use; got {spans:?}"
        );

        // Verify the single span is the import token, not the parameter use.
        let span = &spans[0];
        let span_text = &source[span.start..span.end];
        assert_eq!(span_text, "foo", "reference must be to the import token");
        // The import token appears at offset ~21-24; the parameter appears later.
        // The parameter use in `{ foo }` body is much later (~50-53).
        assert!(
            span.start < 30,
            "reference should be near the import, not the parameter use at offset 49-53"
        );
    }

    #[test]
    fn cursor_on_import_path_segment_not_misclassified_as_binding() {
        // Regression for issue #1283: when a path component shares its name
        // with a binding name (e.g. `import foo::{ foo }`), a cursor on the
        // *path* component (`foo` before `::`) must NOT be treated as a cursor on
        // the import binding.
        //
        // `import foo::{ foo }` — the module path `foo` is at offset 7,
        // the binding `foo` is after `{` at a later offset.
        // find_all_references from the path token must fall through to the
        // local-variable path (not the import-binding path), which means it
        // should either return None (no local `foo` in scope outside an import)
        // or diverge from the binding-cursor result.
        let source = "import foo::{ foo };\nfn main() { foo() }";
        let pr = parse(source);

        // Cursor on the path segment `foo` (before `::`).
        let path_offset = source.find("import foo").unwrap() + 7; // offset of `f` in `foo::`
                                                                  // Cursor on the binding token `foo` (after `{`).
        let binding_offset = source.find("{ foo }").unwrap() + 2;

        let binding_result = find_all_references(source, &pr, binding_offset);

        // The binding cursor should find at least the call in main().
        let (_name, binding_spans) = binding_result.expect("binding cursor should find references");
        assert!(
            !binding_spans.is_empty(),
            "binding cursor should find at least one reference"
        );

        // The path cursor must NOT produce the same wide result as the binding cursor.
        // It may return None or a narrower result — but it must not be identical to
        // the binding-cursor result (which would indicate it was misclassified).
        let path_result = find_all_references(source, &pr, path_offset);
        if let Some((_pname, path_spans)) = path_result {
            // If the path cursor does return references, they must not include
            // the import token itself at path_offset (that would be the binding path).
            // The key invariant: path_spans != binding_spans.
            assert_ne!(
                path_spans, binding_spans,
                "cursor on path component should not be classified as an import-binding cursor"
            );
        }
        // None is also acceptable — means no local `foo` found at path position.
    }

    // ── count_all_references: Supervisor and Machine coverage (#1333 residual) ──

    #[test]
    fn count_matches_find_refs_for_ident_in_supervisor_child_arg() {
        // A global function `make_config` is used as an argument to a supervisor
        // child spec. Previously count_all_references returned 0 for it because
        // Item::Supervisor(_) was silently skipped.
        let source = concat!(
            "fn make_config() -> Int { 42 }\n",
            "actor Worker {\n",
            "    receive fn start() {}\n",
            "}\n",
            "supervisor Pool {\n",
            "    child w: Worker(make_config());\n",
            "}",
        );
        let pr = parse(source);

        // Cursor on `make_config` inside the child arg.
        let ref_offset = source.rfind("make_config").unwrap();
        let (name, find_spans) = find_all_references(source, &pr, ref_offset)
            .expect("should find references to make_config");
        assert_eq!(name, "make_config");
        assert!(
            !find_spans.is_empty(),
            "find_all_references must see make_config inside the supervisor arg"
        );

        let counts = count_all_references(&pr);
        let count = counts.get("make_config").copied().unwrap_or(0);
        assert_eq!(
            count,
            find_spans.len(),
            "count_all_references must agree with find_all_references for make_config \
             (previously 0 due to Supervisor skip); count={count}, find={}",
            find_spans.len(),
        );
    }

    #[test]
    fn count_matches_find_refs_for_ident_in_machine_transition_body() {
        // A global function `compute` is referenced in a machine transition body.
        // Previously count_all_references returned 0 because Item::Machine(_)
        // was silently skipped.
        let source = concat!(
            "fn compute() -> Int { 0 }\n",
            "machine Counter {\n",
            "    state Idle;\n",
            "    state Running;\n",
            "    event Start;\n",
            "    on Start: Idle -> Running { compute() }\n",
            "}",
        );
        let pr = parse(source);

        // Cursor on `compute` inside the transition body.
        let ref_offset = source.rfind("compute").unwrap();
        let (name, find_spans) = find_all_references(source, &pr, ref_offset)
            .expect("should find references to compute");
        assert_eq!(name, "compute");
        assert!(
            !find_spans.is_empty(),
            "find_all_references must see compute inside the machine transition body"
        );

        let counts = count_all_references(&pr);
        let count = counts.get("compute").copied().unwrap_or(0);
        assert_eq!(
            count,
            find_spans.len(),
            "count_all_references must agree with find_all_references for compute \
             (previously 0 due to Machine skip); count={count}, find={}",
            find_spans.len(),
        );
    }

    #[test]
    fn count_matches_find_refs_for_ident_in_machine_transition_guard() {
        // A global `flag` constant is used as a machine transition guard.
        // count_all_references must pick it up alongside find_all_references.
        let source = concat!(
            "const flag: Bool = true;\n",
            "machine Gate {\n",
            "    state Locked;\n",
            "    state Open;\n",
            "    event Try;\n",
            "    on Try: Locked -> Open when flag { Open }\n",
            "}",
        );
        let pr = parse(source);

        // Cursor on `flag` in the guard.
        let ref_offset = source.rfind("flag").unwrap();
        let (name, find_spans) =
            find_all_references(source, &pr, ref_offset).expect("should find references to flag");
        assert_eq!(name, "flag");
        assert!(
            !find_spans.is_empty(),
            "find_all_references must see flag inside the machine transition guard"
        );

        let counts = count_all_references(&pr);
        let count = counts.get("flag").copied().unwrap_or(0);
        assert_eq!(
            count,
            find_spans.len(),
            "count_all_references must agree with find_all_references for flag \
             (previously 0 due to Machine skip); count={count}, find={}",
            find_spans.len(),
        );
    }
}
