//! Find-all-references analysis: scope-aware identifier reference collection.

use std::collections::HashMap;

use hew_parser::ast::{
    Block, Expr, Item, Pattern, Span, Stmt, StringPart, TraitItem, TypeBodyItem,
};
use hew_parser::ParseResult;

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
    for (item, _span) in &parse_result.program.items {
        collect_import_binding_refs_in_item(item, name, &mut spans);
    }

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

    let mut spans = Vec::new();
    for (item, _span) in &parse_result.program.items {
        collect_refs_in_item(item, &name, &mut spans);
    }

    // For top-level names (functions, actors, types, receive handlers, fields),
    // return all references globally.
    if is_top_level_name(parse_result, &name) {
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

/// Collect the start offsets of all binding (declaration) sites for `name`
/// across all items in the parse result.
///
/// Walks `Stmt::Let { pattern }`, `Stmt::Var { name }`, and `Stmt::For { pattern }`
/// in the AST rather than using source-text prefix heuristics.
fn collect_binding_starts_in_parse_result(parse_result: &ParseResult, name: &str) -> Vec<usize> {
    let mut out = Vec::new();
    for (item, _) in &parse_result.program.items {
        collect_binding_starts_in_item(item, name, &mut out);
    }
    out
}

fn collect_binding_starts_in_item(item: &Item, name: &str, out: &mut Vec<usize>) {
    match item {
        Item::Function(f) => {
            collect_binding_starts_in_block(&f.body, name, out);
        }
        Item::Actor(a) => {
            if let Some(init) = &a.init {
                collect_binding_starts_in_block(&init.body, name, out);
            }
            if let Some(term) = &a.terminate {
                collect_binding_starts_in_block(&term.body, name, out);
            }
            for recv in &a.receive_fns {
                collect_binding_starts_in_block(&recv.body, name, out);
            }
            for method in &a.methods {
                collect_binding_starts_in_block(&method.body, name, out);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(m) = body_item {
                    collect_binding_starts_in_block(&m.body, name, out);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                collect_binding_starts_in_block(&method.body, name, out);
            }
        }
        Item::Trait(t) => {
            for trait_item in &t.items {
                if let TraitItem::Method(m) = trait_item {
                    if let Some(body) = &m.body {
                        collect_binding_starts_in_block(body, name, out);
                    }
                }
            }
        }
        Item::Const(_)
        | Item::Import(_)
        | Item::ExternBlock(_)
        | Item::Wire(_)
        | Item::TypeAlias(_)
        | Item::Supervisor(_)
        | Item::Machine(_) => {}
    }
}

fn collect_binding_starts_in_block(block: &Block, name: &str, out: &mut Vec<usize>) {
    for (stmt, stmt_span) in &block.stmts {
        collect_binding_starts_in_stmt(stmt, stmt_span, name, out);
    }
}

/// Recurse into expressions that can introduce new bindings (block-containing
/// expressions only). Used when an arm body or similar is `Spanned<Expr>`.
fn collect_binding_starts_in_expr(expr: &Expr, name: &str, out: &mut Vec<usize>) {
    match expr {
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            collect_binding_starts_in_block(block, name, out);
        }
        Expr::Scope { body, .. } => {
            collect_binding_starts_in_block(body, name, out);
        }
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            collect_binding_starts_in_expr(&then_block.0, name, out);
            if let Some(eb) = else_block {
                collect_binding_starts_in_expr(&eb.0, name, out);
            }
        }
        Expr::IfLet {
            pattern,
            body,
            else_body,
            ..
        } => {
            if pattern_binds_name(&pattern.0, name) {
                out.push(pattern.1.start);
            }
            collect_binding_starts_in_block(body, name, out);
            if let Some(block) = else_body {
                collect_binding_starts_in_block(block, name, out);
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                if pattern_binds_name(&arm.pattern.0, name) {
                    out.push(arm.pattern.1.start);
                }
                collect_binding_starts_in_expr(&arm.body.0, name, out);
            }
        }
        _ => {}
    }
}

fn collect_binding_starts_in_stmt(stmt: &Stmt, stmt_span: &Span, name: &str, out: &mut Vec<usize>) {
    match stmt {
        Stmt::Let { pattern, .. } => {
            if pattern_binds_name(&pattern.0, name) {
                out.push(pattern.1.start);
            }
        }
        Stmt::Var {
            name: binding_name, ..
        } => {
            if binding_name == name {
                // Stmt::Var carries no dedicated name-span; use the statement span.
                out.push(stmt_span.start);
            }
        }
        Stmt::For { pattern, body, .. } => {
            if pattern_binds_name(&pattern.0, name) {
                out.push(pattern.1.start);
            }
            collect_binding_starts_in_block(body, name, out);
        }
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            collect_binding_starts_in_block(then_block, name, out);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_binding_starts_in_stmt(&if_stmt.0, &if_stmt.1, name, out);
                }
                if let Some(block) = &eb.block {
                    collect_binding_starts_in_block(block, name, out);
                }
            }
        }
        Stmt::IfLet {
            pattern,
            body,
            else_body,
            ..
        } => {
            if pattern_binds_name(&pattern.0, name) {
                out.push(pattern.1.start);
            }
            collect_binding_starts_in_block(body, name, out);
            if let Some(block) = else_body {
                collect_binding_starts_in_block(block, name, out);
            }
        }
        Stmt::Loop { body, .. } | Stmt::While { body, .. } => {
            collect_binding_starts_in_block(body, name, out);
        }
        Stmt::WhileLet { pattern, body, .. } => {
            if pattern_binds_name(&pattern.0, name) {
                out.push(pattern.1.start);
            }
            collect_binding_starts_in_block(body, name, out);
        }
        Stmt::Match { arms, .. } => {
            for arm in arms {
                if pattern_binds_name(&arm.pattern.0, name) {
                    out.push(arm.pattern.1.start);
                }
                collect_binding_starts_in_expr(&arm.body.0, name, out);
            }
        }
        Stmt::Assign { .. }
        | Stmt::Return(_)
        | Stmt::Break { .. }
        | Stmt::Continue { .. }
        | Stmt::Expression(_)
        | Stmt::Defer(_) => {}
    }
}

fn collect_import_binding_refs_in_item(item: &Item, name: &str, spans: &mut Vec<Span>) {
    match item {
        Item::Function(f) => {
            let shadowed = f.params.iter().any(|param| param.name == name);
            collect_import_binding_refs_in_block(&f.body, name, shadowed, spans);
        }
        Item::Actor(a) => {
            let field_shadowed = a.fields.iter().any(|field| field.name == name);
            if let Some(init) = &a.init {
                collect_import_binding_refs_in_block(&init.body, name, field_shadowed, spans);
            }
            if let Some(term) = &a.terminate {
                collect_import_binding_refs_in_block(&term.body, name, field_shadowed, spans);
            }
            for recv in &a.receive_fns {
                let shadowed = field_shadowed || recv.params.iter().any(|param| param.name == name);
                collect_import_binding_refs_in_block(&recv.body, name, shadowed, spans);
            }
            for method in &a.methods {
                let shadowed =
                    field_shadowed || method.params.iter().any(|param| param.name == name);
                collect_import_binding_refs_in_block(&method.body, name, shadowed, spans);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(method) = body_item {
                    let shadowed = method.params.iter().any(|param| param.name == name);
                    collect_import_binding_refs_in_block(&method.body, name, shadowed, spans);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                let shadowed = method.params.iter().any(|param| param.name == name);
                collect_import_binding_refs_in_block(&method.body, name, shadowed, spans);
            }
        }
        Item::Trait(t) => {
            for trait_item in &t.items {
                if let TraitItem::Method(method) = trait_item {
                    if let Some(body) = &method.body {
                        let shadowed = method.params.iter().any(|param| param.name == name);
                        collect_import_binding_refs_in_block(body, name, shadowed, spans);
                    }
                }
            }
        }
        Item::Const(c) => {
            collect_import_binding_refs_in_expr(&c.value.0, &c.value.1, name, false, spans);
        }
        Item::Import(_)
        | Item::ExternBlock(_)
        | Item::Wire(_)
        | Item::TypeAlias(_)
        | Item::Supervisor(_)
        | Item::Machine(_) => {}
    }
}

fn collect_import_binding_refs_in_block(
    block: &Block,
    name: &str,
    shadowed: bool,
    spans: &mut Vec<Span>,
) {
    let mut shadowed = shadowed;
    for (stmt, _span) in &block.stmts {
        shadowed = collect_import_binding_refs_in_stmt(stmt, name, shadowed, spans);
    }
    if let Some(trailing) = &block.trailing_expr {
        collect_import_binding_refs_in_expr(&trailing.0, &trailing.1, name, shadowed, spans);
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "statement traversal must preserve shadowing order across variants"
)]
fn collect_import_binding_refs_in_stmt(
    stmt: &Stmt,
    name: &str,
    shadowed: bool,
    spans: &mut Vec<Span>,
) -> bool {
    match stmt {
        Stmt::Let { pattern, value, .. } => {
            if let Some(val) = value {
                collect_import_binding_refs_in_expr(&val.0, &val.1, name, shadowed, spans);
            }
            shadowed || pattern_binds_name(&pattern.0, name)
        }
        Stmt::Var {
            name: binding_name,
            value,
            ..
        } => {
            if let Some(val) = value {
                collect_import_binding_refs_in_expr(&val.0, &val.1, name, shadowed, spans);
            }
            shadowed || binding_name == name
        }
        Stmt::Assign { target, value, .. } => {
            collect_import_binding_refs_in_expr(&target.0, &target.1, name, shadowed, spans);
            collect_import_binding_refs_in_expr(&value.0, &value.1, name, shadowed, spans);
            shadowed
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_import_binding_refs_in_expr(&condition.0, &condition.1, name, shadowed, spans);
            collect_import_binding_refs_in_block(then_block, name, shadowed, spans);
            if let Some(else_block) = else_block {
                if let Some(if_stmt) = &else_block.if_stmt {
                    collect_import_binding_refs_in_stmt(&if_stmt.0, name, shadowed, spans);
                }
                if let Some(block) = &else_block.block {
                    collect_import_binding_refs_in_block(block, name, shadowed, spans);
                }
            }
            shadowed
        }
        Stmt::IfLet {
            pattern,
            expr,
            body,
            else_body,
        } => {
            collect_import_binding_refs_in_expr(&expr.0, &expr.1, name, shadowed, spans);
            collect_import_binding_refs_in_block(
                body,
                name,
                shadowed || pattern_binds_name(&pattern.0, name),
                spans,
            );
            if let Some(block) = else_body {
                collect_import_binding_refs_in_block(block, name, shadowed, spans);
            }
            shadowed
        }
        Stmt::Match { scrutinee, arms } => {
            collect_import_binding_refs_in_expr(&scrutinee.0, &scrutinee.1, name, shadowed, spans);
            for arm in arms {
                let arm_shadowed = shadowed || pattern_binds_name(&arm.pattern.0, name);
                if let Some(guard) = &arm.guard {
                    collect_import_binding_refs_in_expr(
                        &guard.0,
                        &guard.1,
                        name,
                        arm_shadowed,
                        spans,
                    );
                }
                collect_import_binding_refs_in_expr(
                    &arm.body.0,
                    &arm.body.1,
                    name,
                    arm_shadowed,
                    spans,
                );
            }
            shadowed
        }
        Stmt::Loop { body, .. } => {
            collect_import_binding_refs_in_block(body, name, shadowed, spans);
            shadowed
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_import_binding_refs_in_expr(&condition.0, &condition.1, name, shadowed, spans);
            collect_import_binding_refs_in_block(body, name, shadowed, spans);
            shadowed
        }
        Stmt::WhileLet {
            pattern,
            expr,
            body,
            ..
        } => {
            collect_import_binding_refs_in_expr(&expr.0, &expr.1, name, shadowed, spans);
            collect_import_binding_refs_in_block(
                body,
                name,
                shadowed || pattern_binds_name(&pattern.0, name),
                spans,
            );
            shadowed
        }
        Stmt::For {
            pattern,
            iterable,
            body,
            ..
        } => {
            collect_import_binding_refs_in_expr(&iterable.0, &iterable.1, name, shadowed, spans);
            collect_import_binding_refs_in_block(
                body,
                name,
                shadowed || pattern_binds_name(&pattern.0, name),
                spans,
            );
            shadowed
        }
        Stmt::Return(Some(val)) => {
            collect_import_binding_refs_in_expr(&val.0, &val.1, name, shadowed, spans);
            shadowed
        }
        Stmt::Defer(expr) => {
            collect_import_binding_refs_in_expr(&expr.0, &expr.1, name, shadowed, spans);
            shadowed
        }
        Stmt::Expression(expr) => {
            collect_import_binding_refs_in_expr(&expr.0, &expr.1, name, shadowed, spans);
            shadowed
        }
        Stmt::Break { value: Some(v), .. } => {
            collect_import_binding_refs_in_expr(&v.0, &v.1, name, shadowed, spans);
            shadowed
        }
        Stmt::Return(None) | Stmt::Break { value: None, .. } | Stmt::Continue { .. } => shadowed,
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "match arms over all Expr variants is clearest as one function"
)]
fn collect_import_binding_refs_in_expr(
    expr: &Expr,
    span: &Span,
    name: &str,
    shadowed: bool,
    spans: &mut Vec<Span>,
) {
    match expr {
        Expr::Identifier(ident) if !shadowed && ident == name => {
            spans.push(span.clone());
        }
        Expr::Binary { left, right, .. } => {
            collect_import_binding_refs_in_expr(&left.0, &left.1, name, shadowed, spans);
            collect_import_binding_refs_in_expr(&right.0, &right.1, name, shadowed, spans);
        }
        Expr::Unary { operand, .. } => {
            collect_import_binding_refs_in_expr(&operand.0, &operand.1, name, shadowed, spans);
        }
        Expr::Call { function, args, .. } => {
            collect_import_binding_refs_in_expr(&function.0, &function.1, name, shadowed, spans);
            for arg in args {
                let expr = arg.expr();
                collect_import_binding_refs_in_expr(&expr.0, &expr.1, name, shadowed, spans);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            collect_import_binding_refs_in_expr(&receiver.0, &receiver.1, name, shadowed, spans);
            for arg in args {
                let expr = arg.expr();
                collect_import_binding_refs_in_expr(&expr.0, &expr.1, name, shadowed, spans);
            }
        }
        Expr::FieldAccess { object, .. } => {
            collect_import_binding_refs_in_expr(&object.0, &object.1, name, shadowed, spans);
        }
        Expr::Index { object, index } => {
            collect_import_binding_refs_in_expr(&object.0, &object.1, name, shadowed, spans);
            collect_import_binding_refs_in_expr(&index.0, &index.1, name, shadowed, spans);
        }
        Expr::StructInit { fields, .. } => {
            for (_, value) in fields {
                collect_import_binding_refs_in_expr(&value.0, &value.1, name, shadowed, spans);
            }
        }
        Expr::Spawn { target, args } => {
            collect_import_binding_refs_in_expr(&target.0, &target.1, name, shadowed, spans);
            for (_, value) in args {
                collect_import_binding_refs_in_expr(&value.0, &value.1, name, shadowed, spans);
            }
        }
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            collect_import_binding_refs_in_block(block, name, shadowed, spans);
        }
        Expr::Scope { binding, body } => {
            collect_import_binding_refs_in_block(
                body,
                name,
                shadowed || binding.as_deref() == Some(name),
                spans,
            );
        }
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_import_binding_refs_in_expr(&condition.0, &condition.1, name, shadowed, spans);
            collect_import_binding_refs_in_expr(
                &then_block.0,
                &then_block.1,
                name,
                shadowed,
                spans,
            );
            if let Some(else_block) = else_block {
                collect_import_binding_refs_in_expr(
                    &else_block.0,
                    &else_block.1,
                    name,
                    shadowed,
                    spans,
                );
            }
        }
        Expr::IfLet {
            pattern,
            expr,
            body,
            else_body,
        } => {
            collect_import_binding_refs_in_expr(&expr.0, &expr.1, name, shadowed, spans);
            collect_import_binding_refs_in_block(
                body,
                name,
                shadowed || pattern_binds_name(&pattern.0, name),
                spans,
            );
            if let Some(block) = else_body {
                collect_import_binding_refs_in_block(block, name, shadowed, spans);
            }
        }
        Expr::Match { scrutinee, arms } => {
            collect_import_binding_refs_in_expr(&scrutinee.0, &scrutinee.1, name, shadowed, spans);
            for arm in arms {
                let arm_shadowed = shadowed || pattern_binds_name(&arm.pattern.0, name);
                if let Some(guard) = &arm.guard {
                    collect_import_binding_refs_in_expr(
                        &guard.0,
                        &guard.1,
                        name,
                        arm_shadowed,
                        spans,
                    );
                }
                collect_import_binding_refs_in_expr(
                    &arm.body.0,
                    &arm.body.1,
                    name,
                    arm_shadowed,
                    spans,
                );
            }
        }
        Expr::Lambda { params, body, .. } | Expr::SpawnLambdaActor { params, body, .. } => {
            let shadowed = shadowed || params.iter().any(|param| param.name == name);
            collect_import_binding_refs_in_expr(&body.0, &body.1, name, shadowed, spans);
        }
        Expr::ArrayRepeat { value, count } => {
            collect_import_binding_refs_in_expr(&value.0, &value.1, name, shadowed, spans);
            collect_import_binding_refs_in_expr(&count.0, &count.1, name, shadowed, spans);
        }
        Expr::MapLiteral { entries } => {
            for (key, value) in entries {
                collect_import_binding_refs_in_expr(&key.0, &key.1, name, shadowed, spans);
                collect_import_binding_refs_in_expr(&value.0, &value.1, name, shadowed, spans);
            }
        }
        Expr::Tuple(elems) | Expr::Array(elems) | Expr::Join(elems) => {
            for elem in elems {
                collect_import_binding_refs_in_expr(&elem.0, &elem.1, name, shadowed, spans);
            }
        }
        Expr::Send { target, message } => {
            collect_import_binding_refs_in_expr(&target.0, &target.1, name, shadowed, spans);
            collect_import_binding_refs_in_expr(&message.0, &message.1, name, shadowed, spans);
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                collect_import_binding_refs_in_expr(
                    &arm.source.0,
                    &arm.source.1,
                    name,
                    shadowed,
                    spans,
                );
                collect_import_binding_refs_in_expr(
                    &arm.body.0,
                    &arm.body.1,
                    name,
                    shadowed || pattern_binds_name(&arm.binding.0, name),
                    spans,
                );
            }
            if let Some(timeout) = timeout {
                collect_import_binding_refs_in_expr(
                    &timeout.duration.0,
                    &timeout.duration.1,
                    name,
                    shadowed,
                    spans,
                );
                collect_import_binding_refs_in_expr(
                    &timeout.body.0,
                    &timeout.body.1,
                    name,
                    shadowed,
                    spans,
                );
            }
        }
        Expr::Timeout {
            expr: inner,
            duration,
        } => {
            collect_import_binding_refs_in_expr(&inner.0, &inner.1, name, shadowed, spans);
            collect_import_binding_refs_in_expr(&duration.0, &duration.1, name, shadowed, spans);
        }
        Expr::Await(inner)
        | Expr::PostfixTry(inner)
        | Expr::Yield(Some(inner))
        | Expr::Cast { expr: inner, .. } => {
            collect_import_binding_refs_in_expr(&inner.0, &inner.1, name, shadowed, spans);
        }
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                collect_import_binding_refs_in_expr(&start.0, &start.1, name, shadowed, spans);
            }
            if let Some(end) = end {
                collect_import_binding_refs_in_expr(&end.0, &end.1, name, shadowed, spans);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(expr) = part {
                    collect_import_binding_refs_in_expr(&expr.0, &expr.1, name, shadowed, spans);
                }
            }
        }
        _ => {}
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

fn collect_refs_in_item(item: &Item, name: &str, spans: &mut Vec<Span>) {
    match item {
        Item::Function(f) => {
            collect_refs_in_block(&f.body, name, spans);
        }
        Item::Actor(a) => {
            if let Some(init) = &a.init {
                collect_refs_in_block(&init.body, name, spans);
            }
            if let Some(term) = &a.terminate {
                collect_refs_in_block(&term.body, name, spans);
            }
            for recv in &a.receive_fns {
                collect_refs_in_block(&recv.body, name, spans);
            }
            for method in &a.methods {
                collect_refs_in_block(&method.body, name, spans);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(m) = body_item {
                    collect_refs_in_block(&m.body, name, spans);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                collect_refs_in_block(&method.body, name, spans);
            }
        }
        Item::Trait(t) => {
            for trait_item in &t.items {
                if let TraitItem::Method(m) = trait_item {
                    if let Some(body) = &m.body {
                        collect_refs_in_block(body, name, spans);
                    }
                }
            }
        }
        Item::Const(c) => {
            collect_refs_in_expr(&c.value.0, &c.value.1, name, spans);
        }
        Item::Import(_)
        | Item::ExternBlock(_)
        | Item::Wire(_)
        | Item::TypeAlias(_)
        | Item::Supervisor(_)
        | Item::Machine(_) => {}
    }
}

fn collect_refs_in_block(block: &Block, name: &str, spans: &mut Vec<Span>) {
    for (stmt, _span) in &block.stmts {
        collect_refs_in_stmt(stmt, name, spans);
    }
    if let Some(trailing) = &block.trailing_expr {
        collect_refs_in_expr(&trailing.0, &trailing.1, name, spans);
    }
}

fn collect_refs_in_stmt(stmt: &Stmt, name: &str, spans: &mut Vec<Span>) {
    match stmt {
        Stmt::Let { pattern, value, .. } => {
            collect_refs_in_pattern(&pattern.0, &pattern.1, name, spans);
            if let Some(val) = value {
                collect_refs_in_expr(&val.0, &val.1, name, spans);
            }
        }
        Stmt::Var { value, .. } => {
            if let Some(val) = value {
                collect_refs_in_expr(&val.0, &val.1, name, spans);
            }
        }
        Stmt::Assign { target, value, .. } => {
            collect_refs_in_expr(&target.0, &target.1, name, spans);
            collect_refs_in_expr(&value.0, &value.1, name, spans);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_refs_in_expr(&condition.0, &condition.1, name, spans);
            collect_refs_in_block(then_block, name, spans);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_refs_in_stmt(&if_stmt.0, name, spans);
                }
                if let Some(block) = &eb.block {
                    collect_refs_in_block(block, name, spans);
                }
            }
        }
        Stmt::IfLet {
            pattern,
            expr,
            body,
            else_body,
        } => {
            collect_refs_in_pattern(&pattern.0, &pattern.1, name, spans);
            collect_refs_in_expr(&expr.0, &expr.1, name, spans);
            collect_refs_in_block(body, name, spans);
            if let Some(block) = else_body {
                collect_refs_in_block(block, name, spans);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            collect_refs_in_expr(&scrutinee.0, &scrutinee.1, name, spans);
            for arm in arms {
                collect_refs_in_pattern(&arm.pattern.0, &arm.pattern.1, name, spans);
                if let Some(guard) = &arm.guard {
                    collect_refs_in_expr(&guard.0, &guard.1, name, spans);
                }
                collect_refs_in_expr(&arm.body.0, &arm.body.1, name, spans);
            }
        }
        Stmt::Loop { body, .. } => {
            collect_refs_in_block(body, name, spans);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_refs_in_expr(&condition.0, &condition.1, name, spans);
            collect_refs_in_block(body, name, spans);
        }
        Stmt::WhileLet {
            pattern,
            expr,
            body,
            ..
        } => {
            collect_refs_in_pattern(&pattern.0, &pattern.1, name, spans);
            collect_refs_in_expr(&expr.0, &expr.1, name, spans);
            collect_refs_in_block(body, name, spans);
        }
        Stmt::For {
            pattern,
            iterable,
            body,
            ..
        } => {
            collect_refs_in_pattern(&pattern.0, &pattern.1, name, spans);
            collect_refs_in_expr(&iterable.0, &iterable.1, name, spans);
            collect_refs_in_block(body, name, spans);
        }
        Stmt::Return(Some(val)) => {
            collect_refs_in_expr(&val.0, &val.1, name, spans);
        }
        Stmt::Defer(expr) => {
            collect_refs_in_expr(&expr.0, &expr.1, name, spans);
        }
        Stmt::Expression(expr) => {
            collect_refs_in_expr(&expr.0, &expr.1, name, spans);
        }
        Stmt::Break { value: Some(v), .. } => {
            collect_refs_in_expr(&v.0, &v.1, name, spans);
        }
        Stmt::Return(None) | Stmt::Break { value: None, .. } | Stmt::Continue { .. } => {}
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "match arms over all Expr variants is clearest as one function"
)]
fn collect_refs_in_expr(expr: &Expr, span: &Span, name: &str, spans: &mut Vec<Span>) {
    match expr {
        Expr::Identifier(ident) if ident == name => {
            spans.push(span.clone());
        }
        Expr::Binary { left, right, .. } => {
            collect_refs_in_expr(&left.0, &left.1, name, spans);
            collect_refs_in_expr(&right.0, &right.1, name, spans);
        }
        Expr::Unary { operand, .. } => {
            collect_refs_in_expr(&operand.0, &operand.1, name, spans);
        }
        Expr::Call { function, args, .. } => {
            collect_refs_in_expr(&function.0, &function.1, name, spans);
            for arg in args {
                let e = arg.expr();
                collect_refs_in_expr(&e.0, &e.1, name, spans);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            collect_refs_in_expr(&receiver.0, &receiver.1, name, spans);
            for arg in args {
                let e = arg.expr();
                collect_refs_in_expr(&e.0, &e.1, name, spans);
            }
        }
        Expr::FieldAccess { object, .. } => {
            collect_refs_in_expr(&object.0, &object.1, name, spans);
        }
        Expr::Index { object, index } => {
            collect_refs_in_expr(&object.0, &object.1, name, spans);
            collect_refs_in_expr(&index.0, &index.1, name, spans);
        }
        Expr::StructInit { fields, .. } => {
            for (_, val) in fields {
                collect_refs_in_expr(&val.0, &val.1, name, spans);
            }
        }
        Expr::Spawn { target, args } => {
            collect_refs_in_expr(&target.0, &target.1, name, spans);
            for (_, val) in args {
                collect_refs_in_expr(&val.0, &val.1, name, spans);
            }
        }
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            collect_refs_in_block(block, name, spans);
        }
        Expr::Scope { body, .. } => {
            collect_refs_in_block(body, name, spans);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_refs_in_expr(&condition.0, &condition.1, name, spans);
            collect_refs_in_expr(&then_block.0, &then_block.1, name, spans);
            if let Some(eb) = else_block {
                collect_refs_in_expr(&eb.0, &eb.1, name, spans);
            }
        }
        Expr::IfLet {
            pattern,
            expr,
            body,
            else_body,
        } => {
            collect_refs_in_pattern(&pattern.0, &pattern.1, name, spans);
            collect_refs_in_expr(&expr.0, &expr.1, name, spans);
            collect_refs_in_block(body, name, spans);
            if let Some(block) = else_body {
                collect_refs_in_block(block, name, spans);
            }
        }
        Expr::Match { scrutinee, arms } => {
            collect_refs_in_expr(&scrutinee.0, &scrutinee.1, name, spans);
            for arm in arms {
                collect_refs_in_pattern(&arm.pattern.0, &arm.pattern.1, name, spans);
                if let Some(guard) = &arm.guard {
                    collect_refs_in_expr(&guard.0, &guard.1, name, spans);
                }
                collect_refs_in_expr(&arm.body.0, &arm.body.1, name, spans);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            collect_refs_in_expr(&body.0, &body.1, name, spans);
        }
        Expr::ArrayRepeat { value, count } => {
            collect_refs_in_expr(&value.0, &value.1, name, spans);
            collect_refs_in_expr(&count.0, &count.1, name, spans);
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                collect_refs_in_expr(&k.0, &k.1, name, spans);
                collect_refs_in_expr(&v.0, &v.1, name, spans);
            }
        }
        Expr::Tuple(elems) | Expr::Array(elems) | Expr::Join(elems) => {
            for elem in elems {
                collect_refs_in_expr(&elem.0, &elem.1, name, spans);
            }
        }
        Expr::Send { target, message } => {
            collect_refs_in_expr(&target.0, &target.1, name, spans);
            collect_refs_in_expr(&message.0, &message.1, name, spans);
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                collect_refs_in_pattern(&arm.binding.0, &arm.binding.1, name, spans);
                collect_refs_in_expr(&arm.source.0, &arm.source.1, name, spans);
                collect_refs_in_expr(&arm.body.0, &arm.body.1, name, spans);
            }
            if let Some(to) = timeout {
                collect_refs_in_expr(&to.duration.0, &to.duration.1, name, spans);
                collect_refs_in_expr(&to.body.0, &to.body.1, name, spans);
            }
        }
        Expr::Timeout { expr: e, duration } => {
            collect_refs_in_expr(&e.0, &e.1, name, spans);
            collect_refs_in_expr(&duration.0, &duration.1, name, spans);
        }
        Expr::Await(inner)
        | Expr::PostfixTry(inner)
        | Expr::Yield(Some(inner))
        | Expr::Cast { expr: inner, .. } => {
            collect_refs_in_expr(&inner.0, &inner.1, name, spans);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_refs_in_expr(&s.0, &s.1, name, spans);
            }
            if let Some(e) = end {
                collect_refs_in_expr(&e.0, &e.1, name, spans);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(e) = part {
                    collect_refs_in_expr(&e.0, &e.1, name, spans);
                }
            }
        }
        _ => {}
    }
}

fn collect_refs_in_pattern(pattern: &Pattern, span: &Span, name: &str, spans: &mut Vec<Span>) {
    match pattern {
        Pattern::Identifier(ident) if ident == name => {
            spans.push(span.clone());
        }
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
            for (p, s) in patterns {
                collect_refs_in_pattern(p, s, name, spans);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                if let Some((p, s)) = &field.pattern {
                    collect_refs_in_pattern(p, s, name, spans);
                }
            }
        }
        Pattern::Or(left, right) => {
            collect_refs_in_pattern(&left.0, &left.1, name, spans);
            collect_refs_in_pattern(&right.0, &right.1, name, spans);
        }
        _ => {}
    }
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
        Item::Import(_)
        | Item::ExternBlock(_)
        | Item::Wire(_)
        | Item::TypeAlias(_)
        | Item::Supervisor(_)
        | Item::Machine(_) => {}
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
}
