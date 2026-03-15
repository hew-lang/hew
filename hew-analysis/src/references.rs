//! Find-all-references analysis: scope-aware identifier reference collection.

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

/// Check if a name matches a top-level item definition (function, actor, type, etc.).
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
    if let Some(scope) = find_defining_scope(parse_result, offset) {
        spans.retain(|s| s.start >= scope.start && s.end <= scope.end);
    }

    // Further refine: if the name is shadowed within the scope (e.g., a for-loop
    // re-declares `x`), only keep references that share the same binding.
    // Strategy: find all binding (declaration) spans for `name` within the scope,
    // then determine which binding the cursor belongs to and keep only its references.
    if spans.len() > 1 {
        let binding_spans: Vec<&Span> = spans
            .iter()
            .filter(|s| is_binding_span(source, s, &name))
            .collect();

        // If there are multiple bindings of the same name (shadowing), disambiguate.
        if binding_spans.len() > 1 {
            // Find which binding region the cursor falls in.
            // Each binding "owns" references from its declaration to the next binding
            // (or end of scope).
            let mut sorted_bindings: Vec<usize> = binding_spans.iter().map(|s| s.start).collect();
            sorted_bindings.sort_unstable();

            // Find the binding region containing the cursor offset.
            let mut region_start = 0;
            let mut region_end = usize::MAX;
            for (i, &bstart) in sorted_bindings.iter().enumerate() {
                if bstart <= offset {
                    region_start = bstart;
                    region_end = sorted_bindings.get(i + 1).copied().unwrap_or(usize::MAX);
                }
            }

            spans.retain(|s| s.start >= region_start && s.start < region_end);
        }
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

/// Heuristic: check if a span is a binding (declaration) site for a variable name.
/// Looks at the source text preceding the identifier to detect let/var/for keywords.
fn is_binding_span(source: &str, span: &Span, _name: &str) -> bool {
    if span.start < 2 || span.start > source.len() {
        return false;
    }
    let prefix_start = span.start.saturating_sub(20);
    let prefix = source.get(prefix_start..span.start).unwrap_or("");
    let trimmed = prefix.trim_end();
    trimmed.ends_with("let") || trimmed.ends_with("var") || trimmed.ends_with("for")
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
}
