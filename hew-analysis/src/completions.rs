//! Completions analysis: build completion suggestions at a given cursor offset.

use std::collections::HashSet;

use hew_parser::ast::{
    Block, Expr, Item, Pattern, Span, Spanned, Stmt, StringPart, TypeBodyItem, TypeDeclKind,
};
use hew_types::check::FnSig;
use hew_types::TypeCheckOutput;

use crate::hover::format_fn_signature_inline;
use crate::method_lookup::{
    collect_method_sigs_for_receiver, find_receiver_type, lookup_type_def_for_receiver,
};
use crate::{CompletionItem, CompletionKind};

/// Hew language keywords for completion.
/// All language keywords, sourced from the lexer (single source of truth).
const KEYWORDS: &[&str] = hew_lexer::ALL_KEYWORDS;

/// Build completion items at the given byte offset.
#[must_use]
pub fn complete(
    source: &str,
    parse_result: &hew_parser::ParseResult,
    type_output: Option<&TypeCheckOutput>,
    offset: usize,
) -> Vec<CompletionItem> {
    if let Some(items) = try_dot_completions(source, parse_result, type_output, offset) {
        return items;
    }

    if let Some(items) = try_spawn_completions(source, parse_result, offset) {
        return items;
    }

    let mut items: Vec<CompletionItem> = KEYWORDS
        .iter()
        .map(|kw| CompletionItem {
            label: (*kw).to_string(),
            kind: CompletionKind::Keyword,
            detail: None,
            insert_text: None,
            insert_text_is_snippet: false,
            sort_text: None,
        })
        .collect();

    // Snippet completions for control-flow keywords.
    items.extend(keyword_snippets());

    // Add identifiers from AST items.
    for (item, _span) in &parse_result.program.items {
        if let Some((name, kind)) = item_name_and_kind(item) {
            items.push(CompletionItem {
                label: name,
                kind,
                detail: None,
                insert_text: None,
                insert_text_is_snippet: false,
                sort_text: None,
            });
        }
    }

    // Add type definitions and function signatures from the type checker.
    if let Some(tc) = type_output {
        for name in tc.type_defs.keys() {
            items.push(CompletionItem {
                label: name.clone(),
                kind: CompletionKind::Type,
                detail: None,
                insert_text: None,
                insert_text_is_snippet: false,
                sort_text: None,
            });
        }
        for (name, sig) in &tc.fn_sigs {
            items.push(fn_sig_completion(name, sig));
        }
    }

    // Collect local variables visible at cursor offset.
    items.extend(collect_locals_at(parse_result, offset));

    // Deduplicate by label, preferring items with detail (richer completions).
    let mut seen = HashSet::new();
    items.retain(|item| seen.insert(item.label.clone()));

    items
}

/// If the cursor is right after a `.`, find the receiver type and offer its methods/fields.
fn try_dot_completions(
    source: &str,
    _parse_result: &hew_parser::ParseResult,
    type_output: Option<&TypeCheckOutput>,
    offset: usize,
) -> Option<Vec<CompletionItem>> {
    let bytes = source.as_bytes();
    // Walk backwards past any whitespace to find the dot.
    let mut dot_pos = offset;
    while dot_pos > 0 && bytes[dot_pos - 1].is_ascii_whitespace() {
        dot_pos -= 1;
    }
    if dot_pos == 0 || bytes[dot_pos - 1] != b'.' {
        return None;
    }
    let receiver_end = dot_pos - 1;
    let tc = type_output?;
    let receiver_ty = find_receiver_type(tc, receiver_end)?;

    let mut items = Vec::new();
    if let Some(type_def) = lookup_type_def_for_receiver(tc, receiver_ty) {
        for (field_name, field_ty) in &type_def.fields {
            items.push(CompletionItem {
                label: field_name.clone(),
                kind: CompletionKind::Field,
                detail: Some(field_ty.user_facing().to_string()),
                insert_text: None,
                insert_text_is_snippet: false,
                sort_text: None,
            });
        }
    }
    for (method_name, sig) in collect_method_sigs_for_receiver(tc, receiver_ty) {
        items.push(fn_sig_completion(&method_name, &sig));
    }
    let mut seen = HashSet::new();
    items.retain(|item| seen.insert(item.label.clone()));
    Some(items)
}

/// If the cursor is right after `spawn `, offer only actor and supervisor names.
fn try_spawn_completions(
    source: &str,
    parse_result: &hew_parser::ParseResult,
    offset: usize,
) -> Option<Vec<CompletionItem>> {
    let before = &source[..offset];
    let trimmed = before.trim_end();
    if !trimmed.ends_with("spawn") {
        return None;
    }

    let mut items = Vec::new();
    for (item, _span) in &parse_result.program.items {
        match item {
            Item::Actor(a) => {
                items.push(CompletionItem {
                    label: a.name.clone(),
                    kind: CompletionKind::Actor,
                    detail: Some("actor".to_string()),
                    insert_text: None,
                    insert_text_is_snippet: false,
                    sort_text: None,
                });
            }
            Item::Supervisor(s) => {
                items.push(CompletionItem {
                    label: s.name.clone(),
                    kind: CompletionKind::Actor,
                    detail: Some("supervisor".to_string()),
                    insert_text: None,
                    insert_text_is_snippet: false,
                    sort_text: None,
                });
            }
            _ => {}
        }
    }

    Some(items)
}

/// Collect local variable names from function/actor bodies that are in scope at `offset`.
fn collect_locals_at(parse_result: &hew_parser::ParseResult, offset: usize) -> Vec<CompletionItem> {
    let mut locals = Vec::new();

    for (item, span) in &parse_result.program.items {
        // Skip items that don't contain the cursor. Treat empty/unset spans
        // (the parser doesn't always record item spans) as containing everything.
        let span_covers = span_contains_offset(span, offset);
        if !span_covers {
            continue;
        }
        match item {
            Item::Function(f) => {
                for p in &f.params {
                    locals.push(local_completion(&p.name));
                }
                collect_locals_from_block(&f.body, offset, &mut locals);
            }
            Item::Actor(a) => {
                for field in &a.fields {
                    locals.push(local_completion(&field.name));
                }
                for recv in &a.receive_fns {
                    for p in &recv.params {
                        locals.push(local_completion(&p.name));
                    }
                    collect_locals_from_block(&recv.body, offset, &mut locals);
                }
                for method in &a.methods {
                    for p in &method.params {
                        locals.push(local_completion(&p.name));
                    }
                    collect_locals_from_block(&method.body, offset, &mut locals);
                }
            }
            Item::TypeDecl(td) => {
                for body_item in &td.body {
                    if let TypeBodyItem::Method(method) = body_item {
                        for p in &method.params {
                            locals.push(local_completion(&p.name));
                        }
                        collect_locals_from_block(&method.body, offset, &mut locals);
                    }
                }
            }
            Item::Impl(i) => {
                for method in &i.methods {
                    for p in &method.params {
                        locals.push(local_completion(&p.name));
                    }
                    collect_locals_from_block(&method.body, offset, &mut locals);
                }
            }
            _ => {}
        }
    }

    locals
}

fn span_contains_offset(span: &Span, offset: usize) -> bool {
    span.is_empty() || (span.start <= offset && offset <= span.end)
}

fn collect_locals_from_block(block: &Block, offset: usize, locals: &mut Vec<CompletionItem>) {
    for (stmt, span) in &block.stmts {
        if span.start > offset {
            break;
        }
        collect_locals_from_stmt(stmt, span, offset, locals);
    }
}

fn collect_locals_from_stmt(
    stmt: &Stmt,
    stmt_span: &Span,
    offset: usize,
    locals: &mut Vec<CompletionItem>,
) {
    let in_stmt_scope = span_contains_offset(stmt_span, offset);
    match stmt {
        Stmt::Let { pattern, .. } => {
            collect_pattern_names(&pattern.0, locals);
        }
        Stmt::Var { name, .. } => {
            locals.push(local_completion(name));
        }
        Stmt::For { pattern, body, .. } => {
            if in_stmt_scope {
                collect_pattern_names(&pattern.0, locals);
                collect_locals_from_block(body, offset, locals);
            }
        }
        Stmt::Loop { body, .. } | Stmt::While { body, .. } => {
            if in_stmt_scope {
                collect_locals_from_block(body, offset, locals);
            }
        }
        Stmt::WhileLet { pattern, body, .. } => {
            if in_stmt_scope {
                collect_pattern_names(&pattern.0, locals);
                collect_locals_from_block(body, offset, locals);
            }
        }
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            if in_stmt_scope {
                collect_locals_from_block(then_block, offset, locals);
                if let Some(eb) = else_block {
                    if let Some(if_stmt) = &eb.if_stmt {
                        collect_locals_from_stmt(&if_stmt.0, &if_stmt.1, offset, locals);
                    }
                    if let Some(block) = &eb.block {
                        collect_locals_from_block(block, offset, locals);
                    }
                }
            }
        }
        Stmt::IfLet {
            pattern,
            body,
            else_body,
            ..
        } => {
            if in_stmt_scope {
                collect_pattern_names(&pattern.0, locals);
                collect_locals_from_block(body, offset, locals);
                if let Some(block) = else_body {
                    collect_locals_from_block(block, offset, locals);
                }
            }
        }
        Stmt::Match { arms, .. } => {
            if in_stmt_scope {
                for arm in arms {
                    if span_contains_offset(&arm.body.1, offset) {
                        collect_pattern_names(&arm.pattern.0, locals);
                        collect_locals_from_expr(&arm.body.0, offset, locals);
                    }
                }
            }
        }
        Stmt::Defer(expr) => {
            collect_locals_from_expr(&expr.0, offset, locals);
        }
        Stmt::Assign { value, .. } => {
            collect_locals_from_expr(&value.0, offset, locals);
        }
        Stmt::Return(Some(val)) => {
            collect_locals_from_expr(&val.0, offset, locals);
        }
        Stmt::Expression(expr) => {
            collect_locals_from_spanned_expr(expr, offset, locals);
        }
        _ => {}
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive AST match — splitting would obscure the traversal"
)]
fn collect_locals_from_expr(expr: &Expr, offset: usize, locals: &mut Vec<CompletionItem>) {
    match expr {
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            collect_locals_from_block(block, offset, locals);
        }
        Expr::Scope { binding, body } => {
            if let Some(name) = binding {
                locals.push(local_completion(name));
            }
            collect_locals_from_block(body, offset, locals);
        }
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            if span_contains_offset(&then_block.1, offset) {
                collect_locals_from_expr(&then_block.0, offset, locals);
            }
            if let Some(else_expr) = else_block {
                if span_contains_offset(&else_expr.1, offset) {
                    collect_locals_from_expr(&else_expr.0, offset, locals);
                }
            }
        }
        Expr::IfLet {
            body, else_body, ..
        } => {
            collect_locals_from_block(body, offset, locals);
            if let Some(block) = else_body {
                collect_locals_from_block(block, offset, locals);
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                if span_contains_offset(&arm.body.1, offset) {
                    collect_pattern_names(&arm.pattern.0, locals);
                    collect_locals_from_expr(&arm.body.0, offset, locals);
                }
            }
        }
        Expr::PostfixTry(inner) | Expr::Cast { expr: inner, .. } => {
            collect_locals_from_expr(&inner.0, offset, locals);
        }
        Expr::Call { function, args, .. } => {
            collect_locals_from_spanned_expr(function, offset, locals);
            for arg in args {
                collect_locals_from_spanned_expr(arg.expr(), offset, locals);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            collect_locals_from_spanned_expr(receiver, offset, locals);
            for arg in args {
                collect_locals_from_spanned_expr(arg.expr(), offset, locals);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_locals_from_spanned_expr(left, offset, locals);
            collect_locals_from_spanned_expr(right, offset, locals);
        }
        Expr::Unary { operand, .. } => {
            collect_locals_from_spanned_expr(operand, offset, locals);
        }
        Expr::Tuple(exprs) | Expr::Array(exprs) | Expr::Join(exprs) => {
            for expr in exprs {
                collect_locals_from_spanned_expr(expr, offset, locals);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            collect_locals_from_spanned_expr(value, offset, locals);
            collect_locals_from_spanned_expr(count, offset, locals);
        }
        Expr::MapLiteral { entries } => {
            for (key, value) in entries {
                collect_locals_from_spanned_expr(key, offset, locals);
                collect_locals_from_spanned_expr(value, offset, locals);
            }
        }
        Expr::StructInit { fields, .. } => {
            for (_, value) in fields {
                collect_locals_from_spanned_expr(value, offset, locals);
            }
        }
        Expr::Send { target, message } => {
            collect_locals_from_spanned_expr(target, offset, locals);
            collect_locals_from_spanned_expr(message, offset, locals);
        }
        Expr::Spawn { target, args } => {
            collect_locals_from_spanned_expr(target, offset, locals);
            for (_, arg) in args {
                collect_locals_from_spanned_expr(arg, offset, locals);
            }
        }
        Expr::SpawnLambdaActor { body, .. } | Expr::Lambda { body, .. } => {
            collect_locals_from_spanned_expr(body, offset, locals);
        }
        Expr::Timeout { expr, duration } => {
            collect_locals_from_spanned_expr(expr, offset, locals);
            collect_locals_from_spanned_expr(duration, offset, locals);
        }
        Expr::FieldAccess { object, .. } => {
            collect_locals_from_spanned_expr(object, offset, locals);
        }
        Expr::Index { object, index } => {
            collect_locals_from_spanned_expr(object, offset, locals);
            collect_locals_from_spanned_expr(index, offset, locals);
        }
        Expr::Await(inner) | Expr::Yield(Some(inner)) => {
            collect_locals_from_spanned_expr(inner, offset, locals);
        }
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                collect_locals_from_spanned_expr(start, offset, locals);
            }
            if let Some(end) = end {
                collect_locals_from_spanned_expr(end, offset, locals);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(expr) = part {
                    collect_locals_from_spanned_expr(expr, offset, locals);
                }
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                if span_contains_offset(&arm.body.1, offset) {
                    collect_pattern_names(&arm.binding.0, locals);
                    collect_locals_from_expr(&arm.body.0, offset, locals);
                }
            }
            if let Some(tc) = timeout {
                if span_contains_offset(&tc.body.1, offset) {
                    collect_locals_from_expr(&tc.body.0, offset, locals);
                }
            }
        }
        _ => {}
    }
}

fn collect_locals_from_spanned_expr(
    expr: &Spanned<Expr>,
    offset: usize,
    locals: &mut Vec<CompletionItem>,
) {
    if span_contains_offset(&expr.1, offset) {
        collect_locals_from_expr(&expr.0, offset, locals);
    }
}

fn collect_pattern_names(pattern: &Pattern, locals: &mut Vec<CompletionItem>) {
    match pattern {
        Pattern::Identifier(name) => locals.push(local_completion(name)),
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
            for (p, _) in patterns {
                collect_pattern_names(p, locals);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                if let Some((pattern, _)) = &field.pattern {
                    collect_pattern_names(pattern, locals);
                } else {
                    locals.push(local_completion(&field.name));
                }
            }
        }
        Pattern::Or(left, right) => {
            collect_pattern_names(&left.0, locals);
            collect_pattern_names(&right.0, locals);
        }
        Pattern::Literal(_) | Pattern::Wildcard => {}
    }
}

fn local_completion(name: &str) -> CompletionItem {
    CompletionItem {
        label: name.to_string(),
        kind: CompletionKind::Variable,
        detail: None,
        insert_text: None,
        insert_text_is_snippet: false,
        sort_text: None,
    }
}

/// Build a completion item from a function signature.
fn fn_sig_completion(name: &str, sig: &FnSig) -> CompletionItem {
    let detail = format_fn_signature_inline(name, sig);
    CompletionItem {
        label: name.to_string(),
        kind: CompletionKind::Function,
        detail: Some(detail),
        insert_text: None,
        insert_text_is_snippet: false,
        sort_text: None,
    }
}

/// Snippet completions for common language constructs.
#[must_use]
#[expect(
    clippy::too_many_lines,
    reason = "each snippet entry is a simple data tuple; splitting would fragment the table"
)]
pub fn keyword_snippets() -> Vec<CompletionItem> {
    let snippets = [
        (
            "fn",
            "fn ${1:name}(${2:params}) {\n\t$0\n}",
            "fn name(params) { ... }",
        ),
        ("if", "if ${1:condition} {\n\t$0\n}", "if condition { ... }"),
        (
            "if else",
            "if ${1:condition} {\n\t$2\n} else {\n\t$0\n}",
            "if condition { ... } else { ... }",
        ),
        (
            "for",
            "for ${1:item} in ${2:collection} {\n\t$0\n}",
            "for item in collection { ... }",
        ),
        (
            "while",
            "while ${1:condition} {\n\t$0\n}",
            "while condition { ... }",
        ),
        (
            "match",
            "match ${1:value} {\n\t${2:pattern} => $0,\n}",
            "match value { pattern => ... }",
        ),
        ("actor", "actor ${1:Name} {\n\t$0\n}", "actor Name { ... }"),
        (
            "type",
            "type ${1:Name} {\n\t$0\n}",
            "type Name { ... }",
        ),
        ("enum", "enum ${1:Name} {\n\t$0\n}", "enum Name { ... }"),
        ("impl", "impl ${1:Type} {\n\t$0\n}", "impl Type { ... }"),
        (
            "spawn",
            "let ${1:handle} = spawn ${2:Actor}(${3:field}: ${4:value});",
            "let handle = spawn Actor(field: value);",
        ),
        (
            "receive",
            "receive ${1:name}(${2:params}) {\n\t$0\n}",
            "receive name(params) { ... }",
        ),
        ("loop", "loop {\n\t$0\n}", "loop { ... }"),
        (
            "async fn",
            "async fn ${1:name}(${2:params}) {\n\t$0\n}",
            "async fn name(params) { ... }",
        ),
        (
            "supervisor",
            "supervisor ${1:Name} {\n\t$0\n}",
            "supervisor Name { ... }",
        ),
        ("trait", "trait ${1:Name} {\n\t$0\n}", "trait Name { ... }"),
        (
            "spawn lambda",
            "let ${1:handle} = spawn (${2:param}: ${3:Type}) => {\n\t$0\n};",
            "let handle = spawn (param: Type) => { ... };",
        ),
        (
            "select",
            "select {\n\t${1:binding} <- ${2:source} => ${3:expr},\n\tafter ${4:duration} => ${0:timeout_expr},\n}",
            "select { pattern <- source => expr, after duration => expr }",
        ),
        (
            "select from",
            "select {\n\t${1:binding} from ${2:source} => ${3:expr},\n\tafter ${4:duration} => ${0:timeout_expr},\n}",
            "select { pattern from source => expr, after duration => expr }",
        ),
        (
            "timeout",
            "${1:expr} | after ${2:duration}",
            "expr | after duration",
        ),
        (
            "defer",
            "defer ${0:expr};",
            "defer expr;",
        ),
        (
            "while let",
            "while let ${1:Some(value)} = ${2:expr} {\n\t$0\n}",
            "while let pattern = expr { ... }",
        ),
        (
            "if let",
            "if let ${1:Some(value)} = ${2:expr} {\n\t$0\n}",
            "if let pattern = expr { ... }",
        ),
        (
            "scope",
            "scope |${1:cancel}| {\n\t$0\n}",
            "scope |cancel| { ... }",
        ),
    ];
    snippets
        .into_iter()
        .map(|(label, snippet, detail)| CompletionItem {
            label: format!("{label}..."),
            kind: CompletionKind::Snippet,
            insert_text: Some(snippet.to_string()),
            insert_text_is_snippet: true,
            detail: Some(detail.to_string()),
            sort_text: Some(format!("0_{label}")),
        })
        .collect()
}

fn item_name_and_kind(item: &Item) -> Option<(String, CompletionKind)> {
    match item {
        Item::Function(f) => Some((f.name.clone(), CompletionKind::Function)),
        Item::Actor(a) => Some((a.name.clone(), CompletionKind::Actor)),
        Item::Supervisor(s) => Some((s.name.clone(), CompletionKind::Actor)),
        Item::Trait(t) => Some((t.name.clone(), CompletionKind::Type)),
        Item::Const(c) => Some((c.name.clone(), CompletionKind::Constant)),
        Item::TypeDecl(td) => {
            let kind = match td.kind {
                TypeDeclKind::Struct | TypeDeclKind::Enum => CompletionKind::Type,
            };
            Some((td.name.clone(), kind))
        }
        Item::Wire(w) => Some((w.name.clone(), CompletionKind::Type)),
        Item::TypeAlias(ta) => Some((ta.name.clone(), CompletionKind::Type)),
        Item::Machine(m) => Some((m.name.clone(), CompletionKind::Type)),
        Item::Import(_) | Item::Impl(_) | Item::ExternBlock(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CURSOR: &str = "/*cursor*/";

    fn labels_at_cursor(source_with_cursor: &str) -> Vec<String> {
        let offset = source_with_cursor
            .find(CURSOR)
            .expect("test source must contain cursor marker");
        let source = source_with_cursor.replacen(CURSOR, "", 1);
        let parse_result = hew_parser::parse(&source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );

        complete(&source, &parse_result, None, offset)
            .into_iter()
            .map(|item| item.label)
            .collect()
    }

    #[test]
    fn completions_include_locals_inside_call_argument_blocks() {
        let labels = labels_at_cursor(
            r"fn example() {
    foo({
        let arg_local = 5;
        /*cursor*/
        arg_local
    });
}",
        );

        assert!(labels.iter().any(|label| label == "arg_local"));
    }

    #[test]
    fn completions_do_not_leak_call_argument_block_locals_after_statement() {
        let labels = labels_at_cursor(
            r"fn example() {
    foo({
        let arg_local = 5;
        arg_local
    });
    let outside = 1;
    /*cursor*/
    outside
}",
        );

        assert!(labels.iter().any(|label| label == "outside"));
        assert!(!labels.iter().any(|label| label == "arg_local"));
    }

    #[test]
    fn completions_include_locals_inside_method_call_receivers() {
        let labels = labels_at_cursor(
            r"fn example() {
    ({
        let receiver_local = 5;
        /*cursor*/
        receiver_local
    }).display();
}",
        );

        assert!(labels.iter().any(|label| label == "receiver_local"));
    }
}
