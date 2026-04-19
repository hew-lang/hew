//! Go-to-definition analysis: find the definition site of an identifier in the AST.

use hew_parser::ast::{Block, FnDecl, Item, Param, Pattern, Span, Stmt, TraitItem, TypeBodyItem};
use hew_parser::ParseResult;
use hew_types::{Ty, TypeCheckOutput};

use crate::OffsetSpan;

/// Search for a definition matching `word` in the AST, including nested items.
///
/// Returns the byte-offset span of the **name identifier** within the defining
/// item, not the span of the whole item. Returns `None` if no matching
/// definition is found.
#[must_use]
pub fn find_definition(source: &str, parse_result: &ParseResult, word: &str) -> Option<OffsetSpan> {
    for (item, span) in &parse_result.program.items {
        // Top-level name matching.
        let name = match item {
            Item::Function(f) => Some(&f.name),
            Item::Actor(a) => Some(&a.name),
            Item::Supervisor(s) => Some(&s.name),
            Item::Trait(t) => Some(&t.name),
            Item::Const(c) => Some(&c.name),
            Item::TypeDecl(td) => Some(&td.name),
            Item::Wire(w) => Some(&w.name),
            Item::TypeAlias(ta) => Some(&ta.name),
            Item::Machine(m) => Some(&m.name),
            _ => None,
        };
        if name.is_some_and(|n| n == word) {
            return Some(crate::util::find_name_span(source, span.start, word));
        }

        // Search inside actors for fields, receive methods, and methods.
        if let Item::Actor(a) = item {
            for field in &a.fields {
                if field.name == word {
                    return Some(crate::util::find_name_span(source, span.start, word));
                }
            }
            for recv in &a.receive_fns {
                if recv.name == word {
                    // Use the receive fn's own span when available; fall back to
                    // the enclosing item span.
                    let search_from = if recv.span.is_empty() {
                        span.start
                    } else {
                        recv.span.start
                    };
                    return Some(crate::util::find_name_span(source, search_from, word));
                }
            }
            for method in &a.methods {
                if method.name == word {
                    return Some(crate::util::find_name_span(
                        source,
                        method.decl_span.start,
                        word,
                    ));
                }
            }
        }

        // Search inside TypeDecl for fields, variants, and methods.
        if let Item::TypeDecl(td) = item {
            for body_item in &td.body {
                match body_item {
                    TypeBodyItem::Field { name, .. } if name == word => {
                        return Some(crate::util::find_name_span(source, span.start, word));
                    }
                    TypeBodyItem::Variant(v) if v.name == word => {
                        return Some(crate::util::find_name_span(source, span.start, word));
                    }
                    TypeBodyItem::Method(m) if m.name == word => {
                        return Some(crate::util::find_name_span(source, m.decl_span.start, word));
                    }
                    _ => {}
                }
            }
        }

        // Search inside Trait for method and associated type definitions.
        if let Item::Trait(t) = item {
            for trait_item in &t.items {
                match trait_item {
                    TraitItem::Method(m) if m.name == word => {
                        return Some(crate::util::find_name_span(source, span.start, word));
                    }
                    TraitItem::AssociatedType { name, .. } if name == word => {
                        return Some(crate::util::find_name_span(source, span.start, word));
                    }
                    _ => {}
                }
            }
        }

        // Search inside Impl for methods.
        if let Item::Impl(i) = item {
            for method in &i.methods {
                if method.name == word {
                    return Some(crate::util::find_name_span(source, span.start, word));
                }
            }
        }

        // Search inside extern blocks for function declarations.
        if let Item::ExternBlock(extern_block) = item {
            for function in &extern_block.functions {
                if function.name == word {
                    return Some(crate::util::find_name_span(source, span.start, word));
                }
            }
        }
    }
    None
}

/// Find the definition site of a local `let`/`var` binding that is in scope at
/// `offset`.
#[must_use]
pub fn find_local_binding_definition(
    source: &str,
    parse_result: &ParseResult,
    word: &str,
    offset: usize,
) -> Option<OffsetSpan> {
    for (item, _) in &parse_result.program.items {
        if let Some(span) = find_local_in_item(source, item, word, offset) {
            return Some(span);
        }
    }
    None
}

/// Find the definition site of a function or method parameter whose binding is
/// in scope at `offset`.
#[must_use]
pub fn find_param_definition(
    parse_result: &ParseResult,
    word: &str,
    offset: usize,
) -> Option<OffsetSpan> {
    for (item, _) in &parse_result.program.items {
        if let Some(span) = find_param_in_item(item, word, offset) {
            return Some(span);
        }
    }
    None
}

/// Find the definition site of a struct field accessed at `offset`, such as the
/// `x` in `p.x`.
#[must_use]
pub fn find_field_definition(
    source: &str,
    parse_result: &ParseResult,
    type_output: &TypeCheckOutput,
    offset: usize,
) -> Option<OffsetSpan> {
    let (field_name, field_span) = crate::util::simple_word_at_offset(source, offset)?;
    let receiver_end = find_field_receiver_end(source, field_span.start)?;
    let receiver_ty = crate::method_lookup::find_receiver_type(type_output, receiver_end)?;
    let receiver_type_name = receiver_ty.type_name()?;
    let resolved_type_name = type_output
        .type_defs
        .keys()
        .find(|name| Ty::names_match_qualified(name, receiver_type_name))?;
    find_type_field_definition(source, parse_result, resolved_type_name, &field_name)
}

fn find_local_in_item(source: &str, item: &Item, word: &str, offset: usize) -> Option<OffsetSpan> {
    match item {
        Item::Function(function) => find_local_in_block(source, &function.body, word, offset),
        Item::Actor(actor) => {
            if let Some(init) = &actor.init {
                if let Some(span) = find_local_in_block(source, &init.body, word, offset) {
                    return Some(span);
                }
            }
            if let Some(term) = &actor.terminate {
                if let Some(span) = find_local_in_block(source, &term.body, word, offset) {
                    return Some(span);
                }
            }
            for recv in &actor.receive_fns {
                if let Some(span) = find_local_in_block(source, &recv.body, word, offset) {
                    return Some(span);
                }
            }
            for method in &actor.methods {
                if let Some(span) = find_local_in_block(source, &method.body, word, offset) {
                    return Some(span);
                }
            }
            None
        }
        Item::TypeDecl(type_decl) => {
            for body_item in &type_decl.body {
                if let TypeBodyItem::Method(method) = body_item {
                    if let Some(span) = find_local_in_block(source, &method.body, word, offset) {
                        return Some(span);
                    }
                }
            }
            None
        }
        Item::Impl(impl_decl) => {
            for method in &impl_decl.methods {
                if let Some(span) = find_local_in_block(source, &method.body, word, offset) {
                    return Some(span);
                }
            }
            None
        }
        Item::Trait(trait_decl) => {
            for trait_item in &trait_decl.items {
                if let TraitItem::Method(method) = trait_item {
                    if let Some(body) = &method.body {
                        if let Some(span) = find_local_in_block(source, body, word, offset) {
                            return Some(span);
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn find_local_in_block(
    source: &str,
    block: &Block,
    word: &str,
    offset: usize,
) -> Option<OffsetSpan> {
    let mut found = None;
    for (stmt, stmt_span) in &block.stmts {
        if stmt_span.start > offset {
            break;
        }
        if let Some(span) = find_local_in_stmt(source, stmt, stmt_span, word, offset) {
            found = Some(span);
        }
    }
    found
}

fn find_local_in_stmt(
    source: &str,
    stmt: &Stmt,
    stmt_span: &Span,
    word: &str,
    offset: usize,
) -> Option<OffsetSpan> {
    match stmt {
        Stmt::Let { pattern, .. } => find_binding_definition(source, pattern, word, offset),
        Stmt::Var { name, .. } => {
            if name == word {
                Some(crate::util::find_name_span(source, stmt_span.start, word))
            } else {
                None
            }
        }
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            let mut found = None;
            if block_contains_offset(then_block, offset) {
                found = find_local_in_block(source, then_block, word, offset);
            }
            if let Some(else_block) = else_block {
                if let Some(if_stmt) = &else_block.if_stmt {
                    if span_contains_offset(&if_stmt.1, offset) {
                        found = find_local_in_stmt(source, &if_stmt.0, &if_stmt.1, word, offset);
                    }
                } else if let Some(block) = &else_block.block {
                    if block_contains_offset(block, offset) {
                        found = find_local_in_block(source, block, word, offset);
                    }
                }
            }
            found
        }
        Stmt::IfLet {
            pattern,
            body,
            else_body,
            ..
        } => {
            if block_contains_offset(body, offset) {
                return find_binding_definition(source, pattern, word, offset)
                    .or_else(|| find_local_in_block(source, body, word, offset));
            }
            else_body.as_ref().and_then(|block| {
                block_contains_offset(block, offset)
                    .then(|| find_local_in_block(source, block, word, offset))
                    .flatten()
            })
        }
        Stmt::For { pattern, body, .. } => block_contains_offset(body, offset)
            .then(|| {
                find_binding_definition(source, pattern, word, offset)
                    .or_else(|| find_local_in_block(source, body, word, offset))
            })
            .flatten(),
        Stmt::Loop { body, .. } | Stmt::While { body, .. } => block_contains_offset(body, offset)
            .then(|| find_local_in_block(source, body, word, offset))
            .flatten(),
        Stmt::WhileLet { pattern, body, .. } => block_contains_offset(body, offset)
            .then(|| {
                find_binding_definition(source, pattern, word, offset)
                    .or_else(|| find_local_in_block(source, body, word, offset))
            })
            .flatten(),
        Stmt::Match { arms, .. } => arms.iter().find_map(|arm| {
            let in_arm_scope = arm
                .guard
                .as_ref()
                .is_some_and(|(_, guard_span)| span_contains_offset(guard_span, offset))
                || span_contains_offset(&arm.body.1, offset);
            if in_arm_scope {
                find_binding_definition(source, &arm.pattern, word, offset)
            } else {
                None
            }
        }),
        _ => None,
    }
}

fn find_binding_definition(
    source: &str,
    pattern: &(Pattern, Span),
    word: &str,
    offset: usize,
) -> Option<OffsetSpan> {
    if pattern.1.start > offset {
        return None;
    }
    match &pattern.0 {
        Pattern::Identifier(name) => {
            if name == word {
                Some(crate::util::find_name_span(source, pattern.1.start, word))
            } else {
                None
            }
        }
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => patterns
            .iter()
            .find_map(|pattern| find_binding_definition(source, pattern, word, offset)),
        Pattern::Struct { fields, .. } => fields.iter().find_map(|field| {
            field
                .pattern
                .as_ref()
                .and_then(|pattern| find_binding_definition(source, pattern, word, offset))
        }),
        Pattern::Or(left, right) => find_binding_definition(source, left, word, offset)
            .or_else(|| find_binding_definition(source, right, word, offset)),
        Pattern::Wildcard | Pattern::Literal(_) => None,
    }
}

fn find_param_in_item(item: &Item, word: &str, offset: usize) -> Option<OffsetSpan> {
    match item {
        Item::Function(function) => {
            find_param_in_decl(&function.fn_span, &function.params, word, offset)
        }
        Item::Actor(actor) => {
            for recv in &actor.receive_fns {
                if let Some(span) = find_param_in_decl(&recv.span, &recv.params, word, offset) {
                    return Some(span);
                }
            }
            for method in &actor.methods {
                if let Some(span) = find_param_in_method(method, word, offset) {
                    return Some(span);
                }
            }
            None
        }
        Item::TypeDecl(type_decl) => {
            for body_item in &type_decl.body {
                if let TypeBodyItem::Method(method) = body_item {
                    if let Some(span) = find_param_in_method(method, word, offset) {
                        return Some(span);
                    }
                }
            }
            None
        }
        Item::Impl(impl_decl) => {
            for method in &impl_decl.methods {
                if let Some(span) = find_param_in_method(method, word, offset) {
                    return Some(span);
                }
            }
            None
        }
        Item::Trait(trait_decl) => {
            for trait_item in &trait_decl.items {
                if let TraitItem::Method(method) = trait_item {
                    if let Some(span) =
                        find_param_in_decl(&method.span, &method.params, word, offset)
                    {
                        return Some(span);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn find_param_in_method(method: &FnDecl, word: &str, offset: usize) -> Option<OffsetSpan> {
    find_param_in_decl(&method.fn_span, &method.params, word, offset)
}

fn find_param_in_decl(
    decl_span: &Span,
    params: &[Param],
    word: &str,
    offset: usize,
) -> Option<OffsetSpan> {
    if !span_contains_offset(decl_span, offset) {
        return None;
    }
    params
        .iter()
        .find(|param| param.name == word)
        .map(param_name_span)
}

fn param_name_span(param: &Param) -> OffsetSpan {
    let end = param.ty.1.start.saturating_sub(2);
    let start = end.saturating_sub(param.name.len());
    OffsetSpan { start, end }
}

pub(crate) fn find_field_receiver_end(source: &str, field_start: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    let mut dot_pos = field_start;
    while dot_pos > 0 && bytes[dot_pos - 1].is_ascii_whitespace() {
        dot_pos -= 1;
    }
    (dot_pos > 0 && bytes[dot_pos - 1] == b'.').then_some(dot_pos - 1)
}

fn find_type_field_definition(
    source: &str,
    parse_result: &ParseResult,
    type_name: &str,
    field_name: &str,
) -> Option<OffsetSpan> {
    for (item, item_span) in &parse_result.program.items {
        let Item::TypeDecl(type_decl) = item else {
            continue;
        };
        if !Ty::names_match_qualified(type_name, &type_decl.name) {
            continue;
        }
        let mut search_from = item_span.start;
        for body_item in &type_decl.body {
            match body_item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let span = crate::util::find_name_span(source, search_from, name);
                    if name == field_name {
                        return Some(span);
                    }
                    search_from = ty.1.end.max(span.end);
                }
                TypeBodyItem::Variant(variant) => {
                    search_from =
                        crate::util::find_name_span(source, search_from, &variant.name).end;
                }
                TypeBodyItem::Method(method) => {
                    search_from = search_from.max(method.decl_span.end);
                }
            }
        }
    }
    None
}

fn block_contains_offset(block: &Block, offset: usize) -> bool {
    let start = block
        .stmts
        .first()
        .map(|(_, span)| span.start)
        .or_else(|| block.trailing_expr.as_ref().map(|expr| expr.1.start));
    let end = block
        .trailing_expr
        .as_ref()
        .map(|expr| expr.1.end)
        .or_else(|| block.stmts.last().map(|(_, span)| span.end));
    matches!((start, end), (Some(start), Some(end)) if start <= offset && offset <= end)
}

fn span_contains_offset(span: &Span, offset: usize) -> bool {
    span.is_empty() || (span.start <= offset && offset <= span.end)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> hew_parser::ParseResult {
        hew_parser::parse(source)
    }

    #[test]
    fn definition_finds_machine_type() {
        let source = "machine TrafficLight { state Green; state Red; }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "TrafficLight");
        assert!(
            result.is_some(),
            "go-to-definition should resolve machine type name"
        );
        // Span must cover "TrafficLight", not the whole item.
        let span = result.unwrap();
        assert_eq!(&source[span.start..span.end], "TrafficLight");
    }

    #[test]
    fn definition_machine_name_not_confused_with_state() {
        let source = "machine TrafficLight { state Green; state Red; }";
        let pr = parse(source);
        // State names are not top-level items; only the machine name resolves.
        let result = find_definition(source, &pr, "Green");
        assert!(
            result.is_none(),
            "machine state names are not top-level definition sites"
        );
    }

    #[test]
    fn definition_lands_on_name_not_keyword() {
        let source = "fn greet() {}";
        let pr = parse(source);
        let result = find_definition(source, &pr, "greet").expect("should find greet");
        assert_eq!(&source[result.start..result.end], "greet");
    }

    #[test]
    fn definition_actor_name() {
        let source = "actor Counter { receive fn inc() {} }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "Counter").expect("should find Counter");
        assert_eq!(&source[result.start..result.end], "Counter");
    }

    #[test]
    fn definition_receive_fn_name() {
        let source = "actor Counter { receive fn inc() {} }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "inc").expect("should find inc");
        assert_eq!(&source[result.start..result.end], "inc");
    }

    #[test]
    fn definition_type_alias_name() {
        let source = "type Foo = i32;";
        let pr = parse(source);
        let result = find_definition(source, &pr, "Foo").expect("should find Foo");
        assert_eq!(&source[result.start..result.end], "Foo");
    }

    #[test]
    fn definition_actor_method_uses_decl_span() {
        let source = "actor Counter { receive fn ping(foo: i32) { foo } fn foo() {} }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "foo").expect("should find actor method");
        let method_start = source.rfind("fn foo").expect("method should exist") + 3;
        assert_eq!(result.start, method_start);
        assert_eq!(&source[result.start..result.end], "foo");
    }

    #[test]
    fn definition_type_method_uses_decl_span() {
        let source = "type Counter { value: i32 fn foo(value: i32) -> i32 { value } }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "foo").expect("should find type method");
        let method_start = source.rfind("fn foo").expect("method should exist") + 3;
        assert_eq!(result.start, method_start);
        assert_eq!(&source[result.start..result.end], "foo");
    }

    #[test]
    fn definition_finds_struct_field_from_field_access() {
        let source =
            "type Point { x: i32; y: i32 }\nfn main() { let p = Point { x: 1, y: 2 }; p.x }";
        let pr = parse(source);
        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&pr.program);
        let offset = source.rfind("p.x").expect("field access should exist") + 2;

        let result =
            find_field_definition(source, &pr, &type_output, offset).expect("should find field");
        let expected_start = source
            .find("x: i32")
            .expect("field declaration should exist");
        assert_eq!(result.start, expected_start);
        assert_eq!(&source[result.start..result.end], "x");
    }

    #[test]
    fn definition_finds_struct_field_declaration() {
        let source = "type Point { x: i32; y: i32 }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "x").expect("should find field declaration");
        let expected_start = source
            .find("x: i32")
            .expect("field declaration should exist");
        assert_eq!(result.start, expected_start);
        assert_eq!(&source[result.start..result.end], "x");
    }

    #[test]
    fn definition_ignores_struct_init_field_names() {
        let source = "type Point { x: i32 }\nfn main() { Point { x: 1 } }";
        let pr = parse(source);
        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&pr.program);
        let offset = source
            .rfind("x: 1")
            .expect("struct init field should exist");

        assert!(find_field_definition(source, &pr, &type_output, offset).is_none());
    }

    #[test]
    fn definition_finds_local_let_binding() {
        let source = "fn main() { let x = 1; let _ = x + 2; }";
        let pr = parse(source);
        let offset = source.find("x + 2").expect("usage should exist");
        let result = find_local_binding_definition(source, &pr, "x", offset)
            .expect("should find let binding");
        let binding_offset = source.find("let x").expect("binding should exist") + 4;
        assert_eq!(result.start, binding_offset);
        assert_eq!(&source[result.start..result.end], "x");
    }

    #[test]
    fn definition_finds_local_var_binding() {
        let source = "fn main() { var count = 0; count = count + 1; }";
        let pr = parse(source);
        let offset = source.rfind("count").expect("usage should exist");
        let result = find_local_binding_definition(source, &pr, "count", offset)
            .expect("should find var binding");
        let binding_offset = source.find("var count").expect("binding should exist") + 4;
        assert_eq!(result.start, binding_offset);
        assert_eq!(&source[result.start..result.end], "count");
    }

    #[test]
    fn definition_finds_shadowing_local_binding() {
        let source = "fn main() { let x = 1; let x = 2; x + 1; }";
        let pr = parse(source);
        let offset = source.rfind("x + 1").expect("usage should exist");
        let result = find_local_binding_definition(source, &pr, "x", offset)
            .expect("should find inner binding");
        let binding_offset = source.rfind("let x").expect("inner binding should exist") + 4;
        assert_eq!(result.start, binding_offset);
    }

    #[test]
    fn definition_finds_for_pattern_binding() {
        let source = "fn main() { for item in [1, 2] { item } }";
        let pr = parse(source);
        let offset = source.rfind("item").expect("usage should exist");
        let result = find_local_binding_definition(source, &pr, "item", offset)
            .expect("should find for pattern binding");
        let binding_offset = source.find("item in").expect("binding should exist");
        assert_eq!(result.start, binding_offset);
        assert_eq!(&source[result.start..result.end], "item");
    }

    #[test]
    fn definition_finds_while_let_pattern_binding() {
        let source = "fn pair() -> (bool, int) { (true, 1) }\nfn main() { while let (flag, _) = pair() { flag } }";
        let pr = parse(source);
        let offset = source.rfind("flag").expect("usage should exist");
        let result = find_local_binding_definition(source, &pr, "flag", offset)
            .expect("should find while-let pattern binding");
        let binding_offset = source.find("flag, _").expect("binding should exist");
        assert_eq!(result.start, binding_offset);
        assert_eq!(&source[result.start..result.end], "flag");
    }

    #[test]
    fn definition_finds_if_let_pattern_binding() {
        let source = "fn pair() -> (bool, int) { (true, 1) }\nfn main() { if let (flag, _) = pair() { flag } }";
        let pr = parse(source);
        let offset = source.rfind("flag").expect("usage should exist");
        let result = find_local_binding_definition(source, &pr, "flag", offset)
            .expect("should find if-let pattern binding");
        let binding_offset = source.find("flag, _").expect("binding should exist");
        assert_eq!(result.start, binding_offset);
        assert_eq!(&source[result.start..result.end], "flag");
    }

    #[test]
    fn definition_finds_match_arm_pattern_binding() {
        let source = "fn pair() -> (bool, int) { (true, 1) }\nfn main() { match pair() { (flag, _) => flag, } }";
        let pr = parse(source);
        let offset = source.rfind("flag").expect("usage should exist");
        let result = find_local_binding_definition(source, &pr, "flag", offset)
            .expect("should find match-arm pattern binding");
        let binding_offset = source.find("flag, _").expect("binding should exist");
        assert_eq!(result.start, binding_offset);
        assert_eq!(&source[result.start..result.end], "flag");
    }

    #[test]
    fn definition_finds_function_param() {
        let source = "fn add(x: i32, y: i32) -> i32 { x + y }";
        let pr = parse(source);
        let offset = source.find("x + y").expect("usage should exist");
        let result = find_param_definition(&pr, "x", offset).expect("should find parameter");
        let param_offset = source.find("x: i32").expect("param should exist");
        assert_eq!(result.start, param_offset);
        assert_eq!(&source[result.start..result.end], "x");
    }

    #[test]
    fn definition_param_does_not_leak_across_functions() {
        let source = "fn a(x: i32) -> i32 { x }\nfn b() -> i32 { 1 }";
        let pr = parse(source);
        let offset = source.rfind("1 }").expect("usage should exist");
        assert!(find_param_definition(&pr, "x", offset).is_none());
    }

    #[test]
    fn definition_actor_field_found() {
        // Actor fields are top-level names inside the actor; find_definition must
        // resolve them so that detect_conflicts can produce ShadowsTopLevel when a
        // rename target collides with a field name.
        let source = "actor Counter { count: i64; receive fn inc() {} }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "count").expect("should find actor field");
        assert_eq!(&source[result.start..result.end], "count");
    }
}
