//! Go-to-definition analysis: find the definition site of an identifier in the AST.

use hew_parser::ast::{FnDecl, Item, Param, Span, TraitItem, TypeBodyItem};
use hew_parser::ParseResult;
use hew_types::{Ty, TypeCheckOutput};

use crate::ast_visit::{self, BindingKind};
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
    find_visible_binding(source, parse_result, word, offset, BindingKind::Local)
}

/// Find the definition site of a function or method parameter whose binding is
/// in scope at `offset`.
///
/// Parameters live on declaration metadata (`fn_span`, `params`) rather than
/// inside item bodies, so this stays as a thin item-level scan rather than
/// going through `visible_bindings_at` — that helper would require a `&str`
/// source argument which is not part of the public signature here.
#[must_use]
pub fn find_param_definition(
    parse_result: &ParseResult,
    word: &str,
    offset: usize,
) -> Option<OffsetSpan> {
    parse_result
        .program
        .items
        .iter()
        .find_map(|(item, _)| find_param_in_item(item, word, offset))
}

fn find_visible_binding(
    source: &str,
    parse_result: &ParseResult,
    word: &str,
    offset: usize,
    kind: BindingKind,
) -> Option<OffsetSpan> {
    let bindings = ast_visit::visible_bindings_at(source, parse_result, offset);
    bindings
        .into_iter()
        .find(|binding| binding.kind == kind && binding.name == word)
        .map(|binding| OffsetSpan {
            start: binding.span.start,
            end: binding.span.end,
        })
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
    let in_decl = decl_span.is_empty() || (decl_span.start <= offset && offset <= decl_span.end);
    if !in_decl {
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
