use super::*;

#[test]
fn checked_call_hierarchy_incoming_keeps_same_named_methods_separate() {
    let source = "type A {\n    x: i64;\n}\n\nimpl A {\n    fn get(self) -> i64 {\n        self.x\n    }\n}\n\ntype B {\n    x: i64;\n}\n\nimpl B {\n    fn get(self) -> i64 {\n        self.x\n    }\n}\n\nfn main() {\n    let a = A { x: 1 };\n    let b = B { x: 2 };\n    println(a.get());\n    println(b.get());\n}\n";
    let doc = make_typed_doc(source);
    let uri = make_test_uri("/identity-call-hierarchy.hew");
    let declaration = source.rfind("fn get").unwrap() + 3;
    let item = super::super::hierarchy::find_callable_at_offset(
        &uri,
        source,
        &doc.line_offsets,
        &doc.parse_result,
        declaration,
    )
    .expect("B.get callable item");
    let calls = find_incoming_calls(
        &uri,
        source,
        &doc.line_offsets,
        &doc.parse_result,
        "get",
        Some(&item),
        doc.type_output.as_ref(),
    );
    let from_ranges: Vec<_> = calls.iter().flat_map(|call| &call.from_ranges).collect();
    let b_call = source.rfind("b.get()").unwrap();
    let expected = offset_range_to_lsp(source, &doc.line_offsets, b_call, b_call + 7);
    assert_eq!(from_ranges, vec![&expected]);

    let main_declaration = source.find("fn main").unwrap() + 3;
    let main_item = super::super::hierarchy::find_callable_at_offset(
        &uri,
        source,
        &doc.line_offsets,
        &doc.parse_result,
        main_declaration,
    )
    .expect("main callable item");
    let outgoing = find_outgoing_calls(
        &uri,
        source,
        &doc.line_offsets,
        &doc.parse_result,
        "main",
        Some(&main_item),
        doc.type_output.as_ref(),
    );
    let gets: Vec<_> = outgoing
        .iter()
        .filter(|call| call.to.name == "get")
        .collect();
    assert_eq!(gets.len(), 2, "A.get and B.get are separate targets");
    assert_ne!(gets[0].to.selection_range, gets[1].to.selection_range);
}

// ── Type hierarchy tests ────────────────────────────────────────

#[test]
fn type_hierarchy_item_for_struct() {
    let source = "type Point {\n    x: i32;\n    y: i32;\n}\n";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let item = find_type_hierarchy_item(&uri, source, &lo, &parse_result, "Point");
    assert!(item.is_some(), "should find type hierarchy item for Point");
    let item = item.unwrap();
    assert_eq!(item.name, "Point");
    assert_eq!(item.kind, SymbolKind::STRUCT);
}

#[test]
fn type_hierarchy_item_for_actor() {
    let source = "actor Worker { receive fn process() { 0 } }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let item = find_type_hierarchy_item(&uri, source, &lo, &parse_result, "Worker");
    assert!(item.is_some(), "should find type hierarchy item for actor");
    let item = item.unwrap();
    assert_eq!(item.name, "Worker");
    assert_eq!(item.kind, SymbolKind::CLASS);
}

#[test]
fn type_hierarchy_item_for_trait() {
    let source = "trait Drawable { fn draw() -> i32; }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let item = find_type_hierarchy_item(&uri, source, &lo, &parse_result, "Drawable");
    assert!(item.is_some(), "should find type hierarchy item for trait");
    let item = item.unwrap();
    assert_eq!(item.name, "Drawable");
    assert_eq!(item.kind, SymbolKind::INTERFACE);
}

#[test]
fn type_hierarchy_item_not_found() {
    let source = "fn main() { 0 }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let item = find_type_hierarchy_item(&uri, source, &lo, &parse_result, "Nonexistent");
    assert!(item.is_none(), "should not find non-existent type");
}

#[test]
fn subtypes_via_impl_for() {
    let source = "trait Drawable {\n    fn draw() -> i32;\n}\n\ntype Circle {\n    r: i32;\n}\n\nimpl Drawable for Circle {\n    fn draw() -> i32 {\n        0\n    }\n}\n";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let subs = collect_subtypes(&uri, "Drawable", source, &lo, &parse_result);
    assert_eq!(subs.len(), 1, "expected 1 subtype, got {}", subs.len());
    assert_eq!(subs[0].name, "Circle");
}

#[test]
fn supertypes_via_impl_for() {
    let source = "trait Drawable {\n    fn draw() -> i32;\n}\n\ntype Circle {\n    r: i32;\n}\n\nimpl Drawable for Circle {\n    fn draw() -> i32 {\n        0\n    }\n}\n";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let supers = collect_supertypes(&uri, "Circle", source, &lo, &parse_result);
    assert_eq!(
        supers.len(),
        1,
        "expected 1 supertype, got {}",
        supers.len()
    );
    assert_eq!(supers[0].name, "Drawable");
}

#[test]
fn capabilities_advertise_type_hierarchy() {
    let caps = build_server_capabilities();
    let experimental = caps
        .experimental
        .expect("type-hierarchy capability must be present in experimental");
    let advertised = experimental
        .get("typeHierarchyProvider")
        .and_then(serde_json::Value::as_bool)
        .unwrap_or(false);
    assert!(
        advertised,
        "expected experimental.typeHierarchyProvider = true, got: {experimental}"
    );
}

// ── Call hierarchy tests ────────────────────────────────────────

#[test]
fn find_callable_at_function() {
    let source = "fn helper() -> i32 { 42 }\nfn main() { helper() }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let item = find_callable_at(&uri, source, &lo, &parse_result, "helper");
    assert!(item.is_some(), "should find callable for function");
    let item = item.unwrap();
    assert_eq!(item.name, "helper");
    assert_eq!(item.kind, SymbolKind::FUNCTION);
}

#[test]
fn find_callable_at_receive_fn() {
    let source = "actor Counter { receive fn increment(n: i32) { n } }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let item = find_callable_at(&uri, source, &lo, &parse_result, "increment");
    assert!(item.is_some(), "should find callable for receive fn");
    let item = item.unwrap();
    assert_eq!(item.name, "increment");
    assert_eq!(item.kind, SymbolKind::METHOD);
    assert_eq!(item.detail.as_deref(), Some("actor Counter"));
}

#[test]
fn find_callable_at_unknown_returns_none() {
    let source = "fn main() { 0 }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let item = find_callable_at(&uri, source, &lo, &parse_result, "unknown");
    assert!(item.is_none());
}

#[test]
fn incoming_calls_finds_callers() {
    let source = "fn helper() -> i32 { 42 }\nfn main() -> i32 { let r = helper(); r }";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let calls = find_incoming_calls(&uri, source, &lo, &parse_result, "helper", None, None);
    assert_eq!(
        calls.len(),
        1,
        "expected 1 incoming call, got {}",
        calls.len()
    );
    assert_eq!(calls[0].from.name, "main");
}

#[test]
fn outgoing_calls_finds_callees() {
    let source = "fn helper() -> i32 { 42 }\nfn main() -> i32 { let r = helper(); r }";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let calls = find_outgoing_calls(&uri, source, &lo, &parse_result, "main", None, None);
    assert_eq!(
        calls.len(),
        1,
        "expected 1 outgoing call, got {}",
        calls.len()
    );
    assert_eq!(calls[0].to.name, "helper");
}

#[test]
fn outgoing_calls_empty_for_leaf_function() {
    let source = "fn leaf() -> i32 { 42 }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let calls = find_outgoing_calls(&uri, source, &lo, &parse_result, "leaf", None, None);
    assert!(
        calls.is_empty(),
        "leaf function should have no outgoing calls"
    );
}

/// Regression: sibling `receive_fns` in an actor must not bleed into each
/// other's outgoing call sets.  Before the fix, querying `on_msg` would
/// walk *all* actor bodies and surface `target_b` (from `on_other`) as an
/// outgoing call.
#[test]
fn outgoing_calls_actor_receive_fn_no_sibling_bleed() {
    let source = concat!(
        "fn target_a() {}\n",
        "fn target_b() {}\n",
        "actor A {\n",
        "    receive fn on_msg() { target_a(); }\n",
        "    receive fn on_other() { target_b(); }\n",
        "}",
    );
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let calls = find_outgoing_calls(&uri, source, &lo, &parse_result, "on_msg", None, None);
    let callee_names: Vec<&str> = calls.iter().map(|c| c.to.name.as_str()).collect();
    assert!(
        callee_names.contains(&"target_a"),
        "on_msg should have target_a as outgoing call"
    );
    assert!(
        !callee_names.contains(&"target_b"),
        "sibling on_other's target_b must NOT appear in on_msg outgoing calls (regression)"
    );
}

/// Regression: sibling methods in an impl must not bleed into each other's
/// outgoing call sets.
#[test]
fn outgoing_calls_impl_method_no_sibling_bleed() {
    let source = concat!(
        "fn alpha_target() {}\n",
        "fn beta_target() {}\n",
        "impl Foo {\n",
        "    fn alpha() { alpha_target(); }\n",
        "    fn beta() { beta_target(); }\n",
        "}",
    );
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let calls = find_outgoing_calls(&uri, source, &lo, &parse_result, "alpha", None, None);
    let callee_names: Vec<&str> = calls.iter().map(|c| c.to.name.as_str()).collect();
    assert!(
        callee_names.contains(&"alpha_target"),
        "alpha should have alpha_target as outgoing call"
    );
    assert!(
        !callee_names.contains(&"beta_target"),
        "sibling beta's beta_target must NOT appear in alpha outgoing calls (regression)"
    );
}
