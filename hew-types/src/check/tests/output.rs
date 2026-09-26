#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn source_resolutions_join_local_definition_and_use() {
    let source = "fn main() { let value: i64 = 1; let next = value; }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let definition = source.find("value:").expect("binding definition");
    let use_site = source.rfind("value;").expect("binding use");
    let at = |start| SpanKey::in_module(&(start..start + "value".len()), 0);
    let declared = output.resolutions.get(&at(definition));
    assert!(matches!(
        declared,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    assert_eq!(output.resolutions.get(&at(use_site)), declared);
}

#[test]
fn source_resolutions_join_var_statement_and_use() {
    let source = "fn main() { var x: i64 = 1; x = 2; println(x); }";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Function(main), _) = &parsed.program.items[0] else {
        panic!("expected main function");
    };
    let declaration = &main.body.stmts[0].1;
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let use_site = source.rfind("x)").unwrap();
    let binding = output.resolutions.get(&SpanKey::in_module(declaration, 0));
    assert!(matches!(
        binding,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    let name = source.find("var x").unwrap() + "var ".len();
    assert_eq!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(name..name + 1), 0)),
        binding
    );
    assert_eq!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(use_site..use_site + 1), 0)),
        binding
    );
}

#[test]
fn source_resolutions_join_function_and_closure_parameters_to_uses() {
    let source = "fn identity(value: i64) -> i64 { value } \
        fn main() { let f = |n: i64| -> i64 { n + 1 }; println(f(identity(2))); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    for (name, definition, use_site) in [
        (
            "value",
            source.find("value:").unwrap(),
            source.find("value }").unwrap(),
        ),
        (
            "n",
            source.find("n: i64").unwrap(),
            source.find("n +").unwrap(),
        ),
    ] {
        let at = |start| SpanKey::in_module(&(start..start + name.len()), 0);
        let declared = output.resolutions.get(&at(definition));
        assert!(matches!(
            declared,
            Some(crate::check::scope::Resolution::Local(_))
        ));
        assert_eq!(
            output.resolutions.get(&at(use_site)),
            declared,
            "{name}: {:?}",
            output.resolutions
        );
    }
}

#[test]
fn source_resolutions_join_implicit_self_to_its_use() {
    let source = "type Box { value: i64 } \
        impl Box { fn get(self) -> i64 { self.value } }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let definition = source.find("self)").unwrap();
    let use_site = source.find("self.value").unwrap();
    let at = |start| SpanKey::in_module(&(start..start + "self".len()), 0);
    let declared = output.resolutions.get(&at(definition));
    assert!(matches!(
        declared,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    assert_eq!(output.resolutions.get(&at(use_site)), declared);
}

#[test]
fn source_resolutions_distinguish_same_named_fields_by_owner() {
    let source = "type A { x: i64 } type B { x: i64 } fn main() { \
        let a = A { x: 1 }; let b = B { x: 2 }; \
        println(a.x); println(b.x); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let a = source.find("a.x").expect("A field") + 2;
    let b = source.find("b.x").expect("B field") + 2;
    let a_label = source.find("let a = A { x:").expect("A initializer") + "let a = A { ".len();
    let b_label = source.find("let b = B { x:").expect("B initializer") + "let b = B { ".len();
    let at = |start| SpanKey::in_module(&(start..start + 1), 0);
    let a_field = output.resolutions.get(&at(a));
    let b_field = output.resolutions.get(&at(b));
    assert!(matches!(
        a_field,
        Some(crate::check::scope::Resolution::Field(_, 0))
    ));
    assert!(matches!(
        b_field,
        Some(crate::check::scope::Resolution::Field(_, 0))
    ));
    assert_ne!(a_field, b_field);
    assert_eq!(output.resolutions.get(&at(a_label)), a_field);
    assert_eq!(output.resolutions.get(&at(b_label)), b_field);
}

#[test]
fn source_resolutions_publish_qualified_record_constructor_segments() {
    let source = "import ma; fn main() { let shape = ma.Shape { x: 1 }; println(shape.x); }";
    let module = hew_parser::parse("pub type Shape { x: i64 }");
    assert!(module.errors.is_empty(), "{:#?}", module.errors);
    let mut parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Import(import), _) = &mut parsed.program.items[0] else {
        panic!("expected module import");
    };
    import.resolved_items = Some(module.program.items.into());
    import.resolved_source_paths = vec![std::path::PathBuf::from("ma.hew")];
    let root = ModulePath::root();
    let ma = ModulePath::new(["ma"]);
    let mut graph = ModuleGraph::new(root.clone());
    graph
        .add_module(Module {
            id: root.clone(),
            items: Vec::new(),
            imports: Vec::new(),
            source_paths: vec![std::path::PathBuf::from("main.hew")],
            doc: None,
        })
        .unwrap();
    graph
        .add_module(Module {
            id: ma.clone(),
            items: import.resolved_items.as_ref().unwrap().as_ref().clone(),
            imports: Vec::new(),
            source_paths: import.resolved_source_paths.clone(),
            doc: None,
        })
        .unwrap();
    graph.topo_order = vec![ma, root];
    parsed.program.module_graph = Some(graph);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let start = source.find("ma.Shape {").unwrap();
    let at = |offset, len| SpanKey::in_module(&(offset..offset + len), 0);
    assert!(matches!(
        output.resolutions.get(&at(start, 2)),
        Some(crate::check::scope::Resolution::Module(_))
    ));
    assert!(matches!(
        output.resolutions.get(&at(start + 3, 5)),
        Some(crate::check::scope::Resolution::Nominal(_))
    ));
}

#[test]
fn source_resolutions_join_actor_field_uses_across_handlers() {
    let source = "actor Counter { let count: i64, \
        receive fn get() -> i64 { count } \
        receive fn next() -> i64 { count + 1 } }";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Actor(actor), _) = &parsed.program.items[0] else {
        panic!("expected actor");
    };
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let declaration = output
        .resolutions
        .get(&SpanKey::in_module(&actor.fields[0].span, 0));
    assert!(matches!(
        declaration,
        Some(crate::check::scope::Resolution::Field(_, 0))
    ));
    for written in [
        source.find("{ count }").unwrap() + 2,
        source.find("{ count +").unwrap() + 2,
    ] {
        assert_eq!(
            output
                .resolutions
                .get(&SpanKey::in_module(&(written..written + "count".len()), 0)),
            declaration
        );
    }
}

#[test]
fn source_resolutions_publish_selected_function_and_method() {
    let source = "type A { x: i64 } \
        impl A { fn get(self) -> i64 { self.x } } \
        fn helper() -> i64 { 1 } \
        fn main() { let a = A { x: 2 }; println(helper()); println(a.get()); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let helper = source.rfind("helper()").expect("helper call");
    let get = source.rfind("get()").expect("method call");
    let at = |start, len| SpanKey::in_module(&(start..start + len), 0);
    let Some(crate::check::scope::Resolution::Def(function)) =
        output.resolutions.get(&at(helper, "helper".len()))
    else {
        panic!(
            "function call has no declaration resolution: {:?}",
            output.resolutions
        );
    };
    assert_eq!(output.defs.name(*function).as_str(), "helper");
    let Some(crate::check::scope::Resolution::Member(method)) =
        output.resolutions.get(&at(get, "get".len()))
    else {
        panic!(
            "method call has no declaration resolution: {:?}",
            output.resolutions
        );
    };
    assert_eq!(output.defs.name(*method).as_str(), "get");
}

#[test]
fn checker_output_contract_intersects_assignment_target_side_tables() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.assign_target_kinds.insert(
        SpanKey {
            start: 1,
            end: 2,
            module_idx: 0,
        },
        AssignTargetKind::LocalVar,
    );
    checker.assign_target_shapes.insert(
        SpanKey {
            start: 3,
            end: 4,
            module_idx: 0,
        },
        AssignTargetShape { is_unsigned: false },
    );

    let mut expr_types = HashMap::new();
    let mut type_defs = HashMap::new();
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    assert!(
        checker.assign_target_kinds.is_empty(),
        "orphan assign_target_kinds entries should be pruned at the output boundary: {:?}",
        checker.assign_target_kinds
    );
    assert!(
        checker.assign_target_shapes.is_empty(),
        "orphan assign_target_shapes entries should be pruned at the output boundary: {:?}",
        checker.assign_target_shapes
    );
}

#[test]
fn expr_output_contract_rechecks_normalized_unresolved_subset() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let sender_var = TypeVar::fresh();
    let covered_var = TypeVar::fresh();
    let span = SpanKey {
        start: 10,
        end: 20,
        module_idx: 0,
    };
    let mut expr_types = HashMap::from([(
        span.clone(),
        Ty::Tuple(vec![
            Ty::named_for_test("Sender", vec![Ty::Var(sender_var)]),
            Ty::Var(covered_var),
        ]),
    )]);

    // Channel endpoints carry ordinary type parameters now, so a var inside
    // `Sender<?a>` is an ordinary unresolved var: it is covered or it is a leak.
    // The uncovered case is the sibling negative control
    // (`validate_expr_output_contract_reports_and_prunes_ty_var_leak`).
    checker
        .validate_expr_output_contract(&mut expr_types, &HashSet::from([covered_var, sender_var]));

    assert!(
        checker
            .errors
            .iter()
            .all(|error| error.kind != TypeErrorKind::InferenceFailed),
        "normalized covered vars must not emit InferenceFailed: {checker_errors:#?}",
        checker_errors = checker.errors
    );
    assert!(
        !expr_types.contains_key(&span),
        "covered unresolved expr types should still be pruned after normalization: {expr_types:?}"
    );
}

// ── method-call output-contract validation ───────────────────────────────────

/// Valid method-call metadata must survive the output-contract boundary when
/// the corresponding `expr_types` entry is present and fully resolved.
#[test]
fn checker_output_contract_retains_valid_method_call_metadata() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let span = SpanKey {
        start: 10,
        end: 20,
        module_idx: 0,
    };
    checker.method_call_receiver_kinds.insert(
        span.clone(),
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Foo".to_string(),
        },
    );
    checker
        .method_call_rewrites
        .insert(span.clone(), MethodCallRewrite::DeferToLowering);

    // expr_types has the matching span with a concrete, fully-resolved type.
    let mut expr_types = HashMap::new();
    expr_types.insert(span.clone(), Ty::I64);
    // type_defs must include "Foo" so validate_method_call_receiver_kinds_output_contract
    // retains the NamedTypeInstance entry after validate_method_call_output_contract passes it.
    let mut type_defs = HashMap::from([(
        crate::NominalId::from_minted_declaration(checker.defs.mint_for_test("Foo")),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Foo".to_string(),
            type_params: vec![],
            bounds: HashMap::new(),
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    )]);
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    assert!(
        checker.method_call_receiver_kinds.contains_key(&span),
        "valid method_call_receiver_kinds entry must be retained: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        checker.method_call_rewrites.contains_key(&span),
        "valid method_call_rewrites entry must be retained: {:?}",
        checker.method_call_rewrites
    );
}

/// Orphaned method-call metadata — where the corresponding `expr_types` span
/// was pruned — must be removed at the output-contract boundary.
#[test]
fn checker_output_contract_prunes_orphaned_method_call_metadata() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Insert metadata keyed to spans that have NO corresponding expr_types entry.
    checker.method_call_receiver_kinds.insert(
        SpanKey {
            start: 10,
            end: 20,
            module_idx: 0,
        },
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Bar".to_string(),
        },
    );
    checker.method_call_rewrites.insert(
        SpanKey {
            start: 30,
            end: 40,
            module_idx: 0,
        },
        MethodCallRewrite::RewriteToFunction {
            target: CallTarget::Unsupported {
                reason: "orphaned test metadata".to_string(),
            },
            c_symbol: "hew_bar_method".to_string(),
            descriptor: None,
            extern_identity: None,
            consumes_receiver: false,
            requires_mutable_receiver: false,
            receiver_update: crate::ReceiverUpdate::Replace,
            returns_receiver_identity: false,
        },
    );

    // expr_types is empty — no span survives.
    let mut expr_types = HashMap::new();
    let mut type_defs = HashMap::new();
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    assert!(
        checker.method_call_receiver_kinds.is_empty(),
        "orphan method_call_receiver_kinds entries must be pruned: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        checker.method_call_rewrites.is_empty(),
        "orphan method_call_rewrites entries must be pruned: {:?}",
        checker.method_call_rewrites
    );
}

/// When a method-call expression's `expr_types` entry is pruned because it
/// carries an unresolved inference variable (simulating a failed / error-typed
/// receiver), the corresponding receiver-kind and rewrite side-table entries
/// must not leak to the output.
#[test]
fn checker_output_contract_prunes_method_call_metadata_for_leaked_inference_var_expr() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let leaked_span = SpanKey {
        start: 50,
        end: 60,
        module_idx: 0,
    };
    let good_span = SpanKey {
        start: 70,
        end: 80,
        module_idx: 0,
    };

    // The leaked span has an unresolved inference var — validate_expr_output_contract
    // will strip it from expr_types, so the method-call metadata must follow.
    checker.method_call_receiver_kinds.insert(
        leaked_span.clone(),
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Bad".to_string(),
        },
    );
    checker
        .method_call_rewrites
        .insert(leaked_span.clone(), MethodCallRewrite::DeferToLowering);
    // The good span carries a fully-resolved type and its metadata should survive.
    checker.method_call_receiver_kinds.insert(
        good_span.clone(),
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Good".to_string(),
        },
    );
    checker
        .method_call_rewrites
        .insert(good_span.clone(), MethodCallRewrite::DeferToLowering);

    // Build expr_types: leaked entry has a fresh (unresolved) inference var;
    // good entry carries a concrete type.
    let mut expr_types = HashMap::new();
    expr_types.insert(leaked_span.clone(), Ty::Var(TypeVar::fresh()));
    expr_types.insert(good_span.clone(), Ty::Bool);

    // type_defs must include "Good" so validate_method_call_receiver_kinds_output_contract
    // retains the NamedTypeInstance entry for the good span after the span-based pruner passes it.
    let mut type_defs = HashMap::from([(
        crate::NominalId::from_minted_declaration(checker.defs.mint_for_test("Good")),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Good".to_string(),
            type_params: vec![],
            bounds: HashMap::new(),
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    )]);
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    // The leaked span must have been pruned from expr_types by
    // validate_expr_output_contract, which in turn must cascade to prune the
    // orphaned method-call metadata.
    assert!(
        !expr_types.contains_key(&leaked_span),
        "leaked inference-var expr must be pruned from expr_types"
    );
    assert!(
        !checker
            .method_call_receiver_kinds
            .contains_key(&leaked_span),
        "method_call_receiver_kinds entry for pruned expr must not survive: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        !checker.method_call_rewrites.contains_key(&leaked_span),
        "method_call_rewrites entry for pruned expr must not survive: {:?}",
        checker.method_call_rewrites
    );

    // The good span must be retained in all three maps.
    assert!(
        expr_types.contains_key(&good_span),
        "fully-resolved expr must be retained in expr_types"
    );
    assert!(
        checker.method_call_receiver_kinds.contains_key(&good_span),
        "method_call_receiver_kinds entry for valid expr must survive: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        checker.method_call_rewrites.contains_key(&good_span),
        "method_call_rewrites entry for valid expr must survive: {:?}",
        checker.method_call_rewrites
    );
}

#[test]
fn module_qualified_call_rewrites_record_owning_module_endpoint() {
    let parsed = hew_parser::parse(
        r#"
import std.fs;

fn main() {
    let _ = fs.exists("test.txt");
}
"#,
    );
    assert!(
        parsed.errors.is_empty(),
        "expected clean parse, got: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteModuleQualifiedToFunction { c_symbol, .. }
                if c_symbol == "std.fs.exists"
        )),
        "expected the module-qualified rewrite to name the owning module endpoint, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn module_qualified_pure_hew_stdlib_wrapper_rewrites_to_qualified_symbol() {
    let parsed = hew_parser::parse(
        r#"
import std.path;

fn main() {
    let _ = path.dirname("a/b");
}
"#,
    );
    assert!(
        parsed.errors.is_empty(),
        "expected clean parse, got: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteModuleQualifiedToFunction { c_symbol, .. }
                if c_symbol == "std.path.dirname"
        )),
        "expected pure-Hew stdlib wrapper to rewrite to module-qualified symbol, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn tail_ok_publication_preserves_the_source_payload_type() {
    let source = "fn wrap(value: i64) -> Result<i64, string> {\n    value\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let start = source.rfind("value\n").expect("tail identifier");
    let key = output
        .tail_ok_coercions
        .iter()
        .find(|key| key.start == start && key.module_idx == 0)
        .expect("tail identifier must carry the Ok-coercion marker");
    assert_eq!(output.expr_types.get(key), Some(&Ty::I64));
}

#[test]
fn scope_body_with_spawned_call_and_trailing_value_checks_cleanly() {
    let output = check_source(
        r"
        actor Worker {
            receive fn run() {}
        }

        fn main() {
            scope {
                let worker = spawn Worker();
                let _ = worker.run();
                0
            };
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "scope body with a spawned worker and trailing value must typecheck cleanly; got: {:#?}",
        output.errors
    );
}

// Helper functions for testing AST construction

#[test]
fn empty_select_and_match_preserve_source_diagnostics() {
    for (source, expects_error) in [
        // A select with no arms waits on nothing and is refused; an empty
        // match keeps reporting its scrutinee's own diagnostic.
        ("fn main() { let _ = select {}; }", true),
        ("fn main() { let _ = match missing() {}; }", true),
    ] {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&parsed.program);
        assert_eq!(
            !output.errors.is_empty(),
            expects_error,
            "{source}: {:#?}",
            output.errors
        );
    }
}

#[test]
fn expected_variant_type_reaches_nested_binding_blocks() {
    let parsed = hew_parser::parse("enum Value { Text { text: string } } fn main() { let value: Value = { { .Text { text: \"retained\" } } }; match value { .Text { text } => println(text), } }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}
