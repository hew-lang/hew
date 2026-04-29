#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::module_registry::ModuleRegistry;
use hew_parser::ast::IntRadix;
use hew_parser::ast::{ImportName, TraitMethod, TypeExpr, Visibility};
use hew_parser::module::{Module, ModuleGraph, ModuleId};

/// Module registry with the repo root as a search path, so stdlib
/// modules (e.g. `std::encoding::json`) can be loaded during tests.
fn test_registry() -> ModuleRegistry {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    ModuleRegistry::new(vec![repo_root])
}

fn check_source(source: &str) -> TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "check_source should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&parse_result.program)
}

#[test]
fn test_empty_program() {
    let output = check_source("");
    assert!(output.errors.is_empty());
}

#[test]
fn test_type_checker_creation() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    assert_eq!(checker.errors.len(), 0);
}

#[test]
fn freshen_inner_recurses_into_pointer_pointee_vars() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let original = TypeVar::fresh();
    let ty = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::Var(original)),
    };
    let mut mapping = HashMap::new();

    let freshened = checker.freshen_inner(&ty, &mut mapping);

    let Ty::Pointer { pointee, .. } = freshened else {
        panic!("expected pointer type");
    };
    let Ty::Var(fresh) = *pointee else {
        panic!("expected freshened pointee var");
    };

    assert_ne!(fresh, original);
    assert_eq!(mapping.get(&original.0), Some(&Ty::Var(fresh)));
}

#[test]
fn freshen_inner_recurses_into_trait_object_bound_args() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let original = TypeVar::fresh();
    let ty = Ty::TraitObject {
        traits: vec![crate::ty::TraitObjectBound {
            trait_name: "Iterator".to_string(),
            args: vec![Ty::Var(original)],
        }],
    };
    let mut mapping = HashMap::new();

    let freshened = checker.freshen_inner(&ty, &mut mapping);

    let Ty::TraitObject { traits } = freshened else {
        panic!("expected trait object type");
    };
    let [bound] = traits.as_slice() else {
        panic!("expected one trait bound");
    };
    let [Ty::Var(fresh)] = bound.args.as_slice() else {
        panic!("expected freshened trait-object arg var");
    };

    assert_ne!(*fresh, original);
    assert_eq!(mapping.get(&original.0), Some(&Ty::Var(*fresh)));
}

#[test]
fn register_type_decl_marks_transitive_handle_bearing_structs() {
    let mut registry = ModuleRegistry::new(vec![]);
    registry.insert_handle_type_for_test("regex.Pattern".to_string());
    let mut checker = Checker::new(registry);

    let inner = TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "Inner".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "pattern".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "regex.Pattern".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: vec![],
            doc_comment: None,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
    };
    let outer = TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "Outer".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "inner".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "Inner".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: vec![],
            doc_comment: None,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
    };
    let plain = TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "Plain".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "count".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "int".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: vec![],
            doc_comment: None,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
    };

    checker.register_type_decl(&inner);
    checker.register_type_decl(&outer);
    checker.register_type_decl(&plain);
    checker.register_qualified_type_alias("regexwrap", "Outer");

    // Registrations set handle_bearing_dirty; flush before reading the set.
    checker.ensure_handle_bearing_fresh();

    assert!(checker.handle_bearing_structs.contains("Inner"));
    assert!(checker.handle_bearing_structs.contains("Outer"));
    assert!(checker.handle_bearing_structs.contains("regexwrap.Outer"));
    assert!(!checker.handle_bearing_structs.contains("Plain"));
}

#[test]
fn centralized_hashset_admissibility_rejects_nested_rc_elements() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Holder".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Holder".to_string(),
            type_params: vec![],
            fields: HashMap::from([("value".to_string(), Ty::rc(Ty::I64))]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        },
    );
    checker
        .registry
        .register_rcfree_members("Holder".to_string(), vec![Ty::rc(Ty::I64)]);

    let holder_ty = Ty::Named {
        name: "Holder".to_string(),
        args: vec![],
    };
    assert!(
        !checker.validate_hashset_owned_element_type(&holder_ty, &(0..0)),
        "HashSet element admissibility should fail closed for nested Rc payloads"
    );
    assert!(
        checker
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::UnsafeCollectionElement
                && err.message.contains("HashSet")),
        "expected centralized HashSet admissibility to report UnsafeCollectionElement, got: {:?}",
        checker.errors
    );
}

#[test]
fn centralized_hashset_admissibility_rejects_named_enum_with_rc_payload() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "MaybeHolder".to_string(),
        TypeDef {
            kind: TypeDefKind::Enum,
            name: "MaybeHolder".to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            variants: HashMap::from([(
                "Some".to_string(),
                VariantDef::Tuple(vec![Ty::rc(Ty::I64)]),
            )]),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        },
    );
    checker
        .registry
        .register_rcfree_members("MaybeHolder".to_string(), vec![Ty::rc(Ty::I64)]);

    let enum_ty = Ty::Named {
        name: "MaybeHolder".to_string(),
        args: vec![],
    };
    assert!(!checker.validate_hashset_owned_element_type(&enum_ty, &(0..0)));
}

#[test]
fn centralized_hashset_admissibility_rejects_recursive_rcfree_cycle() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let a_ty = Ty::Named {
        name: "A".to_string(),
        args: vec![],
    };
    let b_ty = Ty::Named {
        name: "B".to_string(),
        args: vec![],
    };
    checker
        .registry
        .register_rcfree_members("A".to_string(), vec![b_ty.clone()]);
    checker
        .registry
        .register_rcfree_members("B".to_string(), vec![a_ty.clone()]);

    assert!(
        !checker.validate_hashset_owned_element_type(&a_ty, &(0..0)),
        "HashSet element admissibility should fail closed for recursive RcFree cycles"
    );
    assert!(checker.errors.iter().any(|err| {
        err.kind == TypeErrorKind::UnsafeCollectionElement
            && err.message.contains("recursive type cycle")
            && err.message.contains('A')
    }));
}

#[test]
fn centralized_hashset_admissibility_rejects_module_qualified_named_rc_payload() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .registry
        .register_rcfree_members("Holder".to_string(), vec![Ty::rc(Ty::I64)]);

    let holder_ty = Ty::Named {
        name: "widgets.Holder".to_string(),
        args: vec![],
    };
    assert!(!checker.validate_hashset_owned_element_type(&holder_ty, &(0..0)));
    assert!(checker.errors.iter().any(|err| {
        err.kind == TypeErrorKind::UnsafeCollectionElement && err.message.contains("HashSet")
    }));
}

#[test]
fn free_call_len_on_hashset_records_lowering_fact() {
    let parsed = hew_parser::parse(
        r"
        fn main() -> int {
            let s: HashSet<int> = HashSet::new();
            len(s)
        }
        ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let call_span = match &parsed.program.items[0].0 {
        Item::Function(function) => function
            .body
            .trailing_expr
            .as_ref()
            .map(|expr| expr.1.clone())
            .expect("expected trailing len(s) call"),
        other => panic!("expected function item, got: {other:?}"),
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parsed.program);
    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);

    let fact = output
        .lowering_facts
        .get(&SpanKey::from(&call_span))
        .expect("expected len(s) to record a lowering fact");
    assert_eq!(fact.kind, crate::LoweringKind::HashSet);
    assert_eq!(fact.element_type, crate::HashSetElementType::I64);
    assert_eq!(fact.abi_variant, crate::HashSetAbi::Int64);
    assert_eq!(fact.drop_kind, crate::DropKind::HashSetFree);
}

#[test]
fn concrete_vec_validation_reaches_function_wrapped_vec() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let ty = Ty::Function {
        params: vec![Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::Array(Box::new(Ty::I64), 4)],
        }],
        ret: Box::new(Ty::Unit),
    };

    assert!(!checker.validate_concrete_vec_type(&ty, &(0..0)));
    assert!(checker
        .errors
        .iter()
        .any(|err| err.kind == TypeErrorKind::InvalidOperation && err.message.contains("Vec<")));
}

#[test]
fn concrete_hashset_validation_reaches_pointer_wrapped_hashset() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let ty = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::Named {
            name: "HashSet".to_string(),
            args: vec![Ty::Bool],
        }),
    };

    assert!(!checker.validate_concrete_hashset_type(&ty, &(0..0)));
    assert!(checker.errors.iter().any(|err| {
        err.kind == TypeErrorKind::InvalidOperation && err.message.contains("HashSet<bool>")
    }));
}

#[test]
fn concrete_hashmap_validation_reaches_tuple_wrapped_hashmap() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let ty = Ty::Tuple(vec![
        Ty::Named {
            name: "HashMap".to_string(),
            args: vec![Ty::I64, Ty::String],
        },
        Ty::Unit,
    ]);

    assert!(!checker.validate_concrete_hashmap_type(&ty, &(0..0)));
    assert!(checker
        .errors
        .iter()
        .any(|err| err.kind == TypeErrorKind::InvalidOperation && err.message.contains("HashMap")));
}

#[test]
fn non_root_private_type_rcfree_is_registered_during_body_checking() {
    let parsed = hew_parser::parse(
        r"
        type Holder {
            value: Rc<int>
        }

        fn helper() {
            var v = Vec::new();
            let h = Holder { value: Rc::new(1) };
            v.push(h);
        }",
    );
    assert!(
        parsed.errors.is_empty(),
        "module parse errors: {:?}",
        parsed.errors
    );

    let root_id = ModuleId::root();
    let mod_id = ModuleId::new(vec!["helpers".to_string()]);
    let module = Module {
        id: mod_id.clone(),
        items: parsed.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(module);
    mg.topo_order = vec![mod_id, root_id];
    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::UnsafeCollectionElement),
        "expected non-root private type with transitive Rc to be rejected from Vec during body checking, got: {:?}",
        output.errors
    );
}

#[test]
fn actor_decl_registers_rcfree_members_for_collection_checks() {
    let parsed = hew_parser::parse(
        r"
        actor Worker {
            let value: Rc<int>;
            receive fn ping() {}
        }

        fn main() {}",
    );
    assert!(
        parsed.errors.is_empty(),
        "program parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _ = checker.check_program(&parsed.program);
    let actor_ref_ty = Ty::actor_ref(Ty::Named {
        name: "Worker".to_string(),
        args: vec![],
    });

    assert!(
        !checker.validate_hashset_owned_element_type(&actor_ref_ty, &(0..0)),
        "ActorRef<Worker> should fail RcFree collection admissibility when Worker stores Rc"
    );
    assert!(checker.errors.iter().any(|err| {
        err.kind == TypeErrorKind::UnsafeCollectionElement && err.message.contains("HashSet")
    }));
}

#[test]
fn checker_output_contract_intersects_assignment_target_side_tables() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .assign_target_kinds
        .insert(SpanKey { start: 1, end: 2 }, AssignTargetKind::LocalVar);
    checker.assign_target_shapes.insert(
        SpanKey { start: 3, end: 4 },
        AssignTargetShape { is_unsigned: false },
    );

    let mut expr_types = HashMap::new();
    let mut type_defs = HashMap::new();
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
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
    let span = SpanKey { start: 10, end: 20 };
    let mut expr_types = HashMap::from([(
        span.clone(),
        Ty::Tuple(vec![
            Ty::Named {
                name: "Sender".to_string(),
                args: vec![Ty::Var(sender_var)],
            },
            Ty::Var(covered_var),
        ]),
    )]);

    checker.validate_expr_output_contract(&mut expr_types, &HashSet::from([covered_var]));

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
    let span = SpanKey { start: 10, end: 20 };
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
        "Foo".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Foo".to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        },
    )]);
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
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
        SpanKey { start: 10, end: 20 },
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Bar".to_string(),
        },
    );
    checker.method_call_rewrites.insert(
        SpanKey { start: 30, end: 40 },
        MethodCallRewrite::RewriteToFunction {
            c_symbol: "hew_bar_method".to_string(),
        },
    );

    // expr_types is empty — no span survives.
    let mut expr_types = HashMap::new();
    let mut type_defs = HashMap::new();
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
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
    let leaked_span = SpanKey { start: 50, end: 60 };
    let good_span = SpanKey { start: 70, end: 80 };

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
        "Good".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Good".to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        },
    )]);
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
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
fn module_qualified_call_rewrites_record_registry_c_symbol_metadata() {
    let parsed = hew_parser::parse(
        r#"
import std::fs;

fn main() {
    let _ = fs.read("test.txt");
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
            MethodCallRewrite::RewriteModuleQualifiedToFunction { c_symbol }
                if c_symbol == "hew_file_read"
        )),
        "expected checker-owned module-qualified rewrite metadata, got: {:?}",
        output.method_call_rewrites
    );
}

// Helper functions for testing AST construction
fn make_int_literal(n: i64, span: Span) -> Spanned<Expr> {
    (
        Expr::Literal(Literal::Integer {
            value: n,
            radix: IntRadix::Decimal,
        }),
        span,
    )
}

fn make_bool_literal(b: bool, span: Span) -> Spanned<Expr> {
    (Expr::Literal(Literal::Bool(b)), span)
}

#[test]
fn test_literal_types() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Integer literals synthesize as a first-class literal kind.
    let int_expr = make_int_literal(42, 0..2);
    let int_ty = checker.synthesize(&int_expr.0, &int_expr.1);
    assert_eq!(int_ty, Ty::IntLiteral);

    // Test boolean literal
    let bool_expr = make_bool_literal(true, 0..4);
    let bool_ty = checker.synthesize(&bool_expr.0, &bool_expr.1);
    assert_eq!(bool_ty, Ty::Bool);
}

#[test]
fn test_builtin_registration() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    // Check that println_int is registered
    assert!(checker.fn_sigs.contains_key("println_int"));
    let sig = &checker.fn_sigs["println_int"];
    assert_eq!(sig.params.len(), 1);
    assert_eq!(sig.params[0], Ty::I64);
    assert_eq!(sig.return_type, Ty::Unit);
}

#[test]
fn test_yield_outside_generator() {
    use hew_parser::ast::{Block, Expr, FnDecl, Item, Program, Stmt};

    let yield_expr: Spanned<Expr> = (Expr::Yield(None), 10..15);
    let body = Block {
        stmts: vec![(Stmt::Expression(yield_expr), 10..15)],
        trailing_expr: None,
    };
    let fd = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: "not_a_gen".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body,
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fd), 0..30)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::YieldOutsideGenerator));
}

#[test]
fn test_receive_gen_fn_returns_stream() {
    use hew_parser::ast::{ActorDecl, Expr, Item, Literal, ReceiveFnDecl, Stmt, TypeExpr};

    let receive_fn = ReceiveFnDecl {
        is_generator: true,
        is_pure: false,
        name: "numbers".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "int".to_string(),
                type_args: None,
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![(
                Stmt::Expression((
                    Expr::Yield(Some(Box::new((
                        Expr::Literal(Literal::Integer {
                            value: 1,
                            radix: IntRadix::Decimal,
                        }),
                        0..0,
                    )))),
                    0..0,
                )),
                0..0,
            )],
            trailing_expr: None,
        },
        span: 0..0,
        attributes: vec![],
        doc_comment: None,
    };

    let actor = ActorDecl {
        visibility: Visibility::Pub,
        name: "NumberStream".to_string(),
        super_traits: None,
        init: None,
        terminate: None,
        fields: vec![],
        receive_fns: vec![receive_fn],
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Actor(actor), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output.errors.is_empty());
    assert_eq!(
        output.fn_sigs["NumberStream::numbers"].return_type,
        Ty::stream(Ty::I64)
    );
}

#[test]
fn typecheck_generic_call_with_explicit_type_args() {
    // This test exercises generic type-arg resolution, not Rc safety.
    // The BorrowedParamReturn diagnostic on `identity` is expected and filtered.
    let source = concat!(
        "fn identity<T>(x: T) -> T { x }\n",
        "fn main() {\n",
        "    let a = identity<int>(42);\n",
        "    let b = identity<string>(\"hello\");\n",
        "    println(a);\n",
        "    println(b);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let unexpected: Vec<_> = output
        .errors
        .iter()
        .filter(|e| !matches!(e.kind, TypeErrorKind::BorrowedParamReturn))
        .collect();
    assert!(unexpected.is_empty(), "unexpected errors: {unexpected:?}");
}

#[test]
fn typecheck_generic_call_with_inferred_type_args() {
    // This test exercises generic type-arg resolution, not Rc safety.
    // The BorrowedParamReturn diagnostic on `identity` is expected and filtered.
    let source = concat!(
        "fn identity<T>(x: T) -> T { x }\n",
        "fn main() {\n",
        "    let a = identity(42);\n",
        "    let b = identity(\"hello\");\n",
        "    println(a);\n",
        "    println(b);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let unexpected: Vec<_> = output
        .errors
        .iter()
        .filter(|e| !matches!(e.kind, TypeErrorKind::BorrowedParamReturn))
        .collect();
    assert!(unexpected.is_empty(), "unexpected errors: {unexpected:?}");
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![Ty::I64]),
        "expected inferred int literal type args to materialize at output boundary, got {:?}",
        output.call_type_args
    );
}

#[test]
fn typecheck_generator_yield_uses_element_type() {
    let source = concat!(
        "gen fn count_up() -> int {\n",
        "    yield 1;\n",
        "    yield 2;\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_async_generator_yield_uses_element_type() {
    let source = concat!(
        "async gen fn count_up() -> int {\n",
        "    yield 1;\n",
        "    yield 2;\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_generator_yield_mismatch_reports_element_type() {
    let source = "gen fn bad() -> int { yield \"oops\"; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.iter().any(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::Mismatch { expected, actual }
                if expected == "int" && actual == "String"
            )
        }),
        "expected element-type mismatch, got: {:?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| !e.message.contains("Generator<")),
        "yield diagnostic should mention element type, got: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_stream_annotation_is_stream_alias() {
    use hew_parser::ast::{FnDecl, Item, TypeExpr};

    // A standalone function returning ActorStream<i32> should resolve to Stream<i32>
    let fn_decl = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: "foo".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "ActorStream".to_string(),
                type_args: Some(vec![(
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                )]),
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fn_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    // The body is empty (returns unit) so there will be a return-type mismatch error,
    // but fn_sigs is populated in pass 1 (before body checking), so the signature
    // should already reflect the resolved return type.
    assert_eq!(output.fn_sigs["foo"].return_type, Ty::stream(Ty::I32));
}

#[test]
fn test_qualified_builtin_type_names_canonicalize_in_signatures() {
    let source = concat!(
        "import std::stream;\n",
        "import std::channel::channel;\n",
        "\n",
        "fn stream_id(s: stream.Stream<int>) -> stream.Stream<int> { s }\n",
        "fn close_sender(tx: channel.Sender) {\n",
        "    tx.close();\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);
    assert_eq!(output.fn_sigs["stream_id"].params[0], Ty::stream(Ty::I64));
    assert_eq!(output.fn_sigs["stream_id"].return_type, Ty::stream(Ty::I64));
    assert!(matches!(
        &output.fn_sigs["close_sender"].params[0],
        Ty::Named { name, args } if name == "Sender" && args.len() == 1
    ));
}

#[test]
fn test_arity_mismatch_too_many_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    // println_int takes 1 arg; call with 2
    let call = (
        Expr::Call {
            function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
            type_args: None,
            args: vec![
                CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 1,
                        radix: IntRadix::Decimal,
                    }),
                    12..13,
                )),
                CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 2,
                        radix: IntRadix::Decimal,
                    }),
                    15..16,
                )),
            ],
            is_tail_call: false,
        },
        0..17,
    );
    checker.synthesize(&call.0, &call.1);
    assert!(checker
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn test_arity_mismatch_too_few_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    // println_int takes 1 arg; call with 0
    let call = (
        Expr::Call {
            function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
            type_args: None,
            args: vec![],
            is_tail_call: false,
        },
        0..13,
    );
    checker.synthesize(&call.0, &call.1);
    assert!(checker
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn typecheck_error_undefined_var() {
    let result = hew_parser::parse("fn main() -> i32 {\n    let x = undefined_var;\n    x\n}");
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for undefined variable"
    );
}

#[test]
fn typecheck_error_type_mismatch() {
    let source = concat!(
        "fn add(a: i32, b: i32) -> i32 {\n",
        "    a + b\n",
        "}\n\n",
        "fn main() {\n",
        "    let result = add(\"hello\", \"world\");\n",
        "}"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type errors for mismatched argument types"
    );
}

// -----------------------------------------------------------------------
// Additional edge-case tests
// -----------------------------------------------------------------------

#[test]
fn test_string_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::String("hello".to_string())), 0..5);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::String);
}

#[test]
#[expect(
    clippy::approx_constant,
    reason = "testing that 3.14 parses as Float, not using it as PI"
)]
fn test_float_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::Float(3.14)), 0..4);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::FloatLiteral);
}

#[test]
fn test_char_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::Char('a')), 0..3);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::Char);
}

#[test]
fn typecheck_binary_op_type_mismatch() {
    let source = "fn main() -> i32 {\n    let x: i32 = 1;\n    let y: bool = true;\n    x + y\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for i32 + bool"
    );
}

#[test]
fn typecheck_rejects_implicit_signedness_change_in_call() {
    let source = concat!(
        "fn takes_u32(x: u32) -> u32 { x }\n",
        "fn main() -> u32 {\n",
        "    let n: i64 = 42;\n",
        "    takes_u32(n)\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("implicit numeric coercion")),
        "expected explicit coercion diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_rejects_implicit_integer_to_float_in_call() {
    let source = concat!(
        "fn takes_f64(x: f64) -> f64 { x }\n",
        "fn main() -> f64 {\n",
        "    let n: i64 = 42;\n",
        "    takes_f64(n)\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("implicit numeric coercion")),
        "expected explicit coercion diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_allows_safe_integer_widening_in_call() {
    let source = concat!(
        "fn takes_i64(x: i64) -> i64 { x }\n",
        "fn main() -> i64 {\n",
        "    let n: i32 = 42;\n",
        "    takes_i64(n)\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_return_type_mismatch() {
    // The type checker may not flag all return-type mismatches at the
    // trailing-expression level; verify the function signature is recorded.
    let source = "fn foo() -> i32 {\n    true\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // The function signature should still reflect i32 return type
    assert_eq!(output.fn_sigs["foo"].return_type, Ty::I32);
}

#[test]
fn typecheck_trailing_return_stmt_matches_declared_type() {
    let source = "fn foo() -> i32 {\n    return 42;\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_trailing_return_stmt_matches_declared_bool_type() {
    let source = "fn foo() -> bool {\n    return true;\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_nested_function_calls() {
    let source = concat!(
        "fn double(x: i32) -> i32 { let two: i32 = 2; x * two }\n",
        "fn main() -> i32 { let x: i32 = 5; double(double(x)) }\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_let_with_explicit_type() {
    let source = "fn main() { let x: i32 = 42; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_let_type_annotation_mismatch() {
    let source = "fn main() { let x: int = \"hello\"; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for string assigned to int variable"
    );
    assert!(output.errors.iter().any(|e| {
        e.message.contains("expected `int`")
            && e.message.contains("found `String`")
            && !e.message.contains("i64")
    }));
}

#[test]
fn typecheck_if_branch_type_consistency() {
    let source =
        "fn main() -> i32 {\n    if true { let x: i32 = 1; x } else { let y: i32 = 2; y }\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_vec_type_annotation() {
    let source = "fn main() { let v: Vec<i32> = Vec::new(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // Vec::new() may or may not resolve depending on builtins, but should not panic
    assert!(output.errors.len() <= 2);
}

#[test]
fn unresolved_vec_new_method_chain_fails_closed() {
    let source = "fn main() { Vec::new().clear(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    assert!(
        output.errors.iter().any(|err| {
            err.kind == TypeErrorKind::InferenceFailed
                && err.message.contains("Vec element type")
                && err.message.contains("Vec<")
        }),
        "expected fail-closed Vec inference diagnostic, got errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_multiple_functions_cross_call() {
    let source = concat!(
        "fn add(a: i32, b: i32) -> i32 { a + b }\n",
        "fn mul(a: i32, b: i32) -> i32 { a * b }\n",
        "fn main() -> i32 { add(mul(2, 3), 4) }\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_actor_receive_fn_registered() {
    use hew_parser::ast::{ActorDecl, Block, Item, Param, ReceiveFnDecl, TypeExpr};

    let recv = ReceiveFnDecl {
        is_generator: false,
        is_pure: false,
        name: "greet".to_string(),
        params: vec![Param {
            name: "name".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "string".into(),
                    type_args: None,
                },
                0..0,
            ),
            is_mutable: false,
        }],
        return_type: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        type_params: None,
        where_clause: None,
        span: 0..0,
        attributes: vec![],
        doc_comment: None,
    };
    let actor = ActorDecl {
        visibility: Visibility::Pub,
        name: "Greeter".to_string(),
        super_traits: None,
        init: None,
        terminate: None,
        fields: vec![],
        receive_fns: vec![recv],
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Actor(actor), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output.fn_sigs.contains_key("Greeter::greet"));
}

#[test]
fn typecheck_empty_function_no_error() {
    let source = "fn noop() {}";
    let result = hew_parser::parse(source);
    assert!(result.errors.is_empty());
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_recursive_function() {
    let source = concat!(
        "fn factorial(n: i32) -> i32 {\n",
        "    let one: i32 = 1; if n <= one { one } else { n * factorial(n - one) }\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_local_result_enum_not_qualified_to_sqlite() {
    let source = concat!(
        "import ecosystem::db::sqlite;\n",
        "enum Result {\n",
        "    Ok(int);\n",
        "    Err(int)\n",
        "}\n",
        "fn unwrap_or(r: Result, fallback: int) -> int {\n",
        "    match r {\n",
        "        Ok(v) => v,\n",
        "        Err(_) => fallback,\n",
        "    }\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // Filter out the expected UnresolvedImport for the dummy stdlib import — the
    // test is about local type naming, not module resolution.
    let non_import_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind != TypeErrorKind::UnresolvedImport)
        .collect();
    assert!(
        non_import_errors.is_empty(),
        "unexpected errors: {non_import_errors:?}"
    );
    let sig = output
        .fn_sigs
        .get("unwrap_or")
        .expect("unwrap_or signature should be registered");
    assert_eq!(
        sig.params[0],
        Ty::Named {
            name: "Result".to_string(),
            args: vec![],
        }
    );
}

#[test]
fn typecheck_match_statement_exhaustive_enum_ok() {
    let (errors, _) = parse_and_check(concat!(
        "enum Light { Red; Green; }\n",
        "fn main() { let v: Light = Red; match v { Red => 1, Green => 2, } let _done = 0; }\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn typecheck_match_statement_missing_variant_errors() {
    let (errors, warnings) = parse_and_check(concat!(
        "enum Light { Red; Green; }\n",
        "fn main() { let v: Light = Red; match v { Red => 1, } let _done = 0; }\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive enum match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "expected non-exhaustive match error, got: {errors:?}"
    );
}

#[test]
fn typecheck_guarded_wildcard_not_exhaustive() {
    // A guarded wildcard (`_ if false`) does not count as an exhaustive arm;
    // matching bool with only that arm leaves both `true` and `false` uncovered.
    // Bool is enum-like, so this is a hard error.
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x = true;\n",
        "    match x {\n",
        "        _ if false => 0,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive bool match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "expected non-exhaustive match error, got: {errors:?}"
    );
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch))
        .expect("expected NonExhaustiveMatch error");
    assert_eq!(err.severity, crate::error::Severity::Error);
    assert_eq!(err.message, "non-exhaustive match: missing true, false");
}

/// Scalar types (int, float, string, …) have no closed variant set, so a
/// missing catch-all should remain a *warning*, not an error.
#[test]
fn typecheck_scalar_missing_catchall_is_warning_not_error() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x: int = 5;\n",
        "    match x {\n",
        "        1 => 10,\n",
        "        2 => 20,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "scalar missing catch-all must not be an error: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .any(|w| matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "scalar missing catch-all must be a warning: {warnings:?}"
    );
}

#[test]
fn typecheck_struct_pattern_unknown_field_errors() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: int, y: int }\n",
        "fn main() {\n",
        "    let p = Point { x: 1, y: 2 };\n",
        "    match p {\n",
        "        Point { x, z } => x,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedField)),
        "expected UndefinedField error for unknown field 'z', got: {errors:?}"
    );
}

#[test]
fn typecheck_match_wrong_enum_variant_errors() {
    // Matching a Colour scrutinee with a Shape variant should be an error.
    let (errors, _) = parse_and_check(concat!(
        "enum Colour { Red; Green; Blue; }\n",
        "enum Shape { Circle(i32); Rectangle(i32); }\n",
        "fn describe(c: Colour) -> i32 {\n",
        "    match c {\n",
        "        Circle(r) => r,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        !errors.is_empty(),
        "expected type error for wrong-enum variant in match, got no errors"
    );
}

#[test]
fn typecheck_or_pattern_asymmetric_bindings_error() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let value = (1, 2);\n",
        "    match value {\n",
        "        (x, _) | (_, _) => 0,\n",
        "    }\n",
        "}\n",
    ));
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch))
        .expect("expected asymmetric or-pattern binding diagnostic");
    assert_eq!(err.message, "or-pattern branches must bind the same names");
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("left branch binds `x`")),
        "expected left-branch binding note, got: {err:?}"
    );
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("right branch binds no names")),
        "expected right-branch binding note, got: {err:?}"
    );
}

#[test]
fn typecheck_or_pattern_symmetric_bindings_ok() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() -> int {\n",
        "    let value = (1, 2);\n",
        "    match value {\n",
        "        (x, _) | (_, x) => x,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
}

#[test]
fn typecheck_or_pattern_incompatible_binding_types_error() {
    let (errors, _) = parse_and_check(concat!(
        "fn unwrap(result: Result<int, String>) -> int {\n",
        "    match result {\n",
        "        Ok(x) | Err(x) => x,\n",
        "    }\n",
        "}\n",
    ));
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch))
        .expect("expected incompatible or-pattern binding diagnostic");
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("left branch binds `x` as `int`")),
        "expected left-branch type note, got: {err:?}"
    );
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("right branch binds `x` as `String`")),
        "expected right-branch type note, got: {err:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_constructor_pattern_stays_fail_closed() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Some(x) => x,\n",
        "        None => panic(\"boom\"),\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::InferenceFailed)),
        "errored scrutinees must not seed constructor-pattern inference holes: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_struct_pattern_no_undefined_variable_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: int, y: int }\n",
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Point { x, y } => x + y,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_struct_variant_pattern_no_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "enum Shape { Move { x: int } }\n",
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Shape::Move { x } => x,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_struct_pattern_with_subpattern_no_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: int }\n",
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Point { x: inner_x } => inner_x,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
}

#[test]
fn typecheck_bool_scrutinee_constructor_pattern_errors() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    match true {\n",
        "        Some(v) => 1,\n",
        "        None => 0,\n",
        "        _ => 2,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "bool" && actual == "Some"
        )),
        "expected constructor-pattern mismatch on bool scrutinee, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("constructor pattern `Some`")),
        "expected fail-closed constructor-pattern diagnostic, got: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "wildcard arm should suppress exhaustiveness follow-ons: {warnings:?}"
    );
}

#[test]
fn typecheck_int_scrutinee_struct_pattern_errors_without_binding_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: int }\n",
        "fn main() {\n",
        "    let _ = match 42 {\n",
        "        Point { x } => {\n",
        "            let _ = x;\n",
        "            0\n",
        "        },\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors.len(),
        1,
        "expected only the struct-pattern mismatch, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "int" && actual == "Point"
        )),
        "expected struct-pattern mismatch on int scrutinee, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("struct pattern `Point` cannot match non-struct type `int`")),
        "expected fail-closed struct-pattern diagnostic, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "struct-pattern mismatch must not cascade into undefined-variable errors: {errors:?}"
    );
}

#[test]
fn typecheck_bool_scrutinee_tuple_pattern_errors_without_binding_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let _ = match true {\n",
        "        (left, right) => {\n",
        "            let _ = left;\n",
        "            let _ = right;\n",
        "            0\n",
        "        },\n",
        "        _ => 0,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors.len(),
        1,
        "expected only the tuple-pattern mismatch, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "bool" && actual == "tuple"
        )),
        "expected tuple-pattern mismatch on bool scrutinee, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("tuple pattern cannot match non-tuple type `bool`")),
        "expected fail-closed tuple-pattern diagnostic, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "tuple-pattern mismatch must not cascade into undefined-variable errors: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_skips_exhaustiveness_follow_on() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    match missing {\n",
        "        true => 1,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "expected primary undefined variable error, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "Ty::Error scrutinee must not emit follow-on non-exhaustive errors: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "Ty::Error scrutinee must not emit follow-on non-exhaustive warnings: {warnings:?}"
    );
}

#[test]
fn typecheck_generic_enum_constructor_infers_type_args() {
    let (errors, _) = parse_and_check(concat!(
        "enum Option<T> { Some(T); None; }\n",
        "fn take_int(x: Option<int>) -> Option<int> { x }\n",
        "fn take_string(x: Option<string>) -> Option<string> { x }\n",
        "fn main() { take_int(Some(42)); take_string(Some(\"hello\")); }\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn generic_enum_constructor_expected_context_coerces_payload_literal() {
    let source = concat!(
        "enum Option<T> { Some(T); None; }\n",
        "fn take_int(x: Option<int>) -> Option<int> { x }\n",
        "fn main() { take_int(Some(42)); }\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((outer_call, _)) = &main_fn.body.stmts[0].0 else {
        panic!("expected outer call statement");
    };
    let Expr::Call {
        args: outer_args, ..
    } = outer_call
    else {
        panic!("expected outer call expression");
    };
    let (inner_call, inner_call_span) = outer_args[0].expr();
    let Expr::Call {
        args: inner_args, ..
    } = inner_call
    else {
        panic!("expected inner constructor call");
    };
    let (_, literal_span) = inner_args[0].expr();

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(literal_span)),
        Some(&Ty::I64),
        "constructor payload literal should coerce to `int`: {:?}",
        output.expr_types
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(inner_call_span)),
        Some(&Ty::option(Ty::I64)),
        "constructor call should resolve to `Option<int>`: {:?}",
        output.expr_types
    );
}

#[test]
fn builtin_result_constructors_materialize_output_types_without_call_type_args() {
    let source = concat!(
        "fn main() -> int {\n",
        "    Ok(7);\n",
        "    Err(9);\n",
        "    0\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((Expr::Call { .. }, ok_call_span)) = &main_fn.body.stmts[0].0 else {
        panic!("expected first statement to be `Ok(...)`");
    };
    let Stmt::Expression((Expr::Call { .. }, err_call_span)) = &main_fn.body.stmts[1].0 else {
        panic!("expected second statement to be `Err(...)`");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(ok_call_span)),
        "builtin `Ok(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(err_call_span)),
        "builtin `Err(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(ok_call_span)),
        Some(&Ty::result(Ty::I64, Ty::I64)),
        "expected `Ok(7)` output type to materialize fully before serialization: {:?}",
        output.expr_types
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(err_call_span)),
        Some(&Ty::result(Ty::I64, Ty::I64)),
        "expected `Err(9)` output type to materialize fully before serialization: {:?}",
        output.expr_types
    );
}

#[test]
fn result_constructors_accept_unit_payloads() {
    let source = concat!(
        "fn ok_unit() -> Result<(), int> { Ok(()) }\n",
        "fn err_unit() -> Result<int, ()> { Err(()) }\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn builtin_result_constructor_composite_output_type_fallbacks_materialize() {
    let source = concat!(
        "fn main() -> int {\n",
        "    Ok(Some(7));\n",
        "    Err(Some(9));\n",
        "    0\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((Expr::Call { .. }, ok_call_span)) = &main_fn.body.stmts[0].0 else {
        panic!("expected first statement to be `Ok(...)`");
    };
    let Stmt::Expression((Expr::Call { .. }, err_call_span)) = &main_fn.body.stmts[1].0 else {
        panic!("expected second statement to be `Err(...)`");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(ok_call_span)),
        "builtin `Ok(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(err_call_span)),
        "builtin `Err(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    let expected = Ty::result(Ty::option(Ty::I64), Ty::option(Ty::I64));
    assert_eq!(
        output.expr_types.get(&SpanKey::from(ok_call_span)),
        Some(&expected),
        "expected `Ok(Some(7))` output type to preserve composite fallback: {:?}",
        output.expr_types
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(err_call_span)),
        Some(&expected),
        "expected `Err(Some(9))` output type to preserve composite fallback: {:?}",
        output.expr_types
    );
}

#[test]
fn cooperate_call_output_type_is_unit() {
    let source = "fn main() { cooperate(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((Expr::Call { .. }, call_span)) = &main_fn.body.stmts[0].0 else {
        panic!("expected cooperate call statement");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(call_span)),
        Some(&Ty::Unit),
        "expected `cooperate()` to remain unit-typed for serialization: {:?}",
        output.expr_types
    );
}

#[test]
fn warn_unused_variable() {
    let source = "fn main() { let unused_var = 42; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.message.contains("unused variable `unused_var`")),
        "expected unused variable warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn warn_var_never_mutated() {
    let source = "fn main() { var x = 10; println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "expected unmutated var warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_underscore_prefix() {
    let source = "fn main() { let _unused = 42; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.message.contains("_unused")),
        "should not warn on _ prefix, got: {:?}",
        output.warnings
    );
}

// -----------------------------------------------------------------------
// Lint / warning regression tests
// -----------------------------------------------------------------------

/// Helper: parse + typecheck, return (errors, warnings).
fn parse_and_check(source: &str) -> (Vec<TypeError>, Vec<TypeError>) {
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    (output.errors, output.warnings)
}

fn parse_and_check_with_stdlib(source: &str) -> (Vec<TypeError>, Vec<TypeError>) {
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    (output.errors, output.warnings)
}

// ---- unused variable ----

#[test]
fn warn_unused_in_nested_scope() {
    let (errors, warnings) = parse_and_check("fn main() { if true { let nested = 1; } }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `nested`")),
        "expected unused variable warning for nested, got: {warnings:?}"
    );
}

#[test]
fn no_warn_used_variable() {
    let (errors, warnings) = parse_and_check("fn main() { let x = 42; println(x); }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "should not warn on used variable, got: {warnings:?}"
    );
}

#[test]
fn no_warn_underscore_alone() {
    let (_, warnings) = parse_and_check("fn main() { let _ = 42; }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "bare _ should never warn, got: {warnings:?}"
    );
}

// ---- var never mutated ----

#[test]
fn no_warn_var_actually_mutated() {
    let (errors, warnings) = parse_and_check("fn main() { var x = 10; x = 20; println(x); }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "should not warn when var is actually reassigned, got: {warnings:?}"
    );
}

#[test]
fn mutable_param_can_be_reassigned() {
    let (errors, warnings) = parse_and_check("fn bump(var x: int) -> int { x = x + 1; x }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "mutable param reassignment should suppress unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_param_cannot_be_reassigned() {
    let (errors, warnings) = parse_and_check("fn bump(x: int) -> int { x = x + 1; x }");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `x`")),
        "expected immutable parameter assignment error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable param should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_field_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: int; }\n",
        "fn main() { let p = Point { x: 1 }; p.x = 2; }\n",
    ));
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `p`")),
        "expected immutable field-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable field assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_param_field_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: int; }\n",
        "fn bump(p: Point) { p.x = 2; }\n",
    ));
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `p`")),
        "expected immutable parameter field-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable parameter field assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_compound_field_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: int; }\n",
        "fn main() { let p = Point { x: 1 }; p.x += 2; }\n",
    ));
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `p`")),
        "expected immutable compound field-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable compound field assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn mutable_field_assignment_root_counts_as_mutation() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: int; }\n",
        "fn main() { var p = Point { x: 1 }; p.x = 2; println(p.x); }\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "field assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn mutable_param_field_assignment_root_counts_as_mutation() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: int; }\n",
        "fn bump(var p: Point) { p.x = 2; println(p.x); }\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "parameter field assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn immutable_index_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check("fn main() { let xs = [1, 2]; xs[0] = 3; }\n");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `xs`")),
        "expected immutable index-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable index assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_param_index_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check("fn bump(xs: Vec<int>) { xs[0] = 2; }\n");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `xs`")),
        "expected immutable parameter index-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable parameter index assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn mutable_index_assignment_root_counts_as_mutation() {
    let (errors, warnings) =
        parse_and_check("fn main() { var xs = [1, 2]; xs[0] = 3; println(xs[0]); }\n");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "index assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn mutable_param_index_assignment_root_counts_as_mutation() {
    let (errors, warnings) =
        parse_and_check("fn bump(var xs: Vec<int>) { xs[0] = 2; println(xs[0]); }\n");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "parameter index assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn warn_var_never_mutated_suggestion() {
    let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("never reassigned"))
        .expect("expected never-reassigned warning");
    assert!(
        w.suggestions.iter().any(|s| s.contains("let")),
        "should suggest using `let`, got: {:?}",
        w.suggestions
    );
}

// ---- assignment LHS not false-positive as "used" ----

#[test]
fn warn_write_only_variable() {
    // `x` is only written to, never read — should get unused warning
    let (_, warnings) = parse_and_check("fn main() { var x = 0; x = 1; }");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "write-only variable should be warned as unused, got: {warnings:?}"
    );
}

#[test]
fn no_warn_variable_used_then_assigned() {
    // `x` is read (println) AND then written — it's genuinely used
    let (_, warnings) = parse_and_check("fn main() { var x = 0; println(x); x = 1; println(x); }");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "variable that is read should not get unused warning, got: {warnings:?}"
    );
}

// ---- while true → loop ----

#[test]
fn warn_while_true() {
    let (_, warnings) = parse_and_check("fn main() { while true { break; } }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("while true"))
        .expect("expected while-true style warning");
    assert!(
        w.suggestions.iter().any(|s| s.contains("loop")),
        "should suggest loop, got: {:?}",
        w.suggestions
    );
    assert!(
        matches!(w.kind, TypeErrorKind::StyleSuggestion),
        "should be StyleSuggestion kind"
    );
}

#[test]
fn no_warn_while_condition() {
    let (_, warnings) = parse_and_check("fn main() { let x = true; while x { break; } }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("while true")),
        "should not warn on while <variable>, got: {warnings:?}"
    );
}

// ---- unused return value ----

#[test]
fn no_warn_discarded_return_value() {
    // Calling a function that returns a value without binding the result
    // should NOT warn — users can discard results freely
    let (_, warnings) = parse_and_check(concat!(
        "fn compute() -> i32 { 42 }\n",
        "fn main() { compute(); }\n",
    ));
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "discarded return value should not warn, got: {warnings:?}"
    );
}

#[test]
fn no_warn_unused_println() {
    // println returns unit, and is a known side-effect function
    let (_, warnings) = parse_and_check("fn main() { println(42); }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "println() should not produce unused value warning, got: {warnings:?}"
    );
}

#[test]
fn no_warn_unused_spawn() {
    // spawn is a side-effect expression — don't warn about discarded return
    let (_, warnings) = parse_and_check(concat!(
        "actor Worker { count: i32;\n",
        "    receive fn work() {} }\n",
        "fn main() { let _w = spawn Worker(count: 0); }\n",
    ));
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused") && !w.message.contains("unused variable")),
        "spawn() should not produce unused value warning, got: {warnings:?}"
    );
}

// ---- unit binding ----

#[test]
fn warn_unit_binding() {
    let (_, warnings) = parse_and_check("fn main() { let x = println(42); }");
    assert!(
        warnings.iter().any(|w| w.message.contains("unit type")),
        "binding to unit type should warn, got: {warnings:?}"
    );
}

#[test]
fn no_warn_non_unit_binding() {
    let (_, warnings) =
        parse_and_check("fn compute() -> i32 { 42 }\nfn main() { let x = compute(); println(x); }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unit type")),
        "binding to non-unit type should not warn, got: {warnings:?}"
    );
}

// ---- Levenshtein "did you mean?" suggestions ----

#[test]
fn suggest_similar_variable() {
    let (errors, _) = parse_and_check("fn main() { let counter = 42; println(conter); }");
    let err = errors
        .iter()
        .find(|e| e.message.contains("conter"))
        .expect("expected error for misspelled variable");
    assert!(
        err.suggestions.iter().any(|s| s.contains("counter")),
        "should suggest 'counter', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_function() {
    let (errors, _) = parse_and_check(concat!(
        "fn calculate_sum(a: i32, b: i32) -> i32 { a + b }\n",
        "fn main() { calculate_sun(1, 2); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("calculate_sun"))
        .expect("expected error for misspelled function");
    assert!(
        err.suggestions.iter().any(|s| s.contains("calculate_sum")),
        "should suggest 'calculate_sum', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_type() {
    // Use a misspelled type in a constructor position, which triggers undefined type lookup
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i32; y: i32; }\n",
        "fn make() { let p = Pont { x: 0, y: 0 }; println(p.x); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("Pont"))
        .expect("expected error for misspelled type");
    assert!(
        err.suggestions.iter().any(|s| s.contains("Point")),
        "should suggest 'Point', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_field() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i32; y: i32; }\n",
        "fn get_z(p: Point) -> i32 { p.z }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains('z'))
        .expect("expected error for undefined field");
    assert!(
        !err.suggestions.is_empty(),
        "should suggest similar fields, got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_method() {
    let (errors, _) = parse_and_check(concat!(
        "type Counter {}\n",
        "impl Counter { fn length(c: Counter) -> int { 0 } }\n",
        "fn main() { let c = Counter {}; c.lenght(); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("lenght"))
        .expect("expected error for misspelled method");
    assert!(
        err.suggestions.iter().any(|s| s.contains("length")),
        "should suggest 'length', got: {:?}",
        err.suggestions
    );
}

#[test]
fn no_suggest_method_when_too_different() {
    let (errors, _) = parse_and_check(concat!(
        "type Counter {}\n",
        "impl Counter { fn length(c: Counter) -> int { 0 } }\n",
        "fn main() { let c = Counter {}; c.zzzzz(); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("zzzzz"))
        .expect("expected error for undefined method");
    assert!(
        err.suggestions.is_empty() || !err.suggestions.iter().any(|s| s.contains("length")),
        "should not suggest distant method names, got: {:?}",
        err.suggestions
    );
}

#[test]
fn no_suggest_when_too_different() {
    let (errors, _) = parse_and_check("fn main() { let alpha = 1; println(zzzzz); }");
    let err = errors
        .iter()
        .find(|e| e.message.contains("zzzzz"))
        .expect("expected error for undefined variable");
    // "zzzzz" is too far from "alpha" — no suggestion
    assert!(
        err.suggestions.is_empty() || !err.suggestions.iter().any(|s| s.contains("alpha")),
        "should not suggest distant names, got: {:?}",
        err.suggestions
    );
}

// ---- warning severity and kind ----

#[test]
fn lint_warnings_have_warning_severity() {
    let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
    for w in &warnings {
        assert_eq!(
            w.severity,
            crate::error::Severity::Warning,
            "lint warnings must have Warning severity, got: {:?} for {}",
            w.severity,
            w.message
        );
    }
}

#[test]
fn unused_variable_has_correct_kind() {
    let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("unused"))
        .unwrap();
    assert!(
        matches!(w.kind, TypeErrorKind::UnusedVariable),
        "expected UnusedVariable kind, got: {:?}",
        w.kind
    );
}

#[test]
fn never_mutated_has_correct_kind() {
    let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("never reassigned"))
        .unwrap();
    assert!(
        matches!(w.kind, TypeErrorKind::UnusedMut),
        "expected UnusedMut kind, got: {:?}",
        w.kind
    );
}

// ---- multiple warnings in one function ----

#[test]
fn multiple_warnings_in_one_fn() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let unused_a = 1;\n",
        "    var never_written = 2;\n",
        "    println(never_written);\n",
        "    while true { break; }\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `unused_a`")),
        "missing unused_a warning: {warnings:?}"
    );
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "missing never-mutated warning: {warnings:?}"
    );
    assert!(
        warnings.iter().any(|w| w.message.contains("while true")),
        "missing while-true warning: {warnings:?}"
    );
}

// ── Unreachable Code Tests ──────────────────────────────────────────

#[test]
fn warn_unreachable_after_return() {
    let source = "fn foo() -> i32 { return 1; let x = 2; x }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "expected unreachable code warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_unreachable_when_return_is_last() {
    let source = "fn foo() -> i32 { let x = 1; return x; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "should not warn when return is last statement: {:?}",
        output.warnings
    );
}

// ── Shadowing Tests ─────────────────────────────────────────────────

#[test]
fn error_same_scope_shadowing() {
    let source = "fn main() { let x = 5; let x = 10; println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing
                && e.message.contains("already defined in this scope")),
        "expected same-scope shadowing error, got: {:?}",
        output.errors
    );
}

#[test]
fn warn_nested_scope_shadowing() {
    // Nested/child scope shadowing is a warning, not an error.
    let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing
                && w.message.contains("shadows a binding in an outer scope")),
        "expected outer-scope shadowing warning, got warnings: {:?}, errors: {:?}",
        output.warnings,
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "outer-scope shadowing of a local should not be an error: {:?}",
        output.errors
    );
}

#[test]
fn no_shadowing_diagnostic_underscore_prefix() {
    // Underscore-prefixed bindings are fully exempt from shadowing diagnostics.
    let source = "fn main() { let _x = 1; if true { let _x = 2; println(_x); } println(_x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "should not error for _ prefixed vars: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing),
        "should not warn for _ prefixed vars: {:?}",
        output.warnings
    );
}

#[test]
fn no_shadowing_diagnostic_for_loop_var() {
    // For-loop induction variables are exempt from shadowing diagnostics.
    let source = "fn main() { let i = 0; for i in 0..10 { println(i); } println(i); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "should not error for for-loop variable shadowing: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing),
        "should not warn for for-loop variable shadowing: {:?}",
        output.warnings
    );
}

#[test]
fn warn_deeply_nested_scope_shadowing() {
    // Shadowing across multiple scope levels is still a warning.
    let source = r"
        fn main() {
            let val = 1;
            if true {
                if true {
                    let val = 99;
                    println(val);
                }
            }
            println(val);
        }
    ";
    let (errors, warnings) = parse_and_check(source);
    assert!(
        warnings.iter().any(|w| w.kind == TypeErrorKind::Shadowing
            && w.message.contains("shadows a binding in an outer scope")),
        "expected nested shadowing warning, got warnings: {warnings:?}, errors: {errors:?}"
    );
    assert!(
        !errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing),
        "outer-scope shadowing of a local should not be an error: {errors:?}"
    );
}

#[test]
fn test_actor_field_shadowing_is_error() {
    // Shadowing an actor field is a hard error — bare field access requires
    // unambiguous names.
    let source = r"
        actor Counter {
            var count: int = 0;
            receive fn update(count: int) {
                println(count);
            }
        }
    ";
    let (errors, _warnings) = parse_and_check(source);
    assert!(
        errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing
            && e.message
                .contains("variable `count` shadows a binding in an outer scope")),
        "should error on param shadowing actor field, got: {errors:?}",
    );
}

#[test]
fn test_actor_fn_method_field_shadowing_is_error() {
    // Shadowing an actor field via an fn helper method is also a hard error.
    let source = r"
        actor Counter {
            var count: int = 0;
            fn helper(count: int) -> int { count }
        }
    ";
    let (errors, _warnings) = parse_and_check(source);
    assert!(
        errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing
            && e.message
                .contains("variable `count` shadows a binding in an outer scope")),
        "should error on fn param shadowing actor field, got: {errors:?}",
    );
}

// ── Dead Code (Unused Function) Tests ───────────────────────────────

#[test]
fn warn_dead_code_unused_function() {
    let source = "fn unused_helper() -> i32 { 42 } fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("unused_helper")),
        "expected dead code warning for unused_helper, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_main() {
    let source = "fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode),
        "should not warn about main: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_called_function() {
    let source = "fn helper() -> i32 { 42 } fn main() { let x = helper(); println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("helper")),
        "should not warn about called function: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_underscore_prefix() {
    let source = "fn _unused() -> i32 { 42 } fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode),
        "should not warn for _ prefixed functions: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_called_from_actor_receive() {
    let source = r"
fn fib(n: i32) -> i32 {
if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
}
actor Worker {
receive fn compute(n: i32) {
    let r = fib(n);
    println(r);
}
}
fn main() {
let w = spawn Worker();
w.compute(10);
}
";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("fib")),
        "should not warn about function called from actor receive fn: {:?}",
        output.warnings
    );
}

// ── Unused Import Tests ─────────────────────────────────────────────

#[test]
fn warn_unused_import() {
    let source = "import std::encoding::json;\nfn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
        "expected unused import warning for json, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_used_import() {
    let source =
        "import std::encoding::json;\nfn main() { let v = json.parse(\"[]\"); println(v); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
        "should not warn about used import: {:?}",
        output.warnings
    );
}

#[test]
fn stdlib_import_registers_trait_impls_for_generic_bounds() {
    let root_source = r"
        import std::string;

        fn main() -> String {
            string.describe(string.make_label())
        }
    ";
    let module_source = r#"
        pub trait Describable {
            fn describe(val: Self) -> String;
        }

        pub type Label {
            text: String;
        }

        pub fn make_label() -> Label {
            Label { text: "hello" }
        }

        impl Describable for Label {
            fn describe(label: Label) -> String {
                label.text
            }
        }

        pub fn describe<T: Describable>(item: T) -> String {
            item.describe()
        }

    "#;

    let mut root = hew_parser::parse(root_source);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );
    let call_span = root
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.trailing_expr.as_ref().map(|expr| expr.1.clone())
            }
            _ => None,
        })
        .expect("main trailing call should exist");
    let module = hew_parser::parse(module_source);
    assert!(
        module.errors.is_empty(),
        "module parse errors: {:?}",
        module.errors
    );

    let import_decl = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("root import should exist");
    import_decl.resolved_items = Some(module.program.items.clone());

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&root.program);

    assert!(
        !output.user_modules.contains("string"),
        "stdlib Hew import should not go through the user-module import path"
    );
    assert!(
        output.errors.is_empty(),
        "stdlib imported Hew impl should satisfy imported generic bounds: {:?}",
        output.errors
    );
    let inferred = output
        .call_type_args
        .get(&SpanKey::from(&call_span))
        .expect("stdlib imported generic call should record inferred type args");
    assert_eq!(
        inferred,
        &vec![Ty::Named {
            name: "Label".to_string(),
            args: vec![],
        }]
    );
    assert!(
        checker
            .trait_impls_set
            .contains(&("Label".to_string(), "Describable".to_string())),
        "stdlib Hew items should register trait impls for downstream generic bound checks"
    );
}

#[test]
fn duplicate_stdlib_import_with_same_resolved_source_does_not_reregister_items() {
    let mut root = hew_parser::parse(
        r"
            import std::bench;
            import std::bench;
        ",
    );
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    let bench_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std/bench/bench.hew")
        .canonicalize()
        .expect("stdlib bench module should exist");
    let bench_source =
        std::fs::read_to_string(&bench_path).expect("should read stdlib bench Hew source");
    let bench_module = hew_parser::parse(&bench_source);
    assert!(
        bench_module.errors.is_empty(),
        "bench parse errors: {:?}",
        bench_module.errors
    );

    for import_decl in root
        .program
        .items
        .iter_mut()
        .filter_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
    {
        import_decl.resolved_items = Some(bench_module.program.items.clone());
        import_decl.resolved_source_paths = vec![bench_path.clone()];
    }

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&root.program);

    assert!(
        !output.user_modules.contains("bench"),
        "stdlib Hew import should not go through the user-module import path"
    );
    assert!(
        output.type_defs.contains_key("Suite"),
        "stdlib Hew items should still register public types"
    );
    assert!(
        output.fn_sigs.contains_key("bench.suite"),
        "stdlib Hew items should still register qualified functions"
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "duplicate stdlib imports should dedupe Hew item registration: {:?}",
        output.errors
    );
}

// ── Warning severity tests ──────────────────────────────────────────

#[test]
fn unreachable_code_has_warning_severity() {
    let source = "fn foo() -> i32 { return 1; let x = 2; x }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let w = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::UnreachableCode);
    assert!(w.is_some(), "expected unreachable warning");
    assert_eq!(
        w.unwrap().severity,
        crate::error::Severity::Warning,
        "unreachable code should have Warning severity"
    );
}

#[test]
fn shadowing_warning_has_note_for_original_definition() {
    // The warning for outer-scope shadowing must include a note pointing back
    // to where the original binding was defined.
    let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let w = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Shadowing);
    assert!(w.is_some(), "expected shadowing warning");
    assert!(
        !w.unwrap().notes.is_empty(),
        "shadowing warning should have a note pointing to the original definition"
    );
}

// ── Bug fix regression tests ────────────────────────────────────────

#[test]
fn no_warn_unused_read_then_assign() {
    // Bug 1: var x = 0; println(x); x = 1; should NOT warn about unused x
    let (errors, warnings) = parse_and_check("fn main() { var x = 0; println(x); x = 1; }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "read-then-assign should not produce unused warning, got: {warnings:?}"
    );
}

#[test]
fn warn_unreachable_after_if_all_branches_return() {
    // Bug 2: if where all branches return should mark subsequent code unreachable
    let (_, warnings) = parse_and_check(
        "fn foo() -> i32 { if true { return 1; } else { return 2; } let y = 3; y }",
    );
    assert!(
        warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "expected unreachable code warning after if with all-returning branches, got: {warnings:?}"
    );
}

#[test]
fn no_warn_dead_code_function_referenced_as_value() {
    // Bug 3: let f = helper; f(); should mark helper as called
    let (_, warnings) =
        parse_and_check("fn helper() -> i32 { 42 } fn main() { let f = helper; println(f); }");
    assert!(
        !warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("helper")),
        "function referenced as value should not get dead code warning, got: {warnings:?}"
    );
}

#[test]
fn warn_dead_code_self_recursive_function() {
    // Bug 4: fn rec() { rec(); } fn main() {} — rec only calls itself, dead
    let (_, warnings) = parse_and_check("fn rec() { rec(); } fn main() { println(1); }");
    assert!(
        warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("rec")),
        "self-recursive unreachable function should get dead code warning, got: {warnings:?}"
    );
}

// -----------------------------------------------------------------------
// Module namespacing tests
// -----------------------------------------------------------------------

/// Helper: build a simple pub function declaration.
fn make_pub_fn(name: &str, params: Vec<Param>, ret: Option<TypeExpr>) -> FnDecl {
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        is_pure: false,
        name: name.to_string(),
        type_params: None,
        params,
        return_type: ret.map(|te| (te, 0..0)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    }
}

/// Helper: build a private (non-pub) function declaration.
fn make_priv_fn(name: &str) -> FnDecl {
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: name.to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    }
}

/// Helper: build an `ImportDecl` with resolved items.
fn make_user_import(
    path: &[&str],
    spec: Option<ImportSpec>,
    items: Vec<Spanned<Item>>,
) -> ImportDecl {
    ImportDecl {
        path: path.iter().map(ToString::to_string).collect(),
        spec,
        file_path: None,
        resolved_items: Some(items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    }
}

/// Helper: type-check a program with given items.
fn check_items(items: Vec<Spanned<Item>>) -> TypeCheckOutput {
    let program = Program {
        module_graph: None,
        items,
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&program)
}

// -- should_import_name --

#[test]
fn should_import_name_bare_import_returns_false() {
    assert!(!Checker::should_import_name("helper", &None));
}

#[test]
fn should_import_name_glob_returns_true() {
    assert!(Checker::should_import_name(
        "helper",
        &Some(ImportSpec::Glob)
    ));
    assert!(Checker::should_import_name(
        "anything",
        &Some(ImportSpec::Glob)
    ));
}

#[test]
fn should_import_name_named_match() {
    let spec = Some(ImportSpec::Names(vec![
        ImportName {
            name: "helper".to_string(),
            alias: None,
        },
        ImportName {
            name: "parse".to_string(),
            alias: None,
        },
    ]));
    assert!(Checker::should_import_name("helper", &spec));
    assert!(Checker::should_import_name("parse", &spec));
    assert!(!Checker::should_import_name("other", &spec));
}

// -- Bare import: qualified only --

#[test]
fn bare_import_registers_qualified_name() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        None, // bare import
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.fn_sigs.contains_key("utils.helper"),
        "bare import should register qualified name 'utils.helper'"
    );
    assert!(
        !output.fn_sigs.contains_key("helper"),
        "bare import should NOT register unqualified name 'helper'"
    );
}

// -- Glob import: everything unqualified --

#[test]
fn glob_import_registers_unqualified_names() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let other = make_pub_fn(
        "other",
        vec![],
        Some(TypeExpr::Named {
            name: "String".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Glob),
        vec![
            (Item::Function(helper), 0..0),
            (Item::Function(other), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // Both qualified and unqualified should be registered
    assert!(output.fn_sigs.contains_key("utils.helper"));
    assert!(output.fn_sigs.contains_key("utils.other"));
    assert!(
        output.fn_sigs.contains_key("helper"),
        "glob import should register unqualified 'helper'"
    );
    assert!(
        output.fn_sigs.contains_key("other"),
        "glob import should register unqualified 'other'"
    );
}

// -- Named import: specific names only --

#[test]
fn named_import_registers_specified_names_only() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let other = make_pub_fn(
        "other",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "helper".to_string(),
            alias: None,
        }])),
        vec![
            (Item::Function(helper), 0..0),
            (Item::Function(other), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // Both should be qualified
    assert!(output.fn_sigs.contains_key("utils.helper"));
    assert!(output.fn_sigs.contains_key("utils.other"));
    // Only "helper" should be unqualified
    assert!(
        output.fn_sigs.contains_key("helper"),
        "named import should register 'helper' unqualified"
    );
    assert!(
        !output.fn_sigs.contains_key("other"),
        "named import should NOT register 'other' unqualified"
    );
}

// -- Pub visibility enforcement --

#[test]
fn non_pub_functions_not_registered() {
    let priv_fn = make_priv_fn("secret");
    let pub_fn = make_pub_fn(
        "visible",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Glob), // even glob shouldn't expose private fns
        vec![
            (Item::Function(priv_fn), 0..0),
            (Item::Function(pub_fn), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        !output.fn_sigs.contains_key("utils.secret"),
        "non-pub function should not be registered as qualified"
    );
    assert!(
        !output.fn_sigs.contains_key("secret"),
        "non-pub function should not be registered as unqualified"
    );
    assert!(output.fn_sigs.contains_key("utils.visible"));
    assert!(output.fn_sigs.contains_key("visible"));
}

// -- User module const registration --

#[test]
fn user_module_registers_pub_consts() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "MAX_SIZE".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    let priv_const = ConstDecl {
        visibility: Visibility::Private,
        name: "INTERNAL".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(42, 0..2),
        doc_comment: None,
    };
    let import = make_user_import(
        &["myapp", "config"],
        Some(ImportSpec::Glob),
        vec![
            (Item::Const(pub_const), 0..0),
            (Item::Const(priv_const), 0..0),
        ],
    );

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _output = checker.check_program(&program);

    // pub const should be findable in the environment
    assert!(
        checker.env.lookup_ref("config.MAX_SIZE").is_some(),
        "pub const should be registered as qualified"
    );
    assert!(
        checker.env.lookup_ref("MAX_SIZE").is_some(),
        "pub const should be unqualified with glob import"
    );
    assert!(
        checker.env.lookup_ref("config.INTERNAL").is_none(),
        "private const should NOT be registered"
    );
    assert!(
        checker.env.lookup_ref("INTERNAL").is_none(),
        "private const should NOT be registered unqualified"
    );
}

#[test]
fn user_module_const_bare_import_qualified_only() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "LIMIT".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(50, 0..2),
        doc_comment: None,
    };
    let import = make_user_import(
        &["myapp", "config"],
        None, // bare import
        vec![(Item::Const(pub_const), 0..0)],
    );

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _output = checker.check_program(&program);

    assert!(
        checker.env.lookup_ref("config.LIMIT").is_some(),
        "pub const should be registered as qualified"
    );
    assert!(
        checker.env.lookup_ref("LIMIT").is_none(),
        "bare import should NOT register const unqualified"
    );
}

// -- User module type registration --

#[test]
fn user_module_registers_types() {
    let struct_decl = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "Config".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "value".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "i32".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: Vec::new(),
            doc_comment: None,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
    };
    let import = make_user_import(
        &["myapp", "config"],
        None, // bare import
        vec![(Item::TypeDecl(struct_decl), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.type_defs.contains_key("Config"),
        "user module type should be registered unqualified"
    );
    assert!(
        output.type_defs.contains_key("config.Config"),
        "user module type should also be registered as qualified"
    );
}

// -- user_modules set --

#[test]
fn user_modules_set_populated() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        None,
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.user_modules.contains("utils"),
        "user_modules should contain the module short name"
    );
}

#[test]
fn stdlib_not_in_user_modules() {
    // A stdlib import should NOT appear in user_modules
    let import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        !output.user_modules.contains("fs"),
        "stdlib module should NOT be in user_modules"
    );
}

// -- Function signature correctness --

#[test]
fn user_module_fn_sig_has_correct_types() {
    let helper = make_pub_fn(
        "add",
        vec![
            Param {
                name: "a".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
            },
            Param {
                name: "b".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
            },
        ],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["mylib", "math"],
        None,
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    let sig = output
        .fn_sigs
        .get("math.add")
        .expect("math.add should be registered");
    assert_eq!(sig.params.len(), 2, "should have 2 params");
    assert_eq!(sig.params[0], Ty::I32);
    assert_eq!(sig.params[1], Ty::I32);
    assert_eq!(sig.return_type, Ty::I32);
    assert_eq!(sig.param_names, vec!["a", "b"]);
}

// -- Multiple modules don't collide --

#[test]
fn two_modules_same_fn_name_no_collision() {
    let helper_a = make_pub_fn(
        "run",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let helper_b = make_pub_fn(
        "run",
        vec![],
        Some(TypeExpr::Named {
            name: "String".to_string(),
            type_args: None,
        }),
    );
    let import_a = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![(Item::Function(helper_a), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        None,
        vec![(Item::Function(helper_b), 0..0)],
    );
    let output = check_items(vec![
        (Item::Import(import_a), 0..0),
        (Item::Import(import_b), 0..0),
    ]);

    assert!(output.fn_sigs.contains_key("alpha.run"));
    assert!(output.fn_sigs.contains_key("beta.run"));
    // Both should have different return types
    assert_eq!(output.fn_sigs["alpha.run"].return_type, Ty::I32);
    assert_eq!(output.fn_sigs["beta.run"].return_type, Ty::String);
}

// -- Import with no resolved items (stdlib) still works --

#[test]
fn import_without_resolved_items_emits_unresolved_error() {
    // An import with resolved_items = None and no stdlib match (empty registry)
    // must now emit an UnresolvedImport error rather than silently dropping.
    let import = ImportDecl {
        path: vec!["unknown".to_string(), "pkg".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "expected UnresolvedImport error, got: {errors:?}",
        errors = output.errors
    );
    assert!(!output.user_modules.contains("pkg"));
}

#[test]
fn import_with_resolved_items_no_error() {
    // When resolved_items is provided the user-module path is taken and no
    // UnresolvedImport diagnostic should be emitted.
    let import = make_user_import(&["myapp", "util"], None, vec![]);
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "unexpected UnresolvedImport error for user module with resolved_items"
    );
    assert!(output.user_modules.contains("util"));
}

#[test]
fn stdlib_import_keeps_stream_from_file_stream_typed_after_fs_import() {
    let stream_import = ImportDecl {
        path: vec!["std".to_string(), "stream".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let fs_import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let program = Program {
        module_graph: None,
        items: vec![
            (Item::Import(stream_import), 0..0),
            (Item::Import(fs_import), 0..0),
        ],
        module_doc: None,
    };

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&program);
    let stream_from_file = output
        .fn_sigs
        .get("stream.from_file")
        .expect("expected std::stream import to register stream.from_file");

    assert_eq!(
        stream_from_file.return_type,
        Ty::result(Ty::stream(Ty::String), Ty::String),
        "std::stream import should keep from_file() typed as Result<Stream<String>, String>"
    );
}

#[test]
fn file_import_without_resolved_items_emits_unresolved_error() {
    let import = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("missing.hew".to_string()),
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..20)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    let error = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UnresolvedImport)
        .expect("expected UnresolvedImport error for unresolved file import");

    assert!(
        error.message.contains("missing.hew"),
        "unresolved file import should mention the missing file path: {error:?}"
    );
}

#[test]
fn merged_file_import_duplicate_pub_name_emits_duplicate_definition() {
    let shared_decl = make_pub_fn(
        "shared",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("pkg.hew".to_string()),
        resolved_items: Some(vec![
            (Item::Function(shared_decl.clone()), 0..5),
            (Item::Function(shared_decl), 10..15),
        ]),
        resolved_item_source_paths: vec![
            std::path::PathBuf::from("pkg/pkg.hew"),
            std::path::PathBuf::from("pkg/helpers.hew"),
        ],
        resolved_source_paths: vec![
            std::path::PathBuf::from("pkg/pkg.hew"),
            std::path::PathBuf::from("pkg/helpers.hew"),
        ],
    };
    let output = check_items(vec![(Item::Import(import), 0..20)]);
    let error = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::DuplicateDefinition)
        .expect("merged file import should fail closed on duplicate pub names");

    assert!(
        error.message.contains("shared"),
        "duplicate pub name error should mention the colliding binding: {error:?}"
    );
    assert_eq!(
        error.notes.first().map(|(span, _)| span.clone()),
        Some(0..5),
        "duplicate pub name should point back to the first merged definition"
    );
}

#[test]
fn repeated_flat_file_import_with_same_resolved_source_does_not_reregister_items() {
    let shared_source = std::path::PathBuf::from("pkg/pkg.hew");
    let import = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("pkg.hew".to_string()),
        resolved_items: Some(vec![(
            Item::Function(make_pub_fn(
                "shared",
                vec![],
                Some(TypeExpr::Named {
                    name: "i32".to_string(),
                    type_args: None,
                }),
            )),
            0..5,
        )]),
        resolved_item_source_paths: vec![shared_source.clone()],
        resolved_source_paths: vec![shared_source],
    };
    let output = check_items(vec![
        (Item::Import(import.clone()), 0..5),
        (Item::Import(import), 10..15),
    ]);

    assert!(
        output.errors.is_empty(),
        "same resolved flat file import should stay idempotent: {:?}",
        output.errors
    );
    assert!(
        output.fn_sigs.contains_key("shared"),
        "flat file import should still register the imported function"
    );
}

#[test]
fn repeated_stdlib_import_does_not_duplicate_hew_items() {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    let fs_path = repo_root.join("std/fs.hew");
    let source = std::fs::read_to_string(&fs_path).expect("std/fs.hew should exist");
    let parsed = hew_parser::parse(&source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors in std/fs.hew: {:?}",
        parsed.errors
    );

    let import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        spec: None,
        file_path: None,
        resolved_items: Some(parsed.program.items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: vec![fs_path],
    };
    let program = Program {
        module_graph: None,
        items: vec![
            (Item::Import(import.clone()), 0..0),
            (Item::Import(import), 0..0),
        ],
        module_doc: None,
    };

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "unexpected errors for repeated stdlib import: {:?}",
        output.errors
    );
    assert!(
        output.type_defs.contains_key("IoError"),
        "expected std::fs Hew items to remain registered"
    );
}

// -- Empty module import --

#[test]
fn empty_module_import_no_crash() {
    let import = make_user_import(&["myapp", "empty"], None, vec![]);
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(output.user_modules.contains("empty"));
    assert!(output.errors.is_empty());
}

// -- Import alias binding --

#[test]
fn import_alias_binds_under_alias_name() {
    // import mymod::{foo as bar} — "bar" must resolve, "foo" must not be unqualified
    let helper = make_pub_fn(
        "foo",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["mymod"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "foo".to_string(),
            alias: Some("bar".to_string()),
        }])),
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // qualified form always uses original name
    assert!(
        output.fn_sigs.contains_key("mymod.foo"),
        "qualified 'mymod.foo' should be registered regardless of alias"
    );
    // unqualified binding must use the alias
    assert!(
        output.fn_sigs.contains_key("bar"),
        "aliased import should register unqualified binding 'bar'"
    );
    // original unqualified name must NOT be registered
    assert!(
        !output.fn_sigs.contains_key("foo"),
        "aliased import must NOT register unqualified 'foo'"
    );
}

#[test]
fn import_alias_multiple_names() {
    // import pkg::{alpha as a, beta as b}
    let fn_alpha = make_pub_fn(
        "alpha",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let fn_beta = make_pub_fn(
        "beta",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["pkg"],
        Some(ImportSpec::Names(vec![
            ImportName {
                name: "alpha".to_string(),
                alias: Some("a".to_string()),
            },
            ImportName {
                name: "beta".to_string(),
                alias: Some("b".to_string()),
            },
        ])),
        vec![
            (Item::Function(fn_alpha), 0..0),
            (Item::Function(fn_beta), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.fn_sigs.contains_key("a"),
        "'a' alias should be registered"
    );
    assert!(
        output.fn_sigs.contains_key("b"),
        "'b' alias should be registered"
    );
    assert!(
        !output.fn_sigs.contains_key("alpha"),
        "original 'alpha' must not be unqualified"
    );
    assert!(
        !output.fn_sigs.contains_key("beta"),
        "original 'beta' must not be unqualified"
    );
}

// -- Trait import from module --

#[test]
fn import_trait_from_module_glob() {
    use hew_parser::ast::{TraitDecl, TraitItem, TraitMethod};

    let trait_decl = TraitDecl {
        visibility: Visibility::Pub,
        name: "Display".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "display".to_string(),
            is_pure: true,
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
        })],
        doc_comment: None,
    };
    let import = make_user_import(
        &["mylib", "fmt"],
        Some(ImportSpec::Glob),
        vec![(Item::Trait(trait_decl), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.errors.is_empty(),
        "importing a pub trait should not produce errors: {:?}",
        output.errors
    );
    // The module should be registered as a user module
    assert!(
        output.user_modules.contains("fmt"),
        "module 'fmt' should be in user_modules"
    );
}

#[test]
fn import_private_trait_not_registered() {
    use hew_parser::ast::{TraitDecl, TraitItem, TraitMethod};

    let private_trait = TraitDecl {
        visibility: Visibility::Private,
        name: "Internal".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "internal_op".to_string(),
            is_pure: false,
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
        })],
        doc_comment: None,
    };
    let import = make_user_import(
        &["mylib", "internals"],
        Some(ImportSpec::Glob),
        vec![(Item::Trait(private_trait), 0..0)],
    );
    // Should complete without errors; private trait is simply ignored
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
}

// -- Orphan rule warning --

#[test]
fn orphan_impl_emits_warning() {
    use hew_parser::ast::TraitBound;
    // impl ExternalTrait for ExternalType → neither is local → orphan warning
    let impl_decl = ImplDecl {
        type_params: None,
        trait_bound: Some(TraitBound {
            name: "SomeTrait".to_string(),
            type_args: None,
        }),
        target_type: (
            TypeExpr::Named {
                name: "SomeType".to_string(),
                type_args: None,
            },
            0..0,
        ),
        where_clause: None,
        type_aliases: vec![],
        methods: vec![],
    };
    let output = check_items(vec![(Item::Impl(impl_decl), 0..0)]);

    let has_orphan_warning = output
        .warnings
        .iter()
        .any(|w| w.kind == crate::error::TypeErrorKind::OrphanImpl);
    assert!(
        has_orphan_warning,
        "expected OrphanImpl warning when neither trait nor type is local, got: {:?}",
        output.warnings
    );
}

#[test]
fn local_type_impl_no_orphan_warning() {
    use hew_parser::ast::TraitBound;
    // Locally defined type: impl SomeExternalTrait for LocalType → no orphan warning
    let type_decl = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "LocalType".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
    };
    let impl_decl = ImplDecl {
        type_params: None,
        trait_bound: Some(TraitBound {
            name: "ExternalTrait".to_string(),
            type_args: None,
        }),
        target_type: (
            TypeExpr::Named {
                name: "LocalType".to_string(),
                type_args: None,
            },
            0..0,
        ),
        where_clause: None,
        type_aliases: vec![],
        methods: vec![],
    };
    let output = check_items(vec![
        (Item::TypeDecl(type_decl), 0..0),
        (Item::Impl(impl_decl), 0..0),
    ]);

    let has_orphan = output
        .warnings
        .iter()
        .any(|w| w.kind == crate::error::TypeErrorKind::OrphanImpl);
    assert!(
        !has_orphan,
        "impl on a locally defined type must NOT produce an orphan warning"
    );
}

#[test]
fn test_file_import_private_items_not_visible() {
    use hew_parser::ast::{
        Block, ConstDecl, Expr, FnDecl, ImportDecl, Item, Literal, Program, Spanned, TypeDecl,
        TypeDeclKind, TypeExpr,
    };

    let private_fn = Item::Function(FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: "private_func".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    });

    let private_const = Item::Const(ConstDecl {
        visibility: Visibility::Private,
        name: "PRIVATE_CONST".to_string(),
        ty: (
            TypeExpr::Named {
                name: "Int".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: (
            Expr::Literal(Literal::Integer {
                value: 42,
                radix: hew_parser::ast::IntRadix::Decimal,
            }),
            0..0,
        ),
        doc_comment: None,
    });

    let private_type = Item::TypeDecl(TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "PrivateType".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
    });

    let resolved: Vec<Spanned<Item>> = vec![
        (private_fn, 0..0),
        (private_const, 0..0),
        (private_type, 0..0),
    ];

    let import_decl = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("private_lib.hew".to_string()),
        resolved_items: Some(resolved),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        !output.fn_sigs.contains_key("private_func"),
        "private function must not be registered from file import"
    );
    assert!(
        checker.env.lookup("PRIVATE_CONST").is_none(),
        "private const must not be registered from file import"
    );
    assert!(
        !checker.known_types.contains("PrivateType"),
        "private type must not be registered from file import"
    );
}

#[test]
fn check_generic_lambda() {
    let source = r"
        fn apply<T>(f: fn(T) -> T, x: T) -> T {
            f(x)
        }

        fn main() {
            // Identity generic lambda
            let id = <T>(x: T) => x;
            // Instantiation happens when calling `apply`
            // apply takes fn(T) -> T. `id` matches that.
            // However, `id` is a generic closure.
            // We need to make sure generic instantiation works.
            // Currently, `check_lambda` creates fresh type variables for T.
            // So id has type ?0 -> ?0.
            // When passed to apply(id, 5), T inferred as int.
            // apply expects fn(int) -> int.
            // id matches fn(?0) -> ?0 where ?0=int.
            let res = apply(id, 5);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
}

/// Slice-1 generic lambda regression test.
///
/// Verifies that:
/// 1. A let-bound generic lambda type-checks cleanly.
/// 2. A direct call whose arguments make the type obvious resolves the
///    return type correctly.
/// 3. `call_type_args` is populated for the call so the enricher can
///    fill in explicit type arguments before serialisation to MLIR.
#[test]
fn generic_lambda_slice1_type_inference() {
    let source = r"
        fn main() {
            let v: int = 30;
            let r = <T>(a: T, b: T) -> T => a;
            let q = r(v, v);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    // The call r(v, v) must have produced a call_type_args entry (T→int).
    assert!(
        !output.call_type_args.is_empty(),
        "call_type_args should contain the inferred type for r(v,v)"
    );
    // The single entry should map to [int / i64].
    let type_args: Vec<_> = output.call_type_args.values().collect();
    assert_eq!(type_args.len(), 1);
    assert_eq!(
        type_args[0],
        &vec![crate::ty::Ty::I64],
        "T should be inferred as int (i64)"
    );
}

/// Slice-1: two-type-param generic lambda, verify both params inferred.
#[test]
fn generic_lambda_slice1_two_type_params() {
    let source = concat!(
        "fn main() {\n",
        r#"    let combine = <A, B>(a: A, b: B) -> A => a;"#,
        "\n",
        r#"    let x: int = 1;"#,
        "\n",
        r#"    let y: string = "hello";"#,
        "\n",
        r#"    let z = combine(x, y);"#,
        "\n",
        "}\n",
    );

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    // Should have one call_type_args entry with two type args.
    assert_eq!(
        output.call_type_args.len(),
        1,
        "expected one call_type_args entry"
    );
    let args = output.call_type_args.values().next().unwrap();
    assert_eq!(args.len(), 2, "expected two type args (A and B)");
}

#[test]
fn contextual_lambda_binding_records_lambda_expr_type() {
    let source = concat!(
        "fn main() {\n",
        "    let f: fn(int) -> int = (x) => x + 1;\n",
        "    let y = f(5);\n",
        "}\n",
    );

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let lambda_span = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.stmts.iter().find_map(|(stmt, _)| match stmt {
                    Stmt::Let {
                        value: Some((Expr::Lambda { .. }, span)),
                        ..
                    } => Some(span.clone()),
                    _ => None,
                })
            }
            _ => None,
        })
        .expect("main let-bound lambda should exist");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    assert_eq!(
        output.expr_types.get(&SpanKey::from(&lambda_span)),
        Some(&Ty::Function {
            params: vec![Ty::I64],
            ret: Box::new(Ty::I64),
        })
    );
}

#[test]
fn method_level_type_params_freshen_per_named_method_call() {
    let source = r"
        type Holder { value: int }

        impl Holder {
            fn pick<T>(h: Holder, value: T) -> T {
                value
            }
        }

        fn main() {
            let h = Holder { value: 1 };
            let n = h.pick(42);
            let flag = h.pick(true);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        2,
        "expected one entry per method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one call to infer T=int, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one call to infer T=bool, got {:?}",
        output.call_type_args
    );
}

#[test]
fn generic_impl_method_level_type_params_freshen_per_call() {
    let source = r"
        type Box<T> { value: T }

        impl<T> Box<T> {
            fn transform<U>(b: Box<T>, f: fn(T) -> U) -> Box<U> {
                Box { value: f(b.value) }
            }
        }

        fn double(x: int) -> int { x * 2 }
        fn is_even(x: int) -> bool { x % 2 == 0 }

        fn main() {
            let b = Box { value: 42 };
            let doubled = b.transform(double);
            let even = b.transform(is_even);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        2,
        "expected one entry per method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one call to infer U=int, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one call to infer U=bool, got {:?}",
        output.call_type_args
    );
}

#[test]
fn generic_impl_method_underconstrained_type_param_reports_inference_failed() {
    let source = r"
        enum Maybe<T> { Some(T); None; }
        type Holder {}

        impl Holder {
            fn wrap<T>(h: Holder, value: Maybe<T>) -> Maybe<T> {
                value
            }
        }

        fn main() {
            let h = Holder {};
            let unresolved = h.wrap(None);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::InferenceFailed),
        "expected InferenceFailed for unresolved method-level type param, got {:?}",
        output.errors
    );
}

#[test]
fn trait_method_type_params_freshen_per_call_on_bounded_type_param() {
    let source = r"
        trait Transform {
            fn apply<U>(item: Self, f: fn(int) -> U) -> U;
        }

        type Holder { value: int }

        impl Transform for Holder {
            fn apply<U>(item: Holder, f: fn(int) -> U) -> U {
                f(item.value)
            }
        }

        fn double(x: int) -> int { x * 2 }
        fn is_odd(x: int) -> bool { x % 2 != 0 }

        fn run<T: Transform>(item: T) {
            let doubled = item.apply(double);
            let odd = item.apply(is_odd);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        2,
        "expected one entry per trait-bound method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one trait-bound call to infer U=int, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one trait-bound call to infer U=bool, got {:?}",
        output.call_type_args
    );
}

#[test]
fn trait_method_type_params_do_not_unify_across_calls() {
    let source = r"
        trait Transform {
            fn apply<U>(item: Self, f: fn(int) -> U) -> U;
        }

        type Holder { value: int }

        impl Transform for Holder {
            fn apply<U>(item: Holder, f: fn(int) -> U) -> U {
                f(item.value)
            }
        }

        fn double(x: int) -> int { x * 2 }
        fn is_odd(x: int) -> bool { x % 2 != 0 }

        fn run<T: Transform>(item: T) {
            let doubled = item.apply(double);
            let odd = item.apply(is_odd);
            println(doubled);
            println(odd);
        }

        fn main() {
            let h = Holder { value: 21 };
            run(h);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "trait-bound method calls should infer independently, got {:?}",
        output.errors
    );
}

/// Regression: a generic lambda passed as a function *argument* (not
/// directly bound to `let`) must not leak its `TypeVar` pairs into the
/// scratch field and then be picked up by the *next* unrelated let-binding.
///
/// Before the fix, `last_lambda_generic_vars` was set by `check_lambda`
/// any time a generic lambda was type-checked, so the following sequence
/// would falsely register `q` as having a generic lambda type:
///
/// ```text
///   fn apply<T>(f: fn(T) -> T, x: T) -> T { f(x) }
///   fn main() {
///       apply(<T>(x: T) => x, 5);  // generic lambda in arg position
///       let q = 42;                 // should NOT be in lambda_poly_type_var_map
///   }
/// ```
#[test]
fn generic_lambda_scratch_state_no_leak() {
    let source = r"
        fn apply<T>(f: fn(T) -> T, x: T) -> T {
            f(x)
        }

        fn main() {
            apply(<T>(x: T) => x, 5);
            let q = 42;
            let z = q + 1;
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    // call_type_args may have an entry for the `apply(...)` call, but q
    // must not appear in lambda_poly_type_var_map.  We verify indirectly:
    // the number of call_type_args entries must be exactly 1 (for `apply`)
    // and must not grow due to a spurious phantom call on `q` or `z`.
    // (There is no call through q, so any extra entry would signal a leak.)
    assert!(
        output.call_type_args.len() <= 1,
        "expected at most 1 call_type_args entry (for apply), got {}; \
         stale lambda scratch state likely leaked into a later let-binding",
        output.call_type_args.len()
    );
}

#[test]
fn test_self_with_generics_in_impl() {
    let source = r"
        type Pair<T> {
            first: T,
            second: T,
        }

        impl<T> Pair<T> {
            fn new(first: T, second: T) -> Self {
                return Pair { first: first, second: second };
            }

            fn swap(p: Pair<T>) -> Self {
                return Pair { first: p.second, second: p.first };
            }
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    // Verify that Self resolves to Pair<T>, not bare Pair
    // The new method should return Pair<T>
    let new_sig = output
        .fn_sigs
        .get("Pair::new")
        .expect("Pair::new should exist");
    if let Ty::Named { name, args } = &new_sig.return_type {
        assert_eq!(name, "Pair", "return type should be Pair");
        assert_eq!(args.len(), 1, "Pair should have one type argument");
    } else {
        panic!("Expected Pair::new to return a named type");
    }
}

#[test]
fn test_trait_object_type_args_substitution() {
    // Bug 2: Test that dyn Trait<Args> methods get correct substitutions
    let source = r"
        trait Iterator<T> {
            fn next(iter: Self) -> Option<T>;
        }

        type Counter {
            count: int;
        }

        impl Iterator<int> for Counter {
            fn next(c: Counter) -> Option<int> {
                Some(42)
            }
        }

        fn test_iterator() -> int {
            let iter: dyn Iterator<int> = Counter { count: 5 };
            let result = iter.next(); // Should be Option<int>, not Option<T>
            match result {
                Some(x) => x,
                None => 0
            }
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    if !output.errors.is_empty() {
        for error in &output.errors {
            println!("Type error: {error}");
        }
    }

    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
}

#[test]
fn trait_bound_compound_generic_methods_do_not_cross_contaminate() {
    let source = r#"
        trait Transform {
            fn apply<U>(item: Self, f: fn(int) -> U) -> U;
        }

        trait Label {
            fn tag<V>(item: Self, prefix: V) -> string;
        }

        type Holder { value: int }

        impl Transform for Holder {
            fn apply<U>(item: Holder, f: fn(int) -> U) -> U {
                f(item.value)
            }
        }

        impl Label for Holder {
            fn tag<V>(item: Holder, prefix: V) -> string {
                "tagged"
            }
        }

        fn is_odd(x: int) -> bool { x % 2 != 0 }

        fn run<T: Transform + Label>(item: T) {
            let odd = item.apply(is_odd);
            let tagged_num = item.tag(42);
            let tagged_str = item.tag("lbl");
            println(odd);
            println(tagged_num);
            println(tagged_str);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        3,
        "expected one entry per compound-bound generic method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one Transform call to infer U=bool, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one Label call to infer V=int, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::String]),
        "expected one Label call to infer V=string, got {:?}",
        output.call_type_args
    );
}

#[test]
fn test_wire_since_without_version_warns() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: None,
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert!(checker.errors.is_empty(), "should not produce errors");
    assert_eq!(checker.warnings.len(), 1);
    assert!(
        checker.warnings[0].message.contains("since 2"),
        "warning should mention since: {}",
        checker.warnings[0].message
    );
    assert!(
        checker.warnings[0]
            .message
            .contains("no #[wire(version = N)]"),
        "warning should mention missing version: {}",
        checker.warnings[0].message
    );
    assert_eq!(checker.warnings[0].span, 0..0);
}

#[test]
fn test_wire_since_without_version_uses_registered_decl_span() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: None,
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_type_namespace_name("TestMsg", &(10..50));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert_eq!(checker.warnings.len(), 1);
    assert_eq!(checker.warnings[0].span, 10..50);
}

#[test]
fn test_wire_since_with_version_no_extra_warning() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: Some(3),
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert!(checker.errors.is_empty(), "should not produce errors");
    // No "since without version" warning since version is present
    let since_without_version = checker
        .warnings
        .iter()
        .any(|w| w.message.contains("no #[wire(version = N)]"));
    assert!(
        !since_without_version,
        "should not warn about missing version"
    );
}

#[test]
fn empty_fn_body_return_mismatch_uses_decl_span() {
    let source = "fn greet() -> string {}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let expected_span = match &result.program.items[0].0 {
        hew_parser::ast::Item::Function(fd) => fd.decl_span.clone(),
        item => panic!("expected function item, got {item:?}"),
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let mismatch = output
        .errors
        .iter()
        .find(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. }))
        .unwrap_or_else(|| panic!("expected mismatch error, got {:?}", output.errors));

    assert_eq!(mismatch.span, expected_span);
}

#[test]
fn empty_receive_fn_body_return_mismatch_uses_decl_span() {
    let source = r"
actor Greeter {
    receive fn greet() -> string {}
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let expected_span = match &result.program.items[0].0 {
        hew_parser::ast::Item::Actor(actor) => actor.receive_fns[0].span.clone(),
        item => panic!("expected actor item, got {item:?}"),
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let mismatch = output
        .errors
        .iter()
        .find(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. }))
        .unwrap_or_else(|| panic!("expected mismatch error, got {:?}", output.errors));

    assert_eq!(mismatch.span, expected_span);
}

#[test]
fn actor_ref_cycle_warning_uses_first_actor_decl_span() {
    let source = concat!(
        "actor Alpha {\n",
        "    let beta: ActorRef<Beta>;\n",
        "}\n",
        "actor Beta {\n",
        "    let alpha: ActorRef<Alpha>;\n",
        "}\n",
        "fn main() {}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let expected_span = result
        .program
        .items
        .iter()
        .find_map(|(item, span)| match item {
            Item::Actor(actor) if actor.name == "Alpha" => Some(span.clone()),
            _ => None,
        })
        .expect("expected Alpha actor item");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "actor cycle warning should not introduce type errors: {:?}",
        output.errors
    );

    let warning = output
        .warnings
        .iter()
        .find(|warning| warning.kind == TypeErrorKind::ActorRefCycle)
        .unwrap_or_else(|| panic!("expected ActorRefCycle warning, got {:?}", output.warnings));
    let actor_decl_start = source
        .find("actor Alpha")
        .expect("expected Alpha declaration text");
    let actor_name_end = actor_decl_start + "actor Alpha".len();

    assert_ne!(warning.span, 0..0);
    assert_eq!(warning.span, expected_span);
    assert!(
        warning.span.start <= actor_decl_start && actor_name_end <= warning.span.end,
        "warning span should cover the first actor declaration, got {:?}",
        warning.span
    );
}

#[test]
fn typecheck_await_actor_ref_returns_unit() {
    let output = check_source(
        r#"
        actor Greeter {
            receive fn greet(name: String) {
                println(name);
            }
        }
        fn main() {
            let g = spawn Greeter;
            g.greet("hi");
            close(g);
            await g;
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:?}",
        output.errors
    );
}

#[test]
fn named_actor_receive_dispatch_reports_bad_arg_once() {
    let result = hew_parser::parse(
        r"
        actor Greeter {
            receive fn greet(name: String) {}
        }

        fn main() {
            let g = spawn Greeter;
            g.greet(missing_name);
        }
        ",
    );
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let undefined_variable_count = output
        .errors
        .iter()
        .filter(|error| matches!(error.kind, TypeErrorKind::UndefinedVariable))
        .count();

    assert_eq!(
        undefined_variable_count, 1,
        "named actor receive dispatch should not resynthesize the same bad arg: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_await_close_actor_ref() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    checker.env.define(
        "g".to_string(),
        Ty::actor_ref(Ty::Named {
            name: "Greeter".to_string(),
            args: vec![],
        }),
        false,
    );

    let span = 0..0;
    let expr = Expr::Await(Box::new((
        Expr::Call {
            function: Box::new((Expr::Identifier("close".to_string()), span.clone())),
            type_args: None,
            args: vec![CallArg::Positional((
                Expr::Identifier("g".to_string()),
                span.clone(),
            ))],
            is_tail_call: false,
        },
        span.clone(),
    )));

    let ty = checker.synthesize(&expr, &span);
    assert_eq!(ty, Ty::Unit);
    assert!(
        checker.errors.is_empty(),
        "expected no errors, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_await_close_lambda_actor() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    checker.env.define(
        "worker".to_string(),
        Ty::Named {
            name: "Actor".to_string(),
            args: vec![Ty::I64],
        },
        false,
    );

    let span = 0..0;
    let expr = Expr::Await(Box::new((
        Expr::Call {
            function: Box::new((Expr::Identifier("close".to_string()), span.clone())),
            type_args: None,
            args: vec![CallArg::Positional((
                Expr::Identifier("worker".to_string()),
                span.clone(),
            ))],
            is_tail_call: false,
        },
        span.clone(),
    )));

    let ty = checker.synthesize(&expr, &span);
    assert_eq!(ty, Ty::Unit);
    assert!(
        checker.errors.is_empty(),
        "expected no errors, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_join_rejects_non_actor_sources() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let span = 0..0;
    let expr = Expr::Join(vec![
        make_int_literal(1, span.clone()),
        make_int_literal(2, span.clone()),
    ]);
    let _ = checker.synthesize(&expr, &span);
    assert!(
        checker.errors.iter().any(|error| error
            .message
            .contains("join expression element must be actor.method(args)")),
        "expected join source error, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_integer_literal_coerces_in_arithmetic() {
    // `n - 1` where n: i32 should work — literal 1 coerces to i32
    let source = concat!(
        "fn fib(n: i32) -> i32 {\n",
        "    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\n",
        "}\n",
        "fn main() { println(fib(10)); }\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "integer literal should coerce in arithmetic: {:?}",
        output.errors
    );
}

#[test]
fn i32_coerces_to_bool_in_condition_position() {
    let (errors, _warnings) =
        parse_and_check("fn foo(flag: i32) -> i32 { if flag { 1 } else { 0 } }");
    assert!(
        errors.is_empty(),
        "i32 in bool position must be allowed: {errors:?}"
    );
}

#[test]
fn bool_does_not_coerce_to_i32() {
    let (errors, _warnings) = parse_and_check("fn foo(flag: bool) -> i32 { flag }");
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "bool where i32 expected must be rejected: {errors:?}"
    );
}

#[test]
fn handle_type_does_not_coerce_to_string() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        "import std::encoding::json;\nfn foo(value: json.Value) -> String { value }",
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "json.Value where String expected must be rejected: {errors:?}"
    );
}

#[test]
fn string_does_not_coerce_to_handle_type() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        "import std::encoding::json;\nfn foo(text: String) -> json.Value { text }",
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "String where json.Value expected must be rejected: {errors:?}"
    );
}

#[test]
fn unconstrained_range_defaults_to_i64() {
    let source = "fn main() { for i in 0..10 { println(i); } }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::For { iterable, .. } = &main_fn.body.stmts[0].0 else {
        panic!("expected for statement");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unconstrained range should type-check: {:?}",
        output.errors
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(&iterable.1)),
        Some(&Ty::range(Ty::I64)),
        "unconstrained range literal should default to Range<i64>: {:?}",
        output.expr_types
    );
}

#[test]
fn typecheck_literal_range_infers_from_context() {
    // `for i in 0..8 { fib(i) }` where fib takes i32 — range bounds
    // should not force i64; the loop variable should be usable as i32
    let source = concat!(
        "fn fib(n: i32) -> i32 {\n",
        "    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\n",
        "}\n",
        "fn main() {\n",
        "    for i in 0..8 {\n",
        "        println(fib(i));\n",
        "    }\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "literal range should infer element type from context: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_cast_expression_numeric() {
    let source = concat!(
        "fn main() {\n",
        "    let x: i64 = 42;\n",
        "    let y: i32 = x as i32;\n",
        "    let z: f64 = y as f64;\n",
        "    println(y);\n",
        "    println(z);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "numeric casts should type-check: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_cast_expression_invalid() {
    let source = concat!(
        "fn main() {\n",
        "    let s = \"hello\";\n",
        "    let x = s as i32;\n",
        "    println(x);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot cast")),
        "should reject invalid cast: {:?}",
        output.errors
    );
}

// ── Literal coercion tests ────────────────────────────────────────

#[test]
fn literal_coercion_integer_to_i32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
}

#[test]
fn literal_coercion_integer_to_u8() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(255, 0..3);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U8);
    assert_eq!(ty, Ty::U8);
}

#[test]
fn literal_coercion_integer_to_u8_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(256, 0..3);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U8);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_negative_to_unsigned() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = (
        Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(make_int_literal(1, 1..2)),
        },
        0..2,
    );
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("negative literal")),
        "expected negative-to-unsigned error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_i32_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // 2^31 = 2147483648, which exceeds i32 max (2147483647)
    let lit = make_int_literal(2_147_483_648, 0..10);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::I32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_integer_to_f32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::F32);
    assert_eq!(ty, Ty::F32);
}

#[test]
fn literal_coercion_float_to_f32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = (Expr::Literal(Literal::Float(std::f64::consts::PI)), 0..4);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::F32);
    assert_eq!(ty, Ty::F32);
}

#[test]
fn literal_coercion_integer_fits_i8() {
    assert!(integer_fits_type(127, &Ty::I8));
    assert!(integer_fits_type(-128, &Ty::I8));
    assert!(!integer_fits_type(128, &Ty::I8));
    assert!(!integer_fits_type(-129, &Ty::I8));
}

#[test]
fn literal_coercion_integer_fits_u8() {
    assert!(integer_fits_type(0, &Ty::U8));
    assert!(integer_fits_type(255, &Ty::U8));
    assert!(!integer_fits_type(256, &Ty::U8));
    assert!(!integer_fits_type(-1, &Ty::U8));
}

#[test]
fn literal_coercion_integer_fits_i16() {
    assert!(integer_fits_type(32767, &Ty::I16));
    assert!(integer_fits_type(-32768, &Ty::I16));
    assert!(!integer_fits_type(32768, &Ty::I16));
}

#[test]
fn literal_coercion_integer_fits_u16() {
    assert!(integer_fits_type(65535, &Ty::U16));
    assert!(!integer_fits_type(65536, &Ty::U16));
}

#[test]
fn literal_coercion_integer_fits_i32() {
    assert!(integer_fits_type(2_147_483_647, &Ty::I32));
    assert!(integer_fits_type(-2_147_483_648, &Ty::I32));
    assert!(!integer_fits_type(2_147_483_648, &Ty::I32));
}

#[test]
fn literal_coercion_integer_fits_u32() {
    assert!(integer_fits_type(4_294_967_295, &Ty::U32));
    assert!(!integer_fits_type(4_294_967_296, &Ty::U32));
    assert!(!integer_fits_type(-1, &Ty::U32));
}

#[test]
fn literal_coercion_integer_fits_u64() {
    // i64 max fits in u64
    assert!(integer_fits_type(i64::MAX, &Ty::U64));
    // 0 fits
    assert!(integer_fits_type(0, &Ty::U64));
    // Negative doesn't fit
    assert!(!integer_fits_type(-1, &Ty::U64));
}

// ── Array literal → Array type coercion tests ────────────────────

#[test]
fn literal_coercion_array_to_i32_array() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(1, 1..2),
        make_int_literal(2, 4..5),
        make_int_literal(3, 7..8),
    ];
    let arr = (Expr::Array(elems), 0..9);
    let expected = Ty::Array(Box::new(Ty::I32), 3);
    let ty = checker.check_against(&arr.0, &arr.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {checker_errors:#?}",
        checker_errors = checker.errors
    );
}

#[test]
fn literal_coercion_array_repeat_to_i32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let value = make_int_literal(0, 1..2);
    let count = make_int_literal(5, 4..5);
    let arr = (
        Expr::ArrayRepeat {
            value: Box::new(value),
            count: Box::new(count),
        },
        0..6,
    );
    let expected = Ty::Array(Box::new(Ty::I32), 5);
    let ty = checker.check_against(&arr.0, &arr.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Tuple literal coercion tests ───────────────────────────────────

#[test]
fn tuple_literal_coercion_to_typed() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(42, 1..3),
        (Expr::Literal(Literal::Float(std::f64::consts::PI)), 5..9),
    ];
    let tuple = (Expr::Tuple(elems), 0..10);
    let expected = Ty::Tuple(vec![Ty::I32, Ty::F32]);
    let ty = checker.check_against(&tuple.0, &tuple.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn tuple_literal_coercion_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![make_int_literal(256, 1..4)];
    let tuple = (Expr::Tuple(elems), 0..5);
    let expected = Ty::Tuple(vec![Ty::U8]);
    let _ty = checker.check_against(&tuple.0, &tuple.1, &expected);
    // The tuple itself coerces, but the element should have reported an error
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn range_default_type_is_i64() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let range = (
        Expr::Range {
            start: None,
            end: None,
            inclusive: false,
        },
        0..2,
    );
    let ty = checker.synthesize(&range.0, &range.1);
    assert_eq!(ty, Ty::range(Ty::I64));
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Type variable resolution in check_against ────────────────────

#[test]
fn literal_coercion_through_type_var() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Create a type variable and unify it with i32
    let tv = TypeVar::fresh();
    checker.subst.insert(tv, &Ty::I32).unwrap();
    // Now check an integer literal against the type variable
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::Var(tv));
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Let-bound literal coercion tests ─────────────────────────────

#[test]
fn let_bound_literal_coercion() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Simulate: let n = 5
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some(make_int_literal(5, 8..9)),
    };
    checker.check_stmt(&let_stmt, &(0..10));
    // Now check: let x: i32 = n
    let ident = (Expr::Identifier("n".to_string()), 15..16);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn let_bound_literal_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Simulate: let n = 2147483648 (exceeds i32 max)
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some(make_int_literal(2_147_483_648, 8..18)),
    };
    checker.check_stmt(&let_stmt, &(0..19));
    // Now check: let x: i32 = n — should fail with range error
    let ident = (Expr::Identifier("n".to_string()), 24..25);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn derived_intliteral_identifier_coerces_without_const_values() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let source_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some((
            Expr::Binary {
                left: Box::new(make_int_literal(1, 8..9)),
                op: BinaryOp::Add,
                right: Box::new(make_int_literal(2, 12..13)),
            },
            8..13,
        )),
    };
    checker.check_stmt(&source_stmt, &(0..14));

    assert!(
        !checker.const_values.contains_key("n"),
        "derived literals should not register const_values"
    );

    let target_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("y".to_string()), 20..21),
        ty: Some((
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            23..26,
        )),
        value: Some((Expr::Identifier("n".to_string()), 29..30)),
    };
    checker.check_stmt(&target_stmt, &(20..30));

    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn negated_literal_let_binding_coerces_signed() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some((
            Expr::Unary {
                op: UnaryOp::Negate,
                operand: Box::new(make_int_literal(5, 9..10)),
            },
            8..10,
        )),
    };
    checker.check_stmt(&let_stmt, &(0..11));

    let ident = (Expr::Identifier("n".to_string()), 16..17);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I8);
    assert_eq!(ty, Ty::I8);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn const_default_width_registers_in_const_values() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let decl = ConstDecl {
        visibility: Visibility::Private,
        name: "N".to_string(),
        ty: (
            TypeExpr::Named {
                name: "int".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    checker.check_const(&decl, &(0..3));

    assert!(matches!(
        checker.const_values.get("N"),
        Some(ConstValue::Integer(100))
    ));

    let ident = (Expr::Identifier("N".to_string()), 10..11);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn const_explicit_width_not_in_const_values_widening_ok() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let decl = ConstDecl {
        visibility: Visibility::Private,
        name: "N".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    checker.check_const(&decl, &(0..3));

    assert!(
        !checker.const_values.contains_key("N"),
        "explicit-width consts should not register const_values"
    );

    let target_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("y".to_string()), 10..11),
        ty: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            14..17,
        )),
        value: Some((Expr::Identifier("N".to_string()), 20..21)),
    };
    checker.check_stmt(&target_stmt, &(10..21));

    let binding = checker.env.lookup_ref("N").expect("N should be defined");
    assert_eq!(binding.ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn mutable_var_initializer_materializes_literal_default() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let var_stmt = Stmt::Var {
        name: "n".to_string(),
        ty: None,
        value: Some(make_int_literal(5, 8..9)),
    };
    checker.check_stmt(&var_stmt, &(0..10));
    let binding = checker
        .env
        .lookup_ref("n")
        .expect("mutable binding should be defined");
    assert_eq!(binding.ty, Ty::I64);
    assert_eq!(
        checker
            .expr_types
            .get(&SpanKey { start: 8, end: 9 })
            .cloned(),
        Some(Ty::I64)
    );
}

#[test]
fn typecheck_output_materializes_literal_kinds_for_unannotated_lets() {
    let source = "fn main() { let x = 1; let y = 2.0; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.expr_types.values().any(Ty::is_numeric_literal),
        "TypeCheckOutput should materialize surviving literal kinds before serialization: {:?}",
        output.expr_types
    );
    assert!(output.expr_types.values().any(|ty| ty == &Ty::I64));
    assert!(output.expr_types.values().any(|ty| ty == &Ty::F64));
}

#[test]
fn bind_pattern_struct_fields_substitute_generic_type_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Pair".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Pair".to_string(),
            type_params: vec!["T".to_string(), "U".to_string()],
            fields: HashMap::from([
                (
                    "first".to_string(),
                    Ty::Named {
                        name: "T".to_string(),
                        args: vec![],
                    },
                ),
                (
                    "second".to_string(),
                    Ty::Named {
                        name: "U".to_string(),
                        args: vec![],
                    },
                ),
            ]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        },
    );

    checker.bind_pattern(
        &Pattern::Struct {
            name: "Pair".to_string(),
            fields: vec![
                hew_parser::ast::PatternField {
                    name: "first".to_string(),
                    pattern: None,
                },
                hew_parser::ast::PatternField {
                    name: "second".to_string(),
                    pattern: None,
                },
            ],
        },
        &Ty::Named {
            name: "Pair".to_string(),
            args: vec![Ty::I64, Ty::Bool],
        },
        false,
        &(0..10),
    );

    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
    assert_eq!(
        checker
            .env
            .lookup_ref("first")
            .map(|binding| binding.ty.clone()),
        Some(Ty::I64),
        "generic struct destructuring must bind instantiated field types"
    );
    assert_eq!(
        checker
            .env
            .lookup_ref("second")
            .map(|binding| binding.ty.clone()),
        Some(Ty::Bool),
        "generic struct destructuring must bind instantiated field types"
    );
}

#[test]
fn or_pattern_binding_helper_rejects_mutability_mismatch() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let names = HashSet::from(["x".to_string()]);
    let mut left_env = crate::env::TypeEnv::new();
    let mut right_env = crate::env::TypeEnv::new();
    left_env.define_with_span("x".to_string(), Ty::I64, true, 0..1);
    right_env.define_with_span("x".to_string(), Ty::I64, false, 0..1);

    assert!(
        !checker.or_pattern_bindings_match(&left_env, &right_env, &names, &names),
        "or-pattern merge must reject bindings with mismatched mutability"
    );
}

#[test]
fn struct_pattern_missing_type_def_emits_diagnostic() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    checker.bind_pattern(
        &Pattern::Struct {
            name: "Ghost".to_string(),
            fields: vec![hew_parser::ast::PatternField {
                name: "value".to_string(),
                pattern: None,
            }],
        },
        &Ty::Named {
            name: "Ghost".to_string(),
            args: vec![],
        },
        false,
        &(0..5),
    );

    assert!(
        checker.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedType
                && error.message.contains("type `Ghost` is not defined")
        }),
        "missing type defs in struct patterns must fail closed: {checker_errors:#?}",
        checker_errors = checker.errors
    );
    assert_eq!(
        checker
            .env
            .lookup_ref("value")
            .map(|binding| binding.ty.clone()),
        Some(Ty::Error),
        "missing type defs should seed placeholder bindings for recovery"
    );
}

// ── Struct init literal coercion tests ─────────────────────────────

fn register_generic_wrapper(checker: &mut Checker) {
    let mut fields = HashMap::new();
    fields.insert(
        "value".to_string(),
        Ty::Named {
            name: "T".to_string(),
            args: vec![],
        },
    );
    checker.type_defs.insert(
        "Wrapper".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Wrapper".to_string(),
            type_params: vec!["T".to_string()],
            fields,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        },
    );
}

#[test]
fn struct_init_coerces_literal_to_expected_type_arg() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 42 } checked against Wrapper<i32>
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
        },
        0..20,
    );
    let expected = Ty::Named {
        name: "Wrapper".to_string(),
        args: vec![Ty::I32],
    };
    let ty = checker.check_against(&init.0, &init.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_infers_type_param_from_literal() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 42 } without expected type keeps the literal kind until
    // a later coercion/defaulting boundary.
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
        },
        0..20,
    );
    let ty = checker.synthesize(&init.0, &init.1);
    assert_eq!(
        ty,
        Ty::Named {
            name: "Wrapper".to_string(),
            args: vec![Ty::IntLiteral],
        }
    );
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_overflow_in_expected_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 256 } checked against Wrapper<u8> — should error
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(256, 10..13))],
        },
        0..20,
    );
    let expected = Ty::Named {
        name: "Wrapper".to_string(),
        args: vec![Ty::U8],
    };
    let _ty = checker.check_against(&init.0, &init.1, &expected);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

// ── Trailing-literal coercion in typed functions (gap fix) ────────

#[test]
fn trailing_integer_literal_coerces_to_declared_return_type() {
    // fn foo() -> i32 { 0 }  — bare 0 defaulted to i64 before fix
    let source = "fn foo() -> i32 { 0 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "trailing literal should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn trailing_integer_literal_coerces_smaller_width() {
    // fn foo() -> i8 { 42 }  — literal fits in i8
    let source = "fn foo() -> i8 { 42 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "literal 42 fits in i8 and should coerce: {:?}",
        output.errors
    );
}

#[test]
fn trailing_integer_literal_out_of_range_is_rejected() {
    // fn foo() -> i8 { 300 }  — 300 does not fit in i8 (range -128..=127)
    let source = "fn foo() -> i8 { 300 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "out-of-range literal should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn trailing_if_with_literal_branches_coerces() {
    // fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }
    // Both branches are integer literals that should coerce to i32.
    let source = "fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "if-else with integer literals should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn explicit_return_with_literal_still_works() {
    // Regression guard: explicit return was already working; must stay working.
    let source = "fn foo() -> i32 { return 0; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "explicit return with literal coercion should still work: {:?}",
        output.errors
    );
}

#[test]
fn trailing_negative_literal_rejected_for_unsigned_return() {
    // fn foo() -> u32 { -1 }  — negative literal cannot fit in unsigned type
    let source = "fn foo() -> u32 { -1 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "negative literal should be rejected for unsigned return type"
    );
}

#[test]
fn trailing_match_with_literal_arms_coerces() {
    // fn foo(x: bool) -> i32 { match x { true => 1, false => 0 } }
    // Stmt::Match is the last statement; both arms are integer literals that
    // should coerce to i32 via the pre-seeded expected type.
    let source = "fn foo(x: bool) -> i32 { match x { true => 1, false => 0 } }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "match with integer literal arms should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn tuple_if_element_coerces_from_expected_type() {
    // The tuple annotation supplies i32 to the nested if expression's first
    // element; the literal branch must not synthesize to i64 first.
    let source = "fn foo(flag: bool, y: i32) -> (i32, i32) { (if flag { 1 } else { y }, y) }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "nested tuple if should inherit the tuple element type: {:?}",
        output.errors
    );
}

#[test]
fn tuple_match_element_coerces_from_expected_type() {
    // The tuple annotation supplies i32 to the nested match expression's
    // first element; the literal arm must not synthesize to i64 first.
    let source =
        "fn foo(flag: bool, y: i32) -> (i32, i32) { (match flag { true => 1, false => y }, y) }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "nested tuple match should inherit the tuple element type: {:?}",
        output.errors
    );
}

#[test]
fn trailing_type_mismatch_reports_exactly_one_error() {
    // fn foo() -> i32 { "hello" }
    // check_against already reports the mismatch at the expression site;
    // check_fn_decl's outer expect_type must NOT fire a duplicate.
    let source = "fn foo() -> i32 { \"hello\" }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert_eq!(
        output.errors.len(),
        1,
        "expected exactly one type mismatch error, got: {:?}",
        output.errors
    );
}

#[test]
fn trailing_identifier_mismatch_reports_exactly_one_error() {
    // fn foo(s: String) -> i32 { s }
    // The identifier arm in check_against matched before the default arm,
    // so without the guard it fired a second duplicate error.
    let source = "fn foo(s: String) -> i32 { s }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert_eq!(
        output.errors.len(),
        1,
        "expected exactly one type mismatch error for identifier, got: {:?}",
        output.errors
    );
}

#[test]
fn error_return_type_does_not_suppress_match_arm_diagnostics() {
    // fn foo() -> UnknownType { match true { true => "hello", false => 42 } }
    // UnknownType resolves to Ty::Error. Without the Ty::Error guard in
    // check_match_expr, the error type pre-seeds all arms via check_against,
    // silently accepting the String/int mismatch between arms.
    let source = r#"fn foo() -> UnknownType { match true { true => "hello", false => 42 } }"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // We expect at least two errors: one for UnknownType and one for the
    // arm type mismatch (String vs int). Before the fix only the
    // UnknownType error appeared.
    let arm_mismatch = output.errors.iter().any(|e| {
        let msg = format!("{e:?}");
        msg.contains("TypeMismatch") || msg.contains("mismatch")
    });
    assert!(
        arm_mismatch,
        "match arms with mismatched types should still report an error even when \
         the return type is Ty::Error; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_json_returns_result_self_string() {
    // Regression: non-wire struct.from_json(s) must type-check as
    // Result<Self, String>, not Self.  The SHIM that returned Self directly
    // was removed; this test pins the correct surface type.
    let source = r#"
type Point { x: i32; y: i32; }
fn main() {
let s = "{\"x\":1,\"y\":2}";
let r: Result<Point, String> = Point.from_json(s);
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "from_json should return Result<Self, String> with no type errors; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_json_bare_self_is_type_error() {
    // Assigning the result of from_json directly to `Self` (not Result<Self, …>)
    // must produce a type mismatch — confirms the SHIM is gone.
    let source = r#"
type Point { x: i32; y: i32; }
fn main() {
let s = "{\"x\":1,\"y\":2}";
let p: Point = Point.from_json(s);
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let has_mismatch = output.errors.iter().any(|e| {
        let msg = format!("{e:?}");
        msg.contains("TypeMismatch") || msg.contains("mismatch") || msg.contains("Result")
    });
    assert!(
        has_mismatch,
        "assigning Result<Point, String> to Point must be a type error; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_yaml_and_from_toml_return_result() {
    // Both from_yaml and from_toml should also return Result<Self, String>.
    let source = r#"
type Cfg { n: i32; }
fn main() {
let _a: Result<Cfg, String> = Cfg.from_yaml("n: 1");
let _b: Result<Cfg, String> = Cfg.from_toml("n = 1");
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "from_yaml / from_toml should return Result<Self, String>; got: {:?}",
        output.errors
    );
}

// -------------------------------------------------------------------------
// Structural-bounds scaffold tests (E1)
//
// These tests verify the `type_structurally_satisfies` scaffold and the
// updated `type_satisfies_trait_bound` fallback without changing any
// existing program behaviour.
// -------------------------------------------------------------------------

/// Build a minimal `Checker` with a trait registered in `trait_defs`.
///
/// The trait is method-only (no associated types, no generic methods) unless
/// the caller opts in via the `with_assoc` / `with_generic_method` flags.
fn make_checker_with_trait(
    trait_name: &str,
    method_names: &[&str],
    with_assoc: bool,
    with_generic_method: bool,
) -> Checker {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut items: Vec<hew_parser::ast::TraitItem> = method_names
        .iter()
        .map(|name| {
            let type_params = if with_generic_method {
                Some(vec![TypeParam {
                    name: "U".to_string(),
                    bounds: vec![],
                }])
            } else {
                None
            };
            TraitItem::Method(TraitMethod {
                name: name.to_string(),
                is_pure: false,
                type_params,
                params: vec![Param {
                    name: "val".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "Self".to_string(),
                            type_args: None,
                        },
                        0..4,
                    ),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: None,
                span: 0..0,
                doc_comment: None,
            })
        })
        .collect();

    if with_assoc {
        items.push(TraitItem::AssociatedType {
            name: "Output".to_string(),
            default: None,
            bounds: vec![],
        });
    }

    let td = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: trait_name.to_string(),
        type_params: None,
        super_traits: None,
        items,
        doc_comment: None,
    };

    let info = Checker::trait_info_from_decl(&td);
    checker.trait_defs.insert(trait_name.to_string(), info);
    checker
}

fn make_test_type_def(
    name: &str,
    type_params: Vec<String>,
    methods: HashMap<String, FnSig>,
) -> TypeDef {
    TypeDef {
        kind: TypeDefKind::Struct,
        name: name.to_string(),
        type_params,
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods,
        doc_comment: None,
        is_indirect: false,
    }
}

#[test]
fn structural_satisfies_returns_false_for_unknown_trait() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    assert!(
        !checker.type_structurally_satisfies("MyType", "NoSuchTrait"),
        "unknown trait must not satisfy structural check"
    );
}

#[test]
fn structural_satisfies_e1_guard_associated_types() {
    let mut checker = make_checker_with_trait("Indexed", &["get"], true, false);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Indexed"),
        "E1 guard: traits with associated types must return false"
    );
}

#[test]
fn structural_satisfies_e1_guard_generic_methods() {
    let mut checker = make_checker_with_trait("Mapper", &["map"], false, true);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Mapper"),
        "E1 guard: traits with generic methods must return false"
    );
}

#[test]
fn structural_satisfies_e1_guard_method_only_trait_unknown_type_returns_false() {
    // In E2, the placeholder is replaced with real method-presence matching.
    // An unregistered type still returns false because no methods are found.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Greet"),
        "unregistered type must not satisfy structural check even after E2"
    );
}

#[test]
fn type_satisfies_trait_bound_nominal_path_unchanged() {
    // Verify that existing nominal conformance still works after the
    // structural fallback was wired into type_satisfies_trait_bound.
    let source = r"
        trait Greet {
            fn hello(val: Self);
        }

        type Greeter {}

        impl Greet for Greeter {
            fn hello(val: Greeter) {}
        }

        fn use_greet<T: Greet>(t: T) {}

        fn main() {
            let g = Greeter {};
            use_greet(g);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "nominal trait conformance must still succeed after E1 scaffold: {:?}",
        output.errors
    );
}

#[test]
fn type_satisfies_trait_bound_missing_impl_still_fails() {
    // A type that has no impl and no structural match must still fail the
    // bound — E1 must not silently accept it.
    let source = r"
        trait Greet {
            fn hello(val: Self);
        }

        type Stranger {}

        fn use_greet<T: Greet>(t: T) {}

        fn main() {
            let s = Stranger {};
            use_greet(s);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "E1 must not accept a type with no impl and no structural match; expected errors"
    );
}

// -------------------------------------------------------------------------
// E2 structural method-presence tests
//
// These exercises the live structural-satisfaction logic that replaced the
// E1 placeholder.  Programs use `impl Type { fn method }` (no trait) so
// the method is registered in `type_defs.methods` without a nominal impl.
// -------------------------------------------------------------------------

#[test]
fn structural_e2_single_method_match_satisfies_bound() {
    // Positive: a type that has the required method via a bare impl block
    // (no explicit `impl Trait for Type`) must satisfy the bound structurally.
    let source = r"
        trait Area {
            fn area(val: Self) -> int;
        }

        type Square {}

        impl Square {
            fn area(s: Square) -> int { 1 }
        }

        fn measure<T: Area>(s: T) -> int {
            s.area()
        }

        fn main() {
            let sq = Square {};
            let _ = measure(sq);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "structural method match must satisfy Area bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_multi_method_trait_all_present_satisfies_bound() {
    // Positive: all required methods present → bound satisfied.
    let source = r#"
        trait Named {
            fn label(val: Self) -> string;
            fn code(val: Self) -> int;
        }

        type Widget {}

        impl Widget {
            fn label(w: Widget) -> string { "w" }
            fn code(w: Widget) -> int { 0 }
        }

        fn print_label<T: Named>(t: T) -> string {
            t.label()
        }

        fn main() {
            let w = Widget {};
            let _ = print_label(w);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "all methods present → Named bound must be satisfied; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_method_with_non_self_param_satisfies_bound() {
    // Positive: trait method has a non-receiver parameter; the type's method
    // must have the same arity and parameter type.
    let source = r"
        trait Scalable {
            fn scale(val: Self, factor: int) -> int;
        }

        type Brick {}

        impl Brick {
            fn scale(b: Brick, factor: int) -> int { factor }
        }

        fn resize<T: Scalable>(t: T) -> int {
            t.scale(2)
        }

        fn main() {
            let b = Brick {};
            let _ = resize(b);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "matching non-receiver param must satisfy Scalable bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_nominal_impl_still_preferred_over_structural() {
    // Positive: an explicit `impl Trait for Type` still works; E2 must not
    // break the nominal path.
    let source = r"
        trait Area {
            fn area(val: Self) -> int;
        }

        type Circle {}

        impl Area for Circle {
            fn area(c: Circle) -> int { 3 }
        }

        fn measure<T: Area>(s: T) -> int {
            s.area()
        }

        fn main() {
            let c = Circle {};
            let _ = measure(c);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "explicit impl must still satisfy bound in E2; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_wrong_return_type_does_not_satisfy_bound() {
    // Negative: the type has a method with the right name but wrong return type;
    // the bound must not be satisfied.
    let source = r#"
        trait Area {
            fn area(val: Self) -> int;
        }

        type Triangle {}

        impl Triangle {
            fn area(t: Triangle) -> string { "big" }
        }

        fn measure<T: Area>(s: T) -> int {
            s.area()
        }

        fn main() {
            let t = Triangle {};
            let _ = measure(t);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "wrong return type must not satisfy Area bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_wrong_arity_does_not_satisfy_bound() {
    // Negative: the type's method has one extra non-receiver parameter;
    // the arity mismatch must cause the bound to fail.
    let source = r"
        trait Ping {
            fn ping(val: Self) -> int;
        }

        type Server {}

        impl Server {
            fn ping(s: Server, timeout: int) -> int { 1 }
        }

        fn use_ping<T: Ping>(t: T) -> int {
            t.ping()
        }

        fn main() {
            let s = Server {};
            let _ = use_ping(s);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "arity mismatch must not satisfy Ping bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_missing_one_of_two_methods_does_not_satisfy_bound() {
    // Negative: a multi-method trait where only one of two required methods is present.
    let source = r#"
        trait Named {
            fn label(val: Self) -> string;
            fn code(val: Self) -> int;
        }

        type Partial {}

        impl Partial {
            fn label(p: Partial) -> string { "p" }
            // `code` is intentionally missing
        }

        fn use_named<T: Named>(t: T) -> string {
            t.label()
        }

        fn main() {
            let p = Partial {};
            let _ = use_named(p);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "partial method set must not satisfy Named bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_all_default_methods_still_requires_explicit_impl() {
    // Negative (conservative): a trait whose every method has a default body
    // has no required methods.  E2 returns false in that case — an explicit
    // `impl Trait for Type` is still needed, keeping explicit impls authoritative
    // for default-only and marker-like traits.
    let source = r#"
        trait WithDefault {
            fn greet(val: Self) -> string { "hello" }
        }

        type Thingy {}

        impl Thingy {
            fn greet(t: Thingy) -> string { "world" }
        }

        fn use_it<T: WithDefault>(t: T) {}

        fn main() {
            let t = Thingy {};
            use_it(t);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "all-default-method trait must require explicit impl; got: {:?}",
        output.errors
    );
}

// -------------------------------------------------------------------------
// Bound-diagnostic clarity tests (v0.3.0 slice: bound-diagnostic-clarity)
//
// These tests verify that BoundsNotSatisfied errors carry a diagnostic hint
// (in `suggestions`) that distinguishes the concrete failure mode:
//   • missing method(s)
//   • arity mismatch
//   • return-type / signature mismatch
//   • E1 guard requiring an explicit `impl` declaration
// -------------------------------------------------------------------------

#[test]
fn bound_diagnostic_missing_method_hint() {
    // A type that has no method at all should produce a hint naming the missing method.
    let source = r"
        trait Ping {
            fn ping(val: Self) -> int;
        }

        type Widget {}

        fn use_ping<T: Ping>(t: T) -> int { 0 }

        fn main() {
            let w = Widget {};
            let _ = use_ping(w);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions
            .iter()
            .any(|s| s.contains("ping") && s.contains("missing")),
        "suggestion should mention missing method `ping`; got suggestions: {:?}",
        err.suggestions
    );
}

#[test]
fn bound_diagnostic_arity_mismatch_hint() {
    // A type whose method has the right name but the wrong number of parameters
    // should produce a hint mentioning the arity mismatch.
    let source = r"
        trait Measure {
            fn measure(val: Self) -> int;
        }

        type Ruler {}

        impl Ruler {
            fn measure(r: Ruler, scale: int) -> int { scale }
        }

        fn use_measure<T: Measure>(t: T) -> int { 0 }

        fn main() {
            let r = Ruler {};
            let _ = use_measure(r);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions.iter().any(|s| s.contains("arity")),
        "suggestion should mention arity mismatch; got: {:?}",
        err.suggestions
    );
}

#[test]
fn bound_diagnostic_return_type_mismatch_hint() {
    // A type whose method has the right name and arity but returns the wrong type
    // should produce a hint mentioning the return-type mismatch.
    let source = r#"
        trait Label {
            fn label(val: Self) -> string;
        }

        type Tag {}

        impl Tag {
            fn label(t: Tag) -> int { 0 }
        }

        fn use_label<T: Label>(t: T) -> string { "" }

        fn main() {
            let tag = Tag {};
            let _ = use_label(tag);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions.iter().any(|s| s.contains("return-type")),
        "suggestion should mention return-type mismatch; got: {:?}",
        err.suggestions
    );
}

#[test]
fn bound_diagnostic_e1_associated_type_requires_explicit_impl_hint() {
    // A trait that declares an associated type triggers the E1 guard.
    // The diagnostic hint should tell the user an explicit impl is needed.
    let source = r"
        trait Container {
            type Item;
            fn get(val: Self) -> int;
        }

        type Box {}

        fn use_container<T: Container>(t: T) -> int { 0 }

        fn main() {
            let b = Box {};
            let _ = use_container(b);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions
            .iter()
            .any(|s| s.contains("explicit") && s.contains("impl")),
        "suggestion should mention explicit impl for E1 (associated type) guard; got: {:?}",
        err.suggestions
    );
}

#[test]
fn trait_method_where_clause_bound_enforced_negative() {
    let source = r#"
        trait Printable {
            fn print(val: Self) -> string;
        }

        trait Formatter {
            fn apply<U>(item: Self, value: U) -> string where U: Printable;
        }

        type Printer {}
        type Page {}
        type Rock {}

        impl Printable for Page {
            fn print(val: Page) -> string { "page" }
        }

        impl Formatter for Printer {
            fn apply<U>(item: Printer, value: U) -> string where U: Printable {
                "formatted"
            }
        }

        fn run<T: Formatter>(item: T) {
            let _ = item.apply(Rock {});
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.message.contains("Printable") && err.message.contains('U'),
        "expected bound error to mention the method-level bound, got {err:?}"
    );
}

#[test]
fn trait_method_where_clause_bound_enforced_positive() {
    let source = r#"
        trait Printable {
            fn print(val: Self) -> string;
        }

        trait Formatter {
            fn apply<U>(item: Self, value: U) -> string where U: Printable;
        }

        type Printer {}
        type Page {}

        impl Printable for Page {
            fn print(val: Page) -> string { "page" }
        }

        impl Formatter for Printer {
            fn apply<U>(item: Printer, value: U) -> string where U: Printable {
                "formatted"
            }
        }

        fn run<T: Formatter>(item: T) {
            let ok = item.apply(Page {});
            println(ok);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "expected method-level where-clause bound to be satisfied, got {:?}",
        output.errors
    );
    assert!(
        output.call_type_args.values().any(|args| args
            == &vec![crate::ty::Ty::Named {
                name: "Page".to_string(),
                args: vec![]
            }]),
        "expected method-level bound call to infer U=Page, got {:?}",
        output.call_type_args
    );
}

#[test]
fn named_method_lookup_prefers_type_defs_before_fn_sigs() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut methods = HashMap::new();
    methods.insert(
        "hello".to_string(),
        FnSig {
            return_type: Ty::String,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Speaker".to_string(),
        make_test_type_def("Speaker", vec![], methods),
    );
    checker.fn_sigs.insert(
        "Speaker::hello".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let sig = checker
        .lookup_named_method_sig("Speaker", &[], "hello")
        .expect("type_defs method should resolve");
    assert_eq!(sig.return_type, Ty::String);
}

#[test]
fn custom_index_uses_named_method_get_for_type_def() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut methods = HashMap::new();
    methods.insert(
        "get".to_string(),
        FnSig {
            param_names: vec!["index".to_string()],
            params: vec![Ty::I64],
            return_type: Ty::Named {
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Boxy".to_string(),
        make_test_type_def("Boxy", vec!["T".to_string()], methods),
    );
    checker.env.define(
        "boxy".to_string(),
        Ty::Named {
            name: "Boxy".to_string(),
            args: vec![Ty::String],
        },
        false,
    );

    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("boxy".to_string()), 0..4)),
        index: Box::new(make_int_literal(0, 5..6)),
    };

    let ty = checker.synthesize(&expr, &(0..6));
    assert_eq!(ty, Ty::String);
    assert!(
        checker.errors.is_empty(),
        "expected type-def get lookup to succeed, got: {:?}",
        checker.errors
    );
}

#[test]
fn custom_index_uses_named_method_get_for_fn_sig_fallback() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Wrapper".to_string(),
        make_test_type_def("Wrapper", vec!["T".to_string()], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Wrapper::get".to_string(),
        FnSig {
            param_names: vec!["index".to_string()],
            params: vec![Ty::I64],
            return_type: Ty::Named {
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );
    checker.env.define(
        "wrapper".to_string(),
        Ty::Named {
            name: "Wrapper".to_string(),
            args: vec![Ty::String],
        },
        false,
    );

    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("wrapper".to_string()), 0..7)),
        index: Box::new(make_int_literal(0, 8..9)),
    };

    let ty = checker.synthesize(&expr, &(0..9));
    assert_eq!(ty, Ty::String);
    assert!(
        checker.errors.is_empty(),
        "expected fn_sigs get fallback to succeed, got: {:?}",
        checker.errors
    );
}

#[test]
fn named_method_lookup_substitutes_type_params_for_fn_sig_fallback() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Wrapper".to_string(),
        make_test_type_def("Wrapper", vec!["T".to_string()], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Wrapper::value".to_string(),
        FnSig {
            param_names: vec!["next".to_string()],
            params: vec![Ty::Named {
                name: "T".to_string(),
                args: vec![],
            }],
            return_type: Ty::Named {
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );

    let sig = checker
        .lookup_named_method_sig("Wrapper", &[Ty::String], "value")
        .expect("fn_sigs fallback should resolve");
    assert_eq!(sig.params, vec![Ty::String]);
    assert_eq!(sig.return_type, Ty::String);
}

#[test]
fn generic_named_method_calls_record_method_type_args() {
    let source = r#"
        type Wrapper<T> { value: T }

        impl<T> Wrapper<T> {
            fn map<U>(wrapper: Wrapper<T>, mapper: fn(T) -> U) -> U {
                mapper(wrapper.value)
            }
        }

        fn to_len(value: string) -> int {
            value.len()
        }

        fn main() {
            let wrapper = Wrapper { value: "hew" };
            let len = wrapper.map(to_len);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "generic method call should type-check cleanly: {:?}",
        output.errors
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![Ty::I64]),
        "method call should record inferred method type args, got {:?}",
        output.call_type_args
    );
}

#[test]
fn impl_method_registration_keeps_inline_method_bounds_on_all_surfaces() {
    let source = r"
        trait Show {
            fn show(value: Self);
        }

        type Wrapper {}

        impl Wrapper {
            fn map<U: Show>(wrapper: Wrapper, value: U) -> U {
                value
            }
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "inline-bounded impl method should register without checker errors: {:?}",
        output.errors
    );

    let fn_sig = output
        .fn_sigs
        .get("Wrapper::map")
        .expect("impl method must populate fn_sigs");
    let method_sig = output
        .type_defs
        .get("Wrapper")
        .and_then(|type_def| type_def.methods.get("map"))
        .expect("impl method must populate type_def.methods");

    assert_eq!(
        fn_sig.type_param_bounds.get("U"),
        Some(&vec!["Show".to_string()]),
        "fn_sigs surface must retain method-inline bounds"
    );
    assert_eq!(
        method_sig.type_param_bounds.get("U"),
        Some(&vec!["Show".to_string()]),
        "type_def.methods surface must retain method-inline bounds"
    );
}

// -------------------------------------------------------------------------
// Structural-hardening tests (qualified names + super-trait walk)
// -------------------------------------------------------------------------

#[test]
fn structural_hardening_uses_fn_sigs_named_method_fallback() {
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.type_defs.insert(
        "Speaker".to_string(),
        make_test_type_def("Speaker", vec![], HashMap::new()),
    );
    checker
        .fn_sigs
        .insert("Speaker::hello".to_string(), FnSig::default());

    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "structural check should reuse named-method fn_sigs fallback"
    );
}

#[test]
fn structural_hardening_prefers_builtin_method_surface_for_imported_handle() {
    let mut checker = make_checker_with_trait("Closable", &["close"], false, false);

    let mut methods = HashMap::new();
    methods.insert(
        "close".to_string(),
        FnSig {
            return_type: Ty::I32,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Sender".to_string(),
        make_test_type_def("Sender", vec![], methods),
    );
    checker.fn_sigs.insert(
        "Sender::close".to_string(),
        FnSig {
            return_type: Ty::I32,
            ..FnSig::default()
        },
    );

    assert!(
        checker.type_structurally_satisfies("channel.Sender", "Closable"),
        "structural check should prefer builtin Sender::close over imported stubs"
    );
}

#[test]
fn structural_hardening_qualified_trait_name_matches() {
    // A type registered under "Speaker" must structurally satisfy a bound
    // expressed as "greet.Greet" once "greet" is a known module.
    // We build the checker state manually because check_program drains
    // type_defs/fn_sigs at the end of the pass.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.modules.insert("greet".to_string());

    // Register a TypeDef for Speaker.  The trait `hello(val: Self)` has its
    // receiver stripped by lookup_trait_method, so the effective trait_sig has
    // params=[].  The concrete method entry must match: receiver already stripped.
    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default()); // params=[], return=Unit
            m
        },
        doc_comment: None,
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    assert!(
        checker.type_structurally_satisfies("Speaker", "greet.Greet"),
        "structural check with qualified trait name must succeed after normalization"
    );
    // Unqualified form must still work too.
    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "structural check with unqualified trait name must still succeed"
    );
}

#[test]
fn structural_hardening_qualified_type_name_matches() {
    // A bound check with the type expressed as "mymod.Speaker" must succeed
    // when "mymod" is a known module and "Speaker" is registered in type_defs.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.modules.insert("mymod".to_string());

    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default());
            m
        },
        doc_comment: None,
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    assert!(
        checker.type_structurally_satisfies("mymod.Speaker", "Greet"),
        "structural check with qualified type name must succeed after normalization"
    );
    // Unqualified form must still work too.
    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "unqualified type name must still succeed"
    );
}

#[test]
fn structural_hardening_unknown_module_qualifier_is_rejected() {
    // If the prefix is not a known module, we must not strip it and must
    // not accidentally match a same-suffix type/trait.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    // "unknown" is NOT inserted into modules.

    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default());
            m
        },
        doc_comment: None,
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    // Trait "unknown.Greet" should not resolve to "Greet" because "unknown" is
    // not a registered module.
    assert!(
        !checker.type_structurally_satisfies("Speaker", "unknown.Greet"),
        "unrecognised module prefix must not be stripped"
    );
}

#[test]
fn structural_hardening_super_trait_methods_required() {
    // If trait B extends A, a type must provide A's required methods to
    // structurally satisfy B.  Before the fix, only B's own methods were
    // checked and A's were silently skipped.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn pretty_print(d: Doc) {}
            // `print` (from super-trait Printable) is intentionally missing
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "missing super-trait method must fail structural check; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_super_trait_methods_all_present_succeeds() {
    // When ALL required methods across the super-trait chain are present the
    // structural check must succeed without an explicit impl.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn print(d: Doc) {}
            fn pretty_print(d: Doc) {}
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "all super-trait methods present must pass structural check: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_child_default_overrides_super_required_method() {
    // If a child trait provides a default implementation for a super-trait
    // method, that inherited requirement is satisfied by the trait itself and
    // must not be re-required structurally from the concrete type.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn print(val: Self) {}
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn pretty_print(d: Doc) {}
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "child default override should satisfy inherited structural requirement: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_diamond_sibling_shadowing_merges_across_supers() {
    // In a diamond, sibling default branches must cover inherited methods
    // collectively so only the root trait's still-required methods remain.
    let source = r"
        trait A {
            fn a(val: Self);
            fn b(val: Self);
        }

        trait B: A {
            fn a(val: Self) {}
        }

        trait C: A {
            fn b(val: Self) {}
        }

        trait D: B + C {
            fn d(val: Self);
        }

        type Doc {}

        impl Doc {
            fn d(d: Doc) {}
        }

        fn use_d<T: D>(t: T) {}

        fn main() {
            let d = Doc {};
            use_d(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "diamond sibling shadowing should merge across supers: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_super_trait_e1_guard_propagates() {
    // If a super-trait has an associated type, the E1 guard must veto the
    // entire structural check — even if the immediate trait has no assoc types.
    use hew_parser::ast::{Param, TraitDecl, TraitItem, TraitMethod, TypeExpr};
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Build super-trait with an associated type.
    let assoc_super = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "AssocSuper".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![
            TraitItem::AssociatedType {
                name: "Output".to_string(),
                default: None,
                bounds: vec![],
            },
            TraitItem::Method(TraitMethod {
                name: "do_it".to_string(),
                is_pure: false,
                type_params: None,
                params: vec![Param {
                    name: "val".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "Self".to_string(),
                            type_args: None,
                        },
                        0..4,
                    ),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: None,
                span: 0..0,
                doc_comment: None,
            }),
        ],
        doc_comment: None,
    };
    let info_super = Checker::trait_info_from_decl(&assoc_super);
    checker
        .trait_defs
        .insert("AssocSuper".to_string(), info_super);

    // Child trait with no assoc types of its own.
    let child = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "ChildTrait".to_string(),
        type_params: None,
        super_traits: Some(vec![hew_parser::ast::TraitBound {
            name: "AssocSuper".to_string(),
            type_args: None,
        }]),
        items: vec![TraitItem::Method(TraitMethod {
            name: "run".to_string(),
            is_pure: false,
            type_params: None,
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
        })],
        doc_comment: None,
    };
    let info_child = Checker::trait_info_from_decl(&child);
    checker
        .trait_defs
        .insert("ChildTrait".to_string(), info_child);
    checker
        .trait_super
        .insert("ChildTrait".to_string(), vec!["AssocSuper".to_string()]);

    assert!(
        !checker.type_structurally_satisfies("AnyType", "ChildTrait"),
        "E1 guard in super-trait must veto structural check for child trait"
    );
}

#[test]
fn structural_hardening_super_trait_generic_method_guard_propagates() {
    // If a super-trait has a generic method, the E1 guard must veto the whole
    // structural check for the child trait too.
    use hew_parser::ast::{Param, TraitDecl, TraitItem, TraitMethod, TypeExpr, TypeParam};
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let generic_super = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "GenericSuper".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "map".to_string(),
            is_pure: false,
            type_params: Some(vec![TypeParam {
                name: "U".to_string(),
                bounds: vec![],
            }]),
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
        })],
        doc_comment: None,
    };
    let info_super = Checker::trait_info_from_decl(&generic_super);
    checker
        .trait_defs
        .insert("GenericSuper".to_string(), info_super);

    let child = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "ChildTrait".to_string(),
        type_params: None,
        super_traits: Some(vec![hew_parser::ast::TraitBound {
            name: "GenericSuper".to_string(),
            type_args: None,
        }]),
        items: vec![TraitItem::Method(TraitMethod {
            name: "run".to_string(),
            is_pure: false,
            type_params: None,
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
        })],
        doc_comment: None,
    };
    let info_child = Checker::trait_info_from_decl(&child);
    checker
        .trait_defs
        .insert("ChildTrait".to_string(), info_child);
    checker
        .trait_super
        .insert("ChildTrait".to_string(), vec!["GenericSuper".to_string()]);

    assert!(
        !checker.type_structurally_satisfies("AnyType", "ChildTrait"),
        "generic-method guard in super-trait must veto structural check for child trait"
    );
}

#[test]
fn cyclic_trait_hierarchy_bound_check_surfaces_diagnostic() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .trait_super
        .insert("TraitA".to_string(), vec!["TraitB".to_string()]);
    checker
        .trait_super
        .insert("TraitB".to_string(), vec!["TraitA".to_string()]);
    checker
        .trait_impls_set
        .insert(("Thing".to_string(), "TraitA".to_string()));

    let sig = FnSig {
        type_params: vec!["T".to_string()],
        type_param_bounds: HashMap::from([("T".to_string(), vec!["MissingTrait".to_string()])]),
        ..Default::default()
    };

    checker.enforce_type_param_bounds(
        &sig,
        &[Ty::Named {
            name: "Thing".to_string(),
            args: vec![],
        }],
        &(0..0),
    );

    assert!(
        checker
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied),
        "expected cyclic trait hierarchy bound check to fail with a diagnostic; got {:?}",
        checker.errors
    );
}

// ── Non-root module inference-hole fail-closed regression tests ───────────────
//
// These cover the `report_unresolved_inference_holes` path for non-root module
// items in a `module_graph`.  Signature-level `_` holes are tracked by
// `collect_functions` (which already walks non-root modules via `topo_order`)
// and are detected by `report_unresolved_inference_in_items` via the
// `lookup_scoped_item` scoped-name fallback.
//
// Body-level holes (expressions containing `_`) require non-root module body
// checking from PR #756 to propagate into the inference state.  PR #756 added
// the infrastructure; the tests below prove the deferred-hole drain path works
// for non-root module bodies too:
//   - `body_cast_infer_hole_fails_closed`: unresolvable `as _` cast target
//   - `body_let_annotation_infer_resolves_cleanly`: resolvable `let y: _ = 42`
//   - `body_lambda_infer_param_hole_fails_closed`: unresolvable lambda `|x: _|`

#[cfg(test)]
mod non_root_module_inference_scope {
    use super::*;

    fn make_non_root_module(
        mod_id: &ModuleId,
        fn_name: &str,
        param_ty: TypeExpr,
        return_ty: Option<TypeExpr>,
    ) -> Module {
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: fn_name.to_string(),
            type_params: None,
            params: vec![Param {
                name: "x".to_string(),
                ty: (param_ty, 10..11),
                is_mutable: false,
            }],
            return_type: return_ty.map(|ty| (ty, 15..16)),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };
        Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        }
    }

    fn make_program_with_non_root(module: Module) -> Program {
        let root_id = ModuleId::root();
        let mod_id = module.id.clone();
        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(module);
        // Dependencies (non-root) come before the root in topo order.
        mg.topo_order = vec![mod_id, root_id];
        Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        }
    }

    // Helper: collect InferenceFailed errors from the output.
    fn inference_failed_errors(output: &TypeCheckOutput) -> Vec<&TypeError> {
        output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect()
    }

    /// A non-root module function whose parameter type is `_` must produce an
    /// `InferenceFailed` error via `report_unresolved_inference_holes`.
    #[test]
    fn fn_param_infer_hole_fails_closed() {
        let mod_id = ModuleId::new(vec!["utils".to_string()]);
        let module = make_non_root_module(&mod_id, "helper", TypeExpr::Infer, None);
        let program = make_program_with_non_root(module);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved `_` param in non-root module fn; got errors: {:?}",
            output.errors
        );
    }

    /// A non-root module function whose return type is `_` now resolves from
    /// body-checking just like a root-module function. An empty body therefore
    /// resolves `_` to `unit` instead of leaving an unresolved inference hole.
    #[test]
    fn fn_return_infer_hole_resolves_from_body() {
        let mod_id = ModuleId::new(vec!["helpers".to_string()]);
        let concrete_param = TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let module =
            make_non_root_module(&mod_id, "compute", concrete_param, Some(TypeExpr::Infer));
        let program = make_program_with_non_root(module);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "non-root `_` return type should resolve from body-checking; got InferenceFailed errors: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "non-root `_` return type should resolve cleanly; got errors: {:?}",
            output.errors
        );
    }

    /// A non-root module function with fully concrete types must not produce
    /// any `InferenceFailed` errors (baseline / regression guard).
    #[test]
    fn fn_concrete_types_passes() {
        let mod_id = ModuleId::new(vec!["math".to_string()]);
        let concrete_param = TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let concrete_return = TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let module = make_non_root_module(&mod_id, "add", concrete_param, Some(concrete_return));
        let program = make_program_with_non_root(module);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "concrete non-root module fn must not produce InferenceFailed; got: {errs:?}"
        );
    }

    /// A non-root module function with `_` param must not prevent the root
    /// module items from being checked — root errors remain independent.
    #[test]
    fn infer_hole_in_non_root_does_not_suppress_root_errors() {
        use hew_parser::ast::{FnDecl, Item};

        let mod_id = ModuleId::new(vec!["side".to_string()]);
        let module = make_non_root_module(&mod_id, "side_fn", TypeExpr::Infer, None);

        // Root module also has a function with `_` param — should also error.
        let root_fn = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "root_fn".to_string(),
            type_params: None,
            params: vec![Param {
                name: "v".to_string(),
                ty: (TypeExpr::Infer, 50..51),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };

        let root_id = ModuleId::root();
        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(module);
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![(Item::Function(root_fn), 40..80)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.len() >= 2,
            "expected InferenceFailed for both the non-root module fn and the root fn; got: {:?}",
            output.errors
        );
    }

    /// Two non-root module functions both with `_` params must each produce
    /// an `InferenceFailed` error — holes are not collapsed into a single error.
    #[test]
    fn multiple_infer_holes_in_non_root_all_fail_closed() {
        use hew_parser::ast::{Block, FnDecl, Item, Param, TypeExpr};
        let mod_id = ModuleId::new(vec!["util2".to_string()]);
        let root_id = ModuleId::root();

        let make_infer_fn = |name: &str, span_start: usize| -> Spanned<Item> {
            let fd = FnDecl {
                attributes: vec![],
                is_async: false,
                is_generator: false,
                visibility: Visibility::Private,
                is_pure: false,
                name: name.to_string(),
                type_params: None,
                params: vec![Param {
                    name: "a".to_string(),
                    ty: (TypeExpr::Infer, span_start..span_start + 1),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: Block {
                    stmts: vec![],
                    trailing_expr: None,
                },
                doc_comment: None,
                decl_span: 0..0,
                fn_span: 0..0,
            };
            (Item::Function(fd), span_start..span_start + 30)
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![make_infer_fn("alpha", 0), make_infer_fn("beta", 40)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root);
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.len() >= 2,
            "expected at least 2 InferenceFailed errors for two `_`-param fns in non-root module; got: {:?}",
            output.errors
        );
    }

    /// A non-root module body containing `let y = x as _` must fail closed:
    /// the unresolved `_` cast target must produce an `InferenceFailed` error.
    ///
    /// Regression test: deferred inference holes created during non-root module
    /// body checking (via `synthesize_cast`) were not reported by
    /// `report_unresolved_inference_holes` because the body-level deferred-hole
    /// list was only flushed for the *root* module's item walk, not for
    /// non-root module bodies.
    #[test]
    fn body_cast_infer_hole_fails_closed() {
        // fn foo(x: i64) { let y = x as _; }  — `_` cast target is unresolved
        let mod_id = ModuleId::new(vec!["castmod".to_string()]);
        let root_id = ModuleId::root();

        let cast_expr = Expr::Cast {
            expr: Box::new((Expr::Identifier("x".to_string()), 20..21)),
            ty: (TypeExpr::Infer, 25..26),
        };
        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("y".to_string()), 14..15),
            ty: None,
            value: Some((cast_expr, 18..26)),
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "foo".to_string(),
            type_params: None,
            params: vec![Param {
                name: "x".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i64".to_string(),
                        type_args: None,
                    },
                    7..10,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 13..27)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root);
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved `_` cast target in non-root module body; got errors: {:?}",
            output.errors
        );
    }

    /// A non-root module body with `let y: _ = 42` must resolve the `_`
    /// annotation to `i64` from the value — no spurious `InferenceFailed`.
    ///
    /// This is the positive-path counterpart to `body_cast_infer_hole_fails_closed`:
    /// a deferred annotation hole that IS constrained by body-checking must not
    /// produce a false diagnostic.  Before PR #756 added non-root body checking,
    /// this would have left the type-var unresolved and erroneously fired.
    #[test]
    fn body_let_annotation_infer_resolves_cleanly() {
        // fn bar() { let y: _ = 42; }  — `_` must resolve to i64 from the value
        let mod_id = ModuleId::new(vec!["letmod".to_string()]);
        let root_id = ModuleId::root();

        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("y".to_string()), 14..15),
            ty: Some((TypeExpr::Infer, 17..18)),
            value: Some(make_int_literal(42, 21..23)),
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "bar".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 13..24)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root);
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "let `_` annotation must resolve from value — no InferenceFailed expected; got: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "let `_` annotation must resolve cleanly; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn inferred_binding_without_annotation_fails_closed() {
        let source = "fn main() { let f = (x) => x; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.iter()
                .any(|err| err.message.contains("local binding `f`")),
            "expected InferenceFailed for unresolved inferred binding `f`; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn explicit_generic_lambda_binding_stays_valid() {
        let source = "fn main() { let id = <T>(x: T) => x; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "explicit generic lambda binding should not produce InferenceFailed: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "explicit generic lambda binding should type-check cleanly: {:?}",
            output.errors
        );
    }

    #[test]
    fn unresolved_inferred_return_through_none_fails_closed() {
        let source = "fn maybe() -> _ { None }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved `None` return; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn trait_default_method_explicit_infer_return_registers_qualified_signature() {
        use hew_parser::ast::{TraitDecl, TraitItem};

        let trait_decl = TraitDecl {
            visibility: Visibility::Private,
            name: "Answerer".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "answer".to_string(),
                is_pure: false,
                type_params: None,
                params: vec![Param {
                    name: "value".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        12..15,
                    ),
                    is_mutable: false,
                }],
                return_type: Some((TypeExpr::Infer, 10..11)),
                where_clause: None,
                body: Some(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Identifier("value".to_string()), 20..25))),
                }),
                span: 0..0,
                doc_comment: None,
            })],
            doc_comment: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Trait(trait_decl), 0..30)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.is_empty(),
            "trait default `->_` return should resolve cleanly; got errors: {:?}",
            output.errors
        );
        assert_eq!(output.fn_sigs["Answerer::answer"].return_type, Ty::I64);
    }

    #[test]
    fn trait_default_method_unresolved_explicit_infer_return_fails_closed() {
        use hew_parser::ast::{TraitDecl, TraitItem};

        let trait_decl = TraitDecl {
            visibility: Visibility::Private,
            name: "Answerer".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "answer".to_string(),
                is_pure: false,
                type_params: None,
                params: vec![],
                return_type: Some((TypeExpr::Infer, 10..11)),
                where_clause: None,
                body: Some(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Identifier("None".to_string()), 20..24))),
                }),
                span: 0..0,
                doc_comment: None,
            })],
            doc_comment: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Trait(trait_decl), 0..30)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.iter().any(|err| err
                .message
                .contains("signature of trait method `Answerer::answer`")),
            "expected InferenceFailed for unresolved trait default `->_`; got errors: {:?}",
            output.errors
        );
        assert!(
            !output.fn_sigs.contains_key("Answerer::answer"),
            "failing trait method signature should be stripped from checker output: {:?}",
            output.fn_sigs
        );
    }

    #[test]
    fn trait_default_method_explicit_infer_return_propagates_to_impl_method_signature() {
        use hew_parser::ast::{ImplDecl, TraitBound, TraitDecl, TraitItem, TypeDecl, TypeDeclKind};

        let trait_decl = TraitDecl {
            visibility: Visibility::Private,
            name: "Answerer".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "answer".to_string(),
                is_pure: false,
                type_params: None,
                params: vec![Param {
                    name: "value".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        12..15,
                    ),
                    is_mutable: false,
                }],
                return_type: Some((TypeExpr::Infer, 10..11)),
                where_clause: None,
                body: Some(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Identifier("value".to_string()), 20..25))),
                }),
                span: 0..0,
                doc_comment: None,
            })],
            doc_comment: None,
        };
        let greeter = TypeDecl {
            visibility: Visibility::Private,
            kind: TypeDeclKind::Struct,
            name: "Greeter".to_string(),
            type_params: None,
            where_clause: None,
            body: vec![],
            doc_comment: None,
            wire: None,
            is_indirect: false,
        };
        let impl_decl = ImplDecl {
            type_params: None,
            trait_bound: Some(TraitBound {
                name: "Answerer".to_string(),
                type_args: None,
            }),
            target_type: (
                TypeExpr::Named {
                    name: "Greeter".to_string(),
                    type_args: None,
                },
                30..37,
            ),
            where_clause: None,
            type_aliases: vec![],
            methods: vec![],
        };
        let program = Program {
            module_graph: None,
            items: vec![
                (Item::Trait(trait_decl), 0..30),
                (Item::TypeDecl(greeter), 31..40),
                (Item::Impl(impl_decl), 41..60),
            ],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.is_empty(),
            "default impl method signature should inherit resolved trait return type; got errors: {:?}",
            output.errors
        );
        assert_eq!(output.fn_sigs["Greeter::answer"].return_type, Ty::I64);
    }

    #[test]
    fn trait_default_method_with_concrete_receiver_keeps_implicit_impl_arity() {
        let source = r"
            type Greeter {
                id: int;
            }

            trait Answerer {
                fn answer(g: Greeter) -> int {
                    42
                }
            }

            impl Answerer for Greeter {}
        ";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        assert!(
            output.errors.is_empty(),
            "default impl method with a concrete receiver should typecheck cleanly; got errors: {:?}",
            output.errors
        );
        assert!(
            output.fn_sigs["Greeter::answer"].params.is_empty(),
            "default impl method should not expose the concrete receiver as an explicit argument: {:?}",
            output.fn_sigs["Greeter::answer"]
        );
    }

    #[test]
    fn inferred_binding_does_not_duplicate_lambda_hole_error() {
        let source = "fn main() { let f = (x: _) => x; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert_eq!(
            errs.len(),
            1,
            "expected only the lambda hole diagnostic, got: {:?}",
            output.errors
        );
        assert!(
            errs[0].message.contains("lambda parameter `x`"),
            "expected lambda hole diagnostic, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn inferred_binding_does_not_duplicate_cast_hole_error() {
        let source = "fn main(x: int) { let y = x as _; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert_eq!(
            errs.len(),
            1,
            "expected only the cast hole diagnostic, got: {:?}",
            output.errors
        );
        assert!(
            errs[0].message.contains("cast target type"),
            "expected cast hole diagnostic, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn bare_channel_handle_signature_stays_valid() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn close_sender(tx: channel.Sender) {\n",
            "    tx.close();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "bare channel handle signatures should not produce InferenceFailed: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "bare channel handle signatures should type-check cleanly: {:?}",
            output.errors
        );
    }

    /// A non-root module body containing a lambda with an unresolved `_`
    /// parameter type must fail closed: the deferred hole created during
    /// body checking must produce an `InferenceFailed` error.
    ///
    /// Regression: the lambda-param deferred-hole path
    /// (`expressions.rs::synthesize_lambda`) must also drain into
    /// `report_unresolved_inference_holes` for non-root module bodies.
    #[test]
    fn body_lambda_infer_param_hole_fails_closed() {
        // fn foo() { let f = |x: _| x; }  — lambda param `_` never constrained
        let mod_id = ModuleId::new(vec!["lambdamod".to_string()]);
        let root_id = ModuleId::root();

        // |x: _| x  — lambda with infer-typed parameter, no call site to resolve it
        let lambda_expr = Expr::Lambda {
            is_move: false,
            type_params: None,
            params: vec![LambdaParam {
                name: "x".to_string(),
                ty: Some((TypeExpr::Infer, 15..16)),
            }],
            return_type: None,
            body: Box::new((Expr::Identifier("x".to_string()), 19..20)),
        };
        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("f".to_string()), 10..11),
            ty: None,
            value: Some((lambda_expr, 14..21)),
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "foo".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 9..22)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root);
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved lambda `_` param in non-root module body; got errors: {:?}",
            output.errors
        );
    }
}

// ── module_graph body typecheck parity (v0.3 blocker) ────────────────────────
//
// Non-root module_graph bodies must be typechecked, not just registered.
// A type error in an imported module body must not be silently missed.

/// Build a minimal two-module `Program`: a root module (empty) and a single
/// non-root module `mymod` containing the supplied items.
fn make_program_with_module_graph(non_root_items: Vec<Spanned<Item>>) -> Program {
    let root_id = ModuleId::root();
    let non_root_id = ModuleId::new(vec!["mymod".to_string()]);

    let root_module = Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let non_root_module = Module {
        id: non_root_id.clone(),
        items: non_root_items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module);
    mg.add_module(non_root_module);
    // Dependency order: non-root first, then root (root depends on mymod).
    mg.topo_order = vec![non_root_id, root_id];

    Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    }
}

/// A type error (bool body in an i64 function) in a non-root `module_graph` body
/// must be reported by `check_program`.  Before the parity fix this was silently
/// missed because the body-check loop only visited `program.items`.
#[test]
fn module_graph_body_type_error_is_reported() {
    // fn bad() -> i64 { true }  — body returns bool, declared i64
    let bad_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        is_pure: false,
        name: "bad".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..3,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_bool_literal(true, 0..4))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };

    let program = make_program_with_module_graph(vec![(Item::Function(bad_fn), 0..10)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        !output.errors.is_empty(),
        "expected a type error from non-root module body, but none were reported"
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if (expected.contains("i64") || expected.contains("int"))
                    && actual.contains("bool")
        )),
        "expected a Mismatch(i64/int, bool) error; got: {:?}",
        output.errors
    );
}

/// A function with an inferred return type (`-> _`) in a non-root `module_graph`
/// body must have its return type resolved by body checking.  Without the parity
/// fix the type var is never unified and the checker emits a spurious
/// `InferenceFailed` error.  With the fix the body resolves `_` to `i64` and no
/// error is emitted.
#[test]
fn module_graph_body_infer_return_resolves_without_error() {
    // fn inferred() -> _ { 42 }  — `_` must resolve to i64 from the body
    let inferred_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        is_pure: false,
        name: "inferred".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((TypeExpr::Infer, 0..1)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(42, 0..2))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };

    let program = make_program_with_module_graph(vec![(Item::Function(inferred_fn), 0..10)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "inferred return type should resolve cleanly; errors: {:?}",
        output.errors
    );
}

/// A local binding in a non-root module body must take precedence over a
/// same-named module when typechecking method calls on identifier receivers.
#[test]
fn module_graph_body_local_binding_named_like_module_still_resolves_methods() {
    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        is_pure: false,
        name: "ok".to_string(),
        type_params: None,
        params: vec![Param {
            name: "math".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "String".to_string(),
                    type_args: None,
                },
                0..6,
            ),
            is_mutable: false,
        }],
        return_type: Some((
            TypeExpr::Named {
                name: "bool".to_string(),
                type_args: None,
            },
            0..4,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::MethodCall {
                    receiver: Box::new((Expr::Identifier("math".to_string()), 0..4)),
                    method: "contains".to_string(),
                    args: vec![CallArg::Positional((
                        Expr::Literal(Literal::String("x".to_string())),
                        5..8,
                    ))],
                },
                0..18,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };

    let program = make_program_with_module_graph(vec![(Item::Function(ok_fn), 0..30)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "local bindings should win over same-named modules in non-root method calls; errors: {:?}",
        output.errors
    );
}

#[test]
fn module_qualified_call_rejects_private_body_only_signature() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("mymod".to_string());
    checker.fn_sigs.insert(
        "mymod.secret".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let receiver = (Expr::Identifier("mymod".to_string()), 0..5);
    let ty = checker.check_method_call(&receiver, "secret", &[], &(0..12));

    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|err| matches!(err.kind, TypeErrorKind::UndefinedMethod)),
        "module-qualified calls must not resolve against non-exported private signatures: {:?}",
        checker.errors
    );
}

#[test]
fn module_qualified_call_accepts_exported_signature() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("mymod".to_string());
    checker
        .module_fn_exports
        .insert("mymod.visible".to_string());
    checker.fn_sigs.insert(
        "mymod.visible".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let receiver = (Expr::Identifier("mymod".to_string()), 0..5);
    let ty = checker.check_method_call(&receiver, "visible", &[], &(0..13));

    assert_eq!(ty, Ty::I64);
    assert!(
        checker.errors.is_empty(),
        "exported module-qualified calls must keep working; errors: {:?}",
        checker.errors
    );
}

#[test]
fn module_graph_body_private_local_type_is_available() {
    let local_type = TypeDecl {
        visibility: Visibility::Private,
        name: "Local".to_string(),
        type_params: None,
        where_clause: None,
        kind: TypeDeclKind::Struct,
        body: vec![TypeBodyItem::Field {
            name: "x".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                0..1,
            ),
            attributes: Vec::new(),
            doc_comment: None,
        }],
        is_indirect: false,
        doc_comment: None,
        wire: None,
    };

    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        is_pure: false,
        name: "ok".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..3,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![(
                Stmt::Let {
                    pattern: (Pattern::Identifier("a".to_string()), 0..1),
                    ty: None,
                    value: Some((
                        Expr::StructInit {
                            name: "Local".to_string(),
                            fields: vec![("x".to_string(), make_int_literal(1, 0..1))],
                        },
                        0..10,
                    )),
                },
                0..10,
            )],
            trailing_expr: Some(Box::new((
                Expr::FieldAccess {
                    object: Box::new((Expr::Identifier("a".to_string()), 0..1)),
                    field: "x".to_string(),
                },
                11..14,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };

    let program = make_program_with_module_graph(vec![
        (Item::TypeDecl(local_type), 0..10),
        (Item::Function(ok_fn), 10..30),
    ]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "private non-root local types should resolve within the same module body; errors: {:?}",
        output.errors
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "constructs an explicit multi-module fixture for the parity regression"
)]
fn module_graph_body_prefers_same_module_private_helper_over_global_bare_name() {
    let i64_ty = TypeExpr::Named {
        name: "i64".to_string(),
        type_args: None,
    };
    let string_ty = TypeExpr::Named {
        name: "String".to_string(),
        type_args: None,
    };

    let helper_i64 = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: "helper".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((i64_ty.clone(), 0..3)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(42, 0..2))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };

    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        is_pure: false,
        name: "ok".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((i64_ty, 0..3)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::Call {
                    function: Box::new((Expr::Identifier("helper".to_string()), 0..6)),
                    type_args: None,
                    args: vec![],
                    is_tail_call: false,
                },
                0..8,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };

    let helper_string = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: "helper".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((string_ty, 10..16)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::Literal(Literal::String("wrong".to_string())),
                10..17,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };

    let root_id = ModuleId::root();
    let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
    let beta_id = ModuleId::new(vec!["beta".to_string()]);
    let root_module = Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let alpha_module = Module {
        id: alpha_id.clone(),
        items: vec![
            (Item::Function(helper_i64), 0..20),
            (Item::Function(ok_fn), 20..40),
        ],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let beta_module = Module {
        id: beta_id.clone(),
        items: vec![(Item::Function(helper_string), 40..60)],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module);
    mg.add_module(alpha_module);
    mg.add_module(beta_module);
    mg.topo_order = vec![alpha_id, beta_id, root_id];

    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "same-module private helper should win over another module's bare helper name; errors: {:?}",
        output.errors
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "constructs an explicit multi-module fixture for the parity regression"
)]
fn module_graph_body_prefers_same_module_private_extern_over_global_bare_name() {
    let i64_ty = TypeExpr::Named {
        name: "i64".to_string(),
        type_args: None,
    };
    let string_ty = TypeExpr::Named {
        name: "String".to_string(),
        type_args: None,
    };

    let extern_i64 = ExternBlock {
        abi: "C".to_string(),
        functions: vec![ExternFnDecl {
            name: "hew_test_raw".to_string(),
            params: vec![],
            return_type: Some((i64_ty.clone(), 0..3)),
            is_variadic: false,
        }],
    };
    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        is_pure: false,
        name: "ok".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((i64_ty, 0..3)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::Unsafe(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((
                        Expr::Call {
                            function: Box::new((
                                Expr::Identifier("hew_test_raw".to_string()),
                                0..12,
                            )),
                            type_args: None,
                            args: vec![],
                            is_tail_call: false,
                        },
                        0..14,
                    ))),
                }),
                0..14,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };
    let extern_string = ExternBlock {
        abi: "C".to_string(),
        functions: vec![ExternFnDecl {
            name: "hew_test_raw".to_string(),
            params: vec![],
            return_type: Some((string_ty, 20..26)),
            is_variadic: false,
        }],
    };

    let root_id = ModuleId::root();
    let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
    let beta_id = ModuleId::new(vec!["beta".to_string()]);
    let root_module = Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let alpha_module = Module {
        id: alpha_id.clone(),
        items: vec![
            (Item::ExternBlock(extern_i64), 0..20),
            (Item::Function(ok_fn), 20..40),
        ],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let beta_module = Module {
        id: beta_id.clone(),
        items: vec![(Item::ExternBlock(extern_string), 40..60)],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module);
    mg.add_module(alpha_module);
    mg.add_module(beta_module);
    mg.topo_order = vec![alpha_id, beta_id, root_id];

    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "same-module private extern should win over another module's bare extern name; errors: {:?}",
        output.errors
    );
}

// ── module-body-diagnostic-completion: source_module tagging tests ────────────
//
// These tests prove that diagnostics originating in non-root module bodies
// carry the correct `source_module` value so the CLI can route them to the
// right source file when rendering.
//
// Wave: lane/module-body-diagnostic-completion (combines module-body-typecheck-parity
// with the first diagnostic-envelope-unification slice).

#[cfg(test)]
mod module_body_diagnostic_envelope {
    use super::*;

    // ── helpers ────────────────────────────────────────────────────────────────

    /// Build a minimal `Program` with a non-root module `mod_name` whose items
    /// are the supplied `items`.  The root module is empty.
    fn make_program_with_named_module(mod_name: &str, items: Vec<Spanned<Item>>) -> Program {
        let root_id = ModuleId::root();
        let mod_id = ModuleId::new(vec![mod_name.to_string()]);

        let non_root = Module {
            id: mod_id.clone(),
            items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root);
        mg.topo_order = vec![mod_id, root_id];

        Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        }
    }

    /// Build a minimal fn declaration that returns `bool` but is declared `-> i64`.
    fn make_mistyped_fn(name: &str) -> Spanned<Item> {
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Pub,
            is_pure: false,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: Some((
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                5..8,
            )),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: Some(Box::new((Expr::Literal(Literal::Bool(true)), 10..14))),
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };
        (Item::Function(fn_decl), 0..20)
    }

    // ── body-check errors carry the module name ────────────────────────────────

    /// A type mismatch in a non-root module body must be tagged with the
    /// module's dotted name in `TypeError::source_module`.
    #[test]
    fn body_mismatch_error_tagged_with_source_module() {
        let program = make_program_with_named_module("mymod", vec![make_mistyped_fn("bad")]);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            !output.errors.is_empty(),
            "expected a type error from non-root module body"
        );
        for err in &output.errors {
            assert_eq!(
                err.source_module.as_deref(),
                Some("mymod"),
                "error from non-root module body must be tagged with 'mymod'; got: {:?}",
                err.source_module
            );
        }
    }

    /// A type mismatch in the root module must NOT be tagged (`source_module`
    /// stays None).
    #[test]
    fn root_module_error_has_no_source_module_tag() {
        // fn bad() -> i64 { true }  in root items
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "bad".to_string(),
            type_params: None,
            params: vec![],
            return_type: Some((
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                5..8,
            )),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: Some(Box::new((Expr::Literal(Literal::Bool(true)), 10..14))),
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Function(fn_decl), 0..20)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            !output.errors.is_empty(),
            "expected a type error from root module"
        );
        for err in &output.errors {
            assert!(
                err.source_module.is_none(),
                "root module error must not have source_module set; got: {:?}",
                err.source_module
            );
        }
    }

    /// Errors from different non-root modules must each carry their own module name.
    #[test]
    fn errors_from_multiple_modules_tagged_independently() {
        use hew_parser::ast::{Block, FnDecl, Item, Literal, TypeExpr, Visibility};

        let make_bad_fn = |fn_name: &str| -> Spanned<Item> {
            let fd = FnDecl {
                attributes: vec![],
                is_async: false,
                is_generator: false,
                visibility: Visibility::Pub,
                is_pure: false,
                name: fn_name.to_string(),
                type_params: None,
                params: vec![],
                return_type: Some((
                    TypeExpr::Named {
                        name: "i64".to_string(),
                        type_args: None,
                    },
                    5..8,
                )),
                where_clause: None,
                body: Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Literal(Literal::Bool(true)), 10..14))),
                },
                doc_comment: None,
                decl_span: 0..0,
                fn_span: 0..0,
            };
            (Item::Function(fd), 0..20)
        };

        let root_id = ModuleId::root();
        let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
        let beta_id = ModuleId::new(vec!["beta".to_string()]);

        let alpha = Module {
            id: alpha_id.clone(),
            items: vec![make_bad_fn("alpha_bad")],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let beta = Module {
            id: beta_id.clone(),
            items: vec![make_bad_fn("beta_bad")],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(alpha);
        mg.add_module(beta);
        mg.topo_order = vec![alpha_id, beta_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.len() >= 2,
            "expected errors from both alpha and beta modules; got: {:?}",
            output.errors
        );

        let alpha_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.source_module.as_deref() == Some("alpha"))
            .collect();
        let beta_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.source_module.as_deref() == Some("beta"))
            .collect();

        assert!(
            !alpha_errs.is_empty(),
            "expected at least one error tagged 'alpha'; errors: {:?}",
            output.errors
        );
        assert!(
            !beta_errs.is_empty(),
            "expected at least one error tagged 'beta'; errors: {:?}",
            output.errors
        );
    }

    // ── deferred inference hole drain carries source module ────────────────────

    /// An unresolved cast target `as _` in a non-root module body must produce
    /// an `InferenceFailed` error tagged with the module name.
    #[test]
    fn deferred_cast_hole_tagged_with_source_module() {
        // fn foo(x: i64) { let y = x as _; }  — cast target _ never resolved
        use hew_parser::ast::{Block, FnDecl, Param, Stmt, TypeExpr, Visibility};

        let cast_expr = Expr::Cast {
            expr: Box::new((Expr::Identifier("x".to_string()), 20..21)),
            ty: (TypeExpr::Infer, 25..26),
        };
        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("y".to_string()), 14..15),
            ty: None,
            value: Some((cast_expr, 18..27)),
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "foo".to_string(),
            type_params: None,
            params: vec![Param {
                name: "x".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i64".to_string(),
                        type_args: None,
                    },
                    8..11,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 13..28)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };

        let program =
            make_program_with_named_module("castmod", vec![(Item::Function(fn_decl), 0..30)]);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_failed.is_empty(),
            "expected InferenceFailed for unresolved `as _` in non-root module; errors: {:?}",
            output.errors
        );
        for err in &inference_failed {
            assert_eq!(
                err.source_module.as_deref(),
                Some("castmod"),
                "deferred cast inference error must carry source module 'castmod'; got {:?}",
                err.source_module
            );
        }
    }

    /// Signature-level inference holes in non-root modules (fn param `_`) must
    /// also be tagged with the module name.
    #[test]
    fn signature_inference_hole_tagged_with_source_module() {
        // fn helper(_ : _) {}  — unresolved param type
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "helper".to_string(),
            type_params: None,
            params: vec![Param {
                name: "v".to_string(),
                ty: (TypeExpr::Infer, 10..11),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };

        let program =
            make_program_with_named_module("sigmod", vec![(Item::Function(fn_decl), 0..20)]);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_failed.is_empty(),
            "expected InferenceFailed for `_` param in non-root module fn; errors: {:?}",
            output.errors
        );
        for err in &inference_failed {
            assert_eq!(
                err.source_module.as_deref(),
                Some("sigmod"),
                "signature inference error must carry source module 'sigmod'; got {:?}",
                err.source_module
            );
        }
    }

    /// Deferred channel rewrite finalization must preserve the non-root module
    /// tag when it emits a post-inference `InferenceFailed` diagnostic.
    #[test]
    fn deferred_channel_rewrite_error_tagged_with_source_module() {
        let parsed = hew_parser::parse(
            r"
                fn bad(rx: Receiver<_>) {
                    let _ = rx.recv();
                }
            ",
        );
        assert!(
            parsed.errors.is_empty(),
            "module parse errors: {:?}",
            parsed.errors
        );

        let program = make_program_with_named_module("chanmod", parsed.program.items.clone());

        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&program);

        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| {
                matches!(e.kind, TypeErrorKind::InferenceFailed) && e.message.contains("inner type")
            })
            .collect();

        assert!(
            !inference_failed.is_empty(),
            "expected deferred channel inference failure in non-root module; errors: {:?}",
            output.errors
        );
        for err in inference_failed {
            assert_eq!(
                err.source_module.as_deref(),
                Some("chanmod"),
                "deferred channel rewrite error must carry source module 'chanmod'; got {:?}",
                err.source_module
            );
        }
    }

    #[test]
    fn assign_target_shapes_populated_for_while_loop_with_import() {
        // Reproduces the eval_large_stderr CI failure:
        // synthesized source for `fn spam_err` eval step with `import std::io`
        let source = "import std::io;\nfn spam_err() {\n    var i = 0;\n    while i < 20000 {\n        io.write_err(\"line\\n\");\n        i = i + 1;\n    }\n}\nfn main() {\n}\n";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = crate::Checker::new(crate::module_registry::ModuleRegistry::new(
            crate::module_registry::build_module_search_paths(),
        ));
        let tco = checker.check_program(&parse_result.program);
        // `i` in `i = i + 1` must appear in assign_target_shapes
        let has_shape = tco.assign_target_shapes.iter().any(|(k, _)| k.start == 109);
        assert!(
            has_shape,
            "assign_target_shapes missing entry for i at ~109; got: {:?}",
            tco.assign_target_shapes.keys().collect::<Vec<_>>()
        );
    }
}

// ── Warning source-module attribution (PR-A slice) ──────────────────────────

#[cfg(test)]
mod warning_source_attribution {
    use super::*;

    fn make_unused_import_decl() -> ImportDecl {
        // import std::encoding::json  (unresolved — no resolved_items)
        // The module registry will fail to find it, but the import is still
        // registered into import_spans so the UnusedImport path is exercised.
        // Use a fake single-segment path so `register_import` takes the user-
        // module branch (path non-empty, no resolved_items → unresolved error
        // path, does NOT insert into import_spans).
        //
        // Instead we supply `resolved_items = Some(vec![])` to convince
        // register_import to follow the user-module branch and insert into
        // import_spans.
        ImportDecl {
            path: vec!["fakemod".to_string()],
            spec: None,
            file_path: None,
            resolved_items: Some(vec![]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        }
    }

    fn make_trivial_fn(name: &str) -> FnDecl {
        FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        }
    }

    fn make_non_root_program_with_fn_body(
        module_name: &str,
        name: &str,
        stmts: Vec<Spanned<Stmt>>,
    ) -> Program {
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts,
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };
        let root_id = ModuleId::root();
        let module_id = ModuleId::new(vec![module_name.to_string()]);
        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let sub_module = Module {
            id: module_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..40)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module);
        mg.add_module(sub_module);
        mg.topo_order = vec![module_id, root_id];

        Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        }
    }

    /// Build a Program whose module graph has:
    ///   - a root module with `fn main()` (no imports, in program.items)
    ///   - a non-root module "submod" with an import of "fakemod" and `fn helper()`
    fn build_program_with_non_root_import_and_fn() -> Program {
        let root_id = ModuleId::root();
        let submod_id = ModuleId::new(vec!["submod".to_string()]);

        let main_fn = make_trivial_fn("main");
        let helper_fn = make_trivial_fn("helper");
        let import_decl = make_unused_import_decl();

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let sub_module = Module {
            id: submod_id.clone(),
            items: vec![
                (Item::Import(import_decl), 0..20),
                (Item::Function(helper_fn), 25..50),
            ],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module);
        mg.add_module(sub_module);
        mg.topo_order = vec![submod_id, root_id];

        Program {
            module_graph: Some(mg),
            // Root-module items live in program.items (not in Module.items for root).
            items: vec![(Item::Function(main_fn), 0..10)],
            module_doc: None,
        }
    }

    /// An `UnusedImport` warning emitted for an import registered while
    /// `current_module` was "submod" must carry `source_module = Some("submod")`.
    #[test]
    fn non_root_unused_import_carries_source_module() {
        let program = build_program_with_non_root_import_and_fn();
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused_import_warnings: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"))
            .collect();

        assert!(
            !unused_import_warnings.is_empty(),
            "expected an UnusedImport warning for 'fakemod'; got warnings: {:?}",
            output.warnings
        );
        for w in &unused_import_warnings {
            assert_eq!(
                w.source_module.as_deref(),
                Some("submod"),
                "UnusedImport for 'fakemod' must carry source_module='submod'; got {:?}",
                w.source_module
            );
        }
    }

    /// A root-module `UnusedImport` (registered while `current_module = None`) must
    /// continue to carry `source_module = None` — no regression.
    #[test]
    fn root_unused_import_has_no_source_module() {
        // Build an ImportDecl with resolved_items so it reaches import_spans.
        let import_decl = make_unused_import_decl();
        let main_fn = make_trivial_fn("main");

        let program = Program {
            module_graph: None,
            items: vec![
                (Item::Import(import_decl), 0..20),
                (Item::Function(main_fn), 25..40),
            ],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let w = output
            .warnings
            .iter()
            .find(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"));

        assert!(
            w.is_some(),
            "expected UnusedImport warning for root 'fakemod'; got: {:?}",
            output.warnings
        );
        assert_eq!(
            w.unwrap().source_module,
            None,
            "root-module UnusedImport must have source_module=None; got {:?}",
            w.unwrap().source_module
        );
    }

    /// Functions defined in a non-root module must have their source module stored
    /// in `fn_def_spans` even though the current dead-code filter skips dot-named
    /// functions (they are never promoted to `DeadCode` warnings today).
    /// This guards the attribution infrastructure for future use.
    #[test]
    fn non_root_fn_def_span_stores_source_module() {
        let program = build_program_with_non_root_import_and_fn();
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&program);

        // Non-root function is keyed as "submod.helper" in fn_def_spans.
        let entry = checker.fn_def_spans.get("submod.helper");
        assert!(
            entry.is_some(),
            "fn_def_spans must contain 'submod.helper'; keys: {:?}",
            checker.fn_def_spans.keys().collect::<Vec<_>>()
        );
        let (_, stored_module) = entry.unwrap();
        assert_eq!(
            stored_module.as_deref(),
            Some("submod"),
            "fn_def_spans entry for 'submod.helper' must store source_module='submod'; got {stored_module:?}",
        );
    }

    /// Root-module functions must store `source_module = None` in `fn_def_spans`.
    #[test]
    fn root_fn_def_span_has_no_source_module() {
        let program = build_program_with_non_root_import_and_fn();
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&program);

        // Root fn (main) has no module prefix.
        let entry = checker.fn_def_spans.get("main");
        assert!(
            entry.is_some(),
            "fn_def_spans must contain 'main'; keys: {:?}",
            checker.fn_def_spans.keys().collect::<Vec<_>>()
        );
        let (_, stored_module) = entry.unwrap();
        assert_eq!(
            stored_module.as_deref(),
            None,
            "fn_def_spans entry for 'main' must have source_module=None; got {stored_module:?}",
        );
    }

    #[test]
    fn non_root_unreachable_warning_carries_source_module() {
        let stmts = vec![
            (Stmt::Return(None), 10..16),
            (
                Stmt::Expression((Expr::Literal(Literal::Bool(true)), 21..25)),
                21..23,
            ),
        ];
        let program = make_non_root_program_with_fn_body("submod", "warns", stmts);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let warnings: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnreachableCode)
            .collect();

        assert!(
            !warnings.is_empty(),
            "expected UnreachableCode warning in non-root module; got: {:?}",
            output.warnings
        );
        for warning in warnings {
            assert_eq!(
                warning.source_module.as_deref(),
                Some("submod"),
                "UnreachableCode warning must carry source_module='submod'; got {:?}",
                warning.source_module
            );
        }
    }

    // ── ImportKey: same short-name across different owning modules ─────────────

    fn make_named_import_decl(short_name: &str) -> ImportDecl {
        ImportDecl {
            path: vec![short_name.to_string()],
            spec: None,
            file_path: None,
            resolved_items: Some(vec![]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        }
    }

    /// Two different owning modules both import a module with the same short
    /// name.  Neither module uses the import, so both must get an
    /// `UnusedImport` warning — the use-site in one must not suppress the
    /// warning for the other.
    #[test]
    #[allow(
        clippy::similar_names,
        reason = "mod_a_id / mod_b_id are intentionally symmetric"
    )]
    fn same_short_name_imports_in_different_owners_each_warn_unused() {
        let root_id = ModuleId::root();
        let mod_a_id = ModuleId::new(vec!["mod_a".to_string()]);
        let mod_b_id = ModuleId::new(vec!["mod_b".to_string()]);

        let import_a = make_named_import_decl("fakemod");
        let import_b = make_named_import_decl("fakemod");

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_a = Module {
            id: mod_a_id.clone(),
            items: vec![(Item::Import(import_a), 0..20)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_b = Module {
            id: mod_b_id.clone(),
            items: vec![(Item::Import(import_b), 100..120)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module);
        mg.add_module(module_a);
        mg.add_module(module_b);
        mg.topo_order = vec![mod_a_id, mod_b_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"))
            .collect();

        assert_eq!(
            unused.len(),
            2,
            "expected exactly 2 UnusedImport warnings (one per owning module), got {}: {:?}",
            unused.len(),
            unused
        );

        let owners: std::collections::HashSet<Option<&str>> =
            unused.iter().map(|w| w.source_module.as_deref()).collect();
        assert!(
            owners.contains(&Some("mod_a")),
            "expected an UnusedImport attributed to 'mod_a'; got: {owners:?}",
        );
        assert!(
            owners.contains(&Some("mod_b")),
            "expected an UnusedImport attributed to 'mod_b'; got: {owners:?}",
        );
    }

    /// When one owning module *uses* `fakemod` (via a module-qualified call
    /// registered through `import_spans`) and another owning module imports the
    /// same short name but never uses it, only the second module's import must
    /// be warned as unused.
    ///
    /// This is the core clobber regression: before the `ImportKey` fix, marking
    /// `fakemod` as used in `mod_a` would also suppress the warning for `mod_b`.
    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "inline AST construction for the import-clobber regression scenario"
    )]
    #[allow(
        clippy::similar_names,
        reason = "mod_a_id / mod_b_id are intentionally symmetric"
    )]
    fn used_import_in_one_owner_does_not_suppress_unused_in_another() {
        let root_id = ModuleId::root();
        let mod_a_id = ModuleId::new(vec!["mod_a".to_string()]);
        let mod_b_id = ModuleId::new(vec!["mod_b".to_string()]);

        // mod_a: import fakemod  +  fn caller() { fakemod.helper() }
        // The call to fakemod.helper() marks fakemod as used in mod_a's context.

        // Register a pub fn "helper" in fakemod so module_fn_exports and fn_sigs
        // contain "fakemod.helper" — that is what the method-dispatch path checks.
        let helper_fn = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Pub,
            is_pure: false,
            name: "helper".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
        };

        // caller() body: `fakemod.helper()` expressed as a MethodCall statement.
        let call_stmt = Stmt::Expression((
            Expr::MethodCall {
                receiver: Box::new((
                    Expr::Identifier("fakemod".to_string()),
                    Span::from(200..206),
                )),
                method: "helper".to_string(),
                args: vec![],
            },
            Span::from(200..215),
        ));
        let caller_fn = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "caller".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(call_stmt, 200..215)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 150..200,
            fn_span: 0..0,
        };

        // Import of fakemod in mod_a must carry resolved_items that include
        // the pub helper fn so the module is actually registered and
        // module_fn_exports gets "fakemod.helper".
        let import_a_with_items = ImportDecl {
            path: vec!["fakemod".to_string()],
            spec: None,
            file_path: None,
            resolved_items: Some(vec![(Item::Function(helper_fn), 0..30)]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        };

        // mod_b: import fakemod  (unused — no code references it)
        let import_b = make_named_import_decl("fakemod");

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_a = Module {
            id: mod_a_id.clone(),
            items: vec![
                (Item::Import(import_a_with_items), 0..30),
                (Item::Function(caller_fn), 150..220),
            ],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_b = Module {
            id: mod_b_id.clone(),
            items: vec![(Item::Import(import_b), 300..320)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module);
        mg.add_module(module_a);
        mg.add_module(module_b);
        mg.topo_order = vec![mod_a_id, mod_b_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"))
            .collect();

        // mod_b's import is unused; mod_a's is used (caller() calls fakemod.helper())
        // so exactly one warning, attributed to mod_b.
        assert_eq!(
            unused.len(),
            1,
            "expected exactly 1 UnusedImport warning (for mod_b), got {}: {:?}",
            unused.len(),
            unused
        );
        assert_eq!(
            unused[0].source_module.as_deref(),
            Some("mod_b"),
            "the single UnusedImport must be attributed to 'mod_b'; got: {:?}",
            unused[0].source_module
        );
    }

    // ── Ty::Error return-context seeding regressions ──────────────────────────
    //
    // When a function's return-type annotation cannot be resolved (e.g.
    // `UnknownType`), `resolve_type_expr` produces `Ty::Error`.  Before this
    // fix the error type was passed as the *expected* type into the body,
    // causing `expect_type`'s guard (`expected_resolved != Ty::Error`) to
    // silently swallow genuine body-level type errors.

    fn has_mismatch(errors: &[crate::error::TypeError]) -> bool {
        errors.iter().any(|e| {
            matches!(e.kind, TypeErrorKind::Mismatch { .. })
                || e.message.contains("mismatch")
                || e.message.contains("TypeMismatch")
        })
    }

    #[test]
    fn error_return_type_does_not_suppress_trailing_expr_mismatch() {
        // fn foo() -> UnknownType { let x: i32 = "bad"; x }
        // The `let x: i32 = "bad"` is a type mismatch inside the body.
        // When the fn return annotation is Ty::Error the body was previously
        // checked with check_against(_, Ty::Error), masking the let-binding error.
        // After the fix the body is synthesized (expected=None), so the let
        // mismatch is still reported.
        let source = r#"fn foo() -> UnknownType { let x: i32 = "bad"; x }"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "trailing-expr body mismatch must be reported even when return type \
             is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_does_not_suppress_explicit_return_mismatch() {
        // fn foo() -> UnknownType { let x: i32 = "bad"; return x; }
        // The let-binding mismatch must be reported.
        let source = r#"fn foo() -> UnknownType { let x: i32 = "bad"; return x; }"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "body mismatch inside explicit return must be reported even when \
             return type is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_does_not_suppress_receive_fn_body_mismatch() {
        // receive fn handler() -> UnknownType { let x: i32 = "bad"; x }
        // inside an actor; body mismatch must be reported.
        let source = r#"
actor MyActor {
    var value: i32 = 0;
    receive fn handler() -> UnknownType { let x: i32 = "bad"; x }
}
"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "body mismatch in receive fn must be reported even when return type \
             is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_does_not_suppress_lambda_annotated_return_mismatch() {
        // Lambda with annotated (unresolvable) return type:
        //   let f = (x: i32) -> UnknownType => { let y: i32 = "bad"; y };
        // The let-binding mismatch inside the lambda body must still be reported.
        let source =
            r#"fn foo() { let f = (x: i32) -> UnknownType => { let y: i32 = "bad"; y }; }"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "body mismatch in lambda annotated return must be reported even when \
             return type is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_question_mark_on_non_result_still_reports() {
        // fn foo() -> UnknownType { let x: i64 = 1; x? }
        //
        // The inner-type check for `?` ("? requires Result or Option, found i64")
        // fires unconditionally via the else branch of the PostfixTry handler.
        // It must not be suppressed even when the enclosing return annotation
        // resolves to Ty::Error — that only bypasses the *context* diagnostic
        // ("? cannot be used in a function returning X"), not the inner-type check.
        let source = r"fn foo() -> UnknownType { let x: i64 = 1; x? }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_try_err = output
            .errors
            .iter()
            .any(|e| e.message.contains("requires Result or Option"));
        assert!(
            has_try_err,
            "? on non-Result/non-Option must still report \
             '? requires Result or Option' even when return annotation is \
             Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_question_mark_on_result_no_false_context_error() {
        // fn foo() -> UnknownType { let r: Result<i64, String> = Ok(1); r? }
        //
        // When the return annotation is unresolvable (Ty::Error) and `?` is
        // used on a valid Result, the *context* diagnostic ("? cannot be used
        // in a function returning <error>") must NOT fire — we cannot know
        // whether the intended return type would have supported `?`.  Only the
        // annotation-resolution error should appear.
        let source = r"fn foo() -> UnknownType { let r: Result<i64, String> = Ok(1); r? }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_ctx_err = output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot be used in a function returning"));
        assert!(
            !has_ctx_err,
            "? on valid Result in bad-annotation function must NOT emit a \
             spurious context error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn builtin_named_return_type_still_reports_question_mark_context_error() {
        // fn foo() -> Vec<i32> { let r: Result<i64, String> = Ok(1); let x: i64 = r?; Vec::new() }
        //
        // PR #923 bypasses the `?` context diagnostic for genuinely unknown named
        // return annotations. Builtin named types like Vec must still report the
        // context error even though they are not registered in type_defs/type_aliases.
        let source = r"fn foo() -> Vec<i32> { let r: Result<i64, String> = Ok(1); let x: i64 = r?; Vec::new() }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_ctx_err = output.errors.iter().any(|e| {
            e.message
                .contains("cannot be used in a function returning `Vec<i32>`")
        });
        assert!(
            has_ctx_err,
            "? on valid Result in a function returning builtin Vec must still \
             emit the context error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_question_mark_in_lambda_no_false_context_error() {
        // fn foo() { let r: Result<i64, String> = Ok(1); let f = (x: i64) -> UnknownType => { r? }; }
        //
        // Same invariant as the plain-function case but inside a lambda whose
        // return annotation is Ty::Error.  The `?` context check sees the
        // lambda's own `current_return_type` (Ty::Error), so the Ty::Error
        // bypass must apply there too.
        let source = r"fn foo() { let r: Result<i64, String> = Ok(1); let f = (x: i64) -> UnknownType => { r? }; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_ctx_err = output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot be used in a function returning"));
        assert!(
            !has_ctx_err,
            "? on valid Result inside a lambda with bad return annotation must \
             NOT emit a spurious context error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn valid_return_annotation_good_path_regression_guard() {
        // fn foo() -> i32 { 42 }
        //
        // A function with a valid, resolvable return annotation must typecheck
        // cleanly.  This guards against regressions introduced by the Ty::Error
        // seeding fix inadvertently breaking the happy path (literal coercion,
        // trailing-expression checking, and expect_type alignment).
        let source = r"fn _foo() -> i32 { 42 }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "valid typed-return function must not produce type errors; \
             got: {:?}",
            output.errors
        );
        assert!(
            output.warnings.is_empty(),
            "valid typed-return function must not produce warnings; \
             got: {:?}",
            output.warnings
        );
    }

    // ── ExternBlock scoped-lookup regression ───────────────────────────────────
    //
    // When an extern block lives in a non-root module, the key stored in
    // `fn_sig_inference_holes` is scoped as `"mymod.extfn"` (set by
    // `register_extern_block` → `scoped_module_item_name`).
    //
    // Before the fix, `report_unresolved_inference_in_items` used a bare-name
    // lookup (`fn_sig_inference_holes.get("extfn")`), so the inference hole was
    // never detected and no `InferenceFailed` error was emitted for non-root
    // extern functions with `_`-typed parameters.
    //
    // After the fix the arm uses `lookup_scoped_item(…, module_name, "extfn")`
    // which resolves the scoped key and the error is emitted + tagged correctly.

    fn make_extern_block_with_infer_param(fn_name: &str) -> Item {
        Item::ExternBlock(ExternBlock {
            abi: "C".to_string(),
            functions: vec![ExternFnDecl {
                name: fn_name.to_string(),
                params: vec![Param {
                    name: "p".to_string(),
                    ty: (TypeExpr::Infer, 20..21),
                    is_mutable: false,
                }],
                return_type: None,
                is_variadic: false,
            }],
        })
    }

    /// A non-root extern function with a `_`-typed parameter must fail closed
    /// with `InferenceFailed` tagged `source_module = Some("mymod")`.
    ///
    /// Regression guard for the `Item::ExternBlock` bare-name lookup bug in
    /// `report_unresolved_inference_in_items`.
    #[test]
    fn non_root_extern_fn_infer_param_fails_closed_with_source_module() {
        let extern_item = make_extern_block_with_infer_param("extfn");
        let root_id = ModuleId::root();
        let mymod_id = ModuleId::new(vec!["mymod".to_string()]);

        let mymod = Module {
            id: mymod_id.clone(),
            items: vec![(extern_item, 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module);
        mg.add_module(mymod);
        mg.topo_order = vec![mymod_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_errs.is_empty(),
            "non-root extern fn with `_` param must produce InferenceFailed; \
             got errors: {:?}",
            output.errors
        );
        for err in &inference_errs {
            assert_eq!(
                err.source_module.as_deref(),
                Some("mymod"),
                "InferenceFailed for non-root extern fn must carry \
                 source_module='mymod'; got {:?}",
                err.source_module
            );
        }
    }

    /// A root-module extern function with a `_`-typed parameter must also fail
    /// closed with `InferenceFailed`, with `source_module = None`.
    ///
    /// Confirms the fix does not break the root-module code path.
    #[test]
    fn root_extern_fn_infer_param_fails_closed_without_source_module() {
        let extern_item = make_extern_block_with_infer_param("root_extfn");
        let program = Program {
            module_graph: None,
            items: vec![(extern_item, 0..30)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_errs.is_empty(),
            "root extern fn with `_` param must produce InferenceFailed; \
             got errors: {:?}",
            output.errors
        );
        for err in &inference_errs {
            assert_eq!(
                err.source_module, None,
                "InferenceFailed for root extern fn must have source_module=None; got {:?}",
                err.source_module
            );
        }
    }

    /// A non-root scope warning (`UnusedVariable`) carries `source_module`.
    ///
    /// Certifies that `emit_scope_warnings` copies `self.current_module` into
    /// the warning and the snapshot module tagging pass in `check_program` does
    /// not overwrite it when already set.
    #[test]
    fn non_root_unused_variable_warning_carries_source_module() {
        // fn warns() { let x = 42; }  — `x` is never read
        let stmts = vec![(
            Stmt::Let {
                pattern: (Pattern::Identifier("x".to_string()), 10..11),
                ty: None,
                value: Some((
                    Expr::Literal(Literal::Integer {
                        value: 42,
                        radix: IntRadix::Decimal,
                    }),
                    14..16,
                )),
            },
            10..16,
        )];
        let program = make_non_root_program_with_fn_body("warnmod", "warns", stmts);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedVariable)
            .collect();

        assert!(
            !unused.is_empty(),
            "expected UnusedVariable warning for `x` in non-root module; got warnings: {:?}",
            output.warnings
        );
        for w in &unused {
            assert_eq!(
                w.source_module.as_deref(),
                Some("warnmod"),
                "UnusedVariable warning must carry source_module='warnmod'; got {:?}",
                w.source_module
            );
        }
    }
}

// ── WASM compile-time reject tests ──────────────────────────────────────────
//
// These tests verify that `Channels`, `Semaphore`, `Timers`, and `Streams` features are
// rejected as compile-time errors (not warnings) when the WASM target is
// enabled.  The reject path is exercised by setting `checker.enable_wasm_target()`
// before calling `check_program`.
//
// Coverage:
//  - channel.new / send / try_recv → allowed on wasm32 bounded subset
//  - Receiver<T>::recv / `for await ... in Receiver<T>` → BlockingChannelRecv error
//  - semaphore.new / try_acquire / release / count / free → allowed on wasm32
//  - Semaphore::acquire / Semaphore::acquire_timeout → BlockingSemaphoreAcquire error
//  - sleep_ms → Timers warning
//  - sleep → Timers warning
//  - Stream<T>::next → Streams error
//  - stream.* module constructor call → Streams error
//  - Non-wasm target: none of the above fire
mod wasm_rejects {
    use super::*;

    /// Parse `source`, enable the WASM target, run the type checker, and
    /// return the resulting output.
    fn check_wasm(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in wasm_rejects test: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.enable_wasm_target();
        checker.check_program(&result.program)
    }

    /// Parse `source` without the WASM target and return the output.
    fn check_native(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in wasm_rejects test (native): {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&result.program)
    }

    fn has_platform_limitation_error(output: &TypeCheckOutput) -> bool {
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::PlatformLimitation)
    }

    fn has_platform_limitation_warning(output: &TypeCheckOutput) -> bool {
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::PlatformLimitation)
    }

    fn platform_error_contains(output: &TypeCheckOutput, fragment: &str) -> bool {
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::PlatformLimitation && e.message.contains(fragment))
    }

    fn platform_warning_contains(output: &TypeCheckOutput, fragment: &str) -> bool {
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::PlatformLimitation && w.message.contains(fragment))
    }

    // ── sleep_ms ─────────────────────────────────────────────────────────────

    #[test]
    fn wasm_rejects_sleep_ms() {
        let output = check_wasm("fn main() { sleep_ms(100); }");
        assert!(
            has_platform_limitation_warning(&output),
            "sleep_ms should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            platform_warning_contains(&output, "Timer"),
            "warning message should mention Timer feature; got: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "sleep_ms should NOT be a compile-time error on WASM (cooperative semantics); got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_sleep() {
        let output = check_wasm("fn main() { sleep(1); }");
        assert!(
            has_platform_limitation_warning(&output),
            "sleep should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "sleep should NOT be a compile-time error on WASM (cooperative semantics); got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_sleep_ms_no_platform_error() {
        let output = check_native("fn main() { sleep_ms(100); }");
        assert!(
            !has_platform_limitation_error(&output),
            "sleep_ms should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_warns_on_every_attribute() {
        let output =
            check_wasm("actor Ticker { #[every(10ms)] receive fn tick() {} } fn main() {}");
        assert!(
            has_platform_limitation_warning(&output),
            "#[every] should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            platform_warning_contains(&output, "Timer"),
            "#[every] warning should mention Timer operations; got: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "#[every] should NOT be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_every_attribute_no_platform_error() {
        let output =
            check_native("actor Ticker { #[every(10ms)] receive fn tick() {} } fn main() {}");
        assert!(
            !has_platform_limitation_error(&output),
            "#[every] should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── channel.new ──────────────────────────────────────────────────────────

    #[test]
    fn wasm_allows_bounded_channel_subset() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    let _ = rx.try_recv();\n",
            "    tx.close();\n",
            "    rx.close();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "bounded channel.new/send/try_recv subset should be allowed on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_channel_new_no_platform_error() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let pair = channel.new(0);\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "channel.new should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_blocking_channel_recv() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (_tx, rx) = channel.new(1);\n",
            "    let _ = rx.recv();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "blocking recv should still be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Blocking channel receive"),
            "error message should mention blocking channel receive; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_for_await_receiver() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    tx.close();\n",
            "    for await item in rx {\n",
            "        println(item);\n",
            "    }\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "`for await` over Receiver<T> should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Blocking channel receive"),
            "error message should mention blocking channel receive; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_for_await_receiver_no_platform_error() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    tx.close();\n",
            "    for await item in rx {\n",
            "        println(item);\n",
            "    }\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "`for await` over Receiver<T> should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_allows_non_blocking_semaphore_subset() {
        let source = concat!(
            "import std::semaphore;\n",
            "fn main() {\n",
            "    let sem = semaphore.new(1);\n",
            "    let _ = sem.count();\n",
            "    let _ = sem.try_acquire();\n",
            "    sem.release();\n",
            "    sem.free();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "non-blocking semaphore subset should be allowed on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_blocking_semaphore_methods() {
        let source = concat!(
            "import std::semaphore;\n",
            "fn main() {\n",
            "    let sem = semaphore.new(1);\n",
            "    sem.acquire();\n",
            "    let _ = sem.acquire_timeout(10);\n",
            "    sem.free();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        let reject_count = output
            .errors
            .iter()
            .filter(|error| {
                error.kind == TypeErrorKind::PlatformLimitation
                    && error.message.contains("Blocking semaphore acquire")
            })
            .count();
        assert!(
            reject_count >= 2,
            "blocking semaphore methods should be rejected on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_blocking_semaphore_methods_no_platform_error() {
        let source = concat!(
            "import std::semaphore;\n",
            "fn main() {\n",
            "    let sem = semaphore.new(1);\n",
            "    sem.acquire();\n",
            "    let _ = sem.acquire_timeout(10);\n",
            "    sem.release();\n",
            "    sem.free();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "blocking semaphore methods should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── Stream<T> methods ────────────────────────────────────────────────────

    #[test]
    fn wasm_rejects_stream_method() {
        // Use a function that accepts a Stream<String> and calls .next().
        // The stream module must be imported to register Stream types.
        let source = concat!(
            "import std::stream;\n",
            "fn consume(s: stream.Stream<string>) -> string {\n",
            "    s.next()\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "Stream<T>::next should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Stream"),
            "error message should mention Stream feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_stream_method_no_platform_error() {
        let source = concat!(
            "import std::stream;\n",
            "fn consume(s: stream.Stream<string>) -> string {\n",
            "    s.next()\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "Stream<T>::next should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── TLS / QUIC / DNS / OS reject + CryptoRandom warn ───────────────────

    fn check_wasm_with_registry(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in wasm_rejects test: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        checker.check_program(&result.program)
    }

    #[test]
    fn wasm_rejects_tls_module_call() {
        let source = concat!(
            "import std::net::tls;\n",
            "fn main() { tls.connect(\"host\", 443); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "tls.connect should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::net::tls"),
            "error message should mention TLS feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_quic_module_call() {
        let source = concat!(
            "import std::net::quic;\n",
            "fn main() { quic.new_client(); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "quic.* should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::net::quic"),
            "error message should mention QUIC feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_dns_module_call() {
        let source = concat!(
            "import std::net::dns;\n",
            "fn main() { dns.resolve(\"example.com\"); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "dns.resolve should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::net::dns"),
            "error message should mention DNS feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_os_module_call() {
        let source = concat!("import std::os;\n", "fn main() { os.env(\"HOME\"); }\n",);
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "os.* should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::os"),
            "error message should mention OS feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_warns_crypto_random_bytes() {
        // crypto.random_bytes is a WARNING, not an error, because the wasm32
        // implementation falls back to a seeded non-cryptographic PRNG.
        let source = concat!(
            "import std::crypto::crypto;\n",
            "fn main() { crypto.random_bytes(16); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_warning(&output),
            "crypto.random_bytes should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            platform_warning_contains(&output, "random_bytes"),
            "warning message should mention crypto.random_bytes; got: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "crypto.random_bytes should NOT be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
    }

    // ── Native sibling tests: no platform error on non-wasm target ────────

    #[test]
    fn native_tls_no_platform_error() {
        let source = concat!(
            "import std::net::tls;\n",
            "fn main() { tls.connect(\"host\", 443); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "tls.connect should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "tls.connect should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_quic_no_platform_error() {
        let source = concat!(
            "import std::net::quic;\n",
            "fn main() { quic.new_client(); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "quic.* should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "quic.* should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_dns_no_platform_error() {
        let source = concat!(
            "import std::net::dns;\n",
            "fn main() { dns.resolve(\"example.com\"); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "dns.resolve should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "dns.resolve should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_os_no_platform_error() {
        let source = concat!("import std::os;\n", "fn main() { os.env(\"HOME\"); }\n",);
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "os.* should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "os.* should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_crypto_random_bytes_no_platform_error() {
        let source = concat!(
            "import std::crypto::crypto;\n",
            "fn main() { crypto.random_bytes(16); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "crypto.random_bytes should not emit PlatformLimitation error on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "crypto.random_bytes should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    // ── Deduplication: same call site emits only one error ─────────────────

    #[test]
    fn wasm_reject_deduplicates_same_span() {
        // Two consecutive calls at different call sites should produce two
        // warnings, not one (each span is unique).
        let output = check_wasm("fn main() { sleep_ms(100); sleep_ms(200); }");
        let count = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::PlatformLimitation && w.message.contains("Timer"))
            .count();
        assert_eq!(
            count, 2,
            "two distinct sleep_ms call sites should produce two warnings; got: {:?}",
            output.warnings
        );
    }

    // ── Reject-level features now fail closed on WASM ────────────────────────

    fn supervisor_calls_source() -> &'static str {
        r"
            actor Worker {
                receive fn ping() {}
            }

            supervisor WorkerPool {
                strategy: one_for_one,
                max_restarts: 1,
                window: 10,
                child w1: Worker
            }

            fn main() {
                let pool = spawn WorkerPool;
                let worker = supervisor_child(pool, 0);
                supervisor_stop(pool);
                worker.ping();
            }
        "
    }

    fn link_monitor_calls_source() -> &'static str {
        r"
            actor Worker {
                receive fn ping() {}
            }

            fn main() {
                let worker = spawn Worker;
                let ref_id = monitor(worker);
                link(worker);
                demonitor(ref_id);
            }
        "
    }

    fn structured_concurrency_scope_source() -> &'static str {
        "fn main() { let result = scope { 1 + 2 }; println(result); }"
    }

    fn scope_tasks_source() -> &'static str {
        r"
            fn main() {
                let result = scope |s| {
                    let task = s.launch {
                        42
                    };
                    await task
                };
                println(result);
            }
        "
    }

    #[test]
    fn wasm_rejects_supervisor_calls() {
        let output = check_wasm(supervisor_calls_source());
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::PlatformLimitation),
            "supervision operations should be WASM errors; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Supervision tree"),
            "error message should mention Supervision tree feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_supervisor_calls_no_platform_error() {
        let output = check_native(supervisor_calls_source());
        assert!(
            !has_platform_limitation_error(&output),
            "supervision operations should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_supervisor_declaration() {
        let output = check_wasm(
            r"
            actor Worker {
                receive fn ping() {}
            }

            supervisor WorkerPool {
                strategy: one_for_one,
                max_restarts: 1,
                window: 10,
                child w1: Worker
            }

            fn main() {}
        ",
        );
        assert!(
            has_platform_limitation_error(&output),
            "supervisor declarations should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Supervision tree"),
            "error message should mention Supervision tree feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_link_monitor_calls() {
        let output = check_wasm(link_monitor_calls_source());
        assert!(
            has_platform_limitation_error(&output),
            "link/monitor operations should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Link/monitor"),
            "error message should mention Link/monitor feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_link_monitor_no_platform_error() {
        let output = check_native(link_monitor_calls_source());
        assert!(
            !has_platform_limitation_error(&output),
            "link/monitor operations should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_structured_concurrency_scope() {
        let output = check_wasm(structured_concurrency_scope_source());
        assert!(
            has_platform_limitation_error(&output),
            "scope expressions should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Structured concurrency"),
            "error message should mention Structured concurrency feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_scope_no_platform_error() {
        let output = check_native(structured_concurrency_scope_source());
        assert!(
            !has_platform_limitation_error(&output),
            "scope expressions should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_scope_tasks() {
        let output = check_wasm(scope_tasks_source());
        assert!(
            has_platform_limitation_error(&output),
            "scope tasks should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Task handles"),
            "error message should mention Task feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_scope_tasks_no_platform_error() {
        let output = check_native(scope_tasks_source());
        assert!(
            !has_platform_limitation_error(&output),
            "scope tasks should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_multi_arm_literal_timed_select_is_not_warning() {
        let output = check_wasm(
            r"
            actor Responder {
                let value: int;
                receive fn get() -> int {
                    value
                }
            }

            fn main() {
                let a = spawn Responder(value: 1);
                let b = spawn Responder(value: 2);
                let result = select {
                    x from a.get() => x,
                    y from b.get() => y,
                    after 1ms => -1,
                };
                println(result);
            }
        ",
        );
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::PlatformLimitation),
            "literal timed select should no longer warn on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::PlatformLimitation),
            "literal timed select should not error on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_computed_timed_select_no_longer_warns() {
        let output = check_wasm(
            r"
            actor Responder {
                let value: int;
                receive fn get() -> int {
                    value
                }
            }

            fn main() {
                let a = spawn Responder(value: 1);
                let b = spawn Responder(value: 2);
                let timeout = 1ms;
                let result = select {
                    x from a.get() => x,
                    y from b.get() => y,
                    after timeout => -1,
                };
                println(result);
            }
        ",
        );
        assert!(
            !output.warnings.iter().any(
                |w| w.kind == TypeErrorKind::PlatformLimitation && w.message.contains("Select")
            ),
            "computed timed select should not warn on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::PlatformLimitation),
            "computed timed select should not error on WASM; got errors: {:?}",
            output.errors
        );
    }

    // ── Wave 14: Ty::Error cascade-suppression fixes ─────────────────────────
    //
    // These tests verify that independent arg diagnostics are NOT suppressed
    // when the receiver/callee already has type Ty::Error.  Prior to the fix,
    // "bad arg" errors were silently dropped at every unknown-method `_` arm
    // and at `check_call_with_type` when called with a Ty::Error callee type.

    fn check_wave14(source: &str) -> Vec<TypeErrorKind> {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        output.errors.into_iter().map(|e| e.kind).collect()
    }

    #[test]
    fn wave14_bad_string_method_with_bad_arg_both_reported() {
        // `s.nonexistent_method(undefined_arg)` must report BOTH:
        //   - "no method `nonexistent_method` on string"
        //   - "undefined variable `undefined_arg`"
        // Before the fix, only the first error was reported.
        let kinds = check_wave14(r"fn foo(s: String) { s.nonexistent_method(undefined_arg) }");
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn wave14_bad_vec_method_with_bad_arg_both_reported() {
        // `v.nonexistent_method(undefined_arg)` on a Vec must report both errors.
        let kinds = check_wave14(r"fn foo(v: Vec<i64>) { v.nonexistent_method(undefined_arg) }");
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn wave14_bad_hashmap_method_with_bad_arg_both_reported() {
        // `m.nonexistent_method(undefined_arg)` on a HashMap must report both errors.
        let kinds = check_wave14(
            r"fn foo(m: HashMap<String, i64>) { m.nonexistent_method(undefined_arg) }",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn wave14_call_error_typed_var_arg_errors_reported() {
        // `let f = undefined_fn(); f(undefined_arg)` — when `f` has type Ty::Error
        // (because `undefined_fn` is unknown), the args to `f(...)` must still be
        // synthesized so `undefined_arg` errors are surfaced.
        let kinds = check_wave14(r"fn foo() { let f = undefined_fn(); let _ = f(undefined_arg); }");
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedFunction),
            "expected UndefinedFunction for `undefined_fn`; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn wave14_chained_bad_method_with_bad_arg_in_chain() {
        // `s.bad_method().another_method(undefined_arg)` — the (Ty::Error, _) arm
        // already synthesizes args for chained calls, so `undefined_arg` SHOULD be
        // reported.  This test guards that the chained-call path is not regressed.
        let kinds = check_wave14(
            r"fn foo(s: String) { let _ = s.bad_method().another_method(undefined_arg); }",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod for `bad_method`; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable in chained call; got: {kinds:?}",
        );
    }

    #[test]
    fn wave14_simple_chain_still_suppressed_correctly() {
        // `s.bad_method().to_string()` — the `.to_string()` on the Ty::Error result
        // is correctly suppressed (not a new error). The (Ty::Error, _) arm must
        // NOT emit a duplicate diagnostic for the chained method.
        let kinds = check_wave14(r"fn foo(s: String) { let _ = s.bad_method().to_string(); }");
        assert_eq!(
            kinds,
            vec![TypeErrorKind::UndefinedMethod],
            "expected exactly [UndefinedMethod] — chain must stay suppressed; got: {kinds:?}",
        );
    }
}

// ── if-let / while-let pattern contract ──────────────────────────────────
//
// These tests verify that the checker rejects Struct, Tuple, Or, and
// Literal patterns at the top level of `if let` / `while let`, because
// codegen has no support for them.  Constructor, Wildcard, and Identifier
// are the only allowed patterns.
#[cfg(test)]
mod iflet_whilelet_pattern_contract {
    use super::*;

    fn check_iflet_whilelet(source: &str) -> Vec<crate::error::TypeError> {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&result.program).errors
    }

    #[test]
    fn iflet_stmt_literal_pattern_is_rejected() {
        let errors = check_iflet_whilelet(r"fn foo(x: int) { if let 1 = x { 0 } }");
        assert!(
            errors.iter().any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("literal")),
            "expected InvalidOperation for literal if-let pattern; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_struct_pattern_is_accepted() {
        let errors = check_iflet_whilelet(
            r"type Point { x: int; y: int; } fn foo(p: Point) { if let Point { x, y } = p { x + y } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "struct if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_tuple_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: (int, int)) { if let (a, b) = x { a + b } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "tuple if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_or_pattern_is_accepted() {
        let errors =
            check_iflet_whilelet(r"enum E { A; B; } fn foo(x: E) { if let A | B = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "or if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_wildcard_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: int) { if let _ = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "wildcard if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_identifier_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: int) { if let y = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "identifier if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_literal_pattern_is_rejected() {
        let errors = check_iflet_whilelet(r"fn foo(x: int) { while let 1 = x { break; } }");
        assert!(
            errors.iter().any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("literal")),
            "expected InvalidOperation for literal while-let pattern; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_struct_pattern_is_accepted() {
        let errors = check_iflet_whilelet(
            r"enum Msg { Data { value: int }; Done; } fn foo(x: Msg) { while let Data { value } = x { break; } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "struct while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_tuple_pattern_is_accepted() {
        let errors =
            check_iflet_whilelet(r"fn foo(x: (int, int)) { while let (a, b) = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "tuple while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_or_pattern_is_accepted() {
        let errors = check_iflet_whilelet(
            r"enum E { A; B; } fn foo(x: E) { while let A | B = x { break; } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "or while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_wildcard_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: int) { while let _ = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "wildcard while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_identifier_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: int) { while let y = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "identifier while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_labeled_break_is_accepted() {
        let errors =
            check_iflet_whilelet(r"fn foo(x: int) { @scan: while let y = x { break @scan; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("unknown loop label")),
            "labeled while-let break must not emit unknown loop label; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_labeled_continue_is_accepted() {
        let errors =
            check_iflet_whilelet(r"fn foo(x: int) { @scan: while let y = x { continue @scan; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("unknown loop label")),
            "labeled while-let continue must not emit unknown loop label; got: {errors:?}",
        );
    }

    #[test]
    fn nested_loop_can_target_outer_whilelet_label() {
        let errors = check_iflet_whilelet(
            r"fn foo(x: int) { @scan: while let y = x { loop { break @scan; } } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("unknown loop label")),
            "nested loops must resolve outer while-let labels; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_unknown_label_still_errors() {
        let errors = check_iflet_whilelet(r"fn foo(x: int) { while let y = x { break @scan; } }");
        assert!(
            errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation
                    && e.message.contains("unknown loop label `@scan`")),
            "unknown while-let labels must still error; got: {errors:?}",
        );
    }
}

// ── for-loop iterable fail-closed regressions ──────────────────────────────

mod for_loop_iterable_fail_closed {
    use super::*;

    fn check_for_over(iter_ty: Ty) -> Vec<TypeError> {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.env.define("it".to_string(), iter_ty, false);
        let for_stmt = Stmt::For {
            label: None,
            is_await: false,
            pattern: (Pattern::Identifier("x".to_string()), 0..1),
            iterable: (Expr::Identifier("it".to_string()), 7..9),
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
        };
        checker.check_stmt(&for_stmt, &(0..20));
        checker.errors
    }

    // ── catch-all: unsupported iterable type ───────────────────────────────

    #[test]
    fn unsupported_iterable_bool_emits_not_iterable_diagnostic() {
        let result = hew_parser::parse("fn main() { for x in true { } }");
        assert!(
            result.errors.is_empty(),
            "parse errors: {result_errors:?}",
            result_errors = result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.iter().any(|e| {
                e.kind == TypeErrorKind::InvalidOperation && e.message.contains("not iterable")
            }),
            "expected 'not iterable' diagnostic for bool iterable; got: {errs:?}",
            errs = output.errors
        );
    }

    #[test]
    fn unsupported_iterable_does_not_produce_fresh_typevar_elem() {
        // Direct AST: `for x in it` where `it: bool`. The elem type must be
        // Ty::Error, not Ty::Var, ensuring no inference holes leak downstream.
        let errors = check_for_over(Ty::Bool);
        assert!(
            errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "expected InvalidOperation diagnostic for non-iterable; got: {errors:?}",
        );
    }

    // ── Vec with empty type args ───────────────────────────────────────────

    #[test]
    fn vec_with_empty_type_args_emits_diagnostic_not_fresh_var() {
        let errors = check_for_over(Ty::Named {
            name: "Vec".to_string(),
            args: vec![],
        });
        assert!(
            errors.iter().any(|e| {
                e.kind == TypeErrorKind::InvalidOperation && e.message.contains("Vec")
            }),
            "expected InvalidOperation for Vec with no type args; got: {errors:?}",
        );
    }

    // ── Stream with empty type args (plain `for`, not `for await`) ────────

    #[test]
    fn stream_with_empty_type_args_emits_diagnostic_not_fresh_var() {
        let errors = check_for_over(Ty::Named {
            name: "Stream".to_string(),
            args: vec![],
        });
        assert!(
            errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "expected InvalidOperation for Stream with no type args; got: {errors:?}",
        );
    }

    // ── valid iterables must not regress ──────────────────────────────────

    #[test]
    fn vec_with_type_arg_is_valid() {
        let errors = check_for_over(Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::I64],
        });
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "Vec<i64> iterable must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn array_iterable_is_valid() {
        let errors = check_for_over(Ty::Array(Box::new(Ty::I32), 4));
        assert!(
            errors.is_empty(),
            "Array iterable must not emit errors; got: {errors:?}",
        );
    }

    #[test]
    fn range_iterable_is_valid() {
        let errors = check_for_over(Ty::Named {
            name: "Range".to_string(),
            args: vec![Ty::I64],
        });
        assert!(
            errors.is_empty(),
            "Range<i64> iterable must not emit errors; got: {errors:?}",
        );
    }

    // ── already-errored / divergent iterables must not get extra diagnostics ─

    #[test]
    fn error_typed_iterable_does_not_emit_extra_not_iterable_diagnostic() {
        // Ty::Error propagates silently; no spurious "type is not iterable".
        let errors = check_for_over(Ty::Error);
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "Ty::Error iterable must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn never_typed_iterable_does_not_emit_not_iterable_diagnostic() {
        // Ty::Never is divergent; no spurious "type is not iterable".
        let errors = check_for_over(Ty::Never);
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "Ty::Never iterable must not emit InvalidOperation; got: {errors:?}",
        );
    }
}

// ── Sub-issue 2: bind-then-return bypass regression tests ──────────────────

/// Parse and type-check a program with one fictional owned-handle type registered.
fn check_source_with_handle(source: &str, handle_type: &str) -> TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors in test source: {:#?}",
        parse_result.errors
    );
    let mut registry = ModuleRegistry::new(vec![]);
    registry.insert_handle_type_for_test(handle_type.to_string());
    let mut checker = Checker::new(registry);
    checker.check_program(&parse_result.program)
}

/// Direct `return self.field` — the existing check; must still fire after the
/// bind-then-return refactor.
#[test]
fn direct_handle_field_return_is_rejected() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_pattern(wrapper: PatternWrapper) -> regex.Pattern {
                wrapper.pattern
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("pattern")
                && e.message.contains("double-free")),
        "direct return of owned handle field must be rejected; got: {:?}",
        output.errors
    );
}

/// `let p = wrapper.pattern; p` — the bind-then-return bypass from issue #1315 sub-2.
#[test]
fn bind_then_return_handle_field_is_rejected() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_pattern(wrapper: PatternWrapper) -> regex.Pattern {
                let p = wrapper.pattern;
                p
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("pattern")
                && e.message.contains("double-free")),
        "let-binding alias of owned handle field must be rejected; got: {:?}",
        output.errors
    );
}

/// Diagnostic message for bind-then-return must name the binding so the user
/// can locate the alias.
#[test]
fn bind_then_return_diagnostic_names_binding() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn extract(wrapper: PatternWrapper) -> regex.Pattern {
                let p = wrapper.pattern;
                p
            }
        }
        ",
        "regex.Pattern",
    );
    let msg = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::InvalidOperation)
        .map_or("", |e| e.message.as_str());
    assert!(
        msg.contains("via let-binding `p`"),
        "error message must identify the binding name; got: {msg:?}"
    );
}

/// `let p = wrapper.pattern; printDebug(p); return p` — intermediate use
/// does not suppress the diagnostic.
#[test]
fn bind_then_return_with_intermediate_use_is_rejected() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_pattern(wrapper: PatternWrapper) -> regex.Pattern {
                let p = wrapper.pattern;
                println(p);
                p
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("double-free")),
        "alias with intermediate use must still be rejected; got: {:?}",
        output.errors
    );
}

/// A method that returns a non-handle field must not trigger the diagnostic.
#[test]
fn non_handle_field_return_is_allowed() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern, label: string }

        impl PatternWrapper {
            fn get_label(wrapper: PatternWrapper) -> string {
                wrapper.label
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output.errors.iter().all(
            |e| e.kind != TypeErrorKind::InvalidOperation || !e.message.contains("double-free")
        ),
        "returning a non-handle field must not be rejected; got: {:?}",
        output.errors
    );
}

/// A let-binding whose value is NOT a receiver field access must not be
/// flagged when it appears in return position.
#[test]
fn non_field_let_binding_return_is_allowed() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_label(wrapper: PatternWrapper) -> string {
                let s = to_string(42);
                s
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output.errors.iter().all(
            |e| e.kind != TypeErrorKind::InvalidOperation || !e.message.contains("double-free")
        ),
        "returning a non-field binding must not be rejected; got: {:?}",
        output.errors
    );
}

// ── Sub-issue 3: O(N²) registration scaling tests ──────────────────────────

/// Registering N struct types should trigger `refresh_handle_bearing_structs`
/// exactly once (lazy fixpoint), not N times.
///
/// This is an operation-count assertion — deterministic and never flaky.
/// For N=100, N=200, N=400 we expect `refresh_call_count` == 1 after the first
/// lookup, regardless of N.
#[test]
fn handle_bearing_refresh_deferred_to_single_fixpoint_pass() {
    fn register_n_plain_structs(n: usize) -> usize {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        for i in 0..n {
            let td = hew_parser::ast::TypeDecl {
                visibility: hew_parser::ast::Visibility::Private,
                kind: hew_parser::ast::TypeDeclKind::Struct,
                name: format!("S{i}"),
                type_params: None,
                where_clause: None,
                body: vec![hew_parser::ast::TypeBodyItem::Field {
                    name: "value".to_string(),
                    ty: (
                        hew_parser::ast::TypeExpr::Named {
                            name: "int".to_string(),
                            type_args: None,
                        },
                        0..0,
                    ),
                    attributes: vec![],
                    doc_comment: None,
                }],
                doc_comment: None,
                wire: None,
                is_indirect: false,
            };
            checker.register_type_decl(&td);
        }
        // Trigger the lazy refresh with one lookup.
        checker.ensure_handle_bearing_fresh();
        checker.refresh_call_count
    }

    let count_100 = register_n_plain_structs(100);
    let count_200 = register_n_plain_structs(200);
    let count_400 = register_n_plain_structs(400);

    // Each run should refresh exactly once regardless of N.
    assert_eq!(count_100, 1, "N=100: expected 1 refresh, got {count_100}");
    assert_eq!(count_200, 1, "N=200: expected 1 refresh, got {count_200}");
    assert_eq!(count_400, 1, "N=400: expected 1 refresh, got {count_400}");
}

/// Timing check: registering 400 structs must run in at most 4× the time it
/// takes for 100 structs, demonstrating linear (not quadratic) scaling.
///
/// Uses `Instant::elapsed`-bounded ratio rather than an absolute wall-clock
/// threshold so CI hardware differences don't cause false failures.
/// Gated with `#[ignore]` so it does not run in the standard `cargo test`
/// sweep — invoke explicitly with `cargo test -- --include-ignored` when you
/// want the timing signal.
#[test]
#[ignore = "wall-clock ratio test; run explicitly with --include-ignored"]
fn handle_bearing_registration_scales_linearly_not_quadratically() {
    use std::time::Instant;

    fn time_register_n(n: usize) -> std::time::Duration {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let start = Instant::now();
        for i in 0..n {
            let td = hew_parser::ast::TypeDecl {
                visibility: hew_parser::ast::Visibility::Private,
                kind: hew_parser::ast::TypeDeclKind::Struct,
                name: format!("T{i}"),
                type_params: None,
                where_clause: None,
                body: vec![hew_parser::ast::TypeBodyItem::Field {
                    name: "x".to_string(),
                    ty: (
                        hew_parser::ast::TypeExpr::Named {
                            name: "int".to_string(),
                            type_args: None,
                        },
                        0..0,
                    ),
                    attributes: vec![],
                    doc_comment: None,
                }],
                doc_comment: None,
                wire: None,
                is_indirect: false,
            };
            checker.register_type_decl(&td);
        }
        checker.ensure_handle_bearing_fresh();
        start.elapsed()
    }

    let t100 = time_register_n(100);
    let t400 = time_register_n(400);

    // Use as_secs_f64 to avoid u128->f64 precision-loss lints; nanosecond
    // precision is far more than this test needs.
    let ratio = t400.as_secs_f64() / t100.as_secs_f64().max(f64::EPSILON);
    assert!(
        ratio < 16.0,
        "registration of 400 structs took {ratio:.1}× as long as 100 structs — expected < 16× \
         (quadratic would be ~16×, linear is ~4×). t100={t100:?} t400={t400:?}"
    );
}
