#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

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
            Ty::Named {
                builtin: None,
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
        "Foo".to_string(),
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
            elem_ty: None,
            consumes_receiver: false,
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
        "Good".to_string(),
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
fn checked_expression_publication_covers_blocks_empty_blocks_if_and_match_tails() {
    let source = r#"
fn nested(flag: bool) -> string {
    { if flag { "a" + "b" } else { "c" + "d" } }
}

fn selected(value: i64) -> string {
    match value {
        0 => { "zero" + "!" },
        _ => { "other" + "!" },
    }
}

fn empty() {
    {}
}
"#;
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert_eq!(
        output.produced_value_ownership.len(),
        output.expr_types.len(),
        "every surviving checked expression must publish exactly one ownership fact"
    );
    assert!(
        output
            .expr_types
            .keys()
            .all(|key| output.produced_value_ownership.contains_key(key)),
        "ownership publication must cover Block/default/If/Match expression roots"
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
    assert_eq!(
        output
            .produced_value_ownership
            .get(key)
            .map(|fact| fact.ownership),
        Some(crate::runtime_call::ProducedValueOwnership::NoOwner)
    );
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
                worker.run();
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

fn ownership_graph_key(start: usize, module_idx: u32) -> SpanKey {
    SpanKey {
        start,
        end: start + 1,
        module_idx,
    }
}

fn borrowed_fact() -> ProducedValueFact {
    ProducedValueFact::result(crate::runtime_call::ProducedValueOwnership::Borrowed)
}

#[test]
fn ownership_graph_rejects_missing_leaf_and_orphan_dependency() {
    let parent = ownership_graph_key(1, 0);
    let child = ownership_graph_key(2, 0);
    let expr_types = HashMap::from([(parent.clone(), Ty::String)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .produced_value_dependencies
        .insert(parent.clone(), ProducedValueDependency::Identity(child));

    let invalid = checker.validate_produced_value_graph(&expr_types, &HashMap::new());
    assert!(invalid.contains(&parent));
    assert!(checker.errors.iter().any(|error| {
        error.message.contains("no raw produced-value fact")
            || error.message.contains("no surviving expression")
    }));
}

#[test]
fn ownership_graph_rejects_cycle_and_cross_module_edge() {
    let first = ownership_graph_key(10, 0);
    let second = ownership_graph_key(20, 0);
    let foreign = ownership_graph_key(30, 1);
    let expr_types = HashMap::from([
        (first.clone(), Ty::String),
        (second.clone(), Ty::String),
        (foreign.clone(), Ty::String),
    ]);
    let leaves = HashMap::from([
        (first.clone(), borrowed_fact()),
        (second.clone(), borrowed_fact()),
        (foreign.clone(), borrowed_fact()),
    ]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.produced_value_dependencies.insert(
        first.clone(),
        ProducedValueDependency::Identity(second.clone()),
    );
    checker.produced_value_dependencies.insert(
        second.clone(),
        ProducedValueDependency::Join(vec![first.clone(), foreign]),
    );

    let invalid = checker.validate_produced_value_graph(&expr_types, &leaves);
    assert!(invalid.contains(&first));
    assert!(invalid.contains(&second));
    assert!(checker
        .errors
        .iter()
        .any(|error| error.message.contains("dependency cycle")));
    assert!(checker
        .errors
        .iter()
        .any(|error| error.message.contains("crosses modules")));
}

#[test]
fn ownership_graph_rejects_type_changing_identity() {
    let parent = ownership_graph_key(31, 0);
    let child = ownership_graph_key(32, 0);
    let expr_types = HashMap::from([
        (parent.clone(), Ty::result(Ty::String, Ty::String)),
        (child.clone(), Ty::String),
    ]);
    let leaves = HashMap::from([
        (parent.clone(), borrowed_fact()),
        (child.clone(), borrowed_fact()),
    ]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .produced_value_dependencies
        .insert(parent.clone(), ProducedValueDependency::Identity(child));

    let invalid = checker.validate_produced_value_graph(&expr_types, &leaves);
    assert!(invalid.contains(&parent));
    assert!(checker
        .errors
        .iter()
        .any(|error| error.message.contains("identity dependency changes type")));
}

#[test]
fn empty_select_and_match_publish_no_empty_join_but_validator_rejects_one() {
    let recovery_sources = [
        ("fn main() { let _ = select {}; }", false),
        ("fn main() { let _ = match missing() {}; }", true),
    ];

    for (source, expects_source_error) in recovery_sources {
        let parsed = hew_parser::parse(source);
        assert!(
            parsed.errors.is_empty(),
            "recovery fixture must reach the checker: {:#?}",
            parsed.errors,
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&parsed.program);
        assert_eq!(
            !output.errors.is_empty(),
            expects_source_error,
            "unexpected source diagnostics for `{source}`: {:#?}",
            output.errors,
        );
        assert!(
            output
                .errors
                .iter()
                .all(|error| !error.message.contains("produced-value graph is incomplete")),
            "recovery must preserve only source diagnostics: {:#?}",
            output.errors,
        );
        assert!(
            checker
                .produced_value_dependencies
                .values()
                .all(|dependency| !matches!(dependency, ProducedValueDependency::Join(children) if children.is_empty())),
            "source recovery must never publish an empty ownership join: {:#?}",
            checker.produced_value_dependencies,
        );
    }

    let parent = ownership_graph_key(35, 0);
    let expr_types = HashMap::from([(parent.clone(), Ty::Unit)]);
    let leaves = HashMap::from([(
        parent.clone(),
        ProducedValueFact::result(crate::runtime_call::ProducedValueOwnership::NoOwner),
    )]);
    let mut malformed = Checker::new(ModuleRegistry::new(vec![]));
    malformed
        .produced_value_dependencies
        .insert(parent.clone(), ProducedValueDependency::Join(Vec::new()));

    let invalid = malformed.validate_produced_value_graph(&expr_types, &leaves);
    assert!(invalid.contains(&parent));
    assert!(malformed
        .errors
        .iter()
        .any(|error| error.message.contains("join dependency has no children")));
}

#[test]
fn ownership_graph_requires_exact_receiver_identity_and_call_shape() {
    use crate::runtime_call::{
        ProducedArgumentBoundary as Boundary, ProducedValueOwnership as Ownership,
    };

    let receiver = ownership_graph_key(40, 0);
    let call = ownership_graph_key(50, 0);
    let mut expr_types =
        HashMap::from([(receiver.clone(), Ty::String), (call.clone(), Ty::String)]);
    let mut leaves = HashMap::from([
        (receiver.clone(), borrowed_fact()),
        (
            call.clone(),
            ProducedValueFact {
                ownership: Ownership::ReceiverIdentity,
                receiver_span: Some(receiver),
                receiver_boundary: Some(Boundary::Transfer),
                arguments: vec![Boundary::Borrow],
            },
        ),
    ]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .produced_call_arities
        .insert(call.clone(), (true, 1));
    assert!(checker
        .validate_produced_value_graph(&expr_types, &leaves)
        .is_empty());

    expr_types.insert(call.clone(), Ty::I64);
    let invalid = checker.validate_produced_value_graph(&expr_types, &leaves);
    assert!(
        invalid.contains(&call),
        "receiver-identity transfer must reject type-changing storage"
    );
    expr_types.insert(call.clone(), Ty::String);

    let malformed = leaves.get_mut(&call).expect("call fact");
    malformed.receiver_span = None;
    malformed.receiver_boundary = None;
    malformed.arguments.clear();
    let invalid = checker.validate_produced_value_graph(&expr_types, &leaves);
    assert!(invalid.contains(&call));
    assert!(checker.errors.iter().any(|error| {
        error.message.contains("receiver-identity result")
            || error.message.contains("call boundary arity mismatch")
            || error.message.contains("receiver-boundary mismatch")
    }));
}

#[test]
fn broken_dependency_cannot_publish_owned_or_inflate_final_roots() {
    use crate::runtime_call::{
        ProducedValueAcquisition as Acquisition, ProducedValueOwnership as Ownership,
    };

    let parent = ownership_graph_key(60, 0);
    let missing_child = ownership_graph_key(70, 0);
    let expr_types = HashMap::from([(parent.clone(), Ty::String)]);
    let leaves = HashMap::from([(
        parent.clone(),
        ProducedValueFact::result(Ownership::owned(Acquisition::Fresh)),
    )]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.produced_value_dependencies.insert(
        parent.clone(),
        ProducedValueDependency::Identity(missing_child),
    );
    let invalid = checker.validate_produced_value_graph(&expr_types, &leaves);
    assert!(invalid.contains(&parent));

    let mut visiting = HashSet::new();
    let mut memo = HashMap::new();
    let resolved = resolve_produced_node(
        &parent,
        &checker.produced_value_dependencies,
        &leaves,
        &expr_types,
        &checker.class_declarations(),
        &invalid,
        &mut visiting,
        &mut memo,
    );
    assert_eq!(resolved.ownership, Ownership::Unknown);
    let finalized = HashMap::from([(parent, resolved)]);
    assert_eq!(finalized.len(), expr_types.len());
}

// Helper functions for testing AST construction
