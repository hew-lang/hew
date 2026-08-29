//! Behavioural pins for resolver-anchored callable identity (S6a).
//!
//! Each pin states a fact about the `MirCallableKey` a producer mints that the
//! emitted `name` alone cannot state, and each ships with the counterfactual
//! that would hold if the key were derived from the name instead.

use hew_hir::{lower_program, lower_program_host_target, HirItem, ResolutionCtx};
use hew_mir::{
    identity::validate_unique_callable_keys, lower_closed_scalar_component, lower_hir_module,
    BasicBlock, FunctionCallConv, IrPipeline, MirCallableInstance, MirCallableKey,
    MirDiagnosticKind, RawMirFunction, SourceOrigin, SynthesizedCallable, Terminator,
};
use hew_types::{module_registry::ModuleRegistry, Checker, DefId, ResolvedTy};

fn hir_of(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let type_check = checker.check_program(&parsed.program);
    assert!(
        type_check.errors.is_empty(),
        "type errors: {:#?}",
        type_check.errors
    );
    lower_program(
        &parsed.program,
        &type_check,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn raw<'a>(pipeline: &'a IrPipeline, name: &str) -> &'a RawMirFunction {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| {
            panic!(
                "raw function `{name}` not lowered; got {:?}",
                pipeline
                    .raw_mir
                    .iter()
                    .map(|f| f.name.as_str())
                    .collect::<Vec<_>>()
            )
        })
}

fn declaration_of(hir: &hew_hir::LowerOutput, name: &str) -> DefId {
    hir.module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => {
                Some(function.declaration.clone())
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("HIR function `{name}` not found"))
}

// ── legacy monomorphic producer ─────────────────────────────────────────────

#[test]
fn monomorphic_function_key_is_its_resolver_declaration() {
    let hir = hir_of(
        r#"
        fn measure(s: string) -> i64 {
            s.len()
        }

        fn main() -> i64 {
            measure("abc")
        }
        "#,
    );
    let pipeline = lower_hir_module(&hir.module);

    assert_eq!(
        raw(&pipeline, "measure").key,
        MirCallableKey::declared(declaration_of(&hir, "measure")),
        "a monomorphic body must carry the resolver's declaration identity verbatim"
    );
}

#[test]
fn method_key_is_the_declaration_path_not_the_emitted_symbol() {
    // The counterfactual for the pin above: an inherent method's emitted symbol
    // (`Counter::bump`) is NOT its declaration path, so a key reconstructed
    // from the name would differ from the one the resolver minted.
    let hir = hir_of(
        r"
        type Counter { value: i64 }

        impl Counter {
            fn bump(self) -> i64 { self.value + 1 }
        }

        fn main() -> i64 {
            let c = Counter { value: 1 };
            c.bump()
        }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let method = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name.ends_with("bump"))
        .expect("the inherent method must be lowered");

    assert_ne!(
        method.key.declaration.full_path(),
        method.name,
        "declaration identity must not be recoverable from the emitted symbol"
    );
    assert!(
        method.key.declaration.full_path().contains("Counter"),
        "the declaration path must name the owning type: {}",
        method.key.declaration.full_path()
    );
}

// ── generic instances ───────────────────────────────────────────────────────

#[test]
fn generic_instances_carry_their_declared_type_arguments() {
    let hir = hir_of(
        r"
        fn id<T>(x: T) -> T { x }

        fn main() -> i64 {
            let n = id(7);
            let s = id(true);
            if s { n } else { 0 }
        }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let origin = declaration_of(&hir, "id");

    let instances: Vec<&RawMirFunction> = pipeline
        .raw_mir
        .iter()
        .filter(|function| function.key.declaration == origin)
        .collect();
    assert_eq!(
        instances.len(),
        2,
        "both instantiations must be emitted: {:?}",
        instances
            .iter()
            .map(|f| f.name.as_str())
            .collect::<Vec<_>>()
    );

    let mut type_args: Vec<Vec<ResolvedTy>> = instances
        .iter()
        .map(|function| match &function.key.instance {
            MirCallableInstance::Generic { type_args } => type_args.clone(),
            other => panic!("a monomorphisation must be a Generic instance, got {other:?}"),
        })
        .collect();
    type_args.sort_by_key(|args| format!("{args:?}"));
    assert_eq!(
        type_args,
        vec![vec![ResolvedTy::Bool], vec![ResolvedTy::I64]],
        "each instance carries the exact declared-order arguments it was keyed by"
    );
    assert_ne!(
        instances[0].key, instances[1].key,
        "two instances of one declaration must not share a key"
    );
}

#[test]
fn abstract_generic_origin_key_is_distinct_from_every_instance() {
    // Negative control for the instance pin: the substrate body lowered against
    // `TypeParam` operands shares the declaration but must never compare equal
    // to a concrete instance of it.
    let hir = hir_of(
        r"
        fn id<T>(x: T) -> T { x }

        fn main() -> i64 { id(7) }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let origin = declaration_of(&hir, "id");

    let polymorphic = pipeline
        .polymorphic_mir
        .iter()
        .find(|function| function.raw.key.declaration == origin)
        .expect("the abstract origin must be captured in the representation substrate");
    assert_eq!(
        polymorphic.raw.key,
        MirCallableKey::polymorphic(origin.clone()),
        "the abstract origin is Polymorphic, not a Generic instance"
    );

    for instance in pipeline
        .raw_mir
        .iter()
        .filter(|function| function.key.declaration == origin)
    {
        assert_ne!(
            instance.key, polymorphic.raw.key,
            "instance `{}` must not share the abstract origin's key",
            instance.name
        );
    }
}

// ── synthesized children ────────────────────────────────────────────────────

#[test]
fn closures_are_keyed_as_ordinal_children_of_their_enclosing_function() {
    let hir = hir_of(
        r"
        fn apply(f: fn(i64) -> i64, v: i64) -> i64 { f(v) }

        fn main() -> i64 {
            let n = 3;
            let a = |x: i64| x + n;
            let b = |x: i64| x * n;
            apply(a, 1) + apply(b, 2)
        }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let main_key = raw(&pipeline, "main").key.clone();

    let mut children: Vec<&MirCallableKey> = pipeline
        .raw_mir
        .iter()
        .filter(|function| function.name.starts_with("__hew_closure_invoke_main_"))
        .map(|function| &function.key)
        .collect();
    children.sort_by_key(|key| format!("{key:?}"));
    assert_eq!(children.len(), 2, "both closure shims must be lowered");

    assert_eq!(
        *children[0],
        main_key.child(SynthesizedCallable::ClosureInvokeShim(0))
    );
    assert_eq!(
        *children[1],
        main_key.child(SynthesizedCallable::ClosureInvokeShim(1))
    );
}

#[test]
fn closures_in_different_functions_do_not_share_a_synthesized_key() {
    // Negative control for the ordinal pin: the ordinal restarts per parent, so
    // the parent chain — not the ordinal — is what keeps the keys apart.
    let hir = hir_of(
        r"
        fn make_adder(n: i64) -> fn(i64) -> i64 { |x: i64| x + n }
        fn make_scaler(n: i64) -> fn(i64) -> i64 { |x: i64| x * n }

        fn main() -> i64 {
            let a = make_adder(1);
            let s = make_scaler(2);
            a(1) + s(2)
        }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let adder_child = &raw(&pipeline, "__hew_closure_invoke_make_adder_0").key;
    let scaler_child = &raw(&pipeline, "__hew_closure_invoke_make_scaler_0").key;

    assert_ne!(
        adder_child, scaler_child,
        "two first closures under different parents must not share a key"
    );
    let MirCallableInstance::Synthesized {
        child: adder_ordinal,
        ..
    } = &adder_child.instance
    else {
        panic!("a closure shim must be a Synthesized instance");
    };
    let MirCallableInstance::Synthesized {
        child: scaler_ordinal,
        ..
    } = &scaler_child.instance
    else {
        panic!("a closure shim must be a Synthesized instance");
    };
    assert_eq!(
        adder_ordinal, scaler_ordinal,
        "the ordinals are per-parent, so both are child 0 — only the parent separates them"
    );
}

// ── cross-producer agreement ────────────────────────────────────────────────

#[test]
fn sir_bridge_and_legacy_lowering_agree_on_a_monomorphic_key() {
    let source = r"
        fn helper(x: i64) -> i64 { x + 1 }

        fn main() -> i64 { helper(41) }
        ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let type_check = checker.check_program(&parsed.program);
    assert!(
        type_check.errors.is_empty(),
        "type errors: {:#?}",
        type_check.errors
    );
    let hir = lower_program_host_target(&parsed.program, &type_check, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );

    let legacy = lower_hir_module(&hir.module);
    let sir = hew_sir::lower_module(&hir.module);
    assert!(
        hew_sir::verify_module(&sir.module).is_empty(),
        "SIR must verify: {:#?}",
        hew_sir::verify_module(&sir.module)
    );
    let entry = sir
        .module
        .entry_callable
        .expect("scalar root main must be a strict SIR entry");
    let strict = lower_closed_scalar_component(&sir.module, &[entry])
        .expect("closed scalar component must lower")
        .into_pipeline();

    for name in ["main", "helper"] {
        assert_eq!(
            raw(&legacy, name).key,
            raw(&strict, name).key,
            "the two producers must realize `{name}` under one identity"
        );
    }
    assert_ne!(
        raw(&strict, "main").key,
        raw(&strict, "helper").key,
        "distinct declarations must keep distinct keys on the strict path too"
    );
}

// ── fail-closed uniqueness ──────────────────────────────────────────────────

fn probe(name: &str, key: MirCallableKey) -> RawMirFunction {
    RawMirFunction {
        name: name.to_string(),
        key,
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![],
        local_names: vec![],
        local_scopes: vec![],
        local_decl_bytes: vec![],
        scope_table: vec![],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: vec![],
        span: None,
        instr_spans: std::collections::BTreeMap::new(),
        source_origin: SourceOrigin::Unknown,
    }
}

#[test]
fn two_bodies_claiming_one_callable_key_are_rejected() {
    let shared = MirCallableKey::for_test("app.duplicated");
    let module = vec![
        probe("app$duplicated", shared.clone()),
        probe("app$duplicated$again", shared),
    ];

    let diagnostics = validate_unique_callable_keys(&module);
    assert_eq!(diagnostics.len(), 1, "one collision, one finding");
    let MirDiagnosticKind::CallableKeyCollision {
        declaration,
        first_symbol,
        second_symbol,
    } = &diagnostics[0].kind
    else {
        panic!("a key collision must surface as CallableKeyCollision");
    };
    assert_eq!(declaration, "app.duplicated");
    assert_eq!(first_symbol, "app$duplicated");
    assert_eq!(second_symbol, "app$duplicated$again");
}

#[test]
fn distinct_callable_keys_under_one_emitted_name_are_accepted() {
    // The negative control, and the reason the rule is keyed on identity rather
    // than on the symbol: two bodies may legitimately share a presentation
    // name (a local declaration and an imported same-leaf one) as long as their
    // declarations differ.
    let module = vec![
        probe("render", MirCallableKey::for_test("left.render")),
        probe("render", MirCallableKey::for_test("right.render")),
    ];

    assert!(
        validate_unique_callable_keys(&module).is_empty(),
        "equal emitted names with distinct declarations are not a collision"
    );
}

#[test]
fn one_declaration_lowered_at_two_instances_is_not_a_collision() {
    let declaration = DefId::for_test("app.id");
    let module = vec![
        probe(
            "app$id$$i64",
            MirCallableKey::instance(declaration.clone(), vec![ResolvedTy::I64]),
        ),
        probe(
            "app$id$$bool",
            MirCallableKey::instance(declaration, vec![ResolvedTy::Bool]),
        ),
    ];

    assert!(
        validate_unique_callable_keys(&module).is_empty(),
        "distinct type arguments are distinct identities"
    );
}
