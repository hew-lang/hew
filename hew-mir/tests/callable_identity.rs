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
    let published = hir
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Impl(block) => block
                .method_symbols
                .iter()
                .zip(&block.method_ids)
                .find(|(symbol, _)| symbol.ends_with("bump"))
                .and_then(|(_, id)| id.clone()),
            _ => None,
        })
        .expect("the checker must publish the impl method's declaration identity");
    let pipeline = lower_hir_module(&hir.module);
    let method = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name.ends_with("bump"))
        .expect("the inherent method must be lowered");

    assert_eq!(
        method.key,
        MirCallableKey::declared(published),
        "the key must be the checker-published declaration, projected verbatim"
    );
    assert_ne!(
        method.key.declaration.full_path(),
        method.name,
        "and it must not be recoverable from the emitted symbol"
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

/// Every synthesized child in the pipeline, as `(emitted name, parent key,
/// producer variant)`. Producer coverage is asserted through this rather than
/// through emitted-name shapes, so a rename of a compiler-internal symbol does
/// not silently drop a variant's coverage.
fn synthesized_children(
    pipeline: &IrPipeline,
) -> Vec<(&str, &MirCallableKey, SynthesizedCallable)> {
    pipeline
        .raw_mir
        .iter()
        .filter_map(|function| match &function.key.instance {
            MirCallableInstance::Synthesized { parent, child } => {
                Some((function.name.as_str(), parent.as_ref(), *child))
            }
            _ => None,
        })
        .collect()
}

#[test]
fn a_named_function_used_as_a_value_is_keyed_under_the_first_body_that_references_it() {
    let hir = hir_of(
        r"
        fn twice(x: i64) -> i64 { x * 2 }
        fn apply(f: fn(i64) -> i64, x: i64) -> i64 { f(x) }

        fn main() -> i64 { apply(twice, 4) }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let main_key = raw(&pipeline, "main").key.clone();
    let twice_key = raw(&pipeline, "twice").key.clone();

    let shims: Vec<_> = synthesized_children(&pipeline)
        .into_iter()
        .filter(|(_, _, child)| matches!(child, SynthesizedCallable::NamedFnInvokeShim(_)))
        .collect();
    assert_eq!(shims.len(), 1, "one fn-value reference, one shim");
    assert_eq!(
        *shims[0].1, main_key,
        "the shim belongs to the body that referenced the function, not to the target"
    );
    assert_ne!(
        *shims[0].1, twice_key,
        "keying the shim under its TARGET would make two references from different \
         bodies collide"
    );
}

#[test]
fn a_named_function_referenced_by_two_bodies_keys_its_one_shim_under_the_first_referencer() {
    // Counterfactual for the pin above: with TWO bodies referencing `twice` as
    // a value, `flatten_generated_functions` still dedups the shim module-wide
    // by emitted name (`lower/mod.rs`), so only one shim is ever emitted — and
    // its `MirCallableKey` parent is whichever referencing body forked the
    // shim first (`lower/expr.rs`'s mint site), not the second referencer.
    // This is the approximation marked at both sites and in `.tmp/TODO.md`.
    let hir = hir_of(
        r"
        fn twice(x: i64) -> i64 { x * 2 }
        fn apply(f: fn(i64) -> i64, x: i64) -> i64 { f(x) }

        fn first_ref() -> i64 { apply(twice, 4) }
        fn second_ref() -> i64 { apply(twice, 5) }

        fn main() -> i64 { first_ref() + second_ref() }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let first_ref_key = raw(&pipeline, "first_ref").key.clone();
    let second_ref_key = raw(&pipeline, "second_ref").key.clone();

    let shims: Vec<_> = synthesized_children(&pipeline)
        .into_iter()
        .filter(|(_, _, child)| matches!(child, SynthesizedCallable::NamedFnInvokeShim(_)))
        .collect();
    assert_eq!(
        shims.len(),
        1,
        "two references to the same named fn still dedup to one shim body"
    );
    assert_eq!(
        *shims[0].1, first_ref_key,
        "the shim is parented on the first referencing body in lowering encounter order"
    );
    assert_ne!(
        *shims[0].1, second_ref_key,
        "the second referencer reuses the already-minted shim rather than getting its own"
    );
}

#[test]
fn a_generator_body_is_keyed_as_a_child_of_its_shell() {
    let hir = hir_of(
        r"
        gen fn counter(n: i64) -> i64 {
            var i = 0;
            while i < n {
                yield i;
                i = i + 1;
            }
        }

        fn main() {
            for x in counter(3) {
                println(x);
            }
        }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let shell_key = raw(&pipeline, "counter").key.clone();

    let bodies: Vec<_> = synthesized_children(&pipeline)
        .into_iter()
        .filter(|(_, _, child)| matches!(child, SynthesizedCallable::GeneratorBody(_)))
        .collect();
    assert_eq!(bodies.len(), 1, "one `gen fn`, one coroutine body");
    assert_eq!(
        *bodies[0].1, shell_key,
        "the coroutine body is a child of the generator shell's declaration"
    );
}

#[test]
fn fork_entry_and_task_entry_shims_are_children_of_the_forking_body() {
    let hir = hir_of(
        r"
        fn add_print(a: i64, b: i64) {
            println(a + b);
        }

        fn tick() {
            println(1);
        }

        actor Driver {
            receive fn go() -> i64 {
                scope {
                    fork t = add_print(20, 22);
                    await t;
                };
                scope {
                    fork u = tick();
                    await u;
                };
                7
            }
        }

        fn main() -> i64 {
            let d = spawn Driver;
            match await d.go() {
                .Ok(v) => v - 7,
                .Err(_e) => 1,
            }
        }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let children = synthesized_children(&pipeline);

    let fork_shims: Vec<_> = children
        .iter()
        .filter(|(_, _, child)| matches!(child, SynthesizedCallable::ForkEntryShim(_)))
        .collect();
    assert_eq!(
        fork_shims.len(),
        1,
        "one arg-bearing `fork`, one entry shim"
    );
    let adapters: Vec<_> = children
        .iter()
        .filter(|(_, _, child)| matches!(child, SynthesizedCallable::TaskEntryAdapter(_)))
        .collect();
    assert_eq!(
        adapters.len(),
        1,
        "one no-arg unit `fork` callee, one task-entry adapter"
    );

    let handler_key = &raw(&pipeline, "Driver__recv__go").key;
    assert_eq!(
        fork_shims[0].1, handler_key,
        "the fork shim belongs to the handler that forked"
    );
    assert_eq!(
        adapters[0].1, handler_key,
        "the adapter is minted while lowering the same handler"
    );
    assert_ne!(
        fork_shims[0].0, adapters[0].0,
        "the two producers must not collapse onto one emitted body"
    );
    assert_ne!(
        fork_shims[0].2, adapters[0].2,
        "one parent, two producers — the variant is what keeps the keys apart"
    );
}

#[test]
fn a_lambda_actor_body_is_keyed_as_a_child_of_the_spawning_body() {
    let hir = hir_of(
        r"
        fn main() {
            let dbl = actor |n: i64| -> i64 {
                n * 2
            };
            match dbl(5) {
                .Ok(v) => println(v),
                .Err(_) => println(0),
            }
        }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let main_key = raw(&pipeline, "main").key.clone();

    let bodies: Vec<_> = synthesized_children(&pipeline)
        .into_iter()
        .filter(|(_, _, child)| matches!(child, SynthesizedCallable::LambdaActorBody(_)))
        .collect();
    assert_eq!(bodies.len(), 1, "one actor literal, one body");
    assert_eq!(
        *bodies[0].1, main_key,
        "the lambda-actor body is a child of the body that spawned it"
    );
}

#[test]
#[allow(
    deprecated,
    reason = "pins the same legacy_reconstruct_from_full_path path machine_synth.rs takes; \
              production call sites are covered by lower/mod.rs's module-level allow"
)]
fn an_actor_receive_handler_is_keyed_by_its_reconstructed_declaration_not_synthesized() {
    // Representative pin for the eight `machine_synth.rs` sites (receive
    // handlers, init/lifecycle hooks, supervisor bootstrap) that mint a
    // synthetic `HirFn` whose `declaration` is
    // `DefId::legacy_reconstruct_from_full_path("<Actor>::<handler>")` rather
    // than a resolver-minted id (`HirActorReceiveFn` carries none — see
    // `identity.rs`'s module doc and `.tmp/TODO.md`). Unlike the shim/adapter
    // producers, this handler is lowered as an ordinary non-generic `HirFn`,
    // so its key is `Monomorphic` over that reconstructed declaration — NOT
    // `Synthesized` — even though the declaration itself is fabricated.
    let hir = hir_of(
        r"
        actor Prices {
            receive fn update(price: i64) {}
        }
        ",
    );
    let pipeline = lower_hir_module(&hir.module);
    let handler = raw(&pipeline, "Prices__recv__update");

    assert_eq!(
        handler.key.instance,
        MirCallableInstance::Monomorphic,
        "a receive handler is a plain body realization, not a synthesized child"
    );
    assert_eq!(
        handler.key.declaration,
        DefId::legacy_reconstruct_from_full_path("Prices::update"),
        "the handler's key is anchored on the qualified path machine_synth.rs reconstructs"
    );
}

#[test]
fn a_machine_step_is_keyed_by_the_machine_declaration_and_its_type_arguments() {
    // The machine step is the one synthesized producer whose parent is NOT a
    // function: it projects `HirMachineDecl::declaration`. Reconstructing that
    // owner from the machine's qualified name would make the presentation
    // spelling a second identity authority.
    let hir = hir_of(
        r"
        machine Lifecycle<T> {
            events { ev; }
            state Start;
            state Mid;
            on ev: Start => Mid { .Mid }
            on ev: _ => _ { state }
        }

        type Box { m: Lifecycle<i64>, x: i64 }

        fn main() {
            let b = Box { m: Lifecycle.Start, x: 0 };
            println(b.x);
        }
        ",
    );
    let declaration = hir
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Machine(machine) if machine.name == "Lifecycle" => {
                Some(machine.declaration.clone())
            }
            _ => None,
        })
        .expect("the machine declaration must be lowered");
    let pipeline = lower_hir_module(&hir.module);

    let steps: Vec<_> = synthesized_children(&pipeline)
        .into_iter()
        .filter(|(_, _, child)| matches!(child, SynthesizedCallable::MachineStep))
        .collect();
    assert_eq!(steps.len(), 1, "one realized machine layout, one step");
    assert_eq!(
        *steps[0].1,
        MirCallableKey::instance(declaration, vec![ResolvedTy::I64]),
        "the step's parent is the machine declaration at the realized type arguments"
    );
}

// ── cross-producer agreement ────────────────────────────────────────────────

#[test]
fn sir_bridge_and_legacy_lowering_agree_on_a_call_free_monomorphic_key() {
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
    let sir = hew_sir::lower_module(&hir.module, &type_check);
    assert!(
        hew_sir::verify_module(&sir.module).is_empty(),
        "SIR must verify: {:#?}",
        hew_sir::verify_module(&sir.module)
    );
    let helper = sir
        .module
        .callables
        .iter()
        .find(|callable| callable.symbol == "helper")
        .expect("helper must have a resolved SIR callable");
    let strict = lower_closed_scalar_component(&sir.module, &[helper.id])
        .expect("call-free scalar helper must lower")
        .into_pipeline();

    assert_eq!(
        raw(&legacy, "helper").key,
        raw(&strict, "helper").key,
        "the two producers must realize `helper` under one identity"
    );
}

#[test]
fn sir_bridge_keys_a_generic_instance_by_its_declared_type_arguments() {
    // The `CallableInstance::Generic` arm of the bridge's key projection. Its
    // counterfactual is the arm above: a Monomorphic callable must not acquire
    // an (empty) type-argument list, or the two arms would be interchangeable.
    let source = r"
        fn id<T>(x: T) -> T { x }

        fn main() -> i64 { id(41) }
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
    let origin = declaration_of(&hir, "id");

    let sir = hew_sir::lower_module(&hir.module, &type_check);
    let instance = sir
        .module
        .callables
        .iter()
        .find(|callable| callable.declaration == origin)
        .expect("SIR must own the requested generic instance");
    let strict = lower_closed_scalar_component(&sir.module, &[instance.id])
        .expect("call-free concrete identity body must lower")
        .into_pipeline();

    let instance = strict
        .raw_mir
        .iter()
        .find(|function| function.key.declaration == origin)
        .expect("the strict component must realize the requested instance of `id`");
    assert_eq!(
        instance.key,
        MirCallableKey::instance(origin, vec![ResolvedTy::I64]),
        "the bridge must project SirInstanceKey::type_args, not the mangled symbol `{}`",
        instance.name
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

// ── the gate is reachable from the production lowering entry point ───────────

/// Duplicate one HIR function item under a second emitted name, keeping the
/// resolver's declaration identity. This is exactly the shape HIR used to emit
/// for a file-imported `pub fn` (bare spelling plus module-qualified spelling),
/// and the shape a future producer bug would reintroduce.
fn duplicate_function_item_under_a_second_name(
    module: &mut hew_hir::HirModule,
    name: &str,
    second_name: &str,
) {
    let mut clone = module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => Some(function.clone()),
            _ => None,
        })
        .unwrap_or_else(|| panic!("HIR function `{name}` not found"));
    clone.id = hew_hir::ItemId(u32::MAX);
    clone.name = second_name.to_string();
    module.items.push(HirItem::Function(clone));
}

#[test]
fn lower_hir_module_reports_a_module_that_realizes_one_declaration_twice() {
    // In-situ reachability: the gate must fire through `lower_hir_module`, not
    // only when `validate_unique_callable_keys` is called directly. Deleting the
    // call site — or reintroducing a dedup that drops one of the two bodies
    // before the gate runs — fails this test.
    let mut hir = hir_of(
        r"
        fn measure(x: i64) -> i64 { x + 1 }

        fn main() -> i64 { measure(1) }
        ",
    );
    duplicate_function_item_under_a_second_name(&mut hir.module, "measure", "lib$measure");

    let pipeline = lower_hir_module(&hir.module);
    let collisions: Vec<&MirDiagnosticKind> = pipeline
        .diagnostics
        .iter()
        .map(|diagnostic| &diagnostic.kind)
        .filter(|kind| matches!(kind, MirDiagnosticKind::CallableKeyCollision { .. }))
        .collect();
    assert_eq!(
        collisions.len(),
        1,
        "one duplicated declaration, one finding: {:#?}",
        pipeline.diagnostics
    );
    let MirDiagnosticKind::CallableKeyCollision {
        first_symbol,
        second_symbol,
        ..
    } = collisions[0]
    else {
        unreachable!("filtered above");
    };
    assert_eq!(
        (first_symbol.as_str(), second_symbol.as_str()),
        ("measure", "lib$measure"),
        "the diagnostic must name both emitted symbols"
    );
}

#[test]
fn lower_hir_module_accepts_the_same_module_without_the_duplicate() {
    // Negative control for the pin above: the gate must not fire on the module
    // as the producer actually emits it, or the test above would pass for a
    // reason unrelated to the duplication.
    let hir = hir_of(
        r"
        fn measure(x: i64) -> i64 { x + 1 }

        fn main() -> i64 { measure(1) }
        ",
    );

    let pipeline = lower_hir_module(&hir.module);
    assert!(
        !pipeline.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            MirDiagnosticKind::CallableKeyCollision { .. }
        )),
        "a module with one body per declaration must not collide: {:#?}",
        pipeline.diagnostics
    );
}
