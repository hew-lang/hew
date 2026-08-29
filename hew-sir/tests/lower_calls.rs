use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    dump_sir, lower_module, verify_module, CallableInstance, SemOpKind, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn lower_hir(source: &str) -> hew_hir::HirModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "source must parse before the SIR lowering test: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let type_check_output = checker.check_program(&parsed.program);
    let hir = lower_program_host_target(&parsed.program, &type_check_output, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "source must lower to HIR before the SIR lowering test: {:#?}",
        hir.diagnostics
    );
    hir.module
}

fn lower_source(source: &str) -> hew_sir::LoweredModule {
    let hir = lower_hir(source);
    lower_module(&hir)
}

#[test]
fn two_pass_lowering_resolves_forward_scalar_calls_through_callable_ids() {
    // `main` is intentionally written before its callee. The callable table
    // is declaration-sorted, while body lowering still handles this forward
    // edge without any name-to-symbol reconstruction.
    let lowered = lower_source(
        r"
        fn main() -> i64 {
            add_one(41)
        }

        fn add_one(value: i64) -> i64 {
            value + 1
        }
        ",
    );
    assert!(
        ["main", "add_one"].into_iter().all(|name| {
            matches!(
                lowered
                    .statuses
                    .iter()
                    .find(|(candidate, _)| candidate == name),
                Some((_, SirLoweringStatus::Lowered))
            )
        }),
        "the selected root call graph must lower even though unrelated stdlib bodies remain unsupported: {:#?}",
        lowered.statuses
    );
    let module = &lowered.module;
    let add_one = module
        .callables
        .iter()
        .find(|callable| callable.symbol == "add_one")
        .expect("scalar callee must have a resolved callable");
    let main_callable = module
        .callables
        .iter()
        .find(|callable| callable.symbol == "main")
        .expect("root main must have a resolved callable");
    assert!(
        module
            .callables
            .iter()
            .map(|callable| &callable.declaration)
            .collect::<Vec<_>>()
            .windows(2)
            .all(|pair| pair[0] <= pair[1]),
        "callable IDs must come from deterministic declaration ordering, not body order"
    );
    assert!(add_one.id < main_callable.id);
    assert_eq!(module.root_unit_callables.len(), 2);
    let entry = module
        .entry_callable
        .expect("root source main must establish an entry callable once in HIR-to-SIR lowering");
    assert_eq!(
        module
            .callable(entry)
            .map(|callable| callable.symbol.as_str()),
        Some("main")
    );
    let main = module
        .function_index()
        .function(entry)
        .expect("selected entry must have a lowered scalar SIR body");
    let callee = main
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find_map(|op| match &op.kind {
            SemOpKind::Call { callee, .. } => Some(*callee),
            _ => None,
        })
        .expect("main must contain a resolved SIR direct call");
    assert_eq!(
        module
            .callable(callee)
            .map(|callable| callable.symbol.as_str()),
        Some("add_one"),
        "SIR call must carry CallableId, whose table owns the exact HIR symbol"
    );
    assert!(
        verify_module(module).is_empty(),
        "resolved direct-call SIR must verify: {:#?}",
        verify_module(module)
    );
    assert!(dump_sir(module).contains("call @add_one(%0)"));
}

#[test]
fn unit_direct_call_is_a_zero_result_sir_operation() {
    let lowered = lower_source(
        r"
        fn unit_helper(value: i64) {
        }

        fn main() {
            unit_helper(7);
        }
        ",
    );
    let unit_helper = lowered
        .module
        .callables
        .iter()
        .find(|callable| callable.symbol == "unit_helper")
        .expect("unit-returning declaration must retain an ABI callable entry");
    assert_eq!(unit_helper.signature.return_ty, hew_types::ResolvedTy::Unit);
    assert!(
        lowered
            .module
            .function_index()
            .function(unit_helper.id)
            .is_some(),
        "unit return alone must not prevent its function body from reaching SIR"
    );
    let entry = lowered
        .module
        .entry_callable
        .expect("unit main is still the root entry callable");
    let main = lowered
        .module
        .function_index()
        .function(entry)
        .expect("unit main must lower to a SIR body");
    assert_eq!(main.return_ty, hew_types::ResolvedTy::Unit);
    let call = main
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find(|op| matches!(&op.kind, SemOpKind::Call { .. }))
        .expect("unit main must retain the direct call as SIR operation");
    assert!(
        call.results.is_empty(),
        "a unit-returning direct call must not fabricate a unit SSA value"
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "zero-result unit direct call must satisfy the module verifier: {:#?}",
        verify_module(&lowered.module)
    );
    let dump = dump_sir(&lowered.module);
    assert!(dump.contains("    call @unit_helper(%0)"));
    assert!(
        !dump.contains("= call @unit_helper"),
        "the textual SIR form must distinguish a zero-result unit call"
    );
}

#[test]
fn scalar_binding_and_explicit_return_transfers_lower_without_erasing_resource_rules() {
    let lowered = lower_source(
        r"
        fn f(x: i64, y: i64) -> i64 {
            let z = if x > 0 {
                y + 1
            } else {
                y + 2
            };
            return z * 3;
        }

        fn main() -> i64 {
            f(1, 2)
        }
        ",
    );

    assert!(
        lowered
            .statuses
            .iter()
            .filter(|(name, _)| name == "f" || name == "main")
            .all(|(_, status)| matches!(status, SirLoweringStatus::Lowered)),
        "BitCopy binding/return transfers must be admitted without a legacy fallback: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "the scalar transfer fixture must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );
    let f = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "f")
        .expect("the scalar helper must have a SIR body");
    assert!(
        f.blocks.iter().any(|block| matches!(
            block.terminator,
            hew_sir::SemTerminator::Return { value: Some(_) }
        )),
        "the explicit HIR return must remain a value-carrying SIR return"
    );
}

#[test]
fn unit_direct_call_in_an_explicit_return_is_a_control_transfer_not_a_discarded_read() {
    let lowered = lower_source(
        r"
        fn unit_helper() {
        }

        fn main() {
            return unit_helper();
        }
        ",
    );
    assert!(
        lowered
            .statuses
            .iter()
            .filter(|(name, _)| name == "unit_helper" || name == "main")
            .all(|(_, status)| matches!(status, SirLoweringStatus::Lowered)),
        "a Unit direct call returned to the caller must not be rejected as a discarded Read: {:#?}",
        lowered.statuses
    );
    let entry = lowered
        .module
        .entry_callable
        .expect("the unit main must retain a root callable");
    let main = lowered
        .module
        .function_index()
        .function(entry)
        .expect("the unit main must lower to SIR");
    assert!(
        main.blocks
            .iter()
            .flat_map(|block| &block.ops)
            .any(|operation| {
                matches!(
                    operation.kind,
                    SemOpKind::Call { .. } if operation.results.is_empty()
                )
            }),
        "the returned Unit call must remain a zero-result semantic call"
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "the Unit-return transfer fixture must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );
}

/// Recursive call resolution must use the callable table built before body
/// lowering, not a body-order-dependent symbol lookup.  This is the smallest
/// SIR-only proof that a strict component can contain a cycle.
#[test]
fn recursive_scalar_call_resolves_to_its_own_callable_id() {
    let lowered = lower_source(
        r"
        fn main() -> i64 {
            countdown(3)
        }

        fn countdown(value: i64) -> i64 {
            if value == 0 {
                0
            } else {
                countdown(value - 1)
            }
        }
        ",
    );
    assert!(
        lowered
            .statuses
            .iter()
            .filter(|(name, _)| name == "main" || name == "countdown")
            .all(|(_, status)| matches!(status, SirLoweringStatus::Lowered)),
        "recursive scalar fixture must lower as a closed SIR graph: {:#?}",
        lowered.statuses
    );

    let module = &lowered.module;
    let countdown = module
        .callables
        .iter()
        .find(|callable| callable.symbol == "countdown")
        .expect("recursive declaration must have a callable-table entry");
    let function = module
        .function_index()
        .function(countdown.id)
        .expect("recursive callable must have a lowered SIR body");
    let recursive_callee = function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find_map(|op| match &op.kind {
            SemOpKind::Call { callee, .. } => Some(*callee),
            _ => None,
        })
        .expect("recursive SIR body must contain a direct call");
    assert_eq!(
        recursive_callee, countdown.id,
        "the recursive edge must name the stable CallableId rather than a reconstructed symbol"
    );
    assert!(
        verify_module(module).is_empty(),
        "recursive direct-call SIR must verify: {:#?}",
        verify_module(module)
    );
}

/// Generic SIR instances are discovered from checked call-site facts rather
/// than copied from HIR's legacy monomorphisation registry.  This exercises
/// three core requirements together: nested generic forwarding, per-instance
/// deduplication, and a header-first self-recursive instance.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the fixture intentionally verifies one complete instance graph, malformed-IR boundary, and dump"
)]
fn generic_scalar_instances_are_closed_cached_and_template_free() {
    let mut hir = lower_hir(
        r"
        pub fn id<T>(x: T) -> T {
            let result: T = x;
            return result;
        }

        pub fn relay<U>(y: U) -> U {
            id(id(y))
        }

        pub fn countdown<T>(value: T, n: i64) -> T {
            if n == 0 {
                value
            } else {
                countdown(value, n - 1)
            }
        }

        fn main() -> i64 {
            let forwarded: i64 = relay(40);
            let counted: i64 = countdown(forwarded, 2);
            let flag: bool = id(true);
            if flag { counted } else { 0 }
        }
        ",
    );
    assert!(
        !hir.monomorphisations.is_empty(),
        "the fixture must prove SIR is not merely observing an empty legacy registry"
    );
    hir.monomorphisations.clear();

    let lowered = lower_module(&hir);
    assert!(
        verify_module(&lowered.module).is_empty(),
        "closed generic SIR instances must verify without the legacy registry: {:#?}",
        verify_module(&lowered.module)
    );
    for name in ["id", "relay", "countdown"] {
        assert!(
            matches!(
                lowered
                    .statuses
                    .iter()
                    .find(|(candidate, _)| candidate == name),
                Some((
                    _,
                    SirLoweringStatus::GenericTemplate {
                        failed_instances: 0,
                        ..
                    }
                ))
            ),
            "generic HIR origin `{name}` must remain a template, not an abstract SIR body: {:#?}",
            lowered.statuses
        );
    }
    assert!(matches!(
        lowered
            .statuses
            .iter()
            .find(|(candidate, _)| candidate == "main"),
        Some((_, SirLoweringStatus::Lowered))
    ));
    assert_eq!(
        lowered.module.generic_templates.len(),
        3,
        "SIR must retain only body-free template headers for id, relay, and countdown"
    );

    let generic_callables = lowered
        .module
        .callables
        .iter()
        .filter(|callable| matches!(&callable.instance, CallableInstance::Generic(_)))
        .collect::<Vec<_>>();
    assert_eq!(
        generic_callables.len(),
        4,
        "id<i64>, id<bool>, relay<i64>, and countdown<i64> must be the complete concrete SIR instance set: {generic_callables:#?}"
    );
    assert!(
        lowered
            .module
            .functions
            .iter()
            .all(|function| !["id", "relay", "countdown"].contains(&function.name.as_str())),
        "generic origin templates must never appear as abstract SIR functions: {:#?}",
        lowered.module.functions
    );
    for symbol in ["id$$i64", "id$$bool", "relay$$i64", "countdown$$i64"] {
        assert!(
            lowered
                .module
                .functions
                .iter()
                .any(|function| function.name == symbol),
            "missing concrete semantic SIR body `{symbol}`"
        );
    }

    let id_i64 = generic_callables
        .iter()
        .find(|callable| callable.symbol == "id$$i64")
        .expect("nested forwarding must request id<i64>");
    let id_i64_key = match &id_i64.instance {
        CallableInstance::Generic(key) => key,
        CallableInstance::Monomorphic => panic!("id<i64> must retain a semantic instance key"),
    };
    assert_eq!(
        lowered
            .module
            .callable_for_instance(id_i64_key)
            .map(|callable| callable.id),
        Some(id_i64.id),
        "instance lookup must use the closed semantic key, not a mangled symbol"
    );
    let relay_i64 = generic_callables
        .iter()
        .find(|callable| callable.symbol == "relay$$i64")
        .expect("main must request relay<i64>");
    let relay_body = lowered
        .module
        .function_index()
        .function(relay_i64.id)
        .expect("concrete relay<i64> body must be lowered");
    let relay_callees = relay_body
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .filter_map(|operation| match &operation.kind {
            SemOpKind::Call { callee, .. } => Some(*callee),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(relay_callees, vec![id_i64.id, id_i64.id]);

    let countdown_i64 = generic_callables
        .iter()
        .find(|callable| callable.symbol == "countdown$$i64")
        .expect("main must request countdown<i64>");
    let countdown_body = lowered
        .module
        .function_index()
        .function(countdown_i64.id)
        .expect("concrete countdown<i64> body must be lowered");
    assert!(countdown_body
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .any(|operation| matches!(&operation.kind, SemOpKind::Call { callee, .. } if *callee == countdown_i64.id)),
        "self-recursive concrete instance must resolve through its preallocated CallableId");
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| matches!(status, SirLoweringStatus::Lowered)),
        "every requested concrete instance must have a body: {:#?}",
        lowered.callable_statuses
    );
    let dump = dump_sir(&lowered.module);
    assert!(dump.contains("fn relay$$i64("));
    assert!(dump.contains("call @id$$i64"));
    assert!(dump.contains("fn countdown$$i64("));

    let mut forged_signature = lowered.module.clone();
    let id_i64_index = usize::try_from(id_i64.id.0).expect("callable ID fits usize");
    forged_signature.callables[id_i64_index].signature.return_ty = ResolvedTy::Bool;
    assert!(
        verify_module(&forged_signature).iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidCallable { callable, reason }
                if *callable == id_i64.id
                    && reason.contains("semantic template signature after substitution")
        )),
        "the verifier must reject a generic callable whose concrete signature does not match its key: {:#?}",
        verify_module(&forged_signature)
    );

    let mut mixed_forms = lowered.module.clone();
    let mut forged_monomorphic = (**id_i64).clone();
    forged_monomorphic.id = hew_sir::CallableId(
        u32::try_from(mixed_forms.callables.len()).expect("test callable count fits u32"),
    );
    forged_monomorphic.instance = CallableInstance::Monomorphic;
    forged_monomorphic.symbol = "forged_id_monomorphic".to_string();
    mixed_forms.callables.push(forged_monomorphic);
    assert!(
        verify_module(&mixed_forms)
            .iter()
            .any(|diagnostic| matches!(
                &diagnostic.kind,
                hew_sir::SirDiagnosticKind::InvalidCallable { reason, .. }
                    if reason.contains("generic semantic template header")
            )),
        "the verifier must reject an abstract monomorphic body alongside a generic template: {:#?}",
        verify_module(&mixed_forms)
    );
}

/// A generic instance header must be published before its body is lowered.
/// Otherwise the first body in this cycle could request `odd<i64>` but the
/// second body would be unable to resolve its edge back to `even<i64>`.
#[test]
fn generic_scalar_instances_support_mutual_recursion_without_legacy_monomorphisation() {
    let mut hir = lower_hir(
        r"
        pub fn even<T>(value: T, count: i64) -> T {
            if count == 0 {
                value
            } else {
                odd(value, count - 1)
            }
        }

        pub fn odd<T>(value: T, count: i64) -> T {
            if count == 0 {
                value
            } else {
                even(value, count - 1)
            }
        }

        fn main() -> i64 {
            even(42, 2)
        }
        ",
    );
    assert!(
        !hir.monomorphisations.is_empty(),
        "the fixture must prove SIR closes the generic cycle itself"
    );
    hir.monomorphisations.clear();

    let lowered = lower_module(&hir);
    assert!(
        verify_module(&lowered.module).is_empty(),
        "mutually recursive generic instances must verify without legacy monomorphisation: {:#?}",
        verify_module(&lowered.module)
    );
    let even = lowered
        .module
        .callables
        .iter()
        .find(|callable| callable.symbol == "even$$i64")
        .expect("main must request a concrete even<i64> callable");
    let odd = lowered
        .module
        .callables
        .iter()
        .find(|callable| callable.symbol == "odd$$i64")
        .expect("even<i64> must request a concrete odd<i64> callable");
    for (caller, expected_callee) in [(even, odd.id), (odd, even.id)] {
        let body = lowered
            .module
            .function_index()
            .function(caller.id)
            .expect("every predeclared concrete generic header must receive a body");
        assert!(
            body.blocks
                .iter()
                .flat_map(|block| &block.ops)
                .any(|operation| matches!(&operation.kind, SemOpKind::Call { callee, .. } if *callee == expected_callee)),
            "{} must retain the cross-recursive edge to callable {}",
            caller.symbol,
            expected_callee.0
        );
    }
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| matches!(status, SirLoweringStatus::Lowered)),
        "the closed mutual-recursive instance graph must have no missing body: {:#?}",
        lowered.callable_statuses
    );
}
