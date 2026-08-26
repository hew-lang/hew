use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{dump_sir, lower_module, verify_module, SemOpKind, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_source(source: &str) -> hew_sir::LoweredModule {
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
    lower_module(&hir.module)
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
        .function_for_callable(entry)
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
            .function_for_callable(unit_helper.id)
            .is_some(),
        "unit return alone must not prevent its function body from reaching SIR"
    );
    let entry = lowered
        .module
        .entry_callable
        .expect("unit main is still the root entry callable");
    let main = lowered
        .module
        .function_for_callable(entry)
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
        .function_for_callable(entry)
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
        .function_for_callable(countdown.id)
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
