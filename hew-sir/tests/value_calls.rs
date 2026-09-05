use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BoundaryDecision, CallResult, CallUnwind, SemModule,
    SemTerminator, SirDiagnosticKind,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy, ValueCapability};

fn selected_call(capability: ValueCapability) -> SemModule {
    let (method, invocation) = match capability {
        ValueCapability::Eq => (
            "fn selected(a: i64, b: i64) -> bool { a == b }",
            "if selected(1, 2) { 1 } else { 0 }",
        ),
        ValueCapability::Hash => ("fn selected(a: i64) -> i64 { a }", "selected(1)"),
    };
    let parsed = hew_parser::parse(&format!(
        "{method} fn main() -> i64 {{ let map: HashMap<i64, string> = HashMap.new(); {invocation} }}"
    ));
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let mut module = lower_module(&hir.module, &facts).module;
    assert!(verify_module(&module).is_empty());
    let target = module
        .functions
        .iter()
        .find(|f| f.name == "selected")
        .unwrap()
        .callable;
    let mut converted = 0;
    for function in &mut module.functions {
        for block in &mut function.blocks {
            if let SemTerminator::Call {
                id,
                callee,
                args,
                result,
                normal,
                unwind,
            } = &block.terminator
            {
                if *callee != target {
                    continue;
                }
                let mut args = args.clone();
                for arg in &mut args {
                    arg.decision = BoundaryDecision::Borrow;
                }
                block.terminator = SemTerminator::ValueCall {
                    id: *id,
                    ty: ResolvedTy::I64,
                    capability,
                    args,
                    result: result.clone(),
                    normal: normal.clone(),
                    unwind: unwind.clone(),
                };
                converted += 1;
            }
        }
    }
    assert_eq!(converted, 1);
    assert!(
        verify_module(&module).is_empty(),
        "{:?}",
        verify_module(&module)
    );
    module
}

fn value_call(module: &mut SemModule) -> &mut SemTerminator {
    &mut module
        .functions
        .iter_mut()
        .flat_map(|f| &mut f.blocks)
        .find(|b| matches!(b.terminator, SemTerminator::ValueCall { .. }))
        .unwrap()
        .terminator
}

#[test]
fn selected_methods_share_verified_call_control_flow() {
    for capability in [ValueCapability::Hash, ValueCapability::Eq] {
        let mut module = selected_call(capability);
        hew_sir::canonicalize_module_constant_cfg(&mut module).unwrap();
        assert!(verify_module(&module).is_empty());
        assert!(hew_sir::dump_sir(&module).contains("value.call"));
    }
}

#[test]
fn selected_method_calls_require_the_exact_plan() {
    for capability in [ValueCapability::Hash, ValueCapability::Eq] {
        let mut module = selected_call(capability);
        module
            .value_capabilities
            .remove(&(ResolvedTy::I64, capability));
        let diagnostics = verify_module(&module);
        assert!(diagnostics.iter().any(|d| matches!(&d.kind,
            SirDiagnosticKind::InvalidValueCapability { ty: ResolvedTy::I64, capability: actual, reason }
            if *actual == capability && reason.contains("value call requires"))));
    }
}

#[test]
fn selected_method_call_shape_rejects_owned_transfers_and_mismatched_values() {
    for mutation in 0..5 {
        let mut module = selected_call(ValueCapability::Eq);
        let SemTerminator::ValueCall {
            ty,
            args,
            result,
            unwind,
            ..
        } = value_call(&mut module)
        else {
            unreachable!()
        };
        match mutation {
            0 => args[0].decision = BoundaryDecision::Copy,
            1 => {
                args.pop();
            }
            2 => *ty = ResolvedTy::Bool,
            3 => {
                let CallResult::Value(value) = result else {
                    unreachable!()
                };
                value.ty = ResolvedTy::I64;
            }
            4 => *unwind = CallUnwind::NotApplicable,
            _ => unreachable!(),
        }
        assert!(
            verify_module(&module).iter().any(|d| matches!(&d.kind,
            SirDiagnosticKind::InvalidOperation { reason, .. } if reason.starts_with("selected"))),
            "mutation {mutation}"
        );
    }
}

#[test]
fn selected_method_failure_cannot_discard_its_fault() {
    let mut module = selected_call(ValueCapability::Eq);
    let main = module
        .functions
        .iter_mut()
        .find(|f| f.name == "main")
        .unwrap();
    let target = main
        .blocks
        .iter()
        .find_map(|b| match &b.terminator {
            SemTerminator::ValueCall {
                unwind: CallUnwind::Cleanup(edge),
                ..
            } => Some(edge.target),
            _ => None,
        })
        .unwrap();
    main.blocks[target.0 as usize].terminator = SemTerminator::Unreachable;
    assert!(verify_module(&module)
        .iter()
        .any(|d| matches!(d.kind, SirDiagnosticKind::FaultLifetime { .. })));
}
