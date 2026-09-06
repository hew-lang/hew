use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BoundaryDecision, CallResult, CallUnwind, SemModule,
    SemTerminator, SirDiagnosticKind,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy, ValueCapability};

fn lower_source(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = lower_module(&hir.module, &facts);
    assert!(
        lowered.statuses.iter().any(|status| status.name == "main"
            && matches!(status.status, hew_sir::SirLoweringStatus::Lowered)),
        "{:?}",
        lowered.statuses
    );
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, hew_sir::SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "{:?}",
        verify_module(&lowered.module)
    );
    lowered.module
}

fn selected_call(capability: ValueCapability) -> SemModule {
    let (method, invocation) = match capability {
        ValueCapability::Eq => (
            "fn selected(a: i64, b: i64) -> bool { a == b }",
            "if selected(1, 2) { 1 } else { 0 }",
        ),
        ValueCapability::Hash => ("fn selected(a: i64) -> i64 { a }", "selected(1)"),
    };
    let mut module = lower_source(&format!(
        "{method} fn main() -> i64 {{ let map: HashMap<i64, string> = HashMap.new(); {invocation} }}"
    ));
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

#[test]
fn ordinary_composite_equality_demands_selected_methods() {
    for source in [
        r#"fn main() -> i64 { let a = ["one".to_upper()]; let b = ["one".to_upper()]; if a == b { 1 } else { 0 } }"#,
        r#"fn main() -> i64 { let a = ("one".to_upper(), 1); let b = ("two".to_upper(), 1); if a != b { 1 } else { 0 } }"#,
        r#"type Label { raw: string } type Outer { value: Label }
        impl Eq for Label { fn eq(self, other: Label) -> bool { self.raw.len() == other.raw.len() } }
        fn main() -> i64 { let a = Outer { value: Label { raw: "one".to_upper() } }; let b = Outer { value: Label { raw: "two".to_upper() } }; if a == b { 1 } else { 0 } }"#,
        r#"fn main() -> i64 { let a: Option<string> = Some("one".to_upper()); let b: Option<string> = None; if a != b { 1 } else { 0 } }"#,
        r#"fn main() -> i64 { let a: Result<string, i64> = Ok("one".to_upper()); let b: Result<string, i64> = Err(1); if a != b { 1 } else { 0 } }"#,
    ] {
        let module = lower_source(source);
        assert!(module
            .functions
            .iter()
            .flat_map(|f| &f.blocks)
            .any(|b| matches!(
                b.terminator,
                SemTerminator::ValueCall {
                    capability: ValueCapability::Eq,
                    ..
                }
            )));
        assert!(module
            .value_capabilities
            .keys()
            .all(|(_, capability)| *capability == ValueCapability::Eq));
    }
}

#[test]
fn equality_snapshots_a_whole_binding_before_later_mutation() {
    for read in ["left", "Vec.from(left)", "Vec.from(Vec.from(left))"] {
        let source = r#"
        fn main() -> i64 {
            var left = ["kept".to_upper()];
            let right = left;
            if $LEFT == { left.clear(); right } { 1 } else { 0 }
        }
    "#;
        let module = lower_source(&source.replace("$LEFT", read));
        let main = module.functions.iter().find(|f| f.name == "main").unwrap();
        let argument = main
            .blocks
            .iter()
            .find_map(|b| match &b.terminator {
                SemTerminator::ValueCall { args, .. } => Some(args[0].operand.value),
                _ => None,
            })
            .unwrap();
        let snapshot = main
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .find(|op| op.results.iter().any(|result| result.id == argument))
            .unwrap();
        assert_eq!(snapshot.results[0].own, hew_sir::OwnKind::Owned);
        let hew_sir::BindingTarget::Place(left) = main
            .bindings
            .iter()
            .find(|binding| binding.name == "left")
            .unwrap()
            .target
        else {
            panic!("left must own local storage")
        };
        // Identity conversions preserve the same source place. All three
        // expressions must capture its value before the later mutation.
        assert!(matches!(snapshot.kind, hew_sir::SemOpKind::LoadCopy { place } if place == left));
        let mutation = main
            .blocks
            .iter()
            .find(|block| {
                matches!(
                    block.terminator,
                    SemTerminator::RtCall {
                        family: hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Clear),
                        ..
                    }
                )
            })
            .unwrap();
        assert!(
            mutation.ops.iter().any(|op| op.id == snapshot.id),
            "snapshot must be available before the later argument clears left"
        );
    }
}

#[test]
fn selected_equality_keeps_user_method_fault_cleanup() {
    let module = lower_source(
        r#"
        type Key { label: string, divisor: i64 }
        impl Eq for Key {
            fn eq(self, other: Key) -> bool {
                let local = other.label.to_upper();
                local.len() / self.divisor == 1
            }
        }
        fn main() -> i64 {
            let left = [Key { label: "left".to_upper(), divisor: 0 }];
            let right = [Key { label: "right".to_upper(), divisor: 1 }];
            if left == right { 1 } else { 0 }
        }
    "#,
    );
    assert!(module
        .value_capabilities
        .values()
        .any(|plan| plan.callable.is_some()));
    let main = module.functions.iter().find(|f| f.name == "main").unwrap();
    let failure = main
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
    let cleanup = &main.blocks[failure.0 as usize];
    assert!(matches!(cleanup.terminator, SemTerminator::ResumeUnwind));
    for name in ["left", "right"] {
        let hew_sir::BindingTarget::Place(owner) = main
            .bindings
            .iter()
            .find(|binding| binding.name == name)
            .unwrap()
            .target
        else {
            panic!("equality input must have local storage")
        };
        assert_eq!(cleanup.ops.iter().filter(|op| matches!(op.kind, hew_sir::SemOpKind::EndLifetime { place } if place == owner)).count(), 1);
    }
}

#[test]
fn scalar_float_comparison_preserves_its_numeric_operation() {
    let module = lower_source("type Wrapped { value: f64 } fn main() -> i64 { let x = 0.0 / 0.0; let a = Wrapped { value: x }; let b = Wrapped { value: x }; if (x == x) == (a == b) { 1 } else { 0 } }");
    let mut scalar = false;
    let mut selected = false;
    for block in module.functions.iter().flat_map(|f| &f.blocks) {
        scalar |= block.ops.iter().any(|op| {
            matches!(
                op.kind,
                hew_sir::SemOpKind::Binary {
                    op: hew_parser::ast::BinaryOp::Equal,
                    ..
                }
            )
        });
        selected |= matches!(block.terminator, SemTerminator::ValueCall { .. });
    }
    assert!(scalar && selected);
}

#[test]
fn collection_field_insertion_snapshots_its_own_parent_argument() {
    let module = lower_source(
        r#"
        type Tree { label: string, children: Vec<Tree> }
        fn main() -> i64 {
            var tree = Tree { label: "root".to_upper(), children: Vec.new() };
            tree.children.push(tree);
            tree.children[0].label.len()
        }
    "#,
    );
    assert!(verify_module(&module).is_empty());
}

#[test]
fn permanent_selected_equality_sources_verify() {
    for source in [
        include_str!("../../tests/core-acceptance/cases/selected-composite-equality.hew"),
        include_str!("../../tests/core-acceptance/cases/selected-bytes-equality.hew"),
        include_str!("../../tests/core-acceptance/cases/selected-equality-callback-fault.hew"),
        include_str!("../../tests/core-acceptance/cases/selected-equality-read-order.hew"),
        include_str!("../../tests/core-acceptance/cases/collection-parent-argument.hew"),
    ] {
        lower_source(source);
    }
}
