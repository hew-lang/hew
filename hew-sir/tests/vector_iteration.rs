use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{lower_module, verify_module, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_source(source: &str) -> hew_sir::SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR: {:#?}; rewrites: {:#?}",
        hir.diagnostics,
        facts.method_call_rewrites
    );
    let lowered = lower_module(&hir.module, &facts);
    assert!(
        lowered
            .statuses
            .iter()
            .any(|status| status.name == "main"
                && matches!(status.status, SirLoweringStatus::Lowered)),
        "{:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "{:#?}\n{}",
        verify_module(&lowered.module),
        hew_sir::dump_sir(&lowered.module)
    );
    lowered.module
}

#[test]
fn explicit_next_uses_the_common_vector_and_aggregate_contracts() {
    let module = lower_source(
        r"
        fn main() -> i64 {
            let values = [1, 2, 3];
            var cursor = values.iter();
            match cursor.next() {
                .Some(value) => value,
                .None => 0,
            }
        }
    ",
    );
    let cursor = hew_types::ResolvedTy::named_builtin(
        "VecIter",
        hew_types::BuiltinType::VecIter,
        vec![hew_types::ResolvedTy::I64],
    );
    let shape = module.aggregate_shape_for_type(&cursor).unwrap();
    assert_eq!(shape.instance.nominal.full_path(), "std.builtins.VecIter");
    assert!(module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .any(|block| matches!(
            block.terminator,
            hew_sir::SemTerminator::RtCall {
                family: hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Get),
                ..
            }
        )));
}

#[test]
fn vector_for_in_uses_ordinary_cfg_and_cursor_updates() {
    lower_source(
        r"
        fn main() -> i64 {
            let values = [1, 2, 3];
            var sum = 0;
            for value in values { sum = sum + value; }
            sum
        }
    ",
    );
}

#[test]
fn iteration_exits_preserve_outer_values_and_clean_up_items() {
    lower_source(
        r#"
        fn main() -> i64 {
            var values = ["first", "skip", "last"];
            var result = "";
            for value in values {
                let local = value.to_upper();
                values.clear();
                if local == "SKIP" { continue; }
                result = local;
                if local == "LAST" { break; }
            }
            result.len()
        }
    "#,
    );
}

#[test]
fn nested_fields_update_through_the_same_aggregate_operations() {
    lower_source(
        r#"
        type Inner { value: string, other: Vec<string>, }
        type Outer { inner: Inner, sibling: string, }
        fn main() -> i64 {
            var value = Outer { inner: Inner { value: "old", other: ["keep"] }, sibling: "sibling" };
            value.inner.value = value.sibling;
            value.inner.value.len()
        }
    "#,
    );
}
