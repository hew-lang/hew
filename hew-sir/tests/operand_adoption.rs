//! A runtime operation that adopts its operand takes ownership of it: the
//! caller hands over an independent owner instead of paying for a second
//! structural copy the operation would make and this scope would destroy.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    canonicalize_module_constant_cfg, lower_module, transfer_module_dead_local_reads,
    verify_module, BoundaryDecision, SemModule, SemOpKind, SemTerminator, ValueId,
};
use hew_types::{module_registry::ModuleRegistry, Checker, RuntimeCallFamily, VecValueOp};

const ITEM: &str = "type Item { parts: Vec<i64> }";

fn compiled(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR errors: {:#?}",
        hir.diagnostics
    );
    let mut module = lower_module(&hir.module, &facts).module;
    assert!(
        verify_module(&module).is_empty(),
        "SIR errors: {:#?}",
        verify_module(&module)
    );
    canonicalize_module_constant_cfg(&mut module).expect("canonical SIR");
    transfer_module_dead_local_reads(&mut module).expect("transferred SIR");
    module
}

/// The element operand of the body's single `Vec.push`, and its decision.
fn pushed_element(module: &SemModule) -> (BoundaryDecision, ValueId) {
    let mut found = None;
    for block in module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
    {
        if let SemTerminator::RtCall {
            family: RuntimeCallFamily::Vector(VecValueOp::Push),
            args,
            ..
        } = &block.terminator
        {
            assert!(found.is_none(), "expected exactly one push");
            found = Some((args[1].decision, args[1].operand.value));
        }
    }
    found.expect("body pushes an element")
}

/// The operation kind that defines `value`, following one transfer.
fn producer(module: &SemModule, value: ValueId) -> SemOpKind {
    let defining = |value: ValueId| {
        module
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .flat_map(|block| &block.ops)
            .find(|op| op.results.iter().any(|result| result.id == value))
            .map(|op| op.kind.clone())
            .expect("operand has a defining operation")
    };
    match defining(value) {
        SemOpKind::Move { source } => defining(source.value),
        kind => kind,
    }
}

#[test]
fn a_fresh_temporary_transfers_into_the_push() {
    let module = compiled(&format!(
        "{ITEM}
         fn main() {{
           var bag: Vec<Item> = Vec.new();
           bag.push(Item {{ parts: Vec.new() }});
           println(f\"{{bag.len()}}\");
         }}"
    ));
    let (decision, element) = pushed_element(&module);
    assert_eq!(decision, BoundaryDecision::Move);
    assert!(
        matches!(producer(&module, element), SemOpKind::AggregateMake { .. }),
        "a fresh temporary has no other owner and transfers directly"
    );
}

#[test]
fn a_local_read_after_the_push_transfers_a_copy_and_keeps_its_own() {
    let module = compiled(&format!(
        "{ITEM}
         fn main() {{
           var kept = Item {{ parts: Vec.new() }};
           var bag: Vec<Item> = Vec.new();
           bag.push(kept);
           println(f\"{{bag.len()}} {{kept.parts.len()}}\");
         }}"
    ));
    let (decision, element) = pushed_element(&module);
    assert_eq!(decision, BoundaryDecision::Move);
    assert!(
        matches!(producer(&module, element), SemOpKind::LoadCopy { .. }),
        "a local still read afterwards keeps its contents and transfers a copy"
    );
}

#[test]
fn a_local_dead_after_the_push_transfers_its_contents() {
    let module = compiled(&format!(
        "{ITEM}
         fn main() {{
           var kept = Item {{ parts: Vec.new() }};
           var bag: Vec<Item> = Vec.new();
           bag.push(kept);
           println(f\"{{bag.len()}}\");
         }}"
    ));
    let (decision, element) = pushed_element(&module);
    assert_eq!(decision, BoundaryDecision::Move);
    assert!(
        matches!(producer(&module, element), SemOpKind::LoadTake { .. }),
        "nothing reads the local again, so its contents transfer instead of being copied"
    );
}

#[test]
fn a_bit_copied_element_is_read_in_place() {
    let module = compiled(
        "fn main() {
           var values: Vec<i64> = Vec.new();
           var x = 7;
           values.push(x);
           println(f\"{values.len()} {x}\");
         }",
    );
    let (decision, _) = pushed_element(&module);
    assert_eq!(
        decision,
        BoundaryDecision::Borrow,
        "a bit-copied operand owns nothing to transfer"
    );
}
