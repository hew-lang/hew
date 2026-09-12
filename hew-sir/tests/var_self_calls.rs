//! Mutable methods transfer their exact receiver and write it back on return.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BindingTarget, CallUnwind, SemModule, SemOpKind, SemTerminator,
    SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(Vec::new())).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = lower_module(&hir.module, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    let errors = verify_module(&lowered.module);
    assert!(
        errors.is_empty(),
        "{errors:?}\n{}",
        hew_sir::dump_sir(&lowered.module)
    );
    lowered.module
}

fn cleanup_region(
    function: &hew_sir::SemFunction,
    start: hew_sir::BlockId,
) -> Vec<&hew_sir::SemBlock> {
    let mut pending = vec![start];
    let mut seen = Vec::new();
    let mut blocks = Vec::new();
    while let Some(target) = pending.pop() {
        if seen.contains(&target) {
            continue;
        }
        seen.push(target);
        let block = &function.blocks[target.0 as usize];
        block
            .terminator
            .visit_successors(|edge| pending.push(edge.target));
        blocks.push(block);
    }
    blocks
}

#[test]
fn scalar_record_method_returns_its_result_and_updated_receiver() {
    lower(
        r"
        type Counter { n: i64 }
        trait Bump { fn bump(var self, amount: i64) -> i64; }
        impl Bump for Counter {
            fn bump(var self, amount: i64) -> i64 { self.n += amount; self.n }
        }
        fn main() -> i64 {
            var counter = Counter { n: 10 };
            let first = counter.bump(2);
            first + counter.bump(3) + counter.n
        }
    ",
    );
}

#[test]
fn owning_record_method_returns_owned_results_and_preserves_siblings() {
    lower(
        r#"
        type Text { value: string }
        type Outer { text: Text, sibling: string }
        trait Replace { fn replace(var self, value: string) -> string; }
        impl Replace for Text {
            fn replace(var self, value: string) -> string {
                let old = self.value;
                self.value = value;
                old
            }
        }
        fn main() -> i64 {
            var outer = Outer { text: Text { value: "first".to_upper() }, sibling: "keep".to_upper() };
            var text = outer.text;
            let old = text.replace("second".to_upper());
            old.len() + text.value.len() + outer.sibling.len()
        }
    "#,
    );
}

#[test]
fn generic_unit_method_preserves_scalar_and_owned_receivers() {
    lower(
        r#"
        trait Touch { fn touch(var self, early: bool); }
        type Holder<T> { payload: T }
        impl<T> Touch for Holder<T> {
            fn touch(var self, early: bool) { if early { } }
        }
        fn main() -> i64 {
            var owned = Holder { payload: "kept".to_upper() };
            owned.touch(true);
            owned.touch(false);
            var scalar = Holder { payload: 7 };
            scalar.touch(false);
            owned.payload.len() + scalar.payload
        }
    "#,
    );
}

#[test]
fn argument_alias_and_nested_mutation_precede_the_receiver_take() {
    let module = lower(
        r#"
        type Text { value: string }
        trait Replace { fn replace(var self, value: string) -> i64; }
        impl Replace for Text {
            fn replace(var self, value: string) -> i64 { self.value = value; self.value.len() }
        }
        fn main() -> i64 {
            var text = Text { value: "initial".to_upper() };
            text.replace(text.value);
            text.replace({ text.replace("nested".to_upper()); "final".to_upper() });
            text.value.len()
        }
    "#,
    );
    let main = module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "main")
        .unwrap();
    let BindingTarget::Place(receiver) = main
        .bindings
        .iter()
        .find(|binding| binding.name == "text")
        .unwrap()
        .target
    else {
        panic!("owning receiver requires canonical local storage")
    };
    let mut calls = 0;
    for block in &main.blocks {
        let SemTerminator::Call {
            normal,
            unwind: CallUnwind::Cleanup(unwind),
            ..
        } = &block.terminator
        else {
            continue;
        };
        calls += 1;
        assert!(block
            .ops
            .iter()
            .any(|op| matches!(op.kind, SemOpKind::LoadTake { place } if place == receiver)));
        let normal = &main.blocks[normal.as_ref().expect("returning call").target.0 as usize];
        assert!(normal.ops.iter().any(
            |op| matches!(op.kind, SemOpKind::StoreAssign { place, .. } if place == receiver)
        ));
        let cleanup = cleanup_region(main, unwind.target);
        assert!(cleanup
            .iter()
            .any(|block| matches!(block.terminator, SemTerminator::ResumeUnwind)));
        assert!(!cleanup
            .iter()
            .flat_map(|block| &block.ops)
            .any(|op| matches!(op.kind, SemOpKind::StoreAssign { .. })));
    }
    assert_eq!(
        calls, 3,
        "the alias, nested and outer mutations must all be reached"
    );
}

#[test]
fn argument_and_callee_faults_preserve_the_ordinary_cleanup_contract() {
    lower(
        r#"
        type Text { value: string }
        trait Replace { fn replace(var self, consume value: string, divisor: i64) -> i64; }
        impl Replace for Text {
            fn replace(var self, consume value: string, divisor: i64) -> i64 {
                self.value = value;
                8 / divisor
            }
        }
        fn argument_failure(divisor: i64) -> i64 {
            var text = Text { value: "original".to_upper() };
            text.replace("captured".to_upper(), 12 / divisor)
        }
        fn main() -> i64 {
            let result = argument_failure(2);
            var text = Text { value: "original".to_upper() };
            result + text.replace("replacement".to_upper(), 0)
        }
    "#,
    );
}

#[test]
fn generic_method_transfers_receiver_on_explicit_and_fallthrough_returns() {
    lower(
        r#"
        trait Advance { fn advance(var self, early: bool) -> i64; }
        type Holder<T> { payload: T }
        impl<T> Advance for Holder<T> {
            fn advance(var self, early: bool) -> i64 { if early { return 1; } 2 }
        }
        fn main() -> i64 {
            var holder = Holder { payload: "owned".to_upper() };
            holder.advance(true) + holder.advance(false) + holder.payload.len()
        }
    "#,
    );
}

#[test]
fn unit_method_result_cannot_acquire_a_payload_or_ownership() {
    let module = lower(
        r"
        type Marker { n: i64 }
        trait Touch { fn touch(var self); }
        impl Touch for Marker { fn touch(var self) {} }
        fn main() { var marker = Marker { n: 1 }; marker.touch(); }
    ",
    );
    for ownership_error in [false, true] {
        let mut invalid = module.clone();
        let operation = invalid
            .functions
            .iter_mut()
            .flat_map(|f| &mut f.blocks)
            .flat_map(|block| &mut block.ops)
            .find(|op| matches!(op.kind, SemOpKind::ConstUnit))
            .unwrap();
        let id = operation.id;
        if ownership_error {
            operation.results[0].own = hew_sir::OwnKind::Owned;
        } else {
            operation.results[0].ty = hew_types::ResolvedTy::I64;
        }
        assert!(verify_module(&invalid).iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidOperation { op, reason }
                if *op == id && reason.contains("const unit")
        )));
    }
}

#[test]
fn scalar_field_and_tuple_receivers_preserve_later_argument_updates() {
    lower(
        r"
        type Counter { n: i64 }
        type Outer { counter: Counter, sibling: i64 }
        trait Bump { fn bump(var self, amount: i64) -> i64; }
        impl Bump for Counter {
            fn bump(var self, amount: i64) -> i64 { self.n += amount; self.n }
        }
        fn main() -> i64 {
            var pair = (Outer { counter: Counter { n: 10 }, sibling: 20 }, 40);
            let result = pair.0.counter.bump({
                pair.0.sibling = 30;
                pair.0.counter.bump(2);
                3
            });
            result + pair.0.counter.n + pair.0.sibling + pair.1
        }
    ",
    );
}

#[test]
fn owning_field_and_tuple_receivers_preserve_aliases_and_replaced_roots() {
    lower(
        r#"
        type Text { value: string }
        type Outer { text: Text, sibling: string }
        trait Replace { fn replace(var self, value: string) -> i64; }
        impl Replace for Text {
            fn replace(var self, value: string) -> i64 { self.value = value; self.value.len() }
        }
        fn main() -> i64 {
            var pair = (Outer { text: Text { value: "first".to_upper() }, sibling: "keep".to_upper() }, "tail".to_upper());
            let copied = pair;
            pair.0.text.replace(pair.0.text.value);
            pair.0.text.replace({
                pair.0.sibling = "changed".to_upper();
                pair.0.text.replace("nested".to_upper());
                "final".to_upper()
            });
            pair.0.text.replace({
                pair = (Outer { text: Text { value: "fresh".to_upper() }, sibling: "new".to_upper() }, "new tail".to_upper());
                copied.0.text.value
            });
            pair.0.text.value.len() + pair.0.sibling.len() + pair.1.len() + copied.0.text.value.len()
        }
    "#,
    );
}

#[test]
fn projected_receiver_faults_clean_the_retained_sibling_and_arguments() {
    lower(
        r#"
        type Text { value: string }
        type Outer { text: Text, sibling: string }
        trait Replace { fn replace(var self, consume value: string, divisor: i64) -> i64; }
        impl Replace for Text {
            fn replace(var self, consume value: string, divisor: i64) -> i64 {
                self.value = value;
                8 / divisor
            }
        }
        fn attempt(divisor: i64) -> i64 {
            var state = Outer { text: Text { value: "original".to_upper() }, sibling: "keep".to_upper() };
            state.text.replace("captured".to_upper(), 12 / divisor)
        }
        fn main() -> i64 {
            let first = attempt(2);
            var state = Outer { text: Text { value: "original".to_upper() }, sibling: "keep".to_upper() };
            first + state.text.replace("replacement".to_upper(), 0)
        }
    "#,
    );
}

#[test]
fn unit_method_bare_return_keeps_the_exact_result_and_receiver_pair() {
    lower(
        r#"
        trait Touch { fn touch(var self, early: bool); }
        type Holder<T> { payload: T }
        impl<T> Touch for Holder<T> {
            fn touch(var self, early: bool) { if early { return; } }
        }
        fn main() -> i64 {
            var holder = Holder { payload: "kept".to_upper() };
            holder.touch(true);
            holder.touch(false);
            holder.payload.len()
        }
    "#,
    );
}

#[test]
fn primitive_receiver_transfer_keeps_bitcopy_semantics() {
    lower(
        r"
        trait Bump { fn bump(var self, amount: i64) -> i64; }
        impl Bump for i64 {
            fn bump(var self, amount: i64) -> i64 {
                self += amount;
                if amount == 2 { return self; }
                self
            }
        }
        fn main() -> i64 { var value: i64 = 10; value.bump(2) + value.bump(3) + value }
    ",
    );
}

#[test]
fn returning_self_as_the_method_result_preserves_an_independent_receiver() {
    lower(
        r#"
        type Text { value: string }
        trait Snapshot { fn snapshot(var self, early: bool) -> Text; }
        impl Snapshot for Text {
            fn snapshot(var self, early: bool) -> Text {
                if early { return self; }
                self
            }
        }
        fn main() -> i64 {
            var text = Text { value: "original".to_upper() };
            let early = text.snapshot(true);
            let late = text.snapshot(false);
            text.value.len() + early.value.len() + late.value.len()
        }
    "#,
    );
}
