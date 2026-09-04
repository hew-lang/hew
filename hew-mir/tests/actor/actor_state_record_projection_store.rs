// #3266 — a projection into a record-typed actor state field must publish the
// mutated value back into state. Lowering materialises the state field into a
// frame local (`ActorStateFieldLoad`) before it can project into it, so without
// a trailing `ActorStateFieldStore` the `RecordFieldStore` lands on a copy and
// the handler's write is discarded when the frame goes away.
use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{FieldOffset, Instr};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_source(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:?}", tc_output.errors);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    hew_mir::lower_hir_module(&hir.module)
}

fn instructions_of<'a>(pipeline: &'a hew_mir::IrPipeline, name: &str) -> Vec<&'a Instr> {
    pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == name)
        .unwrap_or_else(|| panic!("no MIR function named `{name}`"))
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .collect()
}

/// The write-back exists, targets the same state field the load read, and
/// republishes the very local the field store mutated.
#[test]
fn record_projection_into_actor_state_field_stores_back_to_the_same_field() {
    let pipeline = lower_source(
        r"
type Point {
    x: i64,
    y: i64,
}

actor Holder {
    var origin: Point;
    receive fn bump() {
        origin.y = 1;
    }
}

fn main() {
    let h = spawn Holder(origin: Point { x: 1, y: 4 });
    h.bump();
}
",
    );

    let instrs = instructions_of(&pipeline, "Holder__recv__bump");
    let found = instrs.windows(3).any(|window| {
        matches!(
            window,
            [
                Instr::ActorStateFieldLoad {
                    field_offset: load_field,
                    dest,
                    ..
                },
                Instr::RecordFieldStore {
                    record,
                    field_offset: leaf,
                    ..
                },
                Instr::ActorStateFieldStore {
                    field_offset: store_field,
                    src,
                    ..
                },
            ] if record == dest
                && src == dest
                && load_field == store_field
                // `origin` is the actor's only state field, `y` the record's
                // second — pinning both proves the offsets are not crossed.
                && *load_field == FieldOffset(0)
                && *leaf == FieldOffset(1)
        )
    });
    assert!(
        found,
        "expected load/field-store/store-back on state field 0 leaf 1, got: {instrs:#?}"
    );
}

/// Negative control for the arm's guard: the same projection shape on a local
/// record is already in-place, so lowering it must not reach into actor state.
/// Without the `actor_state_field_for_target` guard on the write-back this
/// handler would publish an unrelated local into state field 0.
#[test]
fn record_projection_into_a_local_emits_no_actor_state_store() {
    let pipeline = lower_source(
        r"
type Point {
    x: i64,
    y: i64,
}

actor Holder {
    var origin: Point;
    receive fn touch_local() {
        var scratch = Point { x: 0, y: 0 };
        scratch.y = 1;
    }
}

fn main() {
    let h = spawn Holder(origin: Point { x: 1, y: 4 });
    h.touch_local();
}
",
    );

    let instrs = instructions_of(&pipeline, "Holder__recv__touch_local");
    assert!(
        instrs
            .iter()
            .any(|instr| matches!(instr, Instr::RecordFieldStore { .. })),
        "the local projection should still lower to a record field store: {instrs:#?}"
    );
    assert!(
        !instrs
            .iter()
            .any(|instr| matches!(instr, Instr::ActorStateFieldStore { .. })),
        "a local record write must not publish into actor state: {instrs:#?}"
    );
}
