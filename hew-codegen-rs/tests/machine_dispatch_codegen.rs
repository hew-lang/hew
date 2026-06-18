//! End-to-end codegen verification for synthesised `<Machine>__step`
//! functions: parses a real `.hew` source, drives the
//! parser → HIR → MIR → LLVM pipeline, and asserts the produced LLVM IR
//! carries the dispatch-tree shape and the tagged-union access pattern
//! introduced when the machine codegen layout landed.
//!
//! The acceptance criterion (review #22 R1) requires runtime-shape
//! evidence — not just call-presence. The `m.step(ev)` call site
//! and emit-queue runtime are wired in separate work; this
//! fixture verifies the dispatch via:
//!
//! 1. `emit_module` succeeds (LLVM `Module::verify()` returns ok). A
//!    misaligned struct GEP, a width-mismatched tag store, or an
//!    out-of-range variant projection would fail verify here. This is
//!    the load-bearing shape check — `verify()` exercises the same
//!    invariants LLVM applies at the JIT/AOT entry point that the
//!    `m.step` call site reaches.
//! 2. The textual IR for the synthesised `__step` function contains the
//!    expected dispatch shape: a tag-pointer GEP for the state tag, a
//!    tag-pointer GEP for the event tag, a `zext` widening the narrow
//!    tag to i64, and one `icmp eq` per declared state.
//! 3. For the payload-bearing fixture, the IR additionally contains a
//!    payload-pointer GEP and an inner-struct GEP for the
//!    `Place::MachineVariant` access path.
//!
//! Once `m.step(ev)` lowers, this fixture's `verify`-only invariants are
//! subsumed by a JIT-and-run check on the caller's exit code; until then
//! the IR-shape assertions guard the same invariants against codegen
//! regression.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::lower_hir_module;
use hew_types::TypeCheckOutput;

fn pipeline_for(path: &str) -> hew_mir::IrPipeline {
    let source = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("read {path}: {e}"));
    let parsed = hew_parser::parse(&source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors for {path}: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

fn emit_ll_text(pipeline: &hew_mir::IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-machine-codegen-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts = emit_module(pipeline, &options).expect("emit_module must succeed");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

#[test]
fn traffic_light_step_emits_dispatch_shape_with_state_tag_load() {
    // Zero-payload machine: drives Place::MachineTag and
    // Instr::EnumTagLoad, but no Place::MachineVariant. Verifies the
    // narrowest end of the dispatch-tree codegen surface.
    let pipeline = pipeline_for("../examples/machine/traffic_light.hew");
    let ll = emit_ll_text(&pipeline, "traffic_light");

    // The synthesised step fn must be present.
    assert!(
        ll.contains("define") && ll.contains("@TrafficLight__step("),
        "traffic_light .ll missing TrafficLight__step definition; got:\n{ll}"
    );

    // Outer-struct field-0 GEP for the state tag. inkwell labels the
    // GEP `machine_tag_ptr` (see place_pointer Place::MachineTag arm).
    assert!(
        ll.contains("machine_tag_ptr"),
        "traffic_light .ll missing machine_tag_ptr GEP; got:\n{ll}"
    );

    // Outer-struct field-0 GEP for the event tag — EnumTagLoad uses
    // the same struct shape on the `<Name>Event` companion.
    assert!(
        ll.contains("enum_tag_ptr"),
        "traffic_light .ll missing enum_tag_ptr GEP; got:\n{ll}"
    );

    // zext widening the narrow tag to i64 — the dispatch tree compares
    // tags as i64 via Instr::IntCmp.
    assert!(
        ll.contains("enum_tag_zext") || ll.contains("move_iN_zext"),
        "traffic_light .ll missing tag widening (zext); got:\n{ll}"
    );

    // The dispatch cascade must produce at least one icmp per declared
    // state (Red / Green / Yellow).
    let icmp_count = ll.matches("icmp eq i64").count();
    assert!(
        icmp_count >= 3,
        "traffic_light .ll has only {icmp_count} `icmp eq i64` instructions; \
         expected at least one per state (3+) for the dispatch cascade. \
         IR:\n{ll}"
    );
}

#[test]
fn counter_machine_step_emits_variant_payload_access() {
    // Payload-bearing machine: NonZero { value: Int } is referenced
    // both as a constructor target (Place::MachineVariant store) and
    // a source via `self.value` (Place::MachineVariant load) inside
    // the @reenter arm. Exercises the full Place primitive set.
    let pipeline = pipeline_for("../examples/machine/counter_machine.hew");
    let ll = emit_ll_text(&pipeline, "counter_machine");

    assert!(
        ll.contains("define") && ll.contains("@Counter__step("),
        "counter_machine .ll missing Counter__step definition; got:\n{ll}"
    );

    // Tag projection for self's state — same as zero-payload case.
    assert!(
        ll.contains("machine_tag_ptr"),
        "counter_machine .ll missing machine_tag_ptr GEP; got:\n{ll}"
    );

    // Payload-pointer GEP (Place::MachineVariant addresses outer
    // field 1 before reinterpreting as the variant's struct).
    assert!(
        ll.contains("machine_payload_ptr"),
        "counter_machine .ll missing machine_payload_ptr GEP — \
         Place::MachineVariant did not lower. Got:\n{ll}"
    );

    // Variant inner-struct field GEP. The constructor writes
    // `value: 1` and `value: self.value + 1`; the @reenter arm reads
    // `self.value`. Both produce the same GEP label from
    // place_pointer.
    assert!(
        ll.contains("machine_variant_field_ptr"),
        "counter_machine .ll missing machine_variant_field_ptr GEP — \
         Place::MachineVariant field projection did not lower. Got:\n{ll}"
    );

    // The arithmetic must reach LLVM: `self.value + 1` lowers through
    // IntAdd. Surfacing this proves the variant payload read produced
    // a usable i64 (the load+add chain depends on the GEP returning
    // a correctly typed slot).
    assert!(
        ll.contains("add nsw i64")
            || ll.contains("add i64")
            || ll.contains("llvm.sadd.with.overflow.i64")
            || ll.contains("__hew_add_i64"),
        "counter_machine .ll missing i64 add for self.value + 1 — variant \
         payload load did not produce a valid integer. Got:\n{ll}"
    );
}

#[test]
fn padded_payload_machine_uses_abi_correct_payload_size() {
    // Verifies that the tagged-union payload is sized AND aligned correctly.
    //
    // `Active { tag_byte: i8; value: i64; }` has an ABI size of 16 bytes and
    // an ABI alignment of 8 bytes (the alignment of `i64`). The outer machine
    // payload field is emitted as `[K x iA]` where `A` is the max variant
    // ABI alignment (8 here) and `K = ABI_size / A` (so `[2 x i64]` here).
    //
    // The wider integer element makes the payload field naturally aligned to
    // `A` bytes inside the outer struct. Variant access bitcasts the payload
    // pointer to the variant struct type, and the resulting GEP's pointer
    // alignment satisfies the variant's most-aligned field — eliminating the
    // LLVM IR UB that a 1-byte-aligned `[N x i8]` payload would imply.
    let pipeline = pipeline_for("../examples/machine/padded_payload_machine.hew");
    let ll = emit_ll_text(&pipeline, "padded_payload");

    // Payload type is the wider integer array, not `[N x i8]`. On every
    // target Hew lowers to, `{ i8, i64 }` has ABI size 16 and alignment 8,
    // so the payload is `[2 x i64]`.
    assert!(
        ll.contains("[2 x i64]"),
        "padded_payload_machine .ll payload array is not [2 x i64]; \
         alignment-aware payload typing is not active. The outer struct \
         payload must use the max variant ABI alignment as its element type \
         to satisfy variant-field load/store alignment. IR:\n{ll}"
    );

    // Neither the old byte-array shape `[16 x i8]` nor the field-width sum
    // `[9 x i8]` must appear — both would indicate a regression to the
    // 1-byte-aligned payload that is LLVM IR UB for i64 variant fields.
    assert!(
        !ll.contains("[16 x i8]"),
        "padded_payload_machine .ll contains [16 x i8] — the payload is \
         still 1-byte-aligned; variant i64 field GEPs would target \
         sub-naturally-aligned pointers (LLVM IR UB). IR:\n{ll}"
    );
    assert!(
        !ll.contains("[9 x i8]"),
        "padded_payload_machine .ll contains [9 x i8] — field-width-sum \
         sizing is still active; padding-aware ABI sizing required. IR:\n{ll}"
    );

    // Soundness check: no variant-field i64 load/store may claim `align 1`.
    //
    // LLVM `align N` on load/store is a *minimum* guarantee about the
    // operand pointer's alignment, not an equality assertion. UB occurs
    // when the actual pointer alignment is less than the claimed N. The
    // pre-fix `[16 x i8]` payload produced 1-byte-aligned variant-field
    // pointers, and the IR's `align 4` (i64 default) over-claimed alignment
    // on those pointers — that was the UB. The post-fix `[2 x i64]` payload
    // makes the variant-field pointer naturally 8-aligned, so any `align N`
    // (1, 4, or 8) on the resulting load/store is sound because the actual
    // alignment is ≥ N. We assert the UB-pattern guard: no `align 1` on
    // i64 ops inside the step function (which would only appear if the
    // 1-byte-aligned payload regressed).
    let has_align_1_i64 = ll
        .lines()
        .any(|line| line.contains(" i64 ") && line.contains("align 1"));
    assert!(
        !has_align_1_i64,
        "padded_payload_machine .ll contains an i64 load/store with `align 1` — \
         the 1-byte-aligned payload regressed. The `[K x iA]` payload-element-type \
         contract guarantees variant-field pointers meet their natural alignment; \
         an `align 1` on i64 would imply the payload is back to `[N x i8]`. IR:\n{ll}"
    );

    // Module::verify() is exercised by emit_module succeeding above.
    // Confirm step fn is present and the two-field variant access lowers.
    assert!(
        ll.contains("@PaddedPayload__step("),
        "padded_payload_machine .ll missing PaddedPayload__step definition; \
         got:\n{ll}"
    );
    assert!(
        ll.contains("machine_variant_field_ptr"),
        "padded_payload_machine .ll missing machine_variant_field_ptr GEP — \
         multi-field variant access did not lower. Got:\n{ll}"
    );
}
