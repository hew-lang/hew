//! Functional-update field-override release must poison the OWNING slot.
//!
//! `h = R { f: new, ..h }` releases the overridden field `h.f` before the
//! assignment stores the new aggregate. That assignment's own overwrite-drop
//! targets the SAME field, so the release is reached twice by construction. MIR
//! has two ways to emit it and only one survives being reached twice:
//!
//! - `Instr::RecordFieldDrop` GEPs the live record field, releases through it,
//!   and null-stores THAT slot. The second reach loads `null` and calls a
//!   null-tolerant release symbol — a no-op. Idempotent.
//! - `Instr::RecordFieldLoad` + `Instr::Drop` copies the field into a temp local
//!   first, so the post-drop null-store poisons the COPY. The record still holds
//!   the freed pointer, and the second reach frees it again. A DOUBLE FREE, not
//!   a leak.
//!
//! `bytes` used to take the copying path because its slot is a fat
//! `{ ptr, offset, len }` triple rather than a single pointer. Fatness is not the
//! property that matters: only field 0 carries ownership, `hew_bytes_drop` takes
//! exactly that pointer, and it is null-tolerant, so the triple can be poisoned
//! in place like any single-pointer field. The double free it produced was
//! invisible to CI because the only oracle over the shape ran under macOS
//! `leaks(1)` and silently reported success when it could not measure.
//!
//! Both the override release and the assignment's own overwrite-drop appear in
//! the stream, so more than one in-place release of the same field is expected
//! and correct — that is precisely the reach the poison chain neutralises. What
//! must never appear is a COPYING release of that field.
//!
//! These assertions are platform-independent and run on every host, which is the
//! point: the release SHAPE is checkable without an allocator inspector.
//!
//! The negative controls are load-bearing (`drop-allowset-from-value-flow`): a
//! test that only asserted "some release is emitted" would pass just as happily
//! against the copying pair that caused the double free.

use hew_mir::{lower_hir_module, DropFnSpec, Instr, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Full pipeline with type-checking so record field types resolve and the
/// `builtin` discriminant reaches the MIR field type that the override-release
/// routing dispatches on.
fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pl = lower_hir_module(&output.module);
    assert!(
        pl.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pl.diagnostics
    );
    pl
}

/// Every release symbol emitted by an in-place `RecordFieldDrop` in `fn_name`.
fn in_place_release_symbols(pl: &IrPipeline, fn_name: &str) -> Vec<String> {
    instr_symbols(pl, fn_name, |instr| match instr {
        Instr::RecordFieldDrop {
            drop_fn: DropFnSpec::Release(symbol),
            ..
        } => Some(symbol.to_string()),
        _ => None,
    })
}

/// Every release symbol emitted by a copying `Instr::Drop` in `fn_name`. A
/// functional-update field override must contribute NONE of these.
fn copying_release_symbols(pl: &IrPipeline, fn_name: &str) -> Vec<String> {
    instr_symbols(pl, fn_name, |instr| match instr {
        Instr::Drop {
            drop_fn: Some(DropFnSpec::Release(symbol)),
            ..
        } => Some(symbol.to_string()),
        _ => None,
    })
}

fn instr_symbols(
    pl: &IrPipeline,
    fn_name: &str,
    pick: impl Fn(&Instr) -> Option<String>,
) -> Vec<String> {
    let func = pl
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("no MIR function named `{fn_name}`"));
    func.blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter_map(&pick)
        .collect()
}

fn override_source(field_ty: &str, initial: &str, replacement: &str) -> String {
    format!(
        "type Holder {{\n\
         \x20   buf: {field_ty},\n\
         \x20   count: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var h = Holder {{ buf: {initial}, count: 0 }};\n\
         \x20   h = Holder {{ buf: {replacement}, ..h }};\n\
         \x20   h.count\n\
         }}\n"
    )
}

/// ADMIT: a `bytes` field override releases in place via `RecordFieldDrop`.
///
/// This is the regression pin. Before the fix `bytes` was routed to the copying
/// `RecordFieldLoad` + `Drop` pair, whose null-store landed in the temp; the
/// assignment's overwrite-drop then freed the still-live pointer a second time.
#[test]
fn bytes_field_override_releases_in_place_not_through_a_copy() {
    let pl = pipeline(&override_source(
        "bytes",
        "\"initial\".to_bytes()",
        "\"replacement\".to_bytes()",
    ));
    let in_place = in_place_release_symbols(&pl, "main");
    assert!(
        !in_place.is_empty() && in_place.iter().all(|s| s == "hew_bytes_drop"),
        "every release of an overridden `bytes` field must go through Instr::RecordFieldDrop so \
         the post-drop null-store poisons the record's own slot and each later reach reduces to \
         `hew_bytes_drop(null)`; got {in_place:?}"
    );
    assert!(
        !copying_release_symbols(&pl, "main").contains(&"hew_bytes_drop".to_string()),
        "the overridden `bytes` field must NOT also be released through a copying Instr::Drop — \
         that is the double-free shape this pin exists to exclude"
    );
}

/// CONTROL: `string`, which always took the in-place path, still does. Pins that
/// widening the predicate to `bytes` did not perturb the established single-
/// pointer routing.
#[test]
fn string_field_override_still_releases_in_place() {
    let pl = pipeline(&override_source("string", "\"initial\"", "\"replacement\""));
    let in_place = in_place_release_symbols(&pl, "main");
    assert!(
        !in_place.is_empty() && in_place.iter().all(|s| s == "hew_string_drop"),
        "every release of an overridden `string` field must keep going through \
         Instr::RecordFieldDrop; got {in_place:?}"
    );
    assert!(
        !copying_release_symbols(&pl, "main").contains(&"hew_string_drop".to_string()),
        "no release of the overridden `string` field may go through a copying Instr::Drop"
    );
}

/// NEGATIVE CONTROL: a scalar field carries no ownership, so overriding it emits
/// no release at all. Without this, the admit assertions above would pass
/// against a predicate that routed every field to `RecordFieldDrop`.
#[test]
fn scalar_field_override_emits_no_release() {
    let pl = pipeline(
        "type Holder {\n\
         \x20   buf: i64,\n\
         \x20   count: i64,\n\
         }\n\
         \n\
         fn main() -> i64 {\n\
         \x20   var h = Holder { buf: 1, count: 0 };\n\
         \x20   h = Holder { buf: 2, ..h };\n\
         \x20   h.count\n\
         }\n",
    );
    assert!(
        in_place_release_symbols(&pl, "main").is_empty(),
        "an overridden scalar field owns no heap allocation and must emit no release"
    );
}
