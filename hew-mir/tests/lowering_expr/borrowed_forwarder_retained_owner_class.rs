//! A borrowed forwarder's result may be minted a caller-side owner ONLY when the
//! result type actually carries an independent refcount share.
//!
//! HIR promotes a `Borrowed` direct-call result to `Owned { Retained }` when
//! every actual argument is proven owned — the borrowed-forwarder shim in
//! `hew_hir::verify::resolve_user_call_facts`. `Retained` asserts the value
//! ALIASES its source AND holds one independent share, so exactly one extra drop
//! is legal. That is a `string`-only property: codegen `hew_string_clone`s a
//! string field load, so the projection owns a fresh `+1`.
//!
//! The other two field-load classes hold no second share:
//!
//! - `ByteCopyAlias` (an inline aggregate — record / tuple / array / inline
//!   enum): the load byte-copies the member, so the value is an interior alias
//!   whose original the source's composite drop already frees.
//! - `HandleTransfer` (a single-pointer leaf — `Vec` / `bytes` / `HashMap` /
//!   `HashSet`): the load transfers the one owned handle, with nothing left to
//!   share.
//!
//! Minting a caller owner over either is a second claim on live storage. The
//! observable is `MirStatement::Bind { name: "__hew_temp_arg" }`: MIR mints that
//! synthetic owner for an argument temporary whose produced-value fact says
//! `Owned`, and elaboration then plans a drop over it. For
//! `fn getself(w: Wrap) -> Holder { w.h }` passed to a borrowing callee while
//! `w` is still live, that drop and `w`'s own composite drop free the same
//! `Holder.s` buffer — a DOUBLE FREE, which aborted on the `free_cstring`
//! sentinel.
//!
//! One class per test, because the classes are what the mechanism dispatches on:
//! a `string` fixture alone would prove the promotion works and say nothing
//! about the two classes that must refuse it. The `string` case is the
//! load-bearing positive control — without it this file would pass just as
//! happily against a shim deleted outright.
//!
//! These assertions are platform-independent and run on every host. The macOS
//! poisoned-allocator oracle
//! (`hew-cli/tests/alias_return_composite_no_double_free_oracle.rs`) observes the
//! same defect as an abort; this file pins the emission shape that causes it.

use hew_mir::{IrPipeline, MirStatement};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

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
    hew_mir::lower_hir_module(&output.module)
}

/// Synthetic argument-temporary owners minted in `fn_name`. One of these over a
/// borrowed alias is the caller-side drop that double-frees.
fn temp_arg_owners(pipeline: &IrPipeline, fn_name: &str) -> usize {
    pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .filter(|stmt| matches!(stmt, MirStatement::Bind { name, .. } if name == "__hew_temp_arg"))
        .count()
}

/// `ByteCopyAlias`: a record field projection returned from a borrowing callee.
/// `w` is live across the call and its composite drop already frees `w.h.s`.
#[test]
fn record_field_forwarder_result_mints_no_caller_owner() {
    let pipeline = pipeline(
        "type Holder { s: string }\n\
         type Wrap { h: Holder }\n\
         fn borrowLen(h: Holder) -> i64 { h.s.len() }\n\
         fn getself(w: Wrap) -> Holder { w.h }\n\
         fn main() -> i64 {\n\
         \x20   let w: Wrap = Wrap { h: Holder { s: \"a\" + \"b\" } };\n\
         \x20   borrowLen(getself(w))\n\
         }\n",
    );
    assert_eq!(
        temp_arg_owners(&pipeline, "main"),
        0,
        "a record field projection returned from a borrowing callee is an \
         interior alias of the still-live argument; minting a caller owner over \
         it plans a second drop of the same field"
    );
}

/// `ByteCopyAlias`, tuple spelling: the same class reached through a tuple
/// projection rather than a named record field.
///
/// Green on BOTH sides of the fix — the tuple projection never reached the
/// promotion. It is a class control, not a regression pin: it holds the tuple
/// spelling to the record spelling's answer so a later change cannot move one
/// without the other.
#[test]
fn tuple_field_forwarder_result_mints_no_caller_owner() {
    let pipeline = pipeline(
        "type Holder { s: string }\n\
         fn borrowLen(h: Holder) -> i64 { h.s.len() }\n\
         fn firstOf(p: (Holder, i64)) -> Holder { p.0 }\n\
         fn main() -> i64 {\n\
         \x20   let p: (Holder, i64) = (Holder { s: \"a\" + \"b\" }, 1);\n\
         \x20   borrowLen(firstOf(p))\n\
         }\n",
    );
    assert_eq!(
        temp_arg_owners(&pipeline, "main"),
        0,
        "a tuple element projection is the same byte-copy interior alias as a \
         record field and must not acquire a caller-side drop"
    );
}

/// `HandleTransfer`: a single-pointer heap leaf field. The load moves the one
/// owned handle, so there is no second share for a caller drop to release.
#[test]
fn vec_field_forwarder_result_mints_no_caller_owner() {
    let pipeline = pipeline(
        "type Bag { items: Vec<i64> }\n\
         fn borrowCount(v: Vec<i64>) -> i64 { v.len() }\n\
         fn itemsOf(b: Bag) -> Vec<i64> { b.items }\n\
         fn main() -> i64 {\n\
         \x20   let b: Bag = Bag { items: [1, 2] };\n\
         \x20   borrowCount(itemsOf(b))\n\
         }\n",
    );
    assert_eq!(
        temp_arg_owners(&pipeline, "main"),
        0,
        "a single-pointer leaf field load transfers the one owned handle; a \
         caller-side drop over it releases storage the argument still owns"
    );
}

/// `Retained`, the positive control: a `string` result DOES carry an
/// independent refcount share, so the borrowed-forwarder promotion must keep
/// working. A fix that simply deleted the shim fails here.
#[test]
fn string_forwarder_result_keeps_its_retained_owner() {
    let pipeline = pipeline(
        "fn borrowLen(s: string) -> i64 { s.len() }\n\
         fn same(s: string) -> string { s }\n\
         fn main() -> i64 {\n\
         \x20   let s: string = \"a\" + \"b\";\n\
         \x20   borrowLen(same(s))\n\
         }\n",
    );
    assert_eq!(
        temp_arg_owners(&pipeline, "main"),
        1,
        "a string result holds its own refcount share, so the borrowed \
         forwarder promotion still mints exactly one caller owner"
    );
}
