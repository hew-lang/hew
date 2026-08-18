//! "Proven foreign" must be a property of the VALUE, not of how its declaring
//! file happened to be handed to the compiler.
//!
//! # What went wrong
//!
//! `ProvenForeignPolicy` decided an `extern "C"` result was a foreign handle
//! from `extern_is_foreign_host`, whose input is the [`ExternProvenance`] the
//! HIR captured at lowering: `Module("std.process")` when the file is reached
//! through `import std::process`, and `Root` when the same file is handed
//! directly to `hew check`. The stdlib type-check ratchet does the latter, so
//! the compiler read its OWN runtime ABI as a foreign host and refused
//! shipped stdlib code:
//!
//! ```text
//! std/process.hew:235:69: error: E_NOT_YET_IMPLEMENTED: MIR lowering for
//! ownership transfer of a proven-foreign value into a callee-owned parameter
//! 235 |   panic(f"process.run_args failed: {process_error_message(err)}");
//! ```
//!
//! The declaring module's identity is not stable under how a file is compiled,
//! so it cannot be the whole answer to the provenance question.
//!
//! # The answer that IS stable
//!
//! Two facts about the result, neither of which moves when the compilation
//! root moves:
//!
//! * **Clause A — the audited row.** `scripts/jit-symbol-classification.toml`
//!   carries `symbol = "hew_process_last_error"`, `result = "fresh"`,
//!   `release-symbol = "hew_string_drop"`. That row says a newly owned
//!   allocation transfers to the caller AND names the release that balances
//!   it. The refused diagnostic's own wording — "a declared, NON-AUDITED
//!   `extern`" — was false for this symbol. The table already read that file
//!   for the ARGUMENT axis (`borrowing_arg_names`); the result axis was never
//!   read.
//! * **Clause B — the shape.** `hew_cron_next_hew` returns
//!   `CronNextResult { status: i32; timestamp: i64 }`. A value that provably
//!   contains no pointer cannot be a handle to anything, audited or not.
//!
//! # Polarity
//!
//! Both clauses are read ONLY by `ProvenForeignPolicy`, the SUPPRESSION side.
//! `OpaqueExternTaintPolicy`, the mint side — where a wrong answer is a double
//! free rather than a leak — is untouched, so nothing here licenses a new
//! mint. Every admission below is paired with a refusal that must survive it.

use hew_mir::IrPipeline;
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    hew_mir::lower_hir_module(&output.module)
}

fn transfers_are_refused(p: &IrPipeline) -> bool {
    p.diagnostics
        .iter()
        .any(|d| format!("{:?}", d.kind).contains("ownership transfer of a proven-foreign value"))
}

/// The stdlib shape, reduced: an extern produces a value, a Hew frame wraps it
/// in an error carrier, and a callee-owned parameter consumes the payload
/// binder. That is `process.run_args`'s
/// `panic(f"... {process_error_message(err)}")` with the names shortened.
///
/// Every fixture is a ROOT compilation unit, which is precisely the condition
/// that used to make the extern read as a foreign host.
fn stdlib_shape(extern_decls: &str, producer: &str) -> IrPipeline {
    pipeline_with_tc(&format!(
        "extern \"C\" {{\n{extern_decls}}}\n\
         record Holder {{ label: string }}\n\
         type Pod {{ status: i32; timestamp: i64 }}\n\
         #[opaque]\n\
         type Handle {{\n}}\n\
         type Nested {{ inner: Pod; extra: i64 }}\n\
         type WithHandle {{ slot: Handle; code: i64 }}\n\
         enum Carrier {{ Failed(Holder); Ok }}\n\
         {producer}\n\
         fn message(c: Carrier) -> i64 {{ match c {{ Carrier.Failed(h) => h.label.len(), Carrier.Ok => 0 }} }}\n\
         fn main() -> i64 {{\n    \
             var i: i64 = 0;\n    \
             while i < 2 {{\n        \
                 let c = mk(i);\n        \
                 let n = message(c);\n        \
                 println(f\"x={{n}}\");\n        \
                 i = i + 1;\n    \
             }}\n    \
             0\n\
         }}\n"
    ))
}

/// `cron.try_next`'s shape, faithfully: the extern's POD result is PASSED INTO
/// a Hew function that builds the error carrier, so the taint travels by value
/// flow through a parameter and out through that function's return, exactly as
/// `Err(cron_error_from_result(result))` does. The carrier then meets a
/// callee-owned parameter, which is the site that refused.
fn pod_carrier_shape(
    extern_decls: &str,
    pod_ty: &str,
    call: &str,
    scalar_read: &str,
) -> IrPipeline {
    pipeline_with_tc(&format!(
        "extern \"C\" {{\n{extern_decls}}}\n\
         record Holder {{ label: string }}\n\
         type Pod {{ status: i32; timestamp: i64 }}\n\
         #[opaque]\n\
         type Handle {{\n}}\n\
         type Nested {{ inner: Pod; extra: i64 }}\n\
         type WithHandle {{ slot: Handle; code: i64 }}\n\
         enum Carrier {{ Failed(Holder); Ok }}\n\
         fn to_carrier(p: {pod_ty}) -> Carrier {{ \
             Carrier.Failed(Holder {{ label: f\"e{{{scalar_read}}}\" }}) }}\n\
         fn mk(i: i64) -> Carrier {{ to_carrier(unsafe {{ {call} }}) }}\n\
         fn message(c: Carrier) -> i64 {{ \
             match c {{ Carrier.Failed(h) => h.label.len(), Carrier.Ok => 0 }} }}\n\
         fn main() -> i64 {{\n    \
             var i: i64 = 0;\n    \
             while i < 2 {{\n        \
                 let c = mk(i);\n        \
                 let n = message(c);\n        \
                 println(f\"x={{n}}\");\n        \
                 i = i + 1;\n    \
             }}\n    \
             0\n\
         }}\n"
    ))
}

// ---------------------------------------------------------------------------
// The refusal this branch exists to keep: an UNAUDITED extern
// ---------------------------------------------------------------------------
/// The control for everything below. `host_record` appears in no audited row
/// and returns a pointer-bearing record, so neither clause reaches it and the
/// round-5/6 refusal stands verbatim. If this ever passes, the fix has become
/// a blanket suppression.
#[test]
fn an_unaudited_extern_result_is_still_proven_foreign_and_still_refused() {
    let p = stdlib_shape(
        "    fn host_record() -> Holder;\n",
        "fn mk(i: i64) -> Carrier { Carrier.Failed(unsafe { host_record() }) }",
    );
    assert!(
        transfers_are_refused(&p),
        "an unaudited extern handle must still be refused: {:#?}",
        p.diagnostics
    );
}

// ---------------------------------------------------------------------------
// Clause A — the audited result row
// ---------------------------------------------------------------------------

/// THE FIX, at the shape that failed the ratchet. Identical to the control in
/// every respect except the SYMBOL NAME, which is the one thing that carries
/// the audit.
#[test]
fn an_audited_fresh_result_extern_is_not_proven_foreign() {
    let p = stdlib_shape(
        "    fn hew_process_last_error() -> Holder;\n",
        "fn mk(i: i64) -> Carrier { Carrier.Failed(unsafe { hew_process_last_error() }) }",
    );
    assert!(
        p.diagnostics.is_empty(),
        "an audited fresh-result extern hands the caller a newly owned \
         allocation and names its release; refusing to transfer it rejects \
         the standard library: {:#?}",
        p.diagnostics
    );
}

/// The audit is keyed on `(symbol, arity)`. A declaration that disagrees with
/// the audited parameter list is not the audited function, so it gets no
/// admission — a local `extern` block cannot borrow another symbol's contract
/// by reusing its name.
#[test]
fn an_audited_name_declared_at_the_wrong_arity_is_still_refused() {
    let p = stdlib_shape(
        "    fn hew_process_last_error(unused: i64) -> Holder;\n",
        "fn mk(i: i64) -> Carrier { Carrier.Failed(unsafe { hew_process_last_error(i) }) }",
    );
    assert!(
        transfers_are_refused(&p),
        "arity disagreement means this is not the audited symbol: {:#?}",
        p.diagnostics
    );
}

/// `result = "borrowed"` is NOT an admission. `hew_vec_get_owned` hands back a
/// pointer into storage the callee still owns; the caller genuinely must not
/// release it, so the suppression must not fire and the transfer stays
/// refused. Clause A admits `Fresh` and `Retained` only.
#[test]
fn an_audited_borrowed_result_extern_is_still_proven_foreign() {
    let p = stdlib_shape(
        "    fn hew_vec_get_owned(v: i64, idx: i64) -> Holder;\n",
        "fn mk(i: i64) -> Carrier { Carrier.Failed(unsafe { hew_vec_get_owned(i, i) }) }",
    );
    assert!(
        transfers_are_refused(&p),
        "a borrowed result is exactly the value the caller must not take \
         ownership of: {:#?}",
        p.diagnostics
    );
}

// ---------------------------------------------------------------------------
// Clause B — the pointer-free return
// ---------------------------------------------------------------------------

/// `hew_cron_next_hew`'s shape, with an UNAUDITED name so the admission can
/// only be coming from clause B. A record of `i32` and `i64` has nowhere to
/// put a pointer, so it cannot be a foreign handle.
#[test]
fn a_pointer_free_pod_return_is_not_proven_foreign_even_unaudited() {
    let p = pod_carrier_shape(
        "    fn host_pod() -> Pod;\n",
        "Pod",
        "host_pod()",
        "p.timestamp",
    );
    assert!(
        p.diagnostics.is_empty(),
        "a return type with no pointer in it cannot be a handle: {:#?}",
        p.diagnostics
    );
}

/// Clause B is transitive through nested records, by least fixpoint over the
/// module's own declarations.
#[test]
fn a_nested_pointer_free_record_return_is_not_proven_foreign() {
    let p = pod_carrier_shape(
        "    fn host_nested() -> Nested;\n",
        "Nested",
        "host_nested()",
        "p.inner.timestamp",
    );
    assert!(
        p.diagnostics.is_empty(),
        "pointer-freedom composes through fields: {:#?}",
        p.diagnostics
    );
}

/// The boundary of clause B, at the SAME shape as the two admissions above so
/// the only variable is the extern's declared return type. An `#[opaque]`
/// field is a pointer-width handle slot — the exact shape this authority
/// exists to refuse — so a record carrying one is not pointer-free no matter
/// how many scalars sit beside it, the value stays proven foreign, and the
/// transfer stays refused.
#[test]
fn a_record_carrying_an_opaque_handle_is_still_proven_foreign() {
    let p = pod_carrier_shape(
        "    fn host_with_handle() -> WithHandle;\n",
        "WithHandle",
        "host_with_handle()",
        "p.code",
    );
    assert!(
        transfers_are_refused(&p),
        "an opaque handle slot must not be admitted as pointer-free: {:#?}",
        p.diagnostics
    );
}
