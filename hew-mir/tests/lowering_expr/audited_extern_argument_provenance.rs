//! What a callee does with the handle it is GIVEN must not depend on how its
//! declaring file was handed to the compiler.
//!
//! # What went wrong
//!
//! `audited_extern_result_provenance` (the sibling file) removed exactly this
//! dependency from the RESULT axis. It was still live on the ARGUMENT axis —
//! the axis the same table had read FIRST:
//!
//! ```text
//! ownership.rs         callee_is_arg_ownership_opaque_extern answers true for
//!                      every extern absent from `borrowing_arg_names`
//! return_provenance.rs build_extern_contract_table filled that set only under
//!                      `if ef.provenance.is_stdlib()`
//! hew-hir/node.rs      ExternProvenance::Root => false
//! ```
//!
//! So a `let`-bound fresh string passed to ANY declared extern lost its
//! release whenever the declaring file was the root compilation unit:
//!
//! ```text
//! extern "C" { fn hew_tcp_connect_timeout(host: string, port: i32,
//!                                         timeout_ms: i32) -> i32; }
//! fn pick(flag: i64) -> i64 {
//!     let host = "a" + "b";
//!     unsafe { hew_tcp_connect_timeout(host, 80 as i32, 10 as i32) as i64 }
//! }
//! ```
//!
//! `hew_tcp_connect_timeout` is audited `params = ["borrow","borrow","borrow"]`
//! and the runtime reads the pointer through `CStr::from_ptr` without freeing
//! it. The callee borrows and the caller must release — and under `Root`
//! provenance nobody did. In shipped code the same seam cost
//! `std/net/net.hew::connect_timeout` all thirteen of its `host` releases,
//! including the one on the ordinary `return`.
//!
//! # What an UNAUDITED argument means
//!
//! Unchanged, and deliberately: no audited row means the caller withholds its
//! release. The failure direction is a LEAK. The available alternative — read a
//! non-`consume` parameter as a declared borrow, which Hew's surface can
//! express — fails as a DOUBLE RELEASE, and nothing in the front end yet
//! requires an author to have considered ownership at a heap-typed extern
//! parameter, so an omitted `consume` is not evidence of a decision. The
//! refusals below pin that the unaudited case still withholds caller cleanup.

use hew_mir::{Instr, IrPipeline, OwnershipEvent, Terminator};
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

/// Elaborated releases planned inside ONE function. Per-function, because the
/// surrounding `main` legitimately releases its own values and a whole-program
/// count cannot tell the two apart.
fn releases_in(p: &IrPipeline, function: &str) -> usize {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == function)
        .unwrap_or_else(|| panic!("`{function}` must lower"))
        .drop_plans
        .iter()
        .map(|(_, plan)| plan.drops.len())
        .sum()
}

/// Pre-call owner handoffs into one named callee. These are distinct from the
/// runtime ABI's normal-success commits: an opaque extern may adopt the handle
/// before unwinding, so its caller owner must end in the call block itself.
fn pre_call_handoffs_into(p: &IrPipeline, function: &str, callee: &str) -> usize {
    let function = p
        .checked_mir
        .iter()
        .find(|f| f.name == function)
        .unwrap_or_else(|| panic!("`{function}` must lower"));
    function
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            Terminator::Call {
                callee: target,
                args,
                ..
            } if target == callee => Some((&block.instructions, args)),
            _ => None,
        })
        .flat_map(|(instructions, args)| {
            instructions.iter().filter(move |instruction| {
                matches!(
                    instruction,
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        from,
                        to: None,
                        to_owner: None,
                        ..
                    }) if args.contains(from)
                )
            })
        })
        .count()
}

/// The reproducer, parameterised by the extern declaration and the call.
fn caller_of(decl: &str, call: &str) -> IrPipeline {
    pipeline_with_tc(&format!(
        "extern \"C\" {{\n{decl}}}\n\
         fn pick(flag: i64) -> i64 {{\n    \
         let host = \"a\" + \"b\";\n    \
         unsafe {{ {call} }}\n\
         }}\n\
         fn main() -> i64 {{ pick(1) }}\n"
    ))
}

/// The defect, as a number. A root-unit declaration of an audited all-`Borrow`
/// extern must leave the caller holding the release for the temporary it built.
#[test]
fn a_root_declared_audited_borrow_extern_leaves_the_caller_its_release() {
    let p = caller_of(
        "    fn hew_tcp_connect_timeout(host: string, port: i32, timeout_ms: i32) -> i32;\n",
        "hew_tcp_connect_timeout(host, 80 as i32, 10 as i32) as i64",
    );
    assert!(
        releases_in(&p, "pick") > 0,
        "`host` is a fresh `+1` string the callee is audited to BORROW, so the \
         caller keeps the sole release. Zero here is the leak that cost \
         `std/net/net.hew::connect_timeout` thirteen releases."
    );
    assert_eq!(
        pre_call_handoffs_into(&p, "pick", "hew_tcp_connect_timeout"),
        0,
        "an audited all-borrow extern must not take the caller owner"
    );
}

/// The same question asked of a callee with no audited row at all. This is the
/// polarity guard: the fix admits names the machine-checked table claims, and
/// nothing else. A host the runtime does not classify stays opaque, and the
/// caller withholds — a leak, never a second release of a pointer the host may
/// already have freed.
#[test]
fn an_unaudited_host_extern_still_withholds_the_callers_release() {
    let p = caller_of(
        "    fn a_host_symbol_the_runtime_does_not_classify(host: string) -> i32;\n",
        "a_host_symbol_the_runtime_does_not_classify(host) as i64",
    );
    assert_eq!(
        releases_in(&p, "pick"),
        0,
        "an extern with no audited argument contract may have taken the handle; \
         keeping a caller-side release on top of that is a double free"
    );
    assert_eq!(
        pre_call_handoffs_into(&p, "pick", "a_host_symbol_the_runtime_does_not_classify"),
        1,
        "the opaque boundary must end the exact caller owner before invocation"
    );
}

/// A `Consume` position anywhere in the audited signature refuses the whole
/// name, so the universal release can never be read as borrowing its argument.
#[test]
fn an_audited_consuming_extern_still_withholds_the_callers_release() {
    let p = caller_of(
        "    fn hew_string_drop(s: string) -> i32;\n",
        "hew_string_drop(host) as i64",
    );
    assert_eq!(
        releases_in(&p, "pick"),
        0,
        "`hew_string_drop` is audited `params = [consume]`; a caller-side \
         release on top of it frees the buffer twice"
    );
    assert_eq!(
        pre_call_handoffs_into(&p, "pick", "hew_string_drop"),
        1,
        "the audited consuming extern must receive the exact caller owner"
    );
}

/// The audited row describes a signature, not just a name. A declaration that
/// disagrees with it is a different function and says nothing about the
/// audited one.
#[test]
fn an_audited_name_declared_at_the_wrong_arity_withholds_the_callers_release() {
    let p = caller_of(
        "    fn hew_tcp_connect_timeout(host: string) -> i32;\n",
        "hew_tcp_connect_timeout(host) as i64",
    );
    assert_eq!(
        releases_in(&p, "pick"),
        0,
        "arity disagreement means this is not a declaration of the audited \
         callee, so the row must not be claimed by name"
    );
    assert_eq!(
        pre_call_handoffs_into(&p, "pick", "hew_tcp_connect_timeout"),
        1,
        "a mismatched declaration must not acquire the audited borrow capability"
    );
}
