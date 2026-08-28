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
         type field and must not acquire a caller-side drop"
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

/// A fresh composite handed to a borrowing callee, with the producer's return
/// spelled seven different ways.
///
/// The value is the same in every row — a sole-use local the frame hands over —
/// so the caller must mint exactly one temporary owner in every row. What
/// differs is what sits between the local and the return: nothing, a block, two
/// blocks, an explicit `return`, an `if` with one local per arm, a `match` with
/// one local per arm, and no local at all.
///
/// A rule that tests the syntactic return site instead of the value it resolves
/// to answers row 1 and leaks rows 2 through 6, because a block interposes a
/// value-identity relation and a conditional interposes a join. Holding all seven
/// to one number is what stops the next narrowing of this rule from silently
/// dropping a spelling: a single-shape test would have passed throughout that
/// regression.
#[test]
fn every_return_spelling_of_a_fresh_local_mints_one_caller_owner() {
    const PRELUDE: &str = "type Inner { a: string, b: string }\n\
                           type Outer { inner: Inner }\n\
                           fn borrowSum(o: Outer) -> i64 { o.inner.a.len() + o.inner.b.len() }\n\
                           fn mk(i: i64) -> Outer { Outer { inner: Inner { a: \"x\", b: \"y\" } } }\n";
    const CALLER: &str = "fn main() -> i64 { borrowSum(mkOuter(1)) }\n";

    for (spelling, producer) in [
        (
            "bare tail",
            "fn mkOuter(i: i64) -> Outer { let o = mk(i); o }",
        ),
        (
            "block tail",
            "fn mkOuter(i: i64) -> Outer { let o = mk(i); { o } }",
        ),
        (
            "nested block tail",
            "fn mkOuter(i: i64) -> Outer { let o = mk(i); { { o } } }",
        ),
        (
            "explicit return",
            "fn mkOuter(i: i64) -> Outer { let o = mk(i); return o; }",
        ),
        (
            "if arms",
            "fn mkOuter(i: i64) -> Outer { let o = mk(i); let p = mk(i); \
             if i % 2 == 0 { o } else { p } }",
        ),
        (
            "match arms",
            "fn mkOuter(i: i64) -> Outer { let o = mk(i); let p = mk(i); \
             match i % 2 { 0 => o, _ => p } }",
        ),
        ("no local at all", "fn mkOuter(i: i64) -> Outer { mk(i) }"),
    ] {
        let pipeline = pipeline(&format!("{PRELUDE}{producer}\n{CALLER}"));
        assert_eq!(
            temp_arg_owners(&pipeline, "main"),
            1,
            "{spelling}: the producer hands over a fresh composite, so the \
             borrowing caller owns and drops it exactly once — the spelling of \
             the return does not change whose value it is"
        );
    }
}

/// The refusals must survive the same spellings.
///
/// Following value-identity relations to reach the local in the shapes above
/// must not also see THROUGH a parameter alias. A block around `w.h` or around a
/// bare parameter is still an alias of storage the caller keeps, and a join with
/// one owned arm and one parameter arm is still a borrow — one arm executing is
/// enough to double-free.
#[test]
fn no_return_spelling_launders_a_parameter_alias_into_an_owner() {
    const PRELUDE: &str = "type Holder { s: string }\n\
                           type Wrap { h: Holder }\n\
                           fn borrowLen(h: Holder) -> i64 { h.s.len() }\n";

    for (shape, program) in [
        (
            "field projection behind a block",
            "fn getself(w: Wrap) -> Holder { { w.h } }\n\
             fn main() -> i64 { let w: Wrap = Wrap { h: Holder { s: \"a\" + \"b\" } }; \
             borrowLen(getself(w)) }\n",
        ),
        (
            "parameter forwarder behind a block",
            "fn passthrough(h: Holder) -> Holder { { h } }\n\
             fn main() -> i64 { let x: Holder = Holder { s: \"a\" + \"b\" }; \
             borrowLen(passthrough(x)) }\n",
        ),
        (
            "join of an owned local and a parameter",
            "fn mixed(p: Holder, c: bool) -> Holder { let o: Holder = Holder { s: \"z\" + \"z\" }; \
             if c { o } else { p } }\n\
             fn main() -> i64 { let y: Holder = Holder { s: \"a\" + \"b\" }; \
             borrowLen(mixed(y, true)) }\n",
        ),
    ] {
        let pipeline = pipeline(&format!("{PRELUDE}{program}"));
        assert_eq!(
            temp_arg_owners(&pipeline, "main"),
            0,
            "{shape}: the returned value still aliases storage the argument \
             binding owns, so no caller-side drop may be minted"
        );
    }
}

/// A relation chain deeper than the fact-graph populations the old loop bound
/// counted, compiled twice in one process.
///
/// The produced-value solver advances a relation chain by one edge per round
/// because propagation reads from a snapshot, so a chain deeper than the bound
/// left the fixpoint INCOMPLETE — and an incomplete fixpoint answers whatever
/// the iteration order reached. Twenty nested blocks around the returned local
/// build that chain in a module with only a handful of calls, aggregates and
/// binding references.
///
/// Two runs in one process is the discriminator that matters here: the default
/// hasher is seeded per process, so a single run cannot distinguish a stable
/// answer from a lucky one. The assertion is that the answer is the CORRECT one
/// (the caller owns the composite) and that MIR is identical between runs, not
/// merely that two runs agree.
/// Deeply nested source recurses through the parser and checker, and a libtest
/// thread's default 2 MiB stack overflows on it where the CLI's main thread does
/// not (`hew check` handles 40 levels). Run the body on a thread sized for the
/// recursion so the test measures the fixpoint and not the harness.
#[test]
fn a_deep_relation_chain_reaches_the_same_fixpoint_on_every_run() {
    std::thread::Builder::new()
        .stack_size(32 * 1024 * 1024)
        .spawn(deep_relation_chain_body)
        .expect("spawn deep-recursion test thread")
        .join()
        .expect("deep-relation-chain body");
}

fn deep_relation_chain_body() {
    let nesting = 20;
    let source = format!(
        "type Inner {{ a: string, b: string }}\n\
         type Outer {{ inner: Inner }}\n\
         fn borrowSum(o: Outer) -> i64 {{ o.inner.a.len() + o.inner.b.len() }}\n\
         fn mk() -> Outer {{ Outer {{ inner: Inner {{ a: \"x\", b: \"y\" }} }} }}\n\
         fn mkOuter() -> Outer {{ let o = mk(); {}o{} }}\n\
         fn main() -> i64 {{ borrowSum(mkOuter()) }}\n",
        "{ ".repeat(nesting),
        " }".repeat(nesting),
    );

    let first = pipeline(&source);
    let second = pipeline(&source);

    assert_eq!(
        temp_arg_owners(&first, "main"),
        1,
        "a {nesting}-deep value-identity chain still resolves to the sole-use \
         local the frame hands over; a bound that runs out mid-chain answers \
         `borrowed` and leaks the composite"
    );
    assert_eq!(
        format!("{:?}", first.raw_mir),
        format!("{:?}", second.raw_mir),
        "two compilations of one source in the same process must produce \
         identical MIR; a differing answer means the fixpoint terminated on \
         iteration order rather than on convergence"
    );
}
