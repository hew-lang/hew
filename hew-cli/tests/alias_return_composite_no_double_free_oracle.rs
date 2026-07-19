//! ADVERSARIAL double-free pin — the load-bearing guard against a future
//! over-widening of the fresh-composite sole-owner admission.
//!
//! The fresh-owner see-through (fix (i)) and the composite mint admission
//! (fix (ii)) trust a producer as a droppable sole-owner ONLY when the module
//! sole-owner prover (`callee_returns_fresh_owner`) proves EVERY return path is a
//! fresh construction. A callee that hands back a BORROWED parameter alias — a
//! bare forwarder, a conditional forwarder, a field projection of a param, a
//! `var` reassigned from a param, or a `let x = h; x` re-binding of a param —
//! must stay NON-fresh: no move, no caller-side mint. If any such alias-return
//! were admitted as fresh, the caller would drop a value the argument binding
//! ALSO owns → a DOUBLE-FREE.
//!
//! Each shape passes an alias-returning call-result into a borrowing callee while
//! the source binding is still live. Under the poisoned-allocator triple
//! (`MallocScribble` + `MallocPreScribble` + `MallocGuardEdges`) a double-free
//! aborts; a mis-read fails the exact-content assertion. These shapes are
//! copy-in (they may leak — that is the fail-closed-safe outcome and is NOT
//! asserted here), but they must NEVER double-free. A future change that admits
//! an alias-return as fresh breaks this file.

#![cfg(unix)]

mod support;

use support::leak_slope::{compile_to_native, run_under_malloc_scribble};
use support::{describe_output, require_codegen};

const HOLDER: &str = "type Holder { s: string }\n\
                      fn borrowLen(h: Holder) -> i64 { h.s.len() }\n";

/// Bare param forwarder: `fn passthrough(h: Holder) -> Holder { h }`. `x` is a
/// live `let`, so `x` and the returned value are the same storage — a mint would
/// double-free. `x.s == "ab"` (2).
fn passthrough_source(frames: usize) -> String {
    format!(
        "{HOLDER}\
         fn passthrough(h: Holder) -> Holder {{ h }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let x: Holder = Holder {{ s: \"a\" + \"b\" }};\n\
         \x20       total = total + borrowLen(passthrough(x));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 61; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Conditional param passthrough: `if c { a } else { b }` — both arms forward a
/// param, so the return may alias either live binding.
fn conditional_source(frames: usize) -> String {
    format!(
        "{HOLDER}\
         fn cond(a: Holder, b: Holder, c: bool) -> Holder {{ if c {{ a }} else {{ b }} }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let a: Holder = Holder {{ s: \"a\" + \"b\" }};\n\
         \x20       let b: Holder = Holder {{ s: \"c\" + \"d\" }};\n\
         \x20       total = total + borrowLen(cond(a, b, true));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 62; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Field projection of a param: `fn getself(w: Wrap) -> Holder { w.h }` — the
/// return is an interior alias of the live `w`.
fn getfield_source(frames: usize) -> String {
    format!(
        "{HOLDER}\
         type Wrap {{ h: Holder }}\n\
         fn getself(w: Wrap) -> Holder {{ w.h }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let w: Wrap = Wrap {{ h: Holder {{ s: \"a\" + \"b\" }} }};\n\
         \x20       total = total + borrowLen(getself(w));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 63; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// The reassignment adversary: `fn remake(h) { var x = Holder{..}; x = h; x }` —
/// a `var` clobbered by a param. The see-through's `var`/reassignment guards must
/// keep it non-fresh even though its `var` init is a fresh construction.
fn reassign_source(frames: usize) -> String {
    format!(
        "{HOLDER}\
         fn remake(h: Holder) -> Holder {{ var x: Holder = Holder {{ s: \"z\" }}; x = h; x }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let y: Holder = Holder {{ s: \"a\" + \"b\" }};\n\
         \x20       total = total + borrowLen(remake(y));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 64; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// The `let`-of-param adversary: `fn viaLet(h) { let x = h; x }` — the
/// see-through recurses into `x`'s init (the param `h`) and re-derives the param
/// leaf, so `viaLet` stays non-fresh.
fn letparam_source(frames: usize) -> String {
    format!(
        "{HOLDER}\
         fn viaLet(h: Holder) -> Holder {{ let x: Holder = h; x }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let y: Holder = Holder {{ s: \"a\" + \"b\" }};\n\
         \x20       total = total + borrowLen(viaLet(y));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 65; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// The array-of-param adversary: `fn arrayOf(h) -> Vec<Holder> { [h] }` — the
/// array-desugar see-through must union the pushed param element, keeping the
/// producer non-fresh (a push that carried a borrowed param must not be trusted
/// fresh). `borrowVecLen` reads `v.len()`.
fn array_of_param_source(frames: usize) -> String {
    format!(
        "{HOLDER}\
         fn arrayOf(h: Holder) -> Vec<Holder> {{ [h] }}\n\
         fn borrowVecLen(v: Vec<Holder>) -> i64 {{ v.len() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let y: Holder = Holder {{ s: \"a\" + \"b\" }};\n\
         \x20       total = total + borrowVecLen(arrayOf(y));\n\
         \x20   }}\n\
         \x20   if total != {frames} {{ return 66; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// A borrowed-alias-return passed to a borrowing callee while its source is live
/// MUST NOT double-free under the poisoned allocator (and must read correctly).
/// A leak is the fail-closed-safe outcome and is not asserted; a double-free
/// (from wrongly minting a drop over an aliased value) aborts.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("alias-return-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name}: a borrowed-alias-returning composite callee must stay \
         non-fresh — no move, no mint, no double-free;\n{}",
        describe_output(&output)
    );
}

#[test]
fn passthrough_alias_return_does_not_double_free() {
    assert_no_double_free("passthrough", &passthrough_source(50));
}

#[test]
fn conditional_alias_return_does_not_double_free() {
    assert_no_double_free("conditional", &conditional_source(50));
}

#[test]
fn getfield_alias_return_does_not_double_free() {
    assert_no_double_free("getfield", &getfield_source(50));
}

#[test]
fn reassign_alias_return_does_not_double_free() {
    assert_no_double_free("reassign", &reassign_source(50));
}

#[test]
fn letparam_alias_return_does_not_double_free() {
    assert_no_double_free("letparam", &letparam_source(50));
}

#[test]
fn array_of_param_does_not_double_free() {
    assert_no_double_free("array_of_param", &array_of_param_source(50));
}
