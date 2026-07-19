//! Leak oracle for the caller-side drop of a value-receiver method's receiver
//! (Shape A of #2753).
//!
//! A record or collection bound to a `let` and passed as `self` to a
//! NON-consuming value-receiver impl method (`p.touch()`) is BORROWED by the
//! call, not consumed: under the copy-on-write borrow model the CALLER retains
//! ownership and drops the receiver exactly ONCE at its own scope exit. Before
//! the fix the borrow-site collector skipped every method-call argument, so the
//! receiver's owned heap field leaked once per call (32 B/iter for a string
//! field, ~144 B for a `Vec` field). This is the method-dispatch twin of
//! `owned_param_reuse_leak_oracle` (the free-fn positional-borrow precedent):
//! method dispatch and free-fn dispatch now converge on one drop authority.
//!
//! Two independent signals per shape:
//! - the per-iteration leak SLOPE stays flat (`leaks --atExit` node counts under
//!   the poisoned-allocator triple) — the receiver field is released every
//!   iteration; and
//! - the poisoned allocator (`MallocScribble`+`MallocPreScribble`+
//!   `MallocGuardEdges`) does not abort — the receiver is released exactly once,
//!   never double-freed.
//!
//! The consuming-receiver and `#[resource]`-receiver controls pin the
//! exactly-once boundary: a method that CONSUMES `self` (returns it, forwards it
//! whole to a consuming callee) and a `#[resource]` receiver must NOT earn the
//! caller drop, or the receiver frees twice. The controls assert no double-free
//! (a clean exit under the scribbled allocator) — an over-eager caller drop of a
//! consumed/resource receiver reads freed storage or aborts.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// Record with a string field, borrowed as `self` by a non-consuming method.
/// `touch(self) -> i64` reads nothing owned, so `p` is proven-borrow and the
/// caller drops it once per iteration. `touch` returns 1, so `total == frames`.
fn record_string_receiver_source(frames: usize) -> String {
    format!(
        "type Person {{ name: string }}\n\
         impl Person {{ fn touch(self) -> i64 {{ 1 }} }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let p = Person {{ name: \"abc\" + \"def\" }};\n\
         \x20       total = total + p.touch();\n\
         \x20   }}\n\
         \x20   if total != {frames} {{ return 71; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Record with a `Vec<i64>` field, borrowed as `self` by a non-consuming
/// method. The receiver owns the heap-allocated vector backing store; a missing
/// caller drop leaks it per iteration. `touch` returns 1, so `total == frames`.
fn record_vec_receiver_source(frames: usize) -> String {
    format!(
        "type Bag {{ items: Vec<i64> }}\n\
         impl Bag {{ fn touch(self) -> i64 {{ 1 }} }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       var v: Vec<i64> = Vec::new();\n\
         \x20       v.push(1); v.push(2); v.push(3);\n\
         \x20       let b = Bag {{ items: v }};\n\
         \x20       total = total + b.touch();\n\
         \x20   }}\n\
         \x20   if total != {frames} {{ return 72; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Consuming-receiver control: `tag(self) -> Person` returns `self` WHOLE, so
/// the parameter-body summary classifies the receiver CONSUME — it is NOT
/// recorded, and the callee's return hands ownership to `q`, which drops it
/// once. Any caller drop of `p` here would double-free `q`'s record. `q.name`
/// is "abcdef" (len 6), so `total == frames * 6`.
fn consuming_receiver_returns_self_source(frames: usize) -> String {
    format!(
        "type Person {{ name: string }}\n\
         impl Person {{ fn tag(self) -> Person {{ self }} }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let p = Person {{ name: \"abc\" + \"def\" }};\n\
         \x20       let q = p.tag();\n\
         \x20       total = total + q.name.len();\n\
         \x20   }}\n\
         \x20   if total != {frames} * 6 {{ return 73; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Consuming-receiver control via forwarding: `via(self)` passes `self` WHOLE to
/// the consuming free fn `keep` (which returns its param), so the fixpoint flips
/// `via`'s receiver to CONSUME — not recorded. The callee owns and drops the
/// receiver; a caller drop would double-free. `q.name` is "abcdef" (len 6).
fn consuming_receiver_forward_source(frames: usize) -> String {
    format!(
        "type Person {{ name: string }}\n\
         fn keep(p: Person) -> Person {{ p }}\n\
         impl Person {{ fn via(self) -> Person {{ keep(self) }} }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let p = Person {{ name: \"abc\" + \"def\" }};\n\
         \x20       let q = p.via();\n\
         \x20       total = total + q.name.len();\n\
         \x20   }}\n\
         \x20   if total != {frames} * 6 {{ return 74; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// `#[resource]`-receiver control: a `#[resource]` `self` reads BORROW in the
/// body scan (`peek` reads `self.name`) yet consumes at the call site via the
/// terminal-consume registration (`raii-null-after-move`). The receiver is
/// EXCLUDED from the caller drop; if it were recorded, the caller would free the
/// resource's string field that the resource lifecycle also releases — a
/// double-free. The control asserts no double-free (clean exit); the resource
/// field's own release is a separate concern out of scope here. `peek` returns
/// `self.name.len()` == 6.
fn resource_receiver_source(frames: usize) -> String {
    format!(
        "#[resource]\n\
         type Res {{ name: string }}\n\
         impl Res {{\n\
         \x20   fn peek(self) -> i64 {{ self.name.len() }}\n\
         \x20   fn close(self) {{ }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let r = Res {{ name: \"abc\" + \"def\" }};\n\
         \x20       total = total + r.peek();\n\
         \x20       r.close();\n\
         \x20   }}\n\
         \x20   if total != {frames} * 6 {{ return 75; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Poisoned-allocator no-double-free / no-use-after-free pin: an over-eager
/// caller drop of the receiver reads/aborts under the scribbled allocator, and a
/// use-after-free returns a non-zero sentinel from the `total` check. A clean
/// success exit means exactly-once release.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("self-receiver-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name}: the value-receiver of a non-consuming method must be \
         released exactly once at caller scope exit (and a consuming/resource \
         receiver exactly once by the callee/lifecycle);\n{}",
        describe_output(&output)
    );
}

#[test]
fn record_string_receiver_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "self_receiver_record_string",
        record_string_receiver_source,
    );
}

#[test]
fn record_vec_receiver_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("self_receiver_record_vec", record_vec_receiver_source);
}

#[test]
fn consuming_receiver_returns_self_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "self_receiver_consume_return",
        consuming_receiver_returns_self_source,
    );
}

#[test]
fn record_string_receiver_does_not_double_free() {
    assert_no_double_free(
        "self_receiver_record_string_df",
        &record_string_receiver_source(50),
    );
}

#[test]
fn record_vec_receiver_does_not_double_free() {
    assert_no_double_free(
        "self_receiver_record_vec_df",
        &record_vec_receiver_source(50),
    );
}

#[test]
fn consuming_receiver_returns_self_does_not_double_free() {
    assert_no_double_free(
        "self_receiver_consume_return_df",
        &consuming_receiver_returns_self_source(50),
    );
}

#[test]
fn consuming_receiver_forward_does_not_double_free() {
    assert_no_double_free(
        "self_receiver_consume_forward_df",
        &consuming_receiver_forward_source(50),
    );
}

#[test]
fn resource_receiver_does_not_double_free() {
    assert_no_double_free("self_receiver_resource_df", &resource_receiver_source(50));
}
