//! Checker-level tests for `#[resource]` inherent-`close` consume semantics
//! (issue #1295).
//!
//! A `#[resource]` type's inherent `fn close(consume self)` is BOTH the implicit-drop
//! dispatch target (W3.030) AND a terminal consuming method: calling it moves
//! the receiver, so a subsequent use is a compile error and the scope-exit
//! implicit drop is suppressed on the consumed path. These tests verify the
//! checker wires the consume marker (the move-checker mark + the per-call-site
//! `method_call_consumes_receiver` flag HIR reads to lower the receiver with
//! `IntentKind::Consume`).

use crate::common;

use common::typecheck as typecheck_inline;

/// The per-call-site flag in `method_call_consumes_receiver` is recorded for a
/// `#[resource]` inherent `close()` call. HIR reads this side-table fact to
/// lower the receiver with `IntentKind::Consume`, which suppresses the
/// duplicate scope-exit implicit drop.
#[test]
fn resource_inherent_close_records_per_call_site_flag() {
    let output = typecheck_inline(
        r"#[resource]
pub type Conn {
    id: i64;
}

impl Conn {
    fn close(consume self) {}
}

fn main() {
    let c = Conn { id: 1 };
    c.close();
}
",
    );
    assert!(
        output
            .method_call_consumes_receiver
            .iter()
            .any(|key| key.module_idx == 0),
        "expected the `#[resource]` close() call site flagged in \
         method_call_consumes_receiver; got {:?}",
        output.method_call_consumes_receiver,
    );
}

/// A single `close()` call on a `#[resource]` type type-checks cleanly — the
/// move-checker marks the receiver moved but there is no second use.
#[test]
fn resource_inherent_single_close_typechecks_cleanly() {
    let output = typecheck_inline(
        r"#[resource]
pub type Conn {
    id: i64;
}

impl Conn {
    fn close(consume self) {}
}

fn main() {
    let c = Conn { id: 1 };
    c.close();
}
",
    );
    assert!(
        output.errors.is_empty(),
        "expected a clean typecheck for a single `#[resource]` close(); got: {:?}",
        output.errors,
    );
}

/// An ordinary borrowing method named close does not acquire ownership.
/// Metadata from embedded standard-library bodies must not count as a root call.
#[test]
fn ordinary_borrowing_close_does_not_acquire_receiver() {
    let output = typecheck_inline(
        r"pub type Plain {
    id: i64;
}

impl Plain {
    fn close(self) {}
}

fn main() {
    let p = Plain { id: 1 };
    p.close();
    p.close();
}
",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(
        !output
            .method_call_consumes_receiver
            .iter()
            .any(|key| key.module_idx == 0),
        "a non-`#[resource]` inherent close() must not record a resource \
         consume-receiver flag; got {:?}",
        output.method_call_consumes_receiver,
    );
}
