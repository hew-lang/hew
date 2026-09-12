//! Ownership oracle for reading a heap leaf out of a still-live tuple owner.
//!
//! The authoritative shape is `let p = (v, 1); let items = p.0;
//! items.len()`. A tuple-field load on the physical path is a deep clone, not
//! a destructive move: `items` gets its own independent `Vec` handle, and `p`
//! keeps its own live copy of field 0 alongside every sibling. Both owners
//! release their own allocation at scope exit; there is no cleared slot to
//! order and no second owner of the same allocation to refuse.
//!
//! Before this contract was written into MIR, both candidates were excluded
//! fail-closed: the tuple prover saw an active extracted owner, while the Vec
//! prover saw an interior projection. A 64-frame probe leaked exactly 128
//! nodes (the Vec header and backing allocation per frame); the direct Vec
//! control leaked zero. That fail-closed history motivates the flat-leak-slope
//! oracles below; the clone contract is what makes rereading `p.0` after
//! `items = p.0`, or returning `p` whole after taking `p.0`, ordinary code
//! rather than a use-after-move.

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn projected_tuple_source(frames: usize) -> String {
    format!(
        "\
fn build(n: i64) -> i64 {{
    var v: Vec<i64> = Vec.new();
    v.push(n);
    v.push(n + 1);
    let p = (v, 1);
    let items = p.0;
    items.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{frames} {{
        total = total + build(i);
    }}
    if total == {expected} {{ 0 }} else {{ 113 }}
}}
",
        expected = frames * 2
    )
}

fn aliased_tuple_source(frames: usize) -> String {
    format!(
        "\
fn build(n: i64) -> i64 {{
    var v: Vec<i64> = Vec.new();
    v.push(n);
    v.push(n + 1);
    let p = (v, 1);
    let q = p;
    let items = q.0;
    items.len() + q.1
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{frames} {{
        total = total + build(i);
    }}
    if total == {expected} {{ 0 }} else {{ 114 }}
}}
",
        expected = frames * 3
    )
}

fn cancellation_source(frames: usize) -> String {
    format!(
        "\
fn build(n: i64) -> i64 {{
    var v: Vec<i64> = Vec.new();
    v.push(n);
    let p = (v, 7);
    let items = p.0;
    var i: i64 = 0;
    while i < n {{
        i = i + 1;
    }}
    items.len() + p.1
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{frames} {{
        total = total + build(i);
    }}
    if total == {expected} {{ 0 }} else {{ 115 }}
}}
",
        expected = frames * 8
    )
}

fn escaping_partial_tuple_source() -> &'static str {
    "\
fn build(n: i64) -> (Vec<i64>, i64) {
    var v: Vec<i64> = Vec.new();
    v.push(n);
    let p = (v, 1);
    let items = p.0;
    let _length = items.len();
    p
}

fn main() -> i64 {
    let returned = build(3);
    returned.0.len()
}
"
}

fn loop_reread_after_transfer_source() -> &'static str {
    "\
fn main() -> i64 {
    var v: Vec<i64> = Vec.new();
    v.push(1);
    let p = (v, 7);
    var i: i64 = 0;
    var total: i64 = 0;
    while i < 3 {
        let items = p.0;
        total = total + items.len();
        i = i + 1;
    }
    total
}
"
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn projected_tuple_owner_active_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("projected_tuple_owner_active", projected_tuple_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn whole_tuple_alias_then_projection_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("projected_tuple_owner_alias", aliased_tuple_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "poisoned allocator validation is a Darwin-only ownership gate"
)]
#[test]
fn projected_owner_paths_are_exactly_once_under_malloc_scribble() {
    require_codegen();
    for (name, source) in [
        ("projected_tuple_owner_active", projected_tuple_source(8)),
        ("projected_tuple_owner_alias", aliased_tuple_source(8)),
        ("projected_tuple_owner_cancel", cancellation_source(8)),
    ] {
        let dir = tempfile::Builder::new()
            .prefix("projected-tuple-owner-scribble-")
            .tempdir()
            .expect("tempdir");
        let bin = compile_to_native(&source, dir.path(), name);
        let output = run_under_malloc_scribble(&bin);
        assert!(
            output.status.success(),
            "{name} must preserve the transferred Vec through every read and \
             release it exactly once:\n{}",
            describe_output(&output)
        );
    }
}

// `checked_and_elaborated_mir_write_the_disjoint_release_contract` and
// `cancellation_exit_keeps_both_disjoint_drops` pinned this same disjoint-
// release contract (root-relative neutralize before the new owner is
// exposed; both the projected Vec and the neutralized tuple released
// exactly once, including on the loop-backedge/cancellation exit) against
// the retired checked/elaborated MIR text dumps (`--dump-mir checked` /
// `elab`), which no longer exist (`--dump-mir` now accepts only
// `physical`). Physical MIR's debug dump carries no source-level ordering
// text to re-pin the same way (bindings are positional `StorageId`s with no
// retained names), so re-deriving an equally precise structural match would
// mean reconstructing this contract from raw op offsets.
//
// Coverage lost on Linux CI: this was the only always-on pin for the
// disjoint-release ordering and the cancellation-exit both-drops fact.

/// Was a refusal: `p.0` clones, so `p` is never partially moved and returning
/// it whole after reading `items = p.0` is ordinary code. `items` and the
/// returned tuple's field 0 are independent owners of independent
/// allocations; both must release cleanly and the returned copy must still
/// report the one element pushed.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn whole_tuple_escape_after_projection_transfer_runs_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("projected-tuple-owner-escape-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(escaping_partial_tuple_source(), dir.path(), "escape");
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "the returned tuple must survive the poisoned allocator:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        output.status.code(),
        Some(1),
        "the returned tuple's field 0 must still report the one element \
         pushed before the local `items` clone was taken"
    );
}

/// Was a refusal: the backedge carries `p` unchanged, so rereading `p.0` on
/// every iteration is ordinary code, not a use-after-move. The loop bound is
/// an independent counter so the backedge is genuinely taken (the original
/// fixture tied its exit to the projected length and only ever ran once).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn loop_backedge_rereads_the_cloned_field_every_iteration() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("projected-tuple-owner-loop-reread-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        loop_reread_after_transfer_source(),
        dir.path(),
        "loop_reread",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "three backedge-carried rereads of the cloned field must survive the \
         poisoned allocator:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        output.status.code(),
        Some(3),
        "each of the three iterations must clone `p.0` and read one element, \
         so the running total is exactly three"
    );
}
