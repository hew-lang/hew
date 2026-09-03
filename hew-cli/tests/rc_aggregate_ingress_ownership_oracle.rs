//! Ownership oracle for an `Rc` / `Weak` handle moved into a VALUE aggregate.
//!
//! Placing a refcounted handle into a tuple, an `Option` / `Result` payload, a
//! record field, or a nested record byte-copies the handle with no retain: the
//! aggregate's composite drop (`tuple_in_place` / `enum_in_place` /
//! `record_in_place`) becomes an owner of the same strong count the source
//! binder still carries a `DropKind::RcRelease` obligation for. Both directions
//! of that imbalance are pinned here, and both must hold for every shape:
//!
//! * NO OVER-RELEASE — a second release underflows the strong count and the
//!   runtime aborts (`Rc double-free: strong refcount is already 0`), so a clean
//!   exit under the poisoned allocator is itself the over-release oracle. This
//!   half needs no `clone` anywhere: `let pair = (shared, "tag")` alone aborted
//!   before the aggregate-ingress transfer was recorded.
//! * NO UNDER-RELEASE — the per-iteration leak slope must stay flat. Skipping
//!   the binder's release on a path that did NOT transfer the handle would leak
//!   one allocation per frame, which the slope catches and a single-shot
//!   measurement would not.
//!
//! Both scope-exit release ORDERS are covered per shape, because the two orders
//! reach the imbalance through different drop plans:
//!
//! * AGGREGATE-FIRST — the aggregate is a local dropped at the same scope exit,
//!   released ahead of the still-in-scope binder by the LIFO plan.
//! * BINDER-FIRST — the aggregate is returned to the caller, so the callee's
//!   exit sees only the binder and the caller's frame releases the aggregate.
//!
//! The CONDITIONAL shapes are the ones a path-insensitive answer gets wrong,
//! in both directions at once. Only one arm places the handle, so the transfer
//! record has to be path-local or the not-taken arm leaks — and if the record is
//! missing entirely, the join meets `Live` with the moved state, yields
//! `MaybeConsumed`, and the drop filter admits it as live, releasing a handle the
//! aggregate already owns.
//!
//! The shapes span both transfer ROUTES, because they are different code paths:
//! the aggregate-alias funnel (tuple, `Option`/`Result`, record, nested record,
//! machine payload) and the general consume seam (the array-literal desugar's
//! owned-move element push). Reassignment is covered separately as a generation
//! boundary: the record is path-local runtime state and must retire when the slot
//! takes a fresh value, or the replacement handle is never released.

#![cfg(unix)]

mod support;

use std::process::Command;
use std::time::Duration;

#[cfg(target_os = "macos")]
use support::leak_slope::run_under_malloc_scribble;
use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, require_leaks_tool,
};
use support::{describe_output, require_codegen, try_run_bounded_command};

/// Ceiling for one probe run, mirroring the leak harness's own bound for the
/// same kind of run. Every shape here is a bounded counting loop that finishes
/// in milliseconds; a probe that has not exited by now is hung, and a hung probe
/// has established nothing.
const PROBE_TIMEOUT: Duration = Duration::from_secs(90);

/// One frame prints exactly one line, so the leak-slope harness can pin the
/// drained iteration count instead of settling for monotonicity.
fn expected_lines(frames: usize) -> usize {
    frames
}

/// AGGREGATE-FIRST: `pair` and `shared` are both locals of `frame`, so the LIFO
/// scope-exit plan releases the tuple before the binder.
fn tuple_local_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(seed: i64) -> i64 {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    let pair: (Rc<Node>, string) = (shared, "tag");
    pair.1.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed);
        println("frame");
    }}
    total - {frames} * 3
}}
"#
    )
}

/// BINDER-FIRST: the tuple leaves `frame` and is released by the caller's
/// frame, so the callee's exit plan must not release the binder behind it.
fn tuple_returned_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(seed: i64) -> (Rc<Node>, string) {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    (shared, "tag")
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        let pair = frame(seed);
        total = total + pair.1.len();
        println("frame");
    }}
    total - {frames} * 3
}}
"#
    )
}

fn option_local_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(seed: i64) -> i64 {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    let held: Option<Rc<Node>> = Some(shared);
    match held.is_some() {{
        true => 1,
        false => 0,
    }}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed);
        println("frame");
    }}
    total - {frames}
}}
"#
    )
}

fn option_returned_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(seed: i64) -> Option<Rc<Node>> {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    Some(shared)
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        let held = frame(seed);
        match held.is_some() {{
            true => {{ total = total + 1; }}
            false => {{ }}
        }}
        println("frame");
    }}
    total - {frames}
}}
"#
    )
}

fn result_local_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(seed: i64) -> i64 {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    let held: Result<Rc<Node>, string> = Ok(shared);
    match held.is_ok() {{
        true => 1,
        false => 0,
    }}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed);
        println("frame");
    }}
    total - {frames}
}}
"#
    )
}

fn result_returned_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(seed: i64) -> Result<Rc<Node>, string> {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    Ok(shared)
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        let held = frame(seed);
        match held.is_ok() {{
            true => {{ total = total + 1; }}
            false => {{ }}
        }}
        println("frame");
    }}
    total - {frames}
}}
"#
    )
}

fn record_local_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}
type Holder {{ r: Rc<Node>; tag: string; }}

fn frame(seed: i64) -> i64 {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    let holder = Holder {{ r: shared, tag: "tag" }};
    holder.tag.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed);
        println("frame");
    }}
    total - {frames} * 3
}}
"#
    )
}

fn record_returned_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}
type Holder {{ r: Rc<Node>; tag: string; }}

fn frame(seed: i64) -> Holder {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    Holder {{ r: shared, tag: "tag" }}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        let holder = frame(seed);
        total = total + holder.tag.len();
        println("frame");
    }}
    total - {frames} * 3
}}
"#
    )
}

/// A handle two aggregate levels down: the outer record's composite drop
/// recurses into the inner record before reaching the handle.
fn nested_record_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}
type Inner {{ r: Rc<Node>; }}
type Outer {{ i: Inner; tag: string; }}

fn frame(seed: i64) -> i64 {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    let inner = Inner {{ r: shared }};
    let outer = Outer {{ i: inner, tag: "tag" }};
    outer.tag.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed);
        println("frame");
    }}
    total - {frames} * 3
}}
"#
    )
}

/// A `Weak` handle takes the same aggregate ingress and the same guarded
/// `WeakRelease` at scope exit, so it is pinned alongside `Rc`.
fn weak_local_source(frames: usize) -> String {
    format!(
        r#"
fn frame(seed: i64) -> i64 {{
    let rc = Rc.new(seed);
    let weak: Weak<i64> = rc.downgrade();
    let pair: (Weak<i64>, string) = (weak, "tag");
    pair.1.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed);
        println("frame");
    }}
    total - {frames} * 3
}}
"#
    )
}

/// Only the `true` arm places the handle into the tuple. The `false` arm must
/// still release it at scope exit, so a transfer record that is not path-local
/// converts this shape's fix into a per-frame leak.
fn conditional_ingress_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(flag: bool) -> i64 {{
    let shared: Rc<Node> = Rc.new(Node {{ id: 7 }});
    match flag {{
        true => {{
            let pair: (Rc<Node>, string) = (shared, "tag");
            pair.1.len()
        }}
        false => 0,
    }}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed % 2 == 0);
        println("frame");
    }}
    total - {expected}
}}
"#,
        expected = 3 * frames.div_ceil(2)
    )
}

/// ARRAY-LITERAL element ingress. The array-literal desugar binds a synthetic
/// `__hew_array_N` Vec and pushes each element through the owned-move ABI, a
/// DIFFERENT transfer route from the tuple/record funnel: it records the move as
/// a dataflow `Consume` via `mark_binding_moved` rather than an aggregate alias.
fn array_element_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(seed: i64) -> i64 {{
    let shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    let holders: Vec<Rc<Node>> = [shared];
    holders.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed);
        println("frame");
    }}
    total - {frames}
}}
"#
    )
}

/// CONDITIONAL array-literal element ingress — the shape a straight-line
/// transfer record gets wrong.
///
/// A dataflow `Consume` alone suffices when the move dominates the exit
/// (`filter_drops_by_state` excludes a `Consumed` binding), but on a join with
/// an arm that did NOT move, the meet is `MaybeConsumed`, which the same filter
/// admits as LIVE. The binder's guarded release then fires on the arm that
/// already handed the handle to the Vec. Before the transfer record moved onto
/// the general consume seam this aborted with `Rc double-free`.
fn conditional_array_element_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(flag: bool) -> i64 {{
    let shared: Rc<Node> = Rc.new(Node {{ id: 7 }});
    match flag {{
        true => {{
            let holders: Vec<Rc<Node>> = [shared];
            holders.len()
        }}
        false => 0,
    }}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed % 2 == 0);
        println("frame");
    }}
    total - {expected}
}}
"#,
        expected = frames.div_ceil(2)
    )
}

/// MACHINE-PAYLOAD ingress: the handle is placed into a state payload through
/// `Place::MachineVariant`.
///
/// Over-release only. The leak half is deliberately NOT asserted for this shape:
/// the machine LOCAL itself has no scope-exit composite drop here — the compiler
/// says so, with an `ObligationUnderReleased` advisory naming `c` — so the whole
/// state value leaks once per frame no matter what its payload is. Measured, not
/// assumed: the identical machine with a `string` payload leaks at the same rate.
/// That is a machine-composite drop gap, not a refcount-ownership defect, and
/// pinning it here would ratchet an unrelated defect into this oracle.
fn machine_payload_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

machine Cell {{
    events {{ Fill; Drain; }}
    state Empty;
    state Full {{ r: Rc<Node>; }}
    on Fill: Empty => Full {{
        let shared: Rc<Node> = Rc.new(Node {{ id: 7 }});
        Full {{ r: shared }}
    }}
    on Drain: Full => Empty {{ Cell.Empty }}
    default {{ state }}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for _seed in 0..{frames} {{
        var c: Cell = .Empty;
        c.step(.Fill);
        match c {{
            .Empty => {{ total = total + 0; }}
            .Full {{ r }} => {{ total = total + r.get().id; }}
        }}
        println("frame");
    }}
    total - {expected}
}}
"#,
        expected = frames * 7
    )
}

/// REASSIGNMENT after ingress — a generation boundary.
///
/// The transfer record is path-local runtime state, so it has to be retired when
/// the slot receives a fresh value: the replacement handle is owned by this frame
/// outright and must be released at scope exit. Left at 1 by the ingress, the
/// guard suppressed the replacement's release and this leaked one handle per
/// frame.
fn reassign_after_ingress_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(seed: i64) -> i64 {{
    var shared: Rc<Node> = Rc.new(Node {{ id: seed }});
    let pair: (Rc<Node>, string) = (shared, "tag");
    shared = Rc.new(Node {{ id: seed + 1 }});
    pair.1.len() + shared.get().id - seed - 1
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed);
        println("frame");
    }}
    total - {expected}
}}
"#,
        expected = frames * 3
    )
}

/// The same generation boundary crossed once per loop iteration, so a flag that
/// latches at 1 leaks every iteration rather than once.
fn reassign_in_loop_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn main() -> i64 {{
    var total: i64 = 0;
    var shared: Rc<Node> = Rc.new(Node {{ id: 0 }});
    for seed in 0..{frames} {{
        let pair: (Rc<Node>, string) = (shared, "tag");
        shared = Rc.new(Node {{ id: seed }});
        total = total + pair.1.len();
        println("frame");
    }}
    total - {expected}
}}
"#,
        expected = frames * 3
    )
}

/// ALTERNATING move / no-move, then reassign — the shape that exercises the
/// `flag == 0` half of the guarded overwrite release.
///
/// The two straight reassignment shapes above always transfer before they
/// reassign, so they only ever prove the `flag == 1` path (skip the release,
/// the aggregate owns it). Here half the frames never move, so the outgoing
/// generation is still frame-owned at the store and the release MUST run — the
/// binding's scope-exit drop discharges only the last generation, so a skipped
/// release here is a leak that no later drop can recover.
fn alternating_reassign_source(frames: usize) -> String {
    format!(
        r#"
type Node {{ id: i64; }}

fn frame(flag: bool) -> i64 {{
    var shared: Rc<Node> = Rc.new(Node {{ id: 7 }});
    let n = match flag {{
        true => {{
            let pair: (Rc<Node>, string) = (shared, "tag");
            pair.1.len()
        }}
        false => 0,
    }};
    shared = Rc.new(Node {{ id: 1 }});
    n + shared.get().id
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed % 2 == 0);
        println("frame");
    }}
    total - {expected}
}}
"#,
        expected = 4 * frames.div_ceil(2) + (frames / 2)
    )
}

/// The same alternating boundary over a `Weak` handle, which takes the sibling
/// release symbol and the sibling drop kind.
fn alternating_weak_reassign_source(frames: usize) -> String {
    format!(
        r#"
fn frame(flag: bool) -> i64 {{
    let rc = Rc.new(7);
    var w: Weak<i64> = rc.downgrade();
    let n = match flag {{
        true => {{
            let pair: (Weak<i64>, string) = (w, "tag");
            pair.1.len()
        }}
        false => 0,
    }};
    w = rc.downgrade();
    n + rc.get()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed % 2 == 0);
        println("frame");
    }}
    total - {expected}
}}
"#,
        expected = 10 * frames.div_ceil(2) + 7 * (frames / 2)
    )
}

/// A reassignment whose RHS RETAINS the same allocation.
///
/// `Rc.clone()` hands back the handle already in the slot, so this is the shape
/// the overwrite release must not be vetoed for and must not double-free on: the
/// retain is lowered before the release, so `+1` strictly precedes the `-1` and
/// the count cannot reach zero across the store. Running it unretained would
/// free a live allocation, which the poisoned allocator turns into an abort on
/// the next read.
///
/// The allocation is made INSIDE the frame, once per iteration, and that is
/// load-bearing for the leak half. Hoisting it above the loop and self-cloning
/// in place — the obvious way to write this shape — makes a skipped release
/// invisible: it only bumps one allocation's refcount, so nothing new is ever
/// allocated and the leak NODE count stays flat however many iterations run.
/// Allocating per frame turns the same missing release into one orphaned
/// allocation per frame, which is what the slope measures. Verified by deleting
/// the overwrite-release arm: this shape then leaks 1 node per frame, and it was
/// the only shape in the file that did not notice.
fn retained_self_reassign_source(frames: usize) -> String {
    format!(
        r#"
fn frame(seed: i64) -> i64 {{
    var shared: Rc<i64> = Rc.new(seed);
    shared = shared.clone();
    shared.get()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + frame(seed);
        println("frame");
    }}
    total - {expected}
}}
"#,
        expected = frames * (frames - 1) / 2
    )
}

/// Compile the shape at a fixed frame count and run it under the poisoned
/// allocator (`MallocScribble` / `MallocPreScribble` / `MallocGuardEdges`).
///
/// This is the OVER-release half. A second `hew_rc_drop` against a handle whose
/// strong count already reached zero panics inside the runtime and kills the
/// process, so a probe that exits under its own control with status 0 has
/// proved the release count is not too high. The poisoned allocator turns the
/// weaker failure — a release that frees storage the aggregate drop then reads
/// — into an abort as well, rather than a silent read of stale bytes.
fn assert_shape_does_not_over_release(name: &str, source_fn: fn(usize) -> String) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("rc-aggregate-ingress-{name}-"))
        .tempdir()
        .expect("tempdir");
    let binary = compile_to_native(&source_fn(16), dir.path(), name);

    // EVERY host runs this half. Releasing a shared handle once too often trips
    // the runtime's own `Rc double-free` guard, which aborts the process, so a
    // probe that reaches its own exit with status 0 has proved the release count
    // is not too high. That guard lives in `hew-runtime`, not in the allocator,
    // so the oracle is the same everywhere and this half must NOT be gated to
    // macOS — gating it would leave the over-release direction unmeasured on the
    // host CI actually gates on.
    let run = try_run_bounded_command(
        Command::new(&binary),
        format!("run the {name} over-release probe"),
        PROBE_TIMEOUT,
    )
    .unwrap_or_else(|error| {
        panic!("{name}: probe did not finish within {PROBE_TIMEOUT:?}: {error}")
    });
    assert!(
        run.status.success(),
        "{name}: moving a refcounted handle into a value aggregate must transfer its single \
         strong count exactly once — a second release aborts the process:\n{}",
        describe_output(&run)
    );

    // macOS additionally runs it under the poisoned-allocator triple, which
    // turns the weaker failure — a release that frees storage the aggregate drop
    // then reads — into an abort rather than a silent read of stale bytes.
    // `MallocScribble` / `MallocPreScribble` / `MallocGuardEdges` are Darwin
    // libmalloc facilities that are silently ignored elsewhere, so this is the
    // only part of the assertion that is host-specific.
    #[cfg(target_os = "macos")]
    {
        let scribbled = run_under_malloc_scribble(&binary);
        assert!(
            scribbled.status.success(),
            "{name}: the probe exited cleanly but aborted under the poisoned allocator, so a \
             release freed storage that is still read:\n{}",
            describe_output(&scribbled)
        );
    }
}

/// The UNDER-release half: the per-iteration leak slope must stay flat.
fn assert_shape_does_not_under_release(name: &str, source_fn: fn(usize) -> String) {
    require_codegen();
    require_leaks_tool();
    assert_frame_slope_below_tolerance_exact_lines(name, source_fn, expected_lines);
}

macro_rules! aggregate_ingress_shape {
    ($over:ident, $under:ident, $name:literal, $source:ident) => {
        #[test]
        fn $over() {
            assert_shape_does_not_over_release($name, $source);
        }

        #[cfg_attr(
            not(target_os = "macos"),
            ignore = "leak oracle needs macOS `leaks(1)`; absence must be a counted skip"
        )]
        #[test]
        fn $under() {
            assert_shape_does_not_under_release($name, $source);
        }
    };
}

aggregate_ingress_shape!(
    tuple_member_aggregate_first_does_not_over_release,
    tuple_member_aggregate_first_does_not_under_release,
    "tuple_local",
    tuple_local_source
);

aggregate_ingress_shape!(
    tuple_member_binder_first_does_not_over_release,
    tuple_member_binder_first_does_not_under_release,
    "tuple_returned",
    tuple_returned_source
);

aggregate_ingress_shape!(
    option_payload_aggregate_first_does_not_over_release,
    option_payload_aggregate_first_does_not_under_release,
    "option_local",
    option_local_source
);

aggregate_ingress_shape!(
    option_payload_binder_first_does_not_over_release,
    option_payload_binder_first_does_not_under_release,
    "option_returned",
    option_returned_source
);

aggregate_ingress_shape!(
    result_payload_aggregate_first_does_not_over_release,
    result_payload_aggregate_first_does_not_under_release,
    "result_local",
    result_local_source
);

aggregate_ingress_shape!(
    result_payload_binder_first_does_not_over_release,
    result_payload_binder_first_does_not_under_release,
    "result_returned",
    result_returned_source
);

aggregate_ingress_shape!(
    record_field_aggregate_first_does_not_over_release,
    record_field_aggregate_first_does_not_under_release,
    "record_local",
    record_local_source
);

aggregate_ingress_shape!(
    record_field_binder_first_does_not_over_release,
    record_field_binder_first_does_not_under_release,
    "record_returned",
    record_returned_source
);

aggregate_ingress_shape!(
    nested_record_field_does_not_over_release,
    nested_record_field_does_not_under_release,
    "nested_record",
    nested_record_source
);

aggregate_ingress_shape!(
    weak_handle_tuple_member_does_not_over_release,
    weak_handle_tuple_member_does_not_under_release,
    "weak_local",
    weak_local_source
);

aggregate_ingress_shape!(
    conditional_ingress_does_not_over_release,
    conditional_ingress_does_not_under_release,
    "conditional_ingress",
    conditional_ingress_source
);

aggregate_ingress_shape!(
    array_literal_element_does_not_over_release,
    array_literal_element_does_not_under_release,
    "array_element",
    array_element_source
);

aggregate_ingress_shape!(
    conditional_array_literal_element_does_not_over_release,
    conditional_array_literal_element_does_not_under_release,
    "conditional_array_element",
    conditional_array_element_source
);

aggregate_ingress_shape!(
    reassign_after_ingress_does_not_over_release,
    reassign_after_ingress_does_not_under_release,
    "reassign_after_ingress",
    reassign_after_ingress_source
);

aggregate_ingress_shape!(
    reassign_in_loop_does_not_over_release,
    reassign_in_loop_does_not_under_release,
    "reassign_in_loop",
    reassign_in_loop_source
);

aggregate_ingress_shape!(
    alternating_reassign_does_not_over_release,
    alternating_reassign_does_not_under_release,
    "alternating_reassign",
    alternating_reassign_source
);

aggregate_ingress_shape!(
    alternating_weak_reassign_does_not_over_release,
    alternating_weak_reassign_does_not_under_release,
    "alternating_weak_reassign",
    alternating_weak_reassign_source
);

aggregate_ingress_shape!(
    retained_self_reassign_does_not_over_release,
    retained_self_reassign_does_not_under_release,
    "retained_self_reassign",
    retained_self_reassign_source
);

/// Machine-payload ingress carries the over-release half only; see
/// [`machine_payload_source`] for the measured reason the leak half is not
/// pinned on this shape.
#[test]
fn machine_state_payload_does_not_over_release() {
    assert_shape_does_not_over_release("machine_payload", machine_payload_source);
}
