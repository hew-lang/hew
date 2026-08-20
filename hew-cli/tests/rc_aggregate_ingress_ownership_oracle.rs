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
//! The CONDITIONAL shape is the one a path-insensitive fix gets wrong in the
//! other direction: only one arm places the handle into the aggregate, so the
//! transfer record has to be path-local or the not-taken arm leaks.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, require_leaks_tool,
    run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

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
    match total >= 0 {{
        true => 0,
        false => 1,
    }}
}}
"#
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
    let run = run_under_malloc_scribble(&binary);
    assert!(
        run.status.success(),
        "{name}: moving a refcounted handle into a value aggregate must transfer its single \
         strong count exactly once — a second release aborts the process:\n{}",
        describe_output(&run)
    );
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
