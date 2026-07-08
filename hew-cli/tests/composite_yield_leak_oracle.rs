//! Per-yield leak / double-free oracle for owned-composite stream elements:
//! a `receive gen fn` yielding a record/enum whose fields own heap must free
//! BOTH per-yield copies exactly once — the pump's producer copy (released on
//! the stream-send resume edge through the `__hew_record_drop_inplace_<R>` /
//! `__hew_enum_drop_inplace_<E>` thunk; its abandon-edge twin rides the
//! suspend plan) and the consumer's fresh decode copy (released at the
//! consuming body's end through the same thunk).
//!
//! ## Ownership shape under test
//!
//! The layout-witness send (`encode_elem_envelope`, `LayoutManaged`)
//! deep-clones the element into the queue envelope — for a `string` field the
//! clone is a refcount retain, so producer and consumer hold two REFERENCES
//! to one allocation. Slope 0 therefore requires BOTH releases: either one
//! alone leaves the node reachable-from-nothing at refcount 1 (the
//! pre-fix red measured exactly 1 leaked node / 32 B per yield with both
//! references leaked).
//!
//! The failure modes this oracle catches:
//!   * a LEAK (either side's release missing) — positive per-yield slope;
//!   * a DOUBLE-FREE (a release reaching the OTHER side's reference, or one
//!     side releasing twice across the break/body-end edge pair) — an abort
//!     under the poisoned-allocator triple;
//!   * an OVER-DROP corrupting a live value — scribbled output / non-zero
//!     exit (each fixture self-checks its computed total).
//!
//! ## Consumer-shape boundary
//!
//! The enum SLOPE fixture drains without touching the payload, so the frame
//! binding's body-end release is the sole consumer-side owner. A consumer
//! that DESTRUCTURES the payload (`match n { Text(s) => ... }`) moves
//! ownership out of the frame binding — the body-end release is correctly
//! suppressed, and releasing the moved-out arm binding per iteration is the
//! match-arm consume-side discipline, tracked separately from this seam. The
//! destructuring shape is covered here by a no-double-free scribble pin (its
//! per-iteration release is the tracked gap; over-releasing it would be the
//! regression this oracle must catch).

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// -- fixtures ----------------------------------------------------------------

/// Owned-record yields drained by a `for await` body that reads only the
/// `BitCopy` field, so the frame binding keeps ownership of the heap `name`
/// field for its whole iteration and the body-end release is the sole
/// consumer-side dropper. The heap field is deliberately NOT read in the
/// slope body: a retained string field read leaves a per-iteration
/// interpolation/getter temp behind — a pre-existing temp-discipline class
/// (verified: identical leak signature on the pre-fix baseline) that would
/// mask this seam's slope. The scribble pin below reads the field instead
/// (an over-drop there is a poisoned read, not a leak). `main` self-checks
/// the running total so a scribbled read cannot pass silently.
fn record_yield_loop_source(frames: usize) -> String {
    let expected_total = frames * (frames.saturating_sub(1)) / 2;
    format!(
        "record Item {{\n\
         \x20   name: string,\n\
         \x20   value: i64,\n\
         }}\n\
         actor Maker {{\n\
         \x20   receive gen fn items() -> Item {{\n\
         \x20       for i in 0..{frames} {{\n\
         \x20           yield Item {{ name: f\"item-{{i}}\", value: i }};\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   let m = spawn Maker;\n\
         \x20   for await it in m.items() {{\n\
         \x20       total = total + it.value;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 91; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// The scribble-pin twin of [`record_yield_loop_source`]: reads the heap
/// `name` field each iteration (a retained read BEFORE the body-end release),
/// so an over-eager release — the producer's drop reaching the consumer's
/// copy, or a premature body-end drop — surfaces as a poisoned read /
/// self-check failure under the scribbled allocator.
fn record_yield_field_read_loop_source(frames: usize) -> String {
    let expected_total = frames * (frames.saturating_sub(1)) / 2;
    format!(
        "record Item {{\n\
         \x20   name: string,\n\
         \x20   value: i64,\n\
         }}\n\
         actor Maker {{\n\
         \x20   receive gen fn items() -> Item {{\n\
         \x20       for i in 0..{frames} {{\n\
         \x20           yield Item {{ name: f\"item-{{i}}\", value: i }};\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   let m = spawn Maker;\n\
         \x20   for await it in m.items() {{\n\
         \x20       if it.name.len() < 6 {{ return 93; }}\n\
         \x20       total = total + it.value;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 91; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Owned-enum yields (heap `string` payload per frame) drained WITHOUT
/// touching the payload: the frame binding stays the sole consumer-side
/// owner, so its body-end enum in-place release must fire per iteration.
fn enum_yield_drain_loop_source(frames: usize) -> String {
    format!(
        "enum Note {{\n\
         \x20   Text(string);\n\
         \x20   Number(i64);\n\
         }}\n\
         actor Maker {{\n\
         \x20   receive gen fn notes() -> Note {{\n\
         \x20       for i in 0..{frames} {{\n\
         \x20           yield Note::Text(f\"note-{{i}}\");\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var count: i64 = 0;\n\
         \x20   let m = spawn Maker;\n\
         \x20   for await n in m.notes() {{\n\
         \x20       count = count + 1;\n\
         \x20   }}\n\
         \x20   if count != {frames} {{ return 92; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Owned-enum yields drained by a DESTRUCTURING consumer: each `Text(s)`
/// payload moves out of the frame binding into the arm binding, whose length
/// feeds the self-checked total. The frame binding's body-end release must be
/// suppressed (ownership left the frame) — over-releasing it would free the
/// string the arm binding still reads (caught by the poisoned allocator).
fn enum_yield_destructure_loop_source(frames: usize) -> String {
    // Every payload is "note-<i>"; len = 5 + digits(i).
    let expected_total: usize = (0..frames).map(|i| 5 + i.to_string().len()).sum();
    format!(
        "enum Note {{\n\
         \x20   Text(string);\n\
         \x20   Number(i64);\n\
         }}\n\
         actor Maker {{\n\
         \x20   receive gen fn notes() -> Note {{\n\
         \x20       for i in 0..{frames} {{\n\
         \x20           yield Note::Text(f\"note-{{i}}\");\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   let m = spawn Maker;\n\
         \x20   for await n in m.notes() {{\n\
         \x20       match n {{\n\
         \x20           Note::Text(s) => {{ total = total + s.len(); }},\n\
         \x20           Note::Number(v) => {{ total = total + v; }},\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 94; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Mid-drain `break` on an infinite owned-record stream: the break edge must
/// release the breaking iteration's record (the break jumps past the body-end
/// release) and the fall-through iterations must not double-release across
/// the mutually-exclusive edge pair.
fn record_yield_break_loop_source(frames: usize) -> String {
    // Sum of 0..=frames-1 plus the breaking iteration's value (== frames).
    let expected_total = frames * (frames.saturating_sub(1)) / 2 + frames;
    format!(
        "record Item {{\n\
         \x20   name: string,\n\
         \x20   value: i64,\n\
         }}\n\
         actor Maker {{\n\
         \x20   receive gen fn items() -> Item {{\n\
         \x20       var i: i64 = 0;\n\
         \x20       loop {{\n\
         \x20           yield Item {{ name: f\"it-{{i}}\", value: i }};\n\
         \x20           i = i + 1;\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   let m = spawn Maker;\n\
         \x20   for await it in m.items() {{\n\
         \x20       total = total + it.value;\n\
         \x20       if it.value >= {frames} {{\n\
         \x20           break;\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 95; }}\n\
         \x20   0\n\
         }}\n"
    )
}

// -- correctness pins --------------------------------------------------------

/// Compile `source`, run under the poisoned-allocator triple, assert clean
/// exit. A crash is a double-free (one of the per-yield copies released
/// twice, or a release reaching the other side's reference); a non-zero exit
/// is a scribbled-read miscompute or the fixture's own self-check failing.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("composite-yield-df-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{shape_name}: a composite yield value must be released exactly once per owning \
         reference — a crash here is a double-free (the pump and consumer copies share \
         refcounted field allocations); a non-zero exit is a scribbled-read miscompute or \
         the fixture's own total check;\n{}",
        describe_output(&output)
    );
}

// -- oracles -----------------------------------------------------------------

/// Slope oracle (record): pump + consumer each release their copy per yield —
/// flat leak slope. Pre-fix this leaked exactly 1 node / 32 B per yield (the
/// record's `name` string, both references unreleased).
#[test]
fn record_yield_stream_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("record_yield_stream", record_yield_loop_source);
}

/// Slope oracle (enum, drain consumer): the tag-dispatched enum in-place
/// release fires on both sides per yield — flat leak slope.
#[test]
fn enum_yield_stream_drain_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("enum_yield_stream_drain", enum_yield_drain_loop_source);
}

/// No-double-free pin (record): 200 yields, retained heap-field reads in the
/// body — a producer-side release reaching the consumer's copy (or a
/// premature body-end drop) is a poisoned read here, not a leak.
#[test]
fn record_yield_stream_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("record_yield_df", &record_yield_field_read_loop_source(200));
}

/// No-double-free pin (enum, destructuring consumer): 200 yields; the
/// moved-out payload must never be freed by the frame binding's suppressed
/// body-end release.
#[test]
fn enum_yield_destructure_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "enum_yield_destructure_df",
        &enum_yield_destructure_loop_source(200),
    );
}

/// No-double-free pin (record, break edge): the break-edge release and the
/// body-end release are mutually exclusive; 200 fall-through iterations plus
/// the breaking one must stay exactly-once.
#[test]
fn record_yield_break_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "record_yield_break_df",
        &record_yield_break_loop_source(200),
    );
}
