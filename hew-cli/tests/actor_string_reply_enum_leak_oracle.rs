//! Actor string-reply / nested-enum-payload leak oracle (#2717).
//!
//! ## The shape
//!
//! An actor whose handler returns an `enum` carrying a heap payload in one
//! variant and a SCALAR payload in a sibling variant:
//!
//! ```text
//! enum Status { Loaded(i64); Described(string); }
//! actor Describer {
//!     let name: string;
//!     receive fn describe(suffix: string, sel: i64) -> Status {
//!         if sel % 2 == 0 { Loaded(sel) } else { Described(name + suffix) }
//!     }
//! }
//! …
//! let r = await d.describe("-suffix", i);   // r: Result<Status, AskError>
//! match r { Ok(st) => match st { Described(s) => …, Loaded(v) => … }, Err(_) => {} }
//! ```
//!
//! The ask reply is byte-copied into the caller as `Result<Status, AskError>`.
//! The nested `match Ok(st) => match st { … }` destructures the inner `Status`
//! enum out of the outer `Result`. Both composites are heap-owning enum
//! candidates; the outer composite's recursive `EnumInPlace` drop is the single
//! owner of the `Described(string)` payload on the normal path.
//!
//! ## What regressed (reproduced, fresh builds — trunk pre-fix)
//!
//! The inner match's SCALAR arm — `Loaded(v)` binding `v: i64` — lowers as a
//! `Move { dest: v, src: <interior projection of the Status payload binder> }`.
//! The enum-composite drop prover
//! (`hew_mir::lower::composite_own::derive_enum_composite_drop_allowed`) misread
//! that bitcopy destructure as a PAYLOAD ESCAPE (the destination is not a tracked
//! heap-owning binder, so the benign-hand-off gate failed), and its blanket
//! `note_payload_escape` coarsening excluded EVERY candidate root — the outer
//! `Result` AND the inner `Status`. With both composites excluded, neither's
//! `EnumInPlace` fired on the normal path, so the `Described(string)` payload —
//! a `hew_string_concat` → `alloc_cstring_data` block — leaked ~48 bytes per
//! reply. Measured on trunk: ~1 leak / reply (2000 replies → 1999 leaks / 95 952
//! bytes). Surfaced under `leaks --atExit` while validating #2641.
//!
//! A plain-`string` reply (`Result<string, AskError>`, a leaf payload with no
//! nested scalar sibling) never hit the bitcopy-escape path and did NOT leak;
//! both a concat and a constant plain-string reply are pinned here as controls.
//!
//! ## The fix
//!
//! A bitcopy destructure of a payload binder (a `Move` whose destination is a
//! non-heap-owning `Place::Local`) carries no heap out of the composite, so it is
//! NOT a payload escape — the composite still solely owns any sibling variant's
//! heap. The prover now exempts it, mirroring the `local_is_heap_owning` guard
//! its seeding/propagation loops already apply and the sibling `place_is_tag_read`
//! discriminant exemption. The outer composite is re-admitted and its recursive
//! `EnumInPlace` frees the nested payload exactly once.
//!
//! ## What this oracle pins
//!
//! * flat leak SLOPE across a 47-iteration delta for the actor enum reply, the
//!   concat plain-string reply, the constant plain-string reply, and the
//!   non-actor `Result<Status, i64>` substrate reproduction (the minimal shape
//!   the prover fix targets);
//! * no double-free / use-after-free under the poisoned allocator — the handler
//!   alternates the heap and scalar variants so BOTH the composite `EnumInPlace`
//!   drop and the scalar-extraction path run every iteration; an over-eager
//!   re-admission that double-freed the payload aborts here.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── source fixtures ─────────────────────────────────────────────────────

/// Headline #2717 shape: actor returns an enum with a heap variant
/// (`Described(string)`, built by concat) and a scalar sibling
/// (`Loaded(i64)`). The reply is `Result<Status, AskError>`; the caller
/// double-matches. The handler alternates variants so both drop paths run.
fn actor_enum_reply_source(frames: usize) -> String {
    format!(
        "enum Status {{\n\
         \x20   Loaded(i64);\n\
         \x20   Described(string);\n\
         }}\n\
         \n\
         actor Describer {{\n\
         \x20   let name: string;\n\
         \x20   receive fn describe(suffix: string, sel: i64) -> Status {{\n\
         \x20       if sel % 2 == 0 {{\n\
         \x20           Loaded(sel)\n\
         \x20       }} else {{\n\
         \x20           Described(name + suffix)\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let d = spawn Describer(name: \"widget\");\n\
         \x20   var i: i64 = 0;\n\
         \x20   var sink: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = await d.describe(\"-suffix\", i);\n\
         \x20       match r {{\n\
         \x20           Ok(st) => {{\n\
         \x20               match st {{\n\
         \x20                   Described(s) => {{ sink = sink + s.len(); }},\n\
         \x20                   Loaded(v) => {{ sink = sink + v; }},\n\
         \x20               }}\n\
         \x20           }},\n\
         \x20           Err(_) => {{}},\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if sink > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Concat plain-string reply control: `receive fn … -> string` returning a
/// freshly concatenated string, matched `Ok(s)`. Never hit the bitcopy-escape
/// path; pinned flat as a control.
fn actor_string_concat_reply_source(frames: usize) -> String {
    format!(
        "actor Greeter {{\n\
         \x20   let name: string;\n\
         \x20   receive fn greet(suffix: string) -> string {{\n\
         \x20       name + suffix\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let g = spawn Greeter(name: \"widget\");\n\
         \x20   var i: i64 = 0;\n\
         \x20   var sink: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = await g.greet(\"-suffix\");\n\
         \x20       match r {{\n\
         \x20           Ok(s) => {{ sink = sink + s.len(); }},\n\
         \x20           Err(_) => {{}},\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if sink > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Constant plain-string reply control: the handler returns a heap-backed
/// string literal, matched `Ok(s)`.
fn actor_string_plain_reply_source(frames: usize) -> String {
    format!(
        "actor Tagger {{\n\
         \x20   let unused: i64;\n\
         \x20   receive fn tag() -> string {{\n\
         \x20       \"constant-reply-value-heap\"\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let t = spawn Tagger(unused: 0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   var sink: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = await t.tag();\n\
         \x20       match r {{\n\
         \x20           Ok(s) => {{ sink = sink + s.len(); }},\n\
         \x20           Err(_) => {{}},\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if sink > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Non-actor substrate reproduction: a plain function returning
/// `Result<Status, i64>`, double-matched exactly like the ask path. This is the
/// minimal shape the prover fix targets — the ask boundary just always wraps a
/// reply in `Result<R, AskError>`, so the same nested-enum-with-scalar-sibling
/// destructure reaches the prover.
fn result_enum_scalar_sibling_source(frames: usize) -> String {
    format!(
        "enum Status {{\n\
         \x20   Loaded(i64);\n\
         \x20   Described(string);\n\
         }}\n\
         \n\
         fn describe(name: string, suffix: string, sel: i64) -> Result<Status, i64> {{\n\
         \x20   if sel % 2 == 0 {{\n\
         \x20       Ok(Loaded(sel))\n\
         \x20   }} else {{\n\
         \x20       Ok(Described(name + suffix))\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var sink: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = describe(\"widget\", \"-suffix\", i);\n\
         \x20       match r {{\n\
         \x20           Ok(st) => {{\n\
         \x20               match st {{\n\
         \x20                   Described(s) => {{ sink = sink + s.len(); }},\n\
         \x20                   Loaded(v) => {{ sink = sink + v; }},\n\
         \x20               }}\n\
         \x20           }},\n\
         \x20           Err(_) => {{}},\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if sink > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

// ── slope oracles ───────────────────────────────────────────────────────

/// The headline #2717 leak: actor enum reply with a scalar sibling holds a flat
/// slope post-fix (pre-fix: ~1 leak / reply).
#[test]
fn actor_enum_reply_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("actor_enum_reply", actor_enum_reply_source);
}

/// Concat plain-string reply control holds a flat slope.
#[test]
fn actor_string_concat_reply_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "actor_string_concat_reply",
        actor_string_concat_reply_source,
    );
}

/// Constant plain-string reply control holds a flat slope.
#[test]
fn actor_string_plain_reply_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("actor_string_plain_reply", actor_string_plain_reply_source);
}

/// Non-actor `Result<Status, i64>` substrate reproduction holds a flat slope —
/// the minimal shape the prover fix targets.
#[test]
fn result_enum_scalar_sibling_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "result_enum_scalar_sibling",
        result_enum_scalar_sibling_source,
    );
}

// ── scribble (double-free / use-after-free) pins ────────────────────────

/// Run one shape at a small iteration count under the poisoned allocator and
/// require a clean exit. An over-eager re-admission that double-freed the nested
/// payload (or an `EnumInPlace` fired on a moved-out slot) aborts here.
fn assert_scribble_clean(name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("actor-reply-scribble-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — an abort/crash \
         here means the nested-enum payload was freed more than once (an \
         over-eager composite re-admission double-freeing through the payload \
         alias);\n{}",
        describe_output(&output)
    );
}

/// Actor enum reply: the handler alternates the heap and scalar variants, so
/// both the composite `EnumInPlace` drop and the scalar-extraction path run.
#[test]
fn actor_enum_reply_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("actor_enum_reply", &actor_enum_reply_source(6));
}

/// Concat plain-string reply: no double-free of the reply string.
#[test]
fn actor_string_concat_reply_no_double_free_under_malloc_scribble() {
    assert_scribble_clean(
        "actor_string_concat_reply",
        &actor_string_concat_reply_source(6),
    );
}

/// Non-actor substrate reproduction: no re-walk of the nested payload.
#[test]
fn result_enum_scalar_sibling_no_double_free_under_malloc_scribble() {
    assert_scribble_clean(
        "result_enum_scalar_sibling",
        &result_enum_scalar_sibling_source(6),
    );
}
