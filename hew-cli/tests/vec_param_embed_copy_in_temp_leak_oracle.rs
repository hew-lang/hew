//! Prepared parameter carriers embedded in anonymous Vec COPY-IN source temps.
//!
//! A whole by-value parameter remains caller-owned. Direct calls prepare an
//! independent carrier when a callee moves a deep-owned projection into a sink;
//! the callee then neutralizes that moved leaf and drops every remaining sibling.
//! These oracles pin flat leak slopes and prove that caller values remain readable
//! and naturally droppable under the poisoned allocator.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const PUSH_TEMPLATE: &str = r#"
type Wrap { f: Option<string> }

fn pushParam(p: string) -> i64 {
    let v: Vec<Wrap> = [];
    v.push(Wrap { f: Some(p) });
    v.len()
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        let p = f"item-{i}";
        if pushParam(p) != 1 { return 11; }
        if p.len() < 6 { return 12; }
    }
    0
}
"#;

const SET_TEMPLATE: &str = r#"
type Wrap { f: Option<string> }

fn setParam(p: string) -> i64 {
    let v: Vec<Wrap> = [];
    v.push(Wrap { f: None });
    v.set(0, Wrap { f: Some(p) });
    v.len()
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        let p = f"item-{i}";
        if setParam(p) != 1 { return 21; }
        if p.len() < 6 { return 22; }
    }
    0
}
"#;

const BOUND_FIRST_TEMPLATE: &str = r#"
type Wrap { f: Option<string> }

fn pushParam(p: string) -> i64 {
    let v: Vec<Wrap> = [];
    let w = Wrap { f: Some(p) };
    v.push(w);
    v.len()
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        let p = f"item-{i}";
        if pushParam(p) != 1 { return 31; }
        if p.len() < 6 { return 32; }
    }
    0
}
"#;

const MOVE_TEMPLATE: &str = r#"
type Wrap { f: Option<string> }

fn main() -> i64 {
    for i in 0..$FRAMES {
        let v: Vec<Wrap> = [];
        v.push(Wrap { f: Some(f"item-{i}") });
        if v.len() != 1 { return 41; }
    }
    0
}
"#;

const TUPLE_TEMPLATE: &str = r#"
fn pushParam(p: string) -> i64 {
    let v: Vec<(string, i64)> = [];
    v.push((p, 1));
    v.len()
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        let p = f"item-{i}";
        if pushParam(p) != 1 { return 45; }
        if p.len() < 6 { return 46; }
    }
    0
}
"#;

const ARENA_TEMPLATE: &str = r#"
type Key<T> { index: i64 }
type Slot<T> { value: Option<T> }
type Arena<T> { slots: Vec<Slot<T>> }

fn newArena<T>() -> Arena<T> {
    Arena { slots: Vec::new() }
}

impl<T> Arena<T> {
    fn insert(self, value: T) -> Key<T> {
        let index = self.slots.len();
        self.slots.push(Slot { value: Some(value) });
        Key { index: index }
    }

    fn remove(self, key: Key<T>) -> T {
        let slot = self.slots.remove(key.index);
        slot.value.unwrap()
    }

    fn len(self) -> i64 {
        self.slots.len()
    }
}

fn main() -> i64 {
    let arena = newArena<string>();
    for i in 0..$FRAMES {
        let key = arena.insert(f"item-{i}");
        let got = arena.remove(key);
        let expected = f"item-{i}";
        if got != expected { return 52; }
        if got.len() != expected.len() { return 53; }
        if arena.len() != 0 { return 55; }
    }
    0
}
"#;

const PARAM_NOT_DOUBLE_FREED_SOURCE: &str = r#"
type Wrap { f: Option<string> }

fn pushParam(p: string) -> i64 {
    let v: Vec<Wrap> = [];
    v.push(Wrap { f: Some(p) });
    v.len()
}

fn setParam(p: string) -> i64 {
    let v: Vec<Wrap> = [];
    v.push(Wrap { f: None });
    v.set(0, Wrap { f: Some(p) });
    v.len()
}

fn main() -> i64 {
    let p = "param-pin".to_upper();
    print(p.len());
    print("|");
    print(pushParam(p));
    print("|");
    print(p.len());
    print("|");
    print(setParam(p));
    print("|");
    print(p.len());
    print("|");
    print(p);
    print("|OK");
    0
}
"#;

const MIXED_PARAM_NOT_DOUBLE_FREED_SOURCE: &str = r#"
type Holder { items: Vec<string> }
type MixedWrap { s: string, items: Vec<string> }

fn pushMixed(p: string, h: Holder) -> i64 {
    let v: Vec<MixedWrap> = [];
    v.push(MixedWrap { s: p, items: h.items });
    v.len()
}

fn main() -> i64 {
    let p = "param-pin".to_upper();
    let h = Holder { items: ["deep-pin".to_upper()] };
    print(p.len());
    print("|");
    print(pushMixed(p, h));
    print("|");
    print(p);
    print("|");
    print(h.items.len());
    print("|OK");
    0
}
"#;

const MIXED_PARAM_TEMPLATE: &str = r#"
type Holder { items: Vec<string> }
type MixedWrap { s: string, items: Vec<string> }

fn pushMixed(p: string, h: Holder) -> i64 {
    let v: Vec<MixedWrap> = [];
    v.push(MixedWrap { s: p, items: h.items });
    v.len()
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        let p = f"item-{i}";
        let h = Holder { items: [f"deep-{i}"] };
        if pushMixed(p, h) != 1 { return 81; }
        if p.len() < 6 { return 82; }
        if h.items.len() != 1 { return 83; }
    }
    0
}
"#;

const NESTED_RECORD_PARAM_TEMPLATE: &str = r#"
type Inner { items: Vec<string>, inner_keep: string }
type Holder { mid: Inner, outer_keep: string }
type MixedWrap { marker: string, items: Vec<string> }

fn pushNested(h: Holder) -> i64 {
    let v: Vec<MixedWrap> = [];
    v.push(MixedWrap {
        marker: "sink-marker".to_upper(),
        items: h.mid.items,
    });
    v.len()
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        let h = Holder {
            mid: Inner {
                items: [f"nested-{i}"],
                inner_keep: f"inner-{i}",
            },
            outer_keep: f"outer-{i}",
        };
        if pushNested(h) != 1 { return 91; }
        if h.mid.items.len() != 1 { return 92; }
        if h.mid.inner_keep.len() < 7 { return 93; }
        if h.outer_keep.len() < 7 { return 94; }
    }
    0
}
"#;

const TUPLE_PROJECTION_PARAM_TEMPLATE: &str = r#"
type MixedWrap { marker: string, items: Vec<string> }

fn pushTupleProjection(h: (Vec<string>, string)) -> i64 {
    let v: Vec<MixedWrap> = [];
    v.push(MixedWrap {
        marker: "tuple-marker".to_upper(),
        items: h.0,
    });
    v.len()
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        let h = ([f"tuple-{i}"], f"keep-{i}");
        if pushTupleProjection(h) != 1 { return 101; }
        if h.0.len() != 1 { return 102; }
        if h.1.len() < 6 { return 103; }
    }
    0
}
"#;

const PROJECTION_NEUTRALIZE_MIR_SOURCE: &str = r#"
type Inner { items: Vec<string>, keep: string }
type Holder { mid: Inner, keep: string }
type MixedWrap { marker: string, items: Vec<string> }

fn nested(h: Holder) {
    let v: Vec<MixedWrap> = [];
    v.push(MixedWrap { marker: "nested".to_upper(), items: h.mid.items });
}

fn tupled(h: (Vec<string>, string)) {
    let v: Vec<MixedWrap> = [];
    v.push(MixedWrap { marker: "tuple".to_upper(), items: h.0 });
}
"#;

const NON_STRING_PARAM_SOURCE: &str = r"
type Holder { items: Vec<string> }
type Wrap { f: Option<Holder> }
type MixedWrap { s: string, items: Vec<string> }

fn pushParam(p: Holder) {
    let v: Vec<Wrap> = [];
    v.push(Wrap { f: Some(p) });
}

fn pushMixed(p: string, h: Holder) {
    let v: Vec<MixedWrap> = [];
    v.push(MixedWrap { s: p, items: h.items });
}
";

fn with_frames(template: &str, frames: usize) -> String {
    template.replace("$FRAMES", &frames.to_string())
}

fn push_source(frames: usize) -> String {
    with_frames(PUSH_TEMPLATE, frames)
}

fn set_source(frames: usize) -> String {
    with_frames(SET_TEMPLATE, frames)
}

fn bound_first_source(frames: usize) -> String {
    with_frames(BOUND_FIRST_TEMPLATE, frames)
}

fn move_source(frames: usize) -> String {
    with_frames(MOVE_TEMPLATE, frames)
}

fn tuple_source(frames: usize) -> String {
    with_frames(TUPLE_TEMPLATE, frames)
}

fn arena_source(frames: usize) -> String {
    with_frames(ARENA_TEMPLATE, frames)
}

fn mixed_param_source(frames: usize) -> String {
    with_frames(MIXED_PARAM_TEMPLATE, frames)
}

fn nested_record_param_source(frames: usize) -> String {
    with_frames(NESTED_RECORD_PARAM_TEMPLATE, frames)
}

fn tuple_projection_param_source(frames: usize) -> String {
    with_frames(TUPLE_PROJECTION_PARAM_TEMPLATE, frames)
}

fn assert_source_succeeds_under_scribble(shape_name: &str, source_fn: fn(usize) -> String) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-param-embed-run-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&source_fn(3), dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name} must complete its semantic checks before its leak slope is trusted:\n{}",
        describe_output(&output)
    );
}

fn assert_clean_slope(shape_name: &str, source_fn: fn(usize) -> String) {
    assert_source_succeeds_under_scribble(shape_name, source_fn);
    assert_frame_slope_below_tolerance(shape_name, source_fn);
}

fn dump_checked_mir(source: &str, name: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix("vec-param-embed-mir-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&source_path, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args(["compile", "--dump-mir", "checked"])
        .arg(&source_path)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-mir checked");
    assert!(
        output.status.success(),
        "checked MIR dump failed:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("checked MIR is UTF-8")
}

#[test]
fn direct_push_param_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_push", push_source);
}

#[test]
fn direct_set_param_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_set", set_source);
}

#[test]
fn bound_first_copy_in_control_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_bound_first", bound_first_source);
}

#[test]
fn no_param_embed_move_control_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_move", move_source);
}

#[test]
fn tuple_param_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_tuple", tuple_source);
}

#[test]
fn arena_insert_remove_param_embed_has_flat_leak_slope() {
    // `arena_source(100)` is the explicit diagnostic form that reproduces the
    // historical 100-node/3200-byte signature with the mint removed.
    assert_clean_slope("vec_param_embed_arena", arena_source);
}

#[test]
fn mixed_parameter_projection_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_mixed", mixed_param_source);
}

#[test]
fn nested_record_projection_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_nested_record", nested_record_param_source);
}

#[test]
fn tuple_projection_embed_has_flat_leak_slope() {
    assert_clean_slope(
        "vec_param_embed_tuple_projection",
        tuple_projection_param_source,
    );
}

#[test]
fn nested_and_tuple_projection_moves_neutralize_the_original_carrier_slots() {
    let mir = dump_checked_mir(
        PROJECTION_NEUTRALIZE_MIR_SOURCE,
        "projection_neutralize_depth",
    );
    let nested = mir
        .split("fn nested")
        .nth(1)
        .and_then(|section| section.split("fn tupled").next())
        .expect("nested MIR section");
    let tupled = mir.split("fn tupled").nth(1).expect("tuple MIR section");
    assert!(
        nested.contains("aggregate_projection_neutralize _0 fields=[0, 0]"),
        "a two-hop record projection must clear the original carrier leaf; removing second-hop provenance must fail this tooth:\n{nested}"
    );
    assert!(
        tupled.contains("aggregate_projection_neutralize _0 fields=[0]"),
        "a tuple projection must clear the original carrier leaf; removing TupleIndex propagation must fail this tooth:\n{tupled}"
    );
    for section in [nested, tupled] {
        assert_eq!(
            section.matches("aggregate_projection_neutralize").count(),
            1,
            "each moved leaf has exactly one neutralization authority:\n{section}"
        );
        assert_eq!(
            section.matches("snapshot_drop _0").count(),
            1,
            "each carrier keeps one terminal sibling drop after its moved leaf is cleared:\n{section}"
        );
    }
}

#[test]
fn caller_parameter_survives_push_set_and_natural_drop() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("vec-param-embed-caller-pin-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        PARAM_NOT_DOUBLE_FREED_SOURCE,
        dir.path(),
        "param_not_double_freed",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "the mint must drop only the temp's retained share; the caller still owns p:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "9|1|9|1|9|PARAM-PIN|OK",
        "every post-store read must observe the live caller-owned parameter:\n{}",
        describe_output(&output)
    );
}

#[test]
fn mixed_parameter_projection_survives_store_and_natural_drop() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("vec-param-embed-mixed-caller-pin-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        MIXED_PARAM_NOT_DOUBLE_FREED_SOURCE,
        dir.path(),
        "mixed_param_not_double_freed",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "a mixed temp must not drop the caller-owned projected Vec:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "9|1|PARAM-PIN|1|OK",
        "both caller-owned parameters must remain live after the store:\n{}",
        describe_output(&output)
    );
}

#[test]
fn non_string_param_embed_uses_owned_carrier_temp() {
    let mir = dump_checked_mir(NON_STRING_PARAM_SOURCE, "non_string_param_embed");
    assert!(mir.contains("call hew_vec_push_owned("));
    assert!(!mir.contains("hew_vec_push_owned_move"));
    assert!(
        mir.contains("__hew_copy_in_param_temp"),
        "a caller-prepared Holder carrier makes the copy-in temp an independent owner:\n{mir}"
    );
    assert!(
        mir.contains("snapshot_drop _0 ty=Holder") && mir.contains("boundary=LocalCall"),
        "the same carrier fact must install the callee's inverse Holder cleanup:\n{mir}"
    );
}
