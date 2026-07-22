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

const RESOURCE_PROJECTION_DIRECT_CONSUME_TEMPLATE: &str = r#"
#[resource]
type Token { payload: string }

impl Token {
    fn close(self) {
        let sink: Vec<string> = [];
        sink.push(self.payload);
    }
}

type Holder { token: Token, keep: string }

fn consumeToken(consume token: Token) {}

fn maybeConsume(h: Holder, take: bool) {
    if take {
        consumeToken(h.token);
    }
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        let h = Holder {
            token: Token { payload: f"payload-{i}" },
            keep: f"keep-{i}",
        };
        maybeConsume(h, true);
        if h.token.payload.len() < 9 { return 111; }
        if h.keep.len() < 6 { return 112; }
    }
    0
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

const ASSOCIATED_STATIC_CARRIER_SOURCE: &str = r#"
type Ops { marker: i64 }
type Holder { items: Vec<string>, keep: string }
type Wrap { items: Vec<string> }

enum Payload {
    Text(string);
    Scalar(i64);
}

impl Ops {
    fn storeRecord(h: Holder) -> i64 {
        let v: Vec<Wrap> = [];
        v.push(Wrap { items: h.items });
        v.len()
    }

    fn storeTuple(h: (Vec<string>, string)) -> i64 {
        let v: Vec<Wrap> = [];
        v.push(Wrap { items: h.0 });
        v.len()
    }

    fn storeEnum(h: Payload) -> i64 {
        match h {
            Payload::Text(s) => {
                let v: Vec<string> = [];
                v.push(s);
                v.len()
            },
            Payload::Scalar(_) => 0,
        }
    }
}

fn main() -> i64 {
    let h = Holder { items: ["record"], keep: "keep" };
    let t = (["tuple"], "keep");
    let e = Payload::Text("enum");
    let total = Ops::storeRecord(h) + Ops::storeTuple(t) + Ops::storeEnum(e);
    if h.items.len() != 1 { return 91; }
    if t.0.len() != 1 { return 92; }
    total
}
"#;

const REUSABLE_CALLABLE_PARAM_SOURCE: &str = r"
fn apply(f: fn(i64) -> i64, x: i64) -> i64 {
    f(x)
}

fn main() {
    let double = |x: i64| x * 2;
    println(apply(double, 21));
    println(apply(double, 2));
}
";

const STORED_CALLABLE_PARAM_SOURCE: &str = r"
type CallableBox { f: fn(i64) -> i64 }

fn store(f: fn(i64) -> i64) -> CallableBox {
    CallableBox { f: f }
}

fn main() {
    let double = |x: i64| x * 2;
    let _boxed = store(double);
    println(double(2));
}
";

const RETURNED_CALLABLE_PARAM_SOURCE: &str = r"
fn carry(f: fn(i64) -> i64) -> fn(i64) -> i64 {
    f
}

fn main() {
    let double = |x: i64| x * 2;
    let _copy = carry(double);
    println(double(2));
}
";

const CAPTURED_CALLABLE_PARAM_SOURCE: &str = r"
fn carry(f: fn(i64) -> i64) -> fn(i64) -> i64 {
    |x| f(x)
}

fn main() {
    let double = |x: i64| x * 2;
    let _copy = carry(double);
    println(double(2));
}
";

const HYBRID_ENUM_TEMPLATE: &str = r#"
#[opaque]
type Handle {}

extern "C" {
    fn hew_handle_create() -> Handle;
}

enum Mixed {
    Text(string);
    Opaque(Handle);
}

fn inspect(x: Mixed) -> i64 {
    match x {
        Mixed::Text(s) => s.len(),
        Mixed::Opaque(_) => 0,
    }
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        if inspect(Mixed::Text(f"payload-{i}".to_upper())) < 9 { return 61; }
    }
    0
}
"#;

const CLONEABLE_ENUM_CONTROL_SOURCE: &str = r#"
enum Mixed {
    Text(string);
    Scalar(i64);
}

fn inspect(x: Mixed) -> i64 {
    match x {
        Mixed::Text(s) => s.len(),
        Mixed::Scalar(_) => 0,
    }
}

fn main() -> i64 {
    inspect(Mixed::Text("payload".to_upper()))
}
"#;

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

fn resource_projection_direct_consume_source(frames: usize) -> String {
    with_frames(RESOURCE_PROJECTION_DIRECT_CONSUME_TEMPLATE, frames)
}

fn hybrid_enum_source(frames: usize) -> String {
    with_frames(HYBRID_ENUM_TEMPLATE, frames)
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

fn dump_mir(source: &str, name: &str, stage: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix("vec-param-embed-mir-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&source_path, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args(["compile", "--dump-mir", stage])
        .arg(&source_path)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-mir");
    assert!(
        output.status.success(),
        "{stage} MIR dump failed:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("MIR dump is UTF-8")
}

fn dump_checked_mir(source: &str, name: &str) -> String {
    dump_mir(source, name, "checked")
}

fn dump_elaborated_mir(source: &str, name: &str) -> String {
    dump_mir(source, name, "elab")
}

fn compile_callable_source(source: &str, name: &str, target: Option<&str>) -> std::process::Output {
    let dir = tempfile::Builder::new()
        .prefix("owned-callable-param-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join(format!("{name}.hew"));
    let emit_dir = dir.path().join("emit");
    std::fs::create_dir(&emit_dir).expect("create emit dir");
    std::fs::write(&source_path, source).expect("write Hew source");
    let mut command = Command::new(hew_binary());
    command.arg("compile").arg(&source_path);
    if let Some(target) = target {
        command.args(["--target", target]);
    }
    command
        .arg("--emit-dir")
        .arg(&emit_dir)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile")
}

fn compile_source_to_llvm(source: &str, name: &str, target: Option<&str>) -> String {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("owned-call-carrier-llvm-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join(format!("{name}.hew"));
    let emit_dir = dir.path().join("emit");
    std::fs::create_dir(&emit_dir).expect("create emit dir");
    std::fs::write(&source_path, source).expect("write Hew source");
    let mut command = Command::new(hew_binary());
    command.arg("compile").arg(&source_path);
    if let Some(target) = target {
        command.args(["--target", target]);
    }
    let output = command
        .arg("--emit-dir")
        .arg(&emit_dir)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        output.status.success(),
        "LLVM emission failed for {name}:\n{}",
        describe_output(&output)
    );
    std::fs::read_to_string(emit_dir.join(format!("{name}.ll"))).expect("read emitted LLVM IR")
}

fn llvm_function_body<'a>(ir: &'a str, symbol: &str) -> &'a str {
    let marker = format!("@{symbol}(");
    let start = ir
        .lines()
        .position(|line| line.starts_with("define ") && line.contains(&marker))
        .unwrap_or_else(|| panic!("missing LLVM definition for {symbol}"));
    let lines: Vec<&str> = ir.lines().collect();
    let end = lines[start..]
        .iter()
        .position(|line| *line == "}")
        .map(|offset| start + offset + 1)
        .expect("LLVM function terminator");
    let byte_start: usize = lines[..start].iter().map(|line| line.len() + 1).sum();
    let byte_end: usize = lines[..end].iter().map(|line| line.len() + 1).sum();
    &ir[byte_start..byte_end.min(ir.len())]
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
fn resource_projection_direct_consume_preserves_the_poisoned_caller() {
    assert_source_succeeds_under_scribble(
        "resource_projection_direct_consume",
        resource_projection_direct_consume_source,
    );
}

#[test]
fn resource_projection_direct_consume_has_flat_leak_slope() {
    assert_clean_slope(
        "resource_projection_direct_consume",
        resource_projection_direct_consume_source,
    );
}

#[test]
fn prepared_resource_projection_transfers_once_into_direct_consume() {
    let source = resource_projection_direct_consume_source(1);
    let checked = dump_checked_mir(&source, "resource_projection_direct_consume");
    let maybe_consume = checked
        .split("fn maybeConsume")
        .nth(1)
        .and_then(|section| section.split("fn main").next())
        .expect("maybeConsume checked MIR section");
    assert_eq!(
        maybe_consume
            .matches("aggregate_projection_neutralize _0 fields=[0]")
            .count(),
        1,
        "the prepared Holder carrier must transfer its Token leaf exactly once; removing projection authority must fail this tooth:\n{maybe_consume}"
    );
    assert_eq!(
        maybe_consume.matches("snapshot_clone").count(),
        0,
        "an already-prepared Token owner must transfer into the consuming callee without a second clone; removing prepared-owner propagation must fail this tooth:\n{maybe_consume}"
    );
    assert!(
        maybe_consume.contains("call consumeToken(_3)"),
        "the transferred projection local must be the consuming call argument:\n{maybe_consume}"
    );
    assert_eq!(
        maybe_consume
            .matches("snapshot_drop _0 ty=Holder")
            .count(),
        1,
        "the partially neutralized carrier must retain exactly one terminal sibling cleanup:\n{maybe_consume}"
    );

    let elaborated = dump_elaborated_mir(&source, "resource_projection_direct_consume");
    let consume_token = elaborated
        .split("fn consumeToken")
        .nth(1)
        .and_then(|section| section.split("fn maybeConsume").next())
        .expect("consumeToken elaborated MIR section");
    assert_eq!(
        consume_token
            .matches("drop _0 ty=Token kind=record_in_place")
            .count(),
        1,
        "the consuming callee must keep the transferred Token's one disposal authority:\n{consume_token}"
    );
}

#[test]
fn non_cloneable_hybrid_enum_uses_legacy_callee_drop_after_transfer() {
    let hybrid_source = hybrid_enum_source(1);
    let checked = dump_checked_mir(&hybrid_source, "hybrid_enum_carrier");
    let inspect = checked
        .split("fn inspect")
        .nth(1)
        .and_then(|section| section.split("fn main").next())
        .expect("inspect checked MIR section");
    assert_eq!(
        inspect.matches("snapshot_drop _0").count(),
        0,
        "a non-clone-total plan must not claim installed carrier machinery:\n{inspect}"
    );
    let main = checked.split("fn main").nth(1).expect("main checked MIR");
    assert_eq!(
        main.matches("neutralize_payload").count(),
        1,
        "the unique temporary must transfer sole ownership into inspect:\n{main}"
    );
    assert!(
        main.contains("call inspect("),
        "the neutralized temporary must feed the direct callee:\n{main}"
    );

    let elaborated = dump_elaborated_mir(&hybrid_source, "hybrid_enum_carrier");
    let inspect = elaborated
        .split("fn inspect")
        .nth(1)
        .and_then(|section| section.split("fn main").next())
        .expect("inspect elaborated MIR section");
    let return_plan = inspect
        .split("return[bb1] ->")
        .nth(1)
        .and_then(|section| section.split('\n').nth(1))
        .expect("inspect return drop plan");
    assert!(
        return_plan.contains("drop _0 ty=Mixed kind=enum_in_place"),
        "an uninstalled carrier must leave the heap-owning enum fallback active:\n{inspect}"
    );

    let cloneable = dump_checked_mir(CLONEABLE_ENUM_CONTROL_SOURCE, "cloneable_enum_carrier");
    let cloneable_inspect = cloneable
        .split("fn inspect")
        .nth(1)
        .and_then(|section| section.split("fn main").next())
        .expect("cloneable inspect checked MIR section");
    assert_eq!(
        cloneable_inspect
            .matches("snapshot_drop _0 ty=Mixed")
            .count(),
        2,
        "replacing the opaque variant with a scalar must install the clone-total carrier on return and trap; this control separates plan admission from the legacy fallback:\n{cloneable_inspect}"
    );
}

#[test]
fn hybrid_enum_text_transfer_drops_once_on_native_and_wasm() {
    let source = hybrid_enum_source(1);
    for (target_name, target) in [("native", None), ("wasm32", Some("wasm32-unknown-unknown"))] {
        let ir = compile_source_to_llvm(
            &source,
            &format!("hybrid_enum_carrier_{target_name}"),
            target,
        );
        let inspect = llvm_function_body(&ir, "inspect");
        let return_block = inspect
            .split("bb1:")
            .nth(1)
            .and_then(|section| section.split("\nbb2:").next())
            .expect("inspect return block");
        assert_eq!(
            return_block
                .matches("call void @__hew_enum_drop_inplace_Mixed(")
                .count(),
            1,
            "the successful inspect path must dispose its transferred enum exactly once ({target_name}):\n{return_block}"
        );
        assert_eq!(
            inspect
                .matches("call void @__hew_enum_drop_inplace_Mixed(")
                .count(),
            5,
            "every return, trap, and cancellation exit must keep one mutually exclusive enum cleanup ({target_name}):\n{inspect}"
        );
        let drop_thunk = llvm_function_body(&ir, "__hew_enum_drop_inplace_Mixed");
        assert_eq!(
            drop_thunk.matches("call void @hew_string_drop(").count(),
            1,
            "the Text variant must have exactly one payload release in the shared enum authority ({target_name}):\n{drop_thunk}"
        );
    }
}

#[test]
fn hybrid_enum_text_transfer_has_flat_leak_slope() {
    assert_clean_slope("hybrid_enum_text_transfer", hybrid_enum_source);
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
    let tupled = mir
        .split("fn tupled")
        .nth(1)
        .and_then(|section| section.split("fn i8::fmt").next())
        .expect("tuple MIR section");
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

#[test]
fn associated_static_param_zero_keeps_record_tuple_and_enum_carriers() {
    let mir = dump_checked_mir(
        ASSOCIATED_STATIC_CARRIER_SOURCE,
        "associated_static_carriers",
    );
    for (next, name, ty) in [
        ("Ops::storeTuple", "Ops::storeRecord", "Holder"),
        ("Ops::storeEnum", "Ops::storeTuple", "(Vec<string>, string)"),
        ("main", "Ops::storeEnum", "Payload"),
    ] {
        let section = mir
            .split(&format!("fn {name}"))
            .nth(1)
            .and_then(|body| body.split(&format!("fn {next}")).next())
            .unwrap_or_else(|| panic!("missing checked MIR section for {name}:\n{mir}"));
        assert!(
            section.contains("snapshot_drop _0") && section.contains(&format!("ty={ty}")),
            "associated/static parameter zero must retain the owned carrier contract for {ty}:\n{section}"
        );
    }
    let main = mir.split("fn main").nth(1).expect("main checked MIR");
    assert_eq!(
        main.matches("snapshot_clone").count(),
        2,
        "the live-after-call record and tuple values must each mint one independent owner:\n{main}"
    );
    assert!(
        main.contains("neutralize_payload") && main.contains("call Ops::storeEnum"),
        "the last-use enum value must transfer its owner into the static call carrier:\n{main}"
    );
}

#[test]
fn owned_carrier_cancel_exit_drops_once_on_native_and_wasm() {
    require_codegen();
    for (target_name, target) in [("native", None), ("wasm32", Some("wasm32-unknown-unknown"))] {
        let ir = compile_source_to_llvm(
            CLONEABLE_ENUM_CONTROL_SOURCE,
            &format!("owned_carrier_cancel_{target_name}"),
            target,
        );
        let inspect = llvm_function_body(&ir, "inspect");
        let cancel = inspect
            .split("cancel_exit:")
            .nth(1)
            .and_then(|section| section.split("after_cooperate:").next())
            .expect("inspect cancellation block");
        assert_eq!(
            cancel
                .matches("call void @__hew_enum_drop_inplace_Mixed(")
                .count(),
            1,
            "runtime cancellation code 2 must release the prepared enum carrier exactly once ({target_name}):\n{cancel}"
        );
        let normal_drop_count = inspect
            .split("bb1:")
            .nth(1)
            .and_then(|section| section.split("ret i64").next())
            .expect("inspect normal return block")
            .matches("call void @__hew_enum_drop_inplace_Mixed(")
            .count();
        assert_eq!(
            normal_drop_count, 1,
            "the ordinary return path must retain one mutually exclusive carrier cleanup ({target_name}):\n{inspect}"
        );
    }
}

#[test]
fn reusable_callable_parameter_invocation_compiles_native_and_wasm() {
    require_codegen();
    for (target_name, target) in [("native", None), ("wasm32", Some("wasm32-unknown-unknown"))] {
        let output = compile_callable_source(
            REUSABLE_CALLABLE_PARAM_SOURCE,
            "reusable_callable_param",
            target,
        );
        assert!(
            output.status.success(),
            "invoking a reusable callable parameter only borrows its pair ({target_name}):\n{}",
            describe_output(&output)
        );
    }
}

#[test]
fn callable_storage_return_and_capture_stay_fail_closed_on_both_targets() {
    require_codegen();
    for (shape, source) in [
        ("stored", STORED_CALLABLE_PARAM_SOURCE),
        ("returned", RETURNED_CALLABLE_PARAM_SOURCE),
        ("captured", CAPTURED_CALLABLE_PARAM_SOURCE),
    ] {
        for (target_name, target) in [("native", None), ("wasm32", Some("wasm32-unknown-unknown"))]
        {
            let output = compile_callable_source(source, shape, target);
            assert!(
                !output.status.success(),
                "a callable parameter moved into an owning {shape} sink must not be relaxed ({target_name}):\n{}",
                describe_output(&output)
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("live owned call-carrier `fn(i64) -> i64`"),
                "the {shape} mutation must retain the owned-carrier rejection ({target_name}):\n{}",
                describe_output(&output)
            );
        }
    }
}
