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
    var v: Vec<Wrap> = [];
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
    var v: Vec<Wrap> = [];
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

/// A true-receiver trait-impl method forwards its BY-VALUE record receiver
/// into a summary-owning free function. Method callers keep ownership of the
/// receiver (the receiver summary is stripped), so the wrapper must SNAPSHOT
/// the receiver into the owning callee — a last-use transfer would hand the
/// callee release authority over string fields the original caller still
/// drops (the `Version::to_string` double-free).
const BORROWED_RECEIVER_FORWARD_SOURCE: &str = r#"
type Item { tag: i64, label: string, note: string }

trait Render {
    fn render(self) -> string;
}

impl Render for Item {
    fn render(item: Item) -> string {
        let result = render_item(item);
        result
    }
}

fn render_item(v: Item) -> string {
    let label = v.label;
    var s = "i" + ":";
    if label.len() > 0 {
        s = s + "-" + label;
    }
    s
}

fn main() -> i64 {
    let item = Item { tag: 1, label: "al" + "pha", note: "ke" + "ep" };
    let rendered = item.render();
    if rendered != "i:-alpha" { return 1; }
    if item.note != "keep" { return 2; }
    print(rendered);
    print("|");
    print(item.note);
    0
}
"#;

const REPEATED_OWNED_PARAM_READ_SOURCE: &str = r#"
type SavedKey { value: string }

fn borrowThenFree(key: string) -> i64 {
    var saved: Vec<SavedKey> = [];
    saved.push(SavedKey { value: key });
    saved.len()
}

fn inspectTwice(key: string) -> i64 {
    let first = borrowThenFree(key);
    let second = borrowThenFree(key);
    first + second + key.len()
}

fn main() -> i64 {
    let key = "carrier-guard".to_upper();
    print(inspectTwice(key));
    print("|");
    print(key);
    0
}
"#;

const TEMPLATE_IF_RANGE_SOURCE: &str =
    include_str!("../../tests/vertical-slice/accept/template_oracle_03_if_range.hew");

const BOUND_FIRST_TEMPLATE: &str = r#"
type Wrap { f: Option<string> }

fn pushParam(p: string) -> i64 {
    var v: Vec<Wrap> = [];
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
        var v: Vec<Wrap> = [];
        v.push(Wrap { f: Some(f"item-{i}") });
        if v.len() != 1 { return 41; }
    }
    0
}
"#;

const TUPLE_TEMPLATE: &str = r#"
fn pushParam(p: string) -> i64 {
    var v: Vec<(string, i64)> = [];
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
    Arena { slots: Vec.new() }
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
    var arena = newArena<string>();
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
    var v: Vec<Wrap> = [];
    v.push(Wrap { f: Some(p) });
    v.len()
}

fn setParam(p: string) -> i64 {
    var v: Vec<Wrap> = [];
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
    var v: Vec<MixedWrap> = [];
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
    var v: Vec<MixedWrap> = [];
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
    var v: Vec<MixedWrap> = [];
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
    var v: Vec<MixedWrap> = [];
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

const RESOURCE_PROJECTION_DIRECT_CONSUME_TEMPLATE: &str = r#"
#[resource]
type Token { payload: string }

impl Token {
    fn close(consume self) {
        var sink: Vec<string> = [];
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

const NON_STRING_PARAM_SOURCE: &str = r#"
type Holder { items: Vec<string> }
type Wrap { f: Option<Holder> }
type MixedWrap { s: string, items: Vec<string> }

fn pushParam(p: Holder) {
    var v: Vec<Wrap> = [];
    v.push(Wrap { f: Some(p) });
}

fn pushMixed(p: string, h: Holder) {
    var v: Vec<MixedWrap> = [];
    v.push(MixedWrap { s: p, items: h.items });
}

fn main() {
    pushParam(Holder { items: ["a"] });
    pushMixed("s", Holder { items: ["b"] });
}
"#;

const ASSOCIATED_STATIC_CARRIER_SOURCE: &str = r#"
type Ops { marker: i64 }
type Holder { items: Vec<string>, keep: string }
type Wrap { items: Vec<string> }

enum Payload {
    Text(string),
    Scalar(i64),
}

impl Ops {
    fn storeRecord(h: Holder) -> i64 {
        var v: Vec<Wrap> = [];
        v.push(Wrap { items: h.items });
        v.len()
    }

    fn storeTuple(h: (Vec<string>, string)) -> i64 {
        var v: Vec<Wrap> = [];
        v.push(Wrap { items: h.0 });
        v.len()
    }

    fn storeEnum(h: Payload) -> i64 {
        match h {
            Payload.Text(s) => {
                var v: Vec<string> = [];
                v.push(s);
                v.len()
            },
            Payload.Scalar(_) => 0,
        }
    }
}

fn main() -> i64 {
    let h = Holder { items: ["record"], keep: "keep" };
    let t = (["tuple"], "keep");
    let e = Payload.Text("enum");
    let total = Ops.storeRecord(h) + Ops.storeTuple(t) + Ops.storeEnum(e);
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
    Text(string),
    Opaque(Handle),
}

fn inspect(x: Mixed) -> i64 {
    match x {
        Mixed.Text(s) => s.len(),
        Mixed.Opaque(_) => 0,
    }
}

fn main() -> i64 {
    for i in 0..$FRAMES {
        if inspect(Mixed.Text(f"payload-{i}".to_upper())) < 9 { return 61; }
    }
    0
}
"#;

const CLONEABLE_ENUM_CONTROL_SOURCE: &str = r#"
enum Mixed {
    Text(string),
    Scalar(i64),
}

fn inspect(x: Mixed) -> i64 {
    match x {
        Mixed.Text(s) => s.len(),
        Mixed.Scalar(_) => 0,
    }
}

fn main() -> i64 {
    inspect(Mixed.Text("payload".to_upper()))
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

/// Run `hew compile --dump-sir`, the current text-form dump of ownership SIR
/// (params annotated `owned`/`guaranteed`, explicit `move`/`borrow`/
/// `copy_value`/`destroy_value`). This replaces the retired `--dump-mir
/// checked`/`elab` stages, which no longer exist (`--dump-mir` now accepts
/// only `physical`, whose debug dump carries no source-level ownership
/// vocabulary to match against).
fn dump_sir(source: &str, name: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix("vec-param-embed-sir-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&source_path, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args(["compile", "--dump-sir"])
        .arg(&source_path)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-sir");
    assert!(
        output.status.success(),
        "SIR dump failed:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("SIR dump is UTF-8")
}

/// Slice out one function's body from a `--dump-sir` text dump.
fn sir_function_section<'a>(dump: &'a str, symbol: &str) -> &'a str {
    let marker = format!("fn __hew_fn_{symbol}(");
    let start = dump
        .find(&marker)
        .unwrap_or_else(|| panic!("missing `{marker}` in SIR dump:\n{dump}"));
    let tail = &dump[start..];
    tail.find("\nfn ").map_or(tail, |next| &tail[..next])
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
        .arg("--emit-llvm")
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

fn llvm_basic_block<'a>(function: &'a str, label: &str) -> &'a str {
    let start = function
        .lines()
        .position(|line| line.starts_with(&format!("{label}:")))
        .unwrap_or_else(|| panic!("missing LLVM block `{label}`:\n{function}"));
    let lines: Vec<&str> = function.lines().collect();
    let end = lines[start + 1..]
        .iter()
        .position(|line| line.is_empty())
        .map_or(lines.len(), |offset| start + offset + 1);
    let byte_start: usize = lines[..start].iter().map(|line| line.len() + 1).sum();
    let byte_end: usize = lines[..end].iter().map(|line| line.len() + 1).sum();
    &function[byte_start..byte_end.min(function.len())]
}

/// Concatenated text of every basic block reachable from `start` (inclusive),
/// following the `label %target` references in each block's terminator.
///
/// A function whose parameter earns an elaborated plan drop arms the typed
/// helper crash-cleanup, and its return block then BRANCHES into
/// `helper_crash_cleanup_retire*` continuation blocks that LLVM prints far
/// from their predecessor — the plan drop the return path actually executes
/// lands in the retire-merge continuation, not in the return block's own
/// textual body (the same emission shape every admitted `EnumInPlace` local
/// drop has, e.g. a matched `Result<i64, string>` call scrutinee). A textual
/// `bb1:`..`bb2:` slice therefore misses the drop; the CFG walk follows the
/// path the machine takes.
fn llvm_reachable_path(function: &str, start: &str) -> String {
    use std::collections::VecDeque;
    let mut visited: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut queue = VecDeque::from([start.to_string()]);
    let mut path = String::new();
    while let Some(label) = queue.pop_front() {
        if !visited.insert(label.clone()) {
            continue;
        }
        let block = llvm_basic_block(function, &label);
        path.push_str(block);
        for successor in block.split("label %").skip(1) {
            let name: String = successor
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
                .collect();
            queue.push_back(name);
        }
    }
    path
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn direct_push_param_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_push", push_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn direct_set_param_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_set", set_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn bound_first_copy_in_control_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_bound_first", bound_first_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn no_param_embed_move_control_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_move", move_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn tuple_param_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_tuple", tuple_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn arena_insert_remove_param_embed_has_flat_leak_slope() {
    // `arena_source(100)` is the explicit diagnostic form that reproduces the
    // historical 100-node/3200-byte signature with the mint removed.
    assert_clean_slope("vec_param_embed_arena", arena_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn mixed_parameter_projection_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_mixed", mixed_param_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn nested_record_projection_embed_has_flat_leak_slope() {
    assert_clean_slope("vec_param_embed_nested_record", nested_record_param_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn tuple_projection_embed_has_flat_leak_slope() {
    assert_clean_slope(
        "vec_param_embed_tuple_projection",
        tuple_projection_param_source,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn resource_projection_direct_consume_preserves_the_poisoned_caller() {
    assert_source_succeeds_under_scribble(
        "resource_projection_direct_consume",
        resource_projection_direct_consume_source,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn resource_projection_direct_consume_has_flat_leak_slope() {
    assert_clean_slope(
        "resource_projection_direct_consume",
        resource_projection_direct_consume_source,
    );
}

// `prepared_resource_projection_transfers_once_into_direct_consume` pinned
// this fixture's `maybeConsume(h: Holder, take: bool)` transferring the
// prepared `h.token` leaf exactly once into `consumeToken(consume token:
// Token)`, against the retired checked/elaborated MIR text dumps. It is
// deleted here for a different reason than the rest of this file:
// `RESOURCE_PROJECTION_DIRECT_CONSUME_TEMPLATE` no longer compiles at all
// under the current checker —
//
//   error: E_OWN_CONSUME_BORROWED: cannot consume a value through borrowed
//   parameter `h`
//     consumeToken(h.token);
//
// `maybeConsume`'s `h: Holder` is a by-value (guaranteed/borrowed) parameter,
// and the checker now refuses consuming a projection out of it without
// `consume h: Holder`. This is a pre-existing defect independent of the
// dump-mir stage removal: it also breaks this file's two macOS-only
// `#[ignore]`d siblings on the same fixture
// (`resource_projection_direct_consume_preserves_the_poisoned_caller`,
// `resource_projection_direct_consume_has_flat_leak_slope`), so on any host
// that actually exercises the resource-consume path, all three fail. That
// checker/fixture mismatch is reported separately as an out-of-scope defect
// rather than fixed here or worked around with a modified fixture.

// `non_cloneable_hybrid_enum_publishes_callee_ownership_after_normal_return`
// pinned the same fact this file's
// `hybrid_enum_text_transfer_drops_once_on_native_and_wasm` proves at the
// generated-code level, for both native and wasm32, on every exit
// (`inspect`'s normal return, its unmatched-variant trap, each mutually
// exclusive cancellation exit, and the native unwind cleanup): the
// transferred `Mixed` enum carrier is disposed exactly once, and the
// caller-owned handoff stays live across the callee's unwind (the native
// `invoke.cleanup` path is asserted separately). It matched checked/
// elaborated MIR ownership-event text (`[WholeCarrierConsume]`,
// `[SendTransferLastUse]`, `ownership Transfer { from: Local(N), to: None
// }`, `snapshot_drop`) from the retired checked/elaborated MIR dumps, which
// modeled a caller-side copy-in-temp mechanism the current SIR/physical MIR
// pipeline does not have (callers pass `borrow`; callees `copy_value` their
// own owner when they need one — confirmed by inspecting `--dump-sir` on
// this file's other fixtures).
//
// Coverage note: `hybrid_enum_text_transfer_drops_once_on_native_and_wasm`
// targets this exact fact more thoroughly (every exit, both targets) and
// would be the right sibling, but it currently fails independent of this
// change — `HYBRID_ENUM_TEMPLATE`'s `enum Mixed { Text(string); ... }` used
// `;` between variants (fixed here to `,`), and beyond that syntax fix it
// still fails to reach LLVM emission with `E_SIR_UNSUPPORTED: ... nested
// type Handle has no semantic value contract` (an `#[opaque]` extern type
// used as an enum payload has no SIR value contract yet). Both are
// pre-existing defects unrelated to the dump-mir stage removal, reported
// separately; no test in this file currently proves this fact on any host.

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
        let return_path = llvm_reachable_path(inspect, "bb1");
        assert_eq!(
            return_path
                .matches("call void @__hew_enum_drop_inplace_Mixed(")
                .count(),
            1,
            "the successful inspect path must dispose its transferred enum exactly once ({target_name}):\n{return_path}"
        );
        let trap_path = llvm_reachable_path(inspect, "bb4");
        assert_eq!(
            trap_path
                .matches("call void @__hew_enum_drop_inplace_Mixed(")
                .count(),
            1,
            "the unmatched-variant trap must keep its one enum cleanup ({target_name}):\n\
             {trap_path}"
        );
        let cancellation_exits = inspect
            .lines()
            .filter_map(|line| line.split_once(':').map(|(label, _)| label))
            .filter(|label| label.starts_with("cancel_exit"))
            .collect::<Vec<_>>();
        assert_eq!(
            cancellation_exits.len(),
            3,
            "entry, Text, and Opaque paths must each have a cancellation exit ({target_name}):\n\
             {inspect}"
        );
        for block in cancellation_exits {
            let path = llvm_reachable_path(inspect, block);
            assert_eq!(
                path.matches("call void @__hew_enum_drop_inplace_Mixed(")
                    .count(),
                1,
                "{block} must keep its one mutually exclusive enum cleanup ({target_name}):\n\
                 {path}"
            );
        }
        let unwind_cleanup_count = if inspect.contains("invoke.cleanup:") {
            let unwind_path = llvm_reachable_path(inspect, "invoke.cleanup");
            assert_eq!(
                unwind_path
                    .matches("call void @__hew_enum_drop_inplace_Mixed(")
                    .count(),
                1,
                "the native invoke unwind path must dispose its still-caller-owned enum once:\n\
                 {unwind_path}"
            );
            1
        } else {
            0
        };
        assert_eq!(
            inspect
                .matches("call void @__hew_enum_drop_inplace_Mixed(")
                .count(),
            5 + unwind_cleanup_count,
            "every return, trap, cancellation, and supported unwind exit must keep one \
             mutually exclusive enum cleanup ({target_name}):\n{inspect}"
        );
        let drop_thunk = llvm_function_body(&ir, "__hew_enum_drop_inplace_Mixed");
        assert_eq!(
            drop_thunk.matches("call void @hew_string_drop(").count(),
            1,
            "the Text variant must have exactly one payload release in the shared enum authority ({target_name}):\n{drop_thunk}"
        );
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn hybrid_enum_text_transfer_has_flat_leak_slope() {
    assert_clean_slope("hybrid_enum_text_transfer", hybrid_enum_source);
}

// `nested_and_tuple_projection_moves_neutralize_the_original_carrier_slots`
// pinned that a moved-out nested-record leaf (`h.mid.items`) and a moved-out
// tuple leaf (`h.0`) each cleared ("neutralized") their source slot in a
// by-value parameter and kept one terminal drop for the remaining sibling
// fields, against the retired checked MIR text dump. `--dump-sir` on the
// same fixture (with the two helpers made reachable from a `main`, since
// the current pipeline only emits SIR for reachable functions, unlike the
// retired checked-MIR dump) shows the premise no longer holds: `h` is a
// `guaranteed` (borrowed) parameter, and both leaf reads lower to
// `aggregate.project_copy` (`h.mid.items`: two chained project_copy ops;
// `h.0`: one), each immediately paired with its own `destroy_value` after
// the copy is consumed. The caller never hands over a slot for the callee to
// clear, so there is no neutralization to pin: the current lowering copies
// the projected leaf and leaves the caller's whole parameter untouched.
// No coverage is lost to a stage change; the invariant this test asserted
// describes a mechanism the current architecture does not use for by-value
// parameters.

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
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

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
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
    // `hew_vec_push_owned`/`__hew_copy_in_param_temp`/`snapshot_drop ...
    // boundary=LocalCall` named the retired checked-MIR caller-side copy-in
    // mechanism. The current SIR pipeline instead has the callee mint its
    // own owner directly from a borrowed parameter: `pushParam`'s `p:
    // Holder` embeds via one `copy_value`, and `pushMixed`'s `h: Holder`
    // embeds its `items` field via one `aggregate.project_copy` — in both
    // cases from a `guaranteed` (borrowed) parameter, never a moved one.
    // That is the surviving form of "a caller-prepared carrier makes the
    // embed an independent owner, not an alias of the caller's storage".
    let sir = dump_sir(NON_STRING_PARAM_SOURCE, "non_string_param_embed");
    let push_param = sir_function_section(&sir, "pushParam");
    assert!(
        push_param.contains(": Holder guaranteed"),
        "pushParam's Holder parameter must stay borrowed, not consumed:\n{push_param}"
    );
    assert_eq!(
        push_param.matches("copy_value %0").count(),
        1,
        "the caller-prepared Holder carrier must mint exactly one independent owner:\n{push_param}"
    );

    let push_mixed = sir_function_section(&sir, "pushMixed");
    assert!(
        push_mixed.contains(": string guaranteed, %1: Holder guaranteed"),
        "pushMixed's parameters must stay borrowed, not consumed:\n{push_mixed}"
    );
    assert_eq!(
        push_mixed.matches("copy_value %0").count(),
        1,
        "the string embed must mint its own owned carrier:\n{push_mixed}"
    );
    assert!(
        push_mixed.contains("aggregate.project_copy record#0 %1, 0"),
        "the Holder's items field must embed via a copy, not a move of the caller's field:\n{push_mixed}"
    );
}

#[test]
fn associated_static_param_zero_keeps_record_tuple_and_enum_carriers() {
    // `snapshot_drop`/`snapshot_clone`/`neutralize_payload` named the
    // retired checked-MIR caller-side copy-in mechanism. The surviving
    // fact is that each associated-fn call passes its Holder/tuple/enum
    // argument by `borrow`, each callee's parameter zero stays `guaranteed`
    // (never consumed), and each callee mints its own independent owner
    // from that borrow — so `h` and `t` are still readable in `main` after
    // their calls, matching the source's post-call reads.
    let sir = dump_sir(
        ASSOCIATED_STATIC_CARRIER_SOURCE,
        "associated_static_carriers",
    );
    for (name, guaranteed_prefix, own_marker) in [
        (
            "Ops::storeRecord",
            "%0: Holder guaranteed",
            "aggregate.project_copy record#0 %0, 0",
        ),
        (
            "Ops::storeTuple",
            "%0: (Vec<string>, string) guaranteed",
            "aggregate.project_copy tuple %0, 0",
        ),
        ("Ops::storeEnum", "%0: Payload guaranteed", "copy_value %0"),
    ] {
        let section = sir_function_section(&sir, name);
        assert!(
            section.contains(guaranteed_prefix),
            "{name}'s parameter zero must stay borrowed, not consumed:\n{section}"
        );
        assert!(
            section.contains(own_marker),
            "{name} must mint its own independent owner from the borrowed parameter:\n{section}"
        );
    }
    let main = sir_function_section(&sir, "main");
    for call in [
        "call @__hew_fn_Ops::storeRecord(borrow",
        "call @__hew_fn_Ops::storeTuple(borrow",
        "call @__hew_fn_Ops::storeEnum(borrow",
    ] {
        assert!(
            main.contains(call),
            "each associated-fn call must pass its carrier by borrow, keeping the caller's \
             copy live for the post-call reads:\n{main}"
        );
    }
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
        let cancel = llvm_basic_block(inspect, "cancel_exit");
        let guarded_drop_label = cancel
            .lines()
            .find(|line| line.contains("br i1 %carrier_drop_live"))
            .and_then(|line| line.split("label %").nth(1))
            .and_then(|targets| targets.split(',').next())
            .expect("cancellation block must branch to the live-carrier cleanup");
        let guarded_drop = llvm_basic_block(inspect, guarded_drop_label);
        assert_eq!(
            guarded_drop
                .matches("call void @__hew_enum_drop_inplace_Mixed(")
                .count(),
            1,
            "runtime cancellation code 2 must release the live prepared enum carrier exactly once ({target_name}):\n{cancel}\n{guarded_drop}"
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

#[test]
fn repeated_string_parameter_reads_stay_on_the_borrow_spine() {
    // `snapshot_clone`/`neutralize_payload` named the retired checked-MIR
    // caller-side copy-in mechanism. The surviving fact: a whole-string
    // parameter never gets copied or consumed just to be read twice — both
    // calls pass the same borrowed parameter through, with no `copy_value`
    // of it at all (the CoW borrow spine owns string sharing, not a
    // per-read owner mint).
    let sir = dump_sir(
        REPEATED_OWNED_PARAM_READ_SOURCE,
        "repeated_owned_param_reads",
    );
    let inspect_twice = sir_function_section(&sir, "inspectTwice");
    assert!(
        inspect_twice.contains("%0: string guaranteed"),
        "inspectTwice's key parameter must stay borrowed, not consumed:\n{inspect_twice}"
    );
    assert_eq!(
        inspect_twice.matches("copy_value %0").count(),
        0,
        "a whole-string param never registers as an owned call-carrier: each live \
         read passes the caller-owned parameter raw, with no independent owner \
         minted just to read it:\n{inspect_twice}"
    );
    assert_eq!(
        inspect_twice
            .matches("call @__hew_fn_borrowThenFree(borrow %0)")
            .count(),
        2,
        "both reads must pass the same borrowed parameter through, never a moved \
         or cleared copy:\n{inspect_twice}"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn repeated_owned_parameter_reads_survive_native_cleanup() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("repeated-owned-param-reads-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        REPEATED_OWNED_PARAM_READ_SOURCE,
        dir.path(),
        "repeated_owned_param_reads",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "two carrier calls must not clone a neutralized string parameter:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "15|CARRIER-GUARD",
        "the original caller and both helper calls must observe the same live string:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrowed_receiver_forward_releases_each_owner_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("borrowed-receiver-forward-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        BORROWED_RECEIVER_FORWARD_SOURCE,
        dir.path(),
        "borrowed_receiver_forward",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "a borrowed method receiver forwarded into an owning callee must be \
         snapshot-cloned, never last-use transferred — the receiver's caller \
         still drops the original fields:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "i:-alpha|keep",
        "the caller's receiver must stay intact after the method returns:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn template_if_range_survives_owned_key_carrier_reuse() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("template-if-range-carrier-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        TEMPLATE_IF_RANGE_SOURCE,
        dir.path(),
        "template_oracle_03_if_range",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "template range rendering must not clone a neutralized lookup key:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "[red][blue]\n",
        "the nested if/range oracle must render both list elements:\n{}",
        describe_output(&output)
    );
}
