//! Retain-backed string parameter embeds in anonymous Vec COPY-IN source temps.
//!
//! A whole by-value parameter remains caller-owned. Aggregate lowering retains
//! an embedded `string`, however, so the anonymous source temp owns one separate
//! share that must be dropped after `hew_vec_{push,set}_owned` clones it into the
//! Vec. These oracles pin flat leak slopes and prove that the caller's parameter
//! remains readable and naturally droppable under the poisoned allocator.

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

const NON_STRING_PARAM_SOURCE: &str = r"
type Holder { items: Vec<string> }
type Wrap { f: Option<Holder> }

fn pushParam(p: Holder) {
    let v: Vec<Wrap> = [];
    v.push(Wrap { f: Some(p) });
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

fn arena_source(frames: usize) -> String {
    with_frames(ARENA_TEMPLATE, frames)
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
    assert_frame_slope_below_tolerance("vec_param_embed_push", push_source);
}

#[test]
fn direct_set_param_embed_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("vec_param_embed_set", set_source);
}

#[test]
fn bound_first_copy_in_control_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("vec_param_embed_bound_first", bound_first_source);
}

#[test]
fn no_param_embed_move_control_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("vec_param_embed_move", move_source);
}

#[test]
fn arena_insert_remove_param_embed_has_flat_leak_slope() {
    // `arena_source(100)` is the explicit diagnostic form that reproduces the
    // historical 100-node/3200-byte signature with the mint removed.
    assert_frame_slope_below_tolerance("vec_param_embed_arena", arena_source);
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
fn non_string_param_embed_never_mints_a_temp_owner() {
    let mir = dump_checked_mir(NON_STRING_PARAM_SOURCE, "non_string_param_embed");
    assert!(mir.contains("call hew_vec_push_owned("));
    assert!(!mir.contains("hew_vec_push_owned_move"));
    assert!(
        !mir.contains("__hew_copy_in_param_temp"),
        "an unretained Holder parameter is a borrowed alias and must never gain a temp owner:\n{mir}"
    );
}
