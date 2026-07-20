//! Compiled memory-safety oracle for `Rc<T>` graphs with `Weak<T>` back-edges.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn cycle_source(frames: usize) -> String {
    format!(
        r#"
type Node {{
    label: string,
    parent: Option<Weak<Node>>,
}}

fn build_dead_graph() -> Weak<Node> {{
    let root = Rc::new(Node {{ label: "r" + "oot", parent: None }});
    if root.strong_count() != 1 {{ panic("initial strong count"); }}
    if root.weak_count() != 0 {{ panic("initial weak count"); }}

    let weak = root.downgrade();
    if root.weak_count() != 1 {{ panic("downgrade count"); }}

    root.set(Node {{ label: "b" + "ack", parent: Some(weak.clone()) }});
    if root.weak_count() != 2 {{ panic("first back-edge count"); }}

    root.set(Node {{ label: "r" + "eplace", parent: Some(weak.clone()) }});
    if root.weak_count() != 2 {{ panic("replacement release count"); }}

    match weak.upgrade() {{
        Some(alias) => {{
            if alias.strong_count() != 2 {{ panic("upgrade strong count"); }}
        }},
        None => panic("live upgrade returned None"),
    }}
    if root.strong_count() != 1 {{ panic("upgraded owner was not released"); }}

    root.set(Node {{ label: "f" + "inal", parent: None }});
    if root.weak_count() != 1 {{ panic("displaced back-edge was not released"); }}
    weak
}}

fn main() {{
    var dead: i64 = 0;
    for _ in 0..{frames} {{
        let weak = build_dead_graph();
        match weak.upgrade() {{
            Some(_) => panic("dead upgrade returned Some"),
            None => {{
                dead = dead + 1;
            }},
        }}
    }}
    print(dead);
    print("OK");
}}
"#
    )
}

#[test]
fn weak_back_edge_replacement_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("rc-weak-cycle", cycle_source);
}

#[test]
fn weak_back_edge_replacement_is_scribble_clean_with_exact_output() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("rc-weak-cycle-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&cycle_source(8), dir.path(), "rc_weak_cycle_scribble");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "Rc/Weak cycle replacement must not leak, double-free, or read freed storage:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "8OK",
        "every dead allocation must upgrade to exactly None:\n{}",
        describe_output(&output)
    );
}
