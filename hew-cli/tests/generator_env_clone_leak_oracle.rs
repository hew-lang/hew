//! Standalone memory-safety oracle for owned generator environment snapshots.

#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, measure_leaks, require_leaks_tool,
    run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn with_frames(template: &str, frames: usize) -> String {
    template.replace("__FRAMES__", &frames.to_string())
}

const DIRECT_AND_ANONYMOUS: &str = r#"
type Capture {
    label: string,
    values: Vec<i64>,
    root: Rc<i64>,
    edge: Weak<i64>,
}

fn upgraded(edge: Weak<i64>) -> i64 {
    match edge.upgrade() {
        Some(value) => value.get(),
        None => -1,
    }
}

gen fn rows(c: Capture) -> i64 {
    yield c.label.len() + c.values[0] + c.root.get();
    yield upgraded(c.edge);
}

fn complete(root: Rc<i64>, i: i64) {
    let c = Capture {
        label: "cap-" + f"{i}",
        values: [3, 4],
        root: root.clone(),
        edge: root.downgrade(),
    };
    let g = rows(c);
    if c.root.strong_count() != 3 || c.root.weak_count() != 2 {
        panic("complete live counts");
    }
    var seen: i64 = 0;
    loop {
        match g.next() {
            Some(value) => { seen = seen + value; },
            None => { break; },
        }
    }
    if seen <= 0 { panic("complete values"); }
}

fn suspend(root: Rc<i64>, i: i64) {
    let c = Capture {
        label: "cap-" + f"{i}",
        values: [3, 4],
        root: root.clone(),
        edge: root.downgrade(),
    };
    let g = rows(c);
    if c.root.strong_count() != 3 || c.root.weak_count() != 2 {
        panic("suspend live counts");
    }
    match g.next() {
        Some(value) => { if value <= 0 { panic("suspend value"); } },
        None => panic("suspend missing value"),
    }
}

fn never_resume(root: Rc<i64>, i: i64) {
    let c = Capture {
        label: "cap-" + f"{i}",
        values: [3, 4],
        root: root.clone(),
        edge: root.downgrade(),
    };
    let _g = rows(c);
    if c.root.strong_count() != 3 || c.root.weak_count() != 2 {
        panic("never live counts");
    }
}

fn anonymous(root: Rc<i64>, i: i64) {
    let c = Capture {
        label: "cap-" + f"{i}",
        values: [3, 4],
        root: root.clone(),
        edge: root.downgrade(),
    };
    let g = gen {
        yield c.label.len() + c.values[1] + c.root.get();
    };
    if c.root.strong_count() != 3 || c.root.weak_count() != 2 {
        panic("anonymous live counts");
    }
    loop {
        match g.next() {
            Some(value) => { if value <= 0 { panic("anonymous value"); } },
            None => { break; },
        }
    }
}

fn main() {
    var completed: i64 = 0;
    var suspended: i64 = 0;
    var never_resumed: i64 = 0;
    var anonymous: i64 = 0;
    var i: i64 = 0;
    while i < __FRAMES__ {
        let root = Rc::new(i + 10);
        complete(root, i);
        if root.strong_count() != 1 || root.weak_count() != 0 {
            panic("complete release counts");
        }
        completed = completed + 1;

        suspend(root, i);
        if root.strong_count() != 1 || root.weak_count() != 0 {
            panic("suspend release counts");
        }
        suspended = suspended + 1;

        never_resume(root, i);
        if root.strong_count() != 1 || root.weak_count() != 0 {
            panic("never release counts");
        }
        never_resumed = never_resumed + 1;

        anonymous(root, i);
        if root.strong_count() != 1 || root.weak_count() != 0 {
            panic("anonymous release counts");
        }
        anonymous = anonymous + 1;
        i = i + 1;
    }
    print(f"{completed}:{suspended}:{never_resumed}:{anonymous}:OK");
}
"#;

const CLOSURE_SOURCE: &str = r#"
type Capture {
    label: string,
    values: Vec<i64>,
    root: Rc<i64>,
    edge: Weak<i64>,
}

fn upgraded(edge: Weak<i64>) -> i64 {
    match edge.upgrade() {
        Some(value) => value.get(),
        None => -1,
    }
}

fn factory(c: Capture) -> fn() -> Generator<i64, ()> {
    || {
        let captured = c;
        gen {
            yield captured.label.len() + captured.values[0] + captured.root.get();
            yield upgraded(captured.edge);
        }
    }
}

fn closure_complete(root: Rc<i64>, i: i64) {
    let capture = Capture {
        label: "closure-" + f"{i}",
        values: [5, 6],
        root: root.clone(),
        edge: root.downgrade(),
    };
    let make = factory(capture);
    let g = make();
    if root.strong_count() != 3 || root.weak_count() != 2 {
        panic("closure complete live counts");
    }
    loop {
        match g.next() {
            Some(value) => { if value <= 0 { panic("closure complete value"); } },
            None => { break; },
        }
    }
}

fn closure_suspend(root: Rc<i64>, i: i64) {
    let capture = Capture {
        label: "closure-" + f"{i}",
        values: [5, 6],
        root: root.clone(),
        edge: root.downgrade(),
    };
    let make = factory(capture);
    let g = make();
    if root.strong_count() != 3 || root.weak_count() != 2 {
        panic("closure suspend live counts");
    }
    match g.next() {
        Some(value) => { if value <= 0 { panic("closure suspend value"); } },
        None => panic("closure suspend missing value"),
    }
}

fn closure_never(root: Rc<i64>, i: i64) {
    let capture = Capture {
        label: "closure-" + f"{i}",
        values: [5, 6],
        root: root.clone(),
        edge: root.downgrade(),
    };
    let make = factory(capture);
    let _g = make();
    if root.strong_count() != 3 || root.weak_count() != 2 {
        panic("closure never live counts");
    }
}

fn closure_repeat(root: Rc<i64>, i: i64) {
    let capture = Capture {
        label: "closure-" + f"{i}",
        values: [5, 6],
        root: root.clone(),
        edge: root.downgrade(),
    };
    let make = factory(capture);
    var calls: i64 = 0;
    while calls < 2 {
        let g = make();
        loop {
            match g.next() {
                Some(value) => { if value <= 0 { panic("closure repeat value"); } },
                None => { break; },
            }
        }
        calls = calls + 1;
    }
}

fn main() {
    var completed: i64 = 0;
    var suspended: i64 = 0;
    var never_resumed: i64 = 0;
    var repeated: i64 = 0;
    var i: i64 = 0;
    while i < __FRAMES__ {
        let root = Rc::new(i + 20);
        closure_complete(root, i);
        if root.strong_count() != 1 || root.weak_count() != 0 {
            panic("closure complete release counts");
        }
        completed = completed + 1;
        closure_suspend(root, i);
        if root.strong_count() != 1 || root.weak_count() != 0 {
            panic("closure suspend release counts");
        }
        suspended = suspended + 1;
        closure_never(root, i);
        if root.strong_count() != 1 || root.weak_count() != 0 {
            panic("closure never release counts");
        }
        never_resumed = never_resumed + 1;
        closure_repeat(root, i);
        if root.strong_count() != 1 || root.weak_count() != 0 {
            panic("closure repeat release counts");
        }
        repeated = repeated + 1;
        i = i + 1;
    }
    print(f"{completed}:{suspended}:{never_resumed}:{repeated}:OK");
}
"#;

const CLOSURE_COUNTERFACTUALS: &str = r#"
type Label {
    text: string,
}

fn ordinary_factory(label: Label) -> fn() -> i64 {
    || label.text.len()
}

fn noncapturing_generator_factory() -> fn() -> Generator<i64, ()> {
    || {
        gen {
            yield 1;
            yield 2;
        }
    }
}

fn ordinary_capture(i: i64) {
    let label = Label { text: "ordinary-" + f"{i}" };
    let read = ordinary_factory(label);
    if read() <= 0 {
        panic("ordinary capture");
    }
}

fn noncapturing_generator() {
    let make = noncapturing_generator_factory();
    let g = make();
    var total: i64 = 0;
    loop {
        match g.next() {
            Some(value) => { total = total + value; },
            None => { break; },
        }
    }
    if total != 3 {
        panic("noncapturing generator");
    }
}

fn main() {
    var ordinary: i64 = 0;
    var noncapturing: i64 = 0;
    var i: i64 = 0;
    while i < __FRAMES__ {
        ordinary_capture(i);
        ordinary = ordinary + 1;
        noncapturing_generator();
        noncapturing = noncapturing + 1;
        i = i + 1;
    }
    print(f"{ordinary}:{noncapturing}:OK");
}
"#;

const BITCOPY_CONTROL: &str = r#"
type Capture {
    label_len: i64,
    first: i64,
    root: i64,
    edge: i64,
}

gen fn rows(c: Capture) -> i64 {
    yield c.label_len + c.first + c.root;
    yield c.edge;
}

fn main() {
    var completed: i64 = 0;
    var suspended: i64 = 0;
    var never_resumed: i64 = 0;
    var anonymous: i64 = 0;
    var i: i64 = 0;
    while i < __FRAMES__ {
        let capture = Capture { label_len: 5, first: 3, root: i + 10, edge: i + 10 };
        {
            let g = rows(capture);
            loop {
                match g.next() {
                    Some(value) => { if value <= 0 { panic("control complete"); } },
                    None => { break; },
                }
            }
        }
        completed = completed + 1;
        {
            let g = rows(capture);
            match g.next() {
                Some(value) => { if value <= 0 { panic("control suspend"); } },
                None => panic("control missing"),
            }
        }
        suspended = suspended + 1;
        { let _g = rows(capture); }
        never_resumed = never_resumed + 1;
        {
            let g = gen { yield capture.root; };
            loop {
                match g.next() {
                    Some(value) => { if value <= 0 { panic("control anon"); } },
                    None => { break; },
                }
            }
        }
        anonymous = anonymous + 1;
        i = i + 1;
    }
    print(f"{completed}:{suspended}:{never_resumed}:{anonymous}:OK");
}
"#;

const ACTOR_STATE_SOURCE: &str = r#"
record Config { label: string }

actor Reader {
    var name: string;
    receive gen fn stream() -> i64 {
        yield name.len();
        yield name.len() + 1;
    }
}

supervisor App(config: Config) {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child reader: Reader(name: config.label);
}

fn main() {
    let config = Config { label: "actor-state" };
    let app = spawn App(config: config);
    let reader = app.reader;
    var seen: i64 = 0;
    var i: i64 = 0;
    while i < __FRAMES__ {
        for await value in reader.stream() {
            if value <= 0 { panic("actor state value"); }
            seen = seen + 1;
        }
        i = i + 1;
    }
    supervisor_stop(app);
    print(f"{seen}:OK");
}
"#;

fn actor_param_source(frames: usize) -> String {
    r#"
actor Streamer {
    receive gen fn emit(label: string, n: i64) -> i64 {
        var i: i64 = 0;
        while i < n {
            yield i + label.len();
            i = i + 1;
        }
    }
}

fn main() {
    let streamer = spawn Streamer();
    var total: i64 = 0;
    var frame: i64 = 0;
    while frame < __FRAMES__ {
        for await value in streamer.emit("streamlabel".to_upper(), 3) {
            total = total + value;
        }
        frame = frame + 1;
    }
    print(f"{frame}:{total}:OK");
}
"#
    .replace("__FRAMES__", &frames.to_string())
}

fn actor_record_param_source(frames: usize) -> String {
    r#"
record Payload {
    label: string,
    data: bytes,
    pair: (string, i64),
}

indirect enum Tree {
    Leaf(i64);
    Node(Tree, Tree);
}

actor Streamer {
    var state: string;
    receive gen fn emit(payload: Payload, tree: Tree, n: i64) -> i64 {
        var i: i64 = 0;
        if n > 0 {
            yield payload.label.len() + payload.data.len()
                + payload.pair.0.len() + state.len();
            i = 1;
        }
        let tree_kind = match tree {
            Leaf(_) => 1,
            Node(_, _) => 2,
        };
        while i < n {
            yield payload.label.len() + payload.data.len()
                + payload.pair.0.len() + state.len() + i + tree_kind;
            i = i + 1;
        }
    }
    receive gen fn endless(payload: Payload, tree: Tree) -> i64 {
        yield payload.label.len() + payload.data.len()
            + payload.pair.0.len() + state.len();
        let tree_kind = match tree {
            Leaf(_) => 1,
            Node(_, _) => 2,
        };
        var i: i64 = 1;
        loop {
            yield payload.label.len() + payload.data.len()
                + payload.pair.0.len() + state.len() + i + tree_kind;
            i = i + 1;
        }
    }
}

fn main() {
    let streamer = spawn Streamer(state: "actor-state".to_upper());
    var total: i64 = 0;
    var frame: i64 = 0;
    while frame < __FRAMES__ {
        let payload = Payload {
            label: "record-param".to_upper(),
            data: "bytes-param".to_bytes(),
            pair: ("tuple-param".to_upper(), frame),
        };
        let tree = Node(Leaf(frame), Leaf(frame + 1));
        for await value in streamer.emit(payload, tree, 3) {
            total = total + value;
        }
        let cancelled = Payload {
            label: "record-param".to_upper(),
            data: "bytes-param".to_bytes(),
            pair: ("tuple-param".to_upper(), frame),
        };
        let cancelled_tree = Node(Leaf(frame), Leaf(frame + 1));
        for await value in streamer.endless(cancelled, cancelled_tree) {
            total = total + value;
            break;
        }
        frame = frame + 1;
    }
    print(f"{frame}:{total}:OK");
}
"#
    .replace("__FRAMES__", &frames.to_string())
}

const ACTOR_RECORD_PARAM_SUSPEND: &str = r#"
record Payload {
    label: string,
    data: bytes,
    pair: (string, i64),
}

indirect enum Tree {
    Leaf(i64);
    Node(Tree, Tree);
}

actor Streamer {
    var state: string;
    receive gen fn endless(payload: Payload, tree: Tree) -> i64 {
        yield payload.label.len() + payload.data.len()
            + payload.pair.0.len() + state.len();
        let tree_kind = match tree {
            Leaf(_) => 1,
            Node(_, _) => 2,
        };
        var i: i64 = 1;
        loop {
            yield payload.label.len() + payload.data.len()
                + payload.pair.0.len() + state.len() + i + tree_kind;
            i = i + 1;
        }
    }
}

record AppConfig { label: string }

supervisor App(config: AppConfig) {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child streamer: Streamer(state: config.label);
}

fn main() {
    let config = AppConfig { label: "actor-state".to_upper() };
    let sup = spawn App(config: config);
    let streamer = sup.streamer;
    let payload = Payload {
        label: "record-param".to_upper(),
        data: "bytes-param".to_bytes(),
        pair: ("tuple-param".to_upper(), 0),
    };
    let tree = Node(Leaf(0), Leaf(1));
    let stream = streamer.endless(payload, tree);
    let first = await stream.recv();
    match first {
        Some(value) => print(f"{value}:"),
        None => print("missing:"),
    }
    supervisor_stop(sup);
    print("OK");
}
"#;

fn run_exact(bin: &Path, expected: &str) {
    let output = Command::new(bin).output().expect("run compiled oracle");
    assert!(
        output.status.success(),
        "compiled generator env oracle failed:\n{}",
        describe_output(&output)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
}

fn assert_zero_leaks(bin: &Path, shape: &str) {
    require_leaks_tool();
    let leaks = measure_leaks(bin);
    assert_eq!(leaks, 0, "{shape} leaked {leaks} allocation(s)");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn generator_env_clone_direct_and_control_have_zero_leaks() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("generator-env-clone-direct-")
        .tempdir()
        .expect("tempdir");
    let owned = compile_to_native(
        &with_frames(DIRECT_AND_ANONYMOUS, 128),
        dir.path(),
        "generator_env_clone_owned",
    );
    let control = compile_to_native(
        &with_frames(BITCOPY_CONTROL, 128),
        dir.path(),
        "generator_env_clone_control",
    );
    run_exact(&owned, "128:128:128:128:OK");
    run_exact(&control, "128:128:128:128:OK");
    assert_zero_leaks(&owned, "generator-env-clone-owned");
    assert_zero_leaks(&control, "generator-env-clone-control");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn generator_closure_env_clone_has_zero_leaks() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("generator-env-clone-closure-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &with_frames(CLOSURE_SOURCE, 128),
        dir.path(),
        "generator_closure_env_clone",
    );
    run_exact(&bin, "128:128:128:128:OK");
    assert_zero_leaks(&bin, "generator-closure-env-clone");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn generator_closure_counterfactuals_have_zero_leaks() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("generator-env-clone-counterfactuals-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &with_frames(CLOSURE_COUNTERFACTUALS, 128),
        dir.path(),
        "generator_closure_counterfactuals",
    );
    run_exact(&bin, "128:128:OK");
    assert_zero_leaks(&bin, "generator-closure-counterfactuals");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn generator_actor_state_env_clone_has_zero_leaks() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("generator-env-clone-actor-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &with_frames(ACTOR_STATE_SOURCE, 16),
        dir.path(),
        "generator_actor_state_env_clone",
    );
    run_exact(&bin, "32:OK");
    assert_zero_leaks(&bin, "generator-actor-state-env-clone");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn generator_actor_param_transfer_has_zero_leak_slope() {
    assert_frame_slope_below_tolerance("generator_actor_param_transfer", actor_param_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn generator_actor_record_param_transfer_has_zero_leak_slope() {
    assert_frame_slope_below_tolerance(
        "generator_actor_record_param_transfer",
        actor_record_param_source,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn generator_actor_record_param_suspended_teardown_has_zero_leaks() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("generator-env-record-param-suspend-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        ACTOR_RECORD_PARAM_SUSPEND,
        dir.path(),
        "generator_actor_record_param_suspend",
    );
    run_exact(&bin, "45:OK");
    assert_zero_leaks(&bin, "generator-actor-record-param-suspend");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn generator_and_closure_env_clone_are_malloc_scribble_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("generator-env-clone-scribble-")
        .tempdir()
        .expect("tempdir");
    for (name, source, expected) in [
        (
            "generator_env_clone_scribble",
            with_frames(DIRECT_AND_ANONYMOUS, 8),
            "8:8:8:8:OK",
        ),
        (
            "generator_closure_env_clone_scribble",
            with_frames(CLOSURE_SOURCE, 8),
            "8:8:8:8:OK",
        ),
        (
            "generator_actor_param_transfer_scribble",
            actor_param_source(8),
            "8:288:OK",
        ),
        (
            "generator_actor_record_param_transfer_scribble",
            actor_record_param_source(8),
            "8:1496:OK",
        ),
        (
            "generator_actor_record_param_suspend_scribble",
            ACTOR_RECORD_PARAM_SUSPEND.to_string(),
            "45:OK",
        ),
    ] {
        let bin = compile_to_native(&source, dir.path(), name);
        let output = run_under_malloc_scribble(&bin);
        assert!(
            output.status.success(),
            "{name} failed under MallocScribble:\n{}",
            describe_output(&output)
        );
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
}
