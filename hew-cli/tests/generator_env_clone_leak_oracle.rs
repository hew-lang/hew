//! Standalone memory-safety oracle for owned generator environment snapshots.

#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::Command;

use support::leak_slope::{
    compile_to_native, leaks_supported, measure_leaks, run_under_malloc_scribble,
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
    if !leaks_supported(shape) {
        return;
    }
    let Some(leaks) = measure_leaks(bin) else {
        return;
    };
    assert_eq!(leaks, 0, "{shape} leaked {leaks} allocation(s)");
}

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
