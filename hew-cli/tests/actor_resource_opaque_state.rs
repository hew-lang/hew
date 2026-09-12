#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

fn hew_string_literal(path: &Path) -> String {
    path.to_string_lossy()
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
}

fn run_teardown_close_oracle(name: &str, actor_decl: &str, spawn_expr: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("actor-resource-state-{name}-"))
        .tempdir()
        .expect("tempdir");
    let marker = dir.path().join("closed.txt");
    let source_path = dir.path().join(format!("{name}.hew"));
    let marker_literal = hew_string_literal(&marker);
    let source = format!(
        r#"import std.fs;
import std.testing;

#[resource]
#[opaque]
type Dq {{}}

impl Dq {{
    fn close(consume self) {{
        unsafe {{ hew_deque_free(self) }};
        match fs.append("{marker_literal}", "closed\n") {{
            .Ok(_) => {{}},
            .Err(_) => panic("append close marker"),
        }}
    }}
}}

extern "C" {{
    fn hew_deque_new() -> Dq;
    fn hew_deque_free(consume dq: Dq);
}}

type Holder {{
    dq: Dq
}}

{actor_decl}

#[test]
fn actor_resource_state_closes_once() {{
    let keeper = {spawn_expr};
    match keeper.ping() {{
        .Ok(n) => testing.assert_eq(n, 1),
        .Err(_) => testing.assert_true(false),
    }}
}}
"#
    );
    std::fs::write(&source_path, source).expect("write Hew source");

    let output = Command::new(hew_binary())
        .args([
            "test",
            "--no-color",
            source_path.to_str().expect("source path utf-8"),
        ])
        .current_dir(repo_root())
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run Hew test");
    assert!(
        output.status.success(),
        "{name}: actor resource-state Hew test must compile and run cleanly;\n{}",
        describe_output(&output)
    );

    let closes = std::fs::read_to_string(&marker)
        .unwrap_or_else(|error| panic!("{name}: close marker was not written: {error}"));
    assert_eq!(
        closes, "closed\n",
        "{name}: actor teardown must call the resource close exactly once",
    );
}

fn run_builtin_name_collision_teardown_oracle(type_name: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("actor-resource-state-collision-{type_name}-"))
        .tempdir()
        .expect("tempdir");
    let marker = dir.path().join("closed.txt");
    let source_path = dir.path().join(format!("{type_name}.hew"));
    let marker_literal = hew_string_literal(&marker);
    let source = format!(
        r#"import std.fs;
import std.testing;

#[resource]
#[opaque]
type {type_name} {{}}

impl {type_name} {{
    fn close(consume self) {{
        unsafe {{ hew_deque_free(self) }};
        match fs.append("{marker_literal}", "closed\n") {{
            .Ok(_) => {{}},
            .Err(_) => panic("append close marker"),
        }}
    }}
}}

extern "C" {{
    fn hew_deque_new() -> {type_name};
    fn hew_deque_free(consume handle: {type_name});
}}

actor Keeper {{
    let handle: {type_name},
    receive fn ping() -> i64 {{ 1 }}
}}

#[test]
fn colliding_resource_closes_once() {{
    let keeper = spawn Keeper(handle: unsafe {{ hew_deque_new() }});
    match keeper.ping() {{
        .Ok(n) => testing.assert_eq(n, 1),
        .Err(_) => testing.assert_true(false),
    }}
}}
"#
    );
    std::fs::write(&source_path, source).expect("write Hew source");

    let output = Command::new(hew_binary())
        .args([
            "test",
            "--no-color",
            source_path.to_str().expect("source path utf-8"),
        ])
        .current_dir(repo_root())
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run Hew test");
    assert!(
        output.status.success(),
        "{type_name}: builtin-named user resource must compile and run as a user close;\n{}",
        describe_output(&output)
    );

    let closes = std::fs::read_to_string(&marker)
        .unwrap_or_else(|error| panic!("{type_name}: close marker was not written: {error}"));
    assert_eq!(
        closes, "closed\n",
        "{type_name}: actor teardown must call the user close exactly once",
    );
}

fn run_imported_receiver_collision_teardown_oracle(package_import: bool) {
    require_codegen();

    let mode = if package_import { "package" } else { "file" };
    let dir = tempfile::Builder::new()
        .prefix(&format!("actor-resource-imported-receiver-{mode}-"))
        .tempdir()
        .expect("tempdir");
    let marker = dir.path().join("closed.txt");
    let marker_literal = hew_string_literal(&marker);
    let module_source = format!(
        r#"import std.fs;

#[resource]
#[opaque]
pub type UserReceiver {{}}

impl UserReceiver {{
    fn close(consume self) {{
        unsafe {{ hew_deque_free(self) }};
        match fs.append("{marker_literal}", "closed\n") {{
            .Ok(_) => {{}},
            .Err(_) => panic("append close marker"),
        }}
    }}
}}

pub actor Keeper {{
    let handle: UserReceiver = unsafe {{ hew_deque_new() }},
    receive fn ping() -> i64 {{ 1 }}
}}

extern "C" {{
    fn hew_deque_new() -> UserReceiver;
    fn hew_deque_free(consume handle: UserReceiver);
}}
"#
    );
    let (import, actor) = if package_import {
        let pkg = dir.path().join("hew/foo");
        std::fs::create_dir_all(&pkg).expect("create package");
        std::fs::write(
            pkg.join("hew.toml"),
            "[package]\nname = \"hew::foo\"\nversion = \"0.1.0\"\n",
        )
        .expect("write package manifest");
        std::fs::write(pkg.join("foo.hew"), module_source).expect("write package source");
        ("import hew.foo;", "foo.Keeper")
    } else {
        std::fs::write(dir.path().join("foo.hew"), module_source).expect("write file import");
        ("import foo;", "foo.Keeper")
    };
    let source_path = dir.path().join("main.hew");
    let source = format!(
        r#"{import}

fn main() {{
    let keeper = spawn {actor}();
    match keeper.ping() {{
        .Ok(n) => if n != 1 {{ panic("wrong reply") }},
        .Err(_) => panic("ask failed"),
    }}
}}
"#
    );
    std::fs::write(&source_path, source).expect("write root source");

    let output = Command::new(hew_binary())
        .args(["run", source_path.to_str().expect("source path utf-8")])
        .current_dir(repo_root())
        .output()
        .expect("run imported collision test");
    assert!(
        output.status.success(),
        "{mode}-imported user UserReceiver must compile and run with user teardown;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        std::fs::read_to_string(&marker).expect("close marker"),
        "closed\n",
        "{mode}-imported UserReceiver must close exactly once"
    );
}

#[test]
fn direct_resource_actor_state_closes_once_on_teardown() {
    run_teardown_close_oracle(
        "direct",
        r"actor Keeper {
    let dq: Dq,
    receive fn ping() -> i64 { 1 }
}",
        "spawn Keeper(dq: unsafe { hew_deque_new() })",
    );
}

#[test]
fn wrapped_resource_actor_state_still_closes_once_on_teardown() {
    run_teardown_close_oracle(
        "wrapped",
        r"actor Keeper {
    let holder: Holder,
    receive fn ping() -> i64 { 1 }
}",
        "spawn Keeper(holder: Holder { dq: unsafe { hew_deque_new() } })",
    );
}

#[test]
fn user_receiver_resource_shadow_closes_once_on_teardown() {
    run_builtin_name_collision_teardown_oracle("UserReceiver");
}

#[test]
fn user_monitor_ref_resource_closes_once_on_teardown() {
    run_builtin_name_collision_teardown_oracle("UserMonitorRef");
}

#[test]
fn package_imported_user_receiver_closes_once_without_runtime_close() {
    run_imported_receiver_collision_teardown_oracle(true);
}

#[test]
fn file_imported_user_receiver_closes_once_without_runtime_close() {
    run_imported_receiver_collision_teardown_oracle(false);
}
