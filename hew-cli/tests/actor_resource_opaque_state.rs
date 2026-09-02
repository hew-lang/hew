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
    fn close(self) {{
        unsafe {{ hew_deque_free(self) }};
        match fs.try_append("{marker_literal}", "closed\n") {{
            Ok(_) => {{}},
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
    match await keeper.ping() {{
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
    fn close(self) {{
        unsafe {{ hew_deque_free(self) }};
        match fs.try_append("{marker_literal}", "closed\n") {{
            Ok(_) => {{}},
            .Err(_) => panic("append close marker"),
        }}
    }}
}}

extern "C" {{
    fn hew_deque_new() -> {type_name};
    fn hew_deque_free(consume handle: {type_name});
}}

actor Keeper {{
    let handle: {type_name};
    receive fn ping() -> i64 {{ 1 }}
}}

#[test]
fn colliding_resource_closes_once() {{
    let keeper = spawn Keeper(handle: unsafe {{ hew_deque_new() }});
    match await keeper.ping() {{
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
    fn close(self) {{
        unsafe {{ hew_deque_free(self) }};
        match fs.try_append("{marker_literal}", "closed\n") {{
            Ok(_) => {{}},
            .Err(_) => panic("append close marker"),
        }}
    }}
}}

pub actor Keeper {{
    let handle: UserReceiver = unsafe {{ hew_deque_new() }};
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
        ("import \"foo.hew\";", "Keeper")
    };
    let source_path = dir.path().join("main.hew");
    let source = format!(
        r#"{import}

fn main() {{
    let keeper = spawn {actor}();
    match await keeper.ping() {{
        Ok(n) => if n != 1 {{ panic("wrong reply") }},
        .Err(_) => panic("ask failed"),
    }}
}}
"#
    );
    std::fs::write(&source_path, source).expect("write root source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-llvm",
            "--emit-dir",
            dir.path().to_str().expect("emit dir utf-8"),
            source_path.to_str().expect("source path utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("compile imported collision IR");
    assert!(
        output.status.success(),
        "{mode}-imported UserReceiver IR must compile;\n{}",
        describe_output(&output)
    );
    let ll = std::fs::read_to_string(dir.path().join("main.ll")).expect("read emitted IR");
    let drop_body = fn_body(&ll, "__hew_state_drop_");
    assert!(
        drop_body.contains("UserReceiver::close"),
        "{mode}-imported state drop must call the authored UserReceiver close:\n{drop_body}"
    );
    assert!(
        !drop_body.contains("hew_channel_receiver_close"),
        "{mode}-imported user UserReceiver must not route to runtime channel close:\n{drop_body}"
    );

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

fn fn_body<'a>(ll: &'a str, symbol: &str) -> &'a str {
    let start = ll
        .match_indices("define ")
        .find_map(|(start, _)| {
            ll[start..]
                .lines()
                .next()?
                .contains(symbol)
                .then_some(start)
        })
        .unwrap_or_else(|| panic!("missing function definition containing `{symbol}`"));
    let body = &ll[start..];
    let end = body.find("\n}").expect("function body terminator");
    &body[..=end + 1]
}

#[test]
fn direct_resource_actor_state_closes_once_on_teardown() {
    run_teardown_close_oracle(
        "direct",
        r"actor Keeper {
    let dq: Dq;
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
    let holder: Holder;
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

#[test]
fn builtin_cancellation_token_actor_state_uses_runtime_release() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("actor-builtin-cancellation-token-ir-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join("builtin_token.hew");
    let source = r#"extern "C" {
    fn hew_deque_new() -> CancellationToken;
}

actor Keeper {
    let token: CancellationToken;
}

fn main() {
    let token = unsafe { hew_deque_new() };
    let _keeper = spawn Keeper(token: token);
}
"#;
    std::fs::write(&source_path, source).expect("write builtin token source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-llvm",
            "--emit-dir",
            dir.path().to_str().expect("emit dir utf-8"),
            source_path.to_str().expect("source path utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("compile builtin token IR");
    assert!(
        output.status.success(),
        "genuine builtin CancellationToken actor state must compile;\n{}",
        describe_output(&output)
    );
    let ll = std::fs::read_to_string(dir.path().join("builtin_token.ll")).expect("read emitted IR");
    let drop_body = fn_body(&ll, "__hew_state_drop_");
    assert!(
        drop_body.contains("hew_cancel_token_release"),
        "genuine builtin CancellationToken must retain runtime release lowering:\n{drop_body}"
    );
    assert!(
        !drop_body.contains("CancellationToken::close"),
        "genuine builtin CancellationToken must not acquire a user close:\n{drop_body}"
    );
}

#[test]
fn direct_resource_actor_state_uses_restart_clone_refusal_and_single_close_drop() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("actor-resource-state-ir-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join("resource_actor.hew");
    let source = r#"#[resource]
#[opaque]
type Dq {}

impl Dq {
    fn close(self) {
        unsafe { hew_deque_free(self) };
    }
}

extern "C" {
    fn hew_deque_free(consume dq: Dq);
}

type Holder {
    dq: Dq
}

actor Direct {
    let dq: Dq;
}

actor Wrapped {
    let holder: Holder;
}

fn main() {}
"#;
    std::fs::write(&source_path, source).expect("write Hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-llvm",
            "--emit-dir",
            dir.path().to_str().expect("emit dir utf-8"),
            source_path.to_str().expect("source path utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("compile actor resource-state fixture");
    assert!(
        output.status.success(),
        "direct `#[resource] #[opaque]` actor state must compile instead of \
         failing as OpaqueHandle;\n{}",
        describe_output(&output)
    );

    let ll = std::fs::read_to_string(dir.path().join("resource_actor.ll"))
        .expect("read emitted LLVM IR");
    let clone = fn_body(&ll, "@__hew_state_clone_Direct(");
    assert!(
        clone.contains("ret ptr null"),
        "direct resource actor restart clone must refuse without copying its \
         affine handle:\n{clone}",
    );
    assert!(
        !clone.contains("@\"Dq::close\""),
        "clone refusal must not close the live source resource:\n{clone}",
    );

    let direct_drop = fn_body(&ll, "@__hew_state_drop_Direct(");
    assert_eq!(
        direct_drop.matches("@\"Dq::close\"").count(),
        1,
        "direct actor state drop must contain exactly one close call:\n{direct_drop}",
    );
    assert!(
        direct_drop.contains("store ptr null"),
        "direct actor state drop must null the field after close:\n{direct_drop}",
    );

    let wrapped_drop = fn_body(&ll, "@__hew_record_drop_inplace_Holder(");
    assert_eq!(
        wrapped_drop.matches("@\"Dq::close\"").count(),
        1,
        "the existing record-wrapped resource drop must remain exactly-once:\n{wrapped_drop}",
    );
}
