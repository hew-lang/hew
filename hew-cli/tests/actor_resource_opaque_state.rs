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
        r#"import std::fs;
import std::testing;

#[resource]
#[opaque]
type Dq {{}}

impl Dq {{
    fn close(self) {{
        unsafe {{ hew_deque_free(self) }};
        match fs.try_append("{marker_literal}", "closed\n") {{
            Ok(_) => {{}},
            Err(_) => panic("append close marker"),
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
        Ok(n) => testing.assert_eq(n, 1),
        Err(_) => testing.assert_true(false),
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
        clone.contains("field_step_0_clone")
            && clone.contains("rollback_before_step_0")
            && clone.contains("ret ptr null"),
        "direct resource actor restart clone must use the affine clone-refusal \
         protocol, not an opaque-handle codegen error or a shallow copy:\n{clone}",
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
