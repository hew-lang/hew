//! A returned `Result` must release the non-returned sibling on each arm.

mod support;

use std::process::Command;

use tempfile::tempdir;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE: &str = r#"
fn choose(take_x: bool) -> Result<string, string> {
    let x = f"x={1}";
    let y = f"y={2}";
    if take_x { Err(x) } else { Ok(y) }
}

fn main() -> i64 {
    match choose(true) {
        Ok(y) => y.len(),
        Err(x) => x.len(),
    }
}
"#;

const STATIC_SERVER_SHAPED_SOURCE: &str = r#"
fn exists(path: string) -> bool {
    path.len() > 0
}

fn resolve_path(root: string, url_path: string) -> string {
    let path = root + url_path;
    if !exists(path) {
        let index = path + "/index.html";
        if exists(index) {
            return index;
        }
    }
    path
}

fn main() -> i64 {
    resolve_path(".", "/missing").len()
}
"#;

fn function_dump<'a>(dump: &'a str, name: &str) -> &'a str {
    let start = dump
        .find(&format!("fn {name} ->"))
        .unwrap_or_else(|| panic!("missing `{name}` in MIR dump:\n{dump}"));
    let tail = &dump[start..];
    tail.find("\nfn ").map_or(tail, |end| &tail[..end])
}

#[test]
fn returned_result_branch_releases_only_the_nonreturned_string_sibling() {
    require_codegen();
    let dir = tempdir().expect("temporary fixture directory");
    let source = dir.path().join("branch_result.hew");
    std::fs::write(&source, SOURCE).expect("write fixture");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            "elab",
            source.to_str().expect("fixture path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("run compiler");
    assert!(
        output.status.success(),
        "fixture must compile:\n{}",
        describe_output(&output)
    );

    let dump = String::from_utf8(output.stdout).expect("MIR dump is UTF-8");
    let choose = function_dump(&dump, "choose");
    let sibling_drops = choose.matches("kind=cow_heap(hew_string_drop)").count();
    assert_eq!(
        sibling_drops, 2,
        "each return arm must drop exactly its non-returned sibling:\n{choose}"
    );
    assert!(
        choose.contains("goto[bb5->bb7] ->\n      drop ")
            && choose.contains("goto[bb6->bb7] ->\n      drop "),
        "the drops must be attached to the two arm-to-join exits, not the common return:\n{choose}"
    );
    assert!(
        choose.contains("return[bb7] ->\n      (none)"),
        "the joined return owns neither branch-local sibling:\n{choose}"
    );
}

#[test]
fn nested_returned_string_scope_exit_is_discharged_exactly_once() {
    require_codegen();
    let dir = tempdir().expect("temporary fixture directory");
    let source = dir.path().join("static_server_resolve_path.hew");
    std::fs::write(&source, STATIC_SERVER_SHAPED_SOURCE).expect("write fixture");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            "elab",
            source.to_str().expect("fixture path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("run compiler");
    assert!(
        output.status.success(),
        "the nested return/fallthrough shape from static_server::resolve_path \
         must compile without over-releasing `index`:\n{}",
        describe_output(&output)
    );

    let dump = String::from_utf8(output.stdout).expect("MIR dump is UTF-8");
    let resolve_path = function_dump(&dump, "resolve_path");
    let index_drops = resolve_path
        .lines()
        .filter(|line| line.contains("drop _") && line.contains("ty=string"))
        .count();
    assert_eq!(
        index_drops, 2,
        "the function must contain one release for `path` on the nested early \
         return and one release for the unreturned `index` on the sibling \
         fallthrough, with no duplicate at the following join:\n{resolve_path}"
    );
}
