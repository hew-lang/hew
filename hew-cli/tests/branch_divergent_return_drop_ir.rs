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

const CANCELLATION_SOURCE: &str = r#"
fn exists(path: string) -> bool {
    path.len() > 0
}

actor Driver {
    receive fn resolve() -> string {
        let path = f"path";
        if !exists(path) {
            let index = path + "/index.html";
            var before = true;
            while before {
                before = false;
            }
            if exists(index) {
                return index;
            }
        }
        var after = true;
        while after {
            after = false;
        }
        path
    }
}

fn main() {
    let d = spawn Driver;
    let _ = await d.resolve();
}
"#;

fn function_dump<'a>(dump: &'a str, name: &str) -> &'a str {
    let start = dump
        .find(&format!("fn {name} ->"))
        .unwrap_or_else(|| panic!("missing `{name}` in MIR dump:\n{dump}"));
    let tail = &dump[start..];
    tail.find("\nfn ").map_or(tail, |end| &tail[..end])
}

fn llvm_function<'a>(llvm: &'a str, name: &str) -> &'a str {
    let start = llvm
        .match_indices("define ")
        .map(|(index, _)| index)
        .find(|index| {
            llvm[*index..]
                .lines()
                .next()
                .is_some_and(|line| line.contains(&format!("@{name}(")))
        })
        .unwrap_or_else(|| panic!("missing `{name}` in LLVM IR:\n{llvm}"));
    let tail = &llvm[start..];
    let mut offset = 0;
    let end = tail
        .split_inclusive('\n')
        .find_map(|line| {
            offset += line.len();
            (line.trim_end_matches(['\r', '\n']) == "}").then_some(offset)
        })
        .unwrap_or_else(|| panic!("unterminated `{name}` definition in LLVM IR:\n{tail}"));
    &tail[..end]
}

fn llvm_blocks(function: &str) -> Vec<&str> {
    let mut blocks = Vec::new();
    let mut block_start = 0;
    let mut line_start = 0;

    for line in function.split_inclusive('\n') {
        let line_end = line_start + line.len();
        if line.trim().is_empty() {
            if block_start < line_start {
                blocks.push(&function[block_start..line_start]);
            }
            block_start = line_end;
        }
        line_start = line_end;
    }
    if block_start < function.len() {
        blocks.push(&function[block_start..]);
    }
    blocks
}

#[test]
fn llvm_function_slicing_preserves_crlf_and_finds_logical_blocks() {
    let llvm = concat!(
        "define internal i64 @probe() {\r\n",
        "entry:\r\n",
        "  br label %done\r\n",
        "\r\n",
        "done:\r\n",
        "  ret i64 42\r\n",
        "}\r\n",
        "define internal void @next() {\r\n",
        "entry:\r\n",
        "  ret void\r\n",
        "}\r\n",
    );
    let probe = llvm_function(llvm, "probe");

    assert_eq!(
        probe,
        concat!(
            "define internal i64 @probe() {\r\n",
            "entry:\r\n",
            "  br label %done\r\n",
            "\r\n",
            "done:\r\n",
            "  ret i64 42\r\n",
            "}\r\n",
        )
    );
    assert!(
        !probe.contains("@next"),
        "function slicing must stop at the standalone closing brace"
    );
    assert_eq!(
        llvm_blocks(probe),
        [
            "define internal i64 @probe() {\r\nentry:\r\n  br label %done\r\n",
            "done:\r\n  ret i64 42\r\n}\r\n",
        ],
        "logical block parsing must preserve the original CRLF content"
    );
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
    let normal_path_drops = resolve_path
        .lines()
        .scan(false, |in_cancel_plan, line| {
            if line.starts_with("    cancel[") {
                *in_cancel_plan = true;
            } else if line.starts_with("    ")
                && line.contains("] ->")
                && !line.starts_with("      ")
            {
                *in_cancel_plan = false;
            }
            Some(!*in_cancel_plan && line.contains("drop _") && line.contains("ty=string"))
        })
        .filter(|is_drop| *is_drop)
        .count();
    assert_eq!(
        normal_path_drops, 2,
        "the function must contain one release for `path` on the nested early \
         return and one release for the unreturned `index` on the sibling \
         fallthrough, with no duplicate at the following join:\n{resolve_path}"
    );
}

#[test]
fn normal_goto_prevents_later_loop_cancellation_from_releasing_index_twice() {
    require_codegen();
    let dir = tempdir().expect("temporary fixture directory");
    let source = dir.path().join("cancelled_result.hew");
    let emit_dir = dir.path().join("emit");
    std::fs::write(&source, CANCELLATION_SOURCE).expect("write fixture");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            emit_dir.to_str().expect("emit directory path is UTF-8"),
            source.to_str().expect("fixture path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("run compiler");
    assert!(
        output.status.success(),
        "cancellation fixture must compile:\n{}",
        describe_output(&output)
    );

    let llvm = std::fs::read_to_string(emit_dir.join("cancelled_result.ll"))
        .expect("read emitted LLVM IR");
    let resolve = llvm_function(&llvm, "Driver__recv__resolve");
    let blocks = llvm_blocks(resolve);
    let normal_index_releases: Vec<_> = blocks
        .iter()
        .copied()
        .filter(|block| {
            block.trim_start().starts_with("after_cooperate")
                && block.contains("call void @hew_string_drop")
                && block.contains("ptr %local_7")
        })
        .collect();
    assert_eq!(
        normal_index_releases.len(),
        1,
        "the scope-closing Goto must have one normal release authority for index:\n{resolve}"
    );
    let index_cancellation_blocks: Vec<_> = blocks
        .iter()
        .copied()
        .filter(|block| {
            block.trim_start().starts_with("cancel_exit")
                && block.contains("call void @hew_string_drop")
                && block.contains("ptr %local_7")
        })
        .collect();
    assert_eq!(
        index_cancellation_blocks.len(),
        2,
        "only cancellation paths that bypass the normal Goto may release index:\n{resolve}"
    );
    assert!(
        index_cancellation_blocks.iter().all(|block| {
            block.trim_start().starts_with("cancel_exit9")
                || block.trim_start().starts_with("cancel_exit22")
        }),
        "the retained index releases must belong to cancellation paths that run \
         before the normal Goto:\n{index_cancellation_blocks:#?}"
    );
    assert!(
        blocks
            .iter()
            .copied()
            .find(|block| block.trim_start().starts_with("cancel_exit36"))
            .is_some_and(|block| !block.contains("ptr %local_7")),
        "the later loop cancellation must not duplicate the index release after \
         the normal Goto:\n{resolve}"
    );
}
