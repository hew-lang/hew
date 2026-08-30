//! A returned `Result` must release the non-returned sibling on each arm.

mod support;

use std::collections::{BTreeMap, BTreeSet, VecDeque};
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

fn drop_plan_sections(function: &str) -> Vec<Vec<&str>> {
    let mut sections = Vec::new();
    for line in function.lines() {
        let is_header =
            line.starts_with("    ") && !line.starts_with("      ") && line.contains("] ->");
        if is_header {
            sections.push(vec![line.trim()]);
        } else if let Some(section) = sections.last_mut() {
            if line.starts_with("      ") {
                section.push(line.trim());
            }
        }
    }
    sections
}

fn plan_header<'a>(section: &'a [&'a str]) -> &'a str {
    section.first().copied().unwrap_or_default()
}

fn mir_plan_edges(header: &str) -> Vec<(String, String)> {
    let Some((_, tail)) = header.split_once('[') else {
        return Vec::new();
    };
    let Some((inside, _)) = tail.split_once(']') else {
        return Vec::new();
    };
    if header.starts_with("goto[") {
        return inside
            .split_once("->")
            .map(|(from, to)| vec![(from.to_owned(), to.to_owned())])
            .unwrap_or_default();
    }
    if header.starts_with("branch[") {
        let Some((from, targets)) = inside.split_once(": ") else {
            return Vec::new();
        };
        return targets
            .split('/')
            .map(|to| (from.to_owned(), to.to_owned()))
            .collect();
    }
    if header.starts_with("call[") {
        let Some((from, rest)) = inside.split_once(' ') else {
            return Vec::new();
        };
        return rest
            .rsplit_once(" -> ")
            .map(|(_, to)| vec![(from.to_owned(), to.to_owned())])
            .unwrap_or_default();
    }
    Vec::new()
}

fn mir_plan_source(header: &str) -> Option<&str> {
    let (_, tail) = header.split_once('[')?;
    let (inside, _) = tail.split_once(']')?;
    inside
        .split_once([' ', ':', '-', ']'])
        .map_or(Some(inside), |(source, _)| Some(source))
}

fn mir_reachable(sections: &[Vec<&str>], root: &str) -> BTreeSet<String> {
    let mut edges: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for (from, to) in sections
        .iter()
        .flat_map(|section| mir_plan_edges(plan_header(section)))
    {
        edges.entry(from).or_default().push(to);
    }
    let mut reachable = BTreeSet::new();
    let mut pending = VecDeque::from([root.to_owned()]);
    while let Some(block) = pending.pop_front() {
        if reachable.insert(block.clone()) {
            pending.extend(edges.get(&block).into_iter().flatten().cloned());
        }
    }
    reachable
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
    let sections = drop_plan_sections(choose);
    let return_section = sections
        .iter()
        .find(|section| plan_header(section).starts_with("return["))
        .expect("choose must have a return drop plan");
    let return_block = plan_header(return_section)
        .strip_prefix("return[")
        .and_then(|tail| tail.split(']').next())
        .expect("return plan must name its MIR block");
    let arm_exits: Vec<_> = sections
        .iter()
        .filter(|section| {
            let header = plan_header(section);
            header.starts_with("goto[") && header.contains(&format!("->{return_block}]"))
        })
        .collect();
    assert_eq!(
        arm_exits.len(),
        2,
        "both branch arms must flow into the shared return block:\n{choose}"
    );
    assert!(
        arm_exits.iter().all(|section| {
            section
                .iter()
                .filter(|line| line.contains("kind=cow_heap(hew_string_drop)"))
                .count()
                == 1
        }),
        "each arm-to-join exit must release exactly its non-returned sibling:\n{choose}"
    );
    assert!(
        return_section.len() == 2
            && plan_header(return_section) == format!("return[{return_block}] ->")
            && return_section[1] == "(none)",
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
    let sections = drop_plan_sections(resolve_path);
    let normal_release_count = |local: &str| {
        sections
            .iter()
            .filter(|section| {
                let header = plan_header(section);
                header.starts_with("goto[") || header.starts_with("return[")
            })
            .filter(|section| {
                section
                    .iter()
                    .any(|line| line.contains(&format!("drop {local} ty=string")))
            })
            .count()
    };
    assert_eq!(
        normal_release_count("_3"),
        1,
        "the nested early return must release `path` exactly once on normal flow:\n\
         {resolve_path}"
    );
    assert_eq!(
        normal_release_count("_9"),
        1,
        "the sibling fallthrough must release the unreturned `index` exactly once on normal \
         flow, without duplicating it at the following join:\n{resolve_path}"
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
            "--dump-mir",
            "elab",
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

    let dump = String::from_utf8(output.stdout).expect("MIR dump is UTF-8");
    let resolve = function_dump(&dump, "Driver__recv__resolve");
    let sections = drop_plan_sections(resolve);
    let index_release = |section: &&Vec<&str>| {
        section
            .iter()
            .any(|line| line.contains("drop _7 ty=string"))
    };
    let normal_index_releases: Vec<_> = sections
        .iter()
        .enumerate()
        .filter(|(_, section)| plan_header(section).starts_with("goto[") && index_release(section))
        .collect();
    assert_eq!(
        normal_index_releases.len(),
        1,
        "the scope-closing Goto must have one normal release authority for index:\n{resolve}"
    );
    let index_cancellation_blocks: Vec<_> = sections
        .iter()
        .enumerate()
        .filter(|(_, section)| {
            plan_header(section).starts_with("cancel[") && index_release(section)
        })
        .collect();
    assert_eq!(
        index_cancellation_blocks.len(),
        2,
        "only cancellation paths that bypass the normal Goto may release index:\n{resolve}"
    );
    let normal_release_header = plan_header(normal_index_releases[0].1);
    let normal_release_target = normal_release_header
        .split_once("->")
        .and_then(|(_, tail)| tail.split(']').next())
        .expect("normal index release must be a Goto with a target");
    let after_normal_release = mir_reachable(&sections, normal_release_target);
    let later_cancellations: Vec<_> = sections
        .iter()
        .filter(|section| {
            let header = plan_header(section);
            header.starts_with("cancel[")
                && mir_plan_source(header)
                    .is_some_and(|source| after_normal_release.contains(source))
        })
        .collect();
    assert!(
        index_cancellation_blocks
            .iter()
            .all(|(_, section)| mir_plan_source(plan_header(section))
                .is_some_and(|source| !after_normal_release.contains(source)))
            && !later_cancellations.is_empty()
            && later_cancellations
                .iter()
                .all(|section| !index_release(section)),
        "the later loop cancellation must not duplicate the index release after \
         the normal Goto:\n{resolve}"
    );
}
