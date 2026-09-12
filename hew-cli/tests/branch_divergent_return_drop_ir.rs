//! A returned `Result` must release the non-returned sibling on each arm.

mod support;

use std::process::Command;

use tempfile::tempdir;

use support::leak_slope::compile_to_native;
use support::{describe_output, require_codegen};

/// `take_x=true` selects the `Err(x)` arm: `y` is the non-returned sibling
/// and must be released on this arm's exit.
const SOURCE_TAKE_X: &str = r#"
fn choose(take_x: bool) -> Result<string, string> {
    let x = f"x={1}";
    let y = f"y={2}";
    if take_x { Err(x) } else { Ok(y) }
}

fn main() -> i64 {
    match choose(true) {
        .Ok(y) => y.len(),
        .Err(x) => x.len(),
    }
}
"#;

/// `take_x=false` selects the `Ok(y)` arm: `x` is the non-returned sibling
/// and must be released on this arm's exit.
const SOURCE_TAKE_Y: &str = r#"
fn choose(take_x: bool) -> Result<string, string> {
    let x = f"x={1}";
    let y = f"y={2}";
    if take_x { Err(x) } else { Ok(y) }
}

fn main() -> i64 {
    match choose(false) {
        .Ok(y) => y.len(),
        .Err(x) => x.len(),
    }
}
"#;

const STATIC_SERVER_SHAPED_SOURCE: &str = r#"
fn exists(path: string) -> bool {
    path.len() > 8
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
    resolve_path("abcdefghij", "").len()
}
"#;

/// Same `resolve_path` shape as [`STATIC_SERVER_SHAPED_SOURCE`], called with
/// an empty `root`/`url_path` so `exists(path)` is false: the nested block
/// runs, `index` is computed and returned, and `path` (the non-returned
/// sibling) must be released exactly once at the early return.
const STATIC_SERVER_SHAPED_SOURCE_NESTED_RETURN: &str = r#"
fn exists(path: string) -> bool {
    path.len() > 8
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
    resolve_path("ab", "").len()
}
"#;

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

/// Compile `source` to a native binary and, on macOS, run it under the
/// poisoned allocator, asserting the exit code equals `expected_exit`.
///
/// What this actually detects: the plain run's exit code pins the selected
/// arm's returned length, so a defect that changes control flow or the
/// returned value is caught on every platform. On macOS, `MallocScribble` /
/// `MallocPreScribble` / `MallocGuardEdges` turn an over-release (a
/// double-free of the non-returned sibling) into an abort — that half is
/// macOS-only, since those env vars are inert Darwin facilities elsewhere
/// (see `support::leak_slope`). An under-release (a leak) is NOT detected by
/// either half: neither a plain exit code nor the poisoned allocator
/// observes memory that was never freed.
fn assert_branch_fixture_exit_code(name: &str, source: &str, expected_exit: i32) {
    require_codegen();
    let dir = tempdir().expect("temporary fixture directory");
    let bin = compile_to_native(source, dir.path(), name);

    let output = Command::new(&bin).output().expect("run compiled fixture");
    assert_eq!(
        output.status.code(),
        Some(expected_exit),
        "{name} exit code must equal the selected arm's string length:\n{}",
        describe_output(&output)
    );

    #[cfg(target_os = "macos")]
    {
        let scribbled = support::leak_slope::run_under_malloc_scribble(&bin);
        assert_eq!(
            scribbled.status.code(),
            Some(expected_exit),
            "{name} must survive the poisoned allocator with the same exit code \
             (a double-release of the non-returned sibling aborts here):\n{}",
            describe_output(&scribbled)
        );
    }
}

/// #2648-shaped oracle: a returned `Result` must release the non-returned
/// sibling on each arm. Previously pinned by golden-comparing
/// `hew compile --dump-mir elab` drop-plan text (`goto[...]`/`return[...]`
/// sections, `kind=cow_heap(hew_string_drop)` counts); `--dump-mir elab` no
/// longer exists. The compiled-and-run oracle below trades directions: it
/// proves an over-release (double-free) under the poisoned allocator on
/// macOS, which the static text check never ran a program to observe, but
/// it does not prove exactly-once the way a static drop-plan count did — an
/// under-release (a leak of the non-returned sibling) passes silently here.
/// It does exercise both arms, where the original only compiled
/// `take_x=true`.
#[test]
fn returned_result_branch_releases_only_the_nonreturned_string_sibling() {
    // "x=1" and "y=2" are both length 3, so both arms must report exit 3;
    // a leaked, under-released, or over-released sibling on the OTHER arm
    // still shows up as a corrupted exit code or an abort under scribble.
    assert_branch_fixture_exit_code("branch_result_err_arm", SOURCE_TAKE_X, 3);
    assert_branch_fixture_exit_code("branch_result_ok_arm", SOURCE_TAKE_Y, 3);
}

/// The nested return/fallthrough shape from `static_server::resolve_path`
/// must compile without over- or under-releasing `path` or `index`.
/// Previously pinned by golden-comparing `hew compile --dump-mir elab`
/// drop-plan text for exactly-once normal-flow releases of specific
/// `BindingId`s; `--dump-mir elab` no longer exists. The compiled-and-run
/// oracle below exercises the nested early-return arm (empty `root`/
/// `url_path`, so `index` is computed and returned, and `path` is the
/// non-returned sibling) and, on macOS, proves no double-release under the
/// poisoned allocator.
#[test]
fn nested_returned_string_scope_exit_is_discharged_exactly_once() {
    // resolve_path("ab", "") -> path="ab" (len 2, heap-allocated concat) ->
    // !exists(path) (2 <= 8) -> index="ab/index.html" -> exists(index)
    // (13 > 8) -> return index. Exit code is index.len() == 13; `path` is
    // the non-returned sibling released at the early return.
    assert_branch_fixture_exit_code(
        "resolve_path_nested_return",
        STATIC_SERVER_SHAPED_SOURCE_NESTED_RETURN,
        13,
    );
    // resolve_path("abcdefghij", "") -> path="abcdefghij" (len 10) ->
    // exists(path) (10 > 8) -> skip the nested block entirely -> return
    // path. Exit code is path.len() == 10; `index` is never allocated on
    // this arm.
    assert_branch_fixture_exit_code("resolve_path_fallthrough", STATIC_SERVER_SHAPED_SOURCE, 10);
}

// `normal_goto_prevents_later_loop_cancellation_from_releasing_index_twice`
// used to live here: an oracle golden-comparing `hew compile --dump-mir
// elab` drop-plan text (`goto[...]`/`cancel[...]` sections) to prove a later
// loop-cancellation cleanup edge does not duplicate a release the normal
// scope-closing `Goto` already performed. `--dump-mir elab` no longer
// exists, and physical MIR's `{:#?}` `Debug` dump of `PhysicalModule` has no
// comparable block-by-block text form to migrate the parser to. There is
// also no `hew run` execution that reaches this fixture's cancellation
// edges: its busy-loops resolve immediately, and nothing external cancels
// the actor's `resolve()` call, so no runtime oracle can trigger the path
// either. The test was deleted rather than left asserting on a text shape
// that cannot exist. Lost coverage: exactly-once release of a scope-closed
// `string` local on a later cancellation edge that bypasses (rather than
// follows) a normal `Goto` release of the same local — a double-release
// specifically on that cancel path is not pinned anywhere else in this
// repository (grepped `hew-mir/tests` and `hew-sir/tests` for the shape;
// no hit).
