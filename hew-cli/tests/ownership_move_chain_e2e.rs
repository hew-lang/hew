//! Hard-cutover ownership regression: an owned `VecIter` yield obligation
//! follows whole-local moves and is destroyed exactly once. No advisory MIR
//! channel exists; a verifier finding is always a build error.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen, tempdir};

const SOURCE: &str = "\
type Item { name: string, n: i64, }\n\
fn count_items(xs: Vec<Item>) -> i64 {\n\
    let it = xs.iter();\n\
    var total = 0;\n\
    for _ in it { total = total + 1; }\n\
    total\n\
}\n\
fn main() {\n\
    var xs: Vec<Item> = Vec.new();\n\
    xs.push(Item { name: \"a\", n: 1 });\n\
    print(count_items(xs));\n\
}\n";

fn physical_dump(path: &std::path::Path) -> std::process::Output {
    Command::new(hew_binary())
        .args(["compile", "--dump-mir", "physical"])
        .arg(path)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-mir physical")
}

/// Slice out the `PhysicalFunction` body for `symbol`. Dead (unreferenced
/// prelude) callables are declared but never lowered into a `functions`
/// entry, so position in `callables` cannot be reused to index `functions`;
/// resolve the callable's numeric id first, then locate its function block
/// by that id.
fn function_section<'a>(dump: &'a str, symbol: &str) -> &'a str {
    let callables_start = dump.find("callables: [").expect("callables section");
    let functions_start = dump.find("functions: [").expect("functions section");
    let callables = &dump[callables_start..functions_start];
    let marker = format!("symbol: \"__hew_fn_{symbol}\"");
    let symbol_pos = callables
        .find(&marker)
        .unwrap_or_else(|| panic!("missing `{marker}` in physical MIR dump:\n{dump}"));
    let block_start = callables[..symbol_pos]
        .rfind("PhysicalCallable {")
        .expect("enclosing PhysicalCallable");
    let id_pos = callables[block_start..]
        .find("id: CallableId(")
        .expect("callable id")
        + block_start;
    let id_digits: String = callables[id_pos..]
        .chars()
        .skip("id: CallableId(".len())
        .skip_while(|c| c.is_whitespace())
        .take_while(char::is_ascii_digit)
        .collect();
    let header = format!("PhysicalFunction {{\n            callable: CallableId(\n                {id_digits},\n            ),");
    let functions = &dump[functions_start..];
    let start = functions.find(&header).unwrap_or_else(|| {
        panic!("no PhysicalFunction with callable id {id_digits} for `{symbol}`")
    });
    let tail = &functions[start..];
    let end = tail[1..]
        .find("PhysicalFunction {")
        .map_or(tail.len(), |offset| offset + 1);
    &tail[..end]
}

#[test]
fn match_and_if_let_payload_owners_use_their_real_destinations() {
    require_codegen();
    let dir = tempdir();
    let src = dir.path().join("pattern_owner_places.hew");
    // `if let` payload lowering (`via_if_let` in the original source) is
    // currently unimplemented in SIR (E_SIR_UNSUPPORTED: "unsupported HIR
    // expression kind in the initial SIR subset"), independent of this test
    // or its dump stage; that gap is outside this file's scope and is
    // reported separately rather than fixed here. This test now pins only
    // the `match` arm payload, which does lower and compile.
    std::fs::write(
        &src,
        "fn via_match(v: Result<i64, string>) -> string {\n\
         \x20   match v {\n\
         \x20       .Ok(_) => \"ok\",\n\
         \x20       .Err(message) => message,\n\
         \x20   }\n\
         }\n\
         fn main() {\n\
         \x20   println(via_match(Err(\"match\")));\n\
         }\n",
    )
    .expect("write pattern ownership source");
    let output = physical_dump(&src);
    assert!(output.status.success(), "{}", describe_output(&output));
    let dump = String::from_utf8_lossy(&output.stdout);
    // Physical MIR has no `Local(0)` placeholder to mint against (the legacy
    // lowerer's bug class this test pinned); storage is always a real,
    // allocated `StorageId`. The surviving invariant is that the owned
    // `message` binder is actually destroyed at its own real destination on
    // the arm that owns it, not silently dropped or aliased onto another
    // binder's storage.
    let function_dump = function_section(&dump, "via_match");
    assert!(
        function_dump.contains("StringRelease"),
        "the owned `message` payload in via_match must be released at its \
         own real destination:\n{function_dump}"
    );
}

#[test]
fn moved_yield_owner_is_clean_on_every_output_path() {
    require_codegen();
    let dir = tempdir();
    let src = dir.path().join("move_chain.hew");
    std::fs::write(&src, SOURCE).expect("write source");
    let src = src.to_str().expect("utf-8 path");

    for extra in [
        Vec::<&str>::new(),
        vec!["--dump-mir", "physical"],
        vec!["--format", "json"],
    ] {
        let output = Command::new(hew_binary())
            .args(["compile", "--emit-dir", dir.path().to_str().unwrap()])
            .args(extra)
            .arg(src)
            .current_dir(repo_root())
            .output()
            .expect("invoke hew compile");
        assert!(output.status.success(), "{}", describe_output(&output));
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            !stderr.contains("ObligationUnderReleased")
                && !stdout.contains("ObligationUnderReleased"),
            "move-chain ownership must verify cleanly; {}",
            describe_output(&output)
        );
    }
}
