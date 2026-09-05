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
    let xs: Vec<Item> = Vec.new();\n\
    xs.push(Item { name: \"a\", n: 1 });\n\
    print(count_items(xs));\n\
}\n";

fn checked_dump(path: &std::path::Path) -> std::process::Output {
    Command::new(hew_binary())
        .args(["compile", "--dump-mir", "checked"])
        .arg(path)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-mir checked")
}

#[test]
fn match_and_if_let_payload_owners_use_their_real_destinations() {
    require_codegen();
    let dir = tempdir();
    let src = dir.path().join("pattern_owner_places.hew");
    std::fs::write(
        &src,
        "fn via_match(v: Result<i64, string>) -> string {\n\
         \x20   match v {\n\
         \x20       .Ok(_) => \"ok\",\n\
         \x20       .Err(message) => message,\n\
         \x20   }\n\
         }\n\
         fn via_if_let(v: Option<string>) -> string {\n\
         \x20   if let .Some(message) = v { message } else { \"none\" }\n\
         }\n\
         fn main() {\n\
         \x20   println(via_match(Err(\"match\")));\n\
         \x20   println(via_if_let(Some(\"if-let\")));\n\
         }\n",
    )
    .expect("write pattern ownership source");
    let output = checked_dump(&src);
    assert!(output.status.success(), "{}", describe_output(&output));
    let dump = String::from_utf8_lossy(&output.stdout);
    assert!(
        dump.contains("fn via_match") && dump.contains("fn via_if_let"),
        "checked dump must include both pattern controls"
    );
    for function in ["via_match", "via_if_let"] {
        let start = dump
            .find(&format!("fn {function}"))
            .expect("pattern function in checked dump");
        let tail = &dump[start..];
        let end = tail[1..]
            .find("\nfn ")
            .map_or(tail.len(), |offset| offset + 1);
        let function_dump = &tail[..end];
        assert!(
            !function_dump.lines().any(|line| {
                line.contains("ownership Mint") && line.contains("place: Local(0)")
            }),
            "owned binders in {function} must never mint against placeholder Local(0):\n{function_dump}"
        );
    }
}

#[test]
fn pg_checked_mir_has_no_generation_or_place_drift() {
    require_codegen();
    let source = repo_root().join("tests/ownership-balance/pG.hew");
    let output = checked_dump(&source);
    assert!(output.status.success(), "{}", describe_output(&output));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("E_MIR_CHECK"),
        "the exact pG false-alarm reproduction must remain clean: {stderr}"
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
        vec!["--dump-mir", "elab"],
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
