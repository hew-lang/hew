//! Oracle for a copy-requiring `HashMap` operation on an unbounded value type.
//!
//! A type parameter carrying no `Clone` bound promises no instantiation a copy
//! path, and no later stage answers for it, so the obligation belongs to the
//! declaration (spec §3.8.1). These bodies are refused where they are written,
//! with no instantiation needed and no artifact emitted.

mod support;

use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

#[test]
fn generic_hashmap_affine_clone_out_stops_before_codegen() {
    require_codegen();

    let temp = support::tempdir();
    let source = temp.path().join("generic_hashmap_affine_clone.hew");
    let emit_dir = temp.path().join("emit");
    std::fs::write(
        &source,
        r#"
#[resource]
type Token { id: i64 }
impl Token {
    fn close(consume self) {}
}

fn duplicate<V>(values: HashMap<string, V>) -> HashMap<string, V> {
    values.clone()
}

fn index<V>(values: HashMap<string, V>) -> V {
    values["live"]
}

fn main() {
    var cloned: HashMap<string, Token> = HashMap.new();
    cloned.insert("live", Token { id: 1 });
    let _copy = duplicate(cloned);

    var indexed: HashMap<string, Token> = HashMap.new();
    indexed.insert("live", Token { id: 3 });
    let _value = index(indexed);
}
"#,
    )
    .expect("write generic HashMap affine clone source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            emit_dir.to_str().expect("emit path utf-8"),
            source.to_str().expect("source path utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !output.status.success(),
        "a copy of an unbounded value type must be refused at the declaration: {combined}"
    );
    for operation in ["HashMap.clone()", "m[k]"] {
        assert!(
            combined.contains(operation),
            "diagnostic must name the refused {operation}: {combined}"
        );
    }
    assert!(
        combined.contains("`V` has no `Clone` bound") && combined.contains("declare `V: Clone`"),
        "the refusal must name the parameter and the bound it is missing: {combined}"
    );
    assert!(
        combined.contains("generic_hashmap_affine_clone.hew:9:5"),
        "the refusal belongs to the declaration, with its own span: {combined}"
    );
    assert!(
        !emit_dir.exists()
            || std::fs::read_dir(&emit_dir)
                .expect("read emit dir")
                .next()
                .is_none(),
        "MIR rejection must not leave LLVM/native artifacts in {}",
        emit_dir.display()
    );
}
