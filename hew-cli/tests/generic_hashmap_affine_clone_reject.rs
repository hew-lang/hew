//! Native-pipeline oracle for affine clone-out through generic `HashMap` methods.
//!
//! The checker admits these bodies while `V` is abstract. MIR must repeat the
//! clone-totality proof after monomorphisation so a concrete resource value
//! cannot reach the descriptor clone choke and acquire a second owner.

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
    fn close(self) {}
}

fn duplicate<V>(values: HashMap<string, V>) -> HashMap<string, V> {
    values.clone()
}

fn lookup<V>(values: HashMap<string, V>) -> Option<V> {
    values.get("live")
}

fn index<V>(values: HashMap<string, V>) -> V {
    values["live"]
}

fn main() {
    var cloned: HashMap<string, Token> = HashMap::new();
    cloned.insert("live", Token { id: 1 });
    let _copy = duplicate(cloned);

    var looked_up: HashMap<string, Token> = HashMap::new();
    looked_up.insert("live", Token { id: 2 });
    let _value = lookup(looked_up);

    var indexed: HashMap<string, Token> = HashMap::new();
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
        "generic affine clone-out must fail before codegen: {combined}"
    );
    for operation in ["HashMap::clone()", "HashMap::get()", "HashMap indexing"] {
        assert!(
            combined.contains(operation) && combined.contains("affine close contract"),
            "diagnostic must name the rejected {operation} resource clone: {combined}"
        );
    }
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
