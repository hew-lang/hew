//! Sibling reject fixture for `tests/core-acceptance/cases/resource-close-consumes`.
//!
//! D442: a `#[resource]` type's `close` must be `fn close(consume self)`, so
//! calling it explicitly moves the receiver. A later use of the closed
//! binding is a compile-time use-after-move (the "consume wall"), not a
//! second live release at scope exit.

mod support;

use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

#[test]
fn use_after_explicit_close_hits_the_consume_wall() {
    require_codegen();

    let temp = support::tempdir();
    let source = temp
        .path()
        .join("resource_close_consumes_use_after_close.hew");
    let emit_dir = temp.path().join("emit");
    std::fs::write(
        &source,
        r#"
#[resource]
type Token { label: string, id: i64 }

impl Token {
    fn close(consume self) {
        println(f"close {self.label}{self.id}");
    }
}

fn main() {
    let t = Token { label: "e", id: 1 };
    t.close();
    println(f"late {t.id}");
}
"#,
    )
    .expect("write source");

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

    assert!(
        !output.status.success(),
        "a use of `t` after its explicit `close()` must be rejected; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("use of moved value `t`"),
        "the close call must move the receiver so the later read is use-after-move: {combined}"
    );
    assert!(
        !emit_dir.exists()
            || std::fs::read_dir(&emit_dir)
                .expect("read emit dir")
                .next()
                .is_none(),
        "checker rejection must not leave MIR/LLVM/native artifacts in {}",
        emit_dir.display()
    );
}
