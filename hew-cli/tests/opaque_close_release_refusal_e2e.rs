//! An authored opaque `close` that never hands its receiver to a release is
//! refused: nothing else can release the handle. The refusal is a lowering
//! fact, so it needs a compile rather than a check, which a core-acceptance
//! `reject` case cannot express.

mod support;

use support::{describe_output, hew_command, repo_root, require_codegen};

const CLOSE_KEEPS_RECEIVER: &str = "\
#[resource]
#[opaque]
type Kept {}
impl Kept {
    fn close(consume self) { println(\"bye\"); }
}
extern \"C\" {
    fn hew_deque_new() -> Kept;
}
fn main() {
    let _k = unsafe { hew_deque_new() };
    println(\"made\");
}
";

#[test]
fn opaque_close_keeping_its_receiver_is_refused() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("opaque-close-keeps-receiver-")
        .tempdir()
        .expect("tempdir");
    let source = dir.path().join("keeps.hew");
    std::fs::write(&source, CLOSE_KEEPS_RECEIVER).expect("write hew source");
    let output = hew_command()
        .arg("compile")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        !output.status.success(),
        "a close that leaves its receiver owned must be refused;\n{}",
        describe_output(&output)
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("must consume its receiver"),
        "expected the receiver-consumption refusal, got:\n{combined}"
    );
}
