//! Native pipeline rejection oracle for structural clone of affine records.
//!
//! Before the checker guard, both `value.clone()` and `clone value` reached
//! `RecordCloneInplace`. A `#[resource]` value therefore acquired two live
//! owners and its `close` method ran twice at scope exit; a `#[linear]` value
//! could likewise be consumed twice. The source below keeps the release print
//! in the close body as the exact counterfactual: the pre-fix native binary
//! printed each resource id twice. Post-fix compilation must stop in the
//! checker, before MIR/LLVM or a native executable is emitted.

mod support;

use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

#[test]
fn affine_record_clone_stops_before_native_mir_and_release() {
    require_codegen();

    let temp = support::tempdir();
    let source = temp.path().join("affine_record_clone.hew");
    let emit_dir = temp.path().join("emit");
    std::fs::write(
        &source,
        r#"
#[resource]
type ResourceToken { id: i64 }
impl ResourceToken {
    fn close(self) {
        print(self.id);
        println("");
    }
}

#[linear]
type LinearTicket { id: i64 }
impl LinearTicket {
    fn redeem(consuming self) -> i64 { self.id }
}

fn main() -> i64 {
    let r1 = ResourceToken { id: 41 };
    let _r1_copy = r1.clone();
    let r2 = ResourceToken { id: 42 };
    let _r2_copy = clone r2;

    let l1 = LinearTicket { id: 51 };
    let _l1_copy = l1.clone();
    let _ = l1.redeem();
    let l2 = LinearTicket { id: 52 };
    let _l2_copy = clone l2;
    let _ = l2.redeem();

    var resources: Vec<ResourceToken> = Vec.new();
    resources.push(ResourceToken { id: 61 });
    let _resources_copy = resources.clone();

    var tickets: HashMap<string, LinearTicket> = HashMap.new();
    tickets.insert("one", LinearTicket { id: 62 });
    let _tickets_copy = clone tickets;

    var late_resources = Vec.new();
    var i = 0;
    while i < 2 {
        if i == 1 {
            let _late_copy = late_resources.clone();
        } else {
            late_resources.push(ResourceToken { id: 63 });
        }
        i = i + 1;
    }
    0
}
"#,
    )
    .expect("write affine clone source");

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
        "affine record clones must fail before native lowering; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        combined.matches("cannot be cloned").count(),
        7,
        "record and builtin-container dispatch must reject affine clones: {combined}"
    );
    assert!(
        combined.contains("`#[resource]`")
            && combined.contains("affine close contract")
            && combined.contains("`#[linear]`")
            && combined.contains("consumed exactly once"),
        "diagnostics must name both violated ownership contracts: {combined}"
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
