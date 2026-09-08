//! Structural rendering must borrow every aggregate member. This oracle uses a
//! real heap-backed affine resource, renders its owning record twice, and proves
//! the same identity remains live until the record's one scope-exit close.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{compile_to_native, measure_leaks_exact, require_leaks_tool};
use support::{describe_output, require_codegen};

const SOURCE: &str = r#"
#[resource]
#[opaque]
type Dq {}

impl Dq {
    fn close(consume self) {
        unsafe { hew_deque_free(self) };
        println("closed");
    }
}

extern "C" {
    fn hew_deque_new() -> Dq;
    fn hew_deque_free(consume dq: Dq);
}

type Holder {
    dq: Dq,
    label: string,
    values: Vec<i64>,
}

fn cycle() {
    var values: Vec<i64> = Vec.new();
    values.push(7);
    let holder = Holder {
        dq: unsafe { hew_deque_new() },
        label: "held",
        values: values,
    };
    println(f"{holder:?}");
    println(f"{holder:?}");
    println(holder.label);
}

fn main() {
    for _i in 0..32 {
        cycle();
    }
    println("done");
}
"#;

// Lost coverage: `structural_rendering_reads_without_clone_move_or_consume`
// used `--dump-mir raw` (retired) to pin that each `{holder:?}` render site
// lowers to exactly one `hew_structural_format` call and introduces no
// `clone`/`consume`/`Move` ownership operation. Physical MIR's structured
// (Debug) dump has no equivalent single-line text to grep, so this
// MIR-emission coverage has no direct replacement. The externally observable
// half of the same invariant -- rendering twice adds neither a clone leak nor
// a lost resource owner -- is still proven end to end below by the exact
// leak-count oracle.

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact ownership oracle needs macOS leaks(1); unsupported hosts must skip explicitly"
)]
#[test]
fn structural_rendering_has_zero_ownership_effect() {
    require_codegen();
    require_leaks_tool();
    let dir = tempfile::tempdir().expect("tempdir");
    let binary = compile_to_native(SOURCE, dir.path(), "structural_rendering_ownership");
    let output = Command::new(&binary)
        .output()
        .expect("run ownership fixture");
    assert!(
        output.status.success(),
        "ownership fixture failed:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8(output.stdout).expect("stdout is UTF-8");
    let lines = stdout.lines().collect::<Vec<_>>();
    assert_eq!(lines.last(), Some(&"done"));
    assert_eq!(lines.iter().filter(|line| **line == "closed").count(), 32);
    assert_eq!(lines.iter().filter(|line| **line == "held").count(), 32);
    let rendered = lines
        .iter()
        .filter(|line| line.starts_with("Holder { "))
        .copied()
        .collect::<Vec<_>>();
    assert_eq!(rendered.len(), 64);
    for pair in rendered.chunks_exact(2) {
        assert_eq!(
            pair[0], pair[1],
            "rendering the same live owner twice must preserve its identity"
        );
        assert!(pair[0].contains("dq: <Dq@"));
        assert!(pair[0].contains("label: held"));
        assert!(pair[0].contains("values: [7]"));
    }
    assert_eq!(
        measure_leaks_exact(&binary),
        (0, 0),
        "rendering must add neither a clone leak nor a lost resource owner"
    );
}
