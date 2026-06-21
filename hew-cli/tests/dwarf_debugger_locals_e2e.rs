//! Live-debugger e2e for `hew build -g`: build a real binary, drive `lldb`
//! (or `gdb`) in batch mode to a breakpoint, and assert the locals the debugger
//! reports. This is the gate a static-IR review cannot stand in for — it proves
//! the END-TO-END story (codegen → linker → DWARF → debugger) a backend dev
//! actually experiences. It caught what the IR-level harness could not: a
//! shadowed inner `let first` whose breakpoint reported the OUTER binding's
//! value, and an `optnone`-less `-O0` body whose slot lagged its value.
//!
//! Skips (no-op, not fail-open) when no debugger is on the host — a missing
//! `lldb`/`gdb` is not a Hew defect, but a present one must see the right value.

mod support;

use std::path::Path;
use std::process::Command;

use support::{hew_binary, repo_root, require_codegen, run_bounded_command};

const SHADOW_SRC: &str = "\
fn probe(selector: i32) -> i32 {
    let first = selector + 1;
    {
        let first = selector + 2;
        println(first);
    }
    first
}

fn main() {
    println(probe(41))
}
";

fn workspace() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("dwarf-dbg-hew-")
        .tempdir_in(repo_root())
        .expect("temp dir")
}

/// First available batch debugger as `(program, args-builder)`. `lldb -b -o ...`
/// and `gdb --batch -ex ...` both run a script non-interactively.
fn debugger() -> Option<&'static str> {
    ["lldb", "gdb"].into_iter().find(|d| {
        Command::new(d)
            .arg("--version")
            .output()
            .is_ok_and(|o| o.status.success())
    })
}

/// Run the debugger to a breakpoint on `line` and return its stdout. Reads the
/// local `first` at that stop.
fn read_first_at_line(debugger: &str, binary: &Path, src: &Path, line: u32) -> String {
    let src = src.to_str().expect("src path utf8");
    let bin = binary.to_str().expect("bin path utf8");
    let cmd = if debugger == "lldb" {
        let mut c = Command::new("lldb");
        c.args([
            "-b",
            "-o",
            &format!("breakpoint set --file {src} --line {line}"),
            "-o",
            "run",
            "-o",
            "frame variable first",
            "-o",
            "quit",
            bin,
        ]);
        c
    } else {
        let mut c = Command::new("gdb");
        c.args([
            "--batch",
            "-ex",
            &format!("break {src}:{line}"),
            "-ex",
            "run",
            "-ex",
            "print first",
            bin,
        ]);
        c
    };
    let out = run_bounded_command(cmd, format!("{debugger} @ line {line}"));
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn debugger_reads_shadowed_local_by_innermost_binding() {
    require_codegen();
    let Some(dbg) = debugger() else {
        eprintln!("skip: no lldb/gdb on host");
        return;
    };

    let dir = workspace();
    let src = dir.path().join("shadow.hew");
    std::fs::write(&src, SHADOW_SRC).expect("write source");
    let binary = dir
        .path()
        .join(format!("shadow{}", std::env::consts::EXE_SUFFIX));

    let build = Command::new(hew_binary())
        .args([
            "build",
            "-g",
            src.to_str().unwrap(),
            "-o",
            binary.to_str().unwrap(),
        ])
        .output()
        .expect("run hew build -g");
    assert!(
        build.status.success(),
        "hew build -g failed:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(binary.exists(), "binary not produced");

    // Inner breakpoint (the `println(first)` line) must report the INNER
    // binding's value 43 (selector + 2), not the outer 42 (selector + 1).
    let inner = read_first_at_line(dbg, &binary, &src, 5);
    assert!(
        inner.contains("43"),
        "inner breakpoint must read the shadowed inner `first` = 43 (selector+2); \
         got:\n{inner}"
    );
    assert!(
        !inner.contains("= 42"),
        "inner breakpoint must NOT read the outer `first` = 42 (the leak bug);\n{inner}"
    );

    // Outer breakpoint (after the inner block closes) must report the OUTER
    // binding's value 42.
    let outer = read_first_at_line(dbg, &binary, &src, 7);
    assert!(
        outer.contains("42"),
        "outer breakpoint must read the outer `first` = 42 (selector+1); got:\n{outer}"
    );
}
