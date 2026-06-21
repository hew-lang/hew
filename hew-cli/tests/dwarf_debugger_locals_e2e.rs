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
//!
//! # Platform scope
//!
//! This suite is compiled and run on **Linux and macOS only** — the platforms
//! where gdb/lldb consume DWARF from ELF and Mach-O objects respectively.
//!
//! `hew build -g` emits DWARF debug info on all platforms (ELF on Linux, Mach-O
//! on macOS, PE/COFF on Windows). gdb and lldb read DWARF from ELF and Mach-O
//! reliably. On Windows the native debug-info format is CodeView/PDB; lldb-on-
//! Windows does not fully read DWARF embedded in a PE/COFF binary, so the
//! debugger-read assertions here would produce empty output and false-fail.
//!
//! The DWARF *emission* is tested cross-platform by `hew-codegen-rs`'s
//! `dwarf_emitted_object` suite (using dwarfdump/llvm-dwarfdump on the object
//! file directly) — those tests continue to run on all platforms.
//!
//! Windows-native debuggability (CodeView/PDB or a DWARF-in-PE path that
//! lldb-on-Windows fully reads) is tracked as a follow-up; see the linked issue.

mod support;

// The live-debugger helpers and test only compile on the DWARF-debugger platforms
// (Linux / macOS). On Windows, `hew build -g` emits DWARF into PE/COFF but
// lldb-on-Windows does not fully read DWARF-in-PE — the local reads return empty.
// Windows-native debuggability (CodeView/PDB) is a tracked follow-up.
// See: https://github.com/hew-lang/hew/issues/2117
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::path::Path;
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::process::Command;

#[cfg(any(target_os = "linux", target_os = "macos"))]
use support::{hew_binary, repo_root, require_codegen, run_bounded_command};

#[cfg(any(target_os = "linux", target_os = "macos"))]
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

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn workspace() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("dwarf-dbg-hew-")
        .tempdir_in(repo_root())
        .expect("temp dir")
}

/// First available batch debugger. `lldb -b -o ...` and `gdb --batch -ex ...`
/// both run a script non-interactively.
#[cfg(any(target_os = "linux", target_os = "macos"))]
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
#[cfg(any(target_os = "linux", target_os = "macos"))]
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

#[cfg(any(target_os = "linux", target_os = "macos"))]
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
