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
//! This suite is compiled and run on **Linux, macOS, and FreeBSD** — the
//! platforms where gdb/lldb consume DWARF from ELF and Mach-O objects.
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
//! Windows-native debuggability is covered separately by the Windows PDB CI
//! check because lldb-on-Windows does not fully read DWARF-in-PE.

mod support;

// The live-debugger helpers and test only compile on the DWARF-debugger platforms
// (Linux / macOS / FreeBSD). On Windows, `hew build -g` emits DWARF into PE/COFF but
// lldb-on-Windows does not fully read DWARF-in-PE — the local reads return empty.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
use std::path::Path;
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
use std::process::Command;

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
use support::{hew_binary, require_codegen, run_bounded_command, tempdir};

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
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

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn workspace() -> tempfile::TempDir {
    tempdir()
}

/// First available batch debugger. `lldb -b -o ...` and `gdb --batch -ex ...`
/// both run a script non-interactively.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn debugger() -> Option<&'static str> {
    #[cfg(target_os = "linux")]
    let candidates = ["gdb"];
    #[cfg(target_os = "freebsd")]
    let candidates = ["gdb", "lldb"];
    #[cfg(target_os = "macos")]
    let candidates = ["lldb", "gdb"];
    candidates.into_iter().find(|d| {
        Command::new(d)
            .arg("--version")
            .output()
            .is_ok_and(|o| o.status.success())
    })
}

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn debugger_quote(path: &str) -> String {
    let mut quoted = String::with_capacity(path.len() + 2);
    quoted.push('"');
    for ch in path.chars() {
        match ch {
            '\\' => quoted.push_str("\\\\"),
            '"' => quoted.push_str("\\\""),
            _ => quoted.push(ch),
        }
    }
    quoted.push('"');
    quoted
}

/// Run the debugger to a breakpoint on `line` and return its stdout. Reads the
/// local `first` at that stop.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn read_first_at_line(debugger: &str, binary: &Path, src: &Path, line: u32) -> String {
    let src = src.to_str().expect("src path utf8");
    let quoted_src = debugger_quote(src);
    let bin = binary.to_str().expect("bin path utf8");
    let cmd = if debugger == "lldb" {
        let mut c = Command::new("lldb");
        c.args([
            "-b",
            "-o",
            &format!("breakpoint set --file {quoted_src} --line {line}"),
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
            &format!("break {quoted_src}:{line}"),
            "-ex",
            "run",
            "-ex",
            "print first",
            bin,
        ]);
        c
    };
    let out = run_bounded_command(cmd, format!("{debugger} @ line {line}"));
    let mut text = String::from_utf8_lossy(&out.stdout).into_owned();
    if !out.stderr.is_empty() {
        text.push_str("\n[stderr]\n");
        text.push_str(&String::from_utf8_lossy(&out.stderr));
    }
    text
}

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
#[test]
fn debugger_reads_shadowed_local_by_innermost_binding() {
    require_codegen();
    let dbg = debugger().unwrap_or_else(|| {
        #[cfg(target_os = "linux")]
        panic!("no gdb found on Linux CI; install the gdb package");
        #[cfg(target_os = "freebsd")]
        panic!(
            "no gdb or lldb found on FreeBSD CI; install the gdb package or LLVM's lldb package"
        );
        #[cfg(target_os = "macos")]
        {
            eprintln!("skip: no lldb/gdb on host");
            ""
        }
    });
    if dbg.is_empty() {
        return;
    }

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
