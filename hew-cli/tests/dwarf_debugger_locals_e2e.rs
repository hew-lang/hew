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
const AWAIT_SRC: &str = "\
actor Calculator {
    receive fn value(n: i64) -> i64 {
        n + 1
    }
}

fn main() {
    let before: i64 = 41;
    let calculator = spawn Calculator;
    let reply = await calculator.value(before);
    let after = before + 1;
    println(after);
    match reply {
        Ok(value) => println(value),
        Err(_) => println(-1),
    }
}
";

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
const HANDLER_SRC: &str = "\
actor Source {
    receive fn value() -> i64 {
        42
    }
}

actor Handler {
    let source: LocalPid<Source>;

    receive fn run() -> i64 {
        let before: i64 = 7;
        let reply = await source.value();
        let after = before + 1;
        println(after);
        match reply {
            Ok(value) => value,
            Err(_) => -1,
        }
    }
}

fn main() {
    let source = spawn Source;
    let handler = spawn Handler(source: source);
    let _ = await handler.run();
}
";

/// A receive handler that assigns a local AFTER a real suspend point. The
/// handler cannot take `optnone` (`CoroSplit` must run), so `y`'s slot store is
/// free to lag its source line — the case where a whole-scope `dbg.declare`
/// would let the debugger print stale garbage as if it were `y`.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
const SLEEP_SRC: &str = "\
actor Worker {
    receive fn compute(n: i64) -> i64 {
        let x: i64 = n + 1;
        sleep(10ms);
        let y: i64 = x + 34;
        println(y);
        y
    }
}

fn main() {
    let worker = spawn Worker;
    match await worker.compute(7) {
        Ok(value) => println(value),
        Err(_) => println(-1),
    }
}
";

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
const ENUM_SRC: &str = "\
record Payload {
    code: i64,
}

enum Status {
    Idle;
    Packet(Payload);
}

fn main() {
    let status = Status::Packet(Payload { code: 7 });
    println(1);
}
";

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn workspace() -> tempfile::TempDir {
    tempdir()
}

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
struct DebugFixture {
    _dir: tempfile::TempDir,
    src: std::path::PathBuf,
    binary: std::path::PathBuf,
}

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn build_debug_fixture(slug: &str, source: &str) -> DebugFixture {
    let dir = workspace();
    let src = dir.path().join(format!("{slug}.hew"));
    std::fs::write(&src, source).expect("write source");
    let binary = dir
        .path()
        .join(format!("{slug}{}", std::env::consts::EXE_SUFFIX));
    let build = Command::new(hew_binary())
        .args([
            "build",
            "-g",
            src.to_str().expect("source path utf8"),
            "-o",
            binary.to_str().expect("binary path utf8"),
        ])
        .output()
        .expect("run hew build -g");
    assert!(
        build.status.success(),
        "hew build -g failed:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(binary.exists(), "binary not produced");
    DebugFixture {
        _dir: dir,
        src,
        binary,
    }
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

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
#[test]
fn debugger_hits_await_body_before_and_after_suspend_with_live_local() {
    require_codegen();
    let Some(dbg) = debugger() else {
        eprintln!("skip: no lldb/gdb on host");
        return;
    };
    let fixture = build_debug_fixture("await-locals", AWAIT_SRC);
    let src = debugger_quote(fixture.src.to_str().expect("src path utf8"));
    let bin = fixture.binary.to_str().expect("bin path utf8");
    // Hardware breakpoints avoid patching coroutine code while runtime threads execute it.
    let cmd = if dbg == "lldb" {
        let mut command = Command::new("lldb");
        command.args([
            "-b",
            "-o",
            &format!("breakpoint set -H --file {src} --line 10"),
            "-o",
            &format!("breakpoint set -H --file {src} --line 12"),
            "-o",
            "run",
            "-o",
            "frame variable before",
            "-o",
            "continue",
            "-o",
            "frame variable before after",
            "-o",
            "quit",
            bin,
        ]);
        command
    } else {
        let mut command = Command::new("gdb");
        command.args([
            "--batch",
            "-ex",
            &format!("hbreak {src}:10"),
            "-ex",
            &format!("hbreak {src}:12"),
            "-ex",
            "run",
            "-ex",
            "print before",
            "-ex",
            "continue",
            "-ex",
            "print before",
            "-ex",
            "print after",
            bin,
        ]);
        command
    };
    let out = run_bounded_command(cmd, format!("{dbg} await pre/post suspend"));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "{dbg} failed while debugging await body:\n{text}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        text.contains("before = 41") || text.contains("$1 = 41"),
        "pre-suspend breakpoint must expose `before = 41`:\n{text}"
    );
    assert!(
        text.contains("after = 42") || text.contains("$3 = 42"),
        "post-suspend breakpoint must expose `after = 42`:\n{text}"
    );
}

/// A305: a debugger stopped where a local's store has not executed must report
/// the local UNAVAILABLE — never a confidently wrong value. Before the honest
/// location pass, `y` here read as stack garbage at the pre-suspend stop and
/// at the post-suspend stop before its assignment; the declared slot genuinely
/// held the wrong bits at those PCs.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
#[test]
fn debugger_reports_unstored_post_suspend_local_unavailable_not_wrong() {
    require_codegen();
    let Some(dbg) = debugger() else {
        eprintln!("skip: no lldb/gdb on host");
        return;
    };
    let fixture = build_debug_fixture("sleep-honest-locals", SLEEP_SRC);
    let src = debugger_quote(fixture.src.to_str().expect("src path utf8"));
    let bin = fixture.binary.to_str().expect("bin path utf8");
    // Stop 1 (line 4, ramp, pre-suspend): `x` stored, `y` not. Stop 2 (line 7,
    // `.resume`, post-suspend): both stored. Hardware breakpoints avoid
    // patching coroutine code while runtime threads execute it.
    let cmd = if dbg == "lldb" {
        let mut command = Command::new("lldb");
        command.args([
            "-b",
            "-o",
            &format!("breakpoint set -H --file {src} --line 4"),
            "-o",
            &format!("breakpoint set -H --file {src} --line 7"),
            "-o",
            "run",
            "-o",
            "frame variable x y",
            // The line-4 breakpoint also matches the `.resume` re-entry PC;
            // disable it so the next stop is the line-7 one.
            "-o",
            "breakpoint disable 1",
            "-o",
            "continue",
            "-o",
            "frame variable x y",
            "-o",
            "quit",
            bin,
        ]);
        command
    } else {
        let mut command = Command::new("gdb");
        command.args([
            "--batch",
            "-ex",
            &format!("hbreak {src}:4"),
            "-ex",
            &format!("hbreak {src}:7"),
            "-ex",
            "run",
            "-ex",
            "info locals",
            "-ex",
            "disable 1",
            "-ex",
            "continue",
            "-ex",
            "info locals",
            bin,
        ]);
        command
    };
    let out = run_bounded_command(cmd, format!("{dbg} honest post-suspend local"));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "{dbg} failed while debugging sleep handler:\n{text}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    // Pre-suspend stop: the stored local reads its real value.
    assert!(
        text.contains("x = 8"),
        "pre-suspend breakpoint must read the stored `x = 8`:\n{text}"
    );
    // The unstored local must be reported absent, and must never surface a
    // fabricated numeric value at the pre-store stop. `y`'s only true value is
    // 42, and 42 may legitimately appear at the second (post-store) stop — so
    // reject any `y = <digits>` line that is not exactly 42.
    assert!(
        text.contains("not available") || text.contains("optimized out"),
        "unstored `y` must be reported unavailable/optimized-out at the pre-store stop:\n{text}"
    );
    let wrong_y = text.lines().any(|line| {
        line.split_once("y = ")
            .map(|(_, v)| v.trim())
            .is_some_and(|v| {
                v.chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_digit() || c == '-')
                    && v != "42"
            })
    });
    assert!(
        !wrong_y,
        "`y` must never read as a value other than its stored 42:\n{text}"
    );
    // Post-store stop: the value is either honestly absent or the real 42 —
    // this fixture's codegen shape keeps it readable at the `y` return line.
    assert!(
        text.contains("y = 42") || text.contains("= 42"),
        "post-store breakpoint must read the stored `y = 42`:\n{text}"
    );
}

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
#[test]
fn debugger_names_suspended_actor_handler_frame_at_runtime_boundary() {
    require_codegen();
    let Some(dbg) = debugger() else {
        eprintln!("skip: no lldb/gdb on host");
        return;
    };
    let fixture = build_debug_fixture("handler-backtrace", HANDLER_SRC);
    let src = debugger_quote(fixture.src.to_str().expect("src path utf8"));
    let bin = fixture.binary.to_str().expect("bin path utf8");
    let cmd = if dbg == "lldb" {
        let mut command = Command::new("lldb");
        command.args([
            "-b",
            "-o",
            &format!("breakpoint set -H --file {src} --line 13"),
            "-o",
            "run",
            "-o",
            "bt",
            "-o",
            "quit",
            bin,
        ]);
        command
    } else {
        let mut command = Command::new("gdb");
        command.args([
            "--batch",
            "-ex",
            &format!("hbreak {src}:13"),
            "-ex",
            "run",
            "-ex",
            "backtrace",
            bin,
        ]);
        command
    };
    let out = run_bounded_command(cmd, format!("{dbg} handler backtrace"));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "{dbg} failed while reading handler backtrace:\n{text}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        text.contains("Handler__recv__run"),
        "backtrace must name the Hew handler frame:\n{text}"
    );
    assert!(
        text.contains("coro_resume") || text.contains("hew_cont_resume"),
        "backtrace must show the honest transition into the runtime coroutine \
         resume boundary:\n{text}"
    );
}

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
#[test]
fn debugger_renders_only_active_enum_variant_payload() {
    require_codegen();
    let Some(dbg) = debugger() else {
        eprintln!("skip: no lldb/gdb on host");
        return;
    };
    let fixture = build_debug_fixture("enum-render", ENUM_SRC);
    let src = debugger_quote(fixture.src.to_str().expect("src path utf8"));
    let bin = fixture.binary.to_str().expect("bin path utf8");
    let cmd = if dbg == "lldb" {
        let mut command = Command::new("lldb");
        command.args([
            "-b",
            "-o",
            &format!("breakpoint set --file {src} --line 12"),
            "-o",
            "run",
            "-o",
            "frame variable status",
            "-o",
            "quit",
            bin,
        ]);
        command
    } else {
        let mut command = Command::new("gdb");
        command.args([
            "--batch",
            "-ex",
            &format!("break {src}:12"),
            "-ex",
            "run",
            "-ex",
            "print status",
            bin,
        ]);
        command
    };
    let out = run_bounded_command(cmd, format!("{dbg} enum rendering"));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "{dbg} failed while rendering enum:\n{text}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        text.contains("Packet") && text.contains("code = 7"),
        "debugger must render the active Packet payload:\n{text}"
    );
    assert!(
        !text.contains("Idle =") && !text.contains("Idle {"),
        "debugger must not render the inactive Idle variant:\n{text}"
    );
}
