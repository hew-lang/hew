//! Live-debugger e2e for `hew build -g`: build a real binary, drive `lldb`
//! (or `gdb`) in batch mode to a breakpoint, and assert the locals the debugger
//! reports. This is the gate a static-IR review cannot stand in for — it proves
//! the END-TO-END story (codegen → linker → DWARF → debugger) a backend dev
//! actually experiences. It caught what the IR-level harness could not: a
//! shadowed inner `let first` whose breakpoint reported the OUTER binding's
//! value, and an `optnone`-less `-O0` body whose slot lagged its value.
//!
//! A missing debugger is a HARD FAILURE, not a skip: a skip silently passed
//! for as long as no CI runner shipped gdb/lldb, reporting success while
//! proving nothing.
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

/// Scalar AND reference locals reassigned AFTER a suspend point. The string
/// reassignment lowers through the raii-null-after-move sequence (release the
/// prior value, null the slot, store the replacement), so an anchor that
/// blindly tracked every store would carry the interior null into the
/// debugger — `s = 0x0` on the very line a user breaks at. An honesty fixture
/// must cover the type classes where dishonesty is possible: a scalar-only
/// fixture missed exactly this.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
const SLEEP_REASSIGN_SRC: &str = "\
actor Worker {
    receive fn compute(n: i64) -> i64 {
        var k: i64 = n;
        var s: string = \"before\";
        sleep(10ms);
        k = n + 1;
        s = \"after\";
        println(s);
        println(k);
        k
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

/// A reference local CONDITIONALLY reassigned after the suspend, on a branch
/// the run never takes (`n = 7`). Value-anchoring pays for its honesty with
/// availability here: the pre-suspend "before" range dies at the suspend, and
/// no post-suspend anchor executes, so the local reads unavailable even
/// though its value is live in the frame. That is honest-absent — acceptable
/// under A305 — and this fixture pins that it stays absent-or-correct and can
/// never silently become a wrong value. Recovering availability belongs to
/// the full-fidelity follow-on (see `emit_honest_coroutine_local_locations`).
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
const SLEEP_CONDITIONAL_SRC: &str = "\
actor Worker {
    receive fn compute(n: i64) -> i64 {
        var s: string = \"before\";
        sleep(10ms);
        if n > 100 {
            s = \"high\";
        }
        println(n);
        n
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

/// First available batch debugger, preferring each platform's native one.
/// `lldb -b -o ...` and `gdb --batch -ex ...` both run a script
/// non-interactively.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn debugger() -> Option<&'static str> {
    #[cfg(target_os = "linux")]
    let candidates = ["gdb", "lldb"];
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

/// A missing debugger is a hard failure, not a skip. A skip here silently
/// passed for as long as no CI runner shipped gdb/lldb — reporting success
/// while proving nothing, the same failure class as debug info that prints a
/// wrong value. Every platform this suite compiles on must provide a debugger
/// (CI installs gdb on Linux and FreeBSD; macOS runners ship lldb with the
/// Xcode command-line tools).
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn require_debugger() -> &'static str {
    debugger().unwrap_or_else(|| {
        panic!(
            "no debugger found on {}: install gdb (Linux/FreeBSD) or lldb \
             (macOS, via the Xcode command-line tools) — this live-debugger \
             proof must run, a skip would report success while proving nothing",
            std::env::consts::OS
        )
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
    let dbg = require_debugger();

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
    let dbg = require_debugger();
    let fixture = build_debug_fixture("await-locals", AWAIT_SRC);
    let src = debugger_quote(fixture.src.to_str().expect("src path utf8"));
    let bin = fixture.binary.to_str().expect("bin path utf8");
    // lldb uses hardware breakpoints to avoid patching coroutine code while
    // runtime threads execute it. gdb cannot: the CI Linux runner (and every
    // Linux host probed) reports "No hardware breakpoint support in the
    // target" for `hbreak` unconditionally — the debug registers are not
    // available to gdb's ptrace path there — so the gdb branch uses software
    // breakpoints instead.
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
            &format!("break {src}:10"),
            "-ex",
            &format!("break {src}:12"),
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
    let dbg = require_debugger();
    let fixture = build_debug_fixture("sleep-honest-locals", SLEEP_SRC);
    let src = debugger_quote(fixture.src.to_str().expect("src path utf8"));
    let bin = fixture.binary.to_str().expect("bin path utf8");
    // Stop 1 (line 4, ramp, pre-suspend): `x` stored, `y` not. Stop 2 (line 7,
    // `.resume`, post-suspend): both stored. lldb uses hardware breakpoints
    // to avoid patching coroutine code while runtime threads execute it; gdb
    // falls back to software breakpoints (see the await test above for why).
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
            &format!("break {src}:4"),
            "-ex",
            &format!("break {src}:7"),
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

/// Marker printed between the two debugger stops so each stop's reads can be
/// asserted in isolation — a single concatenated transcript cannot tell
/// "unavailable at stop 1, value at stop 2" from the reverse.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
const STOP_SPLIT: &str = "HEW_STOP_SPLIT";

/// gdb `printf` command for the string local `s`, under `set language c`.
/// Prints the pointer's raw hex AND its dereferenced content in one line
/// shaped like lldb's native `s = 0x… "content"` render, so the shared
/// `s_pointer_reads`/`s_reads_null`/`s_reads_unavailable` helpers below work
/// unmodified against either debugger's transcript. `printf` flushes the
/// literal `s = ` prefix before evaluating the arguments, so an unavailable
/// `s` still yields a line starting with `s = ` (followed by gdb's own
/// "optimized out" wording) rather than losing the prefix entirely.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
const GDB_PRINT_S: &str = "printf \"s = 0x%lx \\\"%s\\\"\\n\", (unsigned long)s, (char*)s";

/// A debugger's read of a pointer variable renders as `s = 0x…` (`frame
/// variable` and `info locals` both do). Requiring the `0x` is what keeps
/// these matches meaningful: the debuggee's own stdout AND the source listing
/// the debugger echoes at a stop both contain `s = "after"`-shaped text, but
/// neither ever contains a pointer render.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn s_pointer_reads(section: &str) -> Vec<&str> {
    section
        .lines()
        .filter(|line| line.contains("s = 0x"))
        .collect()
}

/// Whether any pointer read of `s` in this section is the value zero —
/// matched semantically (parse the hex digits) rather than against one
/// debugger's exact rendering width.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn s_reads_null(section: &str) -> bool {
    s_pointer_reads(section).iter().any(|line| {
        line.split_once("s = 0x").is_some_and(|(_, rest)| {
            let hex: String = rest.chars().take_while(char::is_ascii_hexdigit).collect();
            !hex.is_empty() && hex.chars().all(|c| c == '0')
        })
    })
}

/// Whether this section reports `s` as unavailable (lldb) / optimized out
/// (gdb), line-scoped to an `s = ` read.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn s_reads_unavailable(section: &str) -> bool {
    section.lines().any(|line| {
        line.contains("s = ") && (line.contains("not available") || line.contains("optimized out"))
    })
}

/// Build the two-stop debugger script for the reassignment fixture: stop at
/// line 8 and line 9, read `k` and `s` at each, with [`STOP_SPLIT`] printed
/// between the stops. lldb uses hardware breakpoints; gdb falls back to
/// software breakpoints (see the await test's comment for why).
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
fn two_stop_reassign_cmd(dbg: &str, src: &str, bin: &str) -> Command {
    if dbg == "lldb" {
        let mut command = Command::new("lldb");
        command.args([
            "-b",
            "-o",
            &format!("breakpoint set -H --file {src} --line 8"),
            "-o",
            &format!("breakpoint set -H --file {src} --line 9"),
            "-o",
            "run",
            "-o",
            "frame variable k s",
            "-o",
            &format!("script print(\"{STOP_SPLIT}\")"),
            "-o",
            "breakpoint disable 1",
            "-o",
            "continue",
            "-o",
            "frame variable k s",
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
            &format!("break {src}:8"),
            "-ex",
            &format!("break {src}:9"),
            "-ex",
            "run",
            // gdb auto-detects the DWARF source language as Rust from the
            // producer; in Rust mode `print`/`info locals` render a `*mut u8`
            // (Hew's string ABI pointer) as a bare address, never dereferenced
            // into content — unlike lldb, which prints the string. Forcing C
            // mode lets an explicit `(char*)` cast trigger gdb's string
            // rendering.
            "-ex",
            "set language c",
            "-ex",
            "printf \"k = %ld\\n\", k",
            "-ex",
            GDB_PRINT_S,
            "-ex",
            &format!("echo {STOP_SPLIT}\\n"),
            "-ex",
            "disable 1",
            "-ex",
            "continue",
            "-ex",
            "printf \"k = %ld\\n\", k",
            "-ex",
            GDB_PRINT_S,
            bin,
        ]);
        command
    }
}

/// A305, reference/type-class coverage: a string local reassigned after a
/// suspend point must never read as the raii-null-after-move interior null
/// (`s = 0x0`) — it reports unavailable until the replacement value's range
/// begins, then the real value. The reassigned scalar must only ever read its
/// true post-assignment value or unavailable.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
#[test]
fn debugger_never_reads_reassigned_reference_local_as_interior_null() {
    require_codegen();
    let dbg = require_debugger();
    let fixture = build_debug_fixture("sleep-reassign-locals", SLEEP_REASSIGN_SRC);
    let src = debugger_quote(fixture.src.to_str().expect("src path utf8"));
    let bin = fixture.binary.to_str().expect("bin path utf8");
    // Stop 1: `println(s)` (line 8), immediately after both reassignments.
    // Stop 2: `println(k)` (line 9), where `s`'s replacement range has begun.
    let cmd = two_stop_reassign_cmd(dbg, &src, bin);
    let out = run_bounded_command(cmd, format!("{dbg} reassigned reference local"));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "{dbg} failed while debugging reassignment handler:\n{text}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let (stop1, stop2) = text
        .split_once(STOP_SPLIT)
        .unwrap_or_else(|| panic!("transcript is missing the stop marker:\n{text}"));

    for (label, section) in [("stop 1", stop1), ("stop 2", stop2)] {
        // The interior null must never be presented as `s`'s value, at
        // either stop, matched semantically (any all-zero hex render).
        assert!(
            !s_reads_null(section),
            "`s` must never read as the raii interior null at {label}:\n{text}"
        );
        // The released pre-suspend value shown after the reassignment line
        // would equally be a lie.
        assert!(
            !s_pointer_reads(section)
                .iter()
                .any(|line| line.contains("before")),
            "`s` must never read the released pre-suspend \"before\" at {label}:\n{text}"
        );
        // The reassigned scalar reads its true value at both stops — never
        // any other number.
        let wrong_k = section.lines().any(|line| {
            line.split_once("k = ")
                .map(|(_, v)| v.trim())
                .is_some_and(|v| {
                    v.chars()
                        .next()
                        .is_some_and(|c| c.is_ascii_digit() || c == '-')
                        && v != "8"
                })
        });
        assert!(
            !wrong_k,
            "`k` must never read as a value other than its stored 8 at {label}:\n{text}"
        );
        assert!(
            section.contains("k = 8"),
            "reassigned `k` must read 8 at {label}:\n{text}"
        );
    }

    // Stop 1 sits on the reassignment's own line: whether the replacement
    // range has begun at that PC is machine-dependent, so the honest states
    // are exactly "unavailable" or the real replacement — each matched in its
    // specific line-scoped form, never satisfiable by stdout or the listing.
    let stop1_after = s_pointer_reads(stop1)
        .iter()
        .any(|line| line.contains("after"));
    assert!(
        s_reads_unavailable(stop1) || stop1_after,
        "`s` must be unavailable or the real replacement at stop 1:\n{text}"
    );
    // By stop 2 (the next statement) the replacement range HAS begun: the
    // read must be the real value, not absence.
    assert!(
        s_pointer_reads(stop2)
            .iter()
            .any(|line| line.contains("after")),
        "`s` must read the replacement string at stop 2:\n{text}"
    );
}

/// A305, conditional-reassignment coverage: with the reassigning branch NOT
/// taken, the local reads unavailable (the honest cost of value-anchoring —
/// its pre-suspend range died at the suspend) or, should a future change
/// recover availability, its true untouched value. It must never read the
/// branch's never-stored value, an interior null, or any other fabrication.
#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
#[test]
fn debugger_reports_untaken_conditional_reassignment_unavailable_not_wrong() {
    require_codegen();
    let dbg = require_debugger();
    let fixture = build_debug_fixture("sleep-conditional-locals", SLEEP_CONDITIONAL_SRC);
    let src = debugger_quote(fixture.src.to_str().expect("src path utf8"));
    let bin = fixture.binary.to_str().expect("bin path utf8");
    // One stop at `println(n)` (line 8), reached via the untaken branch.
    // lldb uses hardware breakpoints; gdb falls back to software breakpoints
    // (see the await test's comment for why).
    let cmd = if dbg == "lldb" {
        let mut command = Command::new("lldb");
        command.args([
            "-b",
            "-o",
            &format!("breakpoint set -H --file {src} --line 8"),
            "-o",
            "run",
            "-o",
            "frame variable n s",
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
            &format!("break {src}:8"),
            "-ex",
            "run",
            // See `two_stop_reassign_cmd`: force C mode so the `(char*)` cast
            // in `GDB_PRINT_S` dereferences the string pointer instead of
            // printing a bare address.
            "-ex",
            "set language c",
            "-ex",
            "info args",
            "-ex",
            GDB_PRINT_S,
            bin,
        ]);
        command
    };
    let out = run_bounded_command(cmd, format!("{dbg} untaken conditional reassignment"));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "{dbg} failed while debugging conditional handler:\n{text}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    // The never-executed branch's value must not be presented; pointer-render
    // scoped so the source listing's `s = \"high\"` line cannot trip it.
    assert!(
        !s_pointer_reads(&text)
            .iter()
            .any(|line| line.contains("high")),
        "`s` must never read the never-stored branch value \"high\":\n{text}"
    );
    assert!(
        !s_reads_null(&text),
        "`s` must never read as an interior null:\n{text}"
    );
    // The two honest states: unavailable (today's behaviour — the anchor
    // range died at the suspend), or the true untouched \"before\" if a
    // future full-fidelity pass recovers availability.
    let s_reads_before = s_pointer_reads(&text)
        .iter()
        .any(|line| line.contains("before"));
    assert!(
        s_reads_unavailable(&text) || s_reads_before,
        "`s` must read unavailable (or its true untouched value):\n{text}"
    );
}

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
#[test]
fn debugger_names_suspended_actor_handler_frame_at_runtime_boundary() {
    require_codegen();
    let dbg = require_debugger();
    let fixture = build_debug_fixture("handler-backtrace", HANDLER_SRC);
    let src = debugger_quote(fixture.src.to_str().expect("src path utf8"));
    let bin = fixture.binary.to_str().expect("bin path utf8");
    // lldb uses hardware breakpoints; gdb falls back to software breakpoints
    // (see the await test's comment for why).
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
            &format!("break {src}:13"),
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
    let dbg = require_debugger();
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
            // Older lldb releases (e.g. the Xcode 15.x toolchain on GitHub's
            // macos-14 runner) default `target.max-children-depth` shallower
            // than newer ones (Xcode 16.3's lldb-2100 defaults to 5): without
            // an explicit --depth, the nested Payload record inside the
            // active variant renders as an elided `{...}` instead of
            // `(code = 7)`, and this assertion goes looking for text that was
            // never printed. Forcing a depth deep enough for this fixture's
            // nesting (Status -> $variants$ -> $variant$N -> value -> field)
            // makes the rendering version-stable.
            "-o",
            "frame variable status --depth 10",
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
            // gdb auto-detects the DWARF source language as Rust from the
            // producer; in Rust mode `print` renders record fields as
            // `Payload {code: 7}` instead of the C-style `code = 7` this
            // assertion expects. Forcing C mode makes the rendering match
            // the other gdb assertions in this suite (see `set language c`
            // above).
            "-ex",
            "set language c",
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
