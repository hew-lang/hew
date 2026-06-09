mod support;

use std::io::Write;
use std::process::{Command, Stdio};

use support::{hew_binary, repo_root, require_codegen, strip_ansi};

/// Verify that `hew run --timeout` kills the entire process tree spawned by
/// the compiled Hew program, not just the root binary.
///
/// The Hew program uses `process.run()` to start a grandchild `sleep 98765`
/// via the shell and write its PID to a marker file, then spins forever.
/// After the timeout fires, both the compiled binary and the grandchild sleep
/// must be dead — proving that `killpg` is used rather than a bare `kill`.
///
/// Grandchild sleep inherits the compiled binary's process group (the Hew
/// runtime's `hew_process_run` spawns sh without `setpgid`, so background
/// jobs in non-interactive sh retain the parent's PGID). `BoundedChild` then
/// targets that entire group with `killpg(SIGKILL)` on timeout.
#[cfg(unix)]
#[ignore = "v0.5 native codegen lacks a supported long-running std::process fixture for this timeout assertion"]
#[test]
fn run_timeout_kills_grandchild_process_tree() {
    require_codegen();

    let dir = support::tempdir();
    let pid_file = dir.path().join("grandchild.pid");
    let hew_src = dir.path().join("grandchild_spinner.hew");

    // Write a Hew program that:
    //   1. Spawns a grandchild `sleep 98765` via the shell, writes its PID to
    //      the marker file (the shell exits immediately after; sleep stays in
    //      the compiled binary's process group).
    //   2. Loops forever so the timeout is guaranteed to fire.
    std::fs::write(
        &hew_src,
        format!(
            "import std::process;\n\
             fn main() {{\n\
             \x20   process.run(\"sh -c 'sleep 98765 & echo $! > {pid_file}'\");\n\
             \x20   var i = 0;\n\
             \x20   loop {{\n\
             \x20       i = i + 1;\n\
             \x20   }}\n\
             }}\n",
            pid_file = pid_file.display(),
        ),
    )
    .unwrap();

    // The --timeout is a hang watchdog, not a precision timer.  30 s gives the
    // compiled binary generous startup time (compilation + link + spawn) even
    // on a heavily loaded CI runner.  The program loops forever, so the
    // timeout always fires; the assertion is that the whole process group is
    // dead, not how long it took.
    let output = Command::new(hew_binary())
        .arg("run")
        .arg("--timeout")
        .arg("30")
        .arg(&hew_src)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "hew run --timeout should fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Error: program timed out after"),
        "expected timeout error in stderr, got: {stderr}",
    );

    // Give the OS a brief window to finish reaping processes.
    // 50 ms is sufficient on contemporary OSes; 300 ms was conservative overhead.
    std::thread::sleep(std::time::Duration::from_millis(50));

    // Poll for the PID file to exist rather than assuming it is present.
    // Under load during the test run, the Hew program's startup and shell
    // invocation may take longer than expected.
    let pid_file_exists = {
        let mut retries = 0;
        loop {
            if pid_file.exists() {
                break true;
            }
            if retries >= 20 {
                break false;
            }
            retries += 1;
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
    };
    assert!(
        pid_file_exists,
        "grandchild PID file should have been written before the timeout fired"
    );

    let pid_str = std::fs::read_to_string(&pid_file)
        .expect("grandchild PID file should have been written before the timeout fired");
    let grandchild_pid: u32 = pid_str
        .trim()
        .parse()
        .expect("grandchild PID file should contain a numeric PID");

    #[allow(
        clippy::cast_possible_wrap,
        reason = "PIDs fit in i32 on all supported Unix platforms"
    )]
    // SAFETY: `kill(pid, 0)` is a POSIX liveness probe — no signal is sent.
    let alive = unsafe { libc::kill(grandchild_pid as libc::pid_t, 0) } == 0;
    assert!(
        !alive,
        "grandchild PID {grandchild_pid} should be dead after process-group kill on timeout",
    );
}

#[test]
fn timeout_zero_is_rejected() {
    let output = Command::new(hew_binary())
        .args(["run", "--timeout", "0", "placeholder.hew"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: --timeout must be at least 1 second"));
}

#[ignore = "v0.5 native codegen lacks a supported infinite-loop fixture for this timeout assertion"]
#[test]
fn run_timeout_exit_code_is_non_zero() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("timeout_run.hew");
    std::fs::write(
        &path,
        "fn main() {\n    var i = 0;\n    loop {\n        i = i + 1;\n    }\n}\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg("--timeout")
        .arg("1")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: program timed out after 1s"));
}

/// On macOS, `hew run` must not emit ld64.lld "newer than target minimum"
/// deployment-target mismatch warnings.  These warnings appear when the LLVM
/// object emitted for the user's program carries a higher macOS SDK version
/// than the deployment target set at link time.
///
/// Regression test for the fix in `hew-codegen-rs`: native object emission
/// now uses `MACOSX_DEPLOYMENT_TARGET` (defaulting to `"13.0"`) instead of
/// the system default LLVM triple, so the emitted object's minimum-OS version
/// matches the linker's target minimum.
#[cfg(target_os = "macos")]
#[test]
fn native_run_emits_no_deployment_target_mismatch_warning() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("deployment_target_check.hew");
    std::fs::write(&path, "fn main() {}\n").expect("write source");

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .expect("invoke hew run");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("which is newer than target minimum"),
        "ld64.lld deployment-target mismatch warning found in stderr:\n{stderr}",
    );
}

#[test]
fn run_program_with_simple_arithmetic_succeeds() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("arithmetic_run.hew");
    std::fs::write(&path, "fn main() {\n    println(1 + 2);\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
}

#[test]
fn run_string_methods_smoke_matches_expected() {
    require_codegen();

    let source = repo_root().join("examples/string_methods_smoke.hew");
    let expected =
        std::fs::read_to_string(repo_root().join("examples/string_methods_smoke.expected"))
            .expect("read string_methods_smoke.expected");

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// W5-011 function-scope drop elaboration: a `string` returned from a user
/// function and an aliasing call result must be freed exactly once, never
/// twice. `id(s)` returns `s`'s buffer unretained, so `s` and the result
/// alias the same refcount-1 allocation; the drop elaborator excludes the
/// argument source from scope-exit drop and frees only the result. The
/// pre-W5-011 attempt dropped both and double-freed — the runtime's
/// `free_cstring` header-sentinel check aborts the process on a double-free,
/// so a clean exit with `done` is the runtime proof that the buffer is freed
/// exactly once. The structural single-drop proof lives in
/// `hew-mir/tests/elaborate.rs`
/// (`call_arg_source_excluded_so_call_result_is_freed_once`); this test is
/// the behavioural guard that the emitted native binary does not double-free.
#[test]
fn run_move_out_string_is_freed_once_no_double_free() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/move_out_no_double_free.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/move_out_no_double_free.expected"),
    )
    .expect("read move_out_no_double_free.expected");

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .unwrap();

    // A double-free aborts the process (SIGABRT) via the runtime's
    // `free_cstring` sentinel check, so `success()` is itself the proof.
    assert!(
        output.status.success(),
        "move_out_no_double_free should run cleanly (a double-free would abort); \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// W5-011 P3 alias-wrapper double-free guard: every alias site that moves a
/// heap-owning `string` into a persistent slot (tuple element, control-flow
/// join result, call argument, variant payload) must exclude the aliased
/// source from scope-exit drop. The fail-closed sole-owner derivation
/// (`derive_cow_sole_owner`) excludes any local read as a source operand
/// anywhere in the finalized instruction+terminator stream — which every one
/// of these aliased sources is. A regressed derivation that dropped an aliased
/// source in addition to its live owner would double-free the shared
/// refcount-1 buffer; the runtime's `free_cstring` header-sentinel check aborts
/// the process (SIGABRT) on a double-free, so a clean exit across many
/// iterations is the behavioural proof. The structural exclusion proofs live in
/// `hew-mir/tests/elaborate.rs` (the alias-site regression battery); this test
/// guards that the emitted native binary frees each shared buffer exactly once.
#[test]
fn run_alias_wrappers_no_double_free() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/alias_wrappers_no_double_free.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/alias_wrappers_no_double_free.expected"),
    )
    .expect("read alias_wrappers_no_double_free.expected");

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .unwrap();

    // A double-free aborts the process (SIGABRT) via the runtime's
    // `free_cstring` sentinel check, so `success()` is itself the proof.
    assert!(
        output.status.success(),
        "alias_wrappers_no_double_free should run cleanly (a double-free would \
         abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// W5-011 P3 destructure-binder double-free guard: a `match`-destructured
/// enum payload binds a fresh `string` local that aliases the parent's
/// refcount-1 buffer with no retain. Two destructures of the same value each
/// bind such a local; admitting either binder to a scope-exit `hew_string_drop`
/// would double-free the shared buffer. The fail-closed sole-owner derivation
/// (`derive_cow_sole_owner`) seeds projection-alias taint on the destination
/// of any `Move` from an interior projection (`MachineVariant` / `EnumVariant`
/// / `GenState`), so each binder is excluded. A regressed derivation that
/// admitted a binder would double-free; the runtime's `free_cstring` sentinel
/// aborts (SIGABRT) on a double-free, so a clean exit is the behavioural proof.
/// The structural exclusion proofs live in the `cow_sole_owner_derivation`
/// unit tests (hew-mir/src/lower.rs); this test guards the emitted native
/// binary.
#[test]
fn run_destructure_payload_no_double_free() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/accept/destructure_payload_no_double_free.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/destructure_payload_no_double_free.expected"),
    )
    .expect("read destructure_payload_no_double_free.expected");

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "destructure_payload_no_double_free should run cleanly (a double-free \
         would abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// W5-011 leak guard: a heap-owning `string` local that never escapes is
/// freed at function scope exit, so a tight loop that allocates one such
/// local per iteration must run in bounded memory. Before W5-011 these
/// helper-locals leaked (the buffer was never freed). This test asserts the
/// program completes and prints `done` across many iterations; the bounded-RSS
/// proof (identical peak RSS at 100k vs 2M iterations) is recorded in the
/// lane's validation evidence.
#[test]
fn run_fn_local_string_is_dropped_bounded_memory() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/fn_local_string_dropped.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/fn_local_string_dropped.expected"),
    )
    .expect("read fn_local_string_dropped.expected");

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "fn_local_string_dropped should run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// W5-011 P3 actor-context drop-safety guard: a heap-owning `string` moved into
/// an actor mailbox must NOT be scope-dropped by the sender on any exit path.
/// The mailbox takes ownership of the buffer (no retain-on-send on the M-COW
/// spine), so a sender that also scope-dropped it would free a buffer the live
/// mailbox still owns — a use-after-free on the receiving side or a double-free
/// when the handler later releases it. The fail-closed sole-owner derivation
/// excludes the sent string because the send surfaces its backing local as a
/// terminator/instr source operand (`terminator_source_places` /
/// `instr_source_places`). A double-free trips the runtime's `free_cstring`
/// sentinel (SIGABRT); a clean exit across many sends is the behavioural proof.
#[test]
fn run_actor_sent_string_not_double_freed() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/accept/actor_sent_string_not_double_freed.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/actor_sent_string_not_double_freed.expected"),
    )
    .expect("read actor_sent_string_not_double_freed.expected");

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .unwrap();

    // A double-free aborts the process (SIGABRT) via the runtime's
    // `free_cstring` sentinel check, so `success()` is itself the proof.
    assert!(
        output.status.success(),
        "actor_sent_string_not_double_freed should run cleanly (a double-free \
         would abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Stdin round-trip through `std::io`. Guards against the regression that
/// shipped before this test existed: extern declarations in imported stdlib
/// modules failed to register in HIR's `fn_registry`, so any program that did
/// `import std::io; io.read_line()` errored at HIR with `UnresolvedSymbol`.
/// The fix wired the imported-module pre-pass and fourth pass to lower
/// `Item::ExternBlock` entries (`hew-hir/src/lower.rs`); this test compiles
/// AND RUNS a program that exercises both `read_line` and `write`, asserting
/// the byte-level round-trip, not just that the IR contains the declarations.
#[test]
fn run_imports_std_io_and_round_trips_stdin_to_stdout() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("echo_stdin.hew");
    std::fs::write(
        &hew_src,
        "import std::io;\n\
         \n\
         fn main() {\n\
         \x20   let line = io.read_line();\n\
         \x20   io.write(\"echo: \");\n\
         \x20   io.write(line);\n\
         \x20   io.write(\"\\n\");\n\
         }\n",
    )
    .unwrap();

    let mut child = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    {
        let mut stdin = child.stdin.take().expect("stdin should be piped");
        stdin.write_all(b"hello from stdin\n").unwrap();
    }

    let output = child.wait_with_output().unwrap();
    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "echo: hello from stdin\n",
    );
}

/// F-string interpolation routes built-in interpolants (`i64`, `string`) through
/// the `Display` lang-item substrate added in this slice.  This is the gate
/// program from the L3 brief — it must compile-and-run, not just type-check.
/// Regression anchor for the `string_concat` catalog entry plus the
/// `Expr::InterpolatedString` HIR lowering arm (`hew-hir/src/lower.rs`).
#[test]
fn run_fstring_interpolates_primitives_via_display() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("fstring_primitives.hew");
    std::fs::write(
        &hew_src,
        "import std::io;\n\
         \n\
         fn main() {\n\
         \x20   let x: i64 = 42;\n\
         \x20   let s: string = \"hi\";\n\
         \x20   println(f\"value is {x}, msg is {s}\");\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "value is 42, msg is hi\n",
    );
}

/// Companion to the primitive gate: an interpolated string that mixes
/// `bool` and `i64` interpolants exercises a second pair of built-in
/// `impl Display for <primitive>` entries (`to_string_bool`,
/// `to_string_i64`) so the catalog routing isn't silently specialised to
/// the gate case.  Asserts exact stdout to lock in `bool` rendering as
/// `true`/`false` (not `1`/`0`).
#[test]
fn run_fstring_interpolates_bool_and_int() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("fstring_bool_int.hew");
    std::fs::write(
        &hew_src,
        "import std::io;\n\
         \n\
         fn main() {\n\
         \x20   let b: bool = true;\n\
         \x20   let x: i64 = 100;\n\
         \x20   println(f\"flag={b} count={x}\");\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "flag=true count=100\n",
    );
}

/// Negative gate: interpolating a value whose type has no `impl Display`
/// must fail at *check time* with a clear diagnostic, not as an opaque
/// HIR/MIR unresolved-symbol error.  Regression anchor for
/// `Checker::require_display_impl` (`hew-types/src/check/expressions.rs`).
#[test]
fn run_fstring_rejects_type_without_display_impl() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("fstring_missing_display.hew");
    std::fs::write(
        &hew_src,
        "type Foo {\n\
         \x20   x: i64,\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let f = Foo { x: 1 };\n\
         \x20   println(f\"foo is {f}\");\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "hew run should fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        combined.contains("does not implement `Display`"),
        "expected `Display`-missing diagnostic; got: {combined}",
    );
    assert!(
        combined.contains("Foo"),
        "expected the offending type name in the diagnostic; got: {combined}",
    );
}

/// Positive gate: a user-defined `type` with its own `impl Display` is
/// dispatched to by f-string interpolation, end-to-end.  This proves the
/// Display lang-item substrate is genuinely user-extensible — not a
/// hard-coded primitive shim.  Also exercises structural `BitCopy` inference
/// (an unmarked record of `BitCopy` fields lowers cleanly past MIR's
/// value-class gate).
#[test]
fn run_fstring_dispatches_user_defined_display() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("fstring_user_display.hew");
    std::fs::write(
        &hew_src,
        "import std::io;\n\
         \n\
         type Point { x: i64; }\n\
         \n\
         impl Display for Point {\n\
         \x20   fn fmt(p: Point) -> string { f\"Point({p.x})\" }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let p = Point { x: 7 };\n\
         \x20   println(f\"got {p}\");\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "got Point(7)\n",);
}
