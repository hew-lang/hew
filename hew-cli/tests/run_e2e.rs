mod support;

use std::process::Command;
use std::time::{Duration, Instant};

use support::{hew_binary, repo_root, require_codegen, run_bounded_hew_run, strip_ansi};

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
    let mut command = Command::new(hew_binary());
    command
        .arg("run")
        .arg("--timeout")
        .arg("30")
        .arg(&hew_src)
        .current_dir(dir.path());
    let output = support::run_bounded_command(command, format!("hew run {}", hew_src.display()));

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
    let output = support::bounded_hew_command(
        ["run", "--timeout", "0", "placeholder.hew"],
        repo_root(),
        "hew run --timeout 0",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: --timeout must be at least 1 second"));
}

#[cfg(unix)]
#[test]
fn bounded_exec_helper_kills_infinite_output_child() {
    let mut command = Command::new("sh");
    command.args(["-c", "while :; do printf '0123456789abcdef'; done"]);

    let start = Instant::now();
    let result = support::try_run_bounded_command(
        command,
        "infinite-output proof fixture",
        Duration::from_secs(3),
    );
    let elapsed = start.elapsed();

    assert!(
        result
            .as_ref()
            .err()
            .is_some_and(hew_testutil::BoundedExecError::is_timeout),
        "bounded helper should kill infinite-output fixture, got: {result:?}",
    );
    assert!(
        elapsed < Duration::from_secs(10),
        "bounded helper returned too slowly after 3s deadline: {elapsed:?}",
    );
}

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

    let mut command = Command::new(hew_binary());
    command
        .arg("run")
        .arg("--timeout")
        .arg("1")
        .arg(&path)
        .current_dir(dir.path());
    let output = support::run_bounded_command(command, format!("hew run {}", path.display()));

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

    let output = run_bounded_hew_run(&path, dir.path());

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

    let output = run_bounded_hew_run(&path, dir.path());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
}

#[test]
fn run_generic_vec_into_iter_static_dispatch_outputs_first_value() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("generic_vec_into_iter_dispatch.hew");
    std::fs::write(
        &path,
        r"
        fn first_or_zero<I>(var it: I) -> i64
        where
            I: Iterator<Item = i64>,
        {
            match it.next() {
                Some(x) => x,
                None => 0,
            }
        }

        fn main() {
            let v: Vec<i64> = Vec::new();
            v.push(42);
            println(first_or_zero(v.into_iter()));
        }
        ",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, dir.path());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

#[test]
fn run_generic_user_iterator_static_dispatch_outputs_first_value() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("generic_user_iterator_dispatch.hew");
    std::fs::write(
        &path,
        r"
        type Counter {
            cur: i64;
            end: i64;
        }

        impl Iterator for Counter {
            type Item = i64;

            fn next(var self) -> Option<i64> {
                if self.cur >= self.end {
                    None
                } else {
                    let out = self.cur;
                    self.cur = self.cur + 1;
                    Some(out)
                }
            }
        }

        fn first_or_zero<I>(var it: I) -> i64
        where
            I: Iterator<Item = i64>,
        {
            match it.next() {
                Some(x) => x,
                None => 0,
            }
        }

        fn main() {
            println(first_or_zero(Counter { cur: 7, end: 10 }));
        }
        ",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, dir.path());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "7\n");
}

#[test]
fn run_string_methods_smoke_matches_expected() {
    require_codegen();

    let source = repo_root().join("examples/string_methods_smoke.hew");
    let expected =
        std::fs::read_to_string(repo_root().join("examples/string_methods_smoke.expected"))
            .expect("read string_methods_smoke.expected");

    let output = run_bounded_hew_run(&source, repo_root());

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

    let output = run_bounded_hew_run(&source, repo_root());

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

    let output = run_bounded_hew_run(&source, repo_root());

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

    let output = run_bounded_hew_run(&source, repo_root());

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

    let output = run_bounded_hew_run(&source, repo_root());

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

    let output = run_bounded_hew_run(&source, repo_root());

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

    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&hew_src).current_dir(repo_root());
    let output = support::run_bounded_command_with_stdin(
        command,
        format!("hew run {}", hew_src.display()),
        b"hello from stdin\n",
    );
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

    let output = run_bounded_hew_run(&hew_src, repo_root());

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

    let output = run_bounded_hew_run(&hew_src, repo_root());

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

    let output = run_bounded_hew_run(&hew_src, repo_root());

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

    let output = run_bounded_hew_run(&hew_src, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "got Point(7)\n",);
}

/// T-1 value oracle: `type R { b: i64, a: i64 }` matched as `R { a, b }`.
///
/// Declaration order: `b` is at index 0, `a` is at index 1.
/// Pattern binding order: `a` first, then `b`.
/// `R { b: 10, a: 20 }` → `match r { R { a, b } => a - b }` → 20 - 10 = 10.
///
/// If field offsets were resolved alphabetically (`a → 0, b → 1`) the result
/// would be 10 - 20 = -10, producing exit code 246 (i64 → u8 wrapping on Linux)
/// rather than 10 — a distinct wrong value that this oracle catches.
///
/// Pairs with the MIR unit test `record_project_declaration_order_not_alphabetical`
/// which asserts the `FieldOffset` values directly.
#[test]
fn run_struct_match_declaration_order_not_alphabetical() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/match_struct_decl_order.hew");

    let output = run_bounded_hew_run(&source, repo_root());

    let exit_code = output.status.code().unwrap_or(-1);
    assert_eq!(
        exit_code, 10,
        "expected exit 10 (a - b = 20 - 10); \
         exit {exit_code} means field offsets may be wrong (alphabetical sort would give -10 → 246); \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

/// Regression oracle — for-range loop variable derives its type from the
/// checker-resolved `Range<T>` element type, not the start bound.
///
/// `for i in 0..n` with `n: i32` passed to `take_i32(i32)`.  The checker
/// resolves the range as `Range<i32>`, so `i` should be `i32` throughout.
/// Before the fix the loop counter was always `I64`, causing `IntCmp` /
/// `IntArithChecked` width-guard failures at codegen.
///
/// The program sums 0+1+2+3+4+5+6 = 21 and returns 21 as the exit code.
#[test]
fn for_range_i32_bound_runs_and_returns_correct_value() {
    require_codegen();

    let fixture = repo_root().join("tests/vertical-slice/accept/for_range_regression.hew");
    assert!(fixture.exists(), "fixture missing: {}", fixture.display());

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&fixture)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew run");

    assert_eq!(
        output.status.code(),
        Some(21),
        "expected exit 21 (sum 0..7); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stderr.is_empty(),
        "expected no diagnostics; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

/// Regression oracle — mixed-width range bounds (i32..i64) produce a loop
/// variable typed at the wider bound (i64), not the narrower start bound.
///
/// `for i in a..b` with `a: i32 = 2, b: i64 = 6` passed to `id_i64(i64)`.
/// The checker resolves `Range<i64>` (common width of i32 and i64 is i64).
/// Before the fix, HIR derived element type from `start_hir.ty = i32`,
/// causing `call i64 @id_i64(i32 %arg)` which LLVM rejected.
///
/// Sums 2+3+4+5 = 14; exit code 14.
#[test]
fn for_range_mixed_width_bounds_runs_and_returns_correct_value() {
    require_codegen();

    let fixture = repo_root().join("tests/vertical-slice/accept/for_range_mixed_width_bounds.hew");
    assert!(fixture.exists(), "fixture missing: {}", fixture.display());

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&fixture)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew run");

    assert_eq!(
        output.status.code(),
        Some(14),
        "expected exit 14 (sum 2..6 as i64); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stderr.is_empty(),
        "expected no diagnostics; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

/// Regression oracle — negative integer literal range bounds lower correctly
/// when the loop variable is narrowed to a concrete integer type.
///
/// `for i in -5..5` with `id_i32(i)`.  The checker narrows the deferred
/// range `TypeVar` to `i32`.  Before the fix, the inner literal `5` inside
/// the negated start bound `-5` kept the `IntLiteral`→`I64` materialized
/// default, while the outer `-5` span was re-recorded as `i32`.  MIR
/// codegen then rejected `IntNegChecked` because dest (i32) ≠ operand (i64).
///
/// Sum of {-5,-4,-3,-2,-1,0,1,2,3,4} = -5.  Exit code 256 - 5 = 251.
#[test]
fn for_range_negative_literal_bound_runs_and_returns_correct_value() {
    require_codegen();

    let fixture =
        repo_root().join("tests/vertical-slice/accept/for_range_negative_literal_bound.hew");
    assert!(fixture.exists(), "fixture missing: {}", fixture.display());

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&fixture)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew run");

    assert_eq!(
        output.status.code(),
        Some(251),
        "expected exit 251 (sum -5..5 as i32, wraps to 251); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stderr.is_empty(),
        "expected no diagnostics; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}
