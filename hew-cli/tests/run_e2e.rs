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

    // Wait for the grandchild to actually be reaped rather than betting on a
    // fixed sleep: after the process-group kill, the OS may take a brief,
    // load-dependent window to deliver SIGKILL and reap the process. Poll the
    // POSIX liveness probe until it reports the process is gone (`kill(pid, 0)`
    // returns non-zero/ESRCH), bounded by a generous deadline.
    #[allow(
        clippy::cast_possible_wrap,
        reason = "PIDs fit in i32 on all supported Unix platforms"
    )]
    let grandchild_pid = grandchild_pid as libc::pid_t;
    let dead = {
        let deadline = Instant::now() + Duration::from_secs(10);
        loop {
            // SAFETY: `kill(pid, 0)` is a POSIX liveness probe — no signal is sent.
            let alive = unsafe { libc::kill(grandchild_pid, 0) } == 0;
            if !alive {
                break true;
            }
            if Instant::now() >= deadline {
                break false;
            }
            std::thread::sleep(Duration::from_millis(25));
        }
    };
    assert!(
        dead,
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

    assert_eq!(output.status.code(), Some(124));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: program timed out after 1s"));
}

#[test]
fn run_native_compile_error_exits_one() {
    let dir = support::tempdir();
    let path = dir.path().join("compile_err.hew");
    std::fs::write(
        &path,
        "fn main() {\n    let _ = undefined_symbol_xyz();\n}\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert_eq!(
        output.status.code(),
        Some(1),
        "hew run compile error should exit 1, not an internal sentinel; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn run_compile_error_exit_matches_check() {
    let dir = support::tempdir();
    let path = dir.path().join("compile_err.hew");
    std::fs::write(
        &path,
        "fn main() {\n    let _ = undefined_symbol_xyz();\n}\n",
    )
    .unwrap();

    let run = Command::new(hew_binary())
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();
    let check = Command::new(hew_binary())
        .arg("check")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert_eq!(check.status.code(), Some(1));
    assert_eq!(
        run.status.code(),
        check.status.code(),
        "hew run and hew check should use the same compile-error exit code; run stderr: {}\ncheck stderr: {}",
        String::from_utf8_lossy(&run.stderr),
        String::from_utf8_lossy(&check.stderr),
    );
}

#[test]
fn debug_compile_error_exits_one() {
    let dir = support::tempdir();
    let path = dir.path().join("compile_err.hew");
    std::fs::write(
        &path,
        "fn main() {\n    let _ = undefined_symbol_xyz();\n}\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("debug")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert_eq!(
        output.status.code(),
        Some(1),
        "hew debug compile error should exit before debugger resolution with code 1; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
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
fn tcp_loopback_read_string_roundtrip_returns_written_bytes() {
    require_codegen();

    let addr = unused_loopback_addr();
    let dir = support::tempdir();
    let path = dir.path().join("tcp_loopback_roundtrip.hew");
    std::fs::write(
        &path,
        format!(
            r#"
import std::net;

actor EchoServer {{
    receive fn connect_send_and_read(unused: i64) {{
        let conn = net.connect("{addr}");
        conn.write_string("client-ping:r319");
        let reply = conn.read_string();
        println(f"client-read={{reply}}");
        conn.close();
    }}
}}

fn main() {{
    let listener = net.listen("{addr}");
    let client = spawn EchoServer;
    client.connect_send_and_read(0);

    let conn = listener.accept();
    let request = conn.read_string();
    println(f"server-read={{request}}");
    conn.write_string("tcp-echo:hew-net-r319");
    conn.close();
    listener.close();
}}
"#,
        ),
    )
    .expect("write TCP loopback fixture");

    let output = run_bounded_hew_run(&path, repo_root());

    assert!(
        output.status.success(),
        "hew run should complete the TCP roundtrip; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(
        actual,
        "server-read=client-ping:r319\nclient-read=tcp-echo:hew-net-r319\n"
    );
}

fn unused_loopback_addr() -> String {
    std::net::TcpListener::bind(("127.0.0.1", 0))
        .expect("bind ephemeral loopback listener")
        .local_addr()
        .expect("read ephemeral loopback address")
        .to_string()
}

#[test]
fn run_float_comparison_branches_for_f64_and_f32() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("float_comparison_branches.hew");
    std::fs::write(
        &path,
        r"
        fn score_f64() -> i64 {
            let a: f64 = 1.25;
            let b: f64 = 2.5;
            var passed = 0;
            if a < b { passed = passed + 1; } else { passed = passed + 1000; }
            if b < a { passed = passed + 1000; } else { passed = passed + 1; }
            if b > a { passed = passed + 1; } else { passed = passed + 1000; }
            if a > b { passed = passed + 1000; } else { passed = passed + 1; }
            if a <= b { passed = passed + 1; } else { passed = passed + 1000; }
            if b <= a { passed = passed + 1000; } else { passed = passed + 1; }
            if a <= a { passed = passed + 1; } else { passed = passed + 1000; }
            if b >= a { passed = passed + 1; } else { passed = passed + 1000; }
            if a >= b { passed = passed + 1000; } else { passed = passed + 1; }
            if a >= a { passed = passed + 1; } else { passed = passed + 1000; }
            if a == a { passed = passed + 1; } else { passed = passed + 1000; }
            if a == b { passed = passed + 1000; } else { passed = passed + 1; }
            if a != b { passed = passed + 1; } else { passed = passed + 1000; }
            if a != a { passed = passed + 1000; } else { passed = passed + 1; }
            passed
        }

        fn score_f32() -> i64 {
            let a: f32 = 1.25;
            let b: f32 = 2.5;
            var passed = 0;
            if a < b { passed = passed + 1; } else { passed = passed + 1000; }
            if b < a { passed = passed + 1000; } else { passed = passed + 1; }
            if b > a { passed = passed + 1; } else { passed = passed + 1000; }
            if a > b { passed = passed + 1000; } else { passed = passed + 1; }
            if a <= b { passed = passed + 1; } else { passed = passed + 1000; }
            if b <= a { passed = passed + 1000; } else { passed = passed + 1; }
            if a <= a { passed = passed + 1; } else { passed = passed + 1000; }
            if b >= a { passed = passed + 1; } else { passed = passed + 1000; }
            if a >= b { passed = passed + 1000; } else { passed = passed + 1; }
            if a >= a { passed = passed + 1; } else { passed = passed + 1000; }
            if a == a { passed = passed + 1; } else { passed = passed + 1000; }
            if a == b { passed = passed + 1000; } else { passed = passed + 1; }
            if a != b { passed = passed + 1; } else { passed = passed + 1000; }
            if a != a { passed = passed + 1000; } else { passed = passed + 1; }
            passed
        }

        fn main() {
            println(score_f64() + score_f32());
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
    assert_eq!(String::from_utf8_lossy(&output.stdout), "28\n");
}

/// CAP-12: `Node::load_keys` / `Node::allow_peer` compile and run on the native
/// QUIC mesh. `load_keys` mints and persists this node's TLS identity (the
/// keyfile must exist after the run and survive unchanged on a second run);
/// `allow_peer` pins a peer SPKI in the fail-closed allowlist; the node then
/// starts, registers, and shuts down. Native quic-mesh; no parity on WASM.
#[test]
fn run_node_peer_auth_surface_persists_keys_and_runs() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("node_peer_auth.hew");
    std::fs::write(
        &path,
        r#"
        actor Counter {
            var count: i64;
            receive fn increment(n: i64) { count = count + n; }
        }

        fn main() {
            Node::set_transport("quic-mesh");
            Node::load_keys("node.key");
            Node::allow_peer("3059301306072a8648ce3d020106082a8648ce3d030107");
            Node::start("127.0.0.1:0");
            let counter = spawn Counter(count: 0);
            Node::register("counter", counter);
            counter.increment(5);
            Node::shutdown();
            println("peer-auth ok");
        }
        "#,
    )
    .expect("write peer-auth fixture");

    let output = run_bounded_hew_run(&path, dir.path());
    assert!(
        output.status.success(),
        "hew run should complete the peer-auth surface; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        strip_ansi(&String::from_utf8_lossy(&output.stdout)),
        "peer-auth ok\n"
    );

    let keyfile = dir.path().join("node.key");
    let first = std::fs::read(&keyfile).expect("load_keys must persist a keyfile");
    assert!(
        first.starts_with(b"HEWMESHKEY1\0"),
        "keyfile must carry the mesh magic"
    );

    // Stable identity: a second run loads the existing key, never overwrites.
    let output2 = run_bounded_hew_run(&path, dir.path());
    assert!(
        output2.status.success(),
        "second run should reuse the persisted key"
    );
    let second = std::fs::read(&keyfile).expect("keyfile still present");
    assert_eq!(
        first, second,
        "load_keys must not rotate a persisted identity"
    );
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

/// #1929 Stage 1: element-typed `Vec<T>` methods (`push`/`get`/`set`/`pop`)
/// under a type parameter re-resolve to the correct per-ABI runtime symbol per
/// monomorphisation and round-trip the EXACT pushed value for every scalar /
/// string element ABI (i64, f64, bool, string) plus `set`/`pop`.
#[test]
fn run_generic_vec_element_methods_roundtrip_scalar_abis() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/vec_generic_elem_push_get.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/vec_generic_elem_push_get.expected"),
    )
    .expect("read vec_generic_elem_push_get.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Vec element-method round-trip should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// #1929 Stage 1: the pointer-ABI element. A `Vec<T>` `push`/`get` under a type
/// parameter instantiated with the Copy pointer-identity handle
/// `LocalPid<Counter>` re-resolves to `hew_vec_push_ptr` / `hew_vec_get_ptr`,
/// and the retrieved handle drives the SAME actor (bump then report → "1").
#[test]
fn run_generic_vec_element_methods_roundtrip_ptr_abi() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/vec_generic_elem_ptr_handle.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/vec_generic_elem_ptr_handle.expected"),
    )
    .expect("read vec_generic_elem_ptr_handle.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Vec ptr-element round-trip should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// #1929 Stage 1: read-only generic algorithm `items.get(i).show()` over
/// `Vec<T> where T: Show`, instantiated with the Copy value-record `Color`.
/// `get` re-resolves to the `hew_vec_get_layout` (Copy value-record) ABI the
/// concrete path already backs, then `.show()` dispatches statically.
#[test]
fn run_generic_vec_get_copy_record_then_trait_dispatch() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/accept/vec_generic_elem_get_trait_dispatch.hew");
    let expected = std::fs::read_to_string(
        repo_root()
            .join("tests/vertical-slice/accept/vec_generic_elem_get_trait_dispatch.expected"),
    )
    .expect("read vec_generic_elem_get_trait_dispatch.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Vec Copy-record get + trait dispatch should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// #1929 Stage 2: a NON-Copy record element under a type parameter. `Vec<Name>`
/// (Name owns a heap `string`) re-resolves its element ops to the owned
/// descriptor family (`hew_vec_get_owned` / `hew_vec_push_owned`) — the same
/// ABI the concrete owned path uses — through the generic monomorphisation
/// spine. `first<T>(v) -> T` returns the gotten element (the ownership escape);
/// the owned getter borrow, the interior-borrow leak taint, and the scope-exit
/// `hew_vec_free_owned` agree so the round-trip is correct for the owned record
/// alongside the scalar (`i64`) and copy-on-write (`string`) element ABIs.
#[test]
fn run_generic_vec_get_owned_record_under_type_param() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/generic_vec_get_owned_record.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/generic_vec_get_owned_record.expected"),
    )
    .expect("read generic_vec_get_owned_record.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Vec owned-record get should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// #1929 Stage 2 leak proof for the generic owned-element ABI: 100k iterations
/// of push-an-owned-`Name` into a `Vec<T>`, get it back via a returning generic
/// helper (the ownership escape), read the field, and drop. A double-free of the
/// one heap string (the owned getter borrow aliasing the Vec's slot that
/// `hew_vec_free_owned` also releases) SIGABRTs within a few iterations; a
/// missed drop grows RSS linearly. A clean run is the exactly-once witness.
#[test]
fn run_generic_vec_get_owned_element_is_dropped_once() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/generic_vec_get_owned_leak.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/generic_vec_get_owned_leak.expected"),
    )
    .expect("read generic_vec_get_owned_leak.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Vec owned-element round-trip should run cleanly (a double-free \
         would abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// #1929 Stage 2: `for x in items` over a `Vec<T>` where `T` is a type
/// parameter. `copy_all<T>` drives the generic for-in (the Stage 2 target) and
/// re-collects each yielded element into a fresh `Vec<T>` via `out.push(x)`.
/// The synthetic `VecIter<elem>` layout is registered per monomorphisation so
/// the iterator's record-field access resolves for each concrete element. The
/// iterate-and-collect round-trip is summed at the concrete call site for a
/// scalar element (i64 → 60) and a Copy value-record element (Point → 10).
#[test]
fn run_generic_vec_for_in_collect_scalar_and_record() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/for_in_generic_vec_collect.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/for_in_generic_vec_collect.expected"),
    )
    .expect("read for_in_generic_vec_collect.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Vec<T> for-in collect should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// #1929 Stage 2: pure-iteration proof for the generic for-in. `count<T>`
/// increments an `i64` accumulator once per yielded element without touching
/// the element value, isolating the `VecIter` desugar from the element-write
/// path. The loop runs exactly once per element across three element ABIs —
/// `i64` (3), `string` (4), and the Copy value-record `Point` (2).
#[test]
fn run_generic_vec_for_in_count_across_element_abis() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/for_in_generic_vec_count.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/for_in_generic_vec_count.expected"),
    )
    .expect("read for_in_generic_vec_count.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Vec<T> for-in count should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// #1929 Stage 3 / #1565: generic `println` / `print` / `to_string` of a value
/// typed by a `T: Display` type parameter. The builtin print surfaces no longer
/// re-derive a monomorphic overload from the concrete argument type; instead
/// they route through the checker-verified `Display` obligation and dispatch to
/// the concrete `Display::fmt` impl per monomorphisation. Covers a primitive
/// (`i64` → 42, 7), a string (`hi`, `yo`), and a user `impl Display` (`Color`).
#[test]
fn run_generic_display_println_print_to_string() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/generic_display_println.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/generic_display_println.expected"),
    )
    .expect("read generic_display_println.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Display println/print/to_string should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// #1929 Stage 3 / #1565: f-string interpolation `f"{x}"` of a `T: Display`
/// type parameter (previously rejected by the checker's `require_display_impl`
/// gate because it did not recognise a type parameter's `Display` bound), plus
/// generic `println` over a `Vec<T>` — combining Stage 2 generic `for-in` with
/// Stage 3 Display dispatch. Covers `i64`, `string`, and a user `impl Display`.
#[test]
fn run_generic_display_fstring_and_vec_iteration() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/generic_display_fstring.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/generic_display_fstring.expected"),
    )
    .expect("read generic_display_fstring.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Display f-string + Vec<T> iteration should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// #1929 Stage 3 / #1565: a generic `T: Display` consumer dispatches to a
/// `Display` impl defined in a different module. The static-trait-dispatch
/// resolution keys on the canonical `(Display, <concrete>, fmt)` triple, so the
/// impl's defining module is irrelevant — proving the cross-module Display
/// receiver acceptance criterion. Also exercises an `i64` receiver in the same
/// program so the generic and primitive paths coexist.
#[test]
fn run_generic_display_cross_module_receiver() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/generic_display_xmod.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/generic_display_xmod.expected"),
    )
    .expect("read generic_display_xmod.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic Display cross-module receiver should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

#[test]
fn var_self_countdown_loop_writes_receiver_back() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("var_self_countdown.hew");
    std::fs::write(
        &source,
        r"
pub type Countdown { n: i64; }

impl Iterator for Countdown {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        if self.n <= 0 {
            None
        } else {
            let cur = self.n;
            self.n = self.n - 1;
            Some(cur)
        }
    }
}

fn main() {
    var cd = Countdown { n: 3 };
    var total = 0;
    loop {
        match cd.next() {
            Some(v) => { total = total + v; },
            None => { break; },
        }
    }
    println(total);
}
",
    )
    .expect("write var-self countdown fixture");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "6\n");
}

#[test]
fn var_self_direct_second_next_observes_mutated_receiver() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("var_self_direct_second.hew");
    std::fs::write(
        &source,
        r"
pub type Counter { n: i64; }

impl Iterator for Counter {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        self.n = self.n + 1;
        Some(self.n)
    }
}

fn main() {
    var c = Counter { n: 0 };
    let _first = c.next();
    match c.next() {
        Some(v2) => { println(v2); },
        None => { println(-1); },
    }
}
",
    )
    .expect("write var-self direct second fixture");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "2\n");
}

#[test]
fn var_self_cowvalue_receiver_survives_storeback_without_double_drop() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("var_self_cowvalue.hew");
    std::fs::write(
        &source,
        r#"
fn main() {
    let words: Vec<string> = Vec::new();
    words.push("first");
    words.push("second");
    var it = words.into_iter();
    let _first = it.next();
    match it.next() {
        Some(v2) => { println(v2); },
        None => { println("none"); },
    }
}
"#,
    )
    .expect("write var-self CowValue receiver fixture");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed without receiver double-drop; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "second\n");
}

#[test]
fn var_self_nested_block_value_does_not_get_abi_wrapped() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("var_self_nested_block.hew");
    std::fs::write(
        &source,
        r"
pub type Counter { n: i64; }

impl Iterator for Counter {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        let before = { self.n };
        self.n = self.n + 1;
        Some(before)
    }
}

fn main() {
    var c = Counter { n: 1 };
    let _first = c.next();
    match c.next() {
        Some(v2) => { println(v2); },
        None => { println(-1); },
    }
}
",
    )
    .expect("write var-self nested block fixture");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "2\n");
}

#[test]
fn var_self_generic_impl_direct_second_next_resolves_monomorphized_callee() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("var_self_generic_impl_direct.hew");
    std::fs::write(
        &source,
        r"
pub type Slot<T> { x: T; n: i64; }

trait Tick {
    type Item;
    fn next(var self) -> Option<Self::Item>;
}

impl<T> Tick for Slot<T> {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        self.n = self.n + 1;
        Some(self.n)
    }
}

fn main() {
    var s = Slot<i64> { x: 0, n: 0 };
    let _first = s.next();
    match s.next() {
        Some(v2) => { println(v2); },
        None => { println(-1); },
    }
}
",
    )
    .expect("write var-self generic impl direct fixture");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "2\n");
}

#[test]
fn var_self_generic_method_direct_resolves_impl_and_method_type_args() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("var_self_generic_method_direct.hew");
    std::fs::write(
        &source,
        r"
pub type Slot<T> { x: T; }

trait Tick {
    fn take<U>(var self, u: U) -> U;
}

impl<T> Tick for Slot<T> {
    fn take<U>(var self, u: U) -> U { u }
}

fn main() {
    var s = Slot<i64> { x: 5 };
    println(s.take(7));
}
",
    )
    .expect("write var-self generic method direct fixture");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "7\n");
}

#[test]
fn var_self_generic_static_dispatch_second_next_observes_mutated_receiver() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("var_self_generic_static_dispatch.hew");
    std::fs::write(
        &source,
        r"
fn second_or_zero<I>(var it: I) -> i64 where I: Iterator<Item = i64> {
    let _first = it.next();
    match it.next() {
        Some(x) => x,
        None => 0,
    }
}

fn main() {
    let values: Vec<i64> = Vec::new();
    values.push(1);
    values.push(2);
    println(second_or_zero(values.into_iter()));
}
",
    )
    .expect("write var-self generic static dispatch fixture");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "2\n");
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

#[test]
fn run_tuple_numeric_field_access_reads_distinct_mixed_type_elements() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("tuple_numeric_field_access.hew");
    std::fs::write(
        &source,
        r#"
fn main() -> i64 {
    let t0 = (42, false);
    let t1 = (0, true);
    print(t0.0);
    println("");
    print(t1.1);
    println("");
    if t1.1 {
        t0.0
    } else {
        99
    }
}
"#,
    )
    .expect("write tuple numeric field access fixture");

    let output = run_bounded_hew_run(&source, dir.path());

    assert_eq!(
        output.status.code(),
        Some(42),
        "expected exit 42 from t0.0 gated by t1.1; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "42\ntrue\n", "stdout mismatch");
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
/// validation evidence.
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

/// W5.011 P3 owned-`string` temporary substrate double-free guard: a fresh-owned
/// `string` produced by `+`/concat, `.to_upper()`, or the `Vec<string>`
/// getter and then used in a borrowing context (`.len()`) — whether bound
/// (`let y = <producer>; y.len()`), nested (`(<producer>).len()`), or discarded
/// (`<producer>;`) — must be released exactly once. A regressed substrate that
/// dropped such an owner twice would over-decrement the refcount and free early;
/// the runtime's `free_cstring` header-sentinel check aborts the process
/// (SIGABRT) on a double-free, so a clean exit with `done` across every
/// iteration of the hot loop is the behavioural proof that each owner is freed
/// exactly once (and the bounded-RSS leak proof — flat peak RSS at 40M
/// iterations — is recorded in the validation evidence). The structural
/// single-drop proofs live in
/// `hew-mir/tests/owned_string_temp_drop_canary.rs`.
#[test]
fn run_owned_string_temp_no_double_free() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/accept/owned_string_temp_no_double_free.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/owned_string_temp_no_double_free.expected"),
    )
    .expect("read owned_string_temp_no_double_free.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    // A double-free aborts the process (SIGABRT) via the runtime's
    // `free_cstring` sentinel check, so `success()` is itself the proof.
    assert!(
        output.status.success(),
        "owned_string_temp_no_double_free should run cleanly (a double-free would \
         abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

#[test]
fn run_user_record_string_field_is_dropped_once() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/user_record_string_field.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/user_record_string_field.expected"),
    )
    .expect("read user_record_string_field.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "user_record_string_field should run cleanly (a double-free would abort); \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Generic-instantiation twin of `run_user_record_string_field_is_dropped_once`:
/// a `Pair<i64, string>` carrying a computed (heap) string is constructed and
/// dropped on every one of 100k iterations. The generic owned-record drop spine
/// must free the string field exactly once per iteration — a double-free aborts
/// (the loop is the exactly-once witness), a missed drop leaks.
#[test]
fn run_generic_record_string_field_is_dropped_once() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/generic_record_string_field.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/generic_record_string_field.expected"),
    )
    .expect("read generic_record_string_field.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic_record_string_field should run cleanly (a double-free would \
         abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Generic record swap that moves an owned string across instantiations
/// (`Pair<i64, string>` -> `Pair<string, i64>`) and returns the result by value,
/// 100k times. The returned generic owned record is dropped exactly once at the
/// caller; the callee transfers the string into the new instantiation. A
/// double-free aborts, a missed drop leaks.
#[test]
fn run_generic_record_swap_owned_is_dropped_once() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/generic_record_swap_owned.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/generic_record_swap_owned.expected"),
    )
    .expect("read generic_record_swap_owned.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "generic_record_swap_owned should run cleanly (a double-free would \
         abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Non-BitCopy record match destructure — full extraction of an owned
/// `string` field across 100k iterations. Until the partial-move/drop
/// elaboration spine landed, `lower_match_project` rejected the shape
/// fail-closed; this guards the lift. The extracted binder is added to
/// `owned_locals` so its function-scope drop fires exactly once, and
/// `derive_owned_record_drop_allowed` excludes the source aggregate's
/// composite drop via the field-binder release-owner rule. A regressed
/// lift that double-freed would abort at `free_cstring`'s sentinel; a
/// regressed lift that leaked would grow RSS linearly with iteration count.
#[test]
fn run_match_record_destructure_owned_drops_once() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/match_record_destructure_owned.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/match_record_destructure_owned.expected"),
    )
    .expect("read match_record_destructure_owned.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "match_record_destructure_owned should run cleanly (a double-free would \
         abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Non-BitCopy tuple match destructure — full extraction of a
/// `(string, i64)` across 100k iterations. The tuple analogue of
/// `run_match_record_destructure_owned_drops_once`:
/// `derive_tuple_composite_drop_allowed` excludes the tuple temp's composite
/// member drop because the extracted owned binder is in
/// `release_owner_bases`. A double-free aborts; a leak grows RSS.
#[test]
fn run_match_tuple_destructure_owned_drops_once() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/match_tuple_destructure_owned.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/match_tuple_destructure_owned.expected"),
    )
    .expect("read match_tuple_destructure_owned.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "match_tuple_destructure_owned should run cleanly (a double-free would \
         abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Non-BitCopy record match destructure — PARTIAL extraction (wildcard on
/// an owned `string` sibling) across 100k iterations. The partial-extraction
/// emitter loads the wildcarded field into a temp and emits an inline
/// `Instr::Drop` with the leaf release symbol (`hew_string_drop`). Without
/// that emission the wildcarded sibling leaked — the drop spine's composite
/// suppression already kicks in once any binder owns release. A double-free
/// (composite + binder + inline drop all firing) aborts; a leak grows RSS.
#[test]
fn run_match_record_partial_extraction_owned_drops_once() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/accept/match_record_partial_extraction_owned.hew");
    let expected = std::fs::read_to_string(
        repo_root()
            .join("tests/vertical-slice/accept/match_record_partial_extraction_owned.expected"),
    )
    .expect("read match_record_partial_extraction_owned.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "match_record_partial_extraction_owned should run cleanly (a double-free \
         or use-after-free would abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Fail-closed: match-destructure wildcard on an owned-aggregate field
/// (here `Inner` is a record carrying a `string`). The inline-drop
/// dispatcher cannot emit `DropKind::RecordInPlace`, so
/// `project_field_inline_drop_symbol` returns `None` for owned record /
/// tuple / enum field types and the pre-flight emits
/// `E_NOT_YET_IMPLEMENTED: MIR lowering for match-destructure wildcard on
/// owned aggregate field`. The guard guarantees the lift never silently
/// leaks an owned-aggregate sibling.
#[test]
fn check_match_destructure_wildcard_owned_aggregate_fails_closed() {
    require_codegen();

    let source = repo_root()
        .join("tests/vertical-slice/reject/match_destructure_wildcard_owned_aggregate.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("match-destructure wildcard on owned aggregate field"),
        "expected fail-closed diagnostic; got: {combined}"
    );
}

/// Non-BitCopy record match destructure — FULL extraction with a
/// non-escaping bound owned field.
///
/// `Pair { a: x, b: y } => x` binds both heap-owning fields, but `y` is
/// never read. Until the per-binder taint-suppression landed, `y`'s
/// `RecordFieldLoad` dest was tainted as a projection-alias of the parent,
/// `derive_cow_sole_owner` excluded it from the leaf `CoW` allow-set, and
/// `build_lifo_drops` silently emitted no drop for `y` — one allocation
/// leaked per match. The fix pairs the scrutinee consume mark with a
/// `match_project_consumed_binder_locals` exemption: when the scrutinee is
/// a non-captured `BindingRef` (consume-marked at the destructure site),
/// the bound owned fields become sole owners and are admitted. A double-
/// free regression (composite + binder both fire) would abort at
/// `free_cstring`'s sentinel; the prior leak posture grew RSS linearly
/// with iteration count.
#[test]
fn run_match_record_full_extraction_unused_binder_drops_once() {
    require_codegen();

    let source = repo_root()
        .join("tests/vertical-slice/accept/match_record_full_extraction_unused_binder.hew");
    let expected =
        std::fs::read_to_string(repo_root().join(
            "tests/vertical-slice/accept/match_record_full_extraction_unused_binder.expected",
        ))
        .expect("read match_record_full_extraction_unused_binder.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "match_record_full_extraction_unused_binder should run cleanly (a double-free \
         would abort, a leak would grow RSS); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Fail-closed: post-match read of a `BindingRef` scrutinee whose owned
/// fields were destructured. The destructure consumes the scrutinee's
/// storage (per-binding
/// loads hand off ownership; partial-extraction wildcards drop fields IN
/// PLACE). MIR emits a follow-up `Use { intent: Consume }` for the
/// scrutinee binding so the dataflow checker transitions it to
/// `Consumed(site)`; any post-match `BindingRef` use then fires
/// `E_MIR_CHECK: UseAfterConsume`, catching the UAF at check time.
#[test]
fn check_match_destructure_use_after_consume_fails_closed() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/reject/match_destructure_use_after_consume.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("UseAfterConsume"),
        "expected UseAfterConsume diagnostic; got: {combined}"
    );
}

/// Fail-closed: non-BitCopy match destructure on a projection scrutinee
/// (`FieldAccess` / `TupleIndex` / `Index` / `Slice` or captured
/// `BindingRef`).
/// The projection's source storage is re-readable through the same shape
/// after the match, and there is no binding for the dataflow checker to
/// mark `Consumed`. MIR refuses fail-closed with
/// `E_NOT_YET_IMPLEMENTED: MIR lowering for non-BitCopy match destructure
/// on projection scrutinee` and instructs the user to bind the scrutinee
/// to a local first so the consume mark has a target.
#[test]
fn check_match_destructure_projection_scrutinee_fails_closed() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/reject/match_destructure_projection_scrutinee.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("non-BitCopy match destructure on projection scrutinee"),
        "expected fail-closed diagnostic; got: {combined}"
    );
}

/// Non-BitCopy record match destructure — FULL extraction of an owned
/// `Vec<i64>` field with a wildcarded `BitCopy` sibling. The arm
/// `Pair { a: _, b: v } => v` moves the vector into `v`, which enters
/// `owned_locals` exactly like a `string` binder would: the extraction path
/// is type-agnostic over non-BitCopy field types. Correctness oracle — each
/// extracted vector is a valid length-2 buffer holding `[10, 20]`, so the
/// running total over 1000 iterations is `1000 * (2 + 10 + 20) = 32000`. A
/// lift that rejected non-`string` owned binders would fail to compile; a
/// lift that handed back an empty / invalid handle would print the wrong
/// total or abort. (The `Vec` drop-in-loop leak axis is governed by the
/// pre-existing `Vec` value-class drop limitation — identical to a plain
/// `let v = make_vec()` — so this test pins extraction correctness and the
/// absence of double-free / abort across the back-edge drop path, not the
/// per-iteration `Vec` byte count.)
#[test]
fn run_match_record_vec_full_extraction() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/accept/match_record_vec_full_extraction.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/match_record_vec_full_extraction.expected"),
    )
    .expect("read match_record_vec_full_extraction.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "match_record_vec_full_extraction should run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Non-BitCopy record match destructure — ALL-wildcard arm over an owned
/// aggregate (`Outer { name: _, inner: _ } => 0`). The arm binds nothing, so
/// nothing seeds the field-binder release-owner rule and the scrutinee's own
/// composite `RecordInPlace` drop is NOT suppressed: it fires at scope exit
/// and frees every owned field (`name` plus the nested `inner.value`). Drop
/// oracle over 100k iterations of a `string`-backed aggregate (string drop is
/// leak-clean, unlike `Vec`): a regression that suppressed the composite drop
/// without a replacement leaks two strings per iteration (the time-bounded
/// runner trips on linear RSS growth / wall-clock); a regression that BOTH
/// dropped composite AND emitted per-field drops double-frees and aborts at
/// `free_cstring`'s sentinel. A clean `done` is the behavioural proof.
#[test]
fn run_match_record_wildcard_all_owned_drops_once() {
    require_codegen();

    let source = repo_root()
        .join("tests/vertical-slice/accept/match_record_wildcard_all_owned_drops_once.hew");
    let expected =
        std::fs::read_to_string(repo_root().join(
            "tests/vertical-slice/accept/match_record_wildcard_all_owned_drops_once.expected",
        ))
        .expect("read match_record_wildcard_all_owned_drops_once.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "match_record_wildcard_all_owned_drops_once should run cleanly (a leak or \
         double-free would trip the runner / abort); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Non-BitCopy record match destructure — an all-wildcard arm leaves the
/// scrutinee reusable. Because the arm moves nothing out, the scrutinee
/// binding is NOT consume-marked (only an arm that binds at least one owned
/// field earns the mark), so a post-match read of `o.name` is legitimate and
/// the binding's composite drop still fires at scope exit. This is the
/// inverse of `check_match_destructure_use_after_consume_fails_closed`: there
/// an owned field is moved out, the scrutinee is consumed, and a post-match
/// read fires `UseAfterConsume`. A regression that over-eagerly consume-marked
/// an all-wildcard scrutinee would reject this read at check time.
#[test]
fn run_match_record_wildcard_scrutinee_reusable() {
    require_codegen();

    let source = repo_root()
        .join("tests/vertical-slice/accept/match_record_wildcard_scrutinee_reusable.hew");
    let expected = std::fs::read_to_string(
        repo_root()
            .join("tests/vertical-slice/accept/match_record_wildcard_scrutinee_reusable.expected"),
    )
    .expect("read match_record_wildcard_scrutinee_reusable.expected");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "match_record_wildcard_scrutinee_reusable should run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Fail-closed: non-BitCopy match destructure on a TEMPORARY (fresh-value)
/// scrutinee with an all-wildcard arm (`match make() { Outer { .. } => 0 }`).
/// A temporary has no composite drop — nothing in the surrounding scope frees
/// it at scope exit — and an all-wildcard arm emits no per-field drops, so
/// every owned field of the discarded aggregate would leak. The
/// scrutinee-shape gate refuses this fail-closed with
/// `E_NOT_YET_IMPLEMENTED: MIR lowering for non-BitCopy match destructure on
/// temporary scrutinee` and instructs the user to bind the scrutinee to a
/// local first so its composite drop frees the discarded fields. Without the
/// gate covering the zero-binding arm, this shape would `check` green and
/// leak on every evaluation.
#[test]
fn check_match_destructure_temporary_scrutinee_fails_closed() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/reject/match_destructure_temporary_scrutinee.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("non-BitCopy match destructure on temporary scrutinee"),
        "expected temporary-scrutinee fail-closed diagnostic; got: {combined}"
    );
}

/// Fail-closed: non-BitCopy match destructure on a TEMPORARY scrutinee with a
/// binding arm (`match make() { Pair { a: x, b: _ } => x }`). The same
/// scrutinee-shape gate that covers the all-wildcard temporary also covers
/// the partial-extraction temporary: with no binding for the scrutinee, there
/// is no consume mark to untaint the extracted binder, so the temporary's
/// owned payload would leak. Refused fail-closed with the same
/// temporary-scrutinee diagnostic; binding the scrutinee to a local first
/// routes through the consume-mark + binder-untaint path that drops every
/// field exactly once.
#[test]
fn check_match_destructure_temporary_scrutinee_bound_fails_closed() {
    require_codegen();

    let source = repo_root()
        .join("tests/vertical-slice/reject/match_destructure_temporary_scrutinee_bound.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("non-BitCopy match destructure on temporary scrutinee"),
        "expected temporary-scrutinee fail-closed diagnostic; got: {combined}"
    );
}

/// Fail-closed: a guard on a record match-destructure arm. A record project
/// pattern is irrefutable, so `lower_match_project` lowers exactly the first
/// arm's body with no ordered fallthrough chain. An arm guard
/// (`Pattern if <cond>`) has no fallthrough target, so a `false` guard cannot
/// retry a later arm — the guarded arm would run anyway and consume the
/// scrutinee out from under the intended arm, a silent miscompile. This is the
/// exact shape `match p { Pair { a: x, b: _ } if false => x, Pair { a: _, b: y }
/// => y }`, which must print `b-payload` but would wrongly bind `x`. MIR
/// refuses fail-closed with
/// `E_NOT_YET_IMPLEMENTED: guarded record/tuple match destructure` and steers
/// the user to move the condition into the arm body or match on an enum.
#[test]
fn check_match_destructure_guarded_record_fails_closed() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/reject/match_destructure_guarded_record.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("guarded record/tuple match destructure"),
        "expected guarded-destructure fail-closed diagnostic; got: {combined}"
    );
}

/// Fail-closed: a guard on a tuple match-destructure arm. A tuple project
/// pattern is irrefutable exactly like the record case, so the same gate
/// applies — `match p { (a, _) if false => a, (_, b) => b }` would silently
/// take the guarded first arm. MIR refuses fail-closed with the same
/// `guarded record/tuple match destructure` diagnostic, confirming the gate is
/// unconditional across record and tuple projection scrutinees.
#[test]
fn check_match_destructure_guarded_tuple_fails_closed() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/reject/match_destructure_guarded_tuple.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("guarded record/tuple match destructure"),
        "expected guarded-destructure fail-closed diagnostic; got: {combined}"
    );
}
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

#[test]
fn i32_match_integer_literal_arm_runs_correct_branch() {
    require_codegen();

    let fixture = repo_root().join("tests/vertical-slice/accept/i32_match_literal.hew");
    assert!(fixture.exists(), "fixture missing: {}", fixture.display());

    let output = run_bounded_hew_run(&fixture, repo_root());

    assert_eq!(
        output.status.code(),
        Some(42),
        "expected exit 42 from the i32 literal match arm; stdout: {}\nstderr: {}",
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
// WINDOWS-TODO: Windows preserves signed exit codes; test expects Unix u8-wrapped value (251 vs -5).
#[cfg_attr(windows, ignore)]
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

/// Range iterator adapters (`.rev()` / `.step_by(k)`) lower to a strided /
/// descending `ForRange` and produce the EXACT iteration sequences:
///   - `(0..5).rev()`              → 4 3 2 1 0
///   - `(0..10).step_by(2)`        → 0 2 4 6 8
///   - `(0..=10).rev().step_by(3)` → 10 7 4 1   (composed left-to-right)
///
/// Asserting the full stdout (not just an exit code or a count) is the test
/// with teeth: a mis-ordered, off-by-one, or dropped-stride codegen path
/// would change the printed sequence.
#[test]
fn for_range_rev_and_step_by_print_exact_sequences() {
    require_codegen();

    let cases: &[(&str, &str)] = &[
        (
            "tests/vertical-slice/accept/for_range_rev.hew",
            "4\n3\n2\n1\n0\n",
        ),
        (
            "tests/vertical-slice/accept/for_range_step_by.hew",
            "0\n2\n4\n6\n8\n",
        ),
        (
            "tests/vertical-slice/accept/for_range_rev_step_by.hew",
            "10\n7\n4\n1\n",
        ),
    ];

    for (rel, expected_stdout) in cases {
        let fixture = repo_root().join(rel);
        assert!(fixture.exists(), "fixture missing: {}", fixture.display());

        let output = Command::new(hew_binary())
            .arg("run")
            .arg(&fixture)
            .current_dir(repo_root())
            .output()
            .expect("invoke hew run");

        assert!(
            output.status.success(),
            "{rel}: expected clean exit; stdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
        let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
        assert_eq!(
            stdout,
            *expected_stdout,
            "{rel}: exact iteration sequence mismatch; stderr: {}",
            String::from_utf8_lossy(&output.stderr),
        );
    }
}

/// `.step_by(k)` before `.rev()` does not commute with the supported
/// `.rev()`-then-`.step_by(k)` order and is rejected fail-closed at check time
/// rather than silently miscompiled.
///
/// `.rev()` and `.step_by(k)` fold into an order-insensitive
/// `{descending, step}` pair on the `ForRange` primitive.  That fold is correct
/// only for `.rev()` first (descend from the high bound, then stride); a
/// `.step_by(k)` *before* a `.rev()` would have to start the descending
/// sequence at the last strided element (`8` for `(0..10).step_by(2)`), not the
/// raw high bound (`9`).  Before the fail-closed guard, `(0..10).step_by(2).rev()`
/// silently printed `9 7 5 3 1` instead of the correct `8 6 4 2 0`.  The
/// compiler now rejects the unsupported order with an actionable diagnostic.
#[test]
fn for_range_step_by_before_rev_is_rejected() {
    let fixture = repo_root().join("tests/vertical-slice/reject/for_range_step_by_before_rev.hew");
    assert!(fixture.exists(), "fixture missing: {}", fixture.display());

    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&fixture)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected `hew check` to reject `step_by().rev()`; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        combined.contains("`step_by` before `rev` is unsupported"),
        "expected the actionable diagnostic; got: {combined}",
    );
}

/// Named functions used as first-class values: passed as arguments,
/// stored in let bindings, and called through a local binding.
/// Also guards that lambda literals continue to work (regression guard).
///
/// Before this fix, `apply(double, 7)` printed nothing and exited 0 because
/// the wildcard `BindingRef { .. } => None` arm in `lower_value` silently
/// swallowed the `ResolvedRef::Item` case, causing the argument to be missing
/// from the call and the let-binding `r` to have no backend slot.
#[test]
fn named_fn_as_value_four_line_oracle() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/named_fn_as_value.hew");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "named_fn_as_value should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(
        actual, "14\n6\n10\n21\n",
        "expected four lines 14/6/10/21; got: {actual:?}"
    );
}

/// Regression: the same named function used as a value at more than one call
/// site previously caused duplicate/undefined shim symbols in the LLVM module
/// (`__hew_named_fn_invoke_double` and `__hew_named_fn_invoke_double.1`),
/// failing module verification with exit 125.  The fix deduplicates shims at
/// the module-collection level so the body is emitted exactly once regardless
/// of how many sites reference the same named function.
#[test]
fn named_fn_value_reused_across_sites() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/named_fn_value_reused.hew");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "named_fn_value_reused should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(
        actual, "2\n20\n30\n100\n",
        "expected 2/20/30/100; got: {actual:?}"
    );
}

/// Named function returned from a function and called through the result.
///
/// Before this fix, `get_double()` returned an uninitialised closure pair
/// because `lower_value(double)` returned None in the return-statement path,
/// causing `f(7)` to call through a garbage function pointer (exit 1,
/// no output).
#[test]
fn named_fn_returned_and_called() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/named_fn_return.hew");

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "named_fn_return should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "14\n", "expected 14; got: {actual:?}");
}

/// Inline capturing closure: `base` is an untyped local integer binding.
///
/// Previously failed at HIR with `E_HIR: CheckerBoundaryViolation` because
/// the `check_against` const-values coercion path in the type checker called
/// `env.lookup` instead of `synthesize_identifier`, bypassing the
/// `lambda_capture_depth` depth-check that registers closure captures.
#[test]
fn capturing_closure_inline_runs() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/capturing_closure_inline.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "capturing_closure_inline should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "15\n10\n", "expected 15/10; got: {actual:?}");
}

/// Returned capturing closure: a function returns a closure that captures a
/// parameter binding.
///
/// Previously failed at MIR with `E_MIR_CHECK: InitialisedBeforeUse` because
/// `MirStatement::Use` was emitted unconditionally for all `BindingRef` nodes,
/// including captured bindings handled via `ClosureEnvFieldLoad`. The dataflow
/// checker saw the outer binding id as `Uninit` in the closure shim context.
#[test]
fn capturing_closure_returned_runs() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/capturing_closure_returned.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "capturing_closure_returned should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "8\n", "expected 8; got: {actual:?}");
}

/// Returned capturing closure survives the producing frame being clobbered.
///
/// Previously the closure env stayed a stack alloca in the producer's frame
/// even though the checker classified the literal `Escapes`; deep unrelated
/// calls between `make_adder` and the closure call overwrote the frame and
/// the capture read garbage. The env is now heap-promoted at the literal
/// site (`Instr::MakeClosure` `HeapBox` mode) and freed exactly once by the
/// pair's scope-exit `DropKind::ClosurePair` drop.
#[test]
fn returned_closure_survives_stack_clobber() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/closure_env_heap_promotion.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "closure_env_heap_promotion should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "15\n", "expected 15; got: {actual:?}");
}

/// `Vec<fn(i64) -> i64>` storage surface: array literal, indexing, push,
/// get, and pop over boxed-pair elements riding the pointer-element ABI.
#[test]
fn vec_of_fn_storage_ops_run() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/vec_of_fn_storage.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "vec_of_fn_storage should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(
        actual, "2\n6\n101\n105\n2\n",
        "expected 2/6/101/105/2; got: {actual:?}"
    );
}

/// A vec of capture-carrying closures releases each element's env box and
/// pair box exactly once at scope exit (`hew_vec_free_closure_pairs`).
#[test]
fn vec_of_fn_capturing_closures_drop_clean() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/vec_of_fn_drop_discipline.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "vec_of_fn_drop_discipline should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "25\n3\n", "expected 25/3; got: {actual:?}");
}

/// A record with a fn-typed field constructs, stores a capturing closure,
/// returns by value (env heap-live across the return), and drops the env
/// exactly once via the record drop spine.
#[test]
fn record_fn_field_round_trips() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/record_fn_field.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "record_fn_field should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "42\n11\n7\n", "expected 42/11/7; got: {actual:?}");
}

/// `h.cb(args)` on a fn-typed record field dispatches as a field-load +
/// closure call; repeated dispatch borrows the pair (never consumes).
#[test]
fn record_fn_field_method_dispatch_runs() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/record_fn_field_dispatch.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "record_fn_field_dispatch should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(
        actual, "42\n123\n101\n",
        "expected 42/123/101; got: {actual:?}"
    );
}

/// Closure-valued actor state fails closed at compile time (restart needs
/// the clone direction; a closure env has a sole owner and no clone path).
#[test]
fn check_actor_state_closure_field_fails_closed() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/reject/actor_state_closure_field.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("closure-valued actor state is not supported"),
        "expected fail-closed diagnostic; got: {combined}"
    );
}

/// Non-generic cross-module named functions are first-class values: stored,
/// passed, returned, held as Vec elements and record fields, and invoked
/// through each path. The fixture exercises all six behaviours against a
/// seven-line oracle.
#[test]
fn cross_module_fn_value_seven_line_oracle() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/cross_module_fn_value/main.hew");
    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "cross_module_fn_value should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/cross_module_fn_value/main.expected"),
    )
    .expect("read main.expected");
    assert_eq!(
        actual, expected,
        "expected the seven oracle lines from main.expected; got: {actual:?}"
    );
}

/// A local binding that shadows an imported module name resolves as a field
/// access, not a fn-value `BindingRef`.  The checker's `receiver_is_binding`
/// guard records i64 (record field) for `helpers.double`; the HIR must
/// honour that resolution even when the `fn_registry` has a hit for the
/// qualified key.
#[test]
fn cross_module_fn_value_shadowed_local_binding_resolves_as_field() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/accept/cross_module_fn_value_shadow/main.hew");
    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "shadowing fixture should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/cross_module_fn_value_shadow/main.expected"),
    )
    .expect("read main.expected");
    assert_eq!(
        actual, expected,
        "expected field access to print 7; got: {actual:?}"
    );
}

/// An unannotated GENERIC cross-module named function used as a value is
/// rejected: the type parameters cannot be determined from context and the
/// diagnostic names the function and asks for an annotation.
#[test]
fn check_cross_module_generic_fn_value_ambiguous_rejected() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/reject/cross_module_generic_fn_value/main.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("is a generic function"),
        "expected diagnostic to name the generic function; got: {combined}"
    );
    assert!(
        combined.contains("type annotation"),
        "expected the diagnostic to ask for a type annotation; got: {combined}"
    );
    assert!(
        !combined.contains("has no exported constant"),
        "must not misreport an exported function as a missing constant; got: {combined}"
    );
}

/// A context-determined GENERIC cross-module named function used as a value
/// is now accepted (A156): the type parameters are inferred from the binding
/// annotation, the monomorphisation is registered, and the compiled binary
/// produces the expected output.
#[test]
fn cross_module_generic_fn_value_context_determined_accepted() {
    require_codegen();

    let source =
        repo_root().join("tests/vertical-slice/accept/cross_module_generic_fn_value/main.hew");
    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "cross_module_generic_fn_value should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/cross_module_generic_fn_value/main.expected"),
    )
    .expect("read main.expected");
    assert_eq!(
        actual, expected,
        "expected the oracle lines from main.expected; got: {actual:?}"
    );
}

/// A private (non-pub) enum returned from a pub fn across a module boundary
/// must compile and run correctly.  Previously the §4b pre-pass cached the
/// `HirTypeDecl` for all enums, but the fourth-pass emission guard was missing
/// the enum arm, so the private enum's layout never reached MIR and produced
/// `E_MIR: unknown type`.  Regression fixture for #2195.
#[test]
fn cross_module_private_enum_returned_from_pub_fn() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/cross_module_private_enum/main.hew");
    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "cross_module_private_enum should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/cross_module_private_enum/main.expected"),
    )
    .expect("read main.expected");
    assert_eq!(
        actual, expected,
        "expected 'not found' from cross_module_private_enum; got: {actual:?}"
    );
}

/// A Vec element type containing a function value fails closed at the
/// checker (the layout/owned byte-copy would alias the sole-owner env).
#[test]
fn check_vec_of_record_with_fn_field_fails_closed() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/reject/vec_of_record_with_fn_field.hew");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "expected check to fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("the element type contains a function value"),
        "expected fail-closed diagnostic; got: {combined}"
    );
}

/// Runs `hew check` on a reject fixture and returns the combined output
/// after asserting the check failed.
fn check_fails(fixture: &str) -> String {
    let source = repo_root().join(fixture);
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&source)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");
    assert!(
        !output.status.success(),
        "expected check to fail for {fixture}; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

/// Affine closure-pair discipline: storing the same closure binding in two
/// record fields is a use-after-move (both records would free one env),
/// rejected at check time naming the first store as the move site.
#[test]
fn check_closure_shared_across_records_fails_closed() {
    require_codegen();
    let combined = check_fails("tests/vertical-slice/reject/closure_shared_across_records.hew");
    assert!(
        combined.contains("is used after it was consumed"),
        "expected use-after-move diagnostic; got: {combined}"
    );
    assert!(
        combined.contains("binding consumed here"),
        "expected the diagnostic to name the move site; got: {combined}"
    );
}

/// Affine closure-pair discipline: pushing the same closure binding into
/// two Vec slots is a use-after-move (two slots would free one env).
#[test]
fn check_closure_double_vec_push_fails_closed() {
    require_codegen();
    let combined = check_fails("tests/vertical-slice/reject/closure_double_vec_push.hew");
    assert!(
        combined.contains("is used after it was consumed"),
        "expected use-after-move diagnostic; got: {combined}"
    );
}

/// Affine closure-pair discipline: Vec push then record store of the same
/// binding is a use-after-move (mixed-container double consumption).
#[test]
fn check_closure_vec_push_then_record_fails_closed() {
    require_codegen();
    let combined = check_fails("tests/vertical-slice/reject/closure_vec_push_then_record.hew");
    assert!(
        combined.contains("is used after it was consumed"),
        "expected use-after-move diagnostic; got: {combined}"
    );
}

/// A rebind transfers closure-pair ownership; invoking the source binding
/// afterwards is a use-after-move (move-then-invoke). The inverse order is
/// the accept fixture `closure_invoke_then_move.hew`.
#[test]
fn check_closure_move_then_invoke_fails_closed() {
    require_codegen();
    let combined = check_fails("tests/vertical-slice/reject/closure_move_then_invoke.hew");
    assert!(
        combined.contains("is used after it was consumed"),
        "expected use-after-move diagnostic; got: {combined}"
    );
}

/// A Vec element read is a borrow — storing it into another vec would give
/// the second vec ownership of an env the first vec already owns. Refused
/// outright (the store itself is the corruption, no later use needed).
#[test]
fn check_closure_borrowed_element_store_fails_closed() {
    require_codegen();
    let combined = check_fails("tests/vertical-slice/reject/closure_borrowed_element_store.hew");
    assert!(
        combined.contains("borrows a closure environment owned elsewhere"),
        "expected borrowed-store diagnostic; got: {combined}"
    );
}

/// A forwarded fn-typed parameter's closure env is provably heap (the checker
/// Escapes-classifies a closure crossing a call boundary as an argument), so
/// storing it into an owning record field transfers env ownership into the
/// record. The record becomes the sole owner that frees the env once at its
/// drop, and the stored closure stays callable through the field.
/// `make_handler(make_adder(7))` then `h.action(35)` dispatches 35 + 7 = 42.
#[test]
fn closure_param_field_store_runs() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/closure_param_field_store.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert_eq!(
        output.status.code(),
        Some(42),
        "make_handler stored closure must dispatch 35 + 7 = 42 through the field; \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

/// Storing a forwarded fn-typed parameter into a record field is the
/// parameter's single consumption (the record becomes the sole env owner);
/// using the parameter AFTER that store is a use-after-consume, rejected by
/// the dataflow checker — never a silent miscompile or a runtime double-free.
/// The diagnostic is `UseAfterConsume`, not the old `ClosurePairBorrowedStore`
/// (the store itself is now legal; the later use is the error).
#[test]
fn check_closure_param_use_after_store_fails_closed() {
    require_codegen();
    let combined = check_fails("tests/vertical-slice/reject/closure_param_use_after_store.hew");
    assert!(
        combined.contains("is used after it was consumed"),
        "expected use-after-consume diagnostic; got: {combined}"
    );
    assert!(
        combined.contains("UseAfterConsume"),
        "expected UseAfterConsume MIR kind; got: {combined}"
    );
    assert!(
        !combined.contains("ClosurePairBorrowedStore"),
        "store is now legal; the error must be the later use, not a borrowed-store: {combined}"
    );
}

/// Invocation borrows the pair; a later owning store is the binding's
/// single consumption. Invoke-then-move runs clean and the new owner
/// dispatches and drops the env exactly once.
#[test]
fn closure_invoke_then_move_runs() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/closure_invoke_then_move.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "closure_invoke_then_move should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, "41\n42\n", "expected 41/42; got: {actual:?}");
}

/// Spec §3.8.6 Vec pipeline: `map`/`filter`/`reduce` expand to counted loops
/// (closure bound once, called per element), including the chained form and
/// a capture-carrying predicate.
#[test]
fn vec_pipeline_map_filter_reduce_runs() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/vec_pipeline_map_filter_reduce.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "vec_pipeline_map_filter_reduce should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(
        actual, "10\n2\n15\n120\n9\n",
        "expected 10/2/15/120/9; got: {actual:?}"
    );
}

/// `scope { while { println } }`: calls inside nested control-flow within a
/// scope block must NOT be treated as spawned tasks.
///
/// Previously failed at HIR with `TaskSpawnSignatureUnsupported` and
/// `TaskSpawnCalleeUnsupported` because `lower_block` did not reset
/// `scope_depth` to 0, causing the `scope_depth > 0` guard in
/// `lower_expression_stmt_kind` to intercept all calls at any nesting depth
/// inside a scope body.
#[test]
fn scope_nested_while_println_runs() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/scope_while_println.hew");
    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "scope_while_println should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(
        actual, "tick\ntick\ntick\n",
        "expected three tick lines; got: {actual:?}"
    );
}

/// Value-class capstone (RC-6) — a user record carrying an owned `string` field
/// is constructed, returned by value across the MIR boundary, round-tripped
/// through a `let` binding, field-read, and dropped at scope exit. Before this
/// lane MIR rejected it with `UnsupportedUserRecordValueClass` (W3.029).
#[test]
fn owned_record_string_field_by_value_round_trips() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("owned_record_string.hew");
    std::fs::write(
        &path,
        r#"
        type CommandOutput {
            stdout: string;
            code: i64;
        }

        fn run() -> CommandOutput {
            CommandOutput { stdout: "ok", code: 7 }
        }

        fn main() {
            let o = run();
            println(o.stdout);
            println(f"{o.code}");
        }
        "#,
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, dir.path());
    assert!(
        output.status.success(),
        "owned-string-record by value should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "ok\n7\n");
}

/// Value-class capstone (G12) — a user record aggregating a `Vec<i64>` field is
/// constructed (the highest-value shape: rejected at MIR even on construction
/// before this fix), returned by value, field-read, and dropped at scope exit.
#[test]
fn owned_record_vec_field_by_value_round_trips() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("owned_record_vec.hew");
    std::fs::write(
        &path,
        r"
        type Histogram {
            counts: Vec<i64>;
            total: i64;
        }

        fn build() -> Histogram {
            let v: Vec<i64> = Vec::new();
            v.push(10);
            v.push(20);
            Histogram { counts: v, total: 30 }
        }

        fn main() {
            let h = build();
            println(h.counts.len());
            println(h.total);
        }
        ",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, dir.path());
    assert!(
        output.status.success(),
        "owned-Vec-field record by value should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "2\n30\n");
}

/// Value-class capstone — a record nested inside another record (both owning
/// heap) round-trips by value; the outer record's drop thunk recurses into the
/// nested owned-record field.
#[test]
fn owned_nested_record_by_value_round_trips() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("owned_nested_record.hew");
    std::fs::write(
        &path,
        r#"
        type User {
            name: string;
        }

        type Boxed {
            user: User;
            tag: i64;
        }

        fn wrap(n: i64) -> Boxed {
            let u = User { name: "ada" };
            Boxed { user: u, tag: n }
        }

        fn main() {
            let b = wrap(99);
            println(b.user.name);
            println(f"{b.tag}");
        }
        "#,
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, dir.path());
    assert!(
        output.status.success(),
        "nested owned record by value should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "ada\n99\n");
}

/// `import std::encoding::json` then using the imported `#[opaque]` `Value`
/// handle. Guards the regression where an imported opaque type reached the
/// LLVM emitter under its module-qualified name (`json.Value`) while the
/// opaque-name set was keyed by the bare decl name (`Value`), tripping the D10
/// fail-closed sentinel ("Named/user type `json.Value` reached the LLVM
/// emitter"). The fix matches the short name in codegen's opaque-ptr decision
/// (`hew-codegen-rs/src/llvm.rs`). Exercises a trivial pass-through handle
/// method (`get_int`) and `free`, asserting the runtime round-trip.
#[test]
fn run_imports_json_opaque_handle_round_trips() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("json_opaque.hew");
    std::fs::write(
        &hew_src,
        "import std::encoding::json;\n\
         \n\
         fn main() -> i32 {\n\
         \x20   let v = json.from_int(42);\n\
         \x20   let n = v.get_int();\n\
         \x20   v.free();\n\
         \x20   println(f\"n={n}\");\n\
         \x20   0\n\
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
    assert_eq!(String::from_utf8_lossy(&output.stdout), "n=42\n");
}

/// `import std::encoding::json` then chaining the fluent builder methods.
/// Guards the regression where non-trivial imported impl methods (a void C
/// call followed by `return self`, e.g. `with_int` / `push_int`) were absent
/// from `fn_registry` and the lowered item list because imported impl-method
/// registration was gated on per-method `pub` visibility (which impl methods
/// never carry). Across the import boundary they surfaced as
/// `IndirectCallUnsupported` / `CallableUnsupportedInMir`. The fix drops the
/// `pub` gate so HIR matches the checker-authoritative `fn_sigs`
/// (`hew-hir/src/lower.rs`). Exercises an object builder + array builder +
/// serialize + reparse, asserting the runtime byte-level output.
#[test]
fn run_imports_json_fluent_builders_round_trip() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("json_builders.hew");
    std::fs::write(
        &hew_src,
        "import std::encoding::json;\n\
         \n\
         fn main() -> i32 {\n\
         \x20   let obj = json.object()\n\
         \x20       .with_string(\"name\", \"Hew\")\n\
         \x20       .with_int(\"version\", 1);\n\
         \x20   let s = obj.stringify();\n\
         \x20   println(s);\n\
         \x20   let parsed = json.parse(s);\n\
         \x20   let field = parsed.get_field(\"version\");\n\
         \x20   println(f\"version={field.get_int()}\");\n\
         \x20   field.free();\n\
         \x20   parsed.free();\n\
         \x20   obj.free();\n\
         \x20   let arr = json.array().push_int(1).push_int(2).push_int(3);\n\
         \x20   println(f\"len={arr.array_len()}\");\n\
         \x20   arr.free();\n\
         \x20   0\n\
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
        "{\"name\":\"Hew\",\"version\":1}\nversion=1\nlen=3\n",
    );
}

/// W5.021 — a function returning a `(Sink<string>, Stream<string>)` tuple of
/// owned handles compiles, links, and runs with exactly-once teardown. This is
/// the exact `std::stream::pipe` / `Connection::into_stream_sink` shape that was
/// fail-closed before the tuple/record-of-owned-handles drop spine.
///
/// Exercises the full spine end-to-end through link + exec:
/// - the callee (`make_pair` and `pipe`) returns a tuple literal of handle
///   bindings, which must NOT be dropped at the callee's function exit
///   (move-out, defect #1);
/// - the caller destructures the tuple — the `__tuple_N` temp must NOT be
///   dropped once its elements are moved out (defect #3);
/// - the explicit `.close()` on `sink` consumes the receiver so its scope-exit
///   drop does not fire again (consume-intent, defect #2);
/// - `input` is closed implicitly by the per-element handle drop at scope exit.
///
/// A double-close would `Box::from_raw` twice and SIGSEGV; a missed drop would
/// leak. Asserting clean exit + stdout is the value oracle (`pr_test_plans`).
#[test]
fn run_tuple_of_owned_handles_returns_and_drops_exactly_once() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("tuple_handle_drop.hew");
    std::fs::write(
        &hew_src,
        "import std::stream;\n\
         \n\
         fn make_pair() -> (Sink<string>, Stream<string>) {\n\
         \x20   stream.pipe(8)\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let (sink, input) = make_pair();\n\
         \x20   sink.send(\"alpha\");\n\
         \x20   sink.close();\n\
         \x20   input.close();\n\
         \x20   println(\"pair-ok\");\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&hew_src, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed (no double-free / leak); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "pair-ok\n");
}

/// W5.021 — a `(Sink<string>, Stream<string>)` tuple bound WHOLE (not
/// destructured) and dropped at scope exit exercises the `DropKind::TupleInPlace`
/// per-element drop helper (`__hew_tuple_drop_inplace_<key>`), the genuine
/// tuple-in-place path the destructure case bypasses. The helper must close both
/// halves exactly once; a double-free SIGSEGVs, a missed drop leaks.
#[test]
fn run_whole_tuple_of_handles_drops_each_member_once() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("whole_tuple_handle_drop.hew");
    std::fs::write(
        &hew_src,
        "import std::stream;\n\
         \n\
         fn main() {\n\
         \x20   let pair = stream.pipe(8);\n\
         \x20   println(\"whole-ok\");\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&hew_src, repo_root());

    assert!(
        output.status.success(),
        "hew run should succeed (tuple-in-place drop, exactly once); stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "whole-ok\n");
}

// ---------------------------------------------------------------------------
// W5.021 defect #1 — returned-aggregate member drop spine.
//
// The exactly-once ORACLE these tests use is the cheap authoritative one the
// independent review used: a callee that RETURNS an aggregate of owned
// handles must elaborate an EMPTY drop-plan for that return — no
// `Stream::close` / `Sink::close` — because the caller (who received the
// byte-copied aggregate) now owns the members. A non-empty plan IS the
// double-free: two `Box::from_raw` of one allocation (the runtime close is an
// unguarded free; see the codegen Stream/Sink drop comment). `leaks --atExit`
// does NOT flag an un-closed handle Box and exit-success does not prove
// no-double-free, so this dump-mir assertion is the real oracle; the paired
// `hew run` success is the runtime negative-control. These shapes (let-bound
// return, if/match tails, nested) all SIGSEGV'd before the value-flow
// `derive_returned_aggregate_member_bindings` fix.
// ---------------------------------------------------------------------------

/// Compile `source` with `--dump-mir elab` and return the number of
/// `kind=duplex_half_close` drops in `callee`'s elaborated body. The
/// oracle: a callee returning an owned-handle aggregate must report ZERO.
fn callee_handle_close_drops(source: &str, callee: &str) -> usize {
    let dir = support::tempdir();
    let hew_src = dir.path().join("oracle.hew");
    std::fs::write(&hew_src, source).unwrap();
    let out = support::run_hew_in(
        repo_root(),
        &["compile", "--dump-mir", "elab", hew_src.to_str().unwrap()],
    );
    assert!(
        out.status.success(),
        "dump-mir elab must succeed; stderr: {}",
        String::from_utf8_lossy(&out.stderr),
    );
    let dump = String::from_utf8_lossy(&out.stdout);
    // Slice the dump to the named callee function (from its `fn <name> ->` header
    // to the next function header) so closes belonging to `main` (which
    // legitimately closes its own destructured handles) are not counted.
    // The structured MIR renderer emits `fn {name} -> {ret}` (not `name: "{name}"`).
    let header = format!("fn {callee} ->");
    let start = dump
        .find(&header)
        .unwrap_or_else(|| panic!("callee `{callee}` not found in MIR dump:\n{dump}"));
    let rest = &dump[start + header.len()..];
    // Next function starts with `\nfn ` in the structured renderer.
    let end = rest.find("\nfn ").map_or(rest.len(), |i| i);
    let body = &rest[..end];
    // Structured renderer emits `kind=duplex_half_close(recv)` for Stream drops
    // and `kind=duplex_half_close(send)` for Sink drops.
    body.matches("duplex_half_close").count()
}

/// Compile `source` with `--dump-mir elab` and return the number of owned
/// HANDLE-place releases (`Duplex::close` `drop_fn` / `LambdaActorRelease` drop
/// kind) in `callee`'s elaborated body. The oracle: a callee returning an
/// aggregate of owned handle-place members (`Duplex`/lambda-actor handles) must
/// report ZERO — the members are handed to the caller.
///
/// A separate counter from `callee_handle_close_drops` because handle-place
/// members register in `binding_locals` as their handle Place (not a `Local`),
/// elaborate a `kind=lambda_actor_release`/`kind=duplex_close` drop (not
/// `kind=duplex_half_close`), AND fail closed at codegen-front (the
/// `SendHalf`/`RecvHalf`/`LambdaActorHandle` Place lowering is unwired), so
/// the runtime negative-control the Stream/Sink shapes use is impossible — this
/// dump-mir assertion is the only oracle.
fn callee_handle_release_drops(source: &str, callee: &str) -> usize {
    let dir = support::tempdir();
    let hew_src = dir.path().join("oracle.hew");
    std::fs::write(&hew_src, source).unwrap();
    let out = support::run_hew_in(
        repo_root(),
        &["compile", "--dump-mir", "elab", hew_src.to_str().unwrap()],
    );
    assert!(
        out.status.success(),
        "dump-mir elab must succeed; stderr: {}",
        String::from_utf8_lossy(&out.stderr),
    );
    let dump = String::from_utf8_lossy(&out.stdout);
    // Structured renderer emits `fn {name} -> {ret}` (not `name: "{name}"`).
    let header = format!("fn {callee} ->");
    let start = dump
        .find(&header)
        .unwrap_or_else(|| panic!("callee `{callee}` not found in MIR dump:\n{dump}"));
    let rest = &dump[start + header.len()..];
    // Next function starts with `\nfn ` in the structured renderer.
    let end = rest.find("\nfn ").map_or(rest.len(), |i| i);
    let body = &rest[..end];
    // Structured renderer emits `kind=lambda_actor_release` (not `LambdaActorRelease`).
    body.matches("lambda_actor_release").count()
}

/// Oracle: a `(Sink, Stream)` tuple let-bound then returned BY NAME
/// (`let pair = (s, r); pair`) — the most ordinary form — must leave the callee
/// with an empty return drop-plan. The syntactic move-out missed this (it only
/// saw the tail `BindingRef(pair)`), so `s`/`r` stayed drop-eligible and the
/// callee double-freed. Value-flow follows the constructed tuple into `ReturnSlot`.
#[test]
fn returned_let_bound_tuple_callee_does_not_drop_members() {
    require_codegen();
    let closes = callee_handle_close_drops(
        "import std::stream;\n\
         fn make_pair() -> (Sink<string>, Stream<string>) {\n\
         \x20   let (s, r) = stream.pipe(8);\n\
         \x20   let pair = (s, r);\n\
         \x20   pair\n\
         }\n\
         fn main() {\n\
         \x20   let (sink, input) = make_pair();\n\
         \x20   sink.close();\n\
         \x20   input.close();\n\
         }\n",
        "make_pair",
    );
    assert_eq!(
        closes, 0,
        "callee returning a let-bound tuple of handles must not drop its members \
         (double-free); got {closes} handle closes in its drop-plan"
    );
}

/// Oracle: a `(Sink, Stream)` returned from an `if`-expression tail. `If` is a
/// distinct `HirExprKind` the syntactic walk never matched → double-free.
#[test]
fn returned_if_tail_tuple_callee_does_not_drop_members() {
    require_codegen();
    let closes = callee_handle_close_drops(
        "import std::stream;\n\
         fn make_pair(c: bool) -> (Sink<string>, Stream<string>) {\n\
         \x20   let (s, r) = stream.pipe(8);\n\
         \x20   if c { (s, r) } else { (s, r) }\n\
         }\n\
         fn main() {\n\
         \x20   let (sink, input) = make_pair(true);\n\
         \x20   sink.close();\n\
         \x20   input.close();\n\
         }\n",
        "make_pair",
    );
    assert_eq!(
        closes, 0,
        "callee returning a tuple from an if-tail must not drop its members; \
         got {closes} handle closes"
    );
}

/// Oracle: a `(Sink, Stream)` returned from a `match`-expression tail.
#[test]
fn returned_match_tail_tuple_callee_does_not_drop_members() {
    require_codegen();
    let closes = callee_handle_close_drops(
        "import std::stream;\n\
         fn make_pair(c: bool) -> (Sink<string>, Stream<string>) {\n\
         \x20   let (s, r) = stream.pipe(8);\n\
         \x20   match c {\n\
         \x20       true => (s, r),\n\
         \x20       false => (s, r),\n\
         \x20   }\n\
         }\n\
         fn main() {\n\
         \x20   let (sink, input) = make_pair(true);\n\
         \x20   sink.close();\n\
         \x20   input.close();\n\
         }\n",
        "make_pair",
    );
    assert_eq!(
        closes, 0,
        "callee returning a tuple from a match-tail must not drop its members; \
         got {closes} handle closes"
    );
}

/// Return the `--dump-mir checked` text for `source`.
///
/// Runs the compiler from the repo root so that stdlib imports (`std::stream`,
/// `std::net`, etc.) are resolvable; the source file is written to a tempdir
/// and passed by absolute path.
fn mir_checked_dump(source: &str) -> String {
    let dir = support::tempdir();
    let hew_src = dir.path().join("oracle.hew");
    std::fs::write(&hew_src, source).unwrap();
    let out = support::run_hew_in(
        repo_root(),
        &[
            "compile",
            "--dump-mir",
            "checked",
            hew_src.to_str().unwrap(),
        ],
    );
    assert!(
        out.status.success(),
        "dump-mir checked must succeed; stderr: {}",
        String::from_utf8_lossy(&out.stderr),
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// NEW-7 oracle: `await stream.recv()` / `await sink.send(x)` over a
/// `Stream<bytes>` / `Sink<bytes>` in an actor handler (an execution-context
/// caller) flip to the suspending terminators; a context-free caller (`main`,
/// free fn) keeps the blocking `hew_stream_next_layout` / `hew_sink_write_bytes`
/// call.
#[test]
fn suspending_stream_recv_send_flip_in_execution_context() {
    let dump = mir_checked_dump(
        "import std::stream;\n\
         #[opaque]\n\
         type Pair {}\n\
         extern \"C\" {\n\
         \x20   fn hew_stream_channel(capacity: i64) -> Pair;\n\
         \x20   fn hew_stream_pair_sink_bytes(pair: Pair) -> Sink<bytes>;\n\
         \x20   fn hew_stream_pair_stream_bytes(pair: Pair) -> Stream<bytes>;\n\
         \x20   fn hew_stream_pair_free(pair: Pair);\n\
         \x20   fn hew_string_to_bytes(s: string) -> bytes;\n\
         }\n\
         actor Runner {\n\
         \x20   receive fn go(unused: i64) {\n\
         \x20       let pair = unsafe { hew_stream_channel(4) };\n\
         \x20       let sink = unsafe { hew_stream_pair_sink_bytes(pair) };\n\
         \x20       let input = unsafe { hew_stream_pair_stream_bytes(pair) };\n\
         \x20       unsafe { hew_stream_pair_free(pair); }\n\
         \x20       await sink.send(unsafe { hew_string_to_bytes(\"a\") });\n\
         \x20       sink.close();\n\
         \x20       let item = await input.recv();\n\
         \x20       match item { Some(v) => {}, None => {}, }\n\
         \x20   }\n\
         }\n\
         fn main() { let r = spawn Runner(); r.go(0); }\n",
    );
    // After the SuspendKind side-table collapse the old carrier-spelled
    // terminators (`SuspendingStreamNext`, `SuspendingStreamSend`) no longer
    // exist as Terminator variants; both lower to the bare `Terminator::Suspend`
    // (rendering: `suspend is_final=...`).  The actor body has two await sites
    // (recv + send), so two bare suspend terminators must be present.
    let suspend_count = dump.matches("suspend is_final=").count();
    assert!(
        suspend_count >= 2,
        "actor `await stream.recv()` + `await sink.send()` must each lower to \
         a bare `suspend is_final=` terminator (expected >=2, found {suspend_count}):\n{dump}"
    );
}

/// NEW-2 oracle: `await listener.accept()` in an actor handler (an
/// execution-context caller) flips to the `SuspendingAccept` terminator; a
/// context-free caller (`main`, free fn) keeps the blocking `hew_tcp_accept`
/// call. The listener-readiness sibling of the conn-read flip.
#[test]
fn suspending_listener_accept_flip_in_execution_context() {
    let dump = mir_checked_dump(
        "import std::net;\n\
         actor Acceptor {\n\
         \x20   let addr: string;\n\
         \x20   receive fn go(unused: i64) {\n\
         \x20       let listener = net.listen(addr);\n\
         \x20       let conn = await listener.accept();\n\
         \x20       let _ = conn.close();\n\
         \x20       let _ = listener.close();\n\
         \x20   }\n\
         }\n\
         fn main() {\n\
         \x20   let a = spawn Acceptor(addr: \"127.0.0.1:0\");\n\
         \x20   a.go(0);\n\
         }\n",
    );
    // After the SuspendKind side-table collapse `SuspendingAccept` no longer
    // exists as a Terminator variant; it lowers to the bare `Terminator::Suspend`
    // (rendering: `suspend is_final=...`).
    assert!(
        dump.contains("suspend is_final="),
        "actor `await listener.accept()` must lower to a bare `suspend is_final=` \
         terminator (SuspendKind::Accept in the side-table):\n{dump}"
    );
}

/// NEW-2 negative: a context-free caller (`fn main`) keeps the BLOCKING accept
/// (`hew_tcp_accept`); the caller-conv flip must NOT emit `SuspendingAccept`
/// where there is no parkable continuation (mirrors the conn-read negative).
#[test]
fn blocking_listener_accept_in_main_keeps_blocking_call() {
    let dump = mir_checked_dump(
        "import std::net;\n\
         fn main() {\n\
         \x20   let listener = net.listen(\"127.0.0.1:0\");\n\
         \x20   let conn = await listener.accept();\n\
         \x20   let _ = conn.close();\n\
         \x20   let _ = listener.close();\n\
         }\n",
    );
    assert!(
        !dump.contains("SuspendingAccept") && !dump.contains("suspend.accept"),
        "`await listener.accept()` from main must NOT flip to SuspendingAccept:\n{dump}"
    );
    assert!(
        dump.contains("hew_tcp_accept"),
        "`await listener.accept()` from main must keep the blocking hew_tcp_accept:\n{dump}"
    );
}

/// NEW-5 oracle: a cross-node `peer.ask(msg, timeout)` in an actor handler (an
/// execution-context caller) flips to the `SuspendingRemoteAsk` terminator — the
/// caller parks its coroutine on the wire reply instead of blocking a worker; a
/// context-free caller (`main`, free fn) keeps the blocking `RemoteAsk`. The
/// cross-node sibling of the local-ask flip.
#[test]
fn suspending_remote_ask_flip_in_execution_context() {
    let dump = mir_checked_dump(
        "actor Echo {\n\
         \x20   receive fn handle(req: i64) -> i64 { req }\n\
         }\n\
         impl ActorMsg for Echo {\n\
         \x20   type Msg = i64;\n\
         \x20   type Reply = i64;\n\
         }\n\
         actor Client {\n\
         \x20   receive fn go(unused: i64) {\n\
         \x20       let found: Result<RemotePid<Echo>, LookupError> = Node::lookup(\"echo\");\n\
         \x20       match found {\n\
         \x20           Ok(peer) => { let _ = peer.ask(7, 1000); },\n\
         \x20           Err(_) => {},\n\
         \x20       }\n\
         \x20   }\n\
         }\n\
         fn main() { let c = spawn Client(); c.go(0); }\n",
    );
    // After the SuspendKind side-table collapse `SuspendingRemoteAsk` no longer
    // exists as a Terminator variant; it lowers to the bare `Terminator::Suspend`
    // (rendering: `suspend is_final=...`).
    assert!(
        dump.contains("suspend is_final="),
        "actor-handler `peer.ask()` must lower to a bare `suspend is_final=` \
         terminator (SuspendKind::RemoteAsk in the side-table):\n{dump}"
    );
}

/// NEW-5 negative: a context-free caller (`fn main`) keeps the BLOCKING remote
/// ask (`RemoteAsk`); the caller-conv flip must NOT emit `SuspendingRemoteAsk`
/// where there is no parkable continuation (mirrors the local-ask negative).
#[test]
fn blocking_remote_ask_in_main_keeps_blocking_terminator() {
    let dump = mir_checked_dump(
        "actor Echo {\n\
         \x20   receive fn handle(req: i64) -> i64 { req }\n\
         }\n\
         impl ActorMsg for Echo {\n\
         \x20   type Msg = i64;\n\
         \x20   type Reply = i64;\n\
         }\n\
         fn main() {\n\
         \x20   let found: Result<RemotePid<Echo>, LookupError> = Node::lookup(\"echo\");\n\
         \x20   match found {\n\
         \x20       Ok(peer) => { let _ = peer.ask(7, 1000); },\n\
         \x20       Err(_) => {},\n\
         \x20   }\n\
         }\n",
    );
    assert!(
        !dump.contains("SuspendingRemoteAsk") && !dump.contains("suspend.remote_ask"),
        "`peer.ask()` from main must NOT flip to SuspendingRemoteAsk:\n{dump}"
    );
    assert!(
        dump.contains("RemoteAsk") || dump.contains("remote_ask"),
        "`peer.ask()` from main must keep the blocking RemoteAsk terminator:\n{dump}"
    );
}

/// Oracle: a NESTED owned aggregate `((Sink,), Stream)` returned by name. The
/// value-flow decomposition must recurse through the inner `TupleConstruct`.
#[test]
fn returned_nested_tuple_callee_does_not_drop_members() {
    require_codegen();
    let closes = callee_handle_close_drops(
        "import std::stream;\n\
         fn make_nested() -> ((Sink<string>,), Stream<string>) {\n\
         \x20   let (s, r) = stream.pipe(8);\n\
         \x20   let inner = (s,);\n\
         \x20   let pair = (inner, r);\n\
         \x20   pair\n\
         }\n\
         fn main() {\n\
         \x20   let ((sink,), input) = make_nested();\n\
         \x20   sink.close();\n\
         \x20   input.close();\n\
         }\n",
        "make_nested",
    );
    assert_eq!(
        closes, 0,
        "callee returning a nested tuple of handles must not drop any member \
         (the value-flow walk recurses into the inner tuple); got {closes} closes"
    );
}

/// Oracle: a RECORD of owned handles let-bound then returned by name — the
/// record analogue of the let-bound-tuple double-free. `RecordInit` element
/// sources must be followed into the return.
#[test]
fn returned_record_of_handles_callee_does_not_drop_fields() {
    require_codegen();
    let closes = callee_handle_close_drops(
        "import std::stream;\n\
         type Pipe { sink: Sink<string>, input: Stream<string> }\n\
         fn make_pipe() -> Pipe {\n\
         \x20   let (s, r) = stream.pipe(8);\n\
         \x20   let p = Pipe { sink: s, input: r };\n\
         \x20   p\n\
         }\n\
         fn main() {\n\
         \x20   let p = make_pipe();\n\
         \x20   p.sink.close();\n\
         \x20   p.input.close();\n\
         }\n",
        "make_pipe",
    );
    assert_eq!(
        closes, 0,
        "callee returning a record of handles must not drop its fields; \
         got {closes} handle closes"
    );
}

/// Oracle: a tuple of owned HANDLE-place members (lambda-actor `LambdaPid` handles)
/// returned by a direct tail. Handle members register in `binding_locals` as
/// their handle Place, so they surface as `TupleConstruct` elements as that
/// handle Place; the value-flow pass originally gated member sources on
/// `Place::Local(_)` and dropped the handle members on the floor, leaving the
/// callee to double-release them after the caller received the tuple. The pass
/// must now admit owned handle places — callee Return drop-plan empty.
///
/// No `require_codegen` / runtime control: handle-place lowering fails closed at
/// codegen-front (`SendHalf`/`RecvHalf`/`LambdaActorHandle` Place lowering is
/// unwired), so the native binary cannot be produced. The dump-mir Return-plan
/// assertion is the only oracle. Non-tautological: the pre-fix binary emits
/// `LambdaActorRelease` drops here.
#[test]
fn returned_handle_tuple_callee_does_not_drop_members() {
    let releases = callee_handle_release_drops(
        "fn make_pair() -> (LambdaPid<i64, ()>, LambdaPid<i64, ()>) {\n\
         \x20   let a = actor |x: i64| { println(f\"a {x}\"); };\n\
         \x20   let b = actor |x: i64| { println(f\"b {x}\"); };\n\
         \x20   (a, b)\n\
         }\n\
         fn main() {\n\
         \x20   let (a, b) = make_pair();\n\
         \x20   a.send(1);\n\
         \x20   println(\"done\");\n\
         }\n",
        "make_pair",
    );
    assert_eq!(
        releases, 0,
        "callee returning a tuple of owned handle members must not release them \
         (double-free); got {releases} LambdaActorRelease drops in its drop-plan"
    );
}

/// Oracle: a tuple of owned handle members returned through a let-bound rebind
/// tail (`let pair = (a, b); pair`). The value-flow pass must follow the
/// whole-value rebind into `ReturnSlot` and decompose the `TupleConstruct`'s
/// handle-place elements. Callee Return drop-plan empty.
#[test]
fn returned_let_bound_handle_tuple_callee_does_not_drop_members() {
    let releases = callee_handle_release_drops(
        "fn make_pair() -> (LambdaPid<i64, ()>, LambdaPid<i64, ()>) {\n\
         \x20   let a = actor |x: i64| { println(f\"a {x}\"); };\n\
         \x20   let b = actor |x: i64| { println(f\"b {x}\"); };\n\
         \x20   let pair = (a, b);\n\
         \x20   pair\n\
         }\n\
         fn main() {\n\
         \x20   let (a, b) = make_pair();\n\
         \x20   a.send(1);\n\
         \x20   println(\"done\");\n\
         }\n",
        "make_pair",
    );
    assert_eq!(
        releases, 0,
        "callee returning a let-bound tuple of owned handle members must not \
         release them; got {releases} LambdaActorRelease drops"
    );
}

/// Oracle: a tuple of owned handle members returned from a `match`-expression
/// tail (two `TupleConstruct`s flowing to one `ReturnSlot`). The pass must admit
/// the handle-place members of every flowing construct. Callee Return drop-plan
/// empty.
#[test]
fn returned_match_tail_handle_tuple_callee_does_not_drop_members() {
    let releases = callee_handle_release_drops(
        "fn make_pair(c: bool) -> (LambdaPid<i64, ()>, LambdaPid<i64, ()>) {\n\
         \x20   let a = actor |x: i64| { println(f\"a {x}\"); };\n\
         \x20   let b = actor |x: i64| { println(f\"b {x}\"); };\n\
         \x20   match c {\n\
         \x20       true => (a, b),\n\
         \x20       false => (a, b),\n\
         \x20   }\n\
         }\n\
         fn main() {\n\
         \x20   let (a, b) = make_pair(true);\n\
         \x20   a.send(1);\n\
         \x20   println(\"done\");\n\
         }\n",
        "make_pair",
    );
    assert_eq!(
        releases, 0,
        "callee returning a tuple of owned handle members from a match-tail must \
         not release them; got {releases} LambdaActorRelease drops"
    );
}

/// Runtime negative-control: the let-bound-return shape (p7) must RUN to a clean
/// exit. Before the fix this SIGSEGV'd (exit 139) on the callee's double-free.
/// The caller destructures and explicitly closes both handles (the success
/// path), so there is no leak either.
#[test]
fn run_let_bound_tuple_return_no_double_free() {
    require_codegen();
    let dir = support::tempdir();
    let hew_src = dir.path().join("let_bound_return.hew");
    std::fs::write(
        &hew_src,
        "import std::stream;\n\
         fn make_pair() -> (Sink<string>, Stream<string>) {\n\
         \x20   let (s, r) = stream.pipe(8);\n\
         \x20   let pair = (s, r);\n\
         \x20   pair\n\
         }\n\
         fn main() {\n\
         \x20   let (sink, input) = make_pair();\n\
         \x20   sink.send(\"alpha\");\n\
         \x20   sink.close();\n\
         \x20   input.close();\n\
         \x20   println(\"bound-ok\");\n\
         }\n",
    )
    .unwrap();
    let output = run_bounded_hew_run(&hew_src, repo_root());
    assert!(
        output.status.success(),
        "let-bound tuple return must not double-free; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "bound-ok\n");
}

/// Runtime negative-control for the `if`-tail return shape (p7b). Before the fix
/// this SIGSEGV'd; the `If` arm was never walked by the syntactic move-out.
#[test]
fn run_if_tail_tuple_return_no_double_free() {
    require_codegen();
    let dir = support::tempdir();
    let hew_src = dir.path().join("if_tail_return.hew");
    std::fs::write(
        &hew_src,
        "import std::stream;\n\
         fn make_pair(c: bool) -> (Sink<string>, Stream<string>) {\n\
         \x20   let (s, r) = stream.pipe(8);\n\
         \x20   if c { (s, r) } else { (s, r) }\n\
         }\n\
         fn main() {\n\
         \x20   let (sink, input) = make_pair(true);\n\
         \x20   sink.send(\"alpha\");\n\
         \x20   sink.close();\n\
         \x20   input.close();\n\
         \x20   println(\"if-ok\");\n\
         }\n",
    )
    .unwrap();
    let output = run_bounded_hew_run(&hew_src, repo_root());
    assert!(
        output.status.success(),
        "if-tail tuple return must not double-free; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "if-ok\n");
}

/// Runtime: a RECORD of owned handles returned and run end-to-end, with the
/// caller explicitly closing both fields (the record success path). Confirms the
/// `RecordInit` return spine closes each handle exactly once — no double-free,
/// no leak. (The `is_heap_owning_record_composite_return` + `RecordInPlace` path
/// that the boundary admits but the lane never executed.)
#[test]
fn run_record_of_handles_return_drops_each_field_once() {
    require_codegen();
    let dir = support::tempdir();
    let hew_src = dir.path().join("record_handle_return.hew");
    std::fs::write(
        &hew_src,
        "import std::stream;\n\
         type Pipe { sink: Sink<string>, input: Stream<string> }\n\
         fn make_pipe() -> Pipe {\n\
         \x20   let (s, r) = stream.pipe(8);\n\
         \x20   let p = Pipe { sink: s, input: r };\n\
         \x20   p\n\
         }\n\
         fn main() {\n\
         \x20   let p = make_pipe();\n\
         \x20   p.sink.send(\"alpha\");\n\
         \x20   p.sink.close();\n\
         \x20   p.input.close();\n\
         \x20   println(\"record-ok\");\n\
         }\n",
    )
    .unwrap();
    let output = run_bounded_hew_run(&hew_src, repo_root());
    assert!(
        output.status.success(),
        "record of handles return + explicit close must run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "record-ok\n");
}

// ── `clone <expr>` duplication prefix ────────────────────────────────────
//
// End-to-end coverage for the canonical duplication surface. These exercise
// the whole pipeline (parse → check → HIR → MIR → codegen → run), which is the
// only layer where the prefix's runtime effect is observable: `Expr::Clone`
// is erased to an ordinary `.clone()` method call during HIR lowering, so MIR
// and codegen never see a clone-specific node.

/// A cloned string survives a consuming actor send while the original stays
/// usable: `clone s` produces an independent owned value.
#[test]
fn clone_string_survives_consuming_send() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("clone_string_send.hew");
    std::fs::write(
        &path,
        "actor Sink { let id: i64; receive fn take(s: string) -> i64 { s.len() } }\n\
         fn main() {\n\
         \x20   let s: string = \"hello\";\n\
         \x20   let dup = clone s;\n\
         \x20   let sink = spawn Sink(id: 0);\n\
         \x20   let n = await sink.take(dup);\n\
         \x20   match n { Ok(len) => println(f\"len={len}\"), Err(_) => println(\"ask failed\") }\n\
         \x20   println(f\"original still usable: {s}\");\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, repo_root());
    assert!(
        output.status.success(),
        "clone-before-consume should run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("len=5") && stdout.contains("original still usable: hello"),
        "expected both the consumed clone's length and the surviving original; got: {stdout}"
    );
}

/// Formatting a value (original or clone) is non-consuming: both bindings are
/// usable across multiple interpolations.
#[test]
fn clone_then_format_is_non_consuming() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("clone_format.hew");
    std::fs::write(
        &path,
        "fn main() {\n\
         \x20   let s: string = \"world\";\n\
         \x20   let dup = clone s;\n\
         \x20   println(f\"hello {dup}, len={s.len()}\");\n\
         \x20   println(f\"again {s} and {dup}\");\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, repo_root());
    assert!(
        output.status.success(),
        "clone + formatting should run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "hello world, len=5\nagain world and world\n"
    );
}

/// `clone xs` on a `Vec` produces an independent copy: mutating the duplicate
/// leaves the original's length unchanged.
#[test]
fn clone_vec_is_independent_copy() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("clone_vec.hew");
    std::fs::write(
        &path,
        "fn main() {\n\
         \x20   let xs: Vec<i64> = Vec::new();\n\
         \x20   xs.push(1); xs.push(2);\n\
         \x20   let dup = clone xs;\n\
         \x20   dup.push(99);\n\
         \x20   println(f\"original_len={xs.len()} dup_len={dup.len()}\");\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, repo_root());
    assert!(
        output.status.success(),
        "clone of a Vec should run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "original_len=2 dup_len=3\n"
    );
}

/// Regression: the existing `x.clone()` method form still runs unchanged.
#[test]
fn existing_vec_method_clone_still_runs() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("method_clone.hew");
    std::fs::write(
        &path,
        "fn main() {\n\
         \x20   let xs: Vec<i64> = Vec::new();\n\
         \x20   xs.push(1); xs.push(2);\n\
         \x20   let b = xs.clone();\n\
         \x20   b.push(99);\n\
         \x20   println(f\"a={xs.len()} b={b.len()}\");\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, repo_root());
    assert!(
        output.status.success(),
        "`x.clone()` should still run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "a=2 b=3\n");
}

/// `&x` is not an expression: Hew has no prefix borrow. The diagnostic must
/// reject it and steer the author to `clone x`.
#[test]
fn ampersand_expression_is_rejected() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("ampersand_reject.hew");
    std::fs::write(
        &path,
        "fn main() {\n\
         \x20   let x = 5;\n\
         \x20   let y = &x;\n\
         \x20   println(y);\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, repo_root());
    assert!(
        !output.status.success(),
        "`&x` must be rejected; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("not a prefix operator") && combined.contains("clone"),
        "expected the `&`-rejection diagnostic pointing at `clone`; got: {combined}"
    );
}

/// `clone` on a type with no clone method fails closed, exactly as the
/// equivalent `.clone()` call would — no silent success.
#[test]
fn clone_on_unsupported_scalar_fails_closed() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("clone_unsupported.hew");
    std::fs::write(
        &path,
        "fn main() {\n\
         \x20   let n = 5;\n\
         \x20   let m = clone n;\n\
         \x20   println(m);\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, repo_root());
    assert!(
        !output.status.success(),
        "`clone` on an unsupported type must fail closed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("clone") && combined.contains("i64"),
        "expected a fail-closed `clone`-on-`i64` diagnostic; got: {combined}"
    );
}

/// Regression for ecosystem Blocker 2 (file-import half): an actor defined in a
/// sibling file pulled in with `import "counter.hew";` must spawn and answer
/// `ask` calls exactly like a local actor. File imports flatten the imported
/// items into the root program under their bare names, so the unqualified
/// `spawn Counter()` is the correct surface.
///
/// This also guards the dedup path in the imported-module HIR walk: the
/// file-import actor is already emitted by the source-order pass, and the walk
/// must NOT emit a second `HirItem::Actor`. Before the dedup guard the
/// duplicate collided at MIR (`ActorHandlerSymbolCollision`, the bare actor
/// name keys the handler layout).
#[test]
fn run_file_imported_actor_spawns_and_calls() {
    require_codegen();

    let dir = support::tempdir();
    std::fs::write(
        dir.path().join("counter.hew"),
        "pub actor Counter {\n\
         \x20   var n: i64 = 0;\n\
         \x20   receive fn bump() -> i64 {\n\
         \x20       n = n + 1;\n\
         \x20       n\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import \"counter.hew\";\n\
         fn main() {\n\
         \x20   let c = spawn Counter();\n\
         \x20   match await c.bump() {\n\
         \x20       Ok(v) => println(f\"bumped: {v}\"),\n\
         \x20       Err(_) => println(\"err\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "file-imported actor spawn/call should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "bumped: 1\n");
}

/// Revision-2 regression: a FILE-IMPORTED actor whose receive body exercises
/// checker-owned `SpanKey`-keyed facts that are consumed during HIR body
/// lowering — a closure literal (`closure_capture_facts` / `closure_escape_facts`)
/// and a `for i in 0..n` range loop (`deferred_range_bounds`).
///
/// File-import flattening splices the imported actor into the root program AFTER
/// type-checking, so the checker validated the body under its originating
/// module's non-root `current_module_idx` (stamping every fact at index N) while
/// HIR formerly lowered the spliced item at the default root index 0. Every
/// `mk_key` lookup then missed and the fail-closed contracts fired
/// (`ActorStateGuardMissing` at the spawn, and — had the body reached closure /
/// range lowering — `ClosureCaptureModeUnresolved` / range-bound diagnostics).
/// The fix makes HIR lower spliced file-import items under the same module index
/// the checker used, so all body-level facts resolve. This guards the closure
/// and range-bound facts on the file-import path specifically.
#[test]
fn run_file_imported_actor_closure_and_range_body_runs() {
    require_codegen();

    let dir = support::tempdir();
    std::fs::write(
        dir.path().join("summer.hew"),
        "pub actor Summer {\n\
         \x20   var total: i64 = 0;\n\
         \x20   receive fn add_doubled(n: i64) -> i64 {\n\
         \x20       let f = |x: i64| -> i64 { x * 2 };\n\
         \x20       var sum: i64 = 0;\n\
         \x20       for i in 0..n {\n\
         \x20           sum = sum + f(i);\n\
         \x20       }\n\
         \x20       total = total + sum;\n\
         \x20       total\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import \"summer.hew\";\n\
         fn main() {\n\
         \x20   let s = spawn Summer();\n\
         \x20   match await s.add_doubled(4) {\n\
         \x20       Ok(v) => println(f\"sum: {v}\"),\n\
         \x20       Err(_) => println(\"err\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "file-imported actor with closure + range body should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    // 2*(0+1+2+3) = 12
    assert_eq!(String::from_utf8_lossy(&output.stdout), "sum: 12\n");
}

/// Regression for ecosystem Blocker 2 (package-import half): an actor exported
/// from a separate compilation unit (`import hew::bank;`, resolved via the
/// source-relative `hew/<pkg>/` layout) must spawn with `spawn bank.Account(...)`
/// and answer `ask` calls like a local actor. Before the fix, package imports
/// were registered only in the module graph and never lowered into the root
/// program, so MIR had no actor layout and failed with
/// `E_NOT_YET_IMPLEMENTED: MIR lowering for spawn of unknown actor`, while the
/// module-qualified spawn result type failed HIR boundary conversion.
///
/// Exercises the full imported-actor surface: an `init` with a named arg,
/// multiple `receive fn`s, state persisted across asks, and a *private*
/// same-module free function (`clamp_nonneg`) called from the actor body —
/// which forces the imported-private-fn closure to seed from actor bodies and
/// the same-module call rewrites to reach into the imported actor body.
#[test]
fn run_package_module_actor_spawns_and_calls() {
    require_codegen();

    let dir = support::tempdir();
    let pkg_dir = dir.path().join("hew").join("bank");
    std::fs::create_dir_all(&pkg_dir).unwrap();
    std::fs::write(
        pkg_dir.join("hew.toml"),
        "[package]\nname = \"hew::bank\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        pkg_dir.join("bank.hew"),
        "fn clamp_nonneg(x: i64) -> i64 {\n\
         \x20   if x < 0 { 0 } else { x }\n\
         }\n\
         \n\
         pub actor Account {\n\
         \x20   var balance: i64 = 0;\n\
         \x20   init(opening: i64) {\n\
         \x20       balance = clamp_nonneg(opening);\n\
         \x20   }\n\
         \x20   receive fn deposit(amount: i64) -> i64 {\n\
         \x20       balance = balance + clamp_nonneg(amount);\n\
         \x20       balance\n\
         \x20   }\n\
         \x20   receive fn withdraw(amount: i64) -> i64 {\n\
         \x20       let take = clamp_nonneg(amount);\n\
         \x20       if take > balance {\n\
         \x20           return balance;\n\
         \x20       }\n\
         \x20       balance = balance - take;\n\
         \x20       balance\n\
         \x20   }\n\
         \x20   receive fn peek() -> i64 {\n\
         \x20       balance\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew::bank;\n\
         \n\
         fn report(label: string, r: Result<i64, AskError>) {\n\
         \x20   match r {\n\
         \x20       Ok(v) => println(f\"{label}={v}\"),\n\
         \x20       Err(_) => println(f\"{label}=ERR\"),\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let acct = spawn bank.Account(opening: 100);\n\
         \x20   report(\"after_open\", await acct.peek());\n\
         \x20   report(\"after_deposit\", await acct.deposit(50));\n\
         \x20   report(\"after_overdraw\", await acct.withdraw(1000));\n\
         \x20   report(\"after_withdraw\", await acct.withdraw(30));\n\
         \x20   report(\"final\", await acct.peek());\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "package-module actor spawn/call should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "after_open=100\n\
         after_deposit=150\n\
         after_overdraw=150\n\
         after_withdraw=120\n\
         final=120\n",
    );
}

/// Fail-closed guard for Blocker 2: a *non-pub* actor in an imported package is
/// not exported, so `spawn secret.Hidden()` must fail rather than silently
/// resolve. The fix deliberately lowers `HirItem::Actor` only for `pub` imported
/// actors; a private one is never emitted, so MIR rejects the spawn as an
/// unknown actor. This proves the fix did not introduce a broad fallback that
/// would spawn arbitrary unknown actors across the import boundary.
#[test]
fn run_non_pub_imported_actor_fails_closed() {
    require_codegen();

    let dir = support::tempdir();
    let pkg_dir = dir.path().join("hew").join("secret");
    std::fs::create_dir_all(&pkg_dir).unwrap();
    std::fs::write(
        pkg_dir.join("secret.hew"),
        "actor Hidden {\n\
         \x20   var n: i64 = 0;\n\
         \x20   receive fn bump() -> i64 {\n\
         \x20       n = n + 1;\n\
         \x20       n\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew::secret;\n\
         fn main() {\n\
         \x20   let c = spawn secret.Hidden();\n\
         \x20   match await c.bump() {\n\
         \x20       Ok(v) => println(f\"bumped: {v}\"),\n\
         \x20       Err(_) => println(\"err\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        !output.status.success(),
        "non-pub imported actor must fail closed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("Hidden"),
        "expected a fail-closed diagnostic naming the unknown actor `Hidden`; got: {combined}"
    );
}

/// Qualified actor identity (a): two imported packages each exporting a
/// `pub actor` with the same bare name (`Account`) coexist in one program.
/// Identity is the qualified (module, name) pair end-to-end — the checker
/// types `spawn bank.Account()` as `LocalPid<bank.Account>`, MIR layouts key
/// on the dotted name, and native symbols mangle through `bank$Account` — so
/// each spawn binds its own handlers/state/drop glue and the asks route to
/// the right actor.
#[test]
fn run_two_packages_same_actor_name_both_spawn_and_ask() {
    require_codegen();

    let dir = support::tempdir();
    for (pkg, tag) in [("bank", 1_i64), ("store", 2_i64)] {
        let pkg_dir = dir.path().join("hew").join(pkg);
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(
            pkg_dir.join(format!("{pkg}.hew")),
            format!(
                "pub actor Account {{\n\
                 \x20   var n: i64 = 0;\n\
                 \x20   receive fn who() -> i64 {{ {tag} }}\n\
                 }}\n"
            ),
        )
        .unwrap();
    }
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew::bank;\n\
         import hew::store;\n\
         fn main() {\n\
         \x20   let a = spawn bank.Account();\n\
         \x20   let s = spawn store.Account();\n\
         \x20   match await a.who() {\n\
         \x20       Ok(v) => println(f\"a={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   match await s.who() {\n\
         \x20       Ok(v) => println(f\"s={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "two packages with the same actor name must both spawn and answer; \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("a=1") && stdout.contains("s=2"),
        "each ask must route to its own module's actor (a=1, s=2); got: {stdout}"
    );
}

/// Qualified actor identity (b): a root-local actor and an imported
/// `pub actor` with the same bare name (`Account`) coexist. The qualified
/// spawn (`bank.Account`) routes to the package actor and the bare spawn
/// resolves local-first to the root actor — neither shadows the other.
#[test]
fn run_root_and_package_same_actor_name_route_independently() {
    require_codegen();

    let dir = support::tempdir();
    let pkg_dir = dir.path().join("hew").join("bank");
    std::fs::create_dir_all(&pkg_dir).unwrap();
    std::fs::write(
        pkg_dir.join("bank.hew"),
        "pub actor Account {\n\
         \x20   var n: i64 = 0;\n\
         \x20   receive fn who() -> i64 { 999 }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew::bank;\n\
         actor Account {\n\
         \x20   var n: i64 = 0;\n\
         \x20   receive fn who() -> i64 { 111 }\n\
         }\n\
         fn main() {\n\
         \x20   let a = spawn bank.Account();\n\
         \x20   let l = spawn Account();\n\
         \x20   match await a.who() {\n\
         \x20       Ok(v) => println(f\"a={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   match await l.who() {\n\
         \x20       Ok(v) => println(f\"l={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "root + package same-named actors must both spawn and answer; \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("a=999") && stdout.contains("l=111"),
        "qualified spawn must route to the package actor (a=999) and the \
         bare spawn local-first to the root actor (l=111); got: {stdout}"
    );
}

/// Qualified actor identity (c): a supervisor over two same-named module
/// actors (`child b: bank.Account` / `child s: store.Account`, the dotted
/// child-type form) spawns both, routes asks per child, and a crash+restart
/// of one child rebinds ITS OWN qualified dispatch/state glue — the restarted
/// bank child still answers 1 and the untouched store child still answers 2.
#[test]
fn run_supervisor_two_same_named_module_actor_children_restart_routes() {
    require_codegen();

    let dir = support::tempdir();
    for (pkg, tag) in [("bank", 1_i64), ("store", 2_i64)] {
        let pkg_dir = dir.path().join("hew").join(pkg);
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(
            pkg_dir.join(format!("{pkg}.hew")),
            format!(
                "pub actor Account {{\n\
                 \x20   var n: i64 = 0;\n\
                 \x20   receive fn who() -> i64 {{ {tag} }}\n\
                 \x20   receive fn boom() {{ panic(\"{pkg} crash\"); }}\n\
                 }}\n"
            ),
        )
        .unwrap();
    }
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew::bank;\n\
         import hew::store;\n\
         supervisor Pair {\n\
         \x20   strategy: one_for_one;\n\
         \x20   intensity: 5 within 60s;\n\
         \n\
         \x20   child b: bank.Account;\n\
         \x20   child s: store.Account;\n\
         }\n\
         fn main() {\n\
         \x20   let p = spawn Pair;\n\
         \x20   sleep(50ms);\n\
         \x20   let b = p.b;\n\
         \x20   let s = p.s;\n\
         \x20   match await b.who() {\n\
         \x20       Ok(v) => println(f\"b={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   match await s.who() {\n\
         \x20       Ok(v) => println(f\"s={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   b.boom();\n\
         \x20   sleep(200ms);\n\
         \x20   let b2 = p.b;\n\
         \x20   match await b2.who() {\n\
         \x20       Ok(v) => println(f\"b2={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   let s2 = p.s;\n\
         \x20   match await s2.who() {\n\
         \x20       Ok(v) => println(f\"s2={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "supervisor over two same-named module actors must run; \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("b=1")
            && stdout.contains("s=2")
            && stdout.contains("b2=1")
            && stdout.contains("s2=2"),
        "per-child qualified routing must survive a restart \
         (b=1, s=2, b2=1, s2=2); got: {stdout}"
    );
}

/// Security regression (private-actor routing): a root/pub actor `Account` and
/// a *private* (non-pub) imported actor `secret.Account` must NOT let
/// `spawn secret.Account()` silently route to the root actor. Module-qualified
/// actor identity is bare-name-keyed in HIR/MIR, and a private actor is not an
/// export of its module, so the spawn must fail closed at type-check (before
/// the qualifier is stripped to bare `Account`) rather than spawn the root
/// `Account`. Verifies neither the root actor (`a=111`) nor the private actor
/// (`a=999`) ever runs.
#[test]
fn run_private_imported_actor_does_not_route_to_root_actor() {
    require_codegen();

    let dir = support::tempdir();
    let pkg_dir = dir.path().join("hew").join("secret");
    std::fs::create_dir_all(&pkg_dir).unwrap();
    // Note: no `pub` — the actor is private to its module.
    std::fs::write(
        pkg_dir.join("secret.hew"),
        "actor Account {\n\
         \x20   var n: i64 = 0;\n\
         \x20   receive fn id() -> i64 { 999 }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew::secret;\n\
         actor Account {\n\
         \x20   var n: i64 = 0;\n\
         \x20   receive fn id() -> i64 { 111 }\n\
         }\n\
         fn main() {\n\
         \x20   let a = spawn secret.Account();\n\
         \x20   match await a.id() {\n\
         \x20       Ok(v) => println(f\"a={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        !output.status.success(),
        "spawn of a private imported actor must fail closed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    // Must NOT have run either actor — the compile must reject before codegen,
    // and in particular must NOT silently route to the root `Account` (111).
    assert!(
        !combined.contains("a=111") && !combined.contains("a=999"),
        "private imported actor spawn must be rejected before any actor runs; got: {combined}"
    );
    assert!(
        combined.contains("has no exported actor `Account`") && combined.contains("secret"),
        "expected a fail-closed diagnostic that `secret` has no exported actor `Account`; \
         got: {combined}"
    );
}

/// Security regression (non-actor export routing): an imported module that
/// exports a *public non-actor type* named `Account` (e.g. `pub type Account`)
/// must NOT satisfy a module-qualified spawn `spawn secret.Account()` and route
/// to a same-named root actor. Module-qualified spawn is gated on the qualified
/// definition being an actor (`TypeDefKind::Actor`), not merely a public type
/// export, so the spawn fails closed at type-check before the qualifier is
/// stripped to bare `Account`. Verifies the root actor (`a=111`) never runs.
#[test]
fn run_non_actor_export_does_not_route_to_root_actor() {
    require_codegen();

    let dir = support::tempdir();
    let pkg_dir = dir.path().join("hew").join("secret");
    std::fs::create_dir_all(&pkg_dir).unwrap();
    // `secret` exports a public *non-actor* type named `Account`.
    std::fs::write(
        pkg_dir.join("secret.hew"),
        "pub type Account {\n\
         \x20   balance: i64,\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew::secret;\n\
         actor Account {\n\
         \x20   var n: i64 = 0;\n\
         \x20   receive fn id() -> i64 { 111 }\n\
         }\n\
         fn main() {\n\
         \x20   let a = spawn secret.Account();\n\
         \x20   match await a.id() {\n\
         \x20       Ok(v) => println(f\"a={v}\"),\n\
         \x20       Err(_) => println(\"e\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        !output.status.success(),
        "spawn of a non-actor module export must fail closed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    // Must NOT have routed to the root actor (`a=111`) -- reject before codegen.
    assert!(
        !combined.contains("a=111"),
        "non-actor export must not route to the root actor; got: {combined}"
    );
    assert!(
        combined.contains("has no exported actor `Account`") && combined.contains("secret"),
        "expected a fail-closed diagnostic that `secret` has no exported actor `Account`; \
         got: {combined}"
    );
}

/// Arg-bearing fork callees under the poisoned allocator: the fixture moves
/// a heap `string` into a fork-entry shim env via the named form
/// (`fork ts = shout(greeting); await ts;`). `MallocScribble`/`MallocPreScribble`
/// poison freed and fresh allocations, so a child reading the env after a
/// premature parent-side free deterministically corrupts the output (or
/// aborts) instead of passing by luck. The structural single-owner proof
/// lives in `hew-mir/tests/cancellation_scope.rs`
/// (`fork_string_arg_parent_and_shim_emit_no_drops_for_moved_arg`); this is
/// the behavioural freed-read guard on the emitted native binary.
#[test]
fn run_fork_args_spawn_scribbled_no_freed_read() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/accept/fork_args_spawn.hew");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/fork_args_spawn.expected"),
    )
    .expect("read fork_args_spawn.expected");

    let mut command = support::hew_command();
    command
        .arg("run")
        .arg(&source)
        .current_dir(repo_root())
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1");
    let output =
        support::run_bounded_command(command, format!("hew run {} (scribbled)", source.display()));

    assert!(
        output.status.success(),
        "fork_args_spawn must run cleanly under the poisoned allocator; \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {}", source.display());
}

/// Init-closure wall: a supervised child whose actor init-parameter type is not
/// reproducible by the init-closure restart thunk must be rejected at `hew
/// check` with `E_SUPERVISOR_INIT_ARG_NON_BITCOPY` before reaching codegen.
///
/// Admitted types (the thunk can reproduce them on every restart):
///   - scalar `BitCopy` primitives (loaded directly)
///   - owned `string` / `bytes` (deep-cloned per incarnation)
///
/// Rejected types (clone-in-thunk codegen not wired, or structurally
/// forbidden for handles):
///   - owned collections (`Vec`, `HashMap`, `HashSet`)
///   - user records, enums, tuples, type aliases, generic types
///   - `#[resource]` handle types
#[test]
fn check_supervisor_init_arg_non_bitcopy_rejected() {
    require_codegen();
    let combined = check_fails("tests/vertical-slice/reject/supervisor_init_arg_non_bitcopy.hew");
    assert!(
        combined.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY"),
        "expected E_SUPERVISOR_INIT_ARG_NON_BITCOPY diagnostic; got: {combined}"
    );
    assert!(
        combined.contains("Vec<i64>"),
        "diagnostic must name the rejected type; got: {combined}"
    );
    assert!(
        combined.contains("init args are re-produced by the init-closure restart model"),
        "diagnostic must describe the init-closure model; got: {combined}"
    );
}

#[test]
fn check_supervisor_init_arg_non_bitcopy_evasions_rejected() {
    require_codegen();
    let combined =
        check_fails("tests/vertical-slice/reject/supervisor_init_arg_non_bitcopy_evasions.hew");
    assert!(
        combined.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY"),
        "expected E_SUPERVISOR_INIT_ARG_NON_BITCOPY diagnostic; got: {combined}"
    );
    // Each still-walled type must appear in the combined diagnostic output.
    // `string` (now admitted via deep-clone in the init thunk) is not listed.
    for expected_type in [
        "Option<string>",
        "(string, i64)",
        "Wrapper",
        "HashMap<string, i64>",
        "HashSet<i64>",
    ] {
        assert!(
            combined.contains(expected_type),
            "diagnostic must name rejected type `{expected_type}`; got: {combined}"
        );
    }
    assert!(
        combined.contains("init args are re-produced by the init-closure restart model"),
        "diagnostic must describe the init-closure model; got: {combined}"
    );
}

/// Leak oracle: a config supervisor whose children ALL use literal init args
/// (no `config.field` reads) must not leak the config buffer at supervisor
/// start. Before the S1 fix, codegen malloc'd the config buffer unconditionally
/// when `config_param.is_some()`, but never adopted it into the runtime (no
/// child called `set_child_init_fn`), so sizeof(AppConfig) leaked per
/// supervisor start.
///
/// Compiles `supervisor_literal_only_config_param.hew` to a native binary and
/// runs it under `leaks --atExit` on macOS.  A non-zero leak count is a
/// regression.  On non-macOS or when `leaks` is not on PATH the test skips.
#[test]
fn supervisor_literal_only_config_param_no_leak() {
    require_codegen();

    // `leaks --atExit` is macOS-only.
    if !cfg!(target_os = "macos") {
        eprintln!("skip: leaks(1) is macOS-only");
        return;
    }

    // leaks(1) hangs attaching to the supervisor fixture on the GitHub macOS
    // runner (macos-14, 3-core, SIP-enforced) — the 480s budget bump did not
    // help, confirming a real leaks(1) attachment hang rather than slowness.
    // This test is the local dev-time proof; the fix itself is verified by
    // running the test on a developer machine (where leaks(1) exits in ~6.7s
    // with 0 leaks).  Skip in CI rather than mask a genuine future regression
    // behind a timeout-and-retry cycle.
    //
    // Follow-up: investigate why leaks(1) hangs on this multithreaded
    // supervisor fixture on the GitHub macOS runner — it may be SIP preventing
    // leaks(1) from attaching to a binary that spawns OS threads (the
    // supervisor's actor runtime).
    if std::env::var("CI").is_ok() {
        eprintln!(
            "skip: supervisor_literal_only_config_param_no_leak — \
             leaks(1) hangs attaching to this supervisor fixture on the \
             GitHub macOS runner; the no-leak fix is verified locally \
             (0 leaks, ~6.7s)"
        );
        return;
    }

    let leaks_ok = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_ok {
        eprintln!("skip: leaks(1) not found on PATH");
        return;
    }

    // Compile the literal-only fixture to a native binary.
    let dir = support::tempdir();
    let source =
        repo_root().join("tests/vertical-slice/accept/supervisor_literal_only_config_param.hew");
    let compile_out = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.path().to_str().expect("emit-dir utf-8"),
            source.to_str().expect("source utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        compile_out.status.success(),
        "supervisor_literal_only_config_param compile failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&compile_out.stdout),
        String::from_utf8_lossy(&compile_out.stderr),
    );
    let stdout_str = String::from_utf8_lossy(&compile_out.stdout);
    let bin_path: std::path::PathBuf = stdout_str
        .lines()
        .find_map(|l| l.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("no `native:` line in compile output:\n{stdout_str}"))
        .into();

    // Run the binary under `leaks --atExit` + the poisoned-allocator triple.
    let leaks_out = Command::new("leaks")
        .arg("--atExit")
        .arg("--")
        .arg(&bin_path)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("invoke leaks");

    let report = String::from_utf8_lossy(&leaks_out.stdout);
    // Parse the "Process <pid>: N leaks for M bytes" summary line.  If leaks
    // produced no usable report (SIP prevents attachment on some CI
    // configurations) skip rather than fail.
    let leak_count: Option<usize> = report.lines().find_map(|line| {
        if !line.contains(" leaks for ") && !line.contains(" leak for ") {
            return None;
        }
        let rest = line.strip_prefix("Process ")?;
        if !rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            return None;
        }
        let after_colon = rest.split_once(": ").map(|(_, s)| s)?;
        after_colon
            .split_whitespace()
            .next()
            .and_then(|n| n.parse::<usize>().ok())
    });

    match leak_count {
        None => {
            eprintln!(
                "skip: leaks(1) did not emit a parseable summary \
                 (SIP / CI sandbox may prevent attachment);\nstdout:\n{report}\nstderr:\n{}",
                String::from_utf8_lossy(&leaks_out.stderr)
            );
        }
        Some(n) => {
            assert_eq!(
                n, 0,
                "supervisor_literal_only_config_param: {n} leak(s) detected — \
                 the config buffer was malloc'd but not adopted by the runtime \
                 (S1 fix regression);\nleaks report:\n{report}"
            );
        }
    }
}
