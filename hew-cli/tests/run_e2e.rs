mod support;

use std::process::Command;
use std::time::{Duration, Instant};

use support::{hew_binary, repo_root, require_codegen, run_bounded_hew_run, strip_ansi};

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

#[cfg(unix)]
#[test]
fn run_native_signal_prints_typed_failure() {
    require_codegen();
    let dir = support::tempdir();
    let path = dir.path().join("signal_run.hew");
    // SIGKILL cannot be intercepted by the runtime's fault handlers. This
    // exercises the parent CLI's diagnostic without invalid memory access.
    std::fs::write(
        &path,
        format!(
            "extern \"C\" {{ fn raise(signal: i32) -> i32; }}\nfn main() {{ unsafe {{ raise({}); }} }}\n",
            libc::SIGKILL
        ),
    )
    .unwrap();
    for timeout in [false, true] {
        let mut command = Command::new(hew_binary());
        command.arg("run");
        if timeout {
            command.args(["--timeout", "10s"]);
        }
        command.arg(&path).current_dir(dir.path());
        let output = support::run_bounded_command(command, "native signal diagnostic");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.code(), Some(1), "{stderr}");
        assert!(stderr.contains("hew: failure: UnknownFault"), "{stderr}");
        assert!(stderr.contains("SIGKILL"), "{stderr}");
        assert!(!stderr.contains("timed out"), "{stderr}");
    }
}

#[test]
fn run_native_preserves_explicit_exit() {
    require_codegen();
    let dir = support::tempdir();
    let path = dir.path().join("explicit_exit.hew");
    std::fs::write(&path, "fn main() -> i32 { 37 }\n").unwrap();
    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&path).current_dir(dir.path());
    let output = support::run_bounded_command(command, "native explicit exit");
    assert_eq!(output.status.code(), Some(37));
    assert!(output.stderr.is_empty(), "{:?}", output.stderr);
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

/// #3132: `hew run`'s compiled artifact lives under `$TMPDIR/hew-run/<pid>-
/// <rand>/` and RAII drop removes it on a normal exit. Points the child's
/// temp root at an isolated directory (via TMPDIR/TMP/TEMP so the check
/// holds on both the Unix and Windows temp-dir lookup) and asserts nothing
/// remains under `hew-run/` once the process has exited cleanly.
#[test]
fn run_normal_exit_leaves_no_hew_run_artifact_dir_behind() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("trivial_run.hew");
    std::fs::write(&path, "fn main() {\n    println(1);\n}\n").unwrap();

    let tmp_root = support::tempdir();
    let mut command = Command::new(hew_binary());
    command
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .env("TMPDIR", tmp_root.path())
        .env("TMP", tmp_root.path())
        .env("TEMP", tmp_root.path());
    let output = support::run_bounded_command(command, "hew run trivial (artifact-cleanup proof)");

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let hew_run_dir = tmp_root.path().join("hew-run");
    let leftover: Vec<_> = std::fs::read_dir(&hew_run_dir)
        .map(|entries| entries.flatten().map(|e| e.path()).collect())
        .unwrap_or_default();
    assert!(
        leftover.is_empty(),
        "a normal exit must leave no artifact dir behind under {}: {leftover:?}",
        hew_run_dir.display()
    );
}

#[test]
fn qualified_variant_tuple_payload_binds_nested_values() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("qualified_variant_tuple_payload.hew");
    std::fs::write(
        &path,
        r"
enum Pair { Both((i64, i64)), None }

fn main() {
    let pair = Pair.Both((19, 23));
    match pair {
        Pair.Both((a, b)) => println(a * 100 + b),
        Pair.None => println(0),
    }
}
",
    )
    .unwrap();

    let output = run_bounded_hew_run(&path, dir.path());
    assert!(
        output.status.success(),
        "qualified aggregate payload must compile and run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "1923\n");
}

#[test]
fn tcp_loopback_recv_roundtrip_returns_written_bytes() {
    require_codegen();

    let addr = unused_loopback_addr();
    let dir = support::tempdir();
    let path = dir.path().join("tcp_loopback_roundtrip.hew");
    std::fs::write(
        &path,
        format!(
            r#"
import std.net;
import std.encoding.utf8;

actor EchoServer {{
    receive fn connect_send_and_read(unused: i64) {{
        let conn = match net.connect("{addr}") {{ .Ok(value) => value, .Err(error) => panic("network operation failed"), }};
        conn.send("client-ping:r319".to_bytes()).expect("send");
        let reply = match conn.recv() {{ .Some(data) => utf8.decode(data).expect("client read is valid UTF-8"), .None => panic("client read hit end of data"), }};
        println(f"client-read={{reply}}");
        conn.close();
    }}
}}

// The client handler runs forked: a `receive fn` call from `main` completes the
// handler before it returns, so calling it inline would block `main` short of
// `accept()` while the handler blocks on its own read.
fn main() {{
    let listener = match net.listen("{addr}") {{ .Ok(value) => value, .Err(error) => panic("network operation failed"), }};
    let client = spawn EchoServer;
    scope {{
        let _client_turn = fork client.connect_send_and_read(0);

        let conn = listener.accept();
        let request = match conn.recv() {{ .Some(data) => utf8.decode(data).expect("server read is valid UTF-8"), .None => panic("server read hit end of data"), }};
        println(f"server-read={{request}}");
        conn.send("tcp-echo:hew-net-r319".to_bytes()).expect("send");
        conn.close();
    }}
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

/// CAP-12: a `NodeConfig` with a `key` and a pinned `peers` entry mints and
/// persists this node's TLS identity (the keyfile must exist after the run
/// and survive unchanged on a second run) and pins a peer SPKI in the
/// fail-closed allowlist through `Node::start`; the node then starts,
/// registers, and shuts down. Native quic-mesh; no parity on WASM.
#[test]
fn run_node_peer_auth_surface_persists_keys_and_runs() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("node_peer_auth.hew");
    std::fs::write(
        &path,
        r#"
        actor Counter {
            var count: i64,
            receive fn increment(n: i64) { count = count + n; }
        }

        fn main() {
            let config = NodeConfig {
                bind: "127.0.0.1:0",
                transport: "quic-mesh",
                key: "node.key",
                trust: "pinned",
                peers: ["3059301306072a8648ce3d020106082a8648ce3d030107"],
                seeds: [],
            };
            match Node.start(config) {
                .Ok(_) => {},
                .Err(_) => panic("node start failed"),
            }
            let me = Node.identity_key();
            let counter = spawn Counter(count: 0);
            Node.register("counter", counter);
            let _ = counter.increment(5);
            Node.shutdown();
            println(f"peer-auth ok id={me}");
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
    // `Node::identity_key()` must export this node's stable mesh credential
    // (the loaded cert SPKI as lowercase hex) *before* start froze the snapshot;
    // it is non-empty and pure lowercase hex.
    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let id_hex = stdout
        .trim()
        .strip_prefix("peer-auth ok id=")
        .expect("identity_key round-trip line");
    assert!(
        !id_hex.is_empty()
            && id_hex
                .chars()
                .all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase()),
        "identity_key must export non-empty lowercase hex; got {id_hex:?}"
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

/// F6 fail-closed: a bad-hex peer credential in `NodeConfig.peers` is rejected
/// and surfaced (`hew_last_error` + a `hew:` stderr diagnostic) while staging
/// the config; `Node::start` never reaches the low-level bind because the
/// staged config transaction short-circuits on the first failing field,
/// rather than silently coming up with an incomplete peer allowlist. The Hew
/// call form discards the returned `Result`, so the operator-visible signal
/// is the stderr diagnostic.
#[test]
fn run_node_allow_peer_bad_hex_is_surfaced_and_start_fails_closed() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("allow_peer_bad_hex.hew");
    std::fs::write(
        &path,
        r#"
        fn main() {
            let config = NodeConfig {
                bind: "127.0.0.1:0",
                transport: "quic-mesh",
                key: "",
                trust: "pinned",
                peers: ["zznothexzz"],
                seeds: [],
            };
            match Node.start(config) {
                .Ok(_) => println("started"),
                .Err(_) => println("refused"),
            }
        }
        "#,
    )
    .expect("write bad-hex fixture");

    let output = run_bounded_hew_run(&path, dir.path());
    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        stderr.contains("Node::allow_peer")
            && stderr.contains("hex-encoded SPKI")
            && stderr.contains("fail-closed"),
        "bad-hex allow_peer must be surfaced on stderr, fail-closed; stderr: {stderr}"
    );
    assert!(
        stdout.contains("refused") && !stdout.contains("started"),
        "Node::start must refuse (fail-closed), never silently succeed; stdout: {stdout}"
    );
}

/// F6 fail-closed: a corrupt keyfile named by `NodeConfig.key` makes
/// `Node::load_keys` fail and surface the error while staging the config;
/// `Node::start` never reaches the low-level bind, rather than silently
/// presenting an ephemeral self-signed identity (the operator's pinned
/// identity failed to load).
#[test]
fn run_node_load_keys_corrupt_keyfile_is_surfaced_and_start_fails_closed() {
    require_codegen();

    let dir = support::tempdir();
    // A keyfile that exists but is not a valid mesh keyfile frame.
    std::fs::write(dir.path().join("node.key"), b"not-a-keyfile-frame")
        .expect("write corrupt keyfile");
    let path = dir.path().join("load_keys_corrupt.hew");
    std::fs::write(
        &path,
        r#"
        fn main() {
            let config = NodeConfig {
                bind: "127.0.0.1:0",
                transport: "quic-mesh",
                key: "node.key",
                trust: "pinned",
                peers: [],
                seeds: [],
            };
            match Node.start(config) {
                .Ok(_) => println("started"),
                .Err(_) => println("refused"),
            }
        }
        "#,
    )
    .expect("write corrupt-keyfile fixture");

    let output = run_bounded_hew_run(&path, dir.path());
    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        stderr.contains("Node::load_keys") && stderr.contains("fail-closed"),
        "a corrupt keyfile must be surfaced on stderr, fail-closed; stderr: {stderr}"
    );
    assert!(
        stdout.contains("refused") && !stdout.contains("started"),
        "Node::start must refuse (fail-closed), never silently succeed; stdout: {stdout}"
    );
}

/// F6 fail-closed: when the `NodeConfig.key` path cannot establish an
/// identity (here, a parent directory that does not exist, so the fresh
/// identity cannot be persisted), `Node::load_keys` fails while staging the
/// config and `Node::start` never reaches the low-level bind — fail-closed
/// without any identity, never an ephemeral fallback.
#[test]
fn run_node_start_fails_closed_when_identity_cannot_be_established() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("load_keys_missing_dir.hew");
    std::fs::write(
        &path,
        r#"
        fn main() {
            let config = NodeConfig {
                bind: "127.0.0.1:0",
                transport: "quic-mesh",
                key: "no_such_dir/node.key",
                trust: "pinned",
                peers: [],
                seeds: [],
            };
            match Node.start(config) {
                .Ok(_) => println("started"),
                .Err(_) => println("refused"),
            }
        }
        "#,
    )
    .expect("write missing-keyfile fixture");

    let output = run_bounded_hew_run(&path, dir.path());
    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        stderr.contains("Node::load_keys") && stderr.contains("fail-closed"),
        "an unestablishable identity must be surfaced on stderr; stderr: {stderr}"
    );
    assert!(
        stdout.contains("refused") && !stdout.contains("started"),
        "Node::start must refuse (fail-closed), never silently succeed; stdout: {stdout}"
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
                .Some(x) => x,
                .None => 0,
            }
        }

        fn main() {
            var v: Vec<i64> = Vec.new();
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
            cur: i64,
            end: i64,
        }

        impl Iterator for Counter {
            type Item = i64;

            fn next(var self) -> Option<i64> {
                if self.cur >= self.end {
                    .None
                } else {
                    let out = self.cur;
                    self.cur = self.cur + 1;
                    .Some(out)
                }
            }
        }

        fn first_or_zero<I>(var it: I) -> i64
        where
            I: Iterator<Item = i64>,
        {
            match it.next() {
                .Some(x) => x,
                .None => 0,
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

/// A generic Vec iteration body is admitted before its `T` is known, then each
/// concrete resource-bearing instantiation must fail closed before a shallow
/// element copy can acquire a second owner. The diagnostic may come from the
/// generic `get` boundary or a later clone-totality boundary, but it must retain
/// the canonical no-semantic-clone/second-owner explanation.
#[test]
fn compile_generic_vec_for_in_resource_instantiation_fails_closed() {
    require_codegen();

    let dir = support::tempdir();
    let source = repo_root()
        .join("tests/vertical-slice/reject/for_in_generic_vec_resource_instantiation.hew");
    let output = Command::new(hew_binary())
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
        !output.status.success(),
        "a resource-valued VecIter monomorphisation must fail closed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    // `for item in items` binds the element as a borrow, so counting no longer
    // makes a shallow clone and there is nothing to refuse there. The
    // fail-closed property moved to the escape: a borrowed element whose type
    // has no copy operation cannot be transferred out of the loop. Lowering
    // stops at the first refusing instantiation, so the per-instantiation count
    // the old wording asserted is no longer observable. The refusal still
    // arrives wrapped in the semantic-lowering limitation channel rather than as
    // a source diagnostic; that is #3377.
    assert!(
        combined.contains("E_OWN_CONSUME_BORROWED")
            && combined.contains("`item` is borrowed here")
            && combined.contains("transfers only from an owning binding"),
        "expected a resource-bearing Vec element to fail closed when it escapes the \
         loop borrow: {combined}"
    );
}

#[test]
fn var_self_countdown_loop_writes_receiver_back() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("var_self_countdown.hew");
    std::fs::write(
        &source,
        r"
pub type Countdown { n: i64, }

impl Iterator for Countdown {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        if self.n <= 0 {
            .None
        } else {
            let cur = self.n;
            self.n = self.n - 1;
            .Some(cur)
        }
    }
}

fn main() {
    var cd = Countdown { n: 3 };
    var total = 0;
    loop {
        match cd.next() {
            .Some(v) => { total = total + v; },
            .None => { break; },
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
pub type Counter { n: i64, }

impl Iterator for Counter {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        self.n = self.n + 1;
        .Some(self.n)
    }
}

fn main() {
    var c = Counter { n: 0 };
    let _first = c.next();
    match c.next() {
        .Some(v2) => { println(v2); },
        .None => { println(-1); },
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
    var words: Vec<string> = Vec.new();
    words.push("first");
    words.push("second");
    var it = words.into_iter();
    let _first = it.next();
    match it.next() {
        .Some(v2) => { println(v2); },
        .None => { println("none"); },
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
pub type Counter { n: i64, }

impl Iterator for Counter {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        let before = { self.n };
        self.n = self.n + 1;
        .Some(before)
    }
}

fn main() {
    var c = Counter { n: 1 };
    let _first = c.next();
    match c.next() {
        .Some(v2) => { println(v2); },
        .None => { println(-1); },
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
pub type Slot<T> { x: T, n: i64, }

trait Tick {
    type Item;
    fn next(var self) -> Option<Self.Item>;
}

impl<T> Tick for Slot<T> {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        self.n = self.n + 1;
        .Some(self.n)
    }
}

fn main() {
    var s = Slot<i64> { x: 0, n: 0 };
    let _first = s.next();
    match s.next() {
        .Some(v2) => { println(v2); },
        .None => { println(-1); },
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
pub type Slot<T> { x: T, }

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
        .Some(x) => x,
        .None => 0,
    }
}

fn main() {
    var values: Vec<i64> = Vec.new();
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

/// Guard (#2359, recv leg): `Channel<Vec<indirect-enum>>` stays rejected
/// UPSTREAM by the channel element-layout witness — the existing check-time
/// diagnostic, not a new one. No recv surface can type this element class
/// today, so the recv-`Some` release seam is unreachable for it; this guard
/// pins the witness so a future weakening cannot silently open the recv
/// path into the Vec-element release seam.
#[test]
fn check_channel_vec_indirect_enum_rejected_by_layout_witness() {
    require_codegen();

    let source = repo_root().join("tests/vertical-slice/reject/channel_vec_indirect_enum.hew");
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
        combined.contains("cannot ride the element-layout queue witness"),
        "expected the upstream element-layout witness diagnostic; got: {combined}"
    );
}

/// A generator OWNS what its body reads from the enclosing frame: the capture
/// record is heap-copied into the coro env and the generator outlives the call
/// that built it. A captured value whose type has no copy operation therefore
/// has to be moved in, and a borrowed parameter has nothing to move. The
/// checker decides it, so the programmer reads a span and a remedy rather than
/// a SIR verification failure (#3377). A bare `fn(..)` carries no copy
/// operation, so both remedies are offered; a `#[resource]` gets only
/// `consume`. Accepted twins: `gen_fn_consume_borrowed_param` (both remedies
/// run) and `gen_fn_null_env_fn_values` (the `fn[clone]` spelling).
#[test]
fn check_gen_fn_borrowed_fn_param_fails_closed() {
    require_codegen();

    let combined = check_fails("tests/vertical-slice/reject/gen_fn_borrowed_fn_param.hew");
    assert!(
        combined.contains("E_OWN_CONSUME_BORROWED")
            && combined.contains("a generator owns its captures")
            && combined.contains("borrowed parameter `f` of type `fn(i64) -> i64`"),
        "expected the generator capture refusal to name the parameter and its type; got: {combined}"
    );
    assert!(
        combined.contains("gen_fn_borrowed_fn_param.hew:13:15"),
        "the refusal must carry a source span, not an internal-error line; got: {combined}"
    );
    assert!(
        combined.contains("consume f: fn(i64) -> i64")
            && combined.contains("f: fn[clone](i64) -> i64"),
        "expected both remedies for a callable capture; got: {combined}"
    );
}

/// The same rule through the `gen { }` block surface and a non-callable type:
/// a `#[resource]` parameter has no cloneable spelling, so `consume` is the
/// only remedy offered.
#[test]
fn check_gen_block_borrowed_resource_capture_fails_closed() {
    require_codegen();

    let combined =
        check_fails("tests/vertical-slice/reject/gen_block_borrowed_resource_capture.hew");
    assert!(
        combined.contains("E_OWN_CONSUME_BORROWED")
            && combined.contains("borrowed parameter `t` of type `Token`")
            && combined.contains("consume t: Token"),
        "expected the resource capture refusal with the consume remedy; got: {combined}"
    );
    assert!(
        !combined.contains("fn[clone]"),
        "a resource has no cloneable spelling to suggest; got: {combined}"
    );
}

// The conditional carrier-consumption paths run in the carrier-conditional-consume
// acceptance and safety case. The former checker refusal no longer applies.

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
        "import std.io;\n\
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
        "import std.io;\n\
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
        "import std.io;\n\
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
        "import std.io;\n\
         \n\
         type Point { x: i64, }\n\
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
        combined.contains("use of moved value `h`"),
        "expected use-after-move diagnostic; got: {combined}"
    );
    assert!(
        combined.contains("value was consumed here"),
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
        combined.contains("use of moved value `f`") && combined.contains("value was consumed here"),
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
        combined.contains("use of moved value `f`") && combined.contains("value was consumed here"),
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
        combined.contains("use of moved value `f`"),
        "expected use-after-move diagnostic; got: {combined}"
    );
    assert!(
        combined.contains("cannot invoke consumed callable `f`"),
        "expected the invocation itself to be refused; got: {combined}"
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
        combined.contains("E_OWN_CONSUME_BORROWED")
            && combined.contains("cannot consume borrowed collection element `first`")
            && combined.contains("the collection retains this value's owner"),
        "expected the borrowed-element store refusal naming the loan's owner; got: {combined}"
    );
}

/// Storing a `consume` fn-typed parameter into a record field is the
/// parameter's single consumption (the record becomes the sole env owner);
/// using the parameter AFTER that store is a use-after-consume, rejected by
/// the checker — never a silent miscompile or a runtime double-free. The
/// declared store is legal, so the error names the later use and points back
/// at the store, not a borrowed-store refusal.
#[test]
fn check_closure_param_use_after_store_fails_closed() {
    require_codegen();
    let combined = check_fails("tests/vertical-slice/reject/closure_param_use_after_store.hew");
    assert!(
        combined.contains("use of moved value `f`"),
        "expected use-after-consume diagnostic; got: {combined}"
    );
    assert!(
        combined.contains("value was consumed here"),
        "expected the note pointing at the consuming store; got: {combined}"
    );
    assert!(
        !combined.contains("ClosurePairBorrowedStore"),
        "the declared store is legal; the error must be the later use: {combined}"
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
            stdout: string,
            code: i64,
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
            counts: Vec<i64>,
            total: i64,
        }

        fn build() -> Histogram {
            var v: Vec<i64> = Vec.new();
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
            name: string,
        }

        type Boxed {
            user: User,
            tag: i64,
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
/// method (`get_int`) and the implicit close, asserting the runtime round-trip.
#[test]
fn run_imports_json_opaque_handle_round_trips() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("json_opaque.hew");
    std::fs::write(
        &hew_src,
        "import std.encoding.json;\n\
         \n\
         fn main() -> i32 {\n\
         \x20   let v = json.from_int(42);\n\
         \x20   let n = match v.get_int() { .Ok(value) => value, .Err(_) => return 1, };\n\
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

/// `import std.encoding.json` then building an object and an array through the
/// mutating `set` / `push` methods. Guards the regression where non-trivial
/// imported impl methods (a void C call plus a status result) were absent from
/// `fn_registry` and the lowered item list because imported impl-method
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
        "import std.encoding.json;\n\
         \n\
         fn main() -> i32 {\n\
         \x20   var obj = json.object();\n\
         \x20   match obj.set(\"name\", json.from_string(\"Hew\")) { .Ok(_) => {}, .Err(_) => return 1, }\n\
         \x20   match obj.set(\"version\", json.from_int(1)) { .Ok(_) => {}, .Err(_) => return 1, }\n\
         \x20   let s = match obj.stringify() { .Ok(text) => text, .Err(_) => return 1, };\n\
         \x20   println(s);\n\
         \x20   let parsed = match json.parse(s) { .Ok(value) => value, .Err(_) => return 1, };\n\
         \x20   let field = match parsed.get_field(\"version\") {\n\
         \x20       .Ok(.Some(value)) => value,\n\
         \x20       .Ok(.None) => return 1,\n\
         \x20       .Err(_) => return 1,\n\
         \x20   };\n\
         \x20   let version = match field.get_int() { .Ok(value) => value, .Err(_) => return 1, };\n\
         \x20   println(f\"version={version}\");\n\
         \x20   var arr = json.array();\n\
         \x20   match arr.push(json.from_int(1)) { .Ok(_) => {}, .Err(_) => return 1, }\n\
         \x20   match arr.push(json.from_int(2)) { .Ok(_) => {}, .Err(_) => return 1, }\n\
         \x20   match arr.push(json.from_int(3)) { .Ok(_) => {}, .Err(_) => return 1, }\n\
         \x20   let len = match arr.array_len() { .Ok(value) => value, .Err(_) => return 1, };\n\
         \x20   println(f\"len={len}\");\n\
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
        "import std.stream.{ Sink, Stream };\n\
         \n\
         fn make_pair() -> (Sink<string>, Stream<string>) {\n\
         \x20   match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let (sink, input) = make_pair();\n\
         \x20   sink.send(\"alpha\").expect(\"send\");\n\
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
        "import std.stream.{ Sink, Stream };\n\
         \n\
         fn main() {\n\
         \x20   let pair: (Sink<string>, Stream<string>) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
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
        "import std.stream.{ Sink, Stream };\n\
         fn make_pair() -> (Sink<string>, Stream<string>) {\n\
         \x20   let (s, r) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
         \x20   let pair = (s, r);\n\
         \x20   pair\n\
         }\n\
         fn main() {\n\
         \x20   let (sink, input) = make_pair();\n\
         \x20   sink.send(\"alpha\").expect(\"send\");\n\
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
        "import std.stream.{ Sink, Stream };\n\
         fn make_pair(c: bool) -> (Sink<string>, Stream<string>) {\n\
         \x20   let (s, r) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
         \x20   if c { (s, r) } else { (s, r) }\n\
         }\n\
         fn main() {\n\
         \x20   let (sink, input) = make_pair(true);\n\
         \x20   sink.send(\"alpha\").expect(\"send\");\n\
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
        "import std.stream.{ Sink, Stream };\n\
         type Pipe { sink: Sink<string>, input: Stream<string> }\n\
         fn make_pipe() -> Pipe {\n\
         \x20   let (s, r) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
         \x20   let p = Pipe { sink: s, input: r };\n\
         \x20   p\n\
         }\n\
         fn main() {\n\
         \x20   let p = make_pipe();\n\
         \x20   p.sink.send(\"alpha\").expect(\"send\");\n\
         \x20   p.sink.close();\n\
         \x20   p.input.close();\n\
         \x20   println(\"record-ok\");\n\
         }\n",
    )
    .unwrap();
    let output = run_bounded_hew_run(&hew_src, repo_root());
    assert!(
        output.status.success(),
        "record of handles return + explicit close must run cleanly; status: {:?}\nstdout: {}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "record-ok\n");
}

/// Negative control for `run_record_of_handles_return_drops_each_field_once`:
/// the SAME record with NO explicit close must still exit cleanly. The fix for
/// the double close nulls the record's field slot when a consuming call takes
/// the handle; a fix that instead retired the whole record drop would trade the
/// double close for two leaked stream handles, and this program would keep
/// passing. It pins that the composite is still the owner when nothing takes
/// the fields off it.
#[test]
fn run_record_of_handles_return_without_explicit_close_exits_clean() {
    require_codegen();
    let dir = support::tempdir();
    let hew_src = dir.path().join("record_handle_return_noclose.hew");
    std::fs::write(
        &hew_src,
        "import std.stream.{ Sink, Stream };\n\
         type Pipe { sink: Sink<string>, input: Stream<string> }\n\
         fn make_pipe() -> Pipe {\n\
         \x20   let (s, r) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
         \x20   let p = Pipe { sink: s, input: r };\n\
         \x20   p\n\
         }\n\
         fn main() {\n\
         \x20   let p = make_pipe();\n\
         \x20   p.sink.send(\"alpha\").expect(\"send\");\n\
         \x20   println(\"record-noclose-ok\");\n\
         }\n",
    )
    .unwrap();
    let output = run_bounded_hew_run(&hew_src, repo_root());
    assert!(
        output.status.success(),
        "record of handles with no explicit close must run cleanly; status: {:?}\nstdout: {}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "record-noclose-ok\n"
    );
}

/// The TUPLE spelling of the same field projection: `p.0.close()` on a
/// let-bound tuple of handles. #3113 read the tuple return path as correct
/// because the DESTRUCTURED form (`let (sink, input) = ...`) neutralizes its
/// source slots in the pattern; projecting the field straight into the close
/// took the borrow path and left the tuple's `TupleInPlace` walk closing the
/// same handles again.
///
/// KNOWN FAILURE (#3070): on the current lowerer this prints
/// `tuple-field-ok` and then exits non-zero (a double close during scope-exit
/// teardown). See `scripts/nextest-expected-failures.tsv`.
#[test]
fn run_bound_tuple_field_close_drops_each_handle_once() {
    require_codegen();
    let dir = support::tempdir();
    let hew_src = dir.path().join("tuple_field_close.hew");
    std::fs::write(
        &hew_src,
        "import std.stream.{ Sink, Stream };\n\
         fn make_pipe() -> (Sink<string>, Stream<string>) {\n\
         \x20   let (s, r) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
         \x20   (s, r)\n\
         }\n\
         fn main() {\n\
         \x20   let p = make_pipe();\n\
         \x20   p.0.send(\"alpha\").expect(\"send\");\n\
         \x20   p.0.close();\n\
         \x20   p.1.close();\n\
         \x20   println(\"tuple-field-ok\");\n\
         }\n",
    )
    .unwrap();
    let output = run_bounded_hew_run(&hew_src, repo_root());
    assert!(
        output.status.success(),
        "tuple field close must not double-close; status: {:?}\nstdout: {}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "tuple-field-ok\n");
}

// ── `#[resource]`-record FIELD of a plain record (#3070) ────────────────
//
// A `#[resource]` record leaf has no null representation, so clearing the
// composite's slot cannot make its `RecordInPlace` field walk skip it: the walk
// would run the user `close(self)` over zeroed storage. The composite stops
// owning the leaf instead, which retires the WHOLE root — the boundary these
// three pin from all sides.

/// Shared prelude: a `#[resource]` record over a real `malloc` block, so a
/// second close is a genuine double free rather than only an extra line.
const RESOURCE_FIELD_PRELUDE: &str = "\
extern \"C\" {\n\
    fn malloc(size: i64) -> i64;\n\
    fn free(addr: i64);\n\
}\n\
#[resource]\n\
type Slot { addr: i64 }\n\
impl Slot {\n\
    fn close(consume self) { println(\"close\"); unsafe { free(self.addr) }; }\n\
}\n\
fn acquire() -> Slot { Slot { addr: unsafe { malloc(64) } } }\n\
type Two { a: Slot, b: Slot }\n";

fn run_resource_field_program(name: &str, body: &str) -> std::process::Output {
    let dir = support::tempdir();
    let hew_src = dir.path().join(name);
    std::fs::write(&hew_src, format!("{RESOURCE_FIELD_PRELUDE}{body}")).unwrap();
    run_bounded_hew_run(&hew_src, repo_root())
}

/// Both fields closed by the program. Each close must retire the root at most
/// once: a second transfer of an already-ended generation is reported as an
/// ownership-generation ICE, so this also pins the compile side.
///
/// KNOWN FAILURE (#3070): on the current lowerer this double-frees. See
/// `scripts/nextest-expected-failures.tsv`.
#[test]
fn run_record_resource_fields_both_closed_close_once_each() {
    require_codegen();
    let output = run_resource_field_program(
        "resource_field_both.hew",
        "fn main() {\n\
         \x20   let t = Two { a: acquire(), b: acquire() };\n\
         \x20   t.a.close();\n\
         \x20   t.b.close();\n\
         \x20   println(\"done\");\n\
         }\n",
    );
    assert!(
        output.status.success(),
        "closing both resource fields must run cleanly; status: {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "close\nclose\ndone\n"
    );
}

/// Neither field closed: the composite's own field walk is the sole closer, so
/// both leaves still close. The leak control for the retirement.
#[test]
fn run_record_resource_fields_untouched_close_once_each() {
    require_codegen();
    let output = run_resource_field_program(
        "resource_field_untouched.hew",
        "fn main() {\n\
         \x20   let t = Two { a: acquire(), b: acquire() };\n\
         \x20   let _ = t;\n\
         \x20   println(\"done\");\n\
         }\n",
    );
    assert!(
        output.status.success(),
        "an untouched record of resource fields must run cleanly; status: {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "done\nclose\nclose\n"
    );
}

/// Per-field retirement: closing ONE of two resource fields retires only that
/// field, so the untouched sibling still closes on scope exit. The earlier
/// whole-root retirement leaked `b`; before that the program closed `a` twice.
/// Exactly two `close` lines and no abort is the pin on both sides.
#[test]
fn run_record_resource_field_partial_close_leaks_the_sibling() {
    require_codegen();
    let output = run_resource_field_program(
        "resource_field_partial.hew",
        "fn main() {\n\
         \x20   let t = Two { a: acquire(), b: acquire() };\n\
         \x20   t.a.close();\n\
         \x20   println(\"done\");\n\
         }\n",
    );
    assert!(
        output.status.success(),
        "closing one of two resource fields must not double free; status: {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "close\ndone\nclose\n",
        "exactly two closes: `a` through the program, `b` on scope exit"
    );
}

// ── `#[resource]`-record payload of an enum carrier (#3070) ──────────────
//
// A `#[resource]` RECORD payload has no null representation, so the shell's
// tag-aware `EnumInPlace` walk cannot skip a slot the match binder already took
// — it calls the user `close(self)` on zeroed storage. Each of these prints the
// payload's own field from inside `close`, so a second close is visible as a
// second line carrying a zeroed value rather than only as a sanitizer finding.

/// Shared prelude: a `#[resource]` record whose `close` names the value it is
/// closing, and a `Result`-returning producer.
const RESOURCE_PAYLOAD_PRELUDE: &str = "\
#[resource]\n\
type Handle { id: i64 }\n\
impl Handle {\n\
    fn close(consume self) { println(f\"close {self.id}\"); }\n\
}\n\
fn acquire(ok: bool) -> Result<Handle, string> {\n\
    if ok { .Ok(Handle { id: 7 }) } else { .Err(\"declined\") }\n\
}\n";

fn run_resource_payload_program(name: &str, body: &str) -> std::process::Output {
    let dir = support::tempdir();
    let hew_src = dir.path().join(name);
    std::fs::write(&hew_src, format!("{RESOURCE_PAYLOAD_PRELUDE}{body}")).unwrap();
    run_bounded_hew_run(&hew_src, repo_root())
}

/// The reported reduction: an `.Ok(h)` arm that reads a bit-copy field off the
/// binder. The binder owns the close; the shell must not close the payload it
/// handed over. Pre-fix this printed `close 7` and then `close 0` — the shell's
/// walk running the user close over the zeroed variant slot.
///
/// KNOWN FAILURE (#3070): on the current lowerer this prints an extra
/// `close 0` line. See `scripts/nextest-expected-failures.tsv`.
#[test]
fn run_result_resource_payload_field_read_closes_once() {
    require_codegen();
    let output = run_resource_payload_program(
        "resource_payload_read.hew",
        "fn main() {\n\
         \x20   match acquire(true) {\n\
         \x20       .Ok(h) => { println(f\"id={h.id}\"); },\n\
         \x20       .Err(e) => { println(e); },\n\
         \x20   }\n\
         \x20   println(\"done\");\n\
         }\n",
    );
    assert!(
        output.status.success(),
        "field-read arm must run cleanly; status: {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "id=7\nclose 7\ndone\n"
    );
}

/// The same arm with the binder untouched. Reading the field is not what
/// discharges the shell, so the untouched binder must close exactly once too.
///
/// KNOWN FAILURE (#3070): on the current lowerer this prints an extra
/// `close 0` line. See `scripts/nextest-expected-failures.tsv`.
#[test]
fn run_result_resource_payload_untouched_binder_closes_once() {
    require_codegen();
    let output = run_resource_payload_program(
        "resource_payload_untouched.hew",
        "fn main() {\n\
         \x20   match acquire(true) {\n\
         \x20       .Ok(h) => { let _ = h; },\n\
         \x20       .Err(e) => { println(e); },\n\
         \x20   }\n\
         \x20   println(\"done\");\n\
         }\n",
    );
    assert!(
        output.status.success(),
        "untouched binder must run cleanly; status: {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "close 7\ndone\n");
}

/// The explicit-close arm, which was already correct: it must stay at one close
/// after the shell's generation moved to the hand-off. This is the control that
/// catches a fix which ends the shell's generation twice.
#[test]
fn run_result_resource_payload_explicit_close_stays_once() {
    require_codegen();
    let output = run_resource_payload_program(
        "resource_payload_explicit.hew",
        "fn main() {\n\
         \x20   match acquire(true) {\n\
         \x20       .Ok(h) => { h.close(); },\n\
         \x20       .Err(e) => { println(e); },\n\
         \x20   }\n\
         \x20   println(\"done\");\n\
         }\n",
    );
    assert!(
        output.status.success(),
        "explicit close arm must run cleanly; status: {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "close 7\ndone\n");
}

/// Leak control: an arm that binds NOTHING leaves the payload with the shell,
/// whose in-place walk is the only close. A fix that retired the shell's
/// generation unconditionally would print nothing here.
///
/// KNOWN FAILURE (#3070): on the current lowerer this prints an extra
/// `close 0` line. See `scripts/nextest-expected-failures.tsv`.
#[test]
fn run_result_resource_payload_wildcard_arm_closes_once() {
    require_codegen();
    let output = run_resource_payload_program(
        "resource_payload_wildcard.hew",
        "fn main() {\n\
         \x20   match acquire(true) {\n\
         \x20       .Ok(_) => { println(\"ok\"); },\n\
         \x20       .Err(e) => { println(e); },\n\
         \x20   }\n\
         \x20   println(\"done\");\n\
         }\n",
    );
    assert!(
        output.status.success(),
        "wildcard arm must run cleanly; status: {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "ok\nclose 7\ndone\n"
    );
}

/// The declining producer allocates no payload, so no close may run at all —
/// the counterfactual that keeps the positive assertions from passing on a
/// compiler that closes something unconditionally.
#[test]
fn run_result_resource_payload_error_arm_closes_nothing() {
    require_codegen();
    let output = run_resource_payload_program(
        "resource_payload_error.hew",
        "fn main() {\n\
         \x20   match acquire(false) {\n\
         \x20       .Ok(h) => { let _ = h; },\n\
         \x20       .Err(e) => { println(e); },\n\
         \x20   }\n\
         \x20   println(\"done\");\n\
         }\n",
    );
    assert!(
        output.status.success(),
        "error arm must run cleanly; status: {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "declined\ndone\n");
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
        "actor ProbeSink { let id: i64, receive fn take(s: string) -> i64 { s.len() } }\n\
         fn main() {\n\
         \x20   let s: string = \"hello\";\n\
         \x20   let dup = clone s;\n\
         \x20   let sink = spawn ProbeSink(id: 0);\n\
         \x20   let n = sink.take(dup);\n\
         \x20   match n { .Ok(len) => println(f\"len={len}\"), .Err(_) => println(\"ask failed\") }\n\
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
         \x20   var xs: Vec<i64> = Vec.new();\n\
         \x20   xs.push(1); xs.push(2);\n\
         \x20   var dup = clone xs;\n\
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
         \x20   var xs: Vec<i64> = Vec.new();\n\
         \x20   xs.push(1); xs.push(2);\n\
         \x20   var b = xs.clone();\n\
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
         \x20   var n: i64 = 0,\n\
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
         \x20   match c.bump() {\n\
         \x20       .Ok(v) => println(f\"bumped: {v}\"),\n\
         \x20       .Err(_) => println(\"err\"),\n\
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

/// A selectively-imported pub const must bind bare at root exactly like a
/// selectively-imported pub fn from the same module. The HIR pre-pass
/// registers imported consts only under the qualified `{module}.{CONST}` key;
/// the importer-visible binding aliases to the same entry. Before the alias,
/// `import m::{MAX_RETRIES};` + bare `MAX_RETRIES` failed with
/// `E_HIR: identifier has no binding in resolved HIR` while the sibling fn
/// import and the dotted spelling both worked.
#[test]
fn run_selectively_imported_const_binds_bare_like_fn() {
    require_codegen();

    let dir = support::tempdir();
    std::fs::create_dir_all(dir.path().join("src/reasons")).unwrap();
    std::fs::write(
        dir.path().join("src/reasons/reasons.hew"),
        "pub const MAX_RETRIES: i64 = 5;\n\
         pub fn retries_label() -> string {\n\
         \x20   \"retries\"\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import src.reasons.{MAX_RETRIES, retries_label};\n\
         fn main() {\n\
         \x20   println(retries_label());\n\
         \x20   println(f\"max: {MAX_RETRIES}\");\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "selectively-imported const and fn should both bind bare; \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "retries\nmax: 5\n");
}

/// A record reaching root through a string-path import (`import "file.hew";`)
/// is spelled bare at the surface but keyed by its defining file's module
/// identity in the MIR layout registries. The bare parameter annotation was
/// the failing site: construction and inferred locals resolved, while
/// `fn f(w: WorkflowState)` froze a bare binding type that missed the
/// qualified field-order/value-class entries (doubled
/// `E_NOT_YET_IMPLEMENTED … field access on unregistered record type` +
/// `E_MIR: unknown type`). Root annotations now project through the
/// flat-file bare→qualified alias table, so field access through an
/// annotated parameter, construction, and a defining-module factory all
/// agree on one identity.
#[test]
fn run_string_path_imported_record_annotated_param_field_access() {
    require_codegen();

    let dir = support::tempdir();
    std::fs::create_dir_all(dir.path().join("src/workflow")).unwrap();
    std::fs::write(
        dir.path().join("src/workflow/machine.hew"),
        "pub type WorkflowState {\n\
         \x20   name: string,\n\
         \x20   count: i64,\n\
         }\n\
         pub fn make_state(name: string, count: i64) -> WorkflowState {\n\
         \x20   WorkflowState { name: name, count: count }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import \"src/workflow/machine.hew\";\n\
         fn read_name(w: WorkflowState) -> string {\n\
         \x20   w.name\n\
         }\n\
         fn main() {\n\
         \x20   let annotated = WorkflowState { name: \"annotated\", count: 3 };\n\
         \x20   println(read_name(annotated));\n\
         \x20   let made = make_state(\"made\", 7);\n\
         \x20   println(made.name);\n\
         \x20   println(f\"count: {made.count}\");\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "string-path-imported record through an annotated parameter should run; \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "annotated\nmade\ncount: 7\n"
    );
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
         \x20   var total: i64 = 0,\n\
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
         \x20   match s.add_doubled(4) {\n\
         \x20       .Ok(v) => println(f\"sum: {v}\"),\n\
         \x20       .Err(_) => println(\"err\"),\n\
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
         \x20   var balance: i64 = 0,\n\
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
        "import hew.bank;\n\
         \n\
         fn report(label: string, value: i64) {\n\
         \x20   println(f\"{label}={value}\");\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let acct = spawn bank.Account(opening: 100);\n\
         \x20   match acct.peek() { .Ok(v) => report(\"after_open\", v), .Err(_) => println(\"after_open=ERR\"), }\n\
         \x20   match acct.deposit(50) { .Ok(v) => report(\"after_deposit\", v), .Err(_) => println(\"after_deposit=ERR\"), }\n\
         \x20   match acct.withdraw(1000) { .Ok(v) => report(\"after_overdraw\", v), .Err(_) => println(\"after_overdraw=ERR\"), }\n\
         \x20   match acct.withdraw(30) { .Ok(v) => report(\"after_withdraw\", v), .Err(_) => println(\"after_withdraw=ERR\"), }\n\
         \x20   match acct.peek() { .Ok(v) => report(\"final\", v), .Err(_) => println(\"final=ERR\"), }\n\
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

/// Regression for named-import actor identity in actor-state annotations: inside
/// an imported actor body, `let inner: Inner;` is an actor reference and must
/// lower to `conn.Inner`, just like the local-module shorthand. Before
/// the fix, HIR left it as bare `Inner`, so MIR's state-clone classifier tried
/// to resolve it as a nested user record and failed with
/// `ActorStateCloneClassificationFailed` before codegen.
#[test]
fn run_imported_actor_state_bare_actor_field_canonicalizes_to_the_actor_type() {
    require_codegen();

    let dir = support::tempdir();
    let pkg_dir = dir.path().join("hew").join("conn");
    std::fs::create_dir_all(&pkg_dir).unwrap();
    std::fs::write(
        pkg_dir.join("conn.hew"),
        "pub actor Inner {\n\
         \x20   receive fn ping() -> i64 { 41 }\n\
         }\n\
         \n\
         pub actor Outer {\n\
         \x20   let inner: Inner,\n\
         \x20   receive fn go() -> i64 {\n\
         \x20       match inner.ping() {\n\
         \x20           .Ok(v) => v + 1,\n\
         \x20           .Err(_) => -1,\n\
         \x20       }\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew.conn.{Inner, Outer};\n\
         fn main() {\n\
         \x20   let i = spawn Inner();\n\
         \x20   let o = spawn Outer(inner: i);\n\
         \x20   match o.go() {\n\
         \x20       .Ok(v) => println(f\"v={v}\"),\n\
         \x20       .Err(_) => println(\"err\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "named-import actor field should canonicalize to the actor's own handle type; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "v=42\n");
}

/// Regression for the local-shadow scoping defect: a LOCAL non-actor type
/// whose bare name collides with an UNRELATED imported actor's short name
/// must keep its local identity. `canonicalize_actor_ref_field_ty` used to
/// canonicalize any bare field name that uniquely matched an actor's short
/// name ANYWHERE in the program (a global sweep over `actor_type_names`),
/// which silently hijacked this local record field into `m.Inner`
/// and rejected the program with `E_NOT_YET_IMPLEMENTED: field access on
/// unregistered record type ActorHandle$$Inner`. The fix scopes bare-name
/// resolution to `{decl_module}.{name}` (the actor decl's own module), so a
/// root-declared `Holder` referencing a root-declared `type Inner` never
/// consults `m`'s actor at all — local definitions win, per LESSONS
/// `per-module-type-identity`.
#[test]
fn run_local_record_shadows_imported_actor_short_name() {
    require_codegen();

    let dir = support::tempdir();
    let pkg_dir = dir.path().join("hew").join("m");
    std::fs::create_dir_all(&pkg_dir).unwrap();
    std::fs::write(
        pkg_dir.join("m.hew"),
        "pub actor Inner {\n\
         \x20   receive fn ping() -> i64 { 99 }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew.m;\n\
         \n\
         type Inner { x: i64 }\n\
         \n\
         actor Holder {\n\
         \x20   let inner: Inner,\n\
         \x20   receive fn get() -> i64 { inner.x }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let h = spawn Holder(inner: Inner { x: 7 });\n\
         \x20   match h.get() {\n\
         \x20       .Ok(v) => println(f\"v={v}\"),\n\
         \x20       .Err(_) => println(\"err\"),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "a local record shadowing an unrelated imported actor's short name \
         must compile and run as the local record; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "v=7\n");
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
         \x20   var n: i64 = 0,\n\
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
        "import hew.secret;\n\
         fn main() {\n\
         \x20   let c = spawn secret.Hidden();\n\
         \x20   match c.bump() {\n\
         \x20       .Ok(v) => println(f\"bumped: {v}\"),\n\
         \x20       .Err(_) => println(\"err\"),\n\
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
/// types `spawn bank.Account()` as `bank.Account`, MIR layouts key
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
                 \x20   var n: i64 = 0,\n\
                 \x20   receive fn who() -> i64 {{ {tag} }}\n\
                 }}\n"
            ),
        )
        .unwrap();
    }
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew.bank;\n\
         import hew.store;\n\
         fn main() {\n\
         \x20   let a = spawn bank.Account();\n\
         \x20   let s = spawn store.Account();\n\
         \x20   match a.who() {\n\
         \x20       .Ok(v) => println(f\"a={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   match s.who() {\n\
         \x20       .Ok(v) => println(f\"s={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
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
         \x20   var n: i64 = 0,\n\
         \x20   receive fn who() -> i64 { 999 }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew.bank;\n\
         actor Account {\n\
         \x20   var n: i64 = 0,\n\
         \x20   receive fn who() -> i64 { 111 }\n\
         }\n\
         fn main() {\n\
         \x20   let a = spawn bank.Account();\n\
         \x20   let l = spawn Account();\n\
         \x20   match a.who() {\n\
         \x20       .Ok(v) => println(f\"a={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   match l.who() {\n\
         \x20       .Ok(v) => println(f\"l={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
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
                 \x20   var n: i64 = 0,\n\
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
        "import hew.bank;\n\
         import hew.store;\n\
         supervisor Pair {\n\
         \x20   strategy: one_for_one,\n\
         \x20   intensity: 5 within 60s,\n\
         \n\
         \x20   child b: bank.Account,\n\
         \x20   child s: store.Account,\n\
         }\n\
         fn main() {\n\
         \x20   let p = spawn Pair;\n\
         \x20   sleep(50ms);\n\
         \x20   let b = p.b;\n\
         \x20   let s = p.s;\n\
         \x20   match b.who() {\n\
         \x20       .Ok(v) => println(f\"b={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   match s.who() {\n\
         \x20       .Ok(v) => println(f\"s={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   let _ = b.boom();\n\
         \x20   sleep(200ms);\n\
         \x20   let b2 = p.b;\n\
         \x20   match b2.who() {\n\
         \x20       .Ok(v) => println(f\"b2={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
         \x20   }\n\
         \x20   let s2 = p.s;\n\
         \x20   match s2.who() {\n\
         \x20       .Ok(v) => println(f\"s2={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
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

/// A fungible child binding survives a deterministically observed restart and
/// feeds two sequential batch forks through the replacement incarnation.
/// Distinct recursive payload tags make branch order and indirect-enum
/// ownership part of the runtime oracle rather than merely proving that
/// submission did not trap.
#[test]
fn run_fungible_child_binding_joins_after_observed_restart() {
    require_codegen();

    let dir = support::tempdir();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        r#"
indirect enum Tree {
    Leaf(i64),
    Node(Tree, Tree),
}

fn tree_sum(tree: Tree) -> i64 {
    match tree {
        .Leaf(value) => value,
        .Node(left, right) => tree_sum(left) + tree_sum(right),
    }
}

actor Worker {
    receive fn score(tag: i64, tree: Tree) -> i64 { tag + tree_sum(tree) }
    receive fn boom() { panic("restart"); }
}

supervisor App {
    strategy: one_for_one,
    intensity: 3 within 60s,
    child worker: Worker,
}

fn main() -> i64 {
    let sup = spawn App;
    let worker = sup.worker;
    // `boom` is a completion call, so its `Err` proves the crash opened its
    // fault record. `await_restart` then waits for that record to settle,
    // which is the observable restart this test joins on.
    let _ = worker.boom();
    let _ = await_restart sup.worker;
    let (a, b) = await fork (
        worker.score(11, .Node(.Leaf(1), .Leaf(2))),
        worker.score(22, .Node(.Leaf(3), .Leaf(4))),
    );
    let (c, d) = await fork (
        worker.score(33, .Node(.Leaf(5), .Leaf(6))),
        worker.score(44, .Node(.Leaf(7), .Leaf(8))),
    );
    let ra = match a { .Ok(v) => v, .Err(_) => -1, };
    let rb = match b { .Ok(v) => v, .Err(_) => -1, };
    let rc = match c { .Ok(v) => v, .Err(_) => -1, };
    let rd = match d { .Ok(v) => v, .Err(_) => -1, };
    print(f"{ra},{rb},{rc},{rd}");
    supervisor_stop(sup);
    0
}
"#,
    )
    .unwrap();

    let output = run_bounded_hew_run(&main, dir.path());
    assert!(
        output.status.success(),
        "two sequential batch forks through the pre-crash role binding must use the replacement child; \
         stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "14,29,44,59",
        "both forks must preserve exact branch order and recursive payload values"
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
         \x20   var n: i64 = 0,\n\
         \x20   receive fn id() -> i64 { 999 }\n\
         }\n",
    )
    .unwrap();
    let main = dir.path().join("main.hew");
    std::fs::write(
        &main,
        "import hew.secret;\n\
         actor Account {\n\
         \x20   var n: i64 = 0,\n\
         \x20   receive fn id() -> i64 { 111 }\n\
         }\n\
         fn main() {\n\
         \x20   let a = spawn secret.Account();\n\
         \x20   match a.id() {\n\
         \x20       .Ok(v) => println(f\"a={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
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
        combined.contains("has no exported actor or supervisor `Account`") && combined.contains("secret"),
        "expected a fail-closed diagnostic that `secret` has no exported actor or supervisor `Account`; \
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
        "import hew.secret;\n\
         actor Account {\n\
         \x20   var n: i64 = 0,\n\
         \x20   receive fn id() -> i64 { 111 }\n\
         }\n\
         fn main() {\n\
         \x20   let a = spawn secret.Account();\n\
         \x20   match a.id() {\n\
         \x20       .Ok(v) => println(f\"a={v}\"),\n\
         \x20       .Err(_) => println(\"e\"),\n\
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
        combined.contains("has no exported actor or supervisor `Account`") && combined.contains("secret"),
        "expected a fail-closed diagnostic that `secret` has no exported actor or supervisor `Account`; \
         got: {combined}"
    );
}

#[test]
fn suspended_actor_fresh_state_handoff_closes_each_child_once() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("suspended_actor_state_handoff.hew");
    std::fs::write(
        &source,
        "#[resource]\n\
         #[opaque]\n\
         type Marker {}\n\
         impl Marker {\n\
         \x20   fn close(consume self) { unsafe { hew_deque_free(self) }; println(\"closed\"); }\n\
         }\n\
         extern \"C\" {\n\
         \x20   fn hew_deque_new() -> Marker;\n\
         \x20   fn hew_deque_free(consume marker: Marker);\n\
         }\n\
         actor Child {\n\
         \x20   let label: string,\n\
         \x20   let marker: Marker,\n\
         \x20   receive fn ping() {}\n\
         }\n\
         actor Maker {\n\
         \x20   receive fn go() {\n\
         \x20       var i: i64 = 0;\n\
         \x20       while i < 3 {\n\
         \x20           sleep(1ms);\n\
         \x20           let label = f\"child-{i}\";\n\
         \x20           let child = spawn Child(\n\
         \x20               label: label.clone(),\n\
         \x20               marker: unsafe { hew_deque_new() },\n\
         \x20           );\n\
         \x20           child.stop();\n\
         \x20           i = i + 1;\n\
         \x20       }\n\
         \x20       println(\"maker-done\");\n\
         \x20   }\n\
         }\n\
         fn main() {\n\
         \x20   let maker = spawn Maker;\n\
         \x20   let _ = maker.go();\n\
         \x20   sleep(200ms);\n\
         }\n",
    )
    .expect("write suspended actor state handoff fixture");

    let output = run_bounded_hew_run(&source, repo_root());
    assert!(
        output.status.success(),
        "suspended actor state handoff must run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout.lines().filter(|line| *line == "closed").count(),
        3,
        "every transferred child state must close exactly once; stdout: {stdout}"
    );
    assert_eq!(
        stdout.lines().filter(|line| *line == "maker-done").count(),
        1,
        "the loop must resume through every suspension; stdout: {stdout}"
    );
}
