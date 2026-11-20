//! Exercise readable and writable descriptor watches on pipes shared with
//! the test process. Filling, draining and closing the pipes drives readiness.
#![cfg(unix)]

mod support;

use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const LINE_TIMEOUT: Duration = Duration::from_secs(12);

fn compile(source: &str, dir: &Path, opt_level: u8) -> PathBuf {
    let path = dir.join("descriptor_readiness.hew");
    std::fs::write(&path, source).expect("write fixture");
    let output = Command::new(hew_binary())
        .args(["compile", "--emit-dir"])
        .arg(dir)
        .arg(&path)
        .arg("--opt-level")
        .arg(opt_level.to_string())
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        output.status.success(),
        "compile failed\n{}",
        describe_output(&output)
    );
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .map(PathBuf::from)
        .expect("compiler reported a native artifact")
}

/// Run `binary` with a piped standard input: once a stdout line starts with
/// each `(prefix, input)` step's prefix, write that input; then close standard
/// input and return every stdout line.
fn run_scripted(binary: &Path, steps: &[(&str, &[u8])], opt_level: u8) -> Vec<String> {
    run_script(
        Command::new(binary)
            .stdin(Stdio::piped())
            .stderr(Stdio::piped()),
        steps.iter().map(|(prefix, _)| *prefix),
        opt_level,
        |index, child| {
            let stdin = child.stdin.as_mut().expect("child stdin");
            stdin
                .write_all(steps[index].1)
                .expect("write fixture input");
            stdin.flush().expect("flush fixture input");
        },
    )
}

fn run_script<'a>(
    command: &mut Command,
    prefixes: impl IntoIterator<Item = &'a str>,
    opt_level: u8,
    mut on_step: impl FnMut(usize, &mut std::process::Child),
) -> Vec<String> {
    let mut child = command
        .stdout(Stdio::piped())
        .spawn()
        .expect("spawn fixture");
    let stdout = child.stdout.take().expect("child stdout");
    let (lines_tx, lines) = mpsc::channel();
    let reader = std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines() {
            let line = line.expect("read fixture stdout");
            let _ = lines_tx.send(line.clone());
        }
    });
    let mut seen = Vec::new();
    for (index, prefix) in prefixes.into_iter().enumerate() {
        loop {
            let line = match lines.recv_timeout(LINE_TIMEOUT) {
                Ok(line) => line,
                Err(error) => {
                    let _ = child.kill();
                    let output = child.wait_with_output().expect("wait for fixture");
                    reader.join().expect("join stdout reader");
                    panic!(
                        "O{opt_level}: no {prefix} line: {error} {seen:?}, {}\n{}",
                        output.status,
                        describe_output(&output)
                    );
                }
            };
            seen.push(line.clone());
            if line.starts_with(prefix) {
                break;
            }
        }
        on_step(index, &mut child);
    }
    let output = child.wait_with_output().expect("wait for fixture");
    reader.join().expect("join stdout reader");
    seen.extend(lines.try_iter());
    assert!(
        output.status.success(),
        "O{opt_level}: {seen:?} {}",
        describe_output(&output)
    );
    seen
}

/// Standard input stays silent until the program reports that its first
/// watch timed out, so the watches observe the line the test then writes. Two
/// watches of one open file description are independent, the duplicate's
/// number can close before its watch, `try_recv` agrees with `select`, and
/// `read_line` waits on standard input while both watches stay open. An
/// unopened descriptor is refused with a typed error.
#[test]
fn watch_readable_waits_for_a_foreign_descriptor() {
    require_codegen();
    let source = r#"
import std.io;

extern "C" {
    fn dup(fd: i32) -> i32;
    fn close(fd: i32) -> i32;
}

fn main() {
    match io.watch_readable(-1) {
        .Ok(_) => println("UNEXPECTED"),
        .Err(error) => println(f"REFUSED:{error}"),
    }
    let input = io.watch_readable(0).expect("watch standard input");
    let copy = unsafe { dup(0) };
    let other = io.watch_readable(copy).expect("watch a duplicate");
    unsafe { close(copy) };
    let quiet = select {
        ready from input.recv() => false,
        after 50ms => true,
    };
    let idle = match other.try_recv() {
        .Some(_) => false,
        .None => true,
    };
    println(f"QUIET:{quiet} {idle}");
    let woke = select {
        ready from input.recv() => true,
        after 10s => false,
    };
    let both = select {
        ready from other.recv() => true,
        after 10s => false,
    };
    let polled = match other.try_recv() {
        .Some(_) => true,
        .None => false,
    };
    println(f"WOKE:{woke} {both} {polled}");
    println(f"LINE:{io.read_line().expect("standard I/O") ?? "none"}");
    println(f"LINE:{io.read_line().expect("standard I/O") ?? "none"}");
    input.close();
    other.close();
}
"#;
    for opt_level in [0, 2] {
        let dir = tempfile::tempdir().expect("create fixture directory");
        let binary = compile(source, dir.path(), opt_level);
        let seen = run_scripted(
            &binary,
            &[("QUIET:", b"hello\n"), ("LINE:hello", b"again\n")],
            opt_level,
        );
        assert_eq!(
            seen,
            [
                "REFUSED:NotOpen: the number is not an open descriptor",
                "QUIET:true true",
                "WOKE:true true true",
                "LINE:hello",
                "LINE:again",
            ],
            "O{opt_level}"
        );
    }
}

/// A failed probe leaves an empty pipe idle. Restoring the descriptor limit
/// lets the watch resume normally when the parent writes to the pipe.
#[test]
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn a_failed_readiness_probe_does_not_wake_a_descriptor_watch() {
    require_codegen();
    let dir = support::tempdir();
    let limits = dir.path().join("limits.c");
    let object = dir.path().join("limits.o");
    std::fs::write(
        &limits,
        r"
#include <errno.h>
#include <poll.h>
#include <sys/resource.h>

static struct rlimit saved;

int break_probe(void) {
    if (getrlimit(RLIMIT_NOFILE, &saved) != 0) return 0;
    struct rlimit reduced = {0, saved.rlim_max};
    if (setrlimit(RLIMIT_NOFILE, &reduced) != 0) return 0;
    struct pollfd probe = {0, POLLIN, 0};
    return poll(&probe, 1, 0) == -1 && errno == EINVAL;
}

int restore_probe(void) {
    return setrlimit(RLIMIT_NOFILE, &saved);
}
",
    )
    .unwrap();
    let compiler = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
    let output = Command::new(compiler)
        .args(["-c"])
        .arg(&limits)
        .arg("-o")
        .arg(&object)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", describe_output(&output));

    let source = dir.path().join("failed_probe.hew");
    std::fs::write(
        &source,
        r#"
import std.io;

extern "C" {
    fn break_probe() -> i32;
    fn restore_probe() -> i32;
}

fn main() {
    let watch = io.watch_readable(0).expect("watch standard input");
    // Start the reactor before reducing the descriptor limit in this process.
    sleep(1ms);
    assert(unsafe { break_probe() } == 1);
    let idle = watch.try_recv().is_none();
    let restore_before_arming = RESTORE_BEFORE_ARMING;
    if restore_before_arming {
        assert(unsafe { restore_probe() } == 0);
    }
    let timed_out = select {
        ready from watch.recv() => false,
        after 20ms => true,
    };
    if !restore_before_arming {
        assert(unsafe { restore_probe() } == 0);
    }
    println(f"WAIT:{idle} {timed_out}");
    let woke = scope within 5s { watch.recv().is_some() } handle failure { false };
    println(f"WOKE:{woke}");
    println(f"LINE:{io.read_line().expect("read original input") ?? "missing"}");
    watch.close();
}
"#
        .replace(
            "RESTORE_BEFORE_ARMING",
            // On macOS, the zero limit also stops kqueue registering the
            // descriptor. Linux can still arm its epoll wait.
            if cfg!(target_os = "macos") {
                "true"
            } else {
                "false"
            },
        ),
    )
    .unwrap();
    let binary = dir.path().join("failed_probe");
    let output = Command::new(hew_binary())
        .args(["build", "--opt-level", "2", "--link-lib"])
        .arg(&object)
        .arg(&source)
        .arg("-o")
        .arg(&binary)
        .current_dir(repo_root())
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", describe_output(&output));
    let seen = run_scripted(&binary, &[("WAIT:", b"still here\n")], 2);
    assert_eq!(seen, ["WAIT:true true", "WOKE:true", "LINE:still here"]);
}

/// The program closes a watched descriptor before its watch, and a socket the
/// runtime opens next reuses the number. Closing the watch afterwards leaves
/// that socket's registration alone, so a read waiting on it still completes.
#[test]
fn closing_a_watch_after_its_descriptor_spares_a_reused_number() {
    require_codegen();
    let source = r#"
import std.io;
import std.net;

extern "C" {
    fn dup(fd: i32) -> i32;
    fn close(fd: i32) -> i32;
}

fn main() {
    let fd = unsafe { dup(0) };
    let watch = io.watch_readable(fd).expect("watch");
    let quiet = select {
        ready from watch.recv() => false,
        after 20ms => true,
    };
    println(f"QUIET:{quiet}");
    let listener = net.listen("127.0.0.1:0").expect("listen");
    unsafe { close(fd) };
    let client = net.connect(f"127.0.0.1:{listener.local_port()}").expect("connect");
    let server = listener.accept().expect("accept");
    let got = scope within 5s {
        let reader = fork client.recv();
        sleep(50ms);
        watch.close();
        server.send("hi".to_bytes()).expect("send");
        match await reader {
            .Some(data) => data.len(),
            .None => -1,
        }
    } handle failure {
        -2
    };
    println(f"READ:{got}");
    server.close();
    listener.close();
}
"#;
    for opt_level in [0, 2] {
        let dir = tempfile::tempdir().expect("create fixture directory");
        let binary = compile(source, dir.path(), opt_level);
        // Standard input stays open and silent until the read completes.
        let seen = run_scripted(&binary, &[("READ:", b"")], opt_level);
        assert_eq!(seen, ["QUIET:true", "READ:2"], "O{opt_level}");
    }
}

/// A foreign writer has filled a nonblocking pipe. A timer wins the first
/// selection, then draining the pipe wakes an ordinary forked receive. The
/// watched duplicate remains attached to the pipe after its source fd is reused.
#[test]
fn writable_watch_waits_for_pipe_capacity_without_writing() {
    use std::os::fd::AsRawFd;

    require_codegen();
    let source = r#"
import std.io;
import std.stream;
extern "C" {
    fn dup(fd: i32) -> i32;
    fn dup2(from_fd: i32, to_fd: i32) -> i32;
    fn close(fd: i32) -> i32;
}
fn wait_for_space(consume watch: stream.Stream<()>) -> bool {
    let woke = watch.recv().is_some();
    let still_ready = watch.try_recv().is_some();
    watch.close();
    woke && still_ready
}
fn main() {
    let fd = unsafe { dup(2) };
    let watch = io.watch_writable(fd).expect("watch foreign writer");
    assert(unsafe { dup2(0, fd) } == fd);
    let invalid = match io.watch_writable(fd) {
        .Err(.Unsupported) => true,
        _ => false,
    };
    let full = watch.try_recv().is_none();
    let timed_out = select {
        ready from watch.recv() => false,
        after 20ms => true,
    };
    println(f"FULL:{invalid} {full} {timed_out}");
    let writable = scope within 5s {
        let pending = fork wait_for_space(watch);
        println("DRAIN");
        await pending
    };
    println(f"READY:{writable}");
    unsafe { close(fd) };
    io.write_err("sent").expect("write to original");
}
"#;
    let directory = support::tempdir();
    for opt in [0, 2] {
        let binary = compile(source, directory.path(), opt);
        let (mut reader, mut writer) = std::io::pipe().unwrap();
        let fd = writer.as_raw_fd();
        // SAFETY: the writer owns fd while both operations update its status flags.
        unsafe {
            let flags = libc::fcntl(fd, libc::F_GETFL);
            assert!(flags >= 0);
            assert_eq!(libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK), 0);
        }
        let mut filled = 0;
        loop {
            match writer.write(&[0x5a; 4096]) {
                Ok(n) => {
                    assert!(n > 0);
                    filled += n;
                }
                Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => break,
                Err(error) => panic!("fill pipe: {error}"),
            }
        }
        assert!(filled > 0);
        // FreeBSD's piped stdin can be read-write. Use an explicitly read-only
        // descriptor so reusing the original fd exercises wrong-access refusal.
        let read_only = std::fs::File::open("/dev/null").expect("open read-only descriptor");
        let seen = run_script(
            Command::new(&binary).stdin(read_only).stderr(writer),
            ["DRAIN"],
            opt,
            |_, _| {
                let mut existing = vec![0; filled];
                reader.read_exact(&mut existing).unwrap();
                assert!(existing.iter().all(|byte| *byte == 0x5a));
            },
        );
        assert_eq!(
            seen,
            ["FULL:true true true", "DRAIN", "READY:true"],
            "O{opt}"
        );
        let mut delivered = Vec::new();
        reader.read_to_end(&mut delivered).unwrap();
        assert_eq!(delivered, b"sent");
    }
}

/// Exhaust the descriptor table of one child without changing the test
/// runner's limits. A valid descriptor must retain the actual OS failure.
#[test]
fn descriptor_exhaustion_is_distinct_from_a_closed_descriptor() {
    require_codegen();
    let source = r#"
import std.io;
extern "C" { fn dup(fd: i32) -> i32; }
fn main() {
    while unsafe { dup(0) } >= 0 {}
    match io.watch_readable(0) {
        .Err(.System(detail)) => println(f"system:{detail}"),
        _ => println("unexpected"),
    }
}
"#;
    let directory = support::tempdir();
    let binary = compile(source, directory.path(), 2);
    let output = Command::new("sh")
        .args(["-c", "ulimit -n 64 && exec \"$1\"", "descriptor-limit"])
        .arg(binary)
        .stdin(Stdio::null())
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", describe_output(&output));
    let text = String::from_utf8_lossy(&output.stdout);
    assert!(
        text.starts_with("system:") && text.trim().len() > "system:".len(),
        "{text}"
    );
}
