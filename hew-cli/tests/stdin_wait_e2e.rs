//! Standard input is a waiting call: a silent stdin leaves the scheduler free,
//! a deadline cancels a read without losing input, and partial lines and end
//! of input arrive as written. Each program runs natively at O0 and O2 with a
//! stdin pipe this test holds open and feeds on its own schedule.

mod support;

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use support::{hew_binary, repo_root, require_codegen};

const TICKER: &str = r#"
import std.io;
import std.time.datetime;

fn reader() -> string {
    io.read_line().unwrap_or("eof")
}

fn ticks() -> i64 {
    let start = datetime.now_ms();
    for _ in 0..20 {
        sleep(20ms);
    }
    datetime.now_ms() - start
}

fn main() {
    var elapsed = 0;
    var got = "";
    scope {
        let t = fork ticks();
        let r = fork reader();
        elapsed = await t;
        got = await r;
    };
    println(f"ticks {elapsed}");
    println(f"reader {got}");
}
"#;

const WITHIN: &str = r#"
import std.io;
import std.time.datetime;

fn main() {
    let start = datetime.now_ms();
    let first = scope within 200ms {
        io.read_line()
    } handle failure {
        .Some("deadline")
    };
    println(f"elapsed {datetime.now_ms() - start}");
    println(first.unwrap_or("eof"));
    println(io.read_line().unwrap_or("eof"));
}
"#;

const LINES: &str = r#"
import std.io;

fn main() {
    var reading = true;
    while reading {
        match io.read_line() {
            .Some(line) => println(f"[{line}]"),
            .None => {
                reading = false;
            }
        }
    }
    println("end");
    println(io.read_line().unwrap_or("eof again"));
}
"#;

fn build(dir: &Path, name: &str, source: &str, opt: &str) -> PathBuf {
    let src = dir.join(format!("{name}.hew"));
    std::fs::write(&src, source).expect("write source");
    let out = dir.join(format!("{name}-{opt}"));
    let output = Command::new(hew_binary())
        .current_dir(repo_root())
        .args(["build", "--opt-level", &opt[1..]])
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("run hew build");
    assert!(
        output.status.success(),
        "hew build {name} -{opt} failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    out
}

fn spawn(binary: &Path, workers: Option<&str>, leak_check: bool) -> Child {
    let mut command = Command::new(binary);
    command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    if let Some(workers) = workers {
        command.env("HEW_WORKERS", workers);
    }
    if leak_check {
        command.env("HEW_IO_LEAK_CHECK", "1");
    }
    command.spawn().expect("spawn program")
}

/// Feed `steps` (pause, then bytes), close stdin, and return stdout after a
/// successful exit within ten seconds.
fn drive(mut child: Child, steps: &[(u64, &[u8])]) -> String {
    let mut stdin = child.stdin.take().expect("stdin pipe");
    for (pause, bytes) in steps {
        std::thread::sleep(Duration::from_millis(*pause));
        stdin.write_all(bytes).expect("write stdin");
        stdin.flush().expect("flush stdin");
    }
    drop(stdin);
    let deadline = Instant::now() + Duration::from_secs(10);
    let status = loop {
        if let Some(status) = child.try_wait().expect("poll child") {
            break status;
        }
        if Instant::now() > deadline {
            let _ = child.kill();
            panic!("program did not exit within ten seconds");
        }
        std::thread::sleep(Duration::from_millis(10));
    };
    let mut stdout = String::new();
    let mut stderr = String::new();
    child
        .stdout
        .take()
        .expect("stdout")
        .read_to_string(&mut stdout)
        .expect("read stdout");
    child
        .stderr
        .take()
        .expect("stderr")
        .read_to_string(&mut stderr)
        .expect("read stderr");
    assert!(
        status.success(),
        "exit {status}\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
    stdout
}

fn field(stdout: &str, key: &str) -> i64 {
    stdout
        .lines()
        .find_map(|line| line.strip_prefix(key)?.trim().parse().ok())
        .unwrap_or_else(|| panic!("no `{key}` in:\n{stdout}"))
}

#[test]
fn silent_stdin_leaves_the_only_worker_free() {
    require_codegen();
    let dir = tempfile::tempdir().expect("tempdir");
    for opt in ["O0", "O2"] {
        let binary = build(dir.path(), "ticker", TICKER, opt);
        // Input arrives long after the twenty 20 ms sleeps should finish. A
        // read that held the worker would delay them until the input.
        let stdout = drive(spawn(&binary, Some("1"), false), &[(1500, b"x\n")]);
        let ticks = field(&stdout, "ticks ");
        assert!(ticks < 1000, "{opt}: sleeps waited on stdin: {stdout}");
        assert!(stdout.contains("reader x\n"), "{opt}: {stdout}");
    }
}

#[test]
fn a_deadline_cancels_a_read_and_the_next_read_gets_the_line() {
    require_codegen();
    let dir = tempfile::tempdir().expect("tempdir");
    for opt in ["O0", "O2"] {
        let binary = build(dir.path(), "within", WITHIN, opt);
        let stdout = drive(spawn(&binary, None, true), &[(800, b"late\n")]);
        let elapsed = field(&stdout, "elapsed ");
        assert!((200..700).contains(&elapsed), "{opt}: {stdout}");
        assert!(stdout.ends_with("deadline\nlate\n"), "{opt}: {stdout}");
    }
}

#[test]
fn partial_lines_join_and_end_of_input_reports_none() {
    require_codegen();
    let dir = tempfile::tempdir().expect("tempdir");
    for opt in ["O0", "O2"] {
        let binary = build(dir.path(), "lines", LINES, opt);
        let stdout = drive(
            spawn(&binary, Some("1"), true),
            &[(0, b"abc"), (300, b"def\n\ncrlf\r\n"), (100, b"tail")],
        );
        assert_eq!(
            stdout, "[abcdef]\n[]\n[crlf]\n[tail]\nend\neof again\n",
            "{opt}"
        );
    }
}
