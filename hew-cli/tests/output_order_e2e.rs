//! Standard output and standard error share one ordered writer: writes from
//! `main` and from an actor, on either stream, reach a shared file in program
//! order, and a panic report follows the output written before it. A reader
//! that stops early ends the program instead of leaving it blocked. Each
//! program runs natively at O0 and O2.

mod support;

use std::fmt::Write as _;
#[cfg(unix)]
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
#[cfg(unix)]
use std::time::{Duration, Instant};

use support::{hew_binary, repo_root, require_codegen};

const INTERLEAVE: &str = r#"
import std.io;

actor Echo {
    receive fn say(n: i64) -> i64 {
        println(f"actor out {n}");
        io.write_err(f"actor err {n}\n");
        n
    }
}

fn main() {
    let echo = spawn Echo();
    for i in 0..3 {
        print(f"main {i} ");
        println(1.5);
        match echo.say(i) {
            .Ok(_) => {}
            .Err(_) => println("lost"),
        }
        io.write_err(f"main err {i}\n");
    }
    io.write("before panic\n");
    panic("last words");
}
"#;

#[cfg(unix)]
const ENDLESS: &str = r#"
fn main() {
    var i = 0;
    loop {
        println(f"line {i}");
        i += 1;
    }
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

#[test]
fn stdout_and_stderr_reach_one_file_in_program_order() {
    require_codegen();
    let dir = tempfile::tempdir().expect("temp dir");
    for opt in ["O0", "O2"] {
        let binary = build(dir.path(), "interleave", INTERLEAVE, opt);
        for workers in ["1", "4"] {
            let log = dir.path().join(format!("combined-{opt}-{workers}.txt"));
            let file = std::fs::File::create(&log).expect("create log");
            let status = Command::new(&binary)
                .env("HEW_WORKERS", workers)
                .stdout(Stdio::from(file.try_clone().expect("clone log")))
                .stderr(Stdio::from(file))
                .status()
                .expect("run program");
            assert_eq!(status.code(), Some(1), "{opt} workers={workers}");
            let combined = std::fs::read_to_string(&log).expect("read log");
            let mut expected = String::new();
            for i in 0..3 {
                write!(
                    expected,
                    "main {i} 1.5\nactor out {i}\nactor err {i}\nmain err {i}\n"
                )
                .expect("format expected output");
            }
            expected.push_str("before panic\n");
            assert!(
                combined.starts_with(&expected),
                "{opt} workers={workers}:\n{combined}"
            );
            let report = &combined[expected.len()..];
            assert!(
                report.contains("last words") && report.lines().count() == 1,
                "{opt} workers={workers}: the panic report must come last:\n{combined}"
            );
        }
    }
}

#[cfg(unix)]
#[test]
fn a_reader_that_stops_early_ends_the_writer() {
    require_codegen();
    let dir = tempfile::tempdir().expect("temp dir");
    for opt in ["O0", "O2"] {
        let binary = build(dir.path(), "endless", ENDLESS, opt);
        let mut child = Command::new(&binary)
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn program");
        let mut first = String::new();
        BufReader::new(child.stdout.take().expect("stdout pipe"))
            .read_line(&mut first)
            .expect("read first line");
        assert_eq!(first, "line 0\n");
        // The reader is dropped here, as `| head -1` does.
        let deadline = Instant::now() + Duration::from_secs(10);
        loop {
            if child.try_wait().expect("poll child").is_some() {
                break;
            }
            if Instant::now() > deadline {
                let _ = child.kill();
                panic!("{opt}: the writer kept running after its reader left");
            }
            std::thread::sleep(Duration::from_millis(10));
        }
    }
}
