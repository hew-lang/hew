//! `hew run` owns one temporary directory per invocation and removes it on
//! every path it controls, leaves other invocations' directories alone, and
//! never leaves its program running after it is gone.
//!
//! Each test points the platform temporary directory (TMPDIR, TMP, TEMP) at a
//! private directory, so "nothing is left behind" observes only its own runs.

mod support;

use std::io::{BufRead, BufReader, Read};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output, Stdio};
use std::time::{Duration, Instant};

use support::{describe_output, hew_binary, require_codegen};

struct Fixture {
    source_dir: tempfile::TempDir,
    temp_root: tempfile::TempDir,
}

impl Fixture {
    fn new() -> Self {
        Self {
            source_dir: support::tempdir(),
            temp_root: support::tempdir(),
        }
    }

    fn source(&self, name: &str, text: &str) -> PathBuf {
        let path = self.source_dir.path().join(name);
        std::fs::write(&path, text).expect("write source");
        path
    }

    fn hew_run(&self, source: &Path, extra: &[&str]) -> Command {
        let mut command = Command::new(hew_binary());
        command
            .arg("run")
            .args(extra)
            .arg(source)
            .current_dir(self.source_dir.path())
            .env("TMPDIR", self.temp_root.path())
            .env("TMP", self.temp_root.path())
            .env("TEMP", self.temp_root.path());
        command
    }

    fn run(&self, source: &Path, extra: &[&str]) -> Output {
        support::run_bounded_command(self.hew_run(source, extra), "hew run")
    }

    /// Every entry hew created in the private temporary directory.
    fn leftovers(&self) -> Vec<String> {
        std::fs::read_dir(self.temp_root.path())
            .expect("read temp root")
            .flatten()
            .map(|entry| entry.file_name().to_string_lossy().into_owned())
            .filter(|name| name.starts_with("hew-"))
            .collect()
    }
}

#[test]
fn every_exit_path_removes_the_runs_own_directory() {
    require_codegen();
    let fixture = Fixture::new();

    let ok = fixture.source("ok.hew", "fn main() {\n    println(1);\n}\n");
    let output = fixture.run(&ok, &[]);
    assert!(output.status.success(), "{}", describe_output(&output));
    assert_eq!(fixture.leftovers(), Vec::<String>::new(), "normal exit");

    let failing = fixture.source(
        "failing.hew",
        "fn main() {\n    let v: Vec<i64> = Vec.new();\n    println(v[3]);\n}\n",
    );
    let output = fixture.run(&failing, &[]);
    assert_eq!(
        output.status.code(),
        Some(1),
        "{}",
        describe_output(&output)
    );
    assert_eq!(fixture.leftovers(), Vec::<String>::new(), "failing program");

    let broken = fixture.source("broken.hew", "fn main() {\n    let x: i64 = \"text\";\n}\n");
    let output = fixture.run(&broken, &[]);
    assert!(!output.status.success(), "{}", describe_output(&output));
    assert_eq!(fixture.leftovers(), Vec::<String>::new(), "compile error");

    let spin = fixture.source(
        "spin.hew",
        "fn main() {\n    var i: i64 = 0;\n    loop {\n        i += 1;\n    }\n}\n",
    );
    let output = fixture.run(&spin, &["--timeout", "1s"]);
    assert_eq!(
        output.status.code(),
        Some(124),
        "{}",
        describe_output(&output)
    );
    assert_eq!(fixture.leftovers(), Vec::<String>::new(), "timeout");
}

#[test]
fn a_run_leaves_directories_it_did_not_create_alone() {
    require_codegen();
    let fixture = Fixture::new();
    // What a killed earlier run, or any other tool, left in the same place.
    let foreign = fixture.temp_root.path().join("hew-run-abandoned");
    std::fs::create_dir(&foreign).unwrap();
    std::fs::write(foreign.join("program"), b"stale").unwrap();

    let ok = fixture.source("ok.hew", "fn main() {\n    println(1);\n}\n");
    let output = fixture.run(&ok, &[]);
    assert!(output.status.success(), "{}", describe_output(&output));
    assert!(foreign.join("program").exists(), "not this run's to remove");
    assert_eq!(fixture.leftovers(), vec!["hew-run-abandoned".to_string()]);
}

#[test]
fn the_program_is_named_after_its_source() {
    require_codegen();
    let fixture = Fixture::new();
    let source = fixture.source(
        "greeter.hew",
        "import std.os;\n\nfn main() {\n    match os.args().get(0) {\n        \
         .Some(name) => println(name),\n        .None => println(\"\"),\n    }\n}\n",
    );
    let output = fixture.run(&source, &[]);
    assert!(output.status.success(), "{}", describe_output(&output));
    let argv0 = String::from_utf8_lossy(&output.stdout);
    let name = Path::new(argv0.trim()).file_stem().unwrap_or_default();
    assert_eq!(name, "greeter", "{argv0}");
}

/// A program that prints its process id, then waits forever.
#[cfg(unix)]
const REPORT_PID_AND_WAIT: &str = "import std.os;\n\nfn main() {\n    println(os.pid());\n    \
                                   var i: i64 = 0;\n    loop {\n        i += 1;\n    }\n}\n";

#[cfg(unix)]
fn start_reporting(fixture: &Fixture) -> (Child, i32) {
    let source = fixture.source("report.hew", REPORT_PID_AND_WAIT);
    let mut runner = fixture
        .hew_run(&source, &[])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn hew run");
    let mut line = String::new();
    BufReader::new(runner.stdout.take().unwrap())
        .read_line(&mut line)
        .expect("read program pid");
    let pid = line
        .trim()
        .parse()
        .unwrap_or_else(|_| panic!("pid line: {line:?}"));
    (runner, pid)
}

/// Whether `pid` is gone: no such process, or a zombie awaiting its reaper.
#[cfg(unix)]
fn exited(pid: i32) -> bool {
    let state = Command::new("ps")
        .args(["-o", "stat=", "-p", &pid.to_string()])
        .output()
        .expect("run ps");
    let state = String::from_utf8_lossy(&state.stdout);
    state.trim().is_empty() || state.trim_start().starts_with('Z')
}

#[cfg(unix)]
fn wait_until(what: &str, deadline: Duration, mut done: impl FnMut() -> bool) {
    let started = Instant::now();
    while !done() {
        assert!(
            started.elapsed() < deadline,
            "timed out waiting until {what}"
        );
        std::thread::sleep(Duration::from_millis(20));
    }
}

#[cfg(unix)]
#[test]
fn a_terminated_run_stops_its_program_and_removes_its_directory() {
    require_codegen();
    let fixture = Fixture::new();
    let (mut runner, pid) = start_reporting(&fixture);
    assert_eq!(
        fixture.leftovers().len(),
        1,
        "the running program's directory"
    );
    // SAFETY: the runner is an unreaped child of this test.
    let sent = unsafe { libc::kill(runner.id().cast_signed(), libc::SIGTERM) };
    assert_eq!(sent, 0);
    let status = runner.wait().expect("wait for hew run");
    assert!(!status.success(), "{status}");
    wait_until("the program exits", Duration::from_secs(10), || exited(pid));
    assert_eq!(fixture.leftovers(), Vec::<String>::new());
}

/// SIGKILL runs no code in the runner, so its directory stays, as `go run`'s
/// does. The program must not outlive it on platforms that can arrange that.
#[cfg(any(target_os = "linux", target_os = "freebsd"))]
#[test]
fn a_killed_run_takes_its_program_with_it() {
    require_codegen();
    let fixture = Fixture::new();
    let (mut runner, pid) = start_reporting(&fixture);
    runner.kill().expect("SIGKILL hew run");
    runner.wait().expect("reap hew run");
    wait_until("the orphaned program dies", Duration::from_secs(10), || {
        exited(pid)
    });
}

/// Under a terminal the program shares the terminal with the runner, exactly
/// as it would run on its own: it reads interactive input and answers.
#[cfg(unix)]
#[test]
fn an_interactive_program_reads_from_the_terminal() {
    use std::io::Write;
    use std::os::fd::{FromRawFd, OwnedFd};
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::process::CommandExt;

    require_codegen();
    let fixture = Fixture::new();
    let source = fixture.source(
        "ask.hew",
        "import std.io;\n\nfn main() {\n    println(\"name?\");\n    \
         match io.read_line().expect(\"standard I/O\") {\n        \
         .Some(name) => println(f\"hi {name}\"),\n        .None => println(\"no input\"),\n    }\n}\n",
    );

    // SAFETY: plain pseudo-terminal allocation; every return is checked.
    let (master, terminal) = unsafe {
        let master = libc::posix_openpt(libc::O_RDWR | libc::O_NOCTTY);
        assert!(
            master >= 0,
            "posix_openpt: {}",
            std::io::Error::last_os_error()
        );
        assert_eq!(libc::grantpt(master), 0);
        assert_eq!(libc::unlockpt(master), 0);
        let name = libc::ptsname(master);
        assert!(!name.is_null());
        let path = std::ffi::CStr::from_ptr(name).to_owned();
        (OwnedFd::from_raw_fd(master), path)
    };
    let open_terminal = || {
        std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(std::ffi::OsStr::from_bytes(terminal.to_bytes()))
            .expect("open pseudo-terminal")
    };
    let mut command = fixture.hew_run(&source, &[]);
    command
        .stdin(open_terminal())
        .stdout(open_terminal())
        .stderr(open_terminal());
    // SAFETY: runs in the forked child before exec: a new session whose
    // controlling terminal is the pseudo-terminal on stdin.
    unsafe {
        command.pre_exec(|| {
            #[allow(
                trivial_numeric_casts,
                clippy::cast_lossless,
                reason = "the ioctl request type differs across Unix C libraries"
            )]
            let request = libc::TIOCSCTTY as _;
            if libc::setsid() < 0 || libc::ioctl(0, request, 0) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
    let mut runner = command.spawn().expect("spawn hew run on a terminal");

    let mut reader = std::fs::File::from(master.try_clone().expect("clone master"));
    let (sender, receiver) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut buffer = [0u8; 256];
        while let Ok(read) = reader.read(&mut buffer) {
            if read == 0 || sender.send(buffer[..read].to_vec()).is_err() {
                break;
            }
        }
    });
    let mut screen = Vec::new();
    let mut await_text = |text: &str| {
        let deadline = Instant::now() + Duration::from_mins(1);
        while !String::from_utf8_lossy(&screen).contains(text) {
            let left = deadline.saturating_duration_since(Instant::now());
            let Ok(bytes) = receiver.recv_timeout(left) else {
                let _ = runner.kill();
                panic!(
                    "no {text:?} on the terminal; saw {:?}",
                    String::from_utf8_lossy(&screen)
                );
            };
            screen.extend(bytes);
        }
    };
    await_text("name?");
    std::fs::File::from(master)
        .write_all(b"alice\n")
        .expect("type on the terminal");
    await_text("hi alice");
    let status = runner.wait().expect("wait for hew run");
    assert!(status.success(), "{status}");
    assert_eq!(fixture.leftovers(), Vec::<String>::new());
}

/// Ctrl-C while the program is still compiling ends the run at once and
/// removes the half-built directory; the program never starts.
#[cfg(unix)]
#[test]
fn an_interrupt_during_the_compile_removes_the_build() {
    require_codegen();
    let fixture = Fixture::new();
    // Enough code that the debug compiler spends a visible while on it.
    let mut text = String::new();
    for index in 0..4000 {
        text.push_str(&format!(
            "fn step{index}(x: i64) -> i64 {{\n    if x > {index} {{ x - 1 }} else {{ x + {index} }}\n}}\n"
        ));
    }
    text.push_str("fn main() {\n    println(\"program ran\");\n    println(step0(1));\n}\n");
    let source = fixture.source("large.hew", &text);
    let mut runner = fixture
        .hew_run(&source, &[])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn hew run");
    wait_until(
        "the build directory exists",
        Duration::from_secs(30),
        || !fixture.leftovers().is_empty(),
    );
    // SAFETY: the runner is an unreaped child of this test.
    let sent = unsafe { libc::kill(runner.id().cast_signed(), libc::SIGINT) };
    assert_eq!(sent, 0);
    let output = runner.wait_with_output().expect("wait for hew run");
    assert_eq!(
        output.status.code(),
        Some(128 + libc::SIGINT),
        "{}",
        describe_output(&output)
    );
    assert!(
        !String::from_utf8_lossy(&output.stdout).contains("program ran"),
        "{}",
        describe_output(&output)
    );
    assert_eq!(fixture.leftovers(), Vec::<String>::new());
}
