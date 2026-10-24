//! Exercise the CLI through real OS signals and a controlling terminal. Every
//! fixture publishes its own PID before the test sends a termination request.
#![cfg(unix)]

mod support;

use std::fs::File;
use std::io::{BufRead, BufReader, Read, Write};
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::os::unix::process::CommandExt;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

struct OwnedRun {
    cli: Child,
    program_pid: Option<i32>,
    terminal: Option<File>,
}

impl OwnedRun {
    fn wait(&mut self) -> ExitStatus {
        let deadline = Instant::now() + Duration::from_secs(10);
        loop {
            if let Some(status) = self.cli.try_wait().expect("poll hew run") {
                return status;
            }
            assert!(Instant::now() < deadline, "hew run did not terminate");
            std::thread::sleep(Duration::from_millis(10));
        }
    }
}

impl Drop for OwnedRun {
    fn drop(&mut self) {
        // Hang up the owned terminal before reaping, including failed checks.
        // A session leader must not wait for a master this test stopped reading.
        drop(self.terminal.take());
        // Only signal this fixture's published, isolated group. Keep the CLI
        // alive to reap its child whenever possible, including assertion paths.
        if let Some(pid) = self.program_pid {
            // SAFETY: the PID came from this owned fixture's startup handshake.
            unsafe { libc::killpg(pid, libc::SIGKILL) };
        }
        if self.cli.try_wait().ok().flatten().is_none() {
            // SAFETY: the CLI is an unreaped child, so this identity is owned.
            unsafe { libc::kill(self.cli.id().cast_signed(), libc::SIGTERM) };
            let deadline = Instant::now() + Duration::from_secs(3);
            while self.cli.try_wait().ok().flatten().is_none() && Instant::now() < deadline {
                std::thread::sleep(Duration::from_millis(10));
            }
            if self.cli.try_wait().ok().flatten().is_none() {
                let _ = self.cli.kill();
            }
        }
        let _ = self.cli.wait();
    }
}

#[allow(
    clippy::useless_conversion,
    reason = "ioctl request types differ between Linux and macOS"
)]
fn attach_controlling_terminal(command: &mut Command) -> File {
    let mut master = -1;
    let mut slave = -1;
    // SAFETY: both output descriptors are valid pointers; null termios and
    // winsize select the system's default terminal settings.
    let result = unsafe {
        libc::openpty(
            &raw mut master,
            &raw mut slave,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        )
    };
    assert_eq!(result, 0, "create pseudo-terminal");
    // SAFETY: openpty returned two new, independently owned descriptors.
    let master = unsafe { File::from_raw_fd(master) };
    // SAFETY: openpty returned a separate owned slave descriptor.
    let slave = unsafe { OwnedFd::from_raw_fd(slave) };
    // Only stdin uses this terminal; output is captured through separate pipes.
    // Unread input echo can keep macOS draining the slave during process exit.
    // Preserve canonical input and terminal-generated signals, without echo.
    // SAFETY: zero-initialize the output buffer before tcgetattr fills it.
    let mut settings: libc::termios = unsafe { std::mem::zeroed() };
    assert_eq!(
        // SAFETY: the slave descriptor and output termios buffer are valid.
        unsafe { libc::tcgetattr(slave.as_raw_fd(), &raw mut settings) },
        0,
        "read terminal settings"
    );
    settings.c_lflag &= !(libc::ECHO | libc::ECHONL);
    assert_eq!(
        // SAFETY: apply valid settings immediately to this owned terminal.
        unsafe { libc::tcsetattr(slave.as_raw_fd(), libc::TCSANOW, &raw const settings) },
        0,
        "disable input echo"
    );
    for descriptor in [master.as_raw_fd(), slave.as_raw_fd()] {
        assert_eq!(
            // SAFETY: mark only these owned descriptors close-on-exec. Command
            // duplicates the slave onto stdin before closing the original.
            unsafe { libc::fcntl(descriptor, libc::F_SETFD, libc::FD_CLOEXEC) },
            0,
            "keep terminal descriptors private"
        );
    }
    command.stdin(Stdio::from(slave));
    // SAFETY: only async-signal-safe session/terminal operations run after
    // fork. The slave has already been installed as this child's stdin.
    unsafe {
        command.pre_exec(|| {
            if libc::setsid() == -1 || libc::ioctl(0, libc::TIOCSCTTY.into(), 0) == -1 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
    master
}

fn exercise(signal: i32, timeout: bool, terminal: bool, ignore: bool) {
    support::require_codegen();
    let dir = support::tempdir();
    let source = dir.path().join("signal.hew");
    // Select a known disposition after runtime initialization so this tests
    // forwarding independently of the runtime's graceful shutdown policy.
    let handler = i32::from(ignore);
    let setup = format!("unsafe {{ signal({signal}, {handler}); }}");
    std::fs::write(
        &source,
        format!(
            "extern \"C\" {{ fn getpid() -> i32; fn signal(number: i32, handler: usize) -> usize; }}\n\
             fn main() {{ {setup} println(unsafe {{ getpid() }}); loop {{ sleep(100ms); }} }}\n"
        ),
    )
    .unwrap();

    let mut command = Command::new(support::hew_binary());
    command.arg("run");
    if timeout {
        command.args(["--timeout", "30s"]);
    }
    command.arg(&source).current_dir(dir.path());
    command.stdout(Stdio::piped()).stderr(Stdio::inherit());

    let master = if terminal {
        Some(attach_controlling_terminal(&mut command))
    } else {
        command.stdin(Stdio::null());
        None
    };

    let mut run = OwnedRun {
        cli: command.spawn().expect("spawn hew run"),
        program_pid: None,
        terminal: master,
    };
    let stdout = run.cli.stdout.take().unwrap();
    let (sender, receiver) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut line = String::new();
        let result = BufReader::new(stdout).read_line(&mut line);
        let _ = sender.send((result, line));
    });
    let (read, line) = receiver
        .recv_timeout(Duration::from_mins(1))
        .expect("program startup handshake");
    read.expect("read program PID");
    let pid = line.trim().parse::<i32>().expect("numeric program PID");
    run.program_pid = Some(pid);

    if let Some(master) = run.terminal.as_mut() {
        master.write_all(&[3]).expect("type terminal Ctrl-C");
    } else {
        // SAFETY: this is the unreaped CLI child owned by this test.
        assert_eq!(unsafe { libc::kill(run.cli.id().cast_signed(), signal) }, 0);
    }
    let status = run.wait();
    assert_eq!(status.code(), Some(1), "signal exit policy: {status}");
    // ESRCH proves both termination and reaping; a zombie still passes kill(0).
    // SAFETY: signal zero only queries the fixture PID without changing it.
    let child_liveness = unsafe { libc::kill(pid, 0) };
    assert_eq!(child_liveness, -1, "program survived hew run");
    assert_eq!(
        std::io::Error::last_os_error().raw_os_error(),
        Some(libc::ESRCH)
    );
    run.program_pid = None;
}

#[test]
fn run_forwards_term_and_hup_and_reaps_child() {
    for signal in [libc::SIGTERM, libc::SIGHUP] {
        for timeout in [false, true] {
            exercise(signal, timeout, false, false);
        }
    }
}

#[test]
fn run_terminal_ctrl_c_reaps_child() {
    for timeout in [false, true] {
        exercise(libc::SIGINT, timeout, true, false);
    }
}

#[test]
fn run_escalates_ignored_termination_and_reaps_child() {
    for signal in [libc::SIGTERM, libc::SIGHUP, libc::SIGINT] {
        exercise(signal, false, false, true);
    }
}

#[test]
fn run_wasi_foreground_stdin_is_readable() {
    support::require_wasi_runner();
    let dir = support::tempdir();
    let source = dir.path().join("stdin.hew");
    std::fs::write(
        &source,
        "import std.io;\nfn main() { println(\"ready\"); println(io.read_line().expect(\"standard I/O\") ?? \"eof\"); }\n",
    )
    .unwrap();
    let mut command = Command::new(support::hew_binary());
    command
        .args(["run", "--target", "wasm32-wasi", "--timeout", "10s"])
        .arg(&source)
        .current_dir(dir.path())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit());
    let master = attach_controlling_terminal(&mut command);
    let mut run = OwnedRun {
        cli: command.spawn().expect("spawn WASI hew run"),
        program_pid: None,
        terminal: Some(master),
    };
    let stdout = run.cli.stdout.take().unwrap();
    let (ready_sender, ready_receiver) = std::sync::mpsc::channel();
    let (output_sender, output_receiver) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut stdout = BufReader::new(stdout);
        let mut ready = String::new();
        let read = stdout.read_line(&mut ready);
        let _ = ready_sender.send(read.map(|_| ready));
        let mut output = String::new();
        let read = stdout.read_to_string(&mut output);
        let _ = output_sender.send(read.map(|_| output));
    });
    let ready = ready_receiver
        .recv_timeout(Duration::from_mins(1))
        .expect("WASI program startup")
        .expect("read WASI startup line");
    assert_eq!(ready, "ready\n");
    run.terminal
        .as_mut()
        .unwrap()
        .write_all(b"foreground input\n")
        .expect("type terminal input");
    let status = run.wait();
    assert!(status.success(), "WASI terminal reader failed: {status}");
    let output = output_receiver
        .recv_timeout(Duration::from_secs(3))
        .expect("WASI terminal reader output")
        .expect("read WASI output");
    assert_eq!(output, "foreground input\n");
}
