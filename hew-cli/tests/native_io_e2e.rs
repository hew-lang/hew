//! Native I/O preserves ordinary results while suspending its source caller.

mod support;

use std::path::Path;
use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

fn build(source: &str, directory: &Path, opt: &str) -> std::path::PathBuf {
    let input = directory.join("io.hew");
    std::fs::write(&input, source).unwrap();
    let binary = hew_testutil::compiled_binary_path(directory, &format!("io-{opt}"));
    let mut command = Command::new(hew_binary());
    command
        .arg("build")
        .arg(input)
        .args(["--opt-level", opt, "-o"])
        .arg(&binary);
    let output = run_bounded_command(command, "build native I/O");
    assert!(output.status.success(), "{}", describe_output(&output));
    binary
}

fn run(binary: &Path, directory: &Path, expected: &str) {
    let mut command = Command::new(binary);
    command.current_dir(directory).env("HEW_WORKERS", "1");
    let output = run_bounded_command(command, "execute native I/O");
    assert!(output.status.success(), "{}", describe_output(&output));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected,
        "{}",
        describe_output(&output)
    );
    assert!(output.stderr.is_empty(), "{}", describe_output(&output));
}

#[test]
fn file_requests_transfer_strings_and_bytes_and_restore_each_error() {
    require_codegen();
    for opt in ["0", "2"] {
        let directory = tempdir();
        let binary = build(
            r#"
import std.fs;
fn main() {
    match fs.write("first.txt", "native io") {
        .Ok(_) => println("written"),
        .Err(_) => println("write failed"),
    }
    match fs.read_bytes("missing.txt") {
        .Ok(_) => println("wrong success"),
        .Err(_) => println("missing recovered"),
    }
    match fs.read_bytes("first.txt") {
        .Ok(data) => {
            match fs.write_bytes("second.txt", data) {
                .Ok(_) => println("bytes copied"),
                .Err(_) => println("copy failed"),
            }
        },
        .Err(_) => println("stale error"),
    }
    match fs.read("second.txt") {
        .Ok(text) => println(text),
        .Err(_) => println("read failed"),
    }
    match fs.write("missing-directory/file", "data") {
        .Ok(_) => println("wrong write success"),
        .Err(_) => println("write error recovered"),
    }
}
"#,
            directory.path(),
            opt,
        );
        run(
            &binary,
            directory.path(),
            "written\nmissing recovered\nbytes copied\nnative io\nwrite error recovered\n",
        );
        assert_eq!(
            std::fs::read(directory.path().join("second.txt")).unwrap(),
            b"native io"
        );
    }
}

#[cfg(unix)]
#[test]
fn a_file_deadline_recovers_before_the_blocking_producer_finishes() {
    use std::ffi::CString;
    use std::io::Write;
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::OpenOptionsExt;
    use std::time::{Duration, Instant};

    require_codegen();
    for opt in ["0", "2"] {
        let directory = tempdir();
        let fifo = directory.path().join("input.pipe");
        let name = CString::new(fifo.as_os_str().as_bytes()).unwrap();
        // SAFETY: name is a NUL-terminated path in this test's private directory.
        assert_eq!(unsafe { libc::mkfifo(name.as_ptr(), 0o600) }, 0);
        let binary = build(
            r#"
import std.fs;
fn main() {
    scope within 1s {
        let data = fs.read_bytes("input.pipe");
        println("unexpected read completion");
    } handle failure { println("deadline recovered"); };
    let marker = fs.write("cancelled.marker", "ready");
}
"#,
            directory.path(),
            opt,
        );
        let marker = directory.path().join("cancelled.marker");
        let producer = std::thread::spawn(move || {
            let limit = Instant::now() + Duration::from_secs(10);
            let mut output = loop {
                if let Ok(output) = std::fs::OpenOptions::new()
                    .write(true)
                    .custom_flags(libc::O_NONBLOCK)
                    .open(&fifo)
                {
                    break output;
                }
                assert!(
                    Instant::now() < limit,
                    "file request never reached its producer"
                );
                std::thread::sleep(Duration::from_millis(5));
            };
            while !marker.exists() && Instant::now() < limit {
                std::thread::sleep(Duration::from_millis(5));
            }
            let cancelled_first = marker.exists();
            output.write_all(b"late completion").unwrap();
            cancelled_first
        });
        run(&binary, directory.path(), "deadline recovered\n");
        assert!(
            producer.join().unwrap(),
            "cancellation waited for the file syscall"
        );
    }
}

#[test]
fn accepted_tcp_connection_composes_read_write_and_resource_cleanup() {
    use std::io::{Read, Write};
    use std::time::{Duration, Instant};

    require_codegen();
    for opt in ["0", "2"] {
        let directory = tempdir();
        let binary = build(
            r#"
import std.fs;
import std.net;
fn main() {
    match net.listen("127.0.0.1:0") {
        .Ok(listener) => {
            let port = listener.local_port();
            let _announced = fs.write("port", f"{port}");
            let conn = listener.accept();
            match conn.read_string() {
                .Ok(text) => println(text),
                .Err(_) => panic("read failed"),
            }
            match conn.write_string("native reply") {
                .Ok(_) => println("written"),
                .Err(_) => panic("write failed"),
            }
        },
        .Err(_) => panic("listen failed"),
    }
}
"#,
            directory.path(),
            opt,
        );
        let port_file = directory.path().join("port");
        let peer = std::thread::spawn(move || {
            let deadline = Instant::now() + Duration::from_secs(10);
            let port = loop {
                if let Ok(text) = std::fs::read_to_string(&port_file) {
                    if let Ok(port) = text.parse::<u16>() {
                        break port;
                    }
                }
                assert!(
                    Instant::now() < deadline,
                    "listener did not announce its port"
                );
                std::thread::sleep(Duration::from_millis(5));
            };
            let mut connection = std::net::TcpStream::connect(("127.0.0.1", port)).unwrap();
            connection
                .set_read_timeout(Some(Duration::from_secs(5)))
                .unwrap();
            connection.write_all(b"native request").unwrap();
            let mut reply = Vec::new();
            connection.read_to_end(&mut reply).unwrap();
            reply
        });
        run(&binary, directory.path(), "native request\nwritten\n");
        assert_eq!(peer.join().unwrap(), b"native reply");
    }
}
