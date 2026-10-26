mod support;

use std::io::{BufRead, Read, Write};
use std::process::{Command, Stdio};
use std::time::Duration;

fn eval(args: &[&str], input: &[u8]) -> std::process::Output {
    support::require_codegen();
    hew_testutil::run_command_bounded_with_stdin(
        Command::new(support::hew_binary()).arg("eval").args(args),
        "line evaluation",
        Duration::from_mins(3),
        input,
    )
    .unwrap()
}

#[test]
fn eval_lines_preserves_order_empty_lines_unicode_and_unterminated_input() {
    let output = eval(
        &["--lines", "println(f\"[{line}]\");"],
        "one\r\n\n雪\nlast".as_bytes(),
    );
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8(output.stdout)
            .unwrap()
            .replace("\r\n", "\n"),
        "[one]\n[]\n[雪]\n[last]\n"
    );
    let lossy = eval(&["--lines", "--json", "println(line);"], b"a\xff\n");
    assert!(
        lossy.status.success(),
        "{}",
        String::from_utf8_lossy(&lossy.stderr)
    );
    let value: serde_json::Value = serde_json::from_slice(&lossy.stdout).unwrap();
    assert_eq!(value["stdout"], "a\u{fffd}\n");
}

#[test]
fn eval_lines_streams_before_eof() {
    support::require_codegen();
    let mut command = Command::new(support::hew_binary());
    command
        .args(["eval", "--lines", "println(line);"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    hew_testutil::own_process_group(&mut command);
    let mut child = command.spawn().unwrap();
    let mut input = child.stdin.take().unwrap();
    input.write_all(b"first\n").unwrap();
    let mut stdout = std::io::BufReader::new(child.stdout.take().unwrap());
    let (send, receive) = std::sync::mpsc::channel();
    let reader = std::thread::spawn(move || {
        let mut first = String::new();
        let result = stdout.read_line(&mut first);
        let _ = send.send((result, first, stdout));
    });
    let first = receive.recv_timeout(Duration::from_mins(3));
    if first.is_err() {
        hew_testutil::terminate_process_group(&mut child, "line stream").unwrap();
        let _ = child.wait();
    }
    let (read, first, mut remaining) = first.expect("first output must arrive while input is open");
    read.unwrap();
    // Always close and reap before checking observations.
    input.write_all(b"second\n").unwrap();
    drop(input);
    let output = child.wait_with_output().unwrap();
    let mut rest = String::new();
    remaining.read_to_string(&mut rest).unwrap();
    reader.join().unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(first.replace("\r\n", "\n"), "first\n");
    assert_eq!(rest.replace("\r\n", "\n"), "second\n");
}

#[test]
fn eval_lines_diagnostics_use_the_authored_source() {
    let source = support::tempdir();
    let file = source.path().join("filter.hew");
    std::fs::write(&file, "println(line + missing_value);\n").unwrap();
    let output = eval(
        &["--lines", "--json", "-f", file.to_str().unwrap()],
        b"data\n",
    );
    assert!(!output.status.success());
    let value: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(value["status"], "compile_error");
    assert_eq!(value["stdout"], "");
    let diagnostics = value["diagnostics"].as_str().unwrap();
    assert!(diagnostics.contains("filter.hew:1:"), "{diagnostics}");
    assert!(diagnostics.contains("missing_value"), "{diagnostics}");
}

#[test]
fn eval_lines_empty_input_and_runtime_failure() {
    let empty = eval(&["--lines", "println(line);"], b"");
    assert!(empty.status.success());
    assert!(empty.stdout.is_empty());
    let failed = eval(
        &["--lines", "--json", "println(line); assert(false);"],
        b"seen\n",
    );
    assert!(!failed.status.success());
    let value: serde_json::Value = serde_json::from_slice(&failed.stdout).unwrap();
    assert_eq!(value["status"], "runtime_failure");
    assert_eq!(value["stdout"], "seen\n");
    assert_ne!(value["exit_code"], 0);
}

#[test]
fn eval_lines_exposes_wrapper_and_reserves_stdin_for_data() {
    let shown = eval(&["--lines", "--show-wrapper", "println(line);"], b"shown\n");
    assert!(
        shown.status.success(),
        "{}",
        String::from_utf8_lossy(&shown.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&shown.stdout).replace("\r\n", "\n"),
        "shown\n"
    );
    assert!(String::from_utf8_lossy(&shown.stderr).contains("fn main()"));
    let invalid = eval(&["--lines", "-f", "-"], b"data\n");
    assert!(!invalid.status.success());
    assert!(String::from_utf8_lossy(&invalid.stderr).contains("reserves stdin for data"));
}
