//! TCP source wrappers retain their checked ownership and exact native ABI.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

fn run_tcp(source: &str, expected: &str) {
    require_codegen();
    let dir = tempdir();
    let input = dir.path().join("tcp.hew");
    std::fs::write(&input, source).unwrap();
    for opt in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("tcp-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .args(["--opt-level", opt, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build native TCP O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let mut run = Command::new(binary);
        run.env("HEW_WORKERS", "1");
        let output = run_bounded_command(run, format!("run native TCP O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            expected,
            "{}",
            describe_output(&output)
        );
        assert!(output.stderr.is_empty(), "{}", describe_output(&output));
    }
}

#[test]
fn listener_borrows_preserve_ownership_and_both_close_paths_release_the_port() {
    run_tcp(
        r#"
import std.net;

fn inspect(listener: net.Listener) -> i64 { listener.local_port() }
fn open_and_release() -> i64 {
    match net.listen("127.0.0.1:0") {
        .Ok(listener) => {
            let port = inspect(listener);
            println(listener.local_port() == port);
            port
        }
        .Err(_) => panic("listen failed"),
    }
}
fn main() {
    let port = open_and_release();
    let address = f"127.0.0.1:{port}";
    match net.listen(address) {
        .Ok(listener) => { listener.close(); println("explicit close"); }
        .Err(_) => panic("implicit close retained the port"),
    }
    match net.listen(address) {
        .Ok(listener) => println(listener.local_port() == port),
        .Err(_) => panic("explicit close retained the port"),
    }
}
"#,
        "true\nexplicit close\ntrue\n",
    );
}

#[test]
fn listener_failure_cleanup_releases_the_port_and_invalid_addresses_are_values() {
    run_tcp(
        r#"
import std.net;

fn main() {
    var port = 0;
    scope {
        match net.listen("127.0.0.1:0") {
            .Ok(listener) => {
                port = listener.local_port();
                panic("close before recovery");
            }
            .Err(_) => panic("listen failed"),
        }
    } handle failure { println("recovered"); };
    match net.listen(f"127.0.0.1:{port}") {
        .Ok(listener) => println(listener.local_port() == port),
        .Err(_) => panic("fault cleanup retained the port"),
    }
    match net.listen("127.0.0.1:0\0ignored") {
        .Ok(_) => panic("embedded NUL was truncated"),
        .Err(_) => println("invalid address"),
    }
}
"#,
        "recovered\ntrue\ninvalid address\n",
    );
}
