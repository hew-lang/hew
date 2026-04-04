//! Tests for the `BlockingCallInReceiveFn` warning.
//!
//! Actor receive functions run synchronously on scheduler worker threads.
//! Blocking operations inside them (`Receiver::recv`, `net.Connection::read`,
//! `net.Listener::accept`, `http.Server::accept`) can stall the thread and
//! prevent other actors from being scheduled, potentially deadlocking the
//! program.  The type-checker emits a `BlockingCallInReceiveFn` warning for
//! each such call site.

use std::path::{Path, PathBuf};

use hew_types::error::TypeErrorKind;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![
            repo_root(),
        ]));
    checker.check_program(&parse_result.program)
}

// ---------------------------------------------------------------------------
// Positive cases: warn when blocking ops appear inside receive fns
// ---------------------------------------------------------------------------

/// `Receiver::recv` inside a receive function triggers a warning.
#[test]
fn warn_receiver_recv_inside_receive_fn() {
    let output = typecheck(
        r"
        import std::channel;

        actor Worker {
            receive fn process(rx: channel.Receiver<String>) {
                let msg = rx.recv();
            }
        }

        fn main() {}
        ",
    );
    let blocking_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn)
        .collect();
    assert!(
        !blocking_warnings.is_empty(),
        "expected BlockingCallInReceiveFn warning for Receiver::recv, got warnings: {:#?}",
        output.warnings
    );
    assert!(
        blocking_warnings[0].message.contains("Receiver::recv"),
        "warning message should name the operation, got: {:?}",
        blocking_warnings[0].message
    );
}

/// `net.Connection::read` inside a receive function triggers a warning.
#[test]
fn warn_net_connection_read_inside_receive_fn() {
    let output = typecheck(
        r"
        import std::net;

        actor Networker {
            receive fn handle(conn: net.Connection) {
                let data = conn.read();
            }
        }

        fn main() {}
        ",
    );
    let blocking_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn)
        .collect();
    assert!(
        !blocking_warnings.is_empty(),
        "expected BlockingCallInReceiveFn warning for net.Connection::read, got: {:#?}",
        output.warnings
    );
    assert!(
        blocking_warnings[0]
            .message
            .contains("net.Connection::read"),
        "warning message should name the operation, got: {:?}",
        blocking_warnings[0].message
    );
}

/// `net.Listener::accept` inside a receive function triggers a warning.
#[test]
fn warn_net_listener_accept_inside_receive_fn() {
    let output = typecheck(
        r"
        import std::net;

        actor Server {
            receive fn serve(listener: net.Listener) {
                let conn = listener.accept();
            }
        }

        fn main() {}
        ",
    );
    let blocking_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn)
        .collect();
    assert!(
        !blocking_warnings.is_empty(),
        "expected BlockingCallInReceiveFn warning for net.Listener::accept, got: {:#?}",
        output.warnings
    );
    assert!(
        blocking_warnings[0]
            .message
            .contains("net.Listener::accept"),
        "warning message should name the operation, got: {:?}",
        blocking_warnings[0].message
    );
}

// ---------------------------------------------------------------------------
// Negative cases: no spurious warnings outside receive fns
// ---------------------------------------------------------------------------

/// `Receiver::recv` in a plain function must NOT trigger the warning.
#[test]
fn no_warn_receiver_recv_outside_actor() {
    let output = typecheck(
        r"
        import std::channel;

        fn process(rx: channel.Receiver<String>) -> Option<String> {
            rx.recv()
        }

        fn main() {}
        ",
    );
    let blocking_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn)
        .collect();
    assert!(
        blocking_warnings.is_empty(),
        "Receiver::recv outside receive fn must not produce BlockingCallInReceiveFn, got: {blocking_warnings:#?}",
    );
}

/// `Receiver::try_recv` (non-blocking) inside a receive function must NOT warn.
#[test]
fn no_warn_try_recv_inside_receive_fn() {
    let output = typecheck(
        r"
        import std::channel;

        actor Worker {
            receive fn poll(rx: channel.Receiver<String>) {
                let msg = rx.try_recv();
            }
        }

        fn main() {}
        ",
    );
    let blocking_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn)
        .collect();
    assert!(
        blocking_warnings.is_empty(),
        "try_recv is non-blocking and must not warn, got: {blocking_warnings:#?}",
    );
}

/// Multiple blocking calls in the same receive fn produce one warning each.
#[test]
fn multiple_blocking_calls_each_warned() {
    let output = typecheck(
        r"
        import std::channel;
        import std::net;

        actor Combo {
            receive fn handle(rx: channel.Receiver<String>, conn: net.Connection) {
                let msg = rx.recv();
                let data = conn.read();
            }
        }

        fn main() {}
        ",
    );
    let blocking_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn)
        .collect();
    assert_eq!(
        blocking_warnings.len(),
        2,
        "expected exactly 2 BlockingCallInReceiveFn warnings (one per call), got: {:#?}",
        output.warnings
    );
}

/// Warning message includes guidance about scheduler starvation and suggestions.
#[test]
fn warning_message_mentions_scheduler_with_suggestion() {
    let output = typecheck(
        r"
        import std::channel;

        actor Worker {
            receive fn process(rx: channel.Receiver<String>) {
                let _ = rx.recv();
            }
        }

        fn main() {}
        ",
    );
    let w = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn)
        .expect("expected a BlockingCallInReceiveFn warning");
    assert!(
        w.message.contains("scheduler"),
        "warning should mention 'scheduler', got: {:?}",
        w.message
    );
    assert!(
        !w.suggestions.is_empty(),
        "warning should carry at least one suggestion"
    );
}

/// `http.Server::accept` inside a receive function triggers a warning.
#[test]
fn warn_http_server_accept_inside_receive_fn() {
    let output = typecheck(
        r"
        import std::http;

        actor HttpHandler {
            receive fn serve(server: http.Server) {
                let req = server.accept();
            }
        }

        fn main() {}
        ",
    );
    let blocking_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn)
        .collect();
    assert!(
        !blocking_warnings.is_empty(),
        "expected BlockingCallInReceiveFn warning for http.Server::accept, got: {:#?}",
        output.warnings
    );
    assert!(
        blocking_warnings[0].message.contains("http.Server::accept"),
        "warning message should name the operation, got: {:?}",
        blocking_warnings[0].message
    );
}
