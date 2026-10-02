//! Tests for the `BlockingCallInReceiveFn` warning.
//!
//! Actor receive functions run synchronously on scheduler worker threads.
//! Blocking operations inside them (`http.Server.accept`) can stall the thread and
//! prevent other actors from being scheduled, potentially deadlocking the
//! program.  The type-checker emits a `BlockingCallInReceiveFn` warning for
//! each such call site.

use crate::common;

use common::{typecheck, warnings_of_kind};
use hew_types::error::TypeErrorKind;

// ---------------------------------------------------------------------------
// Positive cases: warn when blocking ops appear inside receive fns
// ---------------------------------------------------------------------------

fn assert_single_blocking_warning(output: &hew_types::TypeCheckOutput, operation: &str) {
    let blocking_warnings = warnings_of_kind(output, &TypeErrorKind::BlockingCallInReceiveFn);
    assert_eq!(
        blocking_warnings.len(),
        1,
        "expected exactly one BlockingCallInReceiveFn warning, got: {:#?}",
        output.warnings
    );
    assert!(
        blocking_warnings[0].message.contains(operation),
        "warning message should name the operation, got: {:?}",
        blocking_warnings[0].message
    );
}

/// Reactor-backed TCP receive calls suspend without blocking a scheduler worker.
#[test]
fn no_warn_net_connection_recv_inside_receive_fn() {
    let output = typecheck(
        r"
        import std.net;

        actor Networker {
            receive fn handle(conn: net.Connection) {
                let data = conn.recv();
            }
        }

        fn main() {}
        ",
    );
    let blocking_warnings = warnings_of_kind(&output, &TypeErrorKind::BlockingCallInReceiveFn);
    assert!(
        blocking_warnings.is_empty(),
        "reactor-backed Connection.recv must not produce a blocking warning: {blocking_warnings:#?}"
    );
}

/// Reactor-backed listener acceptance suspends without blocking a scheduler worker.
#[test]
fn no_warn_net_listener_accept_inside_receive_fn() {
    let output = typecheck(
        r"
        import std.net;

        actor Server {
            receive fn serve(listener: net.Listener) {
                let conn = listener.accept();
            }
        }

        fn main() {}
        ",
    );
    let blocking_warnings = warnings_of_kind(&output, &TypeErrorKind::BlockingCallInReceiveFn);
    assert!(
        blocking_warnings.is_empty(),
        "reactor-backed Listener.accept must not produce a blocking warning: {blocking_warnings:#?}"
    );
}

// ---------------------------------------------------------------------------
// Negative cases: no spurious warnings outside receive fns
// ---------------------------------------------------------------------------

/// `net.Connection.read` in a plain function must NOT trigger the warning.
#[test]
fn no_warn_connection_read_outside_actor() {
    let output = typecheck(
        r"
        import std.net;

        fn process(conn: net.Connection) -> bytes {
            conn.read()
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
        "Connection.read outside receive fn must not produce BlockingCallInReceiveFn, got: {blocking_warnings:#?}",
    );
}

/// Configuring a connection timeout inside a receive function must NOT warn.
#[test]
fn no_warn_connection_timeout_inside_receive_fn() {
    let output = typecheck(
        r"
        import std.net;

        actor Worker {
            receive fn configure(conn: net.Connection) {
                let _result = conn.set_read_timeout(100);
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
        "set_read_timeout is non-blocking and must not warn, got: {blocking_warnings:#?}",
    );
}

// Several reactor-backed calls in one receive function stay warning-free.
#[test]
fn multiple_reactor_calls_do_not_warn() {
    let output = typecheck(
        r"
        import std.net;

        actor Combo {
            receive fn handle(listener: net.Listener, conn: net.Connection) {
                let accepted = listener.accept();
                let data = conn.recv();
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
        "reactor-backed network calls must not produce blocking warnings: {blocking_warnings:#?}"
    );
}

/// Warning text explains that another actor or task is not blocking isolation.
#[test]
fn warning_message_mentions_scheduler_with_suggestion() {
    let output = typecheck(
        r"
        import std.net.http;

        actor Worker {
            receive fn process(server: http.Server) {
                let _ = server.accept();
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
    assert!(
        w.suggestions
            .iter()
            .any(|suggestion| suggestion.contains("another actor")),
        "warning should explain that another actor does not isolate blocking work: {:?}",
        w.suggestions
    );
}

/// `http.Server.accept` inside a receive function triggers a warning.
#[test]
fn warn_http_server_accept_inside_receive_fn() {
    let output = typecheck(
        r"
        import std.net.http;

        actor HttpHandler {
            receive fn serve(server: http.Server) {
                let req = server.accept();
            }
        }

        fn main() {}
        ",
    );
    assert_single_blocking_warning(&output, "http.Server.accept");
}

// ---------------------------------------------------------------------------
// Suggestion text: no blocking op has a drop-in suspending spelling
// ---------------------------------------------------------------------------

/// The genuinely blocking HTTP accept warning must not suggest an `await` form
/// that its API does not support.
#[test]
fn no_blocking_suggestion_names_await() {
    let output = typecheck(
        r"
        import std.net.http;

        actor Mixed {
            receive fn serve(server: http.Server) {
                let accepted = server.accept();
            }
        }

        fn main() {}
        ",
    );
    let blocking_warnings = warnings_of_kind(&output, &TypeErrorKind::BlockingCallInReceiveFn);
    assert_eq!(
        blocking_warnings.len(),
        1,
        "expected one warning for the blocking HTTP accept, got: {:#?}",
        output.warnings
    );
    for w in blocking_warnings {
        assert!(
            !w.suggestions.iter().any(|s| s.contains("await")),
            "no blocking suggestion may name an await form, got: {:?}",
            w.suggestions
        );
        assert!(
            !w.suggestions.is_empty(),
            "warning should carry at least one suggestion"
        );
    }
}
