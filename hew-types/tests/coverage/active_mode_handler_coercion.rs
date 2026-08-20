//! Coercion soundness for the active-mode handler surface.
//!
//! `LocalPid<Actor>` narrows to `LocalPid<HandlerTrait>` (the `conn.attach(this)`
//! surface) only when the actor's `receive fn`s structurally satisfy the handler
//! trait. An explicit `impl HandlerTrait for Actor {}` with no matching
//! `receive fn`s must NOT admit the coercion: `attach` codegen synthesises the
//! handler `msg_id`s from the actor's receive-fn protocol descriptor, so a
//! receive-fn-less actor carries nothing codegen can lower. Admitting it would
//! defer the failure to a late `E_CODEGEN`; the checker must reject it early
//! with an honest type error.

use crate::common;

use common::typecheck;
use hew_types::MethodCallRewrite;

fn has_rewrite(output: &hew_types::TypeCheckOutput, symbol: &str) -> bool {
    output.method_call_rewrites.values().any(
        |rewrite| matches!(rewrite, MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == symbol),
    )
}

/// Positive: an actor whose `receive fn`s match the handler trait's methods
/// coerces cleanly to `LocalPid<Handler>`.
#[test]
fn actor_with_matching_receive_fns_coerces_to_handler_pid() {
    let output = typecheck(
        r"
        trait Handler {
            fn on_data(data: bytes);
            fn on_close();
        }

        actor Echo {
            let n: i32;
            init() {}
            receive fn on_data(data: bytes) {}
            receive fn on_close() {}
        }

        fn use_handler(h: LocalPid<Handler>) {}

        fn main() {
            let echo = spawn Echo(n: 0);
            use_handler(echo);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "an actor whose receive fns satisfy the handler trait must coerce \
         cleanly to LocalPid<Handler>: {:#?}",
        output.errors
    );
}

/// Negative (CS-1): an actor with an explicit `impl Handler for X {}` but NO
/// matching `receive fn`s must be rejected at the coercion site, NOT admitted
/// to fail later in codegen. The handler trait's only lowerable satisfaction is
/// the structural receive-fn path; an explicit impl is not enough.
#[test]
fn explicit_handler_impl_without_receive_fns_is_rejected_early() {
    let output = typecheck(
        r"
        trait Handler {
            fn on_data(data: bytes);
            fn on_close();
        }

        actor Bare {
            let n: i32;
            init() {}
        }

        impl Handler for Bare {
            fn on_data(data: bytes) {}
            fn on_close() {}
        }

        fn use_handler(h: LocalPid<Handler>) {}

        fn main() {
            let bare = spawn Bare(n: 0);
            use_handler(bare);
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "an actor with an explicit handler impl but no matching receive fns \
         must be rejected at the coercion site (it cannot be lowered), not \
         admitted to fail late in codegen"
    );
    assert!(
        output.errors.iter().any(|error| {
            let m = error.message.to_lowercase();
            m.contains("handler") || m.contains("localpid") || m.contains("expected")
        }),
        "the rejection must be an honest handler-pid type mismatch, not an \
         unrelated error: {:#?}",
        output.errors
    );
}

/// A user type may legally share a stdlib transport's short name. Bare
/// `Connection`, `TlsStream`, and `Conn` receivers must keep their own
/// `Type::attach` dispatch rather than being hijacked by a runtime attach
/// pseudo-symbol that expects an opaque handle and actor PID.
#[test]
fn user_transport_short_names_keep_user_attach_dispatch() {
    for (module_import, type_name) in [
        ("import std.net;", "Connection"),
        ("import std.net.tls;", "TlsStream"),
        ("import std.net.websocket;", "Conn"),
    ] {
        let output = typecheck(&format!(
            r"
            {module_import}

            type {type_name} {{ value: i64; }}

            impl {type_name} {{
                fn attach(self, increment: i64) -> i64 {{
                    self.value + increment
                }}
            }}

            fn invoke(value: {type_name}) -> i64 {{
                value.attach(1)
            }}
            "
        ));
        assert!(
            output.errors.is_empty(),
            "user-defined {type_name}::attach must typecheck: {:#?}",
            output.errors
        );
        assert!(
            has_rewrite(&output, &format!("{type_name}::attach")),
            "user-defined {type_name}::attach must retain user dispatch: {:#?}",
            output.method_call_rewrites
        );
        for forbidden in [
            "hew_tcp_attach_local",
            "hew_tls_attach_local",
            "hew_ws_attach_local",
        ] {
            assert!(
                !has_rewrite(&output, forbidden),
                "bare user {type_name} must not rewrite to {forbidden}: {:#?}",
                output.method_call_rewrites
            );
        }
    }
}
