//! Coercion soundness for the active-mode handler surface.
//!
//! `Actor` narrows to `HandlerTrait` (the `conn.attach(this)`
//! surface) only when the actor's `receive fn`s structurally satisfy the handler
//! trait. An explicit `impl HandlerTrait for Actor {}` with no matching
//! `receive fn`s must not admit the coercion: runtime invocation facts select
//! concrete receive declarations and protocol IDs before lowering constructs
//! native callbacks. An actor without that protocol must be rejected by the
//! checker.

use crate::common;

use common::typecheck;
use hew_types::MethodCallRewrite;

fn has_rewrite(output: &hew_types::TypeCheckOutput, symbol: &str) -> bool {
    output.method_call_rewrites.values().any(
        |rewrite| matches!(rewrite, MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == symbol),
    )
}

/// Positive: an actor whose `receive fn`s match the handler trait's methods
/// coerces cleanly to `Handler`.
#[test]
fn actor_with_matching_receive_fns_coerces_to_handler_pid() {
    let output = typecheck(
        r"
        trait Handler {
            fn on_data(data: bytes);
            fn on_close();
        }

        actor Echo {
            let n: i32,
            init() {}
            receive fn on_data(data: bytes) {}
            receive fn on_close() {}
        }

        fn use_handler(h: Handler) {}

        fn main() {
            let echo = spawn Echo(n: 0);
            use_handler(echo);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "an actor whose receive fns satisfy the handler trait must coerce \
         cleanly to Handler: {:#?}",
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
            let n: i32,
            init() {}
        }

        impl Handler for Bare {
            fn on_data(data: bytes) {}
            fn on_close() {}
        }

        fn use_handler(h: Handler) {}

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
            m.contains("handler") || m.contains("expected")
        }),
        "the rejection must be an honest handler type mismatch, not an \
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

            type {type_name} {{ value: i64, }}

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

/// Protocol selection happens against concrete receive declarations, before
/// the handler trait can erase the actor identity at a runtime boundary.
#[test]
fn declared_transport_methods_carry_concrete_receive_endpoints() {
    use hew_types::check::dispatch::ResolvedRuntimeResult;
    use hew_types::{CallTarget, RuntimeCallFamily};
    for (module, receiver, data_handler, payload, family, consumes) in [
        (
            "net",
            "Connection",
            "on_data",
            "bytes",
            RuntimeCallFamily::TcpAttachLocal,
            true,
        ),
        (
            "net.tls",
            "TlsStream",
            "on_data",
            "bytes",
            RuntimeCallFamily::TlsAttachLocal,
            false,
        ),
        (
            "net.websocket",
            "Conn",
            "on_message",
            "string",
            RuntimeCallFamily::WebSocketAttachLocal,
            false,
        ),
    ] {
        let alias = module.rsplit('.').next().unwrap();
        let parameter = if consumes {
            "consume connection"
        } else {
            "connection"
        };
        let source = format!(
            r"
            import std.{module};
            actor Handler {{
                receive fn unrelated() {{}}
                receive fn on_close() {{}}
                receive fn {data_handler}(value: {payload}) {{}}
            }}
            fn install({parameter}: {alias}.{receiver}) {{
                let handler = spawn Handler();
                connection.attach(handler);
            }}
        "
        );
        let output = typecheck(&source);
        assert!(output.errors.is_empty(), "{module}: {:#?}", output.errors);
        let (selected, endpoints, adaptation, receiver_consumed) = output
            .method_call_rewrites
            .values()
            .find_map(|rewrite| match rewrite {
                MethodCallRewrite::RewriteToFunction {
                    target:
                        CallTarget::DeclaredRuntime {
                            family,
                            actor_endpoints: Some(endpoints),
                            result,
                            ..
                        },
                    consumes_receiver,
                    ..
                } => Some((family, endpoints, result, consumes_receiver)),
                _ => None,
            })
            .expect("attach must carry a declaration-owned runtime invocation");
        assert_eq!(*selected, family);
        assert_eq!(*receiver_consumed, consumes);
        assert_eq!(endpoints.actor.full_path(), "Handler");
        assert_eq!(
            endpoints.data.handler.full_path(),
            format!("Handler::{data_handler}")
        );
        assert_eq!(endpoints.close.handler.full_path(), "Handler::on_close");
        let protocol = &output.actor_protocol_descriptors["Handler"];
        assert_eq!(
            Some(endpoints.data.msg_id),
            protocol.msg_id_for(data_handler)
        );
        assert_eq!(
            Some(endpoints.close.msg_id),
            protocol.msg_id_for("on_close")
        );
        assert_eq!(
            matches!(adaptation, ResolvedRuntimeResult::StatusResult { .. }),
            consumes
        );
    }
}

#[test]
fn runtime_handler_must_have_a_concrete_receive_protocol() {
    let output = typecheck(
        r"
        import std.net;
        fn install(consume connection: net.Connection, handler: net.ConnectionHandler) {
            connection.attach(handler);
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("receive protocol")),
        "{:#?}",
        output.errors
    );
}
