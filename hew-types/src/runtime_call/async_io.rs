//! Canonical source operations implemented by owned native I/O requests.

use crate::runtime_call::{
    runtime_semantic_contract, RuntimeArgumentContract, RuntimeArgumentEffect, RuntimeCallFamily,
    RuntimeResultEffect, RuntimeSemanticContract, RuntimeValueKind,
};
use crate::ResolvedTy;
use serde::{Deserialize, Serialize};
use strum::{EnumIter, IntoEnumIterator};

/// Source resource identity; the selected extern additionally proves provenance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IoHandleKind {
    Connection,
    Listener,
    TlsStream,
    ReadFile,
    WriteFile,
    WebSocketServer,
    WebSocketConnection,
}

impl IoHandleKind {
    #[must_use]
    pub fn matches(self, ty: &ResolvedTy) -> bool {
        let expected = match self {
            Self::Connection => "std.net.Connection",
            Self::Listener => "std.net.Listener",
            Self::TlsStream => "std.net.tls.TlsStream",
            Self::ReadFile => "std.fs.ReadFile",
            Self::WriteFile => "std.fs.WriteFile",
            Self::WebSocketServer => "std.net.websocket.Server",
            Self::WebSocketConnection => "std.net.websocket.Conn",
        };
        matches!(ty, ResolvedTy::Named { head, args, .. }
            if head.registry_key() == expected && args.is_empty())
    }

    #[must_use]
    pub fn of_ty(ty: &ResolvedTy) -> Option<Self> {
        [
            Self::Connection,
            Self::Listener,
            Self::TlsStream,
            Self::ReadFile,
            Self::WriteFile,
            Self::WebSocketServer,
            Self::WebSocketConnection,
        ]
        .into_iter()
        .find(|kind| kind.matches(ty))
    }
}

/// How an owned operation result becomes the existing source extern result.
/// Restore the operation's error channel on the resume thread before allowing
/// the ordinary source wrapper to run. Cancellation follows task cleanup.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsyncIoResume {
    Unit,
    /// Transfer bytes on success; return empty bytes on ordinary I/O failure.
    Bytes,
    /// Return the successful byte count; return -1 on failure.
    WriteCount,
    /// Transfer an accepted connection; return the invalid handle on failure.
    Connection,
    /// The native library restores its diagnostics and writes the source result.
    Library(&'static str),
}

/// How long a native request needs a borrowed source argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsyncIoLoan {
    /// Submission copies the value into request-owned storage.
    UntilSubmitReturns,
    /// Keep the source resource alive until completion or cancellation detaches.
    UntilQuiescent,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    EnumIter,
    Default,
)]
#[cfg_attr(feature = "sandbox-schema", derive(schemars::JsonSchema, ts_rs::TS))]
pub enum AsyncIoOp {
    #[default]
    TcpRead,
    TcpWrite,
    TcpAccept,
    TcpConnect,
    TcpConnectTimeout,
    StdinReadLine,
    SinkFinish,
    TlsConnect,
    TlsRead,
    TlsWrite,
    FileRead,
    FileWrite,
    WebSocketAccept,
    WebSocketSendText,
    WebSocketSendBinary,
}

impl AsyncIoOp {
    #[must_use]
    pub const fn authority(self) -> crate::runtime_call::RuntimeAuthority {
        match self {
            Self::StdinReadLine | Self::TcpConnect | Self::TcpConnectTimeout | Self::TlsConnect => {
                crate::runtime_call::RuntimeAuthority::Ambient
            }
            Self::TcpRead
            | Self::TcpWrite
            | Self::TcpAccept
            | Self::SinkFinish
            | Self::TlsRead
            | Self::TlsWrite
            | Self::FileRead
            | Self::FileWrite
            | Self::WebSocketAccept
            | Self::WebSocketSendText
            | Self::WebSocketSendBinary => crate::runtime_call::RuntimeAuthority::Service,
        }
    }

    #[must_use]
    pub const fn argument_loan(self) -> AsyncIoLoan {
        match self {
            Self::TcpConnect
            | Self::TcpConnectTimeout
            | Self::StdinReadLine
            | Self::TlsConnect
            | Self::TlsRead
            | Self::TlsWrite
            | Self::WebSocketAccept
            | Self::WebSocketSendText
            | Self::WebSocketSendBinary => AsyncIoLoan::UntilSubmitReturns,
            Self::TcpRead
            | Self::TcpWrite
            | Self::TcpAccept
            | Self::SinkFinish
            | Self::FileRead
            | Self::FileWrite => AsyncIoLoan::UntilQuiescent,
        }
    }
    #[must_use]
    pub const fn c_symbol(self) -> &'static str {
        match self {
            Self::TlsConnect => "hew_tls_connect",
            Self::TlsRead => "hew_tls_read_result",
            Self::TlsWrite => "hew_tls_write_result",
            Self::FileRead => "hew_file_read_remaining",
            Self::FileWrite => "hew_file_write_handle",
            Self::SinkFinish => "hew_sink_finish",
            Self::TcpRead => "hew_tcp_read",
            Self::TcpWrite => "hew_tcp_write",
            Self::TcpAccept => "hew_tcp_accept",
            Self::TcpConnect => "hew_tcp_connect",
            Self::TcpConnectTimeout => "hew_tcp_connect_timeout",
            Self::StdinReadLine => "hew_stdin_read_line",
            Self::WebSocketAccept => "hew_ws_server_accept",
            Self::WebSocketSendText => "hew_ws_send_text",
            Self::WebSocketSendBinary => "hew_ws_send_binary",
        }
    }

    /// Compiler-private submission entry. Its inputs are borrowed for submit;
    /// connect inputs are copied by the operation, socket loans last to quiescence.
    #[must_use]
    pub const fn submit_symbol(self) -> &'static str {
        match self {
            Self::TlsConnect => "hew_async_tls_connect",
            Self::TlsRead => "hew_async_tls_read",
            Self::TlsWrite => "hew_async_tls_write",
            Self::FileRead => "hew_async_file_read",
            Self::FileWrite => "hew_async_file_write",
            Self::SinkFinish => "hew_async_sink_finish",
            Self::TcpRead => "hew_async_tcp_read",
            Self::TcpWrite => "hew_async_tcp_write",
            Self::TcpAccept => "hew_async_tcp_accept",
            Self::TcpConnect => "hew_async_tcp_connect",
            Self::TcpConnectTimeout => "hew_async_tcp_connect_timeout",
            Self::StdinReadLine => "hew_async_stdin_read_line",
            Self::WebSocketAccept => "hew_async_ws_server_accept",
            Self::WebSocketSendText => "hew_async_ws_send_text",
            Self::WebSocketSendBinary => "hew_async_ws_send_binary",
        }
    }

    #[must_use]
    pub const fn resume(self) -> AsyncIoResume {
        match self {
            Self::TcpRead | Self::StdinReadLine | Self::FileRead => AsyncIoResume::Bytes,
            Self::TcpAccept | Self::TcpConnect | Self::TcpConnectTimeout => {
                AsyncIoResume::Connection
            }
            Self::TcpWrite | Self::FileWrite => AsyncIoResume::WriteCount,
            Self::SinkFinish => AsyncIoResume::Unit,
            Self::TlsConnect => AsyncIoResume::Library("hew_async_tls_take_connect"),
            Self::TlsRead => AsyncIoResume::Library("hew_async_tls_take_read"),
            Self::TlsWrite => AsyncIoResume::Library("hew_async_tls_take_write"),
            Self::WebSocketAccept => AsyncIoResume::Library("hew_async_ws_take_accept"),
            Self::WebSocketSendText | Self::WebSocketSendBinary => {
                AsyncIoResume::Library("hew_async_ws_take_send")
            }
        }
    }

    #[must_use]
    pub fn from_c_symbol(symbol: &str) -> Option<Self> {
        Self::iter().find(|op| op.c_symbol() == symbol)
    }

    #[must_use]
    #[expect(
        clippy::too_many_lines,
        reason = "one authoritative typed contract per native I/O operation"
    )]
    pub const fn contract(self) -> RuntimeSemanticContract {
        use RuntimeResultEffect::{BitCopy, FreshOwned};
        use RuntimeValueKind::{Bytes, IoHandle, String, I32};
        const INTEGER: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: I32,
            effect: RuntimeArgumentEffect::Borrow,
        };
        const PATH: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: String,
            effect: RuntimeArgumentEffect::Borrow,
        };
        const DATA: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: Bytes,
            effect: RuntimeArgumentEffect::Borrow,
        };
        const CONNECTION: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::Connection),
            effect: RuntimeArgumentEffect::Borrow,
        };
        const LISTENER: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::Listener),
            effect: RuntimeArgumentEffect::Borrow,
        };
        const TLS: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::TlsStream),
            effect: RuntimeArgumentEffect::Borrow,
        };
        const WEBSOCKET: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::WebSocketConnection),
            effect: RuntimeArgumentEffect::Borrow,
        };
        const READ_FILE: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::ReadFile),
            effect: RuntimeArgumentEffect::Borrow,
        };
        const WRITE_FILE: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::WriteFile),
            effect: RuntimeArgumentEffect::Borrow,
        };
        const TIMEOUT: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::I64,
            effect: RuntimeArgumentEffect::Borrow,
        };
        // I/O errors belong to the source wrapper's Result/error channel,
        // not the logical-fault set on RuntimeSemanticContract.
        match self {
            Self::WebSocketSendText => {
                runtime_semantic_contract(&[WEBSOCKET, PATH], BitCopy(I32), &[])
            }
            Self::WebSocketSendBinary => {
                runtime_semantic_contract(&[WEBSOCKET, DATA], BitCopy(I32), &[])
            }
            Self::WebSocketAccept => runtime_semantic_contract(
                &[RuntimeArgumentContract {
                    ty: IoHandle(IoHandleKind::WebSocketServer),
                    effect: RuntimeArgumentEffect::Borrow,
                }],
                FreshOwned(IoHandle(IoHandleKind::WebSocketConnection)),
                &[],
            ),
            Self::FileRead => runtime_semantic_contract(&[READ_FILE], FreshOwned(Bytes), &[]),
            Self::FileWrite => runtime_semantic_contract(&[WRITE_FILE, DATA], BitCopy(I32), &[]),
            Self::TlsConnect => runtime_semantic_contract(
                &[PATH, INTEGER, INTEGER, DATA, TIMEOUT, TIMEOUT],
                FreshOwned(TLS.ty),
                &[],
            ),
            Self::TlsRead => runtime_semantic_contract(
                &[TLS, INTEGER],
                FreshOwned(RuntimeValueKind::Named("std.net.tls.TlsReadFfiResult")),
                &[],
            ),
            Self::TlsWrite => runtime_semantic_contract(
                &[TLS, DATA],
                BitCopy(RuntimeValueKind::Named("std.net.tls.TlsWriteFfiResult")),
                &[],
            ),
            Self::SinkFinish => runtime_semantic_contract(
                &[RuntimeArgumentContract {
                    ty: RuntimeValueKind::PipeHalf(crate::runtime_call::PipeHalfKind::Sink),
                    effect: RuntimeArgumentEffect::Borrow,
                }],
                RuntimeResultEffect::Unit,
                &[],
            ),
            Self::TcpRead => runtime_semantic_contract(&[CONNECTION], FreshOwned(Bytes), &[]),
            Self::StdinReadLine => runtime_semantic_contract(&[], FreshOwned(Bytes), &[]),
            Self::TcpConnect => runtime_semantic_contract(
                &[PATH],
                FreshOwned(IoHandle(IoHandleKind::Connection)),
                &[],
            ),
            Self::TcpConnectTimeout => runtime_semantic_contract(
                &[PATH, INTEGER, INTEGER],
                FreshOwned(IoHandle(IoHandleKind::Connection)),
                &[],
            ),
            Self::TcpWrite => runtime_semantic_contract(&[CONNECTION, DATA], BitCopy(I32), &[]),
            Self::TcpAccept => runtime_semantic_contract(
                &[LISTENER],
                FreshOwned(IoHandle(IoHandleKind::Connection)),
                &[],
            ),
        }
    }
}

impl RuntimeCallFamily {
    /// The checker must first establish canonical module source provenance.
    #[expect(
        clippy::too_many_arguments,
        reason = "each part of the extern declaration is matched independently"
    )]
    #[must_use]
    pub fn matches_async_io_extern(
        self,
        defs: &crate::DefTable,
        module: &str,
        declaration: &str,
        symbol: &str,
        params: &[ResolvedTy],
        result: &ResolvedTy,
        consuming: &[bool],
    ) -> bool {
        let Self::AsyncIo(op) = self else {
            return false;
        };
        let owner = match op {
            AsyncIoOp::TcpRead
            | AsyncIoOp::TcpWrite
            | AsyncIoOp::TcpAccept
            | AsyncIoOp::TcpConnect
            | AsyncIoOp::TcpConnectTimeout => "std.net",
            AsyncIoOp::StdinReadLine => "std.io",
            AsyncIoOp::SinkFinish => "std.stream",
            AsyncIoOp::TlsConnect | AsyncIoOp::TlsRead | AsyncIoOp::TlsWrite => "std.net.tls",
            AsyncIoOp::FileRead | AsyncIoOp::FileWrite => "std.fs",
            AsyncIoOp::WebSocketAccept
            | AsyncIoOp::WebSocketSendText
            | AsyncIoOp::WebSocketSendBinary => "std.net.websocket",
        };
        module == owner
            && symbol == op.c_symbol()
            && declaration == format!("{owner}.{symbol}")
            && op.contract().matches_signature(defs, params, result)
            && consuming.len() == params.len()
            && consuming.iter().all(|consumes| !consumes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn handle(name: &str) -> ResolvedTy {
        ResolvedTy::opaque_for_test(name, vec![])
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "all extern cases share one provenance and borrowing oracle"
    )]
    fn native_io_externs_require_exact_owner_signature_and_borrow_contract() {
        let connection = handle("std.net.Connection");
        let listener = handle("std.net.Listener");
        for (op, module, params, result) in [
            (
                AsyncIoOp::TcpRead,
                "std.net",
                vec![connection.clone()],
                ResolvedTy::Bytes,
            ),
            (
                AsyncIoOp::TcpWrite,
                "std.net",
                vec![connection.clone(), ResolvedTy::Bytes],
                ResolvedTy::I32,
            ),
            (AsyncIoOp::TcpAccept, "std.net", vec![listener], connection),
            (
                AsyncIoOp::WebSocketAccept,
                "std.net.websocket",
                vec![handle("std.net.websocket.Server")],
                handle("std.net.websocket.Conn"),
            ),
            (
                AsyncIoOp::WebSocketSendText,
                "std.net.websocket",
                vec![handle("std.net.websocket.Conn"), ResolvedTy::String],
                ResolvedTy::I32,
            ),
            (
                AsyncIoOp::WebSocketSendBinary,
                "std.net.websocket",
                vec![handle("std.net.websocket.Conn"), ResolvedTy::Bytes],
                ResolvedTy::I32,
            ),
            (
                AsyncIoOp::StdinReadLine,
                "std.io",
                vec![],
                ResolvedTy::Bytes,
            ),
        ] {
            let family = RuntimeCallFamily::AsyncIo(op);
            let declaration = format!("{module}.{}", op.c_symbol());
            let borrowed = vec![false; params.len()];
            assert!(
                family.matches_async_io_extern(
                    &crate::DefTable::new(),
                    module,
                    &declaration,
                    op.c_symbol(),
                    &params,
                    &result,
                    &borrowed
                ),
                "{op:?}"
            );
            assert!(!family.matches_async_io_extern(
                &crate::DefTable::new(),
                "user.net",
                &declaration,
                op.c_symbol(),
                &params,
                &result,
                &borrowed
            ));
            assert!(!family.matches_async_io_extern(
                &crate::DefTable::new(),
                module,
                "other.declaration",
                op.c_symbol(),
                &params,
                &result,
                &borrowed
            ));
            assert!(!family.matches_async_io_extern(
                &crate::DefTable::new(),
                module,
                &declaration,
                op.c_symbol(),
                &params,
                &ResolvedTy::Unit,
                &borrowed
            ));
            assert!(
                params.is_empty()
                    || !family.matches_async_io_extern(
                        &crate::DefTable::new(),
                        module,
                        &declaration,
                        op.c_symbol(),
                        &params,
                        &result,
                        &vec![true; params.len()]
                    )
            );
        }
        let forged = handle("user.Connection");
        assert!(!AsyncIoOp::TcpRead.contract().matches_signature(
            &crate::DefTable::new(),
            &[forged],
            &ResolvedTy::Bytes
        ));
    }

    #[test]
    fn accept_retains_the_listener_loan_and_returns_an_independent_connection() {
        let listener = handle("std.net.Listener");
        let connection = handle("std.net.Connection");
        let contract = AsyncIoOp::TcpAccept.contract();
        let instantiated = contract
            .instantiate(
                &crate::DefTable::new(),
                std::slice::from_ref(&listener),
                &connection,
            )
            .unwrap();
        assert_eq!(instantiated.result_ty, connection);
        assert!(!contract.matches_signature(
            &crate::DefTable::new(),
            std::slice::from_ref(&listener),
            &listener
        ));
        assert_eq!(contract.arguments[0].effect, RuntimeArgumentEffect::Borrow);
        assert_eq!(
            AsyncIoOp::TcpAccept.argument_loan(),
            AsyncIoLoan::UntilQuiescent
        );
        assert_eq!(
            AsyncIoOp::TcpConnect.argument_loan(),
            AsyncIoLoan::UntilSubmitReturns
        );
    }
}
