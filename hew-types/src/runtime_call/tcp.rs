//! Exact source contracts for native TCP handles and synchronous operations.

use super::{
    runtime_semantic_contract, Deserialize, EnumIter, IntoEnumIterator, IoHandleKind, ResolvedTy,
    RuntimeArgumentContract, RuntimeArgumentEffect, RuntimeCallFamily, RuntimeResultEffect,
    RuntimeSemanticContract, RuntimeValueKind, Serialize,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, EnumIter, Default)]
pub enum TcpOp {
    #[default]
    Listen,
    ListenerValid,
    ConnectionValid,
    ListenerPort,
    ListenerClose,
    ConnectionClose,
    Write,
    ReadTimeout,
    WriteTimeout,
    Detach,
    BroadcastExcept,
}

impl TcpOp {
    #[must_use]
    pub const fn c_symbol(self) -> &'static str {
        match self {
            Self::Listen => "hew_tcp_listen",
            Self::ListenerValid => "hew_listener_is_valid",
            Self::ConnectionValid => "hew_connection_is_valid",
            Self::ListenerPort => "hew_tcp_listener_local_port",
            Self::ListenerClose => "hew_tcp_listener_close",
            Self::ConnectionClose => "hew_tcp_close",
            Self::Write => "hew_tcp_write",
            Self::ReadTimeout => "hew_tcp_set_read_timeout",
            Self::WriteTimeout => "hew_tcp_set_write_timeout",
            Self::Detach => "hew_tcp_detach",
            Self::BroadcastExcept => "hew_tcp_broadcast_except",
        }
    }

    #[must_use]
    pub fn from_c_symbol(symbol: &str) -> Option<Self> {
        Self::iter().find(|op| op.c_symbol() == symbol)
    }

    #[must_use]
    pub const fn is_release(self) -> bool {
        matches!(self, Self::ListenerClose | Self::ConnectionClose)
    }

    #[must_use]
    pub const fn contract(self) -> RuntimeSemanticContract {
        use RuntimeArgumentEffect::{Borrow, Move};
        use RuntimeResultEffect::{BitCopy, FreshOwned, Unit};
        use RuntimeValueKind::{Bool, Bytes, IoHandle, String, I32};
        const ADDRESS: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: String,
            effect: Borrow,
        };
        const INTEGER: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: I32,
            effect: Borrow,
        };
        const BYTES: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: Bytes,
            effect: Borrow,
        };
        const LISTENER: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::Listener),
            effect: Borrow,
        };
        const CONNECTION: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::Connection),
            effect: Borrow,
        };
        const CLOSE_LISTENER: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::Listener),
            effect: Move,
        };
        const CLOSE_CONNECTION: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: IoHandle(IoHandleKind::Connection),
            effect: Move,
        };
        match self {
            Self::Listen => runtime_semantic_contract(
                &[ADDRESS],
                FreshOwned(IoHandle(IoHandleKind::Listener)),
                &[],
            ),
            Self::ListenerValid => runtime_semantic_contract(&[LISTENER], BitCopy(Bool), &[]),
            Self::ConnectionValid => runtime_semantic_contract(&[CONNECTION], BitCopy(Bool), &[]),
            Self::ListenerPort => runtime_semantic_contract(&[LISTENER], BitCopy(I32), &[]),
            Self::ListenerClose => runtime_semantic_contract(&[CLOSE_LISTENER], BitCopy(I32), &[]),
            Self::ConnectionClose => {
                runtime_semantic_contract(&[CLOSE_CONNECTION], BitCopy(I32), &[])
            }
            Self::Write | Self::BroadcastExcept => {
                runtime_semantic_contract(&[CONNECTION, BYTES], BitCopy(I32), &[])
            }
            Self::ReadTimeout | Self::WriteTimeout => {
                runtime_semantic_contract(&[CONNECTION, INTEGER], BitCopy(I32), &[])
            }
            Self::Detach => runtime_semantic_contract(&[CONNECTION], Unit, &[]),
        }
    }
}

impl RuntimeCallFamily {
    #[must_use]
    pub fn matches_tcp_extern(
        self,
        module: &str,
        declaration: &str,
        symbol: &str,
        params: &[ResolvedTy],
        result: &ResolvedTy,
        consuming: &[bool],
    ) -> bool {
        matches!(self, Self::Tcp(_))
            && module == "std.net"
            && symbol == self.c_symbol()
            && declaration == format!("{module}.{symbol}")
            && self.semantic_contract().is_some_and(|contract| {
                contract.matches_signature(params, result)
                    && consuming.len() == contract.arguments.len()
                    && consuming
                        .iter()
                        .zip(contract.arguments)
                        .all(|(consume, argument)| {
                            *consume == (argument.effect == RuntimeArgumentEffect::Move)
                        })
            })
    }
}
