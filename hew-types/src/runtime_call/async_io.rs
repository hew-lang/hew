//! Canonical source operations implemented by owned native I/O requests.

use super::{
    runtime_semantic_contract, Deserialize, EnumIter, IntoEnumIterator, ResolvedTy,
    RuntimeArgumentContract, RuntimeArgumentEffect, RuntimeCallFamily, RuntimeResultEffect,
    RuntimeSemanticContract, RuntimeValueKind, Serialize,
};

/// Source resource identity; the selected extern additionally proves provenance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IoHandleKind {
    Connection,
    Listener,
}

impl IoHandleKind {
    #[must_use]
    pub fn matches(self, ty: &ResolvedTy) -> bool {
        let expected = match self {
            Self::Connection => "std.net.Connection",
            Self::Listener => "std.net.Listener",
        };
        matches!(ty, ResolvedTy::Named { name, args, builtin: None, .. }
            if name == expected && args.is_empty())
    }

    #[must_use]
    pub fn of_ty(ty: &ResolvedTy) -> Option<Self> {
        [Self::Connection, Self::Listener]
            .into_iter()
            .find(|kind| kind.matches(ty))
    }
}

/// How an owned operation result becomes the existing source extern result.
/// Restore the operation's error channel on the resume thread before allowing
/// the ordinary source wrapper to run. Cancellation follows task cleanup.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsyncIoResume {
    /// Transfer bytes on success; return empty bytes on ordinary I/O failure.
    Bytes,
    /// Discard the successful byte count and return 0; return -1 on failure.
    WriteStatus,
    /// Transfer an accepted connection; return the invalid handle on failure.
    Connection,
}

/// How long a native request needs a borrowed source argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsyncIoLoan {
    /// Submission copies the value into request-owned storage.
    UntilSubmitReturns,
    /// Keep the source resource alive until completion or cancellation detaches.
    UntilQuiescent,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, EnumIter, Default)]
pub enum AsyncIoOp {
    #[default]
    FileReadBytes,
    FileWriteString,
    FileWriteBytes,
    TcpRead,
    TcpAccept,
}

impl AsyncIoOp {
    #[must_use]
    pub const fn argument_loan(self) -> AsyncIoLoan {
        match self {
            Self::FileReadBytes | Self::FileWriteString | Self::FileWriteBytes => {
                AsyncIoLoan::UntilSubmitReturns
            }
            Self::TcpRead | Self::TcpAccept => AsyncIoLoan::UntilQuiescent,
        }
    }
    #[must_use]
    pub const fn c_symbol(self) -> &'static str {
        match self {
            Self::FileReadBytes => "hew_file_read_bytes",
            Self::FileWriteString => "hew_file_write",
            Self::FileWriteBytes => "hew_file_write_bytes",
            Self::TcpRead => "hew_tcp_read",
            Self::TcpAccept => "hew_tcp_accept",
        }
    }

    /// Compiler-private submission entry. Its inputs are borrowed for submit;
    /// file inputs are copied by the operation, socket loans last to quiescence.
    #[must_use]
    pub const fn submit_symbol(self) -> &'static str {
        match self {
            Self::FileReadBytes => "hew_async_file_read",
            Self::FileWriteString => "hew_async_file_write_string",
            Self::FileWriteBytes => "hew_async_file_write",
            Self::TcpRead => "hew_async_tcp_read",
            Self::TcpAccept => "hew_async_tcp_accept",
        }
    }

    #[must_use]
    pub const fn resume(self) -> AsyncIoResume {
        match self {
            Self::FileReadBytes | Self::TcpRead => AsyncIoResume::Bytes,
            Self::FileWriteString | Self::FileWriteBytes => AsyncIoResume::WriteStatus,
            Self::TcpAccept => AsyncIoResume::Connection,
        }
    }

    #[must_use]
    pub fn from_c_symbol(symbol: &str) -> Option<Self> {
        Self::iter().find(|op| op.c_symbol() == symbol)
    }

    #[must_use]
    pub const fn contract(self) -> RuntimeSemanticContract {
        use RuntimeResultEffect::{BitCopy, FreshOwned};
        use RuntimeValueKind::{Bytes, IoHandle, String, I32};
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
        // I/O errors belong to the source wrapper's Result/error channel,
        // not the logical-fault set on RuntimeSemanticContract.
        match self {
            Self::FileReadBytes => runtime_semantic_contract(&[PATH], FreshOwned(Bytes), &[]),
            Self::FileWriteString => runtime_semantic_contract(&[PATH, PATH], BitCopy(I32), &[]),
            Self::FileWriteBytes => runtime_semantic_contract(&[PATH, DATA], BitCopy(I32), &[]),
            Self::TcpRead => runtime_semantic_contract(&[CONNECTION], FreshOwned(Bytes), &[]),
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
    #[must_use]
    pub fn matches_async_io_extern(
        self,
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
            AsyncIoOp::FileReadBytes | AsyncIoOp::FileWriteString | AsyncIoOp::FileWriteBytes => {
                "std.fs"
            }
            AsyncIoOp::TcpRead | AsyncIoOp::TcpAccept => "std.net",
        };
        module == owner
            && symbol == op.c_symbol()
            && declaration == format!("{owner}.{symbol}")
            && op.contract().matches_signature(params, result)
            && consuming.len() == params.len()
            && consuming.iter().all(|consumes| !consumes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn handle(name: &str) -> ResolvedTy {
        ResolvedTy::Named {
            name: name.into(),
            args: vec![],
            builtin: None,
            is_opaque: true,
        }
    }

    #[test]
    fn native_io_externs_require_exact_owner_signature_and_borrow_contract() {
        let connection = handle("std.net.Connection");
        let listener = handle("std.net.Listener");
        for (op, module, params, result) in [
            (
                AsyncIoOp::FileReadBytes,
                "std.fs",
                vec![ResolvedTy::String],
                ResolvedTy::Bytes,
            ),
            (
                AsyncIoOp::FileWriteString,
                "std.fs",
                vec![ResolvedTy::String, ResolvedTy::String],
                ResolvedTy::I32,
            ),
            (
                AsyncIoOp::FileWriteBytes,
                "std.fs",
                vec![ResolvedTy::String, ResolvedTy::Bytes],
                ResolvedTy::I32,
            ),
            (
                AsyncIoOp::TcpRead,
                "std.net",
                vec![connection.clone()],
                ResolvedTy::Bytes,
            ),
            (AsyncIoOp::TcpAccept, "std.net", vec![listener], connection),
        ] {
            let family = RuntimeCallFamily::AsyncIo(op);
            let declaration = format!("{module}.{}", op.c_symbol());
            let borrowed = vec![false; params.len()];
            assert!(
                family.matches_async_io_extern(
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
                "user.net",
                &declaration,
                op.c_symbol(),
                &params,
                &result,
                &borrowed
            ));
            assert!(!family.matches_async_io_extern(
                module,
                "other.declaration",
                op.c_symbol(),
                &params,
                &result,
                &borrowed
            ));
            assert!(!family.matches_async_io_extern(
                module,
                &declaration,
                op.c_symbol(),
                &params,
                &ResolvedTy::Unit,
                &borrowed
            ));
            assert!(!family.matches_async_io_extern(
                module,
                &declaration,
                op.c_symbol(),
                &params,
                &result,
                &vec![true; params.len()]
            ));
        }
        let forged = handle("user.Connection");
        assert!(!AsyncIoOp::TcpRead
            .contract()
            .matches_signature(&[forged], &ResolvedTy::Bytes));
    }

    #[test]
    fn accept_retains_the_listener_loan_and_returns_an_independent_connection() {
        let listener = handle("std.net.Listener");
        let connection = handle("std.net.Connection");
        let contract = AsyncIoOp::TcpAccept.contract();
        let instantiated = contract
            .instantiate(std::slice::from_ref(&listener), &connection)
            .unwrap();
        assert_eq!(instantiated.result_ty, connection);
        assert!(!contract.matches_signature(std::slice::from_ref(&listener), &listener));
        assert_eq!(contract.arguments[0].effect, RuntimeArgumentEffect::Borrow);
        assert_eq!(
            AsyncIoOp::TcpAccept.argument_loan(),
            AsyncIoLoan::UntilQuiescent
        );
        assert_eq!(
            AsyncIoOp::FileReadBytes.argument_loan(),
            AsyncIoLoan::UntilSubmitReturns
        );
    }
}
