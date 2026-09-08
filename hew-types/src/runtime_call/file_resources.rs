//! Synchronous file-read operations over the existing runtime owners.

use super::{
    runtime_semantic_contract, BuiltinType, Deserialize, EnumIter, IntoEnumIterator, ResolvedTy,
    RuntimeArgumentContract, RuntimeArgumentEffect, RuntimeCallFamily, RuntimeResultEffect,
    RuntimeSemanticContract, RuntimeValueKind, Serialize,
};

/// Exact handle relationship carried by a file-read operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FileReadHandleKind {
    /// Source nominal selected by the generated producer/release contract.
    Nominal,
    /// The existing builtin readable string stream.
    Stream,
}

impl FileReadHandleKind {
    /// Match checker type identity without reconstructing declaration opacity.
    /// SIR separately requires the admitted lifecycle and affine type facts.
    #[must_use]
    pub fn matches(self, ty: &ResolvedTy) -> bool {
        match (self, ty) {
            (
                Self::Nominal,
                ResolvedTy::Named {
                    name,
                    args,
                    builtin: None,
                    ..
                },
            ) => crate::ffi_contracts::extern_owned_resource_result(FileReadOp::Open.c_symbol())
                .is_some_and(|contract| name == contract.resource_type && args.is_empty()),
            (
                Self::Stream,
                ResolvedTy::Named {
                    args,
                    builtin: Some(BuiltinType::Stream),
                    is_opaque: false,
                    ..
                },
            ) => args.as_slice() == [ResolvedTy::String],
            _ => false,
        }
    }

    #[must_use]
    pub fn of_ty(ty: &ResolvedTy) -> Option<Self> {
        [Self::Nominal, Self::Stream]
            .into_iter()
            .find(|kind| kind.matches(ty))
    }
}

/// Existing synchronous file adapters and their shared error channel.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, EnumIter, Default)]
pub enum FileReadOp {
    #[default]
    Open,
    IsValid,
    Collect,
    Close,
    StreamOpen,
    StreamIsValid,
    StreamCollect,
    HasError,
    LastErrorKind,
    LastErrno,
    LastError,
}

impl FileReadOp {
    #[must_use]
    pub const fn c_symbol(self) -> &'static str {
        match self {
            Self::Open => "hew_file_read_stream_open",
            Self::IsValid => "hew_file_read_stream_is_valid",
            Self::Collect => "hew_file_read_stream_collect_string",
            Self::Close => "hew_file_read_stream_close",
            Self::StreamOpen => "hew_stream_from_file_read",
            Self::StreamIsValid => "hew_stream_is_valid",
            Self::StreamCollect => "hew_stream_collect_string",
            Self::HasError => "hew_stream_has_error",
            Self::LastErrorKind => "hew_stream_last_error_kind",
            Self::LastErrno => "hew_stream_last_errno",
            Self::LastError => "hew_stream_last_error",
        }
    }

    #[must_use]
    pub fn from_c_symbol(symbol: &str) -> Option<Self> {
        Self::iter().find(|op| op.c_symbol() == symbol)
    }

    #[must_use]
    pub const fn contract(self) -> RuntimeSemanticContract {
        use RuntimeArgumentEffect::{Borrow, Move};
        use RuntimeResultEffect::{BitCopy, FreshOwned, Unit};
        use RuntimeValueKind::{Bool, FileReadHandle, String, I32};
        const PATH: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: String,
            effect: Borrow,
        };
        const NOMINAL_READ: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: FileReadHandle(FileReadHandleKind::Nominal),
            effect: Borrow,
        };
        const NOMINAL_MOVE: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: FileReadHandle(FileReadHandleKind::Nominal),
            effect: Move,
        };
        const STREAM_READ: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: FileReadHandle(FileReadHandleKind::Stream),
            effect: Borrow,
        };
        const STREAM_MOVE: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: FileReadHandle(FileReadHandleKind::Stream),
            effect: Move,
        };
        match self {
            Self::Open => runtime_semantic_contract(
                &[PATH],
                FreshOwned(FileReadHandle(FileReadHandleKind::Nominal)),
                &[],
            ),
            Self::StreamOpen => runtime_semantic_contract(
                &[PATH],
                FreshOwned(FileReadHandle(FileReadHandleKind::Stream)),
                &[],
            ),
            Self::IsValid => runtime_semantic_contract(&[NOMINAL_READ], BitCopy(Bool), &[]),
            Self::StreamIsValid => runtime_semantic_contract(&[STREAM_READ], BitCopy(Bool), &[]),
            Self::Collect => runtime_semantic_contract(&[NOMINAL_MOVE], FreshOwned(String), &[]),
            Self::StreamCollect => {
                runtime_semantic_contract(&[STREAM_MOVE], FreshOwned(String), &[])
            }
            Self::Close => runtime_semantic_contract(&[NOMINAL_MOVE], Unit, &[]),
            Self::HasError => runtime_semantic_contract(&[], BitCopy(Bool), &[]),
            Self::LastErrorKind | Self::LastErrno => {
                runtime_semantic_contract(&[], BitCopy(I32), &[])
            }
            Self::LastError => runtime_semantic_contract(&[], FreshOwned(String), &[]),
        }
    }
}

impl RuntimeCallFamily {
    /// Admit file operations only through their exact shipped source declaration.
    /// The checker separately proves the module's canonical source provenance.
    #[must_use]
    pub fn matches_file_read_extern(
        self,
        module: &str,
        declaration: &str,
        symbol: &str,
        params: &[ResolvedTy],
        result: &ResolvedTy,
        consuming: &[bool],
    ) -> bool {
        let owner_matches = match self {
            Self::FileRead(
                FileReadOp::Open | FileReadOp::IsValid | FileReadOp::Collect | FileReadOp::Close,
            ) => module == "std.fs",
            Self::FileRead(
                FileReadOp::StreamOpen | FileReadOp::StreamIsValid | FileReadOp::StreamCollect,
            )
            | Self::StreamClose => module == "std.stream",
            Self::FileRead(
                FileReadOp::HasError
                | FileReadOp::LastErrorKind
                | FileReadOp::LastErrno
                | FileReadOp::LastError,
            ) => matches!(module, "std.fs" | "std.stream" | "std.net"),
            _ => false,
        };
        owner_matches
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_resource_extern_authority_requires_the_exact_owner_and_transfer_signature() {
        let owner = ResolvedTy::Named {
            name: "std.fs.FileReadStream".into(),
            args: vec![],
            builtin: None,
            is_opaque: true,
        };
        let family = RuntimeCallFamily::FileRead(FileReadOp::Close);
        assert!(family.matches_file_read_extern(
            "std.fs",
            "std.fs.hew_file_read_stream_close",
            family.c_symbol(),
            std::slice::from_ref(&owner),
            &ResolvedTy::Unit,
            &[true]
        ));
        assert!(!family.matches_file_read_extern(
            "std.fs",
            "other.hew_file_read_stream_close",
            family.c_symbol(),
            std::slice::from_ref(&owner),
            &ResolvedTy::Unit,
            &[true]
        ));
        assert!(!family.matches_file_read_extern(
            "std.fs",
            "std.fs.hew_file_read_stream_close",
            family.c_symbol(),
            std::slice::from_ref(&owner),
            &ResolvedTy::Unit,
            &[false]
        ));
        assert!(!family.matches_file_read_extern(
            "std.fs",
            "std.fs.hew_file_read_stream_close",
            family.c_symbol(),
            &[ResolvedTy::String],
            &ResolvedTy::Unit,
            &[true]
        ));
        let valid = RuntimeCallFamily::FileRead(FileReadOp::IsValid);
        assert!(valid.matches_file_read_extern(
            "std.fs",
            "std.fs.hew_file_read_stream_is_valid",
            valid.c_symbol(),
            std::slice::from_ref(&owner),
            &ResolvedTy::Bool,
            &[false]
        ));
        assert!(!valid.matches_file_read_extern(
            "std.fs",
            "std.fs.hew_file_read_stream_is_valid",
            valid.c_symbol(),
            &[owner],
            &ResolvedTy::I32,
            &[false]
        ));
    }
}
