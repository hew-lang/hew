//! Authenticated service acquisition over one exact source interface.

use super::{
    RuntimeArgumentContract, RuntimeArgumentEffect, RuntimeAuthority, RuntimeCReturn,
    RuntimeCallAbiShape, RuntimeOpRow, RuntimePhysicalForm, RuntimeResultEffect,
    RuntimeSemanticContract, RuntimeStaging, RuntimeValueKind,
};
use crate::{KnownDecl, ResolvedTy, TypeHead};
use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ServiceHandleKind {
    Peer,
    Export,
}

impl ServiceHandleKind {
    #[must_use]
    pub const fn declaration(self) -> KnownDecl {
        match self {
            Self::Peer => KnownDecl::PeerSession,
            Self::Export => KnownDecl::ServiceExport,
        }
    }

    #[must_use]
    pub fn matches(self, ty: &ResolvedTy) -> bool {
        matches!(ty, ResolvedTy::Named { head, args, is_opaque: true }
            if head.nominal() == Some(self.declaration().nominal())
                && args.len() == usize::from(self != Self::Peer))
    }

    #[must_use]
    pub fn of_ty(ty: &ResolvedTy) -> Option<Self> {
        [Self::Peer, Self::Export]
            .into_iter()
            .find(|kind| kind.matches(ty))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, EnumIter, Default)]
pub enum ServiceOp {
    #[default]
    Peer,
    Export,
    Receive,
    ClosePeer,
    CloseExport,
}

impl ServiceOp {
    #[must_use]
    pub const fn row(self) -> RuntimeOpRow {
        const PEER: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::ServiceHandle(ServiceHandleKind::Peer),
            effect: RuntimeArgumentEffect::Borrow,
        };
        RuntimeOpRow {
            authority: match self {
                Self::Peer => RuntimeAuthority::Ambient,
                _ => RuntimeAuthority::Service,
            },
            symbol: match self {
                Self::Peer => "hew_node_peer_new",
                Self::Export => "hew_service_export",
                Self::Receive => "hew_service_receive",
                Self::ClosePeer => "hew_peer_session_close",
                Self::CloseExport => "hew_service_export_close",
            },
            contract: Some(RuntimeSemanticContract {
                arguments: match self {
                    Self::Peer => &[RuntimeArgumentContract {
                        ty: RuntimeValueKind::String,
                        effect: RuntimeArgumentEffect::Borrow,
                    }],
                    Self::Export => &[
                        PEER,
                        RuntimeArgumentContract {
                            ty: RuntimeValueKind::ActorHandle,
                            effect: RuntimeArgumentEffect::Borrow,
                        },
                    ],
                    Self::Receive => &[PEER],
                    Self::ClosePeer => &[RuntimeArgumentContract {
                        ty: RuntimeValueKind::ServiceHandle(ServiceHandleKind::Peer),
                        effect: RuntimeArgumentEffect::Move,
                    }],
                    Self::CloseExport => &[RuntimeArgumentContract {
                        ty: RuntimeValueKind::ServiceHandle(ServiceHandleKind::Export),
                        effect: RuntimeArgumentEffect::Move,
                    }],
                },
                result: if matches!(self, Self::ClosePeer | Self::CloseExport) {
                    RuntimeResultEffect::Unit
                } else {
                    RuntimeResultEffect::FreshOwned(RuntimeValueKind::ServiceResult(self))
                },
                failures: &[],
            }),
            staging: if matches!(self, Self::ClosePeer | Self::CloseExport) {
                RuntimeStaging::Declared
            } else {
                RuntimeStaging::PreStaged
            },
            abi_shape: RuntimeCallAbiShape::Other,
            physical: match self {
                Self::Peer => RuntimePhysicalForm::NodeResult,
                Self::ClosePeer | Self::CloseExport => RuntimePhysicalForm::Direct,
                _ => RuntimePhysicalForm::NotAnAction,
            },
            c_return: RuntimeCReturn::Storage,
        }
    }

    #[must_use]
    pub fn matches_result(self, ty: &ResolvedTy) -> bool {
        let ResolvedTy::Named {
            head: TypeHead::Builtin(crate::BuiltinType::Result),
            args,
            ..
        } = ty
        else {
            return false;
        };
        let [success, failure] = args.as_slice() else {
            return false;
        };
        match self {
            Self::ClosePeer | Self::CloseExport => false,
            Self::Peer => {
                ServiceHandleKind::Peer.matches(success)
                    && failure.is_builtin(crate::BuiltinType::NodeError)
            }
            Self::Export | Self::Receive => {
                (if self == Self::Receive {
                    matches!(
                        success,
                        ResolvedTy::Named {
                            head: TypeHead::Actor(_),
                            ..
                        }
                    )
                } else {
                    ServiceHandleKind::Export.matches(success)
                }) && failure.to_ty()
                    == crate::Ty::actor_error_with_request(
                        crate::Ty::never_type(),
                        crate::Ty::never_type(),
                    )
            }
        }
    }

    #[must_use]
    pub fn source_method(
        signature: &str,
        symbol: &str,
        params: &[crate::Ty],
        result: &crate::Ty,
    ) -> Option<Self> {
        let op = match signature {
            "std.net.PeerSession::release"
                if params.is_empty()
                    && *result == crate::Ty::Unit
                    && symbol == "hew_peer_session_close" =>
            {
                return Some(Self::ClosePeer)
            }
            "std.net.Export::release"
                if params.is_empty()
                    && *result == crate::Ty::Unit
                    && symbol == "hew_service_export_close" =>
            {
                return Some(Self::CloseExport)
            }
            "std.net.PeerSession::export" => Self::Export,
            "std.net.PeerSession::receive_service" => Self::Receive,
            _ => return None,
        };
        if op.row().symbol != symbol {
            return None;
        }
        let crate::Ty::Named {
            head: TypeHead::Builtin(crate::BuiltinType::Result),
            args,
        } = result
        else {
            return None;
        };
        let [success, error] = args.as_slice() else {
            return None;
        };
        if *error
            != crate::Ty::actor_error_with_request(crate::Ty::never_type(), crate::Ty::never_type())
        {
            return None;
        }
        if op == Self::Receive {
            if !params.is_empty() {
                return None;
            }
        } else {
            let crate::Ty::Named {
                head,
                args: actor_args,
            } = success
            else {
                return None;
            };
            if head.nominal() != Some(KnownDecl::ServiceExport.nominal())
                || actor_args.len() != 1
                || params != actor_args.as_slice()
            {
                return None;
            }
        }
        Some(op)
    }
}
