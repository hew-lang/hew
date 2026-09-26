//! Checked sender policies and source-defined message value contracts.

use crate::{KnownDecl, Ty, TypeHead};

/// The `std/builtins.hew` declarations the delivery protocol is built from.
pub const DECLARATIONS: &[KnownDecl] = &[
    KnownDecl::ActorError,
    KnownDecl::Never,
    KnownDecl::ActorMailbox,
    KnownDecl::ActorPolicy,
    KnownDecl::Message,
    KnownDecl::SendFailure,
    KnownDecl::Delivery,
    KnownDecl::OnFull,
    KnownDecl::RejectSend,
    KnownDecl::WaitSend,
    KnownDecl::DropNewestSend,
    KnownDecl::ReplaceLatestSend,
    KnownDecl::ActorRequest,
    KnownDecl::ActorRequestOwner,
    KnownDecl::ActorRequestAdmission,
];

/// A view's policy is immutable and determines its submission effect.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SendPolicy {
    Reject,
    Wait,
    DropNewest,
    ReplaceLatest,
}

impl SendPolicy {
    /// The witness declaration that carries this policy in a view's type.
    #[must_use]
    pub const fn witness(self) -> KnownDecl {
        match self {
            Self::Reject => KnownDecl::RejectSend,
            Self::Wait => KnownDecl::WaitSend,
            Self::DropNewest => KnownDecl::DropNewestSend,
            Self::ReplaceLatest => KnownDecl::ReplaceLatestSend,
        }
    }

    #[must_use]
    pub fn from_witness(ty: &Ty) -> Option<Self> {
        let Ty::Named { head, args } = ty else {
            return None;
        };
        if !args.is_empty() {
            return None;
        }
        [
            Self::Reject,
            Self::Wait,
            Self::DropNewest,
            Self::ReplaceLatest,
        ]
        .into_iter()
        .find(|policy| policy.witness().head() == *head)
    }

    #[must_use]
    pub const fn may_suspend(self) -> bool {
        matches!(self, Self::Wait)
    }
}

/// The delivery outcome a discarded expression drops, or `None`.
///
/// A submission yields `Result<Delivery, SendFailure<_>>`, the pid/channel
/// `send` family yields `Result<_, SendError>`, and a completion call yields
/// `Result<_, ActorError<_, _>>`. Discarding any of them in statement position loses a
/// delivery failure, which is `E_SEND_RESULT_DROPPED` (HEW-SPEC-2026 §2.1.1,
/// §5.6). The returned name is the error type, for the diagnostic.
#[must_use]
pub fn dropped_delivery_outcome(ty: &Ty) -> Option<&'static str> {
    let (_, error) = ty.as_result()?;
    let Ty::Named { head, .. } = error else {
        return None;
    };
    if *head == TypeHead::Builtin(crate::BuiltinType::SendError) {
        Some("SendError")
    } else if *head == KnownDecl::ActorError.head() {
        Some("ActorError")
    } else if *head == KnownDecl::SendFailure.head() {
        Some("SendFailure")
    } else {
        None
    }
}

/// Selected operations carried independently of source and linker spellings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActorDeliveryCall {
    Close,
    AwaitClosed,
    /// `pid.stop()`: request a graceful stop without waiting. HIR lowers it
    /// to the same request `close` makes and discards the returned handle.
    Stop,
    Policy {
        policy: SendPolicy,
    },
    Readdress {
        policy: SendPolicy,
        target_is_view: bool,
    },
    Submit {
        policy: SendPolicy,
    },
    Resume {
        policy: SendPolicy,
        method_id: String,
        redirect: bool,
    },
}

/// The method declaration identifies a sealed protocol; its arguments retain
/// the concrete request tuple, success and declared failure after binding.
#[must_use]
pub fn request_type(method: crate::NominalHead, params: Ty, success: Ty, failure: Ty) -> Ty {
    nominal(
        KnownDecl::ActorRequest,
        vec![Ty::Named {
            head: TypeHead::Nominal(method),
            args: vec![params, success, failure],
        }],
    )
}

#[must_use]
pub fn request_parts(ty: &Ty) -> Option<(crate::NominalHead, &Ty, &Ty, &Ty)> {
    let Ty::Named { head, args } = ty else {
        return None;
    };
    let [Ty::Named {
        head: TypeHead::Nominal(method),
        args: protocol,
    }] = args.as_slice()
    else {
        return None;
    };
    let [params, success, failure] = protocol.as_slice() else {
        return None;
    };
    (*head == KnownDecl::ActorRequest.head()).then_some((*method, params, success, failure))
}

#[must_use]
pub fn nominal(known: KnownDecl, args: Vec<Ty>) -> Ty {
    Ty::Named {
        head: known.head(),
        args,
    }
}

#[must_use]
pub fn message_type(target: Ty, payload: Ty, policy: SendPolicy) -> Ty {
    nominal(
        KnownDecl::Message,
        vec![target, payload, nominal(policy.witness(), Vec::new())],
    )
}

#[must_use]
pub fn sender_type(target: Ty, policy: SendPolicy) -> Ty {
    nominal(
        KnownDecl::ActorMailbox,
        vec![target, nominal(policy.witness(), Vec::new())],
    )
}

/// The completion view `policy(target, on_full: ..)` builds: calls through it
/// complete like a bare-handle call, under the admission policy it carries.
#[must_use]
pub fn policy_view_type(target: Ty, policy: SendPolicy) -> Ty {
    nominal(
        KnownDecl::ActorPolicy,
        vec![target, nominal(policy.witness(), Vec::new())],
    )
}

#[must_use]
pub fn policy_view_parts(ty: &Ty) -> Option<(&Ty, SendPolicy)> {
    view_parts(ty, KnownDecl::ActorPolicy)
}

#[must_use]
pub fn message_parts(ty: &Ty) -> Option<(&Ty, &Ty, SendPolicy)> {
    let Ty::Named { head, args } = ty else {
        return None;
    };
    let [target, payload, witness] = args.as_slice() else {
        return None;
    };
    if *head != KnownDecl::Message.head() || !target.addresses_local_actor() {
        return None;
    }
    Some((target, payload, SendPolicy::from_witness(witness)?))
}

#[must_use]
pub fn sender_parts(ty: &Ty) -> Option<(&Ty, SendPolicy)> {
    view_parts(ty, KnownDecl::ActorMailbox)
}

fn view_parts(ty: &Ty, view: KnownDecl) -> Option<(&Ty, SendPolicy)> {
    let Ty::Named { head, args } = ty else {
        return None;
    };
    let [target, witness] = args.as_slice() else {
        return None;
    };
    if *head != view.head() || !target.addresses_local_actor() {
        return None;
    }
    Some((target, SendPolicy::from_witness(witness)?))
}

#[must_use]
pub fn result_type(message: Ty) -> Ty {
    Ty::result(
        nominal(KnownDecl::Delivery, Vec::new()),
        nominal(KnownDecl::SendFailure, vec![message]),
    )
}
