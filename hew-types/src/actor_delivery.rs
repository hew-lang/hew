//! Checked sender policies and source-defined message value contracts.

use serde::{Deserialize, Serialize};

use crate::Ty;

pub const MESSAGE_TYPE: &str = "std.builtins.Message";
pub const SENDER_TYPE: &str = "std.builtins.ActorSender";
pub const FAILURE_TYPE: &str = "std.builtins.SendFailure";
pub const DELIVERY_TYPE: &str = "std.builtins.Delivery";
pub const ON_FULL_TYPE: &str = "std.builtins.OnFull";

/// These declarations have one source owner in `std/builtins.hew`.
pub const DECLARATIONS: &[&str] = &[
    "ActorSender",
    "Message",
    "SendFailure",
    "Delivery",
    "OnFull",
    "RejectSend",
    "WaitSend",
    "DropNewestSend",
    "ReplaceLatestSend",
];

/// A view's policy is immutable and determines its submission effect.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SendPolicy {
    Reject,
    Wait,
    DropNewest,
    ReplaceLatest,
}

impl SendPolicy {
    #[must_use]
    pub const fn witness_name(self) -> &'static str {
        match self {
            Self::Reject => "std.builtins.RejectSend",
            Self::Wait => "std.builtins.WaitSend",
            Self::DropNewest => "std.builtins.DropNewestSend",
            Self::ReplaceLatest => "std.builtins.ReplaceLatestSend",
        }
    }

    #[must_use]
    pub fn from_witness(ty: &Ty) -> Option<Self> {
        let Ty::Named {
            name,
            args,
            builtin: None,
        } = ty
        else {
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
        .find(|policy| policy.witness_name() == name)
    }

    #[must_use]
    pub const fn may_suspend(self) -> bool {
        matches!(self, Self::Wait)
    }
}

/// Selected operations carried independently of source and linker spellings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ActorDeliveryCall {
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
}

#[must_use]
pub fn nominal(name: &str, args: Vec<Ty>) -> Ty {
    Ty::Named {
        name: name.to_string(),
        args,
        builtin: None,
    }
}

#[must_use]
pub fn message_type(target: Ty, payload: Ty, policy: SendPolicy) -> Ty {
    nominal(
        MESSAGE_TYPE,
        vec![target, payload, nominal(policy.witness_name(), Vec::new())],
    )
}

#[must_use]
pub fn sender_type(target: Ty, policy: SendPolicy) -> Ty {
    nominal(
        SENDER_TYPE,
        vec![target, nominal(policy.witness_name(), Vec::new())],
    )
}

#[must_use]
pub fn message_parts(ty: &Ty) -> Option<(&Ty, &Ty, SendPolicy)> {
    let Ty::Named {
        name,
        args,
        builtin: None,
    } = ty
    else {
        return None;
    };
    let [target, payload, witness] = args.as_slice() else {
        return None;
    };
    if name != MESSAGE_TYPE || target.as_local_actor_ref().is_none() {
        return None;
    }
    Some((target, payload, SendPolicy::from_witness(witness)?))
}

#[must_use]
pub fn sender_parts(ty: &Ty) -> Option<(&Ty, SendPolicy)> {
    let Ty::Named {
        name,
        args,
        builtin: None,
    } = ty
    else {
        return None;
    };
    let [target, witness] = args.as_slice() else {
        return None;
    };
    if name != SENDER_TYPE || target.as_local_actor_ref().is_none() {
        return None;
    }
    Some((target, SendPolicy::from_witness(witness)?))
}

#[must_use]
pub fn result_type(message: Ty) -> Ty {
    Ty::result(
        nominal(DELIVERY_TYPE, Vec::new()),
        nominal(FAILURE_TYPE, vec![message]),
    )
}
