//! Checked actor protocols: receiver-local dispatch indices and canonical wire identities.

use crate::ResolvedTy;

/// The checked capacity and overflow contract of an actor's mailbox.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MailboxContract {
    #[default]
    Unbounded,
    Bounded {
        capacity: u32,
        overflow: OverflowContract,
    },
}

impl MailboxContract {
    #[must_use]
    pub const fn capacity(self) -> Option<u32> {
        match self {
            Self::Unbounded => None,
            Self::Bounded { capacity, .. } => Some(capacity),
        }
    }

    #[must_use]
    pub const fn overflow(self) -> OverflowContract {
        match self {
            Self::Unbounded => OverflowContract::Block,
            Self::Bounded { overflow, .. } => overflow,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OverflowContract {
    Block,
    DropNew,
    DropOld,
    Fail,
    Coalesce { fallback: CoalesceFallback },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoalesceFallback {
    DropNew,
    DropOld,
    Fail,
}

/// Checked mailbox contract and canonical handler identities, signatures and keys.
///
/// Constructed by the type checker once all handler signatures are known
/// (`Checker::build_actor_protocol_descriptors`), then checked for keyed
/// parameters during item checking. Threaded through `TypeCheckOutput` and
/// HIR into the semantic actor layout. Downstream stages consume these facts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActorProtocolDescriptor {
    /// The actor-owned interceptor entered around each receive handler.
    pub interceptor: Option<crate::DefId>,
    pub mailbox: MailboxContract,
    /// Canonical owner-qualified actor name.
    pub actor_name: String,
    /// One entry per `receive fn`, with a local index in source order.
    pub handlers: Vec<ActorHandlerDescriptor>,
}

/// One handler's descriptor: local dispatch index plus the signature/symbol
/// shape consumers need to issue a call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActorHandlerDescriptor {
    /// Index of the checked coalescing key in `param_tys`, when declared.
    pub coalesce_key: Option<u32>,
    /// The handler's `receive fn` declaration.
    pub declaration: crate::DefId,
    /// Surface name (`"increment"`).
    pub name: String,
    /// Canonical dotted declaration identity, independent of surface aliases.
    pub wire_identity: String,
    /// Only explicitly declared wire handlers may enter an exported interface.
    pub wire: bool,
    /// Receiver-local dispatch index.
    pub msg_id: u32,
    /// Parameter types declared by the handler, in source order.
    pub param_tys: Vec<ResolvedTy>,
    /// Return type. Unit-returning handlers are still recorded explicitly so
    /// `Fire` vs `Ask` decisions stay in one place.
    pub return_ty: ResolvedTy,
    /// This handler declares `fails`, rather than returning an ordinary Result value.
    pub declared_failure: bool,
    /// Mangled callee symbol used by codegen (e.g. `Counter__increment` —
    /// the actual mangling is owned by the MIR layer; this field stores
    /// whatever string the producer chose so a single descriptor row tells a
    /// consumer "the symbol and the `msg_id` for this handler are these
    /// values, atomically").
    pub symbol: String,
    /// How a `fails` handler's declared error renders when it has no caller.
    /// A one-way submission through a mailbox view leaves the handler's
    /// `Err(e)` with nowhere to go, so it becomes the actor's own fault and
    /// this target supplies the fault's text. `None` for a handler without a
    /// `fails` clause, and for an error type whose rendering the checker
    /// cannot name — those handlers stay refused through a view.
    pub failure_display: Option<ReceiveFailureDisplay>,
    /// Checked actor instances whose payloads have admitted wire schemas.
    pub remote_receivers: Vec<ActorRemoteReceiver>,
}

/// The receiver arguments of a checked actor instance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActorRemoteReceiver {
    pub parameters: Vec<crate::ParamHead>,
    pub args: Vec<ResolvedTy>,
}

impl ActorRemoteReceiver {
    #[must_use]
    pub fn matches(&self, arguments: &[ResolvedTy]) -> bool {
        crate::method_resolution::impl_receiver_substitution(
            &self.parameters,
            &self.args.iter().map(ResolvedTy::to_ty).collect::<Vec<_>>(),
            &arguments.iter().map(ResolvedTy::to_ty).collect::<Vec<_>>(),
        )
        .is_some()
    }
}

/// How one `fails` handler's declared error becomes fault text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReceiveFailureDisplay {
    /// The error is already its own rendering: `fails string`.
    Identity,
    /// A declared `impl Display` body for the error type.
    Declared {
        declaration: crate::DefId,
        instance: crate::EntryCallableInstance,
    },
}

/// Inputs selected from the declaration table and checked handler signature.
#[derive(Debug, Clone)]
pub struct ActorHandlerSpec {
    pub declaration: crate::DefId,
    pub wire_identity: String,
    pub wire: bool,
    pub name: String,
    pub param_tys: Vec<ResolvedTy>,
    pub return_ty: ResolvedTy,
    pub symbol: String,
}

impl ActorProtocolDescriptor {
    /// Assign local dispatch indices. Only `wire_identity` crosses a node boundary.
    ///
    /// # Panics
    /// Panics if a handler index exceeds `u32::MAX`.
    #[must_use]
    pub fn from_handlers(actor_name: impl Into<String>, handlers: &[ActorHandlerSpec]) -> Self {
        Self {
            interceptor: None,
            mailbox: MailboxContract::Unbounded,
            actor_name: actor_name.into(),
            handlers: handlers
                .iter()
                .enumerate()
                .map(|(index, spec)| ActorHandlerDescriptor {
                    coalesce_key: None,
                    declaration: spec.declaration,
                    wire_identity: spec.wire_identity.clone(),
                    wire: spec.wire,
                    name: spec.name.clone(),
                    msg_id: u32::try_from(index)
                        .expect("actor handler count fits a dispatch index"),
                    param_tys: spec.param_tys.clone(),
                    return_ty: spec.return_ty.clone(),
                    declared_failure: false,
                    symbol: spec.symbol.clone(),
                    failure_display: None,
                    remote_receivers: Vec::new(),
                })
                .collect(),
        }
    }

    #[must_use]
    pub fn msg_id_for(&self, handler_name: &str) -> Option<u32> {
        self.handlers
            .iter()
            .find(|handler| handler.name == handler_name)
            .map(|handler| handler.msg_id)
    }
}

/// The message id of a lambda actor's one receive handler.
///
/// A lambda actor has exactly one handler and no wire identity to keep stable
/// across builds, so its id is fixed rather than hashed from a name. A call
/// site reaches a lambda through its `actor(M) -> R` handle type, which names no
/// particular lambda, so the id it addresses cannot depend on which one.
pub const LAMBDA_ACTOR_MESSAGE_ID: u32 = 0;

/// Diagnostic render for the shared lambda actor request-protocol declaration.
pub const LAMBDA_ACTOR_PROTOCOL_PATH: &str = "#lambda::call";

/// The resolver-minted identities of one lambda actor (`actor |msg| { .. }`).
///
/// A lambda actor has no source name, so the checker mints its declaration
/// and its single receive handler keyed by the exact span of the `actor`
/// expression. HIR synthesizes the actor declaration these belong to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LambdaActorIdentity {
    /// The synthesized actor declaration.
    pub actor: crate::DefId,
    /// Its one receive handler.
    pub handler: crate::DefId,
    /// The declaration path the actor is registered under, which is also the
    /// name its synthesized declaration carries.
    pub path: String,
}

/// Concrete backing fields of a delivery's linear call token. The actor-owned
/// request declaration is selected by identity, with the actor's arguments.
#[must_use]
pub fn interceptor_fields(
    defs: &crate::DefTable,
    ty: &ResolvedTy,
) -> Option<Vec<(String, ResolvedTy)>> {
    let ResolvedTy::Named { head, args, .. } = ty else {
        return None;
    };
    let call = *head == crate::KnownDecl::Call.head();
    if !call && *head != crate::KnownDecl::Outcome.head() {
        return None;
    }
    let [ResolvedTy::Named {
        head: actor, args, ..
    }] = args.as_slice()
    else {
        return None;
    };
    let owner = actor.nominal()?.declaration();
    if defs.kind(owner) != crate::DeclarationKind::Actor {
        return None;
    }
    let (name, kind) = if call {
        ("Request", crate::DeclarationKind::ActorRequestType)
    } else {
        ("Response", crate::DeclarationKind::ActorResponseType)
    };
    let request = defs.member_of_kind(owner, hew_parser::ast::Symbol::intern(name), kind)?;
    let member = ResolvedTy::named_user(
        crate::NominalHead::new(
            crate::NominalId::from_minted_declaration(request),
            defs.path(request),
        ),
        args.clone(),
    );
    let stream = defs.member_of_kind(
        owner,
        hew_parser::ast::Symbol::intern("<Stream>"),
        crate::DeclarationKind::ActorStreamType,
    )?;
    let stream_ty = ResolvedTy::named_user(
        crate::NominalHead::new(
            crate::NominalId::from_minted_declaration(stream),
            defs.path(stream),
        ),
        args.clone(),
    );
    if !call {
        return Some(vec![
            (
                "response".to_string(),
                ResolvedTy::named_builtin(crate::BuiltinType::Option, vec![member]),
            ),
            ("failed".to_string(), ResolvedTy::Bool),
            ("refused".to_string(), ResolvedTy::Bool),
            ("<stream>".to_string(), stream_ty),
        ]);
    }
    Some(vec![
        (
            "request".to_string(),
            ResolvedTy::named_user(
                crate::NominalHead::new(
                    crate::NominalId::from_minted_declaration(request),
                    defs.path(request),
                ),
                args.clone(),
            ),
        ),
        (
            "caller".to_string(),
            ResolvedTy::Named {
                head: crate::KnownDecl::Caller.head(),
                args: Vec::new(),
                is_opaque: false,
            },
        ),
        ("handler".to_string(), ResolvedTy::String),
        ("message".to_string(), ResolvedTy::U32),
        ("<stream>".to_string(), stream_ty),
    ])
}

/// These projections lend the delivery-owned value instead of copying it.
#[must_use]
pub fn is_interceptor_view(ty: &ResolvedTy, field: &str) -> bool {
    matches!(ty, ResolvedTy::Named { head, .. }
        if (*head == crate::KnownDecl::Call.head() && field == "request")
            || (*head == crate::KnownDecl::Outcome.head() && field == "response"))
}

/// Generated actor members are selected through their actor or expected type,
/// never through the global bare-variant fallback.
#[must_use]
pub fn is_interceptor_member(defs: &crate::DefTable, declaration: crate::DefId) -> bool {
    matches!(
        defs.kind(declaration),
        crate::DeclarationKind::ActorRequestType
            | crate::DeclarationKind::ActorResponseType
            | crate::DeclarationKind::ActorStreamType
    )
}
