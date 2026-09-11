//! Actor identities, state seats and declaration-order protocol contracts.

use hew_types::{DefId, ResolvedTy};

use crate::{CallableId, SemModule};

/// Identity of one demanded actor declaration in this semantic module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ActorId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemActorField {
    pub ty: ResolvedTy,
    pub mutable: bool,
    /// `init` initializes the field (D447): a spawn supplies no value and
    /// the seat is uninitialized until init's first store.
    pub deferred: bool,
}

/// A checker-selected receive protocol member and its private body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemActorHandler {
    pub declaration: DefId,
    pub name: String,
    pub message_id: u32,
    /// Periodic cadence selected from the checked duration literal.
    pub every_ns: Option<i64>,
    pub callable: CallableId,
    /// Message payload fields. A stream producer's last field is the caller's
    /// `Sink<T>`, owned by the body until its turn ends.
    pub params: Vec<ResolvedTy>,
    pub return_ty: ResolvedTy,
    /// `receive gen fn`: the element type each `yield` sends to the sink.
    pub stream: Option<ResolvedTy>,
    /// How this handler's declared failure renders when a one-way submission
    /// leaves it with no caller. `None` when the handler cannot fail.
    pub failure_display: Option<SemFailureDisplay>,
}

impl SemActorHandler {
    /// Whether a submission to this handler owes its sender no value. A unit
    /// handler qualifies, and so does a `fails` handler whose success is unit:
    /// its declared error becomes the actor's own fault, not a reply.
    #[must_use]
    pub fn owes_no_reply(&self) -> bool {
        if self.return_ty == ResolvedTy::Unit {
            return true;
        }
        self.failure_display.is_some()
            && matches!(&self.return_ty, ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Result),
                args,
                ..
            } if matches!(args.as_slice(), [ResolvedTy::Unit, _]))
    }
}

/// The rendering a `fails` handler's declared error uses for its fault text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemFailureDisplay {
    /// The error is already its own rendering: `fails string`.
    Identity,
    /// The demanded `Display::fmt` body for the error type.
    Callable(CallableId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemActorOverflow {
    Block,
    DropNew,
    DropOld,
    Fail,
}

/// One actor owns its initialized state through every strict receive turn.
/// Body parameters receive the exclusive state seat; mailbox and reply owners
/// cross separate explicit transfer boundaries.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemActor {
    pub id: ActorId,
    pub declaration: DefId,
    pub handle_ty: ResolvedTy,
    pub state_ty: ResolvedTy,
    pub fields: Vec<SemActorField>,
    pub init: Option<CallableId>,
    /// `#[on(start)]`: runs once after init, before the handle is published.
    pub start: Option<CallableId>,
    /// `#[on(stop)]` bodies in lexical order, run at the terminal transition
    /// with the state still initialized and before its cleanup.
    pub stop: Vec<CallableId>,
    /// `#[on(crash)]`: runs in the supervising context after a child trap is
    /// classified. The hook receives `CrashInfo` and returns `CrashAction`.
    pub crash: Option<CallableId>,
    /// `#[on(exit)]`: receives the typed link-exit notification.
    pub exit: Option<CallableId>,
    /// `#[on(down)]`: receives the typed monitor DOWN notification.
    pub down: Option<CallableId>,
    /// Plain `fn` items, entered only from this actor's own bodies with the
    /// same exclusive state seat.
    pub methods: Vec<CallableId>,
    pub handlers: Vec<SemActorHandler>,
    pub mailbox_capacity: Option<u32>,
    pub overflow: SemActorOverflow,
    pub max_heap_bytes: Option<u64>,
}

impl SemModule {
    #[must_use]
    pub fn actor(&self, id: ActorId) -> Option<&SemActor> {
        self.actors
            .get(id.0 as usize)
            .filter(|actor| actor.id == id)
    }
}

/// The actor declaration and type arguments a local reference names.
///
/// An actor is the type of its handle, so a handle carries its own identity; a
/// `ChildRef<A>` names the same actor through its role parameter.
pub(crate) fn local_actor_instance(
    ty: &ResolvedTy,
) -> Option<hew_types::resolved_ty::NominalInstance> {
    if let Some(instance) = ty.actor_handle_instance() {
        return Some(instance);
    }
    let ResolvedTy::Named {
        builtin: Some(hew_types::BuiltinType::ChildRef),
        args,
        ..
    } = ty
    else {
        return None;
    };
    let [actor_ty] = args.as_slice() else {
        return None;
    };
    actor_ty.actor_handle_instance()
}

impl SemActor {
    /// The stable supervised role for this actor: re-resolved through its
    /// supervisor on every use, never a cached address.
    #[must_use]
    pub fn child_ref_ty(&self) -> ResolvedTy {
        ResolvedTy::named_builtin(
            hew_types::BuiltinType::ChildRef.canonical_name(),
            hew_types::BuiltinType::ChildRef,
            vec![self.handle_ty.clone()],
        )
    }

    /// Whether this actor's handle is an anonymous actor's `actor(M) -> R`.
    ///
    /// A lambda actor is spawned from an expression, never supervised as a
    /// named role, so it has no `ChildRef` spelling.
    #[must_use]
    pub fn is_lambda(&self) -> bool {
        matches!(
            self.handle_ty,
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::ActorFn),
                ..
            }
        )
    }

    /// A message may address this actor through its handle or its role.
    #[must_use]
    pub fn admits_target(&self, ty: &ResolvedTy) -> bool {
        *ty == self.handle_ty || (!self.is_lambda() && *ty == self.child_ref_ty())
    }

    /// Every private body entered with this actor's exclusive state seat.
    pub fn bodies(&self) -> impl Iterator<Item = CallableId> + '_ {
        self.init
            .into_iter()
            .chain(self.start)
            .chain(self.stop.iter().copied())
            .chain(self.crash)
            .chain(self.exit)
            .chain(self.down)
            .chain(self.methods.iter().copied())
            .chain(self.handlers.iter().map(|handler| handler.callable))
    }

    /// The receive protocol owns request parameter order and the reply value;
    /// the call site owns the sealed message inside its `ActorError`, so the
    /// caller supplies the complete checked error type. Source and downstream
    /// verifiers consume this same signature.
    ///
    /// # Errors
    /// Rejects an unknown protocol member or an unresolved reply type.
    pub fn ask_signature(
        &self,
        message: u32,
        target: &ResolvedTy,
        result_ty: ResolvedTy,
        sealed: bool,
    ) -> Result<crate::SemSignature, String> {
        let handler = self
            .handlers
            .iter()
            .find(|handler| handler.message_id == message)
            .ok_or("ask has no exact receive protocol member")?;
        if !self.admits_target(target) {
            return Err("ask target is neither this actor's handle nor its role".into());
        }
        let mut params = vec![crate::SemAbiParam {
            ty: target.clone(),
            passing: crate::SemParamPassing::Consume,
            caller_visible_projection: false,
        }];
        let request_params = if sealed {
            vec![ResolvedTy::named_opaque(
                "std.builtins.ActorRequestOwner",
                Vec::new(),
            )]
        } else {
            handler.params.clone()
        };
        params.extend(request_params.iter().map(|ty| crate::SemAbiParam {
            ty: ty.clone(),
            passing: crate::SemParamPassing::Consume,
            caller_visible_projection: false,
        }));
        let [reply, error] =
            result_parts(&result_ty).ok_or("a completion call must return its checked Result")?;
        // The handler declares either the reply itself, or `R fails E` — whose
        // `Err` becomes the envelope's `Failed(E)`, so the call's success arm is
        // `R` and its declared failure is the envelope's first argument.
        let declared_failure = match error {
            ResolvedTy::Named { args, .. } => args.first(),
            _ => None,
        };
        let matches_protocol = handler.return_ty == *reply
            || result_parts(&handler.return_ty)
                .is_some_and(|[ok, err]| ok == reply && Some(err) == declared_failure);
        if !matches_protocol {
            return Err("ask reply differs from its receive protocol".into());
        }
        let ResolvedTy::Named { name, args, .. } = error else {
            return Err("ask lacks its completion envelope".into());
        };
        let [failure, request] = args.as_slice() else {
            return Err("ask envelope lacks its failure and request parameters".into());
        };
        if name != hew_types::actor_delivery::ACTOR_ERROR_TYPE {
            return Err("ask must return the checked ActorError envelope".into());
        }
        if request.to_ty() != hew_types::Ty::never_type() {
            let request = request.to_ty();
            let (request_target, payload, policy) =
                hew_types::actor_delivery::message_parts(&request)
                    .ok_or("ask rejection lacks its addressed request type")?;
            let (method, parameters, success, request_failure) =
                hew_types::actor_delivery::request_parts(payload)
                    .ok_or("ask rejection lacks its sealed handler protocol")?;
            if *request_target != target.to_ty()
                || policy != hew_types::actor_delivery::SendPolicy::Reject
                || (method != handler.declaration.full_path()
                    && !(self.is_lambda()
                        && method == hew_types::actor_protocol::LAMBDA_ACTOR_METHOD_ID))
                || *parameters
                    != hew_types::Ty::Tuple(handler.params.iter().map(ResolvedTy::to_ty).collect())
                || *success != reply.to_ty()
                || *request_failure != failure.to_ty()
            {
                return Err("sealed request differs from the admitted handler protocol".into());
            }
        }
        Ok(crate::SemSignature {
            params,
            return_ty: result_ty,
        })
    }

    /// Check the handle spelling this descriptor answers to.
    fn validate_handle(&self) -> Result<(), String> {
        // A named actor is addressed by its own type. An anonymous actor has no
        // source nominal to name, so its handle is `actor(M) -> R` and the
        // protocol it must agree with is its single handler's.
        match &self.handle_ty {
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::ActorHandle),
                ..
            } => {
                if !self
                    .handle_ty
                    .actor_handle_instance()
                    .is_some_and(|instance| instance.nominal.declaration() == &self.declaration)
                {
                    return Err("actor handle refers to another declaration".into());
                }
            }
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::ActorFn),
                args,
                ..
            } => {
                let [msg, reply] = args.as_slice() else {
                    return Err("an anonymous actor handle carries its message and reply".into());
                };
                let [handler] = self.handlers.as_slice() else {
                    return Err("an anonymous actor declares exactly one handler".into());
                };
                let expected_msg = match handler.params.as_slice() {
                    [] => ResolvedTy::Unit,
                    [only] => only.clone(),
                    many => ResolvedTy::Tuple(many.to_vec()),
                };
                if *msg != expected_msg || *reply != handler.return_ty {
                    return Err("anonymous actor handle differs from its handler's protocol".into());
                }
            }
            _ => return Err("actor descriptor requires a typed local handle".into()),
        }
        Ok(())
    }

    fn validate_lifecycle_signature(
        &self,
        body: crate::CallableId,
        callable: &crate::SemCallable,
    ) -> Result<(), String> {
        let typed = if self.crash == Some(body) {
            Some((
                hew_types::BuiltinType::CrashInfo,
                hew_types::BuiltinType::CrashAction,
            ))
        } else if self.exit == Some(body) {
            Some((
                hew_types::BuiltinType::CrashNotification,
                hew_types::BuiltinType::Unit,
            ))
        } else if self.down == Some(body) {
            Some((
                hew_types::BuiltinType::DownNotification,
                hew_types::BuiltinType::Unit,
            ))
        } else {
            None
        };
        let expected_params = usize::from(typed.is_some()) + 1;
        let expected_return = match typed {
            Some((_, hew_types::BuiltinType::CrashAction)) => ResolvedTy::named_builtin(
                "std.failure.CrashAction",
                hew_types::BuiltinType::CrashAction,
                Vec::new(),
            ),
            _ => ResolvedTy::Unit,
        };
        let payload_matches = typed.is_none_or(|(expected, _)| {
            callable.signature.params.get(1).is_some_and(|parameter| {
                parameter.ty.is_builtin(expected)
                    && parameter
                        .ty
                        .nominal_instance()
                        .is_some_and(|instance| instance.args.is_empty())
            })
        });
        if callable.signature.params.len() != expected_params
            || callable.signature.return_ty != expected_return
            || !payload_matches
        {
            return Err("lifecycle hook signature differs from its declared kind".into());
        }
        Ok(())
    }

    pub(crate) fn validate(&self, module: &SemModule) -> Result<(), String> {
        if module.actor(self.id) != Some(self) {
            return Err("actor descriptor is not at its canonical index".into());
        }
        self.validate_handle()?;
        if self.state_ty
            != ResolvedTy::Tuple(self.fields.iter().map(|field| field.ty.clone()).collect())
        {
            return Err("actor state differs from its declaration-order fields".into());
        }
        let mut messages = std::collections::BTreeSet::new();
        let mut bodies = std::collections::BTreeSet::new();
        let hooks = self
            .start
            .iter()
            .chain(&self.stop)
            .chain(self.crash.iter())
            .chain(self.exit.iter())
            .chain(self.down.iter());
        for (body, handler) in self
            .init
            .iter()
            .chain(hooks.clone())
            .chain(&self.methods)
            .map(|id| (*id, None))
            .chain(
                self.handlers
                    .iter()
                    .map(|handler| (handler.callable, Some(handler))),
            )
        {
            let callable = module
                .callables
                .iter()
                .find(|callable| callable.id == body)
                .ok_or("actor body has no callable header")?;
            if !bodies.insert(body) || callable.kind != crate::SemCallableKind::HewActor(self.id) {
                return Err("actor body has an inconsistent owner or repeated identity".into());
            }
            let Some(receiver) = callable.signature.params.first() else {
                return Err("actor body has no state receiver".into());
            };
            if receiver.ty != self.state_ty
                || receiver.passing != crate::SemParamPassing::BorrowMut
                || !receiver.caller_visible_projection
            {
                return Err("actor body requires the exact exclusive state ABI".into());
            }
            if let Some(handler) = handler {
                if !messages.insert(handler.message_id)
                    || callable.declaration != handler.declaration
                    || callable.signature.return_ty != handler.return_ty
                    || callable
                        .signature
                        .params
                        .iter()
                        .skip(1)
                        .map(|param| &param.ty)
                        .ne(handler.params.iter())
                {
                    return Err(
                        "actor receive callable differs from its unique protocol member".into(),
                    );
                }
            } else if self.init == Some(body) {
                if callable.signature.return_ty != ResolvedTy::Unit {
                    return Err("actor init must return unit".into());
                }
            } else if hooks.clone().any(|hook| *hook == body) {
                self.validate_lifecycle_signature(body, callable)?;
            }
            let lends = self.methods.contains(&body);
            for parameter in callable.signature.params.iter().skip(1) {
                let own = crate::OwnKind::of_ty(&parameter.ty, &module.type_facts)?;
                let admitted = match parameter.passing {
                    crate::SemParamPassing::ReadOnly => own == crate::OwnKind::None,
                    crate::SemParamPassing::Consume => own == crate::OwnKind::Owned,
                    crate::SemParamPassing::Borrow => lends && own == crate::OwnKind::Owned,
                    crate::SemParamPassing::BorrowMut => false,
                };
                if !admitted {
                    return Err("actor body parameter passing differs from its ownership".into());
                }
            }
        }
        Ok(())
    }
}

fn body_actor<'a>(function: &crate::SemFunction, actors: &'a [SemActor]) -> Option<&'a SemActor> {
    actors
        .iter()
        .find(|actor| actor.bodies().any(|body| body == function.callable))
}

pub(crate) fn verify_places(
    function: &crate::SemFunction,
    actors: Option<&[SemActor]>,
) -> Result<(), String> {
    let places: Vec<_> = function
        .places
        .iter()
        .filter(|place| matches!(place.origin, crate::PlaceOrigin::ActorState { .. }))
        .collect();
    let actor = actors.and_then(|actors| body_actor(function, actors));
    if places.is_empty() && actor.is_none() {
        return Ok(());
    }
    let actor = actor.ok_or("actor state access has no enclosing actor body")?;
    let receiver = function
        .params
        .first()
        .ok_or("actor body has no state receiver")?;
    if receiver.ty != actor.state_ty
        || receiver.own != crate::OwnKind::Guaranteed
        || places.len() != actor.fields.len()
    {
        return Err("actor body must borrow its complete exclusive state".into());
    }
    for (index, (place, field)) in places.iter().zip(&actor.fields).enumerate() {
        let expected_index = u32::try_from(index).map_err(|_| "actor field count exceeds u32")?;
        let crate::PlaceOrigin::ActorState {
            actor: place_actor,
            state,
            field: place_field,
            initialized,
        } = place.origin
        else {
            unreachable!("filtered to actor state places")
        };
        if place.ty != field.ty
            || place_actor != actor.id
            || state != receiver.value
            || place_field != expected_index
        {
            return Err("actor place differs from its declared state field".into());
        }
        if initialized == (field.deferred && actor.init == Some(function.callable)) {
            return Err(
                "actor place initialization differs from its field's init ownership".into(),
            );
        }
    }
    Ok(())
}

pub(crate) fn verify_operation(
    function: &crate::SemFunction,
    op: &crate::SemOp,
    types: &std::collections::HashMap<crate::ValueId, ResolvedTy>,
    facts: &crate::ownership::TypeFactTable,
    actors: Option<&[SemActor]>,
) -> Option<Result<(), String>> {
    use crate::SemOpKind;
    let mut state_place = None;
    op.kind.visit_places(|id| {
        if let Some(place) = function.places.iter().find(|place| {
            place.id == id && matches!(place.origin, crate::PlaceOrigin::ActorState { .. })
        }) {
            state_place = Some(place);
        }
    });
    let place = state_place?;
    Some((|| {
        let actor = actors
            .and_then(|actors| body_actor(function, actors))
            .ok_or("state operation is outside its actor body")?;
        let crate::PlaceOrigin::ActorState {
            actor: id, field, ..
        } = place.origin
        else {
            unreachable!()
        };
        if actor.id != id {
            return Err("state operation crosses actor identities".into());
        }
        let field = actor
            .fields
            .get(field as usize)
            .ok_or("unknown actor state field")?;
        match &op.kind {
            // Registration observes availability. The defer plan independently
            // verifies the complete set of free places and their lifetimes.
            SemOpKind::RegisterDefer { .. } => {}
            // Init's fault path releases a deferred seat it initialized (D447).
            SemOpKind::EndLifetime { .. }
                if field.deferred && actor.init == Some(function.callable) =>
            {
                if !op.results.is_empty() {
                    return Err("actor state release has no result".into());
                }
            }
            SemOpKind::LoadCopy { .. } | SemOpKind::LoadBorrow { .. } => {
                let [result] = op.results.as_slice() else {
                    return Err("actor state load needs one result".into());
                };
                let expected = if matches!(op.kind, SemOpKind::LoadBorrow { .. }) {
                    crate::OwnKind::Guaranteed
                } else {
                    crate::OwnKind::of_ty(&field.ty, facts)?
                };
                if result.ty != field.ty || result.own != expected {
                    return Err("actor state load changes field type or ownership".into());
                }
                if matches!(op.kind, SemOpKind::LoadCopy { .. })
                    && facts
                        .get(&hew_types::TypeInstanceKey(field.ty.clone()))
                        .is_none_or(|row| row.clone == hew_types::CloneKind::None)
                {
                    return Err("actor state field has no copy operation".into());
                }
            }
            SemOpKind::StoreAssign { value, .. } => {
                if !field.mutable && actor.init != Some(function.callable) {
                    return Err("actor state assignment requires a mutable field".into());
                }
                if !op.results.is_empty() || types.get(&value.value) != Some(&field.ty) {
                    return Err("actor state replacement requires one exact field value".into());
                }
            }
            SemOpKind::StoreInit { value, .. } => {
                if !(field.deferred && actor.init == Some(function.callable)) {
                    return Err("only init initializes a deferred actor state field".into());
                }
                if !op.results.is_empty() || types.get(&value.value) != Some(&field.ty) {
                    return Err("actor state initialization requires one exact field value".into());
                }
            }
            _ => return Err("a handler cannot end or extract an actor state seat".into()),
        }
        Ok(())
    })())
}

/// Actor boundary selected from an exact demanded protocol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActorCallProtocol {
    pub actor: ActorId,
    pub message: u32,
    pub target: ResolvedTy,
    pub result: ResolvedTy,
    pub policy: hew_types::actor_delivery::SendPolicy,
    pub deadline_ns: Option<i64>,
    pub sealed: bool,
}

impl ActorCallProtocol {
    /// The operation is an affine owner, distinct from a scope-owned task.
    #[must_use]
    pub fn operation_ty(&self) -> ResolvedTy {
        ResolvedTy::named_builtin(
            hew_types::BuiltinType::ActorCall.canonical_name(),
            hew_types::BuiltinType::ActorCall,
            vec![self.result.clone()],
        )
    }
}

/// Local observation boundary selected by the checked runtime family.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalObservationKind {
    Link,
    Monitor,
    Unlink,
    Demonitor,
}

impl LocalObservationKind {
    fn signature(
        self,
        target: &ResolvedTy,
        result: &ResolvedTy,
    ) -> Result<crate::SemSignature, String> {
        let valid_target = match self {
            LocalObservationKind::Demonitor => *target == ResolvedTy::U64,
            _ => target.to_ty().as_actor_handle().is_some(),
        };
        let valid_result = match self {
            LocalObservationKind::Link | LocalObservationKind::Monitor => result_parts(result)
                .is_some_and(|[ok, error]| {
                    error.is_builtin(hew_types::BuiltinType::LinkError)
                        && if self == LocalObservationKind::Link {
                            *ok == ResolvedTy::Unit
                        } else {
                            ok.is_builtin(hew_types::BuiltinType::MonitorRef)
                        }
                }),
            _ => *result == ResolvedTy::Unit,
        };
        if !valid_target || !valid_result {
            return Err("local observation changes its checked signature".into());
        }
        Ok(crate::SemSignature {
            params: vec![crate::SemAbiParam {
                ty: target.clone(),
                passing: crate::SemParamPassing::Consume,
                caller_visible_projection: false,
            }],
            return_ty: result.clone(),
        })
    }
}

/// Actor boundary selected from an exact demanded protocol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActorOperation {
    LocalObservation {
        kind: LocalObservationKind,
        target: ResolvedTy,
        result: ResolvedTy,
    },
    /// Start an owned completion operation without waiting for admission or reply.
    CallStart(ActorCallProtocol),
    /// Consume an operation selected as ready, materializing its checked result.
    CallTake(ActorCallProtocol),
    Close(ActorId),
    AwaitClosed(ActorId),
    Spawn(ActorId),
    /// The running actor's own handle — bare `self` in an actor body. Takes no
    /// operands: the handle is the actor the boundary already runs inside.
    SelfHandle(ActorId),
    /// Start a stream producer turn: the request payload, whose last field is
    /// the consumer's sink, waits for mailbox capacity. A closed producer is a
    /// fault, so no consumer waits on a pipe nobody feeds.
    StreamStart {
        actor: ActorId,
        message: u32,
        /// The handle or role the request addresses.
        target: ResolvedTy,
    },
    Submit {
        actor: ActorId,
        policy: hew_types::actor_delivery::SendPolicy,
        message_ty: ResolvedTy,
        result_ty: ResolvedTy,
    },
    /// Construct the supervisor from its config, spawn every declared child
    /// through its spawn callable and start supervising. The result is the
    /// supervisor's handle.
    SupervisorSpawn(crate::SupervisorId),
    /// Produce one declared child's stable role, composing the owner role
    /// when the supervisor is itself a nested child.
    SupervisorChild {
        supervisor: crate::SupervisorId,
        child: u32,
        owner_is_role: bool,
    },
    /// Stop the supervisor and every child; each child's stop hooks run before
    /// its terminal cleanup.
    SupervisorStop(crate::SupervisorId),
    /// Observe supervisor reclamation without requesting shutdown.
    SupervisorAwaitClosed(crate::SupervisorId),
    /// Observe the incarnation selected from a supervisor role once; close
    /// additionally requests its stop after retaining completion.
    SupervisorRoleAwaitClosed {
        supervisor: crate::SupervisorId,
        closing: bool,
    },
    /// Wait until one declared child is Live again after a crash, or is
    /// permanently gone, then produce its role. The restart barrier: without
    /// it a caller cannot tell a pre-crash incarnation from its replacement.
    SupervisorAwaitRestart {
        supervisor: crate::SupervisorId,
        child: u32,
        owner_is_role: bool,
    },
    /// Produce a `pool` child's view: the owning supervisor and the first of
    /// the pool's consecutive member slots. The member count is a declaration
    /// fact, so the view carries only what varies at runtime.
    SupervisorPoolView {
        supervisor: crate::SupervisorId,
        child: u32,
        owner_is_role: bool,
    },
}

impl ActorOperation {
    /// The supervisor boundaries: every one consumes handles and produces a
    /// handle, a role or nothing.
    fn supervisor_signature(
        &self,
        actors: &[SemActor],
        supervisors: &[crate::SemSupervisor],
    ) -> Result<crate::SemSignature, String> {
        let (Self::SupervisorSpawn(id)
        | Self::SupervisorChild { supervisor: id, .. }
        | Self::SupervisorAwaitRestart { supervisor: id, .. }
        | Self::SupervisorPoolView { supervisor: id, .. }
        | Self::SupervisorStop(id)
        | Self::SupervisorAwaitClosed(id)
        | Self::SupervisorRoleAwaitClosed { supervisor: id, .. }) = self
        else {
            return Err("operation is not a supervisor boundary".into());
        };
        let supervisor = supervisors
            .get(id.0 as usize)
            .filter(|supervisor| supervisor.id == *id)
            .ok_or("unknown supervisor identity")?;
        let consume = |types: Vec<ResolvedTy>, return_ty| crate::SemSignature {
            params: types
                .into_iter()
                .map(|ty| crate::SemAbiParam {
                    ty,
                    passing: crate::SemParamPassing::Consume,
                    caller_visible_projection: false,
                })
                .collect(),
            return_ty,
        };
        Ok(match self {
            Self::SupervisorSpawn(_) => {
                consume(supervisor.config.clone(), supervisor.handle_ty.clone())
            }
            Self::SupervisorChild {
                child,
                owner_is_role,
                ..
            }
            | Self::SupervisorAwaitRestart {
                child,
                owner_is_role,
                ..
            } => consume(
                vec![if *owner_is_role {
                    supervisor.child_ref_ty()
                } else {
                    supervisor.handle_ty.clone()
                }],
                supervisor.child_handle_ty(*child as usize, actors, supervisors)?,
            ),
            Self::SupervisorPoolView {
                child,
                owner_is_role,
                ..
            } => consume(
                vec![if *owner_is_role {
                    supervisor.child_ref_ty()
                } else {
                    supervisor.handle_ty.clone()
                }],
                supervisor.pool_view_ty(*child as usize, actors, supervisors)?,
            ),
            Self::SupervisorRoleAwaitClosed { .. } => {
                consume(vec![supervisor.child_ref_ty()], ResolvedTy::Unit)
            }
            _ => consume(vec![supervisor.handle_ty.clone()], ResolvedTy::Unit),
        })
    }

    fn completion_signature(&self, actors: &[SemActor]) -> Result<crate::SemSignature, String> {
        let (Self::CallStart(protocol) | Self::CallTake(protocol)) = self else {
            return Err("operation is not a completion boundary".into());
        };
        let actor = actors
            .get(protocol.actor.0 as usize)
            .filter(|actor| actor.id == protocol.actor)
            .ok_or("unknown completion actor identity")?;
        let sealed = matches!(self, Self::CallStart(_)) && protocol.sealed;
        let mut signature = actor.ask_signature(
            protocol.message,
            &protocol.target,
            protocol.result.clone(),
            sealed,
        )?;
        match self {
            Self::CallStart(_) => {
                signature.params[0].passing = crate::SemParamPassing::Borrow;
                signature.return_ty = protocol.operation_ty();
            }
            Self::CallTake(_) => {
                signature.params = vec![
                    crate::SemAbiParam {
                        ty: protocol.operation_ty(),
                        passing: crate::SemParamPassing::Consume,
                        caller_visible_projection: false,
                    },
                    crate::SemAbiParam {
                        ty: protocol.target.clone(),
                        passing: crate::SemParamPassing::Borrow,
                        caller_visible_projection: false,
                    },
                ];
            }
            _ => unreachable!(),
        }
        Ok(signature)
    }

    /// Project the exact boundary ABI from the demanded actor protocol.
    ///
    /// # Errors
    /// Refuses missing actors, handlers, init bodies or incompatible sends.
    #[allow(
        clippy::too_many_lines,
        reason = "the actor boundary match is the single signature authority for all actor operations"
    )]
    pub fn signature(
        &self,
        actors: &[SemActor],
        supervisors: &[crate::SemSupervisor],
        callable: impl Fn(crate::CallableId) -> Option<crate::SemSignature>,
    ) -> Result<crate::SemSignature, String> {
        if matches!(self, Self::CallStart(_) | Self::CallTake(_)) {
            return self.completion_signature(actors);
        }
        if let Self::LocalObservation {
            kind,
            target,
            result,
        } = self
        {
            return kind.signature(target, result);
        }
        let consume = |types: Vec<ResolvedTy>, return_ty| crate::SemSignature {
            params: types
                .into_iter()
                .map(|ty| crate::SemAbiParam {
                    ty,
                    passing: crate::SemParamPassing::Consume,
                    caller_visible_projection: false,
                })
                .collect(),
            return_ty,
        };
        let id = match self {
            Self::LocalObservation { .. } | Self::CallStart(_) | Self::CallTake(_) => {
                unreachable!("special boundary returned above")
            }
            Self::Spawn(id)
            | Self::Close(id)
            | Self::AwaitClosed(id)
            | Self::SelfHandle(id)
            | Self::StreamStart { actor: id, .. }
            | Self::Submit { actor: id, .. } => *id,
            Self::SupervisorSpawn(_)
            | Self::SupervisorChild { .. }
            | Self::SupervisorAwaitRestart { .. }
            | Self::SupervisorPoolView { .. }
            | Self::SupervisorAwaitClosed(_)
            | Self::SupervisorRoleAwaitClosed { .. }
            | Self::SupervisorStop(_) => return self.supervisor_signature(actors, supervisors),
        };
        let actor = actors
            .get(id.0 as usize)
            .filter(|actor| actor.id == id)
            .ok_or("unknown actor identity")?;
        let (mut types, return_ty) = match self {
            Self::LocalObservation { .. } | Self::CallStart(_) | Self::CallTake(_) => {
                unreachable!("special boundary returned above")
            }
            Self::StreamStart {
                message, target, ..
            } => {
                let handler = actor
                    .handlers
                    .iter()
                    .find(|handler| handler.message_id == *message && handler.stream.is_some())
                    .ok_or("stream start has no exact producer member")?;
                if !actor.admits_target(target) {
                    return Err("stream request addresses neither the handle nor the role".into());
                }
                (
                    vec![target.clone(), ResolvedTy::Tuple(handler.params.clone())],
                    ResolvedTy::Unit,
                )
            }
            Self::Close(_) => (vec![actor.handle_ty.clone()], actor.handle_ty.clone()),
            Self::SupervisorSpawn(_)
            | Self::SupervisorChild { .. }
            | Self::SupervisorAwaitRestart { .. }
            | Self::SupervisorPoolView { .. }
            | Self::SupervisorAwaitClosed(_)
            | Self::SupervisorRoleAwaitClosed { .. }
            | Self::SupervisorStop(_) => unreachable!("supervisor boundaries return above"),
            Self::AwaitClosed(_) => (vec![actor.handle_ty.clone()], ResolvedTy::Unit),
            Self::SelfHandle(_) => (Vec::new(), actor.handle_ty.clone()),
            // Deferred fields (D447) receive their value inside init.
            Self::Spawn(_) => (
                actor
                    .fields
                    .iter()
                    .filter(|field| !field.deferred)
                    .map(|field| field.ty.clone())
                    .collect::<Vec<_>>(),
                actor.handle_ty.clone(),
            ),
            Self::Submit {
                policy,
                message_ty,
                result_ty,
                ..
            } => {
                let source_ty = message_ty.to_ty();
                let (target, _, selected) = hew_types::actor_delivery::message_parts(&source_ty)
                    .ok_or("submission requires an exact message description")?;
                let target = ResolvedTy::from_ty(target).map_err(|error| error.to_string())?;
                if !actor.admits_target(&target)
                    || selected != *policy
                    || result_ty.to_ty()
                        != hew_types::actor_delivery::result_type(source_ty.clone())
                {
                    return Err(
                        "submission changes the checked target, policy or result contract".into(),
                    );
                }
                (vec![message_ty.clone()], result_ty.clone())
            }
        };
        if matches!(self, Self::Spawn(_)) {
            if let Some(init) = actor.init {
                let init = callable(init).ok_or("actor init lacks its callable")?;
                types.extend(init.params.iter().skip(1).map(|param| param.ty.clone()));
            }
        }
        Ok(consume(types, return_ty))
    }
}

/// The `Ok` and `Err` arms of a checked `Result`.
fn result_parts(ty: &ResolvedTy) -> Option<[&ResolvedTy; 2]> {
    let ResolvedTy::Named {
        builtin: Some(hew_types::BuiltinType::Result),
        args,
        ..
    } = ty
    else {
        return None;
    };
    let [ok, err] = args.as_slice() else {
        return None;
    };
    Some([ok, err])
}
