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
}

/// A checker-selected receive protocol member and its private body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemActorHandler {
    pub declaration: DefId,
    pub name: String,
    pub message_id: u32,
    pub callable: CallableId,
    /// Message payload fields. A stream producer's last field is the caller's
    /// `Sink<T>`, owned by the body until its turn ends.
    pub params: Vec<ResolvedTy>,
    pub return_ty: ResolvedTy,
    /// `receive gen fn`: the element type each `yield` sends to the sink.
    pub stream: Option<ResolvedTy>,
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

impl SemActor {
    /// Every private body entered with this actor's exclusive state seat.
    pub fn bodies(&self) -> impl Iterator<Item = CallableId> + '_ {
        self.init
            .into_iter()
            .chain(self.start)
            .chain(self.stop.iter().copied())
            .chain(self.methods.iter().copied())
            .chain(self.handlers.iter().map(|handler| handler.callable))
    }

    /// The receive protocol owns request parameter order and its full fallible
    /// reply type. Source and downstream verifiers consume this same signature.
    ///
    /// # Errors
    /// Rejects an unknown protocol member or an unresolved reply type.
    pub fn ask_signature(&self, message: u32) -> Result<crate::SemSignature, String> {
        let handler = self
            .handlers
            .iter()
            .find(|handler| handler.message_id == message)
            .ok_or("ask has no exact receive protocol member")?;
        if handler.return_ty == ResolvedTy::Unit {
            return Err("a unit receive handler produces a message description, not an ask".into());
        }
        let mut params = vec![crate::SemAbiParam {
            ty: self.handle_ty.clone(),
            passing: crate::SemParamPassing::Consume,
            caller_visible_projection: false,
        }];
        params.extend(handler.params.iter().map(|ty| crate::SemAbiParam {
            ty: ty.clone(),
            passing: crate::SemParamPassing::Consume,
            caller_visible_projection: false,
        }));
        let return_ty = ResolvedTy::from_ty(&hew_types::Ty::result(
            handler.return_ty.to_ty(),
            hew_types::Ty::ask_error(),
        ))
        .map_err(|error| error.to_string())?;
        Ok(crate::SemSignature { params, return_ty })
    }

    pub(crate) fn validate(&self, module: &SemModule) -> Result<(), String> {
        if module.actor(self.id) != Some(self) {
            return Err("actor descriptor is not at its canonical index".into());
        }
        let ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::LocalPid),
            args,
            ..
        } = &self.handle_ty
        else {
            return Err("actor descriptor requires a typed local handle".into());
        };
        if !matches!(args.as_slice(), [ty] if ty.nominal_instance().is_some_and(|instance|
            instance.nominal.declaration() == &self.declaration && instance.args.is_empty()))
        {
            return Err("actor handle refers to another declaration".into());
        }
        if self.state_ty
            != ResolvedTy::Tuple(self.fields.iter().map(|field| field.ty.clone()).collect())
        {
            return Err("actor state differs from its declaration-order fields".into());
        }
        let mut messages = std::collections::BTreeSet::new();
        let mut bodies = std::collections::BTreeSet::new();
        let hooks = self.start.iter().chain(&self.stop);
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
            } else if hooks.clone().any(|hook| *hook == body)
                && (callable.signature.return_ty != ResolvedTy::Unit
                    || callable.signature.params.len() != 1)
            {
                return Err("lifecycle hook takes no parameters and returns unit".into());
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
        if place.ty != field.ty
            || place.origin
                != (crate::PlaceOrigin::ActorState {
                    actor: actor.id,
                    state: receiver.value,
                    field: u32::try_from(index).map_err(|_| "actor field count exceeds u32")?,
                })
        {
            return Err("actor place differs from its declared state field".into());
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
            _ => return Err("a handler cannot end or extract an actor state seat".into()),
        }
        Ok(())
    })())
}

/// Actor boundary selected from an exact demanded protocol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActorOperation {
    Close(ActorId),
    AwaitClosed(ActorId),
    Spawn(ActorId),
    /// Start a stream producer turn: the request payload, whose last field is
    /// the consumer's sink, waits for mailbox capacity. A closed producer is a
    /// fault, so no consumer waits on a pipe nobody feeds.
    StreamStart {
        actor: ActorId,
        message: u32,
    },
    Submit {
        actor: ActorId,
        policy: hew_types::actor_delivery::SendPolicy,
        message_ty: ResolvedTy,
        result_ty: ResolvedTy,
    },
}

impl ActorOperation {
    /// Project the exact boundary ABI from the demanded actor protocol.
    ///
    /// # Errors
    /// Refuses missing actors, handlers, init bodies or incompatible sends.
    pub fn signature(
        &self,
        actors: &[SemActor],
        callable: impl Fn(crate::CallableId) -> Option<crate::SemSignature>,
    ) -> Result<crate::SemSignature, String> {
        let id = match self {
            Self::Spawn(id)
            | Self::Close(id)
            | Self::AwaitClosed(id)
            | Self::StreamStart { actor: id, .. }
            | Self::Submit { actor: id, .. } => *id,
        };
        let actor = actors
            .get(id.0 as usize)
            .filter(|actor| actor.id == id)
            .ok_or("unknown actor identity")?;
        let (mut types, return_ty) = match self {
            Self::StreamStart { message, .. } => {
                let handler = actor
                    .handlers
                    .iter()
                    .find(|handler| handler.message_id == *message && handler.stream.is_some())
                    .ok_or("stream start has no exact producer member")?;
                (
                    vec![
                        actor.handle_ty.clone(),
                        ResolvedTy::Tuple(handler.params.clone()),
                    ],
                    ResolvedTy::Unit,
                )
            }
            Self::Close(_) => (vec![actor.handle_ty.clone()], actor.handle_ty.clone()),
            Self::AwaitClosed(_) => (vec![actor.handle_ty.clone()], ResolvedTy::Unit),
            Self::Spawn(_) => (
                actor
                    .fields
                    .iter()
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
                if *target != actor.handle_ty.to_ty()
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
        Ok(crate::SemSignature {
            params: types
                .into_iter()
                .map(|ty| crate::SemAbiParam {
                    ty,
                    passing: crate::SemParamPassing::Consume,
                    caller_visible_projection: false,
                })
                .collect(),
            return_ty,
        })
    }
}
