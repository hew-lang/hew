//! Checker methods grouped by responsibility: places moves.
//! Split from `expressions.rs`: checker methods, part 1 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::branch_join::BranchArmExit;
use super::super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, EqRequirement, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

impl Checker {
    /// A spread reads each of the operand's elements and pushes an independent
    /// copy onto the new vector, so it admits exactly the element types the
    /// value class gives a copy path — the same answer `xs[i]`, a range slice
    /// and cloning iteration get.
    pub(super) fn refuse_uncopyable_spread_element(&mut self, elem_ty: &Ty, span: &Span) {
        let Some(blocker) = self.element_clone_blocker(elem_ty) else {
            return;
        };
        if let Some(param) = blocker.unbounded_param() {
            let param = param.to_string();
            self.report_unbounded_param_copy(&param, "a spread", span);
            return;
        }
        let blocker = blocker.concrete_text();
        let resolved = self.subst.resolve(elem_ty).materialize_literal_defaults();
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "E_ELEMENT_NO_COPY: spreading a `Vec<{elem}>` copies each element into the \
                 new vector, but {blocker} has no copy operation; use an owning removal such \
                 as `pop()` to move the elements out instead",
                elem = resolved.user_facing()
            ),
        );
    }

    pub(in crate::check) fn mark_expr_moved_if_non_copy(
        &mut self,
        expr: &Expr,
        span: &Span,
        ty: &Ty,
    ) {
        if !self.registry.implements_marker(ty, MarkerTrait::Copy)
            || self.reads_resource_handle_field(expr)
        {
            self.mark_expr_moved(expr, span);
        }
    }

    /// Whether `expr` reads a value carrying an `#[opaque]` handle out of a
    /// `#[resource]` record (D528). The handle itself classes as a bit copy so
    /// FFI calls can borrow it, but the enclosing resource's `close` releases
    /// it: reading it out by value anywhere but that `close` - directly, in a
    /// plain record below the resource or inside an `Option` - would leave two
    /// owners of one handle, so such a read is a transfer and the
    /// partial-consume rule decides it.
    pub(in crate::check) fn reads_resource_handle_field(&self, expr: &Expr) -> bool {
        self.expr_place(expr)
            .is_some_and(|(root, path)| self.resource_handle_owner(&root, &path).is_some())
    }

    /// The depth of the nearest `#[resource]` record on `path` whose `close`
    /// releases a handle the selected value carries.
    pub(super) fn resource_handle_owner(&self, root: &str, path: &[String]) -> Option<usize> {
        let binding = self.env.lookup_ref(root)?;
        let mut parent = self.subst.resolve(&binding.ty);
        let mut owner = None;
        for (depth, step) in path.iter().enumerate() {
            if matches!(&parent, Ty::Named { head, .. } if self.registry.is_resource(head.registry_key()))
            {
                owner = Some(depth);
            }
            let selected = match &parent {
                Ty::Tuple(items) => items.get(step.parse::<usize>().ok()?).cloned(),
                _ => self.project_named_field(&parent, step),
            };
            parent = self.subst.resolve(&selected?);
        }
        owner.filter(|_| self.carries_resource_handle(&parent, &mut HashSet::new()))
    }

    /// Whether a value of `ty` holds a marker-free `#[opaque]` handle that is
    /// not itself owned by a nested `#[resource]`.
    pub(super) fn carries_resource_handle(&self, ty: &Ty, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Ty::Named { head, args } => {
                let name = head.registry_key();
                if self.registry.is_resource(name) {
                    return false;
                }
                if crate::value_class::ClassDeclarations::declared_type(
                    &self.class_declarations(),
                    name,
                )
                .is_some_and(|declaration| {
                    declaration.is_opaque
                        && declaration.marker == crate::value_class::DeclarationMarker::None
                }) {
                    return true;
                }
                if args
                    .iter()
                    .any(|arg| self.carries_resource_handle(&self.subst.resolve(arg), visiting))
                {
                    return true;
                }
                let Some(members) = self.registry.member_types(name) else {
                    return false;
                };
                if !visiting.insert(name.to_string()) {
                    return false;
                }
                let carries = members
                    .to_vec()
                    .iter()
                    .any(|member| self.carries_resource_handle(member, visiting));
                visiting.remove(name);
                carries
            }
            Ty::Tuple(elements) => elements
                .iter()
                .any(|element| self.carries_resource_handle(element, visiting)),
            Ty::Array(element, _) | Ty::Slice(element) => {
                self.carries_resource_handle(element, visiting)
            }
            _ => false,
        }
    }

    /// Mark an identifier binding moved, unconditionally.
    ///
    /// Callers that have already PROVEN the value transfers ownership use this
    /// directly instead of [`Self::mark_expr_moved_if_non_copy`]. The `Copy`
    /// gate is not merely redundant there, it is wrong: an owned handle whose
    /// members are all scalars (`MonitorRef { ref_id: u64 }`) derives `Copy`
    /// structurally under a spelling that carries no negative impl, and the
    /// gate would then silently skip the move — leaving two owners of one
    /// registration. Ownership is decided by the transfer predicate, not by
    /// the representation of the bytes.
    /// Mark the PLACE an expression denotes as moved, unconditionally.
    ///
    /// The place is the root binding plus the projection steps taken from it,
    /// so a field transfer (`await a.take(h.sock)`) records that `h.sock`
    /// specifically is gone while `h`'s siblings stay usable. Consuming a
    /// projection used to no-op here, which is how a transferred field could be
    /// detached a second time through the same projection.
    ///
    /// Deliberately reports nothing: every site that consumes an expression
    /// also SYNTHESISES it first, and the read paths ([`Self::check_field_access`]
    /// and [`Self::synthesize_identifier`]) own the use-after-move diagnostic.
    /// Reporting here as well would double-diagnose one consuming use.
    pub(in crate::check) fn mark_expr_moved(&mut self, expr: &Expr, span: &Span) {
        let Some((root, path)) = self.expr_place(expr) else {
            return;
        };
        if self.reject_borrowed_consumption(expr, span) {
            return;
        }
        if !path.is_empty() {
            if self.reject_borrowed_consumption(expr, span)
                || self.reject_partial_place_consumption(&root, &path, span)
            {
                return;
            }
            self.env.mark_place_moved(&root, path, span.clone());
            return;
        }
        let released_at = self
            .env
            .lookup_ref(&root)
            .and_then(|binding| binding.released_at.clone());
        if let Some(released_at) = released_at {
            let mut error = TypeError::new(
                TypeErrorKind::UseAfterConsume,
                span.clone(),
                format!(
                    "cannot consume released resource `{root}`; its close obligation was already discharged"
                ),
            )
            .with_note(released_at, "resource was closed here");
            if let Some(source_module) = &self.current_module {
                error = error.with_source_module(source_module.clone());
            }
            self.errors.push(error);
        }
        self.env.mark_moved(&root, span.clone());
    }

    /// A selected field may move only when every enclosing value supports
    /// independent field ownership. The selected value's own cleanup contract
    /// does not prevent moving that entire value out of its plain parent.
    pub(in crate::check) fn reject_partial_place_consumption(
        &mut self,
        root: &str,
        path: &[String],
        span: &Span,
    ) -> bool {
        let Some(binding) = self.env.lookup_ref(root) else {
            return false;
        };
        let mut parent = self.subst.resolve(&binding.ty);
        for (depth, field) in path.iter().enumerate() {
            if depth == 0 && self.resource_close_owns_self_field(root, &parent) {
                let Some(selected) = self.project_named_field(&parent, field) else {
                    return false;
                };
                parent = self.subst.resolve(&selected);
                continue;
            }
            let Some(selected) = self.independent_record_or_tuple_field(&parent, field) else {
                if self.resource_handle_owner(root, path) == Some(depth) {
                    let record = Self::render_place(root, &path[..depth]);
                    let handed_out = Self::render_place(field, &path[depth + 1..]);
                    let resource = parent.user_facing().to_string();
                    let short = parent
                        .type_name()
                        .and_then(|name| name.rsplit('.').next())
                        .unwrap_or_default()
                        .to_string();
                    self.report_error_with_suggestions(
                        TypeErrorKind::OwnPartialConsume,
                        span,
                        format!(
                            "cannot read `{}` by value: `{resource}` releases the `#[opaque]` \
                             handle it carries in its `close`, so outside `close` it cannot be \
                             copied or moved out",
                            Self::render_place(root, path),
                        ),
                        vec![format!(
                            "destructure the resource to hand the handle out without running \
                             `close`: `let {short} {{ {field} }} = {record}; {handed_out}`"
                        )],
                    );
                    return true;
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::OwnPartialConsume,
                    span,
                    format!(
                        "cannot consume `{}` separately: enclosing type `{}` must remain whole",
                        Self::render_place(root, path),
                        parent.user_facing(),
                    ),
                    vec!["transfer the enclosing value whole to a consuming operation".to_string()],
                );
                return true;
            };
            parent = self.subst.resolve(&selected);
        }
        false
    }

    pub(super) fn project_named_field(&self, parent: &Ty, field: &str) -> Option<Ty> {
        let Ty::Named { head, args } = parent else {
            return None;
        };
        let name = head.registry_key();
        let definition = self.type_def_at(name)?;
        if definition.type_params.len() != args.len() {
            return None;
        }
        let substitutions = definition
            .type_params
            .iter()
            .cloned()
            .zip(args.iter().cloned())
            .collect();
        definition
            .fields
            .get(field)
            .map(|ty| ty.substitute_named_params_parallel(&substitutions))
    }

    pub(super) fn independent_record_or_tuple_field(&self, parent: &Ty, field: &str) -> Option<Ty> {
        match parent {
            Ty::Tuple(items) => items.get(field.parse::<usize>().ok()?).cloned(),
            Ty::Named { head, args } => {
                let name = head.registry_key();
                let declaration = crate::value_class::ClassDeclarations::declared_type(
                    &self.class_declarations(),
                    name,
                )?;
                if declaration.marker != crate::value_class::DeclarationMarker::None
                    || declaration.is_opaque
                {
                    return None;
                }
                let definition = self.type_def_at(name)?;
                if !matches!(definition.kind, TypeDefKind::Struct | TypeDefKind::Record)
                    || definition.type_params.len() != args.len()
                {
                    return None;
                }
                let substitutions = definition
                    .type_params
                    .iter()
                    .cloned()
                    .zip(args.iter().cloned())
                    .collect();
                Some(
                    definition
                        .fields
                        .get(field)?
                        .substitute_named_params_parallel(&substitutions),
                )
            }
            _ => None,
        }
    }

    /// Resolve an expression to a checker PLACE: the root binding name plus the
    /// projection steps taken from it.
    ///
    /// Tuple element access (`t.0`) parses as a field access with a numeric
    /// field name, so field steps cover both spellings.
    ///
    /// Returns `None` for anything that is not a projection chain rooted in a
    /// binding — indexing, calls, `this`. Those roots have no binding-level
    /// ownership slot to attach a fact to, so nothing is recorded for them
    /// rather than a guess being recorded; element-of-collection places are the
    /// known remaining hole and belong to the MIR half of this family.
    /// Indexed writes and mutating methods need a copy of every indexed parent.
    pub(in crate::check) fn reject_indexed_writable_borrow(&mut self, target: &Spanned<Expr>) {
        let mut parent = target;
        loop {
            if self
                .borrowed_element_index_reads
                .contains(&SpanKey::in_module(&parent.1, self.current_module_idx))
            {
                self.report_error(TypeErrorKind::OwnConsumeBorrowed, &parent.1,
                    "cannot update through a borrowed affine collection element; indexed writeback requires a semantic copy".into());
                return;
            }
            match &parent.0 {
                Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => parent = object,
                _ => return,
            }
        }
    }

    pub(in crate::check) fn expr_place(&self, expr: &Expr) -> Option<(String, PlacePath)> {
        match expr {
            Expr::Ident(name) => Some((name.to_string(), PlacePath::new())),
            Expr::FieldAccess { object, field } => {
                // `self.count` in an actor body denotes the state binding
                // `count`, so the place it names is rooted in that binding —
                // never in a binding called `self`, which does not exist here.
                if let Some(state_field) =
                    self.actor_self_state_field(&object.0, field.0.name.as_str())
                {
                    return Some((state_field.to_string(), PlacePath::new()));
                }
                let (root, mut path) = self.expr_place(&object.0)?;
                path.push(field.0.to_string());
                Some((root, path))
            }
            _ => None,
        }
    }

    /// The actor state field an `object.field` projection names when `object`
    /// is the actor receiver `self`, or `None` when it is an ordinary
    /// projection.
    ///
    /// An actor's state fields are bound as ordinary environment bindings for
    /// the whole body, which is what makes bare `count` work; `self` is the
    /// receiver that spells the same binding explicitly. Every site that
    /// matches on the projection's shape routes through here so the two
    /// spellings share one resolution, one mutability rule, and one lowering
    /// instead of growing a parallel receiver path.
    ///
    /// `self` is a real bound parameter on impl and trait methods, so an
    /// in-scope `self` binding means the projection is an ordinary field
    /// access on the receiver value and is left alone. A name that is not a
    /// declared state field is left alone too, so [`Self::check_field_access`]
    /// can report it against the actor.
    pub(in crate::check) fn actor_self_state_field<'a>(
        &self,
        object: &Expr,
        field: &'a str,
    ) -> Option<&'a str> {
        if !self.is_actor_self_receiver(object) {
            return None;
        }
        self.current_actor_fields
            .iter()
            .any(|f| f.name == field)
            .then_some(field)
    }

    /// Publish the receiver resolution for one `self.field` projection at
    /// `span`, the span of the whole projection.
    ///
    /// [`Self::actor_self_state_field`] is the predicate several checker sites
    /// read; this is the one place that writes the answer down. Every lowerer
    /// looks the projection up in
    /// [`TypeCheckOutput::actor_self_state_fields`](crate::TypeCheckOutput)
    /// instead of re-deciding it, so a projection is the receiver spelling in
    /// every backend or in none.
    pub(in crate::check) fn record_actor_self_state_field(&mut self, span: &Span) {
        self.actor_self_state_fields
            .insert(SpanKey::in_module(span, self.current_module_idx));
    }

    /// Whether an expression is the actor receiver `self`: the bare name,
    /// inside an actor body, with no `self` binding in scope to mean something
    /// else. The projected name may still not be a state field — that case
    /// belongs to [`Self::check_field_access`], which reports it against the
    /// actor rather than letting the receiver be synthesised as a value.
    pub(in crate::check) fn is_actor_self_receiver(&self, object: &Expr) -> bool {
        matches!(object, Expr::Ident(name) if name.name.as_str() == "self")
            && self.current_actor_type.is_some()
            && self.env.lookup_ref("self").is_none()
    }

    /// Render a place for diagnostics: `h.sock`, or plain `h` for the root.
    pub(in crate::check) fn render_place(root: &str, path: &[String]) -> String {
        std::iter::once(root)
            .chain(path.iter().map(String::as_str))
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Report a use of `root`'s place at `path` that collides with a place
    /// already consumed on this path, if it does.
    pub(in crate::check) fn report_place_use_after_move(
        &mut self,
        root: &str,
        path: &[String],
        span: &Span,
    ) {
        let Some((conflict, moved_path, moved_at)) = self.env.place_move_conflict(root, path)
        else {
            return;
        };
        // A place read only to project further into it is not a whole-value
        // use of itself, at any depth: `o.inner` inside `o.inner.ticket` names
        // an address, not the aggregate. Without this the partially-moved-root
        // rule would fire on every ancestor of a moved place and stack one
        // diagnostic per projection step on top of the real one.
        if conflict == PlaceConflict::WholeOfPartial && self.place_base_depth > 0 {
            return;
        }
        let place = Self::render_place(root, path);
        let moved_place = Self::render_place(root, &moved_path);
        let (message, suggestion) = match conflict {
            PlaceConflict::Exact => (
                format!("use of moved place `{place}`"),
                format!(
                    "`{place}` transferred its value away; re-initialise it \
                     (`{place} = ...`) before using it again"
                ),
            ),
            PlaceConflict::UnderMoved => (
                format!("use of `{place}`, which lives inside moved place `{moved_place}`"),
                format!(
                    "`{moved_place}` transferred its value away, taking `{place}` with it; \
                     read it before the transfer, or re-initialise `{moved_place}`"
                ),
            ),
            PlaceConflict::WholeOfPartial => (
                format!("use of `{place}` after its field `{moved_place}` was moved out"),
                format!(
                    "`{place}` is only partially owned here; use the fields that are still \
                     owned, or re-initialise `{moved_place}` before using `{place}` whole"
                ),
            ),
        };
        let mut error = TypeError::new(TypeErrorKind::UseAfterMove, span.clone(), message)
            .with_note(moved_at, "value was consumed here")
            .with_suggestion(suggestion);
        if let Some(source_module) = &self.current_module {
            error = error.with_source_module(source_module.clone());
        }
        self.errors.push(error);
    }

    /// Whether `ty` carries a value whose SOLE ownership crosses an actor
    /// message boundary — a substrate handle, or a user `#[resource]` /
    /// `#[linear]` declaration.
    ///
    /// The builtin half delegates to
    /// [`BuiltinType::transfers_ownership_across_actor_boundary`], the single
    /// authority HIR's intent stamping also reads. The nominal half is this
    /// checker's own: `#[resource]` and `#[linear]` types have exactly one
    /// ownership path, and MIR physically MOVES every message argument out of
    /// the caller frame (`lower_value_for_move`), so a later use of the caller
    /// binding is a genuine use-after-move. Without the nominal arm the caller
    /// kept its binding live and both frames consumed the one value.
    ///
    /// Copy-on-write values (`string`, `Vec`, plain records, tuples of them)
    /// are deliberately NOT here: the boundary copies them and both frames own
    /// their own copy, which is the language's default value semantics. A
    /// record that CONTAINS a resource is a different matter — see below.
    ///
    /// The walk is structural and total. It descends generic arguments, tuple
    /// elements, array/slice elements, AND registered record/enum member types,
    /// because containment is what decides ownership: `type Holder { socket:
    /// Socket }` transfers the socket just as surely as `(Socket, i64)` does,
    /// and sending one `Holder` twice gives the socket two drop paths. Skipping
    /// the named-member edge left exactly that hole open while the tuple edge
    /// was closed.
    pub(super) fn ty_contains_affine_actor_transfer(&self, ty: &Ty) -> bool {
        let mut visiting = std::collections::HashSet::new();
        self.ty_contains_affine_actor_transfer_guarded(ty, &mut visiting)
    }

    /// Recursion body for [`Self::ty_contains_affine_actor_transfer`].
    ///
    /// `visiting` makes the walk total over recursive type graphs
    /// (`type Node { next: Vec<Node> }`). Re-entering a name already on the
    /// stack contributes no NEW ownership edge, so it answers `false` — the
    /// neutral element of the `any(...)` disjunction — and the result is
    /// decided by the non-recursive members. This mirrors the recursion guard
    /// marker derivation already uses (`implements_marker_guarded`).
    pub(super) fn ty_contains_affine_actor_transfer_guarded(
        &self,
        ty: &Ty,
        visiting: &mut std::collections::HashSet<String>,
    ) -> bool {
        match ty {
            Ty::CancellationToken => true,
            Ty::Named { head, args, .. } => {
                let name = head.registry_key();
                let builtin = head.builtin();
                if builtin.is_some_and(BuiltinType::transfers_ownership_across_actor_boundary)
                    || Self::qualified_name_resolves_to_transferring_builtin(name)
                    || self.registry.is_resource(name)
                    || self.registry.is_linear(name)
                {
                    return true;
                }
                if args
                    .iter()
                    .any(|arg| self.ty_contains_affine_actor_transfer_guarded(arg, visiting))
                {
                    return true;
                }
                // A builtin carries no user member set to descend into, and its
                // ownership verdict is already decided above.
                if builtin.is_some() || !visiting.insert(name.to_string()) {
                    return false;
                }
                let members: Vec<Ty> = self
                    .registry
                    .member_types(name)
                    .map(<[Ty]>::to_vec)
                    .unwrap_or_default();
                let carries = members
                    .iter()
                    .any(|member| self.ty_contains_affine_actor_transfer_guarded(member, visiting));
                visiting.remove(name);
                carries
            }
            Ty::Tuple(elements) => elements
                .iter()
                .any(|element| self.ty_contains_affine_actor_transfer_guarded(element, visiting)),
            Ty::Array(element, _) | Ty::Slice(element) => {
                self.ty_contains_affine_actor_transfer_guarded(element, visiting)
            }
            _ => false,
        }
    }

    pub(in crate::check) fn enforce_actor_boundary_send(
        &mut self,
        expr: &Expr,
        move_span: &Span,
        error_span: &Span,
        ty: &Ty,
    ) {
        let ty = self.subst.resolve(ty);
        let boundary_ty = self.normalize_for_use(&ty);
        if !self.type_satisfies_trait_bound(&boundary_ty, "Send") {
            self.report_invalid_actor_send(&ty, error_span);
        }
        if self.ty_contains_affine_actor_transfer(&ty) {
            self.mark_affine_transfer_moved(expr, move_span);
        }
    }

    /// Mark the source of one affine boundary transfer moved.
    ///
    /// A handle sent directly names a place and marks straight through. A
    /// handle packed into a tuple or array literal at the call site names no
    /// place of its own, so the literal is transparent here: the mailbox takes
    /// the aggregate and with it each element, and the element binding is
    /// exactly what a later use must be refused against.
    pub(super) fn mark_affine_transfer_moved(&mut self, expr: &Expr, move_span: &Span) {
        match expr {
            Expr::Tuple(elements) => {
                for (element, span) in elements {
                    self.mark_affine_transfer_element(element, span);
                }
            }
            Expr::Array(elements) => {
                for element in elements {
                    let (element, span) = element.expr();
                    self.mark_affine_transfer_element(element, span);
                }
            }
            _ => self.mark_expr_moved(expr, move_span),
        }
    }

    /// One element of an aggregate literal crossing the boundary: descend only
    /// where the element's own checked type carries a transferring owner.
    pub(super) fn mark_affine_transfer_element(&mut self, expr: &Expr, span: &Span) {
        let key = super::SpanKey::in_module(span, self.current_module_idx);
        let Some(ty) = self.expr_types.get(&key).map(|ty| self.subst.resolve(ty)) else {
            return;
        };
        if self.ty_contains_affine_actor_transfer(&ty) {
            self.mark_affine_transfer_moved(expr, span);
        }
    }

    /// D524: an `#[on(crash)]` hook runs on the crashing incarnation's state,
    /// and a handler that faulted between consuming a copy-less field and
    /// storing its replacement left that seat empty. The hook may not read a
    /// field any body of the actor consumes.
    pub(super) fn reject_crash_hook_consumed_state_read(
        &mut self,
        binding: crate::env::TypeBindingId,
        span: &Span,
    ) {
        let Some(field) = self.crash_hook_consumed_fields.get(&binding) else {
            return;
        };
        let (consumer, consumed_at) = self.actor_consumed_state[field].clone();
        let mut error = TypeError::new(
            TypeErrorKind::UseAfterConsume,
            span.clone(),
            format!(
                "`#[on(crash)]` hook reads actor state `{field}`, which `{consumer}` consumes; \
                 a crash before `{consumer}` stores its replacement leaves `{field}` empty"
            ),
        )
        .with_note(consumed_at, format!("`{consumer}` consumes `{field}` here"))
        .with_suggestion(format!(
            "hold `{field}` as an `Option` and move it out with `{field}.take()`, which leaves \
             `None` in the field instead of consuming it"
        ));
        if let Some(source_module) = &self.current_module {
            error = error.with_source_module(source_module.clone());
        }
        self.errors.push(error);
    }

    /// Reject escaping a borrowed affine-handle parameter without `.clone()`.
    /// Under borrow-on-call semantics the callee does not own its Rc/Weak
    /// parameters, so returning or embedding one would mint an owner without
    /// incrementing the matching reference count.
    pub(in crate::check) fn warn_affine_param_escape(&mut self, fd: &FnDecl) {
        // Collect dangerous params: those with explicit Rc<_>/Weak<_> types.
        //
        // NOTE: generic type params (e.g. `x: T`) are NOT flagged here because
        // the danger only materialises when `T` is instantiated with `Rc<U>` at
        // a call site.  Definition-site checking would reject all generic
        // identity patterns (`fn id<T>(x: T) -> T { x }`) which are safe for
        // non-Rc types.  Call-site / monomorphisation-time checking is deferred
        // to a future slice.
        let dangerous_params: DangerousRcScope = fd
            .params
            .iter()
            .filter_map(|p| {
                let ty = self.resolve_type_expr(&p.ty);
                if matches!(
                    ty,
                    Ty::Named {
                        head: crate::TypeHead::Builtin(BuiltinType::Rc | BuiltinType::Weak),
                        ..
                    }
                ) {
                    return Some((p.name.to_string(), Some(p.name.to_string())));
                }
                None
            })
            .collect();
        if dangerous_params.is_empty() {
            return;
        }
        let mut scopes = vec![dangerous_params];
        self.scan_block_for_rc_param_return(&fd.body, &mut scopes);
    }

    /// Reject consuming a non-Copy by-value parameter into Rc-owned storage.
    /// A parameter is a borrow at the Hew call boundary; only an explicit
    /// clone or a freshly constructed value is an owned source.
    pub(in crate::check) fn reject_borrowed_parameter_consumption(
        &mut self,
        expr: &Expr,
        span: &Span,
        operation: &str,
    ) {
        let Expr::Ident(name) = expr else {
            return;
        };
        let Some(binding) = self.env.lookup_ref(name.name.as_str()) else {
            return;
        };
        let is_parameter = binding.is_param();
        let ty = self.subst.resolve(&binding.ty);
        if !is_parameter || self.registry.implements_marker(&ty, MarkerTrait::Copy) {
            return;
        }
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::BorrowedParamReturn,
            span: span.clone(),
            message: format!(
                "`{operation}` cannot consume borrowed parameter `{name}` of type `{}`",
                ty.user_facing()
            ),
            notes: vec![(
                span.clone(),
                "by-value function parameters are borrowed; the caller retains ownership"
                    .to_string(),
                self.current_module.clone(),
            )],
            suggestions: vec![format!(
                "use `{name}.clone()` to materialize an owned replacement"
            )],
            source_module: self.current_module.clone(),
        });
    }
}
