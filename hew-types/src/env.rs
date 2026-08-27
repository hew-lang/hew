//! Type environment with lexical scoping.
//!
//! The type environment tracks variable bindings across nested scopes,
//! supporting let/var declarations and shadowing.

use crate::ty::Ty;
use hew_parser::ast::{Expr, Span, Spanned};
use std::collections::HashMap;

/// Checker-local identity for a lexical binding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeBindingId(pub u32);

/// A projection path from a binding root down to a sub-place: one field name
/// per step.
///
/// Tuple element access (`t.0`) parses as a field access with a numeric field
/// name, so field names are the only step kind a place path has to carry.
/// The EMPTY path denotes the binding itself and is deliberately never stored
/// in [`Binding::moved_places`]: a whole-binding consume is
/// [`Binding::is_moved`], which every existing diagnostic already reads.
pub type PlacePath = Vec<String>;

/// One consumed strict sub-place of a binding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MovedPlace {
    /// Projection steps from the binding root. Never empty.
    pub path: PlacePath,
    /// Where the consuming use happened, for error reporting.
    pub moved_at: Span,
}

/// How a use of one place collides with an already-consumed place.
///
/// The relation mirrors MIR's partial-move state (`AliasedIntoAggregate` /
/// `partial_projection`) so the two authorities agree on what a place move
/// means: sibling-field moves are independent, a whole-value use of a
/// partially-moved root is refused, and any re-use of a moved place or of
/// storage under it is refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceConflict {
    /// The exact place was already consumed (`h.sock` after `h.sock`).
    Exact,
    /// The use lies UNDER a consumed place (`h.sock.fd` after `h.sock`).
    UnderMoved,
    /// The use is a whole-value use of a partially-moved place
    /// (`h` after `h.sock`).
    WholeOfPartial,
}

/// Whether `path` is `prefix` or extends it.
fn path_extends(path: &[String], prefix: &[String]) -> bool {
    path.len() >= prefix.len() && path[..prefix.len()] == *prefix
}

/// A binding in the type environment.
#[derive(Debug, Clone)]
pub struct Binding {
    /// Stable checker-local identity for this lexical binding.
    pub id: TypeBindingId,
    /// The type of the bound value
    pub ty: Ty,
    /// Whether the binding is mutable (var vs let)
    pub is_mutable: bool,
    /// Whether the value has been moved (e.g., sent to an actor)
    pub is_moved: bool,
    /// Where the move happened, for error reporting
    pub moved_at: Option<Span>,
    /// Strict sub-places of this binding consumed on the current path.
    ///
    /// Separate from `is_moved` because consumption is a fact about a PLACE,
    /// not about a name: `await a.take(h.sock)` transfers the socket out of
    /// `h` while leaving `h`'s other fields perfectly usable. Keyed by
    /// projection path so sibling fields stay independent.
    pub moved_places: Vec<MovedPlace>,
    /// Where an affine resource's explicit `close` discharged its implicit
    /// scope-exit obligation. Unlike a move, discharge leaves the handle bits
    /// readable so non-consuming operations can report a closed-handle error;
    /// a second consuming operation is still rejected.
    pub released_at: Option<Span>,
    /// Count of read accesses (incremented by lookup, decremented by `unmark_used`).
    pub read_count: u32,
    /// Whether the variable has been reassigned after initial definition
    pub is_written: bool,
    /// Source span of the definition, for diagnostics. None for synthetic bindings.
    pub def_span: Option<Span>,
    /// Source span used **only** for outer-scope shadowing classification
    /// (see `check_shadowing`), independent of `def_span`.
    ///
    /// Almost always mirrors `def_span`. The one deliberate exception is
    /// user-visible function/method/init/hook parameters: they keep
    /// `def_span: None` (so they stay exempt from the `UnusedVariable` /
    /// `NeverMutated` scope-exit lints in `pop_scope_with_warnings`, which
    /// only consider bindings with a `def_span`), while still needing a real
    /// span here so a nested local that shadows a parameter is classified as
    /// "shadows a user-visible outer binding" (warning) rather than
    /// "shadows a synthetic binding" (hard error) — see `define_param_with_span`.
    pub shadow_span: Option<Span>,
    /// Where this binding came from: a parameter, a user-written local, or a
    /// compiler-synthesised binding.
    ///
    /// Diagnostics that offer "declare it `var`" as a fix must consult this:
    /// on a by-value aggregate parameter `var` is itself rejected (see
    /// `reject_ineffective_mutable_value_param`), so suggesting it there
    /// routes the user into a construct the compiler refuses.
    pub origin: BindingOrigin,
}

/// What produced a [`Binding`].
///
/// Previously inferred from the `def_span` / `shadow_span` combination each
/// constructor happened to leave behind; that encoded two unrelated lint
/// exemptions rather than the binding's provenance.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingOrigin {
    /// A compiler-synthesised binding with no source declaration.
    Synthetic,
    /// A user-written `let` / `var` local, or a pattern-bound name.
    Local,
    /// A function / method / init / hook / closure parameter.
    Parameter,
    /// A method receiver parameter. Receivers have caller-visible write-back
    /// semantics and are exempt from ordinary by-value parameter guards.
    ReceiverParameter,
}

impl Binding {
    /// Whether this binding is a function parameter.
    #[must_use]
    pub fn is_param(&self) -> bool {
        matches!(
            self.origin,
            BindingOrigin::Parameter | BindingOrigin::ReceiverParameter
        )
    }

    /// Whether this binding is a method receiver parameter.
    #[must_use]
    pub fn is_receiver(&self) -> bool {
        self.origin == BindingOrigin::ReceiverParameter
    }
}

/// The move/release facts tracked per execution path for one binding.
///
/// These four fields are the ONLY flow-sensitive ownership state. `read_count`
/// and `is_written` are any-path lint accumulators (unused / never-mutated) and
/// deliberately stay outside the snapshot: restoring them per branch arm would
/// erase reads and writes that genuinely happened.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OwnershipState {
    /// Whether the value has been moved on this path.
    pub is_moved: bool,
    /// Where the move happened, for error reporting.
    pub moved_at: Option<Span>,
    /// Strict sub-places consumed on this path.
    pub moved_places: Vec<MovedPlace>,
    /// Where the close obligation was discharged on this path.
    pub released_at: Option<Span>,
}

/// Ownership state of every visible binding at one point in the control flow.
///
/// Captured at a branch entry and at each arm's exit so alternative-execution
/// constructs can restore per arm and merge at the join, instead of threading
/// one arm's exit state into the next arm.
#[derive(Debug, Clone, Default)]
pub struct OwnershipSnapshot {
    states: HashMap<TypeBindingId, OwnershipState>,
}

impl OwnershipSnapshot {
    /// The recorded state for `id`, if the binding existed when the snapshot
    /// was taken.
    #[must_use]
    pub fn get(&self, id: TypeBindingId) -> Option<&OwnershipState> {
        self.states.get(&id)
    }

    /// Number of bindings recorded.
    #[must_use]
    pub fn len(&self) -> usize {
        self.states.len()
    }

    /// Whether no bindings were visible when this snapshot was taken.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.states.is_empty()
    }
}

/// A diagnostic about a binding discovered at scope exit.
#[derive(Debug)]
pub struct ScopeWarning {
    /// The variable name
    pub name: String,
    /// Source span of the definition
    pub span: Span,
    /// What kind of warning
    pub kind: ScopeWarningKind,
    /// The bound value's type, so the diagnostic layer can suppress the
    /// unused-binding lint for RAII handle types (their drop is the use).
    pub ty: Ty,
}

/// The kind of scope-level warning.
#[derive(Debug)]
pub enum ScopeWarningKind {
    /// Variable defined but never read
    Unused,
    /// Declared `var` but never reassigned — could be `let`
    NeverMutated,
}

/// Lexically-scoped type environment.
///
/// Maintains a stack of scopes, where each scope maps names to bindings.
/// Lookup walks from innermost to outermost scope.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    scopes: Vec<HashMap<String, Binding>>,
    /// Deferred bodies registered in each lexical scope, parallel to `scopes`.
    deferred_scopes: Vec<Vec<Spanned<Expr>>>,
    /// Lexical scope floors for active loops, paired with their optional labels.
    loop_scope_floors: Vec<(Option<String>, usize)>,
    next_binding_id: u32,
}

impl TypeEnv {
    /// Create a new empty environment with one scope.
    #[must_use]
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            deferred_scopes: vec![Vec::new()],
            loop_scope_floors: Vec::new(),
            next_binding_id: 0,
        }
    }

    fn next_binding_id(&mut self) -> TypeBindingId {
        let id = TypeBindingId(self.next_binding_id);
        self.next_binding_id = self
            .next_binding_id
            .checked_add(1)
            .expect("checker binding id overflow");
        id
    }

    /// Push a new scope onto the stack.
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.deferred_scopes.push(Vec::new());
    }

    /// Pop the current scope from the stack.
    ///
    /// # Panics
    /// Panics if there are no scopes to pop.
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("cannot pop empty scope stack");
        self.deferred_scopes
            .pop()
            .expect("cannot pop empty defer-scope stack");
    }

    /// Register a deferred body in the current lexical scope.
    ///
    /// Returns `false` only if the environment's scope stacks are inconsistent;
    /// callers turn that invariant failure into a fail-closed diagnostic.
    pub fn register_defer(&mut self, body: Spanned<Expr>) -> bool {
        let Some(scope) = self.deferred_scopes.last_mut() else {
            return false;
        };
        scope.push(body);
        true
    }

    /// Deferred bodies materialized by the current scope's normal exit, in
    /// runtime order (innermost registration first).
    #[must_use]
    pub fn current_scope_defers(&self) -> Vec<Spanned<Expr>> {
        self.deferred_scopes
            .last()
            .into_iter()
            .flat_map(|scope| scope.iter().rev().cloned())
            .collect()
    }

    /// Deferred bodies materialized by a function-return edge, in runtime
    /// order: innermost scope first, LIFO within each scope.
    #[must_use]
    pub fn return_edge_defers(&self) -> Vec<Spanned<Expr>> {
        self.deferred_scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev().cloned())
            .collect()
    }

    /// Record the lexical scope depth immediately before a loop body opens.
    pub fn enter_loop(&mut self, label: Option<&str>) {
        self.loop_scope_floors
            .push((label.map(str::to_string), self.deferred_scopes.len()));
    }

    /// Retire the innermost loop boundary.
    ///
    /// # Panics
    ///
    /// Panics if no loop boundary is active, which indicates an unbalanced
    /// checker traversal.
    pub fn exit_loop(&mut self) {
        self.loop_scope_floors
            .pop()
            .expect("cannot exit loop with no active loop boundary");
    }

    /// Deferred bodies materialized by a `break` or `continue` edge.
    ///
    /// An unlabeled edge targets the innermost loop. A labeled edge targets the
    /// nearest active loop carrying that label. `None` fails closed when the
    /// source checker cannot identify the loop boundary.
    #[must_use]
    pub fn loop_edge_defers(&self, label: Option<&str>) -> Option<Vec<Spanned<Expr>>> {
        let (_, floor) = self
            .loop_scope_floors
            .iter()
            .rev()
            .find(|(candidate, _)| label.is_none() || candidate.as_deref() == label)?;
        Some(
            self.deferred_scopes[*floor..]
                .iter()
                .rev()
                .flat_map(|scope| scope.iter().rev().cloned())
                .collect(),
        )
    }

    /// Define a variable in the current scope (synthetic, no source span — not warned about).
    pub fn define(&mut self, name: String, ty: Ty, is_mutable: bool) {
        let id = self.next_binding_id();
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                Binding {
                    id,
                    ty,
                    is_mutable,
                    is_moved: false,
                    moved_at: None,
                    moved_places: Vec::new(),
                    released_at: None,
                    read_count: 1, // synthetic bindings are always "used"
                    is_written: false,
                    def_span: None,
                    shadow_span: None,
                    origin: BindingOrigin::Synthetic,
                },
            );
        }
    }

    /// Define a user-visible variable with a source span for diagnostics.
    pub fn define_with_span(&mut self, name: String, ty: Ty, is_mutable: bool, span: Span) {
        let id = self.next_binding_id();
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                Binding {
                    id,
                    ty,
                    is_mutable,
                    is_moved: false,
                    moved_at: None,
                    moved_places: Vec::new(),
                    released_at: None,
                    read_count: 0,
                    is_written: false,
                    def_span: Some(span.clone()),
                    shadow_span: Some(span),
                    origin: BindingOrigin::Local,
                },
            );
        }
    }

    /// Define a user-visible function/method/init/hook **parameter** with a
    /// source span for outer-scope shadowing classification only.
    ///
    /// Deliberately keeps `def_span: None` (like the fully-synthetic
    /// `define`): parameters are exempt from the `UnusedVariable` /
    /// `NeverMutated` scope-exit lints, which key off `def_span`, matching
    /// existing behaviour before and after this constructor's introduction.
    /// Unlike `define`, `shadow_span` is populated so a nested local that
    /// shadows the parameter name is downgraded from hard error to warning,
    /// the same treatment given to shadowing a user-declared local variable.
    pub fn define_param_with_span(&mut self, name: String, ty: Ty, is_mutable: bool, span: Span) {
        self.define_param_with_span_and_origin(
            name,
            ty,
            is_mutable,
            span,
            BindingOrigin::Parameter,
        );
    }

    /// Define a method receiver parameter, preserving its caller-visible
    /// write-back provenance for mutation diagnostics.
    pub fn define_receiver_param_with_span(
        &mut self,
        name: String,
        ty: Ty,
        is_mutable: bool,
        span: Span,
    ) {
        self.define_param_with_span_and_origin(
            name,
            ty,
            is_mutable,
            span,
            BindingOrigin::ReceiverParameter,
        );
    }

    fn define_param_with_span_and_origin(
        &mut self,
        name: String,
        ty: Ty,
        is_mutable: bool,
        span: Span,
        origin: BindingOrigin,
    ) {
        let id = self.next_binding_id();
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                Binding {
                    id,
                    ty,
                    is_mutable,
                    is_moved: false,
                    moved_at: None,
                    moved_places: Vec::new(),
                    released_at: None,
                    read_count: 1, // exempt from unused-variable lint, like `define`
                    is_written: false,
                    def_span: None,
                    shadow_span: Some(span),
                    origin,
                },
            );
        }
    }

    /// Mark a variable as moved, returning `true` if found.
    pub fn mark_moved(&mut self, name: &str, span: Span) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.is_moved = true;
                binding.moved_at = Some(span);
                return true;
            }
        }
        false
    }

    /// Record a strict sub-place of `name` as consumed, returning `true` if the
    /// binding was found.
    ///
    /// `path` must be non-empty; a whole-binding consume is [`Self::mark_moved`].
    /// Re-recording an already-consumed place keeps the FIRST consume site,
    /// which is the one a diagnostic should point at.
    pub fn mark_place_moved(&mut self, name: &str, path: PlacePath, span: Span) -> bool {
        debug_assert!(!path.is_empty(), "empty place path is `mark_moved`");
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                if !binding.moved_places.iter().any(|m| m.path == path) {
                    binding.moved_places.push(MovedPlace {
                        path,
                        moved_at: span,
                    });
                }
                return true;
            }
        }
        false
    }

    /// The first already-consumed place of `name` that collides with a use of
    /// `path`, together with the colliding place and its consume site.
    ///
    /// The empty `path` asks the whole-value question: it collides with every
    /// consumed sub-place, because using a partially-moved aggregate by value
    /// would hand a second owner the storage that already moved out.
    #[must_use]
    pub fn place_move_conflict(
        &self,
        name: &str,
        path: &[String],
    ) -> Option<(PlaceConflict, PlacePath, Span)> {
        let binding = self.lookup_ref(name)?;
        binding.moved_places.iter().find_map(|moved| {
            let kind = if moved.path == path {
                PlaceConflict::Exact
            } else if path_extends(path, &moved.path) {
                PlaceConflict::UnderMoved
            } else if path_extends(&moved.path, path) {
                PlaceConflict::WholeOfPartial
            } else {
                // Disjoint siblings: independent storage, independent owners.
                return None;
            };
            Some((kind, moved.path.clone(), moved.moved_at.clone()))
        })
    }

    /// Plug the hole a consuming use left: re-initialising `name` at `path`
    /// gives that storage a fresh owner, discharging every consumed place at or
    /// under it.
    ///
    /// The empty `path` is a whole-binding re-initialisation and additionally
    /// clears `is_moved`.
    pub fn reinit_place(&mut self, name: &str, path: &[String]) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding
                    .moved_places
                    .retain(|moved| !path_extends(&moved.path, path));
                if path.is_empty() {
                    binding.is_moved = false;
                    binding.moved_at = None;
                }
                return;
            }
        }
    }

    /// Discharge one affine resource without making its closed handle bits
    /// unreadable. Returns the earlier discharge site when this binding was
    /// already released.
    pub fn mark_released(&mut self, name: &str, span: Span) -> Option<Option<Span>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                let prior = binding.released_at.clone();
                binding.released_at = Some(span);
                return Some(prior);
            }
        }
        None
    }

    /// Restore a binding after a validated receiver-identity method result is
    /// discarded in place. The method temporarily transfers the one owner
    /// through `consuming self` and returns that exact owner to this binding.
    pub fn unmark_moved(&mut self, name: &str) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.is_moved = false;
                binding.moved_at = None;
                return true;
            }
        }
        false
    }

    /// Capture the move/release state of every currently visible binding.
    ///
    /// Keyed by [`TypeBindingId`], so a shadowing re-`define` (which mints a
    /// fresh id) is never confused with the binding it shadows.
    #[must_use]
    pub fn ownership_snapshot(&self) -> OwnershipSnapshot {
        let mut states = HashMap::new();
        for scope in &self.scopes {
            for binding in scope.values() {
                states.insert(
                    binding.id,
                    OwnershipState {
                        is_moved: binding.is_moved,
                        moved_at: binding.moved_at.clone(),
                        moved_places: binding.moved_places.clone(),
                        released_at: binding.released_at.clone(),
                    },
                );
            }
        }
        OwnershipSnapshot { states }
    }

    /// Reset every binding recorded in `snap` back to its snapshotted
    /// move/release state.
    ///
    /// Bindings created after the snapshot are absent from it and are left
    /// alone, as are lint accumulators on every binding.
    pub fn restore_ownership(&mut self, snap: &OwnershipSnapshot) {
        Self::apply_ownership(&mut self.scopes, &snap.states);
    }

    /// Join alternative execution paths: for every binding that existed at
    /// `entry`, take the union of its state across `exits`.
    ///
    /// Union (may-analysis) is the sound direction for a consume: a value moved
    /// on any path is not usable after the join. Callers pass one exit snapshot
    /// per path that reaches the join — including the implicit fall-through
    /// path of an `if` without an `else`.
    pub fn merge_ownership(&mut self, entry: &OwnershipSnapshot, exits: &[OwnershipSnapshot]) {
        let mut merged: HashMap<TypeBindingId, OwnershipState> =
            HashMap::with_capacity(entry.states.len());
        for (id, entry_state) in &entry.states {
            let mut state = entry_state.clone();
            for exit in exits {
                let Some(exit_state) = exit.states.get(id) else {
                    continue;
                };
                if exit_state.is_moved && !state.is_moved {
                    state.is_moved = true;
                    state.moved_at.clone_from(&exit_state.moved_at);
                }
                if state.moved_at.is_none() {
                    state.moved_at.clone_from(&exit_state.moved_at);
                }
                // Place moves union the same monotone way whole-binding moves
                // do: a place consumed on ANY path is not usable after the
                // join, and a place re-initialised on only SOME paths still
                // carries the obligation. Union only ever ADDS facts, which is
                // what keeps the join structurally sound.
                for place in &exit_state.moved_places {
                    if !state.moved_places.iter().any(|m| m.path == place.path) {
                        state.moved_places.push(place.clone());
                    }
                }
                if state.released_at.is_none() {
                    state.released_at.clone_from(&exit_state.released_at);
                }
            }
            merged.insert(*id, state);
        }
        Self::apply_ownership(&mut self.scopes, &merged);
    }

    fn apply_ownership(
        scopes: &mut [HashMap<String, Binding>],
        states: &HashMap<TypeBindingId, OwnershipState>,
    ) {
        for scope in scopes.iter_mut() {
            for binding in scope.values_mut() {
                if let Some(state) = states.get(&binding.id) {
                    binding.is_moved = state.is_moved;
                    binding.moved_at.clone_from(&state.moved_at);
                    binding.moved_places.clone_from(&state.moved_places);
                    binding.released_at.clone_from(&state.released_at);
                }
            }
        }
    }

    /// Mark a variable as written (reassigned after definition).
    pub fn mark_written(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.is_written = true;
                return;
            }
        }
    }

    /// Pop the current scope, returning diagnostics about unused/unmutated bindings.
    #[expect(
        clippy::missing_panics_doc,
        reason = "internal API, panics on invariant violation"
    )]
    pub fn pop_scope_with_warnings(&mut self) -> Vec<ScopeWarning> {
        let scope = self.scopes.pop().expect("cannot pop empty scope stack");
        self.deferred_scopes
            .pop()
            .expect("cannot pop empty defer-scope stack");
        let mut warnings = Vec::new();
        for (name, binding) in &scope {
            let Some(span) = &binding.def_span else {
                continue; // synthetic binding (self, params without spans, etc.)
            };
            if name.starts_with('_') {
                continue; // convention: _ prefix means intentionally unused
            }
            if binding.read_count == 0 {
                warnings.push(ScopeWarning {
                    name: name.clone(),
                    span: span.clone(),
                    kind: ScopeWarningKind::Unused,
                    ty: binding.ty.clone(),
                });
            } else if binding.is_mutable && !binding.is_written {
                warnings.push(ScopeWarning {
                    name: name.clone(),
                    span: span.clone(),
                    kind: ScopeWarningKind::NeverMutated,
                    ty: binding.ty.clone(),
                });
            }
        }
        warnings
    }

    /// Look up a variable by name, marking it as used.
    #[must_use]
    pub fn lookup(&mut self, name: &str) -> Option<&Binding> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.read_count += 1;
                return Some(binding);
            }
        }
        None
    }

    /// Look up a variable by name without marking it as used.
    #[must_use]
    pub fn lookup_ref(&self, name: &str) -> Option<&Binding> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Some(binding);
            }
        }
        None
    }

    /// Read-only lookup with the defining lexical-scope index.
    ///
    /// Import bindings live in the persistent outer scope (index zero), while
    /// locals and parameters live in a body scope. Callers that validate an
    /// imported namespace must preserve that ordinary local-shadowing rule.
    #[must_use]
    pub fn lookup_ref_with_depth(&self, name: &str) -> Option<(usize, &Binding)> {
        self.scopes
            .iter()
            .enumerate()
            .rev()
            .find_map(|(depth, scope)| scope.get(name).map(|binding| (depth, binding)))
    }

    /// Look up a variable by name, returning the scope depth where it was found. Marks as used.
    #[must_use]
    pub fn lookup_with_depth(&mut self, name: &str) -> Option<(usize, &Binding)> {
        for (i, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.read_count += 1;
                return Some((i, binding));
            }
        }
        None
    }

    /// Check if a variable is defined in the current (innermost) scope only.
    #[must_use]
    pub fn is_defined_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .is_some_and(|scope| scope.contains_key(name))
    }

    /// Get the depth of the scope stack.
    #[must_use]
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// Check if `name` already exists in the current (innermost) scope.
    ///
    /// Returns `Some(Some(span))` when the binding has a shadow-classification
    /// span, `Some(None)` when found but synthetic, or `None` when the name
    /// is not bound in the current scope.
    #[must_use]
    pub fn find_in_current_scope(&self, name: &str) -> Option<Option<Span>> {
        self.scopes
            .last()
            .and_then(|scope| scope.get(name))
            .map(|b| b.shadow_span.clone())
    }

    /// Check if a variable name exists in any outer scope (not the current one).
    ///
    /// Returns `Some(Some(span))` when the binding has a shadow-classification
    /// span, `Some(None)` when found but synthetic (e.g. actor fields), or
    /// `None` when the name is not bound in any outer scope.
    #[must_use]
    pub fn find_in_outer_scope(&self, name: &str) -> Option<Option<Span>> {
        // Skip the last (current) scope and check all outer scopes
        for scope in self.scopes.iter().rev().skip(1) {
            if let Some(binding) = scope.get(name) {
                return Some(binding.shadow_span.clone());
            }
        }
        None
    }

    /// Return all variable names visible in the current scope stack.
    pub fn all_names(&self) -> impl Iterator<Item = &str> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.keys().map(String::as_str))
    }

    /// Yield `(name, binding id)` for every binding in the innermost (current)
    /// scope only.
    ///
    /// The binding id is part of the pair so a shadowing re-`define` (which
    /// replaces the entry with a fresh id) is observable: comparing two
    /// snapshots by `(name, id)` distinguishes "same binding untouched" from
    /// "rebound in place". Used to compute the exact set of names a pattern
    /// branch introduced (see `bind_pattern`'s or-pattern arm).
    pub fn current_scope_bindings(&self) -> impl Iterator<Item = (&str, TypeBindingId)> {
        self.scopes
            .last()
            .into_iter()
            .flat_map(|scope| scope.iter().map(|(name, b)| (name.as_str(), b.id)))
    }

    /// Undo the `is_used` mark on a variable (used for plain assignment LHS).
    /// Decrements the read count so that write-only variables are still detected.
    pub fn unmark_used(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                // Decrement the read count to undo the lookup that resolved the
                // assignment target. If the variable was genuinely read before
                // (read_count > 1), the count stays positive and the variable
                // remains "used". For write-only variables, count drops to 0.
                binding.read_count = binding.read_count.saturating_sub(1);
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define_and_lookup() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        let binding = env.lookup("x").unwrap();
        assert_eq!(binding.ty, Ty::I32);
        assert!(!binding.is_mutable);
    }

    #[test]
    fn test_shadowing() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        env.push_scope();
        env.define("x".to_string(), Ty::Bool, true);

        let binding = env.lookup("x").unwrap();
        assert_eq!(binding.ty, Ty::Bool);
        assert!(binding.is_mutable);

        env.pop_scope();
        let binding = env.lookup("x").unwrap();
        assert_eq!(binding.ty, Ty::I32);
    }

    #[test]
    fn test_lookup_outer_scope() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        env.push_scope();
        env.define("y".to_string(), Ty::Bool, false);

        // Can still find x from outer scope
        assert!(env.lookup("x").is_some());
        assert!(env.lookup("y").is_some());
    }

    #[test]
    fn test_undefined() {
        let mut env = TypeEnv::new();
        assert!(env.lookup("x").is_none());
    }

    #[test]
    fn test_is_defined_in_current_scope() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        env.push_scope();

        assert!(!env.is_defined_in_current_scope("x"));
        env.define("x".to_string(), Ty::Bool, false);
        assert!(env.is_defined_in_current_scope("x"));
    }

    #[test]
    fn test_mark_moved() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::String, false);
        assert!(!env.lookup("x").unwrap().is_moved);

        assert!(env.mark_moved("x", 10..20));
        let binding = env.lookup("x").unwrap();
        assert!(binding.is_moved);
        assert_eq!(binding.moved_at, Some(10..20));
    }

    #[test]
    fn test_mark_moved_not_found() {
        let mut env = TypeEnv::new();
        assert!(!env.mark_moved("x", 0..1));
    }

    #[test]
    fn test_new_binding_not_moved() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        let binding = env.lookup("x").unwrap();
        assert!(!binding.is_moved);
        assert_eq!(binding.moved_at, None);
    }

    #[test]
    fn test_define_with_span_tracks_usage() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, false, 0..5);
        // Not yet used
        let b = env.lookup_ref("x").unwrap();
        assert_eq!(b.read_count, 0);
        assert!(!b.is_written);
        assert_eq!(b.def_span, Some(0..5));

        // lookup() marks as used
        let b = env.lookup("x").unwrap();
        assert!(b.read_count > 0);
    }

    #[test]
    fn test_synthetic_define_always_used() {
        let mut env = TypeEnv::new();
        env.define("self_".to_string(), Ty::I32, false);
        let b = env.lookup_ref("self_").unwrap();
        assert!(b.read_count > 0, "synthetic bindings should start as used");
        assert!(b.def_span.is_none());
    }

    #[test]
    fn restore_ownership_undoes_a_move_taken_after_the_snapshot() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::String, false, 0..1);
        let entry = env.ownership_snapshot();

        assert!(env.mark_moved("x", 10..20));
        assert!(env.mark_released("x", 10..20).is_some());
        env.restore_ownership(&entry);

        let b = env.lookup_ref("x").unwrap();
        assert!(!b.is_moved);
        assert_eq!(b.moved_at, None);
        assert_eq!(b.released_at, None);
    }

    #[test]
    fn restore_ownership_preserves_read_and_write_lint_state() {
        // R2: the snapshot carries ownership only. A read or a write inside a
        // branch arm must survive the restore, or the unused / never-mutated
        // lints regress silently.
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::String, true, 0..1);
        let entry = env.ownership_snapshot();

        let _ = env.lookup("x");
        env.mark_written("x");
        env.restore_ownership(&entry);

        let b = env.lookup_ref("x").unwrap();
        assert_eq!(b.read_count, 1);
        assert!(b.is_written);
    }

    #[test]
    fn restore_ownership_leaves_bindings_created_after_the_snapshot_alone() {
        let mut env = TypeEnv::new();
        let entry = env.ownership_snapshot();
        env.define_with_span("later".to_string(), Ty::String, false, 0..1);
        assert!(env.mark_moved("later", 5..6));

        env.restore_ownership(&entry);

        assert!(env.lookup_ref("later").unwrap().is_moved);
    }

    #[test]
    fn restore_ownership_does_not_reach_a_shadowing_rebinding() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::String, false, 0..1);
        assert!(env.mark_moved("x", 2..3));
        let entry = env.ownership_snapshot();

        env.push_scope();
        env.define_with_span("x".to_string(), Ty::String, false, 4..5);
        env.restore_ownership(&entry);

        // The inner `x` is a distinct binding id and keeps its own live state.
        assert!(!env.lookup_ref("x").unwrap().is_moved);
        env.pop_scope();
        assert!(env.lookup_ref("x").unwrap().is_moved);
    }

    #[test]
    fn merge_ownership_unions_a_move_from_any_single_path() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::String, false, 0..1);
        let entry = env.ownership_snapshot();

        env.restore_ownership(&entry);
        let live_exit = env.ownership_snapshot();

        env.restore_ownership(&entry);
        assert!(env.mark_moved("x", 10..20));
        let moved_exit = env.ownership_snapshot();

        env.merge_ownership(&entry, &[live_exit, moved_exit]);

        let b = env.lookup_ref("x").unwrap();
        assert!(b.is_moved, "moved on one path means moved after the join");
        assert_eq!(b.moved_at, Some(10..20));
    }

    #[test]
    fn merge_ownership_unions_move_and_release_from_different_paths() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::String, false, 0..1);
        let entry = env.ownership_snapshot();

        env.restore_ownership(&entry);
        assert!(env.mark_moved("x", 10..20));
        let moved_exit = env.ownership_snapshot();

        env.restore_ownership(&entry);
        assert!(env.mark_released("x", 30..40).is_some());
        let released_exit = env.ownership_snapshot();

        env.merge_ownership(&entry, &[moved_exit, released_exit]);

        let b = env.lookup_ref("x").unwrap();
        assert!(b.is_moved);
        assert_eq!(b.moved_at, Some(10..20));
        assert_eq!(b.released_at, Some(30..40));
    }

    #[test]
    fn merge_ownership_over_only_live_paths_leaves_the_binding_live() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::String, false, 0..1);
        let entry = env.ownership_snapshot();

        env.restore_ownership(&entry);
        assert!(env.mark_moved("x", 10..20));
        let diverging_exit = env.ownership_snapshot();

        env.restore_ownership(&entry);
        let live_exit = env.ownership_snapshot();

        // The diverging arm's exit is excluded by the caller; it must not leak.
        env.merge_ownership(&entry, &[live_exit]);
        assert!(!env.lookup_ref("x").unwrap().is_moved);

        // Including it flips the verdict, proving the exclusion is what matters.
        env.merge_ownership(&entry, &[diverging_exit]);
        assert!(env.lookup_ref("x").unwrap().is_moved);
    }

    #[test]
    fn merge_ownership_ignores_bindings_that_did_not_exist_at_entry() {
        let mut env = TypeEnv::new();
        let entry = env.ownership_snapshot();
        env.define_with_span("arm_local".to_string(), Ty::String, false, 0..1);
        assert!(env.mark_moved("arm_local", 5..6));
        let exit = env.ownership_snapshot();

        env.merge_ownership(&entry, &[exit]);

        assert!(env.lookup_ref("arm_local").unwrap().is_moved);
    }

    #[test]
    fn test_mark_written() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        assert!(!env.lookup_ref("x").unwrap().is_written);
        env.mark_written("x");
        assert!(env.lookup_ref("x").unwrap().is_written);
    }

    #[test]
    fn test_unmark_used_on_first_assignment() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        // Simulate what synthesize does during assignment target resolution
        let _ = env.lookup("x"); // read_count = 1
        assert!(env.lookup_ref("x").unwrap().read_count > 0);
        // unmark_used should reverse it for first assignment
        env.unmark_used("x");
        assert_eq!(env.lookup_ref("x").unwrap().read_count, 0);
    }

    #[test]
    fn test_unmark_used_preserves_after_genuine_read() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        // First: variable is genuinely used somewhere
        let _ = env.lookup("x"); // read_count = 1
                                 // Then: an assignment target lookup + unmark (simulating `x = 1`)
        let _ = env.lookup("x"); // read_count = 2
        env.unmark_used("x"); // read_count = 1
        assert!(
            env.lookup_ref("x").unwrap().read_count > 0,
            "unmark_used should not undo usage when there was a genuine read"
        );
    }

    #[test]
    fn test_lookup_ref_does_not_mark_used() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, false, 0..5);
        let _ = env.lookup_ref("x");
        assert_eq!(
            env.lookup_ref("x").unwrap().read_count,
            0,
            "lookup_ref should not mark as used"
        );
    }

    #[test]
    fn test_pop_scope_warns_unused() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.define_with_span("unused".to_string(), Ty::I32, false, 0..6);
        let warnings = env.pop_scope_with_warnings();
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].name, "unused");
        assert!(matches!(warnings[0].kind, ScopeWarningKind::Unused));
    }

    #[test]
    fn test_pop_scope_warns_never_mutated() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        let _ = env.lookup("x"); // mark used but never written
        let warnings = env.pop_scope_with_warnings();
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].name, "x");
        assert!(matches!(warnings[0].kind, ScopeWarningKind::NeverMutated));
    }

    #[test]
    fn test_pop_scope_no_warn_if_used_and_written() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        let _ = env.lookup("x"); // used
        env.mark_written("x"); // written
        let warnings = env.pop_scope_with_warnings();
        assert!(warnings.is_empty(), "no warning for used+written var");
    }

    #[test]
    fn test_pop_scope_no_warn_underscore_prefix() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.define_with_span("_ignored".to_string(), Ty::I32, false, 0..8);
        let warnings = env.pop_scope_with_warnings();
        assert!(
            warnings.is_empty(),
            "_ prefix should suppress unused warning"
        );
    }

    #[test]
    fn test_pop_scope_no_warn_synthetic() {
        let mut env = TypeEnv::new();
        env.push_scope();
        // synthetic define — no span, read_count = 1
        env.define("self_".to_string(), Ty::I32, false);
        let warnings = env.pop_scope_with_warnings();
        assert!(
            warnings.is_empty(),
            "synthetic bindings should never produce warnings"
        );
    }

    #[test]
    fn test_pop_scope_unused_beats_never_mutated() {
        let mut env = TypeEnv::new();
        env.push_scope();
        // mutable AND unused — should get Unused, not NeverMutated
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        let warnings = env.pop_scope_with_warnings();
        assert_eq!(warnings.len(), 1);
        assert!(
            matches!(warnings[0].kind, ScopeWarningKind::Unused),
            "unused should take priority over never-mutated"
        );
    }

    #[test]
    fn test_all_names() {
        let mut env = TypeEnv::new();
        env.define("a".to_string(), Ty::I32, false);
        env.push_scope();
        env.define("b".to_string(), Ty::Bool, false);
        let names: Vec<_> = env.all_names().collect();
        assert!(names.contains(&"a"));
        assert!(names.contains(&"b"));
    }
}
