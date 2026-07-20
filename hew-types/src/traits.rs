//! Trait definitions and automatic marker trait derivation.
//!
//! This module handles both built-in marker traits (Send, Frozen, Copy, etc.)
//! and user-defined traits with methods.

use crate::ty::Ty;
use crate::BuiltinType;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Built-in marker traits that are automatically derived.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MarkerTrait {
    /// Can cross actor boundaries safely
    Send,
    /// Safe to share references across threads
    Sync,
    /// Deeply immutable (implies Send)
    Frozen,
    /// Implicitly copied on assignment
    Copy,
    /// Has `.clone()` method
    Clone,
    /// Equality comparable
    Eq,
    /// Numeric ordering comparable, including floating-point values.
    PartialOrd,
    /// Ordered
    Ord,
    /// Numeric value accepted by generic math builtins.
    Num,
    /// Hashable
    Hash,
    /// String formatting via `{}`
    Display,
    /// Debug formatting via `{:?}`
    Debug,
    /// Has destructor
    Drop,
    /// Can be deserialized from bytes
    Decode,
    /// Can be serialized to bytes
    Encode,
    /// Can cross a remote actor boundary as a Hew value payload.
    ///
    /// This is a compile-time floor only: current remote-send lowering still
    /// wraps raw in-memory ABI bytes in a CBOR envelope, and structural
    /// Hew-value encoding is a later slice.
    Serializable,
    /// Contains no `Rc<T>` anywhere in its admissible structure
    RcFree,
    /// Owns an operating-system or runtime resource whose drop closes it.
    /// Design contract D3 (@resource) in v0.5. Applies to Duplex, Stream, Sink.
    /// No consumer in slice 2; queried by drop-elaboration in slice 3.
    Resource,
}

impl std::fmt::Display for MarkerTrait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MarkerTrait::Send => write!(f, "Send"),
            MarkerTrait::Sync => write!(f, "Sync"),
            MarkerTrait::Frozen => write!(f, "Frozen"),
            MarkerTrait::Copy => write!(f, "Copy"),
            MarkerTrait::Clone => write!(f, "Clone"),
            MarkerTrait::Eq => write!(f, "Eq"),
            MarkerTrait::PartialOrd => write!(f, "PartialOrd"),
            MarkerTrait::Ord => write!(f, "Ord"),
            MarkerTrait::Num => write!(f, "Num"),
            MarkerTrait::Hash => write!(f, "Hash"),
            MarkerTrait::Display => write!(f, "Display"),
            MarkerTrait::Debug => write!(f, "Debug"),
            MarkerTrait::Drop => write!(f, "Drop"),
            MarkerTrait::Decode => write!(f, "Decode"),
            MarkerTrait::Encode => write!(f, "Encode"),
            MarkerTrait::Serializable => write!(f, "Serializable"),
            MarkerTrait::RcFree => write!(f, "RcFree"),
            MarkerTrait::Resource => write!(f, "Resource"),
        }
    }
}

impl MarkerTrait {
    /// Parse a trait name string into the corresponding `MarkerTrait`, if it is one.
    #[must_use]
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "Send" => Some(Self::Send),
            "Sync" => Some(Self::Sync),
            "Frozen" => Some(Self::Frozen),
            "Copy" => Some(Self::Copy),
            "Clone" => Some(Self::Clone),
            "Eq" => Some(Self::Eq),
            "PartialOrd" => Some(Self::PartialOrd),
            "Ord" => Some(Self::Ord),
            "Num" => Some(Self::Num),
            "Hash" => Some(Self::Hash),
            "Display" => Some(Self::Display),
            "Debug" => Some(Self::Debug),
            "Drop" => Some(Self::Drop),
            "Decode" => Some(Self::Decode),
            "Encode" => Some(Self::Encode),
            "Serializable" => Some(Self::Serializable),
            "RcFree" => Some(Self::RcFree),
            "Resource" => Some(Self::Resource),
            _ => None,
        }
    }
}

/// A method signature in a trait or impl.
#[derive(Debug, Clone)]
pub struct MethodSig {
    /// Method name
    pub name: String,
    /// Parameter types (not including self)
    pub params: Vec<Ty>,
    /// Return type
    pub return_type: Ty,
    /// Whether the method takes self by reference
    pub takes_self: bool,
    /// Whether the method takes self by mutable reference
    pub self_mutable: bool,
    /// Whether the method consumes its receiver (i.e. takes `self` by value
    /// and ends the receiver binding's lifetime). When `true`, every call
    /// site marks the receiver moved so the move-checker rejects subsequent
    /// uses and the codegen scope-exit drop becomes a null-guarded no-op.
    ///
    /// PR 1 (issue #1295) ships this field with the recognised set empty:
    /// no Hew surface syntax sets it today. PR 2 populates it for
    /// `Closable::close`. No attribute syntax is exposed.
    pub consumes_receiver: bool,
}

/// A trait definition.
#[derive(Debug, Clone)]
pub struct TraitDef {
    /// Trait name
    pub name: String,
    /// Type parameters
    pub type_params: Vec<String>,
    /// Super traits this trait extends
    pub super_traits: Vec<String>,
    /// Methods defined by this trait
    pub methods: Vec<MethodSig>,
    /// Associated types
    pub associated_types: Vec<String>,
}

/// Registry of type definitions for trait checking.
///
/// This tracks type definitions and trait implementations to enable
/// automatic derivation of marker traits and method lookup.
#[derive(Debug, Clone, Default)]
pub struct TraitRegistry {
    /// Struct/enum definitions: name → field types
    type_fields: HashMap<String, Vec<Ty>>,
    /// `RcFree` capability members: name → field and variant payload types
    rc_free_members: HashMap<String, Vec<Ty>>,
    /// Explicit negative impls: types that DO NOT implement a trait
    negative_impls: HashMap<String, HashSet<MarkerTrait>>,
    /// Trait declarations with methods
    trait_decls: HashMap<String, TraitDef>,
    /// Trait implementations: (`type_name`, `trait_name`) → methods
    trait_impls: HashMap<(String, String), Vec<MethodSig>>,
    /// Actor definitions (actors are always Send)
    actors: HashSet<String>,
    /// Opaque handle types from loaded modules (e.g. "json.Value", "net.Connection").
    handle_types: HashSet<String>,
    /// Drop types from loaded modules (e.g. "http.Request").
    drop_types: HashSet<String>,
    /// Record types declared with the `record` keyword.
    ///
    /// Records are value types: marker derivation is entirely field-driven, and
    /// the `Resource` marker is always false regardless of field types (a record
    /// wrapping a resource field is not itself an OS/runtime resource).
    records: HashSet<String>,
    /// Named types admitted to the initial `Serializable` subset.
    ///
    /// The value stores every field / variant-payload member that must itself
    /// be serializable. Registration deliberately includes records, enums, and
    /// wire-marked types, but not ordinary plain structs.
    serializable_members: HashMap<String, Vec<Ty>>,
    /// Names of types declared with the `#[resource]` marker.
    ///
    /// A `#[resource]` type's inherent `close(self)` is BOTH the implicit-drop
    /// dispatch target (W3.030) AND a terminal consuming method: calling it
    /// moves the receiver, so a subsequent use is `UseAfterMove` and the
    /// scope-exit implicit drop is suppressed on the consumed path (#1295).
    ///
    /// This is the single source of truth for the user-declared `#[resource]`
    /// fact. The structural `MarkerTrait::Resource` derivation in
    /// `implements_marker` covers the built-in resource handles (`Duplex`,
    /// `CancellationToken`) on their type shape; this set covers the
    /// user/stdlib `#[resource]` declarations keyed by name. Both are owned by
    /// the registry so the checker no longer keeps a parallel allowlist.
    resource_types: HashSet<String>,
    /// Names of types declared with the `#[linear]` marker. Single-owner, no
    /// implicit drop; tracked so the unused-binding lint can treat their handles
    /// as RAII (suppress) rather than nagging for an `_` prefix. See
    /// `register_linear_type` / `is_linear`.
    linear_types: HashSet<String>,
    /// Declared type-parameter names for generic user types.
    ///
    /// Maps a type's declared (bare) name to the ordered list of its type
    /// parameter names as they appear in the declaration
    /// (`type Pair<A, B> { … }` → `"Pair"` → `["A", "B"]`).
    ///
    /// Used by `is_type_param_placeholder` to authoritative distinguish a
    /// genuine type-param placeholder field (e.g. `A` or `B` in `Pair<A, B>`)
    /// from a concrete user-defined type whose bare name happens to look like a
    /// single-letter param. Without this table the placeholder detector falls
    /// back to a heuristic (absence from `type_fields` + absence from
    /// `negative_impls`); with it, placeholder classification is deterministic
    /// and fail-closed: only names that appear in the declared param list are
    /// skipped during the concrete-field pass.
    ///
    /// Populated by `register_type_params`, called alongside `register_type` at
    /// every type / record / machine declaration site.
    type_params: HashMap<String, Vec<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RcFreeStatus {
    RcFree,
    ContainsRc,
    Recursive(String),
}

impl TraitRegistry {
    /// Create a new empty trait registry.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a type definition with its field types.
    pub fn register_type(&mut self, name: String, field_types: Vec<Ty>) {
        self.type_fields.insert(name, field_types);
    }

    /// Mirror every name-keyed marker-derivation record from `bare` onto a
    /// module-`qualified` alias so the qualified name derives the SAME markers
    /// as the bare declaration.
    ///
    /// Marker derivation for a `Ty::Named` keys every name-indexed table on the
    /// type's `name` string: `type_fields` (the structural Send/Copy/… member
    /// set), `rc_free_members`, `serializable_members`, `negative_impls`, and
    /// the `records` / `actors` / `handle_types` / `drop_types` membership sets.
    /// The bare name is last-write-wins across modules: two imported packages
    /// that each export a `Reply` collide on the single `"Reply"` key, so the
    /// loser's fields are lost and a non-Send reply can derive `Send` from the
    /// winner's fields (or a Send reply be rejected from the loser's). The
    /// importer keeps a per-module-qualified `type_defs` alias (`badpkg.Reply`)
    /// already; mirror the marker tables under the same qualified key so a
    /// qualified lookup is collision-free. Bare lookups are unchanged.
    ///
    /// Called when the importer creates the qualified `type_defs` alias, at
    /// which point the bare records still hold THIS type's freshly-registered
    /// members (the alias runs in the same registration step). A bare name with
    /// no record is a no-op for that table.
    pub fn alias_type_markers(&mut self, bare: &str, qualified: &str) {
        if qualified == bare {
            return;
        }
        if let Some(fields) = self.type_fields.get(bare).cloned() {
            self.type_fields.insert(qualified.to_string(), fields);
        }
        if let Some(members) = self.rc_free_members.get(bare).cloned() {
            self.rc_free_members.insert(qualified.to_string(), members);
        }
        if let Some(members) = self.serializable_members.get(bare).cloned() {
            self.serializable_members
                .insert(qualified.to_string(), members);
        }
        if let Some(negatives) = self.negative_impls.get(bare).cloned() {
            self.negative_impls.insert(qualified.to_string(), negatives);
        }
        if self.records.contains(bare) {
            self.records.insert(qualified.to_string());
        }
        if self.actors.contains(bare) {
            self.actors.insert(qualified.to_string());
        }
        if self.handle_types.contains(bare) {
            self.handle_types.insert(qualified.to_string());
        }
        if self.drop_types.contains(bare) {
            self.drop_types.insert(qualified.to_string());
        }
        if let Some(params) = self.type_params.get(bare).cloned() {
            self.type_params.insert(qualified.to_string(), params);
        }
    }

    /// Report whether marker derivation has a structural member set registered
    /// for `name` (under `type_fields`). Used by the ask-reply Send gate to
    /// confirm a module-qualified reply identity (`badpkg.Reply`) resolves to a
    /// real member set before keying the lookup on it; otherwise the gate keeps
    /// the bare type. A `name` with no entry would otherwise hit the
    /// unknown-type fail-closed branch and spuriously reject.
    #[must_use]
    pub fn has_type_markers(&self, name: &str) -> bool {
        self.type_fields.contains_key(name)
    }

    /// Register a `record` declaration so the marker-derivation path knows to
    /// suppress `Resource` and apply field-driven derivation exhaustively.
    ///
    /// Must be called in addition to `register_type` for every `record` decl.
    pub fn register_record_type(&mut self, name: String) {
        self.records.insert(name);
    }

    /// Register the structural members that participate in `RcFree`.
    pub fn register_rcfree_members(&mut self, name: String, member_types: Vec<Ty>) {
        self.rc_free_members.insert(name, member_types);
    }

    /// Register a named type as part of the accepted `Serializable` subset.
    pub fn register_serializable_type(&mut self, name: String, member_types: Vec<Ty>) {
        self.serializable_members.insert(name, member_types);
    }

    /// Look up `RcFree` members by exact or module-qualified/unqualified name.
    fn rc_free_members_any(&self, name: &str) -> Option<&Vec<Ty>> {
        self.rc_free_members
            .get(name)
            .or_else(|| self.rc_free_members.get(crate::short_name(name)))
    }

    /// Look up `Serializable` members by exact or module-qualified/unqualified name.
    fn serializable_members_any(&self, name: &str) -> Option<&Vec<Ty>> {
        self.serializable_members
            .get(name)
            .or_else(|| self.serializable_members.get(crate::short_name(name)))
    }

    fn combine_rc_free_status<I>(&self, tys: I, visiting: &mut HashSet<String>) -> RcFreeStatus
    where
        I: IntoIterator<Item = Ty>,
    {
        let mut unknown = None;
        for ty in tys {
            match self.implements_rc_free(&ty, visiting) {
                RcFreeStatus::RcFree => {}
                RcFreeStatus::ContainsRc => return RcFreeStatus::ContainsRc,
                RcFreeStatus::Recursive(name) => {
                    if unknown.is_none() {
                        unknown = Some(name);
                    }
                }
            }
        }
        unknown.map_or(RcFreeStatus::RcFree, RcFreeStatus::Recursive)
    }

    fn implements_rc_free(&self, ty: &Ty, visiting: &mut HashSet<String>) -> RcFreeStatus {
        match ty {
            Ty::Named {
                builtin: Some(BuiltinType::Rc),
                args,
                ..
            } => {
                if args.is_empty() {
                    RcFreeStatus::RcFree
                } else {
                    RcFreeStatus::ContainsRc
                }
            }
            // LocalPid<T>/RemotePid<T> are opaque identity references;
            // their T parameter is phantom for rc-free structural checks.
            Ty::Named {
                builtin: Some(BuiltinType::LocalPid | BuiltinType::RemotePid),
                ..
            } => RcFreeStatus::RcFree,
            // Heap-indirecting collections (`Vec`/`HashMap`/`HashSet`): the
            // element type lives behind a heap allocation, so a recursive
            // back-edge through one of these is FINITE in value size (the
            // recursion terminates at the heap buffer, not at an infinite-size
            // inline value). `Vec<RedisReply>` where
            // `enum RedisReply { Array(Vec<RedisReply>); ... }` is therefore
            // RcFree-admissible — the indirection is the witness that the value
            // is representable. A `Recursive(name)` reported for a name that is
            // CURRENTLY being visited is exactly such an indirection-broken
            // back-edge → admit it as `RcFree`. A `Recursive(name)` for a name
            // NOT on the active stack is a genuine cycle elsewhere and is
            // preserved (fail-closed). `ContainsRc` is always preserved.
            //
            // This is the ONLY relaxation: a DIRECT value back-edge
            // (`enum E { V(E) }`) never passes through a collection arm — it
            // recurses through the general `Named` member walk below and stays
            // `Recursive`, which keeps the infinite-size value cycle rejected
            // (over-admitting it would unbound codegen layout recursion).
            Ty::Named {
                name,
                args,
                builtin: Some(BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet),
            } => {
                let mut worst = RcFreeStatus::RcFree;
                for arg in args {
                    match self.implements_rc_free(arg, visiting) {
                        RcFreeStatus::RcFree => {}
                        RcFreeStatus::ContainsRc => return RcFreeStatus::ContainsRc,
                        RcFreeStatus::Recursive(rec_name) => {
                            if visiting.contains(&rec_name) {
                                // Indirection-broken back-edge: finite value
                                // size, drop terminates at the heap buffer.
                                // Admit (leave `worst` as RcFree).
                            } else if matches!(worst, RcFreeStatus::RcFree) {
                                // A genuine recursive cycle not broken by this
                                // collection — preserve it (fail-closed).
                                worst = RcFreeStatus::Recursive(rec_name);
                            }
                        }
                    }
                }
                let _ = name;
                worst
            }
            Ty::Named {
                name,
                args,
                builtin: _,
            } => {
                match self.combine_rc_free_status(args.iter().cloned(), visiting) {
                    RcFreeStatus::RcFree => {}
                    outcome => return outcome,
                }
                if !visiting.insert(name.clone()) {
                    return RcFreeStatus::Recursive(name.clone());
                }
                let result = self
                    .rc_free_members_any(name)
                    .map_or(RcFreeStatus::RcFree, |members| {
                        self.combine_rc_free_status(members.iter().cloned(), visiting)
                    });
                visiting.remove(name);
                result
            }
            Ty::Tuple(elems) => self.combine_rc_free_status(elems.iter().cloned(), visiting),
            Ty::Array(inner, _) | Ty::Slice(inner) => self.implements_rc_free(inner, visiting),
            _ => RcFreeStatus::RcFree,
        }
    }

    pub(crate) fn rc_free_status(&self, ty: &Ty) -> RcFreeStatus {
        let mut visiting = HashSet::new();
        self.implements_rc_free(ty, &mut visiting)
    }

    fn has_encode_decode(&self, ty: &Ty) -> bool {
        self.implements_marker(ty, MarkerTrait::Encode)
            && self.implements_marker(ty, MarkerTrait::Decode)
    }

    fn implements_serializable_inner(&self, ty: &Ty, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Ty::Var(_) | Ty::Error => true,
            _ if !self.has_encode_decode(ty) => false,
            Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::IntLiteral
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::Isize
            | Ty::Usize
            | Ty::F32
            | Ty::F64
            | Ty::FloatLiteral
            | Ty::Bool
            | Ty::Char
            | Ty::Duration
            | Ty::Unit
            | Ty::Never
            | Ty::String
            | Ty::Bytes => true,
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.implements_serializable_inner(elem, visiting)),
            Ty::Array(inner, _) => self.implements_serializable_inner(inner, visiting),
            Ty::Named {
                name,
                args,
                builtin,
            } => {
                if let Some(negatives) = self.negative_impls.get(name) {
                    if negatives.contains(&MarkerTrait::Serializable) {
                        return false;
                    }
                }
                if matches!(builtin, Some(BuiltinType::Option | BuiltinType::Result)) {
                    return args
                        .iter()
                        .all(|arg| self.implements_serializable_inner(arg, visiting));
                }
                // Fix 1: Vec, HashMap, and HashSet are NOT in `serializable_members`
                // (they are never registered via `register_serializable_type`), so
                // `serializable_members_any` returns `None` for them and they fall to
                // `return false` below.  This is the fail-closed boundary for the
                // RemotePid serialization gate: neither the codegen walk
                // (`emit_{ser,de}_value`) nor the on-wire codec handles collection
                // types, so admitting them here would be a silent correctness hole.
                //
                // Tests in the `tests` module assert this property for
                // `Vec<i64>` and `HashMap<String, i64>` by name.
                let Some(members) = self.serializable_members_any(name).cloned() else {
                    return false;
                };
                if !visiting.insert(name.clone()) {
                    return false;
                }
                let ok = members
                    .iter()
                    .all(|member| self.implements_serializable_inner(member, visiting));
                visiting.remove(name);
                ok
            }
            Ty::Slice(_)
            | Ty::CancellationToken
            | Ty::Function { .. }
            | Ty::Closure { .. }
            | Ty::Pointer { .. }
            | Ty::Borrow { .. }
            | Ty::TraitObject { .. }
            | Ty::Task(_)
            | Ty::AssocType { .. } => false,
        }
    }

    /// Check whether a type is admitted by the current `Serializable` subset.
    #[must_use]
    pub fn is_serializable(&self, ty: &Ty) -> bool {
        let mut visiting = HashSet::new();
        self.implements_serializable_inner(ty, &mut visiting)
    }

    /// Register an actor type.
    pub fn register_actor(&mut self, name: String) {
        self.actors.insert(name);
    }

    /// Register an opaque handle type (e.g. "json.Value").
    pub fn register_handle_type(&mut self, name: String) {
        self.handle_types.insert(name);
    }

    /// Register a drop type (e.g. "http.Request").
    pub fn register_drop_type(&mut self, name: String) {
        self.drop_types.insert(name);
    }

    /// Check if a name is a handle type (qualified or unqualified).
    fn is_handle_type_any(&self, name: &str) -> bool {
        self.handle_types.contains(name)
            || self
                .handle_types
                .iter()
                .any(|ht| crate::short_name(ht) == name)
    }

    /// Check if a name is a drop type (qualified or unqualified).
    fn is_drop_type_any(&self, name: &str) -> bool {
        self.drop_types.contains(name)
            || self
                .drop_types
                .iter()
                .any(|dt| crate::short_name(dt) == name)
    }

    /// Register a `#[resource]` type by its declared (bare) name.
    ///
    /// Called at type-decl registration for every user/stdlib `#[resource]`
    /// declaration. The registry is the single authority for this fact;
    /// `is_resource` is the only query path.
    pub fn register_resource_type(&mut self, name: String) {
        self.resource_types.insert(name);
    }

    /// Record `name` as a `#[linear]` type.
    ///
    /// Mirrors `register_resource_type`. A `#[linear]` type is single-owner with
    /// no implicit drop: leaving its handle unconsumed is already a hard
    /// `MustConsume` error, so the diagnostic layer treats it as a RAII handle and
    /// suppresses the redundant unused-binding warning. `is_linear` is the query.
    pub fn register_linear_type(&mut self, name: String) {
        self.linear_types.insert(name);
    }

    /// Register the declared type-parameter names for a generic type.
    ///
    /// **Must be called alongside `register_type`** at every type, record, or
    /// machine declaration site. Non-generic types pass an empty `params` list.
    ///
    /// **Always last-write-wins, matching `register_type` / `type_fields`.**
    /// Both tables are written together at every declaration site; they must
    /// have the same write policy so they always describe the same type. If
    /// this table were first-write-wins while `type_fields` is last-write-wins,
    /// a cross-module type-name collision would leave stale params in the table
    /// while the fields are updated — a concrete field whose bare name matched a
    /// stale param name would be misclassified as a placeholder and skipped in
    /// the concrete-field pass, granting Send to a type that holds a non-Send
    /// field (data-race / UAF soundness hole).
    ///
    /// Non-generic types register an empty param list, which is a valid sentinel:
    /// `is_type_param_placeholder` treats `Some([])` as "no params → no field is
    /// a placeholder → check every field directly", which is correct for
    /// non-generic types and faster than falling through to the heuristic.
    ///
    /// After registration, `is_type_param_placeholder` resolves field names
    /// against this list rather than the absence-based heuristic, making
    /// placeholder classification deterministic and fail-closed.
    pub fn register_type_params(&mut self, name: String, params: Vec<String>) {
        // Unconditionally last-write-wins to stay in lockstep with
        // `register_type` / `type_fields`. See doc comment above.
        self.type_params.insert(name, params);
    }

    /// Report whether `name` is a `#[resource]` type.
    ///
    /// `resource_types` is keyed by the declared (bare) type name. A receiver
    /// type name may arrive module-qualified (`mod.Conn`) for an imported
    /// handle type, so the unqualified suffix is matched too — mirroring the
    /// bare/unqualified lookup the `Drop`/handle type sets use.
    #[must_use]
    pub fn is_resource(&self, name: &str) -> bool {
        if self.resource_types.contains(name) {
            return true;
        }
        let unqualified = crate::short_name(name);
        self.resource_types.contains(unqualified)
    }

    /// Report whether `name` is a `#[linear]` type. Matches the bare and
    /// module-qualified-suffix spellings, mirroring `is_resource`.
    #[must_use]
    pub fn is_linear(&self, name: &str) -> bool {
        if self.linear_types.contains(name) {
            return true;
        }
        let unqualified = crate::short_name(name);
        self.linear_types.contains(unqualified)
    }

    /// Register a negative impl (type does NOT implement trait).
    pub fn register_negative_impl(&mut self, type_name: String, marker: MarkerTrait) {
        self.negative_impls
            .entry(type_name)
            .or_default()
            .insert(marker);
    }

    /// Register a trait declaration.
    pub fn register_trait(&mut self, def: TraitDef) {
        self.trait_decls.insert(def.name.clone(), def);
    }

    /// Register a trait implementation for a type.
    pub fn register_impl(
        &mut self,
        type_name: String,
        trait_name: String,
        methods: Vec<MethodSig>,
    ) {
        self.trait_impls.insert((type_name, trait_name), methods);
    }

    /// Look up a trait definition.
    #[must_use]
    pub fn lookup_trait(&self, name: &str) -> Option<&TraitDef> {
        self.trait_decls.get(name)
    }

    /// Look up methods from a trait impl.
    #[must_use]
    pub fn lookup_impl(&self, type_name: &str, trait_name: &str) -> Option<&[MethodSig]> {
        self.trait_impls
            .get(&(type_name.to_string(), trait_name.to_string()))
            .map(Vec::as_slice)
    }

    /// Check if a type implements a marker trait (automatic derivation).
    ///
    /// Marker traits are derived automatically based on the structure of the type:
    /// - Primitives implement most marker traits
    /// - Composite types implement a trait if all their components do
    /// - `LocalPid`/`RemotePid` are always `Send` and `Frozen`
    /// - Negative impls can override automatic derivation
    #[must_use]
    pub fn implements_marker(&self, ty: &Ty, marker: MarkerTrait) -> bool {
        let mut visiting = HashSet::new();
        self.implements_marker_guarded(ty, marker, &mut visiting)
    }

    /// Structural marker derivation with a recursion guard over named types.
    ///
    /// The guard makes the walk total on recursive type graphs. A type whose
    /// member set transitively re-enters itself (`enum E { V(E) }`, or an
    /// indirection-broken recursive enum `enum R { A(Vec<R>) }`) would
    /// otherwise recurse forever through the `type_fields` / record / collection
    /// member arms. On re-entry to a name already on the active stack we return
    /// `true` (the neutral element of the `all(...)` member conjunction): the
    /// re-entered edge contributes no NEW obligation, so the result is decided
    /// by the non-recursive members. Genuine infinite-size value cycles are
    /// rejected upstream by the dedicated cycle detector (`cycle.rs`); this
    /// guard only prevents the derivation from diverging while computing a
    /// marker for a type the cycle detector permits (the `Vec`-indirected case).
    #[expect(
        clippy::too_many_lines,
        reason = "marker derivation covers many Ty variants"
    )]
    fn implements_marker_guarded(
        &self,
        ty: &Ty,
        marker: MarkerTrait,
        visiting: &mut HashSet<String>,
    ) -> bool {
        if marker == MarkerTrait::Serializable {
            return self.is_serializable(ty);
        }
        if marker == MarkerTrait::RcFree {
            return matches!(self.rc_free_status(ty), RcFreeStatus::RcFree);
        }
        if marker == MarkerTrait::Resource {
            // Only resource types are Resource; primitives are NOT.
            // Handled per-type below; fall through to the match.
        }
        if marker == MarkerTrait::Num {
            return ty.is_numeric();
        }
        if marker == MarkerTrait::PartialOrd {
            return ty.is_numeric();
        }
        match ty {
            // Primitives: always Send, Sync, Frozen, Copy, Clone, Eq, Ord, Hash, Debug.
            // NOT Resource: primitives own no OS/runtime resource and need no drop close.
            // Isize/Usize are platform-sized integers: same marker set as fixed-width ints.
            // `instant` is a monotonic i64-nanos timestamp — ABI-identical to a
            // bare i64, so it carries the same marker set (without this an
            // `instant` actor field / spawn arg / message is rejected as
            // non-Send).
            Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::IntLiteral
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::Isize
            | Ty::Usize
            | Ty::Bool
            | Ty::Char
            | Ty::Duration
            | Ty::Named {
                builtin: Some(BuiltinType::Instant),
                ..
            }
            | Ty::Unit
            | Ty::Error
            | Ty::Never => !matches!(marker, MarkerTrait::Resource),

            // Floats: most traits, including Eq and Hash, which use bitwise/total
            // semantics (the eq/hash thunks compare and hash the bit pattern, so
            // NaN==NaN when bits are identical and the relation is reflexive).
            // Still NOT Ord — float ordering stays IEEE partial (`PartialOrd`,
            // granted above via `is_numeric`); a total bitwise order is a
            // separate decision. NOT Resource (value type).
            Ty::F32 | Ty::F64 | Ty::FloatLiteral => {
                !matches!(marker, MarkerTrait::Ord | MarkerTrait::Resource)
            }

            // String: Send + Sync + Clone + Encode + Decode, but NOT Frozen (mutable), NOT Copy
            Ty::String => matches!(
                marker,
                MarkerTrait::Send
                    | MarkerTrait::Sync
                    | MarkerTrait::Clone
                    | MarkerTrait::Eq
                    | MarkerTrait::Ord
                    | MarkerTrait::Hash
                    | MarkerTrait::Display
                    | MarkerTrait::Debug
                    | MarkerTrait::Encode
                    | MarkerTrait::Decode
            ),

            // Bytes: Send + Sync + Clone + Eq + Hash + Debug + Encode + Decode
            Ty::Bytes => matches!(
                marker,
                MarkerTrait::Send
                    | MarkerTrait::Sync
                    | MarkerTrait::Clone
                    | MarkerTrait::Eq
                    | MarkerTrait::Hash
                    | MarkerTrait::Debug
                    | MarkerTrait::Encode
                    | MarkerTrait::Decode
            ),

            // CancellationToken: ref-counted runtime handle. It is movable,
            // cloneable by retain, and dropped by release-ref; it is not Copy.
            Ty::CancellationToken => matches!(
                marker,
                MarkerTrait::Send
                    | MarkerTrait::Sync
                    | MarkerTrait::Clone
                    | MarkerTrait::Debug
                    | MarkerTrait::Resource
            ),

            // LocalPid<T>: process-local actor pid returned by `spawn`.
            // An opaque identity reference, not a resource: Send + Sync + Frozen
            // + Copy + Clone + Debug.
            Ty::Named {
                builtin: Some(BuiltinType::LocalPid),
                ..
            } => matches!(
                marker,
                MarkerTrait::Send
                    | MarkerTrait::Sync
                    | MarkerTrait::Frozen
                    | MarkerTrait::Copy
                    | MarkerTrait::Clone
                    | MarkerTrait::Debug
            ),

            // Key-backed node identity values are immutable inline aggregates.
            Ty::Named {
                builtin: Some(BuiltinType::NodeId | BuiltinType::Location | BuiltinType::RemotePid),
                ..
            } => matches!(
                marker,
                MarkerTrait::Send
                    | MarkerTrait::Sync
                    | MarkerTrait::Frozen
                    | MarkerTrait::Copy
                    | MarkerTrait::Clone
                    | MarkerTrait::Eq
                    | MarkerTrait::Hash
                    | MarkerTrait::Display
                    | MarkerTrait::Debug
            ),

            // Stream<T> and Sink<T>: Send/Sync iff T: Send; NOT Clone, Copy, or Frozen (move-only)
            Ty::Named {
                builtin: Some(BuiltinType::Stream | BuiltinType::Sink),
                args,
                ..
            } if args.len() == 1 => match marker {
                MarkerTrait::Send | MarkerTrait::Sync => {
                    self.implements_marker_guarded(&args[0], MarkerTrait::Send, visiting)
                }
                _ => false,
            },

            // Duplex<S, R>: bidirectional lambda-actor handle.
            // Send/Sync iff BOTH S: Send AND R: Send.
            // NOT Copy (move-only resource — drop closes both directions).
            // NOT Clone (split via send_half/recv_half in slice 4, not by cloning).
            // NOT Frozen (mutable internal queue state).
            // Resource: yes — dropping the last Duplex handle closes both I/O directions
            //   (@resource design contract D3; consumed by drop-elaboration in slice 3).
            Ty::Named {
                builtin: Some(BuiltinType::Duplex),
                args,
                ..
            } if args.len() == 2 => match marker {
                MarkerTrait::Send | MarkerTrait::Sync => {
                    self.implements_marker_guarded(&args[0], MarkerTrait::Send, visiting)
                        && self.implements_marker_guarded(&args[1], MarkerTrait::Send, visiting)
                }
                MarkerTrait::Resource => true,
                _ => false,
            },

            // LambdaPid<M, R>: the user-visible lambda-actor handle.
            // Send/Sync iff BOTH M: Send AND R: Send (message and reply cross
            // the actor boundary).
            // NOT Copy (move-only resource — last-handle drop stops the actor).
            // NOT Clone (a lambda actor handle is not split or duplicated by
            //   cloning; the runtime owns strong/weak ref discipline, §5.9).
            // Resource: yes — dropping the last handle runs the stop-on-last-
            //   handle-drop protocol (`hew_lambda_actor_release`).
            Ty::Named {
                builtin: Some(BuiltinType::LambdaPid),
                args,
                ..
            } if args.len() == 2 => match marker {
                MarkerTrait::Send | MarkerTrait::Sync => {
                    self.implements_marker_guarded(&args[0], MarkerTrait::Send, visiting)
                        && self.implements_marker_guarded(&args[1], MarkerTrait::Send, visiting)
                }
                MarkerTrait::Resource => true,
                _ => false,
            },

            // SendHalf<T> / RecvHalf<T>: exclusive ownership of one end of a
            // duplex channel. They are OS/runtime resources (have a `close()`
            // contract), so `Resource` and `Drop` are true.  They are NOT
            // `Send` or `Sync` — a channel half is an exclusive handle that
            // must not cross an actor boundary.  This arm must come before
            // the fallthrough `Ty::Named` arm so it fires for the builtin
            // variants rather than going through the unknown-type path.
            //
            // Not-Send propagation: any user type that holds a SendHalf or
            // RecvHalf (e.g. `type W { h: RecvHalf<i64> }`) will visit this
            // arm during its field check and correctly derive not-Send.
            Ty::Named {
                builtin: Some(BuiltinType::SendHalf | BuiltinType::RecvHalf),
                ..
            } => matches!(marker, MarkerTrait::Resource | MarkerTrait::Drop),

            // Tuple: marker holds if ALL elements have it
            Ty::Tuple(elems) => elems
                .iter()
                .all(|e| self.implements_marker_guarded(e, marker, visiting)),

            // Array: marker holds if element has it
            Ty::Array(inner, _) => self.implements_marker_guarded(inner, marker, visiting),

            // Slice: like array but NOT Copy (unsized)
            Ty::Slice(elem) => {
                if marker == MarkerTrait::Copy {
                    false
                } else {
                    self.implements_marker_guarded(elem, marker, visiting)
                }
            }

            // Named types: check all fields
            Ty::Named {
                name,
                args,
                builtin,
            } => {
                // Check negative impls first
                if let Some(negatives) = self.negative_impls.get(name) {
                    if negatives.contains(&marker) {
                        return false;
                    }
                }
                // Actors are always Send + Sync
                if self.actors.contains(name)
                    && matches!(marker, MarkerTrait::Send | MarkerTrait::Sync)
                {
                    return true;
                }
                // Drop types (e.g. http.Request): Send + Clone + Debug but NOT Copy.
                // They own resources and need move semantics for actor sends.
                if self.is_drop_type_any(name) {
                    return matches!(
                        marker,
                        MarkerTrait::Send
                            | MarkerTrait::Sync
                            | MarkerTrait::Clone
                            | MarkerTrait::Debug
                            | MarkerTrait::Drop
                    );
                }
                // Opaque stdlib handle types: Send + Copy + Clone + Debug
                // (they're integer file descriptors or pointers at the ABI level)
                // Check both qualified ("net.Connection") and unqualified ("Connection") forms.
                if self.is_handle_type_any(name) {
                    return matches!(
                        marker,
                        MarkerTrait::Send
                            | MarkerTrait::Sync
                            | MarkerTrait::Copy
                            | MarkerTrait::Clone
                            | MarkerTrait::Debug
                    );
                }
                // Built-in generic collections: Send/Clone/Debug if elements are,
                // but NOT Copy or Frozen (heap-allocated, mutable)
                if builtin.is_some_and(BuiltinType::is_collection) {
                    return match marker {
                        MarkerTrait::Copy | MarkerTrait::Frozen => false,
                        _ => args
                            .iter()
                            .all(|a| self.implements_marker_guarded(a, marker, visiting)),
                    };
                }
                // Option<T> and Result<T,E>: pure value-type generic builtins.
                // All markers derive structurally from type arguments — same rule as
                // `implements_serializable_inner` at line ~473 which already had this arm.
                // Resource uses ANY (not all): if any type argument is a built-in resource
                // handle (Duplex, LambdaPid, CancellationToken), the wrapper MAY hold one and
                // must be treated as a resource too. Note: user `#[resource]` types (e.g.
                // `#[resource] type Conn { fd: i64 }`) return Resource=false from their own
                // structural field derivation (fields like i64 are not Resource), so
                // Option<user_resource> is correctly Resource=false here. Variant-aware close
                // for user-resource payloads inside Option/Result requires separate work in
                // MIR/codegen and is tracked as a follow-on lane.
                if matches!(builtin, Some(BuiltinType::Option | BuiltinType::Result)) {
                    return match marker {
                        MarkerTrait::Resource => args
                            .iter()
                            .any(|a| self.implements_marker_guarded(a, marker, visiting)),
                        _ => args
                            .iter()
                            .all(|a| self.implements_marker_guarded(a, marker, visiting)),
                    };
                }
                // Rc<T>/Weak<T>: reference-counted, single-threaded — explicitly
                // NOT Send or Sync. Both support Clone and Drop, but not Copy or
                // Frozen.
                if matches!(builtin, Some(BuiltinType::Rc | BuiltinType::Weak)) {
                    return matches!(marker, MarkerTrait::Clone | MarkerTrait::Drop);
                }
                // Recursion guard: a recursive type graph (e.g. an
                // indirection-broken recursive enum `enum R { A(Vec<R>) }`)
                // re-enters this name through its member walk. On re-entry,
                // return the neutral `true` — the re-entered edge adds no new
                // obligation; the result is decided by the non-recursive
                // members. (Direct infinite-size cycles are rejected by
                // `cycle.rs`; this only keeps the derivation total.)
                if !visiting.insert(name.clone()) {
                    return true;
                }
                // Record types: value types declared with `record`. Markers are
                // derived entirely from field types with two exceptions:
                //   - `Resource` is always false: a record wrapping a resource field
                //     is NOT itself an OS/runtime resource (no drop-close contract).
                //   - `RcFree` is handled at the top of `implements_marker` before
                //     this arm is reached.
                // The two genuine exceptions (Resource, Num) are explicit; all
                // other markers share the structural `field_derives(marker)` rule
                // via the fallthrough.
                let result = if self.records.contains(name) {
                    match marker {
                        // Records are not OS/runtime resources or scalar numeric values.
                        MarkerTrait::Resource | MarkerTrait::Num => false,
                        // All other markers derive from field types. Clone the
                        // member list so the `&self` borrow does not alias the
                        // `&mut visiting` recursion below.
                        _ => {
                            let fields = self.type_fields.get(name).cloned().unwrap_or_default();
                            self.check_structural_fields(name, &fields, args, marker, visiting)
                        }
                    }
                } else if let Some(fields) = self.type_fields.get(name).cloned() {
                    self.check_structural_fields(name, &fields, args, marker, visiting)
                } else {
                    false // Unknown type — conservatively fail
                };
                visiting.remove(name);
                result
            }

            // Pointers: NOT Send (unless explicitly marked), but Copy
            Ty::Pointer { .. } => marker == MarkerTrait::Copy,

            // `&T` immutable borrow: Copy (a borrow is freely copyable) but NOT
            // Send — a borrow must not cross an actor boundary. Kept as a
            // distinct arm from the raw-pointer case above (not merged) so the
            // borrow marker policy can diverge in P5's send gate without
            // disturbing FFI-pointer semantics; today the result coincides.
            #[allow(
                clippy::match_same_arms,
                reason = "borrow send-policy is a deliberate seam for P5; do not merge with the raw-pointer arm"
            )]
            Ty::Borrow { .. } => marker == MarkerTrait::Copy,

            // Function types: always Send, Sync, Clone, Copy (function pointers)
            Ty::Function { .. } => matches!(
                marker,
                MarkerTrait::Send | MarkerTrait::Sync | MarkerTrait::Clone | MarkerTrait::Copy
            ),

            // Closures: Send/Sync only if all captured types are Send/Sync
            Ty::Closure { captures, .. } => match marker {
                MarkerTrait::Send | MarkerTrait::Sync => captures
                    .iter()
                    .all(|c| self.implements_marker_guarded(c, marker, visiting)),
                MarkerTrait::Clone => true,
                _ => false,
            },

            // Var: NOT Send by default.
            // AssocType { base, .. }: a projection's marker disposition cannot
            // be decided without seeing the impl-side binding. The projection
            // is collapsed to a concrete `Ty` before reaching MIR; while still
            // unresolved (generic-fn signature), no marker query can be
            // meaningfully answered. Conservatively return `false` — callers
            // that need a marker on a projection must run projection-collapse
            // first.
            Ty::Var(_) | Ty::AssocType { .. } => false,

            // Trait objects: `dyn T + Send` carries the marker directly as a
            // named bound. Route derivation through the bound names first, then
            // fall back to super-trait lookup in the registered `trait_decls`
            // (populated via `register_trait`; empty in production — seeded only
            // by test fixtures — but retained so that `dyn Drawable` where
            // `Drawable` extends `Send` resolves correctly in those contexts).
            //
            // The direct-bound path is the production fix for #3: the ghost
            // `trait_decls` table is always empty at the compiler's type-check
            // phase because `register_trait` has no production call-sites.
            // `dyn T + Send` now resolves Send correctly via the direct check.
            //
            // Control: `dyn T` (without `+ Send`) has no Send bound and no
            // registered super-traits → returns false for `MarkerTrait::Send`.
            Ty::TraitObject { traits } => traits.iter().any(|bound| {
                // Direct bound: `dyn T + Marker` — marker appears in the list.
                if bound.trait_name == marker.to_string() {
                    return true;
                }
                // Super-trait lookup: `dyn Drawable` where Drawable: Marker.
                if let Some(trait_def) = self.trait_decls.get(&bound.trait_name) {
                    if trait_def
                        .super_traits
                        .iter()
                        .any(|s| s == &marker.to_string())
                    {
                        return true;
                    }
                }
                false
            }),

            // Task<T> is a compiler-internal consume-once handle. It is NOT
            // Copy, Clone, Frozen, or Eq. It IS Send iff T is Send (the task
            // executes on a thread; crossing the fork-block boundary is
            // structurally send-safe). For all other markers it is false today;
            // the full marker set for Task<T> is revisited when task handles
            // become user-accessible (v0.6+).
            Ty::Task(inner) => match marker {
                MarkerTrait::Send | MarkerTrait::Sync => {
                    self.implements_marker(inner, MarkerTrait::Send)
                }
                _ => false,
            },
        }
    }

    /// Check if a type is safe to send across actor boundaries.
    #[must_use]
    pub fn is_send(&self, ty: &Ty) -> bool {
        self.implements_marker(ty, MarkerTrait::Send)
    }

    /// Check if a type is deeply immutable.
    #[must_use]
    pub fn is_frozen(&self, ty: &Ty) -> bool {
        self.implements_marker(ty, MarkerTrait::Frozen)
    }

    /// Check if a type is safe to share references across threads.
    #[must_use]
    pub fn is_sync(&self, ty: &Ty) -> bool {
        self.implements_marker(ty, MarkerTrait::Sync)
    }

    /// Structural field check for user-defined types, with generic-arg substitution.
    ///
    /// For non-generic types (`args` is empty) every registered field is checked
    /// directly — identical to the non-generic path.
    ///
    /// For generic instantiations (`args` non-empty) the derivation separates
    /// fields into two groups:
    ///
    /// * **Concrete fields** — field types whose own marker can be determined
    ///   without substitution (builtins, registered types, or fields that are
    ///   themselves generic instantiations). These are checked recursively as
    ///   before; the existing `visiting` guard prevents infinite recursion.
    ///
    /// * **Type-param placeholder fields** — bare `Ty::Named` entries whose name
    ///   appears in the declared type-parameter list for `parent_name` (see
    ///   [`Self::is_type_param_placeholder`]). These arise because `register_type`
    ///   stores the generic declaration's field types verbatim; `T` in
    ///   `type Box<T> { v: T }` is stored as `Ty::Named { name: "T" }`.
    ///   Placeholder fields are skipped in the direct check and are instead
    ///   covered by the **type-args sweep** below.
    ///
    /// **Type-args sweep** (`all(args.implements(marker))`): checks every
    /// declared type argument against `marker`. This is a conservative proxy
    /// for the substituted-field check: if every instantiation arg satisfies the
    /// marker then every type-param-bearing field in the declaration also
    /// satisfies it, regardless of the positional param-to-arg mapping. The
    /// converse (any arg failing ⇒ outer type fails) is also correct because
    /// a non-marker arg eventually substitutes into at least one field.
    ///
    /// **Soundness (no over-grant):** both arms use `all()`, so no non-Send
    /// component is silently dropped. A type holding ANY non-Send field or ANY
    /// non-Send type arg is correctly rejected.
    fn check_structural_fields(
        &self,
        parent_name: &str,
        fields: &[Ty],
        args: &[Ty],
        marker: MarkerTrait,
        visiting: &mut HashSet<String>,
    ) -> bool {
        // Concrete-field pass: check every field that is not a bare type-param
        // placeholder. For non-generic types all fields are concrete and this
        // is the only pass.
        let concrete_ok = fields.iter().all(|f| {
            if !args.is_empty() && self.is_type_param_placeholder(parent_name, f, marker) {
                // Placeholder covered by the type-args sweep; vacuously ok here.
                true
            } else {
                self.implements_marker_guarded(f, marker, visiting)
            }
        });
        if !concrete_ok {
            return false;
        }
        // Type-args sweep: for generic instantiations, every declared type
        // argument must satisfy the marker. This covers all type-param-bearing
        // fields (spec §4.1.2: "all variant payload / field types satisfy Send").
        args.iter()
            .all(|a| self.implements_marker_guarded(a, marker, visiting))
    }

    /// Returns `true` when `ty` is a bare type-parameter placeholder in the
    /// context of `parent_name`'s declaration.
    ///
    /// **Primary path — explicit params (fail-closed, deterministic):**
    /// If `register_type_params` was called for `parent_name`, a field is a
    /// placeholder iff its bare name appears in that declared list. Anything
    /// not in the declared list is a concrete type and is checked directly —
    /// even if it has no `type_fields` entry and no negative impl.
    ///
    /// **Fallback path — heuristic (for types without explicit param registration):**
    /// A bare `Ty::Named` with no builtin tag, no type arguments, no `type_fields`
    /// entry, and no registered negative marker fact is treated as a placeholder.
    /// This covers test fixtures and any pre-existing path that does not call
    /// `register_type_params`.
    ///
    /// The `negative_impls` guard in the fallback path is the soundness seal:
    /// a type registered via `register_negative_impl` is a KNOWN concrete type,
    /// not a generic param. Without this guard a field like `NoSend` (explicitly
    /// not-Send, but not in `type_fields`) would be misclassified as a
    /// placeholder and skipped in the concrete-field pass, allowing a
    /// `Wrapper<i64>` that holds a `NoSend` field to be granted Send — a
    /// data-race / UAF soundness hole.
    fn is_type_param_placeholder(&self, parent_name: &str, ty: &Ty, marker: MarkerTrait) -> bool {
        let Ty::Named {
            builtin: None,
            args,
            name: field_name,
        } = ty
        else {
            return false;
        };
        if !args.is_empty() {
            return false;
        }
        // Primary path: authoritative declared-param list.
        if let Some(params) = self.type_params.get(parent_name) {
            return params.iter().any(|p| p == field_name);
        }
        // Fallback heuristic: treat as placeholder iff no concrete evidence
        // exists (no type_fields entry, no negative impl for this marker).
        !self.type_fields.contains_key(field_name.as_str())
            && !self
                .negative_impls
                .get(field_name.as_str())
                .is_some_and(|s| s.contains(&marker))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ty::TraitObjectBound;

    #[test]
    fn test_primitives_are_send() {
        let registry = TraitRegistry::new();
        assert!(registry.is_send(&Ty::I32));
        assert!(registry.is_send(&Ty::Bool));
        assert!(registry.is_send(&Ty::F64));
    }

    #[test]
    fn test_string_is_send() {
        let registry = TraitRegistry::new();
        assert!(registry.is_send(&Ty::String));
    }

    #[test]
    fn test_tuple_send_if_all_elements_send() {
        let registry = TraitRegistry::new();
        let tuple = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        assert!(registry.is_send(&tuple));
    }

    #[test]
    fn test_named_type_with_fields() {
        let mut registry = TraitRegistry::new();
        registry.register_type("Point".to_string(), vec![Ty::I32, Ty::I32]);
        let point = Ty::Named {
            builtin: None,
            name: "Point".to_string(),
            args: vec![],
        };
        assert!(registry.is_send(&point));
        assert!(registry.implements_marker(&point, MarkerTrait::Copy));
    }

    #[test]
    fn test_negative_impl() {
        let mut registry = TraitRegistry::new();
        registry.register_type("Handle".to_string(), vec![Ty::I32]);
        registry.register_negative_impl("Handle".to_string(), MarkerTrait::Send);
        let handle = Ty::Named {
            builtin: None,
            name: "Handle".to_string(),
            args: vec![],
        };
        assert!(!registry.is_send(&handle));
    }

    #[test]
    fn test_floats_eq_and_hash_but_not_ord() {
        // Floats satisfy Eq and Hash under bitwise/total semantics, but Ord
        // stays IEEE-partial (PartialOrd only).
        let registry = TraitRegistry::new();
        assert!(registry.implements_marker(&Ty::F64, MarkerTrait::Eq));
        assert!(registry.implements_marker(&Ty::F32, MarkerTrait::Hash));
        assert!(registry.implements_marker(&Ty::F64, MarkerTrait::PartialOrd));
        assert!(!registry.implements_marker(&Ty::F64, MarkerTrait::Ord));
    }

    #[test]
    fn test_option_derives_from_inner() {
        // Previously this test registered "Option" manually, which masked the bug:
        // `type_fields.get("Option")` would return Some and happen to pass.
        // The production path NEVER registers Option via register_type — builtins are
        // handled by the `matches!(builtin, Some(BuiltinType::Option | ...))` arm.
        // Use a plain TraitRegistry::new() so the test exercises the real production path.
        let registry = TraitRegistry::new();
        let option_i32 = Ty::option(Ty::I32);
        assert!(registry.is_send(&option_i32));
        assert!(registry.implements_marker(&option_i32, MarkerTrait::Copy));
    }

    #[test]
    fn test_function_is_send() {
        let registry = TraitRegistry::new();
        let fn_ty = Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
        };
        assert!(registry.is_send(&fn_ty));
    }

    #[test]
    fn test_serializable_marker_uses_encode_decode_and_subset() {
        let mut registry = TraitRegistry::new();
        assert!(registry.is_serializable(&Ty::I64));
        assert!(registry.is_serializable(&Ty::String));
        assert!(registry.is_serializable(&Ty::Bytes));
        assert!(registry.is_serializable(&Ty::Tuple(vec![Ty::I64, Ty::Bool])));
        assert!(registry.is_serializable(&Ty::Array(Box::new(Ty::I64), 4)));

        let ping = Ty::Named {
            builtin: None,
            name: "Ping".to_string(),
            args: vec![],
        };
        registry.register_type("Ping".to_string(), vec![Ty::I64]);
        registry.register_record_type("Ping".to_string());
        registry.register_serializable_type("Ping".to_string(), vec![Ty::I64]);
        assert!(registry.is_serializable(&ping));

        let plain = Ty::Named {
            builtin: None,
            name: "Plain".to_string(),
            args: vec![],
        };
        registry.register_type("Plain".to_string(), vec![Ty::I64]);
        assert!(!registry.is_serializable(&plain));

        let fn_ty = Ty::Function {
            params: vec![Ty::I64],
            ret: Box::new(Ty::I64),
        };
        assert!(registry.is_send(&fn_ty));
        assert!(!registry.is_serializable(&fn_ty));

        let bad = Ty::Named {
            builtin: None,
            name: "Bad".to_string(),
            args: vec![],
        };
        registry.register_type("Bad".to_string(), vec![fn_ty]);
        registry.register_record_type("Bad".to_string());
        registry.register_serializable_type(
            "Bad".to_string(),
            vec![Ty::Function {
                params: vec![Ty::I64],
                ret: Box::new(Ty::I64),
            }],
        );
        assert!(!registry.is_serializable(&bad));
    }

    #[test]
    fn test_primitives_are_sync() {
        let registry = TraitRegistry::new();
        assert!(registry.is_sync(&Ty::I32));
        assert!(registry.is_sync(&Ty::Bool));
        assert!(registry.is_sync(&Ty::F64));
        assert!(registry.is_sync(&Ty::String));
    }

    #[test]
    fn test_struct_with_send_fields_is_sync() {
        let mut registry = TraitRegistry::new();
        registry.register_type("Point".to_string(), vec![Ty::I32, Ty::I32]);
        let point = Ty::Named {
            builtin: None,
            name: "Point".to_string(),
            args: vec![],
        };
        assert!(registry.is_sync(&point));
    }

    #[test]
    fn test_pointer_is_not_sync() {
        let registry = TraitRegistry::new();
        let ptr = Ty::Pointer {
            pointee: Box::new(Ty::I32),
            is_mutable: false,
        };
        assert!(!registry.is_sync(&ptr));
    }

    /// §4e: an immutable borrow `&T` is `Copy` but NOT `Send` — sending a
    /// borrow across an actor boundary must be rejected. The rejection flows
    /// through the dedicated `Ty::Borrow` marker arm (borrow-specific), so it
    /// is distinguishable from the accidental `*const T` pointer rejection.
    #[test]
    fn test_borrow_is_copy_not_send() {
        let registry = TraitRegistry::new();
        let borrow = Ty::Borrow {
            pointee: Box::new(Ty::String),
        };
        // Copy: a borrow is freely copyable (it is just a reference).
        assert!(registry.implements_marker(&borrow, MarkerTrait::Copy));
        // NOT Send: a borrow must not cross an actor boundary.
        assert!(!registry.is_send(&borrow));
        assert!(!registry.implements_marker(&borrow, MarkerTrait::Send));
    }

    #[test]
    fn test_vec_is_send_when_element_is_send() {
        let registry = TraitRegistry::new();
        let vec_i32 = Ty::Named {
            builtin: Some(BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![Ty::I32],
        };
        assert!(registry.is_send(&vec_i32));
        assert!(registry.is_sync(&vec_i32));
        assert!(registry.implements_marker(&vec_i32, MarkerTrait::Clone));
        assert!(!registry.implements_marker(&vec_i32, MarkerTrait::Copy));
        assert!(!registry.implements_marker(&vec_i32, MarkerTrait::Frozen));
    }

    #[test]
    fn test_hashmap_is_send_when_elements_are_send() {
        let registry = TraitRegistry::new();
        let map = Ty::Named {
            builtin: Some(BuiltinType::HashMap),
            name: "HashMap".to_string(),
            args: vec![Ty::String, Ty::I32],
        };
        assert!(registry.is_send(&map));
        assert!(!registry.implements_marker(&map, MarkerTrait::Copy));
    }

    #[test]
    fn test_hashset_is_send_when_element_is_send() {
        let registry = TraitRegistry::new();
        let set = Ty::Named {
            builtin: Some(BuiltinType::HashSet),
            name: "HashSet".to_string(),
            args: vec![Ty::String],
        };
        assert!(registry.is_send(&set));
        assert!(registry.is_sync(&set));
        assert!(registry.implements_marker(&set, MarkerTrait::Clone));
        assert!(!registry.implements_marker(&set, MarkerTrait::Copy));
        assert!(!registry.implements_marker(&set, MarkerTrait::Frozen));
    }

    #[test]
    fn test_hashset_not_send_when_element_not_send() {
        let registry = TraitRegistry::new();
        let ptr = Ty::Pointer {
            pointee: Box::new(Ty::I32),
            is_mutable: false,
        };
        let set_ptr = Ty::Named {
            builtin: None,
            name: "HashSet".to_string(),
            args: vec![ptr],
        };
        assert!(!registry.is_send(&set_ptr));
    }

    #[test]
    fn test_hashset_rc_element_is_not_send() {
        // Rc<T> is explicitly non-Send (single-threaded ref-count). HashSet<Rc<T>>
        // must remain non-Send even after the HashSet Send-marker fix, exercising
        // the element-propagation path with Rc rather than a raw pointer.
        let registry = TraitRegistry::new();
        let rc_i32 = Ty::Named {
            builtin: None,
            name: "Rc".to_string(),
            args: vec![Ty::I32],
        };
        let set_rc = Ty::Named {
            builtin: None,
            name: "HashSet".to_string(),
            args: vec![rc_i32],
        };
        assert!(!registry.is_send(&set_rc));
        assert!(!registry.is_sync(&set_rc));
    }

    #[test]
    fn test_rcfree_rejects_transitive_rc_through_registered_members() {
        let mut registry = TraitRegistry::new();
        registry.register_rcfree_members("Holder".to_string(), vec![Ty::rc(Ty::I32)]);
        let holder = Ty::Named {
            builtin: None,
            name: "Holder".to_string(),
            args: vec![],
        };

        assert!(!registry.implements_marker(&holder, MarkerTrait::RcFree));
        assert!(
            registry.implements_marker(&Ty::Tuple(vec![Ty::I32, Ty::Bool]), MarkerTrait::RcFree)
        );
    }

    #[test]
    fn local_pid_with_rc_bearing_actor_is_rc_free() {
        let mut registry = TraitRegistry::new();
        registry.register_rcfree_members("Worker".to_string(), vec![Ty::rc(Ty::I32)]);
        let handle = Ty::local_pid(Ty::Named {
            builtin: None,
            name: "Worker".to_string(),
            args: vec![],
        });

        assert_eq!(registry.rc_free_status(&handle), RcFreeStatus::RcFree);
        assert!(registry.implements_marker(&handle, MarkerTrait::RcFree));
    }

    #[test]
    fn local_pid_simple_is_rc_free() {
        let mut registry = TraitRegistry::new();
        registry.register_rcfree_members("SimpleActor".to_string(), vec![Ty::I32, Ty::Bool]);
        let handle = Ty::local_pid(Ty::Named {
            builtin: None,
            name: "SimpleActor".to_string(),
            args: vec![],
        });

        assert_eq!(registry.rc_free_status(&handle), RcFreeStatus::RcFree);
        assert!(registry.implements_marker(&handle, MarkerTrait::RcFree));
    }

    #[test]
    fn vec_rc_in_named_still_contains_rc() {
        let registry = TraitRegistry::new();
        let vec_rc = Ty::Named {
            builtin: None,
            name: "Vec".to_string(),
            args: vec![Ty::rc(Ty::I32)],
        };

        assert_eq!(registry.rc_free_status(&vec_rc), RcFreeStatus::ContainsRc);
        assert!(!registry.implements_marker(&vec_rc, MarkerTrait::RcFree));
    }

    #[test]
    fn mutual_local_pid_cycle_is_rc_free_not_recursive() {
        let mut registry = TraitRegistry::new();
        let a = Ty::Named {
            builtin: None,
            name: "A".to_string(),
            args: vec![],
        };
        let b = Ty::Named {
            builtin: None,
            name: "B".to_string(),
            args: vec![],
        };
        registry.register_rcfree_members("A".to_string(), vec![Ty::local_pid(b.clone())]);
        registry.register_rcfree_members("B".to_string(), vec![Ty::local_pid(a.clone())]);

        let a_ref = Ty::local_pid(a);
        let b_ref = Ty::local_pid(b);

        assert_eq!(registry.rc_free_status(&a_ref), RcFreeStatus::RcFree);
        assert_eq!(registry.rc_free_status(&b_ref), RcFreeStatus::RcFree);
        assert!(registry.implements_marker(&a_ref, MarkerTrait::RcFree));
        assert!(registry.implements_marker(&b_ref, MarkerTrait::RcFree));
    }

    #[test]
    fn test_rcfree_rejects_recursive_named_types_without_proof() {
        let mut registry = TraitRegistry::new();
        let list = Ty::Named {
            builtin: None,
            name: "List".to_string(),
            args: vec![],
        };
        registry.register_rcfree_members("List".to_string(), vec![Ty::option(list.clone())]);

        assert_eq!(
            registry.rc_free_status(&list),
            RcFreeStatus::Recursive("List".to_string())
        );
        assert!(!registry.implements_marker(&list, MarkerTrait::RcFree));
    }

    #[test]
    fn test_rcfree_rejects_mutually_recursive_named_types_without_proof() {
        let mut registry = TraitRegistry::new();
        let a = Ty::Named {
            builtin: None,
            name: "A".to_string(),
            args: vec![],
        };
        let b = Ty::Named {
            builtin: None,
            name: "B".to_string(),
            args: vec![],
        };
        registry.register_rcfree_members("A".to_string(), vec![b.clone()]);
        registry.register_rcfree_members("B".to_string(), vec![a.clone()]);

        assert_eq!(
            registry.rc_free_status(&a),
            RcFreeStatus::Recursive("A".to_string())
        );
        assert_eq!(
            registry.rc_free_status(&b),
            RcFreeStatus::Recursive("B".to_string())
        );
        assert!(!registry.implements_marker(&a, MarkerTrait::RcFree));
        assert!(!registry.implements_marker(&b, MarkerTrait::RcFree));
    }

    #[test]
    fn rcfree_admits_recursion_broken_by_vec_indirection() {
        // W5.016 F4: `enum RedisReply { Array(Vec<RedisReply>); ... }` recurses
        // through a `Vec` — the heap indirection makes the value finite-size, so
        // it is RcFree-admissible (the recursive-cycle gate must NOT reject it).
        let mut registry = TraitRegistry::new();
        let reply = Ty::Named {
            builtin: None,
            name: "RedisReply".to_string(),
            args: vec![],
        };
        let vec_of_reply = Ty::Named {
            builtin: Some(BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![reply.clone()],
        };
        registry.register_rcfree_members("RedisReply".to_string(), vec![vec_of_reply]);

        assert_eq!(registry.rc_free_status(&reply), RcFreeStatus::RcFree);
        assert!(registry.implements_marker(&reply, MarkerTrait::RcFree));
    }

    #[test]
    fn rcfree_rejects_direct_value_cycle_not_broken_by_indirection() {
        // W5.016 F4 negative guard: `enum E { V(E) }` recurses by VALUE (no heap
        // indirection) — infinite-size, MUST stay rejected. Over-admitting this
        // would push an infinite-size value type to codegen (unbounded layout
        // recursion / stack overflow).
        let mut registry = TraitRegistry::new();
        let e = Ty::Named {
            builtin: None,
            name: "E".to_string(),
            args: vec![],
        };
        registry.register_rcfree_members("E".to_string(), vec![e.clone()]);

        assert_eq!(
            registry.rc_free_status(&e),
            RcFreeStatus::Recursive("E".to_string())
        );
        assert!(!registry.implements_marker(&e, MarkerTrait::RcFree));
    }

    #[test]
    fn marker_derivation_terminates_on_vec_indirected_recursive_enum() {
        // W5.016 F4: the marker derivation must be total on a recursive type
        // graph reachable through a collection. Before the recursion guard, a
        // `Vec<Self>`-bearing enum overflowed the stack while deriving any
        // structural marker. The enum is NOT Copy (heap-owning Vec field) but
        // the derivation must terminate to report that.
        let mut registry = TraitRegistry::new();
        let reply = Ty::Named {
            builtin: None,
            name: "RedisReply".to_string(),
            args: vec![],
        };
        let vec_of_reply = Ty::Named {
            builtin: Some(BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![reply.clone()],
        };
        registry.register_type("RedisReply".to_string(), vec![vec_of_reply]);

        // Terminates (no overflow) and a Vec-bearing enum is not Copy.
        assert!(!registry.implements_marker(&reply, MarkerTrait::Copy));
    }

    #[test]
    fn test_rcfree_falls_back_to_unqualified_registered_name() {
        let mut registry = TraitRegistry::new();
        registry.register_rcfree_members("Holder".to_string(), vec![Ty::rc(Ty::I32)]);
        let qualified_holder = Ty::Named {
            builtin: None,
            name: "widgets.Holder".to_string(),
            args: vec![],
        };

        assert!(!registry.implements_marker(&qualified_holder, MarkerTrait::RcFree));
    }

    #[test]
    fn test_closure_with_send_captures_is_send() {
        let registry = TraitRegistry::new();
        let closure = Ty::Closure {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
            captures: vec![Ty::I32, Ty::String],
        };
        assert!(registry.is_send(&closure));
        assert!(registry.is_sync(&closure));
    }

    #[test]
    fn test_closure_with_non_send_capture_is_not_send() {
        let registry = TraitRegistry::new();
        let ptr = Ty::Pointer {
            pointee: Box::new(Ty::I32),
            is_mutable: false,
        };
        let closure = Ty::Closure {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
            captures: vec![Ty::I32, ptr],
        };
        assert!(!registry.is_send(&closure));
    }

    #[test]
    fn test_closure_not_copy() {
        let registry = TraitRegistry::new();
        let closure = Ty::Closure {
            params: vec![],
            ret: Box::new(Ty::Unit),
            captures: vec![Ty::I32],
        };
        assert!(!registry.implements_marker(&closure, MarkerTrait::Copy));
        assert!(registry.implements_marker(&closure, MarkerTrait::Clone));
    }

    // Fix 1: confirm Vec and HashMap are rejected at the RemotePid serialization
    // boundary.  The codegen codec walk (`emit_{ser,de}_value`) does not handle
    // collection types; admitting them via `is_serializable` would silently skip
    // serialization.  These tests are the gate-level proof that the checker
    // is already fail-closed for collections — any future change that accidentally
    // admits Vec/HashMap will break them immediately.
    #[test]
    fn vec_payload_rejected_at_serializable_boundary() {
        let registry = TraitRegistry::new();
        let vec_i64 = Ty::Named {
            builtin: Some(BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![Ty::I64],
        };
        assert!(
            !registry.is_serializable(&vec_i64),
            "Vec<i64> must NOT be Serializable at the RemotePid boundary \
             (codec walk does not support collection types)"
        );
    }

    #[test]
    fn hashmap_payload_rejected_at_serializable_boundary() {
        let registry = TraitRegistry::new();
        let map_str_i64 = Ty::Named {
            builtin: Some(BuiltinType::HashMap),
            name: "HashMap".to_string(),
            args: vec![Ty::String, Ty::I64],
        };
        assert!(
            !registry.is_serializable(&map_str_i64),
            "HashMap<String, i64> must NOT be Serializable at the RemotePid boundary \
             (codec walk does not support collection types)"
        );
    }

    /// RI-01: the `TraitRegistry` is the single source of truth for the
    /// user-declared `#[resource]` fact. After the unify the checker keeps no
    /// parallel allowlist — a resource type is recognised ONLY because it was
    /// registered here, via the registry's own `is_resource` query.
    #[test]
    fn registry_is_sole_authority_for_resource_types() {
        let mut registry = TraitRegistry::new();
        // Unregistered type is not a resource.
        assert!(!registry.is_resource("Child"));

        registry.register_resource_type("Child".to_string());
        // Registered bare name resolves through the registry alone.
        assert!(registry.is_resource("Child"));
        // Module-qualified receiver name resolves via the unqualified suffix,
        // matching the imported-handle dispatch path.
        assert!(registry.is_resource("process.Child"));
        // A different qualified type is not admitted by the suffix fallback.
        assert!(!registry.is_resource("process.Parent"));
    }

    #[test]
    fn test_vec_not_send_when_element_not_send() {
        let registry = TraitRegistry::new();
        let ptr = Ty::Pointer {
            pointee: Box::new(Ty::I32),
            is_mutable: false,
        };
        let vec_ptr = Ty::Named {
            builtin: None,
            name: "Vec".to_string(),
            args: vec![ptr],
        };
        assert!(!registry.is_send(&vec_ptr));
    }

    // =========================================================================
    // SendHalf/RecvHalf must be explicitly not-Send
    // =========================================================================

    /// `SendHalf`<T> is NOT Send (exclusive channel ownership; cannot cross actor
    /// boundary). Resource and Drop ARE true (it has a `close()` contract).
    #[test]
    fn send_half_is_not_send() {
        let registry = TraitRegistry::new();
        let sh = Ty::Named {
            builtin: Some(BuiltinType::SendHalf),
            name: "SendHalf".to_string(),
            args: vec![Ty::I64],
        };
        assert!(!registry.is_send(&sh), "SendHalf must NOT be Send");
        assert!(!registry.is_sync(&sh), "SendHalf must NOT be Sync");
        assert!(!registry.implements_marker(&sh, MarkerTrait::Copy));
        assert!(!registry.implements_marker(&sh, MarkerTrait::Clone));
        assert!(registry.implements_marker(&sh, MarkerTrait::Resource));
        assert!(registry.implements_marker(&sh, MarkerTrait::Drop));
    }

    /// `RecvHalf`<T> is NOT Send (exclusive channel ownership; cannot cross actor
    /// boundary). Resource and Drop ARE true (it has a `close()` contract).
    #[test]
    fn recv_half_is_not_send() {
        let registry = TraitRegistry::new();
        let rh = Ty::Named {
            builtin: Some(BuiltinType::RecvHalf),
            name: "RecvHalf".to_string(),
            args: vec![Ty::I64],
        };
        assert!(!registry.is_send(&rh), "RecvHalf must NOT be Send");
        assert!(!registry.is_sync(&rh), "RecvHalf must NOT be Sync");
        assert!(!registry.implements_marker(&rh, MarkerTrait::Copy));
        assert!(!registry.implements_marker(&rh, MarkerTrait::Clone));
        assert!(registry.implements_marker(&rh, MarkerTrait::Resource));
        assert!(registry.implements_marker(&rh, MarkerTrait::Drop));
    }

    /// A user struct that holds a `RecvHalf` field must NOT be Send — the
    /// not-Send-ness of `RecvHalf` propagates structurally to its container.
    /// This is the load-bearing negative guard: sending a struct containing a
    /// channel half to an actor must be rejected.
    #[test]
    fn struct_holding_recv_half_is_not_send() {
        let mut registry = TraitRegistry::new();
        let recv_half = Ty::Named {
            builtin: Some(BuiltinType::RecvHalf),
            name: "RecvHalf".to_string(),
            args: vec![Ty::I64],
        };
        // type Worker { half: RecvHalf<i64>; id: i64 }
        registry.register_type("Worker".to_string(), vec![recv_half, Ty::I64]);
        let worker = Ty::Named {
            builtin: None,
            name: "Worker".to_string(),
            args: vec![],
        };
        assert!(
            !registry.is_send(&worker),
            "Worker holding RecvHalf must NOT be Send"
        );
    }

    // =========================================================================
    // dyn Trait + Send resolves as Send (ghost-registry fix)
    // =========================================================================

    /// `dyn Trait + Send` carries an explicit Send bound: the marker derivation
    /// must recognise it and return true for Send.
    #[test]
    fn dyn_trait_plus_send_is_send() {
        let registry = TraitRegistry::new();
        let dyn_send = Ty::TraitObject {
            traits: vec![
                TraitObjectBound {
                    trait_name: "Handler".to_string(),
                    args: vec![],
                    assoc_bindings: vec![],
                },
                TraitObjectBound {
                    trait_name: "Send".to_string(),
                    args: vec![],
                    assoc_bindings: vec![],
                },
            ],
        };
        assert!(
            registry.is_send(&dyn_send),
            "dyn Handler + Send must be Send"
        );
    }

    /// `dyn Trait` without `+ Send` must NOT be Send — the control case.
    #[test]
    fn dyn_trait_without_send_is_not_send() {
        let registry = TraitRegistry::new();
        let dyn_no_send = Ty::TraitObject {
            traits: vec![TraitObjectBound {
                trait_name: "Handler".to_string(),
                args: vec![],
                assoc_bindings: vec![],
            }],
        };
        assert!(
            !registry.is_send(&dyn_no_send),
            "dyn Handler (no +Send) must NOT be Send"
        );
    }

    // =========================================================================
    // User-defined generic types derive Send from type args
    // =========================================================================

    /// Positive: `Box<T> { v: T }` with `T = i64` IS Send.
    /// Spec §4.1.2: "A type T satisfies Send if all its variant payload / field
    /// types satisfy Send." With T instantiated to i64 (which IS Send), Box<i64>
    /// must be Send.
    #[test]
    fn user_generic_struct_with_send_arg_is_send() {
        let mut registry = TraitRegistry::new();
        // Simulates: type Box<T> { v: T }
        // Registration stores the field as Ty::Named { name: "T" } — the type
        // parameter placeholder.
        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        registry.register_type("Box".to_string(), vec![t_param]);

        // Box<i64> — T instantiated to i64
        let box_i64 = Ty::Named {
            builtin: None,
            name: "Box".to_string(),
            args: vec![Ty::I64],
        };
        assert!(
            registry.is_send(&box_i64),
            "Box<i64> must be Send (T = i64 is Send)"
        );
    }

    /// Positive: `enum Tree<T> { Leaf(T); Empty }` with `T = i64` IS Send.
    /// Variant payloads are stored as structural members alongside field types.
    #[test]
    fn user_generic_enum_with_send_arg_is_send() {
        let mut registry = TraitRegistry::new();
        // Simulates: enum Tree<T> { Leaf(T); Empty }
        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        registry.register_type("Tree".to_string(), vec![t_param]);

        let tree_i64 = Ty::Named {
            builtin: None,
            name: "Tree".to_string(),
            args: vec![Ty::I64],
        };
        assert!(
            registry.is_send(&tree_i64),
            "Tree<i64> must be Send (T = i64 is Send)"
        );
    }

    /// Positive: `Msg<T> { payload: T; id: i64 }` — struct with both a type-param
    /// field and a concrete field.  With T = i64 the whole struct IS Send.
    #[test]
    fn user_generic_struct_with_concrete_and_param_fields_is_send() {
        let mut registry = TraitRegistry::new();
        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        // type Msg<T> { payload: T; id: i64 }
        registry.register_type("Msg".to_string(), vec![t_param, Ty::I64]);

        let msg_i64 = Ty::Named {
            builtin: None,
            name: "Msg".to_string(),
            args: vec![Ty::I64],
        };
        assert!(registry.is_send(&msg_i64), "Msg<i64> must be Send");
    }

    /// NEGATIVE GUARD — load-bearing fail-open prevention:
    /// `Box<T>` with `T = Rc<i64>` must NOT be Send.
    ///
    /// Rc<T> is NOT Send (single-threaded ref-count). Any container whose type
    /// argument is Rc must remain not-Send — admitting it would constitute an
    /// over-grant soundness hole (a Rc value crossing an actor boundary → data
    /// race).
    #[test]
    fn user_generic_struct_with_rc_arg_is_not_send() {
        let mut registry = TraitRegistry::new();
        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        registry.register_type("Box".to_string(), vec![t_param]);

        // Box<Rc<i64>> — Rc is explicitly not-Send
        let rc_i64 = Ty::Named {
            builtin: Some(BuiltinType::Rc),
            name: "Rc".to_string(),
            args: vec![Ty::I64],
        };
        let box_rc = Ty::Named {
            builtin: None,
            name: "Box".to_string(),
            args: vec![rc_i64],
        };
        assert!(
            !registry.is_send(&box_rc),
            "Box<Rc<i64>> must NOT be Send — Rc is not Send"
        );
    }

    /// NEGATIVE GUARD: `Box<T>` with `T = RecvHalf<i64>` must NOT be Send.
    ///
    /// `RecvHalf` is not-Send (exclusive channel ownership). Wrapping it in a
    /// user generic container must not launder it into something sendable.
    #[test]
    fn user_generic_struct_with_recv_half_arg_is_not_send() {
        let mut registry = TraitRegistry::new();
        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        registry.register_type("Box".to_string(), vec![t_param]);

        let recv_half = Ty::Named {
            builtin: Some(BuiltinType::RecvHalf),
            name: "RecvHalf".to_string(),
            args: vec![Ty::I64],
        };
        let box_recv = Ty::Named {
            builtin: None,
            name: "Box".to_string(),
            args: vec![recv_half],
        };
        assert!(
            !registry.is_send(&box_recv),
            "Box<RecvHalf<i64>> must NOT be Send — RecvHalf is not Send"
        );
    }

    /// NEGATIVE GUARD: a concrete user struct that DIRECTLY holds a `RecvHalf`
    /// field (non-generic) must stay not-Send after the fix.
    #[test]
    fn concrete_struct_holding_recv_half_is_not_send() {
        let mut registry = TraitRegistry::new();
        let recv_half = Ty::Named {
            builtin: Some(BuiltinType::RecvHalf),
            name: "RecvHalf".to_string(),
            args: vec![Ty::I64],
        };
        // type Holder { half: RecvHalf<i64>; value: i64 }
        registry.register_type("Holder".to_string(), vec![recv_half, Ty::I64]);
        let holder = Ty::Named {
            builtin: None,
            name: "Holder".to_string(),
            args: vec![],
        };
        assert!(
            !registry.is_send(&holder),
            "Holder (RecvHalf field) must NOT be Send"
        );
    }

    /// Regression guard: non-generic concrete structs whose fields are all Send
    /// must still be Send after the generic-derivation change.
    #[test]
    fn concrete_struct_with_all_send_fields_still_send_after_generic_fix() {
        let mut registry = TraitRegistry::new();
        registry.register_type("Point".to_string(), vec![Ty::I64, Ty::I64]);
        let point = Ty::Named {
            builtin: None,
            name: "Point".to_string(),
            args: vec![],
        };
        assert!(
            registry.is_send(&point),
            "Point (i64 fields) must remain Send"
        );
    }

    // =========================================================================
    // Regression for the placeholder over-grant soundness fix
    // =========================================================================

    /// Regression for the placeholder over-grant soundness hole:
    /// `type Wrapper<T> { hidden: NoSend; value: T }` — `NoSend` has an explicit
    /// negative Send fact via `register_negative_impl`.  `Wrapper<i64>` must NOT
    /// be Send even though the only type arg (`i64`) is Send.
    ///
    /// Before the fix, `NoSend` matched `is_type_param_placeholder` (no builtin,
    /// no args, no `type_fields` entry) and was silently skipped in the
    /// concrete-field pass.  The args sweep then saw only `i64` (Send) and
    /// granted Send to the whole type — a data-race / UAF soundness hole.
    #[test]
    fn negative_impl_field_not_laundered_by_generic_wrapper() {
        let mut registry = TraitRegistry::new();
        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        let no_send = Ty::Named {
            builtin: None,
            name: "NoSend".to_string(),
            args: vec![],
        };
        // NoSend is a concrete type explicitly opted out of Send.
        registry.register_negative_impl("NoSend".to_string(), MarkerTrait::Send);
        // type Wrapper<T> { hidden: NoSend; value: T }
        registry.register_type("Wrapper".to_string(), vec![no_send, t_param]);

        let wrapper_i64 = Ty::Named {
            builtin: None,
            name: "Wrapper".to_string(),
            args: vec![Ty::I64],
        };
        assert!(
            !registry.is_send(&wrapper_i64),
            "Wrapper<i64> must NOT be Send: it holds a NoSend field (registered negative impl)"
        );
        // Positive control: a wrapper with a genuine param field only IS Send.
        let t_only = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        registry.register_type("PureWrapper".to_string(), vec![t_only]);
        let pure_i64 = Ty::Named {
            builtin: None,
            name: "PureWrapper".to_string(),
            args: vec![Ty::I64],
        };
        assert!(
            registry.is_send(&pure_i64),
            "PureWrapper<i64> (no NoSend field) must remain Send"
        );
    }

    /// A generic type containing a concrete field with a negative marker fact
    /// for a marker OTHER than Send must also fail that specific marker while
    /// remaining Send-able (the negative fact is marker-scoped, not global).
    #[test]
    fn negative_impl_is_marker_scoped_not_global() {
        let mut registry = TraitRegistry::new();
        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        let bad_sync = Ty::Named {
            builtin: None,
            name: "BadSync".to_string(),
            args: vec![],
        };
        // BadSync has a negative Sync fact but NOT a negative Send fact.
        registry.register_negative_impl("BadSync".to_string(), MarkerTrait::Sync);
        // type Container<T> { bad: BadSync; item: T }
        registry.register_type("Container".to_string(), vec![bad_sync, t_param]);

        let container_i64 = Ty::Named {
            builtin: None,
            name: "Container".to_string(),
            args: vec![Ty::I64],
        };
        // Must fail Sync (BadSync has negative Sync fact, must not be skipped).
        assert!(
            !registry.implements_marker(&container_i64, MarkerTrait::Sync),
            "Container<i64> must NOT be Sync: BadSync field has negative Sync fact"
        );
    }

    // =========================================================================
    // register_type_params: authoritative placeholder detection
    // =========================================================================

    /// POSITIVE: `Envelope<T> { id: i64; payload: T }` with `T = String` IS Send.
    ///
    /// Uses `register_type_params` so the placeholder check takes the
    /// authoritative path (declared-param list) rather than the heuristic.
    /// The concrete field `i64` must be checked directly; the type-param field
    /// `T` is deferred to the args sweep which checks `String` → Send.
    #[test]
    fn register_type_params_positive_all_send_fields() {
        let mut registry = TraitRegistry::new();
        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        // type Envelope<T> { id: i64; payload: T }
        registry.register_type("Envelope".to_string(), vec![Ty::I64, t_param]);
        registry.register_type_params("Envelope".to_string(), vec!["T".to_string()]);

        let env_string = Ty::Named {
            builtin: None,
            name: "Envelope".to_string(),
            args: vec![Ty::String],
        };
        assert!(
            registry.is_send(&env_string),
            "Envelope<String> must be Send: i64 and String are both Send"
        );
    }

    /// NEGATIVE GUARD: `Envelope<T> { id: i64; payload: T }` with `T = Rc<i64>`
    /// must NOT be Send.
    ///
    /// With `register_type_params` registered, the placeholder check uses the
    /// authoritative param list. `T` is a placeholder (deferred to args sweep);
    /// the args sweep checks `Rc<i64>` → NOT Send → whole type is NOT Send.
    #[test]
    fn register_type_params_negative_non_send_arg() {
        let mut registry = TraitRegistry::new();
        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        registry.register_type("Envelope".to_string(), vec![Ty::I64, t_param]);
        registry.register_type_params("Envelope".to_string(), vec!["T".to_string()]);

        let rc_i64 = Ty::Named {
            builtin: Some(BuiltinType::Rc),
            name: "Rc".to_string(),
            args: vec![Ty::I64],
        };
        let env_rc = Ty::Named {
            builtin: None,
            name: "Envelope".to_string(),
            args: vec![rc_i64],
        };
        assert!(
            !registry.is_send(&env_rc),
            "Envelope<Rc<i64>> must NOT be Send: Rc is not Send"
        );
    }

    /// NEGATIVE GUARD (fail-closed): with `register_type_params`, a concrete
    /// field whose name is NOT in the declared param list must be checked
    /// directly, NOT skipped as a placeholder.
    ///
    /// Setup:
    ///   - `ConcreteBad` is a REAL type registered with a non-Send field (`Rc<i64>`).
    ///   - `Wrapper<T>` has `ConcreteBad` as a field alongside type param `T`.
    ///   - `ConcreteBad` does NOT appear in the declared param list for `Wrapper`.
    ///
    /// Without `register_type_params`, `ConcreteBad` would pass the heuristic
    /// placeholder check (no `type_fields` entry at the time of check — if e.g.
    /// registration order is reversed) and be skipped. With the authoritative
    /// param list, `ConcreteBad` is correctly identified as concrete and checked
    /// directly, firing the non-Send derivation.
    #[test]
    fn register_type_params_concrete_field_not_skipped_as_placeholder() {
        let mut registry = TraitRegistry::new();

        // ConcreteBad has a Rc<i64> field — NOT Send.
        registry.register_type(
            "ConcreteBad".to_string(),
            vec![Ty::Named {
                builtin: Some(BuiltinType::Rc),
                name: "Rc".to_string(),
                args: vec![Ty::I64],
            }],
        );

        let t_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        let concrete_bad = Ty::Named {
            builtin: None,
            name: "ConcreteBad".to_string(),
            args: vec![],
        };
        // type Wrapper<T> { bad: ConcreteBad; item: T }
        registry.register_type("Wrapper".to_string(), vec![concrete_bad, t_param]);
        // Declare T as the only type parameter; ConcreteBad is NOT a param.
        registry.register_type_params("Wrapper".to_string(), vec!["T".to_string()]);

        let wrapper_i64 = Ty::Named {
            builtin: None,
            name: "Wrapper".to_string(),
            args: vec![Ty::I64],
        };
        assert!(
            !registry.is_send(&wrapper_i64),
            "Wrapper<i64> must NOT be Send: ConcreteBad field holds Rc<i64> (not Send)"
        );
    }

    // =========================================================================
    // Cross-module name-collision regression (lockstep write policy)
    // =========================================================================

    /// CRITICAL REGRESSION GUARD: `register_type_params` must be last-write-wins
    /// to match `register_type` / `type_fields`. When two modules export a type
    /// with the same bare name (`Collision`), the later registration overwrites
    /// `type_fields` (last-write-wins). If `register_type_params` were
    /// first-write-wins, the stale param list from the first module would remain
    /// after the second module's fields are written, making them inconsistent:
    ///
    /// - module a: `type Collision<T> { value: T }` — params `["T"]`, fields `[T_a]`
    /// - module b: `type T { rc: Rc<i64> }` — concrete, NOT Send
    /// - module b: `type Collision<U> { hidden: T }` — params `["U"]`, fields `[T_b]`
    ///
    /// After both registrations (last-write-wins fix):
    /// - bare `Collision`: params `["U"]`, fields `[T_b]` (b's definition)
    /// - `a.Collision`: params `["T"]`, fields `[T_a]` (a's snapshot at alias time)
    /// - `b.Collision`: params `["U"]`, fields `[T_b]` (b's snapshot at alias time)
    ///
    /// Checking `b.Collision<i64>`:
    /// - `b.Collision` has params `["U"]`
    /// - field `T` is NOT in `["U"]` — checked directly as concrete
    /// - `type_fields["T"]` = `[Rc<i64>]` — Rc not Send — b.Collision<i64> NOT Send ✓
    ///
    /// With the stale first-write-wins bug:
    /// - bare `Collision` kept params `["T"]` (a's list, never overwritten)
    /// - `b.Collision` copied those stale params — params `["T"]`
    /// - field `T` matched stale param `"T"` — classified as placeholder, SKIPPED
    /// - args sweep only saw i64 (Send) — wrongly granted Send (UAF hole)
    #[test]
    fn cross_module_name_collision_params_must_be_last_write_wins() {
        let mut registry = TraitRegistry::new();

        // --- module a: type Collision<T> { value: T } ---
        let t_a_param = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        registry.register_type("Collision".to_string(), vec![t_a_param]);
        registry.register_type_params("Collision".to_string(), vec!["T".to_string()]);
        // Simulate alias_type_markers("Collision", "a.Collision") when module a
        // is imported — snapshots a's fields and params under the qualified key.
        registry.alias_type_markers("Collision", "a.Collision");

        // --- module b: type T { rc: Rc<i64> } (concrete, NOT Send) ---
        let rc_i64 = Ty::Named {
            builtin: Some(BuiltinType::Rc),
            name: "Rc".to_string(),
            args: vec![Ty::I64],
        };
        registry.register_type("T".to_string(), vec![rc_i64]);
        // Non-generic; register empty params (last-write-wins sentinel).
        registry.register_type_params("T".to_string(), vec![]);

        // --- module b: type Collision<U> { hidden: T } ---
        // Field "T" here refers to b's concrete type, not a type parameter.
        let t_b_field = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        // last-write-wins: overwrites a's fields.
        registry.register_type("Collision".to_string(), vec![t_b_field]);
        // last-write-wins (the fix): overwrites a's stale params ["T"] with ["U"].
        registry.register_type_params("Collision".to_string(), vec!["U".to_string()]);
        // Snapshot b's fields and params under the qualified key.
        registry.alias_type_markers("Collision", "b.Collision");

        // b.Collision<i64>: field "T" is NOT in params ["U"] → checked directly
        // → T has Rc<i64> → NOT Send. This is the laundering hole test.
        let b_collision_i64 = Ty::Named {
            builtin: None,
            name: "b.Collision".to_string(),
            args: vec![Ty::I64],
        };
        assert!(
            !registry.is_send(&b_collision_i64),
            "b.Collision<i64> must NOT be Send: field 'T' is concrete (holds Rc<i64>). \
             Stale params would misclassify it as a placeholder and launder Send."
        );

        // a.Collision<i64>: params ["T"] snapshotted when a was aliased;
        // field "T" IS in params → placeholder → args sweep checks i64 → Send ✓.
        let a_collision_i64 = Ty::Named {
            builtin: None,
            name: "a.Collision".to_string(),
            args: vec![Ty::I64],
        };
        assert!(
            registry.is_send(&a_collision_i64),
            "a.Collision<i64> must be Send: field T is a genuine type param (= i64 here)"
        );
    }
}
