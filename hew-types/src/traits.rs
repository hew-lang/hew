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
        self.rc_free_members.get(name).or_else(|| {
            name.rsplit_once('.')
                .and_then(|(_, unqualified)| self.rc_free_members.get(unqualified))
        })
    }

    /// Look up `Serializable` members by exact or module-qualified/unqualified name.
    fn serializable_members_any(&self, name: &str) -> Option<&Vec<Ty>> {
        self.serializable_members.get(name).or_else(|| {
            name.rsplit_once('.')
                .and_then(|(_, unqualified)| self.serializable_members.get(unqualified))
        })
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
            // ActorRef<T>/LocalPid<T>/RemotePid<T> are opaque identity references;
            // their T parameter is phantom for rc-free structural checks.
            Ty::Named {
                builtin:
                    Some(BuiltinType::ActorRef | BuiltinType::LocalPid | BuiltinType::RemotePid),
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
                .any(|ht| ht.rsplit('.').next() == Some(name))
    }

    /// Check if a name is a drop type (qualified or unqualified).
    fn is_drop_type_any(&self, name: &str) -> bool {
        self.drop_types.contains(name)
            || self
                .drop_types
                .iter()
                .any(|dt| dt.rsplit('.').next() == Some(name))
    }

    /// Register a `#[resource]` type by its declared (bare) name.
    ///
    /// Called at type-decl registration for every user/stdlib `#[resource]`
    /// declaration. The registry is the single authority for this fact;
    /// `is_resource` is the only query path.
    pub fn register_resource_type(&mut self, name: String) {
        self.resource_types.insert(name);
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
        let unqualified = name.rsplit_once('.').map_or(name, |(_, suffix)| suffix);
        self.resource_types.contains(unqualified)
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
    /// - `ActorRef` is always `Send` and `Frozen`
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
            | Ty::Unit
            | Ty::Error
            | Ty::Never => !matches!(marker, MarkerTrait::Resource),

            // Floats: most traits but NOT Eq, Ord, Hash (NaN issues), NOT Resource (value type)
            Ty::F32 | Ty::F64 | Ty::FloatLiteral => !matches!(
                marker,
                MarkerTrait::Eq | MarkerTrait::Ord | MarkerTrait::Hash | MarkerTrait::Resource
            ),

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

            // ActorRef: always Send + Sync + Frozen + Copy (identity reference)
            Ty::Named {
                builtin: Some(BuiltinType::ActorRef),
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

            // LocalPid<T>: process-local actor pid returned by `spawn`.
            // Same marker set as ActorRef — an opaque identity reference, not a resource.
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

            // RemotePid<T>: remote actor pid, from peer-discovery or explicit construction.
            // Same marker set as LocalPid — the pid value itself is an opaque u64.
            Ty::Named {
                builtin: Some(BuiltinType::RemotePid),
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

            // Actor<T>: the built-in lambda-actor handle; always Send + Sync + Clone + Debug.
            // Actor references are channel handles — safe to capture in spawned actors.
            Ty::Named {
                builtin: Some(BuiltinType::Actor),
                ..
            } => matches!(
                marker,
                MarkerTrait::Send | MarkerTrait::Sync | MarkerTrait::Clone | MarkerTrait::Debug
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
                // Rc<T>: reference-counted, single-threaded — explicitly NOT Send or Sync.
                // Supports Clone (inc ref-count) and Drop (dec ref-count); NOT Copy or Frozen.
                if *builtin == Some(BuiltinType::Rc) {
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
                            fields
                                .iter()
                                .all(|f| self.implements_marker_guarded(f, marker, visiting))
                        }
                    }
                } else if let Some(fields) = self.type_fields.get(name).cloned() {
                    fields
                        .iter()
                        .all(|f| self.implements_marker_guarded(f, marker, visiting))
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

            // Trait objects: check if any of the traits has the bound
            Ty::TraitObject { traits } => traits.iter().any(|bound| {
                if let Some(trait_def) = self.trait_decls.get(&bound.trait_name) {
                    trait_def
                        .super_traits
                        .iter()
                        .any(|s| s == &marker.to_string())
                } else {
                    false
                }
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
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_actor_ref_is_send_and_frozen() {
        let registry = TraitRegistry::new();
        let actor_ref = Ty::actor_ref(Ty::Named {
            builtin: None,
            name: "MyActor".to_string(),
            args: vec![],
        });
        assert!(registry.is_send(&actor_ref));
        assert!(registry.is_frozen(&actor_ref));
    }

    #[test]
    fn test_actor_handle_is_send() {
        // Actor<T> (the lambda actor handle returned by spawn) must be Send so it can
        // be captured inside another spawned actor (pipeline pattern).
        let registry = TraitRegistry::new();
        let actor_int = Ty::Named {
            builtin: Some(BuiltinType::Actor),
            name: "Actor".to_string(),
            args: vec![Ty::I32],
        };
        assert!(registry.is_send(&actor_int));
        assert!(registry.is_sync(&actor_int));
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
    fn test_floats_not_eq() {
        let registry = TraitRegistry::new();
        assert!(!registry.implements_marker(&Ty::F64, MarkerTrait::Eq));
        assert!(!registry.implements_marker(&Ty::F32, MarkerTrait::Hash));
    }

    #[test]
    fn test_option_derives_from_inner() {
        let mut registry = TraitRegistry::new();
        registry.register_type("Option".to_string(), vec![Ty::I32]);
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
    fn test_actor_ref_is_sync() {
        let registry = TraitRegistry::new();
        let actor_ref = Ty::actor_ref(Ty::Named {
            builtin: None,
            name: "MyActor".to_string(),
            args: vec![],
        });
        assert!(registry.is_sync(&actor_ref));
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
    fn actorref_with_rc_bearing_actor_is_rc_free() {
        let mut registry = TraitRegistry::new();
        registry.register_rcfree_members("Worker".to_string(), vec![Ty::rc(Ty::I32)]);
        let actor_ref = Ty::actor_ref(Ty::Named {
            builtin: None,
            name: "Worker".to_string(),
            args: vec![],
        });

        assert_eq!(registry.rc_free_status(&actor_ref), RcFreeStatus::RcFree);
        assert!(registry.implements_marker(&actor_ref, MarkerTrait::RcFree));
    }

    #[test]
    fn actorref_simple_is_rc_free() {
        let mut registry = TraitRegistry::new();
        registry.register_rcfree_members("SimpleActor".to_string(), vec![Ty::I32, Ty::Bool]);
        let actor_ref = Ty::actor_ref(Ty::Named {
            builtin: None,
            name: "SimpleActor".to_string(),
            args: vec![],
        });

        assert_eq!(registry.rc_free_status(&actor_ref), RcFreeStatus::RcFree);
        assert!(registry.implements_marker(&actor_ref, MarkerTrait::RcFree));
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
    fn mutual_actorref_cycle_is_rc_free_not_recursive() {
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
        registry.register_rcfree_members("A".to_string(), vec![Ty::actor_ref(b.clone())]);
        registry.register_rcfree_members("B".to_string(), vec![Ty::actor_ref(a.clone())]);

        let a_ref = Ty::actor_ref(a);
        let b_ref = Ty::actor_ref(b);

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
}
