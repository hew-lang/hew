//! Parametric monomorphisation foundation.
//!
//! Introduces a single key shape, [`MonoKey<K>`], that every category of
//! monomorphisable item (functions, machines, actors, ...) reuses. The
//! legacy per-category structs in [`crate::monomorph`] (`MonoKey`,
//! `RecordMonoKey`, `EnumMonoKey`) remain in place for source
//! compatibility; the new parametric shape lives alongside and is what
//! every subsystem added from this point on (machine-mono discovery,
//! actor-spawn discovery, the canonical
//! [`mangle::mangle_instantiation`] helper) consumes directly.
//!
//! ## Layout
//!
//! - [`mangle`] owns the canonical symbol-mangling helper, the
//!   [`mangle::SymbolClass`] discriminator, and the
//!   [`mangle::ConstValue`] placeholder type that future const-generics
//!   work will replace with a real constexpr-evaluated value type.
//! - [`function`] holds the [`function::Function`] kind marker and a
//!   [`function::FunctionMonoKey`] type alias.
//! - [`machine`] holds the [`machine::Machine`] kind marker and the
//!   [`machine::MachineMonoKey`] type alias.
//!
//! ## Non-breaking-change discipline
//!
//! The legacy [`crate::monomorph::mangle`] keeps its signature; its
//! body is a thin wrapper around [`mangle::mangle_instantiation`] with
//! [`mangle::SymbolClass::Function`] hardcoded. A snapshot test asserts
//! every existing function-mono mangled name produced via the legacy
//! path equals the new path's output for the same inputs (see
//! `hew-hir/tests/mono_foundation_byte_compat.rs`).
//!
//! ## Class-tag uniqueness
//!
//! The mangling scheme guarantees that two instantiations with the same
//! origin name and identical type args but different
//! [`mangle::SymbolClass`] produce distinct symbols. See
//! `class_tag_uniqueness` in the same test file.

use std::fmt::Debug;
use std::hash::Hash;

use hew_types::ResolvedTy;

use crate::ids::ItemId;

pub mod function;
pub mod machine;
pub mod mangle;

pub use function::{Function, FunctionMonoKey};
pub use machine::{Machine, MachineMonoEntry, MachineMonoKey};
pub use mangle::{mangle_instantiation, ConstValue, SymbolClass};

/// Type alias for the parametric actor-mono key shape.
///
/// Pairs an `(origin: ItemId, origin_name: String, type_args:
/// Vec<ResolvedTy>, const_args: Vec<ConstValue>)` tuple. Two
/// `ActorMonoKey`s with the same actor origin but different `type_args`
/// (or, when populated, different `const_args`) are disjoint
/// instantiations and produce distinct mangled symbols.
///
/// `ActorMonoKey` mangles via [`MonoKey::mangle`] (the
/// const-arg-bearing specialisation), which routes through
/// [`mangle_instantiation`] with [`SymbolClass::Actor`] — a class
/// tag that guarantees no actor-mono symbol can collide with a
/// machine-mono or function-mono symbol sharing the same origin name
/// and type args.
///
/// The `origin` field is left as the sentinel [`ItemId::PLACEHOLDER`]
/// because the checker does not yet run a full HIR resolve
/// that assigns persistent `ItemId`s to actor declarations. The
/// actor-mono discovery pass (blocked on `MachineMonoPass` infra)
/// will back-fill the real `ItemId` from the lowered module.
pub type ActorMonoKey = MonoKey<Actor>;

/// Zero-sized marker for actor-spawn monomorphisation keys.
///
/// Lives in `mono::mod` rather than a dedicated `actor.rs` because the
/// actor-mono discovery subsystem has not yet landed; the marker is
/// here today so downstream planners can name the kind in type
/// signatures without depending on a module that does not yet exist.
/// When actor-mono lands, the marker may move to `mono::actor`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Actor;

impl MonoKind for Actor {
    type TypeArg = ResolvedTy;
    type ConstArg = ConstValue;
    const SYMBOL_CLASS: SymbolClass = SymbolClass::Actor;
}

/// Trait every kind marker (`Function`, `Machine`, `Actor`, ...)
/// implements. The associated types govern what `type_args` and
/// `const_args` slot into [`MonoKey<K>`] for that kind; the constant
/// pins the [`SymbolClass`] used when the key feeds into
/// [`mangle::mangle_instantiation`].
///
/// All current kinds use [`ResolvedTy`] for `TypeArg` (the v0.5 type
/// system has no other ground-type representation that monomorphisation
/// can key on); `ConstArg` is `()` for kinds with no const-generic
/// surface (`Function`) and [`ConstValue`] for kinds that admit
/// const-generic arguments (`Machine`, `Actor`).
pub trait MonoKind {
    /// Concrete type-argument representation. Always [`ResolvedTy`] for
    /// every current kind; the associated type is retained so a future
    /// kind whose type args are represented by a distinct shape (e.g.
    /// a `TypeBound` carrier) can join the foundation without
    /// destabilising existing kinds.
    type TypeArg: Clone + Debug + Eq + Hash;

    /// Const-argument representation. `()` for kinds with no
    /// const-generic surface; [`ConstValue`] for kinds that do. When
    /// the placeholder [`ConstValue`] is replaced by a future
    /// constexpr-evaluated value type, every kind whose `ConstArg`
    /// equals `ConstValue` picks up the new shape automatically.
    type ConstArg: Clone + Debug + Eq + Hash;

    /// Symbol-class discriminator used by
    /// [`mangle::mangle_instantiation`] to derive a class-prefixed
    /// symbol. Two keys with the same origin name and identical type
    /// args but different [`MonoKind::SYMBOL_CLASS`] mangle to distinct
    /// symbols by construction.
    const SYMBOL_CLASS: SymbolClass;
}

/// Parametric monomorphisation key.
///
/// Two keys are equal iff their `(origin, type_args, const_args)`
/// triples are equal — `origin_name` is carried for diagnostics and the
/// mangling scheme but is not part of identity (it moves with `origin`
/// since the `ItemId` uniquely names a single declaration site).
///
/// The generic parameter `K: MonoKind` selects:
/// - the element type of `type_args` (always [`ResolvedTy`] today),
/// - the element type of `const_args` (`()` for `Function`,
///   [`ConstValue`] for `Machine`/`Actor`),
/// - the [`SymbolClass`] used when this key is fed into
///   [`mangle::mangle_instantiation`] (via [`MonoKey::mangle`] or
///   [`MonoKey::mangle_no_const_args`]).
///
/// `MonoKey<K1>` and `MonoKey<K2>` for distinct `K1, K2` are disjoint
/// types — the type system prevents accidental cross-category
/// comparisons and ensures mangled symbols cannot collide silently
/// across categories.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MonoKey<K: MonoKind> {
    /// `ItemId` of the originating declaration, as allocated in the
    /// HIR first pass. Pairing on `ItemId` rather than `name` defends
    /// against module-qualified shadowing.
    pub origin: ItemId,
    /// Origin declaration name as written in source. Retained for
    /// diagnostics and the mangled-name scheme; not part of identity
    /// when paired with `origin`.
    pub origin_name: String,
    /// Concrete type arguments in source-declared order.
    pub type_args: Vec<K::TypeArg>,
    /// Concrete const arguments in source-declared order. Empty in v0.5
    /// for kinds that do not yet admit const-generics (`Function`).
    pub const_args: Vec<K::ConstArg>,
}

impl<K: MonoKind> MonoKey<K> {
    /// Build a key with empty `const_args`. Convenience constructor for
    /// the (current) common case where the kind has no const-generic
    /// surface or const args are simply not yet populated.
    #[must_use]
    pub fn new(origin: ItemId, origin_name: String, type_args: Vec<K::TypeArg>) -> Self {
        Self {
            origin,
            origin_name,
            type_args,
            const_args: Vec::new(),
        }
    }

    /// Build a key with both `type_args` and `const_args` populated.
    #[must_use]
    pub fn with_const_args(
        origin: ItemId,
        origin_name: String,
        type_args: Vec<K::TypeArg>,
        const_args: Vec<K::ConstArg>,
    ) -> Self {
        Self {
            origin,
            origin_name,
            type_args,
            const_args,
        }
    }
}

impl<K> MonoKey<K>
where
    K: MonoKind<TypeArg = ResolvedTy, ConstArg = ConstValue>,
{
    /// Mangle this key into a class-tagged, LLVM-safe symbol via
    /// [`mangle::mangle_instantiation`]. The class tag is derived from
    /// `K::SYMBOL_CLASS`.
    #[must_use]
    pub fn mangle(&self) -> String {
        mangle::mangle_instantiation(
            K::SYMBOL_CLASS,
            &self.origin_name,
            &self.type_args,
            &self.const_args,
        )
    }
}

impl<K> MonoKey<K>
where
    K: MonoKind<TypeArg = ResolvedTy, ConstArg = ()>,
{
    /// Mangle a const-arg-less key (e.g. function-mono). Specialised
    /// helper that elides the empty-const-args slice argument at the
    /// call site.
    #[must_use]
    pub fn mangle_no_const_args(&self) -> String {
        mangle::mangle_instantiation(K::SYMBOL_CLASS, &self.origin_name, &self.type_args, &[])
    }
}
