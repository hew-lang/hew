//! Machine-mono kind marker, key alias, and the real
//! [`ConstValue`] type filling the machine-mono const-arg slot.
//!
//! Machine monomorphisation admits both type-args and const-args.
//! `const_args` is empty for declarations with no const-generic
//! parameter; declarations such as `machine M<const N: usize>` (W3.039)
//! populate the slot with a constexpr-evaluated [`ConstValue`].
//!
//! Today the registry consumer wires `MachineMonoKey` into a new
//! `machine_instantiations` table on the lowered module; that table
//! lands in a follow-on commit on this branch.

use hew_parser::ast::Span;
use hew_types::ResolvedTy;

use super::{MonoKey, MonoKind, SymbolClass};

/// Zero-sized marker selecting the machine-monomorphisation kind.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Machine;

impl MonoKind for Machine {
    type TypeArg = ResolvedTy;
    type ConstArg = ConstValue;
    const SYMBOL_CLASS: SymbolClass = SymbolClass::Machine;
}

/// Constexpr-evaluated const-argument value carried in
/// [`MachineMonoKey::const_args`].
///
/// Phase 0 (W3.039, R269=A) admits `usize` only — every other integer
/// width and every non-integer width is deferred to a follow-on
/// widening step. The enum is intentionally non-exhaustive in spirit
/// (variants will be added) but the Rust enum is plain so existing
/// `match` sites are forced to update when new variants land. The
/// rendering in [`super::mangle::mangle_const_value`] must extend
/// non-collidingly when new variants are added.
///
/// `MonoKey<Actor>` also references this type as its `ConstArg`. Both
/// kinds default to an empty `const_args` vec for declarations with no
/// const-generic parameter, so adding variants here is non-breaking
/// for those call sites.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ConstValue {
    /// `usize` const-argument value, captured as `u64` (R269=A).
    Usize(u64),
}

/// Type alias for the parametric machine-mono key shape.
///
/// Pairs an `(origin: ItemId, origin_name: String, type_args:
/// Vec<ResolvedTy>, const_args: Vec<ConstValue>)` tuple. Two
/// `MachineMonoKey`s with the same machine origin but different
/// `type_args` (or, when populated, different `const_args`) are
/// disjoint instantiations and produce distinct mangled symbols.
///
/// `MachineMonoKey` mangles via [`super::MonoKey::mangle`] (the
/// const-arg-bearing specialisation), which routes through
/// [`super::mangle_instantiation`] with
/// [`SymbolClass::Machine`] — a class tag that guarantees no
/// machine-mono symbol can collide with a function-mono symbol sharing
/// the same origin name + type args.
pub type MachineMonoKey = MonoKey<Machine>;

/// One discovered machine instantiation in the post-function-mono
/// [`crate::machine_mono::run_machine_mono_pass`] output.
///
/// Pairs the canonical [`MachineMonoKey`] with the source span of the
/// site that first observed this instantiation (declaration site for
/// uniform-path R246 monomorphic machines; first reach-through expression
/// for generic instantiations). Insertion order in
/// [`crate::node::HirModule::machine_instantiations`] is determined by
/// the discovery walk so codegen iteration is deterministic.
///
/// The `source_span` is diagnostic-only — equality and hashing are
/// governed by `key` alone — so two reach-through sites that observe
/// the same instantiation collapse to one entry and the span of the
/// first observed site is retained.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MachineMonoEntry {
    /// Canonical key — `(origin, type_args, const_args)`.
    pub key: MachineMonoKey,
    /// First-observed source span. Used by downstream diagnostics that
    /// need to cite where this instantiation entered the registry.
    pub source_span: Span,
}
