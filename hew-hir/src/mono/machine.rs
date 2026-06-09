//! Machine-mono kind marker and key alias.
//!
//! Machine monomorphisation admits both type-args and const-args.
//! `const_args` is empty in v0.5 baseline (no machine declaration
//! today carries a const-generic parameter); a forthcoming
//! const-generics introduction populates the slot with constexpr-
//! evaluated values represented by [`super::ConstValue`] (currently a
//! placeholder; see that type's documentation for the migration plan).
//!
//! Today the registry consumer wires `MachineMonoKey` into a new
//! `machine_instantiations` table on the lowered module; that table
//! lands in a follow-on commit on this branch.

use hew_types::ResolvedTy;

use super::{ConstValue, MonoKey, MonoKind, SymbolClass};

/// Zero-sized marker selecting the machine-monomorphisation kind.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Machine;

impl MonoKind for Machine {
    type TypeArg = ResolvedTy;
    type ConstArg = ConstValue;
    const SYMBOL_CLASS: SymbolClass = SymbolClass::Machine;
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
