//! Function-mono kind marker and type alias.
//!
//! Function monomorphisation has no const-generic surface in v0.5, so
//! [`Function::ConstArg = ()`]. The mangling specialisation
//! [`super::MonoKey::mangle_no_const_args`] applies to
//! [`FunctionMonoKey`] directly.
//!
//! The legacy [`crate::monomorph::MonoKey`] struct (3 fields, no
//! `const_args`) remains in place for source compatibility with every
//! current call site; this module's [`FunctionMonoKey`] is the new
//! parametric equivalent that downstream callers should prefer.

use hew_types::ResolvedTy;

use super::{MonoKey, MonoKind, SymbolClass};

/// Zero-sized marker selecting the function-monomorphisation kind.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Function;

impl MonoKind for Function {
    type TypeArg = ResolvedTy;
    type ConstArg = ();
    const SYMBOL_CLASS: SymbolClass = SymbolClass::Function;
}

/// Type alias for the parametric function-mono key shape.
///
/// `MonoKey<Function>` carries an empty `Vec<()>` for `const_args` and
/// uses [`SymbolClass::Function`] when mangled — that class produces a
/// symbol byte-compatible with the legacy [`crate::monomorph::mangle`]
/// helper, so a `FunctionMonoKey` can be substituted anywhere the
/// legacy `mangle()` output is consumed.
pub type FunctionMonoKey = MonoKey<Function>;

/// Convert the legacy [`crate::monomorph::MonoKey`] into a
/// [`FunctionMonoKey`]. Used as a migration bridge: a producer that
/// still emits the legacy 3-field struct can feed the new
/// [`super::mangle_instantiation`] helper without a struct-literal
/// rewrite.
impl From<crate::monomorph::MonoKey> for FunctionMonoKey {
    fn from(legacy: crate::monomorph::MonoKey) -> Self {
        Self::new(legacy.origin, legacy.origin_name, legacy.type_args)
    }
}
