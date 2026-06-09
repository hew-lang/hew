//! Canonical type descriptor contract for the v0.5 IR ladder.
//!
//! [`TypeDescriptor`] is a type alias for [`ResolvedTy`] — there is no
//! parallel representation. This module attaches the contract surface:
//! `canonical_string()` (the single mangling source), `is_native_wire()` (true
//! iff the type maps to a non-`Nested` primitive wire kind), and
//! `from_ty_strict_generic_args` (the fail-closed conversion for THIR→MIR
//! layer transitions that must not silently accept un-arged generic types).
//!
//! Wire-kind dispatch (`wire_kind() -> PrimitiveWireKind`) lives in
//! `hew-wirecodec::wire_boundary` as an extension trait on `ResolvedTy`,
//! because `PrimitiveWireKind` is owned by `hew-wirecodec` and that crate
//! already depends on `hew-types`.
//!
//! LESSONS upheld: `checker-output-boundary`, `checker-authority`,
//! `type-info-survival`, `boundary-fail-closed`.

use crate::resolved_ty::{BoundaryError, ResolvedTy};
use crate::ty::Ty;

/// The canonical type descriptor that crosses every layer of the v0.5 IR ladder.
///
/// This is a type alias, not a separate type. There is exactly one
/// representation: [`ResolvedTy`]. Downstream consumers (codegen, wire codecs,
/// MLIR dialect emitter) consume this alias so that the authority is clear
/// without forking the type.
pub type TypeDescriptor = ResolvedTy;

impl ResolvedTy {
    /// Canonical string representation — the single source of truth for
    /// monomorphisation mangling, `PrimitiveWireKind::Nested(name)` carriage,
    /// and IR dump identity.
    ///
    /// The output is deterministic, stable across whitespace and binding
    /// renames, and does not depend on source spans.
    ///
    /// # Canonical form
    ///
    /// Primitives: `"bool"`, `"i8"`, `"i16"`, `"i32"`, `"i64"`, `"u8"`,
    /// `"u16"`, `"u32"`, `"u64"`, `"f32"`, `"f64"`, `"char"`, `"string"`,
    /// `"bytes"`, `"duration"`, `"unit"`, `"never"`.
    ///
    /// Composites: `"(a,b)"` (tuple), `"[t;n]"` (array), `"[t]"` (slice),
    /// `"Name"` (bare named), `"Name<a,b>"` (generic named),
    /// `"fn(p1,p2)->r"` (function), `"closure(p)->r{cap}"` (closure),
    /// `"*mut t"` / `"*const t"` (pointer), `"dyn(t1+t2)"` (trait object).
    ///
    /// One-element tuples are represented structurally as `"(a)"`. They remain
    /// distinct from the bare element canonical string `"a"`.
    ///
    /// # Named-type precondition
    ///
    /// `ResolvedTy::Named.name` is emitted as-is. If module identity matters,
    /// callers must construct `ResolvedTy::Named` with an already canonical,
    /// module-qualified name; `canonical_string()` does not resolve or qualify
    /// bare names.
    #[must_use]
    pub fn canonical_string(&self) -> String {
        match self {
            ResolvedTy::I8 => "i8".into(),
            ResolvedTy::I16 => "i16".into(),
            ResolvedTy::I32 => "i32".into(),
            ResolvedTy::I64 => "i64".into(),
            ResolvedTy::U8 => "u8".into(),
            ResolvedTy::U16 => "u16".into(),
            ResolvedTy::U32 => "u32".into(),
            ResolvedTy::U64 => "u64".into(),
            ResolvedTy::Isize => "isize".into(),
            ResolvedTy::Usize => "usize".into(),
            ResolvedTy::F32 => "f32".into(),
            ResolvedTy::F64 => "f64".into(),
            ResolvedTy::Bool => "bool".into(),
            ResolvedTy::Char => "char".into(),
            ResolvedTy::String => "string".into(),
            ResolvedTy::Bytes => "bytes".into(),
            ResolvedTy::Duration => "duration".into(),
            ResolvedTy::Unit => "unit".into(),
            ResolvedTy::Never => "never".into(),
            ResolvedTy::Tuple(elems) => {
                let inner: Vec<String> = elems.iter().map(Self::canonical_string).collect();
                format!("({})", inner.join(","))
            }
            ResolvedTy::Array(elem, n) => {
                format!("[{};{}]", elem.canonical_string(), n)
            }
            ResolvedTy::Slice(elem) => {
                format!("[{}]", elem.canonical_string())
            }
            ResolvedTy::Named { name, args } if args.is_empty() => name.clone(),
            ResolvedTy::Named { name, args } => {
                let arg_strs: Vec<String> = args.iter().map(Self::canonical_string).collect();
                format!("{}<{}>", name, arg_strs.join(","))
            }
            ResolvedTy::Function { params, ret } => {
                let param_strs: Vec<String> = params.iter().map(Self::canonical_string).collect();
                format!("fn({})->{}", param_strs.join(","), ret.canonical_string())
            }
            ResolvedTy::Closure {
                params,
                ret,
                captures,
            } => {
                let param_strs: Vec<String> = params.iter().map(Self::canonical_string).collect();
                let cap_strs: Vec<String> = captures.iter().map(Self::canonical_string).collect();
                format!(
                    "closure({})->{}{{{}}}",
                    param_strs.join(","),
                    ret.canonical_string(),
                    cap_strs.join(",")
                )
            }
            ResolvedTy::Pointer {
                is_mutable: true,
                pointee,
            } => {
                format!("*mut {}", pointee.canonical_string())
            }
            ResolvedTy::Pointer {
                is_mutable: false,
                pointee,
            } => {
                format!("*const {}", pointee.canonical_string())
            }
            ResolvedTy::TraitObject { traits } => {
                let bound_strs: Vec<String> = traits
                    .iter()
                    .map(|b| {
                        if b.args.is_empty() && b.assoc_bindings.is_empty() {
                            b.trait_name.clone()
                        } else {
                            let mut arg_strs: Vec<String> =
                                b.args.iter().map(Self::canonical_string).collect();
                            arg_strs.extend(
                                b.assoc_bindings
                                    .iter()
                                    .map(|(name, ty)| format!("{name}={}", ty.canonical_string())),
                            );
                            format!("{}<{}>", b.trait_name, arg_strs.join(","))
                        }
                    })
                    .collect();
                format!("dyn({})", bound_strs.join("+"))
            }
            // Task<T> is compiler-internal and not wire-serialisable; the
            // canonical string uses the same `<task<T>>` spelling as `Display`
            // so diagnostic output is consistent.
            ResolvedTy::Task(inner) => format!("<task<{}>>", inner.canonical_string()),
        }
    }

    /// `true` if this type maps to a non-`Nested` primitive wire kind.
    ///
    /// The native-wire types are the scalar and UTF-8/binary buffer primitives
    /// that the wire codec can encode without a peer schema declaration:
    /// `Bool`, all integer widths, floats, `Char`, `String`, `Bytes`,
    /// `Duration`, and `Unit` (encoded as msgpack `nil` through the wire
    /// codec's explicit unit/nil primitive kind).
    ///
    /// Composite types (`Tuple`, `Array`, `Slice`), named types, function/
    /// closure types, pointers, and trait objects are all non-native.
    #[must_use]
    pub fn is_native_wire(&self) -> bool {
        matches!(
            self,
            ResolvedTy::Bool
                | ResolvedTy::I8
                | ResolvedTy::I16
                | ResolvedTy::I32
                | ResolvedTy::I64
                | ResolvedTy::U8
                | ResolvedTy::U16
                | ResolvedTy::U32
                | ResolvedTy::U64
                | ResolvedTy::F32
                | ResolvedTy::F64
                | ResolvedTy::Char
                | ResolvedTy::String
                | ResolvedTy::Bytes
                | ResolvedTy::Duration
                | ResolvedTy::Unit
        )
    }

    /// Fail-closed conversion for the THIR→MIR layer transition.
    ///
    /// Like [`ResolvedTy::from_ty`] but additionally validates that a
    /// `Ty::Named` type has exactly `expected_arity` type arguments. This
    /// enforces the post–PR-#1767 invariant: an absent `type_args` on a
    /// generic-typed call site is a [`BoundaryError::GenericArityMismatch`],
    /// not a silent empty-args default.
    ///
    /// For non-`Named` types the arity check is not applicable; they fall
    /// through to [`ResolvedTy::from_ty`].
    ///
    /// # Errors
    ///
    /// - All errors from [`ResolvedTy::from_ty`].
    /// - [`BoundaryError::GenericArityMismatch`] when a `Ty::Named` type has
    ///   a different number of type arguments than `expected_arity`.
    pub fn from_ty_strict_generic_args(
        ty: &Ty,
        expected_arity: usize,
    ) -> Result<Self, BoundaryError> {
        if let Ty::Named { args, .. } = ty {
            if args.len() != expected_arity {
                return Err(BoundaryError::GenericArityMismatch {
                    expected: expected_arity,
                    got: args.len(),
                });
            }
        }
        Self::from_ty(ty)
    }
}
