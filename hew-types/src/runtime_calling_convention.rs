//! Hew's runtime calling-convention contract.
//!
//! Each variant of [`RuntimeCallingConvention`] documents exactly how
//! the codegen lowers a value of a given [`Ty`] across the C-ABI
//! boundary into `hew-runtime` — register class, pass width, by-value
//! vs. by-pointer, and (for the layout-descriptor case) what
//! out-of-band layout metadata accompanies the value.
//!
//! This is **language** substrate — analogous to Rust's `extern_abi.rs`
//! register-class rules or Zig's `CallConv`. It is **not** a stdlib
//! symbol-name table. The mapping `Ty → RuntimeCallingConvention` is
//! total and stable; the *string token* a stdlib `#[extern_symbol]`
//! template uses for each variant is the variant's
//! [`RuntimeCallingConvention::canonical_token`] — those tokens are
//! the public stable surface contract between the language and the
//! stdlib.
//!
//! When this enum changes (variant added or semantics altered) it is a
//! language-substrate change requiring a dialect-authority refresh.
//!
//! ## Fail-closed default for `Ty::Named`
//!
//! [`RuntimeCallingConvention::for_ty`] is **context-free** — it has
//! no access to the checker's `TypeDef` table — so it cannot tell a
//! heap-handle nominal (`Vec`, `HashMap`, opaque `net.Listener`,
//! `is_indirect = true`) apart from a value-record nominal
//! (`type Connection { id: i32 }`, `is_indirect = false`). The
//! context-free path therefore routes **every** [`Ty::Named`] to
//! [`RuntimeCallingConvention::LayoutDescriptor`], fail-closed: a
//! caller that needs the `Pointer` upgrade must positively prove
//! handle-ness by calling
//! [`RuntimeCallingConvention::for_ty_with_layout`] with the
//! resolved `TypeDef` table.

use crate::ty::Ty;

/// The C-ABI calling-convention class Hew uses for a given [`Ty`].
///
/// Every variant is defined by its codegen lowering semantics —
/// register class, pass mode (by-value vs. by-pointer), and whether
/// out-of-band layout metadata accompanies the value. Variants are
/// **not** named after stdlib symbol suffixes; see the module-level
/// comment for the substrate/contract framing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeCallingConvention {
    /// Pass-by-value in an 8-bit signed integer register class.
    Scalar8Signed,

    /// Pass-by-value in an 8-bit unsigned integer register class.
    Scalar8Unsigned,

    /// Pass-by-value in a 16-bit signed integer register class.
    Scalar16Signed,

    /// Pass-by-value in a 16-bit unsigned integer register class.
    Scalar16Unsigned,

    /// Pass-by-value in a single 32-bit general-purpose register.
    ///
    /// Covers `i32`, `u32`, and `char`.
    Scalar32,

    /// Pass-by-value as Hew's canonical one-byte bool representation.
    ///
    /// Covers: [`Ty::Bool`]. Runtime Vec storage is one byte per element
    /// (`true = 1`, `false = 0`); LLVM codegen converts between Hew's
    /// stored `i8` bool locals and Rust/C-ABI `i1` bool extern calls at
    /// the runtime boundary.
    Bool,

    /// Pass-by-value in a single 64-bit general-purpose register.
    ///
    /// Covers `i64` and `u64`. `isize` and `usize` use the layout
    /// descriptor convention in the context-free mapping.
    Scalar64,

    /// Pass-by-value in a single 32-bit floating-point register.
    Float32,

    /// Pass-by-value in a 64-bit floating-point register (`f64`).
    Float64,

    /// Pass-by-value as a `(*const u8, usize)` slice descriptor pair
    /// (Hew's owned-string representation across the C-ABI).
    ///
    /// Covers: [`Ty::String`].
    StringSlice,

    /// Pass-by-value as a pointer word.
    ///
    /// Covers raw pointers, borrows, callable values, and named types proven
    /// to be indirect by [`Self::for_ty_with_layout`].
    Pointer,

    /// Pass-by-pointer with an out-of-band `*const HewTypeLayout` descriptor.
    ///
    /// This class includes aggregate and unresolved named types. Runtime
    /// entry points must support the specific operation and layout.
    LayoutDescriptor,
}

impl RuntimeCallingConvention {
    /// The stable token used as the `{T}` substitution payload in
    /// `#[extern_symbol]` templates.
    ///
    /// Standard-library templates rely on these canonical spellings.
    #[must_use]
    pub const fn canonical_token(self) -> &'static str {
        match self {
            Self::Scalar8Signed => "i8",
            Self::Scalar8Unsigned => "u8",
            Self::Scalar16Signed => "i16",
            Self::Scalar16Unsigned => "u16",
            Self::Scalar32 => "i32",
            Self::Bool => "bool",
            Self::Scalar64 => "i64",
            Self::Float32 => "f32",
            Self::Float64 => "f64",
            Self::StringSlice => "str",
            Self::Pointer => "ptr",
            Self::LayoutDescriptor => "layout",
        }
    }

    /// The language's **context-free** calling-convention mapping
    /// for an arbitrary [`Ty`].
    ///
    /// Named types use [`Self::LayoutDescriptor`] because this function has
    /// no declaration context. Callers with type definitions should use
    /// [`Self::for_ty_with_layout`] to identify indirect named types.
    ///
    /// ## Totality
    ///
    /// The match is exhaustive (no wildcard arm) so a new [`Ty`]
    /// variant added in a future patch surfaces here as a compile
    /// error rather than silently routing to a default.
    #[must_use]
    pub fn for_ty(ty: &Ty) -> Self {
        match ty {
            Ty::I8 => Self::Scalar8Signed,
            Ty::U8 => Self::Scalar8Unsigned,
            Ty::I16 => Self::Scalar16Signed,
            Ty::U16 => Self::Scalar16Unsigned,
            Ty::I32 | Ty::U32 | Ty::Char => Self::Scalar32,

            // Bool: dedicated one-byte runtime convention.
            Ty::Bool => Self::Bool,

            // Scalar64: 64-bit GPR-class integers.
            Ty::I64 | Ty::U64 => Self::Scalar64,

            Ty::F32 => Self::Float32,
            Ty::F64 => Self::Float64,

            // StringSlice: the owned `string` representation.
            Ty::String => Self::StringSlice,

            // Pointer: raw pointers route here unconditionally;
            // they are the one pointer-shaped case the context-free
            // path can prove without a `TypeDef` lookup. `&T` borrows
            // are pointer-shaped at runtime and route identically.
            //
            // Function / closure values cross the C-ABI boundary as a
            // single heap-boxed pair handle: the 16-byte closure pair
            // `{ fn_ptr, env_ptr }` is copied into a `hew_dyn_box_alloc`
            // box at the boundary and the box address rides the pointer
            // convention. This is the closure-pair container ABI contract
            // (the boxing/unboxing emission lives in codegen's vec-op
            // marshalling); it deliberately avoids a 16-byte element
            // width class and any `_layout` witness entry.
            Ty::Pointer { .. }
            | Ty::Borrow { .. }
            | Ty::GenericCallable { .. }
            | Ty::Function { .. }
            | Ty::Closure { .. } => Self::Pointer,

            // LayoutDescriptor: every value-shaped Ty *and* every
            // `Ty::Named` (fail-closed: callers must consult
            // `for_ty_with_layout` to upgrade proven handles to
            // `Pointer`).
            Ty::Named { .. }
            | Ty::Bytes
            | Ty::CancellationToken
            | Ty::Duration
            | Ty::Unit
            | Ty::Never
            | Ty::Isize
            | Ty::Usize
            | Ty::IntLiteral
            | Ty::FloatLiteral
            | Ty::Tuple(_)
            | Ty::Array(_, _)
            | Ty::Slice(_)
            | Ty::TraitObject { .. }
            | Ty::Task(_)
            | Ty::AssocType { .. }
            | Ty::Var(_)
            | Ty::Error
            | Ty::Generator { .. } => Self::LayoutDescriptor,
        }
    }

    /// The full calling-convention mapping with a resolved `TypeDef`
    /// table, used by callers that can positively prove handle-ness
    /// for named nominals.
    ///
    /// Behaves identically to [`Self::for_ty`] for every variant
    /// except [`Ty::Named`]. For named types the function looks up
    /// `type_defs[name]` and:
    ///
    /// * routes to [`Self::Pointer`] when
    ///   [`TypeDef::is_indirect`] is `true` (heap-handle nominal),
    /// * routes to [`Self::LayoutDescriptor`] when `is_indirect` is
    ///   `false` (value record),
    /// * routes to [`Self::LayoutDescriptor`] when the name is not
    ///   in `type_defs` at all (fail-closed: an unresolved nominal
    ///   cannot be silently treated as a pointer).
    ///
    #[must_use]
    pub fn for_ty_with_layout(ty: &Ty, types: crate::check::TypeDefView<'_>) -> Self {
        match ty {
            Ty::Named { head, .. } => {
                if let Some(td) = types.of(*head) {
                    if td.is_indirect {
                        Self::Pointer
                    } else {
                        Self::LayoutDescriptor
                    }
                } else {
                    // An unresolved nominal cannot be proven pointer-shaped.
                    Self::LayoutDescriptor
                }
            }
            other => Self::for_ty(other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::TypeDef;
    use crate::ty::TypeVar;

    // ── canonical_token: one stable spelling per variant ────────────

    #[test]
    fn canonical_tokens_are_the_documented_stable_surface() {
        assert_eq!(
            RuntimeCallingConvention::Scalar8Signed.canonical_token(),
            "i8"
        );
        assert_eq!(
            RuntimeCallingConvention::Scalar8Unsigned.canonical_token(),
            "u8"
        );
        assert_eq!(
            RuntimeCallingConvention::Scalar16Signed.canonical_token(),
            "i16"
        );
        assert_eq!(
            RuntimeCallingConvention::Scalar16Unsigned.canonical_token(),
            "u16"
        );
        assert_eq!(RuntimeCallingConvention::Scalar32.canonical_token(), "i32");
        assert_eq!(RuntimeCallingConvention::Bool.canonical_token(), "bool");
        assert_eq!(RuntimeCallingConvention::Scalar64.canonical_token(), "i64");
        assert_eq!(RuntimeCallingConvention::Float32.canonical_token(), "f32");
        assert_eq!(RuntimeCallingConvention::Float64.canonical_token(), "f64");
        assert_eq!(
            RuntimeCallingConvention::StringSlice.canonical_token(),
            "str"
        );
        assert_eq!(RuntimeCallingConvention::Pointer.canonical_token(), "ptr");
        assert_eq!(
            RuntimeCallingConvention::LayoutDescriptor.canonical_token(),
            "layout"
        );
    }

    #[test]
    fn canonical_tokens_are_unique() {
        // Two variants must not share a token; the no-aliases rule
        // (LESSONS row `no-type-aliases-one-canonical-name`) demands
        // exactly one canonical string per shape.
        let tokens = [
            RuntimeCallingConvention::Scalar8Signed.canonical_token(),
            RuntimeCallingConvention::Scalar8Unsigned.canonical_token(),
            RuntimeCallingConvention::Scalar16Signed.canonical_token(),
            RuntimeCallingConvention::Scalar16Unsigned.canonical_token(),
            RuntimeCallingConvention::Scalar32.canonical_token(),
            RuntimeCallingConvention::Bool.canonical_token(),
            RuntimeCallingConvention::Scalar64.canonical_token(),
            RuntimeCallingConvention::Float32.canonical_token(),
            RuntimeCallingConvention::Float64.canonical_token(),
            RuntimeCallingConvention::StringSlice.canonical_token(),
            RuntimeCallingConvention::Pointer.canonical_token(),
            RuntimeCallingConvention::LayoutDescriptor.canonical_token(),
        ];
        let mut seen = std::collections::HashSet::new();
        for tok in tokens {
            assert!(
                seen.insert(tok),
                "duplicate canonical token `{tok}` — variants must have unique tokens"
            );
        }
    }

    // ── for_ty: total over every Ty constructor ─────────────────────

    #[test]
    fn scalar32_covers_32_bit_integers_and_char() {
        for ty in [Ty::I32, Ty::U32, Ty::Char] {
            assert_eq!(
                RuntimeCallingConvention::for_ty(&ty),
                RuntimeCallingConvention::Scalar32,
                "expected Scalar32 for {ty:?}",
            );
        }
    }

    #[test]
    fn narrow_integers_route_to_dedicated_width_conventions() {
        for (ty, expected) in [
            (Ty::I8, RuntimeCallingConvention::Scalar8Signed),
            (Ty::U8, RuntimeCallingConvention::Scalar8Unsigned),
            (Ty::I16, RuntimeCallingConvention::Scalar16Signed),
            (Ty::U16, RuntimeCallingConvention::Scalar16Unsigned),
        ] {
            assert_eq!(RuntimeCallingConvention::for_ty(&ty), expected);
        }
    }

    #[test]
    fn bool_routes_to_dedicated_bool_convention() {
        assert_eq!(
            RuntimeCallingConvention::for_ty(&Ty::Bool),
            RuntimeCallingConvention::Bool,
        );
    }

    #[test]
    fn scalar64_covers_64_bit_integers() {
        for ty in [Ty::I64, Ty::U64] {
            assert_eq!(
                RuntimeCallingConvention::for_ty(&ty),
                RuntimeCallingConvention::Scalar64,
                "expected Scalar64 for {ty:?}",
            );
        }
    }

    #[test]
    fn float_conventions_preserve_declared_width() {
        assert_eq!(
            RuntimeCallingConvention::for_ty(&Ty::F32),
            RuntimeCallingConvention::Float32,
        );
        assert_eq!(
            RuntimeCallingConvention::for_ty(&Ty::F64),
            RuntimeCallingConvention::Float64,
        );
    }

    #[test]
    fn string_routes_to_string_slice() {
        assert_eq!(
            RuntimeCallingConvention::for_ty(&Ty::String),
            RuntimeCallingConvention::StringSlice,
        );
    }

    #[test]
    fn named_routes_to_layout_descriptor_in_context_free_path() {
        // Fail-closed default: the context-free `for_ty` cannot tell
        // a heap handle (Vec) from a value record (Connection)
        // because it has no access to `TypeDef.is_indirect`. Both
        // route to `LayoutDescriptor`; callers that need the
        // `Pointer` upgrade must use `for_ty_with_layout`.
        let vec_i32 = Ty::user_for_test("Vec", vec![Ty::I32]);
        let user_record = Ty::named_for_test("Connection", vec![]);
        assert_eq!(
            RuntimeCallingConvention::for_ty(&vec_i32),
            RuntimeCallingConvention::LayoutDescriptor,
            "context-free path must not silently classify Vec as Pointer; \
             callers must positively prove handle-ness via for_ty_with_layout",
        );
        assert_eq!(
            RuntimeCallingConvention::for_ty(&user_record),
            RuntimeCallingConvention::LayoutDescriptor,
        );
    }

    // ── for_ty_with_layout: handle-ness upgrade is explicit ────────

    fn make_type_def(name: &str, is_indirect: bool) -> (crate::NominalId, TypeDef) {
        use crate::check::TypeDefKind;
        (
            crate::NominalId::for_test(name),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: name.to_string(),
                type_params: vec![],
                bounds: crate::check::ParamBounds::default(),
                fields: std::collections::HashMap::new(),
                field_order: vec![],
                variants: std::collections::HashMap::new(),
                implicit_variant_fields: std::collections::HashMap::default(),
                methods: std::collections::HashMap::new(),
                doc_comment: None,
                is_indirect,
            },
        )
    }

    #[test]
    fn for_ty_with_layout_routes_indirect_named_to_pointer() {
        let mut type_defs = std::collections::HashMap::new();
        let (name, td) = make_type_def("Vec", true);
        type_defs.insert(name, td);

        let vec_i32 = Ty::user_for_test("Vec", vec![Ty::I32]);
        assert_eq!(
            RuntimeCallingConvention::for_ty_with_layout(
                &vec_i32,
                crate::check::TypeDefView::for_test(&type_defs)
            ),
            RuntimeCallingConvention::Pointer,
            "is_indirect = true → proven handle → Pointer",
        );
    }

    #[test]
    fn for_ty_with_layout_routes_value_record_to_layout_descriptor() {
        // Regression: user `type Connection { id: i32 }` (no
        // is_indirect) must classify as LayoutDescriptor, NOT
        // Pointer. This is the W4.011-family invariant the
        // substrate-vs-discrimination independent finding pinned.
        let mut type_defs = std::collections::HashMap::new();
        let (name, td) = make_type_def("Connection", false);
        type_defs.insert(name, td);

        let connection = Ty::named_for_test("Connection", vec![]);
        assert_eq!(
            RuntimeCallingConvention::for_ty_with_layout(
                &connection,
                crate::check::TypeDefView::for_test(&type_defs)
            ),
            RuntimeCallingConvention::LayoutDescriptor,
            "value record (is_indirect = false) must NOT be lowered as Pointer",
        );
    }

    #[test]
    fn for_ty_with_layout_routes_unresolved_named_to_layout_descriptor() {
        // Fail-closed: an unresolved nominal cannot be silently
        // treated as a pointer just because we lack a TypeDef entry.
        let type_defs = std::collections::HashMap::new();
        let unknown = Ty::named_for_test("UnknownType", vec![]);
        assert_eq!(
            RuntimeCallingConvention::for_ty_with_layout(
                &unknown,
                crate::check::TypeDefView::for_test(&type_defs)
            ),
            RuntimeCallingConvention::LayoutDescriptor,
        );
    }

    #[test]
    fn for_ty_with_layout_defers_to_for_ty_for_non_named_shapes() {
        // For every non-Named shape, the with-layout variant must
        // agree byte-for-byte with the context-free path — the
        // discrimination is only meaningful for Ty::Named.
        let type_defs = std::collections::HashMap::new();
        for ty in [
            Ty::I32,
            Ty::I64,
            Ty::F64,
            Ty::String,
            Ty::Bool,
            Ty::Char,
            Ty::Pointer {
                is_mutable: false,
                pointee: Box::new(Ty::I32),
            },
            Ty::Tuple(vec![Ty::I32]),
        ] {
            assert_eq!(
                RuntimeCallingConvention::for_ty_with_layout(
                    &ty,
                    crate::check::TypeDefView::for_test(&type_defs)
                ),
                RuntimeCallingConvention::for_ty(&ty),
                "with-layout must agree with context-free for {ty:?}",
            );
        }
    }

    #[test]
    fn fn_and_closure_values_route_to_pointer() {
        // Closure-pair container ABI: a fn/closure value crosses the C-ABI
        // boundary as a heap-boxed pair handle, riding the pointer
        // convention (no 16-byte width class, no `_layout` witness entry).
        let fn_ty = Ty::Function {
            capabilities: crate::CallableCapabilities::default(),
            params: vec![crate::CallableParameter::borrowed(Ty::I64)],
            ret: Box::new(Ty::I64),
        };
        let closure_ty = Ty::Closure {
            identity: crate::ty::EffectBody::Closure(crate::check::SpanKey {
                start: 0,
                end: 0,
                module_idx: 0,
            }),
            capabilities: crate::CallableCapabilities::default(),
            params: vec![],
            ret: Box::new(Ty::Unit),
            captures: vec![],
        };
        for ty in [fn_ty, closure_ty] {
            assert_eq!(
                RuntimeCallingConvention::for_ty(&ty),
                RuntimeCallingConvention::Pointer,
                "expected Pointer for {ty:?}",
            );
        }
    }

    #[test]
    fn raw_pointer_routes_to_pointer_in_both_paths() {
        // Raw pointers are the one pointer-shaped case the
        // context-free path can prove without a TypeDef lookup.
        let ptr = Ty::Pointer {
            is_mutable: false,
            pointee: Box::new(Ty::I32),
        };
        assert_eq!(
            RuntimeCallingConvention::for_ty(&ptr),
            RuntimeCallingConvention::Pointer,
        );
        let type_defs = std::collections::HashMap::new();
        assert_eq!(
            RuntimeCallingConvention::for_ty_with_layout(
                &ptr,
                crate::check::TypeDefView::for_test(&type_defs)
            ),
            RuntimeCallingConvention::Pointer,
        );
    }

    #[test]
    fn layout_descriptor_covers_value_shapes() {
        for ty in [
            Ty::Bytes,
            Ty::Duration,
            Ty::Unit,
            Ty::Never,
            Ty::Isize,
            Ty::Usize,
            Ty::IntLiteral,
            Ty::FloatLiteral,
            Ty::Tuple(vec![Ty::I32, Ty::String]),
            Ty::Array(Box::new(Ty::I32), 4),
            Ty::Slice(Box::new(Ty::I32)),
            Ty::TraitObject { traits: vec![] },
            Ty::Task(Box::new(Ty::I32)),
            Ty::AssocType {
                base: Box::new(Ty::param(crate::ParamHead::for_test("I"))),
                trait_name: "Iterator".into(),
                assoc_name: "Item".into(),
            },
            Ty::Var(TypeVar(0)),
            Ty::Error,
        ] {
            assert_eq!(
                RuntimeCallingConvention::for_ty(&ty),
                RuntimeCallingConvention::LayoutDescriptor,
                "expected LayoutDescriptor for {ty:?}",
            );
        }
    }
}
