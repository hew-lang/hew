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
//! W3.001 Stage 2 introduces this substrate. The mapping is consumed
//! at Stage 3, when `#[extern_symbol]` template expansion at each
//! reachable monomorphization picks the canonical token for the
//! resolved element-type `Ty`.
//!
//! ## Fail-closed default for `Ty::Named`
//!
//! [`RuntimeCallingConvention::for_ty`] is **context-free** — it has
//! no access to the checker's `TypeDef` table — so it cannot tell a
//! heap-handle nominal (`Vec`, `HashMap`, opaque `net.Listener`,
//! `is_indirect = true`) apart from a value-record nominal
//! (`record Connection { id: i32 }`, `is_indirect = false`). The
//! context-free path therefore routes **every** [`Ty::Named`] to
//! [`RuntimeCallingConvention::LayoutDescriptor`], fail-closed: a
//! caller that needs the `Pointer` upgrade must positively prove
//! handle-ness by calling
//! [`RuntimeCallingConvention::for_ty_with_layout`] with the
//! resolved `TypeDef` table.
//!
//! This honours the substrate-first / checker-output-boundary
//! invariants (`LESSONS.md` rows `boundary-fail-closed`,
//! `checker-output-boundary`): the API itself cannot misclassify a
//! value record as a heap handle. Same family of finding as W4.011
//! ("substrate API must fail closed; downstream callers shouldn't
//! have to remember to override"). The earlier shape of `for_ty`
//! that classified every [`Ty::Named`] as [`Pointer`] left Stage 3
//! to remember to consult [`TypeDef::is_indirect`] for
//! re-discrimination; this revision moves that obligation into the
//! type signature.

use crate::check::TypeDef;
use crate::ty::Ty;
use std::collections::HashMap;

/// The C-ABI calling-convention class Hew uses for a given [`Ty`].
///
/// Every variant is defined by its codegen lowering semantics —
/// register class, pass mode (by-value vs. by-pointer), and whether
/// out-of-band layout metadata accompanies the value. Variants are
/// **not** named after stdlib symbol suffixes; see the module-level
/// comment for the substrate/contract framing.
///
/// `W3.003` Stage 2 scope: `bool` has a dedicated one-byte runtime
/// convention; `char` rides the existing 32-bit scalar convention.
/// Layout-descriptor entry points remain future work for records/tuples.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeCallingConvention {
    /// Pass-by-value in a single 32-bit general-purpose register.
    ///
    /// Covers (at the language level): `i8`, `i16`, `i32`, `u8`,
    /// `u16`, `u32`, and `char`. `char` reuses the `i32` Vec runtime
    /// path; no `_char` symbol suffix exists.
    ///
    /// W3.001 stdlib coverage: `i32` only (via the `i32` canonical
    /// token). Other 32-bit primitives are valid at the
    /// calling-convention layer but have no Vec monomorphization in
    /// the W3.001 stdlib slice.
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
    /// Covers: `i64`, `u64`. `isize` and `usize` collapse here on
    /// the native target (64-bit) and to [`Self::Scalar32`] on
    /// `wasm32` — W3.001 is conservative and routes both to
    /// [`Self::LayoutDescriptor`] until W3.017 settles the
    /// cross-target matrix.
    Scalar64,

    /// Pass-by-value in a single XMM / floating-point register.
    ///
    /// Covers: `f32` (widened to `f64` by the codegen — documented
    /// here, not in the stdlib — the widening is a
    /// calling-convention fact), and `f64`.
    Float64,

    /// Pass-by-value as a `(*const u8, usize)` slice descriptor pair
    /// (Hew's owned-string representation across the C-ABI).
    ///
    /// Covers: [`Ty::String`].
    StringSlice,

    /// Pass-by-value as a single tagged-pointer word.
    ///
    /// Covers: raw pointers ([`Ty::Pointer`]) plus every
    /// [`Ty::Named`] that is a **proven** heap-handle nominal
    /// — that is, a name whose resolved
    /// [`TypeDef::is_indirect`](crate::check::TypeDef) is
    /// `true` (`Vec`, `HashMap`, `HashSet`, `ActorRef`, opaque
    /// handles like `net.Listener`, and any user `type T {}` whose
    /// `is_indirect` is `true`).
    ///
    /// **Reachable only via [`RuntimeCallingConvention::for_ty_with_layout`]**:
    /// the context-free [`RuntimeCallingConvention::for_ty`] cannot
    /// see [`TypeDef::is_indirect`] and therefore routes every
    /// [`Ty::Named`] to [`Self::LayoutDescriptor`] (see module
    /// header). [`Ty::Pointer`] is the one shape the context-free
    /// path can prove without a lookup; it routes here directly.
    ///
    /// Excludes: records / tuples / enums whose layout is not
    /// pointer-shaped — those route to [`Self::LayoutDescriptor`].
    Pointer,

    /// Pass-by-pointer with an out-of-band `*const HewTypeLayout`
    /// descriptor (Hew's generic-runtime-ABI pattern).
    ///
    /// Covers (future-state, A249): records, tuples, enums,
    /// fixed arrays, slices, function/closure types,
    /// trait objects, the never type, the unit type, type-parameter
    /// nominals, deferred associated-type projections, and the
    /// error-recovery sentinel.
    ///
    /// **W3.001 fail-closed**: no runtime entry points exist yet
    /// for this convention. Stage 3 emits a precise
    /// `UnsupportedRuntimeCallingConvention` diagnostic naming the
    /// missing `_layout`-suffixed symbol. W3.003 wires the runtime
    /// side.
    LayoutDescriptor,
}

impl RuntimeCallingConvention {
    /// The stable token used as the `{T}` substitution payload in
    /// `#[extern_symbol]` templates.
    ///
    /// These tokens are **public stable surface** — stdlib `.hew`
    /// source depends on them. Adding or changing a token requires
    /// a dialect-authority refresh (see
    /// `.tmp/orchestration/hew-dialect-authority-*.md` §4.0).
    ///
    /// The W3.001 token set is the per-variant canonical spelling
    /// (per the no-aliases rule A250 and `LESSONS.md` row
    /// `no-type-aliases-one-canonical-name`): exactly one canonical
    /// string per variant; no synonyms.
    #[must_use]
    pub const fn canonical_token(self) -> &'static str {
        match self {
            Self::Scalar32 => "i32",
            Self::Bool => "bool",
            Self::Scalar64 => "i64",
            Self::Float64 => "f64",
            Self::StringSlice => "str",
            Self::Pointer => "ptr",
            Self::LayoutDescriptor => "layout",
        }
    }

    /// The language's **context-free** calling-convention mapping
    /// for an arbitrary [`Ty`].
    ///
    /// This function **is** the contract — not a magic table about
    /// Vec, but the language's lowering rule for every [`Ty`] that
    /// can be classified without consulting the checker's `TypeDef`
    /// table. Changes require a dialect-authority refresh.
    ///
    /// ## Fail-closed default for [`Ty::Named`]
    ///
    /// Because `for_ty` has no access to `TypeDef.is_indirect`, it
    /// cannot distinguish a heap-handle nominal (`Vec`) from a
    /// value-record nominal (`record Connection { id: i32 }`).
    /// Misclassifying a value record as [`Self::Pointer`] would
    /// silently lower a 4-byte struct as a pointer-shaped word and
    /// corrupt the C-ABI. To honour the substrate-first /
    /// boundary-fail-closed invariants, every [`Ty::Named`] routes
    /// here to [`Self::LayoutDescriptor`] — Stage 3 then fails
    /// closed with a precise `UnsupportedRuntimeCallingConvention`
    /// diagnostic until the caller positively proves handle-ness
    /// via [`Self::for_ty_with_layout`].
    ///
    /// Callers that already hold a resolved `TypeDef` table (the
    /// checker, dialect-authority validators) **must** prefer
    /// [`Self::for_ty_with_layout`].
    ///
    /// ## Totality
    ///
    /// The match is exhaustive (no wildcard arm) so a new [`Ty`]
    /// variant added in a future patch surfaces here as a compile
    /// error rather than silently routing to a default.
    #[must_use]
    pub fn for_ty(ty: &Ty) -> Self {
        match ty {
            // Scalar32: 8/16/32-bit GPR-class integers plus char.
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::U8 | Ty::U16 | Ty::U32 | Ty::Char => Self::Scalar32,

            // Bool: dedicated one-byte runtime convention.
            Ty::Bool => Self::Bool,

            // Scalar64: 64-bit GPR-class integers.
            Ty::I64 | Ty::U64 => Self::Scalar64,

            // Float64: f32 (calling-convention widening) + f64.
            Ty::F32 | Ty::F64 => Self::Float64,

            // StringSlice: the owned `string` representation.
            Ty::String => Self::StringSlice,

            // Pointer: raw pointers route here unconditionally;
            // they are the one pointer-shaped case the context-free
            // path can prove without a `TypeDef` lookup.
            Ty::Pointer { .. } => Self::Pointer,

            // LayoutDescriptor: every value-shaped Ty *and* every
            // `Ty::Named` (fail-closed: callers must consult
            // `for_ty_with_layout` to upgrade proven handles to
            // `Pointer`).
            Ty::Named { .. }
            | Ty::Bytes
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
            | Ty::Function { .. }
            | Ty::Closure { .. }
            | Ty::TraitObject { .. }
            | Ty::Task(_)
            | Ty::AssocType { .. }
            | Ty::Var(_)
            | Ty::Error => Self::LayoutDescriptor,
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
    /// The substrate-vs-discrimination invariant from this lane's
    /// cross-eco review (and the W4.011 family of findings) lives
    /// here: the only path that returns [`Self::Pointer`] for a
    /// [`Ty::Named`] is the one that consulted
    /// [`TypeDef::is_indirect`].
    #[must_use]
    pub fn for_ty_with_layout(ty: &Ty, type_defs: &HashMap<String, TypeDef>) -> Self {
        match ty {
            Ty::Named { name, .. } => {
                if let Some(td) = type_defs.get(name) {
                    if td.is_indirect {
                        Self::Pointer
                    } else {
                        Self::LayoutDescriptor
                    }
                } else {
                    // Fail-closed: unresolved nominal cannot be
                    // proven pointer-shaped. Stage 3 surfaces the
                    // missing `_layout` symbol via
                    // `UnsupportedRuntimeCallingConvention`, which
                    // is the correct diagnostic for both
                    // "value record needs layout descriptor" and
                    // "name is not registered" (both shapes mean
                    // the call cannot be lowered today).
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
    use crate::ty::TypeVar;

    // ── canonical_token: one stable spelling per variant ────────────

    #[test]
    fn canonical_tokens_are_the_documented_stable_surface() {
        assert_eq!(RuntimeCallingConvention::Scalar32.canonical_token(), "i32");
        assert_eq!(RuntimeCallingConvention::Bool.canonical_token(), "bool");
        assert_eq!(RuntimeCallingConvention::Scalar64.canonical_token(), "i64");
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
            RuntimeCallingConvention::Scalar32.canonical_token(),
            RuntimeCallingConvention::Bool.canonical_token(),
            RuntimeCallingConvention::Scalar64.canonical_token(),
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
    fn scalar32_covers_8_16_32_bit_integers() {
        for ty in [Ty::I8, Ty::I16, Ty::I32, Ty::U8, Ty::U16, Ty::U32, Ty::Char] {
            assert_eq!(
                RuntimeCallingConvention::for_ty(&ty),
                RuntimeCallingConvention::Scalar32,
                "expected Scalar32 for {ty:?}",
            );
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
    fn float64_covers_f32_and_f64() {
        assert_eq!(
            RuntimeCallingConvention::for_ty(&Ty::F32),
            RuntimeCallingConvention::Float64,
            "f32 widens to f64 at the calling-convention layer",
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
        let vec_i32 = Ty::Named {
            builtin: None,
            name: "Vec".to_string(),
            args: vec![Ty::I32],
        };
        let user_record = Ty::Named {
            builtin: None,
            name: "Connection".to_string(),
            args: vec![],
        };
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

    fn make_type_def(name: &str, is_indirect: bool) -> (String, TypeDef) {
        use crate::check::TypeDefKind;
        (
            name.to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: name.to_string(),
                type_params: vec![],
                fields: std::collections::HashMap::new(),
                field_order: vec![],
                variants: std::collections::HashMap::new(),
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

        let vec_i32 = Ty::Named {
            builtin: None,
            name: "Vec".to_string(),
            args: vec![Ty::I32],
        };
        assert_eq!(
            RuntimeCallingConvention::for_ty_with_layout(&vec_i32, &type_defs),
            RuntimeCallingConvention::Pointer,
            "is_indirect = true → proven handle → Pointer",
        );
    }

    #[test]
    fn for_ty_with_layout_routes_value_record_to_layout_descriptor() {
        // Regression: user `type Connection { id: i32 }` (no
        // is_indirect) must classify as LayoutDescriptor, NOT
        // Pointer. This is the W4.011-family invariant the
        // substrate-vs-discrimination cross-eco finding pinned.
        let mut type_defs = std::collections::HashMap::new();
        let (name, td) = make_type_def("Connection", false);
        type_defs.insert(name, td);

        let connection = Ty::Named {
            builtin: None,
            name: "Connection".to_string(),
            args: vec![],
        };
        assert_eq!(
            RuntimeCallingConvention::for_ty_with_layout(&connection, &type_defs),
            RuntimeCallingConvention::LayoutDescriptor,
            "value record (is_indirect = false) must NOT be lowered as Pointer",
        );
    }

    #[test]
    fn for_ty_with_layout_routes_unresolved_named_to_layout_descriptor() {
        // Fail-closed: an unresolved nominal cannot be silently
        // treated as a pointer just because we lack a TypeDef entry.
        let type_defs = std::collections::HashMap::new();
        let unknown = Ty::Named {
            builtin: None,
            name: "UnknownType".to_string(),
            args: vec![],
        };
        assert_eq!(
            RuntimeCallingConvention::for_ty_with_layout(&unknown, &type_defs),
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
                RuntimeCallingConvention::for_ty_with_layout(&ty, &type_defs),
                RuntimeCallingConvention::for_ty(&ty),
                "with-layout must agree with context-free for {ty:?}",
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
            RuntimeCallingConvention::for_ty_with_layout(&ptr, &type_defs),
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
            Ty::Function {
                params: vec![Ty::I32],
                ret: Box::new(Ty::Unit),
            },
            Ty::Closure {
                params: vec![],
                ret: Box::new(Ty::Unit),
                captures: vec![],
            },
            Ty::TraitObject { traits: vec![] },
            Ty::Task(Box::new(Ty::I32)),
            Ty::AssocType {
                base: Box::new(Ty::Named {
                    builtin: None,
                    name: "I".to_string(),
                    args: vec![],
                }),
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
