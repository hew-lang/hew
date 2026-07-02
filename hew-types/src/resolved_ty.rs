//! Checker output boundary type.
//!
//! `ResolvedTy` is the concrete, fully-resolved form of a type after the
//! checker has finished inference, defaulted numeric literals, and rejected
//! any leaked inference or error nodes. It is the wire-level form that
//! crosses from `hew-types` into `hew-mir`, `hew-codegen-rs`, and
//! `hew-lsp` — none of those downstream consumers should ever observe
//! `Ty::Var`, `Ty::Error`, `Ty::IntLiteral`, or `Ty::FloatLiteral`.
//!
//! The single authorised entry point for constructing a `ResolvedTy` from
//! a checker-internal `Ty` is [`ResolvedTy::from_ty`]. It fails closed on
//! any checker-internal state and never silently coerces. Callers are
//! expected to have already run [`Ty::materialize_literal_defaults`] (the
//! existing boundary pass) on the input; an unmaterialized literal is a
//! converter error, not a successful default.

use std::fmt;

use crate::builtin_type::BuiltinType;
use crate::ty::{TraitObjectBound, Ty, TypeVar};

/// A fully-resolved, concrete type that has crossed the checker output
/// boundary.
///
/// Variants mirror [`Ty`] except for the four checker-internal states that
/// are unrepresentable at the boundary:
///
/// - `Var(_)` — unresolved inference variable
/// - `Error` — error-recovery placeholder
/// - `IntLiteral` — numeric literal awaiting contextual defaulting
/// - `FloatLiteral` — numeric literal awaiting contextual defaulting
///
/// Producing a `ResolvedTy` therefore proves at the type level that none of
/// those four states leaked past the checker.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedTy {
    /// 8-bit signed integer
    I8,
    /// 16-bit signed integer
    I16,
    /// 32-bit signed integer
    I32,
    /// 64-bit signed integer
    I64,
    /// 8-bit unsigned integer
    U8,
    /// 16-bit unsigned integer
    U16,
    /// 32-bit unsigned integer
    U32,
    /// 64-bit unsigned integer
    U64,
    /// Platform-sized signed integer: 32-bit on WASM32, 64-bit on native.
    Isize,
    /// Platform-sized unsigned integer: 32-bit on WASM32, 64-bit on native.
    Usize,
    /// 32-bit floating point
    F32,
    /// 64-bit floating point
    F64,
    /// Boolean
    Bool,
    /// Unicode character
    Char,
    /// UTF-8 string
    String,
    /// Ref-counted byte buffer
    Bytes,
    /// Ref-counted cancellation token handle.
    CancellationToken,
    /// Duration in nanoseconds (distinct from i64)
    Duration,
    /// Unit type (void)
    Unit,
    /// Never type (diverging, `!`)
    Never,
    /// Tuple type: `(T1, T2, ...)`
    Tuple(Vec<ResolvedTy>),
    /// Fixed-size array: `[T; N]`
    Array(Box<ResolvedTy>, u64),
    /// Slice: `[T]`
    Slice(Box<ResolvedTy>),
    /// Named types (structs, enums, actors, type params).
    Named {
        /// Type name
        name: String,
        /// Generic type arguments
        args: Vec<ResolvedTy>,
        /// Compiler-known builtin discriminator. `Some(_)` means this `Named`
        /// resolved to a canonical builtin from `builtin_type.rs::BUILTIN_TYPES`
        /// during name resolution; `None` means it is a user-defined record/
        /// enum/actor name or a type parameter. Consumers that need to
        /// discriminate builtin vs user dispatch on this field, NOT on the
        /// `name` string. Propagated round-trip-totally from `Ty::Named.builtin`.
        builtin: Option<BuiltinType>,
        /// `#[opaque]`-handle discriminator. `true` means this `Named` resolved
        /// to a type declared `#[opaque]` (a pointer-width runtime handle such
        /// as `json.Value` / `cron.Expr`) during HIR name resolution; `false`
        /// means it is an ordinary user record/enum/actor or builtin.
        ///
        /// This is the type-identity carrier that lets the actor-state
        /// clone/drop classifier (`hew-mir::state_clone`) distinguish a real
        /// opaque handle from a user record/enum that merely shares its short
        /// name. Name-based resolution is fundamentally ambiguous on such a
        /// collision (`json.Value` and a user `type Value` both arrive at the
        /// classifier as `Named { name: "Value" }`); consumers that must
        /// fail-closed on opaque handles dispatch on THIS field, never on the
        /// `name` string. Stamped by `hew-hir::lower::lower_type` from the
        /// pre-collected opaque-type-decl set.
        is_opaque: bool,
    },
    /// Function type: `fn(T1, T2) -> R`.
    Function {
        /// Parameter types
        params: Vec<ResolvedTy>,
        /// Return type
        ret: Box<ResolvedTy>,
    },
    /// Closure type: like `Function` with captured variable types tracked.
    Closure {
        /// Parameter types
        params: Vec<ResolvedTy>,
        /// Return type
        ret: Box<ResolvedTy>,
        /// Types of captured variables from the enclosing scope
        captures: Vec<ResolvedTy>,
    },
    /// Pointer types (FFI).
    Pointer {
        /// Whether the pointer is mutable
        is_mutable: bool,
        /// Pointee type
        pointee: Box<ResolvedTy>,
    },
    /// Immutable borrow `&T` — a first-class, no-retain shared reference (see
    /// [`Ty::Borrow`]). Non-owning, classified `View`, retain-skipped by
    /// codegen, with borrow-specific send/return-escape semantics.
    Borrow {
        /// Borrowed (pointee) type
        pointee: Box<ResolvedTy>,
    },
    /// Trait object: `dyn Trait` or `dyn (Trait1 + Trait2)`.
    TraitObject {
        /// Trait bounds
        traits: Vec<ResolvedTraitBound>,
    },
    /// Compiler-internal type for a running child task whose result has type
    /// `T`. Produced exclusively by HIR lowering of `fork name = call_expr`
    /// inside a `fork{}` body; never user-nameable (the parser has no grammar
    /// production for `Task<T>` as a type annotation).
    ///
    /// Display: `<task<T>>` (angle brackets signal compiler-internal origin;
    /// implemented via `to_ty()` → `Ty::Task` → `fmt_with_numeric_names`).
    Task(Box<ResolvedTy>),
    /// An abstract generic type parameter, e.g. the `T` in `fn id<T>(x: T) -> T`.
    ///
    /// This is the single type-identity authority for an abstract parameter
    /// (A622 / DI-020): an unsubstituted `T` is represented *structurally*
    /// here rather than via a parallel side-table. A `ResolvedTy::TypeParam`
    /// is a concrete, boundary-legal type — it carries no inference or
    /// error state — but it stands for a type that is not yet known. It is
    /// produced when a polymorphic body is lowered without a concrete
    /// monomorphisation substitution (the generic-origin / pre-W5.008 form);
    /// monomorphisation later replaces it with a concrete `ResolvedTy`.
    ///
    /// Construction from a checker-internal [`Ty`] requires the declared
    /// type-parameter scope — see [`ResolvedTy::from_ty_with_type_params`].
    /// The unscoped [`ResolvedTy::from_ty`] never produces this variant (a
    /// bare `Ty::Named` is indistinguishable from a no-argument user type
    /// without that scope), so its behaviour is unchanged.
    ///
    /// Display: the bare parameter name (`T`), via `to_ty()` →
    /// `Ty::Named { name, args: [], builtin: None }`.
    TypeParam {
        /// The source-declared parameter name (e.g. `"T"`).
        name: String,
    },
}

/// A single trait bound in a resolved trait object.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedTraitBound {
    /// Trait name
    pub trait_name: String,
    /// Resolved type arguments
    pub args: Vec<ResolvedTy>,
    /// Resolved associated-type bindings, sorted by associated-type name.
    pub assoc_bindings: Vec<(String, ResolvedTy)>,
}

/// Reasons a checker-internal [`Ty`] cannot cross the boundary as a
/// [`ResolvedTy`].
///
/// Each variant corresponds to exactly one unrepresentable `Ty` state.
/// Conversion short-circuits on the first offender it encounters during
/// the recursive descent, so the carried span/identity is the innermost
/// leaked node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoundaryError {
    /// A `Ty::Var` leaked past inference without being resolved.
    UnresolvedInference {
        /// The leaked inference variable's id, for diagnostics.
        var: TypeVar,
    },
    /// A `Ty::Error` node from error recovery reached the boundary.
    TaintedError,
    /// A `Ty::IntLiteral` or `Ty::FloatLiteral` reached the boundary
    /// without being defaulted via [`Ty::materialize_literal_defaults`].
    UnmaterializedLiteral {
        /// Whether the literal was an integer (`true`) or float (`false`).
        is_integer: bool,
    },
    /// A [`Ty::Named`] reached `from_ty_strict_generic_args` with a type
    /// argument count that differs from the expected arity.
    GenericArityMismatch {
        /// Number of type arguments that were expected.
        expected: usize,
        /// Number of type arguments that were actually present.
        got: usize,
    },
    /// A [`Ty::AssocType`] projection survived past type-checking. Such a
    /// projection must be collapsed (via the impl's `type Bar = X` binding)
    /// before crossing the checker→HIR boundary; reaching this point means
    /// monomorphisation did not resolve the projection.
    UnresolvedAssocProjection {
        /// Trait that declares the associated type.
        trait_name: String,
        /// Associated-type name (e.g. `"Item"`).
        assoc_name: String,
    },
}

impl fmt::Display for BoundaryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoundaryError::UnresolvedInference { var } => {
                write!(f, "unresolved inference variable {var} leaked to boundary")
            }
            BoundaryError::TaintedError => {
                write!(f, "error-recovery placeholder leaked to boundary")
            }
            BoundaryError::UnmaterializedLiteral { is_integer: true } => {
                write!(f, "integer literal not defaulted before boundary")
            }
            BoundaryError::UnmaterializedLiteral { is_integer: false } => {
                write!(f, "float literal not defaulted before boundary")
            }
            BoundaryError::GenericArityMismatch { expected, got } => {
                write!(
                    f,
                    "generic arity mismatch: expected {expected} type argument(s), got {got}"
                )
            }
            BoundaryError::UnresolvedAssocProjection {
                trait_name,
                assoc_name,
            } => {
                write!(
                    f,
                    "unresolved associated-type projection `{trait_name}::{assoc_name}` leaked to boundary"
                )
            }
        }
    }
}

impl std::error::Error for BoundaryError {}

impl ResolvedTy {
    /// Returns `true` for every concrete integer type admitted by the checker.
    /// `Bool` and `Char` are deliberately excluded: `bool` participates in
    /// explicit casts only through the checker-owned bool<->integer rules, and
    /// `char` is not in `coerce.rs`'s numeric matrix.
    #[must_use]
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::I8
                | Self::I16
                | Self::I32
                | Self::I64
                | Self::U8
                | Self::U16
                | Self::U32
                | Self::U64
                | Self::Isize
                | Self::Usize
        )
    }

    /// Returns `true` when an integer literal pattern can be compared directly
    /// against a match scrutinee of this type.
    ///
    /// This is the checker/HIR/MIR semantic boundary for integer literal-match
    /// admission. Keep all downstream literal-match gates routed through this
    /// predicate so platform-sized integers cannot drift from fixed-width
    /// integers again.
    #[must_use]
    pub fn is_integer_literal_match_scrutinee(&self) -> bool {
        self.is_integer()
    }

    /// Returns `true` for the concrete floating-point types admitted by the
    /// checker numeric matrix.
    #[must_use]
    pub fn is_float(&self) -> bool {
        matches!(self, Self::F32 | Self::F64)
    }

    /// Returns `true` for integer or floating-point types. Mirrors
    /// [`Ty::is_numeric`] after checker-boundary conversion.
    #[must_use]
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Returns `true` for signed integer types, including platform-sized
    /// `isize`.
    #[must_use]
    pub fn is_signed_integer(&self) -> bool {
        matches!(
            self,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::Isize
        )
    }

    /// Returns `true` for unsigned integer types, including platform-sized
    /// `usize`.
    #[must_use]
    pub fn is_unsigned_integer(&self) -> bool {
        matches!(
            self,
            Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::Usize
        )
    }

    /// Checker-authoritative explicit `as` cast matrix after conversion to
    /// `ResolvedTy`.
    ///
    /// This mirrors `hew-types/src/check/coerce.rs::cast_is_valid`: numeric
    /// <-> numeric, `bool` -> integer, integer -> `bool`, and `char` ->
    /// integer (the Unicode scalar value / codepoint). Everything else stays
    /// fail-closed at HIR/MIR/codegen boundaries.
    #[must_use]
    pub fn can_explicitly_numeric_cast_to(&self, target: &Self) -> bool {
        (self.is_numeric() && target.is_numeric())
            || (*self == Self::Bool && target.is_integer())
            || (self.is_integer() && *target == Self::Bool)
            // `char as <integer>` extracts the codepoint as an integer.
            // Mirrors the `Ty::Char` arm in `cast_is_valid`; lowered by
            // `lower_numeric_cast`'s dedicated char->int arm (zero-extend /
            // truncate from the i32 codepoint storage).
            || (*self == Self::Char && target.is_integer())
    }

    /// Convert a checker-internal [`Ty`] into a boundary [`ResolvedTy`].
    ///
    /// This is the **single authorised conversion** from `Ty` to
    /// `ResolvedTy`. It fails closed on any leaked checker-internal state:
    ///
    /// - `Ty::Var` → [`BoundaryError::UnresolvedInference`]
    /// - `Ty::Error` → [`BoundaryError::TaintedError`]
    /// - `Ty::IntLiteral` / `Ty::FloatLiteral` →
    ///   [`BoundaryError::UnmaterializedLiteral`]
    ///
    /// Callers are expected to have run
    /// [`Ty::materialize_literal_defaults`] before converting; the
    /// converter deliberately does not silently default, because the
    /// existing boundary pass is authoritative on where literal defaults
    /// are chosen.
    ///
    /// # Errors
    ///
    /// Returns [`BoundaryError`] on the first checker-internal state
    /// encountered in a recursive descent: an unresolved inference
    /// variable, an `Ty::Error` placeholder, or an unmaterialized numeric
    /// literal. The carried data identifies the innermost offender.
    pub fn from_ty(ty: &Ty) -> Result<Self, BoundaryError> {
        Self::from_ty_scoped(ty, &std::collections::HashSet::new())
    }

    /// Scope-aware variant of [`ResolvedTy::from_ty`] that recognises the
    /// declared generic type parameters of the enclosing item.
    ///
    /// A bare `Ty::Named { name, args: [], builtin: None }` whose `name`
    /// appears in `type_params` is converted to [`ResolvedTy::TypeParam`]
    /// (the A622 abstract-parameter authority) rather than to a
    /// `ResolvedTy::Named` user type. Every other type — and every name not
    /// in scope — follows the exact same rules as [`ResolvedTy::from_ty`],
    /// which is defined as this function with an empty scope. This keeps a
    /// single conceptual boundary converter, so the two entry points cannot
    /// drift: the unscoped form simply has no type parameters in scope.
    ///
    /// # Errors
    ///
    /// Identical fail-closed behaviour to [`ResolvedTy::from_ty`]: returns
    /// the innermost [`BoundaryError`] for any leaked checker-internal state
    /// (unresolved inference variable, error placeholder, unmaterialized
    /// literal, or unresolved associated-type projection).
    pub fn from_ty_with_type_params(
        ty: &Ty,
        type_params: &std::collections::HashSet<String>,
    ) -> Result<Self, BoundaryError> {
        Self::from_ty_scoped(ty, type_params)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single exhaustive `Ty` -> `ResolvedTy` boundary conversion; \
                  every arm is a fail-closed mapping and splitting would scatter \
                  the total match across helpers"
    )]
    fn from_ty_scoped(
        ty: &Ty,
        type_params: &std::collections::HashSet<String>,
    ) -> Result<Self, BoundaryError> {
        match ty {
            Ty::I8 => Ok(ResolvedTy::I8),
            Ty::I16 => Ok(ResolvedTy::I16),
            Ty::I32 => Ok(ResolvedTy::I32),
            Ty::I64 => Ok(ResolvedTy::I64),
            Ty::U8 => Ok(ResolvedTy::U8),
            Ty::U16 => Ok(ResolvedTy::U16),
            Ty::U32 => Ok(ResolvedTy::U32),
            Ty::U64 => Ok(ResolvedTy::U64),
            Ty::Isize => Ok(ResolvedTy::Isize),
            Ty::Usize => Ok(ResolvedTy::Usize),
            Ty::F32 => Ok(ResolvedTy::F32),
            Ty::F64 => Ok(ResolvedTy::F64),
            Ty::Bool => Ok(ResolvedTy::Bool),
            Ty::Char => Ok(ResolvedTy::Char),
            Ty::String => Ok(ResolvedTy::String),
            Ty::Bytes => Ok(ResolvedTy::Bytes),
            Ty::CancellationToken => Ok(ResolvedTy::CancellationToken),
            Ty::Duration => Ok(ResolvedTy::Duration),
            Ty::Unit => Ok(ResolvedTy::Unit),
            Ty::Never => Ok(ResolvedTy::Never),
            Ty::IntLiteral => Err(BoundaryError::UnmaterializedLiteral { is_integer: true }),
            Ty::FloatLiteral => Err(BoundaryError::UnmaterializedLiteral { is_integer: false }),
            Ty::Var(var) => Err(BoundaryError::UnresolvedInference { var: *var }),
            Ty::Error => Err(BoundaryError::TaintedError),
            Ty::Tuple(elems) => Ok(ResolvedTy::Tuple(Self::convert_vec(elems, type_params)?)),
            Ty::Array(elem, size) => Ok(ResolvedTy::Array(
                Box::new(Self::from_ty_scoped(elem, type_params)?),
                *size,
            )),
            Ty::Slice(elem) => Ok(ResolvedTy::Slice(Box::new(Self::from_ty_scoped(
                elem,
                type_params,
            )?))),
            Ty::Named {
                name,
                args,
                builtin: Some(BuiltinType::CancellationToken),
            } if args.is_empty() && name == "CancellationToken" => {
                Ok(ResolvedTy::CancellationToken)
            }
            // `instant` is a monotonic timestamp in nanoseconds; its runtime
            // ABI (`hew_instant_now`/`_elapsed`/`_duration_since`) is a bare
            // `i64`, so it lowers to `ResolvedTy::I64` at the MIR boundary. The
            // checker keeps `instant` distinct (`Ty::Named { instant }`) only to
            // route method dispatch to the `impl instant` block; MIR and codegen
            // see an ordinary `i64` and need no `instant`-specific arm.
            Ty::Named {
                name,
                args,
                builtin: Some(BuiltinType::Instant),
            } if args.is_empty() && name == "instant" => Ok(ResolvedTy::I64),
            // A bare, non-builtin `Named` whose name is a declared generic
            // parameter of the enclosing item is the abstract-parameter form
            // (A622). The unscoped `from_ty` passes an empty scope, so it
            // never reaches this branch and its behaviour is unchanged.
            Ty::Named {
                name,
                args,
                builtin: None,
            } if args.is_empty() && type_params.contains(name) => {
                Ok(ResolvedTy::TypeParam { name: name.clone() })
            }
            Ty::Named {
                name,
                args,
                builtin,
            } => Ok(ResolvedTy::Named {
                name: name.clone(),
                args: Self::convert_vec(args, type_params)?,
                builtin: *builtin,
                // The checker's `Ty::Named` carries no opacity discriminator;
                // opacity is stamped downstream by `hew-hir::lower::lower_type`
                // from the opaque-type-decl set. A `ResolvedTy` produced from a
                // checker `Ty` is never the actor-state-field carrier, so
                // `false` here is correct and behaviour-preserving.
                is_opaque: false,
            }),
            Ty::Function { params, ret } => Ok(ResolvedTy::Function {
                params: Self::convert_vec(params, type_params)?,
                ret: Box::new(Self::from_ty_scoped(ret, type_params)?),
            }),
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ok(ResolvedTy::Closure {
                params: Self::convert_vec(params, type_params)?,
                ret: Box::new(Self::from_ty_scoped(ret, type_params)?),
                captures: Self::convert_vec(captures, type_params)?,
            }),
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ok(ResolvedTy::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(Self::from_ty_scoped(pointee, type_params)?),
            }),
            Ty::Borrow { pointee } => Ok(ResolvedTy::Borrow {
                pointee: Box::new(Self::from_ty_scoped(pointee, type_params)?),
            }),
            Ty::TraitObject { traits } => Ok(ResolvedTy::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| Self::convert_trait_bound(bound, type_params))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            Ty::Task(inner) => Ok(ResolvedTy::Task(Box::new(Self::from_ty_scoped(
                inner,
                type_params,
            )?))),
            Ty::AssocType {
                trait_name,
                assoc_name,
                ..
            } => Err(BoundaryError::UnresolvedAssocProjection {
                trait_name: trait_name.to_string(),
                assoc_name: assoc_name.to_string(),
            }),
        }
    }

    fn convert_vec(
        tys: &[Ty],
        type_params: &std::collections::HashSet<String>,
    ) -> Result<Vec<ResolvedTy>, BoundaryError> {
        tys.iter()
            .map(|ty| Self::from_ty_scoped(ty, type_params))
            .collect()
    }

    fn convert_trait_bound(
        bound: &TraitObjectBound,
        type_params: &std::collections::HashSet<String>,
    ) -> Result<ResolvedTraitBound, BoundaryError> {
        Ok(ResolvedTraitBound {
            trait_name: bound.trait_name.clone(),
            args: Self::convert_vec(&bound.args, type_params)?,
            assoc_bindings: bound
                .assoc_bindings
                .iter()
                .map(|(name, ty)| Ok((name.clone(), Self::from_ty_scoped(ty, type_params)?)))
                .collect::<Result<Vec<_>, BoundaryError>>()?,
        })
    }

    /// Lift a `ResolvedTy` back to a `Ty` for contexts that still consume
    /// the internal form during the co-existence window of the full
    /// checker-output cutover. It is structurally total because
    /// `ResolvedTy` has strictly fewer variants than `Ty`.
    #[must_use]
    pub fn to_ty(&self) -> Ty {
        match self {
            ResolvedTy::I8 => Ty::I8,
            ResolvedTy::I16 => Ty::I16,
            ResolvedTy::I32 => Ty::I32,
            ResolvedTy::I64 => Ty::I64,
            ResolvedTy::U8 => Ty::U8,
            ResolvedTy::U16 => Ty::U16,
            ResolvedTy::U32 => Ty::U32,
            ResolvedTy::U64 => Ty::U64,
            ResolvedTy::Isize => Ty::Isize,
            ResolvedTy::Usize => Ty::Usize,
            ResolvedTy::F32 => Ty::F32,
            ResolvedTy::F64 => Ty::F64,
            ResolvedTy::Bool => Ty::Bool,
            ResolvedTy::Char => Ty::Char,
            ResolvedTy::String => Ty::String,
            ResolvedTy::Bytes => Ty::Bytes,
            ResolvedTy::CancellationToken => Ty::CancellationToken,
            ResolvedTy::Duration => Ty::Duration,
            ResolvedTy::Unit => Ty::Unit,
            ResolvedTy::Never => Ty::Never,
            ResolvedTy::Tuple(elems) => Ty::Tuple(elems.iter().map(Self::to_ty).collect()),
            ResolvedTy::Array(elem, size) => Ty::Array(Box::new(elem.to_ty()), *size),
            ResolvedTy::Slice(elem) => Ty::Slice(Box::new(elem.to_ty())),
            ResolvedTy::Named {
                name,
                args,
                builtin,
                // `Ty::Named` carries no opacity discriminator; opacity is a
                // HIR-resolution fact that does not round-trip back into the
                // checker type. Dropped here intentionally.
                is_opaque: _,
            } => Ty::Named {
                name: name.clone(),
                builtin: *builtin,
                args: args.iter().map(Self::to_ty).collect(),
            },
            ResolvedTy::Function { params, ret } => Ty::Function {
                params: params.iter().map(Self::to_ty).collect(),
                ret: Box::new(ret.to_ty()),
            },
            ResolvedTy::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params.iter().map(Self::to_ty).collect(),
                ret: Box::new(ret.to_ty()),
                captures: captures.iter().map(Self::to_ty).collect(),
            },
            ResolvedTy::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(pointee.to_ty()),
            },
            ResolvedTy::Borrow { pointee } => Ty::Borrow {
                pointee: Box::new(pointee.to_ty()),
            },
            ResolvedTy::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound.args.iter().map(Self::to_ty).collect(),
                        assoc_bindings: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| (name.clone(), ty.to_ty()))
                            .collect(),
                    })
                    .collect(),
            },
            ResolvedTy::Task(inner) => Ty::Task(Box::new(inner.to_ty())),
            // The abstract parameter lifts back to its canonical checker-side
            // carrier — a bare, non-builtin `Named` with no arguments. This is
            // the same shape the type param had before `from_ty_with_type_params`
            // recognised it, so the round-trip is lossless within the declared
            // type-parameter scope.
            ResolvedTy::TypeParam { name } => Ty::Named {
                name: name.clone(),
                args: Vec::new(),
                builtin: None,
            },
        }
    }

    /// Construct a `Named` for a known builtin. Use this in production code
    /// when the call site holds an actual [`BuiltinType`] — it prevents
    /// "I'll just pass `None`" sites that would silently fall onto the
    /// user-record branch of every downstream discriminator.
    #[must_use]
    pub fn named_builtin(
        name: impl Into<String>,
        kind: BuiltinType,
        args: Vec<ResolvedTy>,
    ) -> Self {
        ResolvedTy::Named {
            name: name.into(),
            args,
            builtin: Some(kind),
            is_opaque: false,
        }
    }

    /// Construct a `Named` for a user-defined record/enum/actor name (or a
    /// type parameter reference). Sets `builtin: None` explicitly so the
    /// intent ("not a builtin") is named at the call site rather than being
    /// inferred from an absent field.
    #[must_use]
    pub fn named_user(name: impl Into<String>, args: Vec<ResolvedTy>) -> Self {
        ResolvedTy::Named {
            name: name.into(),
            args,
            builtin: None,
            is_opaque: false,
        }
    }

    /// Construct a `Named` for a `#[opaque]` runtime-handle type (e.g.
    /// `json.Value`, `cron.Expr`). Sets `is_opaque: true` so the actor-state
    /// clone/drop classifier recognises the handle by type identity rather
    /// than by a short-name heuristic that collides with user types of the
    /// same name. `builtin: None` — an opaque handle is a user-module decl,
    /// not a compiler builtin.
    #[must_use]
    pub fn named_opaque(name: impl Into<String>, args: Vec<ResolvedTy>) -> Self {
        ResolvedTy::Named {
            name: name.into(),
            args,
            builtin: None,
            is_opaque: true,
        }
    }

    /// User-facing display wrapper that mirrors [`Ty::user_facing`]: numeric
    /// types render with their explicit-width spellings (`i64`, `f64`, etc.).
    /// Rendering a `ResolvedTy` never exposes the unrepresentable states
    /// (they don't exist), so callers can consume this output directly without
    /// post-filtering.
    #[must_use]
    pub fn user_facing(&self) -> UserFacingResolvedTy<'_> {
        UserFacingResolvedTy(self)
    }
}

/// User-facing display wrapper for [`ResolvedTy`].
#[derive(Debug)]
pub struct UserFacingResolvedTy<'a>(&'a ResolvedTy);

impl fmt::Display for ResolvedTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Delegate to Ty's established formatting so rendered output
        // matches existing consumer expectations byte-for-byte.
        self.to_ty().fmt(f)
    }
}

impl fmt::Display for UserFacingResolvedTy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.to_ty().user_facing().fmt(f)
    }
}

// ── Symbol-key mangling for concrete specialised impls ───────────────────────
//
// When two `impl Trait for Wrapper<i64>` and `impl Trait for Wrapper<string>`
// blocks coexist, their methods are registered and looked up under the bare
// key `"Wrapper::method"` which causes the second registration to clobber the
// first, and codegen to emit two LLVM functions with the same name (the
// linkage error that #2270 surfaced). The fix: for concrete specialised impls
// (no generic type params on the impl, non-empty target type args), use a
// mangled self-type name that embeds the concrete args — e.g. `"Wrapper$$i64"`
// — so `"Wrapper$$i64::describe"` and `"Wrapper$$string::describe"` are
// distinct throughout the checker–HIR–MIR–codegen pipeline.
//
// The mangle format matches `hew-hir::monomorph::mangle` for
// `SymbolClass::Function`: `"{origin}$$" + args joined by "$"`, where each
// arg is its canonical string segment (primitives → their keyword, named
// types → their bare name, compounds → recursively rendered).
//
// These helpers live in `hew-types` (not `hew-hir`) because `hew-types` does
// not depend on `hew-hir` and the checker must compute mangled keys without
// introducing a circular dependency. `hew-hir` calls its own `mangle()` entry
// point over `ResolvedTy` values; both produce byte-identical output for the
// inputs that a concrete specialised impl target can carry.
//
// Scope: only concrete specialised impls ever produce mangled keys here.
// Generic impls (`impl<T> Trait for Wrapper<T>`) and impls with no target type
// args keep the bare name as before. This change is strictly additive on the
// checker side: when the mangled key is absent from `fn_sigs` (e.g. an
// unresolved or complex type arg), we fall back to the bare key so existing
// behaviour is preserved.

/// Render a single `ResolvedTy` as the canonical mangle segment used by
/// `hew-hir::monomorph::mangle_resolved_ty`. Returns `None` for shapes that
/// cannot be embedded (inference variables, error placeholders, abstract
/// type-parameter references).
///
/// The output for every concrete type matches `hew-hir`'s rendering
/// byte-for-byte: scalar keywords (`i64`, `string`, …), named types by their
/// bare name (compound args separated by `_`), and compound shapes recursively.
/// This function must be kept in sync with `hew-hir/src/monomorph.rs::mangle_resolved_ty`.
#[must_use]
pub fn mangle_resolved_ty_segment(ty: &ResolvedTy) -> Option<String> {
    match ty {
        ResolvedTy::I8 => Some("i8".to_string()),
        ResolvedTy::I16 => Some("i16".to_string()),
        ResolvedTy::I32 => Some("i32".to_string()),
        ResolvedTy::I64 => Some("i64".to_string()),
        ResolvedTy::U8 => Some("u8".to_string()),
        ResolvedTy::U16 => Some("u16".to_string()),
        ResolvedTy::U32 => Some("u32".to_string()),
        ResolvedTy::U64 => Some("u64".to_string()),
        ResolvedTy::Isize => Some("isize".to_string()),
        ResolvedTy::Usize => Some("usize".to_string()),
        ResolvedTy::F32 => Some("f32".to_string()),
        ResolvedTy::F64 => Some("f64".to_string()),
        ResolvedTy::Bool => Some("bool".to_string()),
        ResolvedTy::Char => Some("char".to_string()),
        ResolvedTy::String => Some("string".to_string()),
        ResolvedTy::Bytes => Some("bytes".to_string()),
        ResolvedTy::CancellationToken => Some("CancellationToken".to_string()),
        ResolvedTy::Duration => Some("duration".to_string()),
        ResolvedTy::Unit => Some("unit".to_string()),
        ResolvedTy::Never => Some("never".to_string()),
        ResolvedTy::Named { name, args, .. } => {
            // Bare name, args joined by `_` — mirrors the HIR rendering.
            let mut out = name.replace("::", "_");
            for arg in args {
                out.push('_');
                out.push_str(&mangle_resolved_ty_segment(arg)?);
            }
            Some(out)
        }
        // Abstract type parameters, inference leftovers, and complex shapes
        // (closures, tuples, pointers, …) produce None — callers fall back to
        // the bare key, preserving existing behaviour.
        ResolvedTy::TypeParam { .. }
        | ResolvedTy::Tuple(_)
        | ResolvedTy::Array(_, _)
        | ResolvedTy::Slice(_)
        | ResolvedTy::Function { .. }
        | ResolvedTy::Closure { .. }
        | ResolvedTy::Pointer { .. }
        | ResolvedTy::Borrow { .. }
        | ResolvedTy::TraitObject { .. }
        | ResolvedTy::Task(_) => None,
    }
}

/// Produce the mangled self-type name for a concrete specialised impl.
///
/// For `impl Describe for Wrapper<i64>`: returns `Some("Wrapper$$i64")`.
/// For `impl Describe for Wrapper<string>`: returns `Some("Wrapper$$string")`.
/// Returns `None` when any type arg cannot be mangled (e.g. abstract type
/// parameters, inference variables), in which case callers fall back to the
/// bare `name`.
///
/// The produced name matches `hew-hir::monomorph::mangle(name, type_args)` for
/// `SymbolClass::Function` — both omit a class prefix and join args with `$$`
/// + `$` separators.
#[must_use]
pub fn mangle_impl_self_name(name: &str, type_args: &[ResolvedTy]) -> Option<String> {
    if type_args.is_empty() {
        return None;
    }
    let mut out = String::from(name);
    out.push_str("$$");
    for (i, ty) in type_args.iter().enumerate() {
        if i > 0 {
            out.push('$');
        }
        out.push_str(&mangle_resolved_ty_segment(ty)?);
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer_literal_match_scrutinee_admits_all_integer_widths_only() {
        for ty in [
            ResolvedTy::I8,
            ResolvedTy::I16,
            ResolvedTy::I32,
            ResolvedTy::I64,
            ResolvedTy::U8,
            ResolvedTy::U16,
            ResolvedTy::U32,
            ResolvedTy::U64,
            ResolvedTy::Isize,
            ResolvedTy::Usize,
        ] {
            assert!(
                ty.is_integer_literal_match_scrutinee(),
                "{ty:?} must admit integer literal match predicates"
            );
        }

        for ty in [
            ResolvedTy::F32,
            ResolvedTy::F64,
            ResolvedTy::Bool,
            ResolvedTy::Char,
            ResolvedTy::String,
            ResolvedTy::Unit,
        ] {
            assert!(
                !ty.is_integer_literal_match_scrutinee(),
                "{ty:?} must not admit integer literal match predicates"
            );
        }
    }

    #[test]
    fn from_ty_rejects_ty_var() {
        let var = TypeVar::fresh();
        let result = ResolvedTy::from_ty(&Ty::Var(var));
        assert_eq!(result, Err(BoundaryError::UnresolvedInference { var }));
    }

    #[test]
    fn from_ty_rejects_ty_error() {
        assert_eq!(
            ResolvedTy::from_ty(&Ty::Error),
            Err(BoundaryError::TaintedError)
        );
    }

    #[test]
    fn from_ty_rejects_int_literal() {
        assert_eq!(
            ResolvedTy::from_ty(&Ty::IntLiteral),
            Err(BoundaryError::UnmaterializedLiteral { is_integer: true })
        );
    }

    #[test]
    fn from_ty_rejects_float_literal() {
        assert_eq!(
            ResolvedTy::from_ty(&Ty::FloatLiteral),
            Err(BoundaryError::UnmaterializedLiteral { is_integer: false })
        );
    }

    #[test]
    fn from_ty_rejects_nested_inference_variable_in_named_args() {
        let var = TypeVar::fresh();
        let ty = Ty::Named {
            builtin: None,
            name: "Vec".into(),
            args: vec![Ty::Var(var)],
        };
        assert_eq!(
            ResolvedTy::from_ty(&ty),
            Err(BoundaryError::UnresolvedInference { var })
        );
    }

    #[test]
    fn from_ty_rejects_nested_error_in_function_return() {
        let ty = Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Error),
        };
        assert_eq!(ResolvedTy::from_ty(&ty), Err(BoundaryError::TaintedError));
    }

    #[test]
    fn from_ty_rejects_nested_literal_in_tuple() {
        let ty = Ty::Tuple(vec![Ty::I32, Ty::FloatLiteral]);
        assert_eq!(
            ResolvedTy::from_ty(&ty),
            Err(BoundaryError::UnmaterializedLiteral { is_integer: false })
        );
    }

    #[test]
    fn from_ty_accepts_materialized_int_literal_as_i64() {
        // Contract check: after `materialize_literal_defaults`, the int
        // literal kind becomes I64 (Hew's current default integer width).
        let defaulted = Ty::IntLiteral.materialize_literal_defaults();
        assert_eq!(ResolvedTy::from_ty(&defaulted), Ok(ResolvedTy::I64));
    }

    #[test]
    fn from_ty_accepts_materialized_float_literal_as_f64() {
        let defaulted = Ty::FloatLiteral.materialize_literal_defaults();
        assert_eq!(ResolvedTy::from_ty(&defaulted), Ok(ResolvedTy::F64));
    }

    #[test]
    fn from_ty_accepts_fully_concrete_named() {
        let ty = Ty::Named {
            builtin: None,
            name: "Foo".into(),
            args: vec![Ty::I32, Ty::String],
        };
        assert_eq!(
            ResolvedTy::from_ty(&ty),
            Ok(ResolvedTy::Named {
                name: "Foo".into(),
                args: vec![ResolvedTy::I32, ResolvedTy::String],
                builtin: None,
                is_opaque: false,
            })
        );
    }

    #[test]
    fn from_ty_accepts_nested_composites() {
        let ty = Ty::Function {
            params: vec![
                Ty::Array(Box::new(Ty::I32), 4),
                Ty::Slice(Box::new(Ty::Bool)),
            ],
            ret: Box::new(Ty::Tuple(vec![Ty::String, Ty::Unit])),
        };
        let expected = ResolvedTy::Function {
            params: vec![
                ResolvedTy::Array(Box::new(ResolvedTy::I32), 4),
                ResolvedTy::Slice(Box::new(ResolvedTy::Bool)),
            ],
            ret: Box::new(ResolvedTy::Tuple(vec![
                ResolvedTy::String,
                ResolvedTy::Unit,
            ])),
        };
        assert_eq!(ResolvedTy::from_ty(&ty), Ok(expected));
    }

    #[test]
    fn from_ty_accepts_pointer_and_closure() {
        let ty = Ty::Closure {
            params: vec![Ty::Pointer {
                is_mutable: true,
                pointee: Box::new(Ty::I32),
            }],
            ret: Box::new(Ty::Unit),
            captures: vec![Ty::Bool],
        };
        let expected = ResolvedTy::Closure {
            params: vec![ResolvedTy::Pointer {
                is_mutable: true,
                pointee: Box::new(ResolvedTy::I32),
            }],
            ret: Box::new(ResolvedTy::Unit),
            captures: vec![ResolvedTy::Bool],
        };
        assert_eq!(ResolvedTy::from_ty(&ty), Ok(expected));
    }

    #[test]
    fn from_ty_accepts_trait_object() {
        let ty = Ty::TraitObject {
            traits: vec![TraitObjectBound {
                trait_name: "Iterator".into(),
                args: vec![Ty::I32],
                assoc_bindings: vec![],
            }],
        };
        let expected = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Iterator".into(),
                args: vec![ResolvedTy::I32],
                assoc_bindings: vec![],
            }],
        };
        assert_eq!(ResolvedTy::from_ty(&ty), Ok(expected));
    }

    #[test]
    fn from_ty_reports_innermost_trait_object_error() {
        let var = TypeVar::fresh();
        let ty = Ty::TraitObject {
            traits: vec![TraitObjectBound {
                trait_name: "Iterator".into(),
                args: vec![Ty::Var(var)],
                assoc_bindings: vec![],
            }],
        };
        assert_eq!(
            ResolvedTy::from_ty(&ty),
            Err(BoundaryError::UnresolvedInference { var })
        );
    }

    #[test]
    fn round_trip_preserves_builtin_for_user_shadowed_names() {
        // Smoke test for the §5.5 fix: a user-declared type whose *name*
        // collides with a builtin (here `Vec`) MUST round-trip with
        // `builtin: None` preserved. Before the fix, `to_ty` re-derived
        // via `lookup_builtin_type(name)` and silently re-tagged the user
        // type as the builtin.
        let user_vec = Ty::Named {
            name: "Vec".into(),
            args: vec![Ty::I32],
            builtin: None,
        };
        let resolved = ResolvedTy::from_ty(&user_vec).expect("concrete Ty resolves");
        // `from_ty` preserves the discriminator.
        match &resolved {
            ResolvedTy::Named { builtin, .. } => {
                assert_eq!(*builtin, None, "from_ty must preserve user-side `None`");
            }
            other => panic!("expected ResolvedTy::Named, got {other:?}"),
        }
        // `to_ty` consumes the carried discriminator (does NOT re-derive
        // by name). Round-trip equality holds.
        assert_eq!(resolved.to_ty(), user_vec);

        // Sibling case: a true builtin round-trips with `Some(_)` intact.
        let builtin_vec = Ty::Named {
            name: "Vec".into(),
            args: vec![Ty::I32],
            builtin: Some(crate::BuiltinType::Vec),
        };
        let resolved_b = ResolvedTy::from_ty(&builtin_vec).expect("concrete Ty resolves");
        match &resolved_b {
            ResolvedTy::Named { builtin, .. } => {
                assert_eq!(*builtin, Some(crate::BuiltinType::Vec));
            }
            other => panic!("expected ResolvedTy::Named, got {other:?}"),
        }
        assert_eq!(resolved_b.to_ty(), builtin_vec);
    }

    #[test]
    fn named_constructor_helpers_set_builtin_explicitly() {
        let user = ResolvedTy::named_user("Connection", vec![]);
        assert!(matches!(
            user,
            ResolvedTy::Named { ref name, builtin: None, .. } if name == "Connection"
        ));

        let builtin =
            ResolvedTy::named_builtin("Vec", crate::BuiltinType::Vec, vec![ResolvedTy::I32]);
        assert!(matches!(
            builtin,
            ResolvedTy::Named {
                ref name,
                builtin: Some(crate::BuiltinType::Vec),
                ..
            } if name == "Vec"
        ));
    }

    #[test]
    fn round_trip_from_ty_then_to_ty_is_identity_for_resolved_inputs() {
        // Round-trip: concrete Ty -> ResolvedTy -> Ty is identity.
        let inputs = [
            Ty::I32,
            Ty::Bool,
            Ty::String,
            Ty::Tuple(vec![Ty::I64, Ty::Unit]),
            Ty::Named {
                builtin: Some(crate::BuiltinType::Vec),
                name: "Vec".into(),
                args: vec![Ty::I32],
            },
            Ty::Function {
                params: vec![Ty::I32],
                ret: Box::new(Ty::String),
            },
        ];
        for ty in inputs {
            let resolved = ResolvedTy::from_ty(&ty).expect("concrete Ty should resolve");
            assert_eq!(resolved.to_ty(), ty);
        }
    }

    #[test]
    fn user_facing_display_matches_ty_user_facing() {
        let ty = Ty::Named {
            builtin: None,
            name: "Option".into(),
            args: vec![Ty::I64],
        };
        let resolved = ResolvedTy::from_ty(&ty).expect("concrete Ty resolves");
        assert_eq!(
            resolved.user_facing().to_string(),
            ty.user_facing().to_string()
        );
    }

    #[test]
    fn boundary_error_display_is_actionable() {
        let err = BoundaryError::TaintedError;
        let rendered = err.to_string();
        assert!(
            rendered.contains("error-recovery"),
            "expected tainted-error message, got {rendered}"
        );

        let int_err = BoundaryError::UnmaterializedLiteral { is_integer: true };
        assert!(int_err.to_string().contains("integer literal"));

        let float_err = BoundaryError::UnmaterializedLiteral { is_integer: false };
        assert!(float_err.to_string().contains("float literal"));
    }

    // --- Task<T> variant tests ---

    #[test]
    fn task_variant_converts_from_concrete_ty() {
        let ty = Ty::Task(Box::new(Ty::I64));
        assert_eq!(
            ResolvedTy::from_ty(&ty),
            Ok(ResolvedTy::Task(Box::new(ResolvedTy::I64)))
        );
    }

    #[test]
    fn task_variant_rejects_nested_inference_var() {
        let var = TypeVar::fresh();
        let ty = Ty::Task(Box::new(Ty::Var(var)));
        assert_eq!(
            ResolvedTy::from_ty(&ty),
            Err(BoundaryError::UnresolvedInference { var })
        );
    }

    #[test]
    fn task_variant_round_trips_to_ty() {
        let ty = Ty::Task(Box::new(Ty::String));
        let resolved = ResolvedTy::from_ty(&ty).expect("Task<String> is concrete");
        assert_eq!(resolved.to_ty(), ty);
    }

    #[test]
    fn task_display_uses_angle_bracket_spelling() {
        // The `<task<T>>` spelling (with outer angle brackets) signals that
        // this is a compiler-internal type, not a user-writable annotation.
        let resolved = ResolvedTy::Task(Box::new(ResolvedTy::I64));
        assert_eq!(resolved.to_string(), "<task<i64>>");
    }

    #[test]
    fn task_display_nested_named_type() {
        let resolved = ResolvedTy::Task(Box::new(ResolvedTy::Named {
            name: "User".into(),
            args: Vec::new(),
            builtin: None,
            is_opaque: false,
        }));
        assert_eq!(resolved.to_string(), "<task<User>>");
    }

    // --- TypeParam variant tests (A622) ---

    fn type_param_scope(names: &[&str]) -> std::collections::HashSet<String> {
        names.iter().map(|s| (*s).to_string()).collect()
    }

    #[test]
    fn unscoped_from_ty_never_produces_type_param() {
        // Behaviour-preserving: a bare `Named` is a user type under the
        // unscoped converter, exactly as before this variant existed.
        let ty = Ty::Named {
            name: "T".into(),
            args: vec![],
            builtin: None,
        };
        assert_eq!(
            ResolvedTy::from_ty(&ty),
            Ok(ResolvedTy::Named {
                name: "T".into(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            })
        );
    }

    #[test]
    fn scoped_from_ty_recognises_declared_type_param() {
        let ty = Ty::Named {
            name: "T".into(),
            args: vec![],
            builtin: None,
        };
        let scope = type_param_scope(&["T"]);
        assert_eq!(
            ResolvedTy::from_ty_with_type_params(&ty, &scope),
            Ok(ResolvedTy::TypeParam { name: "T".into() })
        );
    }

    #[test]
    fn scoped_from_ty_leaves_out_of_scope_names_as_named() {
        // A no-argument user type whose name is NOT a declared param stays a
        // `Named` even under the scoped converter.
        let ty = Ty::Named {
            name: "Color".into(),
            args: vec![],
            builtin: None,
        };
        let scope = type_param_scope(&["T", "U"]);
        assert_eq!(
            ResolvedTy::from_ty_with_type_params(&ty, &scope),
            Ok(ResolvedTy::Named {
                name: "Color".into(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            })
        );
    }

    #[test]
    fn type_param_round_trips_losslessly_both_directions() {
        // Non-vacuous round-trip: the variant survives ResolvedTy -> Ty and
        // back to the identical ResolvedTy under the declared scope.
        let scope = type_param_scope(&["T"]);
        let resolved = ResolvedTy::TypeParam { name: "T".into() };

        let lowered = resolved.to_ty();
        assert_eq!(
            lowered,
            Ty::Named {
                name: "T".into(),
                args: vec![],
                builtin: None,
            }
        );

        let restored = ResolvedTy::from_ty_with_type_params(&lowered, &scope)
            .expect("type-param carrier resolves within scope");
        assert_eq!(restored, resolved);
    }

    #[test]
    fn nested_type_param_round_trips_inside_composites() {
        let scope = type_param_scope(&["T"]);
        // Vec<T> as a user-named composite carrying an abstract argument.
        let resolved = ResolvedTy::Named {
            name: "Vec".into(),
            args: vec![ResolvedTy::TypeParam { name: "T".into() }],
            builtin: None,
            is_opaque: false,
        };
        let restored = ResolvedTy::from_ty_with_type_params(&resolved.to_ty(), &scope)
            .expect("nested type-param resolves within scope");
        assert_eq!(restored, resolved);
    }

    #[test]
    fn scoped_from_ty_still_fails_closed_on_inference_var() {
        // The scope must not weaken the fail-closed contract for genuine
        // checker-internal leaks.
        let var = TypeVar::fresh();
        let ty = Ty::Tuple(vec![
            Ty::Named {
                name: "T".into(),
                args: vec![],
                builtin: None,
            },
            Ty::Var(var),
        ]);
        let scope = type_param_scope(&["T"]);
        assert_eq!(
            ResolvedTy::from_ty_with_type_params(&ty, &scope),
            Err(BoundaryError::UnresolvedInference { var })
        );
    }

    #[test]
    fn type_param_display_is_bare_name() {
        let resolved = ResolvedTy::TypeParam { name: "T".into() };
        assert_eq!(resolved.to_string(), "T");
    }
}
