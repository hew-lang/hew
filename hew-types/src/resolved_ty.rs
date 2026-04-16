//! Checker output boundary type.
//!
//! `ResolvedTy` is the concrete, fully-resolved form of a type after the
//! checker has finished inference, defaulted numeric literals, and rejected
//! any leaked inference or error nodes. It is the wire-level form that
//! crosses from `hew-types` into `hew-serialize`, `hew-codegen`, and
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
    /// Trait object: `dyn Trait` or `dyn (Trait1 + Trait2)`.
    TraitObject {
        /// Trait bounds
        traits: Vec<ResolvedTraitBound>,
    },
}

/// A single trait bound in a resolved trait object.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedTraitBound {
    /// Trait name
    pub trait_name: String,
    /// Resolved type arguments
    pub args: Vec<ResolvedTy>,
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
        }
    }
}

impl std::error::Error for BoundaryError {}

impl ResolvedTy {
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
        match ty {
            Ty::I8 => Ok(ResolvedTy::I8),
            Ty::I16 => Ok(ResolvedTy::I16),
            Ty::I32 => Ok(ResolvedTy::I32),
            Ty::I64 => Ok(ResolvedTy::I64),
            Ty::U8 => Ok(ResolvedTy::U8),
            Ty::U16 => Ok(ResolvedTy::U16),
            Ty::U32 => Ok(ResolvedTy::U32),
            Ty::U64 => Ok(ResolvedTy::U64),
            Ty::F32 => Ok(ResolvedTy::F32),
            Ty::F64 => Ok(ResolvedTy::F64),
            Ty::Bool => Ok(ResolvedTy::Bool),
            Ty::Char => Ok(ResolvedTy::Char),
            Ty::String => Ok(ResolvedTy::String),
            Ty::Bytes => Ok(ResolvedTy::Bytes),
            Ty::Duration => Ok(ResolvedTy::Duration),
            Ty::Unit => Ok(ResolvedTy::Unit),
            Ty::Never => Ok(ResolvedTy::Never),
            Ty::IntLiteral => Err(BoundaryError::UnmaterializedLiteral { is_integer: true }),
            Ty::FloatLiteral => Err(BoundaryError::UnmaterializedLiteral { is_integer: false }),
            Ty::Var(var) => Err(BoundaryError::UnresolvedInference { var: *var }),
            Ty::Error => Err(BoundaryError::TaintedError),
            Ty::Tuple(elems) => Ok(ResolvedTy::Tuple(Self::convert_vec(elems)?)),
            Ty::Array(elem, size) => Ok(ResolvedTy::Array(Box::new(Self::from_ty(elem)?), *size)),
            Ty::Slice(elem) => Ok(ResolvedTy::Slice(Box::new(Self::from_ty(elem)?))),
            Ty::Named { name, args } => Ok(ResolvedTy::Named {
                name: name.clone(),
                args: Self::convert_vec(args)?,
            }),
            Ty::Function { params, ret } => Ok(ResolvedTy::Function {
                params: Self::convert_vec(params)?,
                ret: Box::new(Self::from_ty(ret)?),
            }),
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ok(ResolvedTy::Closure {
                params: Self::convert_vec(params)?,
                ret: Box::new(Self::from_ty(ret)?),
                captures: Self::convert_vec(captures)?,
            }),
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ok(ResolvedTy::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(Self::from_ty(pointee)?),
            }),
            Ty::TraitObject { traits } => Ok(ResolvedTy::TraitObject {
                traits: traits
                    .iter()
                    .map(Self::convert_trait_bound)
                    .collect::<Result<Vec<_>, _>>()?,
            }),
        }
    }

    fn convert_vec(tys: &[Ty]) -> Result<Vec<ResolvedTy>, BoundaryError> {
        tys.iter().map(Self::from_ty).collect()
    }

    fn convert_trait_bound(bound: &TraitObjectBound) -> Result<ResolvedTraitBound, BoundaryError> {
        Ok(ResolvedTraitBound {
            trait_name: bound.trait_name.clone(),
            args: Self::convert_vec(&bound.args)?,
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
            ResolvedTy::F32 => Ty::F32,
            ResolvedTy::F64 => Ty::F64,
            ResolvedTy::Bool => Ty::Bool,
            ResolvedTy::Char => Ty::Char,
            ResolvedTy::String => Ty::String,
            ResolvedTy::Bytes => Ty::Bytes,
            ResolvedTy::Duration => Ty::Duration,
            ResolvedTy::Unit => Ty::Unit,
            ResolvedTy::Never => Ty::Never,
            ResolvedTy::Tuple(elems) => Ty::Tuple(elems.iter().map(Self::to_ty).collect()),
            ResolvedTy::Array(elem, size) => Ty::Array(Box::new(elem.to_ty()), *size),
            ResolvedTy::Slice(elem) => Ty::Slice(Box::new(elem.to_ty())),
            ResolvedTy::Named { name, args } => Ty::Named {
                name: name.clone(),
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
            ResolvedTy::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound.args.iter().map(Self::to_ty).collect(),
                    })
                    .collect(),
            },
        }
    }

    /// User-facing display wrapper that preserves Hew numeric spellings
    /// (`int`/`float` aliases) identically to
    /// [`Ty::user_facing`]. Rendering a `ResolvedTy` never exposes the
    /// unrepresentable states (they don't exist), so callers can consume
    /// this output directly without post-filtering.
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

#[cfg(test)]
mod tests {
    use super::*;

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
            name: "Foo".into(),
            args: vec![Ty::I32, Ty::String],
        };
        assert_eq!(
            ResolvedTy::from_ty(&ty),
            Ok(ResolvedTy::Named {
                name: "Foo".into(),
                args: vec![ResolvedTy::I32, ResolvedTy::String],
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
            }],
        };
        let expected = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Iterator".into(),
                args: vec![ResolvedTy::I32],
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
            }],
        };
        assert_eq!(
            ResolvedTy::from_ty(&ty),
            Err(BoundaryError::UnresolvedInference { var })
        );
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
}
