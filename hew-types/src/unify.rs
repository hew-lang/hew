//! Type unification and constraint solving.
//!
//! This module implements the core unification algorithm for Hew's type inference.
//! It uses a standard substitution-based approach with occurs checking to prevent
//! infinite types.

use crate::ty::{Substitution, Ty, TypeVar};
use std::fmt;

/// Error that can occur during type unification.
#[derive(Debug, Clone)]
pub enum UnifyError {
    /// Two types are fundamentally incompatible.
    Mismatch {
        /// The expected type
        expected: Ty,
        /// The actual type found
        actual: Ty,
    },
    /// Occurs check failed (would create infinite type).
    OccursCheck {
        /// The type variable
        var: TypeVar,
        /// The type that contains the variable
        ty: Ty,
    },
    /// Wrong number of elements (tuples, function params, type args).
    ArityMismatch {
        /// Expected arity
        expected: usize,
        /// Actual arity
        actual: usize,
    },
}

impl fmt::Display for UnifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnifyError::Mismatch { expected, actual } => {
                write!(f, "type mismatch: expected `{expected}`, found `{actual}`")
            }
            UnifyError::OccursCheck { var, ty } => {
                write!(f, "infinite type: `{var}` occurs in `{ty}`")
            }
            UnifyError::ArityMismatch { expected, actual } => {
                write!(f, "arity mismatch: expected {expected}, found {actual}")
            }
        }
    }
}

impl std::error::Error for UnifyError {}

/// Bind a type variable to a type, performing occurs check.
///
/// # Errors
/// Returns `UnifyError::OccursCheck` if the type contains the variable.
pub fn bind(subst: &mut Substitution, var: TypeVar, ty: &Ty) -> Result<(), UnifyError> {
    // Don't bind a variable to itself
    if let Ty::Var(v) = ty {
        if *v == var {
            return Ok(());
        }
    }

    // Apply existing substitutions to the type before checking
    let ty = ty.apply_subst(subst);

    // Re-check after substitution (may have resolved to the same variable)
    if let Ty::Var(v) = &ty {
        if *v == var {
            return Ok(());
        }
    }

    // Occurs check: prevent T = Vec<T> infinite recursion
    // Must happen AFTER apply_subst so we catch indirect cycles
    if ty.contains_var(var) {
        return Err(UnifyError::OccursCheck { var, ty });
    }

    subst.insert(var, ty);
    Ok(())
}

/// Check if two type names refer to the same type, accounting for module prefixes.
/// e.g., "json.Value" and "Value" are the same type.
fn names_match_qualified(a: &str, b: &str) -> bool {
    let a_bare = a.find('.').map_or(a, |dot| &a[dot + 1..]);
    let b_bare = b.find('.').map_or(b, |dot| &b[dot + 1..]);
    a_bare == b_bare
}

/// Unify two types, updating the substitution.
///
/// This is the core algorithm that makes two types equal by finding
/// a substitution for type variables.
///
/// # Errors
/// Returns a `UnifyError` if the types cannot be unified.
#[expect(
    clippy::too_many_lines,
    reason = "unification covers many Ty variant combinations"
)]
#[expect(
    clippy::unnested_or_patterns,
    reason = "keeping function/closure patterns visually distinct"
)]
pub fn unify(subst: &mut Substitution, a: &Ty, b: &Ty) -> Result<(), UnifyError> {
    let a = subst.resolve(a);
    let b = subst.resolve(b);

    match (&a, &b) {
        // Same type — trivial
        _ if a == b => Ok(()),

        // Type variable on either side — bind
        (Ty::Var(v), _) => bind(subst, *v, &b),
        (_, Ty::Var(v)) => bind(subst, *v, &a),

        // Error/Never type unifies with anything (recovery/diverging)
        (Ty::Error | Ty::Never, _) | (_, Ty::Error | Ty::Never) => Ok(()),

        // Structural: tuples
        (Ty::Tuple(as_), Ty::Tuple(bs)) => {
            if as_.len() != bs.len() {
                return Err(UnifyError::ArityMismatch {
                    expected: as_.len(),
                    actual: bs.len(),
                });
            }
            for (a, b) in as_.iter().zip(bs.iter()) {
                unify(subst, a, b)?;
            }
            Ok(())
        }

        // Structural: arrays
        (Ty::Array(a_elem, a_size), Ty::Array(b_elem, b_size)) => {
            if a_size != b_size {
                return Err(UnifyError::Mismatch {
                    expected: a,
                    actual: b,
                });
            }
            unify(subst, a_elem, b_elem)
        }

        // Structural: slices
        (Ty::Slice(a_elem), Ty::Slice(b_elem)) => unify(subst, a_elem, b_elem),

        // Structural: functions and closures (Closure unifies with Function by params+ret)
        (
            Ty::Function {
                params: ap,
                ret: ar,
            },
            Ty::Function {
                params: bp,
                ret: br,
            },
        )
        | (
            Ty::Closure {
                params: ap,
                ret: ar,
                ..
            },
            Ty::Closure {
                params: bp,
                ret: br,
                ..
            },
        )
        | (
            Ty::Closure {
                params: ap,
                ret: ar,
                ..
            },
            Ty::Function {
                params: bp,
                ret: br,
            },
        )
        | (
            Ty::Function {
                params: ap,
                ret: ar,
            },
            Ty::Closure {
                params: bp,
                ret: br,
                ..
            },
        ) => {
            if ap.len() != bp.len() {
                return Err(UnifyError::ArityMismatch {
                    expected: ap.len(),
                    actual: bp.len(),
                });
            }
            for (a, b) in ap.iter().zip(bp.iter()) {
                unify(subst, a, b)?;
            }
            unify(subst, ar, br)
        }

        // Named types with same name — unify type args.
        // Also handles module-qualified names: "json.Value" matches "Value"
        (Ty::Named { name: an, args: aa }, Ty::Named { name: bn, args: ba })
            if an == bn || names_match_qualified(an, bn) =>
        {
            if aa.len() != ba.len() {
                return Err(UnifyError::ArityMismatch {
                    expected: aa.len(),
                    actual: ba.len(),
                });
            }
            for (a, b) in aa.iter().zip(ba.iter()) {
                unify(subst, a, b)?;
            }
            Ok(())
        }

        // Option / ActorRef / Range / Stream / Sink
        (Ty::Option(a), Ty::Option(b))
        | (Ty::ActorRef(a), Ty::ActorRef(b))
        | (Ty::Range(a), Ty::Range(b))
        | (Ty::Stream(a), Ty::Stream(b))
        | (Ty::Sink(a), Ty::Sink(b)) => unify(subst, a, b),

        // Result
        (Ty::Result { ok: ao, err: ae }, Ty::Result { ok: bo, err: be }) => {
            unify(subst, ao, bo)?;
            unify(subst, ae, be)
        }

        // Pointer (must match mutability)
        (
            Ty::Pointer {
                is_mutable: am,
                pointee: ap,
            },
            Ty::Pointer {
                is_mutable: bm,
                pointee: bp,
            },
        ) if am == bm => unify(subst, ap, bp),

        // Generator
        (
            Ty::Generator {
                yields: ay,
                returns: ar,
            },
            Ty::Generator {
                yields: by,
                returns: br,
            },
        ) => {
            unify(subst, ay, by)?;
            unify(subst, ar, br)
        }

        // AsyncGenerator
        (Ty::AsyncGenerator { yields: ay }, Ty::AsyncGenerator { yields: by }) => {
            unify(subst, ay, by)
        }

        // TraitObject with same traits
        (Ty::TraitObject { traits: a_traits }, Ty::TraitObject { traits: b_traits }) => {
            if a_traits.len() != b_traits.len() {
                return Err(UnifyError::Mismatch {
                    expected: a.clone(),
                    actual: b.clone(),
                });
            }

            // Compare as sets: for each bound in a, find a matching bound in b by trait_name
            let mut matched = vec![false; b_traits.len()];
            for a_bound in a_traits.iter() {
                let Some(idx) = b_traits.iter().enumerate().position(|(i, b_bound)| {
                    !matched[i] && b_bound.trait_name == a_bound.trait_name
                }) else {
                    return Err(UnifyError::Mismatch {
                        expected: a.clone(),
                        actual: b.clone(),
                    });
                };
                matched[idx] = true;
                let b_bound = &b_traits[idx];
                if a_bound.args.len() != b_bound.args.len() {
                    return Err(UnifyError::ArityMismatch {
                        expected: a_bound.args.len(),
                        actual: b_bound.args.len(),
                    });
                }
                for (a_arg, b_arg) in a_bound.args.iter().zip(b_bound.args.iter()) {
                    unify(subst, a_arg, b_arg)?;
                }
            }
            Ok(())
        }

        // Mismatch
        _ => Err(UnifyError::Mismatch {
            expected: a,
            actual: b,
        }),
    }
}

/// Check if type `a` can be coerced to type `b`.
///
/// This is more permissive than unification and allows:
/// - Never coerces to anything
/// - Integer literals can coerce to any integer type
/// - Float literals can coerce to any float type
#[must_use]
pub fn can_coerce(a: &Ty, b: &Ty) -> bool {
    match (a, b) {
        // Never/Error coerces to anything
        (Ty::Never | Ty::Error, _) | (_, Ty::Error) => true,

        // Same types
        _ if a == b => true,

        // Array to slice coercion
        (Ty::Array(elem_a, _), Ty::Slice(elem_b)) => can_coerce(elem_a, elem_b),

        // Mutable pointer to const pointer
        (
            Ty::Pointer {
                is_mutable: true,
                pointee: pa,
            },
            Ty::Pointer {
                is_mutable: false,
                pointee: pb,
            },
        ) => can_coerce(pa, pb),

        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unify_same_type() {
        let mut subst = Substitution::new();
        assert!(unify(&mut subst, &Ty::I32, &Ty::I32).is_ok());
    }

    #[test]
    fn test_unify_type_var_left() {
        TypeVar::reset();
        let mut subst = Substitution::new();
        let v = TypeVar::fresh();
        assert!(unify(&mut subst, &Ty::Var(v), &Ty::I32).is_ok());
        assert_eq!(subst.resolve(&Ty::Var(v)), Ty::I32);
    }

    #[test]
    fn test_unify_type_var_right() {
        TypeVar::reset();
        let mut subst = Substitution::new();
        let v = TypeVar::fresh();
        assert!(unify(&mut subst, &Ty::Bool, &Ty::Var(v)).is_ok());
        assert_eq!(subst.resolve(&Ty::Var(v)), Ty::Bool);
    }

    #[test]
    fn test_unify_tuples() {
        let mut subst = Substitution::new();
        let a = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        let b = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        assert!(unify(&mut subst, &a, &b).is_ok());
    }

    #[test]
    fn test_unify_tuples_with_var() {
        TypeVar::reset();
        let mut subst = Substitution::new();
        let v = TypeVar::fresh();
        let a = Ty::Tuple(vec![Ty::Var(v), Ty::Bool]);
        let b = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        assert!(unify(&mut subst, &a, &b).is_ok());
        assert_eq!(subst.resolve(&Ty::Var(v)), Ty::I32);
    }

    #[test]
    fn test_unify_mismatch() {
        let mut subst = Substitution::new();
        let result = unify(&mut subst, &Ty::I32, &Ty::Bool);
        assert!(matches!(result, Err(UnifyError::Mismatch { .. })));
    }

    #[test]
    fn test_unify_integer_width_mismatch() {
        let mut subst = Substitution::new();
        let result = unify(&mut subst, &Ty::I32, &Ty::I64);
        assert!(matches!(result, Err(UnifyError::Mismatch { .. })));
    }

    #[test]
    fn test_unify_arity_mismatch() {
        let mut subst = Substitution::new();
        let a = Ty::Tuple(vec![Ty::I32]);
        let b = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        let result = unify(&mut subst, &a, &b);
        assert!(matches!(result, Err(UnifyError::ArityMismatch { .. })));
    }

    #[test]
    fn test_occurs_check() {
        TypeVar::reset();
        let mut subst = Substitution::new();
        let v = TypeVar::fresh();
        let ty = Ty::Tuple(vec![Ty::Var(v), Ty::I32]);
        let result = unify(&mut subst, &Ty::Var(v), &ty);
        assert!(matches!(result, Err(UnifyError::OccursCheck { .. })));
    }

    #[test]
    fn test_unify_functions() {
        TypeVar::reset();
        let mut subst = Substitution::new();
        let v = TypeVar::fresh();
        let a = Ty::Function {
            params: vec![Ty::I32, Ty::Var(v)],
            ret: Box::new(Ty::Bool),
        };
        let b = Ty::Function {
            params: vec![Ty::I32, Ty::String],
            ret: Box::new(Ty::Bool),
        };
        assert!(unify(&mut subst, &a, &b).is_ok());
        assert_eq!(subst.resolve(&Ty::Var(v)), Ty::String);
    }

    #[test]
    fn test_unify_named_types() {
        TypeVar::reset();
        let mut subst = Substitution::new();
        let v = TypeVar::fresh();
        let a = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::Var(v)],
        };
        let b = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::I32],
        };
        assert!(unify(&mut subst, &a, &b).is_ok());
        assert_eq!(subst.resolve(&Ty::Var(v)), Ty::I32);
    }

    #[test]
    fn test_unify_never_coerces() {
        let mut subst = Substitution::new();
        assert!(unify(&mut subst, &Ty::Never, &Ty::I32).is_ok());
        assert!(unify(&mut subst, &Ty::Never, &Ty::String).is_ok());
    }

    #[test]
    fn test_unify_error_coerces() {
        let mut subst = Substitution::new();
        assert!(unify(&mut subst, &Ty::Error, &Ty::I32).is_ok());
        assert!(unify(&mut subst, &Ty::Bool, &Ty::Error).is_ok());
    }

    #[test]
    fn test_can_coerce() {
        assert!(can_coerce(&Ty::Never, &Ty::I32));
        assert!(can_coerce(&Ty::I32, &Ty::I32));
        assert!(!can_coerce(&Ty::I32, &Ty::Bool));
    }

    #[test]
    fn test_unify_closure_with_function() {
        let mut subst = Substitution::new();
        let closure = Ty::Closure {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
            captures: vec![Ty::String],
        };
        let function = Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
        };
        assert!(unify(&mut subst, &closure, &function).is_ok());
    }

    #[test]
    fn test_unify_closure_with_closure() {
        TypeVar::reset();
        let mut subst = Substitution::new();
        let v = TypeVar::fresh();
        let a = Ty::Closure {
            params: vec![Ty::Var(v)],
            ret: Box::new(Ty::Bool),
            captures: vec![Ty::I32],
        };
        let b = Ty::Closure {
            params: vec![Ty::String],
            ret: Box::new(Ty::Bool),
            captures: vec![Ty::F64],
        };
        assert!(unify(&mut subst, &a, &b).is_ok());
        assert_eq!(subst.resolve(&Ty::Var(v)), Ty::String);
    }

    #[test]
    fn test_unify_closure_arity_mismatch() {
        let mut subst = Substitution::new();
        let closure = Ty::Closure {
            params: vec![Ty::I32, Ty::Bool],
            ret: Box::new(Ty::Unit),
            captures: vec![],
        };
        let function = Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Unit),
        };
        assert!(matches!(
            unify(&mut subst, &closure, &function),
            Err(UnifyError::ArityMismatch { .. })
        ));
    }

    #[test]
    fn test_unify_option_types() {
        let mut subst = Substitution::new();
        let a = Ty::Option(Box::new(Ty::I32));
        let b = Ty::Option(Box::new(Ty::I32));
        assert!(unify(&mut subst, &a, &b).is_ok());
    }

    #[test]
    fn test_unify_option_mismatch() {
        let mut subst = Substitution::new();
        let a = Ty::Option(Box::new(Ty::I32));
        let b = Ty::Option(Box::new(Ty::String));
        assert!(unify(&mut subst, &a, &b).is_err());
    }

    #[test]
    fn test_unify_result_types() {
        let mut subst = Substitution::new();
        let a = Ty::Result {
            ok: Box::new(Ty::I32),
            err: Box::new(Ty::String),
        };
        let b = Ty::Result {
            ok: Box::new(Ty::I32),
            err: Box::new(Ty::String),
        };
        assert!(unify(&mut subst, &a, &b).is_ok());
    }

    #[test]
    fn test_unify_result_mismatch() {
        let mut subst = Substitution::new();
        let a = Ty::Result {
            ok: Box::new(Ty::I32),
            err: Box::new(Ty::String),
        };
        let b = Ty::Result {
            ok: Box::new(Ty::Bool),
            err: Box::new(Ty::String),
        };
        assert!(unify(&mut subst, &a, &b).is_err());
    }

    #[test]
    fn test_unify_named_different_names() {
        let mut subst = Substitution::new();
        let a = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::I32],
        };
        let b = Ty::Named {
            name: "HashMap".to_string(),
            args: vec![Ty::I32],
        };
        assert!(unify(&mut subst, &a, &b).is_err());
    }

    #[test]
    fn test_unify_chained_vars() {
        TypeVar::reset();
        let mut subst = Substitution::new();
        let v1 = TypeVar::fresh();
        let v2 = TypeVar::fresh();
        assert!(unify(&mut subst, &Ty::Var(v1), &Ty::Var(v2)).is_ok());
        assert!(unify(&mut subst, &Ty::Var(v2), &Ty::I32).is_ok());
        assert_eq!(subst.resolve(&Ty::Var(v1)), Ty::I32);
    }

    #[test]
    fn test_unify_array_types() {
        let mut subst = Substitution::new();
        let a = Ty::Array(Box::new(Ty::I32), 5);
        let b = Ty::Array(Box::new(Ty::I32), 5);
        assert!(unify(&mut subst, &a, &b).is_ok());
    }

    #[test]
    fn test_unify_array_size_mismatch() {
        let mut subst = Substitution::new();
        let a = Ty::Array(Box::new(Ty::I32), 5);
        let b = Ty::Array(Box::new(Ty::I32), 10);
        assert!(unify(&mut subst, &a, &b).is_err());
    }

    #[test]
    fn test_resolve_deeply_nested() {
        TypeVar::reset();
        let mut subst = Substitution::new();
        let v = TypeVar::fresh();
        let ty = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::Tuple(vec![Ty::Var(v), Ty::Bool])],
        };
        assert!(unify(
            &mut subst,
            &ty,
            &Ty::Named {
                name: "Vec".to_string(),
                args: vec![Ty::Tuple(vec![Ty::String, Ty::Bool])],
            }
        )
        .is_ok());
        assert_eq!(subst.resolve(&Ty::Var(v)), Ty::String);
    }
}
