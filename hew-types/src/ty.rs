//! Internal type representation for the Hew type checker.
//!
//! This module defines `Ty`, the core type representation used throughout
//! the type checker. Types are structural and support substitution for
//! type inference variables.

use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicU32, Ordering};

/// A unique type variable ID for inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

/// Counter for generating fresh type variables.
static NEXT_TYPE_VAR: AtomicU32 = AtomicU32::new(0);

impl TypeVar {
    /// Create a fresh, globally unique type variable.
    #[must_use]
    pub fn fresh() -> Self {
        Self(NEXT_TYPE_VAR.fetch_add(1, Ordering::Relaxed))
    }

    /// Reset the type variable counter (for testing).
    pub fn reset() {
        NEXT_TYPE_VAR.store(0, Ordering::Relaxed);
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?T{}", self.0)
    }
}

/// A single trait bound in a trait object.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitObjectBound {
    /// Trait name
    pub trait_name: String,
    /// Type arguments
    pub args: Vec<Ty>,
}

/// The internal representation of a type in Hew.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    // Primitives
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
    /// Unit type (void)
    Unit,
    /// Never type (diverging, `!`)
    Never,

    // Inference
    /// Unresolved type variable
    Var(TypeVar),

    // Composites
    /// Tuple type: `(T1, T2, ...)`
    Tuple(Vec<Ty>),
    /// Fixed-size array: `[T; N]`
    Array(Box<Ty>, u64),
    /// Slice: `[T]`
    Slice(Box<Ty>),

    /// Named types (structs, enums, actors, type params)
    Named {
        /// Type name
        name: String,
        /// Generic type arguments
        args: Vec<Ty>,
    },

    /// Function type: `fn(T1, T2) -> R`
    Function {
        /// Parameter types
        params: Vec<Ty>,
        /// Return type
        ret: Box<Ty>,
    },

    /// Closure type: like Function but with captured variable types for Send checking
    Closure {
        /// Parameter types
        params: Vec<Ty>,
        /// Return type
        ret: Box<Ty>,
        /// Types of captured variables from the enclosing scope
        captures: Vec<Ty>,
    },

    /// Actor reference: `ActorRef<A>`
    ActorRef(Box<Ty>),

    /// Stream: `Stream<T>` — readable sequential source, move-only
    Stream(Box<Ty>),

    /// Sink: `Sink<T>` — writable sequential destination, move-only
    Sink(Box<Ty>),

    /// Option type: `Option<T>`
    Option(Box<Ty>),

    /// Result type: `Result<T, E>`
    Result {
        /// Success type
        ok: Box<Ty>,
        /// Error type
        err: Box<Ty>,
    },

    /// Generator type: `Generator<Y, R>`
    Generator {
        /// Yield type
        yields: Box<Ty>,
        /// Return type
        returns: Box<Ty>,
    },

    /// Async generator: `AsyncGenerator<Y>`
    AsyncGenerator {
        /// Yield type
        yields: Box<Ty>,
    },

    /// Pointer types (FFI)
    Pointer {
        /// Whether the pointer is mutable
        is_mutable: bool,
        /// Pointee type
        pointee: Box<Ty>,
    },

    /// Trait object: `dyn Trait` or `dyn (Trait1 + Trait2)`
    TraitObject {
        /// Trait bounds
        traits: Vec<TraitObjectBound>,
    },

    /// Range type: `Range<T>`
    Range(Box<Ty>),

    /// Error recovery — a type that unifies with anything
    Error,
}

/// A substitution mapping type variables to concrete types.
#[derive(Debug, Clone, Default)]
pub struct Substitution {
    mappings: HashMap<TypeVar, Ty>,
}

impl Substitution {
    /// Create an empty substitution.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a mapping without occurs check (internal use).
    pub fn insert(&mut self, var: TypeVar, ty: Ty) {
        self.mappings.insert(var, ty);
    }

    /// Look up a type variable in the substitution.
    #[must_use]
    pub fn lookup(&self, var: TypeVar) -> Option<&Ty> {
        self.mappings.get(&var)
    }

    /// Get all mappings.
    #[must_use]
    pub fn mappings(&self) -> &HashMap<TypeVar, Ty> {
        &self.mappings
    }

    /// Snapshot the current substitution state for rollback.
    #[must_use]
    pub fn snapshot(&self) -> HashMap<TypeVar, Ty> {
        self.mappings.clone()
    }

    /// Restore the substitution to a previous snapshot.
    pub fn restore(&mut self, snapshot: HashMap<TypeVar, Ty>) {
        self.mappings = snapshot;
    }

    /// Walk a variable to its final resolved type.
    #[must_use]
    pub fn resolve(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(v) => match self.lookup(*v) {
                Some(resolved) => self.resolve(resolved),
                None => ty.clone(),
            },
            _ => ty.apply_subst(self),
        }
    }
}

impl Ty {
    /// Check if this is a numeric type (integer or float).
    #[must_use]
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Check if this is an integer type.
    #[must_use]
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64
        )
    }

    /// Check if this is a floating-point type.
    #[must_use]
    pub fn is_float(&self) -> bool {
        matches!(self, Ty::F32 | Ty::F64)
    }

    /// Check if this is a primitive type.
    #[must_use]
    pub fn is_primitive(&self) -> bool {
        self.is_integer() || self.is_float() || matches!(self, Ty::Bool | Ty::Char | Ty::Unit)
    }

    /// Check if this type is implicitly copied (value semantics).
    #[must_use]
    pub fn is_copy(&self) -> bool {
        if self.is_primitive() {
            return true;
        }
        matches!(self, Ty::Never | Ty::Pointer { .. })
            || matches!(self, Ty::Tuple(elems) if elems.iter().all(Ty::is_copy))
            || matches!(self, Ty::Array(elem, _) if elem.is_copy())
    }

    /// Check if this type contains a specific type variable (occurs check).
    #[must_use]
    pub fn contains_var(&self, v: TypeVar) -> bool {
        if let Ty::Var(tv) = self {
            return *tv == v;
        }
        self.any_child(&|child| child.contains_var(v))
    }

    /// Substitute a single type variable with a replacement type.
    #[must_use]
    pub fn substitute(&self, var: TypeVar, replacement: &Ty) -> Ty {
        if let Ty::Var(tv) = self {
            if *tv == var {
                return replacement.clone();
            }
        }
        self.map_children(&|child| child.substitute(var, replacement))
    }

    /// Apply a full substitution to this type.
    #[must_use]
    pub fn apply_subst(&self, subst: &Substitution) -> Ty {
        if subst.mappings().is_empty() {
            return self.clone();
        }
        if let Ty::Var(v) = self {
            return match subst.lookup(*v) {
                Some(resolved) => resolved.apply_subst(subst),
                None => self.clone(),
            };
        }
        self.map_children(&|child| child.apply_subst(subst))
    }

    /// Apply a function to each child type, reconstructing the composite.
    /// Leaf types (primitives, Var, Error) return `self.clone()`.
    fn map_children(&self, f: &impl Fn(&Ty) -> Ty) -> Ty {
        match self {
            Ty::Tuple(elems) => Ty::Tuple(elems.iter().map(f).collect()),
            Ty::Array(elem, size) => Ty::Array(Box::new(f(elem)), *size),
            Ty::Slice(elem) => Ty::Slice(Box::new(f(elem))),
            Ty::Named { name, args } => Ty::Named {
                name: name.clone(),
                args: args.iter().map(f).collect(),
            },
            Ty::Function { params, ret } => Ty::Function {
                params: params.iter().map(f).collect(),
                ret: Box::new(f(ret)),
            },
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params.iter().map(f).collect(),
                ret: Box::new(f(ret)),
                captures: captures.iter().map(f).collect(),
            },
            Ty::ActorRef(inner) => Ty::ActorRef(Box::new(f(inner))),
            Ty::Stream(inner) => Ty::Stream(Box::new(f(inner))),
            Ty::Sink(inner) => Ty::Sink(Box::new(f(inner))),
            Ty::Option(inner) => Ty::Option(Box::new(f(inner))),
            Ty::Range(inner) => Ty::Range(Box::new(f(inner))),
            Ty::Result { ok, err } => Ty::Result {
                ok: Box::new(f(ok)),
                err: Box::new(f(err)),
            },
            Ty::Generator { yields, returns } => Ty::Generator {
                yields: Box::new(f(yields)),
                returns: Box::new(f(returns)),
            },
            Ty::AsyncGenerator { yields } => Ty::AsyncGenerator {
                yields: Box::new(f(yields)),
            },
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(f(pointee)),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound.args.iter().map(f).collect(),
                    })
                    .collect(),
            },
            _ => self.clone(),
        }
    }

    /// Check if any child type satisfies a predicate (boolean fold).
    fn any_child(&self, f: &impl Fn(&Ty) -> bool) -> bool {
        match self {
            Ty::Tuple(elems) => elems.iter().any(f),
            Ty::Array(elem, _) | Ty::Slice(elem) => f(elem),
            Ty::Named { args, .. } => args.iter().any(f),
            Ty::Function { params, ret } => params.iter().any(f) || f(ret),
            Ty::Closure {
                params,
                ret,
                captures,
            } => params.iter().any(f) || f(ret) || captures.iter().any(f),
            Ty::ActorRef(inner)
            | Ty::Option(inner)
            | Ty::Range(inner)
            | Ty::Stream(inner)
            | Ty::Sink(inner) => f(inner),
            Ty::Result { ok, err } => f(ok) || f(err),
            Ty::Generator { yields, returns } => f(yields) || f(returns),
            Ty::AsyncGenerator { yields } => f(yields),
            Ty::Pointer { pointee, .. } => f(pointee),
            Ty::TraitObject { traits } => traits.iter().any(|bound| bound.args.iter().any(f)),
            _ => false,
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::I8 => write!(f, "i8"),
            Ty::I16 => write!(f, "i16"),
            Ty::I32 => write!(f, "i32"),
            Ty::I64 => write!(f, "i64"),
            Ty::U8 => write!(f, "u8"),
            Ty::U16 => write!(f, "u16"),
            Ty::U32 => write!(f, "u32"),
            Ty::U64 => write!(f, "u64"),
            Ty::F32 => write!(f, "f32"),
            Ty::F64 => write!(f, "f64"),
            Ty::Bool => write!(f, "bool"),
            Ty::Char => write!(f, "char"),
            Ty::String => write!(f, "String"),
            Ty::Unit => write!(f, "()"),
            Ty::Never => write!(f, "!"),
            Ty::Var(v) => write!(f, "{v}"),
            Ty::Tuple(elems) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            Ty::Array(elem, size) => write!(f, "[{elem}; {size}]"),
            Ty::Slice(elem) => write!(f, "[{elem}]"),
            Ty::Named { name, args } => {
                write!(f, "{name}")?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{arg}")?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Ty::Function { params, ret } | Ty::Closure { params, ret, .. } => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{param}")?;
                }
                write!(f, ") -> {ret}")
            }
            Ty::ActorRef(inner) => write!(f, "ActorRef<{inner}>"),
            Ty::Stream(inner) => write!(f, "Stream<{inner}>"),
            Ty::Sink(inner) => write!(f, "Sink<{inner}>"),
            Ty::Option(inner) => write!(f, "Option<{inner}>"),
            Ty::Result { ok, err } => write!(f, "Result<{ok}, {err}>"),
            Ty::Generator { yields, returns } => write!(f, "Generator<{yields}, {returns}>"),
            Ty::AsyncGenerator { yields } => write!(f, "AsyncGenerator<{yields}>"),
            Ty::Pointer {
                is_mutable,
                pointee,
            } => {
                if *is_mutable {
                    write!(f, "*mut {pointee}")
                } else {
                    write!(f, "*const {pointee}")
                }
            }
            Ty::TraitObject { traits } => {
                write!(f, "dyn ")?;
                if traits.len() == 1 {
                    let bound = &traits[0];
                    write!(f, "{}", bound.trait_name)?;
                    if !bound.args.is_empty() {
                        write!(f, "<")?;
                        for (i, arg) in bound.args.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{arg}")?;
                        }
                        write!(f, ">")?;
                    }
                } else {
                    write!(f, "(")?;
                    for (i, bound) in traits.iter().enumerate() {
                        if i > 0 {
                            write!(f, " + ")?;
                        }
                        write!(f, "{}", bound.trait_name)?;
                        if !bound.args.is_empty() {
                            write!(f, "<")?;
                            for (j, arg) in bound.args.iter().enumerate() {
                                if j > 0 {
                                    write!(f, ", ")?;
                                }
                                write!(f, "{arg}")?;
                            }
                            write!(f, ">")?;
                        }
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Ty::Range(inner) => write!(f, "Range<{inner}>"),
            Ty::Error => write!(f, "<error>"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_var_fresh() {
        let v1 = TypeVar::fresh();
        let v2 = TypeVar::fresh();
        assert_ne!(v1, v2);
    }

    #[test]
    fn test_is_numeric() {
        assert!(Ty::I32.is_numeric());
        assert!(Ty::F64.is_numeric());
        assert!(!Ty::Bool.is_numeric());
        assert!(!Ty::String.is_numeric());
    }

    #[test]
    fn test_is_copy() {
        assert!(Ty::I32.is_copy());
        assert!(Ty::Bool.is_copy());
        assert!(!Ty::String.is_copy());
        assert!(Ty::Tuple(vec![Ty::I32, Ty::Bool]).is_copy());
        assert!(!Ty::Tuple(vec![Ty::I32, Ty::String]).is_copy());
    }

    #[test]
    fn test_contains_var() {
        TypeVar::reset();
        let v = TypeVar::fresh();
        let ty = Ty::Tuple(vec![Ty::I32, Ty::Var(v)]);
        assert!(ty.contains_var(v));
        assert!(!Ty::I32.contains_var(v));
    }

    #[test]
    fn test_substitute() {
        TypeVar::reset();
        let v = TypeVar::fresh();
        let ty = Ty::Tuple(vec![Ty::Var(v), Ty::I32]);
        let result = ty.substitute(v, &Ty::Bool);
        assert_eq!(result, Ty::Tuple(vec![Ty::Bool, Ty::I32]));
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Ty::I32), "i32");
        assert_eq!(
            format!(
                "{}",
                Ty::Function {
                    params: vec![Ty::I32, Ty::Bool],
                    ret: Box::new(Ty::String),
                }
            ),
            "fn(i32, bool) -> String"
        );
        assert_eq!(
            format!(
                "{}",
                Ty::Named {
                    name: "Vec".to_string(),
                    args: vec![Ty::I32],
                }
            ),
            "Vec<i32>"
        );
    }

    #[test]
    fn test_is_integer() {
        assert!(Ty::I8.is_integer());
        assert!(Ty::I16.is_integer());
        assert!(Ty::I32.is_integer());
        assert!(Ty::I64.is_integer());
        assert!(Ty::U8.is_integer());
        assert!(Ty::U16.is_integer());
        assert!(Ty::U32.is_integer());
        assert!(Ty::U64.is_integer());
        assert!(!Ty::F32.is_integer());
        assert!(!Ty::F64.is_integer());
        assert!(!Ty::Bool.is_integer());
    }

    #[test]
    fn test_is_float() {
        assert!(Ty::F32.is_float());
        assert!(Ty::F64.is_float());
        assert!(!Ty::I32.is_float());
        assert!(!Ty::Bool.is_float());
    }

    #[test]
    fn test_display_option() {
        assert_eq!(format!("{}", Ty::Option(Box::new(Ty::I32))), "Option<i32>");
    }

    #[test]
    fn test_display_result() {
        assert_eq!(
            format!(
                "{}",
                Ty::Result {
                    ok: Box::new(Ty::I32),
                    err: Box::new(Ty::String),
                }
            ),
            "Result<i32, String>"
        );
    }

    #[test]
    fn test_display_tuple() {
        assert_eq!(
            format!("{}", Ty::Tuple(vec![Ty::I32, Ty::Bool, Ty::String])),
            "(i32, bool, String)"
        );
    }

    #[test]
    fn test_display_empty_tuple() {
        assert_eq!(format!("{}", Ty::Tuple(vec![])), "()");
    }

    #[test]
    fn test_display_unit() {
        assert_eq!(format!("{}", Ty::Unit), "()");
    }

    #[test]
    fn test_display_never() {
        assert_eq!(format!("{}", Ty::Never), "!");
    }

    #[test]
    fn test_substitute_nested() {
        TypeVar::reset();
        let v = TypeVar::fresh();
        let ty = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::Tuple(vec![Ty::Var(v), Ty::Bool])],
        };
        let result = ty.substitute(v, &Ty::String);
        assert_eq!(
            result,
            Ty::Named {
                name: "Vec".to_string(),
                args: vec![Ty::Tuple(vec![Ty::String, Ty::Bool])],
            }
        );
    }

    #[test]
    fn test_substitute_no_match() {
        TypeVar::reset();
        let v1 = TypeVar::fresh();
        let v2 = TypeVar::fresh();
        let ty = Ty::Tuple(vec![Ty::Var(v1), Ty::I32]);
        let result = ty.substitute(v2, &Ty::Bool);
        // v2 is not present, so no substitution happens
        assert_eq!(result, Ty::Tuple(vec![Ty::Var(v1), Ty::I32]));
    }

    #[test]
    fn test_contains_var_in_function() {
        TypeVar::reset();
        let v = TypeVar::fresh();
        let ty = Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Var(v)),
        };
        assert!(ty.contains_var(v));
    }

    #[test]
    fn test_contains_var_in_option() {
        TypeVar::reset();
        let v = TypeVar::fresh();
        let ty = Ty::Option(Box::new(Ty::Var(v)));
        assert!(ty.contains_var(v));
    }

    #[test]
    fn test_is_copy_array() {
        assert!(Ty::Array(Box::new(Ty::I32), 10).is_copy());
        assert!(!Ty::Array(Box::new(Ty::String), 10).is_copy());
    }
}
