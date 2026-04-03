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
    /// Ref-counted byte buffer
    Bytes,
    /// Duration in nanoseconds (distinct from i64)
    Duration,
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

    /// Machine type (value-type state machine)
    Machine { name: String },

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
    /// Canonical mapping from type-name strings (including user-facing aliases)
    /// to primitive `Ty` variants.  Returns `None` for non-primitive names.
    ///
    /// This is the **single source of truth** — every call site that needs to
    /// resolve a type name string to a `Ty` must go through this function.
    #[must_use]
    pub fn from_name(name: &str) -> Option<Ty> {
        Some(match name {
            "i8" => Ty::I8,
            "i16" => Ty::I16,
            "i32" => Ty::I32,
            "i64" | "int" | "Int" => Ty::I64,
            "u8" | "byte" => Ty::U8,
            "u16" => Ty::U16,
            "u32" => Ty::U32,
            "u64" | "uint" => Ty::U64,
            "f32" => Ty::F32,
            "f64" | "float" | "Float" => Ty::F64,
            "bool" | "Bool" => Ty::Bool,
            "char" | "Char" => Ty::Char,
            "string" | "String" | "str" => Ty::String,
            "bytes" | "Bytes" => Ty::Bytes,
            "duration" | "Duration" => Ty::Duration,
            "()" => Ty::Unit,
            _ => return None,
        })
    }

    /// Wrap this type for user-facing display without changing internal identity.
    #[must_use]
    pub fn user_facing(&self) -> UserFacingTy<'_> {
        UserFacingTy(self)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "type display covers all type variants recursively"
    )]
    fn fmt_with_i64_name(&self, f: &mut fmt::Formatter<'_>, i64_name: &str) -> fmt::Result {
        match self {
            Ty::I8 => write!(f, "i8"),
            Ty::I16 => write!(f, "i16"),
            Ty::I32 => write!(f, "i32"),
            Ty::I64 => write!(f, "{i64_name}"),
            Ty::U8 => write!(f, "u8"),
            Ty::U16 => write!(f, "u16"),
            Ty::U32 => write!(f, "u32"),
            Ty::U64 => write!(f, "u64"),
            Ty::F32 => write!(f, "f32"),
            Ty::F64 => write!(f, "f64"),
            Ty::Bool => write!(f, "bool"),
            Ty::Char => write!(f, "char"),
            Ty::String => write!(f, "String"),
            Ty::Bytes => write!(f, "bytes"),
            Ty::Duration => write!(f, "duration"),
            Ty::Unit => write!(f, "()"),
            Ty::Never => write!(f, "!"),
            Ty::Var(v) => write!(f, "{v}"),
            Ty::Tuple(elems) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    elem.fmt_with_i64_name(f, i64_name)?;
                }
                write!(f, ")")
            }
            Ty::Array(elem, size) => {
                write!(f, "[")?;
                elem.fmt_with_i64_name(f, i64_name)?;
                write!(f, "; {size}]")
            }
            Ty::Slice(elem) => {
                write!(f, "[")?;
                elem.fmt_with_i64_name(f, i64_name)?;
                write!(f, "]")
            }
            Ty::Named { name, args } => {
                write!(f, "{name}")?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        arg.fmt_with_i64_name(f, i64_name)?;
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
                    param.fmt_with_i64_name(f, i64_name)?;
                }
                write!(f, ") -> ")?;
                ret.fmt_with_i64_name(f, i64_name)
            }
            Ty::Pointer {
                is_mutable,
                pointee,
            } => {
                if *is_mutable {
                    write!(f, "*mut ")?;
                } else {
                    write!(f, "*const ")?;
                }
                pointee.fmt_with_i64_name(f, i64_name)
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
                            arg.fmt_with_i64_name(f, i64_name)?;
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
                                arg.fmt_with_i64_name(f, i64_name)?;
                            }
                            write!(f, ">")?;
                        }
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Ty::Machine { name } => write!(f, "{name}"),
            Ty::Error => write!(f, "<error>"),
        }
    }

    #[must_use]
    fn canonical_named_builtin(name: &str) -> Option<&'static str> {
        Some(match name {
            "Option" => "Option",
            "Result" => "Result",
            "Vec" => "Vec",
            "HashMap" => "HashMap",
            "ActorRef" => "ActorRef",
            "Actor" => "Actor",
            "Task" => "Task",
            "Stream" | "stream.Stream" => "Stream",
            "Sink" | "stream.Sink" => "Sink",
            "StreamPair" => "StreamPair",
            "Sender" | "channel.Sender" => "Sender",
            "Receiver" | "channel.Receiver" => "Receiver",
            "Generator" => "Generator",
            "AsyncGenerator" => "AsyncGenerator",
            "Range" => "Range",
            _ => return None,
        })
    }

    #[must_use]
    pub(crate) fn is_named_builtin(name: &str) -> bool {
        Self::canonical_named_builtin(name).is_some()
    }

    // -- Constructor helpers: all produce Ty::Named --

    /// Construct `Option<inner>`.
    #[must_use]
    pub fn option(inner: Ty) -> Ty {
        Self::normalize_named("Option".to_string(), vec![inner])
    }

    /// Construct `Result<ok, err>`.
    #[must_use]
    pub fn result(ok: Ty, err: Ty) -> Ty {
        Self::normalize_named("Result".to_string(), vec![ok, err])
    }

    /// Construct `ActorRef<inner>`.
    #[must_use]
    pub fn actor_ref(inner: Ty) -> Ty {
        Self::normalize_named("ActorRef".to_string(), vec![inner])
    }

    /// Construct `Sender<inner>`.
    #[must_use]
    pub fn sender(inner: Ty) -> Ty {
        Self::normalize_named("Sender".to_string(), vec![inner])
    }

    /// Construct `Receiver<inner>`.
    #[must_use]
    pub fn receiver(inner: Ty) -> Ty {
        Self::normalize_named("Receiver".to_string(), vec![inner])
    }

    /// Construct `Stream<inner>`.
    #[must_use]
    pub fn stream(inner: Ty) -> Ty {
        Self::normalize_named("Stream".to_string(), vec![inner])
    }

    /// Construct `Sink<inner>`.
    #[must_use]
    pub fn sink(inner: Ty) -> Ty {
        Self::normalize_named("Sink".to_string(), vec![inner])
    }

    /// Construct `Generator<yields, returns>`.
    #[must_use]
    pub fn generator(yields: Ty, returns: Ty) -> Ty {
        Self::normalize_named("Generator".to_string(), vec![yields, returns])
    }

    /// Construct `AsyncGenerator<yields>`.
    #[must_use]
    pub fn async_generator(yields: Ty) -> Ty {
        Self::normalize_named("AsyncGenerator".to_string(), vec![yields])
    }

    /// Construct `Range<inner>`.
    #[must_use]
    pub fn range(inner: Ty) -> Ty {
        Self::normalize_named("Range".to_string(), vec![inner])
    }

    // -- Accessor helpers: match on Named patterns --

    /// If this is `Option<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_option(&self) -> Option<&Ty> {
        match self {
            Ty::Named { name, args } if name == "Option" && args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// If this is `Result<T, E>`, return `Some((&T, &E))`.
    #[must_use]
    pub fn as_result(&self) -> Option<(&Ty, &Ty)> {
        match self {
            Ty::Named { name, args } if name == "Result" && args.len() == 2 => {
                Some((&args[0], &args[1]))
            }
            _ => None,
        }
    }

    /// If this is `ActorRef<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_actor_ref(&self) -> Option<&Ty> {
        match self {
            Ty::Named { name, args } if name == "ActorRef" && args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// If this is an actor handle (`ActorRef<T>` or `Actor<T>`), return `Some(&T)`.
    #[must_use]
    pub fn as_actor_handle(&self) -> Option<&Ty> {
        match self {
            Ty::Named { name, args }
                if (name == "ActorRef" || name == "Actor") && args.len() == 1 =>
            {
                Some(&args[0])
            }
            _ => None,
        }
    }

    /// If this is `Stream<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_stream(&self) -> Option<&Ty> {
        match self {
            Ty::Named { name, args } if name == "Stream" && args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// If this is `Sink<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_sink(&self) -> Option<&Ty> {
        match self {
            Ty::Named { name, args } if name == "Sink" && args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// If this is `Generator<Y, R>`, return `Some((&Y, &R))`.
    #[must_use]
    pub fn as_generator(&self) -> Option<(&Ty, &Ty)> {
        match self {
            Ty::Named { name, args } if name == "Generator" && args.len() == 2 => {
                Some((&args[0], &args[1]))
            }
            _ => None,
        }
    }

    /// If this is `AsyncGenerator<Y>`, return `Some(&Y)`.
    #[must_use]
    pub fn as_async_generator(&self) -> Option<&Ty> {
        match self {
            Ty::Named { name, args } if name == "AsyncGenerator" && args.len() == 1 => {
                Some(&args[0])
            }
            _ => None,
        }
    }

    /// If this is `Range<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_range(&self) -> Option<&Ty> {
        match self {
            Ty::Named { name, args } if name == "Range" && args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// Check if this is a Stream type.
    #[must_use]
    pub fn is_stream(&self) -> bool {
        self.as_stream().is_some()
    }

    /// Check if this is a Sink type.
    #[must_use]
    pub fn is_sink(&self) -> bool {
        self.as_sink().is_some()
    }

    /// Canonicalize known named builtins to their shared spelling before
    /// constructing `Ty::Named`.
    #[must_use]
    pub fn normalize_named(name: String, args: Vec<Ty>) -> Ty {
        let name = match Self::canonical_named_builtin(&name) {
            Some(canonical) if canonical != name => canonical.to_string(),
            _ => name,
        };
        Ty::Named { name, args }
    }

    /// Check if this is a numeric type (integer or float).
    #[must_use]
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Check if this is the bytes type.
    #[must_use]
    pub fn is_bytes(&self) -> bool {
        matches!(self, Ty::Bytes)
    }

    /// Check if this is the duration type.
    #[must_use]
    pub fn is_duration(&self) -> bool {
        matches!(self, Ty::Duration)
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
        self.is_integer()
            || self.is_float()
            || matches!(self, Ty::Bool | Ty::Char | Ty::Unit | Ty::Duration)
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
            Ty::Machine { name } => Ty::Machine { name: name.clone() },
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
            Ty::Pointer { pointee, .. } => f(pointee),
            Ty::TraitObject { traits } => traits.iter().any(|bound| bound.args.iter().any(f)),
            _ => false,
        }
    }
}

/// User-facing type wrapper that preserves `int` spelling for canonical `Ty::I64`.
#[derive(Debug)]
pub struct UserFacingTy<'a>(&'a Ty);

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_i64_name(f, "i64")
    }
}

impl fmt::Display for UserFacingTy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt_with_i64_name(f, "int")
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
    fn test_user_facing_display_preserves_int_alias() {
        assert_eq!(Ty::I64.user_facing().to_string(), "int");
    }

    #[test]
    fn test_user_facing_display_formats_nested_types() {
        let ty = Ty::Function {
            params: vec![
                Ty::Named {
                    name: "Vec".to_string(),
                    args: vec![Ty::I64],
                },
                Ty::Tuple(vec![Ty::Bool, Ty::I64]),
            ],
            ret: Box::new(Ty::result(
                Ty::I64,
                Ty::Named {
                    name: "HashMap".to_string(),
                    args: vec![Ty::String, Ty::I64],
                },
            )),
        };

        assert_eq!(
            ty.user_facing().to_string(),
            "fn(Vec<int>, (bool, int)) -> Result<int, HashMap<String, int>>"
        );
        assert_eq!(
            ty.to_string(),
            "fn(Vec<i64>, (bool, i64)) -> Result<i64, HashMap<String, i64>>"
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
        assert_eq!(format!("{}", Ty::option(Ty::I32)), "Option<i32>");
    }

    #[test]
    fn test_display_result() {
        assert_eq!(
            format!("{}", Ty::result(Ty::I32, Ty::String)),
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
        let ty = Ty::option(Ty::Var(v));
        assert!(ty.contains_var(v));
    }

    #[test]
    fn test_is_copy_array() {
        assert!(Ty::Array(Box::new(Ty::I32), 10).is_copy());
        assert!(!Ty::Array(Box::new(Ty::String), 10).is_copy());
    }
}
