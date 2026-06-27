//! Internal type representation for the Hew type checker.
//!
//! This module defines `Ty`, the core type representation used throughout
//! the type checker. Types are structural and support substitution for
//! type inference variables.

use crate::builtin_names::BuiltinNamedType;
use crate::builtin_type::{lookup_builtin_type, BuiltinType};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::atomic::{AtomicU32, Ordering};

/// A unique type variable ID for inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

/// Counter for generating fresh type variables.
static NEXT_TYPE_VAR: AtomicU32 = AtomicU32::new(0);

fn builtin_named_type_from_builtin(builtin: Option<BuiltinType>) -> Option<BuiltinNamedType> {
    match builtin {
        Some(BuiltinType::Sender) => Some(BuiltinNamedType::Sender),
        Some(BuiltinType::Receiver) => Some(BuiltinNamedType::Receiver),
        Some(BuiltinType::Stream) => Some(BuiltinNamedType::Stream),
        Some(BuiltinType::Sink) => Some(BuiltinNamedType::Sink),
        Some(BuiltinType::Duplex) => Some(BuiltinNamedType::Duplex),
        Some(BuiltinType::CancellationToken) => Some(BuiltinNamedType::CancellationToken),
        Some(BuiltinType::LocalPid) => Some(BuiltinNamedType::LocalPid),
        Some(BuiltinType::RemotePid) => Some(BuiltinNamedType::RemotePid),
        Some(
            BuiltinType::Option
            | BuiltinType::Result
            | BuiltinType::Vec
            | BuiltinType::HashMap
            | BuiltinType::HashSet
            | BuiltinType::ActorRef
            | BuiltinType::Actor
            | BuiltinType::Task
            | BuiltinType::StreamPair
            | BuiltinType::Generator
            | BuiltinType::AsyncGenerator
            | BuiltinType::Range
            | BuiltinType::Rc
            | BuiltinType::Pid
            | BuiltinType::HewActor
            | BuiltinType::HewDuplex
            | BuiltinType::HewSendHalf
            | BuiltinType::HewRecvHalf
            | BuiltinType::BoxedActor
            | BuiltinType::ActorState
            | BuiltinType::MachineState
            | BuiltinType::SendHalf
            | BuiltinType::RecvHalf
            | BuiltinType::LambdaActorHandle
            | BuiltinType::LambdaPid
            | BuiltinType::CrashInfo
            | BuiltinType::CrashAction
            | BuiltinType::CrashNotification
            | BuiltinType::CrashKind
            | BuiltinType::SendError
            | BuiltinType::AskError
            | BuiltinType::RecvError
            | BuiltinType::LinkError
            | BuiltinType::MonitorRef
            | BuiltinType::NarrowError
            | BuiltinType::CloseError
            | BuiltinType::Iterator
            | BuiltinType::Unit
            | BuiltinType::Duration
            | BuiltinType::Instant
            | BuiltinType::Trap
            | BuiltinType::TimeoutError,
        )
        | None => None,
    }
}

impl TypeVar {
    /// Create a fresh, globally unique type variable.
    #[must_use]
    pub fn fresh() -> Self {
        Self(NEXT_TYPE_VAR.fetch_add(1, Ordering::Relaxed))
    }

    /// Reset the type variable counter.
    ///
    /// Only available in test builds. Production code must never reset the
    /// counter; doing so outside a single-threaded test context causes two
    /// distinct inference variables to share the same ID (a silent
    /// non-determinism source when nextest runs tests in parallel).
    #[cfg(test)]
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
    /// Associated-type bindings projected on this trait object bound.
    ///
    /// Stored sorted by associated-type name at type-resolution boundaries so
    /// equality, hashing, display, and vtable-key construction are canonical.
    pub assoc_bindings: Vec<(String, Ty)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HashSetLoweringTypeKey {
    I64,
    U64,
    String,
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
    /// Platform-sized signed integer: 32-bit on WASM32, 64-bit on native.
    /// Distinct from `i64` (which is always fixed 64-bit).
    Isize,
    /// Platform-sized unsigned integer: 32-bit on WASM32, 64-bit on native.
    /// Distinct from `u64` (which is always fixed 64-bit).
    Usize,
    /// 32-bit floating point
    F32,
    /// 64-bit floating point
    F64,
    /// Integer literal kind awaiting contextual coercion/defaulting
    IntLiteral,
    /// Floating-point literal kind awaiting contextual coercion/defaulting
    FloatLiteral,
    /// Boolean
    Bool,
    /// Unicode character
    Char,
    /// UTF-8 string
    String,
    /// Ref-counted byte buffer
    Bytes,
    /// Ref-counted cancellation token handle
    CancellationToken,
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
        /// Compiler-known builtin discriminator, when this name comes from a
        /// canonical builtin source rather than user-defined source.
        builtin: Option<BuiltinType>,
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

    /// Immutable borrow `&T` — a first-class, no-retain shared reference to a
    /// value owned elsewhere. Distinct from `Pointer { is_mutable: false }`:
    /// a borrow is non-owning, never retained on copy (the M-COW retain-skip
    /// opt-out), classified `View` for drop purposes, and carries
    /// borrow-specific send/return-escape semantics. `&mut`/`&var` are rejected
    /// at parse today, so a borrow is always immutable.
    Borrow {
        /// Borrowed (pointee) type
        pointee: Box<Ty>,
    },

    /// Trait object: `dyn Trait` or `dyn (Trait1 + Trait2)`
    TraitObject {
        /// Trait bounds
        traits: Vec<TraitObjectBound>,
    },

    /// Compiler-internal type for a running child task whose result has type
    /// `T`. Produced exclusively by HIR lowering of `fork name = call_expr`
    /// inside a `fork{}` body; never user-nameable (see `E_TASK_NOT_NAMEABLE`
    /// in HIR diagnostics). The user writes `fork name = expr`, not
    /// `let name: Task<T> = expr`.
    ///
    /// Display: `<task<T>>` (angle brackets signal compiler-internal origin).
    Task(Box<Ty>),

    /// Deferred associated-type projection: `T::Bar` where `T` is a generic
    /// type parameter with a trait bound that declares `type Bar`.
    ///
    /// Produced by the resolver when it sees `T::Bar` in a type expression and
    /// `T` is a known type parameter whose bounds include a trait declaring
    /// `Bar`. The variant carries the trait name (disambiguating when `T` has
    /// multiple bounds) plus the assoc-type name. Substitution collapses this
    /// to the impl's binding once `base` becomes concrete (see
    /// `Checker::project_assoc_types`).
    ///
    /// Display: `<base>::<assoc>` (e.g. `I::Item`).
    ///
    /// Strings are boxed to keep the variant under the `result_large_err`
    /// clippy threshold (Ty appears as both arms of `UnifyError::Mismatch`).
    AssocType {
        /// Carrier of the type parameter (`Ty::Named { name: "I", args: [] }`
        /// at registration time; may become a concrete `Ty` after
        /// monomorphisation substitutes the type-arg).
        base: Box<Ty>,
        /// Trait that declares the associated type. Required for
        /// disambiguation when `base` has multiple bounds.
        trait_name: Box<str>,
        /// Associated type name (e.g. `"Item"`).
        assoc_name: Box<str>,
    },

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

    /// Insert a mapping after resolving and occurs-checking it.
    ///
    /// # Errors
    /// Returns an occurs-check error when the resolved mapping would introduce
    /// an infinite type or substitution cycle.
    #[allow(
        clippy::result_large_err,
        reason = "UnifyError intentionally carries concrete Ty values for diagnostics"
    )]
    pub fn insert(&mut self, var: TypeVar, ty: &Ty) -> Result<(), crate::unify::UnifyError> {
        let resolved = ty.apply_subst(self);
        if resolved.contains_var(var) {
            return Err(crate::unify::UnifyError::OccursCheck { var, ty: resolved });
        }
        self.mappings.insert(var, resolved);
        Ok(())
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
        self.resolve_inner(ty, &mut HashSet::new())
    }

    fn resolve_inner(&self, ty: &Ty, visited: &mut HashSet<TypeVar>) -> Ty {
        match ty {
            Ty::Var(v) => match self.lookup(*v) {
                Some(resolved) => {
                    if !visited.insert(*v) {
                        return Ty::Error;
                    }
                    let resolved = self.resolve_inner(resolved, visited);
                    visited.remove(v);
                    resolved
                }
                None => ty.clone(),
            },
            _ => ty.apply_subst_inner(self, visited),
        }
    }
}

/// Canonical primitive-type alias table: each row is
/// `(canonical_name, all_aliases_including_canonical)`.
///
/// The **canonical name** (`row.0`) is the first alias and is what
/// `canonical_lowering_name` returns.  All other aliases in `row.1` are
/// user-facing spellings that the parser and type-checker accept.
///
/// This is the **single source of truth** for primitive name resolution.
/// `primitive_from_name`, `canonical_lowering_name`, and the wirecodec
/// `PRIMITIVE_DESCS` table all derive their alias lists from here.
///
/// Row order: numeric types first (signed, then unsigned, then floats),
/// followed by non-numeric scalars, then the special forms `()` and `!`.
/// The `!` row is consumed by `primitive_from_name` only; wirecodec has no
/// wire representation for `never`. Unit has an explicit wirecodec nil kind,
/// whose canonical wire spelling is `unit`.
pub const PRIMITIVE_ALIASES: &[(&str, &[&str])] = &[
    ("i8", &["i8"]),
    ("i16", &["i16"]),
    ("i32", &["i32"]),
    ("i64", &["i64"]),
    ("u8", &["u8"]),
    ("u16", &["u16"]),
    ("u32", &["u32"]),
    ("u64", &["u64"]),
    ("isize", &["isize"]),
    ("usize", &["usize"]),
    ("f32", &["f32"]),
    ("f64", &["f64"]),
    ("bool", &["bool"]),
    ("char", &["char"]),
    ("string", &["string"]),
    ("bytes", &["bytes"]),
    ("duration", &["duration"]),
    // Unit's type-system spelling is `()`, while wirecodec also accepts `unit`.
    ("()", &["()"]),
    // Not in wirecodec: Never has no wire representation.
    ("!", &["!"]),
];

/// Return the alias slice for the given canonical primitive name.
///
/// Intended for use in `const` initializers in downstream crates so the
/// wirecodec's `PRIMITIVE_DESCS` can reference the same alias lists without
/// duplicating the string literals.
///
/// # Panics
///
/// Panics (at compile time when used in a `const` context) if `canonical`
/// does not appear as a row key in `PRIMITIVE_ALIASES`.
#[must_use]
pub const fn aliases_for(canonical: &str) -> &'static [&'static str] {
    let needle = canonical.as_bytes();
    let mut i = 0;
    while i < PRIMITIVE_ALIASES.len() {
        let key = PRIMITIVE_ALIASES[i].0.as_bytes();
        if bytes_eq(needle, key) {
            return PRIMITIVE_ALIASES[i].1;
        }
        i += 1;
    }
    panic!("aliases_for: unknown canonical primitive name");
}

/// Byte-by-byte equality check usable in `const fn` context.
const fn bytes_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut i = 0;
    while i < a.len() {
        if a[i] != b[i] {
            return false;
        }
        i += 1;
    }
    true
}

impl Ty {
    fn primitive_from_name(name: &str) -> Option<Ty> {
        for (canonical, aliases) in PRIMITIVE_ALIASES {
            if aliases.contains(&name) {
                return Self::from_canonical_primitive_name(canonical);
            }
        }
        None
    }

    /// Resolve a canonical primitive name (the first alias in
    /// `PRIMITIVE_ALIASES`) to the corresponding `Ty` variant.
    fn from_canonical_primitive_name(canonical: &str) -> Option<Ty> {
        Some(match canonical {
            "i8" => Ty::I8,
            "i16" => Ty::I16,
            "i32" => Ty::I32,
            "i64" => Ty::I64,
            "u8" => Ty::U8,
            "u16" => Ty::U16,
            "u32" => Ty::U32,
            "u64" => Ty::U64,
            "isize" => Ty::Isize,
            "usize" => Ty::Usize,
            "f32" => Ty::F32,
            "f64" => Ty::F64,
            "bool" => Ty::Bool,
            "char" => Ty::Char,
            "string" => Ty::String,
            "bytes" => Ty::Bytes,
            "duration" => Ty::Duration,
            "()" => Ty::Unit,
            "!" => Ty::Never,
            _ => return None,
        })
    }

    /// Canonical mapping from type-name strings (including user-facing aliases)
    /// to primitive `Ty` variants.  Returns `None` for non-primitive names.
    ///
    /// This is the **single source of truth** — every call site that needs to
    /// resolve a type name string to a `Ty` must go through this function.
    #[must_use]
    pub fn from_name(name: &str) -> Option<Ty> {
        Self::primitive_from_name(name)
    }

    /// Canonical internal lowering spelling used when serializing resolved
    /// types back into `TypeExpr`s for downstream codegen consumers.
    ///
    /// `Ty::Unit` is intentionally excluded because it lowers structurally to
    /// `TypeExpr::Tuple([])` rather than through a primitive named spelling.
    #[must_use]
    pub fn canonical_lowering_name(&self) -> Option<&'static str> {
        Some(match self {
            Ty::I8 => "i8",
            Ty::I16 => "i16",
            Ty::I32 => "i32",
            Ty::I64 => "i64",
            Ty::U8 => "u8",
            Ty::U16 => "u16",
            Ty::U32 => "u32",
            Ty::U64 => "u64",
            Ty::Isize => "isize",
            Ty::Usize => "usize",
            Ty::F32 => "f32",
            Ty::F64 => "f64",
            Ty::Bool => "bool",
            Ty::Char => "char",
            Ty::String => "string",
            Ty::Bytes => "bytes",
            Ty::Duration => "duration",
            Ty::Never => "!",
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
    fn fmt_with_numeric_names(
        &self,
        f: &mut fmt::Formatter<'_>,
        i64_name: &str,
        f64_name: &str,
    ) -> fmt::Result {
        match self {
            Ty::I8 => write!(f, "i8"),
            Ty::I16 => write!(f, "i16"),
            Ty::I32 => write!(f, "i32"),
            Ty::I64 => write!(f, "{i64_name}"),
            Ty::U8 => write!(f, "u8"),
            Ty::U16 => write!(f, "u16"),
            Ty::U32 => write!(f, "u32"),
            Ty::U64 => write!(f, "u64"),
            Ty::Isize => write!(f, "isize"),
            Ty::Usize => write!(f, "usize"),
            Ty::F32 => write!(f, "f32"),
            Ty::F64 => write!(f, "{f64_name}"),
            Ty::IntLiteral => write!(f, "<int literal>"),
            Ty::FloatLiteral => write!(f, "<float literal>"),
            Ty::Bool => write!(f, "bool"),
            Ty::Char => write!(f, "char"),
            Ty::String => write!(f, "string"),
            Ty::Bytes => write!(f, "bytes"),
            Ty::CancellationToken => write!(f, "CancellationToken"),
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
                    elem.fmt_with_numeric_names(f, i64_name, f64_name)?;
                }
                write!(f, ")")
            }
            Ty::Array(elem, size) => {
                write!(f, "[")?;
                elem.fmt_with_numeric_names(f, i64_name, f64_name)?;
                write!(f, "; {size}]")
            }
            Ty::Slice(elem) => {
                write!(f, "[")?;
                elem.fmt_with_numeric_names(f, i64_name, f64_name)?;
                write!(f, "]")
            }
            Ty::Named { name, args, .. } => {
                write!(f, "{name}")?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        arg.fmt_with_numeric_names(f, i64_name, f64_name)?;
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
                    param.fmt_with_numeric_names(f, i64_name, f64_name)?;
                }
                write!(f, ") -> ")?;
                ret.fmt_with_numeric_names(f, i64_name, f64_name)
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
                pointee.fmt_with_numeric_names(f, i64_name, f64_name)
            }
            Ty::Borrow { pointee } => {
                write!(f, "&")?;
                pointee.fmt_with_numeric_names(f, i64_name, f64_name)
            }
            Ty::TraitObject { traits } => {
                write!(f, "dyn ")?;
                if traits.len() == 1 {
                    let bound = &traits[0];
                    write!(f, "{}", bound.trait_name)?;
                    if !bound.args.is_empty() || !bound.assoc_bindings.is_empty() {
                        write!(f, "<")?;
                        let mut needs_comma = false;
                        for arg in &bound.args {
                            if needs_comma {
                                write!(f, ", ")?;
                            }
                            arg.fmt_with_numeric_names(f, i64_name, f64_name)?;
                            needs_comma = true;
                        }
                        for (assoc_name, assoc_ty) in &bound.assoc_bindings {
                            if needs_comma {
                                write!(f, ", ")?;
                            }
                            write!(f, "{assoc_name} = ")?;
                            assoc_ty.fmt_with_numeric_names(f, i64_name, f64_name)?;
                            needs_comma = true;
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
                        if !bound.args.is_empty() || !bound.assoc_bindings.is_empty() {
                            write!(f, "<")?;
                            let mut needs_comma = false;
                            for arg in &bound.args {
                                if needs_comma {
                                    write!(f, ", ")?;
                                }
                                arg.fmt_with_numeric_names(f, i64_name, f64_name)?;
                                needs_comma = true;
                            }
                            for (assoc_name, assoc_ty) in &bound.assoc_bindings {
                                if needs_comma {
                                    write!(f, ", ")?;
                                }
                                write!(f, "{assoc_name} = ")?;
                                assoc_ty.fmt_with_numeric_names(f, i64_name, f64_name)?;
                                needs_comma = true;
                            }
                            write!(f, ">")?;
                        }
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Ty::Task(inner) => {
                write!(f, "<task<")?;
                inner.fmt_with_numeric_names(f, i64_name, f64_name)?;
                write!(f, ">>")
            }
            Ty::AssocType {
                base, assoc_name, ..
            } => {
                base.fmt_with_numeric_names(f, i64_name, f64_name)?;
                write!(f, "::{assoc_name}")
            }
            Ty::Error => write!(f, "<error>"),
        }
    }

    #[must_use]
    fn canonical_named_builtin(name: &str) -> Option<&'static str> {
        lookup_builtin_type(name).map(BuiltinType::canonical_name)
    }

    #[must_use]
    pub(crate) fn is_named_builtin(name: &str) -> bool {
        Self::canonical_named_builtin(name).is_some()
    }

    #[must_use]
    pub fn is_well_known_type_name(name: &str) -> bool {
        Self::from_name(name).is_some()
            || Self::canonical_named_builtin(name).is_some()
            || matches!(
                name,
                "void"
                    | "never"
                    | "Self"
                    | "Ok"
                    | "Err"
                    | "Some"
                    | "None"
                    | "Arc"
                    | "Weak"
                    | "Send"
                    | "Frozen"
                    | "Copy"
            )
    }

    #[must_use]
    pub fn type_name(&self) -> Option<&str> {
        match self {
            Ty::Named { name, .. } => Some(name),
            _ => None,
        }
    }

    /// Whether two type-name spellings refer to the same nominal type for the
    /// purposes of unification and constructor/variant resolution.
    ///
    /// Two distinct module-qualified names never match — module identity is
    /// authoritative (`a.Reply` ≠ `b.Reply`). A bare name matches a qualified
    /// name with the same final segment only when at most one side is
    /// qualified: this is the same-module bare↔qualified equivalence (a type's
    /// own bare spelling and its `{module}.{name}` alias resolve to one def, and
    /// builtin handles such as `HashSet`/`Sender` match their qualified
    /// stdlib-carrier spelling). Cross-module bare references that could have
    /// matched the *wrong* module's qualified type are rejected upstream at
    /// resolution under qualified-by-default, so a bare name reaching this
    /// comparison has already been validated to a single owning module.
    #[must_use]
    pub fn names_match_qualified(a: &str, b: &str) -> bool {
        if a == b {
            return true;
        }
        let a_qualified = a.contains('.');
        let b_qualified = b.contains('.');
        // Distinct module-qualified names are distinct nominal types.
        if a_qualified && b_qualified {
            return false;
        }
        let a_bare = a.find('.').map_or(a, |dot| &a[dot + 1..]);
        let b_bare = b.find('.').map_or(b, |dot| &b[dot + 1..]);
        a_bare == b_bare
    }

    #[must_use]
    pub fn contains_error(&self) -> bool {
        matches!(self, Ty::Error) || self.any_child(&Ty::contains_error)
    }

    #[must_use]
    pub fn has_inference_var(&self) -> bool {
        match self {
            Ty::Var(_) => true,
            Ty::Tuple(elems) => elems.iter().any(Ty::has_inference_var),
            Ty::Array(elem, _) | Ty::Slice(elem) => elem.has_inference_var(),
            Ty::Named { args, .. } => args.iter().any(Ty::has_inference_var),
            Ty::Function { params, ret } => {
                params.iter().any(Ty::has_inference_var) || ret.has_inference_var()
            }
            Ty::Closure {
                params,
                ret,
                captures,
            } => {
                params.iter().any(Ty::has_inference_var)
                    || ret.has_inference_var()
                    || captures.iter().any(Ty::has_inference_var)
            }
            Ty::Pointer { pointee, .. } => pointee.has_inference_var(),
            Ty::TraitObject { traits } => traits.iter().any(|bound| {
                bound.args.iter().any(Ty::has_inference_var)
                    || bound
                        .assoc_bindings
                        .iter()
                        .any(|(_, ty)| ty.has_inference_var())
            }),
            Ty::Task(inner) => inner.has_inference_var(),
            Ty::AssocType { base, .. } => base.has_inference_var(),
            _ => false,
        }
    }

    // -- Constructor helpers: all produce Ty::Named --

    /// Construct `Option<inner>`.
    #[must_use]
    pub fn option(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Option, vec![inner])
    }

    /// Construct `Result<ok, err>`.
    #[must_use]
    pub fn result(ok: Ty, err: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Result, vec![ok, err])
    }

    /// Construct `ActorRef<inner>`.
    #[must_use]
    pub fn actor_ref(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::ActorRef, vec![inner])
    }

    /// Construct `LocalPid<inner>` — actor pid in this process, returned by `spawn`.
    #[must_use]
    pub fn local_pid(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::LocalPid, vec![inner])
    }

    /// Construct `RemotePid<inner>` — actor pid on a remote node.
    #[must_use]
    pub fn remote_pid(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::RemotePid, vec![inner])
    }

    /// Construct `Sender<inner>`.
    #[must_use]
    pub fn sender(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Sender, vec![inner])
    }

    /// Construct `Receiver<inner>`.
    #[must_use]
    pub fn receiver(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Receiver, vec![inner])
    }

    /// Construct `Stream<inner>`.
    #[must_use]
    pub fn stream(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Stream, vec![inner])
    }

    /// Construct `Sink<inner>`.
    #[must_use]
    pub fn sink(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Sink, vec![inner])
    }

    /// Construct `Duplex<S, R>` — bidirectional lambda-actor handle.
    ///
    /// `S` is the send direction (message type); `R` is the receive direction
    /// (reply type, or `()` for tell-shaped actors).  Send iff both S and R are Send.
    #[must_use]
    pub fn duplex(send: Ty, reply: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Duplex, vec![send, reply])
    }

    /// Extract `(S, R)` from `Duplex<S, R>`, or `None` if not a Duplex.
    #[must_use]
    pub fn as_duplex(&self) -> Option<(&Ty, &Ty)> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::Duplex),
                args,
                ..
            } if args.len() == 2 => Some((&args[0], &args[1])),
            _ => None,
        }
    }

    /// Construct `LambdaPid<M, R>` — the user-visible lambda-actor handle.
    ///
    /// `M` is the message type (single param, a `Tuple` for multi-param, or
    /// `Unit` for a zero-arg actor); `R` is the reply type (`Unit` for a
    /// tell-shaped actor, or the declared `-> R` reply for an ask-shaped one).
    /// Send iff both `M` and `R` are Send (the message and reply cross the
    /// actor boundary). A PID-like handle, distinct from the `Duplex` channel
    /// substrate: it has no `.recv()` / `.send_half()` / `.recv_half()` surface.
    #[must_use]
    pub fn lambda_pid(msg: Ty, reply: Ty) -> Ty {
        Self::builtin_named(BuiltinType::LambdaPid, vec![msg, reply])
    }

    /// Extract `(M, R)` from `LambdaPid<M, R>`, or `None` if not a `LambdaPid`.
    #[must_use]
    pub fn as_lambda_pid(&self) -> Option<(&Ty, &Ty)> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::LambdaPid),
                args,
                ..
            } if args.len() == 2 => Some((&args[0], &args[1])),
            _ => None,
        }
    }

    /// Construct `SendError` — error type for tell-shaped lambda-actor calls.
    #[must_use]
    pub fn send_error() -> Ty {
        Self::builtin_named(BuiltinType::SendError, vec![])
    }

    /// Construct `AskError` — error type for ask-shaped lambda-actor calls.
    #[must_use]
    pub fn ask_error() -> Ty {
        Self::builtin_named(BuiltinType::AskError, vec![])
    }

    /// Construct `TimeoutError` — the error arm of `await rx.recv() | after d`
    /// and `await stream.recv() | after d`.  A unit enum with one variant
    /// (`Timeout`) distinguishing deadline expiry from a closed channel
    /// (`Ok(None)`).
    #[must_use]
    pub fn timeout_error() -> Ty {
        Self::builtin_named(BuiltinType::TimeoutError, vec![])
    }

    /// Construct `RecvError` — error type for `Duplex::recv` / half-recv calls.
    #[must_use]
    pub fn recv_error() -> Ty {
        Self::builtin_named(BuiltinType::RecvError, vec![])
    }

    /// Construct `LinkError` — error type for `link(handle)` calls.
    ///
    /// The concrete enum (`AlreadyLinked`, `TargetDead`) is declared in
    /// `std/link_monitor.hew` and wired in codegen slice B3. At the checker
    /// layer this is a named-type marker, consistent with `SendError`/`RecvError`.
    #[must_use]
    pub fn link_error() -> Ty {
        Self::builtin_named(BuiltinType::LinkError, vec![])
    }

    /// Construct `MonitorRef` — handle returned by `monitor(handle)`.
    ///
    /// The struct is declared in `std/link_monitor.hew` and registered via
    /// `register_builtin_monitor_ref_surface`. At the checker layer this is a
    /// named-type marker, consistent with how `SendError`/`AskError` are encoded.
    #[must_use]
    pub fn monitor_ref() -> Ty {
        Self::builtin_named(BuiltinType::MonitorRef, vec![])
    }

    /// Construct `NarrowError` — error type returned by `.try_to_<W>()` width-conversion
    /// methods when the source value does not fit in the target integer width.
    #[must_use]
    pub fn narrow_error() -> Ty {
        Self::builtin_named(BuiltinType::NarrowError, vec![])
    }

    /// Return the fixed bit-width of this integer type, or `None` for
    /// platform-sized types (`isize`/`usize`) whose width is target-dependent.
    ///
    /// Used by the width-conversion method family to determine admissibility of
    /// infallible `.to_<W>()`: only fixed-width, same-sign, strictly-wider pairs
    /// are permitted without a fallible wrapper.
    #[must_use]
    pub fn integer_bit_width(&self) -> Option<u8> {
        match self {
            Ty::I8 | Ty::U8 => Some(8),
            Ty::I16 | Ty::U16 => Some(16),
            Ty::I32 | Ty::U32 => Some(32),
            Ty::I64 | Ty::U64 => Some(64),
            // isize/usize and non-integer types have no fixed width.
            _ => None,
        }
    }

    /// Construct `CloseError` — error type for `Duplex::close` / half-close calls.
    ///
    /// Distinct from the process-resource `CloseError` registered by the
    /// `Closable` trait (`registration.rs`); this variant names the duplex
    /// close-failure (double-close / already-closed) at the type-checker
    /// surface.  The two share a name by design; slice 6 (stdlib) will
    /// unify them under a single `CloseError` enum.
    #[must_use]
    pub fn duplex_close_error() -> Ty {
        Self::builtin_named(BuiltinType::CloseError, vec![])
    }

    /// Construct `SendHalf<S>` — send-direction half of a split `Duplex<S, R>`.
    ///
    /// Returned by `Duplex<S, R>::send_half()`; consumes the source handle.
    #[must_use]
    pub fn send_half(s: Ty) -> Ty {
        Self::builtin_named(BuiltinType::SendHalf, vec![s])
    }

    /// Construct `RecvHalf<R>` — receive-direction half of a split `Duplex<S, R>`.
    ///
    /// Returned by `Duplex<S, R>::recv_half()`; consumes the source handle.
    #[must_use]
    pub fn recv_half(r: Ty) -> Ty {
        Self::builtin_named(BuiltinType::RecvHalf, vec![r])
    }

    /// Construct `Generator<yields, returns>`.
    #[must_use]
    pub fn generator(yields: Ty, returns: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Generator, vec![yields, returns])
    }

    /// Construct `AsyncGenerator<yields>`.
    #[must_use]
    pub fn async_generator(yields: Ty) -> Ty {
        Self::builtin_named(BuiltinType::AsyncGenerator, vec![yields])
    }

    /// Construct `Range<inner>`.
    #[must_use]
    pub fn range(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Range, vec![inner])
    }

    /// Construct `Rc<inner>`.
    #[must_use]
    pub fn rc(inner: Ty) -> Ty {
        Self::builtin_named(BuiltinType::Rc, vec![inner])
    }

    // -- Accessor helpers: match on Named patterns --

    /// If this is `Option<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_option(&self) -> Option<&Ty> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::Option),
                args,
                ..
            } if args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// If this is `Result<T, E>`, return `Some((&T, &E))`.
    #[must_use]
    pub fn as_result(&self) -> Option<(&Ty, &Ty)> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::Result),
                args,
                ..
            } if args.len() == 2 => Some((&args[0], &args[1])),
            _ => None,
        }
    }

    /// If this is `ActorRef<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_actor_ref(&self) -> Option<&Ty> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::ActorRef),
                args,
                ..
            } if args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// If this is `LocalPid<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_local_pid(&self) -> Option<&Ty> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::LocalPid),
                args,
                ..
            } if args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// If this is `RemotePid<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_remote_pid(&self) -> Option<&Ty> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::RemotePid),
                args,
                ..
            } if args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// If this is a local actor handle (`ActorRef<T>`, `Actor<T>`, or `LocalPid<T>`), return `Some(&T)`.
    ///
    /// `LocalPid<T>` is the default spawn-return type. `ActorRef<T>` remains
    /// recognized for legacy local references. `RemotePid<T>` is intentionally
    /// excluded — it is a distinct type that does not participate in the local
    /// supervisor graph.
    #[must_use]
    pub fn as_actor_handle(&self) -> Option<&Ty> {
        match self {
            Ty::Named {
                builtin: Some(builtin),
                args,
                ..
            } if builtin.has_role(crate::builtin_type::BuiltinTypeRole::ActorDispatchLocal)
                && args.len() == 1 =>
            {
                Some(&args[0])
            }
            _ => None,
        }
    }

    fn as_single_arg_builtin_named(&self, kind: BuiltinNamedType) -> Option<&Ty> {
        match self {
            Ty::Named { builtin, args, .. }
                if builtin_named_type_from_builtin(*builtin) == Some(kind) && args.len() == 1 =>
            {
                Some(&args[0])
            }
            _ => None,
        }
    }

    /// If this is `Stream<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_stream(&self) -> Option<&Ty> {
        self.as_single_arg_builtin_named(BuiltinNamedType::Stream)
    }

    /// If this is `Sink<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_sink(&self) -> Option<&Ty> {
        self.as_single_arg_builtin_named(BuiltinNamedType::Sink)
    }

    /// If this is `Generator<Y, R>`, return `Some((&Y, &R))`.
    #[must_use]
    pub fn as_generator(&self) -> Option<(&Ty, &Ty)> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::Generator),
                args,
                ..
            } if args.len() == 2 => Some((&args[0], &args[1])),
            _ => None,
        }
    }

    /// If this is `AsyncGenerator<Y>`, return `Some(&Y)`.
    #[must_use]
    pub fn as_async_generator(&self) -> Option<&Ty> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::AsyncGenerator),
                args,
                ..
            } if args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// If this is `Range<T>`, return `Some(&T)`.
    #[must_use]
    pub fn as_range(&self) -> Option<&Ty> {
        match self {
            Ty::Named {
                builtin: Some(BuiltinType::Range),
                args,
                ..
            } if args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    /// Canonicalize known named builtins to their shared spelling before
    /// constructing `Ty::Named`.
    #[must_use]
    pub fn named(name: impl Into<String>, args: Vec<Ty>) -> Ty {
        Ty::Named {
            name: name.into(),
            args,
            builtin: None,
        }
    }

    #[must_use]
    pub fn builtin_named(kind: BuiltinType, args: Vec<Ty>) -> Ty {
        Ty::Named {
            name: kind.canonical_name().to_string(),
            args,
            builtin: Some(kind),
        }
    }

    #[must_use]
    pub fn normalize_named(name: String, args: Vec<Ty>) -> Ty {
        let name = match Self::canonical_named_builtin(&name) {
            Some(canonical) if canonical != name => canonical.to_string(),
            _ => name,
        };
        let builtin = lookup_builtin_type(&name);
        if builtin == Some(BuiltinType::CancellationToken) && args.is_empty() {
            return Ty::CancellationToken;
        }
        Ty::Named {
            name,
            args,
            builtin,
        }
    }

    /// Check if this is a numeric type (integer or float).
    #[must_use]
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Check if this is a numeric literal kind awaiting coercion/defaulting.
    #[must_use]
    pub fn is_numeric_literal(&self) -> bool {
        matches!(self, Ty::IntLiteral | Ty::FloatLiteral)
    }

    /// Check if this is an integer literal kind awaiting coercion/defaulting.
    #[must_use]
    pub fn is_integer_literal(&self) -> bool {
        matches!(self, Ty::IntLiteral)
    }

    /// Check if this is a float literal kind awaiting coercion/defaulting.
    #[must_use]
    pub fn is_float_literal(&self) -> bool {
        matches!(self, Ty::FloatLiteral)
    }

    /// Materialize any remaining numeric literal kinds to their canonical
    /// concrete defaults for downstream consumers that require fixed widths.
    #[must_use]
    pub fn materialize_literal_defaults(&self) -> Ty {
        match self {
            Ty::IntLiteral => Ty::I64,
            Ty::FloatLiteral => Ty::F64,
            _ => self.map_children(&|child| child.materialize_literal_defaults()),
        }
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

    /// Check if this is the `instant` type.
    #[must_use]
    pub fn is_instant(&self) -> bool {
        matches!(
            self,
            Ty::Named {
                builtin: Some(BuiltinType::Instant),
                ..
            }
        )
    }

    /// Check if this is an integer type.
    #[must_use]
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Ty::I8
                | Ty::I16
                | Ty::I32
                | Ty::I64
                | Ty::U8
                | Ty::U16
                | Ty::U32
                | Ty::U64
                | Ty::Isize
                | Ty::Usize
                | Ty::IntLiteral
        )
    }

    #[must_use]
    pub(crate) fn hashset_lowering_type_key(&self) -> Option<HashSetLoweringTypeKey> {
        match self {
            Ty::IntLiteral | Ty::I64 => Some(HashSetLoweringTypeKey::I64),
            Ty::U64 => Some(HashSetLoweringTypeKey::U64),
            Ty::String => Some(HashSetLoweringTypeKey::String),
            _ => None,
        }
    }

    /// Check if this is an unsigned integer type.
    #[must_use]
    pub fn is_unsigned(&self) -> bool {
        matches!(self, Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::Usize)
    }

    /// Check if this is a floating-point type.
    #[must_use]
    pub fn is_float(&self) -> bool {
        matches!(self, Ty::F32 | Ty::F64 | Ty::FloatLiteral)
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
        self.apply_subst_inner(subst, &mut HashSet::new())
    }

    fn apply_subst_inner(&self, subst: &Substitution, visited: &mut HashSet<TypeVar>) -> Ty {
        if subst.mappings().is_empty() {
            return self.clone();
        }
        match self {
            Ty::Var(v) => match subst.lookup(*v) {
                Some(resolved) => {
                    if !visited.insert(*v) {
                        return Ty::Error;
                    }
                    let resolved = resolved.apply_subst_inner(subst, visited);
                    visited.remove(v);
                    resolved
                }
                None => self.clone(),
            },
            Ty::Tuple(elems) => Ty::Tuple(
                elems
                    .iter()
                    .map(|elem| elem.apply_subst_inner(subst, visited))
                    .collect(),
            ),
            Ty::Array(elem, size) => {
                Ty::Array(Box::new(elem.apply_subst_inner(subst, visited)), *size)
            }
            Ty::Slice(elem) => Ty::Slice(Box::new(elem.apply_subst_inner(subst, visited))),
            Ty::Named {
                name,
                args,
                builtin,
            } => Ty::Named {
                name: name.clone(),
                builtin: *builtin,
                args: args
                    .iter()
                    .map(|arg| arg.apply_subst_inner(subst, visited))
                    .collect(),
            },
            Ty::Function { params, ret } => Ty::Function {
                params: params
                    .iter()
                    .map(|param| param.apply_subst_inner(subst, visited))
                    .collect(),
                ret: Box::new(ret.apply_subst_inner(subst, visited)),
            },
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params
                    .iter()
                    .map(|param| param.apply_subst_inner(subst, visited))
                    .collect(),
                ret: Box::new(ret.apply_subst_inner(subst, visited)),
                captures: captures
                    .iter()
                    .map(|capture| capture.apply_subst_inner(subst, visited))
                    .collect(),
            },
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(pointee.apply_subst_inner(subst, visited)),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound
                            .args
                            .iter()
                            .map(|arg| arg.apply_subst_inner(subst, visited))
                            .collect(),
                        assoc_bindings: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| (name.clone(), ty.apply_subst_inner(subst, visited)))
                            .collect(),
                    })
                    .collect(),
            },
            Ty::Task(inner) => Ty::Task(Box::new(inner.apply_subst_inner(subst, visited))),
            Ty::AssocType {
                base,
                trait_name,
                assoc_name,
            } => Ty::AssocType {
                base: Box::new(base.apply_subst_inner(subst, visited)),
                trait_name: trait_name.clone(),
                assoc_name: assoc_name.clone(),
            },
            _ => self.clone(),
        }
    }

    /// Public counterpart to `map_children`: apply `f` to each child type
    /// and reconstruct the composite. Intended for cross-module walkers
    /// (e.g. the checker's projection-collapse) that need structural
    /// recursion without re-implementing every variant.
    #[must_use]
    pub fn map_children_pub(&self, f: &impl Fn(&Ty) -> Ty) -> Ty {
        self.map_children(f)
    }

    /// Apply a function to each child type, reconstructing the composite.
    /// Leaf types (primitives, Var, Error) return `self.clone()`.
    fn map_children(&self, f: &impl Fn(&Ty) -> Ty) -> Ty {
        match self {
            Ty::Tuple(elems) => Ty::Tuple(elems.iter().map(f).collect()),
            Ty::Array(elem, size) => Ty::Array(Box::new(f(elem)), *size),
            Ty::Slice(elem) => Ty::Slice(Box::new(f(elem))),
            Ty::Named {
                name,
                args,
                builtin,
            } => Ty::Named {
                name: name.clone(),
                builtin: *builtin,
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
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(f(pointee)),
            },
            Ty::Borrow { pointee } => Ty::Borrow {
                pointee: Box::new(f(pointee)),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound.args.iter().map(f).collect(),
                        assoc_bindings: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| (name.clone(), f(ty)))
                            .collect(),
                    })
                    .collect(),
            },
            Ty::Task(inner) => Ty::Task(Box::new(f(inner))),
            Ty::AssocType {
                base,
                trait_name,
                assoc_name,
            } => Ty::AssocType {
                base: Box::new(f(base)),
                trait_name: trait_name.clone(),
                assoc_name: assoc_name.clone(),
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
            Ty::Pointer { pointee, .. } | Ty::Borrow { pointee } => f(pointee),
            Ty::TraitObject { traits } => traits.iter().any(|bound| {
                bound.args.iter().any(f) || bound.assoc_bindings.iter().any(|(_, ty)| f(ty))
            }),
            Ty::Task(inner) => f(inner),
            Ty::AssocType { base, .. } => f(base),
            _ => false,
        }
    }

    /// Substitute a named type parameter (e.g. `T`) with a concrete type in a type expression.
    /// Used to resolve generic fields/methods on instantiated types.
    #[must_use]
    pub fn substitute_named_param(&self, param_name: &str, replacement: &Ty) -> Ty {
        match self {
            Ty::Named { name, args, .. } if args.is_empty() && name == param_name => {
                replacement.clone()
            }
            Ty::Named {
                name,
                args,
                builtin,
            } => Ty::Named {
                name: name.clone(),
                builtin: *builtin,
                args: args
                    .iter()
                    .map(|a| a.substitute_named_param(param_name, replacement))
                    .collect(),
            },
            Ty::Tuple(elems) => Ty::Tuple(
                elems
                    .iter()
                    .map(|e| e.substitute_named_param(param_name, replacement))
                    .collect(),
            ),
            Ty::Array(inner, n) => Ty::Array(
                Box::new(inner.substitute_named_param(param_name, replacement)),
                *n,
            ),
            Ty::Slice(inner) => Ty::Slice(Box::new(
                inner.substitute_named_param(param_name, replacement),
            )),
            Ty::Function { params, ret } => Ty::Function {
                params: params
                    .iter()
                    .map(|p| p.substitute_named_param(param_name, replacement))
                    .collect(),
                ret: Box::new(ret.substitute_named_param(param_name, replacement)),
            },
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params
                    .iter()
                    .map(|p| p.substitute_named_param(param_name, replacement))
                    .collect(),
                ret: Box::new(ret.substitute_named_param(param_name, replacement)),
                captures: captures
                    .iter()
                    .map(|c| c.substitute_named_param(param_name, replacement))
                    .collect(),
            },
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(pointee.substitute_named_param(param_name, replacement)),
            },
            Ty::Borrow { pointee } => Ty::Borrow {
                pointee: Box::new(pointee.substitute_named_param(param_name, replacement)),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound
                            .args
                            .iter()
                            .map(|arg| arg.substitute_named_param(param_name, replacement))
                            .collect(),
                        assoc_bindings: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| {
                                (
                                    name.clone(),
                                    ty.substitute_named_param(param_name, replacement),
                                )
                            })
                            .collect(),
                    })
                    .collect(),
            },
            // Task<T> is compiler-internal; T itself may reference a named param
            // in a generic context (e.g. inside a function body that is generic
            // over T), so we recurse into the inner type.
            Ty::Task(inner) => Ty::Task(Box::new(
                inner.substitute_named_param(param_name, replacement),
            )),
            // AssocType { base: T, trait, name }: when `T` is the type param
            // being substituted, recurse into `base` so a later projection-
            // collapse pass can resolve the assoc binding from the impl.
            Ty::AssocType {
                base,
                trait_name,
                assoc_name,
            } => Ty::AssocType {
                base: Box::new(base.substitute_named_param(param_name, replacement)),
                trait_name: trait_name.clone(),
                assoc_name: assoc_name.clone(),
            },
            _ => self.clone(),
        }
    }

    /// Substitute all named type parameters simultaneously in a single structural
    /// traversal. Unlike chaining [`substitute_named_param`] calls sequentially,
    /// this never re-substitutes the result of one replacement into another —
    /// so a swap map like `{"A": B, "B": A}` correctly yields `Pair<B,A>` rather
    /// than aliasing both params back to `A`.
    ///
    /// Every `Ty::Named` leaf with empty args is looked up in `map` exactly once.
    /// If found, the replacement is returned as-is (no further substitution into
    /// the replacement). Composites recurse structurally.
    #[must_use]
    pub fn substitute_named_params_parallel(&self, map: &HashMap<String, Ty>) -> Ty {
        match self {
            Ty::Named { name, args, .. } if args.is_empty() => {
                if let Some(replacement) = map.get(name.as_str()) {
                    replacement.clone()
                } else {
                    self.clone()
                }
            }
            Ty::Named {
                name,
                args,
                builtin,
            } => Ty::Named {
                name: name.clone(),
                builtin: *builtin,
                args: args
                    .iter()
                    .map(|a| a.substitute_named_params_parallel(map))
                    .collect(),
            },
            Ty::Tuple(elems) => Ty::Tuple(
                elems
                    .iter()
                    .map(|e| e.substitute_named_params_parallel(map))
                    .collect(),
            ),
            Ty::Array(inner, n) => {
                Ty::Array(Box::new(inner.substitute_named_params_parallel(map)), *n)
            }
            Ty::Slice(inner) => Ty::Slice(Box::new(inner.substitute_named_params_parallel(map))),
            Ty::Function { params, ret } => Ty::Function {
                params: params
                    .iter()
                    .map(|p| p.substitute_named_params_parallel(map))
                    .collect(),
                ret: Box::new(ret.substitute_named_params_parallel(map)),
            },
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params
                    .iter()
                    .map(|p| p.substitute_named_params_parallel(map))
                    .collect(),
                ret: Box::new(ret.substitute_named_params_parallel(map)),
                captures: captures
                    .iter()
                    .map(|c| c.substitute_named_params_parallel(map))
                    .collect(),
            },
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(pointee.substitute_named_params_parallel(map)),
            },
            Ty::Borrow { pointee } => Ty::Borrow {
                pointee: Box::new(pointee.substitute_named_params_parallel(map)),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound
                            .args
                            .iter()
                            .map(|arg| arg.substitute_named_params_parallel(map))
                            .collect(),
                        assoc_bindings: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| {
                                (name.clone(), ty.substitute_named_params_parallel(map))
                            })
                            .collect(),
                    })
                    .collect(),
            },
            Ty::Task(inner) => Ty::Task(Box::new(inner.substitute_named_params_parallel(map))),
            Ty::AssocType {
                base,
                trait_name,
                assoc_name,
            } => Ty::AssocType {
                base: Box::new(base.substitute_named_params_parallel(map)),
                trait_name: trait_name.clone(),
                assoc_name: assoc_name.clone(),
            },
            _ => self.clone(),
        }
    }
}

/// User-facing type wrapper that preserves Hew numeric spellings.
#[derive(Debug)]
pub struct UserFacingTy<'a>(&'a Ty);

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_numeric_names(f, "i64", "f64")
    }
}

impl fmt::Display for UserFacingTy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.materialize_literal_defaults().fmt(f)
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
        assert!(Ty::IntLiteral.is_numeric());
        assert!(Ty::FloatLiteral.is_numeric());
        assert!(!Ty::Bool.is_numeric());
        assert!(!Ty::String.is_numeric());
    }

    #[test]
    fn test_is_copy() {
        assert!(Ty::I32.is_copy());
        assert!(Ty::Bool.is_copy());
        assert!(Ty::IntLiteral.is_copy());
        assert!(!Ty::String.is_copy());
        assert!(Ty::Tuple(vec![Ty::I32, Ty::Bool]).is_copy());
        assert!(!Ty::Tuple(vec![Ty::I32, Ty::String]).is_copy());
    }

    #[test]
    fn test_contains_var() {
        let v = TypeVar::fresh();
        let ty = Ty::Tuple(vec![Ty::I32, Ty::Var(v)]);
        assert!(ty.contains_var(v));
        assert!(!Ty::I32.contains_var(v));
    }

    #[test]
    fn test_substitute() {
        let v = TypeVar::fresh();
        let ty = Ty::Tuple(vec![Ty::Var(v), Ty::I32]);
        let result = ty.substitute(v, &Ty::Bool);
        assert_eq!(result, Ty::Tuple(vec![Ty::Bool, Ty::I32]));
    }

    #[test]
    fn test_substitute_named_param() {
        let ty = Ty::Function {
            params: vec![Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            }],
            ret: Box::new(Ty::Tuple(vec![
                Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                },
                Ty::I32,
            ])),
        };

        let substituted = ty.substitute_named_param("T", &Ty::String);

        assert_eq!(
            substituted,
            Ty::Function {
                params: vec![Ty::String],
                ret: Box::new(Ty::Tuple(vec![Ty::String, Ty::I32])),
            }
        );
    }

    #[test]
    fn test_borrow_contains_var_recurses_through_pointee() {
        // Guards the type-inference-boundary invariant: an unresolved `Ty::Var`
        // inside `&T` must be detected by the occurs/contains check, not skipped.
        let v = TypeVar::fresh();
        let borrow = Ty::Borrow {
            pointee: Box::new(Ty::Var(v)),
        };
        assert!(borrow.contains_var(v));

        let concrete_borrow = Ty::Borrow {
            pointee: Box::new(Ty::String),
        };
        assert!(!concrete_borrow.contains_var(v));
    }

    #[test]
    fn test_borrow_substitute_named_param_recurses_through_pointee() {
        // `&T` with `T := string` must become `&string`.
        let borrow = Ty::Borrow {
            pointee: Box::new(Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            }),
        };

        let substituted = borrow.substitute_named_param("T", &Ty::String);

        assert_eq!(
            substituted,
            Ty::Borrow {
                pointee: Box::new(Ty::String),
            }
        );
    }

    #[test]
    fn test_borrow_substitute_recurses_through_nested_pointee() {
        // `map_children` (via `substitute`) must recurse through a borrow into a
        // nested generic pointee: `&Vec<$v>` with `$v := i32` becomes `&Vec<i32>`.
        let v = TypeVar::fresh();
        let borrow = Ty::Borrow {
            pointee: Box::new(Ty::Named {
                builtin: None,
                name: "Vec".to_string(),
                args: vec![Ty::Var(v)],
            }),
        };

        let result = borrow.substitute(v, &Ty::I32);

        assert_eq!(
            result,
            Ty::Borrow {
                pointee: Box::new(Ty::Named {
                    builtin: None,
                    name: "Vec".to_string(),
                    args: vec![Ty::I32],
                }),
            }
        );
    }

    #[test]
    fn test_has_inference_var() {
        let inferred = Ty::Var(TypeVar::fresh());
        let ty = Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Tuple(vec![Ty::String, inferred.clone()])),
        };

        assert!(ty.has_inference_var());
        assert!(!Ty::result(Ty::I32, Ty::String).has_inference_var());
        assert!(inferred.has_inference_var());
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
            "fn(i32, bool) -> string"
        );
        assert_eq!(
            format!(
                "{}",
                Ty::Named {
                    builtin: None,
                    name: "Vec".to_string(),
                    args: vec![Ty::I32],
                }
            ),
            "Vec<i32>"
        );
    }

    #[test]
    fn test_user_facing_display_uses_explicit_widths() {
        assert_eq!(Ty::I64.user_facing().to_string(), "i64");
        assert_eq!(Ty::F64.user_facing().to_string(), "f64");
    }

    #[test]
    fn test_user_facing_display_materializes_literal_kinds() {
        assert_eq!(Ty::IntLiteral.user_facing().to_string(), "i64");
        assert_eq!(Ty::FloatLiteral.user_facing().to_string(), "f64");
    }

    #[test]
    fn test_canonical_lowering_name_rejects_literal_kinds() {
        assert_eq!(Ty::IntLiteral.canonical_lowering_name(), None);
        assert_eq!(Ty::FloatLiteral.canonical_lowering_name(), None);
    }

    #[test]
    fn test_user_facing_display_formats_nested_types() {
        let ty = Ty::Function {
            params: vec![
                Ty::Named {
                    builtin: None,
                    name: "Vec".to_string(),
                    args: vec![Ty::I64],
                },
                Ty::Tuple(vec![Ty::Bool, Ty::I64]),
            ],
            ret: Box::new(Ty::result(
                Ty::I64,
                Ty::Named {
                    builtin: None,
                    name: "HashMap".to_string(),
                    args: vec![Ty::String, Ty::I64],
                },
            )),
        };

        assert_eq!(
            ty.user_facing().to_string(),
            "fn(Vec<i64>, (bool, i64)) -> Result<i64, HashMap<string, i64>>"
        );
        assert_eq!(
            ty.to_string(),
            "fn(Vec<i64>, (bool, i64)) -> Result<i64, HashMap<string, i64>>"
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
        assert!(Ty::IntLiteral.is_integer());
        assert!(!Ty::F32.is_integer());
        assert!(!Ty::F64.is_integer());
        assert!(!Ty::Bool.is_integer());
    }

    #[test]
    fn test_is_float() {
        assert!(Ty::F32.is_float());
        assert!(Ty::F64.is_float());
        assert!(Ty::FloatLiteral.is_float());
        assert!(!Ty::I32.is_float());
        assert!(!Ty::Bool.is_float());
    }

    #[test]
    fn test_materialize_literal_defaults() {
        let ty = Ty::Tuple(vec![
            Ty::IntLiteral,
            Ty::Named {
                builtin: None,
                name: "Option".to_string(),
                args: vec![Ty::FloatLiteral],
            },
        ]);
        assert_eq!(
            ty.materialize_literal_defaults(),
            Ty::Tuple(vec![
                Ty::I64,
                Ty::Named {
                    builtin: None,
                    name: "Option".to_string(),
                    args: vec![Ty::F64],
                },
            ])
        );
    }

    #[test]
    fn test_display_option() {
        assert_eq!(format!("{}", Ty::option(Ty::I32)), "Option<i32>");
    }

    #[test]
    fn test_display_result() {
        assert_eq!(
            format!("{}", Ty::result(Ty::I32, Ty::String)),
            "Result<i32, string>"
        );
    }

    #[test]
    fn test_display_tuple() {
        assert_eq!(
            format!("{}", Ty::Tuple(vec![Ty::I32, Ty::Bool, Ty::String])),
            "(i32, bool, string)"
        );
    }

    #[test]
    fn test_display_empty_tuple() {
        assert_eq!(format!("{}", Ty::Tuple(vec![])), "()");
    }

    #[test]
    fn names_match_qualified_distinct_modules_never_match() {
        // Two distinct module-qualified names are distinct nominal types —
        // the cross-module identity guarantee under qualified-by-default.
        assert!(!Ty::names_match_qualified("mod_a.Reply", "mod_b.Reply"));
        assert!(!Ty::names_match_qualified("std.fs.Conn", "net.Conn"));
    }

    #[test]
    fn names_match_qualified_same_module_bare_and_qualified_match() {
        // A type's own bare spelling matches its `{module}.{name}` alias (the
        // same def), and a builtin handle matches its qualified carrier name.
        assert!(Ty::names_match_qualified("Reply", "mod_a.Reply"));
        assert!(Ty::names_match_qualified("mod_a.Reply", "Reply"));
        assert!(Ty::names_match_qualified("HashSet", "collections.HashSet"));
        assert!(Ty::names_match_qualified("Reply", "Reply"));
    }

    #[test]
    fn names_match_qualified_different_bare_suffix_never_matches() {
        assert!(!Ty::names_match_qualified("Reply", "Request"));
        assert!(!Ty::names_match_qualified("mod_a.Reply", "mod_a.Request"));
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
        let v = TypeVar::fresh();
        let ty = Ty::Named {
            builtin: None,
            name: "Vec".to_string(),
            args: vec![Ty::Tuple(vec![Ty::Var(v), Ty::Bool])],
        };
        let result = ty.substitute(v, &Ty::String);
        assert_eq!(
            result,
            Ty::Named {
                builtin: None,
                name: "Vec".to_string(),
                args: vec![Ty::Tuple(vec![Ty::String, Ty::Bool])],
            }
        );
    }

    #[test]
    fn test_substitute_no_match() {
        let v1 = TypeVar::fresh();
        let v2 = TypeVar::fresh();
        let ty = Ty::Tuple(vec![Ty::Var(v1), Ty::I32]);
        let result = ty.substitute(v2, &Ty::Bool);
        // v2 is not present, so no substitution happens
        assert_eq!(result, Ty::Tuple(vec![Ty::Var(v1), Ty::I32]));
    }

    #[test]
    fn test_contains_var_in_function() {
        let v = TypeVar::fresh();
        let ty = Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Var(v)),
        };
        assert!(ty.contains_var(v));
    }

    #[test]
    fn test_contains_var_in_option() {
        let v = TypeVar::fresh();
        let ty = Ty::option(Ty::Var(v));
        assert!(ty.contains_var(v));
    }

    #[test]
    fn test_is_copy_array() {
        assert!(Ty::Array(Box::new(Ty::I32), 10).is_copy());
        assert!(!Ty::Array(Box::new(Ty::String), 10).is_copy());
    }

    #[test]
    fn test_substitution_insert_rejects_direct_cycle() {
        let mut subst = Substitution::new();
        let v = TypeVar::fresh();

        let result = subst.insert(v, &Ty::Var(v));

        assert!(matches!(
            result,
            Err(crate::unify::UnifyError::OccursCheck { var, .. }) if var == v
        ));
    }

    #[test]
    fn test_substitution_resolve_returns_error_on_cycle() {
        let mut subst = Substitution::new();
        let v1 = TypeVar::fresh();
        let v2 = TypeVar::fresh();

        subst.mappings.insert(v1, Ty::Var(v2));
        subst.mappings.insert(v2, Ty::Var(v1));

        assert_eq!(subst.resolve(&Ty::Var(v1)), Ty::Error);
    }

    #[test]
    fn test_apply_subst_returns_error_on_cycle() {
        let mut subst = Substitution::new();
        let v1 = TypeVar::fresh();
        let v2 = TypeVar::fresh();

        subst.mappings.insert(v1, Ty::Var(v2));
        subst.mappings.insert(v2, Ty::Var(v1));

        assert_eq!(
            Ty::Tuple(vec![Ty::Var(v1)]).apply_subst(&subst),
            Ty::Tuple(vec![Ty::Error])
        );
    }

    // --- substitute_named_params_parallel ---

    fn named(n: &str) -> Ty {
        Ty::Named {
            builtin: None,
            name: n.to_string(),
            args: vec![],
        }
    }

    fn named_with_args(n: &str, args: Vec<Ty>) -> Ty {
        Ty::Named {
            builtin: None,
            name: n.to_string(),
            args,
        }
    }

    #[test]
    fn swap_map_parallel_does_not_alias_params() {
        // Regression: sequential substitution of {A→i32, B→string} on
        // Named("Pair", [A, B]) produces Pair<i32, string> after first pass and
        // then, if A→i32 was already applied, the second pass over B→string
        // leaves B unchanged — or worse: a swap map {A→B, B→A} aliases both
        // params back to the same type.
        //
        // Parallel substitution must replace all leaves in one pass, so
        // Pair<A, B> under {A→B, B→A} becomes Pair<B, A>, not Pair<A, A>.
        let pair_a_b = named_with_args("Pair", vec![named("A"), named("B")]);
        let swap_map: HashMap<String, Ty> =
            [("A".to_string(), named("B")), ("B".to_string(), named("A"))]
                .into_iter()
                .collect();

        let result = pair_a_b.substitute_named_params_parallel(&swap_map);

        assert_eq!(
            result,
            named_with_args("Pair", vec![named("B"), named("A")]),
            "swap map must produce Pair<B,A>, not Pair<A,A>"
        );
    }

    #[test]
    fn parallel_substitution_with_concrete_types_is_identical_to_sequential() {
        // When no param in the map is also a replacement target for another
        // param (i.e. no permutation), parallel and sequential results are the same.
        let pair_a_b = named_with_args("Pair", vec![named("A"), named("B")]);
        let map: HashMap<String, Ty> = [("A".to_string(), Ty::I64), ("B".to_string(), Ty::String)]
            .into_iter()
            .collect();

        let result = pair_a_b.substitute_named_params_parallel(&map);

        assert_eq!(result, named_with_args("Pair", vec![Ty::I64, Ty::String]),);
    }

    #[test]
    fn parallel_substitution_leaves_unmentioned_params_intact() {
        // Params not in the map pass through unchanged.
        let ty = named_with_args("Triple", vec![named("X"), named("Y"), named("Z")]);
        let map: HashMap<String, Ty> = [("X".to_string(), Ty::Bool)].into_iter().collect();

        let result = ty.substitute_named_params_parallel(&map);

        assert_eq!(
            result,
            named_with_args("Triple", vec![Ty::Bool, named("Y"), named("Z")]),
        );
    }

    #[test]
    fn parallel_substitution_does_not_recurse_into_replacements() {
        // If A→B and the replacement B is itself a named param, the result
        // must stay as B — not chain through a further lookup for B.
        let ty = named("A");
        let map: HashMap<String, Ty> = [("A".to_string(), named("B")), ("B".to_string(), Ty::I64)]
            .into_iter()
            .collect();

        // Parallel: A → lookup("A") = B, done. Sequential would then apply B→i64.
        let result = ty.substitute_named_params_parallel(&map);
        assert_eq!(result, named("B"), "parallel must not chain through B→i64");
    }
}
