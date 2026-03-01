//! Trait definitions and automatic marker trait derivation.
//!
//! This module handles both built-in marker traits (Send, Frozen, Copy, etc.)
//! and user-defined traits with methods.

use crate::ty::Ty;
use std::collections::{HashMap, HashSet};

/// Built-in marker traits that are automatically derived.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MarkerTrait {
    /// Can cross actor boundaries safely
    Send,
    /// Safe to share references across threads
    Sync,
    /// Deeply immutable (implies Send)
    Frozen,
    /// Implicitly copied on assignment
    Copy,
    /// Has `.clone()` method
    Clone,
    /// Equality comparable
    Eq,
    /// Ordered
    Ord,
    /// Hashable
    Hash,
    /// String formatting via `{}`
    Display,
    /// Debug formatting via `{:?}`
    Debug,
    /// Has destructor
    Drop,
    /// Can be deserialized from bytes
    Decode,
    /// Can be serialized to bytes
    Encode,
}

impl std::fmt::Display for MarkerTrait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MarkerTrait::Send => write!(f, "Send"),
            MarkerTrait::Sync => write!(f, "Sync"),
            MarkerTrait::Frozen => write!(f, "Frozen"),
            MarkerTrait::Copy => write!(f, "Copy"),
            MarkerTrait::Clone => write!(f, "Clone"),
            MarkerTrait::Eq => write!(f, "Eq"),
            MarkerTrait::Ord => write!(f, "Ord"),
            MarkerTrait::Hash => write!(f, "Hash"),
            MarkerTrait::Display => write!(f, "Display"),
            MarkerTrait::Debug => write!(f, "Debug"),
            MarkerTrait::Drop => write!(f, "Drop"),
            MarkerTrait::Decode => write!(f, "Decode"),
            MarkerTrait::Encode => write!(f, "Encode"),
        }
    }
}

/// A method signature in a trait or impl.
#[derive(Debug, Clone)]
pub struct MethodSig {
    /// Method name
    pub name: String,
    /// Parameter types (not including self)
    pub params: Vec<Ty>,
    /// Return type
    pub return_type: Ty,
    /// Whether the method takes self by reference
    pub takes_self: bool,
    /// Whether the method takes self by mutable reference
    pub self_mutable: bool,
}

/// A trait definition.
#[derive(Debug, Clone)]
pub struct TraitDef {
    /// Trait name
    pub name: String,
    /// Type parameters
    pub type_params: Vec<String>,
    /// Super traits this trait extends
    pub super_traits: Vec<String>,
    /// Methods defined by this trait
    pub methods: Vec<MethodSig>,
    /// Associated types
    pub associated_types: Vec<String>,
}

/// Registry of type definitions for trait checking.
///
/// This tracks type definitions and trait implementations to enable
/// automatic derivation of marker traits and method lookup.
#[derive(Debug, Clone, Default)]
pub struct TraitRegistry {
    /// Struct/enum definitions: name → field types
    type_fields: HashMap<String, Vec<Ty>>,
    /// Explicit negative impls: types that DO NOT implement a trait
    negative_impls: HashMap<String, HashSet<MarkerTrait>>,
    /// Trait declarations with methods
    trait_decls: HashMap<String, TraitDef>,
    /// Trait implementations: (`type_name`, `trait_name`) → methods
    trait_impls: HashMap<(String, String), Vec<MethodSig>>,
    /// Actor definitions (actors are always Send)
    actors: HashSet<String>,
}

impl TraitRegistry {
    /// Create a new empty trait registry.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a type definition with its field types.
    pub fn register_type(&mut self, name: String, field_types: Vec<Ty>) {
        self.type_fields.insert(name, field_types);
    }

    /// Register an actor type.
    pub fn register_actor(&mut self, name: String) {
        self.actors.insert(name);
    }

    /// Register a negative impl (type does NOT implement trait).
    pub fn register_negative_impl(&mut self, type_name: String, marker: MarkerTrait) {
        self.negative_impls
            .entry(type_name)
            .or_default()
            .insert(marker);
    }

    /// Register a trait declaration.
    pub fn register_trait(&mut self, def: TraitDef) {
        self.trait_decls.insert(def.name.clone(), def);
    }

    /// Register a trait implementation for a type.
    pub fn register_impl(
        &mut self,
        type_name: String,
        trait_name: String,
        methods: Vec<MethodSig>,
    ) {
        self.trait_impls.insert((type_name, trait_name), methods);
    }

    /// Look up a trait definition.
    #[must_use]
    pub fn lookup_trait(&self, name: &str) -> Option<&TraitDef> {
        self.trait_decls.get(name)
    }

    /// Look up methods from a trait impl.
    #[must_use]
    pub fn lookup_impl(&self, type_name: &str, trait_name: &str) -> Option<&[MethodSig]> {
        self.trait_impls
            .get(&(type_name.to_string(), trait_name.to_string()))
            .map(Vec::as_slice)
    }

    /// Check if a type implements a marker trait (automatic derivation).
    ///
    /// Marker traits are derived automatically based on the structure of the type:
    /// - Primitives implement most marker traits
    /// - Composite types implement a trait if all their components do
    /// - `ActorRef` is always `Send` and `Frozen`
    /// - Negative impls can override automatic derivation
    #[must_use]
    #[expect(
        clippy::too_many_lines,
        reason = "marker derivation covers many Ty variants"
    )]
    pub fn implements_marker(&self, ty: &Ty, marker: MarkerTrait) -> bool {
        match ty {
            // Primitives: always Send, Sync, Frozen, Copy, Clone, Eq, Ord, Hash, Debug
            Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::Bool
            | Ty::Char
            | Ty::Unit
            | Ty::Error
            | Ty::Never => true,

            // Floats: most traits but NOT Eq, Ord, Hash (NaN issues)
            Ty::F32 | Ty::F64 => !matches!(
                marker,
                MarkerTrait::Eq | MarkerTrait::Ord | MarkerTrait::Hash
            ),

            // String: Send + Sync + Clone, but NOT Frozen (mutable), NOT Copy
            Ty::String => matches!(
                marker,
                MarkerTrait::Send
                    | MarkerTrait::Sync
                    | MarkerTrait::Clone
                    | MarkerTrait::Eq
                    | MarkerTrait::Ord
                    | MarkerTrait::Hash
                    | MarkerTrait::Display
                    | MarkerTrait::Debug
            ),

            // ActorRef: always Send + Sync + Frozen + Copy (identity reference)
            Ty::ActorRef(_) => matches!(
                marker,
                MarkerTrait::Send
                    | MarkerTrait::Sync
                    | MarkerTrait::Frozen
                    | MarkerTrait::Copy
                    | MarkerTrait::Clone
                    | MarkerTrait::Debug
            ),

            // Stream<T> and Sink<T>: Send/Sync iff T: Send; NOT Clone, Copy, or Frozen (move-only)
            Ty::Stream(inner) | Ty::Sink(inner) => match marker {
                MarkerTrait::Send | MarkerTrait::Sync => {
                    self.implements_marker(inner, MarkerTrait::Send)
                }
                _ => false,
            },

            // Tuple: marker holds if ALL elements have it
            Ty::Tuple(elems) => elems.iter().all(|e| self.implements_marker(e, marker)),

            // Array/Option/Range: marker holds if element has it
            Ty::Array(inner, _) | Ty::Option(inner) | Ty::Range(inner) => {
                self.implements_marker(inner, marker)
            }

            // Slice: like array but NOT Copy (unsized)
            Ty::Slice(elem) => {
                if marker == MarkerTrait::Copy {
                    false
                } else {
                    self.implements_marker(elem, marker)
                }
            }

            // Result: marker holds if both ok and err have it
            Ty::Result { ok, err } => {
                self.implements_marker(ok, marker) && self.implements_marker(err, marker)
            }

            // Named types: check all fields
            Ty::Named { name, args } => {
                // Check negative impls first
                if let Some(negatives) = self.negative_impls.get(name) {
                    if negatives.contains(&marker) {
                        return false;
                    }
                }
                // Actors are always Send + Sync
                if self.actors.contains(name)
                    && matches!(marker, MarkerTrait::Send | MarkerTrait::Sync)
                {
                    return true;
                }
                // Drop types (e.g. http.Request): Send + Clone + Debug but NOT Copy.
                // They own resources and need move semantics for actor sends.
                if crate::stdlib::is_drop_type(name)
                    || crate::stdlib::is_unqualified_drop_type(name)
                {
                    return matches!(
                        marker,
                        MarkerTrait::Send
                            | MarkerTrait::Sync
                            | MarkerTrait::Clone
                            | MarkerTrait::Debug
                            | MarkerTrait::Drop
                    );
                }
                // Opaque stdlib handle types: Send + Copy + Clone + Debug
                // (they're integer file descriptors or pointers at the ABI level)
                // Check both qualified ("net.Connection") and unqualified ("Connection") forms.
                if crate::stdlib::is_handle_type(name)
                    || crate::stdlib::is_unqualified_handle_type(name)
                {
                    return matches!(
                        marker,
                        MarkerTrait::Send
                            | MarkerTrait::Sync
                            | MarkerTrait::Copy
                            | MarkerTrait::Clone
                            | MarkerTrait::Debug
                    );
                }
                // Built-in generic collections: Send/Clone/Debug if elements are,
                // but NOT Copy or Frozen (heap-allocated, mutable)
                if name == "Vec" || name == "HashMap" {
                    return match marker {
                        MarkerTrait::Copy | MarkerTrait::Frozen => false,
                        _ => args.iter().all(|a| self.implements_marker(a, marker)),
                    };
                }
                // Check if all fields implement the trait
                if let Some(fields) = self.type_fields.get(name) {
                    fields.iter().all(|f| self.implements_marker(f, marker))
                } else {
                    false // Unknown type — conservatively fail
                }
            }

            // Pointers: NOT Send (unless explicitly marked), but Copy
            Ty::Pointer { .. } => marker == MarkerTrait::Copy,

            // Function types: always Send, Sync, Clone, Copy (function pointers)
            Ty::Function { .. } => matches!(
                marker,
                MarkerTrait::Send | MarkerTrait::Sync | MarkerTrait::Clone | MarkerTrait::Copy
            ),

            // Closures: Send/Sync only if all captured types are Send/Sync
            Ty::Closure { captures, .. } => match marker {
                MarkerTrait::Send | MarkerTrait::Sync => {
                    captures.iter().all(|c| self.implements_marker(c, marker))
                }
                MarkerTrait::Clone => true,
                _ => false,
            },

            // Generator/AsyncGenerator/Var: NOT Send by default
            Ty::Generator { .. } | Ty::AsyncGenerator { .. } | Ty::Var(_) => false,

            // Trait objects: check if any of the traits has the bound
            Ty::TraitObject { traits } => traits.iter().any(|bound| {
                if let Some(trait_def) = self.trait_decls.get(&bound.trait_name) {
                    trait_def
                        .super_traits
                        .iter()
                        .any(|s| s == &marker.to_string())
                } else {
                    false
                }
            }),
        }
    }

    /// Check if a type is safe to send across actor boundaries.
    #[must_use]
    pub fn is_send(&self, ty: &Ty) -> bool {
        self.implements_marker(ty, MarkerTrait::Send)
    }

    /// Check if a type is deeply immutable.
    #[must_use]
    pub fn is_frozen(&self, ty: &Ty) -> bool {
        self.implements_marker(ty, MarkerTrait::Frozen)
    }

    /// Check if a type is safe to share references across threads.
    #[must_use]
    pub fn is_sync(&self, ty: &Ty) -> bool {
        self.implements_marker(ty, MarkerTrait::Sync)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitives_are_send() {
        let registry = TraitRegistry::new();
        assert!(registry.is_send(&Ty::I32));
        assert!(registry.is_send(&Ty::Bool));
        assert!(registry.is_send(&Ty::F64));
    }

    #[test]
    fn test_string_is_send() {
        let registry = TraitRegistry::new();
        assert!(registry.is_send(&Ty::String));
    }

    #[test]
    fn test_actor_ref_is_send_and_frozen() {
        let registry = TraitRegistry::new();
        let actor_ref = Ty::ActorRef(Box::new(Ty::Named {
            name: "MyActor".to_string(),
            args: vec![],
        }));
        assert!(registry.is_send(&actor_ref));
        assert!(registry.is_frozen(&actor_ref));
    }

    #[test]
    fn test_tuple_send_if_all_elements_send() {
        let registry = TraitRegistry::new();
        let tuple = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        assert!(registry.is_send(&tuple));
    }

    #[test]
    fn test_named_type_with_fields() {
        let mut registry = TraitRegistry::new();
        registry.register_type("Point".to_string(), vec![Ty::I32, Ty::I32]);
        let point = Ty::Named {
            name: "Point".to_string(),
            args: vec![],
        };
        assert!(registry.is_send(&point));
        assert!(registry.implements_marker(&point, MarkerTrait::Copy));
    }

    #[test]
    fn test_negative_impl() {
        let mut registry = TraitRegistry::new();
        registry.register_type("Handle".to_string(), vec![Ty::I32]);
        registry.register_negative_impl("Handle".to_string(), MarkerTrait::Send);
        let handle = Ty::Named {
            name: "Handle".to_string(),
            args: vec![],
        };
        assert!(!registry.is_send(&handle));
    }

    #[test]
    fn test_floats_not_eq() {
        let registry = TraitRegistry::new();
        assert!(!registry.implements_marker(&Ty::F64, MarkerTrait::Eq));
        assert!(!registry.implements_marker(&Ty::F32, MarkerTrait::Hash));
    }

    #[test]
    fn test_option_derives_from_inner() {
        let registry = TraitRegistry::new();
        assert!(registry.is_send(&Ty::Option(Box::new(Ty::I32))));
        assert!(registry.implements_marker(&Ty::Option(Box::new(Ty::I32)), MarkerTrait::Copy));
    }

    #[test]
    fn test_function_is_send() {
        let registry = TraitRegistry::new();
        let fn_ty = Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
        };
        assert!(registry.is_send(&fn_ty));
    }

    #[test]
    fn test_primitives_are_sync() {
        let registry = TraitRegistry::new();
        assert!(registry.is_sync(&Ty::I32));
        assert!(registry.is_sync(&Ty::Bool));
        assert!(registry.is_sync(&Ty::F64));
        assert!(registry.is_sync(&Ty::String));
    }

    #[test]
    fn test_struct_with_send_fields_is_sync() {
        let mut registry = TraitRegistry::new();
        registry.register_type("Point".to_string(), vec![Ty::I32, Ty::I32]);
        let point = Ty::Named {
            name: "Point".to_string(),
            args: vec![],
        };
        assert!(registry.is_sync(&point));
    }

    #[test]
    fn test_pointer_is_not_sync() {
        let registry = TraitRegistry::new();
        let ptr = Ty::Pointer {
            pointee: Box::new(Ty::I32),
            is_mutable: false,
        };
        assert!(!registry.is_sync(&ptr));
    }

    #[test]
    fn test_actor_ref_is_sync() {
        let registry = TraitRegistry::new();
        let actor_ref = Ty::ActorRef(Box::new(Ty::Named {
            name: "MyActor".to_string(),
            args: vec![],
        }));
        assert!(registry.is_sync(&actor_ref));
    }

    #[test]
    fn test_vec_is_send_when_element_is_send() {
        let registry = TraitRegistry::new();
        let vec_i32 = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::I32],
        };
        assert!(registry.is_send(&vec_i32));
        assert!(registry.is_sync(&vec_i32));
        assert!(registry.implements_marker(&vec_i32, MarkerTrait::Clone));
        assert!(!registry.implements_marker(&vec_i32, MarkerTrait::Copy));
        assert!(!registry.implements_marker(&vec_i32, MarkerTrait::Frozen));
    }

    #[test]
    fn test_hashmap_is_send_when_elements_are_send() {
        let registry = TraitRegistry::new();
        let map = Ty::Named {
            name: "HashMap".to_string(),
            args: vec![Ty::String, Ty::I32],
        };
        assert!(registry.is_send(&map));
        assert!(!registry.implements_marker(&map, MarkerTrait::Copy));
    }

    #[test]
    fn test_closure_with_send_captures_is_send() {
        let registry = TraitRegistry::new();
        let closure = Ty::Closure {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
            captures: vec![Ty::I32, Ty::String],
        };
        assert!(registry.is_send(&closure));
        assert!(registry.is_sync(&closure));
    }

    #[test]
    fn test_closure_with_non_send_capture_is_not_send() {
        let registry = TraitRegistry::new();
        let ptr = Ty::Pointer {
            pointee: Box::new(Ty::I32),
            is_mutable: false,
        };
        let closure = Ty::Closure {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
            captures: vec![Ty::I32, ptr],
        };
        assert!(!registry.is_send(&closure));
    }

    #[test]
    fn test_closure_not_copy() {
        let registry = TraitRegistry::new();
        let closure = Ty::Closure {
            params: vec![],
            ret: Box::new(Ty::Unit),
            captures: vec![Ty::I32],
        };
        assert!(!registry.implements_marker(&closure, MarkerTrait::Copy));
        assert!(registry.implements_marker(&closure, MarkerTrait::Clone));
    }

    #[test]
    fn test_vec_not_send_when_element_not_send() {
        let registry = TraitRegistry::new();
        let ptr = Ty::Pointer {
            pointee: Box::new(Ty::I32),
            is_mutable: false,
        };
        let vec_ptr = Ty::Named {
            name: "Vec".to_string(),
            args: vec![ptr],
        };
        assert!(!registry.is_send(&vec_ptr));
    }
}
