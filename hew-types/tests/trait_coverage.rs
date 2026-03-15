//! Coverage tests for the trait registry and marker trait derivation.
//!
//! These tests exercise `TraitRegistry::implements_marker` across all
//! type variants, plus trait/impl registration and method lookup.

use hew_types::traits::{MarkerTrait, MethodSig, TraitDef, TraitRegistry};
use hew_types::ty::{TraitObjectBound, Ty};

// ---------------------------------------------------------------------------
// Helper: shorthand for a Named type with no generic args
// ---------------------------------------------------------------------------
fn named(name: &str) -> Ty {
    Ty::Named {
        name: name.to_string(),
        args: vec![],
    }
}

fn named_with(name: &str, args: Vec<Ty>) -> Ty {
    Ty::Named {
        name: name.to_string(),
        args,
    }
}

// ===========================================================================
// is_copy — primitive types
// ===========================================================================

#[test]
fn copy_all_integer_primitives() {
    let reg = TraitRegistry::new();
    for ty in [
        Ty::I8,
        Ty::I16,
        Ty::I32,
        Ty::I64,
        Ty::U8,
        Ty::U16,
        Ty::U32,
        Ty::U64,
    ] {
        assert!(
            reg.implements_marker(&ty, MarkerTrait::Copy),
            "{ty:?} should be Copy"
        );
    }
}

#[test]
fn copy_bool_char_unit_never_duration() {
    let reg = TraitRegistry::new();
    for ty in [Ty::Bool, Ty::Char, Ty::Unit, Ty::Never, Ty::Duration] {
        assert!(
            reg.implements_marker(&ty, MarkerTrait::Copy),
            "{ty:?} should be Copy"
        );
    }
}

#[test]
fn copy_floats() {
    let reg = TraitRegistry::new();
    assert!(reg.implements_marker(&Ty::F32, MarkerTrait::Copy));
    assert!(reg.implements_marker(&Ty::F64, MarkerTrait::Copy));
}

// ===========================================================================
// is_copy — non-Copy types
// ===========================================================================

#[test]
fn vec_is_not_copy() {
    let reg = TraitRegistry::new();
    assert!(!reg.implements_marker(&named_with("Vec", vec![Ty::I32]), MarkerTrait::Copy));
}

#[test]
fn hashmap_is_not_copy() {
    let reg = TraitRegistry::new();
    let map = named_with("HashMap", vec![Ty::String, Ty::I32]);
    assert!(!reg.implements_marker(&map, MarkerTrait::Copy));
}

// ===========================================================================
// is_copy — struct auto-derivation
// ===========================================================================

#[test]
fn struct_all_copy_fields_is_copy() {
    let mut reg = TraitRegistry::new();
    reg.register_type("Colour".to_string(), vec![Ty::U8, Ty::U8, Ty::U8]);
    assert!(reg.implements_marker(&named("Colour"), MarkerTrait::Copy));
}

#[test]
fn struct_with_string_field_not_copy() {
    let mut reg = TraitRegistry::new();
    reg.register_type("Person".to_string(), vec![Ty::String, Ty::I32]);
    assert!(!reg.implements_marker(&named("Person"), MarkerTrait::Copy));
}

#[test]
fn struct_with_nested_copy_struct_is_copy() {
    let mut reg = TraitRegistry::new();
    reg.register_type("Inner".to_string(), vec![Ty::I32]);
    reg.register_type("Outer".to_string(), vec![named("Inner"), Ty::Bool]);
    assert!(reg.implements_marker(&named("Outer"), MarkerTrait::Copy));
}

#[test]
fn struct_with_nested_non_copy_is_not_copy() {
    let mut reg = TraitRegistry::new();
    reg.register_type("Inner".to_string(), vec![Ty::String]);
    reg.register_type("Outer".to_string(), vec![named("Inner"), Ty::I32]);
    assert!(!reg.implements_marker(&named("Outer"), MarkerTrait::Copy));
}

// ===========================================================================
// is_send
// ===========================================================================

#[test]
fn all_primitives_are_send() {
    let reg = TraitRegistry::new();
    for ty in [
        Ty::I8,
        Ty::I16,
        Ty::I32,
        Ty::I64,
        Ty::U8,
        Ty::U16,
        Ty::U32,
        Ty::U64,
        Ty::F32,
        Ty::F64,
        Ty::Bool,
        Ty::Char,
        Ty::Unit,
        Ty::Never,
        Ty::Duration,
    ] {
        assert!(reg.is_send(&ty), "{ty:?} should be Send");
    }
}

#[test]
fn pointer_is_not_send() {
    let reg = TraitRegistry::new();
    let ptr = Ty::Pointer {
        pointee: Box::new(Ty::I32),
        is_mutable: false,
    };
    assert!(!reg.is_send(&ptr));
}

// ===========================================================================
// is_frozen
// ===========================================================================

#[test]
fn primitives_are_frozen() {
    let reg = TraitRegistry::new();
    assert!(reg.is_frozen(&Ty::I32));
    assert!(reg.is_frozen(&Ty::Bool));
}

#[test]
fn string_is_not_frozen() {
    let reg = TraitRegistry::new();
    assert!(!reg.is_frozen(&Ty::String));
}

#[test]
fn actor_ref_is_frozen() {
    let reg = TraitRegistry::new();
    let aref = Ty::actor_ref(named("MyActor"));
    assert!(reg.is_frozen(&aref));
}

// ===========================================================================
// is_sync
// ===========================================================================

// ===========================================================================
// Float trait edge cases (NaN issues)
// ===========================================================================

#[test]
fn floats_not_eq_ord_hash() {
    let reg = TraitRegistry::new();
    for ty in [Ty::F32, Ty::F64] {
        assert!(
            !reg.implements_marker(&ty, MarkerTrait::Eq),
            "{ty:?} should NOT be Eq"
        );
        assert!(
            !reg.implements_marker(&ty, MarkerTrait::Ord),
            "{ty:?} should NOT be Ord"
        );
        assert!(
            !reg.implements_marker(&ty, MarkerTrait::Hash),
            "{ty:?} should NOT be Hash"
        );
    }
}

// ===========================================================================
// Array type
// ===========================================================================

#[test]
fn array_of_copy_is_copy() {
    let reg = TraitRegistry::new();
    let arr = Ty::Array(Box::new(Ty::I32), 10);
    assert!(reg.implements_marker(&arr, MarkerTrait::Copy));
    assert!(reg.implements_marker(&arr, MarkerTrait::Send));
}

#[test]
fn array_of_string_is_not_copy() {
    let reg = TraitRegistry::new();
    let arr = Ty::Array(Box::new(Ty::String), 5);
    assert!(!reg.implements_marker(&arr, MarkerTrait::Copy));
    // But should still be Send since String is Send
    assert!(reg.implements_marker(&arr, MarkerTrait::Send));
}

// ===========================================================================
// Slice type
// ===========================================================================

#[test]
fn slice_is_never_copy() {
    let reg = TraitRegistry::new();
    let slice = Ty::Slice(Box::new(Ty::I32));
    assert!(
        !reg.implements_marker(&slice, MarkerTrait::Copy),
        "Slices are unsized and should never be Copy"
    );
}

#[test]
fn slice_derives_other_markers_from_element() {
    let reg = TraitRegistry::new();
    let slice = Ty::Slice(Box::new(Ty::I32));
    assert!(reg.implements_marker(&slice, MarkerTrait::Send));
    assert!(reg.implements_marker(&slice, MarkerTrait::Eq));
}

#[test]
fn slice_of_non_send_is_not_send() {
    let reg = TraitRegistry::new();
    let ptr = Ty::Pointer {
        pointee: Box::new(Ty::I32),
        is_mutable: false,
    };
    let slice = Ty::Slice(Box::new(ptr));
    assert!(!reg.implements_marker(&slice, MarkerTrait::Send));
}

// ===========================================================================
// Tuple type
// ===========================================================================

#[test]
fn tuple_with_non_copy_element_is_not_copy() {
    let reg = TraitRegistry::new();
    let tuple = Ty::Tuple(vec![Ty::I32, Ty::String]);
    assert!(!reg.implements_marker(&tuple, MarkerTrait::Copy));
    // But still Send since both I32 and String are Send
    assert!(reg.implements_marker(&tuple, MarkerTrait::Send));
}

// ===========================================================================
// Stream and Sink types
// ===========================================================================

#[test]
fn stream_send_when_element_send() {
    let reg = TraitRegistry::new();
    let stream = named_with("Stream", vec![Ty::I32]);
    assert!(reg.implements_marker(&stream, MarkerTrait::Send));
    assert!(reg.implements_marker(&stream, MarkerTrait::Sync));
}

#[test]
fn stream_not_send_when_element_not_send() {
    let reg = TraitRegistry::new();
    let ptr = Ty::Pointer {
        pointee: Box::new(Ty::I32),
        is_mutable: false,
    };
    let stream = named_with("Stream", vec![ptr]);
    assert!(!reg.implements_marker(&stream, MarkerTrait::Send));
}

#[test]
fn stream_is_not_copy_or_clone() {
    let reg = TraitRegistry::new();
    let stream = named_with("Stream", vec![Ty::I32]);
    assert!(!reg.implements_marker(&stream, MarkerTrait::Copy));
    assert!(!reg.implements_marker(&stream, MarkerTrait::Clone));
}

#[test]
fn sink_send_when_element_send() {
    let reg = TraitRegistry::new();
    let sink = named_with("Sink", vec![Ty::String]);
    assert!(reg.implements_marker(&sink, MarkerTrait::Send));
}

#[test]
fn sink_not_copy() {
    let reg = TraitRegistry::new();
    let sink = named_with("Sink", vec![Ty::I32]);
    assert!(!reg.implements_marker(&sink, MarkerTrait::Copy));
}

// ===========================================================================
// Pointer type
// ===========================================================================

#[test]
fn pointer_is_copy_but_not_send() {
    let reg = TraitRegistry::new();
    let ptr = Ty::Pointer {
        pointee: Box::new(Ty::I32),
        is_mutable: false,
    };
    assert!(reg.implements_marker(&ptr, MarkerTrait::Copy));
    assert!(!reg.implements_marker(&ptr, MarkerTrait::Send));
}

#[test]
fn mutable_pointer_is_copy_not_send() {
    let reg = TraitRegistry::new();
    let ptr = Ty::Pointer {
        pointee: Box::new(Ty::String),
        is_mutable: true,
    };
    assert!(reg.implements_marker(&ptr, MarkerTrait::Copy));
    assert!(!reg.implements_marker(&ptr, MarkerTrait::Send));
    assert!(!reg.implements_marker(&ptr, MarkerTrait::Clone));
}

// ===========================================================================
// Function type
// ===========================================================================

#[test]
fn function_type_traits() {
    let reg = TraitRegistry::new();
    let fn_ty = Ty::Function {
        params: vec![Ty::I32, Ty::String],
        ret: Box::new(Ty::Bool),
    };
    assert!(reg.implements_marker(&fn_ty, MarkerTrait::Send));
    assert!(reg.implements_marker(&fn_ty, MarkerTrait::Sync));
    assert!(reg.implements_marker(&fn_ty, MarkerTrait::Clone));
    assert!(reg.implements_marker(&fn_ty, MarkerTrait::Copy));
    // Functions shouldn't have Eq, Hash, etc.
    assert!(!reg.implements_marker(&fn_ty, MarkerTrait::Eq));
    assert!(!reg.implements_marker(&fn_ty, MarkerTrait::Debug));
}

// ===========================================================================
// Closure type
// ===========================================================================

#[test]
fn closure_is_clone_but_not_copy() {
    let reg = TraitRegistry::new();
    let closure = Ty::Closure {
        params: vec![Ty::I32],
        ret: Box::new(Ty::Bool),
        captures: vec![Ty::I32],
    };
    assert!(reg.implements_marker(&closure, MarkerTrait::Clone));
    assert!(!reg.implements_marker(&closure, MarkerTrait::Copy));
    assert!(!reg.implements_marker(&closure, MarkerTrait::Eq));
}

// ===========================================================================
// Actor type (registered)
// ===========================================================================

#[test]
fn registered_actor_is_send_and_sync() {
    let mut reg = TraitRegistry::new();
    reg.register_actor("ChatRoom".to_string());
    let actor = named("ChatRoom");
    assert!(reg.implements_marker(&actor, MarkerTrait::Send));
    assert!(reg.implements_marker(&actor, MarkerTrait::Sync));
}

// ===========================================================================
// Handle types
// ===========================================================================

#[test]
fn handle_type_is_send_copy_clone_debug() {
    let mut reg = TraitRegistry::new();
    reg.register_handle_type("net.Connection".to_string());
    let conn = named("net.Connection");
    assert!(reg.implements_marker(&conn, MarkerTrait::Send));
    assert!(reg.implements_marker(&conn, MarkerTrait::Sync));
    assert!(reg.implements_marker(&conn, MarkerTrait::Copy));
    assert!(reg.implements_marker(&conn, MarkerTrait::Clone));
    assert!(reg.implements_marker(&conn, MarkerTrait::Debug));
    assert!(!reg.implements_marker(&conn, MarkerTrait::Eq));
}

#[test]
fn handle_type_unqualified_lookup() {
    // Registering "json.Value" should match unqualified "Value" too
    let mut reg = TraitRegistry::new();
    reg.register_handle_type("json.Value".to_string());
    let val = named("Value");
    assert!(reg.implements_marker(&val, MarkerTrait::Copy));
    assert!(reg.implements_marker(&val, MarkerTrait::Send));
}

// ===========================================================================
// Drop types
// ===========================================================================

#[test]
fn drop_type_is_send_clone_debug_drop_not_copy() {
    let mut reg = TraitRegistry::new();
    reg.register_drop_type("http.Request".to_string());
    let req = named("http.Request");
    assert!(reg.implements_marker(&req, MarkerTrait::Send));
    assert!(reg.implements_marker(&req, MarkerTrait::Sync));
    assert!(reg.implements_marker(&req, MarkerTrait::Clone));
    assert!(reg.implements_marker(&req, MarkerTrait::Debug));
    assert!(reg.implements_marker(&req, MarkerTrait::Drop));
    assert!(!reg.implements_marker(&req, MarkerTrait::Copy));
    assert!(!reg.implements_marker(&req, MarkerTrait::Frozen));
}

#[test]
fn drop_type_unqualified_lookup() {
    let mut reg = TraitRegistry::new();
    reg.register_drop_type("http.Request".to_string());
    let req = named("Request");
    assert!(reg.implements_marker(&req, MarkerTrait::Drop));
    assert!(!reg.implements_marker(&req, MarkerTrait::Copy));
}

// ===========================================================================
// Negative impls
// ===========================================================================

#[test]
fn negative_impl_overrides_auto_derivation() {
    let mut reg = TraitRegistry::new();
    // Register a struct with all-Copy fields
    reg.register_type("Token".to_string(), vec![Ty::I32]);
    assert!(reg.implements_marker(&named("Token"), MarkerTrait::Copy));
    // Now add a negative impl
    reg.register_negative_impl("Token".to_string(), MarkerTrait::Copy);
    assert!(!reg.implements_marker(&named("Token"), MarkerTrait::Copy));
}

// ===========================================================================
// Machine type
// ===========================================================================

#[test]
fn machine_type_derives_from_fields() {
    let mut reg = TraitRegistry::new();
    reg.register_type("CounterMachine".to_string(), vec![Ty::I32, Ty::Bool]);
    let machine = Ty::Machine {
        name: "CounterMachine".to_string(),
    };
    assert!(reg.implements_marker(&machine, MarkerTrait::Copy));
    assert!(reg.implements_marker(&machine, MarkerTrait::Send));
}

// ===========================================================================
// TraitObject
// ===========================================================================

#[test]
fn trait_object_checks_super_traits() {
    let mut reg = TraitRegistry::new();
    reg.register_trait(TraitDef {
        name: "Drawable".to_string(),
        type_params: vec![],
        super_traits: vec!["Send".to_string()],
        methods: vec![],
        associated_types: vec![],
    });
    let obj = Ty::TraitObject {
        traits: vec![TraitObjectBound {
            trait_name: "Drawable".to_string(),
            args: vec![],
        }],
    };
    assert!(reg.implements_marker(&obj, MarkerTrait::Send));
    assert!(!reg.implements_marker(&obj, MarkerTrait::Copy));
}

// ===========================================================================
// Trait registration and lookup
// ===========================================================================

#[test]
fn register_and_lookup_trait() {
    let mut reg = TraitRegistry::new();
    reg.register_trait(TraitDef {
        name: "Printable".to_string(),
        type_params: vec!["T".to_string()],
        super_traits: vec![],
        methods: vec![MethodSig {
            name: "print".to_string(),
            params: vec![],
            return_type: Ty::Unit,
            takes_self: true,
            self_mutable: false,
        }],
        associated_types: vec!["Output".to_string()],
    });

    let def = reg
        .lookup_trait("Printable")
        .expect("trait should be found");
    assert_eq!(def.name, "Printable");
    assert_eq!(def.type_params, vec!["T"]);
    assert_eq!(def.methods.len(), 1);
    assert_eq!(def.methods[0].name, "print");
    assert!(def.methods[0].takes_self);
    assert!(!def.methods[0].self_mutable);
    assert_eq!(def.associated_types, vec!["Output"]);
}

// ===========================================================================
// Trait impl registration and method lookup
// ===========================================================================

#[test]
fn register_and_lookup_impl() {
    let mut reg = TraitRegistry::new();
    let methods = vec![
        MethodSig {
            name: "to_string".to_string(),
            params: vec![],
            return_type: Ty::String,
            takes_self: true,
            self_mutable: false,
        },
        MethodSig {
            name: "format".to_string(),
            params: vec![Ty::String],
            return_type: Ty::String,
            takes_self: true,
            self_mutable: false,
        },
    ];
    reg.register_impl("Dog".to_string(), "Display".to_string(), methods);

    let found = reg
        .lookup_impl("Dog", "Display")
        .expect("impl should exist");
    assert_eq!(found.len(), 2);
    assert_eq!(found[0].name, "to_string");
    assert_eq!(found[1].name, "format");
    assert_eq!(found[1].params, vec![Ty::String]);
}

#[test]
fn lookup_impl_wrong_type_returns_none() {
    let mut reg = TraitRegistry::new();
    reg.register_impl(
        "Dog".to_string(),
        "Display".to_string(),
        vec![MethodSig {
            name: "show".to_string(),
            params: vec![],
            return_type: Ty::String,
            takes_self: true,
            self_mutable: false,
        }],
    );
    assert!(reg.lookup_impl("Cat", "Display").is_none());
    assert!(reg.lookup_impl("Dog", "Clone").is_none());
}

// ===========================================================================
// Empty trait
// ===========================================================================

#[test]
fn empty_trait_can_be_registered_and_looked_up() {
    let mut reg = TraitRegistry::new();
    reg.register_trait(TraitDef {
        name: "Marker".to_string(),
        type_params: vec![],
        super_traits: vec![],
        methods: vec![],
        associated_types: vec![],
    });
    let def = reg.lookup_trait("Marker").unwrap();
    assert!(def.methods.is_empty());
    assert!(def.super_traits.is_empty());
    assert!(def.associated_types.is_empty());
}

// ===========================================================================
// Trait inheritance (super_traits)
// ===========================================================================

#[test]
fn trait_with_super_traits() {
    let mut reg = TraitRegistry::new();
    reg.register_trait(TraitDef {
        name: "Sortable".to_string(),
        type_params: vec![],
        super_traits: vec!["Eq".to_string(), "Ord".to_string()],
        methods: vec![MethodSig {
            name: "sort_key".to_string(),
            params: vec![],
            return_type: Ty::I64,
            takes_self: true,
            self_mutable: false,
        }],
        associated_types: vec![],
    });
    let def = reg.lookup_trait("Sortable").unwrap();
    assert_eq!(def.super_traits, vec!["Eq", "Ord"]);
}

// ===========================================================================
// Trait with associated types
// ===========================================================================

#[test]
fn trait_with_associated_types() {
    let mut reg = TraitRegistry::new();
    reg.register_trait(TraitDef {
        name: "Iterator".to_string(),
        type_params: vec![],
        super_traits: vec![],
        methods: vec![MethodSig {
            name: "next".to_string(),
            params: vec![],
            return_type: Ty::option(Ty::I32),
            takes_self: true,
            self_mutable: true,
        }],
        associated_types: vec!["Item".to_string()],
    });
    let def = reg.lookup_trait("Iterator").unwrap();
    assert_eq!(def.associated_types, vec!["Item"]);
    assert!(def.methods[0].self_mutable);
}

// ===========================================================================
// Vec / HashMap — collection-specific behaviour
// ===========================================================================

#[test]
fn vec_of_non_eq_is_not_eq() {
    let reg = TraitRegistry::new();
    // F64 is not Eq (NaN), so Vec<F64> shouldn't be Eq either
    let vec_f64 = named_with("Vec", vec![Ty::F64]);
    assert!(!reg.implements_marker(&vec_f64, MarkerTrait::Eq));
}

// ===========================================================================
// Method signature fields
// ===========================================================================

#[test]
fn method_sig_mutable_self() {
    let mut reg = TraitRegistry::new();
    let methods = vec![MethodSig {
        name: "push".to_string(),
        params: vec![Ty::I32],
        return_type: Ty::Unit,
        takes_self: true,
        self_mutable: true,
    }];
    reg.register_impl("Buffer".to_string(), "Collection".to_string(), methods);
    let found = reg.lookup_impl("Buffer", "Collection").unwrap();
    assert!(found[0].self_mutable);
    assert_eq!(found[0].return_type, Ty::Unit);
}

// ===========================================================================
// ActorRef special handling
// ===========================================================================

#[test]
fn actor_ref_is_copy_clone_debug() {
    let reg = TraitRegistry::new();
    let aref = Ty::actor_ref(named("Logger"));
    assert!(reg.implements_marker(&aref, MarkerTrait::Copy));
    assert!(reg.implements_marker(&aref, MarkerTrait::Clone));
    assert!(reg.implements_marker(&aref, MarkerTrait::Debug));
    assert!(!reg.implements_marker(&aref, MarkerTrait::Eq));
}
