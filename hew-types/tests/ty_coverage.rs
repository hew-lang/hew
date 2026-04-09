//! Coverage tests for `ty.rs`: Display impls, type accessors, predicates,
//! substitution, and the `from_name` canonical mapping.

use hew_types::ty::{Substitution, TraitObjectBound, Ty, TypeVar};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn named(n: &str) -> Ty {
    Ty::Named {
        name: n.to_string(),
        args: vec![],
    }
}

// ===========================================================================
// Display — every Ty variant
// ===========================================================================

#[test]
fn display_all_primitives() {
    assert_eq!(Ty::I8.to_string(), "i8");
    assert_eq!(Ty::I16.to_string(), "i16");
    assert_eq!(Ty::I32.to_string(), "i32");
    assert_eq!(Ty::I64.to_string(), "i64");
    assert_eq!(Ty::U8.to_string(), "u8");
    assert_eq!(Ty::U16.to_string(), "u16");
    assert_eq!(Ty::U32.to_string(), "u32");
    assert_eq!(Ty::U64.to_string(), "u64");
    assert_eq!(Ty::F32.to_string(), "f32");
    assert_eq!(Ty::F64.to_string(), "f64");
    assert_eq!(Ty::Bool.to_string(), "bool");
    assert_eq!(Ty::Char.to_string(), "char");
    assert_eq!(Ty::String.to_string(), "String");
    assert_eq!(Ty::Bytes.to_string(), "bytes");
    assert_eq!(Ty::Duration.to_string(), "duration");
    assert_eq!(Ty::Unit.to_string(), "()");
    assert_eq!(Ty::Never.to_string(), "!");
}

#[test]
fn display_var() {
    let v = TypeVar(42);
    assert_eq!(Ty::Var(v).to_string(), "?T42");
}

#[test]
fn display_tuple_single_element() {
    assert_eq!(Ty::Tuple(vec![Ty::Bool]).to_string(), "(bool)");
}

#[test]
fn display_array() {
    assert_eq!(Ty::Array(Box::new(Ty::I32), 5).to_string(), "[i32; 5]");
}

#[test]
fn display_slice() {
    assert_eq!(Ty::Slice(Box::new(Ty::U8)).to_string(), "[u8]");
}

#[test]
fn display_named_no_args() {
    assert_eq!(named("Foo").to_string(), "Foo");
}

#[test]
fn display_named_multiple_args() {
    let ty = Ty::Named {
        name: "HashMap".to_string(),
        args: vec![Ty::String, Ty::I64],
    };
    assert_eq!(ty.to_string(), "HashMap<String, i64>");
}

#[test]
fn display_function_no_params() {
    let ty = Ty::Function {
        params: vec![],
        ret: Box::new(Ty::Unit),
    };
    assert_eq!(ty.to_string(), "fn() -> ()");
}

#[test]
fn display_function_multiple_params() {
    let ty = Ty::Function {
        params: vec![Ty::I32, Ty::Bool, Ty::String],
        ret: Box::new(Ty::F64),
    };
    assert_eq!(ty.to_string(), "fn(i32, bool, String) -> f64");
}

#[test]
fn display_closure() {
    // Closure display is identical to Function display (captures are hidden)
    let ty = Ty::Closure {
        params: vec![Ty::I32],
        ret: Box::new(Ty::Bool),
        captures: vec![Ty::String],
    };
    assert_eq!(ty.to_string(), "fn(i32) -> bool");
}

#[test]
fn display_pointer_const() {
    let ty = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::U8),
    };
    assert_eq!(ty.to_string(), "*const u8");
}

#[test]
fn display_pointer_mut() {
    let ty = Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::I32),
    };
    assert_eq!(ty.to_string(), "*mut i32");
}

#[test]
fn display_trait_object_single_no_args() {
    let ty = Ty::TraitObject {
        traits: vec![TraitObjectBound {
            trait_name: "Display".to_string(),
            args: vec![],
        }],
    };
    assert_eq!(ty.to_string(), "dyn Display");
}

#[test]
fn display_trait_object_single_with_args() {
    let ty = Ty::TraitObject {
        traits: vec![TraitObjectBound {
            trait_name: "Iterator".to_string(),
            args: vec![Ty::I32],
        }],
    };
    assert_eq!(ty.to_string(), "dyn Iterator<i32>");
}

#[test]
fn display_trait_object_single_with_multiple_args() {
    let ty = Ty::TraitObject {
        traits: vec![TraitObjectBound {
            trait_name: "Converter".to_string(),
            args: vec![Ty::I32, Ty::String],
        }],
    };
    assert_eq!(ty.to_string(), "dyn Converter<i32, String>");
}

#[test]
fn display_trait_object_multi_trait() {
    let ty = Ty::TraitObject {
        traits: vec![
            TraitObjectBound {
                trait_name: "Display".to_string(),
                args: vec![],
            },
            TraitObjectBound {
                trait_name: "Debug".to_string(),
                args: vec![],
            },
        ],
    };
    assert_eq!(ty.to_string(), "dyn (Display + Debug)");
}

#[test]
fn display_trait_object_multi_trait_with_args() {
    let ty = Ty::TraitObject {
        traits: vec![
            TraitObjectBound {
                trait_name: "Into".to_string(),
                args: vec![Ty::String],
            },
            TraitObjectBound {
                trait_name: "Clone".to_string(),
                args: vec![],
            },
        ],
    };
    assert_eq!(ty.to_string(), "dyn (Into<String> + Clone)");
}

#[test]
fn display_machine() {
    let ty = Ty::Named {
        name: "MyMachine".to_string(),
        args: vec![],
    };
    assert_eq!(ty.to_string(), "MyMachine");
}

#[test]
fn display_error() {
    assert_eq!(Ty::Error.to_string(), "<error>");
}

// ===========================================================================
// TypeVar::Display
// ===========================================================================

#[test]
fn typevar_display() {
    assert_eq!(TypeVar(0).to_string(), "?T0");
    assert_eq!(TypeVar(999).to_string(), "?T999");
}

// ===========================================================================
// from_name — canonical primitive mapping
// ===========================================================================

#[test]
fn from_name_all_aliases() {
    // Signed integers
    assert_eq!(Ty::from_name("i8"), Some(Ty::I8));
    assert_eq!(Ty::from_name("i16"), Some(Ty::I16));
    assert_eq!(Ty::from_name("i32"), Some(Ty::I32));
    assert_eq!(Ty::from_name("i64"), Some(Ty::I64));
    assert_eq!(Ty::from_name("int"), Some(Ty::I64));
    assert_eq!(Ty::from_name("Int"), Some(Ty::I64));
    assert_eq!(Ty::from_name("isize"), Some(Ty::I64));

    // Unsigned integers
    assert_eq!(Ty::from_name("u8"), Some(Ty::U8));
    assert_eq!(Ty::from_name("byte"), Some(Ty::U8));
    assert_eq!(Ty::from_name("u16"), Some(Ty::U16));
    assert_eq!(Ty::from_name("u32"), Some(Ty::U32));
    assert_eq!(Ty::from_name("u64"), Some(Ty::U64));
    assert_eq!(Ty::from_name("uint"), Some(Ty::U64));
    assert_eq!(Ty::from_name("usize"), Some(Ty::U64));

    // Floats
    assert_eq!(Ty::from_name("f32"), Some(Ty::F32));
    assert_eq!(Ty::from_name("f64"), Some(Ty::F64));
    assert_eq!(Ty::from_name("float"), Some(Ty::F64));
    assert_eq!(Ty::from_name("Float"), Some(Ty::F64));

    // Other primitives
    assert_eq!(Ty::from_name("bool"), Some(Ty::Bool));
    assert_eq!(Ty::from_name("Bool"), Some(Ty::Bool));
    assert_eq!(Ty::from_name("char"), Some(Ty::Char));
    assert_eq!(Ty::from_name("Char"), Some(Ty::Char));
    assert_eq!(Ty::from_name("string"), Some(Ty::String));
    assert_eq!(Ty::from_name("String"), Some(Ty::String));
    assert_eq!(Ty::from_name("str"), Some(Ty::String));
    assert_eq!(Ty::from_name("bytes"), Some(Ty::Bytes));
    assert_eq!(Ty::from_name("Bytes"), Some(Ty::Bytes));
    assert_eq!(Ty::from_name("duration"), Some(Ty::Duration));
    assert_eq!(Ty::from_name("Duration"), Some(Ty::Duration));
    assert_eq!(Ty::from_name("()"), Some(Ty::Unit));
    assert_eq!(Ty::from_name("!"), Some(Ty::Never));
}

#[test]
fn from_name_unknown_returns_none() {
    assert_eq!(Ty::from_name("Vec"), None);
    assert_eq!(Ty::from_name("HashMap"), None);
    assert_eq!(Ty::from_name("NonExistent"), None);
    assert_eq!(Ty::from_name(""), None);
}

#[test]
fn canonical_lowering_name_normalizes_internal_spellings() {
    let cases = [
        (Ty::I64, Some("i64")),
        (Ty::U64, Some("u64")),
        (Ty::F64, Some("f64")),
        (Ty::Bool, Some("bool")),
        (Ty::Char, Some("char")),
        (Ty::String, Some("string")),
        (Ty::Bytes, Some("bytes")),
        (Ty::Duration, Some("duration")),
        (Ty::Never, Some("!")),
        (named("Vec"), None),
    ];

    for (ty, expected) in cases {
        assert_eq!(
            ty.canonical_lowering_name(),
            expected,
            "unexpected lowering for {ty:?}"
        );
    }
}

#[test]
fn canonical_lowering_name_round_trips_through_from_name() {
    let primitives = [
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
        Ty::String,
        Ty::Bytes,
        Ty::Duration,
        Ty::Never,
    ];

    for ty in primitives {
        let name = ty.canonical_lowering_name().unwrap();
        assert_eq!(
            Ty::from_name(name),
            Some(ty),
            "round-trip failed for `{name}`"
        );
    }
}

// ===========================================================================
// Constructor + accessor round-trips
// ===========================================================================

#[test]
fn option_constructor_and_accessor() {
    let ty = Ty::option(Ty::I32);
    assert_eq!(ty.as_option(), Some(&Ty::I32));
    // Non-option returns None
    assert_eq!(Ty::I32.as_option(), None);
}

#[test]
fn result_constructor_and_accessor() {
    let ty = Ty::result(Ty::I32, Ty::String);
    let (ok, err) = ty.as_result().unwrap();
    assert_eq!(ok, &Ty::I32);
    assert_eq!(err, &Ty::String);
    assert_eq!(Ty::I32.as_result(), None);
}

#[test]
fn actor_ref_constructor_and_accessor() {
    let ty = Ty::actor_ref(Ty::String);
    assert_eq!(ty.as_actor_ref(), Some(&Ty::String));
    assert_eq!(Ty::I32.as_actor_ref(), None);
}

#[test]
fn actor_handle_accessor() {
    // ActorRef<T> is an actor handle
    let ty = Ty::actor_ref(Ty::I32);
    assert_eq!(ty.as_actor_handle(), Some(&Ty::I32));

    // Actor<T> is also an actor handle
    let actor = Ty::Named {
        name: "Actor".to_string(),
        args: vec![Ty::Bool],
    };
    assert_eq!(actor.as_actor_handle(), Some(&Ty::Bool));

    // Non-actor returns None
    assert_eq!(Ty::I32.as_actor_handle(), None);
    assert_eq!(named("Other").as_actor_handle(), None);
}

#[test]
fn stream_constructor_and_accessor() {
    let ty = Ty::stream(Ty::I32);
    assert_eq!(ty.as_stream(), Some(&Ty::I32));
    assert!(ty.is_stream());
    assert!(!Ty::I32.is_stream());
    assert_eq!(Ty::I32.as_stream(), None);
}

#[test]
fn sink_constructor_and_accessor() {
    let ty = Ty::sink(Ty::String);
    assert_eq!(ty.as_sink(), Some(&Ty::String));
    assert!(ty.is_sink());
    assert!(!Ty::I32.is_sink());
    assert_eq!(Ty::I32.as_sink(), None);
}

#[test]
fn generator_constructor_and_accessor() {
    let ty = Ty::generator(Ty::I32, Ty::Bool);
    let (yields, returns) = ty.as_generator().unwrap();
    assert_eq!(yields, &Ty::I32);
    assert_eq!(returns, &Ty::Bool);
    assert_eq!(Ty::I32.as_generator(), None);
}

#[test]
fn async_generator_constructor_and_accessor() {
    let ty = Ty::async_generator(Ty::String);
    assert_eq!(ty.as_async_generator(), Some(&Ty::String));
    assert_eq!(Ty::I32.as_async_generator(), None);
}

#[test]
fn range_constructor_and_accessor() {
    let ty = Ty::range(Ty::I64);
    assert_eq!(ty.as_range(), Some(&Ty::I64));
    assert_eq!(Ty::I32.as_range(), None);
}

// ===========================================================================
// Accessors — negative cases with wrong arity
// ===========================================================================

#[test]
fn accessor_wrong_arity_returns_none() {
    // Option with wrong number of args
    let bad_option = Ty::Named {
        name: "Option".to_string(),
        args: vec![Ty::I32, Ty::Bool],
    };
    assert_eq!(bad_option.as_option(), None);

    // Result with wrong arity
    let bad_result = Ty::Named {
        name: "Result".to_string(),
        args: vec![Ty::I32],
    };
    assert_eq!(bad_result.as_result(), None);

    // Generator with wrong arity
    let bad_gen = Ty::Named {
        name: "Generator".to_string(),
        args: vec![Ty::I32],
    };
    assert_eq!(bad_gen.as_generator(), None);

    // AsyncGenerator with wrong arity
    let bad_async_gen = Ty::Named {
        name: "AsyncGenerator".to_string(),
        args: vec![Ty::I32, Ty::Bool],
    };
    assert_eq!(bad_async_gen.as_async_generator(), None);

    // ActorRef with wrong arity
    let bad_actor = Ty::Named {
        name: "ActorRef".to_string(),
        args: vec![Ty::I32, Ty::Bool],
    };
    assert_eq!(bad_actor.as_actor_ref(), None);
    assert_eq!(bad_actor.as_actor_handle(), None);

    // Stream/Sink with wrong arity
    let bad_stream = Ty::Named {
        name: "Stream".to_string(),
        args: vec![],
    };
    assert_eq!(bad_stream.as_stream(), None);
    let bad_sink = Ty::Named {
        name: "Sink".to_string(),
        args: vec![Ty::I32, Ty::Bool],
    };
    assert_eq!(bad_sink.as_sink(), None);

    // Range with wrong arity
    let bad_range = Ty::Named {
        name: "Range".to_string(),
        args: vec![],
    };
    assert_eq!(bad_range.as_range(), None);
}

// ===========================================================================
// normalize_named
// ===========================================================================

#[test]
fn normalize_named_produces_named() {
    let ty = Ty::normalize_named("Foo".to_string(), vec![Ty::I32]);
    assert_eq!(
        ty,
        Ty::Named {
            name: "Foo".to_string(),
            args: vec![Ty::I32],
        }
    );
}

#[test]
fn normalize_named_canonicalizes_builtin_spellings() {
    assert_eq!(
        Ty::normalize_named("stream.Stream".to_string(), vec![Ty::Bytes]),
        Ty::stream(Ty::Bytes)
    );
    assert_eq!(
        Ty::normalize_named("channel.Sender".to_string(), vec![Ty::I32]),
        Ty::sender(Ty::I32)
    );
    assert_eq!(
        Ty::normalize_named("channel.Receiver".to_string(), vec![Ty::String]),
        Ty::receiver(Ty::String)
    );
    assert_eq!(
        Ty::normalize_named("stream.Sink".to_string(), vec![Ty::String]),
        Ty::sink(Ty::String)
    );
}

// ===========================================================================
// Predicates — is_bytes, is_duration, is_primitive, is_numeric
// ===========================================================================

#[test]
fn is_bytes_predicate() {
    assert!(Ty::Bytes.is_bytes());
    assert!(!Ty::String.is_bytes());
    assert!(!Ty::I32.is_bytes());
}

#[test]
fn is_duration_predicate() {
    assert!(Ty::Duration.is_duration());
    assert!(!Ty::I64.is_duration());
    assert!(!Ty::String.is_duration());
}

#[test]
fn is_primitive_covers_all_primitive_types() {
    // All integers are primitive
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
        assert!(ty.is_primitive(), "{ty} should be primitive");
    }
    // All floats are primitive
    assert!(Ty::F32.is_primitive());
    assert!(Ty::F64.is_primitive());
    // Bool, Char, Unit, Duration are primitive
    assert!(Ty::Bool.is_primitive());
    assert!(Ty::Char.is_primitive());
    assert!(Ty::Unit.is_primitive());
    assert!(Ty::Duration.is_primitive());
    // Non-primitives
    assert!(!Ty::String.is_primitive());
    assert!(!Ty::Bytes.is_primitive());
    assert!(!Ty::Never.is_primitive());
    assert!(!Ty::Error.is_primitive());
}

#[test]
fn is_numeric_covers_all_numeric_types() {
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
    ] {
        assert!(ty.is_numeric(), "{ty} should be numeric");
    }
    assert!(!Ty::Bool.is_numeric());
    assert!(!Ty::String.is_numeric());
    assert!(!Ty::Duration.is_numeric());
}

// ===========================================================================
// is_copy — extended cases
// ===========================================================================

#[test]
fn is_copy_never_and_pointer() {
    assert!(Ty::Never.is_copy());
    assert!(Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::I32),
    }
    .is_copy());
    assert!(Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::String),
    }
    .is_copy());
}

#[test]
fn is_copy_nested_tuple() {
    // Tuple of copy types is copy
    assert!(Ty::Tuple(vec![Ty::I32, Ty::Tuple(vec![Ty::Bool, Ty::F64])]).is_copy());
    // Tuple containing non-copy type is not copy
    assert!(!Ty::Tuple(vec![Ty::I32, Ty::Tuple(vec![Ty::String])]).is_copy());
}

#[test]
fn is_copy_non_copy_types() {
    assert!(!Ty::String.is_copy());
    assert!(!Ty::Bytes.is_copy());
    assert!(!named("Vec").is_copy());
    assert!(!Ty::Slice(Box::new(Ty::I32)).is_copy());
    assert!(!Ty::Error.is_copy());
    assert!(!Ty::Function {
        params: vec![],
        ret: Box::new(Ty::Unit),
    }
    .is_copy());
}

// ===========================================================================
// contains_var — through all composite types
// ===========================================================================

#[test]
fn contains_var_in_slice() {
    let v = TypeVar(5000);
    let ty = Ty::Slice(Box::new(Ty::Var(v)));
    assert!(ty.contains_var(v));
    assert!(!ty.contains_var(TypeVar(5001)));
}

#[test]
fn contains_var_in_array() {
    let v = TypeVar(5010);
    let ty = Ty::Array(Box::new(Ty::Var(v)), 3);
    assert!(ty.contains_var(v));
    assert!(!Ty::Array(Box::new(Ty::I32), 3).contains_var(v));
}

#[test]
fn contains_var_in_closure() {
    let v = TypeVar(5020);

    // In params
    let ty = Ty::Closure {
        params: vec![Ty::Var(v)],
        ret: Box::new(Ty::I32),
        captures: vec![],
    };
    assert!(ty.contains_var(v));

    // In ret
    let ty = Ty::Closure {
        params: vec![Ty::I32],
        ret: Box::new(Ty::Var(v)),
        captures: vec![],
    };
    assert!(ty.contains_var(v));

    // In captures
    let ty = Ty::Closure {
        params: vec![],
        ret: Box::new(Ty::I32),
        captures: vec![Ty::Var(v)],
    };
    assert!(ty.contains_var(v));

    // Not present
    let ty = Ty::Closure {
        params: vec![Ty::I32],
        ret: Box::new(Ty::Bool),
        captures: vec![Ty::String],
    };
    assert!(!ty.contains_var(v));
}

#[test]
fn contains_var_in_pointer() {
    let v = TypeVar(5030);
    let ty = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::Var(v)),
    };
    assert!(ty.contains_var(v));
    assert!(!Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::I32),
    }
    .contains_var(v));
}

#[test]
fn contains_var_in_trait_object() {
    let v = TypeVar(5040);
    let ty = Ty::TraitObject {
        traits: vec![TraitObjectBound {
            trait_name: "Foo".to_string(),
            args: vec![Ty::Var(v)],
        }],
    };
    assert!(ty.contains_var(v));

    let ty_no_var = Ty::TraitObject {
        traits: vec![TraitObjectBound {
            trait_name: "Foo".to_string(),
            args: vec![Ty::I32],
        }],
    };
    assert!(!ty_no_var.contains_var(v));
}

#[test]
fn contains_var_in_named_machine_and_error() {
    let v = TypeVar(5050);
    assert!(!Ty::Named {
        name: "M".to_string(),
        args: vec![],
    }
    .contains_var(v));
    assert!(!Ty::Error.contains_var(v));
}

// ===========================================================================
// substitute — through all composite types
// ===========================================================================

#[test]
fn substitute_in_slice() {
    let v = TypeVar(6000);
    let ty = Ty::Slice(Box::new(Ty::Var(v)));
    let result = ty.substitute(v, &Ty::U8);
    assert_eq!(result, Ty::Slice(Box::new(Ty::U8)));
}

#[test]
fn substitute_in_array() {
    let v = TypeVar(6010);
    let ty = Ty::Array(Box::new(Ty::Var(v)), 10);
    let result = ty.substitute(v, &Ty::F64);
    assert_eq!(result, Ty::Array(Box::new(Ty::F64), 10));
}

#[test]
fn substitute_in_closure() {
    let v = TypeVar(6020);
    let ty = Ty::Closure {
        params: vec![Ty::Var(v)],
        ret: Box::new(Ty::Var(v)),
        captures: vec![Ty::Var(v)],
    };
    let result = ty.substitute(v, &Ty::Bool);
    assert_eq!(
        result,
        Ty::Closure {
            params: vec![Ty::Bool],
            ret: Box::new(Ty::Bool),
            captures: vec![Ty::Bool],
        }
    );
}

#[test]
fn substitute_in_pointer() {
    let v = TypeVar(6030);
    let ty = Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::Var(v)),
    };
    let result = ty.substitute(v, &Ty::I64);
    assert_eq!(
        result,
        Ty::Pointer {
            is_mutable: true,
            pointee: Box::new(Ty::I64),
        }
    );
}

#[test]
fn substitute_in_trait_object() {
    let v = TypeVar(6040);
    let ty = Ty::TraitObject {
        traits: vec![
            TraitObjectBound {
                trait_name: "Foo".to_string(),
                args: vec![Ty::Var(v)],
            },
            TraitObjectBound {
                trait_name: "Bar".to_string(),
                args: vec![Ty::I32, Ty::Var(v)],
            },
        ],
    };
    let result = ty.substitute(v, &Ty::String);
    assert_eq!(
        result,
        Ty::TraitObject {
            traits: vec![
                TraitObjectBound {
                    trait_name: "Foo".to_string(),
                    args: vec![Ty::String],
                },
                TraitObjectBound {
                    trait_name: "Bar".to_string(),
                    args: vec![Ty::I32, Ty::String],
                },
            ],
        }
    );
}

#[test]
fn substitute_in_function() {
    let v = TypeVar(6050);
    let ty = Ty::Function {
        params: vec![Ty::Var(v), Ty::I32],
        ret: Box::new(Ty::Var(v)),
    };
    let result = ty.substitute(v, &Ty::Bool);
    assert_eq!(
        result,
        Ty::Function {
            params: vec![Ty::Bool, Ty::I32],
            ret: Box::new(Ty::Bool),
        }
    );
}

#[test]
fn substitute_in_named_machine_is_identity() {
    let v = TypeVar(6060);
    let ty = Ty::Named {
        name: "SM".to_string(),
        args: vec![],
    };
    let result = ty.substitute(v, &Ty::I32);
    assert_eq!(
        result,
        Ty::Named {
            name: "SM".to_string(),
            args: vec![],
        }
    );
}

#[test]
fn substitute_var_direct_match() {
    let v = TypeVar(6070);
    let ty = Ty::Var(v);
    assert_eq!(ty.substitute(v, &Ty::String), Ty::String);
}

#[test]
fn substitute_var_no_match() {
    let v1 = TypeVar(6080);
    let v2 = TypeVar(6081);
    let ty = Ty::Var(v1);
    // Substituting v2 in a Var(v1) does nothing — goes through map_children
    assert_eq!(ty.substitute(v2, &Ty::String), Ty::Var(v1));
}

// ===========================================================================
// apply_subst — full substitution application
// ===========================================================================

#[test]
fn apply_subst_empty_is_identity() {
    let subst = Substitution::new();
    let ty = Ty::Tuple(vec![Ty::I32, Ty::String]);
    assert_eq!(ty.apply_subst(&subst), ty);
}

#[test]
fn apply_subst_resolves_var() {
    let v = TypeVar(7000);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::Bool);

    assert_eq!(Ty::Var(v).apply_subst(&subst), Ty::Bool);
}

#[test]
fn apply_subst_chains_through_vars() {
    let v1 = TypeVar(7010);
    let v2 = TypeVar(7011);
    let mut subst = Substitution::new();
    subst.insert(v1, Ty::Var(v2));
    subst.insert(v2, Ty::I64);

    // v1 -> v2 -> I64
    assert_eq!(Ty::Var(v1).apply_subst(&subst), Ty::I64);
}

#[test]
fn apply_subst_through_composite() {
    let v = TypeVar(7020);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::String);

    let ty = Ty::Tuple(vec![Ty::Var(v), Ty::I32]);
    assert_eq!(ty.apply_subst(&subst), Ty::Tuple(vec![Ty::String, Ty::I32]));
}

#[test]
fn apply_subst_unresolved_var_stays() {
    let v = TypeVar(7030);
    let subst = Substitution::new();
    assert_eq!(Ty::Var(v).apply_subst(&subst), Ty::Var(v));
}

// ===========================================================================
// Substitution — resolve, snapshot, restore
// ===========================================================================

#[test]
fn substitution_resolve_walks_chain() {
    let v1 = TypeVar(8000);
    let v2 = TypeVar(8001);
    let v3 = TypeVar(8002);
    let mut subst = Substitution::new();
    subst.insert(v1, Ty::Var(v2));
    subst.insert(v2, Ty::Var(v3));
    subst.insert(v3, Ty::I32);

    assert_eq!(subst.resolve(&Ty::Var(v1)), Ty::I32);
}

#[test]
fn substitution_resolve_non_var() {
    let v = TypeVar(8010);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::Bool);

    // Resolving a non-var applies subst to children
    let ty = Ty::Tuple(vec![Ty::Var(v), Ty::I32]);
    assert_eq!(subst.resolve(&ty), Ty::Tuple(vec![Ty::Bool, Ty::I32]));
}

#[test]
fn substitution_snapshot_and_restore() {
    let v = TypeVar(8020);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::I32);

    let snap = subst.snapshot();
    assert_eq!(subst.lookup(v), Some(&Ty::I32));

    // Modify
    subst.insert(v, Ty::Bool);
    assert_eq!(subst.lookup(v), Some(&Ty::Bool));

    // Restore
    subst.restore(snap);
    assert_eq!(subst.lookup(v), Some(&Ty::I32));
}

#[test]
fn substitution_lookup_missing() {
    let subst = Substitution::new();
    assert_eq!(subst.lookup(TypeVar(9999)), None);
}

#[test]
fn substitution_mappings_returns_all() {
    let v1 = TypeVar(8030);
    let v2 = TypeVar(8031);
    let mut subst = Substitution::new();
    subst.insert(v1, Ty::I32);
    subst.insert(v2, Ty::Bool);

    let m = subst.mappings();
    assert_eq!(m.len(), 2);
    assert_eq!(m.get(&v1), Some(&Ty::I32));
    assert_eq!(m.get(&v2), Some(&Ty::Bool));
}

// ===========================================================================
// Type equality (derived PartialEq)
// ===========================================================================

#[test]
fn ty_equality_same_variant() {
    assert_eq!(Ty::I32, Ty::I32);
    assert_eq!(Ty::String, Ty::String);
    assert_eq!(Ty::Error, Ty::Error);
    assert_eq!(Ty::Never, Ty::Never);
}

#[test]
fn ty_inequality_different_variants() {
    assert_ne!(Ty::I32, Ty::I64);
    assert_ne!(Ty::String, Ty::Bytes);
    assert_ne!(Ty::Bool, Ty::Unit);
}

#[test]
fn ty_equality_composites() {
    assert_eq!(Ty::option(Ty::I32), Ty::option(Ty::I32));
    assert_ne!(Ty::option(Ty::I32), Ty::option(Ty::I64));

    assert_eq!(
        Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
        },
        Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
        }
    );
    assert_ne!(
        Ty::Function {
            params: vec![Ty::I32],
            ret: Box::new(Ty::Bool),
        },
        Ty::Function {
            params: vec![Ty::I64],
            ret: Box::new(Ty::Bool),
        }
    );
}

// ===========================================================================
// apply_subst through deeply nested / complex structures
// ===========================================================================

#[test]
fn apply_subst_through_closure() {
    let v = TypeVar(9000);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::F32);

    let ty = Ty::Closure {
        params: vec![Ty::Var(v)],
        ret: Box::new(Ty::Var(v)),
        captures: vec![Ty::Var(v)],
    };
    assert_eq!(
        ty.apply_subst(&subst),
        Ty::Closure {
            params: vec![Ty::F32],
            ret: Box::new(Ty::F32),
            captures: vec![Ty::F32],
        }
    );
}

#[test]
fn apply_subst_through_pointer() {
    let v = TypeVar(9010);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::U64);

    let ty = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::Var(v)),
    };
    assert_eq!(
        ty.apply_subst(&subst),
        Ty::Pointer {
            is_mutable: false,
            pointee: Box::new(Ty::U64),
        }
    );
}

#[test]
fn apply_subst_through_trait_object() {
    let v = TypeVar(9020);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::Char);

    let ty = Ty::TraitObject {
        traits: vec![TraitObjectBound {
            trait_name: "Iter".to_string(),
            args: vec![Ty::Var(v)],
        }],
    };
    assert_eq!(
        ty.apply_subst(&subst),
        Ty::TraitObject {
            traits: vec![TraitObjectBound {
                trait_name: "Iter".to_string(),
                args: vec![Ty::Char],
            }],
        }
    );
}

#[test]
fn apply_subst_through_slice() {
    let v = TypeVar(9030);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::I16);

    let ty = Ty::Slice(Box::new(Ty::Var(v)));
    assert_eq!(ty.apply_subst(&subst), Ty::Slice(Box::new(Ty::I16)));
}

#[test]
fn apply_subst_through_array() {
    let v = TypeVar(9040);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::U8);

    let ty = Ty::Array(Box::new(Ty::Var(v)), 256);
    assert_eq!(ty.apply_subst(&subst), Ty::Array(Box::new(Ty::U8), 256));
}

#[test]
fn apply_subst_named_machine_unchanged() {
    let v = TypeVar(9050);
    let mut subst = Substitution::new();
    subst.insert(v, Ty::I32);

    let ty = Ty::Named {
        name: "SM".to_string(),
        args: vec![],
    };
    assert_eq!(
        ty.apply_subst(&subst),
        Ty::Named {
            name: "SM".to_string(),
            args: vec![],
        }
    );
}
