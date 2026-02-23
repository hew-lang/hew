//===- HewTypes.td - Hew dialect type definitions ---------*- tablegen -*-===//
//
// Defines custom MLIR types for the Hew dialect.
//
//===----------------------------------------------------------------------===//

#ifndef HEW_TYPES
#define HEW_TYPES

include "hew/mlir/HewDialect.td"
include "mlir/IR/AttrTypeBase.td"

// Base class for Hew dialect types.
class Hew_Type<string name, string typeMnemonic, list<Trait> traits = []>
    : TypeDef<Hew_Dialect, name, traits> {
  let mnemonic = typeMnemonic;
}

//===----------------------------------------------------------------------===//
// !hew.actor_ref — Opaque reference to a runtime actor
//===----------------------------------------------------------------------===//

def Hew_ActorRefType : Hew_Type<"ActorRef", "actor_ref"> {
  let summary = "Reference to a Hew actor instance.";
  let description = [{
    The `!hew.actor_ref` type represents an opaque reference to a runtime
    actor (a `hew_actor*` pointer).  It is the result of `hew.actor_spawn`
    and the target of `hew.actor_send`, `hew.actor_ask`, `hew.actor_stop`,
    and `hew.actor_close`.

    At the LLVM level this lowers to `!llvm.ptr`.

    Example:
    ```mlir
    %actor = hew.actor_spawn ... : ... -> !hew.actor_ref
    hew.actor_send %actor ...
    ```
  }];
}

//===----------------------------------------------------------------------===//
// !hew.typed_actor_ref<"ActorName"> — Typed reference to a named actor
//===----------------------------------------------------------------------===//

def Hew_TypedActorRefType : Hew_Type<"TypedActorRef", "typed_actor_ref"> {
  let summary = "Typed reference to a named actor kind.";
  let description = [{
    The `!hew.typed_actor_ref<"Counter">` type carries the actor type name
    so method dispatch can resolve handlers without side-channel dictionaries.
    At the LLVM level this lowers to `!llvm.ptr`.
  }];
  let parameters = (ins StringRefParameter<"actor type name">:$actorName);
  let assemblyFormat = "`<` $actorName `>`";
}

//===----------------------------------------------------------------------===//
// !hew.string_ref — Opaque reference to a runtime string
//===----------------------------------------------------------------------===//

def Hew_StringRefType : Hew_Type<"StringRef", "string_ref"> {
  let summary = "Reference to a Hew string value.";
  let description = [{
    The `!hew.string_ref` type represents an opaque pointer to a C string.
    At the LLVM level this lowers to `!llvm.ptr`.
  }];
}

//===----------------------------------------------------------------------===//
// !hew.vec<T> — Typed vector collection
//===----------------------------------------------------------------------===//

def Hew_VecType : Hew_Type<"Vec", "vec"> {
  let summary = "Hew Vec<T> collection.";
  let description = [{
    The `!hew.vec<i32>` type represents a growable vector whose element
    type is known.  This lets collection ops (`hew.vec.push`, etc.) select
    the correct runtime function without side-channel type dictionaries.
    At the LLVM level this lowers to `!llvm.ptr`.
  }];
  let parameters = (ins "mlir::Type":$elementType);
  let assemblyFormat = "`<` $elementType `>`";
}

//===----------------------------------------------------------------------===//
// !hew.hashmap<K,V> — Typed hash-map collection
//===----------------------------------------------------------------------===//

def Hew_HashMapType : Hew_Type<"HashMap", "hashmap"> {
  let summary = "Hew HashMap<K,V> collection.";
  let description = [{
    The `!hew.hashmap<i32, !llvm.ptr>` type represents a hash map whose
    key and value types are known.  At the LLVM level this lowers to
    `!llvm.ptr`.
  }];
  let parameters = (ins "mlir::Type":$keyType, "mlir::Type":$valueType);
  let assemblyFormat = "`<` $keyType `,` $valueType `>`";
}

//===----------------------------------------------------------------------===//
// !hew.handle<"kind"> — Opaque stdlib handle
//===----------------------------------------------------------------------===//

def Hew_HandleType : Hew_Type<"Handle", "handle"> {
  let summary = "Opaque handle to a stdlib resource.";
  let description = [{
    The `!hew.handle<"http.Server">` type represents an opaque runtime
    handle whose kind is known.  This replaces the `handleVarTypes`
    dictionary.  At the LLVM level this lowers to `!llvm.ptr`.
  }];
  let parameters = (ins StringRefParameter<"handle kind">:$handleKind);
  let assemblyFormat = "`<` $handleKind `>`";
}

//===----------------------------------------------------------------------===//
// !hew.option<T> — Option type (None | Some(T))
//===----------------------------------------------------------------------===//

def Hew_OptionEnumType : Hew_Type<"OptionEnum", "option"> {
  let summary = "Hew Option<T> algebraic type.";
  let description = [{
    The `!hew.option<i32>` type represents a value that is either `None`
    (tag 0) or `Some(T)` (tag 1). This preserves the algebraic semantics
    in the high-level IR. At the LLVM level this lowers to
    `!llvm.struct<(i32, T)>` where field 0 is the discriminant tag.

    Example:
    ```mlir
    %none = hew.enum_construct variant 0 of "Option"()
              : () -> !hew.option<i32>
    %some = hew.enum_construct variant 1 of "Option"(%val)
              : (i32) -> !hew.option<i32>
    ```
  }];
  let parameters = (ins "mlir::Type":$innerType);
  let assemblyFormat = "`<` $innerType `>`";
}

//===----------------------------------------------------------------------===//
// !hew.result<T, E> — Result type (Ok(T) | Err(E))
//===----------------------------------------------------------------------===//

def Hew_ResultEnumType : Hew_Type<"ResultEnum", "result"> {
  let summary = "Hew Result<T,E> algebraic type.";
  let description = [{
    The `!hew.result<i32, !llvm.ptr>` type represents a value that is
    either `Ok(T)` (tag 0, payload at position 1) or `Err(E)` (tag 1,
    payload at position 2). This preserves the algebraic semantics in the
    high-level IR. At the LLVM level this lowers to
    `!llvm.struct<(i32, T, E)>` where field 0 is the discriminant tag.

    Example:
    ```mlir
    %ok = hew.enum_construct variant 0 of "__Result"(%val)
            payload_positions = [1] : (i32) -> !hew.result<i32, !llvm.ptr>
    %err = hew.enum_construct variant 1 of "__Result"(%e)
            payload_positions = [2] : (!llvm.ptr) -> !hew.result<i32, !llvm.ptr>
    ```
  }];
  let parameters = (ins "mlir::Type":$okType, "mlir::Type":$errType);
  let assemblyFormat = "`<` $okType `,` $errType `>`";
}

//===----------------------------------------------------------------------===//
// !hew.tuple<T1, T2, ...> — Product type (anonymous struct)
//===----------------------------------------------------------------------===//

def Hew_TupleType : Hew_Type<"HewTuple", "tuple"> {
  let summary = "Hew tuple type (anonymous product type).";
  let description = [{
    The `!hew.tuple<i32, !hew.string_ref>` type represents a fixed-size
    product of heterogeneous values.  At the LLVM level this lowers to
    `!llvm.struct<(T1, T2, ...)>`.

    Example:
    ```mlir
    %t = hew.struct_init(%a, %b) : !hew.tuple<i32, !hew.string_ref>
    ```
  }];
  let parameters = (ins ArrayRefParameter<"mlir::Type">:$elementTypes);
  let assemblyFormat = "`<` $elementTypes `>`";
}

//===----------------------------------------------------------------------===//
// !hew.array<T, N> — Fixed-size array
//===----------------------------------------------------------------------===//

def Hew_ArrayType : Hew_Type<"HewArray", "array"> {
  let summary = "Hew fixed-size array type.";
  let description = [{
    The `!hew.array<i32, 4>` type represents a fixed-size array of N
    elements of type T.  At the LLVM level this lowers to
    `!llvm.array<N x T>`.
  }];
  let parameters = (ins "mlir::Type":$elementType, "int64_t":$size);
  let assemblyFormat = "`<` $elementType `,` $size `>`";
}

//===----------------------------------------------------------------------===//
// !hew.trait_object<"TraitName"> — Trait object (fat pointer)
//===----------------------------------------------------------------------===//

def Hew_TraitObjectType : Hew_Type<"HewTraitObject", "trait_object"> {
  let summary = "Hew trait object (fat pointer with vtable).";
  let description = [{
    The `!hew.trait_object<"Display">` type represents a type-erased
    value implementing a trait.  At the LLVM level this lowers to
    `!llvm.struct<(ptr, i32)>` — a data pointer plus a type tag.
  }];
  let parameters = (ins StringRefParameter<"trait name">:$traitName);
  let assemblyFormat = "`<` $traitName `>`";
}

//===----------------------------------------------------------------------===//
// !hew.closure<(T...) -> R> — Closure fat pointer (fn_ptr + env_ptr)
//===----------------------------------------------------------------------===//

def Hew_ClosureType : Hew_Type<"Closure", "closure"> {
  let summary = "Hew closure type (fat pointer: function pointer + environment).";
  let description = [{
    The `!hew.closure<(i32) -> i64>` type represents a callable closure
    value as a fat pointer containing a function pointer and an environment
    pointer.  Non-capturing closures use a null environment pointer.

    The function pointer has the signature `(ptr %env, user_params...) -> ret`.

    At the LLVM level this lowers to `!llvm.struct<(ptr, ptr)>`.

    Example:
    ```mlir
    %c = hew.closure.create(%fn_ptr, %env_ptr)
           : (!llvm.ptr, !llvm.ptr) -> !hew.closure<(i32) -> i64>
    ```
  }];
  let parameters = (ins
    ArrayRefParameter<"mlir::Type">:$inputTypes,
    "mlir::Type":$resultType
  );
  let assemblyFormat = "`<` `(` $inputTypes `)` `->` $resultType `>`";
}

#endif // HEW_TYPES
