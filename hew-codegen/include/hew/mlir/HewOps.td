//===- HewOps.td - Hew dialect operation definitions ------*- tablegen -*-===//
//
// Defines the Phase 1 operations for the Hew MLIR dialect.
//
// Phase 1 covers core language constructs. For arithmetic, control flow,
// functions, and memory we reuse the standard MLIR dialects (arith, scf,
// func, memref). The Hew dialect only defines operations for Hew-specific
// constructs.
//
//===----------------------------------------------------------------------===//

#ifndef HEW_OPS
#define HEW_OPS

include "hew/mlir/HewDialect.td"
include "hew/mlir/HewTypes.td"
include "mlir/IR/BuiltinAttributes.td"
include "mlir/IR/SymbolInterfaces.td"
include "mlir/Interfaces/SideEffectInterfaces.td"

//===----------------------------------------------------------------------===//
// hew.constant — Hew-specific constant values
//===----------------------------------------------------------------------===//
//
// Represents literal values that are specific to Hew (string refs, unit type,
// duration literals). For integer and float constants, prefer arith.constant.
// This op is useful for bootstrapping and for values that don't have a direct
// arith dialect equivalent.
//

def Hew_ConstantOp : Hew_Op<"constant", [Pure]> {
  let summary = "Produce a constant value.";
  let description = [{
    The `hew.constant` operation produces a Hew constant value. This is used
    for Hew-specific constant types (string references, unit, etc.) as well
    as standard integer/float/bool constants during initial lowering before
    they are canonicalized to arith.constant.

    Example:
    ```mlir
    %0 = hew.constant 42 : i64
    %1 = hew.constant 3.14 : f64
    %2 = hew.constant true
    ```
  }];

  let arguments = (ins AnyAttr:$value);
  let results = (outs AnyType:$result);

  let hasCustomAssemblyFormat = 1;

  let builders = [
    // Builder from integer attribute
    OpBuilder<(ins "::mlir::Type":$type, "int64_t":$value), [{
      build($_builder, $_state, type,
            $_builder.getIntegerAttr(type, value));
    }]>,
    // Builder from float attribute
    OpBuilder<(ins "::mlir::Type":$type, "double":$value), [{
      build($_builder, $_state, type,
            $_builder.getFloatAttr(type, value));
    }]>,
    // Builder from bool
    OpBuilder<(ins "bool":$value), [{
      auto type = $_builder.getI1Type();
      build($_builder, $_state, type,
            $_builder.getBoolAttr(value));
    }]>
  ];

  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// hew.global_string — Module-level string literal
//===----------------------------------------------------------------------===//

def Hew_GlobalStringOp : Hew_Op<"global_string", [Symbol]> {
  let summary = "Declare a global string constant.";
  let description = [{
    The `hew.global_string` operation declares a string literal at module scope.
    It is referenced by symbol name from other operations.

    Example:
    ```mlir
    hew.global_string @str0 = "hello, world"
    ```
  }];

  let arguments = (ins
    SymbolNameAttr:$sym_name,
    StrAttr:$value
  );

  let assemblyFormat = "$sym_name `=` $value attr-dict";
}

//===----------------------------------------------------------------------===//
// hew.print — Polymorphic print
//===----------------------------------------------------------------------===//

def Hew_PrintOp : Hew_Op<"print", [MemoryEffects<[MemWrite]>]> {
  let summary = "Print a value to stdout.";
  let description = [{
    The `hew.print` operation prints a value to standard output. It is
    polymorphic over the type of its input and dispatches to the appropriate
    runtime function (hew_print_i64, hew_print_f64, hew_print_str, etc.).

    An optional `newline` attribute controls whether a trailing newline is
    emitted (default: true).

    Example:
    ```mlir
    hew.print %x : i64
    hew.print %s : !hew.string_ref
    ```
  }];

  let arguments = (ins
    AnyType:$input,
    DefaultValuedAttr<BoolAttr, "true">:$newline
  );

  let assemblyFormat = "$input attr-dict `:` type($input)";
}

//===----------------------------------------------------------------------===//
// hew.cast — Hew-specific type conversion
//===----------------------------------------------------------------------===//

def Hew_CastOp : Hew_Op<"cast", [Pure]> {
  let summary = "Convert between Hew types.";
  let description = [{
    The `hew.cast` operation converts a value from one Hew type to another.
    This handles Hew-specific conversions like int-to-float, widening, etc.

    Example:
    ```mlir
    %f = hew.cast %i : i32 to f64
    ```
  }];

  let arguments = (ins AnyType:$input);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$input attr-dict `:` type($input) `to` type($result)";
  let hasFolder = 1;
  let hasCanonicalizer = 1;
}

//===----------------------------------------------------------------------===//
// hew.struct_init — Construct a struct value
//===----------------------------------------------------------------------===//

def Hew_StructInitOp : Hew_Op<"struct_init", [Pure]> {
  let summary = "Initialize a struct value from field values.";
  let description = [{
    The `hew.struct_init` operation constructs a struct value from its
    constituent field values. The `field_names` attribute specifies the
    names of the fields in order.

    Example:
    ```mlir
    %s = hew.struct_init(%x, %y) {field_names = ["x", "y"], struct_name = "Point"}
           : (i64, i64) -> !llvm.struct<(i64, i64)>
    ```
  }];

  let arguments = (ins
    Variadic<AnyType>:$fields,
    StrArrayAttr:$field_names,
    StrAttr:$struct_name
  );
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    `(` $fields `)` attr-dict `:` functional-type($fields, $result)
  }];
  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// hew.field_get — Extract a field from a struct
//===----------------------------------------------------------------------===//

def Hew_FieldGetOp : Hew_Op<"field_get", [Pure]> {
  let summary = "Extract a field from a struct value.";
  let description = [{
    The `hew.field_get` operation extracts a named field from a struct value.

    Example:
    ```mlir
    %x = hew.field_get %s["x"] : !llvm.struct<(i64, i64)> -> i64
    ```
  }];

  let arguments = (ins
    AnyType:$struct_val,
    StrAttr:$field_name,
    I64Attr:$field_index
  );
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    $struct_val `[` $field_name `]` attr-dict `:` type($struct_val) `->` type($result)
  }];
  let hasFolder = 1;
  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// hew.field_set — Set a field in a struct (produces new struct)
//===----------------------------------------------------------------------===//

def Hew_FieldSetOp : Hew_Op<"field_set", [Pure]> {
  let summary = "Set a field in a struct, producing a new struct.";
  let description = [{
    The `hew.field_set` operation returns a new struct value with the
    specified field replaced by the given value. Structs are value types
    in Hew, so this produces a new value rather than mutating in place.

    Example:
    ```mlir
    %s2 = hew.field_set %s["x"] = %v
             : !llvm.struct<(i64, i64)>, i64 -> !llvm.struct<(i64, i64)>
    ```
  }];

  let arguments = (ins
    AnyType:$struct_val,
    AnyType:$value,
    StrAttr:$field_name,
    I64Attr:$field_index
  );
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    $struct_val `[` $field_name `]` `=` $value attr-dict
      `:` type($struct_val) `,` type($value) `->` type($result)
  }];
  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// hew.sizeof — Compute byte size of a type
//===----------------------------------------------------------------------===//

def Hew_SizeOfOp : Hew_Op<"sizeof", [Pure]> {
  let summary = "Compute the byte size of a type.";
  let description = [{
    Returns the size in bytes of the given type as an i64 value.
    Lowers to the null-GEP trick: `ptrtoint(gep(null, 1))`.

    Example:
    ```mlir
    %sz = hew.sizeof !llvm.struct<(i32, i64)>
    ```
  }];

  let arguments = (ins TypeAttr:$measured_type);
  let results = (outs AnySignlessInteger:$result);

  let assemblyFormat = "$measured_type attr-dict `->` type($result)";
}

//===----------------------------------------------------------------------===//
// hew.enum_construct — Construct an enum variant value
//===----------------------------------------------------------------------===//

def Hew_EnumConstructOp : Hew_Op<"enum_construct", [Pure]> {
  let summary = "Construct an enum variant value.";
  let description = [{
    Constructs a tagged-union enum value. `variant_index` selects the variant,
    and `payloads` are the payload values (may be empty for unit variants).
    The result type is the full enum struct type.

    Optional `payload_positions` specifies where each payload goes in the
    struct (for Result<T,E> where Ok fills slot 1, Err fills slot 2).
    If absent, payloads fill positions 1, 2, ... sequentially.

    Example:
    ```mlir
    %r = hew.enum_construct variant 0 of "Color"(%x)
           : (i32) -> !llvm.struct<(i32, i32)>
    ```
  }];

  let arguments = (ins
    I32Attr:$variant_index,
    StrAttr:$enum_name,
    Variadic<AnyType>:$payloads,
    OptionalAttr<I64ArrayAttr>:$payload_positions
  );
  let results = (outs AnyType:$result);

  let hasCustomAssemblyFormat = 1;
  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// hew.enum_extract_tag — Extract tag from an enum value
//===----------------------------------------------------------------------===//

def Hew_EnumExtractTagOp : Hew_Op<"enum_extract_tag", [Pure]> {
  let summary = "Extract the discriminant tag from an enum value.";
  let description = [{
    Extracts the tag (variant index) from an enum value. If the enum type
    is a bare i32 (all-unit-variant enum), passes through unchanged.
    Otherwise extracts field 0 from the struct.
  }];

  let arguments = (ins AnyType:$enum_val);
  let results = (outs I32:$tag);

  let assemblyFormat = "$enum_val attr-dict `:` type($enum_val)";
  let hasVerifier = 1;
  let hasFolder = 1;
}

//===----------------------------------------------------------------------===//
// hew.enum_extract_payload — Extract payload from an enum value
//===----------------------------------------------------------------------===//

def Hew_EnumExtractPayloadOp : Hew_Op<"enum_extract_payload", [Pure]> {
  let summary = "Extract a payload field from an enum variant.";
  let description = [{
    Extracts a single payload field from an enum value at the given
    absolute position in the underlying struct.
  }];

  let arguments = (ins
    AnyType:$enum_val,
    I64Attr:$field_index
  );
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    $enum_val `[` $field_index `]` attr-dict `:` type($enum_val) `->` type($result)
  }];
  let hasVerifier = 1;
  let hasFolder = 1;
}

//===----------------------------------------------------------------------===//
// hew.pack_args — Pack values into a stack-allocated struct buffer
//===----------------------------------------------------------------------===//

def Hew_PackArgsOp : Hew_Op<"pack_args", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Pack values into a stack-allocated struct buffer.";
  let description = [{
    Packs the given values into a stack-allocated struct and returns
    a pointer to it and its byte size. For empty args, returns (null, 0).
    Used by actor send/ask/spawn for message packing.

    Example:
    ```mlir
    %ptr, %sz = hew.pack_args(%x, %y) : (i32, i64) -> (!llvm.ptr, i64)
    ```
  }];

  let arguments = (ins Variadic<AnyType>:$args);
  let results = (outs
    AnyType:$data_ptr,
    AnySignlessInteger:$data_size
  );

  let assemblyFormat = [{
    `(` $args `)` attr-dict `:` functional-type($args, results)
  }];
}

//===----------------------------------------------------------------------===//
// Actor Operations (Phase 1)
//===----------------------------------------------------------------------===//
//
// These operations capture Hew's actor model constructs at a level above raw
// runtime calls.  MLIRGen emits them; the Hew-to-LLVM lowering pass (in
// codegen.cpp) expands each one to state-struct allocation, field stores,
// sizeof computation, and the corresponding runtime function call(s).
//
// Actor references use `!hew.actor_ref`, which lowers to `!llvm.ptr`.
//

//===----------------------------------------------------------------------===//
// hew.actor_spawn — Spawn a new actor
//===----------------------------------------------------------------------===//

def Hew_ActorSpawnOp : Hew_Op<"actor_spawn"> {
  let summary = "Spawn a new actor instance.";
  let description = [{
    Spawns a new actor with the given initial state values.  The `dispatch_fn`
    symbol reference identifies the dispatch function generated for the actor.
    `state_type` carries the LLVM struct type for the actor's state so that
    the lowering pass can allocate, populate, and size the state.

    Returns a `!hew.actor_ref` (lowers to `!llvm.ptr`).

    Example:
    ```mlir
    %actor = hew.actor_spawn @Counter_dispatch(%init)
               {actor_name = "Counter",
                state_type = !llvm.struct<"Counter_state", (i32)>}
               : (i32) -> !hew.actor_ref
    ```
  }];

  let arguments = (ins
    StrAttr:$actor_name,
    FlatSymbolRefAttr:$dispatch_fn,
    TypeAttr:$state_type,
    Variadic<AnyType>:$init_args,
    OptionalAttr<I64Attr>:$mailbox_capacity,
    OptionalAttr<I32Attr>:$overflow_policy,
    OptionalAttr<FlatSymbolRefAttr>:$coalesce_key_fn,
    OptionalAttr<I32Attr>:$coalesce_fallback
  );
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    $dispatch_fn `(` $init_args `)` attr-dict
      `:` functional-type($init_args, $result)
  }];

  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// hew.actor_send — Fire-and-forget message send
//===----------------------------------------------------------------------===//

def Hew_ActorSendOp : Hew_Op<"actor_send"> {
  let summary = "Send a message to an actor (fire-and-forget).";
  let description = [{
    Sends a message to the target actor.  `msg_type` is an integer index
    selecting the receive handler.  The variadic `args` are packed into a
    temporary struct, deep-copied by the runtime.

    Example:
    ```mlir
    hew.actor_send %actor {msg_type = 0 : i32} (%n) : (i32)
    ```
  }];

  let arguments = (ins
    AnyType:$target,
    I32Attr:$msg_type,
    Variadic<AnyType>:$args
  );

  let assemblyFormat = [{
    $target attr-dict `(` $args `)` `:` `(` type($target) `,` type($args) `)`
  }];

  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// hew.actor_ask — Request-response (blocking in Phase 1)
//===----------------------------------------------------------------------===//

def Hew_ActorAskOp : Hew_Op<"actor_ask"> {
  let summary = "Send a message and block for a reply (Phase 1).";
  let description = [{
    Sends a request message to an actor and blocks the calling thread until
    a reply arrives.  In a future coroutine-based Phase 2 this will suspend
    the current task instead.

    Example:
    ```mlir
    %val = hew.actor_ask %actor {msg_type = 1 : i32} (%key)
             : (i32) -> i64
    ```
  }];

  let arguments = (ins
    AnyType:$target,
    I32Attr:$msg_type,
    Variadic<AnyType>:$args,
    OptionalAttr<I64Attr>:$timeout_ms
  );
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    $target attr-dict `(` $args `)` `:` `(` type($target) `,` type($args) `)` `->` type($result)
  }];

  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// hew.actor_stop — Request graceful actor shutdown
//===----------------------------------------------------------------------===//

def Hew_ActorStopOp : Hew_Op<"actor_stop"> {
  let summary = "Request graceful shutdown of an actor.";
  let description = [{
    Sends a system message requesting the actor to stop.  The actor transitions
    to STOPPING state, drains system messages, then STOPPED.
  }];

  let arguments = (ins AnyType:$target);
  let assemblyFormat = "$target attr-dict `:` type($target)";
}

//===----------------------------------------------------------------------===//
// hew.actor_close — Stop accepting new messages
//===----------------------------------------------------------------------===//

def Hew_ActorCloseOp : Hew_Op<"actor_close"> {
  let summary = "Close an actor's mailbox (stop accepting new messages).";

  let arguments = (ins AnyType:$target);
  let assemblyFormat = "$target attr-dict `:` type($target)";
}

//===----------------------------------------------------------------------===//
// hew.actor.self — Get the current actor's reference
//===----------------------------------------------------------------------===//

def Hew_ActorSelfOp : Hew_Op<"actor.self"> {
  let summary = "Get the current actor's reference.";
  let description = [{
    Returns a reference to the currently executing actor.  This calls the
    runtime's `hew_actor_self()` function under the hood.

    Example:
    ```mlir
    %me = hew.actor.self
    ```
  }];

  let results = (outs AnyType:$result);
  let assemblyFormat = "attr-dict `:` type($result)";
}

//===----------------------------------------------------------------------===//
// hew.actor.link — Bidirectional link between two actors
//===----------------------------------------------------------------------===//

def Hew_ActorLinkOp : Hew_Op<"actor.link"> {
  let summary = "Create a bidirectional link between two actors.";
  let description = [{
    Links the calling actor to the target actor.  If either actor crashes
    or stops, the other receives an exit signal.

    Example:
    ```mlir
    hew.actor.link %self_ref, %target : (!hew.actor_ref, !hew.actor_ref)
    ```
  }];

  let arguments = (ins AnyType:$self_ref, AnyType:$target);
  let assemblyFormat = "$self_ref `,` $target attr-dict `:` `(` type($self_ref) `,` type($target) `)`";
}

//===----------------------------------------------------------------------===//
// hew.actor.unlink — Remove a bidirectional link
//===----------------------------------------------------------------------===//

def Hew_ActorUnlinkOp : Hew_Op<"actor.unlink"> {
  let summary = "Remove a bidirectional link between two actors.";

  let arguments = (ins AnyType:$self_ref, AnyType:$target);
  let assemblyFormat = "$self_ref `,` $target attr-dict `:` `(` type($self_ref) `,` type($target) `)`";
}

//===----------------------------------------------------------------------===//
// hew.actor.monitor — Unidirectional monitor on an actor
//===----------------------------------------------------------------------===//

def Hew_ActorMonitorOp : Hew_Op<"actor.monitor"> {
  let summary = "Create a unidirectional monitor on an actor.";
  let description = [{
    Monitors the target actor.  Returns an i64 monitor reference that can
    be passed to `hew.actor.demonitor` to cancel the monitor.

    Example:
    ```mlir
    %ref = hew.actor.monitor %self_ref, %target : (!hew.actor_ref, !hew.actor_ref)
    ```
  }];

  let arguments = (ins AnyType:$self_ref, AnyType:$target);
  let results = (outs I64:$monitor_ref);
  let assemblyFormat = "$self_ref `,` $target attr-dict `:` `(` type($self_ref) `,` type($target) `)`";
}

//===----------------------------------------------------------------------===//
// hew.actor.demonitor — Remove a monitor
//===----------------------------------------------------------------------===//

def Hew_ActorDemonitorOp : Hew_Op<"actor.demonitor"> {
  let summary = "Remove a monitor.";

  let arguments = (ins I64:$monitor_ref);
  let assemblyFormat = "$monitor_ref attr-dict";
}

//===----------------------------------------------------------------------===//
// hew.cooperate — Cooperative scheduler yield point
//===----------------------------------------------------------------------===//

def Hew_CooperateOp : Hew_Op<"cooperate"> {
  let summary = "Cooperative scheduler yield point.";
  let description = [{
    Yields the current actor's execution slot to the scheduler so other
    actors can make progress.  Lowers to `hew_actor_cooperate()`.

    Example:
    ```mlir
    hew.cooperate
    ```
  }];

  let assemblyFormat = "attr-dict";
}

//===----------------------------------------------------------------------===//
// hew.sleep — Sleep for a given number of milliseconds
//===----------------------------------------------------------------------===//

def Hew_SleepOp : Hew_Op<"sleep"> {
  let summary = "Sleep for a given number of milliseconds.";
  let description = [{
    Suspends the current actor/thread for at least the given duration.

    Example:
    ```mlir
    hew.sleep %ms : i64
    ```
  }];

  let arguments = (ins AnyType:$duration_ms);
  let assemblyFormat = "$duration_ms attr-dict `:` type($duration_ms)";
}

//===----------------------------------------------------------------------===//
// hew.panic — Terminate the current actor with a crash
//===----------------------------------------------------------------------===//

def Hew_PanicOp : Hew_Op<"panic"> {
  let summary = "Terminate the current actor with a crash.";
  let description = [{
    Triggers a panic in the current actor.  The runtime signal handler
    catches it, marks the actor as crashed, and longjmps back to the
    scheduler's recovery point.

    Example:
    ```mlir
    hew.panic
    ```
  }];

  let assemblyFormat = "attr-dict";
}

//===----------------------------------------------------------------------===//
// hew.receive — Actor message dispatch
//===----------------------------------------------------------------------===//
//
// Encapsulates the actor dispatch pattern: switch on msg_type, extract args
// from the data pointer, call the appropriate handler.  MLIRGen emits this
// instead of an inline chain of scf.if + LLVM loads; the lowering pass
// expands it to the concrete dispatching code.
//

def Hew_ReceiveOp : Hew_Op<"receive"> {
  let summary = "Dispatch an incoming actor message to a handler.";
  let description = [{
    The `hew.receive` operation dispatches an incoming actor message to the
    appropriate receive handler function based on the `msg_type` index.

    The `handlers` array attribute lists handler function symbols in message-
    type order: `handlers[0]` is called when `msg_type == 0`, etc.

    Each handler has signature `(ptr state, args...) -> void`.  The receive op
    extracts args from the data pointer based on each handler's function
    signature (looked up from the module symbol table during lowering).

    Example:
    ```mlir
    hew.receive(%state, %msg_type, %data, %data_size)
        {handlers = [@Counter_increment, @Counter_get_value]}
        : (!llvm.ptr, !llvm.ptr)
    ```
  }];

  let arguments = (ins
    AnyType:$state,
    I32:$msg_type,
    AnyType:$data,
    AnySignlessInteger:$data_size,
    ArrayAttr:$handlers
  );

  let assemblyFormat = [{
    `(` $state `,` $msg_type `,` $data `,` $data_size `)`
      attr-dict `:` `(` type($state) `,` type($data) `,` type($data_size) `)`
  }];
}

//===----------------------------------------------------------------------===//
// Vec collection ops
//===----------------------------------------------------------------------===//

def Hew_VecNewOp : Hew_Op<"vec.new", [MemoryEffects<[MemWrite]>]> {
  let summary = "Create a new empty Vec<T>.";
  let description = [{
    Creates a new, empty vector.  The result type `!hew.vec<T>` carries the
    element type so that subsequent `hew.vec.push` / `hew.vec.get` ops can
    select the correct runtime function without side-channel dictionaries.
  }];

  let results = (outs AnyType:$result);

  let assemblyFormat = "attr-dict `:` type($result)";
  let hasVerifier = 1;
}

def Hew_VecPushOp : Hew_Op<"vec.push", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Push an element onto a Vec.";
  let arguments = (ins AnyType:$vec, AnyType:$value);
  let assemblyFormat = "$vec `,` $value attr-dict `:` `(` type($vec) `,` type($value) `)`";
  let hasVerifier = 1;
}

def Hew_VecGetOp : Hew_Op<"vec.get", [MemoryEffects<[MemRead]>]> {
  let summary = "Get element at index from a Vec.";
  let arguments = (ins AnyType:$vec, AnyType:$index);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$vec `[` $index `]` attr-dict `:` `(` type($vec) `,` type($index) `)` `->` type($result)";
  let hasVerifier = 1;
}

def Hew_VecSetOp : Hew_Op<"vec.set", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Set element at index in a Vec.";
  let arguments = (ins AnyType:$vec, AnyType:$index, AnyType:$value);
  let assemblyFormat = "$vec `[` $index `]` `=` $value attr-dict `:` `(` type($vec) `,` type($index) `,` type($value) `)`";
  let hasVerifier = 1;
}

def Hew_VecLenOp : Hew_Op<"vec.len", [MemoryEffects<[MemRead]>]> {
  let summary = "Get length of a Vec.";
  let arguments = (ins AnyType:$vec);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$vec attr-dict `:` type($vec) `->` type($result)";
  let hasVerifier = 1;
}

def Hew_VecPopOp : Hew_Op<"vec.pop", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Pop and return the last element from a Vec.";
  let arguments = (ins AnyType:$vec);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$vec attr-dict `:` type($vec) `->` type($result)";
  let hasVerifier = 1;
}

def Hew_VecRemoveOp : Hew_Op<"vec.remove", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Remove first occurrence of value from a Vec.";
  let arguments = (ins AnyType:$vec, AnyType:$value);
  let assemblyFormat = "$vec `,` $value attr-dict `:` `(` type($vec) `,` type($value) `)`";
  let hasVerifier = 1;
}

def Hew_VecIsEmptyOp : Hew_Op<"vec.is_empty"> {
  let summary = "Check if a Vec is empty.";
  let arguments = (ins AnyType:$vec);
  let results = (outs I1:$result);
  let assemblyFormat = "$vec attr-dict `:` type($vec)";
}

def Hew_VecClearOp : Hew_Op<"vec.clear"> {
  let summary = "Remove all elements from a Vec.";
  let arguments = (ins AnyType:$vec);
  let assemblyFormat = "$vec attr-dict `:` type($vec)";
}

def Hew_VecFreeOp : Hew_Op<"vec.free"> {
  let summary = "Free a Vec's memory.";
  let arguments = (ins AnyType:$vec);
  let assemblyFormat = "$vec attr-dict `:` type($vec)";
  let hasCanonicalizer = 1;
}

//===----------------------------------------------------------------------===//
// HashMap collection ops
//===----------------------------------------------------------------------===//

def Hew_HashMapNewOp : Hew_Op<"hashmap.new", [MemoryEffects<[MemWrite]>]> {
  let summary = "Create a new empty HashMap<K,V>.";
  let results = (outs AnyType:$result);
  let assemblyFormat = "attr-dict `:` type($result)";
  let hasVerifier = 1;
}

def Hew_HashMapInsertOp : Hew_Op<"hashmap.insert", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Insert a key-value pair into a HashMap.";
  let arguments = (ins AnyType:$map, AnyType:$key, AnyType:$value);
  let assemblyFormat = "$map `,` $key `,` $value attr-dict `:` `(` type($map) `,` type($key) `,` type($value) `)`";
  let hasVerifier = 1;
}

def Hew_HashMapGetOp : Hew_Op<"hashmap.get", [MemoryEffects<[MemRead]>]> {
  let summary = "Get value by key from a HashMap.";
  let arguments = (ins AnyType:$map, AnyType:$key);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$map `[` $key `]` attr-dict `:` `(` type($map) `,` type($key) `)` `->` type($result)";
  let hasVerifier = 1;
}

def Hew_HashMapContainsKeyOp : Hew_Op<"hashmap.contains_key", [MemoryEffects<[MemRead]>]> {
  let summary = "Check if HashMap contains a key.";
  let arguments = (ins AnyType:$map, AnyType:$key);
  let results = (outs I1:$result);
  let assemblyFormat = "$map `,` $key attr-dict `:` `(` type($map) `,` type($key) `)`";
  let hasVerifier = 1;
}

def Hew_HashMapRemoveOp : Hew_Op<"hashmap.remove", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Remove a key from a HashMap.";
  let arguments = (ins AnyType:$map, AnyType:$key);
  let assemblyFormat = "$map `,` $key attr-dict `:` `(` type($map) `,` type($key) `)`";
  let hasVerifier = 1;
}

def Hew_HashMapLenOp : Hew_Op<"hashmap.len", [MemoryEffects<[MemRead]>]> {
  let summary = "Get number of entries in a HashMap.";
  let arguments = (ins AnyType:$map);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$map attr-dict `:` type($map) `->` type($result)";
  let hasVerifier = 1;
}

def Hew_HashMapFreeOp : Hew_Op<"hashmap.free"> {
  let summary = "Free a HashMap's memory.";
  let arguments = (ins AnyType:$map);
  let assemblyFormat = "$map attr-dict `:` type($map)";
  let hasCanonicalizer = 1;
}

def Hew_HashMapKeysOp : Hew_Op<"hashmap.keys", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Get all keys from a HashMap as a Vec.";
  let arguments = (ins AnyType:$map);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$map attr-dict `:` type($map) `->` type($result)";
  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// Supervisor operations
//===----------------------------------------------------------------------===//

def Hew_SupervisorNewOp : Hew_Op<"supervisor.new"> {
  let summary = "Create a new supervisor with the given strategy and limits.";
  let description = [{
    Creates a new supervisor and returns a pointer to it. The `strategy`
    selects OneForOne (0), OneForAll (1), or RestForOne (2). `max_restarts`
    and `window_secs` control the restart intensity.

    Lowers to `hew_supervisor_new(strategy, max_restarts, window_secs)`.

    Example:
    ```mlir
    %sup = hew.supervisor.new(%strategy, %max, %window)
             : (i32, i32, i32) -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$strategy, AnyType:$max_restarts, AnyType:$window_secs);
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    `(` $strategy `,` $max_restarts `,` $window_secs `)` attr-dict
      `:` `(` type($strategy) `,` type($max_restarts) `,` type($window_secs) `)` `->` type($result)
  }];
}

def Hew_SupervisorStartOp : Hew_Op<"supervisor.start"> {
  let summary = "Start a supervisor (begins watching for child crashes).";
  let description = [{
    Starts the supervisor event loop. Returns an i32 status code.

    Lowers to `hew_supervisor_start(supervisor)`.

    Example:
    ```mlir
    %status = hew.supervisor.start %sup : !llvm.ptr -> i32
    ```
  }];

  let arguments = (ins AnyType:$supervisor);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$supervisor attr-dict `:` type($supervisor) `->` type($result)";
}

def Hew_SupervisorStopOp : Hew_Op<"supervisor.stop"> {
  let summary = "Stop a supervisor.";
  let description = [{
    Stops the supervisor and all its children.

    Lowers to `hew_supervisor_stop(supervisor)`.

    Example:
    ```mlir
    hew.supervisor.stop %sup : !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$supervisor);
  let assemblyFormat = "$supervisor attr-dict `:` type($supervisor)";
}

def Hew_SupervisorAddChildOp : Hew_Op<"supervisor.add_child"> {
  let summary = "Register a child actor spec with a supervisor.";
  let description = [{
    Adds a child specification to the supervisor. The `spec` is a pointer
    to a stack-allocated HewChildSpec struct. Returns an i32 status code.

    Lowers to `hew_supervisor_add_child_spec(supervisor, spec)`.

    Example:
    ```mlir
    %status = hew.supervisor.add_child %sup, %spec
                : (!llvm.ptr, !llvm.ptr) -> i32
    ```
  }];

  let arguments = (ins AnyType:$supervisor, AnyType:$spec);
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    $supervisor `,` $spec attr-dict
      `:` `(` type($supervisor) `,` type($spec) `)` `->` type($result)
  }];
}

def Hew_ChildSpecCreateOp : Hew_Op<"supervisor.child_spec_create"> {
  let summary = "Allocate and populate a HewChildSpec struct.";
  let description = [{
    Creates a stack-allocated HewChildSpec struct from the given fields.
    The struct layout is:
      { ptr name, ptr init_state, i64 init_state_size,
        ptr dispatch, i32 restart_policy, i32 mailbox_capacity, i32 overflow }

    Lowers to: alloca + GEP/store for each field.

    Example:
    ```mlir
    %spec = hew.supervisor.child_spec_create %name, %state, %size, %dispatch,
                %restart, %mbcap, %overflow
              : (!llvm.ptr, !llvm.ptr, i64, !llvm.ptr, i32, i32, i32) -> !llvm.ptr
    ```
  }];

  let arguments = (ins
    AnyType:$name,
    AnyType:$init_state,
    AnyType:$init_state_size,
    AnyType:$dispatch,
    AnyType:$restart_policy,
    AnyType:$mailbox_capacity,
    AnyType:$overflow_policy
  );
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    $name `,` $init_state `,` $init_state_size `,` $dispatch `,`
    $restart_policy `,` $mailbox_capacity `,` $overflow_policy attr-dict
      `:` `(` type($name) `,` type($init_state) `,` type($init_state_size) `,`
             type($dispatch) `,` type($restart_policy) `,` type($mailbox_capacity) `,`
             type($overflow_policy) `)` `->` type($result)
  }];
}

def Hew_SupervisorAddChildSupervisorOp : Hew_Op<"supervisor.add_child_supervisor"> {
  let summary = "Register a child supervisor with an init function.";
  let description = [{
    Adds a child supervisor to a parent supervisor, along with an init
    function pointer that the runtime can use to restart the child supervisor.
    Returns an i32 status code.

    Lowers to `hew_supervisor_add_child_supervisor_with_init(parent, child, init_fn)`.

    Example:
    ```mlir
    %status = hew.supervisor.add_child_supervisor %parent, %child, %init_fn
                : (!llvm.ptr, !llvm.ptr, !llvm.ptr) -> i32
    ```
  }];

  let arguments = (ins AnyType:$parent, AnyType:$child, AnyType:$init_fn);
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    $parent `,` $child `,` $init_fn attr-dict
      `:` `(` type($parent) `,` type($child) `,` type($init_fn) `)` `->` type($result)
  }];
}

//===----------------------------------------------------------------------===//
// hew.runtime_call — Generic runtime function call
//===----------------------------------------------------------------------===//

def Hew_RuntimeCallOp : Hew_Op<"runtime_call"> {
  let summary = "Call a Hew runtime function.";
  let description = [{
    Generic operation for calling runtime library functions (stdlib handles,
    string operations, etc.) that don't need type-polymorphic dispatch.
    This replaces hundreds of lines of inline func.call emission in MLIRGen
    with a single op whose lowering is trivially `func.call @callee(args)`.
  }];

  let arguments = (ins
    FlatSymbolRefAttr:$callee,
    Variadic<AnyType>:$operands
  );
  let results = (outs Optional<AnyType>:$result);

  let assemblyFormat = [{
    $callee `(` $operands `)` attr-dict `:` functional-type($operands, results)
  }];
}

//===----------------------------------------------------------------------===//
// hew.trait_dispatch — Dynamic trait method dispatch (vtable-based)
//===----------------------------------------------------------------------===//
//
// Encapsulates vtable-based dynamic dispatch for trait objects. A trait object
// is a fat pointer {data_ptr, vtable_ptr}. This op takes both components plus
// the method's extra arguments, and dispatches to the correct concrete
// implementation via an indirect call through the vtable.
//
// Lowering: GEP into vtable at method_index, load function pointer, indirect
// call. O(1) dispatch.
//

def Hew_TraitDispatchOp : Hew_Op<"trait_dispatch", []> {
  let summary = "Dispatch a method call on a trait object via vtable.";
  let description = [{
    Dispatches a trait method call using vtable-based dispatch. The
    `vtable_ptr` points to a constant global array of function pointers.
    `method_index` is the index into the vtable for the desired method.

    Example:
    ```mlir
    %r = hew.trait_dispatch @Drawable::draw
        data(%ptr) vtable(%vt) args(%x)
        method_index = 0
        : (!llvm.ptr, !llvm.ptr) -> f64
    ```
  }];

  let arguments = (ins
    StrAttr:$trait_name,
    StrAttr:$method_name,
    AnyType:$data_ptr,
    AnyType:$vtable_ptr,
    Variadic<AnyType>:$extra_args,
    I64Attr:$method_index
  );
  let results = (outs Optional<AnyType>:$result);

  let hasCustomAssemblyFormat = 1;
}

//===----------------------------------------------------------------------===//
// hew.string_concat — String concatenation
//===----------------------------------------------------------------------===//

def Hew_StringConcatOp : Hew_Op<"string_concat", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Concatenate two strings.";
  let arguments = (ins AnyType:$lhs, AnyType:$rhs);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$lhs `,` $rhs attr-dict `:` `(` type($lhs) `,` type($rhs) `)` `->` type($result)";
  let hasCanonicalizer = 1;
}

//===----------------------------------------------------------------------===//
// hew.to_string — Scalar-to-string conversion
//===----------------------------------------------------------------------===//

def Hew_ToStringOp : Hew_Op<"to_string", [MemoryEffects<[MemRead, MemWrite]>]> {
  let summary = "Convert a scalar value to a string.";
  let description = [{
    Converts an integer, float, bool, or char to a heap-allocated string.
    The lowering selects the appropriate runtime function based on the
    operand type (hew_int_to_string, hew_float_to_string, etc.).
  }];
  let arguments = (ins AnyType:$value);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$value attr-dict `:` type($value) `->` type($result)";
}

//===----------------------------------------------------------------------===//
// hew.string_method — Generic string method call
//===----------------------------------------------------------------------===//

def Hew_StringMethodOp : Hew_Op<"string_method"> {
  let summary = "Call a method on a string value.";
  let description = [{
    Generic op for string methods like trim, contains, starts_with, etc.
    The `method` attribute selects the runtime function:
      hew_string_{method}(receiver, args...) -> result
  }];
  let arguments = (ins
    StrAttr:$method,
    AnyType:$receiver,
    Variadic<AnyType>:$extra_args
  );
  let results = (outs Optional<AnyType>:$result);

  let assemblyFormat = [{
    $method `(` $receiver (`,` $extra_args^)? `)` attr-dict `:` `(` type($receiver) (`,` type($extra_args)^)? `)` (`->` type($result)^)?
  }];
  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// Tuple operations
//===----------------------------------------------------------------------===//

def Hew_TupleCreateOp : Hew_Op<"tuple.create", [Pure]> {
  let summary = "Create a tuple from individual values.";
  let arguments = (ins Variadic<AnyType>:$elements);
  let results = (outs AnyType:$result);
  let assemblyFormat = "`(` $elements `)` attr-dict `:` functional-type($elements, $result)";
  let hasVerifier = 1;
}

def Hew_TupleExtractOp : Hew_Op<"tuple.extract", [Pure]> {
  let summary = "Extract an element from a tuple by constant index.";
  let arguments = (ins AnyType:$tuple, I64Attr:$index);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$tuple `[` $index `]` attr-dict `:` type($tuple) `->` type($result)";
  let hasVerifier = 1;
  let hasFolder = 1;
}

//===----------------------------------------------------------------------===//
// Array operations
//===----------------------------------------------------------------------===//

def Hew_ArrayCreateOp : Hew_Op<"array.create", [Pure]> {
  let summary = "Create a fixed-size array from individual values.";
  let arguments = (ins Variadic<AnyType>:$elements);
  let results = (outs AnyType:$result);
  let assemblyFormat = "`(` $elements `)` attr-dict `:` functional-type($elements, $result)";
  let hasVerifier = 1;
}

def Hew_ArrayExtractOp : Hew_Op<"array.extract", [Pure]> {
  let summary = "Extract an element from a fixed-size array by constant index.";
  let arguments = (ins AnyType:$array, I64Attr:$index);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$array `[` $index `]` attr-dict `:` type($array) `->` type($result)";
  let hasVerifier = 1;
  let hasFolder = 1;
}

//===----------------------------------------------------------------------===//
// Trait object operations
//===----------------------------------------------------------------------===//

def Hew_TraitObjectCreateOp : Hew_Op<"trait_object.create", [Pure]> {
  let summary = "Create a trait object from a data pointer and vtable pointer.";
  let arguments = (ins AnyType:$data, AnyType:$vtable_ptr);
  let results = (outs AnyType:$result);
  let assemblyFormat = "`(` $data `,` $vtable_ptr `)` attr-dict `:` `(` type($data) `,` type($vtable_ptr) `)` `->` type($result)";
  let hasVerifier = 1;
}

def Hew_TraitObjectDataOp : Hew_Op<"trait_object.data", [Pure]> {
  let summary = "Extract the data pointer from a trait object.";
  let arguments = (ins AnyType:$trait_object);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$trait_object attr-dict `:` type($trait_object) `->` type($result)";
  let hasVerifier = 1;
  let hasFolder = 1;
}

def Hew_TraitObjectTagOp : Hew_Op<"trait_object.tag", [Pure]> {
  let summary = "Extract the vtable pointer from a trait object.";
  let arguments = (ins AnyType:$trait_object);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$trait_object attr-dict `:` type($trait_object) `->` type($result)";
  let hasVerifier = 1;
  let hasFolder = 1;
}

//===----------------------------------------------------------------------===//
// hew.vtable_ref — Reference a vtable global
//===----------------------------------------------------------------------===//

def Hew_VtableRefOp : Hew_Op<"vtable_ref", [Pure]> {
  let summary = "Reference a vtable global constant array.";
  let arguments = (ins StrAttr:$vtable_name, StrArrayAttr:$functions);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$vtable_name $functions attr-dict `:` type($result)";
}

//===----------------------------------------------------------------------===//
// Closure operations (fat pointer: fn_ptr + env_ptr)
//===----------------------------------------------------------------------===//

def Hew_ClosureCreateOp : Hew_Op<"closure.create", [Pure]> {
  let summary = "Create a closure from a function pointer and environment pointer.";
  let arguments = (ins AnyType:$fn_ptr, AnyType:$env_ptr);
  let results = (outs AnyType:$result);
  let assemblyFormat = "`(` $fn_ptr `,` $env_ptr `)` attr-dict `:` `(` type($fn_ptr) `,` type($env_ptr) `)` `->` type($result)";
  let hasVerifier = 1;
}

def Hew_ClosureGetFnOp : Hew_Op<"closure.get_fn", [Pure]> {
  let summary = "Extract the function pointer from a closure.";
  let arguments = (ins AnyType:$closure);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$closure attr-dict `:` type($closure) `->` type($result)";
  let hasVerifier = 1;
  let hasFolder = 1;
}

def Hew_ClosureGetEnvOp : Hew_Op<"closure.get_env", [Pure]> {
  let summary = "Extract the environment pointer from a closure.";
  let arguments = (ins AnyType:$closure);
  let results = (outs AnyType:$result);
  let assemblyFormat = "$closure attr-dict `:` type($closure) `->` type($result)";
  let hasVerifier = 1;
  let hasFolder = 1;
}

//===----------------------------------------------------------------------===//
// Generator operations
//===----------------------------------------------------------------------===//
//
// These ops model the thread-based generator protocol used by receive-gen
// handlers and actor-based generators.  Each op lowers to the corresponding
// hew_gen_* runtime function call.
//

//===----------------------------------------------------------------------===//
// hew.gen.create — Create a generator context
//===----------------------------------------------------------------------===//

def Hew_GenCtxCreateOp : Hew_Op<"gen.create"> {
  let summary = "Create a new generator context.";
  let description = [{
    Creates a thread-based generator context.  The `body_fn` pointer is the
    generator body function, `args_ptr` points to the packed argument struct,
    and `args_size` is the byte size of that struct.  Returns an opaque
    generator context pointer.

    Lowers to `hew_gen_ctx_create(body_fn, args_ptr, args_size)`.

    Example:
    ```mlir
    %ctx = hew.gen.create(%fn, %args, %sz) : (!llvm.ptr, !llvm.ptr, i64) -> !llvm.ptr
    ```
  }];

  let arguments = (ins
    AnyType:$body_fn,
    AnyType:$args_ptr,
    AnySignlessInteger:$args_size
  );
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    `(` $body_fn `,` $args_ptr `,` $args_size `)` attr-dict
      `:` `(` type($body_fn) `,` type($args_ptr) `,` type($args_size) `)` `->` type($result)
  }];
}

//===----------------------------------------------------------------------===//
// hew.gen.next — Advance generator and get next value
//===----------------------------------------------------------------------===//

def Hew_GenNextOp : Hew_Op<"gen.next"> {
  let summary = "Advance a generator and return a pointer to the next value.";
  let description = [{
    Resumes the generator thread and waits for the next yielded value.
    `ctx` is the generator context, `out_size_ptr` is a pointer to an i64
    where the runtime writes the byte size of the yielded value.
    Returns a pointer to the yielded value (heap-allocated), or null if
    the generator is exhausted.

    Lowers to `hew_gen_next(ctx, out_size_ptr)`.

    Example:
    ```mlir
    %val_ptr = hew.gen.next(%ctx, %out_sz) : (!llvm.ptr, !llvm.ptr) -> !llvm.ptr
    ```
  }];

  let arguments = (ins
    AnyType:$ctx,
    AnyType:$out_size_ptr
  );
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    `(` $ctx `,` $out_size_ptr `)` attr-dict
      `:` `(` type($ctx) `,` type($out_size_ptr) `)` `->` type($result)
  }];
}

//===----------------------------------------------------------------------===//
// hew.gen.yield — Yield a value from inside a generator body
//===----------------------------------------------------------------------===//

def Hew_GenYieldOp : Hew_Op<"gen.yield"> {
  let summary = "Yield a value from a generator body.";
  let description = [{
    Suspends the generator thread and passes the value at `value_ptr`
    (of `value_size` bytes) to the consumer.  Returns an i1 indicating
    whether the generator should continue (true) or has been cancelled
    (false).

    Lowers to `hew_gen_yield(ctx, value_ptr, value_size)`.

    Example:
    ```mlir
    %cont = hew.gen.yield(%ctx, %ptr, %sz) : (!llvm.ptr, !llvm.ptr, i64)
    ```
  }];

  let arguments = (ins
    AnyType:$ctx,
    AnyType:$value_ptr,
    AnySignlessInteger:$value_size
  );
  let results = (outs I1:$result);

  let assemblyFormat = [{
    `(` $ctx `,` $value_ptr `,` $value_size `)` attr-dict
      `:` `(` type($ctx) `,` type($value_ptr) `,` type($value_size) `)`
  }];
}

//===----------------------------------------------------------------------===//
// hew.gen.free — Free a generator context
//===----------------------------------------------------------------------===//

def Hew_GenFreeOp : Hew_Op<"gen.free"> {
  let summary = "Free a generator context.";
  let description = [{
    Frees the generator context and associated resources (thread, stack,
    synchronization primitives).

    Lowers to `hew_gen_free(ctx)`.

    Example:
    ```mlir
    hew.gen.free %ctx : !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$ctx);

  let assemblyFormat = "$ctx attr-dict `:` type($ctx)";
}

//===----------------------------------------------------------------------===//
// hew.gen.wrap_value — Wrap a yielded value into {1, value}
//===----------------------------------------------------------------------===//

def Hew_GenWrapValueOp : Hew_Op<"gen.wrap_value", [Pure]> {
  let summary = "Wrap a yielded value into a generator wrapper struct.";
  let description = [{
    Constructs a generator wrapper struct `{i8, YieldType}` with
    `has_value = 1` and the given value.  Represents "generator yielded
    a value."

    Lowers to `llvm.undef(wrapperType)` + `llvm.insertvalue(1_i8, [0])`
    + `llvm.insertvalue(value, [1])`.

    Example:
    ```mlir
    %w = hew.gen.wrap_value %val : i64 -> !llvm.struct<(i8, i64)>
    ```
  }];

  let arguments = (ins AnyType:$value);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$value attr-dict `:` type($value) `->` type($result)";
  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// hew.gen.wrap_done — Produce a "generator exhausted" wrapper struct {0, default}
//===----------------------------------------------------------------------===//

def Hew_GenWrapDoneOp : Hew_Op<"gen.wrap_done", [Pure]> {
  let summary = "Produce a generator-exhausted wrapper struct.";
  let description = [{
    Constructs a generator wrapper struct `{i8, YieldType}` with
    `has_value = 0` and a zero/default value in the value slot.
    Represents "generator is exhausted."

    Lowers to `llvm.undef(wrapperType)` + `llvm.insertvalue(0_i8, [0])`
    + `llvm.insertvalue(zero_of_value_type, [1])`.

    Example:
    ```mlir
    %w = hew.gen.wrap_done : !llvm.struct<(i8, i64)>
    ```
  }];

  let results = (outs AnyType:$result);

  let assemblyFormat = "attr-dict `:` type($result)";
  let hasVerifier = 1;
}

//===----------------------------------------------------------------------===//
// Regex operations
//===----------------------------------------------------------------------===//

def Hew_RegexNewOp : Hew_Op<"regex.new", [MemoryEffects<[MemWrite]>]> {
  let summary = "Compile a regex pattern into a reusable pattern object.";
  let description = [{
    Compiles the given pattern string into a regex pattern object.
    Returns an opaque pointer to the compiled regex.

    Lowers to `hew_regex_new(pattern)`.

    Example:
    ```mlir
    %re = hew.regex.new %pattern : !llvm.ptr -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$pattern);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$pattern attr-dict `:` type($pattern) `->` type($result)";
}

def Hew_RegexIsMatchOp : Hew_Op<"regex.is_match", [MemoryEffects<[MemRead]>]> {
  let summary = "Check if a regex matches the given text.";
  let description = [{
    Tests whether the regex pattern matches the text string.
    Returns an i1 (bool) result.

    Lowers to `hew_regex_is_match(regex, text)`.

    Example:
    ```mlir
    %matched = hew.regex.is_match %regex, %text : (!llvm.ptr, !llvm.ptr) -> i1
    ```
  }];

  let arguments = (ins AnyType:$regex, AnyType:$text);
  let results = (outs I1:$result);

  let assemblyFormat = "$regex `,` $text attr-dict `:` `(` type($regex) `,` type($text) `)`";
}

def Hew_RegexFindOp : Hew_Op<"regex.find"> {
  let summary = "Find the first match of a regex in the given text.";
  let description = [{
    Finds the first match of the regex pattern in the text string.
    Returns the matched substring as a string pointer.

    Lowers to `hew_regex_find(regex, text)`.

    Example:
    ```mlir
    %match = hew.regex.find %regex, %text : (!llvm.ptr, !llvm.ptr) -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$regex, AnyType:$text);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$regex `,` $text attr-dict `:` `(` type($regex) `,` type($text) `)` `->` type($result)";
}

def Hew_RegexReplaceOp : Hew_Op<"regex.replace"> {
  let summary = "Replace matches of a regex in the given text.";
  let description = [{
    Replaces all matches of the regex pattern in the text with the
    replacement string. Returns a new string.

    Lowers to `hew_regex_replace(regex, text, replacement)`.

    Example:
    ```mlir
    %result = hew.regex.replace %regex, %text, %repl
                : (!llvm.ptr, !llvm.ptr, !llvm.ptr) -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$regex, AnyType:$text, AnyType:$replacement);
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    $regex `,` $text `,` $replacement attr-dict
      `:` `(` type($regex) `,` type($text) `,` type($replacement) `)` `->` type($result)
  }];
}

def Hew_RegexFreeOp : Hew_Op<"regex.free"> {
  let summary = "Free a compiled regex pattern.";
  let description = [{
    Frees the memory associated with a compiled regex pattern.

    Lowers to `hew_regex_free(regex)`.

    Example:
    ```mlir
    hew.regex.free %regex : !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$regex);

  let assemblyFormat = "$regex attr-dict `:` type($regex)";
}

//===----------------------------------------------------------------------===//
// Scheduler lifecycle operations
//===----------------------------------------------------------------------===//

def Hew_SchedInitOp : Hew_Op<"sched.init"> {
  let summary = "Initialize the Hew actor scheduler.";
  let description = [{
    Initializes the M:N work-stealing scheduler.  Must be called at the
    beginning of `main` before any actors are spawned.

    Lowers to `hew_sched_init()`.

    Example:
    ```mlir
    hew.sched.init
    ```
  }];

  let assemblyFormat = "attr-dict";
}

def Hew_SchedShutdownOp : Hew_Op<"sched.shutdown"> {
  let summary = "Shut down the Hew actor scheduler.";
  let description = [{
    Gracefully shuts down the scheduler, waiting for all worker threads
    to complete.  Must be called before `main` returns.

    Lowers to `hew_sched_shutdown()`.

    Example:
    ```mlir
    hew.sched.shutdown
    ```
  }];

  let assemblyFormat = "attr-dict";
}

//===----------------------------------------------------------------------===//
// Arena allocation operation
//===----------------------------------------------------------------------===//

def Hew_ArenaMallocOp : Hew_Op<"arena.malloc"> {
  let summary = "Allocate memory from the arena allocator.";
  let description = [{
    Allocates `size` bytes from the arena allocator.  Used for trait
    object boxing — the concrete value is copied onto the arena so that
    the fat pointer's data component has a stable address.

    Lowers to `hew_arena_malloc(size)`.

    Example:
    ```mlir
    %ptr = hew.arena.malloc %size : i64 -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnySignlessInteger:$size);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$size attr-dict `:` type($size) `->` type($result)";
}

//===----------------------------------------------------------------------===//
// Reference counting operations (closure environments)
//===----------------------------------------------------------------------===//

def Hew_RcNewOp : Hew_Op<"rc.new"> {
  let summary = "Allocate a new reference-counted block.";
  let description = [{
    Allocates a reference-counted memory block of the given size.
    `data` is an optional initial data pointer (null to just allocate),
    `size` is the byte count, and `drop_fn` is an optional destructor
    (null for no destructor).  Returns a pointer to the data region
    (refcount header is hidden before it).

    Lowers to `hew_rc_new(data, size, drop_fn)`.

    Example:
    ```mlir
    %ptr = hew.rc.new(%data, %size, %drop_fn)
             : (!llvm.ptr, i64, !llvm.ptr) -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$data, AnySignlessInteger:$size, AnyType:$drop_fn);
  let results = (outs AnyType:$result);

  let assemblyFormat = [{
    `(` $data `,` $size `,` $drop_fn `)` attr-dict
      `:` `(` type($data) `,` type($size) `,` type($drop_fn) `)` `->` type($result)
  }];
}

def Hew_RcCloneOp : Hew_Op<"rc.clone"> {
  let summary = "Increment the reference count of an Rc block.";
  let description = [{
    Atomically increments the reference count of the given Rc pointer.
    Returns the same pointer (for convenience).

    Lowers to `hew_rc_clone(ptr)`.

    Example:
    ```mlir
    %same = hew.rc.clone %ptr : !llvm.ptr -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$ptr);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$ptr attr-dict `:` type($ptr) `->` type($result)";
}

def Hew_RcDropOp : Hew_Op<"rc.drop"> {
  let summary = "Decrement the reference count of an Rc block.";
  let description = [{
    Atomically decrements the reference count.  If it reaches zero,
    the destructor (if any) is called and the memory is freed.

    Lowers to `hew_rc_drop(ptr)`.

    Example:
    ```mlir
    hew.rc.drop %ptr : !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$ptr);

  let assemblyFormat = "$ptr attr-dict `:` type($ptr)";
}

//===----------------------------------------------------------------------===//
// Scope operations — structured concurrency (task scopes + actor scopes)
//===----------------------------------------------------------------------===//

def Hew_ScopeCreateOp : Hew_Op<"scope.create"> {
  let summary = "Create a scope for structured concurrency.";
  let description = [{
    Creates both an actor scope (for spawned actors) and a task scope
    (for launched tasks). Returns two pointers: the actor scope and the
    task scope.

    Lowers to `hew_scope_create()` and `hew_task_scope_new()`.

    Example:
    ```mlir
    %actor_scope, %task_scope = hew.scope.create : (!llvm.ptr, !llvm.ptr)
    ```
  }];

  let results = (outs AnyType:$actor_scope, AnyType:$task_scope);

  let assemblyFormat = "attr-dict `:` `(` type($actor_scope) `,` type($task_scope) `)`";
}

def Hew_ScopeJoinOp : Hew_Op<"scope.join"> {
  let summary = "Join all tasks and wait for all actors in a scope.";
  let description = [{
    Waits for all launched tasks in the task scope to complete, then
    waits for all actors in the actor scope to finish.

    Lowers to `hew_task_scope_join_all(task_scope)` followed by
    `hew_scope_wait_all(actor_scope)`.

    Example:
    ```mlir
    hew.scope.join %actor_scope, %task_scope : (!llvm.ptr, !llvm.ptr)
    ```
  }];

  let arguments = (ins AnyType:$actor_scope, AnyType:$task_scope);

  let assemblyFormat = "$actor_scope `,` $task_scope attr-dict `:` `(` type($actor_scope) `,` type($task_scope) `)`";
}

def Hew_ScopeDestroyOp : Hew_Op<"scope.destroy"> {
  let summary = "Destroy a scope and free its resources.";
  let description = [{
    Destroys the task scope and frees the actor scope.

    Lowers to `hew_task_scope_destroy(task_scope)` followed by
    `hew_scope_free(actor_scope)`.

    Example:
    ```mlir
    hew.scope.destroy %actor_scope, %task_scope : (!llvm.ptr, !llvm.ptr)
    ```
  }];

  let arguments = (ins AnyType:$actor_scope, AnyType:$task_scope);

  let assemblyFormat = "$actor_scope `,` $task_scope attr-dict `:` `(` type($actor_scope) `,` type($task_scope) `)`";
}

def Hew_ScopeLaunchOp : Hew_Op<"scope.launch"> {
  let summary = "Launch a task on a new thread within a scope.";
  let description = [{
    Creates a new task handle, registers it with the task scope, and
    spawns it on a thread using the given function pointer.

    Lowers to `hew_task_new()`, `hew_task_scope_spawn(scope, task)`,
    and `hew_task_spawn_thread(task, fn_ptr)`.

    Returns the task handle pointer.

    Example:
    ```mlir
    %task = hew.scope.launch %task_scope, %fn_ptr : (!llvm.ptr, !llvm.ptr) -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$task_scope, AnyType:$fn_ptr);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$task_scope `,` $fn_ptr attr-dict `:` `(` type($task_scope) `,` type($fn_ptr) `)` `->` type($result)";
}

def Hew_ScopeAwaitOp : Hew_Op<"scope.await"> {
  let summary = "Await a launched task, blocking until it completes.";
  let description = [{
    Blocks the calling thread until the task completes, then returns
    a pointer to the task's result data.

    Lowers to `hew_task_await_blocking(task)`.

    Example:
    ```mlir
    %result_ptr = hew.scope.await %task : !llvm.ptr -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$task);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$task attr-dict `:` type($task) `->` type($result)";
}

def Hew_ScopeCancelOp : Hew_Op<"scope.cancel"> {
  let summary = "Cancel all tasks in the current task scope.";
  let description = [{
    Signals cancellation to all tasks in the scope.

    Lowers to `hew_task_scope_cancel(task_scope)`.

    Example:
    ```mlir
    hew.scope.cancel %task_scope : !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$task_scope);

  let assemblyFormat = "$task_scope attr-dict `:` type($task_scope)";
}


def Hew_TaskSetResultOp : Hew_Op<"task.set_result"> {
  let summary = "Store a result into a task handle.";
  let description = [{
    Stores a result pointer and size into a task handle so it can be
    retrieved by the awaiting thread.

    Lowers to `hew_task_set_result(task, result_ptr, size)`.

    Example:
    ```mlir
    hew.task.set_result %task, %ptr, %size : (!llvm.ptr, !llvm.ptr, i64)
    ```
  }];

  let arguments = (ins AnyType:$task, AnyType:$result_ptr, AnySignlessInteger:$size);

  let assemblyFormat = "$task `,` $result_ptr `,` $size attr-dict `:` `(` type($task) `,` type($result_ptr) `,` type($size) `)`";
}

def Hew_TaskCompleteOp : Hew_Op<"task.complete"> {
  let summary = "Mark a task as completed.";
  let description = [{
    Signals that a threaded task has finished execution.

    Lowers to `hew_task_complete_threaded(task)`.

    Example:
    ```mlir
    hew.task.complete %task : !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$task);

  let assemblyFormat = "$task attr-dict `:` type($task)";
}

//===----------------------------------------------------------------------===//
// Select operations — first-responder concurrency pattern
//===----------------------------------------------------------------------===//

def Hew_SelectCreateOp : Hew_Op<"select.create"> {
  let summary = "Create a reply channel for use in select or join.";
  let description = [{
    Creates a new reply channel that can receive a single async response.
    Used by both select and join expressions.

    Lowers to `hew_reply_channel_new()`.

    Example:
    ```mlir
    %ch = hew.select.create : !llvm.ptr
    ```
  }];

  let results = (outs AnyType:$result);

  let assemblyFormat = "attr-dict `:` type($result)";
}

def Hew_SelectAddOp : Hew_Op<"select.add"> {
  let summary = "Send an async ask to an actor with a reply channel.";
  let description = [{
    Sends a message to an actor with a reply channel attached. The actor
    will send its response to the channel. This is non-blocking.

    Lowers to `hew_actor_ask_with_channel(actor, msg_type, data, size, channel)`.

    Example:
    ```mlir
    hew.select.add %actor, %msg_type, %data, %size, %channel
        : (!llvm.ptr, i32, !llvm.ptr, i64, !llvm.ptr)
    ```
  }];

  let arguments = (ins
    AnyType:$actor,
    I32:$msg_type,
    AnyType:$data_ptr,
    AnySignlessInteger:$data_size,
    AnyType:$channel
  );

  let assemblyFormat = [{
    $actor `,` $msg_type `,` $data_ptr `,` $data_size `,` $channel attr-dict
      `:` `(` type($actor) `,` type($msg_type) `,` type($data_ptr) `,` type($data_size) `,` type($channel) `)`
  }];
}

def Hew_SelectFirstOp : Hew_Op<"select.first"> {
  let summary = "Wait for the first reply channel to receive a response.";
  let description = [{
    Polls an array of reply channels and returns the index of the first
    channel that receives a response within the timeout.

    Lowers to `hew_select_first(channels, count, timeout_ms)`.

    Example:
    ```mlir
    %idx = hew.select.first %channels, %count, %timeout
             : (!llvm.ptr, i32, i32) -> i32
    ```
  }];

  let arguments = (ins AnyType:$channels, I32:$count, I32:$timeout_ms);
  let results = (outs I32:$result);

  let assemblyFormat = [{
    $channels `,` $count `,` $timeout_ms attr-dict
      `:` `(` type($channels) `,` type($count) `,` type($timeout_ms) `)`
  }];
}

def Hew_SelectDestroyOp : Hew_Op<"select.destroy"> {
  let summary = "Free a reply channel.";
  let description = [{
    Frees a reply channel that is no longer needed.

    Lowers to `hew_reply_channel_free(channel)`.

    Example:
    ```mlir
    hew.select.destroy %channel : !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$channel);

  let assemblyFormat = "$channel attr-dict `:` type($channel)";
}

def Hew_SelectWaitOp : Hew_Op<"select.wait"> {
  let summary = "Wait for a reply on a channel and return the result pointer.";
  let description = [{
    Blocks until a reply arrives on the channel, then returns a pointer
    to the reply data. Used by both select (for the winning arm) and
    join (for each channel).

    Lowers to `hew_reply_wait(channel)`.

    Example:
    ```mlir
    %ptr = hew.select.wait %channel : !llvm.ptr -> !llvm.ptr
    ```
  }];

  let arguments = (ins AnyType:$channel);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$channel attr-dict `:` type($channel) `->` type($result)";
}

//===----------------------------------------------------------------------===//
// hew.drop — Drop (free/cleanup) a resource
//===----------------------------------------------------------------------===//

def Hew_DropOp : Hew_Op<"drop"> {
  let summary = "Drop a resource by calling its drop function.";
  let description = [{
    The `hew.drop` operation frees or cleans up a resource by calling the
    named drop function with the given pointer operand. This replaces
    inline `func.call` emission for resource cleanup, allowing the compiler
    to reason about resource lifetimes at the Hew dialect level.

    The `drop_fn` string attribute names the function to call (e.g.,
    `"hew_rc_drop"`, `"hew_vec_free"`, or a user-defined drop function).
    The `is_user_drop` boolean attribute distinguishes user-defined drops
    from runtime drops; both lower to `func.call @drop_fn(ptr)`.

    Example:
    ```mlir
    hew.drop %ptr {drop_fn = "hew_rc_drop"} : !llvm.ptr
    hew.drop %ptr {drop_fn = "MyStruct_drop", is_user_drop = true} : !llvm.ptr
    ```
  }];

  let arguments = (ins
    AnyType:$value,
    StrAttr:$drop_fn,
    DefaultValuedAttr<BoolAttr, "false">:$is_user_drop
  );

  let assemblyFormat = "$value attr-dict `:` type($value)";
}

//===----------------------------------------------------------------------===//
// hew.func_ptr — Get a function pointer from a symbol
//===----------------------------------------------------------------------===//

def Hew_FuncPtrOp : Hew_Op<"func_ptr", [Pure]> {
  let summary = "Get a pointer to a named function.";
  let description = [{
    The `hew.func_ptr` operation takes a function symbol reference and
    produces an `!llvm.ptr` value suitable for passing to runtime functions
    that expect function pointers (dispatch callbacks, coalesce key functions,
    closure function pointers, task entry points, etc.).

    This replaces the `func.constant` + `UnrealizedConversionCastOp` pattern
    that previously bridged between the `func` dialect's typed function
    references and the LLVM pointer type needed by the runtime ABI.

    During lowering, this expands to `llvm.mlir.addressof @symbol` which
    directly produces the function's address as `!llvm.ptr`.

    Example:
    ```mlir
    %fn = hew.func_ptr @Counter_dispatch : !llvm.ptr
    ```
  }];

  let arguments = (ins FlatSymbolRefAttr:$func_name);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$func_name attr-dict `:` type($result)";
}

//===----------------------------------------------------------------------===//
// hew.bitcast — Type-erased cast between Hew types and LLVM types
//===----------------------------------------------------------------------===//

def Hew_BitcastOp : Hew_Op<"bitcast", [Pure]> {
  let summary = "Bitcast between Hew dialect types and LLVM types.";
  let description = [{
    The `hew.bitcast` operation performs a type-erased cast between Hew
    dialect types (like `!hew.actor_ref`, `!hew.string_ref`, `!hew.vec<T>`)
    and their LLVM representations (typically `!llvm.ptr`), or between
    other type pairs that are layout-compatible but differ in dialect type.

    This is intentional type punning for FFI boundaries — Hew dialect types
    carry semantic information (element types, actor names, etc.) but have
    the same runtime representation as `!llvm.ptr`.

    During lowering, this is an identity operation: the input value is
    used directly as the output since both types have the same LLVM
    representation after type conversion.

    Example:
    ```mlir
    // Cast actor ref to pointer for runtime call
    %ptr = hew.bitcast %actor : !hew.actor_ref to !llvm.ptr

    // Cast pointer back to Hew type after loading from memory
    %ref = hew.bitcast %ptr : !llvm.ptr to !hew.actor_ref

    // Cast between Hew array and LLVM array for dynamic indexing
    %arr = hew.bitcast %hew_arr : !hew.array<i32, 4> to !llvm.array<4 x i32>
    ```
  }];

  let arguments = (ins AnyType:$input);
  let results = (outs AnyType:$result);

  let assemblyFormat = "$input attr-dict `:` type($input) `to` type($result)";
}

//===----------------------------------------------------------------------===//
// Assert operations
//===----------------------------------------------------------------------===//

def Hew_AssertOp : Hew_Op<"assert"> {
  let summary = "Assert a condition is truthy, abort if false.";
  let description = [{
    Asserts that the condition is truthy. If the condition is 0/false,
    the runtime aborts. The condition is coerced to i64 during lowering.

    Lowers to `hew_assert(cond_i64)`.

    Example:
    ```mlir
    hew.assert %cond : i1
    ```
  }];

  let arguments = (ins AnyType:$condition);
  let results = (outs);

  let assemblyFormat = [{
    $condition attr-dict `:` type($condition)
  }];
}

def Hew_AssertEqOp : Hew_Op<"assert_eq"> {
  let summary = "Assert two values are equal, abort if not.";
  let description = [{
    Compares `left` and `right` for equality. Aborts if they differ.
    Type-polymorphic: lowers to `hew_assert_eq_i64`, `hew_assert_eq_f64`,
    `hew_assert_eq_str`, or `hew_assert_eq_bool` based on operand types.

    Example:
    ```mlir
    hew.assert_eq %a, %b : i64, i64
    ```
  }];

  let arguments = (ins AnyType:$left, AnyType:$right);
  let results = (outs);

  let assemblyFormat = [{
    $left `,` $right attr-dict `:` type($left) `,` type($right)
  }];
  let hasVerifier = 1;
}

def Hew_AssertNeOp : Hew_Op<"assert_ne"> {
  let summary = "Assert two values are not equal, abort if they are.";
  let description = [{
    Compares `left` and `right` for inequality. Aborts if they are equal.
    Type-polymorphic: lowers to `hew_assert_ne_i64`, `hew_assert_ne_f64`,
    `hew_assert_ne_str`, or `hew_assert_ne_bool` based on operand types.

    Example:
    ```mlir
    hew.assert_ne %a, %b : i64, i64
    ```
  }];

  let arguments = (ins AnyType:$left, AnyType:$right);
  let results = (outs);

  let assemblyFormat = [{
    $left `,` $right attr-dict `:` type($left) `,` type($right)
  }];
  let hasVerifier = 1;
}

#endif // HEW_OPS
