#[cfg(unix)]
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
#[cfg(unix)]
use std::process::Output;
use std::sync::OnceLock;

#[cfg(unix)]
use assert_cmd::Command;
#[cfg(unix)]
use hew_wasm::sandbox::{compile_to_sandbox_bytecode_with_inputs, Diagnostic};

#[cfg(unix)]
const SANDBOX_PROFILE: &str = "sandbox-vm-export";
#[cfg(unix)]
const HEW_SEED: &str = "42";

const PARITY_CASES: &[ParityCase] = &[
    ParityCase {
        test_name: "actor_reference_equality",
        source_rel: "tests/core-acceptance/cases/actor-reference-equality.hew",
    },
    ParityCase {
        test_name: "actor_same_type_completion_chain",
        source_rel: "tests/core-acceptance/cases/actor-same-type-completion-chain.hew",
    },
    ParityCase {
        test_name: "actor_same_type_completion_ring",
        source_rel: "tests/core-acceptance/cases/actor-same-type-completion-ring.hew",
    },
    ParityCase {
        test_name: "indexed_field_write_in_place",
        source_rel: "tests/core-acceptance/cases/indexed-field-write-in-place.hew",
    },
    ParityCase {
        test_name: "recursive_structural_equality",
        source_rel: "tests/core-acceptance/cases/recursive-structural-equality.hew",
    },
    ParityCase {
        test_name: "lambda_actor_private_capture",
        source_rel: "tests/core-acceptance/cases/lambda-actor-private-capture.hew",
    },
    ParityCase {
        test_name: "task_race",
        source_rel: "examples/sandbox-graduation/task_race.hew",
    },
    ParityCase {
        // A lambda built, passed to a function taking a callable, and called
        // there through the same indirect path both engines use.
        test_name: "closure_values",
        source_rel: "examples/sandbox-graduation/closure_values.hew",
    },
    ParityCase {
        // Named arguments bind by parameter name and evaluate as written, on
        // functions, methods, a trait object and an actor handler.
        test_name: "named_arguments",
        source_rel: "examples/sandbox-graduation/named_arguments.hew",
    },
    ParityCase {
        // A spawn, supervisor child and transition head bind keys by name in
        // braces, with defaults, init and supervisor parameters, while a call
        // with the same bare names stays positional.
        test_name: "spawn_keyed",
        source_rel: "tests/core-acceptance/cases/spawn-keyed.hew",
    },
    ParityCase {
        // A transition head binding read by the `when` guard and the body,
        // across plain, wildcard-source, generic and composite-group rules.
        test_name: "machine_guard_head_binding",
        source_rel: "tests/core-acceptance/cases/machine-guard-head-binding.hew",
    },
    ParityCase {
        // `stopped` on an actor that already crashed returns in both
        // executors; the crash alone keeps the exit status failing.
        test_name: "actor_stopped_after_crash",
        source_rel: "tests/core-acceptance/cases/actor-stopped-after-crash.hew",
    },
    ParityCase {
        // Deferred work runs as the scope ends, in reverse registration order.
        test_name: "defer_order",
        source_rel: "examples/sandbox-graduation/defer_order.hew",
    },
    ParityCase {
        // Result is the prelude's own enum: constructing and matching one is a
        // variant, not a host call.
        test_name: "result_constructors",
        source_rel: "examples/sandbox-graduation/result_constructors.hew",
    },
    ParityCase {
        // A present key, a missing key and a membership test.
        test_name: "map_reads",
        source_rel: "examples/sandbox-graduation/map_reads.hew",
    },
    ParityCase {
        // A map literal's storage and lookup, which the VM now models with a
        // value kind of its own rather than approximating with a record.
        test_name: "map_literal",
        source_rel: "examples/sandbox-graduation/map_literal.hew",
    },
    ParityCase {
        // The pure `std.math` intrinsics. Native emits the LLVM intrinsic, so
        // a shim that rounds differently shows up here rather than in a lesson.
        test_name: "math_intrinsics",
        source_rel: "examples/sandbox-graduation/math_intrinsics.hew",
    },
    ParityCase {
        // Selected component predicates preserve lexicographic ordering and
        // IEEE incomparability in both physical and VM value representations.
        test_name: "derived_ordering",
        source_rel: "tests/vertical-slice/accept/derived_ord_lowering.hew",
    },
    ParityCase {
        // The f64 classification and bit methods, and the std.encoding.binary
        // f32 codecs that refuse non-finite input through them.
        test_name: "float_classification",
        source_rel: "examples/sandbox-graduation/float_classification.hew",
    },
    ParityCase {
        // Integer bit methods and wrapping, saturating and checked arithmetic
        // at narrow, signed and unsigned receiver widths.
        test_name: "integer_bit_methods",
        source_rel: "examples/sandbox-graduation/integer_bit_methods.hew",
    },
    ParityCase {
        // Dynamic dispatch through a trait object: the checker picks the slot
        // and the package carries the table, so neither engine resolves the
        // method by name.
        test_name: "trait_objects",
        source_rel: "examples/sandbox-graduation/trait_objects.hew",
    },
    ParityCase {
        // A sleep advances the virtual clock and the program continues, so the
        // sandbox observes what native observes without waiting for it.
        test_name: "virtual_sleep",
        source_rel: "examples/sandbox-graduation/virtual_sleep.hew",
    },
    ParityCase {
        // A seeded generator: the VM runs the runtime's own MT19937, so the
        // same seed yields the same sequence on both engines.
        test_name: "seeded_random",
        source_rel: "examples/sandbox-graduation/seeded_random.hew",
    },
    ParityCase {
        // A closure fails through a declared or inferred edge, exactly as a
        // function does.
        test_name: "closure_edges",
        source_rel: "examples/sandbox-graduation/closure_edges.hew",
    },
    ParityCase {
        // `impl Error` alone supplies an error type's Display, so both engines
        // print the same implied text.
        test_name: "error_display",
        source_rel: "examples/sandbox-graduation/error_display.hew",
    },
    ParityCase {
        // A failure edge wraps an error in the one variant carrying it alone.
        test_name: "variant_edges",
        source_rel: "examples/sandbox-graduation/variant_edges.hew",
    },
    ParityCase {
        // A failing generator yields `Result` items and ends on its error.
        test_name: "failing_generators",
        source_rel: "examples/sandbox-graduation/failing_generators.hew",
    },
    ParityCase {
        // `fails E` is the failure edge and `-> Result` a value of one type.
        test_name: "failure_edges",
        source_rel: "examples/sandbox-graduation/failure_edges.hew",
    },
    ParityCase {
        // Failure edges: `?` and `return error` keep the same error, apply a
        // declared `From` and erase into `dyn Error`.
        test_name: "error_edges",
        source_rel: "examples/sandbox-graduation/error_edges.hew",
    },
    ParityCase {
        test_name: "hello_world",
        source_rel: "examples/playground/basics/hello_world.hew",
    },
    ParityCase {
        test_name: "fibonacci",
        source_rel: "examples/playground/basics/fibonacci.hew",
    },
    ParityCase {
        // Float add/sub/mul/neg through the type-directed f64.* opcode family.
        test_name: "float_arithmetic",
        source_rel: "examples/playground/basics/float_arithmetic.hew",
    },
    ParityCase {
        // Each f32 operation rounds immediately. Carrying the chain as f64 and
        // rounding only at the final cast would incorrectly print 16777218.
        test_name: "f32_arithmetic_precision",
        source_rel: "examples/sandbox-graduation/f32_arithmetic_precision.hew",
    },
    ParityCase {
        // Every bytes operation, including ranges that share the buffer.
        test_name: "bytes_ops",
        source_rel: "examples/sandbox-graduation/bytes_ops.hew",
    },
    ParityCase {
        // A store past the end faults and unwinds with the buffer intact.
        test_name: "bytes_set_bounds",
        source_rel: "examples/sandbox-graduation/bytes_set_bounds.hew",
    },
    ParityCase {
        // The same fault and unwind for a vector element store.
        test_name: "vec_set_bounds",
        source_rel: "examples/sandbox-graduation/vec_set_bounds.hew",
    },
    ParityCase {
        // A slice past the end faults on both engines instead of clamping.
        test_name: "slice_bounds",
        source_rel: "examples/sandbox-graduation/slice_bounds.hew",
    },
    ParityCase {
        // Float division and remainder (IEEE-754, never trap-on-zero).
        test_name: "float_division",
        source_rel: "examples/playground/basics/float_division.hew",
    },
    ParityCase {
        // Non-finite f64 equality: NaN never equal, ±Infinity compare by sign,
        // -0.0 == 0.0. `==` mirrors native `fcmp OEQ` and `!=` mirrors `fcmp UNE`
        // (true whenever a NaN operand is present), proving the sandbox no longer
        // collapses NaN/±Infinity through the canonical-JSON path.
        test_name: "float_nonfinite_compare",
        source_rel: "examples/playground/basics/float_nonfinite_compare.hew",
    },
    ParityCase {
        // Integer (checked i64.*) and float (f64.*) arithmetic in one program,
        // proving the emitter dispatches the opcode family per operand type.
        test_name: "mixed_numeric",
        source_rel: "examples/playground/basics/mixed_numeric.hew",
    },
    ParityCase {
        test_name: "function_composition",
        source_rel: "examples/playground/basics/higher_order_functions.hew",
    },
    ParityCase {
        test_name: "pattern_matching",
        source_rel: "examples/playground/types/pattern_matching.hew",
    },
    ParityCase {
        test_name: "collections",
        source_rel: "examples/playground/types/collections.hew",
    },
    ParityCase {
        test_name: "record_types",
        source_rel: "examples/playground/types/record_types.hew",
    },
    ParityCase {
        test_name: "structural_records",
        source_rel: "examples/playground/types/structural_bounds.hew",
    },
    ParityCase {
        test_name: "counter_actor",
        source_rel: "examples/playground/concurrency/counter_actor.hew",
        // Actor/supervisor/machine support now emits bytecode; no profile divergence.
    },
    ParityCase {
        test_name: "actor_pipeline",
        source_rel: "examples/playground/concurrency/actor_pipeline.hew",
        // Actor support now emits bytecode; no profile divergence.
    },
    ParityCase {
        test_name: "supervisor",
        source_rel: "examples/playground/concurrency/supervisor.hew",
        // Supervisor source rewritten to not use supervisor_stop (native-only);
        // the supervisor decl, spawn, and child method calls are all sandbox-admitted.
    },
    ParityCase {
        test_name: "traffic_light",
        source_rel: "examples/playground/machines/traffic_light.hew",
        // Machine support now emits bytecode; no profile divergence.
    },
    ParityCase {
        test_name: "stmt_if",
        source_rel: "examples/playground/basics/stmt_if.hew",
    },
    ParityCase {
        test_name: "stmt_match",
        source_rel: "examples/playground/basics/stmt_match.hew",
    },
    ParityCase {
        test_name: "stmt_if_let",
        source_rel: "examples/playground/basics/stmt_if_let.hew",
    },
    ParityCase {
        // Value-position if-let (`let v = if let Value(n) = w { n } else { d }`):
        // the matched arm value is joined on a result local, so the expression
        // yields the matched value, not unit. Proves the value-position lowering.
        test_name: "if_let_value",
        source_rel: "examples/playground/basics/if_let_value.hew",
    },
    ParityCase {
        test_name: "arithmetic_operators",
        source_rel: "examples/playground/language/arithmetic_operators.hew",
        // Integer +,-,*,/,%, unary negate, and all six comparisons.
    },
    ParityCase {
        test_name: "array_indexing",
        source_rel: "examples/playground/language/array_indexing.hew",
        // Array literal, index read, `.len()`, and range-for over the length.
    },
    ParityCase {
        test_name: "string_slicing",
        source_rel: "examples/playground/language/string_slicing.hew",
        // String `.len()` and `.slice(start, end)`.
    },
    ParityCase {
        test_name: "while_loop",
        source_rel: "examples/playground/language/while_loop.hew",
        // `while` loop with a mutable accumulator.
    },
    ParityCase {
        test_name: "wildcard_match",
        source_rel: "examples/playground/language/wildcard_match.hew",
        // Enum dispatch with a catch-all `_` arm.
    },
    ParityCase {
        test_name: "match_guard_parity",
        source_rel: "examples/playground/language/match_guard.hew",
    },
    ParityCase {
        test_name: "match_guard_catch_all_fallthrough",
        source_rel: "examples/enums/match_guard_catch_all_fallthrough.hew",
    },
    ParityCase {
        // Fieldless-enum `==`/`!=`: `BinaryOp::Equal`/`NotEqual` on a fieldless
        // enum emits `cmp.eq`/`cmp.ne`; the VM's `compare` handler uses
        // `canonicalComparable` which serialises `{ type, tag, payload: [] }` to
        // JSON, making same-tag variants equal and different-tag variants unequal.
        // Admitted by the checker in #1987. Source lives outside the curated
        // playground set so the playground manifest is not affected.
        test_name: "fieldless_enum_eq",
        source_rel: "examples/enums/run_colour_eq.hew",
    },
    ParityCase {
        // Structural `==`/`!=` on records (user-defined struct types) and
        // payload enums (enums with data-carrying variants). The profile now
        // admits these; `lower_binary` already emits `cmp.eq`/`cmp.ne` for all
        // types; the VM's `canonicalComparable` serialises record fields and
        // enum payloads recursively to a canonical JSON string, giving
        // structural field-by-field equality that mirrors native Hew semantics.
        test_name: "record_equality",
        source_rel: "examples/playground/types/record_equality.hew",
    },
    ParityCase {
        // `clone expr` produces an independent deep copy. The emitter lowers
        // `Expr::Clone` by evaluating the operand then emitting `local.set`
        // into a fresh temp, which calls `cloneValue` in the VM — a deep
        // recursive copy. Proves vector aliasing safety: push to the original
        // does not affect the clone.
        test_name: "clone_value",
        source_rel: "examples/playground/basics/clone_value.hew",
    },
    ParityCase {
        // Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`) for both i64 and
        // f64. The emitter reads the current binding, applies the type-directed
        // opcode family (i64.checked_* or f64.*), and writes the result back.
        test_name: "compound_assign",
        source_rel: "examples/playground/language/compound_assign.hew",
    },
    ParityCase {
        // Non-finite f64 values (inf, -inf, nan) render identically to native.
        // Native uses printf("%g") which produces lowercase `inf`, `-inf`, `nan`;
        // the sandbox VM's renderF64 must match exactly — not JavaScript's
        // `String(Infinity)` which produces `Infinity` / `NaN` (capitalised).
        test_name: "f64_nonfinite_render",
        source_rel: "examples/playground/language/f64_nonfinite_render.hew",
    },
    ParityCase {
        // Finite f64 values render identically to native printf("%g"): negative
        // zero prints as `-0`, large/small values use scientific notation at the
        // %g thresholds (exp < -4 or >= 6), and 6 significant digits are used
        // with trailing zeros removed. JavaScript's `String()` uses Ryu
        // shortest-round-trip digits and different notation thresholds, so the
        // sandbox VM's renderF64 must implement %g-equivalent logic rather than
        // delegating to `String()` for finite values.
        test_name: "f64_finite_render",
        source_rel: "examples/playground/language/f64_finite_render.hew",
    },
    ParityCase {
        // Tuples lowered as anonymous records with positional fields _0, _1, …
        // so record.new / record.get handle construction and let-destructure.
        test_name: "tuple_values",
        source_rel: "examples/playground/types/tuple_values.hew",
    },
    ParityCase {
        // `==` on generic records and aggregates containing Vec<T>: the checker
        // now resolves equality eligibility after substituting the generic
        // arguments, so Pair<i64, string> compares structurally. The emitter
        // already routes `==` through `cmp.eq`; the VM's `canonicalComparable`
        // handles records, enums, and vectors recursively.
        test_name: "generic_aggregate_eq",
        source_rel: "examples/playground/types/generic_aggregate_eq.hew",
    },
    ParityCase {
        // Option/Result predicates and extractors run their std source bodies
        // on both engines.
        test_name: "option_result_methods",
        source_rel: "examples/playground/types/option_result_methods.hew",
    },
    ParityCase {
        // f-string interpolation (`{x}`) for integer and char types that gained
        // Display impls: i8/i16/i32, u8/u16/u32, u64, isize, usize, char. The
        // VM stores all integers as i64/u64 BigInt and renders them via
        // renderStdout, so the interpolated output matches native printf output.
        test_name: "display_scalars",
        source_rel: "examples/playground/basics/display_scalars.hew",
    },
    ParityCase {
        // This suite compares against host-native `hew run`, whose supported
        // targets are 64-bit. A wasm32 build would truncate both values to zero.
        test_name: "pointer_width_native64",
        source_rel: "examples/sandbox-graduation/pointer_width_native64.hew",
    },
    ParityCase {
        // `#[wire]` is now the sole canonical declaration surface for wire types
        // (bare `wire`/`wire type`/`wire enum` keyword forms removed in 60c50dae;
        // the `struct` keyword itself later removed in favour of `#[wire] type`).
        // Verifies the sandbox profile and emitter treat a `#[wire] type` with
        // tagged fields as a plain record without rejecting the attribute or the
        // optional-field tag annotation.
        test_name: "wire_types_declaration",
        source_rel: "examples/playground/types/wire_types.hew",
    },
    ParityCase {
        // Vec<T>::contains (linear equality scan via canonical comparison) and
        // v[start..end] (exclusive range slice) map to new sandbox VM opcodes
        // `vector.contains` / `vector.range_slice` added in this parity sweep.
        test_name: "vec_operations",
        source_rel: "examples/playground/types/vec_operations.hew",
    },
    ParityCase {
        // v[start..=end] inclusive range slice. The emitter computes the
        // exclusive end by adding 1 and delegates to the existing
        // `vector.range_slice` opcode. Pins end-inclusive vs end-exclusive
        // semantics against native `hew run`.
        test_name: "vec_inclusive_slice",
        source_rel: "examples/playground/types/vec_inclusive_slice.hew",
    },
    ParityCase {
        // `rec.clone()` method on a user-defined record type. The emitter lowers
        // it via `local.set` which calls `cloneValue` in the VM — a deep
        // recursive copy — so the clone and the original are independent
        // objects with no aliased fields, matching native structural copy semantics.
        test_name: "record_clone",
        source_rel: "examples/playground/types/record_clone.hew",
    },
    ParityCase {
        // `(rec.f)(args)` fn-field call. The emitter materialises the function
        // value stored in the record field via `const.function` / `record.get`
        // and invokes it via `call.indirect`. Pins the indirect-call routing
        // against direct `call.direct` results for the same function.
        test_name: "fn_field_call",
        source_rel: "examples/playground/types/fn_field_call.hew",
    },
    ParityCase {
        // Vec<f64>::contains with NaN and +-Infinity: the sandbox VM now routes
        // element equality through valuesEqual (which uses JS === for f64 pairs),
        // matching native fcmp-OEQ semantics.  Pre-fix the VM collapsed NaN and
        // +-Infinity to JSON null so [NaN].contains(NaN) returned true instead
        // of false -- a silent wrong-result divergence.
        test_name: "vec_f64_nonfinite_contains",
        source_rel: "examples/playground/types/vec_f64_nonfinite_contains.hew",
    },
    ParityCase {
        test_name: "bool_not",
        source_rel: "examples/sandbox-graduation/bool_not.hew",
    },
    ParityCase {
        test_name: "scalar_match_int",
        source_rel: "examples/sandbox-graduation/scalar_match_int.hew",
    },
    ParityCase {
        test_name: "scalar_match_string",
        source_rel: "examples/sandbox-graduation/scalar_match_string.hew",
    },
    ParityCase {
        test_name: "bool_match",
        source_rel: "examples/sandbox-graduation/bool_match.hew",
    },
    ParityCase {
        test_name: "struct_functional_update",
        source_rel: "examples/sandbox-graduation/struct_functional_update.hew",
    },
    ParityCase {
        test_name: "struct_pattern_match",
        source_rel: "examples/sandbox-graduation/struct_pattern_match.hew",
    },
    ParityCase {
        test_name: "option_some_none",
        source_rel: "examples/sandbox-graduation/option_some_none.hew",
    },
    ParityCase {
        // A failing `var self` method hands its receiver back, from a local
        // and from a plain field beneath a capture: each defer, including one
        // in the method, sees the value as last written on both engines.
        test_name: "var_self_fault_defer",
        source_rel: "examples/sandbox-graduation/var_self_fault_defer.hew",
    },
    ParityCase {
        // `take()` is a `var self` std method: the value leaves the place and
        // `None` stays behind on both engines.
        test_name: "option_take",
        source_rel: "examples/sandbox-graduation/option_take.hew",
    },
    ParityCase {
        // Option, Result and iterator methods are std source bodies every
        // program sees without an import, chained through closures.
        test_name: "fluent_method_chains",
        source_rel: "examples/sandbox-graduation/fluent_method_chains.hew",
    },
    ParityCase {
        // Pushes, an indexed write and a field assignment on a `#[resource]`
        // record, held in a variable and in a closure's capture, take the
        // record apart around the field and rebuild it in place.
        test_name: "resource_field_collections",
        source_rel: "examples/sandbox-graduation/resource_field_collections.hew",
    },
    ParityCase {
        // Resources declared outside a loop and consumed inside it: whole and
        // field re-initialisation each iteration, and a consume that breaks.
        test_name: "loop_consume_reinitialized",
        source_rel: "examples/sandbox-graduation/loop_consume_reinitialized.hew",
    },
    ParityCase {
        // Map and set inserts and removes on a closure's mutable capture
        // update the closure's own collection across calls on both engines.
        test_name: "capture_collections",
        source_rel: "examples/sandbox-graduation/capture_collections.hew",
    },
    ParityCase {
        test_name: "const_reference",
        source_rel: "examples/sandbox-graduation/const_reference.hew",
    },
    ParityCase {
        test_name: "logical_binary_operators",
        source_rel: "examples/sandbox-graduation/logical_binary_operators.hew",
    },
    ParityCase {
        test_name: "bitwise_binary_operators",
        source_rel: "examples/sandbox-graduation/bitwise_binary_operators.hew",
    },
    ParityCase {
        // `&`, `|` and `^` on bool evaluate both operands on both engines.
        test_name: "bool_bitwise",
        source_rel: "examples/sandbox-graduation/bool_bitwise.hew",
    },
    ParityCase {
        test_name: "compound_bitwise_assign",
        source_rel: "examples/sandbox-graduation/compound_bitwise_assign.hew",
    },
    ParityCase {
        test_name: "shift_out_of_range",
        source_rel: "examples/sandbox-graduation/shift_out_of_range.hew",
    },
    ParityCase {
        test_name: "struct_destructure_let",
        source_rel: "examples/sandbox-graduation/struct_destructure_let.hew",
    },
    ParityCase {
        test_name: "record_shorthand_destructure_let",
        source_rel: "examples/sandbox-graduation/record_shorthand_destructure_let.hew",
    },
    ParityCase {
        test_name: "nested_tuple_destructure_let",
        source_rel: "examples/sandbox-graduation/nested_tuple_destructure_let.hew",
    },
    ParityCase {
        // Wrapping ops at the overflow boundary: the VM truncates `&+`/`&-`/`&*`
        // to two's-complement 64 bits to match native (#2341).
        test_name: "wrapping_binary_operators",
        source_rel: "examples/sandbox-graduation/wrapping_binary_operators.hew",
    },
    ParityCase {
        // `.clone()` method-call syntax on Vec/String/Array/Slice. The
        // emitter's generic clone arm (`local.set` → `cloneValue`) already
        // handled these receiver types; the profile allowlist was the gap.
        // Proves independence: mutating the original after clone leaves the
        // copy unaffected, for every newly-admitted receiver type.
        test_name: "method_clone",
        source_rel: "examples/playground/types/method_clone.hew",
    },
    ParityCase {
        // `regex.Pattern::clone()` — same profile-allowlist gap as
        // `method_clone`, covered by a source file outside the curated
        // playground manifest scope: `std::text::regex` FFI is unresolvable
        // by the hew-wasm browser analyzer (dev-tooling world, distinct from
        // this sandbox VM), so a regex-importing fixture cannot live under
        // `examples/playground/` without failing the analyzer smoke test.
        test_name: "regex_clone",
        source_rel: "examples/sandbox-graduation/regex_clone.hew",
    },
    ParityCase {
        // Array repeat evaluates value then count exactly once and clones the
        // value into every slot; casts cover width/sign/saturation/bool/char;
        // postfix-try covers Result and Option success plus early propagation.
        test_name: "trap_residual",
        source_rel: "examples/sandbox-graduation/trap_residual.hew",
    },
    ParityCase {
        test_name: "deferred_locals",
        source_rel: "examples/sandbox-graduation/deferred_locals.hew",
    },
    ParityCase {
        // `f"{v:?}"` over scalars, strings, tuples, records, enums, Vec,
        // HashMap and an `impl Display` override, reusing the pinned
        // core-acceptance fixture rather than a second copy (#3516).
        test_name: "structural_rendering",
        source_rel: "tests/core-acceptance/cases/structural-rendering.hew",
    },
    ParityCase {
        // Every method of a multi-bound trait object reaches its own body
        // through the one slot layout, reusing the core-acceptance fixture.
        test_name: "dyn_multibound_dispatch",
        source_rel: "tests/core-acceptance/cases/dyn-multibound-dispatch.hew",
    },
    ParityCase {
        // A user trait extending `Display` reaches `Display.fmt` through its
        // trait object, reusing the core-acceptance fixture.
        test_name: "dyn_subtrait_display",
        source_rel: "tests/core-acceptance/cases/dyn-subtrait-display.hew",
    },
    ParityCase {
        // A fallible `main` that returns `Err` writes its rendered `error:`
        // line to stderr and exits 1 in both executors.
        test_name: "entry_result_err",
        source_rel: "tests/core-acceptance/cases/entry-result-err.hew",
    },
    ParityCase {
        // `?` on a `fails` call ends `main` through the same adapter.
        test_name: "error_trait_fallible_main",
        source_rel: "tests/core-acceptance/cases/error-trait-fallible-main.hew",
    },
    ParityCase {
        // Fault exits of one body share cleanup rungs: every exit of a body
        // with a closable binding and two asks closes it once.
        test_name: "cleanup_ladder_shared_exits",
        source_rel: "tests/core-acceptance/cases/cleanup-ladder-shared-exits.hew",
    },
    ParityCase {
        // A shared defer rung reads mutable bindings as each exit left them.
        test_name: "cleanup_ladder_defer_reads",
        source_rel: "tests/core-acceptance/cases/cleanup-ladder-defer-reads.hew",
    },
    ParityCase {
        // A failing close on a shared fault rung is reported after the ask's
        // fault, once every remaining owner has been released.
        test_name: "cleanup_ladder_fault_combine",
        source_rel: "tests/core-acceptance/cases/cleanup-ladder-fault-combine.hew",
    },
];

#[derive(Debug, Clone, Copy)]
struct ParityCase {
    #[cfg_attr(
        not(unix),
        expect(dead_code, reason = "read only by the unix-only VM parity tests")
    )]
    test_name: &'static str,
    source_rel: &'static str,
}

#[cfg(unix)]
#[derive(serde::Deserialize)]
struct CaseEnvironment {
    source: PathBuf,
    #[serde(default)]
    env: BTreeMap<String, String>,
}

#[cfg(unix)]
fn load_case_environments(root: &Path) -> Vec<CaseEnvironment> {
    #[derive(serde::Deserialize)]
    struct Manifest {
        #[serde(rename = "case")]
        cases: Vec<CaseEnvironment>,
    }

    let mut paths: Vec<_> = std::fs::read_dir(root.join("cases"))
        .expect("read core acceptance manifests")
        .map(|entry| entry.expect("core acceptance manifest entry").path())
        .filter(|path| {
            path.extension()
                .is_some_and(|extension| extension == "toml")
        })
        .collect();
    paths.sort();
    let mut environments = Vec::new();
    for path in paths {
        let text = std::fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("read {}: {err}", path.display()));
        let manifest: Manifest =
            toml::from_str(&text).unwrap_or_else(|err| panic!("parse {}: {err}", path.display()));
        for mut case in manifest.cases {
            case.source = root
                .join(&case.source)
                .canonicalize()
                .unwrap_or_else(|err| panic!("resolve source in {}: {err}", path.display()));
            environments.push(case);
        }
    }
    environments
}

#[cfg(unix)]
fn environment_for_source(
    cases: &[CaseEnvironment],
    source_path: &Path,
) -> BTreeMap<String, String> {
    let source = source_path.canonicalize().expect("resolve parity source");
    let mut matching = cases.iter().filter(|case| case.source == source);
    let Some(first) = matching.next() else {
        return BTreeMap::new();
    };
    for other in matching {
        assert_eq!(
            first.env,
            other.env,
            "{} has conflicting core acceptance environments",
            source.display()
        );
    }
    first.env.clone()
}

#[cfg(unix)]
fn fixture_environment(source_path: &Path) -> BTreeMap<String, String> {
    static ENVIRONMENTS: OnceLock<Vec<CaseEnvironment>> = OnceLock::new();
    environment_for_source(
        ENVIRONMENTS
            .get_or_init(|| load_case_environments(&repo_root().join("tests/core-acceptance"))),
        source_path,
    )
}

#[cfg(unix)]
#[test]
fn manifest_environment_reaches_native_execution_and_preserves_unconfigured_values() {
    ensure_native_toolchain();
    let root = tempfile::tempdir().expect("environment contract fixture");
    std::fs::create_dir(root.path().join("cases")).unwrap();
    let source = r#"import std.os;
fn main() {
    match os.env("HEW_WORKERS") { .Some(value) => println(value), .None => println("absent"), }
    match os.env("HEW_SEED") { .Some(value) => println(value), .None => println("absent"), }
}
"#;
    for name in ["configured", "unconfigured", "unregistered"] {
        std::fs::write(root.path().join(format!("{name}.hew")), source).unwrap();
    }
    std::fs::write(
        root.path().join("cases/unconfigured.toml"),
        "[[case]]\nsource = \"unconfigured.hew\"\n",
    )
    .unwrap();
    let cases = load_case_environments(root.path());
    let ring = repo_root().join("tests/core-acceptance/cases/actor-same-type-completion-ring.hew");
    for name in ["configured", "unconfigured", "unregistered"] {
        let path = root.path().join(format!("{name}.hew"));
        let environment = if name == "configured" {
            fixture_environment(&ring)
        } else {
            environment_for_source(&cases, &path)
        };
        let output = run_native(&path, "", &environment);
        let workers = if name == "configured" {
            "1".to_string()
        } else {
            std::env::var("HEW_WORKERS").unwrap_or_else(|_| "absent".into())
        };
        assert!(output.status.success(), "{}", describe_output(&output));
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            format!("{workers}\n{HEW_SEED}\n"),
            "{name}"
        );
        assert!(output.stderr.is_empty(), "{}", describe_output(&output));
    }
}

#[test]
fn sandbox_graduation_corpus_is_fully_covered() {
    let graduation_dir = repo_root().join("examples").join("sandbox-graduation");
    let expected: BTreeSet<PathBuf> = std::fs::read_dir(&graduation_dir)
        .unwrap_or_else(|err| {
            panic!(
                "failed to read sandbox graduation directory {}: {err}",
                graduation_dir.display()
            )
        })
        .map(|entry| {
            entry
                .unwrap_or_else(|err| panic!("failed to read sandbox graduation entry: {err}"))
                .path()
        })
        .filter(|path| path.extension().is_some_and(|extension| extension == "hew"))
        .collect();
    let actual: BTreeSet<PathBuf> = PARITY_CASES
        .iter()
        .filter_map(|case| {
            case.source_rel
                .strip_prefix("examples/sandbox-graduation/")
                .map(|source_rel| graduation_dir.join(source_rel))
        })
        .collect();

    assert_eq!(
        actual, expected,
        "every sandbox-graduation Hew source must have a native↔sandbox parity case"
    );
}

#[cfg(unix)]
#[test]
fn timestamp_values_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "timestamp_values",
            source_rel: "tests/core-acceptance/cases/timestamp-values.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn language_cohort_generic_closure_defaults_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "generic_closure_defaults",
            source_rel: "tests/core-acceptance/cases/generic-closure-composition.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn language_cohort_decimal_time_matches_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "duration_decimals",
            source_rel: "tests/core-acceptance/cases/duration-decimals.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn language_cohort_semaphore_leases_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "semaphore_leases",
            source_rel: "tests/vertical-slice/accept/semaphore_leases.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn language_cohort_semaphore_waits_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "semaphore_owned_waits",
            source_rel: "tests/vertical-slice/accept/semaphore_owned_waits.hew",
        },
        "",
    );
}

// This integration test executes the Node sandbox VM. Windows CI covers the
// native toolchain and WASI target separately; Linux owns this VM contract.
#[cfg(unix)]
#[test]
fn supervisor_wiring_matches_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "supervisor-wired-sibling-restart",
            source_rel: "tests/core-acceptance/cases/supervisor-wired-sibling-restart.hew",
        },
        ParityCase {
            test_name: "supervisor-wired-declaration-restarts",
            source_rel: "tests/core-acceptance/cases/supervisor-wired-declaration-restarts.hew",
        },
        ParityCase {
            test_name: "supervisor-wired-nested-prerequisite",
            source_rel: "tests/core-acceptance/cases/supervisor-wired-nested-prerequisite.hew",
        },
        ParityCase {
            test_name: "supervisor-wired-declaration-shutdown",
            source_rel: "tests/core-acceptance/cases/supervisor-wired-declaration-shutdown.hew",
        },
        ParityCase {
            test_name: "supervisor-wired-construction-cancel",
            source_rel: "tests/core-acceptance/cases/supervisor-wired-construction-cancel.hew",
        },
        ParityCase {
            test_name: "supervisor-wired-stop-during-reconstruction",
            source_rel:
                "tests/core-acceptance/cases/supervisor-wired-stop-during-reconstruction.hew",
        },
        ParityCase {
            test_name: "supervisor-wired-caught-init-crash",
            source_rel: "tests/core-acceptance/cases/supervisor-wired-caught-init-crash.hew",
        },
        ParityCase {
            test_name: "supervisor-wired-caught-start-crash",
            source_rel: "tests/core-acceptance/cases/supervisor-wired-caught-start-crash.hew",
        },
        ParityCase {
            test_name: "supervisor-wired-caught-crash-rollback",
            source_rel: "tests/core-acceptance/cases/supervisor-wired-caught-crash-rollback.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn machine_parallel_regions_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "machine-parallel-regions",
            source_rel: "tests/core-acceptance/cases/machine-parallel-regions.hew",
        },
        ParityCase {
            test_name: "machine-parallel-hooks",
            source_rel: "tests/core-acceptance/cases/machine-parallel-hooks.hew",
        },
        ParityCase {
            test_name: "machine-parallel-nested",
            source_rel: "tests/core-acceptance/cases/machine-parallel-nested.hew",
        },
        ParityCase {
            test_name: "machine-parallel-computed",
            source_rel: "tests/core-acceptance/cases/machine-parallel-computed.hew",
        },
        ParityCase {
            test_name: "machine-parallel-rollback",
            source_rel: "tests/core-acceptance/cases/machine-parallel-rollback.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn machine_initial_constructors_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "machine-initial-constructors",
            source_rel: "tests/core-acceptance/cases/machine-initial-constructors.hew",
        },
        ParityCase {
            test_name: "machine-initial-rollback",
            source_rel: "tests/core-acceptance/cases/machine-initial-rollback.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn machine_shallow_history_matches_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "machine-history-nested-regions",
            source_rel: "tests/core-acceptance/cases/machine-history-nested-regions.hew",
        },
        ParityCase {
            test_name: "machine-history-capture-rollback",
            source_rel: "tests/core-acceptance/cases/machine-history-capture-rollback.hew",
        },
        ParityCase {
            test_name: "machine-history-entry-rollback",
            source_rel: "tests/core-acceptance/cases/machine-history-entry-rollback.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn machine_shallow_history_codecs_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "machine-shallow-history",
            source_rel: "tests/core-acceptance/cases/machine-shallow-history.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn ordinary_and_machine_wire_values_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "wire-generic-values",
            source_rel: "tests/core-acceptance/cases/wire-generic-values.hew",
        },
        ParityCase {
            test_name: "machine-wire-values",
            source_rel: "tests/core-acceptance/cases/machine-wire-values.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn lexical_path_helpers_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "path_value_copies",
            source_rel: "tests/core-acceptance/cases/path-value-copies.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn playground_sources_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();

    for case in PARITY_CASES {
        assert_case(case, "");
    }
}

#[cfg(unix)]
#[test]
fn generic_sort_and_reverse_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "float_sort_values",
            source_rel: "tests/core-acceptance/cases/float-sort-values.hew",
        },
        ParityCase {
            test_name: "sort_values",
            source_rel: "tests/core-acceptance/cases/sort-values.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn impl_method_ownership_matches_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "impl_method_ownership",
            source_rel: "tests/core-acceptance/cases/impl-method-ownership.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn checked_for_loops_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "checked_for_loops",
            source_rel: "tests/core-acceptance/cases/associated-bound-iterator.hew",
        },
        "",
    );
    assert_case(
        &ParityCase {
            test_name: "generic_index_protocol",
            source_rel: "tests/core-acceptance/cases/generic-index-protocol.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn implicit_iterator_suspension_matches_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "implicit_iterator_suspension",
            source_rel: "tests/core-acceptance/cases/iterator-implicit-suspension.hew",
        },
        "",
    );
    assert_case(
        &ParityCase {
            test_name: "nested_iterator_callback_effects",
            source_rel: "tests/core-acceptance/cases/nested-iterator-callback-effects.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn actor_state_iterator_snapshots_and_explicit_drains_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "actor_state_vec_seat",
            source_rel: "tests/core-acceptance/cases/actor-state-vec-seat.hew",
        },
        ParityCase {
            test_name: "actor_state_iterator_snapshot",
            source_rel: "tests/core-acceptance/cases/actor-state-iterator-snapshot.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn vec_iter_owned_slots_match_native_and_vm() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "owning_call_value_semantics",
            source_rel: "tests/core-acceptance/cases/owning-call-value-semantics.hew",
        },
        ParityCase {
            test_name: "vec_iter_copies",
            source_rel: "tests/core-acceptance/cases/vector-iteration-next.hew",
        },
        ParityCase {
            test_name: "vec_iter_owned_cleanup",
            source_rel: "tests/core-acceptance/cases/vector-into-iter-owning.hew",
        },
        ParityCase {
            test_name: "vec_iter_borrowed_source",
            source_rel: "tests/core-acceptance/cases/vector-borrowed-iteration.hew",
        },
        ParityCase {
            test_name: "vec_iter_swap_left_fault",
            source_rel: "tests/core-acceptance/cases/vector-swap-left-fault.hew",
        },
        ParityCase {
            test_name: "vec_iter_swap_right_fault",
            source_rel: "tests/core-acceptance/cases/vector-swap-right-fault.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn hashset_iteration_matches_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "hashset_iteration",
            source_rel: "tests/core-acceptance/cases/hashset-iteration.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn utf8_decode_matches_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "utf8-decode-loop",
            source_rel: "tests/core-acceptance/cases/utf8-decode-loop.hew",
        },
        ParityCase {
            test_name: "utf8-decode-errors",
            source_rel: "tests/core-acceptance/cases/utf8-decode-errors.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn associated_fields_and_index_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "associated-index-dispatch",
            source_rel: "tests/core-acceptance/cases/associated-index-dispatch.hew",
        },
        "",
    );
}

#[cfg(unix)]
#[test]
fn encoding_values_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "sir-json-value-collections",
            source_rel: "tests/core-acceptance/cases/sir-json-value-collections.hew",
        },
        ParityCase {
            test_name: "vec-element-json-value",
            source_rel: "tests/core-acceptance/cases/vec-element-json-value.hew",
        },
        ParityCase {
            test_name: "yaml-value-composition",
            source_rel: "tests/core-acceptance/cases/yaml-value-composition.hew",
        },
        ParityCase {
            test_name: "yaml-mapping-keys",
            source_rel: "tests/core-acceptance/cases/yaml-mapping-keys.hew",
        },
        ParityCase {
            test_name: "yaml-value-argument-fault",
            source_rel: "tests/core-acceptance/cases/yaml-value-argument-fault.hew",
        },
        ParityCase {
            test_name: "value-tree-resource-toml-value-tree",
            source_rel: "tests/core-acceptance/cases/value-tree-resource-toml-value-tree.hew",
        },
        ParityCase {
            test_name: "value-tree-resource-toml-parse-handled-error",
            source_rel:
                "tests/core-acceptance/cases/value-tree-resource-toml-parse-handled-error.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn user_codec_stream_matches_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case(
        &ParityCase {
            test_name: "user_codec_stream",
            source_rel: "tests/core-acceptance/cases/user-codec-stream.hew",
        },
        "",
    );
}

/// Standard input reaches both engines as the same bytes: the VM serves the
/// stdin line read that native suspends on, with line endings, an empty line
/// and the unterminated tail intact.
#[cfg(unix)]
#[test]
fn stdin_reads_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();

    let case = ParityCase {
        test_name: "stdin_lines",
        source_rel: "hew-wasm/tests/fixtures/stdin_lines.hew",
    };
    assert_case(&case, "alpha\r\n\nbeta\r\ngamma");
}

/// Two tasks reading standard input at once each take one line, and a later
/// read to the end takes the rest, in both engines.
#[cfg(unix)]
#[test]
fn concurrent_stdin_reads_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();

    let case = ParityCase {
        test_name: "stdin_concurrent",
        source_rel: "hew-wasm/tests/fixtures/stdin_concurrent.hew",
    };
    assert_case(&case, "one\ntwo\nthree\nfour");
}

#[cfg(unix)]
#[test]
fn identity_w7_record_resource_does_not_join_runtime_admission() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();

    // The root record renders like the runtime admission record, while both
    // programs perform the same actor request and print the same answer.
    for case in [
        ParityCase {
            test_name: "identity_w7_sandbox",
            source_rel: "hew-wasm/tests/fixtures/identity_w7_sandbox.hew",
        },
        ParityCase {
            test_name: "identity_w7_sandbox_control",
            source_rel: "hew-wasm/tests/fixtures/identity_w7_sandbox_control.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
fn assert_case(case: &ParityCase, stdin: &str) {
    assert_case_with_inputs(case, stdin, hew_compile::SourceSet::in_memory());
}

#[cfg(unix)]
#[test]
fn quoted_file_import_aliases_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    let mut inputs = hew_compile::SourceSet::in_memory();
    inputs.insert(
        "codecs/rle.hew",
        include_str!("../../tests/core-acceptance/fixtures/file-import-alias/codecs/rle.hew"),
    );
    assert_case_with_inputs(
        &ParityCase {
            test_name: "file_import_alias_same_source",
            source_rel: "tests/core-acceptance/fixtures/file-import-alias/same_source.hew",
        },
        "",
        inputs,
    );
}

#[cfg(unix)]
#[test]
fn contextual_macros_native_and_vm() {
    let source = "macro add { ($x:expr) => expr { $x + 1 } } fn main() { println(add!(2)); }";
    let expanded = hew_wasm::expand_macro(source, Some(source.find("add!(2)").unwrap()))
        .expect("browser macro expansion");
    assert!(expanded.contains("2 + 1"), "{expanded}");
    let whole = hew_wasm::expand_macro(source, None).expect("browser whole-document expansion");
    assert!(whole.contains("fn main()"), "{whole}");
    assert!(whole.contains("2 + 1"), "{whole}");
    assert!(!whole.contains("add!"), "{whole}");
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    let mut inputs = hew_compile::SourceSet::in_memory();
    inputs.insert(
        "contextual_macros_support.hew",
        include_str!("../../tests/core-acceptance/cases/contextual_macros_support.hew"),
    );
    inputs.insert(
        "contextual_macros_library.hew",
        include_str!("../../tests/core-acceptance/cases/contextual_macros_library.hew"),
    );
    assert_case_with_inputs(
        &ParityCase {
            test_name: "contextual_macros",
            source_rel: "tests/core-acceptance/cases/contextual-macros.hew",
        },
        "",
        inputs,
    );
}

#[cfg(unix)]
#[test]
fn attribute_macros_native_and_vm() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    let mut inputs = hew_compile::SourceSet::in_memory();
    inputs.insert(
        "attribute_macros_library.hew",
        include_str!("../../tests/core-acceptance/cases/attribute_macros_library.hew"),
    );
    assert_case_with_inputs(
        &ParityCase {
            test_name: "attribute_macros",
            source_rel: "tests/core-acceptance/cases/attribute-macros.hew",
        },
        "",
        inputs,
    );
    for (source, expected) in [
        ("#[missing(1 + call(2), [3, 4])] fn f() {} fn main() {}", "E_UNKNOWN_ATTRIBUTE"),
        ("#[missing] type T {} fn main() {}", "E_UNKNOWN_ATTRIBUTE"),
        ("macro m { #[()] $f:fn => expr { 1 } } fn main() {}", "E_MACRO_BAD_CATEGORY"),
        ("macro m { #[()] $f:fn => items { fn other() {} } } fn main() {}", "E_MACRO_NAME_CARRIER"),
        ("macro m { #[()] $f:fn => items { fn $f.name() { loop { $f.body; break; } } } } fn main() {}", "E_MACRO_BODY_PLACEMENT"),
        ("macro m { #[()] $f:fn fails => items { $f } } #[m] fn f() {} fn main() {}", "expected a fallible function"),
        ("macro test { () => expr { 1 } } fn main() {}", "E_MACRO_RESERVED_NAME"),
        ("macro m { #[()] $f:fn => items { $f } } actor A { #[m] receive fn f() {} } fn main() {}", "E_ATTRIBUTE_MACRO_POSITION"),
    ] {
        let analysis = hew_wasm::analyze(source).expect("browser diagnostic output");
        assert!(analysis.contains(expected), "{expected}: {analysis}");
        let result = compile_to_sandbox_bytecode_with_inputs(source, Some(SANDBOX_PROFILE), hew_compile::SourceSet::in_memory()).expect("frontend diagnostic output");
        assert!(result.diagnostics.iter().any(|diagnostic| diagnostic.message.contains(expected)), "{expected}: {:?}", result.diagnostics);
    }
}

#[cfg(unix)]
#[test]
fn compile_time_inputs_native_and_vm() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    let mut inputs = hew_compile::SourceSet::in_memory();
    inputs.insert_bytes(
        "compile-time-inputs-data/message.txt",
        b"Hew caf\xc3\xa9\n".to_vec(),
    );
    assert_case_with_inputs(
        &ParityCase {
            test_name: "compile_time_inputs",
            source_rel: "tests/core-acceptance/cases/compile-time-inputs.hew",
        },
        "",
        inputs,
    );
    assert_case(
        &ParityCase {
            test_name: "source_location",
            source_rel: "tests/core-acceptance/cases/source-location.hew",
        },
        "",
    );
    let mut inputs = hew_compile::SourceSet::in_memory();
    inputs.insert(
        "hew.toml",
        "[package]\nname = \"future\"\nhew = \">=999.0.0\"\n",
    );
    let refused =
        compile_to_sandbox_bytecode_with_inputs("fn main() {}", Some(SANDBOX_PROFILE), inputs)
            .expect("frontend diagnostic output");
    assert!(
        refused
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("requires Hew >=999.0.0")),
        "{:?}",
        refused.diagnostics
    );
    let mut inputs = hew_compile::SourceSet::in_memory();
    inputs.insert("README.md", "outside the standalone source");
    let escaped = compile_to_sandbox_bytecode_with_inputs(
        "fn main() { println(include_str!(\"../README.md\")); }",
        Some(SANDBOX_PROFILE),
        inputs,
    )
    .expect("frontend diagnostic output");
    assert!(
        escaped
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("E_INCLUDE_OUTSIDE_PACKAGE")),
        "{:?}",
        escaped.diagnostics
    );
}

#[cfg(unix)]
#[test]
fn typed_embedding_native_and_vm() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    let mut inputs = hew_compile::SourceSet::in_memory();
    for (name, text) in [
        (
            "config.json",
            include_str!("../../tests/core-acceptance/cases/typed-embed-data/config.json"),
        ),
        (
            "config.yaml",
            include_str!("../../tests/core-acceptance/cases/typed-embed-data/config.yaml"),
        ),
        (
            "config.toml",
            include_str!("../../tests/core-acceptance/cases/typed-embed-data/config.toml"),
        ),
    ] {
        inputs.insert(format!("typed-embed-data/{name}"), text);
    }
    assert_case_with_inputs(
        &ParityCase {
            test_name: "typed_embed",
            source_rel: "tests/core-acceptance/cases/typed-embed.hew",
        },
        "",
        inputs,
    );
    let mut inputs = hew_compile::SourceSet::in_memory();
    inputs.insert_bytes("broken.json", vec![0xff]);
    let invalid_data = compile_to_sandbox_bytecode_with_inputs(
        "fn main() { let text: string = embed!(\"broken.json\"); println(text); }",
        Some(SANDBOX_PROFILE),
        inputs,
    )
    .expect("frontend diagnostic output");
    assert!(
        invalid_data.diagnostics.iter().any(|diagnostic| {
            diagnostic.message.contains("E_EMBED_DECODE")
                && diagnostic
                    .source_module
                    .as_deref()
                    .is_some_and(|source| source.ends_with("broken.json"))
        }),
        "{:?}",
        invalid_data.diagnostics
    );
}

#[cfg(unix)]
#[test]
fn aggregate_constants_native_and_vm() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    assert_case_with_inputs(
        &ParityCase {
            test_name: "aggregate_constants",
            source_rel: "tests/core-acceptance/cases/aggregate-constants.hew",
        },
        "",
        hew_compile::SourceSet::in_memory(),
    );
}

#[cfg(unix)]
#[test]
fn package_coherence_native_and_vm() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    let root = repo_root().join("tests/core-acceptance/cases/package-coherence");
    let mut inputs = hew_compile::SourceSet::in_memory();
    for path in [
        "hew.toml",
        "protocol/hew.toml",
        "protocol/traits.hew",
        "data/hew.toml",
        "data/model.hew",
        "data/roles/format.hew",
    ] {
        inputs.insert(
            path,
            std::fs::read_to_string(root.join(path)).expect("read package fixture"),
        );
    }
    assert_case_with_inputs(
        &ParityCase {
            test_name: "package_coherence",
            source_rel: "tests/core-acceptance/cases/package-coherence/main.hew",
        },
        "",
        inputs.clone(),
    );
    let orphan = compile_to_sandbox_bytecode_with_inputs(
        "import \"protocol/traits.hew\"; import \"data/model.hew\"; impl Render for Widget { fn render(self) -> string { \"foreign\" } } fn main() {}",
        Some(SANDBOX_PROFILE),
        inputs,
    ).expect("compile virtual orphan fixture");
    assert!(
        orphan
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.kind == "OrphanImpl"),
        "{:?}",
        orphan.diagnostics
    );
    assert!(orphan.bytecode.is_none());
}

#[cfg(unix)]
fn assert_case_with_inputs(case: &ParityCase, stdin: &str, inputs: hew_compile::SourceSet) {
    let repo_root = repo_root();
    let source_path = repo_root.join(case.source_rel);
    let environment = fixture_environment(&source_path);
    let native = run_native(&source_path, stdin, &environment);
    let source = std::fs::read_to_string(&source_path).unwrap_or_else(|err| {
        panic!(
            "failed to read parity source {} for {}: {err}",
            source_path.display(),
            case.test_name
        )
    });
    let sandbox_compile =
        compile_to_sandbox_bytecode_with_inputs(&source, Some(SANDBOX_PROFILE), inputs)
            .unwrap_or_else(|err| panic!("sandbox compile threw for {}: {err}", case.test_name));

    assert_no_error_diagnostics(case, &sandbox_compile.diagnostics);
    let bytecode = sandbox_compile.bytecode.unwrap_or_else(|| {
        panic!(
            "sandbox compile emitted no bytecode for {}; diagnostics:\n{}",
            case.test_name,
            diagnostics_dump(&sandbox_compile.diagnostics)
        )
    });
    let bytecode_json = serde_json::to_string_pretty(&bytecode)
        .unwrap_or_else(|err| panic!("failed to serialize bytecode for {}: {err}", case.test_name));
    let tempdir = tempfile::tempdir()
        .unwrap_or_else(|err| panic!("failed to create tempdir for {}: {err}", case.test_name));
    let bytecode_path = tempdir.path().join("bytecode.json");
    std::fs::write(&bytecode_path, bytecode_json)
        .unwrap_or_else(|err| panic!("failed to write bytecode for {}: {err}", case.test_name));

    let stdin_path = tempdir.path().join("stdin.txt");
    std::fs::write(&stdin_path, stdin)
        .unwrap_or_else(|err| panic!("failed to write stdin for {}: {err}", case.test_name));
    let sandbox = run_sandbox(&bytecode_path, &stdin_path, &environment);
    assert_exit_code_parity(case, &native, &sandbox);
    assert_stdout_parity(case, &native, &sandbox);
    assert_stderr_parity(case, &native, &sandbox);
    assert_exact_stdout(case, &native);
}

#[cfg(unix)]
fn assert_stderr_parity(case: &ParityCase, native: &Output, sandbox: &Output) {
    assert_eq!(
        String::from_utf8_lossy(&sandbox.stderr),
        String::from_utf8_lossy(&native.stderr),
        "{} stderr mismatch\nnative:\n{}\nsandbox:\n{}",
        case.test_name,
        describe_output(native),
        describe_output(sandbox)
    );
}

#[cfg(unix)]
fn assert_exact_stdout(case: &ParityCase, native: &Output) {
    let expected = match case.test_name {
        "package_coherence" => Some("blue\nwidget: blue\nnumbers: 2\nwords: 1\n1\none\ninteger: 42\ntext: hello\nyes\nno\nflag: yes\n"),
        "trap_residual" => Some(
            "repeat-value\nrepeat-count\n3\n7\n7\n7\n65535\n18446744073709551615\n-1\n255\n1\ntrue\n65\n42\nboom\n6\nnone\n",
        ),
        "f32_arithmetic_precision" => Some("16777216\n"),
        "integer_bit_methods" => Some(
            "3\n5\n0\n3\n4\n0\n13330\n19\n3\n3221225472\n57\n1\n44\n255\n56\n-128\n-128\n-2\nchecked 255\noverflow\noverflow\n",
        ),
        "float_classification" => Some(
            "nan=false finite=true infinite=false\nnan=false finite=false infinite=true\nnan=false finite=false infinite=true\nnan=true finite=false infinite=false\n4609434218613702656\n13835058055282163712\nfalse\nfalse\ntrue\ntrue\n1.5\n-0.25\nrefused infinity: NonFinite\n",
        ),
        "pointer_width_native64" => Some("4294967296\n4294967296\n"),
        "sort_values" | "float_sort_values" => Some("sort-authority=0\n"),
        "user_codec_stream" => Some("user codec frames\n"),
        "wire-generic-values" => Some("generic wire values passed\n"),
        "machine-wire-values" => Some("machine wire checkpoints passed\n"),
        "recursive_structural_equality" => Some("json true false true\nvec true false true\nnode true false\nrecursive keys ok\nlist true false\n"),
        "lambda_actor_private_capture" => Some("501\n1202\n2303\nouter 0 0\n2\n"),
        "dyn_subtrait_display" => Some("point at 1\n(1, 2)\npoint at 3 / (3, 4)\n"),
        "dyn_multibound_dispatch" => {
            Some("alpha 3\nbeta 3\n300\nalpha 3\nbeta 5 alpha 5 500\nright\nleft\ntag 7\n")
        }
        "identity_w7_sandbox" | "identity_w7_sandbox_control" => Some("7\n8\n"),
        "stdin_concurrent" => Some("one line each\nrest=[three\nfour]\n"),
        "stdin_lines" => Some("first=[alpha] second=[]\nrest=[beta\r\ngamma]\nafter end\n"),
        "closure_edges" => Some(
            "Err(empty) Ok(6) Err(not a number: x)\nOk(4) Err(not a number: x)\nprinted\nErr(nothing to print) Ok(())\nOk(7) Ok(3) Err(not a number: x)\nOk([6, 6])\nErr(not a number: x)\nErr(blank entry)\n[Some(2), None]\nSome(9) None\n",
        ),
        "error_display" => Some(
            "Missing: port\nPair: retries, 3\nOutOfRange: name: port, value: 70000\nClosed\nLocation: file: app.hew, line: 12\nStore: Closed\nRefused: the store is busy\nOther: Location: file: lib.hew, line: 4\nLabeled: label: port, code: 2\nTag: 5\n8080\n",
        ),
        "variant_edges" => Some(
            "port: 8080\nhost: not a number: localhost\nuser: no setting named user\nlock: Store: Locked\nErr(Code: -2) Ok(3)\nErr(Note: noted: busy)\n",
        ),
        "failing_generators" => Some(
            "Ok(line 0)\nOk(line 1)\nvalue 0\nvalue 10\nvalue 20\nvalue 30\nskipped: four is unlucky\nvalue -1\nline 0\nline 1\ncounted 2\nline 0\nline 1\nline 2\nstopped: Corrupt: 3\n",
        ),
        "failure_edges" => Some(
            "Ok(8080) Err(empty)\nOk(Ok(1)) Ok(Err(zero)) Err(refused)\nOk(abc) Err(nothing to read)\nOk(16160)\n",
        ),
        "error_edges" => Some(
            "ok 8080\nparse arm: Empty\nparse arm: NotANumber: abc\nmissing arm: port\nrange arm: 8080\nload failed: port 8080 is reserved\nreport: NotANumber: xyz\nhandled: NotANumber: xyz\nzero 0\nload failed: loading config: Parse: NotANumber: abc\n",
        ),
        _ => None,
    };
    if let Some(expected) = expected {
        assert_eq!(
            String::from_utf8_lossy(&native.stdout),
            expected,
            "{} native stdout changed from its exact-value contract",
            case.test_name
        );
    }
}

#[cfg(unix)]
fn assert_no_error_diagnostics(case: &ParityCase, diagnostics: &[Diagnostic]) {
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.severity != "error"),
        "{} produced sandbox compile errors:\n{}",
        case.test_name,
        diagnostics_dump(diagnostics)
    );
}

#[cfg(unix)]
fn assert_exit_code_parity(case: &ParityCase, native: &Output, sandbox: &Output) {
    assert_eq!(
        sandbox.status.code(),
        native.status.code(),
        "{} exit-code mismatch\nnative:\n{}\nsandbox:\n{}",
        case.test_name,
        describe_output(native),
        describe_output(sandbox)
    );
}

#[cfg(unix)]
fn assert_stdout_parity(case: &ParityCase, native: &Output, sandbox: &Output) {
    assert_eq!(
        String::from_utf8_lossy(&sandbox.stdout),
        String::from_utf8_lossy(&native.stdout),
        "{} stdout mismatch\nnative:\n{}\nsandbox:\n{}",
        case.test_name,
        describe_output(native),
        describe_output(sandbox)
    );
}

#[cfg(unix)]
fn run_native(source_path: &Path, stdin: &str, environment: &BTreeMap<String, String>) -> Output {
    let output_dir = tempfile::tempdir().expect("native parity output directory");
    // Compare execution output on both paths. The sandbox API returns frontend
    // warnings as diagnostics, so native compiler warnings are separate too.
    let compiled = Command::new(hew_binary())
        .arg("compile")
        .arg("--emit-dir")
        .arg(output_dir.path())
        .arg("--opt-level")
        .arg("0")
        .arg(source_path)
        .current_dir(repo_root())
        .env("HEW_STD", repo_root().join("std"))
        .env("HEW_SEED", HEW_SEED)
        .env("NO_COLOR", "1")
        .output()
        .unwrap_or_else(|err| panic!("failed to compile native parity source: {err}"));
    assert!(
        compiled.status.success(),
        "native compilation failed for {}: {}",
        source_path.display(),
        describe_output(&compiled),
    );
    let binary = output_dir
        .path()
        .join(source_path.file_stem().expect("native parity source name"));
    Command::new(binary)
        .current_dir(repo_root())
        .env("HEW_STD", repo_root().join("std"))
        .env("HEW_SEED", HEW_SEED)
        .env("NO_COLOR", "1")
        .envs(environment)
        .write_stdin(stdin)
        .output()
        .unwrap_or_else(|err| panic!("failed to execute native parity program: {err}"))
}

#[cfg(unix)]
fn run_sandbox(
    bytecode_path: &Path,
    stdin_path: &Path,
    environment: &BTreeMap<String, String>,
) -> Output {
    Command::new("npm")
        .arg("--prefix")
        .arg(repo_root().join("hew-sandbox-vm"))
        .arg("run")
        .arg("-s")
        .arg("parity:run")
        .arg("--")
        .arg(bytecode_path)
        .arg("--seed")
        .arg(HEW_SEED)
        .arg("--stdin")
        .arg(stdin_path)
        .current_dir(repo_root())
        .env("NO_COLOR", "1")
        .envs(environment)
        .output()
        .unwrap_or_else(|err| panic!("failed to spawn sandbox parity runner: {err}"))
}

#[cfg(unix)]
fn ensure_native_toolchain() {
    static NATIVE_TOOLCHAIN: OnceLock<()> = OnceLock::new();
    NATIVE_TOOLCHAIN.get_or_init(|| {
        // A `OnceLock` serializes nothing across nextest's per-test processes;
        // both artifacts must go through the cross-process build authority.
        hew_testutil::ensure_hew_bin_built().expect("build the hew binary");
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

#[cfg(unix)]
fn ensure_parity_runner_built() {
    static PARITY_RUNNER: OnceLock<()> = OnceLock::new();
    PARITY_RUNNER.get_or_init(|| {
        let vm_dir = repo_root().join("hew-sandbox-vm");
        assert!(
            vm_dir.join("node_modules").is_dir(),
            "hew-sandbox-vm dependencies are not installed; run `npm --prefix hew-sandbox-vm ci` or `make sandbox-parity`"
        );
        run_bootstrap_command(
            "npm --prefix hew-sandbox-vm run -s build",
            std::process::Command::new("npm")
                .arg("--prefix")
                .arg(&vm_dir)
                .arg("run")
                .arg("-s")
                .arg("build")
                .current_dir(repo_root()),
        );
        assert!(
            vm_dir
                .join("dist")
                .join("interpreter")
                .join("parity-runner.js")
                .is_file(),
            "sandbox parity runner was not built at hew-sandbox-vm/dist/interpreter/parity-runner.js"
        );
    });
}

#[cfg(unix)]
fn run_bootstrap_command(label: &str, command: &mut std::process::Command) {
    let output = command
        .output()
        .unwrap_or_else(|err| panic!("failed to invoke `{label}`: {err}"));
    assert!(
        output.status.success(),
        "`{label}` failed\n{}",
        describe_output(&output)
    );
}

#[cfg(unix)]
fn hew_binary() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_hew") {
        return PathBuf::from(path);
    }

    target_debug_dir().join(format!("hew{}", std::env::consts::EXE_SUFFIX))
}

#[cfg(unix)]
fn target_debug_dir() -> PathBuf {
    if let Ok(target_dir) = std::env::var("CARGO_TARGET_DIR") {
        return PathBuf::from(target_dir).join("debug");
    }
    repo_root().join("target").join("debug")
}

fn repo_root() -> &'static Path {
    static REPO_ROOT: OnceLock<PathBuf> = OnceLock::new();
    REPO_ROOT
        .get_or_init(|| {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .expect("hew-wasm crate should have a workspace parent")
                .to_path_buf()
        })
        .as_path()
}

#[cfg(unix)]
fn set_test_hewpath() {
    std::env::set_var("HEW_STD", repo_root().join("std"));
}

#[cfg(unix)]
fn diagnostics_dump(diagnostics: &[Diagnostic]) -> String {
    serde_json::to_string_pretty(diagnostics).expect("diagnostics should serialize")
}

#[cfg(unix)]
fn describe_output(output: &Output) -> String {
    format!(
        "status: {:?}\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[cfg(unix)]
#[test]
fn doc_fence_imports_its_package_outside_the_working_directory() {
    ensure_native_toolchain();
    ensure_parity_runner_built();
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("greet")).unwrap();
    std::fs::write(
        dir.path().join("greet/hew.toml"),
        "[package]\nname = \"acme.greet\"\nversion = \"0.1.0\"\nedition = \"2026\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("greet/greet.hew"),
        "//! ```hew\n\
         //! import acme.greet;\n\
         //! fn main() {\n\
         //!     println(greet.hello(\"Hew\"));\n\
         //! }\n\
         //! // Output:\n\
         //! // hello, Hew\n\
         //! ```\n\
         pub fn hello(name: string) -> string { f\"hello, {name}\" }\n",
    )
    .unwrap();
    for engine in ["native", "vm"] {
        let output = Command::new(hew_binary())
            .current_dir(dir.path())
            .timeout(std::time::Duration::from_secs(30))
            .args([
                "test",
                "--doc",
                "greet/greet.hew",
                "--engine",
                engine,
                "--format",
                "json",
            ])
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{engine}: stdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        let events = String::from_utf8_lossy(&output.stdout);
        assert!(events.contains("\"passed\":1"), "{engine}: {events}");
        assert!(events.contains("hello, Hew\\n"), "{engine}: {events}");
    }
}

#[cfg(unix)]
#[test]
fn machine_internal_reactions_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "machine-internal-reactions",
            source_rel: "tests/core-acceptance/cases/machine-internal-reactions.hew",
        },
        ParityCase {
            test_name: "machine-internal-fault-rollback",
            source_rel: "tests/core-acceptance/cases/machine-internal-fault-rollback.hew",
        },
        ParityCase {
            test_name: "machine-internal-default-limit",
            source_rel: "tests/core-acceptance/cases/machine-internal-default-limit.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn machine_deep_history_matches_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "machine-deep-history",
            source_rel: "tests/core-acceptance/cases/machine-deep-history.hew",
        },
        ParityCase {
            test_name: "machine-deep-history-parallel",
            source_rel: "tests/core-acceptance/cases/machine-deep-history-parallel.hew",
        },
        ParityCase {
            test_name: "machine-deep-history-rollback",
            source_rel: "tests/core-acceptance/cases/machine-deep-history-rollback.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn recovered_and_actor_fault_messages_match_native() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "scope-failure-helper-match",
            source_rel: "tests/core-acceptance/cases/scope-failure-helper-match.hew",
        },
        ParityCase {
            test_name: "actor-crash-hook-live-state",
            source_rel: "tests/core-acceptance/cases/actor-crash-hook-live-state.hew",
        },
        ParityCase {
            test_name: "task-fault-cleanup-diagnostic",
            source_rel: "tests/core-acceptance/cases/task-fault-cleanup-diagnostic.hew",
        },
    ] {
        assert_case(&case, "");
    }
}

#[cfg(unix)]
#[test]
fn language_cohort_default_arguments() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    for case in [
        ParityCase {
            test_name: "default-argument-constructors",
            source_rel: "tests/core-acceptance/cases/default-argument-constructors.hew",
        },
        ParityCase {
            test_name: "default-argument-actor",
            source_rel: "tests/core-acceptance/cases/default-argument-actor.hew",
        },
        ParityCase {
            test_name: "default-argument-values",
            source_rel: "tests/core-acceptance/cases/default-argument-values.hew",
        },
        ParityCase {
            test_name: "default-argument-imports",
            source_rel: "tests/core-acceptance/cases/default-argument-imports.hew",
        },
        ParityCase {
            test_name: "default-argument-effects",
            source_rel: "tests/core-acceptance/cases/default-argument-effects.hew",
        },
        ParityCase {
            test_name: "machine-generic-step-limits",
            source_rel: "tests/core-acceptance/cases/machine-generic-step-limits.hew",
        },
        ParityCase {
            test_name: "machine-internal-default-limit",
            source_rel: "tests/core-acceptance/cases/machine-internal-default-limit.hew",
        },
        ParityCase {
            test_name: "machine-internal-reactions",
            source_rel: "tests/core-acceptance/cases/machine-internal-reactions.hew",
        },
    ] {
        let mut inputs = hew_compile::SourceSet::in_memory();
        inputs.insert(
            "default_argument_support/provider.hew",
            include_str!("../../tests/core-acceptance/cases/default_argument_support/provider.hew"),
        );
        assert_case_with_inputs(&case, "", inputs);
    }
}
