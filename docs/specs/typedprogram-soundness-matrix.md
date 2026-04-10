# TypedProgram soundness matrix (Wave 13, bounded slice 1)

This artifact maps the first bounded Wave 13 certification slice for checker-owned `TypedProgram` side tables that are already on the wire.

## Scope

Included in this slice:

- `expr_types`
- `method_call_receiver_kinds`
- `assign_target_kinds`
- `assign_target_shapes`
- `lowering_facts`

Explicitly deferred from this slice: every other `TypedProgram` field.

## Shared boundary orchestrators

For all five fields, the Rust frontend hands the selected side tables to codegen in the same two places:

- `hew-compile/src/lib.rs` — `compile_file` and `compile_program` build `FrontendArtifacts`, calling:
  - `enrich_program_ast_with_diagnostics(...)` for `expr_type_entries` and `method_call_receiver_kinds`
  - `build_assign_target_kind_entries(...)`
  - `build_assign_target_shape_entries(...)`
  - `build_lowering_fact_entries(...)`
- `hew-serialize/src/msgpack.rs` — `TypedProgram` is serialized with named wire keys and consumed by the C++ reader.
- `hew-codegen/src/mlir/MLIRGen.cpp` — the loaded vectors are copied into lookup maps before lowering:
  - `exprTypeMap`
  - `methodCallReceiverKindMap`
  - `assignTargetKindMap`
  - `assignTargetShapeMap`
  - `loweringFactMap`

## Matrix

| Field | Producer | Boundary validator | Serializer entry / wire field | C++ reader field | MLIR / codegen consumer | Existing coverage / explicit gap |
| --- | --- | --- | --- | --- | --- | --- |
| `expr_types` | `hew-types/src/check/expressions.rs` — `Checker::record_type` inserts into `self.expr_types`; `hew-types/src/check/resolution.rs` — `Checker::apply_deferred_range_bound_types` re-records resolved range-bound spans; `hew-types/src/check/mod.rs` — `Checker::check_program` materializes `resolved_expr_types` into `TypeCheckOutput.expr_types`. | `hew-types/src/check/admissibility.rs` — `Checker::validate_expr_output_contract`, called by `validate_checker_output_contract`, removes unresolved inference leaks and emits `InferenceFailed` diagnostics before `TypeCheckOutput` escapes. | `hew-serialize/src/enrich.rs` — `build_expr_type_map`; `hew-serialize/src/msgpack.rs` — `TypedProgram.expr_types` / wire key `"expr_types"`. | `hew-codegen/src/msgpack_reader.cpp` — `parseExprTypeEntry`; `parseProgram` loads `prog.expr_types`. | `hew-codegen/src/mlir/MLIRGen.cpp` — `MLIRGen::requireResolvedTypeOf`; downstream call sites include `hew-codegen/src/mlir/MLIRGenExpr.cpp`, `MLIRGenStmt.cpp`, and `MLIRGenMatch.cpp` for signedness, handle dispatch, indirect-enum scrutinees, and match-expression result typing. | Existing coverage: `hew-serialize/src/enrich.rs` tests `test_build_expr_type_map_*`; `hew-codegen/tests/test_mlirgen.cpp` covers multiple fail-closed sites, including missing range-loop, match-expression, `scope.await`, and handle-receiver metadata. Gap: no dedicated `hew-serialize/src/msgpack.rs` test asserting the `"expr_types"` wire key, and no standalone `msgpack_reader.cpp` unit test for this field. |
| `method_call_receiver_kinds` | `hew-types/src/check/methods.rs` — `record_method_call_receiver_kind`; producer call sites currently include named-type dispatch and trait-object dispatch during method checking. `hew-types/src/check/mod.rs` moves the map into `TypeCheckOutput.method_call_receiver_kinds`. | No dedicated post-check validator was found in this slice. The table currently relies on producer discipline plus downstream fail-closed consumption. | `hew-serialize/src/msgpack.rs` — `build_method_call_receiver_kind_entries`; `TypedProgram.method_call_receiver_kinds` / wire key `"method_call_receiver_kinds"`. | `hew-codegen/src/msgpack_reader.cpp` — `parseMethodCallReceiverKindEntry`; `parseProgram` loads `prog.method_call_receiver_kinds`. | `hew-codegen/src/mlir/MLIRGenExpr.cpp` — method dispatch uses `methodCallReceiverKindOf(...)` and `requireMethodCallReceiverKindOf(...)` for trait-object and named-type lowering. | Existing coverage: `hew-serialize/src/msgpack.rs` tests `method_call_receiver_kinds_serialize_to_wire_field`, `build_method_call_receiver_kind_entries_only_emits_surviving_method_calls`, and `build_method_call_receiver_kind_entries_follow_module_graph_items`; `hew-codegen/tests/test_mlirgen.cpp` covers `test_trait_dispatch_requires_receiver_kind`, `test_named_type_dispatch_requires_receiver_kind`, and `test_generic_handle_impl_dispatch_requires_receiver_kind`. Gap: no checker-side boundary pass currently proves that every surviving dispatch site has a corresponding entry. |
| `assign_target_kinds` | `hew-types/src/check/statements.rs` — `Checker::check_stmt` classifies accepted `Stmt::Assign` targets as `LocalVar`, `ActorField`, `FieldAccess`, or `Index`, then inserts into `self.assign_target_kinds`. | `hew-types/src/check/admissibility.rs` — `validate_assign_target_output_contract` intersects `assign_target_kinds` with `assign_target_shapes` and prunes orphan keys before output. | `hew-serialize/src/msgpack.rs` — `build_assign_target_kind_entries`; `TypedProgram.assign_target_kinds` / wire key `"assign_target_kinds"`. | `hew-codegen/src/msgpack_reader.cpp` — `parseAssignTargetKindEntry`; `parseProgram` loads `prog.assign_target_kinds`. | `hew-codegen/src/mlir/MLIRGen.cpp` — `requireAssignTargetKindOf`; `hew-codegen/src/mlir/MLIRGenStmt.cpp` — `generateAssignStmt` treats this table as authoritative for assignment lowering. | Existing coverage: `hew-serialize/src/msgpack.rs` tests `assign_target_kinds_serialize_to_wire_field` and `build_assign_target_kind_entries_emits_assignment_targets`; `hew-codegen/tests/test_mlirgen.cpp` covers authority mismatches via `test_identifier_local_assignment_kind_mismatch_fails_closed` and `test_identifier_actor_field_assignment_kind_mismatch_fails_closed`. Gap: no dedicated negative test currently erases the `assign_target_kinds` entry entirely and asserts the `missing assign_target_kinds entry` diagnostic. |
| `assign_target_shapes` | `hew-types/src/check/statements.rs` — `Checker::check_stmt` synthesizes the accepted assignment target type and records `AssignTargetShape { is_unsigned: target_ty.is_unsigned() }` in `self.assign_target_shapes`. | `hew-types/src/check/admissibility.rs` — `validate_assign_target_output_contract` intersects `assign_target_shapes` with `assign_target_kinds` and prunes orphan keys before output. | `hew-serialize/src/msgpack.rs` — `build_assign_target_shape_entries`; `TypedProgram.assign_target_shapes` / wire key `"assign_target_shapes"`. | `hew-codegen/src/msgpack_reader.cpp` — `parseAssignTargetShapeEntry`; `parseProgram` loads `prog.assign_target_shapes`. | `hew-codegen/src/mlir/MLIRGen.cpp` — `requireAssignTargetShapeOf`; `hew-codegen/src/mlir/MLIRGenStmt.cpp` consumes `is_unsigned` to choose compound-assignment arithmetic semantics without re-deriving signedness from the AST. | Existing coverage: `hew-types/src/check/tests.rs` has `assign_target_shapes_populated_for_while_loop_with_import`; `hew-serialize/src/msgpack.rs` has `build_assign_target_shape_entries_unsigned_target`, `build_assign_target_shape_entries_signed_target`, and `assign_target_shapes_msgpack_roundtrip`. Gap: no dedicated C++ fail-closed test currently removes an `assign_target_shapes` entry and asserts the `missing assign_target_shapes entry` diagnostic. |
| `lowering_facts` | `hew-types/src/check/methods.rs` — `record_hashset_lowering_fact`; current producer call sites are HashSet methods such as `insert`, `contains`, `remove`, `clone`, `len`, `is_empty`, and `clear`. | `hew-types/src/check/methods.rs` — `finalize_lowering_facts`, called from `Checker::check_program`, resolves pending facts through substitution, converts with `LoweringFact::from_hashset_element_type`, emits `InferenceFailed` for unresolved element types, and prunes facts that must not reach codegen. | `hew-serialize/src/msgpack.rs` — `build_lowering_fact_entries`; `TypedProgram.lowering_facts` / wire key `"lowering_facts"`. | `hew-codegen/src/msgpack_reader.cpp` — `parseLoweringFactEntry`; `parseProgram` loads `prog.lowering_facts`. | `hew-codegen/src/mlir/MLIRGen.cpp` — `requireLoweringFactOf`; `hew-codegen/src/mlir/MLIRGenExpr.cpp` consumes the fact in HashSet method lowering and rejects wrong `kind`, `element_type`, or `drop_kind`. | Existing coverage: `hew-types/src/check/tests.rs` has `free_call_len_on_hashset_records_lowering_fact`; `hew-serialize/src/msgpack.rs` has `lowering_facts_serialize_to_wire_field` and `build_lowering_fact_entries_emits_free_call_len_entries`. Gap: no dedicated C++ negative test currently erases a `lowering_facts` entry and asserts the `missing lowering_facts entry` diagnostic, and no standalone reader test checks malformed lowering-fact payloads directly. |

## Cross-cutting findings

1. `expr_types`, `assign_target_kinds`, `assign_target_shapes`, and `lowering_facts` each have an identifiable checker-side output-boundary authority step.
2. `method_call_receiver_kinds` does **not** yet have an equivalent checker-side post-pass validator; its contract is currently enforced by producer discipline and fail-closed MLIR consumption.
3. All five fields are parsed by `hew-codegen/src/msgpack_reader.cpp`, but coverage is mostly indirect through `hew-codegen/tests/test_mlirgen.cpp` rather than field-specific reader unit tests.

## Deferred next steps

These remain intentionally out of scope for this bounded slice, but are the most obvious follow-ons:

1. Add a second matrix slice for the remaining `TypedProgram` side tables and metadata fields.
2. Add dedicated negative tests for erased `assign_target_kinds`, `assign_target_shapes`, and `lowering_facts` entries.
3. Decide whether `method_call_receiver_kinds` needs a checker-side output-boundary validator analogous to `validate_expr_output_contract` / `validate_assign_target_output_contract`.
4. Add direct reader-level tests if Wave 13 requires field-by-field proof at the `msgpack_reader.cpp` boundary rather than only through MLIR integration tests.
