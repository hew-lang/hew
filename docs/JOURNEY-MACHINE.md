# Machine Type — Journey Log

## Phase 0: Design Specification (2025-07-23)

### Agents deployed

- **Multi-model consensus** → Design space exploration (machine as actor vs value type vs trait-based)
- **Sonnet 4.6** → Formal specification authoring

### Key findings

- Three approaches evaluated: machine as actor variant (A), machine as value type (B), machine as trait desugaring (C)
- **Unanimous recommendation: Approach B (Machine as Value Type)**
- Value type compiles to tagged union — zero-cost, no allocations, no threads
- Enables embedding machines in actors, structs, collections (composability)
- Unit-testable without actor runtime (pure transition functions)
- Naturally introduces "enums with payloads and transition logic" to Hew

### Design decisions

**Decision 1: Machine is a value type, not an actor**
Machines define state + transitions as data. Side effects happen in the actor or function that owns the machine. This follows the "Functional Core, Imperative Shell" principle — the machine is the functional core.

**Decision 2: Compiler-checked exhaustiveness over the (State, Event) matrix**
Every cell in the `S × E` matrix must be covered by an explicit transition or a wildcard. This eliminates an entire class of "forgot to handle this event in this state" bugs at compile time.

**Decision 3: Generated `step()` as the primary API**
`m.step(event) -> Machine` is the single entry point for state transitions. It compiles to a nested switch on (state_tag, event_tag). No other mutation path exists.

**Decision 4: Wildcard transitions for default behavior**
`on Event: _ -> _ { self }` fills all uncovered cells for an event. Explicit transitions take priority. This avoids boilerplate while maintaining exhaustiveness.

**Decision 5: Pure transitions, no Mealy outputs in v0.1**
Transition bodies produce only the new state (Moore model). Side effects and output happen outside the machine. Mealy outputs (`-> OutputType`) deferred to a future version.

**Decision 6: State-specific data, not machine-global data**
Each state declares its own fields. There are no machine-level fields shared across states. Data that must persist across states is carried in the state fields themselves or managed by the enclosing actor/struct.

### Artifacts produced

- `docs/specs/MACHINE-SPEC.md` v0.1.0 — formal specification with EBNF grammar, type system integration, generated API, exhaustiveness rules, compilation model, and examples
- `examples/machine-design.md` — RFC with design space evaluation (pre-existing)

### Next steps

1. **Parser** — Add `machine`, `state`, `event`, `on` tokens to `hew-lexer`; add `MachineDecl` AST node to `hew-parser`
2. **Type checker** — Validate exhaustiveness matrix in `hew-types`; generate `{Machine}Event` companion enum
3. **Serialization** — Add machine schema to MessagePack format in `hew-serialize`
4. **Codegen** — Lower `MachineDecl` to tagged union + `step()` switch in `hew-codegen` MLIR pipeline
5. **Tree-sitter** — Add machine grammar to `tree-sitter-hew` for editor support
6. **Examples** — Write circuit breaker, order state machine, and TCP state examples

---

## Phase 1: Lexer and Parser (2025-07-24)

### What was implemented

- **Lexer** (`hew-lexer/src/lib.rs`): Added four new keyword tokens — `Machine`, `State`, `Event`, `On`. All four are registered in the `Token` enum with `#[token(...)]` attributes, the `Display` impl, and the `keyword_str()` method.

- **AST** (`hew-parser/src/ast.rs`): Added four new types — `MachineDecl`, `MachineState`, `MachineEvent`, `MachineTransition`. Added `Machine(MachineDecl)` variant to the `Item` enum. `MachineDecl` carries visibility, name, states, events, and transitions. State/event fields use the same `Vec<(String, Spanned<TypeExpr>)>` pattern as enum struct variants. Transition bodies are stored as `Spanned<Expr>` wrapping an `Expr::Block`.

- **Parser** (`hew-parser/src/parser.rs`): Added `parse_machine_decl()` and `parse_state_pattern()` methods. Machine parsing follows the same pattern as actor/supervisor parsing — loop over the body consuming `state`, `event`, and `on` items. `state` and `event` accept optional `{ field: Type; }` blocks. Transitions parse `on EventName: Source -> Target { body }` using the existing `parse_block()` for bodies. Wildcard patterns (`_`) are detected as `Identifier("_")` from the lexer. Wired into `parse_item()` for both `pub` and non-`pub` positions.

- **Formatter** (`hew-parser/src/fmt.rs`): Added `format_machine()` method that pretty-prints machine declarations with proper indentation. States and events with fields use inline `{ field: Type; }` syntax. Transitions are formatted as `on Event: Source -> Target { ... }`.

- **Cross-crate fixes**: Updated exhaustive `Item` matches in `hew-wasm`, `hew-lsp`, and `hew-cli` (doc extraction, syntax highlighting, folding ranges, symbol lookup, reference collection, identifier counting) to handle the new `Machine` variant.

- **Tests** (`hew-parser/tests/machine_decl.rs`): Four tests covering simple machines, machines with per-state fields, event payloads, and wildcard transitions. All pass alongside the full workspace test suite (0 failures).

### Design decisions

**Decision 1: `state`, `event`, and `on` are full keywords but contextual identifiers**
These are registered as `#[token]` keywords in the lexer for unambiguous matching inside machine bodies. However, they are also listed in `contextual_keyword_name()` in the parser so they can still be used as identifiers in non-machine contexts (e.g., a function parameter named `state`). This avoids breaking existing code while keeping machine parsing simple.

**Decision 2: Transition body is `Expr::Block`, not a separate AST node**
Transition bodies reuse the existing `parse_block()` → `Expr::Block` path. This means transition bodies support the full expression language (if/match/let/etc.) without any new expression types. The spec requires this for conditional transitions like the circuit breaker example.

**Decision 3: Wildcard `_` is stored as a string, not a separate enum**
Source and target state patterns store `"_"` as a plain string rather than using an `Option` or enum. This keeps the AST simple and matches the spec's EBNF where `_` is just a `TransitionSource`/`TransitionTarget` alternative. Semantic validation (wildcard rules, exhaustiveness) belongs in the type checker.

**Decision 4: No type parameters in v0.1 parser**
The spec allows `machine Foo<T> { ... }` but the parser does not yet parse type parameters on machines. This is deliberate — it keeps the initial implementation focused on the core syntax. Type parameters can be added in a follow-up by calling `parse_opt_type_params()` after the machine name, matching how `parse_struct_or_enum()` works.

### Artifacts produced

- `hew-lexer/src/lib.rs` — 4 new keyword tokens
- `hew-parser/src/ast.rs` — 4 new AST types + `Item::Machine` variant
- `hew-parser/src/parser.rs` — `parse_machine_decl()`, `parse_state_pattern()`, wiring in `parse_item()`
- `hew-parser/src/fmt.rs` — `format_machine()` method
- `hew-parser/tests/machine_decl.rs` — 4 parser tests
- Cross-crate fixes in `hew-wasm`, `hew-lsp`, `hew-cli`

### Next steps

1. **Type checker** — Register machine types as tagged unions in `hew-types`; validate exhaustiveness matrix; generate `{Machine}Event` companion enum
2. **Serialization** — Serialize `MachineDecl` to MessagePack in `hew-serialize`
3. **Codegen** — Lower machines to tagged union + `step()` switch in MLIR
4. **Type parameters** — Add `parse_opt_type_params()` to `parse_machine_decl()` for generic machines
5. **Tree-sitter** — Add machine grammar rules to `tree-sitter-hew`

---

## Phase 2: Type Checker (2025-07-25)

### What was implemented

- **`Ty::Machine` variant** (`hew-types/src/ty.rs`): Added a new `Machine { name: String }` variant to the `Ty` enum representing machine types. Updated `map_children`, `Display`, and all downstream match sites (`traits.rs`, `unify.rs`).

- **Type registration** (`hew-types/src/check.rs`): Added `register_machine_decl()` which registers the machine as a `TypeDef` with `TypeDefKind::Machine`. Each state becomes a variant (unit or struct), and unit states get constructor function signatures. Registers all state field types with the trait registry for Send/Frozen derivation.

- **Companion event enum**: The type checker generates a `{MachineName}Event` companion enum type with variants matching the machine's event declarations. This is registered as a standard `TypeDefKind::Enum` type definition.

- **Generated methods**: Registers `step(event: MEvent) -> M` and `state_name() -> String` as methods on the machine type definition.

- **Exhaustiveness checking** (`check_machine_exhaustiveness()`): Validates the state × event matrix. For each state S and event E, checks that either an explicit `on E: S -> ...` transition exists or a wildcard `on E: _ -> ...` covers it. Emits `MachineExhaustivenessError` for uncovered pairs and duplicate explicit transitions.

- **Pattern matching support**: Added `Ty::Machine` case to `check_exhaustiveness()` for `match` expressions, so machine values can be pattern-matched like enums with exhaustiveness checking.

- **Unification**: `Ty::Machine` unifies with itself (same name) and with `Ty::Named` of the same name (for interop with pattern matching and enum-like usage).

- **Cross-crate fixes**: Updated `hew-serialize/src/enrich.rs` (machine type serialization), `hew-lsp/src/server.rs` (diagnostic kind mapping and hover display), and `hew-types/src/traits.rs` (marker trait derivation) to handle the new `Ty::Machine` and `TypeDefKind::Machine` variants.

- **Tests** (`hew-types/tests/machine_typecheck.rs`): Seven tests covering well-formed machines, wildcard transitions, missing transition errors, type def registration, companion event generation, state field registration, and duplicate transition detection. All pass alongside the full workspace suite.

### Design decisions

**Decision 1: `Ty::Machine` is a distinct variant, not `Ty::Named`**
Machines are nominal types with specific semantics (exhaustiveness checking, generated step() method). Using a dedicated `Ty` variant makes it easy to dispatch machine-specific logic without inspecting `TypeDefKind` at every use site. `Ty::Machine` unifies with `Ty::Named` of the same name for interop.

**Decision 2: Exhaustiveness runs in `check_item`, not `collect_types`**
Registration happens in pass 1 (`collect_types`), exhaustiveness validation in pass 3 (`check_item`). This matches how actors and functions are validated — registration first, then semantic checking.

**Decision 3: Transition body type checking deferred to v2**
For this phase, transition bodies pass through with basic expression type checking. Full checking of `self` bindings, event payload scoping, and return type validation against target states will be added incrementally once the core type registration and exhaustiveness checking are proven solid.

**Decision 4: Machine marker traits derive from field types**
Machines implement Send/Frozen/etc. if all their state field types do, matching the behavior of enums and structs. The trait registry receives all field types across all states.

### Artifacts produced

- `hew-types/src/ty.rs` — `Ty::Machine { name }` variant + `map_children` + `Display`
- `hew-types/src/check.rs` — `TypeDefKind::Machine`, `register_machine_decl()`, `check_machine_exhaustiveness()`, match exhaustiveness for `Ty::Machine`
- `hew-types/src/error.rs` — `TypeErrorKind::MachineExhaustivenessError`
- `hew-types/src/unify.rs` — `Ty::Machine` unification rules
- `hew-types/src/traits.rs` — `Ty::Machine` marker trait derivation
- `hew-types/tests/machine_typecheck.rs` — 7 tests
- `hew-serialize/src/enrich.rs` — `Ty::Machine` serialization
- `hew-lsp/src/server.rs` — diagnostic + hover support for machines

### Next steps

1. **Transition body type checking** — Check `self` binding, event payload scoping, and return type validation in transition bodies
2. **Serialization** — Serialize full `MachineDecl` to MessagePack in `hew-serialize`
3. **Codegen** — Lower machines to tagged union + `step()` switch in MLIR
4. **Type parameters** — Add `parse_opt_type_params()` to `parse_machine_decl()` for generic machines
5. **Tree-sitter** — Add machine grammar rules to `tree-sitter-hew`

---

## Phase 2: Serialization (2025-07-25)

### What was implemented

- **Enrichment** (`hew-serialize/src/enrich.rs`): Added `Item::Machine` handling in three enrichment passes — `enrich_item` (enriches transition body expressions), `rewrite_builtin_calls_in_item` (rewrites builtins in transition bodies), and `normalize_item_types` (normalizes type expressions in state fields, event fields, and transition bodies).

- **MessagePack serialization** (`hew-serialize/src/msgpack.rs`): The `MachineDecl` AST type already derives `Serialize`/`Deserialize` via serde, so it serializes through the existing `rmp_serde::to_vec_named` pipeline without additional code. Added a `round_trip_machine_decl` test that verifies a complete machine declaration (with states, events with typed fields, and transitions) survives serialize → deserialize intact.

- **C++ AST types** (`hew-codegen/include/hew/ast_types.h`): Added `MachineState`, `MachineEvent`, `MachineTransition`, and `MachineDecl` structs mirroring the Rust AST. State and event fields use `std::vector<std::pair<std::string, Spanned<TypeExpr>>>` matching the serde tuple serialization. Added `MachineDecl` to the `Item` variant type.

- **C++ MessagePack reader** (`hew-codegen/src/msgpack_reader.cpp`): Added `parseMachineField` (reads serde tuple `[name, spanned_type]`), `parseMachineState`, `parseMachineEvent`, `parseMachineTransition`, and `parseMachineDecl` functions. Wired `"Machine"` variant into `parseItem`. Format matches the Rust serde output exactly.

### Design decisions

**Decision 1: No custom serialization logic needed**
The Rust `MachineDecl` and its children already derive `Serialize`/`Deserialize`. The existing `serialize_to_msgpack` function serializes all `Item` variants uniformly through serde, so Machine serialization works automatically.

**Decision 2: State/event fields as serde tuples**
Rust `Vec<(String, Spanned<TypeExpr>)>` serializes as an array of 2-element arrays `[name, spanned_type]`. The C++ reader uses `parseMachineField` to deserialize this tuple format, matching the serde convention used elsewhere (e.g., struct variant fields).

**Decision 3: Transition body carries full Spanned<Expr>**
Transition bodies are `Spanned<Expr>` (typically `Expr::Block`), preserving source spans for error reporting in codegen. The C++ reader uses the existing `parseSpanned<Expr>` + `parseExpr` infrastructure.

### Artifacts produced

- `hew-serialize/src/enrich.rs` — Machine handling in 3 enrichment passes
- `hew-serialize/src/msgpack.rs` — Round-trip test for MachineDecl
- `hew-codegen/include/hew/ast_types.h` — 4 new C++ structs + Item variant update
- `hew-codegen/src/msgpack_reader.cpp` — 5 new parse functions + Item wiring

### Next steps

1. **Type checker** — Register machine types as tagged unions in `hew-types`; validate exhaustiveness matrix; generate `{Machine}Event` companion enum
2. **Codegen** — Lower machines to tagged union + `step()` switch in MLIR
3. **Type parameters** — Add `parse_opt_type_params()` to `parse_machine_decl()` for generic machines
4. **Tree-sitter** — Add machine grammar rules to `tree-sitter-hew`

---

## Phase 3: MLIR Code Generation (2025-07-26)

### What was implemented

- **Machine type registration** (`MLIRGen.cpp: registerMachineDecl()`): Registers the machine as an enum type in `enumTypes` using an identified LLVM struct type `{ i32 }` (tag-only for unit states) or `{ i32, field1, field2, ... }` (with per-variant payload slots). Machines always use struct layout (never bare `i32`) so that method dispatch (`step()`, `state_name()`) can resolve the type name. Registers both unqualified (`Off`) and qualified (`Light::Off`) variant names in `variantLookup`.

- **Companion event enum**: Registers `{MachineName}Event` as a standard enum type with its own variants and lookup entries. Events follow the regular enum layout rules (unit-only events use `i32`, payload events use struct).

- **State constructor functions**: State constructors work through the existing `EnumConstructOp` infrastructure. Qualified names like `Light::Off` and unqualified names like `Off` both resolve correctly via `variantLookup`.

- **`state_name()` function** (`MLIRGen.cpp: generateMachineDecl()`): Generates a function `MachineName__state_name(self) -> ptr` that extracts the tag and returns the corresponding state name string. Uses `arith.select` chain with global string constants. Returns `!llvm.ptr` (not `!hew.string_ref`) to avoid unrealized_conversion_cast issues.

- **`step()` function**: Generates a function `MachineName__step(self, event) -> MachineName` that implements a tag-based state transition table. For v1, transitions only change the tag (transition bodies are not executed). Uses a flat if-else chain over `(stateTag, eventTag)` pairs with `arith.cmpi` + `arith.andi` + `arith.select`. Wildcard transitions (`on Event: _ -> _`) fill all state slots not covered by explicit transitions.

- **Pattern matching support**: Machine values participate in pattern matching identically to enum values via the existing `EnumExtractTagOp` / `EnumExtractPayloadOp` infrastructure. Both `match m { On => ..., Off => ... }` and `match m { _ => ... }` work.

- **Type checker fixes** (`hew-types/src/check.rs`):
  - Added qualified variant name resolution for `Expr::Identifier` — `Light::Off` is now resolved by splitting on `::` and looking up the variant in the type definition.
  - Added `Ty::Machine` arm in `check_method_call()` so that `step()` and `state_name()` method calls on machine values resolve to the registered method signatures.

- **Pipeline integration**: Machine type registration runs in Pass 1b (alongside struct/enum registration). Machine function generation runs in Pass 1k0 (before user function bodies in Pass 1k), so user code can call machine methods.

### Design decisions

**Decision 1: Machines always use identified struct type, even for unit-only states**
Regular enums with all-unit variants use bare `i32`. Machines always wrap the tag in a named struct (`!llvm.struct<"Light", (i32)>`) so that method call dispatch can resolve the type name. This is a deliberate divergence from enum layout to enable the `step()` and `state_name()` generated methods.

**Decision 2: Per-variant payload positions for state fields**
When states have fields, each variant gets its own dedicated struct slots (per-variant layout), not union-style shared positions. This avoids type mismatches between states that have different field types or counts, at the cost of a larger struct. Union-style optimization can be added later.

**Decision 3: Tag-only step() for v1 (no transition body execution)**
The step function only changes the tag to the target state's index. Transition bodies from the AST are not compiled or executed. This is sufficient for state machines where the transition logic is "go to state X" without computing new field values. Full body execution requires scoping `self` fields and event payloads, which is deferred to v2.

**Decision 4: state_name() returns !llvm.ptr, not !hew.string_ref**
Using the Hew `StringRefType` in `arith.select` would leave `unrealized_conversion_cast` ops that fail during final lowering. Returning `!llvm.ptr` directly avoids this by staying at the LLVM level for the select chain.

### Artifacts produced

- `hew-codegen/include/hew/mlir/MLIRGen.h` — `registerMachineDecl()`, `generateMachineDecl()` declarations
- `hew-codegen/src/mlir/MLIRGen.cpp` — Machine registration (type + event enum), step/state_name codegen, pipeline wiring
- `hew-types/src/check.rs` — Qualified variant resolution, `Ty::Machine` method dispatch

### Next steps

1. **Transition body execution** — Compile transition bodies with `self` field extraction and event payload scoping
2. **State fields in step()** — Copy/transform payload fields during transitions
3. **Type parameters** — Add `parse_opt_type_params()` to `parse_machine_decl()` for generic machines
4. **Tree-sitter** — Add machine grammar rules to `tree-sitter-hew`

---

## Phase 4: End-to-End Tests (2025-07-27)

### What was implemented

Comprehensive E2E test suite for the `machine` type covering all implemented features.

### Test files created

- **`e2e_machine/machine_basic.hew`** — Basic toggle switch (2 states, 1 event). Validates `Light::Off` qualified constructor, `state_name()`, `step()`, and round-trip toggling (Off → On → Off).

- **`e2e_machine/machine_traffic.hew`** — Traffic light (3 states, 2 events). Validates cyclic transitions (Red → Green → Yellow → Red), wildcard emergency transition (`on Emergency: _ -> Red`), and multi-step sequencing.

- **`e2e_machine/machine_match.hew`** — Door lock (3 states, 4 events). Validates pattern matching on machine values (`match d { Locked => ..., Unlocked => ..., Open => ... }`), wildcard self-transitions (`on Lock: _ -> _ { self }`), and invalid-action resilience (pushing a locked door stays locked).

- **`e2e_machine/machine_qualified_event.hew`** — Qualified event constructor syntax. Validates `LightEvent::Toggle` (companion enum qualified name) as an alternative to bare `Toggle`.

- **`e2e_machine/machine_exhaustive_fail.hew`** — Negative test (reject). Validates that the type checker produces "does not handle event" errors for uncovered (state, event) cells.

### CTest registration

All 5 tests registered in `hew-codegen/tests/CMakeLists.txt`:
- 4 `add_e2e_file_test` entries (compile + run + compare output)
- 1 `add_e2e_reject_test` entry (compile must fail with expected error)

### Test results

All tests pass:
- `machine_basic`: Off → On → Off ✓
- `machine_traffic`: Red → Green → Yellow → Red, emergency reset ✓
- `machine_match`: Locked → Unlocked → Open → Unlocked → Locked, invalid push ignored ✓
- `machine_qualified_event`: LightEvent::Toggle qualified syntax ✓
- `machine_exhaustive_fail`: 3 errors for unhandled transitions ✓

### Next steps

1. **Transition body execution** — Compile transition bodies with `self` field extraction and event payload scoping
2. **State fields in step()** — Copy/transform payload fields during transitions
3. **Type parameters** — Add `parse_opt_type_params()` to `parse_machine_decl()` for generic machines
4. **Tree-sitter** — Add machine grammar rules to `tree-sitter-hew`

## Type Checker Validation Hardening

Closed six validation gaps in `check_machine_exhaustiveness`:

| Fix | What changed |
|-----|-------------|
| Unknown states | Reject transitions referencing undeclared `source_state` / `target_state` |
| Unknown events | Reject transitions referencing undeclared `event_name` |
| Duplicate wildcards | Detect two `_ -> …` transitions for the same event |
| Duplicate explicit | Detect two `S -> …` transitions for the same (state, event) |
| Minimum cardinality | Require ≥ 2 states and ≥ 1 event |
| Body validation | Synthesize each transition body with `self` in scope |

All six fixes live in `hew-types/src/check.rs` inside `check_machine_exhaustiveness`.
Tests in `hew-types/tests/machine_typecheck.rs` (13 tests, all passing).
