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
