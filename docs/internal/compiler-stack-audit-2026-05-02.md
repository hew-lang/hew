# Compiler Stack Audit — 2026-05-02

**Source:** Issue #1566 — Compiler stack health audit  
**Scope:** Synthesis of a 10-stage compiler audit (parser through runtime)  
**Date:** 2026-05-02

---

## Completeness rating rubric

Ratings follow the LESSONS.md priority-band convention (P0 = highest):

| Rating | Meaning |
|--------|---------|
| **P0** | Gaps that could cause silent wrong-code, memory-safety violations, or cross-language boundary failures |
| **P1** | Incomplete paths, missing diagnostics, test-coverage holes, or user-visible quality gaps |
| **P2** | Maintenance and architecture concerns; no immediate correctness risk |

---

## Section 1 — Per-stage summary

### Stage 1: Lexer (`hew-lexer`)

**Entrypoint:** `lex(source: &str) -> Vec<(Token<'_>, Span)>` at `hew-lexer/src/lib.rs:21`; streaming iterator via `Lexer<'src>` (line 41).

**Rating: P2 (mature)**

The lexer is the healthiest stage in the pipeline. It has no `panic!`, `todo!`, or `unimplemented!` in any production path. All major token forms are covered — keywords, multi-char operators, string/char literals, nested block comments, and `Token::Error` for malformed input. Spans are byte-offset pairs, correctly deferred to line/column mapping downstream. The single `unreachable!()` at line 532 sits behind an exhaustive match arm and is genuinely unreachable. No action needed.

---

### Stage 2: Parser (`hew-parser`)

**Entrypoint:** `parse(source: &str) -> ParseResult` at `hew-parser/src/lib.rs:10`; internal `Parser<'src>` struct at `parser.rs:329`.

**Rating: P1**

The parser is feature-complete for valid input. The 77 `panic!()` calls in `parser.rs` lines 5062–5787 are all inside `#[test]` functions (assertion panics checking expected AST shapes, not recovery paths). Production parse code has no bare panics. The genuine P1 issue is that all `ParseError.message` fields are free-form strings built via `format!()` — no structured `enum ParseErrorKind` exists — which prevents LSP consumers from extracting semantic error information without string parsing. The depth guard (max 256 recursion) is sound. Error recovery partially implemented; no crashes on malformed input but some constructs fall through with low-quality messages.

---

### Stage 3: Name resolution / module loading (`hew-types`)

**Entrypoint:** `ModuleRegistry::load(module_path: &str) -> Result<&ModuleInfo, ModuleError>` at `hew-types/src/module_registry.rs:163`; underlying `load_module_checked()` at `stdlib_loader.rs:62`.

**Rating: P1**

Production module loading uses `ModuleRegistry::load()` → `load_module_checked()`, which correctly propagates `ModuleError::ParseError` when a module file is malformed. The public convenience `load_module()` (line 56) silently converts errors to `None` via `.ok().flatten()`, but inspection confirms it is only called in tests. The meaningful gaps are: (1) `ModuleInfo` represents C function signatures and handle methods as raw tuples (e.g., `(String, Vec<Ty>, Ty)`, `((String, String), String, Vec<Ty>, Ty)`) — no named struct wrappers; (2) the `ModuleRegistry` keying is `HashMap<String, ModuleInfo>` with string paths rather than a `ModuleId` newtype; (3) module-name collisions are silently last-insert-wins in `ModuleGraph`; (4) `ModuleId` exists in `module.rs` but is not used at the loader boundary.

---

### Stage 4: Type checking / inference (`hew-types`)

**Entrypoint:** `Checker::check_program(&Program)` at `hew-types/src/check/mod.rs:139`; `Checker::default()` + `register_builtins()` at lines 60–140.

**Rating: P1**

Bidirectional inference with constraint gathering, exhaustive pattern-match checking, marker trait derivation (Send, Frozen, Copy), and an output-contract validation pass (`validate_checker_output_contract`) are all present and sound. Deferred monomorphic site records for trait bounds that require concrete type arguments are intentional design (tracked at #1565). Generator types (`Ty::Named { name: "Generator", .. }`) cannot be serialised to lower IR (`enrich.rs:450–454`) — intentional post-MVP exclusion. Numeric literal defaulting (`materialize_literal_defaults()` at `check/mod.rs:235`) is unaudited but appears well-scoped. No substitution memoization means type-variable chains can grow before resolution, but this is a performance concern, not a correctness one.

---

### Stage 5: Upper Hew IR / lowering facts (`hew-types`)

**Entrypoint:** `Checker::finalize_lowering_facts()` at `check/mod.rs:301`; validated by `admissibility::validate_lowering_facts_output_contract` (line 302).

**Rating: P1**

`LoweringFact` for `HashSet` is fully specified (element type + ABI variant + drop kind) with complete unit-test coverage. Deferred admission structures for `HashMap` and `Vec` exist (`DeferredHashMapAdmission`, `DeferredVecAdmission` in `check/types.rs:163–192`); their finalization bodies — `finalize_hashmap_admission` at `check/methods.rs:111`, `finalize_hashset_admission` at `check/methods.rs:194`, and `finalize_vec_admission` at `check/methods.rs:244` — are fully implemented. Unit-test coverage for these three finalization paths is absent; that gap is addressed by #1575. The fact table is keyed by source `SpanKey`, not monomorphic instance name, which means stage-7 codegen must preserve span identity across generic instantiation (a dependency on #1565's design). Generators are intentionally rejected at this stage.

---

### Stage 6: Monomorphization

**Entrypoint:** Responsibility of C++ codegen (`hew-codegen`); checker provides `call_type_args: HashMap<SpanKey, Vec<Ty>>` and `fn_sigs` / `type_defs` as structured inputs.

**Rating: P1 (deferred by design)**

Monomorphization is implemented in C++ codegen, not in `hew-types`. Trait bound checking for `Send`/`Rc<T>` is deferred to monomorphization time via `DeferredMonomorphicSite` records, intentionally parked at #1565. Type arguments flow as structured `Vec<Ty>`, not pre-stringified instance names. Name-mangling scheme and recursive instantiation correctness are not yet audited. This rating reflects design-completeness intentionally deferred (#1565), not a production gap.

---

### Stage 7: Lower Hew IR → MLIR (`hew-codegen`)

**Entrypoint:** `MLIRGen::generate(const ast::Program &prog)` at `hew-codegen/src/mlir/MLIRGen.cpp:100`.

**Rating: P1**

The Rust→C++ msgpack boundary (`codegen_capi.cpp:108–127`) is typed and version-checked (schema version 8, hard-fail on mismatch at `msgpack_reader.cpp:1888`). PR #1530 closed exit-0 astgen failures. MLIR generation has correct `emitError()` paths for unsupported wire types, or-patterns, and unsupported cast types. The WASM unsupported-ops gate (`codegen.cpp:4346–4417`) correctly rejects supervision/link/scope ops, but fires after MLIR generation — code is emitted before the gate checks it. No integration tests directly exercise MLIR generation rejecting a malformed but type-system-valid AST; coverage relies on the frontend preventing invalid constructs. Wire codec unsupported branches (`MLIRGenWire.cpp:450–459`, `662`, `680`, `779`, `1058`) lack dedicated diagnostic test cases.

---

### Stage 8: MLIR → LLVM IR (`hew-codegen`)

**Entrypoint:** `Codegen::buildLLVMModule(mlir::ModuleOp, ...)` at `hew-codegen/src/codegen.cpp:5670`.

**Rating: P2**

MLIR verifier runs before LLVM export. All custom Hew dialect ops lower to runtime calls. Fail-closed error paths exist for unsupported print types, cast types, and constant types. `SealPanicBlocksPass` (lines 4794–4850) prevents control-flow fallthrough from unmatched branches. Source locations (`FileLineColLoc`) flow through to LLVM IR debug metadata. No known correctness gaps.

---

### Stage 9: LLVM IR → object file (`hew-codegen`)

**Entrypoint:** `emitObjectFile(llvm::Module &, ...)` at `hew-codegen/src/codegen.cpp:5608`.

**Rating: P2**

Standard LLVM backend path. Target machine creation fails explicitly if target triple is not found. O2 optimisation hardcoded for non-debug builds. Coroutine passes in place for potential async use. No dedicated unit test for the object emit step — relies on CLI e2e tests. No known gaps.

---

### Stage 10: Link object → executable (`hew-cli`)

**Entrypoint:** `link_executable(...)` at `hew-cli/src/link.rs:111`.

**Rating: P2**

Target-driven routing (native vs WASM), linker probe (clang > cc), explicit SDK path assembly. No platform assumptions outside the `NativeLinkPlan` abstraction. System linker is a black box; no dedicated unit test warranted or feasible. No known gaps.

---

### Runtime

**Entrypoint:** `hew-runtime/src/actor.rs` (actor symbols), `hew-cabi/src/` (vec/sink/option), `hew-runtime/src/` (log, timer, random, shutdown).

**Rating: P2**

All `no_mangle` C-ABI symbols use `pub unsafe extern "C"`. Option tagged-union layout, reply-channel tag validation, vector boundary checks, and sink stream checks were hardened in #1328. Actor type registration is mandatory (unregistered dispatch panics). Non-blocking sends return EAGAIN (no silent drops). No latent ABI bugs identified.

---

## Section 2 — Top 5 architectural findings

### Finding 1: Parser error messages are unstructured strings

**Description:** All `ParseError.message` fields in `hew-parser` are built via `format!()` or `.to_string()`. No `enum ParseErrorKind` with typed variants like `UnexpectedToken { expected: Token, found: Token }` exists. Downstream consumers (LSP, IDE, CI formatters) cannot extract structured error information without string parsing.

**Citations:**
- `hew-parser/src/parser.rs:350–360` — `ParseError` struct definition; `message: String`
- `hew-parser/src/parser.rs:414–433` — error construction via `format!("expected {expected:?}, found {found:?}")`

**Fix shape:** Introduce `enum ParseDiagnosticKind` with typed variants. Retain the rendered `message: String` for backward compatibility during transition but populate `kind` for structured consumers.

**PR size:** M (medium — touches many error construction sites across parser.rs, but mechanical)

**Action:** Filed as #1567.

---

### Finding 2: No structured JSON diagnostic format for LSP consumers

**Description:** The `TypeError` type has spans, notes, suggestions, and a typed `kind` field — but no serialisation path to JSON. `hew eval --json` captures diagnostics as rendered text (`start_diagnostic_capture()` in `diagnostic.rs:31–39`). LSP clients likely consume rendered strings rather than structured `{ kind, span, message, notes, suggestions }` objects, preventing rich IDE integration (inline hints, quick-fixes, multi-span highlights).

**Citations:**
- `hew-cli/src/diagnostic.rs:31–39` — text capture, not structured
- `hew-lsp/src/` — no JSON encoder for `TypeError` found
- audit Section 2, Finding 2

**Fix shape:** Add a `TypeError::to_lsp_diagnostic()` conversion (or a serde-serialisable mirror struct) and wire it through `hew-lsp`. This is the LSP integration gap, not a compiler correctness issue.

**PR size:** M

**Action:** Filed as #1568.

---

### Finding 3: `ModuleInfo` uses raw tuples for C signatures and handle methods

**Description:** `ModuleInfo` (at `hew-types/src/stdlib_loader.rs:19`) represents C function signatures as `(String, Vec<Ty>, Ty)` and handle method info as `((String, String), String, Vec<Ty>, Ty)`. There are no named struct wrappers. Type-checker consumers must destructure these positionally, making the code brittle and hard to evolve.

**Citations:**
- `hew-types/src/stdlib_loader.rs:19–46` — `ModuleInfo` struct with tuple fields

**Fix shape:** Define `struct CFunction { name, params, return_type }` and `struct HandleMethod { type_name, method_name, c_symbol, params, return_type }`. Replace tuple fields in `ModuleInfo`. Mechanical refactor with no behaviour change.

**PR size:** S

**Action:** Filed as #1569.

---

### Finding 4: Module path string-keying (no `ModuleId` newtype at loader boundary)

**Description:** `ModuleRegistry` keys modules by `String` (`HashMap<String, ModuleInfo>`), and `load_module_checked()` accepts `module_path: &str`. A `ModuleId` newtype exists in `module.rs` (line 21) but is not used at this boundary. Paths like `"std::encoding::json"` are split dynamically on every resolution call; no deduplication or interning.

**Citations:**
- `hew-types/src/module_registry.rs:163` — `load(&mut self, module_path: &str)`
- `hew-types/src/stdlib_loader.rs:56` — `load_module(module_path: &str, ...)`
- `hew-types/src/module.rs:21` — `ModuleId` defined but not used here

**Fix shape:** Use `ModuleId` (or a `ModulePath` newtype) as the key at the registry boundary. Split path segments once at construction, not on every call.

**PR size:** S

**Action:** Filed as #1570.

---

### Finding 5: Magic builtins not yet trait-bounded (tracked at #1565)

**Description:** Channel trait bounds (`Send`, `Rc<T>`) cannot be checked at type-checking time for generic call sites; they are deferred to monomorphization. `DeferredMonomorphicSite` records track these sites. Already tracked at #1565.

**Citations:**
- `hew-types/src/check/mod.rs:312` — `report_unresolved_monomorphic_sites`
- `hew-types/src/check/types.rs` — `DeferredMonomorphicSite` definition

**Action:** No new issue; #1565 covers this.

---

## Section 3 — IR-format opportunities

| Boundary | Status | Finding | Action |
|----------|--------|---------|--------|
| `Ty` in type checker | Structured; no string drift | Types remain as `Ty` enum until final `ty_to_type_expr()` conversion for serialisation | No action |
| Msgpack boundary `hew-astgen` ↔ `hew-codegen` | Typed, version-checked, fail-closed | Schema version 8; exact-match gate at `msgpack_reader.cpp:1888`; #1530 completed | No action |
| Diagnostic serialisation | Gap | No JSON serialiser for `TypeError`; LSP sees rendered text (Section 2, Finding 2) | #1568 |
| Module path string-keying | Minor gap | `ModuleRegistry` keyed by `String`, not `ModuleId`; dynamic path splitting on every call (Section 2, Finding 4) | #1570 |
| `ModuleInfo` tuple types | Minor gap | C function and handle method signatures as raw tuples (Section 2, Finding 3) | #1569 |

---

## Section 4 — Layering critique

### `Program.module_graph: Option<ModuleGraph>` coupling (stages 2–3)

**Finding:** The parser's `Program` struct carries an `Option<ModuleGraph>` field (`ast.rs:18`), populated only when the caller explicitly requests module resolution. This blurs the stage 2 / stage 3 boundary.

**Assessment:** Intentional optimisation. The `Option<>` wrapper keeps the base parse lightweight; callers that do not need module resolution never pay the cost. The coupling is visible and documented. Not a layering violation in practice — it is a deliberate API seam.

---

### C function signatures in `ModuleInfo` carry stage-4-level type information (stages 3–4)

**Finding:** `ModuleInfo` contains `Vec<Ty>` — fully resolved Hew types — extracted from parsed extern declarations. Stage 3 (module loading) is producing stage-4 (type) artefacts.

**Assessment:** Necessary. External C modules provide fully-resolved types at load time; the type checker needs them to check call sites. The coupling is unavoidable for FFI modules. Not a violation; an intentional bridging seam. The fix would be to return unresolved type-expression AST and let the type checker resolve — but that would require the type checker to understand C extern declarations, which is strictly worse.

---

### MLIR attribute strings as cross-stage contracts (stages 7–8)

**Finding:** Attribute names like `"hew.ptr_width"`, `"hew.debug.param_name"`, `"hew.explicit_dyn_param"` are string-keyed MLIR attributes consulted during MLIR → LLVM lowering.

**Assessment:** Intentional MLIR idiom. MLIR pass attributes are conventionally string-keyed and verified by the IR type system at the op level. These are optional hints (parameter names, debug info) or explicit contracts (`ptr_width`). Not a violation; this is how MLIR dialect attributes work. The `ptr_width` attribute is set before lowering and read during lowering — a genuine contract, but explicit and documented.

---

### LoweringFacts keyed by source span, not monomorphic instance (stages 5–7)

**Finding:** `lowering_facts: HashMap<SpanKey, LoweringFact>` is indexed by the original source expression span. Stage-7 codegen must preserve span identity across generic instantiation to look up facts for each concrete use site.

**Assessment:** Architectural constraint with downstream impact. If monomorphization (stage 6, #1565) creates instances that lose span identity, the lowering-facts side-table lookup will silently miss. This dependency should be noted on #1565 as a design constraint.

---

### WASM unsupported-ops gate fires after MLIR generation (stage 7)

**Finding:** `codegen.cpp:validateWasmUnsupportedOps()` runs after `MLIRGen::generate()` — supervision, link/monitor, and scope ops are emitted to MLIR before the gate rejects them.

**Assessment:** Latent inefficiency but not a correctness issue. The gate does reject correctly; it just does so after spending time generating code that will be discarded. A pre-generation gate (checking WASM target before lowering actor ops) would be more efficient and give better error messages, but the current behaviour is not incorrect. Filed as #1576.

---

## Section 5 — Test coverage gaps

### Stage 2: Parser

- **Parser error recovery paths** have a dedicated test file (`parser_error_recovery.rs`) but the 77 in-test panics indicate many test assertions use `panic!()` for unexpected shape rather than `expect!` with context. This is test quality, not production quality.
- **No negative test for malformed interpolated strings** showing span context propagation through the sub-parser recursion (`parser.rs:241–256`).

### Stage 3: Name resolution

- **Module name collision:** `ModuleGraph` silently uses last-insert-wins for duplicate module names. No test verifies collision detection or surfaces a user-visible error. Filed as #1571.
- **Import cycle span reporting:** Cycle detection exists but reports as a string, not as multi-span diagnostic. No test verifies that each import in a cycle gets its own span annotation. Filed as #1573.

### Stage 5: Upper IR / lowering facts

- **HashMap and Vec admission:** `DeferredHashMapAdmission` and `DeferredVecAdmission` structs exist and their `finalize_*` implementations are fully present in `check/methods.rs` (lines 111, 194, 244). Unit-test coverage for these finalization paths is absent. Filed as #1575.

### Stage 7: MLIR generation

- **Wire codec unsupported branches:** The four `emitError()` sites in `MLIRGenWire.cpp` (`450–459`, `662`, `680`, `779`, `1058`) for unsupported field types and enum payloads have no dedicated diagnostic test cases. Coverage relies on the type checker preventing these inputs. Filed as #1572.
- **Msgpack roundtrip:** No dedicated test exercises `msgpack_reader` on a hand-crafted payload. All deserialization coverage is incidental (goes through full compile pipeline). Filed as #1574.

### Cross-cutting: Diagnostics

- **LSP diagnostic path:** No automated test exercises the path from `TypeError` → LSP client response. The gap between structured `TypeError` and LSP `Diagnostic` format is untested. Filed as #1568.

---

## Section 6 — Recommended next architectural improvements (ranked)

| Rank | Title | Source | PR size | Impact | Dependencies | GH issue |
|------|-------|--------|---------|--------|--------------|---------|
| 1 | Structured `ParseDiagnosticKind` enum | Section 2, Finding 1 | M | LSP/IDE structured errors; prerequisite for #1568 | None | #1567 |
| 2 | Structured JSON diagnostic format for LSP | Section 2, Finding 2 | M | IDE quick-fixes, multi-span highlights in `hew-lsp` | Depends on #1567 for parser errors | #1568 |
| 3 | `ModuleInfo` tuple → named structs (`CFunction`, `HandleMethod`) | Section 2, Finding 3 | S | Code clarity; prevents positional destructuring bugs | None | #1569 |
| 4 | `ModuleId` newtype at loader boundary | Section 2, Finding 4; Section 4 | S | Eliminates repeated string splitting; type safety | None | #1570 |
| 5 | Module name collision detection in `ModuleGraph` | Section 5, Stage 3 gaps | S | Surfaces silent last-insert-wins bugs in user-facing errors | None | #1571 |
| 6 | Wire codec unsupported-branch diagnostic tests | Section 5, Stage 7 | S | Covers five `emitError()` paths in `MLIRGenWire.cpp` with regression tests | None | #1572 |
| 7 | Import cycle span reporting (multi-span per import) | Section 5, Stage 3 gaps | S | Better cycle error messages with per-import source spans | None | #1573 |
| 8 | Msgpack roundtrip unit test | Section 5, Stage 7 | S | Catches deserializer drift without running full compile | None | #1574 |
| 9 | HashMap/Vec lowering admission audit + tests | Section 5, Stage 5 | S | Confirm `finalize_hashmap_admission` / `finalize_vec_admission` bodies exist and are covered | None | #1575 |
| 10 | LoweringFacts span-identity constraint documented for #1565 | Section 4, Stage 5 | XS | Prevents silent fact-lookup misses during monomorphization | Blocks on #1565 design | Comment on #1565 (no new issue) |
| 11 | WASM unsupported-ops pre-generation gate | Section 4, Stage 7 | S | Earlier rejection + better error messages for WASM targets | None | #1576 |
| 12 | `load_module()` public API clarification | Section 1, Stage 3 | XS | Clarify that only `ModuleRegistry::load()` is for production use; public `load_module()` is test-only | None | No issue — doc comment fix only |

---

## Section 7 — GitHub issues filed

| Issue | Title | Rank in Section 6 |
|-------|-------|-------------------|
| #1567 | parser: introduce structured ParseDiagnosticKind enum | 1 |
| #1568 | lsp: add structured JSON diagnostic serialisation (TypeError → LSP Diagnostic) | 2 |
| #1569 | types: replace ModuleInfo tuple fields with named structs (CFunction, HandleMethod) | 3 |
| #1570 | types: use ModuleId newtype at module registry/loader boundary | 4 |
| #1571 | types: detect and surface module name collisions in ModuleGraph | 5 |
| #1572 | codegen: add diagnostic tests for MLIRGenWire unsupported-type branches | 6 |
| #1573 | parser: surface import-cycle errors with per-import span annotations | 7 |
| #1574 | codegen: add msgpack roundtrip unit test for AST deserialization | 8 |
| #1575 | types: verify and test HashMap/Vec lowering admission finalization | 9 |
| #1576 | codegen: move WASM unsupported-ops gate before MLIR generation | 11 |

All issues cite this audit doc as the source and link to #1566 as the parent.
