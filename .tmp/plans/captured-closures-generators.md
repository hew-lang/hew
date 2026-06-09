# Captured closures + generators substrate lane for v0.5

## Status: Superseded — deferred to v0.6+

Closures with captured environment and generators are explicitly deferred to the
next edition in `docs/specs/HEW-SPEC-2026.md:4684–4689`. This plan's acceptance
block no longer reflects the v0.5 ship contract. Retained for historical
reference; follow-on work belongs in v0.6 planning.

## 1. Mission tenet served
Tenet 1, reliability first: closures and generators must produce diagnostics or
real traps, never fabricated success. The mission defines reliability as
fail-closed, predictable, and deterministic at
`~/.claude/projects/-Users-slepp-projects-hew-lang-hew/memory/project_hew_mission.md:28`.
Tenet 2, actor model and supervision first-class: closures captured into
spawned work become part of actor/task execution and must inherit cancellation,
supervision, and actor-boundary safety. The mission names spawn, link, monitor,
supervisors, and lifecycle hooks as first-class at `project_hew_mission.md:30`.
Tenet 3, substrate over surface: first-class function values and generator
state machines are reusable compiler/runtime primitives, not demo syntax. The
mission requires substrate-first iteration at `project_hew_mission.md:32`.
Tenet 4, no legacy: v0.5 can make a clean closure-capture decision without
Rust's three closure flavors or user-facing lifetimes. The mission prefers clean
breaks at `project_hew_mission.md:34`.
Tenet 5, cross-platform parity: closure environments and generator state
machines must behave the same on native and WASI, or be explicitly gated. The
mission requires real fail-closed WASM semantics at `project_hew_mission.md:36`.
The mission explicitly promoted captured closures + generators, cancellation
tokens, and auto-injected locks into v0.5 substrate at
`project_hew_mission.md:59-62`.
Permanent non-goals apply at planning time: no HKTs, no specialization, no
user-facing lifetimes, no browser runtime VM, and no untracked WASM deferrals
(`project_hew_mission.md:38-44`).
This is substrate because it creates:
- first-class callable values backed by compiler-built environment records;
- resumable state-machine values backed by compiler-built generator records;
- a shared foundation for Iterator, spawn, pipeline, and callback consumers.
Success is not "one closure demo compiles." Success is end-to-end parser,
checker, HIR, MIR, codegen, runtime, analysis, serialization, diagnostics, and
tests with fail-closed seams.
## 2. Substrate ownership claimed
This lane owns first-class function values with captured environments.
It owns the closure surface:
- `|x| x + 1`;
- `|| 42`;
- `|x: i32| -> i32 { x + 1 }`;
- `move |x| ...` for explicit move capture where needed.
It owns by-value capture semantics.
It owns explicit move requirements for affine, linear, or non-copy captures.
It owns closure call checking and closure call lowering.
It owns closure environment layout and drop behavior.
It owns actor/task-boundary validation for closures captured into spawned work.
It owns the fail-closed rule for unresolved, moved, unsafe, or unmaterialized
captures.
It owns `gen { yield ...; }` block expressions.
It owns generator yield-type inference and checking.
It owns generator state record synthesis.
It owns generator state-machine lowering.
It owns auto-generated `impl Iterator for <GenN>`.
It owns generator cancellation, drop, and end-state semantics.
It owns tests proving `for` consumes generator blocks through `Iterator::next`.
Future consumers:
- user-defined iterator sources;
- lazy pipeline combinators such as `collect`, `map`, and `filter`;
- async generators;
- callback-style APIs that accept functions as values;
- actor supervisor hooks that accept local function values;
- stream adapters that bridge Iterator and actor mailboxes;
- pipeline DSL sugar that desugars to closure chains.
This lane does not own the full lazy pipeline library.
It does not own specialization for closure/generator calls.
It does not own HKTs for iterator transformers.
It does not own user-facing lifetimes.
It does not own remote serialization of closure environments.
It does not own untracked WASM deferrals.
## 3. Current state audit
The parser AST already has general `Expr::Lambda` with `is_move`, optional type
params, params, return type, and body (`hew-parser/src/ast.rs:172-178`).
The parser AST also has `Expr::SpawnLambdaActor`, the M2 slice 3.5 foundation
for actor-lambda bodies (`hew-parser/src/ast.rs:183-188`).
The parser currently parses legacy expression lambdas with parenthesized params
and `=>` (`hew-parser/src/parser.rs:5144-5181`).
The parser currently parses `move (...) => ...` lambdas
(`hew-parser/src/parser.rs:5415-5433`).
The parser already parses actor-lambda literals as `actor |params| { body }`
(`hew-parser/src/parser.rs:5300-5338`); that pipe parser shape should guide the
new closure surface.
The HIR already has `HirExprKind::SpawnLambdaActor` carrying parameter
bindings, reply type, body, and resolved capture set
(`hew-hir/src/node.rs:563-585`).
The HIR capture model has `HirLambdaCapture` with binding id, source name, and
capture kind (`hew-hir/src/node.rs:688-710`).
The HIR lowerer resolves actor-lambda captures after lowering the body in a
fresh scope (`hew-hir/src/lower.rs:2846-2915`).
The HIR capture walker deduplicates captures and excludes lambda params
(`hew-hir/src/lower.rs:2918-2937`).
The HIR verifier checks that actor-lambda capture binding ids exist
(`hew-hir/src/verify.rs:184-203`).
The HIR tests cover nested actor-lambda capture scoping
(`hew-hir/tests/vertical.rs:977-1073`).
The HIR tests cover typed actor-let forward binding and weak self capture
(`hew-hir/tests/vertical.rs:1075-1134`).
Do not copy the weak actor self-capture rule into general closures; it is an
actor-handle recursion rule, not a general function-value rule.
The MIR lowerer has `lower_spawn_lambda_actor`
(`hew-mir/src/lower.rs:3201-3265`).
It forwards actor-lambda capture metadata into `lambda_captures`.
It currently silently skips captures whose source binding has no backend slot
(`hew-mir/src/lower.rs:3245-3257`).
That skip is a known anti-pattern for this lane. General closures must
compile-fail when any capture cannot be materialized.
The MIR model has `Place::LambdaActorHandle`
(`hew-mir/src/model.rs:305-310`).
General closures need a distinct MIR representation; do not reuse
`LambdaActorHandle`.
The type checker already synthesizes/checks `Expr::Lambda`
(`hew-types/src/check/expressions.rs:266-280`).
The checker already propagates expected function types into lambda checking
(`hew-types/src/check/expressions.rs:1295-1319`).
The checker tracks lambda capture depth
(`hew-types/src/check/expressions.rs:3337-3345`).
Identifier synthesis records capture types for names resolved outside the
lambda boundary (`hew-types/src/check/expressions.rs:799-804`).
The checker returns `Ty::Function` when no captures exist and `Ty::Closure`
when captures exist (`hew-types/src/check/expressions.rs:3488-3508`).
The call checker accepts both `Ty::Function` and `Ty::Closure`
(`hew-types/src/check/calls.rs:772-789`).
The type display path prints both as function-shaped types
(`hew-types/src/ty.rs:504-514`).
Stack-hint classification already treats lambda literals as closure environment
heap allocations (`hew-types/src/check/expressions.rs:4034-4044`).
The builtins file defines `Iterator { type Item; fn next(self) ->
Option<Self::Item>; }` (`std/builtins.hew:123-133`).
The builtins file defines `Pid { type Msg; fn tell(...) }`
(`std/builtins.hew:64-68`).
`gen` is reserved in syntax data (`docs/syntax-data.json:24`).
`yield` is reserved in syntax data (`docs/syntax-data.json:16`).
Both appear in the flat keyword list (`docs/syntax-data.json:172-173`).
Grammar docs already mention `gen fn`, `async gen fn`, and yield expressions
(`docs/specs/grammar.ebnf:167-168`, `docs/specs/grammar.ebnf:284`).
The parser parses `gen fn` and `async gen fn`
(`hew-parser/src/parser.rs:1271-1323`).
Actor receive handlers can be generator functions
(`hew-parser/src/parser.rs:2347-2375`).
The parser parses `yield` as an expression
(`hew-parser/src/parser.rs:5563-5570`).
The AST has `Expr::Yield` (`hew-parser/src/ast.rs:241-243`).
The type checker rejects `yield` outside generator functions
(`hew-types/src/check/expressions.rs:742-765`).
The item checker stores a declared generator yield type while checking generator
functions (`hew-types/src/check/items.rs:421-431`).
Generator blocks need an expression-local yield context, not only function-item
state.
The MIR declares `Terminator::Yield` (`hew-mir/src/model.rs:170-175`).
The MIR declares `MirCheck::GeneratorBorrowAcrossYield`
(`hew-mir/src/model.rs:1097-1101`).
The MIR lowerer already has an `IntentKind::Yield` strategy hook
(`hew-mir/src/lower.rs:3300-3307`).
Codegen currently rejects `Terminator::Yield`
(`hew-codegen-rs/src/llvm.rs:3029-3036`).
The runtime has a thread-based generator context
(`hew-runtime/src/generator.rs:1-18`) and `hew_gen_yield`
(`hew-runtime/src/generator.rs:213-233`).
Preferred v0.5 generator blocks should lower to deterministic state machines,
not thread-per-generator runtime contexts.
`for` loops already project `Iterator::Item`
(`hew-types/src/check/statements.rs:8-56`).
The checker falls back to Iterator for non-builtin iterable types
(`hew-types/src/check/statements.rs:858-869`).
Tests prove user Iterator impls work in `for`
(`hew-types/src/check/tests.rs:15436-15463`).
Tests prove `dyn Iterator<Item = i32>` accepts a concrete iterator
(`hew-types/src/check/tests.rs:15465-15494`).
There is already a TODO for generator block Iterator smoke tests
(`hew-types/src/check/tests.rs:15496-15505`).
HIR has dyn-trait coercion and dyn-method-call nodes
(`hew-hir/src/node.rs:651-684`).
The checker validates associated-type bindings during dyn coercion
(`hew-types/src/check/coerce.rs:100-135`).
The checker re-projects associated types during type expectations
(`hew-types/src/check/coerce.rs:224-250`).
Dyn Iterator tests prove the substituted signature path through `iter.next()`
(`hew-types/tests/dyn_trait_coercion.rs:226-255`).
## 4. Closure surface design
Primary expression form: `|x| x + 1`.
Typed block form: `|x: i32| -> i32 { x + 1 }`.
Zero-argument forms: `|| 42` and `|| -> i32 { 42 }`.
Multi-argument form: `|a: i32, b: i32| a + b`.
The body may be an expression or a block.
If `-> T` is present, check the body against `T`.
If `-> T` is absent and an expected callable type exists, infer parameter and
return types from the expected type.
If no expected type exists, unannotated parameters create inference variables
that must resolve or emit existing inference diagnostics.
No user-facing lifetime syntax is allowed.
No borrow-capture mode is allowed.
No implicit shared mutable capture is allowed.
No Rust-style `Fn` / `FnMut` / `FnOnce` user model is introduced in v0.5.
Capture rule: all captures are by value.
Capture representation: every captured binding becomes a field in an
auto-generated closure environment record.
Capture order: stable source order by first resolved binding use, after
deduplication.
Capture typing: field type is the fully resolved type after literal
materialization and associated-type projection.
Copyable values may be copied into the closure environment.
Affine, linear, heap-owning, or non-copy values require explicit `move`.
`move` means by-value capture with consumption from the enclosing scope.
`move` does not mean borrow and does not change call flavor.
A non-`move` closure that references a non-copy outer binding must compile-fail.
It must not alias the outer binding.
It must not silently deep-copy unless the type already has an explicit copy
contract.
A moved capture marks the source binding moved at the closure literal span.
Later source-binding use reports the existing use-after-move error.
Capturing a mutable binding captures the current value, not a live connection
to the outer variable.
Mutating captured state inside the closure is allowed only when the closure
environment is mutable and the captured value class permits mutation.
Recursive closures are compile-fail in v0.5 unless a fixed-point surface is
ratified later.
Reason: by-value closure capture cannot capture a value before construction
without cycles or actor-style weak-handle semantics.
Nested closures capture from the nearest lexical environment.
If the nearest environment is an outer closure, the inner closure captures from
the outer closure environment field, not from a stale stack slot.
Preferred user-facing callable type: continue using `fn(A) -> B` as a callable
contract for function items and closure values.
Internally keep `Ty::Function` vs `Ty::Closure`.
If raw function pointers need a distinct future surface, reject ambiguous edge
cases now with precise diagnostics.
Closure-to-spawn boundary: a closure captured into spawned work must satisfy a
Send-like contract enforced by checker/MIR value-class rules, not lifetimes.
Captured values crossing actor/task boundaries must be owned, copyable, or
explicitly actor-boundary-safe.
No shared mutable state crosses actor boundaries.
No captured reference crosses actor boundaries.
No closure environment may hide a non-send resource.
Composition with `Pid`: if a closure is embedded in a `Pid::Msg`, its
environment must satisfy the same transitive message-payload contract as the
message type (`std/builtins.hew:64-68`).
Remote pid messages must reject local-only closure environments.
Remote closure serialization is out of scope.
Fail-closed closure rules:
- unresolved capture is a compile error;
- capture with no backend slot is a compile error;
- capture of a moved binding is a compile error;
- non-copy capture without `move` is a compile error;
- actor-unsafe capture into spawned work is a compile error;
- lock-guard capture across actor/task boundary is a compile error.
Capture analysis is a source of truth, not an optimization.
No later phase may silently drop a capture.
## 5. Generator surface design
Primary expression form: `gen { yield 1; yield 2; }`.
The block is an expression that evaluates to a lazy generator value.
The body does not run at construction time except for capture evaluation needed
to build the generator state.
The body runs when `Iterator::next` is called.
Each `yield expr;` emits one `Some(expr)` from `next`.
When the body falls off the end, the next call to `next` returns `None` exactly
once and transitions to End.
Any later call to `next` after End traps.
This is the fail-closed rule required by `feedback_fail_closed_not_pretend`.
Use or add a specific generator-ended trap kind.
Do not silently return `None` forever.
Yield type inference:
- all `yield` values in one generator block unify to one `Item` type;
- expected `dyn Iterator<Item = T>` checks yields against `T`;
- generic Iterator contexts use projected `Item` when available;
- no expected item type means infer from yields;
- empty generator with no expected item type compile-fails;
- `yield;` yields `()`.
Preferred v0.5 rule: `return` inside a generator block is rejected; generator
blocks end by falling off the block.
`break` and `continue` may target loops inside the generator body.
They may not target loops outside the generator block.
Generator blocks capture by value using closure capture rules.
Generator captures live in the generator state struct.
Generator state contains:
- program counter;
- End/Cancelled marker;
- captured fields;
- locals live across yield points;
- drop flags for initialized fields;
- hidden temporaries needed to resume loops.
Each `gen {}` block creates an anonymous generator type.
The compiler generates `impl Iterator for <GenN>`.
The impl binds `type Item = <yield type>`.
The impl defines `fn next(self) -> Option<Self::Item>`.
`next` advances the state machine until the next yield, End, trap, or
cancellation.
Do not introduce user-facing lifetimes.
Implementation may represent generator values as affine handles to owned state
if needed to mutate through the existing `next(self)` signature.
User semantics remain owned state, not borrowed state.
Preferred lowering: deterministic state machine, not runtime thread.
`Terminator::Yield` may be an intermediate marker before state-machine
elaboration, but codegen must not see unsupported Yield unless it is fully
implemented.
Drop mid-iteration runs drops for every initialized captured field and live
local in the current state.
Drop mid-iteration does not run the rest of the generator body.
Drop mid-iteration releases auto-injected locks according to D24-1.
Drop mid-iteration signals cancellation to nested spawned work according to
D24-2.
Drop after End is cleanup-idempotent, but `next` after End still traps.
Cancellation must not be reported as ordinary iterator exhaustion unless D24-2
explicitly ratifies that behavior.
Given current `Iterator::next -> Option<Item>`, preferred v0.5 behavior is a
cancellation trap rather than cancellation-as-None.
Held locks must not cross yield points.
Generator state stores owned values, not live lock guards.
If a lock guard would be live across yield, compile-fail.
This aligns with `MirCheck::GeneratorBorrowAcrossYield`
(`hew-mir/src/model.rs:1097-1101`).
## 6. Acceptance contract
1. `|n| n * 2` passed to a higher-order helper compiles and runs.
2. `let f = |x: i32| -> i32 { x + 1 }; assert_eq(f(41), 42);` compiles and
runs.
3. `let k = 2; let f = |n: i32| n * k; assert_eq(f(21), 42);` proves by-value
scalar capture.
4. Non-copy capture without `move` compile-fails with explicit-move-required.
5. Using a binding after it was moved into `move || ...` compile-fails.
6. `let f = || missing + 1;` compile-fails with unresolved symbol, not an empty
capture list.
7. A capture whose backend slot cannot be materialized compile-fails.
8. A closure captured into `spawn` runs with captured state.
Candidate: `let seed = 41; let task = spawn || { seed + 1 }; assert_eq(await
task, 42);`.
If D24-2 ratifies different spawn-closure syntax, adapt the test while keeping
the semantic requirement fixed.
9. A closure captured into spawned work rejects actor-boundary-unsafe captures.
10. `gen { yield 1; yield 2; yield 3; }` iterates through `for` correctly.
Candidate: `let g = gen { yield 1; yield 2; yield 3; }; var sum = 0; for x in
g { sum = sum + x; } assert_eq(sum, 6);`.
11. Generator blocks auto-implement Iterator.
Candidate: `fn first(iter: dyn Iterator<Item = i32>) -> Option<i32> {
iter.next() }` accepts `first(gen { yield 1; })`.
12. `gen { yield 1; yield "x"; }` compile-fails on mixed yield types.
13. `let g = gen { };` compile-fails without an expected item type.
14. Direct `next` sequence is `Some(1)`, `None`, then trap.
15. Dropping a generator mid-iteration releases captured owned resources.
16. Cancelling a spawned task holding a generator runs generator cleanup and
observes D24-2.
17. Lock guard live across yield compile-fails.
18. Generator used inside an actor does not hold actor-state lock across yield.
19. Native and WASI behavior match, or every missing path is explicitly gated
with `WASM-TODO(#NNN)`.
20. Analysis, formatting, serialization, syntax highlighting, and LSP visitors
handle new AST/HIR shapes without wildcard fallthrough.
## 7. Implementation slices
### Slice 1: closure parser surface and AST plumbing
Goal: parse `|...|` closure expressions and keep AST consumers exhaustive.
Add parser support for `|x| expr`, `|x: T| expr`, `|x: T| -> U { ... }`, `||
expr`, and `move |...| expr`.
Reuse `LambdaParam` and preferably reuse `Expr::Lambda`.
Set `is_move` for `move |...|`.
Keep `type_params: None` for new pipe closures unless existing generic lambda
tests require a conversion path.
Decide old `(params) => body` syntax. Preferred no-legacy decision: remove or
gate it in v0.5 tests. If removal is too large, mark it transitional internal
syntax with an explicit cleanup slice.
Update formatter, serializer/enrichment, analysis visitors, completions,
references, folding, inlay hints, calls, and syntax docs as needed.
Tests: parser positives, parser malformed-pipe negatives, formatter round trip,
and AST visitor coverage.
Exit: requested closure surface parses and no visitor silently drops a body.
### Slice 2: closure checker and capture analysis
Goal: make by-value capture semantics authoritative in the checker.
Replace capture-type-only tracking with binding-accurate capture facts.
For each capture, record binding id or env symbol, source name, resolved type,
value class, capture mode, source span, and actor/task-boundary use.
Capture mode is by-value only.
Copy captures may be implicit.
Non-copy captures require `move`.
Moved captures consume the source binding.
Reject moved, uninitialized, unresolved, recursive, borrow-like, lock-guard, or
actor-unsafe captures.
Check closure calls through existing callable type logic.
Keep `fn(A) -> B` as v0.5 callable contract unless implementation proves a
separate surface is required.
Add diagnostics: explicit move required, capture after move, recursive closure
unsupported, capture not actor-boundary-safe, cannot infer closure parameter,
and cannot materialize closure capture.
Tests: scalar copy capture, `move` capture, non-copy without move, use after
move, unresolved capture, nested lexical capture, actor-boundary rejection.
Exit: capture facts are binding-accurate and cannot be lost before HIR.
### Slice 3: closure HIR/MIR lowering and codegen
Goal: materialize closures as real callable values with environment records.
Add a HIR closure node or extend `Expr::Lambda` lowering.
HIR must carry params, return type, body, capture ledger, call signature, and
source span.
HIR verifier must ensure captures reference declared bindings, are deduplicated,
and have materialization metadata.
Add MIR closure environment representation distinct from `LambdaActorHandle`.
Preferred lowering: anonymous record for captures plus an invoke shim per
closure literal.
Lower construction to environment field initialization.
Lower call to invoke shim with environment plus user args.
Drop environment fields using existing record/drop order.
When closure moves into spawn, validate transitive actor-boundary safety and
D24-2 cancellation inheritance.
Codegen emits environment struct, invoke shim, closure call, and env drop.
Tests: closure call, captured closure call, env drop, spawned closure after
D24-2, and WASI parity where available.
Exit: first-class closure values execute and bad captures fail closed.
### Slice 4: generator parser surface and expression-local yield checking
Goal: parse `gen { ... }` block expressions and type-check yield sites.
Add expression-position `gen` followed by a block.
Preferred AST: `Expr::GenBlock { body: Block }`.
Do not reuse `gen fn` item parsing.
Make `yield` legal inside `gen {}` even in regular functions.
Keep `yield` illegal outside generator functions or generator blocks.
Add an expression-local generator context stack recording expected item type,
inferred item type, and whether any yield occurred.
Unify every yield with the context item type.
Reject empty generator without expected item type.
Reject `return` crossing out of a generator block.
Prevent an inner closure from accidentally yielding from an outer generator
unless the inner closure is itself in a generator context.
Update formatter, serializer, analysis visitors, syntax docs, and LSP.
Tests: parser positives/negatives, inferred item type, expected `dyn
Iterator<Item = T>`, mixed yield rejection, empty generator rejection.
Exit: generator blocks type as anonymous Iterator-producing values.
### Slice 5: generator state-machine lowering and Iterator impl wiring
Goal: lower each generator block to a state-machine type that implements
Iterator.
Synthesize an anonymous generator record with state discriminant, end/cancel
marker, captures, live-across-yield locals, drop flags, and resume temporaries.
Generate `impl Iterator for GenN { type Item = YieldTy; fn next(self) ->
Option<Self::Item> { ... } }`.
States: Start, resume points after each yield, EndPendingNone, Ended,
Cancelled, Trap.
Start runs until first yield or end.
Yield stores next resume state and returns `Some(value)`.
Body end returns `None` once and transitions to Ended.
Next from Ended traps.
Cancel runs cleanup and follows D24-2 behavior.
Use `Terminator::Yield` only as an intermediate if it helps borrow/drop checks.
Ensure codegen never sees unsupported Yield unless codegen supports it.
Wire `for` through existing Iterator trait path.
Confirm dyn Iterator coercion and associated-type substitution through `next`.
Tests: `for` over generator, direct `next` including trap, captured generator,
nested loop locals across yield, drop mid-iteration, dyn Iterator call.
Exit: generator blocks execute as Iterator values without runtime-thread
dependency.
### Slice 6: integration hardening, docs, and parity gates
Goal: make the lane v0.5 substrate, not a narrow demo.
Add closure + generator integration: `let make = |start| gen { yield start;
yield start + 1; };`.
Add generator passed as `dyn Iterator<Item = i32>`.
Add closure captured into spawn after D24-2 syntax is ratified.
Add actor-state generator tests after D24-1 lock semantics are ratified.
Add negative tests for unsafe actor-boundary captures, lock guard live across
yield, cancellation-as-None, and missing capture slots.
Update grammar docs for `ClosureExpr` and `GenBlockExpr`.
Update builtins docs if callable type wording changes.
Run existing parser, type checker, HIR, MIR, codegen/runtime, and WASI parity
tests that already exist.
Exit: all surfaces agree and no cross-platform gap is untracked.
## 8. Composition with D24-1 locks and D24-2 cancellation
This lane is D24-position-3 and should dispatch after D24-1 auto-injected locks
and D24-2 cancellation tokens settle.
The exact predecessor plan files were not present in this worktree or session
history at drafting time.
Before implementation, dispatch owner must link the final D24-1 and D24-2 plan
docs here.
Until then, this plan references predecessor lanes by name: D24-1
auto-injected locks and D24-2 cancellation tokens.
D24-1 lock composition:
- closures capture values, not live borrowed references;
- lock-protected actor state may be copied/moved only under D24-1 rules;
- closure environments must not store lock guards;
- generator state must not store lock guards across suspension;
- generated generator `next` may acquire/release locks per resume if D24-1
requires it;
- do not hold an actor lock while returning `Some(value)`.
If a value derived from actor state needs a lock guard to remain valid across
yield, compile-fail.
D24-2 cancellation composition:
- closures captured into spawned tasks inherit cancellation tokens from the
spawning scope;
- closure environments do not create new cancellation roots;
- if spawn fails, captured resources drop in the parent context;
- if task cancellation happens before invocation, captured resources drop in
task cleanup;
- if cancellation happens during invocation, normal cleanup runs.
Generator cancellation:
- a generator created inside a cancellable task inherits that task's
cancellation context for cleanup and nested work;
- moving a generator into another task follows D24-2 token inheritance;
- suspended generator cancellation runs cleanup before transition to Cancelled;
- cancellation must not masquerade as ordinary `None`.
Given current `Iterator::next -> Option<Item>`, trap on cancellation is safer
than cancellation-as-None unless D24-2 ratifies a structured channel.
No capture may bypass supervision cleanup.
No capture may outlive its supervising scope without an explicit detached-task
rule from D24-2.
## 9. Composition with Iterator-via-trait
Iterator is the canonical iteration contract (`std/builtins.hew:123-133`).
Generator blocks must auto-implement Iterator.
Do not keep a second compiler-magic generator iteration path.
`for x in gen { ... }` must:
1. synthesize the generator expression type;
2. prove that type satisfies `Iterator`;
3. project `Item`;
4. lower loop advancement through `Iterator::next`.
Any direct generator fast path is a later optimization and must not alter
semantics.
Generator blocks should type as anonymous named types satisfying
`Iterator<Item = YieldTy>`.
They should not publicly depend on legacy `Generator<T>` unless that type
remains as a transitional internal bridge.
If `Generator<T>` remains, it must implement Iterator.
Dyn Iterator requirement:
`gen { yield 1; }` must coerce to `dyn Iterator<Item = i32>`.
The HIR dyn-trait nodes already carry coercion method tables and dyn method-call
slots (`hew-hir/src/node.rs:651-684`).
The checker already validates associated-type bindings while coercing concrete
types to dyn traits (`hew-types/src/check/coerce.rs:100-135`).
The checker already re-projects associated types during expectations
(`hew-types/src/check/coerce.rs:224-250`).
Existing tests prove `Counter` can coerce to `dyn Iterator<Item = i32>` and
`iter.next()` substitutes correctly
(`hew-types/tests/dyn_trait_coercion.rs:226-255`).
Generated generator impls must feed that same substituted-signature path.
Confirm:
- `fn use_iter(iter: dyn Iterator<Item = i32>) -> Option<i32> { iter.next() }`
accepts a generator block;
- `Self::Item` substitutes to `i32`;
- return type reaches codegen as `Option<i32>`;
- vtable method entry points at the generated `next` shim;
- anonymous generator type has a stable internal descriptor for vtable
deduplication.
## 10. Validation candidates, LESSONS triggers, Risks, Out of scope, Handoff
### Validation candidates
Parser: `|| 1`, `|x| x + 1`, `|x: i32| -> i32 { x + 1 }`, `move |x| x`,
`gen { yield 1; yield 2; }`, and nested `gen { let f = |x| x + 1; yield
f(1); }`.
Checker: expected-type closure inference, annotated closure inference, scalar
capture, `move` capture, non-copy capture without move, use after move,
spawn-boundary rejection, yield outside generator rejection, yield inside
`gen {}` acceptance, mixed yield rejection, empty generator rejection.
HIR: capture binding ids, nested lexical capture, generator captures as owned
fields.
MIR: no missing capture slot skip, closure env drop, generator drop flags, lock
guard across yield rejection.
Codegen/runtime: closure call, captured closure call, generator `Some`, `None`,
then trap, `for` over generator, drop mid-iteration, cancellation cleanup,
spawned closure cancellation inheritance.
Parity: native and WASI agree on closure calls, generator next/end/trap, and
cleanup; every gap has an explicit gate.
Analysis/serialization: LSP references find closure params/captures, inlay hints
avoid double-walking bodies, formatter preserves syntax, msgpack/enrich round
trips new AST shapes.
### LESSONS triggers
Trigger `feedback_fail_closed_not_pretend` if any missing capture, cancelled
generator, or ended generator becomes apparent success.
Trigger `checker-authority` if MIR/codegen re-infers capture legality instead
of consuming checker/HIR facts.
Trigger `exhaustive-traversal-and-lowering` if visitors add wildcard fallthrough
for closure or generator nodes.
Trigger `sub-body-scoped-traversal` if analysis walks closure/generator bodies
from the wrong lexical owner.
Trigger `no-untracked-wasm-deferrals` if a closure/generator path is native-only
without a gate.
Trigger `substrate-over-surface` if a slice bypasses Iterator, Pid, locks, or
cancellation for a demo.
Trigger `no-legacy` if old lambda syntax remains user-facing without a v0.5
decision.
Trigger `actor-boundary-safety` if a closure environment can smuggle shared
mutable state into spawn or pid messages.
Trigger `drop-path-completeness` if End, Cancelled, Panic, and Drop do not share
a verified cleanup plan.
### Risks
Existing `(params) => body` lambda tests may be numerous; mitigate by converting
tests or gating transitional syntax explicitly.
`Iterator::next(self)` may not map cleanly to mutable generator state; mitigate
with affine owned-state handles internally.
Closure values and raw function pointers may need separate future surfaces;
mitigate by keeping `fn(A) -> B` as callable contract and rejecting ambiguous raw
pointer cases.
Actor-lambda MIR already silently skips some captures; mitigate by building a
new closure path with structural errors on missing slots.
Generator codegen may be attempted too early; mitigate by keeping
`Terminator::Yield` unsupported until state-machine lowering/codegen is
complete.
Drop/cancel paths multiply states; mitigate with explicit drop flags and one
cleanup builder shared by End, Cancelled, Panic, and Drop.
Locks across yield can deadlock; mitigate by compile-failing live lock guards
across yield unless D24-1 ratifies a safe transformed form.
Cancellation through `Option<Item>` is underspecified; mitigate by trapping
rather than returning `None` unless D24-2 defines another channel.
Dyn Iterator vtables may not accept anonymous generated types; mitigate with
stable internal names/descriptors.
WASI may lack a runtime helper; mitigate by using ordinary state-machine control
flow and existing trap ABI where possible.
### Out of scope
HKTs.
Specialization.
User-facing lifetimes.
Borrow-capturing closures.
Rust-style `Fn` / `FnMut` / `FnOnce` surface.
Remote serialization of closure environments.
Async generator surface beyond existing `async gen fn` compatibility.
Full lazy pipeline library.
Full callback standard library expansion.
Browser runtime VM.
Untracked WASM deferrals.
Generator-to-stream adapters unless required by existing tests.
Recursive closure fixed-point syntax.
Closure equality, hashing, and display.
Capturing raw pointers or unsafe references across tasks except rejection tests.
### Handoff
Dispatch order: wait for D24-1 auto-injected locks and D24-2 cancellation tokens
to land.
Before implementation, link exact D24-1 and D24-2 plan docs in section 8.
Before implementation, confirm whether old `(params) => body` syntax is removed
or temporarily accepted.
Before implementation, confirm final spawn-closure syntax from D24-2.
Start with Slice 1.
Do not start generator lowering before closure capture facts are binding-based.
Do not start spawned closure acceptance before D24-2 cancellation semantics are
available.
Do not start actor generator lock tests before D24-1 lock semantics are
available.
Suggested PR sequence:
1. closure parser/checker;
2. closure HIR/MIR/codegen;
3. generator parser/checker;
4. generator lowering and Iterator wiring;
5. integration, docs, and parity.
Every PR must include negative fail-closed tests.
Every PR must avoid silent fallback behavior.
Every PR must update exhaustive visitors.
Every PR must preserve cross-platform parity or add explicit gates.
Done means closures are first-class callable values with by-value environments.
Done means generator blocks are Iterator values.
Done means `for` over a generator uses the Iterator trait path.
Done means spawned closures inherit cancellation and reject unsafe captures.
Done means generator drop/cancel/end semantics are defined and tested.
Done means calling `next` after End traps.
Done means missing captures compile-fail.
Done means no user-facing lifetimes were introduced.
