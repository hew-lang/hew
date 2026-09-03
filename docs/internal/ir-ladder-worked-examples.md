# The ladder, worked: source → SIR → MIR → LLVM for the shapes that keep failing

Companion to `ir-ladder.md` (`DOC-LADDER`). That document states the rules;
this one shows the full flow for the shapes that produced double frees,
leaks, and silent wrong answers on the legacy lowerer during the v0.6.0
freeze (issues #3070, #3127, #3226, #3250, #3274). Every implementer brief
for `P1-L2` (verifier), `P1-L3` (HIR→SIR lowering), `P1-L4` (one-form MIR)
and `P1-L5` (emitter and glue) cites the example it must reproduce, and
every refuter checks the produced IR against the text here, op for op. When
this file and `ir-ladder.md` disagree, `ir-ladder.md` wins and this file is
the defect.

## Conventions

SIR text follows `hew-sir/src/dump.rs` where the printer already has a
spelling: `%n` values, `bbN(args)` blocks, one op per line, `const`,
`call @symbol(...)`, and the terminators `return`, `goto bbN`,
`branch %c, bbA, bbB`. Every value carries its ownership kind after the
colon on first definition: `%3: Owned Vec<i64>`, `%4: Guaranteed Pair`,
`%7: None i64`.

Three spellings in this file are **not in `dump.rs` today** and are the
vocabulary `P1-L2`/`P1-L3` add to `hew-sir/src/model.rs` and the printer,
with `ir-ladder.md` §1.3 and §1.3.2 as the semantic authority:

- `switch_enum %g [Variant: bbN, ...]` — the tag check of a match on a
  `Guaranteed` enum (§1.3.2: "the tag check ... inside one borrow of the
  scrutinee"). A terminator.
- `project %g, .field` / `project %g, Variant.i` — a `Guaranteed` projection
  of a `Guaranteed` aggregate (§1.3.2: "a nested predicate reads its payload
  slot as a `Guaranteed` projection"). It costs nothing and emits nothing.
- `destructure %v` on an enum whose tag is already proven produces the
  proven variant's parts (§1.3.2: "each nested `bindings` set is a nested
  `destructure` of the corresponding payload part"). It is the §1.3
  `destructure` row, not a new op.

Every ownership op is from the §1.3 set. MIR is written as the §4.3
instruction names with the runtime symbol in a trailing comment. The class
table row for every type in an example is stated first, because the class
decides every op that follows (§1.1).

The recurring lesson, stated once: **the legacy lowerer decides ownership
from the shape of the consumer** (a projection, a match arm, a call
spelling), so every new consumer shape is a new special case and a new
place to be wrong. The ladder decides ownership **from the value's class
and its last use**, once, in SIR, and every consumer shape below reduces to
the §1.3 op set with no shape-specific rule of its own.

Fixtures named below live under `repros/ladder/worked/` and are `P1-L3`
deliverables; none exists on `main` today except where marked. Each is
run by `make asan-fixtures` with its printed output and the zero-leak line
as the oracle, and its `hew compile --dump-sir` output is diffed against
the block in this file.

## W1. Borrowed match on a payload inside a live record (#3226)

```hew
type Pair { other: Vec<i64>, value: Option<Vec<i64>> }

fn total(p: Pair) -> i64 {
    var n = 0;
    match p.value {
        .Some(v) => { n = v.len(); }
        .None => {}
    }
    return n + p.other.len();
}
```

Classes: `Vec<i64>` is `CowValue` (clone `DeepCopy`); `Option<Vec<i64>>` is
`CowValue` by the aggregate rule; `Pair` is `CowValue`; `i64` is `BitCopy`.
`p` has header mode `Borrow` (no `consume`), so `%0` is `Guaranteed` for the
whole body and is never consumed (rule 3).

`p.value` is not a last use of `p` (`p.other` follows), so the match reads
the payload inside the parameter's borrow and never destructures. `n` is a
non-escaping `var`, so it is mem2reg'd into a block argument (§1.3, §1.4).

```
fn total(%0: Guaranteed Pair) -> i64 {
bb0:
    %1: None i64 = const 0
    %2: Guaranteed Option<Vec<i64>> = project %0, .value
    switch_enum %2 [Some: bb1, None: bb2]
bb1:
    %3: Guaranteed Vec<i64> = project %2, Some.0
    %4: None i64 = call @hew_vec_len(%3)
    goto bb3(%4)
bb2:
    goto bb3(%1)
bb3(%5: None i64):
    %6: Guaranteed Vec<i64> = project %0, .other
    %7: None i64 = call @hew_vec_len(%6)
    %8: None i64 = add %5, %7
    return %8
}
```

Verifier trace. Rule 1: no `Owned` value is defined in the body, so there is
nothing to balance. Rule 3: every `Guaranteed` use (`%2`, `%3`, `%6`) is
inside the parameter's whole-body borrow, and `%0` has no consumer. Rule 4:
no places. The proof is trivial, and that is the point: a read-only match
on a borrowed record has **no ownership ops at all**.

MIR: two `Load`s of field projections and two calls; no `Retain`,
`Release`, `Move`, or `Fork`. The caller owns `p` and destroys it once at
its own block exit with `Release { p }` → `hew_drop$Pair` (§5.2 item 1:
`value` then `other`, reverse declaration order; the enum arm releases the
payload under the `Some` tag, §5.2 item 2).

LLVM: two GEPs, two `hew_vec_len` calls, one `phi` for `n`. The callee
emits no cleanup.

What the legacy lowerer did (#3226): it minted an owner for the `Some`
binder at the destructure site, so the payload was released once by that
owner and again by the parent's drop, `free(): invalid pointer` at
teardown; the repair neutralized the parent's field, and the neutralize
retired the whole root generation, which is why every sibling shape (a
loop, a guarded arm, an `if let`) needed its own patch. Under the ladder
there is no binder owner to neutralize: a borrowed payload is a
`Guaranteed` projection, not a value.

Fixture (P1-L3): `repros/ladder/worked/w1_borrowed_match.hew`, expected to
print `5` and exit 0 under ASan with zero leaks.

## W2. Consuming match on a record that is a last use

```hew
fn first_len(p: Pair) -> i64 {
    match p.value {
        .Some(v) => v.len(),
        .None => 0,
    }
}
```

Same classes. `p.value` is the last use of `p`, but the header slot is
`Borrow`, so this is still not a consuming match: the arm reads through the
borrow exactly as W1, and the SIR is W1 without `bb3`'s second projection.
A borrowed parameter is never destructured (rule 3: a `Guaranteed` value is
never an operand of a consuming position).

The consuming variant needs the header to say so:

```hew
fn take_first(consume p: Pair) -> Vec<i64> {
    match p.value {
        .Some(v) => v,
        .None => Vec.new(),
    }
}
```

Now `%0` is `Owned`, `p.value` is its last use, and the match consumes it.
Every check precedes every consume (§1.3.2): the tag switch reads through a
borrow, and only the arm that passed destructures.

```
fn take_first(%0: Owned Pair) -> Vec<i64> {
bb0:
    %1: Guaranteed Pair = begin_borrow %0
    %2: Guaranteed Option<Vec<i64>> = project %1, .value
    switch_enum %2 [Some: bb1, None: bb2]
bb1:
    end_borrow %1
    %3: Owned Vec<i64>, %4: Owned Option<Vec<i64>> = destructure %0
    destroy_value %3
    %5: Owned Vec<i64> = destructure %4
    goto bb3(%5)
bb2:
    end_borrow %1
    %6: Owned Vec<i64>, %7: Owned Option<Vec<i64>> = destructure %0
    destroy_value %6
    destroy_value %7
    %8: Owned Vec<i64> = call @hew_vec_new_with_elem_layout(<i64 descriptor>)
    goto bb3(%8)
bb3(%9: Owned Vec<i64>):
    return %9
}
```

`%3` is `other`, unused in the arm, destroyed. `%4` is `value`; in `bb1`
its tag is proven `Some`, so `destructure %4` yields the one payload part.
In `bb2` the tag is `None`: `destroy_value %7` releases nothing, because
the enum's glue switches on the discriminant (§5.2 item 2).

Verifier trace. Rule 1 for `%0`: consumed exactly once on each path (the
`destructure` in `bb1`, the `destructure` in `bb2`). For `%3` … `%9`: one
consumer each on their path (`destroy_value`, `destructure`, an edge
argument, `return`). Rule 3: `%1`'s region ends before the consumer of `%0`
in each arm. Rule 5: the `return` operand is `Move`.

MIR: `Move` per field out of the aggregate (§4.3 `destructure` row);
`Release { other }` → `hew_vec_free`; in `bb2` `Release { value }` →
`hew_drop$Option<Vec<i64>>`; no `Retain` anywhere. The caller passed `p`
into a `Consume` slot and emits nothing after the call.

What the legacy lowerer did (#3226): it treated `p.value` as a projection
of a still-live owner and moved the payload out while the owner's release
plan still covered it, or retired the owner and leaked `other`; the three
projection shapes the issue names each took a different one of those two
paths. Under the ladder the whole record is destructured in the arm, every
part is named, and every part is consumed exactly once.

Fixture (P1-L3): `repros/ladder/worked/w2_consuming_match.hew`, expected to
print `3` for `take_first(consume Pair{other: [1], value: .Some([1,2,3])})
.len()`; ASan zero leaks.

## W3. Closing one resource field of a composite (#3070)

```hew
#[resource] type Conn { fd: i64 }
impl Conn { fn close(self) { println(f"close {self.fd}") } }
type Two { a: Conn, b: Conn }

fn shut_param(t: Two) { t.a.close(); }          // (1) Borrow parameter

fn shut_local() {                               // (2) owned local
    let t = Two { a: Conn { fd: 1 }, b: Conn { fd: 2 } };
    t.a.close();
    t.b.close();
}
```

Classes: `Conn` is `AffineResource` (clone `None`, `UserClose` glue);
`Two` is `AffineResource` by the aggregate rule.

**Both spellings are refused.** In (1) `t.a` is a `Guaranteed` projection
inside the borrow of `%t`, `close` has a `Consume` receiver slot (§4.2), and
a `Guaranteed` value is never an operand of a consuming position (rule 3):
`E_OWN_CONSUME_BORROWED` "`t.a` is borrowed through parameter `t`; declare
it `consume t: Two`". In (2) `%t` is `Owned`, but `t.a` is still a
`Guaranteed` projection of a live aggregate, and §1.3 emits `destructure`
only for a whole-value last use, never for a field consume. This is a
**decision recorded here** (ledger D345): Hew has no partial move. A field
of a live local is consumed only after the local is destructured, and the
diagnostic is `E_OWN_PARTIAL_CONSUME` "field `a` of `t` is consumed while
`t` is live; destructure first: `let Two { a, b } = t;`". The alternative,
per-field consumed state on a local (Rust's partial moves), is a second
definite-initialization lattice over every aggregate local and the exact
mechanism (per-field release plans) whose repairs produced the double
frees this file exists to end. Issue #3070's acceptance shape, a returned
record whose fields the caller closes, is therefore satisfied by the
destructure spelling and the issue is amended to say so.

The spelling that compiles, and its IR:

```hew
fn shut_both(consume t: Two) {
    let Two { a, b } = t;
    a.close();
    b.close();
}
```

```
fn shut_both(%0: Owned Two) -> () {
bb0:
    %1: Owned Conn, %2: Owned Conn = destructure %0
    call @Conn.close(%1)
    call @Conn.close(%2)
    return
}
```

Rule 1 for `%0`, `%1`, `%2`: one consumer each (`Conn.close` has a
`Consume` receiver slot). Rule 6d does not apply (`AffineResource`, not
`Linear`). If the body were `a.close();` alone, HIR→SIR emits
`destroy_value %2` at scope exit (§1.3.4: every live `Owned` binding), which
lowers to `Release { b }` → `Conn.close` through `UserClose` glue. One close
per field, always, because each field is one `Owned` value with one
consumer. The one place a field is consumed inside a live aggregate is an
actor state field with a taken bit (§1.3.6, W7), where the runtime owns the
aggregate and the bit records the take.

What the legacy lowerer did: it emitted the user close for `t.a` and then
walked `t`'s fields at scope exit, closing `a` a second time. PR 3246's
repair retired the whole record at the first close and thereby leaked `b`,
which its own description records as the trade. Under the ladder the
aggregate has no "walk"; it is either one `Owned` value released by glue,
or it has been destructured into named parts.

Fixtures (P1-L3): `repros/ladder/worked/w3_partial_close_param.hew` and
`w3_partial_close_local.hew` are `reject` fixtures with the two diagnostics
above; `w3_destructure_close.hew` prints `close 1`, `close 2` under ASan
with no double free.

## W4. Match over a fresh call result (#3127)

```hew
fn drain(rx: Receiver<string>) -> string {
    match rx.recv() {
        .Some(s) => s,
        .None => "",
    }
}
```

Classes: `Option<string>` is `CowValue` (string is `CowValue`);
`Receiver<string>` is `AffineResource`; `rx` is `Borrow`. `recv` on a
`Receiver` returns `Option<T>` (`std/channel/channel.hew`, `ChannelRecv`
row of §1.5) and its result is `Fresh` (§4.2), so it is an unnamed `Owned`
temporary of the enclosing block (§1.3.4). This is the free-function form,
which blocks the calling thread; the same `recv` awaited inside a handler
is a `Suspend { kind: ChannelRecv }` (§1.5) whose resume edge carries the
same `Owned Option<string>` and whose cancel edge deregisters the waiter,
and everything below the call is identical.

```
fn drain(%0: Guaranteed Receiver<string>) -> string {
bb0:
    %1: Owned Option<string> = call @hew_channel_recv_layout(%0)
    %2: Guaranteed Option<string> = begin_borrow %1
    switch_enum %2 [Some: bb1, None: bb2]
bb1:
    end_borrow %2
    %3: Owned string = destructure %1
    goto bb3(%3)
bb2:
    end_borrow %2
    destroy_value %1
    %4: Owned string = const ""
    goto bb3(%4)
bb3(%5: Owned string):
    return %5
}
```

Verifier trace. Rule 1 for `%1`: one consumer on each path (`destructure`
in `bb1`, `destroy_value` in `bb2`). Because each arm consumed the
temporary, the block-exit `destroy_value %1` of §1.3.4 is not emitted; had
an arm only read the payload (`.Some(s) => s.len()`), the arm would
`copy_value` the projection inside the borrow, or read its length through
it, and the temporary's `destroy_value` would sit at the block exit, once.

MIR: `Move` of the payload out of the temporary (`Move { src: tmp.Some.0,
dst }`), no `Release` of the temporary on that path (§4.3 `destructure`
row: "`Release` of the shell if boxed", and an `Option` is not boxed);
`Release { tmp }` → `hew_drop$Option<string>` in `bb2`. The recv symbol is
the `runtime_symbols.rs` row whose result is `Fresh`; the receiver handle
was `Borrow` and is untouched.

What the legacy lowerer did (#3127): it registered the `recv` result as a
scrutinee owner and released it at statement end, after the arm had moved
the payload out, so the handler observed an empty string. Under the ladder
the temporary is an `Owned` value like any other and the arm's
`destructure` is its one consumer.

Fixture (P1-L3): `repros/ladder/worked/w4_recv_match.hew`, expected to
print the received string and exit 0 under ASan.

## W5. Block tail versus `return` of a projected field (#3274)

```hew
type Pair2 { other: Vec<i64>, value: i64 }
fn take_a(consume p: Pair2) -> Vec<i64> { p.other }
fn take_b(consume p: Pair2) -> Vec<i64> { return p.other; }
```

Both functions produce **the same SIR**. A block tail in return position
and an explicit `return` both lower to `return` of an `Owned` operand (rule
5: the `Return` operand mode is `Move`); the intent stamp the legacy lowerer
reads (`IntentKind::Read` on a tail, `IntentKind::Consume` on a `return`,
`hew-hir/src/lower.rs`) does not exist in SIR.

```
fn take_a(%0: Owned Pair2) -> Vec<i64> {
bb0:
    %1: Owned Vec<i64>, %2: None i64 = destructure %0
    return %1
}
```

Rule 1: `%0` consumed once, `%1` once (`return`); `%2` is `BitCopy` and
carries no obligation. MIR: `Move` per field, `Move` into the return slot.
The `free(): invalid pointer` abort on `main` for `take_a` is a projection
moved out of a record whose release plan still covered it; the ladder has
no release plan, only consumers.

Fixture (P1-L3): `repros/ladder/worked/w5_tail_vs_return.hew` runs both and
prints `2`, `2`; ASan zero leaks and no abort.

## W6. An owned value across a loop back edge with `break` and `return` (#3250)

```hew
fn find(items: Vec<string>, needle: string) -> string {
    var found = "";
    for s in items {
        if s == needle { found = s; break; }
        if s == "stop" { return "stopped"; }
    }
    return found;
}
```

Classes: `string` and `Vec<string>` are `CowValue`; both parameters are
`Borrow`. `found` is a non-escaping `var`: a block argument at the loop
header. `s` is one element per iteration.

Iterating a **borrowed** collection must not copy it. The `for x in v`
desugar over a `Guaranteed` `Vec` is therefore an index loop: the length is
read once through the borrow, the index is a `BitCopy` block argument, and
each element is read out with `copy_value` of the projection (§1.3
`copy_value` row: "`Index` read-out of a live composite (inside a
borrow)"), realized by the collection's `get_clone` entry (§5.3). The
`VecIter` record of the current desugar (`hew-hir/src/lower.rs`, a
`{ vec, idx }` literal) is what a `for` over an **owned** vector lowers to,
because it moves the vector in; over a borrowed one it would need
`copy_value` of the whole vector, a deep copy per loop, and that is not
admitted. P1-L3 owns this split.

```
fn find(%0: Guaranteed Vec<string>, %1: Guaranteed string) -> string {
bb0:
    %2: Owned string = const ""
    %3: None i64 = call @hew_vec_len(%0)
    %4: None i64 = const 0
    goto bb1(%2, %4)
bb1(%5: Owned string, %6: None i64):
    %7: None i32 = lt %6, %3
    branch %7, bb2, bb7
bb2:
    %8: Guaranteed string = project %0, [%6]
    %9: Owned string = copy_value %8
    %10: Guaranteed string = begin_borrow %9
    %11: None i32 = call @hew_string_equals(%10, %1)
    end_borrow %10
    branch %11, bb3, bb4
bb3:
    destroy_value %5
    goto bb8(%9)
bb4:
    %12: Guaranteed string = begin_borrow %9
    %13: Owned string = const "stop"
    %14: Guaranteed string = begin_borrow %13
    %15: None i32 = call @hew_string_equals(%12, %14)
    end_borrow %14
    end_borrow %12
    destroy_value %13
    branch %15, bb5, bb6
bb5:
    destroy_value %9
    destroy_value %5
    %16: Owned string = const "stopped"
    return %16
bb6:
    destroy_value %9
    %17: None i64 = add %6, 1
    goto bb1(%5, %17)
bb7:
    goto bb8(%5)
bb8(%18: Owned string):
    return %18
}
```

Verifier trace. Rule 1 per path for `%5` (`found`): consumed on the `break`
path (`destroy_value` in `bb3`), on the `return` path (`destroy_value` in
`bb5`), and on the exhausted path (edge argument in `bb7`); re-passed as an
edge argument around the back edge in `bb6`. For `%9` (`s`): consumed in
`bb3` (edge argument), `bb5` (`destroy_value`) or `bb6` (`destroy_value`).
For `%13`: one `destroy_value`. Rule 3: every borrow of `%9` ends before its
consumer. §1.4: every `Owned` value live at the header is a header argument
and nothing else survives the join; `%0` and `%1` are `Guaranteed` and
belong to the caller.

`main` (#3250) releases the loop binder on the `break` edge and withholds
it on the `return` edge. §1.3.4 lists `Return`, `Break` and `Continue`
together as exits of the enclosing scope, so `bb3` and `bb5` are produced
by one rule; the `for await` shape of #3250 is this loop with a `Suspend`
at the top of `bb1` and the frame binder as the loop argument, and nothing
about the exits changes.

MIR: `Retain { elem }` → `hew_string_clone` for the `copy_value` in `bb2`;
`Release` → `hew_string_drop` at each `destroy_value`; `Move` for the edge
arguments; `hew_string_equals` returns `i32` and the `branch` narrows it.

Fixture (P1-L3): `repros/ladder/worked/w6_loop_exits.hew`, expected to
print `b` for `find(["a", "b", "c"], "b")`, `stopped` for `find(["a",
"stop"], "z")` and an empty line for a miss; ASan zero leaks on all three.

## W7. Actor state: push, close, re-initialize

The full rule set is `ir-ladder.md` §1.3.6; this is the op sequence in one
place, because it is the only shape in which a field is consumed inside a
live aggregate, and it is the shape W3's refusal points users to.

```hew
actor Holder {
    var items: Vec<i64> = Vec.new();
    var conn: Conn = Conn.open(1);
    receive fn push(n: i64) { items.push(n); }
    receive fn cycle() -> i64 { conn.close(); conn = Conn.open(2); return conn.fd; }
}
```

```
; push: a mutating call on a CowValue place
    %0: Owned Vec<i64> = load.take %items
    %1: Owned Vec<i64> = fork %0
    call @hew_vec_push_owned_move(%1, %n)
    store.init %items, %1

; cycle: consume, then re-initialize, an AffineResource place
    %2: Owned Conn = load.take %conn
    call @Conn.close(%2)
    %3: Owned Conn = call @Conn.open(2)
    store.assign %conn, %3
    %4: Guaranteed Conn = begin_borrow %conn
    %5: None i64 = project %4, .fd
    end_borrow %4
    return %5
```

In `push` the `fork` is `ensure_unique`, a no-op under §5.5, and the
`store.init` is emitted on every exit including unwind and cancel, so the
place is `Init` again whatever the call does. In `cycle` the `load.take`
sets the taken bit (MIR `MarkUninit`), and the `store.assign` is the
bit-guarded sequence `if !taken { taken := 1; Release }; Store; taken := 0`.
Rule 4 for `%conn`: `Init` at entry, `Uninit` after the take, `Init` after
the store; a read between the two would be `E_OWN_USE_AFTER_CONSUME`. The
teardown glue `hew_drop$State` tests the bit before closing `conn`, so a
trap inside `Conn.close` (bit already set) closes exactly once; `main`
prints `close 1` twice for this program today (§1.3.6).

Fixture: `repros/ladder/state_reinit.hew` (exists) moves from its recorded
double close to `close 1`, `2`, `after`, `close 2`.

## How to use this file

- An implementer lane reproduces the SIR text of its cited example with
  `hew compile --dump-sir` on the fixture and diffs against the block here;
  a mismatch in any ownership op is a defect in the lane, not in the
  example, unless `ir-ladder.md` says otherwise.
- A validator runs each fixture under `make asan-fixtures` and checks the
  printed output and the zero-leak line.
- A refuter that finds a shape this file does not cover writes the shape as
  a W8 candidate in its report with the SIR it expects; the architect adds
  it here before any implementer touches it. The loop on a blocking
  refutation is redesign here first, never a retry in the lowerer (D343).
- The legacy lowerer's `hew-mir/src/lower/**` is not a reference for any
  op in this file; every "what the legacy lowerer did" paragraph is there
  so a reader knows which instinct to unlearn.
