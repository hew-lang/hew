# The ladder, worked: source → SIR → MIR → LLVM for the shapes that keep failing

Companion to `ir-ladder.md` (`DOC-LADDER`). That document states the rules;
this one shows the full flow for the seven shapes that produced double frees,
leaks, and silent wrong answers on the legacy lowerer during the v0.6.0
freeze (issues #3070, #3090, #3127, #3226, #2523, #3250, #3274). Every
implementer brief for `P1-L2` (verifier), `P1-L3` (HIR→SIR lowering),
`P1-L4` (one-form MIR) and `P1-L5` (emitter and glue) cites the example it
must reproduce, and every refuter checks the produced IR against the text
here, op for op. When this file and `ir-ladder.md` disagree, `ir-ladder.md`
wins and this file is the defect.

Conventions. SIR is written in the `hew-sir/src/dump.rs` style: `%n` values,
`bbN(args)` blocks, one op per line, the terminator last. Every value carries
its kind after a colon on first definition: `%3: Owned Vec<i64>`,
`%4: Guaranteed Pair`, `%7: None i64`. Ownership ops are the §1.3 set and
nothing else; there is no operand mode tag. MIR is written as the §4.3
instruction names with the runtime symbol in a trailing comment. The class
table row for every type in an example is stated first, because the class
decides every op that follows (`ir-ladder.md` §1.1).

The recurring lesson, stated once: **the legacy lowerer decides ownership
from the shape of the consumer** (a projection, a match arm, a call
spelling), so every new consumer shape is a new special case and a new
place to be wrong. The ladder decides ownership **from the value's class
and its last use**, once, in SIR, and the consumer shapes below all reduce
to the same six ops.

## W1. Borrowed match on a payload inside a live record (#3226, #3090)

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
`p` has header mode `Borrow` (no `consume`), so `%p` is `Guaranteed` for the
whole body and is never consumed (rule 3).

`p.value` is not a last use of `p` (`p.other` follows), so the match reads
the payload **inside a borrow** and never destructures. `n` is a
non-escaping `var`, so it is mem2reg'd into block arguments (§1.3, §1.4).

```
fn total(%0: Guaranteed Pair) -> i64 {
bb0:
    %1: None i64 = const.i64 0
    %2: Guaranteed Option<Vec<i64>> = project %0, .value      ; a Guaranteed projection, no op cost
    switch_enum %2 [Some: bb1, None: bb2]
bb1:
    %3: Guaranteed Vec<i64> = project_payload %2, Some.0     ; still inside the borrow of %0
    %4: None i64 = call @hew_vec_len(%3)                     ; Borrow slot: reads through the projection
    br bb3(%4)
bb2:
    br bb3(%1)
bb3(%5: None i64):
    %6: Guaranteed Vec<i64> = project %0, .other
    %7: None i64 = call @hew_vec_len(%6)
    %8: None i64 = add %5, %7
    ret %8
}
```

Verifier trace. Rule 1: no `Owned` value is defined in the body, so nothing
to balance. Rule 3: every `Guaranteed` use (`%2`, `%3`, `%6`) is inside the
parameter's whole-body borrow; no consumer of `%0` exists. Rule 4: no
places. The proof is trivial, and that is the point: a read-only match on a
borrowed record has **no ownership ops at all**.

MIR: `Load` of two field projections and two calls; no `Retain`, `Release`,
`Move`, or `Fork`. The caller owns `p` and destroys it once, after the call,
with `Release { p }` → `hew_drop$Pair` (§5.2 item 1: `value` then `other`,
reverse declaration order; the enum arm releases the payload under the
`Some` tag, §5.2 item 2).

LLVM: two GEPs, two `hew_vec_len` calls, one `select`/`phi` for `n`. The
callee emits no cleanup.

What the legacy lowerer did: it minted an owner for the `Some` binder at
the destructure site ("generation" of the payload), then had to neutralize
the parent's field so `hew_drop$Pair` would not release the payload twice,
and the neutralize retired the whole root generation, which is why
`p.other` leaked afterwards (#3090) and why every sibling shape (a loop, a
guarded arm, an `if let`) needed its own patch. Under the ladder there is no
binder owner to neutralize because a borrowed payload is a `Guaranteed`
projection, not a value.

Fixture: `repros/ladder/worked/w1_borrowed_match.hew` prints `5` and exits
0 under ASan with zero leaks; the SIR golden is the block above.

## W2. Consuming match on a record that is a last use

```hew
fn first_len(p: Pair) -> i64 {
    match p.value {
        .Some(v) => v.len(),
        .None => 0,
    }
}
```

Same classes. `p.value` **is** the last use of `p` (the tail expression is
the only use), and the header slot is `Borrow`, so this is still not a
consuming match: the arm reads through the borrow exactly as W1 and the SIR
is W1 without `bb3`'s second projection. A borrowed parameter is never
destructured (rule 3: a `Guaranteed` value is never an operand of a
consuming position).

The consuming variant needs the header to say so:

```hew
fn take_first(consume p: Pair) -> Vec<i64> {
    match p.value {
        .Some(v) => v,
        .None => Vec.new(),
    }
}
```

Now `%p` is `Owned`, `p.value` is its last use, and the match consumes it.
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
    %3: Owned Vec<i64>, %4: Owned Option<Vec<i64>> = destructure %0    ; other, value
    destroy_value %3                                     ; `other` is unused in this arm
    %5: Owned Vec<i64> = destructure_payload %4, Some.0  ; the tag is proven; take the payload
    br bb3(%5)
bb2:
    end_borrow %1
    %6: Owned Vec<i64>, %7: Owned Option<Vec<i64>> = destructure %0
    destroy_value %6
    destroy_value %7                                     ; a None payload releases nothing (glue switches on the tag)
    %8: Owned Vec<i64> = call @hew_vec_new()             ; Fresh
    br bb3(%8)
bb3(%9: Owned Vec<i64>):
    ret %9                                               ; move into the return slot
}
```

Verifier trace. Rule 1 for `%0`: consumed exactly once on each path (the
`destructure` in `bb1`, the `destructure` in `bb2`). For `%3`, `%4`, `%5`,
`%6`, `%7`, `%8`, `%9`: one consumer each on their path (`destroy_value`,
`destructure_payload`, an edge argument, `ret`). Rule 3: `%1`'s region ends
before the consumer of `%0` in each arm. Rule 5: the `ret` operand is
`Move`.

MIR: `Move` per field out of the aggregate (§4.3 `destructure` row);
`Release { other }` → `hew_vec_free`; in `bb2` `Release { value }` →
`hew_drop$Option<Vec<i64>>`; no `Retain` anywhere. The caller passed `p`
into a `Consume` slot and emits nothing after the call.

What the legacy lowerer did: it treated `p.value` as a projection of a
still-live owner and moved the payload out while the owner's release plan
still covered it, or retired the owner and leaked `other`; which of the two
depended on whether the arm was a block tail (#3274). Under the ladder the
whole record is destructured in the arm, every part is named, and every
part is consumed exactly once.

Fixture: `repros/ladder/worked/w2_consuming_match.hew` prints `3` for
`take_first(consume Pair{other: [1], value: .Some([1,2,3])}).len()`; ASan
zero leaks.

## W3. Closing one resource field of a composite (#3070)

```hew
#[resource] type Conn { fd: i64 }
impl Conn { fn close(self) { println(f"close {self.fd}") } }
type Two { a: Conn, b: Conn }

fn shut_a(t: Two) { t.a.close(); }
```

Classes: `Conn` is `AffineResource` (clone `None`, `UserClose` glue);
`Two` is `AffineResource` by the aggregate rule. `t` is `Borrow` (no
`consume`).

This program is **refused**, and the refusal is the whole fix for #3070.
`t.a` is a `Guaranteed` projection inside the borrow of `%t`; `close` has a
`Consume` receiver slot (§4.2); a `Guaranteed` value is never an operand of
a consuming position (rule 3). Diagnostic: `E_OWN_CONSUME_BORROWED`
"`t.a` is borrowed through parameter `t`; declare it `consume t: Two` and
destructure it, or close through a consuming method of `Two`". Nothing is
lowered.

The spellings that compile, and their IR:

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
    call @Conn.close(%1)          ; Consume slot: %1 consumed here
    call @Conn.close(%2)
    ret
}
```

Rule 1 for `%0`, `%1`, `%2`: one consumer each. Rule 6d does not apply
(`AffineResource`, not `Linear`). If the body were `a.close();` alone, the
verifier inserts nothing: HIR→SIR emits `destroy_value %2` at scope exit
(§1.3.4 rule for every live `Owned` binding), which lowers to `Release {
b }` → `Conn.close` through `UserClose` glue. One close per field, always,
because each field is one `Owned` value with one consumer.

A partial close through a live aggregate is expressible only as a state
field with a taken bit (§1.3.6), never as a local record. That is a
tightening the spec already promises (§3.7.8.4 Path 2 is for handlers, not
locals), and `repros/ladder/worked/w3_partial_close.hew` moves to `reject`
with the diagnostic above; `w3_destructure_close.hew` prints `close 1`,
`close 2` under ASan with no double free.

What the legacy lowerer did: it emitted the user close for `t.a` and then
walked `t`'s fields at scope exit, closing `a` a second time; the fix on PR
3246 retired the whole record at the first close and thereby leaked `b`
(refuter finding), and the flow-insensitive retirement double-closed on
`if`/`else`. Under the ladder the aggregate has no "walk"; it is either one
`Owned` value released by glue, or it has been destructured into named
parts.

## W4. Match over a fresh call result (#3127, #2523)

```hew
fn drain(rx: RecvHalf<string>) -> string {
    match rx.recv() {
        .Ok(s) => s,
        .Err(e) => f"error: {e}",
    }
}
```

Classes: `Result<string, RecvError>` is `CowValue` (string is `CowValue`,
`RecvError` is `BitCopy`); `RecvHalf<string>` is `AffineResource`; `rx` is
`Borrow`. `rx.recv()` returns `Fresh` (§4.2), so its result is an unnamed
`Owned` temporary of the enclosing block (§1.3.4).

```
fn drain(%0: Guaranteed RecvHalf<string>) -> string {
bb0:
    %1: Owned Result<string, RecvError> = call @hew_recv_half_recv(%0)   ; Fresh
    %2: Guaranteed Result<string, RecvError> = begin_borrow %1
    switch_enum %2 [Ok: bb1, Err: bb2]
bb1:
    end_borrow %2
    %3: Owned string = destructure_payload %1, Ok.0     ; %1 consumed; the temporary's last use
    br bb3(%3)
bb2:
    end_borrow %2
    %4: None RecvError = destructure_payload %1, Err.0  ; BitCopy part; %1 consumed
    %5: Owned string = call @hew_string_format_error(%4)  ; Fresh
    br bb3(%5)
bb3(%6: Owned string):
    ret %6
}
```

Verifier trace. Rule 1 for `%1`: one consumer on each path (the
`destructure_payload` in each arm). Because the arm consumed the temporary,
the block-exit `destroy_value %1` of §1.3.4 is **not** emitted on those
paths; had an arm only read the payload (`.Ok(s) => s.len()`), the arm
would `copy_value` the projection inside the borrow and the temporary's
`destroy_value` would sit at the block exit, once.

MIR: `Move` of the payload out of the temporary (`Move { src: tmp.Ok.0,
dst }`), no `Release` of the temporary on that path (the shell is inline,
§4.3 `destructure` row: "`Release` of the shell if boxed", and a `Result` is
not boxed). The runtime symbol for `recv` is the row in
`runtime_symbols.rs` whose result is `Fresh`; nothing about the channel's
handle is touched (the handle was `Borrow`).

What the legacy lowerer did: it registered the `recv` result as a
scrutinee owner and released it at statement end, after the arm had
already moved the payload out, so the returned string was freed (#3127);
the fix in codegen special-cased "layout recv" families, and the refuter
found the guarded-arm sibling leaking one frame per iteration. Under the
ladder the temporary is an `Owned` value like any other, and the arm's
`destructure_payload` is its one consumer.

Fixture: `repros/ladder/worked/w4_recv_match.hew` prints the received
string and exits 0 under ASan.

## W5. Block tail versus `return` of a projected field (#3274)

```hew
fn take_a(consume p: Pair) -> Vec<i64> { p.other }
fn take_b(consume p: Pair) -> Vec<i64> { return p.other; }
```

Both functions produce **the same SIR**. A block tail in return position
and an explicit `return` both lower to `ret` of an `Owned` operand (rule 5:
the `Return` operand mode is `Move`); the intent stamp the legacy lowerer
reads (`IntentKind::Read` on a tail, `IntentKind::Consume` on a `return`)
does not exist in SIR.

```
fn take_a(%0: Owned Pair) -> Vec<i64> {
bb0:
    %1: Owned Vec<i64>, %2: Owned Option<Vec<i64>> = destructure %0
    destroy_value %2                     ; value is not returned
    ret %1
}
```

Rule 1: `%0` consumed once, `%1` once (`ret`), `%2` once (`destroy_value`).
MIR: `Move` per field, `Release { value }` → `hew_drop$Option<Vec<i64>>`,
`Move` into the return slot. The `free(): invalid pointer` abort on `main`
for `take_a` is a projection moved out of a record whose release plan still
covered it; the ladder has no release plan, only consumers.

Fixture: `repros/ladder/worked/w5_tail_vs_return.hew` runs both and prints
`2`, `2`; ASan zero leaks and no abort.

## W6. An owned value across a loop back edge with an early exit (#3250)

```hew
fn find(items: Vec<string>, needle: string) -> string {
    var found = "";
    for s in items {
        if s == needle { found = s; break; }
    }
    return found;
}
```

Classes: `string` and `Vec<string>` are `CowValue`; both parameters
`Borrow`. `found` is a non-escaping `var`: block argument at the loop
header. `s` is a loop binding produced by the iterator each iteration: a
`Fresh` `Owned` value per iteration (`hew_vec_iter_next` returns a
retained element under §5.3; the row is `Fresh`).

```
fn find(%0: Guaranteed Vec<string>, %1: Guaranteed string) -> string {
bb0:
    %2: Owned string = const.str ""
    %3: Owned VecIter<string> = call @hew_vec_iter(%0)           ; Fresh, AffineResource
    br bb1(%2)
bb1(%4: Owned string):                                           ; loop header; `found` arrives here
    %5: Owned Option<string> = call @hew_vec_iter_next(%3)       ; Fresh
    %6: Guaranteed Option<string> = begin_borrow %5
    switch_enum %6 [Some: bb2, None: bb5]
bb2:
    end_borrow %6
    %7: Owned string = destructure_payload %5, Some.0            ; `s`
    %8: Guaranteed string = begin_borrow %7
    %9: None bool = call @hew_string_equals(%8, %1)
    end_borrow %8
    br_if %9, bb3, bb4
bb3:                                                             ; found = s; break
    destroy_value %4                                             ; old `found`
    destroy_value %3                                             ; the iterator, leaving the loop
    br bb6(%7)                                                   ; `s` becomes `found`
bb4:                                                             ; continue
    destroy_value %7                                             ; `s` unused past here
    br bb1(%4)                                                   ; `found` unchanged around the back edge
bb5:                                                             ; iterator exhausted
    end_borrow %6
    destroy_value %5                                             ; the None; releases nothing
    destroy_value %3
    br bb6(%4)
bb6(%10: Owned string):
    ret %10
}
```

Verifier trace. Rule 1 per path: `%4` is consumed on the `break` path
(`destroy_value`) and on the exhausted path (edge argument), and re-passed
unchanged around the back edge; `%7` is consumed in `bb3` (edge argument)
or `bb4` (`destroy_value`); `%5` is consumed in `bb2` (`destructure_payload`)
or `bb5` (`destroy_value`); `%3` is consumed on both loop exits and on no
back edge (§1.4: a value live around the loop is a block argument or is
untouched; the iterator is untouched and consumed only on exit). Rule 3:
the `%8` borrow ends before `%7` is consumed.

The `for await` return edge of #3250 is this shape with a `Suspend` in
`bb1` and the frame binder as the loop argument; the release the legacy
lowerer withheld on the `return` edge is `bb3`'s `destroy_value` of the
binder, which the ladder emits by the same rule as `break`, because both
are exits of the loop scope (§1.3.4 lists them together).

MIR: `Release { found_old }` → `hew_string_drop` in `bb3`; `Release { iter }`
→ the iterator's leaf release on both exits; `Move` for the edge
arguments; no `Retain` (the iterator row returns a retained element, so
the element is already owned).

Fixture: `repros/ladder/worked/w6_loop_break.hew` prints `b` for
`find(["a", "b", "c"], "b")` and `""` for a miss; ASan zero leaks on both.

## W7. Actor state: push, close, re-initialize

The full rule set is `ir-ladder.md` §1.3.6; this is the op sequence in one
place, because three of the seven failing shapes were actor-state variants
of W1–W3.

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
    %1: Owned Vec<i64> = fork %0                    ; ensure_unique (no-op under §5.5)
    call @hew_vec_push_owned_move(%1, %n)           ; Borrow slot by pointer
    store.init %items, %1                           ; place Init again on every exit, unwind and cancel included

; cycle: consume, then re-initialize, an AffineResource place
    %2: Owned Conn = load.take %conn                ; sets the taken bit (MIR MarkUninit)
    call @Conn.close(%2)                            ; Consume slot
    %3: Owned Conn = call @Conn.open(2)             ; Fresh
    store.assign %conn, %3                          ; bit-guarded: if !taken { taken := 1; Release }; Store; taken := 0
    %4: Guaranteed Conn = begin_borrow %conn
    %5: None i64 = project %4, .fd
    end_borrow %4
    ret %5
```

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
  refutation is redesign here first, never a retry in the lowerer.
- The legacy lowerer's `hew-mir/src/lower/**` is not a reference for any
  op in this file; every "what the legacy lowerer did" paragraph is there
  so a reader knows which instinct to unlearn.
