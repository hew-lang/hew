#!/usr/bin/env python3
"""Generate the core-matrix cell corpus: every core primitive crossed with
every common operation, one runnable program per cell.

The corpus under tests/core-matrix/cells/ is GENERATED, not hand-maintained.
A new primitive is a new row descriptor here; a new operation is a new column
function. That keeps the matrix a systematic enumeration of the language
rather than an accretion of one-off fixtures.

Each emitted cell is a complete program with a `fn main()` that prints a
canonical, exact result. The runner (scripts/core-matrix.sh) compares stdout
byte-for-byte against the sibling `.expected` file, so a cell proves the VALUE
and -- where the row carries a `#[resource]` whose `close` prints -- proves
exactly-once release as well.

Usage:
    python3 scripts/core-matrix-gen.py [--out tests/core-matrix/cells]
"""

from __future__ import annotations

import argparse
import os
import shutil
import sys

# --------------------------------------------------------------------------
# Columns: the common operations every primitive is crossed with.
# --------------------------------------------------------------------------

COLUMNS = [
    "construct",
    "clone",
    "move",
    "pass_fn",
    "return_fn",
    "store_vec",
    "store_map_value",
    "map_key",
    "read_back",
    "iterate",
    "index",
    "mutate",
    "equality",
    "format",
    "actor_send",
    "across_suspend",
    "drop_scope",
    "drop_early_return",
    "drop_panic",
]

# The observable-close resource used by every ownership cell. `close` prints,
# so exactly-once release is an exact-stdout property rather than an
# out-of-band leak check.
TOK = """#[resource]
type Tok {
    id: i64
}

impl Tok {
    fn close(self) {
        println(f"close {self.id}");
    }
}
"""


def ind(text: str, n: int = 4) -> str:
    pad = " " * n
    return "\n".join(pad + line if line.strip() else line for line in text.splitlines())


class Row:
    """One primitive: how to build it, show it, and what it should print."""

    def __init__(
        self,
        rid,
        ty,
        decls="",
        mk=None,
        show=None,
        exp=None,
        fmt=None,
        keyable=False,
        eq=True,
        display=True,
        owns_resource=None,
        closes=None,
        overrides=None,
    ):
        self.id = rid
        self.ty = ty
        self.decls = decls
        self._mk = mk
        self._show = show
        self._exp = exp
        self._fmt = fmt
        self.keyable = keyable
        self.eq = eq
        self.display = display
        # owns_resource: (decls, mk_stmts(name), expected_close_lines) for the
        # ownership columns -- the row shaped around an observable resource.
        self.owns_resource = owns_resource
        # closes(i) -> the line the row's own value prints when it is released.
        # Rows that release observably (`#[resource]`, `#[opaque]`) carry one;
        # every non-rejecting column's expectation then states exactly-once
        # release as part of the exact-stdout oracle.
        self._closes = closes
        self.overrides = overrides or {}

    def closes(self, i=0):
        return self._closes(i) if self._closes else None

    def mk(self, name, i=0):
        return self._mk(name, i)

    def show(self, name):
        return self._show(name)

    def exp(self, i=0):
        return self._exp(i)

    def fmt(self, i=0):
        return self._fmt(i) if self._fmt else self.exp(i)


# --------------------------------------------------------------------------
# Row descriptors
# --------------------------------------------------------------------------


def scalar(rid, ty, lits, exps, keyable=False):
    return Row(
        rid,
        ty,
        mk=lambda n, i, ty=ty, lits=lits: f"let {n}: {ty} = {lits[i]};",
        show=lambda n: f'println(f"{{{n}}}");',
        exp=lambda i, exps=exps: exps[i],
        keyable=keyable,
    )


ROWS = []

ROWS.append(scalar("i32", "i32", ["7", "9"], ["7", "9"], keyable=True))
ROWS.append(scalar("i64", "i64", ["42", "43"], ["42", "43"], keyable=True))
ROWS.append(scalar("f64", "f64", ["3.5", "4.5"], ["3.5", "4.5"], keyable=True))
ROWS.append(scalar("bool", "bool", ["true", "false"], ["true", "false"], keyable=True))
ROWS.append(scalar("string", "string", ['"ab"', '"cd"'], ["ab", "cd"], keyable=True))

# bytes -- heap buffer, immutable-shareable, no Display.
ROWS.append(
    Row(
        "bytes",
        "bytes",
        mk=lambda n, i: (
            f"let {n}: bytes = bytes::new();\n"
            + "\n".join(f"{n}.push({65 + k});" for k in range(2 + i))
        ),
        show=lambda n: (
            f'let {n}_l = {n}.len();\nlet {n}_0 = {n}[0];\nprintln(f"len={{{n}_l}} b0={{{n}_0}}");'
        ),
        exp=lambda i: f"len={2 + i} b0=65",
        display=False,
        eq=False,
    )
)

# Vec<i64>
ROWS.append(
    Row(
        "vec",
        "Vec<i64>",
        mk=lambda n, i: (
            f"let {n}: Vec<i64> = Vec::new();\n"
            + "\n".join(f"{n}.push({10 * (k + 1) + i});" for k in range(2))
        ),
        show=lambda n: (
            f'let {n}_l = {n}.len();\nlet {n}_0 = {n}[0];\nprintln(f"len={{{n}_l}} v0={{{n}_0}}");'
        ),
        exp=lambda i: f"len=2 v0={10 + i}",
        display=False,
        eq=False,
        owns_resource=(
            TOK,
            lambda n: f"let {n}: Vec<Tok> = Vec::new();\n{n}.push(Tok {{ id: 1 }});",
            ["close 1"],
        ),
    )
)

# HashMap<string, i64>
ROWS.append(
    Row(
        "hashmap",
        "HashMap<string, i64>",
        mk=lambda n, i: (
            f"let {n}: HashMap<string, i64> = HashMap::new();\n"
            f'{n}.insert("a", {1 + i});\n'
            f'{n}.insert("b", {2 + i});'
        ),
        show=lambda n: (
            f"let {n}_l = {n}.len();\n"
            f'let {n}_a = match {n}.get("a") {{ Some({n}_g) => {n}_g, None => -1, }};\n'
            f'println(f"len={{{n}_l}} a={{{n}_a}}");'
        ),
        exp=lambda i: f"len=2 a={1 + i}",
        display=False,
        eq=False,
        owns_resource=(
            TOK,
            lambda n: (
                f'let {n}: HashMap<string, Tok> = HashMap::new();\n{n}.insert("k", Tok {{ id: 1 }});'
            ),
            ["close 1"],
        ),
    )
)

# HashSet<i64>
ROWS.append(
    Row(
        "hashset",
        "HashSet<i64>",
        mk=lambda n, i: (
            f"let {n}: HashSet<i64> = HashSet::new();\n"
            f"{n}.insert({1 + i});\n"
            f"{n}.insert({2 + i});"
        ),
        show=lambda n: (
            f"let {n}_l = {n}.len();\n"
            f"let {n}_h = {n}.contains(1);\n"
            f'println(f"len={{{n}_l}} has1={{{n}_h}}");'
        ),
        exp=lambda i: f"len=2 has1={'true' if i == 0 else 'false'}",
        display=False,
        eq=False,
    )
)

# Option<i64>
ROWS.append(
    Row(
        "option",
        "Option<i64>",
        mk=lambda n, i: f"let {n}: Option<i64> = Some({5 + i});",
        show=lambda n: (
            f'match {n} {{ Some({n}_v) => println(f"some {{{n}_v}}"), None => println("none"), }}'
        ),
        exp=lambda i: f"some {5 + i}",
        display=False,
        owns_resource=(
            TOK,
            lambda n: f"let {n}: Option<Tok> = Some(Tok {{ id: 1 }});",
            ["close 1"],
        ),
    )
)

# Result<i64, string>
ROWS.append(
    Row(
        "result",
        "Result<i64, string>",
        mk=lambda n, i: f"let {n}: Result<i64, string> = Ok({6 + i});",
        show=lambda n: (
            f'match {n} {{ Ok({n}_v) => println(f"ok {{{n}_v}}"), Err({n}_e) => println(f"err {{{n}_e}}"), }}'
        ),
        exp=lambda i: f"ok {6 + i}",
        display=False,
        owns_resource=(
            TOK,
            lambda n: f"let {n}: Result<Tok, string> = Ok(Tok {{ id: 1 }});",
            ["close 1"],
        ),
    )
)

# tuple (i64, string)
ROWS.append(
    Row(
        "tuple",
        "(i64, string)",
        mk=lambda n, i: f'let {n}: (i64, string) = ({1 + i}, "x{i}");',
        show=lambda n: f'let ({n}_a, {n}_b) = {n};\nprintln(f"{{{n}_a}} {{{n}_b}}");',
        exp=lambda i: f"{1 + i} x{i}",
        display=False,
        owns_resource=(
            TOK,
            lambda n: f"let {n}: (i64, Tok) = (1, Tok {{ id: 1 }});",
            ["close 1"],
        ),
    )
)

# record
REC = """type Rec {
    a: i64;
    b: string;
}
"""
ROWS.append(
    Row(
        "record",
        "Rec",
        decls=REC,
        mk=lambda n, i: f'let {n} = Rec {{ a: {1 + i}, b: "x{i}" }};',
        show=lambda n: (
            f'let {n}_a = {n}.a;\nlet {n}_b = {n}.b;\nprintln(f"{{{n}_a}} {{{n}_b}}");'
        ),
        exp=lambda i: f"{1 + i} x{i}",
        display=False,
        owns_resource=(
            TOK + "\ntype ResRec {\n    t: Tok;\n}\n",
            lambda n: f"let {n} = ResRec {{ t: Tok {{ id: 1 }} }};",
            ["close 1"],
        ),
    )
)

# enum without payload
ENUM_UNIT = """enum Colour {
    Red;
    Green;
}
"""
ROWS.append(
    Row(
        "enum_unit",
        "Colour",
        decls=ENUM_UNIT,
        mk=lambda n, i: f"let {n}: Colour = {'Red' if i == 0 else 'Green'};",
        show=lambda n: (
            f'match {n} {{ Red => println("Red"), Green => println("Green"), }}'
        ),
        exp=lambda i: "Red" if i == 0 else "Green",
        display=False,
    )
)

# enum with payload
ENUM_PAY = """enum Shape {
    Empty;
    Circle(i64);
    Rect { w: i64; h: i64 }
}
"""
ROWS.append(
    Row(
        "enum_payload",
        "Shape",
        decls=ENUM_PAY,
        mk=lambda n, i: f"let {n}: Shape = Circle({3 + i});",
        show=lambda n: (
            f"match {n} {{\n"
            f'    Empty => println("empty"),\n'
            f'    Circle({n}_r) => println(f"circle {{{n}_r}}"),\n'
            f'    Rect {{ w, h }} => println(f"rect {{w}}x{{h}}"),\n'
            f"}}"
        ),
        exp=lambda i: f"circle {3 + i}",
        display=False,
        owns_resource=(
            TOK + "\nenum Held {\n    Nothing;\n    One(Tok);\n}\n",
            lambda n: f"let {n}: Held = One(Tok {{ id: 1 }});",
            ["close 1"],
        ),
    )
)

# #[resource]
ROWS.append(
    Row(
        "resource",
        "Tok",
        decls=TOK,
        mk=lambda n, i: f"let {n} = Tok {{ id: {1 + i} }};",
        show=lambda n: f'let {n}_i = {n}.id;\nprintln(f"tok {{{n}_i}}");',
        exp=lambda i: f"tok {1 + i}",
        display=False,
        eq=False,
        closes=lambda i: f"close {1 + i}",
        owns_resource=(TOK, lambda n: f"let {n} = Tok {{ id: 1 }};", ["close 1"]),
    )
)

# #[opaque] -- FFI-backed handle, here the runtime deque.
OPAQUE = """#[opaque]
type DqH {
}

#[resource]
type Dq {
    handle: DqH;
    tag: i64;
}

impl Dq {
    fn close(self) {
        unsafe { hew_deque_free(self.handle) };
        println(f"close {self.tag}");
    }
}

extern "C" {
    fn hew_deque_new() -> DqH;
    fn hew_deque_free(dq: DqH);
    fn hew_deque_len(dq: DqH) -> i64;
}
"""
ROWS.append(
    Row(
        "opaque",
        "Dq",
        decls=OPAQUE,
        mk=lambda n, i: (
            f"let {n} = Dq {{ handle: unsafe {{ hew_deque_new() }}, tag: {1 + i} }};"
        ),
        show=lambda n: (
            f'let {n}_l = unsafe {{ hew_deque_len({n}.handle) }};\nlet {n}_t = {n}.tag;\nprintln(f"dq {{{n}_t}} len={{{n}_l}}");'
        ),
        exp=lambda i: f"dq {1 + i} len=0",
        display=False,
        eq=False,
        closes=lambda i: f"close {1 + i}",
        owns_resource=(
            OPAQUE,
            lambda n: (
                f"let {n} = Dq {{ handle: unsafe {{ hew_deque_new() }}, tag: 1 }};"
            ),
            ["close 1"],
        ),
    )
)

# machine
MACHINE = """machine Counter {
    events { Inc; Reset; }
    state Zero;
    state NonZero { value: i64; }
    on Inc: Zero => NonZero { NonZero { value: 1 } }
    on Inc: NonZero => NonZero reenter { NonZero { value: self.value + 1 } }
    on Reset: NonZero => Zero { Zero }
    default { state }
}
"""
ROWS.append(
    Row(
        "machine",
        "Counter",
        decls=MACHINE,
        mk=lambda n, i: f"var {n} = Zero;\n" + "\n".join([f"{n}.step(Inc);"] * (1 + i)),
        show=lambda n: (
            f"match {n} {{\n"
            f'    Zero => println("zero"),\n'
            f'    NonZero {{ value }} => println(f"n {{value}}"),\n'
            f"}}"
        ),
        exp=lambda i: f"n {1 + i}",
        display=False,
        eq=False,
        owns_resource=(
            TOK
            + """
machine Gate {
    events { Open; Shut; }
    state Closed;
    state Opened { tok: Tok; }
    on Open: Closed => Opened { Opened { tok: Tok { id: 1 } } }
    on Shut: Opened => Closed { Closed }
    default { state }
}
""",
            lambda n: f"var {n} = Closed;\n{n}.step(Open);",
            ["close 1"],
        ),
    )
)

# actor
ACTOR = """actor Echo {
    var seen: i64;

    receive fn take(v: i64) -> i64 {
        seen = seen + v;
        seen
    }
}
"""
ROWS.append(
    Row(
        "actor",
        "LocalPid<Echo>",
        decls=ACTOR,
        mk=lambda n, i: f"let {n} = spawn Echo(seen: {i});",
        show=lambda n: (
            f'match await {n}.take(1) {{ Ok({n}_v) => println(f"echo {{{n}_v}}"), Err(_) => println("err"), }}'
        ),
        exp=lambda i: f"echo {1 + i}",
        display=False,
        eq=False,
    )
)

# closure
ROWS.append(
    Row(
        "closure",
        "fn(i64) -> i64",
        mk=lambda n, i: f"let {n} = |v: i64| v * {2 + i};",
        show=lambda n: f'let {n}_r = {n}(21);\nprintln(f"{{{n}_r}}");',
        exp=lambda i: str(21 * (2 + i)),
        display=False,
        eq=False,
    )
)

# generator
ROWS.append(
    Row(
        "generator",
        "Generator<i64, ()>",
        mk=lambda n, i: (
            f"let {n} = gen {{\n    yield {1 + i};\n    yield {2 + i};\n}};"
        ),
        show=lambda n: (
            f"var {n}_sum = 0;\n"
            f"for {n}_y in {n} {{\n    {n}_sum = {n}_sum + {n}_y;\n}}\n"
            f'println(f"sum {{{n}_sum}}");'
        ),
        exp=lambda i: f"sum {3 + 2 * i}",
        display=False,
        eq=False,
    )
)

# trait object
TRAITOBJ = """trait Named {
    fn name(val: Self) -> string;
}

type P {
    n: string;
}

impl Named for P {
    fn name(val: P) -> string {
        val.n
    }
}

type Q {
    n: string;
}

impl Named for Q {
    fn name(val: Q) -> string {
        val.n
    }
}

fn show_named(v: dyn Named) {
    let s = v.name();
    println(s);
}
"""
ROWS.append(
    Row(
        "traitobj",
        "dyn Named",
        decls=TRAITOBJ,
        mk=lambda n, i: f'let {n}: dyn Named = P {{ n: "p{i}" }};',
        show=lambda n: f"show_named({n});",
        exp=lambda i: f"p{i}",
        display=False,
        eq=False,
    )
)

ROWS_BY_ID = {r.id: r for r in ROWS}

SCALARS = {"i32", "i64", "f64", "bool"}

# --------------------------------------------------------------------------
# Column generators
# --------------------------------------------------------------------------


def prog(row, body, extra_decls="", extra_fns=""):
    parts = []
    if extra_decls:
        parts.append(extra_decls.rstrip() + "\n")
    elif row.decls:
        parts.append(row.decls.rstrip() + "\n")
    if extra_fns:
        parts.append(extra_fns.rstrip() + "\n")
    parts.append("fn main() {\n" + ind(body.rstrip()) + "\n}\n")
    return "\n".join(parts)


def NA(reason):
    return ("NA", reason)


def col_construct(r):
    return prog(r, r.mk("x", 0) + "\n" + r.show("x")), [r.exp(0)]


def col_clone(r):
    body = r.mk("x", 0) + "\nlet y = clone x;\n" + r.show("y") + "\n" + r.show("x")
    return prog(r, body), [r.exp(0), r.exp(0)]


def col_move(r):
    body = r.mk("x", 0) + "\nlet y = x;\n" + r.show("y")
    return prog(r, body), [r.exp(0)]


def col_pass_fn(r):
    fn = "fn use_it(v: " + r.ty + ") {\n" + ind(r.show("v")) + "\n}\n"
    body = r.mk("x", 0) + "\nuse_it(x);"
    return prog(r, body, extra_fns=fn), [r.exp(0)]


def col_return_fn(r):
    fn = "fn make() -> " + r.ty + " {\n" + ind(r.mk("m", 0)) + "\n    m\n}\n"
    body = "let x = make();\n" + r.show("x")
    return prog(r, body, extra_fns=fn), [r.exp(0)]


def col_store_vec(r):
    body = (
        r.mk("x", 0)
        + "\n"
        + r.mk("y", 1)
        + f"\nlet v: Vec<{r.ty}> = Vec::new();\nv.push(x);\nv.push(y);\nlet n = v.len();\n"
        + 'println(f"len={n}");'
    )
    return prog(r, body), ["len=2"]


def col_store_map_value(r):
    body = (
        r.mk("x", 0)
        + f'\nlet m: HashMap<string, {r.ty}> = HashMap::new();\nm.insert("k", x);\n'
        + 'match m.get("k") {\n'
        + "    Some(e) => {\n"
        + ind(r.show("e"), 8)
        + "\n    },\n"
        + '    None => println("missing"),\n'
        + "}"
    )
    return prog(r, body), [r.exp(0)]


def col_map_key(r):
    # Generated for EVERY row, including the ones a user would not expect to
    # key a map with: the quality of the refusal is itself the measurement.
    body = (
        r.mk("k", 0)
        + f"\nlet m: HashMap<{r.ty}, i64> = HashMap::new();\nm.insert(k, 9);\n"
        + "match m.get(k) {\n"
        + '    Some(v) => println(f"got {v}"),\n'
        + '    None => println("missing"),\n'
        + "}"
    )
    return prog(r, body), ["got 9"]


def col_read_back(r):
    body = (
        r.mk("x", 0)
        + f"\nlet v: Vec<{r.ty}> = Vec::new();\nv.push(x);\n"
        + "match v.get(0) {\n"
        + "    Some(e) => {\n"
        + ind(r.show("e"), 8)
        + "\n    },\n"
        + '    None => println("missing"),\n'
        + "}"
    )
    return prog(r, body), [r.exp(0)]


def col_iterate(r):
    body = (
        r.mk("a", 0)
        + "\n"
        + r.mk("b", 1)
        + f"\nlet v: Vec<{r.ty}> = Vec::new();\nv.push(a);\nv.push(b);\n"
        + "for e in v {\n"
        + ind(r.show("e"))
        + "\n}"
    )
    return prog(r, body), [r.exp(0), r.exp(1)]


def col_index(r):
    body = (
        r.mk("a", 0)
        + "\n"
        + r.mk("b", 1)
        + f"\nlet v: Vec<{r.ty}> = Vec::new();\nv.push(a);\nv.push(b);\nlet e = v[0];\n"
        + r.show("e")
    )
    return prog(r, body), [r.exp(0)]


def col_mutate(r):
    # Rebind a `var` of the row's type. Only rows whose construction is a
    # single `let` have a generic form; container rows override with their
    # native in-place mutation.
    mk0 = r.mk("x", 0)
    mk1 = r.mk("t", 1)
    if "\n" in mk0 or not mk0.startswith("let ") or "\n" in mk1:
        return NA("covered by the row-specific mutation override")
    body = (
        mk0.replace("let ", "var ", 1)
        + "\n"
        + f"x = {mk1.split('= ', 1)[1].rstrip(';')};\n"
        + r.show("x")
    )
    return prog(r, body), [r.exp(1)]


def col_equality(r):
    # Generated for EVERY row: whether `==` is supported, and how clearly it
    # is refused when it is not, is the measurement.
    body = (
        r.mk("a", 0)
        + "\n"
        + r.mk("b", 0)
        + "\n"
        + r.mk("c", 1)
        + "\nlet same = a == b;\nlet diff = a == c;\n"
        + 'println(f"{same} {diff}");'
    )
    return prog(r, body), ["true false"]


def col_format(r):
    body = r.mk("x", 0) + '\nprintln(f"v={x}");'
    return prog(r, body), [f"v={r.fmt(0)}"]


def col_actor_send(r):
    decls = (r.decls.rstrip() + "\n\n") if r.decls else ""
    actor = (
        "actor Sink {\n"
        "    var hits: i64;\n\n"
        f"    receive fn take(v: {r.ty}) -> i64 {{\n"
        + ind(r.show("v"), 8)
        + "\n        hits = hits + 1;\n        hits\n    }\n}\n"
    )
    body = (
        r.mk("x", 0)
        + "\nlet s = spawn Sink(hits: 0);\n"
        + "match await s.take(x) {\n"
        + '    Ok(n) => println(f"hits {n}"),\n'
        + '    Err(_) => println("err"),\n'
        + "}"
    )
    return prog(r, body, extra_decls=decls + actor), [r.exp(0), "hits 1"]


def col_across_suspend(r):
    decls = (r.decls.rstrip() + "\n\n") if r.decls else ""
    actor = (
        "actor Holder {\n"
        "    var ready: bool;\n\n"
        f"    receive fn hold(v: {r.ty}) -> i64 {{\n"
        "        sleep(2ms);\n"
        + ind(r.show("v"), 8)
        + "\n        ready = true;\n        1\n    }\n}\n"
    )
    body = (
        r.mk("x", 0)
        + "\nlet h = spawn Holder(ready: false);\n"
        + "match await h.hold(x) {\n"
        + '    Ok(n) => println(f"held {n}"),\n'
        + '    Err(_) => println("err"),\n'
        + "}"
    )
    return prog(r, body, extra_decls=decls + actor), [r.exp(0), "held 1"]


def col_drop_scope(r):
    if r.owns_resource is None:
        return NA("no release obligation: the row cannot carry an owned resource")
    decls, mk, closes = r.owns_resource
    body = mk("h") + '\nprintln("body");'
    return prog(r, body, extra_decls=decls), ["body"] + closes


def col_drop_early_return(r):
    if r.owns_resource is None:
        return NA("no release obligation: the row cannot carry an owned resource")
    decls, mk, closes = r.owns_resource
    fn = (
        "fn run(early: bool) -> i64 {\n"
        + ind(mk("h"))
        + '\n    if early {\n        println("early");\n        return 1;\n    }\n'
        + '    println("late");\n    2\n}\n'
    )
    body = 'let n = run(true);\nprintln(f"n={n}");'
    return prog(r, body, extra_decls=decls, extra_fns=fn), ["early"] + closes + ["n=1"]


def col_drop_panic(r):
    if r.owns_resource is None:
        return NA("no release obligation: the row cannot carry an owned resource")
    decls, mk, closes = r.owns_resource
    fn = (
        "fn run() -> i64 {\n"
        + ind(mk("h"))
        + "\n    let v: Vec<i64> = Vec::new();\n"
        + '    println("before trap");\n'
        + "    v[3]\n}\n"
    )
    body = 'let n = run();\nprintln(f"n={n}");'
    # An out-of-bounds index TRAPS. The trap path still runs the frame's drop
    # obligations before aborting, so the expectation is the pre-trap output
    # followed by exactly-once release -- the same bar as the ordinary exit
    # paths. A row that releases on scope exit but not here would be a
    # trap-path-only leak.
    return (
        prog(r, body, extra_decls=decls, extra_fns=fn),
        ["before trap"] + closes,
        "trap",
    )


COLGEN = {
    "construct": col_construct,
    "clone": col_clone,
    "move": col_move,
    "pass_fn": col_pass_fn,
    "return_fn": col_return_fn,
    "store_vec": col_store_vec,
    "store_map_value": col_store_map_value,
    "map_key": col_map_key,
    "read_back": col_read_back,
    "iterate": col_iterate,
    "index": col_index,
    "mutate": col_mutate,
    "equality": col_equality,
    "format": col_format,
    "actor_send": col_actor_send,
    "across_suspend": col_across_suspend,
    "drop_scope": col_drop_scope,
    "drop_early_return": col_drop_early_return,
    "drop_panic": col_drop_panic,
}

# --------------------------------------------------------------------------
# Row-specific overrides -- container rows mutate/iterate/index natively, and
# a few rows have no meaningful generic form.
# --------------------------------------------------------------------------


# How many of the row's own values each column constructs, and where their
# release trace belongs in the expected output. Only rows that release
# observably (`closes`) consult this; for them exactly-once release is part of
# the same exact-stdout oracle that proves the value.
# The value is the sequence of value-indices the column constructs; each one
# must be released exactly once. "penult" means the release happens before the
# last line (the callee or handler releases before control returns to main).
CLOSE_POLICY = {
    "construct": ("end", [0]),
    "clone": ("end", [0, 0]),
    "move": ("end", [0]),
    "pass_fn": ("end", [0]),
    "return_fn": ("end", [0]),
    "store_vec": ("end", [0, 1]),
    "store_map_value": ("end", [0]),
    "map_key": ("end", [0]),
    "read_back": ("end", [0]),
    "iterate": ("end", [0, 1]),
    "index": ("end", [0, 1]),
    # A rebind releases the overwritten value and then the replacement.
    "mutate": ("end", [0, 1]),
    "equality": ("end", [0, 0, 1]),
    "format": ("end", [0]),
    "actor_send": ("penult", [0]),
    "across_suspend": ("penult", [0]),
}


def apply_close_trace(r, col, expected):
    if r.closes(0) is None or col not in CLOSE_POLICY:
        return expected
    where, idxs = CLOSE_POLICY[col]
    trace = [r.closes(i) for i in idxs]
    if where == "end":
        return expected + trace
    return expected[:-1] + trace + expected[-1:]


def OV(body, expected, decls=None, fns=None):
    return {"body": body, "expected": expected, "decls": decls, "fns": fns}


OVERRIDES = {
    ("bytes", "mutate"): OV(
        "let b: bytes = bytes::new();\nb.push(65);\nb.push(66);\nb.set(0, 67);\n"
        'let f = b[0];\nlet l = b.len();\nprintln(f"len={l} b0={f}");',
        ["len=2 b0=67"],
    ),
    ("vec", "mutate"): OV(
        "let v: Vec<i64> = Vec::new();\nv.push(10);\nv.push(20);\nv.set(0, 99);\n"
        'let f = v[0];\nlet l = v.len();\nprintln(f"len={l} v0={f}");',
        ["len=2 v0=99"],
    ),
    ("vec", "iterate"): OV(
        "let v: Vec<i64> = Vec::new();\nv.push(10);\nv.push(20);\n"
        "for e in v {\n    println(e);\n}",
        ["10", "20"],
    ),
    ("vec", "index"): OV(
        "let v: Vec<i64> = Vec::new();\nv.push(10);\nv.push(20);\n"
        'let a = v[0];\nlet b = v[1];\nprintln(f"{a} {b}");',
        ["10 20"],
    ),
    ("bytes", "iterate"): OV(
        "let b: bytes = bytes::new();\nb.push(65);\nb.push(66);\nvar i = 0;\n"
        "while i < b.len() {\n    println(b[i]);\n    i = i + 1;\n}",
        ["65", "66"],
    ),
    ("bytes", "index"): OV(
        "let b: bytes = bytes::new();\nb.push(65);\nb.push(66);\n"
        'let a = b[0];\nlet c = b[1];\nprintln(f"{a} {c}");',
        ["65 66"],
    ),
    ("hashmap", "mutate"): OV(
        'let m: HashMap<string, i64> = HashMap::new();\nm.insert("a", 1);\n'
        'match m.get("a") {\n    Some(v) => m.insert("a", v + 5),\n    None => {},\n}\n'
        'match m.get("a") {\n    Some(v) => println(f"a={v}"),\n    None => println("missing"),\n}',
        ["a=6"],
    ),
    ("hashmap", "iterate"): OV(
        'let m: HashMap<string, i64> = HashMap::new();\nm.insert("a", 1);\n'
        "let ks = m.keys();\nlet vs = m.values();\n"
        'let k0 = ks[0];\nlet v0 = vs[0];\nprintln(f"{k0}={v0}");',
        ["a=1"],
    ),
    ("hashmap", "index"): OV(
        'let m: HashMap<string, i64> = HashMap::new();\nm.insert("a", 1);\n'
        'let v = m["a"];\nprintln(v);',
        ["1"],
    ),
    ("hashset", "mutate"): OV(
        "let s: HashSet<i64> = HashSet::new();\ns.insert(1);\ns.insert(2);\n"
        'let r = s.remove(1);\nlet n = s.len();\nprintln(f"removed={r} len={n}");',
        ["removed=true len=1"],
    ),
    ("hashset", "iterate"): OV(
        "let s: HashSet<i64> = HashSet::new();\ns.insert(7);\n"
        "let v = s.to_vec();\nfor e in v {\n    println(e);\n}",
        ["7"],
    ),
    ("hashset", "index"): OV(
        "let s: HashSet<i64> = HashSet::new();\ns.insert(7);\n"
        'let h = s.contains(7);\nprintln(f"{h}");',
        ["true"],
    ),
    ("string", "iterate"): OV(
        'let s: string = "a,b";\nlet parts = s.split(",");\n'
        "for p in parts {\n    println(p);\n}",
        ["a", "b"],
    ),
    ("string", "index"): OV(
        'let s: string = "abc";\nlet t = s.slice(0, 1);\nprintln(t);',
        ["a"],
    ),
    ("generator", "iterate"): OV(
        "let g = gen {\n    yield 1;\n    yield 2;\n};\n"
        "for y in g {\n    println(y);\n}",
        ["1", "2"],
    ),
    ("machine", "mutate"): OV(
        "var c = Zero;\nc.step(Inc);\nc.step(Inc);\n"
        'match c {\n    Zero => println("zero"),\n'
        '    NonZero { value } => println(f"n {value}"),\n}',
        ["n 2"],
        decls=MACHINE,
    ),
    ("actor", "mutate"): OV(
        "let e = spawn Echo(seen: 0);\n"
        'match await e.take(5) {\n    Ok(v) => println(f"echo {v}"),\n    Err(_) => println("err"),\n}\n'
        'match await e.take(5) {\n    Ok(v) => println(f"echo {v}"),\n    Err(_) => println("err"),\n}',
        ["echo 5", "echo 10"],
        decls=ACTOR,
    ),
    ("closure", "mutate"): OV(
        "var f = |v: i64| v * 2;\nf = |v: i64| v * 3;\nlet r = f(7);\nprintln(r);",
        ["21"],
    ),
    ("traitobj", "mutate"): OV(
        'var d: dyn Named = P { n: "a" };\nd = Q { n: "b" };\nshow_named(d);',
        ["b"],
        decls=TRAITOBJ,
    ),
    # The natural form: bind a dyn local and dot-call it. The corpus's existing
    # coverage only ever calls through a `dyn` PARAMETER, so this is the cell
    # that asks whether the local binding carries the same capability.
    ("traitobj", "construct"): OV(
        'let d: dyn Named = P { n: "p0" };\nlet s = d.name();\nprintln(s);',
        ["p0"],
        decls=TRAITOBJ,
    ),
    ("machine", "index"): OV(
        "var c = Zero;\nc.step(Inc);\nlet n = c.state_name();\nprintln(n);",
        ["NonZero"],
        decls=MACHINE,
    ),
    ("actor", "index"): OV(
        "let e = spawn Echo(seen: 3);\n"
        'match await e.take(0) {\n    Ok(v) => println(f"echo {v}"),\n    Err(_) => println("err"),\n}',
        ["echo 3"],
        decls=ACTOR,
    ),
}

# Cells that are meaningless for the row: recorded as N/A with the reason.
NA_CELLS = {
    ("traitobj", "index"): "a trait object is not an indexable container",
    (
        "actor",
        "clone",
    ): "an actor handle is a spawned identity, not a value to duplicate",
    ("actor", "equality"): "actor handles have identity, not structural equality",
    ("actor", "drop_scope"): "actor lifetime is the runtime's, not the binding's scope",
    (
        "actor",
        "drop_early_return",
    ): "actor lifetime is the runtime's, not the binding's scope",
    ("actor", "drop_panic"): "actor lifetime is the runtime's, not the binding's scope",
    (
        "generator",
        "drop_scope",
    ): "generator state is the coroutine's, covered by the generator corpus",
    (
        "generator",
        "drop_early_return",
    ): "generator state is the coroutine's, covered by the generator corpus",
    (
        "generator",
        "drop_panic",
    ): "generator state is the coroutine's, covered by the generator corpus",
    ("closure", "drop_scope"): "the closure row here captures no owned value",
    ("closure", "drop_early_return"): "the closure row here captures no owned value",
    ("closure", "drop_panic"): "the closure row here captures no owned value",
    (
        "traitobj",
        "drop_scope",
    ): "trait-object drop is covered by dyn_owned_heap_drop_test",
    (
        "traitobj",
        "drop_early_return",
    ): "trait-object drop is covered by dyn_owned_heap_drop_test",
    (
        "traitobj",
        "drop_panic",
    ): "trait-object drop is covered by dyn_owned_heap_drop_test",
}


def generate(outdir):
    if os.path.isdir(outdir):
        shutil.rmtree(outdir)
    os.makedirs(outdir, exist_ok=True)
    manifest = []
    for r in ROWS:
        for c in COLUMNS:
            key = (r.id, c)
            if key in NA_CELLS:
                manifest.append((r.id, c, "NA", NA_CELLS[key]))
                continue
            trap = False
            if key in OVERRIDES:
                o = OVERRIDES[key]
                src = prog(
                    r,
                    o["body"],
                    extra_decls=o["decls"] if o["decls"] is not None else "",
                    extra_fns=o["fns"] or "",
                )
                exp = o["expected"]
            else:
                out = COLGEN[c](r)
                if out[0] == "NA":
                    manifest.append((r.id, c, "NA", out[1]))
                    continue
                src, exp = out[0], out[1]
                trap = len(out) > 2 and out[2] == "trap"
                exp = apply_close_trace(r, c, exp)
            name = f"{r.id}__{c}"
            with open(os.path.join(outdir, name + ".hew"), "w") as f:
                f.write(src)
            with open(os.path.join(outdir, name + ".expected"), "w") as f:
                f.write("\n".join(exp) + "\n")
            if trap:
                with open(os.path.join(outdir, name + ".trap"), "w") as f:
                    f.write("the program traps by design; no cleanup on that path\n")
            manifest.append((r.id, c, "CELL", name))
    return manifest


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default="tests/core-matrix/cells")
    args = ap.parse_args()
    manifest = generate(args.out)
    cells = sum(1 for m in manifest if m[2] == "CELL")
    na = sum(1 for m in manifest if m[2] == "NA")
    print(f"rows={len(ROWS)} columns={len(COLUMNS)} cells={cells} n/a={na}")
    with open(os.path.join(os.path.dirname(args.out), "na.tsv"), "w") as f:
        for rid, c, kind, note in manifest:
            if kind == "NA":
                f.write(f"{rid}\t{c}\t{note}\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
