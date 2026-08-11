#!/usr/bin/env python3
"""Compute the system-lane reachability closure over the runtime call graph.

`docs/internal/jit-host-abi.md` states the classification invariant:

    No `stable` symbol may produce, install, mutate, observe, or destroy
    system-lane state.

That property is TRANSITIVE. Four hand-audits of the classification table read
the property off each symbol's own body and each produced a different answer,
because a symbol that never mentions the system lane still destroys it one call
away (`hew_actor_free` -> `hew_mailbox_free` -> `sys_queue.drain_and_free`).
This script computes the property mechanically instead:

  1. ROOTS  -- every function whose own body touches the system-lane state set
               (`SYSTEM_LANE_TOKENS`), with comments, string literals and
               character literals stripped so prose cannot create or hide a
               root and a brace in a literal cannot desynchronise the parse.
  2. GRAPH  -- a name-keyed call graph over every `fn` defined in the scanned
               crates. Native and WASM definitions that share a name are merged
               deliberately: the invariant must hold on both targets, so a
               symbol is disqualified if EITHER target's body reaches the lane.
  3. CLOSURE-- reverse reachability from the roots. Anything that can reach a
               root is in the closure.
  4. GATE   -- every `stable` (or `stable-stdlib`) symbol in the closure is a
               violation.

Two escape hatches exist, both in `scripts/jit-symbol-classification.toml`, both
requiring a written reason per entry:

  [sys-lane-closure.authenticated-edges]
      "caller -> callee" = "why this call is authenticated"
    Deletes ONE edge. Use it where the callee really does touch the lane but the
    caller cannot choose what crosses: the runtime derives the signal from a
    transition it won itself, or the callee is the lane's own consumer. An edge
    cut is narrow by construction -- a NEW caller of the same callee still
    trips the gate, because only the named pair is deleted. The caller itself
    must not be user-declarable: a caller user code can name in `extern "rt"`
    composes the call's arguments, so it picks the destination and the reason,
    and no reason paragraph can make that authenticated.

  [sys-lane-closure.non-roots]
      "function" = "why its lane mention is not a lane operation"
    Removes a function from the ROOT set only. It stays a normal graph node, so
    if it calls a real root it is still in the closure. Use it where the textual
    root rule over-fires (e.g. a constructor that only writes `sys_dispatch:
    None`).

Neither hatch is a blanket exemption, and both are diffs a reviewer sees.

The gate fails CLOSED on anything it cannot read. A function body that does not
brace-balance is a hard error naming the symbol and its `file:line`, never a
skip -- a skipped definition contributes no root and no edge, so its symbol
drops out of the closure and the gate reports success on a tree it never
inspected.

Usage:
    python3 scripts/sys-lane-closure.py                 # gate (exit 1 on violation)
    python3 scripts/sys-lane-closure.py --list-roots    # print the root set
    python3 scripts/sys-lane-closure.py --list-closure  # print the whole closure
    python3 scripts/sys-lane-closure.py --explain SYM   # witness path for SYM
"""

from __future__ import annotations

import argparse
import re
import sys
from collections import deque
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent / "lib"))
import toml_compat  # noqa: E402

SCAN_DIRS = [ROOT / "hew-runtime" / "src", ROOT / "hew-std" / "src"]
JIT_SYMBOL_CLASSIFICATION = ROOT / "scripts" / "jit-symbol-classification.toml"
SOURCE_ENCODING = "utf-8"

# The system-lane state set, in source terms. A function whose body names any
# of these produces, installs, mutates, observes, or destroys the private lane:
#
#   sys_queue      -- the system message queue itself (native `MpscQueue`,
#                     wasm `VecDeque`), on both mailbox implementations.
#   sys_count       -- the native queue's published depth; reading it IS an
#                     observation of lane occupancy (the `has_messages` leak).
#   sys_dispatch    -- the second dispatch entry point the lane is delivered to;
#                     installing one redirects the lane at a chosen function.
#   HewSysMsg       -- the closed system-message namespace; constructing or
#                     decoding one is minting/reading lane traffic.
#   Origin::Sys     -- the provenance tag the queue-level split returns; matching
#                     on it is reading the lane's provenance.
SYSTEM_LANE_TOKENS = (
    "sys_queue",
    "sys_count",
    "sys_dispatch",
    "HewSysMsg",
    "Origin::Sys",
)

# Names that are Rust/std/libc plumbing rather than unambiguous runtime
# functions. Exclude them ONLY as call targets so a method call like `.len()`
# cannot fuse two unrelated types through a shared method name. Their own
# bodies remain root candidates: a `drop` or `default` body that directly
# touches system-lane state is a finding, not a graph edge.
GRAPH_EDGE_NAME_DENYLIST = {
    "new",
    "default",
    "drop",
    "clone",
    "fmt",
    "len",
    "is_empty",
    "from",
    "into",
    "next",
    "eq",
    "hash",
    "cmp",
    "partial_cmp",
    "as_ref",
    "as_mut",
    "deref",
    "deref_mut",
}


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--list-roots", action="store_true", help="print the direct root set"
    )
    parser.add_argument(
        "--list-closure",
        action="store_true",
        help="print every function in the reachability closure",
    )
    parser.add_argument(
        "--explain", metavar="SYMBOL", help="print a witness path from SYMBOL to a root"
    )
    parser.add_argument(
        "--json", action="store_true", help="emit the gate result as JSON"
    )
    # The two below exist so scripts/tests/test_sys_lane_closure.py can run the
    # real gate against a synthetic tree and prove it still fails.
    parser.add_argument(
        "--scan-dir",
        type=Path,
        action="append",
        default=[],
        help="override the scanned source directories (repeatable)",
    )
    parser.add_argument(
        "--classification",
        type=Path,
        default=None,
        help="override the classification TOML",
    )
    return parser.parse_args(argv)


def _char_literal_end(source: str, i: int) -> int | None:
    """End offset of the char literal starting at `source[i] == "'"`, or None.

    Rust spells lifetimes and loop labels with the same quote it spells
    character literals with (`&'a str`, `'outer: loop`, `'{'`), so this cannot
    be a naive scan-to-the-next-quote. A `'` opens a literal only when it is
    followed by an escape (`'\\n'`, `'\\''`, `'\\u{7b}'`) or by exactly one
    character and then a closing quote. Everything else is a lifetime or a
    label and must be left alone -- blanking `'a` through to the next quote
    would erase real code and take its braces with it.
    """
    n = len(source)
    if i + 1 >= n:
        return None
    if source[i + 1] == "\\":
        # Escape: consume the selector char, then run to the closing quote.
        # No escape body can contain an unescaped `'`, so the first one ends
        # the literal (`'\''` consumes its quote as the selector).
        j = i + 3
        while j < n and source[j] != "'":
            if source[j] == "\n":
                return None
            j += 1
        return j + 1 if j < n else None
    if i + 2 < n and source[i + 2] == "'":
        return i + 3
    return None


def strip_comments_and_strings(source: str) -> str:
    """Blank out comments and string/char literals, preserving offsets.

    Root detection is textual, so a doc comment that merely NAMES `sys_queue`
    must not mint a root, and a `//` that mentions a callee must not mint an
    edge. Offsets are preserved (replacement is space-for-character) so the
    balanced-brace walks below still line up with the original source.

    Character and byte-character literals are blanked for the same reason the
    brace walk needs them gone: `let _ = '{';` is valid Rust whose brace is
    data, not syntax, and counting it desynchronises every span in the file.
    """
    out = list(source)
    i = 0
    n = len(source)
    while i < n:
        ch = source[i]
        if ch == "/" and i + 1 < n and source[i + 1] == "/":
            j = source.find("\n", i)
            j = n if j == -1 else j
            for k in range(i, j):
                out[k] = " "
            i = j
        elif ch == "/" and i + 1 < n and source[i + 1] == "*":
            depth = 1
            j = i + 2
            while j < n and depth:
                if source[j] == "/" and j + 1 < n and source[j + 1] == "*":
                    depth += 1
                    j += 2
                elif source[j] == "*" and j + 1 < n and source[j + 1] == "/":
                    depth -= 1
                    j += 2
                else:
                    j += 1
            for k in range(i, min(j, n)):
                if source[k] != "\n":
                    out[k] = " "
            i = j
        elif ch == "r" and source.startswith(("r#", 'r"'), i):
            m = re.match(r'r(#*)"', source[i:])
            if m is None:
                i += 1
                continue
            terminator = '"' + m.group(1)
            j = source.find(terminator, i + m.end())
            j = n if j == -1 else j + len(terminator)
            for k in range(i, min(j, n)):
                if source[k] != "\n":
                    out[k] = " "
            i = j
        elif ch == '"':
            j = i + 1
            while j < n:
                if source[j] == "\\":
                    j += 2
                    continue
                if source[j] == '"':
                    j += 1
                    break
                j += 1
            for k in range(i, min(j, n)):
                if source[k] != "\n":
                    out[k] = " "
            i = j
        elif ch == "'":
            end = _char_literal_end(source, i)
            if end is None:
                # A lifetime or a loop label. Not a literal; leave it be.
                i += 1
                continue
            for k in range(i, min(end, n)):
                out[k] = " "
            i = end
        else:
            i += 1
    return "".join(out)


_CFG_RE = re.compile(r"#\[cfg\(")
_TEST_MOD_RE = re.compile(
    r"#\[cfg\(\s*test\s*\)\]\s*(?:pub(?:\s*\([^)]*\))?\s+)?mod\s+(\w+)\s*;"
)


def _cfg_is_test_only(predicate: str) -> bool:
    """True when the item is compiled ONLY under `cfg(test)`.

    `cfg(test)` and `cfg(all(test, ..))` are test-only. `cfg(any(test, ..))` is
    NOT: `#[cfg(any(target_arch = "wasm32", test))]` is production wasm code,
    and treating it as a test item would drop the entire wasm actor-teardown
    path out of the graph -- exactly the kind of blind spot this tool exists to
    remove.
    """
    stripped = predicate.replace(" ", "")
    if stripped == "test":
        return True
    return stripped.startswith("all(") and re.search(r"\btest\b", stripped) is not None


def strip_cfg_test_items(source: str) -> str:
    """Blank out test-only items in-place.

    Unit tests are allowed to reach the system lane -- they are the runtime's
    own coverage of it, not part of the ABI surface -- so they must not create
    roots or graph edges.

    The attributed thing is not always a braced item. `#[cfg(test)]` also
    attaches to a `mod x;` declaration, a `use`, and -- the case that used to
    run away -- a single struct or struct-LITERAL field:

        Box::new(HewAwaitCancel {
            ...
            #[cfg(test)]
            final_free_probe: Mutex::new(None),
        })

    A field ends at a `,` (or at the enclosing `}` when it is the last one),
    never at a `{` or a `;`. Scanning for "a brace pair or a semicolon" ran
    past the field, past the enclosing struct literal's `}` and past the
    function's own `}` to the next statement terminator -- blanking live code
    and leaving the enclosing body unbalanced. Delimiter depth is tracked over
    all three bracket kinds so a `,` inside `Mutex::new(a, b)` does not end the
    field early.
    """
    out = list(source)
    n = len(source)
    for match in _CFG_RE.finditer(source):
        # Read the balanced cfg predicate.
        depth = 0
        end = match.end()
        for j in range(match.end() - 1, n):
            if source[j] == "(":
                depth += 1
            elif source[j] == ")":
                depth -= 1
                if depth == 0:
                    end = j
                    break
        predicate = source[match.end() : end]
        if not _cfg_is_test_only(predicate):
            continue
        # Step past the predicate's `)` and the attribute's own `]` so the
        # scan below does not read that `]` as the enclosing item's closer.
        i = end + 1
        while i < n and source[i].isspace():
            i += 1
        if i < n and source[i] == "]":
            i += 1
        depth = 0
        brace_started = False
        while i < n:
            ch = source[i]
            if ch in "([{":
                if ch == "{" and depth == 0:
                    brace_started = True
                depth += 1
            elif ch in ")]}":
                if depth == 0:
                    # The enclosing item's closer: the attributed thing was its
                    # last field/element and ends immediately before this.
                    break
                depth -= 1
                if depth == 0 and brace_started:
                    i += 1
                    break
            elif depth == 0 and ch in ";,":
                i += 1
                break
            i += 1
        for k in range(match.start(), min(i, n)):
            if source[k] != "\n":
                out[k] = " "
    return "".join(out)


_FN_RE = re.compile(r"\bfn\s+([A-Za-z_]\w*)")


class BodyParseError(Exception):
    """A function definition whose body could not be brace-balanced.

    This is a HARD ERROR, never a skip. A definition the parser cannot balance
    is a definition that contributes no root and no call edge, so the symbol it
    defines silently drops out of the closure and out of the gate -- a stable
    symbol could reach the system queue through a body this script declined to
    read. Failing open is the exact defect class the gate exists to remove, so
    the gate refuses to report a verdict at all until the body parses.
    """


def _body_span(
    source: str, decl_end: int, symbol: str, where: str
) -> tuple[int, int] | None:
    """Return the `{ .. }` span of the fn whose signature starts at `decl_end`.

    Returns `None` only for a genuinely bodyless declaration (a trait method or
    an `extern` block entry, terminated by `;`). Every other outcome -- an
    opening brace that never closes, or end-of-file before either a body or a
    `;` -- raises [`BodyParseError`] naming the symbol and its location.
    """
    i = decl_end
    n = len(source)
    while i < n:
        ch = source[i]
        if ch == ";":
            return None  # a trait/extern declaration, no body
        if ch == "{":
            depth = 0
            for j in range(i, n):
                if source[j] == "{":
                    depth += 1
                elif source[j] == "}":
                    depth -= 1
                    if depth == 0:
                        return (i, j + 1)
            raise BodyParseError(
                f"{where}: the body of `{symbol}` opens at this brace and never "
                f"closes (depth {depth} at end of file)"
            )
        i += 1
    raise BodyParseError(
        f"{where}: `{symbol}` has neither a `{{` body nor a `;` terminator "
        f"before the end of the file"
    )


class Graph:
    def __init__(self) -> None:
        self.bodies: dict[str, list[str]] = {}
        self.sites: dict[str, list[str]] = {}
        self.callers: dict[str, set[str]] = {}
        self.callees: dict[str, set[str]] = {}

    def defined(self) -> set[str]:
        return set(self.bodies)


def build_graph(scan_dirs: list[Path] | None = None) -> Graph:
    graph = Graph()
    raw: list[tuple[Path, str]] = []
    test_only_modules: set[str] = set()
    for scan_dir in scan_dirs if scan_dirs is not None else SCAN_DIRS:
        for rs_file in sorted(scan_dir.rglob("*.rs")):
            source = strip_comments_and_strings(
                rs_file.read_text(encoding=SOURCE_ENCODING)
            )
            raw.append((rs_file, source))
            # `#[cfg(test)] mod foo;` puts a whole FILE behind cfg(test); the
            # attribute lives in the parent module, so scanning the file itself
            # cannot see it.
            test_only_modules.update(m.group(1) for m in _TEST_MOD_RE.finditer(source))

    cleaned: list[tuple[Path, str]] = []
    for rs_file, source in raw:
        if rs_file.stem in test_only_modules:
            continue
        cleaned.append((rs_file, strip_cfg_test_items(source)))

    # Pass 1: every function definition and its body text. An unbalanced body
    # is collected rather than raised immediately so one run names every
    # offending definition instead of only the first.
    unparseable: list[str] = []
    for rs_file, source in cleaned:
        try:
            shown: Path | str = rs_file.relative_to(ROOT)
        except ValueError:
            shown = rs_file
        for match in _FN_RE.finditer(source):
            name = match.group(1)
            line = source.count("\n", 0, match.start()) + 1
            where = f"{shown}:{line}"
            try:
                span = _body_span(source, match.end(), name, where)
            except BodyParseError as exc:
                unparseable.append(str(exc))
                continue
            if span is None:
                continue  # a bodyless declaration contributes no body
            body = source[span[0] : span[1]]
            graph.bodies.setdefault(name, []).append(body)
            graph.sites.setdefault(name, []).append(where)

    if unparseable:
        raise BodyParseError(
            "the closure gate could not read these function bodies, so it "
            "cannot report a verdict:\n  " + "\n  ".join(unparseable)
        )

    defined = graph.defined() - GRAPH_EDGE_NAME_DENYLIST

    # Pass 2: call edges. A callee is any defined name appearing in call
    # position (`name(`, `name::<T>(`, `.name(`, `Type::name(`) inside a body.
    call_re = re.compile(r"([A-Za-z_]\w*)\s*(?:::\s*<[^;{}]*?>\s*)?\(")
    for name, bodies in graph.bodies.items():
        callees: set[str] = set()
        for body in bodies:
            for match in call_re.finditer(body):
                callee = match.group(1)
                if callee != name and callee in defined:
                    callees.add(callee)
        graph.callees[name] = callees
        for callee in callees:
            graph.callers.setdefault(callee, set()).add(name)
    return graph


# Constructing an EMPTY lane is not a lane operation: it produces no signal,
# installs no consumer, and observes nothing. These patterns are RULES applied
# uniformly to every body, not per-symbol waivers -- without them every actor,
# child-spec and mailbox constructor in the runtime is a root purely for
# zero-initialising a struct field.
#
# They only suppress ROOT-ness. Call edges are computed from the unmodified
# body, so a function that merely forwards a freshly built queue still keeps
# its edge to whatever actually operates on it.
_INERT_LANE_WRITES = (
    # `sys_dispatch: None` -- no system-lane consumer installed.
    re.compile(r"\bsys_dispatch\s*:\s*None\b"),
    # `sys_count: AtomicUsize::new(0)` -- empty lane.
    re.compile(r"\bsys_count\s*:\s*[\w:]*AtomicUsize::new\(\s*0\s*\)"),
    # `sys_queue: VecDeque::new()` / `MpscQueue::new()` -- empty lane.
    re.compile(r"\bsys_queue\s*:\s*[\w:<>,\s]*::new\(\s*\)"),
    # `let Some(sys_queue) = MpscQueue::new() else` -- fallible empty lane, and
    # the `sys_queue,` field-init shorthand that consumes the binding.
    re.compile(r"let\s+Some\(\s*sys_queue\s*\)\s*=\s*[\w:]*::new\(\s*\)"),
    re.compile(r"^\s*sys_queue\s*,\s*$", re.MULTILINE),
)


def find_roots(graph: Graph, non_roots: dict[str, str]) -> dict[str, list[str]]:
    """Functions whose OWN body touches the system-lane state set."""
    roots: dict[str, list[str]] = {}
    for name, bodies in graph.bodies.items():
        if name in non_roots:
            continue
        hits: set[str] = set()
        for body in bodies:
            for pattern in _INERT_LANE_WRITES:
                body = pattern.sub(" ", body)
            hits.update(t for t in SYSTEM_LANE_TOKENS if t in body)
        if hits:
            roots[name] = sorted(hits)
    return roots


class Waivers:
    def __init__(
        self, edges: dict[tuple[str, str], str], non_roots: dict[str, str]
    ) -> None:
        self.edges = edges
        self.non_roots = non_roots


_EDGE_RE = re.compile(r"^\s*([A-Za-z_]\w*)\s*->\s*([A-Za-z_]\w*)\s*$")


def load_classification(
    path: Path | None = None,
) -> tuple[dict[str, set[str]], Waivers]:
    text = (path or JIT_SYMBOL_CLASSIFICATION).read_text(encoding=SOURCE_ENCODING)
    document = toml_compat.loads(text)
    tiers = {
        key: set(document.get(key, []))
        for key in ("stable", "stable-stdlib", "codegen-stable", "internal")
    }
    table = document.get("sys-lane-closure", {})
    edges: dict[tuple[str, str], str] = {}
    for key, reason in table.get("authenticated-edges", {}).items():
        match = _EDGE_RE.match(str(key))
        if match is None:
            raise ValueError(
                f"{JIT_SYMBOL_CLASSIFICATION}: authenticated-edges key "
                f'{key!r} is not of the form "caller -> callee"'
            )
        if not str(reason).strip():
            raise ValueError(
                f"{JIT_SYMBOL_CLASSIFICATION}: authenticated edge {key!r} has no reason"
            )
        edges[(match.group(1), match.group(2))] = str(reason)
    non_roots: dict[str, str] = {}
    for key, reason in table.get("non-roots", {}).items():
        if not str(reason).strip():
            raise ValueError(
                f"{JIT_SYMBOL_CLASSIFICATION}: non-root {key!r} has no reason"
            )
        non_roots[str(key)] = str(reason)
    return tiers, Waivers(edges, non_roots)


def compute_closure(
    graph: Graph, roots: dict[str, list[str]], waivers: Waivers
) -> tuple[set[str], dict[str, str]]:
    """Reverse-reachability from the roots, minus the authenticated edges.

    Returns the closure and, for each member, the callee through which it
    reaches the lane (a one-step witness; `--explain` chains them into a path).
    """
    closure: set[str] = set()
    via: dict[str, str] = {}
    queue: deque[str] = deque()
    for root in roots:
        closure.add(root)
        queue.append(root)
    while queue:
        current = queue.popleft()
        for caller in graph.callers.get(current, ()):
            if caller in closure or (caller, current) in waivers.edges:
                continue
            closure.add(caller)
            via[caller] = current
            queue.append(caller)
    return closure, via


def witness_path(
    symbol: str, closure: set[str], via: dict[str, str], roots: dict[str, list[str]]
) -> list[str]:
    path = [symbol]
    seen = {symbol}
    current = symbol
    while current not in roots:
        nxt = via.get(current)
        if nxt is None or nxt in seen:
            break
        path.append(nxt)
        seen.add(nxt)
        current = nxt
    return path


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        graph = build_graph(args.scan_dir or None)
    except BodyParseError as exc:
        print(f"sys-lane closure: {exc}", file=sys.stderr)
        print(
            "A body this parser cannot balance contributes no root and no call "
            "edge, so its symbol drops out of the closure without failing the "
            "gate. That is failing open, so the gate refuses to run instead.",
            file=sys.stderr,
        )
        return 1
    tiers, waivers = load_classification(args.classification)
    roots = find_roots(graph, waivers.non_roots)
    closure, via = compute_closure(graph, roots, waivers)

    if args.list_roots:
        for name in sorted(roots):
            site = graph.sites[name][0]
            print(f"{name}\t{','.join(roots[name])}\t{site}")
        return 0

    if args.list_closure:
        for name in sorted(closure):
            print(name)
        return 0

    if args.explain:
        symbol = args.explain
        if symbol not in closure:
            print(f"{symbol}: not in the system-lane closure")
            return 0
        path = witness_path(symbol, closure, via, roots)
        print(" -> ".join(path))
        tail = path[-1]
        if tail in roots:
            print(
                f"    {tail} touches {', '.join(roots[tail])} at {graph.sites[tail][0]}"
            )
        return 0

    user_declarable = tiers["stable"] | tiers["stable-stdlib"]
    violations = sorted(user_declarable & closure)

    defined = graph.defined()
    stale: list[str] = []
    for caller, callee in sorted(waivers.edges):
        if callee not in graph.callees.get(caller, ()):
            stale.append(f"authenticated edge {caller} -> {callee} is not a call")
    # An authenticated edge says "this caller cannot choose what crosses into
    # the system queue". A user-declarable caller composes the call's arguments
    # itself, so the claim cannot hold for it: the destination and the reason
    # are whatever the user program passed in. Refuse the pairing outright
    # rather than let a reason paragraph assert otherwise.
    unauthenticated: list[str] = [
        f"authenticated edge {caller} -> {callee} has a user-declarable caller"
        for caller, callee in sorted(waivers.edges)
        if caller in user_declarable
    ]
    # A non-root whose body no longer names the lane is waiving nothing, and a
    # non-root that is no longer defined is waiving a symbol that does not
    # exist. Both hide the next edit behind an entry nobody will re-read.
    would_be_roots = find_roots(graph, {})
    for name in sorted(waivers.non_roots):
        if name not in defined:
            stale.append(f"non-root {name} is not a defined function")
        elif name not in would_be_roots:
            stale.append(f"non-root {name} no longer names system-lane state")

    if args.json:
        import json

        print(
            json.dumps(
                {
                    "roots": len(roots),
                    "closure": len(closure),
                    "violations": violations,
                    "stale_waivers": stale,
                    "unauthenticated_edges": unauthenticated,
                },
                indent=2,
            )
        )

    failed = False
    if unauthenticated:
        failed = True
        print(
            "sys-lane closure: these authenticated edges are waived from a\n"
            "user-declarable caller. An authenticated edge means the runtime, not\n"
            "the caller, decides what crosses into the system queue -- but a\n"
            'caller user code can name in `extern "rt"` composes the call\'s\n'
            "arguments itself, so it chooses the destination and the reason. Move\n"
            "the caller out of the user-declarable tier or close the edge:",
            file=sys.stderr,
        )
        for entry in unauthenticated:
            print(f"  {entry}", file=sys.stderr)

    if stale:
        failed = True
        print(
            "sys-lane closure: stale waivers. A waiver that no longer matches the\n"
            "code silently widens the stable tier, so it fails the gate instead of\n"
            "sitting there:",
            file=sys.stderr,
        )
        for entry in stale:
            print(f"  {entry}", file=sys.stderr)

    if violations:
        failed = True
        print(
            "sys-lane closure: these `stable` symbols reach system-lane state.\n"
            'A stable symbol is one user `extern "rt"` code may name directly, and\n'
            "`docs/internal/jit-host-abi.md` forbids any of them from producing,\n"
            "installing, mutating, observing, or destroying the private lane --\n"
            "including through a call. Reclassify, split the capability, or record\n"
            "the authenticated edge in [sys-lane-closure.authenticated-edges].",
            file=sys.stderr,
        )
        for name in violations:
            path = witness_path(name, closure, via, roots)
            tail = path[-1]
            where = graph.sites.get(tail, ["?"])[0]
            print(
                f"  {name}: {' -> '.join(path)}  [{','.join(roots.get(tail, []))} at {where}]",
                file=sys.stderr,
            )

    if failed:
        return 1

    if not args.json:
        print(
            f"sys-lane closure: {len(roots)} roots, {len(closure)} reaching functions, "
            f"{len(waivers.edges)} authenticated edges, {len(waivers.non_roots)} "
            f"non-roots, 0 stable violations"
        )
    return 0


if __name__ == "__main__":
    sys.exit(main())
