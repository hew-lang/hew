#!/usr/bin/env python3
"""
hew-system-explorer — Generate an interactive HTML visualization of a Hew actor system.

Usage:
    hew build --emit-mlir program.hew | python scripts/viz/system-explorer.py -o system.html
    python scripts/viz/system-explorer.py input.mlir -o explorer.html

The tool reads Hew dialect MLIR from a file or stdin, parses actor/function/message
structure, and produces a self-contained HTML file with a D3.js force-directed graph.
"""

from __future__ import annotations

import argparse
import html
import json
import re
import sys
from dataclasses import dataclass, field


# ── Data model ───────────────────────────────────────────────────────────────


@dataclass
class FuncInfo:
    name: str
    args: str = ""
    result: str = ""
    ops: list[str] = field(default_factory=list)
    is_dispatch: bool = False
    is_private: bool = False
    body_lines: list[str] = field(default_factory=list)


@dataclass
class ActorInfo:
    name: str
    state_type: str = ""
    handlers: list[str] = field(default_factory=list)


@dataclass
class SpawnEdge:
    source_func: str
    actor_name: str
    ssa_var: str = ""


@dataclass
class SendEdge:
    source_func: str
    target_ssa: str = ""
    msg_type: int = -1


@dataclass
class AskEdge:
    source_func: str
    target_ssa: str = ""
    msg_type: int = -1
    result_type: str = ""


# ── Per-function CFG extraction (from hew-fn-cfg.py) ─────────────────────────
#
# The following section extracts intra-function control flow graphs so the
# detail-view can show basic blocks, conditionals, and loops inside each
# function.  The logic mirrors hew-fn-cfg.py's classify_op / _CfgBuilder /
# _build_cfg / _process_block / helpers.

from enum import Enum, auto

NOISE_OPS = frozenset([
    "arith.constant", "arith.addi", "arith.subi", "arith.muli", "arith.divsi",
    "arith.remsi", "arith.extsi", "arith.trunci", "arith.andi", "arith.ori",
    "arith.xori", "arith.addf", "arith.subf", "arith.mulf", "arith.divf",
    "arith.negf", "arith.sitofp", "arith.fptosi",
    "llvm.load", "llvm.store", "llvm.getelementptr", "llvm.insertvalue",
    "llvm.extractvalue", "llvm.alloca", "llvm.bitcast",
    "memref.alloca", "memref.store", "memref.load",
    "hew.constant", "hew.bitcast", "hew.drop", "hew.to_string",
    "hew.sched.init", "hew.sched.shutdown",
    "scf.yield", "scf.condition",
])


def _cfg_classify_op(line: str) -> str | None:
    """Return a human-readable label for an MLIR op line, or None to skip."""
    stripped = line.strip()
    if stripped.startswith(("scf.if", "scf.while", "scf.for", "} else {")):
        return None
    op_match = re.match(r'(?:%\S+\s*=\s*)?([a-zA-Z_][a-zA-Z0-9_.]*)', stripped)
    if not op_match:
        return None
    op = op_match.group(1)
    if op in NOISE_OPS:
        return None
    if op == "hew.actor_spawn":
        m = re.search(r'actor_name\s*=\s*"(\w+)"', stripped)
        return f"spawn {m.group(1)}" if m else "spawn ?"
    if op == "hew.actor_send":
        return "send"
    if op == "hew.actor_ask":
        return "ask"
    if op in ("hew.actor_stop", "hew.actor_close"):
        return op.split(".")[-1]
    if op == "hew.sleep":
        return "sleep"
    if op.startswith("hew.vec."):
        return f"vec.{op.split('.')[-1]}"
    if op.startswith("hew.hashmap."):
        return f"hashmap.{op.split('.')[-1]}"
    if op == "hew.print":
        return "print"
    if op == "hew.string_concat":
        return "str_concat"
    if op == "hew.string_method":
        return "str_method"
    if op == "hew.receive":
        m = re.search(r'handlers\s*=\s*\[([^\]]*)\]', stripped)
        if m:
            handlers = [h.strip().lstrip("@") for h in m.group(1).split(",")]
            return "receive [" + ", ".join(handlers) + "]"
        return "receive"
    if op == "hew.runtime_call":
        m = re.search(r'@(\w+)', stripped)
        return f"runtime: {m.group(1)}" if m else "runtime: ?"
    if op == "hew.struct_init":
        return "struct_init"
    if op in ("hew.field_get", "hew.field_set"):
        return op.split(".")[-1]
    if op == "hew.cast":
        return "cast"
    if op in ("func.call", "call"):
        m = re.search(r'@(\w+)', stripped)
        if m:
            name = m.group(1)
            if name.startswith("hew_"):
                return None
            return f"call {name}()"
        return "call ?()"
    if op == "arith.cmpi":
        return "compare"
    if op == "arith.cmpf":
        return "compare_f"
    if op == "return":
        return "return"
    if op.startswith("hew."):
        return op
    return None


class _CfgBlockKind(Enum):
    ENTRY = auto()
    EXIT = auto()
    LOOP_HEADER = auto()
    CONDITIONAL = auto()
    NORMAL = auto()


@dataclass
class _CfgBlock:
    id: str
    label: str
    kind: _CfgBlockKind = _CfgBlockKind.NORMAL
    ops: list[str] = field(default_factory=list)
    truncated: int = 0
    has_return: bool = False


@dataclass
class _CfgEdge:
    src: str
    dst: str
    label: str = ""
    color: str = "black"
    style: str = "solid"


_CFG_MAX_OPS = 8


class _CfgBuilder:
    def __init__(self) -> None:
        self.blocks: list[_CfgBlock] = []
        self.edges: list[_CfgEdge] = []
        self._counter = 0

    def _new_id(self) -> str:
        self._counter += 1
        return f"bb{self._counter}"

    def add_block(self, label: str, kind: _CfgBlockKind = _CfgBlockKind.NORMAL) -> _CfgBlock:
        b = _CfgBlock(id=self._new_id(), label=label, kind=kind)
        self.blocks.append(b)
        return b

    def add_edge(self, src: str, dst: str, label: str = "",
                 color: str = "black", style: str = "solid") -> None:
        self.edges.append(_CfgEdge(src=src, dst=dst, label=label, color=color, style=style))

    def add_op(self, block: _CfgBlock, op: str) -> None:
        if op == "return":
            block.has_return = True
        if len(block.ops) < _CFG_MAX_OPS:
            block.ops.append(op)
        else:
            block.truncated += 1


def _cfg_parse_braced(lines: list[str], start: int) -> tuple[list[str], int]:
    i = start
    depth = 0
    found_open = False
    while i < len(lines):
        for ch in lines[i]:
            if ch == "{":
                depth += 1
                found_open = True
            elif ch == "}":
                depth -= 1
        if found_open and depth == 0:
            return lines[start + 1 : i], i + 1
        i += 1
    return lines[start + 1 :], len(lines)


def _cfg_parse_while(lines: list[str], start: int) -> tuple[list[str], list[str], int]:
    i = start
    depth = 0
    for ch in lines[i]:
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
    cond_start = i + 1
    do_boundary = None
    i += 1
    while i < len(lines):
        stripped = lines[i].strip()
        if "} do {" in stripped and depth == 1:
            do_boundary = i
            depth = 1
            break
        for ch in lines[i]:
            if ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
        i += 1
    if do_boundary is None:
        return [], [], start + 1
    cond_lines = lines[cond_start:do_boundary]
    body_start = do_boundary + 1
    j = body_start
    while j < len(lines):
        for ch in lines[j]:
            if ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
        if depth == 0:
            return cond_lines, lines[body_start:j], j + 1
        j += 1
    return cond_lines, lines[body_start:], len(lines)


def _cfg_parse_if_else(lines: list[str], start: int) -> tuple[list[str], list[str] | None, int]:
    i = start
    depth = 0
    true_start = None
    true_end = None
    for ch in lines[i]:
        if ch == "{":
            depth += 1
            if depth == 1:
                true_start = i + 1

    i += 1
    while i < len(lines):
        for ch in lines[i]:
            if ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
                if depth == 0:
                    true_end = i
                    break
        if true_end is not None:
            break
        i += 1

    true_lines = lines[true_start:true_end] if true_start and true_end else []
    false_lines: list[str] | None = None
    end_i = (true_end + 1) if true_end else len(lines)

    if true_end is not None:
        remaining = lines[true_end].strip()
        if "else" in remaining:
            depth = 0
            for ch in remaining:
                if ch == "{":
                    depth += 1
                elif ch == "}":
                    depth -= 1
            if depth > 0:
                false_start = true_end + 1
                j = false_start
                while j < len(lines):
                    for ch in lines[j]:
                        if ch == "{":
                            depth += 1
                        elif ch == "}":
                            depth -= 1
                    if depth == 0:
                        false_lines = lines[false_start:j]
                        end_i = j + 1
                        break
                    j += 1
        elif true_end + 1 < len(lines) and "else" in lines[true_end + 1].strip():
            el_line = true_end + 1
            depth = 0
            for ch in lines[el_line]:
                if ch == "{":
                    depth += 1
                elif ch == "}":
                    depth -= 1
            if depth > 0:
                false_start = el_line + 1
                j = false_start
                while j < len(lines):
                    for ch in lines[j]:
                        if ch == "{":
                            depth += 1
                        elif ch == "}":
                            depth -= 1
                    if depth == 0:
                        false_lines = lines[false_start:j]
                        end_i = j + 1
                        break
                    j += 1
    return true_lines, false_lines, end_i


def _cfg_process_block(builder: _CfgBuilder, lines: list[str], current: _CfgBlock) -> _CfgBlock | None:
    i = 0
    while i < len(lines):
        stripped = lines[i].strip()
        if not stripped or stripped in ("}", "{"):
            i += 1
            continue

        if stripped.startswith("scf.while"):
            cond_lines, do_lines, end_i = _cfg_parse_while(lines, i)
            loop_hdr = builder.add_block("while", _CfgBlockKind.LOOP_HEADER)
            builder.add_edge(current.id, loop_hdr.id)
            for cl in cond_lines:
                op = _cfg_classify_op(cl)
                if op:
                    builder.add_op(loop_hdr, op)
            body_block = builder.add_block("loop body", _CfgBlockKind.NORMAL)
            builder.add_edge(loop_hdr.id, body_block.id, label="true", color="#2E7D32")
            body_exit = _cfg_process_block(builder, do_lines, body_block)
            if body_exit:
                builder.add_edge(body_exit.id, loop_hdr.id, label="loop", color="#1565C0", style="dashed")
            after = builder.add_block("after loop", _CfgBlockKind.NORMAL)
            builder.add_edge(loop_hdr.id, after.id, label="false", color="#C62828")
            current = after
            i = end_i
            continue

        if stripped.startswith("scf.for"):
            m = re.match(r'scf\.for\s+(%\w+)\s*=\s*(%\w+)\s+to\s+(%\w+)', stripped)
            lbl = f"for {m.group(1)}" if m else "for"
            for_body, end_i = _cfg_parse_braced(lines, i)
            loop_hdr = builder.add_block(lbl, _CfgBlockKind.LOOP_HEADER)
            builder.add_edge(current.id, loop_hdr.id)
            body_block = builder.add_block("for body", _CfgBlockKind.NORMAL)
            builder.add_edge(loop_hdr.id, body_block.id, label="true", color="#2E7D32")
            body_exit = _cfg_process_block(builder, for_body, body_block)
            if body_exit:
                builder.add_edge(body_exit.id, loop_hdr.id, label="loop", color="#1565C0", style="dashed")
            after = builder.add_block("after for", _CfgBlockKind.NORMAL)
            builder.add_edge(loop_hdr.id, after.id, label="done", color="#C62828")
            current = after
            i = end_i
            continue

        if stripped.startswith("scf.if") or re.match(r'%\S+\s*=\s*scf\.if', stripped):
            true_lines, false_lines, end_i = _cfg_parse_if_else(lines, i)
            cond_block = builder.add_block("if", _CfgBlockKind.CONDITIONAL)
            builder.add_edge(current.id, cond_block.id)
            true_block = builder.add_block("then", _CfgBlockKind.NORMAL)
            builder.add_edge(cond_block.id, true_block.id, label="true", color="#2E7D32")
            true_exit = _cfg_process_block(builder, true_lines, true_block)
            after = builder.add_block("after if", _CfgBlockKind.NORMAL)
            if true_exit:
                builder.add_edge(true_exit.id, after.id)
            if false_lines is not None:
                false_block = builder.add_block("else", _CfgBlockKind.NORMAL)
                builder.add_edge(cond_block.id, false_block.id, label="false", color="#C62828")
                false_exit = _cfg_process_block(builder, false_lines, false_block)
                if false_exit:
                    builder.add_edge(false_exit.id, after.id)
            else:
                builder.add_edge(cond_block.id, after.id, label="false", color="#C62828")
            current = after
            i = end_i
            continue

        op = _cfg_classify_op(stripped)
        if op:
            builder.add_op(current, op)
        i += 1

    return current


def _cfg_merge_true_false(edges: list[_CfgEdge]) -> tuple[list[_CfgEdge], bool]:
    from collections import defaultdict
    pair_labels: dict[tuple[str, str], list[int]] = defaultdict(list)
    for idx, e in enumerate(edges):
        pair_labels[(e.src, e.dst)].append(idx)
    to_remove: set[int] = set()
    replacements: list[_CfgEdge] = []
    for (src, dst), indices in pair_labels.items():
        if len(indices) < 2:
            continue
        labels = {edges[i].label for i in indices}
        if "true" in labels and "false" in labels:
            to_remove.update(indices)
            replacements.append(_CfgEdge(src=src, dst=dst))
        elif "" in labels:
            for i in indices:
                if edges[i].label:
                    to_remove.add(i)
    if not to_remove:
        return edges, False
    new_edges = [e for i, e in enumerate(edges) if i not in to_remove]
    new_edges.extend(replacements)
    return new_edges, True


def _cfg_simplify(blocks: list[_CfgBlock], edges: list[_CfgEdge]) -> tuple[list[_CfgBlock], list[_CfgEdge]]:
    changed = True
    while changed:
        changed = False
        edges, merged = _cfg_merge_true_false(edges)
        if merged:
            changed = True
        for b in list(blocks):
            if b.ops or b.truncated or b.kind in (_CfgBlockKind.ENTRY, _CfgBlockKind.EXIT):
                continue
            outgoing = [e for e in edges if e.src == b.id]
            incoming = [e for e in edges if e.dst == b.id]
            if len(outgoing) == 1 and not outgoing[0].label:
                target = outgoing[0].dst
                new_edges = []
                for e in edges:
                    if e.dst == b.id:
                        new_edges.append(_CfgEdge(src=e.src, dst=target, label=e.label, color=e.color, style=e.style))
                    elif e.src == b.id:
                        pass
                    else:
                        new_edges.append(e)
                edges = new_edges
                blocks = [bb for bb in blocks if bb.id != b.id]
                changed = True
                break
            if b.kind == _CfgBlockKind.CONDITIONAL and len(incoming) == 1:
                true_edges = [e for e in outgoing if e.label == "true"]
                false_edges = [e for e in outgoing if e.label == "false"]
                if len(true_edges) == 1 and len(false_edges) == 1:
                    pred = incoming[0].src
                    new_edges = [e for e in edges if e.src != b.id and e.dst != b.id]
                    new_edges.append(_CfgEdge(src=pred, dst=true_edges[0].dst,
                                              label="true", color=true_edges[0].color, style=true_edges[0].style))
                    new_edges.append(_CfgEdge(src=pred, dst=false_edges[0].dst,
                                              label="false", color=false_edges[0].color, style=false_edges[0].style))
                    edges = new_edges
                    blocks = [bb for bb in blocks if bb.id != b.id]
                    changed = True
                    break
    seen: set[tuple[str, str, str]] = set()
    deduped: list[_CfgEdge] = []
    for e in edges:
        key = (e.src, e.dst, e.label)
        if key not in seen:
            seen.add(key)
            deduped.append(e)
    return blocks, deduped


_BLOCK_KIND_MAP = {
    _CfgBlockKind.ENTRY: "entry",
    _CfgBlockKind.EXIT: "exit",
    _CfgBlockKind.LOOP_HEADER: "loop",
    _CfgBlockKind.CONDITIONAL: "conditional",
    _CfgBlockKind.NORMAL: "normal",
}


def build_internal_cfg(body_lines: list[str]) -> dict | None:
    """Build a JSON-serialisable internal CFG from a function's MLIR body lines."""
    if not body_lines:
        return None
    builder = _CfgBuilder()
    entry = builder.add_block("entry", _CfgBlockKind.ENTRY)
    _cfg_process_block(builder, body_lines, entry)
    if len(builder.blocks) > 1:
        for b in builder.blocks:
            if b.has_return:
                b.kind = _CfgBlockKind.EXIT
    blocks, edges = _cfg_simplify(list(builder.blocks), list(builder.edges))
    json_blocks = []
    for b in blocks:
        ops = list(b.ops)
        if b.truncated:
            ops.append(f"... +{b.truncated} more")
        json_blocks.append({
            "id": b.id,
            "label": b.label,
            "kind": _BLOCK_KIND_MAP.get(b.kind, "normal"),
            "ops": ops,
        })
    json_edges = []
    for e in edges:
        edge_d: dict = {"src": e.src, "dst": e.dst}
        if e.label:
            edge_d["label"] = e.label
        if e.color != "black":
            edge_d["color"] = e.color
        if e.style != "solid":
            edge_d["style"] = e.style
        json_edges.append(edge_d)
    if not json_blocks:
        return None
    return {"blocks": json_blocks, "edges": json_edges}


# ── Parser (reused from hew-ir-viz.py) ───────────────────────────────────────

RE_FUNC = re.compile(
    r"^\s*func\.func\s+(?P<private>private\s+)?@(?P<name>\S+)"
    r"\((?P<args>[^)]*)\)"
    r"(?:\s*->\s*(?P<ret>\S+))?\s*\{"
)
RE_GLOBAL_STRING = re.compile(
    r'^\s*hew\.global_string\s+@(?P<sym>\w+)\s*=\s*"(?P<val>[^"]*)"'
)
RE_ACTOR_SPAWN = re.compile(
    r"(?P<ssa>%\S+)\s*=\s*hew\.actor_spawn\s+@(?P<dispatch>\S+)\((?P<init>[^)]*)\)"
    r'\s*\{[^}]*actor_name\s*=\s*"(?P<actor>[^"]+)"'
    r'[^}]*state_type\s*=\s*(?P<state>![^}]+)\}'
)
RE_ACTOR_SEND = re.compile(
    r"hew\.actor_send\s+(?P<target>%\S+)\s*\{msg_type\s*=\s*(?P<msg>\d+)"
)
RE_ACTOR_ASK = re.compile(
    r"(?P<ssa>%\S+)\s*=\s*hew\.actor_ask\s+(?P<target>%\S+)\s*\{msg_type\s*=\s*(?P<msg>\d+)"
    r"[^}]*\}.*->\s*(?P<ret>\S+)"
)
RE_ACTOR_STOP = re.compile(r"hew\.actor_stop\s+(?P<target>%\S+)")
RE_ACTOR_CLOSE = re.compile(r"hew\.actor_close\s+(?P<target>%\S+)")
RE_VEC_OP = re.compile(r"hew\.vec\.(?P<op>\w+)")
RE_HASHMAP_OP = re.compile(r"hew\.hashmap\.(?P<op>\w+)")
RE_VEC_NEW = re.compile(r"(?P<ssa>%\S+)\s*=\s*hew\.vec\.new.*:\s*(?P<ty>!hew\.vec<[^>]+>)")
RE_HASHMAP_NEW = re.compile(
    r"(?P<ssa>%\S+)\s*=\s*hew\.hashmap\.new.*:\s*(?P<ty>!hew\.hashmap<[^>]+>)"
)
RE_RUNTIME_CALL = re.compile(r'hew\.runtime_call\s+@(?P<fn>\w+)')
RE_RECEIVE = re.compile(r"hew\.receive")
RE_FUNC_CALL = re.compile(r"(?:func\.)?call\s+@(?P<fn>\w+)")
RE_CLOSE_BRACE = re.compile(r"^\s*\}")

RE_LLVM_STRUCT = re.compile(r'!llvm\.struct<"[^"]*",\s*\(([^)]*)\)>')


def simplify_mlir_type(raw: str) -> str:
    """Simplify a single MLIR type token to a human-readable form."""
    s = raw.strip()
    s = s.replace("!llvm.ptr", "ptr")
    s = s.replace("!hew.string_ref", "string")
    s = re.sub(r'!hew\.typed_actor_ref<"(\w+)">', r"ActorRef<\1>", s)
    s = re.sub(r"!hew\.vec<(.+?)>", r"Vec<\1>", s)
    s = re.sub(r"!hew\.hashmap<(.+?)>", r"HashMap<\1>", s)
    return s


def format_state_type(raw: str) -> str:
    """Extract and simplify the tuple part from an MLIR struct state type."""
    m = RE_LLVM_STRUCT.search(raw)
    if not m:
        return simplify_mlir_type(raw)
    inner = m.group(1)
    parts = [simplify_mlir_type(p) for p in inner.split(",")]
    return "(" + ", ".join(parts) + ")"


def strip_ssa_params(params_str: str) -> str:
    """Strip SSA names (%argN:) from parameter strings and simplify types."""
    if not params_str.strip():
        return ""
    parts = []
    for p in params_str.split(","):
        p = p.strip()
        if ":" in p:
            # "%arg1: i64" → "i64"
            p = p.split(":", 1)[1].strip()
        parts.append(simplify_mlir_type(p))
    return ", ".join(parts)


def parse_mlir(text: str):
    """Parse MLIR text and extract structural information."""
    funcs: dict[str, FuncInfo] = {}
    actors: dict[str, ActorInfo] = {}
    spawns: list[SpawnEdge] = []
    sends: list[SendEdge] = []
    asks: list[AskEdge] = []
    global_strings: dict[str, str] = {}
    ssa_to_actor: dict[str, str] = {}

    current_func: FuncInfo | None = None
    brace_depth = 0
    func_depth = 0  # brace depth when we entered the current function

    for line in text.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("//"):
            continue

        brace_depth += line.count("{") - line.count("}")

        # Global strings
        m = RE_GLOBAL_STRING.match(stripped)
        if m:
            val = m.group("val")
            if len(val) > 40:
                val = val[:37] + "..."
            global_strings[m.group("sym")] = val
            continue

        # Function start
        m = RE_FUNC.match(stripped)
        if m:
            fi = FuncInfo(
                name=m.group("name"),
                args=m.group("args").strip(),
                result=m.group("ret") or "",
                is_private=bool(m.group("private")),
            )
            if fi.name.endswith("_dispatch"):
                fi.is_dispatch = True
                actor_name = fi.name.replace("_dispatch", "")
                if actor_name not in actors:
                    actors[actor_name] = ActorInfo(name=actor_name)
            funcs[fi.name] = fi
            current_func = fi
            func_depth = brace_depth
            continue

        if current_func is not None and brace_depth < func_depth:
            current_func = None
            continue

        if current_func is None:
            continue

        # Capture body lines for CFG extraction
        current_func.body_lines.append(line)

        fn_name = current_func.name

        # Actor spawn
        m = RE_ACTOR_SPAWN.search(stripped)
        if m:
            actor_name = m.group("actor")
            ssa = m.group("ssa")
            state = m.group("state")
            ssa_to_actor[ssa] = actor_name
            if actor_name not in actors:
                actors[actor_name] = ActorInfo(name=actor_name, state_type=state)
            else:
                actors[actor_name].state_type = state
            spawns.append(SpawnEdge(source_func=fn_name, actor_name=actor_name, ssa_var=ssa))
            current_func.ops.append(f"spawn {actor_name}")
            continue

        # Actor send
        m = RE_ACTOR_SEND.search(stripped)
        if m:
            target_actor = ssa_to_actor.get(m.group("target"), m.group("target"))
            msg = int(m.group("msg"))
            sends.append(SendEdge(source_func=fn_name, target_ssa=target_actor, msg_type=msg))
            current_func.ops.append(f"send msg#{msg}")
            continue

        # Actor ask
        m = RE_ACTOR_ASK.search(stripped)
        if m:
            target_actor = ssa_to_actor.get(m.group("target"), m.group("target"))
            msg = int(m.group("msg"))
            ret = m.group("ret") if m.group("ret") else ""
            asks.append(AskEdge(source_func=fn_name, target_ssa=target_actor, msg_type=msg, result_type=ret))
            current_func.ops.append(f"ask msg#{msg}")
            continue

        # Actor stop/close
        if RE_ACTOR_STOP.search(stripped):
            current_func.ops.append("actor_stop")
            continue
        if RE_ACTOR_CLOSE.search(stripped):
            current_func.ops.append("actor_close")
            continue

        # Collections
        m = RE_VEC_NEW.search(stripped)
        if m:
            current_func.ops.append(f"vec.new {m.group('ty')}")
            continue
        m = RE_HASHMAP_NEW.search(stripped)
        if m:
            current_func.ops.append(f"hashmap.new {m.group('ty')}")
            continue
        m = RE_VEC_OP.search(stripped)
        if m and m.group("op") != "new":
            op = m.group("op")
            if f"vec.{op}" not in current_func.ops:
                current_func.ops.append(f"vec.{op}")
            continue
        m = RE_HASHMAP_OP.search(stripped)
        if m and m.group("op") != "new":
            op = m.group("op")
            if f"hashmap.{op}" not in current_func.ops:
                current_func.ops.append(f"hashmap.{op}")
            continue

        # Runtime calls
        m = RE_RUNTIME_CALL.search(stripped)
        if m:
            rt_fn = m.group("fn")
            label = f"rt:{rt_fn}"
            if label not in current_func.ops:
                current_func.ops.append(label)
            continue

        # Function calls
        m = RE_FUNC_CALL.search(stripped)
        if m:
            callee = m.group("fn")
            if not callee.startswith("hew_") and callee != fn_name:
                label = f"call @{callee}"
                if label not in current_func.ops:
                    current_func.ops.append(label)

    # Infer actor handlers
    for actor_name, actor in actors.items():
        prefix = actor_name + "_"
        for fname in funcs:
            if fname.startswith(prefix) and fname != f"{actor_name}_dispatch":
                handler = fname[len(prefix):]
                actor.handlers.append(handler)

    # Detect supervisors: actors with "supervisor" in their name
    supervisor_names: set[str] = set()
    for actor_name in actors:
        if "supervisor" in actor_name.lower():
            supervisor_names.add(actor_name)

    return funcs, actors, spawns, sends, asks, global_strings, supervisor_names


# ── JSON data builder ────────────────────────────────────────────────────────


def build_graph_data(funcs, actors, spawns, sends, asks, global_strings, title, supervisor_names=None):
    """Build the JSON data structure for the D3.js visualization."""
    nodes = []
    edges = []
    handler_info: dict[str, dict] = {}
    if supervisor_names is None:
        supervisor_names = set()

    # Actor nodes
    for actor_name, actor in actors.items():
        handler_details = []
        for h in actor.handlers:
            full_name = f"{actor_name}_{h}"
            fi = funcs.get(full_name)
            params = ""
            ret = ""
            cfg = None
            if fi:
                param_parts = [p.strip() for p in fi.args.split(",")]
                if param_parts and param_parts[0].startswith("%arg0"):
                    param_parts = param_parts[1:]
                params = strip_ssa_params(", ".join(param_parts))
                ret = simplify_mlir_type(fi.result) if fi.result else ""
                cfg = build_internal_cfg(fi.body_lines)
            hd: dict = {
                "name": h,
                "fullName": full_name,
                "params": params,
                "returnType": ret,
            }
            if cfg:
                hd["internalCFG"] = cfg
            handler_details.append(hd)
            handler_info[full_name] = {
                "actor": actor_name,
                "handler": h,
                "params": params,
                "returnType": ret,
            }

        node_type = "supervisor" if actor_name in supervisor_names else "actor"
        node_data = {
            "id": f"actor:{actor_name}",
            "type": node_type,
            "label": actor_name,
            "stateType": format_state_type(actor.state_type),
            "handlers": handler_details,
            "handlerCount": len(actor.handlers),
        }
        nodes.append(node_data)

    # Function nodes (skip dispatch, private, and handler functions)
    actor_handler_funcs = set()
    for actor_name in actors:
        actor_handler_funcs.add(f"{actor_name}_dispatch")
        for h in actors[actor_name].handlers:
            actor_handler_funcs.add(f"{actor_name}_{h}")

    for fname, finfo in funcs.items():
        if finfo.is_dispatch or finfo.is_private or fname in actor_handler_funcs:
            continue
        node: dict = {
            "id": f"func:{fname}",
            "type": "function",
            "label": fname,
            "args": finfo.args,
            "returnType": finfo.result,
            "ops": finfo.ops,
            "isMain": fname == "main",
        }
        cfg = build_internal_cfg(finfo.body_lines)
        if cfg:
            node["internalCFG"] = cfg
        nodes.append(node)

    # Spawn edges
    for sp in spawns:
        src_id = f"func:{sp.source_func}"
        if sp.source_func in actor_handler_funcs:
            for aname, ainfo in actors.items():
                if sp.source_func.startswith(aname + "_"):
                    src_id = f"actor:{aname}"
                    break
        edges.append({
            "source": src_id,
            "target": f"actor:{sp.actor_name}",
            "type": "spawn",
            "label": "spawn",
        })

    # Send edges
    for se in sends:
        src_id = f"func:{se.source_func}"
        if se.source_func in actor_handler_funcs:
            for aname in actors:
                if se.source_func.startswith(aname + "_"):
                    src_id = f"actor:{aname}"
                    break
        target_actor = se.target_ssa
        handler_label = f"msg#{se.msg_type}"
        if target_actor in actors and se.msg_type < len(actors[target_actor].handlers):
            handler_label = actors[target_actor].handlers[se.msg_type]
        if target_actor in actors:
            edges.append({
                "source": src_id,
                "target": f"actor:{target_actor}",
                "type": "send",
                "label": handler_label,
            })

    # Ask edges
    for ae in asks:
        src_id = f"func:{ae.source_func}"
        if ae.source_func in actor_handler_funcs:
            for aname in actors:
                if ae.source_func.startswith(aname + "_"):
                    src_id = f"actor:{aname}"
                    break
        target_actor = ae.target_ssa
        handler_label = f"msg#{ae.msg_type}"
        if target_actor in actors and ae.msg_type < len(actors[target_actor].handlers):
            handler_label = actors[target_actor].handlers[ae.msg_type]
        ret_label = f" → {ae.result_type}" if ae.result_type else ""
        if target_actor in actors:
            edges.append({
                "source": src_id,
                "target": f"actor:{target_actor}",
                "type": "ask",
                "label": handler_label + ret_label,
            })

    # Call edges (function-to-function, include private user functions, skip hew_ runtime helpers)
    private_funcs_needed: set[str] = set()
    for fname, finfo in funcs.items():
        if finfo.is_dispatch or fname in actor_handler_funcs:
            continue
        src_id = f"func:{fname}"
        for op in finfo.ops:
            if op.startswith("call @"):
                callee = op[6:]
                if callee not in funcs or callee in actor_handler_funcs:
                    continue
                if callee.startswith("hew_") or callee.startswith("__hew_"):
                    continue
                if funcs[callee].is_private:
                    private_funcs_needed.add(callee)
                edges.append({
                    "source": src_id,
                    "target": f"func:{callee}",
                    "type": "call",
                    "label": callee,
                })

    # Add private user-defined function nodes (discovered from call edges)
    for pname in private_funcs_needed:
        pinfo = funcs[pname]
        pnode: dict = {
            "id": f"func:{pname}",
            "type": "function",
            "label": pname,
            "args": pinfo.args,
            "returnType": pinfo.result,
            "ops": pinfo.ops,
            "isMain": False,
            "isPrivate": True,
        }
        cfg = build_internal_cfg(pinfo.body_lines)
        if cfg:
            pnode["internalCFG"] = cfg
        nodes.append(pnode)

    # Deduplicate edges
    seen_edges: set[tuple] = set()
    unique_edges = []
    for e in edges:
        key = (e["source"], e["target"], e["type"], e["label"])
        if key not in seen_edges:
            seen_edges.add(key)
            unique_edges.append(e)

    return {
        "title": title,
        "nodes": nodes,
        "edges": unique_edges,
        "stats": {
            "actorCount": len(actors),
            "functionCount": sum(
                1 for f in funcs.values()
                if not f.is_dispatch and not f.is_private and f.name not in actor_handler_funcs
            ),
            "messageEdgeCount": sum(1 for e in unique_edges if e["type"] in ("send", "ask")),
            "spawnEdgeCount": sum(1 for e in unique_edges if e["type"] == "spawn"),
        },
        "globalStrings": global_strings,
    }


# ── HTML template ────────────────────────────────────────────────────────────

HTML_TEMPLATE = r"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{{TITLE}}</title>
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; display: flex; height: 100vh; overflow: hidden; background: #1a1a2e; color: #e0e0e0; }

/* Sidebar */
#sidebar { width: 250px; min-width: 250px; background: #16213e; display: flex; flex-direction: column; border-right: 1px solid #0f3460; overflow: hidden; }
#sidebar-header { padding: 16px; border-bottom: 1px solid #0f3460; }
#sidebar-header h1 { font-size: 16px; font-weight: 700; color: #e94560; margin-bottom: 10px; }
#search-box { width: 100%; padding: 8px 10px; background: #1a1a2e; border: 1px solid #0f3460; border-radius: 4px; color: #e0e0e0; font-size: 13px; outline: none; }
#search-box:focus { border-color: #e94560; }
#search-box::placeholder { color: #666; }
#sidebar-content { flex: 1; overflow-y: auto; padding: 12px; }
#sidebar-content::-webkit-scrollbar { width: 6px; }
#sidebar-content::-webkit-scrollbar-thumb { background: #0f3460; border-radius: 3px; }

.sidebar-section { margin-bottom: 16px; }
.sidebar-section h2 { font-size: 11px; text-transform: uppercase; letter-spacing: 1px; color: #888; margin-bottom: 8px; padding-bottom: 4px; border-bottom: 1px solid #0f3460; }
.sidebar-item { padding: 6px 8px; border-radius: 4px; cursor: pointer; font-size: 13px; display: flex; align-items: center; justify-content: space-between; margin-bottom: 2px; transition: background 0.15s; }
.sidebar-item:hover { background: #1a1a2e; }
.sidebar-item.active { background: #0f3460; }
.sidebar-item .badge { background: #0f3460; color: #aaa; font-size: 10px; padding: 1px 6px; border-radius: 8px; min-width: 20px; text-align: center; }
.sidebar-item.active .badge { background: #e94560; color: #fff; }
.sidebar-item .dot { width: 8px; height: 8px; border-radius: 50%; margin-right: 8px; flex-shrink: 0; }
.dot-actor { background: #2196F3; }
.dot-supervisor { background: #7B1FA2; }
.dot-function { background: #FFC107; }
.dot-main { background: #FFC107; box-shadow: 0 0 4px #FFC107; }

.stats-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 6px; }
.stat-card { background: #1a1a2e; padding: 8px; border-radius: 4px; text-align: center; }
.stat-card .stat-value { font-size: 20px; font-weight: 700; color: #e94560; }
.stat-card .stat-label { font-size: 10px; color: #888; text-transform: uppercase; }

.legend-item { display: flex; align-items: center; gap: 8px; font-size: 12px; margin-bottom: 4px; color: #aaa; }
.legend-line { width: 24px; height: 3px; border-radius: 2px; }
.legend-circle { width: 12px; height: 12px; border-radius: 50%; flex-shrink: 0; }

/* Main area */
#main { flex: 1; position: relative; background: #1a1a2e; }
svg { width: 100%; height: 100%; }

/* Tooltip */
#tooltip { position: absolute; pointer-events: none; background: #16213e; border: 1px solid #0f3460; border-radius: 6px; padding: 10px 14px; font-size: 12px; line-height: 1.6; box-shadow: 0 4px 16px rgba(0,0,0,0.4); max-width: 320px; display: none; z-index: 100; }
#tooltip .tt-title { font-weight: 700; font-size: 14px; margin-bottom: 4px; }
#tooltip .tt-type { color: #888; font-size: 11px; text-transform: uppercase; margin-bottom: 6px; }
#tooltip .tt-row { color: #bbb; }
#tooltip .tt-label { color: #888; }

/* Node styles */
.node-circle { cursor: pointer; transition: filter 0.2s; }
.node-circle:hover { filter: brightness(1.2); }
.node-label { pointer-events: none; font-size: 11px; font-weight: 600; text-anchor: middle; dominant-baseline: central; fill: #fff; }
.node-label-func { font-size: 10px; font-weight: 400; }
.edge-label { font-size: 10px; pointer-events: none; }
.node-faded { opacity: 0.15; }
.edge-faded { opacity: 0.08; }
.node-highlight { filter: drop-shadow(0 0 8px rgba(233, 69, 96, 0.8)); }
@keyframes pulse { 0%,100% { filter: drop-shadow(0 0 4px rgba(233, 69, 96, 0.5)); } 50% { filter: drop-shadow(0 0 12px rgba(233, 69, 96, 1)); } }
.node-pulse { animation: pulse 1s ease-in-out infinite; }

/* Breadcrumb bar (detail view) */
#breadcrumb { position: absolute; top: 8px; left: 260px; right: 8px; background: #16213e; border: 1px solid #0f3460; border-radius: 4px; padding: 6px 12px; font-size: 13px; z-index: 50; display: none; }
#breadcrumb a { color: #e94560; cursor: pointer; text-decoration: none; }
#breadcrumb a:hover { text-decoration: underline; }
#breadcrumb .breadcrumb-sep { color: #555; margin: 0 6px; }
#breadcrumb .breadcrumb-current { color: #e0e0e0; }
</style>
</head>
<body>
<div id="sidebar">
  <div id="sidebar-header">
    <h1>🔍 Hew System Explorer</h1>
    <input type="text" id="search-box" placeholder="Search actors & functions...">
  </div>
  <div id="sidebar-content"></div>
</div>
<div id="main">
  <svg id="graph"></svg>
  <div id="tooltip"></div>
  <div id="breadcrumb"></div>
</div>

<script>
const GRAPH_DATA = {{GRAPH_DATA}};
</script>
<script src="https://d3js.org/d3.v7.min.js"></script>
<script>
(function() {
  "use strict";

  const data = GRAPH_DATA;
  const nodes = data.nodes.map(d => ({...d}));
  const edges = data.edges.map(d => ({...d}));

  // View state: 'system' or 'detail'
  let currentView = 'system';
  let detailNode = null;

  // Expanded actor state: track which actors have handlers visible
  const expandedActors = new Set();
  // Handler sub-nodes (created on expand)
  let handlerNodes = [];
  let handlerEdges = [];

  // ── Build sidebar ─────────────────────────────────────────────────
  const sidebarContent = document.getElementById("sidebar-content");

  function buildSidebar() {
    let html = "";

    // Actors
    const actorNodes = nodes.filter(n => n.type === "actor" || n.type === "supervisor");
    if (actorNodes.length > 0) {
      html += '<div class="sidebar-section"><h2>Actors</h2>';
      actorNodes.forEach(n => {
        const dotCls = n.type === "supervisor" ? "dot-supervisor" : "dot-actor";
        html += `<div class="sidebar-item" data-id="${n.id}">
          <span style="display:flex;align-items:center"><span class="dot ${dotCls}"></span>${n.label}</span>
          <span class="badge">${n.handlerCount}</span>
        </div>`;
      });
      html += "</div>";
    }

    // Functions
    const funcNodes = nodes.filter(n => n.type === "function");
    if (funcNodes.length > 0) {
      html += '<div class="sidebar-section"><h2>Functions</h2>';
      funcNodes.forEach(n => {
        const cls = n.isMain ? "dot-main" : "dot-function";
        html += `<div class="sidebar-item" data-id="${n.id}">
          <span style="display:flex;align-items:center"><span class="dot ${cls}"></span>${n.label}</span>
        </div>`;
      });
      html += "</div>";
    }

    // Stats
    html += `<div class="sidebar-section"><h2>Stats</h2>
      <div class="stats-grid">
        <div class="stat-card"><div class="stat-value">${data.stats.actorCount}</div><div class="stat-label">Actors</div></div>
        <div class="stat-card"><div class="stat-value">${data.stats.functionCount}</div><div class="stat-label">Functions</div></div>
        <div class="stat-card"><div class="stat-value">${data.stats.messageEdgeCount}</div><div class="stat-label">Messages</div></div>
        <div class="stat-card"><div class="stat-value">${data.stats.spawnEdgeCount}</div><div class="stat-label">Spawns</div></div>
      </div>
    </div>`;

    // Legend
    html += `<div class="sidebar-section"><h2>Legend</h2>
      <div class="legend-item"><span class="legend-circle" style="background:#2196F3"></span> Actor</div>
      <div class="legend-item"><span class="legend-circle" style="background:#7B1FA2"></span> Supervisor</div>
      <div class="legend-item"><span class="legend-circle" style="background:#FFC107"></span> Main function</div>
      <div class="legend-item"><span class="legend-circle" style="background:#9E9E9E"></span> Function</div>
      <div class="legend-item"><span class="legend-circle" style="background:#4CAF50"></span> Handler</div>
      <div class="legend-item"><span class="legend-line" style="background:#2196F3"></span> Spawn</div>
      <div class="legend-item"><span class="legend-line" style="background:#FF9800;border-top:2px dashed #FF9800;height:0"></span> Send</div>
      <div class="legend-item"><span class="legend-line" style="background:#E91E63"></span> Ask</div>
      <div class="legend-item"><span class="legend-line" style="background:#4CAF50;border-top:2px dotted #4CAF50;height:0"></span> Call</div>
    </div>`;

    sidebarContent.innerHTML = html;

    // Click handlers for sidebar items
    document.querySelectorAll(".sidebar-item").forEach(el => {
      el.addEventListener("click", () => {
        const id = el.dataset.id;
        focusNode(id);
        document.querySelectorAll(".sidebar-item").forEach(e => e.classList.remove("active"));
        el.classList.add("active");
      });
    });
  }
  buildSidebar();

  // ── SVG setup ─────────────────────────────────────────────────────
  const svg = d3.select("#graph");
  const width = document.getElementById("main").clientWidth;
  const height = document.getElementById("main").clientHeight;

  // Arrow markers
  const defs = svg.append("defs");

  function addMarker(id, color) {
    defs.append("marker")
      .attr("id", id)
      .attr("viewBox", "0 -5 10 10")
      .attr("refX", 20)
      .attr("refY", 0)
      .attr("markerWidth", 8)
      .attr("markerHeight", 8)
      .attr("orient", "auto")
      .append("path")
      .attr("d", "M0,-5L10,0L0,5")
      .attr("fill", color);
  }
  addMarker("arrow-spawn", "#2196F3");
  addMarker("arrow-send", "#FF9800");
  addMarker("arrow-ask", "#E91E63");
  addMarker("arrow-call", "#4CAF50");

  // Larger markers for bigger nodes
  function addMarkerLg(id, color) {
    defs.append("marker")
      .attr("id", id)
      .attr("viewBox", "0 -5 10 10")
      .attr("refX", 32)
      .attr("refY", 0)
      .attr("markerWidth", 8)
      .attr("markerHeight", 8)
      .attr("orient", "auto")
      .append("path")
      .attr("d", "M0,-5L10,0L0,5")
      .attr("fill", color);
  }
  addMarkerLg("arrow-spawn-lg", "#2196F3");
  addMarkerLg("arrow-send-lg", "#FF9800");
  addMarkerLg("arrow-ask-lg", "#E91E63");

  const g = svg.append("g");

  // Zoom/pan
  const zoom = d3.zoom()
    .scaleExtent([0.1, 4])
    .on("zoom", (event) => g.attr("transform", event.transform));
  svg.call(zoom);

  // ── Force simulation ──────────────────────────────────────────────
  function getAllNodes() { return [...nodes, ...handlerNodes]; }
  function getAllEdges() { return [...edges, ...handlerEdges]; }

  function nodeRadius(d) {
    if (d.type === "supervisor") return 38;
    if (d.type === "actor") return 35;
    if (d.type === "handler") return 12;
    if (d.isPrivate) return 16;
    return 20;
  }

  function nodeColor(d) {
    if (d.type === "supervisor") return "#7B1FA2";
    if (d.type === "actor") return "#2196F3";
    if (d.type === "handler") return "#4CAF50";
    if (d.isMain) return "#FFC107";
    if (d.isPrivate) return "#6E6E6E";
    return "#9E9E9E";
  }

  const edgeStyles = {
    spawn: { color: "#2196F3", dash: "", width: 2.5 },
    send:  { color: "#FF9800", dash: "6,3", width: 1.8 },
    ask:   { color: "#E91E63", dash: "", width: 2 },
    call:  { color: "#4CAF50", dash: "3,3", width: 1 },
    handler: { color: "#4CAF5088", dash: "2,2", width: 1 },
  };

  function markerForEdge(d) {
    const tgt = getAllNodes().find(n => n.id === (typeof d.target === "object" ? d.target.id : d.target));
    const big = tgt && (tgt.type === "actor" || tgt.type === "supervisor");
    if (d.type === "spawn") return big ? "url(#arrow-spawn-lg)" : "url(#arrow-spawn)";
    if (d.type === "send") return big ? "url(#arrow-send-lg)" : "url(#arrow-send)";
    if (d.type === "ask") return big ? "url(#arrow-ask-lg)" : "url(#arrow-ask)";
    if (d.type === "call") return "url(#arrow-call)";
    return "";
  }

  const simulation = d3.forceSimulation()
    .force("link", d3.forceLink().id(d => d.id).distance(d => {
      if (d.type === "handler") return 60;
      return 160;
    }).strength(d => d.type === "handler" ? 1.5 : 0.3))
    .force("charge", d3.forceManyBody().strength(d => d.type === "handler" ? -50 : -400))
    .force("center", d3.forceCenter(width / 2, height / 2))
    .force("collision", d3.forceCollide().radius(d => nodeRadius(d) + 10));

  // ── Render ────────────────────────────────────────────────────────
  let linkGroup = g.append("g").attr("class", "links");
  let linkLabelGroup = g.append("g").attr("class", "link-labels");
  let nodeGroup = g.append("g").attr("class", "nodes");

  function render() {
    const allNodes = getAllNodes();
    const allEdges = getAllEdges().filter(e => {
      // Only render edges whose source and target exist
      const sId = typeof e.source === "object" ? e.source.id : e.source;
      const tId = typeof e.target === "object" ? e.target.id : e.target;
      return allNodes.some(n => n.id === sId) && allNodes.some(n => n.id === tId);
    });

    // Links
    const links = linkGroup.selectAll("line").data(allEdges, d => d.source.id || d.source + "-" + (d.target.id || d.target) + "-" + d.type + "-" + d.label);
    links.exit().transition().duration(300).attr("opacity", 0).remove();
    const linksEnter = links.enter().append("line")
      .attr("stroke", d => (edgeStyles[d.type] || edgeStyles.call).color)
      .attr("stroke-width", d => (edgeStyles[d.type] || edgeStyles.call).width)
      .attr("stroke-dasharray", d => (edgeStyles[d.type] || edgeStyles.call).dash)
      .attr("marker-end", d => markerForEdge(d))
      .attr("opacity", 0)
      .transition().duration(300).attr("opacity", 0.7);

    // Link labels
    const lbls = linkLabelGroup.selectAll("text").data(allEdges, d => d.source.id || d.source + "-" + (d.target.id || d.target) + "-" + d.type + "-" + d.label);
    lbls.exit().transition().duration(300).attr("opacity", 0).remove();
    lbls.enter().append("text")
      .attr("class", "edge-label")
      .attr("fill", d => (edgeStyles[d.type] || edgeStyles.call).color)
      .attr("text-anchor", "middle")
      .attr("dy", -6)
      .text(d => d.label || "")
      .attr("opacity", 0)
      .transition().duration(300).attr("opacity", 0.8);

    // Nodes
    const nodesSel = nodeGroup.selectAll("g.node").data(allNodes, d => d.id);
    nodesSel.exit().transition().duration(300).attr("opacity", 0).remove();

    const enter = nodesSel.enter().append("g")
      .attr("class", "node")
      .attr("opacity", 0)
      .call(d3.drag()
        .on("start", dragStart)
        .on("drag", dragging)
        .on("end", dragEnd));

    enter.append("circle")
      .attr("class", "node-circle")
      .attr("r", d => nodeRadius(d))
      .attr("fill", d => nodeColor(d))
      .attr("stroke", d => d.type === "supervisor" ? "#4A148C" : d.type === "actor" ? "#1565C0" : d.isPrivate ? "#888" : "none")
      .attr("stroke-width", d => (d.type === "actor" || d.type === "supervisor") ? 2 : d.isPrivate ? 1.5 : 0)
      .attr("stroke-dasharray", d => d.isPrivate ? "4,2" : "");

    enter.append("text")
      .attr("class", d => "node-label" + (d.type === "function" ? " node-label-func" : ""))
      .text(d => {
        const lbl = d.label;
        if (d.type === "handler") return lbl;
        if (lbl.length > 10) return lbl.slice(0, 9) + "…";
        return lbl;
      })
      .attr("dy", d => d.type === "handler" ? 20 : 1);

    enter.transition().duration(300).attr("opacity", 1);

    // Event handlers
    enter.on("click", (event, d) => {
      event.stopPropagation();
      if (d.type === "actor" || d.type === "supervisor") toggleExpand(d);
    })
    .on("dblclick", (event, d) => {
      event.stopPropagation();
      if ((d.type === "function" || d.type === "handler") && d.internalCFG && d.internalCFG.blocks.length > 0) {
        enterDetailView(d);
      } else {
        focusNode(d.id);
      }
    })
    .on("mouseover", (event, d) => showTooltip(event, d))
    .on("mousemove", (event) => moveTooltip(event))
    .on("mouseout", hideTooltip);

    // Update simulation
    simulation.nodes(allNodes);
    simulation.force("link").links(allEdges);
    simulation.alpha(0.5).restart();
  }

  simulation.on("tick", () => {
    linkGroup.selectAll("line")
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);

    linkLabelGroup.selectAll("text")
      .attr("x", d => (d.source.x + d.target.x) / 2)
      .attr("y", d => (d.source.y + d.target.y) / 2);

    nodeGroup.selectAll("g.node")
      .attr("transform", d => `translate(${d.x},${d.y})`);
  });

  render();

  // ── Expand/collapse actor handlers ────────────────────────────────
  function toggleExpand(actorNode) {
    const actorId = actorNode.id;
    if (expandedActors.has(actorId)) {
      expandedActors.delete(actorId);
      handlerNodes = handlerNodes.filter(n => n.parentActor !== actorId);
      handlerEdges = handlerEdges.filter(e => {
        const sId = typeof e.source === "object" ? e.source.id : e.source;
        return !sId.startsWith("handler:" + actorNode.label + "_");
      });
      handlerEdges = handlerEdges.filter(e => {
        const tId = typeof e.target === "object" ? e.target.id : e.target;
        return !tId.startsWith("handler:" + actorNode.label + "_");
      });
    } else {
      expandedActors.add(actorId);
      const handlers = actorNode.handlers || [];
      const angleStep = (2 * Math.PI) / Math.max(handlers.length, 1);
      handlers.forEach((h, i) => {
        const angle = angleStep * i - Math.PI / 2;
        const hNode = {
          id: `handler:${actorNode.label}_${h.name}`,
          type: "handler",
          label: h.name,
          params: h.params,
          returnType: h.returnType,
          internalCFG: h.internalCFG || null,
          parentActor: actorId,
          x: actorNode.x + Math.cos(angle) * 70,
          y: actorNode.y + Math.sin(angle) * 70,
        };
        handlerNodes.push(hNode);
        handlerEdges.push({
          source: actorId,
          target: hNode.id,
          type: "handler",
          label: "",
        });
      });
    }
    render();
  }

  // ── Drag ──────────────────────────────────────────────────────────
  function dragStart(event, d) {
    if (!event.active) simulation.alphaTarget(0.3).restart();
    d.fx = d.x; d.fy = d.y;
  }
  function dragging(event, d) { d.fx = event.x; d.fy = event.y; }
  function dragEnd(event, d) {
    if (!event.active) simulation.alphaTarget(0);
    d.fx = null; d.fy = null;
  }

  // ── Tooltip ───────────────────────────────────────────────────────
  const tooltip = document.getElementById("tooltip");

  function showTooltip(event, d) {
    let html = "";
    if (d.type === "actor" || d.type === "supervisor") {
      const actorColor = d.type === "supervisor" ? "#7B1FA2" : "#2196F3";
      const actorLabel = d.type === "supervisor" ? "Supervisor" : "Actor";
      html = `<div class="tt-title" style="color:${actorColor}">${d.label}</div>
        <div class="tt-type">${actorLabel}</div>`;
      if (d.stateType) html += `<div class="tt-row"><span class="tt-label">State:</span> ${esc(d.stateType)}</div>`;
      if (d.handlers && d.handlers.length) {
        html += `<div class="tt-row"><span class="tt-label">Handlers:</span></div>`;
        d.handlers.forEach(h => {
          const ret = h.returnType ? ` → ${esc(h.returnType)}` : "";
          html += `<div class="tt-row" style="padding-left:8px">• ${esc(h.name)}(${esc(h.params)})${ret}</div>`;
        });
      }
      html += `<div class="tt-row" style="margin-top:4px;color:#666;font-size:11px">Click to ${expandedActors.has(d.id) ? "collapse" : "expand"} handlers</div>`;
    } else if (d.type === "function") {
      const color = d.isMain ? "#FFC107" : "#9E9E9E";
      html = `<div class="tt-title" style="color:${color}">${d.label}</div>
        <div class="tt-type">Function</div>`;
      if (d.args) html += `<div class="tt-row"><span class="tt-label">Args:</span> (${esc(d.args)})</div>`;
      if (d.returnType) html += `<div class="tt-row"><span class="tt-label">Returns:</span> ${esc(d.returnType)}</div>`;
      if (d.ops && d.ops.length) {
        html += `<div class="tt-row"><span class="tt-label">Ops:</span> ${d.ops.map(esc).join(", ")}</div>`;
      }
      if (d.internalCFG && d.internalCFG.blocks.length) {
        html += `<div class="tt-row" style="margin-top:4px;color:#666;font-size:11px">Double-click to view control flow</div>`;
      }
    } else if (d.type === "handler") {
      html = `<div class="tt-title" style="color:#4CAF50">${d.label}</div>
        <div class="tt-type">Handler</div>`;
      if (d.params) html += `<div class="tt-row"><span class="tt-label">Params:</span> ${esc(d.params)}</div>`;
      if (d.returnType) html += `<div class="tt-row"><span class="tt-label">Returns:</span> ${esc(d.returnType)}</div>`;
      if (d.internalCFG && d.internalCFG.blocks.length) {
        html += `<div class="tt-row" style="margin-top:4px;color:#666;font-size:11px">Double-click to view control flow</div>`;
      }
    }
    tooltip.innerHTML = html;
    tooltip.style.display = "block";
    moveTooltip(event);
  }

  function moveTooltip(event) {
    const mainRect = document.getElementById("main").getBoundingClientRect();
    let x = event.clientX - mainRect.left + 12;
    let y = event.clientY - mainRect.top + 12;
    if (x + 320 > mainRect.width) x = x - 340;
    if (y + 200 > mainRect.height) y = y - 220;
    tooltip.style.left = x + "px";
    tooltip.style.top = y + "px";
  }

  function hideTooltip() { tooltip.style.display = "none"; }

  function esc(s) {
    if (!s) return "";
    const d = document.createElement("div");
    d.textContent = s;
    return d.innerHTML;
  }

  // ── Focus node ────────────────────────────────────────────────────
  function focusNode(id) {
    const allNodes = getAllNodes();
    const node = allNodes.find(n => n.id === id);
    if (!node || node.x == null) return;

    // Pan to node
    const transform = d3.zoomTransform(svg.node());
    const targetX = width / 2 - node.x * transform.k;
    const targetY = height / 2 - node.y * transform.k;
    svg.transition().duration(500).call(
      zoom.transform,
      d3.zoomIdentity.translate(targetX, targetY).scale(transform.k)
    );

    // Highlight
    nodeGroup.selectAll("g.node").classed("node-pulse", false);
    nodeGroup.selectAll("g.node")
      .filter(d => d.id === id)
      .classed("node-pulse", true);
    setTimeout(() => {
      nodeGroup.selectAll("g.node").classed("node-pulse", false);
    }, 2000);
  }

  // ── Search ────────────────────────────────────────────────────────
  const searchBox = document.getElementById("search-box");
  searchBox.addEventListener("input", () => {
    const q = searchBox.value.toLowerCase().trim();
    if (!q) {
      nodeGroup.selectAll("g.node").classed("node-faded", false).classed("node-highlight", false);
      linkGroup.selectAll("line").classed("edge-faded", false);
      linkLabelGroup.selectAll("text").classed("edge-faded", false);
      document.querySelectorAll(".sidebar-item").forEach(el => el.style.display = "");
      return;
    }

    const allNodes = getAllNodes();
    const matchIds = new Set();
    allNodes.forEach(n => {
      if (n.label.toLowerCase().includes(q) || (n.id && n.id.toLowerCase().includes(q))) {
        matchIds.add(n.id);
      }
    });

    nodeGroup.selectAll("g.node")
      .classed("node-faded", d => !matchIds.has(d.id))
      .classed("node-highlight", d => matchIds.has(d.id));

    linkGroup.selectAll("line")
      .classed("edge-faded", d => {
        const sId = typeof d.source === "object" ? d.source.id : d.source;
        const tId = typeof d.target === "object" ? d.target.id : d.target;
        return !matchIds.has(sId) && !matchIds.has(tId);
      });
    linkLabelGroup.selectAll("text")
      .classed("edge-faded", d => {
        const sId = typeof d.source === "object" ? d.source.id : d.source;
        const tId = typeof d.target === "object" ? d.target.id : d.target;
        return !matchIds.has(sId) && !matchIds.has(tId);
      });

    // Filter sidebar
    document.querySelectorAll(".sidebar-item").forEach(el => {
      const id = el.dataset.id || "";
      const text = el.textContent.toLowerCase();
      el.style.display = (text.includes(q) || id.toLowerCase().includes(q)) ? "" : "none";
    });
  });

  // ── Double-click background to reset ──────────────────────────────
  svg.on("dblclick.zoom", null);
  svg.on("dblclick", () => {
    if (currentView === 'system') {
      svg.transition().duration(500).call(
        zoom.transform,
        d3.zoomIdentity.translate(width / 2, height / 2).scale(0.8).translate(-width / 2, -height / 2)
      );
    }
  });

  // ── Detail view ──────────────────────────────────────────────────
  const breadcrumb = document.getElementById("breadcrumb");
  let detailGroup = null;

  const BLOCK_COLORS = {
    entry: "#C8E6C9",
    exit: "#FFCDD2",
    loop: "#BBDEFB",
    conditional: "#FFF9C4",
    normal: "#F5F5F5",
  };
  const BLOCK_TEXT_COLOR = "#222";

  function enterDetailView(node) {
    if (!node.internalCFG || !node.internalCFG.blocks.length) return;
    currentView = 'detail';
    detailNode = node;

    // Stop simulation and hide system view
    simulation.stop();
    g.style("display", "none");

    // Show breadcrumb
    const typeLabel = node.type === "handler" ? "handler" : "fn";
    breadcrumb.innerHTML = `<a id="breadcrumb-back">← Back to System View</a>` +
      `<span class="breadcrumb-sep">›</span>` +
      `<span class="breadcrumb-current">${typeLabel} ${esc(node.label)}</span>`;
    breadcrumb.style.display = "block";
    document.getElementById("breadcrumb-back").addEventListener("click", exitDetailView);

    // Build detail sidebar
    buildDetailSidebar(node);

    // Render CFG
    renderDetailCFG(node.internalCFG);

    // Reset zoom for detail view
    svg.call(zoom.transform, d3.zoomIdentity);
  }

  function exitDetailView() {
    if (currentView !== 'detail') return;
    currentView = 'system';
    detailNode = null;

    // Remove detail rendering
    if (detailGroup) { detailGroup.remove(); detailGroup = null; }
    breadcrumb.style.display = "none";

    // Restore system view
    g.style("display", null);
    buildSidebar();
    simulation.alpha(0.3).restart();
  }

  // Escape key to go back
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape" && currentView === "detail") exitDetailView();
  });

  function buildDetailSidebar(node) {
    let html = '';
    const typeLabel = node.type === "handler" ? "Handler" : "Function";
    const color = node.type === "handler" ? "#4CAF50" : (node.isMain ? "#FFC107" : "#9E9E9E");
    html += `<div class="sidebar-section"><h2>${typeLabel} Details</h2>`;
    html += `<div style="padding:4px 8px;margin-bottom:8px"><span style="color:${color};font-weight:700;font-size:14px">${esc(node.label)}</span></div>`;
    if (node.args) html += `<div style="padding:2px 8px;font-size:12px;color:#bbb"><span style="color:#888">Args:</span> ${esc(node.args)}</div>`;
    if (node.params) html += `<div style="padding:2px 8px;font-size:12px;color:#bbb"><span style="color:#888">Params:</span> ${esc(node.params)}</div>`;
    if (node.returnType) html += `<div style="padding:2px 8px;font-size:12px;color:#bbb"><span style="color:#888">Returns:</span> ${esc(node.returnType)}</div>`;
    if (node.ops && node.ops.length) {
      html += `<div style="padding:2px 8px;font-size:12px;color:#bbb"><span style="color:#888">Ops:</span> ${node.ops.map(esc).join(", ")}</div>`;
    }
    html += `</div>`;

    // Block list
    const cfg = node.internalCFG;
    if (cfg && cfg.blocks.length) {
      html += `<div class="sidebar-section"><h2>Blocks</h2>`;
      cfg.blocks.forEach(b => {
        const kindColor = BLOCK_COLORS[b.kind] || BLOCK_COLORS.normal;
        html += `<div class="sidebar-item" data-block-id="${b.id}">` +
          `<span style="display:flex;align-items:center"><span class="dot" style="background:${kindColor}"></span>${esc(b.label)}</span>` +
          `<span class="badge">${b.ops.length}</span></div>`;
      });
      html += `</div>`;
    }

    // Legend
    html += `<div class="sidebar-section"><h2>Block Types</h2>`;
    html += `<div class="legend-item"><span class="legend-circle" style="background:#C8E6C9"></span> Entry</div>`;
    html += `<div class="legend-item"><span class="legend-circle" style="background:#FFCDD2"></span> Exit</div>`;
    html += `<div class="legend-item"><span class="legend-circle" style="background:#BBDEFB"></span> Loop</div>`;
    html += `<div class="legend-item"><span class="legend-circle" style="background:#FFF9C4"></span> Conditional</div>`;
    html += `<div class="legend-item"><span class="legend-circle" style="background:#F5F5F5"></span> Normal</div>`;
    html += `</div>`;

    sidebarContent.innerHTML = html;

    // Click handlers for block items → pan to block
    document.querySelectorAll("[data-block-id]").forEach(el => {
      el.addEventListener("click", () => {
        const bid = el.dataset.blockId;
        panToBlock(bid);
        document.querySelectorAll("[data-block-id]").forEach(e => e.classList.remove("active"));
        el.classList.add("active");
      });
    });
  }

  function panToBlock(blockId) {
    if (!detailGroup) return;
    const blockEl = detailGroup.select(`[data-block-id="${blockId}"]`);
    if (blockEl.empty()) return;
    const tx = +blockEl.attr("data-cx");
    const ty = +blockEl.attr("data-cy");
    const transform = d3.zoomTransform(svg.node());
    const targetX = width / 2 - tx * transform.k;
    const targetY = height / 2 - ty * transform.k;
    svg.transition().duration(400).call(
      zoom.transform,
      d3.zoomIdentity.translate(targetX, targetY).scale(transform.k)
    );
  }

  function renderDetailCFG(cfg) {
    if (detailGroup) detailGroup.remove();
    detailGroup = svg.append("g").attr("class", "detail-view");
    svg.call(zoom.on("zoom", (event) => {
      g.attr("transform", event.transform);
      if (detailGroup) detailGroup.attr("transform", event.transform);
    }));

    const blocks = cfg.blocks;
    const cfgEdges = cfg.edges;

    // Build adjacency for layering (BFS from entry)
    const adjOut = {};
    const adjIn = {};
    blocks.forEach(b => { adjOut[b.id] = []; adjIn[b.id] = []; });
    cfgEdges.forEach(e => {
      if (e.style === "dashed") return; // skip back-edges for layering
      if (adjOut[e.src]) adjOut[e.src].push(e.dst);
      if (adjIn[e.dst]) adjIn[e.dst].push(e.src);
    });

    // BFS layering
    const layer = {};
    const entryBlock = blocks.find(b => b.kind === "entry") || blocks[0];
    const queue = [entryBlock.id];
    layer[entryBlock.id] = 0;
    while (queue.length) {
      const cur = queue.shift();
      (adjOut[cur] || []).forEach(nxt => {
        if (layer[nxt] === undefined) {
          layer[nxt] = layer[cur] + 1;
          queue.push(nxt);
        }
      });
    }
    // Assign unvisited blocks
    blocks.forEach(b => { if (layer[b.id] === undefined) layer[b.id] = 0; });

    // Group blocks by layer
    const maxLayer = Math.max(...blocks.map(b => layer[b.id]));
    const layerGroups = [];
    for (let l = 0; l <= maxLayer; l++) {
      layerGroups.push(blocks.filter(b => layer[b.id] === l));
    }

    // Layout constants
    const blockW = 180;
    const opLineH = 16;
    const headerH = 24;
    const padV = 8;
    const layerGap = 60;
    const colGap = 40;

    // Calculate block heights
    const blockH = {};
    blocks.forEach(b => {
      blockH[b.id] = headerH + padV + Math.max(b.ops.length, 1) * opLineH + padV;
    });

    // Position blocks
    const blockPos = {};
    const maxLayerW = Math.max(...layerGroups.map(lg => lg.length)) * (blockW + colGap);
    let yOffset = 50;
    for (let l = 0; l <= maxLayer; l++) {
      const grp = layerGroups[l];
      const totalW = grp.length * blockW + (grp.length - 1) * colGap;
      const startX = (width - totalW) / 2;
      let maxH = 0;
      grp.forEach((b, idx) => {
        const x = startX + idx * (blockW + colGap);
        blockPos[b.id] = { x, y: yOffset, w: blockW, h: blockH[b.id] };
        maxH = Math.max(maxH, blockH[b.id]);
      });
      yOffset += maxH + layerGap;
    }

    // Draw edges first (behind blocks)
    const edgeG = detailGroup.append("g").attr("class", "cfg-edges");

    // Arrow marker for detail view
    const detailDefs = detailGroup.append("defs");
    function addCfgMarker(id, color) {
      detailDefs.append("marker")
        .attr("id", id).attr("viewBox", "0 -4 8 8")
        .attr("refX", 7).attr("refY", 0)
        .attr("markerWidth", 7).attr("markerHeight", 7)
        .attr("orient", "auto")
        .append("path").attr("d", "M0,-4L8,0L0,4").attr("fill", color);
    }
    addCfgMarker("cfg-arrow", "#aaa");
    addCfgMarker("cfg-arrow-green", "#2E7D32");
    addCfgMarker("cfg-arrow-red", "#C62828");
    addCfgMarker("cfg-arrow-blue", "#1565C0");

    cfgEdges.forEach(e => {
      const sp = blockPos[e.src];
      const dp = blockPos[e.dst];
      if (!sp || !dp) return;
      const sx = sp.x + sp.w / 2;
      const sy = sp.y + sp.h;
      const dx = dp.x + dp.w / 2;
      const dy = dp.y;
      const color = e.color || "#aaa";
      const dash = (e.style === "dashed") ? "6,3" : "";
      let markerId = "cfg-arrow";
      if (color === "#2E7D32") markerId = "cfg-arrow-green";
      else if (color === "#C62828") markerId = "cfg-arrow-red";
      else if (color === "#1565C0") markerId = "cfg-arrow-blue";

      // Curved path for back-edges or same-column edges
      const isBackEdge = (e.style === "dashed") || (layer[e.dst] <= layer[e.src]);
      let path;
      if (isBackEdge) {
        const offset = 30;
        const side = sx <= dx ? -1 : 1;
        const cx = Math.min(sp.x, dp.x) - offset;
        path = `M${sx},${sy} C${cx},${sy} ${cx},${dy} ${dx},${dy}`;
      } else {
        const midY = (sy + dy) / 2;
        path = `M${sx},${sy} C${sx},${midY} ${dx},${midY} ${dx},${dy}`;
      }

      edgeG.append("path")
        .attr("d", path).attr("fill", "none")
        .attr("stroke", color).attr("stroke-width", 1.5)
        .attr("stroke-dasharray", dash)
        .attr("marker-end", `url(#${markerId})`);

      if (e.label) {
        const mx = (sx + dx) / 2 + (isBackEdge ? -20 : 0);
        const my = (sy + dy) / 2;
        edgeG.append("text")
          .attr("x", mx).attr("y", my)
          .attr("text-anchor", "middle").attr("font-size", 11)
          .attr("fill", color).attr("dy", -4)
          .text(e.label);
      }
    });

    // Draw blocks
    const blockG = detailGroup.append("g").attr("class", "cfg-blocks");
    blocks.forEach(b => {
      const pos = blockPos[b.id];
      if (!pos) return;
      const bg = blockG.append("g")
        .attr("data-block-id", b.id)
        .attr("data-cx", pos.x + pos.w / 2)
        .attr("data-cy", pos.y + pos.h / 2);

      // Block rectangle
      bg.append("rect")
        .attr("x", pos.x).attr("y", pos.y)
        .attr("width", pos.w).attr("height", pos.h)
        .attr("rx", 4).attr("ry", 4)
        .attr("fill", BLOCK_COLORS[b.kind] || BLOCK_COLORS.normal)
        .attr("stroke", "#999").attr("stroke-width", 1);

      // Header
      bg.append("text")
        .attr("x", pos.x + 8).attr("y", pos.y + 16)
        .attr("font-size", 12).attr("font-weight", 700)
        .attr("fill", BLOCK_TEXT_COLOR)
        .text(b.label);

      // Header divider
      bg.append("line")
        .attr("x1", pos.x).attr("y1", pos.y + headerH)
        .attr("x2", pos.x + pos.w).attr("y2", pos.y + headerH)
        .attr("stroke", "#ccc").attr("stroke-width", 0.5);

      // Ops
      b.ops.forEach((op, idx) => {
        bg.append("text")
          .attr("x", pos.x + 8)
          .attr("y", pos.y + headerH + padV + (idx + 1) * opLineH - 3)
          .attr("font-size", 11).attr("fill", BLOCK_TEXT_COLOR)
          .text(op);
      });
      if (!b.ops.length) {
        bg.append("text")
          .attr("x", pos.x + 8)
          .attr("y", pos.y + headerH + padV + opLineH - 3)
          .attr("font-size", 11).attr("fill", "#999").attr("font-style", "italic")
          .text("(empty)");
      }
    });
  }

})();
</script>
</body>
</html>"""


# ── CLI ──────────────────────────────────────────────────────────────────────


def main():
    parser = argparse.ArgumentParser(
        prog="hew-system-explorer",
        description="Generate an interactive HTML visualization of a Hew actor system.",
        epilog="Example: hew build --emit-mlir prog.hew | %(prog)s -o system.html",
    )
    parser.add_argument(
        "input",
        nargs="?",
        default="-",
        help="Input MLIR file (default: stdin)",
    )
    parser.add_argument(
        "-o", "--output",
        required=True,
        help="Output HTML file path",
    )
    parser.add_argument(
        "--title",
        default="Hew System Explorer",
        help="Custom page title (default: 'Hew System Explorer')",
    )
    args = parser.parse_args()

    # Read input
    if args.input == "-":
        mlir_text = sys.stdin.read()
    else:
        with open(args.input) as f:
            mlir_text = f.read()

    if not mlir_text.strip():
        print("Error: empty input", file=sys.stderr)
        sys.exit(1)

    # Parse MLIR
    funcs, actors, spawns, sends, asks, global_strings, supervisor_names = parse_mlir(mlir_text)

    # Build graph data
    graph_data = build_graph_data(funcs, actors, spawns, sends, asks, global_strings, args.title, supervisor_names)

    # Generate HTML
    html_content = HTML_TEMPLATE.replace("{{TITLE}}", html.escape(args.title))
    html_content = html_content.replace("{{GRAPH_DATA}}", json.dumps(graph_data, indent=2))

    with open(args.output, "w") as f:
        f.write(html_content)

    # Summary
    print(
        f"Generated {args.output}: "
        f"{graph_data['stats']['actorCount']} actors, "
        f"{graph_data['stats']['functionCount']} functions, "
        f"{graph_data['stats']['messageEdgeCount']} messages, "
        f"{graph_data['stats']['spawnEdgeCount']} spawns",
        file=sys.stderr,
    )


if __name__ == "__main__":
    main()
