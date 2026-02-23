#!/usr/bin/env python3
"""Generate control flow graphs from Hew MLIR IR output.

Usage:
    hew build --emit-mlir prog.hew | python scripts/viz/cfg.py --function main
    hew build --emit-mlir prog.hew | python scripts/viz/cfg.py --list
    hew build --emit-mlir prog.hew | python scripts/viz/cfg.py --all -f svg -o all-cfg.svg
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import TextIO


# ---------------------------------------------------------------------------
# Op classification
# ---------------------------------------------------------------------------

# Ops to always skip (noise)
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


def classify_op(line: str, verbose: bool = False) -> str | None:
    """Return a human-readable label for an MLIR op line, or None to skip."""
    stripped = line.strip()

    # scf control flow handled structurally
    if stripped.startswith(("scf.if", "scf.while", "scf.for", "} else {")):
        return None

    # Detect the op name (with or without SSA assignment)
    # Patterns:  %x = hew.vec.push ...   or   hew.print ...   or   func.call @name
    op_match = re.match(r'(?:%\S+\s*=\s*)?([a-zA-Z_][a-zA-Z0-9_.]*)', stripped)
    if not op_match:
        return None
    op = op_match.group(1)

    if not verbose and op in NOISE_OPS:
        return None

    # hew.actor_spawn
    if op == "hew.actor_spawn":
        m = re.search(r'actor_name\s*=\s*"(\w+)"', stripped)
        name = m.group(1) if m else "?"
        return f"spawn {name}"

    # hew.actor_send
    if op == "hew.actor_send":
        return "send"

    # hew.actor_ask
    if op == "hew.actor_ask":
        return "ask"

    # hew.actor_stop / hew.actor_close
    if op in ("hew.actor_stop", "hew.actor_close"):
        return op.split(".")[-1]

    # hew.sleep
    if op == "hew.sleep":
        return "sleep"

    # hew.vec.*
    if op.startswith("hew.vec."):
        method = op.split(".")[-1]
        return f"vec.{method}"

    # hew.hashmap.*
    if op.startswith("hew.hashmap."):
        method = op.split(".")[-1]
        return f"hashmap.{method}"

    # hew.print
    if op == "hew.print":
        return "print"

    # hew.string_concat
    if op == "hew.string_concat":
        return "str_concat"

    # hew.string_method
    if op == "hew.string_method":
        return "str_method"

    # hew.receive
    if op == "hew.receive":
        m = re.search(r'handlers\s*=\s*\[([^\]]*)\]', stripped)
        if m:
            handlers = [h.strip().lstrip("@") for h in m.group(1).split(",")]
            return "receive [" + ", ".join(handlers) + "]"
        return "receive"

    # hew.runtime_call
    if op == "hew.runtime_call":
        m = re.search(r'@(\w+)', stripped)
        name = m.group(1) if m else "?"
        return f"runtime: {name}"

    # hew.struct_init
    if op == "hew.struct_init":
        return "struct_init"

    # hew.field_get / hew.field_set
    if op in ("hew.field_get", "hew.field_set"):
        return op.split(".")[-1]

    # hew.cast
    if op == "hew.cast":
        return "cast"

    # func.call / call
    if op in ("func.call", "call"):
        m = re.search(r'@(\w+)', stripped)
        if m:
            name = m.group(1)
            if not verbose and name.startswith("hew_"):
                return None
            return f"call {name}()"
        return "call ?()"

    # arith.cmpi
    if op == "arith.cmpi":
        return "compare"

    # arith.cmpf
    if op == "arith.cmpf":
        return "compare_f"

    # return
    if op == "return":
        return "return"

    # Catch remaining hew.* ops
    if op.startswith("hew."):
        return op

    if verbose:
        return op

    return None


# ---------------------------------------------------------------------------
# Block / CFG data structures
# ---------------------------------------------------------------------------

class BlockKind(Enum):
    ENTRY = auto()
    EXIT = auto()
    LOOP_HEADER = auto()
    CONDITIONAL = auto()
    NORMAL = auto()


@dataclass
class Block:
    id: str
    label: str
    kind: BlockKind = BlockKind.NORMAL
    ops: list[str] = field(default_factory=list)
    truncated: int = 0  # number of hidden ops
    has_return: bool = False  # track return even if truncated


@dataclass
class Edge:
    src: str
    dst: str
    label: str = ""
    color: str = "black"
    style: str = "solid"


@dataclass
class FunctionCFG:
    name: str
    visibility: str  # "private" or "public"
    args: str
    ret: str
    blocks: list[Block] = field(default_factory=list)
    edges: list[Edge] = field(default_factory=list)


# ---------------------------------------------------------------------------
# MLIR parser
# ---------------------------------------------------------------------------

MAX_OPS_PER_BLOCK = 8


def _indent(line: str) -> int:
    return len(line) - len(line.lstrip())


def parse_mlir(text: str) -> list[FunctionCFG]:
    """Parse MLIR text and extract function CFGs."""
    lines = text.split("\n")
    functions: list[FunctionCFG] = []

    i = 0
    while i < len(lines):
        line = lines[i]
        m = re.match(
            r'\s*func\.func\s+(private\s+)?@(\w+)\(([^)]*)\)(?:\s*->\s*(\S+))?\s*\{',
            line,
        )
        if m:
            vis = "private" if m.group(1) else "public"
            name = m.group(2)
            args = m.group(3) or ""
            ret = m.group(4) or "void"
            j = i + 1
            depth = 1
            body_start = j
            while j < len(lines) and depth > 0:
                for ch in lines[j]:
                    if ch == "{":
                        depth += 1
                    elif ch == "}":
                        depth -= 1
                        if depth == 0:
                            break
                j += 1
            body_lines = lines[body_start : j - 1] if j > body_start else []
            cfg = _build_cfg(name, vis, args, ret, body_lines)
            functions.append(cfg)
            i = j
            continue
        i += 1

    return functions


class _CfgBuilder:
    def __init__(self, fname: str, verbose: bool = False) -> None:
        self.fname = fname
        self.verbose = verbose
        self.blocks: list[Block] = []
        self.edges: list[Edge] = []
        self._counter = 0

    def _new_id(self, hint: str = "bb") -> str:
        self._counter += 1
        return f"{hint}_{self._counter}"

    def add_block(self, label: str, kind: BlockKind = BlockKind.NORMAL) -> Block:
        b = Block(id=self._new_id(), label=label, kind=kind)
        self.blocks.append(b)
        return b

    def add_edge(
        self,
        src: str,
        dst: str,
        label: str = "",
        color: str = "black",
        style: str = "solid",
    ) -> None:
        self.edges.append(Edge(src=src, dst=dst, label=label, color=color, style=style))

    def add_op(self, block: Block, op: str) -> None:
        if op == "return":
            block.has_return = True
        if len(block.ops) < MAX_OPS_PER_BLOCK:
            block.ops.append(op)
        else:
            block.truncated += 1


# Shared verbose flag (set from CLI)
_verbose = False


def _build_cfg(
    name: str, vis: str, args: str, ret: str, body: list[str]
) -> FunctionCFG:
    builder = _CfgBuilder(name, _verbose)
    entry = builder.add_block("entry", BlockKind.ENTRY)

    _process_block(builder, body, entry)

    # Tag exit blocks (but keep single-block functions as ENTRY)
    if len(builder.blocks) > 1:
        for b in builder.blocks:
            if b.has_return:
                b.kind = BlockKind.EXIT

    cfg = FunctionCFG(
        name=name, visibility=vis, args=args, ret=ret,
        blocks=builder.blocks, edges=builder.edges,
    )
    return cfg


def _process_block(
    builder: _CfgBuilder,
    lines: list[str],
    current: Block,
) -> Block | None:
    """Process a sequence of MLIR lines, adding ops/blocks/edges.

    Returns the "exit" block from this sequence (block to chain to next),
    or None if there's no fallthrough.
    """
    i = 0
    while i < len(lines):
        line = lines[i]
        stripped = line.strip()

        # Skip empty / brace-only lines
        if not stripped or stripped in ("}", "{"):
            i += 1
            continue

        # --- scf.while ---
        if stripped.startswith("scf.while"):
            # Find the condition block { ... } do { ... }
            cond_lines, do_lines, end_i = _parse_while(lines, i)

            loop_hdr = builder.add_block("while", BlockKind.LOOP_HEADER)
            builder.add_edge(current.id, loop_hdr.id)

            # Add condition ops to loop header
            for cl in cond_lines:
                op = classify_op(cl, _verbose)
                if op:
                    builder.add_op(loop_hdr, op)

            # Loop body
            body_block = builder.add_block("loop body", BlockKind.NORMAL)
            builder.add_edge(
                loop_hdr.id, body_block.id, label="true", color="#2E7D32", style="solid"
            )

            body_exit = _process_block(builder, do_lines, body_block)
            if body_exit:
                builder.add_edge(
                    body_exit.id, loop_hdr.id, label="loop", color="#1565C0", style="dashed"
                )

            # Exit from loop
            after = builder.add_block("after loop", BlockKind.NORMAL)
            builder.add_edge(
                loop_hdr.id, after.id, label="false", color="#C62828", style="solid"
            )
            current = after
            i = end_i
            continue

        # --- scf.for ---
        if stripped.startswith("scf.for"):
            m = re.match(r'scf\.for\s+(%\w+)\s*=\s*(%\w+)\s+to\s+(%\w+)', stripped)
            lbl = "for"
            if m:
                lbl = f"for {m.group(1)} = {m.group(2)} to {m.group(3)}"

            for_body, end_i = _parse_braced(lines, i)
            loop_hdr = builder.add_block(lbl, BlockKind.LOOP_HEADER)
            builder.add_edge(current.id, loop_hdr.id)

            body_block = builder.add_block("for body", BlockKind.NORMAL)
            builder.add_edge(
                loop_hdr.id, body_block.id, label="true", color="#2E7D32", style="solid"
            )
            body_exit = _process_block(builder, for_body, body_block)
            if body_exit:
                builder.add_edge(
                    body_exit.id, loop_hdr.id, label="loop", color="#1565C0", style="dashed"
                )

            after = builder.add_block("after for", BlockKind.NORMAL)
            builder.add_edge(
                loop_hdr.id, after.id, label="done", color="#C62828", style="solid"
            )
            current = after
            i = end_i
            continue

        # --- scf.if ---
        if stripped.startswith("scf.if") or (
            re.match(r'%\S+\s*=\s*scf\.if', stripped)
        ):
            true_lines, false_lines, end_i = _parse_if_else(lines, i)

            cond_block = builder.add_block("if", BlockKind.CONDITIONAL)
            builder.add_edge(current.id, cond_block.id)

            # True branch
            true_block = builder.add_block("then", BlockKind.NORMAL)
            builder.add_edge(
                cond_block.id, true_block.id, label="true", color="#2E7D32", style="solid"
            )
            true_exit = _process_block(builder, true_lines, true_block)

            # After-merge block
            after = builder.add_block("after if", BlockKind.NORMAL)

            if true_exit:
                builder.add_edge(true_exit.id, after.id)

            if false_lines is not None:
                false_block = builder.add_block("else", BlockKind.NORMAL)
                builder.add_edge(
                    cond_block.id, false_block.id, label="false", color="#C62828", style="solid"
                )
                false_exit = _process_block(builder, false_lines, false_block)
                if false_exit:
                    builder.add_edge(false_exit.id, after.id)
            else:
                builder.add_edge(
                    cond_block.id, after.id, label="false", color="#C62828", style="solid"
                )

            current = after
            i = end_i
            continue

        # --- regular op ---
        op = classify_op(stripped, _verbose)
        if op:
            builder.add_op(current, op)
        i += 1

    return current


def _parse_braced(lines: list[str], start: int) -> tuple[list[str], int]:
    """Parse a braced region starting from `start` (line containing {).

    Returns (body_lines, end_index) where end_index is the line after closing }.
    """
    # Find opening brace
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
            body = lines[start + 1 : i]
            return body, i + 1
        i += 1
    return lines[start + 1 :], len(lines)


def _parse_while(
    lines: list[str], start: int
) -> tuple[list[str], list[str], int]:
    """Parse scf.while { cond } do { body }.

    MLIR format:
        scf.while ... {      ← start line
          <cond ops>
        } do {               ← boundary line (closes cond, opens body)
          <body ops>
        }                    ← end

    Returns (cond_lines, body_lines, end_index).
    """
    # Find the "} do {" line by tracking brace depth from `start`
    i = start
    depth = 0
    # Count braces on the scf.while line itself
    for ch in lines[i]:
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1

    cond_start = i + 1
    do_boundary = None
    i += 1

    while i < len(lines):
        line = lines[i]
        stripped = line.strip()
        # Check for "} do {" before counting — this is the boundary
        if "} do {" in stripped and depth == 1:
            do_boundary = i
            # The "}" closes condition, "do {" opens body → depth = 1
            depth = 1
            break
        for ch in line:
            if ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
        i += 1

    if do_boundary is None:
        # Malformed — return what we can
        return [], [], start + 1

    cond_lines = lines[cond_start:do_boundary]

    # Now find the closing "}" of the body block
    body_start = do_boundary + 1
    j = body_start
    while j < len(lines):
        for ch in lines[j]:
            if ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
        if depth == 0:
            body_lines = lines[body_start:j]
            return cond_lines, body_lines, j + 1
        j += 1

    return cond_lines, lines[body_start:], len(lines)


def _parse_if_else(
    lines: list[str], start: int
) -> tuple[list[str], list[str] | None, int]:
    """Parse scf.if %cond { true } else { false }.

    Returns (true_lines, false_lines_or_None, end_index).
    """
    # Find first opening brace
    i = start
    depth = 0
    true_start = None
    true_end = None
    for ci, ch in enumerate(lines[i]):
        if ch == "{":
            depth += 1
            if depth == 1:
                true_start = i + 1

    i += 1
    while i < len(lines):
        line = lines[i]
        for ch in line:
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

    # Check for else
    # The closing } might be on same line as "else {" or next line
    false_lines: list[str] | None = None
    end_i = (true_end + 1) if true_end else len(lines)

    if true_end is not None:
        remaining = lines[true_end].strip()
        # Check if "} else {" is on the same line as the closing brace
        if "else" in remaining:
            # Find the else block
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


# ---------------------------------------------------------------------------
# DOT generation
# ---------------------------------------------------------------------------

BLOCK_COLORS = {
    BlockKind.ENTRY: "#C8E6C9",
    BlockKind.EXIT: "#FFCDD2",
    BlockKind.LOOP_HEADER: "#BBDEFB",
    BlockKind.CONDITIONAL: "#FFF9C4",
    BlockKind.NORMAL: "#F5F5F5",
}


def _escape_dot(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace('"', "&quot;")


def cfg_to_dot(cfg: FunctionCFG) -> str:
    out: list[str] = []
    out.append(f'digraph "{_escape_dot(cfg.name)}" {{')
    out.append("  rankdir=TB;")
    out.append('  node [shape=record, style=filled, fontname="Helvetica", fontsize=10];')
    out.append('  edge [fontname="Helvetica", fontsize=9];')
    out.append(f'  label="fn {_escape_dot(cfg.name)}({_escape_dot(cfg.args)}) → {_escape_dot(cfg.ret)}";')
    out.append("  labelloc=t;")
    out.append('  fontname="Helvetica";')
    out.append("  fontsize=12;")
    out.append("")

    for block in cfg.blocks:
        color = BLOCK_COLORS.get(block.kind, "#F5F5F5")
        label_parts = [f"<b>{_escape_dot(block.label)}</b>"]
        for op in block.ops:
            label_parts.append(_escape_dot(op))
        if block.truncated > 0:
            label_parts.append(f"... +{block.truncated} more")

        # Use HTML-like labels for Graphviz
        html_label = "<" + "<br/>".join(label_parts) + ">"
        out.append(
            f'  {block.id} [label={html_label}, fillcolor="{color}"];'
        )

    out.append("")

    for edge in cfg.edges:
        attrs = []
        if edge.label:
            attrs.append(f'label="{_escape_dot(edge.label)}"')
        if edge.color != "black":
            attrs.append(f'color="{edge.color}"')
            attrs.append(f'fontcolor="{edge.color}"')
        if edge.style != "solid":
            attrs.append(f'style="{edge.style}"')
        attr_str = f" [{', '.join(attrs)}]" if attrs else ""
        out.append(f"  {edge.src} -> {edge.dst}{attr_str};")

    out.append("}")
    return "\n".join(out)


def multi_cfg_to_dot(cfgs: list[FunctionCFG]) -> str:
    out: list[str] = []
    out.append("digraph functions {")
    out.append("  rankdir=TB;")
    out.append('  node [shape=record, style=filled, fontname="Helvetica", fontsize=10];')
    out.append('  edge [fontname="Helvetica", fontsize=9];')
    out.append("  compound=true;")
    out.append("")

    for idx, cfg in enumerate(cfgs):
        prefix = f"f{idx}_"
        out.append(f"  subgraph cluster_{idx} {{")
        out.append(f'    label="fn {_escape_dot(cfg.name)}({_escape_dot(cfg.args)}) → {_escape_dot(cfg.ret)}";')
        out.append('    fontname="Helvetica";')
        out.append("    fontsize=12;")
        out.append('    style="rounded";')
        out.append('    color="#90A4AE";')
        out.append("")

        for block in cfg.blocks:
            color = BLOCK_COLORS.get(block.kind, "#F5F5F5")
            label_parts = [f"<b>{_escape_dot(block.label)}</b>"]
            for op in block.ops:
                label_parts.append(_escape_dot(op))
            if block.truncated > 0:
                label_parts.append(f"... +{block.truncated} more")

            html_label = "<" + "<br/>".join(label_parts) + ">"
            out.append(
                f'    {prefix}{block.id} [label={html_label}, fillcolor="{color}"];'
            )

        out.append("")

        for edge in cfg.edges:
            attrs = []
            if edge.label:
                attrs.append(f'label="{_escape_dot(edge.label)}"')
            if edge.color != "black":
                attrs.append(f'color="{edge.color}"')
                attrs.append(f'fontcolor="{edge.color}"')
            if edge.style != "solid":
                attrs.append(f'style="{edge.style}"')
            attr_str = f" [{', '.join(attrs)}]" if attrs else ""
            out.append(f"    {prefix}{edge.src} -> {prefix}{edge.dst}{attr_str};")

        out.append("  }")
        out.append("")

    out.append("}")
    return "\n".join(out)


# ---------------------------------------------------------------------------
# Rendering
# ---------------------------------------------------------------------------

def render_dot(dot: str, fmt: str, output: str | None) -> None:
    if fmt == "dot":
        if output:
            with open(output, "w") as f:
                f.write(dot)
        else:
            print(dot)
        return

    try:
        result = subprocess.run(
            ["dot", f"-T{fmt}"],
            input=dot.encode(),
            capture_output=True,
            check=True,
        )
    except FileNotFoundError:
        print("Error: 'dot' (Graphviz) not found. Install graphviz or use -f dot.", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        print(f"Error running Graphviz: {e.stderr.decode()}", file=sys.stderr)
        sys.exit(1)

    if output:
        with open(output, "wb") as f:
            f.write(result.stdout)
    else:
        sys.stdout.buffer.write(result.stdout)


# ---------------------------------------------------------------------------
# Eliminate empty blocks
# ---------------------------------------------------------------------------

def _simplify_cfg(cfg: FunctionCFG) -> FunctionCFG:
    """Remove empty non-entry blocks that just pass through, and empty conditionals."""
    blocks = list(cfg.blocks)
    edges = list(cfg.edges)

    changed = True
    while changed:
        changed = False

        # Merge duplicate true/false edges to same target (enables further simplification)
        edges, merged = _merge_true_false_edges(edges)
        if merged:
            changed = True

        for b in list(blocks):
            if b.ops or b.truncated or b.kind in (BlockKind.ENTRY, BlockKind.EXIT):
                continue

            outgoing = [e for e in edges if e.src == b.id]
            incoming = [e for e in edges if e.dst == b.id]

            # Case 1: Pass-through block (single unlabeled outgoing edge)
            if len(outgoing) == 1 and not outgoing[0].label:
                target = outgoing[0].dst
                new_edges = []
                for e in edges:
                    if e.dst == b.id:
                        new_edges.append(Edge(src=e.src, dst=target,
                                              label=e.label, color=e.color, style=e.style))
                    elif e.src == b.id:
                        pass  # drop outgoing
                    else:
                        new_edges.append(e)
                edges = new_edges
                blocks = [bb for bb in blocks if bb.id != b.id]
                changed = True
                break

            # Case 2: Empty conditional with single incoming and true/false outgoing
            if (b.kind == BlockKind.CONDITIONAL
                    and len(incoming) == 1):
                true_edges = [e for e in outgoing if e.label == "true"]
                false_edges = [e for e in outgoing if e.label == "false"]
                if len(true_edges) == 1 and len(false_edges) == 1:
                    pred = incoming[0].src
                    new_edges = [e for e in edges if e.src != b.id and e.dst != b.id]
                    new_edges.append(Edge(src=pred, dst=true_edges[0].dst,
                                         label="true", color=true_edges[0].color,
                                         style=true_edges[0].style))
                    new_edges.append(Edge(src=pred, dst=false_edges[0].dst,
                                         label="false", color=false_edges[0].color,
                                         style=false_edges[0].style))
                    edges = new_edges
                    blocks = [bb for bb in blocks if bb.id != b.id]
                    changed = True
                    break

    # Final dedup
    seen: set[tuple[str, str, str]] = set()
    deduped: list[Edge] = []
    for e in edges:
        key = (e.src, e.dst, e.label)
        if key not in seen:
            seen.add(key)
            deduped.append(e)

    return FunctionCFG(
        name=cfg.name, visibility=cfg.visibility, args=cfg.args, ret=cfg.ret,
        blocks=blocks, edges=deduped,
    )


def _merge_true_false_edges(edges: list[Edge]) -> tuple[list[Edge], bool]:
    """Merge duplicate true/false edges to the same target into a single unlabeled edge.
    Also remove redundant labeled edges when an unlabeled edge to the same target exists."""
    from collections import defaultdict
    pair_labels: dict[tuple[str, str], list[int]] = defaultdict(list)
    for idx, e in enumerate(edges):
        pair_labels[(e.src, e.dst)].append(idx)

    to_remove: set[int] = set()
    replacements: list[Edge] = []
    for (src, dst), indices in pair_labels.items():
        if len(indices) < 2:
            continue
        labels = {edges[i].label for i in indices}
        # Both true and false go to same target → merge into unlabeled
        if "true" in labels and "false" in labels:
            to_remove.update(indices)
            replacements.append(Edge(src=src, dst=dst))
        # Unlabeled edge exists alongside labeled → drop the labeled ones
        elif "" in labels:
            for i in indices:
                if edges[i].label:
                    to_remove.add(i)

    if not to_remove:
        return edges, False

    new_edges = [e for i, e in enumerate(edges) if i not in to_remove]
    new_edges.extend(replacements)
    return new_edges, True


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    global _verbose

    parser = argparse.ArgumentParser(
        description="Generate control flow graphs from Hew MLIR IR.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""Examples:
  hew build --emit-mlir prog.hew | python scripts/viz/cfg.py --list
  hew build --emit-mlir prog.hew | python scripts/viz/cfg.py -fn main
  hew build --emit-mlir prog.hew | python scripts/viz/cfg.py --all -f svg -o all.svg
""",
    )
    parser.add_argument("input", nargs="?", help="MLIR input file (default: stdin)")
    parser.add_argument(
        "-fn", "--function", help="Function name to generate CFG for"
    )
    parser.add_argument(
        "--list", action="store_true", help="List all functions in the MLIR"
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Generate CFG for all non-private functions",
    )
    parser.add_argument("-o", "--output", help="Output file path")
    parser.add_argument(
        "-f",
        "--format",
        choices=["dot", "svg", "png", "pdf"],
        default="dot",
        help="Output format (default: dot)",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show all ops including noise (arith, loads, stores)",
    )

    args = parser.parse_args()
    _verbose = args.verbose

    # Read input
    if args.input:
        with open(args.input) as f:
            mlir_text = f.read()
    else:
        mlir_text = sys.stdin.read()

    if not mlir_text.strip():
        print("Error: empty input", file=sys.stderr)
        sys.exit(1)

    functions = parse_mlir(mlir_text)

    if not functions:
        print("Error: no functions found in MLIR input", file=sys.stderr)
        sys.exit(1)

    # --list mode
    if args.list:
        for fn in functions:
            vis = " (private)" if fn.visibility == "private" else ""
            print(f"  {fn.name}{vis}  ({fn.args}) → {fn.ret}")
        return

    # --function mode
    if args.function:
        matches = [f for f in functions if f.name == args.function]
        if not matches:
            print(f"Error: function '{args.function}' not found.", file=sys.stderr)
            print("Available functions:", file=sys.stderr)
            for fn in functions:
                print(f"  {fn.name}", file=sys.stderr)
            sys.exit(1)
        cfg = _simplify_cfg(matches[0])
        dot = cfg_to_dot(cfg)
        render_dot(dot, args.format, args.output)
        return

    # --all mode
    if args.all:
        pub_fns = [f for f in functions if f.visibility != "private"]
        if not pub_fns:
            pub_fns = functions
        cfgs = [_simplify_cfg(f) for f in pub_fns]
        dot = multi_cfg_to_dot(cfgs)
        render_dot(dot, args.format, args.output)
        return

    # Default: if only one public function, show it; else list
    pub_fns = [f for f in functions if f.visibility != "private"]
    if len(pub_fns) == 1:
        cfg = _simplify_cfg(pub_fns[0])
        dot = cfg_to_dot(cfg)
        render_dot(dot, args.format, args.output)
    else:
        print("Multiple functions found. Use --function NAME or --all.", file=sys.stderr)
        print("Available functions:", file=sys.stderr)
        for fn in functions:
            vis = " (private)" if fn.visibility == "private" else ""
            print(f"  {fn.name}{vis}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
