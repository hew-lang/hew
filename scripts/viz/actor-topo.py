#!/usr/bin/env python3
"""
hew-actor-topo — Visualize the actor system topology from Hew MLIR dialect IR.

Focuses exclusively on actor-level structure: actors, spawn edges, send/ask
message flows, and handler signatures. Ignores function internals, collection
ops, string ops, and runtime calls.

Usage:
    hew build --emit-mlir prog.hew | python scripts/viz/actor-topo.py > topo.dot
    hew build --emit-mlir prog.hew | python scripts/viz/actor-topo.py -f svg -o topo.svg
    python scripts/viz/actor-topo.py input.mlir -o topo.png
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from dataclasses import dataclass, field


# ── Data model ───────────────────────────────────────────────────────────────


@dataclass
class HandlerInfo:
    name: str  # short name (e.g. "ping")
    full_name: str  # qualified name (e.g. "Pinger_ping")
    params: list[str] = field(default_factory=list)  # types excluding state ptr
    return_type: str = ""


@dataclass
class ActorInfo:
    name: str
    state_fields: list[tuple[str, str]] = field(default_factory=list)  # (name, type)
    state_type_raw: str = ""
    handlers: list[HandlerInfo] = field(default_factory=list)
    handler_order: list[str] = field(default_factory=list)  # from hew.receive


@dataclass
class SpawnEdge:
    source: str  # function or actor name containing the spawn
    target: str  # spawned actor name
    source_is_actor: bool = False


@dataclass
class SendEdge:
    source: str  # function or actor name containing the send
    target_actor: str
    handler_name: str = ""
    param_types: list[str] = field(default_factory=list)
    source_is_actor: bool = False


@dataclass
class AskEdge:
    source: str
    target_actor: str
    handler_name: str = ""
    param_types: list[str] = field(default_factory=list)
    return_type: str = ""
    source_is_actor: bool = False


@dataclass
class SupervisorEdge:
    supervisor: str
    child: str
    strategy: str = ""


# ── Regex patterns ───────────────────────────────────────────────────────────

RE_FUNC = re.compile(
    r"^\s*func\.func\s+(?:private\s+)?@(?P<name>\S+)"
    r"\((?P<args>[^)]*)\)"
    r"(?:\s*->\s*(?P<ret>\S+))?"
)

RE_ACTOR_SPAWN = re.compile(
    r"(?P<ssa>%\S+)\s*=\s*hew\.actor_spawn\s+@(?P<dispatch>\S+)\((?P<init>[^)]*)\)"
    r'\s*\{[^}]*actor_name\s*=\s*"(?P<actor>[^"]+)"'
    r"[^}]*state_type\s*=\s*(?P<state>![^}]+)\}"
)

RE_ACTOR_SEND = re.compile(
    r"hew\.actor_send\s+(?P<target>%\S+)\s*\{msg_type\s*=\s*(?P<msg>\d+)\s*:\s*i32\}"
    r"\((?P<args>[^)]*)\)\s*:\s*\((?P<types>[^)]*)\)"
)

RE_ACTOR_ASK = re.compile(
    r"(?P<ssa>%\S+)\s*=\s*hew\.actor_ask\s+(?P<target>%\S+)\s*\{msg_type\s*=\s*(?P<msg>\d+)\s*:\s*i32\}"
    r"\((?P<args>[^)]*)\)\s*:\s*\((?P<types>[^)]*)\)\s*->\s*(?P<ret>\S+)"
)

RE_ACTOR_STOP = re.compile(r"hew\.actor_stop\s+(?P<target>%\S+)")

RE_RECEIVE = re.compile(
    r"hew\.receive\([^)]*\)\s*\{handlers\s*=\s*\[(?P<handlers>[^\]]+)\]\}"
)

RE_SUPERVISOR_SPAWN = re.compile(
    r'hew\.supervisor\.start_child\s+(?P<target>%\S+)\s*\{[^}]*strategy\s*=\s*"(?P<strategy>[^"]+)"'
)
RE_SUPERVISOR_DISPATCH = re.compile(
    r'supervisor_dispatch'
)
RE_CLOSE_BRACE = re.compile(r"^\s*\}")


# ── State type parsing ───────────────────────────────────────────────────────


def parse_state_type(raw: str) -> list[tuple[str, str]]:
    """Parse !llvm.struct<"Name_state", (i64, f64, ...)> into named fields."""
    m = re.search(r'!llvm\.struct<"(\w+)",\s*\(([^)]*)\)>', raw)
    if not m:
        return []
    types_str = m.group(2).strip()
    if not types_str:
        return []
    types = [t.strip() for t in types_str.split(",")]
    return [(f"field{i}", simplify_type(t)) for i, t in enumerate(types)]


def simplify_type(t: str) -> str:
    """Simplify MLIR/LLVM types for display."""
    t = t.strip()
    if t == "!llvm.ptr":
        return "ptr"
    if t == "!hew.string_ref":
        return "string"
    if t.startswith("!hew.typed_actor_ref"):
        m = re.search(r'"([^"]+)"', t)
        return f"ActorRef<{m.group(1)}>" if m else "ActorRef"
    if t.startswith("!hew.vec<"):
        inner = t[len("!hew.vec<"):-1]
        return f"Vec<{simplify_type(inner)}>"
    if t.startswith("!hew.hashmap<"):
        inner = t[len("!hew.hashmap<"):-1]
        return f"HashMap<{inner}>"
    return t


# ── Parser ───────────────────────────────────────────────────────────────────


def parse_mlir(text: str):
    """Parse MLIR text and extract actor topology information."""
    actors: dict[str, ActorInfo] = {}
    spawns: list[SpawnEdge] = []
    sends: list[SendEdge] = []
    asks: list[AskEdge] = []
    stops: list[tuple[str, str]] = []  # (source, target_actor)
    supervisor_edges: list[SupervisorEdge] = []
    supervisor_names: set[str] = set()

    # First pass: collect all handler function signatures
    handler_sigs: dict[str, HandlerInfo] = {}  # full_name -> HandlerInfo
    # Collect dispatch -> handler mapping from hew.receive
    dispatch_handlers: dict[str, list[str]] = {}  # dispatch_func -> [handler_names]
    # Track which functions are dispatch functions
    dispatch_funcs: set[str] = set()
    # Track which functions are handler functions
    handler_funcs: set[str] = set()

    # SSA → actor name mapping, scoped per function
    func_ssa_map: dict[str, dict[str, str]] = {}  # func_name -> {ssa -> actor}

    current_func = None
    brace_depth = 0

    for line in text.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("//"):
            continue

        brace_depth += line.count("{") - line.count("}")

        # Function start
        m = RE_FUNC.match(stripped)
        if m:
            current_func = m.group("name")
            func_args = m.group("args").strip()
            func_ret = m.group("ret") or ""

            if current_func.endswith("_dispatch"):
                dispatch_funcs.add(current_func)
            else:
                # Potential handler function: ActorName_handlerName
                parts = current_func.split("_", 1)
                if len(parts) == 2 and func_args:
                    arg_list = [a.strip() for a in func_args.split(",")]
                    # Handler functions have %arg0: !llvm.ptr as first param
                    if arg_list and "!llvm.ptr" in arg_list[0]:
                        param_types = []
                        for a in arg_list[1:]:
                            # Extract type from "%argN: type"
                            type_m = re.search(r":\s*(.+)$", a)
                            if type_m:
                                param_types.append(simplify_type(type_m.group(1)))
                        hi = HandlerInfo(
                            name=parts[1],
                            full_name=current_func,
                            params=param_types,
                            return_type=simplify_type(func_ret) if func_ret else "",
                        )
                        handler_sigs[current_func] = hi

            func_ssa_map.setdefault(current_func, {})
            continue

        # Function end
        if brace_depth <= 1 and RE_CLOSE_BRACE.match(stripped):
            current_func = None
            continue

        if current_func is None:
            continue

        # hew.receive — maps msg_type indices to handler functions
        m = RE_RECEIVE.search(stripped)
        if m:
            handlers_str = m.group("handlers")
            handler_names = [h.strip().lstrip("@") for h in handlers_str.split(",")]
            dispatch_handlers[current_func] = handler_names
            for h in handler_names:
                handler_funcs.add(h)
            continue

        # Actor spawn
        m = RE_ACTOR_SPAWN.search(stripped)
        if m:
            actor_name = m.group("actor")
            ssa = m.group("ssa")
            state_raw = m.group("state")
            func_ssa_map[current_func][ssa] = actor_name

            if actor_name not in actors:
                actors[actor_name] = ActorInfo(
                    name=actor_name,
                    state_type_raw=state_raw,
                    state_fields=parse_state_type(state_raw),
                )
            elif not actors[actor_name].state_type_raw:
                actors[actor_name].state_type_raw = state_raw
                actors[actor_name].state_fields = parse_state_type(state_raw)

            # Determine source: is it main, or an actor handler?
            source, is_actor = _resolve_source(current_func, actors)
            spawns.append(SpawnEdge(
                source=source, target=actor_name, source_is_actor=is_actor,
            ))
            continue

        # Actor send
        m = RE_ACTOR_SEND.search(stripped)
        if m:
            target_ssa = m.group("target")
            msg_type = int(m.group("msg"))
            types_str = m.group("types")
            # Extract param types (skip first which is the actor ref)
            all_types = [t.strip() for t in types_str.split(",") if t.strip()]
            param_types = [simplify_type(t) for t in all_types[1:]]

            target_actor = func_ssa_map.get(current_func, {}).get(target_ssa, "")
            source, is_actor = _resolve_source(current_func, actors)

            sends.append(SendEdge(
                source=source,
                target_actor=target_actor if target_actor else target_ssa,
                handler_name="",  # resolved later
                param_types=param_types,
                source_is_actor=is_actor,
            ))
            # Store msg_type for later resolution
            sends[-1]._msg_type = msg_type  # type: ignore[attr-defined]
            sends[-1]._target_dispatch = ""  # type: ignore[attr-defined]
            if target_actor:
                sends[-1]._target_dispatch = f"{target_actor}_dispatch"  # type: ignore[attr-defined]
            continue

        # Actor ask
        m = RE_ACTOR_ASK.search(stripped)
        if m:
            target_ssa = m.group("target")
            msg_type = int(m.group("msg"))
            ret_type = simplify_type(m.group("ret"))
            types_str = m.group("types")
            all_types = [t.strip() for t in types_str.split(",") if t.strip()]
            param_types = [simplify_type(t) for t in all_types[1:]]
            # Also store the result SSA
            result_ssa = m.group("ssa")

            target_actor = func_ssa_map.get(current_func, {}).get(target_ssa, "")
            source, is_actor = _resolve_source(current_func, actors)

            asks.append(AskEdge(
                source=source,
                target_actor=target_actor if target_actor else target_ssa,
                handler_name="",
                param_types=param_types,
                return_type=ret_type,
                source_is_actor=is_actor,
            ))
            asks[-1]._msg_type = msg_type  # type: ignore[attr-defined]
            asks[-1]._target_dispatch = ""  # type: ignore[attr-defined]
            if target_actor:
                asks[-1]._target_dispatch = f"{target_actor}_dispatch"  # type: ignore[attr-defined]
            continue

        # Actor stop
        m = RE_ACTOR_STOP.search(stripped)
        if m:
            target_ssa = m.group("target")
            target_actor = func_ssa_map.get(current_func, {}).get(target_ssa, "")
            source, _ = _resolve_source(current_func, actors)
            if target_actor:
                stops.append((source, target_actor))
            continue

    # Detect supervisors: actors whose dispatch functions contain
    # "supervisor" in their name or that spawn multiple child actors
    for dispatch_func in dispatch_funcs:
        actor_name = dispatch_func.replace("_dispatch", "")
        # Heuristic: if the function name contains "supervisor" or if the actor
        # spawns other actors in its handlers, mark as supervisor
        if "supervisor" in actor_name.lower() or "supervisor" in dispatch_func.lower():
            supervisor_names.add(actor_name)

    # Build supervisor edges from spawn edges where the source is a supervisor
    for sp in spawns:
        if sp.source in supervisor_names:
            supervisor_edges.append(SupervisorEdge(
                supervisor=sp.source,
                child=sp.target,
            ))

    # Resolve handler mappings from dispatch functions
    for dispatch_func, handler_names in dispatch_handlers.items():
        actor_name = dispatch_func.replace("_dispatch", "")
        if actor_name in actors:
            handlers = []
            for full_name in handler_names:
                if full_name in handler_sigs:
                    handlers.append(handler_sigs[full_name])
                else:
                    short = full_name.replace(f"{actor_name}_", "", 1)
                    handlers.append(HandlerInfo(name=short, full_name=full_name))
            actors[actor_name].handlers = handlers
            actors[actor_name].handler_order = handler_names

    # Also ensure actors discovered only via dispatch (no spawn) are included
    for dispatch_func in dispatch_funcs:
        actor_name = dispatch_func.replace("_dispatch", "")
        if actor_name not in actors:
            actors[actor_name] = ActorInfo(name=actor_name)
            if dispatch_func in dispatch_handlers:
                handlers = []
                for full_name in dispatch_handlers[dispatch_func]:
                    if full_name in handler_sigs:
                        handlers.append(handler_sigs[full_name])
                    else:
                        short = full_name.replace(f"{actor_name}_", "", 1)
                        handlers.append(HandlerInfo(name=short, full_name=full_name))
                actors[actor_name].handlers = handlers
                actors[actor_name].handler_order = dispatch_handlers[dispatch_func]

    # Resolve msg_type → handler name for sends and asks
    for edge in sends:
        msg_type = getattr(edge, "_msg_type", -1)
        target_dispatch = getattr(edge, "_target_dispatch", "")
        if target_dispatch and target_dispatch in dispatch_handlers:
            handler_list = dispatch_handlers[target_dispatch]
            if 0 <= msg_type < len(handler_list):
                full_name = handler_list[msg_type]
                if full_name in handler_sigs:
                    edge.handler_name = handler_sigs[full_name].name
                else:
                    edge.handler_name = full_name.split("_", 1)[-1] if "_" in full_name else full_name

    for edge in asks:
        msg_type = getattr(edge, "_msg_type", -1)
        target_dispatch = getattr(edge, "_target_dispatch", "")
        if target_dispatch and target_dispatch in dispatch_handlers:
            handler_list = dispatch_handlers[target_dispatch]
            if 0 <= msg_type < len(handler_list):
                full_name = handler_list[msg_type]
                if full_name in handler_sigs:
                    edge.handler_name = handler_sigs[full_name].name
                    if not edge.return_type:
                        edge.return_type = handler_sigs[full_name].return_type
                else:
                    edge.handler_name = full_name.split("_", 1)[-1] if "_" in full_name else full_name

    # Deduplicate edges (same source→target with same handler)
    sends = _dedup_sends(sends)
    asks = _dedup_asks(asks)

    return actors, spawns, sends, asks, stops, supervisor_edges, supervisor_names


def _resolve_source(func_name: str, actors: dict[str, ActorInfo]) -> tuple[str, bool]:
    """Determine the logical source (main or actor name) from a function name."""
    if func_name == "main":
        return "main", False
    # Check if this function is an actor handler
    for actor_name in actors:
        if func_name.startswith(actor_name + "_") and not func_name.endswith("_dispatch"):
            return actor_name, True
    # Might be a handler for an actor not yet discovered — check prefix pattern
    parts = func_name.rsplit("_", 1)
    if len(parts) == 2:
        # Could be ActorName_handler — we'll check when actors are fully discovered
        return func_name, False
    return func_name, False


def _dedup_sends(sends: list[SendEdge]) -> list[SendEdge]:
    seen: set[tuple[str, str, str]] = set()
    result = []
    for s in sends:
        key = (s.source, s.target_actor, s.handler_name)
        if key not in seen:
            seen.add(key)
            result.append(s)
    return result


def _dedup_asks(asks: list[AskEdge]) -> list[AskEdge]:
    seen: set[tuple[str, str, str]] = set()
    result = []
    for a in asks:
        key = (a.source, a.target_actor, a.handler_name)
        if key not in seen:
            seen.add(key)
            result.append(a)
    return result


# ── DOT generation ───────────────────────────────────────────────────────────


DOT_RESERVED = frozenset({
    "node", "edge", "graph", "digraph", "subgraph", "strict",
})


def sanitize_id(s: str) -> str:
    sid = re.sub(r"[^a-zA-Z0-9_]", "_", s)
    if sid.lower() in DOT_RESERVED:
        sid = f'"{sid}"'
    return sid


def escape_dot(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace('"', "&quot;")


def generate_dot(
    actors: dict[str, ActorInfo],
    spawns: list[SpawnEdge],
    sends: list[SendEdge],
    asks: list[AskEdge],
    stops: list[tuple[str, str]],
    supervisor_edges: list[SupervisorEdge] | None = None,
    supervisor_names: set[str] | None = None,
    *,
    show_types: bool = True,
    show_state: bool = True,
) -> str:
    if supervisor_edges is None:
        supervisor_edges = []
    if supervisor_names is None:
        supervisor_names = set()
    lines = [
        "digraph actor_topology {",
        '  rankdir=LR;',
        '  fontname="Helvetica";',
        '  node [fontname="Helvetica", fontsize=10];',
        '  edge [fontname="Helvetica", fontsize=9];',
        '  compound=true;',
        "",
    ]

    # Collect all sources that aren't actors (main, other functions)
    actor_names = set(actors.keys())
    non_actor_sources: set[str] = set()
    for sp in spawns:
        if sp.source not in actor_names:
            non_actor_sources.add(sp.source)
    for se in sends:
        if se.source not in actor_names:
            non_actor_sources.add(se.source)
    for ae in asks:
        if ae.source not in actor_names:
            non_actor_sources.add(ae.source)

    # ── Entry point / non-actor nodes ────────────────────────────────
    for src in sorted(non_actor_sources):
        sid = sanitize_id(src)
        if src == "main":
            lines.append(
                f'  {sid} [label=<<B>main</B>>, shape=doubleoctagon, '
                f'style=filled, fillcolor="#FFF9C4", color="#F9A825", '
                f'penwidth=2];'
            )
        else:
            lines.append(
                f'  {sid} [label=<<B>fn {escape_dot(src)}</B>>, shape=box, '
                f'style="filled,rounded", fillcolor="#F5F5F5", color="#BDBDBD"];'
            )
    if non_actor_sources:
        lines.append("")

    # ── Actor nodes ──────────────────────────────────────────────────
    for actor_name, actor in sorted(actors.items()):
        aid = sanitize_id(actor_name)

        label_parts = [
            '<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="2" CELLPADDING="2">',
            f'<TR><TD COLSPAN="2"><B>{escape_dot(actor_name)}</B></TD></TR>',
        ]

        # State fields
        if show_state and actor.state_fields:
            # If all field names are positional (field0, field1, ...), show types only
            all_positional = all(
                re.fullmatch(r"field\d+", name) for name, _ in actor.state_fields
            )
            if all_positional:
                fields_str = ", ".join(typ for _, typ in actor.state_fields)
                display = f"({escape_dot(fields_str)})"
            else:
                fields_str = ", ".join(
                    f"{name}: {typ}" for name, typ in actor.state_fields
                )
                display = f"{{{escape_dot(fields_str)}}}"
            label_parts.append(
                f'<TR><TD COLSPAN="2"><FONT POINT-SIZE="8" COLOR="#666666">'
                f'{display}</FONT></TD></TR>'
            )
            label_parts.append('<HR/>')
        elif show_state and not actor.state_fields and actor.state_type_raw:
            # Extract types from raw LLVM struct when fields weren't parsed
            m_raw = re.search(r'\(([^)]*)\)', actor.state_type_raw)
            if m_raw and m_raw.group(1).strip():
                raw_types = [simplify_type(t.strip()) for t in m_raw.group(1).split(",")]
                fields_str = ", ".join(raw_types)
                label_parts.append(
                    f'<TR><TD COLSPAN="2"><FONT POINT-SIZE="8" COLOR="#666666">'
                    f'({escape_dot(fields_str)})</FONT></TD></TR>'
                )
                label_parts.append('<HR/>')

        # Handlers
        if actor.handlers:
            for handler in actor.handlers:
                handler_label = escape_dot(handler.name)
                if show_types and handler.params:
                    p_str = ", ".join(handler.params)
                    handler_label += f'<FONT COLOR="#888888">({escape_dot(p_str)})</FONT>'
                elif show_types:
                    handler_label += '<FONT COLOR="#888888">()</FONT>'
                if show_types and handler.return_type:
                    handler_label += f'<FONT COLOR="#2196F3"> → {escape_dot(handler.return_type)}</FONT>'
                label_parts.append(
                    f'<TR><TD ALIGN="LEFT" COLSPAN="2">'
                    f'<FONT FACE="Courier">⬡</FONT> {handler_label}</TD></TR>'
                )
        else:
            label_parts.append(
                '<TR><TD COLSPAN="2"><FONT COLOR="#999999"><I>no handlers</I></FONT></TD></TR>'
            )

        label_parts.append("</TABLE>")
        label_html = "".join(label_parts)

        if actor_name in supervisor_names:
            fillcolor, border_color = "#E8DAEF", "#7B1FA2"
        else:
            fillcolor, border_color = "#E3F2FD", "#1565C0"
        lines.append(
            f'  {aid} [label=<{label_html}>, shape=box, '
            f'style="filled,rounded", fillcolor="{fillcolor}", color="{border_color}", '
            f'penwidth=1.5];'
        )

    lines.append("")

    # ── Spawn edges (solid blue) ─────────────────────────────────────
    # Count spawn edges by (source, target) pair for deduplication
    spawn_counts: dict[tuple[str, str], int] = {}
    for sp in spawns:
        key = (sp.source, sp.target)
        spawn_counts[key] = spawn_counts.get(key, 0) + 1
    for (source, target), count in spawn_counts.items():
        src = sanitize_id(source)
        tgt = sanitize_id(target)
        label = f"spawn ×{count}" if count > 1 else "spawn"
        lines.append(
            f'  {src} -> {tgt} [label="{label}", color="#2196F3", '
            f'fontcolor="#1565C0", style=bold, penwidth=1.5];'
        )

    # ── Send edges (dashed orange) ───────────────────────────────────
    for se in sends:
        src = sanitize_id(se.source)
        tgt = sanitize_id(se.target_actor)
        label = f".{se.handler_name}" if se.handler_name else "send"
        if show_types and se.param_types:
            p_str = ", ".join(se.param_types)
            label += f"({p_str})"
        elif se.handler_name:
            label += "()"
        lines.append(
            f'  {src} -> {tgt} [label="{escape_dot(label)}", color="#FF9800", '
            f'fontcolor="#E65100", style=dashed];'
        )

    # ── Ask edges (bold pink) ────────────────────────────────────────
    for ae in asks:
        src = sanitize_id(ae.source)
        tgt = sanitize_id(ae.target_actor)
        label = f".{ae.handler_name}" if ae.handler_name else "ask"
        if show_types and ae.param_types:
            p_str = ", ".join(ae.param_types)
            label += f"({p_str})"
        elif ae.handler_name:
            label += "()"
        if show_types and ae.return_type:
            label += f" → {ae.return_type}"
        lines.append(
            f'  {src} -> {tgt} [label="{escape_dot(label)}", color="#E91E63", '
            f'fontcolor="#880E4F", style=bold, penwidth=2];'
        )

    # ── Stop edges (dotted red) ──────────────────────────────────────
    for source, target in stops:
        src = sanitize_id(source)
        tgt = sanitize_id(target)
        lines.append(
            f'  {src} -> {tgt} [label="stop", color="#F44336", '
            f'fontcolor="#B71C1C", style=dotted, arrowhead=tee];'
        )

    # ── Supervisor containment edges (dashed purple) ──────────────────
    seen_supervisor: set[tuple[str, str]] = set()
    for se in supervisor_edges:
        key = (se.supervisor, se.child)
        if key in seen_supervisor:
            continue
        seen_supervisor.add(key)
        src = sanitize_id(se.supervisor)
        tgt = sanitize_id(se.child)
        label = f"supervises ({se.strategy})" if se.strategy else "supervises"
        lines.append(
            f'  {src} -> {tgt} [label="{escape_dot(label)}", color="#7B1FA2", '
            f'fontcolor="#7B1FA2", style=dashed, penwidth=1.5];'
        )

    lines.append("}")
    return "\n".join(lines)


# ── CLI ──────────────────────────────────────────────────────────────────────


def main():
    parser = argparse.ArgumentParser(
        prog="hew-actor-topo",
        description="Visualize the actor system topology from Hew MLIR dialect IR.",
        epilog="Example: hew build --emit-mlir prog.hew | %(prog)s -f svg -o topo.svg",
    )
    parser.add_argument(
        "input",
        nargs="?",
        default="-",
        help="Input MLIR file (default: stdin)",
    )
    parser.add_argument(
        "-o", "--output",
        default=None,
        help="Output file (default: stdout for DOT, required for rendered formats)",
    )
    parser.add_argument(
        "-f", "--format",
        default="dot",
        choices=["dot", "svg", "png", "pdf"],
        help="Output format (default: dot)",
    )
    parser.add_argument(
        "--no-types",
        action="store_true",
        help="Hide type annotations for a cleaner view",
    )
    parser.add_argument(
        "--no-state",
        action="store_true",
        help="Hide state type information from actor boxes",
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

    # Parse and generate DOT
    actors, spawns, sends, asks, stops, supervisor_edges, supervisor_names = parse_mlir(mlir_text)

    if not actors:
        print("Warning: no actors found in input", file=sys.stderr)

    dot = generate_dot(
        actors, spawns, sends, asks, stops,
        supervisor_edges, supervisor_names,
        show_types=not args.no_types,
        show_state=not args.no_state,
    )

    # Output
    if args.format == "dot":
        if args.output:
            with open(args.output, "w") as f:
                f.write(dot)
        else:
            print(dot)
    else:
        is_binary = args.format in ("png", "pdf")
        try:
            result = subprocess.run(
                ["dot", f"-T{args.format}"],
                input=dot.encode() if is_binary else dot,
                capture_output=True,
                text=not is_binary,
                check=True,
            )
        except FileNotFoundError:
            print(
                "Error: 'dot' command not found. Install Graphviz: apt install graphviz",
                file=sys.stderr,
            )
            sys.exit(1)
        except subprocess.CalledProcessError as e:
            stderr = e.stderr if isinstance(e.stderr, str) else e.stderr.decode()
            print(f"Error: Graphviz failed: {stderr}", file=sys.stderr)
            sys.exit(1)

        if args.output:
            mode = "wb" if is_binary else "w"
            with open(args.output, mode) as f:
                f.write(result.stdout)
        else:
            if is_binary:
                sys.stdout.buffer.write(result.stdout)
            else:
                sys.stdout.write(result.stdout)


if __name__ == "__main__":
    main()
