#!/usr/bin/env python3
"""
hew-ir-viz — Visualize Hew MLIR dialect IR as a Graphviz diagram.

Usage:
    hew build --emit-mlir program.hew | python scripts/viz/ir.py > out.dot
    hew build --emit-mlir program.hew | python scripts/viz/ir.py -f svg -o out.svg
    python scripts/viz/ir.py input.mlir -o out.png

The tool reads Hew dialect MLIR from a file or stdin and produces a diagram
showing functions, actors, message flows, and key operations.
"""

from __future__ import annotations

import argparse
import re
import subprocess
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


# ── Parser ───────────────────────────────────────────────────────────────────

# Patterns
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
)
RE_ACTOR_STOP = re.compile(r"hew\.actor_stop\s+(?P<target>%\S+)")
RE_ACTOR_CLOSE = re.compile(r"hew\.actor_close\s+(?P<target>%\S+)")
RE_SCHED_INIT = re.compile(r"hew\.sched\.init")
RE_SCHED_SHUTDOWN = re.compile(r"hew\.sched\.shutdown")
RE_VEC_NEW = re.compile(r"(?P<ssa>%\S+)\s*=\s*hew\.vec\.new.*:\s*(?P<ty>!hew\.vec<[^>]+>)")
RE_HASHMAP_NEW = re.compile(
    r"(?P<ssa>%\S+)\s*=\s*hew\.hashmap\.new.*:\s*(?P<ty>!hew\.hashmap<[^>]+>)"
)
RE_VEC_OP = re.compile(r"hew\.vec\.(?P<op>\w+)")
RE_HASHMAP_OP = re.compile(r"hew\.hashmap\.(?P<op>\w+)")
RE_PRINT = re.compile(r"hew\.print\s+(?P<val>%\S+)")
RE_STRING_CONCAT = re.compile(r"hew\.string_concat")
RE_ENUM_CONSTRUCT = re.compile(
    r'hew\.enum_construct.*enum_name\s*=\s*"(?P<enum>[^"]+)"'
    r'.*variant_name\s*=\s*"(?P<variant>[^"]+)"'
)
RE_RUNTIME_CALL = re.compile(r'hew\.runtime_call\s+@(?P<fn>\w+)')
RE_RECEIVE = re.compile(r"hew\.receive")
RE_SLEEP = re.compile(r"hew\.sleep")
RE_CONSTANT = re.compile(r'hew\.constant\s+"(?P<sym>\w+)"')
RE_SCOPE_IF = re.compile(r"scf\.if\s")
RE_SCOPE_WHILE = re.compile(r"scf\.while\s")
RE_FUNC_CALL = re.compile(r"(?:func\.)?call\s+@(?P<fn>\w+)")
RE_REGEX_OP = re.compile(r"hew\.regex\.(?P<op>\w+)")
RE_SUPERVISOR_OP = re.compile(r"hew\.supervisor\.(?P<op>\w+)")
RE_CLOSE_BRACE = re.compile(r"^\s*\}")


def parse_mlir(text: str):
    """Parse MLIR text and extract structural information."""
    funcs: dict[str, FuncInfo] = {}
    actors: dict[str, ActorInfo] = {}
    spawns: list[SpawnEdge] = []
    sends: list[SendEdge] = []
    asks: list[AskEdge] = []
    global_strings: dict[str, str] = {}
    # Track SSA → actor name for send/ask resolution
    ssa_to_actor: dict[str, str] = {}

    current_func: FuncInfo | None = None
    brace_depth = 0

    for line in text.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("//"):
            continue

        # Track brace depth for function boundaries
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
            continue

        # Function end (top-level brace close)
        if brace_depth <= 1 and RE_CLOSE_BRACE.match(stripped):
            current_func = None
            continue

        if current_func is None:
            continue

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
            asks.append(AskEdge(source_func=fn_name, target_ssa=target_actor, msg_type=msg))
            current_func.ops.append(f"ask msg#{msg}")
            continue

        # Actor stop/close
        if RE_ACTOR_STOP.search(stripped):
            current_func.ops.append("actor_stop")
            continue
        if RE_ACTOR_CLOSE.search(stripped):
            current_func.ops.append("actor_close")
            continue

        # Scheduler
        if RE_SCHED_INIT.search(stripped):
            current_func.ops.append("sched_init")
            continue
        if RE_SCHED_SHUTDOWN.search(stripped):
            current_func.ops.append("sched_shutdown")
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

        # Enum construct
        m = RE_ENUM_CONSTRUCT.search(stripped)
        if m:
            current_func.ops.append(f"{m.group('variant')}")
            continue

        # Sleep
        if RE_SLEEP.search(stripped):
            current_func.ops.append("sleep")
            continue

        # String concat (just note it, don't spam)
        if RE_STRING_CONCAT.search(stripped) and "str_concat" not in current_func.ops:
            current_func.ops.append("str_concat")
            continue

        # Regex ops
        m = RE_REGEX_OP.search(stripped)
        if m:
            op = m.group("op")
            label = f"regex.{op}"
            if label not in current_func.ops:
                current_func.ops.append(label)
            continue

        # Supervisor ops
        m = RE_SUPERVISOR_OP.search(stripped)
        if m:
            op = m.group("op")
            label = f"supervisor.{op}"
            if label not in current_func.ops:
                current_func.ops.append(label)
            continue

        # Control flow (note once per function)
        if RE_SCOPE_IF.search(stripped) and "if" not in current_func.ops:
            current_func.ops.append("if")
        if RE_SCOPE_WHILE.search(stripped) and "while" not in current_func.ops:
            current_func.ops.append("while")

        # Function calls
        m = RE_FUNC_CALL.search(stripped)
        if m:
            callee = m.group("fn")
            # Skip noise (runtime helpers, string drops)
            if not callee.startswith("hew_") and callee != fn_name:
                label = f"call @{callee}"
                if label not in current_func.ops:
                    current_func.ops.append(label)

    # Infer actor handlers: functions named ActorName_methodName
    for actor_name, actor in actors.items():
        prefix = actor_name + "_"
        for fname in funcs:
            if fname.startswith(prefix) and fname != f"{actor_name}_dispatch":
                handler = fname[len(prefix):]
                actor.handlers.append(handler)

    return funcs, actors, spawns, sends, asks, global_strings


# ── DOT generation ───────────────────────────────────────────────────────────


def sanitize_id(s: str) -> str:
    return re.sub(r"[^a-zA-Z0-9_]", "_", s)


def escape_dot(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace('"', "&quot;")


def ops_to_rows(ops: list[str], max_rows: int = 12) -> str:
    """Format ops as HTML table rows for DOT labels."""
    if not ops:
        return ""
    display = ops[:max_rows]
    rows = []
    for op in display:
        color = "#666666"
        if op.startswith("spawn"):
            color = "#2196F3"
        elif op.startswith("send") or op.startswith("ask"):
            color = "#FF9800"
        elif op.startswith("vec.") or op.startswith("hashmap."):
            color = "#9C27B0"
        elif op.startswith("call @"):
            color = "#4CAF50"
        elif op.startswith("rt:"):
            color = "#795548"
        elif op.startswith("regex."):
            color = "#E91E63"
        elif op.startswith("supervisor."):
            color = "#7B1FA2"
        elif op in ("if", "while"):
            color = "#607D8B"
        elif op in ("sched_init", "sched_shutdown", "sleep"):
            color = "#F44336"
        rows.append(
            f'<TR><TD ALIGN="LEFT"><FONT COLOR="{color}">'
            f"{escape_dot(op)}</FONT></TD></TR>"
        )
    if len(ops) > max_rows:
        rows.append(
            f'<TR><TD ALIGN="LEFT"><FONT COLOR="#999">... +{len(ops) - max_rows} more</FONT></TD></TR>'
        )
    return "\n".join(rows)


def generate_dot(funcs, actors, spawns, sends, asks, global_strings) -> str:
    lines = [
        "digraph hew_ir {",
        '  rankdir=TB;',
        '  fontname="Helvetica";',
        '  node [fontname="Helvetica", fontsize=10];',
        '  edge [fontname="Helvetica", fontsize=9];',
        '  compound=true;',
        "",
    ]

    # ── Actor subgraphs ──────────────────────────────────────────────
    for actor_name, actor in actors.items():
        sid = sanitize_id(actor_name)
        state_label = escape_dot(actor.state_type) if actor.state_type else ""

        lines.append(f"  subgraph cluster_{sid} {{")
        lines.append(f'    label=<<B>{escape_dot(actor_name)}</B>>;')
        lines.append('    style=filled; fillcolor="#E3F2FD"; color="#1565C0";')
        lines.append('    fontsize=12;')

        if state_label:
            lines.append(
                f'    {sid}_state [label=<<FONT POINT-SIZE="8">{state_label}</FONT>>, '
                f'shape=note, style=filled, fillcolor="#BBDEFB", fontsize=8];'
            )

        # Dispatch node
        dispatch_func = f"{actor_name}_dispatch"
        if dispatch_func in funcs:
            lines.append(
                f'    {sid}_dispatch [label="dispatch", shape=diamond, '
                f'style=filled, fillcolor="#90CAF9", width=0.6, height=0.4, fontsize=9];'
            )

        # Handler nodes
        for handler in actor.handlers:
            hid = sanitize_id(f"{actor_name}_{handler}")
            func_info = funcs.get(f"{actor_name}_{handler}")
            ops_html = ""
            if func_info and func_info.ops:
                ops_html = ops_to_rows(func_info.ops)

            label_parts = [
                f'<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="1">',
                f'<TR><TD><B>{escape_dot(handler)}</B></TD></TR>',
            ]
            if func_info and func_info.args:
                # Show only non-self params
                params = func_info.args
                # Remove the first param (self/state ptr)
                param_parts = [p.strip() for p in params.split(",")]
                if param_parts and param_parts[0].startswith("%arg0"):
                    param_parts = param_parts[1:]
                if param_parts:
                    p_str = ", ".join(param_parts)
                    label_parts.append(
                        f'<TR><TD><FONT POINT-SIZE="8" COLOR="#666">({escape_dot(p_str)})</FONT></TD></TR>'
                    )
            if ops_html:
                label_parts.append('<HR/>')
                label_parts.append(ops_html)
            label_parts.append("</TABLE>")

            lines.append(
                f"    {hid} [label=<{''.join(label_parts)}>, "
                f'shape=box, style="filled,rounded", fillcolor="#C8E6C9"];'
            )
            # Edge from dispatch to handler
            if dispatch_func in funcs:
                lines.append(f"    {sid}_dispatch -> {hid} [style=dashed, color=\"#666\"];")

        lines.append("  }")
        lines.append("")

    # ── Regular function nodes ───────────────────────────────────────
    for fname, finfo in funcs.items():
        if finfo.is_dispatch or finfo.is_private:
            continue
        # Skip actor handler functions (already in subgraph)
        is_handler = False
        for actor_name in actors:
            if fname.startswith(actor_name + "_") and fname != f"{actor_name}_dispatch":
                is_handler = True
                break
        if is_handler:
            continue

        fid = sanitize_id(fname)
        ops_html = ops_to_rows(finfo.ops)

        fillcolor = "#FFF9C4" if fname == "main" else "#F5F5F5"
        border_color = "#F9A825" if fname == "main" else "#BDBDBD"

        label_parts = [
            f'<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="1">',
            f'<TR><TD><B>fn {escape_dot(fname)}</B></TD></TR>',
        ]
        if finfo.args:
            label_parts.append(
                f'<TR><TD><FONT POINT-SIZE="8" COLOR="#666">({escape_dot(finfo.args)})</FONT></TD></TR>'
            )
        if finfo.result:
            label_parts.append(
                f'<TR><TD><FONT POINT-SIZE="8" COLOR="#666">→ {escape_dot(finfo.result)}</FONT></TD></TR>'
            )
        if ops_html:
            label_parts.append('<HR/>')
            label_parts.append(ops_html)
        label_parts.append("</TABLE>")

        lines.append(
            f'  {fid} [label=<{"".join(label_parts)}>, '
            f'shape=box, style="filled,rounded", fillcolor="{fillcolor}", color="{border_color}"];'
        )

    lines.append("")

    # ── Spawn edges ──────────────────────────────────────────────────
    for sp in spawns:
        src = sanitize_id(sp.source_func)
        tgt_actor = sanitize_id(sp.actor_name)
        # Point to dispatch node if it exists
        tgt_node = f"{tgt_actor}_dispatch"
        if f"{sp.actor_name}_dispatch" not in funcs:
            # Point to first handler or state
            if actors[sp.actor_name].handlers:
                tgt_node = sanitize_id(f"{sp.actor_name}_{actors[sp.actor_name].handlers[0]}")
            else:
                tgt_node = f"{tgt_actor}_state"
        lines.append(
            f'  {src} -> {tgt_node} [label="spawn", color="#2196F3", '
            f'fontcolor="#1565C0", style=bold, lhead=cluster_{tgt_actor}];'
        )

    # ── Send edges ───────────────────────────────────────────────────
    for se in sends:
        src = sanitize_id(se.source_func)
        target_actor = se.target_ssa
        if target_actor in actors:
            tgt = sanitize_id(target_actor) + "_dispatch"
            if f"{target_actor}_dispatch" not in funcs:
                if actors[target_actor].handlers:
                    tgt = sanitize_id(f"{target_actor}_{actors[target_actor].handlers[0]}")
            label = f"send msg#{se.msg_type}"
            # Try to resolve msg_type to handler name
            if target_actor in actors and se.msg_type < len(actors[target_actor].handlers):
                handler = actors[target_actor].handlers[se.msg_type]
                label = f"send .{handler}()"
            lines.append(
                f'  {src} -> {tgt} [label="{label}", color="#FF9800", '
                f'fontcolor="#E65100", style=dashed, lhead=cluster_{sanitize_id(target_actor)}];'
            )

    # ── Ask edges ────────────────────────────────────────────────────
    for ae in asks:
        src = sanitize_id(ae.source_func)
        target_actor = ae.target_ssa
        if target_actor in actors:
            tgt = sanitize_id(target_actor) + "_dispatch"
            if f"{target_actor}_dispatch" not in funcs:
                if actors[target_actor].handlers:
                    tgt = sanitize_id(f"{target_actor}_{actors[target_actor].handlers[0]}")
            label = f"ask msg#{ae.msg_type}"
            if target_actor in actors and ae.msg_type < len(actors[target_actor].handlers):
                handler = actors[target_actor].handlers[ae.msg_type]
                label = f"ask .{handler}()"
            lines.append(
                f'  {src} -> {tgt} [label="{label}", color="#E91E63", '
                f'fontcolor="#880E4F", style=bold, lhead=cluster_{sanitize_id(target_actor)}];'
            )

    # ── Function call edges ──────────────────────────────────────────
    for fname, finfo in funcs.items():
        if finfo.is_dispatch or finfo.is_private:
            continue
        src = sanitize_id(fname)
        for op in finfo.ops:
            if op.startswith("call @"):
                callee = op[6:]
                callee_id = sanitize_id(callee)
                if callee in funcs and not funcs[callee].is_private:
                    lines.append(
                        f'  {src} -> {callee_id} [color="#4CAF50", '
                        f'fontcolor="#2E7D32", style=dotted];'
                    )

    lines.append("}")
    return "\n".join(lines)


# ── CLI ──────────────────────────────────────────────────────────────────────


def main():
    parser = argparse.ArgumentParser(
        prog="hew-ir-viz",
        description="Visualize Hew MLIR dialect IR as a Graphviz diagram.",
        epilog="Example: hew build --emit-mlir prog.hew | %(prog)s -f svg -o prog.svg",
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
    funcs, actors, spawns, sends, asks, global_strings = parse_mlir(mlir_text)
    dot = generate_dot(funcs, actors, spawns, sends, asks, global_strings)

    # Output
    if args.format == "dot":
        if args.output:
            with open(args.output, "w") as f:
                f.write(dot)
        else:
            print(dot)
    else:
        # Render with Graphviz
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
