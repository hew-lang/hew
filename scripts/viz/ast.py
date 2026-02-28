#!/usr/bin/env python3
"""Generate Graphviz diagrams from Hew AST JSON (hew build --emit-ast).

Usage:
    hew build --emit-ast prog.hew | python3 scripts/viz/ast.py > out.dot
    hew build --emit-ast prog.hew | python3 scripts/viz/ast.py -f svg -o out.svg
    python3 scripts/viz/ast.py input.json -o out.png
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from dataclasses import dataclass, field


# ---------------------------------------------------------------------------
# Type formatting
# ---------------------------------------------------------------------------

def fmt_type(ty) -> str:
    """Format a type annotation to a readable Hew-level string."""
    if ty is None:
        return ""
    # ty is [type_content, span]
    if isinstance(ty, list) and len(ty) == 2 and isinstance(ty[1], dict):
        return fmt_type_content(ty[0])
    return fmt_type_content(ty)


def fmt_type_content(tc) -> str:
    if tc is None:
        return ""
    if isinstance(tc, str):
        return tc
    if isinstance(tc, dict):
        if "Named" in tc:
            n = tc["Named"]
            name = n["name"]
            targs = n.get("type_args")
            if targs:
                args = ", ".join(fmt_type(a) for a in targs)
                return f"{name}<{args}>"
            return name
        if "ActorRef" in tc:
            ar = tc["ActorRef"]
            name = ar.get("name", "?")
            return f"ActorRef<{name}>"
        if "Tuple" in tc:
            elems = tc["Tuple"]
            parts = ", ".join(fmt_type(e) for e in elems)
            return f"({parts})"
        if "Function" in tc:
            f = tc["Function"]
            params = ", ".join(fmt_type(p) for p in f.get("params", []))
            ret = fmt_type(f.get("return_type"))
            return f"fn({params}) -> {ret}" if ret else f"fn({params})"
        if "Result" in tc:
            r = tc["Result"]
            ok = fmt_type(r.get("ok"))
            err = fmt_type(r.get("err"))
            return f"Result<{ok}, {err}>"
        if "Option" in tc:
            inner = fmt_type(tc["Option"])
            return f"Option<{inner}>"
        if "Reference" in tc:
            inner = fmt_type(tc["Reference"])
            return f"&{inner}"
        if "Array" in tc:
            a = tc["Array"]
            return f"[{fmt_type(a.get('element_type'))}; {a.get('size', '?')}]"
    return str(tc)


def fmt_param(p) -> str:
    name = p.get("name", "?")
    ty = fmt_type(p.get("ty"))
    return f"{name}: {ty}" if ty else name


def fmt_params(params: list) -> str:
    return ", ".join(fmt_param(p) for p in params)


# ---------------------------------------------------------------------------
# Data structures for graph elements
# ---------------------------------------------------------------------------

@dataclass
class ActorInfo:
    name: str
    fields: list[tuple[str, str]]  # (name, type_str)
    handlers: list[tuple[str, str]]  # (name, params_str)


@dataclass
class FuncInfo:
    name: str
    params_str: str
    return_type: str
    is_async: bool
    operations: list[str]


@dataclass
class TypeDeclInfo:
    name: str
    fields: list[tuple[str, str]]


@dataclass
class Edge:
    src: str
    dst: str
    label: str
    kind: str  # "spawn", "send", "ask", "call"


# ---------------------------------------------------------------------------
# AST walking
# ---------------------------------------------------------------------------

class ASTWalker:
    def __init__(self, ast: dict):
        self.ast = ast
        self.actors: dict[str, ActorInfo] = {}
        self.functions: dict[str, FuncInfo] = {}
        self.type_decls: dict[str, TypeDeclInfo] = {}
        self.edges: list[Edge] = []
        # Track actor names for spawn resolution
        self.actor_names: set[str] = set()

    def walk(self):
        for item_and_span in self.ast.get("items", []):
            item, _span = item_and_span
            if "Actor" in item:
                self._process_actor(item["Actor"])
            elif "Function" in item:
                self._process_function(item["Function"])
            elif "TypeDecl" in item:
                self._process_type_decl(item["TypeDecl"])
            elif "Supervisor" in item:
                self._process_supervisor(item["Supervisor"])
            elif "Wire" in item:
                self._process_wire(item["Wire"])
            # Import, TraitDef, ImplBlock — skip for visualization

        # Walk bodies for edges
        for item_and_span in self.ast.get("items", []):
            item, _span = item_and_span
            if "Function" in item:
                fn = item["Function"]
                ctx = _BodyContext(fn["name"], self)
                self._walk_body(fn.get("body"), ctx)
                self.functions[fn["name"]].operations = ctx.operations
            elif "Actor" in item:
                actor = item["Actor"]
                for handler in actor.get("receive_fns", []):
                    ctx = _BodyContext(actor["name"], self)
                    self._walk_body(handler.get("body"), ctx)

    def _process_actor(self, actor: dict):
        name = actor["name"]
        self.actor_names.add(name)
        fields = []
        for f in actor.get("fields", []):
            fields.append((f["name"], fmt_type(f.get("ty"))))
        handlers = []
        for h in actor.get("receive_fns", []):
            ps = fmt_params(h.get("params", []))
            handlers.append((h["name"], ps))
        self.actors[name] = ActorInfo(name, fields, handlers)

    def _process_function(self, fn: dict):
        name = fn["name"]
        ps = fmt_params(fn.get("params", []))
        rt = fmt_type(fn.get("return_type"))
        is_async = fn.get("is_async", False)
        self.functions[name] = FuncInfo(name, ps, rt, is_async, [])

    def _process_type_decl(self, td: dict):
        name = td.get("name", "?")
        fields = []
        for f in td.get("fields", []):
            fields.append((f["name"], fmt_type(f.get("ty"))))
        self.type_decls[name] = TypeDeclInfo(name, fields)

    def _process_supervisor(self, sd: dict):
        name = sd.get("name", "?")
        strategy = sd.get("strategy", "one_for_one")
        children = []
        for child in sd.get("children", []):
            children.append(child.get("actor_type", "?"))
            # Track supervised actors for spawn edges
            actor_type = child.get("actor_type", "")
            if actor_type:
                self.edges.append(Edge(name, actor_type, f"supervises ({strategy})", "spawn"))
        # Represent supervisor as an actor-like node with children info
        handlers = [(f"strategy: {strategy}", "")]
        for c in children:
            handlers.append((f"child: {c}", ""))
        self.actors[name] = ActorInfo(name, [], handlers)
        self.actor_names.add(name)

    def _process_wire(self, wd: dict):
        name = wd.get("name", "?")
        fields = []
        for f in wd.get("fields", []):
            fields.append((f.get("name", "?"), f.get("ty", "?")))
        self.type_decls[name] = TypeDeclInfo(name, fields)

    def _walk_body(self, body, ctx: _BodyContext):
        if body is None:
            return
        for stmt_and_span in body.get("stmts", []):
            stmt, _span = stmt_and_span
            self._walk_stmt(stmt, ctx)
        trailing = body.get("trailing_expr")
        if trailing:
            self._walk_expr(trailing, ctx)

    def _walk_stmt(self, stmt: dict, ctx: _BodyContext):
        if "Let" in stmt:
            let = stmt["Let"]
            value = let.get("value")
            if value:
                self._walk_expr(value, ctx)
                # Track spawn bindings
                val_content = value[0] if isinstance(value, list) and value else value
                if isinstance(val_content, dict) and "Spawn" in val_content:
                    spawn = val_content["Spawn"]
                    target = self._extract_spawn_target(spawn)
                    var_name = self._extract_pattern_name(let.get("pattern"))
                    if var_name and target:
                        ctx.spawn_bindings[var_name] = target
        elif "Expression" in stmt:
            expr = stmt["Expression"]
            self._walk_expr(expr, ctx)
        elif "If" in stmt:
            iff = stmt["If"]
            self._walk_expr(iff.get("condition"), ctx)
            self._walk_body(iff.get("then_block"), ctx)
            else_block = iff.get("else_block")
            if else_block and isinstance(else_block, dict):
                if else_block.get("block"):
                    self._walk_body(else_block["block"], ctx)
                if else_block.get("if_stmt"):
                    self._walk_stmt(else_block["if_stmt"], ctx)
        elif "For" in stmt:
            f = stmt["For"]
            self._walk_expr(f.get("iterable"), ctx)
            self._walk_body(f.get("body"), ctx)
        elif "While" in stmt:
            w = stmt["While"]
            self._walk_expr(w.get("condition"), ctx)
            self._walk_body(w.get("body"), ctx)
        elif "Match" in stmt:
            m = stmt["Match"]
            self._walk_expr(m.get("expr"), ctx)
            for arm in m.get("arms", []):
                if isinstance(arm, dict):
                    self._walk_expr(arm.get("body"), ctx)
                    self._walk_body(arm.get("block"), ctx)
        elif "Return" in stmt:
            ret = stmt["Return"]
            val = ret.get("value") if isinstance(ret, dict) else ret
            if val:
                self._walk_expr(val, ctx)
        elif "Block" in stmt:
            self._walk_body(stmt["Block"], ctx)
        elif "Assign" in stmt:
            a = stmt["Assign"]
            self._walk_expr(a.get("value"), ctx)
        elif "Defer" in stmt:
            self._walk_expr(stmt["Defer"], ctx)
        elif "Var" in stmt:
            var = stmt["Var"]
            value = var.get("value")
            if value:
                self._walk_expr(value, ctx)

    def _walk_expr(self, expr, ctx: _BodyContext):
        if expr is None:
            return
        # expr is [expr_content, span]
        if isinstance(expr, list) and len(expr) == 2 and isinstance(expr[1], dict):
            ec = expr[0]
        else:
            ec = expr
        if not isinstance(ec, dict):
            return

        if "Spawn" in ec:
            spawn = ec["Spawn"]
            target = self._extract_spawn_target(spawn)
            if target and target in self.actor_names:
                self.edges.append(Edge(ctx.owner, target, "spawn", "spawn"))
                ctx.operations.append(f"spawn {target}")
            for _name, val in spawn.get("args", []):
                self._walk_expr(val, ctx)

        elif "MethodCall" in ec:
            mc = ec["MethodCall"]
            recv = mc.get("receiver", mc.get("object"))
            method = mc["method"]
            recv_name = self._expr_ident(recv)
            # Format args for label
            arg_strs = self._format_call_args(mc.get("args", []))
            label = f".{method}({arg_strs})"
            if recv_name and recv_name in ctx.spawn_bindings:
                actor_target = ctx.spawn_bindings[recv_name]
                if actor_target in self.actor_names:
                    self.edges.append(Edge(ctx.owner, actor_target, label, "send"))
                    ctx.operations.append(f"{recv_name}{label}")
            self._walk_expr(recv, ctx)
            for a in mc.get("args", []):
                self._walk_call_arg(a, ctx)

        elif "Await" in ec:
            inner = ec["Await"]
            # inner is [expr_content, span]
            if isinstance(inner, list) and len(inner) == 2 and isinstance(inner[1], dict):
                inner_content = inner[0]
            else:
                inner_content = inner
            if isinstance(inner_content, dict) and "MethodCall" in inner_content:
                mc = inner_content["MethodCall"]
                recv = mc.get("receiver", mc.get("object"))
                method = mc["method"]
                recv_name = self._expr_ident(recv)
                arg_strs = self._format_call_args(mc.get("args", []))
                label = f"await .{method}({arg_strs})"
                if recv_name and recv_name in ctx.spawn_bindings:
                    actor_target = ctx.spawn_bindings[recv_name]
                    if actor_target in self.actor_names:
                        # Replace any existing send edge with ask edge
                        self.edges.append(Edge(ctx.owner, actor_target, label, "ask"))
                        ctx.operations.append(f"await {recv_name}.{method}()")
                for a in mc.get("args", []):
                    self._walk_call_arg(a, ctx)
                self._walk_expr(recv, ctx)
            else:
                self._walk_expr(inner, ctx)

        elif "Call" in ec:
            call = ec["Call"]
            fn_name = self._expr_ident(call.get("function"))
            if fn_name and fn_name in self.functions and fn_name != ctx.owner:
                arg_strs = self._format_call_args(call.get("args", []))
                label = f"{fn_name}({arg_strs})"
                self.edges.append(Edge(ctx.owner, fn_name, label, "call"))
                ctx.operations.append(f"call {fn_name}()")
            self._walk_expr(call.get("function"), ctx)
            for a in call.get("args", []):
                self._walk_call_arg(a, ctx)

        elif "Binary" in ec:
            b = ec["Binary"]
            self._walk_expr(b.get("left"), ctx)
            self._walk_expr(b.get("right"), ctx)

        elif "If" in ec:
            iff = ec["If"]
            self._walk_expr(iff.get("condition"), ctx)
            self._walk_body(iff.get("then_block"), ctx)
            else_block = iff.get("else_block")
            if else_block and isinstance(else_block, dict):
                if else_block.get("block"):
                    self._walk_body(else_block["block"], ctx)

        elif "Block" in ec:
            self._walk_body(ec["Block"], ctx)

        elif "Closure" in ec:
            c = ec["Closure"]
            self._walk_body(c.get("body"), ctx)

        elif "InterpolatedString" in ec:
            for part in ec["InterpolatedString"]:
                if isinstance(part, dict) and "Expr" in part:
                    self._walk_expr(part["Expr"], ctx)

        elif "Unary" in ec:
            self._walk_expr(ec["Unary"].get("expr"), ctx)

        elif "Index" in ec:
            self._walk_expr(ec["Index"].get("object"), ctx)
            self._walk_expr(ec["Index"].get("index"), ctx)

        elif "FieldAccess" in ec:
            self._walk_expr(ec["FieldAccess"].get("object"), ctx)

        elif "Tuple" in ec:
            for e in ec["Tuple"]:
                self._walk_expr(e, ctx)

        elif "Scope" in ec:
            scope = ec["Scope"]
            self._walk_body(scope.get("body"), ctx)

        elif "ScopeLaunch" in ec:
            self._walk_body(ec["ScopeLaunch"], ctx)

        elif "ScopeSpawn" in ec:
            self._walk_body(ec["ScopeSpawn"], ctx)

        elif "Unsafe" in ec:
            self._walk_body(ec["Unsafe"], ctx)

        elif "Select" in ec:
            sel = ec["Select"]
            for arm in sel.get("arms", []):
                self._walk_expr(arm.get("source"), ctx)
                self._walk_expr(arm.get("body"), ctx)
            timeout = sel.get("timeout")
            if timeout and isinstance(timeout, dict):
                self._walk_expr(timeout.get("duration"), ctx)
                self._walk_expr(timeout.get("body"), ctx)

        elif "Join" in ec:
            for e in ec["Join"]:
                self._walk_expr(e, ctx)

        elif "Timeout" in ec:
            t = ec["Timeout"]
            self._walk_expr(t.get("expr"), ctx)
            self._walk_expr(t.get("duration"), ctx)

        elif "Yield" in ec:
            self._walk_expr(ec["Yield"], ctx)

        elif "PostfixTry" in ec:
            self._walk_expr(ec["PostfixTry"], ctx)

        elif "Range" in ec:
            r = ec["Range"]
            self._walk_expr(r.get("start"), ctx)
            self._walk_expr(r.get("end"), ctx)

        elif "SpawnLambdaActor" in ec:
            sla = ec["SpawnLambdaActor"]
            self._walk_body(sla.get("body"), ctx)
            for a in sla.get("args", []):
                self._walk_call_arg(a, ctx)

        elif "ScopeCancel" in ec or "ScopeIsCancelled" in ec:
            pass  # no sub-expressions to walk

        elif "RegexLiteral" in ec:
            pass  # leaf node, no sub-expressions

    def _walk_call_arg(self, arg, ctx: _BodyContext):
        if isinstance(arg, dict):
            if "Positional" in arg:
                self._walk_expr(arg["Positional"], ctx)
            elif "Named" in arg:
                self._walk_expr(arg["Named"][1], ctx)
        elif isinstance(arg, list):
            # Named arg as [name, expr]
            if len(arg) == 2:
                self._walk_expr(arg[1], ctx)

    def _extract_spawn_target(self, spawn: dict) -> str | None:
        target = spawn.get("target")
        if target is None:
            # Old format: spawn.actor
            return spawn.get("actor")
        return self._expr_ident(target)

    def _extract_pattern_name(self, pattern) -> str | None:
        if pattern is None:
            return None
        if isinstance(pattern, list) and len(pattern) == 2 and isinstance(pattern[1], dict):
            pc = pattern[0]
        else:
            pc = pattern
        if isinstance(pc, dict) and "Identifier" in pc:
            return pc["Identifier"]
        return None

    def _expr_ident(self, expr) -> str | None:
        if expr is None:
            return None
        if isinstance(expr, list) and len(expr) == 2 and isinstance(expr[1], dict):
            ec = expr[0]
        else:
            ec = expr
        if isinstance(ec, dict) and "Identifier" in ec:
            return ec["Identifier"]
        return None

    def _format_call_args(self, args: list) -> str:
        parts = []
        for a in args:
            if isinstance(a, dict) and "Positional" in a:
                parts.append(self._expr_summary(a["Positional"]))
            elif isinstance(a, list) and len(a) == 2:
                parts.append(f"{a[0]}: {self._expr_summary(a[1])}")
            else:
                parts.append("…")
        return ", ".join(parts)

    def _expr_summary(self, expr) -> str:
        """Short summary of an expression for edge labels."""
        if expr is None:
            return "?"
        if isinstance(expr, list) and len(expr) == 2 and isinstance(expr[1], dict):
            ec = expr[0]
        else:
            ec = expr
        if not isinstance(ec, dict):
            return str(ec)
        if "Identifier" in ec:
            return ec["Identifier"]
        if "Literal" in ec:
            lit = ec["Literal"]
            if isinstance(lit, dict):
                if "Integer" in lit:
                    return str(lit["Integer"])
                if "Float" in lit:
                    return str(lit["Float"])
                if "String" in lit:
                    s = lit["String"]
                    return f'"{s}"' if len(s) <= 12 else f'"{s[:10]}…"'
                if "Bool" in lit:
                    return str(lit["Bool"]).lower()
            return str(lit)
        if "Call" in ec:
            fn = self._expr_ident(ec["Call"].get("function"))
            return f"{fn}(…)" if fn else "call(…)"
        if "MethodCall" in ec:
            m = ec["MethodCall"]["method"]
            return f"….{m}(…)"
        if "Binary" in ec:
            return "…"
        return "…"


@dataclass
class _BodyContext:
    owner: str  # function or actor name
    walker: ASTWalker
    spawn_bindings: dict[str, str] = field(default_factory=dict)
    operations: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# DOT generation
# ---------------------------------------------------------------------------

def escape_dot(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace('"', "&quot;")


def generate_dot(walker: ASTWalker) -> str:
    lines = [
        "digraph HewAST {",
        '    rankdir=TB;',
        '    fontname="Helvetica";',
        '    node [fontname="Helvetica", fontsize=11];',
        '    edge [fontname="Helvetica", fontsize=9];',
        "",
    ]

    # Supervisors — purple rounded boxes (subset of actors dict)
    supervisor_names = {name for name, a in walker.actors.items()
                        if any(h[0].startswith("strategy:") for h in a.handlers)}

    # Actors — blue rounded boxes with HTML table labels
    for name, actor in walker.actors.items():
        rows = [f'<tr><td><b>{escape_dot(name)}</b></td></tr>']
        for fn, ft in actor.fields:
            rows.append(f'<tr><td align="left">{escape_dot(fn)}: {escape_dot(ft)}</td></tr>')
        if actor.fields and actor.handlers:
            rows.append('<hr/>')
        for hn, hp in actor.handlers:
            rows.append(
                f'<tr><td align="left">receive {escape_dot(hn)}({escape_dot(hp)})</td></tr>'
            )
        table = f'<table border="0" cellborder="0" cellspacing="0">{"".join(rows)}</table>'
        fillcolor = "#E8DAEF" if name in supervisor_names else "#D6EAF8"
        lines.append(
            f'    "{name}" [shape=box, style="rounded,filled", fillcolor="{fillcolor}", '
            f'label=<{table}>];'
        )

    # Functions — yellow rounded boxes
    for name, fn in walker.functions.items():
        sig = f"fn {name}({escape_dot(fn.params_str)})"
        if fn.is_async:
            sig = f"async {sig}"
        if fn.return_type:
            sig += f" → {escape_dot(fn.return_type)}"
        parts = [f"<b>{sig}</b>"]
        # Deduplicate operations
        seen_ops: set[str] = set()
        for op in fn.operations:
            if op not in seen_ops:
                parts.append(escape_dot(op))
                seen_ops.add(op)
        html = "<br/>".join(parts)
        lines.append(
            f'    "{name}" [shape=box, style="rounded,filled", fillcolor="#FEF9E7", '
            f'label=<{html}>];'
        )

    # Type declarations — gray boxes
    for name, td in walker.type_decls.items():
        parts = [f"<b>type {escape_dot(name)}</b>"]
        for fn, ft in td.fields:
            parts.append(f"{escape_dot(fn)}: {escape_dot(ft)}")
        html = "<br/>".join(parts)
        lines.append(
            f'    "{name}" [shape=box, style="filled", fillcolor="#E5E7E9", '
            f'label=<{html}>];'
        )

    lines.append("")

    # Edges — deduplicate by (src, dst, method_name, kind)
    seen_edges: set[tuple[str, str, str, str]] = set()
    for edge in walker.edges:
        # For send/call edges, deduplicate by method name (strip args)
        dedup_label = edge.label.split("(")[0] if edge.kind in ("send", "call") else edge.label
        key = (edge.src, edge.dst, dedup_label, edge.kind)
        if key in seen_edges:
            continue
        seen_edges.add(key)

        label = escape_dot(edge.label)
        if edge.kind == "spawn":
            lines.append(
                f'    "{edge.src}" -> "{edge.dst}" '
                f'[label="{label}", color="#2E86C1", penwidth=2, style=bold];'
            )
        elif edge.kind == "send":
            lines.append(
                f'    "{edge.src}" -> "{edge.dst}" '
                f'[label="{label}", color="#E67E22", style=dashed];'
            )
        elif edge.kind == "ask":
            lines.append(
                f'    "{edge.src}" -> "{edge.dst}" '
                f'[label="{label}", color="#C0392B", penwidth=2, style=bold];'
            )
        elif edge.kind == "call":
            lines.append(
                f'    "{edge.src}" -> "{edge.dst}" '
                f'[label="{label}", color="#27AE60", style=dotted];'
            )

    lines.append("}")
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Generate Graphviz diagrams from Hew AST JSON.",
        epilog="Example: hew build --emit-ast prog.hew | python3 scripts/viz/ast.py -f svg -o out.svg",
    )
    parser.add_argument("input", nargs="?", help="Input JSON file (default: stdin)")
    parser.add_argument("-o", "--output", help="Output file (default: stdout)")
    parser.add_argument(
        "-f", "--format", default="dot",
        choices=["dot", "svg", "png", "pdf"],
        help="Output format (default: dot)",
    )
    args = parser.parse_args()

    # Read input
    if args.input:
        with open(args.input) as f:
            ast = json.load(f)
    else:
        ast = json.load(sys.stdin)

    # Walk AST
    walker = ASTWalker(ast)
    walker.walk()

    # Generate DOT
    dot_source = generate_dot(walker)

    # Output
    if args.format == "dot":
        if args.output:
            with open(args.output, "w") as f:
                f.write(dot_source)
        else:
            print(dot_source)
    else:
        # Render with graphviz
        cmd = ["dot", f"-T{args.format}"]
        try:
            result = subprocess.run(
                cmd, input=dot_source, capture_output=True, text=True, check=True,
            )
        except FileNotFoundError:
            print("Error: 'dot' (graphviz) not found. Install graphviz or use -f dot.", file=sys.stderr)
            sys.exit(1)
        except subprocess.CalledProcessError as e:
            print(f"Error running dot: {e.stderr}", file=sys.stderr)
            sys.exit(1)

        if args.output:
            with open(args.output, "wb") as f:
                f.write(result.stdout.encode() if isinstance(result.stdout, str) else result.stdout)
        else:
            sys.stdout.write(result.stdout)


if __name__ == "__main__":
    main()
