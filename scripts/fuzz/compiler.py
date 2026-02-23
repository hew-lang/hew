#!/usr/bin/env python3
"""Generative fuzzing of the Hew compiler.

Generates adversarial inputs across 48 categories and feeds them to
``hew build`` (and ``hew check`` for differential testing) to find
crashes, panics (ICEs), or hangs.

Unlike fuzz-grammar.py (which requires Grammarinator and the ANTLR grammar),
this script has **no external dependencies** — it uses pure-Python random
generation and mutation.

Categories span three areas:

  **Input edge cases** (17 categories):
    empty, unicode, types, syntax, actors, interpolation, scope, deep,
    pathological, boundary, strings, control_flow, malformed, random,
    mutations, garbage, binary

  **Language feature coverage** (22 categories):
    traits, enums, structs, patterns, async_await, defer, generators,
    operators, imports, wire_types, labels, ffi, attributes, visibility,
    tuples, dur_regex, unsafe, concurrency, supervisors, mailbox,
    lifecycle, try_op, methods, collections

  **Compiler testing techniques** (9 categories):
    differential (check vs build), cross_feature, error_cascade,
    type_infer, recursion, oracle, comments, whitespace, well_typed

Usage:
    ./scripts/fuzz/compiler.py                   # default: all categories
    ./scripts/fuzz/compiler.py -n 500            # 500 random programs per gen category
    ./scripts/fuzz/compiler.py -t 15             # 15s timeout per program
    ./scripts/fuzz/compiler.py --seed 42         # reproducible run
    ./scripts/fuzz/compiler.py --hew ./target/release/hew  # custom binary
    ./scripts/fuzz/compiler.py --category deep   # run only one category
    ./scripts/fuzz/compiler.py --list-categories # list available categories
    ./scripts/fuzz/compiler.py -k                # keep failing inputs
"""

from __future__ import annotations

import argparse
import os
import random
import shutil
import string
import subprocess
import sys
import tempfile
import traceback
from dataclasses import dataclass, field
from pathlib import Path


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def find_repo_root() -> Path:
    """Walk up from the script to find the repo root (contains Cargo.toml)."""
    d = Path(__file__).resolve().parent
    while d != d.parent:
        if (d / "Cargo.toml").exists():
            return d
        d = d.parent
    sys.exit(f"ERROR: Cannot find repo root from {__file__}")


@dataclass
class Stats:
    total: int = 0
    ok: int = 0
    error: int = 0
    crash: int = 0
    hang: int = 0
    ice: int = 0


@dataclass
class Issue:
    status: str
    label: str
    source: str = ""
    output: str = ""
    returncode: int | None = None


@dataclass
class FuzzContext:
    hew: Path
    timeout: int
    keep: bool
    workdir: Path
    stats: Stats = field(default_factory=Stats)
    issues: list[Issue] = field(default_factory=list)


def run_hew(ctx: FuzzContext, source: str | bytes, label: str,
            *, mode: str = "build") -> Issue | None:
    """Compile *source* with ``hew build`` (or ``hew check``) and classify."""
    ctx.stats.total += 1
    tmpfile = ctx.workdir / "input.hew"
    outfile = ctx.workdir / "output"

    try:
        with open(tmpfile, "wb") as f:
            if isinstance(source, str):
                f.write(source.encode("utf-8", errors="replace"))
            else:
                f.write(source)
    except Exception:
        ctx.stats.error += 1
        return None

    cmd = [str(ctx.hew), mode]
    if mode == "build":
        cmd += [str(tmpfile), "-o", str(outfile)]
    else:
        cmd.append(str(tmpfile))

    try:
        proc = subprocess.run(
            cmd, capture_output=True, text=True, timeout=ctx.timeout,
        )
        combined = proc.stdout + proc.stderr

        if proc.returncode == 0:
            ctx.stats.ok += 1
            return None

        if proc.returncode in (1, 2):
            # Detect Rust panics / ICEs.  Be specific to avoid false positives
            # from user-visible mentions of "panic" in Hew error messages.
            ice_markers = (
                "thread '",            # Rust panic header: "thread 'main' panicked at"
                "rust_backtrace",
                "internal compiler error",
                "has overflowed its stack",
                "panicked at",
            )
            lower = combined.lower()
            if any(m in lower for m in ice_markers):
                ctx.stats.ice += 1
                issue = Issue("ICE", label, _clip(source), combined[:2000])
                ctx.issues.append(issue)
                _save_issue(ctx, issue, source)
                return issue
            ctx.stats.error += 1
            return None

        # Negative return codes → signals (SIGSEGV, SIGABRT, …)
        ctx.stats.crash += 1
        issue = Issue("crash", label, _clip(source), combined[:2000], proc.returncode)
        ctx.issues.append(issue)
        _save_issue(ctx, issue, source)
        return issue

    except subprocess.TimeoutExpired:
        ctx.stats.hang += 1
        issue = Issue("hang", label, _clip(source))
        ctx.issues.append(issue)
        _save_issue(ctx, issue, source)
        return issue
    except Exception:
        ctx.stats.error += 1
        return None
    finally:
        for p in (tmpfile, outfile):
            p.unlink(missing_ok=True)


def _clip(source: str | bytes, limit: int = 300) -> str:
    s = source if isinstance(source, str) else repr(source)
    return s[:limit] + ("…" if len(s) > limit else "")


def _save_issue(ctx: FuzzContext, issue: Issue, source: str | bytes) -> None:
    """Persist the failing input so it can be inspected later."""
    kind_dir = ctx.workdir / issue.status
    kind_dir.mkdir(exist_ok=True)
    safe_label = issue.label.replace("/", "_")
    out = kind_dir / f"{safe_label}.hew"
    with open(out, "wb") as f:
        if isinstance(source, str):
            f.write(source.encode("utf-8", errors="replace"))
        else:
            f.write(source)


def _report(issue: Issue | None) -> None:
    if issue is not None:
        print(f"  !! {issue.status}: {issue.label}")


# ---------------------------------------------------------------------------
# Random generators
# ---------------------------------------------------------------------------

def rand_ident(maxlen: int = 20) -> str:
    first = random.choice(string.ascii_lowercase)
    rest = "".join(random.choices(string.ascii_letters + string.digits + "_", k=random.randint(0, maxlen)))
    return first + rest


def rand_type() -> str:
    return random.choice([
        "i32", "i64", "f64", "bool", "string", "String",
        f"Vec<{random.choice(['i32', 'string', 'f64'])}>",
        f"HashMap<string, {random.choice(['i32', 'string'])}>",
    ])


def rand_expr(depth: int = 0) -> str:
    if depth > 5:
        return str(random.randint(-1_000_000, 1_000_000))
    choice = random.randint(0, 15)
    if choice <= 3:
        return str(random.randint(-(2**31), 2**31 - 1))
    if choice == 4:
        return f"{random.uniform(-1e10, 1e10)}"
    if choice == 5:
        return f'"{rand_ident()}"'
    if choice == 6:
        return random.choice(("true", "false"))
    if choice == 7:
        op = random.choice(["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||"])
        return f"({rand_expr(depth + 1)} {op} {rand_expr(depth + 1)})"
    if choice == 8:
        return f"if {rand_expr(depth + 1)} {{ {rand_expr(depth + 1)} }} else {{ {rand_expr(depth + 1)} }}"
    if choice == 9:
        return f"match {rand_expr(depth + 1)} {{ _ => {rand_expr(depth + 1)} }}"
    if choice == 10:
        elems = ", ".join(rand_expr(depth + 1) for _ in range(random.randint(0, 4)))
        return f"({elems})"
    if choice == 11:
        return f"|{rand_ident()}: i32| -> i32 {{ {rand_expr(depth + 1)} }}"
    if choice == 12:
        args = ", ".join(rand_expr(depth + 1) for _ in range(random.randint(0, 3)))
        return f"{rand_ident()}({args})"
    if choice == 13:
        return f"0..{random.randint(0, 100)}"
    if choice == 14:
        return f'f"{rand_ident()} {{{rand_expr(depth + 1)}}}"'
    return rand_ident()


def rand_stmt(depth: int = 0) -> str:
    choice = random.randint(0, 10)
    if choice <= 2:
        return f"let {rand_ident()} = {rand_expr(depth)};"
    if choice == 3:
        return f"var {rand_ident()} = {rand_expr(depth)};"
    if choice == 4:
        return f"println({rand_expr(depth)});"
    if choice == 5:
        return f"if {rand_expr(depth)} {{ {rand_stmt(depth + 1)} }}"
    if choice == 6:
        return f"for {rand_ident()} in 0..{random.randint(0, 10)} {{ {rand_stmt(depth + 1)} }}"
    if choice == 7:
        return f"while {rand_expr(depth)} {{ {rand_stmt(depth + 1)} break; }}"
    if choice == 8:
        return f"return {rand_expr(depth)};"
    if choice == 9:
        name = rand_ident()
        params = ", ".join(f"{rand_ident()}: {rand_type()}" for _ in range(random.randint(0, 4)))
        return f"fn {name}({params}) -> {rand_type()} {{ {rand_expr(depth)} }}"
    return f"{rand_expr(depth)};"


# ---------------------------------------------------------------------------
# Fuzz categories
# ---------------------------------------------------------------------------

def fuzz_empty(ctx: FuzzContext) -> None:
    """Empty and near-empty inputs."""
    cases = [
        ("empty", ""),
        ("whitespace", "   \n\n\t\t  "),
        ("single_newline", "\n"),
        ("just_comment", "// hello"),
        ("block_comment", "/* comment */"),
        ("null_byte", "\x00"),
        ("null_in_string", 'fn main() { let x = "hello\x00world"; }'),
        ("bom", "\ufeff fn main() { }"),
        ("only_semicolons", ";;;;"),
        ("only_braces", "{{{}}}"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"empty/{label}"))


def fuzz_unicode(ctx: FuzzContext) -> None:
    """Unicode edge cases."""
    cases = [
        ("emoji_ident", "fn main() { let \U0001f389 = 1; }"),
        ("emoji_string", 'fn main() { let x = "\U0001f389\U0001f525\U0001f4af"; println(x); }'),
        ("rtl", 'fn main() { let x = "\u0645\u0631\u062d\u0628\u0627"; }'),
        ("cjk", 'fn main() { let x = "\u4f60\u597d\u4e16\u754c"; }'),
        ("null_char", 'fn main() { let x = "\\0"; }'),
        ("escape_hell", 'fn main() { let x = "\\n\\t\\r\\\\\\\""; }'),
        ("mixed_unicode_ident", "fn main() { let caf\u00e9 = 42; }"),
        ("zero_width_space", "fn main() { let x\u200b = 1; }"),
        ("combining_chars", "fn main() { let a\u0300 = 1; }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"unicode/{label}"))


def fuzz_types(ctx: FuzzContext) -> None:
    """Type system edge cases."""
    cases = [
        ("recursive_type", "type Foo { x: Foo; }\nfn main() { }"),
        ("mutual_recursion", "type A { b: B; }\ntype B { a: A; }\nfn main() { }"),
        ("generic_spam", "fn id<T>(x: T) -> T { x }\nfn main() { let x = id(id(id(id(id(1))))); }"),
        ("option_none", "fn main() { let x: Option<i32> = None; }"),
        ("result_nested", "fn main() { let x: Result<Result<i32, string>, string> = Ok(Ok(1)); }"),
        ("vec_of_vec", "fn main() { let x: Vec<Vec<Vec<i32>>> = Vec::new(); }"),
        ("hashmap_complex", "fn main() { let x: HashMap<string, Vec<i32>> = HashMap::new(); }"),
        ("fn_type_mismatch", "fn f(x: i32) -> string { x }\nfn main() { }"),
        ("wrong_arg_count", "fn f(x: i32) -> i32 { x }\nfn main() { f(1, 2, 3); }"),
        ("assign_to_let", "fn main() { let x = 1; x = 2; }"),
        ("void_in_expr", "fn f() { }\nfn main() { let x = f(); }"),
        ("self_assign", "fn main() { let x = x; }"),
        ("shadow_builtin", "fn main() { let println = 1; println(println); }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"types/{label}"))


def fuzz_syntax(ctx: FuzzContext) -> None:
    """Syntax edge cases that might confuse the parser."""
    cases = [
        ("unclosed_brace", "fn main() {"),
        ("unclosed_paren", "fn main() { let x = (1 + 2; }"),
        ("extra_close", "fn main() { } }"),
        ("missing_semi", "fn main() { let x = 1 let y = 2 }"),
        ("double_semi", "fn main() { let x = 1;; }"),
        ("arrow_chain", "fn main() { let x = 1 => 2 => 3; }"),
        ("triple_eq", "fn main() { if 1 === 2 { } }"),
        ("pipe_op", "fn main() { let x = 1 |> f; }"),
        ("at_label", "fn main() { @label: for i in 0..10 { break @label; } }"),
        ("hash_attr", "#[inline]\nfn f() -> i32 { 1 }\nfn main() { }"),
        ("nested_comments", "/* outer /* inner */ still outer */\nfn main() { }"),
        ("trailing_comma", "fn f(x: i32, y: i32,) -> i32 { x + y }\nfn main() { f(1, 2); }"),
        ("empty_match", "fn main() { match 1 { } }"),
        ("dangling_else", "fn main() { if true { 1 } else if false { 2 } }"),
        ("expr_as_stmt", "fn main() { 1 + 2; }"),
        ("negative_literal", "fn main() { let x = -2147483648; }"),
        ("max_int", "fn main() { let x = 9223372036854775807; }"),
        ("overflow_int", "fn main() { let x = 99999999999999999999999999999999; }"),
        ("float_edge", "fn main() { let x = 0.0; let y = -0.0; let z = 1e308; }"),
        ("keyword_as_ident", "fn main() { let fn = 1; }"),
        ("op_only", "fn main() { +; }"),
        ("dot_only", "fn main() { .; }"),
        ("double_dot", "fn main() { let x = ..; }"),
        ("question_mark", "fn main() { let x = f()?; }"),
        ("fn_no_body", "fn f() -> i32;\nfn main() { }"),
        ("import_nonexistent", "import std::nonexistent;\nfn main() { }"),
        ("circular_import", "import self;\nfn main() { }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"syntax/{label}"))


def fuzz_actors(ctx: FuzzContext) -> None:
    """Actor-specific edge cases."""
    cases = [
        ("empty_actor", "actor Empty { }\nfn main() { let e = spawn Empty(); }"),
        ("actor_no_receive", "actor Foo { x: i32; }\nfn main() { let f = spawn Foo(x: 0); }"),
        ("actor_self_send", (
            "actor Pinger {\n  count: i32;\n"
            "  receive fn ping() {\n    self.count = self.count + 1;\n"
            "    if self.count < 10 { self.ping(); }\n  }\n}\n"
            "fn main() { let p = spawn Pinger(count: 0); p.ping(); }"
        )),
        ("spawn_in_receive", (
            "actor Spawner {\n"
            "  receive fn go() {\n"
            "    let child = spawn (x: i32) => { println(x); };\n"
            "    child.send(42);\n  }\n}\n"
            "fn main() { let s = spawn Spawner(); s.go(); }"
        )),
        ("many_fields", "actor Big {\n" + "\n".join(f"  f{i}: i32;" for i in range(100)) + "\n}\nfn main() { }"),
        ("lambda_no_body", "fn main() { let f = spawn (x: i32) => { }; }"),
        ("actor_duplicate_method", (
            "actor Dup {\n"
            "  receive fn foo() { println(1); }\n"
            "  receive fn foo() { println(2); }\n"
            "}\nfn main() { }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"actor/{label}"))


def fuzz_interpolation(ctx: FuzzContext) -> None:
    """String interpolation edge cases."""
    cases = [
        ("empty_interp", 'fn main() { let x = f"{}"; }'),
        ("nested_braces", 'fn main() { let x = f"{{}}"; }'),
        ("expr_in_interp", 'fn main() { let a = 1; let x = f"{a + a}"; }'),
        ("string_in_interp", 'fn main() { let x = f"{"hello"}"; }'),
        ("interp_chain", 'fn main() { let a = 1; let x = f"{a}{a}{a}{a}{a}"; }'),
        ("interp_only", 'fn main() { let a = 1; let x = f"{a}"; }'),
        ("unmatched_open", 'fn main() { let x = f"{"; }'),
        ("unmatched_close", 'fn main() { let x = f"}"; }'),
        ("nested_interp", 'fn main() { let a = 1; let x = f"{f"{a}"}"; }'),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"interp/{label}"))


def fuzz_scope(ctx: FuzzContext) -> None:
    """Scope and closure edge cases."""
    cases = [
        ("use_before_decl", "fn main() { println(x); let x = 1; }"),
        ("double_decl", "fn main() { let x = 1; let x = 2; }"),
        ("closure_capture", "fn main() { let x = 1; let f = |y: i32| -> i32 { x + y }; }"),
        ("closure_mut_capture", "fn main() { var x = 1; let f = |y: i32| -> i32 { x = x + y; x }; }"),
        ("nested_closure", "fn main() { let f = |x: i32| -> i32 { let g = |y: i32| -> i32 { x + y }; g(1) }; }"),
        ("fn_in_fn", "fn main() { fn inner() -> i32 { 42 } println(inner()); }"),
        ("deeply_nested_scope", "fn main() {\n" + "{\n" * 100 + "let x = 1;\n" + "}\n" * 100 + "}"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"scope/{label}"))


def fuzz_deep(ctx: FuzzContext) -> None:
    """Deep nesting stress tests."""
    cases = [
        ("deep_if_500", "fn main() {\n" + "".join(f"if true {{ let x{i} = {i};\n" for i in range(500)) + "1;\n" + "}" * 500 + "\n}"),
        ("deep_match_200", "fn main() {\n" + "".join(f"match {i} {{ _ => {{\n" for i in range(200)) + "1;\n" + "}}" * 200 + "\n}"),
        ("deep_block_1000", "fn main() {\n" + "{\n" * 1000 + "let x = 1;\n" + "}\n" * 1000 + "}"),
        ("deep_for_100", "fn main() {\n" + "".join(f"for i{n} in 0..1 {{\n" for n in range(100)) + "println(1);\n" + "}" * 100 + "\n}"),
        ("fn_chain_500", "\n".join(f"fn f{i}() -> i32 {{ f{i+1}() }}" for i in range(500)) + "\nfn f500() -> i32 { 42 }\nfn main() { println(f0()); }"),
        ("nested_tuple_50", "fn main() { let x = " + "(" * 50 + "1" + ",)" * 50 + "; }"),
        ("nested_vec_type", "fn main() { let x: " + "Vec<" * 20 + "i32" + ">" * 20 + " = Vec::new(); }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"deep/{label}"))


def fuzz_pathological(ctx: FuzzContext) -> None:
    """Pathological patterns known to stress parsers and compilers."""
    cases = [
        ("deep_parens_500", "fn main() { let x = " + "(" * 500 + "1" + ")" * 500 + "; }"),
        ("long_chain_5000", "fn main() { let x = " + " + ".join(["1"] * 5000) + "; }"),
        ("many_operators_10000", "fn main() { let x = " + " + ".join([str(i) for i in range(10000)]) + "; }"),
        ("repeated_keywords", " ".join(["fn"] * 1000)),
        ("all_operators", "fn main() { let x = 1 + 2 - 3 * 4 / 5 % 6 == 7 != 8 < 9 > 10 <= 11 >= 12 && true || false; }"),
        ("many_matches", "fn main() {\n" + "\n".join(f"  match {i} {{ {i} => println({i}), _ => {{}} }}" for i in range(200)) + "\n}"),
        ("chained_push_1000", "fn main() { let v: Vec<i32> = Vec::new(); " + "v.push(1); " * 1000 + "}"),
        ("many_stmts_5000", "fn main() {\n" + "\n".join(f"  let x{i} = {i};" for i in range(5000)) + "\n}"),
        ("many_functions_2000", "\n".join(f"fn f{i}() -> i32 {{ {i} }}" for i in range(2000)) + "\nfn main() { println(f0()); }"),
        ("many_params_500", f"fn f({', '.join(f'p{i}: i32' for i in range(500))}) -> i32 {{ 0 }}\nfn main() {{ }}"),
        ("long_string_100k", 'fn main() { let x = "' + "a" * 100000 + '"; }'),
        ("long_ident_100k", f"fn main() {{ let {'a' * 100000} = 1; }}"),
        ("crlf_endings", "fn main() {\r\n  let x = 1;\r\n  println(x);\r\n}\r\n"),
        ("many_empty_lines", "\n" * 10000 + "fn main() { }"),
        ("tabs_and_spaces", "fn\tmain()\t{\tlet\tx\t=\t1;\t}"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"pathological/{label}"))


def fuzz_boundary(ctx: FuzzContext) -> None:
    """Numeric boundary values."""
    cases = [
        ("i32_max", "fn main() { let x: i32 = 2147483647; println(x); }"),
        ("i32_min", "fn main() { let x: i32 = -2147483648; println(x); }"),
        ("i64_max", "fn main() { let x = 9223372036854775807; println(x); }"),
        ("i64_min", "fn main() { let x = -9223372036854775808; println(x); }"),
        ("i64_overflow", "fn main() { let x = 9223372036854775808; }"),
        ("huge_float", "fn main() { let x = 1.7976931348623157e+308; }"),
        ("tiny_float", "fn main() { let x = 5e-324; }"),
        ("neg_zero", "fn main() { let x = -0.0; }"),
        ("inf", "fn main() { let x = 1.0 / 0.0; }"),
        ("nan_cmp", "fn main() { let x = 0.0 / 0.0; if x == x { println(1); } }"),
        ("leading_zeros", "fn main() { let x = 0000000000000000000000000; }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"boundary/{label}"))


def fuzz_strings(ctx: FuzzContext) -> None:
    """String edge cases."""
    cases = [
        ("empty", 'fn main() { let x = ""; println(x); }'),
        ("single_char", 'fn main() { let x = "a"; println(x); }'),
        ("long_100k", 'fn main() { let x = "' + "a" * 100000 + '"; }'),
        ("many_escapes", 'fn main() { let x = "' + "\\n" * 1000 + '"; }'),
        ("embedded_quotes", 'fn main() { let x = "he said \\"hello\\""; }'),
        ("backslash_end", 'fn main() { let x = "test\\\\"; }'),
        ("concat_1000", "fn main() { let x = " + " + ".join(['"a"'] * 1000) + "; }"),
        ("null_in_string", 'fn main() { let x = "hello\\0world"; }'),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"string/{label}"))


def fuzz_control_flow(ctx: FuzzContext) -> None:
    """Control flow edge cases."""
    cases = [
        ("infinite_break", "fn main() { while true { break; } }"),
        ("nested_break", "fn main() { for i in 0..10 { for j in 0..10 { break; } } }"),
        ("continue_only", "fn main() { for i in 0..10 { continue; } }"),
        ("return_in_if", "fn f() -> i32 { if true { return 1; } return 2; }\nfn main() { println(f()); }"),
        ("return_in_for", "fn f() -> i32 { for i in 0..10 { return i; } return -1; }\nfn main() { println(f()); }"),
        ("dead_code", "fn f() -> i32 { return 1; let x = 2; x }\nfn main() { println(f()); }"),
        ("empty_if", "fn main() { if true { } }"),
        ("empty_for", "fn main() { for i in 0..0 { println(i); } }"),
        ("empty_while", "fn main() { while false { } }"),
        ("nested_return", "fn f() -> i32 { if true { if true { return 1; } return 2; } return 3; }\nfn main() { println(f()); }"),
        ("match_wildcard_dup", "fn main() { match 1 { _ => println(1), _ => println(2) } }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"cf/{label}"))


def fuzz_malformed(ctx: FuzzContext) -> None:
    """Malformed constructs that should error gracefully."""
    cases = [
        ("fn_no_name", "fn () { }"),
        ("fn_no_parens", "fn main { }"),
        ("actor_no_name", "actor { }"),
        ("type_no_name", "type { x: i32; }"),
        ("receive_outside_actor", "fn main() { receive fn foo() { } }"),
        ("spawn_non_actor", "fn main() { let x = spawn 42; }"),
        ("double_fn", "fn fn main() { }"),
        ("let_in_expr", "fn main() { let x = let y = 1; }"),
        ("missing_type_ann", "fn f(x) -> i32 { x }\nfn main() { }"),
        ("duplicate_param", "fn f(x: i32, x: i32) -> i32 { x }\nfn main() { }"),
        ("duplicate_field", "type Foo { x: i32; x: i32; }\nfn main() { }"),
        ("import_star", "import std::*;\nfn main() { }"),
        ("raw_await", "fn main() { await; }"),
        ("raw_spawn", "fn main() { spawn; }"),
        ("raw_return", "fn main() { return; }"),
        ("assign_to_literal", "fn main() { 1 = 2; }"),
        ("call_int", "fn main() { 42(); }"),
        ("index_int", "fn main() { 42[0]; }"),
        ("unclosed_string", 'fn main() { let x = "hello'),
        ("backslash_only", "fn main() { let x = \\; }"),
        ("hash_bang", "#!/usr/bin/env hew\nfn main() { println(42); }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"malformed/{label}"))


def fuzz_random(ctx: FuzzContext, n: int = 100) -> None:
    """Generate random syntactically-plausible programs."""
    for i in range(n):
        num_stmts = random.randint(1, 15)
        body = "\n  ".join(rand_stmt() for _ in range(num_stmts))
        src = f"fn main() {{\n  {body}\n}}"
        _report(run_hew(ctx, src, f"random/{i}"))


def fuzz_mutations(ctx: FuzzContext, n: int = 200) -> None:
    """Take valid programs and randomly mutate them."""
    seeds = [
        "fn main() { let x = 42; println(x); }",
        "fn add(a: i32, b: i32) -> i32 { a + b }\nfn main() { println(add(1, 2)); }",
        "fn main() { for i in 0..10 { println(i); } }",
        "fn main() { var x = 0; while x < 10 { x = x + 1; } println(x); }",
        "fn main() { let x = if true { 1 } else { 2 }; println(x); }",
        "fn fib(n: i32) -> i32 { if n <= 1 { n } else { fib(n - 1) + fib(n - 2) } }\nfn main() { println(fib(10)); }",
    ]
    mutations = [
        lambda s: s[: random.randint(0, len(s))],
        lambda s: s + random.choice(["{", "}", "(", ")", ";", "\n"]) * random.randint(1, 50),
        lambda s: (
            s[: (p := random.randint(0, len(s)))]
            + random.choice(["fn", "let", "if", "for", "while", "match", "spawn", "actor", "{}", "()", ";"])
            + s[random.randint(0, len(s)) :]
        ),
        lambda s: "".join(random.sample(list(s), len(s))) if s else s,
        lambda s: s.replace(
            random.choice(list(set(s) - {" ", "\n"})) if set(s) - {" ", "\n"} else "a",
            random.choice(string.printable),
        ),
        lambda s: s * random.randint(2, 5),
    ]
    for i in range(n):
        src = random.choice(seeds)
        for _ in range(random.randint(1, 3)):
            src = random.choice(mutations)(src) or src
        _report(run_hew(ctx, src, f"mutant/{i}"))


def fuzz_garbage(ctx: FuzzContext, n: int = 50) -> None:
    """Pure random printable characters."""
    for i in range(n):
        length = random.randint(1, 5000)
        src = "".join(random.choices(string.printable, k=length))
        _report(run_hew(ctx, src, f"garbage/{i}"))


def fuzz_binary(ctx: FuzzContext, n: int = 50) -> None:
    """Random binary data, both raw and injected into valid frames."""
    for i in range(n // 2):
        data = os.urandom(random.randint(1, 2000))
        _report(run_hew(ctx, data, f"binary/raw_{i}"))

    for i in range(n // 2):
        garbage = os.urandom(random.randint(1, 500)).decode("latin-1")
        templates = [
            f"fn main() {{ {garbage} }}",
            f"fn main() {{ let x = {garbage}; }}",
            f"{garbage}\nfn main() {{ }}",
            f"fn main() {{ println({garbage}); }}",
        ]
        _report(run_hew(ctx, random.choice(templates), f"binary/inject_{i}"))


# ---------------------------------------------------------------------------
# NEW: Language feature coverage
# ---------------------------------------------------------------------------

def fuzz_traits(ctx: FuzzContext) -> None:
    """Trait declarations, impls, and bounds."""
    cases = [
        ("empty_trait", "trait Empty { }\nfn main() { }"),
        ("trait_with_method", "trait Greet { fn hello() -> string; }\nfn main() { }"),
        ("trait_with_default", "trait Greet { fn hello() -> string { \"hi\" } }\nfn main() { }"),
        ("impl_trait", (
            "trait Greet { fn hello(self) -> string; }\n"
            "type Dog { name: string; }\n"
            "impl Greet for Dog { fn hello(self) -> string { self.name } }\n"
            "fn main() { }"
        )),
        ("multiple_traits", (
            "trait A { fn a() -> i32; }\n"
            "trait B { fn b() -> i32; }\n"
            "type Foo { }\n"
            "impl A for Foo { fn a() -> i32 { 1 } }\n"
            "impl B for Foo { fn b() -> i32 { 2 } }\n"
            "fn main() { }"
        )),
        ("trait_inheritance", "trait Base { fn base() -> i32; }\ntrait Child : Base { fn child() -> i32; }\nfn main() { }"),
        ("generic_trait_bound", "fn show<T: Display>(x: T) -> string { x.to_string() }\nfn main() { }"),
        ("where_clause", "fn foo<T>(x: T) -> i32 where T: Display { 1 }\nfn main() { }"),
        ("multiple_bounds", "fn foo<T: Display + Send>(x: T) { }\nfn main() { }"),
        ("associated_type", "trait Container { type Item; fn get(self) -> Self::Item; }\nfn main() { }"),
        ("impl_wrong_signature", (
            "trait Foo { fn bar() -> i32; }\n"
            "type Baz { }\n"
            "impl Foo for Baz { fn bar() -> string { \"wrong\" } }\n"
            "fn main() { }"
        )),
        ("impl_missing_method", (
            "trait Foo { fn bar() -> i32; fn baz() -> i32; }\n"
            "type X { }\n"
            "impl Foo for X { fn bar() -> i32 { 1 } }\n"
            "fn main() { }"
        )),
        ("self_param", (
            "trait Describable { fn describe(self) -> string; }\n"
            "type Point { x: i32; y: i32; }\n"
            "impl Describable for Point { fn describe(self) -> string { \"point\" } }\n"
            "fn main() { }"
        )),
        ("dyn_trait", "trait Speak { fn speak(self) -> string; }\nfn loud(s: dyn Speak) { }\nfn main() { }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"trait/{label}"))


def fuzz_enums(ctx: FuzzContext) -> None:
    """Enum declarations, variants, and pattern matching."""
    cases = [
        ("simple_enum", "enum Color { Red, Green, Blue }\nfn main() { let c = Color::Red; }"),
        ("enum_with_data", "enum Shape { Circle(f64), Rect(f64, f64) }\nfn main() { let s = Shape::Circle(3.14); }"),
        ("enum_struct_variant", "enum Msg { Quit, Move { x: i32; y: i32; }, Text(string) }\nfn main() { }"),
        ("match_enum", (
            "enum Dir { Up, Down, Left, Right }\n"
            "fn show(d: Dir) -> string {\n"
            "  match d { Dir::Up => \"up\", Dir::Down => \"down\", Dir::Left => \"left\", Dir::Right => \"right\" }\n"
            "}\nfn main() { println(show(Dir::Up)); }"
        )),
        ("match_enum_data", (
            "enum Val { Int(i32), Str(string) }\n"
            "fn show(v: Val) -> string {\n"
            "  match v { Val::Int(n) => \"int\", Val::Str(s) => s }\n"
            "}\nfn main() { }"
        )),
        ("enum_methods", (
            "enum Option2 { Some2(i32), None2 }\n"
            "impl Option2 { fn is_some(self) -> bool { match self { Option2::Some2(_) => true, _ => false } } }\n"
            "fn main() { }"
        )),
        ("nested_enum", "enum Outer { Inner(Inner) }\nenum Inner { A, B }\nfn main() { }"),
        ("empty_enum", "enum Empty { }\nfn main() { }"),
        ("many_variants", "enum Big {\n" + ",\n".join(f"  V{i}" for i in range(100)) + "\n}\nfn main() { }"),
        ("recursive_enum", "enum List { Nil, Cons(i32, Box<List>) }\nfn main() { }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"enum/{label}"))


def fuzz_structs(ctx: FuzzContext) -> None:
    """Struct declarations, initialization, field access, and methods."""
    cases = [
        ("empty_struct", "type Empty { }\nfn main() { let e = Empty {}; }"),
        ("single_field", "type Wrap { val: i32; }\nfn main() { let w = Wrap { val: 42 }; }"),
        ("many_fields", "type Big {\n" + "\n".join(f"  f{i}: i32;" for i in range(50)) + "\n}\nfn main() { }"),
        ("nested_struct", (
            "type Inner { x: i32; }\n"
            "type Outer { inner: Inner; y: i32; }\n"
            "fn main() { let o = Outer { inner: Inner { x: 1 }, y: 2 }; }"
        )),
        ("struct_method", (
            "type Counter { count: i32; }\n"
            "impl Counter { fn value(self) -> i32 { self.count } }\n"
            "fn main() { }"
        )),
        ("field_access_chain", (
            "type A { b: B; }\ntype B { c: C; }\ntype C { val: i32; }\n"
            "fn main() { }"
        )),
        ("struct_update", "type Point { x: i32; y: i32; }\nfn main() { var p = Point { x: 1, y: 2 }; p.x = 10; }"),
        ("generic_struct", "type Pair<T> { first: T; second: T; }\nfn main() { }"),
        ("struct_with_vec", "type Names { items: Vec<string>; }\nfn main() { }"),
        ("struct_with_option", "type Config { name: string; port: Option<i32>; }\nfn main() { }"),
        ("duplicate_field_names", "type Bad { x: i32; x: string; }\nfn main() { }"),
        ("field_type_mismatch", "type Foo { x: i32; }\nfn main() { let f = Foo { x: \"wrong\" }; }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"struct/{label}"))


def fuzz_patterns(ctx: FuzzContext) -> None:
    """Pattern matching: or-patterns, guards, destructuring, nesting."""
    cases = [
        ("wildcard", "fn main() { match 42 { _ => println(1) } }"),
        ("literal_int", "fn main() { match 42 { 42 => println(1), _ => println(0) } }"),
        ("literal_string", 'fn main() { match "hi" { "hi" => println(1), _ => println(0) } }'),
        ("literal_bool", "fn main() { match true { true => println(1), false => println(0) } }"),
        ("variable_binding", "fn main() { match 42 { x => println(x) } }"),
        ("or_pattern", "fn main() { match 1 { 1 | 2 | 3 => println(1), _ => println(0) } }"),
        ("guard", "fn main() { match 42 { x if x > 10 => println(1), _ => println(0) } }"),
        ("guard_complex", "fn main() { match 42 { x if x > 0 && x < 100 => println(1), _ => println(0) } }"),
        ("nested_match", (
            "fn main() {\n"
            "  match 1 {\n"
            "    1 => match 2 { 2 => println(1), _ => println(0) },\n"
            "    _ => println(0)\n"
            "  }\n"
            "}"
        )),
        ("tuple_destructure", "fn main() { let (a, b) = (1, 2); println(a); }"),
        ("tuple_in_match", "fn main() { match (1, 2) { (1, 2) => println(1), _ => println(0) } }"),
        ("constructor_pattern", (
            "enum Shape { Circle(f64), Rect(f64, f64) }\n"
            "fn area(s: Shape) -> f64 { match s { Shape::Circle(r) => r, Shape::Rect(w, h) => w } }\n"
            "fn main() { }"
        )),
        ("struct_pattern", (
            "type Point { x: i32; y: i32; }\n"
            "fn main() { let p = Point { x: 1, y: 2 }; match p { Point { x, y } => println(x) } }"
        )),
        ("nested_option", (
            "fn main() {\n"
            "  let x: Option<Option<i32>> = Some(Some(42));\n"
            "  match x { Some(Some(v)) => println(v), _ => println(0) }\n"
            "}"
        )),
        ("exhaustiveness_missing", (
            "enum Dir { Up, Down, Left, Right }\n"
            "fn f(d: Dir) -> i32 { match d { Dir::Up => 1, Dir::Down => 2 } }\n"
            "fn main() { }"
        )),
        ("many_arms", "fn main() { match 0 {\n" + "\n".join(f"  {i} => println({i})," for i in range(50)) + "\n  _ => println(-1)\n} }"),
        ("match_on_string", 'fn main() { match "hello" { "hello" => println(1), "world" => println(2), _ => println(0) } }'),
        ("match_result", (
            "fn main() {\n"
            "  let r: Result<i32, string> = Ok(42);\n"
            "  match r { Ok(v) => println(v), Err(e) => println(e) }\n"
            "}"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"pattern/{label}"))


def fuzz_async_await(ctx: FuzzContext) -> None:
    """Async/await patterns and timeout combinators."""
    cases = [
        ("basic_await", (
            "actor Worker { receive fn compute() -> i32 { 42 } }\n"
            "fn main() { let w = spawn Worker(); let r = await w.compute(); println(r); }"
        )),
        ("await_in_if", (
            "actor W { receive fn get() -> i32 { 1 } }\n"
            "fn main() {\n"
            "  let w = spawn W();\n"
            "  if true { let r = await w.get(); println(r); }\n"
            "}"
        )),
        ("await_in_for", (
            "actor W { receive fn get() -> i32 { 1 } }\n"
            "fn main() {\n"
            "  let w = spawn W();\n"
            "  for i in 0..3 { let r = await w.get(); println(r); }\n"
            "}"
        )),
        ("multiple_awaits", (
            "actor A { receive fn val() -> i32 { 1 } }\n"
            "actor B { receive fn val() -> i32 { 2 } }\n"
            "fn main() {\n"
            "  let a = spawn A(); let b = spawn B();\n"
            "  let x = await a.val(); let y = await b.val();\n"
            "  println(x + y);\n"
            "}"
        )),
        ("timeout_expr", (
            "actor Slow { receive fn wait() -> i32 { 42 } }\n"
            "fn main() { let s = spawn Slow(); let r = await s.wait() | after 1s; }"
        )),
        ("await_void", (
            "actor W { receive fn fire() { println(1); } }\n"
            "fn main() { let w = spawn W(); w.fire(); }"
        )),
        ("nested_actor_await", (
            "actor Inner { receive fn value() -> i32 { 99 } }\n"
            "actor Outer {\n"
            "  receive fn go() -> i32 {\n"
            "    let inner = spawn Inner();\n"
            "    let v = await inner.value();\n"
            "    v\n"
            "  }\n"
            "}\n"
            "fn main() { let o = spawn Outer(); let r = await o.go(); println(r); }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"async/{label}"))


def fuzz_defer(ctx: FuzzContext) -> None:
    """Defer statement ordering and interaction with returns."""
    cases = [
        ("basic_defer", "fn main() { defer println(1); println(2); }"),
        ("multiple_defers", "fn main() { defer println(1); defer println(2); defer println(3); println(0); }"),
        ("defer_with_return", "fn f() -> i32 { defer println(1); return 42; }\nfn main() { println(f()); }"),
        ("defer_in_if", "fn main() { if true { defer println(1); } println(2); }"),
        ("defer_in_for", "fn main() { for i in 0..3 { defer println(i); } }"),
        ("defer_in_while", "fn main() { var x = 0; while x < 3 { defer println(x); x = x + 1; break; } }"),
        ("defer_with_var", "fn main() { var x = 0; defer println(x); x = 42; }"),
        ("defer_early_return", (
            "fn f(b: bool) -> i32 {\n"
            "  defer println(1);\n"
            "  if b { return 10; }\n"
            "  defer println(2);\n"
            "  return 20;\n"
            "}\nfn main() { println(f(true)); println(f(false)); }"
        )),
        ("defer_fn_call", "fn cleanup() { println(0); }\nfn main() { defer cleanup(); println(1); }"),
        ("nested_defer", "fn main() { defer { defer println(1); println(2); }; println(3); }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"defer/{label}"))


def fuzz_generators(ctx: FuzzContext) -> None:
    """Generator functions and yield."""
    cases = [
        ("basic_gen", "gen fn count() -> i32 { yield 1; yield 2; yield 3; }\nfn main() { }"),
        ("gen_loop", "gen fn naturals() -> i32 { var i = 0; while true { yield i; i = i + 1; } }\nfn main() { }"),
        ("gen_conditional", (
            "gen fn evens(max: i32) -> i32 {\n"
            "  for i in 0..max { if i % 2 == 0 { yield i; } }\n"
            "}\nfn main() { }"
        )),
        ("for_gen", (
            "gen fn items() -> i32 { yield 10; yield 20; }\n"
            "fn main() { for x in items() { println(x); } }"
        )),
        ("empty_gen", "gen fn empty() -> i32 { }\nfn main() { }"),
        ("nested_yield", (
            "gen fn nested() -> i32 {\n"
            "  for i in 0..3 { for j in 0..3 { yield i * 10 + j; } }\n"
            "}\nfn main() { }"
        )),
        ("yield_string", "gen fn words() -> string { yield \"hello\"; yield \"world\"; }\nfn main() { }"),
        ("receive_gen", (
            "actor Streamer {\n"
            "  receive gen fn stream() -> i32 { yield 1; yield 2; yield 3; }\n"
            "}\nfn main() { }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"gen/{label}"))


def fuzz_operators(ctx: FuzzContext) -> None:
    """All binary, unary, compound-assignment, and bitwise operators."""
    cases = [
        # Arithmetic
        ("add", "fn main() { println(1 + 2); }"),
        ("sub", "fn main() { println(5 - 3); }"),
        ("mul", "fn main() { println(3 * 4); }"),
        ("div", "fn main() { println(10 / 3); }"),
        ("modulo", "fn main() { println(10 % 3); }"),
        # Comparison
        ("eq", "fn main() { println(1 == 1); }"),
        ("neq", "fn main() { println(1 != 2); }"),
        ("lt", "fn main() { println(1 < 2); }"),
        ("gt", "fn main() { println(2 > 1); }"),
        ("le", "fn main() { println(1 <= 1); }"),
        ("ge", "fn main() { println(1 >= 1); }"),
        # Logical
        ("and_sym", "fn main() { println(true && false); }"),
        ("or_sym", "fn main() { println(true || false); }"),
        ("and_kw", "fn main() { println(true and false); }"),
        ("or_kw", "fn main() { println(true or false); }"),
        ("not", "fn main() { println(!true); }"),
        # Bitwise
        ("bit_and", "fn main() { println(0xFF & 0x0F); }"),
        ("bit_or", "fn main() { println(0xF0 | 0x0F); }"),
        ("bit_xor", "fn main() { println(0xFF ^ 0x0F); }"),
        ("bit_not", "fn main() { println(~0); }"),
        ("shl", "fn main() { println(1 << 4); }"),
        ("shr", "fn main() { println(16 >> 2); }"),
        # Compound assignment
        ("add_assign", "fn main() { var x = 1; x += 2; println(x); }"),
        ("sub_assign", "fn main() { var x = 5; x -= 3; println(x); }"),
        ("mul_assign", "fn main() { var x = 3; x *= 4; println(x); }"),
        ("div_assign", "fn main() { var x = 10; x /= 2; println(x); }"),
        ("mod_assign", "fn main() { var x = 10; x %= 3; println(x); }"),
        ("and_assign", "fn main() { var x = 0xFF; x &= 0x0F; println(x); }"),
        ("or_assign", "fn main() { var x = 0xF0; x |= 0x0F; println(x); }"),
        ("xor_assign", "fn main() { var x = 0xFF; x ^= 0x0F; println(x); }"),
        ("shl_assign", "fn main() { var x = 1; x <<= 4; println(x); }"),
        ("shr_assign", "fn main() { var x = 16; x >>= 2; println(x); }"),
        # Precedence mixing
        ("prec_add_mul", "fn main() { println(2 + 3 * 4); }"),
        ("prec_parens", "fn main() { println((2 + 3) * 4); }"),
        ("prec_logical_cmp", "fn main() { println(1 < 2 && 3 > 1); }"),
        ("prec_bit_cmp", "fn main() { println((1 & 3) == 1); }"),
        ("prec_shift_add", "fn main() { println((1 << 2) + 1); }"),
        ("prec_complex", "fn main() { println(1 + 2 * 3 - 4 / 2 + 5 % 3); }"),
        ("mixed_logical_bit", "fn main() { let x = (true && false) || (true and true); println(x); }"),
        # Unary
        ("negate_expr", "fn main() { let x = -(1 + 2); println(x); }"),
        ("double_negate", "fn main() { let x = -(-42); println(x); }"),
        ("not_not", "fn main() { let x = !!true; println(x); }"),
        # Division edge cases
        ("div_by_zero_int", "fn main() { let x = 1 / 0; }"),
        ("div_by_zero_float", "fn main() { let x = 1.0 / 0.0; println(x); }"),
        ("mod_by_zero", "fn main() { let x = 1 % 0; }"),
        # Range
        ("range_exclusive", "fn main() { for i in 0..5 { println(i); } }"),
        ("range_inclusive", "fn main() { for i in 0..=5 { println(i); } }"),
        ("range_negative", "fn main() { for i in -5..5 { println(i); } }"),
        ("range_empty", "fn main() { for i in 5..5 { println(i); } }"),
        ("range_backwards", "fn main() { for i in 5..0 { println(i); } }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"op/{label}"))


def fuzz_imports(ctx: FuzzContext) -> None:
    """Import and module system edge cases."""
    cases = [
        ("import_std_json", "import std::json;\nfn main() { }"),
        ("import_std_fs", "import std::fs;\nfn main() { }"),
        ("import_std_net", "import std::net;\nfn main() { }"),
        ("import_std_log", "import std::log;\nfn main() { }"),
        ("import_nonexistent_mod", "import std::nonexistent;\nfn main() { }"),
        ("import_nonexistent_path", "import foo::bar::baz;\nfn main() { }"),
        ("import_self", "import self;\nfn main() { }"),
        ("import_star", "import std::*;\nfn main() { }"),
        ("import_selective", "import std::json::{ parse, stringify };\nfn main() { }"),
        ("double_import", "import std::json;\nimport std::json;\nfn main() { }"),
        ("import_and_use", "import std::json;\nfn main() { let s = json.stringify(42); }"),
        ("many_imports", "\n".join(f"import std::mod{i};" for i in range(50)) + "\nfn main() { }"),
        ("import_deep_path", "import a::b::c::d::e::f;\nfn main() { }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"import/{label}"))


def fuzz_wire_types(ctx: FuzzContext) -> None:
    """Wire struct/enum declarations for serialization."""
    cases = [
        ("basic_wire_struct", (
            "wire struct Msg { name: string @1; age: u32 @2; }\n"
            "fn main() { }"
        )),
        ("wire_enum", (
            "wire enum Status { Active @1, Inactive @2, Pending @3 }\n"
            "fn main() { }"
        )),
        ("wire_optional", (
            "wire struct Config { host: string @1; optional port: u16 @2; }\n"
            "fn main() { }"
        )),
        ("wire_deprecated", (
            "wire struct Old { name: string @1; deprecated old_name: string @2; }\n"
            "fn main() { }"
        )),
        ("wire_nested", (
            "wire struct Inner { val: i32 @1; }\n"
            "wire struct Outer { inner: Inner @1; label: string @2; }\n"
            "fn main() { }"
        )),
        ("wire_list", (
            "wire struct Batch { items: list[string] @1; }\n"
            "fn main() { }"
        )),
        ("wire_json_attr", (
            "#[json(camelCase)]\n"
            "wire struct ApiResponse { status_code: i32 @1; error_message: string @2; }\n"
            "fn main() { }"
        )),
        ("wire_reserved", (
            "wire struct Evolving { name: string @1; reserved(2, 3); active: bool @4; }\n"
            "fn main() { }"
        )),
        ("wire_many_fields", (
            "wire struct Wide {\n"
            + "\n".join(f"  f{i}: i32 @{i+1};" for i in range(30))
            + "\n}\nfn main() { }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"wire/{label}"))


def fuzz_labels(ctx: FuzzContext) -> None:
    """Labeled loops with break/continue."""
    cases = [
        ("labeled_for", "@outer: for i in 0..10 { break @outer; }\nfn main() { }"),
        ("labeled_while", "fn main() { @outer: while true { break @outer; } }"),
        ("labeled_nested", (
            "fn main() {\n"
            "  @outer: for i in 0..10 {\n"
            "    for j in 0..10 {\n"
            "      if j == 5 { break @outer; }\n"
            "    }\n"
            "  }\n"
            "}"
        )),
        ("labeled_continue", (
            "fn main() {\n"
            "  @outer: for i in 0..10 {\n"
            "    for j in 0..10 {\n"
            "      if j == 5 { continue @outer; }\n"
            "    }\n"
            "  }\n"
            "}"
        )),
        ("multiple_labels", (
            "fn main() {\n"
            "  @a: for i in 0..5 {\n"
            "    @b: for j in 0..5 {\n"
            "      @c: for k in 0..5 {\n"
            "        if k == 2 { break @a; }\n"
            "      }\n"
            "    }\n"
            "  }\n"
            "}"
        )),
        ("break_label_value", "@loop: loop { break @loop 42; }\nfn main() { }"),
        ("label_shadowing", (
            "fn main() {\n"
            "  @x: for i in 0..5 {\n"
            "    @x: for j in 0..5 {\n"
            "      break @x;\n"
            "    }\n"
            "  }\n"
            "}"
        )),
        ("label_no_loop", "fn main() { @label: let x = 1; }"),
        ("nonexistent_label", "fn main() { for i in 0..10 { break @nonexistent; } }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"label/{label}"))


def fuzz_ffi(ctx: FuzzContext) -> None:
    """FFI / extern declarations."""
    cases = [
        ("extern_c", 'extern "C" { fn puts(s: *const u8) -> i32; }\nfn main() { }'),
        ("extern_no_abi", "extern { fn malloc(size: u64) -> *mut u8; }\nfn main() { }"),
        ("extern_variadic", 'extern "C" { fn printf(fmt: *const u8, ...) -> i32; }\nfn main() { }'),
        ("extern_many", (
            'extern "C" {\n'
            "  fn open(path: *const u8, flags: i32) -> i32;\n"
            "  fn close(fd: i32) -> i32;\n"
            "  fn read(fd: i32, buf: *mut u8, count: u64) -> i64;\n"
            "  fn write(fd: i32, buf: *const u8, count: u64) -> i64;\n"
            "}\nfn main() { }"
        )),
        ("foreign_keyword", 'foreign "C" { fn exit(code: i32); }\nfn main() { }'),
        ("unsafe_ffi_call", (
            'extern "C" { fn abs(x: i32) -> i32; }\n'
            "fn main() { unsafe { abs(-42); } }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"ffi/{label}"))


def fuzz_attributes(ctx: FuzzContext) -> None:
    """Attribute annotations."""
    cases = [
        ("test_attr", "#[test]\nfn test_it() { }"),
        ("ignore_attr", "#[test]\n#[ignore]\nfn test_slow() { }"),
        ("should_panic", "#[test]\n#[should_panic]\nfn test_fail() { panic(\"boom\"); }"),
        ("inline_attr", "#[inline]\nfn fast() -> i32 { 42 }\nfn main() { println(fast()); }"),
        ("unknown_attr", "#[banana]\nfn main() { }"),
        ("multiple_attrs", "#[test]\n#[ignore]\n#[inline]\nfn f() { }"),
        ("attr_with_args", '#[export("my_func")]\nfn f() -> i32 { 1 }\nfn main() { }'),
        ("attr_on_type", "#[derive(Debug)]\ntype Foo { x: i32; }\nfn main() { }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"attr/{label}"))


def fuzz_visibility(ctx: FuzzContext) -> None:
    """Visibility modifiers."""
    cases = [
        ("pub_fn", "pub fn greet() -> string { \"hello\" }\nfn main() { println(greet()); }"),
        ("pub_type", "pub type Point { pub x: i32; pub y: i32; }\nfn main() { }"),
        ("pub_package", "pub(package) fn internal() -> i32 { 42 }\nfn main() { }"),
        ("pub_super", "pub(super) fn parent_only() -> i32 { 1 }\nfn main() { }"),
        ("pub_trait", "pub trait Showable { fn show(self) -> string; }\nfn main() { }"),
        ("pub_enum", "pub enum State { On, Off }\nfn main() { }"),
        ("pub_actor", "pub actor Server { receive fn ping() { } }\nfn main() { }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"vis/{label}"))


def fuzz_tuples(ctx: FuzzContext) -> None:
    """Tuple types, construction, and destructuring."""
    cases = [
        ("unit_tuple", "fn main() { let x = (); }"),
        ("pair", "fn main() { let x = (1, 2); }"),
        ("triple", "fn main() { let x = (1, \"hello\", true); }"),
        ("nested_tuple", "fn main() { let x = ((1, 2), (3, 4)); }"),
        ("tuple_access", "fn main() { let x = (10, 20); println(x.0); }"),
        ("tuple_destructure", "fn main() { let (a, b, c) = (1, 2, 3); println(a + b + c); }"),
        ("tuple_return", "fn pair() -> (i32, i32) { (1, 2) }\nfn main() { let (a, b) = pair(); }"),
        ("single_element", "fn main() { let x = (42,); }"),
        ("large_tuple", "fn main() { let x = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10); }"),
        ("tuple_in_vec", "fn main() { let v: Vec<(i32, i32)> = Vec::new(); }"),
        ("tuple_mismatch", "fn main() { let (a, b) = (1, 2, 3); }"),
        ("empty_tuple_type", "fn f() -> () { }\nfn main() { f(); }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"tuple/{label}"))


def fuzz_duration_regex(ctx: FuzzContext) -> None:
    """Duration literals and regex patterns."""
    cases = [
        # Duration literals
        ("dur_ms", "fn main() { let d = 100ms; }"),
        ("dur_s", "fn main() { let d = 5s; }"),
        ("dur_m", "fn main() { let d = 1m; }"),
        ("dur_h", "fn main() { let d = 1h; }"),
        ("dur_zero", "fn main() { let d = 0ms; }"),
        ("dur_large", "fn main() { let d = 999999ms; }"),
        # Regex
        ("regex_basic", 'fn main() { let r = re"hello"; }'),
        ("regex_complex", 'fn main() { let r = re"^[a-z]+\\d{2,4}$"; }'),
        ("regex_empty", 'fn main() { let r = re""; }'),
        ("regex_special", 'fn main() { let r = re"[.*+?()\\[\\]{}|^$]"; }'),
        ("regex_match_op", 'fn main() { let x = "hello123" =~ re"[a-z]+\\d+"; }'),
        ("regex_no_match_op", 'fn main() { let x = "hello" !~ re"\\d+"; }'),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"durregex/{label}"))


def fuzz_unsafe_blocks(ctx: FuzzContext) -> None:
    """Unsafe blocks and raw pointer operations."""
    cases = [
        ("empty_unsafe", "fn main() { unsafe { } }"),
        ("unsafe_expr", "fn main() { let x = unsafe { 42 }; println(x); }"),
        ("nested_unsafe", "fn main() { unsafe { unsafe { println(1); } } }"),
        ("unsafe_in_if", "fn main() { if true { unsafe { println(1); } } }"),
        ("unsafe_in_for", "fn main() { for i in 0..3 { unsafe { println(i); } } }"),
        ("unsafe_fn", "unsafe fn danger() -> i32 { 42 }\nfn main() { unsafe { danger(); } }"),
        ("ptr_type", "fn main() { let p: *const i32 = 0 as *const i32; }"),
        ("mut_ptr_type", "fn main() { let p: *mut i32 = 0 as *mut i32; }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"unsafe/{label}"))


def fuzz_scope_concurrency(ctx: FuzzContext) -> None:
    """Structured concurrency: scope, select, join, cooperate."""
    cases = [
        ("basic_scope", "fn main() { scope { println(1); }; }"),
        ("scope_with_binding", "fn main() { scope |s| { println(1); }; }"),
        ("scope_launch", (
            "fn main() {\n"
            "  scope |s| {\n"
            "    let t = s.launch { 42 };\n"
            "    let r = await t;\n"
            "    println(r);\n"
            "  };\n"
            "}"
        )),
        ("scope_multiple_tasks", (
            "fn main() {\n"
            "  scope |s| {\n"
            "    let t1 = s.launch { 1 };\n"
            "    let t2 = s.launch { 2 };\n"
            "    let t3 = s.launch { 3 };\n"
            "  };\n"
            "}"
        )),
        ("select_basic", (
            "actor A { receive fn val() -> i32 { 1 } }\n"
            "actor B { receive fn val() -> i32 { 2 } }\n"
            "fn main() {\n"
            "  let a = spawn A(); let b = spawn B();\n"
            "  select { a.val() => println(1), b.val() => println(2) }\n"
            "}"
        )),
        ("join_basic", (
            "actor A { receive fn val() -> i32 { 1 } }\n"
            "actor B { receive fn val() -> i32 { 2 } }\n"
            "fn main() {\n"
            "  let a = spawn A(); let b = spawn B();\n"
            "  join(a.val(), b.val());\n"
            "}"
        )),
        ("cooperate_in_loop", "fn main() { for i in 0..100 { cooperate; } }"),
        ("nested_scope", "fn main() { scope { scope { println(1); }; }; }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"concurrency/{label}"))


def fuzz_supervisor(ctx: FuzzContext) -> None:
    """Supervisor declarations and patterns."""
    cases = [
        ("basic_supervisor", (
            "actor Worker {\n"
            "  receive fn work() { println(1); }\n"
            "}\n"
            "supervisor WorkerSup {\n"
            "  strategy: one_for_one;\n"
            "  max_restarts: 3;\n"
            "  window: 5s;\n"
            "  child worker: Worker();\n"
            "}\n"
            "fn main() { let sup = spawn WorkerSup(); }"
        )),
        ("all_for_one", (
            "actor A { receive fn go() { } }\n"
            "actor B { receive fn go() { } }\n"
            "supervisor AllSup {\n"
            "  strategy: one_for_all;\n"
            "  max_restarts: 5;\n"
            "  window: 10s;\n"
            "  child a: A();\n"
            "  child b: B();\n"
            "}\n"
            "fn main() { }"
        )),
        ("rest_for_one", (
            "actor A { receive fn go() { } }\n"
            "actor B { receive fn go() { } }\n"
            "supervisor RestSup {\n"
            "  strategy: rest_for_one;\n"
            "  max_restarts: 3;\n"
            "  window: 5s;\n"
            "  child a: A();\n"
            "  child b: B();\n"
            "}\n"
            "fn main() { }"
        )),
        ("child_restart_permanent", (
            "actor W { receive fn go() { } }\n"
            "supervisor S {\n"
            "  strategy: one_for_one;\n"
            "  max_restarts: 3;\n"
            "  window: 5s;\n"
            "  child w: W() permanent;\n"
            "}\n"
            "fn main() { }"
        )),
        ("child_restart_transient", (
            "actor W { receive fn go() { } }\n"
            "supervisor S {\n"
            "  strategy: one_for_one;\n"
            "  max_restarts: 3;\n"
            "  window: 5s;\n"
            "  child w: W() transient;\n"
            "}\n"
            "fn main() { }"
        )),
        ("child_restart_temporary", (
            "actor W { receive fn go() { } }\n"
            "supervisor S {\n"
            "  strategy: one_for_one;\n"
            "  max_restarts: 3;\n"
            "  window: 5s;\n"
            "  child w: W() temporary;\n"
            "}\n"
            "fn main() { }"
        )),
        ("many_children", (
            "\n".join(f"actor W{i} {{ receive fn go() {{ }} }}" for i in range(20)) + "\n"
            "supervisor BigSup {\n"
            "  strategy: one_for_one;\n"
            "  max_restarts: 10;\n"
            "  window: 30s;\n"
            + "\n".join(f"  child w{i}: W{i}();" for i in range(20))
            + "\n}\nfn main() { }"
        )),
        ("supervisor_with_init_args", (
            "actor Worker {\n"
            "  id: i32;\n"
            "  receive fn go() { println(self.id); }\n"
            "}\n"
            "supervisor S {\n"
            "  strategy: one_for_one;\n"
            "  max_restarts: 3;\n"
            "  window: 5s;\n"
            "  child w: Worker(id: 42);\n"
            "}\n"
            "fn main() { let s = spawn S(); }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"supervisor/{label}"))


def fuzz_mailbox(ctx: FuzzContext) -> None:
    """Actor mailbox configurations."""
    cases = [
        ("default_mailbox", "actor A { receive fn go() { } }\nfn main() { let a = spawn A(); }"),
        ("bounded_mailbox", (
            "actor A {\n"
            "  mailbox 100;\n"
            "  receive fn go() { }\n"
            "}\nfn main() { }"
        )),
        ("mailbox_block", (
            "actor A {\n"
            "  mailbox 10 overflow block;\n"
            "  receive fn go() { }\n"
            "}\nfn main() { }"
        )),
        ("mailbox_drop_new", (
            "actor A {\n"
            "  mailbox 10 overflow drop_new;\n"
            "  receive fn go() { }\n"
            "}\nfn main() { }"
        )),
        ("mailbox_drop_old", (
            "actor A {\n"
            "  mailbox 10 overflow drop_old;\n"
            "  receive fn go() { }\n"
            "}\nfn main() { }"
        )),
        ("mailbox_fail", (
            "actor A {\n"
            "  mailbox 10 overflow fail;\n"
            "  receive fn go() { }\n"
            "}\nfn main() { }"
        )),
        ("mailbox_zero", (
            "actor A {\n"
            "  mailbox 0;\n"
            "  receive fn go() { }\n"
            "}\nfn main() { }"
        )),
        ("mailbox_huge", (
            "actor A {\n"
            "  mailbox 1000000;\n"
            "  receive fn go() { }\n"
            "}\nfn main() { }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"mailbox/{label}"))


# ---------------------------------------------------------------------------
# NEW: Compiler testing techniques
# ---------------------------------------------------------------------------

def fuzz_differential(ctx: FuzzContext) -> None:
    """Differential testing: programs that pass `hew check` must not crash
    `hew build`.  If check succeeds but build crashes, that's a bug."""
    programs = [
        ("fn_returning_struct", (
            "type Point { x: i32; y: i32; }\n"
            "fn origin() -> Point { Point { x: 0, y: 0 } }\n"
            "fn main() { let p = origin(); println(p.x); }"
        )),
        ("match_option", (
            "fn maybe(b: bool) -> Option<i32> { if b { Some(1) } else { None } }\n"
            "fn main() { match maybe(true) { Some(v) => println(v), None => println(0) } }"
        )),
        ("generic_fn", (
            "fn identity<T>(x: T) -> T { x }\n"
            "fn main() { println(identity(42)); }"
        )),
        ("closure_in_var", (
            "fn main() {\n"
            "  let add = |a: i32, b: i32| -> i32 { a + b };\n"
            "  println(add(1, 2));\n"
            "}"
        )),
        ("vec_operations", (
            "fn main() {\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  v.push(1); v.push(2); v.push(3);\n"
            "  println(v.len());\n"
            "}"
        )),
        ("hashmap_operations", (
            "fn main() {\n"
            "  let m: HashMap<string, i32> = HashMap::new();\n"
            "  m.insert(\"a\", 1);\n"
            "  m.insert(\"b\", 2);\n"
            "  println(m.len());\n"
            "}"
        )),
        ("nested_if_match", (
            "fn classify(x: i32) -> string {\n"
            "  if x > 0 {\n"
            "    match x { 1 => \"one\", 2 => \"two\", _ => \"many\" }\n"
            "  } else if x == 0 { \"zero\" }\n"
            "  else { \"negative\" }\n"
            "}\n"
            "fn main() { println(classify(1)); }"
        )),
        ("for_with_vec", (
            "fn main() {\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  v.push(10); v.push(20); v.push(30);\n"
            "  var sum = 0;\n"
            "  for x in v { sum += x; }\n"
            "  println(sum);\n"
            "}"
        )),
        ("string_interpolation", (
            "fn main() {\n"
            "  let name = \"world\";\n"
            "  let age = 42;\n"
            '  let msg = f"hello {name}, age {age}";\n'
            "  println(msg);\n"
            "}"
        )),
        ("actor_with_state", (
            "actor Counter {\n"
            "  count: i32;\n"
            "  receive fn increment() { self.count = self.count + 1; }\n"
            "  receive fn get_count() -> i32 { self.count }\n"
            "}\n"
            "fn main() {\n"
            "  let c = spawn Counter(count: 0);\n"
            "  c.increment(); c.increment();\n"
            "  let n = await c.get_count();\n"
            "  println(n);\n"
            "}"
        )),
        ("multiple_actors", (
            "actor Adder { receive fn add(a: i32, b: i32) -> i32 { a + b } }\n"
            "actor Multiplier { receive fn mul(a: i32, b: i32) -> i32 { a * b } }\n"
            "fn main() {\n"
            "  let a = spawn Adder(); let m = spawn Multiplier();\n"
            "  let sum = await a.add(3, 4);\n"
            "  let prod = await m.mul(3, 4);\n"
            "  println(sum); println(prod);\n"
            "}"
        )),
        ("struct_with_methods", (
            "type Rect { w: i32; h: i32; }\n"
            "impl Rect {\n"
            "  fn area(self) -> i32 { self.w * self.h }\n"
            "  fn perimeter(self) -> i32 { 2 * (self.w + self.h) }\n"
            "}\n"
            "fn main() { let r = Rect { w: 3, h: 4 }; println(r.area()); }"
        )),
        ("recursive_fn", (
            "fn factorial(n: i32) -> i32 {\n"
            "  if n <= 1 { 1 } else { n * factorial(n - 1) }\n"
            "}\n"
            "fn main() { println(factorial(10)); }"
        )),
        ("mutual_recursion", (
            "fn is_even(n: i32) -> bool { if n == 0 { true } else { is_odd(n - 1) } }\n"
            "fn is_odd(n: i32) -> bool { if n == 0 { false } else { is_even(n - 1) } }\n"
            "fn main() { println(is_even(10)); }"
        )),
        ("higher_order_fn", (
            "fn apply(f: fn(i32) -> i32, x: i32) -> i32 { f(x) }\n"
            "fn double(x: i32) -> i32 { x * 2 }\n"
            "fn main() { println(apply(double, 21)); }"
        )),
        ("enum_match_exhaustive", (
            "enum Color { Red, Green, Blue }\n"
            "fn name(c: Color) -> string {\n"
            "  match c { Color::Red => \"red\", Color::Green => \"green\", Color::Blue => \"blue\" }\n"
            "}\n"
            "fn main() { println(name(Color::Red)); }"
        )),
        ("chained_methods", (
            "fn main() {\n"
            "  let s = \"hello world\";\n"
            "  println(s.len());\n"
            "  println(s.contains(\"world\"));\n"
            "  println(s.to_uppercase());\n"
            "}"
        )),
        ("tuple_operations", (
            "fn swap(t: (i32, i32)) -> (i32, i32) { (t.1, t.0) }\n"
            "fn main() { let (a, b) = swap((1, 2)); println(a); println(b); }"
        )),
    ]

    for label, src in programs:
        # First: check must not crash
        check_result = run_hew(ctx, src, f"diff/check_{label}", mode="check")
        if check_result is not None:
            continue  # check itself crashed — already recorded

        # Second: build must not crash (may error due to codegen limitations)
        build_result = run_hew(ctx, src, f"diff/build_{label}")
        if build_result is not None and build_result.status == "crash":
            build_result.label = f"diff/DIVERGE_{label}"
            print(f"  !! DIFFERENTIAL: {label} passes check but crashes build")


def fuzz_cross_feature(ctx: FuzzContext) -> None:
    """Cross-feature interactions: combining multiple language features."""
    cases = [
        ("actor_plus_closure", (
            "actor Processor {\n"
            "  receive fn process(data: Vec<i32>) -> i32 {\n"
            "    var sum = 0;\n"
            "    for x in data { sum = sum + x; }\n"
            "    sum\n"
            "  }\n"
            "}\n"
            "fn main() {\n"
            "  let p = spawn Processor();\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  v.push(1); v.push(2); v.push(3);\n"
            "  let result = await p.process(v);\n"
            "  println(result);\n"
            "}"
        )),
        ("match_in_actor", (
            "enum Cmd { Inc, Dec, Reset }\n"
            "actor Counter {\n"
            "  val: i32;\n"
            "  receive fn exec(cmd: Cmd) {\n"
            "    match cmd {\n"
            "      Cmd::Inc => { self.val = self.val + 1; },\n"
            "      Cmd::Dec => { self.val = self.val - 1; },\n"
            "      Cmd::Reset => { self.val = 0; }\n"
            "    }\n"
            "  }\n"
            "  receive fn get() -> i32 { self.val }\n"
            "}\n"
            "fn main() { let c = spawn Counter(val: 0); }"
        )),
        ("generic_with_option_result", (
            "fn first_ok<T>(items: Vec<Result<T, string>>) -> Option<T> {\n"
            "  for item in items {\n"
            "    match item { Ok(v) => { return Some(v); }, Err(_) => { } }\n"
            "  }\n"
            "  None\n"
            "}\n"
            "fn main() { }"
        )),
        ("struct_with_vec_and_hashmap", (
            "type Database {\n"
            "  tables: Vec<string>;\n"
            "  metadata: HashMap<string, string>;\n"
            "}\n"
            "fn main() {\n"
            "  let db = Database {\n"
            "    tables: Vec::new(),\n"
            "    metadata: HashMap::new()\n"
            "  };\n"
            "  db.tables.push(\"users\");\n"
            "  db.metadata.insert(\"version\", \"1.0\");\n"
            "}"
        )),
        ("defer_plus_match", (
            "fn process(x: i32) -> string {\n"
            "  defer println(0);\n"
            "  match x {\n"
            "    1 => { defer println(1); return \"one\"; },\n"
            "    2 => { defer println(2); return \"two\"; },\n"
            "    _ => { defer println(3); return \"other\"; }\n"
            "  }\n"
            "}\n"
            "fn main() { println(process(2)); }"
        )),
        ("for_loop_with_match_and_break", (
            "fn find_first_even(v: Vec<i32>) -> Option<i32> {\n"
            "  for x in v {\n"
            "    match x % 2 {\n"
            "      0 => { return Some(x); },\n"
            "      _ => { continue; }\n"
            "    }\n"
            "  }\n"
            "  None\n"
            "}\n"
            "fn main() { }"
        )),
        ("lambda_actor_with_closure", (
            "fn main() {\n"
            "  let multiplier = 10;\n"
            "  let worker = spawn (x: i32) => {\n"
            "    println(x * multiplier);\n"
            "  };\n"
            "  worker.send(5);\n"
            "}"
        )),
        ("nested_actors_with_await", (
            "actor Inner { receive fn compute(x: i32) -> i32 { x * x } }\n"
            "actor Outer {\n"
            "  receive fn run(x: i32) -> i32 {\n"
            "    let inner = spawn Inner();\n"
            "    let result = await inner.compute(x);\n"
            "    result + 1\n"
            "  }\n"
            "}\n"
            "fn main() {\n"
            "  let o = spawn Outer();\n"
            "  let r = await o.run(5);\n"
            "  println(r);\n"
            "}"
        )),
        ("string_interp_with_method", (
            "fn main() {\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  v.push(1); v.push(2); v.push(3);\n"
            '  let msg = f"vec has {v.len()} items";\n'
            "  println(msg);\n"
            "}"
        )),
        ("if_expr_in_match_in_for", (
            "fn main() {\n"
            "  for i in 0..10 {\n"
            "    let label = match i % 3 {\n"
            "      0 => if i == 0 { \"start\" } else { \"fizz\" },\n"
            "      _ => \"other\"\n"
            "    };\n"
            "    println(label);\n"
            "  }\n"
            "}"
        )),
        ("trait_with_generics_and_enum", (
            "trait Describable { fn describe(self) -> string; }\n"
            "enum Shape { Circle(f64), Square(f64) }\n"
            "impl Describable for Shape {\n"
            "  fn describe(self) -> string {\n"
            "    match self { Shape::Circle(_) => \"circle\", Shape::Square(_) => \"square\" }\n"
            "  }\n"
            "}\n"
            "fn main() { }"
        )),
        ("multiple_returns_with_defer", (
            "fn complex(x: i32) -> i32 {\n"
            "  defer println(0);\n"
            "  if x < 0 { return -1; }\n"
            "  defer println(1);\n"
            "  if x == 0 { return 0; }\n"
            "  defer println(2);\n"
            "  for i in 0..x {\n"
            "    if i > 100 { return 100; }\n"
            "  }\n"
            "  x\n"
            "}\n"
            "fn main() { println(complex(5)); }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"cross/{label}"))


def fuzz_error_cascade(ctx: FuzzContext) -> None:
    """Multiple errors in one file — compiler should not crash."""
    cases = [
        ("many_type_errors", (
            "fn main() {\n"
            "  let x: i32 = \"wrong\";\n"
            "  let y: string = 42;\n"
            "  let z: bool = 3.14;\n"
            "  let w: f64 = true;\n"
            "  println(x + y + z + w);\n"
            "}"
        )),
        ("many_syntax_errors", (
            "fn main() {\n"
            "  let = 1;\n"
            "  let x = ;\n"
            "  fn ;\n"
            "  if { }\n"
            "  for { }\n"
            "}"
        )),
        ("undefined_everywhere", (
            "fn main() {\n"
            "  println(a);\n"
            "  println(b);\n"
            "  println(c);\n"
            "  let x = foo();\n"
            "  let y = bar();\n"
            "  let z = baz();\n"
            "  a.method();\n"
            "  b[0] = c;\n"
            "}"
        )),
        ("mixed_errors", (
            "fn f(x: i32) -> string { x }\n"
            "fn g() -> i32 { \"wrong\" }\n"
            "fn h(a: i32, b: i32) -> i32 { a }\n"
            "fn main() {\n"
            "  f(\"wrong\");\n"
            "  g();\n"
            "  h(1);\n"
            "  h(1, 2, 3);\n"
            "  unknown_function();\n"
            "}"
        )),
        ("error_after_error", (
            "fn main() {\n"
            "  let x = undefined1;\n"
            "  let y = x + undefined2;\n"
            "  let z = y.field;\n"
            "  z.method(undefined3);\n"
            "}"
        )),
        ("50_errors", (
            "fn main() {\n"
            + "\n".join(f"  let v{i}: i32 = \"str{i}\";" for i in range(50))
            + "\n}"
        )),
        ("recover_after_bad_fn", (
            "fn broken(\n"  # Syntax error
            "fn main() { println(42); }\n"  # Should still parse
        )),
        ("unterminated_string_then_code", (
            'fn main() { let x = "unterminated;\n'
            "  let y = 42;\n"
            "  println(y);\n"
            "}"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"cascade/{label}"))


def fuzz_type_inference(ctx: FuzzContext) -> None:
    """Programs that stress bidirectional type inference."""
    cases = [
        ("infer_int_literal", "fn main() { let x = 42; println(x); }"),
        ("infer_from_return", "fn f() -> i32 { 42 }\nfn main() { let x = f(); println(x); }"),
        ("infer_from_arg", "fn f(x: i32) -> i32 { x }\nfn main() { let x = f(42); }"),
        ("infer_in_if", "fn main() { let x = if true { 1 } else { 2 }; println(x); }"),
        ("infer_in_match", "fn main() { let x = match 1 { 1 => 10, _ => 20 }; println(x); }"),
        ("infer_vec_element", "fn main() { let v: Vec<i32> = Vec::new(); v.push(42); let x = v.get(0); }"),
        ("infer_closure_return", "fn main() { let f = |x: i32| -> i32 { x + 1 }; let y = f(41); println(y); }"),
        ("infer_chain", (
            "fn a() -> i32 { 1 }\n"
            "fn b(x: i32) -> i32 { x + 1 }\n"
            "fn c(x: i32) -> string { \"done\" }\n"
            "fn main() { let x = c(b(a())); println(x); }"
        )),
        ("infer_generic", "fn id<T>(x: T) -> T { x }\nfn main() { let x = id(42); let y = id(\"hi\"); }"),
        ("infer_nested_generic", (
            "fn wrap<T>(x: T) -> Option<T> { Some(x) }\n"
            "fn main() { let x = wrap(wrap(42)); }"
        )),
        ("conflicting_inference", (
            "fn main() {\n"
            "  let x = 42;\n"
            "  let y: string = x;\n"  # Should error
            "}"
        )),
        ("diamond_inference", (
            "fn f<T>(a: T, b: T) -> T { a }\n"
            "fn main() { let x = f(1, 2); println(x); }"
        )),
        ("type_annotation_override", "fn main() { let x: i32 = 42; let y: i64 = 42; }"),
        ("numeric_coercion", "fn main() { let x: f64 = 42; println(x); }"),
        ("ambiguous_numeric", "fn main() { let x = 1 + 2; let y = x + 3; println(y); }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"infer/{label}"))


def fuzz_recursion(ctx: FuzzContext) -> None:
    """Recursive patterns: mutual recursion, deep recursion, recursive types."""
    cases = [
        ("simple_recursion", "fn f(n: i32) -> i32 { if n == 0 { 0 } else { f(n - 1) } }\nfn main() { println(f(10)); }"),
        ("mutual_recursion", (
            "fn ping(n: i32) -> i32 { if n == 0 { 0 } else { pong(n - 1) } }\n"
            "fn pong(n: i32) -> i32 { if n == 0 { 1 } else { ping(n - 1) } }\n"
            "fn main() { println(ping(10)); }"
        )),
        ("triple_mutual", (
            "fn a(n: i32) -> i32 { if n <= 0 { 0 } else { b(n - 1) } }\n"
            "fn b(n: i32) -> i32 { if n <= 0 { 1 } else { c(n - 1) } }\n"
            "fn c(n: i32) -> i32 { if n <= 0 { 2 } else { a(n - 1) } }\n"
            "fn main() { println(a(9)); }"
        )),
        ("recursive_type", "type List { val: i32; next: Option<Box<List>>; }\nfn main() { }"),
        ("recursive_enum", (
            "enum Expr { Num(i32), Add(Box<Expr>, Box<Expr>), Mul(Box<Expr>, Box<Expr>) }\n"
            "fn main() { }"
        )),
        ("recursive_actor_spawn", (
            "actor Tree {\n"
            "  depth: i32;\n"
            "  receive fn grow() {\n"
            "    if self.depth > 0 {\n"
            "      let left = spawn Tree(depth: self.depth - 1);\n"
            "      let right = spawn Tree(depth: self.depth - 1);\n"
            "    }\n"
            "  }\n"
            "}\n"
            "fn main() { let t = spawn Tree(depth: 3); t.grow(); }"
        )),
        ("forward_reference", "fn main() { println(f(5)); }\nfn f(n: i32) -> i32 { n * 2 }"),
        ("fn_as_arg_recursive", (
            "fn apply_n(f: fn(i32) -> i32, x: i32, n: i32) -> i32 {\n"
            "  if n == 0 { x } else { apply_n(f, f(x), n - 1) }\n"
            "}\n"
            "fn inc(x: i32) -> i32 { x + 1 }\n"
            "fn main() { println(apply_n(inc, 0, 10)); }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"recursion/{label}"))


def fuzz_semantic_oracle(ctx: FuzzContext) -> None:
    """Programs with known correct output — compilation must succeed.
    These serve as regression tests for the full pipeline."""
    programs = [
        ("hello", 'fn main() { println("hello"); }'),
        ("arithmetic", "fn main() { println(2 + 3 * 4); }"),
        ("fibonacci", (
            "fn fib(n: i32) -> i32 { if n <= 1 { n } else { fib(n-1) + fib(n-2) } }\n"
            "fn main() { println(fib(10)); }"
        )),
        ("string_ops", (
            "fn main() {\n"
            '  let s = "hello";\n'
            "  println(s.len());\n"
            '  println(s + " world");\n'
            "}"
        )),
        ("vec_basics", (
            "fn main() {\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  v.push(1); v.push(2); v.push(3);\n"
            "  println(v.len());\n"
            "  println(v.get(0));\n"
            "}"
        )),
        ("for_loop_sum", (
            "fn main() {\n"
            "  var sum = 0;\n"
            "  for i in 0..10 { sum = sum + i; }\n"
            "  println(sum);\n"
            "}"
        )),
        ("while_loop", (
            "fn main() {\n"
            "  var x = 1;\n"
            "  while x < 100 { x = x * 2; }\n"
            "  println(x);\n"
            "}"
        )),
        ("match_expr", (
            "fn classify(n: i32) -> string {\n"
            '  match n { 0 => "zero", 1 => "one", _ => "many" }\n'
            "}\n"
            'fn main() { println(classify(0)); println(classify(1)); println(classify(99)); }'
        )),
        ("nested_fn", (
            "fn outer(x: i32) -> i32 { inner(x) + 1 }\n"
            "fn inner(x: i32) -> i32 { x * 2 }\n"
            "fn main() { println(outer(5)); }"
        )),
        ("var_mutation", (
            "fn main() {\n"
            "  var x = 0;\n"
            "  x = x + 1; x = x + 2; x = x + 3;\n"
            "  println(x);\n"
            "}"
        )),
        ("bool_logic", (
            "fn main() {\n"
            "  println(true && true);\n"
            "  println(true && false);\n"
            "  println(false || true);\n"
            "  println(!false);\n"
            "}"
        )),
        ("early_return", (
            "fn first_positive(a: i32, b: i32, c: i32) -> i32 {\n"
            "  if a > 0 { return a; }\n"
            "  if b > 0 { return b; }\n"
            "  c\n"
            "}\n"
            "fn main() { println(first_positive(-1, -2, 3)); }"
        )),
    ]
    for label, src in programs:
        _report(run_hew(ctx, src, f"oracle/{label}"))


def fuzz_comment_placement(ctx: FuzzContext) -> None:
    """Comments in unusual places."""
    cases = [
        ("comment_everywhere", (
            "/* a */ fn /* b */ main /* c */ ( /* d */ ) /* e */ { /* f */\n"
            "  let /* g */ x /* h */ = /* i */ 42 /* j */ ; /* k */\n"
            "  println(x);\n"
            "/* l */ }"
        )),
        ("doc_comment_fn", "/// This is a documented function\nfn f() -> i32 { 42 }\nfn main() { println(f()); }"),
        ("doc_comment_type", "/// A point in 2D space\ntype Point { /// X coordinate\n x: i32; /// Y coordinate\n y: i32; }\nfn main() { }"),
        ("nested_block_comment", "/* outer /* inner /* deep */ inner */ outer */\nfn main() { println(1); }"),
        ("comment_in_string", 'fn main() { let x = "/* not a comment */"; println(x); }'),
        ("string_in_comment", '/* let x = "hello"; */\nfn main() { println(1); }'),
        ("comment_between_tokens", "fn main() { let x = 1 /* gap */ + /* gap */ 2; println(x); }"),
        ("line_comment_at_eof", "fn main() { println(1); } // end"),
        ("only_comments", "// line 1\n// line 2\n/* block */"),
        ("empty_block_comment", "/**/\nfn main() { }"),
        ("comment_with_code_chars", '// fn main() { let x = "};!@#$%"; }\nfn main() { println(1); }'),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"comment/{label}"))


def fuzz_whitespace(ctx: FuzzContext) -> None:
    """Whitespace sensitivity and formatting edge cases."""
    cases = [
        ("no_whitespace", "fn main(){let x=1+2;println(x);}"),
        ("excessive_whitespace", "fn   main  (  )   {   let   x   =   1  +  2  ;   println ( x ) ;   }"),
        ("tabs_only", "fn\tmain()\t{\tlet\tx\t=\t42;\tprintln(x);\t}"),
        ("mixed_indent", "fn main() {\n\t  \t let x = 1;\n  \t\t  println(x);\n}"),
        ("windows_newlines", "fn main() {\r\n  let x = 42;\r\n  println(x);\r\n}\r\n"),
        ("mac_newlines", "fn main() {\r  let x = 42;\r  println(x);\r}\r"),
        ("trailing_whitespace", "fn main() {   \n  let x = 42;   \n  println(x);   \n}   \n"),
        ("leading_whitespace", "   \n   \n   fn main() { println(42); }"),
        ("no_final_newline", "fn main() { println(42); }"),
        ("many_blank_lines", "\n\n\n\n\n\nfn main() {\n\n\n\n\n\n  println(42);\n\n\n\n\n\n}\n\n\n\n\n"),
        ("form_feed", "fn main() {\f println(42); }"),
        ("vertical_tab", "fn main() {\v println(42); }"),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"ws/{label}"))


def fuzz_collections_deep(ctx: FuzzContext) -> None:
    """Deeper collection usage patterns."""
    cases = [
        ("vec_of_strings", (
            "fn main() {\n"
            '  let v: Vec<string> = Vec::new();\n'
            '  v.push("hello"); v.push("world");\n'
            "  for s in v { println(s); }\n"
            "}"
        )),
        ("hashmap_iteration", (
            "fn main() {\n"
            "  let m: HashMap<string, i32> = HashMap::new();\n"
            '  m.insert("a", 1); m.insert("b", 2);\n'
            "  println(m.len());\n"
            "}"
        )),
        ("vec_remove", (
            "fn main() {\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  v.push(1); v.push(2); v.push(3);\n"
            "  v.remove(1);\n"
            "  println(v.len());\n"
            "}"
        )),
        ("vec_pop", (
            "fn main() {\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  v.push(1); v.push(2);\n"
            "  let last = v.pop();\n"
            "  println(v.len());\n"
            "}"
        )),
        ("hashmap_contains", (
            "fn main() {\n"
            "  let m: HashMap<string, i32> = HashMap::new();\n"
            '  m.insert("key", 42);\n'
            '  println(m.contains_key("key"));\n'
            '  println(m.contains_key("missing"));\n'
            "}"
        )),
        ("hashmap_remove", (
            "fn main() {\n"
            "  let m: HashMap<string, i32> = HashMap::new();\n"
            '  m.insert("key", 42);\n'
            '  m.remove("key");\n'
            "  println(m.len());\n"
            "}"
        )),
        ("nested_vec", (
            "fn main() {\n"
            "  let v: Vec<Vec<i32>> = Vec::new();\n"
            "  let inner: Vec<i32> = Vec::new();\n"
            "  inner.push(1); inner.push(2);\n"
            "  v.push(inner);\n"
            "}"
        )),
        ("vec_set", (
            "fn main() {\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  v.push(1); v.push(2); v.push(3);\n"
            "  v.set(1, 99);\n"
            "  println(v.get(1));\n"
            "}"
        )),
        ("empty_vec_operations", (
            "fn main() {\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  println(v.len());\n"
            "}"
        )),
        ("empty_hashmap_operations", (
            "fn main() {\n"
            "  let m: HashMap<string, i32> = HashMap::new();\n"
            "  println(m.len());\n"
            '  println(m.contains_key("x"));\n'
            "}"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"collections/{label}"))


def fuzz_actor_lifecycle(ctx: FuzzContext) -> None:
    """Actor lifecycle: init, stop, link, monitor."""
    cases = [
        ("actor_with_init", (
            "actor Server {\n"
            "  ready: bool;\n"
            "  init() { self.ready = true; }\n"
            "  receive fn is_ready() -> bool { self.ready }\n"
            "}\n"
            "fn main() { let s = spawn Server(ready: false); }"
        )),
        ("actor_stop", (
            "actor Worker {\n"
            "  receive fn finish() { stop(); }\n"
            "}\n"
            "fn main() { let w = spawn Worker(); w.finish(); }"
        )),
        ("actor_link", (
            "actor Parent {\n"
            "  receive fn start() {\n"
            "    let child = spawn (x: i32) => { println(x); };\n"
            "    link(child);\n"
            "  }\n"
            "}\n"
            "fn main() { let p = spawn Parent(); p.start(); }"
        )),
        ("actor_monitor", (
            "actor Watcher {\n"
            "  receive fn watch() {\n"
            "    let target = spawn (x: i32) => { println(x); };\n"
            "    monitor(target);\n"
            "  }\n"
            "}\n"
            "fn main() { let w = spawn Watcher(); w.watch(); }"
        )),
        ("actor_many_handlers", (
            "actor Service {\n"
            "  val: i32;\n"
            + "\n".join(f"  receive fn method{i}() -> i32 {{ self.val + {i} }}" for i in range(20))
            + "\n}\nfn main() { let s = spawn Service(val: 0); }"
        )),
        ("actor_with_collections", (
            "actor Store {\n"
            "  items: Vec<string>;\n"
            '  receive fn add(item: string) { self.items.push(item); }\n'
            "  receive fn count() -> i32 { self.items.len() }\n"
            "}\n"
            "fn main() { let s = spawn Store(items: Vec::new()); }"
        )),
        ("basic_actor", (
            "actor Pure {\n"
            "  val: i32;\n"
            "  receive fn get() -> i32 { self.val }\n"
            "}\n"
            "fn main() { let p = spawn Pure(val: 42); }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"lifecycle/{label}"))


def fuzz_try_operator(ctx: FuzzContext) -> None:
    """Postfix try (?) operator with Result/Option."""
    cases = [
        ("try_result", (
            "fn may_fail() -> Result<i32, string> { Ok(42) }\n"
            "fn caller() -> Result<i32, string> { let x = may_fail()?; Ok(x + 1) }\n"
            "fn main() { }"
        )),
        ("try_chain", (
            "fn a() -> Result<i32, string> { Ok(1) }\n"
            "fn b(x: i32) -> Result<i32, string> { Ok(x + 1) }\n"
            "fn chain() -> Result<i32, string> { let x = a()?; let y = b(x)?; Ok(y) }\n"
            "fn main() { }"
        )),
        ("try_option", (
            "fn find() -> Option<i32> { Some(42) }\n"
            "fn use_it() -> Option<i32> { let x = find()?; Some(x + 1) }\n"
            "fn main() { }"
        )),
        ("try_in_match", (
            "fn get() -> Result<i32, string> { Ok(42) }\n"
            "fn process() -> Result<string, string> {\n"
            '  let x = get()?;\n'
            '  Ok(match x { 42 => "found", _ => "other" })\n'
            "}\n"
            "fn main() { }"
        )),
        ("try_in_loop", (
            "fn step(i: i32) -> Result<i32, string> { Ok(i) }\n"
            "fn run() -> Result<i32, string> {\n"
            "  var sum = 0;\n"
            "  for i in 0..5 { sum = sum + step(i)?; }\n"
            "  Ok(sum)\n"
            "}\n"
            "fn main() { }"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"try/{label}"))


def fuzz_method_chains(ctx: FuzzContext) -> None:
    """Method chaining patterns."""
    cases = [
        ("string_methods", (
            'fn main() {\n'
            '  let s = "Hello World";\n'
            '  println(s.len());\n'
            '  println(s.to_lowercase());\n'
            '  println(s.to_uppercase());\n'
            '  println(s.trim());\n'
            '  println(s.contains("World"));\n'
            '  println(s.starts_with("Hello"));\n'
            '  println(s.ends_with("World"));\n'
            '}'
        )),
        ("string_replace", 'fn main() { let s = "hello world"; println(s.replace("world", "hew")); }'),
        ("string_split", 'fn main() { let parts = "a,b,c".split(","); println(parts.len()); }'),
        ("string_substring", 'fn main() { let s = "hello"; println(s.substring(0, 3)); }'),
        ("string_char_at", 'fn main() { let s = "hello"; println(s.char_at(0)); }'),
        ("string_index_of", 'fn main() { let s = "hello world"; println(s.index_of("world")); }'),
        ("vec_chain", (
            "fn main() {\n"
            "  let v: Vec<i32> = Vec::new();\n"
            "  v.push(3); v.push(1); v.push(2);\n"
            "  println(v.len());\n"
            "  println(v.get(0));\n"
            "}"
        )),
    ]
    for label, src in cases:
        _report(run_hew(ctx, src, f"methods/{label}"))


def fuzz_random_well_typed(ctx: FuzzContext, n: int = 100) -> None:
    """Generate random programs that are more likely to be well-typed,
    using a structured approach (CSmith-inspired)."""
    for i in range(n):
        num_fns = random.randint(0, 3)
        fns = []
        fn_names = []
        for j in range(num_fns):
            name = f"f{j}"
            fn_names.append(name)
            ret_ty = random.choice(["i32", "i64", "f64", "bool", "string"])
            nparams = random.randint(0, 3)
            params = ", ".join(f"p{k}: {random.choice(['i32', 'i64', 'f64'])}" for k in range(nparams))
            if ret_ty in ("i32", "i64", "f64"):
                body_expr = " + ".join(f"p{k}" for k in range(nparams)) if nparams > 0 else str(random.randint(0, 100))
            elif ret_ty == "bool":
                body_expr = random.choice(["true", "false"])
            else:
                body_expr = f'"{name}"'
            fns.append(f"fn {name}({params}) -> {ret_ty} {{ {body_expr} }}")

        # Main body: mix of let bindings, println, if/else, for
        main_stmts = []
        vars_defined: list[str] = []
        for _ in range(random.randint(2, 10)):
            choice = random.randint(0, 5)
            if choice <= 1:
                vname = f"v{len(vars_defined)}"
                val = str(random.randint(-100, 100))
                main_stmts.append(f"let {vname} = {val};")
                vars_defined.append(vname)
            elif choice == 2 and vars_defined:
                v = random.choice(vars_defined)
                main_stmts.append(f"println({v});")
            elif choice == 3:
                main_stmts.append(f"println({random.randint(0, 1000)});")
            elif choice == 4 and fn_names:
                fn = random.choice(fn_names)
                main_stmts.append(f"println({fn}());")
            elif choice == 5:
                main_stmts.append(f"for _i in 0..{random.randint(0, 5)} {{ println(1); }}")
            else:
                main_stmts.append(f"println({random.randint(0, 100)});")

        body = "\n  ".join(main_stmts)
        src = "\n".join(fns) + f"\nfn main() {{\n  {body}\n}}"
        _report(run_hew(ctx, src, f"welltyped/{i}"))

CATEGORIES: dict[str, tuple[str, object]] = {
    # --- Original categories ---
    "empty":         ("Empty and near-empty inputs",            fuzz_empty),
    "unicode":       ("Unicode edge cases",                     fuzz_unicode),
    "types":         ("Type system edge cases",                 fuzz_types),
    "syntax":        ("Syntax torture",                         fuzz_syntax),
    "actors":        ("Actor edge cases",                       fuzz_actors),
    "interpolation": ("String interpolation edge cases",        fuzz_interpolation),
    "scope":         ("Scope and closure edge cases",           fuzz_scope),
    "deep":          ("Deep nesting stress tests",              fuzz_deep),
    "pathological":  ("Pathological patterns",                  fuzz_pathological),
    "boundary":      ("Numeric boundary values",                fuzz_boundary),
    "strings":       ("String edge cases",                      fuzz_strings),
    "control_flow":  ("Control flow edge cases",                fuzz_control_flow),
    "malformed":     ("Malformed constructs",                   fuzz_malformed),
    "random":        ("Random generated programs",              fuzz_random),
    "mutations":     ("Mutated valid programs",                 fuzz_mutations),
    "garbage":       ("Random printable garbage",               fuzz_garbage),
    "binary":        ("Binary / injected data",                 fuzz_binary),
    # --- Language feature coverage ---
    "traits":        ("Trait declarations and impls",           fuzz_traits),
    "enums":         ("Enum declarations and matching",         fuzz_enums),
    "structs":       ("Struct init, fields, methods",           fuzz_structs),
    "patterns":      ("Pattern matching (guards, or, destruct)",fuzz_patterns),
    "async_await":   ("Async/await and timeouts",               fuzz_async_await),
    "defer":         ("Defer statement ordering",               fuzz_defer),
    "generators":    ("Generator functions and yield",          fuzz_generators),
    "operators":     ("All operators and precedence",           fuzz_operators),
    "imports":       ("Import and module system",               fuzz_imports),
    "wire_types":    ("Wire struct/enum serialization",         fuzz_wire_types),
    "labels":        ("Labeled loops with break/continue",      fuzz_labels),
    "ffi":           ("FFI / extern declarations",              fuzz_ffi),
    "attributes":    ("Attribute annotations",                  fuzz_attributes),
    "visibility":    ("Visibility modifiers",                   fuzz_visibility),
    "tuples":        ("Tuple types and destructuring",          fuzz_tuples),
    "dur_regex":     ("Duration literals and regex patterns",   fuzz_duration_regex),
    "unsafe":        ("Unsafe blocks and raw pointers",         fuzz_unsafe_blocks),
    "concurrency":   ("Scope, select, join, cooperate",         fuzz_scope_concurrency),
    "supervisors":   ("Supervisor declarations",                fuzz_supervisor),
    "mailbox":       ("Actor mailbox configurations",           fuzz_mailbox),
    "lifecycle":     ("Actor lifecycle (init, stop, link)",     fuzz_actor_lifecycle),
    "try_op":        ("Postfix try (?) operator",               fuzz_try_operator),
    "methods":       ("Method chaining patterns",               fuzz_method_chains),
    "collections":   ("Deep collection usage",                  fuzz_collections_deep),
    # --- Compiler testing techniques ---
    "differential":  ("Differential testing (check vs build)",  fuzz_differential),
    "cross_feature": ("Cross-feature interactions",             fuzz_cross_feature),
    "error_cascade": ("Multiple errors in one file",            fuzz_error_cascade),
    "type_infer":    ("Type inference stress tests",            fuzz_type_inference),
    "recursion":     ("Recursive patterns",                     fuzz_recursion),
    "oracle":        ("Semantic oracle (known-good programs)",  fuzz_semantic_oracle),
    "comments":      ("Comment placement edge cases",           fuzz_comment_placement),
    "whitespace":    ("Whitespace sensitivity",                 fuzz_whitespace),
    "well_typed":    ("Random well-typed programs (CSmith-like)", fuzz_random_well_typed),
}


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generative fuzzing of the Hew compiler (no external deps)."
    )
    parser.add_argument("-n", type=int, default=100,
                        help="Number of random/mutant programs per category (default: 100)")
    parser.add_argument("-t", type=int, default=10,
                        help="Timeout in seconds per compile (default: 10)")
    parser.add_argument("--seed", type=int, default=None,
                        help="Random seed for reproducibility")
    parser.add_argument("--hew", type=str, default=None,
                        help="Path to hew binary (default: target/debug/hew)")
    parser.add_argument("--category", type=str, default=None,
                        help="Run only a specific category")
    parser.add_argument("--list-categories", action="store_true",
                        help="List available fuzz categories and exit")
    parser.add_argument("-k", action="store_true",
                        help="Keep workdir with failing inputs")
    args = parser.parse_args()

    if args.list_categories:
        print("Available categories:")
        for name, (desc, _) in CATEGORIES.items():
            print(f"  {name:16s}  {desc}")
        sys.exit(0)

    if args.category and args.category not in CATEGORIES:
        sys.exit(f"Unknown category '{args.category}'. Use --list-categories to see options.")

    root = find_repo_root()

    if args.hew:
        hew = Path(args.hew)
    else:
        hew = root / "target" / "debug" / "hew"
    if not hew.exists():
        sys.exit(f"ERROR: hew binary not found at {hew}\n  Build it with: cargo build -p hew-cli")

    if args.seed is not None:
        random.seed(args.seed)

    workdir = Path(tempfile.mkdtemp(prefix="hew-fuzz-"))

    ctx = FuzzContext(hew=hew, timeout=args.t, keep=args.k, workdir=workdir)

    print("=" * 60)
    print("Hew Compiler Fuzzer (generative)")
    print("=" * 60)
    print(f"  binary:   {hew}")
    print(f"  timeout:  {args.t}s")
    print(f"  seed:     {args.seed or 'random'}")
    print(f"  workdir:  {workdir}")
    print()

    cats = {args.category: CATEGORIES[args.category]} if args.category else CATEGORIES

    for name, (desc, func) in cats.items():
        print(f"--- {desc} ---")
        try:
            import inspect
            sig = inspect.signature(func)
            if "n" in sig.parameters:
                func(ctx, n=args.n)
            else:
                func(ctx)
        except Exception as exc:
            print(f"  FUZZER ERROR in {name}: {exc}")
            traceback.print_exc()

    # Summary
    s = ctx.stats
    print()
    print("=" * 60)
    print("RESULTS")
    print("=" * 60)
    print(f"  Total:    {s.total}")
    print(f"  OK:       {s.ok}")
    print(f"  Error:    {s.error} (expected — invalid inputs)")
    print(f"  Crashes:  {s.crash}")
    print(f"  Hangs:    {s.hang}")
    print(f"  ICEs:     {s.ice}")

    if ctx.issues:
        print()
        print(f"{'=' * 60}")
        print(f"ISSUES FOUND: {len(ctx.issues)}")
        print(f"{'=' * 60}")
        for i, issue in enumerate(ctx.issues):
            print(f"\n--- Issue {i + 1}: [{issue.status}] {issue.label} ---")
            if issue.returncode is not None:
                print(f"  Return code: {issue.returncode}")
            if issue.output:
                print(f"  Output: {issue.output[:500]}")
            if issue.source:
                print(f"  Source: {issue.source}")
        print(f"\nFailing inputs saved to: {workdir}")
        ctx.keep = True
    else:
        print()
        print("No crashes, ICEs, or hangs found.")

    if not ctx.keep:
        shutil.rmtree(workdir)

    if s.crash > 0 or s.ice > 0:
        sys.exit(1)


if __name__ == "__main__":
    main()
