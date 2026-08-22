#!/usr/bin/env python3
"""Reject every diagnostic whose primary span belongs to the Hew stdlib."""

from __future__ import annotations

import argparse
import collections
import json
import os
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_STDLIB = REPO_ROOT / "std"
DEFAULT_CALLS = REPO_ROOT / "scripts" / "stdlib-user-build-calls.tsv"
CANONICAL_CALLS = """# module<TAB>one statement that lowers a public function body
std.arena\tlet _store: arena.Arena<i64> = arena.new();
std.bench\tbench.suite("gate");
std.builtins\tabs(1);
std.channel\tchannel.new(1);
std.crypto.crypto\tcrypto.sha256("".to_bytes());
std.crypto.encrypt\tencrypt.last_open_error();
std.crypto.jwt\tjwt.last_error();
std.crypto.password\tpassword.hash("");
std.crypto.sign\tsign.keypair();
std.deque\tdeque.new();
std.encoding.base64\tbase64.encode("".to_bytes());
std.encoding.binary\tbinary.put_u16_le(0);
std.encoding.compress\tcompress.gzip_compress("".to_bytes());
std.encoding.csv\tcsv.try_parse("");
std.encoding.hex\thex.encode("".to_bytes());
std.encoding.json\tjson.parse("null");
std.encoding.markdown\tmarkdown.to_html("");
std.encoding.msgpack\tmsgpack.from_json("null");
std.encoding.protobuf\tprotobuf.new();
std.encoding.toml\ttoml.parse("");
std.encoding.xml\txml.parse("<root />");
std.encoding.yaml\tyaml.parse("null");
std.fmt\tfmt.to_hex(0);
std.fs\tfs.io_error_from_errno(0);
std.io\tio.read_line();
std.io.scanner\tscanner.from_string("");
std.iter\tlet values: Vec<i64> = [1]; iter.count(values.into_iter());
std.link_monitor\tlink_monitor.set_partition_policy(link_monitor.PartitionPolicy.FailFast);
std.math\tmath.pi();
std.mem\tmem.alloc(1, 1);
std.metrics\tmetrics.counter("gate");
std.misc.log\tlog.new_logger(0, 0);
std.misc.uuid\tuuid.v4();
std.net\tnet.net_error_from_errno(0);
std.net.dns\tdns.resolve("localhost");
std.net.http\thttp.listen_error();
std.net.http.http_async_client\thttp_async_client.build_get("localhost", "/");
std.net.http.http_async_server\thttp_async_server.request_complete("");
std.net.http.http_client\thttp_client.last_error();
std.net.ipnet\tipnet.is_valid("127.0.0.1");
std.net.mime\tmime.from_path("file.txt");
std.net.quic\tquic.new_client();
std.net.smtp\tsmtp.last_error();
std.net.tls\ttls.connect("localhost", 443);
std.net.url\turl.parse("https://example.com");
std.net.websocket\twebsocket.last_error();
std.observe\tobserve.read("gate");
std.option\toption.is_some_int(Some(1));
std.os\tos.args_count();
std.path\tpath.combine("a", "b");
std.pipeline\tpipeline.from(1);
std.process\tprocess.try_run("true");
std.random\trandom.seed(1);
std.result\tresult.is_ok_int(Ok(1));
std.semaphore\tsemaphore.new(1);
std.sort\tsort.sort_ints([1]);
std.stream\tstream.pipe(1);
std.string\tstring.from_int(1);
std.testing\ttesting.assert_eq(1, 1);
std.text.regex\tregex.new("a");
std.text.semver\tsemver.try_parse("1.2.3");
std.text.template\ttemplate.try_parse("");
std.text.unicode\tunicode.is_valid_rune(65);
std.time.cron\tcron.parse("* * * * *");
std.time.datetime\tdatetime.now_ms();
std.vec\tvec.first_i64([1]);
"""


@dataclass(frozen=True)
class Module:
    name: str
    source: Path
    call: str | None


@dataclass
class CommandResult:
    label: str
    status: int
    stdout: str
    stderr: str
    diagnostics: list[dict[str, object]]


@dataclass
class ModuleResult:
    module: Module
    commands: list[CommandResult]
    stdlib_diagnostics: list[dict[str, object]]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="build and check every stdlib module from a temporary user package"
    )
    parser.add_argument(
        "--hew-bin",
        type=Path,
        default=Path(os.environ.get("HEW_BIN", REPO_ROOT / "target" / "debug" / "hew")),
    )
    parser.add_argument("--stdlib-dir", type=Path, default=DEFAULT_STDLIB)
    parser.add_argument("--calls", type=Path, default=DEFAULT_CALLS)
    parser.add_argument("--module", action="append", default=[])
    parser.add_argument("--skip-counterfactual", action="store_true")
    parser.add_argument(
        "--write-calls",
        action="store_true",
        help="regenerate the committed public-function call baseline",
    )
    return parser.parse_args()


def dotted_module(stdlib_dir: Path, source: Path) -> str:
    parts = list(source.relative_to(stdlib_dir).with_suffix("").parts)
    if len(parts) >= 2 and parts[-1] == parts[-2]:
        parts.pop()
    return ".".join(("std", *parts))


def load_calls(path: Path) -> dict[str, str]:
    calls: dict[str, str] = {}
    for line_number, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not raw.strip() or raw.lstrip().startswith("#"):
            continue
        try:
            module, call = raw.split("\t", 1)
        except ValueError as error:
            raise ValueError(
                f"{path}:{line_number}: expected module<TAB>statement"
            ) from error
        if module in calls:
            raise ValueError(f"{path}:{line_number}: duplicate module {module}")
        calls[module] = call.strip()
    return calls


def discover_modules(stdlib_dir: Path, calls_path: Path) -> list[Module]:
    calls = load_calls(calls_path)
    sources = sorted(
        source for source in stdlib_dir.rglob("*.hew") if "target" not in source.parts
    )
    if not sources:
        raise ValueError(f"no .hew modules found under {stdlib_dir}")
    names = {dotted_module(stdlib_dir, source) for source in sources}
    unknown = sorted(set(calls) - names)
    if unknown:
        raise ValueError(f"call table names unknown modules: {', '.join(unknown)}")

    modules: list[Module] = []
    missing: list[str] = []
    for source in sources:
        name = dotted_module(stdlib_dir, source)
        call = calls.get(name)
        text = source.read_text(encoding="utf-8")
        if (
            any(line.startswith("pub fn ") for line in text.splitlines())
            and call is None
        ):
            missing.append(name)
        modules.append(Module(name, source, call))
    if missing:
        raise ValueError("public function modules need calls: " + ", ".join(missing))
    return modules


def write_package(package_dir: Path, module: Module) -> None:
    package_dir.mkdir(parents=True, exist_ok=True)
    (package_dir / "hew.toml").write_text(
        '[package]\nname = "stdlib_user_gate"\nedition = "2026"\nversion = "0.1.0"\n',
        encoding="utf-8",
    )
    body = ""
    if module.name not in {"std.builtins", "std.prelude"}:
        body = f"import {module.name};\n\n"
    body += "fn main() {\n"
    if module.call is not None:
        body += f"    {module.call}\n"
    (package_dir / "main.hew").write_text(body + "}\n", encoding="utf-8")


def parse_diagnostics(stdout: str) -> list[dict[str, object]]:
    if not stdout.strip():
        return []
    try:
        value = json.loads(stdout)
    except json.JSONDecodeError:
        return []
    return (
        [item for item in value if isinstance(item, dict)]
        if isinstance(value, list)
        else []
    )


def run_command(
    label: str,
    argv: list[str],
    *,
    cwd: Path,
    stdlib_dir: Path,
    show_imported_stdlib_diagnostics: bool = False,
) -> CommandResult:
    env = os.environ.copy()
    env["HEW_STD"] = str(stdlib_dir)
    if show_imported_stdlib_diagnostics:
        env["HEW_STDLIB_SOURCE_GATE"] = "1"
    completed = subprocess.run(
        argv,
        cwd=cwd,
        env=env,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    return CommandResult(
        label,
        completed.returncode,
        completed.stdout,
        completed.stderr,
        parse_diagnostics(completed.stdout),
    )


def path_is_under(path: Path, directory: Path) -> bool:
    try:
        path.resolve().relative_to(directory.resolve())
    except (OSError, ValueError):
        return False
    return True


def from_stdlib(diagnostic: dict[str, object], stdlib_dir: Path) -> bool:
    filename = diagnostic.get("file")
    if not isinstance(filename, str) or not filename or filename == "<unknown>":
        return False
    path = Path(filename)
    if path.is_absolute():
        return path_is_under(path, stdlib_dir)
    normalised = filename.replace("\\", "/")
    return normalised.startswith("std/") or "/std/" in f"/{normalised}"


def audit_module(
    hew_bin: Path,
    stdlib_dir: Path,
    module: Module,
    package_dir: Path,
    *,
    expose_user_build_stdlib: bool = True,
) -> ModuleResult:
    write_package(package_dir, module)
    commands = [
        run_command(
            "source-check",
            [str(hew_bin), "check", str(module.source), "--format=json"],
            cwd=REPO_ROOT,
            stdlib_dir=stdlib_dir,
        ),
        run_command(
            "user-check",
            [str(hew_bin), "check", str(package_dir), "--format=json"],
            cwd=package_dir,
            stdlib_dir=stdlib_dir,
        ),
        run_command(
            "user-build",
            [str(hew_bin), "build", str(package_dir), "--emit-obj", "--format=json"],
            cwd=package_dir,
            stdlib_dir=stdlib_dir,
            show_imported_stdlib_diagnostics=expose_user_build_stdlib,
        ),
    ]
    diagnostics = [
        diagnostic
        for command in commands
        for diagnostic in command.diagnostics
        if from_stdlib(diagnostic, stdlib_dir)
    ]
    return ModuleResult(module, commands, diagnostics)


def command_failed(command: CommandResult) -> bool:
    if command.status != 0:
        return True
    stripped = command.stdout.strip()
    return bool(stripped and not command.diagnostics and stripped != "[]")


def report_command_failure(module: Module, command: CommandResult) -> None:
    print(f"  {module.name}: {command.label} exited {command.status}", file=sys.stderr)
    for stream, output in (("stdout", command.stdout), ("stderr", command.stderr)):
        for line in output.strip().splitlines()[:20]:
            print(f"    {stream}: {line}", file=sys.stderr)


def run_bare_pattern_counterfactual(hew_bin: Path, modules: list[Module]) -> None:
    arena = next((module for module in modules if module.name == "std.arena"), None)
    if arena is None:
        raise ValueError("counterfactual requires std.arena")
    with tempfile.TemporaryDirectory(prefix="hew-stdlib-user-counterfactual-") as tmp:
        scratch_root = Path(tmp)
        scratch_std = scratch_root / "std"
        shutil.copytree(DEFAULT_STDLIB, scratch_std)
        scratch_arena = scratch_std / arena.source.relative_to(DEFAULT_STDLIB)
        source = scratch_arena.read_text(encoding="utf-8")
        changed = source.replace(".Some(slot)", "Some(slot)", 1)
        if changed == source:
            raise ValueError("counterfactual injection site `.Some(slot)` is missing")
        scratch_arena.write_text(changed, encoding="utf-8")
        result = audit_module(
            hew_bin,
            scratch_std,
            Module(arena.name, scratch_arena, arena.call),
            scratch_root / "user-package",
        )
        bare = [
            diagnostic
            for diagnostic in result.stdlib_diagnostics
            if diagnostic.get("code") == "E_BARE_VARIANT_PATTERN"
        ]
        for diagnostic in bare:
            print(
                "CF-[stdlib-user-build-clean] "
                f"{diagnostic.get('code')}: {diagnostic.get('file')}"
            )
        if not bare:
            raise ValueError(
                "scratch bare pattern did not fail the stdlib source audit"
            )
        for command in result.commands:
            if command.label == "source-check":
                continue
            leaked = [
                item for item in command.diagnostics if from_stdlib(item, scratch_std)
            ]
            if leaked:
                codes = ", ".join(str(item.get("code")) for item in leaked)
                raise ValueError(
                    f"{command.label} printed scratch stdlib diagnostics into user output: {codes}"
                )
        print(
            "PASS: scratch bare pattern fails the source audit and stays out of user output"
        )


def run_leak_advisory_counterfactual(hew_bin: Path, modules: list[Module]) -> None:
    vec = next((module for module in modules if module.name == "std.vec"), None)
    if vec is None:
        raise ValueError("leak counterfactual requires std.vec")
    with tempfile.TemporaryDirectory(prefix="hew-stdlib-leak-counterfactual-") as tmp:
        scratch_root = Path(tmp)
        scratch_std = scratch_root / "std"
        shutil.copytree(DEFAULT_STDLIB, scratch_std)
        scratch_vec = scratch_std / vec.source.relative_to(DEFAULT_STDLIB)
        leak_probe = """

pub type __LeakProbeItem {
    name: string;
    n: i64;
}

pub fn __leak_probe_uncalled(xs: Vec<__LeakProbeItem>) -> i64 {
    let it = xs.iter();
    var total = 0;
    for _ in it {
        total = total + 1;
    }
    total
}
"""
        with scratch_vec.open("a", encoding="utf-8") as source:
            source.write(leak_probe)
        package_dir = scratch_root / "user-package"
        result = audit_module(
            hew_bin,
            scratch_std,
            Module(vec.name, scratch_vec, vec.call),
            package_dir,
            expose_user_build_stdlib=False,
        )
        for command in result.commands:
            if command.label == "source-check":
                continue
            leaked = [
                diagnostic
                for diagnostic in command.diagnostics
                if diagnostic.get("code") == "ObligationUnderReleased"
                or "__leak_probe_uncalled" in str(diagnostic.get("message"))
            ]
            if leaked:
                report_command_failure(vec, command)
                raise ValueError(
                    f"{command.label} printed the scratch stdlib leak into JSON user output"
                )

        text_build = run_command(
            "user-build-text",
            [str(hew_bin), "build", str(package_dir), "--emit-obj"],
            cwd=package_dir,
            stdlib_dir=scratch_std,
        )
        if text_build.status != 0:
            report_command_failure(vec, text_build)
            raise ValueError("text user build failed during the leak counterfactual")
        text_output = text_build.stdout + text_build.stderr
        if (
            "ObligationUnderReleased" in text_output
            or "__leak_probe_uncalled" in text_output
        ):
            raise ValueError("text user build printed the scratch stdlib leak advisory")

        source_build = run_command(
            "source-audit",
            [
                str(hew_bin),
                "build",
                str(package_dir),
                "--emit-obj",
                "--format=json",
            ],
            cwd=package_dir,
            stdlib_dir=scratch_std,
            show_imported_stdlib_diagnostics=True,
        )
        caught = [
            diagnostic
            for diagnostic in source_build.diagnostics
            if diagnostic.get("code") == "ObligationUnderReleased"
            and "__leak_probe_uncalled" in str(diagnostic.get("message"))
            and from_stdlib(diagnostic, scratch_std)
        ]
        for diagnostic in caught:
            print(
                "CF-[stdlib-user-build-clean] "
                f"{diagnostic.get('code')}: {diagnostic.get('file')}"
            )
        if not caught:
            report_command_failure(vec, source_build)
            raise ValueError(
                "scratch obligation under-release did not fail the stdlib source audit"
            )
        print(
            "PASS: scratch obligation under-release fails the source audit and "
            "stays out of text and JSON user output"
        )


def run_counterfactual(hew_bin: Path, modules: list[Module]) -> None:
    run_bare_pattern_counterfactual(hew_bin, modules)
    run_leak_advisory_counterfactual(hew_bin, modules)


def main() -> int:
    args = parse_args()
    if args.write_calls:
        args.calls.write_text(CANONICAL_CALLS, encoding="utf-8")
        return 0
    hew_bin = args.hew_bin.resolve()
    stdlib_dir = args.stdlib_dir.resolve()
    calls_path = args.calls.resolve()
    if not hew_bin.is_file():
        print(f"error: hew binary not found: {hew_bin}", file=sys.stderr)
        return 1
    try:
        all_modules = discover_modules(stdlib_dir, calls_path)
        selected = set(args.module)
        unknown = sorted(selected - {module.name for module in all_modules})
        if unknown:
            raise ValueError("unknown selected modules: " + ", ".join(unknown))
        modules = [
            module for module in all_modules if not selected or module.name in selected
        ]
        if not args.skip_counterfactual:
            run_counterfactual(hew_bin, discover_modules(DEFAULT_STDLIB, calls_path))
    except (OSError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1

    print("==> Stdlib user-build diagnostic gate")
    print(f"Modules: {len(modules)}")
    buckets: collections.Counter[str] = collections.Counter()
    failed = False
    with tempfile.TemporaryDirectory(prefix="hew-stdlib-user-build-") as tmp:
        package_dir = Path(tmp) / "package"
        for index, module in enumerate(modules, 1):
            result = audit_module(hew_bin, stdlib_dir, module, package_dir)
            for command in result.commands:
                if command_failed(command):
                    failed = True
                    report_command_failure(module, command)
            for diagnostic in result.stdlib_diagnostics:
                code = str(diagnostic.get("code") or "<uncoded>")
                buckets[code] += 1
                failed = True
                print(
                    f"  {module.name}: {code}: {diagnostic.get('file')}: "
                    f"{diagnostic.get('message')}",
                    file=sys.stderr,
                )
            print(f"  [{index:02d}/{len(modules):02d}] {module.name}")

    print("\nDiagnostic buckets (stdlib primary spans):")
    if buckets:
        for code, count in sorted(buckets.items()):
            print(f"  {code}: {count}")
    else:
        print("  (none): 0")
    if failed:
        print("\nstdlib user-build diagnostic gate: FAILED", file=sys.stderr)
        return 1
    print("\nstdlib user-build diagnostic gate: PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
