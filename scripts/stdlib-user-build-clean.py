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
) -> CommandResult:
    env = os.environ.copy()
    env["HEW_STD"] = str(stdlib_dir)
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


def verify_broken_stdlib_refuses_user_builds(
    hew_bin: Path, stdlib_dir: Path, modules: list[Module]
) -> None:
    """Prove the sweep's clean verdict is earned, not vacuous.

    Injects a bare variant pattern into a scratch copy of `std/arena.hew` and
    asserts three things: the direct source audit catches it, and both
    user-facing paths refuse rather than build against a stdlib that does not
    type-check, disclosing the defect they refused on.

    Refusing is the point. `retain_user_facing_diagnostics` drops stdlib-owned
    *advisories* because a user cannot act on a compiler-shipped implementation
    site, but an error is never dropped silently: a stdlib that fails to
    type-check cannot be compiled around, and the user needs the span to report
    it. Until v0.6.0 the pattern form of this rule was a deprecation warning, so
    this probe used to assert the opposite - that the user paths succeeded with
    the warning filtered out. Both halves of the rule refuse now.
    """

    arena = next((module for module in modules if module.name == "std.arena"), None)
    if arena is None:
        raise ValueError("broken-stdlib probe requires std.arena")
    with tempfile.TemporaryDirectory(prefix="hew-stdlib-user-boundary-") as tmp:
        scratch_root = Path(tmp)
        scratch_std = scratch_root / "std"
        shutil.copytree(stdlib_dir, scratch_std)
        scratch_arena = scratch_std / arena.source.relative_to(stdlib_dir)
        source = scratch_arena.read_text(encoding="utf-8")
        changed = source.replace(".Some(slot)", "Some(slot)", 1)
        if changed == source:
            raise ValueError("broken-stdlib injection site `.Some(slot)` is missing")
        scratch_arena.write_text(changed, encoding="utf-8")

        result = audit_module(
            hew_bin,
            scratch_std,
            Module(arena.name, scratch_arena, arena.call),
            scratch_root / "user-package",
        )
        source_check = next(
            command for command in result.commands if command.label == "source-check"
        )
        caught = [
            diagnostic
            for diagnostic in source_check.diagnostics
            if diagnostic.get("code") == "E_BARE_VARIANT_PATTERN"
            and from_stdlib(diagnostic, scratch_std)
        ]
        if not caught:
            raise ValueError(
                "scratch bare pattern did not fail the direct stdlib source audit"
            )
        for command in result.commands:
            if command.label == "source-check":
                continue
            if command.status == 0:
                report_command_failure(arena, command)
                raise ValueError(
                    f"{command.label} accepted a stdlib that does not type-check"
                )
            disclosed = [
                diagnostic
                for diagnostic in command.diagnostics
                if diagnostic.get("code") == "E_BARE_VARIANT_PATTERN"
                and diagnostic.get("severity") == "error"
                and from_stdlib(diagnostic, scratch_std)
            ]
            if not disclosed:
                report_command_failure(arena, command)
                raise ValueError(
                    f"{command.label} refused without disclosing the stdlib defect"
                )
        print("PASS: a stdlib that does not type-check refuses every user build")


def main() -> int:
    args = parse_args()
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
        if not selected:
            verify_broken_stdlib_refuses_user_builds(hew_bin, stdlib_dir, all_modules)
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
