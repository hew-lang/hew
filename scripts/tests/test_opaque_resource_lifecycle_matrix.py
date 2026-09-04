#!/usr/bin/env python3
"""Execute source-derived runtime and Wasm opaque-resource lifecycle evidence."""

from __future__ import annotations

import argparse
import copy
import json
import os
import re
import sys
import tempfile
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "scripts/structural-authority-audit.py"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"
MANIFEST = ROOT / "scripts/opaque-resource-lifecycle-evidence.json"
HEW = Path(os.environ.get("HEW_BIN", ROOT / "target/debug/hew"))

# run_bounded caps the child's RLIMIT_AS, which bounds ADDRESS SPACE, not
# resident memory. Wasmtime reserves a fixed 0x1_0400_0000 (4 GiB + 64 MiB
# guard) region per wasm32 linear memory at instantiation, so any bound at or
# below 4096 MiB makes every wasm run fail with "mmap failed to reserve
# 0x104000000 bytes: Cannot allocate memory (os error 12)" — a provisioning
# artefact that reads as a lifecycle rejection. Bound the wasm-executing
# children above that reservation; the runtime cost stays bounded by the
# per-call timeout.
WASM_RUNTIME_MEMORY_MB = 8192
sys.path.insert(0, str(ROOT / "scripts"))
from bounded_subprocess import assert_bounding_contract, run_bounded  # noqa: E402


def fail(message: str) -> None:
    raise AssertionError(message)


def source_key(row: dict[str, object]) -> tuple[str, str]:
    return str(row["source_path"]), str(row["resource"])


def validate_rows(candidates: list[dict], rows: list[dict]) -> dict[str, dict]:
    candidate_by_source = {source_key(row): row for row in candidates}
    if len(candidate_by_source) != len(candidates):
        fail("AST lifecycle facts contain duplicate source identities")
    candidate_by_carrier = {str(row["carrier_key"]): row for row in candidates}
    if len(candidate_by_carrier) != len(candidates):
        fail("AST lifecycle facts contain duplicate derived carrier identities")
    evidence_by_source = {source_key(row): row for row in rows}
    if len(evidence_by_source) != len(rows):
        fail("lifecycle evidence contains duplicate source identities")
    missing = sorted(set(candidate_by_source) - set(evidence_by_source))
    extra = sorted(set(evidence_by_source) - set(candidate_by_source))
    if missing or extra:
        fail(f"source/evidence mismatch: missing={missing}, extra={extra}")

    result = {}
    for identity, candidate in candidate_by_source.items():
        evidence = evidence_by_source[identity]
        if "carrier_key" in evidence:
            fail(
                "carrier keys must be derived from shipped AST facts, not stored in evidence"
            )
        if evidence.get("release_symbol") != candidate.get("release_symbol"):
            fail(f"{candidate['carrier_key']} release authority is stale")
        runtime = evidence.get("runtime", {})
        if runtime.get("valid_handle") is not True:
            fail(
                f"{candidate['carrier_key']} runtime evidence is not a valid-handle proof"
            )
        if not runtime.get("path") or not runtime.get("test"):
            fail(f"{candidate['carrier_key']} runtime evidence is incomplete")
        assert_runtime_anchor(candidate, runtime)
        if runtime.get("execution_profile") not in {"local", "external-network"}:
            fail(f"{candidate['carrier_key']} has an invalid runtime execution profile")
        wasm = evidence.get("wasm", {})
        if wasm.get("profile") != "wasm32-wasi":
            fail(f"{candidate['carrier_key']} has no exact wasm32-wasi evidence")
        if wasm.get("disposition") not in {"accepted", "rejected"}:
            fail(f"{candidate['carrier_key']} has an invalid Wasm disposition")
        proof_kind = wasm.get("proof_kind")
        if wasm["disposition"] == "accepted" and proof_kind not in {
            "public-lifecycle",
            "internal-transient",
        }:
            fail(
                f"{candidate['carrier_key']} accepted Wasm row lacks lifecycle proof kind"
            )
        if wasm["disposition"] == "rejected" and proof_kind != "rejected-boundary":
            fail(
                f"{candidate['carrier_key']} rejected Wasm row lacks boundary proof kind"
            )
        result[str(candidate["carrier_key"])] = evidence
    return result


def expect_counterfactual_failure(candidates: list[dict], rows: list[dict]) -> None:
    try:
        validate_rows(candidates, rows)
    except AssertionError:
        return
    fail("counterfactual unexpectedly passed")


def run_counterfactuals(candidates: list[dict], rows: list[dict]) -> None:
    expect_counterfactual_failure(candidates, rows[:-1])
    extra = copy.deepcopy(rows)
    extra.append({**copy.deepcopy(rows[0]), "source_path": "std/fake/fake.hew"})
    expect_counterfactual_failure(candidates, extra)
    expect_counterfactual_failure(candidates[1:], rows)
    source_added = copy.deepcopy(candidates)
    source_added.append(
        {**copy.deepcopy(candidates[0]), "source_path": "std/new/new.hew"}
    )
    expect_counterfactual_failure(source_added, rows)
    stale = copy.deepcopy(rows)
    stale[0]["runtime"]["test"] = ""
    expect_counterfactual_failure(candidates, stale)
    synthetic = copy.deepcopy(rows)
    synthetic[0]["source_path"] = "tests/synthetic_resource.hew"
    expect_counterfactual_failure(candidates, synthetic)
    false_unsupported = copy.deepcopy(rows)
    false_unsupported[0]["wasm"]["disposition"] = "unsupported"
    expect_counterfactual_failure(candidates, false_unsupported)
    invalid_profile = copy.deepcopy(rows)
    invalid_profile[0]["runtime"]["execution_profile"] = "synthetic"
    expect_counterfactual_failure(candidates, invalid_profile)

    duplicate_carrier = copy.deepcopy(candidates)
    duplicate_carrier[1]["carrier_key"] = duplicate_carrier[0]["carrier_key"]
    expect_counterfactual_failure(duplicate_carrier, rows)

    substituted_runtime = copy.deepcopy(rows)
    substituted_runtime[0]["runtime"] = copy.deepcopy(rows[1]["runtime"])
    expect_counterfactual_failure(candidates, substituted_runtime)

    release_only = (
        f"fn {rows[0]['runtime']['test']}() {{ {candidates[0]['release_symbol']}(0); }}"
    )
    try:
        assert_runtime_semantics(candidates[0], rows[0]["runtime"], release_only)
    except AssertionError:
        pass
    else:
        fail("fabricated-handle release-only runtime anchor unexpectedly passed")

    try:
        assert_wasm_disposition("counterfactual", "rejected", 0, "")
    except AssertionError:
        pass
    else:
        fail("false Wasm rejection unexpectedly passed")

    # The shipped JSON/TOML/YAML rows are a positive control: identical leaf
    # names remain distinct because the joined identity includes source path.
    joined = validate_rows(candidates, rows)
    value_keys = {
        str(row["carrier_key"]) for row in candidates if row["resource"] == "Value"
    }
    assert len(value_keys) >= 2 and value_keys <= set(joined)


def rust_function_body(source: str, name: str) -> str:
    match = re.search(rf"\bfn\s+{re.escape(name)}\s*(?:<[^>{{}}]*>)?\s*\(", source)
    if match is None:
        fail(f"stale runtime test: {name}")
    start = match.start()
    brace = source.find("{", start)
    depth = 0
    for end in range(brace, len(source)):
        if source[end] == "{":
            depth += 1
        elif source[end] == "}":
            depth -= 1
            if depth == 0:
                return source[start : end + 1]
    fail(f"unterminated runtime test: {name}")


def assert_runtime_semantics(
    candidate: dict, anchor: dict[str, object], source: str
) -> None:
    body = rust_function_body(source, str(anchor["test"]))
    release = str(candidate["release_symbol"])
    if release not in body:
        fail(
            f"{candidate['carrier_key']} runtime test does not call exact release {release}"
        )
    reachable = body
    pending = re.findall(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\(", body)
    visited = {str(anchor["test"])}
    while pending and len(visited) < 256:
        function = pending.pop()
        if function in visited:
            continue
        visited.add(function)
        try:
            helper = rust_function_body(source, function)
        except AssertionError:
            continue
        reachable += helper
        pending.extend(re.findall(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\(", helper))
    producers = [
        str(symbol)
        for symbol in candidate.get("producer_symbols", [])
        if str(symbol) in reachable
    ]
    if not producers:
        fail(
            f"{candidate['carrier_key']} runtime test has no reachable exact valid-handle producer"
        )


def assert_runtime_anchor(candidate: dict, anchor: dict[str, object]) -> None:
    path = ROOT / str(anchor["path"])
    if not path.is_file():
        fail(f"stale runtime path: {path.relative_to(ROOT)}")
    assert_runtime_semantics(candidate, anchor, path.read_text())


def cargo_test_command(anchor: dict[str, object]) -> list[str]:
    path = ROOT / str(anchor["path"])
    if not path.is_file():
        fail(f"stale runtime path: {path.relative_to(ROOT)}")
    source = path.read_text()
    if f"fn {anchor['test']}(" not in source:
        fail(f"stale runtime test: {anchor['test']} in {path.relative_to(ROOT)}")
    cargo_root = path.parent
    while cargo_root != ROOT and not (cargo_root / "Cargo.toml").is_file():
        cargo_root = cargo_root.parent
    cargo_toml = cargo_root / "Cargo.toml"
    if not cargo_toml.is_file():
        fail(f"runtime anchor has no Cargo package: {path.relative_to(ROOT)}")
    package = tomllib.loads(cargo_toml.read_text())["package"]["name"]
    command = ["cargo", "test", "-p", package]
    try:
        relative = path.relative_to(cargo_root)
    except ValueError as error:
        raise AssertionError(path) from error
    if len(relative.parts) >= 2 and relative.parts[0] == "tests":
        command += ["--test", path.stem]
    else:
        command += ["--lib"]
    command += [str(anchor["test"]), "--", "--nocapture"]
    return command


def run_runtime_evidence(evidence: dict[str, dict], profile: str) -> None:
    completed = {}
    failures = []
    executed = 0
    deferred = 0
    for carrier, row in evidence.items():
        command = cargo_test_command(row["runtime"])
        row_profile = row["runtime"]["execution_profile"]
        if profile != "composition" and row_profile != profile:
            deferred += 1
            continue
        executed += 1
        key = tuple(command)
        if key not in completed:
            completed[key] = run_bounded(
                command,
                cwd=ROOT,
                timeout_seconds=600,
                memory_mb=16384,
            )
        result = completed[key]
        output = result.stdout + result.stderr
        if (
            result.returncode != 0
            or "test result: ok" not in output
            or "0 passed" in output
        ):
            failures.append(f"{carrier}: {' '.join(command)}\n{output}")
    if failures:
        fail("runtime lifecycle evidence failures:\n" + "\n".join(failures))
    if profile == "composition":
        local = sum(
            row["runtime"]["execution_profile"] == "local" for row in evidence.values()
        )
        external = len(evidence) - local
        if executed != len(evidence) or deferred:
            fail("composition profile did not execute every lifecycle row")
        print(
            f"runtime lifecycle composition: {executed}/{len(evidence)} rows; "
            f"local={local}, external-network={external}"
        )
    else:
        print(
            f"runtime lifecycle {profile} profile: {executed} rows executed, "
            f"{deferred} rows deferred (profile-only; not composition success)"
        )


def is_wasm_platform_rejection(output: str) -> bool:
    diagnostic = output.lower()
    return (
        "not supported on wasm32" in diagnostic
        or "not available on target" in diagnostic
        or "require the native" in diagnostic
    )


def diagnostic_excerpt(output: str, limit: int = 4000) -> str:
    cleaned = "".join(
        character
        if character in "\n\t" or character.isprintable()
        else "\N{REPLACEMENT CHARACTER}"
        for character in output
    )
    if len(cleaned) <= limit:
        return cleaned
    half = limit // 2
    omitted = len(cleaned) - limit
    return (
        cleaned[:half]
        + f"\n... <{omitted} diagnostic characters omitted> ...\n"
        + cleaned[-half:]
    )


def assert_wasm_disposition(
    carrier: str, expected: str, returncode: int, output: str, witness: str = ""
) -> None:
    platform_rejection = is_wasm_platform_rejection(output)
    actual = (
        "accepted"
        if returncode == 0
        and (not witness or witness in output)
        and not platform_rejection
        else "rejected"
    )
    if actual != expected:
        fail(
            f"{carrier}: expected {expected}, got {actual}\n"
            f"{diagnostic_excerpt(output)}"
        )
    if actual == "rejected":
        if not platform_rejection:
            fail(
                f"{carrier}: rejection was not a platform disposition\n"
                f"{diagnostic_excerpt(output)}"
            )


def wasm_public_programs(
    carrier: str, witness: str
) -> tuple[str, tuple[tuple[str, str], ...]]:
    """Return executable producer programs, separate from structural codegen.

    The structural cases prove generic consuming-parameter elaboration.  These
    programs prove the distinct executable claim: a public producer reaches
    the target boundary, and accepted families actually run rather than merely
    printing a witness from an otherwise dead `main`.
    """
    programs: dict[str, tuple[str, tuple[tuple[str, str], ...]]] = {
        "std.channel.ChannelPair": (
            "internal-transient",
            (
                (
                    "internal-wrapper",
                    "import std.channel;\n"
                    f'fn main() {{ let result: Result<(channel.Sender<i64>, channel.Receiver<i64>), string> = channel.new(1); match result {{ .Ok((sender, receiver)) => println("{witness}"), .Err(error) => panic(error), }} }}\n',
                ),
            ),
        ),
        "std.encoding.json.Value": (
            "public-lifecycle",
            (
                (
                    "public-implicit",
                    "import std.encoding.json;\n"
                    f'fn main() {{ let value = json.null(); println("{witness}"); }}\n',
                ),
                (
                    "public-explicit",
                    "import std.encoding.json;\n"
                    f'fn main() {{ let value = json.null(); println("{witness}"); value.close(); }}\n',
                ),
            ),
        ),
        "std.encoding.toml.Value": (
            "public-lifecycle",
            (
                (
                    "public-implicit",
                    "import std.encoding.toml;\n"
                    f'fn main() {{ let value = toml.table(); println("{witness}"); }}\n',
                ),
                (
                    "public-explicit",
                    "import std.encoding.toml;\n"
                    f'fn main() {{ let value = toml.table(); println("{witness}"); value.close(); }}\n',
                ),
            ),
        ),
        "std.encoding.yaml.Value": (
            "public-lifecycle",
            (
                (
                    "public-implicit",
                    "import std.encoding.yaml;\n"
                    f'fn main() {{ let value = yaml.object(); println("{witness}"); }}\n',
                ),
                (
                    "public-explicit",
                    "import std.encoding.yaml;\n"
                    f'fn main() {{ let value = yaml.object(); println("{witness}"); value.close(); }}\n',
                ),
            ),
        ),
        "std.fs.FileReadStream": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.fs;\n"
                    f'fn main() {{ let result = fs.read("/dev/null"); match result {{ .Ok(_) => println("{witness}"), .Err(error) => panic(f"{{error}}"), }} }}\n',
                ),
            ),
        ),
        "std.net.Connection": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net;\n"
                    f'fn main() {{ let connection = net.connect("127.0.0.1:1"); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.net.Listener": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net;\n"
                    f'fn main() {{ let listener = net.listen("127.0.0.1:0"); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.net.http.Server": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net.http;\n"
                    f'fn main() {{ let server = http.listen("127.0.0.1:0"); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.net.quic.QUICConnection": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net.quic;\n"
                    f'fn main() {{ let endpoint = quic.new_client(); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.net.quic.QUICEndpoint": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net.quic;\n"
                    f'fn main() {{ let endpoint = quic.new_client(); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.net.quic.QUICStream": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net.quic;\n"
                    f'fn main() {{ let endpoint = quic.new_client(); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.net.smtp.Conn": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net.smtp;\n"
                    f'fn main() {{ let conn = smtp.connect("127.0.0.1", 1, "", ""); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.net.tls.TlsStream": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net.tls;\n"
                    f'fn main() {{ let stream = tls.connect("127.0.0.1", 1); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.net.websocket.Conn": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net.websocket;\n"
                    f'fn main() {{ let conn = websocket.connect("ws://127.0.0.1:1/"); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.net.websocket.Server": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.net.websocket;\n"
                    f'fn main() {{ let server = websocket.listen("127.0.0.1:0"); println("{witness}"); }}\n',
                ),
            ),
        ),
        "std.process.ProcessResultHandle": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.process;\n"
                    f'fn main() {{ let result = process.run("true"); match result {{ .Ok(_) => println("{witness}"), .Err(error) => panic(f"{{error}}"), }} }}\n',
                ),
            ),
        ),
        "std.stream.StreamPair": (
            "rejected-boundary",
            (
                (
                    "boundary",
                    "import std.stream;\n"
                    f'fn main() {{ let (sink, source) = stream.pipe(1); println("{witness}"); }}\n',
                ),
            ),
        ),
    }
    try:
        return programs[carrier]
    except KeyError as error:
        raise AssertionError(
            f"missing public Wasm producer program for {carrier}"
        ) from error


def assert_wasm_program_has_producer(carrier: str, program: str) -> None:
    main = rust_function_body(program, "main")
    if not re.search(
        r"\blet\s+(?:[A-Za-z_][A-Za-z0-9_]*|\([^)]*\))(?:\s*:\s*[^=;{}]+)?\s*=\s*[^;{}]+\(",
        main,
    ):
        fail(f"{carrier}: Wasm main has no executed producer call")


def generic_lifecycle_sources(case: dict) -> tuple[tuple[str, str], tuple[str, str]]:
    """Keep source-derived implicit/explicit release codegen independent.

    These are compiler-owned generic consuming-parameter cases, not public
    producer programs. In particular, ChannelPair is an internal wrapper
    resource: channel.new frees it before exposing Sender/Receiver.
    """
    scope = str(case.get("scope_exit_source", ""))
    explicit = str(case.get("explicit_close_source", ""))
    if "fn scope_exit_case" not in scope:
        fail(f"{case['carrier_key']}: missing generated scope-exit lifecycle case")
    if "fn explicit_close_case" not in explicit:
        fail(f"{case['carrier_key']}: missing generated explicit-close lifecycle case")
    # The compiler-owned cases do not have a public constructor. Give the
    # freestanding Wasm linker an entry point without calling either case:
    # their generated LLVM remains present for the exact-release assertion.
    entrypoint = "\nfn main() { }\n"
    return (
        ("scope-exit", scope + entrypoint),
        ("explicit-close", explicit + entrypoint),
    )


def llvm_function_body(llvm: str, function: str) -> str:
    """Extract one emitted LLVM function, never its imported stdlib neighbours."""
    header = re.search(
        rf'^define\b[^@]*@(?:"[^"]*{re.escape(function)}[^"]*"|[^\s(]*{re.escape(function)}[^\s(]*)\([^)]*\)\s*\{{',
        llvm,
        re.MULTILINE,
    )
    if header is None:
        fail(f"generated LLVM omitted lifecycle function `{function}`")
    end = llvm.find("\n}", header.end())
    if end < 0:
        fail(f"generated LLVM has unterminated lifecycle function `{function}`")
    return llvm[header.start() : end + 2]


def llvm_calls_symbol(body: str, symbol: str) -> bool:
    return (
        re.search(
            rf'\bcall\b[^@]*@(?:"{re.escape(symbol)}"|{re.escape(symbol)})\(', body
        )
        is not None
    )


def assert_exact_llvm_release_chain(
    carrier: str, llvm: str, function: str, close: str, release: str
) -> None:
    body = llvm_function_body(llvm, function)
    if not llvm_calls_symbol(body, close):
        fail(f"{carrier}: {function} LLVM omits exact close dispatch {close}")
    close_body = llvm_function_body(llvm, close)
    if not llvm_calls_symbol(close_body, release):
        fail(f"{carrier}: {close} LLVM omits exact release {release}")


def run_wasm_evidence(cases: list[dict], evidence: dict[str, dict], temp: Path) -> None:
    accepted_inventory = set()
    failures = []

    for index, case in enumerate(cases):
        carrier = str(case["carrier_key"])
        expected = evidence[carrier]["wasm"]["disposition"]
        witness = f"WASM-LIFECYCLE:{carrier}"
        try:
            proof_kind, public_programs = wasm_public_programs(carrier, witness)
            if proof_kind != evidence[carrier]["wasm"]["proof_kind"]:
                fail(f"{carrier}: public Wasm program proof kind drifted from evidence")
        except AssertionError as error:
            failures.append(str(error))
            continue

        for form, program in public_programs:
            test_id = f"{carrier}::{form}"
            if expected == "accepted":
                accepted_inventory.add(test_id)
            try:
                assert_wasm_program_has_producer(carrier, program)
                source = temp / f"{index:02}-public-{form}.hew"
                source.write_text(program)
                wasi = run_bounded(
                    [
                        str(HEW),
                        "run",
                        "--target",
                        "wasm32-wasi",
                        "--timeout",
                        "20s",
                        str(source),
                    ],
                    cwd=ROOT,
                    timeout_seconds=60,
                    memory_mb=WASM_RUNTIME_MEMORY_MB,
                )
                output = wasi.stdout + wasi.stderr
                assert_wasm_disposition(
                    test_id,
                    expected,
                    wasi.returncode,
                    output,
                    witness,
                )
            except AssertionError as error:
                failures.append(str(error))

        # A rejected public boundary is the target contract for this family.
        # Its imported implementation may itself be unavailable, so only
        # accepted families can establish generic LLVM lowering.
        if expected == "rejected":
            continue

        # These exact source-derived cases establish compiler implicit and
        # explicit release lowering independently of the public execution
        # result; collect their failures into the same final verdict.
        try:
            generic_sources = generic_lifecycle_sources(case)
        except AssertionError as error:
            failures.append(str(error))
            continue
        for form, program in generic_sources:
            try:
                source = temp / f"{index:02}-generic-{form}.hew"
                source.write_text(program)
                emit_dir = temp / f"{index:02}-{form}-emit"
                emit_dir.mkdir()
                codegen = run_bounded(
                    [
                        str(HEW),
                        "compile",
                        "--emit-llvm",
                        "--target",
                        "wasm32-unknown-unknown",
                        "--emit-dir",
                        str(emit_dir),
                        str(source),
                    ],
                    cwd=ROOT,
                    timeout_seconds=120,
                    memory_mb=4096,
                )
                if codegen.returncode != 0:
                    fail(
                        f"{carrier}: {form} Wasm codegen failed\n"
                        + codegen.stdout
                        + codegen.stderr
                    )
                llvm = next(iter(emit_dir.glob("*.ll")), None)
                wasm = next(iter(emit_dir.glob("*.wasm")), None)
                if llvm is None or wasm is None:
                    fail(f"{carrier}: {form} codegen omitted LLVM/Wasm artifacts")
                release = str(evidence[carrier]["release_symbol"])
                function = (
                    "scope_exit_case" if form == "scope-exit" else "explicit_close_case"
                )
                assert_exact_llvm_release_chain(
                    carrier,
                    llvm.read_text(),
                    function,
                    str(case["close_symbol"]),
                    release,
                )
                if not wasm.read_bytes().startswith(b"\0asm"):
                    fail(f"{carrier}: {form} artifact lacks Wasm magic")
                validated = run_bounded(
                    [
                        "wasmtime",
                        "compile",
                        str(wasm),
                        "-o",
                        str(emit_dir / "validated.cwasm"),
                    ],
                    cwd=ROOT,
                    timeout_seconds=60,
                    memory_mb=WASM_RUNTIME_MEMORY_MB,
                )
                if validated.returncode != 0:
                    fail(
                        f"{carrier}: {form} artifact failed Wasmtime validation\n"
                        + validated.stdout
                        + validated.stderr
                    )
            except AssertionError as error:
                failures.append(str(error))

    if failures:
        fail("Wasm lifecycle evidence failures:\n" + "\n".join(failures))
    print(
        f"Wasm lifecycle evidence: {len(accepted_inventory)} accepted-family "
        "programs passed"
    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--runtime-profile",
        choices=("local", "external-network", "composition"),
        default="local",
    )
    args = parser.parse_args()
    if not AST_GREP.is_file() or not HEW.is_file():
        raise SystemExit("bootstrap ast-grep and build `hew` before lifecycle matrix")

    with tempfile.TemporaryDirectory() as directory:
        temp = Path(directory)
        facts_path = temp / "facts.json"
        assert_bounding_contract(ROOT)
        audit = run_bounded(
            [
                "python3",
                str(AUDIT),
                "--ast-grep",
                str(AST_GREP),
                "--opaque-resource-facts",
                str(facts_path),
                "--opaque-resource-facts-only",
            ],
            cwd=ROOT,
            timeout_seconds=120,
            memory_mb=4096,
        )
        assert audit.returncode == 0, audit.stderr
        facts = json.loads(facts_path.read_text())
        manifest = json.loads(MANIFEST.read_text())
        assert manifest["schema_version"] == 2
        evidence = validate_rows(facts["candidates"], manifest["resources"])
        run_counterfactuals(facts["candidates"], manifest["resources"])
        try:
            assert_wasm_program_has_producer(
                "counterfactual",
                'fn main() { println("WASM-LIFECYCLE:counterfactual"); }',
            )
        except AssertionError:
            pass
        else:
            fail("print-only Wasm main unexpectedly passed lifecycle evidence")
        missing_scope_exit = copy.deepcopy(facts["compiler_e2e_cases"][0])
        missing_scope_exit["scope_exit_source"] = ""
        try:
            generic_lifecycle_sources(missing_scope_exit)
        except AssertionError:
            pass
        else:
            fail(
                "missing generated scope-exit case unexpectedly passed lifecycle evidence"
            )
        release = str(
            evidence[str(facts["compiler_e2e_cases"][0]["carrier_key"])][
                "release_symbol"
            ]
        )
        missing_close_dispatch = (
            "define internal i8 @scope_exit_case(ptr %0) {\n"
            "entry:\n"
            "  ret i8 0\n"
            "}\n"
            f'declare i8 @"{facts["compiler_e2e_cases"][0]["close_symbol"]}"(ptr)\n'
            f"declare void @{release}(ptr)\n"
        )
        try:
            assert_exact_llvm_release_chain(
                str(facts["compiler_e2e_cases"][0]["carrier_key"]),
                missing_close_dispatch,
                "scope_exit_case",
                str(facts["compiler_e2e_cases"][0]["close_symbol"]),
                release,
            )
        except AssertionError:
            pass
        else:
            fail(
                "scope-exit LLVM without its close/release chain unexpectedly passed lifecycle evidence"
            )
        missing_close_release = (
            "define internal i8 @scope_exit_case(ptr %0) {\n"
            "entry:\n"
            f'  call i8 @"{facts["compiler_e2e_cases"][0]["close_symbol"]}"(ptr %0)\n'
            "  ret i8 0\n"
            "}\n"
            f'define internal i8 @"{facts["compiler_e2e_cases"][0]["close_symbol"]}"(ptr %0) {{\n'
            "entry:\n"
            "  ret i8 0\n"
            "}\n"
            f"declare void @{release}(ptr)\n"
        )
        try:
            assert_exact_llvm_release_chain(
                str(facts["compiler_e2e_cases"][0]["carrier_key"]),
                missing_close_release,
                "scope_exit_case",
                str(facts["compiler_e2e_cases"][0]["close_symbol"]),
                release,
            )
        except AssertionError:
            pass
        else:
            fail(
                "close wrapper without its native release unexpectedly passed lifecycle evidence"
            )
        run_wasm_evidence(facts["compiler_e2e_cases"], evidence, temp)
        run_runtime_evidence(evidence, args.runtime_profile)


if __name__ == "__main__":
    main()
