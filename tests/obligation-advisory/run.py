#!/usr/bin/env python3
"""Gate direct-call ownership diagnostics against an exact per-file baseline."""

from __future__ import annotations

import os
from pathlib import Path
import platform
import re
import shutil
import subprocess
import sys
import tempfile


ROOT = Path(__file__).resolve().parents[2]
CORPUS = ROOT / "tests" / "obligation-advisory"
BASELINE = ROOT / "tests" / "obligation-advisory" / "baseline.tsv"
FIXED_SITES = ROOT / "tests" / "obligation-advisory" / "dogfood-fixed-sites.tsv"
DOGFOOD_SITE_COUNT = 93
HEW = Path(os.environ.get("HEW_BIN", ROOT / "target" / "debug" / "hew"))
RELEASE_HEW = Path(
    os.environ.get("HEW_RELEASE_BIN", ROOT / "target" / "release-lib" / "hew")
)
RUNTIME_CHECK = "check"
RUNTIME_CLEAN = "clean"
RUNTIME_REFUSE = "refuse"
RUNTIME_MODES = {RUNTIME_CHECK, RUNTIME_CLEAN, RUNTIME_REFUSE}
ZERO_LEAKS = re.compile(r"0 leaks for 0 total leaked bytes\.")
SANITIZER_FINDING = re.compile(
    r"ERROR: (?:AddressSanitizer|LeakSanitizer)"
    r"|detected memory leaks"
    r"|SUMMARY: (?:AddressSanitizer|LeakSanitizer)"
)
RUNTIME_REPORT_PREVIEW_LINES = 40


def bounded_runtime_report(name: str, headline: str, report: str) -> str:
    """Persist a runtime-oracle report and return a bounded diagnostic preview."""
    safe_name = re.sub(r"[^A-Za-z0-9_.-]+", "-", name).strip("-") or "fixture"
    with tempfile.NamedTemporaryFile(
        mode="w",
        encoding="utf-8",
        prefix=f"hew-obligation-{safe_name}-",
        suffix=".log",
        delete=False,
    ) as stream:
        stream.write(report)
        report_path = Path(stream.name)

    lines = report.splitlines()
    preview = lines[:RUNTIME_REPORT_PREVIEW_LINES]
    omitted = max(0, len(lines) - len(preview))
    preview_text = "\n".join(preview)
    if preview_text:
        preview_text += "\n"
    return (
        f"{name}: {headline}\n"
        f"{preview_text}"
        f"[{omitted} additional line(s) omitted; full report: {report_path}]"
    )


def read_baseline() -> dict[str, tuple[int, int, int, str]]:
    rows: dict[str, tuple[int, int, int, str]] = {}
    with BASELINE.open(encoding="utf-8") as stream:
        for line in stream:
            line = line.rstrip("\n")
            if not line or line.startswith("#"):
                continue
            name, advisories, blocking, exit_code, runtime = line.split("\t")
            if runtime not in RUNTIME_MODES:
                raise ValueError(f"{name}: unknown runtime mode {runtime!r}")
            rows[name] = (
                int(advisories),
                int(blocking),
                int(exit_code),
                runtime,
            )
    return rows


def read_fixed_sites(
    expected: dict[str, tuple[int, int, int, str]],
) -> tuple[int, int]:
    sites: set[str] = set()
    fixed = 0
    silenced = 0
    with FIXED_SITES.open(encoding="utf-8") as stream:
        for line in stream:
            line = line.rstrip("\n")
            if not line or line.startswith("#"):
                continue
            site, binding, disposition, mechanism, proof_text = line.split("\t")
            if site in sites:
                raise ValueError(f"duplicate dogfood site {site!r}")
            sites.add(site)
            if not binding or not mechanism:
                raise ValueError(f"{site}: binding and release mechanism are required")
            if disposition == "fixed":
                fixed += 1
            elif disposition == "silenced":
                silenced += 1
            else:
                raise ValueError(f"{site}: unknown disposition {disposition!r}")
            proofs = proof_text.split(",") if proof_text else []
            if disposition == "fixed" and not proofs:
                raise ValueError(f"{site}: fixed site has no leak-oracle proof")
            for proof in proofs:
                row = expected.get(proof)
                if row is None:
                    raise ValueError(f"{site}: unknown proof fixture {proof!r}")
                if row[3] != RUNTIME_CLEAN:
                    raise ValueError(f"{site}: proof fixture {proof!r} is not executed")
    if len(sites) != DOGFOOD_SITE_COUNT:
        raise ValueError(
            f"expected {DOGFOOD_SITE_COUNT} dogfood sites, found {len(sites)}"
        )
    if silenced:
        raise ValueError(f"dogfood classification retains {silenced} silenced site(s)")
    return fixed, silenced


def corpus_entries() -> set[str]:
    root_entries = {path.name for path in CORPUS.glob("*.hew")}
    module_entries = {
        str(path.relative_to(CORPUS)) for path in CORPUS.glob("*/main.hew")
    }
    return root_entries | module_entries


def run_fixture(
    compiler: Path, name: str, environment: dict[str, str]
) -> tuple[int, int, int]:
    result = subprocess.run(
        [str(compiler), "check", str(CORPUS / name)],
        cwd=ROOT,
        env=environment,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    under = result.stderr.count("MIR kind: ObligationUnderReleased")
    all_mir = result.stderr.count("MIR kind:")
    return under, all_mir - under, result.returncode


def runtime_oracle_available() -> tuple[str, str | None]:
    host = platform.system()
    if host == "Darwin":
        leaks = shutil.which("leaks")
        if leaks is None:
            return host, "leaks(1) is missing"
        return host, None
    if host == "Linux":
        if shutil.which("nm") is None:
            return host, "nm is missing"
        return host, None
    return host, f"no leak oracle is defined for {host}"


def compile_runtime_fixture(
    compiler: Path,
    name: str,
    environment: dict[str, str],
    output: Path,
    host: str,
) -> str | None:
    compile_environment = environment.copy()
    if host == "Linux":
        compile_environment["HEW_SANITIZE_ADDRESS"] = "1"
    result = subprocess.run(
        [
            str(compiler),
            "build",
            str(CORPUS / name),
            "--opt-level",
            "2",
            "-o",
            str(output),
        ],
        cwd=ROOT,
        env=compile_environment,
        capture_output=True,
        text=True,
        timeout=180,
        check=False,
    )
    if result.returncode != 0:
        return (
            f"{name}: build exited {result.returncode}\n{result.stdout}{result.stderr}"
        )
    if not output.is_file():
        return f"{name}: build produced no binary at {output}"
    if host == "Linux":
        symbols = subprocess.run(
            ["nm", "-D", str(output)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        if symbols.returncode != 0 or not re.search(
            r"__asan_init|__lsan_", symbols.stdout
        ):
            return f"{name}: linked binary carries no ASan/LSan runtime symbols"
    return None


def run_runtime_fixture(
    name: str,
    binary: Path,
    environment: dict[str, str],
    host: str,
) -> str | None:
    run_environment = environment.copy()
    run_environment["HEW_WORKERS"] = "1"
    if host == "Darwin":
        run_environment["MallocScribble"] = "1"
        run_environment["MallocPreScribble"] = "1"
        command = ["leaks", "--atExit", "--", str(binary)]
    else:
        run_environment["ASAN_OPTIONS"] = "detect_leaks=1:halt_on_error=1"
        run_environment["LSAN_OPTIONS"] = "suppressions=hew-runtime/lsan.supp"
        command = [str(binary)]
    result = subprocess.run(
        command,
        cwd=ROOT,
        env=run_environment,
        capture_output=True,
        text=True,
        timeout=180,
        check=False,
    )
    report = result.stdout + result.stderr
    if result.returncode != 0:
        return bounded_runtime_report(
            name, f"runtime oracle exited {result.returncode}", report
        )
    if host == "Darwin" and ZERO_LEAKS.search(report) is None:
        return bounded_runtime_report(
            name, "leaks(1) did not report zero leaks", report
        )
    if host == "Linux" and SANITIZER_FINDING.search(report) is not None:
        return bounded_runtime_report(
            name, "ASan/LSan reported a memory finding", report
        )
    return None


def main() -> int:
    compilers = (("debug", HEW), ("release", RELEASE_HEW))
    for profile, compiler in compilers:
        if not compiler.is_file():
            print(
                f"error: {profile} compiler binary not found: {compiler}",
                file=sys.stderr,
            )
            return 1
    try:
        expected = read_baseline()
    except (OSError, ValueError) as error:
        print(f"error: invalid baseline: {error}", file=sys.stderr)
        return 1
    try:
        fixed_sites, silenced_sites = read_fixed_sites(expected)
    except (OSError, ValueError) as error:
        print(f"error: invalid dogfood classification: {error}", file=sys.stderr)
        return 1
    entries = corpus_entries()
    if not expected or set(expected) != entries:
        print(
            "error: baseline and corpus entry inventory differ: "
            f"baseline={sorted(expected)} corpus={sorted(entries)}",
            file=sys.stderr,
        )
        return 1
    clean_fixtures = sorted(
        name for name, row in expected.items() if row[3] == RUNTIME_CLEAN
    )
    refused_fixtures = sorted(
        name for name, row in expected.items() if row[3] == RUNTIME_REFUSE
    )
    if not clean_fixtures:
        print("error: corpus has no executable leak-oracle fixture", file=sys.stderr)
        return 1
    if not refused_fixtures or any(
        expected[name][1] == 0 or expected[name][2] == 0 for name in refused_fixtures
    ):
        print(
            "error: corpus must retain a blocking MIR refusal with non-zero exit",
            file=sys.stderr,
        )
        return 1
    host, oracle_error = runtime_oracle_available()
    if oracle_error is not None:
        print(f"error: {oracle_error}", file=sys.stderr)
        return 1

    inherited = os.environ.copy()
    scrubbed = {
        key: value for key, value in inherited.items() if not key.startswith("HEW_")
    }
    failures: list[str] = []
    totals = [0, 0]
    environments = (("inherited", inherited), ("no-HEW-env", scrubbed))
    for build_profile, compiler in compilers:
        for environment_profile, environment in environments:
            for name in sorted(expected):
                observed = run_fixture(compiler, name, environment)
                wanted = expected[name][:3]
                totals[0] += observed[0]
                totals[1] += observed[1]
                if observed != wanted:
                    failures.append(
                        f"{build_profile}/{environment_profile} {name}: "
                        f"expected {wanted}, observed {observed}"
                    )

    runtime_environment = scrubbed.copy()
    with tempfile.TemporaryDirectory(prefix="hew-obligation-runtime-") as directory:
        output_dir = Path(directory)
        for index, name in enumerate(clean_fixtures):
            binary = output_dir / f"fixture-{index}"
            failure = compile_runtime_fixture(
                HEW, name, runtime_environment, binary, host
            )
            if failure is None:
                failure = run_runtime_fixture(name, binary, runtime_environment, host)
            if failure is not None:
                failures.append(f"runtime/{host} {failure}")

    if failures:
        print("ownership-advisory baseline drift:", file=sys.stderr)
        for failure in failures:
            print(f"  {failure}", file=sys.stderr)
        return 1
    print(
        "ownership-advisory: "
        f"fixtures={len(expected)} profiles=4 advisories={totals[0]} "
        f"blocking_mir={totals[1]} runtime={len(clean_fixtures)} "
        f"refusals={len(refused_fixtures)} fixed_sites={fixed_sites} "
        f"silenced_sites={silenced_sites} oracle={host}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
