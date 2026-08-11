#!/usr/bin/env python3
"""Apply and check centralized workflow and nextest hang ceilings."""

from __future__ import annotations

import argparse
from pathlib import Path
import re
import sys


ROOT = Path(__file__).resolve().parent.parent
WORKFLOWS = ROOT / ".github/workflows"
NEXTEST = ROOT / ".config/nextest.toml"

# GitHub evaluates a job timeout before a runner exists, so no workload input
# can calibrate it at runtime. This table centralizes the explicit constants
# and lets the checker reject drift or a job without a ceiling.
WORKFLOW_TIMEOUTS = {
    "alpine-llvm.yml/build-image": 120,
    "alpine-llvm.yml/manifest": 10,
    "ci-local.yml/provisioning-smoke": 45,
    "ci.yml/changes": 5,
    "ci.yml/docs-and-scripts": 15,
    "ci.yml/lint": 45,
    "ci.yml/license-check": 10,
    "ci.yml/playground-wasm-build": 20,
    "ci.yml/compiled-hew-linux": 45,
    "ci.yml/compiled-hew-shards": 60,
    "ci.yml/compiled-hew-aggregate": 10,
    "ci.yml/build-and-test": 180,
    "ci.yml/linux-required": 5,
    "ci.yml/build-and-test-windows": 60,
    "ci.yml/build-and-test-macos": 60,
    "coverage-nightly.yml/runtime-e2e-coverage": 45,
    "coverage-nightly.yml/coverage": 30,
    "coverage-nightly.yml/full-windows": 60,
    "coverage-nightly.yml/full-macos": 60,
    "deploy-docs.yml/deploy": 20,
    "freebsd.yml/build-and-test": 180,
    "nightly-sanitizers.yml/rust-runtime-asan": 45,
    "nightly-sanitizers.yml/compiled-fixture-asan": 60,
    "nightly-sanitizers.yml/rust-runtime-tsan": 45,
    "nightly-sanitizers.yml/rust-runtime-miri": 45,
    "nightly-sanitizers.yml/parser-fuzz-smoke": 40,
    "publish-npm-packages.yml/publish": 30,
    "release-gate.yml/gate-sanitizers": 45,
    "release-gate.yml/gate-linux": 120,
    "release-gate.yml/gate-linux-aarch64": 45,
    "release-gate.yml/gate-macos": 180,
    "release-gate.yml/gate-windows": 60,
    "release-gate.yml/gate-freebsd-x86_64": 180,
    "release-gate.yml/gate-freebsd-aarch64": 300,
    "release.yml/build": 90,
    "release.yml/build-linux": 180,
    "release.yml/build-freebsd": 180,
    "release.yml/build-freebsd-aarch64": 300,
    "release.yml/linux-packages": 60,
    "release.yml/docker-clean-room-test": 20,
    "release.yml/docker": 180,
    "release.yml/release": 60,
    "release.yml/homebrew": 5,
    "release.yml/playground": 120,
    "release.yml/vscode-extension": 15,
    "release.yml/vscode-publish": 10,
    "stdlib-lint.yml/stdlib-int-surface": 15,
}

# Nextest periods are ten-second quanta. Termination counts come from the
# selected workload class; test-internal deadlines and leak grace periods are
# semantic assertions and are intentionally outside this policy.
NEXTEST_CLASSES = {
    "standard": (10, 3),
    "intel-macos": (30, 3),
    "subprocess": (12, 4),
    "ffi-link": (6, 3),
    "timing": (3, 3),
    "thread-handshake": (3, 2),
    "distributed": (12, 3),
    "qemu-network": (12, 6),
    "smoke": (6, 2),
}


def render_workflow(path: Path, text: str) -> tuple[str, set[str]]:
    job = None
    in_jobs = False
    jobs: set[str] = set()
    seen: set[str] = set()
    rendered: list[str] = []
    for line in text.splitlines(keepends=True):
        if line.rstrip("\n") == "jobs:":
            in_jobs = True
            rendered.append(line)
            continue
        if in_jobs and re.match(r"^[^\s#]", line):
            in_jobs = False
            job = None
        match = re.match(r"^  ([A-Za-z0-9_-]+):\s*$", line) if in_jobs else None
        if match is not None:
            job = match.group(1)
            jobs.add(job)
        timeout = re.match(r"^(    )timeout-minutes:\s*\d+\s*$", line.rstrip("\n"))
        if timeout:
            if not in_jobs or job is None:
                raise ValueError(f"{path}: timeout outside a job")
            key = f"{path.name}/{job}"
            try:
                minutes = WORKFLOW_TIMEOUTS[key]
            except KeyError as error:
                raise ValueError(f"{path}: no timeout policy for {job}") from error
            seen.add(key)
            newline = "\n" if line.endswith("\n") else ""
            line = f"{timeout.group(1)}timeout-minutes: {minutes}{newline}"
        rendered.append(line)
    missing_timeouts = jobs - {key.split("/", 1)[1] for key in seen}
    if missing_timeouts:
        raise ValueError(
            f"{path}: jobs without timeout-minutes: {sorted(missing_timeouts)}"
        )
    return "".join(rendered), seen


def render_nextest(text: str) -> str:
    count = 0

    def replace(match: re.Match[str]) -> str:
        nonlocal count
        workload = match.group(2)
        if workload not in NEXTEST_CLASSES:
            raise ValueError(f"unknown nextest timeout class: {workload}")
        period_units, terminate_after = NEXTEST_CLASSES[workload]
        count += 1
        return (
            f"{match.group(1)}# timeout-class: {workload}\n"
            f'{match.group(1)}slow-timeout = {{ period = "{period_units * 10}s", '
            f"terminate-after = {terminate_after} }}"
        )

    rendered = re.sub(
        r'(?m)^(\s*)# timeout-class: ([a-z0-9-]+)\n\1slow-timeout\s*=\s*\{\s*period\s*=\s*"[0-9]+s"\s*,\s*terminate-after\s*=\s*[0-9]+\s*\}',
        replace,
        text,
    )
    total = len(re.findall(r"(?m)^\s*slow-timeout\s*=", text))
    if count != total:
        raise ValueError(
            f"nextest.toml has {total} slow-timeout entries but {count} class markers"
        )
    return rendered


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args(argv)
    drift: list[Path] = []
    all_seen: set[str] = set()
    for path in sorted(WORKFLOWS.glob("*.yml")):
        original = path.read_text(encoding="utf-8")
        try:
            rendered, seen = render_workflow(path, original)
        except ValueError as error:
            print(f"timeout policy error: {error}", file=sys.stderr)
            return 1
        all_seen.update(seen)
        if rendered != original:
            if args.write:
                path.write_text(rendered, encoding="utf-8")
            else:
                drift.append(path)
    missing = set(WORKFLOW_TIMEOUTS) - all_seen
    if missing:
        raise SystemExit(
            f"timeout policy names jobs without timeouts: {sorted(missing)}"
        )

    original = NEXTEST.read_text(encoding="utf-8")
    rendered = render_nextest(original)
    if rendered != original:
        if args.write:
            NEXTEST.write_text(rendered, encoding="utf-8")
        else:
            drift.append(NEXTEST)
    if drift:
        print(
            "timeout policy drift: " + ", ".join(str(path) for path in drift),
            file=sys.stderr,
        )
        print("run scripts/timeout-policy.py --write", file=sys.stderr)
        return 1
    nextest_count = len(re.findall(r"(?m)^\s*slow-timeout\s*=", original))
    print(
        f"timeout policy: {len(all_seen)} workflow jobs and "
        f"{nextest_count} nextest ceilings match centralized constants"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
