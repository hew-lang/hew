#!/usr/bin/env python3
"""Render and check centrally derived workflow and nextest hang ceilings."""

from __future__ import annotations

import argparse
from pathlib import Path
import re
import sys


ROOT = Path(__file__).resolve().parent.parent
WORKFLOWS = ROOT / ".github/workflows"
NEXTEST = ROOT / ".config/nextest.toml"

# GitHub evaluates a job timeout before a runner exists, so measured CPU count
# is unavailable here. Jobs select a calibrated workload class; the dispatcher
# remains the runtime-scaled authority inside a runner.
WORKFLOW_MINUTES = {
    "sentinel": 5,
    "metadata": 10,
    "focused": 15,
    "browser": 20,
    "publication": 30,
    "fuzz": 40,
    "compile": 45,
    "platform": 60,
    "release-build": 90,
    "toolchain": 120,
    "emulated": 180,
    "qemu": 300,
}

WORKFLOW_CLASSES = {
    "alpine-llvm.yml/build-image": "toolchain",
    "alpine-llvm.yml/manifest": "metadata",
    "ci-local.yml/provisioning-smoke": "compile",
    "ci.yml/changes": "sentinel",
    "ci.yml/docs-and-scripts": "focused",
    "ci.yml/lint": "compile",
    "ci.yml/license-check": "metadata",
    "ci.yml/playground-wasm-build": "browser",
    "ci.yml/compiled-hew-linux": "compile",
    "ci.yml/compiled-hew-shards": "platform",
    "ci.yml/compiled-hew-aggregate": "metadata",
    "ci.yml/build-and-test": "emulated",
    "ci.yml/linux-required": "sentinel",
    "ci.yml/build-and-test-windows": "platform",
    "ci.yml/build-and-test-macos": "platform",
    "coverage-nightly.yml/runtime-e2e-coverage": "compile",
    "coverage-nightly.yml/coverage": "publication",
    "coverage-nightly.yml/full-windows": "platform",
    "coverage-nightly.yml/full-macos": "platform",
    "deploy-docs.yml/deploy": "browser",
    "freebsd.yml/build-and-test": "emulated",
    "nightly-sanitizers.yml/rust-runtime-asan": "compile",
    "nightly-sanitizers.yml/compiled-fixture-asan": "platform",
    "nightly-sanitizers.yml/rust-runtime-tsan": "compile",
    "nightly-sanitizers.yml/rust-runtime-miri": "compile",
    "nightly-sanitizers.yml/parser-fuzz-smoke": "fuzz",
    "publish-npm-packages.yml/publish": "publication",
    "release-gate.yml/gate-sanitizers": "compile",
    "release-gate.yml/gate-linux": "toolchain",
    "release-gate.yml/gate-linux-aarch64": "compile",
    "release-gate.yml/gate-macos": "emulated",
    "release-gate.yml/gate-windows": "platform",
    "release-gate.yml/gate-freebsd-x86_64": "emulated",
    "release-gate.yml/gate-freebsd-aarch64": "qemu",
    "release.yml/build": "release-build",
    "release.yml/build-linux": "emulated",
    "release.yml/build-freebsd": "emulated",
    "release.yml/build-freebsd-aarch64": "qemu",
    "release.yml/linux-packages": "platform",
    "release.yml/docker-clean-room-test": "browser",
    "release.yml/docker": "emulated",
    "release.yml/release": "platform",
    "release.yml/homebrew": "sentinel",
    "release.yml/playground": "toolchain",
    "release.yml/vscode-extension": "focused",
    "release.yml/vscode-publish": "metadata",
    "stdlib-lint.yml/stdlib-int-surface": "focused",
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
                workload = WORKFLOW_CLASSES[key]
            except KeyError as error:
                raise ValueError(f"{path}: no timeout policy for {job}") from error
            seen.add(key)
            newline = "\n" if line.endswith("\n") else ""
            line = f"{timeout.group(1)}timeout-minutes: {WORKFLOW_MINUTES[workload]}{newline}"
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
    missing = set(WORKFLOW_CLASSES) - all_seen
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
        f"{nextest_count} nextest ceilings are derived"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
