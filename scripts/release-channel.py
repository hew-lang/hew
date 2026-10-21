#!/usr/bin/env python3
"""Publish proved release artefacts, or verify an identical completed delivery."""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tarfile
import tempfile
import time


def command(*args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(args, text=True, capture_output=True)
    if check and result.returncode:
        raise RuntimeError(f"{args[0]} failed: {result.stderr or result.stdout}")
    return result


def digest(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def npm_dist(spec: str, registry: str) -> dict | None:
    result = command(
        "npm",
        "view",
        spec,
        "dist",
        "--json",
        "--registry",
        registry,
        f"--@hew-lang:registry={registry}",
        check=False,
    )
    if result.returncode:
        # Only an explicit registry not-found response admits a new publish.
        try:
            error = json.loads(result.stdout)["error"]
        except (ValueError, KeyError, TypeError) as exc:
            raise RuntimeError("Cannot determine npm publication state") from exc
        if error.get("code") == "E404":
            return None
        raise RuntimeError(f"Cannot determine npm publication state: {error}")
    metadata = json.loads(result.stdout)
    if not isinstance(metadata, dict) or not metadata.get("tarball"):
        raise RuntimeError("Registry returned no npm tarball")
    return metadata


def npm_verify(spec: str, registry: str, archive: Path) -> None:
    with tempfile.TemporaryDirectory(prefix="hew-npm-") as temp:
        packed = json.loads(
            command(
                "npm",
                "pack",
                spec,
                "--ignore-scripts",
                "--json",
                "--registry",
                registry,
                f"--@hew-lang:registry={registry}",
                "--pack-destination",
                temp,
                "--cache",
                str(Path(temp) / "cache"),
            ).stdout
        )
        remote = Path(temp) / packed[0]["filename"]
        if remote.parent != Path(temp) or digest(remote.read_bytes()) != digest(
            archive.read_bytes()
        ):
            raise RuntimeError(f"Published bytes differ for {spec}; refusing overwrite")
    print(f"Verified identical tarball: {spec} on {registry}")


def publish_npm() -> None:
    version, registry = os.environ["VERSION"], os.environ["REGISTRY"]
    for package in ("wasm", "sandbox-vm"):
        archive = Path(f"packages/hew-lang-{package}-{version}.tgz")
        with tarfile.open(archive) as tar:
            manifest_file = tar.extractfile("package/package.json")
            if manifest_file is None:
                raise RuntimeError("Producer package manifest missing")
            manifest = json.load(manifest_file)
        if (
            manifest.get("name") != f"@hew-lang/{package}"
            or manifest.get("version") != version
            or manifest.get("hewSource", {}).get("commit") != os.environ["GITHUB_SHA"]
            or manifest["hewSource"].get("dirty") is not False
        ):
            raise RuntimeError(f"Producer package source/version mismatch: {archive}")
        spec = f"@hew-lang/{package}@{version}"
        if npm_dist(spec, registry) is None:
            args = [
                "npm",
                "publish",
                str(archive.resolve()),
                "--ignore-scripts",
                "--access",
                "public",
                "--tag",
                os.environ["DIST_TAG"],
                "--registry",
                registry,
                f"--@hew-lang:registry={registry}",
            ]
            if registry == "https://registry.npmjs.org":
                args.append("--provenance")
            command(*args)
            if npm_dist(spec, registry) is None:
                raise RuntimeError(f"Published package is unreadable: {spec}")
        npm_verify(spec, registry, archive)


def oci_raw(reference: str, *, absent: bool = False) -> str | None:
    result = command("skopeo", "inspect", "--raw", reference, check=False)
    if result.returncode:
        if absent and re.search(
            r"\b(manifest unknown|name unknown)\b", result.stderr, re.I
        ):
            return None
        raise RuntimeError(f"Cannot inspect OCI outcome: {result.stderr}")
    return result.stdout


def index_matches(raw: str, expected: dict[str, str]) -> bool:
    manifests = json.loads(raw).get("manifests", [])
    actual = {}
    for item in manifests:
        platform = item.get("platform", {})
        arch = platform.get("architecture")
        if platform.get("os") != "linux" or arch in actual:
            return False
        actual[arch] = item.get("digest")
    return actual == expected


def publish_ghcr() -> None:
    image, version = os.environ["IMAGE"], os.environ["VERSION"]
    expected = {}
    for arch in ("amd64", "arm64"):
        reference = f"oci-archive:images/hew-image-{arch}.tar"
        raw = oci_raw(reference)
        config = json.loads(command("skopeo", "inspect", "--config", reference).stdout)
        labels = config.get("config", {}).get("Labels", {})
        if (
            config.get("architecture") != arch
            or config.get("os") != "linux"
            or labels.get("org.opencontainers.image.revision")
            != os.environ["GITHUB_SHA"]
            or labels.get("org.opencontainers.image.version") != version
        ):
            raise RuntimeError(f"Producer OCI source/platform mismatch: {arch}")
        expected[arch] = digest(raw.encode())
    existing = oci_raw(f"docker://{image}:{version}", absent=True)
    if existing is not None:
        if not index_matches(existing, expected):
            raise RuntimeError("Published OCI index differs; refusing overwrite")
    else:
        # Preflight every immutable architecture tag before making any writes.
        for arch, wanted in expected.items():
            remote = oci_raw(f"docker://{image}:{version}-{arch}", absent=True)
            if remote is not None and digest(remote.encode()) != wanted:
                raise RuntimeError(f"Published OCI architecture differs: {arch}")
        for arch in expected:
            command(
                "skopeo",
                "copy",
                "--preserve-digests",
                f"oci-archive:images/hew-image-{arch}.tar",
                f"docker://{image}:{version}-{arch}",
            )
        args = [
            "docker",
            "buildx",
            "imagetools",
            "create",
            "--tag",
            f"{image}:{version}",
        ]
        if os.environ["PRERELEASE"] == "false":
            args += ["--tag", f"{image}:latest"]
        command(*args, *(f"{image}:{version}-{arch}" for arch in expected))
        existing = oci_raw(f"docker://{image}:{version}")
        if not index_matches(existing, expected):
            raise RuntimeError("Published OCI index does not match producer images")
    # Verify child manifests too, even when the version index already exists.
    for wanted in expected.values():
        remote = oci_raw(f"docker://{image}@{wanted}")
        if digest(remote.encode()) != wanted:
            raise RuntimeError("Published OCI child digest mismatch")
    with open(os.environ["GITHUB_OUTPUT"], "a") as output:
        output.write(f"digest={digest(existing.encode())}\n")
    print("Verified OCI index and both producer architecture digests")


def gh_json(*args: str):
    return json.loads(command("gh", *args).stdout)


def verify_formula(text: str, version: str, checksums: dict[str, str]) -> None:
    match = re.search(r'^\s*version "([^"]+)"', text, re.M)
    if match is None or match[1] != version:
        raise RuntimeError("Homebrew formula version differs")
    entries = re.findall(r'url "([^"]+)"\s+sha256 "([a-f0-9]{64})"', text)
    expected = {
        f"https://github.com/hew-lang/hew/releases/download/v#{{version}}/hew-v#{{version}}-{target}.tar.gz": checksum
        for target, checksum in checksums.items()
    }
    if len(entries) != len(expected) or dict(entries) != expected:
        raise RuntimeError(
            "Homebrew formula source/checksums differ from release artefacts"
        )


def publish_homebrew() -> None:
    version = os.environ["VERSION"]
    repo = "hew-lang/homebrew-hew"
    workflow = "update-formula.yml"
    receipt_path = Path(f"dist/hew-v{version}-platforms.json")
    if receipt_path.is_file():
        receipt = json.loads(receipt_path.read_text())
        if (
            receipt.get("version") != version
            or receipt.get("source") != os.environ["GITHUB_SHA"]
        ):
            raise RuntimeError("Homebrew platform receipt source/version differs")
        macos = receipt.get("platforms", {}).get("darwin-aarch64", {})
        if macos.get("state") == "deferred":
            message = "Homebrew deferred: macOS archive unavailable. " + json.dumps(
                macos
            )
            print(f"::notice::{message}", flush=True)
            if summary := os.environ.get("GITHUB_STEP_SUMMARY"):
                with Path(summary).open("a") as output:
                    output.write(message + "\n")
            return
    if not os.environ.get("GH_TOKEN"):
        raise RuntimeError("HOMEBREW_TAP_TOKEN is not set")
    # Producer checksums come from the downloaded release-assets artefact.
    checksums = {}
    lines = Path(f"dist/hew-v{version}-checksums.txt").read_text().splitlines()
    for target in ("darwin-aarch64", "linux-x86_64", "linux-aarch64"):
        name = f"hew-v{version}-{target}.tar.gz"
        matches = [line.split()[0] for line in lines if line.split()[-1] == name]
        if len(matches) != 1 or not re.fullmatch(r"[a-f0-9]{64}", matches[0]):
            raise RuntimeError(f"Missing producer checksum: {name}")
        checksums[target] = matches[0]
    fields = "databaseId,headSha,status,conclusion,event,url"

    def runs():
        return gh_json(
            "run",
            "list",
            "-R",
            repo,
            "--workflow",
            workflow,
            "--event",
            "workflow_dispatch",
            "--limit",
            "100",
            "--json",
            fields,
        )

    prior = runs()
    branch = gh_json("api", f"repos/{repo}")["default_branch"]

    def run_version(run):
        log = command(
            "gh", "run", "view", str(run["databaseId"]), "-R", repo, "--log"
        ).stdout
        versions = set(re.findall(r"\bVERSION:[ \t]+([^\s]+)[ \t]*$", log, re.M))
        if len(versions) != 1:
            raise RuntimeError(
                "Cannot identify Homebrew run version; inspect downstream run"
            )
        return versions.pop()

    def finish(run, outcome):
        terminal = gh_json(
            "run",
            "view",
            str(run["databaseId"]),
            "-R",
            repo,
            "--json",
            "status,conclusion",
        )
        if terminal != {"status": "completed", "conclusion": "success"}:
            raise RuntimeError("Homebrew update did not complete successfully")
        if run_version(run) != version:
            raise RuntimeError("Homebrew run version differs")
        source = gh_json("api", f"repos/{repo}/commits/{branch}")["sha"]
        formulas = ["hew.rb"] + ([] if "-" in version else ["hew@stable.rb"])
        for formula in formulas:
            text = command(
                "gh",
                "api",
                f"repos/{repo}/contents/Formula/{formula}?ref={source}",
                "-H",
                "Accept: application/vnd.github.raw+json",
            ).stdout
            verify_formula(text, version, checksums)
        print(
            json.dumps(
                {
                    "result": "complete",
                    "outcome": outcome,
                    "run_id": run["databaseId"],
                    "run_url": run["url"],
                    "formula_source": source,
                    "version": version,
                },
                sort_keys=True,
            )
        )

    # A retry first proves the existing downstream result. Unreadable run logs
    # are an unknown outcome and must not trigger another dispatch.
    active = [
        run
        for run in prior
        if run["status"] != "completed" and run_version(run) == version
    ]
    if len(active) > 1:
        raise RuntimeError("Ambiguous active Homebrew runs; inspect downstream runs")
    if active:
        run = active[0]
        command(
            "gh", "run", "watch", str(run["databaseId"]), "-R", repo, "--exit-status"
        )
        finish(run, "verified-existing")
        return
    for run in prior:
        if (
            run["status"] == "completed"
            and run["conclusion"] == "success"
            and run_version(run) == version
        ):
            finish(run, "verified-existing")
            return
    if len(prior) == 100:
        raise RuntimeError(
            "Homebrew history is truncated; inspect older runs before dispatch"
        )

    before = {run["databaseId"] for run in prior}
    head = gh_json("api", f"repos/{repo}/commits/{branch}")["sha"]
    dispatched = command(
        "gh",
        "workflow",
        "run",
        workflow,
        "-R",
        repo,
        "--ref",
        branch,
        "-f",
        f"version={version}",
    )
    urls = set(
        re.findall(
            r"https://github\.com/hew-lang/homebrew-hew/actions/runs/(\d+)",
            dispatched.stdout,
        )
    )
    if len(urls) > 1:
        raise RuntimeError("Ambiguous Homebrew dispatch response")
    run = None
    if urls:
        run_id = int(urls.pop())
        run = {
            "databaseId": run_id,
            "url": f"https://github.com/{repo}/actions/runs/{run_id}",
        }
    else:
        for _ in range(30):
            candidates = [item for item in runs() if item["databaseId"] not in before]
            if len(candidates) > 1:
                raise RuntimeError(
                    "Ambiguous Homebrew dispatch; inspect downstream runs"
                )
            if candidates:
                run = candidates[0]
                if run["headSha"] != head:
                    raise RuntimeError(
                        "Homebrew workflow source changed during dispatch"
                    )
                break
            time.sleep(2)
    if run is None:
        raise RuntimeError("Homebrew dispatch has no readable downstream run")
    print(f"Waiting for Homebrew run: {run['url']}", flush=True)
    command("gh", "run", "watch", str(run["databaseId"]), "-R", repo, "--exit-status")
    finish(run, "verified-after-action")


if __name__ == "__main__":
    try:
        {"npm": publish_npm, "ghcr": publish_ghcr, "homebrew": publish_homebrew}[
            sys.argv[1]
        ]()
    except (RuntimeError, ValueError, KeyError, OSError, tarfile.TarError) as error:
        print(f"::error::{error}", file=sys.stderr)
        sys.exit(1)
