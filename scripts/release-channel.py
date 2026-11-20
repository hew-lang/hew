#!/usr/bin/env python3
"""Publish proved release artefacts, or verify an identical completed delivery."""

from __future__ import annotations

import base64
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


def npm_integrity(spec: str, registry: str) -> str | None:
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
    integrity = metadata.get("integrity") if isinstance(metadata, dict) else None
    if not isinstance(integrity, str) or not integrity:
        raise RuntimeError(f"Registry returned no dist.integrity for {spec}")
    return integrity


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
        integrity = npm_integrity(spec, registry)
        if integrity is not None:
            with archive.open("rb") as payload:
                sha512 = hashlib.file_digest(payload, "sha512").digest()
            expected = "sha512-" + base64.b64encode(sha512).decode("ascii")
            if integrity != expected:
                raise RuntimeError(
                    f"Published integrity differs for {spec}; refusing overwrite"
                )
            print(f"Already published with matching integrity: {spec} on {registry}")
            continue
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
        print(f"Published: {spec} on {registry}")


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


def verified_release_checksums(
    manifest: Path, attestations: list[dict]
) -> dict[str, str]:
    entries = {}
    for line in manifest.read_text().splitlines():
        match = re.fullmatch(r"([0-9a-f]{64})  ([A-Za-z0-9][A-Za-z0-9._+-]*)", line)
        if not match or match[2] in entries:
            raise RuntimeError("Invalid release checksum manifest")
        entries[match[2]] = match[1]

    # The release workflow attests the receipt and every asset in one statement.
    # Match one complete statement, since retries can yield multiple attestations.
    expected = sorted(entries.items())
    if not entries or not any(
        sorted(
            (subject["name"], subject["digest"]["sha256"])
            for subject in result["verificationResult"]["statement"]["subject"]
        )
        == expected
        for result in attestations
    ):
        raise RuntimeError("Release checksums do not match the producer attestation")
    return entries


def defer_distro(reason: str) -> None:
    print(f"::notice::{reason}")
    with Path(os.environ["GITHUB_ENV"]).open("a") as output:
        output.write("SKIP=1\n")


def download_distro() -> None:
    tag, repo, ref = (os.environ[name] for name in ("TAG", "GH_REPO", "GITHUB_REF"))
    if ref != f"refs/tags/{tag}":
        raise RuntimeError(
            "Select the same release tag for the workflow ref and package input"
        )
    if "-" in tag:
        defer_distro(f"{tag} is a prerelease and carries no distro packages")
        return

    with tempfile.TemporaryDirectory(prefix="hew-distro-receipt-") as temp:
        receipt = Path(temp) / f"hew-{tag}-platforms.json"
        checksums = Path(temp) / f"hew-{tag}-checksums.txt"
        download = ("gh", "release", "download", tag, "--repo", repo)
        command(
            *download,
            "--dir",
            temp,
            "--pattern",
            receipt.name,
            "--pattern",
            checksums.name,
        )
        inventory = json.loads(receipt.read_text())
        if inventory["version"] != tag.removeprefix("v"):
            raise RuntimeError("Distro release receipt version differs")
        attestations = json.loads(
            command(
                "gh",
                "attestation",
                "verify",
                str(receipt),
                "--repo",
                repo,
                "--signer-workflow",
                f"{repo}/.github/workflows/release.yml",
                "--source-ref",
                ref,
                "--source-digest",
                inventory["source"],
                "--format",
                "json",
            ).stdout
        )
        entries = verified_release_checksums(checksums, attestations)

    state = inventory["platforms"]["linux-packages"]["state"]
    if state == "unavailable":
        defer_distro(
            f"{tag} has no qualified Linux package set; keeping the existing stable repository"
        )
        return
    if state != "available":
        raise RuntimeError(f"Unexpected Linux package state: {state}")
    packages = {
        name: checksum
        for name, checksum in entries.items()
        if name.endswith((".deb", ".rpm", ".pkg.tar.zst"))
        and not name.endswith(".src.rpm")
    }
    if not packages:
        raise RuntimeError("The qualified release has no distro package checksums")

    directory = Path("pkgs")
    directory.mkdir()
    patterns = [argument for name in packages for argument in ("--pattern", name)]
    command(*download, "--dir", str(directory), *patterns)
    if {path.name for path in directory.iterdir()} != packages.keys():
        raise RuntimeError(
            "Downloaded distro package names differ from the qualified set"
        )
    for name, checksum in packages.items():
        with (directory / name).open("rb") as payload:
            if hashlib.file_digest(payload, "sha256").hexdigest() != checksum:
                raise RuntimeError(f"Downloaded distro package differs: {name}")
    print(f"Verified {len(packages)} distro packages for {tag}")


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
        if macos.get("state") == "unavailable":
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
    fields = "databaseId,headSha,status,conclusion,event,url,displayTitle"
    title = f"Update Hew {version}"
    published = gh_json(
        "release", "view", f"v{version}", "-R", "hew-lang/hew", "--json", "publishedAt"
    )["publishedAt"]
    if not published:
        raise RuntimeError("Source release has no publication time")

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
            "--created",
            f">={published}",
            "--limit",
            "100",
            "--json",
            fields,
        )

    prior = runs()
    branch = gh_json("api", f"repos/{repo}")["default_branch"]

    def finish(run, outcome):
        terminal = gh_json(
            "run",
            "view",
            str(run["databaseId"]),
            "-R",
            repo,
            "--json",
            "status,conclusion,displayTitle",
        )
        if (
            terminal.get("status") != "completed"
            or terminal.get("conclusion") != "success"
        ):
            raise RuntimeError("Homebrew update did not complete successfully")
        if terminal.get("displayTitle") != title:
            raise RuntimeError("Homebrew run version differs")
        source = gh_json("api", f"repos/{repo}/commits/{branch}")["sha"]
        formulas = ["hew.rb"] + ([] if "-" in version else ["hew-stable.rb"])
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

    # Version-bearing workflow metadata survives log expiry. Older runs predate
    # this source release and cannot be its successful downstream delivery.
    active = [
        run
        for run in prior
        if run["status"] != "completed" and run.get("displayTitle") == title
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
            and run.get("displayTitle") == title
        ):
            finish(run, "verified-existing")
            return
    ambiguous = [
        run
        for run in prior
        if not run.get("displayTitle", "").startswith("Update Hew ")
    ]
    if ambiguous:
        raise RuntimeError(
            "Homebrew runs since source publication lack version metadata; "
            f"reconcile before redispatch: {[run['url'] for run in ambiguous]}"
        )
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
            candidates = [
                item
                for item in runs()
                if item["databaseId"] not in before
                and item.get("displayTitle") == title
            ]
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
    # A successful dispatch can return its run URL before GitHub serves that run.
    for attempt in range(30):
        watched = command(
            "gh",
            "run",
            "watch",
            str(run["databaseId"]),
            "-R",
            repo,
            "--exit-status",
            check=False,
        )
        if watched.returncode == 0:
            break
        message = watched.stderr or watched.stdout
        if "failed to get run: HTTP 404:" not in message or attempt == 29:
            raise RuntimeError(
                f"Cannot watch Homebrew run {run['databaseId']}: {message}"
            )
        time.sleep(2)
    finish(run, "verified-after-action")


if __name__ == "__main__":
    try:
        {
            "npm": publish_npm,
            "ghcr": publish_ghcr,
            "homebrew": publish_homebrew,
            "distro": download_distro,
        }[sys.argv[1]]()
    except (RuntimeError, ValueError, KeyError, OSError, tarfile.TarError) as error:
        print(f"::error::{error}", file=sys.stderr)
        sys.exit(1)
