#!/usr/bin/env python3
"""Assemble the producer's available, checksummed release set and platform receipt."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re


NATIVE = {
    "linux-x86_64": ".tar.gz",
    "linux-aarch64": ".tar.gz",
    "darwin-aarch64": ".tar.gz",
    "windows-x86_64": ".zip",
    "freebsd-x86_64": ".tar.gz",
}
CHANNEL_JOBS = {"linux-packages", "docker-image", "build-docs"}
PRIMARY = {"linux-x86_64", "linux-aarch64"}


def assemble(directory, version, source, run, jobs, deferrals):
    if not re.fullmatch(r"[0-9]+\.[0-9]+\.[0-9]+(?:-[0-9A-Za-z.-]+)?", version):
        raise ValueError("invalid release version")
    if not re.fullmatch(r"[a-f0-9]{40}", source):
        raise ValueError("release source must be a full commit identity")
    patch = int(version.split("-", 1)[0].split(".")[2]) > 0
    for name in ("validate", "build-linux", "build-npm-packages"):
        if jobs.get(name, {}).get("result") != "success":
            raise ValueError(f"required producer did not succeed: {name}")
    directory = Path(directory)
    states = {}
    for target, suffix in NATIVE.items():
        name = f"hew-v{version}-{target}{suffix}"
        present = (directory / name).is_file() and (directory / name).stat().st_size > 0
        if not present and (target in PRIMARY or not patch):
            raise ValueError(f"missing required qualified archive: {name}")
        states[target] = {
            "state": "available" if present else "unavailable",
            "asset": name,
        }
    for target in CHANNEL_JOBS:
        result = jobs.get(target, {}).get("result", "missing")
        # Distro packages are not produced for prereleases.
        expected = target != "linux-packages" or "-" not in version
        missing = []
        if expected and target == "linux-packages":
            missing = [
                pattern
                for pattern in ("hew_*.deb", "hew-*.rpm", "hew-bin-*.pkg.tar.zst")
                if not any(
                    path.is_file() and path.stat().st_size
                    for path in directory.glob(pattern)
                )
            ]
        if target == "build-docs":
            docs_asset = f"hew-v{version}-reference-docs.tar.gz"
            if not (directory / docs_asset).is_file() or not (
                directory / docs_asset
            ).stat().st_size:
                missing = [docs_asset]
        available = result == "success" and not missing
        if expected and not available and (target == "build-docs" or not patch):
            raise ValueError(f"missing required channel outputs: {target}")
        states[target] = {
            "state": "not-produced"
            if not expected
            else "available"
            if available
            else "unavailable",
            "result": result,
        }
        if missing:
            states[target]["missing"] = missing
        if target == "build-docs":
            states[target]["asset"] = docs_asset
    required = [f"hew-lang-{name}-{version}.tgz" for name in ("wasm", "sandbox-vm")]
    for name in required:
        if not (directory / name).is_file() or not (directory / name).stat().st_size:
            raise ValueError(f"missing browser release asset: {name}")
    wasi = f"hew-v{version}-wasm32-wasip1-libs.tar.gz"
    states["wasm32-wasip1"] = {
        "state": "available" if (directory / wasi).is_file() else "not-produced",
        "result": jobs.get("build-portable-release-libs", {}).get("result", "missing"),
        "policy": "standalone WASI is non-gating",
    }
    receipt = {
        "version": version,
        "source": source,
        "run": run,
        "notes": deferrals,
        "platforms": states,
        "jobs": {name: job.get("result", "missing") for name, job in jobs.items()},
    }
    receipt_path = directory / f"hew-v{version}-platforms.json"
    receipt_path.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n")
    checksum_path = directory / f"hew-v{version}-checksums.txt"
    lines = []
    for path in sorted(directory.iterdir()):
        if path == checksum_path:
            continue
        if not path.is_file():
            raise ValueError(f"unexpected release entry: {path.name}")
        with path.open("rb") as payload:
            lines.append(
                f"{hashlib.file_digest(payload, 'sha256').hexdigest()}  {path.name}\n"
            )
    checksum_path.write_text("".join(lines))
    return receipt


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--directory", default="dist")
    parser.add_argument("--version", required=True)
    parser.add_argument("--source", required=True)
    parser.add_argument("--run", required=True)
    parser.add_argument("--jobs", required=True)
    parser.add_argument("--deferrals", default="{}")
    args = parser.parse_args()
    try:
        receipt = assemble(
            args.directory,
            args.version,
            args.source,
            args.run,
            json.loads(args.jobs),
            json.loads(args.deferrals),
        )
        print(json.dumps(receipt, indent=2))
        if output := os.environ.get("GITHUB_OUTPUT"):
            with Path(output).open("a") as stream:
                stream.write(
                    f"homebrew={str(receipt['platforms']['darwin-aarch64']['state'] == 'available').lower()}\n"
                )
    except (ValueError, OSError) as error:
        parser.exit(1, f"release-assets: {error}\n")
