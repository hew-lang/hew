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
OPTIONAL_JOBS = {"linux-packages", "docker-image", "build-docs"}
DEFERRABLE = set(NATIVE) - {"linux-x86_64", "linux-aarch64"} | OPTIONAL_JOBS


def assemble(directory, version, source, run, jobs, deferrals):
    if not re.fullmatch(r"[0-9]+\.[0-9]+\.[0-9]+(?:-[0-9A-Za-z.-]+)?", version):
        raise ValueError("invalid release version")
    if not re.fullmatch(r"[a-f0-9]{40}", source):
        raise ValueError("release source must be a full commit identity")
    patch = int(version.split("-", 1)[0].split(".")[2]) > 0
    if not isinstance(deferrals, dict) or set(deferrals) - DEFERRABLE:
        raise ValueError("unknown platform/channel deferral")
    for target, detail in deferrals.items():
        if (
            not patch
            or not isinstance(detail, dict)
            or set(detail) != {"owner", "reason", "evidence"}
        ):
            raise ValueError("patch deferrals require owner, reason and evidence")
        if any(
            not isinstance(value, str) or not value.strip() for value in detail.values()
        ):
            raise ValueError("deferral details must be nonempty text")
    for name in ("validate", "build-linux", "build-npm-packages"):
        if jobs.get(name, {}).get("result") != "success":
            raise ValueError(f"required producer did not succeed: {name}")
    directory = Path(directory)
    states = {}
    for target, suffix in NATIVE.items():
        name = f"hew-v{version}-{target}{suffix}"
        present = (directory / name).is_file() and (directory / name).stat().st_size > 0
        if not present and target not in deferrals:
            raise ValueError(
                f"missing qualified archive without a patch deferral: {name}"
            )
        states[target] = {
            "state": "available" if present else "deferred",
            "asset": name,
        }
        if target in deferrals:
            states[target].update(deferrals[target])
    for target in OPTIONAL_JOBS:
        result = jobs.get(target, {}).get("result", "missing")
        # Distro packages are not produced for prereleases.
        expected = target != "linux-packages" or "-" not in version
        if expected and result != "success" and target not in deferrals:
            raise ValueError(f"failed packaging needs a patch deferral: {target}")
        states[target] = {
            "state": "not-produced"
            if not expected
            else "available"
            if result == "success"
            else "deferred",
            "result": result,
        }
        if expected and result != "success":
            states[target].update(deferrals[target])
    required = [f"hew-lang-{name}-{version}.tgz" for name in ("wasm", "sandbox-vm")]
    for name in required:
        if not (directory / name).is_file() or not (directory / name).stat().st_size:
            raise ValueError(f"missing browser release asset: {name}")
    if jobs.get("linux-packages", {}).get("result") == "success" and "-" not in version:
        for pattern in ("hew_*.deb", "hew-*.rpm", "hew-bin-*.pkg.tar.zst"):
            if not any(path.stat().st_size for path in directory.glob(pattern)):
                raise ValueError(f"successful Linux packaging lacks {pattern}")
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
