#!/usr/bin/env python3
"""Negative controls: changed proof inputs must never reuse cached green evidence."""

import json
from pathlib import Path
import tempfile
import coverage


def main():
    with tempfile.TemporaryDirectory(prefix="hew-evidence-controls-") as directory:
        coverage.ROOT = root = Path(directory)
        coverage.HERE = root / "proofs"
        coverage.TARGET = root / "proofs/target"
        originals = {
            "Cargo.toml": "[workspace]\n",
            "Cargo.lock": "version = 4\n",
            "rust-toolchain.toml": '[toolchain]\nchannel = "1.96.0"\n',
            "Makefile": "prove-runtime-checks:\n\ttrue\n",
            ".cargo/config.toml": "",
            "hew-runtime/build.rs": "fn main() {}\n",
            "hew-runtime/Cargo.toml": '[package]\nname = "runtime"\n',
            "hew-codegen-rs/build.rs": "fn main() {}\n",
            "hew-types/build_support/input.rs": "fn build_input() {}\n",
            "scripts/runtime-export-classification.toml": "[exports]\n",
            "std/builtins.hew": "type Marker {}\n",
            "examples/coroutine.hew": "fn main() {}\n",
            ".github/actions/setup-llvm/action.yml": "name: LLVM\n",
            "proofs/llvm_frames/corpus.json": '[{"source":"examples/coroutine.hew"}]',
            "hew-cabi/Cargo.toml": '[package]\nname = "cabi"\n',
            "hew-runtime/src/kernel.rs": "fn production() {}\n",
            "hew-cabi/src/value.rs": "pub struct Layout;\n",
            "proofs/coverage.py": "# reporting code\n",
            "proofs/check_evidence.py": "# controls\n",
            "proofs/coverage.json": "[]\n",
            "proofs/runtime_protocols/src/checks.rs": "fn proof() {}\n",
            "proofs/runtime_protocols/Cargo.lock": "version = 4\n",
        }
        for relative, text in originals.items():
            path = root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text)
        coverage.TARGET.mkdir(parents=True, exist_ok=True)
        assert coverage.evidence("runtime")["status"] == "not_run"
        stamp = coverage.fingerprint("runtime")
        result = coverage.TARGET / "runtime.json"
        result.write_text(json.dumps({"status": "passed", "fingerprint": stamp}))
        assert coverage.evidence("runtime")["status"] == "passed"
        # Edit actual inputs in this isolated checkout, leaving the old evidence
        # untouched. Each mutation must turn the green report stale.
        changed = [
            "hew-runtime/src/kernel.rs",
            "hew-cabi/src/value.rs",
            "proofs/runtime_protocols/src/checks.rs",
            "proofs/runtime_protocols/Cargo.lock",
            "hew-runtime/Cargo.toml",
            "Makefile",
            "rust-toolchain.toml",
        ]
        for relative in changed:
            path = root / relative
            path.write_text(originals[relative] + "\n# changed\n")
            assert coverage.evidence("runtime")["status"] == "stale", relative
            path.write_text(originals[relative])
            assert coverage.evidence("runtime")["status"] == "passed", relative
        (root / "proofs/README.md").write_text("Prose change only.\n")
        assert coverage.evidence("runtime")["status"] == "passed"
        result.write_text(json.dumps({"status": "failed", "fingerprint": stamp}))
        assert coverage.evidence("runtime")["status"] == "failed"
        llvm_stamp = coverage.fingerprint("llvm")
        llvm_result = coverage.TARGET / "llvm.json"
        llvm_result.write_text(
            json.dumps({"status": "passed", "fingerprint": llvm_stamp})
        )
        llvm_inputs = [
            "hew-codegen-rs/build.rs",
            "hew-types/build_support/input.rs",
            "scripts/runtime-export-classification.toml",
            "std/builtins.hew",
            "examples/coroutine.hew",
        ]
        for relative in llvm_inputs:
            path = root / relative
            path.write_text(originals[relative] + "\n# changed\n")
            assert coverage.evidence("llvm")["status"] == "stale", relative
            path.write_text(originals[relative])
            assert coverage.evidence("llvm")["status"] == "passed", relative
        print(
            "Evidence controls passed: missing/failed/stale results stay distinct; twelve runtime/compiler input mutations invalidate old green evidence."
        )


if __name__ == "__main__":
    main()
