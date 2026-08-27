#!/usr/bin/env python3
"""Behavioural probe for the shared-artefact projection.

A gate that COMPILES its own test binary resolves shared artefacts under
Cargo's target directory -- `hew-testutil` walks `<target>/<profile>/deps/`
upwards from `current_exe()` -- where the archive never put them. This drives
the real script over a real two-root tree in all three shapes hew-testutil
derives (`<profile>/x`, `wasm32-wasip1/<profile>/x`, `<triple>/<profile>/x`):
red before, green after, red again when Cargo replaces a link.
"""

import os
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci-project-shared-artifacts.sh"
INVENTORY = ROOT / "hew-testutil" / "shared-test-artifacts.tsv"
PROFILE = "debug"
CROSS = "aarch64-unknown-linux-gnu"


def _ok(name: str, test) -> bool:
    try:
        test()
    except AssertionError as exc:
        print(f"FAIL {name}: {exc}")
        return False
    print(f"PASS {name}")
    return True


def resolution_paths() -> list[str]:
    """Every path `verify_shared_test_artifacts` joins, per its own inventory.

    Derived from the table hew-testutil compiles in, so an artefact added to
    the verifier is an artefact this probe demands.
    """
    wanted = []
    for line in INVENTORY.read_text(encoding="utf-8").splitlines():
        fields = line.split("\t")
        if len(fields) != 7 or not fields[0][:1].islower():
            continue
        _key, kind, _package, unix_archive = fields[:4]
        if kind in ("host-bin", "host-staticlib"):
            wanted.append(f"{PROFILE}/{unix_archive}")
        elif kind == "wasm-staticlib":
            wanted.append(f"wasm32-wasip1/{PROFILE}/{unix_archive}")
        elif kind == "cross-staticlib":
            wanted.append(f"{CROSS}/{PROFILE}/{unix_archive}")
        else:  # pragma: no cover - a new kind must reach this probe
            raise AssertionError(f"unmodelled artefact kind {kind!r}")
    assert len(set(wanted)) >= 3, wanted
    return sorted(set(wanted))


def run(action: str, artefact: Path, cargo: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(SCRIPT), action, str(artefact), str(cargo)],
        capture_output=True,
        text=True,
        check=False,
    )


def certified_tree(root: Path, paths: list[str]) -> None:
    """Real files, frozen exactly as the workflow freezes them."""
    for relative in paths:
        artefact = root / relative
        artefact.parent.mkdir(parents=True, exist_ok=True)
        artefact.write_text(f"certified {relative}", encoding="utf-8")
    subprocess.run(["chmod", "-R", "a-w", str(root)], check=True)


def test_the_projection_makes_a_fresh_test_binary_resolve_the_certified_tree() -> None:
    paths = resolution_paths()
    with tempfile.TemporaryDirectory() as raw:
        work = Path(raw)
        artefact, cargo = work / "target", work / "ci-cargo-target"
        certified_tree(artefact, paths)
        cargo.mkdir()

        try:
            # RED BEFORE: the hosted failure. A binary compiled into Cargo's
            # authority resolves nothing.
            for relative in paths:
                assert not (cargo / relative).exists(), relative
            before = run("verify", artefact, cargo)
            assert before.returncode != 0, before.stdout + before.stderr

            # GREEN AFTER, in all three shapes.
            linked = run("link", artefact, cargo)
            assert linked.returncode == 0, linked.stdout + linked.stderr
            for relative in paths:
                projected = cargo / relative
                assert projected.is_symlink(), (
                    f"{relative} was copied, not projected; the shards would "
                    "carry a second copy of a two-gigabyte tree"
                )
                assert os.readlink(projected) == str(artefact / relative), relative
                assert projected.read_text(encoding="utf-8") == f"certified {relative}"

                # Projecting must not have thawed the certified original.
                assert not os.access(artefact / relative, os.W_OK), relative

            # Idempotent: warm-up and gate both call `link`.
            assert run("link", artefact, cargo).returncode == 0
            assert run("verify", artefact, cargo).returncode == 0

            # COUNTERFACTUAL: Cargo replaces a projected path with its own
            # build output, and the post-gate verify is what makes that red.
            victim = cargo / paths[0]
            victim.unlink()
            victim.write_text("a build nobody certified", encoding="utf-8")
            after = run("verify", artefact, cargo)
            assert after.returncode != 0, after.stdout + after.stderr
            assert paths[0] in after.stderr, after.stderr

            # And `link` refuses to paper over it rather than replacing a
            # real file with a link to something else.
            relink = run("link", artefact, cargo)
            assert relink.returncode != 0, relink.stdout + relink.stderr
        finally:
            subprocess.run(["chmod", "-R", "u+w", str(artefact)], check=False)


def test_the_projection_refuses_a_single_root() -> None:
    """A caller with one root has lost the split, and linking an artefact onto
    itself would be the corruption rather than the fix."""
    with tempfile.TemporaryDirectory() as raw:
        same = run("link", Path(raw), Path(raw))
        assert same.returncode != 0, same.stdout
        assert "nothing to project" in same.stderr, same.stderr


def test_the_gates_that_compile_project_before_and_verify_after() -> None:
    """The three gates, and only in the mode that has two directories."""
    makefile = (ROOT / "Makefile").read_text(encoding="utf-8")
    for gate in (
        "observe-functional-test",
        "libhew-link-race-test",
        "sandbox-parity",
    ):
        recipe = makefile.split(f"\n{gate}:", 1)[1].split("\n\n", 1)[0]
        assert "$(PROJECT_SHARED_ARTIFACTS)" in recipe, (
            f"{gate} compiles a fresh test binary without projecting the "
            f"certified artefacts it will resolve\n{recipe}"
        )
        assert recipe.rstrip().endswith("$(VERIFY_SHARED_ARTIFACTS)"), (
            f"{gate} does not re-verify after running; a Cargo replacement "
            f"would go unnoticed\n{recipe}"
        )

    # mqtt-broker-e2e runs the ARCHIVE's compiler by absolute path and
    # compiles no test binary, so it resolves nothing through this rule. Its
    # hosted failure is behavioural and must stay red on its own merits.
    mqtt = makefile.split("\nmqtt-broker-e2e:", 1)[1].split("\n\n", 1)[0]
    assert "SHARED_ARTIFACTS" not in mqtt, mqtt

    # Off everywhere else: a developer machine has one target directory.
    assert "PROJECT_SHARED_ARTIFACTS = @:" in makefile


if __name__ == "__main__":
    failures = [
        name
        for name, test in sorted(globals().items())
        if name.startswith("test_") and callable(test) and not _ok(name, test)
    ]
    if failures:
        raise SystemExit(f"{len(failures)} failed: {failures}")
