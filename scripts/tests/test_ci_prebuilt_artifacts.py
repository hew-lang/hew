#!/usr/bin/env python3
"""Contracts for the build-once Linux test archive.

One job builds the Rust test surface; four shards consume it. The properties
worth holding are the ones whose absence is invisible until a wrong answer
ships:

  * the consumers are DAG-gated on the producer, so a producer failure is a
    red required check and never four slow rebuilds;
  * the archive carries the shared Hew outputs a test helper resolves by path,
    each declared `on-missing = "error"`, and carries none of Cargo's build
    state;
  * the archive is materialized at the producer's own target path and Cargo
    is given a different, writable one;
  * the two spellings of every package selection -- Cargo flags locally,
    a filterset in reuse mode, because cargo-nextest accepts exactly one of
    them at a time -- denote the same set;
  * the Makefile verifies in prebuilt mode and builds everywhere else, and a
    half-supplied environment is an error rather than a silent rebuild.

Every assertion is followed by the mutation it would otherwise accept.
"""

from __future__ import annotations

import hashlib
import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CI_WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
NEXTEST_CONFIG = ROOT / ".config" / "nextest.toml"
MAKEFILE = ROOT / "Makefile"
DISPATCHER = ROOT / "scripts" / "ci-preflight-dispatcher.sh"

PRODUCER_JOB = "linux-nextest-archive"
CONSUMER_JOB = "build-and-test"

# The four reuse flags. cargo-nextest 0.9.120 is the pinned build; its
# `ReuseBuildOpts` carries all four and places `--binaries-metadata` in a clap
# group that conflicts with every Cargo package/target flag, which is why the
# filterset spelling below exists at all.
REUSE_FLAGS = (
    "--binaries-metadata",
    "--cargo-metadata",
    "--workspace-remap",
    "--target-dir-remap",
)


# ── helpers ─────────────────────────────────────────────────────────────────


def jobs(text: str) -> dict[str, str]:
    """Top-level job blocks, by name."""
    body = text[text.index("\njobs:\n") + len("\njobs:\n") :]
    starts = list(re.finditer(r"^  (?P<name>[A-Za-z0-9_-]+):\s*$", body, re.MULTILINE))
    found = {}
    for index, start in enumerate(starts):
        end = starts[index + 1].start() if index + 1 < len(starts) else len(body)
        found[start.group("name")] = body[start.start() : end]
    return found


def steps(job: str) -> list[str]:
    starts = list(re.finditer(r"^      - ", job, re.MULTILINE))
    return [
        job[
            start.start() : (starts[i + 1].start() if i + 1 < len(starts) else len(job))
        ]
        for i, start in enumerate(starts)
    ]


def step_named(job: str, fragment: str) -> str:
    for step in steps(job):
        if fragment in step:
            return step
    raise AssertionError(f"no step matching {fragment!r}")


def step_index(job: str, fragment: str) -> int:
    for index, step in enumerate(steps(job)):
        if fragment in step:
            return index
    raise AssertionError(f"no step matching {fragment!r}")


def archive_includes(config: str) -> list[dict[str, str]]:
    """The `[profile.ci] archive.include` entries, as flat key/value maps.

    Hand-parsed for the same reason check-gate-reachability.py hand-parses
    YAML: CI installs no Python packages and the baseline is 3.10, which has
    no TOML reader for a file this one is (tomllib arrived in 3.11).
    """
    marker = "archive.include = ["
    if marker not in config:
        return []
    body = config[config.index(marker) + len(marker) :]
    body = body[: body.index("\n]")]
    entries = []
    for row in re.findall(r"\{([^}]*)\}", body):
        entry = {}
        for key, value in re.findall(r'([a-z-]+)\s*=\s*"([^"]*)"', row):
            entry[key] = value
        entries.append(entry)
    return entries


def make_dry_run(target: str, env: dict[str, str], cwd: Path = ROOT):
    run_env = os.environ.copy()
    for key in tuple(run_env):
        if key.startswith("HEW_CI_") or key == "CARGO_TARGET_DIR":
            run_env.pop(key)
    run_env.update(env)
    return subprocess.run(
        ["make", "-n", target],
        cwd=cwd,
        capture_output=True,
        text=True,
        env=run_env,
    )


def prebuilt_fixture(work: Path) -> dict[str, str]:
    """A complete extracted-archive tree, the shape a consumer materializes."""
    target = work / "target"
    cargo_target = work / "cargo-target"
    cargo_target.mkdir(parents=True)
    (target / "debug").mkdir(parents=True)
    (target / "release-lib").mkdir(parents=True)
    (target / "wasm32-wasip1" / "debug").mkdir(parents=True)
    (target / "nextest").mkdir(parents=True)
    for relative in (
        "debug/hew",
        "debug/libhew.a",
        "debug/libhew_runtime.a",
        "debug/.hew-libhew-freshness-v1",
        "release-lib/hew",
        "release-lib/libhew.a",
        "wasm32-wasip1/debug/libhew_runtime.a",
        "wasm32-wasip1/debug/libhew_std.a",
    ):
        (target / relative).write_text("fixture\n", encoding="utf-8")
    for relative in ("debug/hew", "release-lib/hew"):
        (target / relative).chmod(0o755)
    (target / "nextest" / "binaries-metadata.json").write_text("{}", encoding="utf-8")
    (target / "nextest" / "cargo-metadata.json").write_text("{}", encoding="utf-8")
    return {
        "CARGO_TARGET_DIR": str(cargo_target),
        "HEW_CI_PREBUILT_TEST_ARTIFACTS": "1",
        "HEW_CI_NEXTEST_BINARIES_METADATA": str(
            target / "nextest" / "binaries-metadata.json"
        ),
        "HEW_CI_NEXTEST_CARGO_METADATA": str(
            target / "nextest" / "cargo-metadata.json"
        ),
        "HEW_CI_NEXTEST_TARGET_DIR": str(target),
    }


# ── workflow contract ───────────────────────────────────────────────────────


def assert_producer_gates_the_shards(text: str) -> None:
    workflow = jobs(text)
    assert PRODUCER_JOB in workflow, "no Linux test archive producer job"
    producer = workflow[PRODUCER_JOB]
    consumer = workflow[CONSUMER_JOB]

    header = producer[: producer.index("    steps:\n")]
    assert re.search(r"^    if:", header, re.MULTILINE) is None, (
        "the producer carries a job-level `if:`; a skipped job is not a "
        "satisfied dependency and would strand every shard"
    )
    assert "No code changes detected" in producer, (
        "the producer has no docs-only no-op step"
    )

    consumer_header = consumer[: consumer.index("    steps:\n")]
    needs = re.search(r"^    needs: (.+)$", consumer_header, re.MULTILINE)
    assert needs, "the shard matrix declares no `needs:`"
    assert PRODUCER_JOB in needs.group(1), (
        "the shards do not depend on the producer; a missing archive would "
        "look like an ordinary cold start"
    )


def assert_transport_is_fail_closed(text: str) -> None:
    workflow = jobs(text)
    upload = step_named(workflow[PRODUCER_JOB], "actions/upload-artifact@")
    assert "if-no-files-found: error" in upload, (
        "the producer would upload nothing and report success"
    )
    assert "retention-days: 1" in upload, "the archive is a transport, not a cache"

    # Every consumer of the archive, not just the shard matrix: the bundle
    # packager reads the same bytes and must reject the same corruption.
    consumers = [
        (name, step)
        for name, job in workflow.items()
        for step in steps(job)
        if "actions/download-artifact@" in step and "ci-linux-nextest-" in step
    ]
    assert len(consumers) >= 2, (
        f"expected the shard matrix and the bundle packager to consume the "
        f"archive, found {len(consumers)}"
    )
    for name, step in consumers:
        assert "digest-mismatch: error" in step, (
            f"{name}: a corrupted archive would surface as an inexplicable "
            "failure hours from the download that caused it"
        )
        assert "run-id:" not in step and "repository:" not in step, (
            f"{name}: the transport must stay scoped to this run of this repository"
        )


def assert_materialization_precedes_every_gate(text: str) -> None:
    consumer = jobs(text)[CONSUMER_JOB]
    materialize = step_index(consumer, "Materialize the Linux test archive")
    for later in (
        "Verify the shared Rust test artefacts",
        "Run change-scoped tests",
    ):
        assert materialize < step_index(consumer, later), (
            f"{later!r} runs before the archive is materialized"
        )

    step = step_named(consumer, "Materialize the Linux test archive")
    assert "--extract-to" in step and "--workspace-remap" in step
    for exported in (
        "HEW_CI_PREBUILT_TEST_ARTIFACTS=1",
        "HEW_CI_NEXTEST_BINARIES_METADATA=",
        "HEW_CI_NEXTEST_CARGO_METADATA=",
        "HEW_CI_NEXTEST_TARGET_DIR=",
    ):
        assert exported in step, f"{exported} is never exported to the gates"


def assert_materialization_extracts_at_the_same_root_it_remaps(text: str) -> None:
    """Same-root extraction, not a staging directory and a later `mv`.

    `cargo nextest list` recreates `$GITHUB_WORKSPACE/target` while it
    resolves the workspace, before any later `mv` in the step body would run,
    so a staging directory plus `mv "$staging/target" "$target"` finds a
    directory already at the destination and nests one level down
    (`target/target/...`) instead of replacing it. Every path the step checks
    next then reads nothing, and every bare `test` under `set -e` exits
    silently -- the defect this asserts against. Extracting straight into the
    same root `--workspace-remap` names, with `--extract-overwrite` for a
    rerun, is the one shape with no second path for the tree to land at.
    """
    step = step_named(jobs(text)[CONSUMER_JOB], "Materialize the Linux test archive")
    body = "\n".join(
        line for line in step.splitlines() if not line.strip().startswith("#")
    )
    assert '--extract-to "$GITHUB_WORKSPACE"' in body, (
        "the archive is not extracted straight into $GITHUB_WORKSPACE"
    )
    assert '--workspace-remap "$GITHUB_WORKSPACE"' in body, (
        "the workspace remap no longer names $GITHUB_WORKSPACE"
    )
    assert "--extract-overwrite" in body, (
        "a rerun of this step would fail on the leftover tree from the last one"
    )
    for reintroduced in ("staging", 'mv "', "rmdir "):
        assert reintroduced not in body, (
            f"the staging-directory-then-{reintroduced!r} topology returned; "
            "cargo nextest list recreates $GITHUB_WORKSPACE/target before any "
            "later mv runs, and POSIX mv nests the tree instead of replacing it"
        )
    assert "::error::the Linux test archive is missing" in body, (
        "a missing archive member reports nothing actionable"
    )
    for path in REQUIRED_ARCHIVE_PATHS:
        assert f'"$target/{path}' in body, (
            f"the diagnostic loop no longer checks {path}"
        )


def assert_the_two_target_authorities_are_separate(text: str) -> None:
    """The archive lands on the producer's path; Cargo writes somewhere else.

    An archived binary carries absolute paths nothing remaps -- a baked
    `env!("CARGO_BIN_EXE_hew")`, a `current_exe()`-relative walk -- so any
    other path names a directory no consumer has: ~1069 failures on 33028214259.
    """
    consumer = jobs(text)[CONSUMER_JOB]
    body = step_named(consumer, "Materialize the Linux test archive")
    assert 'target="$GITHUB_WORKSPACE/target"' in body, (
        "the archive is not materialized at the producer's own target path"
    )
    assert 'test "${CARGO_TARGET_DIR:-}" = "$RUNNER_TEMP/ci-cargo-target"' in body, (
        "the materialize step does not assert Cargo writes elsewhere"
    )
    assert "cache-targets: 'false'" in step_named(
        consumer, "./.github/actions/setup-rust-build"
    ), "rust-cache still owns `<workspace>/target`, the archive's own path"


def assert_no_target_directory_transfer(text: str) -> None:
    """The archive is a runnable surface, not a copy of Cargo's build state."""
    producer = jobs(text)[PRODUCER_JOB]
    upload = step_named(producer, "actions/upload-artifact@")
    path = re.search(r"^\s+path: (.+)$", upload, re.MULTILINE)
    assert path, "the upload declares no path"
    assert path.group(1).strip().endswith("ci-linux-nextest.tar.zst"), (
        f"the producer uploads {path.group(1).strip()!r}, not a single archive"
    )


def assert_the_summary_reports_the_cost(text: str) -> None:
    workflow = jobs(text)
    producer = step_named(workflow[PRODUCER_JOB], "Report the archive cost")
    for token in (
        "- shared artefact build:",
        "- archive creation:",
        "- archive size:",
    ):
        assert token in producer, f"the producer never reports {token!r}"
    consumer = step_named(workflow[CONSUMER_JOB], "Materialize the Linux test archive")
    for token in ("- archive size:", "- download:", "- extract:"):
        assert token in consumer, f"the consumer never reports {token!r}"
    assert "GITHUB_STEP_SUMMARY" in consumer
    verify = step_named(workflow[CONSUMER_JOB], "Verify the shared Rust test artefacts")
    assert "- shared artefact verify:" in verify, (
        "the first use of the prebuilt tree reports no elapsed time"
    )


def test_the_workflow_builds_once_and_fails_closed() -> None:
    text = CI_WORKFLOW.read_text(encoding="utf-8")
    assert_producer_gates_the_shards(text)
    assert_transport_is_fail_closed(text)
    assert_materialization_precedes_every_gate(text)
    assert_materialization_extracts_at_the_same_root_it_remaps(text)
    assert_the_two_target_authorities_are_separate(text)
    assert_no_target_directory_transfer(text)
    assert_the_summary_reports_the_cost(text)


def _move_step_to_end(text: str, fragment: str) -> str:
    """Relocate one step to the end of its job, keeping the file otherwise intact."""
    consumer = jobs(text)[CONSUMER_JOB]
    block = step_named(consumer, fragment)
    moved = consumer.replace(block, "") + block
    return text.replace(consumer, moved)


def test_each_workflow_property_rejects_its_own_defect() -> None:
    text = CI_WORKFLOW.read_text(encoding="utf-8")

    def rejects(mutated: str, check) -> None:
        try:
            check(mutated)
        except AssertionError:
            return
        raise AssertionError(f"{check.__name__} accepted its own defect")

    rejects(
        text.replace(
            f"    needs: [changes, ast-grep-toolchain, {PRODUCER_JOB}]\n"
            "    runs-on: ubuntu-24.04",
            "    needs: changes\n    runs-on: ubuntu-24.04",
        ),
        assert_producer_gates_the_shards,
    )
    rejects(
        text.replace("          digest-mismatch: error\n", ""),
        assert_transport_is_fail_closed,
    )
    rejects(
        text.replace(
            "          compression-level: 0\n"
            "          retention-days: 1\n"
            "          if-no-files-found: error\n",
            "          compression-level: 0\n          retention-days: 1\n",
        ),
        assert_transport_is_fail_closed,
    )
    rejects(
        text.replace('            echo "HEW_CI_NEXTEST_TARGET_DIR=$target"\n', ""),
        assert_materialization_precedes_every_gate,
    )
    rejects(
        _move_step_to_end(text, "Materialize the Linux test archive"),
        assert_materialization_precedes_every_gate,
    )
    rejects(
        text.replace(
            "path: ${{ runner.temp }}/ci-linux-nextest.tar.zst",
            "path: target/",
        ),
        assert_no_target_directory_transfer,
    )
    rejects(
        text.replace('echo "- extract: \\`${extract_seconds}s\\`"', "true"),
        assert_the_summary_reports_the_cost,
    )
    rejects(
        text.replace(
            '            --extract-to "$GITHUB_WORKSPACE" \\\n'
            "            --extract-overwrite \\\n",
            '            --extract-to "$RUNNER_TEMP/ci-linux-nextest-staging" \\\n',
        ),
        assert_materialization_extracts_at_the_same_root_it_remaps,
    )
    rejects(
        text.replace("            --extract-overwrite \\\n", ""),
        assert_materialization_extracts_at_the_same_root_it_remaps,
    )
    rejects(
        text.replace(
            '          rm -rf "$target"\n',
            '          staging="$RUNNER_TEMP/ci-linux-nextest-staging"\n'
            '          rm -rf "$target" "$staging"\n'
            '          mkdir -p "$staging"\n',
        ),
        assert_materialization_extracts_at_the_same_root_it_remaps,
    )
    rejects(
        text.replace(
            '            echo "::error::the Linux test archive is missing $kind at $path"\n',
            "",
        ),
        assert_materialization_extracts_at_the_same_root_it_remaps,
    )
    rejects(
        text.replace('            "$target/debug/libhew_runtime.a:f" \\\n', ""),
        assert_materialization_extracts_at_the_same_root_it_remaps,
    )


# ── archive contents ────────────────────────────────────────────────────────

# Every shared output a Hew Make target produces and a test helper resolves by
# path. Present on every Linux runner, so a missing one is a producer defect
# rather than a property of the host.
REQUIRED_ARCHIVE_PATHS = {
    "debug/hew",
    "debug/libhew.a",
    "debug/libhew_runtime.a",
    "debug/.hew-libhew-freshness-v1",
    "release-lib/hew",
    "release-lib/libhew.a",
    "wasm32-wasip1/debug/libhew_runtime.a",
    "wasm32-wasip1/debug/libhew_std.a",
}

FORBIDDEN_ARCHIVE_FRAGMENTS = ("deps", ".fingerprint", "incremental", "build")


def assert_the_archive_carries_the_consumer_interface(config: str) -> None:
    entries = archive_includes(config)
    assert entries, "profile.ci declares no archive.include"
    by_path = {entry.get("path", ""): entry for entry in entries}

    missing = REQUIRED_ARCHIVE_PATHS - set(by_path)
    assert not missing, f"the archive omits shared test artefacts: {sorted(missing)}"

    for path in REQUIRED_ARCHIVE_PATHS:
        entry = by_path[path]
        assert entry.get("relative-to") == "target", (
            f"{path} is not resolved against the target directory"
        )
        assert entry.get("on-missing") == "error", (
            f"{path} is `{entry.get('on-missing')}`; a silently absent archive "
            "fails at the linker inside a timed gate instead of at the "
            "producer that did not build it"
        )

    for path in by_path:
        segments = path.split("/")
        assert path not in ("target", "."), "the archive must not recurse over target/"
        for forbidden in FORBIDDEN_ARCHIVE_FRAGMENTS:
            assert forbidden not in segments, (
                f"{path} carries Cargo build state ({forbidden}); the archive "
                "is a runnable surface, not a target-directory transfer"
            )
        assert "depth" not in by_path[path], (
            f"{path} declares a recursion depth; every entry here is one file"
        )


def assert_the_archive_is_not_cargos_output_directory(text: str) -> None:
    """Cargo must not be pointed at a tree whose contents are certified.

    The archive's `libhew.a` carries a freshness certificate binding it to the
    sources that produced it, and its `hew` is the compiler the producer built.
    Exporting the extracted tree as CARGO_TARGET_DIR made every ordinary cargo
    invocation in the job a writer to it, and one did so deterministically:
    `forced-cancel-composite-check-build` builds a
    `hew-runtime/forced-cancel-test` compiler and archive in WARM-UP, before a
    single gate runs.
    """
    step = step_named(jobs(text)[CONSUMER_JOB], "Materialize the Linux test archive")
    # Read the commands, not the commentary: a rule satisfied by a comment
    # that mentions the command is a rule that checks nothing.
    body = "\n".join(
        line for line in step.splitlines() if not line.strip().startswith("#")
    )
    assert "CARGO_TARGET_DIR=" not in body, (
        "the extracted archive is exported as Cargo's output directory again; "
        "any feature-specific build in the job would overwrite certified "
        "artefacts"
    )
    assert 'chmod -R a-w "$target"' in body, (
        "the extracted tree stays writable; the separation would be trusted "
        "rather than enforced"
    )
    widen = " ".join(re.findall(r"chmod [^\n]*\+w (.*)", body))
    leaves = ["/nextest/ci", "/nextest/ci-cabi", "/forced-cancel-gate"]
    assert re.findall(r'"\$target(/[^"]*)?"', widen) == leaves, (
        f"exactly these leaves may be thawed, once: {widen}"
    )
    for exported in ("HEW_CI_NEXTEST_TARGET_DIR=", "HEW_CI_PREBUILT_TEST_ARTIFACTS=1"):
        assert exported in body, f"{exported} is never exported to the gates"


def test_the_archive_is_immutable_to_the_job_that_reads_it() -> None:
    assert_the_archive_is_not_cargos_output_directory(
        CI_WORKFLOW.read_text(encoding="utf-8")
    )


def test_pointing_cargo_at_the_archive_is_rejected() -> None:
    text = CI_WORKFLOW.read_text(encoding="utf-8")
    for mutation in (
        text.replace(
            '            echo "HEW_CI_PREBUILT_TEST_ARTIFACTS=1"',
            '            echo "CARGO_TARGET_DIR=$target"\n'
            '            echo "HEW_CI_PREBUILT_TEST_ARTIFACTS=1"',
        ),
        text.replace('          chmod -R a-w "$target"\n', ""),
        text.replace("chmod u+w ", 'chmod u+w "$target" '),
        text.replace('.probe"\n', '.probe"\n          chmod u+w "$target"\n', 1),
    ):
        assert mutation != text, "the mutation matched nothing; the test is vacuous"
        try:
            assert_the_archive_is_not_cargos_output_directory(mutation)
        except AssertionError:
            continue
        raise AssertionError("a writable archive under Cargo's output was accepted")


PROJECT = ROOT / "scripts" / "ci-project-shared-artifacts.sh"

# Reordered keys, an omitted policy, and a path holding a space and the
# character a delimiter-based reader would split on.
MANIFEST = """
[profile.ci]
archive.include = [
  { path = "debug/hew", relative-to = "target", on-missing = "error" },
  { on-missing = "error", relative-to = "target", path = "debug/odd | name.a" },
  { path = "debug/plain.a", relative-to = "target" },
  { path = "cross/libhew.a", relative-to = "target", on-missing = "ignore" },
]
"""
REQUIRED = ["debug/hew", "debug/odd | name.a", "debug/plain.a"]
OPTIONAL = "cross/libhew.a"


def test_the_projection_honours_on_missing_and_always_verifies_its_gate() -> None:
    """Absence is a policy read from real TOML, and verify cannot be skipped."""

    def fixture(work: Path, omit: str = "", manifest: str = MANIFEST) -> Path:
        (work / "scripts").mkdir(parents=True)
        (work / "scripts" / PROJECT.name).symlink_to(PROJECT)
        (work / "scripts" / "lib").symlink_to(ROOT / "scripts" / "lib")
        (work / ".config").mkdir()
        (work / ".config" / "nextest.toml").write_text(manifest)
        for source in (work / "art" / p for p in REQUIRED if p != omit):
            source.parent.mkdir(parents=True, exist_ok=True)
            source.write_text("certified")
        (work / "cargo" / OPTIONAL).parent.mkdir(parents=True)
        return work

    def run(root: Path, *argv: str) -> subprocess.CompletedProcess[str]:
        roots = (str(root / "art"), str(root / "cargo"))
        cmd = [str(root / "scripts" / PROJECT.name), argv[0], *roots, *argv[1:]]
        return subprocess.run(cmd, capture_output=True, text=True)

    with tempfile.TemporaryDirectory() as raw:
        # Absent optional, no destination: green; and the awkward path
        # survived the reader exactly, as a link rather than a copy.
        good = fixture(Path(raw) / "good")
        assert run(good, "link").returncode == 0, run(good, "link").stderr
        assert run(good, "verify").returncode == 0
        odd = good / "cargo" / REQUIRED[1]
        assert odd.is_symlink() and os.readlink(odd) == str(good / "art" / REQUIRED[1])

        # An omitted `on-missing` defaults to error, so that entry is required.
        gone = fixture(Path(raw) / "gone", omit="debug/plain.a")
        for verb in ("link", "verify"):
            assert run(gone, verb).returncode, f"a missing required source {verb}ed"

        # Absent by policy means absent on both sides.
        spare = fixture(Path(raw) / "spare")
        stray = spare / "cargo" / OPTIONAL
        for plant in (
            lambda: stray.write_text("cargo"),
            lambda: stray.symlink_to(spare / "art" / OPTIONAL),
        ):
            plant()
            for verb in ("link", "verify"):
                assert run(spare, verb).returncode, f"a stray {verb}ed green"
            stray.unlink()

        # An entry anchored elsewhere, or resolving outside `target/`, is a
        # manifest error before a single source is touched.
        anchored = '{{ path = "{}", relative-to = "target" }}'
        outside = ("/etc/shadow", "//etc/shadow", "../etc", "a/../../b", ".")
        for n, bad in enumerate(
            ['{ path = "debug/hew" }', '{ path = "debug/hew", relative-to = "x" }']
            + [anchored.format(p) for p in outside]
        ):
            bent = Path(raw) / f"bent{n}"
            fixture(bent, manifest=f"[profile.ci]\narchive.include = [{bad}]\n")
            for verb in ("link", "verify"):
                # The inventory diagnostic, not merely non-zero: a bad path is
                # also an absent source, which fails for the wrong reason.
                assert "inventory" in run(bent, verb).stderr, f"{bad} was accepted"

        # Status precedence, and a verifier that runs however the gate ended.
        drop = f"rm '{good / 'cargo' / REQUIRED[0]}'"
        for command, code in (("true", 0), ("exit 3", 3), (drop, 1)):
            assert run(good, "gate", command).returncode == code, command
            run(good, "link")
        both = run(good, "gate", f"{drop}; exit 3")
        assert both.returncode == 3, both.stderr
        assert "is gone; the projection" in both.stderr, (
            f"the verifier did not run after a failing gate: {both.stderr!r}"
        )


def test_the_makefile_reads_the_archive_and_writes_somewhere_else() -> None:
    """One path-authority split, not a list of patched targets.

    ARTIFACT_* is where shared artefacts are READ from; CARGO_* is where Cargo
    WRITES. Locally they are the same directory. In prebuilt mode they are
    not, and that is what makes the clobber class unreachable rather than one
    command's problem.
    """
    makefile = MAKEFILE.read_text(encoding="utf-8")
    assert "ARTIFACT_ROOT := $(HEW_CI_NEXTEST_TARGET_DIR)" in makefile
    assert "ARTIFACT_ROOT := $(CARGO_TARGET_ROOT)" in makefile
    for derived in (
        "DEBUG_DIR  := $(ARTIFACT_NATIVE_OUT)/debug",
        "RELEASE_LIB_DIR := $(ARTIFACT_NATIVE_OUT)/release-lib",
        "WASM_DEBUG_DIR  := $(ARTIFACT_ROOT)/wasm32-wasip1/debug",
    ):
        assert derived in makefile, f"{derived} no longer follows the artefact root"

    with tempfile.TemporaryDirectory() as raw:
        env = prebuilt_fixture(Path(raw))
        archive = env["HEW_CI_NEXTEST_TARGET_DIR"]
        # Every gate that still compiles must compile somewhere else.
        for target in ("test-cabi", "test-runtime-unit", "stdlib", "test"):
            plan = make_dry_run(target, env)
            assert plan.returncode == 0, plan.stderr
            for line in plan.stdout.replace("\\\n", " ").splitlines():
                if not re.search(r"\bcargo\s+(build|run|test|nextest)\b", line):
                    continue
                if "--binaries-metadata" in line or "--target-dir-remap" in line:
                    continue  # reuse metadata is a READ of the archive
                assert f"CARGO_TARGET_DIR={archive}" not in line, (
                    f"make {target} points Cargo at the archive:\n{line}"
                )

        # The one command that must carry its own directory, because its gate
        # does: a forced-cancel-test compiler is not the ordinary one.
        forced = make_dry_run("forced-cancel-composite-check-build", env)
        assert forced.returncode == 0, forced.stderr
        assert "forced-cancel-gate" in forced.stdout, forced.stdout
        assert archive not in forced.stdout, forced.stdout


def test_a_sibling_build_cannot_alter_the_archive() -> None:
    """Attempt the write the old arrangement allowed, and prove it cannot land.

    No self-heal, no repair, no "restored from backup" message: the write
    fails at the writer, the bytes are unchanged, and the prebuilt
    verification that consumers depend on still passes afterwards.
    """
    with tempfile.TemporaryDirectory() as raw:
        env = prebuilt_fixture(Path(raw))
        archive = Path(env["HEW_CI_NEXTEST_TARGET_DIR"])
        certified = {
            path: hashlib.sha256(path.read_bytes()).hexdigest()
            for path in (archive / "debug" / "libhew.a", archive / "debug" / "hew")
        }

        subprocess.run(["chmod", "-R", "a-w", str(archive)], check=True)
        try:
            for path in certified:
                clobber = subprocess.run(
                    ["bash", "-c", f'printf "forced-cancel-test" > {path}'],
                    capture_output=True,
                    text=True,
                )
                assert clobber.returncode != 0, (
                    f"{path.name} was overwritten; the archive is still writable"
                )
                assert (
                    "denied" in clobber.stderr.lower()
                    or "read-only" in clobber.stderr.lower()
                ), clobber.stderr

            for path, digest in certified.items():
                assert hashlib.sha256(path.read_bytes()).hexdigest() == digest, (
                    f"{path.name} changed despite the write being refused"
                )

            # A later consumer still sees what the producer certified.
            verify = make_dry_run("runtime", env)
            assert verify.returncode == 0, verify.stderr
            command = next(
                line
                for line in verify.stdout.replace("\\\n", " ").splitlines()
                if line.strip().startswith("test -s ")
            )
            ran = subprocess.run(
                ["bash", "-c", command], capture_output=True, text=True, cwd=ROOT
            )
            assert ran.returncode == 0, ran.stderr
        finally:
            subprocess.run(["chmod", "-R", "u+w", str(archive)], check=False)


def test_the_archive_declares_the_consumer_interface() -> None:
    assert_the_archive_carries_the_consumer_interface(
        NEXTEST_CONFIG.read_text(encoding="utf-8")
    )


def test_a_weakened_archive_entry_is_rejected() -> None:
    config = NEXTEST_CONFIG.read_text(encoding="utf-8")

    def without(path: str) -> str:
        """The config with one archive entry removed, whatever its indent."""
        kept = [
            line
            for line in config.splitlines(keepends=True)
            if f'path = "{path}"' not in line
        ]
        return "".join(kept)

    entry = '{ path = "debug/libhew.a", relative-to = "target", on-missing = "error" }'
    weakened = config.replace(entry, entry.replace('"error"', '"warn"'))
    assert weakened != config, "the mutation matched nothing; the test is vacuous"
    try:
        assert_the_archive_carries_the_consumer_interface(weakened)
    except AssertionError:
        pass
    else:
        raise AssertionError("a warn-on-missing shared archive was accepted")

    removed = without("debug/libhew.a")
    assert removed != config, "the mutation matched nothing; the test is vacuous"
    try:
        assert_the_archive_carries_the_consumer_interface(removed)
    except AssertionError:
        pass
    else:
        raise AssertionError("a missing shared archive entry was accepted")

    recursive = '{ path = "debug/deps", relative-to = "target", depth = "infinite" },'
    widened = config.replace("archive.include = [", f"archive.include = [\n{recursive}")
    assert widened != config, "the mutation matched nothing; the test is vacuous"
    try:
        assert_the_archive_carries_the_consumer_interface(widened)
    except AssertionError:
        pass
    else:
        raise AssertionError("a recursive deps/ include was accepted")


# ── selection equivalence ───────────────────────────────────────────────────


def selection_variables() -> dict[str, tuple[str, str]]:
    """(cargo spelling, filterset spelling) for every NEXTEST_SELECT_*."""
    text = MAKEFILE.read_text(encoding="utf-8")
    found: dict[str, list[str]] = {}
    for match in re.finditer(r"^(NEXTEST_SELECT_[A-Z_]+) := (.+)$", text, re.MULTILINE):
        found.setdefault(match.group(1), []).append(match.group(2).strip())
    pairs = {}
    for name, values in found.items():
        assert len(values) == 2, (
            f"{name} is defined {len(values)} time(s); it needs exactly two "
            "spellings, one per mode"
        )
        filterset = next(value for value in values if value.startswith("-E "))
        cargo = next(value for value in values if not value.startswith("-E "))
        pairs[name] = (cargo, filterset)
    return pairs


def test_the_two_package_spellings_denote_the_same_set() -> None:
    """cargo-nextest accepts one of them at a time; both must mean one thing.

    `--binaries-metadata` and every Cargo package/target flag are one clap
    group, so reuse mode cannot say `-p`. The translation is mechanical, and a
    mechanical translation that drifts silently narrows what CI runs.
    """
    pairs = selection_variables()
    assert pairs, "no NEXTEST_SELECT_* variables found; the rule is vacuous"

    for name, (cargo, filterset) in pairs.items():
        packages = set(re.findall(r"-p ([A-Za-z0-9_-]+)", cargo))
        binaries = set(re.findall(r"--test ([A-Za-z0-9_-]+)", cargo))
        excluded = set(re.findall(r"--exclude ([A-Za-z0-9_-]+)", cargo))

        selected = set(re.findall(r"(?<!not )package\(([A-Za-z0-9_-]+)\)", filterset))
        negated = set(re.findall(r"not package\(([A-Za-z0-9_-]+)\)", filterset))
        binary_ids = set(
            re.findall(r"binary_id\([A-Za-z0-9_-]+::([A-Za-z0-9_-]+)\)", filterset)
        )

        assert excluded == negated, (
            f"{name}: cargo excludes {sorted(excluded)}, the filterset negates "
            f"{sorted(negated)}"
        )
        if binaries:
            assert binary_ids == binaries, (
                f"{name}: cargo runs test targets {sorted(binaries)}, the "
                f"filterset selects {sorted(binary_ids)}"
            )
            assert not selected, (
                f"{name}: a per-binary selection must not also select whole "
                f"packages ({sorted(selected)})"
            )
        else:
            assert selected == packages, (
                f"{name}: cargo selects {sorted(packages)}, the filterset "
                f"selects {sorted(selected)}"
            )


def test_a_drifted_selection_is_rejected() -> None:
    """Falsifiability: drop a package from one spelling and only one."""
    pairs = selection_variables()
    cargo, filterset = pairs["NEXTEST_SELECT_PIPELINE"]
    dropped = filterset.replace(" + package(hew-pkg)", "")
    assert dropped != filterset, "the mutation matched nothing; the test is vacuous"
    packages = set(re.findall(r"-p ([A-Za-z0-9_-]+)", cargo))
    selected = set(re.findall(r"(?<!not )package\(([A-Za-z0-9_-]+)\)", dropped))
    assert packages != selected, "a narrowed filterset compared equal"


# ── Makefile interface ──────────────────────────────────────────────────────

FOUNDATIONAL_TARGETS = ("stdlib", "hew-profile-check")


def test_local_mode_still_builds_everything_it_did() -> None:
    result = make_dry_run("stdlib", {})
    assert result.returncode == 0, result.stderr
    assert "cargo build -p hew-lib" in result.stdout, result.stdout
    assert "cargo build -p hew-runtime" in result.stdout, result.stdout
    assert "cargo build -p hew-cli" in result.stdout, result.stdout
    assert "prebuilt test artefact missing" not in result.stdout


def test_prebuilt_mode_verifies_rather_than_rebuilds() -> None:
    with tempfile.TemporaryDirectory() as raw:
        env = prebuilt_fixture(Path(raw))
        for target in FOUNDATIONAL_TARGETS:
            result = make_dry_run(target, env)
            assert result.returncode == 0, result.stderr
            assert "cargo build" not in result.stdout, (
                f"prebuilt `make {target}` still invokes Cargo:\n{result.stdout}"
            )
            assert "prebuilt test artefact missing" in result.stdout, (
                f"prebuilt `make {target}` verifies nothing:\n{result.stdout}"
            )

        run = make_dry_run("test", env)
        assert run.returncode == 0, run.stderr
        for flag in REUSE_FLAGS:
            assert flag in run.stdout, f"prebuilt `make test` omits {flag}"
        assert "-E 'not package(hew-cabi)'" in run.stdout
        assert "--no-run" not in run.stdout, (
            "prebuilt `make test` still runs a compile pre-pass"
        )


def test_prebuilt_verification_actually_fails_on_a_missing_artefact() -> None:
    """The recipe is a verification, so it has to be run, not read.

    The command is taken from the Makefile's own expansion and executed
    directly rather than by re-entering `make`: that proves the exact line the
    recipe emits, and it cannot touch the repository's real target directory
    while proving it.
    """
    with tempfile.TemporaryDirectory() as raw:
        env = prebuilt_fixture(Path(raw))
        plan = make_dry_run("runtime", env)
        assert plan.returncode == 0, plan.stderr
        recipe = [
            line
            for line in plan.stdout.replace("\\\n", " ").splitlines()
            if line.strip().startswith("test -s ")
        ]
        assert len(recipe) == 1, f"expected one verification line:\n{plan.stdout}"
        command = recipe[0]

        present = subprocess.run(
            ["bash", "-c", command], capture_output=True, text=True, cwd=ROOT
        )
        assert present.returncode == 0, present.stderr

        Path(env["HEW_CI_NEXTEST_TARGET_DIR"], "debug", "libhew_runtime.a").unlink()
        absent = subprocess.run(
            ["bash", "-c", command], capture_output=True, text=True, cwd=ROOT
        )
        assert absent.returncode != 0, "a missing prebuilt artefact was accepted"
        assert "prebuilt test artefact missing" in absent.stderr, absent.stderr
        assert "linux-nextest-archive" in absent.stderr, (
            "the failure does not name the producer that owns the artefact"
        )


def test_a_half_supplied_prebuilt_environment_is_refused() -> None:
    with tempfile.TemporaryDirectory() as raw:
        complete = prebuilt_fixture(Path(raw))
        for dropped in (
            "HEW_CI_NEXTEST_BINARIES_METADATA",
            "HEW_CI_NEXTEST_CARGO_METADATA",
            "HEW_CI_NEXTEST_TARGET_DIR",
        ):
            partial = {key: value for key, value in complete.items() if key != dropped}
            result = make_dry_run("test", partial)
            assert result.returncode != 0, (
                f"prebuilt mode ran without {dropped}; a shard would have "
                "silently recompiled what the producer failed to supply"
            )
            assert dropped in result.stderr, result.stderr

        # A named file that does not exist is the same defect one layer down.
        broken = dict(complete)
        broken["HEW_CI_NEXTEST_BINARIES_METADATA"] = str(Path(raw) / "absent.json")
        result = make_dry_run("test", broken)
        assert result.returncode != 0, "a missing metadata file was accepted"


def test_an_unknown_prebuilt_flag_value_is_refused() -> None:
    result = make_dry_run("stdlib", {"HEW_CI_PREBUILT_TEST_ARTIFACTS": "yes"})
    assert result.returncode != 0, "an unrecognised prebuilt flag value was accepted"


# ── dispatcher interface ────────────────────────────────────────────────────


def dispatcher_dry_run(paths: list[str], env: dict[str, str]):
    run_env = os.environ.copy()
    for key in tuple(run_env):
        if key.startswith("HEW_CI_") or key.startswith("PREFLIGHT_"):
            run_env.pop(key)
    run_env.update(env)
    return subprocess.run(
        ["bash", str(DISPATCHER), "--dry-run", "--", *paths],
        cwd=ROOT,
        capture_output=True,
        text=True,
        env=run_env,
    )


def test_the_closure_command_reuses_the_archive() -> None:
    with tempfile.TemporaryDirectory() as raw:
        env = prebuilt_fixture(Path(raw))
        result = dispatcher_dry_run(["hew-lexer/src/lib.rs"], env)
        assert result.returncode == 0, result.stderr
        closure = next(
            line
            for line in result.stdout.splitlines()
            if "cargo nextest run" in line and "budget:" in line
        )
        for flag in REUSE_FLAGS:
            assert flag in closure, f"the closure command omits {flag}"
        assert " -p " not in closure, (
            "the closure command keeps Cargo package flags, which cargo-nextest "
            "rejects alongside reuse metadata"
        )
        assert "-E '" in closure and "package(hew-lexer)" in closure, closure
        assert "make stdlib" not in result.stdout, (
            "prebuilt mode still derives a compile warm-up"
        )


def test_the_closure_command_builds_normally_without_the_archive() -> None:
    result = dispatcher_dry_run(["hew-lexer/src/lib.rs"], {})
    assert result.returncode == 0, result.stderr
    assert "make stdlib" in result.stdout, "the normal warm-up disappeared"
    assert "--no-run" in result.stdout, "the normal compile pre-pass disappeared"
    for flag in REUSE_FLAGS:
        assert flag not in result.stdout, (
            f"{flag} leaked into a run with no prebuilt archive"
        )


def test_the_dispatcher_refuses_a_half_supplied_environment() -> None:
    with tempfile.TemporaryDirectory() as raw:
        complete = prebuilt_fixture(Path(raw))
        for dropped in (
            "HEW_CI_NEXTEST_BINARIES_METADATA",
            "HEW_CI_NEXTEST_CARGO_METADATA",
            "HEW_CI_NEXTEST_TARGET_DIR",
        ):
            partial = {key: value for key, value in complete.items() if key != dropped}
            result = dispatcher_dry_run(["hew-lexer/src/lib.rs"], partial)
            assert result.returncode != 0, f"the dispatcher ran without {dropped}"
            assert dropped in result.stderr, result.stderr

        without_flag = {
            key: value
            for key, value in complete.items()
            if key != "HEW_CI_PREBUILT_TEST_ARTIFACTS"
        }
        result = dispatcher_dry_run(["hew-lexer/src/lib.rs"], without_flag)
        assert result.returncode != 0, (
            "reuse metadata without the prebuilt flag was accepted"
        )


# ── pinned tool ─────────────────────────────────────────────────────────────


def test_the_pinned_nextest_understands_the_reuse_interface() -> None:
    """0.9.120 carries all four flags; prove it against the installed build.

    Skipped rather than assumed when cargo-nextest is absent: a contract that
    silently passes on a machine without the tool is not a contract. Every CI
    job that runs this suite installs the pin.
    """
    if shutil.which("cargo-nextest") is None and shutil.which("cargo") is None:
        print("SKIP cargo-nextest is not installed")
        return
    help_text = subprocess.run(
        ["cargo", "nextest", "run", "--help"],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    if help_text.returncode != 0:
        print("SKIP cargo-nextest is not installed")
        return
    for flag in (*REUSE_FLAGS, "--archive-file"):
        assert flag in help_text.stdout, f"the installed cargo-nextest has no {flag}"
    listing = subprocess.run(
        ["cargo", "nextest", "list", "--help"],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert listing.returncode == 0, listing.stderr
    for flag in ("--extract-to", "--extract-overwrite", "--archive-file"):
        assert flag in listing.stdout, f"the installed cargo-nextest has no {flag}"


def _discover_tests() -> list:
    return [
        value
        for name, value in sorted(globals().items())
        if name.startswith("test_") and callable(value)
    ]


if __name__ == "__main__":
    failures = 0
    discovered = _discover_tests()
    for test in discovered:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(discovered)} tests failed")
    print(f"All {len(discovered)} tests passed.")
