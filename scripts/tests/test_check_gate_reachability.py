"""Unit tests for scripts/check-gate-reachability.py.

The primary regression this guards is the defect the checker itself shipped
with: it read every workflow as RAW TEXT and never parsed jobs, steps or
triggers, so a MENTION counted as an EDGE. `make playground-wasi-check` was
reported "invoked directly by a CI workflow step" on the strength of a
release-gate.yml comment that said the gate was NOT wired yet. A comment, a
string echoed to the log, an `if: false` job, a disabled step, and a step in a
workflow nothing can trigger must all be non-edges; only a step that can
actually run and actually invokes the target counts.

The other two classes covered here are the filter parsers. `default-filter` is
a boolean expression, and the original `-\\s*(binary|package|test)\\(…\\)`
pattern silently skipped the leading `not package(hew-wasm)`, so a
five-exclusion filter was reported as "4/4 compensated". Inline `-E` filters
were matched only in single quotes, and any `--workspace` run counted as
compensation for a filtered one — including a `--workspace --exclude P` run
that does not execute P at all. Either hole lets the checker certify that
tests run when they run nowhere, which is the exact failure this file exists
to make impossible to reintroduce.
"""

import importlib.util
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "check-gate-reachability.py"

spec = importlib.util.spec_from_file_location("check_gate_reachability", SCRIPT)
gate = importlib.util.module_from_spec(spec)
assert spec.loader is not None
# Registered before execution because the module defines dataclasses, which
# look their own module up in sys.modules while the class body is executing.
sys.modules["check_gate_reachability"] = gate
spec.loader.exec_module(gate)

KNOWN = {"playground-wasi-check", "lint", "test", "miri"}


def load_workflow(text: str) -> "gate.Workflow":
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "synthetic.yml"
        path.write_text(text)
        return gate._load_workflow(path)


def edges(text: str) -> set[str]:
    """Gate targets a synthetic workflow actually invokes."""
    workflow = load_workflow(text)
    commands = gate.ci_step_commands([workflow])
    return gate.make_targets_in("\n".join(c for _, c in commands), KNOWN)


# ── Finding 1: structural parse ───────────────────────────────────────────────


def test_a_yaml_comment_naming_a_target_is_not_an_edge() -> None:
    # The literal shape that produced the false 54/54: a TODO saying the gate
    # is not wired, counted as the wiring.
    found = edges(
        """
name: synthetic
on:
  push:
jobs:
  gate:
    runs-on: ubuntu-24.04
    steps:
      # TODO(playground-wasi-gate): add `make playground-wasi-check` here once
      # the curated_playground_examples_run_under_wasi test is un-ignored.
      - name: Something else
        run: cargo build
"""
    )
    assert found == set(), f"a YAML comment must not be an edge, got {found}"


def test_a_shell_comment_inside_a_run_body_is_not_an_edge() -> None:
    found = edges(
        """
on: [push]
jobs:
  gate:
    runs-on: ubuntu-24.04
    steps:
      - run: |
          # make lint runs locally; CI does not run it here
          cargo build
"""
    )
    assert found == set(), f"a shell comment must not be an edge, got {found}"


def test_an_echoed_target_name_is_not_an_edge() -> None:
    found = edges(
        """
on: [push]
jobs:
  gate:
    runs-on: ubuntu-24.04
    steps:
      - run: echo "remember to run make lint before pushing"
"""
    )
    assert found == set(), f"echo prints, it does not invoke; got {found}"


def test_a_statically_false_job_is_not_an_edge() -> None:
    found = edges(
        """
on: [push]
jobs:
  gate:
    if: false
    runs-on: ubuntu-24.04
    steps:
      - run: make lint
"""
    )
    assert found == set(), f"an `if: false` job runs nothing, got {found}"


def test_a_statically_false_step_is_not_an_edge() -> None:
    found = edges(
        """
on: [push]
jobs:
  gate:
    runs-on: ubuntu-24.04
    steps:
      - run: make lint
        if: ${{ false }}
      - run: make test
"""
    )
    assert found == {"test"}, f"a disabled step must not count, got {found}"


def test_a_workflow_nothing_can_trigger_is_not_ci() -> None:
    called = load_workflow(
        """
on:
  workflow_call:
jobs:
  gate:
    runs-on: ubuntu-24.04
    steps:
      - run: make lint
"""
    )
    assert gate.triggerable([called]) == [], (
        "a workflow_call-only workflow that nothing calls never runs, so a "
        "gate invoked only there is not reached"
    )
    assert gate.ci_step_commands([called]) == []


def test_a_called_workflow_is_ci_when_a_live_workflow_calls_it() -> None:
    caller = load_workflow(
        """
on: [push]
jobs:
  delegate:
    uses: ./.github/workflows/synthetic.yml
"""
    )
    called = load_workflow(
        """
on:
  workflow_call:
jobs:
  gate:
    runs-on: ubuntu-24.04
    steps:
      - run: make lint
"""
    )
    # Synthetic files live outside .github/, so give the callee the path the
    # caller names.
    called.rel = ".github/workflows/synthetic.yml"
    live = {w.rel for w in gate.triggerable([caller, called])}
    assert called.rel in live, "a called workflow does run"


def test_an_unknown_trigger_fails_closed() -> None:
    try:
        load_workflow(
            """
on:
  invented_event:
jobs:
  gate:
    runs-on: ubuntu-24.04
    steps:
      - run: make lint
"""
        )
    except gate.YamlError:
        return
    raise AssertionError(
        "an unrecognised trigger must fail closed: guessing whether it fires "
        "decides whether every gate under it is reached"
    )


def test_yaml_subset_parser_rejects_what_it_cannot_model() -> None:
    for source, why in [
        ("a: &anchor 1\nb: *anchor\n", "anchors/aliases"),
        ("a: 1\n\tb: 2\n", "tabs"),
        ("a: 1\na: 2\n", "duplicate keys"),
        ("---\na: 1\n---\nb: 2\n", "multi-document streams"),
    ]:
        try:
            gate.parse_yaml(source, "synthetic")
        except gate.YamlError:
            continue
        raise AssertionError(f"parser must refuse {why} rather than mis-read them")


def test_the_real_release_gate_workflow_no_longer_claims_the_wasi_gate() -> None:
    # The stale TODO is gone, and nothing in any workflow names the target;
    # it is reached (if at all) by a containment proof, never by a mention.
    text = (ROOT / ".github" / "workflows" / "release-gate.yml").read_text()
    assert "playground-wasi-check" not in text


# ── Finding 2: the nextest filterset grammar ──────────────────────────────────


def test_leading_not_is_counted_as_an_exclusion() -> None:
    atoms = gate.filterset_exclusions(
        "not package(hew-wasm) - binary(parity) - binary(playground)"
    )
    rendered = {str(a) for a in atoms}
    assert rendered == {
        "package(hew-wasm)",
        "binary(parity)",
        "binary(playground)",
    }, rendered
    # Counterfactual for the pattern that shipped: it only saw terms after a
    # `-`, so it reported two of these three.
    old = re.findall(
        r"-\s*(binary|package|test)\(([^)]+)\)",
        "not package(hew-wasm) - binary(parity) - binary(playground)",
    )
    assert len(old) == 2 < len(atoms), (
        "the old regex must be shown to under-report; if it now agrees, this "
        "test no longer proves anything"
    )


def test_every_negation_spelling_is_counted() -> None:
    for text in [
        "not package(a) and not package(b)",
        "!package(a) & !package(b)",
        "all() - package(a) - package(b)",
        "not (package(a) or package(b))",
    ]:
        atoms = {str(a) for a in gate.filterset_exclusions(text)}
        assert atoms == {"package(a)", "package(b)"}, f"{text} -> {atoms}"


def test_a_filter_whose_subtracted_set_cannot_be_named_fails_closed() -> None:
    for text in ["package(a) or package(b)", "not package(a) or package(b)"]:
        try:
            gate.filterset_exclusions(text)
        except gate.FiltersetError:
            continue
        raise AssertionError(
            f"{text!r} does not reduce to a set of subtracted terms; reporting "
            "a partial exclusion list is how '4/4 compensated' happened"
        )


def test_the_real_profile_ci_filter_has_five_exclusions() -> None:
    atoms = {str(a) for a in gate.profile_ci_exclusions()}
    assert "package(hew-wasm)" in atoms, (
        "the live `not package(hew-wasm)` term must be in the parsed set"
    )
    text = gate.NEXTEST_TOML.read_text()
    line = re.search(r'^\s*default-filter\s*=\s*"([^"]*)"', text, re.M)
    assert line is not None
    # Proof that the parsed set equals the actual set: every selector call in
    # the source line is accounted for, and nothing was invented.
    literal = {
        f"{kind}({value})"
        for kind, value in re.findall(
            r"\b(binary_id|binary|package|test)\(([^)]+)\)", line.group(1)
        )
    }
    assert atoms == literal, f"parsed {atoms} != written {literal}"


# ── Finding 3: `-E` parsing and compensation ──────────────────────────────────


def parse(command: str) -> "gate.CargoInvocation":
    invocation = gate.parse_cargo_command("synthetic", command)
    assert invocation is not None, command
    return invocation


def test_every_dash_e_spelling_is_recognised() -> None:
    for command in [
        "cargo nextest run --workspace -E 'not binary(oracle)'",
        'cargo nextest run --workspace -E "not binary(oracle)"',
        "cargo nextest run --workspace -E=not-binary",
        "cargo nextest run --workspace --filter-expr 'not binary(oracle)'",
        "cargo nextest run --workspace --filter-expr='not binary(oracle)'",
    ]:
        assert parse(command).filtered, f"unrecognised filter form: {command}"
    # A double-quoted filter was invisible to the single-quote-only pattern.
    old = re.compile(r"-E\s+'([^']*)'")
    assert not old.search('cargo nextest run --workspace -E "not binary(oracle)"'), (
        "the shipped pattern must be shown to miss the double-quoted form"
    )


def test_a_positional_test_name_filter_counts_as_filtering() -> None:
    assert parse("cargo nextest run --workspace some_test_name").filtered


def test_an_unfiltered_run_is_not_reported_as_filtered() -> None:
    assert not parse("cargo nextest run --workspace --profile ci").filtered
    assert not parse(
        "cargo llvm-cov nextest --workspace --profile ci --lcov --output-path lcov.info"
    ).filtered, "a flag value is not a test-name filter"


def test_an_unclassified_flag_with_a_value_fails_closed() -> None:
    try:
        gate.parse_cargo_command(
            "synthetic", "cargo nextest run --workspace --invented-flag some_value"
        )
    except SystemExit:
        return
    raise AssertionError(
        "a bare word after an unknown flag is either a value to skip or a "
        "test-name filter to count; guessing decides whether the run is "
        "reported as filtered"
    )


def test_a_workspace_run_that_excludes_the_package_does_not_compensate() -> None:
    filtered = parse("cargo nextest run -p hew-cabi -E 'not test(slow)'")
    compensating = parse("cargo nextest run --workspace --exclude hew-cabi")
    assert gate.uncompensated_packages(filtered, [compensating], ["hew-cabi"]) == [
        "hew-cabi"
    ], (
        "a --workspace run carrying --exclude P executes nothing of P, so it "
        "cannot compensate a filtered run of P"
    )


def test_a_competing_filter_does_not_compensate() -> None:
    filtered = parse("cargo nextest run -p hew-cabi -E 'not test(slow)'")
    also_filtered = parse("cargo nextest run --workspace -E 'not binary(oracle)'")
    assert gate.uncompensated_packages(filtered, [also_filtered], ["hew-cabi"]) == [
        "hew-cabi"
    ], "a run that is itself filtered does not prove the filtered set ran"


def test_a_genuine_unfiltered_run_does_compensate() -> None:
    filtered = parse("cargo nextest run -p hew-cabi -E 'not test(slow)'")
    full = parse("cargo nextest run --workspace --profile ci")
    assert gate.uncompensated_packages(filtered, [full], ["hew-cabi"]) == []


# ── Finding 5: prerequisite lists written as variables ────────────────────────
#
# `check-libhew-fresh` runs on every build of every target that links a native
# Hew program, because it is an order-only prerequisite of the bundle those
# targets depend on. The checker still called it unreached: the bundle is a
# variable, and a reader that does not expand variables sees the seven
# characters `$(LIBHEW_READY)` where the graph has an edge. The demand that
# followed — wire a direct `make check-libhew-fresh` step — would have run the
# check twice and taught the next reader that a redundant step is how you
# satisfy this gate.
#
# The counterfactual matters more than the fix: expanding variables must not
# turn "is a prerequisite of something" into reachability. Only a prerequisite
# of an already-REACHED target is reached.


def variables(makefile: str) -> dict[str, str]:
    return gate.makefile_variables(makefile)


def test_a_prerequisite_bundle_behind_a_variable_is_an_edge() -> None:
    _, prereqs, _ = gate.parse_makefile(
        "LIB := build/lib.a\n"
        "READY := $(LIB) | check-lib-fresh\n"
        "\n"
        "functional-test: hew-native $(READY)\n"
        "\tcargo test --test functional\n"
    )
    assert "check-lib-fresh" in prereqs["functional-test"], (
        "an order-only prerequisite reached through a variable is still run "
        f"whenever the target is built; got {prereqs['functional-test']}"
    )
    assert "build/lib.a" in prereqs["functional-test"]


def test_a_prerequisite_of_an_unreached_target_confers_no_reachability() -> None:
    makefile = (
        "READY := | check-lib-fresh\n"
        "\n"
        "reached-gate: $(READY)\n"
        "\tcargo nextest run --workspace --profile ci\n"
        "\n"
        "check-lib-fresh:\n"
        "\tscripts/check-lib-fresh.sh\n"
        "\n"
        "orphan-gate: check-orphan-fresh\n"
        "\tbash scripts/orphan.sh\n"
        "\n"
        "check-orphan-fresh:\n"
        "\tbash scripts/orphan-fresh.sh\n"
    )
    phony, prereqs, recipes = gate.parse_makefile(makefile)
    known = set(prereqs) | phony
    reached = gate.close_over_makefile({"reached-gate"}, prereqs, recipes, known)
    assert "check-lib-fresh" in reached, "a prerequisite of a reached target is reached"
    assert "check-orphan-fresh" not in reached, (
        "orphan-gate is reached by nothing, so being ITS prerequisite proves "
        "nothing; reachability flows forward from CI roots or not at all"
    )
    assert "orphan-gate" not in reached


def test_a_conditionally_assigned_variable_is_not_inlined() -> None:
    values = variables(
        "ifeq ($(OS),Windows_NT)\nLIBNAME := hew.lib\nelse\nLIBNAME := libhew.a\nendif\n"
    )
    assert "LIBNAME" not in values, (
        "two branches assign it; picking one would invent a path make may never "
        "use, so every reference must stay verbatim"
    )
    assert gate.expand_makefile_text("$(LIBNAME)", values) == "$(LIBNAME)"


def test_a_variable_the_expander_cannot_evaluate_is_not_inlined() -> None:
    values = variables(
        "OUT := $(shell scripts/cargo-output-dir.py --root)\n"
        "FLAG := $(if $(TRIPLE),--target $(TRIPLE),)\n"
        "DEFAULTED ?= host\n"
        "APPENDED := a\nAPPENDED += b\n"
        "PLAIN := build\n"
    )
    assert set(values) == {"PLAIN"}, (
        "a shell call, a conditional function, an environment-overridable "
        f"default and an append are all unmodelled; kept {sorted(values)}"
    )


def test_expansion_leaves_an_opaque_reference_standing_as_one_token() -> None:
    values = variables(
        "ROOT := $(shell scripts/cargo-output-dir.py --root)\n"
        "DEBUG := $(ROOT)/debug\n"
        "LIB := $(DEBUG)/$(NAME)\n"
    )
    expanded = gate.expand_makefile_text("$(LIB)", values)
    assert expanded == "$(ROOT)/debug/$(NAME)", expanded
    assert len(expanded.split()) == 1, (
        "inlining the $(shell …) text would have split one artefact path into "
        "several prerequisites, none of which make ever names"
    )


def test_a_reference_cycle_terminates() -> None:
    values = variables("A := $(B)\nB := $(A)\n")
    assert gate.expand_makefile_text("$(A)", values) in {"$(A)", "$(B)"}


def test_a_shell_dollar_in_a_recipe_is_not_a_variable_reference() -> None:
    values = {"f": "SHOULD-NOT-APPEAR"}
    assert gate.expand_makefile_text("$$(basename $$f)", values) == "$$(basename $$f)"


def test_the_real_makefile_reaches_check_libhew_fresh_through_its_consumers() -> None:
    phony, prereqs, recipes = gate.parse_makefile(gate.MAKEFILE.read_text())
    consumers = {t for t, deps in prereqs.items() if "check-libhew-fresh" in deps}
    assert "observe-functional-test" in consumers, (
        "observe-functional-test depends on the archive-ready bundle, which "
        f"carries the freshness check; consumers seen: {sorted(consumers)}"
    )


# ── Finding 6: a build-artefact precondition behind a variable ────────────────
#
# A target proved through containment may assert that an artefact declared in
# its prerequisites exists. When the path moved behind a variable, an older
# classifier lost that relationship and treated the assertion as opaque.
#
# The licence is the prerequisite, not the path. Everything else about
# `test -f` stays unclassified.


def covered(command: str, artefacts: "set[str] | frozenset[str]" = frozenset()) -> bool:
    return gate._command_is_covered(
        command, "synthetic", set(), set(), [], [], [], set(), artefacts
    )


def test_a_precondition_on_a_declared_prerequisite_is_scaffolding() -> None:
    assert covered("test -f $(OUT)/debug/libhew.a", {"$(OUT)/debug/libhew.a"})


def test_a_precondition_on_a_path_the_target_never_declared_is_not() -> None:
    assert not covered("test -f $(OUT)/debug/libhew.a", {"runtime", "wasm-runtime"}), (
        "a `test -f` on something outside the target's own prerequisites is a "
        "claim about the machine, not about the build this graph describes"
    )
    assert not covered("test -f target/debug/libhew.a"), (
        "the old `target/…` prefix was a guess at 'build artefact'; the build "
        "graph answers exactly, so nothing rides on the path text any more"
    )


def test_a_real_command_is_not_smuggled_in_as_a_precondition() -> None:
    assert not covered("python3 scripts/lint-wasm-todo.py", {"whatever"})
    assert not covered("test -f a && cargo miri test", {"a"}), (
        "the precondition rule matches a whole segment; it cannot be used as a "
        "prefix that launders the command after it"
    )


def test_real_ci_reaches_the_complete_test_prerequisite_graph() -> None:
    phony, prereqs, recipes = gate.parse_makefile(gate.MAKEFILE.read_text())
    known = set(prereqs) | phony
    workflows = gate.load_workflows()
    commands = "\n".join(command for _, command in gate.ci_step_commands(workflows))
    if "ci-preflight-dispatcher.sh" in commands:
        fallback = subprocess.run(
            [
                "bash",
                str(gate.DISPATCHER),
                "--dry-run",
                "--",
                "some-unclassified-root-file.txt",
            ],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
        commands += "\n" + fallback.stdout
    roots = gate.make_targets_in(commands, known)
    reached = gate.close_over_makefile(roots, prereqs, recipes, known)
    required = {
        "test",
        "check-libhew-fresh",
        "libhew-debug",
        "runtime",
        "wasm-runtime",
    }
    assert required <= reached, (
        "CI must execute the authoritative Make test edge and its native, WASI, "
        f"and libhew prerequisites; missing {sorted(required - reached)}"
    )


# ── Counterfactual: a parity marker is not an edge ────────────────────────────
#
# A `parity-cmd` annotation is metadata for the separate preflight-parity
# checker, not an executable CI edge. Honouring the marker here would make a
# YAML comment plus a dispatcher entry sufficient to declare a target reached.
# The real workflow now executes `make test`; keep this counterfactual so a
# future inline replacement cannot recover reachability through a comment.


def test_a_parity_cmd_marker_is_not_an_edge() -> None:
    marked = """
name: ci
on: [pull_request]
jobs:
  build:
    steps:
      - run: >-  # parity-cmd: make miri
          cargo nextest run --workspace --profile ci
"""
    assert edges(marked) == set(), (
        "a `# parity-cmd:` annotation names a target without invoking it; "
        "reachability must come from the commands, not from the comment"
    )


def test_the_gate_has_no_marker_convention_at_all() -> None:
    source = SCRIPT.read_text()
    assert "parity-cmd" not in source, (
        "no annotation may confer reachability: a target whose only claim to "
        "running is a comment is exactly what this checker reports"
    )
    for waiver in ("UNREACHED_BY_DESIGN", "reachability-exempt", "ALLOWED_UNREACHED"):
        assert waiver not in source, (
            f"{waiver} would be an exemption list under another name; an "
            "unreached gate is wired in or deleted"
        )


# ── Finding 7: generic oracle/e2e gates and host release authorities ─────────
#
# `mqtt-broker-e2e` was a real Make target but neither of the old fixed-name
# lists matched it, so A1 never asked whether CI ran it. The inverse mistake is
# just as dangerous: `macos-leak-oracle` measures a local Darwin release host
# with leaks(1), and a hosted macOS step must not be allowed to claim that
# authority. These tests pin the two classes at the model boundary rather than
# trusting workflow prose about either one.


def test_a_new_unwired_generic_oracle_or_e2e_is_red() -> None:
    phony = {"new-behaviour-oracle", "new-network-e2e", "ordinary-build"}
    assert gate.ci_gate_targets(phony) == [
        "new-behaviour-oracle",
        "new-network-e2e",
    ], "generic *-oracle/*-e2e names must join the CI-gate class automatically"
    assert gate.unreached_ci_gates(phony, set()) == [
        "new-behaviour-oracle",
        "new-network-e2e",
    ], "an unwired new oracle/e2e must be red, not invisible to A1"


def test_a_named_host_authority_requires_a_real_uncommented_runner_port() -> None:
    authority = gate.HostReleaseAuthority(
        target="synthetic-darwin-oracle",
        host="Darwin",
        runner="scripts/synthetic-darwin-oracle.sh",
    )
    known = {authority.target}
    assert not gate.host_release_authority_is_ported(
        authority,
        known,
        {
            authority.target: (
                "# scripts/synthetic-darwin-oracle.sh\n@echo skipped on this host\n"
            )
        },
    ), "a comment or a green skip cannot impersonate a local authority"
    assert gate.host_release_authority_is_ported(
        authority,
        known,
        {authority.target: "scripts/synthetic-darwin-oracle.sh\n"},
    ), "the authority port is an exact executable runner command"


def test_macos_leak_runner_rejects_a_non_darwin_host_before_measuring() -> None:
    # Force the host classifier through PATH so this test is safe even when a
    # developer happens to run it on a Darwin workstation: the runner must
    # refuse the wrong host rather than printing a green skip.
    runner = ROOT / "scripts" / "macos-leak-oracle.sh"
    with tempfile.TemporaryDirectory() as tmp:
        fake_bin = Path(tmp) / "bin"
        fake_bin.mkdir()
        fake_uname = fake_bin / "uname"
        fake_uname.write_text("#!/usr/bin/env bash\nprintf 'Linux\\n'\n")
        fake_uname.chmod(0o755)
        env = {**os.environ, "PATH": f"{fake_bin}{os.pathsep}{os.environ['PATH']}"}
        result = subprocess.run(
            ["bash", str(runner)],
            cwd=ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
        )
    assert result.returncode != 0, result.stdout + result.stderr
    assert "Darwin is required" in result.stderr, result.stderr


def test_real_linux_workflows_provision_and_run_mqtt_without_hosting_macos_authority() -> (
    None
):
    expected = (
        (ROOT / ".github" / "workflows" / "ci.yml", "build-and-test"),
        (ROOT / ".github" / "workflows" / "release-gate.yml", "gate-linux"),
    )
    for path, job_name in expected:
        workflow = gate._load_workflow(path)
        job = next(job for job in workflow.jobs if job.ident == job_name)
        runnable = [step.run or "" for step in job.steps if not step.disabled]
        provision = next(
            (index, run)
            for index, run in enumerate(runnable)
            if "mosquitto-clients" in run
        )
        oracle = next(
            (index, run)
            for index, run in enumerate(runnable)
            if "make mqtt-broker-e2e" in run or "ci-preflight-dispatcher.sh" in run
        )
        assert "mosquitto_pub" in provision[1] and "mosquitto_sub" in provision[1], (
            f"{path.name}:{job_name} must verify both MQTT client commands"
        )
        assert provision[0] < oracle[0], (
            f"{path.name}:{job_name} must provision clients before the MQTT oracle"
        )
        if "ci-preflight-dispatcher.sh" in oracle[1]:
            selected = subprocess.run(
                [
                    "bash",
                    str(gate.DISPATCHER),
                    "--dry-run",
                    "--",
                    "some-unclassified-root-file.txt",
                ],
                cwd=ROOT,
                check=True,
                capture_output=True,
                text=True,
            )
            assert "make mqtt-broker-e2e" in selected.stdout
    workflows = gate.load_workflows()
    commands = "\n".join(command for _, command in gate.ci_step_commands(workflows))
    assert "macos-leak-oracle" not in gate.make_targets_in(
        commands, {"macos-leak-oracle"}
    ), "hosted CI must not certify the named local Darwin authority"


# ── Containment proofs ────────────────────────────────────────────────────────


def test_containment_refuses_an_opaque_command() -> None:
    recipes = {"opaque-check": "bash scripts/something.sh"}
    assert not gate.prove_contained(
        "opaque-check",
        {},
        recipes,
        set(),
        {"opaque-check"},
        [],
        [],
        ["hew-cabi"],
        set(),
    ), "a script CI never runs is a wire-or-cut decision, not a proof"


def test_containment_refuses_an_env_prefixed_command() -> None:
    recipes = {"asan-ish": "RUSTFLAGS=-Zsanitizer=address cargo test --workspace"}
    blobs = ["cargo nextest run --workspace --profile ci"]
    assert not gate.prove_contained(
        "asan-ish", {}, recipes, set(), {"asan-ish"}, [], blobs, ["hew-cabi"], set()
    ), "a sanitizer-flagged run proves something the plain CI run does not"


def test_containment_accepts_a_narrower_selection_of_what_ci_runs() -> None:
    recipes = {"test-cabi-only": "cargo nextest run --profile ci -p hew-cabi"}
    blobs = ["cargo nextest run --workspace --profile ci"]
    assert gate.prove_contained(
        "test-cabi-only",
        {},
        recipes,
        set(),
        {"test-cabi-only"},
        [],
        blobs,
        ["hew-cabi"],
        set(),
    )


def test_containment_refuses_a_binary_profile_ci_subtracts() -> None:
    recipes = {"parity-check": "cargo test -p hew-sandbox-wasm --test parity"}
    blobs = ["cargo nextest run --workspace --profile ci"]
    assert not gate.prove_contained(
        "parity-check",
        {},
        recipes,
        set(),
        {"parity-check"},
        [],
        blobs,
        ["hew-sandbox-wasm"],
        {"parity"},
    ), "the CI run subtracts binary(parity), so it does not contain this"


# ── End to end ────────────────────────────────────────────────────────────────


def test_real_repo_state_passes_the_full_check() -> None:
    result = subprocess.run(
        [sys.executable, str(SCRIPT)],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stdout + result.stderr


# ── Finding 4: docs -> Makefile was never checked ─────────────────────────────
#
# A0..A3 read one direction of the edge, CI -> Makefile. Deleting a target
# therefore left every documented invocation of it dangling with nothing to
# notice, which is how the CONTRIBUTING test-suite table came to hand a new
# contributor a command that produces `No rule to make target`. These pin A4's
# two halves: it must SEE a real invocation written in documentation, and it
# must NOT see the verb "make" in ordinary prose, or the gate becomes noise
# somebody switches off.

MISSING = "no-such-target"


def references(chunk: str, prose: bool = False) -> list[str]:
    return gate.make_references_in(chunk, prose)


def test_a_dead_target_in_a_doc_code_span_is_a_reference() -> None:
    found = references(f"make {MISSING}")
    assert found == [MISSING], f"a documented invocation must be seen, got {found}"


def test_the_verb_make_in_prose_is_not_a_reference() -> None:
    for sentence in (
        "make sure the runtime is built first",
        "deep-copy the state; make a second copy for restart",
        "this would make one entry shadow another",
    ):
        found = references(sentence, prose=True)
        assert found == [], f"prose is not an invocation: {sentence!r} gave {found}"


def test_a_hyphenated_english_compound_is_the_accepted_residual() -> None:
    # The mid-prose rule reads the next token when it carries a hyphen, so a
    # hyphenated compound sitting directly after the verb is read as a target.
    # This pins the cost rather than hiding it: it is why A4 does not read Rust
    # sources, where that phrasing is common, and the fix when it does bite is to
    # reword or backtick the sentence — never a per-file skip, which would take
    # the file's real invocations out of the check along with the false one.
    found = references("this would make distinct-but-equal keys collide", prose=True)
    assert found == ["distinct-but-equal"], f"got {found}"


def test_a_hyphenated_target_in_prose_is_still_a_reference() -> None:
    # The stale comments in the two ratchet wrappers were exactly this shape:
    # an invocation embedded in a sentence, with no backticks to mark it as a
    # command. Restricting A4 to code spans alone would have missed all three.
    found = references("wire make test-hew directly into gates", prose=True)
    assert found == ["test-hew"], f"expected the target name, got {found}"


def test_a_commit_subject_in_backticks_is_prose() -> None:
    # LESSONS.md quotes commit subjects in code spans. `fix(build): make Windows
    # source builds link-ready` is a sentence that happens to sit in backticks,
    # and reading it as an invocation of four targets is how a checker earns a
    # reputation for crying wolf.
    found = references("fix(build): make Windows source builds link-ready")
    assert found == [], f"a quoted commit subject is not an invocation, got {found}"


def test_a_metavariable_target_is_not_a_reference() -> None:
    # This is the entire exemption mechanism: an example showing the SHAPE of a
    # command writes a placeholder, so the reader is never handed something that
    # looks runnable and is not. There is no per-file skip to reach for instead.
    for illustrative in ("make <target>", "make $(GATE)", "make ${GATE}", "make foo-%"):
        found = references(illustrative)
        assert found == [], f"{illustrative!r} is illustrative, got {found}"


def test_flags_and_variable_overrides_are_not_targets() -> None:
    found = references("make -j8 fuzz-oracle FUZZ_ORACLE_FULL=1")
    assert found == ["fuzz-oracle"], f"expected one target, got {found}"


def test_every_target_of_a_multi_target_invocation_is_a_reference() -> None:
    found = references("make verify-ffi test-verify-ffi")
    assert found == ["verify-ffi", "test-verify-ffi"], f"got {found}"


def test_a_python_string_is_data_and_its_comment_is_not() -> None:
    chunks = gate.script_chunks(
        f'FIXTURE = "make {MISSING}"  # see make test-compiler-pipeline\n',
        executable=False,
    )
    found = [
        t for _, chunk, prose in chunks for t in gate.make_references_in(chunk, prose)
    ]
    assert found == ["test-compiler-pipeline"], (
        "a target name inside a Python string literal is generated fixture text, "
        f"but its comment is documentation; got {found}"
    )


def test_a_dead_reference_in_a_tracked_doc_is_found_end_to_end() -> None:
    # The counterfactual, run against a real git checkout rather than a string:
    # a doc that is tracked, and an identical one that is not, must differ.
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        env = {
            "GIT_CONFIG_GLOBAL": str(root / "gitconfig"),
            "GIT_CONFIG_SYSTEM": str(root / "gitconfig"),
            "PATH": os.environ.get("PATH", ""),
            "HOME": tmp,
        }
        subprocess.run(["git", "init", "-q", tmp], check=True, env=env)
        (root / "CONTRIBUTING.md").write_text(f"Run `make {MISSING}` before pushing.\n")
        (root / "UNTRACKED.md").write_text(
            f"Run `make {MISSING}-too` before pushing.\n"
        )
        subprocess.run(["git", "add", "CONTRIBUTING.md"], cwd=tmp, check=True, env=env)
        found = gate.documented_make_references(root)
    assert [(r.target, r.where) for r in found] == [(MISSING, "CONTRIBUTING.md:1")], (
        f"expected the tracked doc's dead reference and nothing else, got {found}"
    )


def test_an_external_target_annotation_does_not_hide_an_unannotated_target() -> None:
    # A4 cannot resolve a target owned by another repository, but its exemption
    # must be explicit and cannot make an otherwise identical local typo pass.
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        env = {
            "GIT_CONFIG_GLOBAL": str(root / "gitconfig"),
            "GIT_CONFIG_SYSTEM": str(root / "gitconfig"),
            "PATH": os.environ.get("PATH", ""),
            "HOME": tmp,
        }
        subprocess.run(["git", "init", "-q", tmp], check=True, env=env)
        (root / "CONTRIBUTING.md").write_text(
            f"`make {MISSING}` <!-- external-target: hew-lang/playground -->\n"
            f"`make {MISSING}`\n"
        )
        subprocess.run(["git", "add", "CONTRIBUTING.md"], cwd=tmp, check=True, env=env)
        found = gate.documented_make_references(root)
    assert [(r.target, r.where) for r in found] == [(MISSING, "CONTRIBUTING.md:2")], (
        "the external annotation must exempt only its line; an unannotated "
        f"unknown target must remain a finding, got {found}"
    )


def test_an_external_target_annotation_requires_owner_and_repository() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        env = {
            "GIT_CONFIG_GLOBAL": str(root / "gitconfig"),
            "GIT_CONFIG_SYSTEM": str(root / "gitconfig"),
            "PATH": os.environ.get("PATH", ""),
            "HOME": tmp,
        }
        subprocess.run(["git", "init", "-q", tmp], check=True, env=env)
        (root / "CONTRIBUTING.md").write_text(
            f"`make {MISSING}` <!-- external-target: playground -->\n"
        )
        subprocess.run(["git", "add", "CONTRIBUTING.md"], cwd=tmp, check=True, env=env)
        found = gate.documented_make_references(root)
    assert [(r.target, r.where) for r in found] == [(MISSING, "CONTRIBUTING.md:1")], (
        "a bare external annotation does not identify an owner/repository and "
        f"must not hide an unknown target, got {found}"
    )


# ── A5: a declared target does work ──────────────────────────────────────────
#
# A0..A4 resolve a name. A5 asks whether the name does anything, because a
# `.PHONY` target with no recipe and no prerequisites succeeds having run
# nothing — and satisfies every name-resolving check while it does.


def test_a_phony_target_with_no_rule_has_no_recipe_and_no_prerequisites() -> None:
    phony, prereqs, recipes = gate.parse_makefile(
        ".PHONY: verify-thing\n\nother:\n\techo hi\n"
    )
    assert "verify-thing" in phony
    assert not recipes.get("verify-thing", "").strip(), (
        "a .PHONY name with no rule line must read as having no recipe — that "
        "is the state make treats as a target that succeeds doing nothing"
    )
    assert not prereqs.get("verify-thing"), (
        "and no prerequisites either; with prerequisites it would be a "
        "legitimate aggregator"
    )


def test_an_aggregator_with_prerequisites_and_no_recipe_does_work() -> None:
    # `lint: a b c` is the correct shape for a target that exists to bring
    # others up to date. A5 must not read it as inert.
    _, prereqs, recipes = gate.parse_makefile(
        ".PHONY: lint\nlint: alpha beta\n\nalpha:\n\techo a\n"
    )
    assert not recipes.get("lint", "").strip()
    assert prereqs.get("lint") == {"alpha", "beta"}


def test_a_recipe_guarded_by_a_make_conditional_still_belongs_to_its_target() -> None:
    # A conditional directive sits at column 0 like a rule line does. Reading it
    # as the end of the rule loses every recipe line on both arms, which reads a
    # host-guarded target as inert. `asan-fixtures` and `tsan` are both this
    # shape, and both were false positives until the parser learned it.
    _, _, recipes = gate.parse_makefile(
        ".PHONY: asan-fixtures\n"
        "asan-fixtures:\n"
        "ifeq ($(shell uname -s),Darwin)\n"
        "\t@echo skipped\n"
        "else\n"
        "\tscripts/asan-fixture-check.sh\n"
        "endif\n"
    )
    recipe = recipes.get("asan-fixtures", "")
    assert "asan-fixture-check.sh" in recipe, (
        "the recipe on the else arm belongs to asan-fixtures; losing it reads a "
        f"working gate as one that does nothing, got {recipe!r}"
    )
    assert "@echo skipped" in recipe


def test_the_real_makefile_has_no_inert_declared_target() -> None:
    phony, prereqs, recipes = gate.parse_makefile((ROOT / "Makefile").read_text())
    inert = sorted(
        t for t in phony if not recipes.get(t, "").strip() and not prereqs.get(t)
    )
    assert inert == [], (
        "these targets are declared but neither run a recipe nor pull in a "
        f"prerequisite, so make succeeds having done nothing: {inert}"
    )


def test_script_stays_python_3_10_compatible_with_no_new_dependency() -> None:
    # Structural YAML parsing was the fix for finding 1, and the obvious
    # implementation is PyYAML. Nothing in this repo installs it: no workflow
    # runs pip, and there is no requirements file, so the checker would fail
    # to import on every CI runner and — depending on how it was invoked —
    # take the gate out silently. Hence the hand-written subset parser.
    source = SCRIPT.read_text()
    for banned in ("yaml", "tomllib", "toml", "tomli", "ruamel"):
        assert not re.search(rf"^\s*import {banned}\b", source, re.MULTILINE), (
            f"check-gate-reachability.py must not import {banned}: CI installs "
            "no Python packages, and the tooling baseline is Python 3.10"
        )
        assert not re.search(rf"^\s*from {banned}\b", source, re.MULTILINE), (
            f"check-gate-reachability.py must not import from {banned}"
        )
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            f"compile(open({str(SCRIPT)!r}).read(), {str(SCRIPT)!r}, 'exec')",
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr


# ── A11: harness self-tests under scripts/tests/ are invoked ──────────────────


def _synthetic_repo(tmp: str) -> Path:
    root = Path(tmp)
    (root / "scripts" / "tests").mkdir(parents=True)
    (root / "scripts" / "tests" / "test_wired.py").write_text("")
    (root / "scripts" / "tests" / "test_orphan.sh").write_text("")
    return root


def test_a_harness_test_a_reached_recipe_runs_is_invoked() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        text = gate.harness_invocation_text(
            [], {"gate": "\tpython3 scripts/tests/test_wired.py"}, {"gate"}, root
        )
        assert "scripts/tests/test_wired.py" in text
        assert "scripts/tests/test_orphan.sh" not in text


def test_a_harness_test_named_only_in_a_comment_is_not_invoked() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        recipe = "\t# see scripts/tests/test_orphan.sh for the counterfactual\n\ttrue"
        text = gate.harness_invocation_text([], {"gate": recipe}, {"gate"}, root)
        assert "scripts/tests/test_orphan.sh" not in text


def test_a_harness_test_only_echoed_is_not_invoked() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        recipe = '\techo "run scripts/tests/test_orphan.sh yourself"'
        text = gate.harness_invocation_text([], {"gate": recipe}, {"gate"}, root)
        assert "scripts/tests/test_orphan.sh" not in text


def test_a_shell_script_a_reached_recipe_runs_carries_its_harness_tests() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        (root / "scripts" / "wrapper.sh").write_text(
            "python3 scripts/tests/test_orphan.sh\n"
        )
        text = gate.harness_invocation_text(
            [], {"gate": "\tscripts/wrapper.sh"}, {"gate"}, root
        )
        assert "scripts/tests/test_orphan.sh" in text


def test_a_python_file_naming_a_harness_test_is_not_an_indirection() -> None:
    """A Python gate asserting something ABOUT a test does not run it.

    A checker can carry the literal path of a rule counterfactual because it
    asserts that a wrapper invokes it exactly once. Following that would let an
    assertion about an edge stand in for the edge.
    """
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        (root / "scripts" / "asserter.py").write_text(
            '("python3", "scripts/tests/test_orphan.sh")\n'
        )
        text = gate.harness_invocation_text(
            [], {"gate": "\tpython3 scripts/asserter.py"}, {"gate"}, root
        )
        assert "scripts/tests/test_orphan.sh" not in text


def test_a_host_release_authority_can_invoke_its_own_counterfactuals() -> None:
    """A Darwin-only oracle is unreachable from CI BY DESIGN (A1H).

    Requiring its self-tests to be CI-reached would demand deleting the
    counterfactuals that keep it from certifying a skip as a measurement.
    """
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        authority = gate.HOST_RELEASE_AUTHORITIES[0].target
        text = gate.harness_invocation_text(
            [], {authority: "\tscripts/tests/test_orphan.sh"}, set(), root
        )
        assert "scripts/tests/test_orphan.sh" in text


def _synthetic_workflow(extra_step: str) -> str:
    return f"""name: synthetic
on: push
jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - name: live owner
        run: python3 scripts/tests/test_wired.py
{extra_step}"""


def _workflow_harness_text(root: Path, workflow: str) -> str:
    model = gate._parse_workflow(workflow, "synthetic.yml")
    commands = gate.ci_step_commands([model])
    return gate.harness_invocation_text(commands, {}, set(), root)


def test_a_harness_test_named_only_in_a_workflow_comment_is_not_invoked() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        workflow = _synthetic_workflow("      # python3 scripts/tests/test_orphan.sh\n")
        text = _workflow_harness_text(root, workflow)
        assert "scripts/tests/test_wired.py" in text
        assert "scripts/tests/test_orphan.sh" not in text


def test_a_harness_test_only_echoed_by_a_workflow_step_is_not_invoked() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        workflow = _synthetic_workflow(
            "      - name: mention only\n"
            "        run: echo 'python3 scripts/tests/test_orphan.sh'\n"
        )
        text = _workflow_harness_text(root, workflow)
        assert "scripts/tests/test_wired.py" in text
        assert "scripts/tests/test_orphan.sh" not in text


def test_a_harness_test_only_stored_by_a_workflow_step_is_not_invoked() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        workflow = _synthetic_workflow(
            "      - name: string mention only\n"
            "        run: |\n"
            "          TEST_PATH='scripts/tests/test_orphan.sh'\n"
            '          echo "$TEST_PATH"\n'
        )
        text = _workflow_harness_text(root, workflow)
        assert "scripts/tests/test_wired.py" in text
        assert "scripts/tests/test_orphan.sh" not in text


def test_a_harness_test_in_a_disabled_workflow_step_is_not_invoked() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        workflow = _synthetic_workflow(
            "      - name: disabled owner\n"
            "        if: false\n"
            "        run: python3 scripts/tests/test_orphan.sh\n"
        )
        text = _workflow_harness_text(root, workflow)
        assert "scripts/tests/test_wired.py" in text
        assert "scripts/tests/test_orphan.sh" not in text


def test_a_harness_test_under_a_dynamic_condition_can_be_invoked() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        root = _synthetic_repo(tmp)
        workflow = _synthetic_workflow(
            "      - name: conditional owner\n"
            "        if: ${{ needs.changes.outputs.scripts == 'true' }}\n"
            "        run: bash scripts/tests/test_orphan.sh\n"
        )
        text = _workflow_harness_text(root, workflow)
        assert "scripts/tests/test_wired.py" in text
        assert "scripts/tests/test_orphan.sh" in text


def test_every_real_harness_test_is_invoked() -> None:
    result = subprocess.run(
        [sys.executable, str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    count = len(gate.harness_tests())
    assert f"{count}/{count} self-tests are invoked." in result.stdout, result.stdout


# The runner enumerates this module rather than reading a hand-maintained list.
# The list form had already lost four tests: they were defined, never listed,
# and never run — a self-test file quietly asserting less than it appeared to,
# which is the same defect A11 exists to catch one level out. Discovery cannot
# drift.
def _tests() -> list:
    return [
        value
        for name, value in sorted(globals().items())
        if name.startswith("test_") and callable(value)
    ]


if __name__ == "__main__":
    tests = _tests()
    failures = 0
    for test in tests:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(tests)} tests failed")
    print(f"All {len(tests)} tests passed.")
