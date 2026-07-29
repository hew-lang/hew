"""Static contract tests for the release workflow's prerelease handoff."""

import os
import re
import subprocess
import tempfile
import textwrap
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HEW_SHA = "0123456789abcdef0123456789abcdef01234567"
WORKFLOW = ROOT / ".github" / "workflows" / "release.yml"
NPM_PUBLISH_WORKFLOW = ROOT / ".github" / "workflows" / "publish-npm-packages.yml"
RELEASE_GATE = ROOT / ".github" / "workflows" / "release-gate.yml"
CI_WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
RELEASE_NOTES = ROOT / "docs" / "releases" / "v0.6.0-rc1.md"
RUNBOOK = ROOT / "docs" / "release-runbook.md"
UNIX_INSTALLER = ROOT / "installers" / "install.sh"
PRE_RELEASE_VALIDATOR = ROOT / "scripts" / "pre-release-validate.sh"
RELEASE_LINK_PROBE = ROOT / "scripts" / "test-release-lib-link.sh"
WINDOWS_RELEASE_LINK_PROBE = ROOT / "scripts" / "test-release-lib-link.ps1"
SANITIZER_GATE = ROOT / "scripts" / "check-sanitizer-gate.sh"
MAKEFILE = ROOT / "Makefile"
RELEASE_BINARY_SMOKE = ROOT / "scripts" / "test-release-binary.sh"


def workflow() -> str:
    return WORKFLOW.read_text()


def npm_publish_workflow() -> str:
    return NPM_PUBLISH_WORKFLOW.read_text()


def playground_job(text: str | None = None) -> str:
    text = workflow() if text is None else text
    start = text.index("  playground:\n")
    end = text.index(
        "  # ─────────────────────────────────────────────────────────────────────────\n  # VS Code",
        start,
    )
    return text[start:end]


def playground_script(text: str | None = None) -> str:
    """Extract the exact Bash program executed by the playground job."""
    job = playground_job() if text is None else text
    step = job.index("      - name: Trigger playground image rebuild\n")
    run = job.index("        run: |\n", step) + len("        run: |\n")
    return textwrap.dedent(job[run:]).rstrip() + "\n"


def assert_exact_dispatch_correlation(job: str) -> None:
    """Require the unique caller identity in both dispatch and run selection."""
    assert 'CORRELATION_ID="hew-${GITHUB_RUN_ID}-${GITHUB_RUN_ATTEMPT}"' in job
    assert (
        'EXPECTED_DISPLAY_TITLE="Build Playground mode=publish'
        ' sha=${HEW_SHA} version=${VERSION} correlation=${CORRELATION_ID}"' in job
    )

    dispatch_start = job.index("          gh workflow run build.yml")
    dispatch_end = job.index('          RUN_ID=""', dispatch_start)
    dispatch = job[dispatch_start:dispatch_end]
    assert "-f publish=true \\\n" in dispatch
    assert '-f hew_sha="${HEW_SHA}" \\\n' in dispatch
    assert '-f version="${VERSION}" \\\n' in dispatch
    assert '-f correlation_id="${CORRELATION_ID}"' in dispatch

    query_start = job.index("            if ! RUN_IDS_OUTPUT=$(jq -r")
    query_end = job.index("            RUN_IDS=()", query_start)
    query = job[query_start:query_end]
    expected = """            if ! RUN_IDS_OUTPUT=$(jq -r \\
                --argjson floor "${PRE_DISPATCH_MAX_ID}" \\
                --arg sha "${PLAYGROUND_SHA}" \\
                --arg actor "${DISPATCH_ACTOR}" \\
                --arg display_title "${EXPECTED_DISPLAY_TITLE}" \\
                '.workflow_runs[]
                  | select(.id > $floor)
                  | select(.head_sha == $sha)
                  | select(.actor.login == $actor)
                  | select(.display_title == $display_title)
                  | .id' <<< "${RUNS_JSON}"); then
              echo "::error::failed to parse playground workflow runs"
              exit 1
            fi
"""
    assert query == expected


def assert_fail_closed_run_retrieval(job: str) -> None:
    """Require API retrieval and jq parsing to have independent status checks."""
    assert "        shell: bash\n" in job
    script = playground_script(job)
    assert script.startswith("set -euo pipefail\n")
    start = job.index("            if ! RUNS_JSON=$(gh api -X GET")
    end = job.index("            if ! RUN_IDS_OUTPUT=$(jq -r", start)
    retrieval = job[start:end]
    assert "| jq" not in retrieval
    assert 'echo "::error::failed to list playground workflow runs"' in retrieval
    assert "              exit 1\n" in retrieval


_MOCK_GH = r"""#!/usr/bin/env python3
import json
import os
import sys
from pathlib import Path

args = sys.argv[1:]
state = Path(os.environ["MOCK_STATE"])
log = Path(os.environ["MOCK_LOG"])
with log.open("a") as stream:
    stream.write("gh " + " ".join(args) + "\n")

if args[:2] == ["repo", "view"]:
    raise SystemExit(0)
if args[:2] == ["workflow", "view"]:
    print("active")
    raise SystemExit(0)
if args[:2] == ["workflow", "run"]:
    raise SystemExit(0)
if args[:2] == ["run", "watch"]:
    raise SystemExit(0 if args[2] == "101" else 91)
if not args or args[0] != "api":
    raise SystemExit(92)

joined = " ".join(args)
if "repos/hew-lang/playground/commits/main" in joined:
    print("sha-good")
    raise SystemExit(0)
if "repos/hew-lang/playground/actions/workflows/build.yml/runs" in joined:
    if "--jq" in args:
        print("100")
        raise SystemExit(0)
    scenario = os.environ["MOCK_SCENARIO"]
    if scenario == "api-failure":
        print("mock API failure", file=sys.stderr)
        raise SystemExit(42)
    poll = int(state.read_text() or "0") + 1 if state.exists() else 1
    state.write_text(str(poll))
    matching = {
        "id": 101,
        "head_sha": "sha-good",
        "actor": {"login": "actor-good"},
        "display_title": (
            "Build Playground mode=publish"
            " sha=0123456789abcdef0123456789abcdef01234567"
            " version=0.6.0-rc1 correlation=hew-777-2"
        ),
    }
    if scenario == "empty":
        runs = []
    elif scenario == "ambiguous":
        runs = [matching, {**matching, "id": 102}]
    elif scenario in {"title", "head", "actor", "floor"}:
        candidate = dict(matching)
        if scenario == "title":
            candidate["display_title"] = "Build Playground wrong"
        elif scenario == "head":
            candidate["head_sha"] = "sha-wrong"
        elif scenario == "actor":
            candidate["actor"] = {"login": "actor-wrong"}
        else:
            candidate["id"] = 100
        runs = [candidate]
    else:
        runs = [
            {**matching, "id": 100},
            {**matching, "head_sha": "sha-wrong", "id": 103},
            {**matching, "actor": {"login": "actor-wrong"}, "id": 104},
            {**matching, "display_title": "Build Playground wrong", "id": 105},
            matching,
        ]
    print(json.dumps({"workflow_runs": runs}))
    raise SystemExit(0)
if "repos/hew-lang/playground" in joined:
    print("main")
    raise SystemExit(0)
if args[1:] == ["user", "--jq", ".login"]:
    print("actor-good")
    raise SystemExit(0)
raise SystemExit(93)
"""


def run_playground(
    scenario: str, *, jq_failure: bool = False, hew_sha: str = HEW_SHA
) -> tuple:
    """Execute the workflow's exact Bash with deterministic command doubles."""
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        bin_dir = root / "bin"
        bin_dir.mkdir()
        gh = bin_dir / "gh"
        gh.write_text(_MOCK_GH)
        gh.chmod(0o755)
        sleep = bin_dir / "sleep"
        sleep.write_text("#!/usr/bin/env bash\nexit 0\n")
        sleep.chmod(0o755)
        if jq_failure:
            jq = bin_dir / "jq"
            jq.write_text("#!/usr/bin/env bash\nexit 23\n")
            jq.chmod(0o755)
        state = root / "state"
        log = root / "calls.log"
        env = os.environ.copy()
        env.update(
            {
                "PATH": f"{bin_dir}:{env['PATH']}",
                "GH_TOKEN": "test-token",
                "HEW_SHA": hew_sha,
                "RELEASE_TAG": "v0.6.0-rc1",
                "GITHUB_RUN_ID": "777",
                "GITHUB_RUN_ATTEMPT": "2",
                "MOCK_SCENARIO": scenario,
                "MOCK_STATE": str(state),
                "MOCK_LOG": str(log),
            }
        )
        result = subprocess.run(
            ["bash", "-c", playground_script()],
            cwd=ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
        )
        calls = log.read_text().splitlines() if log.exists() else []
        polls = int(state.read_text()) if state.exists() else 0
        return result, calls, polls


def assert_wait_budget(job: str) -> None:
    """Keep caller time above the downstream 5+5+45+30+5 minute maximum."""
    match = re.search(r"^    timeout-minutes: (\d+)$", job, re.MULTILINE)
    assert match is not None
    assert int(match.group(1)) >= 100


def test_rc_tag_normalization_and_exact_release_body() -> None:
    text = workflow()
    assert "RELEASE_TAG: ${{ github.event.inputs.tag || github.ref_name }}" in text
    assert 'VERSION="${RELEASE_TAG#v}"' in playground_job()
    assert "body_path: docs/releases/${{ env.RELEASE_TAG }}.md" in text
    assert RELEASE_NOTES.exists()


def test_npm_publication_is_pinned_to_a_version_matching_release_tag() -> None:
    text = npm_publish_workflow()
    assert "      release_tag:\n" in text
    assert "Immutable release tag to publish" in text
    assert "        required: true\n" in text
    assert "        type: string\n" in text
    assert "  group: publish-npm-packages-${{ inputs.release_tag }}" in text
    assert "          ref: ${{ inputs.release_tag }}" in text
    assert (
        "      - name: Verify immutable release identity and package versions\n" in text
    )
    assert "RELEASE_TAG: ${{ inputs.release_tag }}" in text
    assert "^v[0-9]+\\.[0-9]+\\.[0-9]+(-[0-9A-Za-z.-]+)?$" in text
    assert 'EXPECTED_VERSION="${RELEASE_TAG#v}"' in text
    assert (
        "Cargo.toml version ${WORKSPACE_VERSION} does not match ${RELEASE_TAG}" in text
    )
    assert (
        "hew-sandbox-vm version ${SANDBOX_VM_VERSION} does not match ${RELEASE_TAG}"
        in text
    )


def test_playground_dispatch_is_purpose_scoped_and_fail_closed() -> None:
    job = playground_job()
    assert "      - name: Resolve release commit identity\n" in job
    assert (
        "gh api \"repos/${GITHUB_REPOSITORY}/commits/${RELEASE_TAG}\" --jq '.sha'"
    ) in job
    assert "did not resolve to an exact lowercase" in job
    assert "HEW_SHA: ${{ steps.release-commit.outputs.hew_sha }}" in job
    assert "PLAYGROUND_DISPATCH_TOKEN" in job
    assert "HOMEBREW_TAP_TOKEN" not in job
    assert 'if [ -z "${GH_TOKEN}" ]; then' in job
    assert "PLAYGROUND_DISPATCH_TOKEN secret is required" in job
    assert "exit 1" in job
    assert "gh repo view hew-lang/playground" in job
    assert "gh api repos/hew-lang/playground --jq '.default_branch'" in job
    assert "gh workflow view build.yml" in job
    assert '!= "active"' in job
    assert "could not correlate" in job


def test_dispatch_uses_exact_playground_workflow_input_and_ref() -> None:
    job = playground_job()
    assert "gh workflow run build.yml" in job
    assert '--ref "${PLAYGROUND_REF}"' in job
    assert "-f publish=true" in job
    assert '-f hew_sha="${HEW_SHA}"' in job
    assert '-f version="${VERSION}"' in job
    assert "-f event=workflow_dispatch" in job
    assert '-f branch="${PLAYGROUND_REF}"' in job
    assert 'gh run watch "${RUN_ID}" -R hew-lang/playground --exit-status' in job


def test_dispatch_correlation_is_unique_and_bounded() -> None:
    job = playground_job()
    assert_wait_budget(job)
    assert_exact_dispatch_correlation(job)
    assert "PLAYGROUND_SHA=" in job
    assert "PRE_DISPATCH_MAX_ID=" in job
    assert '--argjson floor "${PRE_DISPATCH_MAX_ID}"' in job
    assert '--arg sha "${PLAYGROUND_SHA}"' in job
    assert "DISPATCH_ACTOR=" in job
    assert '--arg actor "${DISPATCH_ACTOR}"' in job
    assert "select(.id > $floor)" in job
    assert "select(.head_sha == $sha)" in job
    assert "select(.actor.login == $actor)" in job
    assert_fail_closed_run_retrieval(job)
    assert "if ! RUN_IDS_OUTPUT=$(jq -r" in job
    assert 'if [ -n "${CANDIDATE_ID}" ]; then' in job
    assert "mapfile -t RUN_IDS < <(" not in job
    assert "LAST_CANDIDATE_ID" in job
    assert "ambiguous playground workflow dispatch correlation" in job


def test_exact_workflow_shell_accepts_one_stable_matching_run() -> None:
    result, calls, polls = run_playground("success")
    assert result.returncode == 0, result.stderr
    assert polls == 2
    dispatches = [call for call in calls if call.startswith("gh workflow run")]
    assert dispatches == [
        "gh workflow run build.yml -R hew-lang/playground --ref main"
        f" -f publish=true -f hew_sha={HEW_SHA}"
        " -f version=0.6.0-rc1 -f correlation_id=hew-777-2"
    ]
    watches = [call for call in calls if call.startswith("gh run watch")]
    assert watches == ["gh run watch 101 -R hew-lang/playground --exit-status"]


def test_malformed_release_commit_identity_is_terminal() -> None:
    for bad_sha in ("", "not-a-sha", HEW_SHA[:39], HEW_SHA.upper()):
        result, calls, polls = run_playground("success", hew_sha=bad_sha)
        assert result.returncode != 0, repr(bad_sha)
        assert "release commit identity is not an exact lowercase" in result.stdout, (
            repr(bad_sha)
        )
        assert polls == 0, repr(bad_sha)
        assert not any(call.startswith("gh workflow run") for call in calls), repr(
            bad_sha
        )


def test_run_listing_api_failure_is_terminal() -> None:
    result, calls, polls = run_playground("api-failure")
    assert result.returncode != 0
    assert "failed to list playground workflow runs" in result.stdout
    assert polls == 0
    assert not any(call.startswith("gh run watch") for call in calls)


def test_run_listing_jq_failure_is_terminal() -> None:
    result, calls, polls = run_playground("success", jq_failure=True)
    assert result.returncode != 0
    assert "failed to parse playground workflow runs" in result.stdout
    assert polls == 1
    assert not any(call.startswith("gh run watch") for call in calls)


def test_successful_empty_polls_exhaust_the_bound() -> None:
    result, calls, polls = run_playground("empty")
    assert result.returncode != 0
    assert "could not correlate" in result.stdout
    assert polls == 30
    assert not any(call.startswith("gh run watch") for call in calls)


def test_ambiguous_correlation_fails_on_the_first_poll() -> None:
    result, calls, polls = run_playground("ambiguous")
    assert result.returncode != 0
    assert "ambiguous playground workflow dispatch correlation" in result.stdout
    assert polls == 1
    assert not any(call.startswith("gh run watch") for call in calls)


def test_each_exact_run_identity_dimension_is_mandatory() -> None:
    for scenario in ("title", "head", "actor", "floor"):
        result, calls, polls = run_playground(scenario)
        assert result.returncode != 0, scenario
        assert "could not correlate" in result.stdout, scenario
        assert polls == 30, scenario
        assert not any(call.startswith("gh run watch") for call in calls), scenario


def test_timeout_undercut_mutation_is_rejected() -> None:
    mutated = playground_job().replace(
        "    timeout-minutes: 120", "    timeout-minutes: 80"
    )
    try:
        assert_wait_budget(mutated)
    except AssertionError:
        return
    raise AssertionError(
        "the upstream wait accepted the downstream maximum without margin"
    )


def test_publish_mode_downgrade_mutation_is_rejected() -> None:
    mutated = playground_job().replace("-f publish=true", "-f publish=false")
    mutated += "\n# -f publish=true\n"
    try:
        assert_exact_dispatch_correlation(mutated)
    except (AssertionError, ValueError):
        return
    raise AssertionError("a publish-mode downgrade was hidden by padding")


def test_correlation_swap_with_padding_is_rejected() -> None:
    mutated = playground_job().replace(
        "select(.display_title == $display_title)",
        "select(.display_title == $actor)",
    )
    mutated += "\n# select(.display_title == $display_title)\n"
    try:
        assert_exact_dispatch_correlation(mutated)
    except (AssertionError, ValueError):
        return
    raise AssertionError("a swapped selector was hidden by padding outside its query")


def test_correlation_argument_swap_with_padding_is_rejected() -> None:
    mutated = playground_job().replace(
        '--arg sha "${PLAYGROUND_SHA}"',
        '--arg sha "${DISPATCH_ACTOR}"',
    )
    mutated += '\n# --arg sha "${PLAYGROUND_SHA}"\n'
    try:
        assert_exact_dispatch_correlation(mutated)
    except (AssertionError, ValueError):
        return
    raise AssertionError("a swapped argument was hidden by padding outside its query")


def test_pipeline_status_masking_mutation_is_rejected() -> None:
    mutated = playground_job().replace(
        "            if ! RUNS_JSON=$(gh api -X GET \\",
        "            RUNS_JSON=$(gh api -X GET \\",
    )
    mutated += "\n# if ! RUNS_JSON=$(gh api -X GET\n"
    try:
        assert_fail_closed_run_retrieval(mutated)
    except (AssertionError, ValueError):
        return
    raise AssertionError("an unchecked API retrieval was hidden by padding")


def test_required_downstream_failure_is_not_masked() -> None:
    job = playground_job()
    assert "gh run watch" in job
    assert "continue-on-error" not in job
    assert "skipping playground trigger" not in job


def test_prerelease_policy_uses_selected_release_tag() -> None:
    text = workflow()
    assert "contains(env.RELEASE_TAG, '-rc')" in text
    selected_tag = "github.event.inputs.tag || github.ref_name"

    linux_packages_start = text.index("  linux-packages:\n")
    clean_room_start = text.index("  docker-clean-room-test:\n")
    linux_packages = text[linux_packages_start:clean_room_start]
    assert f"!contains({selected_tag}, '-rc')" in linux_packages

    release_start = text.index("  release:\n")
    docker_start = text.index("  docker:\n")
    release = text[release_start:docker_start]
    assert "prerelease: ${{ contains(env.RELEASE_TAG, '-rc') }}" in release

    homebrew_start = text.index("  homebrew:\n")
    playground_start = text.index("  playground:\n")
    homebrew = text[homebrew_start:playground_start]
    assert "HOMEBREW_TAP_TOKEN" in homebrew
    assert f"!contains({selected_tag}, '-rc')" in homebrew
    assert "PLAYGROUND_DISPATCH_TOKEN" not in homebrew


def test_public_ecosystem_artifacts_follow_canonical_release() -> None:
    text = workflow()
    docker_start = text.index("  docker:\n")
    homebrew_start = text.index("  homebrew:\n")
    docker = text[docker_start:homebrew_start]
    assert "needs: release" in docker
    assert "needs.release.result == 'success'" in docker
    assert 'if [[ "${RELEASE_TAG}" != *-* ]]; then' in docker
    assert "ghcr.io/hew-lang/hew:latest" in docker
    assert "tags: ${{ steps.version.outputs.tags }}" in docker

    vscode_start = text.index("  vscode-extension:\n")
    vscode_publish_start = text.index("  vscode-publish:\n")
    vscode = text[vscode_start:vscode_publish_start]
    assert "needs: release" in vscode
    assert "needs.release.result == 'success'" in vscode


def test_unix_installer_accepts_every_published_freebsd_architecture() -> None:
    workflow_text = workflow()
    installer = UNIX_INSTALLER.read_text()
    assert "hew-v${VERSION}-freebsd-aarch64" in workflow_text
    assert 'aarch64 | arm64) ARCH="aarch64" ;;' in installer
    assert "FreeBSD prebuilt releases are x86_64 only today" not in installer


def test_release_checksums_require_every_platform_asset() -> None:
    text = workflow()
    release_start = text.index("  release:\n")
    docker_start = text.index("  docker:\n")
    release = text[release_start:docker_start]
    for target in (
        "darwin-aarch64.tar.gz",
        "darwin-x86_64.tar.gz",
        "windows-x86_64.zip",
        "linux-x86_64.tar.gz",
        "linux-aarch64.tar.gz",
        "freebsd-x86_64.tar.gz",
        "freebsd-aarch64.tar.gz",
    ):
        assert f'"hew-v${{VERSION}}-{target}"' in release
    assert 'if [ ! -f "${asset}" ]; then' in release
    assert "Missing required release archive" in release
    assert "Final release has no Linux package artifacts" in release
    assert 'sha256sum "${assets[@]}"' in release


def test_prerelease_validator_proves_external_staticlib_linking() -> None:
    validator = PRE_RELEASE_VALIDATOR.read_text()
    probe = RELEASE_LINK_PROBE.read_text()
    windows_probe = WINDOWS_RELEASE_LINK_PROBE.read_text()
    makefile = MAKEFILE.read_text()

    assert "verify_libhew_external_link" in validator
    assert (
        "scripts/test-release-lib-link.sh --hew target/release/hew --archive target/release-lib/libhew.a"
        in validator
    )
    assert "hew.exe build .\\\\_smoke.hew -o .\\\\_smoke.exe" in validator
    assert "ar t target/release-lib" not in validator
    assert "target/release/hew _smoke.hew -o" not in validator
    assert '"$WORK_DIR/release/bin/hew" build' in probe
    assert "--link-lib" in probe
    assert 'String::from("release-link-ok")' in probe
    assert 'String::from("release-link-ok")' in windows_probe
    assert "--link-lib" in windows_probe
    assert "Copy-Item -LiteralPath $Archive" in windows_probe
    assert "& $StagedHew build" in windows_probe
    assert "test-release-lib-link:" in makefile
    assert "--hew $(RELEASE_HEW) --archive $(RELEASE_LIBHEW)" in makefile
    assert "scripts/test-release-lib-link.ps1" in makefile
    assert "RELEASE_HEW := $(RELEASE_DIR)/hew.exe" in makefile
    assert "RELEASE_LIBHEW := $(RELEASE_LIB_DIR)/hew.lib" in makefile


def test_every_release_lane_executes_the_library_consumer_proof() -> None:
    release = workflow()
    gate = RELEASE_GATE.read_text()

    # build matrix: macOS Unix + Windows; Linux matrix; two FreeBSD jobs.
    assert release.count("scripts/test-release-lib-link.sh") == 4
    assert release.count("scripts/test-release-lib-link.ps1") == 1
    # Linux x86_64/aarch64, macOS, Windows, and both FreeBSD gate jobs.
    assert gate.count("scripts/test-release-lib-link.sh") == 4
    assert gate.count("scripts/test-release-lib-link.ps1") == 1
    assert gate.count("make test-release-lib-link") == 1
    for text in (release, gate):
        assert "ar t " not in text
        assert "llvm-ar t " not in text


def test_freebsd_release_lanes_provision_bash_and_package_with_posix_sh() -> None:
    release = workflow()
    gate = RELEASE_GATE.read_text()
    assert release.count("git bash pkgconf") == 2
    assert gate.count("git bash pkgconf") == 2
    assert release.count("command -v bash") == 2
    assert gate.count("command -v bash") == 2
    assert release.count("bash scripts/test-release-lib-link.sh") == 2
    assert gate.count("bash scripts/test-release-lib-link.sh") == 2

    for job_name in ("  build-freebsd:\n", "  build-freebsd-aarch64:\n"):
        start = release.index(job_name)
        next_job = release.find("\n  # ──", start + len(job_name))
        block = release[start : next_job if next_job != -1 else len(release)]
        assert "if [[ " not in block


def test_sanitizer_gate_is_behavioral_and_release_scoped() -> None:
    validator = SANITIZER_GATE.read_text()
    gate = RELEASE_GATE.read_text()
    ledger = (ROOT / "release-sanitizer-waiver.toml").read_text()

    assert "<release-version>" in validator
    assert "release_version" in validator
    assert "behavior" in validator
    assert "reason" in validator
    assert "tracking" in validator
    assert "owner" in validator
    assert "expires" in validator
    assert "git " not in validator
    assert "commit" not in validator
    assert "SHA" not in validator
    assert "GITHUB_SHA" not in gate
    assert 'scripts/check-sanitizer-gate.sh "${RELEASE_VERSION}"' in gate
    for field in (
        "release =",
        "behavior =",
        "reason =",
        "tracking =",
        "owner =",
        "expires =",
    ):
        assert ledger.count(field) >= 2


def test_release_notes_and_runbook_keep_candidate_truthful() -> None:
    notes = RELEASE_NOTES.read_text()
    runbook = RUNBOOK.read_text()
    assert "v0.6.0-rc1" in notes
    assert "not a final release" in notes
    assert "every release bar and the final-candidate checklist are green" in runbook
    assert "Manually dispatch" in runbook
    assert "both independent publication arms" in runbook
    assert "Only after both arms are green" in runbook
    assert "Homebrew" in runbook and "prerelease" in runbook
    assert "publish-npm-packages.yml" in runbook


def test_contract_oracle_runs_in_required_ci() -> None:
    ci = CI_WORKFLOW.read_text()
    assert "'.github/workflows/release.yml'" in ci
    assert "make test-release-workflow-contract" in ci


def _write_release_binary_smoke_double(path: Path) -> None:
    """Emit the narrow CLI surface the --no-build smoke path uses."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        """#!/usr/bin/env bash
set -euo pipefail
if [[ "${1:-}" != "compile" || "${2:-}" != "hello_int.hew" ]]; then
    echo "unexpected release-smoke invocation: $*" >&2
    exit 91
fi
mkdir -p .tmp/compile-out
printf '%s\\n' '#!/usr/bin/env bash' 'exit 42' > .tmp/compile-out/hello_int
chmod +x .tmp/compile-out/hello_int
"""
    )
    path.chmod(0o755)


def _run_release_binary_target_dir_contract(target_dir: Path, env_value: str) -> None:
    _write_release_binary_smoke_double(target_dir / "release" / "hew")
    env = os.environ.copy()
    env["CARGO_TARGET_DIR"] = env_value
    result = subprocess.run(
        ["bash", str(RELEASE_BINARY_SMOKE), "--no-build"],
        cwd=ROOT,
        env=env,
        check=False,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "PASS: release binary compiled fixture" in result.stdout


def test_release_binary_smoke_honors_absolute_and_relative_target_dirs() -> None:
    """The artifact probe must follow the target root Cargo was given."""
    with tempfile.TemporaryDirectory(prefix="hew-release-smoke-absolute-") as absolute:
        absolute_target = Path(absolute)
        _run_release_binary_target_dir_contract(absolute_target, str(absolute_target))

    with tempfile.TemporaryDirectory(
        prefix=".tmp-release-smoke-relative-", dir=ROOT
    ) as relative:
        relative_target = Path(relative)
        _run_release_binary_target_dir_contract(
            relative_target, str(relative_target.relative_to(ROOT))
        )


_TESTS = [
    test_rc_tag_normalization_and_exact_release_body,
    test_npm_publication_is_pinned_to_a_version_matching_release_tag,
    test_playground_dispatch_is_purpose_scoped_and_fail_closed,
    test_dispatch_uses_exact_playground_workflow_input_and_ref,
    test_dispatch_correlation_is_unique_and_bounded,
    test_exact_workflow_shell_accepts_one_stable_matching_run,
    test_malformed_release_commit_identity_is_terminal,
    test_run_listing_api_failure_is_terminal,
    test_run_listing_jq_failure_is_terminal,
    test_successful_empty_polls_exhaust_the_bound,
    test_ambiguous_correlation_fails_on_the_first_poll,
    test_each_exact_run_identity_dimension_is_mandatory,
    test_timeout_undercut_mutation_is_rejected,
    test_publish_mode_downgrade_mutation_is_rejected,
    test_correlation_swap_with_padding_is_rejected,
    test_correlation_argument_swap_with_padding_is_rejected,
    test_pipeline_status_masking_mutation_is_rejected,
    test_required_downstream_failure_is_not_masked,
    test_prerelease_policy_uses_selected_release_tag,
    test_public_ecosystem_artifacts_follow_canonical_release,
    test_unix_installer_accepts_every_published_freebsd_architecture,
    test_release_checksums_require_every_platform_asset,
    test_prerelease_validator_proves_external_staticlib_linking,
    test_every_release_lane_executes_the_library_consumer_proof,
    test_freebsd_release_lanes_provision_bash_and_package_with_posix_sh,
    test_sanitizer_gate_is_behavioral_and_release_scoped,
    test_release_notes_and_runbook_keep_candidate_truthful,
    test_contract_oracle_runs_in_required_ci,
    test_release_binary_smoke_honors_absolute_and_relative_target_dirs,
]


if __name__ == "__main__":
    failures = 0
    for test in _TESTS:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(_TESTS)} tests failed")
    print(f"All {len(_TESTS)} tests passed.")
