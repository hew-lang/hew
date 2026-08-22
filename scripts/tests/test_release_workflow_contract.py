"""Static contract tests for the release workflow's prerelease handoff."""

import json
import os
import re
import shutil
import stat
import subprocess
import tarfile
import tempfile
import textwrap
import tomllib
from contextlib import contextmanager
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from threading import Thread
from time import time
from urllib.parse import urlparse


ROOT = Path(__file__).resolve().parents[2]
HEW_SHA = "0123456789abcdef0123456789abcdef01234567"
WORKSPACE_MANIFEST = ROOT / "Cargo.toml"
SANDBOX_VM_MANIFEST = ROOT / "hew-sandbox-vm" / "package.json"
SANDBOX_VM_LOCKFILE = ROOT / "hew-sandbox-vm" / "package-lock.json"
WORKFLOW = ROOT / ".github" / "workflows" / "release.yml"
RUST_TOOLCHAIN = ROOT / "rust-toolchain.toml"
NPM_PUBLISH_WORKFLOW = ROOT / ".github" / "workflows" / "publish-npm-packages.yml"
RELEASE_GATE = ROOT / ".github" / "workflows" / "release-gate.yml"
CI_WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
COVERAGE_NIGHTLY_WORKFLOW = ROOT / ".github" / "workflows" / "coverage-nightly.yml"
RELEASE_NOTES = ROOT / "docs" / "releases" / "v0.6.0-rc1.md"
RUNBOOK = ROOT / "docs" / "release-runbook.md"
CHANGELOG = ROOT / "CHANGELOG.md"
UNIX_INSTALLER = ROOT / "installers" / "install.sh"
PRE_RELEASE_VALIDATOR = ROOT / "scripts" / "pre-release-validate.sh"
RELEASE_LINK_PROBE = ROOT / "scripts" / "test-release-lib-link.sh"
RELEASE_LINK_FIXTURE = (
    ROOT / "scripts" / "fixtures" / "release-lib-link" / "src" / "lib.rs"
)
RELEASE_LINK_MANIFEST = (
    ROOT / "scripts" / "fixtures" / "release-lib-link" / "Cargo.toml"
)
WINDOWS_RELEASE_LINK_PROBE = ROOT / "scripts" / "test-release-lib-link.ps1"
WINDOWS_RELEASE_BUILD = ROOT / "scripts" / "windows-release-build.ps1"
SANITIZER_GATE = ROOT / "scripts" / "check-sanitizer-gate.sh"
RELEASE_IMAGE_ASSERTION = ROOT / "scripts" / "assert-playground-release-image.sh"
MAKEFILE = ROOT / "Makefile"
RELEASE_BINARY_SMOKE = ROOT / "scripts" / "test-release-binary.sh"
PACKAGE_BUILDER = ROOT / "installers" / "build-packages.sh"
WINDOWS_LLVM_PREBUILD = ROOT / ".github" / "workflows" / "prebuild-llvm.yml"
SETUP_LLVM_ACTION = ROOT / ".github" / "actions" / "setup-llvm" / "action.yml"
SETUP_WASM_PACK_ACTION = ROOT / ".github" / "actions" / "setup-wasm-pack" / "action.yml"
DOWNLOAD_VERIFY_BINARYEN = ROOT / ".github" / "scripts" / "download-verify-binaryen.sh"
NPM_PACKAGE_BUILDER = ROOT / "scripts" / "build-npm-packages.mjs"
WINDOWS_BUILD_GUIDE = ROOT / "docs" / "cross-platform-build-guide.md"
WINDOWS_LLVM_TOOLCHAIN_REPO = "hew-lang/llvm-toolchain"
WINDOWS_LLVM_TOOLCHAIN_VERSION = "22.1.0-windows-msvc-v1"
WINDOWS_LLVM_TOOLCHAIN_TAG = f"llvm-{WINDOWS_LLVM_TOOLCHAIN_VERSION}"
WINDOWS_LLVM_TOOLCHAIN_ASSET = f"hew-llvm-{WINDOWS_LLVM_TOOLCHAIN_VERSION}.tar.gz"
BINARYEN_SHA256 = "3dc677006555b355ea2da5e82602065a161d5e83eaefd3f759afa00b96e83212"


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
    """Extract the exact Bash program executed by the dispatch step.

    Bounded at the next step so the acquisition-mode steps that follow the
    dispatch cannot leak their YAML into the program under test.
    """
    job = playground_job() if text is None else text
    step = job.index("      - name: Trigger playground image rebuild\n")
    run = job.index("        run: |\n", step) + len("        run: |\n")
    next_step = job.find("\n      - name: ", run)
    body = job[run:] if next_step == -1 else job[run : next_step + 1]
    return textwrap.dedent(body).rstrip() + "\n"


def step_of(job: str, name: str) -> str:
    """Extract one named step's YAML from a job, bounded at the next step."""
    start = job.index(f"      - name: {name}\n")
    end = job.find("\n      - name: ", start + 1)
    return job[start:] if end == -1 else job[start : end + 1]


def acquisition_script() -> str:
    """Extract the exact Bash program that selects the acquisition mode."""
    job = playground_job()
    step = step_of(job, "Select release image acquisition mode")
    run = step.index("        run: |\n") + len("        run: |\n")
    return textwrap.dedent(step[run:]).rstrip() + "\n"


def run_acquisition(mode: str | None) -> tuple:
    """Execute the acquisition-mode selection with a given repository variable."""
    with tempfile.TemporaryDirectory() as directory:
        output = Path(directory) / "github_output"
        output.touch()
        env = os.environ.copy()
        env["GITHUB_OUTPUT"] = str(output)
        env.pop("PLAYGROUND_PUBLISH_MODE", None)
        if mode is not None:
            env["PLAYGROUND_PUBLISH_MODE"] = mode
        result = subprocess.run(
            ["bash", "-c", acquisition_script()],
            cwd=ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
        )
        outputs = dict(
            line.split("=", 1)
            for line in output.read_text().splitlines()
            if "=" in line
        )
        return result, outputs


def run_release_image_assertion(env_overrides: dict) -> subprocess.CompletedProcess:
    """Execute the release-image assertion with a fixed environment."""
    env = os.environ.copy()
    for key in (
        "IMAGE_REPOSITORY",
        "IMAGE_TAG",
        "EXPECTED_REVISION",
        "GHCR_TOKEN",
        "GHCR_USERNAME",
        "DEADLINE_MINUTES",
        "DEADLINE_EPOCH",
        "IMAGE_REGISTRY",
        "IMAGE_REGISTRY_SCHEME",
        "POLL_INTERVAL_SECONDS",
    ):
        env.pop(key, None)
    env.update(env_overrides)
    return subprocess.run(
        [str(RELEASE_IMAGE_ASSERTION)],
        cwd=ROOT,
        env=env,
        check=False,
        capture_output=True,
        text=True,
    )


@contextmanager
def canned_registry(responses: dict[str, list[tuple[int, dict]]]):
    requests: list[str] = []
    counts: dict[str, int] = {}

    class Handler(BaseHTTPRequestHandler):
        def do_GET(self) -> None:
            path = urlparse(self.path).path
            requests.append(path)
            sequence = responses.get(path, [(404, {"error": "not found"})])
            index = counts.get(path, 0)
            counts[path] = index + 1
            status, body = sequence[min(index, len(sequence) - 1)]
            encoded = json.dumps(body).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(encoded)))
            self.end_headers()
            self.wfile.write(encoded)

        def log_message(self, _format: str, *_args: object) -> None:
            pass

    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = Thread(target=server.serve_forever)
    thread.start()
    try:
        yield server.server_address[1], requests
    finally:
        server.shutdown()
        thread.join()
        server.server_close()


def image_manifest(config_digest: str) -> dict:
    return {
        "schemaVersion": 2,
        "mediaType": "application/vnd.oci.image.manifest.v1+json",
        "config": {
            "mediaType": "application/vnd.oci.image.config.v1+json",
            "digest": config_digest,
            "size": 1,
        },
        "layers": [],
    }


def image_config(revision: str) -> dict:
    return {
        "architecture": "amd64",
        "os": "linux",
        "config": {"Labels": {"org.opencontainers.image.revision": revision}},
    }


def run_canned_release_image_assertion(
    responses: dict[str, list[tuple[int, dict]]],
) -> tuple[subprocess.CompletedProcess, list[str]]:
    responses = {
        "/token": [(200, {"token": "pull-token"})],
        **responses,
    }
    with canned_registry(responses) as (port, requests):
        result = run_release_image_assertion(
            {
                "IMAGE_REPOSITORY": "hew-lang/playground",
                "IMAGE_TAG": "v0.6.0-rc2",
                "EXPECTED_REVISION": HEW_SHA,
                "GHCR_TOKEN": "test-token",
                "GHCR_USERNAME": "test-user",
                "DEADLINE_MINUTES": "1",
                "IMAGE_REGISTRY": f"127.0.0.1:{port}",
                "IMAGE_REGISTRY_SCHEME": "http",
                "POLL_INTERVAL_SECONDS": "1",
            }
        )
    return result, requests


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
    """Keep the shell's budget authority equal to the enforced job timeout."""
    match = re.search(r"^    timeout-minutes: (\d+)$", job, re.MULTILINE)
    assert match is not None
    shell_budget = re.search(r"^\s+JOB_TIMEOUT_MINUTES=(\d+)$", job, re.MULTILINE)
    assert shell_budget is not None
    assert int(match.group(1)) == int(shell_budget.group(1))
    assert (
        "ASSERTION_BUDGET_MINUTES=$((JOB_TIMEOUT_MINUTES - FINALIZATION_RESERVE_MINUTES))"
        in job
    )


def test_rc_tag_normalization_and_exact_release_body() -> None:
    text = workflow()
    assert "RELEASE_TAG: ${{ github.event.inputs.tag || github.ref_name }}" in text
    assert 'VERSION="${RELEASE_TAG#v}"' in playground_job()
    assert "body_path: docs/releases/${{ env.RELEASE_TAG }}.md" in text
    assert RELEASE_NOTES.exists()


def test_release_tag_must_match_cargo_version_before_build() -> None:
    text = workflow()
    start = text.index("  validate-release-version:\n")
    end = text.index(
        "  # ─────────────────────────────────────────────────────────────────────────\n",
        start,
    )
    job = text[start:end]
    assert "ref: ${{ env.RELEASE_TAG }}" in job
    assert "cargo_version=" in job
    assert 'expected_tag="v${cargo_version}"' in job
    assert 'if [[ "${RELEASE_TAG}" != "${expected_tag}" ]]' in job
    assert "refusing to build" in job
    assert (
        "needs: validate-release-version"
        in text[text.index("  build-cross-release-libs:") :]
    )
    assert "needs: validate-release-version" in text[text.index("  build-linux:") :]


def test_npm_publication_is_pinned_to_a_version_matching_release_tag() -> None:
    text = npm_publish_workflow()
    assert "      release_tag:\n" in text
    assert "Immutable release tag to publish" in text
    assert "        required: true\n" in text
    assert "        type: string\n" in text
    assert "  group: publish-npm-packages-${{ inputs.release_tag }}" in text
    assert "          ref: ${{ inputs.release_tag }}" in text
    assert "          fetch-depth: 0" in text
    assert (
        "      - name: Verify immutable release identity and package versions\n" in text
    )
    assert "RELEASE_TAG: ${{ inputs.release_tag }}" in text
    assert "^v[0-9]+\\.[0-9]+\\.[0-9]+(-[0-9A-Za-z.-]+)?$" in text
    assert 'git show-ref --verify --quiet "refs/tags/${RELEASE_TAG}"' in text
    assert 'git rev-parse "refs/tags/${RELEASE_TAG}^{commit}"' in text
    assert 'if [ "${TAG_COMMIT}" != "${HEAD_COMMIT}" ]; then' in text
    assert 'EXPECTED_VERSION="${RELEASE_TAG#v}"' in text
    assert (
        "Cargo.toml version ${WORKSPACE_VERSION} does not match ${RELEASE_TAG}" in text
    )
    assert (
        "hew-sandbox-vm version ${SANDBOX_VM_VERSION} does not match ${RELEASE_TAG}"
        in text
    )
    assert 'if [[ "${EXPECTED_VERSION}" == *-* ]]; then' in text
    assert "NPM_DIST_TAG=next" in text
    assert "NPM_DIST_TAG=latest" in text
    assert 'echo "NPM_DIST_TAG=${NPM_DIST_TAG}" >> "${GITHUB_ENV}"' in text
    publish_lines = [
        line.strip()
        for line in text.splitlines()
        if line.strip().startswith('npm publish "${PKG_DIR}"')
    ]
    assert len(publish_lines) == 3
    assert all('--tag "${NPM_DIST_TAG}"' in line for line in publish_lines)


def test_current_sandbox_vm_version_matches_workspace_version() -> None:
    workspace_version = tomllib.loads(WORKSPACE_MANIFEST.read_text())["workspace"][
        "package"
    ]["version"]
    sandbox_version = json.loads(SANDBOX_VM_MANIFEST.read_text())["version"]
    lockfile = json.loads(SANDBOX_VM_LOCKFILE.read_text())

    assert sandbox_version == workspace_version, (
        f"hew-sandbox-vm package version {sandbox_version} does not match "
        f"workspace version {workspace_version}"
    )
    assert lockfile["version"] == workspace_version
    assert lockfile["packages"][""]["version"] == workspace_version


def test_playground_dispatch_is_purpose_scoped_and_fail_closed() -> None:
    text = workflow()
    job = playground_job()
    assert "      - name: Resolve release commit identity\n" in job
    assert (
        "gh api \"repos/${GITHUB_REPOSITORY}/commits/${RELEASE_TAG}\" --jq '.sha'"
    ) in job
    assert "did not resolve to an exact lowercase" in job
    assert "HEW_SHA: ${{ steps.release-commit.outputs.hew_sha }}" in job
    assert "PLAYGROUND_DISPATCH_TOKEN" not in text
    assert "HOMEBREW_TAP_TOKEN" not in job
    assert "#   secrets.PLAYGROUND_APP_ID" in text
    assert "#   secrets.PLAYGROUND_APP_PRIVATE_KEY" in text

    validate = job.index("      - name: Validate playground GitHub App configuration\n")
    mint = job.index("      - name: Mint playground GitHub App token\n")
    trigger = job.index("      - name: Trigger playground image rebuild\n")
    assert validate < mint < trigger
    validation_step = job[validate:mint]
    token_step = job[mint:trigger]
    trigger_step = job[trigger:]

    assert "PLAYGROUND_APP_ID: ${{ secrets.PLAYGROUND_APP_ID }}" in validation_step
    assert (
        "PLAYGROUND_APP_PRIVATE_KEY: ${{ secrets.PLAYGROUND_APP_PRIVATE_KEY }}"
        in validation_step
    )
    assert (
        "requires secrets.PLAYGROUND_APP_ID and secrets.PLAYGROUND_APP_PRIVATE_KEY"
        in validation_step
    )
    assert "exit 1" in validation_step
    assert "if: steps.acquisition.outputs.mode == 'actions'" in validation_step
    assert "continue-on-error:" not in validation_step

    assert "id: playground-app-token" in token_step
    assert re.search(
        r"uses: actions/create-github-app-token@[0-9a-f]{40}  # v2\.2\.2$",
        token_step,
        re.MULTILINE,
    )
    assert "app-id: ${{ secrets.PLAYGROUND_APP_ID }}" in token_step
    assert "private-key: ${{ secrets.PLAYGROUND_APP_PRIVATE_KEY }}" in token_step
    assert "owner: hew-lang" in token_step
    assert "repositories: playground" in token_step
    assert "if: steps.acquisition.outputs.mode == 'actions'" in token_step
    assert "continue-on-error:" not in token_step
    assert "GH_TOKEN: ${{ steps.playground-app-token.outputs.token }}" in trigger_step
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


def test_release_image_acquisition_mode_is_exhaustive_and_fails_closed() -> None:
    for mode in ("actions", "local"):
        before = int(time())
        result, outputs = run_acquisition(mode)
        after = int(time())
        assert result.returncode == 0, result.stderr
        assert outputs["mode"] == mode
        assert outputs["assert_budget_minutes"] == "115"
        deadline = int(outputs["assert_deadline_epoch"])
        assert before + 115 * 60 <= deadline <= after + 115 * 60

    for rejected in (None, "", "Actions", "LOCAL", "skip", "true", "actions local"):
        result, outputs = run_acquisition(rejected)
        assert result.returncode != 0, repr(rejected)
        assert "must be exactly 'actions' or 'local'" in result.stdout, repr(rejected)
        assert outputs == {}, repr(rejected)


def test_release_image_assertion_covers_every_acquisition_mode() -> None:
    job = playground_job()
    mode_step = step_of(job, "Select release image acquisition mode")
    assert "id: acquisition" in mode_step
    assert "PLAYGROUND_PUBLISH_MODE: ${{ vars.PLAYGROUND_PUBLISH_MODE }}" in mode_step
    assert "if:" not in mode_step
    assert "continue-on-error:" not in mode_step

    trigger_step = step_of(job, "Trigger playground image rebuild")
    assert "if: steps.acquisition.outputs.mode == 'actions'" in trigger_step

    local_step = step_of(job, "Report expected local release image publish")
    assert "if: steps.acquisition.outputs.mode == 'local'" in local_step
    assert "gh workflow run" not in local_step

    assert_step = step_of(
        job, "Assert release image exists and binds the release commit"
    )
    assert (
        "if: ${{ !cancelled() && steps.acquisition.outputs.mode != '' }}" in assert_step
    )
    assert "continue-on-error:" not in assert_step
    assert "IMAGE_REPOSITORY: hew-lang/playground" in assert_step
    assert "IMAGE_TAG: ${{ env.RELEASE_TAG }}" in assert_step
    assert (
        "EXPECTED_REVISION: ${{ steps.release-commit.outputs.hew_sha }}" in assert_step
    )
    assert "GHCR_TOKEN: ${{ github.token }}" in assert_step
    assert "GHCR_USERNAME: ${{ github.actor }}" in assert_step
    assert (
        "DEADLINE_EPOCH: ${{ steps.acquisition.outputs.assert_deadline_epoch }}"
        in assert_step
    )
    assert "packages: read" in job
    assert "run: release-machinery/scripts/assert-playground-release-image.sh" in (
        assert_step
    )

    machinery_step = step_of(job, "Checkout release machinery")
    assert "ref: ${{ github.sha }}" in machinery_step
    assert "path: release-machinery" in machinery_step

    assert RELEASE_IMAGE_ASSERTION.exists()
    assert RELEASE_IMAGE_ASSERTION.stat().st_mode & stat.S_IXUSR


def test_release_image_assertion_rejects_unusable_inputs() -> None:
    valid = {
        "IMAGE_REPOSITORY": "hew-lang/playground",
        "IMAGE_TAG": "v0.6.0-rc2",
        "EXPECTED_REVISION": HEW_SHA,
        "GHCR_TOKEN": "test-token",
        "GHCR_USERNAME": "test-user",
        "DEADLINE_MINUTES": "1",
    }
    # Each case is rejected before any network call, so the assertion can never
    # reach a registry with an identity it has not fully resolved.
    cases = {
        "IMAGE_REPOSITORY": "",
        "IMAGE_TAG": "",
        "GHCR_TOKEN": "",
    }
    for key, value in cases.items():
        env = dict(valid, **{key: value})
        result = run_release_image_assertion(env)
        assert result.returncode != 0, key
        assert f"{key} must be set" in result.stderr, key

    without_deadline = {
        key: value for key, value in valid.items() if key != "DEADLINE_MINUTES"
    }
    result = run_release_image_assertion(without_deadline)
    assert result.returncode != 0
    assert "DEADLINE_EPOCH or DEADLINE_MINUTES must be set" in result.stderr

    for bad_sha in ("not-a-sha", HEW_SHA[:39], HEW_SHA.upper()):
        result = run_release_image_assertion(dict(valid, EXPECTED_REVISION=bad_sha))
        assert result.returncode != 0, bad_sha
        assert "exact lowercase 40-character commit SHA" in result.stderr, bad_sha

    result = run_release_image_assertion(dict(valid, DEADLINE_MINUTES="soon"))
    assert result.returncode != 0
    assert "whole number of minutes" in result.stderr

    result = run_release_image_assertion(dict(valid, POLL_INTERVAL_SECONDS="0"))
    assert result.returncode != 0
    assert "POLL_INTERVAL_SECONDS must be a positive whole number" in result.stderr


def test_release_image_assertion_accepts_matching_single_manifest() -> None:
    config_digest = "sha256:config-good"
    result, requests = run_canned_release_image_assertion(
        {
            "/v2/hew-lang/playground/manifests/v0.6.0-rc2": [
                (200, image_manifest(config_digest))
            ],
            f"/v2/hew-lang/playground/blobs/{config_digest}": [
                (200, image_config(HEW_SHA))
            ],
        }
    )
    assert result.returncode == 0, result.stderr
    assert requests.count("/token") == 1


def test_release_image_assertion_polls_past_a_stale_tag() -> None:
    stale_digest = "sha256:config-stale"
    good_digest = "sha256:config-good"
    tag_path = "/v2/hew-lang/playground/manifests/v0.6.0-rc2"
    result, requests = run_canned_release_image_assertion(
        {
            tag_path: [
                (200, image_manifest(stale_digest)),
                (200, image_manifest(good_digest)),
            ],
            f"/v2/hew-lang/playground/blobs/{stale_digest}": [
                (200, image_config("f" * 40))
            ],
            f"/v2/hew-lang/playground/blobs/{good_digest}": [
                (200, image_config(HEW_SHA))
            ],
        }
    )
    assert result.returncode == 0, result.stderr
    assert requests.count(tag_path) == 2
    assert "still binds" in result.stderr


def test_release_image_assertion_checks_every_platform_in_an_index() -> None:
    child_digests = ("sha256:manifest-amd64", "sha256:manifest-arm64")
    config_digests = ("sha256:config-amd64", "sha256:config-arm64")
    attestation_digest = "sha256:attestation"
    index = {
        "schemaVersion": 2,
        "mediaType": "application/vnd.oci.image.index.v1+json",
        "manifests": [
            {
                "digest": child_digests[0],
                "platform": {"os": "linux", "architecture": "amd64"},
            },
            {
                "digest": child_digests[1],
                "platform": {"os": "linux", "architecture": "arm64"},
            },
            {
                "digest": attestation_digest,
                "annotations": {"vnd.docker.reference.type": "attestation-manifest"},
            },
        ],
    }
    result, requests = run_canned_release_image_assertion(
        {
            "/v2/hew-lang/playground/manifests/v0.6.0-rc2": [(200, index)],
            f"/v2/hew-lang/playground/manifests/{child_digests[0]}": [
                (200, image_manifest(config_digests[0]))
            ],
            f"/v2/hew-lang/playground/manifests/{child_digests[1]}": [
                (200, image_manifest(config_digests[1]))
            ],
            f"/v2/hew-lang/playground/blobs/{config_digests[0]}": [
                (200, image_config(HEW_SHA))
            ],
            f"/v2/hew-lang/playground/blobs/{config_digests[1]}": [
                (200, image_config(HEW_SHA))
            ],
        }
    )
    assert result.returncode == 0, result.stderr
    assert f"/v2/hew-lang/playground/manifests/{attestation_digest}" not in requests


def test_release_image_assertion_rejects_unclassified_platformless_child() -> None:
    index = {
        "schemaVersion": 2,
        "mediaType": "application/vnd.oci.image.index.v1+json",
        "manifests": [{"digest": "sha256:unclassified"}],
    }
    result, _requests = run_canned_release_image_assertion(
        {"/v2/hew-lang/playground/manifests/v0.6.0-rc2": [(200, index)]}
    )
    assert result.returncode != 0
    assert "has no platform.os and is not marked as an attestation" in result.stderr


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
    windows_build = WINDOWS_RELEASE_BUILD.read_text()
    probe = RELEASE_LINK_PROBE.read_text()
    fixture = RELEASE_LINK_FIXTURE.read_text()
    fixture_manifest = RELEASE_LINK_MANIFEST.read_text()
    windows_probe = WINDOWS_RELEASE_LINK_PROBE.read_text()
    makefile = MAKEFILE.read_text()

    assert "verify_libhew_external_link" in validator
    assert "scripts/cargo-output-dir.py --profile release" in validator
    assert "scripts/cargo-output-dir.py --profile release-lib" in validator
    assert r"--hew \"\$release_dir/hew\"" in validator
    assert r"--archive \"\$release_lib_dir/libhew.a\"" in validator
    assert "& $Hew build $SmokeSource -o $SmokeOutput" in windows_build
    assert "ar t target/release-lib" not in validator
    assert "target/release/hew _smoke.hew -o" not in validator
    assert '"$WORK_DIR/release/bin/hew" build' in probe
    assert "--link-lib" in probe
    assert '"$SCRIPT_DIR/fixtures/release-lib-link/src/lib.rs"' in probe
    assert "--consumer-archive" in probe
    assert 'String::from("release-link-ok")' in fixture
    assert 'crate-type = ["staticlib"]' in fixture_manifest
    assert 'panic = "abort"' in fixture_manifest
    assert "codegen-units = 1" in fixture_manifest
    assert 'String::from("release-link-ok")' in windows_probe
    assert "--link-lib" in windows_probe
    assert "Copy-Item -LiteralPath $Archive" in windows_probe
    assert "& $StagedHew build" in windows_probe
    assert "test-release-lib-link:" in makefile
    assert '--hew "$(RELEASE_HEW)" --archive "$(RELEASE_LIBHEW)"' in makefile
    assert "scripts/test-release-lib-link.ps1" in makefile
    assert "RELEASE_HEW := $(RELEASE_DIR)/hew.exe" in makefile
    assert "RELEASE_LIBHEW := $(RELEASE_LIB_DIR)/hew.lib" in makefile


def test_every_release_lane_executes_the_library_consumer_proof() -> None:
    release = workflow()
    gate = RELEASE_GATE.read_text()

    # build matrix: macOS Unix + Windows; Linux matrix; two FreeBSD jobs.
    assert release.count("scripts/test-release-lib-link.sh") == 4
    assert release.count("scripts/test-release-lib-link.ps1") == 1
    # Linux x86_64/aarch64, macOS, Windows, and the FreeBSD x86_64 gate job.
    # The FreeBSD aarch64 gate leg is intentionally scoped to build+smoke
    # (lib-link coverage is retained on freebsd-x86_64 and linux-aarch64).
    assert gate.count("scripts/test-release-lib-link.sh") == 3
    assert gate.count("scripts/test-release-lib-link.ps1") == 1
    assert gate.count("make test-release-lib-link") == 1
    for text in (release, gate):
        assert "ar t " not in text
        assert "llvm-ar t " not in text


def test_cross_release_machinery_resolves_from_workflow_ref() -> None:
    text = workflow()
    start = text.index("  build-cross-release-libs:\n")
    end = text.index("  # Build — macOS and Windows release artifacts\n", start)
    job = text[start:end]
    assert (
        """      - name: Checkout release machinery
        uses: actions/checkout@df4cb1c069e1874edd31b4311f1884172cec0e10  # v6.0.3
        with:
          ref: ${{ github.sha }}
          path: release-machinery
"""
        in job
    )
    assert (
        "release-machinery/scripts/verify-cross-release-lib.sh "
        '"${{ matrix.rust_target }}" "${archive}"' in job
    )
    unscoped_script_references = [
        line.strip()
        for line in job.splitlines()
        if re.search(r"(?<!release-machinery/)scripts/", line)
    ]
    assert not unscoped_script_references, (
        "cross-release scripts must resolve from the workflow ref: "
        f"{unscoped_script_references}"
    )


def test_npm_publish_machinery_resolves_from_workflow_ref() -> None:
    job = workflow_job(npm_publish_workflow(), "publish")
    assert (
        """      - name: Checkout release machinery
        uses: actions/checkout@df4cb1c069e1874edd31b4311f1884172cec0e10  # v6.0.3
        with:
          ref: ${{ github.sha }}
          path: release-machinery
"""
        in job
    )
    assert "uses: ./release-machinery/.github/actions/setup-wasm-pack" in job
    assert "node release-machinery/scripts/build-npm-packages.mjs" in job
    assert "HEW_SOURCE_ROOT: ${{ github.workspace }}" in job

    unscoped_machinery_references = [
        line.strip()
        for line in job.splitlines()
        if re.search(r"(?<!release-machinery/)scripts/", line)
        or re.search(r"uses:\s+\./(?!release-machinery/)", line)
    ]
    assert not unscoped_machinery_references, (
        "npm publish machinery must resolve from the workflow ref: "
        f"{unscoped_machinery_references}"
    )

    builder = NPM_PACKAGE_BUILDER.read_text()
    assert "process.env.HEW_SOURCE_ROOT ?? SCRIPT_REPO_ROOT" in builder


def test_cross_release_libraries_are_target_keyed_and_natively_proved() -> None:
    release = workflow()
    cross_start = release.index("  build-cross-release-libs:\n")
    build_start = release.index("  build:\n", cross_start)
    freebsd_start = release.index("  build-freebsd:\n", build_start)
    freebsd_aarch64_start = release.index("  build-freebsd-aarch64:\n", freebsd_start)
    linux_packages_start = release.index("  linux-packages:\n", freebsd_aarch64_start)

    cross = release[cross_start:build_start]
    build = release[build_start:freebsd_start]
    freebsd = release[freebsd_start:freebsd_aarch64_start]
    freebsd_aarch64 = release[freebsd_aarch64_start:linux_packages_start]
    verifier = (ROOT / "scripts" / "verify-cross-release-lib.sh").read_text()

    assert 'toolchain: "1.96.0"' in cross
    assert "version: 0.16.0" in cross
    assert "cargo install cargo-zigbuild --locked --version 0.22.3" in cross
    assert "cargo install cargo-xwin --locked --version 0.23.0" in cross
    assert (
        "cargo zigbuild -p hew-lib --profile release-lib --target"
        " ${{ matrix.rust_target }}" in cross
    )
    assert (
        "cargo xwin build -p hew-lib --profile release-lib --target"
        " ${{ matrix.rust_target }}" in cross
    )
    assert 'AWS_LC_SYS_PREBUILT_NASM: "1"' in cross
    assert "components: rust-src" in cross
    assert cross.count('RUSTC_BOOTSTRAP: "1"') == 2
    assert (
        "cargo zigbuild -Zbuild-std=std,panic_abort -p hew-lib"
        " --profile release-lib --target ${{ matrix.rust_target }}" in cross
    )
    assert "      - name: Build aarch64 FreeBSD release consumer" in cross
    assert (
        "--manifest-path "
        "release-machinery/scripts/fixtures/release-lib-link/Cargo.toml" in cross
    )
    assert "cross-release-consumer-target" in cross
    assert "libhew_release_link_probe.a" in cross
    assert 'zig cc -target "${{ matrix.zig_target }}"' in cross
    assert "for FreeBSD 14.0 (1400500)" in cross
    assert 'tee -a "${GITHUB_STEP_SUMMARY}"' in cross
    assert "name: libhew-${{ matrix.rust_target }}" in cross
    assert "cross-release-libs/${{ matrix.rust_target }}" in cross
    assert "if-no-files-found: error" in cross

    for target in (
        "x86_64-unknown-freebsd",
        "aarch64-unknown-freebsd",
        "x86_64-pc-windows-msvc",
    ):
        assert target in cross
    for required in (
        "llvm-readobj --file-headers",
        "llvm-nm --defined-only --extern-only",
        "expected exactly one release archive",
        "hew_alloc",
        "OS/ABI: ${expected_os_abi}",
        "elf64-littleaarch64",
    ):
        assert required in verifier

    native_lib_step = build[
        build.index("      - name: Build libhew.a (runtime + stdlib)\n") : build.index(
            "      - name: Prove release library consumer linking",
            build.index("      - name: Build libhew.a (runtime + stdlib)\n"),
        )
    ]
    assert "if: startsWith(matrix.target, 'darwin')" in native_lib_step
    assert '-Archive "cross-release-libs/${{ matrix.rust_target }}/hew.lib"' in build
    assert 'Copy-Item "${ReleaseLibDir}/hew.lib"' in build
    assert '"${ArchiveName}/lib/${{ matrix.rust_target }}"' in build

    assert "needs: build-cross-release-libs" in freebsd
    assert "name: libhew-x86_64-unknown-freebsd" in freebsd
    assert "cargo build -p hew-lib --profile release-lib" not in freebsd
    assert "--archive cross-release-libs/x86_64-unknown-freebsd/libhew.a" in freebsd
    assert freebsd.count("cp cross-release-libs/x86_64-unknown-freebsd/libhew.a") == 2
    assert '"${ARCHIVE_NAME}/lib/x86_64-unknown-freebsd/"' in freebsd
    assert "pkg-smoke-ok" in freebsd

    assert "needs: build-cross-release-libs" in freebsd_aarch64
    assert "name: libhew-aarch64-unknown-freebsd" in freebsd_aarch64
    assert "cargo build -p hew-lib --profile release-lib" not in freebsd_aarch64
    assert (
        "--archive cross-release-libs/aarch64-unknown-freebsd/libhew.a"
        in freebsd_aarch64
    )
    assert (
        freebsd_aarch64.count("cp cross-release-libs/aarch64-unknown-freebsd/libhew.a")
        == 2
    )
    assert '"${ARCHIVE_NAME}/lib/aarch64-unknown-freebsd/"' in freebsd_aarch64
    assert "pkg-smoke-ok" in freebsd_aarch64


def test_freebsd_release_lanes_provision_bash_and_package_with_posix_sh() -> None:
    release = workflow()
    gate = RELEASE_GATE.read_text()
    assert release.count("git bash pkgconf") == 2
    assert gate.count("git gmake bash pkgconf") == 2
    assert release.count("command -v bash") == 2
    assert gate.count("command -v bash") == 2
    assert release.count("bash scripts/test-release-lib-link.sh") == 1
    assert release.count("bash release-machinery/scripts/test-release-lib-link.sh") == 1
    # Gate: FreeBSD x86_64 only — the aarch64 gate leg is intentionally
    # scoped to build+smoke (coverage retained on freebsd-x86_64/linux-aarch64).
    assert gate.count("bash scripts/test-release-lib-link.sh") == 1

    for job_name in ("  build-freebsd:\n", "  build-freebsd-aarch64:\n"):
        start = release.index(job_name)
        next_job = release.find("\n  # ──", start + len(job_name))
        block = release[start : next_job if next_job != -1 else len(release)]
        assert "if [[ " not in block


def assert_freebsd_x86_64_release_uses_pinned_rust(
    release: str, rust_toolchain: str
) -> None:
    channel = re.search(r'^channel = "([^"]+)"$', rust_toolchain, re.MULTILINE)
    assert channel, "rust-toolchain.toml must declare an exact channel"
    version = channel.group(1)
    assert re.fullmatch(r"\d+\.\d+\.\d+", version), "Rust channel must be pinned"

    start = release.index("  build-freebsd:\n")
    end = release.index("  build-freebsd-aarch64:\n", start)
    job = release[start:end]

    assert (
        "pkg install -y -r FreeBSD llvm22 rustup-init cmake ninja git bash "
        "pkgconf libffi libxml2" in job
    )
    assert "pkg install -y -r FreeBSD llvm22 rust cmake" not in job
    install = (
        "/usr/local/bin/rustup-init -y --no-modify-path --profile minimal \\\n"
        f"              --default-toolchain {version}"
    )
    probe = (
        f"rustup run {version} rustc --version | grep -q '^rustc {re.escape(version)} '"
    )
    assert job.count(install) == 1
    assert job.count('export PATH="$HOME/.cargo/bin:$PATH"') == 1
    assert job.count(probe) == 1
    assert job.index(install) < job.index(probe) < job.index("cargo build -p hew-cli")


def test_freebsd_x86_64_release_uses_repository_pinned_rust() -> None:
    release = workflow()
    toolchain = RUST_TOOLCHAIN.read_text()
    assert_freebsd_x86_64_release_uses_pinned_rust(release, toolchain)
    channel = re.search(r'^channel = "([^"]+)"$', toolchain, re.MULTILINE)
    assert channel
    version = channel.group(1)

    mutations = (
        release.replace("llvm22 rustup-init cmake", "llvm22 rust cmake", 1),
        release.replace(
            f"--default-toolchain {version}", "--default-toolchain stable", 1
        ),
        release.replace(
            f"rustup run {version} rustc --version | "
            f"grep -q '^rustc {re.escape(version)} '",
            "rustc --version",
            1,
        ),
    )
    for mutated in mutations:
        try:
            assert_freebsd_x86_64_release_uses_pinned_rust(mutated, toolchain)
        except AssertionError:
            continue
        raise AssertionError("FreeBSD Rust toolchain mutation escaped the contract")


def assert_freebsd_aarch64_release_uses_cross_built_consumer(
    release: str, rust_toolchain: str
) -> None:
    channel = re.search(r'^channel = "([^"]+)"$', rust_toolchain, re.MULTILINE)
    assert channel, "rust-toolchain.toml must declare an exact channel"
    version = channel.group(1)
    assert re.fullmatch(r"\d+\.\d+\.\d+", version), "Rust channel must be pinned"

    cross_start = release.index("  build-cross-release-libs:\n")
    cross_end = release.index("  build:\n", cross_start)
    cross_job = release[cross_start:cross_end]
    freebsd_start = release.index("  build-freebsd-aarch64:\n")
    freebsd_end = release.index("  linux-packages:\n", freebsd_start)
    freebsd_job = release[freebsd_start:freebsd_end]

    package_install = (
        "pkg install -y -r FreeBSD llvm22 rust cmake ninja git bash pkgconf "
        "libffi libxml2"
    )
    consumer_build = (
        "cargo zigbuild -Zbuild-std=std,panic_abort\n"
        "          --manifest-path "
        "release-machinery/scripts/fixtures/release-lib-link/Cargo.toml\n"
        "          --release --target ${{ matrix.rust_target }}"
    )
    consumer_stage = (
        'cp "cross-release-consumer-target/${{ matrix.rust_target }}/release/'
        'libhew_release_link_probe.a" \\\n'
        '              "${destination}/libhew_release_link_probe.a"'
    )
    consumer_proof = (
        "--consumer-archive cross-release-libs/aarch64-unknown-freebsd/"
        "libhew_release_link_probe.a"
    )
    machinery_checkout = """      - name: Checkout release machinery
        uses: actions/checkout@df4cb1c069e1874edd31b4311f1884172cec0e10  # v6.0.3
        with:
          ref: ${{ github.sha }}
          path: release-machinery
"""

    assert f'toolchain: "{version}"' in cross_job
    assert cross_job.count("      - name: Build aarch64 FreeBSD release consumer") == 1
    assert cross_job.count("CARGO_TARGET_DIR: cross-release-consumer-target") == 1
    assert cross_job.count(consumer_build) == 1
    assert cross_job.count(consumer_stage) == 1
    assert "path: cross-release-libs/${{ matrix.rust_target }}/" in cross_job

    assert freebsd_job.count(package_install) == 1
    assert freebsd_job.count(machinery_checkout) == 1
    assert (
        freebsd_job.count("bash release-machinery/scripts/test-release-lib-link.sh")
        == 1
    )
    assert freebsd_job.count(consumer_proof) == 1
    assert "/usr/local/bin/rustup-init" not in freebsd_job
    assert "rustup run" not in freebsd_job
    assert "aarch64-unknown-freebsd/rustup-init" not in release
    assert (
        freebsd_job.index("/usr/sbin/pkg bootstrap -fy -r FreeBSD")
        < freebsd_job.index("pkg update -f -r FreeBSD")
        < freebsd_job.index(package_install)
        < freebsd_job.index("cargo build -p hew-cli")
        < freebsd_job.index(consumer_proof)
    )


def test_freebsd_aarch64_release_uses_cross_built_consumer() -> None:
    release = workflow()
    toolchain = RUST_TOOLCHAIN.read_text()
    assert_freebsd_aarch64_release_uses_cross_built_consumer(release, toolchain)

    mutations = (
        release.replace("llvm22 rust cmake", "llvm22 cmake", 1),
        release.replace(
            "--consumer-archive cross-release-libs/aarch64-unknown-freebsd/"
            "libhew_release_link_probe.a",
            "",
            1,
        ),
        release.replace(
            "cargo zigbuild -Zbuild-std=std,panic_abort\n"
            "          --manifest-path release-machinery/scripts/fixtures/"
            "release-lib-link/Cargo.toml",
            "cargo zigbuild\n"
            "          --manifest-path release-machinery/scripts/fixtures/"
            "release-lib-link/Cargo.toml",
            1,
        ),
        release.replace(
            "            cargo build -p hew-cli -p hew-lsp -p hew-observe --release",
            "            /usr/bin/fetch https://static.rust-lang.org/rustup/dist/"
            "aarch64-unknown-freebsd/rustup-init\n"
            "            cargo build -p hew-cli -p hew-lsp -p hew-observe --release",
            1,
        ),
    )
    for mutated in mutations:
        try:
            assert_freebsd_aarch64_release_uses_cross_built_consumer(mutated, toolchain)
        except AssertionError:
            continue
        raise AssertionError("FreeBSD aarch64 consumer mutation escaped the contract")


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


def test_release_record_is_durable_and_tag_ready() -> None:
    changelog = CHANGELOG.read_text()
    notes = RELEASE_NOTES.read_text()
    runbook = RUNBOOK.read_text()
    notes_words = " ".join(notes.split())
    runbook_words = " ".join(runbook.split())

    unreleased_start = changelog.index("## [Unreleased]")
    rc1_start = changelog.index("## [0.6.0-rc1] - 2026-07-29")
    unreleased = changelog[unreleased_start:rc1_start]
    assert "### Changed" in unreleased
    assert "- " in unreleased

    next_release = changelog.find("\n## [", rc1_start + 1)
    rc1_record = (
        changelog[rc1_start:]
        if next_release == -1
        else changelog[rc1_start:next_release]
    )
    for provisional in (
        "unreleased",
        "tag is not cut",
        "will be finalized when",
        "in preparation",
    ):
        assert provisional not in rc1_record.lower()

    assert "v0.6.0-rc1" in notes_words
    assert "first release candidate for v0.6.0" in notes_words
    assert "not the final v0.6.0 release" in notes_words
    assert "Publication for this first RC is deliberately staged" in notes_words
    assert (
        "The signed tag publishes the platform assets and checksums first"
        in notes_words
    )
    assert "npm publication is not inferred from the tag" in notes_words
    for pre_tag_only in (
        "tag and final changelog date remain intentionally unset",
        "This candidate does not claim",
    ):
        assert pre_tag_only not in notes_words

    assert (
        "CHANGELOG.md has either a populated `[Unreleased]` section or the dated "
        "`[X.Y.Z]` section for the intended release"
    ) in runbook_words
    assert 'git tag -s v0.6.0-rc1 -m "Hew v0.6.0-rc1"' in runbook
    assert "git push origin v0.6.0-rc1" in runbook
    assert "git tag v0.4.0" not in runbook
    assert "git push origin v0.4.0" not in runbook
    assert "every release bar and the final-candidate checklist are green" in runbook
    assert "Manually dispatch" in runbook
    assert "both independent publication arms" in runbook
    assert "Only after both arms are green" in runbook
    assert "Homebrew" in runbook and "prerelease" in runbook
    assert "publish-npm-packages.yml" in runbook


def test_contract_oracle_runs_in_required_ci() -> None:
    ci = CI_WORKFLOW.read_text()
    assert "'.github/workflows/release.yml'" in ci
    assert 'scripts/ci-preflight-dispatcher.sh "${args[@]}"' in ci
    assert "args=(--base origin/main)" in ci
    dispatcher = (ROOT / "scripts/ci-preflight-dispatcher.sh").read_text()
    assert 'add_command "make test-release-workflow-contract"' in dispatcher


def workflow_job(text: str, name: str) -> str:
    """Return one top-level GitHub Actions job without parsing unrelated YAML."""
    start = text.index(f"  {name}:\n")
    next_job = re.search(r"^  [a-z][a-z0-9-]*:\n", text[start + 1 :], re.MULTILINE)
    end = start + 1 + next_job.start() if next_job else len(text)
    return text[start:end]


def assert_binaryen_downloader_contract(downloader: str) -> None:
    assert (
        "github.com/WebAssembly/binaryen/releases/download/${version}/${asset}"
        in downloader
    )
    assert "--retry-all-errors" in downloader
    assert 'sha256sum -c "${tarball}.sha256"' in downloader
    assert 'tar -xzf "${tarball}" -C "${install_root}"' in downloader


def assert_wasm_pack_action_contract(action: str) -> None:
    assert "WASM_PACK_VERSION=0.13.1" in action
    assert "RETRY_ATTEMPTS=5 RETRY_INITIAL_DELAY=10" in action
    assert "scripts/retry-download.sh" in action
    assert "scripts/download-verify-binaryen.sh" in action
    assert "BINARYEN_VERSION=version_117" in action
    assert f"BINARYEN_SHA256={BINARYEN_SHA256}" in action
    assert '"${BINARYEN_VERSION}" "${BINARYEN_SHA256}"' in action
    assert (
        'echo "${RUNNER_TEMP}/binaryen-${BINARYEN_VERSION}/bin"'
        ' >> "${GITHUB_PATH}"' in action
    )
    assert (
        '"${RUNNER_TEMP}/binaryen-${BINARYEN_VERSION}/bin/wasm-opt" --version' in action
    )


def test_wasm_pack_consumers_prefetch_checksum_pinned_binaryen() -> None:
    action = SETUP_WASM_PACK_ACTION.read_text()
    downloader = DOWNLOAD_VERIFY_BINARYEN.read_text()
    assert_wasm_pack_action_contract(action)
    assert_binaryen_downloader_contract(downloader)

    consumers = (
        (
            workflow_job(CI_WORKFLOW.read_text(), "playground-wasm-build"),
            "uses: ./.github/actions/setup-wasm-pack",
            "make playground-check",
        ),
        (
            workflow_job(CI_WORKFLOW.read_text(), "build-and-test"),
            "uses: ./.github/actions/setup-wasm-pack",
            "scripts/ci-preflight-dispatcher.sh",
        ),
        (
            workflow_job(RELEASE_GATE.read_text(), "gate-linux"),
            "uses: ./.github/actions/setup-wasm-pack",
            "make playground-check",
        ),
        (
            workflow_job(NPM_PUBLISH_WORKFLOW.read_text(), "publish"),
            "uses: ./release-machinery/.github/actions/setup-wasm-pack",
            "node release-machinery/scripts/build-npm-packages.mjs",
        ),
    )
    for job, action_use, build_command in consumers:
        assert job.count(action_use) == 1
        assert job.index(action_use) < job.index(build_command)


def test_ci_wasm_consumers_provision_unknown_target() -> None:
    ci = CI_WORKFLOW.read_text()
    for job_name in ("playground-wasm-build", "build-and-test"):
        job = workflow_job(ci, job_name)
        assert "uses: ./.github/actions/setup-rust-build" in job
        assert "targets: wasm32-unknown-unknown" in job


def test_binaryen_prefetch_pin_mutations_are_rejected() -> None:
    action = SETUP_WASM_PACK_ACTION.read_text()
    mutated = action.replace(BINARYEN_SHA256, "0" * 64)
    try:
        assert_wasm_pack_action_contract(mutated)
    except AssertionError:
        return
    raise AssertionError("Binaryen checksum mutation escaped the contract")


def assert_windows_job_initialises_msvc_before_native_linking(job: str) -> None:
    """Require precisely one MSVC environment import before LLVM/Cargo use."""
    setup_msvc = "uses: ./.github/actions/setup-msvc"
    setup_llvm = "uses: ./.github/actions/setup-llvm"
    assert job.count(setup_msvc) == 1
    assert setup_msvc in job and setup_llvm in job
    assert job.index(setup_msvc) < job.index(setup_llvm)
    assert job.index(setup_msvc) < job.index("cargo ")


def test_windows_test_workflows_initialise_msvc_before_lld_link() -> None:
    workflows = (
        (CI_WORKFLOW.read_text(), "build-and-test-windows"),
        (COVERAGE_NIGHTLY_WORKFLOW.read_text(), "full-windows"),
        (RELEASE_GATE.read_text(), "gate-windows"),
    )
    for text, name in workflows:
        assert_windows_job_initialises_msvc_before_native_linking(
            workflow_job(text, name)
        )


def test_windows_test_workflow_msvc_ordering_mutations_are_rejected() -> None:
    job = workflow_job(CI_WORKFLOW.read_text(), "build-and-test-windows")
    setup_msvc = "uses: ./.github/actions/setup-msvc"
    setup_llvm = "uses: ./.github/actions/setup-llvm"
    for mutation in (
        job.replace(setup_msvc, "", 1),
        job.replace(setup_msvc, setup_msvc + "\n        " + setup_msvc, 1),
        job.replace(setup_msvc, "__MSVC_STEP__", 1)
        .replace(setup_llvm, setup_msvc, 1)
        .replace("__MSVC_STEP__", setup_llvm, 1),
    ):
        try:
            assert_windows_job_initialises_msvc_before_native_linking(mutation)
        except AssertionError:
            continue
        raise AssertionError("Windows MSVC setup mutation escaped the contract")


def assert_windows_llvm_toolchain_contract(
    setup_action: str, release: str, build_guide: str
) -> None:
    """Keep static llvm-sys linking on Windows compatible with the UCRT.

    The source build that produces this archive now lives in
    hew-lang/llvm-toolchain, so the allocator flag and the rpmalloc archive
    scan are asserted in that repository. What this repository owns — and what
    this contract pins — is the consumer side: every download, verification and
    extraction step must name the same toolchain repository, tag and asset, so
    a partial bump cannot verify one archive and extract another.
    """
    assert f'version="{WINDOWS_LLVM_TOOLCHAIN_VERSION}"' in setup_action
    assert f'"{WINDOWS_LLVM_TOOLCHAIN_ASSET}")' in setup_action
    assert (
        f"https://github.com/{WINDOWS_LLVM_TOOLCHAIN_REPO}/releases/download/"
        in setup_action
    )
    assert f'$toolchainRepo = "{WINDOWS_LLVM_TOOLCHAIN_REPO}"' in release
    assert f'$toolchainTag  = "{WINDOWS_LLVM_TOOLCHAIN_TAG}"' in release
    assert f'$asset         = "{WINDOWS_LLVM_TOOLCHAIN_ASSET}"' in release
    # Download, provenance-verify and extract must all name the same archive.
    assert release.count(WINDOWS_LLVM_TOOLCHAIN_ASSET) == 3
    assert release.count(f'$toolchainRepo = "{WINDOWS_LLVM_TOOLCHAIN_REPO}"') == 2
    assert "windows-msvc-v3" not in "\n".join((setup_action, release))
    assert "-DLLVM_INTEGRATED_CRT_ALLOC=OFF" in build_guide


def test_windows_llvm_prebuild_workflow_is_not_owned_by_this_repository() -> None:
    assert not WINDOWS_LLVM_PREBUILD.exists()


def test_windows_llvm_toolchain_pin_is_coherent_across_consumers() -> None:
    assert_windows_llvm_toolchain_contract(
        SETUP_LLVM_ACTION.read_text(),
        workflow(),
        WINDOWS_BUILD_GUIDE.read_text(),
    )


def test_windows_llvm_toolchain_pin_mutations_are_rejected() -> None:
    setup_action = SETUP_LLVM_ACTION.read_text()
    release = workflow()
    build_guide = WINDOWS_BUILD_GUIDE.read_text()
    for mutation in (
        # Half-bumped pin: setup-llvm moves forward, release.yml does not.
        (
            setup_action.replace(
                WINDOWS_LLVM_TOOLCHAIN_VERSION,
                "22.1.0-windows-msvc-v2",
            ),
            release,
            build_guide,
        ),
        # Extraction step left on the previous asset revision.
        (
            setup_action,
            release.replace(
                f'$asset = "{WINDOWS_LLVM_TOOLCHAIN_ASSET}"',
                '$asset = "hew-llvm-22.1.0-windows-msvc-v3.tar.gz"',
                1,
            ),
            build_guide,
        ),
        # Provenance verified against this repository instead of the toolchain one.
        (
            setup_action,
            release.replace(
                f'$toolchainRepo = "{WINDOWS_LLVM_TOOLCHAIN_REPO}"',
                '$toolchainRepo = "hew-lang/hew"',
                1,
            ),
            build_guide,
        ),
        # Local build guide loses the allocator instruction.
        (
            setup_action,
            release,
            build_guide.replace("-DLLVM_INTEGRATED_CRT_ALLOC=OFF", "", 1),
        ),
    ):
        try:
            assert_windows_llvm_toolchain_contract(*mutation)
        except AssertionError:
            continue
        raise AssertionError("Windows LLVM toolchain pin mutation escaped the contract")


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


def test_local_release_builds_and_assembles_every_shipped_binary() -> None:
    makefile = MAKEFILE.read_text()
    release = makefile[
        makefile.index("release:\n") : makefile.index("\n# Validate release builds")
    ]
    assembly = makefile[
        makefile.index("assemble-release:\n") : makefile.index("\n# ── Tests")
    ]
    install = makefile[
        makefile.index("define require_release_artifacts\n") : makefile.index(
            "\nuninstall:"
        )
    ]

    for package in ("hew-cli", "hew-lsp", "hew-observe"):
        assert f"cargo build -p {package} --release" in release
    for binary in ("hew", "hew-lsp", "hew-observe"):
        name = re.escape(binary)
        assert re.search(
            rf'^\s*@ln -sfn "\$\(LINK_UP2\)\$\(RELEASE_DIR\)/{name}"\s+'
            rf'"\$\(BUILD_DIR\)/bin/{name}"$',
            assembly,
            re.MULTILINE,
        )
        assert f'@test -x "$(RELEASE_DIR)/{binary}" \\' in install
        assert f'install -m 755 "$(RELEASE_DIR)/{binary}"' in install
        assert f'"$(DESTDIR)$(PREFIX)/bin/{binary}"' in install
    assert "cargo build -p hew-lib --profile release-lib" in release
    assert "$(RELEASE_LIB_DIR)/libhew.a" in assembly


def test_windows_completion_packaging_fails_closed() -> None:
    release = workflow()
    start = release.index("      - name: Package archive (Windows)\n")
    end = release.index("      # ── macOS code signing", start)
    package = release[start:end]

    assert "function Write-Completion {" in package
    assert "$Completion = & $Executable completions $Shell" in package
    assert "if ($LASTEXITCODE -ne 0) {" in package
    assert "produced empty output" in package
    for executable in ("hew.exe",):
        assert (
            f'Write-Completion "${{ArchiveName}}/bin/{executable}" $shell '
            f'"${{ArchiveName}}/completions/{executable.removesuffix(".exe")}.${{shell}}"'
            in package
        )


def _make_dry_run(target: str, cargo_target_dir: Path, *make_overrides: str) -> str:
    env = os.environ.copy()
    env["CARGO_TARGET_DIR"] = str(cargo_target_dir)
    result = subprocess.run(
        ["make", "-n", target, *make_overrides],
        cwd=ROOT,
        env=env,
        check=False,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "warning: overriding commands for target" not in result.stderr
    assert "warning: ignoring old commands for target" not in result.stderr
    return result.stdout


def test_make_release_surfaces_quote_spacious_cargo_target_dir() -> None:
    """Make must not split Cargo artifact paths into targets or shell words."""
    with tempfile.TemporaryDirectory(prefix="hew-make-output-contract-") as raw:
        cargo_target_dir = Path(raw) / "cargo artifacts with spaces"
        release_dir = cargo_target_dir / "release"
        release_lib_dir = cargo_target_dir / "release-lib"

        assembly = _make_dry_run("assemble-release", cargo_target_dir)
        install = _make_dry_run("install", cargo_target_dir)
        debug = _make_dry_run("assemble", cargo_target_dir)
        target_triple = "x86_64-unknown-linux-gnu"
        cross_assembly = _make_dry_run(
            "assemble-release",
            cargo_target_dir,
            f"TARGET_TRIPLE={target_triple}",
        )

        for binary in ("hew", "hew-lsp", "hew-observe"):
            source = release_dir / binary
            assert f'"{source}"' in assembly
            assert f'"{source}"' in install

        release_archive = release_lib_dir / "libhew.a"
        assert f'--archive "{release_archive}"' in assembly
        assert f'"{release_archive}"' in install

        # The same invariant applies to debug and explicit wasm target layouts.
        assert f'"{cargo_target_dir / "debug" / "hew"}"' in debug
        wasm_debug = cargo_target_dir / "wasm32-wasip1" / "debug"
        assert f'"{wasm_debug}/$lib"' in debug

        cross_release = cargo_target_dir / target_triple / "release"
        cross_release_lib = cargo_target_dir / target_triple / "release-lib"
        assert f'"{cross_release / "hew-lsp"}"' in cross_assembly
        assert f'--archive "{cross_release_lib / "libhew.a"}"' in cross_assembly


def _write_install_artifacts(target_dir: Path) -> None:
    binaries = tuple(
        target_dir / "release" / name for name in ("hew", "hew-lsp", "hew-observe")
    )
    for binary in binaries:
        binary.parent.mkdir(parents=True, exist_ok=True)
        binary.write_text(
            "#!/bin/sh\n"
            'if [ "${1:-}" = completions ]; then\n'
            f"  printf 'completion:{binary.name}:%s\\n' \"${{2:-}}\"\n"
            "fi\n"
        )
        binary.chmod(0o755)
    for archive in (
        target_dir / "release-lib" / "libhew.a",
        target_dir / "wasm32-wasip1" / "release" / "libhew_runtime.a",
    ):
        archive.parent.mkdir(parents=True, exist_ok=True)
        archive.write_bytes(f"fixture:{archive.name}\n".encode())


def test_staged_install_and_uninstall_preserve_spacious_path_boundaries() -> None:
    """The staged prefix is one path, never a shell word list or broad root."""
    with tempfile.TemporaryDirectory(prefix="hew-staged-install-") as raw:
        temp = Path(raw)
        cargo_target_dir = temp / "cargo artifacts"
        destdir = temp / "stage root"
        prefix = "/opt/hew rc1"
        install_root = Path(f"{destdir}{prefix}")
        neighbour = install_root.with_name("hew rc1-neighbour")
        neighbour.mkdir(parents=True)
        sentinel = neighbour / "keep"
        sentinel.write_text("not owned by uninstall\n")
        _write_install_artifacts(cargo_target_dir)

        env = os.environ.copy()
        env["CARGO_TARGET_DIR"] = str(cargo_target_dir)
        overrides = [f"DESTDIR={destdir}", f"PREFIX={prefix}"]
        installed = subprocess.run(
            [
                "bash",
                "-c",
                'umask 077; exec "$@"',
                "staged-install",
                "make",
                "install",
                *overrides,
            ],
            cwd=ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
            timeout=30,
        )
        assert installed.returncode == 0, installed.stdout + installed.stderr

        for binary in ("hew", "hew-lsp", "hew-observe"):
            path = install_root / "bin" / binary
            assert path.is_file()
            assert os.access(path, os.X_OK)
        assert (install_root / "lib" / "libhew.a").is_file()
        assert (install_root / "lib" / "wasm32-wasip1" / "libhew_runtime.a").is_file()
        assert (install_root / "std" / "prelude.hew").is_file()
        for completion in (
            "hew.bash",
            "hew.zsh",
            "hew.fish",
        ):
            path = install_root / "completions" / completion
            tool, shell = completion.split(".")
            assert path.read_text() == f"completion:{tool}:{shell}\n"
            assert stat.S_IMODE(path.stat().st_mode) == 0o644

        removed = subprocess.run(
            ["make", "uninstall", *overrides],
            cwd=ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
            timeout=30,
        )
        assert removed.returncode == 0, removed.stdout + removed.stderr
        assert not install_root.exists()
        assert sentinel.read_text() == "not owned by uninstall\n"

        # Counterfactual: an installed completion is part of the release
        # surface, so a generator failure must make the staged install red.
        hew = cargo_target_dir / "release" / "hew"
        hew.write_text("#!/bin/sh\nexit 17\n")
        hew.chmod(0o755)
        completion_failure = subprocess.run(
            ["make", "install", *overrides],
            cwd=ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
            timeout=30,
        )
        assert completion_failure.returncode != 0

        for unsafe_destdir, unsafe_prefix in (
            ("", ""),
            ("", "/"),
            ("", "/."),
            ("", "/.."),
            ("", "//"),
            ("/.", "/"),
            ("", "."),
            ("", ".."),
            (".", "/"),
        ):
            refused = subprocess.run(
                [
                    "make",
                    "uninstall",
                    f"DESTDIR={unsafe_destdir}",
                    f"PREFIX={unsafe_prefix}",
                ],
                cwd=ROOT,
                env=env,
                check=False,
                capture_output=True,
                text=True,
                timeout=30,
            )
            assert refused.returncode != 0
            assert "Error:" in refused.stderr

        for invalid_destdir, invalid_prefix in (("", "."), (".", "/opt/hew")):
            refused_install = subprocess.run(
                [
                    "make",
                    "install",
                    f"DESTDIR={invalid_destdir}",
                    f"PREFIX={invalid_prefix}",
                ],
                cwd=ROOT,
                env=env,
                check=False,
                capture_output=True,
                text=True,
                timeout=30,
            )
            assert refused_install.returncode != 0
            assert "must be" in refused_install.stderr


_PACKAGING_CARGO_DOUBLE = """#!/usr/bin/env python3
import json
import os
import sys
from pathlib import Path

args = sys.argv[1:]
if args and args[0] == "metadata":
    print(json.dumps({"target_directory": os.environ["MOCK_TARGET_ROOT"]}))
    raise SystemExit(0)

with Path(os.environ["MOCK_CARGO_LOG"]).open("a") as stream:
    stream.write("cargo " + " ".join(args) + "\\n")

root = Path(os.environ.get("CARGO_TARGET_DIR", os.environ["MOCK_TARGET_ROOT"]))
if not root.is_absolute():
    root = Path.cwd() / root
target = ""
if "--target" in args:
    target = args[args.index("--target") + 1]
elif os.environ.get("CARGO_BUILD_TARGET"):
    target = os.environ["CARGO_BUILD_TARGET"]
elif os.environ.get("MOCK_BUILD_TARGET"):
    target = os.environ["MOCK_BUILD_TARGET"]
if target:
    root /= target

profile = "release-lib" if "--profile" in args else "release"
out = root / profile
out.mkdir(parents=True, exist_ok=True)
if profile == "release-lib":
    (out / "libhew.a").write_bytes(b"release-lib-archive")
else:
    # A stale/wrong-profile archive proves package assembly did not copy this.
    (out / "libhew.a").write_bytes(b"fat-lto-release-archive")
    program = (
        "#!/usr/bin/env bash\\n"
        'if [[ "${1:-}" == "completions" ]]; then printf "mock completion\\\\n"; fi\\n'
    )
    for binary in ("hew", "hew-lsp", "hew-observe"):
        path = out / binary
        path.write_text(program)
        path.chmod(0o755)
"""


def test_distro_tarball_uses_cargo_output_layout_and_release_lib_archive() -> None:
    """Execute the real packager against env- and config-selected layouts."""
    scenarios = ("CARGO_TARGET_DIR", "build.target-dir")
    for scenario in scenarios:
        with tempfile.TemporaryDirectory(
            prefix=f"hew-package-{scenario.lower().replace('.', '-')}-"
        ) as directory:
            repo = Path(directory) / "hew"
            (repo / "installers").mkdir(parents=True)
            (repo / "scripts" / "lib").mkdir(parents=True)
            (repo / "std").mkdir()
            (repo / "mock-bin").mkdir()

            for source, destination in (
                (PACKAGE_BUILDER, repo / "installers" / "build-packages.sh"),
                (
                    ROOT / "scripts" / "cargo-output-dir.py",
                    repo / "scripts" / "cargo-output-dir.py",
                ),
                (
                    ROOT / "scripts" / "lib" / "toml_compat.py",
                    repo / "scripts" / "lib" / "toml_compat.py",
                ),
            ):
                shutil.copy2(source, destination)
                destination.chmod(0o755)

            (repo / "std" / "prelude.hew").write_text("// packaging fixture\n")
            for name in ("LICENSE-MIT", "LICENSE-APACHE", "NOTICE", "README.md"):
                (repo / name).write_text(f"{name}\n")

            cargo = repo / "mock-bin" / "cargo"
            cargo.write_text(_PACKAGING_CARGO_DOUBLE)
            cargo.chmod(0o755)
            cargo_log = repo / "cargo.log"
            target_root = repo / ".cargo-artifacts"
            build_target = "x86_64-contract-linux-gnu"

            env = os.environ.copy()
            env.update(
                {
                    "PATH": f"{repo / 'mock-bin'}:{env['PATH']}",
                    "MOCK_CARGO_LOG": str(cargo_log),
                    "MOCK_TARGET_ROOT": str(target_root),
                }
            )
            env.pop("CARGO_TARGET_DIR", None)
            env.pop("CARGO_BUILD_TARGET", None)
            if scenario == "CARGO_TARGET_DIR":
                env["CARGO_TARGET_DIR"] = ".cargo-artifacts"
                env["CARGO_BUILD_TARGET"] = build_target
            else:
                (repo / ".cargo").mkdir()
                (repo / ".cargo" / "config.toml").write_text(
                    "[build]\n"
                    'target-dir = ".cargo-artifacts"\n'
                    f'target = "{build_target}"\n'
                )
                # The command double mirrors Cargo's parsed configuration while
                # the production resolver itself reads the config target.
                env["MOCK_BUILD_TARGET"] = build_target

            result = subprocess.run(
                [
                    "bash",
                    str(repo / "installers" / "build-packages.sh"),
                    "--version",
                    "0.6.0-rc1",
                    "--arch",
                    "x86_64",
                    "--only",
                    "tarball",
                ],
                cwd=repo,
                env=env,
                check=False,
                capture_output=True,
                text=True,
                timeout=30,
            )
            assert result.returncode == 0, (
                scenario + "\n" + result.stdout + result.stderr
            )

            calls = cargo_log.read_text().splitlines()
            assert calls == [
                "cargo build -p hew-cli -p hew-lsp -p hew-observe --release",
                "cargo build -p hew-lib --profile release-lib",
            ]

            archive = repo / "dist" / "hew-v0.6.0-rc1-linux-x86_64.tar.gz"
            package_root = "hew-v0.6.0-rc1-linux-x86_64"
            with tarfile.open(archive) as package:
                names = set(package.getnames())
                for binary in ("hew", "hew-lsp", "hew-observe"):
                    assert f"{package_root}/bin/{binary}" in names
                member = package.extractfile(f"{package_root}/lib/libhew.a")
                assert member is not None
                assert member.read() == b"release-lib-archive"


def test_musl_packaging_uses_explicit_target_release_lib_output() -> None:
    builder = PACKAGE_BUILDER.read_text()
    assert (
        'cargo build --profile release-lib --target "${musl_target}" -p hew-lib'
        in builder
    )
    assert (
        '_cargo_output_dir --native --profile release-lib --target "${musl_target}"'
        in builder
    )
    assert "${REPO_DIR}/target/release" not in builder
    assert "${REPO_DIR}/target/${musl_target}" not in builder


def assert_foundational_release_gate_contract(gate: str, validator: str) -> None:
    linux = gate[gate.index("  gate-linux:\n") : gate.index("  gate-linux-aarch64:\n")]
    for command in (
        "make check-gate-reachability",
        "make test-release-workflow-contract",
        "make test-opaque-resource-lifecycle-matrix-external",
        "make test-vertical-slice",
        "make test-hew-ratchet",
        "make test-stdlib-ratchet",
        "make test-stdlib-execution-proofs",
    ):
        assert command in linux
        assert command in validator
    assert "make test-compiler-lifecycle" in linux
    assert "make test-compiler-pipeline" not in linux
    assert "make test-compiler-pipeline" in validator
    assert "make macos-leak-oracle" in validator
    assert "macos-14" in gate and "macos-15-intel" in gate
    # FreeBSD x86_64 only — the aarch64 gate leg is intentionally scoped to
    # build+smoke (suite coverage retained on freebsd-x86_64 and linux-aarch64).
    assert gate.count("gmake test-vertical-slice") == 1
    assert gate.count("gmake test-hew-ratchet") == 1


def test_foundational_release_gates_are_platform_scoped_and_mandatory() -> None:
    gate = RELEASE_GATE.read_text()
    validator = PRE_RELEASE_VALIDATOR.read_text()
    assert_foundational_release_gate_contract(gate, validator)
    mutations = (
        (gate.replace("make test-stdlib-execution-proofs", "true", 1), validator),
        (gate, validator.replace("make macos-leak-oracle", "true", 1)),
        (gate.replace("macos-15-intel", "macos-14", 1), validator),
        (gate.replace("gmake test-vertical-slice", "true", 1), validator),
    )
    for mutated_gate, mutated_validator in mutations:
        try:
            assert_foundational_release_gate_contract(mutated_gate, mutated_validator)
        except AssertionError:
            continue
        raise AssertionError("foundational release-gate mutation escaped")


_TESTS = [
    test_rc_tag_normalization_and_exact_release_body,
    test_release_tag_must_match_cargo_version_before_build,
    test_npm_publication_is_pinned_to_a_version_matching_release_tag,
    test_current_sandbox_vm_version_matches_workspace_version,
    test_playground_dispatch_is_purpose_scoped_and_fail_closed,
    test_dispatch_uses_exact_playground_workflow_input_and_ref,
    test_dispatch_correlation_is_unique_and_bounded,
    test_release_image_acquisition_mode_is_exhaustive_and_fails_closed,
    test_release_image_assertion_covers_every_acquisition_mode,
    test_release_image_assertion_rejects_unusable_inputs,
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
    test_cross_release_machinery_resolves_from_workflow_ref,
    test_npm_publish_machinery_resolves_from_workflow_ref,
    test_cross_release_libraries_are_target_keyed_and_natively_proved,
    test_freebsd_release_lanes_provision_bash_and_package_with_posix_sh,
    test_freebsd_x86_64_release_uses_repository_pinned_rust,
    test_freebsd_aarch64_release_uses_cross_built_consumer,
    test_sanitizer_gate_is_behavioral_and_release_scoped,
    test_release_record_is_durable_and_tag_ready,
    test_contract_oracle_runs_in_required_ci,
    test_wasm_pack_consumers_prefetch_checksum_pinned_binaryen,
    test_binaryen_prefetch_pin_mutations_are_rejected,
    test_windows_test_workflows_initialise_msvc_before_lld_link,
    test_windows_test_workflow_msvc_ordering_mutations_are_rejected,
    test_windows_llvm_prebuild_workflow_is_not_owned_by_this_repository,
    test_windows_llvm_toolchain_pin_is_coherent_across_consumers,
    test_windows_llvm_toolchain_pin_mutations_are_rejected,
    test_release_binary_smoke_honors_absolute_and_relative_target_dirs,
    test_local_release_builds_and_assembles_every_shipped_binary,
    test_windows_completion_packaging_fails_closed,
    test_make_release_surfaces_quote_spacious_cargo_target_dir,
    test_staged_install_and_uninstall_preserve_spacious_path_boundaries,
    test_distro_tarball_uses_cargo_output_layout_and_release_lib_archive,
    test_musl_packaging_uses_explicit_target_release_lib_output,
    test_foundational_release_gates_are_platform_scoped_and_mandatory,
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
