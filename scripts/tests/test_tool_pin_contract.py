"""Fail-closed contract for build-system tool pins and CI installers."""

import re
import shlex
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
XTASK = ROOT / "xtask" / "src" / "build_system.rs"
CI = ROOT / ".github" / "workflows" / "ci.yml"
COVERAGE = ROOT / ".github" / "workflows" / "coverage-nightly.yml"
RELEASE_GATE = ROOT / ".github" / "workflows" / "release-gate.yml"
FREEBSD = ROOT / ".github" / "workflows" / "freebsd.yml"
SETUP_WASM_PACK = ROOT / ".github" / "actions" / "setup-wasm-pack" / "action.yml"

PINS = {
    "NEXTEST": ("cargo-nextest", "0.9.120"),
    "CARGO_DENY": ("cargo-deny", "0.19.6"),
    "CARGO_ABOUT": ("cargo-about", "0.9.0"),
    "LLVM_COV": ("cargo-llvm-cov", "0.8.7"),
    "WASM_PACK": ("wasm-pack", "0.13.1"),
    "WASMTIME": ("wasmtime", "47.0.2"),
}


def _xtask_pins(source: str) -> dict[str, tuple[str, str]]:
    return {
        name: (tool, version)
        for name, tool, version in re.findall(
            r'^const (\w+): &str = "([^@]+)@([^"]+)";$', source, re.MULTILINE
        )
    }


def _assert_pins(
    xtask: str,
    ci: str,
    coverage: str,
    release_gate: str,
    freebsd: str,
    setup_wasm_pack: str,
) -> None:
    assert _xtask_pins(xtask) == PINS

    assert "cargo install cargo-deny --locked --version 0.19.6" in ci
    assert "cargo install cargo-about --locked --version 0.9.0 --features cli" in ci
    assert "WASM_PACK_VERSION=0.13.1" in setup_wasm_pack

    for workflow in (ci, coverage, release_gate):
        assert 'WASMTIME_VERSION: "v47.0.2"' in workflow
    assert 'WASMTIME_TAG="${WASMTIME_VERSION}"' in coverage
    assert (
        'gh release download "$WASMTIME_TAG" --repo bytecodealliance/wasmtime'
        in coverage
    )
    assert '$version = "${{ env.WASMTIME_VERSION }}"' in release_gate
    assert 'WASMTIME_VERSION: "v47.0.3"' not in release_gate

    for workflow in (ci, coverage, release_gate):
        for tools in re.findall(r"^\s*tools:\s*(.+)$", workflow, re.MULTILINE):
            for tool in tools.split(","):
                if tool.startswith("nextest"):
                    assert tool == "nextest@0.9.120"
                if tool.startswith("cargo-llvm-cov"):
                    assert tool == "cargo-llvm-cov@0.8.7"
    assert "tools: nextest@0.9.120" in ci
    assert "tools: nextest@0.9.120,cargo-llvm-cov@0.8.7" in coverage
    assert "tools: nextest@0.9.120" in release_gate
    assert "cargo install cargo-nextest --locked --version 0.9.120" in release_gate
    assert "cargo install cargo-nextest --locked --version 0.9.120" in freebsd


def test_workflow_pins_match_the_xtask_graph() -> None:
    _assert_pins(
        XTASK.read_text(),
        CI.read_text(),
        COVERAGE.read_text(),
        RELEASE_GATE.read_text(),
        FREEBSD.read_text(),
        SETUP_WASM_PACK.read_text(),
    )


def test_pin_contract_rejects_workflow_drift() -> None:
    ci = CI.read_text().replace("tools: nextest@0.9.120", "tools: nextest@0.9.98", 1)
    try:
        _assert_pins(
            XTASK.read_text(),
            ci,
            COVERAGE.read_text(),
            RELEASE_GATE.read_text(),
            FREEBSD.read_text(),
            SETUP_WASM_PACK.read_text(),
        )
    except AssertionError:
        return
    raise AssertionError("the tool-pin contract accepted a mismatched CI pin")


def test_pin_contract_rejects_installer_drift() -> None:
    coverage = COVERAGE.read_text().replace(
        'WASMTIME_TAG="${WASMTIME_VERSION}"', 'WASMTIME_TAG="v47.0.3"', 1
    )
    try:
        _assert_pins(
            XTASK.read_text(),
            CI.read_text(),
            coverage,
            RELEASE_GATE.read_text(),
            FREEBSD.read_text(),
            SETUP_WASM_PACK.read_text(),
        )
    except AssertionError:
        return
    raise AssertionError("the tool-pin contract accepted a mismatched installer pin")


# ── Pinned nextest vs. the flags our scripts actually pass ──────────────────
#
# A pin is only correct if the pinned binary understands every flag the build
# system hands it.  Pinning CI to a nextest older than a flag one of our
# scripts passes turns into a CI-only failure that no local run reproduces
# (`cargo nextest list --no-pager` under a 0.9.99 pin was exactly that).
#
# Every flag reachable from a `cargo nextest` invocation in scripts/ is keyed
# here to the nextest release that introduced it.  An unrecognised flag is a
# hard failure, not a pass: nobody has established its minimum version, so the
# contract refuses to certify the pin.
NEXTEST_BASELINE = (0, 9, 0)
NEXTEST_FLAG_MIN_VERSION: dict[str, tuple[int, int, int]] = {
    "-E": NEXTEST_BASELINE,
    "-p": NEXTEST_BASELINE,
    "--color": NEXTEST_BASELINE,
    "--exclude": NEXTEST_BASELINE,
    "--no-default-features": NEXTEST_BASELINE,
    "--no-fail-fast": NEXTEST_BASELINE,
    "--no-run": NEXTEST_BASELINE,
    "--profile": NEXTEST_BASELINE,
    "--test": NEXTEST_BASELINE,
    "--version": NEXTEST_BASELINE,
    "--workspace": NEXTEST_BASELINE,
    "--list-type": (0, 9, 10),  # 0.9.10
    "--cargo-quiet": (0, 9, 58),  # 0.9.58
    "--run-ignored": (0, 9, 76),  # 0.9.76 (`all` / `only` values)
    "--message-format": (0, 9, 117),  # 0.9.117 for `list --message-format oneline`
    "--no-pager": (0, 9, 120),  # 0.9.120, alongside pager support
}


def _parse_version(version: str) -> tuple[int, int, int]:
    major, minor, patch = version.split(".")
    return int(major), int(minor), int(patch)


def _nextest_flags(source: str) -> set[str]:
    """Every flag passed to a `cargo nextest` invocation in a shell script.

    Invocations continue across backslash-continued lines, so the logical
    command is reassembled before tokenising.
    """
    flags: set[str] = set()
    lines = source.split("\n")
    for index, line in enumerate(lines):
        if "cargo nextest" not in line:
            continue
        chunk = [line[line.index("cargo nextest") :]]
        cursor = index
        while cursor + 1 < len(lines) and lines[cursor].rstrip().endswith("\\"):
            cursor += 1
            chunk.append(lines[cursor])
        command = " ".join(part.rstrip().rstrip("\\") for part in chunk)
        try:
            tokens = shlex.split(command)
        except ValueError:
            tokens = command.split()
        for token in tokens:
            if not token.startswith("-") or token == "-":
                continue
            # `--flag=value` and shell-string artefacts (a trailing quote from
            # an embedded command spelling) both normalise to the bare flag.
            flag = token.split("=", 1)[0].rstrip("\"'`")
            flags.add(flag)
    return flags


def _scripted_nextest_flags() -> dict[str, set[str]]:
    return {
        str(script.relative_to(ROOT)): _nextest_flags(script.read_text())
        for script in sorted((ROOT / "scripts").rglob("*.sh"))
        if "cargo nextest" in script.read_text()
    }


def _assert_flags_supported(
    per_script: dict[str, set[str]], pinned_version: str
) -> None:
    pinned = _parse_version(pinned_version)
    for script, flags in per_script.items():
        for flag in sorted(flags):
            minimum = NEXTEST_FLAG_MIN_VERSION.get(flag)
            assert minimum is not None, (
                f"{script} passes `{flag}` to cargo nextest, but the tool-pin "
                "contract has no minimum nextest version recorded for it. Add "
                "it to NEXTEST_FLAG_MIN_VERSION with the release that "
                "introduced it."
            )
            assert minimum <= pinned, (
                f"{script} passes `{flag}`, which nextest gained in "
                f"{'.'.join(str(part) for part in minimum)}, but the pin is "
                f"{pinned_version}."
            )


def test_pinned_nextest_accepts_every_scripted_flag() -> None:
    per_script = _scripted_nextest_flags()
    assert per_script, "no scripts/ shell script invokes cargo nextest"
    _assert_flags_supported(per_script, PINS["NEXTEST"][1])


def test_flag_contract_rejects_a_flag_newer_than_the_pin() -> None:
    try:
        _assert_flags_supported({"fixture.sh": {"--no-pager"}}, "0.9.99")
    except AssertionError:
        return
    raise AssertionError("the flag contract accepted a flag newer than the pin")


def test_flag_contract_rejects_an_unrecorded_flag() -> None:
    try:
        _assert_flags_supported(
            {"fixture.sh": {"--flag-from-the-future"}}, PINS["NEXTEST"][1]
        )
    except AssertionError:
        return
    raise AssertionError("the flag contract accepted a flag with no recorded minimum")


# ── Every install site, not just the ones spelled out above ──────────────────
#
# The assertions above prove the pinned strings are PRESENT. Presence is not
# exclusivity: a second `cargo install cargo-nextest`, a `brew install
# wasmtime`, or a wasmtime.dev/install.sh line added next to them installs a
# different version and passes every one of those assertions. So every install
# site for a pinned tool is enumerated here and each one has to carry the pin.
#
# A site that genuinely cannot take a version (the FreeBSD guest's `pkg
# install`) says so on the line above with `unpinned-by-design(<tool>)` and a
# reason. That is a declaration in the file the contract reads, not a list of
# exemptions kept somewhere else.

TOOL_CRATES = {
    "cargo-nextest": "NEXTEST",
    "cargo-deny": "CARGO_DENY",
    "cargo-about": "CARGO_ABOUT",
    "cargo-llvm-cov": "LLVM_COV",
}
TOOL_BINARIES = {"wasmtime": "WASMTIME", "wasm-pack": "WASM_PACK"}
INSTALL_ACTION_TOOLS = {
    "nextest": "NEXTEST",
    "cargo-nextest": "NEXTEST",
    "cargo-deny": "CARGO_DENY",
    "cargo-about": "CARGO_ABOUT",
    "cargo-llvm-cov": "LLVM_COV",
    "wasm-pack": "WASM_PACK",
}


def _marked_unpinned(lines: list[str], index: int, tool: str) -> bool:
    """`unpinned-by-design(<tool>)` on the line itself or in the comment above."""
    marker = f"unpinned-by-design({tool})"
    for candidate in lines[max(0, index - 6) : index + 1]:
        if marker in candidate:
            return True
    return False


_ASSIGNMENT = re.compile(
    r"^\s*(?:-\s*)?(?:run:\s*)?(?P<name>[A-Z][A-Z0-9_]*)\s*(?::\s+|=)\s*(?P<value>\S.*?)\s*$"
)
# PowerShell steps assign the workflow env into a local first
# (`$version = "${{ env.WASMTIME_VERSION }}"`), so a lowercase `$version` in the
# asset URL has to resolve too.
_PS_ASSIGNMENT = re.compile(
    r"^\s*\$(?P<name>[A-Za-z_][A-Za-z0-9_]*)\s*=\s*(?P<value>\S.*?)\s*$"
)
_REFERENCE = re.compile(
    r"\$\{\{\s*env\.(?P<a>[A-Za-z][A-Za-z0-9_]*)\s*\}\}"
    r"|\$\{(?:env:)?(?P<b>[A-Za-z][A-Za-z0-9_]*)\}"
    r"|\$(?P<c>[A-Za-z][A-Za-z0-9_]*)"
)


def _resolve_value(
    name: str, lines: list[str], index: int, depth: int = 0
) -> str | None:
    """The value a variable holds at a site, from the nearest declaration above it.

    Workflow env blocks and shell assignments both declare before they are used,
    and a job- or step-level declaration sits below the workflow-level one, so
    the nearest preceding assignment is the one in scope. Comparing the
    variable NAME instead would let `WASMTIME_VERSION: "v99.0.0"` satisfy a
    contract that pins v47.0.2 — the pin would be a spelling, not a version.

    A declaration written in terms of another variable resolves from its own
    line, not from the site, so a later reassignment cannot be read back into
    an earlier declaration.
    """
    if depth > 4:
        return None
    for cursor in range(index, -1, -1):
        line = lines[cursor]
        if line.strip().startswith("#"):
            continue
        match = _ASSIGNMENT.match(line) or _PS_ASSIGNMENT.match(line)
        if not match or match.group("name") != name:
            continue
        value = match.group("value").strip().strip("\"'")
        reference = _REFERENCE.search(value)
        if reference:
            inner = reference.group("a") or reference.group("b") or reference.group("c")
            resolved = _resolve_value(inner, lines, cursor - 1, depth + 1)
            if resolved is None:
                return None
            value = value[: reference.start()] + resolved + value[reference.end() :]
        return value.strip("\"'")
    return None


_LOOKS_LIKE_A_VERSION = re.compile(r"^v?[0-9]")


def _version_in(text: str, lines: list[str], index: int) -> str | None:
    """The version a release-asset line names, resolving any variable it uses.

    A step can hold several variables at once — `$url`, `$dest`, `$version` —
    so every reference is resolved and the first one that reads as a version is
    the answer. Resolving the name rather than reading it is the point: a site
    spelled `$WASMTIME_VERSION` in a job that sets WASMTIME_VERSION to v99.0.0
    is not pinned to v47.0.2, however familiar the spelling looks.
    """
    literal = re.search(r"/download/v?([0-9][0-9A-Za-z.\-+]*)", text)
    if literal:
        return literal.group(1)
    literal = re.search(r"gh release download\s+[\"\']?v?([0-9][0-9A-Za-z.\-+]*)", text)
    if literal:
        return literal.group(1)
    for reference in _REFERENCE.finditer(text):
        name = reference.group("a") or reference.group("b") or reference.group("c")
        resolved = _resolve_value(name, lines, index)
        if resolved and _LOOKS_LIKE_A_VERSION.match(resolved):
            return resolved
    return None


def _cargo_install_site(line: str) -> tuple[str, str | None] | None:
    """(tool key, version) for a `cargo install` line, whatever the flag order.

    `cargo install --locked cargo-nextest` installs the same crate as
    `cargo install cargo-nextest --locked`; a scan that only reads the token
    after `install` sees the first as an install of `--locked` and lets an
    unpinned installer through.
    """
    if "cargo install" not in line:
        return None
    try:
        argv = shlex.split(line[line.index("cargo install") :], comments=True)
    except ValueError:
        argv = line[line.index("cargo install") :].split()
    argv = argv[2:]
    version: str | None = None
    crate: str | None = None
    index = 0
    while index < len(argv):
        token = argv[index]
        if token in ("--version", "--vers"):
            version = argv[index + 1] if index + 1 < len(argv) else None
            index += 2
            continue
        if token.startswith(("--version=", "--vers=")):
            version = token.split("=", 1)[1]
            index += 1
            continue
        if token.startswith("-"):
            index += 1
            continue
        if crate is None:
            crate, _, at_version = token.partition("@")
            if at_version:
                version = at_version
        index += 1
    if crate is None or crate not in TOOL_CRATES:
        return None
    return TOOL_CRATES[crate], version


# Release-asset downloads, which install a tool without any package manager.
RELEASE_ASSETS = {
    "bytecodealliance/wasmtime": "WASMTIME",
    "rustwasm/wasm-pack": "WASM_PACK",
}


def install_sites(text: str, source: str) -> list[tuple[str, str, str | None]]:
    """(source:line, tool key, pinned version or None) for every install site."""
    sites: list[tuple[str, str, str | None]] = []
    lines = text.splitlines()
    for index, raw in enumerate(lines):
        line = raw.strip()
        where = f"{source}:{index + 1}"
        if line.startswith("#"):
            continue

        match = re.match(r"tools:\s*(.+)$", line)
        if match:
            for entry in match.group(1).split(","):
                entry = entry.strip().strip("'\"")
                name, _, version = entry.partition("@")
                key = INSTALL_ACTION_TOOLS.get(name)
                if key:
                    sites.append((where, key, version or None))
            continue

        cargo_site = _cargo_install_site(line)
        if cargo_site is not None:
            sites.append((where, cargo_site[0], cargo_site[1]))
            continue

        for tool, key in TOOL_BINARIES.items():
            if re.search(rf"brew install [^\n]*\b{tool}\b", line) or re.search(
                rf"pkg install [^\n]*\b{tool}\b", line
            ):
                sites.append(
                    (
                        where,
                        key,
                        "unpinned-by-design"
                        if _marked_unpinned(lines, index, tool)
                        else None,
                    )
                )

        window = " ".join(lines[index : index + 3])
        for repository, key in RELEASE_ASSETS.items():
            if repository not in window:
                continue
            if "gh release download" in line or "releases/download" in line:
                sites.append((where, key, _version_in(window, lines, index)))
                break

        if "wasmtime.dev/install.sh" in line:
            version = None
            match = re.search(r"--version[= ]([A-Za-z0-9.\-+]+)", line)
            if match:
                version = match.group(1)
            elif _marked_unpinned(lines, index, "wasmtime"):
                version = "unpinned-by-design"
            sites.append((where, "WASMTIME", version))
    return sites


def _assert_every_site_pinned(sites: list[tuple[str, str, str | None]]) -> None:
    for where, key, version in sites:
        assert version is not None, (
            f"{where}: installs {PINS[key][0]} without a version. Pin it to "
            f"{PINS[key][1]}, or write `unpinned-by-design({PINS[key][0]})` with "
            f"a reason on the line above if the installer takes no version."
        )
        if version == "unpinned-by-design":
            continue
        expected = PINS[key][1]
        assert version.lstrip("v") == expected.lstrip("v"), (
            f"{where}: installs {PINS[key][0]} {version}, but the contract pins "
            f"{expected}"
        )


def _workflow_sources() -> list[tuple[str, str]]:
    root = ROOT / ".github"
    return [
        (str(path.relative_to(ROOT)), path.read_text())
        for path in sorted(root.rglob("*.yml"))
    ]


def test_every_install_site_carries_the_pin() -> None:
    total = 0
    for source, text in _workflow_sources():
        sites = install_sites(text, source)
        total += len(sites)
        _assert_every_site_pinned(sites)
    assert total >= 12, f"only {total} install sites found; the scan lost its subject"


def test_the_enumeration_rejects_a_second_unpinned_installer() -> None:
    text = (
        "      - run: cargo install cargo-nextest@0.9.120 --locked\n"
        "      - run: cargo install cargo-nextest --locked\n"
    )
    try:
        _assert_every_site_pinned(install_sites(text, "fixture.yml"))
    except AssertionError as error:
        assert "without a version" in str(error), error
        return
    raise AssertionError("an unpinned installer beside a pinned one was accepted")


def test_the_enumeration_rejects_an_unpinned_package_manager_install() -> None:
    try:
        _assert_every_site_pinned(
            install_sites("          brew install wasmtime\n", "fixture.yml")
        )
    except AssertionError as error:
        assert "without a version" in str(error), error
        return
    raise AssertionError("`brew install wasmtime` was accepted with no declaration")


def test_the_enumeration_accepts_a_declared_unpinnable_install() -> None:
    text = (
        "          # unpinned-by-design(wasmtime): pkg serves one per branch.\n"
        "          pkg install -y -r FreeBSD wasmtime\n"
    )
    _assert_every_site_pinned(install_sites(text, "fixture.yml"))


def test_the_enumeration_rejects_an_unpinned_install_with_the_flags_first() -> None:
    """`cargo install --locked cargo-nextest` installs the same crate.

    Reading the token after `install` as the crate name sees `--locked` there,
    finds no such crate, and records no install site at all — an unpinned
    nextest installer that the enumeration never looks at.
    """
    try:
        _assert_every_site_pinned(
            install_sites(
                "      - run: cargo install --locked cargo-nextest\n", "f.yml"
            )
        )
    except AssertionError as error:
        assert "without a version" in str(error), error
        return
    raise AssertionError("`cargo install --locked cargo-nextest` was accepted unpinned")


def test_the_enumeration_reads_a_pin_that_follows_the_flags() -> None:
    sites = install_sites(
        "      - run: cargo install --locked --version 0.9.120 cargo-nextest\n", "f.yml"
    )
    assert sites == [("f.yml:1", "NEXTEST", "0.9.120")], sites
    _assert_every_site_pinned(sites)


def test_the_enumeration_rejects_a_release_asset_download_at_the_wrong_version() -> (
    None
):
    """A `curl` of a GitHub release asset installs a tool with no package manager.

    The wasm-pack install in .github/actions/setup-wasm-pack is exactly this
    shape: nothing named `install`, no `tools:` entry — just a download URL.
    Unenumerated, it could name any version and every pin assertion above would
    still pass.
    """
    text = (
        "        WASM_PACK_VERSION=0.13.9\n"
        '        curl -fsSL "https://github.com/rustwasm/wasm-pack/releases/download'
        '/v${WASM_PACK_VERSION}/wasm-pack.tar.gz"\n'
    )
    try:
        _assert_every_site_pinned(install_sites(text, "fixture.yml"))
    except AssertionError as error:
        assert "the contract pins" in str(error), error
        return
    raise AssertionError("a release-asset download at the wrong version was accepted")


def test_the_enumeration_rejects_a_release_asset_download_with_no_version() -> None:
    text = (
        '        curl -fsSL "https://github.com/rustwasm/wasm-pack/releases/download'
        '/${WASM_PACK_TAG}/wasm-pack.tar.gz"\n'
    )
    try:
        _assert_every_site_pinned(install_sites(text, "fixture.yml"))
    except AssertionError as error:
        assert "without a version" in str(error), error
        return
    raise AssertionError("a release-asset download naming no version was accepted")


def test_a_site_is_pinned_by_the_value_its_variable_holds_not_its_spelling() -> None:
    """The variable name is not the pin; the value it resolves to is.

    A job that sets WASMTIME_VERSION to v99.0.0 and then downloads
    "$WASMTIME_VERSION" is spelled exactly like the pinned sites and installs a
    different wasmtime. Matching on the name accepts it.
    """
    text = (
        '  WASMTIME_VERSION: "v99.0.0"\n'
        '            gh release download "$WASMTIME_VERSION" '
        "--repo bytecodealliance/wasmtime \\\n"
    )
    sites = install_sites(text, "fixture.yml")
    assert sites == [("fixture.yml:2", "WASMTIME", "v99.0.0")], sites
    try:
        _assert_every_site_pinned(sites)
    except AssertionError as error:
        assert "the contract pins" in str(error), error
        return
    raise AssertionError("a site resolving to an off-pin version was accepted")


def test_a_variable_resolves_through_the_local_it_is_copied_into() -> None:
    """PowerShell steps copy the workflow env into a local before using it."""
    text = (
        '  WASMTIME_VERSION: "v47.0.2"\n'
        '          $version = "${{ env.WASMTIME_VERSION }}"\n'
        '          $url = "https://github.com/bytecodealliance/wasmtime/releases'
        '/download/$version/wasmtime-$version-x86_64-windows.zip"\n'
    )
    sites = install_sites(text, "fixture.yml")
    assert sites == [("fixture.yml:3", "WASMTIME", "v47.0.2")], sites
    _assert_every_site_pinned(sites)


def test_the_enumeration_rejects_an_install_site_at_the_wrong_version() -> None:
    try:
        _assert_every_site_pinned(
            install_sites("          tools: nextest@0.9.99\n", "fixture.yml")
        )
    except AssertionError as error:
        assert "the contract pins" in str(error), error
        return
    raise AssertionError("an install site at the wrong version was accepted")


if __name__ == "__main__":
    test_workflow_pins_match_the_xtask_graph()
    test_pin_contract_rejects_workflow_drift()
    test_pin_contract_rejects_installer_drift()
    test_pinned_nextest_accepts_every_scripted_flag()
    test_flag_contract_rejects_a_flag_newer_than_the_pin()
    test_flag_contract_rejects_an_unrecorded_flag()
    test_every_install_site_carries_the_pin()
    test_the_enumeration_rejects_a_second_unpinned_installer()
    test_the_enumeration_rejects_an_unpinned_package_manager_install()
    test_the_enumeration_accepts_a_declared_unpinnable_install()
    test_the_enumeration_rejects_an_unpinned_install_with_the_flags_first()
    test_the_enumeration_reads_a_pin_that_follows_the_flags()
    test_the_enumeration_rejects_a_release_asset_download_at_the_wrong_version()
    test_the_enumeration_rejects_a_release_asset_download_with_no_version()
    test_a_site_is_pinned_by_the_value_its_variable_holds_not_its_spelling()
    test_a_variable_resolves_through_the_local_it_is_copied_into()
    test_the_enumeration_rejects_an_install_site_at_the_wrong_version()
