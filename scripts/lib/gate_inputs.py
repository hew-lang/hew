#!/usr/bin/env python3
"""Single authority for CI gate-input declarations.

The static Linux shard checker uses these facts to require every participating
gate exactly once, and scripts/check-gate-reachability.py asserts that the
declarations stay honest. Its missing-input discoveries are advisory.

# The declarations

Each gate names the paths it reads on an `# inputs:` line above its recipe in
the Makefile. A gate that runs in a different venue, or that is an aggregate
of other gates, may add `# preflight: never` with a reason.

# Consumer scan

Rust, Python, shell, and Make recipe sources declared or executed by a gate are
scanned for tracked path literals, and Rust includes are resolved to tracked
assets. Static analysis cannot enumerate every way a consumer constructs a
path, so a missing-input discovery is reported as advisory. Rust assets
compiled with `include_str!`/`include_bytes!` are checked separately so their
paths cannot silently leave the tracked tree.

# Glob syntax

`*` matches any characters INCLUDING `/`; `?` matches one character.  So
`hew-runtime/src/*.rs` covers nested modules and `*.hew` covers the corpus.

"""

from __future__ import annotations

import ast
import hashlib
import json
import re
import subprocess
from collections.abc import Iterable
from dataclasses import dataclass
from pathlib import Path

TARGET_RE = re.compile(r"^([A-Za-z0-9._-]+):")
PY_IMPORT_RE = re.compile(
    r"^\s*(?:from\s+([A-Za-z_][\w.]*)|import\s+([A-Za-z_][\w.]*))"
)
INCLUDE_OPEN_RE = re.compile(r"include_(?:str|bytes)!\s*\(")
STRING_LITERAL_RE = re.compile(r"\"([^\"]*)\"")


@dataclass(frozen=True)
class Gate:
    target: str
    globs: tuple[str, ...]
    preflight: str

    def participates(self) -> bool:
        return not self.preflight.startswith("never")

    def includes(self) -> tuple[str, ...]:
        return tuple(glob for glob in self.globs if not glob.startswith("!"))

    def excludes(self) -> tuple[str, ...]:
        return tuple(glob[1:] for glob in self.globs if glob.startswith("!"))

    def reads(self, path: str) -> bool:
        """Whether this gate reads a path.

        A leading `!` excludes. A tree-wide scanner can omit paths it does not
        read without widening every matching change into that gate.
        """
        if any(matches(glob, path) for glob in self.excludes()):
            return False
        return any(matches(glob, path) for glob in self.includes())

    def specific_globs(self) -> tuple[str, ...]:
        """Globs that prove a path is classified.

        A gate whose only positive input is the bare `*` is a TREE-WIDE SCANNER
        (leak-scan, lint-wasm-todo): it matches nearly every path, so counting
        it as coverage would make the fail-closed answer unreachable.
        """
        return tuple(glob for glob in self.includes() if glob != "*")

    def reads_specifically(self, path: str) -> bool:
        if any(matches(glob, path) for glob in self.excludes()):
            return False
        return any(matches(glob, path) for glob in self.specific_globs())


@dataclass
class Declarations:
    gates: list[Gate]
    global_globs: list[str]
    no_gate_globs: list[str]
    embeds: dict[str, list[str]]


_GLOB_CACHE: dict[str, re.Pattern[str]] = {}


def glob_to_regex(pattern: str) -> re.Pattern[str]:
    cached = _GLOB_CACHE.get(pattern)
    if cached is not None:
        return cached
    out: list[str] = []
    for char in pattern:
        if char == "*":
            out.append(".*")
        elif char == "?":
            out.append(".")
        else:
            out.append(re.escape(char))
    compiled = re.compile("^" + "".join(out) + "$")
    _GLOB_CACHE[pattern] = compiled
    return compiled


def matches(pattern: str, path: str) -> bool:
    return glob_to_regex(pattern).match(path) is not None


def parse_makefile(text: str) -> tuple[list[Gate], list[str], list[str]]:
    gates: list[Gate] = []
    global_globs: list[str] = []
    no_gate_globs: list[str] = []
    pending: list[str] = []
    preflight = ""
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith("# inputs:"):
            pending.extend(stripped[len("# inputs:") :].split())
            continue
        if stripped.startswith("# preflight:"):
            preflight = stripped[len("# preflight:") :].strip()
            continue
        if stripped.startswith("# global-input:"):
            global_globs.append(stripped[len("# global-input:") :].split(None, 1)[0])
            continue
        if stripped.startswith("# no-gate:"):
            no_gate_globs.append(stripped[len("# no-gate:") :].split(None, 1)[0])
            continue
        if line.startswith("#") or not line.strip():
            continue
        match = TARGET_RE.match(line)
        if match and pending:
            # A `<gate>-build` rule prepares artifacts and is not a gate in its
            # own right, so it declares no inputs of its own.
            if not match.group(1).endswith("-build"):
                gates.append(Gate(match.group(1), tuple(pending), preflight))
        pending = []
        preflight = ""
    return gates, global_globs, no_gate_globs


# ── derivation: the paths a gate's source closure names ──────────────────────
#
# The honest rule, and the only one that does not need a perfect static
# consumer analysis: ANY REPO-TRACKED PATH LITERAL APPEARING IN A GATE'S SOURCE
# CLOSURE IS AN INPUT OF THAT GATE.
#
# Three narrower walks preceded it and each missed a class:
#   * resolving only `<module>.py` imports missed package imports through
#     `__init__.py`, and missed a file opened with `Path.read_text`/`open`;
#   * following only `source`/`.` missed a helper a script EXECUTES
#     (`bash scripts/x.sh`, `python3 scripts/y.py`);
#   * reading only `include_str!`/`include_bytes!` missed a Rust test that
#     reads its fixture through `std::fs`.
# A literal is a literal in all four cases, so one scanner answers all of them.

SOURCE_SUFFIXES = (".rs", ".py", ".sh", ".mjs")
HELPER_SUFFIXES = (".py", ".sh", ".mjs")
CLOSURE_DEPTH_LIMIT = 12

# A path-shaped token. `$`/`{`/`}` are admitted so a literal written through
# a variable survives to _candidates, which resolves its repository-local tail.
PATH_TOKEN_RE = re.compile(r"[$A-Za-z0-9_.{][A-Za-z0-9_./+${}-]*")
# Every string literal in Rust/Python/shell/Make source, plus bare tokens on a
# recipe line (a Makefile names paths unquoted).
QUOTED_RE = re.compile(r"\"([^\"\n]{2,240})\"|'([^'\n]{2,240})'")


def _tracked(root: Path) -> tuple[frozenset[str], frozenset[str]]:
    """Every tracked file, and every directory prefix containing one."""
    result = subprocess.run(
        ["git", "ls-files"], cwd=root, check=True, capture_output=True, text=True
    )
    files = frozenset(line for line in result.stdout.splitlines() if line)
    directories = set()
    for rel in files:
        parts = rel.split("/")
        for index in range(1, len(parts)):
            directories.add("/".join(parts[:index]))
    return files, frozenset(directories)


_TRACKED_CACHE: dict[str, tuple[frozenset[str], frozenset[str]]] = {}


def tracked_paths(root: Path) -> tuple[frozenset[str], frozenset[str]]:
    key = str(root)
    if key not in _TRACKED_CACHE:
        _TRACKED_CACHE[key] = _tracked(root)
    return _TRACKED_CACHE[key]


def _candidates(literal: str, origin: str, crate: str | None) -> list[str]:
    """Every repository-relative reading of one literal.

    A literal is written relative to the repository, to the file that contains
    it, or to the crate root (`concat!(env!("CARGO_MANIFEST_DIR"), ...)`), and
    which one it is cannot be known without resolving it; so all three are
    offered and only the ones that name a tracked path are kept.
    """
    literal = literal.strip()
    if not literal or literal.startswith(("http://", "https://", "-")):
        return []
    if "\\" in literal or "%" in literal:
        return []
    if "$" in literal or "{" in literal:
        # A path written through a variable — `"${ROOT}/scripts/fuzz/run-oracle.py"`
        # is how every script in this tree names a sibling. The expansion cannot
        # be resolved here, but the tail after it can: each `/`-boundary suffix
        # is offered, and only one that names a tracked path is kept, so a
        # coincidental match needs the whole remaining path to line up.
        tail = literal
        readings = []
        while "/" in tail:
            tail = tail.split("/", 1)[1]
            if "/" in tail and "$" not in tail and "{" not in tail:
                readings.append(tail)
        return [_normalise(Path(reading)) for reading in readings if reading]
    readings = [literal]
    parent = str(Path(origin).parent)
    if parent not in (".", ""):
        readings.append(_normalise(Path(parent) / literal))
    if crate:
        readings.append(_normalise(Path(crate) / literal))
    return [_normalise(Path(reading)) for reading in readings if reading]


def _literals(text: str) -> list[str]:
    seen: list[str] = []
    for quoted, single in QUOTED_RE.findall(text):
        value = quoted or single
        if value and PATH_TOKEN_RE.fullmatch(value):
            seen.append(value)
    return seen


def _makefile_bare_tokens(text: str) -> list[str]:
    """Paths a recipe names without quotes."""
    found: list[str] = []
    for line in text.splitlines():
        if not line.startswith("\t"):
            continue
        for token in line.split():
            if "/" in token and PATH_TOKEN_RE.fullmatch(token):
                found.append(token)
    return found


def _crate_of(root: Path, rel: str) -> str | None:
    parent = Path(rel).parent
    while str(parent) not in (".", ""):
        if (root / parent / "Cargo.toml").is_file():
            return str(parent)
        parent = parent.parent
    return None


def source_closure(root: Path, seeds: Iterable[str]) -> list[str]:
    """Files reachable from the seeds by NAMING a repo-local script.

    Sourced, imported and executed alike: a helper a gate runs contributes its
    own literals, however the gate reaches it.  Bounded in depth and
    cycle-safe, because a pair of scripts that name each other would otherwise
    walk forever.
    """
    files, _directories = tracked_paths(root)
    seen: set[str] = set()
    order: list[str] = []
    frontier = [(seed, 0) for seed in seeds]
    while frontier:
        rel, depth = frontier.pop(0)
        if rel in seen or depth > CLOSURE_DEPTH_LIMIT:
            continue
        if rel not in files or not (root / rel).is_file():
            continue
        seen.add(rel)
        order.append(rel)
        try:
            text = (root / rel).read_text(errors="replace")
        except OSError:
            continue
        crate = _crate_of(root, rel)
        for literal in _literals(text):
            for candidate in _candidates(literal, rel, crate):
                if candidate in files and candidate.endswith(HELPER_SUFFIXES):
                    frontier.append((candidate, depth + 1))
        # `import x` / `from x import` name a module, not a path.
        if rel.endswith(".py"):
            for module in _python_modules(root, rel, text):
                frontier.append((module, depth + 1))
    return order


def _python_modules(root: Path, rel: str, text: str) -> list[str]:
    """Repository modules a Python file imports, including explicit file specs."""
    directory = Path(rel).parent
    search = _search_path(rel, text) + [directory, Path("scripts"), Path("scripts/lib")]
    found: list[str] = []
    for line in text.splitlines():
        match = PY_IMPORT_RE.match(line)
        if not match:
            continue
        module = (match.group(1) or match.group(2)).split(".")[0]
        for base in search:
            for candidate in (
                base / f"{module}.py",
                base / module / "__init__.py",
            ):
                if (root / candidate).is_file():
                    found.append(_normalise(candidate))
                    break
            else:
                continue
            break
    if "spec_from_file_location" in text:
        try:
            tree = ast.parse(text)
        except SyntaxError:
            tree = None
        files, _directories = tracked_paths(root)
        for node in ast.walk(tree) if tree is not None else ():
            if not isinstance(node, ast.Call) or len(node.args) < 2:
                continue
            function = node.func
            if not (
                isinstance(function, ast.Attribute)
                and function.attr == "spec_from_file_location"
            ):
                continue
            for value in ast.walk(node.args[1]):
                if not (
                    isinstance(value, ast.Constant)
                    and isinstance(value.value, str)
                    and value.value.endswith(".py")
                ):
                    continue
                for candidate in _candidates(value.value, rel, None):
                    if candidate in files and candidate not in found:
                        found.append(candidate)
                        break
    return found


def _search_path(rel: str, text: str) -> list[Path]:
    """Directories a file adds to `sys.path`, in the order it adds them.

    Two helpers named `bounded_subprocess.py` exist in this tree with different
    APIs; which one a test imports is decided by its own `sys.path.insert`, so
    honouring the hint is the difference between attaching the right helper to
    the right gate and attaching the wrong one.
    """
    directory = Path(rel).parent
    found: list[Path] = []
    for line in text.splitlines():
        if "sys.path.insert" not in line and "sys.path.append" not in line:
            continue
        segments = [value or single for value, single in QUOTED_RE.findall(line)]
        segments = [segment for segment in segments if segment]
        if not segments:
            continue
        base = directory if "__file__" in line else Path(".")
        if "parents[2]" in line or "parents[1]" in line:
            base = Path(".")
        candidate = base
        for segment in segments:
            candidate = candidate / segment
        found.append(Path(_normalise(candidate)))
    return found


def gate_seeds(
    root: Path,
    gate: "Gate",
    recipes: dict[str, str],
    prerequisites: dict[str, list[str]],
) -> tuple[list[str], str]:
    """Return a gate's source seeds and its transitive Make recipe text."""
    files, _directories = tracked_paths(root)
    seeds: list[str] = [
        rel
        for rel in files
        if rel.endswith(SOURCE_SUFFIXES) and gate.reads_specifically(rel)
    ]
    seen_targets: set[str] = set()
    queue = [gate.target]
    crates: set[str] = set()
    recipe_texts: list[str] = []
    while queue:
        target = queue.pop(0)
        if target in seen_targets:
            continue
        seen_targets.add(target)
        queue.extend(prerequisites.get(target, []))
        recipe = recipes.get(target, "")
        recipe_texts.append(recipe)
        for token in _makefile_bare_tokens(recipe) + _literals(recipe):
            for candidate in _candidates(token, "Makefile", None):
                if candidate in files and candidate.endswith(HELPER_SUFFIXES):
                    seeds.append(candidate)
        for package in re.findall(r"-p\s+([A-Za-z0-9_-]+)", recipe):
            crates.add(package)
    if crates:
        packages = {
            str(Path(manifest).parent): name
            for name, manifest in _workspace_manifests(root).items()
        }
        for directory, name in packages.items():
            if name in crates:
                seeds.extend(
                    rel
                    for rel in files
                    if rel.startswith(directory + "/") and rel.endswith(".rs")
                )
    return sorted(set(seeds)), "".join(recipe_texts)


_MANIFEST_CACHE: dict[str, dict[str, str]] = {}


def _workspace_manifests(root: Path) -> dict[str, str]:
    """Package name -> manifest path, for every workspace member."""
    key = str(root)
    if key in _MANIFEST_CACHE:
        return _MANIFEST_CACHE[key]
    result = subprocess.run(
        ["cargo", "metadata", "--no-deps", "--format-version", "1"],
        cwd=root,
        check=True,
        capture_output=True,
        text=True,
    )
    manifests: dict[str, str] = {}
    for package in json.loads(result.stdout)["packages"]:
        manifest = Path(package["manifest_path"])
        manifests[package["name"]] = str(manifest.relative_to(root.resolve()))
    _MANIFEST_CACHE[key] = manifests
    return manifests


def _literal_inputs_from_text(root: Path, rel: str, text: str) -> set[str]:
    """Tracked files and directory prefixes named by one source."""
    files, directories = tracked_paths(root)
    found: set[str] = set()
    crate = _crate_of(root, rel)
    tokens = _literals(text)
    if rel == "Makefile":
        tokens += _makefile_bare_tokens(text)
    for literal in tokens:
        for candidate in _candidates(literal, rel, crate):
            if candidate in files:
                found.add(candidate)
            elif candidate in directories and "/" in candidate:
                # Nested prefixes name fixture trees. Bare top-level words are
                # too ambiguous: ordinary prose and package names would make
                # nearly every gate claim an entire repository subtree.
                found.add(candidate + "/*")
    return found


def literal_inputs(
    root: Path, closure: Iterable[str], recipe_text: str = ""
) -> list[str]:
    """Tracked paths named anywhere in a source closure or its Make recipes."""
    found: set[str] = set()
    for rel in closure:
        try:
            text = (root / rel).read_text(errors="replace")
        except OSError:
            continue
        found.update(_literal_inputs_from_text(root, rel, text))
    if recipe_text:
        found.update(_literal_inputs_from_text(root, "Makefile", recipe_text))
    return sorted(found)


def expand_gates(root: Path, gates: list[Gate], makefile_text: str) -> list[Gate]:
    """Add to each gate the paths its own source closure names."""
    recipes, prerequisites = _parse_rules(makefile_text)
    expanded: list[Gate] = []
    for gate in gates:
        seeds, recipe_text = gate_seeds(root, gate, recipes, prerequisites)
        closure = source_closure(root, seeds)
        # Retain duplicates here. The expanded declarations are advisory-only,
        # and the suffix after the explicit declarations is the exact literal
        # evidence found by the consumer scan.
        extra = literal_inputs(root, closure, recipe_text)
        expanded.append(Gate(gate.target, gate.globs + tuple(extra), gate.preflight))
    return expanded


def _parse_rules(text: str) -> tuple[dict[str, str], dict[str, list[str]]]:
    recipes: dict[str, str] = {}
    prerequisites: dict[str, list[str]] = {}
    current = ""
    for line in text.splitlines():
        if line.startswith("\t") and current:
            recipes[current] = recipes.get(current, "") + line + "\n"
            continue
        match = TARGET_RE.match(line)
        if not match:
            current = ""
            continue
        current = match.group(1)
        tail = line[len(current) + 1 :]
        prerequisites.setdefault(current, []).extend(
            token
            for token in tail.replace("|", " ").split()
            if not token.startswith("$") and token != "="
        )
    return recipes, prerequisites


# ── derivation: assets compiled into a crate ─────────────────────────────────


def crate_directories(root: Path) -> list[str]:
    result = subprocess.run(
        ["git", "ls-files", "*Cargo.toml", "Cargo.toml"],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
    )
    directories = []
    for line in result.stdout.splitlines():
        directory = str(Path(line).parent)
        if directory not in (".", ""):
            directories.append(directory)
    return sorted(set(directories))


def _normalise(path: Path) -> str:
    parts: list[str] = []
    for part in path.as_posix().split("/"):
        if part == "..":
            if parts:
                parts.pop()
        elif part not in ("", "."):
            parts.append(part)
    return "/".join(parts)


def _split_concat_arguments(argument: str) -> list[str]:
    """Top-level comma-separated parts of a `concat!( ... )` argument."""
    inner = argument.strip()
    start = inner.find("concat!")
    if start == -1:
        return []
    inner = inner[inner.find("(", start) + 1 :]
    depth = 0
    parts: list[str] = []
    current: list[str] = []
    for char in inner:
        if char == "(":
            depth += 1
        elif char == ")":
            if depth == 0:
                break
            depth -= 1
        if char == "," and depth == 0:
            parts.append("".join(current))
            current = []
            continue
        current.append(char)
    parts.append("".join(current))
    return parts


def _include_arguments(text: str) -> list[tuple[str, bool]]:
    """Every `include_str!`/`include_bytes!` path, and whether it is crate-anchored.

    Two spellings appear in this tree: a plain literal, resolved against the
    including file, and `concat!(env!("CARGO_MANIFEST_DIR"), "/../x")`, resolved
    against the crate root.  Only reading the first spelling is how
    docs/syntax-data.json came to be declared as read by nothing while
    hew-lexer compiled it in.
    """
    found: list[tuple[str, bool]] = []
    for opening in INCLUDE_OPEN_RE.finditer(text):
        index = opening.end()
        depth = 1
        while index < len(text) and depth:
            if text[index] == "(":
                depth += 1
            elif text[index] == ")":
                depth -= 1
            index += 1
        argument = text[opening.end() : index - 1]
        if "OUT_DIR" in argument:
            # A build-script product, not a tracked input.
            continue
        literals = STRING_LITERAL_RE.findall(argument)
        if "concat!" in argument:
            # A concat! whose parts are not all literals (a macro variable
            # interpolating a fixture name) has no single static answer, and
            # guessing its literal prefix yields a DIRECTORY that no file
            # matches. Skip it: the same fixtures are almost always named
            # literally elsewhere, and a wrong answer here would fail A8.
            parts = [
                part.strip()
                for part in _split_concat_arguments(argument)
                if part.strip()
            ]
            resolvable = all(
                part.startswith('"') or part.startswith('env!("CARGO_MANIFEST_DIR")')
                for part in parts
            )
            if not resolvable:
                continue
            joined = "".join(part for part in literals if part != "CARGO_MANIFEST_DIR")
            if joined:
                found.append((joined.lstrip("/"), "CARGO_MANIFEST_DIR" in argument))
        elif literals:
            found.append((literals[0], False))
    return found


def embedded_assets(root: Path) -> dict[str, list[str]]:
    """Map an embedded file to the crate directories that compile it in.

    `include_str!`/`include_bytes!` make the file a build input of the crate as
    surely as a .rs source does, and the reverse-dependency closure cannot see
    it because it is not a Rust file.
    """
    crates = crate_directories(root)
    sources = subprocess.run(
        ["git", "ls-files", "*.rs"],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
    ).stdout.splitlines()
    embeds: dict[str, list[str]] = {}
    for rel in sources:
        path = root / rel
        try:
            text = path.read_text(errors="replace")
        except OSError:
            continue
        if "include_str!" not in text and "include_bytes!" not in text:
            continue
        owner = ""
        for crate in crates:
            if rel.startswith(crate + "/") and len(crate) > len(owner):
                owner = crate
        for raw, anchored_at_crate_root in _include_arguments(text):
            base = Path(owner) if anchored_at_crate_root else Path(rel).parent
            target = _normalise(base / raw)
            if owner and target:
                embeds.setdefault(target, [])
                if owner not in embeds[target]:
                    embeds[target].append(owner)
    return embeds


CACHE_PATH = "target/preflight/gate-inputs.json"


def _tree_digest(root: Path) -> str:
    """A fingerprint of everything the expansion reads.

    The staged index covers tracked content; the size and mtime of anything
    git reports as changed covers the working tree.  A stale answer here would
    route on yesterday's declarations, so the digest has to move whenever any
    file the scanner reads does.
    """
    hasher = hashlib.sha256()
    index = subprocess.run(
        ["git", "ls-files", "-s"], cwd=root, check=False, capture_output=True
    )
    hasher.update(index.stdout)
    status = subprocess.run(
        ["git", "status", "--porcelain"],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
    )
    for line in sorted(status.stdout.splitlines()):
        rel = line[3:].strip().strip('"')
        hasher.update(line.encode())
        try:
            stat = (root / rel).stat()
        except OSError:
            continue
        hasher.update(f"{stat.st_size}:{stat.st_mtime_ns}".encode())
    return hasher.hexdigest()


def declared(root: Path) -> Declarations:
    """Return only explicit Makefile gate declarations."""
    text = (root / "Makefile").read_text()
    gates, global_globs, no_gate_globs = parse_makefile(text)
    return Declarations(
        gates=gates,
        global_globs=global_globs,
        no_gate_globs=no_gate_globs,
        embeds={},
    )


def _expand(root: Path) -> Declarations:
    base = declared(root)
    text = (root / "Makefile").read_text()
    return Declarations(
        gates=expand_gates(root, base.gates, text),
        global_globs=base.global_globs,
        no_gate_globs=base.no_gate_globs,
        embeds=embedded_assets(root),
    )


def load(root: Path) -> Declarations:
    """Return cached advisory consumer-scan results.

    The scan reads every file in every gate's closure, which is most of the
    tree. Its cache is an optimisation for lint diagnostics only; the static
    shard assignment never reads these results.
    """
    digest = _tree_digest(root)
    cache = root / CACHE_PATH
    try:
        stored = json.loads(cache.read_text())
        if stored.get("digest") == digest:
            return Declarations(
                gates=[
                    Gate(entry["target"], tuple(entry["globs"]), entry["preflight"])
                    for entry in stored["gates"]
                ],
                global_globs=stored["global_globs"],
                no_gate_globs=stored["no_gate_globs"],
                embeds=stored["embeds"],
            )
    except (OSError, ValueError, KeyError):
        pass

    declarations = _expand(root)
    try:
        cache.parent.mkdir(parents=True, exist_ok=True)
        cache.write_text(
            json.dumps(
                {
                    "digest": digest,
                    "gates": [
                        {
                            "target": gate.target,
                            "globs": list(gate.globs),
                            "preflight": gate.preflight,
                        }
                        for gate in declarations.gates
                    ],
                    "global_globs": declarations.global_globs,
                    "no_gate_globs": declarations.no_gate_globs,
                    "embeds": declarations.embeds,
                }
            )
        )
    except OSError:
        pass
    return declarations


def likely_undeclared_inputs(
    root: Path, scanned: Declarations | None = None
) -> dict[str, list[str]]:
    """Scanner discoveries absent from a gate's explicit declarations."""
    base = declared(root)
    scanned = scanned or load(root)
    explicit = {gate.target: gate for gate in base.gates}
    likely: dict[str, list[str]] = {}
    for gate in scanned.gates:
        declared_gate = explicit.get(gate.target)
        if declared_gate is None:
            continue
        missing = sorted(
            path
            for path in gate.specific_globs()
            if not declared_gate.reads_specifically(path)
            and not any(matches(glob, path) for glob in base.global_globs)
        )
        if missing:
            likely[gate.target] = missing
    return likely
