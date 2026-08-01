#!/usr/bin/env python3
"""Counterfactual contracts for the syntax-node authority ratchets."""

from __future__ import annotations

import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "scripts/structural-authority-audit.py"
INVENTORY_HEADER = "group\tform\tpath\tcount\tretirement_stage\treason\n"
PRESENTATION_HEADER = (
    "path\tline\tcolumn\tform\tcontext_form\tcategory\tretirement_stage\treason\n"
)


def run(
    root: Path, *, ast_grep: Path | None = None
) -> subprocess.CompletedProcess[str]:
    command = ["python3", str(AUDIT), "--root", str(root)]
    if ast_grep is not None:
        command.extend(("--ast-grep", str(ast_grep)))
    return subprocess.run(command, text=True, capture_output=True)


def set_inventory(root: Path, body: str = "", *, floor: int | None = None) -> None:
    (root / "scripts/structural-authority-inventory.tsv").write_text(
        INVENTORY_HEADER + body
    )
    presentation = root / "scripts/structural-authority-presentation.tsv"
    presentation_count = len(
        [
            line
            for line in presentation.read_text().splitlines()[1:]
            if line.strip() and not line.startswith("#")
        ]
    )
    row_count = len([line for line in body.splitlines() if line.strip()])
    expected_floor = row_count + presentation_count if floor is None else floor
    (root / "scripts/corpus-floors.tsv").write_text(
        "structural-authority-inventory\texact\t"
        f"{expected_floor}\t-\ttemporary authority inventory\n"
    )


with tempfile.TemporaryDirectory() as temp:
    work = Path(temp)
    (work / "scripts").mkdir()
    (work / "scripts/structural-authority-presentation.tsv").write_text(
        PRESENTATION_HEADER
    )
    source = work / "hew-mir/src/lower"
    source.mkdir(parents=True)
    target = source / "new_authority.rs"
    set_inventory(work)

    # A dead executable that silently returns ast-grep's no-match status must
    # fail the mandatory positive parser/query sentinel.
    fake_ast_grep = work / "fake-ast-grep"
    fake_ast_grep.write_text("#!/bin/sh\nexit 1\n")
    fake_ast_grep.chmod(0o755)
    result = run(work, ast_grep=fake_ast_grep)
    assert result.returncode != 0, "a non-parser executable must fail closed"
    assert "sentinel failed closed" in result.stderr

    # Neither comment tokens nor literal nodes are executable syntax findings,
    # including identifier-shaped text inside a macro string token.
    target.write_text(
        '// short_name(name); name.rsplit("::"); HashMap<SpanKey, SiteId>\n'
        'const DECOY: &str = "short_name(name); name.rsplit(\\"::\\")";\n'
        'fn macro_decoy() { format!("short_name(name) name.rsplit(\\"::\\")"); }\n'
    )
    assert run(work).returncode == 0, "comments and strings must not create findings"

    target.write_text("fn authority() { let _ = short_name(name); }\n")
    result = run(work)
    assert result.returncode != 0, "a new semantic short-name use must fail"
    assert "short-name-identifier" in result.stderr

    # Macro token trees are not call-expression ASTs, but their parsed
    # identifier/field-identifier nodes remain mandatory inventory findings.
    target.write_text('fn authority() { format!("{}", short_name(name)); }\n')
    result = run(work)
    assert result.returncode != 0, "short_name inside a production macro must fail"
    assert "short-name-identifier" in result.stderr
    target.write_text(
        'fn authority() { format!("{}", name.rsplit("::").next().unwrap_or(name)); }\n'
    )
    result = run(work)
    assert result.returncode != 0, "rsplit leaf extraction inside a macro must fail"
    assert "leaf-rsplit-" in result.stderr

    # Parsed cfg(test) module and item ranges are excluded, including macro
    # token trees and forbidden-looking type syntax nested inside them.
    target.write_text(
        "#[cfg(test)]\n"
        "mod tests {\n"
        '    fn macro_authority() { format!("{}", short_name(name)); }\n'
        '    fn macro_leaf() { format!("{}", name.rsplit("::").next()); }\n'
        "    fn scalar() { let _: HashMap<SpanKey, SiteId> = HashMap::new(); }\n"
        "}\n"
        "#[cfg(test)]\n"
        'fn item_macro() { format!("{}", short_name(name)); }\n'
        "fn production() {}\n"
    )
    assert run(work).returncode == 0, "parsed test-only module/items must be excluded"

    # hew-analysis is an audited production root, not a display-shaped escape.
    target.write_text("fn production() {}\n")
    analysis = work / "hew-analysis/src/new_display.rs"
    analysis.parent.mkdir(parents=True)
    analysis.write_text('fn display() { format!("{}", short_name(name)); }\n')
    assert run(work).returncode != 0, "hew-analysis leaf-name seams must be audited"
    analysis.unlink()

    target.write_text(
        "fn forbidden() { let _: HashMap<SpanKey, SiteId> = HashMap::new(); }\n"
    )
    result = run(work)
    assert result.returncode != 0, "a scalar SpanKey-to-SiteId map must fail"
    assert "forbidden scalar SpanKey -> SiteId authority" in result.stderr
    target.write_text(
        "fn forbidden() { let _: BTreeMap<SpanKey, Option<SiteId>> = BTreeMap::new(); }\n"
    )
    assert run(work).returncode != 0, "an optional scalar site map must fail"
    target.write_text(
        "fn forbidden() { let _: HashSet<(SpanKey, SiteId)> = HashSet::new(); }\n"
    )
    assert run(work).returncode != 0, "a span/site pair set must fail"
    target.write_text(
        "fn allowed() { let _: HashMap<SpanKey, Vec<SiteId>> = HashMap::new(); }\n"
    )
    assert run(work).returncode == 0, "a source-to-sites multimap must remain allowed"

    # Presentation is exempt only under the exact parsed debug-builder call
    # context. A same-location substitution of an authority call is debt.
    target.write_text(
        "fn f() { dctx.di_builder.create_enumerator(short_name(name), 0, false); }\n"
    )
    (work / "scripts/structural-authority-presentation.tsv").write_text(
        PRESENTATION_HEADER
        + "hew-mir/src/lower/new_authority.rs\t1\t44\tshort-name-identifier\t"
        "debug-enumerator-argument\tdebug-metadata\tpost-stage-5\t"
        "test debug display\n"
    )
    set_inventory(work)
    assert run(work).returncode == 0, "an exact debug AST context may be exempted"
    target.write_text(
        "fn f() { dctx.di_builder.semantic_resolver(short_name(name), 0, false); }\n"
    )
    result = run(work)
    assert result.returncode != 0, "same-location semantic substitution must fail"
    assert "presentation AST context disappeared" in result.stderr
    assert "short-name-identifier" in result.stderr

    # An exact presentation finding cannot hide a semantic sibling on its line.
    target.write_text(
        "fn f() { dctx.di_builder.create_enumerator(short_name(name), 0, false); let _ = short_name(key); }\n"
    )
    result = run(work)
    assert result.returncode != 0, "same-line presentation must not exempt semantic use"
    assert "expected 0, found 1" in result.stderr

    # Restore an empty presentation baseline and prove syntax-form drift,
    # corpus shrink, count drift, and arbitrary retirement-stage edits fail.
    (work / "scripts/structural-authority-presentation.tsv").write_text(
        PRESENTATION_HEADER
    )
    target.write_text("fn authority() { let _ = short_name(name); }\n")
    inventory_row = (
        "semantic-leaf-name\tshort-name-identifier\t"
        "hew-mir/src/lower/new_authority.rs\t1\tstage-4\ttest fixture\n"
    )
    set_inventory(work, inventory_row)
    assert run(work).returncode == 0, "a fully explicit current inventory must pass"
    target.write_text(
        'fn authority() { let _ = name.rsplit("::").next().unwrap_or(name); }\n'
    )
    result = run(work)
    assert result.returncode != 0, "short_name-to-rsplit form drift must fail"
    assert (
        "short-name-identifier" in result.stderr
        and "leaf-rsplit-field" in result.stderr
    )
    target.write_text("fn authority() {}\n")
    assert run(work).returncode != 0, "inventory shrink must fail"
    target.write_text("fn authority() { let _ = short_name(name); }\n")
    drifted_row = inventory_row.replace("\t1\tstage-4", "\t2\tstage-4")
    set_inventory(work, drifted_row)
    assert run(work).returncode != 0, "inventory count drift must fail"
    wrong_stage = inventory_row.replace("\tstage-4\t", "\tstage-5\t")
    set_inventory(work, wrong_stage)
    result = run(work)
    assert result.returncode != 0, "non-canonical retirement stages must fail"
    assert "requires stage-4" in result.stderr

    # Preserve the qualified-string identity ratchet under parsed let/macro
    # syntax rather than raw-source substring searches.
    target.write_text(
        'fn authority() { let method_key = format!("{}::{}", owner, method); }\n'
    )
    set_inventory(work)
    assert run(work).returncode != 0, "a new string method identity must fail"

print("structural authority audit counterfactuals: PASS")
