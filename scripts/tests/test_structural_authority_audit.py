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
    enum_authority = work / "hew-types/src/stdlib_authority/codegen.rs"
    enum_authority.parent.mkdir(parents=True)
    enum_authority.write_text(
        "struct BuiltinEnumAbi { name: &'static str }\n"
        "const BUILTIN_ENUM_ABI: &[BuiltinEnumAbi] = &[\n"
        '    BuiltinEnumAbi { name: "AskError" },\n'
        "];\n"
    )
    set_inventory(work)

    # A dead executable that silently returns ast-grep's no-match status must
    # fail the mandatory positive parser/query sentinel.
    fake_ast_grep = work / "fake-ast-grep"
    fake_ast_grep.write_text("#!/bin/sh\nexit 1\n")
    fake_ast_grep.chmod(0o755)
    result = run(work, ast_grep=fake_ast_grep)
    assert result.returncode != 0, "a non-parser executable must fail closed"
    assert "sentinel failed closed" in result.stderr

    # Neither comment tokens nor literal nodes create leaf-name findings,
    # including identifier-shaped text inside an ordinary macro string token.
    target.write_text(
        '// short_name(name); name.rsplit("::"); HashMap<SpanKey, SiteId>\n'
        'const DECOY: &str = "short_name(name); name.rsplit(\\"::\\")";\n'
        'fn macro_decoy() { format!("short_name(name) name.rsplit"); }\n'
    )
    result = run(work)
    assert result.returncode == 0, (
        "comments and strings must not create findings: " + result.stderr
    )

    target.write_text("fn authority() { let _ = short_name(name); }\n")
    result = run(work)
    assert result.returncode != 0, "a new semantic short-name use must fail"
    assert "short-name-identifier" in result.stderr

    target.write_text(
        "fn bad() { let _ = ResolvedTy::Named {\n"
        '    name: "AskError".to_string(), args: Vec::new(),\n'
        "    builtin: None, is_opaque: false,\n"
        "}; }\n"
    )
    result = run(work)
    assert result.returncode != 0, "a generated enum leaf struct literal must fail"
    assert (
        "monomorphic-enum-leaf-synthesis/leaf-named-semantic-struct-literal"
        in result.stderr
    )
    target.write_text(
        'fn good() { let _ = resolved_monomorphic_builtin_enum_ty("AskError"); }\n'
    )
    assert run(work).returncode == 0, (
        "the canonical catalog helper is the green control"
    )

    # A raw linker spelling must never select a codegen-special lowering. The
    # carrier's derived linkage assertion is the narrowly permitted control.
    codegen = work / "hew-codegen-rs/src/llvm.rs"
    codegen.parent.mkdir(parents=True)
    codegen.write_text('fn lower_terminator() { if callee == "hew_bytes_get" { } }\n')
    result = run(work)
    assert result.returncode != 0
    codegen.write_text("fn lower_terminator() { if family.c_symbol() != callee { } }\n")
    assert run(work).returncode == 0, "carrier linkage assertion is the green control"
    codegen.write_text(
        'fn lower_terminator() { match callee.as_str() { "hew_bytes_get" => {}, _ => {} } }\n'
    )
    result = run(work)
    assert result.returncode != 0, "raw match dispatch must fail the carrier gate"
    codegen.write_text(
        'fn lower_terminator() { if matches!(callee, "hew_bytes_get") { } }\n'
    )
    result = run(work)
    assert result.returncode != 0, "raw matches! dispatch must fail the carrier gate"
    codegen.write_text(
        "fn lower_terminator() { if kind.expected_callee() == callee { } }\n"
    )
    assert run(work).returncode == 0, "compiler carrier linkage assertion is green"
    codegen.unlink()

    enum_authority.write_text(
        "struct BuiltinEnumAbi { name: &'static str }\n"
        "const BUILTIN_ENUM_ABI: &[BuiltinEnumAbi] = &[\n"
        '    BuiltinEnumAbi { name: "AskError" },\n'
        '    BuiltinEnumAbi { name: "FutureGeneratedError" },\n'
        "];\n"
    )
    target.write_text(
        "fn future_bad() { let _ = Ty::Named {\n"
        '    name: "FutureGeneratedError".to_string(), args: Vec::new(), builtin: None,\n'
        "}; }\n"
    )
    result = run(work)
    assert result.returncode != 0, (
        "an added ABI catalog row must become audited without editing Python"
    )
    assert "monomorphic-enum-leaf-synthesis" in result.stderr
    enum_authority.write_text(
        "struct BuiltinEnumAbi { name: &'static str }\n"
        "const BUILTIN_ENUM_ABI: &[BuiltinEnumAbi] = &[\n"
        '    BuiltinEnumAbi { name: "AskError" },\n'
        "];\n"
    )

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

    target.write_text(
        "#[cfg(all(test))]\n"
        "fn all_single() { let _ = short_name(name); }\n"
        '#[cfg(all(feature = "x", test))]\n'
        "fn all_reordered() { let _ = short_name(name); }\n"
    )
    assert run(work).returncode == 0, "all(...) test guards must be order-independent"
    target.write_text(
        '#[cfg(any(test, feature = "x"))]\n'
        "fn any_guard() { let _ = short_name(name); }\n"
    )
    assert run(work).returncode != 0, "cfg(any(test, feature)) is production-capable"
    target.write_text(
        "#[cfg(not(test))]\nfn non_test_guard() { let _ = short_name(name); }\n"
    )
    assert run(work).returncode != 0, "cfg(not(test)) is production authority"

    target.write_text(
        "fn bypass() { let _ = Ty::names_match_qualified(left, right); }\n"
    )
    result = run(work)
    assert result.returncode != 0, (
        "semantic suffix matching outside its authority must fail"
    )
    assert "forbidden context-free nominal authority" in result.stderr

    target.write_text("fn bypass() { let _ = unify(&mut subst, left, right); }\n")
    result = run(work)
    assert result.returncode != 0, (
        "raw checker unification outside its owner guard must fail"
    )
    assert "forbidden context-free nominal authority" in result.stderr

    # RC1 carrier inventories are syntax-node based: comments and strings do
    # not create a checker fact, call target, suspend, or retirement finding.
    target.write_text(
        "// pub enum CallTarget { New } pub enum ProducedValueDependency { Subsumes } SuspendKind::New Terminator::Join\n"
        'const DECOY: &str = "TypeCheckOutput { rc1_fact: u32 } HirProducedValueRelation::Subsumes suspend_abandon_extra_drops ActorHandlerKind::Crash";\n'
        "fn production() {}\n"
    )
    assert run(work).returncode == 0, "RC1 carrier text in comments/strings is not code"

    # A new checker-produced fact/relation, executable CallTarget variant,
    # SuspendKind/terminator variant or writer, and each owner-retirement path
    # must all acquire an explicit production inventory row.
    checker_types = work / "hew-types/src/check/types.rs"
    checker_types.parent.mkdir(parents=True)
    checker_types.write_text("pub struct TypeCheckOutput { pub rc1_fact: u32 }\n")
    result = run(work)
    assert result.returncode != 0 and "produced-fact-field" in result.stderr
    hir_consumer = work / "hew-hir/src/lower.rs"
    hir_consumer.parent.mkdir(parents=True)
    hir_consumer.write_text(
        "fn lower(output: TypeCheckOutput) { let _ = output.rc1_fact; }\n"
    )
    fact_inventory = (
        "checker-hir-fact-relation\tproduced-fact-field\t"
        "hew-types/src/check/types.rs\t1\tstage-2\tcomposition fact\n"
        "checker-hir-fact-relation\thir-publication-consumer\t"
        "hew-hir/src/lower.rs\t1\tstage-2\tcomposition consumer\n"
    )
    set_inventory(work, fact_inventory)
    assert run(work).returncode == 0, "checker fact publication must have a consumer"
    hir_consumer.write_text("fn lower() {}\n")
    result = run(work)
    assert result.returncode != 0 and "hir-publication-consumer" in result.stderr
    set_inventory(work)
    hir_consumer.unlink()
    checker_types.unlink()

    # Compose the real checker and HIR relation carriers. Both closed variant
    # sets must remain congruent, and explicit producers/consumers are exact.
    checker_relation = work / "hew-types/src/produced_value.rs"
    hir_relation = work / "hew-hir/src/produced_value.rs"
    hir_relation.parent.mkdir(parents=True, exist_ok=True)
    checker_relation.write_text(
        "pub enum ProducedValueDependency { Leaf, Subsumes(u32) }\n"
        "fn produce(anchor: u32) { let _ = ProducedValueDependency::Subsumes(anchor); }\n"
        "fn consume(fact: ProducedValueDependency) { match fact {\n"
        "    ProducedValueDependency::Leaf => {},\n"
        "    ProducedValueDependency::Subsumes(_) => {},\n"
        "} }\n"
    )
    hir_relation.write_text(
        "pub enum HirProducedValueRelation { Leaf, Subsumes(u32) }\n"
        "fn produce(anchor: u32) { let _ = HirProducedValueRelation::Subsumes(anchor); }\n"
        "fn consume(fact: HirProducedValueRelation) { match fact {\n"
        "    HirProducedValueRelation::Leaf => {},\n"
        "    HirProducedValueRelation::Subsumes(_) => {},\n"
        "} }\n"
    )
    relation_inventory = (
        "checker-hir-fact-relation\tchecker-relation-variant\t"
        "hew-types/src/produced_value.rs\t2\tstage-2\treal checker relation variants\n"
        "checker-hir-fact-relation\tchecker-relation-use\t"
        "hew-types/src/produced_value.rs\t3\tstage-2\tchecker relation producers and consumers\n"
        "checker-hir-fact-relation\tchecker-relation-consumer\t"
        "hew-types/src/produced_value.rs\t2\tstage-2\tchecker relation consumers\n"
        "checker-hir-fact-relation\thir-relation-variant\t"
        "hew-hir/src/produced_value.rs\t2\tstage-2\treal HIR relation variants\n"
        "checker-hir-fact-relation\thir-relation-use\t"
        "hew-hir/src/produced_value.rs\t3\tstage-2\tHIR relation producers and consumers\n"
        "checker-hir-fact-relation\thir-relation-consumer\t"
        "hew-hir/src/produced_value.rs\t2\tstage-2\tHIR relation consumers\n"
    )
    set_inventory(work, relation_inventory)
    assert run(work).returncode == 0, (
        "real checker/HIR relation carriers must be nonzero"
    )
    hir_relation.write_text(
        "pub enum HirProducedValueRelation { Leaf, Subsumes(u32) }\n"
        "fn produce(anchor: u32) { let _ = HirProducedValueRelation::Subsumes(anchor); }\n"
        "fn consume(fact: HirProducedValueRelation) { match fact {\n"
        "    HirProducedValueRelation::Leaf => {},\n"
        "} }\n"
    )
    result = run(work)
    assert result.returncode != 0 and "hir-relation-consumer" in result.stderr
    checker_relation.write_text(
        "pub enum ProducedValueDependency { Leaf, Subsumes(u32), Projection(u32) }\n"
    )
    result = run(work)
    assert result.returncode != 0 and "relation variants diverged" in result.stderr
    set_inventory(work)
    checker_relation.unlink()
    hir_relation.unlink()

    target.write_text(
        "fn specialized_rewrite(runtime_symbol: &str) {\n"
        "    let leaf = short_name(runtime_symbol);\n"
        "    let _ = resolve_runtime_symbol(leaf);\n"
        "}\n"
    )
    result = run(work)
    assert result.returncode != 0 and (
        "semantic-owner-shortening-sink/runtime-resolution" in result.stderr
    )
    target.write_text("fn production() {}\n")

    call_target = work / "hew-mir/src/call_target.rs"
    call_target.write_text("pub enum CallTarget { New { value: u32 } }\n")
    result = run(work)
    assert result.returncode != 0 and "call-target-variant" in result.stderr

    # The production carrier is real: a closed enum, constructor, and match consumer must
    # be inventoried together; removing the consumer or adding a variant fails.
    call_target.write_text(
        "pub enum CallTarget { Direct { value: u32 } }\n"
        "fn produce(value: u32) { let _ = CallTarget::Direct { value }; }\n"
        "fn consume(target: CallTarget) { match target { CallTarget::Direct { .. } => {} } }\n"
    )
    call_target_inventory = (
        "call-target-authority\tcall-target-variant\t"
        "hew-mir/src/call_target.rs\t1\tstage-4\tcomposition variant\n"
        "call-target-authority\tcall-target-use\t"
        "hew-mir/src/call_target.rs\t2\tstage-4\tcomposition constructor and consumer\n"
        "call-target-authority\tcall-target-consumer\t"
        "hew-mir/src/call_target.rs\t1\tstage-4\tcomposition consumer\n"
    )
    set_inventory(work, call_target_inventory)
    result = run(work)
    assert result.returncode == 0, (
        "CallTarget composition inventory must be nonzero: " + result.stderr
    )
    call_target.write_text(
        "pub enum CallTarget { Direct { value: u32 } }\n"
        "fn produce(value: u32) { let _ = CallTarget::Direct { value }; }\n"
    )
    result = run(work)
    assert result.returncode != 0 and "call-target-consumer" in result.stderr
    call_target.write_text(
        "pub enum CallTarget { Direct { value: u32 }, Unsupported { value: u32 } }\n"
        "fn produce(value: u32) { let _ = CallTarget::Direct { value }; }\n"
        "fn consume(target: CallTarget) { match target { CallTarget::Direct { .. } => {} } }\n"
    )
    result = run(work)
    assert result.returncode != 0 and "call-target-variant" in result.stderr
    set_inventory(work)
    call_target.unlink()

    runtime_family = work / "hew-types/src/runtime_call.rs"
    runtime_family.write_text(
        "pub enum RuntimeCallFamily { ChannelRecv, StreamRecv }\n"
        "fn produce() { let _ = RuntimeCallFamily::ChannelRecv; }\n"
        "fn consume(family: RuntimeCallFamily) { match family {\n"
        "    RuntimeCallFamily::ChannelRecv => {},\n"
        "    RuntimeCallFamily::StreamRecv => {},\n"
        "} }\n"
    )
    runtime_inventory = (
        "runtime-call-authority\truntime-call-family-variant\t"
        "hew-types/src/runtime_call.rs\t2\tstage-4\truntime family variants\n"
        "runtime-call-authority\truntime-call-family-use\t"
        "hew-types/src/runtime_call.rs\t3\tstage-4\truntime family uses\n"
        "runtime-call-authority\truntime-call-family-consumer\t"
        "hew-types/src/runtime_call.rs\t2\tstage-4\truntime family consumers\n"
    )
    set_inventory(work, runtime_inventory)
    assert run(work).returncode == 0, "runtime family authority must be nonzero"
    runtime_family.write_text(
        "pub enum RuntimeCallFamily { ChannelRecv, StreamRecv, NewFamily }\n"
        "fn produce() { let _ = RuntimeCallFamily::ChannelRecv; }\n"
        "fn consume(family: RuntimeCallFamily) { match family {\n"
        "    RuntimeCallFamily::ChannelRecv => {},\n"
        "    RuntimeCallFamily::StreamRecv => {},\n"
        "} }\n"
    )
    result = run(work)
    assert result.returncode != 0 and "runtime-call-family-variant" in result.stderr
    set_inventory(work)
    runtime_family.unlink()

    lifecycle = work / "hew-types/src/lifecycle.rs"
    lifecycle.write_text(
        "pub struct OpaqueResourceLifecycleCandidate {\n"
        "    pub qualified_name: String,\n"
        "    pub close_method: String,\n"
        "}\n"
        "pub enum OpaqueResourceLifecycleConflictKind { CloseMethodMismatch }\n"
    )
    lifecycle_inventory = (
        "lifecycle-identity-authority\tlifecycle-candidate-field\t"
        "hew-types/src/lifecycle.rs\t2\tstage-2\tcanonical lifecycle identity fields\n"
        "lifecycle-identity-authority\tlifecycle-conflict-variant\t"
        "hew-types/src/lifecycle.rs\t1\tstage-2\tlifecycle conflict discriminator\n"
    )
    set_inventory(work, lifecycle_inventory)
    assert run(work).returncode == 0, "lifecycle identity authority must be nonzero"
    lifecycle.write_text(
        "pub struct OpaqueResourceLifecycleCandidate {\n"
        "    pub qualified_name: String,\n"
        "    pub close_method: String,\n"
        "    pub fallback_name: String,\n"
        "}\n"
        "pub enum OpaqueResourceLifecycleConflictKind { CloseMethodMismatch }\n"
    )
    result = run(work)
    assert result.returncode != 0 and "lifecycle-candidate-field" in result.stderr
    set_inventory(work)
    lifecycle.unlink()

    target.write_text("pub enum SuspendKind { Future { result: Place } }\n")
    result = run(work)
    assert result.returncode != 0 and "suspend-kind-variant" in result.stderr
    target.write_text("fn writer() { self.record_suspend_kind(Future {}); }\n")
    result = run(work)
    assert result.returncode != 0 and "suspend-kind-writer" in result.stderr

    target.write_text("pub enum Terminator { SuspendingFuture { result: Place } }\n")
    result = run(work)
    assert result.returncode != 0 and "suspending-terminator-variant" in result.stderr
    target.write_text(
        "pub enum Terminator { SuspendingFuture { result: Place } }\n"
        "fn writer() { self.finish_current_block(Terminator::SuspendingFuture { result }); }\n"
    )
    result = run(work)
    assert result.returncode != 0 and "suspending-terminator-writer" in result.stderr

    target.write_text(
        "fn join() { let _ = Terminator::Join { branches, result, next }; }\n"
    )
    result = run(work)
    assert result.returncode != 0 and "join-owner-path" in result.stderr
    target.write_text("fn abandon() { let suspend_abandon_extra_drops = 0; }\n")
    result = run(work)
    assert result.returncode != 0 and "abandonment-owner-path" in result.stderr
    target.write_text("fn crash() { let _ = ActorHandlerKind::Crash; }\n")
    result = run(work)
    assert result.returncode != 0 and "crash-cleanup-owner-path" in result.stderr

    # Parsed test-only carrier variants/writers are not production authority.
    target.write_text(
        "#[cfg(test)]\n"
        "mod tests {\n"
        "    pub struct TypeCheckOutput { pub test_fact: u32 }\n"
        "    pub enum CallTarget { Test { value: u32 } }\n"
        "    pub enum ProducedValue { Subsumes { value: u32 } }\n"
        "    pub enum SuspendKind { Test { result: Place } }\n"
        "    pub enum Terminator { SuspendingTest { result: Place } }\n"
        "    fn writer() {\n"
        "        self.record_suspend_kind(Test {});\n"
        "        self.finish_current_block(Terminator::SuspendingTest { result });\n"
        "        let _ = Terminator::Join { branches, result, next };\n"
        "        let suspend_abandon_extra_drops = 0;\n"
        "        let _ = ActorHandlerKind::Crash;\n"
        "    }\n"
        "}\n"
    )
    assert run(work).returncode == 0, "test-only RC1 carriers must be excluded"

    # hew-analysis is an audited production root, not a display-shaped escape.
    target.write_text("fn production() {}\n")
    analysis = work / "hew-analysis/src/new_display.rs"
    analysis.parent.mkdir(parents=True)
    analysis.write_text('fn display() { format!("{}", short_name(name)); }\n')
    assert run(work).returncode != 0, "hew-analysis leaf-name seams must be audited"
    analysis.unlink()

    # Leaf ownership is forbidden when it flows into an executable identity
    # sink, including through local aliases and captured format parameters.
    target.write_text(
        "fn authority(mod_id: &ModId, name: &str) {\n"
        '    let module_short = mod_id.path.last().map_or("", String::as_str);\n'
        '    let key = format!("{module_short}.{name}");\n'
        "    fn_registry.insert(key, entry);\n"
        "}\n"
    )
    set_inventory(work)
    result = run(work)
    assert result.returncode != 0, "a leaf module alias must not feed a registry key"
    assert "semantic-owner-shortening-sink/registry-key" in result.stderr

    target.write_text(
        "fn authority(mod_id: &ModId, name: &str) {\n"
        "    if let Some(module_short) = mod_id.path.last() {\n"
        '        fn_registry.insert(format!("{}.{}", module_short, name), entry);\n'
        "    }\n"
        "}\n"
    )
    set_inventory(work)
    result = run(work)
    assert result.returncode != 0, "if-let bindings must preserve shortening taint"
    assert "semantic-owner-shortening-sink/registry-key" in result.stderr

    target.write_text(
        "fn authority(decl: &ImportDecl, name: &str) {\n"
        "    let module_binding = decl.module_alias.clone()\n"
        "        .or_else(|| decl.path.last().cloned()).unwrap();\n"
        '    record_registry.insert(format!("{}.{}", module_binding, name), entry);\n'
        "}\n"
    )
    set_inventory(work)
    result = run(work)
    assert result.returncode != 0, (
        "a source module alias must not become registry identity"
    )
    assert "semantic-owner-shortening-sink/registry-key" in result.stderr

    target.write_text(
        "fn authority(module_path: &str, name: &str) {\n"
        '    let owner = module_path.rsplit("::").next().unwrap();\n'
        '    let _ = DefId::new(format!("{}.{}", owner, name));\n'
        "}\n"
    )
    set_inventory(work)
    result = run(work)
    assert result.returncode != 0, "a leaf owner must not mint a DefId"
    assert "semantic-owner-shortening-sink/def-id" in result.stderr

    target.write_text(
        "fn authority(current_module: &str, name: &str) {\n"
        "    let owner = short_name(current_module);\n"
        '    let _ = CallTarget::User(DefId::new(format!("{}.{}", owner, name)));\n'
        '    let _ = NominalId::new(format!("{}.{}", owner, name));\n'
        "}\n"
    )
    set_inventory(work)
    result = run(work)
    assert result.returncode != 0, (
        "short_name(current_module) must not mint semantic IDs"
    )
    assert "semantic-owner-shortening-sink/call-target" in result.stderr
    assert "semantic-owner-shortening-sink/nominal-id" in result.stderr

    # Display shortening and unrelated collection `.last()` calls are not
    # executable identity authority. Fully-qualified owners remain valid keys.
    target.write_text(
        "fn display(module_path: &[String]) {\n"
        "    let module_short = module_path.last();\n"
        '    eprintln!("module: {:?}", module_short);\n'
        "}\n"
        "fn canonical(module_identity: &str, name: &str) {\n"
        '    fn_registry.insert(format!("{}.{}", module_identity, name), entry);\n'
        "}\n"
        "fn ordinary(statements: &[Stmt]) {\n"
        "    fn_registry.insert(statements.last().unwrap().name.clone(), entry);\n"
        "}\n"
    )
    set_inventory(work)
    assert run(work).returncode == 0, (
        "display-only shortening, canonical owners, and ordinary `.last()` must remain controls"
    )

    target.write_text(
        'fn display(current_module: &str) { eprintln!("{}", short_name(current_module)); }\n'
    )
    display_inventory = (
        "semantic-leaf-name\tshort-name-identifier\t"
        "hew-mir/src/lower/new_authority.rs\t1\tstage-4\t"
        "display-only short name fixture\n"
    )
    set_inventory(work, display_inventory)
    assert run(work).returncode == 0, (
        "an inventoried display-only short_name must not become an executable sink"
    )

    target.write_text(
        "fn canonical(declaring_module: &str, signature_key: &str) {\n"
        "    let name = signature_key.rsplit('.').next().unwrap();\n"
        '    let declaration = format!("{declaring_module}.{name}");\n'
        "    let _ = DefId::new(declaration);\n"
        "}\n"
    )
    canonical_inventory = (
        "semantic-leaf-name\tleaf-rsplit-field\t"
        "hew-mir/src/lower/new_authority.rs\t1\tstage-4\t"
        "canonical owner reattachment fixture\n"
    )
    set_inventory(work, canonical_inventory)
    assert run(work).returncode == 0, (
        "an item leaf reattached to a resolved full owner must remain canonical"
    )
    set_inventory(work)

    scalar_values = (
        "SiteId",
        "&SiteId",
        "&'static SiteId",
        "&'a mut SiteId",
        "*const SiteId",
        "*mut SiteId",
        "Option<SiteId>",
        "Box<SiteId>",
        "Rc<SiteId>",
        "Arc<SiteId>",
        "Cell<SiteId>",
        "RefCell<SiteId>",
        "Option<Box<&'a SiteId>>",
    )
    for scalar_value in scalar_values:
        target.write_text(
            "fn forbidden<'a>() { let _: HashMap<SpanKey, "
            f"{scalar_value}> = HashMap::new(); }}\n"
        )
        result = run(work)
        assert result.returncode != 0, f"scalar wrapper {scalar_value} must fail"
        assert "forbidden scalar SpanKey -> SiteId authority" in result.stderr
    target.write_text(
        "fn forbidden() { let _: HashSet<(SpanKey, SiteId)> = HashSet::new(); }\n"
    )
    assert run(work).returncode != 0, "a span/site pair set must fail"
    for collection_value in (
        "Vec<SiteId>",
        "SmallVec<[SiteId; 4]>",
        "BTreeSet<SiteId>",
    ):
        target.write_text(
            "fn allowed() { let _: HashMap<SpanKey, "
            f"{collection_value}> = HashMap::new(); }}\n"
        )
        assert run(work).returncode == 0, (
            f"source-to-sites collection {collection_value} must remain allowed"
        )

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

    # The exemption binds the exact designated debug argument expression. A
    # nested semantic sibling elsewhere in that same debug call remains debt.
    target.write_text(
        "fn f() { dctx.di_builder.create_enumerator(short_name(name), semantic(short_name(key)), false); }\n"
    )
    result = run(work)
    assert result.returncode != 0, (
        "a nested sibling in one debug call must remain semantic"
    )
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

    # Preserve qualified-string identity under every binding/assignment form;
    # the authority is the parsed format macro, not a particular let shape.
    for source_text, description in (
        (
            'fn f() { let method_key: String = format!("{}::{}", owner, method); }\n',
            "typed let",
        ),
        (
            'fn f() { let mut method_key = format!("{}::{}", owner, method); }\n',
            "mutable let",
        ),
        (
            'fn f() { method_key = format!("{}::{}", owner, method); }\n',
            "assignment",
        ),
        (
            'fn f() { method_key = format!(r#"{}::{}"#, owner, method); }\n',
            "raw-literal assignment",
        ),
        (
            'fn f() { method_key = std::format!("{}::{}", owner, method); }\n',
            "module-qualified macro",
        ),
        (
            'fn f() { method_key = std::format!["{}::{}", owner, method]; }\n',
            "module-qualified bracket macro",
        ),
        (
            'fn f() { method_key = ::std::format!{"{}::{}", owner, method}; }\n',
            "absolute macro path",
        ),
        (
            'fn f() { method_key = std :: format ! ("{}::{}", owner, method); }\n',
            "whitespace-qualified macro",
        ),
        (
            'fn f() { method_key = :: std :: format ! ["{}::{}", owner, method]; }\n',
            "whitespace-qualified absolute macro",
        ),
        (
            'fn f() { method_key = foo :: format ! {"{}::{}", owner, method}; }\n',
            "whitespace-qualified brace macro",
        ),
    ):
        target.write_text(source_text)
        set_inventory(work)
        result = run(work)
        assert result.returncode != 0, f"a new {description} string identity must fail"
        assert "qualified-format-macro" in result.stderr

    target.write_text(
        'fn f() { method_key = qualified::other!("{}::{}", owner, method); }\n'
    )
    set_inventory(work)
    assert run(work).returncode == 0, "non-format macro paths must remain controls"
    target.write_text(
        'fn f() { method_key = std :: other ! ("{}::{}", owner, method); }\n'
    )
    set_inventory(work)
    assert run(work).returncode == 0, (
        "whitespace-qualified non-format macro paths must remain controls"
    )

print("structural authority audit counterfactuals: PASS")
