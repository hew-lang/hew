use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use crate::diagnostics::{display_path, type_diagnostic_to_frontend, SourceSnapshot};
use crate::{
    ownership_diagnostics_to_frontend, run_source_frontend, DocumentFrontendState,
    FrontendDiagnosticKind, FrontendOptions, SourceAnalysisBatch, SourceSet,
};

fn test_options() -> FrontendOptions {
    FrontendOptions {
        module_search_paths: Some(vec![PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf()]),
        ..FrontendOptions::default()
    }
}

#[test]
fn repeated_requests_refresh_disk_dependencies_and_retained_sources() {
    let dir = tempfile::tempdir().unwrap();
    let dep = dir.path().join("greeting.hew");
    let label = dir.path().join("consumer.hew");
    let label = label.to_str().unwrap();
    let old = "pub fn greet() -> i32 { 7 }";
    fs::write(&dep, old).unwrap();
    let options = test_options();
    let mut batch = SourceAnalysisBatch::new(options.clone());
    let source = "import greeting; fn consumer() -> i32 { greeting.greet() }";
    let first = assert_equivalent(source, label, &options, &mut batch);
    assert!(first.stopped.is_none(), "{:?}", first.diagnostics);
    let retained = &first.typecheck_result.as_ref().unwrap().sources;
    fs::write(&dep, "pub fn greet() -> string { \"changed\" }").unwrap();
    let changed = assert_equivalent(source, label, &options, &mut batch);
    assert!(
        changed.stopped.is_some(),
        "updated dependency must reject the integer caller"
    );
    assert_eq!(batch.dependency_bootstraps(), 2);
    fs::remove_file(&dep).unwrap();
    let deleted = run_source_frontend(source, label, &options);
    assert!(deleted.stopped.is_some());
    let (text, _) = retained.source("greeting").unwrap();
    assert_eq!(&*text, old);
    assert!(retained.documents.is_file(&dep));
    assert_eq!(&*retained.documents.read_text(&dep).unwrap(), old);

    let error = hew_types::TypeError::new(
        hew_types::error::TypeErrorKind::InvalidSend,
        0..3,
        "retained location",
    )
    .with_note_source(0..3, "dependency declaration", Some("greeting".into()));
    let diagnostic = type_diagnostic_to_frontend(error, retained);
    assert_eq!(diagnostic.source.as_deref(), Some(source));
    assert_eq!(&*diagnostic.note_sources[0].as_ref().unwrap().0, old);
    let diagnostic = ownership_diagnostics_to_frontend(
        vec![hew_sir::SirDiagnostic {
            function: "greet".into(),
            callable: None,
            kind: hew_sir::SirDiagnosticKind::UnconsumedLinear {
                binding: Some("value".into()),
                span: 0..3,
                source_origin: hew_sir::FunctionSourceOrigin::Foreign("greeting".into()),
            },
        }],
        retained,
    );
    let FrontendDiagnosticKind::Message(message) = &diagnostic[0].kind else {
        panic!("expected positioned ownership message");
    };
    assert_eq!(message.source.as_deref(), Some(old));
}

#[test]
fn source_epoch_keeps_binary_inputs_until_the_next_request() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("included.txt");
    fs::write(&path, b"original").unwrap();
    let options = SourceSet::new();
    let admitted = options.for_frontend();
    assert_eq!(admitted.read_bytes(&path).unwrap(), b"original");
    fs::write(&path, b"changed").unwrap();
    assert_eq!(&*admitted.read_text(&path).unwrap(), "original");
    let fresh = options.for_frontend();
    assert_eq!(&*fresh.read_text(&path).unwrap(), "changed");
    assert_ne!(admitted.inputs()[0].sha256, fresh.inputs()[0].sha256);
    fs::remove_file(&path).unwrap();
    assert_eq!(admitted.read_bytes(&path).unwrap(), b"original");
    assert!(!options.for_frontend().is_file(&path));

    fs::write(&path, [0xff, 0x00]).unwrap();
    let binary = options.for_frontend();
    assert_eq!(binary.read_bytes(&path).unwrap(), [0xff, 0x00]);
    fs::remove_file(&path).unwrap();
    assert!(binary.is_file(&path));
    assert_eq!(
        binary.read_text(&path).unwrap_err().kind(),
        std::io::ErrorKind::InvalidData
    );
    assert!(binary.admitted_text(&path).is_none());
    let snapshot = SourceSnapshot {
        root_source: "fn main() {}".into(),
        root_filename: "main.hew".into(),
        modules: HashMap::new(),
        documents: binary,
    };
    let diagnostic = type_diagnostic_to_frontend(
        hew_types::TypeError::new(
            hew_types::error::TypeErrorKind::InvalidOperation,
            0..1,
            "invalid embedded data",
        )
        .with_source_input(&path),
        &snapshot,
    );
    assert_eq!(
        diagnostic.filename.as_deref(),
        Some(display_path(&path).as_str())
    );
    assert_eq!(diagnostic.source.as_deref(), Some("\u{fffd}\0"));
    assert_eq!(snapshot.documents.read_bytes(&path).unwrap(), [0xff, 0x00]);
}

fn identity(output: &hew_types::TypeCheckOutput, id: hew_types::DefId) -> String {
    let site = output.defs.site(id);
    let path = output
        .defs
        .module(id)
        .and_then(|module| output.defs.module_source(module));
    format!(
        "{path:?}:{:?}:{:?}:{:?}:{}",
        site.map(hew_types::DeclarationOccurrence::span),
        site.map(hew_types::DeclarationOccurrence::kind),
        site.map(hew_types::DeclarationOccurrence::ordinal),
        output.defs.name(id)
    )
}

fn physical_declarations(output: &hew_types::TypeCheckOutput) -> Vec<String> {
    let mut declarations = output
        .defs
        .declarations()
        .filter_map(|(site, id)| {
            output
                .defs
                .module_source(site.module()?)
                .map(|_| identity(output, id))
        })
        .collect::<Vec<_>>();
    declarations.sort();
    declarations
}

fn checked_resolutions(output: &hew_types::TypeCheckOutput) -> Vec<String> {
    use hew_types::check::scope::Resolution;
    let mut local_sites = HashMap::new();
    for (span, resolution) in &output.resolutions {
        if let Resolution::Local(binding) = resolution {
            let span = format!("{span:?}");
            local_sites
                .entry(*binding)
                .and_modify(|previous: &mut String| {
                    if span < *previous {
                        previous.clone_from(&span);
                    }
                })
                .or_insert(span);
        }
    }
    let mut resolutions = output
        .resolutions
        .iter()
        .map(|(span, resolution)| {
            let target = match resolution {
                Resolution::Def(id) => format!("def:{}", identity(output, *id)),
                Resolution::Member(id) => format!("member:{}", identity(output, *id)),
                Resolution::Nominal(id) => {
                    format!("nominal:{}", identity(output, id.declaration()))
                }
                Resolution::Param(id) => {
                    let owner = id.declaration_owner().map_or_else(
                        || format!("{:?}", id.owner),
                        |owner| identity(output, owner),
                    );
                    format!("param:{owner}:{}", id.index)
                }
                Resolution::Field(id, index) => {
                    format!("field:{}:{index}", identity(output, id.declaration()))
                }
                Resolution::Variant(head, index) => {
                    let owner = head.declaration(&output.defs).map_or_else(
                        || format!("builtin:{:?}", head.builtin()),
                        |id| identity(output, id.declaration()),
                    );
                    format!("variant:{owner}:{index}")
                }
                Resolution::Local(id) => format!("local:{}", local_sites[id]),
                Resolution::Module(id) => format!(
                    "module:{:?}:{}",
                    output.defs.module_source(*id),
                    output.defs.module_path(*id)
                ),
                Resolution::Builtin(kind) => format!("builtin:{kind:?}"),
            };
            format!("{span:?}:{target}")
        })
        .collect::<Vec<_>>();
    resolutions.sort();
    resolutions
}

fn effect_identity(
    output: &hew_types::TypeCheckOutput,
    body: &hew_types::ty::EffectBody,
) -> String {
    use hew_types::ty::EffectBody;
    match body {
        EffectBody::Declaration(id) => format!("declaration:{}", identity(output, *id)),
        EffectBody::Generator(id) => format!("generator:{}", identity(output, *id)),
        EffectBody::GeneratorBlock(span) => format!("generator:{span:?}"),
        EffectBody::Closure(span) => format!("closure:{span:?}"),
    }
}

fn checked_type(output: &hew_types::TypeCheckOutput, ty: &hew_types::Ty) -> String {
    use hew_types::ty::TypeHead;
    use hew_types::Ty;
    match ty {
        Ty::Named { head, args } => {
            let head = match head {
                TypeHead::Nominal(head) => {
                    format!("nominal:{}", identity(output, head.id.declaration()))
                }
                TypeHead::Actor(head) => {
                    format!("actor:{}", identity(output, head.id.declaration()))
                }
                TypeHead::Param(head) => {
                    let owner = head.id.declaration_owner().map_or_else(
                        || format!("{:?}", head.id.owner),
                        |owner| identity(output, owner),
                    );
                    format!("param:{owner}:{}", head.id.index)
                }
                TypeHead::Builtin(kind) => format!("builtin:{kind:?}"),
            };
            format!(
                "{head}<{:?}>",
                args.iter()
                    .map(|ty| checked_type(output, ty))
                    .collect::<Vec<_>>()
            )
        }
        Ty::Function {
            capabilities,
            params,
            ret,
        } => format!(
            "function:{capabilities:?}:{:?}:{}",
            params
                .iter()
                .map(|parameter| (parameter.ownership, checked_type(output, &parameter.ty)))
                .collect::<Vec<_>>(),
            checked_type(output, ret)
        ),
        Ty::Closure {
            capabilities,
            params,
            ret,
            captures,
            identity: body,
        } => format!(
            "closure:{capabilities:?}:{:?}:{}:{:?}:{}",
            params
                .iter()
                .map(|parameter| (parameter.ownership, checked_type(output, &parameter.ty)))
                .collect::<Vec<_>>(),
            checked_type(output, ret),
            captures
                .iter()
                .map(|ty| checked_type(output, ty))
                .collect::<Vec<_>>(),
            effect_identity(output, body)
        ),
        Ty::Var(_) => "inference-variable".to_string(),
        _ => ty.to_string(),
    }
}

fn finalized_facts(output: &hew_types::TypeCheckOutput) -> Vec<String> {
    let mut facts = output
        .expr_types
        .iter()
        .map(|(span, ty)| format!("expr:{span:?}:{}", checked_type(output, ty)))
        .collect::<Vec<_>>();
    facts.extend(output.fn_sigs.iter().map(|(id, signature)| {
        format!(
            "signature:{}:{:?}:{:?}:{}",
            identity(output, *id),
            signature.param_names,
            signature
                .params
                .iter()
                .map(|parameter| (parameter.ownership, checked_type(output, &parameter.ty)))
                .collect::<Vec<_>>(),
            checked_type(output, &signature.return_type)
        )
    }));
    facts.extend(
        output
            .suspension_effects
            .bodies
            .iter()
            .map(|(body, effect)| {
                format!("body-effect:{}:{effect:?}", effect_identity(output, body))
            }),
    );
    facts.extend(
        output
            .suspension_effects
            .calls
            .iter()
            .map(|(span, effect)| format!("call-effect:{span:?}:{effect:?}")),
    );
    facts.extend(
        output
            .suspension_effects
            .fork_transfers
            .iter()
            .map(|(span, transfer)| format!("fork:{span:?}:{transfer:?}")),
    );
    facts.extend(
        output
            .closure_escape_facts
            .iter()
            .map(|(span, escape)| format!("escape:{span:?}:{escape:?}")),
    );
    facts.extend(output.type_facts.iter().map(|(key, row)| {
        format!(
            "type-fact:{}:{row:?}",
            checked_type(output, &key.ty().to_ty())
        )
    }));
    facts.sort();
    facts
}

fn assert_equivalent(
    source: &str,
    label: &str,
    options: &FrontendOptions,
    batch: &mut SourceAnalysisBatch,
) -> DocumentFrontendState {
    let fresh = run_source_frontend(source, label, options);
    let shared = batch.run_source_frontend(source, label);
    assert_eq!(fresh.stopped.is_some(), shared.stopped.is_some());
    assert_eq!(
        format!("{:?}", fresh.diagnostics),
        format!("{:?}", shared.diagnostics)
    );
    let fresh = fresh
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
        .unwrap_or_else(|| panic!("frontend did not reach checking: {:?}", fresh.diagnostics));
    let checked = shared
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
        .unwrap_or_else(|| {
            panic!(
                "shared frontend did not reach checking: {:?}",
                shared.diagnostics
            )
        });
    assert_eq!(physical_declarations(fresh), physical_declarations(checked));
    assert_eq!(checked_resolutions(fresh), checked_resolutions(checked));
    assert_eq!(finalized_facts(fresh), finalized_facts(checked));
    shared
}

#[test]
fn invalid_dependencies_bootstrap_once_and_keep_complete_diagnostics() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("greeting.hew"),
        "pub fn identity<T>(value: T) -> T { value + true }",
    )
    .unwrap();
    let options = test_options();
    let mut batch = SourceAnalysisBatch::new(options.clone());
    // Keep the same dependency traversal order for this exact-snapshot
    // memoization check. Roots on opposite sides of std's lexical position
    // can select a different valid non-root order and require another seed.
    for (name, source) in [
        (
            "consumer_integer.hew",
            "import greeting; fn main() -> i64 { greeting.identity(1) }",
        ),
        (
            "consumer_string.hew",
            "import greeting; fn main() -> string { greeting.identity(\"one\") }",
        ),
    ] {
        let label = dir.path().join(name);
        let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        assert!(state.stopped.is_some());
    }
    assert_eq!(batch.dependency_bootstraps(), 1);
    assert_eq!(batch.reused_roots(), 0);
}

#[test]
fn directory_generated_displays_keep_final_file_indices() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("hew.toml"), "[package]\nname = \"app\"\n").unwrap();
    let catalog = dir.path().join("catalog");
    fs::create_dir(&catalog).unwrap();
    fs::write(
        catalog.join("catalog.hew"),
        "pub fn first(value: i64 = 11) -> i64 { value }",
    )
    .unwrap();
    fs::write(
        catalog.join("other.hew"),
        "pub fn other(value: i32 = 22) -> i32 { value }",
    )
    .unwrap();
    let errors = catalog.join("errors.hew");
    fs::write(&errors, "pub enum Fault { Broken; }\nimpl Error for Fault {}\npub fn message() -> string { f\"{Fault.Broken}\" }").unwrap();
    let errors = errors.canonicalize().unwrap();
    let mut options = test_options();
    options.project_dir = Some(dir.path().to_path_buf());
    let mut batch = SourceAnalysisBatch::new(options.clone());
    let source = "import catalog; fn value() -> i64 { catalog.first() + (catalog.other() as i64) } fn main() { println(catalog.message()); }";
    for name in ["consumer_first.hew", "consumer_second.hew"] {
        let label = dir.path().join(name);
        let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
        let checked = state
            .typecheck_result
            .as_ref()
            .unwrap()
            .tco
            .as_ref()
            .unwrap();
        let normalized = checked
            .normalized_program
            .as_ref()
            .expect("Error generates Display");
        let graph = normalized.program.module_graph.as_ref().unwrap();
        let root_path = graph.modules[&graph.root].source_paths.first().unwrap();
        assert_eq!(checked.file_span_indices.path_index(root_path), Some(0));
        let (module, source_module) = graph
            .modules
            .iter()
            .find(|(_, module)| module.source_paths.contains(&errors))
            .unwrap();
        let error_index = checked.file_span_indices.path_index(&errors).unwrap();
        let error_items: Vec<_> = source_module
            .items
            .iter()
            .enumerate()
            .filter(|(ordinal, _)| graph.item_source(module, *ordinal) == Some(&errors))
            .collect();
        assert!(
            error_items.len() > 3,
            "generated Display stays in the declaring peer"
        );
        for (ordinal, _) in error_items {
            assert_eq!(
                checked.file_span_indices.item_index(module, ordinal),
                Some(error_index)
            );
        }
        let lowered = hew_hir::lower_program(
            &state.program,
            checked,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
        assert!(hew_hir::analyze_hir(&lowered.module)
            .diagnostics()
            .is_empty());
    }
    // User impl declarations deliberately use full checking, not the
    // independent-function dependency checkpoint.
    assert_eq!(batch.reused_roots(), 0);
}

#[test]
fn cached_directory_defaults_keep_final_file_indices() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("hew.toml"), "[package]\nname = \"app\"\n").unwrap();
    let catalog = dir.path().join("catalog");
    fs::create_dir(&catalog).unwrap();
    fs::write(
        catalog.join("catalog.hew"),
        "pub fn first(value: i64 = 11) -> i64 { value }",
    )
    .unwrap();
    fs::write(
        catalog.join("other.hew"),
        "pub fn other(value: i32 = 22) -> i32 { value }",
    )
    .unwrap();
    let first = catalog.join("catalog.hew").canonicalize().unwrap();
    let other = catalog.join("other.hew").canonicalize().unwrap();
    let mut options = test_options();
    options.project_dir = Some(dir.path().to_path_buf());
    let mut batch = SourceAnalysisBatch::new(options.clone());
    let source =
        "import catalog; fn main() { println(catalog.first()); println(catalog.other()); }";
    for name in ["consumer_first.hew", "consumer_second.hew"] {
        let label = dir.path().join(name);
        let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
        let checked = state
            .typecheck_result
            .as_ref()
            .unwrap()
            .tco
            .as_ref()
            .unwrap();
        let graph = state.program.module_graph.as_ref().unwrap();
        let root = graph.modules[&graph.root].source_paths.first().unwrap();
        assert_eq!(checked.file_span_indices.path_index(root), Some(0));
        let first_index = checked.file_span_indices.path_index(&first).unwrap();
        let other_index = checked.file_span_indices.path_index(&other).unwrap();
        assert_ne!(first_index, other_index);
        let lowered = hew_hir::lower_program(
            &state.program,
            checked,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
        assert!(hew_hir::analyze_hir(&lowered.module)
            .diagnostics()
            .is_empty());
    }
    assert_eq!(batch.dependency_bootstraps(), 1);
    assert_eq!(batch.dependency_cache_hits(), 1);
    assert_eq!(batch.reused_roots(), 2);
}

#[test]
fn cached_builtin_iterator_effects_match_the_complete_frontend() {
    let dir = tempfile::tempdir().unwrap();
    let options = test_options();
    let mut batch = SourceAnalysisBatch::new(options.clone());
    let source = r#"
fn cached_consumer_first<I>(var it: I) -> i64 where I: Iterator<Item = i64> {
match it.next() { .Some(value) => value, .None => 0 }
}
fn cached_consumer_ready() -> i64 {
let row: Vec<i64> = [1, 2];
cached_consumer_first(row.iter())
}
fn main() {
let callback: fn() -> i64 = cached_consumer_ready;
let grid: Vec<Vec<i64>> = [[1, 2], [3, 4]];
let sums = grid.iter().map(|row: Vec<i64>| row.iter().fold(0, |a: i64, b: i64| a + b)).collect();
println(f"{callback()}");
println(f"{sums[0] + sums[1]}");
}
"#;
    for name in ["first.hew", "second.hew"] {
        let label = dir.path().join(name);
        let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
    }
    assert_eq!(batch.reused_roots(), 2);
    assert_eq!(batch.dependency_bootstraps(), 1);
    assert_eq!(batch.dependency_cache_hits(), 1);
}

/// The dependencies' finalization runs once for a checkpoint; each root
/// extends it with its own facts and still matches a complete check,
/// including roots whose callables reach a dependency's formals or carry
/// suspension obligations, which finalize their effects in full.
#[test]
fn dependency_finalization_runs_once_across_roots() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("util.hew"),
        "pub fn greet() -> i32 { 1 }\n\
         pub fn apply(f: fn(i32) -> i32, value: i32) -> i32 { f(value) }\n\
         pub fn same<T>(left: T, right: T) -> bool { left == right }\n\
         pub fn pause() { sleep(1ms); }\n\
         pub fn run<T>(value: T) -> i32 { let call: fn() -> i32 = || 1; call() }\n",
    )
    .unwrap();
    let mut options = test_options();
    options.project_dir = Some(dir.path().to_path_buf());
    let mut batch = SourceAnalysisBatch::new(options.clone());
    let roots = [
        "import util;\npub fn read_0() -> i32 { util.greet() }\n",
        "import util;\npub fn read_1() -> i32 { util.greet() + util.greet() }\n",
        "import util;\npub fn compare_2() -> bool { util.same(1, 2) && util.same(\"a\", \"b\") }\n",
        "import util;\npub fn rest_3() { util.pause(); }\n",
        "import util;\npub fn twice_4() -> i32 { util.apply(|value: i32| value * 2, 3) }\n",
        "import util;\npub fn pair_5() -> bool { util.same((1, \"a\"), (2, \"b\")) }\n",
        "import util;\npub fn run_6() -> i32 { util.run(6) }\n",
    ];
    for (index, source) in roots.iter().enumerate() {
        let label = dir.path().join(format!("consumer_{index}.hew"));
        let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
    }
    assert_eq!(batch.dependency_bootstraps(), 1);
    assert_eq!(batch.dependency_finalizations(), 1);
    assert_eq!(batch.reused_roots(), roots.len());
    // The closure passed to `apply` carries a suspension obligation of its
    // own and `run` constructs a dependency obligation in a new context:
    // both finalize their effects in full. Every root passing an argument
    // to a dependency's formal also finalizes its task lifetimes in full.
    assert_eq!(
        batch.finalization_reuse(),
        hew_types::check::FinalizationReuse {
            effects: roots.len() - 2,
            type_facts: roots.len(),
            generic_requirements: roots.len(),
            task_lifetimes: 3,
        }
    );
}

#[test]
fn nested_collect_defaults_preserve_effects_with_cached_dependencies() {
    let dir = tempfile::tempdir().unwrap();
    let options = test_options();
    let source =
        include_str!("../../../tests/core-acceptance/cases/nested-collect-literal-effects.hew");
    let suspending = source.replace(
        "|a: i64, b: i64| a + b",
        "|a: i64, b: i64| { sleep(1ms); a + b }",
    );
    for (source, must_suspend) in [(source, false), (suspending.as_str(), true)] {
        let mut batch = SourceAnalysisBatch::new(options.clone());
        for name in ["first.hew", "second.hew"] {
            let label = dir.path().join(name);
            let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
            if must_suspend {
                let output = state
                    .typecheck_result
                    .as_ref()
                    .unwrap()
                    .tco
                    .as_ref()
                    .unwrap();
                assert!(
                    output
                        .errors
                        .iter()
                        .any(|error| error.message.contains("closure suspends via `sleep(...)`")),
                    "{:?}",
                    state.diagnostics
                );
            } else {
                assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
            }
        }
        assert_eq!(batch.reused_roots(), 2);
        assert_eq!(batch.dependency_bootstraps(), 1);
        assert_eq!(batch.dependency_cache_hits(), 1);
    }
}

#[test]
fn nested_stdlib_iter_adapters_preserve_effects_with_cached_dependencies() {
    let dir = tempfile::tempdir().unwrap();
    let options = test_options();
    let source = "fn nested_chain() -> Vec<i64> {\n\
                      let xs: Vec<i64> = [1, 2, 3, 4];\n\
                      xs.iter().map(|x: i64| x + 1).filter(|x: i64| x > 2).map(|x: i64| x * 2).collect()\n\
                  }\n\
                  fn main() {\n\
                      let f: fn() -> Vec<i64> = nested_chain;\n\
                  }\n";
    let suspending = source.replace("x + 1", "{ sleep(1ms); x + 1 }");
    for (source, must_suspend) in [(source, false), (suspending.as_str(), true)] {
        let mut batch = SourceAnalysisBatch::new(options.clone());
        for name in ["first.hew", "second.hew"] {
            let label = dir.path().join(name);
            let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
            if must_suspend {
                let output = state
                    .typecheck_result
                    .as_ref()
                    .unwrap()
                    .tco
                    .as_ref()
                    .unwrap();
                assert!(
                    output
                        .errors
                        .iter()
                        .any(|error| error.message.contains("suspends via `sleep(...)`")),
                    "{:?}",
                    state.diagnostics
                );
            } else {
                assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
            }
        }
    }
}

#[test]
fn independent_roots_reuse_dependencies_with_fresh_alias_scopes() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("greeting.hew"),
        "fn hidden<T>(value: T) -> T { value } pub fn greet(value: i32 = 7) -> i32 { hidden(value) }\n",
    )
    .unwrap();
    let options = test_options();
    let mut batch = SourceAnalysisBatch::new(options.clone());
    let sources = [
        "import greeting; fn consumer_whole_import() -> i32 { greeting.greet() } fn main() {}",
        "import greeting.{ greet as hello }; fn consumer_aliased_import() -> i32 { hello() } fn main() {}",
        "import greeting.{ greet }; fn consumer_named_import() -> i32 { greet() } fn main() {}",
        "import greeting.{ greet as invoke_greeting }; fn consumer_generic_root<T>(value: T) -> T { value } fn main() { let result = consumer_generic_root(invoke_greeting()); }",
    ];
    for (index, source) in sources.iter().enumerate() {
        let label = dir.path().join(format!("consumer_{index}.hew"));
        assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
    }
    assert_eq!(
        batch.reused_roots(),
        4,
        "all ordinary mixed-import roots must reuse the checkpoint"
    );
    assert_eq!(
        batch.dependency_bootstraps(),
        1,
        "distinct physical consumer names share one dependency bootstrap"
    );
    assert_eq!(
        batch.dependency_cache_hits(),
        3,
        "first seed construction is not a cache hit"
    );
}

#[test]
fn dependency_lookup_overlap_and_new_generic_guard_use_full_checker() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("greeting.hew"),
        "fn helper() -> i32 { 7 } pub fn greet() -> i32 { helper() }\n",
    )
    .unwrap();
    let options = test_options();
    let mut batch = SourceAnalysisBatch::new(options.clone());
    for source in [
        "import greeting; fn helper() -> i32 { 9 } fn main() { greeting.greet(); }",
        "import greeting.{ greet as helper }; fn main() { helper(); }",
        "import greeting; fn println() {} fn main() { greeting.greet(); }",
        "import greeting; fn generic<BrandNew>(value: BrandNew) -> BrandNew { value } fn main() { greeting.greet(); }",
    ] {
        let label = dir.path().join("consumer.hew");
        assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
    }
    assert_eq!(batch.reused_roots(), 0);
}

#[test]
fn imported_nominal_and_generic_private_signatures_preserve_full_frontend() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("greeting.hew"), "pub type Thing { value: i32; } fn identity<T>(value: T) -> T { value } pub fn greet(value: Thing) -> i32 { identity(value.value) }\n").unwrap();
    let options = test_options();
    let mut batch = SourceAnalysisBatch::new(options.clone());
    let label = dir.path().join("consumer.hew");
    let source = "import greeting.{ Thing as Data, greet as call }; fn consume(value: Data) -> i32 { call(value) } fn main() {}";
    let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
    assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
    assert_eq!(
        batch.reused_roots(),
        0,
        "nominal registration order remains on the complete checker"
    );
}

#[test]
fn separate_source_epochs_and_real_roots_never_share_dependency_state() {
    let first = tempfile::tempdir().unwrap();
    let second = tempfile::tempdir().unwrap();
    fs::write(
        first.path().join("greeting.hew"),
        "pub fn greet() -> i32 { 7 }",
    )
    .unwrap();
    fs::write(
        second.path().join("greeting.hew"),
        "pub fn greet() -> string { \"second\" }",
    )
    .unwrap();
    let mut options = test_options();
    let mut batch = SourceAnalysisBatch::new(options.clone());
    let first_label = first.path().join("consumer.hew");
    assert_equivalent(
        "import greeting; fn integer_consumer() -> i32 { greeting.greet() }",
        first_label.to_str().unwrap(),
        &options,
        &mut batch,
    );
    let second_label = second.path().join("consumer.hew");
    assert_equivalent(
        "import greeting; fn string_consumer() -> string { greeting.greet() }",
        second_label.to_str().unwrap(),
        &options,
        &mut batch,
    );
    options.documents.insert(
        first.path().join("greeting.hew"),
        "pub fn greet() -> string { \"changed\" }".to_string(),
    );
    let mut proposed = SourceAnalysisBatch::new(options.clone());
    assert_equivalent(
        "import greeting; fn integer_consumer() -> string { greeting.greet() }",
        first_label.to_str().unwrap(),
        &options,
        &mut proposed,
    );
    // The old immutable snapshot is still valid and independently usable.
    let original_options = test_options();
    assert_equivalent(
        "import greeting; fn integer_consumer() -> i32 { greeting.greet() }",
        first_label.to_str().unwrap(),
        &original_options,
        &mut batch,
    );
    assert_eq!(batch.reused_roots(), 3);
    assert_eq!(proposed.reused_roots(), 1);
}
#[test]
fn inferred_import_signatures_keep_checked_constraints_on_full_frontend() {
    let dir = tempfile::tempdir().unwrap();
    let options = test_options();
    for (dependency, source) in [
        (
            "pub fn greet() -> _ { true }",
            "import greeting; fn consumer() -> i32 { greeting.greet() }",
        ),
        (
            "pub fn greet(value: _) -> bool { value }",
            "import greeting; fn consumer() -> bool { greeting.greet(1) }",
        ),
        (
            "pub fn greet() -> Option<_> { Some(true) }",
            "import greeting; fn consumer() -> Option<i32> { greeting.greet() }",
        ),
        (
            "fn hidden() -> _ { true } pub fn greet() -> i32 { 7 }",
            "import greeting; fn main() {}",
        ),
    ] {
        fs::write(dir.path().join("greeting.hew"), dependency).unwrap();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        for name in ["first.hew", "second.hew"] {
            let label = dir.path().join(name);
            assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        }
        assert_eq!(
            batch.reused_roots(),
            0,
            "signature inference must retain full dependency/root registration order"
        );
        assert_eq!(batch.dependency_bootstraps(), 0);
    }
}

#[test]
fn deferred_closure_and_effect_facts_finalize_independently_for_each_root() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("greeting.hew"),
        "pub fn greet() -> i32 { let call: fn(i32) -> i32 = |value: _| value + 1; call(6) }",
    )
    .unwrap();
    let options = test_options();
    let mut batch = SourceAnalysisBatch::new(options.clone());
    for (name, source) in [
        (
            "first.hew",
            "import greeting; fn first() -> i32 { greeting.greet() }",
        ),
        (
            "second.hew",
            "import greeting.{ greet as invoke_greeting }; fn second() -> i32 { let local: fn[suspends]() -> i32 = || invoke_greeting(); local() }",
        ),
    ] {
        let label = dir.path().join(name);
        let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
        let output = state
            .typecheck_result
            .as_ref()
            .unwrap()
            .tco
            .as_ref()
            .unwrap();
        assert!(!output.closure_escape_facts.is_empty());
        assert!(!output.suspension_effects.calls.is_empty());
    }
    assert_eq!(batch.dependency_bootstraps(), 1);
    assert_eq!(batch.dependency_cache_hits(), 1);
}
