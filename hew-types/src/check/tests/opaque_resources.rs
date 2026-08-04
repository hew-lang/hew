#[allow(
    clippy::wildcard_imports,
    reason = "checker tests use the shared private test helpers"
)]
use super::*;
use crate::ffi_contracts::{
    ExternOwnershipContract, ExternParamOwnership, ExternResultOwnership, ExternResultRetention,
    ReleaseDischargeDepth,
};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

const TCP_CONNECTION_SOURCE: &str = r#"
#[resource]
#[opaque]
pub type Connection {}

impl Connection {
    fn close(consuming self) {
        unsafe { hew_tcp_close(self) };
    }
}

extern "C" {
    fn hew_tcp_connect(addr: string) -> Connection;
    fn hew_tcp_connect_timeout(host: string, port: i32, timeout_ms: i32) -> Connection;
    fn hew_tcp_close(consume conn: Connection) -> i32;
}
"#;

#[test]
fn resource_close_discharges_once_but_keeps_non_consuming_reads_available() {
    let output = check_source(
        r"
        #[resource]
        type Socket { fd: i64 }

        impl Socket {
            fn close(consuming self) {}
            fn status(self) -> i64 { self.fd }
        }

        fn probe(socket: Socket) -> i64 {
            socket.close();
            socket.status()
        }
        ",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert_eq!(output.method_call_discharges_receiver.len(), 1);
}

#[test]
fn resource_close_discharge_rejects_a_second_close() {
    let output = check_source(
        r"
        #[resource]
        type Socket { fd: i64 }

        impl Socket {
            fn close(consuming self) {}
        }

        fn bad(socket: Socket) {
            socket.close();
            socket.close();
        }
        ",
    );
    assert!(
        output.errors.iter().any(|error| {
            error
                .message
                .contains("resource `socket` cannot be closed more than once")
        }),
        "{:#?}",
        output.errors
    );
}

#[test]
fn tcp_like_owned_results_join_one_qualified_lifecycle() {
    let output = check_source_in_module(
        TCP_CONNECTION_SOURCE,
        vec!["std".to_string(), "net".to_string()],
    );
    let candidate = output
        .opaque_resource_candidates
        .candidates
        .get("std.net.Connection")
        .expect("TCP producers must join their qualified lifecycle");
    assert_eq!(candidate.owner_module, "std.net");
    assert_eq!(candidate.release_symbol, "hew_tcp_close");
    assert_eq!(
        candidate.producer_symbols,
        ["hew_tcp_connect", "hew_tcp_connect_timeout"]
            .into_iter()
            .map(str::to_string)
            .collect()
    );
    assert_eq!(
        candidate.producer_modules,
        ["std.net"].into_iter().map(str::to_string).collect()
    );
    assert!(
        output.opaque_resource_candidates.conflicts.is_empty(),
        "{:#?}",
        output.opaque_resource_candidates.conflicts
    );
}

fn collect_hew_sources(directory: &Path, sources: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(directory).expect("read stdlib directory") {
        let path = entry.expect("read stdlib entry").path();
        if path.is_dir() {
            collect_hew_sources(&path, sources);
        } else if path.extension().is_some_and(|extension| extension == "hew") {
            sources.push(path);
        }
    }
}

fn canonical_std_module(std_root: &Path, source: &Path) -> Vec<String> {
    let relative = source.strip_prefix(std_root).expect("source is below std/");
    let mut module = vec!["std".to_string()];
    if let Some(parent) = relative.parent() {
        module.extend(parent.components().map(|component| {
            component
                .as_os_str()
                .to_str()
                .expect("stdlib paths are UTF-8")
                .to_string()
        }));
    }
    let stem = relative
        .file_stem()
        .and_then(|stem| stem.to_str())
        .expect("stdlib source has a UTF-8 stem");
    if module.last().is_none_or(|last| last != stem) {
        module.push(stem.to_string());
    }
    module
}

type ParsedStdModules = BTreeMap<Vec<String>, Vec<hew_parser::ast::Spanned<Item>>>;

fn parse_shipped_std_sources(std_root: &Path) -> (ParsedStdModules, BTreeSet<String>) {
    let mut sources = Vec::new();
    collect_hew_sources(std_root, &mut sources);
    sources.sort();

    let mut parsed_modules = BTreeMap::new();
    let mut resource_types = BTreeSet::new();
    for source in sources {
        let text = fs::read_to_string(&source).expect("read stdlib source");
        let parsed = hew_parser::parse(&text);
        assert!(
            parsed.errors.is_empty(),
            "{} must parse: {:#?}",
            source.display(),
            parsed.errors
        );
        let module_path = canonical_std_module(std_root, &source);
        let inherent_closes: BTreeSet<_> = parsed
            .program
            .items
            .iter()
            .filter_map(|(item, _)| {
                let Item::Impl(implementation) = item else {
                    return None;
                };
                let TypeExpr::Named { name, .. } = &implementation.target_type.0 else {
                    return None;
                };
                implementation
                    .methods
                    .iter()
                    .any(|method| method.name == "close" && method.consumes_self)
                    .then(|| name.clone())
            })
            .collect();
        for (item, _) in &parsed.program.items {
            let Item::TypeDecl(declaration) = item else {
                continue;
            };
            if declaration.resource_marker != hew_parser::ast::ResourceMarker::Resource
                || !declaration.is_opaque
            {
                continue;
            }
            assert!(
                declaration
                    .consuming_methods
                    .iter()
                    .any(|method| method == "close")
                    || inherent_closes.contains(&declaration.name),
                "{}.{} must expose one consuming close method",
                module_path.join("."),
                declaration.name
            );
            resource_types.insert(format!("{}.{}", module_path.join("."), declaration.name));
        }
        assert!(
            parsed_modules
                .insert(module_path, parsed.program.items)
                .is_none(),
            "canonical std module identity must be unique"
        );
    }

    (parsed_modules, resource_types)
}

fn shipped_std_module_graph(parsed_modules: &ParsedStdModules) -> ModuleGraph {
    let mut module_graph = ModuleGraph::new(ModuleId::root());
    for (module_path, source_items) in parsed_modules {
        let mut items = source_items.clone();
        let mut imports = Vec::new();
        for (item, span) in &mut items {
            let Item::Import(declaration) = item else {
                continue;
            };
            let resolved = parsed_modules.get(&declaration.path).unwrap_or_else(|| {
                panic!(
                    "{} imports missing shipped module {}",
                    module_path.join("."),
                    declaration.path.join(".")
                )
            });
            declaration.resolved_items = Some(resolved.clone());
            imports.push(hew_parser::module::ModuleImport {
                target: ModuleId::new(declaration.path.clone()),
                spec: declaration.spec.clone(),
                span: span.clone(),
            });
        }
        module_graph
            .add_module(Module {
                id: ModuleId::new(module_path.clone()),
                items,
                imports,
                source_paths: vec![],
                doc: None,
            })
            .expect("add shipped std module");
    }
    module_graph
        .compute_topo_order()
        .expect("shipped std module graph must be acyclic");
    module_graph
}

fn shipped_std_candidate_inventory() -> (BTreeSet<String>, OpaqueResourceCandidateGraph) {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-types is below repository root")
        .to_path_buf();
    let (resource_types, parsed_modules) = {
        let (modules, resources) = parse_shipped_std_sources(&repo_root.join("std"));
        (resources, modules)
    };
    let program = Program {
        module_graph: Some(shipped_std_module_graph(&parsed_modules)),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    checker.collect_types(&program);
    checker.collect_declared_type_param_names(&program);
    checker.type_decls_registered = true;
    checker.collect_functions(&program);
    let graph = checker.derive_opaque_resource_candidate_graph(&checker.fn_sigs);
    (resource_types, graph)
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct LifecycleEvidenceMatrix {
    schema_version: u32,
    resources: Vec<LifecycleEvidence>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct LifecycleEvidence {
    source_path: String,
    resource: String,
    release_symbol: String,
    runtime: TestEvidence,
    wasm: WasmEvidence,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TestEvidence {
    path: String,
    test: String,
    valid_handle: bool,
    execution_profile: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct WasmEvidence {
    profile: String,
    disposition: String,
    proof_kind: String,
}

fn assert_nonempty(value: &str, field: &str, resource: &str) {
    assert!(
        !value.trim().is_empty(),
        "{resource} has empty lifecycle evidence field {field}"
    );
}

fn assert_test_anchor(repo_root: &Path, evidence: &TestEvidence, field: &str, resource: &str) {
    assert_nonempty(&evidence.path, &format!("{field}.path"), resource);
    assert_nonempty(&evidence.test, &format!("{field}.test"), resource);
    assert!(
        evidence.valid_handle,
        "{resource} {field} evidence must exercise a real compiled value or valid handle"
    );
    assert!(
        matches!(
            evidence.execution_profile.as_str(),
            "local" | "external-network"
        ),
        "{resource} {field} has an invalid execution profile {}",
        evidence.execution_profile
    );
    let path = repo_root.join(&evidence.path);
    let source = fs::read_to_string(&path).unwrap_or_else(|error| {
        panic!(
            "{resource} {field} evidence file {} is missing: {error}",
            path.display()
        )
    });
    assert!(
        source.contains(&format!("fn {}(", evidence.test)),
        "{resource} {field} evidence test {} is missing from {}",
        evidence.test,
        path.display()
    );
}

fn assert_wasm_anchor(evidence: &WasmEvidence, resource: &str) {
    assert_nonempty(&evidence.profile, "wasm.profile", resource);
    assert_nonempty(&evidence.proof_kind, "wasm.proof_kind", resource);
    assert_eq!(evidence.profile, "wasm32-wasi");
    assert!(
        matches!(evidence.disposition.as_str(), "accepted" | "rejected"),
        "{resource} has an invalid measured Wasm disposition {}",
        evidence.disposition
    );
    match evidence.disposition.as_str() {
        "accepted" => assert!(
            matches!(
                evidence.proof_kind.as_str(),
                "public-lifecycle" | "internal-transient"
            ),
            "{resource} accepted Wasm evidence has invalid proof kind {}",
            evidence.proof_kind
        ),
        "rejected" => assert_eq!(
            evidence.proof_kind, "rejected-boundary",
            "{resource} rejected Wasm evidence must prove the rejection boundary"
        ),
        _ => unreachable!("Wasm disposition was validated above"),
    }
}

fn source_derived_resource_key(source_path: &str, resource: &str) -> String {
    let path = Path::new(source_path);
    assert_eq!(
        path.extension().and_then(|value| value.to_str()),
        Some("hew")
    );
    let mut module: Vec<_> = path
        .parent()
        .expect("shipped source has a parent")
        .components()
        .map(|component| component.as_os_str().to_string_lossy().into_owned())
        .collect();
    let stem = path
        .file_stem()
        .expect("shipped source has a stem")
        .to_string_lossy();
    if module.last().is_none_or(|last| last != &stem) {
        module.push(stem.into_owned());
    }
    format!("{}.{}", module.join("."), resource)
}

#[test]
fn shipped_source_and_checker_lifecycle_inventories_are_a_bijection() {
    let (source_resources, graph) = shipped_std_candidate_inventory();
    assert!(
        !source_resources.is_empty(),
        "the source inventory must have teeth"
    );
    assert!(
        graph.conflicts.is_empty(),
        "source-derived lifecycle conflicts: {:#?}",
        graph.conflicts
    );
    let candidate_resources: BTreeSet<_> = graph
        .candidates
        .keys()
        .map(|declaration| declaration.full_path().to_string())
        .collect();
    assert_eq!(
        candidate_resources, source_resources,
        "every shipped closeable opaque declaration must reach exactly one checker candidate, and no contract-only candidate may survive source removal"
    );
    for candidate in graph.candidates.values() {
        assert!(!candidate.producer_symbols.is_empty());
        assert!(!candidate.release_symbol.is_empty());
    }

    let json = serde_json::to_value(&graph).expect("candidate graph is machine-readable");
    assert_eq!(
        json["candidates"]
            .as_object()
            .expect("JSON candidate map")
            .len(),
        source_resources.len()
    );
}

#[test]
fn shipped_lifecycle_evidence_is_complete_for_the_structural_inventory() {
    let (source_resources, graph) = shipped_std_candidate_inventory();
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-types is below repository root")
        .to_path_buf();
    let matrix_path = repo_root.join("scripts/opaque-resource-lifecycle-evidence.json");
    let matrix: LifecycleEvidenceMatrix = serde_json::from_str(
        &fs::read_to_string(&matrix_path).expect("read lifecycle evidence matrix"),
    )
    .expect("lifecycle evidence matrix must match its strict schema");
    assert_eq!(matrix.schema_version, 2);

    let matrix_resources: BTreeSet<_> = matrix
        .resources
        .iter()
        .map(|evidence| source_derived_resource_key(&evidence.source_path, &evidence.resource))
        .collect();
    assert_eq!(
        matrix_resources.len(),
        matrix.resources.len(),
        "each shipped source identity must have exactly one evidence row"
    );
    assert_eq!(
        matrix_resources, source_resources,
        "the evidence matrix must have exactly one row for every structurally discovered closeable opaque resource"
    );

    for evidence in &matrix.resources {
        let resource = source_derived_resource_key(&evidence.source_path, &evidence.resource);
        assert!(
            repo_root.join(&evidence.source_path).is_file(),
            "{resource} points to a missing shipped source {}",
            evidence.source_path
        );
        assert_eq!(
            evidence.release_symbol,
            graph.candidates[resource.as_str()].release_symbol,
            "{resource} evidence must name the source-derived release authority"
        );
        assert_test_anchor(&repo_root, &evidence.runtime, "runtime", &resource);
        assert_wasm_anchor(&evidence.wasm, &resource);
    }
}

#[test]
fn generated_contract_without_source_or_unknown_source_family_never_enters_inventory() {
    let missing_source =
        check_source_in_module("fn main() {}", vec!["std".to_string(), "fs".to_string()]);
    assert!(missing_source
        .opaque_resource_candidates
        .candidates
        .is_empty());

    let unknown_family = check_source_in_module(
        r#"
        #[resource]
        #[opaque]
        type Unknown {}
        impl Unknown {
            fn close(consuming self) { unsafe { unknown_free(self) }; }
        }
        extern "C" {
            fn unknown_new() -> Unknown;
            fn unknown_free(consume value: Unknown);
        }
        "#,
        vec!["std".to_string(), "unknown".to_string()],
    );
    assert!(unknown_family
        .opaque_resource_candidates
        .candidates
        .is_empty());
    assert!(unknown_family
        .opaque_resource_candidates
        .conflicts
        .is_empty());
}

#[test]
fn root_symbol_spoof_cannot_inherit_qualified_lifecycle() {
    let output = check_source(TCP_CONNECTION_SOURCE);
    assert!(output.opaque_resource_candidates.candidates.is_empty());
    assert!(output.opaque_resource_candidates.conflicts.is_empty());
}

#[test]
fn foreign_module_symbol_and_type_spoof_cannot_inherit_lifecycle() {
    let output = check_source_in_module(
        TCP_CONNECTION_SOURCE,
        vec!["user".to_string(), "net".to_string()],
    );
    assert!(output.opaque_resource_candidates.candidates.is_empty());
    assert!(output.opaque_resource_candidates.conflicts.is_empty());
}

#[test]
fn short_name_collision_records_result_mismatch_without_candidate() {
    let output = check_source_in_module(
        r#"
        #[opaque]
        pub type Connection {}
        #[opaque]
        pub type Other {}

        extern "C" {
            fn hew_tcp_connect(addr: string) -> Other;
            fn hew_tcp_close(consume conn: Connection) -> i32;
        }
        "#,
        vec!["std".to_string(), "net".to_string()],
    );
    assert!(output.opaque_resource_candidates.candidates.is_empty());
    assert!(matches!(
        output.opaque_resource_candidates.conflicts.as_slice(),
        [OpaqueResourceLifecycleConflict {
            kind: OpaqueResourceLifecycleConflictKind::ProducerResultMismatch { .. },
            ..
        }]
    ));
}

#[test]
fn mismatched_source_consume_release_records_conflict() {
    let output = check_source_in_module(
        r#"
        #[opaque]
        pub type Connection {}
        #[opaque]
        pub type Other {}

        extern "C" {
            fn hew_tcp_connect(addr: string) -> Connection;
            fn hew_tcp_close(consume conn: Other) -> i32;
        }
        "#,
        vec!["std".to_string(), "net".to_string()],
    );
    assert!(output.opaque_resource_candidates.candidates.is_empty());
    assert!(matches!(
        output.opaque_resource_candidates.conflicts.as_slice(),
        [OpaqueResourceLifecycleConflict {
            kind: OpaqueResourceLifecycleConflictKind::ReleaseSignatureMismatch { .. },
            ..
        }]
    ));
}

#[test]
fn missing_source_release_records_conflict() {
    let output = check_source_in_module(
        r#"
        #[opaque]
        pub type Connection {}

        extern "C" {
            fn hew_tcp_connect(addr: string) -> Connection;
        }
        "#,
        vec!["std".to_string(), "net".to_string()],
    );
    assert!(output.opaque_resource_candidates.candidates.is_empty());
    assert!(matches!(
        output.opaque_resource_candidates.conflicts.as_slice(),
        [OpaqueResourceLifecycleConflict {
            kind: OpaqueResourceLifecycleConflictKind::ReleaseDeclarationMissing,
            ..
        }]
    ));
}

#[test]
fn borrowed_or_untyped_results_do_not_mint_candidates() {
    let output = check_source_in_module(
        r#"
        #[opaque]
        pub type Connection {}

        extern "C" {
            fn hew_tcp_read(conn: Connection) -> bytes;
            fn hew_tcp_close(consume conn: Connection) -> i32;
        }
        "#,
        vec!["std".to_string(), "net".to_string()],
    );
    assert!(output.opaque_resource_candidates.candidates.is_empty());
    assert!(output.opaque_resource_candidates.conflicts.is_empty());
}

#[test]
fn synthetic_borrowed_view_without_disposer_is_excluded() {
    let checker = checker_with_registered_module(
        r#"
        #[opaque]
        pub type SocketView {}
        extern "C" {
            fn example_socket_view() -> SocketView;
        }
        "#,
        &["example", "io"],
    );
    let contracts = [(
        "example_socket_view",
        ExternOwnershipContract {
            params: &[],
            resource_param_types: &[],
            resource_result_type: None,
            result: ExternResultOwnership::Borrowed,
            release_symbol: "",
            discharge_depth: ReleaseDischargeDepth::None,
            result_retention: ExternResultRetention::Unspecified,
        },
    )];
    let graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
    assert!(graph.candidates.is_empty());
    assert!(graph.conflicts.is_empty());
}

fn checker_with_registered_module(source: &str, module_path: &[&str]) -> Checker {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let root_id = ModuleId::root();
    let module_id = ModuleId::new(module_path.iter().map(ToString::to_string).collect());
    let module = Module {
        id: module_id.clone(),
        items: parsed.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let mut module_graph = ModuleGraph::new(root_id.clone());
    module_graph.add_module(module).expect("add module");
    module_graph.topo_order = vec![module_id, root_id];
    let program = Program {
        module_graph: Some(module_graph),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    checker.collect_types(&program);
    checker.collect_declared_type_param_names(&program);
    checker.type_decls_registered = true;
    checker.collect_functions(&program);
    checker
}

fn checker_with_resolved_module_graph(sources: &[(&[&str], &str)]) -> Checker {
    let root_id = ModuleId::root();
    let module_ids: Vec<_> = sources
        .iter()
        .map(|(path, _)| ModuleId::new(path.iter().map(ToString::to_string).collect()))
        .collect();
    let mut parsed_items = Vec::with_capacity(sources.len());
    for (_, source) in sources {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
        parsed_items.push(parsed.program.items);
    }

    let mut module_graph = ModuleGraph::new(root_id.clone());
    for (index, module_id) in module_ids.iter().enumerate() {
        let mut items = parsed_items[index].clone();
        let mut imports = Vec::new();
        for (item, span) in &mut items {
            let Item::Import(declaration) = item else {
                continue;
            };
            let target_index = module_ids
                .iter()
                .position(|candidate| candidate.path == declaration.path)
                .expect("test import target must be present in the graph");
            declaration.resolved_items = Some(parsed_items[target_index].clone());
            imports.push(hew_parser::module::ModuleImport {
                target: module_ids[target_index].clone(),
                spec: declaration.spec.clone(),
                span: span.clone(),
            });
        }
        module_graph
            .add_module(Module {
                id: module_id.clone(),
                items,
                imports,
                source_paths: vec![],
                doc: None,
            })
            .expect("add module");
    }
    module_graph.topo_order = module_ids
        .iter()
        .cloned()
        .chain(std::iter::once(root_id))
        .collect();
    let program = Program {
        module_graph: Some(module_graph),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    checker.collect_types(&program);
    checker.collect_declared_type_param_names(&program);
    checker.type_decls_registered = true;
    checker.collect_functions(&program);
    checker
}

fn synthetic_resource_contracts(
    producers: &[(&'static str, &'static str)],
) -> Vec<(&'static str, ExternOwnershipContract)> {
    let mut contracts = vec![(
        "example_socket_close",
        ExternOwnershipContract {
            params: &[ExternParamOwnership::Consume],
            resource_param_types: &["example.owner.Socket"],
            resource_result_type: None,
            result: ExternResultOwnership::None,
            release_symbol: "",
            discharge_depth: ReleaseDischargeDepth::None,
            result_retention: ExternResultRetention::Unspecified,
        },
    )];
    contracts.extend(producers.iter().map(|(symbol, release_symbol)| {
        (
            *symbol,
            ExternOwnershipContract {
                params: &[],
                resource_param_types: &[],
                resource_result_type: Some("example.owner.Socket"),
                result: ExternResultOwnership::Fresh,
                release_symbol,
                discharge_depth: ReleaseDischargeDepth::Shallow,
                result_retention: ExternResultRetention::ResourceTransfer,
            },
        )
    }));
    contracts
}

const SYNTHETIC_OWNER: &str = r#"
#[resource]
#[opaque]
pub type Socket {}
impl Socket {
    fn close(consuming self) { unsafe { example_socket_close(self) }; }
}
extern "C" {
    fn example_socket_close(consume socket: Socket) -> i32;
}
"#;

#[test]
fn generic_extern_template_joins_only_exact_canonical_contract_expansions() {
    let checker = checker_with_registered_module(
        r#"
        #[resource]
        #[opaque]
        pub type Socket {}
        impl Socket {
            fn close(consuming self) { unsafe { example_socket_close(self) }; }
        }
        extern "C" {
            fn example_socket_close(consume socket: Socket) -> i32;
            #[extern_symbol("example_socket_{T}")]
            fn open_typed() -> Socket;
        }
        "#,
        &["example", "owner"],
    );

    let contracts = synthetic_resource_contracts(&[("example_socket_ptr", "example_socket_close")]);
    let graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
    let candidate = graph
        .candidates
        .get("example.owner.Socket")
        .expect("canonical `{T}` expansion must join the qualified lifecycle");
    assert_eq!(
        candidate.producer_symbols,
        ["example_socket_ptr"]
            .into_iter()
            .map(str::to_string)
            .collect()
    );
    assert_eq!(
        candidate.producer_modules,
        ["example.owner"].into_iter().map(str::to_string).collect()
    );

    let wrong =
        synthetic_resource_contracts(&[("example_socket_not_a_token", "example_socket_close")]);
    let wrong_graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &wrong);
    assert!(
        wrong_graph.candidates.is_empty(),
        "template-shaped but non-canonical endpoint must not gain lifecycle authority"
    );
}

#[test]
fn foreign_producer_joins_release_declared_only_by_nominal_owner() {
    let checker = checker_with_resolved_module_graph(&[
        (&["example", "owner"], SYNTHETIC_OWNER),
        (
            &["example", "bridge"],
            r#"
            import example::owner;
            extern "C" {
                fn example_socket_open() -> owner.Socket;
            }
            "#,
        ),
    ]);
    let contracts =
        synthetic_resource_contracts(&[("example_socket_open", "example_socket_close")]);
    let graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
    let candidate = graph
        .candidates
        .get("example.owner.Socket")
        .expect("direct imported result must join the owner release");
    assert_eq!(candidate.owner_module, "example.owner");
    assert_eq!(
        candidate.producer_modules,
        ["example.bridge"].into_iter().map(str::to_string).collect()
    );
    assert!(graph.conflicts.is_empty(), "{:#?}", graph.conflicts);
}

#[test]
fn module_and_named_import_aliases_preserve_imported_owner() {
    let cases = [
        (
            &["example", "module_alias"][..],
            r#"
            import example::owner as device;
            extern "C" {
                fn example_socket_open() -> device.Socket;
            }
            "#,
        ),
        (
            &["example", "named_alias"][..],
            r#"
            import example::owner::{ Socket as ImportedSocket };
            extern "C" {
                fn example_socket_open() -> ImportedSocket;
            }
            "#,
        ),
    ];
    for (producer_path, producer_source) in cases {
        let checker = checker_with_resolved_module_graph(&[
            (&["example", "owner"], SYNTHETIC_OWNER),
            (producer_path, producer_source),
        ]);
        let contracts =
            synthetic_resource_contracts(&[("example_socket_open", "example_socket_close")]);
        let graph = checker
            .derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
        let candidate = graph
            .candidates
            .get("example.owner.Socket")
            .unwrap_or_else(|| {
                panic!(
                    "resolved import alias must retain owner identity; producer={producer_path:?}; graph={graph:#?}"
                )
            });
        assert_eq!(
            candidate.producer_modules,
            [producer_path.join(".")].into_iter().collect()
        );
        assert_eq!(candidate.producer_symbols.len(), 1);
        assert!(graph.conflicts.is_empty(), "{:#?}", graph.conflicts);
    }
}

#[test]
fn unimported_and_wrong_module_lookalikes_have_no_candidate_authority() {
    let checker = checker_with_resolved_module_graph(&[
        (&["example", "owner"], SYNTHETIC_OWNER),
        (
            &["example", "other"],
            r"
            #[opaque]
            pub type Socket {}
            ",
        ),
        (
            &["example", "unimported"],
            r#"
            extern "C" {
                fn example_socket_open_unimported() -> owner.Socket;
            }
            "#,
        ),
        (
            &["example", "wrong"],
            r#"
            import example::other;
            extern "C" {
                fn example_socket_open_wrong() -> other.Socket;
            }
            "#,
        ),
    ]);
    let contracts = synthetic_resource_contracts(&[
        ("example_socket_open_unimported", "example_socket_close"),
        ("example_socket_open_wrong", "example_socket_close"),
    ]);
    let graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
    assert!(graph.candidates.is_empty());
    assert!(graph.conflicts.is_empty());
}

#[test]
fn release_declared_off_owner_cannot_discharge_imported_result() {
    let checker = checker_with_resolved_module_graph(&[
        (
            &["example", "owner"],
            r"
            #[opaque]
            pub type Socket {}
            ",
        ),
        (
            &["example", "bridge"],
            r#"
            import example::owner;
            extern "C" {
                fn example_socket_open() -> owner.Socket;
                fn example_socket_close(consume socket: owner.Socket) -> i32;
            }
            "#,
        ),
    ]);
    let contracts =
        synthetic_resource_contracts(&[("example_socket_open", "example_socket_close")]);
    let graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
    assert!(graph.candidates.is_empty());
    assert!(matches!(
        graph.conflicts.as_slice(),
        [OpaqueResourceLifecycleConflict {
            kind: OpaqueResourceLifecycleConflictKind::ReleaseDeclarationMissing,
            ..
        }]
    ));
}

#[test]
fn imported_producers_aggregate_only_with_matching_lifecycle() {
    let checker = checker_with_resolved_module_graph(&[
        (&["example", "owner"], SYNTHETIC_OWNER),
        (
            &["example", "left"],
            r#"
            import example::owner;
            extern "C" {
                fn example_socket_open_left() -> owner.Socket;
            }
            "#,
        ),
        (
            &["example", "right"],
            r#"
            import example::owner;
            extern "C" {
                fn example_socket_open_right() -> owner.Socket;
            }
            "#,
        ),
        (
            &["example", "noise"],
            r#"
            import example::owner;
            extern "C" {
                fn example_socket_open_left() -> i64;
            }
            "#,
        ),
    ]);
    let mut contracts = synthetic_resource_contracts(&[
        ("example_socket_open_left", "example_socket_close"),
        ("example_socket_open_right", "example_socket_close"),
    ]);
    let matching_graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
    let matching = matching_graph
        .candidates
        .get("example.owner.Socket")
        .unwrap_or_else(|| {
            panic!("matching imported producers must aggregate: {matching_graph:#?}")
        });
    assert_eq!(matching.producer_symbols.len(), 2);
    assert_eq!(
        matching.producer_modules,
        ["example.left", "example.right"]
            .into_iter()
            .map(str::to_string)
            .collect()
    );
    assert!(matching_graph.conflicts.is_empty());

    let right = contracts
        .iter_mut()
        .find(|(symbol, _)| *symbol == "example_socket_open_right")
        .expect("right producer contract");
    right.1.discharge_depth = ReleaseDischargeDepth::Deep;

    let graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
    assert!(!graph.candidates.contains_key("example.owner.Socket"));
    assert!(graph.conflicts.iter().any(|conflict| matches!(
        conflict.kind,
        OpaqueResourceLifecycleConflictKind::MultipleProducerLifecycle { .. }
    )));
}

#[test]
fn synthetic_non_net_contract_uses_the_same_candidate_graph() {
    let checker = checker_with_registered_module(
        r#"
        #[resource]
        #[opaque]
        pub type Socket {}
        impl Socket {
            fn close(consuming self) {
                unsafe { example_socket_close(self) };
            }
        }
        extern "C" {
            fn example_socket_open() -> Socket;
            fn example_socket_close(consume socket: Socket) -> i32;
        }
        "#,
        &["example", "io"],
    );
    let contracts = [
        (
            "example_socket_close",
            ExternOwnershipContract {
                params: &[ExternParamOwnership::Consume],
                resource_param_types: &["example.io.Socket"],
                resource_result_type: None,
                result: ExternResultOwnership::None,
                release_symbol: "",
                discharge_depth: ReleaseDischargeDepth::None,
                result_retention: ExternResultRetention::Unspecified,
            },
        ),
        (
            "example_socket_open",
            ExternOwnershipContract {
                params: &[],
                resource_param_types: &[],
                resource_result_type: Some("example.io.Socket"),
                result: ExternResultOwnership::Fresh,
                release_symbol: "example_socket_close",
                discharge_depth: ReleaseDischargeDepth::Shallow,
                result_retention: ExternResultRetention::ResourceTransfer,
            },
        ),
    ];
    let graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
    let candidate = graph
        .candidates
        .get("example.io.Socket")
        .expect("synthetic family must use generic qualified support");
    assert_eq!(candidate.owner_module, "example.io");
    assert_eq!(candidate.release_symbol, "example_socket_close");
    assert!(graph.conflicts.is_empty(), "{:#?}", graph.conflicts);
}

#[test]
fn disagreeing_producers_record_conflict_instead_of_selecting_a_release() {
    let checker = checker_with_registered_module(
        r#"
        #[resource]
        #[opaque]
        pub type Socket {}
        impl Socket {
            fn close(consuming self) {
                unsafe { example_socket_close(self) };
            }
        }
        extern "C" {
            fn example_socket_open() -> Socket;
            fn example_socket_clone() -> Socket;
            fn example_socket_close(consume socket: Socket) -> i32;
            fn example_socket_drop(consume socket: Socket) -> i32;
        }
        "#,
        &["example", "io"],
    );
    let release = |symbol| {
        (
            symbol,
            ExternOwnershipContract {
                params: &[ExternParamOwnership::Consume],
                resource_param_types: &["example.io.Socket"],
                resource_result_type: None,
                result: ExternResultOwnership::None,
                release_symbol: "",
                discharge_depth: ReleaseDischargeDepth::None,
                result_retention: ExternResultRetention::Unspecified,
            },
        )
    };
    let producer = |symbol, release_symbol| {
        (
            symbol,
            ExternOwnershipContract {
                params: &[],
                resource_param_types: &[],
                resource_result_type: Some("example.io.Socket"),
                result: ExternResultOwnership::Fresh,
                release_symbol,
                discharge_depth: ReleaseDischargeDepth::Shallow,
                result_retention: ExternResultRetention::ResourceTransfer,
            },
        )
    };
    let contracts = [
        release("example_socket_close"),
        release("example_socket_drop"),
        producer("example_socket_open", "example_socket_close"),
        producer("example_socket_clone", "example_socket_drop"),
    ];
    let graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(&checker.fn_sigs, &contracts);
    assert!(
        !graph.candidates.contains_key("example.io.Socket"),
        "conflicting lifecycle must have no deterministic winner"
    );
    assert!(graph.conflicts.iter().any(|conflict| matches!(
        conflict.kind,
        OpaqueResourceLifecycleConflictKind::MultipleProducerLifecycle { .. }
    )));
}
