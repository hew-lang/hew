#[allow(
    clippy::wildcard_imports,
    reason = "checker tests use the shared private test helpers"
)]
use super::*;
use crate::ffi_contracts::{
    ExternOwnershipContract, ExternParamOwnership, ExternResultOwnership, ExternResultRetention,
    ReleaseDischargeDepth,
};

const TCP_CONNECTION_SOURCE: &str = r#"
#[opaque]
pub type Connection {}

extern "C" {
    fn hew_tcp_connect(addr: string) -> Connection;
    fn hew_tcp_connect_timeout(host: string, port: i32, timeout_ms: i32) -> Connection;
    fn hew_tcp_close(consume conn: Connection) -> i32;
}
"#;

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
                result_retention: ExternResultRetention::Transferred,
            },
        )
    }));
    contracts
}

const SYNTHETIC_OWNER: &str = r#"
#[opaque]
pub type Socket {}
extern "C" {
    fn example_socket_close(consume socket: Socket) -> i32;
}
"#;

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
        .expect("matching imported producers must aggregate");
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
        #[opaque]
        pub type Socket {}
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
                result_retention: ExternResultRetention::Transferred,
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
        #[opaque]
        pub type Socket {}
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
                result_retention: ExternResultRetention::Transferred,
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
