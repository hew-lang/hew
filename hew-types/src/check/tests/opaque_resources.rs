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
#[resource]
#[opaque]
pub type Connection {}

impl Connection {
    fn close(consume self) {
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
fn borrowing_close_on_resource_type_is_rejected() {
    let output = check_source(
        r"#[resource]
type Socket {
    fd: i64;
}

impl Socket {
    fn close(self) {}
}
",
    );
    assert!(
        output.errors.iter().any(|error| error
            .message
            .contains("`close` on a resource type must consume its receiver")),
        "a borrowing `close` on a `#[resource]` type must be rejected, got: {:#?}",
        output.errors
    );
}

#[test]
fn borrowing_close_on_opaque_type_is_rejected() {
    let output = check_source(
        r"
        #[opaque]
        type Handle {}

        impl Handle {
            fn close(self) {}
        }
        ",
    );
    assert!(
        output.errors.iter().any(|error| error
            .message
            .contains("`close` on a resource type must consume its receiver")),
        "a borrowing `close` on an `#[opaque]` type must be rejected, got: {:#?}",
        output.errors
    );
}

#[test]
fn borrowing_close_on_unmarked_type_stays_legal() {
    let output = check_source(
        r"type Widget {
    fd: i64;
}

impl Widget {
    fn close(self) {}
}
",
    );
    assert!(
        !output.errors.iter().any(|error| error
            .message
            .contains("`close` on a resource type must consume its receiver")),
        "a borrowing `close` on an unmarked type is legal, got: {:#?}",
        output.errors
    );
}

#[test]
fn resource_close_discharges_and_moves_the_receiver() {
    let output = check_source(
        r"#[resource]
type Socket {
    fd: i64;
}

impl Socket {
    fn close(consume self) {}
    fn status(self) -> i64 {
        self.fd
    }
}

fn probe(consume socket: Socket) -> i64 {
    socket.close();
    socket.status()
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("use of moved value `socket`")),
        "close must move the receiver so a later probe is use-after-move, got: {:#?}",
        output.errors
    );
    assert_eq!(output.method_call_discharges_receiver.len(), 1);
}

#[test]
fn non_close_consuming_method_moves_the_receiver() {
    let output = check_source(
        r"#[resource]
type Socket {
    fd: i64;
}

impl Socket {
    fn close(consume self) {}
    fn detach(consume self) -> i64 {
        self.fd
    }
    fn status(self) -> i64 {
        self.fd
    }
}

fn probe(consume socket: Socket) -> i64 {
    let _ = socket.detach();
    socket.status()
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("use of moved value `socket`")),
        "a consuming method not named close must still move its receiver, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_discharges_receiver.is_empty(),
        "detach is not the canonical close and must not register a discharge, got: {:?}",
        output.method_call_discharges_receiver
    );
}

#[test]
fn resource_close_discharge_rejects_a_second_close() {
    let output = check_source(
        r"#[resource]
type Socket {
    fd: i64;
}

impl Socket {
    fn close(consume self) {}
}

fn bad(consume socket: Socket) {
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
        .get(
            &output
                .defs
                .lookup_path("std.net.Connection")
                .expect("declared resource"),
        )
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
fn generated_contract_without_source_or_unknown_source_family_never_enters_inventory() {
    let missing_source =
        check_source_in_module("fn main() {}", vec!["std".to_string(), "fs".to_string()]);
    assert!(missing_source
        .opaque_resource_candidates
        .candidates
        .keys()
        .all(|name| missing_source.defs.path(*name) == "std.builtins.ActorRequestOwner"));

    let unknown_family = check_source_in_module(
        r#"
        #[resource]
        #[opaque]
        type Unknown {}
        impl Unknown {
            fn close(consume self) { unsafe { unknown_free(self) }; }
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
        .keys()
        .all(|name| unknown_family.defs.path(*name) == "std.builtins.ActorRequestOwner"));
    assert!(unknown_family
        .opaque_resource_candidates
        .conflicts
        .is_empty());
}

#[test]
fn root_symbol_spoof_cannot_inherit_qualified_lifecycle() {
    let output = check_source(TCP_CONNECTION_SOURCE);
    assert!(output
        .opaque_resource_candidates
        .candidates
        .keys()
        .all(|name| output.defs.path(*name) == "std.builtins.ActorRequestOwner"));
    assert!(output.opaque_resource_candidates.conflicts.is_empty());
}

#[test]
fn foreign_module_symbol_and_type_spoof_cannot_inherit_lifecycle() {
    let output = check_source_in_module(
        TCP_CONNECTION_SOURCE,
        vec!["user".to_string(), "net".to_string()],
    );
    assert!(output
        .opaque_resource_candidates
        .candidates
        .keys()
        .all(|name| output.defs.path(*name) == "std.builtins.ActorRequestOwner"));
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
    assert!(output
        .opaque_resource_candidates
        .candidates
        .keys()
        .all(|name| output.defs.path(*name) == "std.builtins.ActorRequestOwner"));
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
    assert!(output
        .opaque_resource_candidates
        .candidates
        .keys()
        .all(|name| output.defs.path(*name) == "std.builtins.ActorRequestOwner"));
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
    assert!(output
        .opaque_resource_candidates
        .candidates
        .keys()
        .all(|name| output.defs.path(*name) == "std.builtins.ActorRequestOwner"));
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
    assert!(output
        .opaque_resource_candidates
        .candidates
        .keys()
        .all(|name| output.defs.path(*name) == "std.builtins.ActorRequestOwner"));
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
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
    assert!(graph.candidates.is_empty());
    assert!(graph.conflicts.is_empty());
}

fn checker_with_registered_module(source: &str, module_path: &[&str]) -> Checker {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let root_id = ModulePath::root();
    let module_id = ModulePath::new(module_path.iter());
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
        expansions: hew_parser::macro_syntax::ExpansionMap::default(),
        module_graph: Some(module_graph),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.mint_module_identities(&program);
    checker.mint_source_declaration_identities(&program);
    checker.register_builtins();
    checker.collect_types(&program);
    checker.collect_declared_type_param_names(&program);
    checker.type_decls_registered = true;
    checker.collect_functions(&program);
    checker
}

fn checker_with_resolved_module_graph(sources: &[(&[&str], &str)]) -> Checker {
    let root_id = ModulePath::root();
    let module_ids: Vec<_> = sources
        .iter()
        .map(|(path, _)| ModulePath::new(path.iter()))
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
                .position(|candidate| {
                    *candidate == ModulePath::new(import_spellings(&declaration.path))
                })
                .expect("test import target must be present in the graph");
            declaration.resolved_items = Some(parsed_items[target_index].clone().into());
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
        expansions: hew_parser::macro_syntax::ExpansionMap::default(),
        module_graph: Some(module_graph),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.mint_module_identities(&program);
    checker.mint_source_declaration_identities(&program);
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
    fn close(consume self) { unsafe { example_socket_close(self) }; }
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
            fn close(consume self) { unsafe { example_socket_close(self) }; }
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
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
    let candidate = graph
        .candidates
        .get(
            &checker
                .defs
                .lookup_path("example.owner.Socket")
                .expect("declared resource"),
        )
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
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &wrong);
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
            import example.owner;
            extern "C" {
                fn example_socket_open() -> owner.Socket;
            }
            "#,
        ),
    ]);
    let contracts =
        synthetic_resource_contracts(&[("example_socket_open", "example_socket_close")]);
    let graph =
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
    let candidate = graph
        .candidates
        .get(
            &checker
                .defs
                .lookup_path("example.owner.Socket")
                .expect("declared resource"),
        )
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
            import example.owner as device;
            extern "C" {
                fn example_socket_open() -> device.Socket;
            }
            "#,
        ),
        (
            &["example", "named_alias"][..],
            r#"
            import example.owner.{ Socket as ImportedSocket };
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
            .derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
        let candidate = graph
            .candidates
            .get(&checker.defs.lookup_path("example.owner.Socket").expect("declared resource"))
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
            import example.other;
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
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
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
            import example.owner;
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
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
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
            import example.owner;
            extern "C" {
                fn example_socket_open_left() -> owner.Socket;
            }
            "#,
        ),
        (
            &["example", "right"],
            r#"
            import example.owner;
            extern "C" {
                fn example_socket_open_right() -> owner.Socket;
            }
            "#,
        ),
        (
            &["example", "noise"],
            r#"
            import example.owner;
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
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
    let matching = matching_graph
        .candidates
        .get(
            &checker
                .defs
                .lookup_path("example.owner.Socket")
                .expect("declared resource"),
        )
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
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
    assert!(!checker
        .defs
        .lookup_path("example.owner.Socket")
        .is_some_and(|id| graph.candidates.contains_key(&id)));
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
            fn close(consume self) {
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
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
    let candidate = graph
        .candidates
        .get(
            &checker
                .defs
                .lookup_path("example.io.Socket")
                .expect("declared resource"),
        )
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
            fn close(consume self) {
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
        checker.derive_opaque_resource_candidate_graph_for_contracts(checker.sigs(), &contracts);
    assert!(
        !checker
            .defs
            .lookup_path("example.io.Socket")
            .is_some_and(|id| graph.candidates.contains_key(&id)),
        "conflicting lifecycle must have no deterministic winner"
    );
    assert!(graph.conflicts.iter().any(|conflict| matches!(
        conflict.kind,
        OpaqueResourceLifecycleConflictKind::MultipleProducerLifecycle { .. }
    )));
}

// ── machine-state resource payload staging: a step stages a copy of its
// machine until it commits, and a direct `#[resource]` payload has none, so
// it is refused until a step can take an affine receiver (D530) ──────────

#[test]
fn machine_state_resource_payload_rejects() {
    let (errors, _) = parse_and_check(concat!(
        "#[resource]\n",
        "type Tok {\n    id: i64;\n}\n",
        "impl Tok { fn close(consume self) { } }\n",
        "machine Gate {\n",
        "    events { Open; Shut; }\n",
        "    state Closed;\n",
        "    state Opened { tok: Tok; }\n",
        "    on Open: Closed => .Opened { tok: Tok { id: 1 } }\n",
        "    on Shut: Opened => .Closed;\n",
        "    default { state }\n",
        "}\n",
        "fn main() { var h = Gate.Closed; h.step(.Open); }\n",
    ));
    assert!(
        errors.iter().any(|e| {
            e.kind == crate::error::TypeErrorKind::MachineExhaustivenessError
                && e.message.contains("no independent value copy")
        }),
        "a resource in a machine state payload must be rejected: {errors:?}"
    );
}

#[test]
fn machine_state_resource_payload_rejects_transitively() {
    // The veto descends inline structure: a record field wrapping the
    // resource is the same leak.
    let (errors, _) = parse_and_check(concat!(
        "#[resource]\n",
        "type Tok {\n    id: i64;\n}\n",
        "impl Tok { fn close(consume self) { } }\n",
        "type Wrap {\n    t: Tok;\n}\n",
        "machine Gate {\n",
        "    events { Open; }\n",
        "    state Closed;\n",
        "    state Opened { w: Wrap; }\n",
        "    on Open: Closed => .Opened { w: Wrap { t: Tok { id: 1 } } }\n",
        "    default { state }\n",
        "}\n",
        "fn main() { var h = Gate.Closed; h.step(.Open); }\n",
    ));
    assert!(
        errors.iter().any(|e| {
            e.kind == crate::error::TypeErrorKind::MachineExhaustivenessError
                && e.message.contains("no independent value copy")
        }),
        "a record-wrapped resource in a machine state must be rejected: {errors:?}"
    );
}

#[test]
fn machine_state_without_resource_payload_is_admitted() {
    let (errors, _) = parse_and_check(concat!(
        "machine Counter {\n",
        "    events { Inc; }\n",
        "    state Zero;\n",
        "    state NonZero { value: i64; }\n",
        "    on Inc: Zero => .NonZero { value: 1 }\n",
        "    default { state }\n",
        "}\n",
        "fn main() { var x = Counter.Zero; x.step(.Inc); }\n",
    ));
    assert!(
        errors.is_empty(),
        "a scalar machine state payload must stay admitted: {errors:?}"
    );
}

#[test]
fn machine_state_phantom_generic_resource_arg_is_admitted() {
    // `Phantom<T>` never stores `T`; a machine state holding `Phantom<Tok>`
    // carries no close obligation and must stay admitted (the raw-argument
    // walk applies only to builtin/unregistered generics).
    let (errors, _) = parse_and_check(concat!(
        "#[resource]\n",
        "type Tok {\n    id: i64;\n}\n",
        "impl Tok { fn close(consume self) { } }\n",
        "type Phantom<T> {\n    id: i64;\n}\n",
        "machine Gate {\n",
        "    events { Open; }\n",
        "    state Closed;\n",
        "    state Opened { p: Phantom<Tok>; }\n",
        "    on Open: Closed => .Opened { p: Phantom<Tok> { id: 1 } }\n",
        "    default { state }\n",
        "}\n",
        "fn main() { var h = Gate.Closed; h.step(.Open); }\n",
    ));
    assert!(
        !errors
            .iter()
            .any(|e| e.message.contains("machine `Gate` state")),
        "a phantom generic argument must not be treated as stored: {errors:?}"
    );
}

/// D528: an `#[opaque]` handle a `#[resource]` holds is released by that
/// resource's `close`, so reading it out by value anywhere else would leave
/// two owners of one handle - whether it is a direct field, sits in a plain
/// record below the resource or inside an `Option`. `close` may read it, FFI
/// calls borrow it, and the suggestion names the destructure that hands it out
/// without `close`.
#[test]
fn resource_handle_field_is_affine_outside_close() {
    let prelude = r#"#[opaque]
type Dq {
}

extern "C" {
    fn hew_deque_len(dq: Dq) -> i64;
    fn hew_deque_free(consume dq: Dq);
}

type Pair {
    a: Dq;
}

#[resource]
type Value {
    handle: Dq;
    inner: Pair;
    spare: Option<Dq>;
}
"#;
    let close = "fn close(consume self) {
        unsafe { hew_deque_free(self.handle) };
        unsafe { hew_deque_free(self.inner.a) };
        if let .Some(h) = self.spare { unsafe { hew_deque_free(h) }; }
    }";
    for (method, place, destructure) in [
        (
            "fn release(consume self) -> Dq { self.handle }",
            "self.handle",
            "let Value { handle } = self; handle",
        ),
        (
            "fn peek(self) -> Dq { self.handle }",
            "self.handle",
            "let Value { handle } = self; handle",
        ),
        (
            "fn bind(self) -> i64 { let h = self.handle; 1 }",
            "self.handle",
            "let Value { handle } = self; handle",
        ),
        (
            "fn wrap(self) -> Pair { Pair { a: self.handle } }",
            "self.handle",
            "let Value { handle } = self; handle",
        ),
        (
            "fn free(self) { unsafe { hew_deque_free(self.handle) }; }",
            "self.handle",
            "let Value { handle } = self; handle",
        ),
        (
            "fn nested(self) -> Dq { self.inner.a }",
            "self.inner.a",
            "let Value { inner } = self; inner.a",
        ),
        (
            "fn record(self) -> Pair { self.inner }",
            "self.inner",
            "let Value { inner } = self; inner",
        ),
        (
            "fn optional(self) -> Option<Dq> { self.spare }",
            "self.spare",
            "let Value { spare } = self; spare",
        ),
    ] {
        let output = check_source(&format!("{prelude} impl Value {{ {close} {method} }}"));
        let refusals: Vec<_> = output
            .errors
            .iter()
            .filter(|error| error.kind == TypeErrorKind::OwnPartialConsume)
            .collect();
        assert_eq!(refusals.len(), 1, "{method}: {:#?}", output.errors);
        assert!(
            refusals[0]
                .message
                .contains(&format!("cannot read `{place}` by value: `Value` releases"))
                && refusals[0]
                    .suggestions
                    .iter()
                    .any(|hint| hint.contains(&format!("`{destructure}`"))),
            "{method}: {:#?}",
            refusals[0]
        );
    }

    let accepted = check_source(&format!(
        "{prelude} impl Value {{
            {close}
            fn len(self) -> i64 {{ unsafe {{ hew_deque_len(self.handle) }} }}
            fn inner_len(self) -> i64 {{ unsafe {{ hew_deque_len(self.inner.a) }} }}
            fn has_spare(self) -> bool {{ self.spare.is_some() }}
            fn release(consume self) -> Dq {{
                let Value {{ handle, inner, spare }} = self;
                unsafe {{ hew_deque_free(inner.a) }};
                if let .Some(h) = spare {{ unsafe {{ hew_deque_free(h) }}; }}
                handle
            }}
        }}
        fn count(v: Value) -> i64 {{ unsafe {{ hew_deque_len(v.handle) }} }}
        fn plain(p: Pair) -> Dq {{ p.a }}"
    ));
    assert!(accepted.errors.is_empty(), "{:#?}", accepted.errors);
}

fn import_spellings(path: &hew_parser::ast::Path) -> Vec<String> {
    path.segments
        .iter()
        .map(|(segment, _)| segment.to_string())
        .collect()
}
