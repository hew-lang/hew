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
