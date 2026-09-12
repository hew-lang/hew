//! Encoding runtime transfers use the ordinary owner and Local protocols.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_parser::{
    ast::Item,
    module::{Module, ModuleGraph, ModuleId},
};
use hew_sir::{BoundaryDecision, SemOpKind, SemTerminator, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker, EncodingOp, RuntimeCallFamily};

fn lower_encoding(format: &str, body: &str) -> hew_sir::SemModule {
    let source = format!(
        r#"
        #[opaque] pub type Value {{}}
        extern "C" {{
            fn hew_{format}_array_new() -> Value;
            fn hew_{format}_array_push(parent: Value, consume child: Value);
            fn hew_{format}_free(consume value: Value);
        }}
        pub fn probe() -> i64 {{ {body} }}
    "#
    );
    let parsed = hew_parser::parse(&source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut imported = hew_parser::parse(&format!(
        "import std.encoding.{format}; fn main() -> i64 {{ {format}.probe() }}"
    ));
    assert!(imported.errors.is_empty(), "{:?}", imported.errors);
    let Item::Import(import) = &mut imported.program.items[0].0 else {
        panic!("encoding import fixture")
    };
    import.resolved_items = Some(parsed.program.items.clone().into());
    import.resolved_source_paths = vec![std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join(format!("std/encoding/{format}/{format}.hew"))];
    let source_paths = import.resolved_source_paths.clone();
    let root = ModuleId::root();
    let encoding = ModuleId::new(vec!["std".into(), "encoding".into(), format.into()]);
    let mut graph = ModuleGraph::new(root.clone());
    graph
        .add_module(Module {
            id: encoding.clone(),
            items: parsed.program.items,
            imports: vec![],
            source_paths,
            doc: None,
        })
        .unwrap();
    graph
        .add_module(Module {
            id: root.clone(),
            items: imported.program.items.clone(),
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .unwrap();
    graph.topo_order = vec![encoding, root];
    let mut program = imported.program;
    program.module_graph = Some(graph);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = hew_sir::lower_module(&hir.module, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    hew_sir::check_module(&lowered.module).unwrap();
    lowered.module
}

#[test]
fn insertion_transfers_both_owners_and_free_consumes_an_immutable_binding() {
    for format in ["json", "yaml"] {
        let module = lower_encoding(
            format,
            &format!(
                r"
                var parent = unsafe {{ hew_{format}_array_new() }};
                let child = unsafe {{ hew_{format}_array_new() }};
                unsafe {{ hew_{format}_array_push(parent, child); }}
                let disposable = unsafe {{ hew_{format}_array_new() }};
                unsafe {{ hew_{format}_free(disposable); }}
                0
            "
            ),
        );
        let main = module
            .functions
            .iter()
            .find(|function| function.name.ends_with("probe"))
            .unwrap();
        let mut transfers = Vec::new();
        for block in &main.blocks {
            if let SemTerminator::RtCall {
                family:
                    RuntimeCallFamily::Encoding {
                        op: op @ (EncodingOp::ArrayPush | EncodingOp::Free),
                        ..
                    },
                args,
                ..
            } = &block.terminator
            {
                assert!(args
                    .iter()
                    .all(|arg| arg.decision == BoundaryDecision::Move));
                transfers.push((*op, args.len()));
            }
        }
        assert_eq!(
            transfers,
            [(EncodingOp::ArrayPush, 2), (EncodingOp::Free, 1)]
        );
        assert!(main
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .any(|op| matches!(op.kind, SemOpKind::StoreAssign { .. })));
    }
}
