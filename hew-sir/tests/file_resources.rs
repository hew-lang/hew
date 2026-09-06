//! Exact resource metadata and ordinary owner-flow verification.
use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_parser::{
    ast::Item,
    module::{Module, ModuleGraph, ModuleId},
};
use hew_sir::{ResourceRelease, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker, CloneKind, DefId};

fn resource_module() -> hew_sir::SemModule {
    let source = r#"
        #[resource] #[opaque] pub type FileReadStream {}
        impl FileReadStream { fn close(consuming self) { unsafe { hew_file_read_stream_close(self) }; } }
        extern "C" {
            fn hew_file_read_stream_open(path: string) -> FileReadStream;
            fn hew_file_read_stream_is_valid(file: FileReadStream) -> bool;
            fn hew_file_read_stream_close(consume file: FileReadStream);
        }
        pub fn probe() -> i64 {
            let file = unsafe { hew_file_read_stream_open("known") };
            let valid = unsafe { hew_file_read_stream_is_valid(file) };
            unsafe { hew_file_read_stream_close(file); }
            if valid { 0 } else { 1 }
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut imported = hew_parser::parse("import std.fs; fn main() -> i64 { fs.probe() }");
    assert!(imported.errors.is_empty(), "{:?}", imported.errors);
    let Item::Import(import) = &mut imported.program.items[0].0 else {
        panic!("encoding import fixture")
    };
    import.resolved_items = Some(parsed.program.items.clone());
    import.resolved_source_paths = vec![std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std/fs.hew")];
    let source_paths = import.resolved_source_paths.clone();
    let root = ModuleId::root();
    let encoding = ModuleId::new(vec!["std".into(), "fs".into()]);
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
fn resource_release_rejects_missing_identity_signature_discharge_and_copy_authority() {
    let original = resource_module();
    assert_eq!(original.resources.len(), 1);
    for mutation in 0..7 {
        let mut module = original.clone();
        if mutation == 0 {
            module.resources.clear();
        } else if mutation == 1 {
            let ty = module.resources.keys().next().unwrap().clone();
            module
                .type_facts
                .get_mut(&hew_types::TypeInstanceKey(ty))
                .unwrap()
                .clone = CloneKind::Bits;
        } else {
            let ResourceRelease::Nominal {
                lifecycle,
                release,
                producers,
            } = module.resources.values_mut().next().unwrap()
            else {
                panic!("nominal authority");
            };
            match mutation {
                2 => lifecycle.release_declaration = DefId::for_test("impostor.release"),
                3 => release.consumes[0] = false,
                4 => {
                    lifecycle.discharge_depth =
                        hew_types::ffi_contracts::ReleaseDischargeDepth::None;
                }
                5 => producers.clear(),
                6 => lifecycle.resource_declaration = DefId::for_test("impostor.FileReadStream"),
                _ => unreachable!(),
            }
        }
        let diagnostics = hew_sir::check_module(&module).unwrap_err();
        assert!(
            diagnostics.iter().any(|diagnostic| matches!(
                diagnostic.kind,
                hew_sir::SirDiagnosticKind::InvalidResourceType { .. }
            )),
            "{mutation}: {diagnostics:?}"
        );
    }
}

#[test]
fn resource_consume_cannot_be_changed_into_a_borrow() {
    let mut module = resource_module();
    let call =
        module
            .functions
            .iter_mut()
            .flat_map(|function| &mut function.blocks)
            .find_map(|block| match &mut block.terminator {
                hew_sir::SemTerminator::RtCall {
                    family:
                        hew_types::RuntimeCallFamily::FileRead(
                            hew_types::runtime_call::FileReadOp::Close,
                        ),
                    args,
                    ..
                } => Some(args),
                _ => None,
            })
            .unwrap();
    call[0].decision = hew_sir::BoundaryDecision::Borrow;
    assert!(hew_sir::check_module(&module).is_err());
}

#[test]
fn resource_release_cannot_take_an_owner_while_its_read_loan_is_active() {
    let mut module = resource_module();
    let mut removed = 0;
    for function in &mut module.functions {
        let loans = function
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .filter(|operation| matches!(operation.kind, hew_sir::SemOpKind::LoadBorrow { .. }))
            .flat_map(|operation| &operation.results)
            .filter(|value| hew_types::runtime_call::FileReadHandleKind::Nominal.matches(&value.ty))
            .map(|value| value.id)
            .collect::<std::collections::BTreeSet<_>>();
        for block in &mut function.blocks {
            block.ops.retain(|operation| {
                let remove = matches!(&operation.kind, hew_sir::SemOpKind::EndBorrow { borrow } if loans.contains(&borrow.value));
                removed += usize::from(remove);
                !remove
            });
        }
    }
    assert_eq!(removed, 1, "the validity call has one exact owner loan");
    let diagnostics = hew_sir::check_module(&module).unwrap_err();
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            hew_sir::SirDiagnosticKind::PlaceLifetime {
                reason: "value cannot be consumed or ended while a dependent borrow is live",
                ..
            }
        )),
        "{diagnostics:?}"
    );
}
