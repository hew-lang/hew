//! Explicit panic bodies independent of the source panic producer.

use hew_sir as sir;

pub fn module(owned: bool) -> sir::SemModule {
    let passing = if owned { "consume " } else { "" };
    let source =
        format!("fn panic_probe({passing}message: string) -> i64 {{ 0 }} fn main() -> i64 {{ panic_probe(\"probe\") }}");
    let parsed = hew_parser::parse(&source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]))
        .check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir =
        hew_hir::lower_program_host_target(&parsed.program, &checked, &hew_hir::ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let mut module = sir::lower_module(&hir.module, &checked).module;
    let function = module
        .functions
        .iter_mut()
        .find(|f| f.name == "panic_probe")
        .unwrap();
    let message = sir::Operand {
        value: function.params[0].value,
    };
    function.entry = sir::BlockId(0);
    function.blocks = vec![
        sir::SemBlock {
            id: sir::BlockId(0),
            args: vec![],
            ops: vec![],
            terminator: sir::SemTerminator::Panic {
                message: sir::BoundaryOperand {
                    operand: message.clone(),
                    decision: sir::BoundaryDecision::Borrow,
                },
                cleanup: sir::Edge {
                    target: sir::BlockId(1),
                    args: vec![],
                },
            },
        },
        sir::SemBlock {
            id: sir::BlockId(1),
            args: vec![],
            ops: if owned {
                vec![sir::SemOp {
                    id: sir::OpId(0),
                    results: vec![],
                    provenance: sir::Provenance::Synthesized,
                    kind: sir::SemOpKind::DestroyValue { value: message },
                }]
            } else {
                vec![]
            },
            terminator: sir::SemTerminator::ResumeUnwind,
        },
    ];
    if owned {
        // Match a source panic's local read loan: copy the message while the
        // loan is live, then end the loan before releasing its owned backing.
        let loan = sir::Operand {
            value: sir::ValueId(100),
        };
        function.blocks[0].ops.push(sir::SemOp {
            id: sir::OpId(1),
            kind: sir::SemOpKind::BeginBorrow {
                owner: sir::Operand {
                    value: function.params[0].value,
                },
            },
            results: vec![sir::ValueDef {
                id: loan.value,
                ty: hew_types::ResolvedTy::String,
                own: sir::OwnKind::Guaranteed,
            }],
            provenance: sir::Provenance::Synthesized,
        });
        let sir::SemTerminator::Panic { message, .. } = &mut function.blocks[0].terminator else {
            unreachable!()
        };
        message.operand = loan.clone();
        function.blocks[1].ops.insert(
            0,
            sir::SemOp {
                id: sir::OpId(2),
                kind: sir::SemOpKind::EndBorrow { borrow: loan },
                results: vec![],
                provenance: sir::Provenance::Synthesized,
            },
        );
    }
    let diagnostics = sir::verify_module(&module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    module
}
