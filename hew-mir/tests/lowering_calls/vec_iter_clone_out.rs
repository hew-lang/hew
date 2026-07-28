//! `VecIter`'s iterator-only clone-out contract.
//!
//! A nested collection loaded by ordinary `xs[i]` remains a receiver-interior
//! alias (`hew_vec_get_owned`). The same element yielded by `VecIter::next`
//! must instead be an independent owner (`hew_vec_get_clone -> Option<T>`) so
//! the body and the cursor snapshot can be released independently.

use hew_hir::{lower_program, IntentKind, ResolutionCtx};
use hew_mir::{
    lower_hir_module, CmpPred, DropFnSpec, InPlaceReleaseKind, Instr, IrPipeline, MirStatement,
    Place, Terminator,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    let mir = lower_hir_module(&hir.module);
    assert!(
        mir.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        mir.diagnostics
    );
    mir
}

fn pipeline_allowing_mir_diagnostics(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

#[test]
fn affine_vec_iter_snapshot_is_rejected_before_runtime_clone() {
    let pipeline = pipeline_allowing_mir_diagnostics(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(file: File) { } }
        type Holder { file: File }

        fn main() {
            let file = File { fd: 7 };
            let holder = Holder { file: file };
            let values: Vec<Holder> = [holder];
            let _cursor = values.iter();
        }
        ",
    );
    assert!(
        pipeline.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct == "`Vec<Holder>` clone"
        ) && diagnostic
            .note
            .contains("resource `File` has an affine close contract")),
        "Vec::iter must not synthesize an affine Vec clone: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn affine_vec_index_remains_borrow_only() {
    let pipeline = pipeline(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(file: File) { } }
        type Holder { file: File }

        fn main() -> i64 {
            let file = File { fd: 7 };
            let holder = Holder { file: file };
            let values: Vec<Holder> = [holder];
            let first = values[0];
            first.file.fd
        }
        ",
    );
    let calls = call_symbols(&pipeline, "main");
    assert!(
        calls.contains(&"hew_vec_get_ptr"),
        "resource-containing Vec indexing must remain a borrow: {calls:?}"
    );
    assert!(
        !calls.contains(&"hew_vec_get_clone"),
        "resource-containing Vec indexing must not clone out an owner: {calls:?}"
    );
}

#[test]
fn affine_vec_get_stays_on_clone_out_guard() {
    let pipeline = pipeline_allowing_mir_diagnostics(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(file: File) { } }
        type Holder { file: File }

        fn main() {
            let file = File { fd: 7 };
            let holder = Holder { file: file };
            let values: Vec<Holder> = [holder];
            let _value = values.get(0);
        }
        ",
    );
    assert!(
        pipeline.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct == "`VecIter<Holder>` clone-out"
        ) && diagnostic
            .note
            .contains("resource `File` has an affine close contract")),
        "Vec::get must retain its element clone-out guard: {:#?}",
        pipeline.diagnostics
    );
}

fn call_symbols<'a>(pipeline: &'a IrPipeline, function: &str) -> Vec<&'a str> {
    let function = pipeline
        .raw_mir
        .iter()
        .find(|candidate| candidate.name == function)
        .unwrap_or_else(|| panic!("missing raw MIR for `{function}`"));
    function
        .blocks
        .iter()
        .flat_map(|block| {
            block
                .instructions
                .iter()
                .filter_map(|instr| match instr {
                    Instr::CallRuntimeAbi(call) => Some(call.symbol()),
                    _ => None,
                })
                .chain(match &block.terminator {
                    Terminator::Call { callee, .. } => Some(callee.as_str()),
                    _ => None,
                })
        })
        .collect()
}

fn raw_instructions<'a>(pipeline: &'a IrPipeline, function: &str) -> Vec<&'a Instr> {
    pipeline
        .raw_mir
        .iter()
        .find(|candidate| candidate.name == function)
        .unwrap_or_else(|| panic!("missing raw MIR for `{function}`"))
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .collect()
}

fn vec_iter_release_guard_flags(
    pipeline: &IrPipeline,
    function: &str,
) -> std::collections::HashSet<Place> {
    let function = pipeline
        .raw_mir
        .iter()
        .find(|candidate| candidate.name == function)
        .unwrap_or_else(|| panic!("missing raw MIR for `{function}`"));
    let release_blocks: std::collections::HashSet<_> = function
        .blocks
        .iter()
        .filter(|block| {
            block.instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::RecordFieldDrop {
                        field_offset: hew_mir::FieldOffset(0),
                        ..
                    }
                )
            })
        })
        .map(|block| block.id)
        .collect();
    function
        .blocks
        .iter()
        .filter_map(|block| match block.terminator {
            Terminator::Branch {
                cond, then_target, ..
            } if release_blocks.contains(&then_target) => {
                block.instructions.iter().find_map(|instr| match instr {
                    Instr::IntCmp {
                        dest,
                        pred: CmpPred::Eq,
                        lhs,
                        ..
                    } if *dest == cond => Some(*lhs),
                    _ => None,
                })
            }
            _ => None,
        })
        .collect()
}

#[test]
fn nested_vec_iteration_clones_out_while_ordinary_indexing_stays_borrowed() {
    let pipeline = pipeline(
        r"
        fn index_row(rows: Vec<Vec<i64>>) -> i64 {
            rows[0].len()
        }

        fn iterate_rows(rows: Vec<Vec<i64>>) -> i64 {
            var total: i64 = 0;
            for row in rows.iter() {
                total = total + row.len();
            }
            total
        }
        ",
    );

    let index_calls = call_symbols(&pipeline, "index_row");
    assert!(
        index_calls.contains(&"hew_vec_get_owned"),
        "ordinary nested-Vec indexing must preserve its borrowing getter: {index_calls:?}"
    );
    assert!(
        !index_calls.contains(&"hew_vec_get_clone"),
        "ordinary nested-Vec indexing must not inherit VecIter clone-out: {index_calls:?}"
    );

    let iter_calls = call_symbols(&pipeline, "iterate_rows");
    assert!(
        iter_calls.contains(&"hew_vec_get_clone"),
        "VecIter::next must clone every nested-Vec yield into Option<T>: {iter_calls:?}"
    );
    assert!(
        !iter_calls.contains(&"hew_vec_get_owned"),
        "VecIter::next must never borrow a nested collection from its snapshot: {iter_calls:?}"
    );

    let synthetic_value_uses: Vec<_> = pipeline
        .thir
        .iter()
        .find(|candidate| candidate.name == "iterate_rows")
        .expect("missing THIR for `iterate_rows`")
        .statements
        .iter()
        .filter_map(|statement| match statement {
            MirStatement::Use { name, intent, .. } if name.starts_with("__hew_iter_value_") => {
                Some(*intent)
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        synthetic_value_uses,
        [IntentKind::Consume],
        "the clone-out Option<T> intermediate must transfer into the match scrutinee; \
         leaving it as a Read schedules an early payload drop"
    );
}

#[test]
fn admitted_rc_weak_and_tuple_yields_have_total_release_paths() {
    let pipeline = pipeline(
        r"
        fn rc_values(values: Vec<Rc<i64>>) {
            for value in values.iter() {
                let _ = value.get();
            }
        }

        fn weak_values(values: Vec<Weak<i64>>) {
            for value in values.iter() {
                let _ = value;
            }
        }

        fn tuple_values(values: Vec<(string, string)>) {
            for value in values.iter() {
                let _ = value.0.len();
            }
        }
        ",
    );

    let rc = raw_instructions(&pipeline, "rc_values");
    assert!(
        rc.iter().any(|instr| matches!(
            instr,
            Instr::Drop {
                drop_fn: Some(DropFnSpec::Release("hew_rc_drop")),
                ..
            }
        )),
        "an admitted Rc yield must release its retained owner: {rc:#?}"
    );

    let weak = raw_instructions(&pipeline, "weak_values");
    assert!(
        weak.iter().any(|instr| matches!(
            instr,
            Instr::Drop {
                drop_fn: Some(DropFnSpec::Release("hew_weak_drop_rc")),
                ..
            }
        )),
        "an admitted Weak yield must release its retained owner: {weak:#?}"
    );

    let tuple = raw_instructions(&pipeline, "tuple_values");
    assert!(
        tuple.iter().any(|instr| matches!(
            instr,
            Instr::Drop {
                drop_fn: Some(DropFnSpec::InPlace(InPlaceReleaseKind::AggregateRecursive)),
                ..
            }
        )),
        "an admitted tuple yield must recursively release its owned fields: {tuple:#?}"
    );
}

#[test]
fn first_class_vec_iter_owners_drop_at_scope_exit_but_return_transfers() {
    let pipeline = pipeline(
        r"
        fn manual(values: Vec<Rc<i64>>) {
            var cursor = values.iter();
            let _ = cursor.next();
        }

        fn returned(values: Vec<Rc<i64>>) -> VecIter<Rc<i64>> {
            let cursor = values.iter();
            cursor
        }

        fn rebound(values: Vec<Rc<i64>>) {
            let cursor = values.iter();
            var next_cursor = cursor;
            let _ = next_cursor.next();
        }
        ",
    );

    let manual = raw_instructions(&pipeline, "manual");
    assert!(
        manual
            .iter()
            .any(|instr| matches!(instr, Instr::RecordFieldDrop { .. })),
        "a root-scope manual cursor must release its owned Vec snapshot: {manual:#?}"
    );
    assert!(
        manual.iter().any(|instr| matches!(
            instr,
            Instr::Drop {
                drop_fn: Some(DropFnSpec::InPlace(InPlaceReleaseKind::Enum)),
                ..
            }
        )),
        "an ignored next() Option must recursively release its payload immediately: {manual:#?}"
    );

    let returned = raw_instructions(&pipeline, "returned");
    assert!(
        !returned
            .iter()
            .any(|instr| matches!(instr, Instr::RecordFieldDrop { .. })),
        "returning the cursor transfers its snapshot instead of freeing it: {returned:#?}"
    );

    let rebound = raw_instructions(&pipeline, "rebound");
    assert!(
        rebound
            .iter()
            .any(|instr| matches!(instr, Instr::RecordFieldDrop { .. })),
        "whole-value cursor rebinding must transfer the scope owner: {rebound:#?}"
    );
}

#[test]
fn cursor_assignment_drops_are_runtime_guarded_and_absent_from_exit_plans() {
    let pipeline = pipeline(
        r"
        fn reassign(first_values: Vec<Rc<i64>>, second_values: Vec<Rc<i64>>) {
            var first = first_values.iter();
            var second = second_values.iter();
            first = second;
        }
        ",
    );
    let function = pipeline
        .raw_mir
        .iter()
        .find(|candidate| candidate.name == "reassign")
        .expect("missing raw MIR for `reassign`");

    let release_blocks: Vec<_> = function
        .blocks
        .iter()
        .filter(|block| {
            block.instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::RecordFieldDrop {
                        field_offset: hew_mir::FieldOffset(0),
                        ..
                    }
                )
            })
        })
        .collect();
    assert_eq!(
        release_blocks.len(),
        3,
        "assignment must guard the old-destination release plus both mutually \
         exclusive final binding slots: {:#?}",
        function.blocks
    );

    for release in release_blocks {
        let guards: Vec<_> = function
            .blocks
            .iter()
            .filter_map(|predecessor| match predecessor.terminator {
                Terminator::Branch {
                    cond, then_target, ..
                } if then_target == release.id => Some((predecessor, cond)),
                _ => None,
            })
            .collect();
        assert_eq!(
            guards.len(),
            1,
            "every cursor-field release must be reachable only through one ownership-bit guard"
        );
        let (guard_block, cond) = guards[0];
        assert!(
            guard_block.instructions.iter().any(|instr| matches!(
                instr,
                Instr::IntCmp {
                    dest,
                    pred: CmpPred::Eq,
                    lhs: Place::Local(_),
                    rhs: Place::Local(_),
                } if *dest == cond
            )),
            "release bb{} is not controlled by `owner_flag == 0`: {guard_block:#?}",
            release.id
        );
    }

    let elaborated = pipeline
        .elaborated_mir
        .iter()
        .find(|candidate| candidate.name == "reassign")
        .expect("missing elaborated MIR for `reassign`");
    assert!(
        elaborated
            .drop_plans
            .iter()
            .all(|(_, plan)| plan.drops.iter().all(|drop| !matches!(
                &drop.ty,
                ResolvedTy::Named { name, .. }
                    if name.rsplit('.').next() == Some("VecIter")
            ))),
        "cursor ownership is discharged exactly by guarded inline field drops; \
         no unconditional VecIter exit-plan drop may compete: {:#?}",
        elaborated.drop_plans
    );
}

#[test]
fn cursor_call_carrier_transfer_rearms_the_local_destination_owner() {
    let pipeline = pipeline(
        r"
        fn consume_cursor(cursor: VecIter<Rc<i64>>) {
            let moved = cursor;
            let _ = moved;
        }
        ",
    );
    let guard_flags = vec_iter_release_guard_flags(&pipeline, "consume_cursor");
    assert_eq!(
        guard_flags.len(),
        2,
        "the moved local and its discarded read must each have a guarded VecIter release"
    );
    let instructions = raw_instructions(&pipeline, "consume_cursor");
    assert!(
        guard_flags.iter().any(|guard_flag| {
            instructions.iter().any(|instr| match instr {
                Instr::Move { dest, src } if dest == guard_flag => {
                    instructions.iter().any(|candidate| {
                        matches!(
                            candidate,
                            Instr::ConstI64 {
                                dest,
                                value: 0
                            } if dest == src
                        )
                    })
                }
                _ => false,
            })
        }),
        "moving and neutralizing an owned call-carrier parameter must re-arm \
         the destination sidecar as owned: {instructions:#?}"
    );
}

#[test]
fn non_owning_composite_and_closure_reads_do_not_consume_cursor_bindings() {
    let pipeline = pipeline(
        r"
        fn discarded_block(values: Vec<Rc<i64>>) {
            var cursor = values.iter();
            { cursor };
        }

        fn discarded_if(values: Vec<Rc<i64>>, take: bool) {
            var first = values.iter();
            var second = values.iter();
            let _ = if take { first } else { second };
        }

        fn discarded_match(values: Vec<Rc<i64>>, choice: i64) {
            var first = values.iter();
            var second = values.iter();
            let _ = match choice {
                0 => first,
                _ => second,
            };
        }

        fn closure_read(values: Vec<Rc<i64>>) {
            var cursor = values.iter();
            let inspect = |incoming: VecIter<Rc<i64>>| {
                let _ = incoming;
            };
            inspect(cursor);
            let _ = cursor.next();
        }
        ",
    );

    for (function, expected_guards) in [
        ("discarded_block", 2),
        ("discarded_if", 3),
        ("discarded_match", 3),
        ("closure_read", 2),
    ] {
        let guard_flags = vec_iter_release_guard_flags(&pipeline, function);
        assert_eq!(
            guard_flags.len(),
            expected_guards,
            "`{function}` must retain every source-cursor release plus one \
             runtime-gated discarded composite result where applicable: {:#?}",
            raw_instructions(&pipeline, function)
        );
    }
}
