//! `VecIter`'s iterator-only clone-out contract.
//!
//! A nested collection loaded by ordinary `xs[i]` remains a receiver-interior
//! alias (`hew_vec_get_owned`). The same element yielded by `VecIter::next`
//! must instead be an independent owner (`hew_vec_get_clone -> Option<T>`) so
//! the body and the cursor snapshot can be released independently.

use hew_hir::{lower_program, IntentKind, ResolutionCtx};
use hew_mir::model::NeutralizeAuthority;
use hew_mir::{
    lower_hir_module, CmpPred, CowHeapRelease, DropFnSpec, DropKind, ExitPath, InPlaceReleaseKind,
    Instr, IrPipeline, MirStatement, Place, RawMirFunction, Terminator,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

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
    let parsed = hew_parser::parse(
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
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.iter().any(|error| {
            error.message.contains("`VecIter<Holder>` is not supported")
                && error
                    .message
                    .contains("resource/linear value `File` has no semantic clone/retain operation")
        }),
        "Vec::iter must reject affine element clone-out before HIR/MIR: {:#?}",
        tc_output.errors
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
    // The borrow contract is the invariant; the getter SYMBOL follows the
    // construction ABI. A close-obligated element now builds through the owned
    // descriptor (so the Vec's scope-exit free runs each element's `close`
    // exactly once — under the previous plain-Vec classification this Vec
    // leaked its elements), and the congruent slot borrow under that ABI is
    // `hew_vec_get_owned` (full-stride load, dest never drop-scheduled).
    // `hew_vec_get_clone` staying absent is the load-bearing half: a cloned-out
    // owner would mint a second close authority over one context.
    assert!(
        calls.contains(&"hew_vec_get_owned"),
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
                if construct == "drop-only `Vec` element operation `get`"
        ) && diagnostic
            .note
            .contains("drop callback but no semantic clone")),
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

fn assert_cursor_assignment_owner_handoff(function: &RawMirFunction) {
    let cursor_handoffs: Vec<_> = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(hew_mir::OwnershipEvent::Transfer {
                owner,
                from,
                to: Some(to),
                to_owner: Some(successor),
                to_ty: Some(ty),
            }) if owner.binding != successor.binding
                && matches!(
                    ty,
                    hew_types::ResolvedTy::Named {
                        builtin: Some(hew_types::BuiltinType::VecIter),
                        ..
                    }
                ) =>
            {
                Some((*owner, *from, *to, *successor))
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        cursor_handoffs.len(),
        1,
        "cursor reassignment must transfer the source owner into exactly one next \
         destination generation: {:#?}",
        function.blocks
    );
    let (source_owner, source, destination, _) = cursor_handoffs[0];
    assert!(
        !function
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(hew_mir::OwnershipEvent::Relocate {
                    owner,
                    from,
                    to,
                }) if *owner == source_owner && *from == source && *to == destination
            )),
        "the transferred source generation must not survive as a relocation: {:#?}",
        function.blocks
    );
}

#[test]
fn for_vec_capture_keeps_source_carrier_release_authority() {
    let pipeline = pipeline(
        r"
        fn drain(values: Vec<i64>) {
            for value in values {
                let _ = value;
            }
        }

        type Holder { values: Vec<i64> }

        fn wrap(values: Vec<i64>) -> Holder {
            Holder { values: values }
        }
        ",
    );

    let drain = raw_instructions(&pipeline, "drain");
    assert!(
        drain.iter().all(|instr| !matches!(
            instr,
            Instr::NeutralizePayloadSlot {
                place: Place::Local(0),
                authority: NeutralizeAuthority::WholeCarrierConsume,
                ..
            }
        )),
        "the Capture source borrowed by `VecIter.vec` must keep its carrier \
         release authority: {drain:#?}"
    );
    let source_guards: std::collections::HashSet<_> = drain
        .iter()
        .filter_map(|instr| match instr {
            Instr::ValueSnapshotDrop {
                value: Place::Local(0),
                guard: Some(guard),
                ..
            } => Some(*guard),
            _ => None,
        })
        .collect();
    assert_eq!(
        source_guards.len(),
        1,
        "all exits must share the source Vec's one carrier guard"
    );
    let source_guard = *source_guards.iter().next().expect("source guard");
    assert!(
        drain.iter().any(|instr| matches!(
            instr,
            Instr::ConstI64 {
                dest,
                value: 0
            } if *dest == source_guard
        )) && drain.iter().all(|instr| !matches!(
            instr,
            Instr::ConstI64 {
                dest,
                value: 1
            } if *dest == source_guard
        )),
        "the live source owner must stay armed on every loop path: {drain:#?}"
    );

    let wrap = raw_instructions(&pipeline, "wrap");
    assert_eq!(
        wrap.iter()
            .filter(|instr| matches!(
                instr,
                Instr::NeutralizePayloadSlot {
                    place: Place::Local(0),
                    transferee: Some(_),
                    authority: NeutralizeAuthority::WholeCarrierConsume,
                }
            ))
            .count(),
        1,
        "ordinary aggregate ingress remains a real transfer: the source carrier \
         must be neutralized exactly once in favour of the Holder owner"
    );
}

#[test]
fn borrowed_for_source_cannot_move_or_reassign_until_cursor_scope_closes() {
    let pipeline = pipeline_allowing_mir_diagnostics(
        r"
        fn make() -> Vec<i64> {
            [1, 2, 3]
        }

        fn take(values: Vec<i64>) {
            let _ = values.len();
        }

        fn overwrite() {
            var values = make();
            for value in values {
                values = make();
                print(value);
            }
        }

        fn consume() {
            let values = make();
            for value in values {
                let _moved = values;
                print(value);
            }
        }

        fn after_loop() {
            let values = make();
            for value in values {
                print(value);
                print(values.len());
            }
            take(values);
        }
        ",
    );

    let constructs: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter_map(|diagnostic| match &diagnostic.kind {
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("while a VecIter cursor borrows it") =>
            {
                Some(construct.as_str())
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        constructs.len(),
        2,
        "reassignment and whole-value movement must reject while the borrowed \
         cursor is active, while reads and a post-loop move stay valid: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        constructs
            .iter()
            .any(|construct| construct.starts_with("reassigning `values`"))
            && constructs
                .iter()
                .any(|construct| construct.starts_with("moving `values`")),
        "the two invalid ownership boundaries need distinct actionable diagnostics: \
         {constructs:#?}"
    );
}

#[test]
fn borrowed_projection_prefix_stores_reject_while_cursor_active() {
    let pipeline = pipeline_allowing_mir_diagnostics(
        r"
        type Holder {
            items: Vec<i64>,
        }

        type Inner {
            items: Vec<i64>,
        }

        type Outer {
            inner: Inner,
            spare: Vec<i64>,
        }

        fn make() -> Vec<i64> {
            [1, 2, 3]
        }

        fn direct_store() {
            var holder = Holder { items: make() };
            for value in holder.items {
                holder.items = make();
                print(value);
            }
        }

        fn nested_store() {
            var outer = Outer { inner: Inner { items: make() }, spare: make() };
            for value in outer.inner.items {
                outer.inner.items = make();
                print(value);
            }
        }

        fn prefix_store() {
            var outer = Outer { inner: Inner { items: make() }, spare: make() };
            for value in outer.inner.items {
                outer.inner = Inner { items: make() };
                print(value);
            }
        }

        fn sibling_store_stays_valid() {
            var outer = Outer { inner: Inner { items: make() }, spare: make() };
            for value in outer.inner.items {
                outer.spare = make();
                print(value);
            }
        }
        ",
    );

    let constructs: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter_map(|diagnostic| match &diagnostic.kind {
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("while a VecIter cursor borrows it") =>
            {
                Some(construct.as_str())
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        constructs.len(),
        3,
        "the projection itself, its full nested path, and every prefix must \
         reject while the borrowed field cursor is active, while a disjoint \
         sibling field store stays valid: {:#?}",
        pipeline.diagnostics
    );
    for expected in [
        "assigning `holder.items`",
        "assigning `outer.inner.items`",
        "assigning `outer.inner`",
    ] {
        assert!(
            constructs
                .iter()
                .any(|construct| construct.starts_with(expected)),
            "missing rejection for {expected}: {constructs:#?}"
        );
    }
}

#[test]
fn vec_iter_yield_cancel_cleanup_is_path_exact_or_rejected() {
    let pipeline = pipeline_allowing_mir_diagnostics(
        r"
        fn branch_selective(values: Vec<Vec<i64>>, ticks: Vec<i64>, move_value: bool) {
            for value in values {
                if move_value {
                    let _moved = value;
                } else {
                    for tick in ticks {
                        print(tick);
                    }
                }

                for tick in ticks {
                    print(tick);
                }
            }
        }
        ",
    );

    assert!(
        pipeline.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct
                    == "conditionally moved VecIter yield across an abandonment point"
        ) && diagnostic
            .note
            .contains("omitting the release would leak the live path")),
        "a joined cancellation point with MaybeConsumed payload authority must fail \
         closed instead of leaking one predecessor or double-freeing the other: {:#?}",
        pipeline.diagnostics
    );

    let function = pipeline
        .elaborated_mir
        .iter()
        .find(|candidate| candidate.name == "branch_selective")
        .expect("missing elaborated MIR for `branch_selective`");
    let cancel_payload_drops: Vec<_> = function
        .drop_plans
        .iter()
        .filter_map(|(exit, plan)| match exit {
            ExitPath::Cancel { block } => {
                let count = plan
                    .drops
                    .iter()
                    .filter(|drop| {
                        drop.drop_fn.is_none()
                            && drop.kind
                                == DropKind::CowHeap {
                                    release: CowHeapRelease::VecPlain,
                                }
                    })
                    .count();
                (count != 0).then_some((*block, count))
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        cancel_payload_drops.len(),
        5,
        "each cancellation point in the else-only inner loop has definite Live \
         authority and must release the payload; the later joined MaybeConsumed \
         loop must receive no unconditional payload drop: {:#?}",
        function.drop_plans
    );
    assert!(
        cancel_payload_drops.iter().all(|(_, count)| *count == 1),
        "every definite-live cancellation edge owns exactly one payload release: \
         {cancel_payload_drops:#?}"
    );
}

#[test]
fn vec_iter_guarded_release_retires_at_shared_loop_continuation() {
    let pipeline = pipeline(
        r"
        fn nested_cursor_reentry(values: Vec<Vec<i64>>, ticks: Vec<i64>) {
            for value in values {
                print(value.len());
                for tick in ticks {
                    print(tick);
                }
            }
        }
        ",
    );
    let function = pipeline
        .raw_mir
        .iter()
        .find(|candidate| candidate.name == "nested_cursor_reentry")
        .expect("missing raw MIR for `nested_cursor_reentry`");
    let cursor_owners = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(hew_mir::OwnershipEvent::Guard {
                owner,
                kind: hew_mir::OwnershipGuardKind::VecIter,
                ..
            }) => Some(*owner),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert!(
        cursor_owners.len() >= 2,
        "fixture must include the outer yield cursor and re-entered inner cursor"
    );

    for owner in cursor_owners {
        let releases = function
            .blocks
            .iter()
            .filter(|block| {
                block.instructions.iter().any(|instruction| {
                    matches!(
                        instruction,
                        Instr::OwnershipEvent(hew_mir::OwnershipEvent::GuardedRelease {
                            owner: released,
                            ..
                        }) if *released == owner
                    )
                })
            })
            .collect::<Vec<_>>();
        assert_eq!(
            releases.len(),
            1,
            "each cursor generation must have one logical guarded release: {owner:?}"
        );
        let continuation = releases[0];
        let predecessor_count = function
            .blocks
            .iter()
            .filter(|block| block.successors().contains(&continuation.id))
            .count();
        assert_eq!(
            predecessor_count, 2,
            "logical release must live after both physical-release and flag-skip paths: \
             {owner:?} in bb{}",
            continuation.id
        );
        assert!(
            continuation
                .instructions
                .iter()
                .all(|instruction| !matches!(instruction, Instr::RecordFieldDrop { .. })),
            "the shared continuation carries logical authority only; the physical cursor drop \
             remains on the guarded branch: {owner:?} in bb{}",
            continuation.id
        );
    }
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
fn cursor_assignment_drops_are_runtime_guarded_and_exit_plans_are_abandon_only() {
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

    assert_cursor_assignment_owner_handoff(function);

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
    for (exit, plan) in &elaborated.drop_plans {
        for drop in &plan.drops {
            if !matches!(drop.kind, DropKind::VecIterCursor { .. }) {
                continue;
            }
            assert!(
                matches!(
                    exit,
                    ExitPath::Unwind { .. }
                        | ExitPath::Cancel { .. }
                        | ExitPath::Panic { .. }
                        | ExitPath::Yield { .. }
                        | ExitPath::Suspend { .. }
                        | ExitPath::Return { .. }
                ),
                "a cursor field release may enter a drop plan only on an \
                 abandonment edge or its guarded lexical return, never internal \
                 normal flow: {exit:?} -> {drop:?}"
            );
            assert!(
                drop.guard.is_some(),
                "every abandonment cursor release must share the binding's \
                 path-sensitive ownership sidecar: {exit:?} -> {drop:?}"
            );
        }
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "the test audits guarded cursor authority across four distinct exit classes"
)]
fn first_class_cursor_abandonment_plans_cover_cancel_panic_suspend_and_yield() {
    let pipeline = pipeline(
        r"
        fn cancel_cursor(values: Vec<i64>) {
            var cursor = values.iter();
            var i: i64 = 0;
            while i < 2 {
                i = i + 1;
            }
            let _ = cursor.next();
        }

        fn panic_cursor(values: Vec<i64>, index: i64) {
            var cursor = values.iter();
            let _ = values[index];
            let _ = cursor.next();
        }

        gen fn yield_cursor(values: Vec<i64>) -> i64 {
            var cursor = values.iter();
            yield 1;
            let _ = cursor.next();
        }

        actor Sleeper {
            receive fn park() {
                let values: Vec<i64> = Vec.new();
                values.push(1);
                var cursor = values.iter();
                sleep(10s);
                let _ = cursor.next();
            }
        }
        ",
    );

    let cursor_drops: Vec<_> = pipeline
        .elaborated_mir
        .iter()
        .flat_map(|function| {
            function.drop_plans.iter().flat_map(move |(exit, plan)| {
                plan.drops
                    .iter()
                    .filter(|drop| matches!(drop.kind, DropKind::VecIterCursor { .. }))
                    .map(move |drop| (function.name.as_str(), exit, drop))
            })
        })
        .collect();
    assert!(
        !cursor_drops.is_empty(),
        "the fixture must produce first-class cursor abandon releases"
    );
    for (function, exit, drop) in &cursor_drops {
        assert!(
            drop.guard.is_some(),
            "{function}: every cursor abandon release must be flag-gated: \
             {exit:?} -> {drop:?}"
        );
        assert_eq!(
            drop.kind,
            DropKind::VecIterCursor {
                release: CowHeapRelease::VecPlain,
            },
            "{function}: VecIter<i64> must select the plain Vec release"
        );
    }

    for (needle, expected_path, live_at_return) in [
        ("cancel_cursor", "cancel", false),
        ("panic_cursor", "panic", false),
        ("yield_cursor", "yield", true),
        ("park", "suspend", false),
    ] {
        assert!(
            cursor_drops.iter().any(|(function, exit, _)| {
                function.contains(needle)
                    && matches!(
                        (expected_path, *exit),
                        ("cancel", ExitPath::Cancel { .. })
                            | ("panic", ExitPath::Panic { .. })
                            | ("yield", ExitPath::Yield { .. })
                            | ("suspend", ExitPath::Suspend { .. })
                    )
            }),
            "`{needle}` must carry a guarded VecIter field release on its \
             {expected_path} abandonment edge: {cursor_drops:#?}"
        );
        assert_eq!(
            cursor_drops
                .iter()
                .filter(|(function, exit, _)| {
                    function.contains(needle) && matches!(exit, ExitPath::Return { .. })
                })
                .count(),
            usize::from(live_at_return),
            "`{needle}` must carry a guarded Return cleanup exactly when its cursor \
             remains live to Return: {cursor_drops:#?}"
        );
    }

    assert!(
        cursor_drops.iter().all(|(_, exit, _)| !matches!(
            exit,
            ExitPath::Goto { .. }
                | ExitPath::Branch { .. }
                | ExitPath::Call { .. }
                | ExitPath::Send { .. }
                | ExitPath::Ask { .. }
                | ExitPath::Select { .. }
                | ExitPath::Join { .. }
        )),
        "internal normal flow must retain the cursor until its guarded lexical \
         return or an abandonment edge: {cursor_drops:#?}"
    );

    for function in ["cancel_cursor", "panic_cursor", "Sleeper__recv__park"] {
        assert_cursor_release_disarms_before_later_exit(&pipeline, function);
    }
}

fn assert_cursor_release_disarms_before_later_exit(pipeline: &IrPipeline, function_name: &str) {
    let function = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == function_name)
        .unwrap_or_else(|| panic!("{function_name} raw MIR"));
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
        1,
        "{function_name}: the normal path must release its cursor exactly once"
    );
    assert!(
        release_blocks
            .iter()
            .all(|block| block.instructions.iter().any(|instr| matches!(
                instr,
                Instr::ConstI64 {
                    dest: Place::Local(_),
                    value: 1
                }
            ))),
        "every normal cursor field release must disarm its sidecar before \
         continuing, preventing a later exit from reusing the same release \
         authority in {function_name}: {release_blocks:#?}"
    );
}

#[test]
fn cursor_call_carrier_transfer_rearms_the_local_destination_owner() {
    fn inherits_owned_zero(
        place: Place,
        instructions: &[&Instr],
        seen: &mut std::collections::HashSet<Place>,
    ) -> bool {
        if !seen.insert(place) {
            return false;
        }
        if instructions.iter().any(|instr| {
            matches!(
                instr,
                Instr::ConstI64 { dest, value: 0 } if *dest == place
            )
        }) {
            return true;
        }
        instructions.iter().any(|instr| match instr {
            Instr::Move { dest, src } if *dest == place => {
                inherits_owned_zero(*src, instructions, seen)
            }
            _ => false,
        })
    }

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
        3,
        "the source parameter, moved local, and discarded read retain path-sensitive guarded releases: {:#?}",
        raw_instructions(&pipeline, "consume_cursor")
    );
    let instructions = raw_instructions(&pipeline, "consume_cursor");
    assert!(
        guard_flags.iter().any(|guard_flag| inherits_owned_zero(
            *guard_flag,
            &instructions,
            &mut std::collections::HashSet::new(),
        )),
        "moving and neutralizing an owned call-carrier parameter must re-arm \
         the destination sidecar as owned: {instructions:#?}"
    );
    assert!(
        guard_flags.iter().any(|guard_flag| {
            instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::ConstI64 { dest, value: 0 } if dest == guard_flag
                )
            }) && instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::ConstI64 { dest, value: 1 } if dest == guard_flag
                )
            })
        }),
        "the source parameter's initially-owned sidecar must be disarmed after \
         transfer, so its retained path-sensitive release cannot double-drop: \
         {instructions:#?}"
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
