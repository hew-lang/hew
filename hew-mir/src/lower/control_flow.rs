use super::{
    base_local, integer_bit_width, integer_signedness, ActiveIterationOwner, Builder, CmpPred,
    HashSet, HirExpr, Instr, IntArithOp, IntSignedness, LoopFrame, MirDiagnostic,
    MirDiagnosticKind, MirStatement, Place, ProjectedPayloadOrigin, ProjectedPayloadRejectReason,
    ResolvedTy, Terminator, TrapKind, SYNTHETIC_CALL_SCRUTINEE_NAME,
};

impl Builder {
    /// Lower `let PAT = scrutinee else { <divergent block> };`.
    ///
    /// Mirrors `lower_if_let`'s tag-test CFG, with two deliberate differences:
    ///   1. The success-path payload bindings are inserted into
    ///      `binding_locals` and NEVER restored — they escape into the
    ///      enclosing scope so the rest of the block can read them.
    ///   2. There is no join/result place. On a match, control flows straight
    ///      into the continuation block (the cursor) with the binders live. On
    ///      a mismatch, the else block runs; it is divergent (the checker
    ///      proved `Ty::Never`), so it seals its own block with a diverging
    ///      terminator and the continuation is reached only via the match edge.
    #[allow(
        clippy::too_many_lines,
        reason = "mirrors lower_if_let's tag-test CFG builder; splitting would require threading many intermediate block ids across helper boundaries"
    )]
    pub(crate) fn lower_let_else_stmt(
        &mut self,
        scrutinee: &HirExpr,
        variant_idx: u32,
        bindings: &[hew_hir::HirMatchArmBinding],
        success_prelude: &[hew_hir::HirStmt],
        payload_variant_predicates: &[hew_hir::HirPayloadVariantPredicate],
        else_body: &hew_hir::HirBlock,
    ) {
        // #2648 preflight — run BEFORE any allocation or scrutinee lowering. A
        // reject pushes one diagnostic and returns with no partial MIR; the
        // admission token also gates the #2429 from-call owner mint below.
        let scrutinee_admission = match self.classify_call_scrutinee_admission(scrutinee) {
            Ok(admission) => admission,
            Err(diag) => {
                self.diagnostics.push(*diag);
                return;
            }
        };
        // Entry: evaluate scrutinee, load tag, branch.
        let Some(scrutinee_place) = self.lower_value(scrutinee) else {
            return;
        };
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "let-else scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "let-else scrutinee must lower to Place::Local; got {other:?}. \
                         The HIR producer should only emit LetElse for enum-typed \
                         scrutinees backed by a local slot"
                    ),
                });
                return;
            }
        };

        // #2429 — give a FROM-CALL enum-composite let-else scrutinee an owner,
        // symmetric with `lower_match_enum_tag`/`lower_if_let`. Registered BEFORE
        // the branch and the payload-predicate checks that route to `else_bb`, so
        // the synthetic owned local is live on BOTH paths: the match path (payload
        // moved out into the escaping binders, shell composite drop) and the
        // divergent else path (`return`/`break`/`continue`/`panic` — no move-out,
        // so the FULL temp drops once on that edge). The scope-exit drop
        // elaboration frees it on whichever edge leaves the enclosing scope,
        // including the divergent-else edges. No-op for the non-Call / carrier
        // shapes per `register_from_call_scrutinee_owner`.
        self.register_from_call_scrutinee_owner(scrutinee_admission, scrutinee, scrutinee_local);

        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(scrutinee_local),
        });
        let k_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: k_local,
            value: i64::from(variant_idx),
        });
        let cond_local = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::Eq,
            lhs: tag_local,
            rhs: k_local,
            dest: cond_local,
        });

        let bind_bb = self.alloc_block();
        let else_bb = self.alloc_block();
        let cont_bb = self.alloc_block();

        self.finish_current_block(Terminator::Branch {
            cond: cond_local,
            then_target: bind_bb,
            else_target: else_bb,
        });

        // Match path: bind the payload fields into the ENCLOSING scope's
        // binding_locals and DO NOT restore — they escape the statement. Then
        // Goto the continuation, where subsequent statements lower with the
        // binders live.
        self.start_block(bind_bb);
        // #2523 — classify the scrutinee once for the whole let-else so every
        // heap-owning payload binder routes its move-out through the shared
        // default-deny consume policy (see `classify_scrutinee_origin`).
        let scrutinee_origin = self.classify_scrutinee_origin(scrutinee);
        let mut nested_binding_jobs: Vec<(u32, u32, hew_hir::HirMatchArmBinding)> = Vec::new();
        for pvp in payload_variant_predicates {
            // A failed nested check routes to the else block, same as a
            // top-level tag mismatch.
            if self
                .emit_payload_variant_predicate_checks(
                    pvp,
                    scrutinee_local,
                    variant_idx,
                    else_bb,
                    scrutinee.site,
                    &mut nested_binding_jobs,
                )
                .is_none()
            {
                return;
            }
        }

        for binding in bindings {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            // Escape: insert into binding_locals and never restore.
            self.binding_locals.insert(binding.binding, dest);
            // #2523 — record provenance for a heap-owning TOP-LEVEL let-else
            // payload binder so its move-out routes through default-deny.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
                scrutinee_origin.clone(),
                keep_for_drop_elab,
            );
        }
        for (src_local, src_variant_idx, binding) in nested_binding_jobs {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            self.binding_locals.insert(binding.binding, dest);
            // #2523 F2 — nested let-else binder bound from a transient predicate
            // copy; reject a heap-owning move-out fail-closed.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
                ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::NestedDestructure),
                keep_for_drop_elab,
            );
        }
        // Aggregate-payload destructure (`Ok((n, s))`): the prelude `Let`
        // statements project the synthetic `__payload_*` temp into the leaf
        // binders (`n`, `s`). They run on the SUCCESS path after the top-level
        // payload fields bind, and like those fields their binding_locals are
        // inserted (by the normal `Let` lowering) and never restored, so the
        // leaf binders escape into the enclosing scope for the continuation.
        for stmt in success_prelude {
            self.stmt(stmt);
        }
        self.finish_current_block(Terminator::Goto { target: cont_bb });

        // No-match path: run the divergent else block. The checker proved it
        // has type `Ty::Never`, so its body seals the block with a diverging
        // terminator (Return/Trap). Defensive Goto for malformed HIR where the
        // else somehow falls through — the cursor is unreachable in the
        // well-formed case.
        self.start_block(else_bb);
        self.active_scopes.push(else_body.scope);
        for stmt in &else_body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &else_body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(else_body.scope);
        self.active_scopes.pop();
        self.finish_current_block(Terminator::Goto { target: cont_bb });

        // Continuation: subsequent statements lower here, with the escaped
        // binders live in binding_locals.
        self.start_block(cont_bb);
    }

    /// Lower `while cond { body }` to a three-block CFG:
    ///
    /// ```text
    /// entry_bb (current):
    ///   Goto header_bb
    ///
    /// header_bb:
    ///   cond_place = lower(condition)
    ///   Branch { cond: cond_place, then: body_bb, else: exit_bb }
    ///
    /// body_bb:
    ///   lower(body statements)
    ///   Goto header_bb          ← back-edge
    ///
    /// exit_bb:
    ///   (subsequent lowering continues here)
    /// ```
    ///
    /// The while expression always has type Unit; `None` is returned
    /// (no value Place) matching the semantics of a statement-level loop.
    pub(crate) fn lower_while(
        &mut self,
        label: Option<&str>,
        condition: &HirExpr,
        body: &hew_hir::HirBlock,
    ) -> Option<Place> {
        let header_bb = self.alloc_block();
        let body_bb = self.alloc_block();
        let exit_bb = self.alloc_block();

        // Entry: unconditional jump to the header (condition check).
        self.finish_current_block(Terminator::Goto { target: header_bb });

        // Header: evaluate condition and branch.
        self.start_block(header_bb);
        let cond_place = self.lower_value(condition)?;
        self.finish_current_block(Terminator::Branch {
            cond: cond_place,
            then_target: body_bb,
            else_target: exit_bb,
        });

        // Body: lower statements then loop back.
        self.start_block(body_bb);
        // continue → header (re-checks the condition); break → exit.
        let loop_scope_depth = self.active_scopes.len();
        self.loop_stack.push(LoopFrame {
            label: label.map(str::to_string),
            continue_target: header_bb,
            exit_target: exit_bb,
            scope_depth: loop_scope_depth,
            body_scope: body.scope,
        });
        self.active_scopes.push(body.scope);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        // Release generators declared in the loop body before the back-edge so
        // a `let g = gen()` (consumed or not) frees its coro frame + heap
        // companion every iteration rather than accumulating one per pass (see
        // `emit_scope_generator_drops`).
        self.emit_scope_generator_drops(body.scope);
        // #1949 — release sole-owner `for x in …` cursors (`VecIter`) declared
        // directly in this loop body before the back-edge, the cursor analogue of
        // the generator release above (see `emit_scope_vec_iter_drops`).
        self.emit_scope_vec_iter_drops(body.scope);
        self.emit_scope_stream_drops(body.scope);
        // Record this block as a loop-body back-edge so `enumerate_exits`
        // populates its `Goto` `DropPlan` with per-iteration releases for
        // heap-owning bindings declared in `body.scope`. Without this, an
        // `Option<T>` (or other heap-owning) let-binding bound inside the body
        // gets overwritten on the next iteration with no preceding drop — the
        // memory leak this fix closes (Stream<T>/Receiver<T> recv loops).
        // The scope captured is the body's, not any nested block's: nested
        // block-scope bindings already self-drop when their block closes via
        // the existing scope-exit pass.
        self.loop_back_edge_blocks
            .insert(self.current_block_id, body.scope);
        self.active_scopes.pop();
        self.loop_stack.pop();
        self.finish_current_block(Terminator::Goto { target: header_bb });

        // Exit: subsequent lowering continues here.
        self.start_block(exit_bb);
        None
    }

    /// Lower `while let <Ctor>(bindings) = scrutinee { body }` to a four-block
    /// CFG that re-evaluates the scrutinee each iteration and dispatches on
    /// the resulting enum tag:
    ///
    /// ```text
    /// entry_bb (current):
    ///   Goto header_bb
    ///
    /// header_bb:
    ///   scrutinee_place = lower(scrutinee)        ← re-evaluated each iter
    ///   tag_local = Move { src: Place::EnumTag(scrutinee_local) }
    ///   k_local = ConstI64(variant_idx)
    ///   cond_local = IntCmp(Eq, tag_local, k_local)
    ///   Branch { cond: cond_local, then: body_bb, else: exit_bb }
    ///
    /// body_bb:
    ///   for each binding:
    ///     dest = Move { src: Place::MachineVariant { scrutinee_local,
    ///                                                variant_idx, field_idx } }
    ///     register binding_locals[binding.id] = dest
    ///   lower(body)
    ///   Goto header_bb                            ← back-edge
    ///
    /// exit_bb:
    ///   (subsequent lowering continues here)
    /// ```
    ///
    /// The CFG is a hybrid of `lower_while` (header/body/exit shape) and
    /// `lower_match_enum_tag` (tag-compare + payload extraction). The
    /// scrutinee is evaluated inside the header block so each loop iteration
    /// produces a fresh enum value — matching the surface semantics of
    /// `while let` re-checking the pattern on every pass.
    ///
    /// The expression always has type `Unit`; `None` is returned (no value
    /// Place) matching `lower_while`.
    pub(crate) fn lower_while_let(
        &mut self,
        label: Option<&str>,
        scrutinee: &HirExpr,
        variant_idx: u32,
        bindings: &[hew_hir::HirMatchArmBinding],
        payload_variant_predicates: &[hew_hir::HirPayloadVariantPredicate],
        body: &hew_hir::HirBlock,
    ) -> Option<Place> {
        // Two-line addition for back-edge `DropPlan` plumbing tips this fn
        // past clippy's 100-line bar; the algorithm is one coherent unit and
        // factoring it would obscure the loop CFG.
        #![allow(
            clippy::too_many_lines,
            reason = "back-edge DropPlan plumbing tipped this past clippy's 100-line bar; \
                      the algorithm is one coherent unit and factoring it would obscure \
                      the loop CFG"
        )]
        // #2648 preflight — run BEFORE any block allocation or scrutinee lowering.
        // A reject leaves no half-built loop CFG.
        let scrutinee_admission = match self.classify_call_scrutinee_admission(scrutinee) {
            Ok(admission) => admission,
            Err(diag) => {
                self.diagnostics.push(*diag);
                return None;
            }
        };
        // Fail-closed: a `while let` over an enum variant that leaves an OWNED
        // payload field unaccounted for (a `..` rest or a bare `_` sibling)
        // cannot release that field's heap on the loop back-edge. The header
        // re-evaluates the scrutinee every iteration, and the skipped owner has
        // no per-iteration release site — a reassigned scrutinee leaks the
        // prior iteration's skipped payload. The bound and nested-matched
        // siblings ride the composite/back-edge drop; a skipped owned field
        // does not. `match`, `if let`, `let ... else`, and record/tuple
        // destructure DO discharge this shape (the enum composite drop / the
        // field-drop safety loop), so this refusal is `while let`-specific.
        // Run before any block allocation so a reject leaves no partial CFG.
        if let Some((idx, field_ty)) = self.while_let_skipped_owned_payload_field(
            scrutinee,
            variant_idx,
            bindings,
            payload_variant_predicates,
        ) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "while-let skipped owned enum payload field".to_string(),
                    site: scrutinee.site,
                },
                note: format!(
                    "payload field {idx} (`{}`) of this variant is owned but neither \
                     bound nor matched, so `while let` cannot release it on the loop \
                     back-edge — each re-entry would leak the prior iteration's value. \
                     Bind every owned payload field explicitly (use a name instead of \
                     `_`, and list every field instead of `..`), or destructure with \
                     `match` / `if let` / `let ... else`, which release the skipped \
                     owned sibling through the enum composite drop",
                    field_ty.user_facing(),
                ),
            });
            return None;
        }
        let header_bb = self.alloc_block();
        let body_bb = self.alloc_block();
        let exit_bb = self.alloc_block();

        // Entry → header.
        self.finish_current_block(Terminator::Goto { target: header_bb });

        // Header: re-evaluate scrutinee, load enum tag, compare to variant_idx,
        // branch to body or exit.
        self.start_block(header_bb);
        let scrutinee_place = self.lower_value(scrutinee)?;
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                // Fail closed: a poisoned scrutinee shape leaves no half-built
                // CFG (same fail-closed pattern as `lower_match_enum_tag`).
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "while-let scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "while-let scrutinee must lower to Place::Local; got {other:?}. \
                         The HIR producer should only emit WhileLet for enum-typed scrutinees \
                         backed by a local slot"
                    ),
                });
                return None;
            }
        };
        let scrutinee_owner = self.register_from_call_scrutinee_owner(
            scrutinee_admission,
            scrutinee,
            scrutinee_local,
        );
        let false_cleanup_bb = scrutinee_owner.as_ref().map(|_| self.alloc_block());

        // Load the variant tag into a fresh i64 local, mirroring
        // `lower_match_enum_tag`. `Place::EnumTag(local)` is the substrate
        // primitive that codegen GEPs to outer-struct field 0; widening from
        // the per-enum tag width to i64 happens inside the Move arm.
        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(scrutinee_local),
        });

        // Compare tag against the continue-arm variant index.
        let k_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: k_local,
            value: i64::from(variant_idx),
        });
        let cond_local = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::Eq,
            lhs: tag_local,
            rhs: k_local,
            dest: cond_local,
        });
        self.finish_current_block(Terminator::Branch {
            cond: cond_local,
            then_target: body_bb,
            else_target: false_cleanup_bb.unwrap_or(exit_bb),
        });

        // Body: bind payload fields, lower body, loop back to header.
        // The binding writes use `Place::MachineVariant` (same primitive used
        // by `lower_match_enum_tag` arm-body entry); MIR codegen GEPs to the
        // variant payload field.
        //
        // Bindings live for the body block only — we save and restore any
        // pre-existing entries in `binding_locals` so nested while-let loops
        // can shadow the same name without confusion.
        self.start_block(body_bb);
        let mut nested_binding_jobs: Vec<(u32, u32, hew_hir::HirMatchArmBinding)> = Vec::new();
        for pvp in payload_variant_predicates {
            self.emit_payload_variant_predicate_checks(
                pvp,
                scrutinee_local,
                variant_idx,
                false_cleanup_bb.unwrap_or(exit_bb),
                scrutinee.site,
                &mut nested_binding_jobs,
            )?;
        }

        let mut overwritten_bindings =
            Vec::with_capacity(bindings.len() + nested_binding_jobs.len());
        // #2523 — classify the while-let scrutinee once so every heap-owning
        // payload binder routes its move-out through the shared default-deny
        // consume policy. The loop back-edge re-reads the scrutinee every
        // iteration, so an owning-binding move-out MUST become a compile-time
        // use-after-move (the canonical #2523 shape).
        let scrutinee_origin = self.classify_scrutinee_origin(scrutinee);
        for binding in bindings {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            let previous = self.binding_locals.insert(binding.binding, dest);
            if let Some(local) = base_local(dest) {
                self.transient_local_scopes.insert(local, body.scope);
            }
            overwritten_bindings.push((binding.binding, previous));
            // #2523 — record provenance for a heap-owning TOP-LEVEL while-let
            // payload binder so its move-out routes through default-deny.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
                scrutinee_origin.clone(),
                keep_for_drop_elab,
            );
        }
        for (src_local, src_variant_idx, binding) in nested_binding_jobs {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            let previous = self.binding_locals.insert(binding.binding, dest);
            if let Some(local) = base_local(dest) {
                self.transient_local_scopes.insert(local, body.scope);
            }
            overwritten_bindings.push((binding.binding, previous));
            // #2523 F2 — nested while-let binder bound from a transient predicate
            // copy; reject a heap-owning move-out fail-closed.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
                ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::NestedDestructure),
                keep_for_drop_elab,
            );
        }

        self.active_scopes.push(body.scope);
        // continue → header (re-evaluates scrutinee + tag); break → exit.
        let loop_scope_depth = self.active_scopes.len() - 1;
        self.loop_stack.push(LoopFrame {
            label: label.map(str::to_string),
            continue_target: header_bb,
            exit_target: exit_bb,
            scope_depth: loop_scope_depth,
            body_scope: body.scope,
        });
        let active_iteration_owner_mark = self.active_iteration_owners.len();
        if let Some((binding, ty)) = &scrutinee_owner {
            self.active_iteration_owners.push(ActiveIterationOwner {
                scope_depth: self.active_scopes.len(),
                binding: *binding,
                name: SYNTHETIC_CALL_SCRUTINEE_NAME.to_string(),
                site: scrutinee.site,
                ty: ty.clone(),
            });
        }
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        // Release generators declared in the loop body before the back-edge
        // (per-iteration `hew_gen_coro_destroy`; see `emit_scope_generator_drops`).
        self.emit_scope_generator_drops(body.scope);
        // #1949 — release sole-owner `for x in …` cursors (`VecIter`) declared
        // directly in this loop body before the back-edge, the cursor analogue of
        // the generator release above (see `emit_scope_vec_iter_drops`).
        self.emit_scope_vec_iter_drops(body.scope);
        self.emit_scope_stream_drops(body.scope);
        if let Some((binding, ty)) = &scrutinee_owner {
            self.record_iteration_owner_drop(
                *binding,
                SYNTHETIC_CALL_SCRUTINEE_NAME,
                scrutinee.site,
                ty,
            );
        }
        // Record this block as a loop-body back-edge so `enumerate_exits`
        // populates its `Goto` `DropPlan` with per-iteration releases for
        // heap-owning bindings declared in `body.scope` (including the
        // match-arm payload bindings the `while let` itself introduces).
        self.loop_back_edge_blocks
            .insert(self.current_block_id, body.scope);
        self.active_iteration_owners
            .truncate(active_iteration_owner_mark);
        self.active_scopes.pop();
        self.loop_stack.pop();
        // Restore the prior `binding_locals` entries so the binding scope
        // ends at the body's end — matches Match-arm body-block semantics.
        for (binding, previous) in overwritten_bindings.into_iter().rev() {
            if let Some(previous) = previous {
                self.binding_locals.insert(binding, previous);
            } else {
                self.binding_locals.remove(&binding);
            }
        }

        self.finish_current_block(Terminator::Goto { target: header_bb });

        if let (Some(false_cleanup_bb), Some((binding, ty))) = (false_cleanup_bb, &scrutinee_owner)
        {
            self.start_block(false_cleanup_bb);
            self.record_iteration_owner_drop(
                *binding,
                SYNTHETIC_CALL_SCRUTINEE_NAME,
                scrutinee.site,
                ty,
            );
            self.finish_current_block(Terminator::Goto { target: exit_bb });
        }

        // Exit: subsequent lowering continues here.
        self.start_block(exit_bb);
        None
    }

    /// Detect a `while let` enum-variant pattern that leaves an OWNED payload
    /// field unaccounted for — skipped by a `..` rest or a bare `_` sibling.
    /// Returns the first such `(field_idx, field_ty)`, or `None` when every
    /// owned payload field is bound or nested-matched (`BitCopy` skips are
    /// safe: there is no heap to leak). Drives the `while let`-specific
    /// fail-closed refusal in [`Self::lower_while_let`]. "Owned" is the same
    /// non-`BitCopy` drop-seed predicate the record/tuple discharge uses
    /// ([`Self::binding_seeds_drop_elaboration`]); a field is accounted for
    /// when a top-level binding or a nested payload predicate covers it.
    fn while_let_skipped_owned_payload_field(
        &self,
        scrutinee: &HirExpr,
        variant_idx: u32,
        bindings: &[hew_hir::HirMatchArmBinding],
        payload_variant_predicates: &[hew_hir::HirPayloadVariantPredicate],
    ) -> Option<(u32, ResolvedTy)> {
        use crate::model::HeapOwnershipLayouts as _;
        let subst = self.subst_ty(&scrutinee.ty);
        let ResolvedTy::Named { name, args, .. } = &subst else {
            return None;
        };
        let layouts = crate::model::MirHeapLayouts {
            record_field_orders: &self.record_field_orders,
            enum_layouts: &self.enum_layouts,
        };
        let variants = layouts.enum_variant_field_tys(name, args)?;
        let payload = variants.get(variant_idx as usize)?;
        let mut accounted: HashSet<u32> = bindings.iter().map(|b| b.field_idx).collect();
        accounted.extend(payload_variant_predicates.iter().map(|p| p.field_idx));
        for (idx, field_ty) in payload.iter().enumerate() {
            let Ok(idx) = u32::try_from(idx) else {
                continue;
            };
            if accounted.contains(&idx) {
                continue;
            }
            let substituted = self.subst_ty(field_ty);
            if self.binding_seeds_drop_elaboration(&substituted) {
                return Some((idx, substituted));
            }
        }
        None
    }

    /// Lower `if let PAT = scrutinee { body } else { else_body }` to a
    /// three-block CFG that mirrors `lower_if` (for the branch shape) plus the
    /// payload-binding emit from `lower_while_let` (for the then arm):
    ///
    /// ```text
    /// entry_bb (current):
    ///   result_local = alloc(result_ty)
    ///   scrutinee_local = lower(scrutinee)
    ///   tag_local = Move from Place::EnumTag(scrutinee_local)
    ///   k = ConstI64(variant_idx)
    ///   cond = IntCmp(Eq, tag, k)
    ///   Branch { cond, then: then_bb, else: else_bb }
    ///
    /// then_bb:
    ///   [payload bindings via Place::MachineVariant]
    ///   result_local = lower(body)
    ///   Goto join_bb
    ///
    /// else_bb:
    ///   [result_local = lower(else_body)]   (or no-op when else absent)
    ///   Goto join_bb
    ///
    /// join_bb:
    ///   (subsequent lowering continues here; value = result_local)
    /// ```
    ///
    /// Returns the result `Place::Local` (which both branches write). For a
    /// Unit-typed `if let` the result local is allocated but never read.
    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "single coherent CFG builder for if-let; mirrors lower_while_let binding \
                  setup, nested predicate checks, and lower_if branch shape — splitting would \
                  require passing many intermediate block IDs and binding-restore state across \
                  helper boundaries"
    )]
    pub(crate) fn lower_if_let(
        &mut self,
        scrutinee: &HirExpr,
        variant_idx: u32,
        bindings: &[hew_hir::HirMatchArmBinding],
        payload_variant_predicates: &[hew_hir::HirPayloadVariantPredicate],
        body: &hew_hir::HirBlock,
        else_body: Option<&hew_hir::HirBlock>,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        // #2648 preflight — run BEFORE any allocation or scrutinee lowering. A
        // reject short-circuits with no partial MIR; the admission token also
        // gates the #2429 from-call owner mint below (symmetric with
        // `lower_match_enum_tag`/`lower_while_let`), so it is threaded through
        // rather than discarded.
        let scrutinee_admission = match self.classify_call_scrutinee_admission(scrutinee) {
            Ok(admission) => admission,
            Err(diag) => {
                self.diagnostics.push(*diag);
                return None;
            }
        };
        let result_place = self.alloc_local(self.subst_ty(result_ty));

        // Entry: evaluate scrutinee, load tag, branch.
        let scrutinee_place = self.lower_value(scrutinee)?;
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "if-let scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "if-let scrutinee must lower to Place::Local; got {other:?}. \
                         The HIR producer should only emit IfLet for enum-typed scrutinees \
                         backed by a local slot"
                    ),
                });
                return None;
            }
        };

        // #2429 — give a FROM-CALL enum-composite if-let scrutinee an owner so
        // its payload is released on EVERY exit edge, exactly as
        // `lower_match_enum_tag`/`lower_while_let` already do. Registered here,
        // BEFORE the branch and the mid-then `emit_payload_variant_predicate_checks`
        // that route to `else_bb`, so the synthetic owned local is live on the
        // matched edge (payload moved out, shell composite drop), the refuted
        // `else_bb` edge, AND any nested-predicate fallthrough — the scope-exit
        // drop elaboration then frees it once on whichever edge leaves the
        // enclosing scope. No-op for binding-ref scrutinees, runtime-symbol
        // producers, and the recv/iter-next shapes carrying their own release.
        // Non-loop: the return is discarded (mirroring `lower_match_enum_tag`);
        // the scope-exit machinery handles every edge, so no explicit
        // per-iteration owner-drop plumbing is needed (that is `lower_while_let`'s
        // loop-only concern).
        self.register_from_call_scrutinee_owner(scrutinee_admission, scrutinee, scrutinee_local);

        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(scrutinee_local),
        });
        let k_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: k_local,
            value: i64::from(variant_idx),
        });
        let cond_local = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::Eq,
            lhs: tag_local,
            rhs: k_local,
            dest: cond_local,
        });

        let then_bb = self.alloc_block();
        let else_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        // Track whether either arm falls through to the join with a value.
        // When BOTH arms diverge (each `return`s/`panic`s, possibly through a
        // further nested CFG expression) the join has no live predecessor and
        // `result_place` is never written; the cursor must stay unreachable so
        // a tail `if let` does not feed the dead `Unit` i8 stand-in into a
        // non-scalar return slot (the #1907 `Move type mismatch` abort). The
        // reachability flag — not the value `Option` — is the load-bearing
        // signal (see `lower_if`/`lower_match_enum_tag` for the rationale).
        let mut join_reachable = false;

        self.finish_current_block(Terminator::Branch {
            cond: cond_local,
            then_target: then_bb,
            else_target: else_bb,
        });

        // Then arm: bind payload fields (same as lower_while_let body entry),
        // lower body, move result into result_place.
        self.start_block(then_bb);
        let mut nested_binding_jobs: Vec<(u32, u32, hew_hir::HirMatchArmBinding)> = Vec::new();
        for pvp in payload_variant_predicates {
            self.emit_payload_variant_predicate_checks(
                pvp,
                scrutinee_local,
                variant_idx,
                else_bb,
                scrutinee.site,
                &mut nested_binding_jobs,
            )?;
        }

        let mut overwritten_bindings =
            Vec::with_capacity(bindings.len() + nested_binding_jobs.len());
        // #2523 — classify the if-let scrutinee once so every heap-owning
        // payload binder routes its move-out through the shared default-deny
        // consume policy.
        let scrutinee_origin = self.classify_scrutinee_origin(scrutinee);
        for binding in bindings {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            let previous = self.binding_locals.insert(binding.binding, dest);
            overwritten_bindings.push((binding.binding, previous));
            // #2523 — record provenance for a heap-owning TOP-LEVEL if-let
            // payload binder so its move-out routes through default-deny.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
                scrutinee_origin.clone(),
                keep_for_drop_elab,
            );
        }
        for (src_local, src_variant_idx, binding) in nested_binding_jobs {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            let previous = self.binding_locals.insert(binding.binding, dest);
            overwritten_bindings.push((binding.binding, previous));
            // #2523 F2 — nested if-let binder bound from a transient predicate
            // copy; reject a heap-owning move-out fail-closed.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
                ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::NestedDestructure),
                keep_for_drop_elab,
            );
        }

        self.active_scopes.push(body.scope);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        let then_value = if let Some(tail) = &body.tail {
            self.lower_value(tail)
        } else {
            None
        };
        if let Some(src) = then_value {
            self.push_instr(Instr::Move {
                dest: result_place,
                src,
            });
        }
        self.emit_pending_defers(body.scope);
        self.active_scopes.pop();

        // Restore binding_locals after then-arm scope ends.
        for (binding, previous) in overwritten_bindings.into_iter().rev() {
            if let Some(previous) = previous {
                self.binding_locals.insert(binding, previous);
            } else {
                self.binding_locals.remove(&binding);
            }
        }
        // Binding restore does not touch `cursor_unreachable`; the flag still
        // reflects whether the then-body diverged.
        if !self.cursor_unreachable {
            join_reachable = true;
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Else arm: lower else_body (if present), move result into result_place.
        // `else_body: None` emits a Goto-only block that always falls through,
        // so a one-armed `if let ... { return }` keeps the join reachable.
        self.start_block(else_bb);
        if let Some(eb) = else_body {
            self.active_scopes.push(eb.scope);
            for stmt in &eb.statements {
                self.stmt(stmt);
            }
            let else_value = if let Some(tail) = &eb.tail {
                self.lower_value(tail)
            } else {
                None
            };
            if let Some(src) = else_value {
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
            self.emit_pending_defers(eb.scope);
            self.active_scopes.pop();
        }
        if !self.cursor_unreachable {
            join_reachable = true;
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Join: subsequent lowering continues here. `start_block` resets
        // `cursor_unreachable`, so re-flag the dead join AFTER opening it when
        // both arms diverged.
        self.start_block(join_bb);
        if !join_reachable {
            self.cursor_unreachable = true;
        }
        Some(result_place)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single coherent CFG builder for the for-range loop; splitting would require passing many intermediate block IDs across helper boundaries"
    )]
    /// Lower `for binding in start..end { body }` (or `start..=end`) to a
    /// four-block CFG:
    ///
    /// ```text
    /// entry_bb (current):
    ///   counter = lower(start)
    ///   end_val = lower(end)
    ///   [inclusive?]  end_val = IntArithChecked(Add, end_val, 1)
    ///                   → on overflow → trap_bb { IntegerOverflow }
    ///                   → on success → header_bb
    ///   [exclusive?]  Goto header_bb
    ///
    /// header_bb:
    ///   cond = IntCmp(SignedLess, counter, end_val)
    ///   Branch { cond, then: body_bb, else: exit_bb }
    ///
    /// body_bb:
    ///   binding ← counter       (Move)
    ///   lower(body statements)
    ///   Goto inc_bb              ← fall-through (also the `continue` target)
    ///
    /// inc_bb:
    ///   counter = IntArithChecked(Add, counter, 1) → trap on overflow
    ///   Goto header_bb           ← back-edge
    ///
    /// exit_bb:
    ///   (subsequent lowering continues here)
    /// ```
    ///
    /// The inclusive form `start..=end` adjusts `end_val` by +1 before the
    /// loop header so the header's strict-less-than predicate covers the
    /// inclusive endpoint.  Overflow on +1 traps as `IntegerOverflow`,
    /// matching B-2 discipline.
    ///
    /// The counter increment also uses `IntArithChecked` with a
    /// `TrapKind::IntegerOverflow` guard.  A loop iterating to `i64::MAX`
    /// would try to increment past it and trap rather than silently wrapping
    /// — fail-closed per the reliability tenet.
    ///
    /// ## Adapters: `step` and `descending`
    ///
    /// `step` is the per-iteration stride (default literal `1`, set by
    /// `.step_by(k)`); `descending` is set by `.rev()`.  The two compose:
    ///
    ///   - **Ascending** (`descending == false`): counter starts at `start`,
    ///     the header tests `counter < end_val` (with `end_val = end (+1 if
    ///     inclusive)`), and each iteration adds `step` (checked → trap on
    ///     overflow).
    ///   - **Descending** (`descending == true`): counter starts at the high
    ///     element (`end` if inclusive, `end - 1` if exclusive), the header
    ///     tests `counter >= start`, and each iteration subtracts `step`.  A
    ///     checked subtract that underflows the counter's type (e.g.
    ///     `0u32 - 1`) is the natural loop terminus and branches to the exit
    ///     rather than trapping, so a descending unsigned range to `0` does
    ///     not wrap.  This makes `(0..5).rev()` yield `4 3 2 1 0` and
    ///     `(0..=10).rev().step_by(3)` yield `10 7 4 1`.
    ///
    /// A statically-zero step is rejected by the checker; a runtime-zero step
    /// (`step_by(n)` with `n == 0`) traps as `DivideByZero` before the loop
    /// header so the loop can never spin forever — fail-closed.
    #[allow(
        clippy::too_many_arguments,
        reason = "the ForRange node's fields (label, binding, start, end, inclusive, step, descending, body) are threaded individually; bundling them into a struct would only re-spread them here"
    )]
    pub(crate) fn lower_for_range(
        &mut self,
        label: Option<&str>,
        binding: &hew_hir::HirBinding,
        start: &HirExpr,
        end: &HirExpr,
        inclusive: bool,
        step: &HirExpr,
        descending: bool,
        body: &hew_hir::HirBlock,
    ) -> Option<Place> {
        // The loop counter and bound use the checker-resolved element type from
        // the HIR binding.  The HIR lowers this from the range bounds, so a
        // `for i in 2..n` with `n: i32` produces an `i32` counter, matching the
        // widths of any `Vec<i32>` elements or other `i32` operands computed
        // from `i` inside the loop body.  Falls back to I64 when the binding
        // type is not a concrete integer (e.g. unconstrained literal range
        // `0..8` that was never narrowed by use — those still default to i64).
        let elem_ty = self.subst_ty(&binding.ty);
        let counter_ty = if integer_bit_width(&elem_ty, self.pointer_width).is_some() {
            elem_ty.clone()
        } else {
            ResolvedTy::I64
        };
        // Signedness drives the checked-arithmetic intrinsic family for the
        // counter advance.  Falls back to Signed for a non-integer counter_ty
        // (already canonicalised to I64 above), matching the historical
        // ascending behaviour.
        let counter_signedness = integer_signedness(&counter_ty).unwrap_or(IntSignedness::Signed);

        let counter = self.alloc_local(counter_ty.clone());
        // For an ascending loop `bound` is the exclusive upper bound the header
        // compares the counter against; for a descending loop it is the low
        // bound (the `start` of the range) the counter must stay `>=`.
        let bound = self.alloc_local(counter_ty.clone());

        // Loop structure blocks, allocated up front (before the
        // start/bound setup below) so the descending-exclusive emptiness
        // gate (#1948) can jump straight to `exit_bb`, bypassing the header
        // entirely for a statically-known-empty range without needing the
        // header's `counter >= bound` comparison to independently discover
        // the same fact from a synthesized counter value. `alloc_block` only
        // allocates a numeric id (no emission side effect — every block
        // below is forward-referenced by id well before `start_block` is
        // called on it, matching the existing header/body/inc/exit pattern
        // already used throughout this function), so hoisting the four ids
        // here changes nothing about where each block is actually emitted.
        // `inc_bb` is a dedicated advance block: the body falls through to it
        // AND `continue` jumps to it, so the counter advance happens on
        // every path that re-enters the header. Threading `continue` straight
        // to the header would skip the advance and spin forever (Risk 1).
        let header_bb = self.alloc_block();
        let body_bb = self.alloc_block();
        let inc_bb = self.alloc_block();
        let exit_bb = self.alloc_block();

        // Lower the stride into a local once; both directions reuse it. The
        // step expression is user source (the `step_by(n)` argument), so its
        // setup Move carries the for-loop statement span — gdb steps onto the
        // for line, not the prior statement.
        let step_place = self.lower_value(step)?;
        let step_val = self.alloc_local(counter_ty.clone());
        self.push_instr(Instr::Move {
            dest: step_val,
            src: step_place,
        });
        // Runtime fail-closed guard: a zero step would never advance the
        // counter and spin forever.  The checker rejects a statically-zero
        // literal step; this covers a dynamic `step_by(n)` with `n == 0`.
        // (A negative step is impossible for an unsigned width and is rejected
        // by the checker for a signed literal; a dynamic negative signed step
        // is caught by the same `<= 0` test against zero below.)
        //
        // SYNTHETIC step-validation guard — no user source statement. The
        // zero compare and its trap are compiler-inserted fail-closed
        // infrastructure, not anything the programmer wrote, so they stay
        // span-less (`instructions.push`); attributing them to the for line
        // would make gdb stop on a check the user never typed.
        {
            let zero = self.alloc_local(counter_ty.clone());
            self.instructions.push(Instr::ConstI64 {
                dest: zero,
                value: 0,
            });
            let bad_step = self.alloc_local(ResolvedTy::Bool);
            // For an unsigned counter the step is unsigned: only zero is
            // degenerate (`UnsignedLessEq(step, 0)` ≡ `step == 0`).  For a
            // signed counter, `SignedLessEq` also rejects negative steps.
            let step_guard_pred = match counter_signedness {
                IntSignedness::Unsigned => CmpPred::UnsignedLessEq,
                IntSignedness::Signed => CmpPred::SignedLessEq,
            };
            self.instructions.push(Instr::IntCmp {
                dest: bad_step,
                pred: step_guard_pred,
                lhs: step_val,
                rhs: zero,
            });
            let bad_step_trap = self.alloc_block();
            let step_ok_bb = self.alloc_block();
            self.finish_current_block(Terminator::Branch {
                cond: bad_step,
                then_target: bad_step_trap,
                else_target: step_ok_bb,
            });
            self.start_block(bad_step_trap);
            self.finish_current_block(Terminator::Trap {
                kind: TrapKind::DivideByZero,
            });
            self.start_block(step_ok_bb);
        }

        let raw_start = self.lower_value(start)?;
        let raw_end = self.lower_value(end)?;

        if descending {
            // Descending: counter starts at the high element and the header
            // gates on `counter >= start`.  `bound` holds `start`.  The
            // counter/bound init computes the user's range bounds, so it
            // carries the for-loop statement span (`push_instr`) — gdb steps
            // onto the for line for the loop setup.
            self.push_instr(Instr::Move {
                dest: bound,
                src: raw_start,
            });
            if inclusive {
                // `a..=b` reversed starts at `b`.
                self.push_instr(Instr::Move {
                    dest: counter,
                    src: raw_end,
                });
            } else {
                // `a..b` reversed starts at `b - 1` (checked; trap on
                // underflow so `a..MIN` fails closed rather than wrapping).
                //
                // #1948 — an EMPTY exclusive descending range (`raw_start >=
                // raw_end`, e.g. `(0u32..0).rev()`, `(i32::MIN..i32::MIN).rev()`,
                // or the a>b shapes `(5u32..0).rev()` / `(0i32..i32::MIN).rev()`)
                // must still yield zero iterations rather than trap. The `b - 1`
                // decrement below is only meaningful once a real high element
                // exists to start from; for an empty range there is no such
                // element, and when `raw_end` sits at the counter type's
                // representable minimum, `raw_end - 1` unconditionally
                // underflows/overflows the counter width in exactly this case —
                // independent of whether the loop itself has any iterations to
                // run.
                //
                // Gate the decrement on emptiness first: an exclusive range is
                // empty iff `raw_start >= raw_end`, not merely `raw_start ==
                // raw_end` (equality only catches the boundary case; `a > b`
                // is just as empty and can land on the same underflowing `b -
                // 1`). Skip the header/body cycle entirely (`Goto exit_bb`
                // directly) without ever computing `b - 1` — this is NOT the
                // same as routing `counter` to a value the header would
                // independently reject, because the header tests `counter >=
                // bound` (`bound == raw_start`) and any candidate `counter`
                // equal to `raw_start` trivially SATISFIES that predicate
                // (entering the loop once), while any value strictly less than
                // `raw_start` needs its own underflow-safe construction —
                // there is no single representable sentinel that solves this
                // for every width, so bypassing the header outright is the
                // correct fix, not an easier substitute for one. A non-empty
                // range takes the existing decrement-with-trap path unchanged,
                // still routed through `pre_header_bb` → `header_bb` as before.
                let is_empty = self.alloc_local(ResolvedTy::Bool);
                // Same signedness-keyed predicate selection as the header
                // comparison below (`header_pred`): an unsigned counter needs
                // `UnsignedGreaterEq` so a high-bit-set bound still compares
                // correctly, mirroring why the header itself doesn't use a
                // single signed/unsigned-agnostic predicate.
                let empty_pred = match counter_signedness {
                    IntSignedness::Signed => CmpPred::SignedGreaterEq,
                    IntSignedness::Unsigned => CmpPred::UnsignedGreaterEq,
                };
                self.push_instr(Instr::IntCmp {
                    dest: is_empty,
                    pred: empty_pred,
                    lhs: raw_start,
                    rhs: raw_end,
                });
                let nonempty_bb = self.alloc_block();
                let pre_header_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: is_empty,
                    then_target: exit_bb,
                    else_target: nonempty_bb,
                });

                self.start_block(nonempty_bb);
                let one = self.alloc_local(counter_ty.clone());
                self.push_instr(Instr::ConstI64 {
                    dest: one,
                    value: 1,
                });
                let overflow_flag = self.alloc_local(ResolvedTy::Bool);
                self.push_instr(Instr::IntArithChecked {
                    op: IntArithOp::Sub,
                    signed: counter_signedness,
                    dest: counter,
                    lhs: raw_end,
                    rhs: one,
                    overflow_flag,
                });
                let trap_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: overflow_flag,
                    then_target: trap_bb,
                    else_target: pre_header_bb,
                });
                self.start_block(trap_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                });

                self.start_block(pre_header_bb);
            }
        } else {
            // Ascending: counter starts at `start`; `bound` is the exclusive
            // upper bound.  The counter/bound init computes the user's range
            // bounds, so it carries the for-loop statement span (`push_instr`).
            self.push_instr(Instr::Move {
                dest: counter,
                src: raw_start,
            });
            // Exclusive (`a..b`)  → bound = raw_end (simple move).
            // Inclusive (`a..=b`) → bound = raw_end + 1 (checked; trap on
            //                       overflow so `a..=i64::MAX` fails closed).
            if inclusive {
                let one = self.alloc_local(counter_ty.clone());
                self.push_instr(Instr::ConstI64 {
                    dest: one,
                    value: 1,
                });
                let overflow_flag = self.alloc_local(ResolvedTy::Bool);
                self.push_instr(Instr::IntArithChecked {
                    op: IntArithOp::Add,
                    signed: counter_signedness,
                    dest: bound,
                    lhs: raw_end,
                    rhs: one,
                    overflow_flag,
                });
                // On overflow: trap.  On success: fall through to header.
                let trap_bb = self.alloc_block();
                let pre_header_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: overflow_flag,
                    then_target: trap_bb,
                    else_target: pre_header_bb,
                });
                self.start_block(trap_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                });
                // Continue building in pre_header_bb; the loop header is
                // allocated below and we Goto it from here.
                self.start_block(pre_header_bb);
            } else {
                self.push_instr(Instr::Move {
                    dest: bound,
                    src: raw_end,
                });
            }
        }

        // Jump from entry (or post-bound-adjust-overflow-check) to the header.
        self.finish_current_block(Terminator::Goto { target: header_bb });

        // Header: ascending tests `counter < bound`; descending tests
        // `counter >= bound` (bound == start).  Either way, false → exit.
        self.start_block(header_bb);
        let cond = self.alloc_local(ResolvedTy::Bool);
        // Select ordering predicate by both direction AND signedness so that
        // unsigned ranges with high-bit-set bounds (e.g. crossing the sign
        // bit) compare correctly.  Signed ICmp on an unsigned counter treats
        // high-bit values as negative, producing 0 iterations for ranges such
        // as `0x7FFF_FFFF_FFFF_FFFEu64 .. 0x8000_0000_0000_0001u64`.
        let header_pred = match (descending, counter_signedness) {
            (true, IntSignedness::Signed) => CmpPred::SignedGreaterEq,
            (true, IntSignedness::Unsigned) => CmpPred::UnsignedGreaterEq,
            (false, IntSignedness::Signed) => CmpPred::SignedLess,
            (false, IntSignedness::Unsigned) => CmpPred::UnsignedLess,
        };
        self.push_instr(Instr::IntCmp {
            dest: cond,
            pred: header_pred,
            lhs: counter,
            rhs: bound,
        });
        self.finish_current_block(Terminator::Branch {
            cond,
            then_target: body_bb,
            else_target: exit_bb,
        });

        // Body: expose binding → counter, lower body statements, then
        // increment counter and loop back to the header.
        self.start_block(body_bb);

        // Register the loop variable so body BindingRef nodes resolve to
        // `counter`.  The Bind statement is required by MIR move-check
        // bookkeeping; it carries a sentinel SiteId(0) because the loop
        // variable has no checker-recorded call site.
        self.binding_locals.insert(binding.id, counter);
        self.statements.push(MirStatement::Bind {
            binding: binding.id,
            name: binding.name.clone(),
            site: hew_hir::SiteId(0),
            ty: counter_ty.clone(),
        });
        self.record_binding_scope(binding.id);

        self.active_scopes.push(body.scope);
        // continue → inc_bb (advances the counter, then re-checks the header);
        // break → exit. Depth captured before the body scope push so the
        // in-loop defer window covers body.scope and any nested block scopes.
        let loop_scope_depth = self.active_scopes.len() - 1;
        self.loop_stack.push(LoopFrame {
            label: label.map(str::to_string),
            continue_target: inc_bb,
            exit_target: exit_bb,
            scope_depth: loop_scope_depth,
            body_scope: body.scope,
        });
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        // Release generators declared in the loop body before the increment +
        // back-edge (per-iteration `hew_gen_coro_destroy`; see
        // `emit_scope_generator_drops`).
        self.emit_scope_generator_drops(body.scope);
        // #1949 — release sole-owner `for x in …` cursors (`VecIter`) declared
        // directly in this loop body before the back-edge, the cursor analogue of
        // the generator release above (see `emit_scope_vec_iter_drops`).
        self.emit_scope_vec_iter_drops(body.scope);
        self.emit_scope_stream_drops(body.scope);
        // Record body→inc as the per-iteration back-edge for body-scope
        // bindings. body.scope closes here (active_scopes.pop next), so any
        // heap-owning let-binding declared inside the body must be released
        // before fall-through into the inc/header re-evaluation overwrites it
        // on the next iteration. The counter binding itself lives in the
        // outer scope (registered before active_scopes.push(body.scope)) and
        // is i64 — no drop — so the back-edge plan never touches it.
        self.loop_back_edge_blocks
            .insert(self.current_block_id, body.scope);
        self.active_scopes.pop();
        self.loop_stack.pop();
        // Body fall-through → increment block.
        self.finish_current_block(Terminator::Goto { target: inc_bb });

        // Advance the counter by `step` (checked).
        //
        // Ascending: `counter += step`.  Overflow past the type max traps as
        // IntegerOverflow rather than silently wrapping and running forever
        // — fail-closed per the reliability tenet.
        //
        // Descending: `counter -= step`.  An underflow past the type min is the
        // natural loop terminus for a descending range that reaches its low
        // bound (e.g. `(0u32..5).rev()` decrementing past `0`), so it branches
        // to the loop exit instead of trapping.  The header's `counter >= start`
        // guard handles every non-underflowing terminus.
        self.start_block(inc_bb);
        // The counter advance is user-visible loop mechanics (the per-iteration
        // step), so it carries the for-loop statement span (`push_instr`).
        let advance_flag = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntArithChecked {
            op: if descending {
                IntArithOp::Sub
            } else {
                IntArithOp::Add
            },
            signed: counter_signedness,
            dest: counter,
            lhs: counter,
            rhs: step_val,
            overflow_flag: advance_flag,
        });
        if descending {
            // Underflow on the descending decrement → loop is exhausted; exit.
            self.finish_current_block(Terminator::Branch {
                cond: advance_flag,
                then_target: exit_bb,
                else_target: header_bb,
            });
        } else {
            let overflow_trap = self.alloc_block();
            // On overflow → trap; otherwise loop back to the header (re-check bound).
            self.finish_current_block(Terminator::Branch {
                cond: advance_flag,
                then_target: overflow_trap,
                else_target: header_bb,
            });
            self.start_block(overflow_trap);
            self.finish_current_block(Terminator::Trap {
                kind: TrapKind::IntegerOverflow,
            });
        }

        // Exit: subsequent lowering continues here.
        self.start_block(exit_bb);
        None
    }

    /// Lower a bare `loop { body }` to a two-block CFG:
    ///
    /// ```text
    /// entry_bb (current):
    ///   Goto body_bb
    ///
    /// body_bb:
    ///   lower(body statements)
    ///   Goto body_bb             ← unconditional back-edge (also `continue`)
    ///
    /// exit_bb:
    ///   (only reachable via `break`; subsequent lowering continues here)
    /// ```
    ///
    /// A bare `loop` has no condition: the sole way out is `break`, so
    /// `exit_bb` has no predecessor unless the body contains one. We start it
    /// unconditionally anyway (Risk 4) so the post-loop cursor always has a
    /// home and `finalize_blocks` can drop it if it stays empty/unreachable.
    /// `continue` targets `body_bb` directly — there is no header to re-check.
    ///
    /// Always returns `None`: `loop {}` is `Unit`-typed at the MIR boundary
    /// (a `break value` carries its operand for side effects only in this
    /// slice; loop-as-expression is out of scope — see the plan).
    pub(crate) fn lower_loop(
        &mut self,
        label: Option<&str>,
        body: &hew_hir::HirBlock,
    ) -> Option<Place> {
        let body_bb = self.alloc_block();
        let exit_bb = self.alloc_block();

        // Entry → body.
        self.finish_current_block(Terminator::Goto { target: body_bb });

        // Body: lower statements then loop back unconditionally.
        self.start_block(body_bb);
        // continue → body_bb (re-enter the top); break → exit.
        let loop_scope_depth = self.active_scopes.len();
        self.loop_stack.push(LoopFrame {
            label: label.map(str::to_string),
            continue_target: body_bb,
            exit_target: exit_bb,
            scope_depth: loop_scope_depth,
            body_scope: body.scope,
        });
        self.active_scopes.push(body.scope);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        // Release generators declared in the loop body before the back-edge
        // (per-iteration `hew_gen_coro_destroy`; see `emit_scope_generator_drops`).
        self.emit_scope_generator_drops(body.scope);
        // #1949 — release sole-owner `for x in …` cursors (`VecIter`) declared
        // directly in this loop body before the back-edge, the cursor analogue of
        // the generator release above (see `emit_scope_vec_iter_drops`).
        self.emit_scope_vec_iter_drops(body.scope);
        self.emit_scope_stream_drops(body.scope);
        // Record this block as a loop-body back-edge so `enumerate_exits`
        // populates its `Goto` `DropPlan` with per-iteration releases for
        // heap-owning bindings declared in `body.scope`. `loop { ... }` has
        // no separate header — `body_bb` is both entry and re-entry — so
        // this back-edge is the one place per-iteration drops can fire.
        self.loop_back_edge_blocks
            .insert(self.current_block_id, body.scope);
        self.active_scopes.pop();
        self.loop_stack.pop();
        self.finish_current_block(Terminator::Goto { target: body_bb });

        // Exit: only reached via `break`. Always started so the post-loop
        // cursor has a home (Risk 4).
        self.start_block(exit_bb);
        None
    }
    ///
    /// CFG shape:
    ///
    /// ```text
    /// entry_bb (current):
    ///   result_place = false          // pessimistic default
    ///   lhs_place = lower(lhs)
    ///   Branch { cond: lhs_place, then: rhs_bb, else: join_bb }
    ///
    /// rhs_bb:
    ///   rhs_place = lower(rhs)
    ///   Move { dest: result_place, src: rhs_place }
    ///   Goto join_bb
    ///
    /// join_bb:
    ///   -- result_place holds the final bool --
    /// ```
    ///
    /// The rhs block is only entered when lhs is true, so rhs side effects
    /// are correctly guarded. On the false path, `result_place` retains the
    /// `false` constant written in the entry block.
    pub(crate) fn lower_logical_and(
        &mut self,
        lhs_expr: &HirExpr,
        rhs_expr: &HirExpr,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let result_place = self.alloc_local(result_ty.clone());
        // Write `false` as the pessimistic default (the join block reads
        // result_place, and the else path never writes to it).
        self.push_instr(Instr::ConstI64 {
            dest: result_place,
            value: 0,
        });

        let lhs_place = self.lower_value(lhs_expr)?;

        let rhs_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        self.finish_current_block(Terminator::Branch {
            cond: lhs_place,
            then_target: rhs_bb,
            else_target: join_bb,
        });

        // rhs_bb: lhs was true, evaluate rhs and move into result.
        self.start_block(rhs_bb);
        if let Some(rhs_place) = self.lower_value(rhs_expr) {
            self.push_instr(Instr::Move {
                dest: result_place,
                src: rhs_place,
            });
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        self.start_block(join_bb);
        Some(result_place)
    }

    /// Lower `lhs || rhs` with short-circuit semantics.
    ///
    /// CFG shape:
    ///
    /// ```text
    /// entry_bb (current):
    ///   result_place = true           // optimistic default
    ///   lhs_place = lower(lhs)
    ///   Branch { cond: lhs_place, then: join_bb, else: rhs_bb }
    ///
    /// rhs_bb:
    ///   rhs_place = lower(rhs)
    ///   Move { dest: result_place, src: rhs_place }
    ///   Goto join_bb
    ///
    /// join_bb:
    ///   -- result_place holds the final bool --
    /// ```
    ///
    /// The rhs block is only entered when lhs is false, so rhs side effects
    /// are correctly guarded. On the true path, `result_place` retains the
    /// `true` constant written in the entry block.
    pub(crate) fn lower_logical_or(
        &mut self,
        lhs_expr: &HirExpr,
        rhs_expr: &HirExpr,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let result_place = self.alloc_local(result_ty.clone());
        // Write `true` as the optimistic default (the then path never writes
        // to result_place; the else path writes the rhs value into it).
        self.push_instr(Instr::ConstI64 {
            dest: result_place,
            value: 1,
        });

        let lhs_place = self.lower_value(lhs_expr)?;

        let rhs_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        self.finish_current_block(Terminator::Branch {
            cond: lhs_place,
            then_target: join_bb,
            else_target: rhs_bb,
        });

        // rhs_bb: lhs was false, evaluate rhs and move into result.
        self.start_block(rhs_bb);
        if let Some(rhs_place) = self.lower_value(rhs_expr) {
            self.push_instr(Instr::Move {
                dest: result_place,
                src: rhs_place,
            });
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        self.start_block(join_bb);
        Some(result_place)
    }
}
