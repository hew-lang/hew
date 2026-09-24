//! Resource lifecycle admission and close/consume discipline.

use super::*;
use hew_parser::ast::Ident;

/// Whether an inherent impl block's receiver names `declaration`.
///
/// `HirImplBlock::self_type_name` is the checker's resolved receiver spelling.
/// The comparison resolves it through the identity table, which carries one
/// canonical render per declaration, rather than matching text.
pub(super) fn impl_receiver_is(
    identity: &hew_types::IdentityView,
    impl_block: &crate::node::HirImplBlock,
    declaration: &hew_types::DefId,
) -> bool {
    impl_block.self_type_name == declaration.full_path()
        || identity
            .declaration_by_path(&impl_block.self_type_name)
            .is_some_and(|resolved| resolved == declaration)
}

/// Admit field-bearing resource lifecycles while exact HIR declaration and
/// body identities are simultaneously available. No downstream stage may
/// reconstruct a close declaration from a record-layout or symbol spelling.
pub(super) fn admit_resource_record_lifecycles(
    items: &[HirItem],
    identity: &hew_types::IdentityView,
    resource_close_discipline_failures: &HashSet<hew_types::DefId>,
    type_classes: &mut crate::value_class::TypeClassTable,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    for decl in items.iter().filter_map(|item| match item {
        HirItem::TypeDecl(decl)
            if !decl.is_opaque
                && decl.marker == ResourceMarker::Resource
                && decl.kind == HirTypeDeclKind::Struct
                && !resource_close_discipline_failures.contains(&decl.declaration) =>
        {
            Some(decl)
        }
        _ => None,
    }) {
        let exact_owner = decl.declaration.full_path();
        let close_methods: Vec<_> = items
            .iter()
            .filter_map(|item| match item {
                HirItem::Impl(impl_block)
                    if impl_block.trait_name.is_none()
                        && impl_receiver_is(identity, impl_block, &decl.declaration) =>
                {
                    Some(impl_block)
                }
                _ => None,
            })
            .flat_map(|impl_block| {
                impl_block
                    .method_names
                    .iter()
                    .zip(&impl_block.method_ids)
                    .zip(&impl_block.method_symbols)
            })
            .filter_map(|((name, declaration), symbol)| {
                (name == "close")
                    .then(|| declaration.as_ref().map(|id| (id.clone(), symbol.clone())))
                    .flatten()
            })
            .collect();

        let [(close_declaration, close_symbol)] = close_methods.as_slice() else {
            diagnostics.push(resource_lifecycle_boundary_diagnostic(
                exact_owner,
                format!(
                    "resource record requires one exact inherent close declaration; found {}",
                    close_methods.len()
                ),
                &decl.span,
                "resource lifecycle identity did not survive HIR lowering",
            ));
            continue;
        };

        let close_bodies: Vec<_> = items
            .iter()
            .filter_map(|item| match item {
                HirItem::Function(function)
                    if function.declaration == *close_declaration
                        && function.name == *close_symbol =>
                {
                    Some(function)
                }
                _ => None,
            })
            .collect();
        let [close_body] = close_bodies.as_slice() else {
            diagnostics.push(resource_lifecycle_boundary_diagnostic(
                close_declaration.full_path(),
                format!(
                    "resource record close requires one exact emitted body; found {}",
                    close_bodies.len()
                ),
                &decl.span,
                "resource lifecycle close body did not survive HIR lowering",
            ));
            continue;
        };
        if close_body.return_ty != ResolvedTy::Unit || close_body.params.len() != 1 {
            diagnostics.push(resource_lifecycle_boundary_diagnostic(
                close_declaration.full_path(),
                "resource record close body must take one receiver and return unit".to_string(),
                &decl.span,
                "resource lifecycle close body has an inadmissible signature",
            ));
            continue;
        }

        let lifecycle = crate::ResourceRecordLifecycle {
            resource_declaration: decl.declaration.clone(),
            close_declaration: close_declaration.clone(),
            close_symbol: close_symbol.clone(),
        };
        if type_classes
            .admit_resource_record_lifecycle(lifecycle)
            .is_err()
        {
            diagnostics.push(resource_lifecycle_boundary_diagnostic(
                exact_owner,
                "duplicate exact resource-record lifecycle admission".to_string(),
                &decl.span,
                "one qualified resource may have exactly one automatic lifecycle authority",
            ));
        }
    }
}

/// Render an opaque-resource lifecycle conflict for a diagnostic message.
/// A hand-written arm per variant instead of `{:?}` keeps the surfaced text
/// stable and readable if the enum's field names or ordering change.
pub(super) fn describe_opaque_resource_lifecycle_conflict(
    kind: &hew_types::OpaqueResourceLifecycleConflictKind,
) -> String {
    use hew_types::OpaqueResourceLifecycleConflictKind as Kind;
    match kind {
        Kind::ProducerResultMismatch { actual } => {
            format!("producer result type does not match the resource type: found `{actual}`")
        }
        Kind::ReleaseDeclarationMissing => {
            "no matching release declaration was found for the producer".to_string()
        }
        Kind::CloseDeclarationMissing => {
            "no inherent `close` declaration was found for the resource".to_string()
        }
        Kind::ReleaseSignatureMismatch { detail } => {
            format!("release declaration signature does not match: {detail}")
        }
        Kind::MultipleProducerLifecycle {
            established,
            conflicting,
        } => format!(
            "resource already has an established lifecycle from `{established}`; \
             `{conflicting}` conflicts with it"
        ),
    }
}

pub(super) fn resource_lifecycle_boundary_diagnostic(
    name: &str,
    reason: String,
    span: &Span,
    note: &str,
) -> HirDiagnostic {
    HirDiagnostic::new(
        HirDiagnosticKind::CheckerBoundaryViolation {
            name: name.to_string(),
            reason,
        },
        span.clone(),
        note,
    )
}

/// Admit checker-derived opaque lifecycles only after every declaration,
/// extern, and inherent close body has a resolved HIR identity.
pub(super) fn validate_opaque_resource_close(
    items: &[HirItem],
    candidate: &hew_types::OpaqueResourceLifecycleCandidate,
) -> Result<String, String> {
    let close_functions: Vec<_> = items
        .iter()
        .filter_map(|item| match item {
            HirItem::Function(function) if function.declaration == candidate.close_declaration => {
                Some(function)
            }
            _ => None,
        })
        .collect();
    let release_externs: Vec<_> = items
        .iter()
        .filter_map(|item| match item {
            HirItem::ExternFn(extern_fn)
                if extern_fn.declaration == candidate.release_declaration =>
            {
                Some(extern_fn)
            }
            _ => None,
        })
        .collect();

    match (close_functions.as_slice(), release_externs.as_slice()) {
        ([close], [release]) if close.return_ty == ResolvedTy::Unit && close.params.len() == 1 => {
            let receiver = close.params[0].id;
            let exact_wrapper = is_exact_release_forwarding_wrapper(
                &close.body,
                &candidate.release_declaration,
                &candidate.release_symbol,
                candidate.release_param_index,
                receiver,
            );
            exact_wrapper.then(|| close.name.clone()).ok_or_else(|| {
                "canonical close must be one unconditional straight-line exact release call"
                    .to_string()
            })
        }
        (closes, releases) => Err(format!(
            "expected one unit close and one owner-module release declaration; found {} close(s), {} release declaration(s)",
            closes.len(),
            releases.len()
        )),
    }
}

pub(super) fn admit_opaque_resource_lifecycles(
    items: &[HirItem],
    graph: &hew_types::OpaqueResourceCandidateGraph,
    type_classes: &mut crate::value_class::TypeClassTable,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    for conflict in &graph.conflicts {
        diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::OpaqueResourceLifecycleConflict {
                resource_type: conflict.resource_type.clone(),
                producer: conflict.producer_symbol.clone(),
                release: conflict.release_symbol.clone(),
                detail: describe_opaque_resource_lifecycle_conflict(&conflict.kind),
            },
            0..0,
            "conflicting generated/source lifecycle facts leave no automatic close authority",
        ));
    }

    for candidate in graph.candidates.values() {
        let matching_decls: Vec<_> = items
            .iter()
            .filter_map(|item| match item {
                HirItem::TypeDecl(decl) if decl.declaration == candidate.resource_declaration => {
                    Some(decl)
                }
                _ => None,
            })
            .collect();
        let [decl] = matching_decls.as_slice() else {
            diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::OpaqueResourceCloseMismatch {
                    resource_type: candidate.resource_type.clone(),
                    expected_release: candidate.release_symbol.clone(),
                    detail: format!(
                        "expected one exact HIR declaration, found {}",
                        matching_decls.len()
                    ),
                },
                0..0,
                "checker lifecycle identity did not survive HIR declaration lowering",
            ));
            continue;
        };

        if !decl.is_opaque || decl.marker != ResourceMarker::Resource {
            diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CloseableOpaqueMustBeResource {
                    resource_type: candidate.resource_type.clone(),
                    producer: candidate
                        .producer_symbols
                        .iter()
                        .next()
                        .cloned()
                        .unwrap_or_default(),
                    release: candidate.release_symbol.clone(),
                },
                decl.span.clone(),
                "an independently owned opaque result must declare `#[resource]` and one inherent `close`",
            ));
            continue;
        }

        let detail = validate_opaque_resource_close(items, candidate);

        match detail {
            Ok(close_symbol) => {
                let lifecycle = crate::OpaqueResourceLifecycle {
                    resource_declaration: candidate.resource_declaration.clone(),
                    close_declaration: candidate.close_declaration.clone(),
                    release_declaration: candidate.release_declaration.clone(),
                    close_symbol,
                    release_symbol: candidate.release_symbol.clone(),
                    discharge_depth: candidate.discharge_depth,
                    producer_declarations: candidate.producer_declarations.clone(),
                    producer_symbols: candidate.producer_symbols.clone(),
                    producer_modules: candidate.producer_modules.clone(),
                };
                if type_classes
                    .admit_opaque_resource_lifecycle(lifecycle)
                    .is_err()
                {
                    diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::OpaqueResourceCloseMismatch {
                            resource_type: candidate.resource_type.clone(),
                            expected_release: candidate.release_symbol.clone(),
                            detail: "duplicate exact lifecycle admission".to_string(),
                        },
                        decl.span.clone(),
                        "one qualified resource may have exactly one automatic lifecycle authority",
                    ));
                }
            }
            Err(detail) => diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::OpaqueResourceCloseMismatch {
                    resource_type: candidate.resource_type.clone(),
                    expected_release: candidate.release_symbol.clone(),
                    detail: detail.clone(),
                },
                decl.span.clone(),
                format!("the canonical close does not match the checker-admitted consuming release: {detail}"),
            )),
        }
    }
}

/// Admit the ordinary authored `#[resource] #[opaque]` form when it has no
/// checker-discovered producer/release pair.  The candidate graph deliberately
/// covers wrappers around a known external producer; it must not be mistaken
/// for the authority on whether a source-owned resource has an automatic
/// close.  A direct actor-state field can be constructed by arbitrary FFI or
/// passed in from its owner and still needs its exact inherent `close` body at
/// teardown.
///
/// This is declaration-keyed throughout.  In particular, a user `MonitorRef`
/// or `Stream` can never inherit the standard-library lifecycle merely from
/// its leaf spelling.
#[expect(
    clippy::too_many_lines,
    reason = "validates one source-owned opaque lifecycle end-to-end: declaration, exact impl method, emitted body, and registry admission"
)]
pub(super) fn admit_declared_opaque_resource_lifecycles(
    items: &[HirItem],
    graph: &hew_types::OpaqueResourceCandidateGraph,
    identity: &hew_types::IdentityView,
    type_classes: &mut crate::value_class::TypeClassTable,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    for decl in items.iter().filter_map(|item| match item {
        HirItem::TypeDecl(decl) if decl.is_opaque && decl.marker == ResourceMarker::Resource => {
            Some(decl)
        }
        _ => None,
    }) {
        if decl.kind == HirTypeDeclKind::Enum {
            // Every sibling rejection under this filter emits a diagnostic;
            // a variant-bearing `#[resource]` opaque declaration has no
            // single-representation lifecycle boundary to admit and must not
            // fall out of the iterator silently.
            diagnostics.push(resource_lifecycle_boundary_diagnostic(
                decl.declaration.full_path(),
                "opaque #[resource] declarations with variants have no single-representation \
                 lifecycle boundary to admit"
                    .to_string(),
                &decl.span,
                "a variant-bearing opaque resource cannot receive an automatic close",
            ));
            continue;
        }
        // The authored fallback exists only when the checker discovered no
        // producer/release contract for this exact declaration. Once a
        // candidate or conflict exists, the checker graph owns the lifecycle
        // decision; a failed validation must not be converted into an
        // automatic close by this less-informed fallback.
        let checker_owns_lifecycle_decision = graph.candidates.contains_key(&decl.declaration)
            || graph
                .conflicts
                .iter()
                .any(|conflict| conflict.resource_type == decl.declaration.full_path());
        if checker_owns_lifecycle_decision {
            continue;
        }
        if type_classes
            .lifecycle_registry()
            .opaque_resource(&decl.declaration)
            .is_some()
        {
            continue;
        }
        let exact_owner = decl.declaration.full_path();
        let close_methods: Vec<_> = items
            .iter()
            .filter_map(|item| match item {
                HirItem::Impl(impl_block)
                    if impl_block.trait_name.is_none()
                        && impl_receiver_is(identity, impl_block, &decl.declaration) =>
                {
                    Some(impl_block)
                }
                _ => None,
            })
            .flat_map(|impl_block| {
                impl_block
                    .method_names
                    .iter()
                    .zip(&impl_block.method_ids)
                    .zip(&impl_block.method_symbols)
            })
            .filter_map(|((name, declaration), symbol)| {
                (name == "close")
                    .then(|| declaration.as_ref().map(|id| (id.clone(), symbol.clone())))
                    .flatten()
            })
            .collect();
        let [(close_declaration, close_symbol)] = close_methods.as_slice() else {
            diagnostics.push(resource_lifecycle_boundary_diagnostic(
                exact_owner,
                format!(
                    "opaque resource requires one exact inherent close declaration; found {}",
                    close_methods.len()
                ),
                &decl.span,
                "opaque resource lifecycle identity did not survive HIR lowering",
            ));
            continue;
        };
        let close_bodies: Vec<_> = items
            .iter()
            .filter_map(|item| match item {
                HirItem::Function(function)
                    if function.declaration == *close_declaration
                        && function.name == *close_symbol =>
                {
                    Some(function)
                }
                _ => None,
            })
            .collect();
        let [close_body] = close_bodies.as_slice() else {
            diagnostics.push(resource_lifecycle_boundary_diagnostic(
                close_declaration.full_path(),
                format!(
                    "opaque resource close requires one exact emitted body; found {}",
                    close_bodies.len()
                ),
                &decl.span,
                "opaque resource close body did not survive HIR lowering",
            ));
            continue;
        };
        if close_body.return_ty != ResolvedTy::Unit || close_body.params.len() != 1 {
            diagnostics.push(resource_lifecycle_boundary_diagnostic(
                close_declaration.full_path(),
                "opaque resource close body must take one receiver and return unit".to_string(),
                &decl.span,
                "opaque resource close body has an inadmissible signature",
            ));
            continue;
        }
        let lifecycle = crate::OpaqueResourceLifecycle {
            resource_declaration: decl.declaration.clone(),
            close_declaration: close_declaration.clone(),
            // There is no separately declared release ABI in this authored
            // form.  The generated close body is the exact lifecycle endpoint;
            // the fields are retained for the shared lifecycle carrier.
            release_declaration: close_declaration.clone(),
            close_symbol: close_symbol.clone(),
            release_symbol: close_symbol.clone(),
            discharge_depth: hew_types::ffi_contracts::ReleaseDischargeDepth::Shallow,
            producer_declarations: std::collections::BTreeSet::new(),
            producer_symbols: std::collections::BTreeSet::new(),
            producer_modules: std::collections::BTreeSet::new(),
        };
        if type_classes
            .admit_opaque_resource_lifecycle(lifecycle)
            .is_err()
        {
            diagnostics.push(resource_lifecycle_boundary_diagnostic(
                exact_owner,
                "duplicate exact opaque resource lifecycle admission".to_string(),
                &decl.span,
                "one qualified resource may have exactly one automatic lifecycle authority",
            ));
        }
    }
}

pub(super) fn is_exact_release_forwarding_wrapper(
    block: &HirBlock,
    release_declaration: &hew_types::DefId,
    release_symbol: &str,
    release_param_index: usize,
    receiver: BindingId,
) -> bool {
    let expression = match (block.statements.as_slice(), block.tail.as_deref()) {
        ([statement], None) => {
            let HirStmtKind::Expr(expression) = &statement.kind else {
                return false;
            };
            expression
        }
        ([], Some(expression)) => expression,
        _ => return false,
    };
    is_exact_release_forwarding_expr(
        expression,
        release_declaration,
        release_symbol,
        release_param_index,
        receiver,
    )
}

pub(super) fn is_exact_release_forwarding_expr(
    expr: &HirExpr,
    release_declaration: &hew_types::DefId,
    release_symbol: &str,
    release_param_index: usize,
    receiver: BindingId,
) -> bool {
    match &expr.kind {
        HirExprKind::Call { target, args, .. } => {
            matches!(
                target,
                hew_types::CallTarget::Extern {
                    declaration, endpoint, ..
                }
                    if declaration == release_declaration && endpoint == release_symbol
            ) && args.get(release_param_index).is_some_and(|arg| {
                matches!(
                    arg.kind,
                    HirExprKind::BindingRef {
                        resolved: ResolvedRef::Binding(binding),
                        ..
                    } if binding == receiver
                )
            }) && args.iter().enumerate().all(|(index, arg)| {
                index == release_param_index
                    || matches!(
                        arg.kind,
                        HirExprKind::Literal(_) | HirExprKind::BindingRef { .. }
                    )
            })
        }
        HirExprKind::Block(block) => is_exact_release_forwarding_wrapper(
            block,
            release_declaration,
            release_symbol,
            release_param_index,
            receiver,
        ),
        _ => false,
    }
}

impl LowerCtx {
    /// Query checker signatures by their canonical receiver identity.
    pub(super) fn inherent_close_signature(
        &self,
        declaration: &hew_types::DefId,
    ) -> Option<&hew_types::FnSig> {
        self.fn_sigs.values().find(|sig| {
            sig.impl_method.as_ref().is_some_and(|origin| {
                origin.is_inherent
                    && origin.receiver.as_ref() == Some(declaration)
                    && origin.name == "close"
            })
        })
    }

    /// W3.030 Stage 1 — three layered checks on `#[resource]` close:
    ///
    ///   1. Inline `TypeBodyItem::Method` named `close` is rejected
    ///      unconditionally (Q-α-B): the inline form is a silent
    ///      trap today (E8 — body not lowered, drop dispatcher
    ///      emits an unresolvable symbol). Emit
    ///      `ResourceCloseSourceUnsupported`. Short-circuit further
    ///      checks for this type — exactly one named diagnostic
    ///      per `close` slot (review R-4 determinism contract).
    ///
    ///   2. Otherwise look for an inherent-impl `<T>::close`
    ///      (published on the checker signature). Presence satisfies the broadened
    ///      `ResourceMissingClose` check (Q-α-B); a non-unit return
    ///      type triggers `ResourceCloseMustReturnUnit` (Q-β-C).
    ///
    ///   3. Otherwise no `close` is reachable from any surface;
    ///      emit `ResourceMissingClose` as before.
    pub(super) fn check_resource_close_discipline(
        &mut self,
        decl: &TypeDecl,
        span: &Span,
        declaration: &hew_types::DefId,
    ) {
        let inline_close = decl
            .body
            .iter()
            .any(|item| matches!(item, TypeBodyItem::Method(m) if m.name == Ident::new("close")));
        if inline_close {
            self.resource_close_discipline_failures
                .insert(declaration.clone());
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::ResourceCloseSourceUnsupported {
                    name: decl.name.to_string(),
                },
                span.clone(),
                "`#[resource]` types must declare `close` in a sibling \
                 inherent-impl block (`impl T { fn close(consume self) { ... } }`); \
                 the inline `type T { fn close(consume self) ... }` \
                 surface is not lowered in v0.5 and would silently fail \
                 link-time drop dispatch",
            ));
            return;
        }
        if let Some(sig) = self.inherent_close_signature(declaration) {
            if sig.return_type != hew_types::Ty::Unit {
                let display = sig.return_type.to_string();
                let decl_span = sig
                    .impl_method
                    .as_ref()
                    .expect("inherent provenance")
                    .span
                    .clone();
                self.resource_close_discipline_failures
                    .insert(declaration.clone());
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::ResourceCloseMustReturnUnit {
                        name: decl.name.to_string(),
                        return_ty: display.clone(),
                    },
                    decl_span,
                    format!(
                        "`#[resource]` type `{}` declares `close` returning \
                         `{display}`; the implicit drop contract dispatches \
                         `close` on every scope-exit path and only unit return \
                         is admitted in v0.5 — compose fallible cleanup via \
                         `defer` instead",
                        decl.name
                    ),
                ));
            }
            return;
        }
        self.resource_close_discipline_failures
            .insert(declaration.clone());
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::ResourceMissingClose {
                name: decl.name.to_string(),
            },
            span.clone(),
            "`#[resource]` type must declare `fn close(consume self) { ... }` in a \
             sibling inherent-impl block (`impl T { fn close(consume self) { ... } }`); \
             the implicit drop contract dispatches to this method on every \
             scope-exit path",
        ));
    }

    /// Enforce the `#[linear]` consuming-method discipline.
    ///
    /// A `#[linear]` type must declare at least one `consume self` method so
    /// that some exit path can exhaust a binding of the type (the
    /// `MirCheck::MustConsume` enforcement target). The supported surface is a
    /// sibling inherent-impl block (`impl T { fn commit(consume self) { … } }`)
    /// — the form that lowers to a callable consume target.
    ///
    ///   1. A type-body `consume self` method (`type T { fn m(consume self)
    ///      … }`) is rejected: it is not lowered to a callable symbol, so a call
    ///      raises `IndirectCallUnsupported` at the call site, leaving the type
    ///      unusable. Fail-close here at the declaration with a directive to the
    ///      supported surface, mirroring the `#[resource]` inline-`close`
    ///      rejection.
    ///   2. Otherwise the type must carry a sibling-inherent consuming method
    ///      (published on the checker signature); without one no exit path exhausts
    ///      a binding — `LinearNoConsumingMethods`.
    pub(super) fn check_linear_consume_discipline(
        &mut self,
        decl: &TypeDecl,
        span: &Span,
        declaration: &hew_types::DefId,
    ) {
        let has_inline_consuming = decl.body.iter().any(|item| {
            matches!(item, TypeBodyItem::Method(m)
                if decl.consuming_methods.iter().any(|n| n == &m.name))
        });
        if has_inline_consuming {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::LinearConsumingMethodSourceUnsupported {
                    name: decl.name.to_string(),
                },
                span.clone(),
                "`#[linear]` types must declare their `consume self` method in a \
                 sibling inherent-impl block (`impl T { fn commit(consume self) \
                 { ... } }`); the inline `type T { fn commit(consume self) ... }` \
                 surface is not lowered to a callable consume target",
            ));
        } else if !self.fn_sigs.values().any(|sig| {
            sig.consumes_receiver
                && sig.impl_method.as_ref().is_some_and(|origin| {
                    origin.is_inherent && origin.receiver.as_ref() == Some(declaration)
                })
        }) {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::LinearNoConsumingMethods {
                    name: decl.name.to_string(),
                },
                span.clone(),
                "`#[linear]` type must declare at least one `consume self` method \
                 in a sibling inherent-impl block; without one no exit path could \
                 exhaust a binding of this type",
            ));
        }
    }
}
