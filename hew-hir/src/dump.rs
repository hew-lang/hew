use std::fmt::Write as _;

use crate::node::{HirExpr, HirExprKind, HirItem, HirModule, HirStmtKind};

#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "single match on HirItem variants; each arm renders one item kind \
              and splitting would obscure per-variant locality"
)]
pub fn dump_hir(module: &HirModule) -> String {
    let mut out = String::new();
    for item in &module.items {
        match item {
            HirItem::Function(func) => {
                writeln!(
                    out,
                    "fn {} {} -> {}",
                    func.id,
                    func.name,
                    func.return_ty.user_facing()
                )
                .expect("write to string");
                for param in &func.params {
                    writeln!(
                        out,
                        "  param {} {}: {}",
                        param.id,
                        param.name,
                        param.ty.user_facing()
                    )
                    .expect("write to string");
                }
                for stmt in &func.body.statements {
                    match &stmt.kind {
                        HirStmtKind::Let(binding, value) => {
                            writeln!(
                                out,
                                "  let {} {}: {}",
                                binding.id,
                                binding.name,
                                binding.ty.user_facing()
                            )
                            .expect("write to string");
                            if let Some(value) = value {
                                dump_expr(&mut out, value, 4);
                            }
                        }
                        HirStmtKind::Expr(expr) => dump_expr(&mut out, expr, 2),
                        HirStmtKind::Return(Some(expr)) => {
                            writeln!(out, "  return").expect("write to string");
                            dump_expr(&mut out, expr, 4);
                        }
                        HirStmtKind::Return(None) => {
                            writeln!(out, "  return unit").expect("write to string");
                        }
                    }
                }
                if let Some(tail) = &func.body.tail {
                    writeln!(out, "  tail").expect("write to string");
                    dump_expr(&mut out, tail, 4);
                }
            }
            HirItem::TypeDecl(decl) => {
                writeln!(
                    out,
                    "type {} {} marker={:?}",
                    decl.id, decl.name, decl.marker
                )
                .expect("write to string");
                for field in &decl.fields {
                    writeln!(out, "  field {}: {}", field.name, field.ty.user_facing())
                        .expect("write to string");
                }
                for method in &decl.consuming_methods {
                    writeln!(out, "  consuming-method {method}").expect("write to string");
                }
            }
            HirItem::Machine(machine) => {
                writeln!(out, "machine {} {}", machine.id, machine.name).expect("write to string");
                for state in &machine.states {
                    writeln!(
                        out,
                        "  state {} entry={} exit={}",
                        state.name, state.has_entry, state.has_exit
                    )
                    .expect("write to string");
                }
                for event in &machine.events {
                    writeln!(out, "  event {}", event.name).expect("write to string");
                }
                for tr in &machine.transitions {
                    writeln!(
                        out,
                        "  transition on {}: {} -> {} self={}",
                        tr.event_name, tr.source_state, tr.target_state, tr.is_self_transition
                    )
                    .expect("write to string");
                }
            }
            HirItem::Record(record) => {
                writeln!(out, "record {} {}", record.id, record.name).expect("write to string");
                for field in &record.fields {
                    writeln!(out, "  field {}: {}", field.name, field.ty.user_facing())
                        .expect("write to string");
                }
            }
            HirItem::Actor(actor) => {
                writeln!(
                    out,
                    "actor {} {} isolated={} max_heap={:?} mailbox={:?} cycle_capable={}",
                    actor.id,
                    actor.name,
                    actor.is_isolated,
                    actor.max_heap_bytes,
                    actor.mailbox_capacity,
                    actor.cycle_capable
                )
                .expect("write to string");
                for field in &actor.state_fields {
                    writeln!(out, "  field {}: {}", field.name, field.ty.user_facing())
                        .expect("write to string");
                }
                if let Some(init) = &actor.init {
                    writeln!(out, "  init params={}", init.params.len()).expect("write to string");
                }
                for rf in &actor.receive_handlers {
                    writeln!(
                        out,
                        "  receive {} params={} -> {} state_guard={:?} every_ns={:?}",
                        rf.name,
                        rf.params.len(),
                        rf.return_ty.user_facing(),
                        rf.state_guard,
                        rf.every_ns
                    )
                    .expect("write to string");
                }
                for m in &actor.methods {
                    writeln!(
                        out,
                        "  method {} params={} -> {}",
                        m.name,
                        m.params.len(),
                        m.return_ty.user_facing()
                    )
                    .expect("write to string");
                }
                for h in &actor.lifecycle_hooks {
                    writeln!(
                        out,
                        "  on({:?}) {} params={} -> {}",
                        h.kind,
                        h.name,
                        h.params.len(),
                        h.return_ty.user_facing()
                    )
                    .expect("write to string");
                }
            }
            HirItem::Supervisor(sup) => {
                writeln!(out, "supervisor {} {}", sup.id, sup.name).expect("write to string");
            }
        }
    }
    out
}

#[allow(
    clippy::too_many_lines,
    reason = "single match on HirExprKind variants; each arm renders one variant \
              and splitting would obscure the dump's per-variant locality"
)]
fn dump_expr(out: &mut String, expr: &HirExpr, indent: usize) {
    let pad = " ".repeat(indent);
    writeln!(
        out,
        "{pad}expr {} {} {:?} {:?}: {}",
        expr.node,
        expr.site,
        expr.intent,
        expr.value_class,
        expr.ty.user_facing()
    )
    .expect("write to string");
    match &expr.kind {
        HirExprKind::Literal(lit) => {
            writeln!(out, "{pad}  literal {lit:?}").expect("write to string");
        }
        HirExprKind::BindingRef { name, resolved } => {
            writeln!(out, "{pad}  ref {name} -> {resolved:?}").expect("write to string");
        }
        HirExprKind::Binary { op, left, right } => {
            writeln!(out, "{pad}  binary {op}").expect("write to string");
            dump_expr(out, left, indent + 4);
            dump_expr(out, right, indent + 4);
        }
        HirExprKind::IdentityCompare { left, right } => {
            writeln!(out, "{pad}  identity-compare").expect("write to string");
            dump_expr(out, left, indent + 4);
            dump_expr(out, right, indent + 4);
        }
        HirExprKind::Call { callee, args } => {
            writeln!(out, "{pad}  call").expect("write to string");
            dump_expr(out, callee, indent + 4);
            for arg in args {
                dump_expr(out, arg, indent + 4);
            }
        }
        HirExprKind::Block(block) => {
            writeln!(out, "{pad}  block {}", block.scope).expect("write to string");
        }
        HirExprKind::If { .. } => {
            writeln!(out, "{pad}  if").expect("write to string");
        }
        HirExprKind::StructInit {
            name, fields, base, ..
        } => {
            writeln!(out, "{pad}  struct-init {name}").expect("write to string");
            for (field_name, value) in fields {
                writeln!(out, "{pad}    .{field_name}").expect("write to string");
                dump_expr(out, value, indent + 6);
            }
            if let Some(base) = base {
                writeln!(out, "{pad}    ..base").expect("write to string");
                dump_expr(out, base, indent + 6);
            }
        }
        HirExprKind::FieldAccess { object, field } => {
            writeln!(out, "{pad}  field-access .{field}").expect("write to string");
            dump_expr(out, object, indent + 4);
        }
        HirExprKind::Scope { body } => {
            writeln!(out, "{pad}  scope scope={}", body.scope).expect("write to string");
            for stmt in &body.statements {
                match &stmt.kind {
                    HirStmtKind::Let(binding, value) => {
                        writeln!(
                            out,
                            "{pad}    let {} {}: {}",
                            binding.id,
                            binding.name,
                            binding.ty.user_facing()
                        )
                        .expect("write to string");
                        if let Some(value) = value {
                            dump_expr(out, value, indent + 6);
                        }
                    }
                    HirStmtKind::Expr(expr) => dump_expr(out, expr, indent + 4),
                    HirStmtKind::Return(Some(expr)) => {
                        writeln!(out, "{pad}    return").expect("write to string");
                        dump_expr(out, expr, indent + 6);
                    }
                    HirStmtKind::Return(None) => {
                        writeln!(out, "{pad}    return unit").expect("write to string");
                    }
                }
            }
        }
        HirExprKind::SpawnedCall {
            callee,
            args,
            task_ty,
        } => {
            writeln!(out, "{pad}  spawned-call task_ty={}", task_ty.user_facing())
                .expect("write to string");
            dump_expr(out, callee, indent + 4);
            for arg in args {
                dump_expr(out, arg, indent + 4);
            }
        }
        HirExprKind::ForkBlock { body, task_ty } => {
            writeln!(out, "{pad}  fork-block task_ty={}", task_ty.user_facing())
                .expect("write to string");
            for stmt in &body.statements {
                match &stmt.kind {
                    HirStmtKind::Let(binding, value) => {
                        writeln!(
                            out,
                            "{pad}    let {} {}: {}",
                            binding.id,
                            binding.name,
                            binding.ty.user_facing()
                        )
                        .expect("write to string");
                        if let Some(value) = value {
                            dump_expr(out, value, indent + 6);
                        }
                    }
                    HirStmtKind::Expr(expr) => dump_expr(out, expr, indent + 4),
                    HirStmtKind::Return(Some(expr)) => {
                        writeln!(out, "{pad}    return").expect("write to string");
                        dump_expr(out, expr, indent + 6);
                    }
                    HirStmtKind::Return(None) => {
                        writeln!(out, "{pad}    return unit").expect("write to string");
                    }
                }
            }
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            writeln!(out, "{pad}  scope-deadline").expect("write to string");
            dump_expr(out, duration, indent + 4);
            for stmt in &body.statements {
                match &stmt.kind {
                    HirStmtKind::Let(binding, value) => {
                        writeln!(
                            out,
                            "{pad}    let {} {}: {}",
                            binding.id,
                            binding.name,
                            binding.ty.user_facing()
                        )
                        .expect("write to string");
                        if let Some(value) = value {
                            dump_expr(out, value, indent + 6);
                        }
                    }
                    HirStmtKind::Expr(expr) => dump_expr(out, expr, indent + 4),
                    HirStmtKind::Return(Some(expr)) => {
                        writeln!(out, "{pad}    return").expect("write to string");
                        dump_expr(out, expr, indent + 6);
                    }
                    HirStmtKind::Return(None) => {
                        writeln!(out, "{pad}    return unit").expect("write to string");
                    }
                }
            }
        }
        HirExprKind::AwaitTask {
            binding_name,
            binding_id,
            output_ty,
        } => {
            writeln!(
                out,
                "{pad}  await-task {binding_name} ({binding_id}) -> {}",
                output_ty.user_facing()
            )
            .expect("write to string");
        }
        HirExprKind::Select(select) => {
            writeln!(out, "{pad}  select arms={}", select.arms.len()).expect("write to string");
            for arm in &select.arms {
                let kind_label = match &arm.kind {
                    crate::node::HirSelectArmKind::StreamNext { .. } => "stream-next",
                    crate::node::HirSelectArmKind::ActorAsk { method, .. } => {
                        // Borrow the method name into the label transiently.
                        let _ = method;
                        "actor-ask"
                    }
                    crate::node::HirSelectArmKind::TaskAwait { .. } => "task-await",
                    crate::node::HirSelectArmKind::AfterTimer { .. } => "after-timer",
                };
                let binding_label = arm.binding_name.as_deref().unwrap_or("_");
                writeln!(out, "{pad}    arm {kind_label} bind={binding_label}")
                    .expect("write to string");
            }
        }
        HirExprKind::SpawnLambdaActor {
            params,
            reply_ty,
            body,
            captures,
        } => {
            writeln!(
                out,
                "{pad}  spawn-lambda-actor params={} reply_ty={} captures={}",
                params.len(),
                reply_ty.user_facing(),
                captures.len()
            )
            .expect("write to string");
            for capture in captures {
                writeln!(
                    out,
                    "{pad}    capture {} ({}) {:?}",
                    capture.name, capture.binding, capture.kind
                )
                .expect("write to string");
            }
            dump_expr(out, body, indent + 4);
        }
        HirExprKind::Closure {
            params,
            ret_ty,
            body,
            captures,
        } => {
            writeln!(
                out,
                "{pad}  closure params={} ret_ty={} captures={}",
                params.len(),
                ret_ty.user_facing(),
                captures.len()
            )
            .expect("write to string");
            for capture in captures {
                writeln!(
                    out,
                    "{pad}    capture {} ({}) ty={} mode={:?} send={}",
                    capture.name,
                    capture.binding,
                    capture.ty.user_facing(),
                    capture.mode,
                    capture.is_send
                )
                .expect("write to string");
            }
            dump_expr(out, body, indent + 4);
        }
        HirExprKind::TupleIndex { tuple, index } => {
            writeln!(out, "{pad}  tuple-index .{index}").expect("write to string");
            dump_expr(out, tuple, indent + 4);
        }
        HirExprKind::Index { container, index } => {
            writeln!(out, "{pad}  vec-index").expect("write to string");
            dump_expr(out, container, indent + 4);
            dump_expr(out, index, indent + 4);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            inclusive,
        } => {
            let kind = if *inclusive {
                "vec-slice-inclusive"
            } else {
                "vec-slice"
            };
            writeln!(out, "{pad}  {kind}").expect("write to string");
            dump_expr(out, container, indent + 4);
            if let Some(s) = start {
                dump_expr(out, s, indent + 4);
            } else {
                writeln!(out, "{pad}    start (open)").expect("write to string");
            }
            if let Some(e) = end {
                dump_expr(out, e, indent + 4);
            } else {
                writeln!(out, "{pad}    end (open)").expect("write to string");
            }
        }
        HirExprKind::CoerceToDynTrait {
            value,
            trait_name,
            concrete_type,
            method_table,
            vtable_entries,
        } => {
            writeln!(
                out,
                "{pad}  coerce-to-dyn {} <- {} (slots={}, projected={})",
                trait_name,
                concrete_type.user_facing(),
                method_table.len(),
                vtable_entries.len()
            )
            .expect("write to string");
            dump_expr(out, value, indent + 4);
        }
        HirExprKind::CallDynMethod {
            receiver,
            trait_name,
            method_name,
            slot,
            args,
            ret_ty,
        } => {
            writeln!(
                out,
                "{pad}  call-dyn {trait_name}::{method_name} slot={slot} -> {}",
                ret_ty.user_facing()
            )
            .expect("write to string");
            dump_expr(out, receiver, indent + 4);
            for arg in args {
                dump_expr(out, arg, indent + 4);
            }
        }
        HirExprKind::Unsupported(reason) => {
            writeln!(out, "{pad}  unsupported {reason}").expect("write to string");
        }
    }
}
