use std::fmt::Write as _;

use crate::node::{HirExpr, HirExprKind, HirItem, HirModule, HirStmtKind};

#[must_use]
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
        HirExprKind::StructInit { name, fields, .. } => {
            writeln!(out, "{pad}  struct-init {name}").expect("write to string");
            for (_, value) in fields {
                dump_expr(out, value, indent + 4);
            }
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
        HirExprKind::TupleIndex { tuple, index } => {
            writeln!(out, "{pad}  tuple-index .{index}").expect("write to string");
            dump_expr(out, tuple, indent + 4);
        }
        HirExprKind::Unsupported(reason) => {
            writeln!(out, "{pad}  unsupported {reason}").expect("write to string");
        }
    }
}
