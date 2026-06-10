// The dump pass renders the `#[deprecated]` `CallTraitMethodStatic`
// variant alongside the new `ResolvedImplCall`. Allowlist test on
// construction sites is the structural enforcement.
#![allow(
    deprecated,
    reason = "legacy CallTraitMethodStatic variant is allowlist-gated; \
              see hew-hir/tests/call_trait_method_static_creation_allowlist.rs"
)]

use std::fmt::Write as _;

use crate::node::{HirBlock, HirExpr, HirExprKind, HirItem, HirModule, HirStmtKind};

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
                        HirStmtKind::Assign { target, value } => {
                            writeln!(out, "  assign").expect("write to string");
                            dump_expr(&mut out, target, 4);
                            dump_expr(&mut out, value, 4);
                        }
                        HirStmtKind::Expr(expr) => dump_expr(&mut out, expr, 2),
                        HirStmtKind::Return(Some(expr)) => {
                            writeln!(out, "  return").expect("write to string");
                            dump_expr(&mut out, expr, 4);
                        }
                        HirStmtKind::Return(None) => {
                            writeln!(out, "  return unit").expect("write to string");
                        }
                        HirStmtKind::Defer { body, .. } => {
                            writeln!(out, "  defer").expect("write to string");
                            dump_expr(&mut out, body, 4);
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
                    writeln!(
                        out,
                        "  init params={} body_scope={}",
                        init.params.len(),
                        init.body.scope
                    )
                    .expect("write to string");
                    for param in &init.params {
                        writeln!(
                            out,
                            "    param {} {}: {}",
                            param.id,
                            param.name,
                            param.ty.user_facing()
                        )
                        .expect("write to string");
                    }
                    dump_block(&mut out, &init.body, 4);
                }
                for rf in &actor.receive_handlers {
                    writeln!(
                        out,
                        "  receive {} params={} -> {} state_guard={:?} every_ns={:?} is_generator={} body_scope={}",
                        rf.name,
                        rf.params.len(),
                        rf.return_ty.user_facing(),
                        rf.state_guard,
                        rf.every_ns,
                        rf.is_generator,
                        rf.body.scope
                    )
                    .expect("write to string");
                    for param in &rf.params {
                        writeln!(
                            out,
                            "    param {} {}: {}",
                            param.id,
                            param.name,
                            param.ty.user_facing()
                        )
                        .expect("write to string");
                    }
                    dump_block(&mut out, &rf.body, 4);
                }
                for m in &actor.methods {
                    writeln!(
                        out,
                        "  method {} params={} -> {} body_scope={}",
                        m.name,
                        m.params.len(),
                        m.return_ty.user_facing(),
                        m.body.scope
                    )
                    .expect("write to string");
                    for param in &m.params {
                        writeln!(
                            out,
                            "    param {} {}: {}",
                            param.id,
                            param.name,
                            param.ty.user_facing()
                        )
                        .expect("write to string");
                    }
                    dump_block(&mut out, &m.body, 4);
                }
                for h in &actor.lifecycle_hooks {
                    writeln!(
                        out,
                        "  on({:?}) {} params={} -> {} body_scope={}",
                        h.kind,
                        h.name,
                        h.params.len(),
                        h.return_ty.user_facing(),
                        h.body.scope
                    )
                    .expect("write to string");
                    for param in &h.params {
                        writeln!(
                            out,
                            "    param {} {}: {}",
                            param.id,
                            param.name,
                            param.ty.user_facing()
                        )
                        .expect("write to string");
                    }
                    dump_block(&mut out, &h.body, 4);
                }
            }
            HirItem::Supervisor(sup) => {
                writeln!(out, "supervisor {} {}", sup.id, sup.name).expect("write to string");
            }
            HirItem::Impl(block) => {
                writeln!(
                    out,
                    "impl {} {} for {} [methods: {}]",
                    block.id,
                    block.trait_name.as_deref().unwrap_or("<inherent>"),
                    block.self_type_name,
                    block.method_symbols.join(", "),
                )
                .expect("write to string");
                for (name, ty) in &block.type_aliases {
                    writeln!(out, "  type {} = {}", name, ty.user_facing())
                        .expect("write to string");
                }
            }
            HirItem::ExternFn(ef) => {
                let params = ef
                    .param_tys
                    .iter()
                    .map(|ty| ty.user_facing().to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(
                    out,
                    "extern \"{}\" fn {}({}) -> {}",
                    ef.abi,
                    ef.name,
                    params,
                    ef.return_ty.user_facing(),
                )
                .expect("write to string");
            }
            HirItem::Const(c) => {
                let value = match &c.value {
                    crate::node::HirConstValue::Integer(v) => v.to_string(),
                    crate::node::HirConstValue::String(s) => format!("{s:?}"),
                    crate::node::HirConstValue::Float(v) => v.to_string(),
                };
                writeln!(
                    out,
                    "const {} {}: {} = {}",
                    c.id,
                    c.name,
                    c.ty.user_facing(),
                    value,
                )
                .expect("write to string");
            }
        }
    }
    out
}

fn dump_block(out: &mut String, block: &HirBlock, indent: usize) {
    let pad = " ".repeat(indent);
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(binding, value) => {
                writeln!(
                    out,
                    "{pad}let {} {}: {}",
                    binding.id,
                    binding.name,
                    binding.ty.user_facing()
                )
                .expect("write to string");
                if let Some(value) = value {
                    dump_expr(out, value, indent + 2);
                }
            }
            HirStmtKind::Assign { target, value } => {
                writeln!(out, "{pad}assign").expect("write to string");
                dump_expr(out, target, indent + 2);
                dump_expr(out, value, indent + 2);
            }
            HirStmtKind::Expr(expr) => dump_expr(out, expr, indent),
            HirStmtKind::Return(Some(expr)) => {
                writeln!(out, "{pad}return").expect("write to string");
                dump_expr(out, expr, indent + 2);
            }
            HirStmtKind::Return(None) => {
                writeln!(out, "{pad}return unit").expect("write to string");
            }
            HirStmtKind::Defer { body, .. } => {
                writeln!(out, "{pad}defer").expect("write to string");
                dump_expr(out, body, indent + 2);
            }
        }
    }
    if let Some(tail) = &block.tail {
        writeln!(out, "{pad}tail").expect("write to string");
        dump_expr(out, tail, indent + 2);
    }
}

/// Dump one nested constructor payload predicate (recursive).
fn dump_payload_variant_predicate(
    out: &mut String,
    pvp: &crate::node::HirPayloadVariantPredicate,
    indent: usize,
) {
    let pad = " ".repeat(indent);
    writeln!(
        out,
        "{pad}payload[{}] is {}::{} [variant_idx={}, bindings={}]",
        pvp.field_idx,
        pvp.variant_match.type_name,
        pvp.variant_match.variant_name,
        pvp.variant_idx,
        pvp.bindings.len(),
    )
    .expect("write to string");
    for child in &pvp.nested {
        dump_payload_variant_predicate(out, child, indent + 2);
    }
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
        HirExprKind::RegexLiteralRef {
            literal_id,
            pattern,
            ..
        } => {
            writeln!(out, "{pad}  regex-literal #{literal_id} {pattern:?}")
                .expect("write to string");
        }
        HirExprKind::BindingRef { name, resolved } => {
            writeln!(out, "{pad}  ref {name} -> {resolved:?}").expect("write to string");
        }
        HirExprKind::ContextReader { reader } => {
            writeln!(out, "{pad}  context-reader {reader:?}").expect("write to string");
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
        HirExprKind::NumericCast {
            value,
            from_ty,
            to_ty,
        } => {
            writeln!(
                out,
                "{pad}  numeric-cast {} -> {}",
                from_ty.user_facing(),
                to_ty.user_facing()
            )
            .expect("write to string");
            dump_expr(out, value, indent + 4);
        }
        HirExprKind::Call { callee, args } => {
            writeln!(out, "{pad}  call").expect("write to string");
            dump_expr(out, callee, indent + 4);
            for arg in args {
                dump_expr(out, arg, indent + 4);
            }
        }
        HirExprKind::Spawn { actor_name, args } => {
            writeln!(out, "{pad}  spawn {actor_name}").expect("write to string");
            for (arg_name, value) in args {
                writeln!(out, "{pad}    {arg_name}:").expect("write to string");
                dump_expr(out, value, indent + 6);
            }
        }
        HirExprKind::ActorSend {
            receiver,
            method_id,
            args,
        } => {
            writeln!(out, "{pad}  actor-send {method_id}").expect("write to string");
            dump_expr(out, receiver, indent + 4);
            for arg in args {
                dump_expr(out, arg, indent + 4);
            }
        }
        HirExprKind::ActorAsk {
            receiver,
            method_id,
            args,
            reply_ty,
            deadline_ns,
        } => {
            let deadline = deadline_ns
                .map(|ns| format!(" | after {ns}ns"))
                .unwrap_or_default();
            writeln!(
                out,
                "{pad}  actor-ask {method_id} -> {}{deadline}",
                reply_ty.user_facing()
            )
            .expect("write to string");
            dump_expr(out, receiver, indent + 4);
            for arg in args {
                dump_expr(out, arg, indent + 4);
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            reply_ty,
        } => {
            writeln!(out, "{pad}  remote-actor-ask -> {}", reply_ty.user_facing())
                .expect("write to string");
            dump_expr(out, receiver, indent + 4);
            dump_expr(out, msg, indent + 4);
            dump_expr(out, timeout_ms, indent + 4);
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
                    HirStmtKind::Assign { target, value } => {
                        writeln!(out, "{pad}    assign").expect("write to string");
                        dump_expr(out, target, indent + 6);
                        dump_expr(out, value, indent + 6);
                    }
                    HirStmtKind::Expr(expr) => dump_expr(out, expr, indent + 4),
                    HirStmtKind::Return(Some(expr)) => {
                        writeln!(out, "{pad}    return").expect("write to string");
                        dump_expr(out, expr, indent + 6);
                    }
                    HirStmtKind::Return(None) => {
                        writeln!(out, "{pad}    return unit").expect("write to string");
                    }
                    HirStmtKind::Defer { body, .. } => {
                        writeln!(out, "{pad}    defer").expect("write to string");
                        dump_expr(out, body, indent + 6);
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
                    HirStmtKind::Assign { target, value } => {
                        writeln!(out, "{pad}    assign").expect("write to string");
                        dump_expr(out, target, indent + 6);
                        dump_expr(out, value, indent + 6);
                    }
                    HirStmtKind::Expr(expr) => dump_expr(out, expr, indent + 4),
                    HirStmtKind::Return(Some(expr)) => {
                        writeln!(out, "{pad}    return").expect("write to string");
                        dump_expr(out, expr, indent + 6);
                    }
                    HirStmtKind::Return(None) => {
                        writeln!(out, "{pad}    return unit").expect("write to string");
                    }
                    HirStmtKind::Defer { body, .. } => {
                        writeln!(out, "{pad}    defer").expect("write to string");
                        dump_expr(out, body, indent + 6);
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
                    HirStmtKind::Assign { target, value } => {
                        writeln!(out, "{pad}    assign").expect("write to string");
                        dump_expr(out, target, indent + 6);
                        dump_expr(out, value, indent + 6);
                    }
                    HirStmtKind::Expr(expr) => dump_expr(out, expr, indent + 4),
                    HirStmtKind::Return(Some(expr)) => {
                        writeln!(out, "{pad}    return").expect("write to string");
                        dump_expr(out, expr, indent + 6);
                    }
                    HirStmtKind::Return(None) => {
                        writeln!(out, "{pad}    return unit").expect("write to string");
                    }
                    HirStmtKind::Defer { body, .. } => {
                        writeln!(out, "{pad}    defer").expect("write to string");
                        dump_expr(out, body, indent + 6);
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
        HirExprKind::ConnAwaitRead {
            conn,
            to_string,
            deadline_ns,
        } => {
            let deadline = deadline_ns
                .map(|ns| format!(" | after {ns}ns"))
                .unwrap_or_default();
            writeln!(
                out,
                "{pad}  conn-await-read to_string={to_string}{deadline}"
            )
            .expect("write to string");
            dump_expr(out, conn, indent + 2);
        }
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            writeln!(out, "{pad}  listener-await-accept").expect("write to string");
            dump_expr(out, listener, indent + 2);
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
                    crate::node::HirSelectArmKind::ChannelRecv { .. } => "channel-recv",
                    crate::node::HirSelectArmKind::AfterTimer { .. } => "after-timer",
                };
                let binding_label = arm.binding_name.as_deref().unwrap_or("_");
                writeln!(out, "{pad}    arm {kind_label} bind={binding_label}")
                    .expect("write to string");
            }
        }
        HirExprKind::Join(join) => {
            writeln!(out, "{pad}  join branches={}", join.branches.len()).expect("write to string");
            for branch in &join.branches {
                writeln!(out, "{pad}    branch actor-ask method={}", branch.method)
                    .expect("write to string");
                dump_expr(out, &branch.actor, indent + 2);
                for arg in &branch.args {
                    dump_expr(out, arg, indent + 2);
                }
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
            escape_kind,
        } => {
            writeln!(
                out,
                "{pad}  closure params={} ret_ty={} captures={} escape={escape_kind:?}",
                params.len(),
                ret_ty.user_facing(),
                captures.len()
            )
            .expect("write to string");
            for capture in captures {
                writeln!(
                    out,
                    "{pad}    capture {} ({}) ty={} mode={:?} send={} sync={}",
                    capture.name,
                    capture.binding,
                    capture.ty.user_facing(),
                    capture.mode,
                    capture.is_send,
                    capture.is_sync
                )
                .expect("write to string");
            }
            dump_expr(out, body, indent + 4);
        }
        HirExprKind::GenBlock {
            body,
            yield_ty,
            return_ty,
        } => {
            writeln!(
                out,
                "{pad}  gen-block yield_ty={} return_ty={}",
                yield_ty.user_facing(),
                return_ty.user_facing()
            )
            .expect("write to string");
            dump_block(out, body, indent + 4);
        }
        HirExprKind::Yield { value, yield_ty } => {
            writeln!(out, "{pad}  yield yield_ty={}", yield_ty.user_facing())
                .expect("write to string");
            if let Some(value) = value {
                dump_expr(out, value, indent + 4);
            }
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
            signature: _,
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
        HirExprKind::CallTraitMethodStatic {
            receiver,
            receiver_type_param,
            declaring_trait,
            method_name,
            args,
            ret_ty,
            ..
        } => {
            writeln!(
                out,
                "{pad}  call-static-trait {declaring_trait}::{method_name} \
                 [receiver_param={receiver_type_param}] -> {}",
                ret_ty.user_facing()
            )
            .expect("write to string");
            dump_expr(out, receiver, indent + 4);
            for arg in args {
                dump_expr(out, arg, indent + 4);
            }
        }
        HirExprKind::VarSelfMethodCall {
            receiver,
            target,
            args,
            ret_ty,
            receiver_ty,
        } => {
            let target_label = match target {
                crate::node::HirVarSelfMethodTarget::Direct { callee } => callee.clone(),
                crate::node::HirVarSelfMethodTarget::StaticTrait {
                    receiver_type_param,
                    declaring_trait,
                    method_name,
                    ..
                } => format!(
                    "{declaring_trait}::{method_name} [receiver_param={receiver_type_param}]"
                ),
            };
            writeln!(
                out,
                "{pad}  var-self-call {target_label} -> {} writeback {}",
                ret_ty.user_facing(),
                receiver_ty.user_facing()
            )
            .expect("write to string");
            dump_expr(out, receiver, indent + 4);
            for arg in args {
                dump_expr(out, arg, indent + 4);
            }
        }
        HirExprKind::ResolvedImplCall {
            receiver,
            impl_id,
            method_name,
            target_symbol,
            target_family,
            type_args,
            args,
            ret_ty,
        } => {
            writeln!(
                out,
                "{pad}  resolved-impl-call impl_id={} {method_name} -> {target_symbol} (family={target_family:?}) [type_args={}] -> {}",
                impl_id.0,
                type_args
                    .iter()
                    .map(|pat| format!("{pat}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                ret_ty.user_facing()
            )
            .expect("write to string");
            dump_expr(out, receiver, indent + 4);
            for arg in args {
                dump_expr(out, arg, indent + 4);
            }
        }
        HirExprKind::NumericMethod {
            receiver,
            arg,
            family,
            op,
            result_ty,
            operand_ty,
            signedness,
            width,
        } => {
            writeln!(
                out,
                "{pad}  numeric-method {:?} {:?} {} -> {} ({:?}, {:?})",
                family,
                op,
                operand_ty.user_facing(),
                result_ty.user_facing(),
                signedness,
                width
            )
            .expect("write to string");
            dump_expr(out, receiver, indent + 4);
            dump_expr(out, arg, indent + 4);
        }
        HirExprKind::CancellationTokenIsCancelled { receiver } => {
            writeln!(out, "{pad}  cancellation-token-is-cancelled").expect("write to string");
            dump_expr(out, receiver, indent + 4);
        }
        HirExprKind::GeneratorNext { receiver, yield_ty } => {
            writeln!(out, "{pad}  generator-next yield_ty={yield_ty:?}").expect("write to string");
            dump_expr(out, receiver, indent + 4);
        }
        HirExprKind::MachineEmit { event_idx, fields } => {
            writeln!(out, "{pad}  machine-emit event_idx={event_idx}").expect("write to string");
            for (field_name, field_val) in fields {
                writeln!(out, "{pad}    field {field_name}:").expect("write to string");
                dump_expr(out, field_val, indent + 6);
            }
        }
        HirExprKind::MachineStep {
            machine_name,
            receiver,
            event,
        } => {
            writeln!(out, "{pad}  machine-step {machine_name}").expect("write to string");
            writeln!(out, "{pad}    receiver:").expect("write to string");
            dump_expr(out, receiver, indent + 6);
            writeln!(out, "{pad}    event:").expect("write to string");
            dump_expr(out, event, indent + 6);
        }
        HirExprKind::MachineStateName {
            machine_name,
            receiver,
        } => {
            writeln!(out, "{pad}  machine-state-name {machine_name}").expect("write to string");
            dump_expr(out, receiver, indent + 4);
        }
        HirExprKind::MachineVariantCtor {
            machine_name,
            state_idx,
            payload,
        } => {
            writeln!(
                out,
                "{pad}  machine-variant-ctor {machine_name}[{state_idx}]"
            )
            .expect("write to string");
            if let Some(fields) = payload {
                for (name, val) in fields {
                    writeln!(out, "{pad}    field {name}:").expect("write to string");
                    dump_expr(out, val, indent + 6);
                }
            }
        }
        HirExprKind::MachineFieldAccess {
            machine_name,
            state_idx,
            field_idx,
            field_name,
        } => {
            writeln!(
                out,
                "{pad}  machine-field-access {machine_name}[{state_idx}].{field_name}[{field_idx}]"
            )
            .expect("write to string");
        }
        HirExprKind::MachineEventFieldAccess {
            machine_name,
            event_idx,
            field_idx,
            field_name,
        } => {
            writeln!(
                out,
                "{pad}  machine-event-field-access {machine_name}Event[{event_idx}].{field_name}[{field_idx}]"
            )
            .expect("write to string");
        }
        HirExprKind::While {
            label,
            condition,
            body,
        } => {
            writeln!(out, "{pad}  while label={label:?}").expect("write to string");
            dump_expr(out, condition, indent + 4);
            dump_block(out, body, indent + 4);
        }
        HirExprKind::ForRange {
            label,
            binding,
            start,
            end,
            inclusive,
            body,
        } => {
            writeln!(
                out,
                "{pad}  for-range {} (inclusive={inclusive}, label={label:?})",
                binding.name
            )
            .expect("write to string");
            dump_expr(out, start, indent + 4);
            dump_expr(out, end, indent + 4);
            dump_block(out, body, indent + 4);
        }
        HirExprKind::Match { scrutinee, arms } => {
            writeln!(out, "{pad}  match ({} arms)", arms.len()).expect("write to string");
            dump_expr(out, scrutinee, indent + 4);
            for arm in arms {
                let label = match &arm.predicate {
                    crate::node::HirMatchArmPredicate::Wildcard => "_".to_string(),
                    crate::node::HirMatchArmPredicate::Binding { name, .. } => {
                        format!("binding {name}")
                    }
                    crate::node::HirMatchArmPredicate::EnumVariant { variant_match, .. } => {
                        format!(
                            "{}::{}",
                            variant_match.type_name, variant_match.variant_name
                        )
                    }
                    crate::node::HirMatchArmPredicate::Literal { lit, ty } => {
                        format!("literal {lit:?}: {ty:?}")
                    }
                    crate::node::HirMatchArmPredicate::RecordProject { ty } => {
                        format!("record-project {ty:?}")
                    }
                    crate::node::HirMatchArmPredicate::TupleProject { arity } => {
                        format!("tuple-project arity={arity}")
                    }
                    crate::node::HirMatchArmPredicate::Regex { pattern, .. } => {
                        format!("re{pattern:?}")
                    }
                };
                writeln!(out, "{pad}    arm {label}").expect("write to string");
                for pred in &arm.payload_predicates {
                    writeln!(
                        out,
                        "{pad}      payload[{}] == {:?}",
                        pred.field_idx, pred.literal
                    )
                    .expect("write to string");
                }
                for pvp in &arm.payload_variant_predicates {
                    dump_payload_variant_predicate(out, pvp, indent + 6);
                }
                dump_expr(out, &arm.body, indent + 6);
            }
        }
        HirExprKind::Unsupported(reason) => {
            writeln!(out, "{pad}  unsupported {reason}").expect("write to string");
        }
        HirExprKind::Unary { op, operand, .. } => {
            writeln!(out, "{pad}  unary {op:?}").expect("write to string");
            dump_expr(out, operand, indent + 4);
        }
        HirExprKind::TupleLiteral { elements } => {
            writeln!(out, "{pad}  tuple literal ({} elements)", elements.len())
                .expect("write to string");
            for elem in elements {
                dump_expr(out, elem, indent + 4);
            }
        }
        HirExprKind::WhileLet {
            label,
            scrutinee,
            variant_match,
            variant_idx,
            bindings,
            body,
        } => {
            writeln!(
                out,
                "{pad}  while-let {}::{} [variant_idx={variant_idx}, bindings={}, label={label:?}]",
                variant_match.type_name,
                variant_match.variant_name,
                bindings.len(),
            )
            .expect("write to string");
            dump_expr(out, scrutinee, indent + 4);
            dump_block(out, body, indent + 4);
        }
        HirExprKind::IfLet {
            scrutinee,
            variant_match,
            variant_idx,
            bindings,
            body,
            else_body,
            result_ty,
        } => {
            writeln!(
                out,
                "{pad}  if-let {}::{} [variant_idx={variant_idx}, bindings={}, result_ty={result_ty:?}]",
                variant_match.type_name,
                variant_match.variant_name,
                bindings.len(),
            )
            .expect("write to string");
            dump_expr(out, scrutinee, indent + 4);
            dump_block(out, body, indent + 4);
            if let Some(eb) = else_body {
                writeln!(out, "{pad}  else").expect("write to string");
                dump_block(out, eb, indent + 4);
            }
        }
        HirExprKind::Break { label, value } => {
            writeln!(out, "{pad}  break label={label:?}").expect("write to string");
            if let Some(value) = value {
                dump_expr(out, value, indent + 4);
            }
        }
        HirExprKind::Continue { label } => {
            writeln!(out, "{pad}  continue label={label:?}").expect("write to string");
        }
        HirExprKind::Loop { label, body } => {
            writeln!(out, "{pad}  loop label={label:?}").expect("write to string");
            dump_block(out, body, indent + 4);
        }
    }
}
