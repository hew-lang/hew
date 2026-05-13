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
        }
    }
    out
}

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
        HirExprKind::Unsupported(reason) => {
            writeln!(out, "{pad}  unsupported {reason}").expect("write to string");
        }
    }
}
