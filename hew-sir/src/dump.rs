use std::fmt::Write as _;

use crate::{SemModule, SemOpKind, SemTerminator};

/// Deterministic, one-way diagnostic printer for Semantic IR.
#[must_use]
pub fn dump_sir(module: &SemModule) -> String {
    let mut out = String::new();
    for function in &module.functions {
        write!(out, "fn {}(", function.name).expect("write to String");
        for (index, param) in function.params.iter().enumerate() {
            if index != 0 {
                write!(out, ", ").expect("write to String");
            }
            write!(out, "%{}: {}", param.value.0, param.ty.user_facing()).expect("write to String");
        }
        writeln!(out, ") -> {} {{", function.return_ty.user_facing()).expect("write to String");
        for block in &function.blocks {
            write!(out, "bb{}", block.id.0).expect("write to String");
            if !block.args.is_empty() {
                write!(out, "(").expect("write to String");
                for (index, arg) in block.args.iter().enumerate() {
                    if index != 0 {
                        write!(out, ", ").expect("write to String");
                    }
                    write!(out, "%{}: {}", arg.value.0, arg.ty.user_facing())
                        .expect("write to String");
                }
                write!(out, ")").expect("write to String");
            }
            writeln!(out, ":").expect("write to String");
            for op in &block.ops {
                dump_op(&mut out, op);
            }
            dump_term(&mut out, &block.terminator);
        }
        writeln!(out, "}}").expect("write to String");
    }
    out
}

fn dump_op(out: &mut String, op: &crate::SemOp) {
    let result = &op.results[0];
    write!(out, "    %{} = ", result.id.0).expect("write to String");
    match &op.kind {
        SemOpKind::ConstI64(value) => writeln!(out, "const {value}").expect("write to String"),
        SemOpKind::ConstBool(value) => writeln!(out, "const {value}").expect("write to String"),
        SemOpKind::Unary { op, value } => {
            writeln!(out, "{op:?} %{}", value.value.0).expect("write to String");
        }
        SemOpKind::Binary { op, lhs, rhs } => {
            writeln!(out, "{op:?} %{}, %{}", lhs.value.0, rhs.value.0).expect("write to String");
        }
        SemOpKind::Cast { value, to } => {
            writeln!(out, "cast %{} to {}", value.value.0, to.user_facing())
                .expect("write to String");
        }
        SemOpKind::Call { target, args } => {
            write!(out, "call {target:?}(").expect("write to String");
            for (index, arg) in args.iter().enumerate() {
                if index != 0 {
                    write!(out, ", ").expect("write to String");
                }
                write!(out, "%{}", arg.value.0).expect("write to String");
            }
            writeln!(out, ")").expect("write to String");
        }
    }
}

fn dump_term(out: &mut String, term: &SemTerminator) {
    match term {
        SemTerminator::Return { value: Some(value) } => {
            writeln!(out, "    return %{}", value.0).expect("write to String");
        }
        SemTerminator::Return { value: None } => {
            writeln!(out, "    return").expect("write to String");
        }
        SemTerminator::Goto(edge) => {
            writeln!(out, "    goto bb{}{}", edge.target.0, edge_args(edge))
                .expect("write to String");
        }
        SemTerminator::Branch {
            condition,
            then_target,
            else_target,
        } => writeln!(
            out,
            "    branch %{}, bb{}{}, bb{}{}",
            condition.0,
            then_target.target.0,
            edge_args(then_target),
            else_target.target.0,
            edge_args(else_target)
        )
        .expect("write to String"),
        SemTerminator::Unreachable => writeln!(out, "    unreachable").expect("write to String"),
    }
}

fn edge_args(edge: &crate::Edge) -> String {
    if edge.args.is_empty() {
        return String::new();
    }
    format!(
        "({})",
        edge.args
            .iter()
            .map(|value| format!("%{}", value.0))
            .collect::<Vec<_>>()
            .join(", ")
    )
}
