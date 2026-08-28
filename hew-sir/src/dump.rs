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
                dump_op(&mut out, module, op);
            }
            dump_term(&mut out, &block.terminator);
        }
        writeln!(out, "}}").expect("write to String");
    }
    out
}

fn dump_op(out: &mut String, module: &SemModule, op: &crate::SemOp) {
    write!(out, "    ").expect("write to String");
    match op.results.as_slice() {
        [] => {}
        [result] => write!(out, "%{} = ", result.id.0).expect("write to String"),
        // The verifier rejects multi-result operations in this initial slice,
        // but a dump must remain total for malformed IR used in diagnostics.
        results => {
            for (index, result) in results.iter().enumerate() {
                if index != 0 {
                    write!(out, ", ").expect("write to String");
                }
                write!(out, "%{}", result.id.0).expect("write to String");
            }
            write!(out, " = ").expect("write to String");
        }
    }
    match &op.kind {
        SemOpKind::ConstI64(value) => writeln!(out, "const {value}").expect("write to String"),
        SemOpKind::ConstBool(value) => writeln!(out, "const {value}").expect("write to String"),
        SemOpKind::TupleMake { elements } => {
            write!(out, "tuple.make(").expect("write to String");
            for (index, element) in elements.iter().enumerate() {
                if index != 0 {
                    write!(out, ", ").expect("write to String");
                }
                write!(out, "{}", operand(element)).expect("write to String");
            }
            writeln!(out, ")").expect("write to String");
        }
        SemOpKind::TupleGet { tuple, index } => {
            writeln!(out, "tuple.get {}, {index}", operand(tuple)).expect("write to String");
        }
        SemOpKind::Unary { op, value } => {
            writeln!(out, "{op:?} {}", operand(value)).expect("write to String");
        }
        SemOpKind::Binary { op, lhs, rhs } => {
            writeln!(out, "{op:?} {}, {}", operand(lhs), operand(rhs)).expect("write to String");
        }
        SemOpKind::Cast { value, to } => {
            writeln!(out, "cast {} to {}", operand(value), to.user_facing())
                .expect("write to String");
        }
        SemOpKind::Call { callee, args } => {
            let target = module.callable(*callee).map_or_else(
                || format!("<invalid-callable:{}>", callee.0),
                |callable| callable.symbol.clone(),
            );
            write!(out, "call @{target}(").expect("write to String");
            for (index, arg) in args.iter().enumerate() {
                if index != 0 {
                    write!(out, ", ").expect("write to String");
                }
                write!(out, "{}", operand(arg)).expect("write to String");
            }
            writeln!(out, ")").expect("write to String");
        }
    }
}

fn dump_term(out: &mut String, term: &SemTerminator) {
    match term {
        SemTerminator::Return { value: Some(value) } => {
            writeln!(out, "    return {}", operand(value)).expect("write to String");
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
            "    branch {}, bb{}{}, bb{}{}",
            operand(condition),
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
        edge.args.iter().map(operand).collect::<Vec<_>>().join(", ")
    )
}

fn operand(operand: &crate::Operand) -> String {
    match operand.mode {
        crate::UseMode::Read => format!("%{}", operand.value.0),
        crate::UseMode::BorrowShared => format!("borrow %{}", operand.value.0),
        crate::UseMode::BorrowMut => format!("borrow_mut %{}", operand.value.0),
        crate::UseMode::Move => format!("move %{}", operand.value.0),
        crate::UseMode::Consume => format!("consume %{}", operand.value.0),
    }
}
