use std::fmt::Write as _;

use crate::{
    AggregateShapeRef, CallResult, CallUnwind, LoweredModule, OwnKind, SemModule, SemOpKind,
    SemTerminator, SirLoweringStatus,
};

/// Deterministic printer for a whole HIR→SIR lowering result.
///
/// The IR text alone cannot say why a declaration has no body, so a dump that
/// prints only bodies is silent about exactly the thing an inspector is
/// looking for. This prints, ahead of the IR, one stanza per declaration that
/// failed to lower — every one of them, with its reason — so the surface gap
/// is visible in full rather than summarised, truncated, or omitted.
#[must_use]
pub fn dump_lowering(lowered: &LoweredModule) -> String {
    let mut out = String::new();
    if lowered.module.entry_callable.is_none() {
        out.push_str(
            "; no entry callable: this module is not a program, so no body was demanded\n",
        );
    }
    for source in &lowered.statuses {
        let SirLoweringStatus::Unsupported { reason } = &source.status else {
            continue;
        };
        writeln!(out, "; fn {}", source.name).expect("write to String");
        writeln!(out, "; unsupported: {reason}").expect("write to String");
    }
    // Concrete generic instances have no HIR declaration of their own, so
    // their failures are only visible through the callable table.
    for (callable, status) in &lowered.callable_statuses {
        let SirLoweringStatus::Unsupported { reason } = status else {
            continue;
        };
        let Some(header) = lowered.module.callable(*callable) else {
            continue;
        };
        if !matches!(header.instance, crate::CallableInstance::Generic(_)) {
            continue;
        }
        writeln!(out, "; fn {}", header.symbol).expect("write to String");
        writeln!(out, "; unsupported: {reason}").expect("write to String");
    }
    out.push_str(&dump_sir(&lowered.module));
    out
}

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
            write!(
                out,
                "%{}: {}{}",
                param.value.0,
                param.ty.user_facing(),
                own_suffix(param.own)
            )
            .expect("write to String");
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
                    write!(
                        out,
                        "%{}: {}{}",
                        arg.value.0,
                        arg.ty.user_facing(),
                        own_suffix(arg.own)
                    )
                    .expect("write to String");
                }
                write!(out, ")").expect("write to String");
            }
            writeln!(out, ":").expect("write to String");
            for op in &block.ops {
                dump_op(&mut out, op);
            }
            dump_term(&mut out, module, &block.terminator);
        }
        writeln!(out, "}}").expect("write to String");
    }
    out
}

/// The §1.2 ownership kind a value definition carries, as dump text.
///
/// `OwnKind::None` renders as nothing: it is the kind of every value in the
/// domain this dump prints today, so printing it would be noise on every line
/// and would say nothing. The two kinds that carry an obligation are printed,
/// which is what makes `ValueDef.own` and `BlockArg.own` readable facts rather
/// than fields only the lowering ever touches.
const fn own_suffix(own: OwnKind) -> &'static str {
    match own {
        OwnKind::None => "",
        OwnKind::Owned => " owned",
        OwnKind::Guaranteed => " guaranteed",
    }
}

fn aggregate_shape(shape: AggregateShapeRef) -> String {
    match shape {
        AggregateShapeRef::Tuple => "tuple".to_string(),
        AggregateShapeRef::Record(id) => format!("record#{}", id.0),
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "one arm per operation kind is the point of a closed textual rendering"
)]
fn dump_op(out: &mut String, op: &crate::SemOp) {
    write!(out, "    ").expect("write to String");
    match op.results.as_slice() {
        [] => {}
        [result] => {
            write!(out, "%{}{} = ", result.id.0, own_suffix(result.own)).expect("write to String");
        }
        // The verifier rejects multi-result operations in this initial slice,
        // but a dump must remain total for malformed IR used in diagnostics.
        results => {
            for (index, result) in results.iter().enumerate() {
                if index != 0 {
                    write!(out, ", ").expect("write to String");
                }
                write!(out, "%{}{}", result.id.0, own_suffix(result.own)).expect("write to String");
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
        SemOpKind::AggregateMake { shape, fields } => {
            write!(out, "aggregate.make {}(", aggregate_shape(*shape)).expect("write to String");
            for (index, field) in fields.iter().enumerate() {
                if index != 0 {
                    write!(out, ", ").expect("write to String");
                }
                write!(out, "{}", operand(field)).expect("write to String");
            }
            writeln!(out, ")").expect("write to String");
        }
        SemOpKind::AggregateProjectCopy {
            shape,
            aggregate,
            field,
        } => {
            writeln!(
                out,
                "aggregate.project_copy {} {}, {field}",
                aggregate_shape(*shape),
                operand(aggregate)
            )
            .expect("write to String");
        }
        SemOpKind::VariantMake {
            shape,
            variant,
            fields,
        } => {
            write!(out, "variant.make #{}:{variant}(", shape.0).expect("write to String");
            for (index, field) in fields.iter().enumerate() {
                if index != 0 {
                    write!(out, ", ").expect("write to String");
                }
                write!(out, "{}", operand(field)).expect("write to String");
            }
            writeln!(out, ")").expect("write to String");
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
        SemOpKind::ConstF64(value) => writeln!(out, "const {value}").expect("write to String"),
        SemOpKind::ConstChar(value) => writeln!(out, "const {value:?}").expect("write to String"),
        SemOpKind::ConstUnit => writeln!(out, "const ()").expect("write to String"),
        SemOpKind::ConstDuration(nanos) => {
            writeln!(out, "const {nanos}ns").expect("write to String");
        }
        SemOpKind::ConstStr(id) => {
            writeln!(out, "const.str #{}", id.0).expect("write to String");
        }
        SemOpKind::ConstBytes(id) => {
            writeln!(out, "const.bytes #{}", id.0).expect("write to String");
        }
        SemOpKind::StrEq { lhs, rhs } => {
            writeln!(out, "str.eq {}, {}", operand(lhs), operand(rhs)).expect("write to String");
        }
        SemOpKind::BytesEq { lhs, rhs } => {
            writeln!(out, "bytes.eq {}, {}", operand(lhs), operand(rhs)).expect("write to String");
        }
        SemOpKind::CopyValue { source } => {
            writeln!(out, "copy_value {}", operand(source)).expect("write to String");
        }
        SemOpKind::DestroyValue { value } => {
            writeln!(out, "destroy_value {}", operand(value)).expect("write to String");
        }
        SemOpKind::BeginBorrow { owner } => {
            writeln!(out, "begin_borrow {}", operand(owner)).expect("write to String");
        }
        SemOpKind::EndBorrow { borrow } => {
            writeln!(out, "end_borrow {}", operand(borrow)).expect("write to String");
        }
        SemOpKind::Move { source } => {
            writeln!(out, "move {}", operand(source)).expect("write to String");
        }
        SemOpKind::Fork { source } => {
            writeln!(out, "fork {}", operand(source)).expect("write to String");
        }
        SemOpKind::Destructure { shape, aggregate } => {
            writeln!(
                out,
                "aggregate.destructure {} {}",
                aggregate_shape(*shape),
                operand(aggregate)
            )
            .expect("write to String");
        }
        SemOpKind::AllocPlace { place } => {
            writeln!(out, "alloc_place $p{}", place.0).expect("write to String");
        }
        SemOpKind::LoadCopy { place } => {
            writeln!(out, "load.copy $p{}", place.0).expect("write to String");
        }
        SemOpKind::LoadTake { place } => {
            writeln!(out, "load.take $p{}", place.0).expect("write to String");
        }
        SemOpKind::StoreInit { place, value } => {
            writeln!(out, "store.init $p{}, {}", place.0, operand(value)).expect("write to String");
        }
        SemOpKind::StoreAssign { place, value } => {
            writeln!(out, "store.assign $p{}, {}", place.0, operand(value))
                .expect("write to String");
        }
        SemOpKind::EndLifetime { place } => {
            writeln!(out, "end_lifetime $p{}", place.0).expect("write to String");
        }
    }
}

fn dump_term(out: &mut String, module: &SemModule, term: &SemTerminator) {
    match term {
        SemTerminator::Return { value: Some(value) } => {
            writeln!(out, "    return {}", boundary_operand(value)).expect("write to String");
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
        SemTerminator::SwitchVariant {
            shape,
            scrutinee,
            arms,
            ..
        } => dump_variant_switch(out, *shape, scrutinee, arms),
        SemTerminator::CheckedBinary {
            op,
            lhs,
            rhs,
            result,
            normal,
            failures,
            ..
        } => dump_checked_binary(out, *op, lhs, rhs, result, normal, failures),
        SemTerminator::Call {
            callee,
            args,
            result,
            normal,
            unwind,
            ..
        } => {
            let target = module.callable(*callee).map_or_else(
                || format!("<invalid-callable:{}>", callee.0),
                |callable| callable.symbol.clone(),
            );
            dump_call(
                out,
                &format!("call @{target}"),
                args,
                result,
                normal,
                unwind,
            );
        }
        SemTerminator::RtCall {
            family,
            args,
            result,
            normal,
            unwind,
            ..
        } => dump_call(
            out,
            &format!("rt.call{{{family:?}}}"),
            args,
            result,
            normal,
            unwind,
        ),
        SemTerminator::Trap { kind } => {
            writeln!(out, "    trap{{{kind:?}}}").expect("write to String");
        }
        SemTerminator::Suspend {
            kind,
            inputs,
            resumes,
            cancel,
        } => dump_suspend(out, *kind, inputs, resumes, cancel),
        SemTerminator::ResumeUnwind => writeln!(out, "    resume_unwind").expect("write to String"),
        SemTerminator::Unreachable => writeln!(out, "    unreachable").expect("write to String"),
    }
}

fn dump_suspend(
    out: &mut String,
    kind: crate::SuspendKind,
    inputs: &[crate::BoundaryOperand],
    resumes: &[crate::Edge],
    cancel: &crate::Edge,
) {
    write!(out, "    suspend{{{kind:?}}}(").expect("write to String");
    for (index, input) in inputs.iter().enumerate() {
        if index != 0 {
            write!(out, ", ").expect("write to String");
        }
        write!(out, "{}", boundary_operand(input)).expect("write to String");
    }
    write!(out, ") resumes [").expect("write to String");
    for (index, edge) in resumes.iter().enumerate() {
        if index != 0 {
            write!(out, ", ").expect("write to String");
        }
        write!(out, "bb{}{}", edge.target.0, edge_args(edge)).expect("write to String");
    }
    writeln!(out, "] cancel bb{}{}", cancel.target.0, edge_args(cancel)).expect("write to String");
}

fn dump_variant_switch(
    out: &mut String,
    shape: crate::VariantShapeId,
    scrutinee: &crate::Operand,
    arms: &[crate::SemVariantArm],
) {
    write!(
        out,
        "    switch.variant #{} {} [",
        shape.0,
        operand(scrutinee)
    )
    .expect("write to String");
    for (index, arm) in arms.iter().enumerate() {
        if index != 0 {
            write!(out, ", ").expect("write to String");
        }
        write!(
            out,
            "{} => bb{}{}",
            arm.variant,
            arm.target.target.0,
            edge_args(&arm.target)
        )
        .expect("write to String");
    }
    writeln!(out, "]").expect("write to String");
}

fn dump_checked_binary(
    out: &mut String,
    op: hew_parser::ast::BinaryOp,
    lhs: &crate::Operand,
    rhs: &crate::Operand,
    result: &crate::ValueDef,
    normal: &crate::Edge,
    failures: &[crate::CheckedFailure],
) {
    write!(
        out,
        "    %{}{} = checked.binary{{{op}}} {}, {} normal bb{}{} failures [",
        result.id.0,
        own_suffix(result.own),
        operand(lhs),
        operand(rhs),
        normal.target.0,
        edge_args(normal)
    )
    .expect("write to String");
    for (index, failure) in failures.iter().enumerate() {
        if index != 0 {
            write!(out, ", ").expect("write to String");
        }
        write!(
            out,
            "{:?}: bb{}{}",
            failure.kind,
            failure.edge.target.0,
            edge_args(&failure.edge)
        )
        .expect("write to String");
    }
    writeln!(out, "]").expect("write to String");
}

fn dump_call(
    out: &mut String,
    name: &str,
    args: &[crate::BoundaryOperand],
    result: &CallResult,
    normal: &crate::Edge,
    unwind: &CallUnwind,
) {
    write!(out, "    {name}(").expect("write to String");
    for (index, arg) in args.iter().enumerate() {
        if index != 0 {
            write!(out, ", ").expect("write to String");
        }
        write!(out, "{}", boundary_operand(arg)).expect("write to String");
    }
    match result {
        CallResult::Unit => write!(out, ")"),
        CallResult::Value(value) => write!(
            out,
            ") -> %{}: {} [{:?}]",
            value.id.0,
            value.ty.user_facing(),
            value.own
        ),
    }
    .expect("write to String");
    write!(out, " normal bb{}{}", normal.target.0, edge_args(normal)).expect("write to String");
    match unwind {
        CallUnwind::NotApplicable => writeln!(out, " unwind none"),
        CallUnwind::Cleanup(edge) => {
            writeln!(out, " unwind bb{}{}", edge.target.0, edge_args(edge))
        }
    }
    .expect("write to String");
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
    // An operand has no mode: what a use does to its value is the op it feeds,
    // so the rendering is the value alone.
    format!("%{}", operand.value.0)
}

fn boundary_operand(input: &crate::BoundaryOperand) -> String {
    let decision = match input.decision {
        crate::BoundaryDecision::Borrow => "borrow",
        crate::BoundaryDecision::Copy => "copy",
        crate::BoundaryDecision::Move => "move",
        crate::BoundaryDecision::Snapshot(crate::SnapshotDecision::Share) => "snapshot.share",
        crate::BoundaryDecision::Snapshot(crate::SnapshotDecision::DeepCopy) => {
            "snapshot.deep_copy"
        }
        crate::BoundaryDecision::Snapshot(crate::SnapshotDecision::Transfer) => "snapshot.transfer",
    };
    format!("{decision} {}", operand(&input.operand))
}
