//! Borrowed native requests keep their inputs live through checked resumption.

use super::{Builder, CallResult, Edge, HirExpr, Operand, OwnKind, SemTerminator};
use crate::{BlockArg, BoundaryDecision, BoundaryOperand, SuspendKind, ValueDef};
use hew_types::runtime_call::AsyncIoOp;

impl Builder<'_, '_> {
    pub(super) fn lower_native_io(
        &mut self,
        expression: &HirExpr,
        operation: AsyncIoOp,
        args: &[&HirExpr],
    ) -> Result<Option<crate::ValueId>, String> {
        let result_ty = self.ty(&expression.ty);
        let argument_types = args.iter().map(|arg| self.ty(&arg.ty)).collect::<Vec<_>>();
        operation
            .contract()
            .instantiate(&argument_types, &result_ty)?;
        self.service.require_type_facts(&result_ty)?;
        let before: std::collections::HashSet<_> = self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let mut inputs = Vec::with_capacity(args.len());
        for (index, argument) in args.iter().enumerate() {
            let stable_tail = args[index + 1..]
                .iter()
                .all(|arg| Self::stable_argument_read(arg));
            inputs.push(BoundaryOperand {
                operand: self.lower_call_read(argument, &mut loans, stable_tail, true)?,
                decision: BoundaryDecision::Borrow,
            });
        }
        let live = self.owned_live.clone();
        let own = OwnKind::of_ty(&result_ty, self.service.checked_facts.rows())?;
        let raw = self.fresh_value();
        let value = self.fresh_value();
        let normal = self.new_block(vec![BlockArg {
            value,
            ty: result_ty.clone(),
            own,
        }]);
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::NativeIo { operation },
            inputs,
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: result_ty.clone(),
                own,
            }),
            resumes: vec![Edge {
                target: normal,
                args: vec![Operand { value: raw }],
            }],
            cancel: Edge {
                target: cancel,
                args: vec![],
            },
            unwind: Edge {
                target: unwind,
                args: vec![],
            },
        })?;
        // The physical operation guarantees producer quiescence before either
        // fault continuation can release a resource lent to the request.
        for cleanup in [cancel, unwind] {
            self.current = cleanup;
            self.owned_live = live.clone();
            self.end_call_loans(&loans)?;
            self.finish_fault_exit()?;
        }
        self.current = normal;
        self.owned_live = live;
        self.end_call_loans(&loans)?;
        let temporaries = self
            .owned_live
            .keys()
            .filter(|value| !before.contains(value))
            .copied()
            .collect::<Vec<_>>();
        if own == OwnKind::Owned {
            self.owned_live.insert(value, result_ty);
        }
        for temporary in temporaries.into_iter().rev() {
            self.emit_destroy(temporary)?;
        }
        Ok(Some(value))
    }
}
