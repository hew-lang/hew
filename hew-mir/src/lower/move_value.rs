use super::{BindingId, Builder, HirExpr, Place};

impl Builder {
    /// Lower a complete value crossing an ownership boundary. Captured owned
    /// bindings alias still-owning environment fields, so moving one would
    /// create a second drop authority. Borrow and projection roots bypass this
    /// funnel and continue through `lower_value`.
    pub(crate) fn lower_value_for_move(&mut self, expr: &HirExpr) -> Option<Place> {
        if self.reject_capture_env_whole_escape_expr(expr) {
            return None;
        }
        self.lower_value(expr)
    }

    pub(crate) fn lower_let_value(&mut self, binding: BindingId, value: &HirExpr) -> Option<Place> {
        let generator_clone_only = self.prepass_generator_capture_bindings.contains(&binding)
            && !self.prepass_binding_ref_uses.contains(&binding);
        if generator_clone_only {
            self.lower_value(value)
        } else {
            self.lower_value_for_move(value)
        }
    }

    pub(crate) fn lower_method_arg_value(&mut self, arg: &HirExpr, is_move: bool) -> Option<Place> {
        if is_move {
            self.lower_value_for_move(arg)
        } else {
            self.lower_value(arg)
        }
    }

    pub(crate) fn lower_direct_call_args(
        &mut self,
        callee_item: Option<hew_hir::ItemId>,
        args: &[HirExpr],
    ) -> Option<Vec<Place>> {
        args.iter()
            .enumerate()
            .map(|(index, arg)| {
                let is_move = callee_item.is_some_and(|item| {
                    self.param_ownership
                        .call_param_consume
                        .get(&(item, index))
                        .copied()
                        == Some(true)
                });
                self.lower_method_arg_value(arg, is_move)
            })
            .collect()
    }
}
