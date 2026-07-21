use super::{BindingId, Builder, CaptureEnvOwnedLoad, HirExpr, Instr, Place, SiteId};

impl Builder {
    pub(crate) fn lower_capture_env_binding_ref(
        &mut self,
        binding: BindingId,
        name: &str,
        site: SiteId,
    ) -> Option<Place> {
        let source = self.capture_env_sources.get(&binding).cloned()?;
        let dest = self.alloc_local(source.ty.clone());
        if self.capture_env_whole_escape_requires_clone(&source.ty) {
            self.capture_env_owned_loads.insert(
                dest,
                CaptureEnvOwnedLoad {
                    name: name.to_string(),
                    ty: source.ty.clone(),
                    site,
                },
            );
        }
        self.push_instr(Instr::ClosureEnvFieldLoad {
            env: source.env,
            env_ty: source.env_ty,
            field_offset: source.field_offset,
            dest,
        });
        Some(dest)
    }

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
            let place = self.lower_value(value)?;
            // The generator-capture prepass immediately deep-clones this staged
            // binding into the generator environment; the local is not otherwise
            // read. Preserve that proven clone-only ingress by clearing the
            // whole-capture tag before the let-binding's Move.
            self.capture_env_owned_loads.remove(&place);
            Some(place)
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
        callee_symbol: &str,
        callee_item: Option<hew_hir::ItemId>,
        args: &[HirExpr],
    ) -> Option<Vec<Place>> {
        // Supervisor bootstrap parameters own the spawned config snapshot even
        // though the synthesized function has no source `ItemId`.
        let move_all_args = self
            .supervisor_layout_map
            .values()
            .any(|layout| layout.bootstrap_symbol == callee_symbol);
        args.iter()
            .enumerate()
            .map(|(index, arg)| {
                let is_move = move_all_args
                    || callee_item.is_some_and(|item| {
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
