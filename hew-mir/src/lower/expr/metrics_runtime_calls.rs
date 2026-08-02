use super::{Builder, MirDiagnostic, MirDiagnosticKind, Place, ResolvedTy, RuntimeCallContext};

impl Builder {
    pub(super) fn lower_observe_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        let (expected_arity, return_ty) = match symbol {
            "hew_observe_read_u64" => (1, ResolvedTy::I64),
            "hew_observe_scrape" | "hew_observe_series" => (0, ResolvedTy::String),
            "hew_observe_barrier" => (0, ResolvedTy::I64),
            _ => unreachable!("observe lowering called for non-observe symbol"),
        };
        if hir_args.len() != expected_arity {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects {expected_arity} argument(s), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let mut args = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            args.push(self.lower_value(arg)?);
        }
        // A transferred `string` result must materialise even in statement
        // position. The shared fresh-owner temporary pass then sees the
        // audited producer and emits its one `hew_string_drop` on the normal
        // continuation. Omitting this destination used to discard the raw
        // pointer before that machinery could account for it (a leak), while
        // an observe-specific drop here would bypass the authority used by
        // every other transferred extern result.
        let dest = if matches!(return_ty, ResolvedTy::String)
            || context == RuntimeCallContext::ValueNeeded
        {
            Some(self.alloc_local(return_ty))
        } else {
            None
        };
        self.push_runtime_call(symbol, args, dest);
        dest
    }

    /// Lower the scalar `hew_metric_*` emit path that `std::metrics` reaches
    /// through its `extern "C"` block.
    ///
    /// Only the scalar surface routes here: a metric name comes in as a Hew
    /// `string` (lowered like any other string place), a handle and counter /
    /// gauge value as `i64`, and a histogram observation as `f64`. The register
    /// entry points return the `i64` slot handle (>= 0 valid, -1 on a cap /
    /// charset / collision failure); the mutators return unit.
    ///
    /// The labelled `*Vec` registration and the bucketed histogram registration
    /// take raw C arrays (`*const i64` / `*const *const c_char` plus a length)
    /// and are intentionally NOT routed here — a Hew `extern "C"` declaration
    /// marshals a `Vec<T>` to a single `*mut HewVec`, which does not match the
    /// `(ptr, len)` ABI those symbols expose. They stay fail-closed in the
    /// dispatch table until a `HewVec`-shaped ABI lands.
    pub(super) fn lower_metric_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        let (expected_arity, return_ty) = match symbol {
            "hew_metric_counter_register"
            | "hew_metric_gauge_register"
            | "hew_metric_histogram_register_simple" => (1, Some(ResolvedTy::I64)),
            "hew_metric_counter_inc" | "hew_metric_gauge_inc" | "hew_metric_gauge_dec" => (1, None),
            "hew_metric_counter_add"
            | "hew_metric_gauge_set"
            | "hew_metric_gauge_add"
            | "hew_metric_histogram_record" => (2, None),
            _ => unreachable!("metric lowering called for non-scalar-metric symbol `{symbol}`"),
        };
        if hir_args.len() != expected_arity {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects {expected_arity} argument(s), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let mut args = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            args.push(self.lower_value(arg)?);
        }
        let dest = match return_ty {
            Some(ty) if context == RuntimeCallContext::ValueNeeded => Some(self.alloc_local(ty)),
            _ => None,
        };
        self.push_runtime_call(symbol, args, dest);
        dest
    }
}
