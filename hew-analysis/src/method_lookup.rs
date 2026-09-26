use hew_types::check::{FnSig, SpanKey, TypeDef};
use hew_types::{method_resolution, Ty, TypeCheckOutput, TypeHead};
use std::collections::HashSet;

pub(crate) fn find_receiver_type(tc: &TypeCheckOutput, end_offset: usize) -> Option<&Ty> {
    let mut best: Option<(&SpanKey, &Ty)> = None;
    for (span_key, ty) in &tc.expr_types {
        // Analysis requests in this crate are for the root editor buffer.
        // Imported modules can have identical byte offsets and must not
        // displace the root receiver's type according to HashMap iteration.
        if span_key.module_idx != 0 {
            continue;
        }
        if span_key.end <= end_offset && span_key.end + 1 >= end_offset {
            match best {
                Some((prev, _)) if span_key.end > prev.end => {
                    best = Some((span_key, ty));
                }
                Some((prev, _))
                    if span_key.end == prev.end
                        && (span_key.end - span_key.start) < (prev.end - prev.start) =>
                {
                    best = Some((span_key, ty));
                }
                None => {
                    best = Some((span_key, ty));
                }
                _ => {}
            }
        }
    }
    best.map(|(_, ty)| ty)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn receiver_type_uses_the_editor_buffer_module() {
        let tc = TypeCheckOutput {
            expr_types: HashMap::from([
                (
                    SpanKey {
                        start: 12,
                        end: 13,
                        module_idx: 0,
                    },
                    Ty::I64,
                ),
                (
                    SpanKey {
                        start: 12,
                        end: 13,
                        module_idx: 1,
                    },
                    Ty::Bool,
                ),
            ]),
            ..TypeCheckOutput::default()
        };

        assert_eq!(find_receiver_type(&tc, 14), Some(&Ty::I64));
    }
}

pub(crate) fn collect_method_sigs_for_receiver(
    tc: &TypeCheckOutput,
    receiver_ty: &Ty,
) -> Vec<(String, FnSig)> {
    let methods = method_resolution::collect_method_sigs_for_receiver(
        &tc.defs,
        &tc.type_defs,
        tc.sigs(),
        receiver_ty,
    );
    let Ty::Named {
        head: TypeHead::Nominal(nominal),
        ..
    } = receiver_ty
    else {
        return methods;
    };
    let mut declared: HashSet<String> = tc
        .dispatch
        .methods_of(nominal.id)
        .map(|(name, _, _)| name.to_string())
        .collect();
    if let Some(definition) = tc.type_defs.get(&nominal.id) {
        declared.extend(definition.methods.keys().cloned());
    }
    methods
        .into_iter()
        .filter(|(name, _)| declared.contains(name))
        .collect()
}

pub(crate) fn lookup_method_sig(
    tc: &TypeCheckOutput,
    receiver_ty: &Ty,
    method: &str,
) -> Option<FnSig> {
    if let Ty::Named {
        head: TypeHead::Nominal(nominal),
        ..
    } = receiver_ty
    {
        let declared_in_type = tc
            .type_defs
            .get(&nominal.id)
            .is_some_and(|definition| definition.methods.contains_key(method));
        let declared_in_dispatch = tc
            .dispatch
            .methods_of(nominal.id)
            .any(|(name, _, _)| name.as_str() == method);
        if !declared_in_type && !declared_in_dispatch {
            return None;
        }
    }
    method_resolution::lookup_method_sig(&tc.defs, &tc.type_defs, tc.sigs(), receiver_ty, method)
}

pub(crate) fn lookup_type_def_for_receiver(
    tc: &TypeCheckOutput,
    receiver_ty: &Ty,
) -> Option<TypeDef> {
    if let Ty::Named {
        head: TypeHead::Nominal(nominal),
        ..
    } = receiver_ty
    {
        return tc.type_defs.get(&nominal.id).cloned();
    }
    method_resolution::lookup_type_def_for_receiver(&tc.defs, &tc.type_defs, receiver_ty)
}
