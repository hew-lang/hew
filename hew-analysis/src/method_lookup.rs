use hew_types::check::{FnSig, SpanKey, TypeDef};
use hew_types::{method_resolution, Ty, TypeCheckOutput};

pub(crate) fn find_receiver_type(tc: &TypeCheckOutput, end_offset: usize) -> Option<&Ty> {
    let mut best: Option<(&SpanKey, &Ty)> = None;
    for (span_key, ty) in &tc.expr_types {
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

pub(crate) fn collect_method_sigs_for_receiver(
    tc: &TypeCheckOutput,
    receiver_ty: &Ty,
) -> Vec<(String, FnSig)> {
    method_resolution::collect_method_sigs_for_receiver(&tc.type_defs, &tc.fn_sigs, receiver_ty)
}

pub(crate) fn lookup_method_sig(
    tc: &TypeCheckOutput,
    receiver_ty: &Ty,
    method: &str,
) -> Option<FnSig> {
    method_resolution::lookup_method_sig(&tc.type_defs, &tc.fn_sigs, receiver_ty, method)
}

pub(crate) fn lookup_type_def_for_receiver(
    tc: &TypeCheckOutput,
    receiver_ty: &Ty,
) -> Option<TypeDef> {
    method_resolution::lookup_type_def_for_receiver(&tc.type_defs, receiver_ty)
}
