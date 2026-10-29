use super::channel_common::abort_elem_witness;
use hew_cabi::vec::{HewTypeOwnershipKind, HewValueLayout};
/// Release one queue envelope that never reached a consumer.
///
/// Plain, string, and bytes envelopes own only their `Vec<u8>` storage. A
/// layout-managed envelope additionally owns the deep value cloned into its
/// bytes, so its witness drop thunk must run exactly once before the bytes are
/// discarded.
#[allow(dead_code, reason = "used by the wasm32 channel backing")]
pub(crate) fn drop_elem_envelope(
    layout: Option<&HewValueLayout>,
    envelope: Vec<u8>,
    context: &str,
) {
    let Some(layout) = layout else {
        return;
    };
    if layout.ownership_kind != HewTypeOwnershipKind::LayoutManaged {
        return;
    }
    if envelope.len() != layout.size {
        abort_elem_witness(
            context,
            "owned envelope size does not match the stamped witness",
        );
    }
    // SAFETY: the envelope transfers its initialized owner into aligned storage.
    // Synchronous foreign callers must supply a non-suspending release recipe.
    unsafe {
        let cursor =
            crate::release_walker::HewReleaseCursor::detached(envelope.as_ptr().cast(), *layout);
        drop(envelope);
        crate::release_walker::hew_release_sync(cursor);
    }
}
