//! Stamps the runtime + stdlib build identity into `libhew.a` / `hew.lib`.
//!
//! The driver bakes the same digest in at its own build time and refuses to
//! link an archive whose stamp disagrees, so a freshly built `hew` can never
//! silently pair with a stale archive. See `hew-build-identity` for the format.

fn main() {
    hew_build_identity::scan::emit_stamp("hew-lib", hew_build_identity::STAMP_SYMBOL);
}
