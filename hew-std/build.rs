//! Stamps the build identity into `libhew_std.a`.
//!
//! The WASM link line resolves this crate's staticlib directly rather than
//! through `hew-lib`'s umbrella archive, so the driver needs a digest inside it
//! to prove the two halves came from the same sources. See
//! `hew-build-identity` for the format.

fn main() {
    hew_build_identity::scan::emit_stamp("hew-std", hew_build_identity::STAMP_SYMBOL_STD);
}
