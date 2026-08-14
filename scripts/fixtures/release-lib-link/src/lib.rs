#[no_mangle]
pub extern "C" fn release_link_probe() -> i64 {
    // Force this consumer archive to carry the Rust standard library. This is
    // the ecosystem shape that used to expose libstd/personality collisions.
    String::from("release-link-ok").len() as i64
}
