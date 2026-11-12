//! Native code may override one debug table while the others use the fallback.
#![cfg(not(target_arch = "wasm32"))]

#[no_mangle]
pub static __hew_file_table: [u8; 16] = [42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

extern "C" {
    fn hew_debug_file_table() -> *const u8;
    fn hew_debug_site_table() -> *const u8;
    fn hew_debug_symtab() -> *const u8;
    fn hew_debug_has_debug_info() -> *const u8;
}

#[test]
fn strong_file_table_overrides_only_its_fallback() {
    // Link the runtime as Rust code as well as through its C accessors.
    let _ = hew_runtime::debug_accessors::hew_debug_last_fault();
    // SAFETY: each accessor returns a static table or its static fallback.
    unsafe {
        assert_eq!(*hew_debug_file_table(), 42);
        assert_eq!(*hew_debug_site_table(), 0);
        assert_eq!(*hew_debug_symtab(), 0);
        assert_eq!(*hew_debug_has_debug_info(), 0);
    }
}
