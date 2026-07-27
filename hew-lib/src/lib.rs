// hew-lib: umbrella crate that re-exports hew-runtime + the hew-std standard
// library as a single staticlib. Cargo handles deduplication of shared
// dependencies.
//
// The extern crate declarations force Cargo to link each crate's #[no_mangle]
// FFI symbols into the output archive.

extern crate hew_runtime;
extern crate hew_std;
