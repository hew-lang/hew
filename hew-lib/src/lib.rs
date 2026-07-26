// hew-lib: umbrella crate that re-exports hew-runtime + the hew-std standard
// library as a single staticlib. Cargo handles deduplication of shared
// dependencies.
//
// The extern crate declarations force Cargo to link each crate's #[no_mangle]
// FFI symbols into the output archive.

extern crate hew_runtime;
extern crate hew_std;

// Build identity stamp. `build.rs` hashes the hew-lib/hew-runtime/hew-std
// source set and emits a #[no_mangle] static carrying the digest, which the
// compiler driver reads back out of the archive before linking. Without it a
// month-old archive links against a fresh driver and fails with a wall of
// undefined hew_* symbols instead of a diagnosis.
include!(concat!(env!("OUT_DIR"), "/build_identity.rs"));
