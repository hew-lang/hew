# `std.crypto.encrypt`

Pure-Rust AES-256-GCM backend for Hew's `std.crypto.encrypt` module across
native targets and `wasm32-wasip1`.

- `seal(key, plaintext)` prepends a fresh random nonce to the ciphertext.
- `open(key, ciphertext)` returns `Result<string, CryptoError>` and reports
  authentication or malformed-ciphertext failures without aborting the process.
