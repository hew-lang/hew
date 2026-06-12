# `std::crypto::encrypt`

Native AES-256-GCM backend for Hew's `std::crypto::encrypt` module.

- `seal(key, plaintext)` prepends a fresh random nonce to the ciphertext.
- `try_open(key, ciphertext)` returns `Result<string, CryptoError>` and reports
  authentication or malformed-ciphertext failures without aborting the process.
- `open(key, ciphertext)` authenticates before returning the UTF-8 plaintext.
  It is the explicit panicking convenience wrapper; use `try_open` for
  attacker-controlled ciphertext.
- This backend mirrors the sibling native-only crypto modules and is not linked
  into the WASM runtime.
