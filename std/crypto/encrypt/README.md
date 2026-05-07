# `std::crypto::encrypt`

Native AES-256-GCM backend for Hew's `std::crypto::encrypt` module.

- `seal(key, plaintext)` prepends a fresh random nonce to the ciphertext.
- `open(key, ciphertext)` authenticates before returning the UTF-8 plaintext.
- This backend mirrors the sibling native-only crypto modules and is not linked
  into the WASM runtime.
