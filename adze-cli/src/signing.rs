//! Ed25519 signing key management for package publishing.
//!
//! Supports generating adze-managed keypairs and computing fingerprints.
//! Keys are stored in `~/.adze/keys/`.

use std::fmt;
use std::path::{Path, PathBuf};

use ed25519_dalek::{Signer, SigningKey, Verifier, VerifyingKey};
use sha2::{Digest, Sha256};

/// Errors from key management and signing operations.
#[derive(Debug)]
pub enum SignError {
    /// An I/O error occurred.
    Io(std::io::Error),
    /// A key could not be decoded.
    InvalidKey(String),
    /// Signature verification failed.
    BadSignature,
    /// No signing key is configured.
    NoKey,
}

impl fmt::Display for SignError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "signing I/O error: {e}"),
            Self::InvalidKey(msg) => write!(f, "invalid key: {msg}"),
            Self::BadSignature => write!(f, "signature verification failed"),
            Self::NoKey => write!(f, "no signing key found; run `adze key generate`"),
        }
    }
}

impl std::error::Error for SignError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::InvalidKey(_) | Self::BadSignature | Self::NoKey => None,
        }
    }
}

impl From<std::io::Error> for SignError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

/// An Ed25519 keypair for signing packages.
#[derive(Debug)]
pub struct KeyPair {
    signing: SigningKey,
}

impl KeyPair {
    /// Generate a new random Ed25519 keypair.
    #[must_use]
    pub fn generate() -> Self {
        let mut rng = rand_core::OsRng;
        Self {
            signing: SigningKey::generate(&mut rng),
        }
    }

    /// Load a keypair from raw 32-byte secret key material.
    ///
    /// # Errors
    ///
    /// Returns [`SignError::InvalidKey`] if the bytes are not 32 bytes long.
    pub fn from_secret_bytes(bytes: &[u8]) -> Result<Self, SignError> {
        let arr: [u8; 32] = bytes.try_into().map_err(|_| {
            SignError::InvalidKey(format!("expected 32 bytes, got {}", bytes.len()))
        })?;
        Ok(Self {
            signing: SigningKey::from_bytes(&arr),
        })
    }

    /// Return the public key bytes (32 bytes).
    #[must_use]
    pub fn public_key_bytes(&self) -> [u8; 32] {
        self.signing.verifying_key().to_bytes()
    }

    /// Return the public key as a base64 string.
    #[must_use]
    pub fn public_key_base64(&self) -> String {
        use base64::Engine as _;
        base64::engine::general_purpose::STANDARD.encode(self.public_key_bytes())
    }

    /// Return the secret key bytes (32 bytes).
    #[must_use]
    pub fn secret_key_bytes(&self) -> &[u8; 32] {
        self.signing.as_bytes()
    }

    /// Compute the SHA-256 fingerprint of the public key.
    ///
    /// Returns a string like `"SHA256:xYzAbCdEf..."`.
    #[must_use]
    pub fn fingerprint(&self) -> String {
        compute_fingerprint(&self.public_key_bytes())
    }

    /// Sign a message, returning the signature as a hex string prefixed with
    /// `"ed25519:"`.
    #[must_use]
    pub fn sign(&self, message: &[u8]) -> String {
        let sig = self.signing.sign(message);
        format!("ed25519:{}", hex::encode(sig.to_bytes()))
    }
}

/// Compute the SHA-256 fingerprint of a public key.
///
/// Returns `"SHA256:{base64}"`.
#[must_use]
pub fn compute_fingerprint(public_key_bytes: &[u8]) -> String {
    use base64::Engine as _;
    let hash = Sha256::digest(public_key_bytes);
    let b64 = base64::engine::general_purpose::STANDARD_NO_PAD.encode(hash);
    format!("SHA256:{b64}")
}

/// Verify an Ed25519 signature.
///
/// - `message`: the original message bytes
/// - `signature`: hex-encoded signature (with or without `"ed25519:"` prefix)
/// - `public_key_bytes`: 32-byte public key
///
/// # Errors
///
/// Returns [`SignError::InvalidKey`] if the public key is invalid,
/// [`SignError::BadSignature`] if the signature is invalid or doesn't match.
pub fn verify(
    message: &[u8],
    signature: &str,
    public_key_bytes: &[u8; 32],
) -> Result<(), SignError> {
    let sig_hex = signature.strip_prefix("ed25519:").unwrap_or(signature);
    let sig_bytes = hex::decode(sig_hex)
        .map_err(|e| SignError::InvalidKey(format!("bad signature hex: {e}")))?;
    let sig_arr: [u8; 64] = sig_bytes
        .try_into()
        .map_err(|_| SignError::InvalidKey("signature must be 64 bytes".to_string()))?;
    let sig = ed25519_dalek::Signature::from_bytes(&sig_arr);
    let vk = VerifyingKey::from_bytes(public_key_bytes)
        .map_err(|e| SignError::InvalidKey(format!("bad public key: {e}")))?;
    vk.verify(message, &sig)
        .map_err(|_| SignError::BadSignature)
}

// ── Key storage ─────────────────────────────────────────────────────────────

/// Return the default key directory (`~/.adze/keys/`).
#[must_use]
pub fn default_key_dir() -> PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    PathBuf::from(home).join(".adze").join("keys")
}

/// Save a keypair to the key directory.
///
/// Creates `{dir}/id_ed25519` (secret, mode 0600) and `{dir}/id_ed25519.pub`
/// (public, base64-encoded).
///
/// # Errors
///
/// Returns [`SignError::Io`] if the files cannot be written.
pub fn save_keypair(dir: &Path, keypair: &KeyPair) -> Result<(), SignError> {
    std::fs::create_dir_all(dir)?;

    let secret_path = dir.join("id_ed25519");
    use base64::Engine as _;
    let secret_b64 = base64::engine::general_purpose::STANDARD.encode(keypair.secret_key_bytes());
    std::fs::write(&secret_path, &secret_b64)?;

    // Set file permissions to 0600 on Unix.
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt as _;
        std::fs::set_permissions(&secret_path, std::fs::Permissions::from_mode(0o600))?;
    }

    let pub_path = dir.join("id_ed25519.pub");
    std::fs::write(&pub_path, keypair.public_key_base64())?;

    Ok(())
}

/// Load a keypair from the key directory.
///
/// # Errors
///
/// Returns [`SignError::NoKey`] if no key exists, [`SignError::InvalidKey`] if
/// the key file is malformed.
pub fn load_keypair(dir: &Path) -> Result<KeyPair, SignError> {
    let secret_path = dir.join("id_ed25519");
    if !secret_path.exists() {
        return Err(SignError::NoKey);
    }
    let b64 = std::fs::read_to_string(&secret_path)?;
    use base64::Engine as _;
    let bytes = base64::engine::general_purpose::STANDARD
        .decode(b64.trim())
        .map_err(|e| SignError::InvalidKey(format!("bad base64: {e}")))?;
    KeyPair::from_secret_bytes(&bytes)
}

// We need hex encoding for signatures. Inline a minimal implementation to
// avoid pulling in another crate.
mod hex {
    /// Encode bytes as lowercase hex.
    pub fn encode(bytes: impl AsRef<[u8]>) -> String {
        bytes.as_ref().iter().fold(String::new(), |mut acc, b| {
            use std::fmt::Write as _;
            let _ = write!(acc, "{b:02x}");
            acc
        })
    }

    /// Decode a hex string into bytes.
    pub fn decode(s: &str) -> Result<Vec<u8>, String> {
        if s.len() % 2 != 0 {
            return Err("odd-length hex string".to_string());
        }
        (0..s.len())
            .step_by(2)
            .map(|i| {
                u8::from_str_radix(&s[i..i + 2], 16).map_err(|e| format!("bad hex at {i}: {e}"))
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_and_sign_verify() {
        let kp = KeyPair::generate();
        let msg = b"hello world";
        let sig = kp.sign(msg);

        assert!(sig.starts_with("ed25519:"));
        verify(msg, &sig, &kp.public_key_bytes()).unwrap();
    }

    #[test]
    fn verify_rejects_wrong_message() {
        let kp = KeyPair::generate();
        let sig = kp.sign(b"hello");
        let result = verify(b"wrong", &sig, &kp.public_key_bytes());
        assert!(matches!(result, Err(SignError::BadSignature)));
    }

    #[test]
    fn verify_rejects_wrong_key() {
        let kp1 = KeyPair::generate();
        let kp2 = KeyPair::generate();
        let sig = kp1.sign(b"hello");
        let result = verify(b"hello", &sig, &kp2.public_key_bytes());
        assert!(matches!(result, Err(SignError::BadSignature)));
    }

    #[test]
    fn fingerprint_is_deterministic() {
        let kp = KeyPair::generate();
        assert_eq!(kp.fingerprint(), kp.fingerprint());
        assert!(kp.fingerprint().starts_with("SHA256:"));
    }

    #[test]
    fn save_and_load_keypair() {
        let dir = tempfile::tempdir().unwrap();
        let kp = KeyPair::generate();
        let fp = kp.fingerprint();

        save_keypair(dir.path(), &kp).unwrap();
        let loaded = load_keypair(dir.path()).unwrap();

        assert_eq!(loaded.fingerprint(), fp);
        assert_eq!(loaded.public_key_bytes(), kp.public_key_bytes());

        // Verify signature made with original key using loaded key.
        let sig = kp.sign(b"test");
        verify(b"test", &sig, &loaded.public_key_bytes()).unwrap();
    }

    #[test]
    fn load_from_empty_dir_returns_no_key() {
        let dir = tempfile::tempdir().unwrap();
        let result = load_keypair(dir.path());
        assert!(matches!(result, Err(SignError::NoKey)));
    }

    #[test]
    fn from_secret_bytes_rejects_wrong_length() {
        let result = KeyPair::from_secret_bytes(&[0u8; 16]);
        assert!(matches!(result, Err(SignError::InvalidKey(_))));
    }

    #[test]
    fn hex_roundtrip() {
        let data = b"\x00\x01\x0f\x10\xff";
        let encoded = hex::encode(data);
        assert_eq!(encoded, "00010f10ff");
        let decoded = hex::decode(&encoded).unwrap();
        assert_eq!(decoded, data);
    }

    #[test]
    fn verify_without_prefix() {
        let kp = KeyPair::generate();
        let sig = kp.sign(b"msg");
        let hex_only = sig.strip_prefix("ed25519:").unwrap();
        verify(b"msg", hex_only, &kp.public_key_bytes()).unwrap();
    }

    // ── Checksum signing flow (mirrors publish/install pipeline) ────────

    #[test]
    fn sign_and_verify_checksum_string() {
        // This mirrors the exact flow: publisher signs a checksum string,
        // consumer verifies it using the public key from the registry.
        let kp = KeyPair::generate();
        let checksum = "sha256:b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9";

        // Publisher signs the checksum.
        let sig = kp.sign(checksum.as_bytes());
        assert!(sig.starts_with("ed25519:"));

        // Consumer verifies using public key bytes.
        verify(checksum.as_bytes(), &sig, &kp.public_key_bytes()).unwrap();
    }

    #[test]
    fn verify_rejects_tampered_checksum() {
        let kp = KeyPair::generate();
        let real_checksum = "sha256:abc123";
        let sig = kp.sign(real_checksum.as_bytes());

        // Attacker modifies the checksum (e.g., swapped tarball).
        let fake_checksum = "sha256:def456";
        let result = verify(fake_checksum.as_bytes(), &sig, &kp.public_key_bytes());
        assert!(matches!(result, Err(SignError::BadSignature)));
    }

    #[test]
    fn verify_rejects_forged_signature() {
        let real_publisher = KeyPair::generate();
        let attacker = KeyPair::generate();
        let checksum = "sha256:abc123";

        // Attacker signs with their own key.
        let forged_sig = attacker.sign(checksum.as_bytes());

        // Verification against the real publisher's key must fail.
        let result = verify(
            checksum.as_bytes(),
            &forged_sig,
            &real_publisher.public_key_bytes(),
        );
        assert!(matches!(result, Err(SignError::BadSignature)));
    }

    #[test]
    fn fingerprint_matches_public_key() {
        let kp = KeyPair::generate();
        let fp = kp.fingerprint();
        let fp2 = compute_fingerprint(&kp.public_key_bytes());
        assert_eq!(fp, fp2);
        assert!(fp.starts_with("SHA256:"));
        // SHA-256 produces 32 bytes → 43 base64 chars (no pad).
        assert!(fp.len() > 10);
    }

    #[test]
    fn verify_rejects_empty_signature() {
        let kp = KeyPair::generate();
        let result = verify(b"msg", "", &kp.public_key_bytes());
        assert!(result.is_err());
    }

    #[test]
    fn verify_rejects_truncated_signature() {
        let kp = KeyPair::generate();
        let sig = kp.sign(b"msg");
        let truncated = &sig[..20];
        let result = verify(b"msg", truncated, &kp.public_key_bytes());
        assert!(result.is_err());
    }
}
