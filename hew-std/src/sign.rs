//! Hew runtime: `sign` module.
//!
//! Ed25519 (RFC 8032) signing keys and verification for compiled Hew
//! programs. Key and signature material crosses the boundary as `bytes`.
//!
//! A signing key is a runtime-owned [`Ed25519Key`] that Hew holds only as an
//! opaque handle. It keeps the expanded form, the 32-byte clamped scalar
//! followed by the 32-byte nonce prefix (`SHA-512(seed)` with the scalar
//! clamped), and the public key derived from it. A seed, an expanded key and a
//! PKCS#8 document all normalize to that form, so signing has one path, and a
//! key exists only once its material checked out: signing cannot meet a
//! malformed key. The public key is never taken from the caller; signing one
//! key under two public keys leaks it.
//!
//! ## ABI
//!
//! `bytes` inputs arrive as `*const BytesTriple` and stay owned by the
//! caller; `bytes` results return a fresh `BytesTriple` by value. A
//! constructor returns null for malformed material; the Hew layer classifies
//! the material first, so null there is a contract breach. Secret bytes are
//! never written to messages, and a key's memory is zeroed when it is freed.
use ed25519_dalek::hazmat::{raw_sign, ExpandedSecretKey};
use ed25519_dalek::pkcs8::DecodePrivateKey;
use ed25519_dalek::{Digest, Sha512, SigningKey, VerifyingKey};
use ring::signature::{UnparsedPublicKey, ED25519};
use zeroize::Zeroizing;

type BytesTriple = hew_runtime::bytes::BytesTriple;

/// Length (bytes) of an Ed25519 seed.
pub const ED25519_SEED_LEN: usize = 32;

/// Length (bytes) of an expanded Ed25519 secret key (scalar ‖ prefix).
pub const ED25519_EXPANDED_LEN: usize = 64;

/// Length (bytes) of a raw Ed25519 public key.
pub const ED25519_PUBLIC_LEN: usize = 32;

/// Length (bytes) of an Ed25519 signature.
pub const ED25519_SIG_LEN: usize = 64;

/// `hew_ed25519_expanded_check` answers, mirrored by `KeyError` in
/// `std/crypto/sign/sign.hew`.
const EXPANDED_OK: i32 = 0;
const EXPANDED_BAD_LENGTH: i32 = 1;
const EXPANDED_NOT_CLAMPED: i32 = 2;
const EXPANDED_SEED_AND_PUBLIC: i32 = 3;

/// A checked signing key: its expanded material and the public key derived
/// from it.
pub struct Ed25519Key {
    expanded: Zeroizing<[u8; ED25519_EXPANDED_LEN]>,
    secret: ExpandedSecretKey,
    public: VerifyingKey,
}

impl std::fmt::Debug for Ed25519Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Never prints secret material.
        f.debug_struct("Ed25519Key")
            .field("public", &self.public)
            .finish_non_exhaustive()
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// A fresh caller-owned `bytes` value holding `bytes`.
fn bytes_from_slice(bytes: &[u8]) -> BytesTriple {
    let Ok(len) = u32::try_from(bytes.len()) else {
        return empty();
    };
    // SAFETY: `bytes` is valid for `len` bytes; `hew_bytes_from_static` copies it
    // into a fresh, refcount-1 bytes allocation owned by the Hew caller.
    unsafe { hew_runtime::bytes::hew_bytes_from_static(bytes.as_ptr(), len) }
}

/// The empty `bytes` value.
fn empty() -> BytesTriple {
    BytesTriple {
        ptr: std::ptr::null_mut(),
        offset: 0,
        len: 0,
    }
}

/// Borrow the active region of a by-pointer `BytesTriple`.
///
/// # Safety
///
/// `triple` is null or a live `BytesTriple` that outlives the slice.
unsafe fn view<'a>(triple: *const BytesTriple) -> &'a [u8] {
    // SAFETY: the caller passes null or a live triple.
    let Some(t) = (unsafe { triple.as_ref() }) else {
        return &[];
    };
    if t.len == 0 || t.ptr.is_null() {
        return &[];
    }
    // SAFETY: BytesTriple invariant: ptr+offset is valid for len bytes.
    unsafe { std::slice::from_raw_parts(t.ptr.add(t.offset as usize), t.len as usize) }
}

/// Whether `scalar` is an RFC 8032 clamped scalar: low three bits clear,
/// bit 255 clear and bit 254 set.
fn is_clamped(scalar: &[u8]) -> bool {
    scalar.len() == 32 && scalar[0].trailing_zeros() >= 3 && scalar[31] & 0b1100_0000 == 0b0100_0000
}

/// The expanded form of `seed`: `SHA-512(seed)` with the scalar clamped.
fn expand_seed(seed: &[u8; ED25519_SEED_LEN]) -> Zeroizing<[u8; ED25519_EXPANDED_LEN]> {
    let mut expanded = Zeroizing::new([0u8; ED25519_EXPANDED_LEN]);
    expanded.copy_from_slice(&Sha512::digest(seed));
    expanded[0] &= 0b1111_1000;
    expanded[31] &= 0b0111_1111;
    expanded[31] |= 0b0100_0000;
    expanded
}

/// Classify 64 bytes offered as an expanded key. A seed followed by its own
/// public key, the 64-byte form Go and libsodium store, is reported apart
/// from an unclamped scalar so it is never read as a different key.
fn check_expanded(material: &[u8]) -> i32 {
    let Ok(bytes) = <&[u8; ED25519_EXPANDED_LEN]>::try_from(material) else {
        return EXPANDED_BAD_LENGTH;
    };
    let seed: &[u8; ED25519_SEED_LEN] = bytes[..32].try_into().expect("32-byte half");
    if SigningKey::from_bytes(seed).verifying_key().as_bytes() == &bytes[32..] {
        return EXPANDED_SEED_AND_PUBLIC;
    }
    if !is_clamped(&bytes[..32]) {
        return EXPANDED_NOT_CLAMPED;
    }
    EXPANDED_OK
}

/// A key from checked expanded material, or `None` when it is malformed.
fn key_from_expanded(expanded: Zeroizing<[u8; ED25519_EXPANDED_LEN]>) -> Option<Ed25519Key> {
    if !is_clamped(&expanded[..32]) {
        return None;
    }
    let secret = ExpandedSecretKey::from_bytes(&expanded);
    let public = VerifyingKey::from(&secret);
    Some(Ed25519Key {
        expanded,
        secret,
        public,
    })
}

fn into_handle(key: Option<Ed25519Key>) -> *mut Ed25519Key {
    key.map_or(std::ptr::null_mut(), |key| Box::into_raw(Box::new(key)))
}

// ---------------------------------------------------------------------------
// BytesTriple-ABI entry points (called from std/crypto/sign/sign.hew)
// ---------------------------------------------------------------------------

/// A key from a 32-byte seed; null for any other length.
///
/// # Safety
///
/// `seed` is null or a live `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_ed25519_key_from_seed(seed: *const BytesTriple) -> *mut Ed25519Key {
    // SAFETY: forwarded caller contract.
    let Ok(seed) = <&[u8; ED25519_SEED_LEN]>::try_from(unsafe { view(seed) }) else {
        return std::ptr::null_mut();
    };
    into_handle(key_from_expanded(expand_seed(seed)))
}

/// Classify 64 bytes offered as an expanded key: 0 usable, 1 wrong length,
/// 2 scalar not clamped, 3 a seed followed by its own public key.
///
/// # Safety
///
/// `material` is null or a live `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_ed25519_expanded_check(material: *const BytesTriple) -> i32 {
    // SAFETY: forwarded caller contract.
    check_expanded(unsafe { view(material) })
}

/// A key from 64 bytes of expanded material that
/// [`hew_ed25519_expanded_check`] accepts; null otherwise.
///
/// # Safety
///
/// `expanded` is null or a live `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_ed25519_key_from_expanded(
    expanded: *const BytesTriple,
) -> *mut Ed25519Key {
    // SAFETY: forwarded caller contract.
    let material = unsafe { view(expanded) };
    if check_expanded(material) != EXPANDED_OK {
        return std::ptr::null_mut();
    }
    let mut bytes = Zeroizing::new([0u8; ED25519_EXPANDED_LEN]);
    bytes.copy_from_slice(material);
    into_handle(key_from_expanded(bytes))
}

/// A key from a PKCS#8 v1 or v2 Ed25519 document; null when the document
/// does not parse or its embedded public key does not match.
///
/// # Safety
///
/// `document` is null or a live `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_ed25519_key_from_pkcs8(
    document: *const BytesTriple,
) -> *mut Ed25519Key {
    // SAFETY: forwarded caller contract.
    match SigningKey::from_pkcs8_der(unsafe { view(document) }) {
        Ok(key) => into_handle(key_from_expanded(expand_seed(&key.to_bytes()))),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Whether a constructor produced a key.
#[no_mangle]
pub extern "C" fn hew_ed25519_key_is_valid(key: *const Ed25519Key) -> bool {
    !key.is_null()
}

/// Sign `message`, returning the 64-byte signature.
///
/// # Safety
///
/// `key` is a live key from a constructor; `message` is null or a live
/// `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_ed25519_key_sign(
    key: *const Ed25519Key,
    message: *const BytesTriple,
) -> BytesTriple {
    // SAFETY: forwarded caller contract.
    let (key, message) = unsafe { (&*key, view(message)) };
    bytes_from_slice(&raw_sign::<Sha512>(&key.secret, message, &key.public).to_bytes())
}

/// The key's 32-byte public key.
///
/// # Safety
///
/// `key` is a live key from a constructor.
#[no_mangle]
pub unsafe extern "C" fn hew_ed25519_key_public(key: *const Ed25519Key) -> BytesTriple {
    // SAFETY: forwarded caller contract.
    bytes_from_slice(unsafe { &*key }.public.as_bytes())
}

/// The key's 64-byte expanded secret material, for persisting it.
///
/// # Safety
///
/// `key` is a live key from a constructor.
#[no_mangle]
pub unsafe extern "C" fn hew_ed25519_key_expanded(key: *const Ed25519Key) -> BytesTriple {
    // SAFETY: forwarded caller contract.
    bytes_from_slice(&unsafe { &*key }.expanded[..])
}

/// Free a key, zeroing its secret material. Null is a no-op.
///
/// # Safety
///
/// `key` is null or a key from a constructor that is not used again.
#[no_mangle]
pub unsafe extern "C" fn hew_ed25519_key_free(key: *mut Ed25519Key) {
    if !key.is_null() {
        // SAFETY: a constructor produced this Box and the caller gives it up.
        drop(unsafe { Box::from_raw(key) });
    }
}

/// Verify an Ed25519 signature: `1` when valid, `0` for an invalid signature
/// or any malformed input (wrong lengths, non-canonical `S`).
///
/// # Safety
///
/// Each pointer is null or a live `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_ed25519_verify_hew(
    message: *const BytesTriple,
    signature: *const BytesTriple,
    public_key: *const BytesTriple,
) -> i32 {
    // SAFETY: forwarded caller contract for all three views.
    let (message, signature, public_key) =
        unsafe { (view(message), view(signature), view(public_key)) };
    i32::from(
        UnparsedPublicKey::new(&ED25519, public_key)
            .verify(message, signature)
            .is_ok(),
    )
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[allow(
    clippy::undocumented_unsafe_blocks,
    reason = "test module calls the FFI with live local triples throughout"
)]
mod tests {
    use super::*;

    fn hex(text: &str) -> Vec<u8> {
        (0..text.len())
            .step_by(2)
            .map(|at| u8::from_str_radix(&text[at..at + 2], 16).unwrap())
            .collect()
    }

    fn triple(data: &[u8]) -> BytesTriple {
        BytesTriple {
            ptr: data.as_ptr().cast_mut(),
            offset: 0,
            len: u32::try_from(data.len()).unwrap(),
        }
    }

    /// Copy and release one returned buffer.
    fn take(result: BytesTriple) -> Vec<u8> {
        let out = unsafe { view(&raw const result) }.to_vec();
        unsafe { hew_runtime::bytes::hew_bytes_drop(result.ptr) };
        out
    }

    /// An owned key handle, freed on drop.
    struct Key(*mut Ed25519Key);

    impl Drop for Key {
        fn drop(&mut self) {
            unsafe { hew_ed25519_key_free(self.0) };
        }
    }

    impl Key {
        fn from_seed(seed: &[u8]) -> Option<Self> {
            let seed = triple(seed);
            Self::checked(unsafe { hew_ed25519_key_from_seed(&raw const seed) })
        }

        fn from_expanded(expanded: &[u8]) -> Option<Self> {
            let expanded = triple(expanded);
            Self::checked(unsafe { hew_ed25519_key_from_expanded(&raw const expanded) })
        }

        fn checked(key: *mut Ed25519Key) -> Option<Self> {
            hew_ed25519_key_is_valid(key).then_some(Self(key))
        }

        fn public(&self) -> Vec<u8> {
            take(unsafe { hew_ed25519_key_public(self.0) })
        }

        fn expanded(&self) -> Vec<u8> {
            take(unsafe { hew_ed25519_key_expanded(self.0) })
        }

        fn sign(&self, message: &[u8]) -> Vec<u8> {
            let message = triple(message);
            take(unsafe { hew_ed25519_key_sign(self.0, &raw const message) })
        }
    }

    fn check(material: &[u8]) -> i32 {
        let material = triple(material);
        unsafe { hew_ed25519_expanded_check(&raw const material) }
    }

    fn verify(message: &[u8], signature: &[u8], public_key: &[u8]) -> bool {
        let (m, s, p) = (triple(message), triple(signature), triple(public_key));
        unsafe { hew_ed25519_verify_hew(&raw const m, &raw const s, &raw const p) == 1 }
    }

    /// RFC 8032 §7.1 tests 1–3: secret key, public key, message, signature.
    const RFC8032: [(&str, &str, &str, &str); 3] = [
        (
            "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60",
            "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a",
            "",
            "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b",
        ),
        (
            "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb",
            "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c",
            "72",
            "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00",
        ),
        (
            "c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7",
            "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025",
            "af82",
            "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a",
        ),
    ];

    #[test]
    fn rfc8032_vectors_sign_and_verify_from_the_seed() {
        for (seed, public_key, message, signature) in RFC8032 {
            let key = Key::from_seed(&hex(seed)).unwrap();
            assert_eq!(key.expanded().len(), ED25519_EXPANDED_LEN);
            assert_eq!(key.public(), hex(public_key));
            let signed = key.sign(&hex(message));
            assert_eq!(signed, hex(signature));
            assert!(verify(&hex(message), &signed, &hex(public_key)));
        }
    }

    #[test]
    fn expanded_key_from_another_implementation_signs_identically() {
        let (seed, public_key, message, signature) = RFC8032[2];
        // The SHA-512 form another library stores, clamped by its owner.
        let mut foreign = Sha512::digest(hex(seed)).to_vec();
        foreign[0] &= 0b1111_1000;
        foreign[31] = (foreign[31] & 0b0111_1111) | 0b0100_0000;
        let key = Key::from_expanded(&foreign).unwrap();
        assert_eq!(key.expanded(), foreign);
        assert_eq!(key.public(), hex(public_key));
        assert_eq!(key.sign(&hex(message)), hex(signature));
    }

    #[test]
    fn unclamped_or_short_expanded_keys_are_refused() {
        let mut expanded = Key::from_seed(&hex(RFC8032[0].0)).unwrap().expanded();
        assert_eq!(check(&expanded), EXPANDED_OK);
        assert_eq!(check(&expanded[..63]), EXPANDED_BAD_LENGTH);
        assert!(Key::from_expanded(&expanded[..63]).is_none());
        expanded[0] |= 1;
        assert_eq!(check(&expanded), EXPANDED_NOT_CLAMPED);
        assert!(Key::from_expanded(&expanded).is_none());
        assert!(Key::from_seed(&[0u8; 31]).is_none());
    }

    #[test]
    fn seed_followed_by_its_public_key_is_not_an_expanded_key() {
        // Find a seed that also passes the clamping test, as one in 32 do,
        // so only the seed-and-public check can tell the forms apart.
        let seed = (0u8..=255)
            .map(|fill| [fill; ED25519_SEED_LEN])
            .find(|seed| is_clamped(seed))
            .unwrap();
        let mut stored = seed.to_vec();
        stored.extend(Key::from_seed(&seed).unwrap().public());
        assert_eq!(check(&stored), EXPANDED_SEED_AND_PUBLIC);
        assert!(Key::from_expanded(&stored).is_none());
        // The same seed with any other second half is still an expanded key.
        stored[63] ^= 1;
        assert_eq!(check(&stored), EXPANDED_OK);
    }

    #[test]
    fn ring_pkcs8_document_loads_and_signs_verifiably() {
        use ring::signature::{Ed25519KeyPair, KeyPair};
        let rng = ring::rand::SystemRandom::new();
        let document = Ed25519KeyPair::generate_pkcs8(&rng).unwrap();
        let ring_key = Ed25519KeyPair::from_pkcs8(document.as_ref()).unwrap();
        let doc = triple(document.as_ref());
        let key = Key::checked(unsafe { hew_ed25519_key_from_pkcs8(&raw const doc) }).unwrap();
        assert_eq!(key.public(), ring_key.public_key().as_ref());
        assert_eq!(key.sign(b"hew"), ring_key.sign(b"hew").as_ref());
        let garbage = triple(b"not a document");
        assert!(Key::checked(unsafe { hew_ed25519_key_from_pkcs8(&raw const garbage) }).is_none());
    }

    #[test]
    fn verify_rejects_non_canonical_s_and_tampering() {
        let (seed, public_key, message, _) = RFC8032[1];
        let mut signed = Key::from_seed(&hex(seed)).unwrap().sign(&hex(message));
        assert!(verify(&hex(message), &signed, &hex(public_key)));
        assert!(!verify(b"other", &signed, &hex(public_key)));
        // S + L encodes the same scalar non-canonically; verification must
        // refuse it (signature malleability).
        let order = hex("edd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010");
        let mut carry = 0u16;
        for (byte, add) in signed[32..].iter_mut().zip(order) {
            let sum = u16::from(*byte) + u16::from(add) + carry;
            *byte = (sum & 0xff) as u8;
            carry = sum >> 8;
        }
        assert!(!verify(&hex(message), &signed, &hex(public_key)));
        assert!(!verify(&hex(message), &signed[..63], &hex(public_key)));
    }
}
