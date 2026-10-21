//! Cluster authority enrolment of existing transport identities.
//!
//! The installed bundle supplies the trust root. An incoming bundle can only
//! demonstrate membership under that root; it cannot select a different root.

use std::io;
use std::path::Path;

use ed25519_dalek::{Signature, Signer, SigningKey, VerifyingKey};
use hew_codec::Format;
use rand::RngExt as _;
use zeroize::Zeroizing;

use crate::node_protocol_generated::{ClusterEnrollment, ClusterMember};
use crate::peer_binding::{PeerCredential, TransportSelection};

const MAX_ENROLLMENT_BYTES: usize = 4096;
const AUTHORITY_MAGIC: &[u8; 8] = b"HEW-CA01";
const SIGNATURE_DOMAIN: &[u8] = b"hew-cluster-enrollment-v1\0";

fn invalid(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message)
}

fn authority_key(path: &Path, create: bool) -> io::Result<SigningKey> {
    fn check(bytes: &[u8]) -> io::Result<()> {
        if bytes.len() != AUTHORITY_MAGIC.len() + 32 || !bytes.starts_with(AUTHORITY_MAGIC) {
            return Err(invalid("invalid cluster authority key"));
        }
        Ok(())
    }
    let bytes = if create {
        crate::identity_file::load_or_create(
            path,
            AUTHORITY_MAGIC.len() + 32,
            || {
                let mut bytes = Zeroizing::new(Vec::with_capacity(AUTHORITY_MAGIC.len() + 32));
                bytes.extend_from_slice(AUTHORITY_MAGIC);
                let mut secret = Zeroizing::new([0_u8; 32]);
                rand::rng().fill(&mut *secret);
                bytes.extend_from_slice(&*secret);
                Ok(bytes)
            },
            check,
        )?
    } else {
        let bytes = crate::identity_file::load(path, AUTHORITY_MAGIC.len() + 32)?;
        check(&bytes)?;
        bytes
    };
    let mut secret = Zeroizing::new([0_u8; 32]);
    secret.copy_from_slice(&bytes[AUTHORITY_MAGIC.len()..]);
    Ok(SigningKey::from_bytes(&secret))
}

/// Load or create the private cluster authority and return its public key.
///
/// # Errors
/// Returns an error for invalid or insecure identity storage.
pub fn authority_identity(path: &Path) -> io::Result<String> {
    Ok(crate::peer_binding::hex_lower(
        authority_key(path, true)?.verifying_key().as_bytes(),
    ))
}

fn member(credential: &PeerCredential) -> ClusterMember {
    match credential {
        PeerCredential::NoiseKey(key) => ClusterMember {
            transport: 0,
            credential: key.to_vec(),
        },
        PeerCredential::Spki(spki) => ClusterMember {
            transport: 1,
            credential: spki.clone(),
        },
    }
}

fn signed_bytes(encoded: &[u8]) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(SIGNATURE_DOMAIN.len() + encoded.len());
    bytes.extend_from_slice(SIGNATURE_DOMAIN);
    bytes.extend_from_slice(encoded);
    bytes
}

fn decode(bytes: &[u8]) -> io::Result<(ClusterEnrollment, ClusterMember)> {
    if bytes.len() > MAX_ENROLLMENT_BYTES {
        return Err(invalid("cluster enrollment exceeds its size limit"));
    }
    let enrollment = ClusterEnrollment::decode(Format::Cbor, bytes)
        .map_err(|_| invalid("invalid cluster enrollment structure"))?;
    let member = ClusterMember::decode(Format::Cbor, &enrollment.member)
        .map_err(|_| invalid("invalid cluster member structure"))?;
    match member.transport {
        0 if member.credential.len() == 32 => {}
        1 if !member.credential.is_empty() => {}
        _ => return Err(invalid("invalid enrolled transport credential")),
    }
    Ok((enrollment, member))
}

fn verify(enrollment: &ClusterEnrollment, authority: &VerifyingKey) -> io::Result<()> {
    if enrollment.authority != authority.as_bytes() {
        return Err(invalid("cluster enrollment belongs to another authority"));
    }
    let signature = Signature::from_slice(&enrollment.signature)
        .map_err(|_| invalid("invalid cluster enrollment signature length"))?;
    authority
        .verify_strict(&signed_bytes(&enrollment.member), &signature)
        .map_err(|_| invalid("cluster enrollment signature refused"))
}

/// Issue an enrollment bundle for one existing transport credential.
///
/// # Errors
/// Returns an error for invalid credential bytes or authority storage.
pub fn issue(path: &Path, credential: &PeerCredential) -> io::Result<Vec<u8>> {
    let signing = authority_key(path, false)?;
    let member = member(credential).encode(Format::Cbor);
    let signature = signing.sign(&signed_bytes(&member)).to_bytes().to_vec();
    let bytes = ClusterEnrollment {
        authority: signing.verifying_key().to_bytes().to_vec(),
        member,
        signature,
    }
    .encode(Format::Cbor);
    decode(&bytes)?;
    Ok(bytes)
}

/// Publish an enrollment bundle without replacing a different existing bundle.
///
/// # Errors
/// Returns an error for invalid enrollment or filesystem publication failure.
pub fn write_bundle(path: &Path, bytes: &[u8]) -> io::Result<()> {
    let checked = Admission::from_bytes(bytes)?;
    let persisted = crate::identity_file::load_or_create(
        path,
        MAX_ENROLLMENT_BYTES,
        || Ok(Zeroizing::new(bytes.to_vec())),
        |bytes| {
            Admission::from_bytes(bytes)?;
            Ok(())
        },
    )?;
    if checked.bundle != *persisted {
        return Err(invalid("enrollment output already contains another bundle"));
    }
    Ok(())
}

/// Frozen cluster-root authority and the local node's enrollment.
#[derive(Clone, Debug)]
pub struct Admission {
    authority: VerifyingKey,
    credential: PeerCredential,
    bundle: Vec<u8>,
}

impl Admission {
    /// Read the operator-installed trust root and enrollment.
    ///
    /// # Errors
    /// Returns an error for invalid, oversized or insecure enrollment storage.
    pub fn load(path: &Path) -> io::Result<Self> {
        Self::from_bytes(&crate::identity_file::load(path, MAX_ENROLLMENT_BYTES)?)
    }

    fn from_bytes(bytes: &[u8]) -> io::Result<Self> {
        let (enrollment, member) = decode(bytes)?;
        let authority_bytes: [u8; 32] = enrollment
            .authority
            .as_slice()
            .try_into()
            .map_err(|_| invalid("invalid cluster authority public key length"))?;
        let authority = VerifyingKey::from_bytes(&authority_bytes)
            .map_err(|_| invalid("invalid cluster authority public key"))?;
        verify(&enrollment, &authority)?;
        let credential = match member.transport {
            0 => PeerCredential::NoiseKey(
                member
                    .credential
                    .as_slice()
                    .try_into()
                    .map_err(|_| invalid("invalid Noise credential length"))?,
            ),
            1 => PeerCredential::Spki(member.credential),
            _ => unreachable!("checked transport"),
        };
        Ok(Self {
            authority,
            credential,
            bundle: bytes.to_vec(),
        })
    }

    /// The locally installed enrollment, sent before route publication.
    #[must_use]
    pub fn bundle(&self) -> &[u8] {
        &self.bundle
    }

    /// The credential enrolled by the operator for this node.
    #[must_use]
    pub fn credential(&self) -> &PeerCredential {
        &self.credential
    }

    /// Verify membership under the installed root and bind key possession.
    ///
    /// # Errors
    /// Returns an error for another root, forged enrollment or wrong credential.
    pub fn verify_peer(&self, bytes: &[u8], credential: &PeerCredential) -> io::Result<()> {
        let (enrollment, enrolled_member) = decode(bytes)?;
        verify(&enrollment, &self.authority)?;
        if enrolled_member != member(credential) {
            return Err(invalid(
                "enrollment does not bind the authenticated credential",
            ));
        }
        Ok(())
    }

    /// The transport selected by this enrollment.
    #[must_use]
    pub fn transport(&self) -> TransportSelection {
        match self.credential {
            PeerCredential::NoiseKey(_) => TransportSelection::Tcp,
            PeerCredential::Spki(_) => TransportSelection::QuicMesh,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn common_root_admits_existing_credentials_and_rejects_substitution_or_forgery() {
        let directory = tempfile::tempdir().expect("authority directory");
        let authority = directory.path().join("authority.key");
        authority_identity(&authority).expect("create authority");
        let a = PeerCredential::NoiseKey([1; 32]);
        let b = PeerCredential::NoiseKey([2; 32]);
        let local =
            Admission::from_bytes(&issue(&authority, &a).expect("enroll a")).expect("local");
        let remote = issue(&authority, &b).expect("enroll b");
        local.verify_peer(&remote, &b).expect("same root");
        assert!(local.verify_peer(&remote, &a).is_err());
        let other_path = directory.path().join("other.key");
        authority_identity(&other_path).expect("create other authority");
        let other = issue(&other_path, &b).expect("other issuer");
        assert!(local.verify_peer(&other, &b).is_err());
        let (mut forged, mut forged_member) = decode(&remote).expect("record");
        forged_member.credential[0] ^= 1;
        forged.member = forged_member.encode(Format::Cbor);
        assert!(local.verify_peer(&forged.encode(Format::Cbor), &b).is_err());
        let mut trailing = remote;
        trailing.push(0);
        assert!(local.verify_peer(&trailing, &b).is_err());
    }

    #[test]
    fn enrollment_rejects_oversized_and_malformed_signed_bodies() {
        let directory = tempfile::tempdir().expect("authority directory");
        let authority = directory.path().join("authority.key");
        let credential = PeerCredential::NoiseKey([7; 32]);
        assert!(issue(&authority, &credential).is_err());
        assert!(
            !authority.exists(),
            "issuing cannot create a missing authority"
        );
        authority_identity(&authority).expect("authority");
        let bytes = issue(&authority, &credential).expect("issue");
        let (mut wire, _) = decode(&bytes).expect("decode");
        assert!(Admission::from_bytes(&vec![0; MAX_ENROLLMENT_BYTES + 1]).is_err());
        let original = wire.member.clone();
        // Add an unknown field to the generated two-field record.
        assert_eq!(wire.member[0], 0xa2);
        wire.member[0] = 0xa3;
        wire.member.extend_from_slice(&[0x18, 99, 0]);
        assert!(decode(&wire.encode(Format::Cbor)).is_err());
        wire.member = original;
        wire.member.push(0);
        assert!(decode(&wire.encode(Format::Cbor)).is_err());
    }

    #[test]
    fn hardened_authority_and_enrollment_files_preserve_identity() {
        let directory = tempfile::tempdir().expect("authority directory");
        let authority = directory.path().join("authority.key");
        let public = authority_identity(&authority).expect("create authority");
        assert_eq!(
            public,
            authority_identity(&authority).expect("reload authority")
        );
        let credential = PeerCredential::NoiseKey([3; 32]);
        let bytes = issue(&authority, &credential).expect("issue");
        let bundle = directory.path().join("member.enrollment");
        write_bundle(&bundle, &bytes).expect("write");
        write_bundle(&bundle, &bytes).expect("idempotent");
        let loaded = Admission::load(&bundle).expect("load");
        assert_eq!(loaded.credential(), &credential);
        assert_eq!(loaded.credential().node_id(), credential.node_id());
        let other = issue(&authority, &PeerCredential::NoiseKey([4; 32])).expect("other member");
        assert!(write_bundle(&bundle, &other).is_err());
        assert_eq!(
            Admission::load(&bundle).expect("preserved").credential(),
            &credential
        );
    }
}
