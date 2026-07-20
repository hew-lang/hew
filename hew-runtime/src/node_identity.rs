//! Key-derived node identity and carried actor-location values.

use std::fmt;
use std::str::FromStr;

use sha2::{Digest, Sha256};

const NODE_ID_DOMAIN: &[u8] = b"hew-node-id-v1\0";
const NODE_ID_BYTES: usize = 16;
const NODE_ID_HEX_LEN: usize = NODE_ID_BYTES * 2;

/// Credential namespace used when deriving a [`NodeId`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum CredentialKind {
    /// Noise-XX X25519 static public key.
    NoiseStaticKey = 1,
    /// DER-encoded TLS leaf `SubjectPublicKeyInfo`.
    Spki = 2,
}

/// C-compatible two-word node identifier.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[repr(C)]
pub struct HewNodeId {
    /// Most-significant 64 bits, interpreted in network byte order.
    pub hi: u64,
    /// Least-significant 64 bits, interpreted in network byte order.
    pub lo: u64,
}

/// Immutable key-derived node identifier.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct NodeId(HewNodeId);

impl NodeId {
    /// Derive an identity from a Noise static public key.
    #[must_use]
    pub fn from_noise_static_key(public_key: &[u8; 32]) -> Self {
        Self::derive(CredentialKind::NoiseStaticKey, public_key)
    }

    /// Derive an identity from a canonical DER-encoded TLS leaf SPKI.
    #[must_use]
    pub fn from_spki(spki: &[u8]) -> Self {
        Self::derive(CredentialKind::Spki, spki)
    }

    /// Derive an identity from canonical credential bytes.
    ///
    /// # Panics
    ///
    /// Panics when `credential` is larger than the protocol's `u32` length
    /// field. Production Noise and SPKI credentials are bounded far below it.
    #[must_use]
    pub fn derive(kind: CredentialKind, credential: &[u8]) -> Self {
        let length = u32::try_from(credential.len())
            .expect("canonical node credentials are bounded to u32 length");
        let mut hasher = Sha256::new();
        hasher.update(NODE_ID_DOMAIN);
        hasher.update([kind as u8]);
        hasher.update(length.to_be_bytes());
        hasher.update(credential);
        let digest = hasher.finalize();
        let mut bytes = [0_u8; NODE_ID_BYTES];
        bytes.copy_from_slice(&digest[..NODE_ID_BYTES]);
        Self::from_bytes(bytes)
    }

    /// Construct from the canonical 16-byte network representation.
    #[must_use]
    pub const fn from_bytes(bytes: [u8; NODE_ID_BYTES]) -> Self {
        let hi = u64::from_be_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]);
        let lo = u64::from_be_bytes([
            bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15],
        ]);
        Self(HewNodeId { hi, lo })
    }

    /// Return the canonical 16-byte network representation.
    #[must_use]
    pub const fn to_bytes(self) -> [u8; NODE_ID_BYTES] {
        let hi = self.0.hi.to_be_bytes();
        let lo = self.0.lo.to_be_bytes();
        [
            hi[0], hi[1], hi[2], hi[3], hi[4], hi[5], hi[6], hi[7], lo[0], lo[1], lo[2], lo[3],
            lo[4], lo[5], lo[6], lo[7],
        ]
    }

    /// Most-significant 64-bit word.
    #[must_use]
    pub const fn hi(self) -> u64 {
        self.0.hi
    }

    /// Least-significant 64-bit word.
    #[must_use]
    pub const fn lo(self) -> u64 {
        self.0.lo
    }
}

impl From<NodeId> for HewNodeId {
    fn from(value: NodeId) -> Self {
        value.0
    }
}

impl From<HewNodeId> for NodeId {
    fn from(value: HewNodeId) -> Self {
        Self(value)
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:016x}{:016x}", self.0.hi, self.0.lo)
    }
}

/// Failure to parse a canonical node identity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ParseNodeIdError {
    /// The input was not exactly 32 hexadecimal digits.
    InvalidLength,
    /// The input contained a non-hexadecimal byte.
    InvalidHex,
}

impl fmt::Display for ParseNodeIdError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidLength => formatter.write_str("node id must be exactly 32 hex digits"),
            Self::InvalidHex => formatter.write_str("node id contains a non-hexadecimal digit"),
        }
    }
}

impl std::error::Error for ParseNodeIdError {}

impl FromStr for NodeId {
    type Err = ParseNodeIdError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let input = input.as_bytes();
        if input.len() != NODE_ID_HEX_LEN {
            return Err(ParseNodeIdError::InvalidLength);
        }
        let mut bytes = [0_u8; NODE_ID_BYTES];
        for (index, pair) in input.chunks_exact(2).enumerate() {
            bytes[index] = (hex_nibble(pair[0])? << 4) | hex_nibble(pair[1])?;
        }
        Ok(Self::from_bytes(bytes))
    }
}

fn hex_nibble(byte: u8) -> Result<u8, ParseNodeIdError> {
    match byte {
        b'0'..=b'9' => Ok(byte - b'0'),
        b'a'..=b'f' => Ok(byte - b'a' + 10),
        b'A'..=b'F' => Ok(byte - b'A' + 10),
        _ => Err(ParseNodeIdError::InvalidHex),
    }
}

/// C-compatible carried actor location.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[repr(C)]
pub struct HewLocation {
    /// Key-derived node identity.
    pub node: HewNodeId,
    /// Node-local monotonic actor slot.
    pub slot: u64,
    /// Durable node-session incarnation.
    pub incarnation: u32,
    /// Must be zero.
    pub reserved: u32,
}

/// Immutable carried actor location.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct Location(HewLocation);

impl Location {
    /// Construct a valid carried actor location.
    ///
    /// # Errors
    ///
    /// Returns an error when `slot` or `incarnation` is zero.
    pub fn new(node: NodeId, slot: u64, incarnation: u32) -> Result<Self, InvalidLocation> {
        if slot == 0 {
            return Err(InvalidLocation::ZeroSlot);
        }
        if incarnation == 0 {
            return Err(InvalidLocation::ZeroIncarnation);
        }
        Ok(Self(HewLocation {
            node: node.into(),
            slot,
            incarnation,
            reserved: 0,
        }))
    }

    /// Node identity projection.
    #[must_use]
    pub const fn node(self) -> NodeId {
        NodeId(self.0.node)
    }

    /// Node-local actor slot projection.
    #[must_use]
    pub const fn slot(self) -> u64 {
        self.0.slot
    }

    /// Durable node-session projection.
    #[must_use]
    pub const fn incarnation(self) -> u32 {
        self.0.incarnation
    }
}

impl From<Location> for HewLocation {
    fn from(value: Location) -> Self {
        value.0
    }
}

impl TryFrom<HewLocation> for Location {
    type Error = InvalidLocation;

    fn try_from(value: HewLocation) -> Result<Self, Self::Error> {
        if value.reserved != 0 {
            return Err(InvalidLocation::NonZeroReserved);
        }
        Self::new(value.node.into(), value.slot, value.incarnation)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{}/{:}@{}",
            NodeId(self.0.node),
            self.0.slot,
            self.0.incarnation
        )
    }
}

/// Invalid carried-location representation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InvalidLocation {
    /// Actor slot zero is reserved.
    ZeroSlot,
    /// Node-session incarnation zero is invalid.
    ZeroIncarnation,
    /// A C-ABI caller supplied non-zero reserved bits.
    NonZeroReserved,
}

impl fmt::Display for InvalidLocation {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ZeroSlot => formatter.write_str("location actor slot must be nonzero"),
            Self::ZeroIncarnation => {
                formatter.write_str("location session incarnation must be nonzero")
            }
            Self::NonZeroReserved => formatter.write_str("location reserved field must be zero"),
        }
    }
}

impl std::error::Error for InvalidLocation {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixed_noise_derivation_vector() {
        let id = NodeId::from_noise_static_key(&[0_u8; 32]);
        assert_eq!(id.to_string(), "8a1fb99bf51fb5f5db7c18af66971a4e");
        assert_eq!(id.hi(), 0x8a1f_b99b_f51f_b5f5);
        assert_eq!(id.lo(), 0xdb7c_18af_6697_1a4e);
    }

    #[test]
    fn fixed_spki_derivation_vector() {
        let id = NodeId::from_spki(&[0x30, 0x03, 0x01, 0x02, 0x03]);
        assert_eq!(id.to_string(), "a853e3f85b7d5090c4d374f55e9cffc9");
    }

    #[test]
    fn credential_kinds_are_domain_separated() {
        let bytes = b"credential";
        assert_eq!(
            NodeId::derive(CredentialKind::NoiseStaticKey, bytes).to_string(),
            "65633aea619b679c2d4ffb24533083e8"
        );
        assert_eq!(
            NodeId::derive(CredentialKind::Spki, bytes).to_string(),
            "b0d49c7f06bd2a9cea07dd3a0173cb24"
        );
    }

    #[test]
    fn node_id_display_parse_round_trip() {
        let id = NodeId::from_bytes([
            0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54,
            0x32, 0x10,
        ]);
        assert_eq!(id.to_string(), "0123456789abcdeffedcba9876543210");
        assert_eq!(id.to_string().parse::<NodeId>(), Ok(id));
        assert_eq!("0123456789ABCDEFFEDCBA9876543210".parse::<NodeId>(), Ok(id));
        assert_eq!("00".parse::<NodeId>(), Err(ParseNodeIdError::InvalidLength));
        assert_eq!(
            "0123456789abcdeffedcba987654321g".parse::<NodeId>(),
            Err(ParseNodeIdError::InvalidHex)
        );
    }

    #[test]
    fn location_rejects_reserved_values_and_formats_canonically() {
        let node = NodeId::from_bytes([0x11; 16]);
        assert_eq!(Location::new(node, 0, 1), Err(InvalidLocation::ZeroSlot));
        assert_eq!(
            Location::new(node, 1, 0),
            Err(InvalidLocation::ZeroIncarnation)
        );
        let location = Location::new(node, u64::MAX, u32::MAX).unwrap();
        assert_eq!(
            location.to_string(),
            "11111111111111111111111111111111/18446744073709551615@4294967295"
        );
        let mut raw: HewLocation = location.into();
        raw.reserved = 1;
        assert_eq!(
            Location::try_from(raw),
            Err(InvalidLocation::NonZeroReserved)
        );
    }

    #[test]
    fn c_layouts_are_fixed_width() {
        assert_eq!(std::mem::size_of::<HewNodeId>(), 16);
        assert_eq!(std::mem::align_of::<HewNodeId>(), 8);
        assert_eq!(std::mem::size_of::<NodeId>(), 16);
        assert_eq!(std::mem::size_of::<HewLocation>(), 32);
        assert_eq!(std::mem::align_of::<HewLocation>(), 8);
        assert_eq!(std::mem::size_of::<Location>(), 32);
    }
}
