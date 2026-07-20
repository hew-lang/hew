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

#[cfg(not(target_arch = "wasm32"))]
mod session_journal {
    use std::fmt;
    use std::fs::{File, OpenOptions};
    use std::io::{self, Read, Seek, SeekFrom, Write};
    use std::mem::ManuallyDrop;
    use std::path::{Path, PathBuf};
    use std::ptr::NonNull;

    use fd_lock::{RwLock, RwLockWriteGuard};
    use sha2::{Digest, Sha256};

    use super::NodeId;

    const JOURNAL_MAGIC: [u8; 8] = *b"HEWID\x02\0\0";
    const JOURNAL_CHECKSUM_DOMAIN: &[u8] = b"hew-node-state-v1\0";
    const RECORD_SIZE: usize = 64;
    const JOURNAL_SIZE: usize = RECORD_SIZE * 2;

    /// Exclusive lease on one identity path's durable session journal.
    ///
    /// Dropping the final owner unlocks the journal, so the lease must live in
    /// the same frozen node snapshot that owns the session incarnation.
    pub struct NodeSessionLease {
        incarnation: u32,
        journal_path: PathBuf,
        _file_lock: SessionFileLock,
    }

    impl NodeSessionLease {
        /// Lock `identity_path.hew-state`, durably advance its session, and keep
        /// the lock until this lease is dropped.
        ///
        /// # Errors
        ///
        /// Returns an error for I/O failure, concurrent use, invalid records,
        /// identity mismatch, or counter exhaustion.
        pub fn acquire(identity_path: &Path, node_id: NodeId) -> Result<Self, SessionJournalError> {
            let journal_path = journal_path(identity_path);
            let file = OpenOptions::new()
                .create(true)
                .read(true)
                .write(true)
                .truncate(false)
                .open(&journal_path)
                .map_err(|source| SessionJournalError::Io {
                    operation: "open",
                    path: journal_path.clone(),
                    source,
                })?;
            let mut file_lock = SessionFileLock::try_acquire(file, &journal_path)?;
            let incarnation = advance_journal(file_lock.file_mut(), &journal_path, node_id)?;
            Ok(Self {
                incarnation,
                journal_path,
                _file_lock: file_lock,
            })
        }

        /// Durable session incarnation reserved by this lease.
        #[must_use]
        pub const fn incarnation(&self) -> u32 {
            self.incarnation
        }

        /// Journal path held by this lease.
        #[must_use]
        pub fn journal_path(&self) -> &Path {
            &self.journal_path
        }
    }

    impl fmt::Debug for NodeSessionLease {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter
                .debug_struct("NodeSessionLease")
                .field("incarnation", &self.incarnation)
                .field("journal_path", &self.journal_path)
                .finish_non_exhaustive()
        }
    }

    struct SessionFileLock {
        guard: ManuallyDrop<RwLockWriteGuard<'static, File>>,
        owner: NonNull<RwLock<File>>,
    }

    // SAFETY: `owner` is an exclusively owned heap allocation, `guard` is its
    // only live mutable borrow, and no file access occurs after construction.
    // The final owner may release the OS lock and allocation on any thread.
    unsafe impl Send for SessionFileLock {}
    // SAFETY: shared access exposes no mutable operation; mutation is confined
    // to acquisition before the lock is published and to final drop.
    unsafe impl Sync for SessionFileLock {}

    impl SessionFileLock {
        fn try_acquire(file: File, path: &Path) -> Result<Self, SessionJournalError> {
            let owner = Box::new(RwLock::new(file));
            let owner_ptr = NonNull::from(Box::leak(owner));
            let guard = loop {
                // SAFETY: `owner_ptr` is the leaked exclusive allocation above;
                // no other reference is created while the guard lives.
                match unsafe { owner_ptr.as_ptr().as_mut().unwrap().try_write() } {
                    Ok(guard) => break guard,
                    Err(error) if error.kind() == io::ErrorKind::Interrupted => {}
                    Err(error) if error.kind() == io::ErrorKind::WouldBlock => {
                        // SAFETY: no guard was created, so reclaim the leaked box.
                        drop(unsafe { Box::from_raw(owner_ptr.as_ptr()) });
                        return Err(SessionJournalError::InUse(path.to_path_buf()));
                    }
                    Err(source) => {
                        // SAFETY: no guard was created, so reclaim the leaked box.
                        drop(unsafe { Box::from_raw(owner_ptr.as_ptr()) });
                        return Err(SessionJournalError::Io {
                            operation: "lock",
                            path: path.to_path_buf(),
                            source,
                        });
                    }
                }
            };
            // SAFETY: the pinned owner and manual drop order above guarantee the
            // referenced lock outlives this guard.
            let guard = unsafe {
                std::mem::transmute::<RwLockWriteGuard<'_, File>, RwLockWriteGuard<'static, File>>(
                    guard,
                )
            };
            Ok(Self {
                guard: ManuallyDrop::new(guard),
                owner: owner_ptr,
            })
        }

        fn file_mut(&mut self) -> &mut File {
            &mut self.guard
        }
    }

    impl Drop for SessionFileLock {
        fn drop(&mut self) {
            // SAFETY: the guard was initialized exactly once and must release
            // the OS lock before its exclusively owned allocation is reclaimed.
            unsafe {
                ManuallyDrop::drop(&mut self.guard);
                drop(Box::from_raw(self.owner.as_ptr()));
            }
        }
    }

    /// Durable journal failure.
    #[derive(Debug)]
    pub enum SessionJournalError {
        /// A filesystem operation failed.
        Io {
            /// Operation being attempted.
            operation: &'static str,
            /// Journal path.
            path: PathBuf,
            /// Underlying error.
            source: io::Error,
        },
        /// Another process or node already holds this identity path.
        InUse(PathBuf),
        /// Neither journal slot contains a valid record.
        InvalidRecords(PathBuf),
        /// A valid journal record belongs to a different node identity.
        NodeIdMismatch {
            /// Journal path.
            path: PathBuf,
            /// Identity requested by the caller.
            expected: NodeId,
            /// Identity stored in the journal.
            found: NodeId,
        },
        /// Equal record sequences disagree on the session value.
        ConflictingRecords(PathBuf),
        /// The durable session counter reached `u32::MAX`.
        SessionExhausted(PathBuf),
        /// The journal sequence counter reached `u64::MAX`.
        SequenceExhausted(PathBuf),
        /// The journal file exceeds the fixed two-record layout.
        Oversize(PathBuf),
    }

    impl fmt::Display for SessionJournalError {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Io {
                    operation,
                    path,
                    source,
                } => write!(
                    formatter,
                    "failed to {operation} node session journal {}: {source}",
                    path.display()
                ),
                Self::InUse(path) => write!(
                    formatter,
                    "node identity path is already in use: {}",
                    path.display()
                ),
                Self::InvalidRecords(path) => write!(
                    formatter,
                    "node session journal has no valid record: {}",
                    path.display()
                ),
                Self::NodeIdMismatch {
                    path,
                    expected,
                    found,
                } => write!(
                    formatter,
                    "node session journal identity mismatch at {}: expected {expected}, found {found}",
                    path.display()
                ),
                Self::ConflictingRecords(path) => write!(
                    formatter,
                    "node session journal has conflicting equal-sequence records: {}",
                    path.display()
                ),
                Self::SessionExhausted(path) => write!(
                    formatter,
                    "node session incarnation exhausted at {}",
                    path.display()
                ),
                Self::SequenceExhausted(path) => write!(
                    formatter,
                    "node session journal sequence exhausted at {}",
                    path.display()
                ),
                Self::Oversize(path) => write!(
                    formatter,
                    "node session journal exceeds {JOURNAL_SIZE} bytes: {}",
                    path.display()
                ),
            }
        }
    }

    impl std::error::Error for SessionJournalError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            match self {
                Self::Io { source, .. } => Some(source),
                _ => None,
            }
        }
    }

    /// Return the journal path paired with an identity/key path.
    #[must_use]
    pub fn journal_path(identity_path: &Path) -> PathBuf {
        let mut path = identity_path.as_os_str().to_os_string();
        path.push(".hew-state");
        PathBuf::from(path)
    }

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    struct JournalRecord {
        node_id: NodeId,
        incarnation: u32,
        sequence: u64,
    }

    fn advance_journal(
        file: &mut File,
        path: &Path,
        expected_node_id: NodeId,
    ) -> Result<u32, SessionJournalError> {
        let file_length = file
            .metadata()
            .map_err(|source| SessionJournalError::Io {
                operation: "stat",
                path: path.to_path_buf(),
                source,
            })?
            .len();
        if file_length > JOURNAL_SIZE as u64 {
            return Err(SessionJournalError::Oversize(path.to_path_buf()));
        }
        let length = usize::try_from(file_length)
            .map_err(|_| SessionJournalError::Oversize(path.to_path_buf()))?;
        file.seek(SeekFrom::Start(0))
            .map_err(|source| SessionJournalError::Io {
                operation: "seek",
                path: path.to_path_buf(),
                source,
            })?;
        let mut bytes = vec![0_u8; length];
        file.read_exact(&mut bytes)
            .map_err(|source| SessionJournalError::Io {
                operation: "read",
                path: path.to_path_buf(),
                source,
            })?;

        let records = [
            decode_record(bytes.get(..RECORD_SIZE)),
            decode_record(bytes.get(RECORD_SIZE..JOURNAL_SIZE)),
        ];
        for record in records.iter().flatten() {
            if record.node_id != expected_node_id {
                return Err(SessionJournalError::NodeIdMismatch {
                    path: path.to_path_buf(),
                    expected: expected_node_id,
                    found: record.node_id,
                });
            }
        }

        let current = match records {
            [None, None] if length == 0 => None,
            [None, None] => return Err(SessionJournalError::InvalidRecords(path.to_path_buf())),
            [Some(record), None] => Some((0_usize, record)),
            [None, Some(record)] => Some((1_usize, record)),
            [Some(left), Some(right)] if left.sequence > right.sequence => Some((0, left)),
            [Some(left), Some(right)] if right.sequence > left.sequence => Some((1, right)),
            [Some(left), Some(right)] if left.incarnation == right.incarnation => Some((1, right)),
            [Some(_), Some(_)] => {
                return Err(SessionJournalError::ConflictingRecords(path.to_path_buf()));
            }
        };

        let (slot, incarnation, sequence) = if let Some((current_slot, record)) = current {
            let incarnation = record
                .incarnation
                .checked_add(1)
                .ok_or_else(|| SessionJournalError::SessionExhausted(path.to_path_buf()))?;
            let sequence = record
                .sequence
                .checked_add(1)
                .ok_or_else(|| SessionJournalError::SequenceExhausted(path.to_path_buf()))?;
            (1 - current_slot, incarnation, sequence)
        } else {
            (0, 1, 1)
        };
        let encoded = encode_record(JournalRecord {
            node_id: expected_node_id,
            incarnation,
            sequence,
        });
        file.seek(SeekFrom::Start((slot * RECORD_SIZE) as u64))
            .map_err(|source| SessionJournalError::Io {
                operation: "seek",
                path: path.to_path_buf(),
                source,
            })?;
        file.write_all(&encoded)
            .map_err(|source| SessionJournalError::Io {
                operation: "write",
                path: path.to_path_buf(),
                source,
            })?;
        file.sync_all().map_err(|source| SessionJournalError::Io {
            operation: "sync",
            path: path.to_path_buf(),
            source,
        })?;
        Ok(incarnation)
    }

    fn encode_record(record: JournalRecord) -> [u8; RECORD_SIZE] {
        let mut bytes = [0_u8; RECORD_SIZE];
        bytes[..8].copy_from_slice(&JOURNAL_MAGIC);
        bytes[8..24].copy_from_slice(&record.node_id.to_bytes());
        bytes[24..28].copy_from_slice(&record.incarnation.to_be_bytes());
        bytes[32..40].copy_from_slice(&record.sequence.to_be_bytes());
        let checksum = journal_checksum(&bytes[..40]);
        bytes[40..56].copy_from_slice(&checksum);
        bytes
    }

    fn decode_record(bytes: Option<&[u8]>) -> Option<JournalRecord> {
        let bytes: &[u8; RECORD_SIZE] = bytes?.try_into().ok()?;
        if bytes[..8] != JOURNAL_MAGIC
            || bytes[28..32] != [0_u8; 4]
            || bytes[56..64] != [0_u8; 8]
            || bytes[40..56] != journal_checksum(&bytes[..40])
        {
            return None;
        }
        let node_id = NodeId::from_bytes(bytes[8..24].try_into().ok()?);
        let incarnation = u32::from_be_bytes(bytes[24..28].try_into().ok()?);
        let sequence = u64::from_be_bytes(bytes[32..40].try_into().ok()?);
        if incarnation == 0 || sequence == 0 {
            return None;
        }
        Some(JournalRecord {
            node_id,
            incarnation,
            sequence,
        })
    }

    fn journal_checksum(prefix: &[u8]) -> [u8; 16] {
        let mut hasher = Sha256::new();
        hasher.update(JOURNAL_CHECKSUM_DOMAIN);
        hasher.update(prefix);
        let digest = hasher.finalize();
        digest[..16]
            .try_into()
            .expect("SHA-256 digest always contains 16 checksum bytes")
    }

    #[cfg(test)]
    mod tests {
        use std::fs;

        use tempfile::tempdir;

        use super::*;

        fn identity(byte: u8) -> NodeId {
            NodeId::from_bytes([byte; 16])
        }

        #[test]
        fn missing_journal_starts_at_one_and_advances() {
            let temp = tempdir().unwrap();
            let key_path = temp.path().join("node.key");
            let first = NodeSessionLease::acquire(&key_path, identity(1)).unwrap();
            assert_eq!(first.incarnation(), 1);
            assert_eq!(first.journal_path(), journal_path(&key_path));
            drop(first);

            let second = NodeSessionLease::acquire(&key_path, identity(1)).unwrap();
            assert_eq!(second.incarnation(), 2);
        }

        #[test]
        fn torn_newer_slot_recovers_from_other_valid_record() {
            let temp = tempdir().unwrap();
            let key_path = temp.path().join("node.key");
            drop(NodeSessionLease::acquire(&key_path, identity(2)).unwrap());

            let path = journal_path(&key_path);
            let mut file = OpenOptions::new().write(true).open(&path).unwrap();
            file.seek(SeekFrom::Start(RECORD_SIZE as u64)).unwrap();
            file.write_all(&[0xAA; 20]).unwrap();
            file.sync_all().unwrap();

            let recovered = NodeSessionLease::acquire(&key_path, identity(2)).unwrap();
            assert_eq!(recovered.incarnation(), 2);
        }

        #[test]
        fn mismatched_node_identity_is_rejected() {
            let temp = tempdir().unwrap();
            let key_path = temp.path().join("node.key");
            drop(NodeSessionLease::acquire(&key_path, identity(3)).unwrap());

            let error = NodeSessionLease::acquire(&key_path, identity(4)).unwrap_err();
            assert!(matches!(error, SessionJournalError::NodeIdMismatch { .. }));
        }

        #[test]
        fn one_identity_path_cannot_be_used_concurrently() {
            let temp = tempdir().unwrap();
            let key_path = temp.path().join("node.key");
            let first = NodeSessionLease::acquire(&key_path, identity(5)).unwrap();
            let error = NodeSessionLease::acquire(&key_path, identity(5)).unwrap_err();
            assert!(matches!(error, SessionJournalError::InUse(_)));
            drop(first);
            assert!(NodeSessionLease::acquire(&key_path, identity(5)).is_ok());
        }

        #[test]
        fn failed_start_burns_reserved_incarnation() {
            let temp = tempdir().unwrap();
            let key_path = temp.path().join("node.key");
            let failed_start = NodeSessionLease::acquire(&key_path, identity(6)).unwrap();
            assert_eq!(failed_start.incarnation(), 1);
            drop(failed_start);

            let retry = NodeSessionLease::acquire(&key_path, identity(6)).unwrap();
            assert_eq!(retry.incarnation(), 2);
        }

        #[test]
        fn exhausted_session_counter_is_rejected() {
            let temp = tempdir().unwrap();
            let key_path = temp.path().join("node.key");
            let path = journal_path(&key_path);
            fs::write(
                &path,
                encode_record(JournalRecord {
                    node_id: identity(7),
                    incarnation: u32::MAX,
                    sequence: 9,
                }),
            )
            .unwrap();

            let error = NodeSessionLease::acquire(&key_path, identity(7)).unwrap_err();
            assert!(matches!(error, SessionJournalError::SessionExhausted(_)));
        }

        #[test]
        fn two_invalid_records_are_rejected() {
            let temp = tempdir().unwrap();
            let key_path = temp.path().join("node.key");
            fs::write(journal_path(&key_path), [0xAA; JOURNAL_SIZE]).unwrap();

            let error = NodeSessionLease::acquire(&key_path, identity(8)).unwrap_err();
            assert!(matches!(error, SessionJournalError::InvalidRecords(_)));
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use session_journal::{journal_path, NodeSessionLease, SessionJournalError};

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
