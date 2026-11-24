//! Key-derived node identity and carried actor-location values.

use std::fmt;
use std::num::NonZeroU64;
use std::str::FromStr;

use hew_cabi::string::{string_from_str, HewString};
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
    /// Non-zero exact actor serial or canonical role token, selected by `target_kind`.
    pub target_id: u64,
    /// Durable node-session incarnation.
    pub incarnation: u32,
    /// Target discriminator: zero for an exact actor, one for a stable role.
    pub target_kind: u32,
}

// C-ABI accessors for the `NodeId` and `Location` builtins.

/// Return the node identity carried by a location.
///
/// Fails closed to the zero node identity when `location` is null.
///
/// # Safety
///
/// `location` must point to a readable `HewLocation`.
#[no_mangle]
pub unsafe extern "C" fn hew_location_node_id(location: *const HewLocation) -> HewNodeId {
    if location.is_null() {
        return HewNodeId::default();
    }
    // SAFETY: caller guarantees `location` is readable.
    let location = unsafe { *location };
    location.node
}

/// Return the node-session incarnation carried by a location.
///
/// Fails closed to `0` when `location` is null.
///
/// # Safety
///
/// `location` must point to a readable `HewLocation`.
#[no_mangle]
pub unsafe extern "C" fn hew_location_incarnation(location: *const HewLocation) -> u32 {
    if location.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `location` is readable.
    let location = unsafe { *location };
    location.incarnation
}

/// Format a location as `<node-hex>/<target-kind>:<identity>@<incarnation>`, returned as an
/// owned managed string. Reproduces
/// [`crate::hew_node::hew_location_format`]'s text exactly.
///
/// Returns a null pointer when `location` is null - null is the managed
/// carrier's canonical empty string, matching the existing formatters'
/// fail-closed behaviour on a null input.
///
/// # Safety
///
/// `location` must point to a readable `HewLocation`.
#[no_mangle]
pub unsafe extern "C" fn hew_location_display(location: *const HewLocation) -> *mut HewString {
    if location.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `location` is readable.
    let location = unsafe { *location };
    Location::try_from(location).map_or(std::ptr::null_mut(), |location| {
        string_from_str(&location.to_string())
    })
}

/// Format a node identity as 32 lowercase hexadecimal digits, returned as an
/// owned managed string. Reproduces [`crate::hew_node::hew_node_id_format`]'s
/// text exactly.
///
/// Returns a null pointer when `node` is null - null is the managed
/// carrier's canonical empty string, matching the existing formatters'
/// fail-closed behaviour on a null input.
///
/// # Safety
///
/// `node` must point to a readable `HewNodeId`.
#[no_mangle]
pub unsafe extern "C" fn hew_node_id_display(node: *const HewNodeId) -> *mut HewString {
    if node.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `node` is readable.
    let node = unsafe { *node };
    string_from_str(&format!("{:016x}{:016x}", node.hi, node.lo))
}

/// Semantic node-local destination. The variants have disjoint identity even
/// when their scalar representations coincide. Neither variant grants access.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum Target {
    Direct(NonZeroU64),
    Role(NonZeroU64),
}

impl Target {
    pub(crate) const fn id(self) -> u64 {
        match self {
            Self::Direct(id) | Self::Role(id) => id.get(),
        }
    }

    pub(crate) const fn kind(self) -> u32 {
        match self {
            Self::Direct(_) => 0,
            Self::Role(_) => 1,
        }
    }
}

/// Immutable carried actor location.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct Location(HewLocation);

impl Location {
    /// Construct a location for one exact actor allocation serial.
    ///
    /// # Errors
    /// Returns an error when the serial or node-session incarnation is zero.
    pub fn direct(node: NodeId, serial: u64, incarnation: u32) -> Result<Self, InvalidLocation> {
        let id = NonZeroU64::new(serial).ok_or(InvalidLocation::ZeroTarget)?;
        Self::new(node, Target::Direct(id), incarnation)
    }

    /// Construct a location carrying a canonical stable role token.
    ///
    /// # Errors
    /// Returns an error when the token or node-session incarnation is zero.
    pub fn role(node: NodeId, token: u64, incarnation: u32) -> Result<Self, InvalidLocation> {
        let id = NonZeroU64::new(token).ok_or(InvalidLocation::ZeroTarget)?;
        Self::new(node, Target::Role(id), incarnation)
    }

    fn new(node: NodeId, target: Target, incarnation: u32) -> Result<Self, InvalidLocation> {
        if incarnation == 0 {
            return Err(InvalidLocation::ZeroIncarnation);
        }
        Ok(Self(HewLocation {
            node: node.into(),
            target_id: target.id(),
            incarnation,
            target_kind: target.kind(),
        }))
    }

    /// Stable node identity.
    #[must_use]
    pub const fn node(self) -> NodeId {
        NodeId(self.0.node)
    }

    pub(crate) fn target(self) -> Target {
        let id = NonZeroU64::new(self.0.target_id).expect("validated carried target");
        match self.0.target_kind {
            0 => Target::Direct(id),
            1 => Target::Role(id),
            _ => unreachable!("validated carried target discriminator"),
        }
    }

    /// Exact actor serial, when this location identifies a direct actor.
    #[must_use]
    pub(crate) fn direct_serial(self) -> Option<u64> {
        match self.target() {
            Target::Direct(id) => Some(id.get()),
            Target::Role(_) => None,
        }
    }

    /// Durable node-session incarnation.
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
        let id = NonZeroU64::new(value.target_id).ok_or(InvalidLocation::ZeroTarget)?;
        let target = match value.target_kind {
            0 => Target::Direct(id),
            1 => Target::Role(id),
            _ => return Err(InvalidLocation::UnknownTargetKind),
        };
        Self::new(value.node.into(), target, value.incarnation)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match self.target() {
            Target::Direct(_) => "actor",
            Target::Role(_) => "role",
        };
        write!(
            formatter,
            "{}/{kind}:{}@{}",
            self.node(),
            self.target().id(),
            self.incarnation()
        )
    }
}

/// Invalid carried-location representation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InvalidLocation {
    /// Target identity zero is reserved.
    ZeroTarget,
    /// Node-session incarnation zero is invalid.
    ZeroIncarnation,
    /// The target discriminator is not part of the carried contract.
    UnknownTargetKind,
}

impl fmt::Display for InvalidLocation {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ZeroTarget => formatter.write_str("location target identity must be nonzero"),
            Self::ZeroIncarnation => {
                formatter.write_str("location session incarnation must be nonzero")
            }
            Self::UnknownTargetKind => formatter.write_str("unknown location target kind"),
        }
    }
}

impl std::error::Error for InvalidLocation {}

#[cfg(not(target_arch = "wasm32"))]
mod session_journal {
    use std::fmt;
    #[cfg(any(not(windows), test))]
    use std::fs::OpenOptions;
    use std::fs::{File, TryLockError};
    use std::io::{self, Read, Seek, SeekFrom, Write};
    use std::path::{Path, PathBuf};

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
            let file = open_journal(&journal_path).map_err(|source| SessionJournalError::Io {
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
        file: File,
    }

    impl SessionFileLock {
        fn try_acquire(file: File, path: &Path) -> Result<Self, SessionJournalError> {
            loop {
                match file.try_lock() {
                    Ok(()) => return Ok(Self { file }),
                    Err(TryLockError::Error(error))
                        if error.kind() == io::ErrorKind::Interrupted => {}
                    Err(TryLockError::WouldBlock) => {
                        return Err(SessionJournalError::InUse(path.to_path_buf()));
                    }
                    Err(TryLockError::Error(source)) => {
                        return Err(SessionJournalError::Io {
                            operation: "lock",
                            path: path.to_path_buf(),
                            source,
                        });
                    }
                }
            }
        }

        fn file_mut(&mut self) -> &mut File {
            &mut self.file
        }
    }

    impl Drop for SessionFileLock {
        fn drop(&mut self) {
            let _ = self.file.unlock();
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

    #[cfg(unix)]
    fn open_journal(path: &Path) -> io::Result<File> {
        use std::os::unix::fs::{MetadataExt, OpenOptionsExt, PermissionsExt};

        fn open_one(path: &Path, create: bool) -> io::Result<File> {
            let file = OpenOptions::new()
                .read(true)
                .write(true)
                .create_new(create)
                .mode(0o600)
                // O_NOFOLLOW rejects symlinks; O_NONBLOCK prevents a hostile FIFO
                // from blocking before metadata validation.
                .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC | libc::O_NONBLOCK)
                .open(path)?;
            if create {
                file.set_permissions(std::fs::Permissions::from_mode(0o600))?;
            }
            let metadata = file.metadata()?;
            // SAFETY: geteuid has no preconditions.
            let current_user = unsafe { libc::geteuid() };
            if !metadata.is_file() || metadata.uid() != current_user {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "node session journal must be a regular file owned by the current user",
                ));
            }
            Ok(file)
        }

        match open_one(path, false) {
            Ok(file) => Ok(file),
            Err(error) if error.kind() == io::ErrorKind::NotFound => match open_one(path, true) {
                Ok(file) => Ok(file),
                Err(error) if error.kind() == io::ErrorKind::AlreadyExists => open_one(path, false),
                Err(error) => Err(error),
            },
            Err(error) => Err(error),
        }
    }

    #[cfg(windows)]
    fn open_journal(path: &Path) -> io::Result<File> {
        use std::os::windows::ffi::OsStrExt;
        use std::os::windows::fs::MetadataExt;
        use std::os::windows::io::{AsRawHandle, FromRawHandle};
        use std::ptr;
        use windows_sys::Win32::Foundation::INVALID_HANDLE_VALUE;
        use windows_sys::Win32::Storage::FileSystem::{
            CreateFileW, GetFileType, CREATE_NEW, FILE_ATTRIBUTE_NORMAL,
            FILE_ATTRIBUTE_REPARSE_POINT, FILE_FLAG_BACKUP_SEMANTICS, FILE_FLAG_OPEN_REPARSE_POINT,
            FILE_GENERIC_READ, FILE_GENERIC_WRITE, FILE_SHARE_DELETE, FILE_SHARE_READ,
            FILE_SHARE_WRITE, FILE_TYPE_DISK, OPEN_EXISTING,
        };

        fn open_one(path: &Path, create: bool) -> io::Result<File> {
            let mut path_wide: Vec<u16> = path.as_os_str().encode_wide().collect();
            if path_wide.contains(&0) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "node session journal path contains NUL",
                ));
            }
            path_wide.push(0);
            // SAFETY: path_wide is NUL-terminated for this CreateFileW call.
            let handle = unsafe {
                CreateFileW(
                    path_wide.as_ptr(),
                    FILE_GENERIC_READ | FILE_GENERIC_WRITE,
                    FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                    ptr::null(),
                    if create { CREATE_NEW } else { OPEN_EXISTING },
                    FILE_ATTRIBUTE_NORMAL
                        | FILE_FLAG_OPEN_REPARSE_POINT
                        | FILE_FLAG_BACKUP_SEMANTICS,
                    ptr::null_mut(),
                )
            };
            if handle == INVALID_HANDLE_VALUE {
                return Err(io::Error::last_os_error());
            }
            // SAFETY: this newly opened handle is uniquely owned by `File`.
            let file = unsafe { File::from_raw_handle(handle) };
            // SAFETY: `file` owns this live handle.
            if unsafe { GetFileType(file.as_raw_handle()) } != FILE_TYPE_DISK {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "node session journal must be a disk file",
                ));
            }
            let metadata = file.metadata()?;
            if !metadata.is_file() || metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
            {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "node session journal must be a regular file, never a reparse point",
                ));
            }
            Ok(file)
        }

        match open_one(path, false) {
            Ok(file) => Ok(file),
            Err(error) if error.kind() == io::ErrorKind::NotFound => match open_one(path, true) {
                Ok(file) => Ok(file),
                Err(error) if error.kind() == io::ErrorKind::AlreadyExists => open_one(path, false),
                Err(error) => Err(error),
            },
            Err(error) => Err(error),
        }
    }

    #[cfg(not(any(unix, windows)))]
    fn open_journal(path: &Path) -> io::Result<File> {
        let file = OpenOptions::new()
            .create(true)
            .read(true)
            .write(true)
            .truncate(false)
            .open(path)?;
        if !file.metadata()?.is_file() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "node session journal must be a regular file",
            ));
        }
        Ok(file)
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
        if file_length == 0 {
            sync_journal_parent(path)?;
        }
        Ok(incarnation)
    }

    #[cfg(unix)]
    fn sync_journal_parent(path: &Path) -> Result<(), SessionJournalError> {
        let parent = path
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .unwrap_or_else(|| Path::new("."));
        File::open(parent)
            .and_then(|dir| dir.sync_all())
            .map_err(|source| SessionJournalError::Io {
                operation: "sync parent",
                path: parent.to_path_buf(),
                source,
            })
    }

    #[cfg(not(unix))]
    #[allow(
        clippy::unnecessary_wraps,
        reason = "Unix syncs the parent directory; non-Unix shares the fallible signature"
    )]
    fn sync_journal_parent(_path: &Path) -> Result<(), SessionJournalError> {
        Ok(())
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

        #[test]
        fn journal_rejects_directories_and_unsafe_paths() {
            let temp = tempdir().unwrap();
            let dir_key = temp.path().join("dir.key");
            fs::create_dir(journal_path(&dir_key)).unwrap();
            assert!(
                NodeSessionLease::acquire(&dir_key, identity(9)).is_err(),
                "directory at journal path must be rejected"
            );

            #[cfg(unix)]
            {
                use std::ffi::CString;
                use std::os::unix::fs::PermissionsExt;

                let key_path = temp.path().join("owner_only.key");
                let lease = NodeSessionLease::acquire(&key_path, identity(10)).unwrap();
                let mode = fs::metadata(lease.journal_path())
                    .unwrap()
                    .permissions()
                    .mode()
                    & 0o777;
                assert_eq!(mode, 0o600, "newly created journal must be owner-only");
                drop(lease);

                // Live symlink to an existing file must not be followed or modified.
                let victim = temp.path().join("victim.txt");
                fs::write(&victim, b"do not clobber").unwrap();
                let link_key = temp.path().join("symlink.key");
                std::os::unix::fs::symlink(&victim, journal_path(&link_key)).unwrap();
                assert!(
                    NodeSessionLease::acquire(&link_key, identity(11)).is_err(),
                    "symlink at journal path must be rejected"
                );
                assert_eq!(fs::read(&victim).unwrap(), b"do not clobber");

                // Dangling symlink must not create its target.
                let missing_target = temp.path().join("never_created.txt");
                let dangling_key = temp.path().join("dangling.key");
                std::os::unix::fs::symlink(&missing_target, journal_path(&dangling_key)).unwrap();
                assert!(
                    NodeSessionLease::acquire(&dangling_key, identity(12)).is_err(),
                    "dangling symlink at journal path must be rejected"
                );
                assert!(
                    !missing_target.exists(),
                    "dangling symlink rejection must not create target file"
                );

                // Named pipe (FIFO) must be rejected without blocking.
                let fifo_key = temp.path().join("fifo.key");
                let fifo_path = journal_path(&fifo_key);
                let c_fifo = CString::new(fifo_path.as_os_str().as_encoded_bytes()).unwrap();
                // SAFETY: NUL-terminated path and valid mode bits.
                assert_eq!(unsafe { libc::mkfifo(c_fifo.as_ptr(), 0o600) }, 0);
                assert!(
                    NodeSessionLease::acquire(&fifo_key, identity(13)).is_err(),
                    "FIFO at journal path must be rejected"
                );
            }
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
        assert_eq!(
            Location::direct(node, 0, 1),
            Err(InvalidLocation::ZeroTarget)
        );
        assert_eq!(
            Location::direct(node, 1, 0),
            Err(InvalidLocation::ZeroIncarnation)
        );
        let location = Location::direct(node, u64::MAX, u32::MAX).unwrap();
        assert_eq!(
            location.to_string(),
            "11111111111111111111111111111111/actor:18446744073709551615@4294967295"
        );
        let mut raw: HewLocation = location.into();
        raw.target_kind = 2;
        assert_eq!(
            Location::try_from(raw),
            Err(InvalidLocation::UnknownTargetKind)
        );
    }

    #[test]
    fn direct_and_role_targets_preserve_disjoint_full_width_identity() {
        let node = NodeId::from_bytes([0x22; 16]);
        let direct = Location::direct(node, u64::MAX, 9).unwrap();
        let role = Location::role(node, u64::MAX, 9).unwrap();
        assert_ne!(direct, role);
        assert_eq!(role.direct_serial(), None);
        assert_eq!(direct.direct_serial(), Some(u64::MAX));
        assert_eq!(Location::try_from(HewLocation::from(role)), Ok(role));
        assert_eq!(
            role.to_string(),
            "22222222222222222222222222222222/role:18446744073709551615@9"
        );
        assert_eq!(Location::role(node, 0, 9), Err(InvalidLocation::ZeroTarget));
    }

    fn sample_location() -> HewLocation {
        HewLocation {
            node: HewNodeId {
                hi: 0x0123_4567_89ab_cdef,
                lo: 0xfedc_ba98_7654_3210,
            },
            target_id: 7,
            incarnation: 3,
            target_kind: 0,
        }
    }

    fn owned_string_text(value: *mut HewString) -> String {
        assert!(!value.is_null());
        // SAFETY: the accessor under test returned a live owned managed
        // string; `string_as_str` borrows it without transferring ownership.
        let text = unsafe { hew_cabi::string::string_as_str(value) }.to_owned();
        // SAFETY: this test holds the sole reference; drop the allocation.
        unsafe { hew_cabi::string::string_release(value) };
        text
    }

    #[test]
    fn location_field_accessors_read_the_exact_fields() {
        let location = sample_location();
        // SAFETY: `location` is a live, readable `HewLocation` for this call.
        let (node, incarnation) = unsafe {
            (
                hew_location_node_id(&raw const location),
                hew_location_incarnation(&raw const location),
            )
        };
        assert_eq!(node, location.node);
        assert_eq!(incarnation, location.incarnation);
    }

    #[test]
    fn location_field_accessors_fail_closed_on_null() {
        // SAFETY: every accessor documents a null pointer as a fail-closed input.
        let (node, incarnation, display) = unsafe {
            (
                hew_location_node_id(std::ptr::null()),
                hew_location_incarnation(std::ptr::null()),
                hew_location_display(std::ptr::null()),
            )
        };
        assert_eq!(node, HewNodeId::default());
        assert_eq!(incarnation, 0);
        assert!(display.is_null());
    }

    #[test]
    fn display_accessors_match_the_existing_formatters_exactly() {
        let location = sample_location();
        let expected_node = "0123456789abcdeffedcba9876543210".to_string();
        let expected_location = "0123456789abcdeffedcba9876543210/actor:7@3".to_string();

        // SAFETY: both carriers are live and readable; each display returns a
        // fresh owned string that `owned_string_text` releases.
        let (node_text, location_text) = unsafe {
            (
                hew_node_id_display(&raw const location.node),
                hew_location_display(&raw const location),
            )
        };
        assert_eq!(owned_string_text(node_text), expected_node);
        assert_eq!(owned_string_text(location_text), expected_location);

        // Cross-check against the pre-existing `hew_node.rs` formatter this
        // display text is required to reproduce exactly.
        // SAFETY: the node id is live; `hew_node_id_format` returns an owned C
        // string this block reads once and frees.
        let via_hew_node_format = unsafe {
            let raw = crate::hew_node::hew_node_id_format(&raw const location.node);
            assert!(!raw.is_null());
            let text = std::ffi::CStr::from_ptr(raw).to_str().unwrap().to_owned();
            crate::cabi::free_cstring(raw);
            text
        };
        assert_eq!(via_hew_node_format, expected_node);
    }

    #[test]
    fn node_id_display_fails_closed_on_null() {
        // SAFETY: a null node id is a documented fail-closed input.
        assert!(unsafe { hew_node_id_display(std::ptr::null()) }.is_null());
    }

    #[test]
    fn c_layouts_are_fixed_width() {
        assert_eq!(std::mem::size_of::<HewNodeId>(), 16);
        assert_eq!(std::mem::align_of::<HewNodeId>(), 8);
        assert_eq!(std::mem::size_of::<NodeId>(), 16);
        assert_eq!(std::mem::size_of::<HewLocation>(), 32);
        assert_eq!(std::mem::align_of::<HewLocation>(), 8);
        assert_eq!(std::mem::size_of::<Location>(), 32);
        assert_eq!(std::mem::offset_of!(HewLocation, node), 0);
        assert_eq!(std::mem::offset_of!(HewNodeId, hi), 0);
        assert_eq!(std::mem::offset_of!(HewNodeId, lo), 8);
        assert_eq!(std::mem::offset_of!(HewLocation, target_id), 16);
        assert_eq!(std::mem::offset_of!(HewLocation, incarnation), 24);
        assert_eq!(std::mem::offset_of!(HewLocation, target_kind), 28);
    }
}
