//! Map failure frames to functions and source locations.
//!
//! A failure record carries each frame as a program counter, the runtime base
//! of the image that holds it and that image's build identity. [`Symbolizer`]
//! opens the image on disk (an executable, a split `.debug` file, a `.dSYM`
//! bundle or a `.pdb`), refuses symbols whose build identity differs from the
//! frame's, and returns the frame's function and source location.
//!
//! # Addresses
//!
//! `image_base` is the address the image's lowest mapped byte was loaded at:
//! `dladdr`'s `dli_fbase` on ELF and Mach-O, the `HMODULE` on Windows. The
//! symbolizer subtracts it from `pc` and adds the image's static base (the
//! lowest `PT_LOAD` address on ELF, the `__TEXT` address on Mach-O, the
//! optional header's `ImageBase` on PE) to reach the address the debug
//! information describes.
//!
//! # Build identity
//!
//! [`BuildId`] holds the bytes each format records: the `NT_GNU_BUILD_ID`
//! note on ELF, `LC_UUID` on Mach-O, and the `CodeView` GUID (as stored in
//! the PE debug directory) followed by the little-endian age on Windows.

mod dwarf;
mod pdb_file;

use std::fmt;
use std::path::{Path, PathBuf};

/// The identity of one linked image.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BuildId(Vec<u8>);

impl BuildId {
    /// Wrap the raw identity bytes of an image.
    #[must_use]
    pub fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    /// Parse the lowercase or uppercase hex spelling written in failure records.
    ///
    /// Returns `None` for an empty string, an odd length or a non-hex digit.
    #[must_use]
    pub fn from_hex(text: &str) -> Option<Self> {
        if text.is_empty() || !text.len().is_multiple_of(2) {
            return None;
        }
        let bytes = text
            .as_bytes()
            .chunks(2)
            .map(|pair| {
                let digits = std::str::from_utf8(pair).ok()?;
                u8::from_str_radix(digits, 16).ok()
            })
            .collect::<Option<Vec<u8>>>()?;
        Some(Self(bytes))
    }

    /// The raw identity bytes.
    #[must_use]
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

impl fmt::Display for BuildId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.iter().try_for_each(|byte| write!(f, "{byte:02x}"))
    }
}

impl fmt::Debug for BuildId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BuildId({self})")
    }
}

/// One frame of a failure record.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Frame {
    /// The return or fault address.
    pub pc: u64,
    /// The runtime address of the image's lowest mapped byte.
    pub image_base: u64,
    /// The identity of the image that holds `pc`, when the runtime knew it.
    pub build_id: Option<BuildId>,
    /// `pc` is a return address, so the call that made the frame is the
    /// instruction before it. A fault address names its own instruction.
    pub return_address: bool,
}

/// A function and source position for one frame.
///
/// A frame expands to several locations when the compiler inlined calls into
/// it; the innermost call comes first.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Location {
    /// The function's display name.
    pub function: Option<String>,
    /// The source file, as the compiler recorded it.
    pub file: Option<String>,
    /// The one-based source line.
    pub line: Option<u32>,
    /// The one-based source column.
    pub column: Option<u32>,
}

/// Why symbols could not be read or applied.
#[derive(Debug)]
pub enum Error {
    /// The file could not be read.
    Io {
        path: PathBuf,
        source: std::io::Error,
    },
    /// The file is not an image or symbol file this crate reads.
    Format { path: PathBuf, message: String },
    /// The symbols belong to a different build than the frame or executable.
    BuildIdMismatch {
        path: PathBuf,
        expected: BuildId,
        found: Option<BuildId>,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io { path, source } => write!(f, "cannot read {}: {source}", path.display()),
            Self::Format { path, message } => write!(f, "{}: {message}", path.display()),
            Self::BuildIdMismatch {
                path,
                expected,
                found: Some(found),
            } => write!(
                f,
                "{} is from a different build: it has build id {found}, expected {expected}",
                path.display()
            ),
            Self::BuildIdMismatch {
                path,
                expected,
                found: None,
            } => write!(
                f,
                "{} has no build id, so it cannot be matched to build {expected}",
                path.display()
            ),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io { source, .. } => Some(source),
            _ => None,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub(crate) fn format_error(path: &Path, message: impl fmt::Display) -> Error {
    Error::Format {
        path: path.to_path_buf(),
        message: message.to_string(),
    }
}

pub(crate) fn read_file(path: &Path) -> Result<Vec<u8>> {
    std::fs::read(path).map_err(|source| Error::Io {
        path: path.to_path_buf(),
        source,
    })
}

/// Symbols for one image, ready to answer frame lookups.
#[derive(Debug)]
pub struct Symbolizer {
    path: PathBuf,
    build_id: Option<BuildId>,
    source: Source,
}

#[derive(Debug)]
enum Source {
    /// An ELF, Mach-O or PE image: DWARF, then its symbol table.
    Object(dwarf::ObjectSymbols),
    /// A Windows program database, addressed by image-relative offset.
    Pdb(pdb_file::PdbSymbols),
}

impl Symbolizer {
    /// Open an executable, a split `.debug` file, a `.dSYM` bundle or a `.pdb`.
    ///
    /// An executable without its own debug information also loads the symbol
    /// file it names: the `.gnu_debuglink` file beside it (or in `.debug/`),
    /// the `.dSYM` bundle beside it, or the `.pdb` its debug directory names.
    ///
    /// # Errors
    /// Fails when a file cannot be read or parsed, or when a symbol file found
    /// beside the executable belongs to a different build.
    pub fn open(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let file = dsym_dwarf_file(path)?.unwrap_or_else(|| path.to_path_buf());
        let data = read_file(&file)?;
        let (build_id, source) = if pdb_file::is_pdb(&data) {
            let symbols = pdb_file::PdbSymbols::parse(&file, data)?;
            (Some(symbols.build_id().clone()), Source::Pdb(symbols))
        } else {
            let image = dwarf::ObjectSymbols::parse(&file, &data)?;
            let build_id = image.build_id().cloned();
            match image.sidecar() {
                Some(dwarf::Sidecar::Object(sidecar)) if !image.has_dwarf() => {
                    let symbols = Self::open(&sidecar)?;
                    check_identity(&symbols, build_id.as_ref())?;
                    let Source::Object(debug) = symbols.source else {
                        return Err(format_error(&sidecar, "is not an object file"));
                    };
                    (build_id, Source::Object(image.with_debug(debug)))
                }
                Some(dwarf::Sidecar::Pdb(sidecar)) if !image.has_dwarf() => {
                    let symbols = Self::open(&sidecar)?;
                    check_identity(&symbols, build_id.as_ref())?;
                    (build_id, symbols.source)
                }
                _ => (build_id, Source::Object(image)),
            }
        };
        Ok(Self {
            path: path.to_path_buf(),
            build_id,
            source,
        })
    }

    /// The file this symbolizer was opened from.
    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// The build identity of the opened image or symbol file.
    #[must_use]
    pub fn build_id(&self) -> Option<&BuildId> {
        self.build_id.as_ref()
    }

    /// Resolve one frame, innermost inlined call first.
    ///
    /// An empty result means the image has no symbols covering `pc`; the
    /// caller collapses such a frame.
    ///
    /// # Errors
    /// Refuses a frame whose build identity differs from these symbols, and
    /// reports malformed debug information.
    pub fn symbolize(&self, frame: &Frame) -> Result<Vec<Location>> {
        if let Some(expected) = &frame.build_id {
            if self.build_id.as_ref() != Some(expected) {
                return Err(Error::BuildIdMismatch {
                    path: self.path.clone(),
                    expected: expected.clone(),
                    found: self.build_id.clone(),
                });
            }
        }
        let pc = frame.pc - u64::from(frame.return_address && frame.pc > 0);
        let offset = pc.wrapping_sub(frame.image_base);
        match &self.source {
            Source::Object(symbols) => symbols.lookup(offset, &self.path),
            Source::Pdb(symbols) => Ok(symbols.lookup(offset)),
        }
    }
}

fn check_identity(symbols: &Symbolizer, expected: Option<&BuildId>) -> Result<()> {
    match expected {
        Some(expected) if symbols.build_id.as_ref() != Some(expected) => {
            Err(Error::BuildIdMismatch {
                path: symbols.path.clone(),
                expected: expected.clone(),
                found: symbols.build_id.clone(),
            })
        }
        _ => Ok(()),
    }
}

/// The DWARF file inside a `.dSYM` bundle, or `None` for any other path.
fn dsym_dwarf_file(path: &Path) -> Result<Option<PathBuf>> {
    if !path.is_dir() {
        return Ok(None);
    }
    let dwarf_dir = path.join("Contents/Resources/DWARF");
    let entries = std::fs::read_dir(&dwarf_dir).map_err(|source| Error::Io {
        path: dwarf_dir.clone(),
        source,
    })?;
    let mut files = entries
        .filter_map(std::result::Result::ok)
        .map(|entry| entry.path())
        .filter(|file| file.is_file());
    match (files.next(), files.next()) {
        (Some(file), None) => Ok(Some(file)),
        _ => Err(format_error(
            path,
            "a .dSYM bundle must hold exactly one file in Contents/Resources/DWARF",
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::BuildId;

    #[test]
    fn build_id_hex_round_trips() {
        let id = BuildId::from_hex("0A78a9").expect("valid hex");
        assert_eq!(id.as_bytes(), &[0x0a, 0x78, 0xa9]);
        assert_eq!(id.to_string(), "0a78a9");
    }

    #[test]
    fn build_id_rejects_malformed_hex() {
        assert!(BuildId::from_hex("").is_none());
        assert!(BuildId::from_hex("abc").is_none());
        assert!(BuildId::from_hex("zz").is_none());
    }
}
