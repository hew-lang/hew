//! Private node identities: validate the opened owner and publish complete files.

use std::fs::File;
use std::io::{self, Read, Seek, Write};
use std::path::Path;
use zeroize::Zeroizing;

#[cfg(windows)]
mod windows;

fn invalid(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message)
}

#[cfg(unix)]
fn open(path: &Path, create: bool) -> io::Result<File> {
    use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
    let file = std::fs::OpenOptions::new()
        .read(true)
        .write(create)
        .create_new(create)
        .mode(0o600)
        // Nonblocking prevents a hostile FIFO from blocking before validation.
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC | libc::O_NONBLOCK)
        .open(path)?;
    if create {
        file.set_permissions(std::fs::Permissions::from_mode(0o600))?;
    }
    validate(&file)?;
    if create {
        file.lock()?;
    } else {
        file.lock_shared()?;
    }
    Ok(file)
}

#[cfg(unix)]
fn validate(file: &File) -> io::Result<()> {
    use std::os::unix::fs::MetadataExt;
    let metadata = file.metadata()?;
    // SAFETY: geteuid has no preconditions.
    let current_user = unsafe { libc::geteuid() };
    if !metadata.is_file() || metadata.uid() != current_user || metadata.mode() & 0o077 != 0 {
        return Err(invalid(
            "identity must be a regular, current-owner-only file",
        ));
    }
    Ok(())
}

#[cfg(windows)]
use windows::{open, validate};

#[cfg(not(any(unix, windows)))]
fn open(_: &Path, _: bool) -> io::Result<File> {
    Err(io::Error::new(
        io::ErrorKind::Unsupported,
        "secure identity files are unsupported",
    ))
}

#[cfg(not(any(unix, windows)))]
fn validate(_: &File) -> io::Result<()> {
    Err(io::Error::new(
        io::ErrorKind::Unsupported,
        "secure identity files are unsupported",
    ))
}

fn read(file: File, maximum: usize) -> io::Result<Zeroizing<Vec<u8>>> {
    validate(&file)?;
    // Spare capacity beyond the read limit prevents reallocating secret bytes.
    let mut bytes = Zeroizing::new(Vec::with_capacity(maximum + 2));
    file.take(maximum as u64 + 1).read_to_end(&mut bytes)?;
    if bytes.len() > maximum {
        return Err(invalid("identity file exceeds its format bound"));
    }
    Ok(bytes)
}

pub(crate) fn load(path: &Path, maximum: usize) -> io::Result<Zeroizing<Vec<u8>>> {
    let file = open(path, false)?;
    sync_parent(path)?;
    read(file, maximum)
}

fn sync_parent(path: &Path) -> io::Result<()> {
    #[cfg(unix)]
    File::open(parent(path))?.sync_all()?;
    #[cfg(not(unix))]
    let _ = path;
    Ok(())
}

fn parent(path: &Path) -> &Path {
    path.parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or(Path::new("."))
}

#[cfg(not(windows))]
fn publish(temporary: tempfile::NamedTempFile, path: &Path) -> io::Result<File> {
    temporary
        .persist_noclobber(path)
        .map_err(|error| error.error)
}

#[cfg(windows)]
use windows::publish;

/// Return a validated existing identity, or atomically publish a complete new
/// one. A racing creator adopts only the winner read through its validated
/// handle. Errors after publication never remove the published identity.
pub(crate) fn load_or_create(
    path: &Path,
    maximum: usize,
    generate: impl FnOnce() -> io::Result<Zeroizing<Vec<u8>>>,
    check: impl Fn(&[u8]) -> io::Result<()>,
) -> io::Result<Zeroizing<Vec<u8>>> {
    match open(path, false) {
        Ok(file) => {
            sync_parent(path)?;
            let bytes = read(file, maximum)?;
            check(&bytes)?;
            return Ok(bytes);
        }
        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
        Err(error) => return Err(error),
    }
    let bytes = generate()?;
    if bytes.len() > maximum {
        return Err(invalid("generated identity exceeds its format bound"));
    }
    check(&bytes)?;
    let parent = parent(path);
    #[cfg(unix)]
    let directory = File::open(parent)?;
    let mut temporary = tempfile::Builder::new()
        .prefix(".hew-identity-")
        .make_in(parent, |path| open(path, true))?;
    temporary.write_all(&bytes)?;
    temporary.as_file().sync_all()?;
    match publish(temporary, path) {
        Ok(mut file) => {
            // Flush the name after its contents. Keep the published owner on
            // a durability failure: another process may already have adopted it.
            #[cfg(unix)]
            directory.sync_all()?;
            file.rewind()?;
            let persisted = read(file, maximum)?;
            check(&persisted)?;
            Ok(persisted)
        }
        Err(error) if error.kind() == io::ErrorKind::AlreadyExists => {
            let winner = load(path, maximum)?;
            check(&winner)?;
            Ok(winner)
        }
        Err(error) => Err(error),
    }
}
