//! Noise-protocol static-identity key management.
//!
//! Mints, persists, and loads the per-node Noise XX static keypair
//! (`Noise_XX_25519_ChaChaPoly_BLAKE2s`) that the tcp-noise transport pins for
//! its in-band `upgrade_noise` handshake. Key material is written owner-only
//! and zeroised on every exit path.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

#[cfg(windows)]
use std::ffi::c_void;
use std::ffi::{c_char, c_int};
use std::fs;
#[cfg(not(windows))]
use std::fs::OpenOptions;
use std::io::{self, Write};
#[cfg(unix)]
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
#[cfg(windows)]
use std::os::windows::io::FromRawHandle;
use std::ptr;

#[cfg(windows)]
use windows_sys::Win32::Foundation::{CloseHandle, LocalFree, INVALID_HANDLE_VALUE};
#[cfg(windows)]
use windows_sys::Win32::Security::Authorization::{
    ConvertStringSecurityDescriptorToSecurityDescriptorW, SDDL_REVISION_1,
};
#[cfg(windows)]
use windows_sys::Win32::Security::{
    DACL_SECURITY_INFORMATION, PROTECTED_DACL_SECURITY_INFORMATION, SECURITY_ATTRIBUTES,
};
#[cfg(windows)]
use windows_sys::Win32::Storage::FileSystem::{
    CreateFileW, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, FILE_GENERIC_WRITE, WRITE_DAC,
};

use snow::Builder;
use zeroize::Zeroize;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const NOISE_PATTERN: &str = "Noise_XX_25519_ChaChaPoly_BLAKE2s";
const KEY_LEN: usize = 32;
const KEYPAIR_FILE_LEN: usize = KEY_LEN * 2;
#[cfg(windows)]
const OWNER_ONLY_KEY_SDDL: &str = "D:P(A;;FA;;;OW)";

// ---------------------------------------------------------------------------
// Key-file persistence
// ---------------------------------------------------------------------------

#[cfg(windows)]
struct LocalSecurityDescriptor(*mut c_void);

#[cfg(windows)]
impl Drop for LocalSecurityDescriptor {
    fn drop(&mut self) {
        if !self.0.is_null() {
            // SAFETY: the pointer was allocated by ConvertStringSecurityDescriptorToSecurityDescriptorW.
            unsafe { LocalFree(self.0) };
        }
    }
}

#[cfg(windows)]
fn owner_only_key_security_descriptor() -> io::Result<LocalSecurityDescriptor> {
    let sddl: Vec<u16> = OWNER_ONLY_KEY_SDDL
        .encode_utf16()
        .chain(std::iter::once(0))
        .collect();
    let mut descriptor = ptr::null_mut();
    // SAFETY: sddl is null-terminated and descriptor points to writable storage for the result.
    let ok = unsafe {
        ConvertStringSecurityDescriptorToSecurityDescriptorW(
            sddl.as_ptr(),
            SDDL_REVISION_1,
            &mut descriptor,
            ptr::null_mut(),
        )
    };
    if ok == 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(LocalSecurityDescriptor(descriptor))
}

#[cfg(windows)]
fn write_noise_key_file(path_str: &str, public: &[u8], private: &[u8]) -> io::Result<()> {
    let path_wide: Vec<u16> = path_str.encode_utf16().chain(std::iter::once(0)).collect();
    let descriptor = owner_only_key_security_descriptor()?;
    #[expect(
        clippy::cast_possible_truncation,
        reason = "SECURITY_ATTRIBUTES size fits in u32 on Windows"
    )]
    let security_attributes = SECURITY_ATTRIBUTES {
        nLength: std::mem::size_of::<SECURITY_ATTRIBUTES>() as u32,
        lpSecurityDescriptor: descriptor.0,
        bInheritHandle: 0,
    };

    // SAFETY: path_wide is null-terminated; security_attributes and its descriptor
    // remain valid until after CreateFileW returns.
    let handle = unsafe {
        CreateFileW(
            path_wide.as_ptr(),
            FILE_GENERIC_WRITE | WRITE_DAC,
            0,
            &raw const security_attributes,
            CREATE_ALWAYS,
            FILE_ATTRIBUTE_NORMAL,
            ptr::null_mut(),
        )
    };
    if handle == INVALID_HANDLE_VALUE {
        return Err(io::Error::last_os_error());
    }

    // CREATE_ALWAYS truncates an existing file before bytes are written, but Windows
    // ignores lpSecurityAttributes for existing files. Replace the DACL on the open
    // handle before persisting private key material so overwrites are not left with
    // inherited/world-readable ACLs.
    // SAFETY: handle is valid and descriptor contains a valid protected DACL.
    let secured = unsafe {
        windows_sys::Win32::Security::SetKernelObjectSecurity(
            handle,
            DACL_SECURITY_INFORMATION | PROTECTED_DACL_SECURITY_INFORMATION,
            descriptor.0,
        )
    };
    if secured == 0 {
        // SAFETY: handle was returned by CreateFileW and has not been transferred to File.
        unsafe { CloseHandle(handle) };
        return Err(io::Error::last_os_error());
    }

    // SAFETY: handle is uniquely owned and will be closed when file is dropped.
    let mut file = unsafe { fs::File::from_raw_handle(handle) };
    file.write_all(public)?;
    file.write_all(private)?;
    file.flush()
}

#[cfg(not(windows))]
fn write_noise_key_file(path_str: &str, public: &[u8], private: &[u8]) -> io::Result<()> {
    let mut opts = OpenOptions::new();
    opts.create(true).truncate(true).write(true);
    #[cfg(unix)]
    opts.mode(0o600);

    let mut file = opts.open(path_str)?;
    file.write_all(public)?;
    file.write_all(private)?;

    #[cfg(unix)]
    fs::set_permissions(path_str, fs::Permissions::from_mode(0o600))?;

    Ok(())
}

/// Load (or, if absent, mint-and-persist) this node's stable Noise static
/// identity for the tcp-noise transport (issue #2652, D1). The keyfile stores
/// `32-byte public || 32-byte private`, written owner-only. Peers pin the
/// resulting public key via `Node::allow_peer`; persisting it keeps that key
/// stable across restarts, which is what makes the key bindable. An existing
/// file is loaded verbatim (never re-minted); a malformed/oversize file is
/// rejected fail-closed rather than silently replaced.
///
/// # Errors
///
/// Returns a diagnostic string if the keyfile cannot be read/written, has the
/// wrong size, or a fresh identity cannot be minted.
pub fn noise_identity_load_or_create(
    path: &std::path::Path,
) -> Result<crate::peer_binding::StableNoiseIdentity, String> {
    use crate::peer_binding::StableNoiseIdentity;
    let path_str = path
        .to_str()
        .ok_or_else(|| "load_keys: non-UTF-8 keyfile path".to_string())?;
    if path.exists() {
        let mut bytes = read_noise_keyfile_bounded(path)?;
        if bytes.len() != KEYPAIR_FILE_LEN {
            bytes.zeroize();
            return Err("load_keys: noise keyfile has unexpected size".into());
        }
        let mut public = [0u8; KEY_LEN];
        let mut private = [0u8; KEY_LEN];
        public.copy_from_slice(&bytes[..KEY_LEN]);
        private.copy_from_slice(&bytes[KEY_LEN..]);
        bytes.zeroize();
        return Ok(StableNoiseIdentity::from_raw(public, private));
    }
    let builder = Builder::new(
        NOISE_PATTERN
            .parse()
            .map_err(|e| format!("load_keys: invalid noise pattern: {e}"))?,
    );
    let mut keypair = builder
        .generate_keypair()
        .map_err(|e| format!("load_keys: mint noise identity: {e}"))?;
    if keypair.public.len() != KEY_LEN || keypair.private.len() != KEY_LEN {
        keypair.private.zeroize();
        return Err("load_keys: noise keygen produced unexpected key length".into());
    }
    let mut public = [0u8; KEY_LEN];
    let mut private = [0u8; KEY_LEN];
    public.copy_from_slice(&keypair.public);
    private.copy_from_slice(&keypair.private);
    keypair.private.zeroize();
    write_noise_key_file(path_str, &public, &private)
        .map_err(|e| format!("load_keys: write {}: {e}", path.display()))?;
    Ok(StableNoiseIdentity::from_raw(public, private))
}

/// Bounded read of a Noise keyfile: cap the read at one byte past the exact
/// keypair length so a truncated-but-huge or corrupt file cannot drive an
/// unbounded allocation before the length check rejects it.
fn read_noise_keyfile_bounded(path: &std::path::Path) -> Result<Vec<u8>, String> {
    use std::io::Read;
    let file =
        fs::File::open(path).map_err(|e| format!("load_keys: read {}: {e}", path.display()))?;
    let mut buf = Vec::new();
    file.take((KEYPAIR_FILE_LEN + 1) as u64)
        .read_to_end(&mut buf)
        .map_err(|e| format!("load_keys: read {}: {e}", path.display()))?;
    Ok(buf)
}

// ---------------------------------------------------------------------------
// Public C ABI — Noise static keypair generation and persistence
// ---------------------------------------------------------------------------

/// Generate a new X25519 static keypair for Noise XX.
///
/// # Safety
///
/// - `public_key_out` must point to at least 32 writable bytes.
/// - `private_key_out` must point to at least 32 writable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_noise_keygen(
    public_key_out: *mut u8,
    private_key_out: *mut u8,
) -> c_int {
    cabi_guard!(public_key_out.is_null() || private_key_out.is_null(), -1);

    let Ok(pattern) = NOISE_PATTERN.parse() else {
        return -1;
    };
    let builder = Builder::new(pattern);
    let Ok(mut keypair) = builder.generate_keypair() else {
        return -1;
    };

    // SAFETY: pointers are validated non-null and caller guarantees writable
    // buffers of KEY_LEN bytes.
    unsafe {
        ptr::copy_nonoverlapping(keypair.public.as_ptr(), public_key_out, KEY_LEN);
        ptr::copy_nonoverlapping(keypair.private.as_ptr(), private_key_out, KEY_LEN);
    }
    keypair.private.zeroize();
    0
}

/// Save a keypair to a raw binary file (`32-byte public || 32-byte private`).
///
/// # Safety
///
/// - `path` must be a valid null-terminated C string.
/// - `public_key` and `private_key` must each point to at least 32 readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_noise_key_save(
    path: *const c_char,
    public_key: *const u8,
    private_key: *const u8,
) -> c_int {
    cabi_guard!(
        path.is_null() || public_key.is_null() || private_key.is_null(),
        -1
    );

    // SAFETY: path was null-checked by cabi_guard above.
    let Some(path_str) = (unsafe { crate::util::cstr_to_str(&path, "hew_noise_key_save") }) else {
        return -1;
    };

    // SAFETY: pointers are validated non-null and caller guarantees KEY_LEN bytes.
    let public = unsafe { std::slice::from_raw_parts(public_key, KEY_LEN) };
    // SAFETY: pointers are validated non-null and caller guarantees KEY_LEN bytes.
    let private = unsafe { std::slice::from_raw_parts(private_key, KEY_LEN) };

    if write_noise_key_file(path_str, public, private).is_err() {
        return -1;
    }

    0
}

/// Load a Noise keypair from a raw 64-byte file.
///
/// # Safety
///
/// - `path` must be a valid null-terminated C string.
/// - `public_key_out` and `private_key_out` must each point to at least 32 writable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_noise_key_load(
    path: *const c_char,
    public_key_out: *mut u8,
    private_key_out: *mut u8,
) -> c_int {
    cabi_guard!(
        path.is_null() || public_key_out.is_null() || private_key_out.is_null(),
        -1
    );

    // SAFETY: path was null-checked by cabi_guard above.
    let Some(path_str) = (unsafe { crate::util::cstr_to_str(&path, "hew_noise_key_load") }) else {
        return -1;
    };
    let Ok(mut bytes) = fs::read(path_str) else {
        return -1;
    };
    if bytes.len() != KEYPAIR_FILE_LEN {
        bytes.zeroize();
        return -1;
    }

    // SAFETY: output pointers are validated non-null and caller guarantees
    // writable KEY_LEN-byte buffers.
    unsafe {
        ptr::copy_nonoverlapping(bytes.as_ptr(), public_key_out, KEY_LEN);
        ptr::copy_nonoverlapping(bytes.as_ptr().add(KEY_LEN), private_key_out, KEY_LEN);
    }
    bytes.zeroize();
    0
}

/// Generate a new Noise keypair.
///
/// Returns a pointer to a 64-byte heap allocation (`32-byte public || 32-byte
/// private`) via `libc::malloc`. The first [`KEY_LEN`] bytes are the public key
/// and the next [`KEY_LEN`] bytes are the private key.
///
/// # Safety
///
/// The caller must `free()` the returned pointer when done. The private key
/// portion should be zeroised before freeing if it is no longer needed.
///
/// Returns null if the pattern is invalid or keypair generation fails.
#[no_mangle]
pub unsafe extern "C" fn hew_noise_keypair_generate() -> *mut u8 {
    let Ok(params) = NOISE_PATTERN.parse() else {
        return ptr::null_mut();
    };
    let builder = Builder::new(params);
    let Ok(mut keypair) = builder.generate_keypair() else {
        return ptr::null_mut();
    };

    // Allocate space for both public and private keys.
    // SAFETY: malloc with a valid size.
    let buf = unsafe { libc::malloc(KEYPAIR_FILE_LEN) }.cast::<u8>(); // ALLOCATOR-PAIRING: libc
    if buf.is_null() {
        keypair.private.zeroize();
        return ptr::null_mut();
    }
    // SAFETY: buf is freshly allocated with KEYPAIR_FILE_LEN bytes.
    unsafe {
        ptr::copy_nonoverlapping(keypair.public.as_ptr(), buf, KEY_LEN);
        ptr::copy_nonoverlapping(keypair.private.as_ptr(), buf.add(KEY_LEN), KEY_LEN);
    }
    keypair.private.zeroize();
    buf
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::c_void;
    use std::ffi::CString;
    use tempfile::tempdir;
    #[cfg(windows)]
    use windows_sys::Win32::Foundation::{LocalFree, ERROR_SUCCESS};
    #[cfg(windows)]
    use windows_sys::Win32::Security::Authorization::{
        ConvertSecurityDescriptorToStringSecurityDescriptorW, GetNamedSecurityInfoW,
        SDDL_REVISION_1, SE_FILE_OBJECT,
    };
    #[cfg(windows)]
    use windows_sys::Win32::Security::DACL_SECURITY_INFORMATION;

    #[cfg(windows)]
    struct LocalAllocString(*mut u16);

    #[cfg(windows)]
    impl Drop for LocalAllocString {
        fn drop(&mut self) {
            if !self.0.is_null() {
                // SAFETY: the pointer was allocated by a Windows security API.
                unsafe { LocalFree(self.0.cast()) };
            }
        }
    }

    #[cfg(windows)]
    struct LocalAllocDescriptor(*mut c_void);

    #[cfg(windows)]
    impl Drop for LocalAllocDescriptor {
        fn drop(&mut self) {
            if !self.0.is_null() {
                // SAFETY: the pointer was allocated by GetNamedSecurityInfoW.
                unsafe { LocalFree(self.0) };
            }
        }
    }

    #[cfg(windows)]
    fn key_file_dacl_sddl(path: &std::path::Path) -> String {
        let mut path_wide: Vec<u16> = path
            .as_os_str()
            .to_string_lossy()
            .encode_utf16()
            .chain(std::iter::once(0))
            .collect();
        let mut descriptor = ptr::null_mut();
        // SAFETY: path_wide is null-terminated and descriptor points to writable storage.
        let rc = unsafe {
            GetNamedSecurityInfoW(
                path_wide.as_mut_ptr(),
                SE_FILE_OBJECT,
                DACL_SECURITY_INFORMATION,
                ptr::null_mut(),
                ptr::null_mut(),
                ptr::null_mut(),
                ptr::null_mut(),
                &mut descriptor,
            )
        };
        assert_eq!(rc, ERROR_SUCCESS, "GetNamedSecurityInfoW failed: {rc}");
        let _descriptor = LocalAllocDescriptor(descriptor);

        let mut sddl_ptr = ptr::null_mut();
        let mut sddl_len = 0;
        // SAFETY: descriptor is valid and output pointers are writable.
        let ok = unsafe {
            ConvertSecurityDescriptorToStringSecurityDescriptorW(
                descriptor,
                SDDL_REVISION_1,
                DACL_SECURITY_INFORMATION,
                &mut sddl_ptr,
                &mut sddl_len,
            )
        };
        assert_ne!(
            ok, 0,
            "ConvertSecurityDescriptorToStringSecurityDescriptorW failed"
        );
        let _sddl = LocalAllocString(sddl_ptr);
        // SAFETY: sddl_ptr is valid for sddl_len UTF-16 code units on success.
        String::from_utf16_lossy(unsafe { std::slice::from_raw_parts(sddl_ptr, sddl_len as usize) })
    }

    #[cfg(windows)]
    fn assert_owner_only_key_acl(path: &std::path::Path) {
        let sddl = key_file_dacl_sddl(path);
        assert!(
            sddl.starts_with("D:P"),
            "key file DACL must be protected from inherited broad ACEs: {sddl}"
        );
        assert!(
            sddl.contains("OW") || sddl.contains("S-1-3-4"),
            "key file DACL must grant owner rights only: {sddl}"
        );
        assert_eq!(
            sddl.matches('(').count(),
            1,
            "key file DACL must contain exactly one ACE: {sddl}"
        );
        for broad_sid in ["WD", "AU", "BU", "BG", "AN"] {
            assert!(
                !sddl.contains(broad_sid),
                "key file DACL must not grant broad SID {broad_sid}: {sddl}"
            );
        }
    }

    // -----------------------------------------------------------------------
    // Key generation
    // -----------------------------------------------------------------------

    #[test]
    fn noise_keygen_fills_public_and_private_keys() {
        let mut public = [0u8; KEY_LEN];
        let mut private = [0u8; KEY_LEN];

        // SAFETY: output buffers are valid and writable.
        let rc = unsafe { hew_noise_keygen(public.as_mut_ptr(), private.as_mut_ptr()) };
        assert_eq!(rc, 0);
        assert!(
            !public.iter().all(|&b| b == 0),
            "public key must not be all zeros"
        );
        assert!(
            !private.iter().all(|&b| b == 0),
            "private key must not be all zeros"
        );
    }

    #[test]
    fn noise_key_save_load_round_trip() {
        let mut public = [0u8; KEY_LEN];
        let mut private = [0u8; KEY_LEN];

        // SAFETY: output buffers are valid and writable.
        let rc = unsafe { hew_noise_keygen(public.as_mut_ptr(), private.as_mut_ptr()) };
        assert_eq!(rc, 0);

        let dir = tempdir().unwrap();
        let key_file = dir.path().join("noise.key");
        let path = CString::new(key_file.to_string_lossy().into_owned()).unwrap();

        // SAFETY: path and key pointers are valid.
        let save_rc =
            unsafe { hew_noise_key_save(path.as_ptr(), public.as_ptr(), private.as_ptr()) };
        assert_eq!(save_rc, 0);

        let on_disk = fs::read(&key_file).unwrap();
        assert_eq!(on_disk.len(), KEYPAIR_FILE_LEN);

        #[cfg(unix)]
        {
            let mode = fs::metadata(&key_file).unwrap().permissions().mode() & 0o777;
            assert_eq!(mode, 0o600);
        }
        #[cfg(windows)]
        assert_owner_only_key_acl(&key_file);

        let mut loaded_public = [0u8; KEY_LEN];
        let mut loaded_private = [0u8; KEY_LEN];
        // SAFETY: path and output buffers are valid.
        let load_rc = unsafe {
            hew_noise_key_load(
                path.as_ptr(),
                loaded_public.as_mut_ptr(),
                loaded_private.as_mut_ptr(),
            )
        };
        assert_eq!(load_rc, 0);
        assert_eq!(loaded_public, public);
        assert_eq!(loaded_private, private);
    }

    /// Issue #2652 D1: `noise_identity_load_or_create` mints a stable Noise
    /// identity on first call, persists it owner-only, and reloads the *same*
    /// public/private key on a second call — the property that makes the key
    /// bindable via `Node::allow_peer` across restarts. A corrupt/wrong-size
    /// file is rejected fail-closed rather than silently re-minted.
    #[test]
    fn noise_identity_load_or_create_persists_stable_identity() {
        let dir = tempdir().unwrap();
        let key_file = dir.path().join("noise_id.key");

        let first = noise_identity_load_or_create(&key_file).expect("mint stable noise identity");
        assert!(key_file.exists(), "first call must persist the keyfile");
        assert_eq!(
            fs::read(&key_file).unwrap().len(),
            KEYPAIR_FILE_LEN,
            "keyfile is public || private"
        );
        #[cfg(unix)]
        {
            let mode = fs::metadata(&key_file).unwrap().permissions().mode() & 0o777;
            assert_eq!(mode, 0o600, "private key material must be owner-only");
        }

        let second = noise_identity_load_or_create(&key_file).expect("reload stable identity");
        assert_eq!(
            first.public(),
            second.public(),
            "reload must yield the same stable public key (bindable across restarts)"
        );
        assert_eq!(
            first.private(),
            second.private(),
            "reload must yield the same private key"
        );

        // A truncated / wrong-size file is rejected, never silently re-minted.
        fs::write(&key_file, [0xAB; KEYPAIR_FILE_LEN - 1]).unwrap();
        let err =
            noise_identity_load_or_create(&key_file).expect_err("wrong-size keyfile rejected");
        assert!(err.contains("unexpected size"), "diagnostic: {err}");
    }

    #[cfg(windows)]
    #[test]
    fn noise_key_save_replaces_existing_windows_dacl_before_writing_private_key() {
        let mut public = [0u8; KEY_LEN];
        let mut private = [0u8; KEY_LEN];

        // SAFETY: output buffers are valid and writable.
        let rc = unsafe { hew_noise_keygen(public.as_mut_ptr(), private.as_mut_ptr()) };
        assert_eq!(rc, 0);

        let dir = tempdir().unwrap();
        let path_buf = dir.path().join("noise.key");
        fs::write(&path_buf, [0xAA; KEYPAIR_FILE_LEN]).unwrap();
        let path = CString::new(path_buf.to_string_lossy().into_owned()).unwrap();

        // SAFETY: path and key pointers are valid.
        let save_rc =
            unsafe { hew_noise_key_save(path.as_ptr(), public.as_ptr(), private.as_ptr()) };
        assert_eq!(save_rc, 0);

        assert_owner_only_key_acl(&path_buf);
        let on_disk = fs::read(&path_buf).unwrap();
        assert_eq!(&on_disk[..KEY_LEN], public);
        assert_eq!(&on_disk[KEY_LEN..], private);
    }

    #[test]
    fn keypair_generate_returns_both_keys() {
        // SAFETY: no preconditions.
        let buf = unsafe { hew_noise_keypair_generate() };
        assert!(!buf.is_null());

        // SAFETY: buf is valid for KEYPAIR_FILE_LEN bytes (public || private).
        let public = unsafe { std::slice::from_raw_parts(buf, KEY_LEN) };
        // SAFETY: buf + KEY_LEN is within the KEYPAIR_FILE_LEN allocation.
        let private = unsafe { std::slice::from_raw_parts(buf.add(KEY_LEN), KEY_LEN) };
        assert!(
            !public.iter().all(|&b| b == 0),
            "public key must not be all zeros"
        );
        assert!(
            !private.iter().all(|&b| b == 0),
            "private key must not be all zeros"
        );
        assert_ne!(public, private, "public and private keys must differ");

        // SAFETY: buf was allocated by libc::malloc.
        unsafe { libc::free(buf.cast::<c_void>()) }; // ALLOCATOR-PAIRING: libc
    }

    #[test]
    fn keypair_generate_private_key_derives_matching_public() {
        // SAFETY: no preconditions.
        let buf = unsafe { hew_noise_keypair_generate() };
        assert!(!buf.is_null());

        // SAFETY: buf is valid for KEYPAIR_FILE_LEN bytes.
        let returned_public = unsafe { std::slice::from_raw_parts(buf, KEY_LEN) }.to_vec();
        // SAFETY: buf + KEY_LEN is within the KEYPAIR_FILE_LEN allocation.
        let private = unsafe { std::slice::from_raw_parts(buf.add(KEY_LEN), KEY_LEN) };

        // Perform a Noise XX handshake using the generated private key as the
        // initiator's static key. After the exchange the responder sees the
        // initiator's public key via get_remote_static(). If it matches the
        // public key we got from hew_noise_keypair_generate, the pair is valid.
        let params: snow::params::NoiseParams = NOISE_PATTERN.parse().unwrap();

        let mut initiator = snow::Builder::new(params.clone())
            .local_private_key(private)
            .unwrap()
            .build_initiator()
            .unwrap();

        let responder_kp = snow::Builder::new(params.clone())
            .generate_keypair()
            .unwrap();
        let mut responder = snow::Builder::new(params)
            .local_private_key(&responder_kp.private)
            .unwrap()
            .build_responder()
            .unwrap();

        // XX three-message handshake: → e, ← e ee s es, → s se
        let mut msg = vec![0u8; 65535];
        let mut payload = vec![0u8; 65535];

        let len = initiator.write_message(&[], &mut msg).unwrap();
        responder.read_message(&msg[..len], &mut payload).unwrap();

        let len = responder.write_message(&[], &mut msg).unwrap();
        initiator.read_message(&msg[..len], &mut payload).unwrap();

        let len = initiator.write_message(&[], &mut msg).unwrap();
        responder.read_message(&msg[..len], &mut payload).unwrap();

        assert!(responder.is_handshake_finished());
        assert_eq!(
            responder.get_remote_static().unwrap(),
            &returned_public,
            "responder must see the public key returned by hew_noise_keypair_generate"
        );

        // SAFETY: buf was allocated by libc::malloc.
        unsafe { libc::free(buf.cast::<c_void>()) }; // ALLOCATOR-PAIRING: libc
    }

    #[test]
    fn keypair_generate_produces_unique_pairs() {
        // SAFETY: no preconditions.
        let buf1 = unsafe { hew_noise_keypair_generate() };
        // SAFETY: no preconditions.
        let buf2 = unsafe { hew_noise_keypair_generate() };
        assert!(!buf1.is_null());
        assert!(!buf2.is_null());

        // SAFETY: buf1 is valid for KEYPAIR_FILE_LEN bytes.
        let pub1 = unsafe { std::slice::from_raw_parts(buf1, KEY_LEN) };
        // SAFETY: buf2 is valid for KEYPAIR_FILE_LEN bytes.
        let pub2 = unsafe { std::slice::from_raw_parts(buf2, KEY_LEN) };
        assert_ne!(pub1, pub2, "two generated public keys should differ");

        // SAFETY: buf1 + KEY_LEN is within the KEYPAIR_FILE_LEN allocation.
        let priv1 = unsafe { std::slice::from_raw_parts(buf1.add(KEY_LEN), KEY_LEN) };
        // SAFETY: buf2 + KEY_LEN is within the KEYPAIR_FILE_LEN allocation.
        let priv2 = unsafe { std::slice::from_raw_parts(buf2.add(KEY_LEN), KEY_LEN) };
        assert_ne!(priv1, priv2, "two generated private keys should differ");

        // SAFETY: buffers were allocated by libc::malloc.
        unsafe {
            libc::free(buf1.cast::<c_void>()); // ALLOCATOR-PAIRING: libc
            libc::free(buf2.cast::<c_void>()); // ALLOCATOR-PAIRING: libc
        }
    }
}
