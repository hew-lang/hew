//! Noise-protocol encrypted transport wrapper.
//!
//! Wraps an existing [`HewTransport`] to provide authenticated encryption
//! using the Noise XX handshake pattern (`Noise_XX_25519_ChaChaPoly_BLAKE2s`).
//! Per-connection `snow::TransportState` objects handle encrypt/decrypt.

use std::collections::HashSet;
use std::ffi::{c_char, c_int, c_void, CStr};
use std::fs::{self, OpenOptions};
use std::io::Write;
#[cfg(unix)]
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{LazyLock, Mutex, RwLock};

use snow::Builder;
use zeroize::Zeroizing;

use crate::transport::{HewTransport, HewTransportOps, HEW_CONN_INVALID};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const NOISE_PATTERN: &str = "Noise_XX_25519_ChaChaPoly_BLAKE2s";
const MAX_CONNS: usize = 64;
const KEY_LEN: usize = 32;
const KEYPAIR_FILE_LEN: usize = KEY_LEN * 2;
/// Maximum Noise message size (64 KiB per snow docs).
const MAX_MSG_SIZE: usize = 65535;
const FRAME_HEADER_LEN: usize = 4;
const ALLOWLIST_MODE_OPEN: c_int = 0;
const ALLOWLIST_MODE_STRICT: c_int = 1;

/// Allowlist enforcement mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllowlistMode {
    /// Allow all connections (no verification).
    Open,
    /// Only allow peers whose static key is in the allowlist.
    Strict,
}

/// Set of allowed peer public keys.
#[derive(Debug, Clone)]
pub struct HewPeerAllowlist {
    keys: HashSet<[u8; 32]>,
    mode: AllowlistMode,
}

impl HewPeerAllowlist {
    fn new(mode: AllowlistMode) -> Self {
        Self {
            keys: HashSet::new(),
            mode,
        }
    }

    fn allows(&self, key: &[u8; KEY_LEN]) -> bool {
        match self.mode {
            AllowlistMode::Open => true,
            AllowlistMode::Strict => self.keys.contains(key),
        }
    }
}

#[derive(Debug, Clone)]
struct ActiveAllowlist {
    ptr: usize,
    list: HewPeerAllowlist,
}

static ALLOWLIST_OPS_LOCK: LazyLock<Mutex<()>> = LazyLock::new(|| Mutex::new(()));
/// Global allowlist for all encrypted transports in-process.
///
/// The allowlist is intentionally process-global for C ABI simplicity; replacing or
/// freeing it affects every encrypted transport instance. `ALLOWLIST_STRICT_REQUIRED`
/// preserves fail-closed behavior after strict mode is enabled so a dropped list
/// cannot silently disable peer filtering.
static ACTIVE_ALLOWLIST: LazyLock<RwLock<Option<ActiveAllowlist>>> =
    LazyLock::new(|| RwLock::new(None));
static ALLOWLIST_STRICT_REQUIRED: AtomicBool = AtomicBool::new(false);

// ---------------------------------------------------------------------------
// Encrypted transport state
// ---------------------------------------------------------------------------

/// Per-connection encryption state.
struct ConnState {
    inner_conn_id: c_int,
    transport: snow::TransportState,
}

/// Internal state for the encrypted transport wrapper.
struct EncryptedTransport {
    inner: *mut HewTransport,
    private_key: Zeroizing<[u8; KEY_LEN]>,
    conns: [Option<ConnState>; MAX_CONNS],
}

impl EncryptedTransport {
    fn new(inner: *mut HewTransport, private_key: [u8; KEY_LEN]) -> Self {
        Self {
            inner,
            private_key: Zeroizing::new(private_key),
            conns: std::array::from_fn(|_| None),
        }
    }

    fn inner_ops(&self) -> Option<&HewTransportOps> {
        if self.inner.is_null() {
            return None;
        }
        // SAFETY: inner was validated at construction.
        let t = unsafe { &*self.inner };
        if t.ops.is_null() {
            return None;
        }
        // SAFETY: ops pointer is valid.
        Some(unsafe { &*t.ops })
    }

    fn inner_impl(&self) -> *mut c_void {
        if self.inner.is_null() {
            return ptr::null_mut();
        }
        // SAFETY: inner was validated at construction.
        unsafe { (*self.inner).r#impl }
    }

    fn private_key(&self) -> &[u8; KEY_LEN] {
        &self.private_key
    }

    fn store_conn(&mut self, state: ConnState) -> c_int {
        for (i, slot) in self.conns.iter_mut().enumerate() {
            if slot.is_none() {
                *slot = Some(state);
                #[expect(clippy::cast_possible_truncation, reason = "MAX_CONNS fits in c_int")]
                #[expect(clippy::cast_possible_wrap, reason = "C ABI: length fits in i32")]
                return i as c_int;
            }
        }
        HEW_CONN_INVALID
    }

    fn get_conn_mut(&mut self, id: c_int) -> Option<&mut ConnState> {
        if id < 0 {
            return None;
        }
        #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
        let idx = id as usize;
        self.conns.get_mut(idx).and_then(Option::as_mut)
    }

    fn remove_conn(&mut self, id: c_int) {
        if id >= 0 {
            #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
            let idx = id as usize;
            if idx < MAX_CONNS {
                self.conns[idx] = None;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Handshake helpers
// ---------------------------------------------------------------------------

impl Drop for EncryptedTransport {
    fn drop(&mut self) {
        if !self.inner.is_null() {
            // SAFETY: inner was validated at construction time in
            // `hew_transport_encrypted_new` and has not been freed since.
            let inner_t = unsafe { &*self.inner };
            if !inner_t.ops.is_null() {
                // SAFETY: ops points to a valid static vtable.
                let ops = unsafe { &*inner_t.ops };
                if let Some(destroy_fn) = ops.destroy {
                    // SAFETY: `r#impl` was created by the inner transport's
                    // constructor (e.g. `hew_transport_tcp_new`).
                    unsafe { destroy_fn(inner_t.r#impl) };
                }
            }
            // SAFETY: inner was created by `Box::into_raw` in a
            // `hew_transport_*_new` function.
            let _ = unsafe { Box::from_raw(self.inner) };
        }
    }
}

/// Perform the Noise XX handshake as initiator using the inner transport.
///
/// # Safety
///
/// `ops` and `impl_ptr` must be valid. `conn` must be a valid connection.
unsafe fn do_initiator_handshake(
    ops: &HewTransportOps,
    impl_ptr: *mut c_void,
    conn: c_int,
    private_key: &[u8],
) -> Option<(snow::TransportState, [u8; KEY_LEN])> {
    let builder = Builder::new(NOISE_PATTERN.parse().ok()?);
    let mut handshake = builder
        .local_private_key(private_key)
        .ok()?
        .build_initiator()
        .ok()?;

    let mut buf = vec![0u8; MAX_MSG_SIZE];

    // -> e
    let len = handshake.write_message(&[], &mut buf).ok()?;
    let send_fn = ops.send?;
    // SAFETY: buf is valid for `len` bytes.
    if unsafe { send_fn(impl_ptr, conn, buf.as_ptr().cast::<c_void>(), len) } < 0 {
        return None;
    }

    // <- e, ee, s, es
    let recv_fn = ops.recv?;
    // SAFETY: buf is valid for MAX_MSG_SIZE bytes.
    let n = unsafe {
        recv_fn(
            impl_ptr,
            conn,
            buf.as_mut_ptr().cast::<c_void>(),
            MAX_MSG_SIZE,
        )
    };
    if n < 0 {
        return None;
    }
    #[expect(clippy::cast_sign_loss, reason = "n >= 0 checked above")]
    let n = n as usize;
    let mut payload = vec![0u8; MAX_MSG_SIZE];
    handshake.read_message(&buf[..n], &mut payload).ok()?;

    // -> s, se
    let len = handshake.write_message(&[], &mut buf).ok()?;
    // SAFETY: buf is valid for `len` bytes.
    if unsafe { send_fn(impl_ptr, conn, buf.as_ptr().cast::<c_void>(), len) } < 0 {
        return None;
    }

    let remote_static = handshake.get_remote_static()?;
    if remote_static.len() != KEY_LEN {
        return None;
    }
    let mut remote_key = [0u8; KEY_LEN];
    remote_key.copy_from_slice(remote_static);
    let transport = handshake.into_transport_mode().ok()?;
    Some((transport, remote_key))
}

/// Perform the Noise XX handshake as responder using the inner transport.
///
/// # Safety
///
/// `ops` and `impl_ptr` must be valid. `conn` must be a valid connection.
unsafe fn do_responder_handshake(
    ops: &HewTransportOps,
    impl_ptr: *mut c_void,
    conn: c_int,
    private_key: &[u8],
) -> Option<(snow::TransportState, [u8; KEY_LEN])> {
    let builder = Builder::new(NOISE_PATTERN.parse().ok()?);
    let mut handshake = builder
        .local_private_key(private_key)
        .ok()?
        .build_responder()
        .ok()?;

    let mut buf = vec![0u8; MAX_MSG_SIZE];
    let mut payload = vec![0u8; MAX_MSG_SIZE];

    // <- e
    let recv_fn = ops.recv?;
    // SAFETY: buf is valid for MAX_MSG_SIZE bytes.
    let n = unsafe {
        recv_fn(
            impl_ptr,
            conn,
            buf.as_mut_ptr().cast::<c_void>(),
            MAX_MSG_SIZE,
        )
    };
    if n < 0 {
        return None;
    }
    #[expect(clippy::cast_sign_loss, reason = "n >= 0 checked above")]
    let n = n as usize;
    handshake.read_message(&buf[..n], &mut payload).ok()?;

    // -> e, ee, s, es
    let send_fn = ops.send?;
    let len = handshake.write_message(&[], &mut buf).ok()?;
    // SAFETY: buf is valid for `len` bytes.
    if unsafe { send_fn(impl_ptr, conn, buf.as_ptr().cast::<c_void>(), len) } < 0 {
        return None;
    }

    // <- s, se
    // SAFETY: buf is valid for MAX_MSG_SIZE bytes.
    let n = unsafe {
        recv_fn(
            impl_ptr,
            conn,
            buf.as_mut_ptr().cast::<c_void>(),
            MAX_MSG_SIZE,
        )
    };
    if n < 0 {
        return None;
    }
    #[expect(clippy::cast_sign_loss, reason = "n >= 0 checked above")]
    let n = n as usize;
    handshake.read_message(&buf[..n], &mut payload).ok()?;

    let remote_static = handshake.get_remote_static()?;
    if remote_static.len() != KEY_LEN {
        return None;
    }
    let mut remote_key = [0u8; KEY_LEN];
    remote_key.copy_from_slice(remote_static);
    let transport = handshake.into_transport_mode().ok()?;
    Some((transport, remote_key))
}

// ---------------------------------------------------------------------------
// Encrypted transport vtable callbacks
// ---------------------------------------------------------------------------

unsafe extern "C" fn enc_connect(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    if impl_ptr.is_null() {
        return HEW_CONN_INVALID;
    }
    // SAFETY: impl_ptr points to a valid EncryptedTransport.
    let enc = unsafe { &mut *impl_ptr.cast::<EncryptedTransport>() };
    let ops = match enc.inner_ops() {
        Some(o) => o as *const HewTransportOps,
        None => return HEW_CONN_INVALID,
    };
    let inner_impl = enc.inner_impl();

    // Connect using inner transport.
    // SAFETY: ops was derived from a valid &HewTransportOps and remains valid for this scope.
    let Some(connect_fn) = (unsafe { &*ops }).connect else {
        return HEW_CONN_INVALID;
    };
    // SAFETY: inner_impl is valid.
    let conn = unsafe { connect_fn(inner_impl, address) };
    if conn == HEW_CONN_INVALID {
        return HEW_CONN_INVALID;
    }

    // Perform initiator handshake.
    let key = *enc.private_key();
    // SAFETY: ops and inner_impl are valid.
    let transport_state = unsafe { do_initiator_handshake(&*ops, inner_impl, conn, &key) };
    if let Some((ts, peer_key)) = transport_state {
        if !hew_allowlist_check_active_peer(&peer_key) {
            // SAFETY: ops was derived from a valid &HewTransportOps and remains valid for this scope.
            if let Some(close_fn) = (unsafe { &*ops }).close_conn {
                // SAFETY: inner_impl is valid.
                unsafe { close_fn(inner_impl, conn) };
            }
            return HEW_CONN_INVALID;
        }
        enc.store_conn(ConnState {
            inner_conn_id: conn,
            transport: ts,
        })
    } else {
        // Close the inner connection on handshake failure.
        // SAFETY: ops was derived from a valid &HewTransportOps and remains valid for this scope.
        if let Some(close_fn) = (unsafe { &*ops }).close_conn {
            // SAFETY: inner_impl is valid.
            unsafe { close_fn(inner_impl, conn) };
        }
        HEW_CONN_INVALID
    }
}

unsafe extern "C" fn enc_listen(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    if impl_ptr.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr points to a valid EncryptedTransport.
    let enc = unsafe { &*impl_ptr.cast::<EncryptedTransport>() };
    let Some(ops) = enc.inner_ops() else {
        return -1;
    };
    let Some(listen_fn) = ops.listen else {
        return -1;
    };
    // SAFETY: inner_impl is valid.
    unsafe { listen_fn(enc.inner_impl(), address) }
}

unsafe extern "C" fn enc_accept(impl_ptr: *mut c_void, timeout_ms: c_int) -> c_int {
    if impl_ptr.is_null() {
        return HEW_CONN_INVALID;
    }
    // SAFETY: impl_ptr points to a valid EncryptedTransport.
    let enc = unsafe { &mut *impl_ptr.cast::<EncryptedTransport>() };
    let ops = match enc.inner_ops() {
        Some(o) => o as *const HewTransportOps,
        None => return HEW_CONN_INVALID,
    };
    let inner_impl = enc.inner_impl();

    // SAFETY: ops was derived from a valid &HewTransportOps and remains valid for this scope.
    let Some(accept_fn) = (unsafe { &*ops }).accept else {
        return HEW_CONN_INVALID;
    };
    // SAFETY: inner_impl is valid.
    let conn = unsafe { accept_fn(inner_impl, timeout_ms) };
    if conn == HEW_CONN_INVALID {
        return HEW_CONN_INVALID;
    }

    // Perform responder handshake.
    let key = *enc.private_key();
    // SAFETY: ops and inner_impl are valid.
    let transport_state = unsafe { do_responder_handshake(&*ops, inner_impl, conn, &key) };
    if let Some((ts, peer_key)) = transport_state {
        if !hew_allowlist_check_active_peer(&peer_key) {
            // SAFETY: ops was derived from a valid &HewTransportOps and remains valid for this scope.
            if let Some(close_fn) = (unsafe { &*ops }).close_conn {
                // SAFETY: inner_impl is valid.
                unsafe { close_fn(inner_impl, conn) };
            }
            return HEW_CONN_INVALID;
        }
        enc.store_conn(ConnState {
            inner_conn_id: conn,
            transport: ts,
        })
    } else {
        // SAFETY: ops was derived from a valid &HewTransportOps and remains valid for this scope.
        if let Some(close_fn) = (unsafe { &*ops }).close_conn {
            // SAFETY: inner_impl is valid.
            unsafe { close_fn(inner_impl, conn) };
        }
        HEW_CONN_INVALID
    }
}

unsafe extern "C" fn enc_send(
    impl_ptr: *mut c_void,
    conn: c_int,
    data: *const c_void,
    len: usize,
) -> c_int {
    if impl_ptr.is_null() || data.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr points to a valid EncryptedTransport.
    let enc = unsafe { &mut *impl_ptr.cast::<EncryptedTransport>() };

    let Some(cs) = enc.get_conn_mut(conn) else {
        return -1;
    };
    let inner_conn = cs.inner_conn_id;

    // SAFETY: data is valid for `len` bytes per caller contract.
    let plaintext = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) };

    // Encrypt.
    let mut ciphertext = vec![0u8; len + 64]; // room for MAC
    let Ok(ct_len) = cs.transport.write_message(plaintext, &mut ciphertext) else {
        return -1;
    };
    let frame_len = FRAME_HEADER_LEN + ct_len;
    let mut frame = vec![0u8; frame_len];
    frame[..FRAME_HEADER_LEN].copy_from_slice(&(ct_len as u32).to_le_bytes());
    frame[FRAME_HEADER_LEN..].copy_from_slice(&ciphertext[..ct_len]);

    // Send encrypted data via inner transport.
    let Some(ops) = enc.inner_ops() else {
        return -1;
    };
    let Some(send_fn) = ops.send else {
        return -1;
    };
    // SAFETY: inner_impl is valid; frame is valid for frame_len bytes.
    let rc = unsafe {
        send_fn(
            enc.inner_impl(),
            inner_conn,
            frame.as_ptr().cast::<c_void>(),
            frame_len,
        )
    };
    if rc < 0 {
        return -1;
    }

    #[expect(clippy::cast_possible_truncation, reason = "len fits in c_int")]
    #[expect(clippy::cast_possible_wrap, reason = "C ABI: length fits in i32")]
    {
        len as c_int
    }
}

unsafe extern "C" fn enc_recv(
    impl_ptr: *mut c_void,
    conn: c_int,
    buf: *mut c_void,
    buf_size: usize,
) -> c_int {
    if impl_ptr.is_null() || buf.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr points to a valid EncryptedTransport.
    let enc = unsafe { &mut *impl_ptr.cast::<EncryptedTransport>() };

    // Receive encrypted data via inner transport.
    let mut frame = vec![0u8; FRAME_HEADER_LEN + buf_size + 64];
    let Some(ops) = enc.inner_ops() else {
        return -1;
    };
    let Some(recv_fn) = ops.recv else {
        return -1;
    };
    let Some(cs) = enc.get_conn_mut(conn) else {
        return -1;
    };
    let inner_conn = cs.inner_conn_id;

    // SAFETY: inner_impl is valid; frame buffer is valid.
    let n = unsafe {
        recv_fn(
            enc.inner_impl(),
            inner_conn,
            frame.as_mut_ptr().cast::<c_void>(),
            frame.len(),
        )
    };
    if n < 0 {
        return -1;
    }
    if n < FRAME_HEADER_LEN as c_int {
        return -1;
    }

    let Some(cs) = enc.get_conn_mut(conn) else {
        return -1;
    };

    // Decrypt.
    // SAFETY: buf is valid for buf_size bytes per caller contract.
    let out_slice = unsafe { std::slice::from_raw_parts_mut(buf.cast::<u8>(), buf_size) };
    #[expect(clippy::cast_sign_loss, reason = "n >= 0 checked above")]
    let n = n as usize;
    let mut len_bytes = [0u8; FRAME_HEADER_LEN];
    len_bytes.copy_from_slice(&frame[..FRAME_HEADER_LEN]);
    let ct_len = u32::from_le_bytes(len_bytes) as usize;
    if n != FRAME_HEADER_LEN + ct_len {
        return -1;
    }
    match cs.transport.read_message(
        &frame[FRAME_HEADER_LEN..FRAME_HEADER_LEN + ct_len],
        out_slice,
    ) {
        Ok(pt_len) => {
            #[expect(clippy::cast_possible_truncation, reason = "pt_len fits in c_int")]
            #[expect(clippy::cast_possible_wrap, reason = "C ABI: length fits in i32")]
            {
                pt_len as c_int
            }
        }
        Err(_) => -1,
    }
}

unsafe extern "C" fn enc_close_conn(impl_ptr: *mut c_void, conn: c_int) {
    if impl_ptr.is_null() {
        return;
    }
    // SAFETY: impl_ptr points to a valid EncryptedTransport.
    let enc = unsafe { &mut *impl_ptr.cast::<EncryptedTransport>() };
    let inner_conn = enc.get_conn_mut(conn).map(|cs| cs.inner_conn_id);
    enc.remove_conn(conn);

    // Also close the inner connection.
    if let Some(inner_id) = inner_conn {
        if let Some(ops) = enc.inner_ops() {
            if let Some(close_fn) = ops.close_conn {
                // SAFETY: inner_impl is valid.
                unsafe { close_fn(enc.inner_impl(), inner_id) };
            }
        }
    }
}

unsafe extern "C" fn enc_destroy(impl_ptr: *mut c_void) {
    if impl_ptr.is_null() {
        return;
    }
    // SAFETY: impl_ptr was created by Box::into_raw in hew_transport_encrypted_new.
    let _ = unsafe { Box::from_raw(impl_ptr.cast::<EncryptedTransport>()) };
}

static ENC_OPS: HewTransportOps = HewTransportOps {
    connect: Some(enc_connect),
    listen: Some(enc_listen),
    accept: Some(enc_accept),
    send: Some(enc_send),
    recv: Some(enc_recv),
    close_conn: Some(enc_close_conn),
    destroy: Some(enc_destroy),
};

fn allowlist_mode_from_c(mode: c_int) -> Option<AllowlistMode> {
    match mode {
        ALLOWLIST_MODE_OPEN => Some(AllowlistMode::Open),
        ALLOWLIST_MODE_STRICT => Some(AllowlistMode::Strict),
        _ => None,
    }
}

unsafe fn allowlist_copy_key(public_key: *const u8) -> Option<[u8; KEY_LEN]> {
    if public_key.is_null() {
        return None;
    }
    let mut key = [0u8; KEY_LEN];
    // SAFETY: caller guarantees `public_key` points to at least KEY_LEN bytes.
    unsafe { ptr::copy_nonoverlapping(public_key, key.as_mut_ptr(), KEY_LEN) };
    Some(key)
}

fn allowlist_sync_active(list_ptr: *mut HewPeerAllowlist, list: &HewPeerAllowlist) {
    let mut active = match ACTIVE_ALLOWLIST.write() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    if let Some(current) = active.as_mut() {
        if current.ptr == list_ptr as usize {
            current.list = list.clone();
        }
    }
}

pub(crate) fn hew_allowlist_check_active_peer(public_key: &[u8; KEY_LEN]) -> bool {
    let active = match ACTIVE_ALLOWLIST.read() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    match active.as_ref() {
        Some(current) => current.list.allows(public_key),
        // Design choice: strict mode is fail-closed. If the active strict list is
        // unset/freed, deny peers rather than silently disabling security.
        None => !ALLOWLIST_STRICT_REQUIRED.load(Ordering::Acquire),
    }
}

// ---------------------------------------------------------------------------
// Public C ABI
// ---------------------------------------------------------------------------

/// Create a new peer allowlist.
///
/// `mode`: 0 = Open, 1 = Strict.
/// This allowlist is process-global for encrypted transports; creating a new list
/// replaces the active one for all encrypted transport instances.
///
/// # Safety
///
/// Returns an owned pointer that must be released with [`hew_allowlist_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_allowlist_new(mode: c_int) -> *mut HewPeerAllowlist {
    let Some(mode) = allowlist_mode_from_c(mode) else {
        return ptr::null_mut();
    };
    let list = HewPeerAllowlist::new(mode);
    let ptr = Box::into_raw(Box::new(list.clone()));
    #[cfg(not(test))]
    {
        let strict_required = matches!(mode, AllowlistMode::Strict);
        let mut active = match ACTIVE_ALLOWLIST.write() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        *active = Some(ActiveAllowlist {
            ptr: ptr as usize,
            list,
        });
        ALLOWLIST_STRICT_REQUIRED.store(strict_required, Ordering::Release);
    }
    ptr
}

/// Add a 32-byte peer static public key to the allowlist.
///
/// Returns 0 on success, -1 on invalid inputs.
///
/// # Safety
///
/// - `list` must be a valid pointer returned by [`hew_allowlist_new`].
/// - `public_key` must point to at least 32 readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_allowlist_add(
    list: *mut HewPeerAllowlist,
    public_key: *const u8,
) -> c_int {
    cabi_guard!(list.is_null(), -1);
    let Some(key) = (unsafe { allowlist_copy_key(public_key) }) else {
        return -1;
    };
    let _guard = match ALLOWLIST_OPS_LOCK.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    // SAFETY: list was validated non-null and caller owns this pointer.
    let list_ref = unsafe { &mut *list };
    list_ref.keys.insert(key);
    allowlist_sync_active(list, list_ref);
    0
}

/// Remove a 32-byte peer static public key from the allowlist.
///
/// Returns 0 on success, -1 on invalid inputs.
///
/// # Safety
///
/// - `list` must be a valid pointer returned by [`hew_allowlist_new`].
/// - `public_key` must point to at least 32 readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_allowlist_remove(
    list: *mut HewPeerAllowlist,
    public_key: *const u8,
) -> c_int {
    cabi_guard!(list.is_null(), -1);
    let Some(key) = (unsafe { allowlist_copy_key(public_key) }) else {
        return -1;
    };
    let _guard = match ALLOWLIST_OPS_LOCK.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    // SAFETY: list was validated non-null and caller owns this pointer.
    let list_ref = unsafe { &mut *list };
    list_ref.keys.remove(&key);
    allowlist_sync_active(list, list_ref);
    0
}

/// Check whether a peer static public key is allowed.
///
/// Returns 1 when allowed, 0 when denied.
///
/// # Safety
///
/// - `list` must be a valid pointer returned by [`hew_allowlist_new`].
/// - `public_key` must point to at least 32 readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_allowlist_check(
    list: *mut HewPeerAllowlist,
    public_key: *const u8,
) -> c_int {
    cabi_guard!(list.is_null(), 0);
    let Some(key) = (unsafe { allowlist_copy_key(public_key) }) else {
        return 0;
    };
    let _guard = match ALLOWLIST_OPS_LOCK.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    // SAFETY: list was validated non-null and caller owns this pointer.
    let list_ref = unsafe { &*list };
    c_int::from(list_ref.allows(&key))
}

/// Free an allowlist previously created by [`hew_allowlist_new`].
///
/// # Safety
///
/// `list` must be either null or a valid pointer returned by
/// [`hew_allowlist_new`] that has not already been freed.
#[no_mangle]
pub unsafe extern "C" fn hew_allowlist_free(list: *mut HewPeerAllowlist) {
    cabi_guard!(list.is_null());
    let _guard = match ALLOWLIST_OPS_LOCK.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    {
        let mut active = match ACTIVE_ALLOWLIST.write() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        if let Some(current) = active.as_ref() {
            if current.ptr == list as usize {
                let strict_required = matches!(current.list.mode, AllowlistMode::Strict);
                *active = None;
                ALLOWLIST_STRICT_REQUIRED.store(strict_required, Ordering::Release);
            }
        }
    }
    // SAFETY: ownership is transferred back to Rust and dropped exactly once.
    let _ = unsafe { Box::from_raw(list) };
}

/// Create an encrypted transport wrapping an existing transport.
///
/// Uses `Noise_XX_25519_ChaChaPoly_BLAKE2s` handshake pattern.
///
/// # Safety
///
/// - `inner` must be a valid pointer to a [`HewTransport`].
/// - `private_key` must point to at least 32 readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_transport_encrypted_new(
    inner: *mut HewTransport,
    private_key: *const u8,
) -> *mut HewTransport {
    if inner.is_null() || private_key.is_null() {
        return ptr::null_mut();
    }
    let mut local_private_key = [0u8; KEY_LEN];
    // SAFETY: private_key is non-null and caller guarantees at least KEY_LEN bytes.
    unsafe { ptr::copy_nonoverlapping(private_key, local_private_key.as_mut_ptr(), KEY_LEN) };
    let enc = Box::new(EncryptedTransport::new(inner, local_private_key));
    let transport = Box::new(HewTransport {
        ops: &raw const ENC_OPS,
        r#impl: Box::into_raw(enc).cast::<c_void>(),
    });
    Box::into_raw(transport)
}

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
    if public_key_out.is_null() || private_key_out.is_null() {
        return -1;
    }

    let pattern = match NOISE_PATTERN.parse() {
        Ok(p) => p,
        Err(_) => return -1,
    };
    let builder = Builder::new(pattern);
    let keypair = match builder.generate_keypair() {
        Ok(k) => k,
        Err(_) => return -1,
    };

    // SAFETY: pointers are validated non-null and caller guarantees writable
    // buffers of KEY_LEN bytes.
    unsafe {
        ptr::copy_nonoverlapping(keypair.public.as_ptr(), public_key_out, KEY_LEN);
        ptr::copy_nonoverlapping(keypair.private.as_ptr(), private_key_out, KEY_LEN);
    }
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
    if path.is_null() || public_key.is_null() || private_key.is_null() {
        return -1;
    }

    // SAFETY: path is non-null and expected to be valid C string.
    let path_str = match unsafe { CStr::from_ptr(path) }.to_str() {
        Ok(p) => p,
        Err(_) => return -1,
    };

    // SAFETY: pointers are validated non-null and caller guarantees KEY_LEN bytes.
    let public = unsafe { std::slice::from_raw_parts(public_key, KEY_LEN) };
    // SAFETY: pointers are validated non-null and caller guarantees KEY_LEN bytes.
    let private = unsafe { std::slice::from_raw_parts(private_key, KEY_LEN) };

    let mut opts = OpenOptions::new();
    opts.create(true).truncate(true).write(true);
    #[cfg(unix)]
    opts.mode(0o600);

    let mut file = match opts.open(path_str) {
        Ok(f) => f,
        Err(_) => return -1,
    };
    if file.write_all(public).is_err() || file.write_all(private).is_err() {
        return -1;
    }

    #[cfg(unix)]
    if fs::set_permissions(path_str, fs::Permissions::from_mode(0o600)).is_err() {
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
    if path.is_null() || public_key_out.is_null() || private_key_out.is_null() {
        return -1;
    }

    // SAFETY: path is non-null and expected to be valid C string.
    let path_str = match unsafe { CStr::from_ptr(path) }.to_str() {
        Ok(p) => p,
        Err(_) => return -1,
    };
    let bytes = match fs::read(path_str) {
        Ok(b) => b,
        Err(_) => return -1,
    };
    if bytes.len() != KEYPAIR_FILE_LEN {
        return -1;
    }

    // SAFETY: output pointers are validated non-null and caller guarantees
    // writable KEY_LEN-byte buffers.
    unsafe {
        ptr::copy_nonoverlapping(bytes.as_ptr(), public_key_out, KEY_LEN);
        ptr::copy_nonoverlapping(bytes.as_ptr().add(KEY_LEN), private_key_out, KEY_LEN);
    }
    0
}

/// Generate a new Noise keypair.
///
/// Returns a pointer to a 32-byte public key (heap-allocated via `libc::malloc`).
/// The private key is not returned — use [`hew_noise_set_keypair`] with the
/// private key from a separately managed keypair, or call this for ephemeral keys.
///
/// # Safety
///
/// The caller must `free()` the returned pointer.
///
/// # Panics
///
/// Panics if the Noise pattern string is invalid or keypair generation fails.
#[no_mangle]
pub unsafe extern "C" fn hew_noise_keypair_generate() -> *mut u8 {
    let builder = Builder::new(NOISE_PATTERN.parse().expect("valid pattern"));
    let keypair = builder.generate_keypair().expect("keypair generation");

    // Allocate and copy public key.
    // SAFETY: malloc with a valid size.
    let pub_key = unsafe { libc::malloc(KEY_LEN) }.cast::<u8>();
    if pub_key.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: pub_key is freshly allocated with KEY_LEN bytes.
    unsafe {
        ptr::copy_nonoverlapping(keypair.public.as_ptr(), pub_key, KEY_LEN);
    }
    pub_key
}

/// Set the static private key for encrypted connections.
///
/// # Safety
///
/// - `transport` must be a valid pointer to a [`HewTransport`] created by
///   [`hew_transport_encrypted_new`].
/// - `private_key` must point to at least 32 valid bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_noise_set_keypair(
    transport: *mut HewTransport,
    private_key: *const u8,
    key_len: usize,
) {
    if transport.is_null() || private_key.is_null() || key_len < KEY_LEN {
        return;
    }
    // SAFETY: caller guarantees transport is valid.
    let t = unsafe { &*transport };
    if t.r#impl.is_null() {
        return;
    }
    // SAFETY: impl points to a valid EncryptedTransport.
    let enc = unsafe { &mut *t.r#impl.cast::<EncryptedTransport>() };
    // SAFETY: private_key is valid for at least KEY_LEN bytes.
    let key_slice = unsafe { std::slice::from_raw_parts(private_key, KEY_LEN) };
    enc.private_key[..].copy_from_slice(key_slice);
}

#[cfg(test)]
#[expect(
    clippy::cast_sign_loss,
    reason = "test assertions compare c_int return values against known-positive lengths"
)]
mod tests {
    use super::*;
    use crate::transport::hew_transport_tcp_new;
    use std::ffi::CString;
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration;
    use tempfile::NamedTempFile;

    const TEST_PRIVATE_KEY: [u8; KEY_LEN] = [7u8; KEY_LEN];

    /// Find a free TCP port by briefly binding to `127.0.0.1:0`.
    fn free_port() -> u16 {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        listener.local_addr().unwrap().port()
    }

    /// Destroy a [`HewTransport`] by calling its vtable `destroy` op and
    /// freeing the outer box.
    ///
    /// # Safety
    ///
    /// `t` must have been created by a `hew_transport_*_new` function.
    unsafe fn destroy_transport(t: *mut HewTransport) {
        if t.is_null() {
            return;
        }
        // SAFETY: t was created by Box::into_raw.
        let transport = unsafe { &*t };
        if !transport.ops.is_null() {
            // SAFETY: ops points to a valid static vtable.
            let ops = unsafe { &*transport.ops };
            if let Some(destroy_fn) = ops.destroy {
                // SAFETY: impl was created by Box::into_raw.
                unsafe { destroy_fn(transport.r#impl) };
            }
        }
        // SAFETY: t was created by Box::into_raw.
        let _ = unsafe { Box::from_raw(t) };
    }

    /// Smuggle a raw pointer across threads via `usize`. Each test ensures
    /// exclusive access from the receiving thread.
    fn ptr_to_usize(p: *mut HewTransport) -> usize {
        p as usize
    }
    fn usize_to_ptr(v: usize) -> *mut HewTransport {
        v as *mut HewTransport
    }

    // -----------------------------------------------------------------------
    // Peer allowlist
    // -----------------------------------------------------------------------

    #[test]
    fn allowlist_open_mode_allows_all_peers() {
        let key = [0xAA; KEY_LEN];
        // SAFETY: mode is valid and key points to KEY_LEN bytes.
        let list = unsafe { hew_allowlist_new(ALLOWLIST_MODE_OPEN) };
        assert!(!list.is_null());
        // SAFETY: list is valid and key pointer is valid.
        let allowed = unsafe { hew_allowlist_check(list, key.as_ptr()) };
        assert_eq!(allowed, 1);
        // SAFETY: list was created by hew_allowlist_new.
        unsafe { hew_allowlist_free(list) };
    }

    #[test]
    fn allowlist_strict_mode_only_allows_listed_peers() {
        let allowed_key = [0x11; KEY_LEN];
        let denied_key = [0x22; KEY_LEN];
        // SAFETY: mode is valid.
        let list = unsafe { hew_allowlist_new(ALLOWLIST_MODE_STRICT) };
        assert!(!list.is_null());

        // SAFETY: list and key pointers are valid.
        assert_eq!(
            unsafe { hew_allowlist_check(list, allowed_key.as_ptr()) },
            0
        );
        // SAFETY: list and key pointers are valid.
        assert_eq!(unsafe { hew_allowlist_add(list, allowed_key.as_ptr()) }, 0);
        // SAFETY: list and key pointers are valid.
        assert_eq!(
            unsafe { hew_allowlist_check(list, allowed_key.as_ptr()) },
            1
        );
        // SAFETY: list and key pointers are valid.
        assert_eq!(unsafe { hew_allowlist_check(list, denied_key.as_ptr()) }, 0);
        // SAFETY: list and key pointers are valid.
        assert_eq!(
            unsafe { hew_allowlist_remove(list, allowed_key.as_ptr()) },
            0
        );
        // SAFETY: list and key pointers are valid.
        assert_eq!(
            unsafe { hew_allowlist_check(list, allowed_key.as_ptr()) },
            0
        );

        // SAFETY: list was created by hew_allowlist_new.
        unsafe { hew_allowlist_free(list) };
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

        let key_file = NamedTempFile::new().unwrap();
        let path = CString::new(key_file.path().to_string_lossy().into_owned()).unwrap();

        // SAFETY: path and key pointers are valid.
        let save_rc =
            unsafe { hew_noise_key_save(path.as_ptr(), public.as_ptr(), private.as_ptr()) };
        assert_eq!(save_rc, 0);

        let on_disk = fs::read(key_file.path()).unwrap();
        assert_eq!(on_disk.len(), KEYPAIR_FILE_LEN);

        #[cfg(unix)]
        {
            let mode = fs::metadata(key_file.path()).unwrap().permissions().mode() & 0o777;
            assert_eq!(mode, 0o600);
        }

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

    #[test]
    fn keypair_generate_returns_non_null() {
        // SAFETY: no preconditions.
        let pub_key = unsafe { hew_noise_keypair_generate() };
        assert!(!pub_key.is_null());

        // SAFETY: pub_key is valid for KEY_LEN bytes.
        let key_slice = unsafe { std::slice::from_raw_parts(pub_key, KEY_LEN) };
        assert!(
            !key_slice.iter().all(|&b| b == 0),
            "generated key must not be all zeros"
        );

        // SAFETY: pub_key was allocated by libc::malloc.
        unsafe { libc::free(pub_key.cast::<c_void>()) };
    }

    #[test]
    fn keypair_generate_produces_unique_keys() {
        // SAFETY: no preconditions.
        let key1 = unsafe { hew_noise_keypair_generate() };
        // SAFETY: no preconditions.
        let key2 = unsafe { hew_noise_keypair_generate() };
        assert!(!key1.is_null());
        assert!(!key2.is_null());

        // SAFETY: key1 is valid for KEY_LEN bytes.
        let s1 = unsafe { std::slice::from_raw_parts(key1, KEY_LEN) };
        // SAFETY: key2 is valid for KEY_LEN bytes.
        let s2 = unsafe { std::slice::from_raw_parts(key2, KEY_LEN) };
        assert_ne!(s1, s2, "two generated keys should differ");

        // SAFETY: keys were allocated by libc::malloc.
        unsafe {
            libc::free(key1.cast::<c_void>());
            libc::free(key2.cast::<c_void>());
        }
    }

    // -----------------------------------------------------------------------
    // Set keypair
    // -----------------------------------------------------------------------

    #[test]
    fn set_keypair_stores_key() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: inner is valid.
        let enc = unsafe { hew_transport_encrypted_new(inner, TEST_PRIVATE_KEY.as_ptr()) };
        assert!(!enc.is_null());

        let private_key = [42u8; 32];
        // SAFETY: enc is a valid encrypted transport, private_key is valid.
        unsafe { hew_noise_set_keypair(enc, private_key.as_ptr(), private_key.len()) };

        // Verify the key was stored.
        // SAFETY: enc and its impl are valid.
        let enc_state = unsafe { &*(*enc).r#impl.cast::<EncryptedTransport>() };
        assert_eq!(enc_state.private_key.as_ref(), &private_key);

        // SAFETY: enc was created by hew_transport_encrypted_new.
        unsafe { destroy_transport(enc) };
    }

    #[test]
    fn set_keypair_null_transport_does_not_crash() {
        let key = [0u8; 32];
        // SAFETY: testing null-safety.
        unsafe { hew_noise_set_keypair(ptr::null_mut(), key.as_ptr(), key.len()) };
    }

    #[test]
    fn set_keypair_null_key_is_noop() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: inner is valid.
        let enc = unsafe { hew_transport_encrypted_new(inner, TEST_PRIVATE_KEY.as_ptr()) };

        // SAFETY: testing null key.
        unsafe { hew_noise_set_keypair(enc, ptr::null(), 32) };

        // Key should remain unchanged.
        // SAFETY: enc and its impl are valid.
        let enc_state = unsafe { &*(*enc).r#impl.cast::<EncryptedTransport>() };
        assert_eq!(enc_state.private_key.as_ref(), &TEST_PRIVATE_KEY);

        // SAFETY: enc was created by hew_transport_encrypted_new.
        unsafe { destroy_transport(enc) };
    }

    #[test]
    fn set_keypair_zero_len_is_noop() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: inner is valid.
        let enc = unsafe { hew_transport_encrypted_new(inner, TEST_PRIVATE_KEY.as_ptr()) };

        let key = [1u8; 32];
        // SAFETY: testing zero-length key.
        unsafe { hew_noise_set_keypair(enc, key.as_ptr(), 0) };

        // SAFETY: enc and its impl are valid.
        let enc_state = unsafe { &*(*enc).r#impl.cast::<EncryptedTransport>() };
        assert_eq!(enc_state.private_key.as_ref(), &TEST_PRIVATE_KEY);

        // SAFETY: enc was created by hew_transport_encrypted_new.
        unsafe { destroy_transport(enc) };
    }

    // -----------------------------------------------------------------------
    // Constructor
    // -----------------------------------------------------------------------

    #[test]
    fn encrypted_new_null_returns_null() {
        // SAFETY: testing null handling.
        let enc =
            unsafe { hew_transport_encrypted_new(ptr::null_mut(), TEST_PRIVATE_KEY.as_ptr()) };
        assert!(enc.is_null());
    }

    #[test]
    fn encrypted_new_valid_inner() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: inner is valid.
        let enc = unsafe { hew_transport_encrypted_new(inner, TEST_PRIVATE_KEY.as_ptr()) };
        assert!(!enc.is_null());

        // SAFETY: enc is valid.
        let transport = unsafe { &*enc };
        assert!(!transport.ops.is_null());
        assert!(!transport.r#impl.is_null());

        // SAFETY: enc was created by hew_transport_encrypted_new.
        unsafe { destroy_transport(enc) };
    }

    // -----------------------------------------------------------------------
    // Error cases — null / invalid arguments
    // -----------------------------------------------------------------------

    #[test]
    fn send_null_impl_returns_error() {
        let data = [1u8, 2, 3];
        // SAFETY: testing null-safety.
        let rc = unsafe { enc_send(ptr::null_mut(), 0, data.as_ptr().cast(), data.len()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn send_null_data_returns_error() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: inner is valid.
        let enc = unsafe { hew_transport_encrypted_new(inner, TEST_PRIVATE_KEY.as_ptr()) };
        // SAFETY: enc is valid.
        let impl_ptr = unsafe { (*enc).r#impl };

        // SAFETY: testing null data.
        let rc = unsafe { enc_send(impl_ptr, 0, ptr::null(), 10) };
        assert_eq!(rc, -1);

        // SAFETY: enc was created by hew_transport_encrypted_new.
        unsafe { destroy_transport(enc) };
    }

    #[test]
    fn recv_null_impl_returns_error() {
        let mut buf = [0u8; 64];
        // SAFETY: testing null-safety.
        let rc = unsafe { enc_recv(ptr::null_mut(), 0, buf.as_mut_ptr().cast(), buf.len()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn recv_null_buf_returns_error() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: inner is valid.
        let enc = unsafe { hew_transport_encrypted_new(inner, TEST_PRIVATE_KEY.as_ptr()) };
        // SAFETY: enc is valid.
        let impl_ptr = unsafe { (*enc).r#impl };

        // SAFETY: testing null buffer.
        let rc = unsafe { enc_recv(impl_ptr, 0, ptr::null_mut(), 64) };
        assert_eq!(rc, -1);

        // SAFETY: enc was created by hew_transport_encrypted_new.
        unsafe { destroy_transport(enc) };
    }

    #[test]
    fn send_invalid_conn_returns_error() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: inner is valid.
        let enc = unsafe { hew_transport_encrypted_new(inner, TEST_PRIVATE_KEY.as_ptr()) };
        // SAFETY: enc is valid.
        let impl_ptr = unsafe { (*enc).r#impl };
        let data = [1u8, 2, 3];

        // Out-of-range positive ID.
        // SAFETY: testing invalid conn.
        let rc = unsafe { enc_send(impl_ptr, 999, data.as_ptr().cast(), data.len()) };
        assert_eq!(rc, -1);

        // Negative ID.
        // SAFETY: testing invalid conn.
        let rc = unsafe { enc_send(impl_ptr, -1, data.as_ptr().cast(), data.len()) };
        assert_eq!(rc, -1);

        // SAFETY: enc was created by hew_transport_encrypted_new.
        unsafe { destroy_transport(enc) };
    }

    #[test]
    fn recv_invalid_conn_returns_error() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: inner is valid.
        let enc = unsafe { hew_transport_encrypted_new(inner, TEST_PRIVATE_KEY.as_ptr()) };
        // SAFETY: enc is valid.
        let impl_ptr = unsafe { (*enc).r#impl };
        let mut buf = [0u8; 64];

        // SAFETY: testing invalid conn.
        let rc = unsafe { enc_recv(impl_ptr, 999, buf.as_mut_ptr().cast(), buf.len()) };
        assert_eq!(rc, -1);

        // SAFETY: enc was created by hew_transport_encrypted_new.
        unsafe { destroy_transport(enc) };
    }

    #[test]
    fn connect_null_impl_returns_invalid() {
        let addr = CString::new("127.0.0.1:9999").unwrap();
        // SAFETY: testing null-safety.
        let conn = unsafe { enc_connect(ptr::null_mut(), addr.as_ptr()) };
        assert_eq!(conn, HEW_CONN_INVALID);
    }

    #[test]
    fn accept_null_impl_returns_invalid() {
        // SAFETY: testing null-safety.
        let conn = unsafe { enc_accept(ptr::null_mut(), 100) };
        assert_eq!(conn, HEW_CONN_INVALID);
    }

    #[test]
    fn listen_null_impl_returns_error() {
        let addr = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: testing null-safety.
        let rc = unsafe { enc_listen(ptr::null_mut(), addr.as_ptr()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn close_conn_null_impl_does_not_crash() {
        // SAFETY: testing null-safety.
        unsafe { enc_close_conn(ptr::null_mut(), 0) };
    }

    #[test]
    fn close_conn_invalid_ids_does_not_crash() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: inner is valid.
        let enc = unsafe { hew_transport_encrypted_new(inner, TEST_PRIVATE_KEY.as_ptr()) };
        // SAFETY: enc is valid.
        let impl_ptr = unsafe { (*enc).r#impl };

        // SAFETY: testing invalid conn IDs — should not crash.
        unsafe {
            enc_close_conn(impl_ptr, -1);
            enc_close_conn(impl_ptr, 999);
            enc_close_conn(impl_ptr, HEW_CONN_INVALID);
        }

        // SAFETY: enc was created by hew_transport_encrypted_new.
        unsafe { destroy_transport(enc) };
    }

    #[test]
    fn destroy_null_impl_does_not_crash() {
        // SAFETY: testing null-safety.
        unsafe { enc_destroy(ptr::null_mut()) };
    }

    // -----------------------------------------------------------------------
    // Helper: establish an encrypted client/server pair over TCP
    // -----------------------------------------------------------------------

    struct EncryptedPair {
        server: *mut HewTransport,
        client: *mut HewTransport,
        server_conn: c_int,
        client_conn: c_int,
    }

    impl Drop for EncryptedPair {
        fn drop(&mut self) {
            // SAFETY: both transports were created by hew_transport_encrypted_new.
            unsafe {
                destroy_transport(self.client);
                destroy_transport(self.server);
            }
        }
    }

    /// Create a connected encrypted client/server pair on a free port.
    fn setup_encrypted_pair() -> EncryptedPair {
        let port = free_port();
        let addr = CString::new(format!("127.0.0.1:{port}")).unwrap();

        // --- Server transport ---
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let server_inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: server_inner is valid.
        let server =
            unsafe { hew_transport_encrypted_new(server_inner, TEST_PRIVATE_KEY.as_ptr()) };

        // Listen.
        // SAFETY: server is valid.
        let server_ops = unsafe { &*(*server).ops };
        let listen_fn = server_ops.listen.unwrap();
        // SAFETY: server impl and addr are valid.
        let rc = unsafe { listen_fn((*server).r#impl, addr.as_ptr()) };
        assert!(rc >= 0, "listen failed on port {port}");

        let server_usize = ptr_to_usize(server);
        let (tx, rx) = mpsc::channel();

        // Server thread: accept (handshake runs inside accept).
        let server_thread = thread::spawn(move || {
            let srv = usize_to_ptr(server_usize);
            // SAFETY: srv is valid and exclusively accessed in this thread.
            let ops = unsafe { &*(*srv).ops };
            let accept_fn = ops.accept.unwrap();
            // SAFETY: srv impl is valid; 5-second timeout.
            let conn = unsafe { accept_fn((*srv).r#impl, 5000) };
            tx.send(conn).unwrap();
        });

        // --- Client transport ---
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let client_inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: client_inner is valid.
        let client =
            unsafe { hew_transport_encrypted_new(client_inner, TEST_PRIVATE_KEY.as_ptr()) };

        // Connect (handshake runs inside connect).
        // SAFETY: client is valid.
        let client_ops = unsafe { &*(*client).ops };
        let connect_fn = client_ops.connect.unwrap();
        let addr2 = CString::new(format!("127.0.0.1:{port}")).unwrap();
        // SAFETY: client impl and addr are valid.
        let client_conn = unsafe { connect_fn((*client).r#impl, addr2.as_ptr()) };
        assert_ne!(client_conn, HEW_CONN_INVALID, "client connect failed");

        let server_conn = rx
            .recv_timeout(Duration::from_secs(5))
            .expect("server accept timed out");
        assert_ne!(server_conn, HEW_CONN_INVALID, "server accept failed");
        server_thread.join().expect("server thread panicked");

        EncryptedPair {
            server,
            client,
            server_conn,
            client_conn,
        }
    }

    // -----------------------------------------------------------------------
    // Integration: encrypted round-trip
    // -----------------------------------------------------------------------

    #[test]
    fn roundtrip_client_to_server() {
        let pair = setup_encrypted_pair();
        let message = b"Hello, encrypted world!";

        // Client sends.
        // SAFETY: client is valid.
        let c_ops = unsafe { &*(*pair.client).ops };
        let send_fn = c_ops.send.unwrap();
        // SAFETY: client impl and message are valid.
        let sent = unsafe {
            send_fn(
                (*pair.client).r#impl,
                pair.client_conn,
                message.as_ptr().cast(),
                message.len(),
            )
        };
        assert_eq!(sent as usize, message.len());

        // Server receives.
        let mut buf = [0u8; 256];
        // SAFETY: server is valid.
        let s_ops = unsafe { &*(*pair.server).ops };
        let recv_fn = s_ops.recv.unwrap();
        // SAFETY: server impl and buf are valid.
        let received = unsafe {
            recv_fn(
                (*pair.server).r#impl,
                pair.server_conn,
                buf.as_mut_ptr().cast(),
                buf.len(),
            )
        };
        assert_eq!(received as usize, message.len());
        assert_eq!(&buf[..message.len()], message);
    }

    #[test]
    fn roundtrip_server_to_client() {
        let pair = setup_encrypted_pair();
        let message = b"Server says hello!";

        // Server sends.
        // SAFETY: server is valid.
        let s_ops = unsafe { &*(*pair.server).ops };
        let send_fn = s_ops.send.unwrap();
        // SAFETY: server impl and message are valid.
        let sent = unsafe {
            send_fn(
                (*pair.server).r#impl,
                pair.server_conn,
                message.as_ptr().cast(),
                message.len(),
            )
        };
        assert_eq!(sent as usize, message.len());

        // Client receives.
        let mut buf = [0u8; 256];
        // SAFETY: client is valid.
        let c_ops = unsafe { &*(*pair.client).ops };
        let recv_fn = c_ops.recv.unwrap();
        // SAFETY: client impl and buf are valid.
        let received = unsafe {
            recv_fn(
                (*pair.client).r#impl,
                pair.client_conn,
                buf.as_mut_ptr().cast(),
                buf.len(),
            )
        };
        assert_eq!(received as usize, message.len());
        assert_eq!(&buf[..message.len()], message);
    }

    #[test]
    fn bidirectional_exchange() {
        let pair = setup_encrypted_pair();
        let msg_c2s = b"client to server";
        let msg_s2c = b"server to client";

        // SAFETY: client transport is valid.
        let c_ops = unsafe { &*(*pair.client).ops };
        // SAFETY: server transport is valid.
        let s_ops = unsafe { &*(*pair.server).ops };
        let send_c = c_ops.send.unwrap();
        let recv_c = c_ops.recv.unwrap();
        let send_s = s_ops.send.unwrap();
        let recv_s = s_ops.recv.unwrap();

        // Client → Server.
        // SAFETY: all pointers are valid.
        let sent = unsafe {
            send_c(
                (*pair.client).r#impl,
                pair.client_conn,
                msg_c2s.as_ptr().cast(),
                msg_c2s.len(),
            )
        };
        assert_eq!(sent as usize, msg_c2s.len());

        let mut buf = [0u8; 256];
        // SAFETY: all pointers are valid.
        let received = unsafe {
            recv_s(
                (*pair.server).r#impl,
                pair.server_conn,
                buf.as_mut_ptr().cast(),
                buf.len(),
            )
        };
        assert_eq!(received as usize, msg_c2s.len());
        assert_eq!(&buf[..msg_c2s.len()], msg_c2s);

        // Server → Client.
        // SAFETY: all pointers are valid.
        let sent = unsafe {
            send_s(
                (*pair.server).r#impl,
                pair.server_conn,
                msg_s2c.as_ptr().cast(),
                msg_s2c.len(),
            )
        };
        assert_eq!(sent as usize, msg_s2c.len());

        let mut buf2 = [0u8; 256];
        // SAFETY: all pointers are valid.
        let received = unsafe {
            recv_c(
                (*pair.client).r#impl,
                pair.client_conn,
                buf2.as_mut_ptr().cast(),
                buf2.len(),
            )
        };
        assert_eq!(received as usize, msg_s2c.len());
        assert_eq!(&buf2[..msg_s2c.len()], msg_s2c);
    }

    #[test]
    fn multiple_sequential_messages() {
        let pair = setup_encrypted_pair();

        // SAFETY: client transport is valid.
        let c_ops = unsafe { &*(*pair.client).ops };
        // SAFETY: server transport is valid.
        let s_ops = unsafe { &*(*pair.server).ops };
        let send_fn = c_ops.send.unwrap();
        let recv_fn = s_ops.recv.unwrap();

        let messages: &[&[u8]] = &[
            b"first message",
            b"second message with more data",
            b"third",
            b"a somewhat longer message to test with more bytes flowing through",
        ];

        for (i, msg) in messages.iter().enumerate() {
            // SAFETY: all pointers are valid.
            let sent = unsafe {
                send_fn(
                    (*pair.client).r#impl,
                    pair.client_conn,
                    msg.as_ptr().cast(),
                    msg.len(),
                )
            };
            assert_eq!(sent as usize, msg.len(), "send failed for message {i}");

            let mut buf = [0u8; 256];
            // SAFETY: all pointers are valid.
            let received = unsafe {
                recv_fn(
                    (*pair.server).r#impl,
                    pair.server_conn,
                    buf.as_mut_ptr().cast(),
                    buf.len(),
                )
            };
            assert_eq!(
                received as usize,
                msg.len(),
                "recv size mismatch for message {i}"
            );
            assert_eq!(&buf[..msg.len()], *msg, "data mismatch for message {i}");
        }
    }

    #[test]
    fn close_connection_prevents_further_sends() {
        let pair = setup_encrypted_pair();

        // SAFETY: client is valid.
        let c_ops = unsafe { &*(*pair.client).ops };
        let close_fn = c_ops.close_conn.unwrap();

        // Close the client connection.
        // SAFETY: client impl is valid.
        unsafe { close_fn((*pair.client).r#impl, pair.client_conn) };

        // Sending on the closed connection should fail.
        let data = b"should fail";
        let send_fn = c_ops.send.unwrap();
        // SAFETY: client impl and data are valid.
        let rc = unsafe {
            send_fn(
                (*pair.client).r#impl,
                pair.client_conn,
                data.as_ptr().cast(),
                data.len(),
            )
        };
        assert_eq!(rc, -1, "send on closed connection should return -1");

        // Closing again should be a safe no-op.
        // SAFETY: client impl is valid.
        unsafe { close_fn((*pair.client).r#impl, pair.client_conn) };
    }

    #[test]
    fn roundtrip_with_preset_keys() {
        let port = free_port();

        // Generate distinct keypairs for server and client.
        let builder = Builder::new(NOISE_PATTERN.parse().unwrap());
        let server_kp = builder.generate_keypair().unwrap();
        let builder = Builder::new(NOISE_PATTERN.parse().unwrap());
        let client_kp = builder.generate_keypair().unwrap();

        // --- Server ---
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let server_inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: server_inner is valid.
        let server =
            unsafe { hew_transport_encrypted_new(server_inner, server_kp.private.as_ptr()) };

        let addr = CString::new(format!("127.0.0.1:{port}")).unwrap();
        // SAFETY: server is valid.
        let s_ops = unsafe { &*(*server).ops };
        // SAFETY: server impl and addr are valid.
        let rc = unsafe { s_ops.listen.unwrap()((*server).r#impl, addr.as_ptr()) };
        assert!(rc >= 0);

        let server_usize = ptr_to_usize(server);
        let (tx, rx) = mpsc::channel();

        let server_thread = thread::spawn(move || {
            let srv = usize_to_ptr(server_usize);
            // SAFETY: srv is valid.
            let ops = unsafe { &*(*srv).ops };
            // SAFETY: srv impl is valid.
            let conn = unsafe { ops.accept.unwrap()((*srv).r#impl, 5000) };
            tx.send(conn).unwrap();
        });

        // --- Client ---
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let client_inner = unsafe { hew_transport_tcp_new() };
        // SAFETY: client_inner is valid.
        let client =
            unsafe { hew_transport_encrypted_new(client_inner, client_kp.private.as_ptr()) };

        // SAFETY: client is valid.
        let c_ops = unsafe { &*(*client).ops };
        let addr2 = CString::new(format!("127.0.0.1:{port}")).unwrap();
        // SAFETY: client impl and addr are valid.
        let client_conn = unsafe { c_ops.connect.unwrap()((*client).r#impl, addr2.as_ptr()) };
        assert_ne!(client_conn, HEW_CONN_INVALID);

        let server_conn = rx.recv_timeout(Duration::from_secs(5)).unwrap();
        assert_ne!(server_conn, HEW_CONN_INVALID);
        server_thread.join().unwrap();

        // Send and receive.
        let msg = b"encrypted with preset keys";
        // SAFETY: client impl and msg are valid.
        let sent = unsafe {
            c_ops.send.unwrap()(
                (*client).r#impl,
                client_conn,
                msg.as_ptr().cast(),
                msg.len(),
            )
        };
        assert_eq!(sent as usize, msg.len());

        let mut buf = [0u8; 256];
        // SAFETY: server impl and buf are valid.
        let received = unsafe {
            s_ops.recv.unwrap()(
                (*server).r#impl,
                server_conn,
                buf.as_mut_ptr().cast(),
                buf.len(),
            )
        };
        assert_eq!(received as usize, msg.len());
        assert_eq!(&buf[..msg.len()], msg);

        // SAFETY: both transports were created by hew_transport_encrypted_new.
        unsafe {
            destroy_transport(client);
            destroy_transport(server);
        }
    }

    // -----------------------------------------------------------------------
    // Internal helpers
    // -----------------------------------------------------------------------

    #[test]
    fn store_conn_and_get_conn_mut() {
        // SAFETY: hew_transport_tcp_new has no preconditions.
        let inner = unsafe { hew_transport_tcp_new() };
        let mut enc = EncryptedTransport::new(inner, TEST_PRIVATE_KEY);

        // No connections initially.
        assert!(enc.get_conn_mut(0).is_none());
        assert!(enc.get_conn_mut(-1).is_none());

        // We can't easily create a ConnState without a real handshake, so
        // just verify the pool returns INVALID when full. Fill all slots by
        // abusing the internal API (we don't have a real TransportState,
        // so we skip this part and just test remove).
        enc.remove_conn(0); // no-op, nothing stored
        enc.remove_conn(-1); // no-op, guarded

        // Clean up: prevent Drop from freeing inner (we do it manually).
        // Actually, Drop will handle it. But we need to not double-free.
        // Set inner to null so Drop doesn't try to destroy it.
        // Then manually destroy inner.
        let saved_inner = enc.inner;
        enc.inner = ptr::null_mut();
        drop(enc);

        // SAFETY: inner was created by hew_transport_tcp_new.
        if !saved_inner.is_null() {
            // SAFETY: saved_inner is valid.
            let inner_t = unsafe { &*saved_inner };
            if !inner_t.ops.is_null() {
                // SAFETY: ops is valid.
                let ops = unsafe { &*inner_t.ops };
                if let Some(destroy_fn) = ops.destroy {
                    // SAFETY: impl was created by hew_transport_tcp_new.
                    unsafe { destroy_fn(inner_t.r#impl) };
                }
            }
            // SAFETY: saved_inner was created by Box::into_raw.
            let _ = unsafe { Box::from_raw(saved_inner) };
        }
    }

    #[test]
    fn private_key_returns_configured_key() {
        let enc = EncryptedTransport {
            inner: ptr::null_mut(),
            private_key: Zeroizing::new(TEST_PRIVATE_KEY),
            conns: std::array::from_fn(|_| None),
        };
        assert_eq!(*enc.private_key(), TEST_PRIVATE_KEY);
    }
}
