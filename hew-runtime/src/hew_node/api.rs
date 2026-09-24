//! Process-wide node API: connect, start/shutdown, registry and identity FFI.

use super::{
    hew_node_free, hew_node_lookup_location, hew_node_new, hew_node_register,
    hew_node_set_auth_snapshot, hew_node_start, hew_node_stop, hew_node_unregister,
    normalize_transport_name, peer_transport_env_name, peer_transport_from_normalized, reply_table,
    transport_selection_from_env, with_current_node, with_current_node_read, HewNode,
    TransportSelection,
};
use crate::connection;
use crate::node_identity::{HewNodeId, HewRemotePid};
use crate::peer_binding::{
    ConfigState, PeerCredential, TransportSelection as PeerTransport, PEER_AUTH_STATE,
};
use crate::set_last_error;
use crate::transport::HEW_CONN_INVALID;
use crate::vec::HewVec;
use hew_cabi::string::{string_as_str, string_to_cstring, HewString};
use std::ffi::{c_char, c_int, CStr, CString};
use std::ptr;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

unsafe fn parse_connect_target(addr: *const c_char) -> Option<(Option<u16>, CString)> {
    // SAFETY: caller validates non-null and C-string.
    let c_addr = unsafe { CStr::from_ptr(addr) };
    let addr_text = c_addr.to_string_lossy();
    if let Some((prefix, target)) = addr_text.split_once('@') {
        if let Ok(node_id) = prefix.parse::<u16>() {
            if node_id != 0 && !target.is_empty() {
                let c_target = CString::new(target.as_bytes()).ok()?;
                return Some((Some(node_id), c_target));
            }
        }
    }
    Some((None, CString::new(c_addr.to_bytes()).ok()?))
}

/// Connect to a remote node and register routing for its node ID.
///
/// Supports `"<node_id>@<addr>"` to require an exact peer `NodeId`. Without a
/// prefix, the authenticated handshake `NodeId` is authoritative.
///
/// # Safety
///
/// - `node` must be valid.
/// - `addr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_connect(node: *mut HewNode, addr: *const c_char) -> c_int {
    if node.is_null() || addr.is_null() {
        set_last_error("hew_node_connect: node or addr is null");
        return -1;
    }
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &mut *node };
    if node.transport.is_null() || node.conn_mgr.is_null() {
        set_last_error("hew_node_connect: node is not started");
        return -1;
    }

    // SAFETY: addr pointer is non-null and valid by caller contract.
    let Some((expected_peer_node_id, target_addr)) = (unsafe { parse_connect_target(addr) }) else {
        set_last_error("hew_node_connect: invalid connect target");
        return -1;
    };

    // SAFETY: transport pointer validated above.
    let t = unsafe { &*node.transport };
    // SAFETY: valid transport vtable pointer from transport object.
    let Some(ops) = (unsafe { t.ops.as_ref() }) else {
        set_last_error("hew_node_connect: transport ops are null");
        return -1;
    };
    let Some(connect_fn) = ops.connect else {
        set_last_error("hew_node_connect: transport connect op missing");
        return -1;
    };

    // SAFETY: transport impl and C string are valid.
    let conn_id = unsafe { connect_fn(t.r#impl, target_addr.as_ptr()) };
    if conn_id == HEW_CONN_INVALID {
        set_last_error("hew_node_connect: transport connect failed");
        return -1;
    }
    if let Some(expected) = expected_peer_node_id {
        // SAFETY: conn_mgr is live and conn_id was just returned by this node's
        // transport. The expectation is consumed by hew_connmgr_add.
        if unsafe { connection::hew_connmgr_expect_peer(node.conn_mgr, conn_id, expected) } != 0 {
            // hew_connmgr_expect_peer does not take ownership on failure.
            // SAFETY: transport and conn_id are still owned by this function.
            if let Some(close_fn) = ops.close_conn {
                // SAFETY: conn_id was just created by this transport and has not
                // been transferred to the connection manager.
                unsafe { close_fn(t.r#impl, conn_id) };
            }
            return -1;
        }
    }

    // SAFETY: conn_mgr pointer is valid and owned by this node.
    if unsafe { connection::hew_connmgr_add(node.conn_mgr, conn_id) } != 0 {
        // hew_connmgr_add owns conn_id cleanup on failure; no close needed here.
        let detail = {
            let error = crate::hew_last_error();
            if error.is_null() {
                None
            } else {
                // SAFETY: `hew_last_error` returns a live NUL-terminated string.
                Some(
                    unsafe { CStr::from_ptr(error) }
                        .to_string_lossy()
                        .into_owned(),
                )
            }
        };
        set_last_error(detail.map_or_else(
            || "hew_node_connect: failed to add connection".to_string(),
            |detail| format!("hew_node_connect: failed to add connection: {detail}"),
        ));
        return -1;
    }
    // SAFETY: conn_mgr and conn_id are valid on successful add.
    let _ = unsafe {
        connection::hew_connmgr_configure_reconnect(
            node.conn_mgr,
            conn_id,
            target_addr.as_ptr(),
            1,
            0,
            expected_peer_node_id.map_or(0, i32::from),
        )
    };

    0
}

// ============================================================================
// Simplified Node API for Hew-language builtins
// ============================================================================
//
// These functions manage the CURRENT_NODE internally, providing a stateless
// interface for the compiler-generated code. Each corresponds to a
// `Node::*` builtin in the Hew language.

/// Per-process offset for receiver-local route-slot allocation.
///
/// When a process calls `Node::start` more than once, each call gets a
/// distinct offset added to the process base.
static LOCAL_ROUTE_SLOT_COUNTER: std::sync::atomic::AtomicU32 =
    std::sync::atomic::AtomicU32::new(0);

/// Per-process base for receiver-local route-slot allocation.
static PROCESS_ROUTE_SLOT_BASE: std::sync::OnceLock<u16> = std::sync::OnceLock::new();

/// Derive a non-zero local route-slot base from the OS process ID.
///
/// Uses a FNV-1a–style fold to spread the PID bits across the full u16
/// range, then forces non-zero because route slot zero is local dispatch.
fn process_route_slot_base() -> u16 {
    *PROCESS_ROUTE_SLOT_BASE.get_or_init(|| {
        let pid = std::process::id(); // u32 OS PID
                                      // FNV-1a 32-bit fold into 16 bits.
        let h = (2_166_136_261u32)
            .wrapping_mul(16_777_619)
            .wrapping_add(pid)
            .wrapping_mul(16_777_619)
            .wrapping_add(pid >> 8)
            .wrapping_mul(16_777_619)
            .wrapping_add(pid >> 16);
        #[expect(
            clippy::cast_possible_truncation,
            reason = "XOR of two u16-range halves of a u32 always fits in u16"
        )]
        let folded = ((h >> 16) ^ (h & 0xFFFF)) as u16;
        // Ensure non-zero (0 is local dispatch).
        if folded == 0 {
            1
        } else {
            folded
        }
    })
}

/// Map `(base, offset)` to a route slot in `1..=65535`.
///
/// A bare `base.wrapping_add(offset)` can wrap a second `Node::start` to `0`
/// when `base == 65535` (offset 1). Mapping through the
/// `1..=65535` ring (`1 + ((base - 1 + offset) mod 65535)`) keeps the result a
/// valid peer route slot for any base/offset, including the wrap case.
pub(super) fn next_local_route_slot(base: u16, offset: u32) -> u16 {
    // Work in u32 to avoid intermediate u16 wrap; the ring has 65535 slots
    // (1..=65535). `base` is already guaranteed non-zero by
    // `process_route_slot_base`, so `base - 1` is in `0..=65534`.
    let ring = 65_535u32;
    let zero_based = (u32::from(base) - 1 + (offset % ring)) % ring;
    #[expect(
        clippy::cast_possible_truncation,
        reason = "zero_based is in 0..65535 so 1 + zero_based fits in u16 (max 65535)"
    )]
    let route_slot = (1 + zero_based) as u16;
    route_slot
}

pub(super) fn select_local_route_slot(
    base: u16,
    offset: u32,
    bindings: &crate::peer_binding::PeerBindings,
) -> Option<std::num::NonZeroU16> {
    for attempt in 0..65_535 {
        let candidate = next_local_route_slot(base, offset.wrapping_add(attempt));
        if bindings.get(&candidate).is_none() {
            return std::num::NonZeroU16::new(candidate);
        }
    }
    None
}

/// Merge the start-time transport selection into staged pre-start config.
///
/// # Errors
///
/// Returns a typed message when an environment value is malformed.
fn merge_start_env_into_config(
    cfg: &mut crate::peer_binding::PeerAuthConfig,
) -> Result<(), String> {
    // Transport (issue #2652): a selection pinned by an earlier transport-
    // sensitive op (`Node::set_transport` / `Node::load_keys` /
    // `Node::allow_peer`) is authoritative and wins over any later env change —
    // start uses the STORED selection. Only when unpinned do we adopt the
    // start-time env selection.
    let effective = if let Some(pinned) = cfg.transport {
        pinned
    } else {
        let sel = transport_selection_from_env()?.as_peer_transport();
        cfg.transport = Some(sel);
        sel
    };
    // Re-assert the effective selection onto `HEW_TRANSPORT` so the low-level
    // transport construction (which reads the env) builds the stored selection,
    // not a value that diverged after the pin.
    // SAFETY: ENV_LOCK synchronises access to the process-global environ array.
    crate::env::ENV_LOCK.access(|()| unsafe {
        std::env::set_var("HEW_TRANSPORT", peer_transport_env_name(effective));
    });
    Ok(())
}

/// `Node::start(addr)` — Create and start a node, binding to `addr`.
///
/// # Safety
///
/// `addr` must be a valid null-terminated C string.
/// ABI view of the source-owned `NodeConfig` record. `Node.start` consumes the
/// record, so this boundary copies the configuration into Rust values and
/// releases every managed field before staging the runtime state.
#[repr(C)]
#[derive(Debug)]
pub struct HewNodeConfig {
    pub(super) bind: *const HewString,
    pub(super) transport: *const HewString,
    pub(super) key: *const HewString,
    pub(super) trust: *const HewString,
    pub(super) peers: *mut HewVec,
    pub(super) seeds: *mut HewVec,
}

/// Serializes complete source-owned `NodeConfig` transactions. It owns no
/// configuration state; `PEER_AUTH_STATE` remains the lifecycle authority.
static NODE_CONFIG_TRANSACTION: std::sync::LazyLock<Mutex<()>> =
    std::sync::LazyLock::new(|| Mutex::new(()));

const _: () = {
    assert!(std::mem::size_of::<HewNodeConfig>() == 6 * std::mem::size_of::<usize>());
    assert!(std::mem::offset_of!(HewNodeConfig, bind) == 0);
    assert!(std::mem::offset_of!(HewNodeConfig, transport) == std::mem::size_of::<usize>());
    assert!(std::mem::offset_of!(HewNodeConfig, key) == 2 * std::mem::size_of::<usize>());
    assert!(std::mem::offset_of!(HewNodeConfig, trust) == 3 * std::mem::size_of::<usize>());
    assert!(std::mem::offset_of!(HewNodeConfig, peers) == 4 * std::mem::size_of::<usize>());
    assert!(std::mem::offset_of!(HewNodeConfig, seeds) == 5 * std::mem::size_of::<usize>());
};

struct ConsumedNodeConfig {
    bind: *const HewString,
    transport: *const HewString,
    key: *const HewString,
    trust: *const HewString,
    peers: *mut HewVec,
    seeds: *mut HewVec,
}

impl Drop for ConsumedNodeConfig {
    fn drop(&mut self) {
        for field in [self.bind, self.transport, self.key, self.trust] {
            if !field.is_null() {
                // SAFETY: this owner is created only from the six moved fields
                // of one NodeConfig and drops each managed string once.
                unsafe { crate::string::hew_string_drop(field.cast_mut()) };
            }
        }
        for field in [self.peers, self.seeds] {
            if !field.is_null() {
                // SAFETY: this owner is created only from the two moved vector
                // fields of one NodeConfig and drops each vector once.
                unsafe { crate::vec::hew_vec_free(field) };
            }
        }
    }
}

unsafe fn config_string(value: *const HewString, field: &str) -> Result<String, c_int> {
    if value.is_null() {
        set_last_error(format!("NodeConfig.{field} is null"));
        return Err(-1);
    }
    // SAFETY: ConsumedNodeConfig keeps the field live through this copy.
    Ok(unsafe { string_as_str(value) }.to_owned())
}

unsafe fn config_strings(value: *mut HewVec, field: &str) -> Result<Vec<String>, c_int> {
    if value.is_null() {
        set_last_error(format!("NodeConfig.{field} is null"));
        return Err(-1);
    }
    // SAFETY: ConsumedNodeConfig keeps the field live through this decode.
    let len = unsafe { crate::vec::hew_vec_len(value) };
    if len < 0 {
        set_last_error(format!("NodeConfig.{field} has an invalid length"));
        return Err(-1);
    }
    let mut strings = Vec::with_capacity(usize::try_from(len).map_err(|_| -1)?);
    for index in 0..len {
        // SAFETY: index is within the checked vector range.
        let item = unsafe { crate::vec::hew_vec_get_str(value, index) };
        if item.is_null() {
            set_last_error(format!("NodeConfig.{field}[{index}] is null"));
            return Err(-1);
        }
        // SAFETY: getter result is live until balanced below.
        strings.push(unsafe { string_as_str(item) }.to_owned());
        // SAFETY: balances the owned getter result.
        unsafe { crate::string::hew_string_drop(item.cast_mut()) };
    }
    Ok(strings)
}

/// Clear a previous incomplete source-config staging transaction. A running
/// node owns its state and must never be overwritten by a second start call.
fn reset_node_config_staging() -> Result<(), c_int> {
    let mut guard = PEER_AUTH_STATE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    match guard.state {
        ConfigState::Building(_) => {
            guard.state = ConfigState::default();
            Ok(())
        }
        ConfigState::Starting { .. } | ConfigState::Running { .. } => {
            set_last_error("Node::start: a public node lifecycle is already active (fail-closed)");
            Err(-1)
        }
    }
}

/// Start a node from one complete source-owned configuration record.
///
/// # Safety
///
/// The config pointer must name the exact compiler-lowered source record
/// and remain live for the duration of this call.
#[expect(
    clippy::too_many_lines,
    reason = "one transaction keeps the staged configuration and rollback boundaries auditable"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_start_config(config: *const HewNodeConfig) -> c_int {
    let _transaction = NODE_CONFIG_TRANSACTION
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if config.is_null() {
        set_last_error("Node::start: NodeConfig is null");
        return -1;
    }
    // SAFETY: the ABI contract above requires a live compiler-lowered record.
    let config = unsafe { &*config };
    let owned = ConsumedNodeConfig {
        bind: config.bind,
        transport: config.transport,
        key: config.key,
        trust: config.trust,
        peers: config.peers,
        seeds: config.seeds,
    };
    // SAFETY: each helper validates its managed source handle before borrowing.
    let Ok(bind) = (unsafe { config_string(owned.bind, "bind") }) else {
        return -1;
    };
    // SAFETY: each helper validates its managed source handle before borrowing.
    let Ok(transport) = (unsafe { config_string(owned.transport, "transport") }) else {
        return -1;
    };
    let key = if owned.key.is_null() {
        // The managed empty-string representation is a null handle. This is the
        // NodeConfig.at default; start validation refuses it until the caller
        // supplies a stable key path.
        String::new()
    } else {
        // SAFETY: the helper validates the non-null managed source handle.
        let Ok(key) = (unsafe { config_string(owned.key, "key") }) else {
            return -1;
        };
        key
    };
    // SAFETY: each helper validates its managed source handle before borrowing.
    let Ok(trust) = (unsafe { config_string(owned.trust, "trust") }) else {
        return -1;
    };
    // SAFETY: the helper validates and balances every borrowed vector string.
    let Ok(peers) = (unsafe { config_strings(owned.peers, "peers") }) else {
        return -1;
    };
    // SAFETY: the helper validates and balances every borrowed vector string.
    let Ok(seeds) = (unsafe { config_strings(owned.seeds, "seeds") }) else {
        return -1;
    };
    if trust != "pinned" {
        set_last_error("Node::start: NodeConfig.trust must be pinned");
        return -1;
    }
    let Ok(transport) = CString::new(transport) else {
        set_last_error("Node::start: transport contains NUL");
        return -1;
    };
    let key = if key.is_empty() {
        None
    } else if let Ok(key) = CString::new(key) {
        Some(key)
    } else {
        set_last_error("Node::start: key contains NUL");
        return -1;
    };
    let mut peer_credentials = Vec::with_capacity(peers.len());
    for peer in peers {
        if let Ok(peer) = CString::new(peer) {
            peer_credentials.push(peer);
        } else {
            set_last_error("Node::start: peer credential contains NUL");
            return -1;
        }
    }
    let Ok(bind_c) = CString::new(bind.clone()) else {
        set_last_error("Node::start: bind contains NUL");
        return -1;
    };
    let mut seed_addresses = Vec::with_capacity(seeds.len());
    for seed in seeds {
        if seed == bind {
            continue;
        }
        if let Ok(seed) = CString::new(seed) {
            seed_addresses.push(seed);
        } else {
            set_last_error("Node::start: seed contains NUL");
            return -1;
        }
    }

    // A complete config replaces an incomplete transaction rather than adding
    // fields to it. This is the only public path that stages these low-level
    // operations, and every error below restores the empty Building state.
    if reset_node_config_staging().is_err() {
        return -1;
    }
    let staged = (|| {
        // SAFETY: the C string lives until this closure returns; the callee
        // copies its selection into the locked staging record.
        if unsafe { hew_node_api_set_transport(transport.as_ptr()) } != 0 {
            return Err(-1);
        }
        if let Some(key) = key.as_ref() {
            // SAFETY: the key C string lives for this complete transaction.
            if unsafe { hew_node_api_load_keys(key.as_ptr()) } != 0 {
                return Err(-1);
            }
        }
        for (index, credential) in peer_credentials.iter().enumerate() {
            let Ok(slot) = u16::try_from(index + 1) else {
                set_last_error("Node::start: too many pinned peers");
                return Err(-1);
            };
            // SAFETY: the credential C string lives for this transaction; the
            // low-level call copies and validates it before returning.
            if unsafe { hew_node_api_allow_peer(slot, credential.as_ptr()) } != 0 {
                return Err(-1);
            }
        }
        // SAFETY: the bind C string lives until the legacy start call has read it.
        if unsafe { hew_node_api_start(bind_c.as_ptr()) } != 0 {
            return Err(-1);
        }
        Ok(())
    })();
    if staged.is_err() {
        let _ = reset_node_config_staging();
        return -1;
    }

    for seed in seed_addresses {
        // SAFETY: each seed C string is live for the duration of this call.
        if unsafe { hew_node_api_connect(seed.as_ptr()) } != 0 {
            // SAFETY: a successful start above owns the singleton public node;
            // shutdown releases it and resets its staging state.
            unsafe { node_api_shutdown_inner() };
            return -1;
        }
    }
    0
}

/// Create and start a node, binding to addr.
///
/// # Safety
///
/// The address must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_start(addr: *const c_char) -> c_int {
    if addr.is_null() {
        set_last_error("Node::start: address is null");
        return -1;
    }
    // Fail-closed: a failed pre-start peer-auth step (`Node::load_keys` /
    // `Node::allow_peer`) poisons the staged config; the check runs below inside
    // the staging lock (D14 — the poison lives on the per-node config, not a
    // process-global record). The mesh `ensure_identity` boundary re-checks the
    // installed snapshot as defence in depth.

    // ── Public owner-scoped staging (BLOCK-6) ─────────────────────────────
    // The singleton public `Node::*` API stages config in `PEER_AUTH_STATE`.
    // Transition `Building → Starting{owner}` under the lock, then run the
    // shared low-level start WITHOUT the lock (it reads the node's installed
    // snapshot, never `ConfigState`), then re-acquire for `Running` / restore.
    let offset = LOCAL_ROUTE_SLOT_COUNTER.fetch_add(1, Ordering::Relaxed);
    let (node, cfg, generation) = {
        let mut guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let ConfigState::Building(building) = &guard.state else {
            // A public node lifecycle already owns the staging state
            // (Starting/Running). Refuse a repeated public start (composes with
            // #2656, which owns the pre-allocation rejection).
            set_last_error("Node::start: a public node lifecycle is already active (fail-closed)");
            return -1;
        };
        // Merge start-time transport selection into the staged config.
        let mut cfg = building.clone();
        // Fail-closed: a failed pre-start peer-auth step poisoned the staged
        // config. Refuse to start rather than binding a listener with an
        // ephemeral self-signed identity (the operator's pinned key failed to
        // load) or an incomplete peer allowlist — the F6 fail-open. The Hew call
        // form discards this `-1`, so the operator-visible signal is the `hew: `
        // stderr diagnostic.
        if let Some(reason) = cfg.setup_error.as_deref() {
            let msg = format!(
                "Node::start: refusing to bind listener — peer-auth setup failed (fail-closed): {reason}"
            );
            eprintln!("hew: {msg}");
            set_last_error(msg);
            return -1;
        }
        if let Err(msg) = merge_start_env_into_config(&mut cfg) {
            eprintln!("hew: {msg}");
            set_last_error(msg);
            return -1;
        }
        // Pre-listen public validation (fail-closed before allocation).
        if let Err(msg) = cfg.validate_public() {
            eprintln!("hew: {msg}");
            set_last_error(msg);
            return -1;
        }
        // Allocate a receiver-local route slot that cannot collide with a
        // configured peer slot.
        let Some(route_slot) =
            select_local_route_slot(process_route_slot_base(), offset, cfg.bindings())
        else {
            let msg = "Node::start: no free local route slot remains after peer bindings";
            eprintln!("hew: {msg}");
            set_last_error(msg);
            return -1;
        };
        cfg.local_route_slot = Some(route_slot);
        let snapshot = match cfg.snapshot_for_start() {
            Ok(snapshot) => snapshot,
            Err(msg) => {
                eprintln!("hew: {msg}");
                set_last_error(msg);
                return -1;
            }
        };
        // SAFETY: addr was null-checked above and is a valid C string.
        let node = unsafe { hew_node_new(route_slot.get(), addr) };
        if node.is_null() {
            // State unchanged (still Building); the staged config is intact.
            return -1;
        }
        // Install the frozen per-node snapshot before the low-level start.
        // SAFETY: node was just created and is STOPPED.
        if unsafe { hew_node_set_auth_snapshot(node, snapshot) } != 0 {
            // SAFETY: node is valid, not started; free it.
            unsafe { hew_node_free(node) };
            return -1;
        }
        let generation = guard.next_generation;
        guard.next_generation = guard.next_generation.wrapping_add(1);
        guard.state = ConfigState::Starting {
            generation,
            owner: node as usize,
            config: cfg.clone(),
        };
        (node, cfg, generation)
    };

    // SAFETY: node was created above; the low-level start reads node.auth only.
    let rc = unsafe { hew_node_start(node) };
    let mut guard = PEER_AUTH_STATE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if rc == 0 {
        // Success: compute the standalone identity export clone and go Running.
        let identity_export = cfg.identity_export_string();
        let node_identity = cfg.node_identity;
        if matches!(&guard.state, ConfigState::Starting { generation: g, owner, .. }
            if *g == generation && *owner == node as usize)
        {
            guard.state = ConfigState::Running {
                generation,
                owner: node as usize,
                identity_export,
                node_identity,
            };
        }
        0
    } else {
        // Public fail_start: restore the exact held config iff owner+generation
        // still match (a concurrent stage that advanced generation is untouched).
        if let ConfigState::Starting {
            generation: g,
            owner,
            config,
        } = &guard.state
        {
            if *g == generation && *owner == node as usize {
                let restored = config.clone();
                guard.state = ConfigState::Building(restored);
            }
        }
        drop(guard);
        // SAFETY: node is valid but not started; free the allocation.
        unsafe { hew_node_free(node) };
        rc
    }
}

/// `Node::shutdown()` — Stop and free the current node.
///
/// # Safety
///
/// Must only be called when a node was previously started via
/// [`hew_node_api_start`]. The global `CURRENT_NODE` pointer must still be
/// valid.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_shutdown() -> c_int {
    let _transaction = NODE_CONFIG_TRANSACTION
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    // SAFETY: the public lifecycle lock excludes an incomplete source start.
    unsafe { node_api_shutdown_inner() }
}

/// Stop the public node while the caller owns its complete lifecycle transaction.
unsafe fn node_api_shutdown_inner() -> c_int {
    // Claim CURRENT_NODE under the write lock so exactly one caller owns the
    // stop/free sequence.
    let Some(ptr) = with_current_node(|guard| {
        let ptr = *guard as *mut HewNode;
        if ptr.is_null() {
            return None;
        }
        *guard = 0;
        reply_table().fail_all();
        Some(ptr)
    }) else {
        return -1;
    };
    // SAFETY: ptr is non-null and was created by hew_node_api_start.
    unsafe { hew_node_stop(ptr) };
    // SAFETY: ptr is valid; the node has been stopped.
    unsafe { hew_node_free(ptr) };
    // Owner-scoped reset: return the public staging state to `Building` and bump
    // the generation ONLY when the shutting node owns it (mirrors
    // `hew_node_stop`'s `CURRENT_NODE` owner check). `owner` is compared as an
    // integer — never dereferenced. A secondary low-level `hew_node_stop` never
    // reaches this path, so it leaves `ConfigState` intact.
    {
        let mut guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let owner_matches = match &guard.state {
            ConfigState::Running { owner, .. } | ConfigState::Starting { owner, .. } => {
                *owner == ptr as usize
            }
            ConfigState::Building(_) => false,
        };
        if owner_matches {
            guard.state = ConfigState::default();
            guard.next_generation = guard.next_generation.wrapping_add(1);
        }
    }
    0
}

/// `Node::connect(addr)` — Connect the current node to a peer.
///
/// # Safety
///
/// `addr` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_connect(addr: *const c_char) -> c_int {
    if addr.is_null() {
        return -1;
    }
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            set_last_error("Node::connect: no active node");
            return -1;
        }
        // SAFETY: node and addr are non-null and validated above.
        unsafe { hew_node_connect(node, addr) }
    })
}

/// `Node::connect(addr)` from compiled Hew, which passes the managed string.
///
/// # Safety
///
/// `addr` must be null or a live managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_connect_string(addr: *const HewString) -> c_int {
    // SAFETY: caller supplies a live managed string for this synchronous copy.
    let Ok(addr) = (unsafe { string_to_cstring(addr) }) else {
        return -1;
    };
    // SAFETY: the CString is live and NUL-terminated for the call.
    unsafe { hew_node_api_connect(addr.as_ptr()) }
}

/// `Node::register(name, actor_ptr)` — Register a named actor.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string. `actor` must be a valid
/// pointer to a live [`crate::actor::HewActor`].
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_register(
    name: *const c_char,
    actor: *mut crate::actor::HewActor,
) -> c_int {
    if name.is_null() || actor.is_null() {
        return -1;
    }
    // SAFETY: actor was null-checked above and is a valid HewActor pointer.
    let actor_id = unsafe { (*actor).id };
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            set_last_error("Node::register: no active node");
            return -1;
        }
        // SAFETY: node, name, and actor_id are validated above.
        unsafe { hew_node_register(node, name, actor_id) }
    })
}

/// `Node::register<T>(name, pid)` — Register a named actor by bare PID.
///
/// An actor handle lowers to a bare `u64` PID at this ABI boundary rather than a
/// `*mut HewActor` pointer. The caller
/// extracts the PID via `hew_actor_pid` before passing it here.
///
/// Returns 0 on success, -1 on any of the fail-closed conditions:
/// - `name` is null
/// - `pid` is 0 (sentinel for an invalid actor)
/// - The actor is not found in the live-actor table
/// - No node has been started (`hew_node_api_start` not yet called)
/// - Internal name-string allocation fails
///
/// On failure, the last error is set via `set_last_error` so callers can
/// retrieve a diagnostic string.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string that remains live for
/// the duration of this call.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_register_by_pid(name: *const c_char, pid: u64) -> c_int {
    if name.is_null() {
        set_last_error("Node::register: name pointer is null");
        return -1;
    }
    if pid == 0 {
        set_last_error("Node::register: pid is zero (invalid actor)");
        return -1;
    }
    // Verify the actor is currently live in the actor table before attempting
    // to register it.  This prevents dangling registrations after an actor
    // stops between spawn and register.
    let is_live = crate::lifetime::live_actors::get_actor_ptr_by_id(pid).is_some();
    if !is_live {
        set_last_error(format!(
            "Node::register: actor with pid {pid} not found in live-actor table"
        ));
        return -1;
    }
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            set_last_error("Node::register: no active node (call Node::start first)");
            return -1;
        }
        // SAFETY: node is non-null; name is non-null and caller guarantees a
        // valid C string; pid was validated above.
        unsafe { hew_node_register(node, name, pid) }
    })
}

/// Managed-string adapter for compiler-owned `Node::register` calls.
///
/// This keeps the public C-string ABI above available to native clients while
/// making the Hew compiler's managed `String` ownership explicit at its FFI
/// boundary. Embedded NUL names fail closed instead of truncating a registry
/// identity.
///
/// # Safety
///
/// `name` must be null (the Hew empty string) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_register_by_pid_string(
    name: *const HewString,
    pid: u64,
) -> c_int {
    // SAFETY: caller supplies a live managed string for this synchronous copy.
    let Ok(name) = (unsafe { string_to_cstring(name) }) else {
        set_last_error("Node::register: name contains an embedded NUL");
        return -1;
    };
    // SAFETY: CString provides a live NUL-terminated pointer for this call.
    unsafe { hew_node_api_register_by_pid(name.as_ptr(), pid) }
}

/// `Node::unregister(name)` — remove a name registration from the active node.
///
/// The symmetric counterpart of [`hew_node_api_register_by_pid`]: it clears the
/// current mapping for `name` and broadcasts removal of that exact location so
/// a reordered old removal cannot erase a later re-registration. Returns 0 on
/// success, -1 on error (no active node, null name).
///
/// # Safety
///
/// `name` must be a valid null-terminated C string that remains live for the
/// duration of this call.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_unregister(name: *const c_char) -> c_int {
    if name.is_null() {
        set_last_error("Node::unregister: name pointer is null");
        return -1;
    }
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            set_last_error("Node::unregister: no active node (call Node::start first)");
            return -1;
        }
        // SAFETY: node is non-null; name is non-null and caller guarantees a
        // valid C string.
        unsafe { hew_node_unregister(node, name) }
    })
}

/// `Node::lookup(name)` exact-location runtime entry point.
///
/// Writes `out` only when the name resolves successfully.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string and `out` must be writable.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_lookup_location(
    name: *const c_char,
    out: *mut HewRemotePid,
) -> c_int {
    if name.is_null() || out.is_null() {
        return -1;
    }
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            return -1;
        }
        // SAFETY: node is pinned by the read lock; caller guarantees name/output.
        unsafe { hew_node_lookup_location(node, name, out) }
    })
}

/// Managed-string adapter for compiler-owned `Node::lookup` calls.
///
/// The public C-string ABI remains available above; this adapter owns the
/// conversion boundary so a Hew `String` is never reinterpreted as a C string.
///
/// # Safety
///
/// `name` must be null (the Hew empty string) or a live managed string handle,
/// and `out` must be writable.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_lookup_location_string(
    name: *const HewString,
    out: *mut HewRemotePid,
) -> c_int {
    // SAFETY: caller supplies a live managed string for this synchronous copy.
    let Ok(name) = (unsafe { string_to_cstring(name) }) else {
        return -1;
    };
    // SAFETY: CString and output pointer satisfy the public ABI's requirements.
    unsafe { hew_node_api_lookup_location(name.as_ptr(), out) }
}

/// `Node::set_transport(name)` — Set the transport type before starting.
///
/// Supported values: `"tcp"` (default), `"quic"`, `"quic-mesh"`.
/// Must be called before `Node::start` — and, per issue #2652, before any
/// transport-sensitive credential is staged (`Node::allow_peer` /
/// `Node::load_keys`). Peer credentials and stable identities are interpreted
/// under the pinned transport (a 32-byte Noise pubkey on TCP vs a cert SPKI on
/// quic-mesh), so once any is staged a transport change is refused fail-closed
/// rather than silently reinterpreting the already-staged material. The
/// selection is pinned into the pre-start `Building` config so `Node::start`
/// uses the stored selection.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_set_transport(name: *const c_char) -> c_int {
    // SAFETY: caller guarantees name is a valid C string (or null).
    let Some(s) = (unsafe { crate::util::cstr_to_str(&name, "hew_node_set_transport") }) else {
        return -1;
    };
    let normalized = match normalize_transport_name(s) {
        Ok(normalized) => normalized,
        Err(err) => {
            set_last_error(format!("Node::set_transport: {err}"));
            return -1;
        }
    };
    let requested = peer_transport_from_normalized(normalized);

    // Transport must be pinned BEFORE identity credential staging (issue #2652):
    // `Node::allow_peer` / `Node::load_keys` interpret their credential under the
    // pinned transport. A `set_transport` after staging would reinterpret the
    // already-staged Noise pubkeys / mesh SPKIs under a different transport —
    // credential confusion. Refuse fail-closed (no env mutation) when the
    // Building config already carries staged credentials and the request would
    // change the pinned selection; an idempotent re-selection of the same
    // transport is allowed. While the public node lifecycle owns the state
    // (Starting/Running) the started node already froze its snapshot, so leave
    // the env write to preserve legacy behaviour for any low-level node.
    //
    // Hold PEER_AUTH_STATE across the staged-credential check, the HEW_TRANSPORT
    // env write, AND the Building-config pin as ONE critical section. The check
    // and the pin must be atomic: if the guard were released between them, a
    // concurrent `Node::allow_peer` / `Node::load_keys` could stage a credential
    // under the old transport in the gap and have it silently reinterpreted
    // under the newly-pinned transport — the exact credential confusion the
    // check exists to prevent. ENV_LOCK is acquired *inside* the PEER_AUTH_STATE
    // guard, matching the PEER_AUTH_STATE → ENV_LOCK order that
    // `merge_start_env_into_config` already establishes (so the two paths cannot
    // deadlock against each other).
    {
        let mut guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let ConfigState::Building(cfg) = &guard.state {
            if cfg.has_staged_credentials() && cfg.transport != Some(requested) {
                drop(guard);
                let msg = "Node::set_transport: transport cannot be changed after peer \
                     credentials or a stable identity have been staged (Node::allow_peer / \
                     Node::load_keys) — select the transport before staging (fail-closed)"
                    .to_string();
                eprintln!("hew: {msg}");
                set_last_error(msg);
                return -1;
            }
        }

        // Still holding PEER_AUTH_STATE: set the process-global selection and pin
        // it into the Building config so `Node::start` (and `identity_key()`) use
        // the stored selection and a later start-time env merge does not override
        // it. Env write and pin are one indivisible step under the guard.
        // SAFETY: ENV_LOCK synchronises access to the process-global environ array;
        // set_var is safe under exclusive write access.
        crate::env::ENV_LOCK.access(|()| unsafe { std::env::set_var("HEW_TRANSPORT", normalized) });
        if let ConfigState::Building(cfg) = &mut guard.state {
            cfg.transport = Some(requested);
        }
    }
    0
}

/// Surface a `Node::load_keys` / `Node::allow_peer` peer-auth setup failure on
/// every channel:
///
/// - `hew_last_error` — the thread-local programmatic surface (unchanged from
///   the pre-F6 behaviour);
/// - a `hew: ` stderr diagnostic — operator-visible, because the Hew call form
///   discards the `-1` return (these builtins are typed `Unit`);
/// - the sticky setup-error poison on the staged `Building` config (D14 —
///   per-node config state, no process-global record), which makes the next
///   `Node::start` refuse to bind a listener (fail-closed) so a node never
///   silently comes up with an ephemeral identity or an incomplete peer
///   allowlist after the operator's setup failed.
fn node_peer_auth_setup_failed(msg: impl Into<String>) {
    let msg = msg.into();
    eprintln!("hew: {msg} (fail-closed)");
    // Poison the staged config. If a public node lifecycle already owns the
    // state (Starting/Running) the poison is moot: that node already froze and
    // installed its snapshot. First poison wins (a later generic failure must
    // not overwrite the specific first cause).
    {
        let mut guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let ConfigState::Building(cfg) = &mut guard.state {
            if cfg.setup_error.is_none() {
                cfg.setup_error = Some(msg.clone());
            }
        }
    }
    set_last_error(msg);
}

/// `Node::load_keys(path)` — Load (or, if absent, mint-and-persist) this node's
/// stable transport identity from `path`, selected by the pinned transport: a
/// mesh TLS identity (SPKI) on quic-mesh, a stable Noise static keypair on
/// tcp-noise. Must be called **before** `Node::start` so the listener/handshake
/// presents the loaded key. Peers pin the resulting public credential via
/// `Node::allow_peer`; persisting the key keeps that credential stable across
/// restarts. Native only (plain quic has no pinnable peer identity).
///
/// Returns `0` on success, `-1` on a null/invalid path or key I/O failure
/// (error string available via `hew_last_error`). Never fabricates an identity.
/// On failure the peer-auth setup is marked failed so a subsequent `Node::start`
/// refuses to bind a listener (fail-closed) rather than silently presenting an
/// ephemeral identity.
///
/// # Safety
///
/// `path` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_load_keys(path: *const c_char) -> c_int {
    // SAFETY: caller guarantees path is a valid C string (or null).
    let Some(p) = (unsafe { crate::util::cstr_to_str(&path, "Node::load_keys") }) else {
        node_peer_auth_setup_failed("Node::load_keys: invalid path argument");
        return -1;
    };
    // Transport-selected identity (issue #2652, D1): load the identity whose
    // credential the pinned transport interprets — a mesh TLS identity on
    // quic-mesh, a stable Noise static keypair on tcp-noise. Reading the
    // selection here also pins it early into the Building config so
    // `Node::identity_key()` can export the pinned credential before start.
    let selection = match transport_selection_from_env() {
        Ok(s) => s,
        Err(err) => {
            node_peer_auth_setup_failed(format!("Node::load_keys: {err}"));
            return -1;
        }
    };
    match selection {
        TransportSelection::Tcp => {
            #[cfg(feature = "encryption")]
            {
                match crate::encryption::noise_identity_load_or_create(std::path::Path::new(p)) {
                    Ok(identity) => {
                        let node_id =
                            crate::node_identity::NodeId::from_noise_static_key(&identity.public());
                        stage_loaded_identity(
                            std::path::Path::new(p),
                            node_id,
                            |cfg| cfg.noise_identity = Some(identity),
                            PeerTransport::Tcp,
                        )
                    }
                    Err(err) => {
                        node_peer_auth_setup_failed(format!("Node::load_keys: {err}"));
                        -1
                    }
                }
            }
            #[cfg(not(feature = "encryption"))]
            {
                let _ = p;
                node_peer_auth_setup_failed(
                    "Node::load_keys: tcp-noise identity requires the hew-runtime encryption \
                     feature (peer auth unavailable)",
                );
                -1
            }
        }
        #[cfg(feature = "quic")]
        TransportSelection::QuicMesh => {
            match crate::quic_mesh::mesh_identity_load_or_create(std::path::Path::new(p)) {
                Ok(material) => {
                    let node_id = crate::node_identity::NodeId::from_spki(material.spki());
                    stage_loaded_identity(
                        std::path::Path::new(p),
                        node_id,
                        |cfg| cfg.mesh_identity = Some(material),
                        PeerTransport::QuicMesh,
                    )
                }
                Err(err) => {
                    node_peer_auth_setup_failed(format!("Node::load_keys: {err}"));
                    -1
                }
            }
        }
        #[cfg(feature = "quic")]
        TransportSelection::Quic => {
            let _ = p;
            node_peer_auth_setup_failed(
                "Node::load_keys: plain quic transport has no pinnable peer identity \
                 (use quic-mesh or tcp-noise)",
            );
            -1
        }
    }
}

/// Stage a freshly loaded stable identity into the pre-start `Building` config
/// and pin the transport selection (issue #2652, D1/D14). The identity is
/// frozen into the per-node snapshot at `Node::start` and presented by the
/// listener (mesh via `install_auth`) / handshake (Noise). Rejected once the
/// public node lifecycle owns the state (`Starting`/`Running`) so a load never
/// races a running node. Returns `0` on success, `-1` after marking the
/// peer-auth setup failed when the config is locked.
#[cfg(any(feature = "quic", feature = "encryption"))]
fn stage_loaded_identity(
    identity_path: &std::path::Path,
    node_identity: crate::node_identity::NodeId,
    apply: impl FnOnce(&mut crate::peer_binding::PeerAuthConfig),
    selection: PeerTransport,
) -> c_int {
    let mut guard = PEER_AUTH_STATE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let ConfigState::Building(cfg) = &mut guard.state else {
        drop(guard);
        node_peer_auth_setup_failed(
            "Node::load_keys: peer auth config is locked while the public node \
             lifecycle is active (Starting/Running)",
        );
        return -1;
    };
    // Fail closed if a concurrent `Node::set_transport` re-pinned the selection
    // between the env read that chose which identity file to load (Noise vs
    // mesh) and this stage under the lock. The loaded identity is for
    // `selection`; staging it while the config is pinned to a different
    // transport would present the wrong stable identity at start (issue #2652).
    // The pinned config selection is authoritative — refuse rather than stage a
    // mismatched identity.
    if let Some(pinned) = cfg.transport {
        if pinned != selection {
            drop(guard);
            node_peer_auth_setup_failed(
                "Node::load_keys: the transport was changed concurrently while this \
                 identity was being loaded — select the transport before loading keys \
                 (fail-closed)",
            );
            return -1;
        }
    }
    apply(cfg);
    cfg.node_identity = Some(node_identity);
    cfg.identity_path = Some(identity_path.to_path_buf());
    // Early transport pin: loading a stable identity commits this node to the
    // operator's selected transport, so `identity_key()` can export the pinned
    // credential before `Node::start` freezes the snapshot. Idempotent with the
    // authoritative pin in `merge_start_env_into_config` (both read the env).
    if cfg.transport.is_none() {
        cfg.transport = Some(selection);
    }
    0
}

/// `Node::allow_peer(route_slot, credential_hex)` — pin a peer's authenticated
/// credential to a receiver-local route slot. The
/// credential is interpreted by the node's pinned transport (TCP ⇒ 32-byte
/// Noise pubkey; quic-mesh ⇒ cert SPKI). The exact one-to-one pin is staged
/// into the pre-start `Building` `PeerAuthConfig` and frozen at `Node::start`.
///
/// Returns `0` on success, `-1` on route slot zero, a null/odd/
/// non-hex string, a credential of the wrong length for the transport, a
/// route-slot or credential collision, or while the public node lifecycle owns
/// the config. Adds nothing on error.
///
/// # Safety
///
/// `credential_hex` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_allow_peer(
    route_slot: u16,
    credential_hex: *const c_char,
) -> c_int {
    // SAFETY: caller guarantees credential_hex is a valid C string (or null).
    let Some(s) = (unsafe { crate::util::cstr_to_str(&credential_hex, "Node::allow_peer") }) else {
        node_peer_auth_setup_failed("Node::allow_peer: invalid credential argument");
        return -1;
    };
    let Some(bytes) = decode_hex(s) else {
        node_peer_auth_setup_failed("Node::allow_peer: peer key must be hex-encoded SPKI bytes");
        return -1;
    };
    // Interpret the credential per the node's pinned transport (env-resolved;
    // the same selection is pinned into the config at start). Plain QUIC has no
    // peer-credential channel, so strict binding is unsupported there.
    let transport = match transport_selection_from_env() {
        Ok(sel) => sel,
        Err(msg) => {
            node_peer_auth_setup_failed(format!("Node::allow_peer: {msg}"));
            return -1;
        }
    };
    let credential = match transport {
        TransportSelection::Tcp => {
            let Ok(key) = <[u8; crate::peer_binding::NOISE_KEY_LEN]>::try_from(bytes.as_slice())
            else {
                node_peer_auth_setup_failed("Node::allow_peer: Noise pubkey must be 32 bytes");
                return -1;
            };
            PeerCredential::NoiseKey(key)
        }
        #[cfg(feature = "quic")]
        TransportSelection::QuicMesh => {
            if bytes.is_empty() || bytes.len() > MAX_SPKI_DECODE_BYTES {
                node_peer_auth_setup_failed("Node::allow_peer: mesh SPKI is empty or oversize");
                return -1;
            }
            PeerCredential::Spki(bytes)
        }
        #[cfg(feature = "quic")]
        TransportSelection::Quic => {
            node_peer_auth_setup_failed(
                "Node::allow_peer: strict binding unsupported on plain quic transport \
                 (use quic-mesh or tcp-noise)",
            );
            return -1;
        }
    };
    // Stage into the pre-start Building config. Rejected while the public node
    // lifecycle owns the state (Starting/Running).
    let mut guard = PEER_AUTH_STATE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let ConfigState::Building(cfg) = &mut guard.state else {
        drop(guard);
        node_peer_auth_setup_failed(
            "Node::allow_peer: peer auth config is locked while the public node lifecycle \
             is active (Starting/Running)",
        );
        return -1;
    };
    // Fail closed if a concurrent `Node::set_transport` re-pinned the selection
    // between the env read above (which typed `credential`) and this staging
    // under the lock. `credential` was interpreted as the type of `transport`;
    // staging it while the config is pinned to a different transport is exactly
    // the credential confusion issue #2652 guards against (a NoiseKey bound
    // while the node is pinned to quic-mesh, or vice versa). The pinned config
    // selection is authoritative — refuse rather than reinterpret.
    if let Some(pinned) = cfg.transport {
        if pinned != transport.as_peer_transport() {
            drop(guard);
            node_peer_auth_setup_failed(
                "Node::allow_peer: the transport was changed concurrently while this \
                 credential was being interpreted — select the transport before staging \
                 peer credentials (fail-closed)",
            );
            return -1;
        }
    }
    if let Err(err) = cfg.pin_peer(route_slot, credential) {
        let msg = format!("Node::allow_peer: {err}");
        drop(guard);
        node_peer_auth_setup_failed(msg);
        return -1;
    }
    // Pin the transport selection (issue #2652): the credential was interpreted
    // under `transport`, so bind the config to it now. Idempotent with the pins
    // in `Node::set_transport` / `stage_loaded_identity` and the start-time
    // merge; once pinned, a later `Node::set_transport` that would change it is
    // rejected via `has_staged_credentials`.
    if cfg.transport.is_none() {
        cfg.transport = Some(transport.as_peer_transport());
    }
    0
}

/// `Node::identity_key()` — Return this node's stable public credential for the
/// pinned transport as lowercase hex (issue #2652, D8): the Noise static public
/// key on TCP, the certificate SPKI on quic-mesh. Operators hand the returned
/// value to peers so they can bind it via `Node::allow_peer`.
///
/// Reads the three-state public staging config (BLOCK-7 point 3 — never
/// dereferences the owning node pointer):
/// - `Building` — the staged config (before start);
/// - `Starting` — the consumed config held for the in-flight start;
/// - `Running` — the standalone `identity_export` clone captured at the
///   `Starting → Running` transition.
///
/// Returns an **owned** managed string, which is null when no stable identity
/// has been loaded - null is the managed carrier's canonical empty string. The
/// caller frees it via `hew_string_drop`; the runtime never retains it.
#[no_mangle]
pub extern "C" fn hew_node_api_identity_key() -> *mut hew_cabi::string::HewString {
    let export = {
        let guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match &guard.state {
            ConfigState::Building(cfg) => cfg.identity_export_string(),
            ConfigState::Starting { config, .. } => config.identity_export_string(),
            ConfigState::Running {
                identity_export, ..
            } => identity_export.clone(),
        }
    };
    hew_cabi::string::string_from_str(&export)
}

/// `Node::id()` — write the stable key-derived node identity when configured.
///
/// Returns `0` and writes `out` on success; returns `-1` without writing when
/// no stable identity has been loaded.
///
/// # Safety
///
/// `out` must be writable when non-null.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_id(out: *mut HewNodeId) -> c_int {
    if out.is_null() {
        return -1;
    }
    let identity = {
        let guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match &guard.state {
            ConfigState::Building(cfg) => cfg.node_identity,
            ConfigState::Starting { config, .. } => config.node_identity,
            ConfigState::Running { node_identity, .. } => *node_identity,
        }
    };
    let Some(identity) = identity else {
        return -1;
    };
    // SAFETY: caller guarantees `out` is writable.
    unsafe { out.write(HewNodeId::from(identity)) };
    0
}

unsafe fn owned_identity_string(value: &str) -> *mut c_char {
    // SAFETY: `value` is valid UTF-8 and malloc_cstring copies the bytes.
    unsafe { crate::cabi::malloc_cstring(value.as_ptr(), value.len()) }
}

/// Format a `NodeId` as 32 lowercase hexadecimal digits.
///
/// # Safety
///
/// `node` must point to a readable `HewNodeId`.
#[no_mangle]
pub unsafe extern "C" fn hew_node_id_format(node: *const HewNodeId) -> *mut c_char {
    if node.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees `node` is readable.
    let node = unsafe { *node };
    // SAFETY: the formatted string is copied into Hew-owned storage.
    unsafe { owned_identity_string(&format!("{:016x}{:016x}", node.hi, node.lo)) }
}

/// Format a full location as `<node-hex>/<slot>@<session>`.
///
/// # Safety
///
/// `location` must point to a readable `HewRemotePid`.
#[no_mangle]
pub unsafe extern "C" fn hew_location_format(location: *const HewRemotePid) -> *mut c_char {
    if location.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees `location` is readable.
    let location = unsafe { *location };
    // SAFETY: the formatted string is copied into Hew-owned storage.
    unsafe {
        owned_identity_string(&format!(
            "{:016x}{:016x}/{}@{}",
            location.node.hi, location.node.lo, location.slot, location.incarnation
        ))
    }
}

/// (4 KiB). The mesh allowlist re-enforces this, but `decode_hex` checks it
/// *before* allocating so an oversize hex argument can't force a large up-front
/// `Vec`. A well-formed RSA-4096 SPKI is under 1 KiB; anything larger is junk.
pub(super) const MAX_SPKI_DECODE_BYTES: usize = 4096;

/// Decode a lowercase/uppercase hex string to bytes. Returns `None` on an odd
/// length, oversize input, or any non-hex digit — fail-closed, no partial decode.
pub(super) fn decode_hex(s: &str) -> Option<Vec<u8>> {
    let s = s.as_bytes();
    if s.is_empty() || !s.len().is_multiple_of(2) {
        return None;
    }
    // Bound before allocating: reject anything that would decode past the SPKI
    // cap so a huge argument can't drive a large `with_capacity`.
    if s.len() / 2 > MAX_SPKI_DECODE_BYTES {
        return None;
    }
    let nibble = |b: u8| -> Option<u8> {
        match b {
            b'0'..=b'9' => Some(b - b'0'),
            b'a'..=b'f' => Some(b - b'a' + 10),
            b'A'..=b'F' => Some(b - b'A' + 10),
            _ => None,
        }
    };
    let mut out = Vec::with_capacity(s.len() / 2);
    for pair in s.chunks_exact(2) {
        out.push((nibble(pair[0])? << 4) | nibble(pair[1])?);
    }
    Some(out)
}
