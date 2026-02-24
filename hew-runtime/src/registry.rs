//! Hew runtime: `registry` module.
//!
//! Global registry mapping string names to actor pointers.
//!
//! On native targets, uses 256 `RwLock` shards to reduce lock contention.
//! On WASM (single-threaded), uses a plain `HashMap` — no locking needed.

// ── Native (multi-threaded) implementation ──────────────────────────────────

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use std::collections::HashMap;
    use std::ffi::{c_char, c_void, CStr};
    use std::sync::{LazyLock, RwLock};

    const N_SHARDS: usize = 256;

    /// Wrapper around the raw-pointer map so we can mark it `Send + Sync`.
    #[derive(Debug)]
    struct RegistryShard(HashMap<String, *mut c_void>);

    // SAFETY: The registry stores raw pointers that may be sent/shared between
    // threads. Callers are responsible for ensuring the pointed-to actors remain
    // valid for the duration of their registration.
    unsafe impl Send for RegistryShard {}
    // SAFETY: Access is serialized by the RwLock wrapping the RegistryShard.
    unsafe impl Sync for RegistryShard {}

    /// Sharded registry with 256 shards to reduce lock contention.
    struct ShardedRegistry {
        shards: [RwLock<RegistryShard>; N_SHARDS],
    }

    impl ShardedRegistry {
        /// Create a new sharded registry.
        fn new() -> Self {
            Self {
                shards: std::array::from_fn(|_| RwLock::new(RegistryShard(HashMap::new()))),
            }
        }

        /// Select the shard for a given name using FNV-1a hash.
        fn shard_for(&self, name: &str) -> &RwLock<RegistryShard> {
            let hash = fnv1a_hash(name.as_bytes());
            #[expect(
                clippy::cast_possible_truncation,
                reason = "hash is modulo N_SHARDS so will fit in usize"
            )]
            &self.shards[hash as usize % N_SHARDS]
        }
    }

    /// FNV-1a hash function for shard selection.
    fn fnv1a_hash(data: &[u8]) -> u64 {
        const FNV_OFFSET_BASIS: u64 = 14_695_981_039_346_656_037;
        const FNV_PRIME: u64 = 1_099_511_628_211;

        let mut hash = FNV_OFFSET_BASIS;
        for &byte in data {
            hash ^= u64::from(byte);
            hash = hash.wrapping_mul(FNV_PRIME);
        }
        hash
    }

    static REGISTRY: LazyLock<ShardedRegistry> = LazyLock::new(ShardedRegistry::new);

    /// Register an actor by name.
    ///
    /// Returns 0 on success, -1 if the name is already taken.
    ///
    /// # Panics
    ///
    /// Panics if the registry `RwLock` is poisoned.
    ///
    /// # Safety
    ///
    /// - If `name` is non-null, it must be a valid, NUL-terminated C string.
    /// - `actor` must be a valid pointer (or null — but that is the caller's choice).
    #[no_mangle]
    pub unsafe extern "C" fn hew_registry_register(name: *const c_char, actor: *mut c_void) -> i32 {
        if name.is_null() {
            return -1;
        }
        // SAFETY: `name` was checked for null above and caller guarantees it points
        // to a valid NUL-terminated C string.
        let key = unsafe { CStr::from_ptr(name) }
            .to_string_lossy()
            .into_owned();
        let shard = REGISTRY.shard_for(&key);
        let mut reg = shard.write().unwrap_or_else(|e| e.into_inner());
        if reg.0.contains_key(&key) {
            return -1;
        }
        reg.0.insert(key, actor);
        0
    }

    /// Look up an actor by name.
    ///
    /// Returns the actor pointer, or null if not found.
    ///
    /// # Panics
    ///
    /// Panics if the registry `RwLock` is poisoned.
    ///
    /// # Safety
    ///
    /// If `name` is non-null, it must be a valid, NUL-terminated C string.
    #[no_mangle]
    pub unsafe extern "C" fn hew_registry_lookup(name: *const c_char) -> *mut c_void {
        if name.is_null() {
            return std::ptr::null_mut();
        }
        // SAFETY: `name` was checked for null above and caller guarantees it points
        // to a valid NUL-terminated C string.
        let key = unsafe { CStr::from_ptr(name) }.to_string_lossy();
        let shard = REGISTRY.shard_for(key.as_ref());
        let reg = shard.read().unwrap_or_else(|e| e.into_inner());
        reg.0
            .get(key.as_ref())
            .copied()
            .unwrap_or(std::ptr::null_mut())
    }

    /// Remove an actor registration by name.
    ///
    /// Returns 0 on success, -1 if the name was not found.
    ///
    /// # Panics
    ///
    /// Panics if the registry `RwLock` is poisoned.
    ///
    /// # Safety
    ///
    /// If `name` is non-null, it must be a valid, NUL-terminated C string.
    #[no_mangle]
    pub unsafe extern "C" fn hew_registry_unregister(name: *const c_char) -> i32 {
        if name.is_null() {
            return -1;
        }
        // SAFETY: `name` was checked for null above and caller guarantees it points
        // to a valid NUL-terminated C string.
        let key = unsafe { CStr::from_ptr(name) }.to_string_lossy();
        let shard = REGISTRY.shard_for(key.as_ref());
        let mut reg = shard.write().unwrap_or_else(|e| e.into_inner());
        if reg.0.remove(key.as_ref()).is_some() {
            0
        } else {
            -1
        }
    }

    /// Return the number of registered actors.
    ///
    /// # Panics
    ///
    /// Panics if the registry `RwLock` is poisoned.
    #[no_mangle]
    pub extern "C" fn hew_registry_count() -> i32 {
        let mut total_count = 0usize;
        for shard in &REGISTRY.shards {
            let reg = shard.read().unwrap_or_else(|e| e.into_inner());
            total_count += reg.0.len();
        }
        #[expect(
            clippy::cast_possible_truncation,
            reason = "registry size won't exceed i32::MAX"
        )]
        #[expect(
            clippy::cast_possible_wrap,
            reason = "registry size won't exceed i32::MAX"
        )]
        let count = total_count as i32;
        count
    }

    /// Remove all entries from the registry.
    ///
    /// # Panics
    ///
    /// Panics if the registry `RwLock` is poisoned.
    #[no_mangle]
    pub extern "C" fn hew_registry_clear() {
        for shard in &REGISTRY.shards {
            let mut reg = shard.write().unwrap_or_else(|e| e.into_inner());
            reg.0.clear();
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use native::*;

// ── WASM (single-threaded) implementation ───────────────────────────────────

#[cfg(target_arch = "wasm32")]
mod wasm {
    use std::collections::HashMap;
    use std::ffi::{c_char, c_void, CStr};

    /// Simple single-threaded registry for WASM.
    /// SAFETY: WASM is single-threaded, no data races possible.
    static mut REGISTRY: Option<HashMap<String, *mut c_void>> = None;

    fn get_or_init() -> &'static mut HashMap<String, *mut c_void> {
        // SAFETY: WASM is single-threaded, no concurrent access.
        unsafe { REGISTRY.get_or_insert_with(HashMap::new) }
    }

    /// Register an actor by name.
    ///
    /// Returns 0 on success, -1 if the name is already taken.
    ///
    /// # Safety
    ///
    /// - If `name` is non-null, it must be a valid, NUL-terminated C string.
    /// - `actor` must be a valid pointer (or null — but that is the caller's choice).
    #[no_mangle]
    pub unsafe extern "C" fn hew_registry_register(name: *const c_char, actor: *mut c_void) -> i32 {
        if name.is_null() {
            return -1;
        }
        // SAFETY: `name` was checked for null above and caller guarantees it points
        // to a valid NUL-terminated C string.
        let key = unsafe { CStr::from_ptr(name) }
            .to_string_lossy()
            .into_owned();
        let reg = get_or_init();
        if reg.contains_key(&key) {
            return -1;
        }
        reg.insert(key, actor);
        0
    }

    /// Look up an actor by name.
    ///
    /// Returns the actor pointer, or null if not found.
    ///
    /// # Safety
    ///
    /// If `name` is non-null, it must be a valid, NUL-terminated C string.
    #[no_mangle]
    pub unsafe extern "C" fn hew_registry_lookup(name: *const c_char) -> *mut c_void {
        if name.is_null() {
            return std::ptr::null_mut();
        }
        // SAFETY: `name` was checked for null above and caller guarantees it points
        // to a valid NUL-terminated C string.
        let key = unsafe { CStr::from_ptr(name) }.to_string_lossy();
        get_or_init()
            .get(key.as_ref())
            .copied()
            .unwrap_or(std::ptr::null_mut())
    }

    /// Remove an actor registration by name.
    ///
    /// Returns 0 on success, -1 if the name was not found.
    ///
    /// # Safety
    ///
    /// If `name` is non-null, it must be a valid, NUL-terminated C string.
    #[no_mangle]
    pub unsafe extern "C" fn hew_registry_unregister(name: *const c_char) -> i32 {
        if name.is_null() {
            return -1;
        }
        // SAFETY: `name` was checked for null above and caller guarantees it points
        // to a valid NUL-terminated C string.
        let key = unsafe { CStr::from_ptr(name) }.to_string_lossy();
        if get_or_init().remove(key.as_ref()).is_some() {
            0
        } else {
            -1
        }
    }

    /// Return the number of registered actors.
    #[no_mangle]
    pub extern "C" fn hew_registry_count() -> i32 {
        get_or_init().len() as i32
    }

    /// Remove all entries from the registry.
    #[no_mangle]
    pub extern "C" fn hew_registry_clear() {
        get_or_init().clear();
    }
}

#[cfg(target_arch = "wasm32")]
pub use wasm::*;
