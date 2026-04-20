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
    use std::ffi::{c_char, c_void};
    use std::sync::{LazyLock, RwLock};

    use crate::util::RwLockExt;

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
    /// # Safety
    ///
    /// - If `name` is non-null, it must be a valid, NUL-terminated C string.
    /// - `actor` must be a valid pointer (or null — but that is the caller's choice).
    #[no_mangle]
    pub unsafe extern "C" fn hew_registry_register(name: *const c_char, actor: *mut c_void) -> i32 {
        // SAFETY: caller guarantees `name` is a live C string when non-null.
        let Some(key) = (unsafe { crate::util::cstr_to_str(&name, "hew_registry_register") })
        else {
            return -1;
        };
        let key = key.to_owned();
        let shard = REGISTRY.shard_for(&key);
        let mut reg = shard.write_or_recover();
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
    /// # Safety
    ///
    /// If `name` is non-null, it must be a valid, NUL-terminated C string.
    #[no_mangle]
    pub unsafe extern "C" fn hew_registry_lookup(name: *const c_char) -> *mut c_void {
        // SAFETY: caller guarantees `name` is a live C string when non-null.
        let Some(key) = (unsafe { crate::util::cstr_to_str(&name, "hew_registry_lookup") }) else {
            return std::ptr::null_mut();
        };
        let shard = REGISTRY.shard_for(key);
        let reg = shard.read_or_recover();
        reg.0.get(key).copied().unwrap_or(std::ptr::null_mut())
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
        // SAFETY: caller guarantees `name` is a live C string when non-null.
        let Some(key) = (unsafe { crate::util::cstr_to_str(&name, "hew_registry_unregister") })
        else {
            return -1;
        };
        let shard = REGISTRY.shard_for(key);
        let mut reg = shard.write_or_recover();
        if reg.0.remove(key).is_some() {
            0
        } else {
            -1
        }
    }

    /// Return the number of registered actors.
    #[no_mangle]
    pub extern "C" fn hew_registry_count() -> i32 {
        let mut total_count = 0usize;
        for shard in &REGISTRY.shards {
            let reg = shard.read_or_recover();
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
    #[no_mangle]
    pub extern "C" fn hew_registry_clear() {
        for shard in &REGISTRY.shards {
            let mut reg = shard.write_or_recover();
            reg.0.clear();
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use native::*;

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use std::ffi::CString;
    use std::sync::RwLock;

    use super::*;

    /// A poisoned `RwLock` shard must not crash subsequent registry operations.
    #[test]
    fn registry_survives_poisoned_shard() {
        // Poison a shard by panicking inside a write guard.
        let lock = RwLock::new(42);
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _guard = lock.write().unwrap();
            panic!("intentional poison");
        }));
        assert!(lock.is_poisoned());

        // After poisoning, the global registry should still work because
        // it uses write_or_recover / read_or_recover.
        let name = CString::new("poison_test_actor").unwrap();

        // SAFETY: name is a valid C string; 0x1 is a dummy non-null pointer.
        unsafe {
            // Register, lookup, count, unregister — none should panic.
            let result = hew_registry_register(name.as_ptr(), std::ptr::dangling_mut());
            assert_eq!(result, 0);

            let ptr = hew_registry_lookup(name.as_ptr());
            assert_eq!(ptr, std::ptr::dangling_mut());

            assert!(hew_registry_count() >= 1);

            let result = hew_registry_unregister(name.as_ptr());
            assert_eq!(result, 0);
        }
    }

    #[test]
    fn registry_rejects_invalid_utf8_keys() {
        hew_registry_clear();
        crate::hew_clear_error();
        let invalid_name = b"bad\xff\0";

        // SAFETY: `invalid_name` is NUL-terminated test input with invalid UTF-8 bytes.
        unsafe {
            assert_eq!(
                hew_registry_register(invalid_name.as_ptr().cast(), std::ptr::dangling_mut()),
                -1
            );
            assert!(hew_registry_lookup(invalid_name.as_ptr().cast()).is_null());
            assert_eq!(hew_registry_unregister(invalid_name.as_ptr().cast()), -1);
        }

        // SAFETY: the test just populated `hew_last_error()` with a NUL-terminated message.
        let err = unsafe { std::ffi::CStr::from_ptr(crate::hew_last_error()) }
            .to_str()
            .unwrap();
        assert!(err.contains("invalid UTF-8"), "unexpected error: {err}");
        assert_eq!(hew_registry_count(), 0);
    }
}

// ── WASM (single-threaded) implementation ───────────────────────────────────

#[cfg(target_arch = "wasm32")]
mod wasm {
    use std::collections::HashMap;
    use std::ffi::{c_char, c_void, CStr};
    use std::sync::Mutex;

    use crate::util::MutexExt;

    /// Wrapper around `*mut c_void` that implements Send.
    /// SAFETY: WASM is single-threaded, no cross-thread sharing.
    struct SendPtr(*mut c_void);
    unsafe impl Send for SendPtr {}

    static REGISTRY: Mutex<Option<HashMap<String, SendPtr>>> = Mutex::new(None);

    unsafe fn cstr_key<'a>(name: *const c_char, context: &str) -> Option<&'a str> {
        if name.is_null() {
            crate::set_last_error(format!("{context}: null pointer"));
            return None;
        }
        match unsafe { CStr::from_ptr(name) }.to_str() {
            Ok(key) => Some(key),
            Err(_) => {
                crate::set_last_error(format!("{context}: invalid UTF-8"));
                None
            }
        }
    }

    fn with_registry<F, R>(f: F) -> R
    where
        F: FnOnce(&mut HashMap<String, SendPtr>) -> R,
    {
        let mut guard = REGISTRY.lock_or_recover();
        f(guard.get_or_insert_with(HashMap::new))
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
        let Some(key) = (unsafe { cstr_key(name, "hew_registry_register") }) else {
            return -1;
        };
        let key = key.to_owned();
        with_registry(|reg| {
            if reg.contains_key(&key) {
                return -1;
            }
            reg.insert(key, SendPtr(actor));
            0
        })
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
        let Some(key) = (unsafe { cstr_key(name, "hew_registry_lookup") }) else {
            return std::ptr::null_mut();
        };
        with_registry(|reg| reg.get(key).map(|p| p.0).unwrap_or(std::ptr::null_mut()))
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
        let Some(key) = (unsafe { cstr_key(name, "hew_registry_unregister") }) else {
            return -1;
        };
        with_registry(|reg| if reg.remove(key).is_some() { 0 } else { -1 })
    }

    /// Return the number of registered actors.
    #[no_mangle]
    pub extern "C" fn hew_registry_count() -> i32 {
        with_registry(|reg| reg.len() as i32)
    }

    /// Remove all entries from the registry.
    #[no_mangle]
    pub extern "C" fn hew_registry_clear() {
        with_registry(|reg| reg.clear());
    }
}

#[cfg(target_arch = "wasm32")]
pub use wasm::*;
