//! Exclusive execution and semantic cleanup of compiler-owned session places.

use std::ffi::{c_void, CStr};
use std::ptr;

use hew_cabi::{host_error::HewError, live::HewLiveSession};
use serde::{Deserialize, Serialize};

use crate::native::{Generation, NativeHost};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;
type RuntimeNew = unsafe extern "C" fn() -> *mut c_void;
type RuntimeRelease = unsafe extern "C" fn(*mut c_void);
type SlotInitialized = unsafe extern "C" fn(*const HewLiveSession, usize) -> bool;
type SlotRevision = unsafe extern "C" fn(*const HewLiveSession, usize) -> u64;
type SlotLeafCount = unsafe extern "C" fn(*const HewLiveSession, usize) -> usize;
type SlotLeaf =
    unsafe extern "C" fn(*const HewLiveSession, usize, usize, *mut usize, *mut bool) -> *const u32;
type SlotRelease = unsafe extern "C" fn(*mut HewLiveSession, usize, *mut *mut HewError) -> i32;
type ErrorRelease = unsafe extern "C" fn(*mut HewError);
type Entry = unsafe extern "C" fn(*mut HewLiveSession, *mut *mut HewError) -> i32;

/// A copied diagnostic; its runtime-owned error has already been released.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fault {
    pub code: i32,
    pub message: String,
}

/// Actual ownership after execution, including execution that ended in a fault.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SlotState {
    pub initialized: bool,
    pub revision: u64,
    /// Canonical declaration-order field paths and their actual occupancy.
    pub leaves: Vec<(Vec<u32>, bool)>,
}

/// One submission's outcome. Mutations completed before a fault remain visible.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Execution {
    pub fault: Option<Fault>,
    pub slots: Vec<SlotState>,
}

#[derive(Debug)]
struct RuntimeApi {
    release: RuntimeRelease,
    initialized: SlotInitialized,
    revision: SlotRevision,
    leaf_count: SlotLeafCount,
    leaf: SlotLeaf,
    release_slot: SlotRelease,
    release_error: ErrorRelease,
}

/// Persistent places in a process that imports one shared Hew runtime.
///
/// Every call exclusively borrows the places. Their typed runtime owners retain
/// executable provenance, including callbacks and suspended cleanup. This host
/// never interprets a payload or reconstructs ownership from source text.
#[derive(Debug)]
pub struct NativeSession {
    host: NativeHost,
    runtime: *mut c_void,
    slots: Vec<*mut c_void>,
    api: RuntimeApi,
}

impl NativeSession {
    /// Start a session in an already admitted same-build runtime.
    ///
    /// # Errors
    /// Returns a missing runtime-symbol or runtime initialization error.
    pub fn new(host: NativeHost) -> Result<Self> {
        // SAFETY: NativeHost admission establishes this crate's same-build ABI.
        // Resolve every function before acquiring a runtime lifetime token.
        let (new, api) = unsafe {
            (
                *host.runtime_symbol::<RuntimeNew>(b"hew_runtime_new")?,
                RuntimeApi {
                    release: *host.runtime_symbol(b"hew_runtime_release")?,
                    initialized: *host.runtime_symbol(b"hew_live_slot_initialized")?,
                    revision: *host.runtime_symbol(b"hew_live_slot_revision")?,
                    leaf_count: *host.runtime_symbol(b"hew_live_slot_leaf_count")?,
                    leaf: *host.runtime_symbol(b"hew_live_slot_leaf")?,
                    release_slot: *host.runtime_symbol(b"hew_live_slot_release")?,
                    release_error: *host.runtime_symbol(b"hew_host_error_release")?,
                },
            )
        };
        // SAFETY: all symbols belong to the one admitted runtime image.
        let runtime = unsafe { new() };
        if runtime.is_null() {
            return Err("Hew runtime initialization failed".into());
        }
        Ok(Self {
            host,
            runtime,
            slots: Vec::new(),
            api,
        })
    }

    /// Execute one prepared submission against the retained places.
    ///
    /// `slot_count` is the compiler's next-slot bound, including new bindings.
    /// Existing places are never renumbered or shrunk by a submission. After a
    /// logical fault the returned occupancy describes the actual partial work;
    /// the frontend must use it rather than roll back or replay initializers.
    ///
    /// # Safety
    /// The entry must be a compiler-generated live submission with exactly the
    /// `Entry` ABI, prepared with executable-provenance tracking. Its retained
    /// parameter types and slot indices must match this session's prior receipts.
    /// Code must not retain or concurrently access the borrowed session context.
    ///
    /// # Errors
    /// Returns incompatible-host, missing-symbol, allocation or invalid ABI
    /// errors. A Hew logical failure is returned as `Execution::fault`.
    pub unsafe fn execute(
        &mut self,
        generation: &Generation,
        entry: &CStr,
        slot_count: usize,
    ) -> Result<Execution> {
        if !self.host.owns(generation) {
            return Err("submission belongs to another native host".into());
        }
        if slot_count < self.slots.len() {
            return Err("submission cannot discard existing session slots".into());
        }
        // Resolve the entry before changing the session's storage bound.
        // SAFETY: the caller establishes the compiler-generated entry's ABI.
        let entry = unsafe { generation.symbol::<Entry>(entry.to_bytes_with_nul())? };
        self.slots.try_reserve(slot_count - self.slots.len())?;
        self.slots.resize(slot_count, ptr::null_mut());
        let mut context = self.context();
        let mut error = ptr::null_mut();
        // SAFETY: the context is exclusively borrowed and the generation stays
        // rooted until this invocation and its error handling complete.
        let status = unsafe { entry(&raw mut context, &raw mut error) };
        let fault = self.take_fault(status, error)?;
        Ok(Execution {
            fault,
            slots: self.states(),
        })
    }

    /// Inspect occupancy without reading or cloning the retained payloads.
    #[must_use]
    pub fn states(&self) -> Vec<SlotState> {
        let context = HewLiveSession {
            slots: self.slots.as_ptr().cast_mut(),
            len: self.slots.len(),
        };
        (0..self.slots.len())
            .map(|index| {
                // SAFETY: the session is borrowed, no submission can run and
                // both queries accept an empty, in-bounds slot.
                unsafe {
                    SlotState {
                        initialized: (self.api.initialized)(&raw const context, index),
                        revision: (self.api.revision)(&raw const context, index),
                        leaves: (0..(self.api.leaf_count)(&raw const context, index))
                            .map(|leaf| {
                                let mut length = 0;
                                let mut initialized = false;
                                let path = (self.api.leaf)(
                                    &raw const context,
                                    index,
                                    leaf,
                                    &raw mut length,
                                    &raw mut initialized,
                                );
                                // The runtime retains the registered metadata
                                // through this exclusive, stopped inspection.
                                let path = if length == 0 {
                                    Vec::new()
                                } else {
                                    std::slice::from_raw_parts(path, length).to_vec()
                                };
                                (path, initialized)
                            })
                            .collect(),
                    }
                }
            })
            .collect()
    }

    /// Close every place in reverse declaration order, collecting all failures.
    ///
    /// Cleanup completes before its slot is discarded. A failed close does not
    /// prevent cleanup of the remaining places. The emptied session can be reused.
    pub fn clear(&mut self) -> Vec<Fault> {
        let faults = self.release_slots((0..self.slots.len()).rev());
        self.slots.clear();
        faults
    }

    /// Semantically release bindings hidden by a later submission. Slot indices
    /// stay stable for surviving bindings and future submissions.
    ///
    /// # Errors
    /// Rejects repeated or out-of-bounds indices before starting any cleanup.
    pub fn release(&mut self, slots: &[usize]) -> Result<Vec<Fault>> {
        let mut slots = slots.to_vec();
        slots.sort_unstable();
        if slots.last().is_some_and(|&slot| slot >= self.slots.len()) {
            return Err("release refers to an unknown session slot".into());
        }
        if slots.windows(2).any(|pair| pair[0] == pair[1]) {
            return Err("release repeats a session slot".into());
        }
        Ok(self.release_slots(slots.into_iter().rev()))
    }

    fn release_slots(&mut self, slots: impl Iterator<Item = usize>) -> Vec<Fault> {
        let mut context = self.context();
        let mut faults = Vec::new();
        for index in slots {
            let mut error = ptr::null_mut();
            // SAFETY: no invocation can overlap this exclusive borrow. Runtime
            // release owns suspension and final semantic cleanup of each slot.
            let status =
                unsafe { (self.api.release_slot)(&raw mut context, index, &raw mut error) };
            match self.take_fault(status, error) {
                Ok(Some(fault)) => faults.push(fault),
                Ok(None) => {}
                Err(error) => faults.push(Fault {
                    code: status,
                    message: error.to_string(),
                }),
            }
        }
        faults
    }

    fn context(&mut self) -> HewLiveSession {
        HewLiveSession {
            slots: self.slots.as_mut_ptr(),
            len: self.slots.len(),
        }
    }

    fn take_fault(&self, status: i32, error: *mut HewError) -> Result<Option<Fault>> {
        if error.is_null() {
            return if status == 0 {
                Ok(None)
            } else {
                Err(format!("submission returned status {status} without an error owner").into())
            };
        }
        // SAFETY: the runtime transferred one error owner with its same-build
        // representation. Copy its text, then release through its own runtime.
        let fault = unsafe {
            let fault = Fault {
                code: (*error).code(),
                message: (*error).message().to_owned(),
            };
            (self.api.release_error)(error);
            fault
        };
        if status == 0 || status != fault.code {
            return Err(format!(
                "submission status {status} disagrees with error code {}: {}",
                fault.code, fault.message
            )
            .into());
        }
        Ok(Some(fault))
    }
}

impl Drop for NativeSession {
    fn drop(&mut self) {
        // Explicit clear lets callers report cleanup failures; Drop still
        // completes every remaining cleanup and releases all error owners.
        let _ = self.clear();
        // SAFETY: slots and their cleanup are finished; this token was acquired
        // from the admitted runtime, which self.host still retains.
        unsafe { (self.api.release)(self.runtime) };
    }
}
