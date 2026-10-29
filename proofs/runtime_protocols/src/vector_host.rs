//! External contracts for ordinary vectors. No vector operation is replaced.
use hew_cabi::value::HewValueLayout;
use std::{ffi::c_void, ptr};

pub mod code_owner {
    use hew_cabi::code::HewCodeLeaseSlot;
    pub struct CodeRoot;
    impl CodeRoot {
        pub unsafe fn retain_for_carrier(slot: *const HewCodeLeaseSlot) {
            assert!(
                slot.is_null(),
                "vector: executable leases outside this proof"
            );
        }
        pub unsafe fn take(slot: *const HewCodeLeaseSlot) -> Self {
            Self::retain_for_carrier(slot);
            Self
        }
    }
}

pub mod owned_value_budget {
    use super::*;
    #[derive(Clone)]
    pub struct ValueOwner;
    pub struct ManagedStorage {
        pub identity: usize,
        pub bytes: u64,
    }
    pub struct BackingChange;
    pub struct ClonePermit;
    pub fn current_owner() -> Option<ValueOwner> {
        None
    }
    impl ValueOwner {
        pub fn prepare_backing_change(
            &self,
            _: Option<ManagedStorage>,
            _: u64,
            _: bool,
        ) -> Result<BackingChange, ()> {
            panic!("vector: quota path outside proof")
        }
        pub fn release_backing(&self, _: *mut c_void, _: u64) {
            panic!("vector: quota path outside proof")
        }
        pub unsafe fn reserve_clone(
            &self,
            _: &HewValueLayout,
            _: *const c_void,
        ) -> Result<ClonePermit, i32> {
            panic!("vector: quota path outside proof")
        }
    }
    impl BackingChange {
        pub fn commit(self, _: usize) {
            panic!("vector: quota path outside proof")
        }
    }
    impl ClonePermit {
        pub unsafe fn commit_value(self, _: &HewValueLayout, _: *mut c_void) -> Result<(), i32> {
            panic!("vector: quota path outside proof")
        }
    }
    pub mod visit {
        pub fn slot_bytes(_: &super::HewValueLayout) -> Result<u64, i32> {
            panic!("vector: quota path outside proof")
        }
    }
    pub mod materialize {
        use super::*;
        pub struct HewManagedInput<'a> {
            pub layout: &'a HewValueLayout,
            pub slot: *const c_void,
            pub copies: u64,
            pub consume: u8,
        }
        pub struct MaterializationPermit;
        pub unsafe fn invocation_owner(
            _: *const crate::coro_state::HewCoroState,
        ) -> Option<ValueOwner> {
            panic!("vector: managed invocation outside proof")
        }
        impl MaterializationPermit {
            pub unsafe fn prepare(
                _: ValueOwner,
                _: u64,
                _: &[HewManagedInput<'_>],
            ) -> Result<Self, i32> {
                panic!("vector: quota path outside proof")
            }
            pub unsafe fn commit(self, _: &HewValueLayout, _: *mut c_void) -> Result<(), i32> {
                panic!("vector: quota path outside proof")
            }
        }
    }
}

pub mod string {
    use hew_cabi::string::HewString;
    pub unsafe fn hew_string_clone(_: *const HewString) -> *mut HewString {
        panic!("vector: String callbacks outside proof")
    }
    pub unsafe fn hew_string_drop(_: *mut HewString) {
        panic!("vector: String callbacks outside proof")
    }
    pub unsafe fn hew_string_equals(_: *const HewString, _: *const HewString) -> i32 {
        panic!("vector: String callbacks outside proof")
    }
}
pub mod trait_object {
    pub unsafe fn hew_dyn_box_free(_: *mut u8, _: usize, _: usize) {
        panic!("vector: closure pairs outside proof")
    }
}

// Cursor boundary: detach moves bytes into stable storage; join preserves
// owners; synchronous consumption drops each once. The runtime's generic
// release walker is outside this component proof. Unsupported paths fail.
pub mod release_walker {
    use super::*;
    use hew_cabi::{string::HewString, vec::HewVec};
    pub const STEP_ELEMENTS: usize = 64;
    pub enum ReleaseItem {
        Vector {
            vec: *mut HewVec,
        },
        VectorStorage {
            vec: *mut HewVec,
        },
        VectorElements {
            vec: *mut HewVec,
            next: usize,
            end: usize,
        },
        Value {
            slot: *mut c_void,
            layout: HewValueLayout,
        },
        String {
            value: *mut HewString,
        },
    }
    #[repr(C)]
    #[derive(Clone, Copy)]
    struct Held {
        resource: *mut c_void,
        scalar: u64,
    }
    pub struct HewReleaseCursor {
        slots: [Held; 4],
        len: usize,
    }
    impl HewReleaseCursor {
        fn empty() -> Self {
            Self {
                slots: [Held {
                    resource: ptr::null_mut(),
                    scalar: 0,
                }; 4],
                len: 0,
            }
        }
        pub fn new(pending: Vec<ReleaseItem>) -> *mut Self {
            assert!(pending.is_empty(), "vector: unmodeled release cursor");
            ptr::null_mut()
        }
        pub unsafe fn detached(slot: *const c_void, layout: HewValueLayout) -> *mut Self {
            assert_eq!(
                layout.size,
                std::mem::size_of::<Held>(),
                "vector: unsupported release payload"
            );
            assert_eq!(
                layout.align,
                std::mem::align_of::<Held>(),
                "vector: unsupported release alignment"
            );
            assert!(
                layout.release_start.is_none(),
                "vector: asynchronous finalizer outside proof"
            );
            #[cfg(kani)]
            assert!(
                layout.drop_fn.is_some_and(|f| std::ptr::fn_addr_eq(
                    f,
                    crate::vec::formal_vectors::drop_element
                        as unsafe extern "C-unwind" fn(*mut c_void)
                )),
                "vector: wrong release recipe"
            );
            let mut cursor = Self::empty();
            cursor.slots[0] = slot.cast::<Held>().read();
            cursor.len = 1;
            Box::into_raw(Box::new(cursor))
        }
        pub unsafe fn join(cursors: Vec<*mut Self>) -> *mut Self {
            let mut result = Self::empty();
            for cursor in cursors {
                if !cursor.is_null() {
                    let cursor = Box::from_raw(cursor);
                    for i in 0..cursor.len {
                        assert!(result.len < 4, "vector: release scenario exceeds bound");
                        result.slots[result.len] = cursor.slots[i];
                        result.len += 1;
                    }
                }
            }
            if result.len == 0 {
                ptr::null_mut()
            } else {
                Box::into_raw(Box::new(result))
            }
        }
    }
    pub unsafe fn release_now(_: ReleaseItem) {
        panic!("vector: generic release walker outside proof")
    }
    pub unsafe fn release_deferred(_: ReleaseItem) {
        panic!("vector: generic release walker outside proof")
    }
    pub unsafe extern "C-unwind" fn hew_release_sync(cursor: *mut HewReleaseCursor) {
        #[cfg(not(kani))]
        assert!(
            cursor.is_null(),
            "vector: owned release requires Kani's callback monitor"
        );
        #[cfg(kani)]
        if !cursor.is_null() {
            let mut cursor = Box::from_raw(cursor);
            for i in 0..cursor.len {
                crate::vec::formal_vectors::drop_element((&raw mut cursor.slots[i]).cast());
            }
        }
    }
}
