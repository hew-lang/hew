use std::ffi::c_void;
pub struct ActorStreamCancellation;
impl ActorStreamCancellation {
    pub fn close_reader(&self) { panic!("channel: actor stream cancellation outside model"); }
}
pub mod shutdown {
    pub const PHASE_DRAIN: i32 = 2;
    pub fn hew_shutdown_phase() -> i32 { panic!("channel: blocking shutdown path outside model"); }
}
pub mod driver { pub fn active() -> bool { false } }
pub mod code_owner {
    #[derive(Debug)]
    pub struct CodeRoot;
    impl CodeRoot {
        pub unsafe fn acquire(slot: *const hew_cabi::code::HewCodeLeaseSlot) -> Self {
            assert!(slot.is_null(), "channel: dynamic code leases outside model");
            Self
        }
    }
}
pub mod channel_common {
    pub fn abort_elem_witness(_: &str, _: &str) -> ! {
        panic!("channel: malformed element witness");
    }
    pub(crate) use crate::channel_envelope::drop_elem_envelope;
}
pub mod release_walker {
    use super::*;
    use hew_cabi::value::HewValueLayout;
    pub struct HewReleaseCursor { bytes: [usize; 3], layout: HewValueLayout }
    impl HewReleaseCursor {
        pub unsafe fn detached(value: *const c_void, layout: HewValueLayout) -> *mut Self {
            assert!(layout.size == 3 * size_of::<usize>() && layout.align == align_of::<usize>(),
                    "channel: unmodeled release layout");
            assert!(layout.release_start.is_none(), "channel: cooperative release outside model");
            let mut cursor = Box::new(Self { bytes: [0;3], layout });
            std::ptr::copy_nonoverlapping(value.cast::<u8>(), cursor.bytes.as_mut_ptr().cast(), layout.size);
            Box::into_raw(cursor)
        }
    }
    pub unsafe fn hew_release_sync(cursor: *mut HewReleaseCursor) {
        if !cursor.is_null() {
            let mut cursor = Box::from_raw(cursor);
            let drop = cursor.layout.drop_fn.expect("channel: missing release monitor");
            drop(cursor.bytes.as_mut_ptr().cast());
        }
    }
}
