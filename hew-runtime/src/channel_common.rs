use std::ffi::c_char;
use std::ptr;

pub(crate) fn bytes_to_cstr(item: &[u8]) -> *mut c_char {
    let len = item.len();
    // SAFETY: libc::malloc returns a valid aligned pointer or null.
    let buf = unsafe { libc::malloc(len + 1) };
    if buf.is_null() {
        return ptr::null_mut();
    }
    if len > 0 {
        // SAFETY: `buf` has `len + 1` bytes and `item` has `len` readable bytes.
        unsafe { ptr::copy_nonoverlapping(item.as_ptr(), buf.cast::<u8>(), len) };
    }
    // SAFETY: writing the NUL terminator at offset `len` stays in-bounds.
    unsafe { *buf.cast::<u8>().add(len) = 0 };
    buf.cast::<c_char>()
}

pub(crate) unsafe fn free_channel_pair<P, S, R>(
    pair: *mut P,
    split: impl FnOnce(&mut P) -> (&mut *mut S, &mut *mut R),
) {
    if pair.is_null() {
        return;
    }

    // SAFETY: caller guarantees `pair` came from Box::into_raw.
    let mut pair = unsafe { Box::from_raw(pair) };
    let (sender, receiver) = split(&mut pair);
    // SAFETY: `sender` points into the boxed pair and is valid for replacement.
    let sender = unsafe { ptr::replace(sender, ptr::null_mut()) };
    // SAFETY: `receiver` points into the boxed pair and is valid for replacement.
    let receiver = unsafe { ptr::replace(receiver, ptr::null_mut()) };
    drop(pair);

    if !sender.is_null() {
        // SAFETY: unextracted sender handles are still Box-owned here.
        unsafe { drop(Box::from_raw(sender)) };
    }
    if !receiver.is_null() {
        // SAFETY: unextracted receiver handles are still Box-owned here.
        unsafe { drop(Box::from_raw(receiver)) };
    }
}
