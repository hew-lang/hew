//! Hew runtime: `print` module.
//!
//! Compiled Hew programs call a generic C ABI print entrypoint with a type tag
//! plus payload bits. The runtime dispatches to the correct `libc::printf`
//! format while preserving Hew's `print`/`println` behavior.

use std::os::raw::c_char;

#[repr(u8)]
enum PrintKind {
    I32 = 0,
    I64 = 1,
    F64 = 2,
    Bool = 3,
    Str = 4,
    U32 = 5,
    U64 = 6,
}

impl PrintKind {
    fn from_abi(kind: u8) -> Option<Self> {
        match kind {
            0 => Some(Self::I32),
            1 => Some(Self::I64),
            2 => Some(Self::F64),
            3 => Some(Self::Bool),
            4 => Some(Self::Str),
            5 => Some(Self::U32),
            6 => Some(Self::U64),
            _ => None,
        }
    }
}

fn decode_low_u32(bits: u64) -> u32 {
    u32::try_from(bits & u64::from(u32::MAX)).expect("masked to 32 bits")
}

fn decode_low_i32(bits: u64) -> i32 {
    decode_low_u32(bits).cast_signed()
}

fn decode_i64(bits: u64) -> i64 {
    i64::from_ne_bytes(bits.to_ne_bytes())
}

unsafe fn print_i32(x: i32, newline: bool) {
    let fmt = if newline { c"%d\n" } else { c"%d" };
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain i32.
    unsafe { libc::printf(fmt.as_ptr(), x) };
}

unsafe fn print_i64(x: i64, newline: bool) {
    let fmt = if newline { c"%lld\n" } else { c"%lld" };
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain i64.
    // Use %lld (long long) because `long` is 32-bit on wasm32.
    unsafe { libc::printf(fmt.as_ptr(), x) };
}

unsafe fn print_f64(x: f64, newline: bool) {
    let fmt = if newline { c"%g\n" } else { c"%g" };
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain f64.
    unsafe { libc::printf(fmt.as_ptr(), x) };
}

unsafe fn print_bool(x: bool, newline: bool) {
    let s = match (x, newline) {
        (true, true) => c"true\n",
        (true, false) => c"true",
        (false, true) => c"false\n",
        (false, false) => c"false",
    };
    // SAFETY: Format string and s are valid NUL-terminated C literals.
    unsafe { libc::printf(c"%s".as_ptr(), s.as_ptr()) };
}

unsafe fn print_str(bits: u64, newline: bool) {
    let Ok(ptr_bits) = usize::try_from(bits) else {
        std::process::abort();
    };
    let s = ptr_bits as *const c_char;
    if s.is_null() {
        if newline {
            // SAFETY: Format string is a valid NUL-terminated C literal.
            unsafe { libc::printf(c"\n".as_ptr()) };
        }
        return;
    }

    let fmt = if newline { c"%s\n" } else { c"%s" };
    // SAFETY: Caller guarantees s is a valid NUL-terminated C string.
    unsafe { libc::printf(fmt.as_ptr(), s) };
}

unsafe fn print_u32(x: u32, newline: bool) {
    let fmt = if newline { c"%u\n" } else { c"%u" };
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain u32.
    unsafe { libc::printf(fmt.as_ptr(), x) };
}

unsafe fn print_u64(x: u64, newline: bool) {
    let fmt = if newline { c"%llu\n" } else { c"%llu" };
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain u64.
    // Use %llu (unsigned long long) because `unsigned long` is 32-bit on wasm32.
    unsafe { libc::printf(fmt.as_ptr(), x) };
}

/// Print a Hew value using the generic runtime print dispatcher.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. `kind` and `bits` must match
/// the payload encoding emitted by the compiler.
#[no_mangle]
pub unsafe extern "C" fn hew_print_value(kind: u8, bits: u64, newline: bool) {
    let Some(kind) = PrintKind::from_abi(kind) else {
        // Fail closed on an ABI mismatch rather than silently emitting the wrong
        // value format.
        std::process::abort();
    };

    // SAFETY: `kind` determines how the raw payload bits are decoded before
    // calling the matching typed print helper.
    unsafe {
        match kind {
            PrintKind::I32 => print_i32(decode_low_i32(bits), newline),
            PrintKind::I64 => print_i64(decode_i64(bits), newline),
            PrintKind::F64 => print_f64(f64::from_bits(bits), newline),
            PrintKind::Bool => print_bool(bits != 0, newline),
            PrintKind::Str => print_str(bits, newline),
            PrintKind::U32 => print_u32(decode_low_u32(bits), newline),
            PrintKind::U64 => print_u64(bits, newline),
        }
    }
}
