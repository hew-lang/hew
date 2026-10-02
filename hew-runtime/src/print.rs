//! Hew runtime: `print` module.
//!
//! Compiled Hew programs call a generic C ABI print entrypoint with a type tag
//! plus payload bits. The value is rendered here and its bytes join the one
//! ordered output queue ([`crate::output`]).
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use hew_cabi::string::{string_as_bytes, HewString};

#[repr(u8)]
enum PrintKind {
    I32 = 0,
    I64 = 1,
    F64 = 2,
    Bool = 3,
    Str = 4,
    U32 = 5,
    U64 = 6,
    U8 = 7,
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
            7 => Some(Self::U8),
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

/// Canonicalize NaN for user-visible formatting.
///
/// IEEE-754 leaves the sign and payload of a NaN unspecified for arithmetic
/// such as `0.0 / 0.0`; LLVM optimization and target hardware may therefore
/// produce different bits for the same Hew expression. Hew renders every NaN
/// as the unsigned spelling `nan`, independent of those non-semantic bits.
pub(crate) fn canonical_f64_for_render(value: f64) -> f64 {
    if value.is_nan() {
        f64::NAN
    } else {
        value
    }
}

/// C `%g` rendering, kept so float output is byte-for-byte what it was.
fn render_f64(x: f64) -> Vec<u8> {
    let mut buffer = [0_u8; 64];
    // SAFETY: the buffer is writable for its length and the format is a valid
    // NUL-terminated literal consuming one double.
    let len = unsafe {
        libc::snprintf(
            buffer.as_mut_ptr().cast(),
            buffer.len(),
            c"%g".as_ptr(),
            canonical_f64_for_render(x),
        )
    };
    let len = usize::try_from(len).unwrap_or(0).min(buffer.len() - 1);
    buffer[..len].to_vec()
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
    let mut text = match kind {
        PrintKind::I32 => decode_low_i32(bits).to_string().into_bytes(),
        PrintKind::I64 => decode_i64(bits).to_string().into_bytes(),
        // The compiler stores u8 zero-extended in the u64 bits field.
        PrintKind::U8 => (bits & 0xFF).to_string().into_bytes(),
        PrintKind::F64 => render_f64(f64::from_bits(bits)),
        PrintKind::Bool => if bits != 0 { "true" } else { "false" }.into(),
        PrintKind::Str => {
            let Ok(ptr_bits) = usize::try_from(bits) else {
                std::process::abort();
            };
            // SAFETY: the compiler supplies a live managed string handle; null
            // is empty.
            unsafe { string_as_bytes(ptr_bits as *const HewString) }.to_vec()
        }
        PrintKind::U32 => decode_low_u32(bits).to_string().into_bytes(),
        PrintKind::U64 => bits.to_string().into_bytes(),
    };
    if newline {
        text.push(b'\n');
    }
    crate::output::write(crate::output::Stream::Out, &text);
}

#[cfg(test)]
mod tests {
    use super::{canonical_f64_for_render, render_f64};

    #[test]
    fn f64_rendering_matches_c_general_format() {
        for (value, text) in [
            (42.5, "42.5"),
            (1e21, "1e+21"),
            (0.1, "0.1"),
            (-0.0, "-0"),
            (f64::NAN, "nan"),
            (f64::INFINITY, "inf"),
        ] {
            assert_eq!(render_f64(value), text.as_bytes());
        }
    }

    #[test]
    fn f64_rendering_canonicalizes_nan_sign_and_payload() {
        let negative_payload_nan = f64::from_bits(0xfff8_0000_0000_0042);
        assert_eq!(
            canonical_f64_for_render(negative_payload_nan).to_bits(),
            f64::NAN.to_bits()
        );
    }

    #[test]
    fn f64_rendering_preserves_non_nan_bits() {
        for value in [-0.0, 0.0, f64::INFINITY, f64::NEG_INFINITY, 42.5] {
            assert_eq!(canonical_f64_for_render(value).to_bits(), value.to_bits());
        }
    }
}
