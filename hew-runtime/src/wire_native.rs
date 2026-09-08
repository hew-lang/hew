//! Managed-value adapters for compiler-generated wire codec callbacks.
//!
//! CBOR cursor validation and text transcoding stay in their existing engines.
//! These entry points only adapt managed strings/bytes and own temporary buffers.

use core::ffi::{c_char, c_void};
use hew_cabi::string::{string_as_str, string_from_str, HewString};

use crate::bytes::{hew_bytes_new, BytesTriple};
use crate::cbor_serial;

struct RawBytes(*mut u8);
impl Drop for RawBytes {
    fn drop(&mut self) {
        // SAFETY: this guard owns a single allocation from the CBOR engine.
        unsafe { libc::free(self.0.cast()) };
    }
}

fn owned_bytes(bytes: &[u8]) -> Result<BytesTriple, String> {
    let len = u32::try_from(bytes.len())
        .map_err(|_| "wire byte value exceeds the bytes length limit".to_string())?;
    if len == 0 {
        return Ok(BytesTriple {
            ptr: core::ptr::null_mut(),
            offset: 0,
            len: 0,
        });
    }
    let ptr = hew_bytes_new(len);
    // SAFETY: the managed buffer has room for exactly len source bytes.
    unsafe { core::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, bytes.len()) };
    Ok(BytesTriple {
        ptr,
        offset: 0,
        len,
    })
}

/// Serialize complete managed UTF-8, including NUL.
///
/// # Safety
/// `writer` is live; `value` is a borrowed managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_string_hew(writer: *mut c_void, value: *const HewString) {
    // SAFETY: both handles remain borrowed throughout the writer operation.
    unsafe { cbor_serial::ser_text(writer, string_as_str(value)) };
}

/// Decode complete UTF-8 into an owned managed string. Cursor failure remains
/// latched so the generated callback can reject before publishing the value.
///
/// # Safety
/// `reader` is a live CBOR reader.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_string_hew(reader: *mut c_void) -> *mut HewString {
    // SAFETY: the caller supplies the live reader.
    unsafe { cbor_serial::de_text(reader) }
        .map_or(core::ptr::null_mut(), |text| string_from_str(&text))
}

/// Decode a byte string into owned managed storage.
///
/// # Safety
/// `reader` is live; `out` points to uninitialized `BytesTriple` storage.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_bytes_hew(reader: *mut c_void, out: *mut BytesTriple) {
    let mut len = 0;
    // SAFETY: reader is live and the local length is writable.
    let raw = RawBytes(unsafe { cbor_serial::hew_cbor_de_bytes(reader, &raw mut len) });
    let bytes = if len == 0 {
        &[]
    } else {
        // SAFETY: CBOR returns an allocation containing len initialized bytes.
        unsafe { core::slice::from_raw_parts(raw.0, len as usize) }
    };
    let value = owned_bytes(bytes).unwrap_or_else(|_| {
        // SAFETY: the reader remains live; failure never publishes a value.
        unsafe { cbor_serial::hew_cbor_de_fail(reader) };
        BytesTriple {
            ptr: core::ptr::null_mut(),
            offset: 0,
            len: 0,
        }
    });
    // SAFETY: caller supplies uninitialized storage and receives its sole owner.
    unsafe { out.write(value) };
}

/// Finish a generated encode walk into bytes (`format == -1`) or text (0 JSON,
/// 1 YAML). Consume the writer on every path. Only success initializes `out`.
///
/// # Safety
/// `writer` is a live writer transferred here; `descriptor` is a UTF-8 C string;
/// `out` is aligned writable `BytesTriple` or managed-string-pointer storage as
/// selected by `format`. `error` is writable managed-string-pointer storage.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_finish(
    writer: *mut c_void,
    format: i32,
    descriptor: *const c_char,
    out: *mut c_void,
    error: *mut *mut HewString,
) -> i32 {
    let outcome = std::panic::catch_unwind(|| -> Result<(), String> {
        let mut len = 0;
        // SAFETY: the writer is transferred and the local length is writable.
        let raw = RawBytes(unsafe { cbor_serial::hew_cbor_ser_finish(writer, &raw mut len) });
        if raw.0.is_null() {
            return Err("wire encoding failed".into());
        }
        // SAFETY: the CBOR allocation contains len initialized bytes.
        let bytes = unsafe { core::slice::from_raw_parts(raw.0, len) };
        if format == -1 {
            let value = owned_bytes(bytes)?;
            // SAFETY: binary format selects writable bytes storage.
            unsafe { out.cast::<BytesTriple>().write(value) };
        } else {
            // SAFETY: descriptor is a UTF-8 C string per the ABI contract.
            let descriptor = unsafe { core::ffi::CStr::from_ptr(descriptor) }
                .to_str()
                .map_err(|_| "internal: malformed wire descriptor".to_string())?;
            let text = crate::wire_text::cbor_to_text(bytes, descriptor, format)
                .ok_or_else(|| "wire text encoding failed".to_string())?;
            // SAFETY: text format selects writable managed string storage.
            unsafe { out.cast::<*mut HewString>().write(string_from_str(&text)) };
        }
        Ok(())
    });
    // SAFETY: error is writable and receives the sole error-string owner on failure.
    unsafe { finish_status(outcome, error) }
}

/// Prepare a generated decode walk from borrowed bytes or managed text. Only
/// success initializes `reader_out`; the generated walk then owns that reader.
///
/// # Safety
/// `input` is a live borrowed `BytesTriple` (`format == -1`) or managed string slot
/// (JSON 0, YAML 1). `descriptor` is a UTF-8 C string. Both out pointers are valid.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_decode_begin(
    input: *const c_void,
    format: i32,
    descriptor: *const c_char,
    reader_out: *mut *mut c_void,
    error: *mut *mut HewString,
) -> i32 {
    let outcome = std::panic::catch_unwind(|| -> Result<(), String> {
        let encoded;
        let bytes = if format == -1 {
            // SAFETY: binary format selects a valid borrowed byte carrier.
            let value = unsafe { &*input.cast::<BytesTriple>() };
            if value.len == 0 {
                &[][..]
            } else {
                // SAFETY: the live managed byte carrier establishes readable bounds.
                unsafe {
                    core::slice::from_raw_parts(
                        value.ptr.add(value.offset as usize),
                        value.len as usize,
                    )
                }
            }
        } else {
            // SAFETY: text format selects a valid borrowed managed string slot.
            let text = unsafe { string_as_str(*input.cast::<*const HewString>()) };
            // SAFETY: descriptor is a valid UTF-8 C string per the ABI contract.
            let descriptor = unsafe { core::ffi::CStr::from_ptr(descriptor) }
                .to_str()
                .map_err(|_| "internal: malformed wire descriptor".to_string())?;
            encoded = crate::wire_text::text_to_cbor(text, descriptor, format)?;
            &encoded
        };
        // SAFETY: the decoder copies the complete input into its owned value tree.
        let reader = unsafe { cbor_serial::hew_cbor_de_new(bytes.as_ptr(), bytes.len()) };
        // SAFETY: the decoder returned a live handle (including its failure state).
        if unsafe { cbor_serial::hew_cbor_de_failed(reader) } != 0 {
            // SAFETY: no typed output exists and this call owns the reader.
            unsafe { cbor_serial::hew_cbor_de_free(reader) };
            return Err("invalid CBOR wire body".into());
        }
        // SAFETY: the caller receives sole ownership of the live reader.
        unsafe { reader_out.write(reader) };
        Ok(())
    });
    // SAFETY: error is writable and receives the sole error-string owner on failure.
    unsafe { finish_status(outcome, error) }
}

unsafe fn finish_status(
    outcome: std::thread::Result<Result<(), String>>,
    error: *mut *mut HewString,
) -> i32 {
    let message = match outcome {
        Ok(Ok(())) => return 0,
        Ok(Err(message)) => message,
        Err(payload) => {
            crate::util::quarantine_panic_payload(payload);
            "internal: wire codec panicked".to_string()
        }
    };
    // SAFETY: caller supplies the writable error slot and takes its sole owner.
    unsafe { error.write(string_from_str(&message)) };
    1
}
