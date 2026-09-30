//! The serialization event ABI: C entry points over `hew_codec::{Sink, Source}`
//! that compiled encode and decode walks call.
//!
//! An encode walk opens a sink with `hew_ser_new`, emits one value's events and
//! takes the document with `hew_ser_finish_bytes` or `hew_ser_finish_string`.
//! A decode walk opens a source with `hew_de_new`, which always returns a
//! handle, and pulls. The first failure is latched: every later pull returns
//! a zero value and leaves the handle failed, so the walk checks
//! `hew_de_failed` where it must stop, releases what it built, and reads the
//! error text with `hew_de_error`. The runtime never frees anything the walk
//! owns.
//!
//! Tables are `hew_codec::Table` globals the compiler emits; handles are
//! exclusively owned by the walk that opened them.

use core::ffi::c_void;
use hew_cabi::string::{string_as_str, string_from_str, HewString};
use hew_codec::{DecodeError, Format, Sink, Source, Table};

use crate::bytes::{hew_bytes_new, BytesTriple};

fn format(code: i32) -> Format {
    match code {
        0 => Format::Cbor,
        1 => Format::Json,
        2 => Format::Yaml,
        3 => Format::Toml,
        4 => Format::Msgpack,
        _ => panic!("hew codec: unknown format {code}"),
    }
}

fn owned_bytes(bytes: &[u8]) -> Option<BytesTriple> {
    let len = u32::try_from(bytes.len()).ok()?;
    let ptr = if len == 0 {
        core::ptr::null_mut()
    } else {
        let ptr = hew_bytes_new(len);
        // SAFETY: the managed buffer has room for exactly `len` bytes.
        unsafe { core::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, bytes.len()) };
        ptr
    };
    Some(BytesTriple {
        ptr,
        offset: 0,
        len,
    })
}

/// # Safety
/// `bytes` is a live managed byte carrier.
unsafe fn bytes_of<'a>(bytes: *const BytesTriple) -> &'a [u8] {
    // SAFETY: the caller supplies a live carrier.
    let value = unsafe { &*bytes };
    if value.len == 0 || value.ptr.is_null() {
        &[]
    } else {
        // SAFETY: a live carrier's active range is readable.
        unsafe {
            core::slice::from_raw_parts(value.ptr.add(value.offset as usize), value.len as usize)
        }
    }
}

// ── Encode ──────────────────────────────────────────────────────────────────

/// # Safety
/// `sink` is a live handle from `hew_ser_new`.
unsafe fn sink<'a>(sink: *mut c_void) -> &'a mut Sink<'static> {
    // SAFETY: the caller supplies the live, exclusively owned handle.
    unsafe { &mut *sink.cast::<Sink<'static>>() }
}

/// # Safety
/// `table` points at a compiler-emitted table that lives for the program.
unsafe fn table(table: *const Table<'static>) -> Table<'static> {
    // SAFETY: the caller supplies a static table.
    unsafe { *table }
}

#[no_mangle]
pub extern "C" fn hew_ser_new(format_code: i32) -> *mut c_void {
    Box::into_raw(Box::new(Sink::new(format(format_code)))).cast()
}

/// # Safety
/// `s` is a live sink.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_null(s: *mut c_void) {
    // SAFETY: per contract.
    unsafe { sink(s) }.null();
}

/// # Safety
/// `s` is a live sink.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_bool(s: *mut c_void, value: i8) {
    // SAFETY: per contract.
    unsafe { sink(s) }.bool(value != 0);
}

/// # Safety
/// `s` is a live sink.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_i64(s: *mut c_void, value: i64) {
    // SAFETY: per contract.
    unsafe { sink(s) }.i64(value);
}

/// # Safety
/// `s` is a live sink.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_u64(s: *mut c_void, value: u64) {
    // SAFETY: per contract.
    unsafe { sink(s) }.u64(value);
}

/// # Safety
/// `s` is a live sink.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_f64(s: *mut c_void, value: f64) {
    // SAFETY: per contract.
    unsafe { sink(s) }.f64(value);
}

/// A `char` is a string of one scalar.
///
/// # Safety
/// `s` is a live sink; `value` is a Unicode scalar value.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_char(s: *mut c_void, value: u32) {
    let ch = char::from_u32(value).expect("hew codec: a char is a Unicode scalar");
    // SAFETY: per contract.
    unsafe { sink(s) }.str(ch.encode_utf8(&mut [0; 4]));
}

/// # Safety
/// `s` is a live sink; `value` is a borrowed managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_str(s: *mut c_void, value: *const HewString) {
    // SAFETY: per contract.
    unsafe { sink(s).str(string_as_str(value)) };
}

/// # Safety
/// `s` is a live sink; `value` is a live managed byte carrier.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_bytes(s: *mut c_void, value: *const BytesTriple) {
    // SAFETY: per contract.
    unsafe { sink(s).bytes(bytes_of(value)) };
}

/// # Safety
/// `s` is a live sink.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_seq_begin(s: *mut c_void, len: i64) {
    // SAFETY: per contract.
    unsafe { sink(s) }.seq_begin(usize::try_from(len).unwrap_or(0));
}

/// # Safety
/// `s` is a live sink.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_set_begin(s: *mut c_void, len: i64) {
    // SAFETY: per contract.
    unsafe { sink(s) }.set_begin(usize::try_from(len).unwrap_or(0));
}

/// # Safety
/// `s` is a live sink with an open sequence or set.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_seq_end(s: *mut c_void) {
    // SAFETY: per contract.
    unsafe { sink(s) }.seq_end();
}

/// # Safety
/// `s` is a live sink.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_map_begin(s: *mut c_void, len: i64, string_keys: i8) {
    // SAFETY: per contract.
    unsafe { sink(s) }.map_begin(usize::try_from(len).unwrap_or(0), string_keys != 0);
}

/// # Safety
/// `s` is a live sink with an open map.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_map_end(s: *mut c_void) {
    // SAFETY: per contract.
    unsafe { sink(s) }.map_end();
}

/// # Safety
/// `s` is a live sink; `t` is a static table.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_record_begin(s: *mut c_void, t: *const Table<'static>) {
    // SAFETY: per contract.
    unsafe { sink(s).record_begin(table(t)) };
}

/// # Safety
/// `s` is a live sink with an open record.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_field(s: *mut c_void, index: u32) {
    // SAFETY: per contract.
    unsafe { sink(s) }.field(index as usize);
}

/// # Safety
/// `s` is a live sink with an open record.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_record_end(s: *mut c_void) {
    // SAFETY: per contract.
    unsafe { sink(s) }.record_end();
}

/// # Safety
/// `s` is a live sink; `t` is a static table.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_variant(s: *mut c_void, t: *const Table<'static>, index: u32) {
    // SAFETY: per contract.
    unsafe { sink(s).variant(table(t), index as usize) };
}

/// # Safety
/// `s` is a live sink with an open variant.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_variant_end(s: *mut c_void) {
    // SAFETY: per contract.
    unsafe { sink(s) }.variant_end();
}

/// # Safety
/// `s` is a live sink holding one complete value.
unsafe fn finish(s: *mut c_void) -> Vec<u8> {
    // SAFETY: the caller transfers the handle here.
    unsafe { Box::from_raw(s.cast::<Sink<'static>>()) }.finish()
}

/// Consume the sink and write its binary document to `out`.
///
/// # Safety
/// `s` is a live sink holding one complete value, transferred here; `out` is
/// writable `BytesTriple` storage.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_finish_bytes(s: *mut c_void, out: *mut BytesTriple) {
    // SAFETY: per contract.
    let document = unsafe { finish(s) };
    let value = owned_bytes(&document).expect("hew codec: document exceeds the bytes length limit");
    // SAFETY: `out` is writable and receives the sole owner.
    unsafe { out.write(value) };
}

/// Consume the sink and return its text document.
///
/// # Safety
/// `s` is a live text-format sink holding one complete value, transferred here.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_finish_string(s: *mut c_void) -> *mut HewString {
    // SAFETY: per contract.
    let document = unsafe { finish(s) };
    let text = String::from_utf8(document).expect("hew codec: text formats write UTF-8");
    string_from_str(&text)
}

// ── Decode ──────────────────────────────────────────────────────────────────

/// A decode handle: the source while it is healthy, the first error after.
struct Reader {
    source: Option<Source<'static>>,
    error: Option<DecodeError>,
}

impl Reader {
    /// Run one pull; a failure latches and every later pull yields `default`.
    fn pull<T>(
        &mut self,
        default: T,
        read: impl FnOnce(&mut Source<'static>) -> Result<T, DecodeError>,
    ) -> T {
        let Some(source) = self.source.as_mut() else {
            return default;
        };
        match read(source) {
            Ok(value) => value,
            Err(error) => {
                self.fail(error);
                default
            }
        }
    }

    fn fail(&mut self, error: DecodeError) {
        self.source = None;
        self.error.get_or_insert(error);
    }
}

/// # Safety
/// `r` is a live handle from `hew_de_new`.
unsafe fn reader<'a>(r: *mut c_void) -> &'a mut Reader {
    // SAFETY: the caller supplies the live, exclusively owned handle.
    unsafe { &mut *r.cast::<Reader>() }
}

/// Parse a document. Always returns a handle; a malformed document leaves
/// it failed.
///
/// # Safety
/// For a binary format `input` points at a live `BytesTriple`; for a text
/// format it points at a slot holding a borrowed managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_de_new(format_code: i32, input: *const c_void) -> *mut c_void {
    let format = format(format_code);
    let text;
    let bytes = if format.is_text() {
        // SAFETY: text formats pass a managed string slot.
        text = unsafe { string_as_str(*input.cast::<*const HewString>()) };
        text.as_bytes()
    } else {
        // SAFETY: binary formats pass a byte carrier.
        unsafe { bytes_of(input.cast()) }
    };
    let reader = match Source::new(format, bytes) {
        Ok(source) => Reader {
            source: Some(source),
            error: None,
        },
        Err(error) => Reader {
            source: None,
            error: Some(error),
        },
    };
    Box::into_raw(Box::new(reader)).cast()
}

/// # Safety
/// `r` is a live reader, transferred here.
#[no_mangle]
pub unsafe extern "C" fn hew_de_free(r: *mut c_void) {
    // SAFETY: per contract.
    drop(unsafe { Box::from_raw(r.cast::<Reader>()) });
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_failed(r: *mut c_void) -> i32 {
    // SAFETY: per contract.
    i32::from(unsafe { reader(r) }.error.is_some())
}

/// The latched error as text, or null when the reader is healthy.
///
/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_error(r: *mut c_void) -> *mut HewString {
    // SAFETY: per contract.
    unsafe { reader(r) }
        .error
        .as_ref()
        .map_or(core::ptr::null_mut(), |error| {
            string_from_str(&error.to_string())
        })
}

/// Latch a duplicate the walk found by the value's own equality.
///
/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_duplicate(r: *mut c_void) {
    // SAFETY: per contract.
    let reader = unsafe { reader(r) };
    if let Some(error) = reader.source.as_ref().map(Source::duplicate) {
        reader.fail(error);
    }
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_is_null(r: *mut c_void) -> i32 {
    // SAFETY: per contract.
    i32::from(unsafe { reader(r) }.pull(false, Source::is_null))
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_bool(r: *mut c_void) -> i8 {
    // SAFETY: per contract.
    i8::from(unsafe { reader(r) }.pull(false, Source::read_bool))
}

/// An integer that fits `bits` bits, signed or unsigned.
///
/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_int(r: *mut c_void, bits: u32, signed: i8) -> i64 {
    // SAFETY: per contract.
    let reader = unsafe { reader(r) };
    if signed != 0 {
        let max = i64::MAX >> (64 - bits);
        reader.pull(0, |source| source.read_i64(-max - 1, max))
    } else {
        let max = u64::MAX >> (64 - bits);
        #[expect(
            clippy::cast_possible_wrap,
            reason = "the walk reinterprets the bits at its own unsigned width"
        )]
        let value = reader.pull(0, |source| source.read_u64(max)) as i64;
        value
    }
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_f64(r: *mut c_void) -> f64 {
    // SAFETY: per contract.
    unsafe { reader(r) }.pull(0.0, Source::read_f64)
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_char(r: *mut c_void) -> u32 {
    // SAFETY: per contract.
    u32::from(unsafe { reader(r) }.pull('\0', Source::read_char))
}

/// An owned managed string, or null after a failure.
///
/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_str(r: *mut c_void) -> *mut HewString {
    // SAFETY: per contract.
    unsafe { reader(r) }
        .pull(None, |source| source.read_str().map(Some))
        .map_or(core::ptr::null_mut(), |text| string_from_str(&text))
}

/// Write owned bytes to `out`; empty after a failure.
///
/// # Safety
/// `r` is a live reader; `out` is writable `BytesTriple` storage.
#[no_mangle]
pub unsafe extern "C" fn hew_de_bytes(r: *mut c_void, out: *mut BytesTriple) {
    // SAFETY: per contract.
    let reader = unsafe { reader(r) };
    let bytes = reader.pull(Vec::new(), Source::read_bytes);
    let value = owned_bytes(&bytes).unwrap_or_else(|| {
        reader.fail(DecodeError::Invalid {
            path: String::new(),
            reason: "bytes value exceeds the length limit".into(),
        });
        BytesTriple {
            ptr: core::ptr::null_mut(),
            offset: 0,
            len: 0,
        }
    });
    // SAFETY: `out` is writable and receives the sole owner.
    unsafe { out.write(value) };
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_seq_begin(r: *mut c_void) {
    // SAFETY: per contract.
    unsafe { reader(r) }.pull((), |source| source.seq_begin().map(drop));
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_set_begin(r: *mut c_void) {
    // SAFETY: per contract.
    unsafe { reader(r) }.pull((), |source| source.set_begin().map(drop));
}

/// A sequence of exactly `len` values.
///
/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_tuple_begin(r: *mut c_void, len: u32) {
    // SAFETY: per contract.
    unsafe { reader(r) }.pull((), |source| source.tuple_begin(len as usize));
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_seq_next(r: *mut c_void) -> i32 {
    // SAFETY: per contract.
    i32::from(unsafe { reader(r) }.pull(false, Source::seq_next))
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_map_begin(r: *mut c_void, string_keys: i8) {
    // SAFETY: per contract.
    unsafe { reader(r) }.pull((), |source| source.map_begin(string_keys != 0).map(drop));
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_map_next(r: *mut c_void) -> i32 {
    // SAFETY: per contract.
    i32::from(unsafe { reader(r) }.pull(false, Source::map_next))
}

/// # Safety
/// `r` is a live reader; `t` is a static table.
#[no_mangle]
pub unsafe extern "C" fn hew_de_record_begin(r: *mut c_void, t: *const Table<'static>) {
    // SAFETY: per contract.
    let t = unsafe { table(t) };
    // SAFETY: per contract.
    unsafe { reader(r) }.pull((), |source| source.record_begin(t));
}

/// Stage the next member; its index, or -1 at the end or after a failure.
///
/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_record_next(r: *mut c_void) -> i32 {
    // SAFETY: per contract.
    unsafe { reader(r) }.pull(-1, |source| {
        source
            .record_next()
            .map(|index| index.map_or(-1, |index| i32::try_from(index).unwrap_or(-1)))
    })
}

/// The variant's index, or -1 after a failure.
///
/// # Safety
/// `r` is a live reader; `t` is a static table.
#[no_mangle]
pub unsafe extern "C" fn hew_de_variant(r: *mut c_void, t: *const Table<'static>) -> i32 {
    // SAFETY: per contract.
    let t = unsafe { table(t) };
    // SAFETY: per contract.
    unsafe { reader(r) }.pull(-1, |source| {
        source
            .variant(t)
            .map(|index| i32::try_from(index).unwrap_or(-1))
    })
}

/// # Safety
/// `r` is a live reader.
#[no_mangle]
pub unsafe extern "C" fn hew_de_variant_end(r: *mut c_void) {
    // SAFETY: per contract.
    unsafe { reader(r) }.pull((), Source::variant_end);
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_codec::Member;

    const JSON: i32 = 1;
    const CBOR: i32 = 0;

    static POINT_MEMBERS: [Member<'static>; 2] = [
        Member::new("x", 1, 0),
        Member::new("note", 2, Member::ACCEPT_ABSENT | Member::OMIT_NULL),
    ];
    static POINT: Table<'static> = Table::new(&POINT_MEMBERS, true);

    fn text(s: *mut HewString) -> String {
        // SAFETY: the test owns the fresh string until it drops it.
        let out = unsafe { string_as_str(s) }.to_owned();
        // SAFETY: fresh managed string.
        unsafe { crate::string::hew_string_drop(s) };
        out
    }

    /// `{x: 7}` with its optional note absent, through the extern calls.
    fn encode(format_code: i32) -> *mut c_void {
        let s = hew_ser_new(format_code);
        // SAFETY: the sink and table are live for the whole walk.
        unsafe {
            hew_ser_record_begin(s, &raw const POINT);
            hew_ser_field(s, 0);
            hew_ser_i64(s, 7);
            hew_ser_field(s, 1);
            hew_ser_null(s);
            hew_ser_record_end(s);
        }
        s
    }

    #[test]
    fn a_walk_encodes_through_static_tables() {
        // SAFETY: the sink is complete and transferred.
        assert_eq!(
            text(unsafe { hew_ser_finish_string(encode(JSON)) }),
            r#"{"x":7}"#
        );
        let mut out = BytesTriple {
            ptr: core::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
        // SAFETY: the sink is complete and transferred; `out` is writable.
        unsafe { hew_ser_finish_bytes(encode(CBOR), &raw mut out) };
        // SAFETY: `out` holds the fresh document.
        assert_eq!(unsafe { bytes_of(&raw const out) }, [0xa1, 0x01, 0x07]);
        // SAFETY: the test owns the fresh buffer.
        unsafe { crate::bytes::hew_bytes_drop(out.ptr) };
    }

    fn decode(document: &str) -> (i64, bool, Option<String>) {
        let input = string_from_str(document);
        // SAFETY: the slot holds a live managed string for the whole walk.
        unsafe {
            let r = hew_de_new(JSON, (&raw const input).cast());
            hew_de_record_begin(r, &raw const POINT);
            let mut x = 0;
            let mut note_absent = false;
            loop {
                match hew_de_record_next(r) {
                    0 => x = hew_de_int(r, 8, 1),
                    1 => note_absent = hew_de_is_null(r) != 0,
                    _ => break,
                }
            }
            let error = (hew_de_failed(r) != 0).then(|| text(hew_de_error(r)));
            hew_de_free(r);
            crate::string::hew_string_drop(input);
            (x, note_absent, error)
        }
    }

    #[test]
    fn a_walk_decodes_and_a_failure_latches_its_path() {
        assert_eq!(decode(r#"{"x":7}"#), (7, true, None));
        assert_eq!(
            decode(r#"{"x":300}"#),
            (0, false, Some("Range: .x: 300 does not fit i8".into()))
        );
        assert_eq!(
            decode(r#"{"x":"#),
            (
                0,
                false,
                Some("Syntax: line 1, column 6: unexpected end of input".into())
            )
        );
    }
}
