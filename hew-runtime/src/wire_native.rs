//! Managed-value adapters for compiler-generated wire codec callbacks.
//!
//! The generated walk speaks CBOR through the `cbor_serial` cursor. JSON and
//! YAML go through `hew-codec`: encode reads the walk's CBOR with a CBOR
//! [`Source`] and replays it into a text [`Sink`]; decode does the reverse
//! before the walk reads the CBOR. Both directions follow the per-type JSON
//! descriptor the compiler passes.
//!
//! WHY: the generated walk is CBOR-specific and knows each type's schema only
//! as that descriptor, so text has to cross through CBOR.
//! WHEN obsolete: when codegen emits the format-neutral event walk over
//! `hew-codec` tables (design-data-codegen §1.4) and deletes
//! `text_descriptor`.
//! WHAT the real fix is: that walk drives a `Sink`/`Source` of the requested
//! format directly; this bridge, the descriptor and `cbor_serial` go with it.

use core::ffi::{c_char, c_void};
use hew_cabi::string::{string_as_str, string_from_str, HewString};
use hew_codec::{DecodeError, Format, Member, Sink, Source, Table};

use crate::bytes::{hew_bytes_new, BytesTriple};
use crate::cbor_serial;

struct RawBytes(*mut u8);
impl Drop for RawBytes {
    fn drop(&mut self) {
        // SAFETY: this guard owns a single allocation from the CBOR engine.
        unsafe { crate::mem::buf_free(self.0.cast()) };
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
            let text = cbor_to_text(bytes, descriptor, format)
                .map_err(|e| format!("wire text encoding failed: {e}"))?;
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
            encoded = text_to_cbor(text, descriptor, format)?;
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

// ── JSON/YAML bridge over hew-codec ─────────────────────────────────────────

/// Text format selectors the generated walk passes (`WireTextFormat`).
const FORMAT_JSON: i32 = 0;
const FORMAT_YAML: i32 = 1;

fn text_format(format: i32) -> Result<Format, String> {
    match format {
        FORMAT_JSON => Ok(Format::Json),
        FORMAT_YAML => Ok(Format::Yaml),
        _ => Err("internal: unknown wire text format".into()),
    }
}

/// One node of the codegen descriptor, borrowing its keys from the parsed
/// descriptor document:
///
/// ```text
/// {"k":"i64"|"u64"|"f64"|"bool"|"str"|"bytes"}
/// {"k":"vec"|"set"|"opt","e":<node>}   {"k":"map","key":<node>,"value":<node>}
/// {"k":"struct","f":[{"t":<tag>,"n":"<key>","p":"required"|"optional","d":<node>}]}
/// {"k":"enum","v":[{"t":<tag>,"n":"<key>","p":[<node>...]}]}
/// ```
#[derive(Debug)]
enum Desc<'d> {
    Int,
    Uint,
    Float,
    Bool,
    Str,
    Bytes,
    Vec(Box<Self>),
    Set(Box<Self>),
    Opt(Box<Self>),
    Map(Box<Self>, Box<Self>),
    Struct(Vec<Member<'d>>, Vec<Self>),
    Enum(Vec<Member<'d>>, Vec<Vec<Self>>),
}

impl<'d> Desc<'d> {
    /// `None` for a malformed node: a compiler defect the bridge refuses.
    fn parse(node: &'d serde_json::Value, depth: usize) -> Option<Self> {
        if depth > hew_codec::MAX_DEPTH {
            return None;
        }
        let child = |key: &str| Self::parse(node.get(key)?, depth + 1).map(Box::new);
        let entry = |entry: &'d serde_json::Value| -> Option<(&'d str, u64)> {
            Some((entry.get("n")?.as_str()?, entry.get("t")?.as_u64()?))
        };
        Some(match node.get("k")?.as_str()? {
            "i64" => Self::Int,
            "u64" => Self::Uint,
            "f64" => Self::Float,
            "bool" => Self::Bool,
            "str" => Self::Str,
            "bytes" => Self::Bytes,
            "vec" => Self::Vec(child("e")?),
            "set" => Self::Set(child("e")?),
            "opt" => Self::Opt(child("e")?),
            "map" => Self::Map(child("key")?, child("value")?),
            "struct" => {
                let (mut members, mut fields) = (Vec::new(), Vec::new());
                for field in node.get("f")?.as_array()? {
                    let (key, tag) = entry(field)?;
                    let flags = match field.get("p")?.as_str()? {
                        "required" => 0,
                        "optional" => Member::ACCEPT_ABSENT | Member::OMIT_NULL,
                        _ => return None,
                    };
                    members.push(Member::new(key, tag, flags));
                    fields.push(Self::parse(field.get("d")?, depth + 1)?);
                }
                Self::Struct(members, fields)
            }
            "enum" => {
                let (mut members, mut payloads) = (Vec::new(), Vec::new());
                for variant in node.get("v")?.as_array()? {
                    let (key, tag) = entry(variant)?;
                    let payload = variant
                        .get("p")?
                        .as_array()?
                        .iter()
                        .map(|p| Self::parse(p, depth + 1))
                        .collect::<Option<Vec<_>>>()?;
                    let flags = if payload.is_empty() {
                        0
                    } else {
                        Member::PAYLOAD
                    };
                    members.push(Member::new(key, tag, flags));
                    payloads.push(payload);
                }
                Self::Enum(members, payloads)
            }
            _ => return None,
        })
    }
}

fn table<'t>(members: &'t [Member<'t>]) -> Table<'t> {
    Table::new(members, true)
}

/// Replay one value from `source` into `sink`, shaped by `desc`. A variant
/// payload is always a sequence, the shape the CBOR walk reads and writes.
fn transfer<'t>(
    desc: &'t Desc<'t>,
    source: &mut Source<'t>,
    sink: &mut Sink<'t>,
) -> Result<(), DecodeError> {
    match desc {
        Desc::Int => sink.i64(source.read_i64(i64::MIN, i64::MAX)?),
        Desc::Uint => sink.u64(source.read_u64(u64::MAX)?),
        Desc::Float => sink.f64(source.read_f64()?),
        Desc::Bool => sink.bool(source.read_bool()?),
        Desc::Str => sink.str(&source.read_str()?),
        Desc::Bytes => sink.bytes(&source.read_bytes()?),
        Desc::Opt(inner) => {
            if source.is_null()? {
                sink.null();
            } else {
                transfer(inner, source, sink)?;
            }
        }
        Desc::Vec(element) | Desc::Set(element) => {
            if matches!(desc, Desc::Set(_)) {
                sink.set_begin(source.set_begin()?);
            } else {
                sink.seq_begin(source.seq_begin()?);
            }
            while source.seq_next()? {
                transfer(element, source, sink)?;
            }
            sink.seq_end();
        }
        Desc::Map(key, value) => {
            let string_keys = matches!(**key, Desc::Str);
            sink.map_begin(source.map_begin(string_keys)?, string_keys);
            while source.map_next()? {
                transfer(key, source, sink)?;
                transfer(value, source, sink)?;
            }
            sink.map_end();
        }
        Desc::Struct(members, fields) => {
            source.record_begin(table(members))?;
            sink.record_begin(table(members));
            while let Some(index) = source.record_next()? {
                sink.field(index);
                transfer(&fields[index], source, sink)?;
            }
            sink.record_end();
        }
        Desc::Enum(members, payloads) => {
            let index = source.variant(table(members))?;
            sink.variant(table(members), index);
            let payload = &payloads[index];
            if !payload.is_empty() {
                source.tuple_begin(payload.len())?;
                sink.seq_begin(payload.len());
                for field in payload {
                    source.seq_next()?;
                    transfer(field, source, sink)?;
                }
                source.seq_next()?;
                sink.seq_end();
            }
            source.variant_end()?;
            sink.variant_end();
        }
    }
    Ok(())
}

fn parse_descriptor(descriptor: &str) -> Result<serde_json::Value, String> {
    serde_json::from_str(descriptor).map_err(|_| "internal: malformed wire descriptor".to_string())
}

fn transcode(input: &[u8], from: Format, to: Format, descriptor: &str) -> Result<Vec<u8>, String> {
    let document = parse_descriptor(descriptor)?;
    let desc = Desc::parse(&document, 0)
        .ok_or_else(|| "internal: malformed wire descriptor".to_string())?;
    let mut source = Source::new(from, input).map_err(|e| e.to_string())?;
    let mut sink = Sink::new(to);
    transfer(&desc, &mut source, &mut sink).map_err(|e| e.to_string())?;
    Ok(sink.finish())
}

/// The walk's CBOR as a complete JSON or YAML document.
fn cbor_to_text(bytes: &[u8], descriptor: &str, format: i32) -> Result<String, String> {
    let text = transcode(bytes, Format::Cbor, text_format(format)?, descriptor)?;
    String::from_utf8(text).map_err(|_| "internal: text output is not UTF-8".to_string())
}

/// Untrusted JSON or YAML as the CBOR the walk decodes.
fn text_to_cbor(text: &str, descriptor: &str, format: i32) -> Result<Vec<u8>, String> {
    transcode(
        text.as_bytes(),
        text_format(format)?,
        Format::Cbor,
        descriptor,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    const POINT: &str = r#"{"k":"struct","f":[
        {"t":2,"n":"y","p":"required","d":{"k":"i64"}},
        {"t":1,"n":"x","p":"required","d":{"k":"i64"}}]}"#;

    const PRESENCE: &str = r#"{"k":"struct","f":[
        {"t":1,"n":"required_value","p":"required","d":{"k":"i64"}},
        {"t":2,"n":"required_option","p":"required","d":{"k":"opt","e":{"k":"str"}}},
        {"t":3,"n":"optional_option","p":"optional","d":{"k":"opt","e":{"k":"str"}}}]}"#;

    const SHAPE: &str = r#"{"k":"enum","v":[
        {"t":1,"n":"Circle","p":[{"k":"f64"}]},
        {"t":2,"n":"Rect","p":[{"k":"i64"},{"k":"i64"}]},
        {"t":3,"n":"Empty","p":[]}]}"#;

    /// Decode through the legacy cursor, as the generated walk does, reading
    /// `keys` as i64 fields.
    fn walk_reads_struct(cbor: &[u8], keys: &[u64]) -> Option<Vec<i64>> {
        // SAFETY: the reader is created, used and freed within this function.
        unsafe {
            let reader = cbor_serial::hew_cbor_de_new(cbor.as_ptr(), cbor.len());
            cbor_serial::hew_cbor_de_enter_map(reader);
            let values = keys
                .iter()
                .map(|&key| {
                    cbor_serial::hew_cbor_de_select_key(reader, key);
                    cbor_serial::hew_cbor_de_i64(reader)
                })
                .collect();
            cbor_serial::hew_cbor_de_exit_map(reader);
            let failed = cbor_serial::hew_cbor_de_failed(reader) != 0;
            cbor_serial::hew_cbor_de_free(reader);
            (!failed).then_some(values)
        }
    }

    #[test]
    fn struct_text_is_keyed_by_name_and_cbor_by_tag() {
        let cbor = text_to_cbor(r#"{"x":3,"y":4}"#, POINT, FORMAT_JSON).unwrap();
        assert_eq!(cbor, [0xa2, 0x01, 0x03, 0x02, 0x04]);
        assert_eq!(walk_reads_struct(&cbor, &[1, 2]), Some(vec![3, 4]));
        // Fields are written in descriptor order.
        assert_eq!(
            cbor_to_text(&cbor, POINT, FORMAT_JSON).unwrap(),
            r#"{"y":4,"x":3}"#
        );
        assert_eq!(
            cbor_to_text(&cbor, POINT, FORMAT_YAML).unwrap(),
            "y: 4\nx: 3\n"
        );
        assert_eq!(
            text_to_cbor("y: 4\nx: 3\n", POINT, FORMAT_YAML).unwrap(),
            cbor
        );
    }

    #[test]
    fn presence_matrix_is_shared_by_json_and_yaml() {
        for (format, none, some) in [
            (
                FORMAT_JSON,
                r#"{"required_value":7,"required_option":null}"#,
                r#"{"required_value":7,"required_option":"a","optional_option":"b"}"#,
            ),
            (
                FORMAT_YAML,
                "required_value: 7\nrequired_option: null\n",
                "required_value: 7\nrequired_option: a\noptional_option: b\n",
            ),
        ] {
            // The absent optional key stays absent in CBOR; the required None is null.
            let cbor = text_to_cbor(none, PRESENCE, format).unwrap();
            assert_eq!(cbor, [0xa2, 0x01, 0x07, 0x02, 0xf6]);
            assert_eq!(cbor_to_text(&cbor, PRESENCE, format).unwrap(), none);
            let cbor = text_to_cbor(some, PRESENCE, format).unwrap();
            assert_eq!(cbor_to_text(&cbor, PRESENCE, format).unwrap(), some);
            // Explicit null is None for either presence.
            let explicit = if format == FORMAT_JSON {
                r#"{"required_value":7,"required_option":null,"optional_option":null}"#
            } else {
                "required_value: 7\nrequired_option: null\noptional_option: null\n"
            };
            assert_eq!(
                text_to_cbor(explicit, PRESENCE, format).unwrap(),
                [0xa2, 0x01, 0x07, 0x02, 0xf6]
            );
            // A required Option must be present.
            let absent = if format == FORMAT_JSON {
                r#"{"required_value":7}"#
            } else {
                "required_value: 7\n"
            };
            assert_eq!(
                text_to_cbor(absent, PRESENCE, format).unwrap_err(),
                "Missing: .required_option"
            );
        }
    }

    #[test]
    fn enum_payloads_are_sequences_on_both_sides() {
        for (text, cbor) in [
            (r#""Empty""#, vec![0x03]),
            (
                r#"{"Circle":[1.5]}"#,
                vec![0xa1, 0x01, 0x81, 0xf9, 0x3e, 0x00],
            ),
            (
                r#"{"Rect":[40,2]}"#,
                vec![0xa1, 0x02, 0x82, 0x18, 0x28, 0x02],
            ),
        ] {
            assert_eq!(
                text_to_cbor(text, SHAPE, FORMAT_JSON).unwrap(),
                cbor,
                "{text}"
            );
            assert_eq!(cbor_to_text(&cbor, SHAPE, FORMAT_JSON).unwrap(), text);
        }
        assert_eq!(
            text_to_cbor(r#"{"Rect":[40]}"#, SHAPE, FORMAT_JSON).unwrap_err(),
            "Type: .Rect: expected sequence of 2, found sequence of 1"
        );
        assert_eq!(
            text_to_cbor(r#""Hexagon""#, SHAPE, FORMAT_JSON).unwrap_err(),
            "UnknownVariant: no variant named Hexagon"
        );
        assert_eq!(
            cbor_to_text(&[0x09], SHAPE, FORMAT_JSON).unwrap_err(),
            "UnknownVariant: no variant named 9"
        );
    }

    #[test]
    fn collections_keep_key_types_and_refuse_repeats() {
        let desc = r#"{"k":"struct","f":[
            {"t":1,"n":"names","p":"required","d":{"k":"map","key":{"k":"str"},"value":{"k":"i64"}}},
            {"t":2,"n":"codes","p":"required","d":{"k":"map","key":{"k":"i64"},"value":{"k":"str"}}},
            {"t":3,"n":"ids","p":"required","d":{"k":"set","e":{"k":"u64"}}},
            {"t":4,"n":"blob","p":"required","d":{"k":"bytes"}}]}"#;
        let text =
            r#"{"names":{"a":1,"b":2},"codes":[[1,"one"],[2,"two"]],"ids":[1,2,3],"blob":"AP8="}"#;
        let cbor = text_to_cbor(text, desc, FORMAT_JSON).unwrap();
        assert_eq!(cbor_to_text(&cbor, desc, FORMAT_JSON).unwrap(), text);
        for (bad, error) in [
            (
                text.replace(r#"{"a":1,"b":2}"#, r#"{"a":1,"a":2}"#),
                "Syntax: line 1, column 17: duplicate key \"a\"",
            ),
            (
                text.replace(r#"[2,"two"]"#, r#"[1,"two"]"#),
                "Duplicate: .codes: 1 repeats",
            ),
            (
                text.replace("[1,2,3]", "[1,1]"),
                "Duplicate: .ids: 1 repeats",
            ),
            (
                text.replace(r#"[[1,"one"],[2,"two"]]"#, r#"{"1":"one"}"#),
                "Type: .codes: expected sequence of [key, value] pairs, found map",
            ),
            (
                text.replace(r#"{"a":1,"b":2}"#, r#"[["a",1]]"#),
                "Type: .names: expected map, found sequence",
            ),
            (
                text.replace(r#"{"a":1,"b":2}"#, "7"),
                "Type: .names: expected map, found integer",
            ),
            (
                text.replace("AP8=", "AP8"),
                "Type: .blob: expected base64 string, found string",
            ),
            (
                text.replace("[1,2,3]", "[-1]"),
                "Range: .ids[0]: -1 does not fit u64",
            ),
        ] {
            assert_eq!(
                text_to_cbor(&bad, desc, FORMAT_JSON).unwrap_err(),
                error,
                "{bad}"
            );
        }
        // A duplicate YAML key is refused before the walk sees it.
        let yaml = "names:\n  a: 1\n  a: 2\ncodes: []\nids: []\nblob: ''\n";
        assert!(text_to_cbor(yaml, desc, FORMAT_YAML)
            .unwrap_err()
            .starts_with("Syntax:"));
    }

    #[test]
    fn malformed_input_and_descriptors_fail_closed() {
        assert_eq!(
            text_to_cbor(r#"{"x":3,"#, POINT, FORMAT_JSON).unwrap_err(),
            "Syntax: line 1, column 8: unexpected end of input"
        );
        assert_eq!(
            text_to_cbor(r#"{"x":"3","y":4}"#, POINT, FORMAT_JSON).unwrap_err(),
            "Type: .x: expected integer, found string"
        );
        let deep = format!("{}{}", "[".repeat(200), "]".repeat(200));
        let list = r#"{"k":"vec","e":{"k":"i64"}}"#;
        assert!(text_to_cbor(&deep, list, FORMAT_JSON)
            .unwrap_err()
            .contains("nesting exceeds"));
        let no_presence = r#"{"k":"struct","f":[{"t":1,"n":"x","d":{"k":"i64"}}]}"#;
        assert_eq!(
            text_to_cbor(r#"{"x":1}"#, no_presence, FORMAT_JSON).unwrap_err(),
            "internal: malformed wire descriptor"
        );
        // A tag repeated in the walk's CBOR is refused, never resolved by order.
        assert!(
            cbor_to_text(&[0xa2, 0x01, 0x03, 0x01, 0x04], POINT, FORMAT_JSON)
                .unwrap_err()
                .contains("duplicate key 1")
        );
    }

    /// The CBOR the bridge writes is byte-identical to the legacy writer's,
    /// so a text-decoded value and a directly received one are the same body.
    #[test]
    fn codec_cbor_matches_the_legacy_writer() {
        let desc = r#"{"k":"struct","f":[
            {"t":10,"n":"big","p":"required","d":{"k":"i64"}},
            {"t":1,"n":"name","p":"required","d":{"k":"str"}},
            {"t":2,"n":"ratio","p":"required","d":{"k":"f64"}},
            {"t":3,"n":"tags","p":"required","d":{"k":"set","e":{"k":"str"}}},
            {"t":4,"n":"blob","p":"required","d":{"k":"bytes"}}]}"#;
        let text =
            r#"{"big":-70000,"name":"node","ratio":0.1,"tags":["bb","a","ccc"],"blob":"AQI="}"#;
        let codec = text_to_cbor(text, desc, FORMAT_JSON).unwrap();
        let blob = [1u8, 2];
        // SAFETY: the writer is created, driven and consumed within this block.
        let legacy = unsafe {
            let w = cbor_serial::hew_cbor_ser_new();
            cbor_serial::hew_cbor_ser_begin_map(w);
            cbor_serial::hew_cbor_ser_key_u64(w, 10);
            cbor_serial::hew_cbor_ser_i64(w, -70000);
            cbor_serial::hew_cbor_ser_key_u64(w, 1);
            cbor_serial::ser_text(w, "node");
            cbor_serial::hew_cbor_ser_key_u64(w, 2);
            cbor_serial::hew_cbor_ser_f64(w, 0.1);
            cbor_serial::hew_cbor_ser_key_u64(w, 3);
            cbor_serial::hew_cbor_ser_begin_set(w);
            for tag in ["bb", "a", "ccc"] {
                cbor_serial::ser_text(w, tag);
            }
            cbor_serial::hew_cbor_ser_end_array(w);
            cbor_serial::hew_cbor_ser_key_u64(w, 4);
            cbor_serial::hew_cbor_ser_bytes(w, blob.as_ptr(), 0, 2);
            cbor_serial::hew_cbor_ser_end_map(w);
            let mut len = 0;
            let raw = RawBytes(cbor_serial::hew_cbor_ser_finish(w, &raw mut len));
            core::slice::from_raw_parts(raw.0, len).to_vec()
        };
        assert_eq!(codec, legacy);
    }
}
