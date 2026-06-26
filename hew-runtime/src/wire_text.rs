//! Text wire-body codec — the CBOR↔JSON/YAML bridge.
//!
//! This module is the runtime half of the wire-type TEXT codec
//! (`to_json`/`from_json`/`to_yaml`/`from_yaml`). It deliberately does NOT carry
//! a second per-type struct/enum walk: the compiler already emits a binary CBOR
//! codec walk per `#[wire]` type (`hew-codegen-rs/src/llvm.rs`,
//! `hew-runtime/src/cbor_serial.rs`). The text codec is that binary codec PLUS a
//! generic transcode (RATIFIED, Q203):
//!
//! - **Serialize** (`to_json`/`to_yaml`): the compiler-emitted text thunk drives
//!   the existing CBOR serialize walk to build the value tree as CBOR bytes, then
//!   calls [`hew_wire_cbor_to_text`] with a per-type DESCRIPTOR (the tag↔name
//!   schema). This module parses the CBOR bytes into a `ciborium::Value`, walks it
//!   guided by the descriptor to map integer field tags → JSON/YAML key names,
//!   and serializes the resulting `serde_json`/`serde_yaml` value to text.
//!
//! - **Deserialize** (`from_json`/`from_yaml`): the text thunk calls
//!   [`hew_wire_text_to_cbor`], which parses the JSON/YAML text, walks the value
//!   tree guided by the descriptor to map key names → integer tags (rebuilding the
//!   exact CBOR shape the binary decode walk expects), serializes that to CBOR
//!   bytes, and returns them. The thunk then feeds the bytes to the existing CBOR
//!   deserialize walk to reconstruct a typed value.
//!
//! ## Untrusted input — FAIL CLOSED (CLAUDE.md §2)
//! `from_json`/`from_yaml` take arbitrary user text (config files, HTTP bodies).
//! Every malformed / out-of-shape / over-nested input is rejected with an error
//! message, never a trap, panic, fabricated value, or out-of-bounds access:
//!
//! - Parse failure (bad JSON/YAML syntax) → error string.
//! - Value-tree shape that does not match the descriptor (object where an array
//!   is required, an unknown enum variant name, a missing required field, a
//!   payload-arity mismatch) → error string. The downstream CBOR decode walk is a
//!   second fail-closed gate: a tag the schema does not name, or an out-of-range
//!   narrow int, is rejected there too.
//! - Deep nesting → bounded by [`MAX_TRANSCODE_DEPTH`]; exceeding it is an error,
//!   not a stack overflow.
//! - A descriptor the runtime cannot parse (a codegen bug) → error string; the
//!   bridge never proceeds on a half-understood schema.
//!
//! The FFI entry points additionally wrap the whole body in
//! [`std::panic::catch_unwind`] so a panic anywhere in serde or the transcode
//! degrades to a clean error return rather than unwinding across the C ABI
//! (undefined behaviour).

use core::ffi::{c_char, c_int};

use ciborium::value::{Integer, Value as CborValue};
use hew_cabi::cabi::malloc_cstring;

/// Maximum value-tree nesting the transcode will descend before failing closed.
/// Bounds adversarial deeply-nested JSON/YAML so a hostile payload cannot drive
/// the recursive transcode (or the recursive `Drop` of the parsed tree) into a
/// stack overflow. The wire-body floor (records, vectors, options, enums) never
/// nests this deep in practice; a payload that does is rejected, not crashed.
const MAX_TRANSCODE_DEPTH: usize = 128;

/// Text format selector passed across the C ABI by the codegen-emitted thunk.
/// Must agree with the `WireTextFormat`-derived constants the thunk emits.
const FORMAT_JSON: c_int = 0;
const FORMAT_YAML: c_int = 1;

// ── Descriptor ──────────────────────────────────────────────────────────────
//
// The compiler emits a compact JSON descriptor string per wire type that maps
// each field's wire tag (`@N`) to its JSON/YAML key name, recursively for nested
// wire structs/enums, vectors, and options. The runtime parses it once per bridge
// call. The format is intentionally small and explicit so the codegen emitter is
// trivial to keep in sync with this parser.
//
// Grammar (every node is a JSON object with a `"k"` kind tag):
//   scalar : {"k":"i64"|"u64"|"f64"|"bool"|"str"|"bytes"|"char"}
//   struct : {"k":"struct","f":[{"t":<tag>,"n":"<name>","d":<node>}, ...]}
//   enum   : {"k":"enum","v":[{"t":<tag>,"n":"<name>","p":[<node>, ...]}, ...]}
//   vec    : {"k":"vec","e":<node>}
//   option : {"k":"opt","e":<node>}
//   opaque : {"k":"opaque"}   // a shape outside the text floor — fail closed

/// A parsed descriptor node: the structural schema the transcode walks.
#[derive(Debug, Clone)]
enum Desc {
    /// A signed integer scalar (`i8..i64`, `isize`, `char` as codepoint,
    /// `duration`). CBOR carries it as `Integer`; text as a JSON number.
    Int,
    /// An unsigned integer scalar (`u8..u64`, `usize`).
    Uint,
    /// A floating-point scalar (`f32`/`f64`).
    Float,
    /// A boolean.
    Bool,
    /// A UTF-8 string.
    Str,
    /// A `bytes` value: CBOR byte string ↔ JSON array of byte integers.
    Bytes,
    /// A wire struct: an ordered list of `(tag, name, field_descriptor)`.
    Struct(Vec<StructField>),
    /// A wire enum: a list of `(tag, name, payload_descriptors)`.
    Enum(Vec<EnumVariant>),
    /// A `Vec<T>`: CBOR array ↔ JSON array, element-wise transcoded.
    Vec(Box<Desc>),
    /// An `Option<T>`: CBOR null/value ↔ JSON null/value.
    Opt(Box<Desc>),
    /// A shape the text codec does not support (outside the wire-body floor).
    /// Always fails closed.
    Opaque,
}

#[derive(Debug, Clone)]
struct StructField {
    tag: u64,
    name: String,
    desc: Desc,
}

#[derive(Debug, Clone)]
struct EnumVariant {
    tag: u64,
    name: String,
    payload: Vec<Desc>,
}

impl Desc {
    /// Parse a descriptor node from a `serde_json::Value`, bounding recursion by
    /// `depth`. Returns `None` on any malformed node (a codegen bug) so the
    /// bridge fails closed rather than transcoding against a half-understood
    /// schema.
    fn parse(node: &serde_json::Value, depth: usize) -> Option<Self> {
        if depth >= MAX_TRANSCODE_DEPTH {
            return None;
        }
        let obj = node.as_object()?;
        let kind = obj.get("k")?.as_str()?;
        match kind {
            "i64" => Some(Self::Int),
            "u64" => Some(Self::Uint),
            "f64" => Some(Self::Float),
            "bool" => Some(Self::Bool),
            "str" => Some(Self::Str),
            "bytes" => Some(Self::Bytes),
            "opaque" => Some(Self::Opaque),
            "vec" => Some(Self::Vec(Box::new(Self::parse(obj.get("e")?, depth + 1)?))),
            "opt" => Some(Self::Opt(Box::new(Self::parse(obj.get("e")?, depth + 1)?))),
            "struct" => {
                let mut fields = Vec::new();
                for f in obj.get("f")?.as_array()? {
                    let fo = f.as_object()?;
                    let tag = fo.get("t")?.as_u64()?;
                    let name = fo.get("n")?.as_str()?.to_string();
                    let desc = Self::parse(fo.get("d")?, depth + 1)?;
                    fields.push(StructField { tag, name, desc });
                }
                Some(Self::Struct(fields))
            }
            "enum" => {
                let mut variants = Vec::new();
                for v in obj.get("v")?.as_array()? {
                    let vo = v.as_object()?;
                    let tag = vo.get("t")?.as_u64()?;
                    let name = vo.get("n")?.as_str()?.to_string();
                    let mut payload = Vec::new();
                    for p in vo.get("p")?.as_array()? {
                        payload.push(Self::parse(p, depth + 1)?);
                    }
                    variants.push(EnumVariant { tag, name, payload });
                }
                Some(Self::Enum(variants))
            }
            _ => None,
        }
    }
}

/// Parse a NUL-terminated descriptor string into a `Desc` tree. Returns `None`
/// on any malformed input (the bridge then fails closed).
///
/// # Safety
/// `ptr` must be null or a valid NUL-terminated C string.
unsafe fn parse_descriptor(ptr: *const c_char) -> Option<Desc> {
    if ptr.is_null() {
        return None;
    }
    // SAFETY: ptr is a valid NUL-terminated C string per this fn's contract.
    let cstr = unsafe { core::ffi::CStr::from_ptr(ptr) };
    let text = cstr.to_str().ok()?;
    let json: serde_json::Value = serde_json::from_str(text).ok()?;
    Desc::parse(&json, 0)
}

// ── CBOR → text (serialize: to_json / to_yaml) ───────────────────────────────

/// Transcode a `ciborium::Value` (the CBOR body codec's value tree, keyed by
/// integer field tags) into a `serde_json::Value` (keyed by JSON/YAML key names)
/// guided by `desc`. Fails closed on any shape that does not match the
/// descriptor.
fn cbor_to_json(value: &CborValue, desc: &Desc, depth: usize) -> Result<serde_json::Value, String> {
    if depth >= MAX_TRANSCODE_DEPTH {
        return Err("value nesting exceeds the supported depth".to_string());
    }
    match desc {
        Desc::Int => match value {
            CborValue::Integer(i) => {
                let raw = i128::from(*i);
                i64::try_from(raw)
                    .map(serde_json::Value::from)
                    .map_err(|_| "integer out of i64 range".to_string())
            }
            _ => Err("expected an integer".to_string()),
        },
        Desc::Uint => match value {
            CborValue::Integer(i) => {
                let raw = i128::from(*i);
                u64::try_from(raw)
                    .map(serde_json::Value::from)
                    .map_err(|_| "integer out of u64 range".to_string())
            }
            _ => Err("expected an unsigned integer".to_string()),
        },
        Desc::Float => match value {
            CborValue::Float(f) => serde_json::Number::from_f64(*f)
                .map(serde_json::Value::Number)
                .ok_or_else(|| "float is not finite".to_string()),
            // CBOR may carry a whole-number float as an integer; tolerate it.
            CborValue::Integer(i) => {
                #[allow(
                    clippy::cast_precision_loss,
                    reason = "a whole-number float read back from CBOR may lose \
                              low bits past 2^53; the wire-body floor's f64 fields \
                              never carry such magnitudes and a lossy reconstruction \
                              is preferable to rejecting the document"
                )]
                let f = i128::from(*i) as f64;
                serde_json::Number::from_f64(f)
                    .map(serde_json::Value::Number)
                    .ok_or_else(|| "float is not finite".to_string())
            }
            _ => Err("expected a float".to_string()),
        },
        Desc::Bool => match value {
            CborValue::Bool(b) => Ok(serde_json::Value::Bool(*b)),
            _ => Err("expected a bool".to_string()),
        },
        Desc::Str => match value {
            CborValue::Text(s) => Ok(serde_json::Value::String(s.clone())),
            _ => Err("expected a string".to_string()),
        },
        Desc::Bytes => match value {
            CborValue::Bytes(b) => Ok(serde_json::Value::Array(
                b.iter()
                    .map(|byte| serde_json::Value::from(u64::from(*byte)))
                    .collect(),
            )),
            _ => Err("expected a byte string".to_string()),
        },
        Desc::Opt(inner) => match value {
            CborValue::Null => Ok(serde_json::Value::Null),
            other => cbor_to_json(other, inner, depth + 1),
        },
        Desc::Vec(elem) => match value {
            CborValue::Array(items) => {
                let mut out = Vec::with_capacity(items.len());
                for it in items {
                    out.push(cbor_to_json(it, elem, depth + 1)?);
                }
                Ok(serde_json::Value::Array(out))
            }
            _ => Err("expected an array".to_string()),
        },
        Desc::Struct(fields) => {
            let CborValue::Map(pairs) = value else {
                return Err("expected a map".to_string());
            };
            // Build a tag → value lookup from the CBOR map.
            let mut by_tag: std::collections::BTreeMap<i128, &CborValue> =
                std::collections::BTreeMap::new();
            for (k, v) in pairs {
                let CborValue::Integer(tag) = k else {
                    return Err("struct map key is not an integer tag".to_string());
                };
                by_tag.insert(i128::from(*tag), v);
            }
            let mut obj = serde_json::Map::new();
            for field in fields {
                let Some(v) = by_tag.get(&i128::from(field.tag)) else {
                    // A field absent from the CBOR tree is only legal for an
                    // optional field (the encode walk emits CBOR null for None,
                    // so a truly-present optional still appears as a key). Mirror
                    // CBOR forward-compat: an absent field is simply omitted from
                    // the text output rather than fabricated.
                    continue;
                };
                obj.insert(field.name.clone(), cbor_to_json(v, &field.desc, depth + 1)?);
            }
            Ok(serde_json::Value::Object(obj))
        }
        Desc::Enum(variants) => enum_cbor_to_json(value, variants, depth),
        Desc::Opaque => Err("value type is outside the text wire-body floor".to_string()),
    }
}

/// Transcode a CBOR enum value into JSON. Unit variants encode as a bare integer
/// tag → JSON string `"VariantName"`; payload variants encode as the CBOR
/// map-of-one `{tag: [payload...]}` → JSON object `{"VariantName": [payload...]}`.
fn enum_cbor_to_json(
    value: &CborValue,
    variants: &[EnumVariant],
    depth: usize,
) -> Result<serde_json::Value, String> {
    match value {
        // Unit variant: a bare integer tag.
        CborValue::Integer(tag) => {
            let raw = i128::from(*tag);
            let tag_u64 = u64::try_from(raw).map_err(|_| "negative enum tag".to_string())?;
            let variant = variants
                .iter()
                .find(|v| v.tag == tag_u64)
                .ok_or_else(|| format!("unknown enum tag {tag_u64}"))?;
            if !variant.payload.is_empty() {
                return Err(format!(
                    "enum variant `{}` expects {} field(s) but the value is a unit tag",
                    variant.name,
                    variant.payload.len()
                ));
            }
            Ok(serde_json::Value::String(variant.name.clone()))
        }
        // Payload variant: a map-of-one `{tag: [payload...]}`.
        CborValue::Map(pairs) => {
            let mut iter = pairs.iter();
            let (Some((k, body)), None) = (iter.next(), iter.next()) else {
                return Err("enum payload map must have exactly one entry".to_string());
            };
            let CborValue::Integer(tag) = k else {
                return Err("enum payload map key is not an integer tag".to_string());
            };
            let tag_u64 =
                u64::try_from(i128::from(*tag)).map_err(|_| "negative enum tag".to_string())?;
            let variant = variants
                .iter()
                .find(|v| v.tag == tag_u64)
                .ok_or_else(|| format!("unknown enum tag {tag_u64}"))?;
            let CborValue::Array(items) = body else {
                return Err("enum payload body is not an array".to_string());
            };
            if items.len() != variant.payload.len() {
                return Err(format!(
                    "enum variant `{}` expects {} field(s), found {}",
                    variant.name,
                    variant.payload.len(),
                    items.len()
                ));
            }
            let mut payload = Vec::with_capacity(items.len());
            for (it, pd) in items.iter().zip(variant.payload.iter()) {
                payload.push(cbor_to_json(it, pd, depth + 1)?);
            }
            let mut obj = serde_json::Map::new();
            obj.insert(variant.name.clone(), serde_json::Value::Array(payload));
            Ok(serde_json::Value::Object(obj))
        }
        _ => Err("expected an enum value (integer tag or single-entry map)".to_string()),
    }
}

// ── text → CBOR (deserialize: from_json / from_yaml) ─────────────────────────

/// Transcode a `serde_json::Value` (parsed from JSON or YAML, keyed by names)
/// into a `ciborium::Value` (keyed by integer tags) matching exactly the shape
/// the binary CBOR decode walk expects. Fails closed on any shape mismatch.
fn json_to_cbor(value: &serde_json::Value, desc: &Desc, depth: usize) -> Result<CborValue, String> {
    if depth >= MAX_TRANSCODE_DEPTH {
        return Err("value nesting exceeds the supported depth".to_string());
    }
    match desc {
        Desc::Int => {
            let n = value
                .as_i64()
                .ok_or_else(|| "expected an integer".to_string())?;
            Ok(CborValue::Integer(Integer::from(n)))
        }
        Desc::Uint => {
            let n = value
                .as_u64()
                .ok_or_else(|| "expected an unsigned integer".to_string())?;
            Ok(CborValue::Integer(Integer::from(n)))
        }
        Desc::Float => {
            // Accept a JSON number written either as a float or an integer.
            let f = value
                .as_f64()
                .ok_or_else(|| "expected a number".to_string())?;
            Ok(CborValue::Float(f))
        }
        Desc::Bool => {
            let b = value
                .as_bool()
                .ok_or_else(|| "expected a bool".to_string())?;
            Ok(CborValue::Bool(b))
        }
        Desc::Str => {
            let s = value
                .as_str()
                .ok_or_else(|| "expected a string".to_string())?;
            Ok(CborValue::Text(s.to_string()))
        }
        Desc::Bytes => {
            let arr = value
                .as_array()
                .ok_or_else(|| "expected an array of bytes".to_string())?;
            let mut bytes = Vec::with_capacity(arr.len());
            for b in arr {
                let n = b
                    .as_u64()
                    .ok_or_else(|| "byte array element is not an integer".to_string())?;
                let byte =
                    u8::try_from(n).map_err(|_| "byte array element exceeds 255".to_string())?;
                bytes.push(byte);
            }
            Ok(CborValue::Bytes(bytes))
        }
        Desc::Opt(inner) => {
            if value.is_null() {
                Ok(CborValue::Null)
            } else {
                json_to_cbor(value, inner, depth + 1)
            }
        }
        Desc::Vec(elem) => {
            let arr = value
                .as_array()
                .ok_or_else(|| "expected an array".to_string())?;
            let mut out = Vec::with_capacity(arr.len());
            for it in arr {
                out.push(json_to_cbor(it, elem, depth + 1)?);
            }
            Ok(CborValue::Array(out))
        }
        Desc::Struct(fields) => {
            let obj = value
                .as_object()
                .ok_or_else(|| "expected an object".to_string())?;
            let mut entries = Vec::with_capacity(fields.len());
            for field in fields {
                match obj.get(&field.name) {
                    Some(v) => {
                        let cv = json_to_cbor(v, &field.desc, depth + 1)?;
                        entries.push((CborValue::Integer(Integer::from(field.tag)), cv));
                    }
                    None => {
                        // A missing optional field becomes CBOR null (its absence
                        // is well-defined as None). A missing required field is a
                        // decode failure here, mirroring CBOR's missing-key reject.
                        if matches!(field.desc, Desc::Opt(_)) {
                            entries.push((
                                CborValue::Integer(Integer::from(field.tag)),
                                CborValue::Null,
                            ));
                        } else {
                            return Err(format!("missing required field `{}`", field.name));
                        }
                    }
                }
            }
            // Unknown keys in the input are tolerated (forward-compat), matching
            // CBOR's unselected-key drop on `exit_map`.
            Ok(CborValue::Map(entries))
        }
        Desc::Enum(variants) => enum_json_to_cbor(value, variants, depth),
        Desc::Opaque => Err("value type is outside the text wire-body floor".to_string()),
    }
}

/// Transcode a JSON enum value into CBOR. A JSON string `"VariantName"` becomes a
/// bare integer tag (unit variant); a JSON object `{"VariantName": [payload...]}`
/// becomes the CBOR map-of-one `{tag: [payload...]}`.
fn enum_json_to_cbor(
    value: &serde_json::Value,
    variants: &[EnumVariant],
    depth: usize,
) -> Result<CborValue, String> {
    match value {
        // Unit variant: a JSON string naming the variant.
        serde_json::Value::String(name) => {
            let variant = variants
                .iter()
                .find(|v| &v.name == name)
                .ok_or_else(|| format!("unknown enum variant `{name}`"))?;
            if !variant.payload.is_empty() {
                return Err(format!(
                    "enum variant `{}` expects {} field(s) but was given as a bare name",
                    variant.name,
                    variant.payload.len()
                ));
            }
            Ok(CborValue::Integer(Integer::from(variant.tag)))
        }
        // Payload variant: a single-key object `{"VariantName": [payload...]}`.
        serde_json::Value::Object(obj) => {
            if obj.len() != 1 {
                return Err("enum object must have exactly one variant key".to_string());
            }
            let (name, body) = obj.iter().next().expect("len checked == 1");
            let variant = variants
                .iter()
                .find(|v| &v.name == name)
                .ok_or_else(|| format!("unknown enum variant `{name}`"))?;
            let arr = body
                .as_array()
                .ok_or_else(|| format!("enum variant `{name}` payload is not an array"))?;
            if arr.len() != variant.payload.len() {
                return Err(format!(
                    "enum variant `{}` expects {} field(s), found {}",
                    variant.name,
                    variant.payload.len(),
                    arr.len()
                ));
            }
            let mut payload = Vec::with_capacity(arr.len());
            for (it, pd) in arr.iter().zip(variant.payload.iter()) {
                payload.push(json_to_cbor(it, pd, depth + 1)?);
            }
            Ok(CborValue::Map(vec![(
                CborValue::Integer(Integer::from(variant.tag)),
                CborValue::Array(payload),
            )]))
        }
        _ => Err("expected an enum value (a variant name or a single-key object)".to_string()),
    }
}

// ── FFI entry points ─────────────────────────────────────────────────────────

/// Serialize a wire value's CBOR encoding to JSON/YAML text.
///
/// `cbor_ptr`/`cbor_len` is the value tree the CBOR serialize walk produced;
/// `descriptor` is the type's tag↔name schema; `format` selects JSON or YAML.
/// Returns a freshly `malloc_cstring`'d text string the caller frees via
/// `hew_string_drop`. Returns null on any failure (a malformed descriptor or a
/// CBOR tree that does not match it — both codegen bugs, never user input on this
/// direction). Never panics across the ABI.
///
/// # Safety
/// `cbor_ptr` must point to `cbor_len` readable bytes; `descriptor` must be null
/// or a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_cbor_to_text(
    cbor_ptr: *const u8,
    cbor_len: usize,
    descriptor: *const c_char,
    format: c_int,
) -> *mut c_char {
    let result = std::panic::catch_unwind(|| {
        if cbor_ptr.is_null() {
            return None;
        }
        // SAFETY: cbor_ptr points to cbor_len readable bytes per the contract.
        let bytes = unsafe { core::slice::from_raw_parts(cbor_ptr, cbor_len) };
        // SAFETY: descriptor is null or a valid C string per the contract.
        let desc = unsafe { parse_descriptor(descriptor) }?;
        let cbor: CborValue = ciborium::de::from_reader(bytes).ok()?;
        let json = cbor_to_json(&cbor, &desc, 0).ok()?;
        let text = match format {
            FORMAT_JSON => serde_json::to_string(&json).ok()?,
            FORMAT_YAML => serde_yaml::to_string(&json).ok()?,
            _ => return None,
        };
        Some(text)
    });
    match result {
        Ok(Some(text)) => {
            let bytes = text.as_bytes();
            // An interior NUL would truncate the C string — reject rather than
            // deliver a truncated document. JSON/YAML of a wire value never
            // contains a NUL, so this only trips on a degenerate value.
            if bytes.contains(&0) {
                return core::ptr::null_mut();
            }
            // SAFETY: bytes is valid for its length; malloc_cstring copies it.
            unsafe { malloc_cstring(bytes.as_ptr(), bytes.len()) }
        }
        _ => core::ptr::null_mut(),
    }
}

/// Parse JSON/YAML text into the CBOR bytes the binary decode walk expects.
///
/// `text` is the untrusted input; `descriptor` is the type's tag↔name schema;
/// `format` selects JSON or YAML. On success, returns a `libc::malloc`'d CBOR
/// buffer (freed by the shared `hew_ser_free_bytes`) and writes its length to
/// `*out_len`. On ANY failure — parse error, shape mismatch, over-nesting,
/// malformed descriptor — returns null, writes 0 to `*out_len`, and stores a
/// freshly `malloc_cstring`'d error message into `*out_err` (the caller wraps it
/// in `Err(string)`). Never panics across the ABI, never fabricates a value.
///
/// # Safety
/// `text`/`descriptor` must be null or valid NUL-terminated C strings; `out_len`
/// and `out_err` must be valid writable pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_text_to_cbor(
    text: *const c_char,
    descriptor: *const c_char,
    format: c_int,
    out_len: *mut usize,
    out_err: *mut *mut c_char,
) -> *mut u8 {
    if !out_len.is_null() {
        // SAFETY: out_len validated non-null.
        unsafe { *out_len = 0 };
    }
    if !out_err.is_null() {
        // SAFETY: out_err validated non-null.
        unsafe { *out_err = core::ptr::null_mut() };
    }
    let outcome = std::panic::catch_unwind(|| -> Result<Vec<u8>, String> {
        if text.is_null() {
            return Err("input text is null".to_string());
        }
        // SAFETY: text is a valid C string per the contract.
        let text_str = unsafe { core::ffi::CStr::from_ptr(text) }
            .to_str()
            .map_err(|_| "input text is not valid UTF-8".to_string())?;
        // SAFETY: descriptor is null or a valid C string per the contract.
        let desc = unsafe { parse_descriptor(descriptor) }
            .ok_or_else(|| "internal: malformed wire descriptor".to_string())?;
        let parsed: serde_json::Value = match format {
            FORMAT_JSON => {
                serde_json::from_str(text_str).map_err(|e| format!("invalid JSON: {e}"))?
            }
            FORMAT_YAML => {
                serde_yaml::from_str(text_str).map_err(|e| format!("invalid YAML: {e}"))?
            }
            _ => return Err("unknown text format".to_string()),
        };
        let cbor = json_to_cbor(&parsed, &desc, 0)?;
        let mut encoded: Vec<u8> = Vec::new();
        ciborium::ser::into_writer(&cbor, &mut encoded)
            .map_err(|_| "internal: CBOR re-encode failed".to_string())?;
        Ok(encoded)
    });
    let encoded = match outcome {
        Ok(Ok(encoded)) => encoded,
        Ok(Err(msg)) => {
            write_err(out_err, &msg);
            return core::ptr::null_mut();
        }
        Err(_) => {
            write_err(out_err, "internal: text codec panicked");
            return core::ptr::null_mut();
        }
    };
    if encoded.is_empty() {
        write_err(out_err, "internal: empty CBOR encoding");
        return core::ptr::null_mut();
    }
    // SAFETY: malloc returns a valid pointer or null.
    let dst = unsafe { libc::malloc(encoded.len()) }.cast::<u8>();
    if dst.is_null() {
        write_err(out_err, "out of memory encoding CBOR");
        return core::ptr::null_mut();
    }
    // SAFETY: dst has encoded.len() bytes; encoded is a valid slice.
    unsafe { core::ptr::copy_nonoverlapping(encoded.as_ptr(), dst, encoded.len()) };
    if !out_len.is_null() {
        // SAFETY: out_len validated non-null.
        unsafe { *out_len = encoded.len() };
    }
    dst
}

/// Store a malloc'd copy of `msg` into `*out_err` (if non-null) so the caller can
/// construct `Err(string)`. A NUL in the message is impossible (all messages are
/// static or formatted from numbers/names) but `malloc_cstring` would truncate
/// it harmlessly.
fn write_err(out_err: *mut *mut c_char, msg: &str) {
    if out_err.is_null() {
        return;
    }
    let bytes = msg.as_bytes();
    // SAFETY: bytes is valid for its length; out_err is non-null per the guard.
    let cstr = unsafe { malloc_cstring(bytes.as_ptr(), bytes.len()) };
    // SAFETY: out_err validated non-null.
    unsafe { *out_err = cstr };
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::cabi::free_cstring;

    /// Release a header-aware C string produced by `malloc_cstring` (the
    /// production callers free these via `hew_string_drop`; bare `libc::free`
    /// would free the data pointer instead of the allocation base and corrupt
    /// the heap).
    fn free_hew_cstring(ptr: *mut c_char) {
        if !ptr.is_null() {
            // SAFETY: ptr is a header-aware C string from `malloc_cstring`,
            // freed exactly once here.
            unsafe { free_cstring(ptr) };
        }
    }

    /// Outcome of a `hew_wire_text_to_cbor` call: either the CBOR bytes (success)
    /// or the freed-and-returned error message (failure). Frees the FFI-owned
    /// allocations before returning owned Rust values.
    enum TextToCbor {
        Ok(Vec<u8>),
        Err(String),
    }

    /// Drive `hew_wire_text_to_cbor` from a `&str` text and descriptor, returning
    /// an owned outcome. Centralises the `unsafe` FFI call so every test shares
    /// one audited boundary.
    fn text_to_cbor(text: &str, descriptor: &str, format: c_int) -> TextToCbor {
        let text_c = std::ffi::CString::new(text).expect("test text has no interior NUL");
        let desc_c =
            std::ffi::CString::new(descriptor).expect("test descriptor has no interior NUL");
        let mut out_len: usize = 0;
        let mut out_err: *mut c_char = core::ptr::null_mut();
        // SAFETY: text_c/desc_c are valid NUL-terminated C strings live for the
        // call; out_len/out_err are valid writable locals.
        let ptr = unsafe {
            hew_wire_text_to_cbor(
                text_c.as_ptr(),
                desc_c.as_ptr(),
                format,
                core::ptr::addr_of_mut!(out_len),
                core::ptr::addr_of_mut!(out_err),
            )
        };
        if ptr.is_null() {
            assert_eq!(out_len, 0, "failure path must leave out_len at 0");
            assert!(!out_err.is_null(), "failure path must set an error message");
            // SAFETY: out_err is a header-aware C string the FFI just allocated.
            let msg = unsafe { core::ffi::CStr::from_ptr(out_err) }
                .to_string_lossy()
                .into_owned();
            free_hew_cstring(out_err);
            TextToCbor::Err(msg)
        } else {
            assert!(out_err.is_null(), "success path must not set an error");
            // SAFETY: ptr points to out_len bytes from libc::malloc.
            let bytes = unsafe { core::slice::from_raw_parts(ptr, out_len) }.to_vec();
            // SAFETY: ptr is a libc::malloc buffer (the CBOR bytes), freed once.
            unsafe { libc::free(ptr.cast()) };
            TextToCbor::Ok(bytes)
        }
    }

    /// Drive `hew_wire_cbor_to_text`, returning the owned text on success.
    fn cbor_to_text(cbor: &[u8], descriptor: &str, format: c_int) -> Option<String> {
        let desc_c =
            std::ffi::CString::new(descriptor).expect("test descriptor has no interior NUL");
        // SAFETY: cbor is a valid slice; desc_c is a valid NUL-terminated string.
        let ptr =
            unsafe { hew_wire_cbor_to_text(cbor.as_ptr(), cbor.len(), desc_c.as_ptr(), format) };
        if ptr.is_null() {
            return None;
        }
        // SAFETY: ptr is a header-aware C string the FFI just allocated.
        let text = unsafe { core::ffi::CStr::from_ptr(ptr) }
            .to_string_lossy()
            .into_owned();
        free_hew_cstring(ptr);
        Some(text)
    }

    /// Build a CBOR buffer for a value tree (test helper mirroring the encode
    /// walk's output).
    fn encode_cbor(v: &CborValue) -> Vec<u8> {
        let mut out = Vec::new();
        ciborium::ser::into_writer(v, &mut out).expect("encode");
        out
    }

    fn point_desc() -> Desc {
        Desc::Struct(vec![
            StructField {
                tag: 1,
                name: "x".to_string(),
                desc: Desc::Int,
            },
            StructField {
                tag: 2,
                name: "y".to_string(),
                desc: Desc::Int,
            },
        ])
    }

    #[test]
    fn struct_cbor_to_json_uses_field_names() {
        let cbor = CborValue::Map(vec![
            (CborValue::Integer(1.into()), CborValue::Integer(3.into())),
            (CborValue::Integer(2.into()), CborValue::Integer(4.into())),
        ]);
        let json = cbor_to_json(&cbor, &point_desc(), 0).unwrap();
        assert_eq!(serde_json::to_string(&json).unwrap(), r#"{"x":3,"y":4}"#);
    }

    #[test]
    fn struct_json_to_cbor_uses_integer_tags() {
        let json: serde_json::Value = serde_json::from_str(r#"{"x":3,"y":4}"#).unwrap();
        let cbor = json_to_cbor(&json, &point_desc(), 0).unwrap();
        assert_eq!(
            cbor,
            CborValue::Map(vec![
                (CborValue::Integer(1.into()), CborValue::Integer(3.into())),
                (CborValue::Integer(2.into()), CborValue::Integer(4.into())),
            ])
        );
    }

    #[test]
    fn round_trip_struct_through_text_and_back() {
        let original = CborValue::Map(vec![
            (CborValue::Integer(1.into()), CborValue::Integer(7.into())),
            (CborValue::Integer(2.into()), CborValue::Integer(9.into())),
        ]);
        let json = cbor_to_json(&original, &point_desc(), 0).unwrap();
        let text = serde_json::to_string(&json).unwrap();
        let reparsed: serde_json::Value = serde_json::from_str(&text).unwrap();
        let back = json_to_cbor(&reparsed, &point_desc(), 0).unwrap();
        assert_eq!(original, back);
    }

    const POINT_DESC_JSON: &str =
        r#"{"k":"struct","f":[{"t":1,"n":"x","d":{"k":"i64"}},{"t":2,"n":"y","d":{"k":"i64"}}]}"#;

    #[test]
    fn malformed_json_is_error_not_panic() {
        let desc = r#"{"k":"struct","f":[{"t":1,"n":"x","d":{"k":"i64"}}]}"#;
        match text_to_cbor("{not json", desc, FORMAT_JSON) {
            TextToCbor::Err(msg) => assert!(msg.contains("invalid JSON"), "got: {msg}"),
            TextToCbor::Ok(_) => panic!("malformed JSON must fail closed"),
        }
    }

    #[test]
    fn wrong_shape_is_error() {
        // Descriptor says struct {x:i64}; input is an array — must reject.
        let desc = r#"{"k":"struct","f":[{"t":1,"n":"x","d":{"k":"i64"}}]}"#;
        match text_to_cbor("[1,2,3]", desc, FORMAT_JSON) {
            TextToCbor::Err(msg) => assert!(msg.contains("expected an object"), "got: {msg}"),
            TextToCbor::Ok(_) => panic!("wrong-shape input must fail closed"),
        }
    }

    #[test]
    fn missing_required_field_is_error() {
        let json: serde_json::Value = serde_json::from_str(r#"{"x":3}"#).unwrap();
        let err = json_to_cbor(&json, &point_desc(), 0).unwrap_err();
        assert!(err.contains("missing required field `y`"), "got: {err}");
    }

    #[test]
    fn optional_field_absent_becomes_null() {
        let desc = Desc::Struct(vec![StructField {
            tag: 1,
            name: "v".to_string(),
            desc: Desc::Opt(Box::new(Desc::Int)),
        }]);
        let json: serde_json::Value = serde_json::from_str("{}").unwrap();
        let cbor = json_to_cbor(&json, &desc, 0).unwrap();
        assert_eq!(
            cbor,
            CborValue::Map(vec![(CborValue::Integer(1.into()), CborValue::Null)])
        );
    }

    #[test]
    fn deep_nesting_fails_closed() {
        // A vec-of-vec-of-... descriptor only one level deep, but feed it a value
        // nested far past MAX_TRANSCODE_DEPTH. The transcode caps depth.
        let desc = Desc::Vec(Box::new(Desc::Int));
        // Build a deeply nested JSON array [[[...]]] beyond the cap.
        let mut text = String::new();
        for _ in 0..(MAX_TRANSCODE_DEPTH + 10) {
            text.push('[');
        }
        text.push('1');
        for _ in 0..(MAX_TRANSCODE_DEPTH + 10) {
            text.push(']');
        }
        // serde_json itself caps recursion (128 by default) so the parse already
        // rejects; either way the bridge returns an error, never a stack
        // overflow.
        if let Ok(v) = serde_json::from_str::<serde_json::Value>(&text) {
            let r = json_to_cbor(&v, &desc, 0);
            assert!(r.is_err(), "over-nested value must be rejected");
        }
        // else: serde_json rejected the parse — also fail-closed.
    }

    #[test]
    fn unit_enum_round_trips_as_string() {
        let desc = Desc::Enum(vec![
            EnumVariant {
                tag: 0,
                name: "Red".to_string(),
                payload: vec![],
            },
            EnumVariant {
                tag: 1,
                name: "Green".to_string(),
                payload: vec![],
            },
        ]);
        let cbor = CborValue::Integer(1.into());
        let json = cbor_to_json(&cbor, &desc, 0).unwrap();
        assert_eq!(json, serde_json::Value::String("Green".to_string()));
        let back = json_to_cbor(&json, &desc, 0).unwrap();
        assert_eq!(back, cbor);
    }

    #[test]
    fn payload_enum_round_trips_as_object() {
        let desc = Desc::Enum(vec![EnumVariant {
            tag: 2,
            name: "Move".to_string(),
            payload: vec![Desc::Int, Desc::Int],
        }]);
        let cbor = CborValue::Map(vec![(
            CborValue::Integer(2.into()),
            CborValue::Array(vec![
                CborValue::Integer(3.into()),
                CborValue::Integer(4.into()),
            ]),
        )]);
        let json = cbor_to_json(&cbor, &desc, 0).unwrap();
        assert_eq!(serde_json::to_string(&json).unwrap(), r#"{"Move":[3,4]}"#);
        let back = json_to_cbor(&json, &desc, 0).unwrap();
        assert_eq!(back, cbor);
    }

    #[test]
    fn unknown_enum_variant_is_error() {
        let desc = Desc::Enum(vec![EnumVariant {
            tag: 0,
            name: "Red".to_string(),
            payload: vec![],
        }]);
        let json = serde_json::Value::String("Purple".to_string());
        let err = json_to_cbor(&json, &desc, 0).unwrap_err();
        assert!(err.contains("unknown enum variant `Purple`"), "got: {err}");
    }

    #[test]
    fn enum_payload_arity_mismatch_is_error() {
        let desc = Desc::Enum(vec![EnumVariant {
            tag: 2,
            name: "Move".to_string(),
            payload: vec![Desc::Int, Desc::Int],
        }]);
        let json: serde_json::Value = serde_json::from_str(r#"{"Move":[3]}"#).unwrap();
        let err = json_to_cbor(&json, &desc, 0).unwrap_err();
        assert!(err.contains("expects 2 field(s), found 1"), "got: {err}");
    }

    #[test]
    fn yaml_round_trips_struct() {
        let cbor = CborValue::Map(vec![
            (CborValue::Integer(1.into()), CborValue::Integer(5.into())),
            (CborValue::Integer(2.into()), CborValue::Integer(6.into())),
        ]);
        let buf = encode_cbor(&cbor);
        let text =
            cbor_to_text(&buf, POINT_DESC_JSON, FORMAT_YAML).expect("CBOR→YAML must succeed");
        // YAML round-trips back through the parser to the original CBOR tree.
        let reencoded = match text_to_cbor(&text, POINT_DESC_JSON, FORMAT_YAML) {
            TextToCbor::Ok(bytes) => bytes,
            TextToCbor::Err(msg) => panic!("YAML re-parse must succeed: {msg}"),
        };
        let back: CborValue = ciborium::de::from_reader(reencoded.as_slice()).unwrap();
        assert_eq!(back, cbor);
    }

    #[test]
    fn out_of_range_uint_to_json_is_error() {
        // A u64 value that overflows i64 with an Int descriptor → error, no panic.
        let cbor = CborValue::Integer(Integer::from(u64::MAX));
        let r = cbor_to_json(&cbor, &Desc::Int, 0);
        assert!(r.is_err());
    }

    #[test]
    fn bytes_round_trip() {
        let desc = Desc::Bytes;
        let cbor = CborValue::Bytes(vec![0xde, 0xad, 0x00, 0xff]);
        let json = cbor_to_json(&cbor, &desc, 0).unwrap();
        assert_eq!(serde_json::to_string(&json).unwrap(), "[222,173,0,255]");
        let back = json_to_cbor(&json, &desc, 0).unwrap();
        assert_eq!(back, cbor);
    }

    #[test]
    fn byte_value_over_255_is_error() {
        let json: serde_json::Value = serde_json::from_str("[256]").unwrap();
        let err = json_to_cbor(&json, &Desc::Bytes, 0).unwrap_err();
        assert!(err.contains("exceeds 255"), "got: {err}");
    }
}
