//! Hew `std::encoding::toml` — TOML parsing and generation.
//!
//! Provides TOML parsing, serialization, and value inspection for compiled
//! Hew programs. Text inputs borrow managed [`HewString`] handles and text
//! results transfer an independent managed owner. Null is the canonical
//! empty string. Opaque [`HewTomlValue`] handles must be freed with
//! [`hew_toml_free`].
use hew_cabi::string::{string_as_str, string_from_str, HewString};

/// Opaque wrapper around a [`toml::Value`].
///
/// Heap-allocated via `Box`; must be freed with [`hew_toml_free`].
#[derive(Debug)]
pub struct HewTomlValue {
    inner: toml::Value,
}

#[cfg(test)]
std::thread_local! {
    /// Test-only oracle for live outer `HewTomlValue` boxes. Nested TOML
    /// values move into their parent tree and carry no second wrapper.
    static LIVE_TOML_VALUE_BOXES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Wrap a [`toml::Value`] into a heap-allocated [`HewTomlValue`].
fn boxed_value(v: toml::Value) -> *mut HewTomlValue {
    #[cfg(test)]
    LIVE_TOML_VALUE_BOXES.with(|live| live.set(live.get() + 1));
    Box::into_raw(Box::new(HewTomlValue { inner: v }))
}

#[cfg(test)]
fn live_value_boxes() -> usize {
    LIVE_TOML_VALUE_BOXES.with(std::cell::Cell::get)
}

#[cfg(test)]
fn record_value_box_consumed() {
    LIVE_TOML_VALUE_BOXES.with(|live| {
        live.set(
            live.get()
                .checked_sub(1)
                .expect("TOML value box counter underflow"),
        );
    });
}

std::thread_local! {
    static LAST_SERIALIZE_ERROR: std::cell::RefCell<Option<String>> =
        const { std::cell::RefCell::new(None) };
}

fn set_serialize_last_error(msg: impl Into<String>) {
    LAST_SERIALIZE_ERROR.with(|slot| *slot.borrow_mut() = Some(msg.into()));
}

fn clear_serialize_last_error() {
    LAST_SERIALIZE_ERROR.with(|slot| *slot.borrow_mut() = None);
}

fn set_parse_last_error(msg: impl Into<String>) {
    hew_runtime::parse_error_slot::set_error(
        hew_runtime::parse_error_slot::ErrorSlotKind::Toml,
        msg,
    );
}

fn clear_parse_last_error() {
    hew_runtime::parse_error_slot::clear_error(hew_runtime::parse_error_slot::ErrorSlotKind::Toml);
}

fn get_parse_last_error() -> String {
    hew_runtime::parse_error_slot::get_error(hew_runtime::parse_error_slot::ErrorSlotKind::Toml)
        .unwrap_or_default()
}

/// Parse a TOML string into an opaque [`HewTomlValue`].
///
/// The canonical empty input (null) parses as a valid empty table, matching
/// TOML's own treatment of an empty document. Returns null on any other
/// parse error.
/// Call [`hew_toml_last_error`] to retrieve this actor's last TOML parse failure.
///
/// # Safety
///
/// `s` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_parse(s: *const HewString) -> *mut HewTomlValue {
    // SAFETY: the caller borrows a live managed string or canonical empty handle.
    let rust_str = unsafe { string_as_str(s) };
    match rust_str.parse::<toml::Table>() {
        Ok(table) => {
            clear_parse_last_error();
            boxed_value(toml::Value::Table(table))
        }
        Err(err) => {
            set_parse_last_error(err.to_string());
            std::ptr::null_mut()
        }
    }
}

/// Return the most recent parse error for this Hew actor.
///
/// Returns canonical empty (null) when no error is set.
///
/// Errors are keyed per (actor, parser-kind), so a different parser's success
/// does not clear this slot.
#[no_mangle]
pub extern "C" fn hew_toml_last_error() -> *mut HewString {
    string_from_str(&get_parse_last_error())
}

/// Return the type of a TOML value.
///
/// Type codes: 0 = null (reserved), 1 = boolean, 2 = integer, 3 = float,
/// 4 = string, 5 = array, 6 = table, 7 = datetime. Returns -1 on null input.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_type(val: *const HewTomlValue) -> i32 {
    if val.is_null() {
        return -1;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::Boolean(_) => 1,
        toml::Value::Integer(_) => 2,
        toml::Value::Float(_) => 3,
        toml::Value::String(_) => 4,
        toml::Value::Array(_) => 5,
        toml::Value::Table(_) => 6,
        toml::Value::Datetime(_) => 7,
    }
}

/// Returns an owned managed string. The caller must release it with
/// `hew_string_drop`. Returns canonical empty (null) for an empty string or a
/// value that is not a string; use [`hew_toml_type`] to distinguish.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_string(val: *const HewTomlValue) -> *mut HewString {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::String(s) => string_from_str(s),
        _ => std::ptr::null_mut(),
    }
}

/// Get the integer value from a TOML integer value.
///
/// Returns 0 if `val` is null or not an integer.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_int(val: *const HewTomlValue) -> i64 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::Integer(i) => *i,
        _ => 0,
    }
}

/// Get the float value from a TOML float value.
///
/// Returns 0.0 if `val` is null or not a float.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_float(val: *const HewTomlValue) -> f64 {
    if val.is_null() {
        return 0.0;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::Float(f) => *f,
        _ => 0.0,
    }
}

/// Get the boolean value from a TOML boolean value.
///
/// Returns 0 (false) or 1 (true). Returns 0 if `val` is null or not a boolean.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_bool(val: *const HewTomlValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::Boolean(b) => i32::from(*b),
        _ => 0,
    }
}

/// Look up a field in a TOML table by key.
///
/// Returns a new heap-allocated [`HewTomlValue`] (cloned) or null if the
/// key does not exist, `val` is null, or `val` is not a table.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `key` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_field(
    val: *const HewTomlValue,
    key: *const HewString,
) -> *mut HewTomlValue {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: key is null (canonical empty) or a live managed string handle.
    let key_str = unsafe { string_as_str(key) };
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    let toml::Value::Table(table) = &(unsafe { &*val }.inner) else {
        return std::ptr::null_mut();
    };
    match table.get(key_str) {
        Some(v) => boxed_value(v.clone()),
        None => std::ptr::null_mut(),
    }
}

/// Return the number of elements in a TOML array.
///
/// Returns -1 if `val` is null or not an array. The count crosses at `i64`
/// width: narrowing it to `i32` would have to saturate, and a saturated count
/// is a length the array does not have.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_len(val: *const HewTomlValue) -> i64 {
    if val.is_null() {
        return -1;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::Array(a) => i64::try_from(a.len()).unwrap_or(-1),
        _ => -1,
    }
}

/// Get an element from a TOML array by index.
///
/// Returns a new heap-allocated [`HewTomlValue`] (cloned) or null if out
/// of bounds, `val` is null, or `val` is not an array.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_get(
    val: *const HewTomlValue,
    index: i32,
) -> *mut HewTomlValue {
    if val.is_null() || index < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    let toml::Value::Array(arr) = &(unsafe { &*val }.inner) else {
        return std::ptr::null_mut();
    };
    #[expect(
        clippy::cast_sign_loss,
        reason = "C ABI: negative values checked before cast"
    )]
    match arr.get(index as usize) {
        Some(v) => boxed_value(v.clone()),
        None => std::ptr::null_mut(),
    }
}

/// Serialize a TOML value back to a TOML-formatted string.
///
/// Returns an owned managed string. The caller must release every non-null
/// result with `hew_string_drop`. Returns canonical empty (null) if `val` is
/// null or serialization fails, and records the reason in the serialize slot
/// so an empty document (a valid table with no keys) stays distinguishable
/// from a root TOML cannot represent — check [`hew_toml_last_serialize_error`],
/// not the nullness of this result.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_stringify(val: *const HewTomlValue) -> *mut HewString {
    if val.is_null() {
        set_serialize_last_error("toml: cannot serialize an invalid value");
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    let v = &unsafe { &*val }.inner;
    match toml::to_string(v) {
        Ok(s) => {
            clear_serialize_last_error();
            string_from_str(&s)
        }
        Err(err) => {
            set_serialize_last_error(format!("toml: {err}"));
            std::ptr::null_mut()
        }
    }
}

/// Return and clear the reason the most recent [`hew_toml_stringify`] call on
/// this thread failed, or canonical empty (null) if it succeeded.
///
/// A TOML document root must be a table. Serializing an integer, string, or
/// array root produces no text at all, which is exactly what an empty table
/// produces, so the reason slot is what separates the two.
#[no_mangle]
pub extern "C" fn hew_toml_last_serialize_error() -> *mut HewString {
    let message = LAST_SERIALIZE_ERROR.with(|slot| slot.borrow_mut().take());
    string_from_str(&message.unwrap_or_default())
}

// ---------------------------------------------------------------------------
// Table builder — typed field setters
// ---------------------------------------------------------------------------

/// Create a new empty TOML table.
///
/// Returns a heap-allocated [`HewTomlValue`] wrapping an empty table.
/// Must be freed with [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_table_new() -> *mut HewTomlValue {
    boxed_value(toml::Value::Table(toml::map::Map::new()))
}

/// Set a boolean field on a TOML table.
///
/// Does nothing if `tbl` is null or not a table. A null `key` is the
/// canonical empty key.
///
/// # Safety
///
/// `tbl` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `key` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set_bool(
    tbl: *mut HewTomlValue,
    key: *const HewString,
    val: i32,
) {
    if tbl.is_null() {
        return;
    }
    // SAFETY: key is null (canonical empty) or a live managed string handle.
    let key = unsafe { string_as_str(key) }.to_owned();
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, toml::Value::Boolean(val != 0));
    }
}

/// Set an integer field on a TOML table.
///
/// # Safety
///
/// Same as [`hew_toml_table_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set_int(
    tbl: *mut HewTomlValue,
    key: *const HewString,
    val: i64,
) {
    if tbl.is_null() {
        return;
    }
    // SAFETY: key is null (canonical empty) or a live managed string handle.
    let key = unsafe { string_as_str(key) }.to_owned();
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, toml::Value::Integer(val));
    }
}

/// Set a float field on a TOML table.
///
/// # Safety
///
/// Same as [`hew_toml_table_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set_float(
    tbl: *mut HewTomlValue,
    key: *const HewString,
    val: f64,
) {
    if tbl.is_null() {
        return;
    }
    // SAFETY: key is null (canonical empty) or a live managed string handle.
    let key = unsafe { string_as_str(key) }.to_owned();
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, toml::Value::Float(val));
    }
}

/// Set a string field on a TOML table. The string value is copied.
///
/// A null `val` sets the field to the empty string, not a no-op: a managed
/// empty string is indistinguishable from null.
///
/// # Safety
///
/// Same as [`hew_toml_table_set_bool`]. `val` must be null (canonical empty)
/// or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set_string(
    tbl: *mut HewTomlValue,
    key: *const HewString,
    val: *const HewString,
) {
    if tbl.is_null() {
        return;
    }
    // SAFETY: key is null (canonical empty) or a live managed string handle.
    let key = unsafe { string_as_str(key) }.to_owned();
    // SAFETY: val is null (canonical empty) or a live managed string handle.
    let val = unsafe { string_as_str(val) }.to_owned();
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, toml::Value::String(val));
    }
}

/// Set a [`HewTomlValue`] child on a TOML table, taking ownership.
///
/// The `val` pointer is consumed and must not be used or freed after this call.
/// A non-null `val` is consumed on every return path, including when `tbl` is
/// null or not a table. A null `val` is a no-op. A null `key` is the
/// canonical empty key.
///
/// # Safety
///
/// `tbl` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `key` must be null (canonical empty) or a live managed string handle.
/// `val` must be a pointer previously returned by a function in this module
/// and must not have been freed already (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set(
    tbl: *mut HewTomlValue,
    key: *const HewString,
    val: *mut HewTomlValue,
) {
    if val.is_null() {
        return;
    }
    // SAFETY: val was allocated with Box::into_raw and has not been freed.
    // Ownership transfers at the ABI boundary even if insertion is rejected.
    let child = unsafe { Box::from_raw(val) };
    #[cfg(test)]
    record_value_box_consumed();
    if tbl.is_null() {
        return;
    }
    // SAFETY: key is null (canonical empty) or a live managed string handle.
    let key = unsafe { string_as_str(key) }.to_owned();
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, child.inner);
    }
}

// ---------------------------------------------------------------------------
// Array builder — typed element pushers
// ---------------------------------------------------------------------------

/// Create a new empty TOML array.
///
/// Returns a heap-allocated [`HewTomlValue`] wrapping an empty array.
/// Must be freed with [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_array_new() -> *mut HewTomlValue {
    boxed_value(toml::Value::Array(Vec::new()))
}

/// Push a boolean onto a TOML array.
///
/// Does nothing if `arr` is null or not an array.
///
/// # Safety
///
/// `arr` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push_bool(arr: *mut HewTomlValue, val: i32) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(toml::Value::Boolean(val != 0));
    }
}

/// Push an integer onto a TOML array.
///
/// # Safety
///
/// Same as [`hew_toml_array_push_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push_int(arr: *mut HewTomlValue, val: i64) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(toml::Value::Integer(val));
    }
}

/// Push a float onto a TOML array.
///
/// # Safety
///
/// Same as [`hew_toml_array_push_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push_float(arr: *mut HewTomlValue, val: f64) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(toml::Value::Float(val));
    }
}

/// Push a string onto a TOML array. The string value is copied.
///
/// Does nothing if `arr` is null or not an array. A null `val` pushes the
/// empty string, not a no-op: a managed empty string is indistinguishable
/// from null.
///
/// # Safety
///
/// `arr` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `val` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push_string(arr: *mut HewTomlValue, val: *const HewString) {
    if arr.is_null() {
        return;
    }
    // SAFETY: val is null (canonical empty) or a live managed string handle.
    let val = unsafe { string_as_str(val) }.to_owned();
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(toml::Value::String(val));
    }
}

/// Push a [`HewTomlValue`] child onto a TOML array, taking ownership.
///
/// A non-null `val` is consumed on every return path and must not be used or
/// freed after this call, including when `arr` is null or not an array. A null
/// `val` is a no-op.
///
/// # Safety
///
/// `arr` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `val` must be a pointer previously returned by a function in this module
/// and must not have been freed already (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push(arr: *mut HewTomlValue, val: *mut HewTomlValue) {
    if val.is_null() {
        return;
    }
    // SAFETY: val was allocated with Box::into_raw and has not been freed.
    // Ownership transfers at the ABI boundary even if insertion is rejected.
    let child = unsafe { Box::from_raw(val) };
    #[cfg(test)]
    record_value_box_consumed();
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(child.inner);
    }
}

// ---------------------------------------------------------------------------
// Scalar constructors
// ---------------------------------------------------------------------------

/// Create a TOML boolean value.
///
/// Returns a heap-allocated [`HewTomlValue`]. Must be freed with
/// [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_from_bool(val: i32) -> *mut HewTomlValue {
    boxed_value(toml::Value::Boolean(val != 0))
}

/// Create a TOML integer value.
///
/// Returns a heap-allocated [`HewTomlValue`]. Must be freed with
/// [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_from_int(val: i64) -> *mut HewTomlValue {
    boxed_value(toml::Value::Integer(val))
}

/// Create a TOML float value.
///
/// Returns a heap-allocated [`HewTomlValue`]. Must be freed with
/// [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_from_float(val: f64) -> *mut HewTomlValue {
    boxed_value(toml::Value::Float(val))
}

/// Create a TOML string value. The string is copied.
///
/// A null `val` creates an empty string value. Must be freed with
/// [`hew_toml_free`].
///
/// # Safety
///
/// `val` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_from_string(val: *const HewString) -> *mut HewTomlValue {
    // SAFETY: val is null (canonical empty) or a live managed string handle.
    let s = unsafe { string_as_str(val) }.to_owned();
    boxed_value(toml::Value::String(s))
}

/// Free a [`HewTomlValue`] previously returned by any function in this
/// module.
///
/// # Safety
///
/// `val` must be a pointer previously returned by a function in this module
/// and must not have been freed already (or null, which is a no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_free(val: *mut HewTomlValue) {
    if val.is_null() {
        return;
    }
    // SAFETY: val was allocated with Box::into_raw and has not been freed.
    let _ = unsafe { Box::from_raw(val) };
    #[cfg(test)]
    record_value_box_consumed();
}

#[cfg(test)]
#[expect(
    clippy::approx_constant,
    reason = "test data uses hardcoded floats, not mathematical constants"
)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_release;

    /// Helper: read a managed string handle (or canonical empty) and release it.
    unsafe fn read_and_free_string(ptr: *mut HewString) -> String {
        // SAFETY: ptr is a live managed string owner or canonical empty.
        let s = unsafe { string_as_str(ptr) }.to_owned();
        // SAFETY: ptr is an owned managed result or canonical empty.
        unsafe { string_release(ptr) };
        s
    }

    #[test]
    fn child_transfer_is_unconditional_and_deep_clone_is_independent() {
        let baseline = live_value_boxes();
        let key = ManagedString::new("child");

        // Null/invalid parents and invalid keys still consume a non-null child.
        // SAFETY: every non-null handle below is freshly allocated and freed or
        // transferred exactly once; null pointers exercise the documented no-op path.
        unsafe {
            hew_toml_table_set(std::ptr::null_mut(), key.as_ptr(), hew_toml_from_int(1));
            assert_eq!(live_value_boxes(), baseline);

            let scalar_parent = hew_toml_from_int(2);
            hew_toml_table_set(scalar_parent, std::ptr::null(), hew_toml_from_int(3));
            assert_eq!(live_value_boxes(), baseline + 1);
            hew_toml_table_set(scalar_parent, key.as_ptr(), hew_toml_from_int(4));
            assert_eq!(live_value_boxes(), baseline + 1);
            hew_toml_free(scalar_parent);

            hew_toml_array_push(std::ptr::null_mut(), hew_toml_from_int(5));
            assert_eq!(live_value_boxes(), baseline);
            let table_parent = hew_toml_table_new();
            hew_toml_array_push(table_parent, hew_toml_from_int(6));
            assert_eq!(live_value_boxes(), baseline + 1);
            hew_toml_free(table_parent);
        }

        // A high nested tree has one outer wrapper after every successful
        // transfer. A cloned getter is a separate deep owner and remains valid
        // after the original root is released.
        let mut root = hew_toml_array_new();
        for _ in 0..128 {
            let parent = hew_toml_array_new();
            // SAFETY: parent and root are distinct live handles; ownership of root transfers.
            unsafe { hew_toml_array_push(parent, root) };
            root = parent;
            assert_eq!(live_value_boxes(), baseline + 1);
        }
        // SAFETY: root is a live nested array and index zero exists.
        let clone = unsafe { hew_toml_array_get(root, 0) };
        assert!(!clone.is_null());
        assert_eq!(live_value_boxes(), baseline + 2);
        // SAFETY: root remains live and has not otherwise been released.
        unsafe { hew_toml_free(root) };
        // SAFETY: clone is an independently allocated live value.
        assert_eq!(unsafe { hew_toml_type(clone) }, 5);
        // SAFETY: clone remains live and has not otherwise been released.
        unsafe { hew_toml_free(clone) };
        assert_eq!(live_value_boxes(), baseline);
    }

    #[test]
    fn test_parse_and_get_string() {
        let input = ManagedString::new("name = \"hew\"");
        // SAFETY: input is a live managed string handle.
        let root = unsafe { hew_toml_parse(input.as_ptr()) };
        assert!(!root.is_null());

        let key = ManagedString::new("name");
        // SAFETY: root is valid and key is a live managed string handle.
        let field = unsafe { hew_toml_get_field(root, key.as_ptr()) };
        assert!(!field.is_null());
        // SAFETY: field is valid.
        assert_eq!(unsafe { hew_toml_type(field) }, 4); // string

        // SAFETY: field is a string value.
        let result = unsafe { read_and_free_string(hew_toml_get_string(field)) };
        assert_eq!(result, "hew");

        // SAFETY: field and root were allocated by this module.
        unsafe {
            hew_toml_free(field);
            hew_toml_free(root);
        }
    }

    #[test]
    fn test_parse_numeric_types() {
        let input = ManagedString::new("port = 8080\npi = 3.14\nenabled = true");
        // SAFETY: input is a live managed string handle.
        let root = unsafe { hew_toml_parse(input.as_ptr()) };
        assert!(!root.is_null());

        let key_port = ManagedString::new("port");
        // SAFETY: root is valid and key_port is a live managed string handle.
        let port = unsafe { hew_toml_get_field(root, key_port.as_ptr()) };
        assert!(!port.is_null());
        // SAFETY: port is valid.
        assert_eq!(unsafe { hew_toml_type(port) }, 2); // integer
                                                       // SAFETY: port is a valid integer TOML value.
        assert_eq!(unsafe { hew_toml_get_int(port) }, 8080);

        let key_pi = ManagedString::new("pi");
        // SAFETY: root is valid and key_pi is a live managed string handle.
        let pi = unsafe { hew_toml_get_field(root, key_pi.as_ptr()) };
        assert!(!pi.is_null());
        // SAFETY: pi is valid.
        assert_eq!(unsafe { hew_toml_type(pi) }, 3); // float
                                                     // SAFETY: pi is a valid float TOML value.
        let pi_val = unsafe { hew_toml_get_float(pi) };
        assert!((pi_val - 3.14).abs() < f64::EPSILON);

        let key_en = ManagedString::new("enabled");
        // SAFETY: root is valid and key_en is a live managed string handle.
        let en = unsafe { hew_toml_get_field(root, key_en.as_ptr()) };
        assert!(!en.is_null());
        // SAFETY: en is valid.
        assert_eq!(unsafe { hew_toml_type(en) }, 1); // boolean
                                                     // SAFETY: en is a valid boolean TOML value.
        assert_eq!(unsafe { hew_toml_get_bool(en) }, 1);

        // SAFETY: all pointers were allocated by this module.
        unsafe {
            hew_toml_free(en);
            hew_toml_free(pi);
            hew_toml_free(port);
            hew_toml_free(root);
        }
    }

    #[test]
    fn test_array_access() {
        let input = ManagedString::new("ports = [80, 443, 8080]");
        // SAFETY: input is a live managed string handle.
        let root = unsafe { hew_toml_parse(input.as_ptr()) };
        assert!(!root.is_null());

        let key = ManagedString::new("ports");
        // SAFETY: root is valid and key is a live managed string handle.
        let arr = unsafe { hew_toml_get_field(root, key.as_ptr()) };
        assert!(!arr.is_null());
        // SAFETY: arr is valid.
        assert_eq!(unsafe { hew_toml_type(arr) }, 5); // array
                                                      // SAFETY: arr is a valid array TOML value.
        assert_eq!(unsafe { hew_toml_array_len(arr) }, 3);

        // SAFETY: arr is a valid array value.
        let elem = unsafe { hew_toml_array_get(arr, 1) };
        assert!(!elem.is_null());
        // SAFETY: elem is valid.
        assert_eq!(unsafe { hew_toml_get_int(elem) }, 443);

        // Out-of-bounds returns null.
        // SAFETY: arr is valid.
        let oob = unsafe { hew_toml_array_get(arr, 10) };
        assert!(oob.is_null());

        // SAFETY: all pointers were allocated by this module.
        unsafe {
            hew_toml_free(elem);
            hew_toml_free(arr);
            hew_toml_free(root);
        }
    }

    #[test]
    fn test_null_inputs() {
        // All functions must handle null gracefully. Null is the canonical
        // empty managed string on every string slot, so a null TOML source
        // parses as a valid empty document rather than an error.
        // SAFETY: testing null handling.
        unsafe {
            let root = hew_toml_parse(std::ptr::null());
            assert!(!root.is_null());
            assert_eq!(hew_toml_type(root), 6); // empty table
            assert!(hew_toml_last_error().is_null());
            hew_toml_free(root);

            assert_eq!(hew_toml_type(std::ptr::null()), -1);
            assert!(hew_toml_get_string(std::ptr::null()).is_null());
            assert_eq!(hew_toml_get_int(std::ptr::null()), 0);
            assert!((hew_toml_get_float(std::ptr::null())).abs() < f64::EPSILON);
            assert_eq!(hew_toml_get_bool(std::ptr::null()), 0);
            assert!(hew_toml_get_field(std::ptr::null(), std::ptr::null()).is_null());
            assert_eq!(hew_toml_array_len(std::ptr::null()), -1);
            assert!(hew_toml_array_get(std::ptr::null(), 0).is_null());
            // A null root is not serializable, so the reason slot is what
            // distinguishes it from a valid empty table (also canonical empty).
            assert!(hew_toml_stringify(std::ptr::null()).is_null());
            assert_eq!(
                read_and_free_string(hew_toml_last_serialize_error()),
                "toml: cannot serialize an invalid value"
            );
            hew_toml_free(std::ptr::null_mut()); // must not crash
        }
    }

    #[test]
    fn test_parse_failure_sets_last_error() {
        let input = ManagedString::new("not = [valid toml");
        // SAFETY: input is a live managed string handle for the TOML parser.
        let root = unsafe { hew_toml_parse(input.as_ptr()) };
        assert!(root.is_null());

        // SAFETY: hew_toml_last_error returns an owned managed string or canonical empty.
        let err = unsafe { read_and_free_string(hew_toml_last_error()) };
        assert!(!err.is_empty());
    }

    #[test]
    fn test_parse_success_clears_last_error() {
        let bad_input = ManagedString::new("not = [valid toml");
        // SAFETY: bad_input is a live managed string handle for the TOML parser.
        assert!(unsafe { hew_toml_parse(bad_input.as_ptr()) }.is_null());

        let good_input = ManagedString::new("key = \"value\"");
        // SAFETY: good_input is a live managed string handle for the TOML parser.
        let root = unsafe { hew_toml_parse(good_input.as_ptr()) };
        assert!(!root.is_null());

        // SAFETY: hew_toml_last_error returns an owned managed string or canonical empty.
        let err = unsafe { read_and_free_string(hew_toml_last_error()) };
        assert!(err.is_empty());

        // SAFETY: root was allocated by this module.
        unsafe { hew_toml_free(root) };
    }

    #[test]
    fn test_stringify_roundtrip() {
        let input = ManagedString::new("key = \"value\"");
        // SAFETY: input is a live managed string handle.
        let root = unsafe { hew_toml_parse(input.as_ptr()) };
        assert!(!root.is_null());

        // SAFETY: root is valid.
        let roundtrip = unsafe { read_and_free_string(hew_toml_stringify(root)) };
        assert!(roundtrip.contains("key"));
        assert!(roundtrip.contains("value"));

        // SAFETY: root was allocated by this module.
        unsafe { hew_toml_free(root) };
    }

    // -----------------------------------------------------------------------
    // Builder tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_table_builder_typed_setters() {
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let tbl = hew_toml_table_new();
            assert!(!tbl.is_null());
            assert_eq!(hew_toml_type(tbl), 6); // table

            let k_name = ManagedString::new("name");
            let v_name = ManagedString::new("hew");
            hew_toml_table_set_string(tbl, k_name.as_ptr(), v_name.as_ptr());

            let k_port = ManagedString::new("port");
            hew_toml_table_set_int(tbl, k_port.as_ptr(), 8080);

            let k_pi = ManagedString::new("pi");
            hew_toml_table_set_float(tbl, k_pi.as_ptr(), 3.14);

            let k_en = ManagedString::new("enabled");
            hew_toml_table_set_bool(tbl, k_en.as_ptr(), 1);

            // Read back via getter API.
            let name = hew_toml_get_field(tbl, k_name.as_ptr());
            assert!(!name.is_null());
            assert_eq!(read_and_free_string(hew_toml_get_string(name)), "hew");
            hew_toml_free(name);

            let port = hew_toml_get_field(tbl, k_port.as_ptr());
            assert!(!port.is_null());
            assert_eq!(hew_toml_get_int(port), 8080);
            hew_toml_free(port);

            let pi = hew_toml_get_field(tbl, k_pi.as_ptr());
            assert!(!pi.is_null());
            assert!((hew_toml_get_float(pi) - 3.14).abs() < f64::EPSILON);
            hew_toml_free(pi);

            let en = hew_toml_get_field(tbl, k_en.as_ptr());
            assert!(!en.is_null());
            assert_eq!(hew_toml_get_bool(en), 1);
            hew_toml_free(en);

            hew_toml_free(tbl);
        }
    }

    #[test]
    fn test_table_set_value_child() {
        // Build a nested structure: { server = { host = "localhost", port = 9090 } }
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let inner = hew_toml_table_new();
            let k_host = ManagedString::new("host");
            let v_host = ManagedString::new("localhost");
            hew_toml_table_set_string(inner, k_host.as_ptr(), v_host.as_ptr());

            let k_port = ManagedString::new("port");
            hew_toml_table_set_int(inner, k_port.as_ptr(), 9090);

            let outer = hew_toml_table_new();
            let k_server = ManagedString::new("server");
            // Takes ownership of inner — do not free inner after this.
            hew_toml_table_set(outer, k_server.as_ptr(), inner);

            // Verify nested access.
            let server = hew_toml_get_field(outer, k_server.as_ptr());
            assert!(!server.is_null());
            assert_eq!(hew_toml_type(server), 6); // table

            let host = hew_toml_get_field(server, k_host.as_ptr());
            assert!(!host.is_null());
            assert_eq!(read_and_free_string(hew_toml_get_string(host)), "localhost");
            hew_toml_free(host);

            let port = hew_toml_get_field(server, k_port.as_ptr());
            assert!(!port.is_null());
            assert_eq!(hew_toml_get_int(port), 9090);
            hew_toml_free(port);

            hew_toml_free(server);
            hew_toml_free(outer);
        }
    }

    #[test]
    fn test_array_builder_typed_pushers() {
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let arr = hew_toml_array_new();
            assert!(!arr.is_null());
            assert_eq!(hew_toml_type(arr), 5); // array
            assert_eq!(hew_toml_array_len(arr), 0);

            hew_toml_array_push_int(arr, 10);
            hew_toml_array_push_int(arr, 20);
            hew_toml_array_push_int(arr, 30);
            assert_eq!(hew_toml_array_len(arr), 3);

            let elem = hew_toml_array_get(arr, 1);
            assert!(!elem.is_null());
            assert_eq!(hew_toml_get_int(elem), 20);
            hew_toml_free(elem);

            hew_toml_free(arr);
        }
    }

    #[test]
    fn test_array_push_mixed_scalars() {
        // TOML arrays are homogeneous in spec, but toml::Value allows mixed.
        // We test push_bool, push_float, push_string independently.
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let bools = hew_toml_array_new();
            hew_toml_array_push_bool(bools, 1);
            hew_toml_array_push_bool(bools, 0);
            assert_eq!(hew_toml_array_len(bools), 2);
            let b0 = hew_toml_array_get(bools, 0);
            assert_eq!(hew_toml_get_bool(b0), 1);
            hew_toml_free(b0);
            let b1 = hew_toml_array_get(bools, 1);
            assert_eq!(hew_toml_get_bool(b1), 0);
            hew_toml_free(b1);
            hew_toml_free(bools);

            let floats = hew_toml_array_new();
            hew_toml_array_push_float(floats, 1.5);
            hew_toml_array_push_float(floats, 2.5);
            assert_eq!(hew_toml_array_len(floats), 2);
            let f0 = hew_toml_array_get(floats, 0);
            assert!((hew_toml_get_float(f0) - 1.5).abs() < f64::EPSILON);
            hew_toml_free(f0);
            hew_toml_free(floats);

            let strings = hew_toml_array_new();
            let s_a = ManagedString::new("alpha");
            let s_b = ManagedString::new("beta");
            hew_toml_array_push_string(strings, s_a.as_ptr());
            hew_toml_array_push_string(strings, s_b.as_ptr());
            assert_eq!(hew_toml_array_len(strings), 2);
            let e1 = hew_toml_array_get(strings, 1);
            assert_eq!(read_and_free_string(hew_toml_get_string(e1)), "beta");
            hew_toml_free(e1);
            hew_toml_free(strings);
        }
    }

    #[test]
    fn test_array_push_value_child() {
        // Push sub-tables into an array: [[servers]] equivalent.
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let arr = hew_toml_array_new();

            let tbl1 = hew_toml_table_new();
            let k = ManagedString::new("host");
            let v = ManagedString::new("alpha");
            hew_toml_table_set_string(tbl1, k.as_ptr(), v.as_ptr());
            hew_toml_array_push(arr, tbl1); // takes ownership

            let tbl2 = hew_toml_table_new();
            let v2 = ManagedString::new("beta");
            hew_toml_table_set_string(tbl2, k.as_ptr(), v2.as_ptr());
            hew_toml_array_push(arr, tbl2); // takes ownership

            assert_eq!(hew_toml_array_len(arr), 2);

            let elem = hew_toml_array_get(arr, 0);
            assert_eq!(hew_toml_type(elem), 6); // table
            let host = hew_toml_get_field(elem, k.as_ptr());
            assert_eq!(read_and_free_string(hew_toml_get_string(host)), "alpha");
            hew_toml_free(host);
            hew_toml_free(elem);

            hew_toml_free(arr);
        }
    }

    #[test]
    fn test_scalar_constructors() {
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let b = hew_toml_from_bool(1);
            assert!(!b.is_null());
            assert_eq!(hew_toml_type(b), 1); // boolean
            assert_eq!(hew_toml_get_bool(b), 1);
            hew_toml_free(b);

            let b_false = hew_toml_from_bool(0);
            assert_eq!(hew_toml_get_bool(b_false), 0);
            hew_toml_free(b_false);

            let i = hew_toml_from_int(42);
            assert!(!i.is_null());
            assert_eq!(hew_toml_type(i), 2); // integer
            assert_eq!(hew_toml_get_int(i), 42);
            hew_toml_free(i);

            let f = hew_toml_from_float(2.718);
            assert!(!f.is_null());
            assert_eq!(hew_toml_type(f), 3); // float
            assert!((hew_toml_get_float(f) - 2.718).abs() < f64::EPSILON);
            hew_toml_free(f);

            let cs = ManagedString::new("hello");
            let s = hew_toml_from_string(cs.as_ptr());
            assert!(!s.is_null());
            assert_eq!(hew_toml_type(s), 4); // string
            assert_eq!(read_and_free_string(hew_toml_get_string(s)), "hello");
            hew_toml_free(s);

            // A null text handle creates an empty string value, not a null Value:
            // a managed empty string is indistinguishable from null.
            let empty = hew_toml_from_string(std::ptr::null());
            assert!(!empty.is_null());
            assert_eq!(hew_toml_type(empty), 4); // string
            assert!(hew_toml_get_string(empty).is_null());
            hew_toml_free(empty);
        }
    }

    #[test]
    fn test_builder_stringify_roundtrip() {
        // Build { name = "hew", version = 1, tags = ["fast", "safe"] }
        // then stringify and re-parse.
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let tbl = hew_toml_table_new();

            let k_name = ManagedString::new("name");
            let v_name = ManagedString::new("hew");
            hew_toml_table_set_string(tbl, k_name.as_ptr(), v_name.as_ptr());

            let k_ver = ManagedString::new("version");
            hew_toml_table_set_int(tbl, k_ver.as_ptr(), 1);

            let tags = hew_toml_array_new();
            let t1 = ManagedString::new("fast");
            let t2 = ManagedString::new("safe");
            hew_toml_array_push_string(tags, t1.as_ptr());
            hew_toml_array_push_string(tags, t2.as_ptr());

            let k_tags = ManagedString::new("tags");
            hew_toml_table_set(tbl, k_tags.as_ptr(), tags); // takes ownership

            // Stringify.
            let toml_str = read_and_free_string(hew_toml_stringify(tbl));
            assert!(toml_str.contains("name"));
            assert!(toml_str.contains("hew"));
            assert!(toml_str.contains("version"));

            // Re-parse the stringified output.
            let reparsed_input = ManagedString::new(&toml_str);
            let reparsed = hew_toml_parse(reparsed_input.as_ptr());
            assert!(!reparsed.is_null());

            let name = hew_toml_get_field(reparsed, k_name.as_ptr());
            assert!(!name.is_null());
            assert_eq!(read_and_free_string(hew_toml_get_string(name)), "hew");
            hew_toml_free(name);

            let ver = hew_toml_get_field(reparsed, k_ver.as_ptr());
            assert!(!ver.is_null());
            assert_eq!(hew_toml_get_int(ver), 1);
            hew_toml_free(ver);

            let tags_field = hew_toml_get_field(reparsed, k_tags.as_ptr());
            assert!(!tags_field.is_null());
            assert_eq!(hew_toml_array_len(tags_field), 2);
            let first_tag = hew_toml_array_get(tags_field, 0);
            assert_eq!(read_and_free_string(hew_toml_get_string(first_tag)), "fast");
            hew_toml_free(first_tag);
            hew_toml_free(tags_field);

            hew_toml_free(reparsed);
            hew_toml_free(tbl);
        }
    }

    #[test]
    fn test_builder_null_safety() {
        // All builder functions must handle null gracefully. Null string
        // arguments are the canonical empty string, not a no-op sentinel: a
        // null key targets the empty-string key and a null value sets or
        // pushes an empty string value.
        // SAFETY: testing null handling.
        unsafe {
            // Table setters with null table are no-ops.
            let k = ManagedString::new("k");
            let v = ManagedString::new("v");
            hew_toml_table_set_bool(std::ptr::null_mut(), k.as_ptr(), 1);
            hew_toml_table_set_int(std::ptr::null_mut(), k.as_ptr(), 1);
            hew_toml_table_set_float(std::ptr::null_mut(), k.as_ptr(), 1.0);
            hew_toml_table_set_string(std::ptr::null_mut(), k.as_ptr(), v.as_ptr());
            hew_toml_table_set(std::ptr::null_mut(), k.as_ptr(), std::ptr::null_mut());

            // A null key targets the empty-string key rather than a no-op.
            let tbl = hew_toml_table_new();
            hew_toml_table_set_bool(tbl, std::ptr::null(), 1);
            hew_toml_table_set_int(tbl, std::ptr::null(), 1);
            hew_toml_table_set_float(tbl, std::ptr::null(), 1.0);
            hew_toml_table_set_string(tbl, std::ptr::null(), v.as_ptr());
            // A null child value remains a no-op even with a null key.
            hew_toml_table_set(tbl, std::ptr::null(), std::ptr::null_mut());
            let empty_key_field = hew_toml_get_field(tbl, std::ptr::null());
            assert!(!empty_key_field.is_null());
            assert_eq!(hew_toml_type(empty_key_field), 4); // string, from the last successful set
            assert_eq!(
                read_and_free_string(hew_toml_get_string(empty_key_field)),
                "v"
            );
            hew_toml_free(empty_key_field);
            hew_toml_free(tbl);

            // Array pushers with null array.
            hew_toml_array_push_bool(std::ptr::null_mut(), 1);
            hew_toml_array_push_int(std::ptr::null_mut(), 1);
            hew_toml_array_push_float(std::ptr::null_mut(), 1.0);
            hew_toml_array_push_string(std::ptr::null_mut(), v.as_ptr());
            hew_toml_array_push(std::ptr::null_mut(), std::ptr::null_mut());

            // A null string pushes the empty string, not a no-op.
            let arr = hew_toml_array_new();
            hew_toml_array_push_string(arr, std::ptr::null());
            assert_eq!(hew_toml_array_len(arr), 1);
            let elem = hew_toml_array_get(arr, 0);
            assert!(hew_toml_get_string(elem).is_null()); // empty string reads as canonical empty
            hew_toml_free(elem);
            hew_toml_free(arr);
        }
    }

    #[test]
    fn runtime_type_tags_match_format_wrapper() {
        // Keep the runtime tags used by the TOML wrapper stable.
        // SAFETY: every handle below is allocated by boxed_value and freed once
        // by hew_toml_free in the same scope.
        unsafe {
            let bool_val = boxed_value(toml::Value::Boolean(true));
            assert_eq!(hew_toml_type(bool_val), 1);
            hew_toml_free(bool_val);

            let int_val = boxed_value(toml::Value::Integer(42));
            assert_eq!(hew_toml_type(int_val), 2);
            hew_toml_free(int_val);

            let float_val = boxed_value(toml::Value::Float(3.25));
            assert_eq!(hew_toml_type(float_val), 3);
            hew_toml_free(float_val);

            let string_val = boxed_value(toml::Value::String("hew".to_owned()));
            assert_eq!(hew_toml_type(string_val), 4);
            hew_toml_free(string_val);

            let array_val = boxed_value(toml::Value::Array(Vec::new()));
            assert_eq!(hew_toml_type(array_val), 5);
            hew_toml_free(array_val);

            let object_val = boxed_value(toml::Value::Table(toml::Table::new()));
            assert_eq!(hew_toml_type(object_val), 6);
            hew_toml_free(object_val);
        }
    }

    // ── Cross-thread / cross-actor regression (#1420) ────────────────────────

    /// Regression test for issue #1420: `LAST_PARSE_ERROR` thread-locals drifted
    /// across actor migrations.
    ///
    /// Scenario: actor migration between `parse()` and `last_error()`.
    ///
    /// Thread A writes a parse error for actor-id X via the shared slot helper
    /// (the same internal path taken by `hew_toml_parse` on failure when
    /// `hew_actor_current_id()` returns X).
    ///
    /// Thread B reads it back via `hew_toml_last_error()` as if it were
    /// continuing actor X's execution on a different worker thread.
    ///
    /// With the old TLS implementation, thread B would get an empty string.
    /// With the actor-keyed map, thread B gets the correct error string.
    ///
    /// A `Barrier` provides the only ordering guarantee — no sleeps.
    ///
    /// Run 3× to satisfy the "flake gate" requirement.
    #[test]
    fn parse_error_visible_across_worker_threads_regression_1420() {
        for run in 0..3_u32 {
            const ACTOR_ID: u64 = 777_001;

            // Barrier: thread B waits until thread A writes, then reads.
            let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
            let barrier2 = barrier.clone();

            let handle = std::thread::spawn(move || {
                // Simulate: actor ACTOR_ID resumes on thread B (different OS thread).
                // Wait until thread A has written the parse error.
                barrier2.wait();

                // Read the error via the actual FFI function.  With the old TLS
                // this returned an empty string; with the actor-keyed map it
                // returns the error written on thread A.
                //
                // We call the slot helper directly as the stand-in for
                // hew_actor_current_id() returning ACTOR_ID, because we cannot
                // inject actor context from a unit test without a full scheduler.
                hew_runtime::parse_error_slot::__get_error_for_actor(
                    ACTOR_ID,
                    hew_runtime::parse_error_slot::ErrorSlotKind::Toml,
                )
            });

            // Thread A: write a parse error as if actor ACTOR_ID called hew_toml_parse
            // on bad input.
            hew_runtime::parse_error_slot::__set_error_for_actor(
                ACTOR_ID,
                hew_runtime::parse_error_slot::ErrorSlotKind::Toml,
                "invalid TOML: actor-migration regression",
            );
            barrier.wait();

            let result = handle.join().expect("thread B panicked");
            assert_eq!(
                result.as_deref(),
                Some("invalid TOML: actor-migration regression"),
                "run {run}: error written on thread A must be visible on thread B for same actor"
            );

            // Clean up so runs don't interfere.
            hew_runtime::parse_error_slot::__clear_error_for_actor(
                ACTOR_ID,
                hew_runtime::parse_error_slot::ErrorSlotKind::Toml,
            );
        }
    }
}
