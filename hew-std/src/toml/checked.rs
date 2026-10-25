//! Checked allocation boundaries for owned encoding results.

use super::*;
use hew_cabi::value::HewValueLayout;

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_clone`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_clone_checked(
    val: *const HewTomlValue,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value =
        hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe { hew_toml_clone(val) });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_parse`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_parse_checked(
    s: *const HewString,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value =
        hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe { hew_toml_parse(s) });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_last_error`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_last_error_checked(
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewString {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_toml_last_error());
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            string_release(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_get_string`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_string_checked(
    val: *const HewTomlValue,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewString {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_toml_get_string(val)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            string_release(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_get_datetime`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_datetime_checked(
    val: *const HewTomlValue,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewString {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_toml_get_datetime(val)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            string_release(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_get_field`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_field_checked(
    val: *const HewTomlValue,
    key: *const HewString,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_toml_get_field(val, key)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_object_keys`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_object_keys_checked(
    val: *const HewTomlValue,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_toml_object_keys(val)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_array_get`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_get_checked(
    val: *const HewTomlValue,
    index: i32,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_toml_array_get(val, index)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_stringify`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_stringify_checked(
    val: *const HewTomlValue,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewString {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_toml_stringify(val)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            string_release(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_object_new`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_object_new_checked(
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_toml_object_new());
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_array_new`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_new_checked(
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_toml_array_new());
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_from_bool`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_from_bool_checked(
    val: i32,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value =
        hew_runtime::owned_value_budget::with_value_owner(&None, || hew_toml_from_bool(val));
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_from_int`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_from_int_checked(
    val: i64,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_toml_from_int(val));
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_from_float`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_from_float_checked(
    val: f64,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value =
        hew_runtime::owned_value_budget::with_value_owner(&None, || hew_toml_from_float(val));
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_toml_from_string`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_from_string_checked(
    val: *const HewString,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewTomlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_toml_from_string(val)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_toml_free(value);
        })
    }
}

use hew_cabi::managed::HEAP_EXCEEDED;
use hew_cabi::string::string_release;
use hew_runtime::owned_value_budget::{current_owner, ManagedStorage};
use toml::Value;

fn payload(value: &Value) -> Option<u64> {
    hew_codec::managed_tree::toml_payload_bytes(value, false)
}

// Capacity planning borrows both inputs until admission commits ownership.
fn plan(
    value: &Value,
    key: Option<&str>,
    old_bytes: Option<u64>,
    child_bytes: Option<u64>,
) -> Option<(u64, Option<Vec<Value>>)> {
    let mut growth = None;
    let planned = (|| {
        let old_bytes = old_bytes?;
        let child_bytes = child_bytes?;
        match (value, key) {
            (Value::Table(values), Some(key)) => match values.get(key) {
                Some(old) => old_bytes
                    .checked_sub(payload(old)?)?
                    .checked_add(child_bytes),
                None => old_bytes
                    .checked_add(8)?
                    .checked_add(u64::try_from(key.len()).ok()?)?
                    .checked_add(child_bytes),
            },
            (Value::Array(values), None) => {
                growth = crate::managed_tree::array_growth(values).ok()?;
                let capacity = growth.as_ref().map_or(values.capacity(), Vec::capacity);
                old_bytes
                    .checked_add(
                        u64::try_from(capacity.checked_sub(values.capacity())?)
                            .ok()?
                            .checked_mul(20)?,
                    )?
                    .checked_add(child_bytes.checked_sub(20)?)
            }
            _ => unreachable!(),
        }
    })();
    Some((planned?, growth))
}

unsafe fn edit(
    destination: *mut HewTomlValue,
    key: Option<&str>,
    child_pointer: *mut HewTomlValue,
    status: *mut i32,
) {
    // SAFETY: checked calls supply a writable status slot and live handles.
    unsafe { status.write(0) };
    let owner = current_owner();
    // SAFETY: the child handle stays borrowed until admission commits it.
    let Some(child_handle) = (unsafe { child_pointer.as_ref() }) else {
        return;
    };
    let child_value = &child_handle.inner;
    // SAFETY: the receiver is borrowed until quota admission.
    let destination_value = unsafe { destination.as_ref() };
    let valid = destination_value.is_some_and(|value| {
        matches!(
            (&value.inner, key),
            (Value::Table(_), Some(_)) | (Value::Array(_), None)
        )
    });
    if !valid {
        if !child_pointer.is_null() {
            if let Some(owner) = owner {
                owner.release(&[ManagedStorage {
                    identity: child_pointer as usize,
                    bytes: payload(child_value).expect("live encoding payload"),
                }]);
            }
            // SAFETY: successful no-op calls consume their owned child too.
            unsafe { hew_toml_free(child_pointer) };
        }
        return;
    }
    let destination_value = destination_value.expect("validated receiver");
    let old_bytes = payload(&destination_value.inner);
    let child_bytes = payload(child_value);
    let Some((planned, growth)) = plan(&destination_value.inner, key, old_bytes, child_bytes)
    else {
        // SAFETY: no live value changes when capacity cannot be represented.
        unsafe { status.write(HEAP_EXCEEDED) };
        return;
    };
    let mut retired = vec![(
        ManagedStorage {
            identity: destination as usize,
            bytes: old_bytes.unwrap_or(0),
        },
        true,
    )];
    if !child_pointer.is_null() {
        retired.push((
            ManagedStorage {
                identity: child_pointer as usize,
                bytes: child_bytes.unwrap_or(0),
            },
            true,
        ));
    }
    let permit = match crate::managed_tree::replacement(owner.as_ref(), &retired, Some(planned)) {
        Ok(permit) => permit,
        Err(failure) => {
            // SAFETY: refusal leaves both the receiver and owned child intact.
            unsafe { status.write(failure) };
            return;
        }
    };
    // SAFETY: admission transfers the unique outer child box.
    let child = unsafe { Box::from_raw(child_pointer) };
    #[cfg(test)]
    record_value_box_consumed();
    let child_value = child.inner;
    // SAFETY: checked source calls hold exclusive mutable receiver access.
    match (&mut unsafe { &mut *destination }.inner, key) {
        (Value::Table(values), Some(key)) => {
            values.insert(key.to_owned(), child_value);
        }
        (Value::Array(values), None) => crate::managed_tree::push(values, growth, child_value),
        _ => unreachable!(),
    }
    if let Some(permit) = permit {
        permit.commit(destination as usize);
    }
}

/// Insert a child only after admitting the complete replacement tree.
///
/// # Safety
/// Inputs meet [`hew_toml_object_set`]'s contract; `status` is writable.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_object_set_checked(
    obj: *mut HewTomlValue,
    key: *const HewString,
    val: *mut HewTomlValue,
    status: *mut i32,
) {
    // SAFETY: borrowed key and owned input stay live until edit commits.
    unsafe { edit(obj, Some(string_as_str(key)), val, status) };
}

/// Append a child only after admitting the complete replacement tree.
///
/// # Safety
/// Inputs meet [`hew_toml_array_push`]'s contract; `status` is writable.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push_checked(
    arr: *mut HewTomlValue,
    val: *mut HewTomlValue,
    status: *mut i32,
) {
    // SAFETY: borrowed receiver and owned input stay live until edit commits.
    unsafe { edit(arr, None, val, status) };
}

/// Release one semantic tree interest before physically freeing its handle.
///
/// # Safety
/// `val` is null or an owned live handle in the current retained account.
#[no_mangle]
pub unsafe extern "C" fn hew_toml_free_managed(val: *mut HewTomlValue) {
    // SAFETY: the semantic caller retains the handle through its leaf release.
    if let (Some(value), Some(owner)) = (unsafe { val.as_ref() }, current_owner()) {
        owner.release(&[ManagedStorage {
            identity: val as usize,
            bytes: payload(&value.inner).expect("owned encoding payload"),
        }]);
    }
    hew_runtime::owned_value_budget::with_value_owner(&None, || {
        // SAFETY: accounting has ended; this frees only the physical owner.
        unsafe { hew_toml_free(val) };
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::managed::HewManagedValueDescriptor;
    use hew_runtime::owned_value_budget::{with_value_owner, ValueOwner};

    fn layout() -> HewValueLayout {
        static MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: Some(hew_managed_visit_toml),
        };
        crate::managed_tree::tests::layout(&MANAGED)
    }

    unsafe fn release(value: *mut HewTomlValue) {
        // SAFETY: this acquired handle belongs to the test's current account.
        unsafe { hew_toml_free_managed(value) };
    }

    #[test]
    fn checked_constructor_refuses_without_publishing_or_leaking() {
        let before = live_value_boxes();
        let owner = ValueOwner::new(Some(20 - 1));
        let layout = layout();
        let mut status = -1;
        with_value_owner(&Some(owner.clone()), || {
            // SAFETY: live descriptor and writable status cover the call.
            let result = unsafe { hew_toml_object_new_checked(&raw const layout, &raw mut status) };
            assert!(result.is_null());
            assert_eq!(status, HEAP_EXCEEDED);
            assert_eq!(owner.live_bytes(), 0);
        });
        assert_eq!(live_value_boxes(), before);
    }

    #[test]
    fn checked_object_edit_moves_child_and_rolls_back_refusal() {
        let layout = layout();
        let key = string_from_str("k");
        for limit in [49 - 1, 49] {
            let owner = ValueOwner::new(Some(limit));
            with_value_owner(&Some(owner.clone()), || {
                let mut status = -1;
                // SAFETY: each handle is uniquely retained until edit or release.
                unsafe {
                    let object = hew_toml_object_new_checked(&raw const layout, &raw mut status);
                    assert_eq!(status, 0);
                    let child = hew_toml_from_int_checked(42, &raw const layout, &raw mut status);
                    assert_eq!(status, 0);
                    assert_eq!(owner.live_bytes(), 20 * 2);
                    hew_toml_object_set_checked(object, key, child, &raw mut status);
                    if limit == 49 {
                        assert_eq!(status, 0);
                        assert_eq!(owner.live_bytes(), 49);
                        let field = hew_toml_get_field(object, key);
                        assert_eq!(hew_toml_get_int(field), 42);
                        hew_toml_free(field);
                    } else {
                        assert_eq!(status, HEAP_EXCEEDED);
                        assert!(hew_toml_get_field(object, key).is_null());
                        assert_eq!(hew_toml_get_int(child), 42);
                        assert_eq!(owner.live_bytes(), 20 * 2);
                        release(child);
                    }
                    release(object);
                    assert_eq!(owner.live_bytes(), 0);
                }
            });
        }
        // SAFETY: this unbounded test-owned input remains live through both calls.
        unsafe { string_release(key) };
    }

    #[test]
    fn checked_array_growth_preserves_inputs_on_refusal() {
        let layout = layout();
        // The first array reserves four canonical slots, plus its root.
        for limit in [20 * 5 - 1, 20 * 5] {
            let owner = ValueOwner::new(Some(limit));
            with_value_owner(&Some(owner.clone()), || {
                let mut status = -1;
                // SAFETY: acquired handles retain their ownership until commit.
                unsafe {
                    let array = hew_toml_array_new_checked(&raw const layout, &raw mut status);
                    let child = hew_toml_from_int_checked(7, &raw const layout, &raw mut status);
                    assert_eq!(status, 0);
                    hew_toml_array_push_checked(array, child, &raw mut status);
                    if limit == 20 * 5 {
                        assert_eq!(status, 0);
                        assert_eq!(hew_toml_array_len(array), 1);
                        assert_eq!(owner.live_bytes(), 20 * 5);
                    } else {
                        assert_eq!(status, HEAP_EXCEEDED);
                        assert_eq!(hew_toml_array_len(array), 0);
                        assert_eq!(hew_toml_get_int(child), 7);
                        release(child);
                    }
                    release(array);
                    assert_eq!(owner.live_bytes(), 0);
                }
            });
        }
    }
}
