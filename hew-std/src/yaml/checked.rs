//! Checked allocation boundaries for owned encoding results.

use super::*;
use hew_cabi::value::HewValueLayout;

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_clone`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_clone_checked(
    val: *const HewYamlValue,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value =
        hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe { hew_yaml_clone(val) });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_parse`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_parse_checked(
    yaml_str: *const HewString,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_yaml_parse(yaml_str)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_last_error`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_last_error_checked(
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewString {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_yaml_last_error());
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
/// Inputs meet [`hew_yaml_stringify`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_stringify_checked(
    val: *const HewYamlValue,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewString {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_yaml_stringify(val)
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
/// Inputs meet [`hew_yaml_get_string`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_string_checked(
    val: *const HewYamlValue,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewString {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_yaml_get_string(val)
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
/// Inputs meet [`hew_yaml_get_field`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_field_checked(
    val: *const HewYamlValue,
    key: *const HewString,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_yaml_get_field(val, key)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_keys`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_keys_checked(
    val: *const HewYamlValue,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_yaml_object_keys(val)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_array_get`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_get_checked(
    val: *const HewYamlValue,
    index: i32,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_yaml_array_get(val, index)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_new`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_new_checked(
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_yaml_object_new());
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_array_new`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_new_checked(
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_yaml_array_new());
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_from_bool`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_from_bool_checked(
    val: i32,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value =
        hew_runtime::owned_value_budget::with_value_owner(&None, || hew_yaml_from_bool(val));
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_from_int`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_from_int_checked(
    val: i64,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_yaml_from_int(val));
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_from_u64`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_from_u64_checked(
    val: u64,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_yaml_from_u64(val));
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_from_float`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_from_float_checked(
    val: f64,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value =
        hew_runtime::owned_value_budget::with_value_owner(&None, || hew_yaml_from_float(val));
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_from_string`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_from_string_checked(
    val: *const HewString,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || unsafe {
        hew_yaml_from_string(val)
    });
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

/// Construct and acquire the result before publishing it to Hew.
///
/// # Safety
/// Inputs meet [`hew_yaml_from_null`]'s contract. `layout` describes the result
/// and `status` points to a writable status slot.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_from_null_checked(
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewYamlValue {
    // SAFETY: the borrowed inputs satisfy the physical constructor contract.
    let value = hew_runtime::owned_value_budget::with_value_owner(&None, || hew_yaml_from_null());
    // SAFETY: result representation and disposal follow this provider's ABI.
    unsafe {
        crate::managed_tree::publish(value, core::ptr::null_mut(), layout, status, |value| {
            hew_yaml_free(value);
        })
    }
}

use hew_cabi::managed::HEAP_EXCEEDED;
use hew_runtime::owned_value_budget::{current_owner, ManagedStorage};
use serde_yaml::Value;

fn payload(value: &Value) -> Option<u64> {
    hew_codec::managed_tree::yaml_payload_bytes(value)
}

fn storage(value: &HewYamlValue) -> ManagedStorage {
    ManagedStorage {
        identity: Arc::as_ptr(&value.inner) as usize,
        bytes: payload(value.value()).expect("live encoding payload"),
    }
}

enum Child {
    Private(Value),
    Owned(*mut HewYamlValue),
}

struct EditPlan {
    bytes: u64,
    array: Option<Vec<Value>>,
    mapping: Option<serde_yaml::Mapping>,
}

fn plan(candidate: &Value, key: Option<&Value>, child_value: &Value) -> Option<EditPlan> {
    let mut array_growth = None;
    let mut mapping_growth = None;
    let planned = (|| {
        let old_bytes = payload(candidate)?;
        let child_bytes = payload(child_value)?;
        match (candidate, key) {
            (Value::Mapping(values), Some(key)) => {
                mapping_growth = hew_codec::managed_tree::yaml_mapping_growth(values, key);
                if let Some(old) = values.get(key) {
                    old_bytes
                        .checked_sub(payload(old)?)?
                        .checked_add(child_bytes)
                } else {
                    let capacity = mapping_growth
                        .as_ref()
                        .map_or(values.capacity(), serde_yaml::Mapping::capacity);
                    old_bytes
                        .checked_add(
                            u64::try_from(capacity.checked_sub(values.capacity())?)
                                .ok()?
                                .checked_mul(24)?,
                        )?
                        .checked_add(payload(key)?.checked_sub(12)?)?
                        .checked_add(child_bytes.checked_sub(12)?)
                }
            }
            (Value::Sequence(values), None) => {
                array_growth = crate::managed_tree::array_growth(values).ok()?;
                let capacity = array_growth
                    .as_ref()
                    .map_or(values.capacity(), Vec::capacity);
                old_bytes
                    .checked_add(
                        u64::try_from(capacity.checked_sub(values.capacity())?)
                            .ok()?
                            .checked_mul(12)?,
                    )?
                    .checked_add(child_bytes.checked_sub(12)?)
            }
            _ => unreachable!(),
        }
    })();
    Some(EditPlan {
        bytes: planned?,
        array: array_growth,
        mapping: mapping_growth,
    })
}

unsafe fn edit(destination: *mut HewYamlValue, key: Option<&str>, child: Child, status: *mut i32) {
    // SAFETY: checked calls supply a writable status slot and live handles.
    unsafe { status.write(0) };
    let owner = current_owner();
    let child_pointer = match &child {
        Child::Owned(pointer) => *pointer,
        Child::Private(_) => core::ptr::null_mut(),
    };
    // SAFETY: an owned argument remains live until admission commits it.
    let child_handle = unsafe { child_pointer.as_ref() };
    if matches!(child, Child::Owned(_)) && child_handle.is_none() {
        return;
    }
    // SAFETY: the receiver remains borrowed and unchanged during planning.
    let Some(receiver) = (unsafe { destination.as_ref() }) else {
        if let Some(child) = child_handle {
            if let Some(owner) = owner {
                owner.release(&[storage(child)]);
            }
            // SAFETY: successful no-op consumes the provided child handle.
            unsafe { hew_yaml_free(child_pointer) };
        }
        return;
    };
    if !matches!(
        (receiver.value(), key),
        (Value::Mapping(_), Some(_)) | (Value::Sequence(_), None)
    ) {
        if let Some(child) = child_handle {
            if let Some(owner) = owner {
                owner.release(&[storage(child)]);
            }
            // SAFETY: successful wrong-kind no-op consumes its child handle.
            unsafe { hew_yaml_free(child_pointer) };
        }
        return;
    }
    let unique = Arc::strong_count(&receiver.inner) == 1;
    let child_unique = child_handle.is_none_or(|child| Arc::strong_count(&child.inner) == 1);
    let mut receiver_copy = (!unique).then(|| receiver.value().clone());
    let child_copy = child_handle
        .filter(|_| !child_unique)
        .map(|child| child.value().clone());
    let candidate = receiver_copy.as_ref().unwrap_or_else(|| receiver.value());
    let child_value = match &child {
        Child::Private(value) => value,
        Child::Owned(_) => child_copy
            .as_ref()
            .unwrap_or_else(|| child_handle.expect("live owned child").value()),
    };
    let key_value = key.map(|key| Value::String(key.to_owned()));
    let Some(plan) = plan(candidate, key_value.as_ref(), child_value) else {
        // SAFETY: capacity refusal leaves all live trees and handles intact.
        unsafe { status.write(HEAP_EXCEEDED) };
        return;
    };
    let mut retired = vec![(storage(receiver), unique)];
    if let Some(child) = child_handle {
        retired.push((storage(child), child_unique));
    }
    let permit = match crate::managed_tree::replacement(owner.as_ref(), &retired, Some(plan.bytes))
    {
        Ok(permit) => permit,
        Err(failure) => {
            // SAFETY: refusal retains the original receiver and owned child.
            unsafe { status.write(failure) };
            return;
        }
    };
    let child_value = match child {
        Child::Private(value) => value,
        Child::Owned(pointer) => {
            // SAFETY: admission transfers the child's unique outer handle.
            let child = unsafe { Box::from_raw(pointer) };
            #[cfg(test)]
            record_value_box_consumed();
            if let Some(copy) = child_copy {
                copy
            } else {
                child.into_value()
            }
        }
    };
    // SAFETY: the receiver is exclusively mutable after successful preflight.
    let receiver = unsafe { &mut *destination };
    if let Some(copy) = receiver_copy.take() {
        receiver.inner = Arc::new(copy);
    }
    let candidate = Arc::get_mut(&mut receiver.inner).expect("admitted unique YAML tree");
    match (candidate, key_value) {
        (Value::Mapping(values), Some(key)) => {
            hew_codec::managed_tree::yaml_mapping_insert(values, plan.mapping, key, child_value);
        }
        (Value::Sequence(values), None) => {
            crate::managed_tree::push(values, plan.array, child_value);
        }
        _ => unreachable!(),
    }
    if let Some(permit) = permit {
        permit.commit(Arc::as_ptr(&receiver.inner) as usize);
    }
}

/// Insert a child after admitting the complete COW replacement.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_set`]'s contract; `status` is writable.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set_checked(
    obj: *mut HewYamlValue,
    key: *const HewString,
    val: *mut HewYamlValue,
    status: *mut i32,
) {
    // SAFETY: all borrowed and owned inputs remain live until admission.
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(obj, Some(string_as_str(key)), Child::Owned(val), status) };
}

/// Append a child after admitting the complete COW replacement.
///
/// # Safety
/// Inputs meet [`hew_yaml_array_push`]'s contract; `status` is writable.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_push_checked(
    arr: *mut HewYamlValue,
    val: *mut HewYamlValue,
    status: *mut i32,
) {
    // SAFETY: all borrowed and owned inputs remain live until admission.
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(arr, None, Child::Owned(val), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_set_bool`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_object_set_bool_checked(
    obj: *mut HewYamlValue,
    key: *const HewString,
    val: i32,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Bool(val != 0);
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(obj, Some(string_as_str(key)), Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_set_int`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_object_set_int_checked(
    obj: *mut HewYamlValue,
    key: *const HewString,
    val: i64,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Number(val.into());
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(obj, Some(string_as_str(key)), Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_set_float`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_object_set_float_checked(
    obj: *mut HewYamlValue,
    key: *const HewString,
    val: f64,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Number(val.into());
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(obj, Some(string_as_str(key)), Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_set_string`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_object_set_string_checked(
    obj: *mut HewYamlValue,
    key: *const HewString,
    val: *const HewString,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::String(unsafe { string_as_str(val) }.to_owned());
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(obj, Some(string_as_str(key)), Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_set_null`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_object_set_null_checked(
    obj: *mut HewYamlValue,
    key: *const HewString,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Null;
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(obj, Some(string_as_str(key)), Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_set_char`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_object_set_char_checked(
    obj: *mut HewYamlValue,
    key: *const HewString,
    val: i64,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Number(val.into());
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(obj, Some(string_as_str(key)), Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_object_set_duration`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_object_set_duration_checked(
    obj: *mut HewYamlValue,
    key: *const HewString,
    val: i64,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Number(val.into());
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(obj, Some(string_as_str(key)), Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_array_push_bool`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_array_push_bool_checked(
    arr: *mut HewYamlValue,
    val: i32,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Bool(val != 0);
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(arr, None, Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_array_push_int`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_array_push_int_checked(
    arr: *mut HewYamlValue,
    val: i64,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Number(val.into());
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(arr, None, Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_array_push_float`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_array_push_float_checked(
    arr: *mut HewYamlValue,
    val: f64,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Number(val.into());
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(arr, None, Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_array_push_string`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_array_push_string_checked(
    arr: *mut HewYamlValue,
    val: *const HewString,
    status: *mut i32,
) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::String(unsafe { string_as_str(val) }.to_owned());
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(arr, None, Child::Private(child), status) };
}

/// Apply a checked primitive edit, preserving COW aliases on refusal.
///
/// # Safety
/// Inputs meet [`hew_yaml_array_push_null`]'s contract; `status` is writable.
pub(super) unsafe fn hew_yaml_array_push_null_checked(arr: *mut HewYamlValue, status: *mut i32) {
    // SAFETY: the checked ABI retains all borrowed inputs and status.
    let child = Value::Null;
    // SAFETY: all borrowed inputs and the writable status slot remain live.
    unsafe { edit(arr, None, Child::Private(child), status) };
}

/// Release one semantic tree interest before physically freeing its handle.
///
/// # Safety
/// `val` is null or an owned live handle in the current retained account.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_free_managed(val: *mut HewYamlValue) {
    // SAFETY: the semantic caller retains the handle through its leaf release.
    if let (Some(value), Some(owner)) = (unsafe { val.as_ref() }, current_owner()) {
        owner.release(&[storage(value)]);
    }
    hew_runtime::owned_value_budget::with_value_owner(&None, || {
        // SAFETY: accounting has ended; this frees only the physical owner.
        unsafe { hew_yaml_free(val) };
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
            visit: Some(hew_managed_visit_yaml),
        };
        crate::managed_tree::tests::layout(&MANAGED)
    }

    unsafe fn release(value: *mut HewYamlValue) {
        // SAFETY: this acquired handle belongs to the test's current account.
        unsafe { hew_yaml_free_managed(value) };
    }

    #[test]
    fn checked_constructor_refuses_without_publishing_or_leaking() {
        let before = live_value_boxes();
        let owner = ValueOwner::new(Some(12 - 1));
        let layout = layout();
        let mut status = -1;
        with_value_owner(&Some(owner.clone()), || {
            // SAFETY: live descriptor and writable status cover the call.
            let result = unsafe { hew_yaml_object_new_checked(&raw const layout, &raw mut status) };
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
        for limit in [109 - 1, 109] {
            let owner = ValueOwner::new(Some(limit));
            with_value_owner(&Some(owner.clone()), || {
                let mut status = -1;
                // SAFETY: each handle is uniquely retained until edit or release.
                unsafe {
                    let object = hew_yaml_object_new_checked(&raw const layout, &raw mut status);
                    assert_eq!(status, 0);
                    let child = hew_yaml_from_int_checked(42, &raw const layout, &raw mut status);
                    assert_eq!(status, 0);
                    assert_eq!(owner.live_bytes(), 12 * 2);
                    hew_yaml_object_set_checked(object, key, child, &raw mut status);
                    if limit == 109 {
                        assert_eq!(status, 0);
                        assert_eq!(owner.live_bytes(), 109);
                        let field = hew_yaml_get_field(object, key);
                        assert_eq!(hew_yaml_get_int(field), 42);
                        hew_yaml_free(field);
                    } else {
                        assert_eq!(status, HEAP_EXCEEDED);
                        assert!(hew_yaml_get_field(object, key).is_null());
                        assert_eq!(hew_yaml_get_int(child), 42);
                        assert_eq!(owner.live_bytes(), 12 * 2);
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
        for limit in [12 * 5 - 1, 12 * 5] {
            let owner = ValueOwner::new(Some(limit));
            with_value_owner(&Some(owner.clone()), || {
                let mut status = -1;
                // SAFETY: acquired handles retain their ownership until commit.
                unsafe {
                    let array = hew_yaml_array_new_checked(&raw const layout, &raw mut status);
                    let child = hew_yaml_from_int_checked(7, &raw const layout, &raw mut status);
                    assert_eq!(status, 0);
                    hew_yaml_array_push_checked(array, child, &raw mut status);
                    if limit == 12 * 5 {
                        assert_eq!(status, 0);
                        assert_eq!(hew_yaml_array_len(array), 1);
                        assert_eq!(owner.live_bytes(), 12 * 5);
                    } else {
                        assert_eq!(status, HEAP_EXCEEDED);
                        assert_eq!(hew_yaml_array_len(array), 0);
                        assert_eq!(hew_yaml_get_int(child), 7);
                        release(child);
                    }
                    release(array);
                    assert_eq!(owner.live_bytes(), 0);
                }
            });
        }
    }

    #[test]
    fn checked_yaml_cow_charges_new_tree_without_changing_alias() {
        let layout = layout();
        let key = string_from_str("k");
        for limit in [120, 121] {
            let owner = ValueOwner::new(Some(limit));
            with_value_owner(&Some(owner.clone()), || {
                let mut status = -1;
                // SAFETY: independent outer handles retain their common tree.
                unsafe {
                    let original = hew_yaml_object_new_checked(&raw const layout, &raw mut status);
                    let alias =
                        hew_yaml_clone_checked(original, &raw const layout, &raw mut status);
                    assert_eq!(status, 0);
                    assert_eq!(owner.live_bytes(), 12);
                    let child = hew_yaml_from_int_checked(7, &raw const layout, &raw mut status);
                    hew_yaml_object_set_checked(original, key, child, &raw mut status);
                    assert!(hew_yaml_get_field(alias, key).is_null());
                    if limit == 121 {
                        assert_eq!(status, 0);
                        assert_eq!(owner.live_bytes(), 121);
                        let field = hew_yaml_get_field(original, key);
                        assert_eq!(hew_yaml_get_int(field), 7);
                        hew_yaml_free(field);
                    } else {
                        assert_eq!(status, HEAP_EXCEEDED);
                        assert!(hew_yaml_get_field(original, key).is_null());
                        assert_eq!(hew_yaml_get_int(child), 7);
                        release(child);
                    }
                    release(alias);
                    release(original);
                    assert_eq!(owner.live_bytes(), 0);
                }
            });
        }
        // SAFETY: this unbounded test key stays live through both cases.
        unsafe { string_release(key) };
    }
}
