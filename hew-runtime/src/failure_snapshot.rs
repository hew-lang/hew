//! Owned, bounded facts copied from one failure while its code image is live.
//!
//! Unlike `FailureRecord`, this representation contains no image pointers and
//! can outlive the actor or travel to a different process. Capture never uses
//! the actor's allocation budget. Failed text or frame allocation leaves optional detail
//! unavailable rather than raising another Hew failure.

use crate::failure_record::{FailureRecord, MAX_FRAMES};
use std::ffi::c_void;
use std::ffi::{c_char, CStr};

const TEXT_LIMIT: usize = 4096;
const TRUNCATED: &str = " [truncated]";

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct Location {
    pub file: String,
    pub line: u32,
    /// Zero when no column was recorded.
    pub column: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct Frame {
    pub function: Option<String>,
    /// The function's declaration site when the compact table names it.
    pub site: Option<Location>,
    pub repeat: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FailureSnapshot {
    pub code: i32,
    /// Absent if the diagnostic could not be copied.
    pub message: Option<String>,
    pub site: Option<Location>,
    /// Absent when no frames were captured or memory could not be reserved.
    #[serde(deserialize_with = "deserialize_frames")]
    pub frames: Option<Vec<Frame>>,
    pub frames_omitted: u32,
}

impl FailureSnapshot {
    pub(crate) fn without_detail(code: i32, message: &str) -> Self {
        Self {
            code,
            message: copy_message(message),
            site: None,
            frames: None,
            frames_omitted: 0,
        }
    }
}

fn deserialize_frames<'de, D>(deserializer: D) -> Result<Option<Vec<Frame>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct OptionalFrames;
    impl<'de> serde::de::Visitor<'de> for OptionalFrames {
        type Value = Option<Vec<Frame>>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            formatter.write_str("an optional bounded frame list")
        }

        fn visit_none<E: serde::de::Error>(self) -> Result<Self::Value, E> {
            Ok(None)
        }

        fn visit_unit<E: serde::de::Error>(self) -> Result<Self::Value, E> {
            Ok(None)
        }

        fn visit_some<D: serde::Deserializer<'de>>(
            self,
            deserializer: D,
        ) -> Result<Self::Value, D::Error> {
            struct Frames;
            impl<'de> serde::de::Visitor<'de> for Frames {
                type Value = Vec<Frame>;

                fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    formatter.write_str("at most 48 failure frames")
                }

                fn visit_seq<A: serde::de::SeqAccess<'de>>(
                    self,
                    mut sequence: A,
                ) -> Result<Self::Value, A::Error> {
                    let mut frames = Vec::new();
                    while let Some(frame) = sequence.next_element()? {
                        if frames.len() == MAX_FRAMES {
                            return Err(serde::de::Error::custom("too many failure frames"));
                        }
                        frames.push(frame);
                    }
                    Ok(frames)
                }
            }
            deserializer.deserialize_seq(Frames).map(Some)
        }
    }
    deserializer.deserialize_option(OptionalFrames)
}

/// A wire diagnostic is versioned independently of the enclosing protocol.
#[cfg(not(target_arch = "wasm32"))]
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct WireFailure {
    version: u8,
    failure: FailureSnapshot,
}

#[cfg(not(target_arch = "wasm32"))]
const WIRE_LIMIT: usize = 512 * 1024;

#[cfg(not(target_arch = "wasm32"))]
impl FailureSnapshot {
    fn valid_wire(&self) -> bool {
        fn valid_location(location: &Location) -> bool {
            !location.file.is_empty() && location.file.len() <= TEXT_LIMIT && location.line > 0
        }
        self.code > 0
            && self
                .message
                .as_ref()
                .is_none_or(|message| message.len() <= TEXT_LIMIT)
            && self.site.as_ref().is_none_or(valid_location)
            && self.frames.as_ref().is_none_or(|frames| {
                frames.len() <= MAX_FRAMES
                    && frames.iter().all(|frame| {
                        frame.repeat > 0
                            && frame
                                .function
                                .as_ref()
                                .is_none_or(|name| !name.is_empty() && name.len() <= TEXT_LIMIT)
                            && frame.site.as_ref().is_none_or(valid_location)
                    })
            })
    }

    pub(crate) fn encode_wire(&self) -> Option<Vec<u8>> {
        if !self.valid_wire() {
            return None;
        }
        let mut bytes = Vec::new();
        ciborium::into_writer(
            &WireFailure {
                version: 1,
                failure: self.clone(),
            },
            &mut bytes,
        )
        .ok()?;
        (bytes.len() <= WIRE_LIMIT).then_some(bytes)
    }

    pub(crate) fn decode_wire(bytes: &[u8]) -> Option<Self> {
        if bytes.len() > WIRE_LIMIT {
            return None;
        }
        let mut input = std::io::Cursor::new(bytes);
        let wire: WireFailure = ciborium::from_reader(&mut input).ok()?;
        (wire.version == 1 && input.position() == bytes.len() as u64 && wire.failure.valid_wire())
            .then_some(wire.failure)
    }
}

/// Copy a UTF-8 string with a fallible, bounded allocation. Paths and names
/// are omitted when too long: a truncated source identity would name a
/// different file or function.
fn copy_text(text: &str) -> Option<String> {
    if text.len() > TEXT_LIMIT {
        return None;
    }
    let mut result = String::new();
    result.try_reserve_exact(text.len()).ok()?;
    result.push_str(text);
    Some(result)
}

fn copy_message(text: &str) -> Option<String> {
    if text.len() <= TEXT_LIMIT {
        return copy_text(text);
    }
    let mut end = TEXT_LIMIT - TRUNCATED.len();
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    let mut result = String::new();
    result.try_reserve_exact(TEXT_LIMIT).ok()?;
    result.push_str(&text[..end]);
    result.push_str(TRUNCATED);
    Some(result)
}

/// # Safety
/// `text` is null or points to a live NUL-terminated string for this call.
unsafe fn copy_c_text(text: *const c_char) -> Option<String> {
    if text.is_null() {
        return None;
    }
    // SAFETY: the caller retains the image which owns the string.
    copy_text(unsafe { CStr::from_ptr(text) }.to_str().ok()?)
}

/// # Safety
/// Non-null source pointers in `record` remain live for the duration of this
/// call. The returned snapshot does not borrow any of them.
pub(crate) unsafe fn capture(
    record: &FailureRecord,
    message: &str,
    include_frames: bool,
) -> FailureSnapshot {
    let site = if record.site.line == 0 {
        None
    } else {
        // SAFETY: the caller retains the image through this capture.
        unsafe { copy_c_text(record.site.file) }.map(|file| Location {
            file,
            line: record.site.line,
            column: record.site.col,
        })
    };
    let frames = if !include_frames || record.frames().is_empty() {
        None
    } else {
        let mut out = Vec::new();
        if out
            .try_reserve_exact(record.frames().len().min(MAX_FRAMES))
            .is_err()
        {
            None
        } else {
            for frame in record.frames() {
                // SAFETY: the caller retains the image through this capture.
                let function = unsafe { copy_c_text(frame.hew_fn) };
                let site = if frame.line == 0 {
                    None
                } else {
                    // SAFETY: the caller retains the image through this capture.
                    unsafe { copy_c_text(frame.file) }.map(|file| Location {
                        file,
                        line: frame.line,
                        column: 0,
                    })
                };
                out.push(Frame {
                    function,
                    site,
                    repeat: frame.repeat,
                });
            }
            Some(out)
        }
    };
    FailureSnapshot {
        code: record.code,
        message: copy_message(message),
        site,
        frames,
        frames_omitted: record.frames_omitted,
    }
}

// These accessors are a temporary borrow used only while generated code
// materializes ordinary owned Hew values. Their pointers are never serialized.
unsafe fn snapshot<'a>(value: *const c_void) -> Option<&'a FailureSnapshot> {
    // SAFETY: forwarded from each accessor's caller contract.
    unsafe { value.cast::<FailureSnapshot>().as_ref() }
}

fn named_frame(snapshot: &FailureSnapshot, index: u32) -> Option<&Frame> {
    snapshot
        .frames
        .as_ref()?
        .iter()
        .filter(|frame| frame.function.is_some())
        .nth(index as usize)
}

/// Return the public crash-class tag for an owned failure.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_kind(value: *const c_void) -> i32 {
    let code = unsafe { snapshot(value) }.map_or(0, |snapshot| snapshot.code);
    crate::internal::types::ExitReason::from_error_code(code).to_crash_kind() as i32
}

/// Return the public trap tag, or -1 when no known trap was recorded.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_trap(value: *const c_void) -> i32 {
    unsafe { snapshot(value) }
        .and_then(|snapshot| crate::failure_kinds::trap_kind_tag(snapshot.code))
        .and_then(|tag| i32::try_from(tag).ok())
        .unwrap_or(-1)
}

/// Copy the diagnostic message into one managed string owner.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_message(
    value: *const c_void,
) -> *mut hew_cabi::string::HewString {
    let message = unsafe { snapshot(value) }
        .and_then(|snapshot| snapshot.message.as_deref())
        .unwrap_or("");
    hew_cabi::string::string_try_from_str(message).unwrap_or(std::ptr::null_mut())
}

/// Report whether a source site is available.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_has_site(value: *const c_void) -> bool {
    unsafe { snapshot(value) }.is_some_and(|snapshot| snapshot.site.is_some())
}

/// Copy the source filename into one managed string owner.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_site_file(
    value: *const c_void,
) -> *mut hew_cabi::string::HewString {
    let file = unsafe { snapshot(value) }
        .and_then(|snapshot| snapshot.site.as_ref())
        .map_or("", |site| site.file.as_str());
    hew_cabi::string::string_try_from_str(file).unwrap_or(std::ptr::null_mut())
}

/// Return the site's one-based line, or zero when unavailable.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_site_line(value: *const c_void) -> i64 {
    i64::from(
        unsafe { snapshot(value) }
            .and_then(|snapshot| snapshot.site.as_ref())
            .map_or(0, |site| site.line),
    )
}

/// Return the site's one-based column, or zero when unavailable.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_site_column(value: *const c_void) -> i64 {
    i64::from(
        unsafe { snapshot(value) }
            .and_then(|snapshot| snapshot.site.as_ref())
            .map_or(0, |site| site.column),
    )
}

/// Number of named Hew frames, or -1 when no trace was captured.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_frame_count(value: *const c_void) -> i32 {
    unsafe { snapshot(value) }
        .and_then(|snapshot| snapshot.frames.as_ref())
        .map_or(-1, |frames| {
            i32::try_from(
                frames
                    .iter()
                    .filter(|frame| frame.function.is_some())
                    .count(),
            )
            .unwrap_or(i32::MAX)
        })
}

/// Copy one named frame's function into a managed string owner.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_frame_function(
    value: *const c_void,
    index: u32,
) -> *mut hew_cabi::string::HewString {
    let function = unsafe { snapshot(value) }
        .and_then(|snapshot| named_frame(snapshot, index))
        .and_then(|frame| frame.function.as_deref())
        .unwrap_or("");
    hew_cabi::string::string_try_from_str(function).unwrap_or(std::ptr::null_mut())
}

/// Report whether one named frame has a source declaration.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_frame_has_site(
    value: *const c_void,
    index: u32,
) -> bool {
    unsafe { snapshot(value) }
        .and_then(|snapshot| named_frame(snapshot, index))
        .is_some_and(|frame| frame.site.is_some())
}

/// Copy one named frame's source filename into a managed string owner.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_frame_file(
    value: *const c_void,
    index: u32,
) -> *mut hew_cabi::string::HewString {
    let file = unsafe { snapshot(value) }
        .and_then(|snapshot| named_frame(snapshot, index))
        .and_then(|frame| frame.site.as_ref())
        .map_or("", |site| site.file.as_str());
    hew_cabi::string::string_try_from_str(file).unwrap_or(std::ptr::null_mut())
}

/// Return one named frame's source line, or zero when unavailable.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_frame_line(value: *const c_void, index: u32) -> i64 {
    i64::from(
        unsafe { snapshot(value) }
            .and_then(|snapshot| named_frame(snapshot, index))
            .and_then(|frame| frame.site.as_ref())
            .map_or(0, |site| site.line),
    )
}

/// Return the number of identical physical frames folded into one entry.
/// # Safety
/// `value` is null or a live borrowed failure snapshot.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_snapshot_frame_repeat(
    value: *const c_void,
    index: u32,
) -> i64 {
    i64::from(
        unsafe { snapshot(value) }
            .and_then(|snapshot| named_frame(snapshot, index))
            .map_or(0, |frame| frame.repeat),
    )
}

/// Acquire the completed diagnostic in the current actor or task account.
/// A refusal leaves the unpublished value unchanged and unaccounted.
/// # Safety
/// `layout` and `value` describe one live unpublished value.
#[no_mangle]
pub unsafe extern "C" fn hew_failure_try_adopt(
    layout: *const hew_cabi::value::HewValueLayout,
    value: *const c_void,
) -> bool {
    let Some(owner) = crate::owned_value_budget::current_owner() else {
        return true;
    };
    // SAFETY: the generated caller supplies the exact live value descriptor.
    let Some(layout) = (unsafe { layout.as_ref() }) else {
        return false;
    };
    // SAFETY: the value remains unpublished while this transaction runs.
    match unsafe { owner.acquire_value(layout, value) } {
        Ok(acquired) => {
            acquired.commit();
            true
        }
        Err(_) => false,
    }
}

/// Release a diagnostic whose adoption failed; it has no budget owner.
/// # Safety
/// `layout` and `value` describe one unpublished value with synchronous drop.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_failure_drop_unaccounted(
    layout: *const hew_cabi::value::HewValueLayout,
    value: *mut c_void,
) {
    // SAFETY: the caller retains the compiler-generated descriptor and value.
    let layout = unsafe { &*layout };
    if layout.release_start.is_some() {
        std::process::abort();
    }
    if let Some(drop) = layout.drop_fn {
        crate::owned_value_budget::with_value_owner(&None, || {
            // SAFETY: adoption failed and the caller transfers this one live value.
            unsafe { drop(value) };
        });
    }
}

/// Release a partially built trace before it has an account owner.
/// # Safety
/// `value` is one unpublished vector owner.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_failure_free_unaccounted_trace(value: *mut crate::vec::HewVec) {
    crate::owned_value_budget::with_value_owner(&None, || {
        // SAFETY: the caller transfers the partial vector and its elements.
        unsafe { crate::vec::hew_vec_free_owned(value) };
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::failure_record::FailureFrame;
    use std::ffi::CString;

    #[test]
    fn snapshot_outlives_source_strings_and_preserves_absence() {
        let file = CString::new("app.hew").unwrap();
        let function = CString::new("Worker.run").unwrap();
        let mut record = FailureRecord::new(212);
        record.site.file = file.as_ptr();
        record.site.line = 7;
        record.site.col = 4;
        record.frames[0] = FailureFrame {
            pc: 42,
            hew_fn: function.as_ptr(),
            file: file.as_ptr(),
            line: 3,
            repeat: 2,
        };
        record.frame_count = 1;
        // SAFETY: both source strings remain live through this call.
        let snapshot = unsafe { capture(&record, "broken", true) };
        drop(file);
        drop(function);
        assert_eq!(snapshot.message.as_deref(), Some("broken"));
        assert_eq!(snapshot.site.as_ref().unwrap().file, "app.hew");
        let frame = &snapshot.frames.as_ref().unwrap()[0];
        assert_eq!(frame.function.as_deref(), Some("Worker.run"));
        assert_eq!(frame.site.as_ref().unwrap().line, 3);
        assert_eq!(frame.repeat, 2);
        // SAFETY: an empty record has no source pointers to retain.
        let no_detail = unsafe { capture(&FailureRecord::new(202), "", true) };
        assert_eq!(no_detail.site, None);
        assert_eq!(no_detail.frames, None);
    }

    #[test]
    fn oversized_text_is_bounded_without_inventing_source_names() {
        let message = "é".repeat(TEXT_LIMIT);
        let copied = copy_message(&message).unwrap();
        assert!(copied.len() <= TEXT_LIMIT);
        assert!(copied.ends_with(TRUNCATED));
        assert!(copy_text(&message).is_none());
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn wire_failure_round_trips_and_rejects_invalid_data() {
        let frame = Frame {
            function: Some("Worker.run".into()),
            site: Some(Location {
                file: "worker.hew".into(),
                line: 4,
                column: 0,
            }),
            repeat: 2,
        };
        let snapshot = FailureSnapshot {
            code: 212,
            message: Some("broken".into()),
            site: Some(Location {
                file: "worker.hew".into(),
                line: 8,
                column: 3,
            }),
            frames: Some(vec![frame.clone()]),
            frames_omitted: 1,
        };
        let bytes = snapshot.encode_wire().unwrap();
        assert_eq!(FailureSnapshot::decode_wire(&bytes), Some(snapshot.clone()));
        assert!(FailureSnapshot::decode_wire(&bytes[..bytes.len() - 1]).is_none());
        let mut trailing = bytes;
        trailing.push(0);
        assert!(FailureSnapshot::decode_wire(&trailing).is_none());
        let mut bad_version = Vec::new();
        ciborium::into_writer(
            &WireFailure {
                version: 2,
                failure: snapshot.clone(),
            },
            &mut bad_version,
        )
        .unwrap();
        assert!(FailureSnapshot::decode_wire(&bad_version).is_none());
        let mut too_many = snapshot;
        too_many.frames = Some(vec![frame; MAX_FRAMES + 1]);
        assert!(too_many.encode_wire().is_none());
        let mut invalid = Vec::new();
        ciborium::into_writer(
            &WireFailure {
                version: 1,
                failure: too_many,
            },
            &mut invalid,
        )
        .unwrap();
        assert!(FailureSnapshot::decode_wire(&invalid).is_none());
        assert!(FailureSnapshot::decode_wire(&vec![0; WIRE_LIMIT + 1]).is_none());
    }
}
