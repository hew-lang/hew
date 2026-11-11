//! Owned, bounded facts copied from one failure while its code image is live.
//!
//! Unlike `FailureRecord`, this representation contains no image pointers and
//! can outlive the actor or travel to a different process. Capture never uses
//! the actor's allocation budget. Failed text or frame allocation leaves optional detail
//! unavailable rather than raising another Hew failure.

use crate::failure_record::{FailureRecord, MAX_FRAMES};
use std::ffi::{c_char, CStr};

const TEXT_LIMIT: usize = 4096;
const TRUNCATED: &str = " [truncated]";

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Location {
    pub file: String,
    pub line: u32,
    /// Zero when no column was recorded.
    pub column: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Frame {
    pub function: Option<String>,
    /// The function's declaration site when the compact table names it.
    pub site: Option<Location>,
    pub repeat: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FailureSnapshot {
    pub code: i32,
    /// Absent if the diagnostic could not be copied.
    pub message: Option<String>,
    pub site: Option<Location>,
    /// Absent when no frames were captured or memory could not be reserved.
    pub frames: Option<Vec<Frame>>,
    pub frames_omitted: u32,
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
pub(crate) unsafe fn capture(record: &FailureRecord, message: &str) -> FailureSnapshot {
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
    let frames = if record.frames().is_empty() {
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
        let snapshot = unsafe { capture(&record, "broken") };
        drop(file);
        drop(function);
        assert_eq!(snapshot.message.as_deref(), Some("broken"));
        assert_eq!(snapshot.site.as_ref().unwrap().file, "app.hew");
        let frame = &snapshot.frames.as_ref().unwrap()[0];
        assert_eq!(frame.function.as_deref(), Some("Worker.run"));
        assert_eq!(frame.site.as_ref().unwrap().line, 3);
        assert_eq!(frame.repeat, 2);
        // SAFETY: an empty record has no source pointers to retain.
        let no_detail = unsafe { capture(&FailureRecord::new(202), "") };
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
}
