//! Ownership adapter shared by the checked synchronous Hew text exports.

use hew_cabi::host_error::{HewError, HewText};
use hew_runtime::host_api::{
    hew_host_error_message, hew_host_error_release, hew_host_text_data, hew_host_text_from_utf8,
    hew_host_text_release,
};

struct Text(*mut HewText);

impl Drop for Text {
    fn drop(&mut self) {
        // SAFETY: this adapter owns the one release obligation for this text.
        unsafe { hew_host_text_release(self.0) };
    }
}

fn checked(status: i32, error: *mut HewError) {
    if status != 0 {
        let mut len = 0;
        // SAFETY: a failed generated call publishes a live error owner. The
        // borrowed bytes are copied before releasing that owner.
        let message = unsafe {
            let data = hew_host_error_message(error, &raw mut len);
            let message =
                String::from_utf8_lossy(std::slice::from_raw_parts(data, len)).into_owned();
            hew_host_error_release(error);
            message
        };
        panic!("Hew presentation failed: {message}");
    }
}

/// Call a generated borrowed-string -> owned-string host export.
///
/// # Safety
/// `export` must uphold the checked `HostExport` contract, including ownership of
/// the returned text or error. The build script verifies each selected export.
pub(crate) unsafe fn call(
    input: &str,
    export: unsafe extern "C" fn(*const HewText, *mut *mut HewText, *mut *mut HewError) -> i32,
) -> String {
    let mut source = std::ptr::null_mut();
    let mut output = std::ptr::null_mut();
    let mut error = std::ptr::null_mut();
    // SAFETY: input is valid borrowed UTF-8; all owner/error slots are empty
    // and disjoint. The generated export borrows source and publishes output.
    unsafe {
        checked(
            hew_host_text_from_utf8(input.as_ptr(), input.len(), &raw mut source, &raw mut error),
            error,
        );
        let source = Text(source);
        checked(export(source.0, &raw mut output, &raw mut error), error);
        let output = Text(output);
        let mut len = 0;
        let data = hew_host_text_data(output.0, &raw mut len);
        // The host text contract guarantees valid UTF-8 for the owner's life.
        std::str::from_utf8(std::slice::from_raw_parts(data, len))
            .expect("renderer returns UTF-8")
            .to_owned()
    }
}
